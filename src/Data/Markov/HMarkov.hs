{-# LANGUAGE FlexibleContexts, TemplateHaskell #-}
module Data.Markov.HMarkov
  (
    MarkovMap
  , MarkovProcess
  , buildMap
  , buildProc
  , runMarkov
  , runUntil
  ) where

import Control.Lens
import Control.Monad.State
import Data.Maybe
import Data.Monoid
import Data.Vector as V
import System.Random


data CountMarkovMap a = CMarkovMap { _cidx :: V.Vector a,
                                     _cmapping :: V.Vector (V.Vector Double) }

data MarkovMap a = MarkovMap { _idx :: V.Vector a,
                               _mMap :: V.Vector (V.Vector Double) }
                   deriving (Show)

data MarkovProcess m a = MarkovProcess { _pMap :: MarkovMap a,
                                         _g :: StdGen,
                                         _lastT :: a,
                                         _acc :: m a }
                         deriving (Show)

makeLenses ''CountMarkovMap
makeLenses ''MarkovMap
makeLenses ''MarkovProcess


vApply :: (a -> a -> b -> c) -> b -> V.Vector a -> c
vApply f x v = f (v V.! 0) (v V.! 1) x

vidx :: (Eq a) => a -> V.Vector a -> Int
vidx x v = fromJust $ V.elemIndex x v

ded :: (Eq a) => V.Vector a -> V.Vector a
ded = V.foldl f V.empty where
            f accm x = if V.elem x accm then accm else V.snoc accm x

initMap :: (Eq a) => V.Vector a -> CountMarkovMap a
initMap xs = let d = ded xs
                 l = V.length xs in
             CMarkovMap d (V.replicate l (V.replicate l 0))

updateMarkov :: (Eq a) => a -> a -> CountMarkovMap a -> CountMarkovMap a
updateMarkov a b (CMarkovMap i m) = CMarkovMap i $ over (ix (vidx a i) . ix (vidx b i)) (+ 1) m

makeSlices :: V.Vector a -> V.Vector (V.Vector a)
makeSlices xs = V.map (\i -> V.slice i 2 xs) $ V.enumFromN 1 (V.length xs  - 2)

nrmlz :: V.Vector Double -> V.Vector Double
nrmlz v = V.map (/ V.sum v) v

sumP :: V.Vector Double -> V.Vector Double
sumP v = fst $ V.foldl f (V.empty, 0.0) v where
    f (accm, n) a = if a > 0 then (V.snoc accm (n + a), n + a) else (V.snoc accm 0, n)

toMarkovMap :: CountMarkovMap a -> MarkovMap a
toMarkovMap (CMarkovMap ci cm) = MarkovMap ci $ V.map (sumP . nrmlz) cm

buildMap :: (Eq a) => V.Vector a -> MarkovMap a
buildMap xs = toMarkovMap $ V.foldl (vApply updateMarkov) (initMap xs) (makeSlices xs)

pix :: Double -> V.Vector Double -> Int
pix x = V.ifoldr f 0 where
    f i p a = if x <= p then i else a

getNext :: (Eq a) => a -> Double -> MarkovMap a -> a
getNext t x m = (m ^. idx) V.! (pix x $ (m ^. mMap) V.! (vidx t $ m ^. idx))

buildProc :: (Eq a, MonadPlus m) => V.Vector a -> a -> StdGen -> MarkovProcess m a
buildProc xs x gen = MarkovProcess (buildMap xs) gen x mzero

runMarkov :: (Eq a, MonadPlus m) => MarkovProcess m a -> (a, MarkovProcess m a)
runMarkov p = let (x, g') = random $ p ^. g
                  lst = p ^. lastT
                  new = getNext lst x $ p ^. pMap in
              (new, p & g .~ g' & lastT .~ new & acc %~ \a -> mplus a $ return lst)

runUntil :: (Eq a, MonadPlus m) => (a -> Bool) -> MarkovProcess m a -> (a, MarkovProcess m a)
runUntil p = runState . fix $
             \continue -> state runMarkov >>=
             \a -> if p a then continue else pure a

