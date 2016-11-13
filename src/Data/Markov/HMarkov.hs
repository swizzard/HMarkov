module Data.Markov.HMarkov (MarkovMap(), buildMap, runMarkov) where

import Control.Lens
import Data.Maybe
import Data.Vector as V
import System.Random


data CountMarkovMap a = CMarkovMap { cidx :: V.Vector a,
                                     cmapping :: V.Vector (V.Vector Double) }
                        deriving (Show)

data MarkovMap a = MarkovMap { idx :: V.Vector a,
                               map :: V.Vector (V.Vector Double) }


vApply :: (a -> a -> b -> c) -> b -> V.Vector a -> c
vApply f x v = f (v V.! 0) (v V.! 1) x


ded :: (Eq a) => V.Vector a -> V.Vector a
ded xs = V.foldl f V.empty xs where
            f acc x = if (V.elem x acc) then acc else (V.snoc acc x)


vidx :: (Eq a) => a -> V.Vector a -> Int
vidx x v = fromJust $ V.elemIndex x v


initMap :: (Eq a) => V.Vector a -> CountMarkovMap a
initMap xs = let d = ded xs
                 l = V.length xs in
             CMarkovMap d (V.replicate l (V.replicate l 0))


updateMarkov :: (Eq a) => a -> a -> CountMarkovMap a -> CountMarkovMap a
updateMarkov a b (CMarkovMap i m) = CMarkovMap i $ over (ix (vidx a i) . ix (vidx b i)) (+ 1) m


makeSlices :: V.Vector a -> V.Vector (V.Vector a)
makeSlices xs = V.map (\i -> V.slice i 2 xs) $ V.enumFromN 1 (V.length xs  - 2)


nrmlz :: V.Vector Double -> V.Vector Double
nrmlz v = V.map (/ (V.sum v)) v


sumP :: V.Vector Double -> V.Vector Double
sumP v = fst $ V.foldl f (V.empty, 0.0) v where
    f (acc, n) a = if a > 0 then (V.snoc acc (n + a), n + a) else (V.snoc acc 0, n)


toMarkovMap :: CountMarkovMap a -> MarkovMap a
toMarkovMap (CMarkovMap ci cm) = MarkovMap ci $ V.map (sumP . nrmlz) cm


buildMap :: (Eq a) => V.Vector a -> MarkovMap a
buildMap xs = toMarkovMap $ V.foldl (vApply updateMarkov) (initMap xs) (makeSlices xs)


pix :: Double -> V.Vector Double -> Int
pix x v = V.ifoldr f 0 v where
    f i p a = if x <= p then i else a


runMarkov :: (Eq a, RandomGen g) => MarkovMap a -> a -> g -> (a, g)
runMarkov (MarkovMap i m) start g = let (x, g') = random g
                                        v = m V.! (vidx start i)
                                        new = i V.! (pix x v) in
                                    (new, g')

