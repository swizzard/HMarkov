{-# LANGUAGE TemplateHaskell #-}
{-|
   Module      : Data.Markov.HMarkov.Helpers
   Description : Helpers for Data.Markov.HMarkov
   Copyright   : (c) Sam Raker, 2016
   License     : BSD3
   Maintainer  : sam.raker@gmail.com
   Stability   : experimental
   Portability : POSIX
-}
module Data.Markov.HMarkov.Helpers
  (
  -- * Data structures
    CountMarkovMap(..)
  , MarkovMap(..)
  -- * Helper functions
  , vApply
  , vidx
  , ded
  , makeSlices
  , nrmlz
  , sumP
  , pix
  , getNext
  , buildMap
  , initMap
  , updateMarkov
  , toMarkovMap
  ) where

import Control.Lens
import Data.Maybe
import Data.Vector as V

-- | Map of counts
data CountMarkovMap a
  = CMarkovMap (V.Vector a) (V.Vector (V.Vector Double)) -- ^ Map of counts

-- | Map of frequencies
data MarkovMap a = MarkovMap {
-- | Index of elements
  _idx :: V.Vector a,
-- | Map of frequencies
   _mMap :: V.Vector (V.Vector Double) }
   deriving (Show)

makeLenses ''MarkovMap

-- | Apply a function to the first two elements in a vector and a third thing
vApply :: (a -> a -> b -> c) -> b -> V.Vector a -> c
vApply f x v = f (v V.! 0) (v V.! 1) x

-- | Get the index of an element in a vector
-- WARNING: will throw an error if `x` is not in `v`
vidx :: (Eq a) => a -> V.Vector a -> Int
vidx x v = fromJust $ V.elemIndex x v

-- | Deduplicate a vector
ded :: (Eq a) => V.Vector a -> V.Vector a
ded = V.foldl f V.empty where
            f accm x = if V.elem x accm then accm else V.snoc accm x

-- | Initialize a CountMarkovMap from a vector
initMap :: (Eq a) => V.Vector a -> CountMarkovMap a
initMap xs = let d = ded xs
                 l = V.length xs in
             CMarkovMap d (V.replicate l (V.replicate l 0))

-- | Update a CountMarkovMap
-- `a` and `b` should be sequential elements
updateMarkov :: (Eq a) => a -> a -> CountMarkovMap a -> CountMarkovMap a
updateMarkov a b (CMarkovMap i m) = CMarkovMap i $ over (ix (vidx a i) . ix (vidx b i)) (+ 1) m

-- | Make 2-element (vector) slices of a vector
makeSlices :: V.Vector a -> V.Vector (V.Vector a)
makeSlices xs = V.map (\i -> V.slice i 2 xs) $ V.enumFromN 0 (V.length xs - 1)

-- | Normalize a vector of doubles by dividing each element by the sum of the vector
nrmlz :: V.Vector Double -> V.Vector Double
nrmlz v = V.map (/ V.sum v) v

-- | Progressively sum elements in a vector of doubles, skipping over 0s
sumP :: V.Vector Double -> V.Vector Double
sumP v = fst $ V.foldl f (V.empty, 0.0) v where
    f (accm, n) a = if a > 0 then (V.snoc accm (n + a), n + a) else (V.snoc accm 0, n)

-- | Convert a CountMarkovMap to a MarkovMap by normalizing and summing its elements
toMarkovMap :: CountMarkovMap a -> MarkovMap a
toMarkovMap (CMarkovMap ci cm) = MarkovMap ci $ V.map (sumP . nrmlz) cm

-- | Get the index of the first element in a vector of doubles that's less than or equal to
-- a value
pix :: Double -> V.Vector Double -> Int
pix x = V.ifoldr f 0 where
    f i p a = if x <= p then i else a

-- | Generate the 'next' element from a MarkovMap
getNext :: (Eq a) => a -> Double -> MarkovMap a -> a
getNext t x m = (m ^. idx) V.! (pix x ((m ^. mMap) V.! (vidx t $ m ^. idx)))

-- | Build a MarkovMap from a vector of elements
buildMap :: (Eq a) => V.Vector a -> MarkovMap a
buildMap xs = toMarkovMap $ V.foldl (vApply updateMarkov) (initMap xs) (makeSlices xs)

