{-# LANGUAGE FlexibleContexts, TemplateHaskell #-}
module Data.Markov.HMarkov
  (
    MarkovMap(..)
  , MarkovProcess(..)
  , buildMap
  , buildProc
  , runMarkov
  , runUntil
  ) where

import Control.Lens
import Control.Monad.State
import Data.Vector as V
import System.Random
import Data.Markov.HMarkov.Helpers


data MarkovProcess m a = MarkovProcess { _pMap :: MarkovMap a,
                                         _g :: StdGen,
                                         _lastT :: a,
                                         _acc :: m a }
                                         deriving (Show)

makeLenses ''MarkovProcess

buildMap :: (Eq a) => V.Vector a -> MarkovMap a
buildMap xs = toMarkovMap $ V.foldl (vApply updateMarkov) (initMap xs) (makeSlices xs)

buildProc :: (Eq a, MonadPlus m) => V.Vector a -> a -> StdGen -> MarkovProcess m a
buildProc xs x gen = MarkovProcess (buildMap xs) gen x mzero

runMarkov :: (Eq a, MonadPlus m) => MarkovProcess m a -> (m a, MarkovProcess m a)
runMarkov p = let (x, g') = random $ p ^. g
                  lst = p ^. lastT
                  new = getNext lst x $ p ^. pMap
                  (acc', m) = p & acc <%~ \ac -> mplus ac $ return lst in
              (acc', m & g .~ g' & lastT .~ new)

runUntil' :: (Eq a, MonadPlus m) => (m a -> Bool) -> MarkovProcess m a -> (m a, MarkovProcess m a)
runUntil' p = runState . fix $
              \continue -> state runMarkov >>=
              \a -> if p a then pure a else continue

runUntil :: (Eq a, MonadPlus m) => (m a -> Bool) -> MarkovProcess m a -> m a
runUntil p m = fst $ runUntil' p m

