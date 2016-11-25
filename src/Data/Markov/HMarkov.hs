{-# LANGUAGE FlexibleContexts, TemplateHaskell #-}
{-|
   Module      : Data.Markov.HMarkov
   Description : Markov sequences, Haskelly
   Copyright   : (c) Sam Raker, 2016
   License     : BSD3
   Maintainer  : sam.raker@gmail.com
   Stability   : experimental
   Portability : POSIX (FlexibleContexts, TemplateHaskell)

   Generate Markov sequences from vectors.
   The main entry points are:
      'buildProc' creates a 'MarkovProcess' from a vector of training elements,
        a starting element, and a 'System.Random.StdGen'
      'runUntil' runs a 'MarkovProcess' until a termination condition is met, and
        returns the resulting sequence
-}
module Data.Markov.HMarkov
  (
  -- * Data structures
  -- ** Map of frequencies
    MarkovMap(..)
  -- ** Complete process
  , MarkovProcess(..)
  -- * Construction helpers
  , buildMap
  , buildProc
  -- * Run processes
  , runMarkov
  , runUntil
  ) where

import Control.Lens
import Control.Monad.State
import Data.Vector as V
import System.Random
import Data.Markov.HMarkov.Helpers


-- | 'Control.Monad.State.State'-compatible wrapper around a trained
-- 'MarkovMap', which includes a 'System.Random.StdGen' and the most
-- recently-generated element, or the starting element if the process hasn't
-- been run yet
data MarkovProcess m a = MarkovProcess {
-- | Wrapped MarkovMap
  _pMap :: MarkovMap a,
  _g :: StdGen,
-- | Most recently generated element (or starting element)
  _lastT :: a,
-- | MonadPlus of already-generated elements
  _acc :: m a }
  deriving (Show)

makeLenses ''MarkovProcess

-- | Build a MarkovProcess from a vector of elements, a starting element, and a source
-- of randomness
-- NOTE: the starting element should be a member of the training vector
buildProc :: (Eq a, MonadPlus m) => V.Vector a -> a -> StdGen -> MarkovProcess m a
buildProc xs x gen = MarkovProcess (buildMap xs) gen x mzero

-- | Run a MarkovProcess once, generating a new element that is appended to the
-- accumulator
runMarkov :: (Eq a, MonadPlus m) => MarkovProcess m a -> (m a, MarkovProcess m a)
runMarkov p = let (x, g') = random $ p ^. g
                  lst = p ^. lastT
                  new = getNext lst x $ p ^. pMap
                  (acc', m) = p & acc <%~ \ac -> mplus ac $ return lst in
              (acc', m & g .~ g' & lastT .~ new)

-- | Run a MarkovProcess continually until a termination condition is met
runUntil' :: (Eq a, MonadPlus m) => (m a -> Bool) -> MarkovProcess m a -> (m a, MarkovProcess m a)
runUntil' p = runState . fix $
              \continue -> state runMarkov >>=
              \a -> if p a then pure a else continue

-- | Run a MarkovProcess continually until a termination condition is met, returning the
-- accumulator
runUntil :: (Eq a, MonadPlus m) => (m a -> Bool) -> MarkovProcess m a -> m a
runUntil p m = fst $ runUntil' p m

