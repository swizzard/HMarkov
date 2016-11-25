module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import Test.QuickCheck.Arbitrary

import Control.Monad (mzero)
import qualified Data.Vector as V
import qualified Data.Foldable as F
import System.Random

import Data.Markov.HMarkov
import Data.Markov.HMarkov.Helpers

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests, properties]

unitTests = testGroup "unit tests"
    [ testCase "vApply" $ vApply (\a b c -> a + b + c) 3 (fl [1, 2] :: V.Vector Int) @?= 6
    , testCase "vidx" $
      vidx 1 (fl [0, 1, 2]) @?= 1
    , testCase "ded w/dupes" $ ded (fl [0, 1, 1, 2] :: V.Vector Int) @?= fl [0,1,2]
    , testCase "ded w/o dupes" $
      (ded $ fl [0, 1, 2]) @?= fl [0,1,2]
    , testCase "makeSlices" $
      (makeSlices $ fl [0, 1, 2, 3]) @?=
        (fl [(fl [0, 1]), (fl [1, 2]),
                    (fl [2, 3])])
    , testCase "nrmlz" $
      (nrmlz $ fl [0.0, 2.0, 3.0]) @?=
        fl [0.0, 0.4, 0.6]
    , testCase "sumP" $ sumP (fl [0.0, 0.1, 0.1, 0.2]) @?=
        fl [0.0, 0.1, 0.2, 0.4]
    , testCase "pix end" $ pix 0.1 (fl [0.0, 0.1]) @?= 1
    , testCase "pix start" $ pix 0.1 (fl [0.1, 0.2]) @?= 0
    , testCase "pix mid" $ pix 0.2 (fl [0.1, 0.2, 0.3]) @?= 1
    , testCase "getNext" $ getNext 'a' 0.1 (MarkovMap (fl ['a', 'b'])
                                              (fl [fl [0.0, 0.1],
                                                           fl [0.0, 0.0]])) @?= 'b'
    ]

properties = testGroup "properties"
  [ QC.testProperty "runMarkov yields elems from input" $
    \(NonEmpty lst) -> F.and (V.map (flip Prelude.elem (lst :: [Int])) $
      (fst $ runMarkov (bp lst)))
  ]

fl = V.fromList
bp l = buildProc (fl l) (l !! 0) (mkStdGen 0)
