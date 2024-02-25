import Test.Tasty
import Test.Tasty.HUnit (testCase, (@?=))

import qualified Data.Range as R
import Data.Range.Internal (Span(..), Range(..))
import qualified Data.Range.Internal as RI

main = defaultMain $ testGroup "Range"
  [ construction
  , union
  , fromList
  , include
  , difference
  , getNth
  ]

construction = testGroup "Construction"
  [ testCase "empty" $ (R.empty :: Range Int) @?= (Range [] 0)
  , testCase "singleton" $ (R.singleton 1) @?= (Range [Span 1 1 1] 1)
  , testCase "simple range" $ (R.range 1 5) @?= (Range [Span 1 5 5] 5)
  , testCase "singelton range" $ (R.range 1 1) @?= (R.singleton 1)
  ]

union = testGroup "Unions"
  [ testCase "entirely below" $ (R.union (R.range 4 6) (R.range 0 2)) @?= (Range [Span 0 2 3, Span 4 6 3] 6)
  , testCase "at lower bound" $ (R.union (R.range 4 6) (R.range 0 4)) @?= (R.range 0 6)
  , testCase "covering lower" $ (R.union (R.range 4 6) (R.range 0 5)) @?= (R.range 0 6)
  , testCase "<= upper bound" $ (R.union (R.range 4 6) (R.range 0 7)) @?= (R.range 0 7)
  , testCase "matching bound" $ (R.union (R.range 4 6) (R.range 4 6)) @?= (R.range 4 6)
  , testCase "within bounds"  $ (R.union (R.range 4 6) (R.range 5 6)) @?= (R.range 4 6)
  , testCase "<= lower bound" $ (R.union (R.range 4 6) (R.range 4 9)) @?= (R.range 4 9)
  , testCase "covering upper" $ (R.union (R.range 4 6) (R.range 5 9)) @?= (R.range 4 9)
  , testCase "at upper bound" $ (R.union (R.range 4 6) (R.range 6 9)) @?= (R.range 4 9)
  , testCase "entirely above" $ (R.union (R.range 4 6) (R.range 8 9)) @?= (Range [Span 4 6 3, Span 8 9 2] 5)
  ]

fromList = testGroup "From List"
  [ testCase "singleton" $ (R.fromList [ 1 ]) @?= (R.singleton 1)
  , testCase "duplicates" $ (R.fromList [ 1, 1, 1, 1, 1 ]) @?= (R.singleton 1)
  , testCase "continuous" $ (R.fromList [ 1, 2, 3, 4, 5 ]) @?= (R.range 1 5)
  , testCase "unordered" $ (R.fromList [ 5, 2, 3, 1, 4 ]) @?= (R.range 1 5)
  , testCase "disjoint" $ (R.fromList [ 1, 2, 3, 6, 7, 8 ]) @?= (R.union (R.range 1 3) (R.range 6 8))
  , testCase "unordered disjoint" $ (R.fromList [ 7, 8, 1, 3, 6, 2 ]) @?= (R.union (R.range 1 3) (R.range 6 8))
  , testCase "leap frog" $ (R.fromList [ 4, 1, 5, 2, 6, 3 ]) @?= (R.range 1 6)
  , testCase "leap frog 2" $ (R.fromList [ 1, 4, 2, 5, 3, 6 ]) @?= (R.range 1 6)
  ]

include = testGroup "Include"
  [ testCase "before" $ (R.include 0 (R.range 3 5)) @?= (R.union (R.singleton 0) (R.range 3 5))
  , testCase "lower bound" $ (R.include 3 (R.range 3 5)) @?= (R.range 3 5)
  , testCase "within" $ (R.include 4 (R.range 3 5)) @?= (R.range 3 5)
  , testCase "upper bound" $ (R.include 5 (R.range 3 5)) @?= (R.range 3 5)
  , testCase "after" $ (R.include 8 (R.range 3 5)) @?= (R.union (R.range 3 5) (R.singleton 8))
  ]

difference = testGroup "Difference"
  [ testCase "entirely below" $ (R.difference (R.range 4 7) (R.range 0 2)) @?= (R.range 4 7)
  , testCase "at lower bound" $ (R.difference (R.range 4 7) (R.range 0 4)) @?= (R.range 5 7)
  , testCase "covering lower" $ (R.difference (R.range 4 7) (R.range 0 5)) @?= (R.range 6 7)
  , testCase "<= upper bound" $ (R.difference (R.range 4 7) (R.range 0 7)) @?= (R.empty :: Range Int)
  , testCase "matching bound" $ (R.difference (R.range 4 7) (R.range 4 7)) @?= (R.empty :: Range Int)
  , testCase "within bounds"  $ (R.difference (R.range 4 7) (R.range 5 6)) @?= R.union (R.singleton 4) (R.singleton 7)
  , testCase "<= lower bound" $ (R.difference (R.range 4 7) (R.range 4 9)) @?= (R.empty :: Range Int)
  , testCase "covering upper" $ (R.difference (R.range 4 7) (R.range 6 9)) @?= (R.range 4 5)
  , testCase "at upper bound" $ (R.difference (R.range 4 7) (R.range 7 9)) @?= (R.range 4 6)
  , testCase "entirely above" $ (R.difference (R.range 4 7) (R.range 8 9)) @?= (R.range 4 7)
  ]

getNth = testGroup "GetNth"
  [ testCase "zeroth" $ (R.getNth 0 (R.range 0 10)) @?= 0
  , testCase "mid" $ (R.getNth 5 (R.range 0 10)) @?= 5
  , testCase "last" $ (R.getNth 10 (R.range 0 10)) @?= 10
  , testCase "disjoint first" $ (R.getNth 5 (R.union (R.range 0 4) (R.range 6 10))) @?= 6
  , testCase "disjoint mid" $ (R.getNth 7 (R.union (R.range 0 4) (R.range 6 10))) @?= 8
  , testCase "disjoint last" $ (R.getNth 4 (R.union (R.range 0 4) (R.range 6 10))) @?= 4
  , testCase "very disjoint 1" $ (R.getNth 4 (R.unions [ R.range 0 2, R.range 4 6, R.range 8 10 ])) @?= 5
  , testCase "very disjoint 2" $ (R.getNth 5 (R.unions [ R.range 0 2, R.range 4 6, R.range 8 10 ])) @?= 6
  , testCase "very disjoint 3" $ (R.getNth 7 (R.unions [ R.range 0 2, R.range 4 6, R.range 8 10 ])) @?= 9
  ]