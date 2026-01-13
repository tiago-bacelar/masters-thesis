{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}


import Data.Ratio ((%))
import Data.Proxy (Proxy(..))
import Data.CallStack (HasCallStack)
import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.SmallCheck.Series

import Utils
import CompOrd
import CompReal
import Boundable
import Powers
import Solver.Poly
import Solver.Solver
import Generators
import Assert

import qualified CompReal.Instances.CDAR as CDAR
import qualified CompReal.Instances.AERN2 as AERN2
import qualified CompReal.Instances.ERA as ERA
import qualified CompReal.Instances.ExactReal as ExactReal
import qualified CompReal.Instances.IReal as IReal


type TestNum r = (CompReal r, Powers r, Boundable r, Show r)
data SomeProxy where SomeProxy :: forall r. TestNum r => Proxy r -> SomeProxy

{-
Tests the listed CompReal instances for correction. Smallcheck is used to check the
properties a CompReal should satisfy. The depth of the search is the maximum accuracy
(e.g. depth of 10 means accuracies [0..10] are tested). The CompReal generator is
hardcoded to 20 different CompReals, obtained with fromRational, sin, limits, pow, etc
-}
main :: (HasCallStack) => IO ()
main = defaultMain $ localOption depth $ testGroup "Unit tests" $ uncurry unitTests <$>
    [ ("CDAR",      SomeProxy (Proxy :: Proxy CDAR.CR))
    , ("AERN2",     SomeProxy (Proxy :: Proxy AERN2.CReal))
    , ("ERA",       SomeProxy (Proxy :: Proxy ERA.CReal))
    , ("ExactReal", SomeProxy (Proxy :: Proxy ExactReal.AnyCReal))
    , ("IReal",     SomeProxy (Proxy :: Proxy IReal.IReal))
    ]
    where depth = 19 :: SmallCheckDepth

unitTests :: (HasCallStack) => String -> SomeProxy -> TestTree
unitTests name someProxy = case someProxy of
    (SomeProxy proxy) -> testGroup name
        [ compRealTests proxy
        , powersTests proxy
        , rationalLimitTests proxy
        , limitTests proxy
        , compOrdTests proxy
        , odeTests proxy
        ]

compRealTests :: forall r. (TestNum r, HasCallStack) => Proxy r -> TestTree
compRealTests _ = testGroup "CompReal instance"
    [ SC.testProperty "bound order" $
        \(CRS x) (Acc n) -> uncurry (<=) $ bound (x :: r) n

    , SC.testProperty "bound width" $
        \(CRS x) (Acc n) -> uncurry (flip (-)) (bound (x :: r) n) @?<= 1%pow2 n

    ,  SC.testProperty "approx ∈ bound" $
        \(CRS x) (Acc n) -> approx (x :: r) n @?∈ bound x n

    , SC.testProperty "fromRational" $
        \(Rat a) -> (fromRational a :: r) @?~ a

    , SC.testProperty "sum" $
        \(Rat a) (Rat b) -> (fromRational a + fromRational b :: r) @?~ a + b

    , SC.testProperty "product" $
        \(Rat a) (Rat b) -> (fromRational a * fromRational b :: r) @?~ a * b

    , SC.testProperty "division" $
        \(Rat a) (NonZero (Rat b)) -> (fromRational a / fromRational b :: r) @?~ a / b

    , SC.testProperty "recip" $
        \(NonZero (Rat a)) -> (recip $ fromRational a :: r) @?~ recip a

    --I was going to use this as a test for sin and cos, but CDAR's trig is so slow I can't lol
    -- , SC.testProperty "pythagorean identity (sin^2 + cos^2 = 1)" $
    --     \(CRS x) -> (sin x ^ 2 + cos x ^ 2 :: r) @?~ 1

    {-
        An error in any of the following 3 tests may not be a problem of the
        CompReal. Since the approximations here only have an accuracy of 300,
        should the CompReal generate a more accurate approximation the test
        may report a failure. 
    -}
    , SC.testProperty "pi" $
        (pi :: r) @?~ ratPi --only accurate up to n ~ 300

    , SC.testProperty "e" $
        (exp 1 :: r) @?~ ratE --only accurate up to n ~ 300

    , SC.testProperty "sqrt 2" $
        (sqrt 2 :: r) @?~ ratSqrt2 --only accurate up to n ~ 300
    ]
    where ratPi    = 3141592653589793238462643383279502884197169399375105820974944592307816406286208998628034825342117067982148 % 10^(105 :: Int)
          ratE     = 2718281828459045235360287471352662497757247093699959574966967627724076630353547594571382178525166427427466 % 10^(105 :: Int)
          ratSqrt2 = 1414213562373095048801688724209698078569671875376948073176679737990732478462107038850387534327641572735013 % 10^(105 :: Int)

powersTests :: forall r. (TestNum r, HasCallStack) => Proxy r -> TestTree
powersTests _ = testGroup "Powers instance"
    [ SC.testProperty "pow" $
        \(Rat a) (NonNegative i) -> (pow (fromRational a) i :: r) @?~ a^^i
    ]

rationalLimitTests :: forall r. (TestNum r, HasCallStack) => Proxy r -> TestTree
rationalLimitTests _ = testGroup "Limit Rational r instance"
    [ SC.testProperty "geometric series (r=1/2)" $
        (geometricSeriesDyadRat :: r) @?~ 1

    , SC.testProperty "geometric series (r=1/3)" $
        (geometricSeriesRat :: r) @?~ 0.5

    , SC.testProperty "finite list [-0.2, 0.045, 1/6, 2/7]" $
        (finiteListRat :: r) @?~ 2%7
    ]

limitTests :: forall r. (TestNum r, HasCallStack) => Proxy r -> TestTree
limitTests _ = testGroup "Limit r r instance"
    [ SC.testProperty "geometric series (r=1/2)" $
        (geometricSeriesDyadCR :: r) @?~ 2

    , SC.testProperty "geometric series (r=1/3)" $
        (geometricSeriesCR :: r) @?~ 1.5

    , SC.testProperty "finite list [0.8, 1.045, 7/6, 9/7]" $
        (finiteListCR :: r) @?~ 9%7
    ]

--Errors in these tests usually indicate that the Limit instance is not correct
--Solve errors in rationalLimitTests and limitTests before trying to solve errors here
compOrdTests :: forall r. (TestNum r, HasCallStack) => Proxy r -> TestTree
compOrdTests _ = testGroup "CompOrd instance"
    [ SC.testProperty "domCompare is monotonic" $
        \(CRS x) (CRS y) (Acc n) -> domCompare (x :: r) (y :: r) n `extendedBy` domCompare x y (n+1)

    , SC.testProperty "domCompare inversion" $
        \(CRS x) (CRS y) (Acc n) -> domCompare (x :: r) (y :: r) n `consistent` invert (domCompare y x n)

    , SC.testProperty "domCompare respects accuracy (not bottom)" $
        \(Rat a) (Rat b) (Acc n) -> abs (a - b) >= 2 % pow2 n ==> not $ isBottom $ domCompare (fromRational a :: r) (fromRational b :: r) n

    , SC.testProperty "domCompare respects accuracy (top)" $
        \(Rat a) (Rat b) (Acc n) -> abs (a - b) > 2 % pow2 n ==> isTop $ domCompare (fromRational a :: r) (fromRational b :: r) n
    
    , SC.testProperty "infCompare is consistent" $
        \(Distinct2 (CRS x, CRS y)) -> let (mm,o) = infCompare (x :: r) (y :: r)
                                       in maybe True (\m -> Middle m `extendedBy` Top o) mm

    , SC.testProperty "infCompare extends domCompare" $
        \(Distinct2 (CRS x, CRS y)) (Acc n) -> let (mm,o) = infCompare (x :: r) (y :: r)
                                               in maybe (Top o) Middle mm `consistent` domCompare x y n

    , SC.testProperty "min" $
        \(Rat a) (Rat b) -> compMin (fromRational a :: r) (fromRational b :: r) @?~ min a b

    , SC.testProperty "max" $
        \(Rat a) (Rat b) -> compMax (fromRational a :: r) (fromRational b :: r) @?~ max a b
    ]

--These tests aren't really testing the CompReal instance.
--Rather, they are testing the ODE solver. If a CompReal instance 
--passed all previous tests but failed here, the problem is likely
--in the implementation of solvePoly, not on the instance.
odeTests :: forall r. (TestNum r, HasCallStack) => Proxy r -> TestTree
odeTests _ = testGroup "ODE solver"
    [ SC.testProperty "linear ODE" $
        \(Rat a) (Rat x) -> solvePoly [(0, constPoly $ fromRational a)] (fromRational x :: r) !! 0 @?~ a * x

    , SC.testProperty "quadratic ODE (varying a)" $
        \(Rat a) (Rat x) -> solvePoly [(0, varPoly 1), (0, constPoly $ fromRational a)] (fromRational x :: r) !! 0 @?~ a/2 * x*x

    , SC.testProperty "quadratic ODE (varying b)" $
        \(Rat b) (Rat x) -> solvePoly [(0, varPoly 1), (fromRational b, constPoly 2)] (fromRational x :: r) !! 0 @?~ x*x + b*x

    , SC.testProperty "exponential ODE" $
        solvePoly [(1, varPoly 0)] (1 :: r) !! 0 @?~ ratE
    ]
    where ratE = 2718281828459045235360287471352662497757247093699959574966967627724076630353547594571382178525166427427466 % 10^(105 :: Int)