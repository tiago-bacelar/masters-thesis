module Solver.Poly (Poly, toPoly, evalPoly, numCoef, evalCoef, joinCoef, degree, norm, compNorm) where

import Utils
import CompOrd
import Lang.Expr
import Solver.Powers

import Data.List (singleton, sortOn, intersperse)
import Data.Maybe (isNothing, fromJust)
import Data.Map hiding (map, singleton, filter)
import qualified Data.Map as Map
import GHC.Exts (groupWith)
import Control.Applicative (liftA2)
import Control.Monad.Reader
import Control.Monad.State
import Data.Ratio
import GHC.Real (Ratio(..))

--coefficients are the product of an integer part and a real part for efficency
newtype Coef r = Coef { unCoef :: (Rational, Maybe r) } deriving (Functor)

numCoef :: r -> Coef r
numCoef r = Coef (1, Just r)

evalCoef :: (Fractional r) => Coef r -> r
evalCoef (Coef (n, Nothing)) = fromRational n
evalCoef (Coef (0, Just c)) = 0
evalCoef (Coef (1, Just c)) = c
evalCoef (Coef (-1, Just c)) = negate c
evalCoef (Coef (n, Just c)) = fromRational n * c

coefAux :: (Fractional a) => (Rational -> b) -> (a -> b) -> Coef a -> b
coefAux f g (Coef (n, Nothing)) = f n
coefAux f g (Coef (0, _)) = f 0
coefAux f g x = g (evalCoef x)

coef2Aux :: (Fractional a, Fractional b) => (Rational -> Rational -> c) -> (a -> b -> c) -> Coef a -> Coef b -> c
coef2Aux f g (Coef (n, Nothing)) (Coef (m, Nothing)) = f n m
coef2Aux f g (Coef (0, _)) (Coef (0, _)) = f 0 0
coef2Aux f g (Coef (n, Nothing)) (Coef (0, _)) = f n 0
coef2Aux f g (Coef (0, _)) (Coef (m, Nothing)) = f 0 m
coef2Aux f g x y = g (evalCoef x) (evalCoef y)

instance (Fractional r, Show r) => Show (Coef r) where
    show (Coef (0, _)) = "0"
    show (Coef (1, Nothing)) = "1"
    show (Coef (-1, Nothing)) = "-1"
    show (Coef (n, Nothing)) = "(" ++ show n ++ ")"
    show (Coef (n, Just c)) = show (fromRational n * c)

instance (Fractional r, Eq r) => Eq (Coef r) where
    (==) = coef2Aux (==) (==)

instance (Fractional r, Ord r) => Ord (Coef r) where
    compare = coef2Aux compare compare
    max = coef2Aux (fromRational .-. max) (numCoef .-. max)
    min = coef2Aux (fromRational .-. min) (numCoef .-. min)

instance (Fractional r, CompOrd r) => CompOrd (Coef r) where
    domCompare = coef2Aux (const .-. Top .-. compare) domCompare
    compMax = coef2Aux (Coef .-. (,Nothing) .-. max) (numCoef .-. compMax)
    compMin = coef2Aux (Coef .-. (,Nothing) .-. min) (numCoef .-. compMin)

instance (Fractional r) => Num (Coef r) where
    (+) = coef2Aux (fromRational .-. (+)) (numCoef .-. (+))
    (-) = coef2Aux (fromRational .-. (-)) (numCoef .-. (-))
    (Coef (0, _)) * _ = 0
    _ * (Coef (0, _)) = 0
    (Coef (n, mx)) * (Coef (m, my)) = Coef (n * m, maybe my (\x -> Just $ maybe x (x*) my) mx)
    negate = Coef . (negate >< id) . unCoef
    abs = Coef . (abs >< fmap abs) . unCoef
    signum (Coef (n, mx)) = case signum n of
                            0  -> 0
                            1  -> Coef (1, fmap signum mx)
                            -1 -> Coef (-1, fmap signum mx)
    fromInteger n = Coef (fromInteger n, Nothing)

instance (Fractional r) => Fractional (Coef r) where
    (Coef (0, _)) / _ = 0
    _ / (Coef (0, _)) = error "divide by 0"
    (Coef (n, mx)) / (Coef (m, my)) = Coef (n / m, maybe (fmap recip my) (\x -> Just $ maybe x (x/) my) mx)
    recip (Coef (n, mx)) = Coef (recip n, fmap recip mx)
    fromRational n = Coef (n, Nothing)

instance (Fractional r, Powers r) => Powers (Coef r) where
    pow x 0 = 1
    pow x 1 = x
    pow x n = coefAux (fromRational . (^^n)) (numCoef . (`pow` n)) x

joinCoef :: (Fractional r) => Coef (Coef r) -> Coef r
joinCoef (Coef (n, mx)) = maybe (fromRational n) (Coef . ((n*) >< id) . unCoef) mx

coefRealPow :: (Floating r, Powers r) => Coef r -> Coef r -> Coef r
coefRealPow (Coef (0, _)) (Coef (0, _)) = error "0^0"
coefRealPow (Coef (0, _)) _ = 0
coefRealPow _ (Coef (0, _)) = 1
coefRealPow x (Coef (m :% 1, Nothing)) = pow x (fromInteger m)
coefRealPow x y = numCoef $ evalCoef x ** evalCoef y



--This type doesn't support constants, i.e. non-variable unknown values.
--Because of this, all expression variables have to be inserted as polynomial variables, even
--the ones that are constant (don't have a differential expression i.e. have derivative 0).
--An alternative definition of Poly including these constants could reduce the
--number of polynomial variables and simplify the resulting polynomial projection.
newtype Poly r = Poly { unPoly :: [([Int], Coef r)] } deriving (Functor)

instance (Fractional r, Show r) => Show (Poly r) where
    show (Poly []) = "0"
    show (Poly [([],c)]) = show c
    show p = concat $ intersperse " + " [show c ++ concat (map showVar $ zip [0..] vs) | (vs,c) <- unPoly p]
        where showVar (i,0) = ""
              showVar (i,1) = "x_" ++ show i
              showVar (i,n) = "x_" ++ show i ++ "^" ++ show n

constPoly :: Coef r -> Poly r
constPoly c = Poly [([], c)]

varPoly :: (Fractional r) => Int -> Poly r
varPoly n = Poly [(replicate n 0 ++ [1], 1)]

--does not simplify the terms if s is Coef (_, Just 0)
scalePoly :: (Fractional r) => Coef r -> Poly r -> Poly r
scalePoly (Coef (0, _)) = const 0
scalePoly c = Poly . map (id >< (c*)) . unPoly

evalPoly :: (Fractional r, Powers r) => Poly r -> [r] -> Coef r
evalPoly p xs = mySum [c * myProduct (map (uncurry pow) $ filter ((/=0) . snd) $ zip xs vs) | (vs, c) <- unPoly p]
    where mySum [] = 0
          mySum l = foldTree1 (+) l
          myProduct [] = 1
          myProduct l = numCoef $ foldTree1 (*) l

--returns the value of a constant polynomial
--raises an error if the polynomial isn't constant
fromConstPoly :: Poly r -> Coef r
fromConstPoly (Poly [([], c)]) = c

instance (Fractional r) => Num (Poly r) where
    p + q = Poly $ map (\((k,c):t) -> (k, sum (c:map snd t))) $ groupWith fst $ mergeOn fst (unPoly p) (unPoly q)
    p - q = p + negate q
    p * q = Poly $ concat $ map (unPoly . foldr1 (+) . map (Poly . singleton)) $ diags (unPoly p) (unPoly q)
        where mult (k1,c1) (k2,c2) = (joinWith (+) k1 k2, c1 * c2)
              diags [] _ = []
              diags (x:xs) ys = aux xs ys [x]
              aux (x:xs) ys revs = zipWith mult revs ys : aux xs ys (x:revs)
              aux [] (y:ys) revs = zipWith mult revs (y:ys) : aux [] ys revs
              aux [] [] _ = []
    negate = Poly . map (id >< negate) . unPoly
    abs = error "Poly doesn't implement abs"
    signum = error "Poly doesn't implement signum"
    fromInteger 0 = Poly []
    fromInteger n = Poly [([], fromInteger n)]

instance (Fractional r, Powers r) => Powers (Poly r) where
    pow p 1 = p
    pow (Poly [(k,c)]) n = Poly [(map (*n) k, pow c n)]
    pow p n | n `rem` 2 == 0 = pow (p * p) (n `div` 2)
            | otherwise = p * (pow (p * p) (n `div` 2))

--the degree of a single multi-var polynomial (assumes non-zero coefficients)
--m
degree :: Poly r -> Int
degree = maximum . (0:) . map (sum . fst) . unPoly

--the L_INF subordinate norm of the transformation (assumes non-zero coefficients)
--B_N
norm :: (Ord r, Fractional r) => [Poly r] -> Coef r
norm = maximum . map (sum . map (abs . snd) . unPoly)

--same as norm, but rewritten with a CompOrd constraint
compNorm :: (CompOrd r, Fractional r) => [Poly r] -> Coef r
compNorm = compMaximum . map (sum . map (abs . snd) . unPoly)


--The gene used in the first graph iteration, agnostic to the input (a) and output (b)
type Gen a b s = a -> (a -> State s b) -> State s b
--This function performs the first iteration of the graph, which is stateful.
--The iteration has multiple start points and returns the state at the end of the
--iteration (along with the output of each of the start points, which is unused)
stateFix :: (Traversable t) => Gen a b s -> s -> t a -> (t b, s)
stateFix g i xs = runState (mapM rec xs) i
    where rec x = g x rec

--The output of the first iteration, per node, agnostic to the output of the second iteration (c)
type B t c = (Bool, Reader (t c) c)
--This function performs the second iteration. Unlike the first, there is no state.
--There is also no mechanism to detect loops. Loops are allowed as long as they don't
--introduce a data dependency loop, in which case the program hangs. Other than that,
--haskell's laziness handles everything for us
readerFix :: (Traversable t) => t (Reader (t a) a) -> t a
readerFix rs = let ans = runReader (sequence rs) ans in ans

myFMap :: (c -> c) -> State s (B t c) -> State s (B t c)
myFMap f = fmap (id >< fmap f)

myLift2 :: (c -> c -> c) -> State s (B t c) -> State s (B t c) -> State s (B t c)
myLift2 f = liftA2 aux
    where aux (isC1,r1) (isC2,r2) = (isC1 && isC2, liftA2 f r1 r2)


--The state of the first iteration
data St t c = St    { visited   :: Map Expr (B t c)
                    , vars      :: Map Expr Int --each expr has a unique number starting from 0
                    , derivExs  :: Map Ident Expr --immutable
                    }

initial :: [(Ident,Expr)] -> St t c
initial exs = St    { visited   = empty
                    , vars      = fromList [(Var (V i),j) | (j,(i,_)) <- zip [0..] exs]
                    , derivExs  = fromList exs
                    }

skipVisited :: Expr -> State (St (Map Expr) c) (B (Map Expr) c) -> State (St (Map Expr) c) (B (Map Expr) c)
skipVisited ex f = gets (Map.lookup ex . visited) >>= maybe runF (return . readVisited)
    where readVisited (isConst, _) = (isConst, asks (! ex))
          runF = do
                    --it is safe to set non-var isConst as undefined since there are no subexpression loops
                    let isConst = case ex of
                                    (Var _) -> False
                                    _       -> undefined
                    --also safe to set the second iteration as undefined because it isn't performed until the end of the first
                    modify (\st -> st { visited = insert ex (isConst, undefined) (visited st) })
                    ans <- f
                    modify (\st -> st { visited = insert ex ans (visited st) })
                    return ans

--SHOULD ONLY BE USED WHEN VISITING ex
addVar :: (Fractional r) => Expr -> State (St t c) (Poly r)
addVar ex = gets (Map.lookup ex . vars) >>= maybe runAdd (return . varPoly)
    where runAdd = do
                    j <- gets (size . vars)
                    modify (\st -> st { vars = insert ex j (vars st) })
                    return $ varPoly j

returnConst :: Reader (t c) c -> State s (B t c)
returnConst = return . (True,)

returnNotConst :: Reader (t c) c -> State s (B t c)
returnNotConst = return . (False,)

--The output of the second iteration, per node
type C r = (Poly r, Poly r)

--shortcuts to the specified function on constant polynomials
shortConst :: (Fractional r) => State s (B t (C r)) -> (Coef r -> Coef r) -> (Reader (t (C r)) (C r) -> State s (B t (C r))) -> State s (B t (C r))
shortConst x f g = do
    (isConst, aux) <- x
    if isConst
    then returnConst (fmap ((constPoly . f . fromConstPoly) >< const 0) aux)
    else g aux

shortConst2 :: (Fractional r) => State s (B t (C r)) -> State s (B t (C r)) -> (Coef r -> Coef r -> Coef r)
                          -> (Reader (t (C r)) (C r) -> Reader (t (C r)) (C r) -> State s (B t (C r)))
                          -> (Reader (t (C r)) (C r) -> Reader (t (C r)) (C r) -> State s (B t (C r)))
                          -> (Reader (t (C r)) (C r) -> Reader (t (C r)) (C r) -> State s (B t (C r)))
                          -> State s (B t (C r))
shortConst2 x y f g h i = do
    (isConst1, aux1) <- x
    (isConst2, aux2) <- y
    if isConst1
    then if isConst2
         then returnConst $ liftA2 (\(v1,_) (v2,_) -> (constPoly $ f (fromConstPoly v1) (fromConstPoly v2), 0)) aux1 aux2
         else g aux1 aux2
    else if isConst2
         then h aux1 aux2
         else i aux1 aux2


polyGen :: (Floating r, Powers r) => Gen Expr (B (Map Expr) (C r)) (St (Map Expr) (C r))
polyGen (Var T) rec = addVar (Var T) >>= returnNotConst . return . (,1)
polyGen (Var (V i)) rec = do
    mv <- gets ((Map.lookup (Var (V i))) . vars)
    if isNothing mv
    then addVar (Var (V i)) >>= returnNotConst . return . (,0)
    else do
        (_,aux) <- gets ((! i) . derivExs) >>= rec
        returnNotConst $ do
            (v,_) <- aux
            return (varPoly $ fromJust mv, v)
polyGen (Num c) rec = returnConst $ return (constPoly $ fromRational c, 0)
polyGen (Const c) rec = returnConst $ return (constPoly $ numCoef $ evalConst c, 0)
polyGen (Func Neg ex) rec = myFMap (negate >< negate) (rec ex)
polyGen (Func Exp ex) rec = shortConst (rec ex) (numCoef . exp . evalCoef) $ \aux -> do
    p <- addVar (Func Exp ex)
    returnNotConst $ do
        (_,d) <- aux
        return (p, p * d)
polyGen (Func Ln ex) rec = shortConst (rec ex) (numCoef . log . evalCoef) $ \aux -> do
    p1 <- addVar (Func Ln ex)
    (_,aux2) <- rec (Op Div (Num 1) ex)
    returnNotConst $ do
        (_,d) <- aux
        (v2,_) <- aux2
        return (p1, d * v2)
polyGen (Func Sin ex) rec = shortConst (rec ex) (numCoef . sin . evalCoef) $ \aux -> do
    p <- addVar (Func Sin ex)
    (_,aux2) <- rec (Func Cos ex)
    returnNotConst $ do
        (_,d) <- aux
        (vCos,_) <- aux2
        return (p, vCos * d)
polyGen (Func Cos ex) rec = shortConst (rec ex) (numCoef . cos . evalCoef) $ \aux -> do
    p <- addVar (Func Cos ex)
    (_,aux2) <- rec (Func Sin ex)
    returnNotConst $ do
        (_,d) <- aux
        (vSin,_) <- aux2
        return (p, - vSin * d)
polyGen (Func Tan ex) rec = shortConst (rec ex) (numCoef . tan . evalCoef) $ \aux -> do
    (_,aux2) <- rec (Func Sin ex)
    (_,aux3) <- rec (Op Div (Num 1) (Func Cos ex))
    returnNotConst $ do
        (_,d) <- aux
        (vSin,_) <- aux2
        (vSec,_) <- aux3
        return (vSin * vSec, pow vSec 2 * d)
polyGen (Op Add ex1 ex2) rec = myLift2 (\(v1, d1) (v2, d2) -> (v1 + v2, d1 + d2)) (rec ex1) (rec ex2)
polyGen (Op Sub ex1 ex2) rec = myLift2 (\(v1, d1) (v2, d2) -> (v1 - v2, d1 - d2)) (rec ex1) (rec ex2)
polyGen (Op Mult ex1 ex2) rec = myLift2 (\(v1, d1) (v2, d2) -> (v1 * v2, d1 * v2 + v1 * d2)) (rec ex1) (rec ex2)
polyGen (Op Div (Num 1) ex) rec = shortConst (rec ex) recip $ \aux -> do
    p <- addVar (Op Div (Num 1) ex)
    returnNotConst $ do
        (_,d) <- aux
        return (p, negate d * pow p 2)
polyGen (Op Div ex1 ex2) rec = shortConst2 (rec ex1) (rec ex2) (/) const1 const2 noConst
    where const1 aux1 aux2 = do
            (_,aux3) <- rec (Op Div (Num 1) ex2)
            returnNotConst $ do
                (v1,_) <- aux1
                (_,d2) <- aux2
                (v3,_) <- aux3
                return (v1 * v3, - pow v3 2 * v1 * d2)
          const2 aux1 aux2 = returnNotConst $ do
                (v1,d1) <- aux1
                (v2,_)  <- aux2
                return (scalePoly (recip $ fromConstPoly v2) v1, scalePoly (recip $ fromConstPoly v2) d1)
          noConst aux1 aux2 = do
            (_,aux3) <- rec (Op Div (Num 1) ex2)
            returnNotConst $ do
                (v1,d1) <- aux1
                (v2,d2) <- aux2
                (v3,_)  <- aux3
                return (v1 * v3, pow v3 2 * (d1 * v2 - v1 * d2))
polyGen (Op Pow (Const E) ex) rec = rec (Func Exp ex)
polyGen (Op Pow ex1 ex2) rec = shortConst2 (rec ex1) (rec ex2) coefRealPow const1 const2 noConst
    where const1 aux1 aux2 = do
            p <- addVar (Op Pow ex1 ex2)
            returnNotConst $ do
                (v1,_)  <- aux1
                (v2,d2) <- aux2
                return (p, scalePoly (numCoef $ log $ evalCoef $ fromConstPoly v1) (p * d2))
          const2 aux1 aux2 = do
            p <- addVar (Op Pow ex1 ex2)
            (_,aux4) <- rec (Op Div (Num 1) ex1)
            returnNotConst $ do
                (_,d1) <- aux1
                (v2,_) <- aux2
                (v4,_) <- aux4
                return (p, p * d1 * v2 * v4)
          noConst aux1 aux2 = do
            p <- addVar (Op Pow ex1 ex2)
            (_,aux3) <- rec (Func Ln ex1)
            (_,aux4) <- rec (Op Div (Num 1) ex1)
            returnNotConst $ do
                (_,d1)  <- aux1
                (v2,d2) <- aux2
                (v3,_)  <- aux3
                (v4,_)  <- aux4
                return (p, p * (d2 * v3 + d1 * v2 * v4))
polyGen (Op Log (Const E) ex) rec = rec (Func Ln ex)
polyGen (Op Log ex1 ex2) rec = rec (Op Div (Func Ln ex2) (Func Ln ex1))
polyGen (NatPow ex 1) rec = rec ex
polyGen (NatPow ex n) rec = shortConst (rec ex) (`pow` n) $ \aux -> do
    returnNotConst $ do
        (v,d) <- aux
        return (pow v n, scalePoly (fromIntegral n) (d * pow v (n-1)))

--turns a system of ODEs into a system of plynomial ODEs.
--if the input has n variables, the output will start with those n and may contain others afterwards
--the input never includes the independent variable (time), but the output may
toPoly :: (Floating r, Powers r) => [(Ident, Expr)] -> [(Expr, Poly r)]
toPoly exs = map (getData . fst) $ sortOn snd $ toList $ vars st
    where (_, st) = stateFix g (initial exs) [Var (V i) | (i,_) <- exs]
          g ex rec = skipVisited ex $ polyGen ex rec
          nodes = readerFix $ Map.map (fmap lazyPair . snd) $ visited st
          getData ex = (ex, snd $ nodes ! ex)
          lazyPair ~(x, y) = (x, y)
          --i do not know exactly why lazyPair is needed, but without it the function just hangs
          --so, please leave the proverbial coconut alone