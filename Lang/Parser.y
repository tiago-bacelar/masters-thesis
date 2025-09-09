{
module Lang.Parser (parseJaguar, ParseResult(..), Program(..), Comparator(..), BTerm(..), BExpr(..), Expr(..), Operator(..), Function(..), Var(..), AnyFloat(..), Ident) where

import Prelude hiding (Ordering(..))
import Data.Char
import GHC.Real

--TODO: make ; after CB optional (after while loops for example)
--TODO: validate var usage (unassigned vars)
--TODO: return position info along with Program to include in runtime error msgs
--TODO: warnings? (using changing var or time in for statement for time, since its wrong, but could be misleading)
}

%name parser
%tokentype { Token }
%error { parseError }
%lexer { lexer } { EOF }
%monad { P } { thenP } { returnP }

%token
      var       { TokenVar $$ }
      int       { TokenInt $$ }
      num       { TokenNum $$ }
      func      { TokenFunc $$ }
      '+'       { TokenOp Add }
      '-'       { TokenOp Sub }
      '*'       { TokenOp Mult }
      '/'       { TokenOp Div }
      '^'       { TokenOp Pow }
      log       { TokenOp Log }
      '('       { TokenOP }
      ')'       { TokenCP }
      '\''      { TokenDeriv }
      ','       { TokenComma }
      bconst    { TokenBool $$ }
      comp      { TokenComp $$ }
      '!'       { TokenNot }
      '&&'      { TokenAnd }
      '||'      { TokenOr }
      ':='      { TokenAssign }
      '='       { TokenEquals }
      for       { TokenFor }
      forever   { TokenForever }
      wait      { TokenWait }
      if        { TokenIf }
      then      { TokenThen }
      else      { TokenElse }
      while     { TokenWhile }
      do        { TokenDo }
      ';'       { TokenSep }
      '{'       { TokenOB }
      '}'       { TokenCB }

%right ';'
%nonassoc if then while do
%left else
%left '||'
%left '&&'
%right '!'
%nonassoc comp
%left '+' '-'
%left '*' '/'
%left NEG
%left log
%right '^'
%right NP
%nonassoc func
%%

Program     :: { Program }
    : var ':=' Expr                         {% case $1 of {T -> failP "Can't assign to time"; V v -> returnP (Assign v $3)}}
    | For for Expr                          { For (reverse $1) (Just $3) }
    | For forever                           { For (reverse $1) Nothing }
    | wait Expr                             { For [] (Just $2) }
    | wait forever                          { For [] Nothing }
    | if BExpr then Program else Program    { IfThenElse $2 $4 $6 }
    | if BExpr then Program                 { IfThenElse $2 $4 Nop }
    | while BExpr do Program                { WhileDo $2 $4 }
    | Program ';' Program                   { Seq $1 $3 }
    | Program ';'                           { $1 }
    | '{' Program '}'                       { $2 }


For         :: { [(Ident, Expr)] }
    : DifEq                 { [$1] }
    | For ',' DifEq         {% if elem (fst $3) (map fst $1) then failP ("Multiple equations for variable " ++ show (fst $3)) else returnP ($3:$1) }

DifEq       :: { (Ident, Expr) }
    : var '\'' '=' Expr     {% case $1 of {T -> failP "Can't alter evolution rate of time"; V v -> returnP (v, $4)}}


BExpr       :: { BExpr }
    : BTerm             { Term $1 }
    | '!' BExpr         { Not $2 }
    | BExpr '&&' BExpr  { And $1 $3 }
    | BExpr '||' BExpr  { Or $1 $3 }
    | '(' BExpr ')'     { $2 }

BTerm       :: { BTerm }
    : bconst                { BConst $1 }
    | Expr comp Expr        { Comp $2 $1 $3 }


Expr :: { Expr }
    : var                   { Var $1 }
    | int                   { Num (AnyFloat (fromInteger $1)) }
    | num                   { Num $1 }
    | func Expr             { Func $1 $2 }
    | Expr '+' Expr         { Op Add $1 $3 }
    | Expr '-' Expr         { Op Sub $1 $3 }
    | Expr '*' Expr         { Op Mult $1 $3 }
    | Expr '/' Expr         { Op Div $1 $3 }
    | Expr '^' Expr         { Op Pow $1 $3 }
    | Expr '^' int %prec NP {% if $3 > toInteger (maxBound :: Int) then failP ("Integer overflow: " ++ show $3 ++ " isn't a valid exponent") else returnP $ NatPow $1 (fromInteger $3) }
    | Expr log Expr         { Op Log $1 $3 }
    | '-' Expr %prec NEG    { Func Neg $2 }
    | '(' Expr ')'          { $2 }

{
type Ident = String
newtype AnyFloat = AnyFloat { anyFloat :: forall a. Floating a => a}

instance Show AnyFloat where
    show x = show (anyFloat x :: Double)

--TODO: sqrt (and other roots?)
data Var = T | V Ident deriving (Show)
data Function = Neg | Exp | Ln | Sin | Cos | Tan deriving (Show)
data Operator = Add | Sub | Mult | Div | Pow | Log deriving (Show)
data Expr = Var Var
            | Num AnyFloat
            | Func Function Expr
            | Op Operator Expr Expr
            | NatPow Expr Int
        deriving (Show)

data Comparator = LT | GT | LLT | LGT | LEQ | GEQ deriving (Show)
data BTerm = BConst Bool | Comp Comparator Expr Expr deriving (Show)
data BExpr = Term BTerm | Not BExpr | And BExpr BExpr | Or BExpr BExpr deriving (Show)

data Program = Assign Ident Expr
                | For [(Ident, Expr)] (Maybe Expr)
                | IfThenElse BExpr Program Program
                | WhileDo BExpr Program
                | Seq Program Program
                | Nop
            deriving (Show)



data Token = TokenVar Var
            | TokenInt Integer
            | TokenNum AnyFloat
            | TokenDot
            | TokenFunc Function
            | TokenOp Operator
            | TokenOP
            | TokenCP
            | TokenDeriv
            | TokenComma
            | TokenBool Bool
            | TokenComp Comparator
            | TokenNot
            | TokenAnd
            | TokenOr
            | TokenAssign
            | TokenEquals
            | TokenFor
            | TokenForever
            | TokenWait
            | TokenIf
            | TokenThen
            | TokenElse
            | TokenWhile
            | TokenDo
            | TokenSep
            | TokenOB
            | TokenCB
            | EOF
        deriving (Show)



data ParseResult a = Ok a | Failed String deriving (Show, Functor)
type PState = (Int, Int)
type P a = String -> PState -> ParseResult a

initialPState :: PState
initialPState = (1, 1)

getPState :: P PState
getPState = \s st -> Ok st

nextLine :: PState -> PState
nextLine (line, _) = (line+1, 1)

moveColumn :: Int -> PState -> PState
moveColumn n (line, col) = (line, col + n)

lexer :: (Token -> P a) -> P a
lexer cont s =
    case s of
        []          -> cont EOF []
        '\n':cs     -> lexer cont cs . nextLine
        '+':cs      -> cont (TokenOp Add) cs . moveColumn 1
        '-':cs      -> cont (TokenOp Sub) cs . moveColumn 1
        '*':cs      -> cont (TokenOp Mult) cs . moveColumn 1
        '/':cs      -> cont (TokenOp Div) cs . moveColumn 1
        '^':cs      -> cont (TokenOp Pow) cs . moveColumn 1
        '(':cs      -> cont TokenOP cs . moveColumn 1
        ')':cs      -> cont TokenCP cs . moveColumn 1
        '\'':cs     -> cont TokenDeriv cs . moveColumn 1
        ',':cs      -> cont TokenComma cs . moveColumn 1
        '<':'=':cs  -> cont (TokenComp LEQ) cs . moveColumn 2
        '>':'=':cs  -> cont (TokenComp GEQ) cs . moveColumn 2
        '<':'!':cs  -> cont (TokenComp LLT) cs . moveColumn 2
        '>':'!':cs  -> cont (TokenComp LGT) cs . moveColumn 2
        '<':cs      -> cont (TokenComp LT) cs . moveColumn 1
        '>':cs      -> cont (TokenComp GT) cs . moveColumn 1
        '!':cs      -> cont (TokenNot) cs . moveColumn 1
        '&':'&':cs  -> cont (TokenAnd) cs . moveColumn 2
        '|':'|':cs  -> cont (TokenOr) cs . moveColumn 2
        '=':cs      -> cont TokenEquals cs . moveColumn 1
        ':':'=':cs  -> cont TokenAssign cs . moveColumn 2
        ';':cs      -> cont TokenSep cs . moveColumn 1
        '{':cs      -> cont TokenOB cs . moveColumn 1
        '}':cs      -> cont TokenCB cs . moveColumn 1
        '#':cs      -> let (comment, rest) = span (/= '\n') cs in lexer cont rest . moveColumn (length comment + 1)
        c:cs | isSpace c -> lexer cont cs . moveColumn 1
             | isDigit c -> let (x, rest, n) = lexFloat s in cont x rest . moveColumn n
             | isAlpha c -> let (word, rest) = span isAlpha s in cont (lexAlpha word) rest . moveColumn (length word)
             | otherwise -> \(line, col) -> Failed ("Unknown symbol " ++ show c ++ " at line " ++ show line ++ " column " ++ show col)

lexFloat :: String -> (Token, String, Int)
lexFloat s =
    case s1 of
        ('e':s2) -> let (e, s3, n3) = readDigits s2 in (TokenInt $ i * 10 ^ e, s3, n1 + n3 + 1)
        ('.':s2) -> let (nr, s3, n3) = readDigits s2; r = nr % (10 ^ n3) in
            case s3 of
                ('e':s4) -> let (e, s5, n5) = readDigits s4
                            in if e >= n3
                               then (TokenInt $ (i * 10 ^ n3 + nr) * 10 ^ (e - n3), s5, n1 + n3 + n5 + 2)
                               else (TokenNum $ AnyFloat $ (fromInteger i + fromRational r) * fromInteger (10 ^ e), s5, n1 + n3 + n5 + 2)
                _        -> (TokenNum $ AnyFloat $ fromInteger i + fromRational r, s3, n1 + n3 + 1)
        _       -> (TokenInt i, s1, n1)
    where (i, s1, n1) = readDigits s
          readDigits s = let (di, s') = span isDigit s in (read di, s', length di)

lexAlpha :: String -> Token
lexAlpha "e"        = TokenNum $ AnyFloat $ exp 1
lexAlpha "pi"       = TokenNum $ AnyFloat pi
lexAlpha "log"      = TokenOp Log
lexAlpha "exp"      = TokenFunc Exp
lexAlpha "ln"       = TokenFunc Ln
lexAlpha "sin"      = TokenFunc Sin
lexAlpha "cos"      = TokenFunc Cos
lexAlpha "tan"      = TokenFunc Tan
lexAlpha "true"     = TokenBool True
lexAlpha "false"    = TokenBool False
lexAlpha "for"      = TokenFor
lexAlpha "forever"  = TokenForever
lexAlpha "wait"     = TokenWait
lexAlpha "if"       = TokenIf
lexAlpha "then"     = TokenThen
lexAlpha "else"     = TokenElse
lexAlpha "while"    = TokenWhile
lexAlpha "do"       = TokenDo
lexAlpha "t"        = TokenVar T
lexAlpha var        = TokenVar (V var)



parseError :: Token -> P a
parseError t = getPState `thenP` \(line, col) ->
                failP ("Parse error on " ++ show t ++ " (line " ++ show line ++ ", column " ++ show col ++ ")")


thenP :: P a -> (a -> P b) -> P b
m `thenP` k = \s st ->
    case m s st of
        Ok a     -> k a s st
        Failed e -> Failed e

returnP :: a -> P a
returnP a = \s st -> Ok a

failP :: String -> P a
failP err = \s st -> Failed err

catchP :: P a -> (String -> P a) -> P a
catchP m k = \s st ->
    case m s st of
        Ok a     -> Ok a
        Failed e -> k e s st



--automatically generated
--parser :: P Program

tokenize :: P [Token]
tokenize = lexer cont
    where cont t [] st = Ok [t] 
          cont t s  st = fmap (t:) (tokenize s st)


parseJaguar :: String -> ParseResult Program
parseJaguar = ($ initialPState) . parser

main = readFile "input.txt" >>= print . parseJaguar
mainTokenize = readFile "input.txt" >>= print . ($ initialPState) . tokenize
}