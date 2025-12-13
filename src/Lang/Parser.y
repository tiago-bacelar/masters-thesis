{

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}

module Lang.Parser (parseJaguar, ParseResult(..), Program(..), getFor) where

import Utils
import Powers
import Solver.Poly
import Lang.Expr

import Prelude hiding (Ordering(..))
import Data.Char
import Data.List
import Data.Ratio ((%))

import Data.List (sortOn)

--TODO: make ; after CB optional (after while loops for example)
--TODO: return position info along with Program to include in runtime error msgs
--TODO: warnings? (using changing var or time in for statement for time, since its wrong, but could be misleading)
}

%name parser Root
%tokentype { Token }
%error { parseError }
%lexer { lexer } { EOF }
%monad { P } { thenP } { returnP }

%token
      var       { TokenVar $$ }
      int       { TokenInt $$ }
      num       { TokenNum $$ }
      const     { TokenConst $$ }
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

Root        :: { ([String], AnyProgram) }
    : Program               {% getPState `thenP` \(_,_,vars) -> returnP (vars, $1) }

Program     :: { AnyProgram }
    : var ':=' Expr                         {% case $1 of {T -> failP "Can't assign to time"; V v -> returnP $ AnyProgram $ Assign v $3}}
    | For for Expr                          { AnyProgram $ getFor $1 (Just $3) }
    | For forever                           { AnyProgram $ getFor $1 Nothing }
    | wait Expr                             { AnyProgram $ getFor [] (Just $2) }
    | wait forever                          { AnyProgram $ getFor [] Nothing }
    | if BExpr then Program else Program    { AnyProgram $ IfThenElse $2 (anyProgram $4) (anyProgram $6) }
    | if BExpr then Program                 { AnyProgram $ IfThenElse $2 (anyProgram $4) Nop }
    | while BExpr do Program                { AnyProgram $ WhileDo $2 (anyProgram $4) }
    | Program ';' Program                   { AnyProgram $ Seq (anyProgram $1) (anyProgram $3) }
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
    | int                   { Num (fromInteger $1) }
    | num                   { Num $1 }
    | const                 { Const $1 }
    | func Expr             { Func $1 $2 }
    | Expr '+' Expr         { Op Add $1 $3 }
    | Expr '-' Expr         { Op Sub $1 $3 }
    | Expr '*' Expr         { Op Mult $1 $3 }
    | Expr '/' Expr         { Op Div $1 $3 }
    | Expr '^' Expr         { Op Pow $1 $3 }
    | Expr '^' int %prec NP {% if $3 > toInteger (maxBound :: Int) then failP ("Integer overflow: " ++ show $3 ++ " isn't a valid exponent") else if $3 == 0 then failP "0 isn't a valid exponent" else returnP $ NatPow $1 (fromInteger $3) }
    | Expr log Expr         { Op Log $3 $1 }
    | '-' Expr %prec NEG    { Func Neg $2 }
    | '(' Expr ')'          { $2 }

{

newtype AnyProgram = AnyProgram { anyProgram :: forall r. (Floating r, Powers r) => Program r }
data Program r = Assign Ident Expr
                | For [(Ident, Expr)] [(Expr, Poly r)] (Maybe Expr) --lists are returned ordered by Ident
                | IfThenElse BExpr (Program r) (Program r)
                | WhileDo BExpr (Program r)
                | Seq (Program r) (Program r)
                | Nop
            deriving (Show)

getFor :: (Floating r, Powers r) => [(Ident,Expr)] -> (Maybe Expr) -> Program r
getFor l d = For sorted (toPoly sorted) d
    where sorted = sortOn fst l


data Token = TokenVar Var
            | TokenInt Integer
            | TokenNum Rational
            | TokenConst Constant
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
type PState = (Int, Int, [String]) --line, col, vars
type P a = String -> PState -> ParseResult a

initialPState :: PState
initialPState = (1, 1, [])

getPState :: P PState
getPState = \s st -> Ok st

nextLine :: PState -> PState
nextLine (line, _, vars) = (line+1, 1, vars)

moveLine :: Int -> PState -> PState
moveLine n (line, _, vars) = (line + n, 1, vars)

moveColumn :: Int -> PState -> PState
moveColumn n (line, col, vars) = (line, col + n, vars)

lexer :: (Token -> P a) -> P a
lexer cont s =
    case s of
        []               -> cont EOF []
        '/':'/':cs       -> let (comment, rest) = span (/= '\n') s in lexer cont rest . moveColumn (length comment)
        '/':'*':cs       -> blockComment (lexer cont) s
        '\n':cs          -> lexer cont cs . nextLine
        '+':cs           -> cont (TokenOp Add) cs . moveColumn 1
        '-':cs           -> cont (TokenOp Sub) cs . moveColumn 1
        '*':cs           -> cont (TokenOp Mult) cs . moveColumn 1
        '/':cs           -> cont (TokenOp Div) cs . moveColumn 1
        '^':cs           -> cont (TokenOp Pow) cs . moveColumn 1
        '(':cs           -> cont TokenOP cs . moveColumn 1
        ')':cs           -> cont TokenCP cs . moveColumn 1
        '\'':cs          -> cont TokenDeriv cs . moveColumn 1
        ',':cs           -> cont TokenComma cs . moveColumn 1
        '<':'=':cs       -> cont (TokenComp LEQ) cs . moveColumn 2
        '>':'=':cs       -> cont (TokenComp GEQ) cs . moveColumn 2
        '<':'!':cs       -> cont (TokenComp LLT) cs . moveColumn 2
        '>':'!':cs       -> cont (TokenComp LGT) cs . moveColumn 2
        '<':cs           -> cont (TokenComp LT) cs . moveColumn 1
        '>':cs           -> cont (TokenComp GT) cs . moveColumn 1
        '!':cs           -> cont (TokenNot) cs . moveColumn 1
        '&':'&':cs       -> cont (TokenAnd) cs . moveColumn 2
        '|':'|':cs       -> cont (TokenOr) cs . moveColumn 2
        '=':cs           -> cont TokenEquals cs . moveColumn 1
        ':':'=':cs       -> cont TokenAssign cs . moveColumn 2
        ';':cs           -> cont TokenSep cs . moveColumn 1
        '{':cs           -> cont TokenOB cs . moveColumn 1
        '}':cs           -> cont TokenCB cs . moveColumn 1
        c:cs | isSpace c -> lexer cont cs . moveColumn 1
             | isDigit c -> let (x, rest, n) = lexFloat s in cont x rest . moveColumn n
             | isAlpha c -> let (word, rest) = span isAlpha s in lexAlpha cont word rest . moveColumn (length word)
             | otherwise -> \(line, col, _) -> Failed ("Unknown symbol " ++ show c ++ " at line " ++ show line ++ " column " ++ show col)

lexFloat :: String -> (Token, String, Int)
lexFloat s =
    case s1 of
        ('e':s2) -> let (e, s3, n3) = readDigits s2 in (TokenInt $ i * 10 ^ e, s3, n1 + n3 + 1)
        ('.':s2) -> let (nr, s3, n3) = readDigits s2; r = nr % (10 ^ n3) in
            case s3 of
                ('e':s4) -> let (e, s5, n5) = readDigits s4
                            in if e >= n3
                               then (TokenInt $ (i * 10 ^ n3 + nr) * 10 ^ (e - n3), s5, n1 + n3 + n5 + 2)
                               else (TokenNum $ (fromInteger i + r) * fromInteger (10 ^ e), s5, n1 + n3 + n5 + 2)
                _        -> (TokenNum $ fromInteger i + r, s3, n1 + n3 + 1)
        _       -> (TokenInt i, s1, n1)
    where (i, s1, n1) = readDigits s
          readDigits s = let (di, s') = span isDigit s in (read di, s', length di)

lexAlpha :: (Token -> P a) -> String -> P a
lexAlpha cont "e"       = cont $ TokenConst E
lexAlpha cont "pi"      = cont $ TokenConst PI
lexAlpha cont "log"     = cont $ TokenOp Log
lexAlpha cont "exp"     = cont $ TokenFunc Exp
lexAlpha cont "ln"      = cont $ TokenFunc Ln
lexAlpha cont "sin"     = cont $ TokenFunc Sin
lexAlpha cont "cos"     = cont $ TokenFunc Cos
lexAlpha cont "tan"     = cont $ TokenFunc Tan
lexAlpha cont "true"    = cont $ TokenBool True
lexAlpha cont "false"   = cont $ TokenBool False
lexAlpha cont "for"     = cont $ TokenFor
lexAlpha cont "forever" = cont $ TokenForever
lexAlpha cont "wait"    = cont $ TokenWait
lexAlpha cont "if"      = cont $ TokenIf
lexAlpha cont "then"    = cont $ TokenThen
lexAlpha cont "else"    = cont $ TokenElse
lexAlpha cont "while"   = cont $ TokenWhile
lexAlpha cont "do"      = cont $ TokenDo
lexAlpha cont "t"       = cont $ TokenVar T
lexAlpha cont var       = \cs st@(line,col,vars) ->
                            case elemIndex var vars of
                                Just i ->  cont (TokenVar $ V i) cs st
                                Nothing -> cont (TokenVar $ V $ length vars) cs (line, col, vars ++ [var])

blockComment :: P a -> P a
blockComment f s = case rest of
    '*':'/':cs -> f cs . moveColumn cols . moveLine lines
    []         -> \(line, col, _) -> Failed ("Unclosed block comment at line " ++ show line ++ " column " ++ show col)
    where (comment, rest) = spanList ((/= "*/") . take 2) s
          lines = count '\n' comment
          cols = length $ takeWhile (/= '\n') $ reverse comment



parseError :: Token -> P a
parseError t = getPState `thenP` \(line, col, _) ->
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
--parser :: P ([String], AnyProgram)

tokenize :: P [Token]
tokenize = lexer cont
    where cont t [] st = Ok [t] 
          cont t s  st = fmap (t:) (tokenize s st)

parseJaguar :: (Floating r, Powers r) => String -> ParseResult ([String], Program r)
parseJaguar = fmap (id >< (\p -> anyProgram p)) . ($ initialPState) . parser

--for testing
main = readFile "input.txt" >>= (\s -> print (parseJaguar s :: ParseResult ([String], Program Double)))
mainTokenize = readFile "input.txt" >>= print . ($ initialPState) . tokenize
}