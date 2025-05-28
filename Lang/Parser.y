{
module Lang.Parser (parseJaguar, ParseResult(..), Program(..), BTerm(..), Comparator(..), Expr(..), Operator(..), Function(..), Var(..), AnyFloat(..), Ident) where

import Prelude hiding (Ordering(..))
import Data.Char
import GHC.Real

--TODO: validate for statements (can't have repeated var, can't use those vars in expr for time)
--TODO: validate var usage (unassigned vars)
}

%name parser
%tokentype { Token }
%error { parseError }
%lexer { lexer } { EOF }
%monad { P } { thenP } { returnP }

%token
      var       { TokenVar $$ }
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
%nonassoc comp
%left '+' '-'
%left '*' '/'
%left NEG
%left log
%right '^'
%nonassoc func
%%

Program     :: { Program }
    : var ':=' Expr                         {% case $1 of {T -> failP "Can't assign to time"; V v -> returnP (Assign v $3)}}
    | For for Expr                          { For (reverse $1) (Just $3) }
    | For forever                           { For (reverse $1) Nothing }
    | wait Expr                             { For [] (Just $2) }
    | wait forever                          { For [] Nothing }
    | if BTerm then Program else Program    { IfThenElse $2 $4 $6 }
    | if BTerm then Program                 { IfThenElse $2 $4 Nop }
    | while BTerm do Program                { WhileDo $2 $4 }
    | Program ';' Program                   { Seq $1 $3 }
    | Program ';'                           { $1 }
    | '{' Program '}'                       { $2 }


For         :: { [(Ident, Expr)] }
    : DifEq                 { [$1] }
    | For ',' DifEq         { $3 : $1 }

DifEq       :: { (Ident, Expr) }
    : var '\'' '=' Expr     {% case $1 of {T -> failP "Can't alter evolution rate of time"; V v -> returnP (v, $4)}}


BTerm       :: { BTerm }
    : bconst                { BConst $1 }
    | Expr comp Expr        { Comp $2 $1 $3 }


Expr :: { Expr }
    : var                   { Var $1 }
    | num                   { Num $1 }
    | func Expr             { Func $1 $2 }
    | Expr '+' Expr         { Op Add $1 $3 }
    | Expr '-' Expr         { Op Sub $1 $3 }
    | Expr '*' Expr         { Op Mult $1 $3 }
    | Expr '/' Expr         { Op Div $1 $3 }
    | Expr '^' Expr         { Op Pow $1 $3 }
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
        deriving (Show)

data Comparator = LT | GT | LEQ | GEQ | EQ | NEQ deriving (Show)
data BTerm = BConst Bool | Comp Comparator Expr Expr deriving (Show)
--TODO: BExpr (and, or, not)

data Program = Assign Ident Expr
                | For [(Ident, Expr)] (Maybe Expr)
                | IfThenElse BTerm Program Program
                | WhileDo BTerm Program
                | Seq Program Program
                | Nop
            deriving (Show)



data Token = TokenVar Var
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
        '(':cs      -> cont TokenOB cs . moveColumn 1
        ')':cs      -> cont TokenCB cs . moveColumn 1
        '\'':cs     -> cont TokenDeriv cs . moveColumn 1
        ',':cs      -> cont TokenComma cs . moveColumn 1
        '<':cs      -> cont (TokenComp LT) cs . moveColumn 1
        '>':cs      -> cont (TokenComp GT) cs . moveColumn 1
        '<':'=':cs  -> cont (TokenComp LEQ) cs . moveColumn 2
        '>':'=':cs  -> cont (TokenComp GEQ) cs . moveColumn 2
        '=':'=':cs  -> cont (TokenComp EQ) cs . moveColumn 2
        '!':'=':cs  -> cont (TokenComp NEQ) cs . moveColumn 2
        '=':cs      -> cont TokenEquals cs . moveColumn 1
        ':':'=':cs  -> cont TokenAssign cs . moveColumn 2
        ';':cs      -> cont TokenSep cs . moveColumn 1
        '{':cs      -> cont TokenOB cs . moveColumn 1
        '}':cs      -> cont TokenCB cs . moveColumn 1
        '#':cs      -> let (comment, rest) = span (/= '\n') cs in lexer cont rest . moveColumn (length comment + 1)
        c:cs | isSpace c -> lexer cont cs . moveColumn 1
             | isDigit c -> let (x, rest, n) = lexFloat s in cont (TokenNum x) rest . moveColumn n
             | isAlpha c -> let (word, rest) = span isAlpha s in cont (lexAlpha word) rest . moveColumn (length word)
             | otherwise -> \(line, col) -> Failed ("Unknown symbol " ++ show c ++ " at line " ++ show line ++ " column " ++ show col)

lexFloat :: String -> (AnyFloat, String, Int)
lexFloat s =
    case s1 of
        ('e':s2) -> let (e, s3, n3) = readDigits s2 in (AnyFloat $ fromInteger $ i * 10 ^ e, s3, n1 + n3 + 1)
        ('.':s2) -> let (r, s3, n3) = readFrac s2 in
            case s3 of
                ('e':s4) -> let (e, s5, n5) = readDigits s4 in (AnyFloat $ (fromInteger i + fromRational r) * fromInteger (10 ^ e), s5, n1 + n3 + n5 + 2)
                _        -> (AnyFloat $ fromInteger i + fromRational r, s3, n1 + n3 + 1)
        _       -> (AnyFloat $ fromInteger i, s1, n1)
    where (i, s1, n1) = readDigits s
          readDigits s = let (di, s') = span isDigit s in (read di, s', length di)
          readFrac s = (read dr % (10 ^ n), s', n)
                where (dr, s') = span isDigit s
                      n = length dr

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