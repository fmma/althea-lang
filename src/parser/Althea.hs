module Althea where

import Text.ParserCombinators.ReadP
import Data.List
import Data.Char

{-

n ::= <natural number>
x ::= <identifier> : t


e ::=
    n |
    x => e | e e | x |
    let x = e in e |
    letrec x = e in e |
    fst e | snd e | <e, e> |
    inl t e | inr t e | [e, e]
t ::= Nat | t -> t | t * t | t + t

v ::= 
    n | 
    x => e |
    <v, v> |
    inl t v | inr t v

|v| = n
|n| = 32
|<v1, v2>| = |v1| + |v2|
|x => e| = ??
|inl v| = 1 + |v|
|inr v| = 1 + |v|

G |- e : t

--
G |- n : Nat

G, x:t1 |- e0 : t2
--
G |- x:t1 => e0 : t1 -> t2

G |- e1 : t2 -> t1
G |- e2 : t2
--
G |-e1 e2 : t1

(G x = t)
--
G |- x : t : t

G |- e1 : t1
G, x:t1 |- e2 : t2
--
G |- let x:t1 = e1 in e2 : t2

G |- e1 : t1
G, x:t1 |- e2 : t2
--
G |- let x:t1 = e1 in e2 : t2

G, x:t1 |- e1 : t1
G, x:t1 |- e2 : t2
--
G |- letrec x:t1 = e1 in e2 : t2

G |- e0 : t1 * t2
--
G |- fst e0 : t1

G |- e0 : t1 * t2
--
G |- snd e0 : t2

G |- e1 : a -> b
G |- e2 : a -> c
--
G |- <e1, e2> : a -> b * c

G |- e0 : t1
--
G |- inl t2 e0 : t1 + t2

G |- e0 : t2
--
G |- inr t1 e0 : t1 + t2

G |- e1 : a -> c
G |- e2 : b -> c
--
G |- [e1, e2] : a + b -> c


e ==> v

--
n ==> n

--
x => e ==> x => e

e1 ==> x => e1'
e2 ==> v2
--
e1 e2 ==> e1'[x/v2] 

e1 ==> v1
e2[x/v1] ==> v2
--
let x = e1 in e2 ==> v2

e1[x/letrec x = e1 in x] ==> v1
e2[x/v1] ==> v2
--
letrec x = e1 in e2

e0 ==> <v1, v2>
--
fst e0 ==> v1

e ::=
    n |
    x => e | e e | x |
    let x = e in e |
    letrec x = e in e |
    fst e | snd e | <e, e> |
    inl t e | inr t e | [e, e]


[ 
Fib[n] =>
    [1, 1, n => Fib[n-2] + Fib[n-1]][n],

Fac[n] =>
    [1, n => n * Fac[n-1]][n],

Plus[x,y] => x + y
]
-}

fix = "[f => [x => f[x[x]]][[x => f[x[x]]]]]"

fac = "[fac => [1, n => n * fac[n-1]]]"

test = fix ++ "[" ++ fac ++ ", 10]"

omega = "[x => x[x]]"

modul = "\
\ [m => m[Fac]][[\
\ Fib => [1, 1, n => Fib[n-2] + Fib[n-1]],\
\ \
\ Fac => [fac => [1, n => n * fac[n-1]]],\
\ \
\ MyPlus[x,y] => x + y\
\ ]]"

fib = "[Fib[n] => [1, 1, n => Fib[n-2] + Fib[n-1]][n]][Fib[1]]"

data Arg = ArgOpen | ArgExp Exp
data Elt = EltGuarded Pat Exp | EltExp Exp 
data Pat = PatWild | PatVar String | PatIndex String [Pat]
data Exp 
    = Name String
    | X String
    | Index Exp [Arg]
    | Array [Elt]

go x = do
    let e = read x
        e' = desugar e
        v = eval e'
    putStrLn (show e)
    putStrLn (show e')
    putStrLn (show v)

argParser :: ReadP Arg
argParser =
    (skipSpaces >> string "-" >> pure ArgOpen) +++
    (ArgExp <$> expParser)

eltParser :: ReadP Elt
eltParser =
    ((\ p _ e -> EltGuarded p e) <$> patParser <*> (skipSpaces >> string "=>") <*> expParser) +++
    (EltExp <$> expParser)

varParser :: ReadP String
varParser = skipSpaces >> ((:) <$> satisfy isLower <*> many (satisfy isAlphaNum))

nameParser :: ReadP String
nameParser = skipSpaces >> ((:) <$> satisfy isUpper <*> many (satisfy isAlphaNum)) +++
                           (many1 (satisfy isNumber))

prodParser =
    (skipSpaces >> string "*" >> return (\e1 e2 -> Index (Name "Times") [ArgExp e1, ArgExp e2])) +++
    (skipSpaces >> string "/" >> return (\e1 e2 -> Index (Name "Div") [ArgExp e1, ArgExp e2]))

sumParser =
    (skipSpaces >> string "+" >> return (\e1 e2 -> Index (Name "Plus") [ArgExp e1, ArgExp e2])) +++
    (skipSpaces >> string "-" >> return (\e1 e2 -> Index (Name "Minus") [ArgExp e1, ArgExp e2]))

patParser :: ReadP Pat
patParser =
    (skipSpaces >> string "_" >> pure PatWild) +++
    (PatVar <$> varParser) +++
    (PatIndex <$> nameParser <*> between (string "[") (skipSpaces >> string "]") (sepBy patParser (skipSpaces >> string ","))) +++
    ((\ n -> PatIndex n []) <$> nameParser) +++
    between (skipSpaces >> string "(") (skipSpaces >> string ")") patParser

indexParser = (\ args e -> Index e args) <$> between (string "[") (skipSpaces >> string "]") (sepBy argParser (skipSpaces >> string ","))

expParser = chainl1 (chainl1 expParserAtom prodParser) sumParser

expParserAtom = 
    (\ e ix -> foldl (flip ($)) e ix) 
    <$> 
    ( (Name <$> nameParser) +++
      (X <$> varParser) +++
      (Array <$> between (skipSpaces >> string "[") (skipSpaces >> string "]") (sepBy eltParser (skipSpaces >> string ","))) +++
      between (skipSpaces >> string "(") (skipSpaces >> string ")") expParser
    )
    <*> many indexParser

instance Read Exp where
    readsPrec _ = readP_to_S expParser

instance Show Arg where
    show a =
        case a of
            ArgOpen -> "-"
            ArgExp e -> show e

instance Show Elt where
    show e =
        case e of
            EltGuarded p e -> show p ++ " => " ++ show e
            EltExp e -> show e

instance Show Pat where
    show p =
        case p of
            PatWild -> "_"
            PatVar x -> x
            PatIndex p0 ps -> p0 ++ "[" ++ intercalate ", " (map show ps) ++ "]"

instance Show Exp where
    show e =
        case e of
            Name x -> x
            X x -> x
            Index e0 args -> show e0 ++ "[" ++ intercalate ", " (map show args) ++ "]"
            Array elts -> "[" ++ intercalate ", " (map show elts) ++ "]"

genVar :: Int -> String
genVar i = "_z" ++ show i

desugar :: Exp -> Core
desugar e =
    let (e', _) = desugar' 0 e
    in e'

desugar' :: Int -> Exp -> (Core, Int)
desugar' i e =
    case e of
        Name x -> (Const x, 0)
        X x -> (Var x, 0)
        Index e0 args -> 
            let (e0', d1) = desugar' i e0
                (e', d2) = desugarIndex (i + d1) e0' args
                in (e', d1 + d2)
        Array elts -> desugarArray i elts

desugarIndex :: Int -> Core -> [Arg] -> (Core, Int)
desugarIndex i e args =
    let 
        f [] e0 = e0
        f (i:is) e0 = lam (genVar i) (f is e0)
        (e', d, lams) = desugarIndex' i e args
    in (f lams e', d)

desugarIndex' :: Int -> Core -> [Arg] -> (Core, Int, [Int])
desugarIndex' i e args =
    case args of
        [] -> (e, 0, [])
        (a:as) ->
            case a of
                ArgOpen ->
                    let (e', d, lams) = desugarIndex' (i + 1) (App e (Var $ genVar i)) as
                    in (e', d + 1, i:lams)
                ArgExp e0 ->
                    let (e0', d1) = desugar' i e0
                        (e', d2, lams) = desugarIndex' (i + d1) (App e e0') as
                    in (e', d1 + d2, lams)
 
desugarPatten :: Int -> Pat -> (Core, Int)
desugarPatten i p =
    case p of
        PatWild -> (Var (genVar i), 1)
        PatVar x -> (Var x, 0)
        PatIndex x ps -> desugarPattenIndex i (Const x) ps

desugarPattenIndex :: Int -> Core -> [Pat] -> (Core, Int)
desugarPattenIndex i e ps =
    case ps of
        [] -> (e, 0)
        (p0:ps0) ->
            let (e0, d1) = desugarPatten i p0
                (e', d2) = desugarPattenIndex (i + d1) (App e e0) ps0
            in (e', d1 + d2)

desugarArray :: Int -> [Elt] -> (Core, Int)
desugarArray i elts = desugarArray' 0 i elts

desugarArray' :: Int -> Int -> [Elt] -> (Core, Int)
desugarArray' n i elts =
    case elts of
        [] -> (void, 0)
        (e:es) ->
            case e of
                EltGuarded p e0 ->
                    let (p', d1) = desugarPatten i p
                        (e0', d2) = desugar' (i + d1) e0
                        (e1', d3) = desugarArray' (n + 1) (i + d1 + d2) es
                    in (Match p' e0' e1', d1 + d2 + d3) 
                EltExp e0 -> desugarArray' n i (EltGuarded (PatIndex (show n) []) e0:es)

data Core
    = Const String
    | Var String
    | App Core Core
    | Match Core Core Core

instance Show Core where
    show e =
        case e of
            Const x -> x
            Var x -> x
            App e1 e2 -> show e1 ++ "[" ++ show e2 ++ "]"
            Match (Var x) e1 e2 -> "{" ++ show (Var x) ++ " => " ++ show e1 ++ "}"
            Match p e1 e2 -> "{" ++ show p ++ " => " ++ show e1 ++ ", " ++ show e2 ++ "}"

lam x e = Match (Var x) e void

pair x y = App (App (Const "Pair") x) y
pair2 x y = App (App (Const "Pair2") x) y

void = Const "Bot"

ex1 = App (Match (pair (Var "a") (Var "b")) (pair (Var "b") (Var "a")) void) (pair (Var "x") (Var "y"))

idexp = lam "x" (Var "x")

buildtinBinop "Plus" = Just (+)
buildtinBinop "Times" = Just (*)
buildtinBinop "Minus" = Just (-)
buildtinBinop "Div" = Just div
buildtinBinop _ = Nothing

builtin e =
    case e of
        App (App (Const op) (Const x)) (Const y) | Just f <- buildtinBinop op -> Just $ Const (show $ (read x `f` read y))
        _ -> Nothing

eval :: Core -> Core
eval e =
    case e of
        App e1 e2 ->
            let v1 = eval e1
                v2 = eval e2
            in 
            case (v1, v2) of
                _ | Just x <- builtin (App v1 v2) -> x
                (Match p e3 e4, _) -> 
                    case pm p v2 of
                        Nothing -> eval (App e4 v2)
                        Just s -> eval (subst s e3)
                _ -> App v1 v2
        _ -> e

subst :: [(String, Core)] -> Core -> Core
subst s e =
    case e of
        Const x -> Const x
        Var x ->
            case lookup x s of
                Nothing -> Var x
                Just e' -> e'
        App e1 e2 -> App (subst s e1) (subst s e2)
        Match e1 e2 e3 -> 
            Match e1 (subst (filter f s) e2) (subst s e3)
                where f (x, _) = x `notElem` freeVars e1

freeVars :: Core -> [String]
freeVars e =
    case e of
        Const _ -> []
        Var x -> [x]
        App e1 e2 -> nub $ freeVars e1 ++ freeVars e2
        Match e1 e2 e3 -> (freeVars e2 \\ freeVars e1) ++ freeVars e3

pm :: Core -> Core -> Maybe [(String, Core)]
pm p e =
    case (p, e) of
        (Const x, Const y) | x == y -> Just []
        (Var x, _) -> Just [(x, e)]
        (App p1 p2, App e1 e2) -> (++) <$> pm p1 e1 <*> pm p2 e2
        _ -> Nothing
