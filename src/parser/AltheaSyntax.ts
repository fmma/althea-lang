import P, { IParser } from './Parser';

/*

Fib : [Nat => Nat]
Fib = 
    [ 0 => 1
    , 1 => 1
    , n => Fib[n-1] + Fib[n-2]
    ]

Fib : Nat => Nat
Fib
    0 => 1
    1 => 1
    n => Fib[n-1] + Fib[n-2]

Fac : Nat => Nat
Fac
    0 => 1
    n => n * Fac[n-1]

Record : "age" => Nat, "name" => String
Record
    "age" => 10
    "name" => "bob bob"

Project : ["age" => Nat, "name" => String] => String
Project r => r["name"]

// ["age" => Nat, "name" => String] is a sort of a dependent function type
// PI x in "age" | "name". if x == "age" then Nat else String

Fac : Nat => Nat
Fac = 
    [ 0 => 1
    , n => n * Fac[n-1]
    ]

Record : "age" => Nat, "name" => String
Record =
    [ "age" => 10
    , "name" => "bob bob"
    ]

Pair : Nat, String
Pair = [10, "bob bob"]

Array : Fin[6] => Num
Array = [1,2,3,4,5,6]

Map : 
a => b
i => a
i => b
Map = [F, A => F . A]

[m => [n => [f => m[f, n[f, x]]]]]

[m,n,f] => m[f, n[f,x]]

[ 0 => [0 => 1, 1 => 2, 2 => 3]
, 1 => [0 => 1, 1 => 2, 2 => 3]
, 2 => [1, 2, 3]
]

[ Just x => [Just y => e(x,y), Nothing]
, Nothing
]

[ Just x, Just y => e(x, y)
, Nothing
]

[x, y] => e

Map[+[-,1]]

FRONTEND

arg ::= - | exp
elt ::= exp | pat => exp
pat ::= _ | x | TypeConstructor pat ... pat
exp ::= [elt,...,elt] | exp[arg,...,arg] | x | n

CORE

exp ::= [x] => exp | exp[exp] | x | n   

desugarings:

Step1 - specialization:
F[x1,...,xk1,-,y1,...,yk2] =DEF= [_z0] => F[x1,...,xk1,_z0,y1,...,yk2]  (yi !== '-')

Step2 - currying:
F[x1,...,xk] =DEF= F[x1]...[xk]

Step3 - cocurrying:
[x1,...,xk] => e =DEF= [x1] => ... => [xk] => e


F[x,-,y] = [_z0] => F[x,_z0,y]
         = [_z0] => F[x][_z0][y]


*[-, +[-,-]] = [x] => *[x, +[-,-]]
             = [x] => *[x, [y,z] => +[y, z]]

F[x]

Zip[*[-, +[-,-]], A, B]

F[x] = x + x

Map[F[x] = x + x]

(F . G)[x] = F[G[x]]

Map[F, A] = F . A

list.map(x => {
    return x+x
})

map(x => {
    return x+x
}, list)

{ return x+x | x in list}

x in list => {
    return x+x
}

forall x : list 
{
    return x+x;
}

e ::= e e | x.e | x
t ::= t t | a

+: ab cd ac+bd

ac+bd             (ac)+(bd)+
a c+b d a(c+b)d-
ac + bd           (ac)+(bd)+
a c+bd   a(c+(bd))

x.x : aa

x.y.x : aba

x.y.y : abb

f.x.ffx : aa aa

f.g.h.x.h[f gx] : ab bc cd ad
f.g.h.x.h (f (gx)) : [a Maybe.b] [Maybe.b c] cd ad

x.x [x.x x.x x.x]

x.x[x x[x xx]]
x.x(x(x(x(xx))))
x.x    x   x  x xx

true = xy.x : tft
false = xy.y : tff
if = ctf.ctf : tfr t f r
and = xytf.x ytf f : yfx tfy t f x
or  = xytf.x t ytf : tyx tfy t f x
not = xtf.xft : tfr f t r

nothing = nj.n
just    = xnj.jx
isNothhing = m.m xy.x zxy.y
isJust     = m.m xy.y zxy.x
fromJust   = m.m _|_ x.x
maybe      = dfm.mdf

pair = xyz.zxy
fst  = p.p xy.x
snd  = p.p xy.y

id x = x  --  id = \x.x
id x      --  id x

this is shit. i want

id.a = a

list.a = 1 + a list.a

maybe.a = nothing + just a

either.ab = a + b

tree.a = a + tree.a tree.a

fix.f = f.(fix.f)

cont.ra = r^r^a

data List = \a -> Nil | Cons a (List a)

data List.a = Nil | Cons a (List a)

app: b^(b^a * a)

foo.xyz = x.(y.z)

Types:
althea-lang ... haskell
a+b         ... Foo a | Bar b
ab          ... Foo a b
f.a         ... Foo (f a)
a^b         ... Foo (a -> b)

Terms:
a b         ... (a,b)
fst         ... fst
snd         ... snd
f+g         ... \x -> case x og {Left l => f l; Right r => g r}
inl         ... Left
inr         ... Right
log(f)=x    ... f x
a^x         ... \x. a


log(x^x)=10
> 10  cancer

case (Left 10) of
    Left x => x-x
    Right y => 0

10.inl.((x-x)^x + 0^x)

I am confused!

0x = (undefined, x)

1^x = \x.()
x^1 = \().x
0^x = \x.case x of   (x :: Void)
(x+y)^a = 

1a = (1, a)

abc+3 = (a,b,c)

(xy)^a = x^(y^a)



*/