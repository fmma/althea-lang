import P, { IParser } from '../Parser'

/*
Lambda calculus grammar:

e ::= \ x . e
    | e e
    | x
    | ( e )
*/
type Exp = Var | Abs | App;
type Var = {  // x
    type: "var",
    var: string
}
type Abs = {  // \ x . e
    type: "abs",
    var: string,
    body: Exp
}
type App = {  // e e
    type: "app",
    fun: Exp,
    arg: Exp
}

// Since white-space is a semantic part of the syntax (in application (e1 e2)),
// The production rules use string instead of tokens in order to avoid discarding
// trailing whitespace.
const exp = P.fix<Exp>( exp => {
    const parens: IParser<Exp> = P.symbol("(")
        .pair(exp).token()
        .pair(P.string(")"))
        .map(p => p.fst.snd);

    const x = P.varName.map(p => <Exp>{type: "var", var: p});

    const abs: IParser<Exp> = P.symbol("\\")
        .pair(P.varName.token())
        .pair(P.symbol("."))
        .pair(exp)
        .map(p => <Exp>{type: "abs", var: p.fst.fst.snd, body: p.snd});

    const atom = parens.or(x).or(abs);

    const app : IParser<(e1: any, e2: any) => any> = 
        P.regex(/\s/)
        .token()
        .map(() => (e1: any, e2: any) => <Exp>{ type: "app", fun: e1, arg: e2});

    return atom.chainl1(app);
});

// Pretty printing lambda expressions:
function print1(exp: Exp, parens: boolean = false): string {

    function makeParens(x: string) {
        return parens ? "(" + x + ")" : x;
    }

    switch(exp.type) {
        case "var":
            return exp.var;
        case "abs": {
            return makeParens("\\ " + exp.var + ". " + print1(exp.body));
        }
        case "app": {
            return makeParens(print1(exp.fun, exp.fun.type === "abs") + " " + print1(exp.arg, true));
        }
    }
    return print1(exp);
}

let applvl = 0;

// Pretty printing lambda expressions:
function print2(exp: Exp, parens: boolean = false): string {

    function makeParens(x: string) {
        return x; // parens ? "(" + x + ")" : x;
    }

    switch(exp.type) {
        case "var":
            return exp.var;
        case "abs": {
            applvl = 0;
            return makeParens(exp.var + "." + print2(exp.body));
        }
        case "app": {
            const s1 = print2(exp.fun, exp.fun.type === "abs");
            const s2 = print2(exp.arg, true)
            return makeParens(s1 + "" + s2);
        }
    }
    return print2(exp);
}

const lambdas = [
    "(\\ x . x) \\ x . x", //Identity function applied to itself
    "(\\ x . x x) \\ x . x x", // Omega term
    "\\ f . \\x.f f  f x  ", // Chuch encoding of the number 3
    "\\ m . \\ n . \\ f . \\ x . m f (n f x)", // Chuch encoding of 'plus'
    "\\ x . x (x (x x))"
] ;

lambdas.forEach( l => {
    console.log(print1(exp.token().apply(l))); 
});

console.log(exp.printParser());


function subst(x: string, val: Exp, exp: Exp): Exp {
    switch(exp.type) {
        case "var": {
            if(x === exp.var) {
                return val;
            }
            return exp;
        }
        case "abs": {
            if(x === exp.var) {
                return exp;
            }
            return {type: "abs", var: exp.var, body: subst(x, val, exp.body)};
        }
        case "app": {
            return {type: "app", fun: subst(x, val, exp.fun), arg: subst(x, val, exp.arg)};
        }
    }
    return subst(x, val, exp);
}

function normalize(exp: Exp): Exp {
    switch(exp.type) {
        case "var":
            return exp;
        case "abs": {
            return {type: "abs", var: exp.var, body: normalize(exp.body)};
        }
        case "app": {
            const f = normalize(exp.fun);
            const x = normalize(exp.arg);
            if(f.type === "abs") {
                return normalize(subst(f.var, x, f.body));
            }
            return {type: "app", fun: f, arg: x};
        }
    }
    return normalize(exp);
}

console.log(print1(normalize(exp.token().apply("(\\ m . \\ n . \\ f . \\ x . m f (n f x)) (\\ f. \\x. f x) (\\ f. \\x. f f x)")))); // Normalize 1 + 2
