import P, { IParser } from '../Parser';

/*
Calculator grammer:

sum_op ::= '+' | '-'
prod_op ::= '*' | '/'
exp ::= sum
sum ::= prod (sum_op prod)*
prod ::= atom (prod_op atom)*
atom ::= <number> | '(' exp ')' | <ident> '(' exp ')'
*/

// This files contains two versions that implement the above parser.
// The first one is perhaps easier to understand, but uses js scoping
// tricks in a way that breaks printParser. For parsing, it works just fine.
//
// Both parsers are also evaluators as they evaluate the parsed expression immediately. 

const times: IParser<(a: number, b: number) => number> =
    P.symbol("*")
    .map(() => (a: number, b: number) => a * b);

const div: IParser<(a: number, b: number) => number> =
    P.symbol("/")
    .map(() => (a: number, b: number) => a / b);

const plus: IParser<(a: number, b: number) => number> =
    P.symbol("+")
    .map(() => (a: number, b: number) => a + b);

const minus: IParser<(a: number, b: number) => number> =
    P.symbol("-")
    .map(() => (a: number, b: number) => a - b);

const parens: IParser<number> =
    P.symbol("(")
    .pair(P.recurse(() => exp)) // "exp" is defined further down. This is the "trick"
    .pair(P.symbol(")"))
    .map(p => p.fst.snd);

const fun =
    P.symbol("sqrt")
    .pair(P.symbol("("))
    .pair(P.recurse(() => exp))
    .pair(P.symbol(")"))
    .map( p => Math.sqrt(p.fst.snd) )

const atom: IParser<number> =
    P.number.token().or(parens).or(fun);

const prod : IParser<number> =
    atom.chainl1(times.choice(div));

const sum: IParser<number> =
    prod.chainl1(plus.choice(minus));

const exp: IParser<number> = sum;

const exampleSrc1 = "100"
const exampleSrc = "(((((((sqrt(   ( (200+11) *3 / 100.1   + 1000)) )))))))"
const parseResult = exp.apply(exampleSrc);

console.log("Parsing \"" + exampleSrc + "\" gives:");
console.log(parseResult);


{  // "correct" way of using fix (i.e. as a fix point of exp):
    const exp = P.fix((exp: IParser<number>) => {
        const parens = P.parens("(", exp, ")");

        const fun =
            P.symbol("sqrt")
            .pair(parens)
            .map( p => Math.sqrt(p.snd) )

        const atom = P.number.token().or(parens).or(fun);

        const prod = atom.chainl1(times.choice(div));
        const sum = prod.chainl1(plus.choice(minus));
        return sum;
    });

    const parseResult = exp.apply(exampleSrc);

    console.log("Parsing \"" + exampleSrc + "\" gives:");
    console.log(parseResult);
    console.log(exp.printParser());
}
