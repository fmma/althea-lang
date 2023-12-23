export type Pair<A, B> = {fst: A; snd: B};
export type Either<A, B> = {inl: A} | {inr: B};
export type Maybe<A> = Either<A, null>

export interface IParser<A> {
    parse(src: string, cursor: number): [Maybe<A>, number];
    apply(src: string): A;
    printParser(prec?: string): string;

    map<B>(f: (x: A) => B): IParser<B>;
    and<B>(p: IParser<B>): IParser<A & B>; // 'and' is provided instead of Applicative (<*>), because it is more TypeScript'y and equally powerful. 
    or<B>(p: IParser<B>): IParser<A | B>;
    pair<B>(p: IParser<B>): IParser<Pair<A, B>>;
    either<B>(p: IParser<B>): IParser<Either<A, B>>;
    choice(p: IParser<A>): IParser<A>;
    many(): IParser<A[]>;
    many1(): IParser<A[]>;
    infixl<B>(sep: IParser<B>): IParser< Pair<A, Pair<B, A>[]> >;
    infixr<B>(sep: IParser<B>): IParser< Pair<Pair<A, B>[], A> >;
    sepby<B>(sep: IParser<B>): IParser<A[]>;
    sepby1<B>(sep: IParser<B>): IParser<A[]>;
    chainl(sep: IParser<(a: A, b: A) => A>, def: A): IParser<A>;
    chainl1(sep: IParser<(a: A, b: A) => A>): IParser<A>;
    chainr(sep: IParser<(a: A, b: A) => A>, def: A): IParser<A>;
    chainr1(sep: IParser<(a: A, b: A) => A>): IParser<A>;
    token(): IParser<A>;
}

interface IParsers {
    pure<A>(x: A): IParser<A>;
    zero<A>(): IParser<A>;
    regex(r: RegExp): IParser<string>;
    string(s: string): IParser<string>;
    symbol(sym: string): IParser<string>;
    fix<A>(f: (p: IParser<A>) => IParser<A>): IParser<A>;
    recurse<A>(f: () => IParser<A>): IParser<A>;
    parens<A>(open: string, p : IParser<A>, close: string): IParser<A>;
    
    readonly item: IParser<string>;
    readonly space: IParser<string>;
    readonly digit: IParser<string>;
    readonly nonZeroDigit: IParser<string>;
    readonly int: IParser<number>;
    readonly decimal: IParser<number>;
    readonly number: IParser<number>;
    readonly stringLitSingleQuote: IParser<string>;
    readonly stringLit: IParser<string>;
    readonly varName: IParser<string>
}

class Parsers implements IParsers {

    pure<A>(x: A): IParser<A> {
        return new PurePars(x);
    }

    private zeroParser = new ZeroPars<any>();

    zero<A>(): IParser<A> {
        return this.zeroParser;
    }

    private regExpParsers: {[i: string]: RegExpParser} = {};

    regex(r: RegExp): IParser<string> {
        const cached = this.regExpParsers[r.source];
        if(cached) {
            return cached;
        }
        const rp = new RegExpParser(r);
        this.regExpParsers[r.source] = rp;
        return rp;
    }

    private escapeRegExp(str: string): string {
        return str.replace(/[\-\[\]\/\{\}\(\)\*\+\?\.\\\^\$\|]/g, "\\$&");
    }

    string(s: string): IParser<string> {
        return this.regex(new RegExp(this.escapeRegExp(s)));
    }

    symbol(sym: string): IParser<string> {
        return this.string(sym).token();
    }
    
    fix<A>(f: (p: IParser<A>) => IParser<A>): IParser<A> {
        return new FixParser(f);
    }
    
    recurse<A>(f: () => IParser<A>): IParser<A> {
        return new FixParser(f);
    }
    
    parens<A>(open: string, p : IParser<A>, close: string): IParser<A> {
        return this.symbol(open)
        .pair(p)
        .pair(this.symbol(close))
        .map(p => p.fst.snd);
    }
    

    readonly item: IParser<string> = this.regex(/./);

    readonly space: IParser<string> = this.regex(/\s*/);

    readonly digit: IParser<string> = this.regex(/[0123456789]/);

    readonly nonZeroDigit: IParser<string> = this.regex(/[123456789]/);

    readonly int: IParser<number> = this.regex(/[1-9][0-9]*/).map(n => Number(n));
       // this.nonZeroDigit.pair(this.digit.many()).map(p => Number(p.fst + p.snd.join("")));

    readonly decimal: IParser<number> = this.regex(/[1-9][0-9]*\.[0-9]+/).map(n => Number(n));

    readonly number: IParser<number> = this.decimal.choice(this.int);
    /*
        this.nonZeroDigit                .map(x => ({a: x}))
        .and(this.digit.many()           .map(x => ({b: x.join("")})))
        .and(this.char(".")              .map(() => undefined))
        .and(this.digit.many1()          .map(x => ({c: x.join("")})))
        .map(p => Number(p.a + p.b + "." + p.c));
    */

    readonly stringLitSingleQuote: IParser<string> = this.regex(/'(\\.|[^'\\])*'/);

    readonly stringLit: IParser<string> = this.regex(/"(\\.|[^"\\])*"/);
    readonly varName: IParser<string> = this.regex(/[a-zA-Z_$][a-zA-Z_$0-9]*/);
}

abstract class Parser<A> implements IParser<A> {

    abstract parse(src: string, cursor: number): [Maybe<A>, number];

    abstract printParser(prec?: string): string;

    apply(src: string): A {
        const [x, i] = parsers.space.pair(this).parse(src, 0);
        if(src.length !== i) {
            throw new Error("No parse at position " + i);
        }
        if("inl" in x) {
            return x.inl.snd;
        }
        throw new Error("No parse");
    }

    map<B>(f: (x: A) => B): IParser<B> {
        return new MapPars(this, f);
    }

    and<B>(p: IParser<B>): IParser<A & B> {
        return new AndPars(this, p);
    }

    or<B>(p: IParser<B>): IParser<A | B> {
        return new OrPars(this, p);
    }

    pair<B>(p: IParser<B>): IParser<Pair<A, B>> {
        return this.map(x => ({fst: x})).and(p.map(x => ({snd: x})));
    }

    either<B>(p: IParser<B>): IParser<Either<A, B>> {
        return this.map(a => ({inl: a})).or(p.map(b => ({inr: b})));
    }

    choice(p: IParser<A>): IParser<A> {
        return this.or(p);
    }

    many(): IParser<A[]> {
        return new ManyPars(this); // this.many1().or(new PurePars([]));
    }

    many1(): IParser<A[]> {
        return this.pair(this.many()).map(p => [p.fst, ...p.snd]);
    }

    infixl<B>(sep: IParser<B>): IParser<{fst: A, snd: {fst: B, snd: A}[]}> {
        return this.pair(sep.pair(this).many()); // A ::= A (B A)*
    }

    infixr<B>(sep: IParser<B>): IParser<{fst: {fst: A, snd: B}[], snd: A}> {
        return this.pair(sep).many().pair(this); // A ::= (A B)* A 
    }

    sepby<B>(sep: IParser<B>): IParser<A[]> {
        return this.sepby1(sep).choice(new PurePars<A[]>([]));
    }

    sepby1<B>(sep: IParser<B>): IParser<A[]> {
        return this.infixl(sep).map(p => [p.fst, ... p.snd.map(x => x.snd)]);
    }

    chainl(sep: IParser<(a: A, b: A) => A>, def: A): IParser<A> {
        return this.chainl1(sep).choice(new PurePars(def));
    }

    chainl1(sep: IParser<(a: A, b: A) => A>): IParser<A> {
        return this.infixl(sep)
            .map(p => {
                let res = p.fst;
                for(let i = 0; i < p.snd.length; ++i) {
                    res = p.snd[i].fst(res, p.snd[i].snd);
                }
                return res;
            });
    }

    chainr(sep: IParser<(a: A, b: A) => A>, def: A): IParser<A> {
        return this.chainr1(sep).choice(new PurePars(def));
    }

    chainr1(sep: IParser<(a: A, b: A) => A>): IParser<A> {
        return this.infixr(sep)
            .map(p => {
                let res = p.snd;
                for(let i = p.fst.length-1; i >= 0; --i) {
                    res = p.fst[i].snd(p.fst[i].fst, res);
                }
                return res;
            });
    }

    token(): IParser<A> {
        return this.pair(parsers.space).map(p => p.fst);
    }
}

function just<A>(a: A): Maybe<A> {
    return {inl: a};
}

function nothing<A>(): Maybe<A> {
    return {inr: null};
}

class PurePars<A> extends Parser<A> {
    constructor(readonly x: A) {
        super();
    }

    parse(): [Maybe<A>, 0] {
        return [just(this.x), 0];
    }

    printParser() {
        return "pure(<x>)";
    }
}

class ZeroPars<A> extends Parser<A> {
    parse(): [Maybe<A>, 0] {
        return [nothing(), 0];
    }

    printParser() {
        return "zero";
    }
}

class MapPars<A, B> extends Parser<B> {
    constructor(readonly p: IParser<A>, readonly f: (x: A) => B) {
        super();
    }

    parse(src: string, c: number): [Maybe<B>, number] {
        const [x, i] = this.p.parse(src, c);
        if("inl" in x) {
            return [just(this.f(x.inl)), i];
        }
        return [nothing(), 0];
    }

    printParser(prec?: string) {
        return this.p.printParser(prec);
    }
}

class AndPars<A, B> extends Parser<A & B> {
    constructor(readonly pA: IParser<A>, readonly pB: IParser<B>) {
        super();
    }

    parse(src: string, c: number): [ Maybe<A & B>, number] {
        const [x, i] = this.pA.parse(src, c);
        if("inl" in x) {
            const [y, j] = this.pB.parse(src, c + i);
            if("inl" in y) {
                return [just(Object.assign({}, x.inl, y.inl)), i + j];   
            }
        }
        return [nothing(), 0];
    }

    printParser(prec?: string) {
        if(prec != null && ["fix", "*"].indexOf(prec) > -1)
            return "(" + this.pA.printParser("&") + " " + this.pB.printParser("&") + ")";
        return this.pA.printParser("&") + " " + this.pB.printParser("&");
    }
}

class OrPars<A, B> extends Parser<A | B> {
    constructor(readonly pA: IParser<A>, readonly pB: IParser<B>) {
        super();
    }

    parse(src: string, c: number): [ Maybe<A | B>, number ] {
        const [x, i] = this.pA.parse(src, c);
        if("inl" in x) {
            return [x, i];
        }
        return this.pB.parse(src, c);
    }

    printParser(prec?: string) {
        if(prec != null && ["&", "fix", "*"].indexOf(prec) > -1)
            return "(" + this.pA.printParser("|") + " | " + this.pB.printParser("|") + ")";
        return this.pA.printParser("|") + "\n | " + this.pB.printParser("|");
    }
}

class ManyPars<A> extends Parser<A[]> {
    constructor(readonly p: IParser<A>) {
        super();
    }

    parse(src: string, c: number): [ Maybe<A[]>, number ] {
        const [x, i] = this.p.parse(src, c);
        if("inl" in x) {
            const [y, j] = this.parse(src, c + i);
            if("inl" in y) {
                return [just([x.inl, ... y.inl]), i + j];
            }
            return [nothing(), 0];
        }
        return [just([]), 0];
    }

    printParser(prec?: string) {
        if(prec != null && ["fix"].indexOf(prec) > -1)
            return "(" + this.p.printParser("*") + "*)";
        return this.p.printParser("*") + "*";
    }
}

class SymbolicParser<A> extends Parser<A> {
    constructor(readonly name: string) {
        super();
    }

    parse(): [ Maybe<A>, number ] {
        throw new Error();
    }

    printParser() {
        return this.name;
    }
}

class FixParser<A> extends Parser<A> {
    body: Maybe<IParser<A>>;
    static n = 0;
    readonly i = FixParser.n++;

    constructor(readonly f: (p: IParser<A>) => IParser<A>) {
        super();
        this.body = nothing();
    }

    parse(src: string, c: number): [ Maybe<A>, number ] {
        if("inl" in this.body) {
            return this.body.inl.parse(src, c);
        }
        this.body = just(this.f(this));
        return this.parse(src, c);
    }
    
    printParser() {
        const x = String.fromCharCode("A".charCodeAt(0) + this.i);
        return "fix " + x + "." + this.f(new SymbolicParser(x)).printParser("fix");
    }
}

class RegExpParser extends Parser<string> {

    readonly regex: RegExp;

    constructor(r: RegExp) {
        super();
        this.regex = new RegExp("^" + r.source);
    }

    parse(src: string, c: number): [ Maybe<string>, number ] {
        const result = this.regex.exec(src.substring(c));
        if(result == null)
            return [nothing(), 0];
        return [just(result[0]), result[0].length];
    }

    printParser() {
        return this.regex.source.substring(1);
    }
}

const parsers: IParsers = new Parsers();
export default parsers;
