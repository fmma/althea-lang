type SimpleParser<A> = (src: string, cursor: number) => [A | null, number];

function item(): SimpleParser<string> {
    return (src, c) => src.length > c ? [src[c], 1] : [null, 0];
}

function pure<A>(x: A): SimpleParser<A> {
    return () => [x, 0];
}

function map<A, B>(f: (x: A) => B, px: SimpleParser<A>): SimpleParser<B> {
    return (src, c) => {
        const [x, i] = px(src, c);
        if(x != null)
            return [f(x), i];
        return [null, 0];
    }
}

function apply<A, B>(pf: SimpleParser<(x: A) => B>, px: SimpleParser<A>): SimpleParser<B> {
    return (src, c) => {
        const [f, i] = pf(src, c);
        if(f != null) {
            const [x, j] = px(src, c + i);
            if(x != null) {
                return [f(x), i + j];   
            }
        }
        return [null, 0];
    }
}

function zero<A>(): SimpleParser<A> {
    return () => [null, 0];
}

function choice<A>(p0: SimpleParser<A>, p1: SimpleParser<A>): SimpleParser<A> {
    return (src, c) => {
        const [x, i] = p0(src, c);
        if(x != null) {
            return [x, i];
        }
        return p1(src, c);
    }
}

function sat(pred: (char: string) => boolean): SimpleParser<string> {
    return (src, c) => {
        if(src.length > c && pred(src[c])) {
            return [src[0], 1];
        }
        return [null, 0];
    }
}

function char(c: string): SimpleParser<string> {
    return sat(c0 => c0 === c);
}

function string(s: string): SimpleParser<string> {
    if(s.length == 0)
        return pure("");
    return apply(map(x => (y: string) => x.concat(y), char(s[0])), string(s.substr(1)));
}