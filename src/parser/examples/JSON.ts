import P, { IParser } from '../Parser';

/*
JSON parser
*/

const value: IParser<any> = P.fix( (value: IParser<any>)  => {
    const member: IParser<any> = P.stringLit.token()
        .pair(P.symbol(":"))
        .pair(value)
        .map(p => ({ key: p.fst.fst, val: p.snd }));

    const object: IParser<any> = P.symbol("{")
        .pair(member.sepby(P.symbol(",")))
        .pair(P.symbol("}"))
        .map(p => {
            const pairs = p.fst.snd;
            const res: {[ix: string]: any} = {};
            pairs.forEach(p => {
                res[p.key] = p.val;
            });
            return res;
        })
    
    const array: IParser<any> = P.symbol("[")
        .pair(value.sepby(P.symbol(",")))
        .pair(P.symbol("]"))
        .map(p => {
            const res: any[] = [];
            p.fst.snd.forEach(e => {
                res.push(e);
            });
            return res;
        });

    const res: IParser<any> = 
    P.stringLit.token()
    .choice(object)
    .choice(array)
    .choice(P.number.token() as IParser<any>)
    .choice(P.symbol("true").map(() => true)  as IParser<any>)
    .choice(P.symbol("false").map(() => false)  as IParser<any>)
    .choice(P.symbol("null").map(() => null)  as IParser<any>);

    return res
});

console.log(value.apply("  { \"key\": 100, \"array\": [true, false] , \"nested\"  : {} }  "));

console.log(value.printParser());