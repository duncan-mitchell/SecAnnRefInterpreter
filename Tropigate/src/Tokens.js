export default function(acorn, inject, instance, opts) {
    let tt = acorn.tokTypes;
    let tc = acorn.tokContexts;

    tt.typeAnnotation = tt.colon;
    tt.startTrait = new acorn.TokenType("<!", {});
    tt.endTrait = new acorn.TokenType("!>", {});

    // Succinct definitions of keyword token types
    function kw(name, options = {}) {
        options.keyword = name
        return new acorn.TokenType(name, options)
    }

    function binop(name, kw, prec) {
        let options = {
            beforeExpr: false,
            binop: prec,
            keyword: kw
        };
        return new acorn.TokenType(name, options);
    }

    tt.assert = kw('assert');
    tt.where = kw('where');
    tt.traitdef = kw('SecAnn'); //kw('traitdef');
    tt.traitrule = kw('traitrule');
    tt.extends = kw('Extends');
      
    tt.symbolic = new acorn.TokenType("symbolic", {
        startsExpr: true
    });

    tt.ptrait = kw('ptrait');
    tt.is = kw('is');

    tt.drop = new acorn.TokenType("drop", {
        postfix: true,
        startsExpr: true
    });
    
    tt.as = new acorn.TokenType("as", {
        postfix: true,
        startsExpr: true
    });

    tt.ObjAnnSpecfiedProp = new acorn.TokenType(":S", {});
    tt.ObjAnnAllProps = new acorn.TokenType(":A", {});
    tt.ObjAnnNumProps = new acorn.TokenType(":E", {});
    tt.thisAnn = new acorn.TokenType(":this", {})

    function ReadToken(inner) {
        return function(code) {

            if (this.input.slice(this.pos, this.pos + 7) === 'assert ') {
                this.pos += 7;
                return this.finishToken(tt.assert);
            }

            if (this.input.slice(this.pos, this.pos + 5) === 'drop ') {
                this.pos += 5;
                return this.finishToken(tt.drop);
            }

            if (this.input.slice(this.pos, this.pos + 6) === 'where ') {
                this.pos += 6;
                return this.finishToken(tt.where);
            }

            if (this.input.slice(this.pos, this.pos + 9) === 'symbolic ') {
                this.pos += 9;
                return this.finishToken(tt.symbolic);
            }

            /*if (this.input.slice(this.pos, this.pos + 9) === 'traitdef ') {
                this.pos += 9;
                return this.finishToken(tt.traitdef);
            }*/

            if (this.input.slice(this.pos, this.pos + 7) === 'SecAnn ') {
                this.pos += 7;
                return this.finishToken(tt.traitdef);
            }

            if (this.input.slice(this.pos, this.pos + 8) === 'Extends ') {
                this.pos += 8;
                return this.finishToken(tt.extends);
            }

            if (this.input.slice(this.pos, this.pos + 10) === 'traitrule ') {
                this.pos += 10;
                return this.finishToken(tt.traitrule);
            }

            if (this.input.slice(this.pos, this.pos + 7) === 'ptrait ') {
                this.pos += 7;
                return this.finishToken(tt.ptrait);
            }

            if (this.input.slice(this.pos, this.pos + 3) == 'is ') {
                this.pos += 2;
                return this.finishToken(tt.is);
            }

            if (this.input.slice(this.pos, this.pos + 3) == 'as ') {
                this.pos += 2;
                return this.finishToken(tt.as);
            }

            if (this.input.slice(this.pos, this.pos + 2) == '<!') {
                this.pos += 2;
                return this.finishToken(tt.startTrait);
            }

            if (this.input.slice(this.pos, this.pos + 2) == '!>') {
                this.pos += 2;
                return this.finishToken(tt.endTrait);
            }

            if (this.input.slice(this.pos, this.pos + 2) == ':S') {
                this.pos += 2;
                return this.finishToken(tt.ObjAnnSpecfiedProp);
            }            

            if (this.input.slice(this.pos, this.pos + 2) == ':A') {
                this.pos += 2;
                return this.finishToken(tt.ObjAnnAllProps);
            }            

            if (this.input.slice(this.pos, this.pos + 2) == ':E') {
                this.pos += 2;
                return this.finishToken(tt.ObjAnnNumProps);
            }            
            if (this.input.slice(this.pos, this.pos + 5) == ':this') {
                this.pos += 5;
                return this.finishToken(tt.thisAnn);
            }            

            return inner.call(this, code);
        }
    }

    instance.extend('readToken', ReadToken);

    return tt;
}