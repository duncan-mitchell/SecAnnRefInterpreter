import utils from './utils';
import Generator from './Generator';
import InjectHelper from './InjectHelper';
import TypeParser from './TypeParser';

export default function(acorn, doInjection, instance, opts) {
    let tt = acorn.tokTypes;

    function ParseTraitP() {
        let names = [];

        do {
            names.push(this.parseIdent());
        } while (this.eat(tt.comma));

        this.expect(tt.endTrait);

        return names;
    }

    function ParseTraitDependants() {
        let names = [];

        do {
            names.push(this.parseIdent());
        } while (this.eat(tt.comma));

        this.expect(tt.parenR);

        return names;
    }

    function ParseTraitParams() {
        let ptraits = [];
        let dvals = [];

        if (this.eat(tt.startTrait)) {
            ptraits = ParseTraitP.call(this);
        }

        if (this.eat(tt.parenL)) {
            dvals = ParseTraitDependants.call(this);
        }

        return [ptraits, dvals];
    }

    function ParseTraitDef() {
        let name, name2, nameDef, name2Def; 
        //let ptraits, dvals;
        /*if (this.eat(tt.startTrait)) {
            name = this.parseIdent();
            if (this.eat(tt.endTrait)) {
                [ptraits, dvals] = ParseTraitParams.call(this);
                this.semicolon();
            } else {
                //TODO: This should through an error if we can't get the end of the security annotation, i think.
                name = undefined;
            }
        }*/
        name = TypeParser.parseTypeAnnotation.call(this, tt);
        nameDef = !((name == undefined) || name == []);
        if (this.eat(tt.extends)) {
            name2 = TypeParser.parseTypeAnnotation.call(this, tt);
            name2Def = !((name2 == undefined) || name2 == []);
            this.semicolon();
            if (name2Def && nameDef && doInjection) {
                return Generator.genExtends.call(this, name, name2);
            }
        } else {
            this.semicolon();
            if(doInjection && nameDef) {
                return Generator.genTraitDef.call(this, name);
            }
        }
        return this.finishNode(this.startNode(), "EmptyStatement");
    }

    function ParseTraitRule() {
        let traitPath = this.parseExpression(false, {});
        let toOverride = this.parseIdent();
        let fnOveride = this.startNode();
        this.parseFunction(fnOveride, false);
        this.semicolon();


        if (doInjection) {
            return utils.wrapStatement.call(this, Generator.genTraitRule.call(this, traitPath, toOverride, fnOveride));
        } else {
            return this.finishNode(this.startNode(), "EmptyStatement");
        }
    }

    function ParseAssert() {
        let leftSide = this.parseExpression(false, {});

        let result;

        if (this.eat(tt.is)) {
            let rightSide = TypeParser.parseTypeAnnotation.call(this, tt, true);
            result = this.startNode();
            result.body = Generator.genExpectation.call(this, leftSide, rightSide).map(item => utils.wrapStatement.call(this, item));
            result = this.finishNode(result, 'BlockStatement');
        } else {
            result = utils.wrapStatement.call(this, Generator.genAssert.call(this, leftSide));
        }

        this.semicolon();

        if (doInjection) {
            return result;
        } else {
            return this.finishNode(this.startNode(), "EmptyStatement");
        }
    }

    function ParseStatement(inner) {
        return function(declaration, topLevel, exports) {
            if (this.eat(tt.traitdef)) {
                return ParseTraitDef.call(this);
            } else if (this.eat(tt.traitrule)) {
                return ParseTraitRule.call(this);
            } else if (this.eat(tt.assert)) {
                return ParseAssert.call(this);
            } else {
                return inner.call(this, declaration, topLevel, exports);
            }
        }
    }

    instance.extend('parseStatement', ParseStatement);
}