import utils from './utils';
import Generator from './Generator';
import InjectHelper from './InjectHelper';
import TypeParser from './TypeParser';

export default function(acorn, doInjection, instance, opts) {
	let tt = acorn.tokTypes;

    //TODO: Push upstream change to acorn so I dont have to replace alllll of this crazyness
    function ParseExprAtom(inner) {
        return function(refDestructuringErrors) {
            if (this.eat(tt.symbolic)) {
                let name = this.parseIdent();
                let exprFollows = this.parseMaybeConditional(true, refDestructuringErrors);

                if (doInjection) {
                    return Generator.genSymbol.call(this, name, exprFollows);
                } else {
                    return exprFollows;
                }
            } else {
                return inner.call(this, refDestructuringErrors);
            }
        }
    }

    instance.extend('parseExprAtom', ParseExprAtom);
}