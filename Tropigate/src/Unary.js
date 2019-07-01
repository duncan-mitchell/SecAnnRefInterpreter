import utils from './utils';
import Generator from './Generator';
import InjectHelper from './InjectHelper';
import TypeParser from './TypeParser';

export default function(acorn, doInjection, instance, opts) {
	let tt = acorn.tokTypes;

    function BuildWrapper(cb, expr, tAnnotation) {
        tAnnotation.annotations.forEach(ann => {
            expr = cb.call(this, expr, ann);
        });
        return expr;
    }

    //TODO: Push upstream change to acorn so I dont have to replace alllll of this crazyness
    function ParseMaybeUnary(inner) {
        return function(refDestructuringErrors, sawUnary) {
            let startPos = this.start,
                startLoc = this.startLoc,
                expr
            if (this.inAsync && this.isContextual("await")) {
                expr = this.parseAwait(refDestructuringErrors)
                sawUnary = true
            } else if (this.type.prefix) {
                let node = this.startNode(),
                    update = this.type === tt.incDec
                node.operator = this.value
                node.prefix = true
                this.next()
                node.argument = this.parseMaybeUnary(refDestructuringErrors, true)
                this.checkExpressionErrors(refDestructuringErrors, true)
                if (update) this.checkLVal(node.argument)
                else if (this.strict && node.operator === "delete" &&
                    node.argument.type === "Identifier")
                    this.raiseRecoverable(node.start, "Deleting local variable in strict mode")
                else sawUnary = true
                expr = this.finishNode(node, update ? "UpdateExpression" : "UnaryExpression")
            } else {
                expr = this.parseExprSubscripts(refDestructuringErrors)
                if (this.checkExpressionErrors(refDestructuringErrors)) return expr
                while (this.type.postfix && !this.canInsertSemicolon()) {
                    if (this.type == tt.as) { 
                        this.next();
                        
                        let annot = TypeParser.parseTypeAnnotation.call(this, tt, false);
                        
                        //Replace EXPR with assume if Injection
                        if (doInjection) {
                            expr = BuildWrapper.call(this, Generator.genAssume, expr, annot);
                        }

                    } else if (this.type == tt.drop) { 
                        this.next();
                        
                        let annot = TypeParser.parseTypeAnnotation.call(this, tt, false);
                        
                        //Replace EXPR with assume if Injection
                        if (doInjection) {
                            expr = BuildWrapper.call(this, Generator.genDrop, expr, annot);
                        }

                    } else {
                        let node = this.startNodeAt(startPos, startLoc)
                        node.operator = this.value
                        node.prefix = false
                        node.argument = expr
                        this.checkLVal(expr)
                        this.next()
                        expr = this.finishNode(node, "UpdateExpression")
                    }
                }
            }

            if (!sawUnary && this.eat(tt.starstar))
                return this.buildBinary(startPos, startLoc, expr, this.parseMaybeUnary(refDestructuringErrors, false), "**", false)
            else
                return expr
        };
    }

    instance.extend('parseMaybeUnary', ParseMaybeUnary);
}