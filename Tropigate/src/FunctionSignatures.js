import utils from './utils';
import Generator from './Generator';
import InjectHelper from './InjectHelper';
import TypeParser from './TypeParser';

export default function(acorn, doInjection, instance, opts) {
	let tt = acorn.tokTypes;

    function ParseFunctionBody(inner) {
        return function(node, isArrowFunction) {
            inner.call(this, node, isArrowFunction);
            if (doInjection) {
                InjectHelper.injectExpectations.call(this, node);
            }
        };
    }

    function ParseFunctionParams(inner) {
        return function(node) {
            inner.call(this, node);
            if (node.params != undefined) {
                node.params = node.params.filter(param => {
                    if (param.thisEnf) {
                        node.thisEnf = true;
                        node.thisExpectedType = param.expectedType;
                        node.thisExpectedTypeSpecifiedPropName = param.expectedTypeSpecifiedPropName;
                        node.thisExpectedTypeSpecifiedPropType = param.expectedTypeSpecifiedPropType;
                        node.thisExpectedTypeNumProps = param.expectedTypeNumProps;
                        node.thisExpectedTypeNumPropsAnn = param.expectedTypeNumPropsAnn;
                        node.thisExpectedTypeAllProps = param.expectedTypeAllProps;
                        return false;
                    }
                    return true;
                });
            }
            if (this.eat(tt.thisAnn) && node.thisType != undefined) {
                node.thisType = TypeParser.parseTypeAnnotation.call(this, tt, true);
            }
            if (this.eat(tt.typeAnnotation)) {
                node.returnType = TypeParser.parseTypeAnnotation.call(this, tt, true);
            }

            if (this.eat(tt.where)) {
                node.whereClause = this.parseExpression(true, {});
            }
        }
    }

    function ParseBindingAtom(inner) {
        /*
         * Of course, the overall object annotation, and the check on all 
         * properties only appear once. The others can appear repeatedly for 
         * new properties or numbers of properties. 
         *
         */
        return function(refDestructuringErrors) {
            if (this.type == tt.name) {
                let ident = this.parseIdent();

                while(this.type == tt.typeAnnotation ||
                        this.type == tt.ObjAnnSpecfiedProp ||
                        this.type == tt.ObjAnnNumProps ||
                        this.type == tt.ObjAnnAllProps) {
                    if (this.eat(tt.typeAnnotation) && !ident.expectedType) {
                        ident.expectedType = TypeParser.parseTypeAnnotation.call(this, tt, true);
                    }
                    if (this.eat(tt.ObjAnnSpecfiedProp)) {
                        this.expect(tt.bracketL);
                        if(!ident.expectedTypeSpecifiedPropName){
                            ident.expectedTypeSpecifiedPropName = [];
                            ident.expectedTypeSpecifiedPropType = []
                        }
                        ident.expectedTypeSpecifiedPropName.push(this.parseExprAtom());
                        this.expect(tt.comma);
                        ident.expectedTypeSpecifiedPropType.push(TypeParser.parseTypeAnnotation.call(this, tt, true));
                        this.expect(tt.bracketR); 
                    }

                    if (this.eat(tt.ObjAnnNumProps)) {
                        this.expect(tt.bracketL);
                        if(!ident.expectedTypeNumProps) {
                            ident.expectedTypeNumProps = [];
                            ident.expectedTypeNumPropsAnn = [];
                        }
                        ident.expectedTypeNumProps.push(this.parseExprAtom());
                        this.expect(tt.comma);                 
                        ident.expectedTypeNumPropsAnn.push(TypeParser.parseTypeAnnotation.call(this, tt, true));
                        this.expect(tt.bracketR);
                    }

                    if (this.eat(tt.ObjAnnAllProps) 
                        && !ident.expectedTypeAllProps) {
                        ident.expectedTypeAllProps = TypeParser.parseTypeAnnotation.call(this, tt, true);
                    }
                }

                return ident;
            } else if (this.type == tt.typeAnnotation ||
                        this.type == tt.ObjAnnSpecfiedProp ||
                        this.type == tt.ObjAnnNumProps ||
                        this.type == tt.ObjAnnAllProps) {
                let thisNode = this.startNode();
                thisNode.name = 'THIS_IDENTIFIER';
                thisNode.thisEnf = true;
                // if (this.eat(tt.thisAnn)) {
                //     thisNode.thisType = TypeParser.parseTypeAnnotation.call(this, tt, true);
                // }
                while(this.type == tt.typeAnnotation ||
                        this.type == tt.ObjAnnSpecfiedProp ||
                        this.type == tt.ObjAnnNumProps ||
                        this.type == tt.ObjAnnAllProps) {
                    if (this.eat(tt.typeAnnotation) && !thisNode.expectedType) {
                        thisNode.expectedType = TypeParser.parseTypeAnnotation.call(this, tt, true);
                    }
                    if (this.eat(tt.ObjAnnSpecfiedProp)) {
                        this.expect(tt.bracketL);
                        if(!thisNode.expectedTypeSpecifiedPropName){
                            thisNode.expectedTypeSpecifiedPropName = [];
                            thisNode.expectedTypeSpecifiedPropType = []
                        }
                        thisNode.expectedTypeSpecifiedPropName.push(this.parseExprAtom());
                        this.expect(tt.comma);
                        thisNode.expectedTypeSpecifiedPropType.push(TypeParser.parseTypeAnnotation.call(this, tt, true));
                        this.expect(tt.bracketR); 
                    }

                    if (this.eat(tt.ObjAnnNumProps)) {
                        this.expect(tt.bracketL);
                        if(!thisNode.expectedTypeNumProps) {
                            thisNode.expectedTypeNumProps = [];
                            thisNode.expectedTypeNumPropsAnn = [];
                        }
                        thisNode.expectedTypeNumProps.push(this.parseExprAtom());
                        this.expect(tt.comma);                 
                        thisNode.expectedTypeNumPropsAnn.push(TypeParser.parseTypeAnnotation.call(this, tt, true));
                        this.expect(tt.bracketR);
                    }

                    if (this.eat(tt.ObjAnnAllProps) 
                        && !thisNode.expectedTypeAllProps) {
                        thisNode.expectedTypeAllProps = TypeParser.parseTypeAnnotation.call(this, tt, true);
                    }
                }

                return this.finishNode(thisNode, 'Identifier');
            } else {
                return inner.call(this, refDestructuringErrors);
            }
        }
    }

    instance.extend('parseFunctionParams', ParseFunctionParams);
    instance.extend('parseFunctionBody', ParseFunctionBody);
    instance.extend('parseBindingAtom', ParseBindingAtom);
}