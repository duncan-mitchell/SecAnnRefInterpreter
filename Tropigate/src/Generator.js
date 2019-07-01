import utils from './utils';
let escodegen = require('escodegen');

function Member(ident, name, memberIsExpr, propIsExpr, computed) {
    let S$ = this.startNode()
    S$.object = memberIsExpr ? ident : utils.makeIdent.call(this, ident);
    S$.property = propIsExpr? name : utils.makeIdent.call(this, name);
    S$.computed = computed;
    S$ = this.finishNode(S$, "MemberExpression");
    return S$;
}

function makeBlock(arr) {
    let block = this.startNode();
    block.body = arr.map(item => utils.wrapStatement.call(this, item));
    return this.finishNode(block, "BlockStatement");
}

function StoreAs(vname, expr) {
    let outer = this.startNode();
    outer.kind = 'var';

    let decl = this.startNode();
    decl.id = vname;
    decl.init = expr;
    decl = this.finishNode(decl, "VariableDeclarator");

    outer.declarations = [decl];
    return this.finishNode(outer, "VariableDeclaration");
}

function MakeExpressionStatement(expr) {
    let stmt = this.startNode();
    stmt.expression = expr;
    return this.finishNode(stmt, "ExpressionStatement");
}

function Require(vname, name) {
    let call = this.startNode();
    call.callee = utils.makeIdent.call(this, 'require');
    call.arguments = [utils.makeIdent.call(this, name)];
    call = this.finishNode(call, "CallExpression");

    return StoreAs.call(this, utils.makeIdent.call(this, vname), call);
}

function WrapT(args) {
    let S$ = this.startNode()
    S$.object = utils.makeIdent.call(this, 'S$');
    S$.property = utils.makeIdent.call(this, 't');
    S$.computed = false;
    S$ = this.finishNode(S$, "MemberExpression");

    let call = this.startNode();
    call.callee = S$;
    call.arguments = args;
    call = this.finishNode(call, "CallExpression");

    return call;
}

function GenArguments(trait) {

    //If the source code marked an expression as a trait (For annotations referencing their own bound traits)
    //TODO: Needing to do an if ptrait check twice is ugly
    if (trait.ptrait) {
        return [trait.ptrait];
    }

    let args = [];
    args.push(trait);

    trait.parameters.forEach(item => {
        if (item.ptrait) {
            args.push(item.ptrait);
        } else {
            args.push(GenArguments.call(this, item));
        }
    })

    trait.dependants.forEach(item => {
        args.push(item);
    });

    return [WrapT.call(this, args)];
}

function ToLiteral(ident) {
    let node = this.startNode();
    node.value = ident.name;
    node.raw = ident.name;
    return this.finishNode(node, "Literal");
}

function AnnToLiteral(ann) {
    let len = ann.annotations.length;
    /* Generates a single argument for each annotation
    let annotations = [];
    for (let i = 0; i < len; i++) {
        annotations.push(ToLiteral.call(this, ann.annotations[i]));
    }*/
    /* Generates the * delimited list of annotations for parsing in the 
       reference implementation. */
    let annotations = ann.annotations[0];
    for (let i = 1; i < len; i++) {
        annotations.name += ' * ' + ann.annotations[i].name;
    }
    return ToLiteral.call(this, annotations);
    }

function WrapIdentList(list) {
    let node = this.startNode();
    node.elements = list.map(x => ToLiteral.call(this, x));
    return this.finishNode(node, 'ArrayExpression');
}

export default {
    genTraitDef(name) { //, ptraits, dvals) {
        /*let methodToCall = Member.call(this, 'Trait', 'create');*/
        let methodToCall = Member.call(this, 'SecurityAnnotation', 'declare');
        let newTrait = this.startNode();
        newTrait.callee = methodToCall;
        /*newTrait.arguments = [ToLiteral.call(this, name), WrapIdentList.call(this, ptraits), WrapIdentList.call(this, dvals)];*/
        newTrait.arguments = [AnnToLiteral.call(this, name)];
        newTrait.computed = false;
        newTrait = this.finishNode(newTrait, "CallExpression");

        //return StoreAs.call(this, name, newTrait);
        return MakeExpressionStatement.call(this, newTrait);
    },
    genExtends(ann1, ann2) {
        let methodToCall = Member.call(this, 'SecurityAnnotation', 'extends');
        let newHier = this.startNode();
        newHier.callee = methodToCall;
        newHier.arguments = [AnnToLiteral.call(this, ann1), AnnToLiteral.call(this, ann2)];
        newHier.computed = false;
        newHier = this.finishNode(newHier, "CallExpression");
        return MakeExpressionStatement.call(this, newHier);
    },
    genTraitRule(trait, id, fn) {
        let methodToCall = Member.call(this, 'Trait', 'extend');

        let node = this.startNode();
        node.callee = methodToCall;
        node.arguments = [trait, ToLiteral.call(this, id), fn];
        node.computed = false;
        return this.finishNode(node, "CallExpression");
    },
    genExpectation(val, expected) {

        if (!expected) {
            return [];
        }

        let exp = [];

        /*let S$ = this.startNode()
        S$.object = utils.makeIdent.call(this, 'S$');
        S$.property = utils.makeIdent.call(this, 'assert');
        S$.computed = false;
        S$ = this.finishNode(S$, "MemberExpression");
        
        let call = this.startNode();
        call.callee = S$;
        call.callee = assert;
        call.arguments = [val];
        call = this.finishNode(call, "CallExpression");

        let isMember = this.startNode();
        isMember.object = call;
        isMember.property = utils.makeIdent.call(this, 'ist');
        isMember.computed = false;
        isMember = this.finishNode(isMember, 'MemberExpression');

        console.log(expected);*/
        let assert = Member.call(this, "SecurityAnnotation", "assert");

        if (expected.base) {
            exp.push(utils.buildCall.call(this, 
                assert, [val, expected.base]));
        }

        expected.annotations.forEach(arg => {
            exp.push(utils.buildCall.call(this, 
                assert, [val, ToLiteral.call(this, arg)]));
        });

        return exp;
    },
    genThisExpectation(expected) {

        if (!expected) {
            return [];
        }

        let exp = [];

        /*let S$ = this.startNode()
        S$.object = utils.makeIdent.call(this, 'S$');
        S$.property = utils.makeIdent.call(this, 'assert');
        S$.computed = false;
        S$ = this.finishNode(S$, "MemberExpression");
        
        let call = this.startNode();
        call.callee = S$;
        call.callee = assert;
        call.arguments = [val];
        call = this.finishNode(call, "CallExpression");

        let isMember = this.startNode();
        isMember.object = call;
        isMember.property = utils.makeIdent.call(this, 'ist');
        isMember.computed = false;
        isMember = this.finishNode(isMember, 'MemberExpression');

        console.log(expected);*/
        let assert = Member.call(this, "SecurityAnnotation", "assert");
        let thisIdent = utils.makeIdent.call(this, "this");

        if (expected.base) {
            exp.push(utils.buildCall.call(this, 
                assert, [thisIdent, expected.base]));
        }

        expected.annotations.forEach(arg => {
            exp.push(utils.buildCall.call(this, 
                assert, [thisIdent, ToLiteral.call(this, arg)]));
        });
        return exp;
    },
    genExpectationAll(obj, expected) {
        if (!expected) {
            return [];
        }
        let exp = [];

        //Check the type
        let type = utils.buildUnary.call(this, "typeof", true, obj);
        let objLit = utils.buildLiteral.call(this, "object");
        let objCheck = utils.buildBinary.call(this, "==", type, objLit);

        //GetOwnPropertyNames & assign it
        let identArr = utils.makeIdent.call(this, "secAnnProps");
        let getOwn = Member.call(this, "Object", "getOwnPropertyNames");
        let propCheck = utils.buildCall.call(this, getOwn, [obj]);
        let declPropsArr = utils.declVar.call(this, identArr, propCheck);

        //Configure For Loop
        //Declare & initialize iterator
        let identIter = utils.makeIdent.call(this, "secAnnIter");
        let zero = utils.buildLiteral.call(this, 0);
        let iterDeclaration = utils.declVar.call(this, identIter, zero);

        //Declare Max
        let propsLength = Member.call(this, identArr, utils.makeIdent.call(this,'length'), true, true, false)
        let lengthCheck = utils.buildBinary.call(this, "<", identIter, propsLength);
        
        //IteratorIncrememnt
        let updateIter = utils.buildUpdate.call(this, "++", false, identIter);

        //Compute Inside of For Loop
        //Build the expression for obj[SecAnnProps[SecAnnIter]]
        let iterOfArr = Member.call(this, identArr, identIter, true, true, true);
        let objProp = Member.call(this, obj, iterOfArr, true, true, true);

        let assert = Member.call(this, "SecurityAnnotation", "assert");

        if (expected.base) {
            exp.push(utils.buildCall.call(this, 
                assert, [objProp, expected.base]));
        }

        expected.annotations.forEach(arg => {
            exp.push(utils.buildCall.call(this, 
                assert, [objProp, ToLiteral.call(this, arg)]));
        });

        let asserts = makeBlock.call(this, exp);

        //Construct For Loop
        let forLoop = utils.buildFor.call(this, iterDeclaration, lengthCheck, updateIter, asserts);

        //Construct Err
        let throwErr = utils.buildAnnErr.call(this);

        //Construct Consequent
        let consequent = makeBlock.call(this, [declPropsArr, forLoop]);
       
        //If Statement
        return utils.buildIf.call(this, objCheck, consequent, throwErr);
    },
    genExpectationNumProps(obj, num, expected) {
        if (!expected) {
            return [];
        }
        let exp = [];

        //Check the type
        let type = utils.buildUnary.call(this, "typeof", true, obj);
        let objLit = utils.buildLiteral.call(this, "object");
        let objCheck = utils.buildBinary.call(this, "==", type, objLit);

        //GetOwnPropertyNames & assign it
        let identArr = utils.makeIdent.call(this,"secAnnProps");
        let getOwn = Member.call(this, "Object", "getOwnPropertyNames");
        let propCheck = utils.buildCall.call(this, getOwn, [obj]);
        let declPropsArr = utils.declVar.call(this, identArr, propCheck);

        //initialize a counter
        //Declare & initialize success iterator
        let identSuccIter = utils.makeIdent.call(this, "secAnnNumSuccesses");
        let zero = utils.buildLiteral.call(this, 0);
        let iterSuccDeclaration = 
            utils.declVar.call(this, identSuccIter, zero);

        //Configure For Loop
        //Declare & initialize iterator
        let identIter = utils.makeIdent.call(this, "secAnnIter");
        let iterDeclaration = utils.declVar.call(this, identIter, zero);

        //Declare Max
        let propsLength = Member.call(this, identArr, 
            utils.makeIdent.call(this,'length'), true, true, false)
        let lengthCheck = utils.buildBinary.call(this, "<", identIter,
            propsLength);
        
        //IteratorIncrememnt
        let updateIter = utils.buildUpdate.call(this, "++", false, identIter);

        //Compute Inside of For Loop
        //Build the expression for obj[SecAnnProps[SecAnnIter]]
        let iterOfArr = Member.call(this, identArr, identIter, true, true, true);
        let objProp = Member.call(this, obj, iterOfArr, true, true, true);

        let assert = Member.call(this, "SecurityAnnotation", "assert");

        if (expected.base) {
            exp.push(utils.buildCall.call(this, 
                assert, [objProp, expected.base]));
        }

        expected.annotations.forEach(arg => {
            exp.push(utils.buildCall.call(this, 
                assert, [objProp, ToLiteral.call(this, arg)]));
        });
        //count successes
        exp.push(utils.buildUpdate.call(this, "++", false, identSuccIter));

        let asserts = makeBlock.call(this, exp);

        //Construct Try/Catch Block
        let catchClause = utils.buildCatch.call(this, "e", []);
        let tryStatement = 
            utils.buildTry.call(this, asserts, catchClause, null);

        //Construct Err
        let throwErr = utils.buildAnnErr.call(this);

        //Construct the final comparison
        let succComparison = utils.buildBinary.call(this, "<", identSuccIter,
            num);
        let ifLtNum = utils.buildIf.call(this, succComparison, throwErr);

        //Construct For Loop
        let forLoop = utils.buildFor.call(this, iterDeclaration, lengthCheck, updateIter, tryStatement);

        //Construct Consequent
        let consequent = makeBlock.call(this, [declPropsArr, iterSuccDeclaration, forLoop, ifLtNum]);
       
        //If Statement
        return utils.buildIf.call(this, objCheck, consequent, throwErr);
    },
    genExpectationSpecifiedProp(obj, prop, expected) {
        if (!expected) {
            return [];
        }
        let exp = [];

        //Check the type
        let type = utils.buildUnary.call(this, "typeof", true, obj);
        let objLit = utils.buildLiteral.call(this, "object");
        let objCheck = utils.buildBinary.call(this, "==", type, objLit);

        //GetOwnPropertyNames & assign it
        let getOwn = Member.call(this, "Object", "getOwnPropertyNames");
        let propCheck = utils.buildCall.call(this, getOwn, [obj]);

        //Get the index of prop in GetOwnPropertyNames
        let indexCheck = Member.call(this, propCheck, "indexOf", true);
        let callIndexCheck = utils.buildCall.call(this, indexCheck, [prop]);

        //Set up the right-hand side of the comparison, 0
        let zero = utils.buildLiteral.call(this, 0);

        //Check prop in getOwnPropNames
        let confirmIndexCheck = utils.buildBinary.call(this, ">=", callIndexCheck, zero);

        let ifTest = utils.buildBinary.call(this, "&&", objCheck, confirmIndexCheck, true);

        //Build the expression for obj[prop]
        let objProp = Member.call(this, obj, prop, true, true, true);

        //The assert bit for consequent
        let assert = Member.call(this, "SecurityAnnotation", "assert");

        if (expected.base) {
            exp.push(utils.buildCall.call(this, 
                assert, [objProp, expected.base]));
        }

        expected.annotations.forEach(arg => {
            exp.push(utils.buildCall.call(this, 
                assert, [objProp, ToLiteral.call(this, arg)]));
        });

        let asserts = makeBlock.call(this, exp);

        //Build a Throws expression for the alternate
        //First build the throws error
        let throwErr = utils.buildAnnErr.call(this);

        return utils.buildIf.call(this, ifTest, asserts, throwErr);
    },
    genSymbol(name, expr) {
        let node = this.startNode();
        node.callee = utils.makeIdent.call(this, 'S$');
        node.arguments = [ToLiteral.call(this, name), expr];
        return this.finishNode(node, 'CallExpression');
    },
    genAssume(expr, trait) {
        let S$ = this.startNode()
        S$.object = utils.makeIdent.call(this, 'SecurityAnnotation');
        S$.property = utils.makeIdent.call(this, 'as');
        S$.computed = false;
        S$ = this.finishNode(S$, "MemberExpression");

        let call = this.startNode();
        call.callee = S$;
        call.arguments = [expr, ToLiteral.call(this, trait)];
        return this.finishNode(call, "CallExpression");
        
        /*call.arguments = [expr];
        call = this.finishNode(call, "CallExpression");
        let is = this.startNode();
        is.object = call;
        is.property = utils.makeIdent.call(this, 'ist');
        is.computed = false;
        is = this.finishNode(is, "MemberExpression");
        
        let result = this.startNode();
        result.callee = is;
        result.arguments = GenArguments.call(this, trait);
        return this.finishNode(result, 'CallExpression');*/
    },
    genDrop(expr, trait) {
        let S$ = this.startNode()
        S$.object = utils.makeIdent.call(this, 'SecurityAnnotation');
        S$.property = utils.makeIdent.call(this, 'drop');
        S$.computed = false;
        S$ = this.finishNode(S$, "MemberExpression");

        let call = this.startNode();
        call.callee = S$;
        call.arguments = [expr, ToLiteral.call(this, trait)];
        return this.finishNode(call, "CallExpression");

        /*let S$ = this.startNode()
        S$.object = utils.makeIdent.call(this, 'S$');
        S$.property = utils.makeIdent.call(this, 'assume');
        S$.computed = false;
        S$ = this.finishNode(S$, "MemberExpression");

        let call = this.startNode();
        call.callee = S$;
        call.arguments = [expr];
        call = this.finishNode(call, "CallExpression");

        let is = this.startNode();
        is.object = call;
        is.property = utils.makeIdent.call(this, 'dropt');
        is.computed = false;
        is = this.finishNode(is, "MemberExpression");

        let result = this.startNode();
        result.callee = is;
        result.arguments = GenArguments.call(this, trait);
        return this.finishNode(result, 'CallExpression');*/
    },
    genAssert(expr, assertName) {

        assertName = assertName || "assertion ";
        let exprToString = escodegen.generate(expr);
        assertName += ' ' + exprToString;

        assertName = utils.makeIdent.call(this, assertName);

        let S$ = this.startNode()
        //S$.object = utils.makeIdent.call(this, 'S$');
        S$.object = utils.makeIdent.call(this, "SecurityAnnotation");
        S$.property = utils.makeIdent.call(this, 'assert');
        S$.computed = false;
        S$ = this.finishNode(S$, "MemberExpression");

        //TODO: Use the Expr itself to generate the assertion name
        let call = this.startNode();
        call.callee = S$;
        call.arguments = [expr, ToLiteral.call(this, assertName)];
        call = this.finishNode(call, "CallExpression");

        return call;
    },
    genCpAnn(args) {
        let copy = this.startNode();
        copy.object = utils.makeIdent.call(this, 'SecurityAnnotation');
        copy.property = utils.makeIdent.call(this, 'cpAnn');
        copy.computed = false;
        return this.finishNode(copy, "MemberExpression");

    }
}