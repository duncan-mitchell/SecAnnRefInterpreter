export default {
	makeIdent(name) {
		let node = this.startNode();
		node.name = name;
		return this.finishNode(node, 'Identifier');
	},
	wrapStatement(expr) {
        let node = this.startNode();
        node.expression = expr;
        return this.finishNode(node, 'ExpressionStatement');
    },
	buildCall(callee, args) {
		let call = this.startNode();
        call.callee = callee;
        call.arguments = args;
        return this.finishNode(call, "CallExpression");
	},
	buildUnary(op, prefix, arg) {
		let unary = this.startNode();
        unary.operator = op;
        unary.prefix = prefix;
        unary.argument = arg;
        return this.finishNode(unary, "UnaryExpression");
	},
	buildUpdate(op, prefix, arg) {
		let unary = this.startNode();
        unary.operator = op;
        unary.prefix = prefix;
        unary.argument = arg;
        return this.finishNode(unary, "UpdateExpression");
	},
	buildBinary(op, left, right, logical) {
		let binop = this.startNode();
        binop.left = left;
        binop.right = right;
        binop.operator = op;
        return logical ? this.finishNode(binop, "LogicalExpression") : this.finishNode(binop, "BinaryExpression");
	},
	declVar(id, init) {
		let declarator = this.startNode();
        declarator.id = id;
        declarator.init = init;
        declarator = this.finishNode(declarator, "VariableDeclarator")

        let declaration = this.startNode();
        declaration.kind = "var";
        declaration.declarations = [declarator];
        return this.finishNode(declaration, "VariableDeclaration");
	},
	buildLiteral(literal) {
		let lit = this.startNode();
		lit.raw = literal;
		lit.value = literal;
		return this.finishNode(lit, "Literal");
	},
	buildFor(init, test, update, body) {
		let forLoop = this.startNode();
        forLoop.init = init;
        forLoop.test = test;
        forLoop.update = update;
        forLoop.body = body;
        return this.finishNode(forLoop, "ForStatement");
	},
	buildAnnErr() {
        let err = this.startNode();
        err.value = "FailedAnnotationCheck";
        err.raw = "FailedAnnotationCheck";
        err = this.finishNode(err, "Literal");

        let throwErr = this.startNode();
        throwErr.argument = err;
        throwErr.computed = true;
        return this.finishNode(throwErr, "ThrowStatement");
	},
	buildIf(test, consequent, alternate) {
		let ifNode = this.startNode();
        ifNode.test = test;
        ifNode.consequent = consequent;
        if (alternate) { ifNode.alternate = alternate; }
        return this.finishNode(ifNode, "IfStatement");
	},
	buildCatch(identName, body) {
		let catchIdent = this.startNode();
        catchIdent.name = identName;
        catchIdent = this.finishNode(catchIdent, "Identifier");

        let catchBody = this.startNode();
        catchBody.body = body;
        catchBody = this.finishNode(catchBody, "BlockStatement");

        let catchClause = this.startNode();
        catchClause.param = catchIdent;
        catchClause.body = catchBody;
        return this.finishNode(catchClause, "CatchClause")
	},
	buildTry(block, handler, finalizer) {
		let tryStatement = this.startNode();
        tryStatement.block = block;
        tryStatement.handler = handler;
        tryStatement.finalizer = finalizer;
        return this.finishNode(tryStatement, "TryStatement")
	}
}