import utils from './utils';
import Generator from './Generator';

function InjectPreConditions(node) {
    if (node.thisEnf) {
        let thisIdent = utils.makeIdent.call(this, "this");
        // Generator.genThisExpectation.call(this, node.thisType).map(item => utils.wrapStatement.call(this, item)).forEach(item => node.body.body.unshift(item));

        Generator.genExpectation.call(this, thisIdent, node.thisExpectedType).map(item => utils.wrapStatement.call(this, item)).forEach(item => node.body.body.unshift(item));
        if (node.thisExpectedTypeAllProps) {
            node.body.body.unshift(utils.wrapStatement.call(this, Generator.genExpectationAll.call(this, thisIdent, node.thisExpectedTypeAllProps)));
        }
        if (node.thisExpectedTypeSpecifiedPropName) {
            for (let i = 0; i < node.thisExpectedTypeSpecifiedPropName.length; i++) {
                node.body.body.unshift(utils.wrapStatement.call(this, 
                    Generator.genExpectationSpecifiedProp.call(this, thisIdent, node.thisExpectedTypeSpecifiedPropName[i], node.thisExpectedTypeSpecifiedPropType[i])));
            }
        }
        if (node.thisExpectedTypeNumProps) {
            for (let i = 0; i < node.thisExpectedTypeNumProps.length; i++) {
                node.body.body.unshift(utils.wrapStatement.call(this, 
                    Generator.genExpectationNumProps.call(this, thisIdent, node.thisExpectedTypeNumProps[i], node.thisExpectedTypeNumPropsAnn[i])));
            }
        }

    }
    node.params.forEach(param => {
        Generator.genExpectation.call(this, param, param.expectedType).map(item => utils.wrapStatement.call(this, item)).forEach(item => node.body.body.unshift(item));
/*        if (param.thisType != undefined) {
            Generator.genThisExpectation.call(this, param.thisType).map(item => utils.wrapStatement.call(this, item)).forEach(item => node.body.body.unshift(item));
        }*/
        if (param.expectedTypeAllProps) {
            node.body.body.unshift(utils.wrapStatement.call(this, Generator.genExpectationAll.call(this, param, param.expectedTypeAllProps)));
        }
        if (param.expectedTypeSpecifiedPropName) {
            for (let i = 0; i < param.expectedTypeSpecifiedPropName.length; i++) {
                node.body.body.unshift(utils.wrapStatement.call(this, 
                    Generator.genExpectationSpecifiedProp.call(this, param, param.expectedTypeSpecifiedPropName[i], param.expectedTypeSpecifiedPropType[i])));
            }
        }
        if (param.expectedTypeNumProps) {
            for (let i = 0; i < param.expectedTypeNumProps.length; i++) {
                node.body.body.unshift(utils.wrapStatement.call(this, 
                    Generator.genExpectationNumProps.call(this, param, param.expectedTypeNumProps[i], param.expectedTypeNumPropsAnn[i])));
            }
        }
    });


    if (node.whereClause) {
        node.body.body.unshift(Generator.genAssert.call(this, node.whereClause, (node.id.name || "anonymous") + ' where clause failed'));
    }
}

function HandleIf(node, item) {
    WholeInject.call(this, node, item.consequent.body);
    if (item.alternate) {
        if (item.alternate.type === "IfStatement") {
            HandleIf.call(this, node, item.alternate);
        } else {
            WholeInject.call(this, node, item.alternate.body);
        }
    }
}

function WholeInject(node, relevent) {

    relevent.forEach(item => {
        if (item.type === "BlockStatement") {
            WholeInject.call(this, node, item.body);
        }

        if (item.type === "ForStatement" || item.type === "WhileStatement") {
            WholeInject.call(this, node, item.body.body);
        }

        if (item.type === "IfStatement") {
            HandleIf.call(this, node, item);
        }
    });

    RecursiveInject.call(this, node, relevent);
}

function RecursiveInject(node, relevent) {
    let nextIdx = relevent.findIndex(item => !item._typeInjectionDone && item.type === "ReturnStatement");
    if (nextIdx != -1) {
        let toInjectExpr = Generator.genExpectation.call(this, relevent[nextIdx].argument ? relevent[nextIdx].argument : utils.makeIdent.call(this, 'undefined'), node.returnType);

        if (toInjectExpr.length) {
            let blockReplace = this.startNode();
            blockReplace.body = toInjectExpr.map(item => utils.wrapStatement.call(this, item));
            blockReplace.body.push(relevent[nextIdx]);
            blockReplace = this.finishNode(blockReplace, "BlockStatement");
            relevent[nextIdx] = blockReplace;
        }

        relevent[nextIdx]._typeInjectionDone = true;
        RecursiveInject.call(this, node, relevent);

        return;
    }
}

function InjectPostConditions(node, relevent) {
    WholeInject.call(this, node, node.body.body);
    Generator.genExpectation.call(this, utils.makeIdent.call(this, 'undefined'), node.returnType).map(item => utils.wrapStatement.call(this, item)).forEach(item => node.body.body.push(item));
}

function InjectExpectations(node) {
    InjectPreConditions.call(this, node);
    InjectPostConditions.call(this, node);
}

export default {
    injectExpectations: InjectExpectations
}