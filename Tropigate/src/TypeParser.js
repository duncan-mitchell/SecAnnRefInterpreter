function StartTrait(tt) {
    return this.type == tt.startTrait;
}

function ParseTrait(tt) {

    let startPos = this.start,
        startLoc = this.startLoc;
    let annotation = this.parseSubscripts(this.parseExprAtom({}), startPos, startLoc, true, {});

    if (StartTrait.call(this, tt)) {
        this.next();
        annotation.parameters = ParseTraitList.call(this, tt);
    } else {
        annotation.parameters = [];
    }

    if (this.eat(tt.parenL)) {
        annotation.dependants = this.parseExprList(tt.parenR, true, false, {});
    } else {
        annotation.dependants = [];
    }

    return annotation;
}

function ParseTraitList(tt) {
    let traitList = [];

    do {
        if (this.eat(tt.ptrait)) {
            traitList.push({
                ptrait: this.parseSubscripts(this.parseExprAtom({}), this.start, this.startLoc, true, {})
            });
        } else {
            traitList.push(ParseTrait.call(this, tt));
        }
    } while (this.eat(tt.star));

    this.expect(tt.endTrait);

    return traitList;
}

function ParseTypeAnnotation(tt, allowBaseType) {

    if (!StartTrait.call(this, tt) && this.type !== tt.name) {
        this.unexpected();
    }

    let base, annotations, whereClause;

    if (allowBaseType && this.type == tt.name) {
        base = this.parseExpression(true, {});
    }

    if (this.eat(tt.startTrait)) {
        annotations = ParseTraitList.call(this, tt);
    }

    return {
        base: base,
        annotations: annotations ? annotations : [],
        whereClause: whereClause
    };
}

export default {
    parseTypeAnnotation: ParseTypeAnnotation
}