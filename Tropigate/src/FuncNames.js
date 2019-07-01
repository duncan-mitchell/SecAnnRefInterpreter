import utils from './utils';
import Generator from './Generator';
import InjectHelper from './InjectHelper';
import TypeParser from './TypeParser';

export default function(acorn, doInjection, instance, opts) {
  let tt = acorn.tokTypes;

    function identIsCp() {
      let ret = Generator.genCpAnn.call(this);
      this.next();
      return ret;
    }

    function parseIdent(inner) {
      return function(liberal) {
        let node = this.startNode()
        if (liberal && this.options.allowReserved == "never") liberal = false
        if (this.type === tt.name) {
          if (this.value === "cpAnn") {
            if(doInjection) {
              return identIsCp.call(this);
           } else { //Parsing cpAnn whenever doInjection is off. 
              this.next();
              this.eat(tt.parenL);
              return (this.parseExprList(tt.parenR, false))[1];
            }
          }
          if (!liberal && (this.strict ? this.reservedWordsStrict : this.reservedWords).test(this.value) &&
              (this.options.ecmaVersion >= 6 ||
               this.input.slice(this.start, this.end).indexOf("\\") == -1))
            this.raiseRecoverable(this.start, "The keyword '" + this.value + "' is reserved")
          if (!liberal && this.inGenerator && this.value === "yield")
            this.raiseRecoverable(this.start, "Can not use 'yield' as identifier inside a generator")
          node.name = this.value
        } else if (liberal && this.type.keyword) {
          node.name = this.type.keyword
        } else {
          this.unexpected()
        }
        this.next()
        return this.finishNode(node, "Identifier")
      }
    }

    instance.extend('parseIdent', parseIdent);
}