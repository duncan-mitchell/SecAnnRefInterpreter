SecAnn <!A!>;
SecAnn <!B!>;

let id = function(: <!B!>, arg : <!A!>) {
	return this.val;
}
let obj = {f : id, val: "hello world" } as <!B!>;
obj.f(true as <!A!>);