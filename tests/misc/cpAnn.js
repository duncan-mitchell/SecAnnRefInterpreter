SecAnn <!A * C!>;
SecAnn <!B!> Extends <!A!>;

let x = true as <!B!>;
let func = function(arg : <!A!>) {
	return cpAnn(arg, false as <!C!>);
}
func(x);