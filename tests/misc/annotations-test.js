SecAnn <!A * C!>;
SecAnn <!B!> Extends <!A!>;

let x = true as <!B * C!>;
let func = function(arg : <!A * C!>) {
	return cpAnn(arg, false) drop <!C!>;
}
func(x);