SecAnn <!A * C!>;
SecAnn <!B!> Extends <!A!>;

let x = true as <!B * C!>;
let func = function(arg : <!A!>) {
	return arg drop <!C!>;
}
func(x);