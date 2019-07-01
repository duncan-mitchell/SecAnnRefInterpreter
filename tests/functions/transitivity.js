SecAnn <!A * B!>;
SecAnn <!C!> Extends <!B!>;
SecAnn <!D!> Extends <!C!>;

var f = function(y : <!B!>) {
	return y;
}
f(true as <!D!>);