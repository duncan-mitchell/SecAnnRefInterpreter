SecAnn <!A * B!>;
SecAnn <!C!> Extends <!B!>;

var f = function(y : <!B!>) {
	return y;
}
f(true as <!C!>);