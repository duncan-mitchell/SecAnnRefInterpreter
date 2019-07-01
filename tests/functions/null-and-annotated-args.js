SecAnn <!A * B!>;
SecAnn <!C!> Extends <!B!>;

var f = function(x : <!Top!>, y : <!B!>) {
	return y;
}
f(null, true as <!C!>);