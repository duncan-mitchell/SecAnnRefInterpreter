SecAnn <!A * B!>;
SecAnn <!C!> Extends <!B!>;

var f = function(y : <!B * A!>) {
	return y;
}
f(true as <!A * B!>);