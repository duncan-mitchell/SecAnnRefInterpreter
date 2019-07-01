SecAnn <!A!>;

var f = function(x : <!A!>) {
	return x;
}
var f2 = function(x : <!Top!>) {
	return x(true as <!A!>);
}
f2(f);