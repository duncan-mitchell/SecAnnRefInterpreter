SecAnn <!A * B * C * D!>;

var obj = {
	one: true as <!B * C!>,
	two: 3 as <!B!>,
	three: "Hello" as <!B * C * D!>
} as <!A!>;


var func = function (o : <!A!> :S ["two", <!B!>] :A <!B!> :S ["three", <!D!>] :S ["one", <!C!>] :E [2, <!C!>]) {
	console.log("passed annotation checks.");
}
func(obj);
