SecAnn <!A* C!>
var obj = {a: true as <!A*C!> , b: false as <!A!>};
let prop = "a";

/* Mockup of what the wrapper looks like
let func2 = function(x, y) {
	if (typeof(x) && (Object.getOwnPropertyNames(x).indexOf(y)>=0)) {
		console.log(x[y]);
	} else {
		throw "oh dear";
	}
	console.log("function body");
}*/

let func = function(x :S ["a", <!A*C!>]) {
	console.log("passed specified fields check");
}

func(obj);