SecAnn <!S!>;
var obj = {a: true as <!S!>, b: false};
/*
if (typeof(obj)) {
	var props = Object.getOwnPropertyNames(obj);
	var j = 0;
	for(var i = 0; i<props.length; i++) {
		try { 
			console.log(obj[props[i]]);
			j++;
		} catch(err) {}
	}
	if (j < n) {
		throw "FailedAnnotationCheck";
	}

} else {
	throw "FailedAnnotationCheck";
}
console.log("function body");
*/

let func = function(x :E [1, <!S!>]) {
	console.log("passed at least 1 fields fields check");
}
func(obj);