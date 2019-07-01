SecAnn <!S!>;

let obj = {a: true as <!S!>, b: false as <!S!>};

/*if (typeof(obj)) {
	var props = Object.getOwnPropertyNames(obj);
	for(var i = 0; i<props.length; i++) {
		console.log(obj[props[i]]);
	}
} else {
	throw "FailedAnnotationCheck";
}
console.log("function body");*/

let func = function(x :A <!S!>, bool) {
	console.log("passed all fields check");
}
func(obj, true)