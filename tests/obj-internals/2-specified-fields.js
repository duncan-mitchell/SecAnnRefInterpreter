SecAnn <!B * C!>
var obj = {a: true as <!B!>, b: false as <!C!>};

let func = function(x :S ["a", <!B!>] :S ["b", <!C!>]) {
	console.log("passed specified fields check");
}

func(obj);