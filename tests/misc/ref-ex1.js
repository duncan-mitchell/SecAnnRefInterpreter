SecAnn <!A * B * C!>;

function func(arg : <!A * C!>) {
	return arg as <!A * B!> drop <!C!>;
}

var x = (true&&false) as <!A * C!>;
func(x);