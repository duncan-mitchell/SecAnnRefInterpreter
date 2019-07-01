import "babel-polyfill";
import Prelude from './Prelude';
import tropigate from './Tropigate';

let acorn = require('acorn');
let escodegen = require('escodegen');

function dEnv(field, dVal) {
	if (process.env[field] !== undefined) {
		return process.env[field];
	}
	return dVal;
}

let doInjection = dEnv('TROP_DO_INJECT', 'YES') != 'NO';

//Inject tropigate
tropigate(acorn, doInjection);

function convert(src) {
	let comments = [], tokens = [];
	// use our plugin to transform the source
	let ast = acorn.parse(Prelude(doInjection) + src, {
	  	ranges: true,
	  	onComment: comments,
	  	onToken: tokens,
	  	plugins: { 
	  		tropigate: true
	  	}
	});
	escodegen.attachComments(ast, comments, tokens);
	return escodegen.generate(ast, {comments: true});
}

export default convert;