#!/bin/bash

#Store current working directory
ORIGIN_PATH="$(pwd)"

#Compute the absolute path of the test script
cd "$(dirname "$1")"
TARGET_REAL_PATH="$(printf "%s/%s\n" "$(pwd)" "$(basename "$1")")"

#Return to Current Working Directory
cd $ORIGIN_PATH

#cd to Script directory
cd $ORIGIN_PATH
cd "$(dirname "${BASH_SOURCE[0]}")"

#Take as input a .js source file and construct a ls5 filename.
tstFile=${TARGET_REAL_PATH}
outFile=${tstFile%?}
outFile="${outFile%?}ls5"

#store the desugared js in the ls5 file.
./../src/s5.d.byte -desugar ${tstFile} -print-src > ${outFile}

#Evaluate the Program
ocamlrun ../src/s5.d.byte ${outFile} \
	-env ../envs/regexp.env -apply \
	-desugar ../lib/js-regexp/regexp.js -to-env -apply \
	-internal-env env-vars -apply \
	-env ../envs/es5.env -apply \
	-eval