#!/bin/bash

#Store current working directory
ORIGIN_PATH="$(pwd)"

#Compute the absolute path of the test script
cd "$(dirname "$1")"
TARGET_REAL_PATH="$(printf "%s/%s\n" "$(pwd)" "$(basename "$1")")"

#Return to Current Working Directory
cd $ORIGIN_PATH

#Construct transformed tropigated / ls5 filenames.
outFile=${TARGET_REAL_PATH%?}
outFileTropigate="${outFile%?}an.js"
outFileLS5="${outFileTropigate%?}"
outFileLS5="${outFileLS5%?}ls5"

#Construct paths to Tropigate and LS5 directories if supplied
pathToTropigate=${2:-Tropigate/} 
pathToLS5=${3:-SecurityAnnotationsS5/}

#run tropigate on the js file
NO_COMPILE=1 TROP_DO_INJECT=YES ${pathToTropigate}/tropigate $1 $outFileTropigate

#run ls5 interpreter on the script
${pathToLS5}/scripts/ls5.sh $outFileTropigate

#cleanup the outfiles
rm $outFileLS5
rm $outFileTropigate