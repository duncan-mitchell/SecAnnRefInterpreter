# SecAnnRefInterpreter
A reference interpreter for Security Annotations in JavaScript.

This implementation comprises the artifact accompanying the paper *A Formal Model for Checking Cryptographic API Usage in JavaScript*, which appears in the European Symposium On Research in Computer Security 2019, *ESORICS'19*.

## Install
Follow the install instructions included in both `SecurityAnnotationsS5/` and `Tropigate/`.

## Usage
In order to run a JavaScript file with S5 extended with Security Annotations, use:
`./runRefInterpreter.sh FILENAME`

## Notes on Examples
The small example given in Listing 1 of the paper, together with the annotated library in-lined, can be found in `examples/listing1.js`, with minor alterations to allow execution within this framework. A Security Annotation error will be thrown, since the message to be signed is not annotated with the Ciphertext annotation.

Section 5 of the paper describes a client-server application. The paper outlines the modifications necessary to allow execution in this framework, along with a discussion of a security bug.
1. `paper-examples/caseStudy/node-bug.js` and `paper-examples/caseStudy/node-fixed.js` contain the Node developer code for this application both with and without the bug discussed in the paper.
2. `paper-examples/caseStudy/refInt-bug.js` and `paper-examples/refInt-fixed.js` contain JavaScript programs with in-lined library mocks enabling the testing of this application within the reference interpreter; these demonstrate the reference interpreter's ability to detect such security property violations.