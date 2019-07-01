SecAnn <!A * B!>;

var obj = ({field1: "val1" as <!A!>} as <!B!>);
obj["field1"] = 1.0 as <!B!>;
console.log(obj);