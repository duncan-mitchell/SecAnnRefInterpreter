SecAnn <!A * B!>;

var obj = ({field1: "val1" as <!A!>} as <!B!>)
delete obj.field1;
console.log(obj);