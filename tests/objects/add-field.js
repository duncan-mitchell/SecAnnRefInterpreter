SecAnn <!A * B * C!>;

var obj = ({field1: "val1" as <!A!>} as <!B!>);
obj.field2 = "val2" as <!C!>;
console.log(obj);