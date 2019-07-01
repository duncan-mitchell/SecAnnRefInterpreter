SecurityAnnotation.declare("A * B");
var x = SecurityAnnotation.as(true, "A");
SecurityAnnotation.assert(x, "A");