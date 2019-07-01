SecurityAnnotation.declare("A");
SecurityAnnotation.declare("B");
SecurityAnnotation.extends("C", "A");

var x = SecurityAnnotation.as(true, "A * B");
x = SecurityAnnotation.cpAnn(SecurityAnnotation.drop(x, "B"), false);
SecurityAnnotation.assert(x, "A");