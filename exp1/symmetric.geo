deltal=800000.0;
calvingfront=760000.0;
calvingrange=210000.0;
resbbox=50000.0;
resbboxc=20000.0;
rescntr=50000.0;
resfront=2500.0;
nlr=100;

Point(1) = {0,0,0,rescntr};
Point(2) = {calvingfront - calvingrange,0,0,resfront};
Point(3) = {calvingfront,0,0,resfront};
Point(4) = {-deltal,-deltal,0,resbbox};
Point(5) = {-deltal,deltal,0,resbbox};
Point(6) = {deltal,deltal,0,resbbox};
Point(7) = {deltal,-deltal,0,resbbox};
Point(8) = {-deltal,0,0,resbboxc};
Point(9) = {deltal,0,0,resbboxc};
Point(10) = {0,-deltal,0,resbboxc};
Point(11) = {0,deltal,0,resbboxc};


//+ BOUNDING BOX
Line(1) = {10, 7};
//+
Line(2) = {7, 9};
//+
Line(3) = {9, 6};
//+
Line(4) = {6, 11};
//+
Line(5) = {11, 5};
//+
Line(6) = {5, 8};
//+
Line(7) = {8, 4};
//+
Line(8) = {4, 10};

//+ CIRCLE 1
Line(9) = {1, 2};
Line(10) = {2, 3};
//+

Extrude {{0, 0, 1}, {0, 0, 0}, Pi/2} {
  Curve{10}; Layers{nlr}; Recombine;
}//+
Extrude {{0, 0, 1}, {0, 0, 0}, Pi/2} {
  Curve{9};
}//+
Extrude {{0, 0, 1}, {0, 0, 0}, Pi/2} {
  Curve{11}; Layers{nlr}; Recombine;
}//+
Extrude {{0, 0, 1}, {0, 0, 0}, Pi/2} {
  Curve{15};
}//+
Extrude {{0, 0, 1}, {0, 0, 0}, Pi/2} {
  Curve{18}; Layers{nlr}; Recombine;
}//+
Extrude {{0, 0, 1}, {0, 0, 0}, Pi/2} {
  Curve{22};
}//+
Extrude {{0, 0, 1}, {0, 0, 0}, Pi/2} {
  Curve{25}; Layers{nlr}; Recombine;
}//+
Extrude {{0, 0, 1}, {0, 0, 0}, Pi/2} {
  Curve{29};
}//+//+
Line(35) = {15, 8};
//+
Line(36) = {13, 11};
//+
Line(37) = {3, 9};
//+
Line(38) = {17, 10};
//+
Curve Loop(1) = {38, 1, 2, -37, -34};
//+
Plane Surface(39) = {1};
//+
Curve Loop(2) = {13, 36, -4, -3, -37};
//+
Plane Surface(40) = {2};
//+
Curve Loop(3) = {36, 5, 6, -35, -20};
//+
Plane Surface(41) = {3};
//+
Curve Loop(4) = {35, 7, 8, -38, -27};
//+
Plane Surface(42) = {4};
//+
Physical Surface(43) = {42, 28, 31, 39, 35, 38, 40, 14, 17, 24, 21, 41};
//+
Physical Curve(44) = {8, 1};
//+
Physical Curve(45) = {2, 3};
//+
Physical Curve(46) = {4, 5};
//+
Physical Curve(47) = {6, 7};
