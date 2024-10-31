deltal=800000.0;
calvingfront=750000.0;
calvingrange=100000.0;
resbbox=50000.0;
resbboxc=20000.0;
rescntr=50000.0;
resfront=2500.0;

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
  Curve{9}; Curve{10};
}//+
Extrude {{0, 0, 1}, {0, 0, 0}, Pi/2} {
  Curve{11}; Curve{14}; 
}
//+
Extrude {{0, 0, 1}, {0, 0, 0}, Pi/2} {
  Curve{18}; Curve{21}; 
}
//+
Extrude {{0, 0, 1}, {0, 0, 0}, Pi/2} {
  Curve{25}; Curve{28}; 
}
//+
Line(38) = {15, 11};
//+
Line(39) = {3, 9};
//+
Line(40) = {19, 8};
//+
Line(41) = {23, 10};
//+
Curve Loop(1) = {23, 40, -6, -5, -38};
//+
Plane Surface(39) = {1};
//+
Curve Loop(2) = {38, -4, -3, -39, 16};
//+
Plane Surface(40) = {2};
//+
Curve Loop(3) = {39, -2, -1, -41, 37};
//+
Plane Surface(41) = {3};
//+
Curve Loop(4) = {41, -8, -7, -40, 30};
//+
Plane Surface(42) = {4};
//+
Physical Surface(43) = {27, 20, 13, 34, 38, 31, 24, 17, 40, 39, 42, 41};
//+
Physical Curve(44) = {6, 7};
//+
Physical Curve(45) = {8, 1};
//+
Physical Curve(46) = {2, 3};
//+
Physical Curve(47) = {4, 5};
