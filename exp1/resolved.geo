deltal=800000.0;
resbbox=25000.0;
rescntr=25000.0;
resfront=2500.0;
// circle
Point(1) = {0,0,0,rescntr};
Point(2) = {0.5*deltal,0,0,resfront};
Point(3) = {0.75*deltal,0,0,resfront};
Point(4) = {deltal,0,0,resbbox};
Point(5) = {-deltal,-deltal,0,resbbox};
Point(6) = {-deltal,deltal,0,resbbox};
Point(7) = {deltal,deltal,0,resbbox};
Point(8) = {deltal,-deltal,0,resbbox};
//+
Line(1) = {1, 2};
//+
Line(2) = {2, 3};
//+
Line(3) = {3, 4};
//+
Extrude {{0, 0, 1}, {0, 0, 0}, Pi/2} {
  Curve{1}; Curve{2}; Curve{3};
}
Extrude {{0, 0, 1}, {0, 0, 0}, Pi/2} {
  Curve{4}; Curve{7}; Curve{11};
}
Extrude {{0, 0, 1}, {0, 0, 0}, Pi/2} {
  Curve{15}; Curve{18}; Curve{22};
}
Extrude {{0, 0, 1}, {0, 0, 0}, Pi/2} {
  Curve{26}; Curve{29}; Curve{33};
}
//+
Line(47) = {26, 5};
//+
Line(48) = {5, 20};
//+
Line(49) = {20, 6};
//+
Line(50) = {6, 14};
//+
Line(51) = {14, 7};
//+
Line(52) = {7, 4};
//+
Line(53) = {4, 8};
//+
Line(54) = {8, 26};
//+
Curve Loop(1) = {24, 49, 50};
//+
Plane Surface(48) = {1};
//+
Curve Loop(2) = {35, 47, 48};
//+
Plane Surface(49) = {2};
//+
Curve Loop(3) = {54, 46, 53};
Plane Surface(50) = {3};
//+
Curve Loop(4) = {13, 51, 52};
Plane Surface(51) = {4};

//+
Physical Surface(55) = {49, 36, 32, 28, 48, 25, 21, 17, 51, 14, 10, 6, 50, 47, 43, 39};
//+
Physical Curve(56) = {49, 48};
//+
Physical Curve(57) = {47, 54};
//+
Physical Curve(58) = {52, 53};
//+
Physical Curve(59) = {50, 51};
