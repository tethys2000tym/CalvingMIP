lc=5000;
// parameters: resolution and rectangle length and width
// can be changed in line using e.g. "-setnumber lc [VALUE]"
DefineConstant[ lc = {10.000e3, Name "resolution"},
                x1 = {-800.000e3, Name "n lenght"},
                x2 = {800.000e3, Name "lenght"},
                y1 = {-800.000e3, Name "n width"},
                y2 = {800.000e3, Name "width"}];

////////////////////////////////////////////////////////

// Points
Point(1) = {0, 0, 0, lc};
Point(2) = {x2, 0, 0, lc};
Point(3) = {x2, y2, 0, lc};
Point(4) = {0, y2, 0, lc};
Point(5) = {x1, y1, 0, lc};
Point(6) = {x1, 0, 0, lc};
Point(7) = {0, y1, 0, lc};
Point(8) = {x2, y1, 0, lc};
Point(9) = {x1, y2, 0, lc};
// Lines
Line(1) = {1, 2};
Line(2) = {2, 3};
Line(3) = {3, 4};
Line(4) = {4, 1};
Line(5) = {4, 9};
Line(6) = {9, 6};
Line(7) = {6, 1};
Line(8) = {1, 7};
Line(9) = {7, 8};
Line(10) = {8, 2};
Line(11) = {6, 5};
Line(12) = {5, 7};
// Surfaces
//Curve Loop(1) = {6, 7, -4, 5};
Line Loop(1) = {6, 7, -4, 5};
Plane Surface(4) = {1};
//+
//Curve Loop(2) = {4, 1, 2, 3};
Line Loop(2) = {4, 1, 2, 3};
Plane Surface(1) = {2};
//+
//Curve Loop(3) = {8, 9, 10, -1};
Line Loop(3) = {8, 9, 10, -1};
Plane Surface(2) = {3};
//+
//Curve Loop(4) = {7, 8, -12, -11};
Line Loop(4) = {7, 8, -12, -11};
Plane Surface(3) = {4};
//+
Physical Surface(5) = {4, 1, 3, 2};
//+
Physical Line(1) = {6, 11};
//+
Physical Line(2) = {12, 9};
//+
Physical Line(3) = {10, 2};
//+
Physical Line(4) = {3, 5};
