3dpuzzle
========

A solver for a 3D, tetris-like puzzle based on a reduction to SAT.
It uses a predefined set of blocks (`src/main/resources/pieceset-standard.3d`). 
Each block may only be used once.

Encoding
--------
The encoding is rather implicit.
Variables select the index of a possible placements of a certain piece (pieces can also be unused, i.e. there is the null placement).
Other variables specify whether a voxel is occupied by a certain piece (total voxels times number of pieces).
Then piece-specific occupation is related to general occupation.

Input format
------------
Input problems can be found under the `problems` folder. 
The format specifies a depth-map. 
There is a second input format implemented in the parser that allows undercuts.
You have to figure this format out yourself, as there are no example files.
