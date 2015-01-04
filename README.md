3dpuzzle
========

A solver for a 3D, tetris-like puzzle based on a reduction to SAT.

The goal is to fill a predefined form with a given set of blocks (see `pieceset-standard.3d` under resources). 
Each block may only be used at most once.

### Tetris
Using the `--tetris` option, the standard set of tetris pieces is loaded, together with the option to allow multiple placements
of the same piece. The tetris pieces are ordinary 3d pieces using only two dimensions. Since they will be rotated around
three dimensions, the pieces are used potentially mirrored in a 2d setting.


SAT-Encoding
--------
The encoding is rather implicit.
 - Variables select the index of a possible placements of a certain piece (pieces can also be unused, i.e. there is the null placement).
 - Other variables specify whether a voxel is occupied by a certain piece (total voxels times number of pieces).
 - Then piece-specific occupation is related to general occupation.

Input format
------------
Input problems can be found under the `problems` folder. 

###Depth Map

    solid(
    00123,
    00012,
    00001,
    00000,
    00000
    )

###Voxel
No example yet. See parser implementation in `PieceParser.scala`.

###2D
Allows to draw two dimensional problems using ASCII art (`#` as filled). Only one problem per file. Specify file using
`--file-2d`.

    2D
        ##
        ##
    #######
      #####
      ##
      ##


