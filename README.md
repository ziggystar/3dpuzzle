3dpuzzle
========

Some Scala code that allows filling arbitrary shapes with predefined pieces using a reduction to SAT.

##Update - 2D GUI
While the old code for solving 3d puzzles (described further down) still lives within the repository, new code under `puzzle2d` has shifted the focus of the project to solving 2d puzzles. There you can find the following

- new SAT encoding for 2d problems only, now supporting cardinality constraints on the pieces
- read/write of json format for piece sets and goal shapes
 
##GUI

The 2d solver comes with a (more or less undocumented) Swing GUI that supports editing and solving goal shapes (hit F12 for more controls).

- save goal shapes to `~/.2dpuzzle/instances` and load them again
- switch between pre-installed piece-sets, change cardinality of pieces in GUI
    - Tetris pieces
    - Ubongo Pieces
    - Kubix: a set of building blocks available at Amazon
- drawing of goal shapes, solver runs automatically with 10s time-out
- export of goal shapes to SVG to facilitate printing of problem instances for solving with physical pieces; if you want to print shapes, I suggest editing the resulting SVG with [Inkscape](https://inkscape.org/) to rescale to the correct dimensions of your physical puzzle pieces. Inkscape can select all elements with the same color/stroke, which comes handy for this task.
- comes with evil mixture of German and English

![screenshot](/screenshot.png?raw=true "Screenshot")


##Old Description for 3d solver part
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


