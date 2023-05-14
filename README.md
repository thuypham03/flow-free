# CS3110 - Final Project

## Team members:
- Thuy Pham (tpp38)
- Raina Hoffmann (roh27)
- Jeana Hoffmann (jmh472)

## CITATIONS
https://github.com/thomasahle/numberlink 
Everything in the ``numberlink`` folder is from the source link above by Thomas Dybdahl Ahle (see more in numberlink/citation.txt) (except the file ``gen_json.py`` which was written by Thuy)
We refer to this code for generating a random board and this is the only code in our project that is from an outside source. It is in Python and does not count towards our lines of code. **We do not claim this code to be ours in any way and only use it to enhance the enjoyment of our game.**

## About
Our project implements the mobile game FlowFree. It is a pipe connecting game where the goal is to connect together p pipes (of p different colors). On a square grid of board size n x n, there will be 2p nodes, 2 of each of p colors. We connect the same color circles to create a pipe. The board is solved and complete when the entire board is covered, there are no overlapping pipes, and the pipes are all valid (made using valid moves, no going outside the board).

This is a puzzle game where the objective is to connect each pair of different colored circles together. There are three difficulties: EASY - generates a random board of size 5x5 - 8x8 MEDIUM - generates a random board of size 9x9 - 12x12 HARD - generates a random board of size 13x13 - 15x15 There is only one solution to each puzzle. Pipes should fill the entire board. You cannot overlap pipes. Click a circle node to start. Use the keys WASD to move Up, Left, Down, and Right, respectively. To clear the current pipe, click a circle. To clear the entire board, press "O"

## Install Directions

Our program requires the installation of OCaml Graphics and some Python libraries:

1. To install OCaml Graphics: open a new terminal and run ``opam install graphics``

2. Running and playing Flow Once you have this installed, it's time to run our game!

a. Download the submitted source code zip file from CMS.

b. Make a new folder in VS Code to run our program in a directory of your choice (perhaps your 3110 directory) by running: ``mkdir FlowMS3`` then ``cd FlowMS3`` 

c. Open VS Code in FlowMS3 by running ``code .`` 

d. Drag and drop the downloaded submission zip file into folder FlowMS3 by the means specific to your computer. 

e. Open the zip file in the folder FlowMS3 by running ``unzip ms3_code.zip``

This will extract our submission code into a folder directory named ``ms3_code``. Now run: ``cd ms3_code`` to access this directory.

f. You are now in the ``ms3_code`` directory. From here you will be able to run the "make" commands.

g. Run ``make build``

3. To install Python libraries, use your pip installer to install the required libraries (they should be marked in yellow in VS Code if you do not have them) Example: ``pip3 install matplotlib``

4. Playing the game: Now that you have all of the modules installed, it is time to play the game!

a. Run ``make build`` then ``make play``

b. A welcome screen will pop up and a Directions panel tells the user the game rules.

There are 3 difficulties: 
- EASY: random boards 5x5 - 8x8 
- MEDIUM: random boards 9x9 - 12x12
- HARD: random boards 13x13 - 15x15

Click a box to get started! To play again, close the graphics window and repeat steps a-b.