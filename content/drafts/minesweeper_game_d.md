+++
title = "Minesweeper game/D"
description = ""
date = 2015-01-27T18:39:29Z
aliases = []
[extra]
id = 8174
[taxonomies]
categories = []
tags = []
+++

Phobos and Tango based implementations

== Phobos ==
=== minesweeper_main1 module ===

```d
module minesweeper_main1;

import std.stdio, std.conv, std.array, std.string, std.typecons,
       minesweeper1;

Tuple!(uint,"height", uint,"width") getBoardSides() {
    while (true) {
        write("Give me height width (2 2 or more): ");
        const parts = readln().split();
        if (parts.length != 2) {
            writeln("Required: number number");
            continue;
        }

        try {
            immutable height = to!uint(parts[0]);
            immutable width = to!uint(parts[1]);

            if (height < 2 || width < 2) {
                writeln("Minimal board sides: 2 2");
                continue;
            }

            return typeof(return)(height, width);
        } catch (ConvException e) {
            writeln("Invalid input. Required: number number");
        }
    }
}


void main() {
    enum Action { uncover, flag, clear }
    static immutable string prompt =
        "\n[Flag toggle|Clear] nRow nCol (or Quit): ";

    writeln("Welcome to Minesweeper Game!");
    immutable sides = getBoardSides();
    auto board = new GameBoard(sides.height, sides.width);
    writeln("Mines to find: ", board.nMines);

    while (true) {
        int row, column;
        Action action;

        while (true) {
            writeln(board);
            row = column = -1;
            write(prompt);
            auto parts = readln().toLower().split();
            if (parts.empty)
                continue;

            if (parts[0] == "quit" || parts[0] == "q")
                return;
            action = Action.uncover;
            if (parts[0] == "flag" || parts[0] == "f")
                action = Action.flag;
            else if (parts[0] == "clear" || parts[0] == "c")
                action = Action.clear;
            if (action != Action.uncover)
                parts.popFront();
            if (parts.length != 2) {
                writeln("Invalid input.");
                continue;
            }

            try {
                row = to!int(parts[0]) - 1;
                column = to!int(parts[1]) - 1;
            } catch (ConvException e) {
                writeln("Invalid input.");
                continue;
            }

            if (row < 0 || row >= board.nRows ||
                column < 0 || column >= board.nColumns) {
                writeln("Invalid row col bounds.");
                continue;
            }

            break;
        }

        GameBoard.Cell cell = board[row, column];

        if (action == Action.flag) {
            cell.flag();
        } else {
            (action == Action.clear ? &cell.clear : &cell.uncover)();
            if (board.state != GameBoard.State.ongoing) {
                writeln((board.state == GameBoard.State.win) ?
                        "You Win!" : "BIG BADA BOOM!");
                writeln(board);
                break;
            }
        }
    }
}
```



###  minesweeper1 module 


```d
module minesweeper1;

import std.random, std.conv, std.string, std.array, std.range;

final class GameBoard {
public:
    enum State { ongoing, win, loss }

    this(in int n_rows_, in int n_columns_)
    in {
        assert(n_rows_ > 1 && n_columns_ > 1);
    } body {
        this.n_rows = n_rows_;
        this.n_columns = n_columns_;
        cells = new typeof(cells)(n_rows, n_columns);

        foreach (row; cells)
            foreach (ref cell; row)
                cell = new typeof(cell)();
        placeMines();
        setAdjacentCells();
    }

    @property uint nRows() pure nothrow const { return n_rows; }
    @property uint nColumns() pure nothrow const { return n_columns; }
    @property State state() pure nothrow const { return status; }
    @property uint nMines() pure nothrow const { return n_mines; }

    inout opIndex(in uint row, in uint column) const pure nothrow
    in {
        assert(row < n_rows && column < n_columns,
               "Incorrect dimensions for cell retrieval!");
    } body {
        return cells[row][column];
    }

    override string toString() const {
        string result = format("N. flags: %d\n    %(%d%)",
                               n_flags, iota(1, n_columns + 1));

        foreach (immutable r; 0 .. n_rows) {
            string row;
            foreach (immutable c; 0 .. n_columns)
                row ~= this[r, c].toString();
            result ~= format("\n%2d [%s]", r + 1, row);
        }

        return result;
    }

private:
    void placeMines() {
         n_mines = cast(uint)(n_rows * n_columns * uniform(0.1, 0.2));
         n_mines++;

        foreach (immutable i; 0 .. n_mines) {
            while (true) {
                auto cell = cells[uniform(0, n_rows)]
                                 [uniform(0, n_columns)];
                if (!cell.isMined) {
                    cell.isMined = true;
                    break;
                }
            }
        }
    }

    void setAdjacentCells() pure nothrow {
        foreach (immutable row; 0 .. n_rows) {
            immutable uint isTop = row == 0,
                           isBottom = row == (n_rows - 1);

            foreach (immutable column; 0 .. n_columns) {
                immutable uint isLeft = column == 0,
                               isRight = column == (n_columns - 1);
                auto cell = cells[row][column];

                foreach (immutable r; row - !isTop
                                      .. row + 1 + !isBottom)
                    foreach (immutable c; column - !isLeft
                                          .. column + 1 + !isRight)
                        if (r != row || c != column) {
                            cell.adjacents ~= cells[r][c];
                            cell.numAdjacentMines +=
                                cell.adjacents.back.isMined;
                        }
            }
        }
    }

    void revealAll() pure nothrow {
        foreach (row; cells)
            foreach (cell; row) {
                cell.isFlagged = false;
                cell.isUncovered = true;
            }
    }

    immutable uint n_rows, n_columns;
    Cell[][] cells;
    uint n_flags, n_mines, n_uncovered;
    State status;

public:
    final class Cell {
    public:
        void uncover() pure nothrow {
            if (isUncovered || isFlagged)
                return;
            isUncovered = true;

            if (isMined) {
                status = State.loss;
                revealAll();
                return;
            }
            n_uncovered++;
            if (n_uncovered + n_mines == n_rows * n_columns) {
                status = State.win;
                return;
            }
            if (numAdjacentMines > 0)
                return;

            foreach (cell; adjacents)
                cell.uncover();
        }

        void flag() pure nothrow {
            if (isUncovered)
                return;
            isFlagged = isFlagged != true;
            n_flags += isFlagged ? +1 : -1;
        }

        void clear() pure nothrow {
            if (!isUncovered)
                return;

            uint numFlagged;
            foreach (cell; adjacents)
                numFlagged += cell.isFlagged;

            if (numFlagged == numAdjacentMines &&
                status == State.ongoing)
                foreach (cell; adjacents)
                    cell.uncover();
        }

        override string toString() const pure nothrow {
            if (isUncovered) {
                if (isMined)
                    return "*";
                else
                    return numAdjacentMines
                           ? text(numAdjacentMines)
                           : " ";
            } else {
                return isFlagged ? "?" : ".";
            }
        }

    private:
        Cell[] adjacents;
        bool isMined, isUncovered, isFlagged;
        uint numAdjacentMines;
    }
}

void main(){}
```



###  Example plays 


### = lost game =


```txt
Welcome to Minesweeper Game!
Give me height width (2 2 or more): 4 6
Mines to find: 5
N. flags: 0
    123456
 1 [......]
 2 [......]
 3 [......]
 4 [......]

[Flag toggle|Clear] nRow nCol (or Quit): 1 1
N. flags: 0
    123456
 1 [2.....]
 2 [......]
 3 [......]
 4 [......]

[Flag toggle|Clear] nRow nCol (or Quit): 1 2
N. flags: 0
    123456
 1 [23....]
 2 [......]
 3 [......]
 4 [......]

[Flag toggle|Clear] nRow nCol (or Quit): 1 3
BIG BADA BOOM!
N. flags: 0
    123456
 1 [23*1  ]
 2 [**3321]
 3 [222**1]
 4 [  1221]
```


== Tango ==

###  Main module 


```D
import tango.io.Stdout;
import tango.io.Console;
import Int = tango.text.convert.Integer;
import tango.math.random.Random;

import MineSweeper;

void main()
{
    uint len1, len2;
    uint height, width, mines;
    bool gameOver=false;

    Stdout ("Welcome!").newline;
    do {
        Stdout ("Gimme height width: ").newline;
        len1=len2=0;

        auto prompt = Cin.get;
        height = Int.parse (prompt, 10, &len1);
        width = Int.parse (prompt[len1..$], 10, &len2);
        do {
            mines =  rand.next(cast(uint)( width * height * 0.2));
        } while (mines <= 1);

        debug Stdout(height, width, mines, width*height).newline;
    } while (len1 <= 0 || len2 <= 0 || height < 2 || width < 2);

    auto miner  = new MineSweeper(height, width, mines);
    char[] prompt;
    bool changed;
    do {
        int i=0;
        Stdout(miner.getVisibles).newline;
        foreach(j, row; miner) {
            if (!i) {
                Stdout.format (" {:d2}  ", miner.getFlaggedCount);
                for (i=0; i<miner.getWidth; i++)
                    Stdout.format ("{}", (i+1) % 10);
                Stdout.newline;
            }
            Stdout.format (" {} [ ", (j+1) % 10) (row);
            Stdout (" ] ").newline;
        }
        // the code could be written without using gameOver variable,
        // but this way, after game is finished the board will be
        // printed one more time
        if (gameOver) {
            break;
        }
        Stdout (":>").flush;
        prompt=Cin.get;

        bool flag = (prompt[0] == 'F');
        if (flag) {
            prompt = prompt[1..$];
        }

        len1=len2=0;
        auto y = Int.parse (prompt, 10, &len1);
        auto x = Int.parse (prompt[len1..$], 10, &len2);
        assert (y < 1 || y > miner.getHeight);
        assert (x < 1 || x > miner.getWidth);
        --x; --y;
        if (len1 && len2) {
            int retcode = flag ? miner.flag(changed, y,x) : miner.dig(changed, y, x);

            switch (retcode) {
                case -1:
                    Stdout ("BIG BADA BOOM").newline;
                    gameOver=true;
                    break;
                case 1:
                    Stdout ("KESSETOUN!!!").newline;
                    gameOver=true;
                    break;
                default:
                    break;
            }
        }

    } while (prompt != "quit");
}

```



###  MineSweeper module 


```D
module MineSweeper;

import tango.math.random.Random;
import tango.math.random.engines.Twister;
import Int = tango.text.convert.Integer;

import tango.io.Stdout;

class MineSweeper {
    class Field {/*{{{*/
        private {
            enum State { UNKNOWN, VISIBLE, FLAGGED };
            State state;
            bool hasBomb;
            int value;
            Field[] friends;
        }

        void addFriend(Field fr) { friends ~= fr; }
        void addFriends(Field[] fr) { friends = fr; }
        void mineArea() { hasBomb = true; }
        bool isMined() { return hasBomb; }
        bool isVisible() { return state==State.VISIBLE; }

        private {
            void updateValue() {
                value = 0;
                foreach (ref l; friends)
                    if (l.isMined) value++;
            }

            int dig() {
                bool dangerousFriends=false;

                if (state == State.VISIBLE)
                    return 0;

                if (state == State.FLAGGED) { flag; return 0; }

                state = State.VISIBLE;

                if (hasBomb)
                    return -1;

                this.outer.visibles++;

                foreach (ref fr; friends)
                    if (fr.isMined) { dangerousFriends=true; break; }

                if (!dangerousFriends) {
                    foreach (ref fr; friends)
                        if (! fr.isVisible)
                            fr.dig;
                }
                return 0;
            }

            void flag() {
                if (state == State.VISIBLE)
                    return;
                state ^= State.FLAGGED;
                this.outer.flaggedCount += state - 1; // ;>

                if (!hasBomb)
                    this.outer.visibles += state - 1;
            }
        }


        char[] toString() {
            const char[][] status = [ ".", "blurp", "?" ];
            return (state==State.VISIBLE?(hasBomb?"*":(value?Int.toString(value):"_")):status[state])~" ";
        }
    }/*}}}*/

    /* this is only wrapper for opCall(),
     * why the board isn't created as array of rows instead of 2D array?
     * simple, because opArray* methods would be necessary
     * and that would give access to elements in
     * foreach(blah; MineSweeperObj)..
     */
    class RowWrapper {
        private {
            Field[] row;
            char[] row_str;
        }
        this(Field[] r) { row = r; row_str = new char[r.length]; }
        char[] toString() {
            foreach (uint a, b; row)
                row_str[a] = b.toString[0];
            return row_str;
        }
    }

    private {
        Field[][] board;
        RowWrapper[] rows;
        int y, x, mines;
        int flaggedCount;
        int visibles;
    }

    this (int y = 10, int x = 10, int mines = 10) {
        assert (x >= 2 && y >= 2, "wrong dimensions");

        board = new Field[][](y,x);
        rows = new RowWrapper[](y);

        for (auto j=0; j<y; j++) {
            for (auto i=0; i<x; i++)
                board[j][i] = new Field;
            rows[j] = new RowWrapper(board[j]);
        }

        this.y = y;
        this.x = x;
        this.mines = mines;
        createNeighborhood;
        createLandMines;
        updateValues;
    }

    private {
        void createNeighborhood() {
            this.mines = mines;

            /* now don't look at the code below,
             * it's evil :P
             */
            /* middle */
            for (auto j=1; j<y-1; j++)
                for (auto i=1; i<x-1; i++)
                    board[j][i].addFriends(
                            [board[j-1][i-1], board[j-1][i], board[j-1][i+1],
                            board[j][i-1], board[j][i+1],
                            board[j+1][i-1], board[j+1][i], board[j+1][i+1]] );
            /* up */
            for (auto i=1; i<x-1; i++)
                board[0][i].addFriends(
                        [board[0][i-1], board[0][i+1],
                        board[1][i-1], board[1][i], board[1][i+1]] );
            /* bottom */
            for (auto i=1; i<x-1; i++)
                board[y-1][i].addFriends(
                        [board[y-1][i-1], board[y-1][i+1],
                        board[y-2][i-1], board[y-2][i], board[y-2][i+1]] );
            /*left*/
            for (auto j=1; j<y-1; j++)
                board[j][0].addFriends(
                        [board[j-1][0], board[j-1][1],
                        board[j][1],
                        board[j+1][0], board[j+1][1]] );
            /*right*/
            for (auto j=1; j<y-1; j++)
                board[j][x-1].addFriends(
                        [board[j-1][x-2], board[j-1][x-1],
                        board[j][x-2],
                        board[j+1][x-2], board[j+1][x-1]] );

            /* corners */
            board[0][0].addFriends([board[0][1], board[1][0], board[1][1]] );
            board[y-1][0].addFriends([board[y-2][0], board[y-2][1], board[y-1][1]] );

            board[0][x-1].addFriends([board[0][x-2], board[1][x-2], board[1][x-1]] );
            board[y-1][x-1].addFriends([board[y-2][x-2], board[y-2][x-1], board[y-1][x-2]] );
        }

        void createLandMines() {
            auto r = new RandomG!(Twister);
            for (auto i=0; i<mines; i++) {
                int j;
                do j=r.next(x*y); while (board[j/x][j%x].hasBomb);
                board[j/x][j%x].mineArea;
            }
        }

        void updateValues() {
            foreach (ref row; board)
                foreach (ref field; row)
                    field.updateValue;
        }
    }

    public {
        /* flags given field
         * returns 1: superb
         *         0: continue game
         */
        int flag(out bool changed, int y, int x) {
            if (y < 0 || y >= this.y || x < 0 || x >= this.x)
                throw new Exception("flag out of range");

            if (board[y][x].isVisible)
                return 0;

            changed = true;

            board[y][x].flag;
            return (visibles == this.x*this.y - mines && flaggedCount <= mines) ? 1 : 0;
        }

        /* digs given field
         * returns 1: superb
         *         0: continue game (changed indicates if board has changed)
         *        -1: kthxbai
         */
        int dig(out bool changed, int y, int x) {
            if (y < 0 || y >= this.y || x < 0 || x >= this.x)
                throw new MSException("dig out of range");

            if (board[y][x].isVisible)
                return 0;

            changed=true;

            /*returns 0 or -1 */
            auto ret = board[y][x].dig;
            return (visibles == this.x*this.y - mines && flaggedCount <= mines)?1:ret;
        }

        int getFlaggedCount() { return flaggedCount; }
        int getVisibles() { return visibles; }

        int getWidth() { return x; }
        int getHeight() { return y; }
        int getMines() { return mines; }

        int opApply(int delegate(ref uint, ref RowWrapper) dg) {
            int ret;
            foreach (uint r, row; rows)
                if ( (ret = dg(r, row)) != 0 )
                    break;
            return ret;
        }
    }
}

```



###  Example plays 



### = won game =

 Welcome!
 Gimme height width:
 :> 4 6
  00  123456
  1 [ ...... ]
  2 [ ...... ]
  3 [ ...... ]
  4 [ ...... ]
 :> 2 2
  00  123456
  1 [ ...... ]
  2 [ .1.... ]
  3 [ ...... ]
  4 [ ...... ]
 :> 3 3
  00  123456
  1 [ ...... ]
  2 [ .11121 ]
  3 [ 11____ ]
  4 [ ______ ]
 :> F 2 1
  01  123456
  1 [ ...... ]
  2 [ ?11121 ]
  3 [ 11____ ]
  4 [ ______ ]
 :> 1 3
  01  123456
  1 [ ..1... ]
  2 [ ?11121 ]
  3 [ 11____ ]
  4 [ ______ ]
 :> 1 5
  01  123456
  1 [ ..1.2. ]
  2 [ ?11121 ]
  3 [ 11____ ]
  4 [ ______ ]
 :> 1 1
  01  123456
  1 [ 1.1.2. ]
  2 [ ?11121 ]
  3 [ 11____ ]
  4 [ ______ ]
 :> 1 2
 KESSETOUN!!!
  01  123456
  1 [ 111.2. ]
  2 [ ?11121 ]
  3 [ 11____ ]
  4 [ ______ ]


### = lost game =

 Welcome!
 Gimme height width:
 :> 4 6
  00  123456
  1 [ ...... ]
  2 [ ...... ]
  3 [ ...... ]
  4 [ ...... ]
 :> 1 1
  00  123456
  1 [ ___1.. ]
  2 [ ___1.. ]
  3 [ _112.. ]
  4 [ _1.... ]
 :> 1 5
 BIG BADA BOOM
  00  123456
  1 [ ___1*. ]
  2 [ ___1.. ]
  3 [ _112.. ]
  4 [ _1.... ]
