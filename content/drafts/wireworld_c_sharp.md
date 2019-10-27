+++
title = "Wireworld/C sharp"
description = ""
date = 2011-11-04T01:44:51Z
aliases = []
[extra]
id = 10776
[taxonomies]
categories = []
tags = []
+++


```c sharp
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Text;

namespace Wireworld
{
    internal static class Program
    {
        private static void Main()
        {
            const string sample = // Two clock generators sending electrons into an XOR gate.
                @"
 tH......
.        ......
 ...Ht...      .
              ....
              .  .....
              ....
 ......tH      .
.        ......
 ...Ht...";

            var state = new Wireworld(sample);

            for (uint i = 0; i <= 12; i++)
            {
                state.Step((i == 0 ? i : 1));
                state.Print();
            }
        }

        private static void Print(this Wireworld state)
        {
            Console.WriteLine("Generation {0}", state.Generation);
            Console.WriteLine(state.StateString);
            Console.WriteLine("Generated in {0} ms.", state.MillisecondsToGenerate);
        }

        private class Wireworld
        {
            private enum Cell : byte
            {
                Empty,
                Conductor,
                ElectronHead,
                ElectronTail
            }

            public Wireworld(string state,
                             char conductor = '.', char head = 'H', char tail = 't', char empty = ' ')
            {
                _empty = empty;
                _conductor = conductor;
                _head = head;
                _tail = tail;

                _stateCells = GetCellsFromString(state);
                StateString = GetStringFromCells(_stateCells);

                _stopwatch = new Stopwatch();
            }

            public void Step(uint steps = 1)
            {
                _stopwatch.Restart();

                for (int step = 0; step < steps; step++)
                {
                    /*
                     * Copies the current state of the cells to a new variable so we
                     * can read from this variable and write to the current cells.
                     */
                    var oldState = new Cell[_stateCells.Length][];
                    for (int row = 0; row < _stateCells.Length; row++)
                        oldState[row] = (Cell[]) _stateCells[row].Clone();

                    for (int i = 0; i < _stateCells.Length; i++)
                    {
                        for (int o = 0; o < _stateCells[i].Length; o++)
                        {
                            /* 
                             * Check the value of the current cell (i, o).
                             * 
                             * If it's a conductor, if 1 or 2 neighbors are electron heads,
                             * we will change this cell to an electron head.
                             * 
                             * If it's an electron head, we will turn this cell into an
                             * electron tail.
                             * 
                             * If it's an electron tail, we will turn this cell into a
                             * conductor.
                             */
                            switch (oldState[i][o])
                            {
                                case Cell.Conductor:
                                    var neighbors = GetNeighborCount(oldState, i, o, Cell.ElectronHead);
                                    if (neighbors == 1 || neighbors == 2)
                                        _stateCells[i][o] = Cell.ElectronHead;
                                    break;
                                case Cell.ElectronHead:
                                    _stateCells[i][o] = Cell.ElectronTail;
                                    break;
                                case Cell.ElectronTail:
                                    _stateCells[i][o] = Cell.Conductor;
                                    break;
                            }
                        }
                    }
                }

                _stopwatch.Stop();

                Generation += steps;

                // Set the current state string to the current state, for printing.
                StateString = GetStringFromCells(_stateCells);
            }

            private byte GetNeighborCount(Cell[][] cells, int row, int column, Cell value)
            {
                byte neighbors = 0;

                // If the row of the cell is 0, we will start checking from 0 cells above.
                int rowStart = row == 0 ? 0 : -1;


                // If the row of the cell is that last row, we will end check 0 cells below.
                int rowEnd = row == cells.Length - 1 ? 0 : 1;

                // If the column of the cell is 0, we will start checking from 0 cells to the left.
                int columnStart = column == 0 ? 0 : -1;

                for (int i = rowStart; i <= rowEnd; i++)
                {
                    /* If the column of the cell is at or after the last column in this row, then we will
                     * stop checking at the last column. */
                    int columnEnd = column >= cells[row + i].Length - 1 ? (cells[row + i].Length - 1) - column : 1;

                    for (int o = columnStart; o <= columnEnd; o++)
                    {
                        /* 
                         * If the current cell is the same as the specified cell, then we will continue
                         * checking on the next cell. Otherwise, if the current cell is of the specified
                         * value, we will add to our neighbor count.
                         */

                        if (i == 0 && o == 0)
                            continue;
                        if (cells[row + i][column + o] == value)
                            neighbors++;
                    }
                }

                return neighbors;
            }

            private Cell[][] GetCellsFromString(string state)
            {
                var lines = state.Replace("\r", "").Split('\n');
                var cells = new Cell[lines.Length][];

                for (int i = 0; i < lines.Length; i++)
                {
                    string line = lines[i];
                    cells[i] = new Cell[line.Length];

                    for (int o = 0; o < line.Length; o++)
                    {
                        char c = line[o];
                        Cell thisState;

                        if (c == _empty)
                            thisState = Cell.Empty;
                        else if (c == _conductor)
                            thisState = Cell.Conductor;
                        else if (c == _head)
                            thisState = Cell.ElectronHead;
                        else if (c == _tail)
                            thisState = Cell.ElectronTail;
                        else
                            throw new ArgumentException("State string contains invalid characters.", "state");

                        cells[i][o] = thisState;
                    }
                }

                return cells;
            }

            private string GetStringFromCells(IEnumerable<Cell[]> cells)
            {
                var outputString = new StringBuilder();

                foreach (var row in cells)
                {
                    foreach (var cell in row)
                    {
                        switch (cell)
                        {
                            case Cell.Empty:
                                outputString.Append(_empty);
                                break;
                            case Cell.Conductor:
                                outputString.Append(_conductor);
                                break;
                            case Cell.ElectronHead:
                                outputString.Append(_head);
                                break;
                            case Cell.ElectronTail:
                                outputString.Append(_tail);
                                break;
                        }
                    }
                    outputString.AppendLine();
                }

                return outputString.ToString();
            }

            public uint Generation { get; private set; }

            public long MillisecondsToGenerate
            {
                get { return _stopwatch.ElapsedMilliseconds; }
            }

            public string StateString { get; private set; }

            private readonly Cell[][] _stateCells;
            private readonly Stopwatch _stopwatch;

            private readonly char _empty;
            private readonly char _conductor;
            private readonly char _head;
            private readonly char _tail;
        }
    }
}
```


'''Sample Output'''


```txt
Generation 0

 tH......
.        ......
 ...Ht...      .
              ....
              .  .....
              ....
 ......tH      .
.        ......
 ...Ht...

Generated in 0 ms.
Generation 1

 .tH.....
.        ......
 ..Ht....      .
              ....
              .  .....
              ....
 .......t      .
.        H.....
 ..Ht....

Generated in 0 ms.
Generation 2

 ..tH....
.        ......
 .Ht.....      .
              ....
              .  .....
              ....
 ........      .
.        tH....
 .Ht....H

Generated in 0 ms.
Generation 3

 ...tH...
.        ......
 Ht......      .
              ....
              .  .....
              ....
 ........      .
.        .tH...
 Ht....Ht

Generated in 0 ms.
Generation 4

 ....tH..
H        ......
 t.......      .
              ....
              .  .....
              ....
 ........      .
H        ..tH..
 t....Ht.

Generated in 0 ms.
Generation 5

 H....tH.
t        ......
 ........      .
              ....
              .  .....
              ....
 H.......      .
t        ...tH.
 ....Ht..

Generated in 0 ms.
Generation 6

 tH....tH
.        ......
 ........      .
              ....
              .  .....
              ....
 tH......      .
.        ....tH
 ...Ht...

Generated in 0 ms.
Generation 7

 .tH....t
.        H.....
 ........      .
              ....
              .  .....
              ....
 .tH.....      H
.        .....t
 ..Ht....

Generated in 0 ms.
Generation 8

 ..tH....
.        tH....
 .......H      .
              ....
              .  .....
              HHH.
 ..tH....      t
.        ......
 .Ht.....

Generated in 0 ms.
Generation 9

 ...tH...
.        .tH...
 ......Ht      .
              ....
              H  H....
              tttH
 ...tH...      .
.        ......
 Ht......

Generated in 0 ms.
Generation 10

 ....tH..
.        ..tH..
 .....Ht.      .
              HHHH
              t  tH...
              ...t
 ....tH..      .
H        ......
 t.......

Generated in 0 ms.
Generation 11

 .....tH.
.        ...tH.
 ....Ht..      .
              tttt
              .  .tH..
              ....
 H....tH.      .
t        ......
 ........

Generated in 0 ms.
Generation 12

 ......tH
.        ....tH
 ...Ht...      .
              ....
              .  ..tH.
              ....
 tH....tH      .
.        ......
 ........

Generated in 0 ms.

```

