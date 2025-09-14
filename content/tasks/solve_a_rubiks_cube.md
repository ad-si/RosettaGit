+++
title = "Solve a Rubik's Cube"
description = ""
date = 2018-06-22T10:01:50Z
aliases = []
[extra]
id = 21285
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "go",
  "kotlin",
  "phix",
]
+++

Create a program that is capable of solving a Rubik's Cube as efficiently as possible.

You may use any sort of input you wish.



## Go

As in the case of the Kotlin entry, this is a translation of the C++ competition code by Stefan Pochmann.

On the same machine, typical timings for the 100 line dataset are just over 200 milliseconds which is significantly slower than both the Kotlin and C++ code but still acceptable.

The relative slowness may be partly due to maps in Go not being able to accept reference types (such as slices) as a key because they don't support the '==' operator which tests for structural equality. I've therefore had to copy each slice returned by the 'id' function into a forty element array (a value type in Go) and use that as a key instead.

For the single line example, typical timings are around 240 milliseconds which is much faster than Kotlin due, no doubt, to JVM warm up time.  

```go
/**********************************************************************
 *
 * A cube 'state' is an int array with 40 entries, the first 20
 * are a permutation of {0,...,19} and describe which cubie is at
 * a certain position (regarding the input ordering). The first
 * twelve are for edges, the last eight for corners.
 *
 * The last 20 entries are for the orientations, each describing
 * how often the cubie at a certain position has been turned
 * counterclockwise away from the correct orientation. Again the
 * first twelve are edges, the last eight are corners. The values
 * are 0 or 1 for edges and 0, 1 or 2 for corners.
 *
 **********************************************************************/

package main

import (
    "bufio"
    "fmt"
    "log"
    "os"
    "strings"
    "time"
)

type ai = [40]int

var applicableMoves = [5]int{0, 262143, 259263, 74943, 74898}

var phase = 0

var affectedCubies = [6][8]int{
    {0, 1, 2, 3, 0, 1, 2, 3},   // U
    {4, 7, 6, 5, 4, 5, 6, 7},   // D
    {0, 9, 4, 8, 0, 3, 5, 4},   // F
    {2, 10, 6, 11, 2, 1, 7, 6}, // B
    {3, 11, 7, 9, 3, 2, 6, 5},  // L
    {1, 8, 5, 10, 1, 0, 4, 7},  // R
}

func btoi(b bool) int {
    if b {
        return 1
    }
    return 0
}

func sliceToAi(s []int) ai {
    var a ai
    copy(a[:], s)
    for i := len(s); i < 40; i++ {
        a[i] = -1
    }
    return a
}

func applyMove(move int, state ai) ai {
    turns := move%3 + 1
    face := move / 3
    for turns != 0 {
        turns--
        oldState := state
        for i := 0; i < 8; i++ {
            isCorner := btoi(i > 3)
            target := affectedCubies[face][i] + isCorner*12
            temp := i + 1
            if (i & 3) == 3 {
                temp = i - 3
            }
            killer := affectedCubies[face][temp] + isCorner*12
            var orientationDelta int
            switch {
            case i < 4:
                orientationDelta = btoi(face > 1 && face < 4)
            case face < 2:
                orientationDelta = 0
            default:
                orientationDelta = 2 - (i & 1)
            }
            state[target] = oldState[killer]
            state[target+20] = oldState[killer+20] + orientationDelta
            if turns == 0 {
                state[target+20] %= 2 + isCorner
            }
        }
    }
    return state
}

func inverse(move int) int {
    return move + 2 - 2*(move%3)
}

func id(state ai) ai {
    //--- Phase 1: Edge orientations.
    if phase < 2 {
        return sliceToAi(state[20:32])
    }

    //-- Phase 2: Corner orientations, E slice edges.
    if phase < 3 {
        result := state[31:40]
        for e := uint(0); e < 12; e++ {
            result[0] |= (state[e] / 8) << e
        }
        return sliceToAi(result)
    }

    //--- Phase 3: Edge slices M and S, corner tetrads, overall parity.
    if phase < 4 {
        result := []int{0, 0, 0}
        for e := uint(0); e < 12; e++ {
            temp := 2
            if state[e] <= 7 {
                temp = state[e] & 1
            }
            result[0] |= temp << (2 * e)
        }
        for c := uint(0); c < 8; c++ {
            result[1] |= ((state[c+12] - 12) & 5) << (3 * c)
        }
        for i := 12; i < 19; i++ {
            for j := i + 1; j < 20; j++ {
                result[2] ^= btoi(state[i] > state[j])
            }
        }
        return sliceToAi(result)
    }

    //--- Phase 4: The rest.
    return state
}

func main() {
    startTime := time.Now()
    aggregateMoves := 0

    //--- Define the goal.
    goal := [20]string{
        "UF", "UR", "UB", "UL", "DF", "DR", "DB", "DL", "FR", "FL", "BR", "BL",
        "UFR", "URB", "UBL", "ULF", "DRF", "DFL", "DLB", "DBR",
    }

    //--- Load dataset (file name should be passed as a command line argument).
    if len(os.Args) != 2 {
        log.Fatal("the file name should be passed as a command line argument") 
    }
    file, err := os.Open(os.Args[1])
    if err != nil {
        log.Fatal(err)       
    }
    defer file.Close()

    var lineCount = 0
    scanner := bufio.NewScanner(file)
    for scanner.Scan() {
        line := scanner.Text()
        inputs := strings.Fields(line)
        lineCount++
        phase = 0
        totalMoves := 0

        //--- Prepare current (start) and goal state.
        var currentState ai
        var goalState ai
        for i := 0; i < 20; i++ {
            //--- Goal state.
            goalState[i] = i

            //--- Current (start) state.
            cubie := inputs[i]
            for {
                idx := -1
                for c := 0; c < len(goal); c++ {
                    if goal[c] == cubie {
                        idx = c
                        break
                    }
                }
                if idx >= 0 {
                    currentState[i] = idx
                } else {
                    currentState[i] = 20
                }
                if currentState[i] != 20 {
                    break
                }
                cubie = cubie[1:] + cubie[:1]
                currentState[i+20]++
            }
        }

        //--- Dance the funky Thistlethwaite..
    nextPhase:
        for phase++; phase < 5; phase++ {

            //--- Compute ids for current and goal state, skip phase if equal.
            currentId := id(currentState)
            goalId := id(goalState)
            if currentId == goalId {
                continue
            }

            //--- Initialize the BFS queue.
            q := []ai{currentState, goalState}

            //--- Initialize the BFS tables.
            predecessor := make(map[ai]ai)
            direction := make(map[ai]int)
            lastMove := make(map[ai]int)
            direction[currentId] = 1
            direction[goalId] = 2

            //--- Dance the funky bidirectional BFS...
            for {
                //--- Get state from queue, compute its ID and get its direction.
                oldState := q[0]
                q = q[1:]
                oldId := id(oldState)
                oldDir := direction[oldId]

                //--- Apply all applicable moves to it and handle the new state.
                for move := 0; move < 18; move++ {
                    if (applicableMoves[phase] & (1 << uint(move))) != 0 {
                        //--- Apply the move.
                        newState := applyMove(move, oldState)
                        newId := id(newState)
                        newDir := direction[newId]

                        //--- Have we seen this state (id) from the other direction already?
                        //--- I.e. have we found a connection?
                        if (newDir != 0) && (newDir != oldDir) {
                            //--- Make oldId represent the forwards
                            //--- and newId the backwards search state.
                            if oldDir > 1 {
                                newId, oldId = oldId, newId
                                move = inverse(move)
                            }

                            //--- Reconstruct the connecting algorithm.
                            algorithm := []int{move}
                            for oldId != currentId {
                                algorithm = append(algorithm, 0)
                                copy(algorithm[1:], algorithm[0:])
                                algorithm[0] = lastMove[oldId]
                                oldId = predecessor[oldId]
                            }
                            for newId != goalId {
                                algorithm = append(algorithm, inverse(lastMove[newId]))
                                newId = predecessor[newId]
                            }

                            //--- Print and apply the algorithm.
                            for i := 0; i < len(algorithm); i++ {
                                fmt.Printf("%c", "UDFBLR"[algorithm[i]/3])
                                fmt.Print(algorithm[i]%3 + 1)
                                fmt.Print(" ")
                                totalMoves++
                                currentState = applyMove(algorithm[i], currentState)
                            }

                            //--- Jump to the next phase.
                            continue nextPhase
                        }

                        //--- If we've never seen this state (id) before, visit it.
                        if newDir == 0 {
                            q = append(q, newState)
                            direction[newId] = oldDir
                            lastMove[newId] = move
                            predecessor[newId] = oldId
                        }
                    }
                }
            }
        }
        fmt.Printf(" (moves %d)\n", totalMoves)
        aggregateMoves += totalMoves
    }
    if err := scanner.Err(); err != nil {
        log.Fatal(err)
    }
    endTime := time.Now()
    elapsedTime := endTime.Sub(startTime).Nanoseconds() / 1000000
    fmt.Println("\nAverage number of moves =", float64(aggregateMoves)/float64(lineCount))
    fmt.Println("\nAverage time =", elapsedTime/int64(lineCount), "milliseconds")
}
```


```txt

Same as Kotlin entry apart, of course, from the timings.

```



## Kotlin

This is a translation of Stefan Pochmann's C++ entry in the 2004 competition which was linked to by the author of the Phix entry. This program won the Judge's prize, finished second overall and (as in the case of the winner) is based on Thistlethwaite's algorithm.

I've adjusted the code to accept input from a file whose name is supplied via a command line argument and, as in the case of the original competition, to calculate the average number of moves for each line in the file and the average time to process each one.

To aid readability I've also inserted spaces between each move in the results and added the total moves needed for each line.

```scala
// version 1.2.21

/**********************************************************************
 * 
 * A cube 'state' is a vector<int> with 40 entries, the first 20
 * are a permutation of {0,...,19} and describe which cubie is at
 * a certain position (regarding the input ordering). The first
 * twelve are for edges, the last eight for corners.
 * 
 * The last 20 entries are for the orientations, each describing
 * how often the cubie at a certain position has been turned
 * counterclockwise away from the correct orientation. Again the
 * first twelve are edges, the last eight are corners. The values
 * are 0 or 1 for edges and 0, 1 or 2 for corners.
 * 
 **********************************************************************/

import java.util.ArrayDeque
import java.io.File

fun Boolean.toInt() = if (this) 1 else 0

typealias AI = ArrayList<Int>

val applicableMoves = intArrayOf(0, 262143, 259263, 74943, 74898)

val affectedCubies = listOf(
    intArrayOf(0,  1, 2,  3, 0, 1, 2, 3),  // U
    intArrayOf(4,  7, 6,  5, 4, 5, 6, 7),  // D
    intArrayOf(0,  9, 4,  8, 0, 3, 5, 4),  // F
    intArrayOf(2, 10, 6, 11, 2, 1, 7, 6),  // B
    intArrayOf(3, 11, 7,  9, 3, 2, 6, 5),  // L
    intArrayOf(1,  8, 5, 10, 1, 0, 4, 7)   // R
)

fun applyMove(move: Int, state: AI): AI {
    val state2 = AI(state)  // avoids mutating original 'state'.
    var turns = move % 3 + 1
    val face = move / 3
    while (turns-- != 0) {
        val oldState2 = AI(state2)
        for (i in 0..7) {
            val isCorner = (i > 3).toInt()
            val target = affectedCubies[face][i] + isCorner * 12
            val temp = if ((i and 3) == 3) i - 3 else i + 1
            val killer = affectedCubies[face][temp] + isCorner * 12
            val orientationDelta =
                if (i < 4) (face in 2..3).toInt()
                else if (face < 2) 0
                else 2 - (i and 1)
            state2[target] = oldState2[killer]
            state2[target + 20] = oldState2[killer + 20] + orientationDelta
            if (turns == 0) state2[target + 20] %= 2 + isCorner
        }
    }
    return state2
}

fun inverse(move: Int) = move + 2 - 2 * (move % 3)

var phase = 0

fun id(state: AI): AI {
    //--- Phase 1: Edge orientations.
    if (phase < 2) return AI(state.subList(20, 32))

    //-- Phase 2: Corner orientations, E slice edges.
    if (phase < 3) {
        val result =  AI(state.subList(31, 40))
        for (e in 0..11) result[0] = result[0] or ((state[e] / 8) shl e)
        return result
    }

    //--- Phase 3: Edge slices M and S, corner tetrads, overall parity.
    if (phase < 4) {
        val result = AI(3)
        repeat(3) { result.add(0) }
        for (e in 0..11) {
            val temp  = (if (state[e] > 7) 2 else (state[e] and 1)) shl (2 * e)
            result[0] = result[0] or temp
        }
        for (c in 0..7) {
            val temp =  ((state[c + 12] - 12) and 5) shl (3 * c)
            result[1] = result[1] or temp
        }
        for (i in 12..18) {
            for (j in (i + 1)..19) {
                result[2] = result[2] xor (state[i] > state[j]).toInt()
            }
        }
        return result
    }

    //--- Phase 4: The rest.
    return state
}

fun main(args: Array<String>) {
    val startTime = System.currentTimeMillis()
    var aggregateMoves = 0

    //--- Define the goal.
    val goal = listOf(
        "UF", "UR", "UB", "UL", "DF", "DR", "DB", "DL", "FR", "FL", "BR", "BL",
        "UFR", "URB", "UBL", "ULF", "DRF", "DFL", "DLB", "DBR"
    )

    //--- Load dataset (file name should be passed as a command line argument).
    val file = File(args[0])
    var lineCount = 0
    file.forEachLine { line ->
        val inputs = line.split(' ')
        lineCount++
        phase = 0
        var totalMoves = 0

        //--- Prepare current (start) and goal state.
        var currentState = AI(40)
        repeat(40) { currentState.add(0) }
        val goalState = AI(40)
        repeat(40) { goalState.add(0) }
        for (i in 0..19) {
            //--- Goal state.
            goalState[i] = i

            //--- Current (start) state.
            var cubie = inputs[i]
            while (true) {
                val idx = goal.indexOf(cubie)
                currentState[i] = if (idx >= 0) idx else 20
                if (currentState[i] != 20) break
                cubie = cubie.substring(1) + cubie[0]
                currentState[i + 20]++
            }
        }

        //--- Dance the funky Thistlethwaite...
        nextPhase@ while (++phase < 5) {
            //--- Compute ids for current and goal state, skip phase if equal.
            val currentId = id(currentState)
            val goalId = id(goalState)
            if (currentId == goalId) continue

            //--- Initialize the BFS queue.
            val q = ArrayDeque<AI>()
            q.addLast(currentState)
            q.addLast(goalState)

            //--- Initialize the BFS tables.
            val predecessor = mutableMapOf<AI, AI>()
            val direction = mutableMapOf<AI, Int>()
            val lastMove = mutableMapOf<AI, Int>()
            direction[currentId] = 1
            direction[goalId] = 2

            //--- Dance the funky bidirectional BFS...
            while (true) {
                //--- Get state from queue, compute its ID and get its direction. 
                val oldState = q.peek()
                q.pop()
                var oldId = id(oldState)
                val oldDir = direction.getOrPut(oldId) { 0 }

                //--- Apply all applicable moves to it and handle the new state.
                var move = 0
                while (move < 18) {
                    if ((applicableMoves[phase] and (1 shl move)) != 0) {
                        //--- Apply the move.
                        val newState = applyMove(move, oldState)
                        var newId = id(newState)
                        var newDir = direction.getOrPut(newId) { 0 }

                        //--- Have we seen this state (id) from the other direction already?
                        //--- I.e. have we found a connection?
                        if ((newDir != 0) && (newDir != oldDir)) {
                            //--- Make oldId represent the forwards
                            //--- and newId the backwards search state.
                            if (oldDir > 1) {
                                val temp = newId
                                newId = oldId
                                oldId = temp
                                move = inverse(move)
                            }

                            //--- Reconstruct the connecting algorithm.
                            val algorithm = AI()
                            algorithm.add(move)
                            while (oldId != currentId) {
                                val tempI = lastMove.getOrPut(oldId) { 0 }
                                algorithm.add(0, tempI)
                                val tempAI = predecessor.getOrPut(oldId) { AI() }
                                oldId = tempAI
                            }
                            while (newId != goalId) {
                                val tempI = lastMove.getOrPut(newId) { 0 }
                                algorithm.add(inverse(tempI))
                                val tempAI = predecessor.getOrPut(newId) { AI() }
                                newId = tempAI
                            }

                            //--- Print and apply the algorithm.
                            for (i in 0 until algorithm.size) {
                                print("UDFBLR"[algorithm[i] / 3])
                                print(algorithm[i] % 3 + 1)
                                print(" ")
                                totalMoves++
                                currentState = applyMove(algorithm[i], currentState)
                            }

                            //--- Jump to the next phase.
                            continue@nextPhase
                        }

                        //--- If we've never seen this state (id) before, visit it.

                        if (newDir == 0) {
                            q.addLast(newState)
                            direction[newId] = oldDir
                            lastMove[newId] = move
                            predecessor[newId] = oldId
                        }
                    }
                    move++
                }
            }
        }
        println(" (moves $totalMoves)")
        aggregateMoves += totalMoves
    }
    val elapsedTime = System.currentTimeMillis() - startTime
    println("\nAverage number of moves = ${aggregateMoves.toDouble() / lineCount}")
    println("\nAverage time = ${elapsedTime / lineCount} milliseconds")
}
```


Using the original dataset of 100 lines, the results were as follows (time doesn't mean much but is typical for my modest machine):
<pre style="height:45ex">
U1 U2  (moves 2)
U2  (moves 1)
U1  (moves 1)
F1 F2  (moves 2)
F2  (moves 1)
F1  (moves 1)
R1 R2  (moves 2)
R2  (moves 1)
R1  (moves 1)
D1 D2  (moves 2)
D2  (moves 1)
D1  (moves 1)
B1 B2  (moves 2)
B2  (moves 1)
B1  (moves 1)
L1 L2  (moves 2)
L2  (moves 1)
L1  (moves 1)
U2 B3 B2  (moves 3)
L2 U3  (moves 2)
R1 U1  (moves 2)
D3 L3  (moves 2)
D3 L2  (moves 2)
D2 F3 F2  (moves 3)
R2 F3  (moves 2)
R1 F2 F2 R2 F2  (moves 5)
D1 D2 U2  (moves 3)
L1 B2 F2 L2 R2 B2 F2  (moves 7)
L1 L2 D3  (moves 3)
D1 F2  (moves 2)
U2 R3  (moves 2)
L1 L2 U3  (moves 3)
U1 R2  (moves 2)
U1 R3  (moves 2)
F1 U2  (moves 2)
U2 R3 R2  (moves 3)
F2 D1 F3  (moves 3)
F2 D3 D2 U2  (moves 4)
L3 D2 R3  (moves 3)
D2 R3 R2 D3  (moves 4)
F1 R1 B2 B2 R2 B2  (moves 6)
L1 B2 F2  (moves 3)
U1 R2 B3 B2  (moves 4)
R2 F3 R2  (moves 3)
L2 D3 R3 R2  (moves 4)
L2 F3 L2 L2 F2 L2  (moves 6)
F1 R1 B3 D3 B2 D3 L3 U2 L3 U2 L2 U2 L2 U3 R2 U1 F2 L2 U2 B2 L2 F2 U2 L2 U2 F2 D2 F2 U2  (moves 29)
L1 L2 U3 R2  (moves 4)
L3 B3 B2 R3 R2  (moves 5)
B2 U1 R3 R2  (moves 4)
L3 B3 L2  (moves 3)
D1 B2 L3 L2  (moves 4)
B1 B2 R3 F2 F2 R2 F2  (moves 7)
D1 F3 D1 D2  (moves 4)
R1 B3 D1 R2 F3 L1 U2 F2 D3 B2 D1 L3 U1 F2 R2 U1 L2 U1 F2 U2 R2 U3 F2 U3 F2 D2 F2 L2 F2 L2 F2 U2 F2 D2 L2  (moves 35)
F1 R1 U3 D1 B3 U3 D2 L3 B2 R1 F2 D3 L3 U2 R2 D1 R2 B2 U2 L2 U3 U2 F2 U2 L2 B2 R2 D2 R2 D2 B2 L2 F2 U2  (moves 34)
U1 R1 F3 L1 R2 U3 L1 D2 F2 U3 L3 L2 U1 F2 D3 F2 F2 U2 D2 L2 U2 F2 R2 F2 D2 R2 U2  (moves 27)
R1 D3 R3 F3 R1 D3 F2 U1 R3 D2 U1 R3 F2 U1 L2 U3 F2 U1 B2 D2 L2 D3 F2 F2 U2 F2 D2 L2 U2 L2 F2 L2 U2 R2  (moves 34)
D2 R1 B1 L1 F3 U2 D3 L2 R1 D3 B2 U1 F2 L3 F2 U3 B2 U3 B2 L2 U3 B2 U3 F2 U2 F2 U2 L2 F2 R2 B2 D2 L2 D2 F2  (moves 35)
B3 U2 L1 F3 F2 U1 R3 D1 F2 R3 D3 B2 R3 B2 U1 F2 U3 R2 U1 R2 U3 R2 L2 F2 U2 R2 F2 D2 R2 B2 U2 R2 B2  (moves 33)
L1 F3 L2 D3 U1 F3 L1 B2 R1 U1 D1 B2 R3 U1 L2 U1 B2 U1 F2 U3 L2 D1 F2 U3 F2 U2 D2 R2 U2 L2 F2 R2 U2 F2 R2  (moves 35)
F1 R3 U2 F3 B2 U1 L1 U1 R3 B2 U1 R3 R2 U3 L2 U1 B2 U2 R2 U3 L2 U3 F2 L2 U2 F2 U2 B2 L2 D2 L2 F2 L2 F2 U2  (moves 35)
U2 D3 B3 U1 L2 D1 R1 U3 L3 D2 U1 R3 U3 B2 D3 R2 F2 U3 F2 U3 U2 B2 D2 B2 L2 F2 R2 D2 R2  (moves 29)
D2 L3 U3 F3 B2 U3 L1 U2 R3 D2 L3 U2 L2 U3 R2 B2 D3 F2 R2 U3 U2 L2 U2 F2 U2 D2 R2 F2 U2 B2 D2  (moves 31)
F1 L1 U1 L1 B3 U3 L1 F2 L1 B2 F2 U3 L3 F2 D3 B2 D3 R2 U1 F2 U3 L2 U3 U2 F2 D2 B2 U2 F2 R2 F2 L2  (moves 32)
B1 L1 U3 F3 B2 U1 L3 B2 R3 L3 U3 L3 D2 B2 D3 F2 L2 U2 R2 U3 F2 U3 F2 L2 U2 B2 L2 U2 B2 F2 R2 U2 F2  (moves 33)
F2 L3 F1 D3 B3 B2 U3 R1 D3 U3 L3 L2 R2 U3 B2 D1 B2 U3 R2 U3 F2 L2 F2 L2 U2 F2 B2 R2 B2 L2  (moves 30)
D3 F3 U1 B2 U3 L1 D1 R3 U3 F2 L3 U1 F2 U2 L2 U3 L2 B2 R2 L2 D1 F2 F2 L2 U2 F2 L2 U2 R2 D2 B2 R2 L2  (moves 33)
F2 R3 U2 F3 U2 L2 B2 D1 F2 R3 B2 D1 L3 D2 R2 B2 D1 R2 B2 U1 L2 B2 R2 B2 R2 D2 L2 D2 F2 L2 B2 L2 F2  (moves 33)
U1 R3 F3 D3 R3 B2 L3 D1 F2 U1 R3 L2 U1 B2 D1 L2 U3 R2 U1 L2 U3 F2 U3 F2 U2 B2 L2 D2 L2 U2 R2 F2  (moves 32)
F1 R1 F1 D3 B3 F3 U1 R2 F2 U1 L3 U1 F2 U2 R2 U1 R2 U3 L2 U3 D2 L2 F2 L2 F2 U2 B2 U2 L2 F2  (moves 30)
F3 R3 L2 B3 U1 B2 U1 F2 U2 B2 R3 D2 L3 B2 U1 F2 U3 L2 U1 B2 D3 L2 U1 L2 U2 L2 D2 R2 F2 D2 F2 U2 R2 U2  (moves 34)
D1 B3 D2 L1 F3 U3 F2 D2 L3 F2 L3 D3 L3 D1 L2 B2 U3 R2 U1 R2 U3 L2 U3 L2 D2 R2 D2 F2 R2 F2 L2 D2 U2 F2 U2  (moves 35)
U1 B1 D1 R3 F3 U1 B2 R1 U3 L1 D3 B2 U1 R3 U2 F2 L2 U3 R2 U1 B2 D3 B2 U1 L2 F2 U2 D2 L2 U2 B2 R2 B2 L2 D2 L2  (moves 36)
U3 F1 L1 F3 B2 U1 B2 R2 D1 R1 U2 L3 U1 F2 R2 U3 L2 U3 B2 F2 R2 F2 B2 U2 R2 F2 L2 U2 L2  (moves 29)
U1 B2 L1 B3 F3 U1 F2 B2 R2 D3 R3 U3 L3 B2 U1 F2 U3 F2 U3 L2 U2 F2 R2 B2 R2 U2 F2 U2 F2 U2 F2  (moves 31)
U1 B1 U3 D1 F3 U2 D3 R3 D1 U1 L1 F2 L3 D2 L2 U1 F2 B2 R2 U3 F2 U1 L2 U2 R2 F2 D2 F2 U2 L2 B2 U2 F2  (moves 33)
D1 F3 U2 R1 B3 L1 B2 R1 U3 L2 D3 F2 R3 L2 D2 L2 D3 L2 F2 U3 F2 B2 D2 L2 B2 L2 B2 D2 L2 F2 U2 F2 U2  (moves 33)
R1 B1 D3 F3 D1 R3 D3 B2 L2 B2 U2 L3 L2 U2 B2 L2 U2 F2 U3 U2 F2 L2 D2 L2 B2 U2 R2 F2 L2 B2  (moves 30)
U1 D2 F1 L1 B3 D1 B2 D3 L2 R1 D3 L2 U2 R3 D2 F2 U3 F2 R2 U1 B2 U1 F2 U3 B2 R2 U2 L2 U2 B2 D2 F2 L2 B2 F2 U2  (moves 36)
U1 L1 D1 L1 F3 L1 U1 L3 U3 L1 D1 R3 F2 U2 R2 U1 F2 U2 F2 L2 D1 F2 U3 F2 U2 R2 D2 F2 R2 B2 L2 B2 D2 B2  (moves 34)
L3 D3 U1 F3 U2 F2 D3 R3 B2 D3 U3 L3 R2 U2 L2 D3 R2 U1 L2 U2 F2 U3 F2 U3 U2 L2 U2 R2 B2 R2 B2 U2 F2 U2  (moves 34)
L1 B1 U2 D1 F3 B2 U3 R3 D2 U3 L2 U3 L3 F2 U3 R2 U1 F2 R2 L2 D3 F2 F2 B2 L2 D2 F2 R2 D2 B2 R2 F2 L2  (moves 33)
L1 B1 U3 F3 U1 L2 D3 L1 B2 R1 L3 U3 L3 U3 L2 D3 L2 U1 L2 B2 F2 U3 L2 D2 L2 U2 R2 F2 D2 B2 U2 R2 U2  (moves 33)
F1 U1 B3 F3 B2 U2 D3 L3 U3 L3 U3 R2 D3 B2 D2 L2 U3 R2 U2 F2 U3 F2 L2 D2 L2 R2 F2 L2 U2 L2 D2 U2 F2 U2  (moves 34)
D1 L3 R1 F2 U2 F3 U3 F2 D3 L3 D2 L3 U2 L3 D1 L2 U3 R2 D1 B2 U1 L2 R2 U2 L2 D2 B2 R2 F2 L2 D2 U2 F2 U2  (moves 34)
L2 R3 D1 F3 D3 L1 U3 L3 D2 L3 B2 L2 U1 R2 U2 F2 U3 F2 L2 F2 L2 B2 U2 R2 U2 R2 B2 U2 B2  (moves 29)
L2 U2 R1 B3 F3 U1 L1 D3 F2 U2 R3 U2 L3 L2 U2 L2 D2 L2 F2 U3 F2 U2 L2 U2 B2 U2 R2 D2 R2 B2  (moves 30)
L2 F1 U2 F3 D3 R3 U1 D3 L3 D1 U1 L3 R2 D3 R2 F2 D2 B2 U3 F2 U1 F2 D2 L2 B2 U2 L2 U2 R2 B2 F2 U2  (moves 32)
F3 D2 F3 L2 B2 D3 L2 U3 B2 L3 L2 F2 L2 D3 L2 B2 U3 L2 L2 U2 F2 D2 L2 D2 L2 B2 L2 F2 L2 U2  (moves 30)
F3 R2 U3 F3 U3 R3 D2 R2 D1 F2 U2 L3 D3 L2 D3 R2 U1 F2 U3 L2 U1 F2 L2 B2 D2 B2 L2 F2 U2 L2 F2 D2  (moves 32)
B1 R1 U1 D3 F3 R2 U1 L2 R2 D2 B2 U3 L3 R2 F2 U1 L2 B2 U1 F2 U3 L2 U3 R2 U2 F2 D2 F2 L2 B2 L2 U2 F2  (moves 33)
L3 D3 U1 B3 L2 F2 D3 L3 R1 U1 R3 L2 D3 F2 R2 U3 F2 R2 U3 L2 D2 R2 U2 B2 U2 B2 L2 U2 R2 F2 U2  (moves 31)
L1 F3 L2 R1 B3 L1 U3 L1 R1 D3 F2 D1 R3 B2 R2 U1 F2 L2 D1 R2 F2 D2 L2 R2 B2 R2 B2 U2 L2 U2  (moves 30)
U1 F1 U3 L1 B3 D1 L2 B2 R1 D3 R3 F2 L3 U3 F2 B2 L2 U3 B2 D3 L2 U3 L2 D2 R2 F2 D2 L2 F2 D2 F2  (moves 31)
D2 L3 U1 F3 U1 D3 L1 F2 D3 R3 F2 U3 R3 L2 U1 L2 U3 L2 U1 F2 L2 U1 R2 U3 F2 U2 L2 D2 B2 U2 L2 F2 U2 F2 U2  (moves 35)
F3 L2 F3 U1 L1 U1 D2 L3 U3 L3 B2 U1 F2 U1 F2 D2 R2 U3 L2 U2 F2 U3 R2 F2 D2 L2 D2 R2 F2 U2 F2 U2 F2 U2  (moves 34)
U2 L3 B3 U2 R3 U3 L2 B2 U1 R3 R2 D3 R2 U1 R2 U3 F2 U1 F2 U3 F2 U2 L2 F2 R2 F2 D2 L2 D2 L2 B2  (moves 31)
F1 B1 R3 F3 U3 F3 B2 D1 B2 R1 U3 L1 U2 L3 F2 U3 F2 R2 U1 R2 U2 B2 D3 F2 U3 F2 B2 L2 U2 F2 L2 D2 B2 F2 L2 B2  (moves 36)
B2 D3 B3 U2 B2 D3 L3 D1 R1 U2 L3 L2 U3 B2 U1 B2 D2 R2 U3 F2 U3 U2 L2 U2 F2 D2 F2 U2 B2 L2 D2 F2 R2 F2  (moves 34)

```


```txt


Average number of moves = 16.72

Average time = 127 milliseconds

```


When run with a file containing the single line:

UL DL RF UB FD BR DB UF DR UR BL FL FDR BLU DLB URB RUF FLD BRD FUL

a typical result was:

```txt

U3 F1 L1 F3 B2 U1 B2 R2 D1 R1 U2 L3 U1 F2 R2 U3 L2 U3 B2 F2 R2 F2 B2 U2 R2 F2 L2 U2 L2  (moves 29)

Average number of moves = 29.0

Average time = 522 milliseconds

```



## Phix


### cfop

Uses brute-force (width/highscore-first) Fridrich-steps (ie cross,f2l,oll,pll).

Not the fastest (see THRESHOLD) or shortest results (see thistlethwaite) but the code is pretty easy to follow.

The final stage (pll) would probably benefit the most from being replaced with standard algorithms.

```Phix
--
-- demo\rosetta\rubik_cfop.exw
--
-- Each stage uses a workspace of moves tried so far, ranked by score.
-- We repeatedly take the best scoring so far and try more moves, storing
-- those results in a second/new workspace. The THRESHOLD value below
-- determines the minimum number we should examine before discarding a
-- workspace and switching to the new (one move longer) one. We only ever
-- switch on change of score, and obviously the first workspace is empty,
-- and the next new workspace has a maximum of 12 entries (+/-90 by 6), 
-- both of which will force earlier switches.
--
constant THRESHOLD = 100000 -- 100000 -- very slow (100s), best results
                            --  10000 -- slow (10s), reasonable results
                            --   1000 -- fast (1s), fairly poor results
                            --    100 -- (counter-productive/slower)

string init ="""
_____________---YYY--------
             ---YYY--------
             ---YYY--------
             BBBRRRGGGOOO--
             BBBRRRGGGOOO--
             BBBRRRGGGOOO--
             ------WWW-----
             ------WWW-----
             ------WWW-----
             
             """
-- numbering:
--  1..15:   ---456--------\n
--  16..30:  ---901--------\n   -- U
--  31..45:  ---456--------\n
--  46..60:  678901234567--\n
--  61..75:  123456789012--\n   -- LFRB
--  76..90:  678901234567--\n
--  91..105: ------789-----\n
--  106..120:------234-----\n   -- D
--  121..136:------789-----\n\n

if length(init)!=136 then ?9/0 end if

--
-- TIP: Wrap a cube with blank paper, and write
--      the numbers on it, to derive these sets.
--
constant centres = {20,62,65,68,71,113}

constant edges = {{  4,  5,  6,57,56,55},   -- ie YYY/OOO
                  {  6, 21, 36,54,53,52},   --    YYY/GGG
                  { 34, 35, 36,49,50,51},   --    YYY/RRR
                  {  4, 19, 34,46,47,48},   --    YYY/BBB
                  { 51, 66, 81,52,67,82},   --    RRR/GGG
                  { 54, 69, 84,55,70,85},   --    GGG/OOO
                  { 57, 72, 87,46,61,76},   --    OOO/BBB
                  { 48, 63, 78,49,64,79},   --    BBB/RRR
                  { 97, 98, 99,82,83,84},   --    WWW/GGG
                  { 99,114,129,85,86,87},   --    WWW/OOO
                  {127,128,129,78,77,76},   --    WWW/BBB
                  { 97,112,127,81,80,79}}   --    WWW/RRR

constant corners = {{ 4, 57,46},{34,48, 49},{36,51,52},{ 6,54,55},
                --   YOB/UBL     YBR/UFL     YRG/UFR    YGO/UBL
                    {76,129,87},{78,79,127},{81,82,97},{84,85,99}}
                --   BWO/DBL     BRW/DFL     RGW/DFR    GOW/DFL

constant facing_corners = {-16,-14,16,14}, -- (nb not 14,16)
         facing_edges   = {-15,  1,15,-1},
         fce = facing_corners&facing_edges,
         rotations = {
                      -- up (clockwise):
                      {{57,54,51,48},   -- clockwise corners
                       {46,55,52,49},   -- anticlockwise corners
                       {47,56,53,50}},  -- middle edges
                      -- left
                      {{ 4,49,127, 87},
                       {57,34, 79,129},
                       {19,64,128, 72}},
                      -- front
                      {{34,52, 97, 78},
                       {48,36, 82,127},
                       {35,67,112, 63}},
                      -- right
                      {{36,55,99,81},
                       {51, 6,85,97},
                       {21,70,98,66}},
                      -- back
                      {{ 6,46,129,84},
                       {54, 4, 76,99},
                       { 5,61,114,69}},
                      -- down
                      {{82,85,76,79},
                       {81,84,87,78},
                       {83,86,77,80}}}

--Up/Left/Front/Right/Back/Down
enum U=1,L=2,F=3,/*R=4,*/B=5,D=6,Dbl=#08,Shift=#10
constant U2 = U+Dbl, F2 = F+Dbl, /*R2 = R+Dbl, B2 = B+Dbl,*/ D2 = D+Dbl,
         Us = U+Shift, Fs = F+Shift, Bs = B+Shift, Rs = R+Shift, Ds = D+Shift

enum CROSS,F2L,OLL,PLL

integer f2l = 0         -- (28==done)
integer edge_score = 0  -- (0..12 for f2l [as U cleared],
                        --  0..24 for oll and pll stages)

function score(string cube, integer stage)
integer res = 0, c, cc, k
    f2l = 0
    for i=1 to length(centres) do
        c = centres[i]
        cc = cube[c]
        for j=1 to length(fce) do -- (the 8 next to c)
            k = c+fce[j]
            if cube[k]=cc then
                res += 1
                f2l += (stage>CROSS and k>=61)
            end if
        end for
    end for
    -- give extra credit for edges paired with corners
    edge_score = 0  -- += (0|1|2) for the 12 edges:
    if stage>CROSS then
        for i=1 to length(edges) do
            sequence ei = edges[i]  -- as 123
            --                      --    456
            -- then if {1,4}=={2,5} then edge_score += 1, 
            -- plus if {2,5}=={3,6} then edge_score += 1.
            edge_score += (cube[ei[1]]=cube[ei[2]] and
                           cube[ei[4]]=cube[ei[5]]) +
                          (cube[ei[2]]=cube[ei[3]] and
                           cube[ei[5]]=cube[ei[6]])
        end for
    end if
    return res
end function

function oll_score(string cube)
-- (should only be invoked if f2l==28)
integer res = 0     -- (true if res=8)
integer cu = centres[U]
    if cube[cu]!='Y' then ?9/0 end if
    for i=1 to length(fce) do
        integer fcei = fce[i]
        res += (cube[cu+fcei]='Y')
    end for
    return res
end function

function rotate_face(string cube, integer face)
--
-- face is 1..6 for clockwise (ULFRBD), 
-- plus #08(Dbl) for a 180 (clockwise),
-- plus #10(Shift) for anti-clockwise.
--
    integer dbl = 1+(and_bits(face,Dbl)=Dbl)
    bool cw = 1-floor(face/Shift)
    face = remainder(face,Dbl)
    integer cf = centres[face]
    sequence rf = {sq_add(facing_corners,cf),
                   sq_add(facing_edges,cf)}
                  &rotations[face]
    for d=1 to dbl do
        for i=1 to length(rf) do
            sequence rfi = rf[i]
            if cw then rfi = reverse(rfi) end if
            integer rfi1 = cube[rfi[1]]
            for j=1 to 3 do
                cube[rfi[j]] = cube[rfi[j+1]]
            end for
            cube[rfi[4]] = rfi1
        end for
    end for
    return cube
end function

function apply_moves(string cube, sequence moves)
    for i=1 to length(moves) do
        cube = rotate_face(cube,moves[i])
    end for
    return cube
end function

constant ULFRBD = "ULFRBD"

function moves_to_string(sequence moves)
-- convert eg {1,20,11} to "UR'F2"
string res = ""
integer l = length(moves)
    for i=1 to l do
        integer face = moves[i]
        integer dbl = and_bits(face,Dbl)=Dbl
        bool anticw = floor(face/Shift)
        face = remainder(face,Dbl)
        res &= ULFRBD[face]
        if dbl then
            res &= '2'
        elsif anticw then
            res &= '\''
        end if
    end for
    res &=sprintf("  (%d move%s)     ",{l,iff(l=1?"":"s")})
    return res
end function
        
--
-- The seen dictionary.
--  Without this, since it uses a breadth/highscore-first
--  algorithm, after f2l (for instance) it would probably
--  just do U and U' as the new high scores, forever.
--  (The THRESHOLD constant mitigates that to some extent)
--
integer seen = new_dict()

function solve_stage(string cube, integer stage)
atom t1 = time()+1
string moves = "", moves2
sequence workspace, w2,
         init
integer wslen, high = 1,
        s, c2c = 0, o = 0
bool done

    if stage=CROSS then
        --
        -- first, blank out all corners, and   
        -- all edges without a white on them.
        --
        for i=1 to length(rotations) do
            for j=1 to 2 do -- (just corners)
                for k=1 to 4 do
                    cube[rotations[i][j][k]]='-'
                end for
            end for
        end for
        for i=1 to length(edges) do
            integer {?,m1,?,?,m2,?} = edges[i]
            if cube[m1]!='W'
            and cube[m2]!='W' then
                cube[m1] = '-'
                cube[m2] = '-'
            end if
        end for
        wslen = 8
        s = score(cube,CROSS)
        done = (s=8)
    elsif stage=F2L then
        --
        -- first, blank out all pieces with a yellow
        --
        for i=1 to length(corners) do
            integer {c1,c2,c3} = corners[i]
            if cube[c1]='Y'
            or cube[c2]='Y'
            or cube[c3]='Y' then
                cube[c1] = '-'
                cube[c2] = '-'
                cube[c3] = '-'
            end if
        end for
        for i=1 to length(edges) do
            integer {?,m1,?,?,m2,?} = edges[i]
            if cube[m1]='Y'
            and cube[m2]='Y' then
                cube[m1] = '-'
                cube[m2] = '-'
            end if
        end for
        wslen = 57+12
        s = score(cube,F2L)
        done = (f2l=28)
    else
        wslen = 77+24
        s = score(cube,stage)
        if f2l!=28 then ?9/0 end if
        if stage=OLL then
            done = (oll_score(cube)=8)
        else -- (stage=PLL)
            done = (s=48)
        end if
    end if
    if not done then
        workspace = repeat({},wslen)
        w2 = workspace
        init = cube
        workspace[high] = {""}
        destroy_dict(seen,justclear:=1)
        integer move_count = 1
        while 1 do
            if workspace[high]={} then
                while high and workspace[high]={} do high -= 1 end while
                if high=0 or (stage!=CROSS and c2c>THRESHOLD) then
                    move_count += 1
                    workspace = w2
                    w2 = repeat({},wslen)
                    c2c = 0
                    high = wslen
                    while workspace[high]={} do high -= 1 end while
                end if
            end if
            moves = workspace[high][1]
            workspace[high] = workspace[high][2..$]
            cube = apply_moves(init,moves)
            for face=U to D do
                -- (originally this loop did 180s as well, but that
                --  gave them far too much dominance, esp during pll.
                --  instead we now coalese those that survive a 90.)
                for m=0 to Shift by Shift do
                    integer mi = face+m
                    sequence cube2 = rotate_face(cube,mi)
                    if getd_index(cube2,seen)=0 then
                        putd(cube2,0,seen)
                        s = score(cube2,stage)
                        if stage=CROSS then
                            done = (s=8)
                        elsif stage=F2L then
                            done = (f2l=28)
                        else
                            if f2l=28 then
                                o = oll_score(cube2)
                            else
                                o = 0
                            end if
                            if stage=OLL then
                                done = (o=8)
                            else
                                done = (s=48)
                            end if
                        end if
                        moves2 = moves
                        if length(moves2) and moves2[$]=mi then
                            moves2[$] = face+Dbl
                        else
                            moves2 &= mi
                        end if
                        if done then
                            destroy_dict(seen,justclear:=1)
                            return moves2
                        end if
                        s += 1+edge_score*2+o
                        w2[s] = append(w2[s],moves2)
                        c2c += 1
                    end if
                end for
            end for
            if time()>t1 then
                printf(1,"working... %d moves, %d positions\r",{move_count,dict_size(seen)})
                t1 = time()+1
                if get_key()=#1B then exit end if
            end if
        end while   
    end if
    return ""   -- (already solved case)
end function
        
constant stage_desc = { "make cross",
                        "solve first two layers",
                        "orientate last layer",
                        "permute last layer" }

procedure main()
string cube
sequence moves
integer total_moves = 0
atom t0 = time()

    -- "hardest case" from http://www.cube20.org
    moves = {F, Us, F2, Ds, B, U, Rs, Fs, L, Ds, 
             Rs, Us, L, U, Bs, D2, Rs, F, U2, D2}
    cube = apply_moves(init,moves)
    if length(moves)<=20 then
        printf(1,"scramble: %s\n",{moves_to_string(moves)})
    end if

    puts(1,substitute(cube,"-"," "))

    for stage=CROSS to PLL do
        moves = solve_stage(cube, stage)
        total_moves += length(moves)
        cube = apply_moves(cube,moves)
        printf(1,"%s: %s\n",{stage_desc[stage],moves_to_string(moves)})
        if length(moves) then
            puts(1,substitute(cube,"-"," "))
        end if
    end for
    printf(1,"\nsolution of %d total moves found in %3.2fs\n",{total_moves,time()-t0})
end procedure
main()
```

The "hardest case" from http://www.cube20.org with a high threshold. You can try this manually.
Disclaimer: the results are not always quite as good as this!

```txt

scramble: FU'F2D'BUR'F'LD'R'U'LUB'D2R'FU2D2  (20 moves)
   ROB
   BYG
   GRO
YYOYYBYYRYYG
RBOBRGOGRGOB
WWOWWBWWRWWG
      OGB
      RWO
      GBR

make cross: DLBRFL  (6 moves)
   BRB
   YYG
   YYY
ROBRBRGOYOGW
OBRBRYRGYGOB
RBYORGOGOBOW
      WWW
      WWW
      GWG

solve first two layers: FUL'R'FLRF'LRB'R'U'BU'B'U'B  (18 moves)
   RYG
   OYR
   YYY
BYRGGOBYOYBY
BBBRRRGGGOOO
BBBRRRGGGOOO
      WWW
      WWW
      WWW

orientate last layer: R'F'U'FUR  (6 moves)
   YYY
   YYY
   YYY
GGBRRGOOOBBR
BBBRRRGGGOOO
BBBRRRGGGOOO
      WWW
      WWW
      WWW

permute last layer: RU'L'UR'U2LU'L'U2LU'  (12 moves)
   YYY      
   YYY      
   YYY      
BBBRRRGGGOOO
BBBRRRGGGOOO
BBBRRRGGGOOO
      WWW
      WWW
      WWW


solution of 42 total moves found in 81.33s

```


### thistlethwaite

Translation/de-golf(hrumph) of Tomas Sirgedas' winning entry from http://tomas.rokicki.com/cubecontest as held in 2004.

Faster and shorter solutions (in most cases) than cfop, however probably nigh on impossible to debug/enhance...

```Phix
--
-- demo\rosetta\rubik_tomas.exw
--
function xor_string(string s)
    return xor_bits(s[1],xor_bits(s[2],iff(length(s)=3?s[3]:'!')))
end function

function xor_all(sequence s)
    for i=1 to length(s) do
        s[i] = xor_string(s[i])
    end for
    return s
end function

constant d1 = xor_all(split("UF DF UB DB UR DR UL DL FR FL BR BL UFR DBR UBL DFL DLB ULF DRF URB"))
-- This is Mike Reid's notation, 12 sides then 8 corners, which may be rotated - hence we xor the
-- characters for fast lookup. The above string represents a cube in the solved state.

constant d2 = {18,12,17,15,0, 9,1,8,16,14,19,13,2,10,3,11,12,18,13,19,4,8,5,10,
               14,16,15,17,6,11,7,9,17,12,19,14,6, 0,4, 2,18,15,16,13,1,7,3, 5}
--?sort(d2): (0..11 appear twice, 12..19 appear thrice - edges/corners is pretty much all I can say)

constant d3 = {13,16,15,1,3,
               19,18,17,4,6}
-- these apppear to be swapped during initialisation, dunno why...

integer cur_phase, search_mode, history_idx
sequence history_mov = repeat(0,48),
         history_rpt = repeat(0,48),
         depth_to_go,
         hash_table = repeat(repeat(6,6912),48)
         -- (hash_table can/should be preserved for different problems)

sequence cubelet_pos = repeat(0,48),
         cubelet_twi = repeat(0,48)

procedure rot(integer cur_phase)
    if cur_phase<4 then
        for i=0 to 3 do
            integer di = cur_phase*8+i+1,
                    j = d2[di]+1,
                    k = d2[di+4]+1
            cubelet_twi[j] = mod(cubelet_twi[j]+2-mod(i,2),3)
            cubelet_twi[k] = xor_bits(cubelet_twi[k],cur_phase<2)
        end for
    end if
    
    for i=0 to 6 do
        integer di = cur_phase*8+i+1,
                j = d2[di+(i!=3)]+1,
                k = d2[di]+1
        -- swap(cubelet[j]], cubelet[k]);
        {cubelet_pos[j],cubelet_pos[k]} = {cubelet_pos[k],cubelet_pos[j]}
        {cubelet_twi[j],cubelet_twi[k]} = {cubelet_twi[k],cubelet_twi[j]}
    end for
end procedure

function hashf()
    int ret = 0;
    switch cur_phase do
        case 0:
                for i=0 to 10 do
                    ret += ret + cubelet_twi[i+1]
                end for
                return ret;
        case 1:
                for i=0 to 6 do
                    ret = ret*3 + cubelet_twi[i+12+1]
                end for
                for i=0 to 10 do
                    ret += ret + (cubelet_pos[i+1]>7)
                end for
                return ret-7;
        case 2:
                sequence inva = repeat(0,48),
                         b = repeat(0,48)
                for i=0 to 7 do
                    integer ci12p = cubelet_pos[i+12+1], 
                            ci12p3 = and_bits(ci12p,3)
                    if ci12p<16 then
                        inva[ci12p3+1] = ret
                        ret += 1
                    else
                        b[i-ret+1] = ci12p3 
                    end if
                end for
                for i=0 to 6 do
                    ret += ret + (cubelet_pos[i+1]>3);
                end for
                for i=0 to 6 do
                    ret += ret + (cubelet_pos[i+12+1]>15);
                end for
                integer ib2 = xor_bits(inva[b[1]+1],inva[b[2]+1])*2,
                        ib3 = xor_bits(inva[b[1]+1],inva[b[3]+1]),
                        ib4 = xor_bits(inva[b[1]+1],inva[b[4]+1])
                return ret*54 + ib2 + (ib3 > ib4) - 3587708
    end switch
    for i=0 to 4 do
        ret *= 24;
        for cp=0 to 3 do
            for k=0 to cp-1 do
                if cubelet_pos[i*4+cp+1] < cubelet_pos[i*4+k+1] then
                    ret += cp + iff(cp=3?cp:0)
                end if
            end for
        end for
    end for
    return floor(ret/2)
end function

function do_search(integer dpt)
    integer h = hashf(), 
            q = (floor(cur_phase/2)*19+8)*power(2,7),
            hmq = mod(h,q)+1,
            hfq = floor(h/q)+1,
            d = (dpt < hash_table[cur_phase+1][hmq] or 
                 dpt < hash_table[cur_phase+4+1][hfq])

    if d xor search_mode then
        if search_mode then
            if dpt <= depth_to_go[h+1] then
                return not h;
            else
                depth_to_go[h+1] = dpt;
            end if
        end if

        hash_table[cur_phase+1][hmq] = min(hash_table[cur_phase+1][hmq],dpt);
        hash_table[cur_phase+5][hfq] = min(hash_table[cur_phase+5][hfq],dpt);
        
        for k=0 to 5 do
            for i=0 to 3 do
                rot(k)
                if (k>=cur_phase*2 or i=1) and i<=2 then
                    history_idx += 1
                    history_mov[history_idx] = k
                    history_rpt[history_idx] = i
                    if do_search(dpt-search_mode*2+1) then return 1 end if
                    history_idx -= 1
                end if
            end for
        end for
    end if
    return 0
end function

function pack_moves()
string moves = ""
integer n = 0, this, last, last_rpt
    if history_idx!=0 then
        -- add a dummy move to trigger the last move print:
        last = xor_bits(history_mov[history_idx],1) -- F<->B, etc
        history_idx += 1
        history_mov[history_idx] = last
        history_rpt[history_idx] = 0
        last = history_mov[1]
        last_rpt = 0
        for i=1 to history_idx do
            this = history_mov[i]
            if this!=last then
                -- coalesce eg F1F2 to F' (unless you wanna fix do_search()!)
                if last_rpt then
                    moves &= "FBRLUD"[last+1] & {"","2","'"}[last_rpt]
                    n += 1
                end if
                last = this
                last_rpt = history_rpt[i]+1
            else
                last_rpt = mod(last_rpt+history_rpt[i]+1,4)
            end if
        end for
    end if
    return {moves,n,iff(n=1?"":"s")}
end function

function tomas(sequence args)
    search_mode = 0
    history_idx = 0
    depth_to_go = repeat(0,5*power(2,20))

    for i=0 to 19 do
        cubelet_pos[i+1] = i
    end for
    for i=0 to 3 do
        cur_phase = i
        {} = do_search(0)
    end for
    args = split(args)
    for i=0 to 19 do
        string s = args[i+1]    -- (may be rotated, eg RU or UR)
        integer p = find(xor_string(s),d1)
        if p=0 then ?9/0 end if -- sensible message(bad args)?
        cubelet_pos[i+1] = p-1
        int x = max(find('U',s), find('D',s));
        cubelet_twi[i+1] = iff(x!=0 ? x-1 : s[1]>'F')
    end for
    for i=0 to 4 do
        integer j = d3[i+1]+1,
                k = d3[i+6]+1
        -- swap(cubelet[j], cubelet[k]);        
        {cubelet_pos[j],cubelet_pos[k]} = {cubelet_pos[k],cubelet_pos[j]}
        {cubelet_twi[j],cubelet_twi[k]} = {cubelet_twi[k],cubelet_twi[j]}
    end for
    search_mode = 1;
    for cp=0 to 3 do
        cur_phase = cp
        for i=0 to 19 do
            if do_search(i) then exit end if
        end for
    end for
    return pack_moves()
end function

printf(1,"%s (%d move%s)\n",tomas("UL DL RF UB FD BR DB UF DR UR BL FL FDR BLU DLB URB RUF FLD BRD FUL"))
```

```txt

UF'R'FB2R2B2LD2L2DLR2U'F2UF2U2F2L2UF2DF2U2R2U2R2B2D2R2F2L2B2D2 (35 moves)

```

The distributed copy of demo\rosetta\rubik_cfop.exw also contains routines to convert between my 136-character cube and reid notation,
and demo\rosetta\rubik_tomas.exw also contains the full 100-long test set from the original competition.
