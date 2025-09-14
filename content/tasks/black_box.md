+++
title = "Black Box"
description = ""
date = 2019-10-09T20:39:11Z
aliases = []
[extra]
id = 21511
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "go",
  "javascript",
]
+++

Implement a version of the [https://en.wikipedia.org/wiki/Black_Box_(game) Black Box game] beginners configuration: 4 Atoms in an 8 x 8 grid.

Determine where the hidden atoms are in the box, by observing how the light beams fired into the box react when leaving it.<br />
Possible results:<br />
'H': the beam hit an atom and stopped<br />
'R': Either the beam was reflected back the way it came or there was a ball just to one side of its entry point<br />
'Numbers': indicate that the beam entered one of those squares and emerged from the other


Extra credit (Different game types):<br />
-More or less atoms (maybe random)<br />
-Different grid sizes



## Go

Terminal based game.

Just the basic configuration - 4 atoms in an 8 x 8 grid.

To test it against known output (as opposed to playing a sensible game), the program has been fixed (wikiGame = true) to reproduce the atom position in the Wikipedia article's example game, followed by a complete set of beams and one incorrect and three correct guesses.

Set wikiGame to false to play a normal 'random' game. 

```go
package main

import (
    "bufio"
    "fmt"
    "log"
    "math/rand"
    "os"
    "strings"
    "time"
)

var (
    b        = make([]rune, 100) // displayed board
    h        = make([]rune, 100) // hidden atoms
    scanner  = bufio.NewScanner(os.Stdin)
    wikiGame = true // set to false for a 'random' game
)

func initialize() {
    for i := 0; i < 100; i++ {
        b[i] = ' '
        h[i] = 'F'
    }
    if !wikiGame {
        hideAtoms()
    } else {
        h[32] = 'T'
        h[37] = 'T'
        h[64] = 'T'
        h[87] = 'T'
    }
    fmt.Println(`
    
###  BLACK BOX 


    H    Hit (scores 1)
    R    Reflection (scores 1)
    1-9, Detour (scores 2)
    a-c  Detour for 10-12 (scores 2)
    G    Guess (maximum 4)
    Y    Correct guess
    N    Incorrect guess (scores 5)
    A    Unguessed atom
  
    Cells are numbered a0 to j9.
    Corner cells do nothing.
    Use edge cells to fire beam.
    Use middle cells to add/delete a guess.
    Game ends automatically after 4 guesses.
    Enter q to abort game at any time.
    `)
}

func drawGrid(score, guesses int) {
    fmt.Printf("      0   1   2   3   4   5   6   7   8   9 \n")
    fmt.Printf("\n")
    fmt.Printf("        ╔═══╦═══╦═══╦═══╦═══╦═══╦═══╦═══╗\n")
    fmt.Printf("a     %c ║ %c ║ %c ║ %c ║ %c ║ %c ║ %c ║ %c ║ %c ║ %c\n",
        b[0], b[1], b[2], b[3], b[4], b[5], b[6], b[7], b[8], b[9])
    fmt.Printf("    ╔═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╗\n")
    fmt.Printf("b   ║ %c ║ %c ║ %c ║ %c ║ %c ║ %c ║ %c ║ %c ║ %c ║ %c ║\n",
        b[10], b[11], b[12], b[13], b[14], b[15], b[16], b[17], b[18], b[19])
    fmt.Printf("    ╠═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╣\n")
    fmt.Printf("c   ║ %c ║ %c ║ %c ║ %c ║ %c ║ %c ║ %c ║ %c ║ %c ║ %c ║\n",
        b[20], b[21], b[22], b[23], b[24], b[25], b[26], b[27], b[28], b[29])
    fmt.Printf("    ╠═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╣\n")
    fmt.Printf("d   ║ %c ║ %c ║ %c ║ %c ║ %c ║ %c ║ %c ║ %c ║ %c ║ %c ║\n",
        b[30], b[31], b[32], b[33], b[34], b[35], b[36], b[37], b[38], b[39])
    fmt.Printf("    ╠═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╣\n")
    fmt.Printf("e   ║ %c ║ %c ║ %c ║ %c ║ %c ║ %c ║ %c ║ %c ║ %c ║ %c ║\n",
        b[40], b[41], b[42], b[43], b[44], b[45], b[46], b[47], b[48], b[49])
    fmt.Printf("    ╠═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╣\n")
    fmt.Printf("f   ║ %c ║ %c ║ %c ║ %c ║ %c ║ %c ║ %c ║ %c ║ %c ║ %c ║\n",
        b[50], b[51], b[52], b[53], b[54], b[55], b[56], b[57], b[58], b[59])
    fmt.Printf("    ╠═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╣\n")
    fmt.Printf("g   ║ %c ║ %c ║ %c ║ %c ║ %c ║ %c ║ %c ║ %c ║ %c ║ %c ║\n",
        b[60], b[61], b[62], b[63], b[64], b[65], b[66], b[67], b[68], b[69])
    fmt.Printf("    ╠═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╣\n")
    fmt.Printf("h   ║ %c ║ %c ║ %c ║ %c ║ %c ║ %c ║ %c ║ %c ║ %c ║ %c ║\n",
        b[70], b[71], b[72], b[73], b[74], b[75], b[76], b[77], b[78], b[79])
    fmt.Printf("    ╠═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╣\n")
    fmt.Printf("i   ║ %c ║ %c ║ %c ║ %c ║ %c ║ %c ║ %c ║ %c ║ %c ║ %c ║\n",
        b[80], b[81], b[82], b[83], b[84], b[85], b[86], b[87], b[88], b[89])
    fmt.Printf("    ╚═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╝\n")
    fmt.Printf("j     %c ║ %c ║ %c ║ %c ║ %c ║ %c ║ %c ║ %c ║ %c ║ %c\n",
        b[90], b[91], b[92], b[93], b[94], b[95], b[96], b[97], b[98], b[99])
    fmt.Printf("        ╚═══╩═══╩═══╩═══╩═══╩═══╩═══╩═══╝\n")
    status := "In play"
    if guesses == 4 {
        status = "Game over!"
    }
    fmt.Println("\n        Score =", score, "\tGuesses =", guesses, "\t Status =", status, "\n")
}

func hideAtoms() {
    placed := 0
    for placed < 4 {
        a := 11 + rand.Intn(78) // 11 to 88 inclusive
        m := a % 10
        if m == 0 || m == 9 || h[a] == 'T' {
            continue
        }
        h[a] = 'T'
        placed++
    }
}

func nextCell() int {
    var ix int
    for {
        fmt.Print("    Choose cell : ")
        scanner.Scan()
        sq := strings.ToLower(scanner.Text())
        if len(sq) == 1 && sq[0] == 'q' {
            log.Fatal("program aborted")
        }
        if len(sq) != 2 || sq[0] < 'a' || sq[0] > 'j' || sq[1] < '0' || sq[1] > '9' {
            continue
        }
        ix = int((sq[0]-'a')*10 + sq[1] - 48)
        if atCorner(ix) {
            continue
        }
        break
    }
    check(scanner.Err())
    fmt.Println()
    return ix
}

func atCorner(ix int) bool { return ix == 0 || ix == 9 || ix == 90 || ix == 99 }

func inRange(ix int) bool { return ix >= 1 && ix <= 98 && ix != 9 && ix != 90 }

func atTop(ix int) bool { return ix >= 1 && ix <= 8 }

func atBottom(ix int) bool { return ix >= 91 && ix <= 98 }

func atLeft(ix int) bool { return inRange(ix) && ix%10 == 0 }

func atRight(ix int) bool { return inRange(ix) && ix%10 == 9 }

func inMiddle(ix int) bool {
    return inRange(ix) && !atTop(ix) && !atBottom(ix) && !atLeft(ix) && !atRight(ix)
}

func play() {
    score, guesses := 0, 0
    num := '0'
outer:
    for {
        drawGrid(score, guesses)
        ix := nextCell()
        if !inMiddle(ix) && b[ix] != ' ' { // already processed
            continue
        }
        var inc, def int
        switch {
        case atTop(ix):
            inc, def = 10, 1
        case atBottom(ix):
            inc, def = -10, 1
        case atLeft(ix):
            inc, def = 1, 10
        case atRight(ix):
            inc, def = -1, 10
        default:
            if b[ix] != 'G' {
                b[ix] = 'G'
                guesses++
                if guesses == 4 {
                    break outer
                }
            } else {
                b[ix] = ' '
                guesses--
            }
            continue
        }
        var x int
        first := true
        for x = ix + inc; inMiddle(x); x += inc {
            if h[x] == 'T' { // hit
                b[ix] = 'H'
                score++
                first = false
                continue outer
            }
            if first && (inMiddle(x+def) && h[x+def] == 'T') ||
                (inMiddle(x-def) && h[x-def] == 'T') { // reflection
                b[ix] = 'R'
                score++
                first = false
                continue outer
            }
            first = false
            y := x + inc - def
            if inMiddle(y) && h[y] == 'T' { // deflection
                switch inc {
                case 1, -1:
                    inc, def = 10, 1
                case 10, -10:
                    inc, def = 1, 10
                }
            }
            y = x + inc + def
            if inMiddle(y) && h[y] == 'T' { // deflection or double deflection
                switch inc {
                case 1, -1:
                    inc, def = -10, 1
                case 10, -10:
                    inc, def = -1, 10
                }
            }
        }
        if num != '9' {
            num++
        } else {
            num = 'a'
        }
        if b[ix] == ' ' {
            score++
        }
        b[ix] = num
        if inRange(x) {
            if ix == x {
                b[ix] = 'R'
            } else {
                if b[x] == ' ' {
                    score++
                }
                b[x] = num
            }
        }
    }
    drawGrid(score, guesses)
    finalScore(score, guesses)
}

func check(err error) {
    if err != nil {
        log.Fatal(err)
    }
}

func finalScore(score, guesses int) {
    for i := 11; i <= 88; i++ {
        m := i % 10
        if m == 0 || m == 9 {
            continue
        }
        if b[i] == 'G' && h[i] == 'T' {
            b[i] = 'Y'
        } else if b[i] == 'G' && h[i] == 'F' {
            b[i] = 'N'
            score += 5
        } else if b[i] == ' ' && h[i] == 'T' {
            b[i] = 'A'
        }
    }
    drawGrid(score, guesses)
}

func main() {
    rand.Seed(time.Now().UnixNano())
    for {
        initialize()
        play()
    inner:
        for {
            fmt.Print("    Play again y/n : ")
            scanner.Scan()
            yn := strings.ToLower(scanner.Text())
            switch yn {
            case "n":
                return
            case "y":
                break inner
            }
        }
        check(scanner.Err())
    }
}
```


As the grid is displayed 29 times in all, this has been abbreviated to show just the first 2 and the last 3.

```txt

    
###  BLACK BOX 


    H    Hit (scores 1)
    R    Reflection (scores 1)
    1-9, Detour (scores 2)
    a-c  Detour for 10-12 (scores 2)
    G    Guess (maximum 4)
    Y    Correct guess
    N    Incorrect guess (scores 5)
    A    Unguessed atom
  
    Cells are numbered a0 to j9.
    Corner cells do nothing.
    Use edge cells to fire beam.
    Use middle cells to add/delete a guess.
    Game ends automatically after 4 guesses.
    Enter q to abort game at any time.
    
      0   1   2   3   4   5   6   7   8   9 

        ╔═══╦═══╦═══╦═══╦═══╦═══╦═══╦═══╗
a       ║   ║   ║   ║   ║   ║   ║   ║   ║  
    ╔═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╗
b   ║   ║   ║   ║   ║   ║   ║   ║   ║   ║   ║
    ╠═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╣
c   ║   ║   ║   ║   ║   ║   ║   ║   ║   ║   ║
    ╠═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╣
d   ║   ║   ║   ║   ║   ║   ║   ║   ║   ║   ║
    ╠═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╣
e   ║   ║   ║   ║   ║   ║   ║   ║   ║   ║   ║
    ╠═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╣
f   ║   ║   ║   ║   ║   ║   ║   ║   ║   ║   ║
    ╠═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╣
g   ║   ║   ║   ║   ║   ║   ║   ║   ║   ║   ║
    ╠═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╣
h   ║   ║   ║   ║   ║   ║   ║   ║   ║   ║   ║
    ╠═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╣
i   ║   ║   ║   ║   ║   ║   ║   ║   ║   ║   ║
    ╚═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╝
j       ║   ║   ║   ║   ║   ║   ║   ║   ║  
        ╚═══╩═══╩═══╩═══╩═══╩═══╩═══╩═══╝

        Score = 0 	Guesses = 0 	 Status = In play 

    Choose cell : b0

      0   1   2   3   4   5   6   7   8   9 

        ╔═══╦═══╦═══╦═══╦═══╦═══╦═══╦═══╗
a       ║   ║   ║   ║   ║   ║   ║   ║   ║  
    ╔═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╗
b   ║ 1 ║   ║   ║   ║   ║   ║   ║   ║   ║ 1 ║
    ╠═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╣
c   ║   ║   ║   ║   ║   ║   ║   ║   ║   ║   ║
    ╠═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╣
d   ║   ║   ║   ║   ║   ║   ║   ║   ║   ║   ║
    ╠═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╣
e   ║   ║   ║   ║   ║   ║   ║   ║   ║   ║   ║
    ╠═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╣
f   ║   ║   ║   ║   ║   ║   ║   ║   ║   ║   ║
    ╠═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╣
g   ║   ║   ║   ║   ║   ║   ║   ║   ║   ║   ║
    ╠═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╣
h   ║   ║   ║   ║   ║   ║   ║   ║   ║   ║   ║
    ╠═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╣
i   ║   ║   ║   ║   ║   ║   ║   ║   ║   ║   ║
    ╚═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╝
j       ║   ║   ║   ║   ║   ║   ║   ║   ║  
        ╚═══╩═══╩═══╩═══╩═══╩═══╩═══╩═══╝

        Score = 2 	Guesses = 0 	 Status = In play 

    Choose cell : c0

................ (Screens 3 to 26 omitted) ................

        Score = 32 	Guesses = 2 	 Status = In play 

    Choose cell : g4

      0   1   2   3   4   5   6   7   8   9 

        ╔═══╦═══╦═══╦═══╦═══╦═══╦═══╦═══╗
a       ║ 2 ║ H ║ 9 ║ H ║ 7 ║ 9 ║ H ║ 8 ║  
    ╔═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╗
b   ║ 1 ║   ║   ║   ║   ║   ║   ║   ║   ║ 1 ║
    ╠═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╣
c   ║ 2 ║   ║   ║   ║   ║   ║   ║   ║   ║ 8 ║
    ╠═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╣
d   ║ H ║ G ║   ║   ║   ║   ║   ║ G ║   ║ H ║
    ╠═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╣
e   ║ 3 ║   ║   ║   ║   ║   ║   ║   ║   ║ 6 ║
    ╠═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╣
f   ║ 4 ║   ║   ║   ║   ║   ║   ║   ║   ║ 7 ║
    ╠═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╣
g   ║ H ║   ║   ║   ║ G ║   ║   ║   ║   ║ H ║
    ╠═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╣
h   ║ 5 ║   ║   ║   ║   ║   ║   ║   ║   ║ 6 ║
    ╠═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╣
i   ║ H ║   ║   ║   ║   ║   ║   ║   ║   ║ H ║
    ╚═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╝
j       ║ 3 ║ H ║ 5 ║ H ║ 4 ║ R ║ H ║ R ║  
        ╚═══╩═══╩═══╩═══╩═══╩═══╩═══╩═══╝

        Score = 32 	Guesses = 3 	 Status = In play 

    Choose cell : i7

      0   1   2   3   4   5   6   7   8   9 

        ╔═══╦═══╦═══╦═══╦═══╦═══╦═══╦═══╗
a       ║ 2 ║ H ║ 9 ║ H ║ 7 ║ 9 ║ H ║ 8 ║  
    ╔═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╗
b   ║ 1 ║   ║   ║   ║   ║   ║   ║   ║   ║ 1 ║
    ╠═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╣
c   ║ 2 ║   ║   ║   ║   ║   ║   ║   ║   ║ 8 ║
    ╠═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╣
d   ║ H ║ G ║   ║   ║   ║   ║   ║ G ║   ║ H ║
    ╠═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╣
e   ║ 3 ║   ║   ║   ║   ║   ║   ║   ║   ║ 6 ║
    ╠═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╣
f   ║ 4 ║   ║   ║   ║   ║   ║   ║   ║   ║ 7 ║
    ╠═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╣
g   ║ H ║   ║   ║   ║ G ║   ║   ║   ║   ║ H ║
    ╠═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╣
h   ║ 5 ║   ║   ║   ║   ║   ║   ║   ║   ║ 6 ║
    ╠═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╣
i   ║ H ║   ║   ║   ║   ║   ║   ║ G ║   ║ H ║
    ╚═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╝
j       ║ 3 ║ H ║ 5 ║ H ║ 4 ║ R ║ H ║ R ║  
        ╚═══╩═══╩═══╩═══╩═══╩═══╩═══╩═══╝

        Score = 32 	Guesses = 4 	 Status = Game over! 

      0   1   2   3   4   5   6   7   8   9 

        ╔═══╦═══╦═══╦═══╦═══╦═══╦═══╦═══╗
a       ║ 2 ║ H ║ 9 ║ H ║ 7 ║ 9 ║ H ║ 8 ║  
    ╔═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╗
b   ║ 1 ║   ║   ║   ║   ║   ║   ║   ║   ║ 1 ║
    ╠═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╣
c   ║ 2 ║   ║   ║   ║   ║   ║   ║   ║   ║ 8 ║
    ╠═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╣
d   ║ H ║ N ║ A ║   ║   ║   ║   ║ Y ║   ║ H ║
    ╠═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╣
e   ║ 3 ║   ║   ║   ║   ║   ║   ║   ║   ║ 6 ║
    ╠═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╣
f   ║ 4 ║   ║   ║   ║   ║   ║   ║   ║   ║ 7 ║
    ╠═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╣
g   ║ H ║   ║   ║   ║ Y ║   ║   ║   ║   ║ H ║
    ╠═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╣
h   ║ 5 ║   ║   ║   ║   ║   ║   ║   ║   ║ 6 ║
    ╠═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╣
i   ║ H ║   ║   ║   ║   ║   ║   ║ Y ║   ║ H ║
    ╚═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╝
j       ║ 3 ║ H ║ 5 ║ H ║ 4 ║ R ║ H ║ R ║  
        ╚═══╩═══╩═══╩═══╩═══╩═══╩═══╩═══╝

        Score = 37 	Guesses = 4 	 Status = Game over! 

    Play again y/n : n

```




## JavaScript

Play it [http://paulo-jorente.de/tests/blackbox/ here].

```javascript

var sel, again, check, score, done, atoms, guesses, beamCnt, brdSize;

function updateScore( s ) {
    score += s || 0;
    para.innerHTML = "Score: " + score;
}
function checkIt() {
    check.className = "hide";
    again.className = "again";
    done = true;
    var b, id;
    for( var j = 0; j < brdSize; j++ ) {
        for( var i = 0; i < brdSize; i++ ) {
            if( board[i][j].H ) {
                b = document.getElementById( "atom" + ( i + j * brdSize ) );
                b.innerHTML = "&#x2688;";
                if( board[i][j].T ) {
                    b.style.color = "#0a2";
                } else {
                    b.style.color = "#f00";
                     updateScore( 5 );
                }
            } 
        }
    }
}
function isValid( n ) {
    return n > -1 && n < brdSize;
}
function stepBeam( sx, sy, dx, dy ) {
    var s = brdSize - 2
    if( dx ) {
        if( board[sx][sy].H ) return {r:"H", x:sx, y:sy};
        if( ( (sx == 1 && dx == 1) || (sx == s && dx == -1) ) && ( ( sy > 0 && board[sx][sy - 1].H ) || 
            ( sy < s && board[sx][sy + 1].H ) ) ) return {r:"R", x:sx, y:sy};
        if( isValid( sx + dx ) ) {
            if( isValid( sy - 1 ) && board[sx + dx][sy - 1].H ) {
                dx = 0; dy = 1;
            }
            if( isValid( sy + 1 ) && board[sx + dx][sy + 1].H ) {
                dx = 0; dy = -1;
            }
            sx += dx;
            return stepBeam( sx, sy, dx, dy );
        } else {
            return {r:"O", x:sx, y:sy};
        }
    } else {
        if( board[sx][sy].H ) return {r:"H", x:sx, y:sy}; 
        if( ( (sy == 1 && dy == 1) || (sy == s && dy == -1) ) && ( ( sx > 0 && board[sx - 1][sy].H ) || 
           ( sx < s && board[sx + 1][sy].H ) ) ) return {r:"R", x:sx, y:sy};
        if( isValid( sy + dy ) ) {
            if( isValid( sx - 1 ) && board[sx - 1][sy + dy].H ) {
                dy = 0; dx = 1;
            }
            if( isValid( sx + 1 ) && board[sx + 1][sy + dy].H ) {
                dy = 0; dx = -1;
            }
            sy += dy;
            return stepBeam( sx, sy, dx, dy );
        } else {
            return {r:"O", x:sx, y:sy};
        }
    }
}
function fireBeam( btn ) {
    var sx = btn.i, sy = btn.j, dx = 0, dy = 0;

    if( sx == 0 || sx == brdSize - 1 ) dx = sx == 0 ? 1 : - 1;
    else if( sy == 0 || sy == brdSize - 1 ) dy = sy == 0 ? 1 : - 1;
    var s = stepBeam( sx + dx, sy + dy, dx, dy );
    switch( s.r ) {
        case "H": 
            btn.innerHTML = "H"; 
            updateScore( 1 );
            break;
        case "R":
            btn.innerHTML = "R";
            updateScore( 1 );
            break;
        case "O":
            if( s.x == sx && s.y == sy ) {
                btn.innerHTML = "R";
                updateScore( 1 );
            }
            else {
                var b = document.getElementById( "fire" + ( s.x + s.y * brdSize ) );
                btn.innerHTML = "" + beamCnt;
                b.innerHTML = "" + beamCnt;
                beamCnt++;
                updateScore( 2 );
            }
    }
}
function setAtom( btn ) {
    if( done ) return;
    
    var b = document.getElementById( "atom" + ( btn.i + btn.j * brdSize ) );
    if( board[btn.i][btn.j].T == 0 && guesses < atoms ) {
        board[btn.i][btn.j].T = 1;
        guesses++;
        b.innerHTML = "&#x2688;";
    } else if( board[btn.i][btn.j].T == 1 && guesses > 0 ) {
        board[btn.i][btn.j].T = 0;
        guesses--;
        b.innerHTML = " ";
    }
    if( guesses == atoms ) check.className = "check";
    else check.className = "hide";
}
function startGame() {
    score = 0;
    updateScore();
    check.className = again.className = "hide";
    var e = document.getElementById( "mid" );
    if( e.firstChild ) e.removeChild( e.firstChild );
    
    brdSize = sel.value;
    done = false;

    if( brdSize < 5 ) return;

    var brd = document.createElement( "div" );
    brd.id = "board";
    brd.style.height = brd.style.width = 5.2 * brdSize + "vh"
    e.appendChild( brd );
    
    var b, c, d;
    for( var j = 0; j < brdSize; j++ ) {
        for( var i = 0; i < brdSize; i++ ) {
            b = document.createElement( "button" );
            b.i = i; b.j = j;
            if( j == 0 && i == 0 || j == 0 && i == brdSize - 1 ||
                j == brdSize - 1 && i == 0 || j == brdSize - 1 && i == brdSize - 1 ) {
                b.className = "corner";
            } else {
                if( j == 0 || j == brdSize - 1 || i == 0 || i == brdSize - 1 ) {
                    b.className = "fire";
                    b.id = "fire" + ( i + j * brdSize );
                } else {
                    b.className = "atom";
                    b.id = "atom" + ( i + j * brdSize );
                }
                b.addEventListener( "click", 
                    function( e ) {
                        if( e.target.className == "fire" && e.target.innerHTML == " " ) fireBeam( e.target );
                        else if( e.target.className == "atom" ) setAtom( e.target );
                    }, false );
            }
            b.appendChild( document.createTextNode( " " ) );
            brd.appendChild( b );
        }
    }

    board = new Array( brdSize );
    for( var j = 0; j < brdSize; j++ ) {
        board[j] = new Array( brdSize );
        for( i = 0; i < brdSize; i++ ) {
            board[j][i] = {H: 0, T: 0};
        }
    }

    guesses = 0; beamCnt = 1;
    atoms = brdSize == 7 ? 3 : brdSize == 10 ? 4 : 4 + Math.floor( Math.random() * 5 );

    var s = brdSize - 2, i, j;
    for( var k = 0; k < atoms; k++ ) {
        while( true ) {
            i = 1 + Math.floor( Math.random() * s );
            j = 1 + Math.floor( Math.random() * s );
            if( board[i][j].H == 0 ) break;
        }
        board[i][j].H = 1;
    }
}
function init() {
    sel = document.createElement( "select");
    sel.options.add( new Option( "5 x 5 [3 atoms]", 7 ) );
    sel.options.add( new Option( "8 x 8 [4 atoms]", 10 ) );
    sel.options.add( new Option( "10 x 10 [4 - 8 atoms]", 12 ) );
    sel.addEventListener( "change", startGame, false );
    document.getElementById( "top" ).appendChild( sel );
    
    check = document.createElement( "button" );
    check.appendChild( document.createTextNode( "Check it!" ) );
    check.className = "hide";
    check.addEventListener( "click", checkIt, false );
    
    again = document.createElement( "button" );
    again.appendChild( document.createTextNode( "Again" ) );
    again.className = "hide";
    again.addEventListener( "click", startGame, false );
    
    para = document.createElement( "p" );
    para.className = "txt";
    var d = document.getElementById( "bot" );
    
    d.appendChild( para );
    d.appendChild( check );
    d.appendChild( again );
    startGame();
}

```

