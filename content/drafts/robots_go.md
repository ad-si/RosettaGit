+++
title = "Robots/Go"
description = ""
date = 2019-04-23T19:00:32Z
aliases = []
[extra]
id = 22290
[taxonomies]
categories = []
tags = []
+++

==Code==
{{trans|C++}}
{{libheader|termbox-go}}
{{works with|Ubuntu 16.04}}


This hasn't been tested on Windows 10 but should work.

Note that this version uses the Z key (rather than the Y key) to move diagonally downwards to the left and the H key (rather than the Z key) to wait for the end. A leave key, L, has also been added in case one wants to end the game immediately.

```go
package main

import (
    "fmt"
    "github.com/nsf/termbox-go"
    "log"
    "math/rand"
    "time"
)

type coord struct{ x, y int }

const (
    width  = 62
    height = 42
    inc    = 10
)

var (
    board       [width * height]rune
    robotsCount = 0
    score       = 0
    aliveRobots = 0
    bold        = termbox.AttrBold
    cursor      coord
    alive       bool
)

var colors = [10]termbox.Attribute{
    termbox.ColorDefault,
    termbox.ColorWhite,
    termbox.ColorBlack | bold,
    termbox.ColorBlue | bold,
    termbox.ColorGreen | bold,
    termbox.ColorCyan | bold,
    termbox.ColorRed | bold,
    termbox.ColorMagenta | bold,
    termbox.ColorYellow | bold,
    termbox.ColorWhite | bold,
}

func printAt(x, y int, s string, fg, bg termbox.Attribute) {
    for _, r := range s {
        termbox.SetCell(x, y, r, fg, bg)
        x++
    }
}

func clearBoard() {
    for y := 0; y < height; y++ {
        for x := 0; x < width; x++ {
            board[x+width*y] = 32
            if x == 0 || x == width-1 || y == 0 || y == height-1 {
                board[x+width*y] = '#'
            }
        }
    }
}

func createBoard() {
    aliveRobots = robotsCount
    for x := 0; x < robotsCount; x++ {
        var a, b int
        for {
            a = rand.Intn(width)
            b = rand.Intn(height)
            if board[a+width*b] == 32 {
                break
            }
        }
        board[a+width*b] = '+'
    }
    printScore()
}

func displayBoard() {
    var fg termbox.Attribute
    bg := colors[0]
    for y := 0; y < height; y++ {
        for x := 0; x < width; x++ {
            t := board[x+width*y]
            switch t {
            case ' ':
                fg = colors[0]
            case '#':
                fg = colors[3]
            case '+':
                fg = colors[8]
            case 'Å', '*':
                fg = colors[6]
            case '@':
                fg = colors[4]
            }
            printAt(x, y, string(t), fg, bg)
        }
    }
    termbox.Flush()
}

func teleport() {
    board[cursor.x+width*cursor.y] = 32
    cursor.x = rand.Intn(width-2) + 1
    cursor.y = rand.Intn(height-2) + 1
    x := cursor.x + width*cursor.y
    if board[x] == '*' || board[x] == '+' || board[x] == '~' {
        alive = false
        board[x] = 'Å'
    } else {
        board[x] = '@'
    }
}

func printScore() {
    fg := colors[4]
    bg := termbox.ColorGreen
    s := fmt.Sprintf("      SCORE: %d      ", score)
    printAt(0, height, s, fg, bg)
    termbox.Flush()
}

func execute(x, y int) {
    board[cursor.x+width*cursor.y] = 32
    cursor.x += x
    cursor.y += y
    board[cursor.x+width*cursor.y] = '@'
    moveRobots()
}

func waitForEnd() {
    for aliveRobots != 0 && alive {
        moveRobots()
        displayBoard()
        time.Sleep(500 * time.Millisecond)
    }
}

func moveRobots() {
    for y := 0; y < height; y++ {
        for x := 0; x < width; x++ {
            if board[x+width*y] != '+' {
                continue
            }
            tx, ty := x, y
            if tx < cursor.x {
                tx++
            } else if tx > cursor.x {
                tx--
            }
            if ty < cursor.y {
                ty++
            } else if ty > cursor.y {
                ty--
            }
            if tx != x || ty != y {
                board[x+width*y] = 32
                if board[tx+width*ty] == 32 {
                    board[tx+width*ty] = '~'
                } else {
                    checkCollision(tx, ty)
                }
            }
        }
    }
    for x := 0; x < width*height; x++ {
        if board[x] == '~' {
            board[x] = '+'
        }
    }
}

func checkCollision(x, y int) {
    if cursor.x == x && cursor.y == y {
        alive = false
        board[x+width*y] = 'Å'
        return
    }
    x += y * width
    if board[x] == '*' || board[x] == '+' || board[x] == '~' {
        if board[x] != '*' {
            aliveRobots--
            score++
        }
        board[x] = '*'
        aliveRobots--
        score++
    }
}

func check(err error) {
    if err != nil {
        log.Fatal(err)
    }
}

func main() {
    rand.Seed(time.Now().UnixNano())
    err := termbox.Init()
    check(err)
    defer termbox.Close()

    eventQueue := make(chan termbox.Event)
    go func() {
        for {
            eventQueue <- termbox.PollEvent()
        }
    }()

    for {
        termbox.HideCursor()
        robotsCount = 10
        score = 0
        alive = true
        clearBoard()
        cursor.x = rand.Intn(width-2) + 1
        cursor.y = rand.Intn(height-2) + 1
        board[cursor.x+width*cursor.y] = '@'
        createBoard()
        for {
            displayBoard()
            select {
            case ev := <-eventQueue:
                if ev.Type == termbox.EventKey {
                    switch ev.Ch {
                    case 'q', 'Q':
                        if cursor.x > 1 && cursor.y > 1 {
                            execute(-1, -1)
                        }
                    case 'w', 'W':
                        if cursor.y > 1 {
                            execute(0, -1)
                        }
                    case 'e', 'E':
                        if cursor.x < width-2 && cursor.y > 1 {
                            execute(1, -1)
                        }
                    case 'a', 'A':
                        if cursor.x > 1 {
                            execute(-1, 0)
                        }
                    case 'd', 'D':
                        if cursor.x < width-2 {
                            execute(1, 0)
                        }
                    case 'z', 'Z':
                        if cursor.x > 1 && cursor.y < height-2 {
                            execute(-1, 1)
                        }
                    case 'x', 'X':
                        if cursor.y < height-2 {
                            execute(0, 1)
                        }
                    case 'c', 'C':
                        if cursor.x < width-2 && cursor.y < height-2 {
                            execute(1, 1)
                        }
                    case 't', 'T':
                        teleport()
                        moveRobots()
                    case 'h', 'H':
                        waitForEnd()
                    case 'l', 'L': // leave key
                        return
                    }
                } else if ev.Type == termbox.EventResize {
                    termbox.Flush()
                }
            }
            printScore()
            if aliveRobots == 0 {
                robotsCount += inc
                clearBoard()
                board[cursor.x+width*cursor.y] = '@'
                createBoard()
            }
            if !alive {
                break
            }
        }
        displayBoard()
        fg := colors[7]
        bg := colors[0]
        printAt(10, 8, "+----------------------------------------+", fg, bg)
        printAt(10, 9, "|               GAME OVER                |", fg, bg)
        printAt(10, 10, "|            PLAY AGAIN(Y/N)?            |", fg, bg)
        printAt(10, 11, "+----------------------------------------+", fg, bg)
        termbox.SetCursor(39, 10)
        termbox.Flush()
        select {
        case ev := <-eventQueue:
            if ev.Type == termbox.EventKey {
                if ev.Ch == 'y' || ev.Ch == 'Y' {
                    break
                } else {
                    return
                }
            }
        }
    }
}
```

