+++
title = "Robots/Kotlin"
description = ""
date = 2017-10-30T13:59:59Z
aliases = []
[extra]
id = 21650
[taxonomies]
categories = []
tags = []
+++

==Code==
{{trans|Java}}

```scala
// version 1.1.51

import java.util.Random
import java.awt.*
import java.awt.BasicStroke.*
import java.awt.event.*
import javax.swing.JFrame
import javax.swing.JPanel
import javax.swing.SwingUtilities

val rand = Random()

val dirs = listOf(
    -1 to 1, 0 to 1, 1 to 1, -1 to 0, 1 to 0, -1 to -1, 0 to -1, 1 to -1
)

class Robots : JPanel() {

    val nRows = 38
    val nCols = 50
    val dash = BasicStroke(2.0f, CAP_BUTT, JOIN_MITER, 10.0f, floatArrayOf(5.0f), 0.0f)

    var playerRow = 0
    var playerCol = 0
    var score = 0
    var hiScore = 0
    var level = 0
    var gameOver = true

    enum class Grid(val symbol: String) {
       Player("@"), Robot("+"), Scrap("*"), Mark("~")
    }

    lateinit var grid: Array<Array<Grid?>>

    init {
        preferredSize = Dimension(800, 650)
        background = Color.white
        foreground = Color.lightGray
        font = Font("SansSerif", Font.PLAIN, 18)
        isFocusable = true

        addMouseListener(object : MouseAdapter() {
            override fun mousePressed(e: MouseEvent) {
                if (gameOver) {
                    startNewGame()
                    repaint()
                }
            }
        })

        addKeyListener(object : KeyAdapter() {
            override fun keyPressed(e: KeyEvent) {
                if (gameOver) return  // disable keystrokes until game starts
                val keyCode = e.keyCode
                if (keyCode == KeyEvent.VK_NUMPAD5) {
                    teleport()
                }
                else {
                    var k = keyCode - KeyEvent.VK_NUMPAD1
                    if (k in 0..8) move(if (k > 4) --k else k)
                }
                repaint()
            }
        })
    }

    fun startNewGame() {
        level = 1
        if (score > hiScore) hiScore = score
        score = 0 
        initGrid()
        gameOver = false
    }

    fun initGrid() {
        grid = Array(nRows) { arrayOfNulls<Grid>(nCols) }
        teleport()
        val numRobots = 7 * level
        var i = 0
        while (i < numRobots) {
            val r = rand.nextInt(nRows)
            val c = rand.nextInt(nCols)
            if (grid[r][c] == null) {
                grid[r][c] = Grid.Robot
                i++
            }
        }
    }

    fun movePlayer(r: Int, c: Int): Boolean {
        if (grid[r][c] != null) {
            gameOver = true
        }
        else {
            grid[playerRow][playerCol] = null
            playerRow = r
            playerCol = c
            grid[r][c] = Grid.Player
        }
        return !gameOver
    }

    fun move(d: Int) {
        val c = playerCol + dirs[d].first
        val r = playerRow + dirs[d].second

        if (!withinBounds(r, c)) return
        if (!movePlayer(r, c)) return

        for (rr in 0 until nRows) {
            for (cc in 0 until nCols) {
                if (grid[rr][cc] == Grid.Robot) {
                    // calc new r and c based on dx + cc and dy + rr
                    val nc = (if (c == cc) 0 else (c - cc) / Math.abs(c - cc)) + cc
                    val nr = (if (r == rr) 0 else (r - rr) / Math.abs(r - rr)) + rr
                    if (!withinBounds(nr, nc)) continue
                    grid[rr][cc] = null

                    if (grid[nr][nc] == Grid.Player) {
                        gameOver = true
                        return /* EARLY RETURN */
                    }
                    else if (grid[nr][nc] != null) {
                        score++
                        if (grid[nr][nc] != Grid.Scrap) score++
                        grid[nr][nc] = Grid.Scrap
                    }
                    else {
                        // avoid processing the same robot twice
                        grid[nr][nc] = Grid.Mark
                    }
                }
            }
        }

        var robotsLeft = 0
        for (rr in 0 until nRows) {
            for (cc in 0 until nCols) {
                if (grid[rr][cc] == Grid.Mark) grid[rr][cc] = Grid.Robot
                if (grid[rr][cc] == Grid.Robot) robotsLeft++
            }
        }
        if (robotsLeft == 0) {
            level++
            initGrid()
        }
    }

    fun teleport() {
        movePlayer(rand.nextInt(nRows), rand.nextInt(nCols))
    }

    fun drawBorder(g: Graphics2D) {
        g.stroke = dash
        g.color = foreground
        g.drawRect(22, 20, width - 41, height - 72)
    }

    fun drawGrid(g: Graphics2D) {
        for (r in 0 until nRows) {
            for (c in 0 until nCols) {
                if (grid[r][c] != null)
                    g.drawString(grid[r][c]!!.symbol, 24 + c * 15, 36 + r * 15)
            }
        }
    }

    fun drawStartScreen(g: Graphics2D) {
        g.color = Color.gray
        g.font  = Font("SansSerif", Font.BOLD, 48)
        g.drawString("robots", 315, 280)

        g.font = this.font
        g.drawString("(use numpad to move player)", 270, 350)
        g.drawString("(teleport is numpad 5)", 300, 380)
        g.drawString("(click to start)", 328, 410)
    }

    fun drawScore(g: Graphics2D) {
         g.color = Color.gray
         g.font  = this.font
         val s = String.format("hiscore   %s    score   %s", hiScore, score)
         g.drawString(s, 30, height - 17)
    }

    fun withinBounds(r: Int, c: Int) = (c in 0 until nCols) && (r in 0 until nRows)

    override fun paintComponent(gg: Graphics) {
        super.paintComponent(gg)
        val g =  gg as Graphics2D
        g.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
                           RenderingHints.VALUE_ANTIALIAS_ON)
        drawBorder(g)
        drawScore(g)
        if (gameOver) drawStartScreen(g)
        else drawGrid(g)
    }
}

fun main(args: Array<String>) {
    SwingUtilities.invokeLater {
        val f = JFrame()
        with (f) {
            defaultCloseOperation = JFrame.EXIT_ON_CLOSE
            title = "Robots"
            isResizable = false
            add(Robots(), BorderLayout.CENTER)
            pack()
            setLocationRelativeTo(null)
            isVisible = true
        }
    }
}
```

