+++
title = "Tetris/Java"
description = ""
date = 2017-09-29T13:51:04Z
aliases = []
[extra]
id = 20660
[taxonomies]
categories = []
tags = []
+++

{{collection|Tetris}}
==Code==
{{works with|java|8}}

```java
package tetris;

import java.awt.*;
import java.awt.event.*;
import static java.lang.Math.*;
import static java.lang.String.format;
import java.util.*;
import javax.swing.*;
import static tetris.Config.*;

public class Tetris extends JPanel implements Runnable {
    enum Dir {
        right(1, 0), down(0, 1), left(-1, 0);

        Dir(int x, int y) {
            this.x = x;
            this.y = y;
        }
        final int x, y;
    };

    public static final int EMPTY = -1;
    public static final int BORDER = -2;

    Shape fallingShape;
    Shape nextShape;

    // position of falling shape
    int fallingShapeRow;
    int fallingShapeCol;

    final int[][] grid = new int[nRows][nCols];

    Thread fallingThread;
    final Scoreboard scoreboard = new Scoreboard();
    static final Random rand = new Random();

    public Tetris() {
        setPreferredSize(dim);
        setBackground(bgColor);
        setFocusable(true);

        initGrid();
        selectShape();

        addMouseListener(new MouseAdapter() {
            @Override
            public void mousePressed(MouseEvent e) {
                if (scoreboard.isGameOver()) {
                    startNewGame();
                    repaint();
                }
            }
        });

        addKeyListener(new KeyAdapter() {
            boolean fastDown;

            @Override
            public void keyPressed(KeyEvent e) {

                if (scoreboard.isGameOver())
                    return;

                switch (e.getKeyCode()) {

                    case KeyEvent.VK_UP:
                        if (canRotate(fallingShape))
                            rotate(fallingShape);
                        break;

                    case KeyEvent.VK_LEFT:
                        if (canMove(fallingShape, Dir.left))
                            move(Dir.left);
                        break;

                    case KeyEvent.VK_RIGHT:
                        if (canMove(fallingShape, Dir.right))
                            move(Dir.right);
                        break;

                    case KeyEvent.VK_DOWN:
                        if (!fastDown) {
                            fastDown = true;
                            while (canMove(fallingShape, Dir.down)) {
                                move(Dir.down);
                                repaint();
                            }
                            shapeHasLanded();
                        }
                }
                repaint();
            }

            @Override
            public void keyReleased(KeyEvent e) {
                fastDown = false;
            }
        });
    }

    void selectShape() {
        fallingShapeRow = 1;
        fallingShapeCol = 5;
        fallingShape = nextShape;
        Shape[] shapes = Shape.values();
        nextShape = shapes[rand.nextInt(shapes.length)];
        if (fallingShape != null)
            fallingShape.reset();
    }

    void startNewGame() {
        stop();
        initGrid();
        selectShape();
        scoreboard.reset();
        (fallingThread = new Thread(this)).start();
    }

    void stop() {
        if (fallingThread != null) {
            Thread tmp = fallingThread;
            fallingThread = null;
            tmp.interrupt();
        }
    }

    void initGrid() {
        for (int r = 0; r < nRows; r++) {
            Arrays.fill(grid[r], EMPTY);
            for (int c = 0; c < nCols; c++) {
                if (c == 0 || c == nCols - 1 || r == nRows - 1)
                    grid[r][c] = BORDER;
            }
        }
    }

    @Override
    public void run() {

        while (Thread.currentThread() == fallingThread) {

            try {
                Thread.sleep(scoreboard.getSpeed());
            } catch (InterruptedException e) {
                return;
            }

            if (!scoreboard.isGameOver()) {
                if (canMove(fallingShape, Dir.down)) {
                    move(Dir.down);
                } else {
                    shapeHasLanded();
                }
                repaint();
            }
        }
    }

    void drawStartScreen(Graphics2D g) {
        g.setFont(mainFont);

        g.setColor(titlebgColor);
        g.fill(titleRect);
        g.fill(clickRect);

        g.setColor(textColor);
        g.drawString("Tetris", titleX, titleY);

        g.setFont(smallFont);
        g.drawString("click to start", clickX, clickY);
    }

    void drawSquare(Graphics2D g, int colorIndex, int r, int c) {
        g.setColor(colors[colorIndex]);
        g.fillRect(leftMargin + c * blockSize, topMargin + r * blockSize,
                blockSize, blockSize);

        g.setStroke(smallStroke);
        g.setColor(squareBorder);
        g.drawRect(leftMargin + c * blockSize, topMargin + r * blockSize,
                blockSize, blockSize);
    }

    void drawUI(Graphics2D g) {
        // grid background
        g.setColor(gridColor);
        g.fill(gridRect);

        // the blocks dropped in the grid
        for (int r = 0; r < nRows; r++) {
            for (int c = 0; c < nCols; c++) {
                int idx = grid[r][c];
                if (idx > EMPTY)
                    drawSquare(g, idx, r, c);
            }
        }

        // the borders of grid and preview panel
        g.setStroke(largeStroke);
        g.setColor(gridBorderColor);
        g.draw(gridRect);
        g.draw(previewRect);

        // scoreboard
        int x = scoreX;
        int y = scoreY;
        g.setColor(textColor);
        g.setFont(smallFont);
        g.drawString(format("hiscore  %6d", scoreboard.getTopscore()), x, y);
        g.drawString(format("level    %6d", scoreboard.getLevel()), x, y + 30);
        g.drawString(format("lines    %6d", scoreboard.getLines()), x, y + 60);
        g.drawString(format("score    %6d", scoreboard.getScore()), x, y + 90);

        // preview
        int minX = 5, minY = 5, maxX = 0, maxY = 0;
        for (int[] p : nextShape.pos) {
            minX = min(minX, p[0]);
            minY = min(minY, p[1]);
            maxX = max(maxX, p[0]);
            maxY = max(maxY, p[1]);
        }
        double cx = previewCenterX - ((minX + maxX + 1) / 2.0 * blockSize);
        double cy = previewCenterY - ((minY + maxY + 1) / 2.0 * blockSize);

        g.translate(cx, cy);
        for (int[] p : nextShape.shape)
            drawSquare(g, nextShape.ordinal(), p[1], p[0]);
        g.translate(-cx, -cy);
    }

    void drawFallingShape(Graphics2D g) {
        int idx = fallingShape.ordinal();
        for (int[] p : fallingShape.pos)
            drawSquare(g, idx, fallingShapeRow + p[1], fallingShapeCol + p[0]);
    }

    @Override
    public void paintComponent(Graphics gg) {
        super.paintComponent(gg);
        Graphics2D g = (Graphics2D) gg;
        g.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
                RenderingHints.VALUE_ANTIALIAS_ON);

        drawUI(g);

        if (scoreboard.isGameOver()) {
            drawStartScreen(g);
        } else {
            drawFallingShape(g);
        }
    }

    boolean canRotate(Shape s) {
        if (s == Shape.Square)
            return false;

        int[][] pos = new int[4][2];
        for (int i = 0; i < pos.length; i++) {
            pos[i] = s.pos[i].clone();
        }

        for (int[] row : pos) {
            int tmp = row[0];
            row[0] = row[1];
            row[1] = -tmp;
        }

        for (int[] p : pos) {
            int newCol = fallingShapeCol + p[0];
            int newRow = fallingShapeRow + p[1];
            if (grid[newRow][newCol] != EMPTY) {
                return false;
            }
        }
        return true;
    }

    void rotate(Shape s) {
        if (s == Shape.Square)
            return;

        for (int[] row : s.pos) {
            int tmp = row[0];
            row[0] = row[1];
            row[1] = -tmp;
        }
    }

    void move(Dir dir) {
        fallingShapeRow += dir.y;
        fallingShapeCol += dir.x;
    }

    boolean canMove(Shape s, Dir dir) {
        for (int[] p : s.pos) {
            int newCol = fallingShapeCol + dir.x + p[0];
            int newRow = fallingShapeRow + dir.y + p[1];
            if (grid[newRow][newCol] != EMPTY)
                return false;
        }
        return true;
    }

    void shapeHasLanded() {
        addShape(fallingShape);
        if (fallingShapeRow < 2) {
            scoreboard.setGameOver();
            scoreboard.setTopscore();
            stop();
        } else {
            scoreboard.addLines(removeLines());
        }
        selectShape();
    }

    int removeLines() {
        int count = 0;
        for (int r = 0; r < nRows - 1; r++) {
            for (int c = 1; c < nCols - 1; c++) {
                if (grid[r][c] == EMPTY)
                    break;
                if (c == nCols - 2) {
                    count++;
                    removeLine(r);
                }
            }
        }
        return count;
    }

    void removeLine(int line) {
        for (int c = 0; c < nCols; c++)
            grid[line][c] = EMPTY;

        for (int c = 0; c < nCols; c++) {
            for (int r = line; r > 0; r--)
                grid[r][c] = grid[r - 1][c];
        }
    }

    void addShape(Shape s) {
        for (int[] p : s.pos)
            grid[fallingShapeRow + p[1]][fallingShapeCol + p[0]] = s.ordinal();
    }

    public static void main(String[] args) {
        SwingUtilities.invokeLater(() -> {
            JFrame f = new JFrame();
            f.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
            f.setTitle("Tetris");
            f.setResizable(false);
            f.add(new Tetris(), BorderLayout.CENTER);
            f.pack();
            f.setLocationRelativeTo(null);
            f.setVisible(true);
        });
    }
}
```



```java
package tetris;

class Scoreboard {
    static final int MAXLEVEL = 9;

    private int level;
    private int lines;
    private int score;
    private int topscore;
    private boolean gameOver = true;

    void reset() {
        setTopscore();
        level = lines = score = 0;
        gameOver = false;
    }

    void setGameOver() {
        gameOver = true;
    }

    boolean isGameOver() {
        return gameOver;
    }

    void setTopscore() {
        if (score > topscore)
            topscore = score;
    }

    int getTopscore() {
        return topscore;
    }

    int getSpeed() {

        switch (level) {
            case 0:
                return 700;
            case 1:
                return 600;
            case 2:
                return 500;
            case 3:
                return 400;
            case 4:
                return 350;
            case 5:
                return 300;
            case 6:
                return 250;
            case 7:
                return 200;
            case 8:
                return 150;
            case 9:
                return 100;
            default:
                return 100;
        }
    }

    void addScore(int sc) {
        score += sc;
    }

    void addLines(int line) {

        switch (line) {
            case 1:
                addScore(10);
                break;
            case 2:
                addScore(20);
                break;
            case 3:
                addScore(30);
                break;
            case 4:
                addScore(40);
                break;
            default:
                return;
        }

        lines += line;
        if (lines > 10)
            addLevel();
    }

    void addLevel() {
        lines %= 10;
        if (level < MAXLEVEL)
            level++;
    }

    int getLevel() {
        return level;
    }

    int getLines() {
        return lines;
    }

    int getScore() {
        return score;
    }
}
```



```java
package tetris;

enum Shape {
    ZShape(new int[][]{{0, -1}, {0, 0}, {-1, 0}, {-1, 1}}),
    SShape(new int[][]{{0, -1}, {0, 0}, {1, 0}, {1, 1}}),
    IShape(new int[][]{{0, -1}, {0, 0}, {0, 1}, {0, 2}}),
    TShape(new int[][]{{-1, 0}, {0, 0}, {1, 0}, {0, 1}}),
    Square(new int[][]{{0, 0}, {1, 0}, {0, 1}, {1, 1}}),
    LShape(new int[][]{{-1, -1}, {0, -1}, {0, 0}, {0, 1}}),
    JShape(new int[][]{{1, -1}, {0, -1}, {0, 0}, {0, 1}});

    private Shape(int[][] shape) {
        this.shape = shape;
        pos = new int[4][2];
        reset();
    }

    void reset() {
        for (int i = 0; i < pos.length; i++) {
            pos[i] = shape[i].clone();
        }
    }

    final int[][] pos, shape;
}
```



```java
package tetris;

import java.awt.*;

final class Config {
    final static Color[] colors = {Color.green, Color.red, Color.blue,
        Color.pink, Color.orange, Color.cyan, Color.magenta};

    final static Font mainFont = new Font("Monospaced", Font.BOLD, 48);
    final static Font smallFont = mainFont.deriveFont(Font.BOLD, 18);

    final static Dimension dim = new Dimension(640, 640);

    final static Rectangle gridRect = new Rectangle(46, 47, 308, 517);
    final static Rectangle previewRect = new Rectangle(387, 47, 200, 200);
    final static Rectangle titleRect = new Rectangle(100, 85, 252, 100);
    final static Rectangle clickRect = new Rectangle(50, 375, 252, 40);

    final static int blockSize = 30;
    final static int nRows = 18;
    final static int nCols = 12;
    final static int topMargin = 50;
    final static int leftMargin = 20;
    final static int scoreX = 400;
    final static int scoreY = 330;
    final static int titleX = 130;
    final static int titleY = 150;
    final static int clickX = 120;
    final static int clickY = 400;
    final static int previewCenterX = 467;
    final static int previewCenterY = 97;

    final static Stroke largeStroke = new BasicStroke(5);
    final static Stroke smallStroke = new BasicStroke(2);

    final static Color squareBorder = Color.white;
    final static Color titlebgColor = Color.white;
    final static Color textColor = Color.black;
    final static Color bgColor = new Color(0xDDEEFF);
    final static Color gridColor = new Color(0xBECFEA);
    final static Color gridBorderColor = new Color(0x7788AA);
}
```

