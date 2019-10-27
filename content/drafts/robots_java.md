+++
title = "Robots/Java"
description = ""
date = 2017-10-30T08:08:02Z
aliases = []
[extra]
id = 21648
[taxonomies]
categories = []
tags = []
+++

==Code==
{{works with|Java|8}}

```java
import java.awt.*;
import static java.awt.BasicStroke.*;
import java.awt.event.*;
import static java.lang.Math.abs;
import static java.lang.String.format;
import java.util.Random;
import javax.swing.*;

public class Robots extends JPanel {
    enum Grid {
        Player("@"), Robot("+"), Scrap("*"), Mark("~");

        Grid(String s) {
            symbol = s;
        }
        final String symbol;
    }

    final static int[][] dirs = {{-1, 1}, {0, 1}, {1, 1}, {-1, 0}, {1, 0},
    {-1, -1}, {0, -1}, {1, -1}};

    final static Random rand = new Random();

    final int nRows;
    final int nCols;

    Grid[][] grid;
    int playerRow, playerCol, score, hiScore, level;
    boolean gameOver = true;
    Stroke dash;

    public Robots() {
        setPreferredSize(new Dimension(800, 650));
        setBackground(Color.white);
        setForeground(Color.lightGray);
        setFont(new Font("SansSerif", Font.PLAIN, 18));
        setFocusable(true);

        nRows = 38;
        nCols = 50;

        dash = new BasicStroke(2.0f, CAP_BUTT, JOIN_MITER, 10.0f,
                new float[]{5.0f}, 0.0f);

        addMouseListener(new MouseAdapter() {
            @Override
            public void mousePressed(MouseEvent e) {
                if (gameOver) {
                    startNewGame();
                    repaint();
                }
            }
        });

        addKeyListener(new KeyAdapter() {
            @Override
            public void keyPressed(KeyEvent e) {
                if (gameOver) return;  // disable keystrokes until game starts
                int keyCode = e.getKeyCode();

                if (keyCode == KeyEvent.VK_NUMPAD5) {
                    teleport();

                } else {
                    int k = keyCode - KeyEvent.VK_NUMPAD1;
                    if (k >= 0 && k < 9) {
                        move(k > 4 ? --k : k);
                    }
                }
                repaint();
            }
        });
    }

    void startNewGame() {
        level = 1;
        if (score > hiScore)
            hiScore = score;
        score = 0;
        initGrid();
        gameOver = false;
    }

    void initGrid() {
        grid = new Grid[nRows][nCols];

        teleport();

        int numRobots = 7 * level;
        for (int i = 0; i < numRobots;) {
            int r = rand.nextInt(nRows);
            int c = rand.nextInt(nCols);
            if (grid[r][c] == null) {
                grid[r][c] = Grid.Robot;
                i++;
            }
        }
    }

    boolean movePlayer(int r, int c) {
        if (grid[r][c] != null) {
            gameOver = true;
        } else {
            grid[playerRow][playerCol] = null;
            playerRow = r;
            playerCol = c;
            grid[r][c] = Grid.Player;
        }
        return !gameOver;
    }

    void move(int d) {
        int c = playerCol + dirs[d][0];
        int r = playerRow + dirs[d][1];

        if (!withinBounds(r, c))
            return;

        if (!movePlayer(r, c))
            return;

        for (int rr = 0; rr < nRows; rr++)
            for (int cc = 0; cc < nCols; cc++) {
                if (grid[rr][cc] == Grid.Robot) {

                    // calc new r and c based on dx + cc and dy + rr
                    int nc = (c == cc ? 0 : (c - cc) / abs(c - cc)) + cc;
                    int nr = (r == rr ? 0 : (r - rr) / abs(r - rr)) + rr;

                    if (!withinBounds(nr, nc))
                        continue;

                    grid[rr][cc] = null;

                    if (grid[nr][nc] == Grid.Player) {
                        gameOver = true;
                        return; /* EARLY RETURN */

                    } else if (grid[nr][nc] != null) {
                        score++;
                        if (grid[nr][nc] != Grid.Scrap)
                            score++;
                        grid[nr][nc] = Grid.Scrap;

                    } else {
                        // avoid processing the same robot twice
                        grid[nr][nc] = Grid.Mark;
                    }
                }
            }

        int robotsLeft = 0;
        for (int rr = 0; rr < nRows; rr++)
            for (int cc = 0; cc < nCols; cc++) {
                if (grid[rr][cc] == Grid.Mark)
                    grid[rr][cc] = Grid.Robot;
                if (grid[rr][cc] == Grid.Robot)
                    robotsLeft++;
            }

        if (robotsLeft == 0) {
            level++;
            initGrid();
        }
    }

    void teleport() {
        movePlayer(rand.nextInt(nRows), rand.nextInt(nCols));
    }

    void drawBorder(Graphics2D g) {
        g.setStroke(dash);
        g.setColor(getForeground());
        g.drawRect(22, 20, getWidth() - 41, getHeight() - 72);
    }

    void drawGrid(Graphics2D g) {
        for (int r = 0; r < nRows; r++)
            for (int c = 0; c < nCols; c++) {
                if (grid[r][c] != null)
                    g.drawString(grid[r][c].symbol, 24 + c * 15, 36 + r * 15);
            }
    }

    void drawStartScreen(Graphics2D g) {
        g.setColor(Color.gray);
        g.setFont(new Font("SansSerif", Font.BOLD, 48));
        g.drawString("robots", 315, 280);

        g.setFont(getFont());
        g.drawString("(use numpad to move player)", 270, 350);
        g.drawString("(teleport is numpad 5)", 300, 380);
        g.drawString("(click to start)", 328, 410);
    }

    void drawScore(Graphics2D g) {
        g.setColor(Color.gray);
        g.setFont(getFont());
        String s = format("hiscore   %s    score   %s", hiScore, score);
        g.drawString(s, 30, getHeight() - 17);
    }

    boolean withinBounds(int r, int c) {
        return c >= 0 && c < nCols && r >= 0 && r < nRows;
    }

    @Override
    public void paintComponent(Graphics gg) {
        super.paintComponent(gg);
        Graphics2D g = (Graphics2D) gg;
        g.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
                RenderingHints.VALUE_ANTIALIAS_ON);

        drawBorder(g);
        drawScore(g);
        if (gameOver) {
            drawStartScreen(g);
        } else {
            drawGrid(g);
        }
    }

    public static void main(String[] args) {
        SwingUtilities.invokeLater(() -> {
            JFrame f = new JFrame();
            f.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
            f.setTitle("Robots");
            f.setResizable(false);
            f.add(new Robots(), BorderLayout.CENTER);
            f.pack();
            f.setLocationRelativeTo(null);
            f.setVisible(true);
        });
    }
}
```

