+++
title = "Greed/Java"
description = ""
date = 2016-04-16T19:31:24Z
aliases = []
[extra]
id = 20808
[taxonomies]
categories = []
tags = []
+++

==Code==

```java
import java.awt.*;
import java.awt.event.*;
import static java.lang.String.format;
import java.util.Random;
import static java.util.stream.IntStream.range;
import javax.swing.*;

public class Greed extends JPanel {
    final static int PLAYER = -1;
    final static int EMPTY = 0;

    final static int[][] dirs = {{-1, 1}, {0, 1}, {1, 1}, {-1, 0}, {1, 0},
    {-1, -1}, {0, -1}, {1, -1}};

    final int nRows;
    final int nCols;

    Random rand = new Random();
    boolean[] availableDirections;
    int[][] grid;
    int playerRow, playerCol, score, hiScore;
    boolean gameOver = true;

    public Greed() {
        setPreferredSize(new Dimension(800, 650));
        setBackground(Color.white);
        setForeground(Color.lightGray);
        setFont(new Font("SansSerif", Font.PLAIN, 18));
        setFocusable(true);

        nRows = 22;
        nCols = 61;
        availableDirections = new boolean[dirs.length];
        initGrid();

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
                int k = e.getKeyCode() - KeyEvent.VK_NUMPAD1;
                if (k >= 0 && k != 4 && k < 9) { // skip 5 key
                    move(k > 4 ? --k : k);
                }
                repaint();
            }
        });
    }

    void startNewGame() {
        initGrid();
        if (score > hiScore)
            hiScore = score;
        score = 0;
        playerRow = rand.nextInt(nRows);
        playerCol = rand.nextInt(nCols);
        grid[playerRow][playerCol] = PLAYER;
        availableDirections();
        gameOver = false;
    }

    void initGrid() {
        grid = new int[nRows][nCols];
        for (int r = 0; r < nRows; r++) {
            grid[r] = range(0, nCols).map(v -> rand.nextInt(9) + 1).toArray();
        }
    }

    void move(int d) {
        if (availableDirections[d]) {
            grid[playerRow][playerCol] = EMPTY;

            int x = dirs[d][0];
            int y = dirs[d][1];
            int c = playerCol + x;
            int r = playerRow + y;
            int val = grid[r][c];

            for (int v = 0; v < val - 1; v++, c += x, r += y) {
                score += grid[r][c];
                grid[r][c] = EMPTY;
            }

            score += grid[r][c];
            grid[r][c] = PLAYER;
            playerCol = c;
            playerRow = r;

            if (!availableDirections()) {
                gameOver = true;
            }
        }
    }

    void drawGrid(Graphics2D g) {
        for (int r = 0; r < nRows; r++) {
            for (int c = 0; c < nCols; c++) {
                int val = grid[r][c];
                if (val > 0)
                    g.drawString(String.valueOf(val), 30 + c * 12, 40 + r * 25);
                else if (val == PLAYER) {
                    g.setColor(Color.black);
                    g.fillRect(30 + c * 12, 26 + r * 25, 9, 15);
                    g.setColor(getForeground());
                }
            }
        }
    }

    boolean availableDirections() {
        int countAvailable = 0;

        for (int d = 0; d < dirs.length; d++) {
            availableDirections[d] = false;

            int x = dirs[d][0];
            int y = dirs[d][1];

            int c = playerCol + x;
            int r = playerRow + y;
            if (!withinBounds(r, c))
                continue;

            int val = grid[r][c];

            for (int v = 1; v <= val; v++, c += x, r += y) {
                if (!withinBounds(r, c) || grid[r][c] == 0)
                    break;
                if (v == val) {
                    availableDirections[d] = true;
                    countAvailable++;
                }
            }
        }
        return countAvailable > 0;
    }

    void drawPreview(Graphics2D g) {
        g.setXORMode(Color.white);
        g.setColor(Color.gray);
        for (int d = 0; d < dirs.length; d++) {
            if (!availableDirections[d])
                continue;

            int x = dirs[d][0];
            int y = dirs[d][1];

            int c = playerCol + x;
            int r = playerRow + y;

            int val = grid[r][c];
            for (int v = 1; v <= val; v++, c += x, r += y) {
                g.fillRect(30 + c * 12, 26 + r * 25, 9, 15);
            }
        }
        g.setPaintMode();
    }

    void drawStartScreen(Graphics2D g) {
        g.setStroke(new BasicStroke(2.0f, BasicStroke.CAP_BUTT,
                BasicStroke.JOIN_MITER, 10.0f, new float[]{5.0f}, 0.0f));
        g.setColor(getBackground());
        g.fillRect(150, 120, getWidth() - 320, getHeight() - 300);
        g.setColor(getForeground());
        g.drawRect(170, 140, getWidth() - 360, getHeight() - 340);

        g.setColor(Color.gray);
        g.setFont(new Font("SansSerif", Font.BOLD, 48));
        g.drawString("greed", 315, 280);

        g.setFont(getFont());
        g.drawString("(use numpad to move player)", 270, 350);
        g.drawString("(click to start)", 328, 380);
    }

    void drawScore(Graphics2D g) {
        g.setColor(Color.gray);
        g.setFont(getFont());
        String s = format("hiscore   %s    score   %s", hiScore, score);
        g.drawString(s, 30, getHeight() - 40);
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

        drawGrid(g);
        drawScore(g);

        if (gameOver) {
            drawStartScreen(g);
        } else {
            drawPreview(g);
        }
    }

    public static void main(String[] args) {
        SwingUtilities.invokeLater(() -> {
            JFrame f = new JFrame();
            f.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
            f.setTitle("Greed");
            f.setResizable(false);
            f.add(new Greed(), BorderLayout.CENTER);
            f.pack();
            f.setLocationRelativeTo(null);
            f.setVisible(true);
        });
    }
}
```

