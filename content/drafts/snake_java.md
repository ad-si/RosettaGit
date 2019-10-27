+++
title = "Snake/Java"
description = ""
date = 2019-09-01T18:12:53Z
aliases = []
[extra]
id = 20703
[taxonomies]
categories = []
tags = []
+++

==Code==
{{works with|Java|8}}

```java

/***************************************************/ 
/*  Sourced By Samuel Dennis                       */ 
/*                                                 */
/*  Last Editied (09/01/2019)                      */
/***************************************************/
import java.awt.*;
import java.awt.event.*;
import static java.lang.String.format;
import java.util.*;
import java.util.List;
import javax.swing.*;

public class Snake extends JPanel implements Runnable {
    enum Dir {
        up(0, -1), right(1, 0), down(0, 1), left(-1, 0);

        Dir(int x, int y) {
            this.x = x; this.y = y;
        }

        final int x, y;
    }

    static final Random rand = new Random();
    static final int WALL = -1;
    static final int MAX_ENERGY = 1500;

    volatile boolean gameOver = true;

    Thread gameThread;
    int score, hiScore;
    int nRows = 44;
    int nCols = 64;
    Dir dir;
    int energy;

    int[][] grid;
    List<Point> snake, treats;
    Font smallFont;

    public Snake() {
        setPreferredSize(new Dimension(640, 440));
        setBackground(Color.white);
        setFont(new Font("SansSerif", Font.BOLD, 48));
        setFocusable(true);

        smallFont = getFont().deriveFont(Font.BOLD, 18);
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

                switch (e.getKeyCode()) {

                    case KeyEvent.VK_UP:
                        if (dir != Dir.down)
                            dir = Dir.up;
                        break;

                    case KeyEvent.VK_LEFT:
                        if (dir != Dir.right)
                            dir = Dir.left;
                        break;

                    case KeyEvent.VK_RIGHT:
                        if (dir != Dir.left)
                            dir = Dir.right;
                        break;

                    case KeyEvent.VK_DOWN:
                        if (dir != Dir.up)
                            dir = Dir.down;
                        break;
                }
                repaint();
            }
        });
    }

    void startNewGame() {
        gameOver = false;

        stop();
        initGrid();
        treats = new LinkedList<>();

        dir = Dir.left;
        energy = MAX_ENERGY;

        if (score > hiScore)
            hiScore = score;
        score = 0;

        snake = new ArrayList<>();
        for (int x = 0; x < 7; x++)
            snake.add(new Point(nCols / 2 + x, nRows / 2));

        do
            addTreat();
        while(treats.isEmpty());

        (gameThread = new Thread(this)).start();
    }

    void stop() {
        if (gameThread != null) {
            Thread tmp = gameThread;
            gameThread = null;
            tmp.interrupt();
        }
    }

    void initGrid() {
        grid = new int[nRows][nCols];
        for (int r = 0; r < nRows; r++) {
            for (int c = 0; c < nCols; c++) {
                if (c == 0 || c == nCols - 1 || r == 0 || r == nRows - 1)
                    grid[r][c] = WALL;
            }
        }
    }

    @Override
    public void run() {

        while (Thread.currentThread() == gameThread) {

            try {
                Thread.sleep(Math.max(75 - score, 25));
            } catch (InterruptedException e) {
                return;
            }

            if (energyUsed() || hitsWall() || hitsSnake()) {
                gameOver();
            } else {
                if (eatsTreat()) {
                    score++;
                    energy = MAX_ENERGY;
                    growSnake();
                }
                moveSnake();
                addTreat();
            }
            repaint();
        }
    }

    boolean energyUsed() {
        energy -= 10;
        return energy <= 0;
    }

    boolean hitsWall() {
        Point head = snake.get(0);
        int nextCol = head.x + dir.x;
        int nextRow = head.y + dir.y;
        return grid[nextRow][nextCol] == WALL;
    }

    boolean hitsSnake() {
        Point head = snake.get(0);
        int nextCol = head.x + dir.x;
        int nextRow = head.y + dir.y;
        for (Point p : snake)
            if (p.x == nextCol && p.y == nextRow)
                return true;
        return false;
    }

    boolean eatsTreat() {
        Point head = snake.get(0);
        int nextCol = head.x + dir.x;
        int nextRow = head.y + dir.y;
        for (Point p : treats)
            if (p.x == nextCol && p.y == nextRow) {
                return treats.remove(p);
            }
        return false;
    }

    void gameOver() {
        gameOver = true;
        stop();
    }

    void moveSnake() {
        for (int i = snake.size() - 1; i > 0; i--) {
            Point p1 = snake.get(i - 1);
            Point p2 = snake.get(i);
            p2.x = p1.x;
            p2.y = p1.y;
        }
        Point head = snake.get(0);
        head.x += dir.x;
        head.y += dir.y;
    }

    void growSnake() {
        Point tail = snake.get(snake.size() - 1);
        int x = tail.x + dir.x;
        int y = tail.y + dir.y;
        snake.add(new Point(x, y));
    }

    void addTreat() {
        if (treats.size() < 3) {

            if (rand.nextInt(10) == 0) { // 1 in 10

                if (rand.nextInt(4) != 0) {  // 3 in 4
                    int x, y;
                    while (true) {

                        x = rand.nextInt(nCols);
                        y = rand.nextInt(nRows);
                        if (grid[y][x] != 0)
                            continue;

                        Point p = new Point(x, y);
                        if (snake.contains(p) || treats.contains(p))
                            continue;

                        treats.add(p);
                        break;
                    }
                } else if (treats.size() > 1)
                    treats.remove(0);
            }
        }
    }

    void drawGrid(Graphics2D g) {
        g.setColor(Color.lightGray);
        for (int r = 0; r < nRows; r++) {
            for (int c = 0; c < nCols; c++) {
                if (grid[r][c] == WALL)
                    g.fillRect(c * 10, r * 10, 10, 10);
            }
        }
    }

    void drawSnake(Graphics2D g) {
        g.setColor(Color.blue);
        for (Point p : snake)
            g.fillRect(p.x * 10, p.y * 10, 10, 10);

        g.setColor(energy < 500 ? Color.red : Color.orange);
        Point head = snake.get(0);
        g.fillRect(head.x * 10, head.y * 10, 10, 10);
    }

    void drawTreats(Graphics2D g) {
        g.setColor(Color.green);
        for (Point p : treats)
            g.fillRect(p.x * 10, p.y * 10, 10, 10);
    }

    void drawStartScreen(Graphics2D g) {
        g.setColor(Color.blue);
        g.setFont(getFont());
        g.drawString("Snake", 240, 190);
        g.setColor(Color.orange);
        g.setFont(smallFont);
        g.drawString("(click to start)", 250, 240);
    }

    void drawScore(Graphics2D g) {
        int h = getHeight();
        g.setFont(smallFont);
        g.setColor(getForeground());
        String s = format("hiscore %d    score %d", hiScore, score);
        g.drawString(s, 30, h - 30);
        g.drawString(format("energy %d", energy), getWidth() - 150, h - 30);
    }

    @Override
    public void paintComponent(Graphics gg) {
        super.paintComponent(gg);
        Graphics2D g = (Graphics2D) gg;
        g.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
                RenderingHints.VALUE_ANTIALIAS_ON);

        drawGrid(g);

        if (gameOver) {
            drawStartScreen(g);
        } else {
            drawSnake(g);
            drawTreats(g);
            drawScore(g);
        }
    }

    public static void main(String[] args) {
        SwingUtilities.invokeLater(() -> {
            JFrame f = new JFrame();
            f.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
            f.setTitle("Snake");
            f.setResizable(false);
            f.add(new Snake(), BorderLayout.CENTER);
            f.pack();
            f.setLocationRelativeTo(null);
            f.setVisible(true);
        });
    }
}
```

