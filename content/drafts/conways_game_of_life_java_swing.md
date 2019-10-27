+++
title = "Conway's Game of Life/Java/Swing"
description = ""
date = 2015-10-26T20:19:37Z
aliases = []
[extra]
id = 17096
[taxonomies]
categories = []
tags = []
+++

{{works with|Java|6+}}

This swing based implementation will take user input, and can be interacted with using the mouse.


```java

import java.awt.*;
import java.awt.event.*;
import java.net.URI;
import java.util.ArrayList;
import java.util.ConcurrentModificationException;
import javax.swing.*;

/**
 * Conway's game of life is a cellular automaton devised by the
 * mathematician John Conway.
 */
public class ConwaysGameOfLife extends JFrame implements ActionListener {
    private static final Dimension DEFAULT_WINDOW_SIZE = new Dimension(800, 600);
    private static final Dimension MINIMUM_WINDOW_SIZE = new Dimension(400, 400);
    private static final int BLOCK_SIZE = 10;

    private JMenuBar mb_menu;
    private JMenu m_file, m_game, m_help;
    private JMenuItem mi_file_options, mi_file_exit;
    private JMenuItem mi_game_autofill, mi_game_play, mi_game_stop, mi_game_reset;
    private JMenuItem mi_help_about, mi_help_source;
    private int i_movesPerSecond = 3;
    private GameBoard gb_gameBoard;
    private Thread game;
    
    public static void main(String[] args) {
        // Setup the swing specifics
        JFrame game = new ConwaysGameOfLife();
        game.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        game.setTitle("Conway's Game of Life");
        game.setIconImage(new ImageIcon(ConwaysGameOfLife.class.getResource("/images/logo.png")).getImage());
        game.setSize(DEFAULT_WINDOW_SIZE);
        game.setMinimumSize(MINIMUM_WINDOW_SIZE);
        game.setLocation((Toolkit.getDefaultToolkit().getScreenSize().width - game.getWidth())/2, 
                (Toolkit.getDefaultToolkit().getScreenSize().height - game.getHeight())/2);
        game.setVisible(true);
    }
    
    public ConwaysGameOfLife() {
        // Setup menu
        mb_menu = new JMenuBar();
        setJMenuBar(mb_menu);
        m_file = new JMenu("File");
        mb_menu.add(m_file);
        m_game = new JMenu("Game");
        mb_menu.add(m_game);
        m_help = new JMenu("Help");
        mb_menu.add(m_help);
        mi_file_options = new JMenuItem("Options");
        mi_file_options.addActionListener(this);
        mi_file_exit = new JMenuItem("Exit");
        mi_file_exit.addActionListener(this);
        m_file.add(mi_file_options);
        m_file.add(new JSeparator());
        m_file.add(mi_file_exit);
        mi_game_autofill = new JMenuItem("Autofill");
        mi_game_autofill.addActionListener(this);
        mi_game_play = new JMenuItem("Play");
        mi_game_play.addActionListener(this);
        mi_game_stop = new JMenuItem("Stop");
        mi_game_stop.setEnabled(false);
        mi_game_stop.addActionListener(this);
        mi_game_reset = new JMenuItem("Reset");
        mi_game_reset.addActionListener(this);
        m_game.add(mi_game_autofill);
        m_game.add(new JSeparator());
        m_game.add(mi_game_play);
        m_game.add(mi_game_stop);
        m_game.add(mi_game_reset);
        mi_help_about = new JMenuItem("About");
        mi_help_about.addActionListener(this);
        mi_help_source = new JMenuItem("Source");
        mi_help_source.addActionListener(this);
        m_help.add(mi_help_about);
        m_help.add(mi_help_source);
        // Setup game board
        gb_gameBoard = new GameBoard();
        add(gb_gameBoard);
    }
    
    public void setGameBeingPlayed(boolean isBeingPlayed) {
        if (isBeingPlayed) {
            mi_game_play.setEnabled(false);
            mi_game_stop.setEnabled(true);
            game = new Thread(gb_gameBoard);
            game.start();
        } else {
            mi_game_play.setEnabled(true);
            mi_game_stop.setEnabled(false);
            game.interrupt();
        }
    }
        
    @Override
    public void actionPerformed(ActionEvent ae) {
        if (ae.getSource().equals(mi_file_exit)) {
            // Exit the game
            System.exit(0);
        } else if (ae.getSource().equals(mi_file_options)) {
            // Put up an options panel to change the number of moves per second
            final JFrame f_options = new JFrame();
            f_options.setTitle("Options");
            f_options.setSize(300,60);
            f_options.setLocation((Toolkit.getDefaultToolkit().getScreenSize().width - f_options.getWidth())/2, 
                (Toolkit.getDefaultToolkit().getScreenSize().height - f_options.getHeight())/2);
            f_options.setResizable(false);
            JPanel p_options = new JPanel();
            p_options.setOpaque(false);
            f_options.add(p_options);
            p_options.add(new JLabel("Number of moves per second:"));
            Integer[] secondOptions = {1,2,3,4,5,10,15,20};
            final JComboBox cb_seconds = new JComboBox(secondOptions);
            p_options.add(cb_seconds);
            cb_seconds.setSelectedItem(i_movesPerSecond);
            cb_seconds.addActionListener(new ActionListener(){
                @Override
                public void actionPerformed(ActionEvent ae) {
                    i_movesPerSecond = (Integer)cb_seconds.getSelectedItem();
                    f_options.dispose();
                }
            });
            f_options.setVisible(true);
        } else if (ae.getSource().equals(mi_game_autofill)) {
            final JFrame f_autoFill = new JFrame();
            f_autoFill.setTitle("Autofill");
            f_autoFill.setSize(360, 60);
            f_autoFill.setLocation((Toolkit.getDefaultToolkit().getScreenSize().width - f_autoFill.getWidth())/2, 
                (Toolkit.getDefaultToolkit().getScreenSize().height - f_autoFill.getHeight())/2);
            f_autoFill.setResizable(false);
            JPanel p_autoFill = new JPanel();
            p_autoFill.setOpaque(false);
            f_autoFill.add(p_autoFill);
            p_autoFill.add(new JLabel("What percentage should be filled? "));
            Object[] percentageOptions = {"Select",5,10,15,20,25,30,40,50,60,70,80,90,95};
            final JComboBox cb_percent = new JComboBox(percentageOptions);
            p_autoFill.add(cb_percent);
            cb_percent.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(ActionEvent e) {
                    if (cb_percent.getSelectedIndex() > 0) {
                        gb_gameBoard.resetBoard();
                        gb_gameBoard.randomlyFillBoard((Integer)cb_percent.getSelectedItem());
                        f_autoFill.dispose();
                    }
                }
            });
            f_autoFill.setVisible(true);
        } else if (ae.getSource().equals(mi_game_reset)) {
            gb_gameBoard.resetBoard();
            gb_gameBoard.repaint();
        } else if (ae.getSource().equals(mi_game_play)) {
            setGameBeingPlayed(true);
        } else if (ae.getSource().equals(mi_game_stop)) {
            setGameBeingPlayed(false);
        } else if (ae.getSource().equals(mi_help_source)) {
            Desktop desktop = Desktop.isDesktopSupported() ? Desktop.getDesktop() : null;
            try {
                desktop.browse(new URI("https://github.com/Burke9077/Conway-s-Game-of-Life"));
            } catch (Exception ex) {
                JOptionPane.showMessageDialog(null, "Source is available on GitHub at:\nhttps://github.com/Burke9077/Conway-s-Game-of-Life", "Source", JOptionPane.INFORMATION_MESSAGE);
            }
        } else if (ae.getSource().equals(mi_help_about)) {
            JOptionPane.showMessageDialog(null, "Conway's game of life was a cellular animation devised by the mathematician John Conway.\nThis Java, swing based implementation was created by Matthew Burke.\n\nhttp://burke9077.com\nBurke9077@gmail.com\n@burke9077\n\nCreative Commons Attribution 4.0 International");
        }
    }
    
    private class GameBoard extends JPanel implements ComponentListener, MouseListener, MouseMotionListener, Runnable {
        private Dimension d_gameBoardSize = null;
        private ArrayList<Point> point = new ArrayList<Point>(0);
        
        public GameBoard() {
            // Add resizing listener
            addComponentListener(this);
            addMouseListener(this);
            addMouseMotionListener(this);
        }
        
        private void updateArraySize() {
            ArrayList<Point> removeList = new ArrayList<Point>(0);
            for (Point current : point) {
                if ((current.x > d_gameBoardSize.width-1) || (current.y > d_gameBoardSize.height-1)) {
                    removeList.add(current);
                }
            }
            point.removeAll(removeList);
            repaint();
        }
        
        public void addPoint(int x, int y) {
            if (!point.contains(new Point(x,y))) {
                point.add(new Point(x,y));
            } 
            repaint();
        }
        
        public void addPoint(MouseEvent me) {
            int x = me.getPoint().x/BLOCK_SIZE-1;
            int y = me.getPoint().y/BLOCK_SIZE-1;
            if ((x >= 0) && (x < d_gameBoardSize.width) && (y >= 0) && (y < d_gameBoardSize.height)) {
                addPoint(x,y);
            }
        }
        
        public void removePoint(int x, int y) {
            point.remove(new Point(x,y));
        }
        
        public void resetBoard() {
            point.clear();
        }
        
        public void randomlyFillBoard(int percent) {
            for (int i=0; i<d_gameBoardSize.width; i++) {
                for (int j=0; j<d_gameBoardSize.height; j++) {
                    if (Math.random()*100 < percent) {
                        addPoint(i,j);
                    }
                }
            }
        }
        
        @Override
        public void paintComponent(Graphics g) {
            super.paintComponent(g);
            try {
                for (Point newPoint : point) {
                    // Draw new point
                    g.setColor(Color.blue);
                    g.fillRect(BLOCK_SIZE + (BLOCK_SIZE*newPoint.x), BLOCK_SIZE + (BLOCK_SIZE*newPoint.y), BLOCK_SIZE, BLOCK_SIZE);
                }
            } catch (ConcurrentModificationException cme) {}
            // Setup grid
            g.setColor(Color.BLACK);
            for (int i=0; i<=d_gameBoardSize.width; i++) {
                g.drawLine(((i*BLOCK_SIZE)+BLOCK_SIZE), BLOCK_SIZE, (i*BLOCK_SIZE)+BLOCK_SIZE, BLOCK_SIZE + (BLOCK_SIZE*d_gameBoardSize.height));
            }
            for (int i=0; i<=d_gameBoardSize.height; i++) {
                g.drawLine(BLOCK_SIZE, ((i*BLOCK_SIZE)+BLOCK_SIZE), BLOCK_SIZE*(d_gameBoardSize.width+1), ((i*BLOCK_SIZE)+BLOCK_SIZE));
            }
        }

        @Override
        public void componentResized(ComponentEvent e) {
            // Setup the game board size with proper boundries
            d_gameBoardSize = new Dimension(getWidth()/BLOCK_SIZE-2, getHeight()/BLOCK_SIZE-2);
            updateArraySize();
        }
        @Override
        public void componentMoved(ComponentEvent e) {}
        @Override
        public void componentShown(ComponentEvent e) {}
        @Override
        public void componentHidden(ComponentEvent e) {}
        @Override
        public void mouseClicked(MouseEvent e) {}
        @Override
        public void mousePressed(MouseEvent e) {}
        @Override
        public void mouseReleased(MouseEvent e) {
            // Mouse was released (user clicked)
            addPoint(e);
        }
        @Override
        public void mouseEntered(MouseEvent e) {}

        @Override
        public void mouseExited(MouseEvent e) {}

        @Override
        public void mouseDragged(MouseEvent e) {
            // Mouse is being dragged, user wants multiple selections
            addPoint(e);
        }
        @Override
        public void mouseMoved(MouseEvent e) {}

        @Override
        public void run() {
            boolean[][] gameBoard = new boolean[d_gameBoardSize.width+2][d_gameBoardSize.height+2];
            for (Point current : point) {
                gameBoard[current.x+1][current.y+1] = true;
            }
            ArrayList<Point> survivingCells = new ArrayList<Point>(0);
            // Iterate through the array, follow game of life rules
            for (int i=1; i<gameBoard.length-1; i++) {
                for (int j=1; j<gameBoard[0].length-1; j++) {
                    int surrounding = 0;
                    if (gameBoard[i-1][j-1]) { surrounding++; }
                    if (gameBoard[i-1][j])   { surrounding++; }
                    if (gameBoard[i-1][j+1]) { surrounding++; }
                    if (gameBoard[i][j-1])   { surrounding++; }
                    if (gameBoard[i][j+1])   { surrounding++; }
                    if (gameBoard[i+1][j-1]) { surrounding++; }
                    if (gameBoard[i+1][j])   { surrounding++; }
                    if (gameBoard[i+1][j+1]) { surrounding++; }
                    if (gameBoard[i][j]) {
                        // Cell is alive, Can the cell live? (2-3)
                        if ((surrounding == 2) || (surrounding == 3)) {
                            survivingCells.add(new Point(i-1,j-1));
                        } 
                    } else {
                        // Cell is dead, will the cell be given birth? (3)
                        if (surrounding == 3) {
                            survivingCells.add(new Point(i-1,j-1));
                        }
                    }
                }
            }
            resetBoard();
            point.addAll(survivingCells);
            repaint();
            try {
                Thread.sleep(1000/i_movesPerSecond);
                run();
            } catch (InterruptedException ex) {}
        }
    }
}

```

