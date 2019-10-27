+++
title = "Hunt The Wumpus/Java"
description = ""
date = 2016-06-23T00:21:58Z
aliases = []
[extra]
id = 20811
[taxonomies]
categories = []
tags = []
+++

{{collection|Hunt_The_Wumpus}}
In this version, arrows only fire a distance of one room. 
Since bats relocate, we don't have to worry about the wumpus room being unreachable. 

There are also 3 bats instead of 2, and you get 3 arrows instead of 5.



==Code==
[[File:wumpus_java.png|200px|thumb|right]]
{{works with|Java|8}}

```java
import java.awt.*;
import java.awt.event.*;
import java.awt.geom.Path2D;
import java.util.*;
import java.util.List;
import javax.swing.*;
import static java.util.stream.Collectors.*;
import static javax.swing.SwingUtilities.*;

public class Wumpus extends JPanel {
    enum Hazard {
        Wumpus("there's an awful smell"),
        Bat("you hear a rustling"),
        Pit("you feel a draft");

        Hazard(String warning) {
            this.warning = warning;
        }
        final String warning;
    }

    static final Random rand = new Random();

    final int roomSize = 45;
    final int playerSize = 16;

    boolean gameOver = true;
    int currRoom, numArrows, wumpusRoom;
    List<String> messages;
    Set<Hazard>[] hazards;

    public Wumpus() {
        setPreferredSize(new Dimension(721, 687));
        setBackground(Color.white);
        setForeground(Color.lightGray);
        setFont(new Font("SansSerif", Font.PLAIN, 18));
        setFocusable(true);

        addMouseListener(new MouseAdapter() {

            @Override
            public void mousePressed(MouseEvent e) {

                if (gameOver) {
                    startNewGame();

                } else {
                    int selectedRoom = -1;
                    int ex = e.getX();
                    int ey = e.getY();

                    for (int link : links[currRoom]) {
                        int cx = rooms[link][0];
                        int cy = rooms[link][1];
                        if (insideRoom(ex, ey, cx, cy)) {
                            selectedRoom = link;
                            break;
                        }
                    }

                    if (selectedRoom == -1)
                        return;

                    if (isLeftMouseButton(e)) {
                        currRoom = selectedRoom;
                        situation();

                    } else if (isRightMouseButton(e)) {
                        shoot(selectedRoom);
                    }
                }
                repaint();
            }

            boolean insideRoom(int ex, int ey, int cx, int cy) {
                return ((ex > cx && ex < cx + roomSize)
                        && (ey > cy && ey < cy + roomSize));
            }
        });
    }

    void startNewGame() {
        numArrows = 3;
        currRoom = rand.nextInt(rooms.length);
        messages = new ArrayList<>();

        hazards = new Set[rooms.length];
        for (int i = 0; i < rooms.length; i++)
            hazards[i] = EnumSet.noneOf(Hazard.class);

        // hazards can share rooms (unless they are identical)
        int[] ordinals = {0, 1, 1, 1, 2, 2};
        Hazard[] values = Hazard.values();
        for (int ord : ordinals) {
            int room;
            do {
                room = rand.nextInt(rooms.length);
            } while (tooClose(room) || hazards[room].contains(values[ord]));

            if (ord == 0)
                wumpusRoom = room;

            hazards[room].add(values[ord]);
        }

        gameOver = false;
    }

    // don't place hazards close to the starting room
    boolean tooClose(int room) {
        if (currRoom == room)
            return true;
        for (int link : links[currRoom])
            if (room == link)
                return true;
        return false;
    }

    void situation() {
        Set<Hazard> set = hazards[currRoom];

        if (set.contains(Hazard.Wumpus)) {
            messages.add("you've been eaten by the Wumpus");
            gameOver = true;

        } else if (set.contains(Hazard.Pit)) {
            messages.add("you fell into a pit");
            gameOver = true;

        } else if (set.contains(Hazard.Bat)) {
            messages.add("a bat dropped you in a random room");

            // teleport, but avoid 2 teleports in a row
            do {
                currRoom = rand.nextInt(rooms.length);
            } while (hazards[currRoom].contains(Hazard.Bat));

            // relocate the bat, but not to the player room or a room with a bat
            set.remove(Hazard.Bat);
            int newRoom;
            do {
                newRoom = rand.nextInt(rooms.length);
            } while (newRoom == currRoom || hazards[newRoom].contains(Hazard.Bat));
            hazards[newRoom].add(Hazard.Bat);

            // re-evaluate
            situation();

        } else {

            // look around
            for (int link : links[currRoom]) {
                for (Hazard hazard : hazards[link])
                    messages.add(hazard.warning);
            }
        }
    }

    void shoot(int room) {
        if (hazards[room].contains(Hazard.Wumpus)) {
            messages.add("You win! You've killed the Wumpus!");
            gameOver = true;

        } else {
            numArrows--;
            if (numArrows == 0) {
                messages.add("You ran out of arrows.");
                gameOver = true;

            } else if (rand.nextInt(4) != 0) { // 75 %
                hazards[wumpusRoom].remove(Hazard.Wumpus);
                wumpusRoom = links[wumpusRoom][rand.nextInt(3)];

                if (wumpusRoom == currRoom) {
                    messages.add("You woke the Wumpus and he ate you");
                    gameOver = true;

                } else {
                    messages.add("You woke the Wumpus and he moved");
                    hazards[wumpusRoom].add(Hazard.Wumpus);
                }
            }
        }
    }

    void drawPlayer(Graphics2D g) {
        int x = rooms[currRoom][0] + (roomSize - playerSize) / 2;
        int y = rooms[currRoom][1] + (roomSize - playerSize) - 2;

        Path2D player = new Path2D.Double();
        player.moveTo(x, y);
        player.lineTo(x + playerSize, y);
        player.lineTo(x + playerSize / 2, y - playerSize);
        player.closePath();

        g.setColor(Color.white);
        g.fill(player);
        g.setStroke(new BasicStroke(1));
        g.setColor(Color.black);
        g.draw(player);
    }

    void drawStartScreen(Graphics2D g) {
        g.setColor(new Color(0xDDFFFFFF, true));
        g.fillRect(0, 0, getWidth(), getHeight() - 60);

        g.setColor(Color.darkGray);
        g.setFont(new Font("SansSerif", Font.BOLD, 48));
        g.drawString("hunt the wumpus", 160, 240);

        g.setFont(getFont());
        g.drawString("left-click to move, right-click to shoot", 210, 310);
        g.drawString("be aware that hazards may be in the same room", 175, 345);
        g.drawString("click to start", 310, 380);
    }

    void drawRooms(Graphics2D g) {
        g.setColor(Color.darkGray);
        g.setStroke(new BasicStroke(2));

        for (int i = 0; i < links.length; i++) {
            for (int link : links[i]) {
                int x1 = rooms[i][0] + roomSize / 2;
                int y1 = rooms[i][1] + roomSize / 2;
                int x2 = rooms[link][0] + roomSize / 2;
                int y2 = rooms[link][1] + roomSize / 2;
                g.drawLine(x1, y1, x2, y2);
            }
        }

        g.setColor(Color.orange);
        for (int[] r : rooms)
            g.fillOval(r[0], r[1], roomSize, roomSize);

        if (!gameOver) {
            g.setColor(Color.magenta);
            for (int link : links[currRoom])
                g.fillOval(rooms[link][0], rooms[link][1], roomSize, roomSize);
        }

        g.setColor(Color.darkGray);
        for (int[] r : rooms)
            g.drawOval(r[0], r[1], roomSize, roomSize);
    }

    void drawMessage(Graphics2D g) {
        if (!gameOver)
            g.drawString("arrows  " + numArrows, 610, 30);

        if (messages != null) {
            g.setColor(Color.black);

            // collapse identical messages
            messages = messages.stream().distinct().collect(toList());

            // concat at most three
            String msg = messages.stream().limit(3).collect(joining(" & "));
            g.drawString(msg, 20, getHeight() - 40);

            // if there's more, print underneath
            if (messages.size() > 3) {
                g.drawString("& " + messages.get(3), 20, getHeight() - 17);
            }

            messages.clear();
        }
    }

    @Override
    public void paintComponent(Graphics gg) {
        super.paintComponent(gg);
        Graphics2D g = (Graphics2D) gg;
        g.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
                RenderingHints.VALUE_ANTIALIAS_ON);

        drawRooms(g);
        if (gameOver) {
            drawStartScreen(g);
        } else {
            drawPlayer(g);
        }
        drawMessage(g);
    }

    public static void main(String[] args) {
        invokeLater(() -> {
            JFrame f = new JFrame();
            f.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
            f.setTitle("Hunt The Wumpus");
            f.setResizable(false);
            f.add(new Wumpus(), BorderLayout.CENTER);
            f.pack();
            f.setLocationRelativeTo(null);
            f.setVisible(true);
        });
    }

    int[][] rooms = {{334, 20}, {609, 220}, {499, 540}, {169, 540}, {62, 220},
    {169, 255}, {232, 168}, {334, 136}, {435, 168}, {499, 255}, {499, 361},
    {435, 447}, {334, 480}, {232, 447}, {169, 361}, {254, 336}, {285, 238},
    {387, 238}, {418, 336}, {334, 393}};

    int[][] links = {{4, 7, 1}, {0, 9, 2}, {1, 11, 3}, {4, 13, 2}, {0, 5, 3},
    {4, 6, 14}, {7, 16, 5}, {6, 0, 8}, {7, 17, 9}, {8, 1, 10}, {9, 18, 11},
    {10, 2, 12}, {13, 19, 11}, {14, 3, 12}, {5, 15, 13}, {14, 16, 19},
    {6, 17, 15}, {16, 8, 18}, {19, 10, 17}, {15, 12, 18}};
}
```

