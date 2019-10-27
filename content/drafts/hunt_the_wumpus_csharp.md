+++
title = "Hunt The Wumpus/CSharp"
description = ""
date = 2017-09-09T08:14:42Z
aliases = []
[extra]
id = 21589
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
{{trans|Java}}

```csharp
using System;
using System.Collections.Generic;
using System.Drawing;
using System.Drawing.Drawing2D;
using System.Linq;
using System.Windows.Forms;

namespace HuntTheWumpus
{
    public partial class Form1 : Form
    {
        enum Hazard { Wumpus, Bat, Pit }

        static readonly string[] warnings = { "there's an awful smell", "you hear a rustling", "you feel a draft" };
        static readonly Random rand = new Random();

        readonly int roomSize = 45;
        readonly int playerSize = 16;

        bool gameOver = true;
        int currRoom, numArrows, wumpusRoom;
        List<string> messages;
        HashSet<Hazard>[] hazards;

        Font font = new Font("SansSerif", 10, FontStyle.Bold);

        public Form1()
        {
            Width = 721;
            Height = 687;
            StartPosition = FormStartPosition.CenterScreen;
            SetStyle(
                ControlStyles.AllPaintingInWmPaint |
                ControlStyles.UserPaint |
                ControlStyles.DoubleBuffer,
                true);

            MouseDown += HandleMouseDown;
        }

        private void HandleMouseDown(object sender, MouseEventArgs e)
        {
            if (gameOver)
            {
                StartNewGame();
            }
            else
            {
                int selectedRoom = -1;

                for (int i = 0; i < links.GetLength(1); i++)
                {
                    int link = links[currRoom, i];
                    int cx = rooms[link, 0];
                    int cy = rooms[link, 1];
                    if (InsideRoom(e.X,  e.Y, cx, cy))
                    {
                        selectedRoom = link;
                        break;
                    }
                }

                if (selectedRoom == -1)
                {
                    return;
                }

                if (MouseButtons.Left == e.Button)
                {
                    currRoom = selectedRoom;
                    Situation();

                }
                else if (MouseButtons.Right == e.Button)
                {
                    Shoot(selectedRoom);
                }
            }
            Refresh();

            bool InsideRoom(int ex, int ey, int cx, int cy)
            {
                return ((ex > cx && ex < cx + roomSize)
                        && (ey > cy && ey < cy + roomSize));
            }
        }

        // don't place hazards close to the starting room
        private bool TooClose(int room)
        {
            if (currRoom == room)
            {
                return true;
            }

            for (int i = 0; i < links.GetLength(1); i++)
            {
                int link = links[currRoom, i];
                if (room == link)
                    return true;
            }
            return false;
        }

        private void Shoot(int room)
        {
            if (hazards[room].Contains(Hazard.Wumpus))
            {
                messages.Add("You win! You've killed the Wumpus!");
                gameOver = true;
            }
            else
            {
                numArrows--;
                if (numArrows == 0)
                {
                    messages.Add("You ran out of arrows.");
                    gameOver = true;
                }
                else if (rand.Next(4) != 0) // 75 %
                {
                    hazards[wumpusRoom].Remove(Hazard.Wumpus);
                    wumpusRoom = links[wumpusRoom, rand.Next(3)];

                    if (wumpusRoom == currRoom)
                    {
                        messages.Add("You woke the Wumpus and he ate you");
                        gameOver = true;
                    }
                    else
                    {
                        messages.Add("You woke the Wumpus and he moved");
                        hazards[wumpusRoom].Add(Hazard.Wumpus);
                    }
                }
            }
        }

        private void Situation()
        {
            var set = hazards[currRoom];

            if (set.Contains(Hazard.Wumpus))
            {
                messages.Add("you've been eaten by the Wumpus");
                gameOver = true;
            }
            else if (set.Contains(Hazard.Pit))
            {
                messages.Add("you fell into a pit");
                gameOver = true;
            }
            else if (set.Contains(Hazard.Bat))
            {
                messages.Add("a bat dropped you in a random room");

                // teleport, but avoid 2 teleports in a row
                do
                {
                    currRoom = rand.Next(rooms.GetLength(0));
                }
                while (hazards[currRoom].Contains(Hazard.Bat));

                // relocate the bat, but not to the player room or a room with a bat
                set.Remove(Hazard.Bat);
                int newRoom;
                do
                {
                    newRoom = rand.Next(rooms.GetLength(0));
                }
                while (newRoom == currRoom || hazards[newRoom].Contains(Hazard.Bat));
                hazards[newRoom].Add(Hazard.Bat);

                // re-evaluate
                Situation();
            }
            else
            {
                // look around
                for (int i = 0; i < links.GetLength(1); i++)
                {
                    int link = links[currRoom, i];
                    foreach (var hazard in hazards[link])
                        messages.Add(warnings[(Int32)hazard]);
                }
            }
        }

        private void StartNewGame()
        {
            numArrows = 3;
            currRoom = rand.Next(rooms.GetLength(0));
            messages = new List<string>();

            hazards = new HashSet<Hazard>[rooms.GetLength(0)];
            for (int i = 0; i < rooms.GetLength(0); i++)
                hazards[i] = new HashSet<Hazard>();

            // hazards can share rooms (unless they are identical)
            int[] ordinals = { 0, 1, 1, 1, 2, 2 };
            var values = Enum.GetValues(typeof(Hazard));

            foreach (int ord in ordinals)
            {
                int room;
                do
                {
                    room = rand.Next(rooms.GetLength(0));
                }
                while (TooClose(room) || hazards[room].Contains((Hazard)values.GetValue(ord)));

                if (ord == 0)
                    wumpusRoom = room;

                hazards[room].Add((Hazard)values.GetValue(ord));
            }
            gameOver = false;
        }

        protected override void OnPaint(PaintEventArgs args)
        {
            Graphics g = args.Graphics;
            g.SmoothingMode = SmoothingMode.AntiAlias;
            g.Clear(Color.White);

            DrawRooms(g);
            if (gameOver)
            {
                DrawStartScreen(g);
            }
            else
            {
                DrawPlayer(g);
            }
            DrawMessage(g);
        }

        private void DrawPlayer(Graphics g)
        {
            int x = rooms[currRoom, 0] + (roomSize - playerSize) / 2;
            int y = rooms[currRoom, 1] + (roomSize - playerSize) - 2;

            var player = new GraphicsPath();
            player.AddLine(x, y, x + playerSize, y);
            player.AddLine(x + playerSize, y, x + playerSize / 2, y - playerSize);
            player.CloseFigure();

            g.FillPath(Brushes.White, player);
            g.DrawPath(Pens.Black, player);
        }

        private void DrawStartScreen(Graphics g)
        {
            var brush = new SolidBrush(Color.FromArgb(0xDD, Color.White));
            g.FillRectangle(brush, 0, 0, Width, Height - 60);

            g.DrawString("HUNT THE WUMPUS", font, Brushes.DarkGray, 280, 240);
            g.DrawString("left-click to move, right-click to shoot", font, Brushes.DarkGray, 230, 310);
            g.DrawString("be aware that hazards may be in the same room", font, Brushes.DarkGray, 195, 345);
            g.DrawString("click to start", font, Brushes.DarkGray, 315, 380);
        }

        private void DrawRooms(Graphics g)
        {
            var fatLine = new Pen(Color.DarkGray, 2);

            for (int i = 0; i < links.GetLength(0); i++)
            {
                for (int j = 0; j < links.GetLength(1); j++)
                {
                    int link = links[i, j];
                    int x1 = rooms[i, 0] + roomSize / 2;
                    int y1 = rooms[i, 1] + roomSize / 2;
                    int x2 = rooms[link, 0] + roomSize / 2;
                    int y2 = rooms[link, 1] + roomSize / 2;
                    g.DrawLine(fatLine, x1, y1, x2, y2);
                }
            }

            for (int i = 0; i < rooms.GetLength(0); i++)
            {
                g.FillEllipse(Brushes.Orange, rooms[i, 0], rooms[i, 1], roomSize, roomSize);
            }

            if (!gameOver)
            {
                for (int i = 0; i < links.GetLength(1); i++)
                {
                    var link = links[currRoom, i];
                    g.FillEllipse(Brushes.Magenta, rooms[link, 0], rooms[link, 1], roomSize, roomSize);
                }
            }

            for (int i = 0; i < rooms.GetLength(0); i++)
            {
                g.DrawEllipse(fatLine, rooms[i, 0], rooms[i, 1], roomSize, roomSize);
            }
        }

        private void DrawMessage(Graphics g)
        {
            if (!gameOver)
                g.DrawString("arrows  " + numArrows, font, Brushes.Black, 610, 30);

            if (messages != null)
            {
                // collapse identical messages
                messages = messages.Distinct().ToList();

                // join at most three
                var msg = string.Join(" & ", messages.Take(3));
                g.DrawString(msg, font, Brushes.Black, 20, Height - 80);

                // if there's more, print underneath
                if (messages.Count > 3)
                {
                    g.DrawString("& " + messages[3], font, Brushes.Black, 20, Height - 60);
                }

                messages.Clear();
            }
        }

        private int[,] rooms = {{334, 20}, {609, 220}, {499, 540}, {169, 540}, {62, 220},
            {169, 255}, {232, 168}, {334, 136}, {435, 168}, {499, 255}, {499, 361},
            {435, 447}, {334, 480}, {232, 447}, {169, 361}, {254, 336}, {285, 238},
            {387, 238}, {418, 336}, {334, 393}};

        private int[,] links = {{4, 7, 1}, {0, 9, 2}, {1, 11, 3}, {4, 13, 2}, {0, 5, 3},
            {4, 6, 14}, {7, 16, 5}, {6, 0, 8}, {7, 17, 9}, {8, 1, 10}, {9, 18, 11},
            {10, 2, 12}, {13, 19, 11}, {14, 3, 12}, {5, 15, 13}, {14, 16, 19},
            {6, 17, 15}, {16, 8, 18}, {19, 10, 17}, {15, 12, 18}};
    }
}
```

