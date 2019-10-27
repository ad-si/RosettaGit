+++
title = "Remote agent/Agent logic/PicoLisp"
description = ""
date = 2014-09-08T15:27:16Z
aliases = []
[extra]
id = 9083
[taxonomies]
categories = []
tags = []
+++

This is the client for [[Remote agent/Agent logic]]. For the server, see [[Remote agent/Simulation#PicoLisp]].

```PicoLisp
# Global variables:
#  '*Sock' is the TCP socket to the server
#  '*Dir' is a circular list of direction structures
#  '*World' holds the explored world
#  '*Ball' is the ball found in current field
#  '*Todo' is the list of mismatching fields and balls

(load "@lib/simul.l")

(de *Dir .
   ((north south . extendNorth) (east west . extendEast)
      (south north . extendSouth) (west east . extendWest) . ) )

(de gameClient (Host Port)
   (unless (setq *Sock (connect Host Port))
      (quit "Can't connect to " (cons Host Port)) )
   (in *Sock
      (when (= "A" (char (rd 1)))  # Greeting
         (out *Sock (prin "A"))
         (with (def (box) (cons (cons) (cons)))
            # Explore the world
            (setq *World (cons (cons This)))
            (off *Ball *Todo)
            (let (Turns 4  Color T)  # Initially 4 turns, unknown color
               (recur (This Turns Color)
                  (setThis Color)
                  (turnLeft)
                  (do Turns
                     (ifn (and (not (get This (caar *Dir))) (goForward))
                        (turnRight)
                        (let Next @
                           (unless ((caar *Dir) This)
                              ((cddar *Dir)) )  # Extend world
                           (put This (caar *Dir) ((caar *Dir) This))
                           (put ((caar *Dir) This) (cadar *Dir) This)
                           (if (get ((caar *Dir) This) 'field)
                              (do 2 (turnRight))
                              (recurse ((caar *Dir) This) 3 Next) )
                           (setThis (goForward)) )  # Final color on return
                        (turnLeft) ) ) ) )
            # Establish the walls
            (for Col *World
               (for This Col
                  (set This
                     (cons
                        (cons (: west) (: east))
                        (cons (: south) (: north)) ) ) ) )
            (prinl "Initial state:")
            (showWorld)
            (prin "Moving balls ... ")
            # Move balls to proper fields
            (for X *Todo
               (findField                    # Move to next field
                  (== This (car X)) )
               (getBall)                     # Pick the ball
               (findField                    # Find a suitable field
                  (unless (: ball)
                     (= (: field) (cdr X)) ) )
               (prin (cdr X))
               (flush)
               (dropBall (cdr X)) )          # Drop the ball
            (prinl "Final state:")
            (showWorld) ) ) ) )

# Set color and ball in field
(de setThis (Color)
   (=: field Color)
   (=: ball *Ball)
   (and
      *Ball
      (<> @ Color)
      (push1 '*Todo (cons This *Ball)) ) )

# Commands to server
(de goForward ()
   (out *Sock (prin "\^"))
   (in *Sock
      (let F (char (rd 1))
         (cond
            ((= "|" F) (off *Ball F) (rd 1))
            ((= "." (setq *Ball (uppc (char (rd 1)))))
               (off *Ball) )
            (T (rd 1)) )
         F ) ) )

(de turnRight ()
   (out *Sock (prin ">"))
   (pop '*Dir)
   (rd 1) )

(de turnLeft ()
   (out *Sock (prin "<"))
   (do 3 (pop '*Dir))
   (rd 1) )

(de getBall ()
   (out *Sock (prin "@"))
   (case (char (rd 1))
      ("s" (quit "No ball in sector"))
      ("A" (quit "Agent full"))
      ("." (=: ball NIL))
      (T (quit "Unexpected event" @)) ) )

(de dropBall (Ball)
   (out *Sock (prin "!"))
   (case (char (rd 1))
      ("a" (quit "No ball in agent"))
      ("S" (quit "Sector full"))
      ("." (=: ball Ball))
      ("+" (rd 1) (prinl " ... Game over!"))
      (T (quit "Unexpected event" @)) ) )

# Extend world to the north
(de extendNorth ()
   (let Last NIL
      (for Col *World
         (let (Old (last Col)  New (def (box) (cons (cons Last) (cons Old))))
            (conc Col (cons New))
            (and Last (con (car (val @)) New))
            (setq Last (con (cdr (val Old)) New)) ) ) ) )

# Extend world to the east
(de extendEast ()
   (conc *World
      (cons
         (let Last NIL
            (mapcar
               '((Old)
                  (let New (def (box) (cons (cons Old) (cons Last)))
                     (and Last (con (cdr (val @)) New))
                     (setq Last (con (car (val Old)) New)) ) )
               (last *World) ) ) ) ) )

# Extend world to the south
(de extendSouth ()
   (let Last NIL
      (map
         '((Lst)
            (push Lst
               (let
                  (Old (caar Lst)
                     New (def (box) (cons (cons Last) (cons NIL Old))) )
                  (and Last (con (car (val @)) New))
                  (setq Last (set (cdr (val Old)) New)) ) ) )
         *World ) ) )

# Extend world to the west
(de extendWest ()
   (push '*World
      (let Last NIL
         (mapcar
            '((Old)
               (let New (def (box) (cons (cons NIL Old) (cons Last)))
                  (and Last (con (cdr (val @)) New))
                  (setq Last (set (car (val Old)) New)) ) )
            (car *World) ) ) ) )

# Find matching field
(de findField Prg
   (setq This
      (catch NIL
         (recur (This)
            (unless (: mark)
               (and (run Prg) (throw NIL This))
               (finally (=: mark NIL)
                  (=: mark T)
                  (do 4
                     (when ((caar *Dir) This)
                        (goForward)
                        (recurse ((caar *Dir) This))
                        (do 2 (turnRight))
                        (goForward)
                        (do 2 (turnRight)) )
                     (turnRight) ) ) ) )
         (quit "Can't find field") ) ) )

# Visualize (debug)
(de showWorld ()
   (disp *World 0
      '((This)
         (pack " "
            (: field)
            (if (: ball) (lowc @) " ") ) ) ) )
```

Output:

```txt
: (gameClient "picolisp.com" 54545)
Initial state:
   +---+---+---+---+---+---+---+---+
 8 | G   G   Y   Yr| Y   Yb  G   R |
   +   +---+---+   +   +---+---+   +
 7 | Y | Y | B   Gy  Bg  Y   B | Gg|
   +---+   +   +---+   +   +   +   +
 6 | Gb| Gy  G   R   B   Y | B   Bg|
   +   +---+   +   +---+---+---+   +
 5 | R | B   G | B | R | B   R   Yg|
   +   +---+   +   +   +   +   +   +
 4 | B   B | G | Y   B   Bg| Bg  R |
   +---+   +   +---+   +   +   +   +
 3 | G | Y   Gr  R | B   B   Br  B |
   +   +   +---+---+---+   +   +---+
 2 | G   Rr  B | Gy  Y | Bg| Bb  B |
   +---+   +---+   +   +   +   +   +
 1 | R   R   Gb| Bg| G   G   R | Yg|
   +---+---+---+---+---+---+---+---+
     a   b   c   d   e   f   g   h
Moving balls ... GBGRYYBBRGGGYGRGG ... Game over!
Final state:
   +---+---+---+---+---+---+---+---+
 8 | G   Gg  Y   Y | Y   Y   Gg  R |
   +   +---+---+   +   +---+---+   +
 7 | Y | Yy| B   Gg  B   Yy  B | Gg|
   +---+   +   +---+   +   +   +   +
 6 | G | Gg  Gg  R   Bb  Y | B   B |
   +   +---+   +   +---+---+---+   +
 5 | Rr| B   G | B | Rr| B   R   Y |
   +   +---+   +   +   +   +   +   +
 4 | Bb  Bb| G | Y   B   B | B   R |
   +---+   +   +---+   +   +   +   +
 3 | G | Y   G   Rr| B   B   B   B |
   +   +   +---+---+---+   +   +---+
 2 | G   Rr  B | G   Yy| B | Bb  B |
   +---+   +---+   +   +   +   +   +
 1 | R   R   G | B | Gg  Gg  R | Y |
   +---+---+---+---+---+---+---+---+
     a   b   c   d   e   f   g   h
```

