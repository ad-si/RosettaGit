+++
title = "21 Game"
description = ""
date = 2019-08-28T13:56:25Z
aliases = []
[extra]
id = 21960
[taxonomies]
categories = ["Games", "Puzzles", "task"]
tags = []
+++

## Task

**21**   is a two player game,   the game is played by choosing a 
number   (**1**,   **2**,   or   **3**)   to be added to the   *running total*.

The game is won by the player whose chosen number causes the   *running total*   to reach   *exactly*   **21**.

The   *running total*   starts at zero.

One player will be the computer.    Players alternate supplying a number to be added to the   *running total*. 


### Task:
Write a computer program that will:
::*   do the prompting   (or provide a button menu), 
::*   check for errors and display appropriate error messages, 
::*   do the additions   (add a chosen number to the   *running total*), 
::*   display the   *running total*, 
::*   provide a mechanism for the player to    quit/exit/halt/stop/close   the program,
::*   issue a notification when there is a winner,   and
::*   determine who goes first   (possibly a random choice, or it can be specified when the game begins). 




__TOC__    <!--      to be removed later when there is at least four computer programming languages.    !-->  


## Go

To give the human player a reasonable chance whoever goes first, the computer always chooses randomly when the running total is below 18 but otherwise chooses the exact number needed to win the game.

```go
package main

import (
    "bufio"
    "fmt"
    "log"
    "math/rand"
    "os"
    "strconv"
    "time"
)

var scanner = bufio.NewScanner(os.Stdin)

var (
    total = 0
    quit  = false
)

func itob(i int) bool {
    if i == 0 {
        return false
    }
    return true
}

func getChoice() {
    for {
        fmt.Print("Your choice 1 to 3 : ")
        scanner.Scan()
        if scerr := scanner.Err(); scerr != nil {
            log.Fatalln(scerr, "when choosing number")
        }
        text := scanner.Text()
        if text == "q" || text == "Q" {
            quit = true
            return
        }
        input, err := strconv.Atoi(text)
        if err != nil {
            fmt.Println("Invalid number, try again")
            continue
        }
        newTotal := total + input
        switch {
        case input < 1 || input > 3:
            fmt.Println("Out of range, try again")
        case newTotal > 21:
            fmt.Println("Too big, try again")
        default:
            total = newTotal
            fmt.Println("Running total is now", total)
            return
        }
    }
}

func main() {
    rand.Seed(time.Now().UnixNano())
    computer := itob(rand.Intn(2))
    fmt.Println("Enter q to quit at any time\n")
    if computer {
        fmt.Println("The computer will choose first")
    } else {
        fmt.Println("You will choose first")
    }
    fmt.Println("\nRunning total is now 0\n")
    var choice int
    for round := 1; ; round++ {
        fmt.Printf("ROUND %d:\n\n", round)
        for i := 0; i < 2; i++ {
            if computer {
                if total < 18 {
                    choice = 1 + rand.Intn(3)
                } else {
                    choice = 21 - total
                }
                total += choice
                fmt.Println("The computer chooses", choice)
                fmt.Println("Running total is now", total)
                if total == 21 {
                    fmt.Println("\nSo, commiserations, the computer has won!")
                    return
                }
            } else {
                getChoice()
                if quit {
                    fmt.Println("OK, quitting the game")
                    return
                }
                if total == 21 {
                    fmt.Println("\nSo, congratulations, you've won!")
                    return
                }
            }
            fmt.Println()
            computer = !computer
        }
    }
}
```


### Output
A sample game where the human player manages to win even though the computer (chosen randomly) goes first.

```txt

Enter q to quit at any time

The computer will choose first

Running total is now 0

ROUND 1:

The computer chooses 1
Running total is now 1

Your choice 1 to 3 : 3
Running total is now 4

ROUND 2:

The computer chooses 2
Running total is now 6

Your choice 1 to 3 : 4
Out of range, try again
Your choice 1 to 3 : 3
Running total is now 9

ROUND 3:

The computer chooses 2
Running total is now 11

Your choice 1 to 3 : 2
Running total is now 13

ROUND 4:

The computer chooses 2
Running total is now 15

Your choice 1 to 3 : 2
Running total is now 17

ROUND 5:

The computer chooses 1
Running total is now 18

Your choice 1 to 3 : 3
Running total is now 21

So, congratulations, you've won!

```


A sample game where the human player plays sensibly but still loses when the computer goes first.

```txt

Enter q to quit at any time

The computer will choose first

Running total is now 0

ROUND 1:

The computer chooses 3
Running total is now 3

Your choice 1 to 3 : 3
Running total is now 6

ROUND 2:

The computer chooses 3
Running total is now 9

Your choice 1 to 3 : 3
Running total is now 12

ROUND 3:

The computer chooses 1
Running total is now 13

Your choice 1 to 3 : 3
Running total is now 16

ROUND 4:

The computer chooses 1
Running total is now 17

Your choice 1 to 3 : 1
Running total is now 18

ROUND 5:

The computer chooses 3
Running total is now 21

So, commiserations, the computer has won!

```



## Julia

The computer or player can always win if they go first and choose a number that brings the total to one of the following: 1, 5, 9, 13, or 17. To make things vary more, there is a choice of computer play level at the start. The computer will randomly respond with level 1 and choose a random response 1/4 of the time at level 2. 

```julia

function trytowin(n)
    if 21 - n < 4
        println("Computer chooses $(21 - n) and wins. GG!")
        exit(0)
    end
end

function choosewisely(n)
    trytowin(n)
    targets = [1, 5, 9, 13, 17, 21]
    pos = findfirst(x -> x > n, targets)
    bestmove = targets[pos] - n
    if bestmove > 3
        println("Looks like I could lose. Choosing a 1, total now $(n + 1).")
        return n + 1
    end
    println("On a roll, choosing a $bestmove, total now $(n + bestmove).")
    n + bestmove
end

function choosefoolishly(n)
    trytowin(n)
    move = rand([1, 2, 3])
    println("Here goes, choosing $move, total now $(n + move).")
    n + move
end

function choosesemiwisely(n)
    trytowin(n)
    if rand() > 0.75
        choosefoolishly(n)
    else
        choosewisely(n)
    end
end

prompt(s) = (println(s, ": => "); return readline())

function playermove(n)
    rang = (n > 19) ? "1 is all" : ((n > 18) ? "1 or 2" : "1, 2 or 3")
    choice = 0
    while true
        nstr = prompt("Your choice ($rang), 0 to exit")
        if nstr == "0"
            exit(0)
        elseif nstr == "1"
            return n + 1
        elseif nstr == "2" && n < 20
            return n + 2
        elseif nstr == "3" && n < 19
            return n + 3
        end
    end
end


function play21game()
    n = 0
    level = prompt("Level of play (1=dumb, 3=smart)")
    algo = choosewisely
    if level == "1"
        algo = choosefoolishly
    elseif level == "2"
        algo = choosesemiwisely
    elseif level != "3"
        println("Bad choice syntax--default to smart choice")
    end
    whofirst = prompt("Does computer go first? (y or n)")
    if whofirst[1] == 'y' || whofirst[1] == 'Y'
        n = algo(n)
    end
    while n < 21
        n = playermove(n)
        if n == 21
            println("Player wins! Game over, gg!")
            break
        end
        n = algo(n)
    end
end

play21game()

```



## Perl

{{trans|Perl 6}}

```perl
print <<'HERE';
The 21 game. Each player chooses to add 1, 2, or 3 to a running total.
The player whose turn it is when the total reaches 21 wins. Enter q to quit.
HERE

my $total = 0;

while () {
    print "Running total is: $total\n";
    my ($me,$comp);
    while () {
        print 'What number do you play> ';
        $me = <>; chomp $me;
        last if $me =~ /^[123]$/;
        insult($me);
    }
    $total += $me;
    win('Human') if $total >= 21;
    print "Computer plays: " . ($comp = 1+int(rand(3))) . "\n";
    $total += $comp;
    win('Computer') if $total >= 21;
}

sub win {
    my($player) = @_;
    print "$player wins.\n";
    exit;
}

sub insult {
    my($g) = @_;
    exit if $g =~ /q/i;
    my @insults = ('Yo mama', 'Jeez', 'Ummmm', 'Grow up');
    my $i = $insults[1+int rand($#insults)];
    print "$i, $g is not an integer between 1 and 3...\n"
}
```

### Output

```txt
The 21 game. Each player chooses to add 1, 2, or 3 to a running total.
The player whose turn it is when the total reaches 21 wins. Enter q to quit.
Running total is: 0
What number do you play> 3
Computer plays: 3
Running total is: 6
What number do you play> 3
Computer plays: 3
Running total is: 12
What number do you play> 3
Computer plays: 2
Running total is: 17
What number do you play> 1
Computer plays: 1
Running total is: 19
What number do you play> 2
Human wins.
```



## Perl 6

*Works with: Rakudo 2018.09*
Since there is no requirement that the computer play sensibly, it always plays a random guess so the player has some chance to win.

```perl6
say qq :to 'HERE';
    The 21 game. Each player chooses to add 1, 2, or 3 to a running total.
    The player whose turn it is when the total reaches 21 wins. Enter q to quit.
    HERE

my $total = 0;
loop {
    say "Running total is: $total";
    my ($me,$comp);
    loop {
        $me = prompt 'What number do you play> ';
        last if $me ~~ /^<[123]>$/;
        insult $me;
    }
    $total += $me;
    win('Human') if $total >= 21;
    say "Computer plays: { $comp = (1,2,3).roll }\n";
    $total += $comp;
    win('Computer') if $total >= 21;
}

sub win ($player) {
    say "$player wins.";
    exit;
}

sub insult ($g) {
    exit if $g eq 'q';
    print ('Yo mama,', 'Jeez,', 'Ummmm,', 'Grow up,', 'Did you even READ the instructions?').roll;
    say " $g is not an integer between 1 & 3..."
}
```

### Sample game

```txt
The 21 game. Each player chooses to add 1, 2, or 3 to a running total.
The player whose turn it is when the total reaches 21 wins. Enter q to quit.

Running total is: 0
What number do you play> 5
Did you even READ the instructions? 5 is not an integer between 1 & 3...
What number do you play> g
Yo mama, g is not an integer between 1 & 3...
What number do you play> 12
Jeez, 12 is not an integer between 1 & 3...
What number do you play> 3
Computer plays: 2

Running total is: 5
What number do you play> 3
Computer plays: 3

Running total is: 11
What number do you play> 1
Computer plays: 1

Running total is: 13
What number do you play> 3
Computer plays: 2

Running total is: 18
What number do you play> 3
Human wins.
```



## Phix

If the computer goes first you cannot win.

Once the computer stops displaying "no clear strategy" you cannot win.

The computer_first flag only applies to the first game. After winning, losing, or conceding, you go first.

```Phix
bool computer_first = false
bool show_spoiler = false

integer total = 0

procedure computer_play()
    integer move = 0
    for i=1 to 3 do
        if mod(total+i,4)=1 then
            move = i
            exit
        end if
    end for
    if move=0 then
        puts(1,"no clear strategy\n")
        move = rand(min(3,21-total))
    end if
    printf(1,"Total is %d. I play %d.\n",{total,move})
    total += move
    if total=21 then
        puts(1,"21! I win!\n")
    end if
end procedure

puts(1,"\n21 game\n\n")
puts(1,"Press escape or q to quit the game, c to concede and start a new game from 0\n\n")

if computer_first then
    printf(1,"Total is %d.\n",{total})  
    computer_play()
elsif show_spoiler then
    -- The secret to winning!
    puts(1,sq_sub("Uif!pomz!xbz!up!xjo!jt!qmbz!2!gjstu-!uifo!5.=dpnqvufs!npwf?!fwfsz!ujnf",1)&"\n\n")
end if

while 1 do
    printf(1,"Total is %d. enter 1, 2, or 3: ",{total})
    integer ch = wait_key()
    puts(1,iff(ch=#1B?"esc":ch)&"\n")
    if ch>='1' and ch<='3' then
        ch -= '0'
        if total+ch>21 then
            puts(1,"Too big\n")
        else
            total += ch
            if total=21 then
                puts(1,"21! You win!\n")
            else
                computer_play()
            end if
        end if
    elsif ch=#1B or lower(ch)='q' then
        puts(1,"Quitting\n")
        exit
    end if
    if lower(ch)='c' or total=21 then
        total = 0
    end if
end while
```

### Output

```txt

21 game

Press escape or q to quit the game, c to concede and start a new game from 0

Total is 0. enter 1, 2, or 3: 1
no clear strategy
Total is 1. I play 3.
Total is 4. enter 1, 2, or 3: 1
no clear strategy
Total is 5. I play 1.
Total is 6. enter 1, 2, or 3: 3
no clear strategy
Total is 9. I play 1.
Total is 10. enter 1, 2, or 3: 1
Total is 11. I play 2.
Total is 13. enter 1, 2, or 3: 3
Total is 16. I play 1.
Total is 17. enter 1, 2, or 3: 2
Total is 19. I play 2.
21! I win!
Total is 0. enter 1, 2, or 3: q
Quitting

```



## Racket



```racket
#lang racket

(define limit 21)
(define max-resp 3)

(define (get-resp)
  (let loop ()
    (match (read-line)
      [(app (conjoin string? string->number) n)
       #:when (and (exact-integer? n) (<= 1 n max-resp))
       n]
      ["q" (exit)]
      [n (printf "~a is not in range 1 and ~a\n" n max-resp)
         (loop)])))

(define (win human?) (printf "~a wins\n" (if human? "Human" "Computer")))

(printf "The ~a game. Each player chooses to add a number
in range 1 and ~a to a running total.
The player whose turn it is when the total reaches exactly ~a wins.
Enter q to quit.\n\n" limit max-resp limit)

(let loop ([total 0] [human-turn? (= 0 (random 2))])
  (define new-total
    (+ total
       (cond
         [human-turn? (printf "Running total is: ~a\n" total)
                      (printf "Your turn:\n")
                      (get-resp)]
         [else (define resp (random 1 (add1 max-resp)))
               (printf "Computer plays: ~a\n" resp)
               resp])))
  (cond
    [(= new-total limit) (win human-turn?)]
    [(> new-total limit) (win (not human-turn?))]
    [else (loop new-total (not human-turn?))]))
```


### Output

```txt

The 21 game. Each player chooses to add an integer
in range 1 and 3 to a running total.
The player whose turn it is when the total reaches exactly 21 wins.
Enter q to quit.

Running total is: 0
Your turn:
1
Computer plays: 3
Running total is: 4
Your turn:
foo
foo is not an integer in range 1 and 3
Your turn:
bar
bar is not an integer in range 1 and 3
Your turn:
3
Computer plays: 1
Running total is: 8
Your turn:
1
Computer plays: 1
Running total is: 10
Your turn:
1
Computer plays: 2
Running total is: 13
Your turn:
1
Computer plays: 3
Running total is: 17
Your turn:
1
Computer plays: 2
Running total is: 20
Your turn:
1
Human wins

```



## REXX

Around half of the REXX program deals with incorrect or missing input   (of the required integer).

```rexx
/*REXX program plays the  21  game with a human,  each player chooses 1, 2, or 3  which */
/*──────────── is added to the current sum, the first player to reach  21  exactly wins.*/
sep= copies('─', 8);  sep2= " "copies('═', 8)" " /*construct an eye─catching msg fences.*/
say sep 'Playing the  21  game.'                 /*tell what's happening here at the zoo*/
high=  3                                         /*the highest integer that can be used.*/
game= 21                                         /*the game target  (winning total/sum).*/
$= 0                                             /*the sum [or running total]  (so far).*/
    do j=1  while $<game;     bad= 0             /*obtain optional move from ARG or ask.*/
    low = (j \== 1)                              /*allow user to have computer go first.*/
    say;     say sep  'Please enter a number from ' low " ───► 3               (or Quit):"
    if j==1  then say sep '[A value of 0 (zero) means you want the computer to go first.]'
    pull x _ . 1 ax                              /*obtain an uppercased answer & others.*/
    if   x=''             then call ser "Nothing entered."
    if _\==''             then call ser "Too many arguments entered: "       ax
    if abbrev('QUIT', x, 1)  then do;   say;      say sep  "quitting.";      exit 1
                                  end
    if \datatype(x, 'N')  then call ser "Argument isn't numeric: "            x
    if \datatype(x, 'W')  then call ser "Number isn't an integer: "           x
    if x<0                then call ser "Number can't be negative: "          x
    if x=0  &  j>1        then call ser "Number can't be zero: "              x
    if x>high             then call ser "Number is too large  (>"high'): '    x
    if bad                then iterate           /*Had an error? Then get another number*/
    x= x / 1                                     /*normalize the  X  number;  +2 001 3. */
    if $+x > game         then call ser "Number will cause the sum to exceed " g': '   x
    if x\==0              then call tot x, 1     /*Not 0?   The user wants to go first. */
    if $==game            then leave             /*the user won the game with the last #*/
    y=.                                          /*indicate that no choice has been made*/
         do c=1  for high  until y\==.           /*find a # that yields the best choice.*/
         if (c+$) // (1+high) == 1  then y= c    /*a bit of a highfalutin calculation.  */
         end   /*c*/
    if y==.  then y= random(1, high)             /*get random choice (maybe a lost game)*/
    say sep 'The computer chooses '     y     " as it's choice."        /*inform player.*/
    call tot y, 0                                /*call subroutine to show the total.   */
    end   /*j*/
say
if who  then say sep  'Congratulations!   You have won the  21  game.'
        else say sep  'The computer has won the  21  game.'
exit 0                                           /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
ser: if bad  then return;  bad=1;  say;  say;  say sep '***error***' arg(1);  say;  return
tot: arg q,who; $=$+q; say sep 'The game total is now' sep2 $ sep2; return /*add; show $*/
```

### output

```txt

──────── Playing the  21  game.

──────── Please enter a number from  0 ───► 3               (or Quit):
──────── [A value of 0 (zero) means you want the computer to go first.]
3                                                                         ◄■■■■■■■■■■ user input
──────── The game total is now  ════════  3  ════════
──────── The computer chooses  1  as it's choice.
──────── The game total is now  ════════  4  ════════

──────── Please enter a number from  1 ───► 3               (or Quit):
3                                                                          ◄■■■■■■■■■■ user input
──────── The game total is now  ════════  7  ════════
──────── The computer chooses  1  as it's choice.
──────── The game total is now  ════════  8  ════════

──────── Please enter a number from  1 ───► 3               (or Quit):
3                                                                          ◄■■■■■■■■■■ user input
──────── The game total is now  ════════  11  ════════
──────── The computer chooses  1  as it's choice.
──────── The game total is now  ════════  12  ════════

──────── Please enter a number from  1 ───► 3               (or Quit):
3                                                                          ◄■■■■■■■■■■ user input
──────── The game total is now  ════════  15  ════════
──────── The computer chooses  1  as it's choice.
──────── The game total is now  ════════  16  ════════

──────── Please enter a number from  1 ───► 3               (or Quit):
1                                                                          ◄■■■■■■■■■■ user input
──────── The game total is now  ════════  17  ════════
──────── The computer chooses  1  as it's choice.
──────── The game total is now  ════════  18  ════════

──────── Please enter a number from  1 ───► 3               (or Quit):
3                                                                          ◄■■■■■■■■■■ user input 
──────── The game total is now  ════════  21  ════════

──────── Congratulations!   You have won the  21  game.

```



## Ring


```ring

# Project : 21 Game

load "guilib.ring"

limit = 21
posold = 0
button = list(limit)
mynum = list(3)
yournum = list(3)

new qapp 
        {
        win1 = new qwidget() {
                  setwindowtitle("21 Game")
                  setgeometry(100,100,1000,600)
                  label1 = new qlabel(win1) {
                              setgeometry(10,10,1000,600)
                              settext("")
                  }

                  label2 = new qlabel(win1) {
                              setgeometry(240,50,120,40)
                              setAlignment(Qt_AlignHCenter)
                              setFont(new qFont("Verdana",12,100,0))
                              settext("my number:")
                  }

                  label3 = new qlabel(win1) {
                              setgeometry(640,50,120,40)
                              setAlignment(Qt_AlignHCenter)
                              setFont(new qFont("Verdana",12,100,0))
                              settext("your number:")
                  }

                  for p = 1 to 3
                       mynum[p] = new qpushbutton(win1) {
                                          setgeometry(200+p*40,100,40,40)
                                          setstylesheet("background-color:orange")
                                          settext(string(p))
                                          setclickevent("choose(" + string(p) + ",1)")
                                          }
                   next

                   for p = 1 to 3
                        yournum[p] = new qpushbutton(win1) {
                                             setgeometry(600+p*40,100,40,40)
                                             setstylesheet("background-color:white")
                                             settext(string(p))
                                             setclickevent("choose(" + string(p) + ",2)")
                                             }
                   next

                   for n = 1 to limit
                        button[n] = new qpushbutton(win1) {
                                          setgeometry(40+n*40,190,40,40)
                                          settext(string(n))
                                          }
                    next
                    show()
        }
        exec()
        }

func choose(ch,ym)
        pos = posold + ch
        if pos > limit
           msg = "You must choose number from 1 to " + string(limit - posold)
           msgBox(msg)
           for n = 1 to 3
                mynum[n].setenabled(false)
                yournum[n].setenabled(false)
           next
           return
        ok
        for n = posold+1 to pos
             if ym = 1
                button[n] { setstylesheet("background-color:orange") }
             else
                button[n] { setstylesheet("background-color:white") }
             ok
        next
        posold = pos
        if ym = 1
           for n = 1 to 3
                mynum[n].setenabled(false)
                yournum[n].setenabled(true)
           next
         else
           for n = 1 to 3
                mynum[n].setenabled(true)
                yournum[n].setenabled(false)
           next
         ok
         if pos = 21
            if ym = 1
               msgBox("I won!")
            else
               msgBox("You won!")
            ok
         ok

func msgBox(text) {
	m = new qMessageBox(win1) {
	       setWindowTitle("21 Game")
	       setText(text)
	       show()
	       }
        }

```

Output:

[21 Game](http://kepkezelo.com/images/1hyutakip6vo1t1rpypy.jpg)
