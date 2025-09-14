+++
title = "Dining philosophers"
description = ""
date = 2019-08-05T18:51:16Z
aliases = []
[extra]
id = 3098
[taxonomies]
categories = ["task", "Concurrency"]
tags = []
languages = [
  "ada",
  "autohotkey",
  "bbc_basic",
  "c",
  "clojure",
  "common_lisp",
  "cpp",
  "csharp",
  "d",
  "e",
  "echolisp",
  "eiffel",
  "elixir",
  "erlang",
  "euphoria",
  "go",
  "groovy",
  "haskell",
  "j",
  "java",
  "jocaml",
  "julia",
  "kotlin",
  "logtalk",
  "m2000_interpreter",
  "nim",
  "oxygenbasic",
  "oz",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "picolisp",
  "pike",
  "prolog",
  "purebasic",
  "python",
  "racket",
  "rexx",
  "ruby",
  "rust",
  "simula",
  "smalltalk",
  "tcl",
  "vba",
  "visual_basic_dotnet",
  "zkl",
]
+++

## Task

The dining philosophers problem illustrates non-composability of low-level synchronization primitives like [[semaphore]]s. It is a modification of a problem posed by [https://en.wikipedia.org/wiki/Edsger_W._Dijkstra Edsger Dijkstra.]

Five philosophers, Aristotle, Kant, Spinoza, Marx, and Russell (the [[task]]s) spend their time thinking and eating spaghetti. They eat at a round table with five individual seats. For eating each philosopher needs two forks (the resources). There are five forks on the table, one left and one right of each seat. When a philosopher cannot grab both forks it sits and waits. Eating takes random time, then the philosopher puts the forks down and leaves the dining room. After spending some random time thinking about the nature of the universe, he again becomes hungry, and the circle repeats itself.

It can be observed that a straightforward solution, when forks are implemented by [[semaphore]]s, is exposed to deadlock. There exist two deadlock states when all five philosophers are sitting at the table holding one fork each. One deadlock state is when each philosopher has grabbed the fork left of him, and another is when each has the fork on his right.

There are many solutions of the problem, program at least one, and explain how the deadlock is prevented.


## Ada



### Array of mutexes

The following solution uses an array of [[mutex]]es in order to model the forks. The forks used by a philosopher compose a subset in the array. When the the philosopher seizes his forks from the subset the array object prevents deadlocking since it is an atomic operation.

```ada
with Ada.Numerics.Float_Random;  use Ada.Numerics.Float_Random;
with Ada.Text_IO;                use Ada.Text_IO;

with Synchronization.Generic_Mutexes_Array;

procedure Test_Dining_Philosophers is
   type Philosopher is (Aristotle, Kant, Spinoza, Marx, Russel);
   package Fork_Arrays is new Synchronization.Generic_Mutexes_Array (Philosopher);
   use Fork_Arrays;

   Life_Span : constant := 20;    -- In his life a philosopher eats 20 times
   Forks : aliased Mutexes_Array; -- Forks for hungry philosophers

   function Left_Of (Fork : Philosopher) return Philosopher is
   begin
      if Fork = Philosopher'First then
         return Philosopher'Last;
      else
         return Philosopher'Pred (Fork);
      end if;
   end Left_Of;

   task type Person (ID : Philosopher);
   task body Person is
      Cutlery : aliased Mutexes_Set := ID or Left_Of (ID);
      Dice    : Generator;
   begin
      Reset (Dice);
      for Life_Cycle in 1..Life_Span loop
         Put_Line (Philosopher'Image (ID) & " is thinking");
         delay Duration (Random (Dice) * 0.100);
         Put_Line (Philosopher'Image (ID) & " is hungry");
         declare
            Lock : Set_Holder (Forks'Access, Cutlery'Access);
         begin
            Put_Line (Philosopher'Image (ID) & " is eating");
            delay Duration (Random (Dice) * 0.100);
         end;
      end loop;
      Put_Line (Philosopher'Image (ID) & " is leaving");
   end Person;

   Ph_1 : Person (Aristotle); -- Start philosophers
   Ph_2 : Person (Kant);
   Ph_3 : Person (Spinoza);
   Ph_4 : Person (Marx);
   Ph_5 : Person (Russel);
begin
   null; -- Nothing to do in the main task, just sit and behold
end Test_Dining_Philosophers;
```


### Ordered mutexes

In the following solution forks are implemented as plain [[mutex]]es. The deadlock is prevented by ordering mutexes. Philosopher tasks seize them in same order 1, 2, 3, 4, 5.

```ada
with Ada.Numerics.Float_Random;  use Ada.Numerics.Float_Random;
with Ada.Text_IO;                use Ada.Text_IO;

procedure Test_Dining_Philosophers is
   type Philosopher is (Aristotle, Kant, Spinoza, Marx, Russel);
   protected type Fork is
      entry Grab;
      procedure Put_Down;
   private
      Seized : Boolean := False;
   end Fork;
   protected body Fork is
      entry Grab when not Seized is
      begin
         Seized := True;
      end Grab;
      procedure Put_Down is
      begin
         Seized := False;
      end Put_Down;
   end Fork;

   Life_Span : constant := 20;    -- In his life a philosopher eats 20 times

   task type Person (ID : Philosopher; First, Second : not null access Fork);
   task body Person is
      Dice : Generator;
   begin
      Reset (Dice);
      for Life_Cycle in 1..Life_Span loop
         Put_Line (Philosopher'Image (ID) & " is thinking");
         delay Duration (Random (Dice) * 0.100);
         Put_Line (Philosopher'Image (ID) & " is hungry");
         First.Grab;
         Second.Grab;
         Put_Line (Philosopher'Image (ID) & " is eating");
         delay Duration (Random (Dice) * 0.100);
         Second.Put_Down;
         First.Put_Down;
      end loop;
      Put_Line (Philosopher'Image (ID) & " is leaving");
   end Person;

   Forks : array (1..5) of aliased Fork; -- Forks for hungry philosophers
                                         -- Start philosophers
   Ph_1 : Person (Aristotle, Forks (1)'Access, Forks (2)'Access);
   Ph_2 : Person (Kant,      Forks (2)'Access, Forks (3)'Access);
   Ph_3 : Person (Spinoza,   Forks (3)'Access, Forks (4)'Access);
   Ph_4 : Person (Marx,      Forks (4)'Access, Forks (5)'Access);
   Ph_5 : Person (Russel,    Forks (1)'Access, Forks (5)'Access);
begin
   null; -- Nothing to do in the main task, just sit and behold
end Test_Dining_Philosophers;
```


### Host of the dining room

Both deadlocks happen when all philosophers are in the dining room. The following solution has a host of the room who chatters the last philosopher while four of them are in the room. So there are never more than four of them in there, which prevents deadlock. Now the forks can be picked up in a "wrong" order, i.e. the left one first.

```ada
with Ada.Numerics.Float_Random;  use Ada.Numerics.Float_Random;
with Ada.Text_IO;                use Ada.Text_IO;

procedure Test_Dining_Philosophers is
   type Philosopher is (Aristotle, Kant, Spinoza, Marx, Russel);

   protected type Fork is
      entry Grab;
      procedure Put_Down;
   private
      Seized : Boolean := False;
   end Fork;

   protected Host is
      entry Greet;
      procedure Farewell;
   private
      Guests : Natural := 0;
   end Host;

   protected body Fork is
      entry Grab when not Seized is
      begin
         Seized := True;
      end Grab;
      procedure Put_Down is
      begin
         Seized := False;
      end Put_Down;
   end Fork;

   protected body Host is
      entry Greet when Guests < 5 is
      begin
         Guests := Guests + 1;
      end Greet;
      procedure Farewell is
      begin
         Guests := Guests - 1;
      end Farewell;
   end Host;

   Life_Span : constant := 20;    -- In his life a philosopher eats 20 times

   task type Person (ID : Philosopher; Left, Right : not null access Fork);
   task body Person is
      Dice : Generator;
   begin
      Reset (Dice);
      for Life_Cycle in 1..Life_Span loop
         Put_Line (Philosopher'Image (ID) & " is thinking");
         delay Duration (Random (Dice) * 0.100);
         Put_Line (Philosopher'Image (ID) & " is hungry");
         Host.Greet;
         Left.Grab;
         Right.Grab;
         Put_Line (Philosopher'Image (ID) & " is eating");
         delay Duration (Random (Dice) * 0.100);
         Left.Put_Down;
         Right.Put_Down;
         Host.Farewell;
      end loop;
      Put_Line (Philosopher'Image (ID) & " is leaving");
   end Person;

   Forks : array (1..5) of aliased Fork; -- Forks for hungry philosophers
                                         -- Start philosophers
   Ph_1 : Person (Aristotle, Forks (1)'Access, Forks (2)'Access);
   Ph_2 : Person (Kant,      Forks (2)'Access, Forks (3)'Access);
   Ph_3 : Person (Spinoza,   Forks (3)'Access, Forks (4)'Access);
   Ph_4 : Person (Marx,      Forks (4)'Access, Forks (5)'Access);
   Ph_5 : Person (Russel,    Forks (5)'Access, Forks (1)'Access);
begin
   null; -- Nothing to do in the main task, just sit and behold
end Test_Dining_Philosophers;
```



## AutoHotkey

AutoHotkey doesn't support concurrency, so we fake it with timers.
Deadlock is prevented by releasing a single fork when the other is unobtainable.
Livelock is prevented by randomly trying for the opposite fork first.
Starvation will only occur if one (or more) of the philosophers never stops eating.
Try changing EnoughForks to 4 and fork supply per philosopher to 2.

```ahk
#Persistent
SetWorkingDir, %A_ScriptDir%
FileDelete, output.txt
EnoughForks := 2 ; required forks to begin eating
Fork1 := Fork2 := Fork3 := Fork4 := Fork5 := 1 ; fork supply per philosopher
SetTimer, AristotleWaitForLeftFork
SetTimer, KantWaitForLeftFork
SetTimer, SpinozaWaitForLeftFork
SetTimer, MarxWaitForLeftFork
SetTimer, RussellWaitForLeftFork
Return ;---------------------------------------------------------------

AristotleWaitForLeftFork:
	WaitForFork("Aristotle", "left", Fork1, Fork2, AristotleLeftForkCount, AristotleRightForkCount, AristotleWaitCount, EnoughForks)
Return
AristotleWaitForRightFork:
	WaitForFork("Aristotle", "right", Fork2, Fork1, AristotleRightForkCount, AristotleLeftForkCount, AristotleWaitCount, EnoughForks)
Return
AristotleFinishEating:
	ReturnForks("Aristotle", Fork1, Fork2, AristotleLeftForkCount, AristotleRightForkCount, EnoughForks)
Return

KantWaitForLeftFork:
	WaitForFork("Kant", "left", Fork2, Fork3, KantLeftForkCount, KantRightForkCount, KantWaitCount, EnoughForks)
Return
KantWaitForRightFork:
	WaitForFork("Kant", "right", Fork3, Fork2, KantRightForkCount, KantLeftForkCount, KantWaitCount, EnoughForks)
Return
KantFinishEating:
	ReturnForks("Kant", Fork2, Fork3, KantLeftForkCount, KantRightForkCount, EnoughForks)
Return

SpinozaWaitForLeftFork:
	WaitForFork("Spinoza", "left", Fork3, Fork4, SpinozaLeftForkCount, SpinozaRightForkCount, SpinozaWaitCount, EnoughForks)
Return
SpinozaWaitForRightFork:
	WaitForFork("Spinoza", "right", Fork4, Fork3, SpinozaRightForkCount, SpinozaLeftForkCount, SpinozaWaitCount, EnoughForks)
Return
SpinozaFinishEating:
	ReturnForks("Spinoza", Fork3, Fork4, SpinozaLeftForkCount, SpinozaRightForkCount, EnoughForks)
Return

MarxWaitForLeftFork:
	WaitForFork("Marx", "left", Fork4, Fork5, MarxLeftForkCount, MarxRightForkCount, MarxWaitCount, EnoughForks)
Return
MarxWaitForRightFork:
	WaitForFork("Marx", "right", Fork5, Fork4, MarxRightForkCount, MarxLeftForkCount, MarxWaitCount, EnoughForks)
Return
MarxFinishEating:
	ReturnForks("Marx", Fork4, Fork5, MarxLeftForkCount, MarxRightForkCount, EnoughForks)
Return

RussellWaitForLeftFork:
	WaitForFork("Russell", "left", Fork5, Fork1, RussellLeftForkCount, RussellRightForkCount, RussellWaitCount, EnoughForks)
Return
RussellWaitForRightFork:
	WaitForFork("Russell", "right", Fork1, Fork5, RussellRightForkCount, RussellLeftForkCount, RussellWaitCount, EnoughForks)
Return
RussellFinishEating:
	ReturnForks("Russell", Fork5, Fork1, RussellLeftForkCount, RussellRightForkCount, EnoughForks)
Return

ReturnForks(Philosopher, ByRef ThisFork, ByRef OtherFork, ByRef CurrentThisForkCount, ByRef CurrentOtherForkCount, EnoughForks) {
	OutputDebug, %Philosopher% finishes eating.
	FileAppend, %Philosopher% finishes eating.`n,output.txt
	ThisFork += CurrentThisForkCount ; return this fork
	OtherFork += CurrentOtherForkCount ; return other fork
	CurrentThisForkCount := 0 ; release this fork
	CurrentOtherForkCount := 0 ; release other fork
	OutputDebug, %Philosopher% returns all forks.
	FileAppend, %Philosopher% returns all forks.`n,output.txt

	; do something while resting

	Random, Rand, 0, 1
	Rand := Rand ? "Left" : "Right"
	SetTimer, %Philosopher%WaitFor%Rand%Fork
}

WaitForFork(Philosopher, This, ByRef ThisFork, ByRef OtherFork, ByRef CurrentThisForkCount, ByRef CurrentOtherForkCount, ByRef CurrentWaitCount, EnoughForks) {
	If This not in Left,Right
		Return Error
	Other := (This="right") ? "left" : "right"
	OutputDebug, %Philosopher% is hungry.
	FileAppend, %Philosopher% is hungry.`n,output.txt
	If (ThisFork) ; if this fork available
	{
		SetTimer, %Philosopher%WaitFor%This%Fork, Off
		CurrentWaitCount := 0
		ThisFork-- ; take this fork
		CurrentThisForkCount++ ; receive this fork
		OutputDebug, %Philosopher% grabs %This% fork.
		FileAppend, %Philosopher% grabs %This% fork.`n,output.txt
		If (CurrentThisForkCount + CurrentOtherForkCount = EnoughForks) ; if philosopher has enough forks
		{
			OutputDebug, %Philosopher% starts eating.
			FileAppend, %Philosopher% starts eating.`n,output.txt

			; do something while eating

			SetTimer, %Philosopher%FinishEating, -250
		}
		Else If (EnoughForks=2)
		{
			SetTimer, %Philosopher%WaitFor%Other%Fork
		}
		Else
		{
			Random, Rand, 0, 1
			Rand := Rand ? "Left" : "Right"
			SetTimer, %Philosopher%WaitFor%Rand%Fork
		}
	}
	Else If (CurrentOtherForkCount and CurrentWaitCount > 5) ; if we've been holding other fork too long
	{
		SetTimer, %Philosopher%WaitFor%This%Fork, Off
		CurrentWaitCount := 0
		OtherFork++ ; return other fork
		CurrentOtherForkCount-- ; release other fork
		OutputDebug, %Philosopher% drops %Other% fork.
		FileAppend, %Philosopher% drops %Other% fork.`n,output.txt
		Random, Rand, 0, 1
		Rand := Rand ? "Left" : "Right"
		SetTimer, %Philosopher%WaitFor%Rand%Fork
	}
	Else If (CurrentThisForkCount and CurrentWaitCount > 5) ; if we've been holding one of this fork too long
	{
		SetTimer, %Philosopher%WaitFor%This%Fork, Off
		CurrentWaitCount := 0
		ThisFork++ ; return other fork
		CurrentThisForkCount-- ; release other fork
		OutputDebug, %Philosopher% drops %This% fork.
		FileAppend, %Philosopher% drops %This% fork.`n,output.txt
		Random, Rand, 0, 1
		Rand := Rand ? "Left" : "Right"
		SetTimer, %Philosopher%WaitFor%Rand%Fork
	}
	Else
	{
		CurrentWaitCount++
	}
}
```

<pre style="height:40ex;overflow:scroll">Aristotle is hungry.
Aristotle grabs left fork.
Kant is hungry.
Kant grabs left fork.
Spinoza is hungry.
Spinoza grabs left fork.
Marx is hungry.
Marx grabs left fork.
Russell is hungry.
Russell grabs left fork.
Aristotle is hungry.
Kant is hungry.
Spinoza is hungry.
Marx is hungry.
Russell is hungry.
Aristotle is hungry.
Kant is hungry.
Spinoza is hungry.
Marx is hungry.
Russell is hungry.
Aristotle is hungry.
Kant is hungry.
Spinoza is hungry.
Marx is hungry.
Russell is hungry.
Aristotle is hungry.
Kant is hungry.
Spinoza is hungry.
Marx is hungry.
Russell is hungry.
Aristotle is hungry.
Kant is hungry.
Spinoza is hungry.
Marx is hungry.
Russell is hungry.
Aristotle is hungry.
Kant is hungry.
Spinoza is hungry.
Marx is hungry.
Russell is hungry.
Aristotle is hungry.
Aristotle drops left fork.
Kant is hungry.
Kant drops left fork.
Spinoza is hungry.
Spinoza drops left fork.
Marx is hungry.
Marx drops left fork.
Russell is hungry.
Russell grabs right fork.
Russell starts eating.
Marx is hungry.
Marx grabs left fork.
Aristotle is hungry.
Aristotle grabs right fork.
Kant is hungry.
Kant grabs right fork.
Spinoza is hungry.
Russell finishes eating.
Russell returns all forks.
Aristotle is hungry.
Aristotle grabs left fork.
Aristotle starts eating.
Kant is hungry.
Spinoza is hungry.
Marx is hungry.
Marx grabs right fork.
Marx starts eating.
Russell is hungry.
Kant is hungry.
Spinoza is hungry.
Aristotle finishes eating.
Aristotle returns all forks.
Marx finishes eating.
Marx returns all forks.
Russell is hungry.
Russell grabs left fork.
Kant is hungry.
Spinoza is hungry.
Spinoza grabs right fork.
Marx is hungry.
Russell is hungry.
Russell grabs right fork.
Russell starts eating.
Aristotle is hungry.
Aristotle grabs right fork.
Kant is hungry.
Aristotle is hungry.
Spinoza is hungry.
Marx is hungry.
Russell finishes eating.
Russell returns all forks.
Kant is hungry.
Aristotle is hungry.
Aristotle grabs left fork.
Aristotle starts eating.
Spinoza is hungry.
Marx is hungry.
Marx grabs right fork.
Russell is hungry.
⋮
```



## BBC BASIC

This avoids deadlocks using the same strategy as the C solution (one of the philosophers picks up the left fork first).

```bbcbasic
      INSTALL @lib$+"TIMERLIB"

      nSeats% = 5
      DIM Name$(nSeats%-1), Fork%(nSeats%-1), tID%(nSeats%-1), Leftie%(nSeats%-1)

      Name$() = "Aristotle", "Kant", "Spinoza", "Marx", "Russell"
      Fork%() = TRUE : REM All forks are initially on the table
      Leftie%(RND(nSeats%)-1) = TRUE : REM One philosopher is lefthanded

      tID%(0) = FN_ontimer(10, PROCphilosopher0, 1)
      tID%(1) = FN_ontimer(10, PROCphilosopher1, 1)
      tID%(2) = FN_ontimer(10, PROCphilosopher2, 1)
      tID%(3) = FN_ontimer(10, PROCphilosopher3, 1)
      tID%(4) = FN_ontimer(10, PROCphilosopher4, 1)

      ON CLOSE PROCcleanup : QUIT
      ON ERROR PRINT REPORT$ : PROCcleanup : END

      DEF PROCphilosopher0 : PROCtask(0) : ENDPROC
      DEF PROCphilosopher1 : PROCtask(1) : ENDPROC
      DEF PROCphilosopher2 : PROCtask(2) : ENDPROC
      DEF PROCphilosopher3 : PROCtask(3) : ENDPROC
      DEF PROCphilosopher4 : PROCtask(4) : ENDPROC

      REPEAT
        WAIT 0
      UNTIL FALSE
      END

      DEF PROCtask(n%)
      PRIVATE state%(), lh%(), rh%()
      DIM state%(nSeats%-1), lh%(nSeats%-1), rh%(nSeats%-1)
      REM States: 0 = waiting for forks, > 0 = eating, < 0 = left the room
      CASE TRUE OF
        WHEN state%(n%) < 0:
          state%(n%) += 1 : REM Waiting to get hungry again
          IF state%(n%) = 0 PRINT Name$(n%) " is hungry again"
        WHEN state%(n%) > 0:
          state%(n%) -= 1 : REM Eating
          IF state%(n%) = 0 THEN
            SWAP Fork%((n%-1+nSeats%) MOD nSeats%), lh%(n%)
            SWAP Fork%((n% + 1) MOD nSeats%), rh%(n%)
            state%(n%) = -RND(100)
            PRINT Name$(n%) " is leaving the room"
          ENDIF
        WHEN state%(n%) = 0:
          IF Leftie%(n%) THEN
            IF NOT lh%(n%) SWAP Fork%((n%-1+nSeats%) MOD nSeats%), lh%(n%)
            IF lh%(n%) IF NOT rh%(n%) SWAP Fork%((n% + 1) MOD nSeats%), rh%(n%)
          ELSE
            IF NOT rh%(n%) SWAP Fork%((n% + 1) MOD nSeats%), rh%(n%)
            IF rh%(n%) IF NOT lh%(n%) SWAP Fork%((n%-1+nSeats%) MOD nSeats%), lh%(n%)
          ENDIF
          IF lh%(n%) AND rh%(n%) THEN
            state%(n%) = RND(100)
            PRINT Name$(n%) " is eating (" ; state%(n%) " ticks)"
          ENDIF
      ENDCASE
      ENDPROC

      DEF PROCcleanup
      LOCAL I%
      FOR I% = 0 TO nSeats%-1
        PROC_killtimer(tID%(I%))
      NEXT
      ENDPROC
```

'''Sample output:'''

```txt

Russell is eating (92 ticks)
Marx is eating (94 ticks)
Russell is leaving the room
Spinoza is eating (96 ticks)
Marx is leaving the room
Kant is eating (40 ticks)
Marx is hungry again
Kant is leaving the room
Kant is hungry again
Russell is hungry again
Spinoza is leaving the room
Aristotle is eating (30 ticks)
Aristotle is leaving the room
Marx is eating (19 ticks)
Spinoza is hungry again
Marx is leaving the room
Kant is eating (20 ticks)
Marx is hungry again
Aristotle is hungry again
Kant is leaving the room
Russell is eating (100 ticks)
Marx is eating (7 ticks)
Marx is leaving the room
Kant is hungry again
Marx is hungry again
Russell is leaving the room
Spinoza is eating (7 ticks)
Kant is eating (82 ticks)
Spinoza is leaving the room
Aristotle is eating (74 ticks)

```



## C

Avoid deadlocks by making each philosopher have a different order of picking up forks.  As long as one person waits for left fork first and another waits for right first, cycles can't form.

```c
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdarg.h>

#define N 5
const char *names[N] = { "Aristotle", "Kant", "Spinoza", "Marx", "Russell" };
pthread_mutex_t forks[N];

#define M 5 /* think bubbles */
const char *topic[M] = { "Spaghetti!", "Life", "Universe", "Everything", "Bathroom" };

#define lock pthread_mutex_lock
#define unlock pthread_mutex_unlock
#define xy(x, y) printf("\033[%d;%dH", x, y)
#define clear_eol(x) print(x, 12, "\033[K")
void print(int y, int x, const char *fmt, ...)
{
	static pthread_mutex_t screen = PTHREAD_MUTEX_INITIALIZER;
	va_list ap;
	va_start(ap, fmt);

	lock(&screen);
	xy(y + 1, x), vprintf(fmt, ap);
	xy(N + 1, 1), fflush(stdout);
	unlock(&screen);
}

void eat(int id)
{
	int f[2], ration, i; /* forks */
	f[0] = f[1] = id;

	/* make some (but not all) philosophers leftie.
	   could have been f[!id] = (id + 1) %N; for example */
	f[id & 1] = (id + 1) % N;

	clear_eol(id);
	print(id, 12, "..oO (forks, need forks)");

	for (i = 0; i < 2; i++) {
		lock(forks + f[i]);
		if (!i) clear_eol(id);

		print(id, 12 + (f[i] != id) * 6, "fork%d", f[i]);
		/* delay 1 sec to clearly show the order of fork acquisition */
		sleep(1);
	}

	for (i = 0, ration = 3 + rand() % 8; i < ration; i++)
		print(id, 24 + i * 4, "nom"), sleep(1);

	/* done nomming, give up forks (order doesn't matter) */
	for (i = 0; i < 2; i++) unlock(forks + f[i]);
}

void think(int id)
{
	int i, t;
	char buf[64] = {0};

	do {
		clear_eol(id);
		sprintf(buf, "..oO (%s)", topic[t = rand() % M]);

		for (i = 0; buf[i]; i++) {
			print(id, i+12, "%c", buf[i]);
			if (i < 5) usleep(200000);
		}
		usleep(500000 + rand() % 1000000);
	} while (t);
}

void* philosophize(void *a)
{
	int id = *(int*)a;
	print(id, 1, "%10s", names[id]);
	while(1) think(id), eat(id);
}

int main()
{
	int i, id[N];
	pthread_t tid[N];

	for (i = 0; i < N; i++)
		pthread_mutex_init(forks + (id[i] = i), 0);

	for (i = 0; i < N; i++)
		pthread_create(tid + i, 0, philosophize, id + i);

	/* wait forever: the threads don't actually stop */
	return pthread_join(tid[0], 0);
}
```


This uses a modified version of the Python algorithm version below.  Uses POSIX threads.

```C>#include <pthread.h

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

typedef struct philData {
    pthread_mutex_t *fork_lft, *fork_rgt;
    const char *name;
    pthread_t thread;
    int   fail;
} Philosopher;

int running = 1;

void *PhilPhunction(void *p) {
    Philosopher *phil = (Philosopher*)p;
    int failed;
    int tries_left;
    pthread_mutex_t *fork_lft, *fork_rgt, *fork_tmp;

    while (running) {
        printf("%s is sleeping --er thinking\n", phil->name);
        sleep( 1+ rand()%8);

        fork_lft = phil->fork_lft;
        fork_rgt = phil->fork_rgt;
        printf("%s is hungry\n", phil->name);
        tries_left = 2;   /* try twice before being forceful */
        do {
            failed = pthread_mutex_lock( fork_lft);
            failed = (tries_left>0)? pthread_mutex_trylock( fork_rgt )
                                   : pthread_mutex_lock(fork_rgt);
            if (failed) {
                pthread_mutex_unlock( fork_lft);
                fork_tmp = fork_lft;
                fork_lft = fork_rgt;
                fork_rgt = fork_tmp;
                tries_left -= 1;
            }
        } while(failed && running);

        if (!failed) {
            printf("%s is eating\n", phil->name);
            sleep( 1+ rand() % 8);
            pthread_mutex_unlock( fork_rgt);
            pthread_mutex_unlock( fork_lft);
        }
    }
    return NULL;
}

void Ponder()
{
    const char *nameList[] = { "Kant", "Guatma", "Russel", "Aristotle", "Bart" };
    pthread_mutex_t forks[5];
    Philosopher philosophers[5];
    Philosopher *phil;
    int i;
    int failed;

    for (i=0;i<5; i++) {
        failed = pthread_mutex_init(&forks[i], NULL);
        if (failed) {
            printf("Failed to initialize mutexes.");
            exit(1);
        }
    }

    for (i=0;i<5; i++) {
        phil = &philosophers[i];
        phil->name = nameList[i];
        phil->fork_lft = &forks[i];
        phil->fork_rgt = &forks[(i+1)%5];
        phil->fail = pthread_create( &phil->thread, NULL, PhilPhunction, phil);
    }

    sleep(40);
    running = 0;
    printf("cleanup time\n");

    for(i=0; i<5; i++) {
        phil = &philosophers[i];
        if ( !phil->fail && pthread_join( phil->thread, NULL) ) {
            printf("error joining thread for %s", phil->name);
            exit(1);
        }
    }
}

int main()
{
    Ponder();
    return 0;
}
```


This version uses C11 threads and the approach of making one of the philosophers left-handed to avoid deadlock.

```c
#include <stdio.h>
#include <threads.h>
#include <stdlib.h>

#define NUM_THREADS 5

struct timespec time1;
mtx_t forks[NUM_THREADS];

typedef struct {
	char *name;
	int left;
	int right;
} Philosopher;

Philosopher *create(char *nam, int lef, int righ) {
	Philosopher *x = malloc(sizeof(Philosopher));
	x->name = nam;
	x->left = lef;
	x->right = righ;
	return x;
}

int eat(void *data) {
	time1.tv_sec = 1;
	Philosopher *foo = (Philosopher *) data;
	mtx_lock(&forks[foo->left]);
	mtx_lock(&forks[foo->right]);
	printf("%s is eating\n",  foo->name);
	thrd_sleep(&time1, NULL);
	printf("%s is done eating\n",  foo->name);
	mtx_unlock(&forks[foo->left]);
	mtx_unlock(&forks[foo->right]);
	return 0;
}

int main(void) {
    thrd_t threadId[NUM_THREADS];
	Philosopher *all[NUM_THREADS] = {create("Teral", 0 ,1),
					create("Billy", 1, 2),
					create("Daniel", 2,3),
					create("Philip", 3, 4),
					create("Bennet", 0, 4)};
	for (int i = 0; i < NUM_THREADS; i++){
		if (mtx_init(&forks[i], mtx_plain) != thrd_success){
			puts("FAILED IN MUTEX INIT!");
			return 0;
		}
	}
    for (int i=0; i < NUM_THREADS; ++i) {
        if (thrd_create(threadId+i, eat, all[i]) != thrd_success) {
            printf("%d-th thread create error\n", i);
            return 0;
        }
    }

    for (int i=0; i < NUM_THREADS; ++i)
        thrd_join(threadId[i], NULL);
    return 0;
}

```



## C++


Compile with (something like): '''g++ -Wall -lboost_thread dining_philosophers.cpp -o dining_philosophers'''

```cpp

#include <vector>
#include <string>
#include <iostream>
#include <boost/cstdint.hpp>
#include <boost/thread.hpp>
#include <boost/thread/locks.hpp>
#include <boost/format.hpp>
#include <boost/shared_ptr.hpp>

typedef boost::mutex Fork;
typedef boost::shared_ptr< Fork > ForkPtr;
typedef boost::lock_guard< Fork > ForkLock;

#define MIN_WAIT_TIME 100
#define NUM_MEALS     10
#define MAX_JITTER    50

template< typename Stream >
class AtomicLogger {
public:

  AtomicLogger( Stream& stream ) :
    m_mutex(),
    m_stream( stream )
  {
  }

  void log( const std::string& str ) {
    boost::mutex::scoped_lock lock( m_mutex );
    m_stream << str << std::endl;
  }

private:
  mutable boost::mutex m_mutex;
  Stream& m_stream;
};
typedef AtomicLogger< std::ostream > AtomicLoggerOstream;
typedef boost::shared_ptr< AtomicLoggerOstream > AtomicLoggerOstreamPtr;

class Philosopher {
public:

  Philosopher(
	      const std::string& name,
	      ForkPtr fork_left,
	      ForkPtr fork_right,
	      AtomicLoggerOstreamPtr p_logger ) :
    m_name( name ),
    m_continue( true ),
    mp_fork_left( fork_left ),
    mp_fork_right( fork_right ),
    m_thread( boost::thread( boost::bind( &Philosopher::thread_func,
					  this,
					  &m_continue,
					  mp_fork_left,
					  mp_fork_right ) ) ),
    m_meals_left( NUM_MEALS ),
    mp_logger( p_logger )
  {
  }

  ~Philosopher() {
    done_dining();
    wait_for_cmplt();
  }

  void done_dining() { m_continue = false; }

  void wait_for_cmplt() { m_thread.join(); }

private:
  inline bool can_grab_fork( ForkPtr& p_fork ) { return p_fork->try_lock(); }

  void thread_func( volatile bool* p_continue, ForkPtr fork_left, ForkPtr fork_right ) {
    bool failed_to_grab_fork = false;

    while( p_continue && m_meals_left ) {
      mp_logger->log( boost::str( boost::format( "%1% is thinking" ) % this->m_name ) );
      wait();
      mp_logger->log( boost::str( boost::format( "%1% is hungry" ) % this->m_name ) );

      // attempt to grab forks
      if( can_grab_fork( fork_left ) ) {
	ForkLock lock_left( *fork_left, boost::adopt_lock );
	if( can_grab_fork( fork_right ) ) {
	  ForkLock lock_right( *fork_right, boost::adopt_lock );
	  // eating
	  mp_logger->log( boost::str( boost::format( "%1% is eating (%2%)..." ) % m_name % m_meals_left ) );
	  wait();
	  // record the meal
	  --m_meals_left;
	} else {
	  failed_to_grab_fork = true;
	}
      } else {
	failed_to_grab_fork = true;
      }

      if( failed_to_grab_fork ) {
	mp_logger->log( boost::str( boost::format( "%1% couldn't get forks; waiting..." ) % m_name ) );
	failed_to_grab_fork = false;
	wait();
      }
    }

    mp_logger->log( boost::str( boost::format( "%1% is done dining" ) % m_name ) );
  }

  inline void wait() {
    wait( MIN_WAIT_TIME + ( std::rand() % MAX_JITTER ) );
  }

  inline void wait( boost::uint32_t time_in_ms ) {
    boost::this_thread::sleep( boost::posix_time::milliseconds( time_in_ms ) );
  }

  std::string m_name;
  volatile bool m_continue;
  ForkPtr mp_fork_left;  // must be declared before the thread
  ForkPtr mp_fork_right; // must be declared before the thread
  boost::thread m_thread;
  boost::uint32_t m_meals_left;
  AtomicLoggerOstreamPtr mp_logger;
};
typedef boost::shared_ptr< Philosopher > PhilosopherPtr;

int main() {
  const int N = 5;
  std::string names[] = { "Aristotle", "Spinoza", "Russell", "Kant", "Plato" };

  std::vector< PhilosopherPtr > philosophers;
  philosophers.reserve( N );

  // create logger
  AtomicLoggerOstreamPtr p_logger( new AtomicLoggerOstream( std::cout ) );

  // create forks
  std::vector< ForkPtr > forks;
  forks.reserve( N );
  for( int i = 0; i < N; ++i ) {
    forks.push_back( ForkPtr( new Fork() ) );
  }

  // create philosophers
  for( int i = 0; i < N; ++i ) {
    philosophers.push_back( PhilosopherPtr(
					   new Philosopher( names[ i ], forks[ i ], forks[ (i + 1) % N ], p_logger ) ) );
  }

  // wait for them to finish
  for( int i = 0; i < N; ++i ) {
    philosophers[ i ]->wait_for_cmplt();
  }

  p_logger->log( "Everyone is done dining." );

  return 0;
}


```



###  Standard C++ only

```cpp
#include <algorithm>
#include <array>
#include <atomic>
#include <chrono>
//We are using only standard library, so snprintf instead of Boost::Format
#include <cstdio>
#include <iostream>
#include <mutex>
#include <random>
#include <string>
#include <thread>

std::mutex cout_mutex;

struct Fork {
    std::mutex mutex;
};

struct Dinner {
    std::atomic<bool> ready {false};
    std::array<Fork, 5> forks;
    ~Dinner() { std::cout << "Dinner is over"; }
};

class Philosopher
{
    std::mt19937 rng{std::random_device {}()};

    const std::string name;
    const Dinner& dinner;
    Fork& left;
    Fork& right;
    std::thread worker;

    void live();
    void dine();
    void ponder();
  public:
    Philosopher(std::string name_, const Dinner& dinn, Fork& l, Fork& r)
      : name(std::move(name_)), dinner(dinn) , left(l), right(r), worker(&Philosopher::live, this)
    {}
    ~Philosopher()
    {
        worker.join();
        std::lock_guard<std::mutex>  cout_lock(cout_mutex);
        std::cout << name << " went to sleep." << std::endl;
    }
};

void Philosopher::live()
{
    while (not dinner.ready)
        ; //You spin me right round, baby, right round...
    do {//Aquire forks first
        //lock uses deadlock prevention mechanism to acquire mutexes safely
        std::lock(left.mutex, right.mutex);
        dine(); //Dine adopts lock on forks and releases them
        if(not dinner.ready) break;
        ponder();
    } while(dinner.ready);
}

void Philosopher::dine()
{
    std::lock_guard<std::mutex>  left_lock( left.mutex, std::adopt_lock);
    std::lock_guard<std::mutex> right_lock(right.mutex, std::adopt_lock);

    thread_local std::array<const char*, 3> foods {{"chicken", "rice", "soda"}};
    thread_local std::array<const char*, 3> reactions {{
        "I like this %s!", "This %s is good.", "Mmm, %s..."
    }};
    thread_local std::uniform_int_distribution<> dist(1, 6);
    std::shuffle(    foods.begin(),     foods.end(), rng);
    std::shuffle(reactions.begin(), reactions.end(), rng);

    if(not dinner.ready) return;
    {
        std::lock_guard<std::mutex>  cout_lock(cout_mutex);
        std::cout << name << " started eating." << std::endl;
    }
    constexpr size_t buf_size = 64;
    char buffer[buf_size];
    for(int i = 0; i < 3; ++i) {
        std::this_thread::sleep_for(std::chrono::milliseconds(dist(rng)*50));
        snprintf(buffer, buf_size, reactions[i], foods[i]);
        std::lock_guard<std::mutex>  cout_lock(cout_mutex);
        std::cout << name << ": " << buffer << std::endl;
    }
    std::this_thread::sleep_for(std::chrono::milliseconds(dist(rng))*50);
    std::lock_guard<std::mutex>  cout_lock(cout_mutex);
    std::cout << name << " finished and left." << std::endl;
}

void Philosopher::ponder()
{
    static constexpr std::array<const char*, 5> topics {{
        "politics", "art", "meaning of life", "source of morality", "how many straws makes a bale"
    }};
    thread_local std::uniform_int_distribution<> wait(1, 6);
    thread_local std::uniform_int_distribution<> dist(0, topics.size() - 1);
    while(dist(rng) > 0) {
        std::this_thread::sleep_for(std::chrono::milliseconds(wait(rng)*150));
        std::lock_guard<std::mutex>  cout_lock(cout_mutex);
        std::cout << name << " is pondering about " << topics[dist(rng)] << '.' << std::endl;
        if(not dinner.ready) return;
    }
    std::this_thread::sleep_for(std::chrono::milliseconds(wait(rng)*150));
    std::lock_guard<std::mutex>  cout_lock(cout_mutex);
    std::cout << name << " is hungry again!" << std::endl;
}

int main()
{
    Dinner dinner;
    std::array<Philosopher, 5> philosophers {{
            {"Aristotle", dinner, dinner.forks[0], dinner.forks[1]},
            {"Kant",      dinner, dinner.forks[1], dinner.forks[2]},
            {"Spinoza",   dinner, dinner.forks[2], dinner.forks[3]},
            {"Marx",      dinner, dinner.forks[3], dinner.forks[4]},
            {"Russell",   dinner, dinner.forks[4], dinner.forks[0]},
    }};
    std::this_thread::sleep_for(std::chrono::seconds(1));
    std::cout << "Dinner started!" << std::endl;
    dinner.ready = true;
    std::this_thread::sleep_for(std::chrono::seconds(5));
    dinner.ready = false;
    std::lock_guard<std::mutex>  cout_lock(cout_mutex);
    std::cout << "It is dark outside..." << std::endl;
}
```

Output: http://coliru.stacked-crooked.com/a/1b34c1fc36f5a30c
<div style="height:10em; overflow:auto; border: 1px solid #AAA">
Dinner started!

Spinoza started eating.

Aristotle started eating.

Spinoza: This soda is good.

Spinoza: Mmm, rice...

Aristotle: Mmm, rice...

Spinoza: I like this chicken!

Aristotle: I like this soda!

Spinoza finished and left.

Marx started eating.

Marx: This soda is good.

Aristotle: This chicken is good.

Aristotle finished and left.

Kant started eating.

Marx: I like this rice!

Kant: I like this chicken!

Marx: Mmm, chicken...

Kant: Mmm, rice...

Marx finished and left.

Russell started eating.

Kant: This soda is good.

Spinoza is pondering about source of morality.

Kant finished and left.

Russell: I like this soda!

Aristotle is hungry again!

Russell: This rice is good.

Marx is pondering about art.

Russell: Mmm, chicken...

Russell finished and left.

Aristotle started eating.

Aristotle: This rice is good.

Kant is pondering about meaning of life.

Spinoza is pondering about source of morality.

Aristotle: Mmm, soda...

Spinoza is pondering about source of morality.

Aristotle: I like this chicken!

Aristotle finished and left.

Marx is hungry again!

Marx started eating.

Russell is pondering about how many straws makes a bale.

Kant is hungry again!

Kant started eating.

Aristotle is pondering about meaning of life.

Russell is pondering about how many straws makes a bale.

Marx: This soda is good.

Kant: Mmm, rice...

Russell is pondering about art.

Spinoza is pondering about politics.

Marx: Mmm, rice...

Aristotle is pondering about how many straws makes a bale.

Kant: I like this soda!

Marx: I like this chicken!

Marx finished and left.

Kant: This chicken is good.

Aristotle is pondering about how many straws makes a bale.

Aristotle is pondering about how many straws makes a bale.

Kant finished and left.

Russell is pondering about how many straws makes a bale.

Aristotle is hungry again!

Aristotle started eating.

Spinoza is pondering about source of morality.

Marx is hungry again!

Marx started eating.

Kant is pondering about source of morality.

Marx: This soda is good.

Russell is hungry again!

Aristotle: Mmm, soda...

Spinoza is pondering about source of morality.

Marx: I like this chicken!

Aristotle: This chicken is good.

Kant is pondering about source of morality.

Spinoza is pondering about source of morality.

Marx: Mmm, rice...

Aristotle: I like this rice!

Aristotle finished and left.

It is dark outside...

Marx finished and left.

Russell went to sleep.

Marx went to sleep.

Aristotle is hungry again!

Spinoza is pondering about meaning of life.

Spinoza went to sleep.

Kant is pondering about politics.

Kant went to sleep.

Aristotle went to sleep.

Dinner is over

</div>

## C#

```C sharp

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading;
using System.Threading.Tasks;

namespace Dining_Philosophers
{
    class Program
    {
        private const int DinerCount = 5;
        private static List<Diner> Diners = new List<Diner>();
        private static List<Fork> Forks = new List<Fork>();
        private static DateTime TimeToStop;

        static void Main(string[] args)
        {
            Initialize();
            WriteHeaderLine();

            do
            {
                WriteStatusLine();
                Thread.Sleep(1000);
            }
            while (DateTime.Now < TimeToStop);

            TearDown();
        }

        private static void Initialize()
        {
            for (int i = 0; i < DinerCount; i++)
                Forks.Add(new Fork());
            for (int i = 0; i < DinerCount; i++)
                Diners.Add(new Diner(i, Forks[i], Forks[(i + 1) % DinerCount]));

            TimeToStop = DateTime.Now.AddSeconds(60);
        }

        private static void TearDown()
        {
            foreach (var diner in Diners)
                diner.Dispose();
        }

        private static void WriteHeaderLine()
        {
            Console.Write("|");

            foreach (Diner d in Diners)
                Console.Write("D " + d.ID + "|");

            Console.Write("    |");

            for (int i = 0; i < DinerCount; i++)
                Console.Write("F" + i + "|");

            Console.WriteLine();
        }

        private static void WriteStatusLine()
        {
            Console.Write("|");

            foreach (Diner d in Diners)
                Console.Write(FormatDinerState(d) + "|");

            Console.Write("    |");

            foreach (Fork f in Forks)
                Console.Write(FormatForkState(f) + "|");

            Console.WriteLine();
        }

        private static string FormatDinerState(Diner diner)
        {
            switch (diner.State)
            {
                case Diner.DinerState.Eating:
                    return "Eat";
                case Diner.DinerState.Pondering:
                    return "Pon";
                case Diner.DinerState.TryingToGetForks:
                    return "Get";
                default:
                    throw new Exception("Unknown diner state.");
            }
        }

        private static string FormatForkState(Fork fork)
        {
            return (!ForkIsBeingUsed(fork) ? "  " : "D" + GetForkHolder(fork));
        }

        private static bool ForkIsBeingUsed(Fork fork)
        {
            return Diners.Count(d => d.CurrentlyHeldForks.Contains(fork)) > 0;
        }

        private static int GetForkHolder(Fork fork)
        {
            return Diners.Single(d => d.CurrentlyHeldForks.Contains(fork)).ID;
        }
    }

    class Diner : IDisposable
    {
        private bool IsCurrentlyHoldingLeftFork = false;
        private bool IsCurrentlyHoldingRightFork = false;
        private const int MaximumWaitTime = 100;
        private static Random Randomizer = new Random();
        private bool ShouldStopEating = false;

        public int ID { get; private set; }
        public Fork LeftFork { get; private set; }
        public Fork RightFork { get; private set; }
        public DinerState State { get; private set; }

        public IEnumerable<Fork> CurrentlyHeldForks
        {
            get
            {
                var forks = new List<Fork>();
                if (IsCurrentlyHoldingLeftFork)
                    forks.Add(LeftFork);
                if (IsCurrentlyHoldingRightFork)
                    forks.Add(RightFork);
                return forks;
            }
        }

        public Diner(int id, Fork leftFork, Fork rightFork)
        {
            InitializeDinerState(id, leftFork, rightFork);
            BeginDinerActivity();
        }

        private void KeepTryingToEat()
        {
            do
                if (State == DinerState.TryingToGetForks)
                {
                    TryToGetLeftFork();
                    if (IsCurrentlyHoldingLeftFork)
                    {
                        TryToGetRightFork();
                        if (IsCurrentlyHoldingRightFork)
                        {
                            Eat();
                            DropForks();
                            Ponder();
                        }
                        else
                        {
                            DropForks();
                            WaitForAMoment();
                        }
                    }
                    else
                        WaitForAMoment();
                }
                else
                    State = DinerState.TryingToGetForks;
            while (!ShouldStopEating);
        }

        private void InitializeDinerState(int id, Fork leftFork, Fork rightFork)
        {
            ID = id;
            LeftFork = leftFork;
            RightFork = rightFork;
            State = DinerState.TryingToGetForks;
        }

        private async void BeginDinerActivity()
        {
            await Task.Run(() => KeepTryingToEat());
        }

        private void TryToGetLeftFork()
        {
            Monitor.TryEnter(LeftFork, ref IsCurrentlyHoldingLeftFork);
        }

        private void TryToGetRightFork()
        {
            Monitor.TryEnter(RightFork, ref IsCurrentlyHoldingRightFork);
        }

        private void DropForks()
        {
            DropLeftFork();
            DropRightFork();
        }

        private void DropLeftFork()
        {
            if (IsCurrentlyHoldingLeftFork)
            {
                IsCurrentlyHoldingLeftFork = false;
                Monitor.Exit(LeftFork);
            }
        }

        private void DropRightFork()
        {
            if (IsCurrentlyHoldingRightFork)
            {
                IsCurrentlyHoldingRightFork = false;
                Monitor.Exit(RightFork);
            }
        }

        private void Eat()
        {
            State = DinerState.Eating;
            WaitForAMoment();
        }

        private void Ponder()
        {
            State = DinerState.Pondering;
            WaitForAMoment();
        }

        private static void WaitForAMoment()
        {
            Thread.Sleep(Randomizer.Next(MaximumWaitTime));
        }

        public void Dispose()
        {
            ShouldStopEating = true;
        }

        public enum DinerState
        {
            Eating,
            TryingToGetForks,
            Pondering
        }
    }

    class Fork { }
}

```



## Clojure

Clojure's STM allows us to avoid low-level synchronization primitives like semaphores.  In order to simulate the Dining Philosophers scenario, the forks are [http://clojure.org/refs references] to a boolean indicating whether or not it is available for use.  Each philosopher (also held in a ref) has a fixed amount of food he will try to eat, first by trying to acquire both forks, eating for some period of time, releasing both forks, then thinking for some period of time; if the forks cannot be acquired, the philosopher waits for a fixed amount of time and tries again.

```clojure
(defn make-fork []
  (ref true))

(defn make-philosopher [name forks food-amt]
  (ref {:name name :forks forks :eating? false :food food-amt}))

(defn start-eating [phil]
  (dosync
    (if (every? true? (map ensure (:forks @phil)))  ; <-- the essential solution
      (do
        (doseq [f (:forks @phil)] (alter f not))
        (alter phil assoc :eating? true)
        (alter phil update-in [:food] dec)
        true)
      false)))

(defn stop-eating [phil]
  (dosync
    (when (:eating? @phil)
      (alter phil assoc :eating? false)
      (doseq [f (:forks @phil)] (alter f not)))))

(defn dine [phil retry-interval max-eat-duration max-think-duration]
  (while (pos? (:food @phil))
    (if (start-eating phil)
      (do
        (Thread/sleep (rand-int max-eat-duration))
        (stop-eating phil)
        (Thread/sleep (rand-int max-think-duration)))
      (Thread/sleep retry-interval))))
```

The second line of the <tt>start-eating</tt> function contains the essential solution: by invoking <tt>ensure</tt> on every fork reference, we are guaranteed that the state of the forks will not be modified by other transactions, thus we can rely on those values for the rest of the transaction.  Now we just need to run it:

```clojure
(def *forks* (cycle (take 5 (repeatedly #(make-fork)))))

(def *philosophers*
  (doall (map #(make-philosopher %1 [(nth *forks* %2) (nth *forks* (inc %2))] 1000)
              ["Aristotle" "Kant" "Spinoza" "Marx" "Russell"]
              (range 5))))

(defn start []
  (doseq [phil *philosophers*]
    (.start (Thread. #(dine phil 5 100 100)))))

(defn status []
  (dosync
    (doseq [i (range 5)]
      (let [f @(nth *forks* i)
            p @(nth *philosophers* i)]
        (println (str "fork: available=" f))
        (println (str (:name p)
                      ": eating=" (:eating? p)
                      " food=" (:food p)))))))
```

The <tt>status</tt> function inspects the data structures within a transaction so as to give consistent results (e.g., every unavailable fork has exactly one "eating" philosopher adjacent).


## Common Lisp


This is a translation of the Python solution with small improvements.

Random times are calculated based upon a normal distribution; the main loop doesn't sleep to wait for all philosophers to end dining, it uses a condition variable instead.


```lisp
(in-package :common-lisp-user)

;;
;; FLAG -- if using quicklisp, you can get bordeaux-threads loaded up
;; with: (ql:quickload :bordeaux-threads)
;;

(defvar *philosophers* '(Aristotle Kant Spinoza Marx Russell))

(defclass philosopher ()
  ((name :initarg :name :reader name-of)
   (left-fork :initarg :left-fork :accessor left-fork-of)
   (right-fork :initarg :right-fork :accessor right-fork-of)
   (meals-left :initarg :meals-left :accessor meals-left-of)))

(defclass fork ()
  ((lock :initform (bt:make-lock "fork") :reader lock-of)))

(defun random-normal (&optional (mean 0.0) (sd 1.0))
  (do* ((x1 #1=(1- (* 2.0d0 (random 1d0))) #1#)
        (x2 #2=(1- (* 2.0d0 (random 1d0))) #2#)
        (w  #3=(+ (* x1 x1) (* x2 x2)) #3#))
      ((< w 1d0) (+ (* (* x1 (sqrt (/ (* -2d0 (log w)) w))) sd) mean))))

(defun sleep* (time) (sleep (max time (/ (expt 10 7)))))

(defun dining-philosophers (&key (philosopher-names *philosophers*)
                                 (meals 30)
                                 (dining-time'(1 2))
                                 (thinking-time '(1 2))
                                 ((stream e) *error-output*))
  (let* ((count (length philosopher-names))
         (forks (loop repeat count collect (make-instance 'fork)))
         (philosophers (loop for i from 0
                             for name in philosopher-names collect
                               (make-instance 'philosopher
                                    :left-fork (nth (mod i count) forks)
                                    :right-fork (nth (mod (1+ i) count) forks)
                                    :name name
                                    :meals-left meals)))
         (condition (bt:make-condition-variable))
         (lock (bt:make-lock "main loop"))
         (output-lock (bt:make-lock "output lock")))
    (dolist (p philosophers)
      (labels ((think ()
                 (/me "is now thinking")
                 (sleep* (apply #'random-normal thinking-time))
                 (/me "is now hungry")
                 (dine))
               (dine ()
                 (bt:with-lock-held ((lock-of (left-fork-of p)))
                   (or (bt:acquire-lock (lock-of (right-fork-of p)) nil)
                       (progn (/me "couldn't get a fork and ~
                                    returns to thinking")
                              (bt:release-lock (lock-of (left-fork-of p)))
                              (return-from dine (think))))
                   (/me "is eating")
                   (sleep* (apply #'random-normal dining-time))
                   (bt:release-lock (lock-of (right-fork-of p)))
                   (/me "is done eating (~A meals left)"
                        (decf (meals-left-of p))))
                 (cond ((<= (meals-left-of p) 0)
                        (/me "leaves the dining room")
                        (bt:with-lock-held (lock)
                          (setq philosophers (delete p philosophers))
                          (bt:condition-notify condition)))
                       (t (think))))
               (/me (control &rest args)
                 (bt:with-lock-held (output-lock)
                   (write-sequence (string (name-of p)) e)
                   (write-char #\Space e)
                   (apply #'format e (concatenate 'string control "~%")
                          args))))
        (bt:make-thread #'think)))
    (loop (bt:with-lock-held (lock)
            (when (endp philosophers)
              (format e "all philosophers are done dining~%")
              (return)))
          (bt:with-lock-held (lock)
            (bt:condition-wait condition lock)))))

```

Alternative solution using library STMX which provides Software Transactional Memory, as well as BORDEAUX-THREADS above. Depends on Quicklisp. TAKE will wait until something is available in a TCELL, then remove it. PUT will wait for a TCELL to become empty, then add it. ATOMIC ensures STM operations in its body happen atomically.

```lisp
(ql:quickload '(:stmx :bordeaux-threads))

(defpackage :dining-philosophers
  (:use :cl))

(in-package :dining-philosophers)

(defstruct philosopher
  name
  left-fork
  right-fork)

(defparameter *philosophers* '("Aristotle" "Kant" "Spinoza" "Marx" "Russell"))
(defparameter *eating-max* 5.0)
(defparameter *thinking-max* 5.0)
(defvar *log-lock* (bt:make-lock))
(defvar *running* nil)

(defun print-log (name status)
  (bt:with-lock-held (*log-lock*)
    (format t "~a is ~a~%" name status)))

(defun philosopher-cycle (philosopher)
  "Continously atomically grab and return the left and right forks of the given PHILOSOPHER."
  (with-slots (name left-fork right-fork) philosopher
    (loop while *running*
       do
         (print-log name "hungry")
         (stmx:atomic
          (stmx.util:take left-fork)
          (stmx.util:take right-fork))
         (print-log name "eating")
         (sleep (random *eating-max*))
         (stmx:atomic
          (stmx.util:put left-fork t)
          (stmx.util:put right-fork t))
         (print-log name "thinking")
         (sleep (random *thinking-max*)))))

(defun scenario ()
  (let ((forks (loop repeat (length *philosophers*) collect (stmx.util:tcell t))))
    (setf *running* t)
    (loop for name in *philosophers*
       for left-fork in forks
       for right-fork in (append (cdr forks) (list (car forks)))
       do (let ((philosopher (make-philosopher :name name :left-fork left-fork :right-fork right-fork)))
            (bt:make-thread (lambda () (philosopher-cycle philosopher))
                            :initial-bindings (cons (cons '*standard-output* *standard-output*)
                                                    bt:*default-special-bindings*))))))

```

```lisp
DINING-PHILOSOPHERS> (scenario)
Aristotle is hungry
Aristotle is eating
Kant is hungry
Spinoza is hungry
Spinoza is eating
Marx is hungry
NIL
Russell is hungry
Aristotle is thinking
Russell is eating
Spinoza is thinking
Kant is eating
Spinoza is hungry
Russell is thinking
Marx is eating
Kant is thinking
Aristotle is hungry
Aristotle is eating
Marx is thinking
Spinoza is eating
Spinoza is thinking
Marx is hungry
Marx is eating
Russell is hungry
Marx is thinking
Kant is hungry
Aristotle is thinking
Russell is eating
Kant is eating
Marx is hungry
Spinoza is hungry
Kant is thinking
Spinoza is eating
Kant is hungry
Aristotle is hungry
Russell is thinking
Aristotle is eating
Aristotle is thinking
Aristotle is hungry
Aristotle is eating
Spinoza is thinking
Marx is eating
...
```



## D

This code is using a strict order for the forks/mutexes to prevent a deadlock.


```d
import std.stdio, std.algorithm, std.string, std.parallelism,
       core.sync.mutex;

void eat(in size_t i, in string name, Mutex[] forks) {
    writeln(name, " is hungry.");
    immutable j = (i + 1) % forks.length;

    // Take forks i and j. The lower one first to prevent deadlock.
    auto fork1 = forks[min(i, j)];
    auto fork2 = forks[max(i, j)];

    fork1.lock;
    scope(exit) fork1.unlock;

    fork2.lock;
    scope(exit) fork2.unlock;

    writeln(name, " is eating.");
    writeln(name, " is full.");
}

void think(in string name) {
    writeln(name, " is thinking.");
}

void main() {
    const philosophers = "Aristotle Kant Spinoza Marx Russell".split;
    Mutex[philosophers.length] forks;
    foreach (ref fork; forks)
        fork = new Mutex;

    defaultPoolThreads = forks.length;
    foreach (i, philo; taskPool.parallel(philosophers)) {
        foreach (immutable _; 0 .. 100) {
            eat(i, philo, forks);
            philo.think;
        }
    }
}
```

```txt
Spinoza is full.
Spinoza is thinking.
Russel is eating.
Russel is full.
Russel is thinking.
Russel is hungry.
Kant is eating.
Kant is full.
Kant is thinking.
Kant is hungry.
Spinoza is hungry.
Aristotle is eating.
Aristotle is full.
```



## E


A classic article on solving a version of this problem in E is [http://www.erights.org/e/satan/index.html Satan Comes to Dinner in E].


## EchoLisp

We introduce a laquais who checks that no more than 4 philosophers are sitting at the same time. This prevents deadlocks. Reference : [http://greenteapress.com/semaphores/downey08semaphores.pdf The little book of semaphores].


```scheme

(lib 'tasks)

(define names #(Aristotle Kant Spinoza Marx Russell))
(define abouts #("Wittgenstein" "the nature of the World" "Kant"  "starving"
    "spaghettis" "the essence of things" "Ω" "📞" "⚽️" "🍅" "🌿"
    "philosophy" "💔"  "👠" "rosetta code" "his to-do list" ))
(define (about) (format "thinking about %a." (vector-ref abouts (random (vector-length abouts)))))

;; statistics
(define rounds (make-vector 5 0))
(define (eat i) (vector-set! rounds i (1+ (vector-ref rounds i))))

;; forks are resources = semaphores
(define (left i) i)
(define (right i) (modulo (1+ i) 5))
(define forks (for/vector ((i 5)) (make-semaphore 1)))
(define (fork i) (vector-ref forks i))

(define laquais (make-semaphore 4))

;; philosophers tasks
(define (philo i)
;; thinking
       (writeln (vector-ref names i) (about))
    (sleep (+ 2000 (random 1000)))
    (wait laquais)
;; get forks
       (writeln (vector-ref names i) 'sitting)
    (wait (fork (left i)))
    (wait (fork (right i)))
       (writeln (vector-ref names i) 'eating)
       (eat i)
       (sleep (+ 6000 (random 1000)))
;; put-forks
    (signal (fork (left i)))
    (signal (fork (right i)))
    (signal laquais)
  i)
(define tasks (for/vector ((i 5)) (make-task philo i)))

```

```scheme

(define (observe dummmy)
		(writeln 'observer 'rounds= rounds)
		#t)
(define observer (make-task observe #t ))

(define (dinner)
	(task-run observer 5000)
	(for ((t tasks)) (task-run t)))

(dinner)

```

<small>
Marx     thinking about philosophy.

Russell     thinking about Kant.

Aristotle     thinking about 🌿.

Spinoza     thinking about Ω.

Kant     thinking about 🍅.

Marx     sitting

Marx     eating

Russell     sitting

Aristotle     sitting

Aristotle     eating

Spinoza     sitting

observer     rounds=     #( 1 0 0 1 0)

observer     rounds=     #( 1 0 0 1 0)

Spinoza     eating

Marx     thinking about 🍅.

Kant     sitting

Russell     eating

Aristotle     thinking about 💔.

observer     rounds=     #( 1 0 1 1 1)

Marx     sitting

Kant     eating

Aristotle     sitting

Spinoza     thinking about Ω.

observer     rounds=     #( 1 1 1 1 1)

Marx     eating

Russell     thinking about 🌿.

Spinoza     sitting

observer     rounds=     #( 1 1 1 2 1)

Russell     sitting

Aristotle     eating

Kant     thinking about 💔.

Spinoza     eating

Marx     thinking about 📞.

Kant     sitting

observer     rounds=     #( 2 1 2 2 1)

Russell     eating

Marx     sitting

Aristotle     thinking about Kant.

Kant     eating

Spinoza     thinking about spaghettis.

observer     rounds=     #( 2 2 2 2 2)

Aristotle     sitting

observer     rounds=     #( 2 2 2 2 2)

Spinoza     sitting

Marx     eating

Russell     thinking about 📞.

Aristotle     eating

Kant     thinking about the essence of things.

Russell     sitting

[...] CTRL-C to stop.

</small>




## Eiffel

This solution for the dining philosophers is programmed in Eiffel using [http://docs.eiffel.com/book/solutions/concurrent-eiffel-scoop Simple Concurrent Object-Oriented Programming] (SCOOP). In SCOOP for Eiffel, the keyword <code lang="eiffel">separate</code> in a declaration designates that the associated object may be handled by a SCOOP processor other than (separate from) the one handling the current object. So, in this example, philosophers and forks are all declared as separate types.

The synchronization of access to the resources (the forks) occurs when the routine <code lang="eiffel">eat</code> is called. The two arguments are the two separate forks adjacent to the philosopher. The <code lang="eiffel">eat</code> routine will not proceed until exclusive access to all separate arguments is assured. The resources are released when the routine terminates.

The example uses numbers (versus names) to identify the philosophers in order to allow the user to vary the number of philosophers.


```eiffel
class
    DINING_PHILOSOPHERS

create
    make

feature -- Initialization

    make
            -- Create philosophers and forks.
        local
            first_fork: separate FORK
            left_fork: separate FORK
            right_fork: separate FORK
            philosopher: separate PHILOSOPHER
            i: INTEGER
        do
            print ("Dining Philosophers%N" + philosopher_count.out + " philosophers, " + round_count.out + " rounds%N%N")
            create philosophers.make
            from
                i := 1
                create first_fork.make (philosopher_count, 1)
                left_fork := first_fork
            until
                i > philosopher_count
            loop
                if i < philosopher_count then
                    create right_fork.make (i, i + 1)
                else
                    right_fork := first_fork
                end
                create philosopher.make (i, left_fork, right_fork, round_count)
                philosophers.extend (philosopher)
                left_fork := right_fork
                i := i + 1
            end
            philosophers.do_all (agent launch_philosopher)
            print ("Make Done!%N")
        end

feature {NONE} -- Implementation

    philosopher_count: INTEGER = 5
            -- Number of philosophers.

    round_count: INTEGER = 30
            -- Number of times each philosopher should eat.

    philosophers: LINKED_LIST [separate PHILOSOPHER]
            -- List of philosophers.

    launch_philosopher (a_philosopher: separate PHILOSOPHER)
            -- Launch a_philosopher.
        do
            a_philosopher.live
        end

end -- class DINING_PHILOSOPHERS
```


```eiffel
class
    PHILOSOPHER

create
    make

feature -- Initialization

    make (philosopher: INTEGER; left, right: separate FORK; rounds: INTEGER)
            -- Initialize with ID of `philosopher', forks `left' and `right', and for `rounds' times to eat.
        require
            valid_id: philosopher >= 1
            valid_times_to_eat: rounds >= 1
        do
            id := philosopher
            left_fork := left
            right_fork := right
            round_count := rounds
            report ("announced")
        ensure
            id_set: id = philosopher
            left_fork_set: left_fork = left
            right_fork_set: right_fork = right
            rounds_set: round_count = rounds
        end

feature -- Access

    id: INTEGER
            -- Philosopher's id.

feature -- Basic operations

    live
            -- Model philosopher's life.
        do
            from
                report ("joined")
                has_eaten_count := 0
            until
                has_eaten_count >= round_count
            loop
                think
                eat (left_fork, right_fork)
            end
            report ("done")
        end

    eat (left, right: separate FORK)
            -- Eat, having acquired `left' and `right' forks.
        do
                -- Take forks.
            report ("taking forks")
            left.pick (Current)
            right.pick (Current)
                -- Eat.
            report ("eating")
            delay (200)
                -- Put forks back.
            report ("putting forks back")
            left.put (Current)
            right.put (Current)
                -- Report statistics.
            has_eaten_count := has_eaten_count + 1
            report ("has eaten " + has_eaten_count.out + " times")
        end

    think
            -- Think ... for a short time.
        do
            report ("thinking")
            delay (400)
        end

feature {NONE} -- Output

    report (task: STRING)
            -- Report about execution of the specified `task'.
        do
            print ("Philosopher " + id.out + ": " + task + ".%N")
        end

feature {NONE} -- Timing

    delay (milliseconds: INTEGER_64)
            -- Delay execution by `milliseconds'.
        do
            (create {EXECUTION_ENVIRONMENT}).sleep (milliseconds * 1_000_000)
        end

feature {NONE} -- Status

    round_count: INTEGER
            -- Number of times philosopher should eat.

    has_eaten_count: INTEGER
            -- Number of times philosopher has eaten so far.

    left_fork: separate FORK
            -- Left fork used for eating.

    right_fork: separate FORK
            -- Right fork used for eating.

invariant
    valid_id: id >= 1
    valid_round_count: round_count >= 1
    valid_has_eaten_count: has_eaten_count <= round_count

end -- class PHILOSOPHER
```


```eiffel
class
    FORK

create
    make

feature -- Initialization

    make (left, right: INTEGER)
            -- Initialize between philosophers `left' and `right'.
        do
            id := left.out + "F" + right.out
        end

feature -- Access

    id: STRING
            -- Identification: `F' enclosed by adjacent philosopher id's.

feature -- Basic operations

    pick (philosopher: separate PHILOSOPHER)
            -- Report fork picked up.
        do
            print ("Fork " + id + " picked up by Philosopher " + philosopher.id.out + ".%N")
        end

    put (philosopher: separate PHILOSOPHER)
            -- Report fork put back.
        do
            print ("Fork " + id + " put back by Philosopher " + philosopher.id.out + ".%N")
        end

end -- class FORK
```



## Elixir

Implements the Chandy-Misra algorithm.


```Elixir

defmodule Philosopher do

  defstruct missing: [], clean: [], promised: []

  def run_demo do
    pid1 = spawn(__MODULE__, :init, ["Russell"])
    pid2 = spawn(__MODULE__, :init, ["Marx"])
    pid3 = spawn(__MODULE__, :init, ["Spinoza"])
    pid4 = spawn(__MODULE__, :init, ["Kant"])
    pid5 = spawn(__MODULE__, :init, ["Aristotle"])

    # a chopstick is simply represented by the pid of the neighbour that shares it.

    send(pid1, {:run, %Philosopher{}})
    send(pid2, {:run, %Philosopher{missing: [pid1]}})
    send(pid3, {:run, %Philosopher{missing: [pid2]}})
    send(pid4, {:run, %Philosopher{missing: [pid3]}})
    send(pid5, {:run, %Philosopher{missing: [pid1, pid4]}})
  end

  def init(philosopher_name) do
    receive do
      {:run, state} ->
        spawn(__MODULE__, :change_state, [self()])
        case flip_coin() do
          :heads -> thinking(philosopher_name, state)
          :tails -> hungry(philosopher_name, state)
        end
    end
  end

  defp thinking(philosopher_name, state) do
    receive do
      {:change_state} ->
        hungry(philosopher_name, state)
      {:chopstick_request, pid} ->
        if clean?(pid, state) do
          thinking(philosopher_name, promise_chopstick(philosopher_name, pid, state))
        else
          give_chopstick(philosopher_name, self(), pid)
          %{missing: missing} = state
          thinking(philosopher_name, %{state | missing: [pid | missing]})
        end
    end
  end

  defp hungry(philosopher_name, state) do
    IO.puts "#{philosopher_name} is hungry."
    %{missing: missing} = state
    for pid <- missing, do: request_chopstick(philosopher_name, self(), pid)
    wait_for_chopsticks(philosopher_name, state)
  end

  defp wait_for_chopsticks(philosopher_name, state) do
    if has_chopsticks?(state) do
      eating(philosopher_name, state)
    end
    receive do
      {:chopstick_request, pid} ->
        if clean?(pid, state) do
          wait_for_chopsticks(philosopher_name, promise_chopstick(philosopher_name, pid, state))
        else
          give_chopstick(philosopher_name, self(), pid)
          request_chopstick(philosopher_name, self(), pid)
          %{missing: missing} = state
          wait_for_chopsticks(philosopher_name, %{state | missing: [pid | missing]})
        end
      {:chopstick_response, pid} ->
        %{missing: missing, clean: clean} = state
        wait_for_chopsticks(philosopher_name, %{state | missing: List.delete(missing, pid), clean: [pid | clean]})
    end
  end

  defp eating(philosopher_name, state) do
    IO.puts "*** #{philosopher_name} is eating."
    receive do
      {:change_state} ->
        %{promised: promised} = state
        for pid <- promised, do: give_chopstick(philosopher_name, self(), pid)
        thinking(philosopher_name, %Philosopher{missing: promised})
    end
  end

  defp clean?(pid, state) do
    %{clean: clean} = state
    Enum.member?(clean, pid)
  end

  defp has_chopsticks?(state) do
    %{missing: missing} = state
    Enum.empty?(missing)
  end

  defp promise_chopstick(philosopher_name, pid, state) do
    IO.puts "#{philosopher_name} promises a chopstick."
    %{promised: promised} = state
    %{state | promised: [pid | promised]}
  end

  defp request_chopstick(philosopher_name, snd_pid, recv_pid) do
    IO.puts "#{philosopher_name} requests a chopstick."
    send(recv_pid, {:chopstick_request, snd_pid})
  end

  defp give_chopstick(philosopher_name, snd_pid, recv_pid) do
    IO.puts "#{philosopher_name} gives a chopstick."
    send(recv_pid, {:chopstick_response, snd_pid})
  end

  defp flip_coin do
    case Enum.random(0..1) do
      0 -> :heads
      1 -> :tails
    end
  end

  def change_state(pid) do
    Process.sleep(Enum.random(1..10) * 1000)
    send(pid, {:change_state})
    change_state(pid)
  end

```



## Erlang

This solution avoids deadlock by employing a waiter that only serves a plate of spaghetti when a philosopher has access to two forks. The philosophers wait until a plate is put in front of them before grabbing the forks.


```erlang
-module(philosophers).
-export([dining/0]).

sleep(T) ->
  receive
after T ->
		true
	end.

doForks(ForkList) ->
  receive
	{grabforks, {Left, Right}} -> doForks(ForkList -- [Left, Right]);
	{releaseforks, {Left, Right}} -> doForks([Left, Right| ForkList]);
	{available, {Left, Right}, Sender} ->
          Sender ! {areAvailable, lists:member(Left, ForkList) andalso lists:member(Right, ForkList)},
          doForks(ForkList);
	{die} -> io:format("Forks put away.~n")
  end.

areAvailable(Forks) ->
	forks ! {available, Forks, self()},
	receive
		{areAvailable, false} -> false;
		{areAvailable, true} -> true
	end.


processWaitList([]) -> false;
processWaitList([H|T]) ->
	{Client, Forks} = H,
	case areAvailable(Forks) of
		true -> Client ! {served},
			true;
		false -> processWaitList(T)
	end.

doWaiter([], 0, 0, false) ->
	forks ! {die},
	io:format("Waiter is leaving.~n"),
	diningRoom ! {allgone};
doWaiter(WaitList, ClientCount, EatingCount, Busy) ->
	receive
		{waiting, Client} ->
			WaitList1 = [Client|WaitList],	%% add to waiting list
			case (not Busy) and (EatingCount<2) of
				true ->	Busy1 = processWaitList(WaitList1);
				false -> Busy1 = Busy
			end,
			doWaiter(WaitList1, ClientCount, EatingCount, Busy1);

		{eating, Client} ->
			doWaiter(WaitList -- [Client], ClientCount, EatingCount+1, false);

		{finished} ->
			doWaiter(WaitList, ClientCount, EatingCount-1,
				processWaitList(WaitList));
		{leaving} ->
			doWaiter(WaitList, ClientCount-1, EatingCount, Busy)
	end.


philosopher(Name, Forks, 0) ->
	io:format("~s is leaving.~n", [Name]),

	waiter ! {leaving};


philosopher(Name, Forks, Cycle) ->
	io:format("~s is thinking.~n", [Name]),
	sleep(random:uniform(1000)),

	io:format("~s is hungry.~n", [Name]),
	waiter ! {waiting, {self(), Forks}}, %%sit at table

	receive
		{served}-> forks ! {grabforks, Forks},	%%grab forks
			waiter ! {eating, {self(), Forks}},	%%start eating
			io:format("~s is eating.~n", [Name])
	end,

	sleep(random:uniform(1000)),
	forks ! {releaseforks, Forks},					%% put forks down
	waiter ! {finished},

	philosopher(Name, Forks, Cycle-1).


dining() ->	AllForks = [1, 2, 3, 4, 5],
		Clients = 5,
		register(diningRoom, self()),

		register(forks, spawn(fun()-> doForks(AllForks) end)),
		register(waiter, spawn(fun()-> doWaiter([], Clients, 0, false) end)),
		Life_span = 20,
		spawn(fun()-> philosopher('Aristotle', {5, 1}, Life_span) end),
		spawn(fun()-> philosopher('Kant', {1, 2}, Life_span) end),
		spawn(fun()-> philosopher('Spinoza', {2, 3}, Life_span) end),
		spawn(fun()-> philosopher('Marx', {3, 4}, Life_span) end),
		spawn(fun()-> philosopher('Russel', {4, 5}, Life_span) end),

		receive
 			{allgone} -> io:format("Dining room closed.~n")

		end,
		unregister(diningRoom).
```


## Euphoria


```Euphoria
constant FREE = 0, LOCKED = 1
sequence forks
forks = repeat(FREE,5)

procedure person(sequence name, integer left_fork, integer right_fork)
    while 1 do
        while forks[left_fork] = LOCKED or forks[right_fork] = LOCKED do
            if forks[left_fork] = FREE then
                puts(1, name & " hasn't right fork.\n")
            elsif forks[right_fork] = FREE then
                puts(1, name & " hasn't left fork.\n")
            else
                puts(1, name & " hasn't both forks.\n")
            end if
            puts(1, name & " is waiting.\n")
            task_yield()
        end while

        puts(1, name & " grabs forks.\n")
        forks[left_fork] = LOCKED
        forks[right_fork] = LOCKED
        for i = 1 to rand(10) do
            puts(1, name & " is eating.\n")
            task_yield()
        end for
        puts(1, name & " puts forks down and leaves the dinning room.\n")
        forks[left_fork] = FREE
        forks[right_fork] = FREE

        for i = 1 to rand(10) do
            puts(1, name & " is thinking.\n")
            task_yield()
        end for
        puts(1, name & " becomes hungry.\n")
    end while
end procedure

integer rid
atom taskid
rid = routine_id("person")
taskid = task_create(rid,{"Aristotle",1,2})
task_schedule(taskid,{1,2})
taskid = task_create(rid,{"Kant",2,3})
task_schedule(taskid,{1,2})
taskid = task_create(rid,{"Spinoza",3,4})
task_schedule(taskid,{1,2})
taskid = task_create(rid,{"Marx",4,5})
task_schedule(taskid,{1,2})
taskid = task_create(rid,{"Russell",5,1})
task_schedule(taskid,{1,2})

while get_key() = -1 do
    task_yield()
end while
```


Sample output:

```txt
Russell grabs forks.
Russell is eating.
Marx hasn't right fork.
Marx is waiting.
Spinoza grabs forks.
Spinoza is eating.
Kant hasn't right fork.
Kant is waiting.
Aristotle hasn't left fork.
Aristotle is waiting.
Russell is eating.
Marx hasn't both forks.
Marx is waiting.
Spinoza is eating.
Kant hasn't right fork.
Kant is waiting.
Aristotle hasn't left fork.
Aristotle is waiting.
Russell is eating.
Marx hasn't both forks.
Marx is waiting.
Spinoza is eating.
Kant hasn't right fork.
Kant is waiting.
Aristotle hasn't left fork.
Aristotle is waiting.
Russell puts forks down and leaves the dinning room.
Russell is thinking.
Marx hasn't left fork.
Marx is waiting.
Spinoza puts forks down and leaves the dinning room.
Spinoza is thinking.
Kant grabs forks.
Kant is eating.
Aristotle hasn't right fork.
Aristotle is waiting.
Russell becomes hungry.
Russell grabs forks.
Russell is eating.
Marx hasn't right fork.
Marx is waiting.
Spinoza is thinking.
Kant is eating.
Aristotle hasn't both forks.
Aristotle is waiting.
```


=={{header|F Sharp|F#}}==
This solution avoids deadlock by employing a waiter.


```fsharp

open System

let flip f x y = f y x

let rec cycle s = seq { yield! s; yield! cycle s }

type Agent<'T> = MailboxProcessor<'T>

type Message = Waiting of (Set<int> * AsyncReplyChannel<unit>) | Done of Set<int>

let reply (c: AsyncReplyChannel<_>) = c.Reply()

let strategy forks waiting =
    let aux, waiting = List.partition (fst >> flip Set.isSubset forks) waiting
    let forks = aux |> List.map fst |> List.fold (-) forks
    List.iter (snd >> reply) aux
    forks, waiting

let waiter strategy forkCount =
  Agent<_>.Start(fun inbox ->
    let rec loop forks waiting =
      async { let forks, waiting = strategy forks waiting
              let! msg = inbox.Receive()
              match msg with
                | Waiting r -> return! loop forks (waiting @ [r])
                | Done f -> return! loop (forks + f) waiting }
    loop (Set.ofList (List.init forkCount id)) [])

let philosopher (waiter: Agent<_>) name forks =
  let rng = new Random()
  let forks = Set.ofArray forks
  Agent<_>.Start(fun inbox ->
    let rec loop () =
      async { printfn "%s is thinking" name
              do! Async.Sleep(rng.Next(100, 500))
              printfn "%s is hungry" name
              do! waiter.PostAndAsyncReply(fun c -> Waiting (forks, c))
              printfn "%s is eating" name
              do! Async.Sleep(rng.Next(100, 500))
              printfn "%s is done eating" name
              waiter.Post(Done (forks))
              return! loop () }
    loop ())

[<EntryPoint>]
let main args =
  let forks = Seq.init 5 id |> cycle |> Seq.windowed 2 |> Seq.take 5 |> Seq.toList
  let names = ["plato"; "aristotel"; "kant"; "nietzsche"; "russel"]
  let waiter = waiter strategy 5
  List.map2 (philosopher waiter) names forks |> ignore
  Console.ReadLine() |> ignore
  0

```



## Go


### Channels

Goroutine synchronization done with Go channels.  Deadlock prevented by making one philosopher "left handed."

```go
package main

import (
    "hash/fnv"
    "log"
    "math/rand"
    "os"
    "time"
)

// Number of philosophers is simply the length of this list.
// It is not otherwise fixed in the program.
var ph = []string{"Aristotle", "Kant", "Spinoza", "Marx", "Russell"}

const hunger = 3                // number of times each philosopher eats
const think = time.Second / 100 // mean think time
const eat = time.Second / 100   // mean eat time

var fmt = log.New(os.Stdout, "", 0) // for thread-safe output

var done = make(chan bool)

// This solution uses channels to implement synchronization.
// Sent over channels are "forks."
type fork byte

// A fork object in the program models a physical fork in the simulation.
// A separate channel represents each fork place.  Two philosophers
// have access to each fork.  The channels are buffered with capacity = 1,
// representing a place for a single fork.

// Goroutine for philosopher actions.  An instance is run for each
// philosopher.  Instances run concurrently.
func philosopher(phName string,
    dominantHand, otherHand chan fork, done chan bool) {
    fmt.Println(phName, "seated")
    // each philosopher goroutine has a random number generator,
    // seeded with a hash of the philosopher's name.
    h := fnv.New64a()
    h.Write([]byte(phName))
    rg := rand.New(rand.NewSource(int64(h.Sum64())))
    // utility function to sleep for a randomized nominal time
    rSleep := func(t time.Duration) {
        time.Sleep(t/2 + time.Duration(rg.Int63n(int64(t))))
    }
    for h := hunger; h > 0; h-- {
        fmt.Println(phName, "hungry")
        <-dominantHand // pick up forks
        <-otherHand
        fmt.Println(phName, "eating")
        rSleep(eat)
        dominantHand <- 'f' // put down forks
        otherHand <- 'f'
        fmt.Println(phName, "thinking")
        rSleep(think)
    }
    fmt.Println(phName, "satisfied")
    done <- true
    fmt.Println(phName, "left the table")
}

func main() {
    fmt.Println("table empty")
    // Create fork channels and start philosopher goroutines,
    // supplying each goroutine with the appropriate channels
    place0 := make(chan fork, 1)
    place0 <- 'f' // byte in channel represents a fork on the table.
    placeLeft := place0
    for i := 1; i < len(ph); i++ {
        placeRight := make(chan fork, 1)
        placeRight <- 'f'
        go philosopher(ph[i], placeLeft, placeRight, done)
        placeLeft = placeRight
    }
    // Make one philosopher left handed by reversing fork place
    // supplied to philosopher's dominant hand.
    // This makes precedence acyclic, preventing deadlock.
    go philosopher(ph[0], place0, placeLeft, done)
    // they are all now busy eating
    for range ph {
        <-done // wait for philosphers to finish
    }
    fmt.Println("table empty")
}
```

Output:

```txt

table empty
Kant seated
Marx seated
Spinoza seated
Aristotle seated
Kant hungry
Russell seated
Marx hungry
Russell hungry
Kant eating
Marx eating
Aristotle hungry
Spinoza hungry
Kant thinking
Marx thinking
Spinoza eating
Russell eating
Kant hungry
Russell thinking
Aristotle eating
Marx hungry
Spinoza thinking
Marx eating
Russell hungry
Marx thinking
Aristotle thinking
Russell eating
Kant eating
Russell thinking
Aristotle hungry
Kant thinking
Aristotle eating
Spinoza hungry
Spinoza eating
Marx hungry
Aristotle thinking
Russell hungry
Aristotle hungry
Kant hungry
Spinoza thinking
Kant eating
Marx eating
Marx thinking
Russell eating
Kant thinking
Marx satisfied
Marx left the table
Russell thinking
Aristotle eating
Spinoza hungry
Spinoza eating
Russell satisfied
Russell left the table
Kant satisfied
Kant left the table
Spinoza thinking
Aristotle thinking
Aristotle satisfied
Aristotle left the table
Spinoza satisfied
Spinoza left the table
table empty

```


### Mutexes and WaitGroup

The first solution just uses channels for synchronization.  Channels can solve lots of problems but the sync library has a few other functions to more directly model common operations.  In Dining Philosophers, fork use is mutually exclusive so it's very clear
to model forks with sync.Mutex objects.  Also waiting for a number of concurrent tasks to finish is a common pattern directly implemented with sync.WaitGroup.

One more concurrency technique actually used in both solutions is to use the log package for output rather than the fmt package.  Output from concurrent goroutines can get accidentally interleaved in some cases.  While neither package makes claims about this problem, the log package historically has been coded to avoid interleaved output.

```go
package main

import (
    "hash/fnv"
    "log"
    "math/rand"
    "os"
    "sync"
    "time"
)

var ph = []string{"Aristotle", "Kant", "Spinoza", "Marx", "Russell"}

const hunger = 3
const think = time.Second / 100
const eat = time.Second / 100

var fmt = log.New(os.Stdout, "", 0)

var dining sync.WaitGroup

func philosopher(phName string, dominantHand, otherHand *sync.Mutex) {
    fmt.Println(phName, "seated")
    h := fnv.New64a()
    h.Write([]byte(phName))
    rg := rand.New(rand.NewSource(int64(h.Sum64())))
    rSleep := func(t time.Duration) {
        time.Sleep(t/2 + time.Duration(rg.Int63n(int64(t))))
    }
    for h := hunger; h > 0; h-- {
        fmt.Println(phName, "hungry")
        dominantHand.Lock() // pick up forks
        otherHand.Lock()
        fmt.Println(phName, "eating")
        rSleep(eat)
        dominantHand.Unlock() // put down forks
        otherHand.Unlock()
        fmt.Println(phName, "thinking")
        rSleep(think)
    }
    fmt.Println(phName, "satisfied")
    dining.Done()
    fmt.Println(phName, "left the table")
}

func main() {
    fmt.Println("table empty")
    dining.Add(5)
    fork0 := &sync.Mutex{}
    forkLeft := fork0
    for i := 1; i < len(ph); i++ {
        forkRight := &sync.Mutex{}
        go philosopher(ph[i], forkLeft, forkRight)
        forkLeft = forkRight
    }
    go philosopher(ph[0], fork0, forkLeft)
    dining.Wait() // wait for philosphers to finish
    fmt.Println("table empty")
}
```



## Groovy

Deadlocks are avoided by always getting locks on forks with lower numbers first.

```groovy
import groovy.transform.Canonical

import java.util.concurrent.locks.Lock
import java.util.concurrent.locks.ReentrantLock

@Canonical
class Fork {
    String name
    Lock lock = new ReentrantLock()

    void pickUp(String philosopher) {
        lock.lock()
        println "  $philosopher picked up $name"
    }

    void putDown(String philosopher) {
        lock.unlock()
        println "  $philosopher put down $name"
    }
}

@Canonical
class Philosopher extends Thread {
    Fork f1
    Fork f2

    @Override
    void run() {
        def random = new Random()
        (1..20).each { bite ->
            println "$name is hungry"
            f1.pickUp name
            f2.pickUp name
            println "$name is eating bite $bite"
            Thread.sleep random.nextInt(300) + 100
            f2.putDown name
            f1.putDown name
        }
    }
}

void diningPhilosophers(names) {
    def forks = (1..names.size()).collect { new Fork(name: "Fork $it") }
    def philosophers = []
    names.eachWithIndex{ n, i ->
        def (i1, i2) = [i, (i + 1) % 5]
        if (i2 < i1) (i1, i2) = [i2, i]

        def p = new Philosopher(name: n, f1: forks[i1], f2: forks[i2])
        p.start()
        philosophers << p
    }
    philosophers.each { it.join() }
}

diningPhilosophers(['Aristotle', 'Kant', 'Spinoza', 'Marx', 'Russell'])
```



## Haskell

Using the built-in Software Transactional Memory in GHC.

```haskell
module Philosophers where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import System.Random

-- TMVars are transactional references. They can only be used in transactional actions.
-- They are either empty or contain one value. Taking an empty reference fails and
-- putting a value in a full reference fails. A transactional action only succeeds
-- when all the component actions succeed, else it rolls back and retries until it
-- succeeds.
-- The Int is just for display purposes.
type Fork = TMVar Int

newFork :: Int -> IO Fork
newFork i = newTMVarIO i

-- The basic transactional operations on forks
takeFork :: Fork -> STM Int
takeFork fork = takeTMVar fork

releaseFork :: Int -> Fork -> STM ()
releaseFork i fork = putTMVar fork i

type Name = String

runPhilosopher :: Name -> (Fork, Fork) -> IO ()
runPhilosopher name (left, right) = forever $ do
  putStrLn (name ++ " is hungry.")

  -- Run the transactional action atomically.
  -- The type system ensures this is the only way to run transactional actions.
  (leftNum, rightNum) <- atomically $ do
    leftNum <- takeFork left
    rightNum <- takeFork right
    return (leftNum, rightNum)

  putStrLn (name ++ " got forks " ++ show leftNum ++ " and " ++ show rightNum ++ " and is now eating.")
  delay <- randomRIO (1,10)
  threadDelay (delay * 1000000) -- 1, 10 seconds. threadDelay uses nanoseconds.
  putStrLn (name ++ " is done eating. Going back to thinking.")

  atomically $ do
    releaseFork leftNum left
    releaseFork rightNum right

  delay <- randomRIO (1, 10)
  threadDelay (delay * 1000000)

philosophers :: [String]
philosophers = ["Aristotle", "Kant", "Spinoza", "Marx", "Russel"]

main = do
  forks <- mapM newFork [1..5]
  let namedPhilosophers  = map runPhilosopher philosophers
      forkPairs          = zip forks (tail . cycle $ forks)
      philosophersWithForks = zipWith ($) namedPhilosophers forkPairs

  putStrLn "Running the philosophers. Press enter to quit."

  mapM_ forkIO philosophersWithForks

  -- All threads exit when the main thread exits.
  getLine

```


==Icon and {{header|Unicon}}==

Icon doesn't support concurrency.  This Unicon solution avoids deadlock and
livelock (but not starvation) by not allowing philosophers to hold onto one
fork if they can't get the other, and by having each philosopher pick up
their lowest numbered fork first.
The code would be slightly simpler if the philosophers wouldn't waste time waiting
when they can't get both forks and went back to thinking instead.  (Take away their grant money.)


```unicon
global forks, names

procedure main(A)
    names := ["Aristotle","Kant","Spinoza","Marks","Russell"]
    write("^C to terminate")
    nP := *names
    forks := [: |mutex([])\nP :]
    every p := !nP do thread philosopher(p)
    delay(-1)
end

procedure philosopher(n)
    f1 := forks[min(n, n%*forks+1)]
    f2 := forks[max(n, n%*forks+1)]
    repeat {
        write(names[n]," thinking")
        delay(1000*?5)
        write(names[n]," hungry")
        repeat {
            fork1 := lock(f1)
            if fork2 := trylock(f2) then {
                write(names[n]," eating")
                delay(1000*?5)
                break (unlock(fork2), unlock(fork1))  # full
                }
            unlock(fork1)  # Free first fork and go back to waiting
            }
        }
end
```


A sample run, terminated after some time.

```txt
->dp
^C to terminate
Kant thinking
Spinoza thinking
Aristotle thinking
Russell thinking
Marks thinking
Kant hungry
Russell hungry
Kant eating
Spinoza hungry
Russell eating
Aristotle hungry
Marks hungry
Kant thinking
Spinoza eating
Russell thinking
Aristotle eating
Kant hungry
Spinoza thinking
Marks eating
Aristotle thinking
Kant eating
Russell hungry
Spinoza hungry
Russell eating
Marks thinking
Aristotle hungry
Kant thinking
Spinoza eating
Russell thinking
Marks hungry
Aristotle eating
Kant hungry
Spinoza thinking
Marks eating
Russell hungry
Aristotle thinking
Spinoza hungry
Kant eating
Russell eating
Marks thinking
Kant thinking
Marks hungry
Spinoza eating
Aristotle hungry
Russell thinking
Aristotle eating
^C

```



## J

These philosophers are very smart and polite: they figured out immediately that at most two of them can eat simultaneously (take the floor of n divided by 2 for n philosophers); so, when they are hungry and it is necessary, they wait in line.  (In general, for n > 1, because they are very smart and polite, when a philosopher seats he leaves exactly one empty seat between himself and one of the philosophers which are already eating if any.)

J does not support concurrency; so, this is a discrete-event simulation (DES).  The time spent thinking and eating is assumed to be exponentially distributed, respectively, at the rates of 1 and 0.5 per time unit.


### The simulation code

The simulation is defined in terms of fixed tacit (stateless point-free) code (a Turing complete dialect of J; see, https://rosettacode.org/wiki/Universal_Turing_machine#J),


```j
". noun define -. CRLF     NB. Fixed tacit simulation code...

simulate=.
''"_@:((<@:(1 -~ 1&({::)) 1} ])@:(([ 0 0&$@(1!:2&2)@:(((6j3 ": 9&({::)) , ':
'"_) , ' starts waiting and thinking about hunger.' ,~ 8&({::) {:: 0&({::)))@
:(<@:(6&({::) , 8&({::)) 6} ])@:((<@:((0 (0 {:: ])`(<@:(1 {:: ]))`(2 {:: ])}
])@:(3 8 2&{)) 2} ])@:(<@:2: 3} ]))@:((<@:((0 (0 {:: ])`(<@:(1 {:: ]))`(2 {::
 ])} ])@:(5 8 4&{)) 4} ])@:(<@:_: 5} ]))`(([ 0 0&$@(1!:2&2)@:(((6j3 ": 9&({::
)) , ': '"_) , ' starts eating.' ,~ 8&({::) {:: 0&({::)))@:((<@:((0 (0 {:: ])
`(<@:(1 {:: ]))`(2 {:: ])} ])@:(3 8 2&{)) 2} ])@:(<@:1: 3} ]))@:((<@:((0 (0 {
:: ])`(<@:(1 {:: ]))`(2 {:: ])} ])@:(5 8 4&{)) 4} ])@:(<@:(_2 * ^.@:?@:0:) 5}
 ])))@.(7&({::) > 1 +/@:= 2&({::))`((<@:(}.@:(6&({::))) 6} ])@:(([ 0 0&$@(1!:
2&2)@:(((6j3 ": 9&({::)) , ': '"_) , ' starts eating.' ,~ 8&({::) {:: 0&({::)
))@:((<@:((0 (0 {:: ])`(<@:(1 {:: ]))`(2 {:: ])} ])@:(3 8 2&{)) 2} ])@:(<@:1:
 3} ]))@:((<@:((0 (0 {:: ])`(<@:(1 {:: ]))`(2 {:: ])} ])@:(5 8 4&{)) 4} ])@:(
<@:(_2 * ^.@:?@:0:) 5} ])))@:(<@:({.@:(6&({::))) 8} ])^:(1 <: #@:(6&({::)))@:
([ 0 0&$@(1!:2&2)@:(((6j3 ": 9&({::)) , ': '"_) , ' starts thinking.' ,~ 8&({
::) {:: 0&({::)))@:((<@:((0 (0 {:: ])`(<@:(1 {:: ]))`(2 {:: ])} ])@:(3 8 2&{)
) 2} ])@:(<@:0: 3} ]))@:((<@:((0 (0 {:: ])`(<@:(1 {:: ]))`(2 {:: ])} ])@:(5 8
 4&{)) 4} ])@:(<@:(_1 * ^.@:?@:0:) 5} ])))@.('' ($ ,) 8&({::) { 2&({::)))@:(<
@:(0 I.@:= 4&({::)) 8} ])@:(<@:((- <./)@:(4&({::))) 4} ])@:(<@:(9&({::) + <./
@:(4&({::))) 9} ])^:(0 < 1&({::))^:_)@:(([ 0 0&$@(1!:2&2)@:(((6j3 ": 9&({::))
 , ': '"_) , 'All of them start thinking.'"_))@:((0 ; <.@:(2 %~ #@:(0&({::)))
) 9 7} ])@:((0:"_1 ,&< (_1 * ^.@:?@:0:)&>)@:(0&({::)) 2 4} ])@:((;:@:(0&({::)
) ,&< ''"_) 0 6} ]))@:(,&(;:8$','))@:;

)
```



### Simulation of 11 chronological events for five philosophers



```j
   'Aristotle Kant Spinoza Marx Russell' simulate 11
 0.000: All of them start thinking.
 0.097: Spinoza starts eating.
 0.474: Aristotle starts eating.
 0.950: Russell starts waiting and thinking about hunger.
 1.125: Kant starts waiting and thinking about hunger.
 2.263: Spinoza starts thinking.
 2.263: Russell starts eating.
 2.762: Marx starts waiting and thinking about hunger.
 2.771: Spinoza starts waiting and thinking about hunger.
 4.769: Russell starts thinking.
 4.769: Kant starts eating.
 4.845: Russell starts waiting and thinking about hunger.
 5.166: Aristotle starts thinking.
 5.166: Marx starts eating.
 5.915: Marx starts thinking.
 5.915: Spinoza starts eating.
```



### Simulation of 22 chronological events for eight philosophers



```j
   'Aristotle Kant Spinoza Marx Russell Laozi Nezahualcoyotl Averroes' simulate 22
 0.000: All of them start thinking.
 0.077: Nezahualcoyotl starts eating.
 0.312: Marx starts eating.
 0.424: Laozi starts eating.
 0.502: Kant starts eating.
 0.541: Marx starts thinking.
 0.545: Marx starts eating.
 0.660: Laozi starts thinking.
 0.715: Laozi starts eating.
 0.766: Aristotle starts waiting and thinking about hunger.
 0.871: Laozi starts thinking.
 0.871: Aristotle starts eating.
 0.893: Averroes starts waiting and thinking about hunger.
 1.035: Nezahualcoyotl starts thinking.
 1.035: Averroes starts eating.
 1.071: Laozi starts waiting and thinking about hunger.
 1.168: Kant starts thinking.
 1.168: Laozi starts eating.
 1.614: Russell starts waiting and thinking about hunger.
 1.660: Spinoza starts waiting and thinking about hunger.
 1.813: Aristotle starts thinking.
 1.813: Russell starts eating.
 2.022: Marx starts thinking.
 2.022: Spinoza starts eating.
 2.164: Russell starts thinking.
 2.182: Aristotle starts eating.
 2.339: Marx starts waiting and thinking about hunger.
 2.446: Aristotle starts thinking.
 2.446: Marx starts eating.
```


===The structured derivation of the verb (function)===

The fixed tacit code of the verb (simulate) was produced by means of an unorthodox tacit toolkit; however, the verb produced is orthodox (compliant):


```j
NB. Quick and dirty tacit toolkit...

o=. @:
c=."_

ver=. (0:`)([:^:)

d=. (fix=. (;:'f.')ver) (train=.(;:'`:')ver&6) (an=. <@:((,'0') (,&<) ]))
ver=. (an f. o fix'ver')ver o an f.
z=. ((an'')`($ ,)`) (`:6)
d=. (a0=. `'') (a1=. (@[) ((<'&')`) (`:6)) (a2=. (`(<(":0);_)) (`:6))
av=. ((an o fix'a0')`)  (`(an o fix'a1')) (`(an o fix'a2') ) (`:6)

Fetch=. (ver o train ;:'&{::')&.> o i. f.av
tie=. ver o train ;:'`'

indices=. (, $~ 1 -.~ $) o (train"0 o ((1 -: L.)S:1 # <S:1) o (tie&'') o fix :: ])
f=. ((ver o train ;:'&{')) o indices o train f.av

'A B'=. 2 Fetch
head=. (;:'<@:') {.~ 2 * 1 = #@[
h=. train o (indices o train o (A f) (head , (B f)@] , < o an@[  , (;:'}]')c) ]) f.av

DropIfNB=. < o ('('"_ , ] , ')'"_) o ((}: ^: ('NB.' -: 3&{. o > o {:)) &. ;:)
pipe=. ([ , ' o ' , ])&:>/ o |.

is=. ". o (, o ": o > , '=. ' , pipe o (DropIfNB;._2) o ". o ('0 ( : 0)'c)) f.av

NB.--------------------------------------------------------------------------------------

NB. Producing the verb simulate...

Note 0

NB. X and Y...
  N - Philosophers names
  C - Number of chronological events to simulate

NB. Local...
  A - Activity (0 - Thinking, 1 -eating, 2 - Thinking while queuing,)
  B - New activity
  T - Residual time left for the activity
  S - Starting time for the new activity
  Q - Queue
  U - Upper bound for the number of philosophers which can eat simultaneously
  P - Active philosopher
  E - Elapsed Time (only for information purposes)
)

amend=. 0 (0 {:: ])`(<@:(1 {:: ]))`(2 {:: ])} ]

'N C A B T S Q U P E'=. 10 Fetch  NB. 10 Boxes

thinktime=. _1 * ^. o ? o 0:  NB. Exponentially distributed at a rate of one
eattime  =. _2 * ^. o ? o 0:  NB. Exponentially distributed at a rate of one-half
j=. ,&<

time=. (6j3 ": E) , ': 'c

start is
  (N Q)`((;: o N) j (''c))            h NB. Boxing the names, empty queue
  (A T)`((0:items j thinktime&>) o N) h NB. All start thinking
  (E U)`(0 ; <. o (2 %~ # o N))       h NB. elapsed time 0, Upper bound
  [ echo o (time , 'All of them start thinking.'c)
)

CanEat=. U > 1 +/ o = A   NB. Can eat if there is a suitable place at the table

eat is
  T`(amend o ((S P T)f))h o (S`eattime h)  NB. Eating time
  A`(amend o ((B P A)f))h o (B`1:      h)  NB. Activity: eating
  [ echo o (time , ' starts eating.' ,~ P {:: N)
)

enqueue is
  T`(amend o ((S P T)f))h o (S`_:h) NB. Inactive until someone else ends eating
  A`(amend o ((B P A)f))h o (B`2:h) NB. Activity: thinking while queuing
  Q`(Q , P)h                        NB. Enqueuing
  [ echo o (time , ' starts waiting and thinking about hunger.' ,~ P {:: N)
)

thinking=. enqueue`eat@.CanEat  NB. Either enqueues or eats after thinking

dequeue is
  P`({. o Q)h  NB. Activating the one in front of the queue
  eat          NB. and starts eating
  Q`(}. o Q)h  NB. dequeuing
)

eating is  NB. Thinks after eating
  T`(amend o ((S P T)f))h o (S`thinktime h) NB. Thinking time
  A`(amend o ((B P A)f))h o (B`0:        h) NB. Activity: thinking
  [ echo o ( time , ' starts thinking.' ,~ P {:: N)
  dequeue ^: (1 <: # o Q)     NB. dequeuing a philosopher (if possible)
)

update is
  E`(E + <./ o T)h            NB. Updating the elapsed time
  T`((- <./)@:T) h            NB. Updating the residual times
  P`(0 I. o = T) h            NB. Setting the active philosopher
  thinking`eating@.((P { A)z) NB. Was thinking or eating?
  C`(1 -~ C)     h            NB. One chronological event completed
)

simulate is NB. Discrete event simulation (dyadic verb)
  ;                           NB. Linking the arguments (N C)
  ,&(;:8$',')                 NB. Appending 8 local boxes (A B T S Q U P E)
  start
  update ^: (0 < C) ^: _      NB. Updating while events are less than C
  ''c
)

simulate=. simulate f.

NB. The simulation code is produced by the sentence,
NB. 77 (-@:[ ]\ 5!:5@<@:]) 'simulate'
```



## Java

This Java implementation uses a token system.  If a philosopher's number is on the token, they pick up their left and right forks.  Passing the token to their immediate neighbor would be pointless, so they increment the token by 2, passing it to the philosopher after their neighbor.  The +2 works well for odd numbers of philosophers.  With wait down at 1 millisecond I get about 1.5M eats/sec running 5 philosophers, down to about 0.5M eats/sec running 25.  The single token generates good availability for 80% of 5 forks, but a much lower availability % of 25 forks.

```Java

package diningphilosophers;

import java.util.ArrayList;
import java.util.Random;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;

enum PhilosopherState { Get, Eat, Pon }

class Fork {
    public static final int ON_TABLE = -1;
    static int instances = 0;
    public int id;
    public AtomicInteger holder = new AtomicInteger(ON_TABLE);

    Fork() { id = instances++; }
}

class Philosopher implements Runnable {
    static final int maxWaitMs = 100;                          //  must be > 0
    static AtomicInteger token = new AtomicInteger(0);
    static int instances = 0;
    static Random rand = new Random();
    AtomicBoolean end = new AtomicBoolean(false);
    int id;
    PhilosopherState state = PhilosopherState.Get;
    Fork left;
    Fork right;
    int timesEaten = 0;

    Philosopher() {
        id = instances++;
        left = Main.forks.get(id);
        right = Main.forks.get((id+1)%Main.philosopherCount);
    }

    void sleep() { try { Thread.sleep(rand.nextInt(maxWaitMs)); }
        catch (InterruptedException ex) {} }

    void waitForFork(Fork fork) {
        do {
            if (fork.holder.get() == Fork.ON_TABLE) {
                fork.holder.set(id);                //  my id shows I hold it
                return;
            } else {                                //  someone still holds it
                sleep();                            //  check again later
            }
        } while (true);
    }

    public void run() {
        do {
            if (state == PhilosopherState.Pon) {    //  all that pondering
                state = PhilosopherState.Get;       //  made me hungry
            } else { // ==PhilosopherState.Get
                if (token.get() == id) {            //  my turn now
                    waitForFork(left);
                    waitForFork(right);             //  Ah needs me some foahks!
                    token.set((id+2)% Main.philosopherCount);
                    state = PhilosopherState.Eat;
                    timesEaten++;
                    sleep();                        //  eat for a while
                    left.holder.set(Fork.ON_TABLE);
                    right.holder.set(Fork.ON_TABLE);
                    state = PhilosopherState.Pon;   //  ponder for a while
                    sleep();
                } else {                    //  token.get() != id, so not my turn
                    sleep();
                }
            }
        } while (!end.get());
    }
}

public class Main {
    static final int philosopherCount = 5; //  token +2 behavior good for odd #s
    static final int runSeconds = 15;
    static ArrayList<Fork> forks = new ArrayList<Fork>();
    static ArrayList<Philosopher> philosophers = new ArrayList<Philosopher>();

    public static void main(String[] args) {
        for (int i = 0 ; i < philosopherCount ; i++) forks.add(new Fork());
        for (int i = 0 ; i < philosopherCount ; i++)
            philosophers.add(new Philosopher());
        for (Philosopher p : philosophers) new Thread(p).start();
        long endTime = System.currentTimeMillis() + (runSeconds * 1000);

        do {                                                    //  print status
            StringBuilder sb = new StringBuilder("|");

            for (Philosopher p : philosophers) {
                sb.append(p.state.toString());
                sb.append("|");            //  This is a snapshot at a particular
            }                              //  instant.  Plenty happens between.

            sb.append("     |");

            for (Fork f : forks) {
                int holder = f.holder.get();
                sb.append(holder==-1?"   ":String.format("P%02d",holder));
                sb.append("|");
            }

            System.out.println(sb.toString());
            try {Thread.sleep(1000);} catch (Exception ex) {}
        } while (System.currentTimeMillis() < endTime);

        for (Philosopher p : philosophers) p.end.set(true);
        for (Philosopher p : philosophers)
            System.out.printf("P%02d: ate %,d times, %,d/sec\n",
                p.id, p.timesEaten, p.timesEaten/runSeconds);
    }
}

```



## JoCaml


### Minimal simple solution

This solution allows a philosopher to take only two forks at once, or none at all. This is achieved by making each fork into a channel, and guarding the eating process by two forks. There are two channels for each philosopher: a thinking philosopher and a hungry philosopher.

What this simple solution achieves:

*no deadlock (waiting for forks forever)
*no "livelock" (trying to pick up and put down forks forever)
*philosophers can eat at any time (no fixed order is imposed)

Deficiencies of this solution:

*Supports only a fixed set of philosophers, since all channels are declared statically. More philosophers needs more lines of code.
*The mean time of waiting while hungry is not bounded and grows very slowly (logarithmically) with time.


```jocaml
let random_wait n = Unix.sleep (Random.int n);;
let print s m = Printf.printf "philosopher %s is %s\n" s m; flush(stdout);;
let will_eat s = print s "eating"; random_wait 10;;
let will_think s = print s "thinking"; random_wait 20; print s "hungry";;

  (* a,b,c,d,e are thinking philosophers; ah,bh,ch,dh,eh are the same philosophers when hungry;
     fab is the fork located between philosophers a and b; similarly for fbc, fcd, ... *)

def  ah() & fab() & fea() = will_eat "Aristotle"; a() & fab() & fea()
 or  bh() & fab() & fbc() = will_eat "Kant";      b() & fab() & fbc()
 or  ch() & fbc() & fcd() = will_eat "Spinoza";   c() & fbc() & fcd()
 or  dh() & fcd() & fde() = will_eat "Marx";      d() & fcd() & fde()
 or  eh() & fde() & fea() = will_eat "Russell";   e() & fde() & fea()

 and a() = will_think "Aristotle"; ah()
 and b() = will_think "Kant";      bh()
 and c() = will_think "Spinoza";   ch()
 and d() = will_think "Marx";      dh()
 and e() = will_think "Russell";   eh()
;;
spawn fab() & fbc() & fcd() & fde() & fea() & a() & b() & c() & d() & e();;
```

Sample output:

```txt
philosopher Aristotle is thinking
philosopher Russell is thinking
philosopher Marx is thinking
philosopher Kant is thinking
philosopher Spinoza is thinking
philosopher Kant is hungry
philosopher Kant is eating
philosopher Kant is thinking
philosopher Russell is hungry
philosopher Russell is eating
philosopher Russell is thinking
philosopher Spinoza is hungry
philosopher Spinoza is eating
philosopher Spinoza is thinking
philosopher Spinoza is hungry
philosopher Spinoza is eating
philosopher Spinoza is thinking
philosopher Aristotle is hungry
philosopher Aristotle is eating
philosopher Marx is hungry
philosopher Marx is eating
philosopher Russell is hungry
philosopher Aristotle is thinking
philosopher Russell is eating
philosopher Marx is thinking
philosopher Kant is hungry
philosopher Kant is eating
philosopher Russell is thinking
philosopher Kant is thinking

```



### Simple solution with statistics

This solution is logically the same as the "minimal simple" solution above, but now the timing information is printed. Statistical information is also printed on hungry waiting time before eating: average among all instances of eating, and maximum time ever waited by anyone.


```jocaml
let print s t m = Printf.printf "t=%d: philosopher %s is %s\n" t s m; flush(stdout);;
let random_wait n = Unix.sleep (Random.int n);;

(* auxiliary function to keep track of time ticks, using integer seconds *)
def  ts () & counter(n) = counter(n) & reply n to ts
or   update_counter() & counter(n) = counter(n+1) & reply to update_counter
and  counter_sentinel() = Unix.sleep 1; update_counter(); counter_sentinel()
;;
spawn counter(0) & counter_sentinel();;

def stats(n, waited, maxwaited) & report_wait_time(m) =
 let (n', waited', maxwaited') = (n+1, waited+m, max maxwaited m) in
 Printf.printf "waiting average %f, max waited %d\n"
   (float_of_int waited' /. float_of_int n')
   maxwaited';
 flush(stdout);
 stats(n',waited',maxwaited') & reply () to report_wait_time
;;

spawn stats(0,0,0);;

let eat s t = print s t "eating"; random_wait 10;;
let think s = print s (ts()) "thinking"; random_wait 20;;

(* "p" will be a philosopher channel, to be defined later
 the messages ah, bh, ... do not need to be injected now. *)

let will_eat s t = let t' = ts() in report_wait_time(t'-t); eat s t';;

def ah(t,p) & fab() & fea() = will_eat "Aristotle" t; p() & fab() & fea()
or  bh(t,p) & fab() & fbc() = will_eat "Kant" t; p() & fab() & fbc()
or  ch(t,p) & fbc() & fcd() = will_eat "Spinoza" t; p() & fbc() & fcd()
or  dh(t,p) & fcd() & fde() = will_eat "Marx" t; p() & fcd() & fde()
or  eh(t,p) & fde() & fea() = will_eat "Russell" t; p() & fde() & fea()
;;

spawn fab() & fbc() & fcd() & fde() & fea();;

(* define the thinking -> hungry transitions using local philosophers, and inject the philosophers *)
List.map
 (fun (h,s) -> def p() = think s; let t = ts() in print s t "hungry"; h(t,p) in spawn p())
 [(ah,"Aristotle"); (bh,"Kant"); (ch,"Spinoza"); (dh,"Marx"); (eh,"Russell")]
;;
(* this replaces repetitive code such as that shown in the previous solution *)

(* now we need to wait and do nothing; nobody will be able to inject godot() *)
def wait_forever() & godot() = reply () to wait_forever in wait_forever();;
```

Sample output (excerpt):

```txt
t=2: philosopher Aristotle is thinking
t=3: philosopher Aristotle is hungry
waiting average 0.000000, max waited 0
t=3: philosopher Aristotle is eating
t=3: philosopher Aristotle is thinking
t=4: philosopher Russell is hungry
waiting average 0.000000, max waited 0
t=4: philosopher Russell is eating
t=5: philosopher Marx is hungry
t=5: philosopher Kant is hungry
waiting average 0.000000, max waited 0
t=5: philosopher Kant is eating
waiting average 0.666667, max waited 4
t=9: philosopher Marx is eating
t=9: philosopher Russell is thinking
t=14: philosopher Kant is thinking
t=17: philosopher Marx is thinking
t=18: philosopher Marx is hungry
waiting average 0.571429, max waited 4
t=18: philosopher Marx is eating
t=19: philosopher Spinoza is hungry
t=20: philosopher Aristotle is hungry
waiting average 0.500000, max waited 4
t=20: philosopher Aristotle is eating
t=24: philosopher Russell is hungry
waiting average 1.000000, max waited 5
t=24: philosopher Marx is thinking
t=24: philosopher Spinoza is eating
t=26: philosopher Kant is hungry
waiting average 1.300000, max waited 5
t=28: philosopher Russell is eating
t=28: philosopher Aristotle is thinking
t=31: philosopher Russell is thinking
t=33: philosopher Marx is hungry
waiting average 1.181818, max waited 5
t=33: philosopher Marx is eating
```



### Fair solution


This solution implements "fairness" -- if two neighbors are hungry, the one who waited more will eat first. The waiting time for each philosopher is bounded by twice the maximum eating time.


```jocaml
#!/usr/bin/jocamlrun jocaml

(* eating and thinking between 0 and this-1 *)
let eating_max_interval = 10;;
let thinking_max_interval = 10;;
let number_of_philosophers = 5;;
let random_wait n = Unix.sleep (Random.int n);;

(* counter for unique timestamp, not related to time in seconds *)
def get_current_time () & unique_ts_counter(n) = unique_ts_counter(n+1) & reply n to get_current_time;;
spawn unique_ts_counter(0);;

(* functions that wait and print diagnostics *)
let name i = List.nth ["Aristotle"; "Kant"; "Spinoza"; "Marx"; "Russell"] i;;
let message i m = Printf.printf "philosopher %s is %s\n" (name i) m; flush(stdout);;
let eat i = message i "eating"; random_wait eating_max_interval;;
let think i = message i "thinking"; random_wait thinking_max_interval;;

type philosopher_state_t = Eating | Hungry of int | Thinking;;

(* initial states *)
let states = Array.make number_of_philosophers Thinking;;
(* one philosopher's processes *)
let make_philosopher i got_hungry done_eating =
 def hungry() & forks() = eat i ; done_eating(i) & thinking()
 and thinking() = think i; got_hungry(i) & hungry()
 in spawn thinking(); forks
;;

(* deciding who will eat first *)
let next_phil i = (i+1) mod number_of_philosophers;;
let prev_phil i = (number_of_philosophers+i-1) mod number_of_philosophers;;
let is_hungry p = match p with
    | Hungry h -> true
    | _ -> false;;
let not_eating p = match p with
    | Eating -> false
    | _ -> true;;
let is_more_hungry p q = match q with
    | Hungry hj -> (
    	match p with
	    | Hungry hi -> hi <= hj
	    | _ -> false
    )
    | _ -> true
;;

let may_eat_first i =
  is_hungry states.(i)
  && not_eating states.(next_phil i) && not_eating states.(prev_phil i)
  && is_more_hungry states.(i) states.(next_phil i)
  && is_more_hungry states.(i) states.(prev_phil i);;

let decide_eating i =
 if (may_eat_first i) then (states.(i) <- Eating; true)
 else false;;

def waiter(all_forks) & got_hungry(i) =
 states.(i) <- Hungry (get_current_time());
 let will_eat = decide_eating i in (
 waiter(all_forks) & (if will_eat then all_forks.(i)() else 0)
)
or  waiter(all_forks) & done_eating(i) =
  states.(i) <- Thinking;
  let next_will_eat = decide_eating (next_phil i) in
  let prev_will_eat = decide_eating (prev_phil i) in (
 waiter(all_forks)
  & (if next_will_eat then all_forks.(next_phil i)() else 0)
  & (if prev_will_eat then all_forks.(prev_phil i)() else 0)
 );;

let all_forks = Array.init number_of_philosophers (fun i -> make_philosopher i got_hungry done_eating)
in spawn waiter(all_forks);;

(* now we need to wait and do nothing; nobody will be able to inject godot() *)

def wait_forever() & godot() = reply () to wait_forever in wait_forever();;
```


Sample output:

```txt
philosopher Aristotle is thinking
philosopher Kant is thinking
philosopher Marx is thinking
philosopher philosopher Spinoza is thinking
Russell is thinking
philosopher Spinoza is eating
philosopher Spinoza is thinking
philosopher Marx is eating
philosopher Marx is thinking
philosopher Marx is eating
philosopher Marx is thinking
philosopher Aristotle is eating
philosopher Aristotle is thinking
philosopher Kant iphilosopher s eating
Russell is eating
philosopher Russell is thinking
philosopher Kant is thinking
philosopher Spinoza is eating
philosopher Spinoza is thinking
philosopher Marx is eating

```



## Julia

Pentagonal table with assigned seats. Aristotle, seated on the north side, takes his left fork
first since he was left-handed, see historical note in http://time.com/3107557/top-10-lefties/
and the others take the right fork first. The forks are represented by 5 channels.
One lefty's taking left fork before right prevents deadlocks (see C solution).

```julia

mutable struct Philosopher
    name::String
    hungry::Bool
    righthanded::Bool
    rightforkheld::Channel
    leftforkheld::Channel
    function Philosopher(name, leftfork, rightfork)
        this = new()
        this.name = name
        this.hungry = rand([false, true]) # not specified so start as either
        this.righthanded   = (name == "Aristotle") ? false : true
        this.leftforkheld  = leftfork
        this.rightforkheld = rightfork
        this
    end
end

mutable struct FiveForkTable
    fork51::Channel
    fork12::Channel
    fork23::Channel
    fork34::Channel
    fork45::Channel
    function FiveForkTable()
        this = new()
        this.fork51 = Channel(1); put!(this.fork51, "fork") # start with one fork per channel
        this.fork12 = Channel(1); put!(this.fork12, "fork")
        this.fork23 = Channel(1); put!(this.fork23, "fork")
        this.fork34 = Channel(1); put!(this.fork34, "fork")
        this.fork45 = Channel(1); put!(this.fork45, "fork")
        this
    end
end


table = FiveForkTable();
tasks = [Philosopher("Aristotle", table.fork12, table.fork51),
         Philosopher("Kant", table.fork23, table.fork12),
         Philosopher("Spinoza", table.fork34, table.fork23),
         Philosopher("Marx", table.fork45, table.fork34),
         Philosopher("Russell", table.fork51, table.fork45)]

function dine(t,p)
    if p.righthanded
       take!(p.rightforkheld); println("$(p.name) takes right fork")
       take!(p.leftforkheld); println("$(p.name) takes left fork")
    else
       take!(p.leftforkheld); println("$(p.name) takes left fork")
       take!(p.rightforkheld); println("$(p.name) takes right fork")
    end
end

function leavetothink(t, p)
    put!(p.rightforkheld, "fork"); println("$(p.name) puts down right fork")
    put!(p.leftforkheld, "fork");  println("$(p.name) puts down left fork")
end

contemplate(t) = sleep(t)

function dophil(p, t, fullaftersecs=2.0, hungryaftersecs=10.0)
    while true
        if p.hungry
            println("$(p.name) is hungry")
            dine(table, p)
            sleep(fullaftersecs)
            p.hungry = false
            leavetothink(t, p)
        else
            println("$(p.name) is out of the dining room for now.")
            contemplate(hungryaftersecs)
            p.hungry = true
        end
    end
end

function runall(tasklist)
    for p in tasklist
        @async dophil(p, table)
    end
    while true begin sleep(5) end end
end

runall(tasks)

```

Aristotle is out of the dining room for now.
Kant is out of the dining room for now.
Spinoza is hungry
Spinoza takes right fork
Spinoza takes left fork
Marx is hungry
Russell is hungry
Russell takes right fork
Russell takes left fork
Spinoza puts down right fork
Spinoza puts down left fork
Spinoza is out of the dining room for now.
Marx takes right fork
Marx takes left fork
Russell puts down right fork
Russell puts down left fork
Russell is out of the dining room for now.
Marx puts down right fork
Marx puts down left fork
Marx is out of the dining room for now.
Aristotle is hungry
Aristotle takes left fork
Aristotle takes right fork
Kant is hungry
Aristotle puts down right fork
Aristotle puts down left fork
Aristotle is out of the dining room for now.
Kant takes right fork
Kant takes left fork
Spinoza is hungry
Russell is hungry
Russell takes right fork
Russell takes left fork
Kant puts down right fork
Kant puts down left fork
Kant is out of the dining room for now.
Spinoza takes right fork
Spinoza takes left fork
Marx is hungry
Russell puts down right fork
Russell puts down left fork
Russell is out of the dining room for now.
Spinoza puts down right fork
Spinoza puts down left fork
Spinoza is out of the dining room for now.
Marx takes right fork
Marx takes left fork
Marx puts down right fork
Marx puts down left fork
Marx is out of the dining room for now.
Aristotle is hungry
Aristotle takes left fork
Aristotle takes right fork
Aristotle puts down right fork
Aristotle puts down left fork
Aristotle is out of the dining room for now.
Kant is hungry
Kant takes right fork
Kant takes left fork
Russell is hungry
Russell takes right fork
Russell takes left fork
Kant puts down right fork
Kant puts down left fork
Kant is out of the dining room for now.
Spinoza is hungry
Spinoza takes right fork
Spinoza takes left fork
Russell puts down right fork
Russell puts down left fork
Russell is out of the dining room for now.


## Kotlin

As noted in the Groovy entry, deadlocks are avoided by always getting locks on forks with lower numbers first.

```scala
// Version 1.2.31

import java.util.Random
import java.util.concurrent.locks.Lock
import java.util.concurrent.locks.ReentrantLock

val rand = Random()

class Fork(val name: String) {
    val lock = ReentrantLock()

    fun pickUp(philosopher: String) {
        lock.lock()
        println("  $philosopher picked up $name")
    }

    fun putDown(philosopher: String) {
        lock.unlock()
        println("  $philosopher put down $name")
    }
}

class Philosopher(val pname: String, val f1: Fork, val f2: Fork) : Thread() {
    override fun run() {
        (1..20).forEach {
            println("$pname is hungry")
            f1.pickUp(pname)
            f2.pickUp(pname)
            println("$pname is eating bite $it")
            Thread.sleep(rand.nextInt(300) + 100L)
            f2.putDown(pname)
            f1.putDown(pname)
        }
    }
}

fun diningPhilosophers(names: List<String>) {
    val size = names.size
    val forks = List(size) { Fork("Fork ${it + 1}") }
    val philosophers = mutableListOf<Philosopher>()
    names.forEachIndexed { i, n ->
        var i1 = i
        var i2 = (i + 1) % size
        if (i2 < i1) {
            i1 = i2
            i2 = i
        }
        val p = Philosopher(n, forks[i1], forks[i2])
        p.start()
        philosophers.add(p)
    }
    philosophers.forEach { it.join() }
}

fun main(args: Array<String>) {
    val names = listOf("Aristotle", "Kant", "Spinoza", "Marx", "Russell")
    diningPhilosophers(names)
}
```



## Logtalk

Works when using SWI-Prolog, XSB, or YAP as the backend compiler:

```logtalk
:- category(chopstick).

    % chopstick actions (picking up and putting down) are synchronized using a notification
    % such that a chopstick can only be handled by a single philosopher at a time:

    :- public(pick_up/0).
    pick_up :-
        threaded_wait(available).

    :- public(put_down/0).
    put_down :-
        threaded_notify(available).

:- end_category.


:- object(cs1,
    imports(chopstick)).

    :- threaded.
    :- initialization(threaded_notify(available)).

:- end_object.


:- object(cs2,
    imports(chopstick)).

    :- threaded.
    :- initialization(threaded_notify(available)).

:- end_object.


:- object(cs3,
    imports(chopstick)).

    :- threaded.
    :- initialization(threaded_notify(available)).

:- end_object.


:- object(cs4,
    imports(chopstick)).

    :- threaded.
    :- initialization(threaded_notify(available)).

:- end_object.


:- object(cs5,
    imports(chopstick)).

    :- threaded.
    :- initialization(threaded_notify(available)).

:- end_object.


:- category(philosopher).

    :- public(left_chopstick/1).
    :- public(right_chopstick/1).
    :- public(run/2).

    :- private(message/1).
    :- synchronized(message/1).

    :- uses(random, [random/3]).

    run(0, _) :-
        this(Philosopher),
        message([Philosopher, ' terminated.']).

    run(Count, MaxTime) :-
        Count > 0,
        think(MaxTime),
        eat(MaxTime),
        Count2 is Count - 1,
        run(Count2, MaxTime).

    think(MaxTime):-
        this(Philosopher),
        random(1, MaxTime, ThinkTime),
        message(['Philosopher ', Philosopher, ' thinking for ', ThinkTime, ' seconds.']),
        thread_sleep(ThinkTime).

    eat(MaxTime):-
        this(Philosopher),
        random(1, MaxTime, EatTime),
        ::left_chopstick(LeftStick),
        ::right_chopstick(RightStick),
        LeftStick::pick_up,
        RightStick::pick_up,
        message(['Philosopher ', Philosopher, ' eating for ', EatTime, ' seconds with chopsticks ', LeftStick, ' and ', RightStick, '.']),
        thread_sleep(EatTime),
        ::LeftStick::put_down,
        ::RightStick::put_down.

    % writing a message needs to be synchronized as it's accomplished
    % using a combination of individual write/1 and nl/0 calls:
    message([]) :-
        nl,
        flush_output.
    message([Atom| Atoms]) :-
        write(Atom),
        message(Atoms).

:- end_category.


:- object(aristotle,
    imports(philosopher)).

    left_chopstick(cs1).
    right_chopstick(cs2).

:- end_object.


:- object(kant,
    imports(philosopher)).

    left_chopstick(cs2).
    right_chopstick(cs3).

:- end_object.


:- object(spinoza,
    imports(philosopher)).

    left_chopstick(cs3).
    right_chopstick(cs4).

:- end_object.


:- object(marx,
    imports(philosopher)).

    left_chopstick(cs4).
    right_chopstick(cs5).

:- end_object.


:- object(russell,
    imports(philosopher)).

    left_chopstick(cs1).    % change order so that the chopsticks are picked
    right_chopstick(cs5).   % in different order from the other philosophers

:- end_object.
```


## M2000 Interpreter

There are two kind of threads depends on thread plan. Each thread triggered in an interval. When the plan in sequwntial all the thread code executed. When the plan is concurrent then for each statement or block of code (in brackets) executed and then pass the execution to a thread mananager, so may another thread execute statement or block of statement. M2000 Intepreter compiled in Visual Basic 6 so its a single thread application, so threads are scheduled via a task manager inside interpreter. Any function we call from a thread executed stopping the polling of threads from task manager.

To handle deadlock we have to alter the way which a philosopher get the first fork, so we have a R variable to hold true (-1) if the right fork taken first. To get the other fork a philosopher must get the first one. The time for thinking varies. Also the time which a philosopher enter to table varies too.

In the output we have time in milliseconds when the philosopher status show eating or thinking.

Threads are code running in same scope when defined. From a thread we can see any variable or function or other modules local to module, but not the static variables of module because a thread has own static variables. Also we can change the time interval from the thread, and we do that defining the eating time and the thinking time. If we place all philosophers with R=0 we get soon a deadlock. Some philosophers eat more than other, but this depends on how long stay at thinking (maybe X1 to X5 of a base thinking time)

The Main.Task is a thread which display the eating counting for each philosopher, and the table with five values (Fork or No Fork). Also there is a counter which advance if table has no Forks and reset to zero if one or more forks exist. So if we get a deadlock the program stops after some seconds.

We can press Esc to stop program, or we can press right mouse button to stop it from code. In either way threads erased. If the code is in module A we can execute from M2000 console Test A to execute it diplaying the code, including the code in threads. We can execute statement by statement also pressing Next Step in Control Form (opened with Test statement).



```M2000 Interpreter

Module Dining_philosophers (whichplan) {
	Form 80, 32
	Document Doc$
	const nl$={
	}
	Print $(,12),  ' set column width to 12
	Pen 14
	Pen 15	{
		Doc$="Dining Philosophers"+nl$
		\\ we can change thread plan only if no threads defined
		if whichplan=1 then
			Doc$="Sequential threads - to execute exclusive one threads code"+nl$
			thread.plan sequential
			\\ need time_to_think>time_to_eat, but time_to_appear maybe the same for all
			time_to_think=150  ' one or more intervals
			time_to_eat=100 ' one interval to eat only
			time_to_appear=(150,150,150,150,150)
			Return time_to_appear, random(0,3):=300
		else
			Doc$="Concurrent threads  - to execute a statement or a block of code"+nl$
			thread.plan concurrent
			time_to_think=100  ' one or more intervals
			time_to_eat=50 ' one interval to eat only
			time_to_appear=(100,100,100,100,100)
			Return time_to_appear, random(1,4):=200
		end if
		Print #-2,Doc$
		Print @(0,2),"Press left mouse button to exit"
	}
	Pen 13 {Print "Aristotle", "Kant", "Spinoza", "Marx", "Russell"}
	enum philosopher {
		Aristotle, Kant, Spinoza, Marx, Russell
	}
	global enum forks {NoFork, Fork}
	RoundTable =(Fork, Fork, Fork, Fork, Fork)
	Getleft=lambda RoundTable (ph as philosopher) -> {
		where=(ph+4) mod 5
		= RoundTable#val(where)
		Return RoundTable, where:=NoFork
	}
	GetRight=lambda RoundTable (ph as philosopher) -> {
		where=ph mod 5
		=RoundTable#val(where)
		Return RoundTable, where:=NoFork
	}
	PlaceForks=lambda RoundTable (ph as philosopher) -> {
		Return RoundTable,  (ph+4) mod 5:=Fork, ph mod 5:=Fork
	}
	ShowTable=lambda RoundTable -> {
		m=each(RoundTable)
		while m
			print if$(array(m)=NoFork->"No Fork", "Fork"),
		end while
		Print
	}
	noforks=lambda RoundTable -> {
		k=0
		m=each(RoundTable)
		while m
			if array(m)=NoFork then k++
		end while
		=k=5
	}

	dim eattimes(1 to 5)=0
	def critical as long, basetick
	Document page$
	m=each(philosopher)
	while m {
		\\ we make 5 threads
		\\ a thread has module scope (except for own static variables, and stack of values)
		thread {
			Page$=format$("{0::-12} - ",tick-basetick)+eval$(f)+if$(forkL=Nofork or forkR=Nofork->" thinking",  " eating")+nl$
			if not think then
				{ \\ a block always run blocking all other threads
					eattimes(f)++
					Call PlaceForks(f) : forkL=Nofork:forkR=NoFork
					think=true :thread this interval  time_to_think*random(1,5)
				}
			else.if R then
					if forkR=Nofork then forkR=GetRight(f)
					if forkR=fork and forkL=Nofork then forkL=GetLeft(f)
					if forkL=fork then think=false:thread this interval  time_to_eat
			else
					if forkL=Nofork then forkL=GetLeft(f)
					if forkL=fork and forkR=Nofork then forkR=GetRight(f)
					if forkR=fork then think=false:thread this interval  time_to_eat
			end if
		} as a interval time_to_appear#val(m^)
		\\ a is a variable which hold the number of thread (as returned from task manager)
		\\ so we can get 5 times a new number.
		\\ for each thread we make some static variables (only for each thread)
		\\ this statement execute a line of code in thread a
		thread a execute static f=eval(m), think=true, forkL=NoFork, forkR=NoFork, R=Random(-1,0)
	}
	cls ,4  ' set split screen from fifth row
	\\ Main.Task is a thread also. Normaly exit if no other threads running in background
	\\ also serve a the wait loop for task manager (we can use Every 200 {} but isn't a thread, is a kind of a wait statement)
	\\ tick return the counter from  task manager which used to triger threads
	basetick=tick
	\\ 4hz display results
	Main.Task 1000/4 {
		{ \\ a block always run blocking all other threads
			cls
			Print $(1),eattimes()
			Print $(0)
			Print "Table:"
			Call ShowTable()
			if noforks() then critical++  else critical=0
			Print "noforks on table counter:";critical
			Print #-2,Page$
			Doc$=Page$
			Clear Page$
			if critical>40 or keypress(1) then break
		}
	}
	\\ because we can exit when some threads are alive, we can erase them now
	threads erase
	Clipboard Doc$
}
Dining_philosophers Random(1,2)

```


Sometime the above code exit with deadlock. One way to prevent this is to use another approach which we get two items from table, and if they are forks then we change status to eating, ore we place items back to table. All the code include in a block so no other thread executed in the background.
So the thread code can be this:


```M2000 Interpreter

		thread {
			Page$=format$("{0::-12} - ",tick-basetick)+eval$(f)+if$(forkL=Nofork or forkR=Nofork->" thinking",  " eating")+nl$
			if not think then
				{ \\ a block always run blocking all other threads
					eattimes(f)++
					Call PlaceForks(f) : forkL=Nofork:forkR=NoFork
					think=true :thread this interval  time_to_think*random(1,5)
				}
			else
				{
					forkR=GetRight(f) : forkL=GetLeft(f)
					if forkR=fork and forkL=fork then
						 think=false:thread this interval  time_to_eat
					else
						Return RoundTable,  f mod 5:=forkR,  (f+4) mod 5:=ForkL
					end if
				}
			end if
		} as a interval time_to_appear#val(m^)
		thread a execute static f=eval(m), think=true, forkL=NoFork, forkR=NoFork

```








<pre style="height:30ex;overflow:scroll">
Dining Philosophers
Sequential threads - to execute exclusive one threads code
        1257 - Kant thinking
        1258 - Spinoza thinking
        1259 - Marx thinking
        1260 - Russell thinking
        2073 - Kant eating
        2135 - Marx eating
        2144 - Aristotle thinking
..................................
       55163 - Marx eating
       55455 - Kant thinking
       56193 - Kant eating
       56394 - Russell thinking
       56812 - Spinoza thinking
       57192 - Russell eating
       57514 - Spinoza eating
       59056 - Spinoza thinking
       59060 - Aristotle thinking

Dining Philosophers
Concurrent threads  - to execute a statement or a block of code
         818 - Aristotle thinking
         825 - Kant thinking
         832 - Spinoza thinking
         839 - Marx thinking
        1610 - Aristotle thinking
        1617 - Kant thinking
        1618 - Spinoza thinking
        1626 - Russell thinking
....................................
       56585 - Marx eating
       56744 - Aristotle eating
       58311 - Kant thinking
       59397 - Kant eating
       60424 - Spinoza thinking
       61468 - Spinoza eating
       61974 - Russell thinking
</pre >

=={{header|Modula-3}}==
From this implementation's point of view, a "resource" is not a ''fork'', but rather a ''place at the table''. Rather than use one <code>MUTEX</code> for ''each'' fork, it uses one <code>MUTEX</code> for the entire table.

* Each philosopher starts on his feet, waits until the <code>MUTEX</code> allows him to look for an available seat, and looks to see if two forks are available ''at one place''.
* After determining whether a place is available, the philosopher does one of two things '''before''' releasing the <code>MUTEX</code>.
** If a place is available, he sits down, take both forks at the place (which must be free), and releases the <code>MUTEX</code>.
** If a place is not available, the philosopher does the following.
*** He notifies a "condition variable", comparable to what some restaurants call a "host", that he will wait on a place. ''He simultaneously releases the <code>MUTEX</code>.''
*** He receives the <code>MUTEX</code> again once the condition variable informs him that someone has risen from the table. (This brings us back to the first step.)
* When a philosopher has finished eating, he puts down both forks and rises to think a while.


'''Note.''' These philosophers actually follow the directions and spend a ''random'' amount of time eating and thinking.

It is easy to modify this so that each philosopher does not rise from the table, but eats and thinks only at his assigned place. In fact, the original implementation did precisely that, but when I saw that some implementations allowed the philosophers to sit at any place with two available forks, I opted for that.

While this implementation is not a translation of the [[#Eiffel|Eiffel]] solution, it still owes it a heads-up for the basic principle. Bertrand Meyer's ACM Webinar on [https://en.wikipedia.org/wiki/SCOOP_(software) SCOOP] directed my attention to this problem, and probably influenced the solution.

```modula3
MODULE DiningPhilosophers EXPORTS Main;

IMPORT IO, Random, Thread;

CONST

  PartySize = 5; (* modify for more/fewer philosophers *)

TYPE

  Closure = Thread.Closure OBJECT
  (* thread information *)
    which: [1..PartySize]; (* identifies the thread *)
  OVERRIDES
    apply := Live; (* procedure to execute *)
  END;

VAR

  (* how long to eat/think *)
  random: Random.T;

  (* controls access to resources *)
  test := NEW(MUTEX);
  forks := NEW(Thread.Condition); (* condition variable, used for signaling *)
  forkAvailable := ARRAY[1..PartySize] OF BOOLEAN {
    TRUE, TRUE, TRUE, TRUE, TRUE
  };
  (* the philosophers/tasks *)
  thread: ARRAY[1..PartySize] OF Thread.T;
  name := ARRAY[1..PartySize] OF TEXT {
    "Aristotle", "Kant", "Spinoza", "Marx", "Russell"
  };

PROCEDURE PlaceAvailable(): CARDINAL =
(*
  Determines whether a place is available at the table.
  If so, returns the place number. Otherwise, returns 0.
  We consider a place available if and only if *both* forks are free.
*)
BEGIN
  FOR i := 1 TO PartySize DO
    IF forkAvailable[i] AND forkAvailable[((i+1) MOD PartySize) + 1] THEN
      RETURN i;
    END;
  END;
  RETURN 0;
END PlaceAvailable;

PROCEDURE Live(philosopher: Closure): REFANY =
(* philosophers eat, sleep, ... and that's about it *)
VAR
  place: CARDINAL;
BEGIN
  WITH which = philosopher.which DO
    WHILE TRUE DO
      (* first make sure a place is available: both forks must be free! *)
      LOCK test DO
        place := PlaceAvailable();
        (* if not, release mutex and use condition variable to wait for one *)
        WHILE place = 0 DO
          IO.Put(name[which]); IO.Put(" starving!\n");
          Thread.Wait(test, forks);
          (* in Modula-3 we arrive here only if we have the lock again *)
          place := PlaceAvailable();
        END;
        (* a place has come available! seize the forks while mutex is locked *)
        forkAvailable[place] := FALSE;
        forkAvailable[(place MOD PartySize) + 1] := FALSE;
        IO.Put(name[which]); IO.Put(" eating at place "); IO.PutInt(place);
        IO.PutChar('\n');
      END;
      Thread.Pause(FLOAT(random.integer(1,3), LONGREAL));
      (* put down the forks *)
      forkAvailable[place]  := TRUE;
      forkAvailable[(place MOD PartySize) + 1] := TRUE;
      Thread.Signal(forks); (* signal the condition variable *)
      LOCK test DO
        IO.Put(name[which]); IO.Put(" thinking\n");
      END;
      Thread.Pause(FLOAT(random.integer(1,3), LONGREAL));
    END; (* WHILE *)
  END; (* WITH *)
  RETURN NIL;
END Live;

BEGIN
  random := NEW(Random.Default).init();
  (* bring philosophers to life *)
  FOR i := 1 TO PartySize DO
    thread[i] := Thread.Fork(NEW(Closure, apply := Live, which := i));
  END;
  (*
    We need to wait, otherwise the program will terminate,
    and the philosophers with it. Technically we could wait
    for just one philosopher, but in the interest of symmetry...
  *)
  FOR i := 1 TO PartySize DO
    EVAL Thread.Join(thread[i]);
  END;
END DiningPhilosophers.
```


```txt
Aristotle eating at place 1
Kant eating at place 3
Spinoza starving!
Marx starving!
Russell starving!
Aristotle thinking
Spinoza eating at place 5
Aristotle eating at place 2
Kant thinking
Marx starving!
Kant eating at place 4
Spinoza thinking
Russell starving!
...
```



## Nim

Prevents deadlocks by ordering the forks. Compile with <code>nim --threads:on c diningphilosophers.nim</code>

```nim
import threadpool, locks, math, os, random
# to call randomize() as a seed, need to import random module
randomize()

type Philosopher = ref object
  name: string
  food: string
  forkLeft, forkRight: int

const
  n = 5
  names = ["Aristotle", "Kant", "Spinoza", "Marx", "Russell"]
  foods = [" rat poison", " cockroaches", " dog food", " lemon-curd toast", " baked worms"]

var
  forks: array[n, Lock]
  phils: array[n, Philosopher]
  threads: array[n, Thread[Philosopher]]

proc run(p: Philosopher) {.thread.} =
  # random deprecated, use rand(x .. y)
  sleep rand(1..10) * 500
  echo p.name, " is hungry."

  acquire forks[min(p.forkLeft, p.forkRight)]
  sleep rand(1..5) * 500
  acquire forks[max(p.forkLeft, p.forkRight)]

  echo p.name, " starts eating", p.food, "."
  sleep rand(1..10) * 500

  echo p.name, " finishes eating", p.food, " and leaves to think."

  release forks[p.forkLeft]
  release forks[p.forkRight]

for i in 0..<n:
  initLock forks[i]
  phils[i] = Philosopher(
    name: names[i],
    food: foods[rand(0 .. n) mod n],
    forkLeft: i,
    forkRight: (i + 1) mod n
  )
  createThread(threads[i], run, phils[i])

joinThreads(threads)
```



## OxygenBasic


```oxygenbasic

'
### ===================

class RoundTableWith5Seats
'
### ===================


  % hungry    0
  % beingUsed 1
  % putDown   0
  % empty     0

  sys fork[5], plate[5],chair[5],philosopher[5]
  sys first

  method AddPasta() as sys
    function rand() as sys
      static seed=0x12345678
      mov eax,seed
      rol eax,7
      mul seed
      xor eax,0x5335ABD9
      mov seed,eax
      return seed
    end function
    return 4+(rand() and 15)
  end method

  method dine()
  first++ 'PRIORITY DINER
  if first>5 then first-=5
  for i=1 to 5
    kl=first+i-1
    kr=first+i
    if kl>5 then kl-=5
    if kr>5 then kr-=5
    if philosopher(kl) = hungry then
      if not fork(kl) or fork(kr) = beingUsed then
        plate(kl) = AddPasta()
        fork(kl)=beingUsed
        fork(kr)=beingUsed
      end if
    end if
    '
  next
  '
  for kl=1 to 5
    kr=kl+1 : if kr>5 then kr-=5
    if plate(kl)
      philosopher(kl)+=1 'PHILOSOPHER DINING
      --plate(kl)
      if plate(kl)=empty
        fork(kl)=PutDown
        fork(kr)=PutDown
      end if
    else
      if philosopher(kl)>0
        --philosopher(kl) 'PHILOSOPHER THINKING
      end if
    end if
  next
  '
  end method

  method show() as string
  cr=chr(13)+chr(10) : tab=chr(9)
  pr="philos" tab "activity" tab "plate" tab "fork L" tab "fork R" cr cr
  for i=1 to 5
  j=i+1 : if j>5 then j-=5
  if plate(i)=0 then
    if philosopher(i)=0 then
      act="waiting"
    else
      act="thinks"
    end if
  else
    act="dining"
  end if
  '
  pr+=i tab act tab plate(i) tab fork(i) tab fork(j) cr
  next
  return pr
  end method

end class

'TEST
'====

RoundTableWith5Seats Sopho
for i=1 to 100
  Sopho.dine
next

print Sopho.show
'putfile "s.txt",Sopho.show

'philos	action	plate	fork L	fork R
'
'1	waiting	0	0	1
'2	dining	8	1	1
'3	thinks	0	1	1
'4	dining	8	1	1
'5	thinks	0	1	0

```



## Oz

Using first-class computation spaces as transactions on dataflow variables. Computations spaces are normally used to implement constraint search engines. But here we use them to bind two variables atomically.


```oz
declare
  Philosophers = [aristotle kant spinoza marx russell]

  proc {Start}
     Forks = {MakeList {Length Philosophers}}
  in
     {ForAll Forks NewFork}
     for
        Name in Philosophers
        LeftFork in Forks
        RightFork in {RightShift Forks}
     do
        thread
           {Philosopher Name LeftFork RightFork}
        end
     end
  end

  proc {Philosopher Name LeftFork RightFork}
     for do
        {ShowInfo Name#" is hungry."}

        {TakeForks [LeftFork RightFork]}
        {ShowInfo Name#" got forks."}
        {WaitRandom}
        {ReleaseFork LeftFork}
        {ReleaseFork RightFork}

        {ShowInfo Name#" is thinking."}
        {WaitRandom}
     end
  end

  proc {WaitRandom}
     {Delay 1000 + {OS.rand} mod 4000} %% 1-5 seconds
  end

  proc {TakeForks Forks}
     {ForAll Forks WaitForFork}
     case {TryAtomically proc {$}
                            {ForAll Forks TakeFork}
                         end}
     of true then
        {ForAll Forks InitForkNotifier}
     [] false then
        {TakeForks Forks}
     end
  end

  %%
  %% Fork type
  %%

  %% A fork is a mutable reference to a pair
  fun {NewFork}
     {NewCell
      unit(taken:_     %% a fork is taken by setting this value to a unique value
           notify:unit %% to wait for a taken fork
	  )}
  end

  proc {TakeFork F}
     (@F).taken = {NewName}
  end

  proc {InitForkNotifier F}
     %% we cannot do this in TakeFork
     %% because side effect are not allowed in subordinate spaces
     New Old
  in
     {Exchange F Old New}
     New = unit(taken:Old.taken notify:_)
  end

  proc {ReleaseFork F}
     New Old
  in
     {Exchange F Old New}
     New = unit(taken:_ notify:unit)
     Old.notify = unit %% notify waiters
  end

  proc {WaitForFork F}
     {Wait (@F).notify}  %% returns immediatly if fork is free, otherwise blocks
  end

  %%
  %% Helpers
  %%

  %% Implements transactions on data flow variables
  %% with computation spaces. Returns success.
  fun {TryAtomically P}
     try
	S = {Space.new
	     proc {$ Sync}
		{P}
		Sync = unit
	     end}
     in
	{Space.askVerbose S} \= failed = true
	{Wait {Space.merge S}}
	true
     catch _ then
	false
     end
  end

  fun {RightShift Xs} %% circular
     case Xs of nil then nil
     else {Append Xs.2 [Xs.1]}
     end
  end

  ShowInfo = System.showInfo
in
  {Start}
```


## Pascal

This FreePascal implementation uses the idea of ordered resourses, each fork is numbered by index in the array.

In order to avoid deadlocks each member first takes fork with lower number.

```Pascal

program dining_philosophers;
{$mode objfpc}{$H+}
uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, SyncObjs;
const
  PHIL_COUNT   = 5;
  LIFESPAN     = 7;
  DELAY_RANGE  = 950;
  DELAY_LOW    = 50;
  PHIL_NAMES: array[1..PHIL_COUNT] of string = ('Aristotle', 'Kant', 'Spinoza', 'Marx', 'Russell');
type
  TFork        = TCriticalSection;
  TPhilosopher = class;
var
  Forks: array[1..PHIL_COUNT] of TFork;
  Philosophers: array[1..PHIL_COUNT] of TPhilosopher;
type
  TPhilosopher = class(TThread)
  private
    FName: string;
    FFirstFork, FSecondFork: TFork;
  protected
    procedure Execute; override;
  public
    constructor Create(const aName: string; aForkIdx1, aForkIdx2: Integer);
  end;

procedure TPhilosopher.Execute;
var
  LfSpan: Integer = LIFESPAN;
begin
  while LfSpan > 0 do
    begin
      Dec(LfSpan);
      WriteLn(FName, ' sits down at the table');
      FFirstFork.Acquire;
      FSecondFork.Acquire;
      WriteLn(FName, ' eating');
      Sleep(Random(DELAY_RANGE) + DELAY_LOW);
      FSecondFork.Release;
      FFirstFork.Release;
      WriteLn(FName, ' is full and leaves the table');
      if LfSpan = 0 then
        continue;
      WriteLn(FName, ' thinking');
      Sleep(Random(DELAY_RANGE) + DELAY_LOW);
      WriteLn(FName, ' is hungry');
    end;
end;

constructor TPhilosopher.Create(const aName: string; aForkIdx1, aForkIdx2: Integer);
begin
  inherited Create(True);
  FName := aName;
  if aForkIdx1 < aForkIdx2 then
    begin
      FFirstFork := Forks[aForkIdx1];
      FSecondFork := Forks[aForkIdx2];
    end
  else
    begin
      FFirstFork := Forks[aForkIdx2];
      FSecondFork := Forks[aForkIdx1];
    end;
end;

procedure DinnerBegin;
var
  I: Integer;
  Phil: TPhilosopher;
begin
  for I := 1 to PHIL_COUNT do
    Forks[I] := TFork.Create;
  for I := 1 to PHIL_COUNT do
    Philosophers[I] := TPhilosopher.Create(PHIL_NAMES[I], I, Succ(I mod PHIL_COUNT));
  for Phil in Philosophers do
    Phil.Start;
end;

procedure WaitForDinnerOver;
var
  Phil: TPhilosopher;
  Fork: TFork;
begin
  for Phil in Philosophers do
    begin
      Phil.WaitFor;
      Phil.Free;
    end;
  for Fork in Forks do
    Fork.Free;
end;

begin
  Randomize;
  DinnerBegin;
  WaitForDinnerOver;
end.

```

The next variant exploits the idea of ​​a single left-handed philosopher.

```Pascal

program dining_philosophers2;
{$mode objfpc}{$H+}
uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, SyncObjs;
const
  PHIL_COUNT   = 5;
  LIFESPAN     = 7;
  DELAY_RANGE  = 950;
  DELAY_LOW    = 50;
  PHIL_NAMES: array[1..PHIL_COUNT] of string = ('Aristotle', 'Kant', 'Spinoza', 'Marx', 'Russell');
type
  TFork        = TCriticalSection;
  TPhilosopher = class;
var
  Forks: array[1..PHIL_COUNT] of TFork;
  Philosophers: array[1..PHIL_COUNT] of TPhilosopher;
type
  TPhilosopher = class(TThread)
  private
    FName: string;
    FLeftFork, FRightFork: TFork;
    FLefty: Boolean;
    procedure SetLefty(aValue: Boolean);
    procedure SwapForks;
  protected
    procedure Execute; override;
  public
    constructor Create(const aName: string; aForkIdx1, aForkIdx2: Integer);
    property Lefty: Boolean read FLefty write SetLefty;
  end;

procedure TPhilosopher.SetLefty(aValue: Boolean);
begin
  if Lefty = aValue then
    exit;
  FLefty := aValue;
  SwapForks;
end;

procedure TPhilosopher.SwapForks;
var
  Fork: TFork;
begin
  Fork := FLeftFork;
  FLeftFork := FRightFork;
  FRightFork := Fork;
end;

procedure TPhilosopher.Execute;
var
  LfSpan: Integer = LIFESPAN;
begin
  while LfSpan > 0 do
    begin
      Dec(LfSpan);
      WriteLn(FName, ' sits down at the table');
      FLeftFork.Acquire;
      FRightFork.Acquire;
      WriteLn(FName, ' eating');
      Sleep(Random(DELAY_RANGE) + DELAY_LOW);
      FRightFork.Release;
      FLeftFork.Release;
      WriteLn(FName, ' is full and leaves the table');
      if LfSpan = 0 then
        continue;
      WriteLn(FName, ' thinking');
      Sleep(Random(DELAY_RANGE) + DELAY_LOW);
      WriteLn(FName, ' is hungry');
    end;
end;

constructor TPhilosopher.Create(const aName: string; aForkIdx1, aForkIdx2: Integer);
begin
  inherited Create(True);
  FName := aName;
  FLeftFork := Forks[aForkIdx1];
  FRightFork := Forks[aForkIdx2];
end;

procedure DinnerBegin;
var
  I: Integer;
  Phil: TPhilosopher;
begin
  for I := 1 to PHIL_COUNT do
    Forks[I] := TFork.Create;
  for I := 1 to PHIL_COUNT do
    Philosophers[I] := TPhilosopher.Create(PHIL_NAMES[I], I, Succ(I mod PHIL_COUNT));
  Philosophers[Succ(Random(5))].Lefty := True;
  for Phil in Philosophers do
    Phil.Start;
end;

procedure WaitForDinnerOver;
var
  Phil: TPhilosopher;
  Fork: TFork;
begin
  for Phil in Philosophers do
    begin
      Phil.WaitFor;
      Phil.Free;
    end;
  for Fork in Forks do
    Fork.Free;
end;

begin
  Randomize;
  DinnerBegin;
  WaitForDinnerOver;
end.

```

Another way to avoid deadlock: if a philosopher takes left fork but cannot take the right fork, he returns the left fork.

```Pascal

program dining_philosophers3;
{$mode objfpc}{$H+}
uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, SyncObjs;
const
  PHIL_COUNT   = 5;
  LIFESPAN     = 7;
  DELAY_RANGE  = 950;
  DELAY_LOW    = 50;
  PHIL_NAMES: array[1..PHIL_COUNT] of string = ('Aristotle', 'Kant', 'Spinoza', 'Marx', 'Russell');
type
  TFork        = TCriticalSection;
  TPhilosopher = class;
var
  Forks: array[1..PHIL_COUNT] of TFork;
  Philosophers: array[1..PHIL_COUNT] of TPhilosopher;
type
  TPhilosopher = class(TThread)
  private
    FName: string;
    FLeftFork, FRightFork: TFork;
  protected
    procedure Execute; override;
  public
    constructor Create(const aName: string; aForkIdx1, aForkIdx2: Integer);
  end;

procedure TPhilosopher.Execute;
var
  LfSpan: Integer = LIFESPAN;
begin
  while LfSpan > 0 do
    begin
      Dec(LfSpan);
      WriteLn(FName, ' sits down at the table');
      repeat
        FLeftFork.Acquire;
        if not FRightFork.TryEnter then
          begin
            FLeftFork.Release;
            Sleep(Random(DELAY_RANGE));
            continue;
          end;
        break;
      until False;
      WriteLn(FName, ' eating');
      Sleep(Random(DELAY_RANGE) + DELAY_LOW);
      FRightFork.Release;
      FLeftFork.Release;
      WriteLn(FName, ' is full and leaves the table');
      if LfSpan = 0 then
        continue;
      WriteLn(FName, ' thinking');
      Sleep(Random(DELAY_RANGE) + DELAY_LOW);
      WriteLn(FName, ' is hungry');
    end;
end;

constructor TPhilosopher.Create(const aName: string; aForkIdx1, aForkIdx2: Integer);
begin
  inherited Create(True);
  FName := aName;
  FLeftFork := Forks[aForkIdx1];
  FRightFork := Forks[aForkIdx2];
end;

procedure DinnerBegin;
var
  I: Integer;
  Phil: TPhilosopher;
begin
  for I := 1 to PHIL_COUNT do
    Forks[I] := TFork.Create;
  for I := 1 to PHIL_COUNT do
    Philosophers[I] := TPhilosopher.Create(PHIL_NAMES[I], I, Succ(I mod PHIL_COUNT));
  for Phil in Philosophers do
    Phil.Start;
end;

procedure WaitForDinnerOver;
var
  Phil: TPhilosopher;
  Fork: TFork;
begin
  for Phil in Philosophers do
    begin
      Phil.WaitFor;
      Phil.Free;
    end;
  for Fork in Forks do
    Fork.Free;
end;

begin
  Randomize;
  DinnerBegin;
  WaitForDinnerOver;
end.

```

And the last: deadlock can only happen if all the members are seated at the table.

This variant tries to avoid this situation.

```Pascal

program dining_philosophers4;
{$mode objfpc}{$H+}
uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, SyncObjs;
const
  PHIL_COUNT   = 5;
  LIFESPAN     = 7;
  DELAY_RANGE  = 950;
  DELAY_LOW    = 50;
  PHIL_NAMES: array[1..PHIL_COUNT] of string = ('Aristotle', 'Kant', 'Spinoza', 'Marx', 'Russell');
type
  TFork        = TCriticalSection;
  TPhilosopher = class;
var
  Forks: array[1..PHIL_COUNT] of TFork;
  Philosophers: array[1..PHIL_COUNT] of TPhilosopher;
  StilDining: Integer = 0;
procedure WaitForPlaceFree;
begin
  repeat
    if InterlockedIncrement(StilDining) > Pred(PHIL_COUNT) then
      begin
        InterlockedDecrement(StilDining);
        Sleep(Random(DELAY_LOW));
        continue;
      end;
    exit;
  until False;
end;

procedure FreePlace;
begin
  InterLockedDecrement(StilDining);
end;

type
  TPhilosopher = class(TThread)
  private
    FName: string;
    FLeftFork, FRightFork: TFork;
  protected
    procedure Execute; override;
  public
    constructor Create(const aName: string; aForkIdx1, aForkIdx2: Integer);
  end;

procedure TPhilosopher.Execute;
var
  LfSpan: Integer = LIFESPAN;
begin
  while LfSpan > 0 do
    begin
      Dec(LfSpan);
      WaitForPlaceFree;
      WriteLn(FName, ' sits down at the table');
      FLeftFork.Acquire;
      FRightFork.Acquire;
      WriteLn(FName, ' eating');
      Sleep(Random(DELAY_RANGE) + DELAY_LOW);
      FRightFork.Release;
      FLeftFork.Release;
      FreePlace;
      WriteLn(FName, ' is full and leaves the table');
      if LfSpan = 0 then
        continue;
      WriteLn(FName, ' thinking');
      Sleep(Random(DELAY_RANGE) + DELAY_LOW);
      WriteLn(FName, ' is hungry');
    end;
end;

constructor TPhilosopher.Create(const aName: string; aForkIdx1, aForkIdx2: Integer);
begin
  inherited Create(True);
  FName := aName;
  FLeftFork := Forks[aForkIdx1];
  FRightFork := Forks[aForkIdx2];
end;

procedure DinnerBegin;
var
  I: Integer;
  Phil: TPhilosopher;
begin
  for I := 1 to PHIL_COUNT do
    Forks[I] := TFork.Create;
  for I := 1 to PHIL_COUNT do
    Philosophers[I] := TPhilosopher.Create(PHIL_NAMES[I], I, Succ(I mod PHIL_COUNT));
  for Phil in Philosophers do
    Phil.Start;
end;

procedure WaitForDinnerOver;
var
  Phil: TPhilosopher;
  Fork: TFork;
begin
  for Phil in Philosophers do
    begin
      Phil.WaitFor;
      Phil.Free;
    end;
  for Fork in Forks do
    Fork.Free;
end;

begin
  Randomize;
  DinnerBegin;
  WaitForDinnerOver;
end.

```



## Perl

This solution requires that perl have been compiled with threads enabled.

Deadlock is prevented by having even numbered and odd numbered philosophers
grab their forks in opposite orders.


```Perl

use threads;
use threads::shared;
my @names = qw(Aristotle Kant Spinoza Marx Russell);

my @forks = ('On Table') x @names;
share $forks[$_] for 0 .. $#forks;

sub pick_up_forks {
   my $philosopher = shift;
   my ($first, $second) = ($philosopher, $philosopher-1);
   ($first, $second) = ($second, $first) if $philosopher % 2;
   for my $fork ( @forks[ $first, $second ] ) {
      lock $fork;
      cond_wait($fork) while $fork ne 'On Table';
      $fork = 'In Hand';
   }
}

sub drop_forks {
   my $philosopher = shift;
   for my $fork ( @forks[$philosopher, $philosopher-1] ) {
      lock $fork;
      die unless $fork eq 'In Hand';
      $fork = 'On Table';
      cond_signal($fork);
   }
}

sub philosopher {
   my $philosopher = shift;
   my $name = $names[$philosopher];
   for my $meal ( 1..5 ) {
      print $name, " is pondering\n";
      sleep 1 + rand 8;
      print $name, " is hungry\n";
      pick_up_forks( $philosopher );
      print $name, " is eating\n";
      sleep 1 + rand 8;
      drop_forks( $philosopher );
   }
   print $name, " is done\n";
}

my @t = map { threads->new(\&philosopher, $_) } 0 .. $#names;
for my $thread ( @t ) {
   $thread->join;
}

print "Done\n";
__END__

```



### =One solution based on Coro and AnyEvent=

To prevent deadlock the philosophers must not start eating at the same time and the time between getting the first fork and getting second one must be shorter as possible.


```Perl

#!/usr/bin/perl
use common::sense;
use Coro;
use AnyEvent;
use Coro::AnyEvent;
use EV;

my @philosophers = qw(Aristotle Kant Spinoza Marx Russell);
my @forks = (1..@philosophers);
my @fork_sem;

$fork_sem[$_] = new Coro::Semaphore for (0..$#philosophers);


for(my $i = $#philosophers; $i >= 0; $i--){
	say $philosophers[$i] . " has fork #" . $forks[$i] . " and fork #" . $forks[$i-1];
	async {
		my ($name, ,$no, $forks_got) = (@_);

		$Coro::current->{desc} = $name;
		Coro::AnyEvent::sleep(rand 4);

		while(1){
			say $name . " is hungry.";
			$$forks_got[$no]->down();
			Coro::AnyEvent::sleep(rand 1); #Let's make deadlock!
			$$forks_got[$no-1]->down();
			say $name . " is eating.";
			Coro::AnyEvent::sleep(1 + rand 8);

			$$forks_got[$no]->up();
			$$forks_got[$no-1]->up();

			say $name . " is thinking.";
			Coro::AnyEvent::sleep(1 + rand 8);
		}
	}($philosophers[$i], $i, \@fork_sem);
}

EV::loop;

```



## Perl 6

We use locking mutexes for the forks, and a lollipop to keep the last person who finished eating from getting hungry until the next person finishes eating, which prevents a cycle of dependency from forming.  The algorithm should scale to many philosophers, and no philosopher need be singled out to be left-handed, so it's fair in that sense.

```perl6
class Fork {
    has $!lock = Lock.new;
    method grab($who, $which) {
	say "$who grabbing $which fork";
	$!lock.lock;
    }
    method drop($who, $which) {
	say "$who dropping $which fork";
	$!lock.unlock;
    }
}

class Lollipop {
    has $!channel = Channel.new;
    method mine($who) { $!channel.send($who) }
    method yours { $!channel.receive }
}

sub dally($sec) { sleep 0.01 + rand * $sec }

sub MAIN(*@names) {
    @names ||= <Aristotle Kant Spinoza Marx Russell>;

    my @lfork = Fork.new xx @names;
    my @rfork = @lfork.rotate;

    my $lollipop = Lollipop.new;
    start { $lollipop.yours; }

    my @philosophers = do for flat @names Z @lfork Z @rfork -> $n, $l, $r {
	start {
	    sleep 1 + rand*4;
	    loop {
		$l.grab($n,'left');
		dally 1;  # give opportunity for deadlock
		$r.grab($n,'right');
		say "$n eating";
		dally 10;
		$l.drop($n,'left');
		$r.drop($n,'right');

		$lollipop.mine($n);
		sleep 1;  # lick at least once
		say "$n lost lollipop to $lollipop.yours(), now digesting";

		dally 20;
	    }
	}
    }
    sink await @philosophers;
}
```

```txt
Aristotle grabbing left fork
Aristotle grabbing right fork
Aristotle eating
Marx grabbing left fork
Marx grabbing right fork
Marx eating
Spinoza grabbing left fork
Aristotle dropping left fork
Aristotle dropping right fork
Russell grabbing left fork
Kant grabbing left fork
Kant grabbing right fork
Spinoza grabbing right fork
Marx dropping left fork
Marx dropping right fork
Aristotle lost lollipop to Marx, now digesting
Spinoza eating
Spinoza dropping left fork
Spinoza dropping right fork
Kant eating
Russell grabbing right fork
Russell eating
Marx lost lollipop to Spinoza, now digesting
Kant dropping left fork
Kant dropping right fork
Spinoza lost lollipop to Kant, now digesting
Russell dropping left fork
Russell dropping right fork
Kant lost lollipop to Russell, now digesting
Spinoza grabbing left fork
Spinoza grabbing right fork
Spinoza eating
Aristotle grabbing left fork
Aristotle grabbing right fork
Aristotle eating
Aristotle dropping left fork
Aristotle dropping right fork
Russell lost lollipop to Aristotle, now digesting
Spinoza dropping left fork
Spinoza dropping right fork
Aristotle lost lollipop to Spinoza, now digesting
Russell grabbing left fork
Marx grabbing left fork
Russell grabbing right fork
Russell eating
Marx grabbing right fork
Kant grabbing left fork
Kant grabbing right fork
Kant eating
Russell dropping left fork
Russell dropping right fork
Spinoza lost lollipop to Russell, now digesting
Marx eating
Spinoza grabbing left fork
Kant dropping left fork
Kant dropping right fork
Russell lost lollipop to Kant, now digesting
Spinoza grabbing right fork
^C
```



## Phix

Deadlocks are avoided by always getting the lowest numbered fork first.

You can substitute the indicated line for Russell to prove that it does indeed deadlock when the program fails to follow that rule.

If you uncomment the sleep(1)s you will probably want do the same to the terminate checks, otherwise after keying 'Q' or Escape it could take 20 seconds per diner to finish.

```Phix
integer fork1 = init_cs(),
        fork2 = init_cs(),
        fork3 = init_cs(),
        fork4 = init_cs(),
        fork5 = init_cs()
integer terminate = 0                   -- control flag

procedure person(sequence name, atom left_fork, atom right_fork)
-- (except Russell, who gets left and right the other way round)
    while terminate=0 do
        enter_cs(left_fork)
        enter_cs(right_fork)
        puts(1, name & " grabs forks.\n")
        for i=1 to rand(10) do
--          if terminate then exit end if
            puts(1, name & " is eating.\n")
--          sleep(1)
        end for
        puts(1, name & " puts forks down and leaves the dinning room.\n")
        leave_cs(left_fork)
        leave_cs(right_fork)
        for i=1 to rand(10) do
--          if terminate then exit end if
            puts(1, name & " is thinking.\n")
--          sleep(1)
        end for
        puts(1, name & " becomes hungry.\n")
    end while
end procedure
constant r_person = routine_id("person")

constant threads = {create_thread(r_person,{"Aristotle",fork1,fork2}),
                    create_thread(r_person,{"Kant",fork2,fork3}),
                    create_thread(r_person,{"Spinoza",fork3,fork4}),
                    create_thread(r_person,{"Marx",fork4,fork5}),
--                  create_thread(r_person,{"Russell",fork5,fork1})}    -- this will deadlock!
                    create_thread(r_person,{"Russell",fork1,fork5})}

constant ESC = #1B
while not find(get_key(),{ESC,'q','Q'}) do
    sleep(1)
end while
terminate = 1
wait_thread(threads)    -- (not strictly necessary)
delete_cs(fork1)        -- ""
delete_cs(fork2)
delete_cs(fork3)
delete_cs(fork4)
delete_cs(fork5)
```



## PicoLisp

This following solution uses the built-in fininte state machine function
'[http://software-lab.de/doc/refS.html#state state]'. Deadlocks are avoided, as
each philosopher releases the first fork if he doesn't succeed to obtain the
second fork, and waits for a random time.

Another solution, using the Chandy/Misra method, can be found
[http://logand.com/sw/phil.l here].

```PicoLisp
(de dining (Name State)
   (loop
      (prinl Name ": " State)
      (state 'State                       # Dispatch according to state
         (thinking 'hungry)               # If thinking, get hungry
         (hungry                          # If hungry, grab random fork
            (if (rand T)
               (and (acquire leftFork) 'leftFork)
               (and (acquire rightFork) 'rightFork) ) )
         (hungry 'hungry                  # Failed, stay hungry for a while
            (wait (rand 1000 3000)) )
         (leftFork                        # If holding left fork, try right one
            (and (acquire rightFork) 'eating)
            (wait 2000) )                 # then eat for 2 seconds
         (rightFork                       # If holding right fork, try left one
            (and (acquire leftFork) 'eating)
            (wait 2000) )                 # then eat for 2 seconds
         ((leftFork rightFork) 'hungry    # Otherwise, go back to hungry,
            (release (val State))         # release left or right fork
            (wait (rand 1000 3000)) )     # and stay hungry
         (eating 'thinking             # After eating, resume thinking
            (release leftFork)
            (release rightFork)
            (wait 6000) ) ) ) )           # for 6 seconds

(setq *Philosophers
   (maplist
      '((Phils Forks)
         (let (leftFork (tmp (car Forks))  rightFork (tmp (cadr Forks)))
            (or
               (fork)  # Parent: Collect child process IDs
               (dining (car Phils) 'hungry) ) ) )  # Initially hungry
      '("Aristotle" "Kant" "Spinoza" "Marx" "Russell")
      '("ForkA" "ForkB" "ForkC" "ForkD" "ForkE" .) ) )

(push '*Bye '(mapc kill *Philosophers))  # Terminate all upon exit
```

Output:

```txt
Aristotle: hungry
Aristotle: rightFork
Kant: hungry
Kant: rightFork
Spinoza: hungry
Spinoza: rightFork
Marx: hungry
Marx: rightFork
Russell: hungry
Marx: hungry
Spinoza: hungry
Kant: hungry
Russell: hungry
Aristotle: eating
...
```



## Pike


using Pike Backend call_out(), this solution avoids deadlocks by adding a 20% chance that a philosopher drops the fork if he can't pick up both.


```Pike
class Philosopher
{
    string name;
    object left;
    object right;

    void create(string _name, object _left, object _right)
    {
        name = _name;
        left = _left;
        right = _right;
    }

    void take_forks()
    {
        if (left->take(this) && right->take(this))
        {
            write("%s is EATING\n", name);
            call_out(drop_forks, random(30));
        }
        else
        {
            write("%s is WAITING\n", name);
            if (random(10) >= 8)
                drop_forks();
            call_out(take_forks, random(10));
        }
    }

    void drop_forks()
    {
        left->drop(this);
        right->drop(this);
        write("%s is THINKING\n", name);
        call_out(take_forks, random(30));
    }
}

class Fork
{
    int number;
    Philosopher user;

    void create(int _number)
    {
        number = _number;
    }

    int take(object new_user)
    {
        if (!user)
        {
            write("%s takes fork %d\n", new_user->name, number);
            user = new_user;
            return 1;
        }
        else if (new_user == user)
        {
            write("%s has fork %d\n", new_user->name, number);
            return 1;
        }
        else
            write("%s tries to take fork %d from %s\n", new_user->name, number, user->name);
    }

    void drop(object old_user)
    {
        if (old_user == user)
        {
            write("%s drops fork %d\n", old_user->name, number);
            user = 0;
        }
    }
}

int main(int argc, array argv)
{

  array forks = ({ Fork(1), Fork(2), Fork(3), Fork(4), Fork(5) });
  array philosophers = ({
                           Philosopher("einstein", forks[0], forks[1]),
                           Philosopher("plato", forks[1], forks[2]),
                           Philosopher("sokrates", forks[2], forks[3]),
                           Philosopher("chomsky", forks[3], forks[4]),
                           Philosopher("archimedes", forks[4], forks[0]),
                        });

  call_out(philosophers[0]->take_forks, random(5));
  call_out(philosophers[1]->take_forks, random(5));
  call_out(philosophers[2]->take_forks, random(5));
  call_out(philosophers[3]->take_forks, random(5));
  call_out(philosophers[4]->take_forks, random(5));
  return -1;
}
```



## Prolog

Works with SWI-Prolog and XPCE.

Use the same solution as in Erlang (a waiter gives the forks to philosophers).

Bonus : the code of an animation in XPCE is given, and statistics are displayed at the end of the process.

```Prolog
dining_philosophers :-
	new(D, window('Dining philosophers')),
	new(S, window('Dining philosophers : statistics')),
	send(D, size, new(_, size(800,800))),

	new(E, ellipse(400,400)),
	send(E, center, point(400,400)),
	send(D, display, E),

	new(F1, fork(0)),
	new(F2, fork(1)),
	new(F3, fork(2)),
	new(F4, fork(3)),
	new(F5, fork(4)),

	send_list(D, display, [F1,F2,F3,F4,F5]),

	new(Waiter, waiter(F1, F2, F3, F4, F5)),

	create_plate(P1, 0),
	create_plate(P2, 1),
	create_plate(P3, 2),
	create_plate(P4, 3),
	create_plate(P5, 4),

	create_point(0, Pt1),
	create_point(1, Pt2),
	create_point(2, Pt3),
	create_point(3, Pt4),
	create_point(4, Pt5),


	new(Ph1, philosopher('Aristotle', Waiter, P1, D, S, 0, Pt1, left)),
	new(Ph2, philosopher('Kant', Waiter, P2, D, S, 1, Pt2, left)),
	new(Ph3, philosopher('Spinoza', Waiter, P3, D, S, 2, Pt3, right)),
	new(Ph4, philosopher('Marx', Waiter, P4, D, S, 3, Pt4, right)),
	new(Ph5, philosopher('Russell', Waiter, P5, D, S, 4, Pt5, left)),

	send(Waiter, init_phi, Ph1, Ph2, Ph3, Ph4, Ph5),

	send_list([Ph1, Ph2, Ph3, Ph4, Ph5], start),

	send(D, done_message, and(message(Waiter, free),
				 message(Ph1, free),
				 message(Ph2, free),
				 message(Ph3, free),
				 message(Ph4, free),
				 message(Ph5, free),
				 message(S, open),
				 message(D, destroy))),

	send(D, open).


create_plate(P, N) :-
	new(P, ellipse(80,80)),
	X is 400 + 140 * cos(N * pi / 2.5),
	Y is 400 + 140 * sin(N * pi / 2.5),
	send(P, center, point(X, Y)).

create_point(N, point(X, Y)) :-
	X is 400 + 220 * cos(N * pi / 2.5),
	Y is 400 + 220 * sin(N * pi / 2.5) - 20.


:- pce_begin_class(waiter , object, "gives the forks to the philosophers").
variable(f1, fork, both, "free or used").
variable(f2, fork, both, "free or used").
variable(f3, fork, both, "free or used").
variable(f4, fork, both, "free or used").
variable(f5, fork, both, "free or used").
variable(phi1, philosopher, both, "philosopher").
variable(phi2, philosopher, both, "philosopher").
variable(phi3, philosopher, both, "philosopher").
variable(phi4, philosopher, both, "philosopher").
variable(phi5, philosopher, both, "philosopher").

initialise(P, F1, F2, F3, F4, F5) :->
	send(P, slot, f1, F1),
	send(P, slot, f2, F2),
	send(P, slot, f3, F3),
	send(P, slot, f4, F4),
	send(P, slot, f5, F5).

init_phi(P, Phi1,Phi2, Phi3, Phi4, Phi5) :->
	send(P, slot, phi1, Phi1),
	send(P, slot, phi2, Phi2),
	send(P, slot, phi3, Phi3),
	send(P, slot, phi4, Phi4),
	send(P, slot, phi5, Phi5).


want_forks(P, Phi) :->
	( get(P, slot, phi1, Phi) ,!, check_forks(P, Phi, f5, f1);
	 get(P, slot, phi2, Phi),!, check_forks(P, Phi, f1, f2);
	 get(P, slot, phi3, Phi),!, check_forks(P, Phi, f2, f3);
	 get(P, slot, phi4, Phi),!, check_forks(P, Phi, f3, f4);
	 get(P, slot, phi5, Phi),!, check_forks(P, Phi, f4, f5)).




give_back_forks(P, Phi) :->
	( get(P, slot, phi1, Phi) ,!, release_forks(P, phi1);
	 get(P, slot, phi2, Phi),!, release_forks(P, phi2);
	 get(P, slot, phi3, Phi),!, release_forks(P, phi3);
	 get(P, slot, phi4, Phi),!, release_forks(P, phi4);
	 get(P, slot, phi5, Phi),!, release_forks(P, phi5)),

	get(P, slot, phi1, Phi1),
	check_forks(P, Phi1, f5, f1),
	get(P, slot, phi2, Phi2),
	check_forks(P, Phi2, f1, f2),
	get(P, slot, phi3, Phi3),
	check_forks(P, Phi3, f2, f3),
	get(P, slot, phi4, Phi4),
	check_forks(P, Phi4, f3, f4),
	get(P, slot, phi5, Phi5),
	check_forks(P, Phi5, f4, f5).

release_forks(P, phi1) :-
	get(P, slot, f5, F5),
	send(F5, free),
	get(P, slot, f1, F1),
	send(F1, free).

release_forks(P, phi2) :-
	get(P, slot, f1, F1),
	send(F1, free),
	get(P, slot, f2, F2),
	send(F2, free).

release_forks(P, phi3) :-
	get(P, slot, f2, F2),
	send(F2, free),
	get(P, slot, f3, F3),
	send(F3, free).

release_forks(P, phi4) :-
	get(P, slot, f3, F3),
	send(F3, free),
	get(P, slot, f4, F4),
	send(F4, free).

release_forks(P, phi5) :-
	get(P, slot, f4, F4),
	send(F4, free),
	get(P, slot, f5, F5),
	send(F5, free).

check_forks(P, Phi, F1, F2) :-
	get(P, slot, F1, FF1),
	get(P, slot, F2, FF2),
	(   (get(Phi, slot, status, waiting),
	     get(FF1, slot, status, free),
	     get(FF2, slot, status, free))
	->
	     send(Phi, receive_forks),
	     send(FF1, used, right),
	     send(FF2, used, left)
	;
	     true).

:- pce_end_class.


:- pce_begin_class(philosopher , object, "eat, think or wait !").
variable(name, string, both).
variable(window, object, both).
variable(status, object, both, "eating/thinking/waiting").
variable(waiter, object, both).
variable(plate,  object, both).
variable(mytimer, timer, both).
variable(pos, point, both).
variable(side, object, both).
variable(old_text, object, both).
variable(window_stat, object, both).
variable(line_stat, number, both).
variable(stat_wait, my_stat, both).
variable(stat_eat, my_stat, both).
variable(stat_think, my_stat, both).

% méthode appelée lors de la destruction de l'objet
% On arrête d'abord le timer pour poursuivre ensuite
% sans problème (appel par le timer de ressources libérées)
unlink(P) :->
	send(P?mytimer, stop),

	get(P, status, Sta),
	stop_timer(P, Sta),
	get(P, slot, window_stat, WS),
	get(P, slot, line_stat, LS),
	get(LS, value, VLS),
	get(P, slot, name, Name),
	get(Name, value, V),
	sformat(A, 'Statistics of philosopher : ~w', [V]),
	new(Text, text(A)),
	send(Text, font, font(times, bold, 16)),
	Y is VLS * 30,
	send(WS, display, Text, point(30, Y)),

	VLS1 is VLS+1,
	get(P, slot, stat_think, ST),
	send(ST, statistics, WS, VLS1),

	VLS2 is VLS+2,
	get(P, slot, stat_eat, SE),
	send(SE, statistics, WS, VLS2),

	VLS3 is VLS+3,
	get(P, slot, stat_wait, SW),
	send(SW, statistics, WS, VLS3),

	send(P, send_super, unlink).

initialise(P, Name, Waiter, Plate, Window, Window_stat, Line_stat, Point, Side) :->
	% gtrace,
	send(P, slot, name, Name),
	send(P, slot, window, Window),
	send(P, slot, window_stat, Window_stat),
	Line is Line_stat * 5,
	send(P, slot, line_stat, Line),
	send(P, slot, waiter,Waiter),
	send(P, slot, plate,Plate),
	send(P, slot, status, thinking),
	send(P, slot, pos, Point),
	send(P, slot, side, Side),
	send(Window, display, Plate),
	send(P, slot, old_text, new(_, text(' '))),
	send(P, display_status),
	send(P, slot, stat_wait, new(_, my_stat('Waiting'))),
	send(P, slot, stat_eat, new(_, my_stat('Eating'))),
	send(P, slot, stat_think, new(_, my_stat('Thinking'))).


stop_timer(P, eating) :-
	get(P, slot, stat_eat, SE),
	send(SE, stop).

stop_timer(P, waiting) :-
	get(P, slot, stat_wait, SW),
	send(SW, stop).

stop_timer(P, thinking) :-
	get(P, slot, stat_think, ST),
	send(ST, stop).


% internal message send by the timer
my_message(P) :->
	% gtrace,
	get(P, slot, status, Status),
	next_status(P, Status).

% philosopher eating ==> thinking
next_status(P, eating) :-
	get(P, slot, waiter, Waiter),
	get(P, slot, stat_eat, SE),
	send(SE, stop),
	get(P, slot, stat_think, ST),
	send(ST, start),
	send(Waiter, give_back_forks, P),
	send(P, slot, status, thinking),
	send(P, display_status),
	get(P, plate, Plate),
	send(Plate, fill_pattern, colour(white)),
	I is random(20)+ 10,
	get(P, slot, mytimer, Timer),
	send(Timer, interval, I),
	send(Timer, start, once).

next_status(P, thinking) :-
	get(P, slot, waiter, Waiter),
	send(P, slot, status, waiting),
	send(P, display_status),
	get(P, slot, stat_think, ST),
	send(ST, stop),
	get(P, slot, stat_wait, SW),
	send(SW, start),
	send(Waiter, want_forks, P).

% send by the waiter
% philosopher can eat !
receive_forks(P) :->
	get(P, slot, stat_wait, SW),
	send(SW, stop),
	get(P, slot, stat_eat, SE),
	send(SE, start),
	send(P, slot, status, eating),
	send(P, display_status),
	get(P, plate, Plate),
	send(Plate, fill_pattern, colour(black)),
	I is random(20)+ 5,
	get(P, slot, mytimer, Timer),
	send(Timer, interval, I),
	send(Timer, start, once).

display_status(P) :->
	get(P, old_text, OT),
	free(OT),
	get(P, name, Name),
	get(Name, value, V),
	get(P, status, Status),
	choose_color(Status, Colour),
	sformat(A, '~w ~w', [V, Status]),
	get(P, window, W),
	get(P, pos, point(X, Y)),
	new(Text, text(A)),
	send(Text, font, font(times, bold, 16)),
	send(Text, colour, Colour),
	get(Text, string, Str),
	get(font(times, bold, 16), width(Str), M),
	(get(P, side, right) -> X1 is X - M; X1 = X),
	send(W, display, Text, point(X1, Y)),
	send(P, old_text, Text).


start(P) :->
	I is random(10)+ 2,
	get(P, slot, stat_think, ST),
	send(ST, start),
	send(P, mytimer, new(_, timer(I,message(P, my_message)))),
	send(P?mytimer, start, once).


choose_color(eating, colour(blue)).
choose_color(thinking, colour(green)).
choose_color(waiting, colour(red)).

:- pce_end_class.



:- pce_begin_class(disk, ellipse, "disk with color ").

initialise(P, C, R, Col) :->
        send(P, send_super, initialise, R, R),
	send(P, center, C),
	send(P, pen, 0),
	send(P, fill_pattern, Col).

change_color(P, Col) :->
	send(P, fill_pattern, Col).

:- pce_end_class.

:- pce_begin_class(my_stat , object, "statistics").
variable(name, string, both).
variable(nb, number, both).
variable(duration, real, both).
variable(start, real, both).

initialise(P, Name) :->
	send(P, name, Name),
	send(P, nb, 0),
	send(P, duration, 0.0).

start(P) :->
	get_time(T),
	send(P, slot, start, T).

stop(P) :->
	get_time(Fin),

	get(P, slot, nb, N),
	send(N, plus,1),
	send(P, slot, nb, N),

	get(P, slot, duration, D),
	get(P, slot, start, Deb),

	get(D, value, VD),
	get(Deb, value, VDeb),
	X is VD + Fin - VDeb,

	send(P, slot, duration, X).

statistics(P, W, L) :->
	get(P, nb, N),
	get(N, value, VN),
	get(P, duration, D),
	get(D, value, VD),
	get(P, name, Name),
	get(Name, value, V),
	sformat(A, '~w~tnb :~13| ~t~w~17| duration : ~t~1f~35|', [V, VN, VD]),
	new(Text, text(A)),
	send(Text, font, font(screen, roman, 14)),
	Y is L * 30,
	send(W, display, Text, point(40, Y)).

:-pce_end_class.

% forks changes of place
:- pce_begin_class(fork, line, "to help philosopphers to eat").
variable(value, number, both, "0 => 4").
variable(side, object, both), "left / right".
variable(status, object, both, "free / used").

initialise(P, Val) :->
	send_super(P, initialise),
	send(P, slot, value, Val),
	send(P, slot, status, free),
	compute(Val, free, _, PS, PE),
	send(P, start, PS),
	send(P, end, PE).

free(P) :->
	send(P, status, free),
	send(P, position).


used(P, Side) :->
	send(P, status, used),
	send(P, side, Side),
	send(P, position).

position(P) :->
	get(P, value, V),
	get(V, value, N),
	get(P, status, St),
	get(P, side, Side),
	compute(N, St, Side, PS, PE),
	send(P, start, PS),
	send(P, end, PE).


compute(N, free, _Side, point(XS,YS), point(XE,YE)) :-
	A is N * pi / 2.5 + pi / 5,
	XS is 400 + 100 * cos(A),
	YS is 400 + 100 * sin(A),
	XE is 400 + 180 * cos(A),
	YE is 400 + 180 * sin(A).


compute(N, used, left, point(XS,YS), point(XE,YE)) :-
	A is N * pi / 2.5 + pi / 5 - 2 * pi / 15,
	XS is 400 + 100 * cos(A),
	YS is 400 + 100 * sin(A),
	XE is 400 + 180 * cos(A),
	YE is 400 + 180 * sin(A).

compute(N, used, right, point(XS,YS), point(XE,YE)) :-
	A is N * pi / 2.5 + pi / 5 +  2 * pi / 15,
	XS is 400 + 100 * cos(A),
	YS is 400 + 100 * sin(A),
	XE is 400 + 180 * cos(A),
	YE is 400 + 180 * sin(A).

:- pce_end_class.

```

[[File:Prolog-Philosophers-1.png|500px|thumb|center]]

 [[File:Prolog-Philosophers-3.png|450px|thumb|center]]


## PureBasic


My Philosophers are very polite, if one can not get both forks they then
put down the first and waits a few breaths, then boldly tries in the opposite order.


```PureBasic
Macro Tell(Mutex, Message) ; Make a macro to easy send info back to main thread
  LockMutex(Mutex)
    LastElement(Queue())
    AddElement(Queue())
    Queue() = Message
    SignalSemaphore(Semaphore)
  UnlockMutex(Mutex)
EndMacro

;Set up a data structure to pass needed info into the threads
Structure Thread_Parameters
  Name.s
  fork1.i
  fork2.i
EndStructure

; Declare function to be used
Declare.i TryFork(n)
Declare   PutDownFork(n)
Declare   Invite(Namn.s, Fork1, Fork2)
Declare   _philosophers(*arg.Thread_Parameters)

Global Semaphore = CreateSemaphore()
Global Mutex1     = CreateMutex() ; Eg. fork 1
Global Mutex2     = CreateMutex() ; Eg. fork 2
Global Mutex3     = CreateMutex() ; Eg. fork 3
Global Mutex4     = CreateMutex() ; Eg. fork 4
Global Mutex5     = CreateMutex() ; Eg. fork 5
Global Mutex_main = CreateMutex() ; locking communication with the main thread which do all output.
Global NewList Queue.s()

If OpenConsole()
  Invite("Aristotle",1,2)  ; Get all Philosophers activated
  Invite("Kant",     2,3)
  Invite("Spinoza",  3,4)
  Invite("Marx",     4,5)
  Invite("Russell",  5,1)
  CompilerIf #PB_Compiler_OS=#PB_OS_Windows
    SetConsoleTitle_("Dining philosophers, by Jofur")   ; Using a Windows-API here, so checking before
  CompilerEndIf
  ; Wait and see if any Philosophers want to tell me anything
  Repeat
    WaitSemaphore(Semaphore)
    LockMutex(Mutex_main)
      ForEach Queue()
        PrintN( Queue() )  ; Print what the Philosopher(s) told me
        i-1
      Next Queue()
      ClearList(Queue())
    UnlockMutex(Mutex_main)
  ForEver
EndIf

Procedure TryFork(n)  ; Se is fork #n is free and if so pick it up
  Select n
    Case 1: ProcedureReturn TryLockMutex(Mutex1)
    Case 2: ProcedureReturn TryLockMutex(Mutex2)
    Case 3: ProcedureReturn TryLockMutex(Mutex3)
    Case 4: ProcedureReturn TryLockMutex(Mutex4)
    Default:ProcedureReturn TryLockMutex(Mutex5)
  EndSelect
EndProcedure

Procedure PutDownFork(n) ; put down fork #n and free it to be used by neighbors.
  Select n
    Case 1: UnlockMutex(Mutex1)
    Case 2: UnlockMutex(Mutex2)
    Case 3: UnlockMutex(Mutex3)
    Case 4: UnlockMutex(Mutex4)
    Default:UnlockMutex(Mutex5)
  EndSelect
EndProcedure

Procedure Invite(Namn.s, Fork1, Fork2)
  Protected *arg.Thread_Parameters ;create the structure containing the parameters
  Protected Thread
  *arg = AllocateMemory(SizeOf(Thread_Parameters))
  *arg\Name = Namn
  *arg\fork1 = Fork1
  *arg\fork2 = Fork2
  Thread=CreateThread(@_philosophers(), *arg) ;send the thread a pointer to our structure
  ProcedureReturn Thread
EndProcedure

Procedure _philosophers(*arg.Thread_Parameters)
  Protected Iam.s=*arg\Name, j=*arg\fork1, k=*arg\fork2
  Protected f1, f2
  ClearStructure(*arg, Thread_Parameters)
  FreeMemory(*arg)
  ;
  Repeat
    Tell(Mutex_main,Iam+": Going to the table")
    Repeat          ;Trying to get my two forks
      f1=TryFork(j)
      If f1
        f2=TryFork(k)
        If Not f2   ; I got only one fork
          PutDownFork(j)
          f1=0
        EndIf
      EndIf
      If Not f2
        Delay(Random(100))  ; Take a short breath, then try the forks in the other order
        Swap j,k
      EndIf
    Until f1 And f2
    Tell(Mutex_main,Iam+": I have fork #"+Str(j)+" & #"+Str(k)+" and I'm eating now")
    Delay(Random(1500)+15)
    Tell(Mutex_main,Iam+": release fork #"+Str(j)+" & #"+Str(k)+"")
    Delay(Random(45)+15)
    PutDownFork(j)
    PutDownFork(k)
    f1=0:f2=0
    Tell(Mutex_main,Iam+": Thinking about the nature of the universe...")
    Delay(Random(2500)+25)
  ForEver
EndProcedure
```



## Python

This solution avoids deadlock by never waiting for a fork while having one in hand.  If
a philosopher acquires one fork but can't acquire the second, he releases the first fork
before waiting to acquire the other (which then becomes the first fork acquired).

```python
import threading
import random
import time

# Dining philosophers, 5 Phillies with 5 forks. Must have two forks to eat.
#
# Deadlock is avoided by never waiting for a fork while holding a fork (locked)
# Procedure is to do block while waiting to get first fork, and a nonblocking
# acquire of second fork.  If failed to get second fork, release first fork,
# swap which fork is first and which is second and retry until getting both.
#
# See discussion page note about 'live lock'.

class Philosopher(threading.Thread):

    running = True

    def __init__(self, xname, forkOnLeft, forkOnRight):
        threading.Thread.__init__(self)
        self.name = xname
        self.forkOnLeft = forkOnLeft
        self.forkOnRight = forkOnRight

    def run(self):
        while(self.running):
            #  Philosopher is thinking (but really is sleeping).
            time.sleep( random.uniform(3,13))
            print '%s is hungry.' % self.name
            self.dine()

    def dine(self):
        fork1, fork2 = self.forkOnLeft, self.forkOnRight

        while self.running:
            fork1.acquire(True)
            locked = fork2.acquire(False)
            if locked: break
            fork1.release()
            print '%s swaps forks' % self.name
            fork1, fork2 = fork2, fork1
        else:
            return

        self.dining()
        fork2.release()
        fork1.release()

    def dining(self):
        print '%s starts eating '% self.name
        time.sleep(random.uniform(1,10))
        print '%s finishes eating and leaves to think.' % self.name

def DiningPhilosophers():
    forks = [threading.Lock() for n in range(5)]
    philosopherNames = ('Aristotle','Kant','Buddha','Marx', 'Russel')

    philosophers= [Philosopher(philosopherNames[i], forks[i%5], forks[(i+1)%5]) \
            for i in range(5)]

    random.seed(507129)
    Philosopher.running = True
    for p in philosophers: p.start()
    time.sleep(100)
    Philosopher.running = False
    print ("Now we're finishing.")

DiningPhilosophers()
```



## Racket



```racket

#lang racket

;; Racket has traditional semaphores in addition to several higher level
;; synchronization tools.  (Note that these semaphores are used for Racket's
;; green-threads, there are also "future semaphores" which are used for OS
;; threads, with a similar interface.)

;; ----------------------------------------------------------------------------
;; First, a bunch of code to run the experiments below

;; Only two philosophers to make it deadlock very fast
(define philosophers '(Aristotle Kant #|Spinoza Marx Russell|#))

(define (run-philosopher name fork1 fork2)
  (define (show what) (displayln (~a name " " what)))
  (define (loop)
    (show "thinks") (sleep (* 2 (random))) (show "is hungry")
    (grab-forks fork1 fork2 (λ() (show "eats") (sleep (random))))
    (loop))
  (thread loop))

(define (run:simple)
  (define forks (for/list ([i philosophers]) (make-semaphore 1)))
  (for ([i philosophers] [fork1 forks] [fork2 (cons (last forks) forks)])
    (run-philosopher i fork1 fork2))
  (sleep (* 60 60 24 365)))

;; ----------------------------------------------------------------------------
;; This is the naive implementation, which can be used to try getting a
;; deadlock.

(define (grab:naive fork1 fork2 eat!)
  (semaphore-wait fork1)
  (sleep (random)) ; to make deadlocks probable
  (semaphore-wait fork2)
  (eat!)
  (semaphore-post fork1)
  (semaphore-post fork2))

;; ----------------------------------------------------------------------------
;; One way to solve it is to release the first fork if the second is busy and
;; wait for a while.

(define (grab:release+wait fork1 fork2 eat!)
  (semaphore-wait fork1)
  (if (not (semaphore-try-wait? fork2))
    ;; couldn't grab the second fork, so release the first and wait
    (begin (semaphore-post fork1)
           (sleep (random))
           (grab-forks fork1 fork2)) ; can swap them to improve chances
    ;; we have both forks
    (begin (eat!)
           (semaphore-post fork1)
           (semaphore-post fork2))))

;; ----------------------------------------------------------------------------
;; Another solution is to label the forks and lock the lowest-id one first,
;; which makes the naive solution work.

(define (run:labeled-forks)
  (define forks (for/list ([i philosophers]) (make-semaphore 1)))
  ;; the simple run used forks as (1 2 3 4) (4 1 2 3) -- so to implement this,
  ;; we can swap the two first ones: (4 2 3 4) (1 1 2 3)
  (for ([i philosophers]
        [fork1 (cons (last forks) (cdr forks))]
        [fork2 (cons (first forks) forks)])
    (run-philosopher i fork1 fork2))
  (sleep (* 60 60 24 365)))

;; ----------------------------------------------------------------------------
;; Homework: implement the centralized waiter solution

;; ...

;; ----------------------------------------------------------------------------
;; Uncomment one of the following pairs to try it

;; (define grab-forks grab:naive)
;; (define run run:simple)

;; (define grab-forks grab:release+wait)
;; (define run run:simple)

;; (define grab-forks grab:naive)
;; (define run run:labeled-forks)

(run)

```



## REXX

Programming notes:   This REXX version allows a specification of the names and numbers of dining philosophers   (but no check was made for the number of philosophers less than two).   The philosopher's names may have imbedded blanks in them, blanks are signified by an underscore   (<big>'''_'''</big>).   A random number   ''seed''   can be specified to allow for repeatability.   The duration of any of the activities the philosophers partake in are herein designated in   ''minutes'',   but any consistent timer unit may be used.   Intermediate steps (such as putting the forks down after finishing eating, leaving the dining room to contemplating the nature of the universe, then getting hungry and entering the dining room) are not shown.   If the   ''seed''   (first argument) has a leading plus sign   ('''+'''),   then no status trace is shown.   A random selection of diners (for determining who gets to grab for the forks first) could've been implemented to make a more realistic simulation.

Deadlocks are eliminated by the method of acquiring the resources (forks):   both forks are (attempted to be) grabbed at the same time (by both hands).

```rexx
/*REXX program demonstrates a solution in solving the  dining philosophers problem.     */
signal on halt                                   /*branches to  HALT:   (on Ctrl─break).*/
parse arg seed diners                            /*obtain optional arguments from the CL*/
if datatype(seed, 'W')  then call random ,, seed /*this allows for random repeatability.*/
if diners= ''           then diners = 'Aristotle, Kant, Spinoza, Marx, Russell'
  tell=(left(seed, 1) \== '+')                   /*Leading + in SEED? Then no statistics*/
diners=space( translate(diners, , ',') )         /*change to an uncommatized diners list*/
     #=words(diners);      @.=  0                /*#: the number of dining philosophers.*/
  eatL=15;               eatH= 60                /*minimum & maximum minutes for eating.*/
thinkL=30;             thinkH=180                /*   "    "    "       "     " thinking*/
forks.=1                                         /*indicate that all forks are on table.*/
          do tic=1         /*'til halted.*/      /*use  "minutes"  for time advancement.*/
          call grabForks                         /*determine if anybody can grab 2 forks*/
          call passTime                          /*handle philosophers eating|thinking. */
          end   /*tic*/                          /*     ··· and time marches on ···     */
                                                 /* [↓]    this REXX program was halted,*/
halt: say '  ··· REXX program halted!'           /*probably by Ctrl─Break or equivalent.*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
fork: parse arg x 1 ox;  x=abs(x);   L=x - 1;    if L==0  then L=#  /*use "round Robin".*/
      if ox<0  then do;  forks.L=1;  forks.x=1;  return;  end       /*drop the forks.   */
      got2= forks.L & forks.x                                       /*get 2 forks or not*/
      if got2  then do;  forks.L=0;  forks.x=0;           end       /*obtained 2 forks. */
      return got2                                /*return with success  ··· or failure. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
grabForks:   do person=1  for  #                 /*see if any person can grab two forks.*/
             if @.person.state\==0  then iterate /*this diner ain't in a waiting state. */
             if \fork(person)       then iterate /*this diner didn't grab two forks.    */
             @.person.state= 'eating'            /*this diner is slurping spaghetti.    */
             @.person.dur=random(eatL, eatH)     /*how long will this diner eat pasta ? */
             end   /*person*/                    /* [↑]  process the dining philosophers*/
          return                                 /*all the diners have been examined.   */
/*──────────────────────────────────────────────────────────────────────────────────────*/
passTime: if tell  then say                      /*display a handy blank line separator.*/
            do p=1  for #                        /*handle each of the diner's activity. */
            if tell  then say  right(tic, 9, .)           right( word( diners, p), 20),
                      right(word(@.p.state 'waiting',1+(@.p.state==0)),9) right(@.p.dur,5)
            if @.p.dur==0   then iterate         /*this diner is waiting for two forks. */
            @.p.dur= @.p.dur - 1                 /*indicate single time unit has passed.*/
            if @.p.dur\==0  then iterate         /*Activity done?   No, then keep it up.*/
            if @.p.state=='eating'  then do                      /*now, leave the table.*/
                                         call fork  -p           /*drop the darn forks. */
                                         @.p.state= 'thinking'                 /*status.*/
                                         @.p.dur=random(thinkL, thinkH)        /*length.*/
                                         end     /* [↓]  a diner goes   ──►  the table. */
                                    else if  @.p.state=='thinking'  then @.p.state=0
            end   /*p*/                          /*[↑]  P (person)≡ dining philosophers.*/
          return
```

{{out|output|text=    (some middle and end portions have been elided):

```txt

........1            Aristotle    eating    42
........1                 Kant   waiting     0
........1              Spinoza    eating    26
........1                 Marx   waiting     0
........1              Russell   waiting     0

........2            Aristotle    eating    41
........2                 Kant   waiting     0
........2              Spinoza    eating    25
........2                 Marx   waiting     0
........2              Russell   waiting     0

         ~~~elided~~~

.....2074            Aristotle  thinking    20
.....2074                 Kant  thinking    56
.....2074              Spinoza  thinking   157
.....2074                 Marx    eating    29
.....2074              Russell  thinking    85

         ~~~elided~~~

```



## Ruby

```ruby
require 'mutex_m'

class Philosopher
  def initialize(name, left_fork, right_fork)
    @name = name
    @left_fork = left_fork
    @right_fork = right_fork
    @meals = 0
  end

  def go
    while @meals < 5
      think
      dine
    end
    puts "philosopher #@name is full!"
  end

  def think
    puts "philosopher #@name is thinking..."
    sleep(rand())
    puts "philosopher #@name is hungry..."
  end

  def dine
    fork1, fork2 = @left_fork, @right_fork
    while true
      pickup(fork1, :wait => true)
      puts "philosopher #@name has fork #{fork1.fork_id}..."
      if pickup(fork2, :wait => false)
        break
      end
      puts "philosopher #@name cannot pickup second fork #{fork2.fork_id}..."
      release(fork1)
      fork1, fork2 = fork2, fork1
    end
    puts "philosopher #@name has the second fork #{fork2.fork_id}..."

    puts "philosopher #@name eats..."
    sleep(rand())
    puts "philosopher #@name belches"
    @meals += 1

    release(@left_fork)
    release(@right_fork)
  end

  def pickup(fork, opt)
    puts "philosopher #@name attempts to pickup fork #{fork.fork_id}..."
    opt[:wait] ? fork.mutex.mu_lock : fork.mutex.mu_try_lock
  end

  def release(fork)
    puts "philosopher #@name releases fork #{fork.fork_id}..."
    fork.mutex.unlock
  end
end

n = 5

Fork = Struct.new(:fork_id, :mutex)
forks = Array.new(n) {|i| Fork.new(i, Object.new.extend(Mutex_m))}

philosophers = Array.new(n) do |i|
                 Thread.new(i, forks[i], forks[(i+1)%n]) do |id, f1, f2|
                   ph = Philosopher.new(id, f1, f2).go
                 end
               end

philosophers.each {|thread| thread.join}
```



## Rust

A Rust implementation of a solution for the Dining Philosophers Problem. We prevent a deadlock by using Dijkstra's solution of making a single diner "left-handed." That is, all diners except one pick up the chopstick "to their left" and then the chopstick "to their right." The remaining diner performs this in reverse.


```rust
use std::thread;
use std::sync::{Mutex, Arc};

struct Philosopher {
    name: String,
    left: usize,
    right: usize,
}

impl Philosopher {
    fn new(name: &str, left: usize, right: usize) -> Philosopher {
        Philosopher {
            name: name.to_string(),
            left: left,
            right: right,
        }
    }

    fn eat(&self, table: &Table) {
        let _left = table.forks[self.left].lock().unwrap();
        let _right = table.forks[self.right].lock().unwrap();

        println!("{} is eating.", self.name);

        thread::sleep_ms(1000);

        println!("{} is done eating.", self.name);
    }
}

struct Table {
    forks: Vec<Mutex<()>>,
}

fn main() {
    let table = Arc::new(Table { forks: vec![
        Mutex::new(()),
        Mutex::new(()),
        Mutex::new(()),
        Mutex::new(()),
        Mutex::new(()),
    ]});

    let philosophers = vec![
        Philosopher::new("Baruch Spinoza", 0, 1),
        Philosopher::new("Gilles Deleuze", 1, 2),
        Philosopher::new("Karl Marx", 2, 3),
        Philosopher::new("Friedrich Nietzsche", 3, 4),
        Philosopher::new("Michel Foucault", 0, 4),
    ];

    let handles: Vec<_> = philosophers.into_iter().map(|p| {
        let table = table.clone();

        thread::spawn(move || {
            p.eat(&table);
        })
    }).collect();

    for h in handles {
        h.join().unwrap();
    }
}
```


## Simula


```simula
COMMENT
!    DEADLOCK IS PREVENTED BY REVERSING THE ORDER OF TAKING THE CHOPSTICKS FOR THE LAST PHILOSOPHER.
!    THAT MEANS ALL PHILOSOPHERS FIRST TAKE THE LEFT CHOPSTICK, THEN THE RIGHT CHOPSTICK.
!    BUT THE LAST PHILOSOPHER FIRST TAKES THE RIGHT CHOPSTICK, THEN THE LEFT.
!
!    THE DETACH STATEMENT IN CLASS PHILOSOPHER GIVES CONTROL BACK TO THE MAIN BLOCK.
!    THE MAIN BLOCK CALLS/RESUMES ALL THE PHILOSOPHERS USING THE RESUME(PHILOSOPHER) STATEMENT.
!    THIS CONTINUES THE CODE IN THE PHILOSOPHER CLASS AFTER THE LAST DETACH STATEMENT.
!    (ANOTHER NAME FOR THIS FEATURE IS THE CONCEPT OF A COROUTINE)
;
BEGIN
    INTEGER N;
    INTEGER PNR, CNR;
    INTEGER SEED;
    SEED := ININT;
    N := 5;
    BEGIN

        CLASS CHOPSTICK;
        BEGIN
            REF(PHILOSOPHER) OWNER;
            INTEGER ID;
            ID := CNR := CNR + 1;
        END CHOPSTICK;

        CLASS PHILOSOPHER(L,R);
            REF(CHOPSTICK) L,R;
        BEGIN
            INTEGER ID;
            ID := PNR := PNR + 1;
            WHILE TRUE DO
            BEGIN
                DETACH;

                OUTTEXT("PHILOSOPHER(");
                OUTINT(ID, 0);
                OUTTEXT(") THINKING...");
                OUTIMAGE;
                DETACH;

                WHILE RANDINT(0,1,SEED) = 0 DO BEGIN
                    OUTTEXT("PHILOSOPHER(");
                    OUTINT(ID, 0);
                    OUTTEXT(") THINKING DEEPER...");
                    OUTIMAGE;
                    DETACH;
                END;

                WHILE L.OWNER =/= NONE DO BEGIN
                    OUTTEXT("PHILOSOPHER(");
                    OUTINT(ID, 0);
                    OUTTEXT(") WAITING FOR LEFT CHOPSTICK(");
                    OUTINT(L.ID, 0);
                    OUTTEXT(") ...");
                    OUTIMAGE;
                    DETACH;
                END;
                L.OWNER :- THIS PHILOSOPHER;
                OUTTEXT("PHILOSOPHER(");
                OUTINT(ID, 0);
                OUTTEXT(") GRABBED LEFT CHOPSTICK(");
                OUTINT(L.ID, 0);
                OUTTEXT(")");
                OUTIMAGE;

                WHILE R.OWNER =/= NONE DO BEGIN
                    OUTTEXT("PHILOSOPHER(");
                    OUTINT(ID, 0);
                    OUTTEXT(") WAITING FOR RIGHT CHOPSTICK(");
                    OUTINT(R.ID, 0);
                    OUTTEXT(") ...");
                    OUTIMAGE;
                    DETACH;
                END;
                R.OWNER :- THIS PHILOSOPHER;
                OUTTEXT("PHILOSOPHER(");
                OUTINT(ID, 0);
                OUTTEXT(") GRABBED RIGHT CHOPSTICK(");
                OUTINT(R.ID, 0);
                OUTTEXT(")");
                OUTIMAGE;

                OUTTEXT("PHILOSOPHER(");
                OUTINT(ID, 0);
                OUTTEXT(") EATING...");
                OUTIMAGE;
                WHILE RANDINT(0,1,SEED) = 0 DO BEGIN
                    DETACH;
                    OUTTEXT("PHILOSOPHER(");
                    OUTINT(ID, 0);
                    OUTTEXT(") STILL EATING...");
                    OUTIMAGE;
                END;
                L.OWNER :- NONE;
                R.OWNER :- NONE;
                OUTTEXT("PHILOSOPHER(");
                OUTINT(ID, 0);
                OUTTEXT(") RELEASED LEFT CHOPSTICK(");
                OUTINT(L.ID, 0);
                OUTTEXT(")");
                OUTIMAGE;
                OUTTEXT("PHILOSOPHER(");
                OUTINT(ID, 0);
                OUTTEXT(") RELEASED RIGHT CHOPSTICK(");
                OUTINT(R.ID, 0);
                OUTTEXT(")");
                OUTIMAGE;
            END;
        END PHILOSOPHER;

        !---------------------------------------|
        !                                       |
        !                                       |
        !                  (3)                  |
        !             P2         P3             |
        !                                       |
        !        (2)                  (4)       |
        !                                       |
        !                                       |
        !      P1                       P4      |
        !                                       |
        !                                       |
        !         (1)              (5)          |
        !                                       |
        !                  P5                   |  only P5 takes Right first (5), then Left (1)
        !                                       |
        !---------------------------------------|
        !;

        REF(PHILOSOPHER) ARRAY PHILS (1:N);
        REF(CHOPSTICK) L, R;
        INTEGER I, LOOPS;

        R :- NEW CHOPSTICK;
        FOR I := 1 STEP 1 UNTIL N-1 DO
        BEGIN
            L :- NEW CHOPSTICK;
            PHILS(I) :- NEW PHILOSOPHER(L,R);
            R :- L;
        END;
        ! REVERSED ORDER FOR THE LAST PHILOSOPHER ;
        PHILS(N) :- NEW PHILOSOPHER(R,PHILS(1).R);

        FOR I := 1 STEP 1 UNTIL N DO BEGIN
            OUTTEXT("PHILOSOPHER(ID=");
            OUTINT(PHILS(I).ID, 0);
            OUTTEXT(", L=");
            OUTINT(PHILS(I).L.ID, 0);
            OUTTEXT(", R=");
            OUTINT(PHILS(I).R.ID, 0);
            OUTTEXT(")");
            OUTIMAGE;
        END;

        FOR LOOPS := 1 STEP 1 UNTIL 10 DO BEGIN
            FOR I := 1 STEP 1 UNTIL N DO BEGIN
                RESUME(PHILS(I));
            END;
        END;

    END;
END.
```

```txt

12121

```

```txt
PHILOSOPHER(ID=1, L=2, R=1)
PHILOSOPHER(ID=2, L=3, R=2)
PHILOSOPHER(ID=3, L=4, R=3)
PHILOSOPHER(ID=4, L=5, R=4)
PHILOSOPHER(ID=5, L=5, R=1)
PHILOSOPHER(1) THINKING...
PHILOSOPHER(2) THINKING...
PHILOSOPHER(3) THINKING...
PHILOSOPHER(4) THINKING...
PHILOSOPHER(5) THINKING...
PHILOSOPHER(1) THINKING DEEPER...
PHILOSOPHER(2) GRABBED LEFT CHOPSTICK(3)
PHILOSOPHER(2) GRABBED RIGHT CHOPSTICK(2)
PHILOSOPHER(2) EATING...
PHILOSOPHER(3) THINKING DEEPER...
PHILOSOPHER(4) GRABBED LEFT CHOPSTICK(5)
PHILOSOPHER(4) GRABBED RIGHT CHOPSTICK(4)
PHILOSOPHER(4) EATING...
PHILOSOPHER(5) WAITING FOR LEFT CHOPSTICK(5) ...
PHILOSOPHER(1) WAITING FOR LEFT CHOPSTICK(2) ...
PHILOSOPHER(2) STILL EATING...
PHILOSOPHER(3) WAITING FOR LEFT CHOPSTICK(4) ...
PHILOSOPHER(4) STILL EATING...
PHILOSOPHER(5) WAITING FOR LEFT CHOPSTICK(5) ...
PHILOSOPHER(1) WAITING FOR LEFT CHOPSTICK(2) ...
PHILOSOPHER(2) STILL EATING...
PHILOSOPHER(2) RELEASED LEFT CHOPSTICK(3)
PHILOSOPHER(2) RELEASED RIGHT CHOPSTICK(2)
PHILOSOPHER(3) WAITING FOR LEFT CHOPSTICK(4) ...
PHILOSOPHER(4) STILL EATING...
PHILOSOPHER(5) WAITING FOR LEFT CHOPSTICK(5) ...
PHILOSOPHER(1) GRABBED LEFT CHOPSTICK(2)
PHILOSOPHER(1) GRABBED RIGHT CHOPSTICK(1)
PHILOSOPHER(1) EATING...
PHILOSOPHER(1) RELEASED LEFT CHOPSTICK(2)
PHILOSOPHER(1) RELEASED RIGHT CHOPSTICK(1)
PHILOSOPHER(2) THINKING...
PHILOSOPHER(3) WAITING FOR LEFT CHOPSTICK(4) ...
PHILOSOPHER(4) STILL EATING...
PHILOSOPHER(5) WAITING FOR LEFT CHOPSTICK(5) ...
PHILOSOPHER(1) THINKING...
PHILOSOPHER(2) GRABBED LEFT CHOPSTICK(3)
PHILOSOPHER(2) GRABBED RIGHT CHOPSTICK(2)
PHILOSOPHER(2) EATING...
PHILOSOPHER(2) RELEASED LEFT CHOPSTICK(3)
PHILOSOPHER(2) RELEASED RIGHT CHOPSTICK(2)
PHILOSOPHER(3) WAITING FOR LEFT CHOPSTICK(4) ...
PHILOSOPHER(4) STILL EATING...
PHILOSOPHER(4) RELEASED LEFT CHOPSTICK(5)
PHILOSOPHER(4) RELEASED RIGHT CHOPSTICK(4)
PHILOSOPHER(5) GRABBED LEFT CHOPSTICK(5)
PHILOSOPHER(5) GRABBED RIGHT CHOPSTICK(1)
PHILOSOPHER(5) EATING...
PHILOSOPHER(5) RELEASED LEFT CHOPSTICK(5)
PHILOSOPHER(5) RELEASED RIGHT CHOPSTICK(1)
PHILOSOPHER(1) GRABBED LEFT CHOPSTICK(2)
PHILOSOPHER(1) GRABBED RIGHT CHOPSTICK(1)
PHILOSOPHER(1) EATING...
PHILOSOPHER(2) THINKING...
PHILOSOPHER(3) GRABBED LEFT CHOPSTICK(4)
PHILOSOPHER(3) GRABBED RIGHT CHOPSTICK(3)
PHILOSOPHER(3) EATING...
PHILOSOPHER(4) THINKING...
PHILOSOPHER(5) THINKING...
PHILOSOPHER(1) STILL EATING...
PHILOSOPHER(1) RELEASED LEFT CHOPSTICK(2)
PHILOSOPHER(1) RELEASED RIGHT CHOPSTICK(1)
PHILOSOPHER(2) THINKING DEEPER...
PHILOSOPHER(3) STILL EATING...
PHILOSOPHER(4) THINKING DEEPER...
PHILOSOPHER(5) THINKING DEEPER...
PHILOSOPHER(1) THINKING...
PHILOSOPHER(2) WAITING FOR LEFT CHOPSTICK(3) ...
PHILOSOPHER(3) STILL EATING...
PHILOSOPHER(3) RELEASED LEFT CHOPSTICK(4)
PHILOSOPHER(3) RELEASED RIGHT CHOPSTICK(3)
PHILOSOPHER(4) GRABBED LEFT CHOPSTICK(5)
PHILOSOPHER(4) GRABBED RIGHT CHOPSTICK(4)
PHILOSOPHER(4) EATING...
PHILOSOPHER(5) THINKING DEEPER...
PHILOSOPHER(1) THINKING DEEPER...
PHILOSOPHER(2) GRABBED LEFT CHOPSTICK(3)
PHILOSOPHER(2) GRABBED RIGHT CHOPSTICK(2)
PHILOSOPHER(2) EATING...
PHILOSOPHER(2) RELEASED LEFT CHOPSTICK(3)
PHILOSOPHER(2) RELEASED RIGHT CHOPSTICK(2)
PHILOSOPHER(3) THINKING...
PHILOSOPHER(4) STILL EATING...
PHILOSOPHER(4) RELEASED LEFT CHOPSTICK(5)
PHILOSOPHER(4) RELEASED RIGHT CHOPSTICK(4)
PHILOSOPHER(5) THINKING DEEPER...

```



## Smalltalk

This solution is similar to the Python solution, except this uses Semaphore instances for forks and an OrderedCollection to hold sequence of forks and philosophers around the table.  The method #pickUpForks releases and retries after a small delay till it has both.  <b>Special note:</b> Smalltalk concurrent processes are created by sending the message #fork to a block.  Do not confuse this method name with the forks of the problem domain.


```Smalltalk
'From Squeak3.7 of ''4 September 2004'' [latest update: #5989] on 13 October 2011 at 2:44:42 pm'!
Object subclass: #Philosopher
	instanceVariableNames: 'table random name seat forks running'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'rosettacode'!

!Philosopher methodsFor: 'private'!
createfork
	^ Semaphore forMutualExclusion! !

!Philosopher methodsFor: 'private'!
displayState: aStateName
	Transcript show: name , ' is ' , aStateName;
		 cr! !

!Philosopher methodsFor: 'private'!
pickUpForkAt: relativePosition
	| fork pos |
	pos := self tableIndex: seat + relativePosition.
	(fork := table at: pos)
		ifNotNil: [fork
				critical: [(table at: pos) notNil
						ifTrue: [table at: pos put: nil]
						ifFalse: [fork := nil]]].
	^ (forks at: relativePosition put: fork) notNil! !

!Philosopher methodsFor: 'private'!
putBackForkAt: aRelativePosition
	| fork |
	fork := forks at: aRelativePosition.
	fork
		ifNotNil: [table
				at: (self tableIndex: seat + aRelativePosition)
				put: fork.
			forks at: aRelativePosition put: nil]! !

!Philosopher methodsFor: 'private'!
tableIndex: aNum
	^ aNum - 1 \\ table size + 1! !

!Philosopher methodsFor: 'private'!
waitRandomTime
	(Delay forMilliseconds: (random next * 4000) rounded) wait! !


!Philosopher methodsFor: 'dining'!
eat
	self displayState: 'eating';
		 waitRandomTime;
		 putBackForkAt: -1;
		 putBackForkAt: 1! !

!Philosopher methodsFor: 'dining'!
pickUpForks
	self displayState: 'trying to pick up forks'.
	[(self pickUpForkAt: -1)
		ifTrue: [(self pickUpForkAt: 1)
				ifFalse: [self putBackForkAt: -1]].
	(forks at: 1) notNil]
		whileFalse: [(Delay forMilliseconds: 10) wait]! !

!Philosopher methodsFor: 'dining'!
think
	self displayState: 'thinking';
		 waitRandomTime! !


!Philosopher methodsFor: 'initialize-release'!
beginDining: aName at: aTable
	name := aName.
	table := aTable.
	forks := Dictionary new at: -1 put: nil;
				 at: 1 put: nil;
				 yourself.
	random := Random new seed: name hash.
	seat := table size + 1.
	table add: self;
		 add: self createfork.
	running := true.
	[(Delay forSeconds: 20) wait.
	running := false] fork.
	[[running]
		whileTrue: [self think; pickUpForks; eat].
	nil] fork! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

Philosopher class
	instanceVariableNames: ''!

!Philosopher class methodsFor: 'examples'!
diningPhilosophersTest
	| diningTable |
	diningTable := OrderedCollection new.
	#('Aristotle' 'Kant' 'Buddha' 'Marx' 'Russel' )
		do: [:aName | Philosopher new beginDining: aName at: diningTable]! !

```



## Tcl

```tcl
package require Thread

foreach name {Aristotle Kant Spinoza Marx Russel} {
    lappend forks [thread::mutex create]
    lappend tasks [set t [thread::create -preserved {
        # Implement each task as a coroutine internally for simplicity of presentation
        # This is because we want to remain able to receive messages so we can shut
        # down neatly at the end of the program.
	interp alias {} doTask {} coroutine t philosopher
	proc delay {expression} {
	    yield [after [expr $expression] [info coroutine]]
	}

        # Forks are mutexes...
        proc pickUpFork fork {
            thread::mutex lock $fork
        }
        proc putDownFork fork {
            thread::mutex unlock $fork
        }

        # The actual implementation of the task
	proc philosopher {f1 f2} {
	    global name
	    # Always acquire forks in order; prevents deadlock
            # Uses the "natural" order of the lexicographical order of the fork names
	    if {$f1 > $f2} {
                lassign [list $f1 $f2] f2 f1
            }

            # The classic "philosophers" loop
	    while {true} {
		puts "$name is thinking"
		delay {int(200*rand())}

		puts "$name is hungry, getting fork in left hand"
		pickUpFork $f1
		delay {int(2000*rand())} ;# Make deadlock likely if it is possible!

		puts "$name is hungry, getting fork in right hand"
		pickUpFork $f2

		puts "$name is eating"
		delay {int(2000*rand())}

		puts "$name has finished eating; putting down forks"
		putDownFork $f2
		putDownFork $f1
                delay 100
	    }
	}
	thread::wait
    }]]
    thread::send $t [list set name $name]
}

# Set the tasks going
foreach t $tasks {f1 f2} {0 1 1 2 2 3 3 4 4 0} {
    thread::send -async $t [list \
            doTask [lindex $forks $f1] [lindex $forks $f2]]
}

# Kill everything off after 30 seconds; that's enough for demonstration!
after 30000
puts "Completing..."
foreach t $tasks {
    thread::send -async $t thread::exit
}
```



## VBA

Unfortunately, VBA is single threaded. The task is implemented by introducing a separate program counter for each individual philosopher. While the routine dine loops through all philosophers the routine philosopher execute the actions for each
philosopher separately, as if in separate threads. Program terminates after max count is reached. Statistics show how many
program ticks are spent at each step at the dining table.

```vb
'The combination of holding to the second fork
'(HOLDON=True) and all philosophers start
'with same hand (DIJKSTRASOLUTION=False) leads
'to a deadlock. To prevent deadlock
'set HOLDON=False, and DIJKSTRASOLUTION=True.
Public Const HOLDON = False
Public Const DIJKSTRASOLUTION = True
Public Const X = 10 'chance to continue eating/thinking
Public Const GETS = 0
Public Const PUTS = 1
Public Const EATS = 2
Public Const THKS = 5
Public Const FRSTFORK = 0
Public Const SCNDFORK = 1
Public Const SPAGHETI = 0
Public Const UNIVERSE = 1
Public Const MAXCOUNT = 100000
Public Const PHILOSOPHERS = 5
Public semaphore(PHILOSOPHERS - 1) As Integer
Public positi0n(1, PHILOSOPHERS - 1) As Integer
Public programcounter(PHILOSOPHERS - 1) As Long
Public statistics(PHILOSOPHERS - 1, 5, 1) As Long
Public names As Variant
Private Sub init()
    names = [{"Aquinas","Babbage","Carroll","Derrida","Erasmus"}]
    For j = 0 To PHILOSOPHERS - 2
        positi0n(0, j) = j + 1 'first fork in right hand
        positi0n(1, j) = j     'second fork in left hand
    Next j
    If DIJKSTRASOLUTION Then
        positi0n(0, PHILOSOPHERS - 1) = j '  first fork in left hand
        positi0n(1, PHILOSOPHERS - 1) = 0 'second fork in right hand
    Else
        positi0n(0, PHILOSOPHERS - 1) = 0 'first fork in right hand
        positi0n(1, PHILOSOPHERS - 1) = j 'second fork in left hand
    End If
End Sub
Private Sub philosopher(subject As Integer, verb As Integer, objekt As Integer)
    statistics(subject, verb, objekt) = statistics(subject, verb, objekt) + 1
    If verb < 2 Then
        If semaphore(positi0n(objekt, subject)) <> verb Then
            If Not HOLDON Then
                'can't get a fork, release first fork if subject has it, and
                'this won't toggle the semaphore if subject hasn't firt fork
                semaphore(positi0n(FRSTFORK, subject)) = 1 - objekt
                'next round back to try to get first fork
                programcounter(subject) = 0
            End If
        Else
            'just toggle semaphore and move on
            semaphore(positi0n(objekt, subject)) = 1 - verb
            programcounter(subject) = (programcounter(subject) + 1) Mod 6
        End If
    Else
        'when eating or thinking, (100*(X-1)/X)% continue eating or thinking
        '(100/X)% advance program counter
        programcounter(subject) = IIf(X * Rnd > 1, verb, verb + 1) Mod 6
    End If
End Sub
Private Sub dine()
    Dim ph As Integer
    Do While TC < MAXCOUNT
        For ph = 0 To PHILOSOPHERS - 1
            Select Case programcounter(ph)
                Case 0: philosopher ph, GETS, FRSTFORK
                Case 1: philosopher ph, GETS, SCNDFORK
                Case 2: philosopher ph, EATS, SPAGHETI
                Case 3: philosopher ph, PUTS, FRSTFORK
                Case 4: philosopher ph, PUTS, SCNDFORK
                Case 5: philosopher ph, THKS, UNIVERSE
            End Select
            TC = TC + 1
        Next ph
    Loop
End Sub
Private Sub show()
    Debug.Print "Stats", "Gets", "Gets", "Eats", "Puts", "Puts", "Thinks"
    Debug.Print "", "First", "Second", "Spag-", "First", "Second", "About"
    Debug.Print "", "Fork", "Fork", "hetti", "Fork", "Fork", "Universe"
    For subject = 0 To PHILOSOPHERS - 1
        Debug.Print names(subject + 1),
        For objekt = 0 To 1
            Debug.Print statistics(subject, GETS, objekt),
        Next objekt
        Debug.Print statistics(subject, EATS, SPAGHETI),
        For objekt = 0 To 1
            Debug.Print statistics(subject, PUTS, objekt),
        Next objekt
        Debug.Print statistics(subject, THKS, UNIVERSE)
    Next subject
End Sub
Public Sub main()
    init
    dine
    show
End Sub
```
```txt
Stats         Gets          Gets          Eats          Puts          Puts          Thinks
              First         Second        Spag-         First         Second        About
              Fork          Fork          hetti         Fork          Fork          Universe
Aquinas        5595          1902          5843          550           550           5560
Babbage        5811          2360          5585          529           529           5186
Carroll        6445          2359          4929          523           523           5221
Derrida        6341          1828          5479          545           545           5262
Erasmus        5998          1556          5891          550           550           5455

```


## Visual Basic .NET


This has three modes.

* In the first mode a dead lock will occur if each philosopher picks up their left fork before any pick up their right.
* In the second mode each philosopher will put down their left fork if they cannot pick up their right. This is susceptible to ''live lock'', where each philosopher keeps retrying but never makes any progress.
* In the third mode, each fork is numbered. The philosopher will always pick up a lower number fork before a higher number fork, thus preventing a dead-lock while guaranteeing forward progress.


```vbnet
Imports System.Threading
Module Module1
   Public rnd As New Random

   Sub Main()
       'Aristotle, Kant, Spinoza, Marx, and Russel
       Dim f1 As New Fork(1)
       Dim f2 As New Fork(2)
       Dim f3 As New Fork(3)
       Dim f4 As New Fork(4)
       Dim f5 As New Fork(5)

       Console.WriteLine("1: Deadlock")
       Console.WriteLine("2: Live lock")
       Console.WriteLine("3: Working")
       Select Console.ReadLine
           Case "1"
               Using _
                   Aristotle As New SelfishPhilosopher("Aristotle", f1, f2), _
                   Kant As New SelfishPhilosopher("Kant", f2, f3), _
                   Spinoza As New SelfishPhilosopher("Spinoza", f3, f4), _
                   Marx As New SelfishPhilosopher("Marx", f4, f5), _
                   Russel As New SelfishPhilosopher("Russel", f5, f1)

                   Console.ReadLine()
               End Using
           Case "2"
               Using _
                   Aristotle As New SelflessPhilosopher("Aristotle", f1, f2), _
                   Kant As New SelflessPhilosopher("Kant", f2, f3), _
                   Spinoza As New SelflessPhilosopher("Spinoza", f3, f4), _
                   Marx As New SelflessPhilosopher("Marx", f4, f5), _
                   Russel As New SelflessPhilosopher("Russel", f5, f1)

                   Console.ReadLine()
               End Using
           Case "3"
               Using _
                   Aristotle As New WisePhilosopher("Aristotle", f1, f2), _
                   Kant As New WisePhilosopher("Kant", f2, f3), _
                   Spinoza As New WisePhilosopher("Spinoza", f3, f4), _
                   Marx As New WisePhilosopher("Marx", f4, f5), _
                   Russel As New WisePhilosopher("Russel", f5, f1)

                   Console.ReadLine()
               End Using
       End Select
   End Sub

End Module
```



```vbnet
Class Fork
   Private ReadOnly m_Number As Integer
   Public Sub New(ByVal number As Integer)
       m_Number = number
   End Sub
   Public ReadOnly Property Number() As Integer
       Get
           Return m_Number
       End Get
   End Property
End Class
```



```vbnet
MustInherit Class PhilosopherBase
   Implements IDisposable

   Protected m_Disposed As Boolean
   Protected ReadOnly m_Left As Fork
   Protected ReadOnly m_Right As Fork
   Protected ReadOnly m_Name As String
   Public Sub New(ByVal name As String, ByVal right As Fork, ByVal left As Fork)
       m_Name = name
       m_Right = right
       m_Left = left
       Dim t As New Thread(AddressOf MainLoop)
       t.IsBackground = True
       t.Start()
   End Sub
   Protected Overridable Sub Dispose(ByVal disposing As Boolean)
       m_Disposed = True
   End Sub

   Public Sub Dispose() Implements IDisposable.Dispose
       Dispose(True)
       GC.SuppressFinalize(Me)
   End Sub
   Public ReadOnly Property Name() As String
       Get
           Return m_Name
       End Get
   End Property

   Public MustOverride Sub MainLoop()
End Class
```



###  Deadlock



```vbnet
Class SelfishPhilosopher
   Inherits PhilosopherBase
   Public Sub New(ByVal name As String, ByVal right As Fork, ByVal left As Fork)
       MyBase.New(name, right, left)
   End Sub

   Public Overrides Sub MainLoop()
       Do
           Console.WriteLine(Name & " sat down")
           SyncLock m_Left
               Console.WriteLine(Name & " picked up fork " & m_Left.Number)

               SyncLock m_Right
                   Console.WriteLine(Name & " picked up fork " & m_Right.Number)

                   Console.WriteLine(Name & " ate!!!!")

                   Console.WriteLine(Name & " put down fork " & m_Right.Number)
               End SyncLock


               Console.WriteLine(Name & " put down fork " & m_Left.Number)
           End SyncLock

           Console.WriteLine(Name & " stood up")

           Thread.Sleep(rnd.Next(0, 10000))
       Loop Until m_Disposed
   End Sub

End Class
```



###  Live Lock


```vbnet
Class SelflessPhilosopher
   Inherits PhilosopherBase

   Public Sub New(ByVal name As String, ByVal right As Fork, ByVal left As Fork)
       MyBase.New(name, right, left)
   End Sub

   Public Overrides Sub MainLoop()
       Do
           Console.WriteLine(Name & " sat down")
           Dim needDelay = False
TryAgain:
           If needDelay Then Thread.Sleep(rnd.Next(0, 10000))
           Try
               Monitor.Enter(m_Left)
               Console.WriteLine(Name & " picked up fork " & m_Left.Number)

               If Monitor.TryEnter(m_Right) Then
                   Console.WriteLine(Name & " picked up fork " & m_Right.Number)

                   Console.WriteLine(Name & " ate!!!!!!")

                   Console.WriteLine(Name & " put down fork " & m_Right.Number)
                   Monitor.Exit(m_Right)
               Else
                   Console.WriteLine(Name & " is going to wait")
                   needDelay = True
                   GoTo TryAgain
               End If
           Finally
               Console.WriteLine(Name & " put down fork " & m_Left.Number)
           End Try

           Console.WriteLine(Name & " stood up")

           Thread.Sleep(rnd.Next(0, 10000))

       Loop Until m_Disposed
   End Sub

End Class
```



###  Working


```vbnet
Class WisePhilosopher
   Inherits PhilosopherBase
   Public Sub New(ByVal name As String, ByVal right As Fork, ByVal left As Fork)
       MyBase.New(name, right, left)
   End Sub

   Public Overrides Sub MainLoop()
       Do
           Console.WriteLine(Name & " sat down")

           Dim first As Fork, second As Fork
           If m_Left.Number > m_Right.Number Then
               first = m_Left
               second = m_Right
           Else
               first = m_Right
               second = m_Left
           End If

           SyncLock first
               Console.WriteLine(Name & " picked up fork " & m_Left.Number)

               SyncLock second
                   Console.WriteLine(Name & " picked up fork " & m_Right.Number)

                   Console.WriteLine(Name & " ate!!!!")

                   Console.WriteLine(Name & " put down fork " & m_Right.Number)
               End SyncLock


               Console.WriteLine(Name & " put down fork " & m_Left.Number)
           End SyncLock

           Console.WriteLine(Name & " stood up")

           Thread.Sleep(rnd.Next(0, 10000))
       Loop Until m_Disposed
   End Sub

End Class
```



## zkl

The model used here is five seats, each with two forks. Sn:(Fn,Fn+1). Seat(n+1) shares a fork with seat n and so on. Fork are represented by an atomic bool, a bool that changes state atomically. Each philosopher is a thread. Each philosopher is ambidextrous, leading with a random hand. If the trailing hand can not get a fork, the already in hand fork is put down.

```zkl
var [const] forks=(5).pump(List,Atomic.Bool.fp(False)),  // True==fork in use
    seats=(5).pump(List,'wrap(n){ List(forks[n],forks[(n+1)%5]) });
fcn sitAndEat(name,n){  // assigned seating
   while(1){
      fa,fb:=seats[n].shuffle(); // ambidextrous
      if(fa.setIf(True,False)){  // got the first fork
	 if(fb.setIf(True,False)){  // got the other fork, nom nom time
	    name.println(" is eating");
	    Atomic.sleep((1).random(5));
	    fa.set(False); fb.set(False);  // put forks down
	    return();  // leave the table
	 }
	 else{
	    fa.set(False);  // put fork down, try again in a bit
	    name.println(": Could not get two forks");
	 }
      } else name.println(": Could not get first fork");
      Atomic.sleep((1).random(2));  // sits for a bit
   }
}
fcn philo([(seat,name)]){  // a thread
   while(1){  // eat and think forever
      name.println(" is thinking."); Atomic.sleep((1).random(5));
      sitAndEat(name,seat);
   }
}
```

The setIf method atomically sets the bool to the first value if it is currently the second value.

```zkl
T("Aristotle", "Kant", "Spinoza", "Marx", "Russell").enumerate()
.apply(philo.launch);

Atomic.sleep(100000);  // hang out in the REPL, aka thread keep alive
```

```txt

...
Aristotle: Could not get two forks
Russell: Could not get two forks
Aristotle: Could not get two forks
Russell: Could not get first fork
Spinoza: Could not get first fork
Kant is thinking.
Aristotle is eating
...

```



{{omit from|gnuplot}} <!-- Not applicable, no concurrency. -->
{{omit from|J}} <!-- current J implementations do not implement deadlocks -->
{{omit from|TI-89 BASIC}} <!-- Not applicable, no concurrency. -->
{{omit from|SVG}} <!-- Not applicable, no concurrency. -->
{{omit from|XSLT}} <!-- Not applicable, no concurrency. -->
