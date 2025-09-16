+++
title = "Old lady swallowed a fly"
description = ""
date = 2019-10-18T11:47:11Z
aliases = []
[extra]
id = 10317
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "ada",
  "algol_68",
  "autohotkey",
  "awk",
  "babel",
  "batch_file",
  "bbc_basic",
  "befunge",
  "c",
  "cobol",
  "common_lisp",
  "cpp",
  "csharp",
  "d",
  "elena",
  "elixir",
  "factor",
  "forth",
  "fortran",
  "free_pascal",
  "frege",
  "go",
  "haskell",
  "j",
  "java",
  "julia",
  "kotlin",
  "liberty_basic",
  "logo",
  "lua",
  "maple",
  "mathematica",
  "mercury",
  "nim",
  "ocaml",
  "perl",
  "perl_6",
  "phix",
  "php",
  "picolisp",
  "powershell",
  "python",
  "racket",
  "related_tasks",
  "rexx",
  "ruby",
  "rust",
  "scala",
  "scratch",
  "seed7",
  "sidef",
  "tcl",
  "txr",
  "unix_shell",
  "ursa",
  "zkl",
]
+++

## Task

Present a program which emits the lyrics to the song   ''[[wp:There Was an Old Lady Who Swallowed a Fly|I Knew an Old Lady Who Swallowed a Fly]]'',   taking advantage of the repetitive structure of the song's lyrics.

This song has multiple versions with slightly different lyrics, so all these programs might not emit identical output.


## Related tasks

*   [[99 Bottles of Beer]]
*   [[The Twelve Days of Christmas]]





## Ada


```Ada
with Ada.Text_IO, Ada.Containers.Indefinite_Doubly_Linked_Lists; use Ada.Text_IO;

procedure Swallow_Fly is

   package Strings is new Ada.Containers.Indefinite_Doubly_Linked_Lists(String);

   Lines, Animals: Strings.List;

   procedure Swallow(Animal: String;
                     Second_Line: String;
                     Permanent_Second_Line: Boolean := True) is

      procedure Print(C: Strings.Cursor) is
      begin
         Put_Line(Strings.Element(C));
      end Print;

   begin
      Put_Line("There was an old lady who swallowed a " & Animal & ",");
      Put_Line(Second_Line);
      if not Animals.Is_Empty then
         Lines.Prepend("She swallowed the " & Animal & " to catch the " &
			 Animals.Last_Element & ",");
      end if;
      Lines.Iterate(Print'Access);
      New_Line;
      if Permanent_Second_Line then
         Lines.Prepend(Second_Line);
      end if;
      Animals.Append(Animal); -- you need "to catch the " most recent animal
   end Swallow;

   procedure Swallow_TSA(Animal: String; Part_Of_Line_2: String) is
   begin
      Swallow(Animal, Part_Of_Line_2 &", to swallow a " & Animal & ";", False);
   end Swallow_TSA;

   procedure Swallow_SSA(Animal: String; Part_Of_Line_2: String) is
   begin
      Swallow(Animal, Part_Of_Line_2 &" she swallowed a " & Animal & ";", False);
   end Swallow_SSA;

begin
   Lines.Append("Perhaps she'll die!");

   Swallow("fly", "But I don't know why she swallowed the fly,");
   Swallow("spider",  "That wriggled and jiggled and tickled inside her;");
   Swallow_TSA("bird", "Quite absurd");
   Swallow_TSA("cat", "Fancy that");
   Swallow_TSA("dog", "What a hog");
   Swallow_TSA("pig", "Her mouth was so big");
   Swallow_TSA("goat","She just opened her throat");
   Swallow_SSA("cow", "I don't know how");
   Swallow_TSA("donkey", "It was rather wonky");

   Put_Line("There was an old lady who swallowed a horse ...");
   Put_Line("She's dead, of course!");
end Swallow_Fly;
```



## ALGOL 68


### Using Logic

```algol68
#!/usr/local/bin/a68g --script #

STRING sw=" swallow ",swd=sw[:UPB sw-1]+"ed ", tsa=". To"+sw+"a";

INT count prev := 0; [9]STRING prev;

PROC vs = (STRING in wot,[]STRING co)VOID: (
  STRING wot = " "+in wot;
  printf(($g$,"I know an old lady who",swd,"a",wot,".",$l$));
  IF UPB co = 1 THEN
    printf(($gl$,co))
  ELIF UPB co > 1 THEN
    printf(($g$,co,wot+".",$l$))
  FI;
  IF count prev NE UPB prev THEN
    prev[count prev+:=1]:=wot;
    FOR i FROM count prev BY -1 TO 2 DO
      printf(($gl$,"She"+swd+"the"+prev[i]+" to catch the"+prev[i-1]+"."))
    OD;
    printf(($gl$,"I don't know why she"+swd+"the fly.",
                 "Perhaps she'll die.", $l$))
  FI
);

vs("fly",());
vs("spider","That wriggled and jiggled and tickled inside her.");
vs("Bird",("Quite absurd",tsa));
vs("Cat",("Fancy that",tsa));
vs("Dog",("What a hog",tsa));
vs("Pig",("Her mouth was so big",tsa));
vs("Goat",("She just opened her throat",tsa));
vs("Cow",("I don't know how",tsa));
vs("Donkey",("It was rather wonky",tsa));
vs("Horse","She's dead, of course!")
```



### Using a dictionary

```algol68
#!/usr/local/bin/a68g --script #

STRING a="WBXAY",b="WCXBY",c="WDXCY",d="WEXDY",
       e="WFXEY",f="WGXFY",g="WHXGY",h="WIXHY",
       k="K",z="Z",l="";

[]STRING
  w=( # Assuming ASCII ordering #
    "fly","spider","Bird","Cat","Dog","Pig","Goat","Cow","Donkey","Horse",

    "I don't know why she swallowed the fly.",
    "That wriggled and jiggled and tickled inside her.",
    "Quite absurd","Fancy that","What a hog",
    "Her mouth was so big","She just opened her throat",
    "I don't know how","It was rather wonky",
    "She's dead, of course!",

    "I know an old lady who swallowed a ",
    ". To swallow a ",
    "She swallowed the "," to catch the ",".",
    "Perhaps she'll die."),
  v=(
    "UAY",k,z,l,
    "UBY","L",a,k,z,l,
    "UCY","MVCY",b,a,k,z,l,
    "UDY","NVDY",c,b,a,k,z,l,
    "UEY","OVEY",d,c,b,a,k,z,l,
    "UFY","PVFY",e,d,c,b,a,k,z,l,
    "UGY","QVGY",f,e,d,c,b,a,k,z,l,
    "UHY","RVHY",g,f,e,d,c,b,a,k,z,l,
    "UIY","SVIY",h,g,f,e,d,c,b,a,k,z,l,
    "UJY","T");

FOR i TO UPB v DO
  FOR j TO UPB v[i] DO
    print(w[ABS v[i][j] - ABS "A" + 1])
  OD;
  print(new line)
OD
```



## AutoHotkey

```AutoHotkey
Animals := [["fly",	"I don't know why she swallowed the "]
	  , ["spider",	"That wriggled and jiggled and tickled inside her"]
	  , ["bird",	"Quite absurd"]
	  , ["cat",	"Fancy that"]
	  , ["dog",	"What a hog"]
	  , ["pig",	"Her mouth was so big"]
	  , ["goat",	"She just opened her throat"]
	  , ["cow",	"I don't know how"]
	  , ["donkey",	"It was rather wonky"]
	  , ["horse",	"She's dead, of course!"]]

for i, V in Animals {
	Output .= "I know an old lady who swallowed a " V.1 ".`n"
	. (i = 1 ? Saved := V.2 V.1 ".`nPerhaps she'll die.`n`n"
	: V.2 (i = Animals.MaxIndex() ? "" : (i = 2 ? "" : ". To swallow a " V.1) ".`n"
	. (Saved := "She swallowed the " V.1 " to catch the " Animals[i - 1].1 ".`n" Saved)))
}

MsgBox, % Output
```



## AWK


```AWK

# syntax: GAWK -f OLD_LADY_SWALLOWED_A_FLY.AWK
BEGIN {
    arr[++i] = "fly/"
    arr[++i] = "spider/That wriggled and jiggled and tickled inside her"
    arr[++i] = "bird/Quite absurd@"
    arr[++i] = "cat/Fancy that@"
    arr[++i] = "dog/What a hog@"
    arr[++i] = "pig/Her mouth was so big@"
    arr[++i] = "goat/Opened her throat and down went the goat"
    arr[++i] = "cow/I don't know how@"
    arr[++i] = "donkey/It was rather wonkey@"
    arr[++i] = "horse/She's dead of course"
    leng = i # array length
    for (i=1; i<=leng; i++) {
      s = arr[i]
      A[i] = substr(s,1,index(s,"/")-1) # critter name
      text = substr(s,index(s,"/")+1)
      sub(/@/," to swallow a "A[i],text)
      printf("I know an old lady who swallowed a %s.\n",A[i])
      if (text != "") {
        printf("%s.\n",text)
      }
      if (i == leng) {
        break
      }
      for (j=i; j>1; j--) {
        printf("She swallowed the %s to catch the %s.\n",A[j],A[j-1])
      }
      printf("I don't know why she swallowed the fly.\n")
      printf("Perhaps she'll die.\n\n")
    }
    exit(0)
}

```



## Babel


```babel
((main {fly !})

(fly
    {{"There was an old lady who swallowed a " <<
        iter 1 - dup
        <- 0 animal ! nl << ->
           1 animal !    <<
        {iter 10 ~=}{
            " " <<
            {iter 1 =}{last}{fnord} ifte
            iter 1 - 0 animal ! nl <<
                {"She swallowed the " <<
                this_iter ! 0 animal ! <<
                " to catch the " <<
                next_iter ! 0 animal ! nl <<
                {iter 2 =}
                    {8 1 animal ! nl <<}
                    {fnord}
                ifte}
            11 iter - 1 -
            times}
            {fnord}
        ifte
        "But I don't know why she swallowed the fly\nPerhaps she'll die\n\n" <<}
    animals len
    times})

(next_iter {10 iter - })
(this_iter {next_iter ! 1 -})

(animal { <- <- animals -> ith -> ith})

-- There are 10 animals
(animals
   (("horse"    "She's dead of course...\n")
    ("donkey"   "It was rather wonkey! To swallow a")
    ("cow"      "I don't know how she swallowed a")
    ("goat"     "She just opened her throat! And swallowed the")
    ("pig"      "Her mouth was so big, to swallow a")
    ("dog"      "What a hog! To swallow a")
    ("cat"      "Fancy that! She swallowed a")
    ("bird"     "Quite absurd to swallow a")
    ("spider"   "That wriggled and jiggled and tickled inside her")
    ("fly"      " "))))

```



## Batch File


```dos
@echo off
setlocal enabledelayedexpansion

	%== An "ugly" pseudo-array ===%
set pseudo=^
fly/@^
spider/That_wiggled_and_jiggled_and_tickled_inside_her,@^
bird/How_absurd,_to_swallow_a_bird,@^
cat/Imagine_that._She_swallowed_a_cat,@^
dog/What_a_hog_to_swallow_a_dog,@^
goat/She_just_opened_her_throat_and_swallowed_that_goat,@^
cow/I_don't_know_how_she_swallowed_that_cow,@^
horse/She's_dead_of_course...

	%== Counting and seperating... ===%
set str=!pseudo!
:count
if "!str!"=="" goto print_song
for /f "tokens=1,* delims=@" %%A in ("!str!") do (
	set /a cnt+=1
	for /f "tokens=1,2 delims=/" %%C in ("%%A") do (
		set animal!cnt!=%%C
		set comment!cnt!=%%D
	)
	set str=%%B
)
goto count

	%== Print the song ===%
:print_song
for /l %%i in (1,1,!cnt!) do (
	echo There was an old lady who swallowed a !animal%%i!.
	if not "!comment%%i!"=="" echo !comment%%i:_= !
	if %%i equ !cnt! goto done

	for /l %%j in (%%i,-1,2) do (
		set/a prev=%%j-1
		call set prev_animal=%%animal!prev!%%
		echo She swallowed the !animal%%j! to catch the !prev_animal!.
	)
	echo I don't know why she swallowed the fly.
	echo Perhaps she'll die.
	echo.
)
:done
pause>nul&exit/b 0
```



## Befunge

We start with a collection of reusable phrases stored as a linked list. We then build up the sequence of indices into that list necessary to produce the song (this step is largely programmatic, because of the repetitive nature, although some of the verses require special case handling). Finally we iterate over the indices and print out the associated phrases.


```befunge
055*46*146*1->00p 36268>5\:4\:2v >\#%"O"/#:3#:+#< g48*- >1-:!#v_\1+::"O"%\"O"/v
>-#2:#\8#1`#:|#-1:-1\7_^#`g00:+<>\#%"O"/#::$#<3#$+g48*-v^\,+*+ 55!:*!!-"|":g+3<
             >$ 36 26 58 49 81 36 26 10 \1-:#^\_^#:-1\+<00_@#:>#<$<
DI know an old lady who swallowed a F.|I don't know why she swallowed the 8.|Pe
rhaps she'll die.||5.|She swallowed the / to catch the $fly0. To swallow a 'spi
derS.|That wriggled and jiggled and tickled inside her%Bird/.|Quite absurd$Cat-
.|Fancy that$Dog-.|What a hog$Pig7.|Her mouth was so big%Goat=.|She just opened
 her throat$Cow3.|I don't know how'Donkey6.|It was rather wonky&Horse:.|She's d
ead, of course!|
```



## BBC BASIC

This prints the lyrics from the Wikipedia page, more or less. I don't know anything about goats and donkeys and the like.

```bbcbasic
REM>
oldlady
DIM swallowings$(6, 1)
swallowings$() = "fly", "+why", "spider", "That wriggled and wiggled and tickled inside her", "bird", ":How absurd", "cat", ":Fancy that", "dog", ":What a hog", "cow", "+how", "horse", "She's dead, of course"
FOR i% = 0 TO 6
  PRINT "There was an old lady who swallowed a "; swallowings$(i%, 0); "..."
  PROC_comment_on_swallowing(swallowings$(i%, 0), swallowings$(i%, 1))
  IF i% > 0 AND i% < 6 THEN
    FOR j% = i% TO 1 STEP -1
      PRINT "She swallowed the "; swallowings$(j%, 0); " to catch the "; swallowings$(j% - 1, 0); ","
    NEXT
    PROC_comment_on_swallowing(swallowings$(0, 0), swallowings$(0, 1))
  ENDIF
  PRINT
NEXT
END
:
DEF PROC_comment_on_swallowing(animal$, observation$)
CASE LEFT$(observation$, 1) OF
WHEN "+":
  PRINT "I don't know "; MID$(observation$, 2); " she swallowed a "; animal$;
  IF animal$ = "fly" THEN PRINT " -- perhaps she'll die";
  PRINT "!"
WHEN ":"
  PRINT MID$(observation$, 2); ", to swallow a "; animal$; "!"
OTHERWISE
  PRINT observation$; "!"
ENDCASE
ENDPROC
```



## C


```c
#include <stdio.h>
static char const *animals[] = {
    "fly",
    "spider",
    "bird",
    "cat",
    "dog",
    "goat",
    "cow",
    "horse"
};
static char const *verses[]  = {
    "I don't know why she swallowed that fly.\nPerhaps she'll die\n",
    "That wiggled and jiggled and tickled inside her",
    "How absurd, to swallow a bird",
    "Imagine that. She swallowed a cat",
    "What a hog to swallow a dog",
    "She just opened her throat and swallowed that goat",
    "I don't know how she swallowed that cow",
    "She's dead of course"
};

#define LEN(ARR) (sizeof ARR / sizeof *ARR)

int main(void)
{
    for (size_t i = 0; i < LEN(animals); i++) {
        printf("There was an old lady who swallowed a %s\n%s\n", animals[i], verses[i]);
        for (size_t j = i; j > 0 && i < LEN(animals) - 1; j--) {
            printf("She swallowed the %s to catch the %s\n", animals[j], animals[j-1]);
            if (j == 1) {
                printf("%s\n", verses[0]);
            }
        }
    }
}
```



## C++

```cpp
#include <iostream>

const char *REASON = "She swallowed the %s to catch the %s\n";
const char *CREATURES[] = { "fly", "spider", "bird", "cat", "dog", "goat", "cow", "horse" };
const char *COMMENTS[] = {
    "I don't know why she swallowed that fly.\nPerhaps she'll die\n",
    "That wiggled and jiggled and tickled inside her",
    "How absurd, to swallow a bird",
    "Imagine that. She swallowed a cat",
    "What a hog to swallow a dog",
    "She just opened her throat and swallowed that goat",
    "I don't know how she swallowed that cow",
    "She's dead of course"
};

int main() {
    auto max = sizeof(CREATURES) / sizeof(char*);
    for (size_t i = 0; i < max; ++i) {
        std::cout << "There was an old lady who swallowed a " << CREATURES[i] << '\n';
        std::cout << COMMENTS[i] << '\n';
        for (int j = i; j > 0 && i < max - 1; --j) {
            printf(REASON, CREATURES[j], CREATURES[j - 1]);
            if (j == 1) {
                std::cout << COMMENTS[j - 1] << '\n';
            }
        }
    }

    return 0;
}
```

```txt
There was an old lady who swallowed a fly
I don't know why she swallowed that fly.
Perhaps she'll die

There was an old lady who swallowed a spider
That wiggled and jiggled and tickled inside her
She swallowed the spider to catch the fly
I don't know why she swallowed that fly.
Perhaps she'll die

There was an old lady who swallowed a bird
How absurd, to swallow a bird
She swallowed the bird to catch the spider
She swallowed the spider to catch the fly
I don't know why she swallowed that fly.
Perhaps she'll die

There was an old lady who swallowed a cat
Imagine that. She swallowed a cat
She swallowed the cat to catch the bird
She swallowed the bird to catch the spider
She swallowed the spider to catch the fly
I don't know why she swallowed that fly.
Perhaps she'll die

There was an old lady who swallowed a dog
What a hog to swallow a dog
She swallowed the dog to catch the cat
She swallowed the cat to catch the bird
She swallowed the bird to catch the spider
She swallowed the spider to catch the fly
I don't know why she swallowed that fly.
Perhaps she'll die

There was an old lady who swallowed a goat
She just opened her throat and swallowed that goat
She swallowed the goat to catch the dog
She swallowed the dog to catch the cat
She swallowed the cat to catch the bird
She swallowed the bird to catch the spider
She swallowed the spider to catch the fly
I don't know why she swallowed that fly.
Perhaps she'll die

There was an old lady who swallowed a cow
I don't know how she swallowed that cow
She swallowed the cow to catch the goat
She swallowed the goat to catch the dog
She swallowed the dog to catch the cat
She swallowed the cat to catch the bird
She swallowed the bird to catch the spider
She swallowed the spider to catch the fly
I don't know why she swallowed that fly.
Perhaps she'll die

There was an old lady who swallowed a horse
She's dead of course
```


## C#

```c#
using System;

namespace OldLady
{
    internal class Program
    {
        private const string reason = "She swallowed the {0} to catch the {1}";
        private static readonly string[] creatures = {"fly", "spider", "bird", "cat", "dog", "goat", "cow", "horse"};

        private static readonly string[] comments =
        {
            "I don't know why she swallowed that fly.\nPerhaps she'll die\n",
            "That wiggled and jiggled and tickled inside her",
            "How absurd, to swallow a bird",
            "Imagine that. She swallowed a cat",
            "What a hog to swallow a dog",
            "She just opened her throat and swallowed that goat",
            "I don't know how she swallowed that cow",
            "She's dead of course"
        };

        private static void Main()
        {
            int max = creatures.Length;
            for (int i = 0; i < max; i++)
            {
                Console.WriteLine("There was an old lady who swallowed a {0}", creatures[i]);
                Console.WriteLine(comments[i]);
                for (int j = i; j > 0 && i < max - 1; j--)
                {
                    Console.WriteLine(reason, creatures[j], creatures[j - 1]);
                    if (j == 1)
                    {
                        Console.WriteLine(comments[j - 1]);
                    }
                }
            }
            Console.Read();
        }
    }
}
```



## COBOL


```cobol
        IDENTIFICATION DIVISION.
        PROGRAM-ID. OLD-LADY.

        DATA DIVISION.
        WORKING-STORAGE SECTION.

        01  LYRICS.
            03  THERE-WAS       PIC X(38) VALUE
            "There was an old lady who swallowed a ".
            03  SHE-SWALLOWED   PIC X(18) VALUE "She swallowed the ".
            03  TO-CATCH        PIC X(14) VALUE " to catch the ".
        01  ANIMALS.
            03  FLY.
                05  NAME        PIC X(6) VALUE "fly".
                05  VERSE       PIC X(60) VALUE
            "I don't know why she swallowed a fly. Perhaps she'll die.".
            03  SPIDER.
                05  NAME        PIC X(6) VALUE "spider".
                05  VERSE       PIC X(60) VALUE
            "That wiggled and jiggled and tickled inside her.".
            03  BIRD.
                05  NAME        PIC X(6) VALUE "bird".
                05  VERSE       PIC X(60) VALUE
            "How absurd, to swallow a bird.".
            03  CAT.
                05  NAME        PIC X(6) VALUE "cat".
                05  VERSE       PIC X(60) VALUE
            "Imagine that, she swallowed a cat.".
            03  DOG.
                05  NAME        PIC X(6) VALUE "dog".
                05  VERSE       PIC X(60) VALUE
            "What a hog, to swallow a dog.".
            03  GOAT.
                05  NAME        PIC X(6) VALUE "goat".
                05  VERSE       PIC X(60) VALUE
            "She just opened her throat and swallowed that goat.".
            03  COW.
                05  NAME        PIC X(6) VALUE "cow".
                05  VERSE       PIC X(60) VALUE
            "I don't know how she swallowed that cow.".
            03  HORSE.
                05  NAME        PIC X(6) VALUE "horse".
                05  VERSE       PIC X(60) VALUE
            "She's dead, of course.".
        01  ANIMAL-ARRAY REDEFINES ANIMALS.
            03  ANIMAL OCCURS 8 TIMES.
                05  NAME        PIC X(6).
                05  VERSE       PIC X(60).
        01  MISC.
            03  LINE-OUT        PIC X(80).
            03  A-IDX           PIC 9(2).
            03  S-IDX           PIC 9(2).

        PROCEDURE DIVISION.
        MAIN SECTION.
            PERFORM DO-ANIMAL
                VARYING A-IDX FROM 1 BY 1 UNTIL A-IDX > 8.
            STOP RUN.

        DO-ANIMAL SECTION.
            MOVE SPACES TO LINE-OUT.
            STRING
                THERE-WAS DELIMITED BY SIZE,
                NAME OF ANIMAL(A-IDX) DELIMITED BY SPACE,
                ","
                INTO LINE-OUT
            END-STRING.
            DISPLAY LINE-OUT.
            IF A-IDX > 1 THEN
                DISPLAY VERSE OF ANIMAL(A-IDX)
            END-IF.
            IF A-IDX = 8 THEN
                EXIT SECTION
            END-IF.
            PERFORM DO-SWALLOW
                VARYING S-IDX FROM A-IDX BY -1 UNTIL S-IDX = 1.
            DISPLAY VERSE OF ANIMAL(1).
            DISPLAY SPACES.

        DO-SWALLOW SECTION.
            MOVE SPACES TO LINE-OUT.
            STRING
                SHE-SWALLOWED DELIMITED BY SIZE,
                NAME OF ANIMAL(S-IDX) DELIMITED BY SPACE,
                TO-CATCH DELIMITED BY SIZE,
                NAME OF ANIMAL(S-IDX - 1) DELIMITED BY SPACE
                INTO LINE-OUT
            END-STRING.
            DISPLAY LINE-OUT.


```



## Common Lisp

A rather iterative, rather than recursive solution.  The lyrics match the original Burl Ives recording (not the later animated cartoon):


```lisp
(defun verse  (what remark &optional always die) (list what remark always die))
(defun what   (verse) (first verse))
(defun remark (verse) (second verse))
(defun always (verse) (third verse))
(defun die    (verse) (fourth verse))

(defun ssa (what remark &optional always die )
    (verse what (format nil "~a she swallowed a ~a!" remark what always die)))
(defun tsa (what remark &optional always die)
    (verse what (format nil "~a, to swallow a ~a!" remark what)))
(defun asa (what remark &optional always die)
    (verse what (format nil "~a, and swallowed a ~a!" remark what)))


(let ((verses (list
        (verse "fly" "I don't know why she swallowed the fly" T)
        (verse "spider" "That wriggled and jiggled and tickled inside her" T)
        (tsa   "bird" "Now how absurd")
        (tsa   "cat" "Now fancy that")
        (tsa   "dog" "what a hog")
        (asa   "goat" "She just opened her throat")
        (ssa   "cow" "I don't know how")
        (verse "horse" "She's dead, of course!" T T))))

  (loop for verse in verses for i from 0 doing
    (let ((it (what verse)))
      (format t "I know an old lady who swallowed a ~a~%" it)
      (format t "~a~%" (remark verse))
      (if (not (die verse)) (progn
        (if (> i 0)
          (loop for j from (1- i) downto 0 doing
            (let* ((v (nth j verses)))
              (format t "She swallowed the ~a to catch the ~a~%" it (what v))
              (setf it (what v))
              (if (always v)
                (format t "~a~a~%" (if (= j 0) "But " "") (remark v))))))
        (format t "Perhaps she'll die. ~%~%"))))))
```


```txt
I know an old lady who swallowed a fly
I don't know why she swallowed the fly
Perhaps she'll die.

I know an old lady who swallowed a spider
That wriggled and jiggled and tickled inside her
She swallowed the spider to catch the fly
But I don't know why she swallowed the fly
Perhaps she'll die.

...

I know an old lady who swallowed a cow
I don't know how she swallowed a cow!
She swallowed the cow to catch the goat
She swallowed the goat to catch the dog
She swallowed the dog to catch the cat
She swallowed the cat to catch the bird
She swallowed the bird to catch the spider
That wriggled and jiggled and tickled inside her
She swallowed the spider to catch the fly
But I don't know why she swallowed the fly
Perhaps she'll die.

I know an old lady who swallowed a horse
She's dead, of course!
```



## D

```d
import core.stdc.stdio;

immutable data = [
"_ha _c _e _p,/Quite absurd_f_p;_`cat,/Fancy that_fcat;_j`dog,/What a hog"~
"_fdog;_l`pig,/Her mouth_qso big_fpig;_d_r,/She just opened her throat_f_"~
"r;_icow,/_mhow she_ga cow;_k_o,/It_qrather wonky_f_o;_a_o_bcow,_khorse.."~
"./She's dead, of course!/","_a_p_b_e ","/S_t "," to catch the ","fly,/Bu"~
"t _mwhy s_t fly,/Perhaps she'll die!//_ha","_apig_bdog,_l`","spider,/Tha"~
"t wr_nj_ntickled inside her;_aspider_b_c",", to_s a ","_sed ","There_qan"~
" old lady who_g","_a_r_bpig,_d","_acat_b_p,_","_acow_b_r,_i","_adog_bcat"~
",_j","I don't know ","iggled and ","donkey","bird"," was ","goat"," swal"~
"low","he_gthe"];

bool oldLady(in string part, bool s=false) nothrow @nogc {
    foreach (immutable ch; part) {
        if (s)
            s = oldLady(data[ch - '_'], false);
        else if (ch == '_')
            s = true;
        else
            putchar(ch == '/' ? '\n' : ch);
    }

    return s;
}

void main() {
    data[0].oldLady;
}
```


A more structured alternative version:

```d
enum Action { once, every, die }

immutable struct T {
    string anim;
    Action act;
    string phrase;
}

immutable T[10] animals = [
    T("horse",  Action.die,   "She's dead, of course!"),
    T("donkey", Action.once,  "It was rather wonky. To swallow a donkey."),
    T("cow",    Action.once,  "I don't know how. To swallow a cow."),
    T("goat",   Action.once,  "She just opened her throat. To swallow a goat."),
    T("pig",    Action.once,  "Her mouth was so big. To swallow a pig."),
    T("dog",    Action.once,  "What a hog. To swallow a dog."),
    T("cat",    Action.once,  "Fancy that. To swallow a cat."),
    T("bird",   Action.once,  "Quite absurd. To swallow a bird."),
    T("spider", Action.once,  "That wriggled and jiggled and tickled inside her."),
    T("fly",    Action.every, "I don't know why she swallowed the fly.")];

void main() {
    import std.stdio;

    foreach_reverse (immutable i; 0 .. animals.length) {
        writeln("I know an old lady who swallowed a ",
                animals[i].anim, ".");
        animals[i].phrase.writeln;

        if (animals[i].act == Action.die)
            break;

        foreach (immutable j, immutable r; animals[i + 1 .. $]) {
            writeln("She swallowed the ", animals[i + j].anim,
                    " to catch the ", r.anim, ".");
            if (r.act == Action.every)
                r.phrase.writeln;
        }

        "Perhaps she'll die.\n".writeln;
    }
}
```



## Elena

ELENA 4.1 :

```elena
import extensions;

const Creatures = new string[]::("fly", "spider", "bird", "cat", "dog", "goat", "cow", "horse");
const Comments = new string[]::
(
            "I don't know why she swallowed that fly"$10"Perhaps she'll die",
            "That wiggled and jiggled and tickled inside her",
            "How absurd, to swallow a bird",
            "Imagine that. She swallowed a cat",
            "What a hog to swallow a dog",
            "She just opened her throat and swallowed that goat",
            "I don't know how she swallowed that cow",
            "She's dead of course"
);

public program()
{
    for(int i := 0, i < Creatures.Length, i += 1)
    {
        console
            .printLineFormatted("There was an old lady who swallowed a {0}",Creatures[i])
            .printLine(Comments[i]);

        if(i != 0 && i != Creatures.Length - 1)
        {
            for(int j := i, j > 0, j -= 1)
            {
                console.printLineFormatted("She swallowed the {0} to catch the {1}",Creatures[j],Creatures[j - 1])
            };

            console.writeLine(Comments[0])
        };

        console.writeLine()
    }
}
```



## Elixir

```elixir
defmodule Old_lady do
  @descriptions [
    fly:    "I don't know why S",
    spider: "That wriggled and jiggled and tickled inside her.",
    bird:   "Quite absurd T",
    cat:    "Fancy that, S",
    dog:    "What a hog, S",
    goat:   "She opened her throat T",
    cow:    "I don't know how S",
    horse:  "She's dead, of course.",
  ]

  def swallowed do
    {descriptions, animals} = setup(@descriptions)
    Enum.each(Enum.with_index(animals), fn {animal, idx} ->
      IO.puts "There was an old lady who swallowed a #{animal}."
      IO.puts descriptions[animal]
      if animal == :horse, do: exit(:normal)
      if idx > 0 do
        Enum.each(idx..1, fn i ->
          IO.puts "She swallowed the #{Enum.at(animals,i)} to catch the #{Enum.at(animals,i-1)}."
          case Enum.at(animals,i-1) do
            :spider -> IO.puts descriptions[:spider]
            :fly -> IO.puts descriptions[:fly]
            _ -> :ok
          end
        end)
      end
      IO.puts "Perhaps she'll die.\n"
    end)
  end

  def setup(descriptions) do
    animals = Keyword.keys(descriptions)
    descs = Enum.reduce(animals, descriptions, fn animal, acc ->
              Keyword.update!(acc, animal, fn d ->
                case String.last(d) do
                  "S" -> String.replace(d, ~r/S$/, "she swallowed a #{animal}.")
                  "T" -> String.replace(d, ~r/T$/, "to swallow a #{animal}.")
                  _   -> d
                end
              end)
            end)
    {descs, animals}
  end
end

Old_lady.swallowed
```



## Factor

Windows users should put zlib1.dll[http://downloads.factorcode.org/dlls/] to factor folder.

```factor
USING: base85 compression.zlib ;

"c%1E4%WlIU5WMphn^P`UexSD=s^-?Jk$NVE7-Gs=fQ9_`T}Y~ggi4hvMSb{SX}vpQXJ;^Yqok7%xd(0
mjR3>N1W_UQ$c@1$1#sAzsbTkHfHerT%K*K_NT><Cl4r<3ZyEa}o#KN}<)twov|KQ@`BE=GXdzugXdWO
s-E}7At$_Vm9CX{KSX)nUpq1~~%N3WyS`ZLg9$IzcccWRh+KGlek2*-;TR+lUB6EZs0X5<&U()_dvQXE
CJ#gDjv>e5yCRDAFrgX{pAnt$DPGHxt*E9$RMRBPeRcoXvT{6xN%o=~9@t{fLM`G}XV^A6Hk!HSBn{YM
ylrFhv&t_M?=}Lzm^6W<+00(I)uM#EY@ah;z@Y)zDUk;J&o^8C<;g7LlMIS{^*(al_mjFQv=BG_DyZn
<}rUt#FCDtKB9S`Y4jg+0PuB?Qt-&(11p?caq^S=1C`$D1fa<y6|YD*77a{4949T_-MVet;6abaEn"

base85> 3215 <compressed> uncompress >string print
```



## Forth

swallow-addr is obviously a good candidate for an object, but Forth has many OO candidates - we won't settle that argument here.

```forth
: string, ( c-addr u -- ) \ store string at HERE , with a count
  dup c, here swap dup allot move ;

\ doubly linked list: (0|prev, 0|next, aside?, cstr animal; cstr aside)
\ aside? is true if the aside is always displayed.
variable swallowed
variable first
: >next ( swallow-addr -- swallow-addr' )
  cell+ @ ;
: >aside? ( swallow-addr -- f )
  2 cells + @ ;
: >animal ( swallow-addr -- c-addr u )
  3 cells + count ;
: >aside ( swallow-addr -- c-addr u )
  >animal + count ;

: swallow ( "animal" -- )
  align swallowed @ if here swallowed @ cell+ ! else here first ! then
  here swallowed @ , swallowed !
  0 , 0 , parse-word string, ; \ data structure still needs the aside
: always ( -- )  \ set aside? of last-defined swallow to true
  swallowed @ 2 cells + on ;
: aside ( "aside" -- )
  0 parse string, ;

swallow fly always aside But I don't know why she swallowed the fly,
swallow spider always aside That wriggled and jiggled and tickled inside her;
swallow bird aside Quite absurd, she swallowed the bird;
swallow cat aside Fancy that, she swallowed the cat;
swallow dog aside What a hog, she swallowed the dog;
swallow pig aside Her mouth was so big, she swallowed the pig;
swallow goat aside She just opened her throat, she swallowed the goat;
swallow cow aside I don't know how, she swallowed the cow;
swallow donkey aside It was rather wonky, she swallowed the donkey;

: ?aside ( swallow-addr -- )  \ print aside if aside? is true
  dup >aside? if >aside cr type else drop then ;

: reasons ( swallow-addr -- )  \ print reasons she swallowed something
  begin dup @ while
    dup cr ." She swallowed the " >animal type ."  to catch the "
    @ dup >animal type ." ," dup ?aside
  repeat drop ;

: verse ( swallow-addr -- )
  cr ." There was an old lady who swallowed a " dup >animal type ." ,"
  dup >aside cr type
  reasons
  cr ." Perhaps she'll die!" ;

: song ( -- )
  first @ begin dup verse cr >next dup 0= until drop
  cr ." There was an old lady who swallowed a horse..."
  cr ." She's dead, of course!" ;
```



## Fortran


```Fortran

program fly
  !A program to print the "Old lady swallowed a fly" poem
  implicit none

  character(len=52), dimension(0:8,2) :: line
  integer :: i,j
  !Define Lines of Poem
  line(1,1) = 'fly'
  line(2,1) = 'spider'
  line(3,1) = 'bird'
  line(4,1) = 'cat'
  line(5,1) = 'dog'
  line(6,1) = 'goat'
  line(7,1) = 'cow'
  line(8,1) = 'horse'
  line(0,2) = "Perhaps she'll die."
  line(1,2) = "I don't know why she swallowed that fly."
  line(2,2) = "That wiggled and jiggled and tickled inside her."
  line(3,2) = "How absurd to swallow a bird."
  line(4,2) = "Imagine that. She swallowed a cat."
  line(5,2) = "What a hog to swallow a dog."
  line(6,2) = "She just opened her throat and swallowed that goat."
  line(7,2) = "I don't know how she swallowed that cow."
  line(8,2) = "She's dead of course."

  !List each verse
  verses:do i = 1,8
     write(*,*) 'There was an old lady who swallowed a '//trim(line(i,1))//"."
     write(*,*) trim(line(i,2))
     !List things swallowed
     swallowed:do j = i,2,-1
        write(*,*) "She swallowed the "//trim(line(j,1))//" to catch the "//trim(line(j-1,1))//","
     end do swallowed
     write(*,*)  trim(line(0,2))
     write(*,*)
  end do verses

end program fly

```



## Free Pascal


```Free Pascal
(* A mixture of the lyrics found in Wikipedia and the Ada version
   in Rosetta Code. It formats the lyrics like the Wikipedia one  *)
program OldLady;

uses
  SysUtils;

const
  MaxLadies = 9; //Number of iterations and array items
  Animals: array[1..MaxLadies + 1] of shortstring = (  //Add one for the song's end
    'fly',
    'spider',
    'bird',
    'cat',
    'dog',
    'pig',
    'goat',
    'cow',
    'donkey',
    'horse'
    );

  Verse1 = 'There was an old lady who swallowed a %s;';
  //Verse 2 variations
  Verse2Var1 = '   %s'; //Doubles as a spacing formatter
  Verse2Var2 = '   %s to swallow a %s;';
  Verse2Var3 = '   %s and swallowed a %s';
  Verse2Var4 = '   %s she swallowed a %s';
  Verse3 = '   She swallowed the %s to catch the %s;';
  VerseEnd = 'I don''t know why she swallowed a fly - perhaps she''ll die!';
  SongEnd = '   ...She''s dead of course!';

  SwallowResult: array[1..MaxLadies] of shortstring = (
    VerseEnd,
    'That wiggled and jiggled and tickled inside her!',
    'Quite absurd',
    'Fancy that',
    'What a hog,',
    'Her mouth was so big',
    'She just opened her throat',
    'I don''t know how',
    'It was rather wonky'
    );

  procedure PrintSong;
  var
    i, j: byte;
    SwallowStr: shortstring;

  begin
    for i := 1 to MaxLadies do
    begin
      WriteLn(Format(Verse1, [Animals[i]]));
      case i of
        1..2: SwallowStr := Verse2Var1;
        3..5: SwallowStr := Verse2Var2;
        6, 8..9: SwallowStr := Verse2Var4;
        else
          SwallowStr := Verse2Var3;
      end;
      WriteLn(Format(SwallowStr, [SwallowResult[i], Animals[i]]));
      if i >= 2 then
      begin
        for j := i downto 2 do
        begin
          WriteLn(Format(Verse3, [Animals[j], Animals[j - 1]]));
          if (j - 1 = 2) and (i >= 3) then
            WriteLn(Format(Verse2Var1, [SwallowResult[2]]));
        end;
        if j >= 2 then
          WriteLn(Format(Verse2Var1, [VerseEnd]));
      end;
    end;
    WriteLn(Format(Verse1, [Animals[MaxLadies + 1]]));
    WriteLn(SongEnd);
  end;

begin
  PrintSong;
end.

```



## Frege

Nearly identical to the Haskell.  Only the first line of the program is different.


```frege
module OldLady where

import Data.List

-- Once means the phrase is only printed in the verse about that animal.
-- Every means the phrase is printed for every verse.  It is used for "fly",
-- and could optionally be used for "spider", in the version of the song where
-- "wriggled and jiggled..." is repeated every verse.
-- Die is only used for the horse, and means the chain of animals won't be
-- included in the verse.
data AnimalAction = Once | Every | Die

animals = [("horse", Die, "She's dead, of course!"),
           ("donkey", Once, "It was rather wonky. To swallow a donkey."),
           ("cow", Once, "I don't know how. To swallow a cow."),
           ("goat", Once, "She just opened her throat. To swallow a goat."),
           ("pig", Once, "Her mouth was so big. To swallow a pig."),
           ("dog", Once, "What a hog. To swallow a dog."),
           ("cat", Once, "Fancy that. To swallow a cat."),
           ("bird", Once, "Quite absurd. To swallow a bird."),
           ("spider", Once, "That wriggled and jiggled and tickled inside her."),
           ("fly", Every, "I don't know why she swallowed the fly.")]

verse :: [(String, AnimalAction, String)] -> [String]
verse ((anim, act, phrase):restAnims) =
  let lns = ["I know an old lady who swallowed a " ++ anim ++ ".", phrase]
  in case act of Die -> lns
                 _ -> lns ++ verse' restAnims anim

verse' :: [(String, AnimalAction, String)] -> String -> [String]
verse' [] _ = ["Perhaps she'll die."]
verse' ((anim, act, phrase):restAnims) prevAnim =
  let why = "She swallowed the " ++ prevAnim ++ " to catch the " ++ anim ++ "."
      lns = case act of Every -> [why, phrase]
                        _ -> [why]
  in lns ++ verse' restAnims anim

song :: [String]
song = concatMap verse $ tail $ reverse $ tails animals

main = putStr $ unlines song
```



## Go

[https://play.golang.org/p/NwG13guusv Go Playground]

```go
package main

import "fmt"

var name, lyric, animals = 0, 1, [][]string{
    {"fly", "I don't know why she swallowed a fly. Perhaps she'll die."},
    {"spider", "That wiggled and jiggled and tickled inside her."},
    {"bird", "How absurd, to swallow a bird."},
    {"cat", "Imagine that, she swallowed a cat."},
    {"dog", "What a hog, to swallow a dog."},
    {"goat", "She just opened her throat and swallowed that goat."},
    {"cow", "I don't know how she swallowed that cow."},
    {"horse", "She's dead, of course."},
}

func main() {
    for i, animal := range animals {
        fmt.Printf("There was an old lady who swallowed a %s,\n", animal[name])

        if i > 0 {
            fmt.Println(animal[lyric])
        }

        //Swallowing the last animal signals her death, cutting the lyrics short
        if i+1 == len(animals) {
            break
        }

        for ; i > 0; i-- {
            fmt.Printf("She swallowed the %s to catch the %s,\n", animals[i][name], animals[i-1][name])
        }

        fmt.Println(animals[0][lyric] + "\n")
    }
}
```



## Haskell



```haskell
import Data.List (tails)

animals :: [String]
animals =
  [ "fly.\nI don't know why she swallowed a fly.\nPerhaps she'll die.\n"
  , "spider.\nThat wiggled and jiggled and tickled inside her."
  , "bird.\t\nHow absurd, to swallow a bird."
  , "cat.\t\nImagine that. She swallowed a cat."
  , "dog.\t\nWhat a hog to swallow a dog."
  , "goat.\t\nShe just opened her throat and swallowed a goat."
  , "cow.\nI don't know how she swallowed a cow."
  , "horse.\nShe's dead, of course."
  ]

beginnings :: [String]
beginnings = ("There was an old lady who swallowed a " ++) <$> animals

lastVerse :: [String]
lastVerse =
  reverse
    [ "She swallowed the " ++
     takeWhile (/= '.') y ++ " to catch the " ++ takeWhile (/= '\t') x
    | (x:y:_:_) <- tails animals ]

main :: IO ()
main =
  putStr $
  concatMap unlines $
  zipWith (:) beginnings $ reverse $ ([] :) (tails lastVerse)
```

```txt
There was an old lady who swallowed a fly.
I don't know why she swallowed a fly.
Perhaps she'll die.

There was an old lady who swallowed a spider.
That wiggled and jiggled and tickled inside her.
She swallowed the spider to catch the fly.
I don't know why she swallowed a fly.
Perhaps she'll die.

There was an old lady who swallowed a bird.
How absurd, to swallow a bird.
She swallowed the bird to catch the spider.
That wiggled and jiggled and tickled inside her.
She swallowed the spider to catch the fly.
I don't know why she swallowed a fly.
Perhaps she'll die.

There was an old lady who swallowed a cat.
Imagine that. She swallowed a cat.
She swallowed the cat to catch the bird.
She swallowed the bird to catch the spider.
That wiggled and jiggled and tickled inside her.
She swallowed the spider to catch the fly.
I don't know why she swallowed a fly.
Perhaps she'll die.

There was an old lady who swallowed a dog.
What a hog to swallow a dog.
She swallowed the dog to catch the cat.
She swallowed the cat to catch the bird.
She swallowed the bird to catch the spider.
That wiggled and jiggled and tickled inside her.
She swallowed the spider to catch the fly.
I don't know why she swallowed a fly.
Perhaps she'll die.

There was an old lady who swallowed a goat.
She just opened her throat and swallowed a goat.
She swallowed the goat to catch the dog.
She swallowed the dog to catch the cat.
She swallowed the cat to catch the bird.
She swallowed the bird to catch the spider.
That wiggled and jiggled and tickled inside her.
She swallowed the spider to catch the fly.
I don't know why she swallowed a fly.
Perhaps she'll die.

There was an old lady who swallowed a cow.
I don't know how she swallowed a cow.
She swallowed the cow to catch the goat.
She swallowed the goat to catch the dog.
She swallowed the dog to catch the cat.
She swallowed the cat to catch the bird.
She swallowed the bird to catch the spider.
That wiggled and jiggled and tickled inside her.
She swallowed the spider to catch the fly.
I don't know why she swallowed a fly.
Perhaps she'll die.

There was an old lady who swallowed a horse.
She's dead, of course.

```


And a variation on this theme (a little more disaggregated):


```haskell
import Prelude hiding (lookup)
import Data.Map.Strict (Map, fromList, lookup)
import Data.List (inits, intercalate)
import Data.Maybe (fromMaybe)
import Data.Bool (bool)

main :: IO ()
main = (putStrLn . unlines) (ingestion <$> meal)

meal :: [[String]]
meal = init (reverse <$> tail (inits menu)) ++ [[last menu]]

menu :: [String]
menu = fst <$> courses

courses :: [(String, String)]
courses =
  [ ("fly", "I don't know why she swallowed a fly - perhaps she'll die")
  , ("spider", "It wiggled and jiggled and tickled inside her")
  , ("bird", "How absurd, to swallow a bird")
  , ("cat", "Imagine that. She swallowed a cat")
  , ("dog", "What a hog to swallow a dog")
  , ("goat", "She just opened her throat and swallowed a goat")
  , ("cow", "I don't know how she swallowed a cow")
  , ("horse", "She died, of course")
  ]

ingestion :: [String] -> String
ingestion [] = []
ingestion dishes =
  let appetiser = head dishes
      story = motivation dishes
  in concat
       [ "\nThere was an old lady who swallowed a "
       , appetiser
       , ";\n"
       , reputation appetiser
       , "."
       , bool ("\n\n" ++ story ++ ".") [] (null story)
       ]

motivation :: [String] -> String
motivation [] = []
motivation dishes =
  intercalate ";\n" $
  zipWith
    (\a b -> concat ["She swallowed the ", a, " to catch the ", fullName b])
    dishes
    (tail dishes)

fullName :: String -> String
fullName dish
  | "spider" == dish =
    dish ++ "\nthat " ++ unwords (tail (words (reputation dish)))
  | "fly" == dish = dish ++ ".\n" ++ reputation dish
  | otherwise = dish

reputation :: String -> String
reputation = fromMaybe [] . flip lookup comments

comments :: Map String String
comments = fromList courses
```

```txt
There was an old lady who swallowed a fly;
I don't know why she swallowed a fly - perhaps she'll die.

There was an old lady who swallowed a spider;
It wiggled and jiggled and tickled inside her.

She swallowed the spider to catch the fly.
I don't know why she swallowed a fly - perhaps she'll die.

There was an old lady who swallowed a bird;
How absurd, to swallow a bird.

She swallowed the bird to catch the spider
that wiggled and jiggled and tickled inside her;
She swallowed the spider to catch the fly.
I don't know why she swallowed a fly - perhaps she'll die.

There was an old lady who swallowed a cat;
Imagine that. She swallowed a cat.

She swallowed the cat to catch the bird;
She swallowed the bird to catch the spider
that wiggled and jiggled and tickled inside her;
She swallowed the spider to catch the fly.
I don't know why she swallowed a fly - perhaps she'll die.

There was an old lady who swallowed a dog;
What a hog to swallow a dog.

She swallowed the dog to catch the cat;
She swallowed the cat to catch the bird;
She swallowed the bird to catch the spider
that wiggled and jiggled and tickled inside her;
She swallowed the spider to catch the fly.
I don't know why she swallowed a fly - perhaps she'll die.

There was an old lady who swallowed a goat;
She just opened her throat and swallowed a goat.

She swallowed the goat to catch the dog;
She swallowed the dog to catch the cat;
She swallowed the cat to catch the bird;
She swallowed the bird to catch the spider
that wiggled and jiggled and tickled inside her;
She swallowed the spider to catch the fly.
I don't know why she swallowed a fly - perhaps she'll die.

There was an old lady who swallowed a cow;
I don't know how she swallowed a cow.

She swallowed the cow to catch the goat;
She swallowed the goat to catch the dog;
She swallowed the dog to catch the cat;
She swallowed the cat to catch the bird;
She swallowed the bird to catch the spider
that wiggled and jiggled and tickled inside her;
She swallowed the spider to catch the fly.
I don't know why she swallowed a fly - perhaps she'll die.

There was an old lady who swallowed a horse;
She died, of course.
```


=={{header|Icon}} and {{header|Unicon}}==
This version isn't as compressed as some of the others but it is very straightforward to modify.  Just add a new long and terse verse entry and amend the line marked order.  This uses a feature of Icon/Unicon that allows procedures to be called with a list datatype instead of an argument list, so we just pre-build argument lists for printf.

```Icon
procedure main()  #:  There Was An Old Lady Lyrics

verse := table()   # arglists for printf - [1] long asides and [2] terse joiners
verse["bird"]   := [["%s,\nQuite absurd, %s %s;\n",1,2,1],["%s,\n",1]]
verse["cat"]    := [["%s,\nFancy that, %s %s;\n",1,2,1],["%s,\n",1]]
verse["dog"]    := [["%s,\nWhat a hog, %s %s;\n",1,2,1],["%s,\n",1]]
verse["pig"]    := [["%s,\nHer mouth was so big, %s %s;\n",1,2,1],["%s,\n",1]]
verse["goat"]   := [["%s,\nShe just opened her throat, %s %s;\n",1,2,1],["%s,\n",1]]
verse["cow"]    := [["%s,\nI don't know how, %s %s;\n",1,2,1],["%s,\n",1]]
verse["donkey"] := [["%s,\nIt was rather wonky, %s %s;\n",1,2,1],["%s,\n",1]]

                   # just long versions of these
verse["fly"]    := [["%s,\nBut I don't know why %s %s,\nPerhaps she'll die!\n\n",1,2,1]]
verse["spider"] := [["%s,\nThat wriggled and jiggled and tickled inside her;\n",1]]
verse["horse"]  := [["%s...\nShe's dead, of course!\n",1]]

every (f := verse[k := key(verse)][1|2])[i := 1 to *f] do   # fix every printf args
   f[i] := case f[i] of { 1 : k ; 2 : "she swallowed the"; default : f[i]}

zoofilo := []
"fly,spider,bird,cat,dog,pig,goat,cow,donkey,horse," ?   # order
   while push(zoofilo,tab(find(","))) & move(1) do {
      printf("There was an old lady who swallowed a ")
      every critter := !zoofilo do {
         printf!verse[critter,(critter == (zoofilo[1] | "spider" | "fly"),1)|2]
         if critter == "horse" then stop()               # dead
         printf("She swallowed the %s to catch the ","fly" ~== critter)
         }
      }
end

link printf
```

Sample output omitted.

[http://www.cs.arizona.edu/icon/library/src/procs/printf.icn printf.icn provides printf formatting]


## J

This defines T to be the required text.

```j
T=:''
e=:3 :'T=:T,y,LF'
E=:e@,&'.'
O=:'I know an old lady who swallowed a 'E@,]
I=:e bind('I don''t know why she swallowed the fly.',LF,'Perhaps she''ll die.',LF)
I O 'fly'
O P=:'spider'
E 'That wriggled and jiggled and tickled inside her'
I E A=:'She swallowed the spider to catch the fly'
N=:4 :0
  O x
  E y,'. To swallow a ',x
  I E A=:'She swallowed the ',x,' to catch the ',P,'.',LF,A
  P=:x
)
'Bird'N'Quite absurd'
'Cat'N'Fancy that'
'Dog'N'What a hog'
'Pig'N'Her mouth was so big'
'Goat'N'She just opened her throat'
'Cow'N'I don''t know how'
'Donkey'N'It was rather wonky'
O'Horse'
e'She''s dead, of course!'
```



## Java

```java
public class OldLadySwallowedAFly {

    final static String[] data = {
        "_ha _c _e _p,/Quite absurd_f_p;_`cat,/Fancy that_fcat;_j`dog,/What a hog"
        + "_fdog;_l`pig,/Her mouth_qso big_fpig;_d_r,/She just opened her throat_f_"
        + "r;_icow,/_mhow she_ga cow;_k_o,/It_qrather wonky_f_o;_a_o_bcow,_khorse.."
        + "./She's dead, of course!/", "_a_p_b_e ", "/S_t ", " to catch the ", "fly,/Bu"
        + "t _mwhy s_t fly,/Perhaps she'll die!//_ha", "_apig_bdog,_l`", "spider,/Tha"
        + "t wr_nj_ntickled inside her;_aspider_b_c", ", to_s a ", "_sed ", "There_qan"
        + " old lady who_g", "_a_r_bpig,_d", "_acat_b_p,_", "_acow_b_r,_i", "_adog_bcat"
        + ",_j", "I don't know ", "iggled and ", "donkey", "bird", " was ", "goat", " swal"
        + "low", "he_gthe"};

    static boolean oldLady(String part, boolean s) {
        for (char c : part.toCharArray()) {
            if (s)
                s = oldLady(data[c - '_'], false);
            else if (c == '_')
                s = true;
            else
                System.out.print(c == '/' ? '\n' : c);
        }
        return s;
    }

    public static void main(String[] args) {
        oldLady(data[0], false);
    }
}
```



## Julia

```julia
using CodecZlib

b64 = b"""eNrtVE1rwzAMvedXaKdeRn7ENrb21rHCzmrs1m49K9gOJv9+cko/HBcGg0LHcpOfnq2np0QL
    2FuKgBbICDAoeoiKwEc0hqIUgLAxfV0tQJCdhQM7qh68kheswKeBt5ROYetTemYMCC3rii//
    WMS3WkhXVyuFAaLT261JuBWwu4iDbvYp1tYzHVS68VEIObwFgaDB0KizuFs38aSdqKv3TgcJ
    uPYdn2B1opwIpeKE53qPftxRd88Y6uoVbdPzWxznrQ3ZUi3DudQ/bcELbevqM32iCIrj3IIh
    W6plOJf6L6xaajZjzqW/qAsKIvITBGs9Nm3glboZzkVP5l6Y+0bHLnedD0CttIyrpEU5Kv7N
    Mz3XkPBc/TSN3yxGiqMiipHRekycK0ZwMhM8jerGC9zuZaoTho3kMKSfJjLaF8v8wLzmXMqM
    zJvGew/jnZPzclA08yAkikegDTTUMfzwDXBcwoE="""
println(String(transcode(ZlibDecompressor(), base64decode(b64))))
```



```julia
animals = [
    ("fly", "I don't know why she swallowed a fly, perhaps she'll die."),
    ("spider", "It wiggled and jiggled and tickled inside her."),
    ("bird", "How absurd, to swallow a bird."),
    ("cat", "Imagine that, she swallowed a cat."),
    ("dog", "What a hog, to swallow a dog."),
    ("goat", "She just opened her throat and swallowed a goat."),
    ("cow", "I don't know how she swallowed a cow."),
    ("horse", "She's dead, of course.")]

for (i, (animal, lyric)) in enumerate(animals)
    println("There was an old lady who swallowed a $animal.\n$lyric")

    if animal == "horse" break end

    for ((predator, _), (prey, _)) in zip(animals[i:-1:1], animals[i-1:-1:1])
        println("\tShe swallowed the $predator to catch the $prey")
    end

    if animal != "fly" println(animals[1][2]) end  # fly lyric
    println()  # new line
end
```



## Kotlin


```scala
// version 1.1.3

val animals = listOf("fly", "spider", "bird", "cat","dog", "goat", "cow", "horse")

val phrases = listOf(
    "",
    "That wriggled and jiggled and tickled inside her",
    "How absurd to swallow a bird",
    "Fancy that to swallow a cat",
    "What a hog, to swallow a dog",
    "She just opened her throat and swallowed a goat",
    "I don't know how she swallowed a cow",
    "\n  ...She's dead of course"
)

fun sing() {
    for (i in 0..7) {
       println("There was an old lady who swallowed a ${animals[i]};")
       if (i > 0) println("${phrases[i]}!")
       if (i == 7) return
       println()
       if (i > 0) {
           for (j in i downTo 1) {
               print("  She swallowed the ${animals[j]} to catch the ${animals[j - 1]}")
               println(if (j < 3) ";" else ",")
               if (j == 2) println("  ${phrases[1]}!")
           }
       }
       println("  I don't know why she swallowed a fly - Perhaps she'll die!\n")
    }
}

fun main(args: Array<String>) {
    sing()
}
```



## Liberty BASIC


```lb

'[RC] Old lady swallowed a fly
'http://rosettacode.org/wiki/Old_lady_swallowed_a_fly
'lyrics
'http://www.timmyabell.com/music/lyrics/ol/oldlady.htm

animalList$="fly spider bird cat dog goat cow horse"
reason$(1)="I don't know why she swallowed the fly,"
reason$(2)="That wriggled and jiggled and tickled inside her."
reason$(3)="How absurd to swallow a bird!"
reason$(4)="Imagine that, to swallow a cat!"
reason$(5)="My, what a hog, to swallow a dog!"
reason$(6)="Just opened her throat and swallowed a goat!"
reason$(7)="I wonder how she swallowed a cow?!"
reason$(8)="She's dead, of course!!"

i=0
while 1
    i=i+1
    animal$ = word$(animalList$, i)
    if animal$ ="" then exit while
    verse$ = "I know an old lady who "
    verse2$ = "swallowed a " +animal$
    print verse$ +verse2$+","
    print reason$(i)
    if i = 8 then end
    '--------------
    animals$=animal$+" "+animals$
    animal2$="*"
    j=1
    while 1
        j=j+1
        animal2$ = word$(animals$, j)
        if animal2$ ="" then exit while
        reason$="She swallowed the "+animal$+" to catch the "+animal2$+","
        animal$ = animal2$
        print reason$
        if animal2$ = "fly" then print reason$(1)
        if animal2$ = "spider" then print reason$(2)
    wend
    '--------------
    print "I guess she'll die."
    print
wend

end

```



## Logo


```logo
make "data [
; animal inc comment
  [fly    2 [I don't know why she swallowed that fly]]
  [spider 2 [That wriggled and jiggled and tickled inside her]]
  [bird   1 [Quite absurd, to swallow a bird]]
  [cat    1 [How about that, to swallow a cat]]
  [dog    1 [What a hog, to swallow a dog]]
  [pig    1 [Her mouth was so big to swallow a pig]]
  [goat   1 [She just opened her throat to swallow a goat.]]
  [cow    1 [I don't know how she swallowed a cow.]]
  [donkey 1 [It was rather wonky to swallow a donkey]]
  [horse  0 [She's dead, of course!]]
]

foreach :data [
  local "i make "i #
  (local "animal "include "comment)
  (foreach [animal include comment] ? "make)
  print se [There was an old lady who swallowed a] :animal
  print :comment
  if greater? :include 0 [
    if greater? :i 1 [
      repeat difference :i 1 [
        local "j make "j difference :i repcount
        print (se [She swallowed the] (first item sum 1 :j :data)
                  [to catch the] (first item :j :data))
        if greater? item 2 item :j :data 1 [print item 3 item :j :data]
      ]
    ]
    print [Perhaps she'll die]
    print "
  ]
]

bye
```



## Lua


```lua
animals = {"fly", "spider", "bird", "cat","dog", "goat", "cow", "horse"}
phrases = {
    "",
    "That wriggled and jiggled and tickled inside her",
    "How absurd to swallow a bird",
    "Fancy that to swallow a cat",
    "What a hog, to swallow a dog",
    "She just opened her throat and swallowed a goat",
    "I don't know how she swallowed a cow",
    "  ...She's dead of course"
}

for i=0,7 do
    io.write(string.format("There was an old lady who swallowed a %s\n", animals[i+1]))
    if i>0 then io.write(phrases[i+1]) end
    if i==7 then break end
    if i>0 then
        io.write("\n")
        for j=i,1,-1 do
            io.write(string.format("She swallowed the %s to catch the %s", animals[j+1], animals[j]))
            -- if j<4 then p='.' else p=',' end
            -- io.write(string.format("%s\n", p))
            io.write("\n")
            if j==2 then
                io.write(string.format("%s!\n", phrases[2]))
            end
        end
    end
    io.write("I don't know why she swallowed a fly - Perhaps she'll die!\n\n")
end
```



## Maple


```maple
swallowed := ["fly", "spider", "bird", "cat", "dog", "cow", "horse"]:
phrases := ["I don't know why she swallowed a fly, perhaps she'll die!",
		  "That wriggled and wiggled and tiggled inside her.",
		  "How absurd to swallow a bird.",
		  "Fancy that to swallow a cat!",
		  "What a hog, to swallow a dog.",
		  "I don't know how she swallowed a cow.",
		  "She's dead, of course."]:
for i to numelems(swallowed) do
	printf("There was an old lady who swallowed a %s.\n%s\n", swallowed[i], phrases[i]);
	if i > 1 and i < 7 then
		for j from i by -1 to 2 do
			printf("\tShe swallowed the %s to catch the %s.\n", swallowed[j], swallowed[j-1]);
		end do;
		printf("%s\n\n", phrases[1]);
	elif i = 1 then
		printf("\n");
	end if;
end do;
```

```txt

There was an old lady who swallowed a fly.
I don't know why she swallowed a fly, perhaps she'll die!

There was an old lady who swallowed a spider.
That wriggled and wiggled and tiggled inside her.
	She swallowed the spider to catch the fly.
I don't know why she swallowed a fly, perhaps she'll die!

There was an old lady who swallowed a bird.
How absurd to swallow a bird.
	She swallowed the bird to catch the spider.
	She swallowed the spider to catch the fly.
I don't know why she swallowed a fly, perhaps she'll die!

There was an old lady who swallowed a cat.
Fancy that to swallow a cat!
	She swallowed the cat to catch the bird.
	She swallowed the bird to catch the spider.
	She swallowed the spider to catch the fly.
I don't know why she swallowed a fly, perhaps she'll die!

There was an old lady who swallowed a dog.
What a hog, to swallow a dog.
	She swallowed the dog to catch the cat.
	She swallowed the cat to catch the bird.
	She swallowed the bird to catch the spider.
	She swallowed the spider to catch the fly.
I don't know why she swallowed a fly, perhaps she'll die!

There was an old lady who swallowed a cow.
I don't know how she swallowed a cow.
	She swallowed the cow to catch the dog.
	She swallowed the dog to catch the cat.
	She swallowed the cat to catch the bird.
	She swallowed the bird to catch the spider.
	She swallowed the spider to catch the fly.
I don't know why she swallowed a fly, perhaps she'll die!

There was an old lady who swallowed a horse.
She's dead, of course.

```



## Mathematica


```Mathematica
animals = {"fly", "spider", "bird", "cat", "dog", "goat", "cow",
   "horse"};
notes = {"", "That wiggled and jiggled and tickled inside her.\n",
   "How absurd, to swallow a bird.\n",
   "Imagine that. She swallowed a cat.\n",
   "What a hog to swallow a dog.\n",
   "She just opened her throat and swallowed that goat.\n",
   "I don't know how she swallowed that cow.\n",
   "She's dead, of course.",
   "I don't know why she swallowed that fly.\nPerhaps she'll die.\n\n\
"};
Print[StringJoin @@ ("There was an old lady who swallowed a " <>
       animals[[#]] <> ".\n" <> notes[[#]] <>
       If[# == 8, "",
        StringJoin @@ ("She swallowed the " <> animals[[#]] <>
              " to catch the " <> animals[[# - 1]] <> ".\n" & /@
            Range[#, 2, -1]) <> notes[[9]]] & /@ Range[8])];
```

```txt
There was an old lady who swallowed a fly.
I don't know why she swallowed that fly.
Perhaps she'll die.

There was an old lady who swallowed a spider.
That wiggled and jiggled and tickled inside her.
She swallowed the spider to catch the fly.
I don't know why she swallowed that fly.
Perhaps she'll die.

There was an old lady who swallowed a bird.
How absurd, to swallow a bird.
She swallowed the bird to catch the spider.
She swallowed the spider to catch the fly.
I don't know why she swallowed that fly.
Perhaps she'll die.

There was an old lady who swallowed a cat.
Imagine that. She swallowed a cat.
She swallowed the cat to catch the bird.
She swallowed the bird to catch the spider.
She swallowed the spider to catch the fly.
I don't know why she swallowed that fly.
Perhaps she'll die.

There was an old lady who swallowed a dog.
What a hog to swallow a dog.
She swallowed the dog to catch the cat.
She swallowed the cat to catch the bird.
She swallowed the bird to catch the spider.
She swallowed the spider to catch the fly.
I don't know why she swallowed that fly.
Perhaps she'll die.

There was an old lady who swallowed a goat.
She just opened her throat and swallowed that goat.
She swallowed the goat to catch the dog.
She swallowed the dog to catch the cat.
She swallowed the cat to catch the bird.
She swallowed the bird to catch the spider.
She swallowed the spider to catch the fly.
I don't know why she swallowed that fly.
Perhaps she'll die.

There was an old lady who swallowed a cow.
I don't know how she swallowed that cow.
She swallowed the cow to catch the goat.
She swallowed the goat to catch the dog.
She swallowed the dog to catch the cat.
She swallowed the cat to catch the bird.
She swallowed the bird to catch the spider.
She swallowed the spider to catch the fly.
I don't know why she swallowed that fly.
Perhaps she'll die.

There was an old lady who swallowed a horse.
She's dead, of course.
```



## Mercury


```Mercury
:- module oldlady.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.
:- implementation.
:- import_module list, string.

:- type animal
    --->    horse
    ;       donkey
    ;       cow
    ;       goat
    ;       pig
    ;       dog
    ;       cat
    ;       bird
    ;       spider
    ;       fly.

:- func verse(animal) = string.
verse(horse)    = "She's dead, of course!".
verse(donkey)   = "It was rather wonky. To swallow a donkey.".
verse(cow)      = "I don't know how. To swallow a cow.".
verse(goat)     = "She just opened her throat. To swallow a goat.".
verse(pig)      = "Her mouth was so big. To swallow a pig.".
verse(dog)      = "What a hog. To swallow a dog.".
verse(cat)      = "Fancy that. To swallow a cat.".
verse(bird)     = "Quite absurd. To swallow a bird.".
verse(spider)   = "That wriggled and jiggled and tickled inside her.".
verse(fly)      = "I don't know why she swallowed the fly.".

:- pred tocatch(animal, animal).
:- mode tocatch(in, out) is semidet.
:- mode tocatch(out, in) is semidet.
tocatch(horse, donkey).
tocatch(donkey, cow).
tocatch(cow, goat).
tocatch(goat, pig).
tocatch(pig, dog).
tocatch(dog, cat).
tocatch(cat, bird).
tocatch(bird, spider).
tocatch(spider, fly).

:- pred swallow(animal::in, io::di, io::uo) is det.
swallow(A, !IO) :-
    ( if tocatch(A, B) then
        io.format("She swallowed the %s to catch the %s.\n",
            [s(string(A)), s(string(B))], !IO),
        swallow(B, !IO)
    else
        io.format("%s\nPerhaps she'll die.\n\n", [s(verse(fly))], !IO)
    ).

:- pred swallowed(animal::in, io::di, io::uo) is det.
swallowed(A, !IO) :-
    io.format("I know an old lady who swallowed a %s.\n", [s(string(A))], !IO),
    ( if A = horse then
        io.write_string("She's dead, of course!\n", !IO)
    else if A = fly, tocatch(B, A) then
        swallow(A, !IO),
        swallowed(B, !IO)
    else if tocatch(B, A) then
        io.write_string(verse(A), !IO),
        io.nl(!IO),
        swallow(A, !IO),
        swallowed(B, !IO)
    else
        true
    ).

main(!IO) :-
    swallowed(fly, !IO).
```


=={{header|Modula-2}}==

```modula2
MODULE OldLady;
FROM FormatString IMPORT FormatString;
FROM Terminal IMPORT WriteString,WriteLn,ReadChar;

TYPE
    AA = ARRAY[0..7] OF ARRAY[0..7] OF CHAR;
    VA = ARRAY[0..7] OF ARRAY[0..63] OF CHAR;
VAR
    buf : ARRAY[0..127] OF CHAR;
    animals : AA;
    verses : VA;
    i,j : INTEGER;
BEGIN
    FormatString("I don't know why she swallowed that fly.\nPerhaps she'll die\n", buf);

    animals := AA{"fly", "spider", "bird", "cat", "dog", "goat", "cow", "horse"};
    verses := VA{
        "I don't know why she swallowed that fly.",
        "That wiggled and jiggled and tickled inside her",
        "How absurd, to swallow a bird",
        "Imagine that. She swallowed a cat",
        "What a hog to swallow a dog",
        "She just opened her throat and swallowed that goat",
        "I don't know how she swallowed that cow",
        "She's dead of course"
    };

    FOR i:=0 TO 7 DO
        FormatString("There was an old lady who swallowed a %s\n%s\n", buf, animals[i], verses[i]);
        WriteString(buf);
        IF i=0 THEN
            WriteString("Perhaps she'll die");
            WriteLn;
            WriteLn;
        END;
        j := i;
        WHILE (j>0) AND (i<7) DO
            FormatString("She swallowed the %s to catch the %s\n", buf, animals[j], animals[j-1]);
            WriteString(buf);
            IF j=1 THEN
                WriteString(verses[0]);
                WriteLn;
                WriteString("Perhaps she'll die");
                WriteLn;
                WriteLn
            END;
            DEC(j)
        END;
    END;

    ReadChar
END OldLady.
```



## Nim

```nim
import zlib, base64

const b64 = """
eNrtVE1rwzAMvedXaKdeRn7ENrb21rHCzmrs1m49K9gOJv9+cko/HBcGg0LHcpOfnq2np0QL
2FuKgBbICDAoeoiKwEc0hqIUgLAxfV0tQJCdhQM7qh68kheswKeBt5ROYetTemYMCC3rii//
WMS3WkhXVyuFAaLT261JuBWwu4iDbvYp1tYzHVS68VEIObwFgaDB0KizuFs38aSdqKv3TgcJ
uPYdn2B1opwIpeKE53qPftxRd88Y6uoVbdPzWxznrQ3ZUi3DudQ/bcELbevqM32iCIrj3IIh
W6plOJf6L6xaajZjzqW/qAsKIvITBGs9Nm3glboZzkVP5l6Y+0bHLnedD0CttIyrpEU5Kv7N
Mz3XkPBc/TSN3yxGiqMiipHRekycK0ZwMhM8jerGC9zuZaoTho3kMKSfJjLaF8v8wLzmXMqM
zJvGew/jnZPzclA08yAkikegDTTUMfzwDXBcwoE="""

proc uncompress*(source: string, destLen: var int): string =
  result = newString(destLen)
  discard uncompress(result.cstring, addr destLen, source.cstring, source.len)

var length = 10_000
echo b64.decode.uncompress length
```



## OCaml


### Using a dictionary

Only one minimalist line of code (the last line):

```ocaml
let d = [|
  "I know an old lady who swallowed a "; "fly"; ".\n";
  "I don't know why she swallowed the fly.\nPerhaps she'll die.\n\n";
  "spider"; "That wriggled and jiggled and tickled inside her";
  "She swallowed the "; " to catch the "; "Bird"; "Quite absurd";
  ". To swallow a "; "Cat"; "Fancy that"; "Dog"; "What a hog"; "Pig";
  "Her mouth was so big"; "Goat"; "She just opened her throat"; "Cow";
  "I don't know how"; "Donkey"; "It was rather wonky";
  "I know an old lady who swallowed a Horse.\nShe's dead, of course!\n";
|]

let s0 = [6;4;7;1;2;3]
let s1 = [6;8;7;4;2]   @ s0
let s2 = [6;11;7;8;2]  @ s1
let s3 = [6;13;7;11;2] @ s2
let s4 = [6;15;7;13;2] @ s3
let s5 = [6;17;7;15;2] @ s4
let s6 = [6;19;7;17;2] @ s5
let s7 = [6;21;7;19;2] @ s6

let s =
  [0;1;2;3;0;4;2;5;2] @ s0 @
  [0;8;2;9;10;8;2]    @ s1 @
  [0;11;2;12;10;11;2] @ s2 @
  [0;13;2;14;10;13;2] @ s3 @
  [0;15;2;16;10;15;2] @ s4 @
  [0;17;2;18;10;17;2] @ s5 @
  [0;19;2;20;10;19;2] @ s6 @
  [0;21;2;22;10;21;2] @ s7 @
  [23] ;;

List.iter (fun i -> print_string d.(i)) s
```


### Using dictionary based decompression

Here we use the function <code ocaml>String.fold_left</code> [http://ocaml-extlib.googlecode.com/svn/doc/apiref/ExtString.String.html#VALfold_left 1] which is not available in the standard library but in the [http://code.google.com/p/ocaml-extlib/ extlib]:

```ocaml
let dict = [|
  "_ha _c _e _p,\nQuite absurd_f_p;_`cat,\nFancy that_fcat;_j`dog,\nWhat a hog_\
  fdog;_l`pig,\nHer mouth_qso big_fpig;_d_r,\nShe just opened her throat_f_r;_i\
  cow,\n_mhow she_ga cow;_k_o,\nIt_qrather wonky_f_o;_a_o_bcow,_khorse...\nShe'\
  s dead, of course!\n";"_a_p_b_e ";"\nS_t ";" to catch the ";"fly,\nBut _mwhy \
  s_t fly,\nPerhaps she'll die!\n\n_ha";"_apig_bdog,_l`";"spider,\nThat wr_nj_n\
  tickled inside her;_aspider_b_c";", to_s a ";"_sed ";"There_qan old lady who_\
  g";"_a_r_bpig,_d";"_acat_b_p,_";"_acow_b_r,_i";"_adog_bcat,_j";"I don't know \
  ";"iggled and ";"donkey";"bird";" was ";"goat";" swallow";"he_gthe" |]

let rec old_lady part s =
  ExtString.String.fold_left (fun s c ->
    if s then old_lady dict.(Char.code c - 95) false
    else if c = '_' then true
    else (print_char c; s)
  ) s part

let _ =
  old_lady dict.(0) false
```


### Using Logic


```ocaml
let an =
  [| "fly"; "spider"; "bird"; "cat"; "dog"; "pig"; "goat"; "cow"; "donkey" |]

let cm =
  [| "Quite absurd";
     "Fancy that";
     "What a hog";
     "Her mouth was so big";
     "She just opened her throat";
     "I don't know how";
     "It was rather wonky"; |]

let p = Printf.printf

let h n =
  for i = n downto 1 do
    if i = 1 then
      p "That wriggled and jiggled and tickled inside her;\n";
    p "She swallowed the %s to catch the %s,\n" an.(i) an.(i-1)
  done

let g n =
  if n >= 2 then p "%s, to swallow a %s;\n" cm.(n-2) an.(n)

let f n =
  p "There was an old lady who swallowed a %s,\n" an.(n); g n; h n;
  p "But I don't know why she swallowed the fly,\nPerhaps she'll die!\n\n"

let () =
  for i = 0 to 8 do f i done;
  p "There was an old lady who swallowed a horse...\n\
     She's dead, of course!"
```



## Perl

Using string subst:
```perl
my @animals = (
	"fly",
	"spider/That wriggled and jiggled and tickled inside her.\n",
	"bird//Quite absurd!",
	"cat//Fancy that!",
	"dog//What a hog!",
	"pig//Her mouth was so big!",
	"goat//She just opened her throat!",
	"cow//I don't know how;",
	"donkey//It was rather wonkey!",
	"horse:",
);

my $s = "swallow";
my $e = $s."ed";
my $t = "There was an old lady who $e a ";
my $_ = $t."But I don't know why she $e the fly;\nPerhaps she'll die!\n\n";

my ($a, $b, $c, $d);
while (my $x = shift @animals) {
	s/$c//;
	($a, $b, $c) = split('/', $x);
	$d = " the $a";

	$c =~ s/;/ she $e$d;\n/;
	$c =~ s/!/, to $s$d;\n/;

	s/$t/"$t$a,\n$c".(($b||$c) && "${b}She $e$d to catch the ")/e;

	s/:.*/--\nShe's dead, of course!\n/s;
	print;
}
```

Using compression:
(Assumes a Unix-like OS and the availability of the uudecode and bunzip2 utilities).

```perl
open OUT, "| uudecode | bunzip2" and
print OUT <DATA> and
close OUT;

__DATA__
begin-base64 644 -
QlpoOTFBWSZTWUSbX/0AAZRfgAAQYIUACBFgbIA//96gQAK9oAAAxhMTQYIx
DIwmDEoAAAAAAxhMTQYIxDIwmBSkiNIaJtCCAzJPU1G4ueVmGZMsMzBz0N5v
hr4j29SRSSCZgyv8BDAAdOE3oFIFIhMAQMtm2Zy/MbRs9U1pgzCzGcaGnTYN
u5NJ+/D4TfkcZZ39PmNJuN8rxjMrJTfvr8rFkxmTDMGFjDLBleGh3L8zlhuO
9vcq6rom3TonOONxyJ1TlG3dz2Tu3xZNtzTLgZu21y1r0dOW/HLntrgdi9ow
hlHTsnRVbJ98DxjYs/K87Q1rJjWazCO7kHbIXUj9DS7dSMHVNSmhwrjHMc8D
INk476V5jJDmnOPXZM38aeAd+DUp/39ccxmDEf3H7u30Rk6zDLGZkPYNq9CP
Pzj39xsVe+KeupMjKsjONsG6dk1bajByHYPOMHxneP2Og3q+dR9ryGk19o0n
onYPUfEfhVc1V+kcbJwmQ/nRwn3Hp6pP4TqvTO/2TfNJkvrrbt8+a9N92oy2
FeXUOI8486Wvor1zajqPDfpwnrn2jOzvo8hkOPrpVajlwnjqPfIry5c0TbKL
559fx8xqpsquRaFYV9I9fT6p7RrI/Gv/F3JFOFCQRJtf/Q==
====
```


## Perl 6


```perl6
my @victims =
    fly    => "  I don't know why S—",
    spider => "  That wriggled and jiggled and tickled inside her.",
    bird   => "  How absurd, T!",
    cat    => "  Fancy that, S!",
    dog    => "  What a hog, T!",
    goat   => "  She just opened her throat, and in walked the goat!",
    cow    => "  I don't know how S!",
    horse  => "  She's dead, of course...";

my @history = "I guess she'll die...\n";

for (flat @victims».kv) -> $victim, $_ is copy {
    say "There was an old lady who swallowed a $victim...";

    s/ «S» /she swallowed the $victim/;
    s/ «T» /to swallow a $victim!/;
    .say;
    last when /dead/;

    @history[0] ~~ s/^X/She swallowed the $victim/;
    .say for @history;
    @history.unshift($_) if @history < 5;
    @history.unshift("X to catch the $victim,");
}
```

And that's how I larned it!


## Phix

```Phix
sequence lines = {"Perhaps she'll die!\n"}, animals = {}

procedure swallow(string animal, second_line, integer permanent_second_line=TRUE)
    printf(1,"There was an old lady who swallowed a %s,\n%s\n",{animal,second_line})
    if length(animals)!=0 then
        lines = prepend(lines,sprintf("She swallowed the %s to catch the %s,\n",{animal,animals[$]}))
    end if
    printf(1,"%s\n",{join(lines,"")})
    if permanent_second_line then
        lines = prepend(lines,second_line&"\n")
    end if
    animals = append(animals,animal)
end procedure

procedure swallow_all(sequence all)
    for i=1 to length(all) do
        string {animal,line2} = all[i]
        swallow(animal, sprintf("%s, %s a %s;",{line2,iff(animal="cow"?"she swallowed":"to swallow"),animal}), FALSE);
    end for
end procedure

swallow("fly", "But I don't know why she swallowed the fly,");
swallow("spider",  "That wriggled and jiggled and tickled inside her;");
swallow_all({{"bird", "Quite absurd"},{"cat", "Fancy that"},{"dog", "What a hog"},
             {"pig", "Her mouth was so big"},{"goat","She just opened her throat"},
             {"cow", "I don't know how"},{"donkey", "It was rather wonky"}})
printf(1, "There was an old lady who swallowed a horse ...\nShe's dead, of course!")
```



## PHP


```php
<?php

$swallowed = array(
  array('swallowed' => 'fly.',
        'reason' => "I don't know why she swallowed the fly."),
  array('swallowed' => 'spider,',
        'aside' => "which wiggled and jiggled and tickled inside her.",
        'reason' => "She swallowed the spider to catch the fly"),
  array('swallowed' => 'bird.',
        'aside' => "How absurd! To swallow a bird!",
        'reason' => "She swallowed the bird to catch the spider,"),
  array('swallowed' => 'cat.',
        'aside' => "Imagine that! To swallow a cat!",
        'reason' => "She swallowed the cat to catch the bird."),
  array('swallowed' => 'dog.',
        'aside' => "What a hog! To swallow a dog!",
        'reason' => "She swallowed the dog to catch the cat."),
  array('swallowed' => 'horse',
        'aside' => "She's dead, of course. She swallowed a horse!",
        'reason' => "She swallowed the horse to catch the dog."));

foreach($swallowed as $creature)
{
  print "I knew an old lady who swallowed a " . $creature['swallowed'] . "\n";
  if(array_key_exists('aside', $creature))
    print $creature['aside'] . "\n";

  $reversed = array_reverse($swallowed);
  $history = array_slice($reversed, array_search($creature, $reversed));

  foreach($history as $note)
  {
    print $note['reason'] . "\n";
  }

  if($swallowed[count($swallowed) - 1] == $creature)
    print "But she sure died!\n";
  else
    print "Perhaps she'll die." . "\n\n";
}
```



## PicoLisp

```PicoLisp
(de *Dict
   `(chop
      "_ha _c _e _p,/Quite absurd_f_p;_`cat,/Fancy that_fcat;_j`dog,\
         /What a hog_fdog;_l`pig,/Her mouth_qso big_fpig;_d_r,/She just \
         opened her throat_f_r;_icow,/_mhow she_ga cow;_k_o,/It_qrather \
         wonky_f_o;_a_o_bcow,_khorse.../She's dead, of course!/" )
   `(chop "_a_p_b_e ")
   `(chop "/S_t ")
   `(chop " to catch the ")
   `(chop "fly,/But _mwhy s_t fly,/Perhaps she'll die!//_ha")
   `(chop "_apig_bdog,_l`")
   `(chop "spider,/That wr_nj_ntickled inside her;_aspider_b_c")
   `(chop ", to_s a ")
   `(chop "_sed ")
   `(chop "There_qan old lady who_g")
   `(chop "_a_r_bpig,_d")
   `(chop "_acat_b_p,_")
   `(chop "_acow_b_r,_i")
   `(chop "_adog_bcat,_j")
   `(chop "I don't know ")
   `(chop "iggled and ")
   `(chop "donkey")
   `(chop "bird")
   `(chop " was ")
   `(chop "goat")
   `(chop " swallow")
   `(chop "he_gthe") )

(de oldLady (Lst Flg)
   (loop
      (let C (pop 'Lst)
         (cond
            (Flg
               (setq Flg
                  (oldLady (get *Dict (- (char C) 94))) ) )
            ((= "_" C) (on Flg))
            ((= "/" C) (prinl))
            (T (prin C)) ) )
      (NIL Lst) )
   Flg )

(oldLady (car *Dict))
```



## PowerShell


```powershell

$lines = @(
    'fly/'
    'spider/That wiggled and jiggled and tickled inside her,'
    'bird/How absurd, to swallow a bird,'
    'cat/Imagine that. She swallowed a cat,'
    'dog/What a hog to swallow a dog,'
    'goat/She just opened her throat and swallowed that goat,'
    'cow/I don''t know how she swallowed that cow,'
    'horse/She''s dead of course!'
)

$eatenThings = @()

for($i=0; $i -lt $lines.Count; $i++)
{
    $creature, $comment = $lines[$i].Split("/")
    $eatenThings += $creature

    "I know an old lady who swallowed a $creature,"

    if ($comment) {$comment}
    if ($i -eq ($lines.Count - 1)) {continue}

    for($j=$i; $j -ge 1; $j--)
    {
        "She swallowed the {0} to catch the {1}," -f $eatenThings[$j, ($j-1)]
    }

	"I don't know why she swallowed the fly."
	"Perhaps she'll die."
	""
}
```



## Python


```python
import zlib, base64

b64 = b'''
eNrtVE1rwzAMvedXaKdeRn7ENrb21rHCzmrs1m49K9gOJv9+cko/HBcGg0LHcpOfnq2np0QL
2FuKgBbICDAoeoiKwEc0hqIUgLAxfV0tQJCdhQM7qh68kheswKeBt5ROYetTemYMCC3rii//
WMS3WkhXVyuFAaLT261JuBWwu4iDbvYp1tYzHVS68VEIObwFgaDB0KizuFs38aSdqKv3TgcJ
uPYdn2B1opwIpeKE53qPftxRd88Y6uoVbdPzWxznrQ3ZUi3DudQ/bcELbevqM32iCIrj3IIh
W6plOJf6L6xaajZjzqW/qAsKIvITBGs9Nm3glboZzkVP5l6Y+0bHLnedD0CttIyrpEU5Kv7N
Mz3XkPBc/TSN3yxGiqMiipHRekycK0ZwMhM8jerGC9zuZaoTho3kMKSfJjLaF8v8wLzmXMqM
zJvGew/jnZPzclA08yAkikegDTTUMfzwDXBcwoE='''
print(zlib.decompress(base64.b64decode(b64)).decode("utf-8", "strict"))
```



```python
animals = [
        ("fly", "I don't know why she swallowed a fly, perhaps she'll die."),
        ("spider", "It wiggled and jiggled and tickled inside her."),
        ("bird", "How absurd, to swallow a bird."),
        ("cat", "Imagine that, she swallowed a cat."),
        ("dog", "What a hog, to swallow a dog."),
        ("goat", "She just opened her throat and swallowed a goat."),
        ("cow", "I don't know how she swallowed a cow."),
        ("horse", "She's dead, of course.")]

for i, (animal, lyric) in enumerate(animals):
    print "There was an old lady who swallowed a {}.\n{}".format(animal, lyric)

    if animal == "horse": break

    for (predator, _), (prey, _) in zip(animals[i:0:-1], animals[i-1::-1]):
        print "\tShe swallowed the {} to catch the {}".format(predator, prey)

    if animal != "fly": print animals[0][1]  # fly lyric
    print  # new line
```



## Racket


```racket

#lang at-exp racket

(define (line . xs) (for-each display xs) (newline))

(let loop ([animals
            '([fly #f]
              [spider "That wriggled and wiggled and tiggled inside her"]
              [bird   "How absurd to swallow a bird"]
              [cat    "Fancy that to swallow a cat"]
              [dog    "What a hog, to swallow a dog"]
              [cow    "I don't know how she swallowed a cow"]
              [horse  "She's dead, of course"])]
           [seen '()])
  (when (pair? animals)
    (match animals
      [(list (list animal desc) more ...)
       @line{There was an old lady that swallowed a @animal,}
       (when desc @line{@|desc|.})
       (when (pair? more)
         (for ([this (cons animal seen)] [that seen])
           @line{She swallowed the @this to catch the @that,})
         @line{I don't know why she swallowed a fly - perhaps she'll die!}
         @line{}
         (loop more (cons animal seen)))])))

```

Alternative solution:

```racket

#lang s-exp framework/private/decode
7VK7TsQwEOz9FcM1AcncZyCoQaLey/piCyuObJ+s/D3rXHj4lIIKgaCItJkZ7cyOfM0uTZ5m
7J6siQaFEmhE8AxPPCNbykiFvA/FMAhHP2v1AA5jl/EyhoJiZyRrLlW4xWSipSlVtvMe7MyV
Ul/0SZNjE7XIhSjRDYOv+Mgon+a8zm5MIoes3qvHJkuuf8su5ICecm8X7LvuOLjIWt3Lfjqk
U+SaYuVXditxxdu8b338rOvEUas7GvuVa44Tcuu2/qz7CHuu6PeWwGHQ6rmiBCtz24KwWy0I
3KZdqvwDbfWhXPhY+S59RLX5dkTapF66/2/3fYMNMZklVJfAhlgjHKW2k8B7tbtRrw==

```



## REXX


```rexx
/*REXX program displays  song lyrics  for:   "I Know an Old Lady who Swallowed a Fly".  */
sw= 79                                           /*the width of the terminal screen, -1.*/
@.=;            @.1 = "I don't know why she swallowed a fly,"
                @.2 = "That wriggled and jiggled and tickled inside her.";    @.2.0=.
                @.3 = "How absurd to swallow a bird!"
                @.4 = "Imagine that, to swallow a cat!"
                @.5 = "My, what a hog, to swallow a dog!"
                @.6 = "Just opened her throat and swallowed a goat!"
                @.7 = "I wonder how she swallowed a cow?!"
                @.8 = "She's dead, of course!!"
$ = 'fly spider bird cat dog goat cow horse'
#= words($)                                      /*#:  number of animals to be swallowed*/

  do j=1  for #;        say
  say center('I know an old lady who swallowed a'     word($, j)",",  sw)
  if j\==1  then  say center(@.j, sw)
  if j ==#  then leave                           /*Is this the last verse?   We're done.*/
                      do k=j  to 2  by -1;      km= k-1;              ??= word($, km)','
                      say center('She swallowed the'   word($,k)  "to catch the"   ??, sw)
                      if @.km.0\==''  then say center(@.km, sw)
                      end   /*k*/                /* [↑]  display the lyrics of the song.*/
  say center(@.1, sw)
  say center("I guess she'll die.", sw)
  end   /*j*/                                    /*stick a fork in it,  we're all done. */
```

```txt

                    I know an old lady who swallowed a fly,
                     I don't know why she swallowed a fly,
                              I guess she'll die.

                  I know an old lady who swallowed a spider,
               That wriggled and jiggled and tickled inside her.
                  She swallowed the spider to catch the fly,
                     I don't know why she swallowed a fly,
                              I guess she'll die.

                   I know an old lady who swallowed a bird,
                         How absurd to swallow a bird!
                  She swallowed the bird to catch the spider,
               That wriggled and jiggled and tickled inside her.
                  She swallowed the spider to catch the fly,
                     I don't know why she swallowed a fly,
                              I guess she'll die.

                    I know an old lady who swallowed a cat,
                        Imagine that, to swallow a cat!
                   She swallowed the cat to catch the bird,
                  She swallowed the bird to catch the spider,
               That wriggled and jiggled and tickled inside her.
                  She swallowed the spider to catch the fly,
                     I don't know why she swallowed a fly,
                              I guess she'll die.

                    I know an old lady who swallowed a dog,
                       My, what a hog, to swallow a dog!
                    She swallowed the dog to catch the cat,
                   She swallowed the cat to catch the bird,
                  She swallowed the bird to catch the spider,
               That wriggled and jiggled and tickled inside her.
                  She swallowed the spider to catch the fly,
                     I don't know why she swallowed a fly,
                              I guess she'll die.

                   I know an old lady who swallowed a goat,
                 Just opened her throat and swallowed a goat!
                   She swallowed the goat to catch the dog,
                    She swallowed the dog to catch the cat,
                   She swallowed the cat to catch the bird,
                  She swallowed the bird to catch the spider,
               That wriggled and jiggled and tickled inside her.
                  She swallowed the spider to catch the fly,
                     I don't know why she swallowed a fly,
                              I guess she'll die.

                    I know an old lady who swallowed a cow,
                      I wonder how she swallowed a cow?!
                   She swallowed the cow to catch the goat,
                   She swallowed the goat to catch the dog,
                    She swallowed the dog to catch the cat,
                   She swallowed the cat to catch the bird,
                  She swallowed the bird to catch the spider,
               That wriggled and jiggled and tickled inside her.
                  She swallowed the spider to catch the fly,
                     I don't know why she swallowed a fly,
                              I guess she'll die.

                   I know an old lady who swallowed a horse,
                            She's dead, of course!!

```



## Ruby


```ruby
descriptions = {
  :fly    => "I don't know why S",
  :spider => "That wriggled and jiggled and tickled inside her.",
  :bird   => "Quite absurd T",
  :cat    => "Fancy that, S",
  :dog    => "What a hog, S",
  :goat   => "She opened her throat T",
  :cow    => "I don't know how S",
  :horse  => "She's dead, of course.",
}
animals = descriptions.keys

animals.each_with_index do |animal, idx|
  puts "There was an old lady who swallowed a #{animal}."

  d = descriptions[animal]
  case d[-1]
  when "S" then d[-1] = "she swallowed a #{animal}."
  when "T" then d[-1] = "to swallow a #{animal}."
  end
  puts d
  break if animal == :horse

  idx.downto(1) do |i|
    puts "She swallowed the #{animals[i]} to catch the #{animals[i-1]}."
    case animals[i-1]
    when :spider, :fly then puts descriptions[animals[i-1]]
    end
  end

  print "Perhaps she'll die.\n\n"
end
```


## Rust

[https://play.rust-lang.org/?gist=294c9e29f206d7c8d8028aa5e7002d9a&version=nightly Rust Playground]

```rust
enum Action {Once, Every, Die}
use Action::*;

fn main() {
    let animals = [ ("horse" , Die  , "She's dead, of course!")
                  , ("donkey", Once , "It was rather wonky. To swallow a donkey.")
                  , ("cow"   , Once , "I don't know how. To swallow a cow.")
                  , ("goat"  , Once , "She just opened her throat. To swallow a goat.")
                  , ("pig"   , Once , "Her mouth was so big. To swallow a pig.")
                  , ("dog"   , Once , "What a hog. To swallow a dog.")
                  , ("cat"   , Once , "Fancy that. To swallow a cat.")
                  , ("bird"  , Once , "Quite absurd. To swallow a bird.")
                  , ("spider", Once , "That wriggled and jiggled and tickled inside her.")
                  , ("fly"   , Every, "I don't know why she swallowed the fly.")
                  ];

    for (i, a) in animals.iter().enumerate().rev() {
        println!("There was an old lady who swallowed a {}\n{}", a.0, a.2);

        if let Die = a.1 {break}

        for (swallowed, to_catch) in animals[i..].iter().zip(&animals[i+1..]) {
            println!("She swallowed the {} to catch the {}.", swallowed.0, to_catch.0);

            if let Every = to_catch.1 {
                println!("{}", to_catch.2);
            }
        }

        println!("Perhaps she'll die.\n");
    }
}
```



## Scala


```Scala
case class Verse(animal: String, remark: String, die: Boolean = false, always: Boolean = false)

val verses = List(
  Verse("horse", "She’s dead, of course!", die = true),
  Verse("donkey", "It was rather wonky. To swallow a donkey."),
  Verse("cow", "I don’t know how. To swallow a cow."),
  Verse("goat", "She just opened her throat. To swallow a goat."),
  Verse("pig", "Her mouth was so big. To swallow a pig."),
  Verse("dog", "What a hog. To swallow a dog."),
  Verse("cat", "Fancy that. To swallow a cat."),
  Verse("bird", "Quite absurd. To swallow a bird."),
  Verse("spider", "That wriggled and jiggled and tickled inside her."),
  Verse("fly", "I don’t know why she swallowed the fly.", always = true)
)

for (i <- 1 to verses.size; verse = verses takeRight i; starting = verse.head) {
  println(s"There was an old lady who swallowed a ${starting.animal},")
  println(starting.remark)
  if (!starting.die) {
    for (List(it, next) <- verse.sliding(2,1)) {
      println(s"She swallowed the ${it.animal} to catch the ${next.animal},")
      if (next.always) println(next.remark)
    }
    println("Perhaps she’ll die!")
    println
  }
}
```

```txt
There was an old lady who swallowed a fly,
I don’t know why she swallowed the fly.
Perhaps she’ll die!

There was an old lady who swallowed a spider,
That wriggled and jiggled and tickled inside her.
She swallowed the spider to catch the fly,
I don’t know why she swallowed the fly.
Perhaps she’ll die!

There was an old lady who swallowed a bird,
Quite absurd. To swallow a bird.
She swallowed the bird to catch the spider,
She swallowed the spider to catch the fly,
I don’t know why she swallowed the fly.
Perhaps she’ll die!

...

There was an old lady who swallowed a horse,
She’s dead, of course!

```



## Scratch

See scratch.mit.edu/projects/192324257/


## Seed7


```seed7
$ include "seed7_05.s7i";

const array array string: verses is [] (
    [] ("fly", "I don't know why she swallowed the fly."),
    [] ("spider", "That wriggled and jiggled and tickled inside her."),
    [] ("bird", "Quite absurd. To swallow a bird."),
    [] ("cat", "Fancy that. To swallow a cat."),
    [] ("dog", "What a hog. To swallow a dog."),
    [] ("pig", "Her mouth was so big. To swallow a pig."),
    [] ("goat", "She just opened her throat. To swallow a goat."),
    [] ("cow", "I don't know how. To swallow a cow."),
    [] ("donkey", "It was rather wonky. To swallow a donkey."),
    [] ("horse", "She's dead, of course!"));

const proc: main is func
  local
    var integer: verseNumber is 0;
    var integer: animal is 0;
  begin
    for key verseNumber range verses do
      writeln("There was an old lady who swallowed a " <& verses[verseNumber][1] <& ",");
      writeln(verses[verseNumber][2]);
      if verseNumber <> length(verses) then
        for animal range verseNumber downto 2 do
          writeln("She swallowed the " <& verses[animal][1] <& " to catch the " <& verses[pred(animal)][1] <& ",");
        end for;
        if verseNumber <> 1 then
          writeln(verses[1][2]);
        end if;
        writeln("Perhaps she'll die!");
        writeln;
      end if;
    end for;
  end func;
```



## Sidef

```ruby
var victims = [
    :fly:    "  I don't know why S—",
    :spider: "  That wriggled and jiggled and tickled inside her.",
    :bird:   "  How absurd, T!",
    :cat:    "  Fancy that, S!",
    :dog:    "  What a hog, T!",
    :goat:   "  She just opened her throat, and in walked the goat!",
    :cow:    "  I don't know how S!",
    :horse:  "  She's dead, of course...",
];

var history = ["I guess she'll die...\n"];

victims.each { |pair|
    var (victim, verse) = pair...;
    say "There was an old lady who swallowed a #{victim}...";

    verse.sub!(/\bS\b/, "she swallowed the #{victim}");
    verse.sub!(/\bT\b/, "to swallow a #{victim}!");

    say verse;
    verse ~~ /dead/ && break;

    history[0].sub!(/^X/, "She swallowed the #{victim}");
    history.each{.say};
    history.len < 5 && history.unshift(verse);
    history.unshift("X to catch the #{victim},");
};
```



## Tcl

Because the song is highly repetitive, it compresses wonderfully.

```tcl
package require Tcl 8.6

puts [zlib inflate [binary decode base64 "
    7VRNa8MwDL3nV2inXkZ+xDa29taxws5q7NZuPSvYDib/fnJKPxwXBoNCx3KTn56t
    p6dEC9hbioAWyAgwKHqIisBHNIaiFICwMX1dLUCQnYUDO6oevJIXrMCngbeUTmHr
    U3pmDAgt64ov/1jEt1pIV1crhQGi09utSbgVsLuIg272KdbWMx1UuvFRCDm8BYGg
    wdCos7hbN/Gknair904HCbj2HZ9gdaKcCKXihOd6j37cUXfPGOrqFW3T81sc560N
    2VItw7nUP23BC23r6jN9ogiK49yCIVuqZTiX+i+sWmo2Y86lv6gLCiLyEwRrPTZt
    4JW6Gc5FT+ZemPtGxy53nQ9ArbSMq6RFOSr+zTM915DwXP00jd8sRoqjIoqR0XpM
    nCtGcDITPI3qxgvc7mWqE4aN5DCknyYy2hfL/MC85lzKjMybxnsP452T83JQNPMg
    JIpHoA001DH88A0=
"]]
```



## TXR

Here is somewhat verbose program showing a different approach.

The idea is to start with the last two verses of the song, and then work backwards to produce the earlier verses. This is done by recursively pattern matching on the song to extract text and produce the earlier verse, which is then prepended to the song.

The later verse does not contain one key piece of information we need to produce the prior verse: the animal-specific answer line for the prior animal. So we look this up by scanning a text which serves as a table.

The recursion terminates when the second pattern case matches the first verse: the third line is "Perhaps she'll die". In this case the song is not lengthened any  more, and a terminating flag variable is bound to true.

Note one detail: in the first verse we have "... don't know why she swallowed the fly". But in subsequent verses it is
"that fly" not "the fly".  So we do a lookup on the fly also to substitute the appropriate line, and in the fly case we skip the original line (see the first <code>@(maybe)</code>).

```txr
@(deffilter abbr
   ("IK" "I know an old lady who swallowed a") ("SW" "She swallowed the")
   ("SS" "she swallowed") ("CA" "to catch the") ("XX" "Perhaps she'll die")
   ("C" "cow") ("G" "goat") ("D" "dog") ("T" "cat") ("R" "bird")
   ("S " "spider ") ("F" "fly"))
@(bind lastverse
   ("IK C"
    "I don't know how SS the C"
    "SW C CA G"
    "SW G CA D"
    "SW D CA T"
    "SW T CA R"
    "SW R CA S"
    "SW S CA F"
    "But I don't know why SS that F"
    "XX"
    ""
    "IK horse"
    "She's alive and well of course!"))
@(bind animal_line
  ("G: Opened her throat and down went the G!"
   "D: What a hog to swallow a D!"
   "T: Imagine that! She swallowed a T!"
   "R: How absurd to swallow a R!"
   "S: That wriggled and jiggled and tickled inside her"
   "F: But I don't know why SS the F"))
@(define expand_backwards (song lengthened_song done))
@  (local line2 line3 verse rest animal previous_animal previous_animal_verse)
@  (next :list song)
@  (cases)
IK @animal
@line2
SW @animal CA @previous_animal
@    (maybe)
But @(skip)F
@    (end)
@    (collect)
@    verse
@    (until)

@    (end)
@    (collect)
@    rest
@    (end)
@    (next :list animal_line)
@    (skip)
@previous_animal: @previous_animal_verse
@    (output :into lengthened_song)
IK @previous_animal
@previous_animal_verse
@      (repeat)
@      verse
@      (end)

@      (repeat)
@      song
@      (end)
@    (end)
@    (bind done nil)
@ (or)
IK @(skip)
@line2
XX
@    (bind lengthened_song song)
@    (bind done t)
@ (end)
@(end)
@(define expand_song (in out))
@  (local lengthened done)
@  (expand_backwards in lengthened done)
@  (cases)
@    (bind done nil)
@    (expand_song lengthened out)
@  (or)
@    (bind out lengthened)
@  (end)
@(end)
@(expand_song lastverse song)
@(output :filter abbr)
@  (repeat)
@song
@  (end)
@(end)
```



## UNIX Shell

```sh
animals=(fly spider bird cat dog pig goat cow donkey horse)
comments=("I don't know why she swallowed that fly"
          "That wriggled and jiggled and tickled inside her"
          "Quite absurd, to swallow a bird"
          "How about that, to swallow a cat"
          "What a hog, to swallow a dog"
          "Her mouth was so big to swallow a pig"
          "She just opened her throat to swallow a goat."
          "I don't know how she swallowed a cow."
          "It was rather wonky to swallow a donkey"
          "She's dead, of course!")
include=(2 2 1 1 1 1 1 1 1 0)

for (( i=0; i<${#animals[@]}; ++i )); do
   echo "There was an old lady who swallowed a ${animals[i]}"
   echo "${comments[i]}"
   if (( include[i] )); then
     if (( i )); then
       for (( j=i-1; j>=0; --j )); do
         echo "She swallowed the ${animals[j+1]} to catch the ${animals[j]}"
         if (( include[j] > 1 )); then
           echo "${comments[j]}"
         fi
       done
     fi
     echo "Perhaps she'll die"
     echo
   fi
done
```



## Ursa

```ursa
decl string<>
 reason creatures comments
append "She swallowed the " " to catch the " reason
append "fly" "spider" "bird" "cat" "dog" "goat" "cow" "horse" creatures
append "I don't know why she swallowed that fly.\nPerhaps she'll die\n" comments
append "That wiggled and jiggled and tickled inside her" comments
append "How absurd, to swallow a bird" comments
append "Imagine that. She swallowed a cat" comments
append "What a hog to swallow a dog" comments
append "She just opened her throat and swallowed that goat" comments
append "I don't know how she swallowed that cow" comments
append "She's dead of course" comments

decl int max
set max (size creatures)
for (decl int i) (< i max) (inc i)
        out "There was an old lady who swallowed a " creatures<i> endl console
        out comments<i> endl console
        decl int j
        for (set j i) (and (> j 0) (< i (- max 1))) (dec j)
                out reason<0> creatures<j> reason<1> creatures<(int (- j 1))> endl console
                if (= j 1)
                        out comments<(int (- j 1))> endl console
                end if
        end for
end for
in string console
```



## zkl

Uncompresses the base64 encoded gziped text of the song. Uses shared libraries zlib and msg hashing for the codecs.

```zkl
var ZLib=Import("zeelib"), MsgHash=Import("zklMsgHash");
text:=
  "eJztlE1uwyAQhdflFOOVNyhXyLap1FV7AWKIoaFMBFjIt++M7YVpLfVHilQp2cHMg3l8T+IA"
  "54AFVAD0GrzSIxSLkIryHovRoODkRykOoDG0eVYXO0KyZqXKtFt0/WBS4nbrPWhndoKK3w5J"
  "F6dNlA/i1aoMJbq+99wIGt5W6+y6M69dSKQHa+JOvHxxMl8GGaFTubNrd9d9xdFFLcUjq45p"
  "iJotLP2l22zY5XptdqHxaxjyP8GgcXT4XfUuGLqNdjUO6m/RoHJtdoZ6M9g09lI8j5Ia9AoF"
  "lvY1OFJsgaNybXcK4LYA/4Bvj4zlaUgZ8GIC1SzbsBEZN9n/LN5izfXa+hTbPZQ/fxZY+HDB"
  "wPMtqesBk2K/+V+QtvI7B3zP7OqZWYzJTI7aBNooLQFPlMdA5aYRH3dS5jc=";

MsgHash.base64decode(text) :
ZLib.Inflator().write(_).close().read().text.println();
```

```txt

I know an old lady who swallowed a fly,
I don't know why she swallowed the fly,
I guess she'll die.
...
I know an old lady who swallowed a horse,
She's dead, of course!!

```

