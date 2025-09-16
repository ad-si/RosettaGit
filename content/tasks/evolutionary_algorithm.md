+++
title = "Evolutionary algorithm"
description = ""
date = 2019-10-18T09:56:58Z
aliases = []
[extra]
id = 4873
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "8th",
  "ada",
  "aime",
  "algol_68",
  "as_the_fitness_function",
  "autohotkey",
  "awk",
  "batch_file",
  "bbc_basic",
  "c",
  "ceylon",
  "clojure",
  "cobol",
  "coldfusion",
  "common_lisp",
  "cpp",
  "csharp",
  "d",
  "e",
  "easier_if_the_string_is_a_character_vector",
  "echolisp",
  "elena",
  "elixir",
  "erlang",
  "euphoria",
  "factor",
  "fantom",
  "forth",
  "fortran",
  "freebasic",
  "go",
  "hamming_distance_between_strings_normalized_by_string_length_is_used",
  "haskell",
  "j",
  "java",
  "javascript",
  "julia",
  "kotlin",
  "liberty_basic",
  "logo",
  "lua",
  "m2000_interpreter",
  "matlab",
  "nim",
  "number_of_offspring_in_each_generation",
  "objeck",
  "ocaml",
  "octave",
  "oforth",
  "oorexx",
  "oxygenbasic",
  "oz",
  "pari_gp",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "php",
  "picolisp",
  "pike",
  "pony",
  "prolog",
  "purebasic",
  "python",
  "r",
  "racket",
  "rexx",
  "ring",
  "ruby",
  "rust",
  "scala",
  "scheme",
  "seed7",
  "sequencel",
  "sidef",
  "sinclair_zx81_basic",
  "smalltalk",
  "swift",
  "tcl",
  "ubasic_4th",
  "ursala",
  "utfool",
  "vbscript",
  "visual_basic",
  "xpl0",
  "zkl",
]
+++

Starting with:
* The <code>target</code> string: <code>"METHINKS IT IS LIKE A WEASEL"</code>.
* An array of random characters chosen from the set of upper-case letters together with the space, and of the same length as the target string. (Call it the <code>parent</code>).
* A <code>fitness</code> function that computes the ‘closeness’ of its argument to the target string.
* A <code>mutate</code> function that given a string and a mutation rate returns a copy of the string, with some characters probably mutated.
* While the <code>parent</code> is not yet the <code>target</code>:
:* copy the <code>parent</code> C times, each time allowing some random probability that another character might be substituted using <code>mutate</code>.
:* Assess the <code>fitness</code> of the parent and all the copies to the <code>target</code> and make the most fit string the new <code>parent</code>, discarding the others.
:* repeat until the parent converges, (hopefully), to the target.


## See also

*   Wikipedia entry:   [[wp:Weasel_program#Weasel_algorithm|Weasel algorithm]].
*   Wikipedia entry:   [[wp:Evolutionary algorithm|Evolutionary algorithm]].



<small>Note: to aid comparison, try and ensure the variables and functions mentioned in the task description appear in solutions</small>



A cursory examination of a few of the solutions reveals that the instructions have not been followed rigorously in some solutions. Specifically,
* While the <code>parent</code> is not yet the <code>target</code>:
:* copy the <code>parent</code> C times, each time allowing some random probability that another character might be substituted using <code>mutate</code>.

Note that some of the the solutions given retain characters in the mutated string that are ''correct'' in the target string. However, the instruction above does not state to retain any of the characters while performing the mutation. Although some may believe to do so is implied from the use of "converges"

 (:* repeat until the parent converges, (hopefully), to the target.

Strictly speaking, the new parent should be selected from the new pool of mutations, and then the new parent used to generate the next set of mutations with parent characters getting retained only by ''not'' being mutated. It then becomes possible that the new set of mutations has no member that is fitter than the parent!

As illustration of this error, the code for 8th has the following remark.

 Create a new string based on the TOS, '''changing randomly any characters which
 don't already match the target''':

''NOTE:'' this has been changed, the 8th version is completely random now

Clearly, this algo will be applying the mutation function only to the parent characters that don't match to the target characters!

To ensure that the new parent is never less fit than the prior parent, both the parent and all of the latest mutations are subjected to the fitness test to select the next parent.





## 8th


```forth

\ RosettaCode challenge http://rosettacode.org/wiki/Evolutionary_algorithm
\ Responding to the criticism that the implementation was too directed, this
\ version does a completely random selection of chars to mutate

var gen
\ Convert a string of valid chars into an array of char-strings:
"ABCDEFGHIJKLMNOPQRSTUVWXYZ " null s:/ var, valid-chars

\ How many mutations each generation will handle; the larger, the slower each
\ generation but the fewer generations required:
300 var, #mutations
23 var, mutability

: get-random-char
  valid-chars @
  27 rand-pcg n:abs swap n:mod
  a:@ nip ;

: mutate-string \ s -- s'
  (
    rand-pcg mutability @ n:mod not if
     drop get-random-char
    then
  ) s:map ;

: mutate \ s n -- a
  \ iterate 'n' times over the initial string, mutating it each time
  \ save the original string, as the best of the previous generation:
  >r [] over a:push swap
  (
  tuck mutate-string
  a:push swap
  ) r> times drop ;

\ compute Hamming distance of two strings:
: hamming \ s1 s2 -- n
  0 >r
  s:len n:1-
  (
   2 pick over s:@ nip
   2 pick rot s:@ nip
   n:- n:abs r> n:+ >r
  ) 0 rot loop
  2drop r> ;

var best
: fitness-check \ s a -- s t
  10000 >r
  -1 best !
  (
   \ ix s ix s'
    2 pick hamming
   r@
   over n:> if
      rdrop >r
      best !
   else
      2drop
   then
  )
  a:each
  rdrop best @  a:@ nip  ;


: add-random-char \ s -- s'
  get-random-char s:+ ;

\ take the target and make a random string of the same length
: initial-string \ s -- s
  s:len "" swap
  ' add-random-char
  swap times ;

: done? \ s1 s2 -- s1 s2 | bye
  2dup s:= if
   "Done in " . gen @ . " generations" . cr ;;;
  then ;

: setup-random
  rand rand rand-pcg-seed ;

: evolve
  1 gen n:+!
  \ create an array of #mutations strings mutated from the random string, drop the random
  #mutations @ mutate
  \ iterate over the array and pick the closest fit:
  fitness-check
  \ show this generation's best match:
  dup . cr
  \ check for end condition and continue if not done:
  done? evolve ;

"METHINKS IT IS LIKE A WEASEL"
setup-random initial-string evolve bye
```


The output:

```txt

PIQSLOGHISTIPSDLZFGRDBYUCADA
PIQSNOGH SQIPSDLZFGRDBYUEADA
PIQSNOGH SQIPSDLZFG DBYUEDDA
...
METHINKS IT IS LIKD A WEASEL
METHINKS IT IS LIKD A WEASEL
METHINKS IT IS LIKE A WEASEL
Done in 43 generations

```



## Ada

Very simple fitness determination. For testing purposes you can add a static seed value to the RNG initializations (sample output uses '12345' for both).


```Ada
with Ada.Text_IO;
with Ada.Numerics.Discrete_Random;
with Ada.Numerics.Float_Random;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;

procedure Evolution is

   -- only upper case characters allowed, and space, which uses '@' in
   -- internal representation (allowing subtype of Character).
   subtype DNA_Char is Character range '@' .. 'Z';

   -- DNA string is as long as target string.
   subtype DNA_String is String (1 .. 28);

   -- target string translated to DNA_Char string
   Target : constant DNA_String := "METHINKS@IT@IS@LIKE@A@WEASEL";

   -- calculate the 'closeness' to the target DNA.
   -- it returns a number >= 0 that describes how many chars are correct.
   -- can be improved much to make evolution better, but keep simple for
   -- this example.
   function Fitness (DNA : DNA_String) return Natural is
      Result : Natural := 0;
   begin
      for Position in DNA'Range loop
         if DNA (Position) = Target (Position) then
            Result := Result + 1;
         end if;
      end loop;
      return Result;
   end Fitness;

   -- output the DNA using the mapping
   procedure Output_DNA (DNA : DNA_String; Prefix : String := "") is
      use Ada.Strings.Maps;
      Output_Map : Character_Mapping;
   begin
      Output_Map := To_Mapping
        (From => To_Sequence (To_Set (('@'))),
         To   => To_Sequence (To_Set ((' '))));
      Ada.Text_IO.Put (Prefix);
      Ada.Text_IO.Put (Ada.Strings.Fixed.Translate (DNA, Output_Map));
      Ada.Text_IO.Put_Line (", fitness: " & Integer'Image (Fitness (DNA)));
   end Output_DNA;

   -- DNA_Char is a discrete type, use Ada RNG
   package Random_Char is new Ada.Numerics.Discrete_Random (DNA_Char);
   DNA_Generator : Random_Char.Generator;

   -- need generator for floating type, too
   Float_Generator : Ada.Numerics.Float_Random.Generator;

   -- returns a mutated copy of the parent, applying the given mutation rate
   function Mutate (Parent        : DNA_String;
                    Mutation_Rate : Float)
                    return          DNA_String
   is
      Result : DNA_String := Parent;
   begin
      for Position in Result'Range loop
         if Ada.Numerics.Float_Random.Random (Float_Generator) <= Mutation_Rate
         then
            Result (Position) := Random_Char.Random (DNA_Generator);
         end if;
      end loop;
      return Result;
   end Mutate;

   -- genetic algorithm to evolve the string
   -- could be made a function returning the final string
   procedure Evolve (Child_Count   : Positive := 100;
                     Mutation_Rate : Float    := 0.2)
   is
      type Child_Array is array (1 .. Child_Count) of DNA_String;

      -- determine the fittest of the candidates
      function Fittest (Candidates : Child_Array) return DNA_String is
         The_Fittest : DNA_String := Candidates (1);
      begin
         for Candidate in Candidates'Range loop
            if Fitness (Candidates (Candidate)) > Fitness (The_Fittest)
            then
               The_Fittest := Candidates (Candidate);
            end if;
         end loop;
         return The_Fittest;
      end Fittest;

      Parent, Next_Parent : DNA_String;
      Children            : Child_Array;
      Loop_Counter        : Positive := 1;
   begin
      -- initialize Parent
      for Position in Parent'Range loop
         Parent (Position) := Random_Char.Random (DNA_Generator);
      end loop;
      Output_DNA (Parent, "First: ");
      while Parent /= Target loop
         -- mutation loop
         for Child in Children'Range loop
            Children (Child) := Mutate (Parent, Mutation_Rate);
         end loop;
         Next_Parent := Fittest (Children);
         -- don't allow weaker children as the parent
         if Fitness (Next_Parent) > Fitness (Parent) then
            Parent := Next_Parent;
         end if;
         -- output every 20th generation
         if Loop_Counter mod 20 = 0 then
            Output_DNA (Parent, Integer'Image (Loop_Counter) & ": ");
         end if;
         Loop_Counter := Loop_Counter + 1;
      end loop;
      Output_DNA (Parent, "Final (" & Integer'Image (Loop_Counter) & "): ");
   end Evolve;

begin
   -- initialize the random number generators
   Random_Char.Reset (DNA_Generator);
   Ada.Numerics.Float_Random.Reset (Float_Generator);
   -- evolve!
   Evolve;
end Evolution;
```


sample output:


```txt
First: FCLYNZAOQ KBSZHJAKAWOSZKBOBT, fitness:  1
 20: MKTHCPKS IT MSBBIKEVB SPASEH, fitness:  17
 40: METHIDKS IT NS BIKE B OQASET, fitness:  21
 60: METHIDKS IT NS BIKE B OQASET, fitness:  21
 80: METHIDKS IT NS BIKE B OQASET, fitness:  21
 100: METHIDKS IT VS BIKE B WQASEP, fitness:  22
 120: METHIDKS IT VS BIKE B WQASEP, fitness:  22
 140: METHIDKS ITBVS LIKE B WEASEP, fitness:  23
 160: METHIDKS ITBVS LIKE B WEASEP, fitness:  23
 180: METHIDKS ITBVS LIKE B WEASEP, fitness:  23
 200: METHIDKS ITBIS LIKE B WEASEP, fitness:  24
 220: METHITKS ITBIS LIKE B WEASEL, fitness:  25
 240: METHITKS ITBIS LIKE B WEASEL, fitness:  25
 260: METHITKS ITBIS LIKE B WEASEL, fitness:  25
 280: METHITKS ITBIS LIKE B WEASEL, fitness:  25
 300: METHITKS ITBIS LIKE B WEASEL, fitness:  25
 320: METHITKS ITBIS LIKE B WEASEL, fitness:  25
 340: METHITKS ITBIS LIKE B WEASEL, fitness:  25
 360: METHITKS ITBIS LIKE B WEASEL, fitness:  25
 380: METHINKS ITBIS LIKE A WEASEL, fitness:  27
Final ( 384): METHINKS IT IS LIKE A WEASEL, fitness:  28
```



## Aime

```aime
integer
fitness(data t, data b)
{
    integer c, f, i;

    f = 0;

    for (i, c in b) {
        f += sign(t[i] ^ c);
    }

    f;
}

void
mutate(data e, data b, data u)
{
    integer c;

    for (, c in b) {
        e.append(drand(15) ? c : u[drand(26)]);
    }
}

integer
main(void)
{
    data b, t, u;
    integer f, i;

    t = "METHINK IT IS LIKE A WEASEL";
    u = "ABCDEFGHIJKLMNOPQRSTUVWXYZ ";

    i = ~t;
    while (i) {
        i -= 1;
        b.append(u[drand(26)]);
    }

    f = fitness(t, b);
    while (f) {
        data n;
        integer a;

        o_form("/lw4/~\n", f, b);

        n = b;

        i = 32;
        while (i) {
            data c;

            i -= 1;
            mutate(c, b, u);
            a = fitness(t, c);
            if (a < f) {
                f = a;
                n = c;
            }
        }

        b = n;
    }

    o_form("/lw4/~\n", f, b);

    return 0;
}
```

```txt
23  EAAXIZJROVOHSKREBNSAFHEKF B
22  EAUHIZJREVOHSKREBNSAFHEKF B
21  IAUHIZJREVOHSKREBESAFHEKF B
20  IKUHIZJRETOTSKREBESAFHEKFWB
20  IKUHIZJRETOTSKREBESAFHEKFWB
19  IKUHIZJRET USKREBESAFHEKFWA
19  IKUHIZJRET USKREBESAFHEKFWA
19  IKUHIZJRET USKREBESAFHEKFWA
18  IKUHIZJRET US REBESAFHEKFWA
18  IKUHIZJRET US REBESAFHEKFWA
17  IKMHIZJKET US REBESA HEKFWA
16  IKMHIZJKET US LEBEJA HEKJWA
16  IKMHIZJKET US LEBEJA HEKJWA
16  IKMHIZJKET US LEBEJA HEKJWA
16  IKMHIZJKET US LEBEJA HEKJWA
15  MKKHIZJ ET US LEBEJF HEKJWA
14  MEEHIZJ ET US LEBEJF HEKJWA
14  MEEHIZJ ET US LEBEJF HEKJWA
13  MEEHIZJ ET US LKBE F OEKJWA
12  MEEHIZJ ET US LKKE F OEKJWA
12  MEEHIZJ ET US LKKE F OEKJWA
11  MEEHIZJ ET US LIKE F OEKJWA
11  MEEHIZJ ET US LIKE F OEKJWA
10  MEEHIZJ IT US LIKE F OEKJWA
10  MEEHIZJ IT US LIKE F OEKJWA
...
1   METHINK IT IS LIKE F WEASEL
1   METHINK IT IS LIKE F WEASEL
0   METHINK IT IS LIKE A WEASEL
```



## ALGOL 68

{{trans|C}} Note: This specimen retains the original [[#C|C]] coding style.
```algol68
STRING target := "METHINKS IT IS LIKE A WEASEL";

PROC fitness = (STRING tstrg)REAL:
(
   INT sum := 0;
   FOR i FROM LWB tstrg TO UPB tstrg DO
      sum +:= ABS(ABS target[i] - ABS tstrg[i])
   OD;
   # fitness := # 100.0*exp(-sum/10.0)
);

PROC rand char = CHAR:
(
   #STATIC# []CHAR ucchars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ ";
   # rand char := # ucchars[ENTIER (random*UPB ucchars)+1]
);

PROC mutate = (REF STRING kid, parent, REAL mutate rate)VOID:
(
   FOR i FROM LWB parent TO UPB parent DO
      kid[i] := IF random < mutate rate THEN rand char ELSE parent[i] FI
   OD
);

PROC kewe = ( STRING parent, INT iters, REAL fits, REAL mrate)VOID:
(
   printf(($"#"4d" fitness: "g(-6,2)"% "g(-6,4)" '"g"'"l$, iters, fits, mrate, parent))
);

PROC evolve = VOID:
(
   FLEX[UPB target]CHAR parent;
   REAL fits;
   [100]FLEX[UPB target]CHAR kid;
   INT iters := 0;
   kid[LWB kid] := LOC[UPB target]CHAR;
   REAL mutate rate;

   #  initialize  #
   FOR i FROM LWB parent TO UPB parent DO
      parent[i] := rand char
   OD;

   fits := fitness(parent);
   WHILE fits < 100.0 DO
      INT j;
      REAL kf;
      mutate rate := 1.0  - exp(- (100.0 - fits)/400.0);
      FOR j FROM LWB kid TO UPB kid DO
         mutate(kid[j], parent, mutate rate)
      OD;
      FOR j FROM LWB kid TO UPB kid DO
         kf := fitness(kid[j]);
         IF fits < kf THEN
            fits := kf;
            parent := kid[j]
         FI
      OD;
      IF iters MOD 100 = 0 THEN
         kewe( parent, iters, fits, mutate rate )
      FI;
      iters+:=1
   OD;
   kewe( parent, iters, fits, mutate rate )
);

main:
(
   evolve
)
```

Sample output:

```txt

#0000 fitness:   0.00% 0.2212 'JUQBKWCHNPJ LO LFDKHDJJNQIFQ'
#0100 fitness:   5.50% 0.2104 'NGVGIOJV IT JS MGLD C VEAWCI'
#0200 fitness:  22.31% 0.1765 'MGTGIOJS IU JS MGKD C VEAREL'
#0300 fitness:  60.65% 0.0937 'METHIOKS IU IS LIKE B VFASEL'
#0354 fitness: 100.00% 0.0235 'METHINKS IT IS LIKE A WEASEL'

```



## AutoHotkey



```AutoHotkey
output := ""
target := "METHINKS IT IS LIKE A WEASEL"
targetLen := StrLen(target)
Loop, 26
	possibilities_%A_Index% := Chr(A_Index+64) ; A-Z
possibilities_27  := " "
C := 100

parent := ""
Loop, %targetLen%
{
	Random, randomNum, 1, 27
  parent .= possibilities_%randomNum%
}

Loop,
{
	If (target = parent)
		Break
	If (Mod(A_Index,10) = 0)
		output .= A_Index ": " parent ", fitness: " fitness(parent, target) "`n"
	bestFit := 0
	Loop, %C%
	  If ((fitness := fitness(spawn := mutate(parent), target)) > bestFit)
		  bestSpawn := spawn , bestFit := fitness
	parent := bestFit > fitness(parent, target) ? bestSpawn : parent
	iter := A_Index
}
output .= parent ", " iter
MsgBox, % output
ExitApp

mutate(parent) {
	local	output, replaceChar, newChar
	output := ""
	Loop, %targetLen%
	{
		Random, replaceChar, 0, 9
		If (replaceChar != 0)
			output .= SubStr(parent, A_Index, 1)
		else
		{
			Random, newChar, 1, 27
			output .= possibilities_%newChar%
		}
	}
	Return output
}

fitness(string, target) {
	totalFit := 0
	Loop, % StrLen(string)
		If (SubStr(string, A_Index, 1) = SubStr(target, A_Index, 1))
			totalFit++
	Return totalFit
}
```

Output:

```txt
10: DETRNNKR IAQPFLNVKZ AMXEASEL, fitness: 14
20: METKNNKS IL PALLKKE A XEASEL, fitness: 20
30: METHGNKS IT PSXLKKE A XEASEL, fitness: 23
40: METHGNKS IT IS LKKE A EEASEL, fitness: 25
50: METHGNKS IT IS LKKE A WEASEL, fitness: 26
60: METHGNKS IT IS LKKE A WEASEL, fitness: 26
70: METHGNKS IT IS LIKE A WEASEL, fitness: 27
METHINKS IT IS LIKE A WEASEL, 72

```



## AWK

I apply the rate to each character in each generated child. The number of generations required seems to be really sensitive to the rate. I used the default seeding in GNU awk to obtain the results below. I suspect the algorithm used to generate the pseudo-random numbers may also influence the rapidity of convergence but I haven't investigated that yet. The output shown was obtained using GNU Awk 3.1.5.  BusyBox v1.20.0.git also works but using the same rate generates 88 generations before converging.

```awk

#!/bin/awk -f
function randchar(){
return substr(charset,randint(length(charset)+1),1)
}
function mutate(gene,rate    ,l,newgene){
newgene = ""
for (l=1; l < 1+length(gene); l++){
if (rand() < rate)
   newgene = newgene randchar()
else
   newgene = newgene substr(gene,l,1)
}
return newgene
}
function fitness(gene,target  ,k,fit){
fit = 0
for (k=1;k<1+length(gene);k++){
if (substr(gene,k,1) == substr(target,k,1)) fit = fit + 1
}
return fit
}
function randint(n){
return int(n * rand())
}
function evolve(){
     maxfit = fitness(parent,target)
     oldfit = maxfit
     maxj = 0
     for (j=1; j < D; j++){
         child[j] = mutate(parent,mutrate)
         fit[j] = fitness(child[j],target)
         if (fit[j] > maxfit) {
            maxfit = fit[j]
            maxj = j
            }
          }
     if (maxfit > oldfit) parent = child[maxj]
     }

BEGIN{
target = "METHINKS IT IS LIKE A WEASEL"
charset = " ABCDEFGHIJKLMNOPQRSTUVWXYZ"
mutrate = 0.10
if (ARGC > 1) mutrate = ARGV[1]
lenset = length(charset)
C = 100
D = C + 1
parent = ""
for (j=1; j < length(target)+1; j++) {
     parent = parent randchar()
     }
print "target: " target
print "fitness of target: " fitness(target,target)
print "initial parent: " parent
gens = 0
while (parent != target){
      evolve()
      gens = gens + 1
      if (gens % 10 == 0) print "after " gens " generations,","new parent: " parent," with fitness: " fitness(parent,target)
      }
print "after " gens " generations,"," evolved parent: " parent
}


```

Output:

```txt

# ./awkevolution .08998
target: METHINKS IT IS LIKE A WEASEL
fitness of target: 28
initial parent: EGVCODUCLCILXFXEPNHAMNV BP S
after 10 generations, new parent: EGTSIDKS IT XFXXIKHAANUDEW S  with fitness: 11
after 20 generations, new parent: MKTIIDKS IT IF XIKB A WEEWEL  with fitness: 20
after 30 generations, new parent: M TIIDKS IT IF LIKE A WENSEL  with fitness: 23
after 40 generations, new parent: METIIDKS IT IF LIKE A WEASEL  with fitness: 25
after 50 generations, new parent: METHIDKS IT IS LIKE A WEASEL  with fitness: 27
after 60 generations, new parent: METHINKS IT IS LIKE A WEASEL  with fitness: 28
after 60 generations,  evolved parent: METHINKS IT IS LIKE A WEASEL
#

```



## Batch File


```dos

@echo off
setlocal enabledelayedexpansion

set target=M E T H I N K S @ I T @ I S @ L I K E @ A @ W E A S E L
set chars=A B C D E F G H I J K L M N O P Q R S T U V W X Y Z @

set tempcount=0
for %%i in (%target%) do (
  set /a tempcount+=1
  set target!tempcount!=%%i
)
call:parent

echo  %target%
echo  --------------------------------------------------------

:loop
call:fitness parent
set currentfit=%errorlevel%
if %currentfit%==28 goto end
echo %parent% - %currentfit% [%attempts%]
set attempts=0

:innerloop
set /a attempts+=1
title Attemps - %attempts%
call:mutate %parent%
call:fitness tempparent
set newfit=%errorlevel%
if %newfit% gtr %currentfit% (
  set tempcount=0
  set "parent="
  for %%i in (%tempparent%) do (
    set /a tempcount+=1
    set parent!tempcount!=%%i
    set parent=!parent! %%i
  )
  goto loop
)
goto innerloop

:end
echo %parent% - %currentfit% [%attempts%]
echo Done.
exit /b

:parent
set "parent="
for /l %%i in (1,1,28) do (
  set /a charchosen=!random! %% 27 + 1
  set tempcount=0
  for %%j in (%chars%) do (
    set /a tempcount+=1
    if !charchosen!==!tempcount! (
      set parent%%i=%%j
      set parent=!parent! %%j
    )
  )
)
exit /b

:fitness
set fitness=0
set array=%1
for /l %%i in (1,1,28) do if !%array%%%i!==!target%%i! set /a fitness+=1
exit /b %fitness%

:mutate
set tempcount=0
set returnarray=tempparent
set "%returnarray%="
for %%i in (%*) do (
  set /a tempcount+=1
  set %returnarray%!tempcount!=%%i
  set %returnarray%=!%returnarray%! %%i
)
set /a tomutate=%random% %% 28 + 1
set /a mutateto=%random% %% 27 + 1
set tempcount=0
for %%i in (%chars%) do (
  set /a tempcount+=1
  if %mutateto%==!tempcount! (
    set %returnarray%!tomutate!=%%i
  )
)
set "%returnarray%="
for /l %%i in (1,1,28) do set %returnarray%=!%returnarray%! !%returnarray%%%i!
exit /b

```

Sample Output:

```txt

 M E T H I N K S @ I T @ I S @ L I K E @ A @ W E A S E L
 --------------------------------------------------------
 R S T L U M F Q Y B T L G P L Q T B F C B X F S X S H Y - 3 []
 R S T L I M F Q Y B T L G P L Q T B F C B X F S X S H Y - 4 [9]
 R S T L I M F Q Y B T L G S L Q T B F C B X F S X S H Y - 5 [49]
 R E T L I M F Q Y B T L G S L Q T B F C B X F S X S H Y - 6 [2]
 R E T L I M F Q Y B T L G S L Q T B F C B X F S X S H L - 7 [18]
 R E T L I M F Q Y B T L G S L Q T B F C B X W S X S H L - 8 [5]
 R E T L I M F Q Y B T @ G S L Q T B F C B X W S X S H L - 9 [13]
 R E T L I M F Q Y B T @ G S L L T B F C B X W S X S H L - 10 [114]
 R E T L I M K Q Y B T @ G S L L T B F C B X W S X S H L - 11 [9]
 R E T L I M K Q Y B T @ G S @ L T B F C B X W S X S H L - 12 [17]
 R E T L I M K S Y B T @ G S @ L T B F C B X W S X S H L - 13 [53]
 R E T L I M K S Y I T @ G S @ L T B F C B X W S X S H L - 14 [20]
 R E T L I M K S @ I T @ G S @ L T B F C B X W S X S H L - 15 [121]
 R E T L I M K S @ I T @ G S @ L T B F C B X W S X S E L - 16 [86]
 R E T L I M K S @ I T @ G S @ L T B F C B X W E X S E L - 17 [115]
 R E T H I M K S @ I T @ G S @ L T B F C B X W E X S E L - 18 [54]
 R E T H I M K S @ I T @ G S @ L T B F @ B X W E X S E L - 19 [121]
 R E T H I M K S @ I T @ G S @ L T B F @ B X W E A S E L - 20 [207]
 M E T H I M K S @ I T @ G S @ L T B F @ B X W E A S E L - 21 [5]
 M E T H I M K S @ I T @ G S @ L I B F @ B X W E A S E L - 22 [163]
 M E T H I M K S @ I T @ G S @ L I B E @ B X W E A S E L - 23 [84]
 M E T H I M K S @ I T @ G S @ L I K E @ B X W E A S E L - 24 [31]
 M E T H I N K S @ I T @ G S @ L I K E @ B X W E A S E L - 25 [432]
 M E T H I N K S @ I T @ I S @ L I K E @ B X W E A S E L - 26 [85]
 M E T H I N K S @ I T @ I S @ L I K E @ A X W E A S E L - 27 [144]
 M E T H I N K S @ I T @ I S @ L I K E @ A @ W E A S E L - 28 [227]
Done.

```



## BBC BASIC


```bbcbasic
      target$ = "METHINKS IT IS LIKE A WEASEL"
      parent$ = "IU RFSGJABGOLYWF XSMFXNIABKT"
      mutation_rate = 0.5
      children% = 10

      DIM child$(children%)

      REPEAT
        bestfitness = 0
        bestindex% = 0
        FOR index% = 1 TO children%
          child$(index%) = FNmutate(parent$, mutation_rate)
          fitness = FNfitness(target$, child$(index%))
          IF fitness > bestfitness THEN
            bestfitness = fitness
            bestindex% = index%
          ENDIF
        NEXT index%

        parent$ = child$(bestindex%)
        PRINT parent$
      UNTIL parent$ = target$
      END

      DEF FNfitness(text$, ref$)
      LOCAL I%, F%
      FOR I% = 1 TO LEN(text$)
        IF MID$(text$, I%, 1) = MID$(ref$, I%, 1) THEN F% += 1
      NEXT
      = F% / LEN(text$)

      DEF FNmutate(text$, rate)
      LOCAL C%
      IF rate > RND(1) THEN
        C% = 63+RND(27)
        IF C% = 64 C% = 32
        MID$(text$, RND(LEN(text$)), 1) = CHR$(C%)
      ENDIF
      = text$
```



## C


```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

const char target[] = "METHINKS IT IS LIKE A WEASEL";
const char tbl[] = "ABCDEFGHIJKLMNOPQRSTUVWXYZ ";

#define CHOICE (sizeof(tbl) - 1)
#define MUTATE 15
#define COPIES 30

/* returns random integer from 0 to n - 1 */
int irand(int n)
{
	int r, rand_max = RAND_MAX - (RAND_MAX % n);
	while((r = rand()) >= rand_max);
	return r / (rand_max / n);
}

/* number of different chars between a and b */
int unfitness(const char *a, const char *b)
{
	int i, sum = 0;
	for (i = 0; a[i]; i++)
		sum += (a[i] != b[i]);
	return sum;
}

/* each char of b has 1/MUTATE chance of differing from a */
void mutate(const char *a, char *b)
{
	int i;
	for (i = 0; a[i]; i++)
		b[i] = irand(MUTATE) ? a[i] : tbl[irand(CHOICE)];

	b[i] = '\0';
}

int main()
{
	int i, best_i, unfit, best, iters = 0;
	char specimen[COPIES][sizeof(target) / sizeof(char)];

	/* init rand string */
	for (i = 0; target[i]; i++)
		specimen[0][i] = tbl[irand(CHOICE)];
	specimen[0][i] = 0;

	do {
		for (i = 1; i < COPIES; i++)
			mutate(specimen[0], specimen[i]);

		/* find best fitting string */
		for (best_i = i = 0; i < COPIES; i++) {
			unfit = unfitness(target, specimen[i]);
			if(unfit < best || !i) {
				best = unfit;
				best_i = i;
			}
		}

		if (best_i) strcpy(specimen[0], specimen[best_i]);
		printf("iter %d, score %d: %s\n", iters++, best, specimen[0]);
	} while (best);

	return 0;
}
```
output<lang>iter 0, score 26: WKVVYFJUHOMQJNZYRTEQAGDVXKYC
iter 1, score 25: WKVVTFJUHOMQJN YRTEQAGDVSKXC
iter 2, score 25: WKVVTFJUHOMQJN YRTEQAGDVSKXC
iter 3, score 24: WKVVTFJUHOMQJN YRTEQAGDVAKFC
...
iter 221, score 1: METHINKSHIT IS LIKE A WEASEL
iter 222, score 1: METHINKSHIT IS LIKE A WEASEL
iter 223, score 0: METHINKS IT IS LIKE A WEASEL
```



## C++


```cpp
#include <string>
#include <cstdlib>
#include <iostream>
#include <cassert>
#include <algorithm>
#include <vector>
#include <ctime>

std::string allowed_chars = " ABCDEFGHIJKLMNOPQRSTUVWXYZ";

// class selection contains the fitness function, encapsulates the
// target string and allows access to it's length. The class is only
// there for access control, therefore everything is static. The
// string target isn't defined in the function because that way the
// length couldn't be accessed outside.
class selection
{
public:
  // this function returns 0 for the destination string, and a
  // negative fitness for a non-matching string. The fitness is
  // calculated as the negated sum of the circular distances of the
  // string letters with the destination letters.
  static int fitness(std::string candidate)
  {
    assert(target.length() == candidate.length());

    int fitness_so_far = 0;

    for (int i = 0; i < target.length(); ++i)
    {
      int target_pos = allowed_chars.find(target[i]);
      int candidate_pos = allowed_chars.find(candidate[i]);
      int diff = std::abs(target_pos - candidate_pos);
      fitness_so_far -= std::min(diff, int(allowed_chars.length()) - diff);
    }

    return fitness_so_far;
  }

  // get the target string length
  static int target_length() { return target.length(); }
private:
  static std::string target;
};

std::string selection::target = "METHINKS IT IS LIKE A WEASEL";

// helper function: cyclically move a character through allowed_chars
void move_char(char& c, int distance)
{
  while (distance < 0)
    distance += allowed_chars.length();
  int char_pos = allowed_chars.find(c);
  c = allowed_chars[(char_pos + distance) % allowed_chars.length()];
}

// mutate the string by moving the characters by a small random
// distance with the given probability
std::string mutate(std::string parent, double mutation_rate)
{
  for (int i = 0; i < parent.length(); ++i)
    if (std::rand()/(RAND_MAX + 1.0) < mutation_rate)
    {
      int distance = std::rand() % 3 + 1;
      if(std::rand()%2 == 0)
        move_char(parent[i], distance);
      else
        move_char(parent[i], -distance);
    }
  return parent;
}

// helper function: tell if the first argument is less fit than the
// second
bool less_fit(std::string const& s1, std::string const& s2)
{
  return selection::fitness(s1) < selection::fitness(s2);
}

int main()
{
  int const C = 100;

  std::srand(time(0));

  std::string parent;
  for (int i = 0; i < selection::target_length(); ++i)
  {
    parent += allowed_chars[std::rand() % allowed_chars.length()];
  }

  int const initial_fitness = selection::fitness(parent);

  for(int fitness = initial_fitness;
      fitness < 0;
      fitness = selection::fitness(parent))
  {
    std::cout << parent << ": " << fitness << "\n";
    double const mutation_rate = 0.02 + (0.9*fitness)/initial_fitness;
    std::vector<std::string> childs;
    childs.reserve(C+1);

    childs.push_back(parent);
    for (int i = 0; i < C; ++i)
      childs.push_back(mutate(parent, mutation_rate));

    parent = *std::max_element(childs.begin(), childs.end(), less_fit);
  }
  std::cout << "final string: " << parent << "\n";
}
```

Example output:

```txt

BBQYCNLDIHG   RWEXN PNGFTCMS: -203
ECPZEOLCHFJBCXTXFYLZQPDDQ KP: -177
HBSBGMKEEIM BUTUGWKWNRCGSZNN: -150
EEUCGNKDCHN  RSSITKZPRBESYQK: -134
GBRFGNKDAINX TVRITIZPSBERXTH: -129
JEUFILLDDGNZCWYRIWFWSUAERZUI: -120
JESGILIGDJOZCWXRIWFVSXZESXXI: -109
JCSHILIIDIOZCTZOIUIVVXZEUVXI: -93
KDSHHLJIDIOZER LIUGXVXXFWW I: -76
KDSHGNMIDIOZHR LIUHXWXWFWW L: -69
LDSHHNMLDIOZKR LGSEXWXWFYV L: -59
LDSHHNMNDIOYKU LGSEXY WFYV M: -55
LCSHHNMLDHR IT LGSEZY WFYSBM: -44
LCSHHNMNBIR IT LGSEZY WFASBM: -36
LCSHHNMQBIQ JT LGQEZY WFASBM: -33
LCSIHNMRBIS JT LGQE Y WFASBM: -30
LESIHNMSBIS JR LGQE Y WFASBM: -27
LESIJNMSBIS JR LHOE A WFASBM: -21
LERIJNJSBIS JR LHOF A WFASEM: -19
LERIJNJSBIS JR LHLF A WFASEM: -16
NERIJNJS IS JR LHLF A WFASEM: -14
NERIJNJS IS JS LHLF A WFASEM: -13
NERIJNKS IS JS LHLF A WFASEM: -12
NERIJNKS IS JS LHKF A WFASEM: -11
NERIJNKS IS JS LHKF A WFASEM: -11
NERIJNKS IS JS LHKF A WEASEM: -10
NERIJNKS IS JS LHKF A WEASEM: -10
NERIJNKS IS JS LHKF A WEASEL: -9
NERIJNKS IS JS LHKF A WEASEL: -9
NETIJNKS IS JS LHKF A WEASEL: -7
NETIJNKS IS JS LHKF A WEASEL: -7
NETIJNKS IT JS LHKF A WEASEL: -6
NETIINKS IT JS LHKF A WEASEL: -5
NETIINKS IT JS LHKE A WEASEL: -4
NETHINKS IT JS LHKE A WEASEL: -3
NETHINKS IT JS LIKE A WEASEL: -2
NETHINKS IT JS LIKE A WEASEL: -2
NETHINKS IT JS LIKE A WEASEL: -2
NETHINKS IT JS LIKE A WEASEL: -2
NETHINKS IT JS LIKE A WEASEL: -2
NETHINKS IT JS LIKE A WEASEL: -2
METHINKS IT JS LIKE A WEASEL: -1
METHINKS IT JS LIKE A WEASEL: -1
METHINKS IT JS LIKE A WEASEL: -1
final string: METHINKS IT IS LIKE A WEASEL

```


## C#

```c#
using System;
using System.Collections.Generic;
using System.Linq;

static class Program {
    static Random Rng = new Random((int)DateTime.Now.Ticks);

    static char NextCharacter(this Random self) {
        const string AllowedChars = " ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        return AllowedChars[self.Next() % AllowedChars.Length];
    }

    static string NextString(this Random self, int length) {
        return String.Join("", Enumerable.Repeat(' ', length)
            .Select(c => Rng.NextCharacter()));
    }

    static int Fitness(string target, string current) {
        return target.Zip(current, (a, b) => a == b ? 1 : 0).Sum();
    }

    static string Mutate(string current, double rate) {
        return String.Join("", from c in current
               select Rng.NextDouble() <= rate ? Rng.NextCharacter() : c);
    }

    static void Main(string[] args) {
        const string target = "METHINKS IT IS LIKE A WEASEL";
        const int C = 100;
        const double P = 0.05;

        // Start with a random string the same length as the target.
        string parent = Rng.NextString(target.Length);

        Console.WriteLine("START:       {0,20} fitness: {1}",
            parent, Fitness(target, parent));
        int i = 0;

        while (parent != target) {
            // Create C mutated strings + the current parent.
            var candidates = Enumerable.Range(0, C + 1)
                .Select(n => n > 0 ? Mutate(parent, P) : parent);

            // select the fittest
            parent = candidates.OrderByDescending(c => Fitness(target, c)).First();

            ++i;
            Console.WriteLine("     #{0,6} {1,20} fitness: {2}",
                i, parent, Fitness(target, parent));
        }

        Console.WriteLine("END: #{0,6} {1,20}", i, parent);
    }
}
```


Example output:
<div style="height:30ex;overflow:scroll">
```txt

START:       PACQXJB CQPWEYKSVDCIOUPKUOJY fitness: 0
     #     1 PALQXJB CQPWEYKSVDCIOUPEUOJY fitness: 1
     #     2 PALQXJB CQPWEYKSVDEIOUPEUOJY fitness: 2
     #     3 PALQXJB CQPWEYKSVDE OUPEUOJY fitness: 3
     #     4 MALQOJB CQPWEYKSVDE OUPEUOJY fitness: 4
     #     5 MALQOJB CQPWEYKSVKE OUPEUOJY fitness: 5
     #     6 MALQOJB CQPWEYKLVKE OUPEUOES fitness: 7
     #     7 MALQOJB CQPWEYKLVKE OUPEAOES fitness: 8
     #     8 M LQOJB CQPWEYKLVKE OUPEAOES fitness: 8
     #     9 M LQOJB CQPWEYKL KE OUPEAOES fitness: 8
     #    10 M LHOJB CQPWEYKL KE OUPEAOES fitness: 9
     #    11 M LHOJB CQPWEYKL KE OGYEAOEL fitness: 10
     #    12 M LHOJB CQP EYKL KE OGYEAOEL fitness: 11
     #    13 M THOJB CQP EYKL KE OGYEAOEL fitness: 12
     #    14 M THOJB CQP ESKL KE OGYEAOEL fitness: 13
     #    15 M THOJB CQP ESKL KE AGYEAOEL fitness: 14
     #    16 M THHJBSCQP ESKL KE AGYEAOEL fitness: 15
     #    17 M THHJBSCQP ES L KE AGYEAOEL fitness: 16
     #    18 MXTHHJBSCQP ES L KE AGYEASEL fitness: 17
     #    19 MXTHHJBSCOT ES L KE AGYEASEL fitness: 18
     #    20 MXTHHJBSCOT ES L KE AGYEASEL fitness: 18
     #    21 METHHJBSCOT GS L KE ACYEASEL fitness: 19
     #    22 METHIJBSCOT GS L KE ACYEASEL fitness: 20
     #    23 METHILBSCOT GS L KE ACYEASEL fitness: 20
     #    24 METHILBSCOT GS L KE ACWEASEL fitness: 21
     #    25 METHILBS OT GS LBKE ACWEASEL fitness: 22
     #    26 METHILBS OT GS LBKE ACWEASEL fitness: 22
     #    27 METHILBS OT IS LBKE ACWEASEL fitness: 23
     #    28 METHILBS OT IS LBKE ACWEASEL fitness: 23
     #    29 METHILBS OT IS LBKE ACWEASEL fitness: 23
     #    30 METHILBS CT IS LPKE ACWEASEL fitness: 23
     #    31 METHILBS CT IS LPKE ACWEASEL fitness: 23
     #    32 METHILBS CT IS LPKE A WEASEL fitness: 24
     #    33 METHILBS ET IS LPKE A WEASEL fitness: 24
     #    34 METHILBS ET IS LPKE A WEASEL fitness: 24
     #    35 METHILBS ET IS LPKE A WEASEL fitness: 24
     #    36 METHILBS ET IS LPKE A WEASEL fitness: 24
     #    37 METHILBS IT IS LPKE A WEASEL fitness: 25
     #    38 METHILBS IT IS LPKE A WEASEL fitness: 25
     #    39 METHILBS IT IS LPKE A WEASEL fitness: 25
     #    40 METHILBS IT IS LPKE A WEASEL fitness: 25
     #    41 METHILBS IT IS LPKE A WEASEL fitness: 25
     #    42 METHILBS IT IS LPKE A WEASEL fitness: 25
     #    43 METHINBS IT IS LPKE A WEASEL fitness: 26
     #    44 METHINBS IT IS LPKE A WEASEL fitness: 26
     #    45 METHINBS IT IS LPKE A WEASEL fitness: 26
     #    46 METHINBS IT IS LIKE A WEASEL fitness: 27
     #    47 METHINBS IT IS LIKE A WEASEL fitness: 27
     #    48 METHINBS IT IS LIKE A WEASEL fitness: 27
     #    49 METHINBS IT IS LIKE A WEASEL fitness: 27
     #    50 METHINBS IT IS LIKE A WEASEL fitness: 27
     #    51 METHINBS IT IS LIKE A WEASEL fitness: 27
     #    52 METHINBS IT IS LIKE A WEASEL fitness: 27
     #    53 METHINBS IT IS LIKE A WEASEL fitness: 27
     #    54 METHINBS IT IS LIKE A WEASEL fitness: 27
     #    55 METHINBS IT IS LIKE A WEASEL fitness: 27
     #    56 METHINBS IT IS LIKE A WEASEL fitness: 27
     #    57 METHINBS IT IS LIKE A WEASEL fitness: 27
     #    58 METHINBS IT IS LIKE A WEASEL fitness: 27
     #    59 METHINBS IT IS LIKE A WEASEL fitness: 27
     #    60 METHINBS IT IS LIKE A WEASEL fitness: 27
     #    61 METHINBS IT IS LIKE A WEASEL fitness: 27
     #    62 METHINKS IT IS LIKE A WEASEL fitness: 28
END: #    62 METHINKS IT IS LIKE A WEASEL

```
</div>


## Ceylon


```ceylon
import ceylon.random {

	DefaultRandom
}

shared void run() {

	value mutationRate = 0.05;
	value childrenPerGeneration = 100;
	value target = "METHINKS IT IS LIKE A WEASEL";
	value alphabet = {' ', *('A'..'Z')};
	value random = DefaultRandom();

	value randomLetter => random.nextElement(alphabet);

	function fitness(String a, String b) =>
			count {for([c1, c2] in zipPairs(a, b)) c1 == c2};

	function mutate(String string) =>
			String {
				for(letter in string)
				if(random.nextFloat() < mutationRate)
				then randomLetter
				else letter
			};

	function makeCopies(String string) =>
			{for(i in 1..childrenPerGeneration) mutate(string)};

	function chooseFittest(String+ children) =>
			children
			.map((String element) => element->fitness(element, target))
			.max(increasingItem)
			.key;

	variable value parent = String {for(i in 1..target.size) randomLetter};
	variable value generationCount = 0;
	function display() => print("``generationCount``: ``parent``");

	display();
	while(parent != target) {
		parent = chooseFittest(parent, *makeCopies(parent));
		generationCount++;
		display();
	}

	print("mutated into target in ``generationCount`` generations!");

}
```



## Clojure

Define the evolution parameters (values here per Wikipedia article), with a couple of problem constants.

```clojure
(def c 100)  ;number of children in each generation
(def p 0.05) ;mutation probability

(def target "METHINKS IT IS LIKE A WEASEL")
(def tsize (count target))

(def alphabet " ABCDEFGHIJLKLMNOPQRSTUVWXYZ")
```

Now the major functions. ''fitness'' simply counts the number of characters matching the target.

```clojure
(defn fitness [s] (count (filter true? (map = s target))))
(defn perfectly-fit? [s] (= (fitness s) tsize))

(defn randc [] (rand-nth alphabet))
(defn mutate [s] (map #(if (< (rand) p) (randc) %) s))
```

Finally evolve. At each generation, print the generation number, the parent, and the parent's fitness.

```clojure
(loop [generation 1, parent (repeatedly tsize randc)]
  (println generation, (apply str parent), (fitness parent))
  (if-not (perfectly-fit? parent)
    (let [children (repeatedly c #(mutate parent))
          fittest (apply max-key fitness parent children)]
      (recur (inc generation), fittest))))
```



## COBOL

For testing purposes, you can comment out the first two sentences in the <tt>CONTROL-PARAGRAPH</tt> and the program will then use the same sequence of pseudo-random numbers on each run.

```cobol
identification division.
program-id. evolutionary-program.
data division.
working-storage section.
01  evolving-strings.
    05 target                pic a(28)
        value 'METHINKS IT IS LIKE A WEASEL'.
    05 parent                pic a(28).
    05 offspring-table.
        10 offspring         pic a(28)
            occurs 50 times.
01  fitness-calculations.
    05 fitness               pic 99.
    05 highest-fitness       pic 99.
    05 fittest               pic 99.
01  parameters.
    05 character-set         pic a(27)
        value 'ABCDEFGHIJKLMNOPQRSTUVWXYZ '.
    05 size-of-generation    pic 99
        value 50.
    05 mutation-rate         pic 99
        value 5.
01  counters-and-working-variables.
    05 character-position    pic 99.
    05 randomization.
        10 random-seed       pic 9(8).
        10 random-number     pic 99.
        10 random-letter     pic 99.
    05 generation            pic 999.
    05 child                 pic 99.
    05 temporary-string      pic a(28).
procedure division.
control-paragraph.
    accept random-seed from time.
    move function random(random-seed) to random-number.
    perform random-letter-paragraph,
    varying character-position from 1 by 1
    until character-position is greater than 28.
    move temporary-string to parent.
    move zero to generation.
    perform output-paragraph.
    perform evolution-paragraph,
    varying generation from 1 by 1
    until parent is equal to target.
    stop run.
evolution-paragraph.
    perform mutation-paragraph varying child from 1 by 1
    until child is greater than size-of-generation.
    move zero to highest-fitness.
    move 1 to fittest.
    perform check-fitness-paragraph varying child from 1 by 1
    until child is greater than size-of-generation.
    move offspring(fittest) to parent.
    perform output-paragraph.
output-paragraph.
    display generation ': ' parent.
random-letter-paragraph.
    move function random to random-number.
    divide random-number by 3.80769 giving random-letter.
    add 1 to random-letter.
    move character-set(random-letter:1)
    to temporary-string(character-position:1).
mutation-paragraph.
    move parent to temporary-string.
    perform character-mutation-paragraph,
    varying character-position from 1 by 1
    until character-position is greater than 28.
    move temporary-string to offspring(child).
character-mutation-paragraph.
    move function random to random-number.
    if random-number is less than mutation-rate
    then perform random-letter-paragraph.
check-fitness-paragraph.
    move offspring(child) to temporary-string.
    perform fitness-paragraph.
fitness-paragraph.
    move zero to fitness.
    perform character-fitness-paragraph,
    varying character-position from 1 by 1
    until character-position is greater than 28.
    if fitness is greater than highest-fitness
    then perform fittest-paragraph.
character-fitness-paragraph.
    if temporary-string(character-position:1) is equal to
    target(character-position:1) then add 1 to fitness.
fittest-paragraph.
    move fitness to highest-fitness.
    move child to fittest.
```

<div style="height:50ex;overflow:scroll">

```txt
000: YZPLJKKFEZTWMSGAPVMUZBKBLLRS
001: YZPLJKKFEZTWMSGAPVMUZBKBLLRS
002: YZPLJKKFEZTWMS APVMUZBKBLLRS
003: JZPLJKKFEZTWMS AIVMUZBKBLLRS
004: JZPLJKKFEZTWMS AIVBUABKBLLRS
005: JZPLJKKFEZTWIS AIVBUABKBLLRS
006: JZPLJKKFEZTWIS AIVBUABKBLLRS
007: MVPLXKKFECTWIS AIVBUABKBLLRS
008: MVPLXKKSECTWIS AIVBUABKBLLRS
009: MVPLCKKSUCTWIS AIVBUABKBLLRS
010: MVPLCKKSUCTJIS LIVBVABKBLLRS
011: MVPLCKKSUCTJIS LIVBVABKBLSRS
012: MVPLCKKSUCTJIS LIVBQABKBLSRS
013: MVPLCKKSUCTJIS LIVBQABKBLSRS
014: MEPLCKKSUCTJIS LIVBQABKBLSRS
015: MEPVCKKSUCTJIS LIVBFABKBLSRS
016: MEPVCKKSUCTJIS LIVBFABKBLSRE
017: MEPVCKKSUCTJIS LIVBFABKBLSEE
018: MEPVCKKSUCTJIS LIVBFABWBLSEE
019: MEPVCKKSUCTJIS LIVBFABWBLSEE
020: MEPXCKKSUCTJIS LIVBFABWBLSEE
021: MEPXCKKSUCTJIS LIVBFABWBLSEE
022: MEPXCKKSUSTJIS LIVBFABWBLSEE
023: MEPXCKKSUSTJIS LIVBFABWBASEE
024: MEPXCKKSUSTJIS LIVEFABWBASEM
025: MEPXCKKSUSTJIS LIVEFABWEASEM
026: MEPXCKKSUSTJIS LIVEFABWEASEM
027: MEPXCKKSUITJIS LIVEFABWEASEM
028: MEPXCNKSUITJIS LIVEFABWEASEM
029: MEPXCNKSUITJIS LIVEFABWEASEM
030: MEPXCNKS ITJIS LIVEFABWEASEM
031: MEPXCNKS ITJIS LIVEFABWEASEM
032: MEPXCNKS ITJIS LIVEFABWEASEM
033: MEPXCNKS ITJIS LIVEFABWEASEM
034: MEPXCNKS ITNIS LIVEFABWEASEM
035: METICNKS ITNIS LIVEYABWEASEM
036: METICNKS ITNIS LIVEYABWEASEM
037: METICNKS ITMIS LIVEYABWEASEM
038: METIHNKS ITMIS LIVEYABWEASEM
039: METIHNKS ITMIS LIVEYABWEASEM
040: METIHNKS ITMIS LIKEYABWEASEM
041: METIHNKS IT IS LIKEYABWEASEM
042: METIHNKS IT IS LIKEYABWEASEM
043: METIHNKS IT IS LIKEPABWEASEM
044: METIHNKS IT IS LIKEPABWEASEM
045: METHHNKS IT IS LIKEPABWEASEM
046: METHHNKS IT IS LIKEPABWEASEM
047: METHHNKS IT IS LIKEPABWEASEM
048: METHHNKS IT IS LIKEPABWEASEM
049: METHHNKS IT IS LIKEPABWEASEM
050: METHHNKS IT IS LIKEPABWEASEM
051: METHHNKS IT IS LIKEPABWEASEM
052: METHHNKS IT IS LIKEPABWEASEL
053: METHHNKS IT IS LIKEPABWEASEL
054: METHHNKS IT IS LIKEPA WEASEL
055: METHHNKS IT IS LIKEPA WEASEL
056: METHHNKS IT IS LIKEPA WEASEL
057: METHINKS IT IS LIKEPA WEASEL
058: METHINKS IT IS LIKEPA WEASEL
059: METHINKS IT IS LIKECA WEASEL
060: METHINKS IT IS LIKECA WEASEL
061: METHINKS IT IS LIKEAA WEASEL
062: METHINKS IT IS LIKEAA WEASEL
063: METHINKS IT IS LIKEAA WEASEL
064: METHINKS IT IS LIKETA WEASEL
065: METHINKS IT IS LIKETA WEASEL
066: METHINKS IT IS LIKETA WEASEL
067: METHINKS IT IS LIKETA WEASEL
068: METHINKS IT IS LIKETA WEASEL
069: METHINKS IT IS LIKETA WEASEL
070: METHINKS IT IS LIKETA WEASEL
071: METHINKS IT IS LIKETA WEASEL
072: METHINKS IT IS LIKETA WEASEL
073: METHINKS IT IS LIKETA WEASEL
074: METHINKS IT IS LIKETA WEASEL
075: METHINKS IT IS LIKETA WEASEL
076: METHINKS IT IS LIKETA WEASEL
077: METHINKS IT IS LIKETA WEASEL
078: METHINKS IT IS LIKETA WEASEL
079: METHINKS IT IS LIKETA WEASEL
080: METHINKS IT IS LIKETA WEASEL
081: METHINKS IT IS LIKETA WEASEL
082: METHINKS IT IS LIKETA WEASEL
083: METHINKS IT IS LIKETA WEASEL
084: METHINKS IT IS LIKETA WEASEL
085: METHINKS IT IS LIKETA WEASEL
086: METHINKS IT IS LIKETA WEASEL
087: METHINKS IT IS LIKETA WEASEL
088: METHINKS IT IS LIKETA WEASEL
089: METHINKS IT IS LIKETA WEASEL
090: METHINKS IT IS LIKETA WEASEL
091: METHINKS IT IS LIKETA WEASEL
092: METHINKS IT IS LIKETA WEASEL
093: METHINKS IT IS LIKETA WEASEL
094: METHINKS IT IS LIKETA WEASEL
095: METHINKS IT IS LIKETA WEASEL
096: METHINKS IT IS LIKETA WEASEL
097: METHINKS IT IS LIKETA WEASEL
098: METHINKS IT IS LIKETA WEASEL
099: METHINKS IT IS LIKETA WEASEL
100: METHINKS IT IS LIKETA WEASEL
101: METHINKS IT IS LIKETA WEASEL
102: METHINKS IT IS LIKETA WEASEL
103: METHINKS IT IS LIKETA WEASEL
104: METHINKS IT IS LIKETA WEASEL
105: METHINKS IT IS LIKETA WEASEL
106: METHINKS IT IS LIKETA WEASEL
107: METHINKS IT IS LIKETA WEASEL
108: METHINKS IT IS LIKETA WEASEL
109: METHINKS IT IS LIKETA WEASEL
110: METHINKS IT IS LIKETA WEASEL
111: METHINKS IT IS LIKETA WEASEL
112: METHINKS IT IS LIKETA WEASEL
113: METHINKS IT IS LIKETA WEASEL
114: METHINKS IT IS LIKETA WEASEL
115: METHINKS IT IS LIKETA WEASEL
116: METHINKS IT IS LIKETA WEASEL
117: METHINKS IT IS LIKETA WEASEL
118: METHINKS IT IS LIKETA WEASEL
119: METHINKS IT IS LIKETA WEASEL
120: METHINKS IT IS LIKETA WEASEL
121: METHINKS IT IS LIKETA WEASEL
122: METHINKS IT IS LIKETA WEASEL
123: METHINKS IT IS LIKETA WEASEL
124: METHINKS IT IS LIKETA WEASEL
125: METHINKS IT IS LIKETA WEASEL
126: METHINKS IT IS LIKETA WEASEL
127: METHINKS IT IS LIKEDA WEASEL
128: METHINKS IT IS LIKEDA WEASEL
129: METHINKS IT IS LIKEDA WEASEL
130: METHINKS IT IS LIKEKA WEASEL
131: METHINKS IT IS LIKEKA WEASEL
132: METHINKS IT IS LIKEKA WEASEL
133: METHINKS IT IS LIKEKA WEASEL
134: METHINKS IT IS LIKEKA WEASEL
135: METHINKS IT IS LIKEKA WEASEL
136: METHINKS IT IS LIKEKA WEASEL
137: METHINKS IT IS LIKEKA WEASEL
138: METHINKS IT IS LIKEKA WEASEL
139: METHINKS IT IS LIKEKA WEASEL
140: METHINKS IT IS LIKEKA WEASEL
141: METHINKS IT IS LIKEKA WEASEL
142: METHINKS IT IS LIKEKA WEASEL
143: METHINKS IT IS LIKEKA WEASEL
144: METHINKS IT IS LIKEKA WEASEL
145: METHINKS IT IS LIKEKA WEASEL
146: METHINKS IT IS LIKEKA WEASEL
147: METHINKS IT IS LIKEKA WEASEL
148: METHINKS IT IS LIKEKA WEASEL
149: METHINKS IT IS LIKEKA WEASEL
150: METHINKS IT IS LIKEKA WEASEL
151: METHINKS IT IS LIKEKA WEASEL
152: METHINKS IT IS LIKEKA WEASEL
153: METHINKS IT IS LIKEKA WEASEL
154: METHINKS IT IS LIKEKA WEASEL
155: METHINKS IT IS LIKEKA WEASEL
156: METHINKS IT IS LIKEKA WEASEL
157: METHINKS IT IS LIKEKA WEASEL
158: METHINKS IT IS LIKEKA WEASEL
159: METHINKS IT IS LIKEKA WEASEL
160: METHINKS IT IS LIKEKA WEASEL
161: METHINKS IT IS LIKEKA WEASEL
162: METHINKS IT IS LIKEKA WEASEL
163: METHINKS IT IS LIKEKA WEASEL
164: METHINKS IT IS LIKEHA WEASEL
165: METHINKS IT IS LIKEHA WEASEL
166: METHINKS IT IS LIKEHA WEASEL
167: METHINKS IT IS LIKEHA WEASEL
168: METHINKS IT IS LIKEHA WEASEL
169: METHINKS IT IS LIKEHA WEASEL
170: METHINKS IT IS LIKEYA WEASEL
171: METHINKS IT IS LIKEYA WEASEL
172: METHINKS IT IS LIKEYA WEASEL
173: METHINKS IT IS LIKEYA WEASEL
174: METHINKS IT IS LIKEYA WEASEL
175: METHINKS IT IS LIKEYA WEASEL
176: METHINKS IT IS LIKEYA WEASEL
177: METHINKS IT IS LIKEYA WEASEL
178: METHINKS IT IS LIKEYA WEASEL
179: METHINKS IT IS LIKEYA WEASEL
180: METHINKS IT IS LIKEYA WEASEL
181: METHINKS IT IS LIKEYA WEASEL
182: METHINKS IT IS LIKEYA WEASEL
183: METHINKS IT IS LIKEYA WEASEL
184: METHINKS IT IS LIKEYA WEASEL
185: METHINKS IT IS LIKEYA WEASEL
186: METHINKS IT IS LIKEYA WEASEL
187: METHINKS IT IS LIKEYA WEASEL
188: METHINKS IT IS LIKEYA WEASEL
189: METHINKS IT IS LIKEZA WEASEL
190: METHINKS IT IS LIKEZA WEASEL
191: METHINKS IT IS LIKEZA WEASEL
192: METHINKS IT IS LIKEZA WEASEL
193: METHINKS IT IS LIKEZA WEASEL
194: METHINKS IT IS LIKE A WEASEL
```
</div>


## ColdFusion


```cfm

<Cfset theString = 'METHINKS IT IS LIKE A WEASEL'>
<cfparam name="parent" default="">
<Cfset theAlphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ ">
<Cfset fitness = 0>
<Cfset children = 3>
<Cfset counter = 0>

<Cfloop from="1" to="#children#" index="child">
  <Cfparam name="child#child#" default="">
  <Cfparam name="fitness#child#" default=0>
</Cfloop>

<Cfloop condition="fitness lt 1">

  <Cfset oldparent = parent>
  <Cfset counter = counter + 1>

  <cfloop from="1" to="#children#" index="child">
    <Cfset thischild = ''>

    <Cfloop from="1" to="#len(theString)#" index="i">
      <cfset Mutate = Mid(theAlphabet, RandRange(1, 28), 1)>
      <cfif fitness eq 0>
        <Cfset thischild = thischild & mutate>
      <Cfelse>

        <Cfif Mid(theString, i, 1) eq Mid(variables["child" & child], i, 1)>
          <Cfset thischild = thischild & Mid(variables["child" & child], i, 1)>
        <Cfelse>
          <cfset MutateChance = 1/fitness>
          <Cfset MutateChanceRand = rand()>
          <Cfif MutateChanceRand lte MutateChance>
            <Cfset thischild = thischild & mutate>
          <Cfelse>
            <Cfset thischild = thischild & Mid(variables["child" & child], i, 1)>
          </Cfif>
        </Cfif>

      </cfif>
    </Cfloop>

    <Cfset variables["child" & child] = thischild>

</cfloop>

  <cfloop from="1" to="#children#" index="child">
    <Cfset thisChildFitness = 0>
    <Cfloop from="1" to="#len(theString)#" index="i">
      <Cfif Mid(variables["child" & child], i, 1) eq Mid(theString, i, 1)>
        <Cfset thisChildFitness = thisChildFitness + 1>
      </Cfif>
    </Cfloop>

    <Cfset variables["fitness" & child] = (thisChildFitness)/len(theString)>

    <Cfif variables["fitness" & child] gt fitness>
      <Cfset fitness = variables["fitness" & child]>
      <Cfset parent = variables["child" & child]>
    </Cfif>

  </cfloop>

  <Cfif parent neq oldparent>
    <Cfoutput>###counter# #numberformat(fitness*100, 99)#% fit: #parent#
</Cfoutput><cfflush>
  </Cfif>

</Cfloop>

```


```txt

#1 7% fit: VOPJOBSYPTTUNYYSAFHTPJUIAIL
#2 18% fit: FQUFHEKPLXTQISYZZRIEVQWBHRC
#3 21% fit: MGTUKIRICATKDDMSIUNDERUAASKT
#33 29% fit: M THILKORWP XSRVOLV GVIRVJHE
#34 36% fit: MEBHRNTSYPH IHTCHMH LGWBAFZ
#37 39% fit: MSTHIWKLIHU KSSLECR Z WGUMZE
#61 43% fit: METHINKA RT ZRQCEFVEAMWKZEBA
#62 50% fit: METHINKA GT RLQAOHVSAXWNAS A
#67 54% fit: MESHINKT IGBWSRLIEEAF WERYWH
#72 57% fit: METHINKE VT YBUJNRXRA W XSEL
#129 64% fit: METHINKS ITCIEHLPNB A YYAAPL
#156 68% fit: METHINKS IT IHIWJKY I W GSAL
#177 71% fit: METHINKS IT IS RIPRPA BEAVYN
#180 75% fit: METHINKS IT IS OI BAA TEABBL
#185 79% fit: METHINKS IT IS LIQEWA EEARLX
#197 82% fit: METHINKS IT IS LIKP OKWEASMU
#222 86% fit: METHINKS IT IS LIKESG WEALEH
#245 89% fit: METHINKS IT IS LIKEOA GEAQEL
#304 93% fit: METHINKS IT IS LIKE A WESSYL
#349 96% fit: METHINKS IT IS LIKE A WEASOL
#360 100% fit: METHINKS IT IS LIKE A WEASEL

```




## Common Lisp


```lisp
(defun fitness (string target)
  "Closeness of string to target; lower number is better"
  (loop for c1 across string
        for c2 across target
        count (char/= c1 c2)))

(defun mutate (string chars p)
  "Mutate each character of string with probablity p using characters from chars"
  (dotimes (n (length string))
    (when (< (random 1.0) p)
      (setf (aref string n) (aref chars (random (length chars))))))
  string)

(defun random-string (chars length)
  "Generate a new random string consisting of letters from char and specified length"
  (do ((n 0 (1+ n))
       (str (make-string length)))
      ((= n length) str)
    (setf (aref str n) (aref chars (random (length chars))))))

(defun evolve-string (target string chars c p)
  "Generate new mutant strings, and choose the most fit string"
  (let ((mutated-strs (list string)))
    (dotimes (n c)
      (push (mutate (copy-seq string) chars p) mutated-strs))
    (reduce #'(lambda (s0 s1)
                (if (< (fitness s0 target)
                       (fitness s1 target))
                    s0
                    s1))
            mutated-strs)))

(defun evolve-gens (target c p)
  (let ((chars " ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
    (do ((parent (random-string chars (length target))
                 (evolve-string target parent chars c p))
         (n 0 (1+ n)))
        ((string= target parent) (format t "Generation ~A: ~S~%" n parent))
      (format t "Generation ~A: ~S~%" n parent))))
```

Sample output:

```txt
CL-USER> (evolve-gens "METHINKS IT IS LIKE A WEASEL" 100 0.05)
Generation 0: "IFNGR ACQNOAWQZYHNIUPLRHTPCP"
Generation 1: "IUNGRHAC NOAWQZYHNIUPLRHTPCP"
Generation 2: "IUNGRHAC YO WQZYHNIUPLRHTPCP"
Generation 3: "IUNGRHKC YO WQZYHNIUPLJHTPRP"
Generation 4: "IUNGRHKC IO WQZYHVIUPLVHTPRP"
Generation 5: "IUNGRNKC IO WQZYHVIUPLVHNPRP"
Generation 6: "IUNGRNKC IO WQZYHVIUPLVHNPRP"
Generation 7: "IENGRNKC IO WQZYHVIUPLVHNPRP"
Generation 8: "IENGRNKC IO WQZYHVEURLVHNPRP"
Generation 9: "IENMRNKC IO WQZYHVE RLVHNPRP"
Generation 10: "IENMRNKC IO WQZYHVE RLVHNPRP"
Generation 11: "IENMRNKC IO WQZYHVE RLVHNPRP"
Generation 12: "IEZMRNKC IO WQZYAVE RLVHNSRP"
Generation 13: "IEZMRNKC IO WQZYIVE RLVHNSRP"
Generation 14: "IEZMRNKC IO WQZYIKE RLVHNSRP"
Generation 15: "IEZMRNKC IO WQZYIKE RLVHNSRL"
Generation 16: "IEZ INKC IZ WQZYIKE RLVHNSRL"
Generation 17: "IET INKC IZ WQZYIKE RLVHNSRL"
Generation 18: "IET INKC IZ WQZYIKE RLVHNSEL"
Generation 19: "IET INKC IZ WQZ IKE RLVHASEL"
Generation 20: "GET INKC IZ WSZ IKE RLVHASEL"
Generation 21: "GET INKC IZ WSZ IKE RLVHASEL"
Generation 22: "GET INKC IZ WSZ IKE RLVHASEL"
Generation 23: "GET INKC IZ ISZ IKE RLVHASEL"
Generation 24: "GET INKC IZ ISZ IKE RLWHASEL"
Generation 25: "MET INKC IZ ISZ IKE OLWHASEL"
Generation 26: "MET INKC IZ ISZ IKE OLWHASEL"
Generation 27: "MET INKC IZ ISZ IKE ALWHASEL"
Generation 28: "MET INKC IZ ISZ IKE A WHASEL"
Generation 29: "METHINKC IZ ISZ IKE A WHASEL"
Generation 30: "METHINKC IZ ISZ IKE A WHASEL"
Generation 31: "METHINKC IZ ISZ IKE A WHASEL"
Generation 32: "METHINKC IZ ISZ IKE A WEASEL"
Generation 33: "METHINKC IZ ISZ IKE A WEASEL"
Generation 34: "METHINKC IZ ISZ IKE A WEASEL"
Generation 35: "METHINKC IT ISZLIKD A WEASEL"
Generation 36: "METHINKC IT ISZLIKD A WEASEL"
Generation 37: "METHINKC IT ISZLIKD A WEASEL"
Generation 38: "METHINKC IT ISZLIKD A WEASEL"
Generation 39: "METHINKC IT ISZLIKD A WEASEL"
Generation 40: "METHINKC IT ISZLIKE A WEASEL"
Generation 41: "METHINKC IT IS LIKE A WEASEL"
Generation 42: "METHINKC IT IS LIKE A WEASEL"
Generation 43: "METHINKS IT IS LIKE A WEASEL"

```


Mutates one character at a time, with only on offspring each generation (which competes against the parent):

```lisp
(defun unfit (s1 s2)
  (loop for a across s1
	for b across s2 count(char/= a b)))

(defun mutate (str alp n) ; n: number of chars to mutate
  (let ((out (copy-seq str)))
    (dotimes (i n) (setf (char out (random (length str)))
			 (char alp (random (length alp)))))
    out))

(defun evolve (changes alpha target)
  (loop for gen from 1
	with f2 with s2
	with str = (mutate target alpha 100)
	with fit = (unfit target str)
	while (plusp fit) do
	(setf s2 (mutate str alpha changes)
	      f2 (unfit target s2))
	(when (> fit f2)
	  (setf str s2 fit f2)
	  (format t "~5d: ~a (~d)~%" gen str fit))))

(evolve 1 " ABCDEFGHIJKLMNOPQRSTUVWXYZ" "METHINKS IT IS LIKE A WEASEL")
```
outupt<lang>   44: DYZTOREXDML ZCEUCSHRVHBEPGJE (26)
   57: DYZTOREXDIL ZCEUCSHRVHBEPGJE (25)
   83: DYZTOREX IL ZCEUCSHRVHBEPGJE (24)
   95: MYZTOREX IL ZCEUCSHRVHBEPGJE (23)
  186: MYZTOREX IL ZCEUISHRVHBEPGJE (22)
  208: MYZTOREX IL ZCEUISH VHBEPGJE (21)
  228: MYZTOREX IL ZCEUISH VHBEPGEE (20)
  329: MYZTOREX IL ZCEUIKH VHBEPGEE (19)
  330: MYTTOREX IL ZCEUIKH VHBEPGEE (18)
  354: MYTHOREX IL ZCEUIKH VHBEPGEE (17)
  365: MYTHOREX IL ICEUIKH VHBEPGEE (16)
  380: MYTHOREX IL ISEUIKH VHBEPGEE (15)
  393: METHOREX IL ISEUIKH VHBEPGEE (14)
  407: METHORKX IL ISEUIKH VHBEPGEE (13)
  443: METHORKX IL ISEUIKH VHBEPSEE (12)
  455: METHORKX IL ISEUIKE VHBEPSEE (11)
  477: METHIRKX IL ISEUIKE VHBEPSEE (10)
  526: METHIRKS IL ISEUIKE VHBEPSEE (9)
  673: METHIRKS IL ISEUIKE VHBEPSEL (8)
  800: METHINKS IL ISEUIKE VHBEPSEL (7)
  875: METHINKS IL ISEUIKE AHBEPSEL (6)
  941: METHINKS IL ISEUIKE AHBEASEL (5)
 1175: METHINKS IT ISEUIKE AHBEASEL (4)
 1214: METHINKS IT ISELIKE AHBEASEL (3)
 1220: METHINKS IT IS LIKE AHBEASEL (2)
 1358: METHINKS IT IS LIKE AHWEASEL (1)
 2610: METHINKS IT IS LIKE A WEASEL (0)
```



## D


```d
import std.stdio, std.random, std.algorithm, std.range, std.ascii;

enum target = "METHINKS IT IS LIKE A WEASEL"d;
enum C = 100;  // Number of children in each generation.
enum P = 0.05; // Mutation probability.
enum fitness = (dchar[] s) => target.zip(s).count!q{ a[0] != a[1] };
dchar rnd() { return (uppercase ~ " ")[uniform(0, $)]; }
enum mut = (dchar[] s) => s.map!(a => uniform01 < P ? rnd : a).array;

void main() {
    auto parent = generate!rnd.take(target.length).array;
    for (auto gen = 1; parent != target; gen++) {
        // parent = parent.repeat(C).map!mut.array.max!fitness;
        parent = parent.repeat(C).map!mut.array
                 .minPos!((a, b) => a.fitness < b.fitness)[0];
        writefln("Gen %2d, dist=%2d: %s", gen, parent.fitness, parent);
    }
}
```

```txt
Generation  0, dist=25: PTJNKPFVJFTDRSDVNUB ESJGU MF
Generation  1, dist=18: PEKNKNKSBFTDISDVIUB ESJEP MF
Generation  2, dist=12: NETVKNKS FTDISDLIUE EIJEPSEF
Generation  3, dist= 8: NETVONKS ITDISDLIUE AIWEASEF
Generation  4, dist= 8: NETVONKS ITDISDLIUE AIWEASEF
Generation  5, dist= 6: NETHONKS ITDIS LINE AIWEASEW
Generation  6, dist= 5: NETHINKS ITSIS LINE AIWEASEW
Generation  7, dist= 5: NETHINKS ITSIS LINE AIWEASEW
Generation  8, dist= 4: NETHINKS ITSIS LINE A WEASEW
Generation  9, dist= 3: METHINKS ITSIS LINE A WEASEW
Generation 10, dist= 3: METHINKS ITSIS LINE A WEASEW
Generation 11, dist= 3: METHINKS ITSIS LINE A WEASEW
Generation 12, dist= 2: METHINKS IT IS LINE A WEASEW
Generation 13, dist= 2: METHINKS IT IS LINE A WEASEW
Generation 14, dist= 1: METHINKS IT IS LIKE A WEASEW
Generation 15, dist= 1: METHINKS IT IS LIKE A WEASEW
Generation 16, dist= 1: METHINKS IT IS LIKE A WEASEW
Generation 17, dist= 1: METHINKS IT IS LIKE A WEASEW
Generation 18, dist= 1: METHINKS IT IS LIKE A WEASEW
Generation 19, dist= 1: METHINKS IT IS LIKE A WEASEW
Generation 20, dist= 1: METHINKS IT IS LIKE A WEASEW
Generation 21, dist= 1: METHINKS IT IS LIKE A WEASEW
Generation 22, dist= 1: METHINKS IT IS LIKE A WEASEW
Generation 23, dist= 1: METHINKS IT IS LIKE A WEASEW
Generation 24, dist= 0: METHINKS IT IS LIKE A WEASEL
```



## E


```e
pragma.syntax("0.9")
pragma.enable("accumulator")

def target := "METHINKS IT IS LIKE A WEASEL"
def alphabet := "ABCDEFGHIJKLMNOPQRSTUVWXYZ "
def C := 100
def RATE := 0.05

def randomCharString() {
  return E.toString(alphabet[entropy.nextInt(alphabet.size())])
}

def fitness(string) {
    return accum 0 for i => ch in string {
      _ + (ch == target[i]).pick(1, 0)
    }
}

def mutate(string, rate) {
  return accum "" for i => ch in string {
    _ + (entropy.nextDouble() < rate).pick(randomCharString(), E.toString(ch))
  }
}

def weasel() {
  var parent := accum "" for _ in 1..(target.size()) { _ + randomCharString() }
  var generation := 0

  while (parent != target) {
    println(`$generation $parent`)
    def copies := accum [] for _ in 1..C { _.with(mutate(parent, RATE)) }
    var best := parent
    for c in copies {
      if (fitness(c) > fitness(best)) {
        best := c
      }
    }
    parent := best
    generation += 1
  }
  println(`$generation $parent`)
}

weasel()
```



## EchoLisp


```scheme

(require 'sequences)
(define ALPHABET (list->vector  ["A" .. "Z"] ))
(vector-push ALPHABET " ")

(define (fitness source target) ;; score >=0, best is 0
	(for/sum  [(s source)(t target)]
		(if (= s t) 0 1)))

(define (mutate source rate)
	(for/string [(s source)]
		(if (< (random) rate) [ALPHABET (random 27)] s)))

(define (select parent target rate copies (copy) (score))
	(define best (fitness parent target))
	(define selected parent)
	(for [(i copies)]
		(set! copy (mutate parent rate))
		(set! score (fitness copy target))
		(when (< score  best)
			(set! selected copy)
			(set! best  score)))
	selected )

(define MUTATION_RATE 0.05) ;; 5% chances to change
(define COPIES 100)
(define TARGET "METHINKS IT IS LIKE A WEASEL")

(define (task (rate MUTATION_RATE) (copies COPIES) (target TARGET) (score))
	(define parent ;; random source
		(for/string
                [(i (string-length target))] [ALPHABET (random 27)]))

	(for [(i (in-naturals))]
		(set! score (fitness parent target))
		(writeln i parent 'score score)
		#:break (zero? score)
		(set! parent (select parent target rate copies))
		))

```

```txt

(task)
0     "TNCEKMNVYOW NSMSZ BZDODMMAXE"     score     26
1     "TNCEKBNVYOW NSMSZ AZDODMMAEE"     score     25
2     "TNCEKINVYOW NSMSZKEZDODMMAEE"     score     23
3     "TNCEKIKVYOW NSMSZKEZDODMMAEE"     score     22
4     "TNCEKIKVYOW NSMSZKEZDOWMMAEE"     score     21
5     "TNCEKIKVYOW NSMSZKEZDOWMMAEE"     score     21
6     "MNCEKIKVYOW NSMSZKEZSOWMMAEE"     score     20
7     "MNCEKIKAYOE NSMLZKEZSOWMMAEE"     score     19
8     "MNCEKIKAYOE NSMLZKEZS WMMAEE"     score     18
9     "MNCEKIKAYOE ISMLZKEZS WMMAEE"     score     17
10     "MECEKIKAYBE ISMLZKEZS WMMAEE"     score     16
11     "MECEKLKAYBE ISMLZKE S WMMAEE"     score     15
12     "METEKZKAYBE ISMLZKE S WMMAEE"     score     14
13     "METEKZKAYBE ISMLZKE S WMMSEE"     score     13
14     "METEIZKAYBE ISMLZKE S WMMSEH"     score     12
15     "METEIZKAYBE ISMLZKE S WMMSEH"     score     12
16     "METHIZKAYBE ISMLZKE S WMMSEH"     score     11
17     "METHIZKAYBE ISMLZKE S WMASEH"     score     10
18     "METHIZKAYBE ISMLZKE S WMASEH"     score     10
[...]
67     "METHINKS RT ISMLIKE A WEASEL"     score     2
68     "METHINKS RT ISMLIKE A WEASEL"     score     2
69     "METHINKS RT ISMLIKE A WEASEL"     score     2
70     "METHINKS RT ISMLIKE A WEASEL"     score     2
71     "METHINKS RT ISMLIKE A WEASEL"     score     2
72     "METHINKS RT IS LIKE A WEASEL"     score     1
73     "METHINKS RT IS LIKE A WEASEL"     score     1
74     "METHINKS RT IS LIKE A WEASEL"     score     1
75     "METHINKS IT IS LIKE A WEASEL"     score     0

```



## Elena

ELENA 4.1 :

```elena
import system'routines;
import extensions;
import extensions'text;

const string Target = "METHINKS IT IS LIKE A WEASEL";
const string AllowedCharacters = " ABCDEFGHIJKLMNOPQRSTUVWXYZ";

const int C = 100;
const real P = 0.05r;

rnd = randomGenerator;

randomChar
    = AllowedCharacters[rnd.nextInt(AllowedCharacters.Length)];

extension evoHelper
{
    randomString()
        = 0.repeatTill(self).selectBy:(x => randomChar).summarize(new StringWriter());

    fitnessOf(s)
        = self.zipBy(s, (a,b => a==b ? 1 : 0)).summarize(new Integer()).toInt();

    mutate(p)
        = self.selectBy:(ch => rnd.nextReal() <= p ? randomChar : ch).summarize(new StringWriter());
}

class EvoAlgorithm : Enumerator
{
    object theTarget;
    object theCurrent;
    object theVariantCount;

    constructor new:of(s,count)
    {
        theTarget := s;
        theVariantCount := count.toInt();
    }

    get() = theCurrent;

    bool next()
    {
        if (nil == theCurrent)
            { theCurrent := theTarget.Length.randomString(); ^ true };

        if (theTarget == theCurrent)
            { ^ false };

        auto variants := Array.allocate(theVariantCount).populate:(x => theCurrent.mutate:P );

        theCurrent := variants.sort:(a,b => a.fitnessOf:Target > b.fitnessOf:Target ).at:0;

        ^ true
    }

    reset()
    {
        theCurrent := nil
    }

    enumerable() => theTarget;
}

public program()
{
    var attempt := new Integer();
    EvoAlgorithm.new:Target &of:C.forEach:(current)
    {
        console
            .printPaddingLeft(10,"#",attempt.append(1))
            .printLine(" ",current," fitness: ",current.fitnessOf(Target))
    };

    console.readChar()
}
```

```txt

        #1 WYHOOITVJKCPTOOTEVZJUNLCFDCV fitness: 0
        #2 WYHOOITV KCPTOOTEVZJUNLCFDCV fitness: 1
        #3 WYHOOITS KCPTOCTEVZ UNLCFDCV fitness: 3
        #4 WYHO ITS KCPTO TEVZ UELCFDCV fitness: 4
        #5 WYGO ITS DC ZO TEVZ UELCFDCV fitness: 5
        #6 WYGO ITS DC ZO TEVZ UELCADCV fitness: 6
        #7 WYGO ITS DT ZO TEVZ UELCADCV fitness: 7
        #8 WYGOIITS DT ZO TEVZ LELCADRV fitness: 8
        #9 WYGOIITS DT ZO TEVZ LELCADRL fitness: 9
       #10 WYTOIITS HT ZZ TEVZ LEQCADRL fitness: 10
       #11 WYTOIITS HT ZZ IEKZ LEQCADRL fitness: 11
       #12 WYTOIITS HT ZZ IEKZ LEQCADEL fitness: 12
       #13 WYTOIITS HT ZZ IEKZ LEQCASEL fitness: 13
       #14 WYTOIIKS HT BZ IEKZ LEQCASEL fitness: 14
...
       #34 METHINKS GT BS LGKE AEWGASEL fitness: 23
       #35 METHINKS GT BS LIKE AEWGASEL fitness: 24
       #36 METHINKS GT BS LIKE AEWGASEL fitness: 24
       #37 METHINKS GT BS LIKE AEWGASEL fitness: 24
       #38 METHINKS GT BS LIKE AEWGASEL fitness: 24
       #39 METHINKS GT IS LIKE AEWYASEL fitness: 25
       #40 METHINKS GT IS LIKE AEWYASEL fitness: 25
       #41 METHINKS GT IS LIKE AEWEASEL fitness: 26
       #42 METHINKS GT IS LIKE AEWEASEL fitness: 26
       #43 METHINKS GT IS LIKE AEWEASEL fitness: 26
       #44 METHINKS GT IS LIKE AEWEASEL fitness: 26
       #45 METHINKS GT IS LIKE AEWEASEL fitness: 26
       #46 METHINKS GT IS LIKE AEWEASEL fitness: 26
       #47 METHINKS GT IS LIKE AEWEASEL fitness: 26
...
       #57 METHINKS GT IS LIKE A WEASEL fitness: 27
       #58 METHINKS GT IS LIKE A WEASEL fitness: 27
       #59 METHINKS GT IS LIKE A WEASEL fitness: 27
       #60 METHINKS GT IS LIKE A WEASEL fitness: 27
       #61 METHINKS GT IS LIKE A WEASEL fitness: 27
       #62 METHINKS GT IS LIKE A WEASEL fitness: 27
       #63 METHINKS GT IS LIKE A WEASEL fitness: 27
       #64 METHINKS LT IS LIKE A WEASEL fitness: 27
       #65 METHINKS LT IS LIKE A WEASEL fitness: 27
       #66 METHINKS LT IS LIKE A WEASEL fitness: 27
       #67 METHINKS LT IS LIKE A WEASEL fitness: 27
       #68 METHINKS LT IS LIKE A WEASEL fitness: 27
       #69 METHINKS LT IS LIKE A WEASEL fitness: 27
       #70 METHINKS LT IS LIKE A WEASEL fitness: 27
       #71 METHINKS IT IS LIKE A WEASEL fitness: 28

```



## Elixir

Print current gen and most fit offspring if more fit than parent.

Print the target and the total number of generations (iterations) it took to reach it.


```Elixir
defmodule Log do
  def show(offspring,i) do
    IO.puts "Generation: #{i}, Offspring: #{offspring}"
  end

  def found({target,i}) do
    IO.puts "#{target} found in #{i} iterations"
  end
end

defmodule Evolution do
  # char list from A to Z; 32 is the ord value for space.
  @chars  [32 | Enum.to_list(?A..?Z)]

  def select(target) do
    (1..String.length(target)) # Creates parent for generation 0.
      |> Enum.map(fn _-> Enum.random(@chars) end)
      |> mutate(to_charlist(target),0)
      |> Log.found
  end

  # w is used to denote fitness in population genetics.

  defp mutate(parent,target,i) when target == parent, do: {parent,i}
  defp mutate(parent,target,i) do
    w = fitness(parent,target)
    prev = reproduce(target,parent,mu_rate(w))

    # Check if the most fit member of the new gen has a greater fitness than the parent.
    if w < fitness(prev,target) do
      Log.show(prev,i)
      mutate(prev,target,i+1)
    else
      mutate(parent,target,i+1)
    end
  end

  # Generate 100 offspring and select the one with the greatest fitness.

  defp reproduce(target,parent,rate) do
    [parent | (for _ <- 1..100, do: mutation(parent,rate))]
      |> Enum.max_by(fn n -> fitness(n,target) end)
  end

  # Calculate fitness by checking difference between parent and offspring chars.

  defp fitness(t,r) do
    Enum.zip(t,r)
      |> Enum.reduce(0, fn {tn,rn},sum -> abs(tn - rn) + sum end)
      |> calc
  end

  # Generate offspring based on parent.

  defp mutation(p,r) do
    # Copy the parent chars, then check each val against the random mutation rate
    Enum.map(p, fn n -> if :rand.uniform <= r, do: Enum.random(@chars), else: n end)
  end

  defp calc(sum),  do: 100 * :math.exp(sum/-10)
  defp mu_rate(n), do: 1   - :math.exp(-(100-n)/400)
end

Evolution.select("METHINKS IT IS LIKE A WEASEL")
```


```txt
Generation: 0, Offspring: AFOSPRRLTLF CQKYFIGUMEUVBLRN
Generation: 1, Offspring: HFOMJRRESLL FQKYQRGUM UVBLRN
Generation: 2, Offspring: HFOMCRLIDLL FDKYQRGNM UVBLIN
Generation: 3, Offspring: HFOMCOLIDQL FDKYQRG M UVBLIP
Generation: 4, Offspring: HFOMCOLVLRL FD YYRG M UEBLIP
Generation: 5, Offspring: HFOMCOLVLRL FS YYNH M UEBXJP
Generation: 6, Offspring: KFOMCOLVLRL FS YYNH C UEBXJP
Generation: 7, Offspring: EFOFCOCVLFT FV YCNH C UEBMJP
Generation: 8, Offspring: EFWFCOCV FTBFV YCSH C UEBMJP
Generation: 9, Offspring: EFWFJOCZ FTBRV DCMH C UEBMJP
Generation: 11, Offspring: PFSFJOCL FVBRV DCMH C UEBJJP
Generation: 12, Offspring: PFSDJYCL LV RK DKMH C UEBJJR
Generation: 13, Offspring: IFSDJYCP LV MK DKMH C UEBSJR
Generation: 14, Offspring: IFSDJTIP LV MK DKMH C UEBSGR
Generation: 15, Offspring: IFSDJTIO JV MK SKMH C UEBSGR
Generation: 16, Offspring: IFSKIJIO JV MK DKMH C UEBSGG
Generation: 19, Offspring: IFSJIJIP JV MK DKIH C UEBSGH
Generation: 20, Offspring: IFSJIJIP JV MO DMIH C UEBSGH
Generation: 21, Offspring: IFWJDJIP JV IO EHJH C UEBSGH
Generation: 23, Offspring: IFWJDJIP JV IO SHJH A XEBSGH
Generation: 25, Offspring: IFWJDJIP JV IO SHJC A XEBSGH
Generation: 26, Offspring: IFWJKJIP JV IO LHJC A XEBOGH
Generation: 34, Offspring: IFTJKJIT JV IO LHJC A XEBOGH
Generation: 39, Offspring: IFTJKOIT JV IO LHJC A XEBOGH
Generation: 53, Offspring: IETJKOIT JV IO LHJC A XEBOGH
Generation: 60, Offspring: IETJKOIT JV IO LHJC A XEBOEG
Generation: 64, Offspring: IETJKOIT JV IO LHJF A XEBOEG
Generation: 68, Offspring: LETGKOIT JV IO LHJF A XEBOEG
Generation: 70, Offspring: LETGKOIT JV IS LHJF A XEBOBG
Generation: 76, Offspring: LETEKOIT JV IS LHJF A XEBOBN
Generation: 83, Offspring: LETHKOIT JV IS LHJF A XEBOFN
Generation: 90, Offspring: LBTHKOIT JV IS LHJF A XEBSFN
Generation: 92, Offspring: LBTHKOIT JV IS LHJF A XEBSFL
Generation: 93, Offspring: LBTHKOJT JV IS LHJF A XEBSFL
Generation: 123, Offspring: LETHKOJT JV IS LHJF A XEBSFL
Generation: 125, Offspring: LETHHOJT JV IS LHJF A XEBSFL
Generation: 135, Offspring: LETHHOJT JV IS LIJF A XEBSFL
Generation: 143, Offspring: LETHHOJT IV IS LIJF A XEBSFL
Generation: 161, Offspring: LETHHNJT IV IS LIJF A XEBSFL
Generation: 165, Offspring: METHHNJT IV IS LIJF A XEBSFL
Generation: 169, Offspring: METHHNKT IV IS LIJF A XEBSFL
Generation: 171, Offspring: METHHNKT IV IS LIJE A XEBSFL
Generation: 175, Offspring: METHHNKT IS IS LIJE A XEBSFL
Generation: 213, Offspring: METHHNKT IS IS LIKE A XEBSFL
Generation: 218, Offspring: METHINKT IS IS LIKE A XEBSFL
Generation: 234, Offspring: METHINKT IS IS LIKE A XEBSEL
Generation: 237, Offspring: METHINKT IS IS LIKE A XEASEL
Generation: 241, Offspring: METHINKT IS IS LIKE A WEASEL
Generation: 243, Offspring: METHINKT IT IS LIKE A WEASEL
Generation: 247, Offspring: METHINKS IT IS LIKE A WEASEL
METHINKS IT IS LIKE A WEASEL found in 248 iterations

```



## Erlang


```erlang
-module(evolution).
-export([run/0]).

-define(MUTATE, 0.05).
-define(POPULATION, 100).
-define(TARGET, "METHINKS IT IS LIKE A WEASEL").
-define(MAX_GENERATIONS, 1000).

run() -> evolve_gens().

evolve_gens() ->
    Initial = random_string(length(?TARGET)),
    evolve_gens(Initial,0,fitness(Initial)).
evolve_gens(Parent,Generation,0) ->
    io:format("Generation[~w]: Achieved the target: ~s~n",[Generation,Parent]);
evolve_gens(Parent,Generation,_Fitness) when Generation == ?MAX_GENERATIONS ->
    io:format("Reached Max Generations~nFinal string is ~s~n",[Parent]);
evolve_gens(Parent,Generation,Fitness) ->
    io:format("Generation[~w]: ~s, Fitness: ~w~n",
              [Generation,Parent,Fitness]),
    Child = evolve_string(Parent),
    evolve_gens(Child,Generation+1,fitness(Child)).

fitness(String) -> fitness(String, ?TARGET).
fitness([],[]) -> 0;
fitness([H|Rest],[H|Target]) -> fitness(Rest,Target);
fitness([_H|Rest],[_T|Target]) -> 1+fitness(Rest,Target).

mutate(String) -> mutate(String,[]).
mutate([],Acc) -> lists:reverse(Acc);
mutate([H|T],Acc) ->
    case random:uniform() < ?MUTATE of
        true ->
            mutate(T,[random_character()|Acc]);
        false ->
            mutate(T,[H|Acc])
    end.

evolve_string(String) ->
    evolve_string(String,?TARGET,?POPULATION,String).
evolve_string(_,_,0,Child) -> Child;
evolve_string(Parent,Target,Population,Best_Child) ->
    Child = mutate(Parent),
    case fitness(Child) < fitness(Best_Child) of
        true ->
            evolve_string(Parent,Target,Population-1,Child);
        false ->
            evolve_string(Parent,Target,Population-1,Best_Child)
    end.

random_character() ->
    case random:uniform(27)-1 of
        26  -> $ ;
        R -> $A+R
    end.

random_string(Length) -> random_string(Length,[]).
random_string(0,Acc) -> Acc;
random_string(N,Acc) when N > 0 ->
    random_string(N-1,[random_character()|Acc]).


```



## Euphoria


```euphoria
constant table = "ABCDEFGHIJKLMNOPQRSTUVWXYZ "
function random_generation(integer len)
    sequence s
    s = rand(repeat(length(table),len))
    for i = 1 to len do
        s[i] = table[s[i]]
    end for
    return s
end function

function mutate(sequence s, integer n)
    for i = 1 to length(s) do
        if rand(n) = 1 then
            s[i] = table[rand(length(table))]
        end if
    end for
    return s
end function

function fitness(sequence probe, sequence target)
    atom sum
    sum = 0
    for i = 1 to length(target) do
        sum += power(find(target[i], table) - find(probe[i], table), 2)
    end for
    return sqrt(sum/length(target))
end function

constant target = "METHINKS IT IS LIKE A WEASEL", C = 30, MUTATE = 15
sequence parent, specimen
integer iter, best
atom fit, best_fit
parent = random_generation(length(target))
iter = 0
while not equal(parent,target) do
    best_fit = fitness(parent, target)
    printf(1,"Iteration: %3d, \"%s\", deviation %g\n", {iter, parent, best_fit})
    specimen = repeat(parent,C+1)
    best = C+1
    for i = 1 to C do
        specimen[i] = mutate(specimen[i], MUTATE)
        fit = fitness(specimen[i], target)
        if fit < best_fit then
            best_fit = fit
            best = i
        end if
    end for
    parent = specimen[best]
    iter += 1
end while
printf(1,"Finally, \"%s\"\n",{parent})
```


Output:

```txt
Iteration:   0, "HRGPWKOOARZL KTJEBPUYPTOLGDK", deviation 11.1002
Iteration:   1, "HRGPWKOOWRZLLKTJEBPUYPTOLGDK", deviation 9.40175
Iteration:   2, "HRGPOKOOWRZVLKTJEBPUYPTOLGDK", deviation 8.69113
Iteration:   3, "HRKPOKOOWRZVLKTJEBPUDPTOLGDB", deviation 7.46181
Iteration:   4, "HEKPOKOOWRZVLKTJEBPUDPTOLGDB", deviation 7.04577
Iteration:   5, "HEKPOKOOWRZVLKTJEBEUDPTOLGDB", deviation 6.73212
Iteration:   6, "HEKPOKOOWRZVLKTJEBEUDPTALGDB", deviation 6.50549
Iteration:   7, "HEKPOKOOWIZVLKTJEBEUDPTALGDB", deviation 6.27922
Iteration:   8, "HESPOKOOWIZVLKTJEBEUDPTALJDB", deviation 5.85845
Iteration:   9, "HESPOKOOWIZVLKTJEBEUIPTALJDJ", deviation 5.73212
...
Iteration: 201, "METHINKS IT IT LIKE A WEASEL", deviation 0.188982
Iteration: 202, "METHINKS IT IT LIKE A WEASEL", deviation 0.188982
Iteration: 203, "METHINKS IT IT LIKE A WEASEL", deviation 0.188982
Iteration: 204, "METHINKS IT IT LIKE A WEASEL", deviation 0.188982
Iteration: 205, "METHINKS IT IT LIKE A WEASEL", deviation 0.188982
Iteration: 206, "METHINKS IT IT LIKE A WEASEL", deviation 0.188982
Iteration: 207, "METHINKS IT IT LIKE A WEASEL", deviation 0.188982
Iteration: 208, "METHINKS IT IT LIKE A WEASEL", deviation 0.188982
Iteration: 209, "METHINKS IT IT LIKE A WEASEL", deviation 0.188982
Iteration: 210, "METHINKS IT IT LIKE A WEASEL", deviation 0.188982
Finally, "METHINKS IT IS LIKE A WEASEL"

```


=={{header|F Sharp|F#}}==

```fsharp

//A functional implementation of Evolutionary algorithm
//Nigel Galloway February 7th., 2018
let G=System.Random 23
let fitness n=Array.fold2(fun a n g->if n=g then a else a+1) 0 n ("METHINKS IT IS LIKE A WEASEL".ToCharArray())
let alphabet="QWERTYUIOPASDFGHJKLZXCVBNM ".ToCharArray()
let mutate (n:char[]) g=Array.iter(fun g->n.[g]<-alphabet.[G.Next()%27]) (Array.init g (fun _->G.Next()%(Array.length n)));n
let nextParent n g=List.init 500 (fun _->mutate (Array.copy n) g)|>List.minBy fitness
let evolution n=let rec evolution n g=match fitness n with |0->(0,n)::g |l->evolution (nextParent n ((l/2)+1)) ((l,n)::g)
                evolution n []
let n = evolution (Array.init 28 (fun _->alphabet.[G.Next()%27]))

```

Real: 00:00:00.021, CPU: 00:00:00.050, GC gen0: 1, gen1: 0


Length of n (37) is the number of generations including the original parent as follows:

```txt

Length of n (37) is the number of generations including the original parent as follows:
(28, [|' '; 'V'; 'L'; 'D'; 'N'; 'Q'; 'A'; 'Z'; 'P'; 'A'; 'J'; 'A'; 'T'; 'C'; 'S'; 'I'; 'G'; 'H'; 'M'; 'Q'; 'M'; 'J'; 'Y'; 'L'; 'Q'; 'H'; 'S'; 'A'|])
(25, [|'D'; 'V'; 'L'; 'B'; 'N'; 'S'; 'A'; 'Z'; 'B'; 'A'; 'J'; 'Y'; 'T'; 'M'; 'U'; 'L'; 'G'; 'M'; 'M'; 'Q'; 'M'; ' '; 'Y'; 'L'; 'Q'; S'; 'X'; 'Y'|])
(23, [|'V'; 'E'; 'L'; 'A'; 'N'; 'S'; 'A'; 'Z'; 'P'; 'A'; 'J'; ' '; 'T'; 'M'; 'L'; 'L'; 'G'; 'D'; 'M'; 'Z'; 'S'; ' '; 'A'; 'L'; 'L'; 'S'; 'X'; 'Y'|])
(21, [|'V'; 'S'; 'L'; 'J'; 'N'; 'S'; 'A'; 'S'; 'P'; 'A'; 'J'; ' '; ' '; 'M'; 'L'; 'L'; 'G'; 'D'; 'E'; 'I'; 'A'; ' '; 'A'; 'L'; 'L'; 'S'; 'X'; 'Y'|])
(20, [|'V'; 'S'; 'E'; 'H'; 'N'; ' '; 'A'; 'S'; 'P'; 'S'; 'J'; ' '; 'Z'; 'P'; 'L'; 'L'; 'G'; 'B'; 'E'; 'Y'; 'A'; ' '; 'D'; 'H'; 'V'; 'S'; 'X'; 'Y'|])
(18, [|'V'; 'S'; 'K'; 'H'; 'N'; ' '; 'K'; 'S'; 'M'; 'S'; 'J'; ' '; 'I'; 'P'; 'V'; 'L'; 'D'; 'B'; 'E'; 'Y'; 'A'; ' '; 'X'; 'J'; 'V'; 'S'; 'X'; 'Y'|])
(16, [|'W'; 'S'; 'K'; 'H'; 'N'; ' '; 'K'; 'S'; 'M'; 'S'; 'D'; ' '; 'I'; 'S'; 'V'; 'L'; 'D'; 'T'; 'E'; ' '; 'A'; ' '; 'C'; 'J'; 'V'; 'S'; 'W'; 'Y'|])
(14, [|'W'; 'E'; 'K'; 'H'; 'X'; 'G'; 'K'; 'S'; 'M'; 'H'; 'D'; ' '; 'I'; 'S'; 'V'; 'L'; ' '; 'T'; 'E'; ' '; 'A'; ' '; 'C'; 'J'; 'R'; 'S'; 'W'; 'L'|])
(14, [|'W'; 'E'; 'E'; 'H'; 'I'; 'L'; 'K'; 'S'; 'M'; 'H'; 'D'; 'W'; 'I'; 'S'; 'O'; 'L'; 'M'; 'A'; 'E'; ' '; 'A'; ' '; 'Q'; 'J'; 'R'; 'S'; 'W'; 'L'|])
(13, [|'W'; 'E'; 'E'; 'H'; 'I'; 'L'; 'K'; 'S'; 'M'; 'H'; 'D'; 'W'; 'I'; 'S'; 'R'; 'L'; 'S'; 'A'; 'E'; ' '; 'A'; ' '; 'Q'; 'J'; 'Z'; 'S'; 'E'; 'L'|])
(12, [|'W'; 'E'; 'E'; 'H'; 'I'; 'L'; 'K'; 'S'; 'M'; 'H'; 'D'; 'O'; 'I'; 'S'; 'C'; 'L'; 'I'; 'Y'; 'E'; ' '; 'A'; ' '; 'J'; 'O'; 'R'; 'S'; 'E'; 'L'|])
(10, [|'B'; 'E'; 'A'; 'H'; 'I'; 'N'; 'K'; 'S'; 'M'; 'C'; 'T'; 'O'; 'I'; 'S'; 'C'; 'L'; 'I'; 'R'; 'E'; ' '; 'A'; ' '; 'J'; 'O'; 'R'; 'S'; 'E'; 'L'|])
(9, [|'M'; 'E'; 'A'; 'H'; 'I'; 'N'; 'K'; 'S'; 'N'; 'C'; 'T'; 'F'; 'I'; 'S'; 'C'; 'L'; 'I'; 'R'; 'E'; ' '; 'A'; ' '; 'K'; 'N'; 'R'; 'S'; 'E'; 'L'|])
(9, [|'M'; 'E'; 'A'; 'H'; 'I'; 'N'; 'K'; 'S'; 'T'; 'P'; 'T'; 'F'; 'I'; 'S'; 'C'; 'L'; 'I'; 'R'; 'E'; ' '; 'A'; ' '; 'K'; 'N'; 'P'; 'S'; 'E'; 'L'|])
(8, [|'M'; 'E'; 'N'; 'H'; 'I'; 'N'; 'K'; 'S'; 'L'; 'P'; 'T'; 'F'; 'I'; 'S'; 'Y'; 'L'; 'I'; 'K'; 'E'; ' '; 'A'; ' '; 'K'; 'H'; 'P'; 'S'; 'E'; 'L'|])
(8, [|'M'; 'E'; 'N'; 'H'; 'I'; 'N'; 'K'; 'S'; 'L'; 'E'; 'T'; 'F'; 'I'; 'R'; 'Y'; 'L'; 'I'; 'K'; 'E'; ' '; 'A'; ' '; 'Q'; 'H'; 'A'; 'S'; 'E'; 'L'|])
(7, [|'M'; 'E'; ' '; 'H'; 'I'; 'N'; 'K'; 'S'; ' '; 'E'; 'T'; 'F'; 'I'; 'K'; 'Y'; 'L'; 'I'; 'K'; 'E'; ' '; 'A'; ' '; 'Q'; 'H'; 'A'; 'S'; 'E'; 'L'|])
(7, [|'M'; 'E'; ' '; 'H'; 'I'; 'N'; 'K'; 'S'; ' '; 'E'; 'T'; 'F'; 'I'; 'K'; 'J'; 'L'; 'I'; 'K'; 'E'; ' '; 'A'; ' '; 'Q'; 'H'; 'A'; 'S'; 'E'; 'L'|])
(6, [|'M'; 'E'; 'T'; 'H'; 'I'; 'N'; 'K'; 'S'; ' '; 'I'; 'T'; 'F'; 'I'; 'K'; 'J'; 'L'; 'I'; 'D'; 'E'; ' '; 'A'; ' '; 'Q'; 'Z'; 'A'; 'S'; 'E'; 'L'|])
(5, [|'M'; 'E'; 'T'; 'H'; 'I'; 'N'; 'K'; 'S'; ' '; 'I'; 'T'; ' '; 'I'; 'E'; 'J'; 'L'; 'I'; 'T'; 'E'; ' '; 'A'; ' '; 'X'; 'Z'; 'A'; 'S'; 'E'; 'L'|])
(5, [|'M'; 'E'; 'T'; 'H'; 'I'; 'F'; 'K'; 'S'; ' '; 'I'; 'T'; ' '; 'I'; 'S'; 'I'; 'L'; 'I'; 'T'; 'E'; ' '; 'A'; ' '; 'X'; 'Z'; 'A'; 'S'; 'E'; 'L'|])
(5, [|'M'; 'E'; 'T'; 'H'; 'I'; 'F'; 'K'; 'S'; ' '; 'I'; 'T'; ' '; 'I'; 'S'; ' '; 'L'; 'I'; 'T'; 'E'; ' '; 'A'; ' '; 'K'; 'Z'; 'A'; 'Z'; 'E'; 'L'|])
(5, [|'M'; 'E'; 'T'; 'H'; 'I'; 'F'; 'K'; 'S'; ' '; 'I'; 'T'; ' '; 'I'; 'S'; ' '; 'L'; 'I'; 'F'; 'E'; ' '; 'A'; ' '; 'K'; 'Z'; 'A'; 'P'; 'E'; 'L'|])
(5, [|'M'; 'E'; 'T'; 'H'; 'I'; 'R'; 'K'; 'S'; ' '; 'I'; 'T'; ' '; 'I'; 'S'; ' '; 'L'; 'I'; 'F'; 'E'; ' '; 'A'; ' '; 'K'; 'Z'; 'A'; 'F'; 'E'; 'L'|])
(4, [|'M'; 'E'; 'T'; 'H'; 'I'; 'N'; 'K'; 'S'; ' '; 'I'; 'T'; ' '; 'I'; 'S'; ' '; 'L'; 'I'; 'F'; 'E'; ' '; 'A'; ' '; 'K'; 'Z'; 'A'; 'F'; 'E'; 'L'|])
(3, [|'M'; 'E'; 'T'; 'H'; 'I'; 'N'; 'K'; 'S'; ' '; 'I'; 'T'; ' '; 'I'; 'S'; ' '; 'L'; 'I'; 'J'; 'E'; ' '; 'A'; ' '; 'K'; 'E'; 'A'; 'F'; 'E'; 'L'|])
(3, [|'M'; 'E'; 'T'; 'H'; 'I'; 'N'; 'K'; 'S'; ' '; 'I'; 'T'; ' '; 'I'; 'S'; ' '; 'L'; 'I'; 'J'; 'E'; ' '; 'A'; ' '; 'Y'; 'E'; 'A'; 'F'; 'E'; 'L'|])
(3, [|'M'; 'E'; 'T'; 'H'; 'I'; 'N'; 'K'; 'S'; ' '; 'I'; 'T'; ' '; 'I'; 'S'; ' '; 'L'; 'I'; 'J'; 'E'; ' '; 'A'; ' '; 'K'; 'E'; 'A'; 'G'; 'E'; 'L'|])
(3, [|'M'; 'E'; 'T'; 'H'; 'I'; 'N'; 'K'; 'S'; ' '; 'I'; 'T'; ' '; 'I'; 'S'; ' '; 'L'; 'I'; 'A'; 'E'; ' '; 'A'; ' '; 'K'; 'E'; 'A'; 'G'; 'E'; 'L'|])
(2, [|'M'; 'E'; 'T'; 'H'; 'I'; 'N'; 'K'; 'S'; ' '; 'I'; 'T'; ' '; 'I'; 'S'; ' '; 'L'; 'I'; 'K'; 'E'; ' '; 'A'; ' '; 'K'; 'E'; 'A'; 'Q'; 'E'; 'L'|])
(2, [|'M'; 'E'; 'T'; 'H'; 'I'; 'N'; 'K'; 'S'; ' '; 'I'; 'T'; ' '; 'I'; 'S'; ' '; 'L'; 'I'; 'K'; 'E'; ' '; 'A'; ' '; 'K'; 'E'; 'A'; 'Q'; 'E'; 'L'|])
(2, [|'M'; 'E'; 'T'; 'H'; 'I'; 'N'; 'K'; 'S'; ' '; 'I'; 'T'; ' '; 'I'; 'S'; ' '; 'L'; 'I'; 'K'; 'E'; ' '; 'A'; ' '; 'K'; 'E'; 'A'; 'N'; 'E'; 'L'|])
(1, [|'M'; 'E'; 'T'; 'H'; 'I'; 'N'; 'K'; 'S'; ' '; 'I'; 'T'; ' '; 'I'; 'S'; ' '; 'L'; 'I'; 'K'; 'E'; ' '; 'A'; ' '; 'K'; 'E'; 'A'; 'S'; 'E'; 'L'|])
(1, [|'M'; 'E'; 'T'; 'H'; 'I'; 'N'; 'K'; 'S'; ' '; 'I'; 'T'; ' '; 'I'; 'S'; ' '; 'L'; 'I'; 'K'; 'E'; ' '; 'A'; ' '; 'K'; 'E'; 'A'; 'S'; 'E'; 'L'|])
(1, [|'M'; 'E'; 'T'; 'H'; 'I'; 'N'; 'K'; 'S'; ' '; 'I'; 'T'; ' '; 'I'; 'S'; ' '; 'L'; 'I'; 'K'; 'E'; ' '; 'A'; ' '; 'N'; 'E'; 'A'; 'S'; 'E'; 'L'|])
(1, [|'M'; 'E'; 'T'; 'H'; 'I'; 'N'; 'K'; 'S'; ' '; 'I'; 'T'; ' '; 'I'; 'S'; ' '; 'L'; 'I'; 'K'; 'E'; ' '; 'A'; ' '; 'H'; 'E'; 'A'; 'S'; 'E'; 'L'|])
(0, [|'M'; 'E'; 'T'; 'H'; 'I'; 'N'; 'K'; 'S'; ' '; 'I'; 'T'; ' '; 'I'; 'S'; ' '; 'L'; 'I'; 'K'; 'E'; ' '; 'A'; ' '; 'W'; 'E'; 'A'; 'S'; 'E'; 'L'|])

```



## Factor


```factor
USING: arrays formatting io kernel literals math prettyprint
random sequences strings ;
FROM: math.extras => ... ;
IN: rosetta-code.evolutionary-algorithm

CONSTANT: target "METHINKS IT IS LIKE A WEASEL"
CONSTANT: mutation-rate 0.1
CONSTANT: num-children 25
CONSTANT: valid-chars
    $[ CHAR: A ... CHAR: Z >array { 32 } append ]

: rand-char ( -- n )
    valid-chars random ;

: new-parent ( -- str )
    target length [ rand-char ] replicate >string ;

: fitness ( str -- n )
    target [ = ] { } 2map-as sift length ;

: mutate ( str rate -- str/str' )
    [ random-unit > [ drop rand-char ] when ] curry map ;

: next-parent ( str -- str/str' )
    dup [ mutation-rate mutate ] curry num-children 1 - swap
    replicate [ 1array ] dip append [ fitness ] supremum-by ;

: print-parent ( str -- )
    [ fitness pprint bl ] [ print ] bi ;

: main ( -- )
    0 new-parent
    [ dup target = ]
    [ next-parent dup print-parent [ 1 + ] dip ] until drop
    "Finished in %d generations." printf ;

MAIN: main
```

```txt

1 JWTBPZMHKOFFWDSBCLZUCFUAWUJ
2 JWTAPFMSKOFFWDSBCLZUCHUAWUJ
3 JWTAPSOSKOFFWDOBFLZ CHGAWUJ
...
14 MWTTISKS EFFWS LIKE JZGAWBKL
...
28 METHINKS IT IS LIKE A WEASEL
Finished in 298 generations.

```



## Fantom



```fantom

class Main
{
  static const Str target := "METHINKS IT IS LIKE A WEASEL"
  static const Int C := 100     // size of population
  static const Float p := 0.1f  // chance any char is mutated

  // compute distance of str from target
  static Int fitness (Str str)
  {
    Int sum := 0
    str.each |Int c, Int index|
    {
      if (c != target[index]) sum += 1
    }
    return sum
  }

  // mutate given parent string
  static Str mutate (Str str)
  {
    Str result := ""
    str.size.times |Int index|
    {
      result += ((Float.random < p) ? randomChar() : str[index]).toChar
    }
    return result
  }

  // return a random char
  static Int randomChar ()
  {
    "ABCDEFGHIJKLMNOPQRSTUVWXYZ "[Int.random(0..26)]
  }

  // make population by mutating parent and sorting by fitness
  static Str[] makePopulation (Str parent)
  {
    Str[] result := [,]
    C.times { result.add (mutate(parent)) }
    result.sort |Str a, Str b -> Int| { fitness(a) <=> fitness(b) }
    return result
  }

  public static Void main ()
  {
    Str parent := ""
    target.size.times { parent += randomChar().toChar }

    while (parent != target)
    {
      echo (parent)
      parent = makePopulation(parent).first
    }
    echo (parent)
  }
}

```


=={{header|Fōrmulæ}}==

In [https://wiki.formulae.org/Evolutionary_algorithm this] page you can see the solution of this task.

Fōrmulæ programs are not textual, visualization/edition of programs is done showing/manipulating structures but not text ([http://wiki.formulae.org/Editing_F%C5%8Drmul%C3%A6_expressions more info]). Moreover, there can be multiple visual representations of the same program. Even though it is possible to have textual representation &mdash;i.e. XML, JSON&mdash; they are intended for transportation effects more than visualization and edition.

The option to show Fōrmulæ programs and their results is showing images. Unfortunately images cannot be uploaded in Rosetta Code.


## Forth

```forth
include lib/choose.4th
                                       \ target string
s" METHINKS IT IS LIKE A WEASEL" sconstant target

27 constant /charset                   \ size of characterset
29 constant /target                    \ size of target string
32 constant #copies                    \ number of offspring

/target string charset                 \ characterset
/target string this-generation         \ current generation and offspring
/target #copies [*] string new-generation

:this new-generation does> swap /target chars * + ;
                                       \ generate a mutation
: mutation charset /charset choose chars + c@ ;
                                       \ print the current candidate
: .candidate                           ( n1 n2 -- n1 f)
  ." Generation " over 2 .r ." : " this-generation count type cr /target -1 [+] =
;                                      \ test a candidate on
                                       \ THE NUMBER of correct genes
: test-candidate                       ( a -- a n)
  dup target 0 >r >r                   ( a1 a2)
  begin                                ( a1 a2)
    r@                                 ( a1 a2 n)
  while                                ( a1 a2)
    over c@ over c@ =                  ( a1 a2 n)
    r> r> rot if 1+ then >r 1- >r      ( a1 a2)
    char+ swap char+ swap              ( a1+1 a2+1)
  repeat                               ( a1+1 a2+1)
  drop drop r> drop r>                 ( a n)
;
                                       \ find the best candidate
: get-candidate                        ( -- n)
  #copies 0 >r >r                      ( --)
  begin                                ( --)
    r@                                 ( n)
  while                                ( --)
    r@ 1- new-generation               ( a)
    test-candidate r'@ over <          ( a n f)
    if swap count this-generation place r> 1- swap r> drop >r >r
    else drop drop r> 1- >r then       ( --)
  repeat                               ( --)
  r> drop r>                           ( n)
;
                                       \ generate a new candidate
: make-candidate                       ( a --)
  dup charset count rot place          ( a1)
  this-generation target >r            ( a1 a2 a3)
  begin                                ( a1 a2 a3)
    r@                                 ( a1 a2 a3 n)
  while                                ( a1 a2 a3)
    over c@ over c@ =                  ( a1 a2 a3 f)
    swap >r >r over r>                 ( a1 a2 a1 f)
    if over c@ else mutation then      ( a1 a2 a1 c)
    swap c! r> r> 1- >r                ( a1 a2 a3)
    char+ rot char+ rot char+ rot      ( a1+1 a2+1 a3+1)
  repeat                               ( a1+1 a2+1 a3+1)
  drop drop drop r> drop               ( --)
;
                                       \ make a whole new generation
: make-generation #copies 0 do i new-generation make-candidate loop ;
                                       \ weasel program
: weasel
  s"  ABCDEFGHIJKLMNOPQRSTUVWXYZ " 2dup
  charset place                        \ initialize the characterset
  this-generation place 0              \ initialize the first generation
  begin                                \ start the program
    1+ make-generation                 \ make a new generation
    get-candidate .candidate           \ select the best candidate
  until drop                           \ stop when we've found perfection
;

weasel
```

Output:

```txt

habe@linux-471m:~> 4th cxq weasel1.4th
Generation  1: MUPHMOOXEIBGELPUZZEGXIVMELFL
Generation  2: MUBHIYDPKIQWYXSVLUEBH TYJMRL
Generation  3: MEVHIUTZDIVQSMRT KEDP GURBSL
Generation  4: MEWHIHKPKITBWSYVYKEXZ  ASBAL
Generation  5: MEVHIPKMRIT VSTSBKE R YNJWEL
Generation  6: MERHIIKQ IT OSNEUKE A TKCLEL
Generation  7: METHINKO IT  SXREKE A JDAIEL
Generation  8: METHINKS IT SSSVIKE A OIA EL
Generation  9: METHINKS IT ISICIKE A IGASEL
Generation 10: METHINKS IT ISITIKE A WZASEL
Generation 11: METHINKS IT ISACIKE A WEASEL
Generation 12: METHINKS IT ISKLIKE A WEASEL
Generation 13: METHINKS IT IS LIKE A WEASEL
```



## Fortran

```fortran

 !***************************************************************************************************
 	module evolve_routines
 !***************************************************************************************************
 	implicit none

 	!the target string:
 	character(len=*),parameter :: targ = 'METHINKS IT IS LIKE A WEASEL'

 	contains
 !***************************************************************************************************

 !********************************************************************
 	pure elemental function fitness(member) result(n)
 !********************************************************************
 ! The fitness function.  The lower the value, the better the match.
 ! It is zero if they are identical.
 !********************************************************************

 	implicit none
 	integer :: n
 	character(len=*),intent(in) :: member

 	integer :: i

 	n=0
 	do i=1,len(targ)
 		n = n + abs( ichar(targ(i:i)) - ichar(member(i:i))  )
 	end do

 !********************************************************************
 	end function fitness
 !********************************************************************

 !********************************************************************
 	pure elemental subroutine mutate(member,factor)
 !********************************************************************
 ! mutate a member of the population.
 !********************************************************************

 	implicit none
 	character(len=*),intent(inout) :: member   !population member
 	real,intent(in) :: factor                  !mutation factor

 	integer,parameter :: n_chars = 27	!number of characters in set
 	character(len=n_chars),parameter :: chars = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ '

 	real    :: rnd_val
 	integer :: i,j,n

 	n = len(member)

 	do i=1,n
 		rnd_val = rand()
 		if (rnd_val<=factor) then   !mutate this element
 			rnd_val = rand()
 			j = int(rnd_val*n_chars)+1   !an integer between 1 and n_chars
 			member(i:i) = chars(j:j)
 		end if
 	end do

 !********************************************************************
	end subroutine mutate
 !********************************************************************

 !***************************************************************************************************
 	end module evolve_routines
 !***************************************************************************************************

 !***************************************************************************************************
 	program evolve
 !***************************************************************************************************
 ! The main program
 !***************************************************************************************************
 	use evolve_routines

 	implicit none

 	!Tuning parameters:
 	integer,parameter :: seed = 12345             !random number generator seed
 	integer,parameter :: max_iter = 10000         !maximum number of iterations
 	integer,parameter :: population_size = 200    !size of the population
 	real,parameter    :: factor = 0.04            ![0,1] mutation factor
 	integer,parameter :: iprint = 5               !print every iprint iterations

 	!local variables:
 	integer :: i,iter
 	integer,dimension(1) :: i_best
 	character(len=len(targ)),dimension(population_size) :: population

 	!initialize random number generator:
 	call srand(seed)

 	!create initial population:
 	! [the first element of the population will hold the best member]
 	population(1) = 'PACQXJB CQPWEYKSVDCIOUPKUOJY'  !initial guess
 	iter=0

 	write(*,'(A10,A30,A10)') 'iter','best','fitness'
 	write(*,'(I10,A30,I10)') iter,population(1),fitness(population(1))

 	do

 		iter = iter + 1 !iteration counter

  		!write the iteration:
 		if (mod(iter,iprint)==0) write(*,'(I10,A30,I10)') iter,population(1),fitness(population(1))

 		!check exit conditions:
 		if ( iter>max_iter .or. fitness(population(1))==0 ) exit

 		!copy best member and mutate:
 		population = population(1)
 		do i=2,population_size
 			call mutate(population(i),factor)
 		end do

 		!select the new best population member:
 		! [the best has the lowest value]
 		i_best = minloc(fitness(population))
 		population(1) = population(i_best(1))

 	end do

 	!write the last iteration:
 	if (mod(iter,iprint)/=0) write(*,'(I10,A30,I10)') iter,population(1),fitness(population(1))

 	if (iter>max_iter) then
 		write(*,*) 'No solution found.'
 	else
 		write(*,*) 'Solution found.'
 	end if

 !***************************************************************************************************
 	end program evolve
 !***************************************************************************************************

```


The output is:

<lang>
      iter                          best   fitness
         0  PACQXJB CQPWEYKSVDCIOUPKUOJY       459
         5  PACDXJBRCQP EYKSVDK OAPKGOJY       278
        10  PAPDJJBOCQP EYCDKDK A PHGQJF       177
        15  PAUDJJBO FP FY VKBL A PEGQJF       100
        20  PEUDJMOO KP FY IKLD A YECQJF        57
        25  PEUHJMOT KU FS IKLD A YECQJL        35
        30  PEUHJMIT KU GS LKJD A YEAQFL        23
        35  MERHJMIT KT IS LHJD A YEASFL        15
        40  MERHJMKS IT IS LIJD A WEASFL         7
        45  MERHINKS IT IS LIJD A WEASFL         5
        50  MERHINKS IT IS LIJD A WEASEL         4
        55  MERHINKS IT IS LIKD A WEASEL         3
        60  MESHINKS IT IS LIKD A WEASEL         2
        65  MESHINKS IT IS LIKD A WEASEL         2
        70  MESHINKS IT IS LIKE A WEASEL         1
        75  METHINKS IT IS LIKE A WEASEL         0

```


## FreeBASIC


```freebasic
' version 01-07-2018
' compile with: fbc -s console

Randomize Timer
Const As UInteger children = 100
Const As Double mutate_rate = 0.05

Function fitness(target As String, tmp As String) As UInteger

    Dim As UInteger x, f

    For x = 0 To Len(tmp) -1
        If tmp[x] = target[x] Then f += 1
    Next
    Return f

End Function

Sub mutate(tmp As String, chars As String, mute_rate As Double)

    If Rnd <= mute_rate Then
        tmp[Int(Rnd * Len(tmp))] = chars[Int(Rnd * Len(chars))]
    End If

End Sub

' ------=< MAIN >=------

Dim As String target = "METHINKS IT IS LIKE A WEASEL"
Dim As String chars  = " ABCDEFGHIJKLMNOPQRSTUVWXYZ"
Dim As String parent, mutation()
Dim As UInteger x, iter, f, fit(), best_fit, parent_fit

For x = 1 To Len(target)
    parent += Chr(chars[Int(Rnd * Len(chars))])
Next

f = fitness(target, parent)
parent_fit = f
best_fit = f

Print "iteration  best fit   Parent"
Print "
### ======  ========   =========================
"
Print Using "     ####      ####   ";iter; best_fit;
Print parent

Do
    iter += 1
    ReDim mutation(1 To children),fit(1 To children)

    For x = 1 To children
        mutation(x) = parent
        mutate(mutation(x), chars, mutate_rate)
    Next

    For x = 1 To children
        If mutation(x) <> parent Then
            f = fitness(target, mutation(x))
            If best_fit < f Then
                best_fit = f
                fit(x) = f
            Else
                fit(x) = parent_fit
            End If
        End If
    Next

    If best_fit > parent_fit Then
        For x = 1 To children
            If fit(x) = best_fit Then
                parent = mutation(x)
                Print Using "     ####      ####   ";iter; best_fit;
                Print parent
            End If
        Next
    End If

Loop Until parent = target

' empty keyboard buffer
While InKey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

```txt
iteration  best fit   Parent

### ======  ========   =========================

        0         2   VDHQATVSHHSVRFNAPFEGZARZGCZE
        3         3   VEHQATVSHHSVRFNAPFEGZARZGCZE
        5         4   VEHQATVSHHSVRFNAPFEGZAREGCZE
       11         5   VEHQATKSHHSVRFNAPFEGZAREGCZE
       19         6   VEHQATKSHHSVRFNAPFEGZAREGSZE
       32         7   VEHQANKSHHSVRFNAPFEGZAREGSZE
       36         8   VEHQANKSHHSVRFNAPFEGAAREGSZE
       38         9   VEHQANKSHHTVRFNAPFEGAAREGSZE
       39        10   VEHQANKSHHTVRFNAPFEGAAREGSEE
       48        11   VEHHANKSHHTVRFNAPFEGAAREGSEE
       53        12   VEHHANKSHITVRFNAPFEGAAREGSEE
       73        13   VEHHINKSHITVRFNAPFEGAAREGSEE
       81        14   VEHHINKSHITVRFNAPFEGAAWEGSEE
       95        15   VEHHINKSHITVIFNAPFEGAAWEGSEE
       96        16   VEHHINKSHITVIFNLPFEGAAWEGSEE
      135        17   VETHINKSHITVIFNLPFEGAAWEGSEE
      137        18   VETHINKSHITVISNLPFEGAAWEGSEE
      152        19   VETHINKSHITVISNLPKEGAAWEGSEE
      171        20   VETHINKSHITVISNLPKEGAAWEGSEL
      174        21   VETHINKSHITVIS LPKEGAAWEGSEL
      188        22   VETHINKSHITVIS LIKEGAAWEGSEL
      213        23   VETHINKSHIT IS LIKEGAAWEGSEL
      220        24   METHINKSHIT IS LIKEGAAWEGSEL
      374        25   METHINKSHIT IS LIKE AAWEGSEL
      378        26   METHINKSHIT IS LIKE A WEGSEL
      555        27   METHINKS IT IS LIKE A WEGSEL
      585        28   METHINKS IT IS LIKE A WEASEL
```



## Go

I took the liberty to use <code>[]byte</code> for the "strings" mentioned in the task description.  Go has a native string type, but in this case it was both easier and more efficient to work with byte slices and just convert to string when there was something to print.

```go
package main

import (
    "fmt"
    "math/rand"
    "time"
)

var target = []byte("METHINKS IT IS LIKE A WEASEL")
var set = []byte("ABCDEFGHIJKLMNOPQRSTUVWXYZ ")
var parent []byte

func init() {
    rand.Seed(time.Now().UnixNano())
    parent = make([]byte, len(target))
    for i := range parent {
        parent[i] = set[rand.Intn(len(set))]
    }
}

// fitness:  0 is perfect fit.  greater numbers indicate worse fit.
func fitness(a []byte) (h int) {
    // (hamming distance)
    for i, tc := range target {
        if a[i] != tc {
            h++
        }
    }
    return
}

// set m to mutation of p, with each character of p mutated with probability r
func mutate(p, m []byte, r float64) {
    for i, ch := range p {
        if rand.Float64() < r {
            m[i] = set[rand.Intn(len(set))]
        } else {
            m[i] = ch
        }
    }
}

func main() {
    const c = 20 // number of times to copy and mutate parent

    copies := make([][]byte, c)
    for i := range copies {
        copies[i] = make([]byte, len(parent))
    }

    fmt.Println(string(parent))
    for best := fitness(parent); best > 0; {
        for _, cp := range copies {
            mutate(parent, cp, .05)
        }
        for _, cp := range copies {
            fm := fitness(cp)
            if fm < best {
                best = fm
                copy(parent, cp)
                fmt.Println(string(parent))
            }
        }
    }
}
```

<div style='height: 15em; overflow: scroll'>
```txt

HRVDKMXETOIOVSFMVHWKIY ZDXEY
HRVDKMXE OIOVSFMVHWKIY ZDWEY
HRVDKMXE OIOISFMVHWVIY ZDSEY
HRVDKMXE OIOISFMFHWVI  ZDSEL
HRVDKMXE OIOISFLFHWVI  ZDSEL
HRVDKMXE OIOISFLFHWVI  ZASEL
HRVDKMXS OIOISFLFHWVI  ZASEL
HRVHKMXS OIOISFLHHWVI  ZASEL
MRVHKMXS OHOISFLHHWVI  ZASEL
MRVHKMXS OTOISFLHHWVI  FASEL
MRVHKNXS OTOISFLHHWVI  FASEL
MRVHKNXS OTOISFLHHWVI  EASEL
MEVHKNXS OTOISFLHHWVI IEASEL
MEVHKNXS OTOISFLHHWVI WEASEL
METHKNXS OTOISFLHHWVI WEASEL
METHKNXS ZTOIS LHHWVI WEASEL
METHKNKS ZTOIS LHHWVI WEASEL
METHKNKS ZTOIS LHKWEI WEASEL
METHKNKS ZT IS LHKWEI WEASEL
METHKNKS ZT IS LHKEEI WEASEL
METHKNKS ZT IS LHKEEA WEASEL
METHKNKS ZT IS LHKE A WEASEL
METHKNKS ZT IS LIKE A WEASEL
METHINKS ZT IS LIKE A WEASEL
METHINKS IT IS LIKE A WEASEL
```
</div>


## Haskell

```Haskell
import System.Random
import Control.Monad
import Data.List
import Data.Ord
import Data.Array

showNum :: (Num a, Show a) => Int -> a -> String
showNum w = until ((>w-1).length) (' ':) . show

replace :: Int -> a -> [a] -> [a]
replace n c ls = take (n-1) ls ++ [c] ++ drop n ls

target = "METHINKS IT IS LIKE A WEASEL"
pfit = length target
mutateRate = 20
popsize = 100
charSet = listArray (0,26) $ ' ': ['A'..'Z'] :: Array Int Char

fitness = length . filter id . zipWith (==) target

printRes i g = putStrLn $
     "gen:" ++ showNum 4 i ++ "  "
     ++ "fitn:" ++ showNum 4  (round $ 100 * fromIntegral s / fromIntegral pfit ) ++ "%  "
     ++ show g
    where s = fitness g

mutate :: [Char] -> Int -> IO [Char]
mutate g mr = do
  let r = length g
  chances <- replicateM r $ randomRIO (1,mr)
  let pos = elemIndices 1 chances
  chrs <- replicateM (length pos) $ randomRIO (bounds charSet)
  let nchrs = map (charSet!) chrs
  return $ foldl (\ng (p,c) -> replace (p+1) c ng) g (zip pos nchrs)

evolve :: [Char] -> Int -> Int -> IO ()
evolve parent gen mr = do
  when ((gen-1) `mod` 20 == 0) $ printRes (gen-1) parent
  children <- replicateM popsize (mutate parent mr)
  let child = maximumBy (comparing fitness) (parent:children)
  if fitness child == pfit then printRes gen child
                           else evolve child (succ gen) mr

main = do
  let r = length target
  genes <- replicateM r $ randomRIO (bounds charSet)
  let parent = map (charSet!) genes
  evolve parent 1 mutateRate
```

Example run in GHCi:

```txt
*Main> main
gen:   0  fitn:   4%  "AICJEWXYSFTMOAYOHNFZ HSLFNBY"
gen:  20  fitn:  54%  "XZTHIWXSSVTMSUYOIKEZA WEFSEL"
gen:  40  fitn:  89%  "METHINXSSIT IS OIKE A WEASEL"
gen:  60  fitn:  93%  "METHINXSSIT IS LIKE A WEASEL"
gen:  78  fitn: 100%  "METHINKS IT IS LIKE A WEASEL"
```



### Alternate Presentation

I find this easier to read.


```Haskell
import System.Random
import Data.List
import Data.Ord
import Data.Array
import Control.Monad
import Control.Arrow

target = "METHINKS IT IS LIKE A WEASEL"
mutateRate = 0.1
popSize = 100
printEvery = 10

alphabet = listArray (0,26) (' ':['A'..'Z'])

randomChar = (randomRIO (0,26) :: IO Int) >>= return . (alphabet !)

origin = mapM createChar target
    where createChar c = randomChar

fitness = length . filter id . zipWith (==) target

mutate = mapM mutateChar
    where mutateChar c = do
            r <- randomRIO (0.0,1.0) :: IO Double
            if r < mutateRate then randomChar else return c

converge n parent = do
    if n`mod`printEvery == 0 then putStrLn fmtd else return ()
    if target == parent
        then putStrLn $ "\nFinal: " ++ fmtd
        else mapM mutate (replicate (popSize-1) parent) >>=
                converge (n+1) . fst . maximumBy (comparing snd) . map (id &&& fitness) . (parent:)
    where fmtd = parent ++ ": " ++ show (fitness parent) ++ " (" ++ show n ++ ")"

main = origin >>= converge 0
```

Example:

```txt
YUZVNNZ SXPSNGZFRHZKVDOEPIGS: 2 (0)
BEZHANK KIPONSYSPKV F AEULEC: 11 (10)
BETHANKSFIT ISYHIKJ I TERLER: 17 (20)
METHINKS IT IS YIKE R TERYER: 22 (30)
METHINKS IT IS YIKE   WEASEQ: 25 (40)
METHINKS IT IS MIKE   WEASEI: 25 (50)
METHINKS IT IS LIKE D WEASEI: 26 (60)
METHINKS IT IS LIKE T WEASEX: 26 (70)
METHINKS IT IS LIKE I WEASEL: 27 (80)

Final: METHINKS IT IS LIKE A WEASEL: 28 (86)
```


=={{header|Icon}} and {{header|Unicon}}==

```icon
global target, chars, parent, C, M, current_fitness

procedure fitness(s)
	fit := 0
	#Increment the fitness for every position in the string s that matches the target
	every i := 1 to *target & s[i] == target[i] do fit +:= 1
	return fit
end

procedure mutate(s)
	#If a random number between 0 and 1 is inside the bounds of mutation randomly alter a character in the string
	if (?0 <= M) then ?s := ?chars
	return s
end

procedure generation()
	population := [ ]
	next_parent := ""
	next_fitness := -1

	#Create the next population
	every 1 to C do push(population, mutate(parent))
	#Find the member of the population with highest fitness, or use the last one inspected
	every x := !population & (xf := fitness(x)) > next_fitness do {
		next_parent := x
		next_fitness := xf
	}

	parent := next_parent

	return next_fitness
end

procedure main()
	target := "METHINKS IT IS LIKE A WEASEL"			#Our target string
	chars := &ucase ++ " "						#Set of usable characters
	parent := "" & every 1 to *target do parent ||:= ?chars		#The universal common ancestor!
	current_fitness := fitness(parent)				#The best fitness we have so far


	C := 50		#Population size in each generation
	M := 0.5	#Mutation rate per individual in a generation

	gen := 1
	#Until current fitness reaches a score of perfect match with the target string keep generating new populations
	until ((current_fitness := generation()) = *target) do {
                write(gen || " " || current_fitness || " " || parent)
                gen +:= 1
	}
	write("At generation " || gen || " we found a string with perfect fitness at " || current_fitness || " reading: " || parent)
end

```



## J


'''Solution:'''

Using sum of differences from the target for fitness, i.e. <code>0</code> is optimal fitness.

```j
CHARSET=: 'ABCDEFGHIJKLMNOPQRSTUVWXYZ '
NPROG=:   100                            NB. number of progeny (C)
MRATE=:   0.05                           NB. mutation rate

create  =: (?@$&$ { ])&CHARSET           NB. creates random list from charset of same shape as y
fitness =: +/@:~:"1
copy    =: # ,:
mutate  =: &(>: $ ?@$ 0:)(`(,: create))} NB. adverb
select  =: ] {~ (i. <./)@:fitness        NB. select fittest member of population

nextgen =: select ] , [: MRATE mutate NPROG copy ]
while   =: conjunction def '(] , (u {:))^:(v {:)^:_ ,:'

evolve=: nextgen while (0 < fitness) create
```


'''Example usage:'''

Returns list of best solutions at each generation until converged.

```j
   filter=: {: ,~ ({~ i.@>.&.(%&20)@#)   NB. take every 20th and last item
   filter evolve 'METHINKS IT IS LIKE A WEASEL'
XXURVQXKQXDLCGFVICCUA NUQPND
MEFHINVQQXT IW LIKEUA WEAPEL
METHINVS IT IW LIKEUA WEAPEL
METHINKS IT IS LIKE A WEASEL
```


'''Alternative solution:'''

Using explicit versions of <code>mutate</code> and <code>evolve</code> above.

```j
CHARSET=: 'ABCDEFGHIJKLMNOPQRSTUVWXYZ '
NPROG=:   100                             NB. "C" from specification

fitness=: +/@:~:"1
select=: ] {~ (i. <./)@:fitness           NB. select fittest member of population
populate=: (?@$&# { ])&CHARSET            NB. get random list from charset of same length as y
log=: [: smoutput [: ;:inv (('#';'fitness: ';'; ') ,&.> ":&.>)

mutate=: dyad define
  idxmut=. I. x >: (*/$y) ?@$ 0
  (populate idxmut) idxmut"_} y
)

evolve=: monad define
  target=. y
  parent=. populate y
  iter=. 0
  mrate=. %#y
  while. 0 < val=. target fitness parent do.
    if. 0 = 50|iter do. log iter;val;parent end.
    iter=. iter + 1
    progeny=. mrate mutate NPROG # ,: parent  NB. create progeny by mutating parent copies
    parent=. target select parent,progeny     NB. select fittest parent for next generation
  end.
  log iter;val;parent
  parent
)
```


'''Example Usage:'''

```j
   evolve 'METHINKS IT IS LIKE A WEASEL'
#0 fitness: 27 ; YGFDJFTBEDB FAIJJGMFKDPYELOA
#50 fitness: 2 ; MEVHINKS IT IS LIKE ADWEASEL
#76 fitness: 0 ; METHINKS IT IS LIKE A WEASEL
METHINKS IT IS LIKE A WEASEL
```



## Java

'''(Close)''' {{trans|Python}}

```java5

import java.util.Random;

public class EvoAlgo {
  static final String target = "METHINKS IT IS LIKE A WEASEL";
  static final char[] possibilities = "ABCDEFGHIJKLMNOPQRSTUVWXYZ ".toCharArray();
  static int C = 100; //number of spawn per generation
  static double minMutateRate = 0.09;
  static int perfectFitness = target.length();
  private static String parent;
  static Random rand = new Random();

  private static int fitness(String trial){
    int retVal = 0;
    for(int i = 0;i < trial.length(); i++){
      if (trial.charAt(i) == target.charAt(i)) retVal++;
    }
    return retVal;
  }

  private static double newMutateRate(){
    return (((double)perfectFitness - fitness(parent)) / perfectFitness * (1 - minMutateRate));
  }

  private static String mutate(String parent, double rate){
    String retVal = "";
    for(int i = 0;i < parent.length(); i++){
      retVal += (rand.nextDouble() <= rate) ?
        possibilities[rand.nextInt(possibilities.length)]:
        parent.charAt(i);
    }
    return retVal;
  }

  public static void main(String[] args){
    parent = mutate(target, 1);
    int iter = 0;
    while(!target.equals(parent)){
      double rate = newMutateRate();
      iter++;
      if(iter % 100 == 0){
        System.out.println(iter +": "+parent+ ", fitness: "+fitness(parent)+", rate: "+rate);
      }
      String bestSpawn = null;
      int bestFit = 0;
      for(int i = 0; i < C; i++){
        String spawn = mutate(parent, rate);
        int fitness = fitness(spawn);
        if(fitness > bestFit){
          bestSpawn = spawn;
          bestFit = fitness;
        }
      }
      parent = bestFit > fitness(parent) ? bestSpawn : parent;
    }
    System.out.println(parent+", "+iter);
  }

}
```

Output:

```txt
100: MEVHIBXSCG  TP QIK  FZGJ SEL, fitness: 13, rate: 0.4875
200: MEBHINMSVI  IHTQIKW FTDEZSWL, fitness: 15, rate: 0.42250000000000004
300: METHINMSMIA IHUFIKA F WEYSEL, fitness: 19, rate: 0.29250000000000004
400: METHINSS IT IQULIKA F WEGSEL, fitness: 22, rate: 0.195
METHINKS IT IS LIKE A WEASEL, 492
```



## JavaScript

Using cross-browser techniques to support Array.reduce and Array.map


```javascript
// ------------------------------------- Cross-browser Compatibility -------------------------------------

/* Compatibility code to reduce an array
 * Source: https://developer.mozilla.org/en/JavaScript/Reference/Global_Objects/Array/Reduce
 */
if (!Array.prototype.reduce) {
    Array.prototype.reduce = function (fun /*, initialValue */ ) {
        "use strict";

        if (this === void 0 || this === null) throw new TypeError();

        var t = Object(this);
        var len = t.length >>> 0;
        if (typeof fun !== "function") throw new TypeError();

        // no value to return if no initial value and an empty array
        if (len == 0 && arguments.length == 1) throw new TypeError();

        var k = 0;
        var accumulator;
        if (arguments.length >= 2) {
            accumulator = arguments[1];
        } else {
            do {
                if (k in t) {
                    accumulator = t[k++];
                    break;
                }

                // if array contains no values, no initial value to return
                if (++k >= len) throw new TypeError();
            }
            while (true);
        }

        while (k < len) {
            if (k in t) accumulator = fun.call(undefined, accumulator, t[k], k, t);
            k++;
        }

        return accumulator;
    };
}

/* Compatibility code to map an array
 * Source: https://developer.mozilla.org/en/JavaScript/Reference/Global_Objects/Array/Map
 */
if (!Array.prototype.map) {
    Array.prototype.map = function (fun /*, thisp */ ) {
        "use strict";

        if (this === void 0 || this === null) throw new TypeError();

        var t = Object(this);
        var len = t.length >>> 0;
        if (typeof fun !== "function") throw new TypeError();

        var res = new Array(len);
        var thisp = arguments[1];
        for (var i = 0; i < len; i++) {
            if (i in t) res[i] = fun.call(thisp, t[i], i, t);
        }

        return res;
    };
}

/* ------------------------------------- Generator -------------------------------------
 * Generates a fixed length gene sequence via a gene strategy object.
 * The gene strategy object must have two functions:
 *	- "create": returns create a new gene
 *	- "mutate(existingGene)": returns mutation of an existing gene
 */
function Generator(length, mutationRate, geneStrategy) {
    this.size = length;
    this.mutationRate = mutationRate;
    this.geneStrategy = geneStrategy;
}

Generator.prototype.spawn = function () {
    var genes = [],
        x;
    for (x = 0; x < this.size; x += 1) {
        genes.push(this.geneStrategy.create());
    }
    return genes;
};

Generator.prototype.mutate = function (parent) {
    return parent.map(function (char) {
        if (Math.random() > this.mutationRate) {
            return char;
        }
        return this.geneStrategy.mutate(char);
    }, this);
};

/* ------------------------------------- Population -------------------------------------
 * Helper class that holds and spawns a new population.
 */
function Population(size, generator) {
    this.size = size;
    this.generator = generator;

    this.population = [];
    // Build initial popuation;
    for (var x = 0; x < this.size; x += 1) {
        this.population.push(this.generator.spawn());
    }
}

Population.prototype.spawn = function (parent) {
    this.population = [];
    for (var x = 0; x < this.size; x += 1) {
        this.population.push(this.generator.mutate(parent));
    }
};

/* ------------------------------------- Evolver -------------------------------------
 * Attempts to converge a population based a fitness strategy object.
 * The fitness strategy object must have three function
 *	- "score(individual)": returns a score for an individual.
 *	- "compare(scoreA, scoreB)": return true if scoreA is better (ie more fit) then scoreB
 *	- "done( score )": return true if score is acceptable (ie we have successfully converged).
 */
function Evolver(size, generator, fitness) {
    this.done = false;
    this.fitness = fitness;
    this.population = new Population(size, generator);
}

Evolver.prototype.getFittest = function () {
    return this.population.population.reduce(function (best, individual) {
        var currentScore = this.fitness.score(individual);
        if (best === null || this.fitness.compare(currentScore, best.score)) {
            return {
                score: currentScore,
                individual: individual
            };
        } else {
            return best;
        }
    }, null);
};

Evolver.prototype.doGeneration = function () {
    this.fittest = this.getFittest();
    this.done = this.fitness.done(this.fittest.score);
    if (!this.done) {
        this.population.spawn(this.fittest.individual);
    }
};

Evolver.prototype.run = function (onCheckpoint, checkPointFrequency) {
    checkPointFrequency = checkPointFrequency || 10; // Default to Checkpoints every 10 generations
    var generation = 0;
    while (!this.done) {
        this.doGeneration();
        if (generation % checkPointFrequency === 0) {
            onCheckpoint(generation, this.fittest);
        }
        generation += 1;
    }
    onCheckpoint(generation, this.fittest);
    return this.fittest;
};

// ------------------------------------- Exports -------------------------------------
window.Generator = Generator;
window.Evolver = Evolver;


// helper utitlity to combine elements of two arrays.
Array.prototype.zip = function (b, func) {
    var result = [],
        max = Math.max(this.length, b.length),
        x;
    for (x = 0; x < max; x += 1) {
        result.push(func(this[x], b[x]));
    }
    return result;
};

var target = "METHINKS IT IS LIKE A WEASEL", geneStrategy, fitness, target, generator, evolver, result;

geneStrategy = {
    // The allowed character set (as an array)
    characterSet: "ABCDEFGHIJKLMNOPQRSTUVWXYZ ".split(""),

    /*
        Pick a random character from the characterSet
    */
    create: function getRandomGene() {
        var randomNumber = Math.floor(Math.random() * this.characterSet.length);
        return this.characterSet[randomNumber];
    }
};
geneStrategy.mutate = geneStrategy.create; // Our mutation stragtegy is to simply get a random gene
fitness = {
    // The target (as an array of characters)
    target: target.split(""),
    equal: function (geneA, geneB) {
        return (geneA === geneB ? 0 : 1);
    },
    sum: function (runningTotal, value) {
        return runningTotal + value;
    },

    /*
        We give one point to for each corect letter
    */
    score: function (genes) {
        var diff = genes.zip(this.target, this.equal); // create an array of ones and zeros
        return diff.reduce(this.sum, 0); // Sum the array values together.
    },
    compare: function (scoreA, scoreB) {
        return scoreA <= scoreB; // Lower scores are better
    },
    done: function (score) {
        return score === 0; // We have matched the target string.
    }
};

generator = new Generator(target.length, 0.05, geneStrategy);
evolver = new Evolver(100, generator, fitness);

function showProgress(generation, fittest) {
    document.write("Generation: " + generation + ", Best: " + fittest.individual.join("") + ", fitness:" + fittest.score + "
");
}
result = evolver.run(showProgress);
```

Output:

```txt

Generation: 0, Best: KSTFOKJC XZYLWCLLGYZJNXYEGHE, fitness:25
Generation: 10, Best: KOTFINJC XX LS LIGYZT WEPSHL, fitness:14
Generation: 20, Best: KBTHINKS BT LS LIGNZA WEPSEL, fitness:8
Generation: 30, Best: KETHINKS IT BS LISNZA WEASEL, fitness:5
Generation: 40, Best: KETHINKS IT IS LIKEZA WEASEL, fitness:2
Generation: 50, Best: METHINKS IT IS LIKEZA WEASEL, fitness:1
Generation: 52, Best: METHINKS IT IS LIKE A WEASEL, fitness:0

```



## Julia

```julia
fitness(a::AbstractString, b::AbstractString) = count(l == t for (l, t) in zip(a, b))
function mutate(str::AbstractString, rate::Float64)
    L = collect(Char, " ABCDEFGHIJKLMNOPQRSTUVWXYZ")
    return map(str) do c
        if rand() < rate rand(L) else c end
    end
end

function evolve(parent::String, target::String, mutrate::Float64, nchild::Int)
    println("Initial parent is $parent, its fitness is $(fitness(parent, target))")
    gens = 0
    while parent != target
        children = collect(mutate(parent, mutrate) for i in 1:nchild)
        bestfit, best = findmax(fitness.(children, target))
        parent = children[best]
        gens += 1
        if gens % 10 == 0
            println("After $gens generations, the new parent is $parent and its fitness is $(fitness(parent, target))")
        end
    end
    println("After $gens generations, the parent evolved into the target $target")
end

evolve("IU RFSGJABGOLYWF XSMFXNIABKT", "METHINKS IT IS LIKE A WEASEL", 0.08998, 100)
```


```txt
Initial parent is IU RFSGJABGOLYWF XSMFXNIABKT, its fitness is 1
After 10 generations, the new parent is MOTBSNGTABTTIL LIXEMA WMALSN and its fitness is 13
After 20 generations, the new parent is METHINGATITUIS LIXE A WEASEQ and its fitness is 22
After 30 generations, the new parent is METHINKSLIT ISELIAE A WEASES and its fitness is 24
After 40 generations, the new parent is METHINKS IT IS LINE A WEASEL and its fitness is 27
After 50 generations, the new parent is METHINKS IT IS LINE A WEASEL and its fitness is 27
After 60 generations, the new parent is METHINKS IT IS PIKE A WEASEL and its fitness is 27
After 70 generations, the new parent is METHINKS IT IS AIKE A WEASEL and its fitness is 27
After 80 generations, the new parent is METHINKS IT IS AIKE A WEASEL and its fitness is 27
After 81 generations, the parent evolved into the target METHINKS IT IS LIKE A WEASEL
```



## Kotlin


```scala
import java.util.*

val target = "METHINKS IT IS LIKE A WEASEL"
val validChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ "

val random = Random()

fun randomChar() = validChars[random.nextInt(validChars.length)]
fun hammingDistance(s1: String, s2: String) =
        s1.zip(s2).map { if (it.first == it.second) 0 else 1 }.sum()

fun fitness(s1: String) = target.length - hammingDistance(s1, target)

fun mutate(s1: String, mutationRate: Double) =
        s1.map { if (random.nextDouble() > mutationRate) it else randomChar() }
                .joinToString(separator = "")

fun main(args: Array<String>) {
    val initialString = (0 until target.length).map { randomChar() }.joinToString(separator = "")

    println(initialString)
    println(mutate(initialString, 0.2))

    val mutationRate = 0.05
    val childrenPerGen = 50

    var i = 0
    var currVal = initialString
    while (currVal != target) {
        i += 1
        currVal = (0..childrenPerGen).map { mutate(currVal, mutationRate) }.maxBy { fitness(it) }!!
    }
    println("Evolution found target after $i generations")
}
```



## Liberty BASIC


```lb
C = 10
'mutaterate has to be greater than 1 or it will not mutate
mutaterate = 2
mutationstaken = 0
generations = 0
Dim parentcopies$((C - 1))
Global targetString$ : targetString$ = "METHINKS IT IS LIKE A WEASEL"
Global allowableCharacters$ : allowableCharacters$ = " ABCDEFGHIJKLMNOPQRSTUVWXYZ"
currentminFitness = Len(targetString$)

For i = 1 To Len(targetString$)
    parent$ = parent$ + Mid$(allowableCharacters$, Int(Rnd(1) * Len(allowableCharacters$)), 1)
Next i

Print "Parent = " + parent$

While parent$ <> targetString$
    generations = (generations + 1)
    For i = 0 To (C - 1)
        parentcopies$(i) = mutate$(parent$, mutaterate)
        mutationstaken = (mutationstaken + 1)
    Next i
    For i = 0 To (C - 1)
        currentFitness = Fitness(targetString$, parentcopies$(i))
        If currentFitness = 0 Then
            parent$ = parentcopies$(i)
            Exit For
        Else
            If currentFitness < currentminFitness Then
                currentminFitness = currentFitness
                parent$ = parentcopies$(i)
            End If
        End If
    Next i
    CLS
    Print "Generation - " + str$(generations)
    Print "Parent - " + parent$
    Scan
Wend

Print
Print "Congratulations to me; I finished!"
Print "Final Mutation: " + parent$
'The ((i + 1) - (C)) reduces the total number of mutations that it took by one generation
'minus the perfect child mutation since any after that would not have been required.
Print "Total Mutations Taken - " + str$(mutationstaken - ((i + 1) - (C)))
Print "Total Generations Taken - " + str$(generations)
Print "Child Number " + str$(i) + " has perfect similarities to your target."
End



Function mutate$(mutate$, mutaterate)
        If (Rnd(1) * mutaterate) > 1 Then
            'The mutatingcharater randomizer needs 1 more than the length of the string
            'otherwise it will likely take forever to get exactly that as a random number
            mutatingcharacter = Int(Rnd(1) * (Len(targetString$) + 1))
            mutate$ = Left$(mutate$, (mutatingcharacter - 1))  + Mid$(allowableCharacters$, Int(Rnd(1) * Len(allowableCharacters$)), 1) _
                      + Mid$(mutate$, (mutatingcharacter + 1))
        End If
End Function

Function Fitness(parent$, offspring$)
    For i = 1 To Len(targetString$)
        If Mid$(parent$, i, 1) <> Mid$(offspring$, i, 1) Then
            Fitness = (Fitness + 1)
        End If
    Next i
End Function
```



## Logo


```logo
make "target "|METHINKS IT IS LIKE A WEASEL|

to distance :w
  output reduce "sum (map.se [ifelse equal? ?1 ?2 [0][1]] :w :target)
end

to random.letter
  output pick "| ABCDEFGHIJKLMNOPQRSTUVWXYZ|
end

to mutate :parent :rate
  output map [ifelse random 100 < :rate [random.letter] [?]] :parent
end

make "C 100
make "mutate.rate 10     ; percent

to breed :parent
  make "parent.distance distance :parent
  localmake "best.child :parent
  repeat :C [
    localmake "child mutate :parent :mutate.rate
    localmake "child.distance distance :child
    if greater? :parent.distance :child.distance [
      make "parent.distance :child.distance
      make "best.child :child
    ]
  ]
  output :best.child
end

to progress
  output (sentence :trials :parent "distance: :parent.distance)
end

to evolve
  make "parent cascade count :target [lput random.letter ?] "||
  make "trials 0
  while [not equal? :parent :target] [
    make "parent breed :parent
    print progress
    make "trials :trials + 1
  ]
end
```



## Lua

```lua
local target = "METHINKS IT IS LIKE A WEASEL"
local alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ "
local c, p = 100, 0.06

local function fitness(s)
	local score = #target
	for i = 1,#target do
		if s:sub(i,i) == target:sub(i,i) then score = score - 1 end
	end
	return score
end

local function mutate(s, rate)
	local result, idx = ""
	for i = 1,#s do
		if math.random() < rate then
			idx = math.random(#alphabet)
			result = result .. alphabet:sub(idx,idx)
		else
			result = result .. s:sub(i,i)
		end
	end
	return result, fitness(result)
end

local function randomString(len)
	local result, idx = ""
	for i = 1,len do
		idx = math.random(#alphabet)
		result = result .. alphabet:sub(idx,idx)
	end
	return result
end

local function printStep(step, s, fit)
	print(string.format("%04d: ", step) .. s .. " [" .. fit .."]")
end

math.randomseed(os.time())
local parent = randomString(#target)
printStep(0, parent, fitness(parent))

local step = 0
while parent ~= target do
	local bestFitness, bestChild, child, fitness = #target + 1
	for i = 1,c do
		child, fitness = mutate(parent, p)
		if fitness < bestFitness then bestFitness, bestChild = fitness, child end
	end
	parent, step = bestChild, step + 1
	printStep(step, parent, bestFitness)
end
```



## M2000 Interpreter


### Version 1


```M2000 Interpreter

Module WeaselAlgorithm {
      Print "Evolutionary Algorithm"
      \\ Weasel Algorithm
      \\ Using dynamic array, which expand if no fitness change,
      \\ and reduce to minimum when fitness changed
      \\ Abandon strings when fitness change
      \\ Also lambda function Mutate$ change when topscore=10, to change only one character
      l$="ABCDEFGHIJKLMNOPQRSTUVWXYZ "
      randomstring$=lambda$ l$ ->{
            res$=""
            For i=1 to 28: res$+=Mid$(L$,Random(1,27),1):next i
            =res$
      }
      m$="METHINKS IT IS LIKE A WEASEL"
      lm=len(m$)
      fitness=lambda m$, lm  (this$)-> {
            score=0 : For i=1 to lm {score+=If(mid$(m$,i,1)=mid$(this$, i, 1)->1,0)} : =score
      }
      Mutate$=lambda$ l$ (w$)-> {
            a=random(1,28) : insert a, 1 w$=mid$(l$, random(1,27),1)
            If random(3)=1 Then b=a:while b=a {b=random(1,28)} : insert b, 1 w$=mid$(l$, random(1,27),1)
            =w$
      }
      Mutate1$=lambda$ l$ (w$)-> {
            insert random(1,28), 1 w$=mid$(l$, random(1,27),1) : =w$
      }
      f$=randomstring$()
      topscore=0
      last=0
      Pen 11 {Print "Fitness |Target:", @(16),m$, @(47),"|Total Strings"}
      Print Over $(3,8), str$(topscore/28,"##0.0%"),"",$(0),f$, 0
      count=0
      gen=30
      mut=0
      {
            last=0
            Dim a$(1 to gen)<<mutate$(f$)
            mut+=gen
            oldscore=topscore
            For i=1 to gen {
                  topscore=max.data(topscore, fitness(a$(i)))
                  If oldscore<topscore Then last=i:Exit
            }
            If last>0 Then {
                  f$=a$(last) : gen=30 : If topscore=10 Then mutate$=mutate1$
            } Else gen+=50
            Print Over $(3,8), str$(topscore/28,"##0.0%"), "",$(0),f$, mut : refresh
            count+=min(gen,i)
            If topscore<28 Then loop
      }
      Print
      Print "Results"
      Print "I found this:"; a$(i)
      Print "Total strings which evalute fitness:"; count
      Print "Done"
}
WeaselAlgorithm


```

```txt
Fitness |Target: METHINKS IT IS LIKE A WEASEL |Total strings
    3,6%         ZZBZSVEOWPSQGJXNIXTFQCDQTJFE        30
    7,1%         ZZBZSVEOWPSQGJXNIXTFQCDQAJFE        60
   14,3%         ZZBZSVEOWPTQGJXNIXTFACDQAJFE        90
   17,9%         ZZBZSVEOWPTQGJXNIXTFA DQAJFE       200
   21,4%         ZEBZSVEOWPTQGJXNIXTFA DQAJFE       230
   25,0%         ZEBZSVEOWPTQGJXNIXT A DQAJFE       260
   28,6%         MEBZSVEOCPTQGJXNIXT A DQAJFE       290
   32,1%         MEBZSVEOCITQGJXNIXT A DQAJFE       320
   35,7%         MEBZSVEOCITQGJXNIKT A DQAJFE       350
   39,3%         MEBZSVEOCITQGJ NIKT A DQAJFE       380
   42,9%         MEBZSVEOCITQGJ NIKT A WQAJFE       410
   46,4%         MEBZSVESCITQGJ NIKT A WQAJFE       440
   50,0%         MEBZSVESCITQIJ NIKT A WQAJFE       680
   53,6%         MEBZSVESCIT IJ NIKT A WQAJFE      1100
   57,1%         MEBZSVESCIT IJ LIKT A WQAJFE      1130
   60,7%         MEBZSVKSCIT IJ LIKT A WQAJFE      1240
   64,3%         MEBZSVKS IT IJ LIKT A WQAJFE      1480
   67,9%         MEBZSNKS IT IJ LIKT A WQAJFE      1900
   71,4%         MEBHSNKS IT IJ LIKT A WQAJFE      2010
   75,0%         METHSNKS IT IJ LIKT A WQAJFE      2430
   78,6%         METHSNKS IT IJ LIKE A WQAJFE      2670
   82,1%         METHSNKS IT IJ LIKE A WQAJFL      3090
   85,7%         METHSNKS IT IJ LIKE A WEAJFL      3330
   89,3%         METHSNKS IT IJ LIKE A WEASFL      3980
   92,9%         METHINKS IT IJ LIKE A WEASFL      4400
   96,4%         METHINKS IT IJ LIKE A WEASEL      5050
  100,0%         METHINKS IT IS LIKE A WEASEL      5290
Results
I found this:METHINKS IT IS LIKE A WEASEL
Total strings which evaluate fitness:3230

```



### Version 2

The second version check fitness for all strings until became 28 (100%)

Also here we have one Mutate function which change letters using 5% probability for each place in the parent string.


```M2000 Interpreter

Module WeaselAlgorithm2 {
      Print "Evolutionary Algorithm"
      \\ Weasel Algorithm
      \\ Using dynamic array, which expand if no fitness change,
      \\ and reduce to minimum when fitness changed
      l$="ABCDEFGHIJKLMNOPQRSTUVWXYZ "
      randomstring$=lambda$ l$ ->{
            res$=""
            For i=1 to 28: res$+=Mid$(L$,Random(1,27),1):next i
            =res$
      }
      m$="METHINKS IT IS LIKE A WEASEL"
      lm=len(m$)
      fitness=lambda m$, lm  (this$)-> {
            score=0 : For i=1 to lm {score+=If(mid$(m$,i,1)=mid$(this$, i, 1)->1,0)} : =score
      }
      Mutate$=lambda$ l$ (w$)-> {
            for i=1 to len(w$) {
                  if random(1,100)<=5 then { insert i, 1 w$=mid$(l$, random(1,27),1)  }
            }
            =w$
      }
      f$=randomstring$()
      topscore=0
      last=0
      Pen 11 {Print "Fitness |Target:", @(16),m$, @(47),"|Total Strings"}
      Print Over $(3,8), str$(topscore/28,"##0.0%"),"",$(0),f$, 0
      count=0
      gen=30
      mut=0
      {
            last=0
            Dim a$(1 to gen)<<mutate$(f$)
            mut+=gen
            oldscore=topscore
            For i=1 to gen {
                  topscore=max.data(topscore, fitness(a$(i)))
                  If oldscore<topscore Then last=i: oldscore=topscore
            }
            If last>0 Then {
                  f$=a$(last) : gen=30
            } Else gen+=50
            Print Over $(3,8), str$(topscore/28,"##0.0%"), "",$(0),f$, mut : refresh
            count+=min(gen,i)
            If topscore<28 Then loop
      }
      Print
      Print "Results"
      Print "I found this:"; a$(last)
      Print "Total strings which evalute fitness:"; count
      Print "Done"
}
WeaselAlgorithm2

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
target = "METHINKS IT IS LIKE A WEASEL";
alphabet = CharacterRange["A", "Z"]~Join~{" "};
fitness = HammingDistance[target, #] &;
Mutate[parent_String, rate_: 0.01, fertility_Integer: 25] := Module[
   {offspring, kidfits, gen = 0, alphabet = CharacterRange["A", "Z"]~Join~{" "}},
   offspring = ConstantArray[Characters[parent], fertility];
   Table[
    If[RandomReal[] <= rate, offspring[[j, k]] = RandomChoice[alphabet]],
    {j, fertility}, {k, StringLength@parent}
    ];
   offspring = StringJoin[#] & /@ offspring;
   kidfits = fitness[#] & /@ Flatten[{offspring, parent}];
   Return[offspring[[First@Ordering[kidfits]]]];
   ];

mutationRate = 0.02;
parent = StringJoin[ alphabet[[RandomInteger[{1, Length@alphabet}, StringLength@target]]] ];
results = NestWhileList[Mutate[#, mutationRate, 100] &, parent, fitness[#] > 0 &];
fits = fitness[#] & /@ results;
results = Transpose[{results, fits}];
TableForm[results[[;; ;; 2]], TableHeadings->{Range[1, Length@results, 2],{"String","Fitness"}}, TableSpacing -> {1, 2}]

```


Output:

```txt
GBPQVCRDTMCPVZBRLLRKPF GXATW	28
GBTQVCKDTMTPVZBRLLEKPF GXATW	24
GBTQICKDTMTPVZBILLE PF GXATL	21
GBTQICKD ITPVZBILLE PF EXATL	18
GBTQICKD ITPVZBPILE PS EAAVL	16
GBTQICKS ITPVZBLILE A WEAAVL	11
GBTQICKS ITPVSBLILE A WEAAEL	9
METQICKS ITPVS LIHE A WEAAEL	6
METHICKS ITPIS LIKE A WEAAEL	3
METHINKS ITPIS LIKE A WEAYEL	2
METHINKS IT IS LIKE A WEAYEL	1
METHINKS IT IS LIKE A WEAYEL	1
METHINKS IT IS LIKE A WEATEL	1
METHINKS IT IS LIKE A WEATEL	1
METHINKS IT IS LIKE A WEATEL	1
METHINKS IT IS LIKE A WEAXEL	1
METHINKS IT IS LIKE A WEASEL	0
```



## MATLAB

This solution implements a class called EvolutionaryAlgorithm, the members of the class are the variables required by the task description. You can see them using the disp() function on an instance of the class. To use this class you only need to specify the target, mutation rate, number of children (called C in the task spec), and maximum number of evolutionary cycles. After doing so, call the evolve() function on the class instance to start the evolution cycle. Note, the fitness function computes the hamming distance between the target string and another string, this can be changed if a better heuristic exists.

To use this code, create a folder in your MATLAB directory titled "@EvolutionaryAlgorithm". Within that folder save this code in a file named "EvolutionaryAlgorithm.m".


```MATLAB
%This class impliments a string that mutates to a target
classdef EvolutionaryAlgorithm

    properties

        target;
        parent;
        children = {};
        validAlphabet;

        %Constants
        numChildrenPerIteration;
        maxIterations;
        mutationRate;

    end

    methods

        %Class constructor
        function family = EvolutionaryAlgorithm(target,mutationRate,numChildren,maxIterations)

            family.validAlphabet = char([32 (65:90)]); %Space char and A-Z
            family.target = target;
            family.children = cell(numChildren,1);
            family.numChildrenPerIteration = numChildren;
            family.maxIterations = maxIterations;
            family.mutationRate = mutationRate;
            initialize(family);

        end %class constructor

        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        %Helper functions and class get/set functions
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

        %setAlphabet() - sets the valid alphabet for the current instance
        %of the EvolutionaryAlgorithm class.
        function setAlphabet(family,alphabet)

            if(ischar(alphabet))
                family.validAlphabet = alphabet;

                %Makes change permanent
                assignin('caller',inputname(1),family);
            else
                error 'New alphabet must be a string or character array';
            end

        end

        %setTarget() - sets the target for the current instance
        %of the EvolutionaryAlgorithm class.
        function setTarget(family,target)

            if(ischar(target))
                family.target = target;

                %Makes change permanent
                assignin('caller',inputname(1),family);
            else
                error 'New target must be a string or character array';
            end

        end

        %setMutationRate() - sets the mutation rate for the current instance
        %of the EvolutionaryAlgorithm class.
        function setMutationRate(family,mutationRate)

            if(isnumeric(mutationRate))
                family.mutationRate = mutationRate;

                %Makes change permanent
                assignin('caller',inputname(1),family);
            else
                error 'New mutation rate must be a double precision number';
            end

        end

        %setMaxIterations() - sets the maximum number of iterations during
        %evolution for the current instance of the EvolutionaryAlgorithm class.
        function setMaxIterations(family,maxIterations)

            if(isnumeric(maxIterations))
                family.maxIterations = maxIterations;

                %Makes change permanent
                assignin('caller',inputname(1),family);
            else
                error 'New maximum amount of iterations must be a double precision number';
            end

        end

        %display() - overrides the built-in MATLAB display() function, to
        %display the important class variables
        function display(family)
            disp([sprintf('Target: %s\n',family.target)...
                  sprintf('Parent: %s\n',family.parent)...
                  sprintf('Valid Alphabet: %s\n',family.validAlphabet)...
                  sprintf('Number of Children: %d\n',family.numChildrenPerIteration)...
                  sprintf('Mutation Rate [0,1]: %d\n',family.mutationRate)...
                  sprintf('Maximum Iterations: %d\n',family.maxIterations)]);
        end

        %disp() - overrides the built-in MATLAB disp() function, to
        %display the important class variables
        function disp(family)
            display(family);
        end

        %randAlphabetElement() - Generates a random character from the
        %valid alphabet for the current instance of the class.
        function elements = randAlphabetElements(family,numChars)

            %Sample the valid alphabet randomly from the uniform
            %distribution
            N = length(family.validAlphabet);
            choices = ceil(N*rand(1,numChars));

            elements = family.validAlphabet(choices);

        end

        %initialize() - Sets the parent to a random string of length equal
        %to the length of the target
        function parent = initialize(family)

            family.parent = randAlphabetElements(family,length(family.target));
            parent = family.parent;

            %Makes changes to the instance of EvolutionaryAlgorithm permanent
            assignin('caller',inputname(1),family);

        end %initialize

        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        %Functions required by task specification
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

        %mutate() - generates children from the parent and mutates them
        function mutate(family)

            sizeParent = length(family.parent);

            %Generate mutatant children sequentially
            for child = (1:family.numChildrenPerIteration)

                parentCopy = family.parent;

                for charIndex = (1:sizeParent)
                    if (rand(1) < family.mutationRate)
                        parentCopy(charIndex) = randAlphabetElements(family,1);
                    end
                end

                family.children{child} = parentCopy;

            end

            %Makes changes to the instance of EvolutionaryAlgorithm permanent
            assignin('caller',inputname(1),family);

        end %mutate

        %fitness() - Computes the Hamming distance between the target
        %string and the string input as the familyMember argument
        function theFitness = fitness(family,familyMember)

            if not(ischar(familyMember))
                error 'The second argument must be a string';
            end

            theFitness = sum(family.target == familyMember);
        end

        %evolve() - evolves the family until the target is reached or it
        %exceeds the maximum amount of iterations
        function [iteration,mostFitFitness] = evolve(family)

            iteration = 0;
            mostFitFitness = 0;
            targetFitness = fitness(family,family.target);

            disp(['Target fitness is ' num2str(targetFitness)]);

            while (mostFitFitness < targetFitness) && (iteration < family.maxIterations)

                iteration = iteration + 1;

                mutate(family);

                parentFitness = fitness(family,family.parent);
                mostFit = family.parent;
                mostFitFitness = parentFitness;

                for child = (1:family.numChildrenPerIteration)

                    childFitness = fitness(family,family.children{child});
                    if childFitness > mostFitFitness
                        mostFit = family.children{child};
                        mostFitFitness = childFitness;
                    end

                end

                family.parent = mostFit;
                disp([num2str(iteration) ': ' mostFit ' - Fitness: ' num2str(mostFitFitness)]);

            end

            %Makes changes to the instance of EvolutionaryAlgorithm permanent
            assignin('caller',inputname(1),family);

        end %evolve

    end %methods
end %classdef
```

Sample Output: (Some evolutionary cycles omitted for brevity)

```matlab
>
 instance = EvolutionaryAlgorithm('METHINKS IT IS LIKE A WEASEL',.08,50,1000)
Target: METHINKS IT IS LIKE A WEASEL
Parent: UVEOCXXFBGDCSFNMJQNWTPJ PCVA
Valid Alphabet:  ABCDEFGHIJKLMNOPQRSTUVWXYZ
Number of Children: 50
Mutation Rate [0,1]: 8.000000e-002
Maximum Iterations: 1000

>> evolve(instance);
Target fitness is 28
1: MVEOCXXFBYD SFCMJQNWTPM PCVA - Fitness: 2
2: MEEOCXXFBYD SFCMJQNWTPM PCVA - Fitness: 3
3: MEEHCXXFBYD SFCMJXNWTPM ECVA - Fitness: 4
4: MEEHCXXFBYD SFCMJXNWTPM ECVA - Fitness: 4
5: METHCXAFBYD SFCMJXNWXPMARPVA - Fitness: 5
6: METHCXAFBYDFSFCMJXNWX MARSVA - Fitness: 6
7: METHCXKFBYDFBFCQJXNWX MATSVA - Fitness: 7
8: METHCXKFBYDFBF QJXNWX MATSVA - Fitness: 8
9: METHCXKFBYDFBF QJXNWX MATSVA - Fitness: 8
10: METHCXKFUYDFBF QJXNWX MITSEA - Fitness: 9
20: METHIXKF YTBOF LIKN G MIOSEI - Fitness: 16
30: METHIXKS YTCOF LIKN A MIOSEL - Fitness: 19
40: METHIXKS YTCIF LIKN A MEUSEL - Fitness: 21
50: METHIXKS YT IS LIKE A PEUSEL - Fitness: 24
100: METHIXKS YT IS LIKE A WEASEL - Fitness: 26
150: METHINKS YT IS LIKE A WEASEL - Fitness: 27
195: METHINKS IT IS LIKE A WEASEL - Fitness: 28
```




###  Genetic Algorithm Example

This solution uses a subset of evolutionary programming called the Genetic Algorithm. It is very similar to the basic evolutionary algorithm, but instead of just using mutations it also makes use of other genetic operators. The algorithm begins by importing the target text (in this case 'METHINKS IT IS LIKE A WEASEL') and then the algorithm performs genetic operations until the target string is obtained or the maximum number of iterations is reached (which will never happen with the given target string). The algorithm first measures how fit each potential answer is, and then selects strings to perform operations on. The selected answers go through the crossover stage where their data is split and recombined into new potential answers. Then a chance for the answer to mutate slightly occurs and the algorithm repeats itself.

Presented is very efficient and vectorized version of the genetic algorithm. To run the algorithm simply copy and paste the code into a script and hit run. You can adjust the style of selection and crossover used to learn more about how they effect solutions. The algorithm can also handle any target string that uses ASCII characters and will allow for any phrase to be used regardless of length.

```MATLAB

%% Genetic Algorithm -- Solves For A User Input String

% #### PLEASE NOTE: you can change the selection and crossover type in the
% parameters and see how the algorithm changes. ####

clear;close all;clc;    %Clears variables, closes windows, and clears the command window
tic                     % Begins the timer

%% Select Target String
target  = 'METHINKS IT IS LIKE A WEASEL';
% *Can Be Any String With Any Values and Any Length!*
% but for this example we use 'METHINKS IT IS LIKE A WEASEL'

%% Parameters
popSize = 1000;                                 % Population Size (100-10000 generally produce good results)
genome  = length(target);                       % Genome Size
mutRate = .01;                                  % Mutation Rate (5%-25% produce good results)
S       = 4;                                    % Tournament Size (2-6 produce good results)
best    = Inf;                                  % Initialize Best (arbitrarily large)
MaxVal  = max(double(target));                  % Max Integer Value Needed
ideal   = double(target);                       % Convert Target to Integers

selection = 0;                                  % 0: Tournament
                                                % 1: 50% Truncation

crossover = 1;                                  % 0: Uniform crossover
                                                % 1: 1 point crossover
                                                % 2: 2 point crossover
%% Initialize Population
Pop = round(rand(popSize,genome)*(MaxVal-1)+1); % Creates Population With Corrected Genome Length

for Gen = 1:1e6                                 % A Very Large Number Was Chosen, But Shouldn't Be Needed

    %% Fitness

    % The fitness function starts by converting the characters into integers and then
    % subtracting each element of each member of the population from each element of
    % the target string. The function then takes the absolute value of
    % the differences and sums each row and stores the function as a mx1 matrix.

    F = sum(abs(bsxfun(@minus,Pop,ideal)),2);



    % Finding Best Members for Score Keeping and Printing Reasons
    [current,currentGenome] = min(F);   % current is the minimum value of the fitness array F
                                        % currentGenome is the index of that value in the F array

    % Stores New Best Values and Prints New Best Scores
    if current < best
        best = current;
        bestGenome = Pop(currentGenome,:); % Uses that index to find best value

        fprintf('Gen: %d  |  Fitness: %d  |  ',Gen, best);  % Formatted printing of generation and fitness
        disp(char(bestGenome));                             % Best genome so far
    elseif best == 0
        break                                               % Stops program when we are done
    end

    %% Selection

    % TOURNAMENT
    if selection == 0
    T = round(rand(2*popSize,S)*(popSize-1)+1);                     % Tournaments
    [~,idx] = min(F(T),[],2);                                       % Index to Determine Winners
    W = T(sub2ind(size(T),(1:2*popSize)',idx));                     % Winners

    % 50% TRUNCATION
    elseif selection == 1
    [~,V] = sort(F,'descend');                                      % Sort Fitness in Ascending Order
    V = V(popSize/2+1:end);                                         % Winner Pool
    W = V(round(rand(2*popSize,1)*(popSize/2-1)+1))';               % Winners
    end

    %% Crossover

    % UNIFORM CROSSOVER
    if crossover == 0
    idx = logical(round(rand(size(Pop))));                          % Index of Genome from Winner 2
    Pop2 = Pop(W(1:2:end),:);                                       % Set Pop2 = Pop Winners 1
    P2A = Pop(W(2:2:end),:);                                        % Assemble Pop2 Winners 2
    Pop2(idx) = P2A(idx);                                           % Combine Winners 1 and 2

    % 1-POINT CROSSOVER
    elseif crossover == 1
    Pop2 = Pop(W(1:2:end),:);                                       % New Population From Pop 1 Winners
    P2A = Pop(W(2:2:end),:);                                        % Assemble the New Population
    Ref = ones(popSize,1)*(1:genome);                               % The Reference Matrix
    idx = (round(rand(popSize,1)*(genome-1)+1)*ones(1,genome))>Ref; % Logical Indexing
    Pop2(idx) = P2A(idx);                                           % Recombine Both Parts of Winners

    % 2-POINT CROSSOVER
    elseif crossover == 2
    Pop2 = Pop(W(1:2:end),:);                                       % New Pop is Winners of old Pop
    P2A  = Pop(W(2:2:end),:);                                       % Assemble Pop2 Winners 2
    Ref  = ones(popSize,1)*(1:genome);                              % Ones Matrix
    CP   = sort(round(rand(popSize,2)*(genome-1)+1),2);             % Crossover Points
    idx = CP(:,1)*ones(1,genome)<Ref&CP(:,2)*ones(1,genome)>Ref;    % Index
    Pop2(idx)=P2A(idx);                                             % Recombine Winners
    end
    %% Mutation
    idx = rand(size(Pop2))<mutRate;                                 % Index of Mutations
    Pop2(idx) = round(rand([1,sum(sum(idx))])*(MaxVal-1)+1);        % Mutated Value

    %% Reset Poplulations
    Pop = Pop2;

end

toc % Ends timer and prints elapsed time

```


Sample Output: (The Algorithm was run with 1000 population members, Tournament Selection (with tournament size of 4), 1-Point Crossover, and a mutation rate of 10%).

```MATLAB

Gen: 1  |  Fitness: 465  |  C�I1%G+<%?R�8>9�JU#(E�UO�PHI
Gen: 2  |  Fitness: 429  |  W=P6>D�I)VU6$T 99,� B�BMP0JH
Gen: 3  |  Fitness: 366  |  P�;R08AS�GJ�IS&T38IE�)SJERLJ
Gen: 4  |  Fitness: 322  |  KI8M5LAS�GJ�IS�SP�@)D�V@
JCP
Gen: 5  |  Fitness: 295  |  UAUR08AS�GJ�IS�8HG*�+�=C?UB(
Gen: 6  |  Fitness: 259  |  VCUQH35S�HR4.L�ISJQ%J�OC*T=E
Gen: 7  |  Fitness: 226  |  LFB8GPET(LODKQ�KQ<K	E*PEMA6I
Gen: 8  |  Fitness: 192  |  EPKOLCIR�QQ�NF�QG:B(D/U>BQGF
Gen: 9  |  Fitness: 159  |  N8R7?SOU�NO$OK O?K?!;�MB?QHG
Gen: 10  |  Fitness: 146  |  TGN@EQR4)PS%IS#TFJQ%A!U>BVLI
Gen: 11  |  Fitness: 120  |  L?VMALJS%?R EK IILE�6'RRERLJ
Gen: 12  |  Fitness: 102  |  R@T9COMR�NU CS*R?K?!; VD>LCL
Gen: 13  |  Fitness: 96  |  NENMVOMR�NU CS*R?K?!; VD>LCL
Gen: 14  |  Fitness: 82  |  REJGNPMU�KR CS JKI@+D�UD?QHG
Gen: 15  |  Fitness: 75  |  NETI=HPQ�FT ID EFKE D"WD>QDQ
Gen: 16  |  Fitness: 70  |  R@TKCOOT)@R$IS KKLE�D"WC?UBJ
Gen: 17  |  Fitness: 61  |  NESIKQRP�NU CS�MFKE ; SEETCP
Gen: 18  |  Fitness: 57  |  LFSGLPTN�NU GQ IIKE D"VD>LCL
Gen: 19  |  Fitness: 40  |  NENKJLMS�GS%IS#MFKE B UFATCL
Gen: 21  |  Fitness: 39  |  NETIGPEU�KR IS IIKD"? UFDQEK
Gen: 22  |  Fitness: 33  |  NETGCOMT�LU IS#MFKE B UFATCL
Gen: 23  |  Fitness: 32  |  NETIKNPQ�NU IS#IIKE B UFATCL
Gen: 24  |  Fitness: 27  |  NETKJLMS�LU IS MFKE B UFATCL
Gen: 25  |  Fitness: 23  |  LETIKOMS LU IS IIKE D WEDQEK
Gen: 26  |  Fitness: 22  |  NETIKMJS LU IS IIKE D WEDQEK
Gen: 27  |  Fitness: 20  |  LETIKOMS LU IS KILE B"WFATCL
Gen: 28  |  Fitness: 19  |  NESGJQJS�GU IS KIKE B WFATEK
Gen: 29  |  Fitness: 16  |  NETIHPMS KR IS KIKE B WFATEK
Gen: 30  |  Fitness: 15  |  NESHLPKS KU IS KIKE B WFATEK
Gen: 31  |  Fitness: 13  |  NETGGNKS KU IS KIKE C WFATEK
Gen: 32  |  Fitness: 12  |  NETHGNJS IU IS JIKE B WFATCL
Gen: 33  |  Fitness: 11  |  NETIJPKS IU IS KIKE B WFATEK
Gen: 35  |  Fitness: 8  |  LEUIHNJS IT IS JIKE A WEATEL
Gen: 37  |  Fitness: 7  |  NETIHNJS IS IS LIKE B WFASEL
Gen: 38  |  Fitness: 6  |  NETHGNJS IT IS LIKE A WFASEK
Gen: 39  |  Fitness: 4  |  METGHNKS IT IS LIKE B WEATEL
Gen: 42  |  Fitness: 3  |  NETHINKS IT IS KIKE B WEASEL
Gen: 43  |  Fitness: 2  |  NETHINKS IT IS LIKE A WFASEL
Gen: 44  |  Fitness: 1  |  METHHNKS IT IS LIKE A WEASEL
Gen: 46  |  Fitness: 0  |  METHINKS IT IS LIKE A WEASEL
Elapsed time is 0.099618 seconds.

```



## Nim

```nim
import math, os
randomize()

const
  target = "METHINKS IT IS LIKE A WEASEL"
  alphabet = " ABCDEFGHIJLKLMNOPQRSTUVWXYZ"
  p = 0.05
  c = 100

proc random(a: string): char = a[random(a.low..a.len)]

proc negFitness(trial): int =
  for i in 0 .. <trial.len:
    if target[i] != trial[i]: inc result

proc mutate(parent): string =
  result = ""
  for c in parent: result.add if random(1.0) < p: random(alphabet) else: c

var parent = ""
for i in 1..target.len: parent.add random(alphabet)

var i = 0
while parent != target:
  var copies = newSeq[string](c)
  for i in 0 .. <copies.len: copies[i] = mutate(parent)

  var best = copies[0]
  for i in 1 .. <copies.len:
    if negFitness(copies[i]) < negFitness(best): best = copies[i]
  parent = best

  echo i, " ", parent
  inc i
```

Sample output:

```txt
0 DDTAXEPAFNI RIKNLUBKPXKBFHGA
1 DDTJXEPAFNI RIKNLUB PXKBFHGA
2 CDTJXEPAFNI RI NLUB ZXKBFHGA
3 CDTJXEPAFNI RI KLUB ZXKEFHGA
[...]
37 METJINKS IT IS LIBE A WEANEL
[...]
70 MET INKS IT IS LIKE A WEASEL
71 METHINKS IT IS LIKE A WEASEL
```



## Objeck

```objeck
bundle Default {
  class Evolutionary {
    target : static : String;
    possibilities : static : Char[];
    C : static : Int;
    minMutateRate : static : Float;
    perfectFitness : static : Int;
    parent : static : String ;
    rand : static : Float;

    function : Init() ~ Nil {
      target := "METHINKS IT IS LIKE A WEASEL";
      possibilities := "ABCDEFGHIJKLMNOPQRSTUVWXYZ "->ToCharArray();
      C := 100;
      minMutateRate := 0.09;
      perfectFitness := target->Size();
    }

    function : fitness(trial : String) ~ Int {
        retVal := 0;

        each(i : trial) {
          if(trial->Get(i) = target->Get(i)) {
            retVal += 1;
          };
        };

        return retVal;
      }

      function : newMutateRate() ~ Float {
        x : Float := perfectFitness - fitness(parent);
        y : Float := perfectFitness->As(Float) * (1.01 - minMutateRate);

         return x / y;
      }

      function : mutate(parent : String, rate : Float) ~ String {
        retVal := "";

        each(i : parent) {
          rand := Float->Random();
          if(rand <= rate) {
            rand *= 1000.0;
            intRand := rand->As(Int);
            index : Int := intRand % possibilities->Size();
            retVal->Append(possibilities[index]);
          }
          else {
            retVal->Append(parent->Get(i));
          };
      };

        return retVal;
      }

      function : Main(args : String[]) ~ Nil {
        Init();
        parent := mutate(target, 1.0);

        iter := 0;
        while(target->Equals(parent) <> true) {
          rate := newMutateRate();
          iter += 1;

          if(iter % 100 = 0){
            IO.Console->Instance()->Print(iter)->Print(": ")->PrintLine(parent);
          };

          bestSpawn : String;
          bestFit := 0;

          for(i := 0; i < C; i += 1;) {
            spawn := mutate(parent, rate);
            fitness := fitness(spawn);

            if(fitness > bestFit) {
              bestSpawn := spawn;
              bestFit := fitness;
            };
          };

          if(bestFit > fitness(parent)) {
            parent := bestSpawn;
          };
        };
        parent->PrintLine();
      }
    }
  }
}
```


Output:

```txt

100: DETHILBMDEB QR YIEGYEBWCCSBN
200: D THIWTXEXH IO SVUDHEEWQASEL
300: DVTHINTILS RIO SVGEKNEWEASEU
400: MFTH AWBLIXNIE STFE AWWEASEJ
500: MFTHIAWDIIRMIY QTFE AWWEASEJ
600: MZTCIAKDQIRNIY NWFE A WEASEJ
700: MZTCIAKDQIRNIY NWFE A WEASEJ
800: MZTCIAKDQIRNIY NWFE A WEASEJ
900: MZTCIAKOWIRNIY NILE A WEASEJ
1000: MZTCIAKOWIRNIY NILE A WEASEJ
1100: MZTCIAKOWIRNIY NILE A WEASEJ
1200: MZTCIAKOWIRNIY NILE A WEASEJ
1300: METCITKSTIRSIY JYKE A WDASEJ
1400: METHITKSTIJ IB FYKE A WDASEJ
1500: METHINKSZIJ IB FYKE A WEASEQ
METHINKS IT IS LIKE A WEASEL
```



## OCaml



```ocaml
let target = "METHINKS IT IS LIKE A WEASEL"
let charset = "ABCDEFGHIJKLMNOPQRSTUVWXYZ "
let tlen = String.length target
let clen = String.length charset
let () = Random.self_init()

let parent =
  let s = String.create tlen in
  for i = 0 to tlen-1 do
    s.[i] <- charset.[Random.int clen]
  done;
  s

let fitness ~trial =
  let rec aux i d =
    if i >= tlen then d else
      aux (i+1) (if target.[i] = trial.[i] then d+1 else d) in
  aux 0 0

let mutate parent rate =
  let s = String.copy parent in
  for i = 0 to tlen-1 do
    if Random.float 1.0 > rate then
      s.[i] <- charset.[Random.int clen]
  done;
  s, fitness s

let () =
  let i = ref 0 in
  while parent <> target do
    let pfit = fitness parent in
    let rate = float pfit /. float tlen in
    let tries = Array.init 200 (fun _ -> mutate parent rate) in
    let min_by (a, fa) (b, fb) = if fa > fb then a, fa else b, fb in
    let best, f = Array.fold_left min_by (parent, pfit) tries in
    if !i mod 100 = 0 then
      Printf.printf "%5d - '%s'  (fitness:%2d)\n%!" !i best f;
    String.blit best 0 parent 0 tlen;
    incr i
  done;
  Printf.printf "%5d - '%s'\n" !i parent
```



## Octave

```octave
global target;
target = split("METHINKS IT IS LIKE A WEASEL", "");
charset = ["A":"Z", " "];
p = ones(length(charset), 1) ./ length(charset);
parent = discrete_rnd(charset, p, length(target), 1);
mutaterate = 0.1;

C = 1000;

function r = fitness(parent, target)
  r = sum(parent == target) ./ length(target);
endfunction

function r = mutate(parent, mutaterate, charset)
  r = parent;
  p = unifrnd(0, 1, length(parent), 1);
  nmutants = sum( p < mutaterate );
  if (nmutants)
    s = discrete_rnd(charset, ones(length(charset), 1) ./ length(charset),nmutants,1);
    r( p < mutaterate ) = s;
  endif
endfunction

function r = evolve(parent, mutatefunc, fitnessfunc, C, mutaterate, charset)
  global target;
  children = [];
  for i = 1:C
    children = [children, mutatefunc(parent, mutaterate, charset)];
  endfor
  children = [parent, children];
  fitval = [];
  for i = 1:columns(children)
    fitval = [fitval, fitnessfunc(children(:,i), target)];
  endfor
  [m, im] = max(fitval);
  r = children(:, im);
endfunction

function printgen(p, t, i)
  printf("%3d %5.2f %s\n", i, fitness(p, t), p');
endfunction

i = 0;

while( !all(parent == target) )
  i++;
  parent = evolve(parent, @mutate, @fitness, C, mutaterate, charset);
  if ( mod(i, 1) == 0 )
    printgen(parent, target, i);
  endif
endwhile
disp(parent');

```



## Oforth



```oforth
200 Constant new: C
  5 Constant new: RATE

: randChar  // -- c
   27 rand dup 27 == ifTrue: [ drop ' ' ] else: [ 'A' + 1- ] ;

: fitness(a b -- n)
   a b zipWith(#==) sum ;

: mutate(s -- s')
   s map(#[ 100 rand RATE <= ifTrue: [ drop randChar ] ]) charsAsString ;

 : evolve(target)
| parent |
   ListBuffer init(target size, #randChar) charsAsString ->parent

   1 while ( parent target <> ) [
      ListBuffer init(C, #[ parent mutate ]) dup add(parent)
      maxFor(#[ target fitness ]) dup ->parent . dup println 1+
      ] drop ;
```


```txt

>evolve("METHINKS IT IS LIKE A WEASEL")
WHQHNXXAWACZKTTIHKVBCYLPATN  1
WHQHNXXAWACZKTTIHKV CYLPATN  2
WHQHNXXAWACZKTTIHKV C LPATC  3
WHQHNXXSWATZKTTIHKV C LPATC  4
WHQHNXXSWATCKTTIHKV C LEATC  5
WHQHNXXSWATCKTTIHKV C LEATCL 6
WHQHNXXSWATCKFTIHKV C LEASCL 7
WHQHNXXSWATCKF IHKV C LEASCL 8
WHQHNXXSWATZKF IHKV A LEASCL 9
MHQHNXXSWATZKF IHKV A LEASCL 10
MATHNXXSWATZKF ICKV A LEASCL 11
MATHIXXSBATZKF ICKV A LEASCL 12
MATHIXXSBATZKS ICKV A LEASCL 13
MATHIXXSBATZKS BCKV A LEASCL 14
MATHIXXSBATZKS LCKV A LEASCL 15
MATHIXXS ATZKS LSKV A LEASCL 16
MATHIXXS ATJKS LSKV A LEASEL 17
METHIXXS ATJKS LSKV A LEASEL 18
METHIXXS ATJKS LSKE A LEASEL 19
METHINXS ATJKS LSKE A LEASEL 20
METHINXS ATJKS LSKE A WEASEL 21
METHINKS ATJKS LSKE A WEASEL 22
METHINKS ATJUS LSKE A WEASEL 23
METHINKS ATJUS LSKE A WEASEL 24
METHINKS ATJIS LSKE A WEASEL 25
METHINKS ATJIS LSKE A WEASEL 26
METHINKS ATJIS LIKE A WEASEL 27
METHINKS ATJIS LIKE A WEASEL 28
METHINKS STJIS LIKE A WEASEL 29
METHINKS STJIS LIKE A WEASEL 30
METHINKS OT IS LIKE A WEASEL 31
METHINKS OT IS LIKE A WEASEL 32
METHINKS OT IS LIKE A WEASEL 33
METHINKS OT IS LIKE A WEASEL 34
METHINKS OT IS LIKE A WEASEL 35
METHINKS IT IS LIKE A WEASEL 36
ok

```



## OoRexx

Run with Open Object Rexx 4.1.0 by IBM Corporation 1995,2004 Rexx LA 2005-2010. Host OS: Microsoft Windows 7.

```oorexx

/* Weasel.rex - Me thinks thou art a weasel. - G,M.D. - 2/25/2011 */
arg C M
/* C is the number of children parent produces each generation. */
/* M is the mutation rate of each gene (character) */

call initialize
generation = 0
do until parent = target
   most_fitness = fitness(parent)
   most_fit     = parent
   do C
      child = mutate(parent, M)
      child_fitness = fitness(child)
      if child_fitness > most_fitness then
      do
         most_fitness = child_fitness
         most_fit = child
         say "Generation" generation": most fit='"most_fit"', fitness="left(most_fitness,4)
      end
   end
   parent = most_fit
   generation = generation + 1
end
exit

initialize:
   target   = "METHINKS IT IS LIKE A WEASEL"
   alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ "
   c_length_target = length(target)
   parent  = mutate(copies(" ", c_length_target), 1.0)
   do i = 1 to c_length_target
      target_ch.i = substr(target,i,1)
   end
return

fitness: procedure expose target_ch. c_length_target
   arg parm_string
   fitness = 0
   do i_target = 1 to c_length_target
      if substr(parm_string,i_target,1) = target_ch.i_target then
         fitness = fitness + 1
   end
return fitness

mutate:procedure expose alphabet
arg string, parm_mutation_rate
   result = ""
   do istr = 1 to length(string)
      if random(1,1000)/1000 <= parm_mutation_rate then
         result = result || substr(alphabet,random(1,length(alphabet)),1)
      else
         result = result || substr(string,istr,1)
   end
return result

```

Output:
<div style='height: 15em; overflow: scroll'>
```txt

C:\usr\rex>weasel 10 .01
Generation 20, most fit='BZTACOQCQ CTMPIXPXBVKRUCLY F', fitness=1
Generation 30, most fit='BZTHCOQCQ CTMPIXPXBVKRUCLY F', fitness=2
Generation 34, most fit='BZTHCOQSQ CTMPIXPXBVKRUCLY F', fitness=3
Generation 61, most fit='BZTHCOQSQ CTIPIXPXBVKRUCLY F', fitness=4
Generation 95, most fit='BZTHCNQSQ CTIPIXPXBVKRUCLY F', fitness=5
Generation 107, most fit='BZTHCNQSQ CTISIXPXBVKRUCLY F', fitness=6
Generation 121, most fit='BZTHCNQS  CTISIXPXBVKRUCLY F', fitness=7
Generation 129, most fit='BZTHCNQS  CTISIXPXBVKRUELY F', fitness=8
Generation 142, most fit='BZTHCNQS  CTISIXPXBVKRUELS F', fitness=9
Generation 143, most fit='BZTHCNQS ICTISIXPXBVKRUEHS F', fitness=10
Generation 147, most fit='BZTHCNQS ICTISIXPXBVKRUEHS L', fitness=11
Generation 154, most fit='BZTHCNQS IC ISIXPXBVKRUEHS L', fitness=12
Generation 201, most fit='BZTHCNQS IT ISIXPXBVKRUEHS L', fitness=13
Generation 213, most fit='BZTHCNQS IT ISIXPXEVKRUEHS L', fitness=14
Generation 250, most fit='BZTHCNKS IT ISIXPXEVKRUEHS L', fitness=15
Generation 268, most fit='BZTHCNKS IT ISIXPXEVKFUEAS L', fitness=16
Generation 274, most fit='BZTHCNKS IT ISIXPKEVKFUEAS L', fitness=17
Generation 292, most fit='BZTHCNKS IT ISIXPKEVKFWEAS L', fitness=18
Generation 353, most fit='BZTHCNKS IT ISIXPKEVKFWEASEL', fitness=19
Generation 358, most fit='BZTHCNKS IT ISIXPKEVK WEASEL', fitness=20
Generation 374, most fit='BETHCNKS IT ISIXPKEVK WEASEL', fitness=21
Generation 404, most fit='BETHCNKS IT ISILPKEVK WEASEL', fitness=22
Generation 405, most fit='BETHCNKS IT ISILPKE K WEASEL', fitness=23
Generation 448, most fit='FETHCNKS IT ISILPKE A WEASEL', fitness=24
Generation 679, most fit='FETHINKS IT ISILPKE A WEASEL', fitness=25
Generation 964, most fit='METHINKS IT ISILPKE A WEASEL', fitness=26
Generation 1018, most fit='METHINKS IT ISILIKE A WEASEL', fitness=27
Generation 1250, most fit='METHINKS IT IS LIKE A WEASEL', fitness=28

C:\usr\rex>

```
</div>


## OxygenBasic

The algorithm pared down to the essentials. It takes around 1200 to 6000 mutations to attain the target. Fitness is measured by the number of beneficial mutations. The cycle ends when this is equal to the string length.

```oxygenbasic


'EVOLUTION

target="METHINKS IT IS LIKE A WEASEL"
le=len target
progeny=string le,"X"

quad seed
declare QueryPerformanceCounter lib "kernel32.dll" (quad*q)
QueryPerformanceCounter seed

Function Rand(sys max) as sys
  mov    eax,max
  inc    eax
  imul   edx,seed,0x8088405
  inc    edx
  mov    seed,edx
  mul    edx
  return edx
End Function

sys ls=le-1,cp=0,ct=0,ch=0,fit=0,gens=0

do                         '1 mutation per generation
  i=1+rand ls              'mutation position
  ch=64+rand 26            'mutation ascii code
  if ch=64 then ch=32      'change '@' to ' '
  ct=asc target,i          'target ascii code
  cp=asc progeny,i         'parent ascii code
  '
  if ch=ct then
    if cp<>ct then
      mid progeny,i,chr ch 'carry improvement
      fit++                'increment fitness
    end if
  end if
  gens++
  if fit=le then exit do   'matches target
end do
print progeny "  " gens 'RESULT (range 1200-6000 generations)

```



## Oz


```oz
declare
  Target = "METHINKS IT IS LIKE A WEASEL"
  C = 100
  MutateRate = 5 %% percent

  proc {Main}
     X0 = {MakeN {Length Target} RandomChar}
  in
     for Xi in {Iterate Evolve X0} break:Break do
        {System.showInfo Xi}
        if Xi == Target then {Break} end
     end
  end

  fun {Evolve Xi}
     Copies = {MakeN C fun {$} {Mutate Xi} end}
  in
     {FoldL Copies MaxByFitness Xi}
  end

  fun {Mutate Xs}
     {Map Xs
      fun {$ X}
         if {OS.rand} mod 100 < MutateRate then {RandomChar}
         else X
         end
      end}
  end

  fun {MaxByFitness A B}
     if {Fitness B} > {Fitness A} then B else A end
  end

  fun {Fitness Candidate}
     {Length {Filter {List.zip Candidate Target Value.'=='} Id}}
  end

  Alphabet = & |{List.number &A &Z 1}
  fun {RandomChar}
     I = {OS.rand} mod {Length Alphabet} + 1
  in
     {Nth Alphabet I}
  end

  %% General purpose helpers

  fun {Id X} X end

  fun {MakeN N F}
     Xs = {List.make N}
  in
     {ForAll Xs F}
     Xs
  end

  fun lazy {Iterate F X}
     X|{Iterate F {F X}}
  end
in
  {Main}
```



## PARI/GP

The algorithm given here is more general than the one described, in which letters can be inserted or deleted as well as mutated. The rate for insertions and deletions are set to 0, however, so the results are the same.

This code is inefficient (tens of milliseconds) since it converts back and forth between string and vector format. A more efficient version would keep the information in a Vecsmall instead.

```parigp
target="METHINKS IT IS LIKE A WEASEL";
fitness(s)=-dist(Vec(s),Vec(target));
dist(u,v)=sum(i=1,min(#u,#v),u[i]!=v[i])+abs(#u-#v);
letter()=my(r=random(27)); if(r==26, " ", Strchr(r+65));
insert(v,x=letter())=
{
	my(r=random(#v+1));
	if(r==0, return(concat([x],v)));
	if(r==#v, return(concat(v,[x])));
	concat(concat(v[1..r],[x]),v[r+1..#v]);
}
delete(v)=
{
	if(#v<2, return([]));
	my(r=random(#v)+1);
	if(r==1, return(v[2..#v]));
	if(r==#v, return(v[1..#v-1]));
	concat(v[1..r-1],v[r+1..#v]);
}
mutate(s,rateM,rateI,rateD)=
{
	my(v=Vec(s));
	if(random(1.)<rateI, v=insert(v));
	if(random(1.)<rateD, v=delete(v));
	for(i=1,#v,
		if(random(1.)<rateM, v[i]=letter())
	);
	concat(v);
}
evolve(C,rate)=
{
	my(parent=concat(vector(#target,i,letter())),ct=0);
	while(parent != target,
		print(parent" "fitness(parent));
		my(v=vector(C,i,mutate(parent,rate,0,0)),best,t);
		best=fitness(parent=v[1]);
		for(i=2,C,
			t=fitness(v[i]);
			if(t>best, best=t; parent=v[i])
		);
		ct++
	);
	print(parent" "fitness(parent));
	ct;
}
evolve(35,.05)
```



## Pascal


This Pascal version of the program displays the initial random string and every hundredth generation after that. It also displays the final generation count. Mutation happens relatively slowly, about once in every 1000 characters, but this can be changed by altering the RATE constant. Lower values for RATE actually speed up the mutations.


```pascal
PROGRAM EVOLUTION (OUTPUT);

CONST
	TARGET = 'METHINKS IT IS LIKE A WEASEL';
	COPIES = 100;  (* 100 children in each generation. *)
	RATE = 1000;  (* About one character in 1000 will be a mutation. *)

TYPE
	STRLIST = ARRAY [1..COPIES] OF STRING;

FUNCTION RANDCHAR : CHAR;
 (* Generate a random letter or space. *)
 VAR RANDNUM : INTEGER;
 BEGIN
	RANDNUM := RANDOM(27);
	IF RANDNUM = 26 THEN
		RANDCHAR := ' '
	ELSE
		RANDCHAR := CHR(RANDNUM + ORD('A'))
 END;

FUNCTION RANDSTR (SIZE : INTEGER) : STRING;
 (* Generate a random string. *)
 VAR
	N : INTEGER;
	S : STRING;
 BEGIN
	S := '';
	FOR N := 1 TO SIZE DO
		INSERT(RANDCHAR, S, 1);
	RANDSTR := S
 END;

FUNCTION FITNESS (CANDIDATE, GOAL : STRING) : INTEGER;
 (* Count the number of correct letters in the correct places *)
 VAR N, MATCHES : INTEGER;
 BEGIN
	MATCHES := 0;
	FOR N := 1 TO LENGTH(GOAL) DO
		IF CANDIDATE[N] = GOAL[N] THEN
			MATCHES := MATCHES + 1;
	FITNESS := MATCHES
 END;

FUNCTION MUTATE (RATE : INTEGER; S : STRING) : STRING;
 (* Randomly alter a string. Characters change with probability 1/RATE. *)
 VAR
	N : INTEGER;
	CHANGE : BOOLEAN;
 BEGIN
	FOR N := 1 TO LENGTH(TARGET) DO
	 BEGIN
		CHANGE := RANDOM(RATE) = 0;
		IF CHANGE THEN
			S[N] := RANDCHAR
	 END;
	MUTATE := S
 END;

PROCEDURE REPRODUCE (RATE : INTEGER; PARENT : STRING; VAR CHILDREN : STRLIST);
 (* Generate children with random mutations. *)
 VAR N : INTEGER;
 BEGIN
	FOR N := 1 TO COPIES DO
		CHILDREN[N] := MUTATE(RATE, PARENT)
 END;

FUNCTION FITTEST(CHILDREN : STRLIST; GOAL : STRING) : STRING;
 (* Measure the fitness of each child and return the fittest. *)
 (* If multiple children equally match the target, then return the first. *)
 VAR
	MATCHES, MOST_MATCHES, BEST_INDEX, N : INTEGER;
 BEGIN
	MOST_MATCHES := 0;
	BEST_INDEX := 1;
	FOR N := 1 TO COPIES DO
	 BEGIN
		MATCHES := FITNESS(CHILDREN[N], GOAL);
		IF MATCHES > MOST_MATCHES THEN
		 BEGIN
			MOST_MATCHES := MATCHES;
			BEST_INDEX := N
		 END
	 END;
	FITTEST := CHILDREN[BEST_INDEX]
 END;

VAR
	PARENT, BEST_CHILD : STRING;
	CHILDREN : STRLIST;
	GENERATIONS : INTEGER;

BEGIN
	RANDOMIZE;
	GENERATIONS := 0;
	PARENT := RANDSTR(LENGTH(TARGET));
	WHILE NOT (PARENT = TARGET) DO
	 BEGIN
		IF (GENERATIONS MOD 100) = 0 THEN WRITELN(PARENT);
		GENERATIONS := GENERATIONS + 1;
		REPRODUCE(RATE, PARENT, CHILDREN);
		BEST_CHILD := FITTEST(CHILDREN, TARGET);
		IF FITNESS(PARENT, TARGET) < FITNESS(BEST_CHILD, TARGET) THEN
			PARENT := BEST_CHILD
	 END;
	WRITE('The string was matched in ');
	WRITELN(GENERATIONS, ' generations.')
END.
```



## Perl


This implementation usually converges in less than 70 iterations.


```perl
use List::Util 'reduce';
use List::MoreUtils 'false';

### Generally useful declarations

sub randElm
 {$_[int rand @_]}

sub minBy (&@)
 {my $f = shift;
  reduce {$f->($b) < $f->($a) ? $b : $a} @_;}

sub zip
 {@_ or return ();
  for (my ($n, @a) = 0 ;; ++$n)
    {my @row;
     foreach (@_)
        {$n < @$_ or return @a;
         push @row, $_->[$n];}
     push @a, \@row;}}

### Task-specific declarations

my $C = 100;
my $mutation_rate = .05;
my @target = split '', 'METHINKS IT IS LIKE A WEASEL';
my @valid_chars = (' ', 'A' .. 'Z');

sub fitness
 {false {$_->[0] eq $_->[1]} zip shift, \@target;}

sub mutate
 {my $rate = shift;
  return [map {rand() < $rate ? randElm @valid_chars : $_} @{shift()}];}

### Main loop

my $parent = [map {randElm @valid_chars} @target];

while (fitness $parent)
   {$parent =
       minBy \&fitness,
       map {mutate $mutation_rate, $parent}
       1 .. $C;
    print @$parent, "\n";}
```




## Perl 6

```perl6
constant target = "METHINKS IT IS LIKE A WEASEL";
constant mutate_chance = .08;
constant @alphabet = flat 'A'..'Z',' ';
constant C = 100;

sub mutate { [~] (rand < mutate_chance ?? @alphabet.pick !! $_ for $^string.comb) }
sub fitness { [+] $^string.comb Zeq target.comb }

loop (
    my $parent = @alphabet.roll(target.chars).join;
    $parent ne target;
    $parent = max :by(&fitness), mutate($parent) xx C
) { printf "%6d: '%s'\n", $++, $parent }
```



## Phix


```Phix
constant target = "METHINKS IT IS LIKE A WEASEL",
         AZS    = "ABCDEFGHIJKLMNOPQRSTUVWXYZ ",
         C = 5000,  -- children in each generation
         P = 15     -- probability of mutation (1 in 15)

function fitness(string sample, string target)
    return sum(sq_eq(sample,target))
end function

function mutate(string s, integer n)
    for i=1 to length(s) do
        if rand(n)=1 then
            s[i] = AZS[rand(length(AZS))]
        end if
    end for
    return s
end function

string parent = mutate(target,1) -- (mutate with 100% probability)
sequence samples = repeat(0,C)
integer gen = 0, best, fit, best_fit = fitness(parent,target)
while parent!=target do
    printf(1,"Generation%3d: %s, fitness %3.2f%%\n", {gen, parent, (best_fit/length(target))*100})
    best_fit = -1
    for i=1 to C do
        samples[i] = mutate(parent, P)
        fit = fitness(samples[i], target)
        if fit > best_fit then
            best_fit = fit
            best = i
        end if
    end for
    parent = samples[best]
    gen += 1
end while
printf(1,"Finally, \"%s\"\n",{parent})
```

```txt

Generation  0: NKY NWLYBJOPOJFE RRISKGJD RS, fitness 0.00%
Generation  1: NKYHNNLYAIOPOJFE ERISKGJD RS, fitness 10.71%
Generation  2: NKYHNNLYAIOPOJFEIER SKGJD RS, fitness 17.86%
Generation  3: IKYHNNLSAIOPOJFLIER SKGJW RS, fitness 25.00%
Generation  4: MKTHNNLSAIOPOJILIER SKGJW RS, fitness 32.14%
Generation  5: MKTHNNLSAITFOJILIEE SKGJW RS, fitness 39.29%
Generation  6: MKTHONLSAITFOJILIEE SKGJW EL, fitness 46.43%
Generation  7: MKTHINLSAITFIJILIIE SKJJW EL, fitness 53.57%
Generation  8: MKTHINLSAITFIS LIIE SKJJW EL, fitness 60.71%
Generation  9: MKTHINLSAITFIS LIKE AKJJW EL, fitness 67.86%
Generation 10: MKTHINLSAITFIS LIKE AKJEA EL, fitness 75.00%
Generation 11: METHINLSAIT IS LIKE AKJEA EL, fitness 82.14%
Generation 12: METHINLSAIT IS LIKE AKWEA EL, fitness 85.71%
Generation 13: METHINLS IT IS LIKE AKWEA EL, fitness 89.29%
Generation 14: METHINLS IT IS LIKE A WEA EL, fitness 92.86%
Generation 15: METHINLS IT IS LIKE A WEASEL, fitness 96.43%
Finally, "METHINKS IT IS LIKE A WEASEL"

```



## PicoLisp

This example uses 'gen', the genetic function in "lib/simul.l"

```PicoLisp
(load "@lib/simul.l")

(setq *Target (chop "METHINKS IT IS LIKE A WEASEL"))

# Generate random character
(de randChar ()
   (if (=0 (rand 0 26))
      " "
      (char (rand `(char "A") `(char "Z"))) ) )

# Fitness function (Hamming distance)
(de fitness (A)
   (cnt = A *Target) )

# Genetic algorithm
(gen
   (make                               # Parent population
      (do 100                             # C = 100 children
         (link
            (make
               (do (length *Target)
                  (link (randChar)) ) ) ) ) )
   '((A)                               # Termination condition
      (prinl (maxi fitness A))            # Print the fittest element
      (member *Target A) )                # and check if solution is found
   '((A B)                             # Recombination function
      (mapcar
         '((C D) (if (rand T) C D))       # Pick one of the chars
         A B ) )
   '((A)                               # Mutation function
      (mapcar
         '((C)
            (if (=0 (rand 0 10))          # With a proability of 10%
               (randChar)                 # generate a new char, otherwise
               C ) )                      # return the current char
         A ) )
   fitness )                           # Selection function
```

Output:

```txt
RQ ASLWWWI ANSHPNABBAJ ZLTKX
DETGGNGHWITIKSXLIIEBA WAATPC
CETHINWS ITKESQGIKE A WSAGHO
METHBNWS IT NSQLIKE A WEAEWL
METHINKS IT ISCLIKE A WVASEL
METHINKS IT ISOLIKE A WEASEL
METHINKS IT IS LIKE A WEASEL
```



## PHP


```php

define('TARGET','METHINKS IT IS LIKE A WEASEL');
define('TBL','ABCDEFGHIJKLMNOPQRSTUVWXYZ ');

define('MUTATE',15);
define('COPIES',30);
define('TARGET_COUNT',strlen(TARGET));
define('TBL_COUNT',strlen(TBL));

// Determine number of different chars between a and b

function unfitness($a,$b)
{
        $sum=0;
        for($i=0;$i<strlen($a);$i++)
                if($a[$i]!=$b[$i]) $sum++;
        return($sum);
}

function mutate($a)
{
        $tbl=TBL;
        for($i=0;$i<strlen($a);$i++) $out[$i]=mt_rand(0,MUTATE)?$a[$i]:$tbl[mt_rand(0,TBL_COUNT-1)];
        return(implode('',$out));
}


$tbl=TBL;
for($i=0;$i<TARGET_COUNT;$i++) $tspec[$i]=$tbl[mt_rand(0,TBL_COUNT-1)];
$parent[0]=implode('',$tspec);
$best=TARGET_COUNT+1;
$iters=0;
do {
        for($i=1;$i<COPIES;$i++) $parent[$i]=mutate($parent[0]);

        for($best_i=$i=0; $i<COPIES;$i++) {
                $unfit=unfitness(TARGET,$parent[$i]);
                if($unfit < $best || !$i) {
                        $best=$unfit;
                        $best_i=$i;
                }
        }
        if($best_i>0) $parent[0]=$parent[$best_i];
        $iters++;
        print("Generation $iters, score $best: $parent[0]\n");
} while($best);


```

Sample Output:

```txt

Generation 1, score 25: IIVHUVOC NRGYBUEXLF LXZ SGMT
Generation 2, score 24: MIVHUVOC MKGYBUEXLF LXZ HGMT
Generation 3, score 24: MIVHUVOC MKGYBUEXLF LXZ HGMT
...
Generation 177, score 1: METHQNKS IT IS LIKE A WEASEL
Generation 178, score 0: METHINKS IT IS LIKE A WEASEL

```




## Pike

C is not used because i found it has no effect on the number of mutations needed to find the solution. in difference to the proposal, rate is not set as a percentage but as the number of characters to mutate when generating an offspring.

the rate is fixed at 2 as that is the lowest most successful rate still in the spirit of the original proposal (where mutation allows a previously successful change to be undone). if the rate is 1 than every successful character change can not change again (because it would not cause an improvement and thus be rejected.)


```Pike
string chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ ";

string mutate(string data, int rate)
{
    array(int) alphabet=(array(int))chars;
    multiset index = (multiset)enumerate(sizeof(data));
    while(rate)
    {
        int pos = random(index);
        data[pos]=random(alphabet);
        rate--;
    }
    return data;
}

int fitness(string input, string target)
{
    return `+(@`==(((array)input)[*], ((array)target)[*]));
}

void main()
{
    array(string) alphabet = chars/"";
    string target = "METHINKS IT IS LIKE A WEASEL";
    string parent = "";

    while(sizeof(parent) != sizeof(target))
    {
        parent += random(alphabet);
    }

    int count;
    write(" %5d: %s\n", count, parent);
    while (parent != target)
    {
        string child = mutate(parent, 2);
        count++;
        if (fitness(child, target) > fitness(parent, target))
        {
            write(" %5d: %s\n", count, child);
            parent = child;
        }
    }
}
```


Output:
     0: TIRABZB IGVG TDXTGODFOXO UPU
     2: TIRABZB IGVG TDXTGO FOXOTUPU
    32: TIRABZB IGVG T XTGO FOXOTUPU
    39: TIRABZB IGVG T JTGO AOXOTUPU
    44: TIRABNB IGMG T JTGO AOXOTUPU
    57: TIRABNB IGMG T ITGO AOXOTSPU
    62: TISHBNB IGMG T ITGO AOXOTSPU
    63: TISHBNB IGM  T ITGO AOXONSPU
    74: TISHBNB  GM  T ITGO AOHONSPU
    89: TISHBNB  GM  S ITGO AYHONSPU
   111: TISHBNB  GM  S ITGO AYHOASPU
   112: MISHBNB  GM  S ITGO AYHUASPU
   145: MISHBNBG IM  S ITGO AYHUASPU
   169: MISHBNBG IM NS ITGO AYHEASPU
   182: MESHBNBG IM NS ATGO AYHEASPU
   257: MESHBNBG ID NS ATGO A HEASPU
   320: MESHBNBG ID NS LRGO A HEASPU
   939: MESHINBG ID NS LRGO A HEASPU
  1134: MESHINBG ID NS LRZO A HEASEU
  1264: MESHINBG ID US LIZO A HEASEU
  1294: MEYHINBG IT US LIZO A HEASEU
  1507: MEYHINBG IT US LIZO A HEASEL
  1823: METHINBG IT US LIZO A HEASEL
  2080: METHINBG IT US LI E A HEASEL
  2143: METHINBG IT IS LI E A HEASEL
  3118: METHINWG IT IS LIKE A HEASEL
  3260: METHINWC IT IS LIKE A WEASEL
  3558: METHINWS IT IS LIKE A WEASEL
  4520: METHINKS IT IS LIKE A WEASEL


## Pony

```Pony
use "random"

actor Main
  let _env: Env
  let _rand: MT = MT	// Mersenne Twister
  let _target: String = "METHINKS IT IS LIKE A WEASEL"
  let _possibilities: String = "ABCDEFGHIJKLMNOPQRSTUVWXYZ "
  let _c: U16 = 100	// number of spawn per generation
  let _min_mutate_rate: F64 = 0.09
  let _perfect_fitness: USize = _target.size()
  var _parent: String = ""

  new create(env: Env) =>
    _env = env
    _parent = mutate(_target, 1.0)
    var iter: U64 = 0
    while not _target.eq(_parent) do
      let rate: F64 = new_mutate_rate()
      iter = iter + 1
      if (iter % 100) == 0 then
        _env.out.write(iter.string() + ": " + _parent)
        _env.out.write(", fitness: " + fitness(_parent).string())
        _env.out.print(", rate: " + rate.string())
      end
      var best_spawn = ""
      var best_fit: USize = 0
      var i: U16 = 0
      while i < _c do
        let spawn = mutate(_parent, rate)
        let spawn_fitness = fitness(spawn)
        if spawn_fitness > best_fit then
          best_spawn = spawn
          best_fit = spawn_fitness
        end
        i = i + 1
      end
      if best_fit > fitness(_parent) then
        _parent = best_spawn
      end
    end
    _env.out.print(_parent + ", " + iter.string())

  fun fitness(trial: String): USize =>
    var ret_val: USize = 0
    var i: USize = 0
    while i < trial.size() do
      try
        if trial(i)? == _target(i)? then
          ret_val = ret_val + 1
        end
      end
      i = i + 1
    end
    ret_val

  fun new_mutate_rate(): F64 =>
    let perfect_fit = _perfect_fitness.f64()
    ((perfect_fit - fitness(_parent).f64()) / perfect_fit) * (1.0 - _min_mutate_rate)

  fun ref mutate(parent: String box, rate: F64): String =>
    var ret_val = recover trn String end
    for char in parent.values() do
      let rnd_real: F64 = _rand.real()
      if rnd_real <= rate then
        let rnd_int: U64 = _rand.int(_possibilities.size().u64())
        try
          ret_val.push(_possibilities(rnd_int.usize())?)
        end
      else
        ret_val.push(char)
      end
    end
    consume ret_val
```

Output:

```txt
100: UMMMDNKR IEIIB IIKZ A THAHEL, fitness: 14, rate: 0.455
200: UMMMDNKR IEIIB IIKZ A THAHEL, fitness: 14, rate: 0.455
300: KMHJZNKS IUIIS IISQ A TWASEL, fitness: 16, rate: 0.39
400: KHHHCNKS IT I  CIKE A XFASEL, fitness: 20, rate: 0.26
500: MINHINKS IT IS LIKE A WEASEL, fitness: 26, rate: 0.065
METHINKS IT IS LIKE A WEASEL, 526
```


'''Alternative solution:'''

Using a more OO approach that leverages classes for encapsulation.

```Pony
use "random"
use "collections"

class CreationFactory
  let _desired: String

  new create(d: String) =>
    _desired = d

  fun apply(c: String): Creation =>
    Creation(c, _fitness(c))

  fun _fitness(s: String): USize =>
    var f = USize(0)
    for i in Range(0, s.size()) do
      try
        if s(i)? == _desired(i)? then
          f = f +1
        end
      end
    end
    f

class val Creation
  let string: String
  let fitness: USize

  new val create(s: String = "", f: USize = 0) =>
    string = s
    fitness = f

class Mutator
  embed _rand: MT = MT
  let _possibilities: String = "ABCDEFGHIJKLMNOPQRSTUVWXYZ "
  let _cf: CreationFactory

  new create(cf: CreationFactory) =>
    _cf = cf

  fun ref apply(parent: Creation, rate: F64): Creation =>
    let ns = _new_string(parent.string, rate)
    _cf(ns)

  fun ref _new_string(parent: String, rate: F64): String =>
    var mutated = recover String(parent.size()) end
    for char in parent.values() do
      mutated.push(_mutate_letter(char, rate))
    end
    consume mutated

  fun ref _mutate_letter(current: U8, rate: F64): U8 =>
    if _rand.real() <= rate then
      _random_letter()
    else
      current
    end

  fun ref _random_letter(): U8 =>
    let ln = _rand.int(_possibilities.size().u64()).usize()
    try _possibilities(ln)? else ' ' end

class Generation
  let _size: USize
  let _desired: Creation
  let _mutator: Mutator

  new create(size: USize = 100, desired: Creation, mutator: Mutator) =>
    _size = size
    _desired = desired
    _mutator = consume mutator

  fun ref apply(parent: Creation): Creation =>
    var best = parent
    let mutation_rate = _mutation_rate(best)
    for i in Range(0, _size) do
      let candidate = _mutator(best, mutation_rate)
      if candidate.fitness > best.fitness then
        best = candidate
      end
    end
    best

  fun _mutation_rate(best: Creation): F64 =>
    let min_mutate_rate: F64 = 0.09

    let df = _desired.fitness.f64()
    let bf = best.fitness.f64()

    ((df - bf) / df) * (1.0 - min_mutate_rate)

actor Main
  new create(env: Env) =>
    let d = "METHINKS IT IS LIKE A WEASEL"
    let cf = CreationFactory(d)
    let desired = cf(d)
    let mutator = Mutator(cf)
    let start = mutator(desired, 1.0)
    let spawn_per_generation = USize(100)

    var iterations = U64(0)
    var best = start

    repeat
      best = Generation(spawn_per_generation, desired, mutator)(best)

      iterations = iterations + 1
      if (iterations % 100) == 0 then
        env.out.print(
          iterations.string() + ": "
          + best.string + ", fitness: " + best.fitness.string()
          )
      end
    until best.string == desired.string end

    env.out.print(best.string + ", " + iterations.string())
```


Output:

```txt
100: MELWILYSH TDKKTPIKE DXWEASKL, fitness: 14
200: MEMHINTSLLT M KPFKETN WEASHL, fitness: 16
300: MQTHINFS ET MT DIKEVA WEASEL, fitness: 21
400: METHINKS IT IS DIKEDA WEASEL, fitness: 26
METHINKS IT IS LIKE A WEASEL, 442
```



## Prolog

```Prolog
target("METHINKS IT IS LIKE A WEASEL").

rndAlpha(64, 32).     % Generate a single random character
rndAlpha(P, P).	      % 32 is a space, and 65->90 are upper case
rndAlpha(Ch) :- random(N), P is truncate(64+(N*27)), !, rndAlpha(P, Ch).

rndTxt(0, []).        % Generate some random text (fixed length)
rndTxt(Len, [H|T]) :- succ(L, Len), rndAlpha(H), !, rndTxt(L, T).

score([], [], Score, Score).   % Score a generated mutation (count diffs)
score([Ht|Tt], [Ht|Tp], C, Score) :- !, score(Tt, Tp, C, Score).
score([_|Tt], [_|Tp], C, Score) :- succ(C, N), !, score(Tt, Tp, N, Score).
score(Txt, Score, Target) :- !, score(Target, Txt, 0, Score).

mutate(_, [], []).             % mutate(Probability, Input, Output)
mutate(P, [H|Txt], [H|Mut]) :- random(R), R < P, !, mutate(P, Txt, Mut).
mutate(P, [_|Txt], [M|Mut]) :- rndAlpha(M), !, mutate(P, Txt, Mut).

weasel(Tries, _, _, mutation(0, Result)) :-               % No differences=success
	format('~w~4|:~w~3| - ~s\n', [Tries, 0, Result]).
weasel(Tries, Chance, Target, mutation(S, Value)) :-	    % output progress
	format('~w~4|:~w~3| - ~s\n', [Tries, S, Value]), !, % and call again
	weasel(Tries, Chance, Target, Value).
weasel(Tries, Chance, Target, Start) :-
	findall(mutation(S,M),  % Generate 30 mutations, select the best.
		(between(1, 30, _), mutate(Chance, Start, M), score(M,S,Target)),
		Mutations),     % List of 30 mutations and their scores
	sort(Mutations, [Best|_]), succ(Tries, N),
	!, weasel(N, Chance, Target, Best).
weasel :-  % Chance->probability for a mutation, T=Target, Start=initial text
	target(T), length(T, Len), rndTxt(Len, Start), Chance is 1 - (1/(Len+1)),
	!, weasel(0, Chance, T, Start).
```

Output:

```txt
 time(weasel).
1   :27 - SGR JDTLWJQNGFOEJNQTVQOJLEEV
2   :27 - SGR DDTLWJQNGFOEJNQTVQOJLEEV
3   :26 - SGR DDTLWJQNGFHEJNQTVQOJLSEV
4   :25 - MGR DDWLWJQNGFHEJDQTVQOJLSEV
5   :24 - MGR DDWL JQNGFHEJDQTVQOJLSEV
6   :24 - MGR DBWL JQNGFHEJUQTVQOJLSEV
7   :23 - MRR IBWL JQNGFHEJUQTVFOJLSEV
...
168 :1 - METHINKS IT I  LIKE A WEASEL
169 :1 - METHINKS IT I  LIKE A WEASEL
170 :1 - METHINKS IT I  LIKE A WEASEL
171 :1 - METHINKS IT I  LIKE A WEASEL
172 :1 - METHINKS IT I  LIKE A WEASEL
173 :0 - METHINKS IT IS LIKE A WEASEL
% 810,429 inferences, 0.125 CPU in 0.190 seconds (66% CPU, 6493780 Lips)
true
```



## PureBasic


```PureBasic
Define population = 100, mutationRate = 6
Define.s target$ = "METHINKS IT IS LIKE A WEASEL"
Define.s charSet$ = "ABCDEFGHIJKLMNOPQRSTUVWXYZ "

Procedure.i fitness(Array aspirant.c(1), Array target.c(1))
  Protected i, len, fit
  len = ArraySize(aspirant())
  For i = 0 To len
    If aspirant(i) = target(i): fit +1: EndIf
  Next
  ProcedureReturn fit
EndProcedure

Procedure mutatae(Array parent.c(1), Array child.c(1), Array charSetA.c(1), rate.i)
  Protected i, L, maxC
  L = ArraySize(child())
  maxC = ArraySize(charSetA())
  For i = 0 To L
    If Random(100) < rate
      child(i) = charSetA(Random(maxC))
    Else
      child(i) = parent(i)
    EndIf
  Next
EndProcedure

Procedure.s cArray2string(Array A.c(1))
  Protected S.s, len
  len = ArraySize(A())+1 : S = Space(len)
  CopyMemory(@A(0), @S, len * SizeOf(Character))
  ProcedureReturn S
EndProcedure

Define mutationRate, maxChar, target_len, i, maxfit, gen, fit, bestfit
Dim targetA.c(Len(target$) - 1)
CopyMemory(@target$, @targetA(0), StringByteLength(target$))

Dim charSetA.c(Len(charSet$) - 1)
CopyMemory(@charSet$, @charSetA(0), StringByteLength(charSet$))

maxChar   = Len(charSet$) - 1
maxfit = Len(target$)
target_len   = Len(target$) - 1
Dim    parent.c(target_len)
Dim     child.c(target_len)
Dim Bestchild.c(target_len)


For i = 0 To target_len
  parent(i) = charSetA(Random(maxChar))
Next

fit = fitness (parent(), targetA())
OpenConsole()

PrintN(Str(gen) + ": " + cArray2string(parent()) + ": Fitness= " + Str(fit) + "/" + Str(maxfit))

While bestfit <> maxfit
  gen + 1
  For i = 1 To population
    mutatae(parent(),child(),charSetA(), mutationRate)
    fit = fitness (child(), targetA())
    If fit > bestfit
      bestfit = fit: CopyArray(child(), Bestchild())
    EndIf
  Next
  CopyArray(Bestchild(), parent())
  PrintN(Str(gen) + ": " + cArray2string(parent()) + ": Fitness= " + Str(bestfit) + "/" + Str(maxfit))
Wend

PrintN("Press any key to exit"): Repeat: Until Inkey() <> ""
```



## Python

Using lists instead of strings for easier manipulation, and a mutation rate that gives more mutations the further the parent is away from the target.

```python
from string import letters
from random import choice, random

target  = list("METHINKS IT IS LIKE A WEASEL")
charset = letters + ' '
parent  = [choice(charset) for _ in range(len(target))]
minmutaterate  = .09
C = range(100)

perfectfitness = float(len(target))

def fitness(trial):
    'Sum of matching chars by position'
    return sum(t==h for t,h in zip(trial, target))

def mutaterate():
    'Less mutation the closer the fit of the parent'
    return 1-((perfectfitness - fitness(parent)) / perfectfitness * (1 - minmutaterate))

def mutate(parent, rate):
    return [(ch if random() <= rate else choice(charset)) for ch in parent]

def que():
    '(from the favourite saying of Manuel in Fawlty Towers)'
    print ("#%-4i, fitness: %4.1f%%, '%s'" %
           (iterations, fitness(parent)*100./perfectfitness, ''.join(parent)))

def mate(a, b):
    place = 0
    if choice(xrange(10)) < 7:
        place = choice(xrange(len(target)))
    else:
        return a, b

    return a, b, a[:place] + b[place:], b[:place] + a[place:]

iterations = 0
center = len(C)/2
while parent != target:
    rate = mutaterate()
    iterations += 1
    if iterations % 100 == 0: que()
    copies = [ mutate(parent, rate) for _ in C ]  + [parent]
    parent1 = max(copies[:center], key=fitness)
    parent2 = max(copies[center:], key=fitness)
    parent = max(mate(parent1, parent2), key=fitness)
que()
```


Sample output

```txt
#100 , fitness: 50.0%, 'DVTAIKKS OZ IAPYIKWXALWE CEL'
#200 , fitness: 60.7%, 'MHUBINKMEIG IS LIZEVA WEOPOL'
#300 , fitness: 71.4%, 'MEYHINKS ID SS LIJF A KEKUEL'

#378 , fitness: 100.0%, 'METHINKS IT IS LIKE A WEASEL'
```


A simpler Python version that converges in less steps:

```python
from random import choice, random

target  = list("METHINKS IT IS LIKE A WEASEL")
alphabet = " ABCDEFGHIJLKLMNOPQRSTUVWXYZ"
p = 0.05 # mutation probability
c = 100  # number of children in each generation

def neg_fitness(trial):
    return sum(t != h for t,h in zip(trial, target))

def mutate(parent):
    return [(choice(alphabet) if random() < p else ch) for ch in parent]

parent = [choice(alphabet) for _ in xrange(len(target))]
i = 0
print "%3d" % i, "".join(parent)
while parent != target:
    copies = (mutate(parent) for _ in xrange(c))
    parent = min(copies, key=neg_fitness)
    print "%3d" % i, "".join(parent)
    i += 1
```



## R



```R
set.seed(1234, kind="Mersenne-Twister")

## Easier if the string is a character vector
target <- unlist(strsplit("METHINKS IT IS LIKE A WEASEL", ""))

charset <- c(LETTERS, " ")
parent <- sample(charset, length(target), replace=TRUE)

mutaterate <- 0.01

## Number of offspring in each generation
C <- 100

## Hamming distance between strings normalized by string length is used
## as the fitness function.
fitness <- function(parent, target) {
    sum(parent == target) / length(target)
}

mutate <- function(parent, rate, charset) {
    p <- runif(length(parent))
    nMutants <- sum(p < rate)
    if (nMutants) {
        parent[ p < rate ] <- sample(charset, nMutants, replace=TRUE)
    }
    parent
}

evolve <- function(parent, mutate, fitness, C, mutaterate, charset) {
    children <- replicate(C, mutate(parent, mutaterate, charset),
                          simplify=FALSE)
    children <- c(list(parent), children)
    children[[which.max(sapply(children, fitness, target=target))]]
}

.printGen <- function(parent, target, gen) {
    cat(format(i, width=3),
        formatC(fitness(parent, target), digits=2, format="f"),
        paste(parent, collapse=""), "\n")
}

i <- 0
.printGen(parent, target, i)
while ( ! all(parent == target)) {
    i <- i + 1
    parent <- evolve(parent, mutate, fitness, C, mutaterate, charset)

    if (i %% 20 == 0) {
        .printGen(parent, target, i)
    }
}
.printGen(parent, target, i)
```


output:

```txt

  0 0.00 DQQQXRAGRNSOHYHWHHFGIIEBFVOY
 20 0.36 MQQQXBAS TTOHSHLHKF I ABFSOY
 40 0.71 MQTHINKS TTXHSHLIKE A WBFSEY
 60 0.82 METHINKS IT HSHLIKE A WBFSEY
 80 0.93 METHINKS IT HS LIKE A WEFSEL
 99 1.00 METHINKS IT IS LIKE A WEASEL

```



### Alternative


Very close to former solution, but a bit easier.


```R
# Setup
set.seed(42)
target= unlist(strsplit("METHINKS IT IS LIKE A WEASEL", ""))
chars= c(LETTERS, " ")
C= 100

# Fitness function; high value means higher fitness
fitness= function(x){
  sum(x == target)
}

# Mutate function
mutate= function(x, rate= 0.01){
  idx= which(runif(length(target)) <= rate)
  x[idx]= replicate(n= length(idx), expr= sample(x= chars, size= 1, replace= T))
  x
}

# Evolve function
evolve= function(x){
  parents= rep(list(x), C+1) # Repliction
  parents[1:C]= lapply(parents[1:C], function(x) mutate(x)) # Mutation
  idx= which.max(lapply(parents, function(x) fitness(x))) # Selection
  parents[[idx]]
}

# Initialize first parent
parent= sample(x= chars, size= length(target), replace= T)

# Main program
while (fitness(parent) < fitness(target)) {
  parent= evolve(parent)
  cat(paste0(parent, collapse=""), "\n")
}
```



output:

```txt

YEHWROTERTMEZGMZ DMPYD ZCNKY
...
METHINKS IT IS LIKE A WEASEL

```



## Racket



```Racket

#lang racket

(define alphabet " ABCDEFGHIJKLMNOPQRSTUVWXYZ")
(define (randch) (string-ref alphabet (random 27)))

(define (fitness s1 s2)
  (for/sum ([c1 (in-string s1)] [c2 (in-string s2)])
    (if (eq? c1 c2) 1 0)))

(define (mutate s P)
  (define r (string-copy s))
  (for ([i (in-range (string-length r))] #:when (<= (random) P))
    (string-set! r i (randch)))
  r)

(define (evolution target C P)
  (let loop ([parent (mutate target 1.0)] [n 0])
    ;; (printf "~a: ~a\n" n parent)
    (if (equal? parent target)
      n
      (let cloop ([children (for/list ([i (in-range C)]) (mutate parent P))]
                  [best #f] [fit -1])
        (if (null? children)
          (loop best (add1 n))
          (let ([f (fitness target (car children))])
            (if (> f fit)
              (cloop (cdr children) (car children) f)
              (cloop (cdr children) best fit))))))))

;; Some random experiment using all of this
(define (try-run C P)
  (define ns
    (for/list ([i 10])
      (evolution "METHINKS IT IS LIKE A WEASEL" C P)))
  (printf "~s Average generation: ~s\n" C (/ (apply + 0.0 ns) (length ns)))
  (printf "~s      Total strings: ~s\n" C (for/sum ([n ns]) (* n 50))))
(for ([C (in-range 10 501 10)]) (try-run C 0.001))

```



## REXX


### optimized

This REXX version:
::*   allows random seed for repeatability of runs
::*   allows mutation rate to be expressed as a percentage (%)
::*   echoes specification(s) and target string
::*   columnar alignment of output
::*   optimized for speed (only one random number/mutation)
::*   supports an alphabet with lowercase letters and other letters and/or punctuation.

```rexx
/*REXX program  demonstrates  an  evolutionary algorithm  (by using mutation).          */
parse arg  children  MR  seed .                  /*get optional arguments from the C.L. */
if children==''  then children = 10              /*# children produced each generation. */
if MR      ==''  then MR       = "4%"            /*the character Mutation Rate each gen.*/
if right(MR,1)=='%'  then MR=strip(MR,,"%")/100  /*expressed as a percent?  Then adjust.*/
if seed\=='' then call random ,,seed             /*SEED allow the runs to be repeatable.*/
abc   = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ '  ;     Labc=length(abc)
target= 'METHINKS IT IS LIKE A WEASEL' ;     Ltar=length(target)
parent= mutate( left('',Ltar), 1)                /*gen rand string,same length as target*/
say center('target string', Ltar, "─")   'children'        "mutationRate"
say target  center(children,8)    center((MR*100/1)'%', 12);                  say
say center('new string'    ,Ltar, "─")   "closeness"       'generation'

       do gen=0  until  parent==target;                     close=fitness(parent)
       almost=parent
                        do  children;                       child=mutate(parent,MR)
                        _=fitness(child);                   if _<=close  then iterate
                        close=_;                            almost=child
                        say almost  right(close, 9)   right(gen,10)
                        end   /*children*/
       parent=almost
       end   /*gen*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
fitness: parse arg x; $=0;   do k=1 for Ltar; $=$+(substr(x,k,1)==substr(target,k,1)); end
         return $
/*──────────────────────────────────────────────────────────────────────────────────────*/
mutate:  parse arg x,rate;  $=                   /*set  X  to 1st argument, RATE to 2nd.*/
                   do j=1  for Ltar;       r=random(1,100000)    /*REXX's max for RANDOM*/
                   if .00001*r<=rate  then $=$ || substr(abc,r//Labc+1, 1)
                                      else $=$ || substr(x  ,j        , 1)
                   end   /*j*/
         return $
```

'''output'''   when using the following input:   <tt> 20   4%   11 </tt>

```txt

───────target string──────── children mutationRate
METHINKS IT IS LIKE A WEASEL    20         4%

─────────new string───────── closeness generation
TWLPLGNVVMXFBUKHUPEQXOCUPIUS         1          0
TWLPLGNVVMXFBU HUPEQXOCUPIUS         2          1
TWLPLGNVVMX BU HUPEQXOCUPIUS         3          2
TWLPLCNVFMX BP HUPEQAOCUPIUS         4          4
TWLPLQNVFMX BP HUPEQAOCUPGUL         5          6
TWLHLQNVFMX BS HUPEQAOUUPGUL         7          9
RWLHLQNZFMX BS HUPEQAOUUEGEL         8         14
RWLHLQNZFIX BS HUPEQAOUUEGEL         9         15
RWLHLQNZFIX BS HUPE AOUUEGEL        10         19
RWLHLQNZFIX BS LWPE AOUUEGEL        11         22
RWLHLQNZFIX BS LWPE A UUEGEL        12         28
RWLHLNNZFIX BS LWPE A UUEGEL        13         36
RELHLNNZFIX BE LWPE A UUAGEL        14         40
RELHLNNZFIX BE LWPE A UUASEL        15         43
RELHLNNZFIX BE LWKE A  UASEL        16         50
RELHLNNZFIT BE LWKE A  UASEL        17         62
RELHLNNSFIT IE LWKE A  UASEL        19         67
RETHLNNSFIT IE LWKE A  UASEL        20         71
RETHLNNSFIT IE LIKE A  UASEL        21         79
METHLNNSFIT IE LIKE A  LASEL        22         91
METHLNNSFIT IE LIKE A WLASEL        23        112
METHLNNSFIT IE LIKE A WEASEL        24        144
METHLNNS IT IE LIKE A WEASEL        25        151
METHLNKS IT IM LIKE A WEASEL        26        160
METHLNKS IT IS LIKE A WEASEL        27        164
METHINKS IT IS LIKE A WEASEL        28        170

```


===optimized, stemmed arrays===
This REXX version uses stemmed arrays for the character-by-character comparison   ['''T.n''']   as well as

generating a random character   ['''@.n''']   during mutation,   thus making it slightly faster,   especially for a

longer string and/or a low mutation rate.

```rexx
/*REXX program  demonstrates  an  evolutionary algorithm  (by using mutation).          */
parse arg  children  MR  seed .                  /*get optional arguments from the C.L. */
if children==''  then children = 10              /*# children produced each generation. */
if MR      ==''  then MR       = "4%"            /*the character Mutation Rate each gen.*/
if right(MR,1)=='%'  then MR=strip(MR,,"%")/100  /*expressed as a percent?  Then adjust.*/
if seed\==''  then call random ,,seed            /*SEED allow the runs to be repeatable.*/
abc = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ ';     Labc=length(abc)

       do i=0  for Labc                          /*define array  (for faster compare),  */
       @.i=substr(abc, i+1, 1)                   /*     it's better than picking out a  */
       end   /*i*/                               /*     byte from a character string.   */

target= 'METHINKS IT IS LIKE A WEASEL' ;    Ltar=length(target)

       do i=1  for Ltar                          /*define an array (for faster compare),*/
       T.i=substr(target, i, 1)                  /*     it's better than a byte-by-byte */
       end   /*i*/                               /*     compare using character strings.*/

parent= mutate( left('', Ltar), 1)               /*gen rand string, same length as tar. */
say center('target string', Ltar, "─")    'children'       "mutationRate"
say target  center(children, 8)   center((MR*100/1)'%',12);                     say
say center('new string'   , Ltar, "─")    'closeness'      "generation"

       do gen=0  until  parent==target;                     close=fitness(parent)
       almost=parent
                         do  children;                      child=mutate(parent,MR)
                         _=fitness(child);                  if _<=close  then iterate
                         close=_;                           almost=child
                         say almost  right(close, 9)  right(gen, 10)
                         end   /*children*/
       parent=almost
       end   /*gen*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
fitness: parse arg x; $=0;   do k=1  for Ltar;  $=$+(substr(x,k,1)==T.k);  end;   return $
/*──────────────────────────────────────────────────────────────────────────────────────*/
mutate:  parse arg x,rate                        /*set  X  to 1st argument, RATE to 2nd.*/
         $=;       do j=1  for Ltar;       r=random(1, 100000)   /*REXX's max for RANDOM*/
                   if .00001*r<=rate  then do;    _=r//Labc;    $=$ || @._;  end
                                      else $=$ || substr(x, j, 1)
                   end   /*j*/
         return $
```

'''output'''   is the same as the previous version.





## Ring


```ring

# Project : Evolutionary algorithm

target = "METHINKS IT IS LIKE A WEASEL"
parent = "IU RFSGJABGOLYWF XSMFXNIABKT"
num = 0
mutationrate = 0.5
children = len(target)
child = list(children)
while parent != target
        bestfitness = 0
        bestindex = 0
        for index = 1 to children
             child[index] = mutate(parent, mutationrate)
             fitness = fitness(target, child[index])
             if fitness > bestfitness
                bestfitness = fitness
                bestindex = index
             ok
        next
        if bestindex > 0
           parent = child[bestindex]
           num = num + 1
           see "" + num + ": " + parent + nl
        ok
end

func fitness(text, ref)
       f = 0
       for i = 1 to len(text)
            if substr(text, i, 1) = substr(ref, i, 1)
               f = f + 1
            ok
       next
       return (f / len(text))

func mutate(text, rate)
        rnd = randomf()
        if rate > rnd
           c = 63+random(27)
           if c = 64
              c = 32
           ok
           rnd2 = random(len(text))
           if rnd2 > 0
              text[rnd2] = char(c)
           ok
        ok
        return text

func randomf()
       decimals(10)
       str = "0."
       for i = 1 to 10
            nr = random(9)
            str = str + string(nr)
       next
       return number(str)

```

Output:

```txt

1: IU RFPGJABGOLYWF XSMFXNIABKT
2: IU RFPGJABGOLQWF XSMFXNIABKT
3: IU RFPGJABGOLQWF XSMAXNIABKT
4: IU RFPGJABGOLQWF XSMA NIABKT
5: IU RFPGJABGOLQWF XSMA NIABKT
6: IU RFPGJABGOLSWF XSMA NIABKT
7: IU RFPGJABGOLSWF XSMA NIABKT
8: IUTRFPGJABGOLSWF XSMA NIABKT
9: IUTRFPGSABGOLSWF XSMA NIABKT
10: IUTRFPGSABGOLSWF XSMA NIABKT
11: IUTRFPGSABGOLSWF XSMA NIABKT
12: IUTRFPGSABGOLSWF XSMA NIABKT
13: IUTRFPGSABGOLSWF XSMA NIABKE
14: IUTRFPGSABGOMSWF XSMA NIABKE
15: IUTRFPGSYBGOMSWF XSMA NIABKE
16: IUTRFPGSYBGOMSWF XSMA NIABKE
17: IUTRFPGSYBGOMSWF XSMA NIABKE
18: IUTRFPGSYBGOMSWF XSMA NIASKE
19: IUTRFPGSYBGOMSWF XSMA NIASKE
20: IUTRFPGSYMGOMSWF XSMA NIASKE
21: IUTRFPGSYMGOMSWF XSMA NIASKE
22: IUTRFPGSYMGOMSWF XSMA NEASKE
23: IUTRFPGSYMGOMSWF XSMA NEASKE
24: IUTRFPGSYMG MSWF XSMA NEASKE
25: IUTRFPGSYMG MSWF XSMA NEASKE
26: MUTRFPGSYMG MSWF XSMA NEASKE
27: MUTRFPGSYMX MSWF XSMA NEASKE
28: MUTRFPGSYMX MSWFIXSMA NEASKE
29: MUTRFPGSYMX MSWFIXSMA NEASKE
30: MUTRFPGSYIX MSWFIXSMA NEASKE
31: MUTRFPGSYIX MSWLIXSMA NEASKE
32: MUTRFPGSYIX MSWLIXSMA NEASKE
33: MUTRFPGSYIX MSWLIXSMA NEASKL
34: MUTRFPGSYIX MSWLIXSMA NEASKL
35: MUTRFPGSYIX MSKLIXSMA NEASKL
36: MUTRFPKSYIX MSKLIXSMA NEASKL
37: MUTRFPKSYIX MSKLIXSMA NEASKL
38: MUTRFPKSYIX MS LIXSMA NEASKL
39: MUTRFPKSYIX MS LIXSMA NEASEL
40: MUTRFPKSYIX MS LIXYMA NEASEL
41: MUTRFPKS IX MS LIXYMA NEASEL
42: MUTRFPKS IX MS LIXYMA NEASEL
43: MUTRFPKS IX MS LIXYMA WEASEL
44: MUTRFPKS IX MS LIXYMA WEASEL
45: MUTRFPKS IX MS LIXYMA WEASEL
46: MUTRFPKS IJ MS LIXYMA WEASEL
47: MUTRFPKS IJ MS LIXYMA WEASEL
48: MUTRFPKS IJ MS LIXYMA WEASEL
49: MUTRFPKS IJ MS LIXYMA WEASEL
50: MUTRFPKS IJ MS LIXYMA WEASEL
51: MUTRFPKS IJ MS LIXYMA WEASEL
52: MUTRIPKS IJ MS LIXYMA WEASEL
53: MUTRIPKS IJ MS LIXYMA WEASEL
54: MUTRIPKS IJ MS LIXYMA WEASEL
55: MUTRIPKS IJ MS LIXYMA WEASEL
56: MUTRIPKS IJ MS LIKYMA WEASEL
57: MUTRIPKS IJ MS LIKYMA WEASEL
58: MUTRIPKS IJ MS LIKYMA WEASEL
59: MUTRIPKS IJ MS LIKYMA WEASEL
60: MUTRIPKS IJ MS LIKY A WEASEL
61: MUTRIPKS IJ MS LIKY A WEASEL
62: MUTRIPKS IJ MS LIKY A WEASEL
63: MUTRIPKS IT MS LIKY A WEASEL
64: MUTRIPKS IT MS LIKY A WEASEL
65: MUTRIPKS IT MS LIKY A WEASEL
66: MUTRIPKS IT MS LIKY A WEASEL
67: MUTRIPKS IT MS LIKE A WEASEL
68: MUTRIPKS IT MS LIKE A WEASEL
69: MUTRIPKS IT MS LIKE A WEASEL
70: MUTRIPKS IT MS LIKE A WEASEL
71: MUTVIPKS IT MS LIKE A WEASEL
72: MUTVIPKS IT MS LIKE A WEASEL
73: MUTVIPKS IT MS LIKE A WEASEL
74: MUTVIPKS IT MS LIKE A WEASEL
75: MUTVIPKS IT MS LIKE A WEASEL
76: MUTVIPKS IT MS LIKE A WEASEL
77: MUTVIPKS IT MS LIKE A WEASEL
78: METVIPKS IT MS LIKE A WEASEL
79: METVIPKS IT MS LIKE A WEASEL
80: METVIPKS IT RS LIKE A WEASEL
81: METVIPKS IT RS LIKE A WEASEL
82: METVIPKS IT RS LIKE A WEASEL
83: METFIPKS IT RS LIKE A WEASEL
84: METFIPKS IT RS LIKE A WEASEL
85: METFIPKS IT RS LIKE A WEASEL
86: METFIPKS IT RS LIKE A WEASEL
87: METFIPKS IT RS LIKE A WEASEL
88: METFIPKS IT RS LIKE A WEASEL
89: METHIPKS IT RS LIKE A WEASEL
90: METHINKS IT RS LIKE A WEASEL
91: METHINKS IT ?S LIKE A WEASEL
92: METHINKS IT ?S LIKE A WEASEL
93: METHINKS IT ?S LIKE A WEASEL
94: METHINKS IT ?S LIKE A WEASEL
95: METHINKS IT ?S LIKE A WEASEL
96: METHINKS IT ?S LIKE A WEASEL
97: METHINKS IT ?S LIKE A WEASEL
98: METHINKS IT ?S LIKE A WEASEL
99: METHINKS IT ?S LIKE A WEASEL
100: METHINKS IT ?S LIKE A WEASEL
101: METHINKS IT ?S LIKE A WEASEL
102: METHINKS IT ?S LIKE A WEASEL
103: METHINKS IT ?S LIKE A WEASEL
104: METHINKS IT ?S LIKE A WEASEL
105: METHINKS IT ?S LIKE A WEASEL
106: METHINKS IT ?S LIKE A WEASEL
107: METHINKS IT ?S LIKE A WEASEL
108: METHINKS IT ?S LIKE A WEASEL
109: METHINKS IT ?S LIKE A WEASEL
110: METHINKS IT ?S LIKE A WEASEL
111: METHINKS IT ?S LIKE A WEASEL
112: METHINKS IT ?S LIKE A WEASEL
113: METHINKS IT ?S LIKE A WEASEL
114: METHINKS IT ?S LIKE A WEASEL
115: METHINKS IT ?S LIKE A WEASEL
116: METHINKS IT ?S LIKE A WEASEL
117: METHINKS IT ?S LIKE A WEASEL
118: METHINKS IT ?S LIKE A WEASEL
119: METHINKS IT ?S LIKE A WEASEL
120: METHINKS IT ?S LIKE A WEASEL
121: METHINKS IT ?S LIKE A WEASEL
122: METHINKS IT ?S LIKE A WEASEL
123: METHINKS IT ?S LIKE A WEASEL
124: METHINKS IT ?S LIKE A WEASEL
125: METHINKS IT ?S LIKE A WEASEL
126: METHINKS IT ?S LIKE A WEASEL
127: METHINKS IT ?S LIKE A WEASEL
128: METHINKS IT IS LIKE A WEASEL

```



## Ruby

{{works with|Ruby|1.9.3+}} for the <code>sample</code> method.

```ruby
@target = "METHINKS IT IS LIKE A WEASEL"
Charset = [" ", *"A".."Z"]
COPIES = 100

def random_char; Charset.sample end

def fitness(candidate)
  sum = 0
  candidate.chars.zip(@target.chars) {|x,y| sum += (x[0].ord - y[0].ord).abs}
  100.0 * Math.exp(Float(sum) / -10.0)
end

def mutation_rate(candidate)
  1.0 - Math.exp( -(100.0 - fitness(candidate)) / 400.0)
end

def mutate(parent, rate)
  parent.each_char.collect {|ch| rand <= rate ? random_char : ch}.join
end

def log(iteration, rate, parent)
  puts "%4d %.2f %5.1f %s" % [iteration, rate, fitness(parent), parent]
end

iteration = 0
parent = Array.new(@target.length) {random_char}.join
prev = ""

while parent != @target
  iteration += 1
  rate = mutation_rate(parent)
  if prev != parent
    log(iteration, rate, parent)
    prev = parent
  end
  copies = [parent] + Array.new(COPIES) {mutate(parent, rate)}
  parent = copies.max_by {|c| fitness(c)}
end
log(iteration, rate, parent)
```


<div style='height: 15em; overflow: scroll'>
```txt

   1 0.22   0.0 FBNLRACAYQJAAJRNKNGZJMBQWBBW
   2 0.22   0.0 QBNLGHPAYQJALJZGZNGAJMVQLBBW
   3 0.22   0.0 JBNLGDPA QJALJZOZNGGTMVKLTBV
   4 0.22   0.0 NSNLGDPA QTAMJ OZNVGTMVHOTBV
   5 0.22   0.0 NSNLGVPA QTAMR OZVVGT VHOTBV
   6 0.22   0.0 NSWLGVPA QTAMR OZVHGD VHOTBV
   7 0.22   0.0 NSWLGVPA QTALR OGJHGD VHOTBV
   8 0.22   0.0 NSWLGNPA QTALR OGJHGE VHNTBV
   9 0.22   0.0 NSWWGMPY QT LR OJAHGE VHNTBV
  10 0.22   0.0 NSWWGMPW QT LR OJAH E VJNTXV
  11 0.22   0.0 JSZWGMPW QT LR OQAH E VJNWLF
  12 0.22   0.0 JJZGJMPW QT LR OIAH E VJNWLF
  13 0.22   0.0 IJZGJMPW DT HR OIHH E VJNWLF
  14 0.22   0.1 NJZGJMPW DT HR OIHH E VCEZLF
  17 0.22   0.2 NJZGJMPW KT HR OIHH E VCEPLF
  22 0.22   0.2 NDZGJMPQ KW HR OIHH E VCEPLF
  25 0.22   0.3 NDZGJMPQ KW HR LIHH E VCEPOO
  26 0.22   0.5 NDZGJQJQ JS HR LIHH E VCEPOO
  28 0.22   0.6 NDZGJQJQ IS HR LIHH E VCEPOO
  29 0.22   0.6 NDZGJLJQ IS HR LIHH E VCEPOO
  30 0.22   0.7 NDZGJLJQ IS ER LIHH E VCEPKO
  35 0.22   0.8 NDZGJLJQ IS KR LIHH E VCEPKO
  40 0.22   1.5 NDZGJLJQ IS KR LINH D VCEPFO
  46 0.22   1.7 NDZGJLJQ IS KR LIMH D VCEPFO
  47 0.21   3.3 NDZGJLJQ IS KR LILB D VCAPFM
  66 0.21   3.7 NDSGJLJQ IS KR LIGI D VCAPFM
  67 0.21   4.5 NDSGJLJQ IS IR LIGI D VCAPFM
  70 0.21   6.1 NDTGJLMQ IS IS LIGI D VCATFM
  72 0.21   6.7 NDTGJLMQ IS IS LIHI D VCATFM
  77 0.21   8.2 NDTGJLMQ IU IS LIHI B VCATFM
  83 0.20   9.1 NDTGJLLQ IU IS LIHI B VCATFM
  87 0.20  10.0 NDTGJLLQ IU IS LIHH B VCATFM
 108 0.20  11.1 NDTGJLLT IU IS LIHH B VCATFM
 118 0.19  13.5 NDTGJNLT IU IS LIHH B VCATFM
 128 0.18  18.3 MDTGJNLT IU IS LILH B VCATFM
 153 0.18  20.2 NDTGJNLT IU IS LILH B VEATFM
 155 0.17  24.7 NDTGJNLT IU IS LILE B VDATFM
 192 0.17  27.3 NDTGJNLS IU IS LILE B VDATFM
 225 0.16  30.1 NDTGJNLS IU IS LILE B VDASFM
 226 0.15  33.3 NDTGJNLS IU IS LILE B VDASFL
 227 0.15  36.8 NDTGJNLS IT IS LILE B VDASFL
 246 0.14  40.7 NDTGJNKS IT IS LILE B VDASFL
 252 0.13  44.9 NETGJNKS IT IS LILE B VDASFL
 256 0.12  49.7 NETGJNKS IT IS LILE B WDASFL
 260 0.11  54.9 NETGINKS IT IS LILE B WDASDL
 284 0.09  60.7 NETHINKS IT IS LILE B WDASDL
 300 0.08  67.0 NETHINKS IT IS LIKE B WDASDL
 309 0.06  74.1 NETHINKS IT IS LIKE B WDASEL
 311 0.04  81.9 NETHINKS IT IS LIKE A WDASEL
 316 0.02  90.5 METHINKS IT IS LIKE A WDASEL
 335 0.02 100.0 METHINKS IT IS LIKE A WEASEL
```
</div>

## Rust


```Rust
//! Author : Thibault Barbie
//!
//! A simple evolutionary algorithm written in Rust.

extern crate rand;

use rand::Rng;

fn main() {
    let target = "METHINKS IT IS LIKE A WEASEL";
    let copies = 100;
    let mutation_rate = 20; // 1/20 = 0.05 = 5%

    let mut rng = rand::weak_rng();

    // Generate first sentence, mutating each character
    let start = mutate(&mut rng, target, 1); // 1/1 = 1 = 100%

    println!("{}", target);
    println!("{}", start);

    evolve(&mut rng, target, start, copies, mutation_rate);
}

/// Evolution algorithm
///
/// Evolves `parent` to match `target`.  Returns the number of evolutions performed.
fn evolve<R: Rng>(
    rng: &mut R,
    target: &str,
    mut parent: String,
    copies: usize,
    mutation_rate: u32,
) -> usize {
    let mut counter = 0;
    let mut parent_fitness = target.len() + 1;

    loop {
        counter += 1;

        let (best_fitness, best_sentence) = (0..copies)
            .map(|_| {
                // Copy and mutate a new sentence.
                let sentence = mutate(rng, &parent, mutation_rate);
                // Find the fitness of the new mutation
                (fitness(target, &sentence), sentence)
            })
            .min_by_key(|&(f, _)| f) // find the closest mutation to the target
            .unwrap(); // fails if `copies == 0`

        // If the best mutation of this generation is better than `parent` then "the fittest
        // survives" and the next parent becomes the best of this generation.
        if best_fitness < parent_fitness {
            parent = best_sentence;
            parent_fitness = best_fitness;
            println!(
                "{} : generation {} with fitness {}",
                parent, counter, best_fitness
            );

            if best_fitness == 0 {
                return counter;
            }
        }
    }
}

/// Computes the fitness of a sentence against a target string, returning the number of
/// incorrect characters.
fn fitness(target: &str, sentence: &str) -> usize {
    sentence
        .chars()
        .zip(target.chars())
        .filter(|&(c1, c2)| c1 != c2)
        .count()
}

/// Mutation algorithm.
///
/// It mutates each character of a string, according to a `mutation_rate`.
fn mutate<R: Rng>(rng: &mut R, sentence: &str, mutation_rate: u32) -> String {
    let maybe_mutate = |c| {
        if rng.gen_weighted_bool(mutation_rate) {
            random_char(rng)
        } else {
            c
        }
    };
    sentence.chars().map(maybe_mutate).collect()
}

/// Generates a random letter or space.
fn random_char<R: Rng>(rng: &mut R) -> char {
    // Returns a value in the range [A, Z] + an extra slot for the space character.  (The `u8`
    // values could be cast to larger integers for a better chance of the RNG hitting the proper
    // range).
    match rng.gen_range(b'A', b'Z' + 2) {
        c if c == b'Z' + 1 => ' ', // the `char` after 'Z'
        c => c as char,
    }
}
```

<div style='height: 15em; overflow: scroll'>
```txt

METHINKS IT IS LIKE A WEASEL
ZPNUDZUKIHR SRD SZNRWOZDAXJX
ZPNUDZKKIHR SRD SZNJWOZDAXJX : generation 1 with fitness 25
ZPHUDZKKIHR SRD SZNJWOWDAXJX : generation 2 with fitness 24
ZPGUDZKSIHR SRD SZNJWOWDAXJX : generation 3 with fitness 23
ZPGUDZKSIIR SRD SUNJWOWXAXJX : generation 4 with fitness 22
ZEGUDZKSIIR SRD SUNJWOWXAXJX : generation 5 with fitness 21
ZECUDZKSIIR SRD SUN WOWEAXJX : generation 6 with fitness 19
ZECUDZKSIIN SRD SUN AOWEAXJX : generation 7 with fitness 18
ZECUDSKSIIN IRD SUN AOWEAXJX : generation 8 with fitness 17
ZECUDSKSIIN IRDLSUN AOWEAXJX : generation 9 with fitness 16
ZETUDSKSIIN IRDLSUN AOWEAXJX : generation 10 with fitness 15
ZETUDSKSIIN IRDLSUN A WEAXJX : generation 11 with fitness 14
ZETUDSKSIIT IRDLSUN A WEAXJX : generation 12 with fitness 13
ZETUDSKSIIT IRDLSUN A WEAXER : generation 13 with fitness 12
ZETHDSKSIIT IRDLSUN A WEAXER : generation 14 with fitness 11
ZETHDSKSIIT IRDLSKN A WEAXER : generation 15 with fitness 10
ZETHDSKSIIT IRDLIKN A WEAXER : generation 17 with fitness 9
ZETHDSKSIIT IR LIKN A WEAXER : generation 19 with fitness 8
ZETHDSKS IT IR LIKN A WEAXER : generation 23 with fitness 7
ZETHDSKS IT IR LIKN A WEASER : generation 26 with fitness 6
ZETHDOKS IT IR LIKE A WEASER : generation 28 with fitness 5
ZETHDNKS IT IR LIKE A WEASER : generation 31 with fitness 4
ZETHCNKS IT IR LIKE A WEASEL : generation 45 with fitness 3
ZETHCNKS IT IS LIKE A WEASEL : generation 46 with fitness 2
METHCNKS IT IS LIKE A WEASEL : generation 68 with fitness 1
METHINKS IT IS LIKE A WEASEL : generation 79 with fitness 0

```
</div>


## Scala

```scala
import scala.annotation.tailrec

case class LearnerParams(target:String,rate:Double,C:Int)

val chars =  ('A' to 'Z') ++ List(' ')
val randgen = new scala.util.Random
def randchar = {
   val charnum = randgen.nextInt(chars.size)
   chars(charnum)
}

class RichTraversable[T](t: Traversable[T]) {
    def maxBy[B](fn: T => B)(implicit ord: Ordering[B]) = t.max(ord on fn)
    def minBy[B](fn: T => B)(implicit ord: Ordering[B]) = t.min(ord on fn)
}

implicit def toRichTraversable[T](t: Traversable[T]) = new RichTraversable(t)

def fitness(candidate:String)(implicit params:LearnerParams) =
   (candidate zip params.target).map { case (a,b) => if (a==b) 1 else 0 }.sum

def mutate(initial:String)(implicit params:LearnerParams) =
   initial.map{ samechar => if(randgen.nextDouble < params.rate) randchar else samechar }

@tailrec
def evolve(generation:Int, initial:String)(implicit params:LearnerParams){
   import params._
   printf("Generation: %3d  %s\n",generation, initial)
   if(initial == target) return ()
   val candidates = for (number <- 1 to C) yield mutate(initial)
   val next = candidates.maxBy(fitness)
   evolve(generation+1,next)
}

implicit val params = LearnerParams("METHINKS IT IS LIKE A WEASEL",0.01,100)
val initial = (1 to params.target.size) map(x => randchar) mkString
evolve(0,initial)
```



## Scheme



```scheme

(import (scheme base)
        (scheme write)
        (srfi 27))     ; random numbers

(random-source-randomize! default-random-source)

(define target "METHINKS IT IS LIKE A WEASEL") ; target string
(define C 100) ; size of population
(define p 0.1) ; chance any char is mutated

;; return a random character in given range
(define (random-char)
  (string-ref "ABCDEFGHIJKLMNOPQRSTUVWXYZ "
              (random-integer 27)))

;; compute distance of given string from target
(define (fitness str)
  (apply +
         (map (lambda (c1 c2) (if (char=? c1 c2) 0 1))
              (string->list str)
              (string->list target))))

;; mutate given parent string, returning a new string
(define (mutate str)
  (string-map (lambda (c)
                (if (< (random-real) p)
                  (random-char)
                  c))
              str))

;; create a population by mutating parent,
;; returning a list of variations
(define (make-population parent)
  (do ((pop '() (cons (mutate parent) pop)))
    ((= C (length pop)) pop)))

;; find the most fit candidate in given list
(define (find-best candidates)
  (define (select-best a b)
    (if (< (fitness a) (fitness b)) a b))
  ;
  (do ((best (car candidates) (select-best best (car rem)))
       (rem (cdr candidates) (cdr rem)))
    ((null? rem) best)))

;; create first parent from random characters
;; of same size as target string
(define (initial-parent)
  (do ((res '() (cons (random-char) res)))
    ((= (length res) (string-length target))
     (list->string res))))

;; run the search
(do ((parent (initial-parent) (find-best (cons parent (make-population parent))))) ; select best from parent and population
  ((string=? parent target)
   (display (string-append "Found: " parent "\n")))
  (display parent) (newline))

```



## Seed7


```seed7
$ include "seed7_05.s7i";

const string: table is "ABCDEFGHIJKLMNOPQRSTUVWXYZ ";

const func integer: unfitness (in string: a, in string: b) is func
  result
    var integer: sum is 0;
  local
    var integer: index is 0;
  begin
    for index range 1 to length(a) do
      sum +:= ord(a[index] <> b[index]);
    end for;
  end func;

const proc: mutate (in string: a, inout string: b) is func
  local
    var integer: index is 0;
  begin
    b := a;
    for index range 1 to length(a) do
      if rand(1, 15) = 1 then
        b @:= [index] table[rand(1, 27)];
      end if;
    end for;
  end func;

const proc: main is func
  local
     const string: target is "METHINKS IT IS LIKE A WEASEL";
     const integer: OFFSPRING is 30;
     var integer: index is 0;
     var integer: unfit is 0;
     var integer: best is 0;
     var integer: bestIndex is 0;
     var integer: generation is 1;
     var string: parent is " " mult length(target);
     var array string: children is OFFSPRING times " " mult length(target);
  begin
    for index range 1 to length(target) do
      parent @:= [index] table[rand(1, 27)];
    end for;
    repeat
      for index range 1 to OFFSPRING do
        mutate(parent, children[index]);
      end for;
      best := succ(length(parent));
      bestIndex := 0;
      for index range 1 to OFFSPRING do
        unfit := unfitness(target, children[index]);
        if unfit < best then
          best := unfit;
          bestIndex := index;
        end if;
      end for;
      if bestIndex <> 0 then
        parent := children[bestIndex];
      end if;
      writeln("generation " <& generation <& ": score " <& best <& ": " <& parent);
      incr(generation);
    until best = 0;
  end func;
```



## SequenceL

'''SequenceL Code:'''


```sequencel
import <Utilities/Sequence.sl
;

AllowedChars := " ABCDEFGHIJKLMNOPQRSTUVWXYZ";

initializeParent(randChars(1)) := AllowedChars[randChars];

Fitness(target(1), current(1)) :=
	let
		fit[i] := true when target[i] = current[i];
	in
		size(fit);

Mutate(letter(0), rate(0), randRate(0), randChar(0)) :=
		letter when randRate > rate
	else
		AllowedChars[randChar];

evolve(target(1), parent(1), C(0), P(0), rateRands(2), charRands(2)) :=
	let
		mutations[i] := Mutate(parent, P, rateRands[i], charRands[i]) foreach i within 1 ... C;
		fitnesses := Fitness(target, mutations);
	in
		mutations[firstIndexOf(fitnesses, vectorMax(fitnesses))];
```


'''C++ Driver Code:'''


```cpp
#include <iostream>
#include <time.h>
#include "SL_Generated.h"

using namespace std;

int main(int argc, char** argv)
{
	int threads = 0;

	char* targetString = "METHINKS IT IS LIKE A WEASEL";
	if(argc > 1) targetString = argv[1];
	int C = 100;
	if(argc > 2) C = atoi(argv[2]);
	SL_FLOAT P = 0.05;
	if(argc > 3) P = atof(argv[3]);
	int seed = time(NULL);
	if(argc > 4) seed = atoi(argv[4]);

	int targetDims[] = {strlen(targetString), 0};
	Sequence<char> target((void*)targetString, targetDims);

	sl_init(threads);

	Sequence<char> parent;
	Sequence<char> newParent;
	Sequence<int> parentRands;
	sl_create(seed++, 1, 27, target.size(), threads, parentRands);
	sl_initializeParent(parentRands, threads, parent);

	Sequence< Sequence<int> > charRands;
	Sequence< Sequence<SL_FLOAT> > rateRands;

	cout << "Start:\t" << parent << endl;
	for(int i = 1; !(parent == target); i++)
	{
		sl_create(seed++, 1, 27, C, target.size(), threads, charRands);
		sl_create(seed++, 0.0, 1.0, C, target.size(), threads, rateRands);

		sl_evolve(target, parent, C, P, rateRands, charRands, threads, newParent);
		parent = newParent;

		cout << "#" << i << ":\t" << parent << endl;
	}
	cout << "End:\t" << parent << endl;

	sl_done();

	return 0;
}
```


```txt
Start:	"EDVSWRXSQWK VWUOGAWSTRJWY EW"
#1:	"EDVSWRXSQIK VWUOGAWSTRJWY EW"
#2:	"EDVSWRXSQIK VWUOGAESTRJWY EW"
#3:	"EDVSWRXSQIK VWUOGAESTRJWY EL"
#4:	"MDVSWRHSQIK VWUOGAESTRJWY EL"
#5:	"MDVSWRHSQIK VW OGAESTRJWY EL"
#6:	"MDVSWRHSQIK IW OGAESTRJOY EL"
#7:	"MDVSWRHSQIK IW OGAESTRWOY EL"
#8:	"MDVSWRHSQIK IW OGAESARWOY EL"
#9:	"MDVSWRHSQIK IW OGAESARWOY EL"
#10:	"MDVSWRHSQIK IW OGAESARWOY EL"
#11:	"MDVSWRHSQIK IW OGAESARWOY EL"
#12:	"MDVSWRHSQIK IW LGAESANWOY EL"
#13:	"MDVSWJHSXIK IW LGAESANWOY EL"
#14:	"MEVSWJHSXIK IW LGAESANWOY EL"
#15:	"MEVSWJHSXIK IA LVAESANWOY EL"
#16:	"MEVSWJHSXIK IA LVAESACWOY EL"
#17:	"MEVSWRHSXIK IA LVAESACWOADEL"
#18:	"MEVIWRHSXIK IA LVAESACWOADEL"
#19:	"MENIWRHSXIK IA LVAE ACWOADEL"
#20:	"MENIWRHSXIK IA LVAE ACWOADEL"
#21:	"MENIWRHS IK IA LVAE ACWOADEL"
#22:	"MENIWRHS IK IA LVAE A WOADEL"
#23:	"METIKRAS IK IA LCAE A WOADEL"
#24:	"METIKRAS IK IA LCIE A WOADEL"
#25:	"METIKRAS IK IA LCIE A WOASEL"
#26:	"METIKRAS IK IA LCIE A WOASEL"
#27:	"METIKRAS IK IA LCIE A WEASEL"
#28:	"METIKRKS IK IA LCIE A WEASEL"
#29:	"METIKRKS IK IA LCIE A WEASEL"
#30:	"METIKRKS IK IA LCIE A WEASEL"
#31:	"METIKRKS IK IU LOIE A WEASEL"
#32:	"METIKRKS IT IU LOIE A WEASEL"
#33:	"METIKRKS IT IU LOIE A WEASEL"
#34:	"METIKRKS IT IU LIIE A WEASEL"
#35:	"METIKRKS IT IU LIIE A WEASEL"
#36:	"METHKRKS IT IU LIIE A WEASEL"
#37:	"METHKRKS IT IU LIIE A WEASEL"
#38:	"METHKRKS IT IU LIIE A WEASEL"
#39:	"METHCRKS IT IU LIIE A WEASEL"
#40:	"METHCRKS IT IU LIIE A WEASEL"
#41:	"METHCRKS IT IU LIIE A WEASEL"
#42:	"METHCRKS IT IU LIIE A WEASEL"
#43:	"METHCRKS IT IU LIIE A WEASEL"
#44:	"METHCRKS IT IU LIIE A WEASEL"
#45:	"METHCRKS IT IU LIIE A WEASEL"
#46:	"METHZRKS IT IU LIKE A WEASEL"
#47:	"METHZRKS IT IU LIKE A WEASEL"
#48:	"METHZRKS IT IU LIKE A WEASEL"
#49:	"METHZRKS IT IU LIKE A WEASEL"
#50:	"METHGRKS IT IU LIKE A WEASEL"
#51:	"METHGRKS IT IL LIKE A WEASEL"
#52:	"METHGYKS IT IL LIKE A WEASEL"
#53:	"METHGYKS IT IL LIKE A WEASEL"
#54:	"METHIYKS IT IL LIKE A WEASEL"
#55:	"METHIYKS IT IS LIKE A WEASEL"
#56:	"METHIYKS IT IS LIKE A WEASEL"
#57:	"METHIYKS IT IS LIKE A WEASEL"
#58:	"METHIYKS IT IS LIKE A WEASEL"
#59:	"METHIYKS IT IS LIKE A WEASEL"
#60:	"METHIYKS IT IS LIKE A WEASEL"
#61:	"METHIYKS IT IS LIKE A WEASEL"
#62:	"METHIYKS IT IS LIKE A WEASEL"
#63:	"METHIYKS IT IS LIKE A WEASEL"
#64:	"METHIYKS IT IS LIKE A WEASEL"
#65:	"METHIYKS IT IS LIKE A WEASEL"
#66:	"METHIYKS IT IS LIKE A WEASEL"
#67:	"METHIYKS IT IS LIKE A WEASEL"
#68:	"METHIYKS IT IS LIKE A WEASEL"
#69:	"METHIYKS IT IS LIKE A WEASEL"
#70:	"METHIYKS IT IS LIKE A WEASEL"
#71:	"METHIYKS IT IS LIKE A WEASEL"
#72:	"METHIYKS IT IS LIKE A WEASEL"
#73:	"METHIYKS IT IS LIKE A WEASEL"
#74:	"METHIYKS IT IS LIKE A WEASEL"
#75:	"METHIYKS IT IS LIKE A WEASEL"
#76:	"METHIYKS IT IS LIKE A WEASEL"
#77:	"METHINKS IT IS LIKE A WEASEL"
End:	"METHINKS IT IS LIKE A WEASEL"

```



## Sidef

```ruby
define target = "METHINKS IT IS LIKE A WEASEL"
define mutate_chance = 0.08
define alphabet = [('A'..'Z')..., ' ']
define C = 100

func fitness(str) { str.chars ~Z== target.chars -> count(true) }
func mutate(str)  { str.gsub(/(.)/, {|s1| 1.rand < mutate_chance ? alphabet.pick : s1 }) }

for (
    var (i, parent) = (0, alphabet.rand(target.len).join);
    parent != target;
    parent = C.of{ mutate(parent) }.max_by(fitness)
) { printf("%6d: '%s'\n", i++, parent) }
```



## Sinclair ZX81 BASIC

Requires at least 2k of RAM. Displaying everything while it's running (generation count, parent, children, and their fitness scores) slows it down somewhat; but it would be very slow anyway, and it's nice to be able to look in on it from time to time and see how the program's getting along.

```basic
 10 LET A$="ABCDEFGHIJKLMNOPQRSTUVWXYZ "
 20 LET T$="METHINKS IT IS LIKE A WEASEL"
 30 LET L=LEN T$
 40 LET C=10
 50 LET M=0.05
 60 LET G=0
 70 DIM C$(C,L)
 80 LET P$=""
 90 FOR I=1 TO L
100 LET P$=P$+A$(INT (RND*LEN A$)+1)
110 NEXT I
120 PRINT AT 1,0;P$
130 LET S$=P$
140 GOSUB 390
150 LET N=R
160 PRINT AT 1,30;N
170 PRINT AT 0,4;G
180 IF P$=T$ THEN GOTO 440
190 FOR I=1 TO C
200 FOR J=1 TO L
210 LET C$(I,J)=P$(J)
220 IF RND<=M THEN LET C$(I,J)=A$(INT (RND*LEN A$)+1)
230 PRINT AT I+2,J-1;C$(I,J)
240 NEXT J
250 PRINT AT I+2,30;"  "
260 NEXT I
270 LET F=0
280 FOR I=1 TO C
290 LET S$=C$(I)
300 GOSUB 390
310 PRINT AT I+2,30;R
320 IF R>N THEN LET F=I
330 IF R>N THEN LET N=R
340 NEXT I
350 IF F>0 THEN LET P$=C$(F)
360 LET G=G+1
370 PRINT AT 1,0;P$
380 GOTO 160
390 LET R=0
400 FOR K=1 TO L
410 IF S$(K)=T$(K) THEN LET R=R+1
420 NEXT K
430 RETURN
```

```txt
    349
METHINKS IT IS LIKE A WEASEL  28

METHINKS ITCIS LIKE A WEASEL  27
METHINKS ITCIS LIKE A WEASEL  27
METHINKS ITCIS LIKECA WEASEL  26
METHINKS ITCIS LPKE AUWEASLL  24
METHINKS CTCIS LIKP A WEASEL  25
METHINKS ITCIC LIKE A WEASER  25
METHINKS I CIS LIKE A WEASEL  26
METHINKS IYCIS LIREAA WEAWEL  23
METHINKS IT IS LIKE A WEASEL  28
METHINKSIITCIS LIKE A WEASEJ  25
```



## Smalltalk

```smalltalk
String subclass: Mutant [
    <shape: #character>

    Target := Mutant from: 'METHINKS IT IS LIKE A WEASEL'.
    Letters := ' ABCDEFGHIJKLMNOPQRSTUVWXYZ'.

    Mutant class >> run: c rate: p
        ["Run Evolutionary algorighm, using c copies and mutate rate p."
        | pool parent |
        parent := self newRandom.
        pool := Array new: c+1.

        [parent displayNl.
        parent = Target] whileFalse:
            [1 to: c do: [:i | pool at: i put: (parent copy mutate: p)].
            pool at: c+1 put: parent.
            parent := pool fold: [:winner :each | winner fittest: each]]]

    Mutant class >> newRandom
        [^(self new: Target size)
            initializeToRandom;
            yourself]

    initializeToRandom
        [self keys do: [:i | self at: i put: self randomLetter]]

    mutate: p
        [self keys do:
            [:i |
            Random next <= p ifTrue: [self at: i put: self randomLetter]]]

    fitness
        [| score |
        score := 0.
        self with: Target do:
            [:me :you |
            me = you ifTrue: [score := score + 1]].
        ^score]

    fittest: aMutant
        [^self fitness > aMutant fitness
            ifTrue: [self]
            ifFalse: [aMutant]]

    randomLetter
        [^Letters at: (Random between: 1 and: Letters size)]
]
```


Use example:

```smalltalk
st
 Mutant run: 2500 rate: 0.1
QJUUIQHYXEZORSXGJCAHEWACH KG
QJUUIQHYXEZORSXGJCAHEWWCMSKG
QEUUIUHYXEZORSOGICAHYWWCSSKG
QETUIUHGXEZORS GICE YWWCSSEG
METUIUHSXOZORS OICE YWWCSSEG
METUIUHSXOZORS OICE Y WCSSEG
METUIUHSXOZMIS OIOE Y WCNSEG
METKIUKSTOFMIS LIOE Y WCNSEG
METKINKSTOFMIS LIKE E WCNSEG
METKINKSTOFMIS LIKE F WCNSEL
METHINKSTOF IS LIKE F WCNSEL
METHINKS OW IS LIKE F WCNSEL
METHINKS IW IS LIKE F WCNSEL
METHINKS IW IS LIKE C WCASEL
METHINKS IW IS LIKE C WCASEL
METHINKS IW IS LIKE A WCASEL
METHINKS IW IS LIKE A WCASEL
METHINKS IW IS LIKE A WEASEL
METHINKS IT IS LIKE A WEASEL
Mutant
```



## Swift


```swift
func evolve(
  to target: String,
  parent: inout String,
  mutationRate: Int,
  copies: Int
) {
  var parentFitness: Int {
    return fitness(target: target, sentence: parent)
  }

  var generation = 0

  while parent != target {
    generation += 1

    let bestOfGeneration =
        (0..<copies)
          .map({_ in mutate(sentence: parent, rate: mutationRate) })
          .map({ (fitness(target: target, sentence: $0), $0) })
          .sorted(by: { $0.0 < $1.0 })
          .first!

    if bestOfGeneration.0 < parentFitness {
      print("Gen \(generation) produced better fit. \(bestOfGeneration.1) with fitness \(bestOfGeneration.0)")
      parent = bestOfGeneration.1
    }
  }
}

func fitness(target: String, sentence: String) -> Int {
  return zip(target, sentence).filter(!=).count
}

func mutate(sentence: String, rate: Int) -> String {
  return String(
    sentence.map({char in
      if Int.random(in: 1...100) - rate <= 0 {
        return "ABCDEFGHIJKLMNOPQRSTUVWXYZ ".randomElement()!
      } else {
        return char
      }
    })
  )
}

let target = "METHINKS IT IS LIKE A WEASEL"
let copies = 100
let mutationRate = 20

var start = mutate(sentence: target, rate: 100)

print("target: \(target)")
print("Gen 0: \(start) with fitness \(fitness(target: target, sentence: start))")

evolve(to: target, parent: &start, mutationRate: mutationRate, copies: 100)
```


<div style='height: 15em; overflow: scroll'>
```txt
target: METHINKS IT IS LIKE A WEASEL
Gen 0: LODDQKXSYLDCNBEGKOOPAPNBQEVO with fitness 26
Gen 1 produced better fit. LVDDQKXS LLCIBEDKOOPAPNBQELO with fitness 24
Gen 2 produced better fit. LVDKQGXS LLCIBELKODPAPNBQELO with fitness 23
Gen 3 produced better fit. LVDKQGXS LLNISELKTEPAQNBQELO with fitness 21
Gen 4 produced better fit. LVDKQGXS ILMISELKREPA NBQELO with fitness 19
Gen 5 produced better fit. MHDKQGXS ILMISELKREPA NBQELO with fitness 18
Gen 7 produced better fit. MHDHQGXS ILMIS LKRHPA XBQELS with fitness 17
Gen 8 produced better fit. MHDHORXS IHMIS LIUHPA XBNEYS with fitness 16
Gen 9 produced better fit. MHDHORXS IHMIS LIMEPA XBNEYS with fitness 15
Gen 10 produced better fit. MKDHORRS IHMIS LIKEGA X MSYS with fitness 13
Gen 11 produced better fit. MKDHONRS IHMIS LIKEGA X MSYO with fitness 12
Gen 12 produced better fit.  BDHONRS ILMIS LIKEGA T MSEL with fitness 11
Gen 13 produced better fit. GBPHONRS IQMIS LIKE A T MSEL with fitness 10
Gen 19 produced better fit. GOPHONKS IX IS LIKE A T QSEL with fitness 8
Gen 22 produced better fit. MOPHKNKS IX IS LIKE A TIQSEL with fitness 7
Gen 35 produced better fit. MEPHKNKS IX IS LIKE A TIQSEL with fitness 6
Gen 67 produced better fit. MEPHKNKS IX IS LIKE A TIASEL with fitness 5
Gen 132 produced better fit. MEPHINKS IZ IS LIKE A TIASEL with fitness 4
Gen 193 produced better fit. MEPHINKS IZ IS LIKE A TEASEL with fitness 3
Gen 572 produced better fit. MEPHINKS IT IS LIKE A TEASEL with fitness 2
Gen 579 produced better fit. MEPHINKS IT IS LIKE A WEASEL with fitness 1
Gen 937 produced better fit. METHINKS IT IS LIKE A WEASEL with fitness 0
```
</div>


## Tcl

```tcl
package require Tcl 8.5

# A function to select a random character from an argument string
proc tcl::mathfunc::randchar s {
    string index $s [expr {int([string length $s]*rand())}]
}

# Set up the initial variables
set target "METHINKS IT IS LIKE A WEASEL"
set charset "ABCDEFGHIJKLMNOPQRSTUVWXYZ "
set parent [subst [regsub -all . $target {[expr {randchar($charset)}]}]]
set MaxMutateRate 0.91
set C 100

# Work with parent and target as lists of characters so iteration is more efficient
set target [split $target {}]
set parent [split $parent {}]

# Generate the fitness *ratio*
proc fitness s {
    global target
    set count 0
    foreach c1 $s c2 $target {
	if {$c1 eq $c2} {incr count}
    }
    return [expr {$count/double([llength $target])}]
}
# This generates the converse of the Python version; logically saner naming
proc mutateRate {parent} {
    expr {(1.0-[fitness $parent]) * $::MaxMutateRate}
}
proc mutate {rate} {
    global charset parent
    foreach c $parent {
	lappend result [expr {rand() <= $rate ? randchar($charset) : $c}]
    }
    return $result
}
proc que {} {
    global iterations parent
    puts [format "#%-4i, fitness %4.1f%%, '%s'" \
	    $iterations [expr {[fitness $parent]*100}] [join $parent {}]]
}

while {$parent ne $target} {
    set rate [mutateRate $parent]
    if {!([incr iterations] % 100)} que
    set copies [list [list $parent [fitness $parent]]]
    for {set i 0} {$i < $C} {incr i} {
	lappend copies [list [set copy [mutate $rate]] [fitness $copy]]
    }
    set parent [lindex [lsort -real -decreasing -index 1 $copies] 0 0]
}
puts ""
que
```

Produces this example output:

```txt
#100 , fitness 42.9%, 'GSTBIGFS ITLSS LMD  NNJPESZL'
#200 , fitness 57.1%, 'SCTHIOAS ITHIS LNK  PPLEASOG'
#300 , fitness 64.3%, 'ILTHIBKS IT IS LNKE PPLEBSIS'
#400 , fitness 96.4%, 'METHINKS IT IS LIKE A  EASEL'

#431 , fitness 100.0%, 'METHINKS IT IS LIKE A WEASEL'
```

Note that the effectiveness of the algorithm can be tuned by adjusting the mutation rate; with a <tt>C</tt>adre size of 100, a very rapid convergence happens for a maximum mutation rate of 0.3…


### Alternate Presentation

This alternative presentation factors out all assumption of what constitutes a “fit” solution to the <code>fitness</code> command, which is itself just a binding of the <code>fitnessByEquality</code> procedure to a particular target. None of the rest of the code knows anything about what constitutes a solution (and only <code>mutate</code> and <code>fitness</code> really know much about the data being evolved).

```tcl
package require Tcl 8.5
proc tcl::mathfunc::randchar {} {
    # A function to select a random character
    set charset "ABCDEFGHIJKLMNOPQRSTUVWXYZ "
    string index $charset [expr {int([string length $charset] * rand())}]
}
set target "METHINKS IT IS LIKE A WEASEL"
set initial [subst [regsub -all . $target {[expr randchar()]}]]
set MaxMutateRate 0.91
set C 100

# A place-wise equality function defined over two lists (assumed equal length)
proc fitnessByEquality {target s} {
    set count 0
    foreach c1 $s c2 $target {
	if {$c1 eq $c2} {incr count}
    }
    return [expr {$count / double([llength $target])}]
}
# Generate the fitness *ratio* by place-wise equality with the target string
interp alias  {} fitness  {} fitnessByEquality [split $target {}]

# This generates the converse of the Python version; logically saner naming
proc mutationRate {individual} {
    global MaxMutateRate
    expr {(1.0-[fitness $individual]) * $MaxMutateRate}
}

# Mutate a string at a particular rate (per character)
proc mutate {parent rate} {
    foreach c $parent {
	lappend child [expr {rand() <= $rate ? randchar() : $c}]
    }
    return $child
}

# Pretty printer
proc prettyPrint {iterations parent} {
    puts [format "#%-4i, fitness %5.1f%%, '%s'" $iterations \
	[expr {[fitness $parent]*100}] [join $parent {}]]
}

# The evolutionary algorithm itself
proc evolve {initialString} {
    global C

    # Work with the parent as a list; the operations are more efficient
    set parent [split $initialString {}]

    for {set iterations 0} {[fitness $parent] < 1} {incr iterations} {
	set rate [mutationRate $parent]

	if {$iterations % 100 == 0} {
	    prettyPrint $iterations $parent
	}

	set copies [list [list $parent [fitness $parent]]]
	for {set i 0} {$i < $C} {incr i} {
	    lappend copies [list \
		    [set copy [mutate $parent $rate]] [fitness $copy]]
	}
	set parent [lindex [lsort -real -decreasing -index 1 $copies] 0 0]
    }
    puts ""
    prettyPrint $iterations $parent

    return [join $parent {}]
}

evolve $initial
```



## uBasic/4tH

This is a bit of a stretch, since uBasic/4tH doesn't support strings. Hence, the array is used to store the data.
<lang>T = 0                                  ' Address of target
L = 28                                 ' Length of string
P = T + L                              ' Address of parent
R = 6                                  ' Mutation rate in percent
C = 7                                  ' Number of children
B = 0                                  ' Best rate so far

Proc _Initialize                       ' Initialize

Do                                     ' Now start mutating
  I = 0                                ' Nothing does it better so far

  For x = 2 To C+1                     ' Addresses of children
    Proc _MutateDNA (x, P, R)          ' Now mutate their DNA
    F = FUNC(_Fitness (x, T))          ' Check for fitness
    If F > B Then B = F : I = x        ' If fitness of child is better
  Next                                 ' Make it the best score

  If I Then                            ' If a better child was found
    Proc _MakeParent (P, I)            ' Make the child the parent
    Proc _PrintParent (P)              ' Print the new parent
  EndIf

  Until B = L                          ' Until top score equals length
Loop

End


_MutateDNA Param(3)                    ' Mutate an entire DNA
  Local(1)

  For d@ = 0 to L-1                    ' For the entire string
    If c@ > Rnd(100) Then              ' If mutation rate is met
       @(a@*L+d@) = Ord("A") + Rnd(27) ' Mutate the gene
    Else
       @(a@*L+d@) = @(b@+d@)           ' Otherwise copy it from the parent
    EndIf
  Next
Return


_Fitness Param(2)                      ' Check for fitness
  Local(2)

  c@ = 0                               ' Fitness is zero
  For d@ = 0 to L-1                    ' For the entire string
    If @(a@*L+d@) = @(b@+d@) Then c@ = c@ + 1
  Next                                 ' If string matches, increment score
Return (c@)                            ' Return the fitness


_MakeParent Param(2)                   ' Make a child into a parent
  Local(1)

  For c@ = 0 to L-1                    ' For the entire string
    @(a@+c@) = @(b@*L+c@)              ' Copy the DNA gene by gene
  Next
Return


_PrintParent Param(1)                  ' Print the parent
  Local(1)

  For b@ = 0 to L-1                    ' For the entire string
    If (@(a@+b@)) > Ord ("Z") Then
      Print " ";                       ' Cater for the space
    Else
      Print CHR(@(a@+b@));             ' Print a gene
    EndIf
  Next

  Print                                ' Issue a linefeed
Return


_Initialize                            ' Initialize target and parent
  @(0)=Ord("M")                        ' Initialize target (long!)
  @(1)=Ord("E")                        ' Character by character
  @(2)=Ord("T")
  @(3)=Ord("H")
  @(4)=Ord("I")
  @(5)=Ord("N")
  @(6)=Ord("K")
  @(7)=Ord("S")
  @(8)=Ord("Z")+1
  @(9)=Ord("I")
  @(10)=Ord("T")
  @(11)=Ord("Z")+1
  @(12)=Ord("I")
  @(13)=Ord("S")
  @(14)=Ord("Z")+1
  @(15)=Ord("L")
  @(16)=Ord("I")
  @(17)=Ord("K")
  @(18)=Ord("E")
  @(19)=Ord("Z")+1
  @(20)=Ord("A")
  @(21)=Ord("Z")+1
  @(22)=Ord("W")
  @(23)=Ord("E")
  @(24)=Ord("A")
  @(25)=Ord("S")
  @(26)=Ord("E")
  @(27)=Ord("L")

  Proc _MutateDNA (P/L, P, 100)          ' Now mutate the parent DNA
Return
```

```txt
ZACXCLONTNTEAMJXYYFEP QQMDTA
ZACXILONTBTEALJXYYFEP QQPDTA
ZACNILONTBTEALJXYYXER WQPDTA
ZACNILKNTBTEALJXYYXER WQPDTA
ZACNILKNWBTEALJLYYXER WQPDTA
ZACNIEKNYITEALJLYYPER WSPDTA
ZACNIEKNYITEALJLYYPEA WSPDTA
QYCNIEKNYITEALJLYYPEA WSPDTL
MYCGIEKNYITEALJLYYPEA WSPDTL
MYCGIGKN ITEALJLYYPEA WSUDTL
MYCJIGKN ITEKLJLIYPEA WSUDTL
MYCJIGKN ITEKLJLIYP A WSUDTL
MYCJIGKN ITUKL LIYP A WSUDCL
MYCJIGKS ITUKL LIYP A WSRDCL
MYCJIGKS ITUUL LIYP A WSRDEL
MYCJIGKS ITUUL LIYP A WSRSEL
MYCJIGKS ITTUL LIYP A WWASEL
MECJIGKS ITTUL LIYP A WWASEL
MECHIGKS ITTUL LIYP A WWASEL
MECHIGKS ITTUS LIYP A WWASEL
MECHINKS ITTUS LIYP A WWASEL
MECHINKS ITOUS LIYE A WWASEL
MECHINKS ITOUS LIYE A WEASEL
MECHINKS ITOIS LIYE A WEASEL
MECHINKS ITOIS LIKE A WEASEL
MECHINKS IT IS LIKE A WEASEL
METHINKS IT IS LIKE A WEASEL

0 OK, 0:962
```



## Ursala

The fitness function is given by the number of characters in the
string not matching the target. (I.e., 0 corresponds to optimum
fitness.) With characters mutated at a fixed probability of 10%, it
takes about 500 iterations give or take 100.


```Ursala
#import std
#import nat

rand_char = arc ' ABCDEFGHIJKLMNOPQRSTUVWXYZ'

target = 'METHINKS IT IS LIKE A WEASEL'

parent = rand_char* target

fitness = length+ (filter ~=)+ zip/target

mutate("string","rate") = "rate"%~?(rand_char,~&)* "string"

C = 32

evolve = @iiX ~&l->r @r -*iota(C); @lS nleq$-&l+ ^(fitness,~&)^*C/~&h mutate\*10

#cast %s

main = evolve parent
```

output:

```txt

'METHINKS IT IS LIKE A WEASEL'

```



## UTFool


```UTFool

···
http://rosettacode.org/wiki/Evolutionary_algorithm
···
■ Evolutionary
  § static
    target⦂ String: "METHINKS IT IS LIKE A WEASEL"
    letter⦂ char[]: "ABCDEFGHIJKLMNOPQRSTUVWXYZ ".toCharArray°
    parent⦂ String
    random⦂ java.util.Random°
    rate⦂ double: 0.5
    C⦂ int: 1000

    ▶ fittness⦂ int    · computes the 'closeness' of its
    • argument⦂ String · to the target string
      closeness⦂ int: 0
      ∀ i ∈ 0 … target.length°
        closeness◥ if target.charAt i = argument.charAt i
      return closeness

    ▶ mutate⦂ String · returns a copy of the
    • given⦂ String  · with some characters probably mutated
    • rate⦂ double
      copy⦂ char[]: given.toCharArray°
      ∀ i ∈ 0 … given.length°
        copy[i]: letter[random.nextInt letter.length] if rate > random.nextDouble°
      return String.valueOf copy

    ▶ main
    • args⦂ String[]
      ancest⦂ StringBuilder°
      ∀ i ∈ 0 … target.length°
        ancest.append letter[random.nextInt letter.length]
      parent: ancest.toString°
      currentFittness⦂ int: fittness parent
      generation⦂ int: 0
      🔁 until the parent ≈ target
        if fittness parent > currentFittness
           currentFittness: fittness parent
           System.out.println "Fittness of generation #⸨generation⸩ is ⸨currentFittness⸩"
        for each time from 1 to C
            mutation⦂ String: mutate parent, rate
            parent: mutation if fittness parent < fittness mutation
        generation◥
      System.out.println "Target reached by generation #⸨generation⸩"

```



## vbscript


```vbscript

'This is the string we want to "evolve" to. Any string of any length will
'do as long as it consists only of upper case letters and spaces.

Target  = "METHINKS IT IS LIKE A WEASEL"

'This is the pool of letters that will be selected at random for a mutation

letters = " ABCDEFGHIJKLMNOPQRSTUVWXYZ"

'A mutation rate of 0.5 means that there is a 50% chance that one letter
'will be mutated at random in the next child

mutation_rate = 0.5

'Set for 10 children per generation

Dim child(10)

'Generate the first guess as random letters

Randomize
Parent = ""

for i = 1 to len(Target)
    Parent = Parent & Mid(letters,Random(1,Len(letters)),1)
next

gen = 0

Do
    bestfit = 0
    bestind = 0

    gen = gen + 1

    'make n copies of the current string and find the one
    'that best matches the target string

    For i = 0 to ubound(child)

        child(i) = Mutate(Parent, mutation_rate)

        fit = Fitness(Target, child(i))

        If fit > bestfit Then
            bestfit = fit
            bestind = i
        End If

    Next

    'Select the child that has the best fit with the target string

    Parent = child(bestind)
    Wscript.Echo parent, "(fit=" & bestfit & ")"

Loop Until Parent = Target

Wscript.Echo vbcrlf & "Generations = " & gen

'apply a random mutation to a random character in a string

Function Mutate ( ByVal str , ByVal rate )

    Dim pos        'a random position in the string'
    Dim ltr        'a new letter chosen at random    '

    If rate > Rnd(1) Then

        ltr = Mid(letters,Random(1,len(letters)),1)
        pos = Random(1,len(str))
        str = Left(str, pos - 1) & ltr & Mid(str, pos + 1)

    End If

    Mutate = str

End Function

'returns the number of letters in the two strings that match

Function Fitness (ByVal str , ByVal ref )

    Dim i

    Fitness = 0

    For i = 1 To Len(str)
        If Mid(str, i, 1) = Mid(ref, i, 1) Then Fitness = Fitness + 1
    Next

End Function

'Return a random integer in the range lower to upper (inclusive)

Private Function Random ( lower , upper )
  Random = Int((upper - lower + 1) * Rnd + lower)
End Function
```


Example output:


```txt

JTXBMMYUFUWTKJRVVNOGGUAIGSIF (fit=1)
JTXBMMYUFYWTKJRVVNOGGUAIGSIF (fit=1)
JTXKMMYUFYWTKJRVVNOGGUAIGSIF (fit=1)
JTXKMMYUFYWTKJRVVNOGGUAIGSIF (fit=1)
UTXKMMYUFYWTKJRVVNOGGUAIGSIF (fit=1)
UTXKMMYUFYWTKJJVVNOGGUAIGSIF (fit=1)
UTXKMMYUFYWTKJJVVNDGGUAIGSIF (fit=1)
UTXKMMYUFYWTKJJVVNDGGUAIGSIF (fit=1)
UTXKMMYUFYWTKJJVVNDGGUWIGSIF (fit=2)
UTXKMMYUFYWTKJJVVNDGGUWIGSIF (fit=2)
UTXKMMYUFYWTKJJVVNDGGUWIGSIF (fit=2)
UBXKMMYUFYWTKJJVVNDGGUWIGSIF (fit=2)
UBNKMMYUFYWTKJJVVNDGGUWIGSIF (fit=2)
.
.
.
METHINKS IT IS LIKEVA WEASEL (fit=27)
METHINKS IT IS LIKEVA WEASEL (fit=27)
METHINKS IT IS LIKEVA WEASEL (fit=27)
METHINKS IT IS LIKEVA WEASEL (fit=27)
METHINKS IT IS LIKEVA WEASEL (fit=27)
METHINKS IT IS LIKE A WEASEL (fit=28)

Generations = 580

```



## Visual Basic

Adapted from BBC Basic Code in this page. One diference from BBC Basic code is that in this code mutations are always good

```Visual Basic



Option Explicit

Private Sub Main()
   Dim Target
   Dim Parent
   Dim mutation_rate
   Dim children
   Dim bestfitness
   Dim bestindex
   Dim Index
   Dim fitness

      Target = "METHINKS IT IS LIKE A WEASEL"
      Parent = "IU RFSGJABGOLYWF XSMFXNIABKT"
      mutation_rate = 0.5
       children = 10
      ReDim child(children)

      Do
        bestfitness = 0
        bestindex = 0
        For Index = 1 To children
          child(Index) = FNmutate(Parent, mutation_rate, Target)
          fitness = FNfitness(Target, child(Index))
          If fitness > bestfitness Then
            bestfitness = fitness
            bestindex = Index
          End If
        Next Index

        Parent = child(bestindex)
        Debug.Print Parent
      Loop Until Parent = Target
      End


End Sub

Function FNmutate(Text, Rate, ref)
   Dim C As Integer
   Dim Aux As Integer

     If Rate > Rnd(1) Then
        C = 63 + 27 * Rnd() + 1
        If C = 64 Then C = 32
        Aux = Len(Text) * Rnd() + 1
        If Mid(Text, Aux, 1) <> Mid(ref, Aux, 1) Then
            Text = Left(Text, Aux - 1) & Chr(C) & Mid(Text, Aux + 1)
        End If

     End If
      FNmutate = Text
End Function
Function FNfitness(Text, ref)
    Dim I, F
      For I = 1 To Len(Text)
        If Mid(Text, I, 1) = Mid(ref, I, 1) Then F = F + 1
      Next
      FNfitness = F / Len(Text)
End Function

```


Example output:


```txt

U RFSGJABGOLYWF XSMFXNIABKT
IU RFSGJABGOLYWF XSMFXNIABKT
IU NFSGJABGOLYWF XSMFXNIABKT
IU NFSGJABGOLYWF XSMFXNIABKT
IU NFSGJABGOLYWF XSMFXNIABOT
IUFNISGJABGOLYWF TSMFXCIABOT
IUFNISGJABGOLYWF TSMFXCIABOT
IUFNISGRABGOLYWF TSMFXCIABOT
.....
IEFMI GUASGLOYWF DSMFPRIAROT
IEFMI GUASGLOYWF DSMFPRZAROT
IEFMI GUASGLOYWFFDSMFPRZAROT
IEFMI GUASGLOYWFFDSMFPRZAQOT
IEFMI GUASGLOYBFFDSMFPRZAQOT
.....
METHINKS IT IS LVKE A WEASEL
METHINKS IT IS LVKE A WEASEL
METHINKS IT IS LRKE A WEASEL
METHINKS IT IS LRKE A WEASEL
METHINKS IT IS LRKE A WEASEL
METHINKS IT IS LRKE A WEASEL
METHINKS IT IS LIKE A WEASEL


```



## XPL0


```XPL0
include c:\cxpl\codes;          \intrinsic code declarations
string  0;                      \use zero-terminated convention (instead of MSb)

def     MutateRate = 15,        \1 chance in 15 of a mutation
        Copies = 30;            \number of mutated copies
char    Target, AlphaTbl;
int     SizeOfAlpha;


func    StrLen(Str);    \Return the number of characters in a string
char    Str;
int     I;
for I:= 0 to -1>>1-1 do
        if Str(I) = 0 then return I;


func    Unfitness(A, B); \Return number of characters different between A and B
char    A, B;
int     I, C;
[C:= 0;
for I:= 0 to StrLen(A)-1 do
        if A(I) # B(I) then C:= C+1;
return C;
];      \Unfitness


proc    Mutate(A, B);   \Copy string A to B, but with each character of B having
char    A, B;           \ a 1 in MutateRate chance of differing from A
int     I;
[for I:= 0 to StrLen(A)-1 do
        B(I):= if Ran(MutateRate) then A(I) else AlphaTbl(Ran(SizeOfAlpha));
B(I):= 0;               \terminate string
];      \Mutate


int     I, BestI, Diffs, Best, Iter;
def     SizeOfTarget = 28;
char    Specimen(Copies, SizeOfTarget+1);
int     ISpecimen, Temp;

[Target:= "METHINKS IT IS LIKE A WEASEL";
AlphaTbl:= "ABCDEFGHIJKLMNOPQRSTUVWXYZ ";
SizeOfAlpha:= StrLen(AlphaTbl);
ISpecimen:= Specimen;   \integer accesses pointers rather than bytes

\Initialize first Specimen, the parent, to a random string
for I:= 0 to SizeOfTarget-1 do
        Specimen(0,I):= AlphaTbl(Ran(SizeOfAlpha));
Specimen(0,I):= 0;      \terminate string

Iter:= 0;
repeat  for I:= 1 to Copies-1 do Mutate(ISpecimen(0), ISpecimen(I));

        Best:= SizeOfTarget;            \find best matching string
        for I:= 0 to Copies-1 do
                [Diffs:= Unfitness(Target, ISpecimen(I));
                if Diffs < Best then [Best:= Diffs;  BestI:= I];
                ];
        if BestI \#0\ then              \swap best string with first string
                [Temp:= ISpecimen(0);
                ISpecimen(0):= ISpecimen(BestI);
                ISpecimen(BestI):= Temp;
                ];
        Text(0, "Iter ");  IntOut(0, Iter);
        Text(0, " Score ");  IntOut(0, Best);
        Text(0, ": ");  Text(0, ISpecimen(0));  CrLf(0);
        Iter:= Iter+1;
until   Best = 0;
]
```


Example output:


```txt

Iter 0 Score 26: YIOHAVRGQLXRZJOSHNPRY VIQDNK
Iter 1 Score 25: YYOHAVRGQLX ZJOSHNPRY VIQDNK
Iter 2 Score 24: YYOHAVRGQLX ZJOSHNPRY VIQSNK
Iter 3 Score 24: YYOHAVRGQLX ZJOSHNPRY VIQSNK
Iter 4 Score 23: YYOHAVRGQLX ZJOSHNERY VIQSNK
Iter 5 Score 22: YYUHAVRGQLX ZJOSHNERY JDQSNL
...
Iter 200 Score 1: METHINKS IT IS LIKE K WEASEL
Iter 201 Score 1: METHINKS IT IS LIKE K WEASEL
Iter 202 Score 1: METHINKS IT IS LIKE K WEASEL
Iter 203 Score 0: METHINKS IT IS LIKE A WEASEL

```



## zkl

```zkl
const target = "METHINKS IT IS LIKE A WEASEL";
const C = 100;  // Number of children in each generation.
const P = 0.05; // Mutation probability.
const A2ZS = ["A".."Z"].walk().append(" ").concat();
fcn fitness(s){ Utils.zipWith('!=,target,s).sum(0) } // bigger is worser
fcn rnd{ A2ZS[(0).random(27)] }
fcn mutate(s){ s.apply(fcn(c){ if((0.0).random(1) < P) rnd() else c }) }

parent := target.len().pump(String,rnd);  // random string of "A..Z "
gen:=0; do{  // mutate C copies of parent and pick the fittest
   parent = (0).pump(C,List,T(Void,parent),mutate)
	    .reduce(fcn(a,b){ if(fitness(a)<fitness(b)) a else b });
   println("Gen %2d, dist=%2d: %s".fmt(gen+=1, fitness(parent), parent));
}while(parent != target);
```

```txt
Gen  1, dist=26: JNGUIMCMOLLEULERIFPCYYZA  JR
Gen  2, dist=25: JNGUIMCMOLLEULERIFECYYZA  JR
Gen  3, dist=24: JNGUIMVMOLLEILERIFECYYZA  JU
...
Gen  7, dist=20: GNPHIMKMCLLEI ERIFECY ZA SJU
Gen  8, dist=19: GNPHIMKMCLLEI ERIKECY Z  SJH
...
Gen 13, dist=14: CNTHIMKSCLHEIB RIKECY ME S L
Gen 14, dist=14: CNTHIMKSCLHEIB RIKECY ME S L
Gen 15, dist=14: CNTHIMKSCLHEIB RIKECY ME S L
...
Gen 24, dist= 7: MLTHIMKS LTEIB MIKE Y WEASEL
Gen 25, dist= 7: MLTHIMKS LTEIB MIKE Y WEASEL
Gen 26, dist= 7: MLTHIMKS LTEIB KIKE Y WEASEL
...
Gen 48, dist= 1: METHINKS IT IS LIKE Z WEASEL
Gen 49, dist= 1: METHINKS IT IS LIKE G WEASEL
Gen 50, dist= 0: METHINKS IT IS LIKE A WEASEL

```

