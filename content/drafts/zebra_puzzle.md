+++
title = "Zebra puzzle"
description = ""
date = 2019-08-28T12:05:13Z
aliases = []
[extra]
id = 10989
[taxonomies]
categories = []
tags = []
+++

[[File:zebra.png|550px||right]]
{{task|Puzzles}}
[[Category:Constraint Handling Rules]]
[[Category:Puzzles]]

The [[wp:Zebra puzzle|Zebra puzzle]], a.k.a. Einstein's Riddle,
is a logic puzzle which is to be solved programmatically.


It has several variants, one of them this:
# There are five houses.
# The English man lives in the red house.
# The Swede has a dog.
# The Dane drinks tea.
# The green house is immediately to the left of the white house.
# They drink coffee in the green house.
# The man who smokes Pall Mall has birds.
# In the yellow house they smoke Dunhill.
# In the middle house they drink milk.
# The Norwegian lives in the first house.
# The man who smokes Blend lives in the house next to the house with cats.
# In a house next to the house where they have a horse, they smoke Dunhill.
# The man who smokes Blue Master drinks beer.
# The German smokes Prince.
# The Norwegian lives next to the blue house.
# They drink water in a house next to the house where they smoke Blend.


The question is, who owns the zebra?

Additionally, list the solution for all the houses.
Optionally, show the solution is unique.


;Related tasks:
*   [[Dinesman's multiple-dwelling problem]]
*   [[Twelve statements]]





## Ada

Not the prettiest Ada, but it's simple and very fast. Similar to my Dinesman's code; uses enums to keep things readable.

```Ada
with Ada.Text_IO; use Ada.Text_IO;
procedure Zebra is
   type Content is (Beer, Coffee, Milk, Tea, Water,
      Danish, English, German, Norwegian, Swedish,
      Blue, Green, Red, White, Yellow,
      Blend, BlueMaster, Dunhill, PallMall, Prince,
      Bird, Cat, Dog, Horse, Zebra);
   type Test is (Drink, Person, Color, Smoke, Pet);
   type House is (One, Two, Three, Four, Five);
   type Street is array (Test'Range, House'Range) of Content;
   type Alley is access all Street;

   procedure Print (mat : Alley) is begin
      for H in House'Range loop
         Put(H'Img&": ");
         for T in Test'Range loop
            Put(T'Img&"="&mat(T,H)'Img&" ");
      end loop; New_Line; end loop;
   end Print;

   function FinalChecks (mat : Alley) return Boolean is
      function Diff (A, B : Content; CA , CB : Test) return Integer is begin
         for H1 in House'Range loop for H2 in House'Range loop
               if mat(CA,H1) = A and mat(CB,H2) = B then
                  return House'Pos(H1) - House'Pos(H2);
               end if;
         end loop; end loop;
      end Diff;
   begin
      if abs(Diff(Norwegian, Blue, Person, Color)) = 1
        and Diff(Green, White, Color, Color) = -1
        and abs(Diff(Horse, Dunhill, Pet, Smoke)) = 1
        and abs(Diff(Water, Blend, Drink, Smoke)) = 1
        and abs(Diff(Blend, Cat, Smoke, Pet)) = 1
      then return True;
      end if;
      return False;
   end FinalChecks;

   function Constrained (mat : Alley; atest : Natural) return Boolean is begin
      --  Tests seperated into levels for speed, not strictly necessary
      --  As such, the program finishes in around ~0.02s
      case Test'Val (atest) is
         when Drink => --  Drink
            if mat (Drink, Three) /= Milk then return False; end if;
            return True;
         when Person => --  Drink+Person
            for H in House'Range loop
               if (mat(Person,H) = Norwegian and H /= One)
               or (mat(Person,H) = Danish and mat(Drink,H) /= Tea)
               then return False; end if;
            end loop;
            return True;
         when Color => --  Drink+People+Color
            for H in House'Range loop
               if (mat(Person,H) = English and mat(Color,H) /= Red)
               or (mat(Drink,H) = Coffee and mat(Color,H) /= Green)
               then return False; end if;
            end loop;
            return True;
         when Smoke => --  Drink+People+Color+Smoke
            for H in House'Range loop
               if (mat(Color,H) = Yellow and mat(Smoke,H) /= Dunhill)
               or (mat(Smoke,H) = BlueMaster and mat(Drink,H) /= Beer)
               or (mat(Person,H) = German and mat(Smoke,H) /= Prince)
               then return False; end if;
            end loop;
            return True;
         when Pet => --  Drink+People+Color+Smoke+Pet
            for H in House'Range loop
               if (mat(Person,H) = Swedish and mat(Pet,H) /= Dog)
               or (mat(Smoke,H) = PallMall and mat(Pet,H) /= Bird)
               then return False; end if;
            end loop;
            return FinalChecks(mat); --  Do the next-to checks
      end case;
   end Constrained;

   procedure Solve (mat : Alley; t, n : Natural) is
      procedure Swap (I, J : Natural) is
         temp : constant Content := mat (Test'Val (t), House'Val (J));
      begin
         mat (Test'Val (t), House'Val (J)) := mat (Test'Val (t), House'Val (I));
         mat (Test'Val (t), House'Val (I)) := temp;
      end Swap;
   begin
      if n = 1 and Constrained (mat, t) then --  test t passed
         if t < 4 then Solve (mat, t + 1, 5); --  Onto next test
         else Print (mat); return; --  Passed and t=4 means a solution
         end if;
      end if;
      for i in 0 .. n - 1 loop --  The permutations part
         Solve (mat, t, n - 1);
         if n mod 2 = 1 then Swap (0, n - 1);
         else Swap (i, n - 1); end if;
      end loop;
   end Solve;

   myStreet : aliased Street;
   myAlley : constant Alley := myStreet'Access;
begin
   for i in Test'Range loop for j in House'Range loop --  Init Matrix
      myStreet (i,j) := Content'Val(Test'Pos(i)*5 + House'Pos(j));
   end loop; end loop;
   Solve (myAlley, 0, 5); --  start at test 0 with 5 options
end Zebra;
```

{{out}}

```txt
ONE: DRINK=WATER PERSON=NORWEGIAN COLOR=YELLOW SMOKE=DUNHILL PET=CAT
TWO: DRINK=TEA PERSON=DANISH COLOR=BLUE SMOKE=BLEND PET=HORSE
THREE: DRINK=MILK PERSON=ENGLISH COLOR=RED SMOKE=PALLMALL PET=BIRD
FOUR: DRINK=COFFEE PERSON=GERMAN COLOR=GREEN SMOKE=PRINCE PET=ZEBRA
FIVE: DRINK=BEER PERSON=SWEDISH COLOR=WHITE SMOKE=BLUEMASTER PET=DOG
```



## ALGOL 68

Attempts to find solutions using the rules.

```algol68
BEGIN
    # attempt to solve Einstein's Riddle - the Zebra puzzle                     #
    INT unknown   = 0, same    = -1;
    INT english   = 1, swede   = 2, dane  = 3, norwegian   = 4, german = 5;
    INT dog       = 1, birds   = 2, cats  = 3, horse       = 4, zebra  = 5;
    INT red       = 1, green   = 2, white = 3, yellow      = 4, blue   = 5;
    INT tea       = 1, coffee  = 2, milk  = 3, beer        = 4, water  = 5;
    INT pall mall = 1, dunhill = 2, blend = 3, blue master = 4, prince = 5;
    []STRING nationality = ( "unknown", "english",   "swede",   "dane",   "norwegian",   "german" );
    []STRING animal      = ( "unknown", "dog",       "birds",   "cats",   "horse",       "ZEBRA"  );
    []STRING colour      = ( "unknown", "red",       "green",   "white",  "yellow",      "blue"   );
    []STRING drink       = ( "unknown", "tea",       "coffee",  "milk",   "beer",        "water"  );
    []STRING smoke       = ( "unknown", "pall mall", "dunhill", "blend",  "blue master", "prince" );
    MODE HOUSE = STRUCT( INT nationality, animal, colour, drink, smoke );
    # returns TRUE if a field in a house could be set to value, FALSE otherwise #
    PROC can set = ( INT field, INT value )BOOL: field = unknown OR value = same;
    # returns TRUE if the fields of house h could be set to those of            #
    #              suggestion s, FALSE otherwise                                #
    OP   XOR     = ( HOUSE h, HOUSE s )BOOL:
         (   can set( nationality OF h, nationality OF s ) AND can set( animal OF h, animal OF s )
         AND can set( colour      OF h, colour      OF s ) AND can set( drink  OF h, drink  OF s )
         AND can set( smoke       OF h, smoke       OF s )
         ) # XOR # ;
    # sets a field in a house to value if it is unknown                         #
    PROC set     = ( REF INT field, INT value )VOID: IF field = unknown AND value /= same THEN field := value FI;
    # sets the unknown fields in house h to the non-same fields of suggestion s #
    OP   +:=     = ( REF HOUSE h, HOUSE s )VOID:
         ( set( nationality OF h, nationality OF s ); set( animal OF h, animal OF s )
         ; set( colour      OF h, colour      OF s ); set( drink  OF h, drink  OF s )
         ; set( smoke       OF h, smoke       OF s )
         ) # +:= # ;
    # sets a field in a house to unknown if the value is not same               #
    PROC reset   = ( REF INT field, INT value )VOID: IF value /= same THEN field := unknown FI;
    # sets fields in house h to unknown if the suggestion s is not same         #
    OP   -:=     = ( REF HOUSE h, HOUSE s )VOID:
         ( reset( nationality OF h, nationality OF s ); reset( animal OF h, animal OF s )
         ; reset( colour      OF h, colour      OF s ); reset( drink  OF h, drink  OF s )
         ; reset( smoke       OF h, smoke       OF s )
         ) # -:= # ;
    # attempts a partial solution for the house at pos                          #
    PROC try = ( INT pos, HOUSE suggestion, PROC VOID continue )VOID:
         IF pos >= LWB house AND pos <= UPB house THEN
             IF house[ pos ] XOR suggestion THEN
                house[ pos ] +:= suggestion; continue; house[ pos ] -:= suggestion
             FI
         FI # try # ;
    # attempts a partial solution for the neighbours of a house                 #
    PROC left or right = ( INT pos, BOOL left, BOOL right, HOUSE neighbour suggestion
                         , PROC VOID continue )VOID:
         ( IF left  THEN try( pos - 1, neighbour suggestion, continue ) FI
         ; IF right THEN try( pos + 1, neighbour suggestion, continue ) FI
         ) # left or right # ;
    # attempts a partial solution for all houses and possibly their neighbours  #
    PROC any2 = ( REF INT number, HOUSE suggestion
                , BOOL left, BOOL right, HOUSE neighbour suggestion
                , PROC VOID continue )VOID:
         FOR pos TO UPB house DO
             IF house[ pos ] XOR suggestion THEN
                 number    := pos;
                 house[ number ] +:= suggestion;
                 IF NOT left AND NOT right THEN # neighbours not involved       #
                     continue
                 ELSE                           # try one or both neighbours    #
                     left or right( pos, left, right, neighbour suggestion, continue )
                 FI;
                 house[ number ] -:= suggestion
             FI
         OD # any2 # ;
    # attempts a partial solution for all houses                                #
    PROC any = ( HOUSE suggestion, PROC VOID continue )VOID:
         any2( LOC INT, suggestion, FALSE, FALSE, SKIP, continue );
    # find solution(s)                                                          #
    INT blend pos;
    INT solutions := 0;
    # There are five houses.                                                    #
    [ 1 : 5 ]HOUSE house;
    FOR h TO UPB house DO house[ h ] := ( unknown, unknown, unknown, unknown, unknown ) OD;
    # In the middle house they drink milk.                                      #
    drink       OF house[ 3 ] := milk;
    # The Norwegian lives in the first house.                                   #
    nationality OF house[ 1 ] := norwegian;
    # The Norwegian lives next to the blue house.                               #
    colour      OF house[ 2 ] := blue;
    # They drink coffee in the green house.                                     #
    # The green house is immediately to the left of the white house.            #
    any2( LOC INT,     ( same, same, green, coffee, same )
       ,  FALSE, TRUE, ( same, same, white, same,   same ), VOID:
      # In a house next to the house where they have a horse,                   #
      # they smoke Dunhill.                                                     #
      # In the yellow house they smoke Dunhill.                                 #
      any2( LOC INT,    ( same, horse, same,   same, same    )
          , TRUE, TRUE, ( same, same,  yellow, same, dunhill ), VOID:
        # The English man lives in the red house.                               #
        any( ( english, same, red, same, same ), VOID:
          # The man who smokes Blend lives in the house next to the             #
          # house with cats.                                                    #
          any2( blend pos,  ( same, same, same, same, blend )
              , TRUE, TRUE, ( same, cats, same, same, same  ), VOID:
            # They drink water in a house next to the house where               #
            # they smoke Blend.                                                 #
            left or right( blend pos, TRUE, TRUE, ( same, same, same, water, same ), VOID:
              # The Dane drinks tea.                                            #
              any( ( dane, same, same, tea, same ), VOID:
                # The man who smokes Blue Master drinks beer.                   #
                any( ( same, same, same, beer, blue master ), VOID:
                  # The Swede has a dog.                                        #
                  any( ( swede, dog, same, same, same ), VOID:
                    # The German smokes Prince.                                 #
                    any( ( german, same, same, same, prince ), VOID:
                      # The man who smokes Pall Mall has birds.                 #
                      any( ( same, birds, same, same, pall mall ), VOID:
                        # if we can place the zebra, we have a solution         #
                        any( ( same, zebra, same, same, same ), VOID:
                             ( solutions +:= 1;
                               FOR h TO UPB house DO
                                 print( ( whole( h, 0 )
                                        , " ",  nationality[ 1 + nationality OF house[ h ] ]
                                        , ", ", animal     [ 1 + animal      OF house[ h ] ]
                                        , ", ", colour     [ 1 + colour      OF house[ h ] ]
                                        , ", ", drink      [ 1 + drink       OF house[ h ] ]
                                        , ", ", smoke      [ 1 + smoke       OF house[ h ] ]
                                        , newline
                                        )
                                      )
                               OD;
                               print( ( newline ) )
                             )
                           ) # zebra     #
                         ) # pall mall  #
                       ) # german      #
                     ) # swede        #
                   ) # beer          #
                 ) # dane           #
               ) # blend L/R       #
             ) # blend            #
           ) # red               #
         ) # horse              #
       ) # green               # ;
    print( ( "solutions: ", whole( solutions, 0 ), newline ) )
END

```

{{out}}

```txt

1 norwegian, cats, yellow, water, dunhill
2 dane, horse, blue, tea, blend
3 english, birds, red, milk, pall mall
4 german, ZEBRA, green, coffee, prince
5 swede, dog, white, beer, blue master

solutions: 1

```



## AutoHotkey

See [[Dinesman's multiple-dwelling problem/AutoHotkey]].


## BBC BASIC

{{works with|BBC BASIC for Windows}}

```bbcbasic
      REM The names (only used for printing the results):
      DIM Drink$(4), Nation$(4), Colr$(4), Smoke$(4), Animal$(4)
      Drink$()  = "Beer", "Coffee", "Milk", "Tea", "Water"
      Nation$() = "Denmark", "England", "Germany", "Norway", "Sweden"
      Colr$()   = "Blue", "Green", "Red", "White", "Yellow"
      Smoke$()  = "Blend", "BlueMaster", "Dunhill", "PallMall", "Prince"
      Animal$() = "Birds", "Cats", "Dog", "Horse", "Zebra"

      REM Some single-character tags:
      a$ = "A" : b$ = "B" : c$ = "C" : d$ = "D" : e$ = "E"

      REM BBC BASIC Doesn't have enumerations!
      Beer$=a$    : Coffee$=b$     : Milk$=c$    : Tea$=d$      : Water$=e$
      Denmark$=a$ : England$=b$    : Germany$=c$ : Norway$=d$   : Sweden$=e$
      Blue$=a$    : Green$=b$      : Red$=c$     : White$=d$    : Yellow$=e$
      Blend$=a$   : BlueMaster$=b$ : Dunhill$=c$ : PallMall$=d$ : Prince$=e$
      Birds$=a$   : Cats$=b$       : Dog$=c$     : Horse$=d$    : Zebra$=e$

      REM Create the 120 permutations of 5 objects:
      DIM perm$(120), x$(4) : x$() = a$, b$, c$, d$, e$
      REPEAT
        p% += 1
        perm$(p%) = x$(0)+x$(1)+x$(2)+x$(3)+x$(4)
      UNTIL NOT FNperm(x$())

      REM Express the statements as conditional expressions:
      ex2$ = "INSTR(Nation$,England$) = INSTR(Colr$,Red$)"
      ex3$ = "INSTR(Nation$,Sweden$) = INSTR(Animal$,Dog$)"
      ex4$ = "INSTR(Nation$,Denmark$) = INSTR(Drink$,Tea$)"
      ex5$ = "INSTR(Colr$,Green$+White$) <> 0"
      ex6$ = "INSTR(Drink$,Coffee$) = INSTR(Colr$,Green$)"
      ex7$ = "INSTR(Smoke$,PallMall$) = INSTR(Animal$,Birds$)"
      ex8$ = "INSTR(Smoke$,Dunhill$) = INSTR(Colr$,Yellow$)"
      ex9$ = "MID$(Drink$,3,1) = Milk$"
      ex10$ = "LEFT$(Nation$,1) = Norway$"
      ex11$ = "ABS(INSTR(Smoke$,Blend$)-INSTR(Animal$,Cats$)) = 1"
      ex12$ = "ABS(INSTR(Smoke$,Dunhill$)-INSTR(Animal$,Horse$)) = 1"
      ex13$ = "INSTR(Smoke$,BlueMaster$) = INSTR(Drink$,Beer$)"
      ex14$ = "INSTR(Nation$,Germany$) = INSTR(Smoke$,Prince$)"
      ex15$ = "ABS(INSTR(Nation$,Norway$)-INSTR(Colr$,Blue$)) = 1"
      ex16$ = "ABS(INSTR(Smoke$,Blend$)-INSTR(Drink$,Water$)) = 1"

      REM Solve:
      solutions% = 0
      TIME = 0
      FOR nation% = 1 TO 120
        Nation$ = perm$(nation%)
        IF EVAL(ex10$) THEN
          FOR colr% = 1 TO 120
            Colr$ = perm$(colr%)
            IF EVAL(ex5$) IF EVAL(ex2$) IF EVAL(ex15$) THEN
              FOR drink% = 1 TO 120
                Drink$ = perm$(drink%)
                IF EVAL(ex9$) IF EVAL(ex4$) IF EVAL(ex6$) THEN
                  FOR smoke% = 1 TO 120
                    Smoke$ = perm$(smoke%)
                    IF EVAL(ex14$) IF EVAL(ex13$) IF EVAL(ex16$) IF EVAL(ex8$) THEN
                      FOR animal% = 1 TO 120
                        Animal$ = perm$(animal%)
                        IF EVAL(ex3$) IF EVAL(ex7$) IF EVAL(ex11$) IF EVAL(ex12$) THEN
                          PRINT "House     Drink     Nation    Colour    Smoke     Animal"
                          FOR house% = 1 TO 5
                            PRINT ; house% ,;
                            PRINT Drink$(ASCMID$(Drink$,house%)-65),;
                            PRINT Nation$(ASCMID$(Nation$,house%)-65),;
                            PRINT Colr$(ASCMID$(Colr$,house%)-65),;
                            PRINT Smoke$(ASCMID$(Smoke$,house%)-65),;
                            PRINT Animal$(ASCMID$(Animal$,house%)-65)
                          NEXT
                          solutions% += 1
                        ENDIF
                      NEXT animal%
                    ENDIF
                  NEXT smoke%
                ENDIF
              NEXT drink%
            ENDIF
          NEXT colr%
        ENDIF
      NEXT nation%
      PRINT '"Number of solutions = "; solutions%
      PRINT "Solved in " ; TIME/100 " seconds"
      END

      DEF FNperm(x$())
      LOCAL i%, j%
      FOR i% = DIM(x$(),1)-1 TO 0 STEP -1
        IF x$(i%) < x$(i%+1) EXIT FOR
      NEXT
      IF i% < 0 THEN = FALSE
      j% = DIM(x$(),1)
      WHILE x$(j%) <= x$(i%) j% -= 1 : ENDWHILE
      SWAP x$(i%), x$(j%)
      i% += 1
      j% = DIM(x$(),1)
      WHILE i% < j%
        SWAP x$(i%), x$(j%)
        i% += 1
        j% -= 1
      ENDWHILE
      = TRUE
```

'''Output:'''

```txt

House     Drink     Nation    Colour    Smoke     Animal
1         Water     Norway    Yellow    Dunhill   Cats
2         Tea       Denmark   Blue      Blend     Horse
3         Milk      England   Red       PallMall  Birds
4         Coffee    Germany   Green     Prince    Zebra
5         Beer      Sweden    White     BlueMasterDog

Number of solutions = 1
Solved in 0.12 seconds

```



## Bracmat


```bracmat
(     (English Swede Dane Norwegian German,)
      (red green white yellow blue,(red.English.))
      (dog birds cats horse zebra,(dog.?.Swede.))
      ( tea coffee milk beer water
      , (tea.?.?.Dane.) (coffee.?.green.?.)
      )
      ( "Pall Mall" Dunhill Blend "Blue Master" Prince
      ,   ("Blue Master".beer.?.?.?.)
          ("Pall Mall".?.birds.?.?.)
          (Dunhill.?.?.yellow.?.)
          (Prince.?.?.?.German.)
      )
      ( 1 2 3 4 5
      , (3.?.milk.?.?.?.) (1.?.?.?.?.Norwegian.)
      )
  : ?properties
& ( relations
  =   next leftOf
    .   ( next
        =   a b A B
          .   !arg:(?S,?A,?B)
            & !S:? (?a.!A) ?:? (?b.!B) ?
            & (!a+1:!b|!b+1:!a)
        )
      & ( leftOf
        =   a b A B
          .   !arg:(?S,?A,?B)
            & !S:? (?a.!A) ?:? (?b.!B) ?
            & !a+1:!b
        )
      &   leftOf
        $ (!arg,(?.?.?.green.?.),(?.?.?.white.?.))
      & next$(!arg,(Blend.?.?.?.?.),(?.?.cats.?.?.))
      &   next
        $ (!arg,(?.?.horse.?.?.),(Dunhill.?.?.?.?.))
      &   next
        $ (!arg,(?.?.?.?.Norwegian.),(?.?.?.blue.?.))
      & next$(!arg,(?.water.?.?.?.),(Blend.?.?.?.?.))
  )
& ( props
  =     a constraint constraints house houses
      , remainingToDo shavedToDo toDo value values z
    .   !arg:(?toDo.?shavedToDo.?house.?houses)
      & (   !toDo:(?values,?constraints) ?remainingToDo
          &   !values
            : (   ?a
                  ( %@?value
                  &   !constraints
                    : (   ?
                          ( !value
                          .   ?constraint
                            & !house:!constraint
                          )
                          ?
                      | ~(   ?
                             ( ?
                             .   ?constraint
                               & !house:!constraint
                             )
                             ?
                         | ? (!value.?) ?
                         )
                      )
                  )
                  ( ?z
                  &   props
                    $ ( !remainingToDo
                      . !shavedToDo (!a !z,!constraints)
                      . (!value.!house)
                      . !houses
                      )
                  )
              |
                & relations$!houses
                & out$(Solution !houses)
              )
        |   !toDo:
          & props$(!shavedToDo...!house !houses)
        )
  )
& props$(!properties...)
& done
);
```

Output:

```txt
  Solution
  (4.Prince.coffee.zebra.green.German.)
  (1.Dunhill.water.cats.yellow.Norwegian.)
  (2.Blend.tea.horse.blue.Dane.)
  (5.Blue Master.beer.dog.white.Swede.)
  (3.Pall Mall.milk.birds.red.English.)
{!} done
```



## C


```c
#include <stdio.h>
#include <string.h>

enum HouseStatus { Invalid, Underfull, Valid };

enum Attrib { C, M, D, A, S };

// Unfilled attributes are represented by -1
enum Colors { Red, Green, White, Yellow, Blue };
enum Mans { English, Swede, Dane, German, Norwegian };
enum Drinks { Tea, Coffee, Milk, Beer, Water };
enum Animals { Dog, Birds, Cats, Horse, Zebra };
enum Smokes { PallMall, Dunhill, Blend, BlueMaster, Prince };


void printHouses(int ha[5][5]) {
    const char *color[] =  { "Red", "Green", "White", "Yellow", "Blue" };
    const char *man[] =    { "English", "Swede", "Dane", "German", "Norwegian" };
    const char *drink[] =  { "Tea", "Coffee", "Milk", "Beer", "Water" };
    const char *animal[] = { "Dog", "Birds", "Cats", "Horse", "Zebra" };
    const char *smoke[] =  { "PallMall", "Dunhill", "Blend", "BlueMaster", "Prince" };

    printf("%-10.10s%-10.10s%-10.10s%-10.10s%-10.10s%-10.10s\n",
           "House", "Color", "Man", "Drink", "Animal", "Smoke");

    for (int i = 0; i < 5; i++) {
        printf("%-10d", i);
        if (ha[i][C] >= 0)
            printf("%-10.10s", color[ha[i][C]]);
        else
            printf("%-10.10s", "-");
        if (ha[i][M] >= 0)
            printf("%-10.10s", man[ha[i][M]]);
        else
            printf("%-10.10s", "-");
        if (ha[i][D] >= 0)
            printf("%-10.10s", drink[ha[i][D]]);
        else
            printf("%-10.10s", "-");
        if (ha[i][A] >= 0)
            printf("%-10.10s", animal[ha[i][A]]);
        else
            printf("%-10.10s", "-");
        if (ha[i][S] >= 0)
            printf("%-10.10s\n", smoke[ha[i][S]]);
        else
            printf("-\n");
    }
}


int checkHouses(int ha[5][5]) {
    int c_add = 0, c_or = 0;
    int m_add = 0, m_or = 0;
    int d_add = 0, d_or = 0;
    int a_add = 0, a_or = 0;
    int s_add = 0, s_or = 0;

    // Cond 9: In the middle house they drink milk.
    if (ha[2][D] >= 0 && ha[2][D] != Milk)
        return Invalid;

    // Cond 10: The Norwegian lives in the first house.
    if (ha[0][M] >= 0 && ha[0][M] != Norwegian)
        return Invalid;

    for (int i = 0; i < 5; i++) {
        // Uniqueness tests.
        if (ha[i][C] >= 0) {
            c_add += (1 << ha[i][C]);
            c_or |= (1 << ha[i][C]);
        }
        if (ha[i][M] >= 0) {
            m_add += (1 << ha[i][M]);
            m_or |= (1 << ha[i][M]);
        }
        if (ha[i][D] >= 0) {
            d_add += (1 << ha[i][D]);
            d_or |= (1 << ha[i][D]);
        }
        if (ha[i][A] >= 0) {
            a_add += (1 << ha[i][A]);
            a_or |= (1 << ha[i][A]);
        }
        if (ha[i][S] >= 0) {
            s_add += (1 << ha[i][S]);
            s_or |= (1 << ha[i][S]);
        }

        // Cond 2: The English man lives in the red house.
        if ((ha[i][M] >= 0 && ha[i][C] >= 0) &&
            ((ha[i][M] == English && ha[i][C] != Red) || // Checking both
             (ha[i][M] != English && ha[i][C] == Red)))  // to make things quicker.
            return Invalid;

        // Cond 3: The Swede has a dog.
        if ((ha[i][M] >= 0 && ha[i][A] >= 0) &&
            ((ha[i][M] == Swede && ha[i][A] != Dog) ||
             (ha[i][M] != Swede && ha[i][A] == Dog)))
            return Invalid;

        // Cond 4: The Dane drinks tea.
        if ((ha[i][M] >= 0 && ha[i][D] >= 0) &&
            ((ha[i][M] == Dane && ha[i][D] != Tea) ||
             (ha[i][M] != Dane && ha[i][D] == Tea)))
            return Invalid;

        // Cond 5: The green house is immediately to the left of the white house.
        if ((i > 0 && ha[i][C] >= 0 /*&& ha[i-1][C] >= 0 */ ) &&
            ((ha[i - 1][C] == Green && ha[i][C] != White) ||
             (ha[i - 1][C] != Green && ha[i][C] == White)))
            return Invalid;

        // Cond 6: drink coffee in the green house.
        if ((ha[i][C] >= 0 && ha[i][D] >= 0) &&
            ((ha[i][C] == Green && ha[i][D] != Coffee) ||
             (ha[i][C] != Green && ha[i][D] == Coffee)))
            return Invalid;

        // Cond 7: The man who smokes Pall Mall has birds.
        if ((ha[i][S] >= 0 && ha[i][A] >= 0) &&
            ((ha[i][S] == PallMall && ha[i][A] != Birds) ||
             (ha[i][S] != PallMall && ha[i][A] == Birds)))
            return Invalid;

        // Cond 8: In the yellow house they smoke Dunhill.
        if ((ha[i][S] >= 0 && ha[i][C] >= 0) &&
            ((ha[i][S] == Dunhill && ha[i][C] != Yellow) ||
             (ha[i][S] != Dunhill && ha[i][C] == Yellow)))
            return Invalid;

        // Cond 11: The man who smokes Blend lives in the house next to the house with cats.
        if (ha[i][S] == Blend) {
            if (i == 0 && ha[i + 1][A] >= 0 && ha[i + 1][A] != Cats)
                return Invalid;
            else if (i == 4 && ha[i - 1][A] != Cats)
                return Invalid;
            else if (ha[i + 1][A] >= 0 && ha[i + 1][A] != Cats && ha[i - 1][A] != Cats)
                return Invalid;
        }

        // Cond 12: In a house next to the house where they have a horse, they smoke Dunhill.
        if (ha[i][S] == Dunhill) {
            if (i == 0 && ha[i + 1][A] >= 0 && ha[i + 1][A] != Horse)
                return Invalid;
            else if (i == 4 && ha[i - 1][A] != Horse)
                return Invalid;
            else if (ha[i + 1][A] >= 0 && ha[i + 1][A] != Horse && ha[i - 1][A] != Horse)
                return Invalid;
        }

        // Cond 13: The man who smokes Blue Master drinks beer.
        if ((ha[i][S] >= 0 && ha[i][D] >= 0) &&
            ((ha[i][S] == BlueMaster && ha[i][D] != Beer) ||
             (ha[i][S] != BlueMaster && ha[i][D] == Beer)))
            return Invalid;

        // Cond 14: The German smokes Prince
        if ((ha[i][M] >= 0 && ha[i][S] >= 0) &&
            ((ha[i][M] == German && ha[i][S] != Prince) ||
             (ha[i][M] != German && ha[i][S] == Prince)))
            return Invalid;

        // Cond 15: The Norwegian lives next to the blue house.
        if (ha[i][M] == Norwegian &&
            ((i < 4 && ha[i + 1][C] >= 0 && ha[i + 1][C] != Blue) ||
             (i > 0 && ha[i - 1][C] != Blue)))
            return Invalid;

        // Cond 16: They drink water in a house next to the house where they smoke Blend.
        if (ha[i][S] == Blend) {
            if (i == 0 && ha[i + 1][D] >= 0 && ha[i + 1][D] != Water)
                return Invalid;
            else if (i == 4 && ha[i - 1][D] != Water)
                return Invalid;
            else if (ha[i + 1][D] >= 0 && ha[i + 1][D] != Water && ha[i - 1][D] != Water)
                return Invalid;
        }

    }

    if ((c_add != c_or) || (m_add != m_or) || (d_add != d_or)
        || (a_add != a_or) || (s_add != s_or)) {
        return Invalid;
    }

    if ((c_add != 0b11111) || (m_add != 0b11111) || (d_add != 0b11111)
        || (a_add != 0b11111) || (s_add != 0b11111)) {
        return Underfull;
    }

    return Valid;
}


int bruteFill(int ha[5][5], int hno, int attr) {
    int stat = checkHouses(ha);
    if ((stat == Valid) || (stat == Invalid))
        return stat;

    int hb[5][5];
    memcpy(hb, ha, sizeof(int) * 5 * 5);
    for (int i = 0; i < 5; i++) {
        hb[hno][attr] = i;
        stat = checkHouses(hb);
        if (stat != Invalid) {
            int nexthno, nextattr;
            if (attr < 4) {
                nextattr = attr + 1;
                nexthno = hno;
            } else {
                nextattr = 0;
                nexthno = hno + 1;
            }

            stat = bruteFill(hb, nexthno, nextattr);
            if (stat != Invalid) {
                memcpy(ha, hb, sizeof(int) * 5 * 5);
                return stat;
            }
        }
    }

    // We only come here if none of the attr values assigned were valid.
    return Invalid;
}


int main() {
    int ha[5][5] = {{-1, -1, -1, -1, -1}, {-1, -1, -1, -1, -1},
                    {-1, -1, -1, -1, -1}, {-1, -1, -1, -1, -1},
                    {-1, -1, -1, -1, -1}};

    bruteFill(ha, 0, 0);
    printHouses(ha);

    return 0;
}
```

{{out}}

```txt
% gcc -Wall -O3 -std=c99 zebra.c -o zebra && time ./zebra
House     Color     Man       Drink     Animal    Smoke
0         Yellow    Norwegian Water     Cats      Dunhill
1         Blue      Dane      Tea       Horse     Blend
2         Red       English   Milk      Birds     PallMall
3         Green     German    Coffee    Zebra     Prince
4         White     Swede     Beer      Dog       BlueMaster
./zebra  0.00s user 0.00s system 0% cpu 0.002 total
```

The execution time is too small to be reliably measured on my machine.

### C Generated from Perl


I'll be the first to admit the following doesn't quite look like a C program.  It's in fact in Perl, which outputs a C source, which in turn solves the puzzle.  If you think this is long, wait till you see the C it writes.

```perl
#!/usr/bin/perl

use utf8;
no strict;

my (%props, %name, @pre, @conds, @works, $find_all_solutions);

sub do_consts {
	local $";
	for my $p (keys %props) {
		my @s = @{ $props{$p} };

		$" = ", ";
		print "enum { ${p}_none = 0, @s };\n";

		$" = '", "';
		print "const char *string_$p [] = { \"###\", \"@s\" };\n\n";
	}
	print "#define FIND_BY(p)	\\
int find_by_##p(int v) {		\\
int i;					\\
for (i = 0; i < N_ITEMS; i++)		\\
	if (house[i].p == v) return i;	\\
return -1; }\n";

	print "FIND_BY($_)" for (keys %props);

	local $" = ", ";
	my @k = keys %props;

	my $sl = 0;
	for (keys %name) {
		if (length > $sl) { $sl = length }
	}

	my $fmt = ("%".($sl + 1)."s ") x @k;
	my @arg = map { "string_$_"."[house[i].$_]" } @k;
	print << "SNIPPET";
int work0(void) {
	int i;
	for (i = 0; i < N_ITEMS; i++)
		printf("%d $fmt\\n", i, @arg);
	puts(\"\");
	return 1;
}
SNIPPET

}

sub setprops {
	%props = @_;
	my $l = 0;
	my @k = keys %props;
	for my $p (@k) {
		my @s = @{ $props{$p} };

		if ($l && $l != @s) {
			die "bad length @s";
		}
		$l = @s;
		$name{$_} = $p for @s;
	}
	local $" = ", ";
	print "#include <stdio.h>
#define N_ITEMS $l
struct item_t { int @k; } house[N_ITEMS] = {{0}};\n";
}

sub pair {NB.   h =.~.&> compose&.>~/y,<h

	my ($c1, $c2, $diff) = @_;
	$diff //= [0];
	$diff = [$diff] unless ref $diff;

	push @conds, [$c1, $c2, $diff];
}

sub make_conditions {
	my $idx = 0;
	my $return1 = $find_all_solutions ? "" : "return 1";
	print "
#define TRY(a, b, c, d, p, n)		\\
if ((b = a d) >= 0 && b < N_ITEMS) {	\\
	if (!house[b].p) {		\\
		house[b].p = c;		\\
		if (n()) $return1;	\\
		house[b].p = 0;		\\
	}}
";

	while (@conds) {
		my ($c1, $c2, $diff) = @{ pop @conds };
		my $p2 = $name{$c2} or die "bad prop $c2";

		if ($c1 =~ /^\d+$/) {
			push @pre, "house[$c1].$p2 = $c2;";
			next;
		}

		my $p1 = $name{$c1} or die "bad prop $c1";
		my $next = "work$idx";
		my $this = "work".++$idx;

		print "
/* condition pair($c1, $c2, [@$diff]) */
int $this(void) {
int a = find_by_$p1($c1);
int b = find_by_$p2($c2);
if (a != -1 && b != -1) {
switch(b - a) {
";
		print "case $_: " for @$diff;
		print "return $next(); default: return 0; }\n } if (a != -1) {";
		print "TRY(a, b, $c2, +($_), $p2, $next);" for @$diff;
		print " return 0; } if (b != -1) {";
		print "TRY(b, a, $c1, -($_), $p1, $next);" for @$diff;
		print "
return 0; }
/* neither condition is set; try all possibles */
for (a = 0; a < N_ITEMS; a++) {
if (house[a].$p1) continue;
house[a].$p1 = $c1;
";

		print "TRY(a, b, $c2, +($_), $p2, $next);" for @$diff;
		print " house[a].$p1 = 0; } return 0; }";
	}

	print "int main() { @pre return !work$idx(); }";
}

sub make_c {
	do_consts;
	make_conditions;
}

# ---- above should be generic for all similar puzzles ---- #

# ---- below: per puzzle setup ---- #
# property names and values
setprops (
	'nationality'	# Svensk n. a Swede, not a swede (kålrot).
			# AEnglisk (from middle Viking "Æŋløsåksen") n. a Brit.
		=> [ qw(Deutsch Svensk Norske Danske AEnglisk) ],
	'pet'	=> [ qw(birds dog horse zebra cats) ],
	'drink'	=> [ qw(water tea milk beer coffee) ],
	'smoke'	=> [ qw(dunhill blue_master prince blend pall_mall) ],
	'color'	=> [ qw(red green yellow white blue) ]
);

# constraints
pair(AEnglisk, red);
pair(Svensk, dog);
pair(Danske, tea);
pair(green, white, 1);	# "to the left of" can mean either 1 or -1: ambiguous
pair(coffee, green);
pair(pall_mall, birds);
pair(yellow, dunhill);
pair(2, milk);
pair(0, Norske);
pair(blend, cats, [-1, 1]);
pair(horse, dunhill, [-1, 1]);
pair(blue_master, beer);	# Nicht das Deutsche Bier trinken? Huh.
pair(Deutsch, prince);
pair(Norske, blue, [-1, 1]);
pair(water, blend, [-1, 1]);

# "zebra lives *somewhere* relative to the Brit".  It has no effect on
# the logic.  It's here just to make sure the code will insert a zebra
# somewhere in the table (after all other conditions are met) so the
# final print-out shows it. (the C code can be better structured, but
# meh, I ain't reading it, so who cares).
pair(zebra, AEnglisk, [ -4 .. 4 ]);

# write C code.  If it's ugly to you: I didn't write; Perl did.
make_c;
```

output (ran as <code>perl test.pl | gcc -Wall -x c -; ./a.out</code>):
```txt

0      dunhill         cats       yellow        water       Norske
1        blend        horse         blue          tea       Danske
2    pall_mall        birds          red         milk     AEnglisk
3       prince        zebra        green       coffee      Deutsch
4  blue_master          dog        white         beer       Svensk

```



## C++


This is a modification of the C submission that uses rule classes and reduces the number of permutations evaluated.


```cpp

#include <stdio.h>
#include <string.h>

#define defenum(name, val0, val1, val2, val3, val4) \
    enum name { val0, val1, val2, val3, val4 }; \
    const char *name ## _str[] = { # val0, # val1, # val2, # val3, # val4 }

defenum( Attrib,    Color, Man, Drink, Animal, Smoke );
defenum( Colors,    Red, Green, White, Yellow, Blue );
defenum( Mans,      English, Swede, Dane, German, Norwegian );
defenum( Drinks,    Tea, Coffee, Milk, Beer, Water );
defenum( Animals,   Dog, Birds, Cats, Horse, Zebra );
defenum( Smokes,    PallMall, Dunhill, Blend, BlueMaster, Prince );

void printHouses(int ha[5][5]) {
    const char **attr_names[5] = {Colors_str, Mans_str, Drinks_str, Animals_str, Smokes_str};

    printf("%-10s", "House");
    for (const char *name : Attrib_str) printf("%-10s", name);
    printf("\n");

    for (int i = 0; i < 5; i++) {
        printf("%-10d", i);
        for (int j = 0; j < 5; j++) printf("%-10s", attr_names[j][ha[i][j]]);
        printf("\n");
    }
}

struct HouseNoRule {
    int houseno;
    Attrib a; int v;
} housenos[] = {
    {2, Drink, Milk},     // Cond 9: In the middle house they drink milk.
    {0, Man, Norwegian}   // Cond 10: The Norwegian lives in the first house.
};

struct AttrPairRule {
    Attrib a1; int v1;
    Attrib a2; int v2;

    bool invalid(int ha[5][5], int i) {
        return (ha[i][a1] >= 0 && ha[i][a2] >= 0) &&
               ((ha[i][a1] == v1 && ha[i][a2] != v2) ||
                (ha[i][a1] != v1 && ha[i][a2] == v2));
    }
} pairs[] = {
    {Man, English,      Color, Red},     // Cond 2: The English man lives in the red house.
    {Man, Swede,        Animal, Dog},    // Cond 3: The Swede has a dog.
    {Man, Dane,         Drink, Tea},     // Cond 4: The Dane drinks tea.
    {Color, Green,      Drink, Coffee},  // Cond 6: drink coffee in the green house.
    {Smoke, PallMall,   Animal, Birds},  // Cond 7: The man who smokes Pall Mall has birds.
    {Smoke, Dunhill,    Color, Yellow},  // Cond 8: In the yellow house they smoke Dunhill.
    {Smoke, BlueMaster, Drink, Beer},    // Cond 13: The man who smokes Blue Master drinks beer.
    {Man, German,       Smoke, Prince}    // Cond 14: The German smokes Prince
};

struct NextToRule {
    Attrib a1; int v1;
    Attrib a2; int v2;

    bool invalid(int ha[5][5], int i) {
        return (ha[i][a1] == v1) &&
               ((i == 0 && ha[i + 1][a2] >= 0 && ha[i + 1][a2] != v2) ||
                (i == 4 && ha[i - 1][a2] != v2) ||
                (ha[i + 1][a2] >= 0 && ha[i + 1][a2] != v2 && ha[i - 1][a2] != v2));
    }
} nexttos[] = {
    {Smoke, Blend,      Animal, Cats},    // Cond 11: The man who smokes Blend lives in the house next to the house with cats.
    {Smoke, Dunhill,    Animal, Horse},   // Cond 12: In a house next to the house where they have a horse, they smoke Dunhill.
    {Man, Norwegian,    Color, Blue},     // Cond 15: The Norwegian lives next to the blue house.
    {Smoke, Blend,      Drink, Water}     // Cond 16: They drink water in a house next to the house where they smoke Blend.
};

struct LeftOfRule {
    Attrib a1; int v1;
    Attrib a2; int v2;

    bool invalid(int ha[5][5]) {
        return (ha[0][a2] == v2) || (ha[4][a1] == v1);
    }

    bool invalid(int ha[5][5], int i) {
        return ((i > 0 && ha[i][a1] >= 0) &&
                ((ha[i - 1][a1] == v1 && ha[i][a2] != v2) ||
                 (ha[i - 1][a1] != v1 && ha[i][a2] == v2)));
    }
} leftofs[] = {
    {Color, Green,  Color, White}     // Cond 5: The green house is immediately to the left of the white house.
};

bool invalid(int ha[5][5]) {
    for (auto &rule : leftofs) if (rule.invalid(ha)) return true;

    for (int i = 0; i < 5; i++) {
#define eval_rules(rules) for (auto &rule : rules) if (rule.invalid(ha, i)) return true;
        eval_rules(pairs);
        eval_rules(nexttos);
        eval_rules(leftofs);
    }
    return false;
}

void search(bool used[5][5], int ha[5][5], const int hno, const int attr) {
    int nexthno, nextattr;
    if (attr < 4) {
        nextattr = attr + 1;
        nexthno = hno;
    } else {
        nextattr = 0;
        nexthno = hno + 1;
    }

    if (ha[hno][attr] != -1) {
        search(used, ha, nexthno, nextattr);
    } else {
        for (int i = 0; i < 5; i++) {
            if (used[attr][i]) continue;
            used[attr][i] = true;
            ha[hno][attr] = i;

            if (!invalid(ha)) {
                if ((hno == 4) && (attr == 4)) {
                    printHouses(ha);
                } else {
                    search(used, ha, nexthno, nextattr);
                }
            }

            used[attr][i] = false;
        }
        ha[hno][attr] = -1;
    }
}

int main() {
    bool used[5][5] = {};
    int ha[5][5]; memset(ha, -1, sizeof(ha));

    for (auto &rule : housenos) {
        ha[rule.houseno][rule.a] = rule.v;
        used[rule.a][rule.v] = true;
    }

    search(used, ha, 0, 0);

    return 0;
}

```

{{out}}

```txt

$ g++ -O3 -std=c++11 zebra.cpp -o zebracpp && time ./zebracpp
House     Color     Man       Drink     Animal    Smoke
0         Yellow    Norwegian Water     Cats      Dunhill
1         Blue      Dane      Tea       Horse     Blend
2         Red       English   Milk      Birds     PallMall
3         Green     German    Coffee    Zebra     Prince
4         White     Swede     Beer      Dog       BlueMaster

real	0m0.003s
user	0m0.000s
sys	0m0.000s

```


My measured time is slower than that posted for the original C code, but on my machine this C++ code is faster than the original C code.

=={{header|C sharp|C#}}==
=== "Manual" solution (Norvig-style) ===
{{works with|C sharp|C#|7+ (but easy to adapt to lower versions)}}
<!-- By Martin Freedman, 17/01/2018 -->
This is adapted from a solution to a similar problem by Peter Norvig in his [https://www.udacity.com/course/design-of-computer-programs--cs212 Udacity course CS212], originally written in Python. This is translated from [https://github.com/exercism/python/blob/master/exercises/zebra-puzzle/example.py example python solution on exercism]. This is a Generate-and-Prune Constraint Programming algorithm written with Linq. (See Benchmarks below)

```csharp
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using static System.Console;

public enum Colour { Red, Green, White, Yellow, Blue }
public enum Nationality { Englishman, Swede, Dane, Norwegian,German }
public enum Pet { Dog, Birds, Cats, Horse, Zebra }
public enum Drink { Coffee, Tea, Milk, Beer, Water }
public enum Smoke { PallMall, Dunhill, Blend, BlueMaster, Prince}

public static class ZebraPuzzle
{
    private static (Colour[] colours, Drink[] drinks, Smoke[] smokes, Pet[] pets, Nationality[] nations) _solved;

    static ZebraPuzzle()
    {
        var solve = from colours in Permute<Colour>()  //r1 5 range
                    where (colours,Colour.White).IsRightOf(colours, Colour.Green) // r5
                    from nations in Permute<Nationality>()
                    where nations[0] == Nationality.Norwegian // r10
                    where (nations, Nationality.Englishman).IsSameIndex(colours, Colour.Red) //r2
                    where (nations,Nationality.Norwegian).IsNextTo(colours,Colour.Blue) // r15
                    from drinks in Permute<Drink>()
                    where drinks[2] == Drink.Milk //r9
                    where (drinks, Drink.Coffee).IsSameIndex(colours, Colour.Green) // r6
                    where (drinks, Drink.Tea).IsSameIndex(nations, Nationality.Dane) //r4
                    from pets in Permute<Pet>()
                    where (pets, Pet.Dog).IsSameIndex(nations, Nationality.Swede) // r3
                    from smokes in Permute<Smoke>()
                    where (smokes, Smoke.PallMall).IsSameIndex(pets, Pet.Birds) // r7
                    where (smokes, Smoke.Dunhill).IsSameIndex(colours, Colour.Yellow) // r8
                    where (smokes, Smoke.Blend).IsNextTo(pets, Pet.Cats) // r11
                    where (smokes, Smoke.Dunhill).IsNextTo(pets, Pet.Horse) //r12
                    where (smokes, Smoke.BlueMaster).IsSameIndex(drinks, Drink.Beer) //r13
                    where (smokes, Smoke.Prince).IsSameIndex(nations, Nationality.German) // r14
                    where (drinks,Drink.Water).IsNextTo(smokes,Smoke.Blend) // r16
                    select (colours, drinks, smokes, pets, nations);

        _solved = solve.First();
    }

    private static int IndexOf<T>(this T[] arr, T obj) => Array.IndexOf(arr, obj);

    private static bool IsRightOf<T, U>(this (T[] a, T v) right, U[] a, U v) => right.a.IndexOf(right.v) == a.IndexOf(v) + 1;

    private static bool IsSameIndex<T, U>(this (T[] a, T v)x, U[] a, U v) => x.a.IndexOf(x.v) == a.IndexOf(v);

    private static bool IsNextTo<T, U>(this (T[] a, T v)x, U[] a,  U v) => (x.a,x.v).IsRightOf(a, v) || (a,v).IsRightOf(x.a,x.v);

    // made more generic from https://codereview.stackexchange.com/questions/91808/permutations-in-c
    public static IEnumerable<IEnumerable<T>> Permutations<T>(this IEnumerable<T> values)
    {
        if (values.Count() == 1)
            return values.ToSingleton();

        return values.SelectMany(v => Permutations(values.Except(v.ToSingleton())),(v, p) => p.Prepend(v));
    }

    public static IEnumerable<T[]> Permute<T>() => ToEnumerable<T>().Permutations().Select(p=>p.ToArray());

    private static IEnumerable<T> ToSingleton<T>(this T item){ yield return item; }

    private static IEnumerable<T> ToEnumerable<T>() => Enum.GetValues(typeof(T)).Cast<T>();

    public static new String ToString()
    {
        var sb = new StringBuilder();
        sb.AppendLine("House Colour Drink    Nationality Smokes     Pet");
        sb.AppendLine("───── ────── ──────── ─────────── ────────── ─────");
        var (colours, drinks, smokes, pets, nations) = _solved;
        for (var i = 0; i < 5; i++)
            sb.AppendLine($"{i+1,5} {colours[i],-6} {drinks[i],-8} {nations[i],-11} {smokes[i],-10} {pets[i],-10}");
        return sb.ToString();
    }

    public static void Main(string[] arguments)
    {
        var owner = _solved.nations[_solved.pets.IndexOf(Pet.Zebra)];
        WriteLine($"The zebra owner is {owner}");
        Write(ToString());
        Read();
    }
}
```

Produces:

```txt

The zebra owner is German
House Colour Drink    Nationality Smokes     Pet
───── ────── ──────── ─────────── ────────── ─────
    1 Yellow Water    Norwegian   Dunhill    Cats
    2 Blue   Tea      Dane        Blend      Horse
    3 Red    Milk     Englishman  PallMall   Birds
    4 Green  Coffee   German      Prince     Zebra
    5 White  Beer     Swede       BlueMaster Dog

```

=== "Manual" solution (Combining Houses) ===
{{works with|C sharp|C#|7+ }}
{{trans|Scala}}
<!-- By Martin Freedman, 19/01/2018 -->
This is similar to the Scala solution although there are differences in how the rules are calculated and it keeps all the original constraints/rules rather than does any simplification of them.

This is a different type of generate-and-prune compared to Norvig.  The Norvig solution generates each attribute for 5 houses, then prunes and repeats with the next attribute. Here all houses with possible attributes are first generated and pruned to 78 candidates. The second phase proceeds over the combination of 5 houses from that 78, generating and pruning 1 house at a time. (See Benchmarks below)

```csharp
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

using static System.Console;

namespace ZebraPuzzleSolver
{
    public enum Colour { Red, Green, White, Yellow, Blue }
    public enum Nationality { Englishman, Swede, Dane, Norwegian, German }
    public enum Pet { Dog, Birds, Cats, Horse, Zebra }
    public enum Drink { Coffee, Tea, Milk, Beer, Water }
    public enum Smoke { PallMall, Dunhill, Blend, BlueMaster, Prince }

    public struct House
    {
        public Drink D { get; }
        public Colour C { get; }
        public Pet P { get; }
        public Nationality N { get; }
        public Smoke S { get; }

        House(Drink d, Colour c, Pet p, Nationality n, Smoke s) => (D, C, P, N, S) = (d, c, p, n, s);

        public static House Create(Drink d, Colour c, Pet p, Nationality n, Smoke s) => new House(d, c, p, n, s);

        public bool AllUnequal(House other) => D != other.D && C != other.C && P != other.P && N != other.N && S != other.S;

        public override string ToString() =>$"{C,-6} {D,-8} {N,-11} {S,-10} {P,-10}";
    }

    public static class LinqNoPerm
    {
        public static IEnumerable<T> ToEnumerable<T>() => Enum.GetValues(typeof(T)).Cast<T>();

        public static IEnumerable<House> FreeCandidates(this IEnumerable<House> houses, IEnumerable<House> picked) =>
            houses.Where(house => picked.All(house.AllUnequal));

       static Dictionary<Type, Func<House, dynamic, bool>> _eFn = new Dictionary<Type, Func<House, dynamic, bool>>
            { {typeof(Drink),(h,e)=>h.D==e},
              {typeof(Nationality),(h,e)=>h.N==e},
              {typeof(Colour),(h,e)=>h.C==e},
              {typeof(Pet),(h,e)=>h.P==e},
              {typeof(Smoke),(h, e)=>h.S==e}
            };

        public static bool IsNextTo<T, U>(this IEnumerable<House> hs,T t, U u) => hs.IsLeftOf(t,u) || hs.IsLeftOf(u, t);

        public static bool IsLeftOf<T, U>(this IEnumerable<House> hs, T left, U right) =>
            hs.Zip(hs.Skip(1), (l, r) => (_eFn[left.GetType()](l, left) && _eFn[right.GetType()](r, right))).Any(l => l);

        static House[] _solved;

        static LinqNoPerm()
        {
            var candidates =
                from colours in ToEnumerable<Colour>()
                from nations in ToEnumerable<Nationality>()
                from drinks in ToEnumerable<Drink>()
                from pets in ToEnumerable<Pet>()
                from smokes in ToEnumerable<Smoke>()
                where (colours == Colour.Red) == (nations == Nationality.Englishman) //r2
                where (nations == Nationality.Swede) == (pets == Pet.Dog) //r3
                where (nations == Nationality.Dane) == (drinks == Drink.Tea) //r4
                where (colours == Colour.Green) == (drinks == Drink.Coffee) //r6
                where (smokes == Smoke.PallMall) == (pets == Pet.Birds) //r7
                where (smokes == Smoke.Dunhill) == (colours == Colour.Yellow) // r8
                where (smokes == Smoke.BlueMaster) == (drinks == Drink.Beer) //r13
                where (smokes == Smoke.Prince) == (nations == Nationality.German) // r14
                select House.Create(drinks,colours,pets,nations, smokes);
            var members =
                from h1 in candidates
                where h1.N == Nationality.Norwegian //r10
                from h3 in candidates.FreeCandidates(new[] { h1 })
                where h3.D == Drink.Milk //r9
                from h2 in candidates.FreeCandidates(new[] { h1, h3 })
                let h123 = new[] { h1, h2, h3 }
                where h123.IsNextTo(Nationality.Norwegian, Colour.Blue) //r15
                where h123.IsNextTo(Smoke.Blend, Pet.Cats)//r11
                where h123.IsNextTo(Smoke.Dunhill, Pet.Horse) //r12
                from h4 in candidates.FreeCandidates(h123)
                from h5 in candidates.FreeCandidates(new[] { h1, h3, h2, h4 })
                let houses = new[] { h1, h2, h3, h4, h5 }
                where houses.IsLeftOf(Colour.Green, Colour.White) //r5
                select houses;
            _solved = members.First();
        }

        public static new String ToString()
        {
            var sb = new StringBuilder();

            sb.AppendLine("House Colour Drink    Nationality Smokes     Pet");
            sb.AppendLine("───── ────── ──────── ─────────── ────────── ─────");
            for (var i = 0; i < 5; i++)
                sb.AppendLine($"{i + 1,5} {_solved[i].ToString()}");
            return sb.ToString();
        }

        public static void Main(string[] arguments)
        {
            var owner = _solved.Where(h=>h.P==Pet.Zebra).Single().N;
            WriteLine($"The zebra owner is {owner}");
            Write(ToString());
            Read();
        }
    }
}
```

Produces

```txt
The zebra owner is German
House Colour Drink    Nationality Smokes     Pet
───── ────── ──────── ─────────── ────────── ─────
    1 Yellow Water    Norwegian   Dunhill    Cats
    2 Blue   Tea      Dane        Blend      Horse
    3 Red    Milk     Englishman  PallMall   Birds
    4 Green  Coffee   German      Prince     Zebra
    5 White  Beer     Swede       BlueMaster Dog

```


=== "Amb" solution ===
This uses the second version of the [https://rosettacode.org/wiki/Amb#C.23 Amb C# class] in the Amb challenge
{{works with|C sharp|C#|7.1}}
<!-- By Martin Freedman, 9/02/2018 -->

```csharp
using Amb;
using System;
using System.Collections.Generic;
using System.Linq;
using static System.Console;

static class ZebraProgram
{
    public static void Main()
    {
        var amb = new Amb.Amb();

        var domain = new[] { 1, 2, 3, 4, 5 };
        var terms = new Dictionary<IValue<int>, string>();
        IValue<int> Term(string name)
        {
            var x = amb.Choose(domain);
            terms.Add(x, name);
            return x;
        };

        void IsUnequal(params IValue<int>[] values) =>amb.Require(() => values.Select(v => v.Value).Distinct().Count() == 5);
        void IsSame(IValue<int> left, IValue<int> right) => amb.Require(() => left.Value == right.Value);
        void IsLeftOf(IValue<int> left, IValue<int> right) => amb.Require(() => right.Value - left.Value == 1);
        void IsIn(IValue<int> attrib, int house) => amb.Require(() => attrib.Value == house);
        void IsNextTo(IValue<int> left, IValue<int> right) => amb.Require(() => Math.Abs(left.Value - right.Value) == 1);

        IValue<int> english = Term("Englishman"), swede = Term("Swede"), dane = Term("Dane"), norwegian = Term("Norwegian"), german = Term("German");
        IsIn(norwegian, 1);
        IsUnequal(english, swede, german, dane, norwegian);

        IValue<int> red = Term("red"), green = Term("green"), white = Term("white"), blue = Term("blue"), yellow = Term("yellow");
        IsUnequal(red, green, white, blue, yellow);
        IsNextTo(norwegian, blue);
        IsLeftOf(green, white);
        IsSame(english, red);

        IValue<int> tea = Term("tea"), coffee = Term("coffee"), milk = Term("milk"), beer = Term("beer"), water = Term("water");
        IsIn(milk, 3);
        IsUnequal(tea, coffee, milk, beer, water);
        IsSame(dane, tea);
        IsSame(green, coffee);

        IValue<int> dog = Term("dog"), birds = Term("birds"), cats = Term("cats"), horse = Term("horse"), zebra = Term("zebra");
        IsUnequal(dog, cats, birds, horse, zebra);
        IsSame(swede, dog);

        IValue<int> pallmall = Term("pallmall"), dunhill = Term("dunhill"), blend = Term("blend"), bluemaster = Term("bluemaster"),prince = Term("prince");
        IsUnequal(pallmall, dunhill, bluemaster, prince, blend);
        IsSame(pallmall, birds);
        IsSame(dunhill, yellow);
        IsNextTo(blend, cats);
        IsNextTo(horse, dunhill);
        IsSame(bluemaster, beer);
        IsSame(german, prince);
        IsNextTo(water, blend);

        if (!amb.Disambiguate())
        {
            WriteLine("No solution found.");
            Read();
            return;
        }

        var h = new List<string>[5];
        for (int i = 0; i < 5; i++)
            h[i] = new List<string>();

        foreach (var (key, value) in terms.Select(kvp => (kvp.Key, kvp.Value)))
        {
            h[key.Value - 1].Add(value);
        }

        var owner = String.Concat(h.Where(l => l.Contains("zebra")).Select(l => l[0]));
        WriteLine($"The {owner} owns the zebra");

        foreach (var house in h)
        {
            Write("|");
            foreach (var attrib in house)
                Write($"{attrib,-10}|");
            Write("\n");
        }
        Read();
    }
}
```

Produces

```txt
The zebra owner is German
House Colour Drink    Nationality Smokes     Pet
───── ────── ──────── ─────────── ────────── ─────
    1 Yellow Water    Norwegian   Dunhill    Cats
    2 Blue   Tea      Dane        Blend      Horse
    3 Red    Milk     Englishman  PallMall   Birds
    4 Green  Coffee   German      Prince     Zebra
    5 White  Beer     Swede       BlueMaster Dog

```


=== "Automatic" solution ===
{{works with|C sharp|C#|7}}
{{libheader|Microsoft Solver Foundation}}
<!-- By Martin Freedman, 19/01/2018 based on MS example code-->

```csharp
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Microsoft.SolverFoundation.Solvers;

using static System.Console;

static class ZebraProgram
{
    static ConstraintSystem _solver;

    static CspTerm IsLeftOf(this CspTerm left, CspTerm right) => _solver.Equal(1, right - left);
    static CspTerm IsInSameHouseAs(this CspTerm left, CspTerm right) => _solver.Equal(left, right);
    static CspTerm IsNextTo(this CspTerm left, CspTerm right) => _solver.Equal(1,_solver.Abs(left-right));
    static CspTerm IsInHouse(this CspTerm @this, int i) => _solver.Equal(i, @this);

    static (ConstraintSystem, Dictionary<CspTerm, string>) BuildSolver()
    {
        var solver = ConstraintSystem.CreateSolver();
        _solver = solver;
        var terms = new Dictionary<CspTerm, string>();

        CspTerm Term(string name)
        {
            CspTerm x = solver.CreateVariable(solver.CreateIntegerInterval(1, 5), name);
            terms.Add(x, name);
            return x;
        };

        CspTerm red = Term("red"), green = Term("green"), white = Term("white"), blue = Term("blue"), yellow = Term("yellow");
        CspTerm tea = Term("tea"), coffee = Term("coffee"), milk = Term("milk"), beer = Term("beer"), water = Term("water");
        CspTerm english = Term("Englishman"), swede = Term("Swede"), dane = Term("Dane"), norwegian = Term("Norwegian"),
            german = Term("German");
        CspTerm dog = Term("dog"), birds = Term("birds"), cats = Term("cats"), horse = Term("horse"), zebra = Term("zebra");
        CspTerm pallmall = Term("pallmall"), dunhill = Term("dunhill"), blend = Term("blend"), bluemaster = Term("bluemaster"),
            prince = Term("prince");

        solver.AddConstraints(
            solver.Unequal(english, swede, german, dane, norwegian),
            solver.Unequal(red, green, white, blue, yellow),
            solver.Unequal(dog, cats, birds, horse, zebra),
            solver.Unequal(pallmall, dunhill, bluemaster, prince, blend),
            solver.Unequal(tea, coffee, milk, beer, water),

            english.IsInSameHouseAs(red), //r2
            swede.IsInSameHouseAs(dog), //r3
            dane.IsInSameHouseAs(tea), //r4
            green.IsLeftOf(white), //r5
            green.IsInSameHouseAs(coffee), //r6
            pallmall.IsInSameHouseAs(birds), //r7
            dunhill.IsInSameHouseAs(yellow), //r8
            milk.IsInHouse(3), //r9
            norwegian.IsInHouse(1), //r10
            blend.IsNextTo(cats), //r11
            horse.IsNextTo(dunhill),// r12
            bluemaster.IsInSameHouseAs(beer), // r13
            german.IsInSameHouseAs(prince), // r14
            norwegian.IsNextTo(blue), //r15
            water.IsNextTo(blend) //r16
        );
        return (solver, terms);
    }

    static List<string>[] TermsToString(ConstraintSolverSolution solved, Dictionary<CspTerm, string> terms)
    {
        var h = new List<string>[5];
        for (int i = 0; i < 5; i++)
            h[i] = new List<string>();

        foreach (var (key, value) in terms.Select(kvp => (kvp.Key, kvp.Value)))
        {
            if (!solved.TryGetValue(key, out object house))
                throw new InvalidProgramException("Can't find a term - {value} - in the solution");
            h[(int)house - 1].Add(value);
        }

        return h;
    }

    static new string ToString(List<string>[] houses)
    {
        var sb = new StringBuilder();
        foreach (var house in houses)
        {
            sb.Append("|");
            foreach (var attrib in house)
                sb.Append($"{attrib,-10}|");
            sb.Append("\n");
        }
        return sb.ToString();
    }

    public static void Main()
    {
        var (solver, terms) = BuildSolver();

        var solved = solver.Solve();

        if (solved.HasFoundSolution)
        {
            var h = TermsToString(solved, terms);

            var owner = String.Concat(h.Where(l => l.Contains("zebra")).Select(l => l[2]));
            WriteLine($"The {owner} owns the zebra");
            WriteLine();
            Write(ToString(h));
        }
        else
            WriteLine("No solution found.");
        Read();
    }
}
```

Produces:

```txt

The German owns the zebra

|yellow    |water     |Norwegian |cats      |dunhill   |
|blue      |tea       |Dane      |horse     |blend     |
|red       |milk      |Englishman|birds     |pallmall  |
|green     |coffee    |German    |zebra     |prince    |
|white     |beer      |Swede     |dog       |bluemaster|

```


### Benchmarking the 3 solutions:


```txt

BenchmarkDotNet=v0.10.12, OS=Windows 10 Redstone 3 [1709, Fall Creators Update] (10.0.16299.192)
Intel Core i7-7500U CPU 2.70GHz (Kaby Lake), 1 CPU, 4 logical cores and 2 physical cores
Frequency=2835943 Hz, Resolution=352.6164 ns, Timer=TSC
  DefaultJob : .NET Framework 4.6.1 (CLR 4.0.30319.42000), 64bit RyuJIT-v4.7.2600.0

Method	Mean	Error	StdDev
Norvig	65.32 ms	1.241 ms	1.328 ms
Combin	93.62 ms	1.792 ms	1.918 ms
Solver  148.7 us	2.962 us	6.248 us

```

I think that it is Enums (not the use of ''dynamic'' in a dictionary, which is only called 8 times in Combine), and Linq query comprehensions (versus for loops) slow down the 2 non_solver solutions. A non-type-safe non-Enum int version of Combine (not posted here) runs at ~21ms, which is a nearly 5x speed up for that algo. (Not tried with Norvig). Regardless, learning and using the Solver class (and that solution already uses ints rather than enums) provides a dramatic x 100 + performance increase compared to the best manual solutions.


## Clojure

This solution uses the contributed package ''clojure.core.logic'' (with ''clojure.tools.macro''), a mini-Kanren based logic solver. The solution is basically the one in [http://github.com/swannodette/logic-tutorial Swannodette's logic tutorial], adapted to the problem statement here.

```clojure
(ns zebra.core
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic]
        [clojure.tools.macro :as macro]))

(defne lefto [x y l]
       ([_ _ [x y . ?r]])
       ([_ _ [_ . ?r]] (lefto x y ?r)))

(defn nexto [x y l]
  (conde
    ((lefto x y l))
    ((lefto y x l))))

(defn zebrao [hs]
  (macro/symbol-macrolet [_ (lvar)]
    (all
      (== [_ _ _ _ _] hs)
      (membero ['englishman _ _ _ 'red] hs)
      (membero ['swede _ _ 'dog _] hs)
      (membero ['dane _ 'tea _ _] hs)
      (lefto [_ _ _ _ 'green] [_ _ _ _ 'white] hs)
      (membero [_ _ 'coffee _ 'green] hs)
      (membero [_ 'pallmall _ 'birds _] hs)
      (membero [_ 'dunhill _ _ 'yellow] hs)
      (== [_ _ [_ _ 'milk _ _] _ _ ] hs)
      (firsto hs ['norwegian _ _ _ _])
      (nexto [_ 'blend _ _ _] [_ _ _ 'cats _ ] hs)
      (nexto [_ _ _ 'horse _] [_ 'dunhill _ _ _] hs)
      (membero [_ 'bluemaster 'beer _ _] hs)
      (membero ['german 'prince _ _ _] hs)
      (nexto ['norwegian _ _ _ _] [_ _ _ _ 'blue] hs)
      (nexto [_ _ 'water _ _] [_ 'blend _ _ _] hs)
      (membero [_ _ _ 'zebra _] hs))))

(let [solns (run* [q] (zebrao q))
      soln (first solns)
      zebra-owner (->> soln (filter #(= 'zebra (% 3))) first (#(% 0)))]
  (println "solution count:" (count solns))
  (println "zebra owner is the" zebra-owner)
  (println "full solution (in house order):")
  (doseq [h soln] (println " " h)))

```

{{output}}

```txt
solution count: 1
zebra owner is the german
full solution (in house order):
  [norwegian dunhill water cats yellow]
  [dane blend tea horse blue]
  [englishman pallmall milk birds red]
  [german prince coffee zebra green]
  [swede bluemaster beer dog white]

```



=== Alternate solution (Norvig-style) ===
{{libheader|clojure.math.combinatorics}}

This is adapted from a solution to a similar problem by Peter Norvig in his [https://www.udacity.com/course/design-of-computer-programs--cs212 Udacity course CS212], originally written in Python but equally applicable in any language with for-comprehensions.


```clojure
(ns zebra
  (:require [clojure.math.combinatorics :as c]))

(defn solve []
  (let [arrangements (c/permutations (range 5))
        before? #(= (inc %1) %2)
        after? #(= (dec %1) %2)
        next-to? #(or (before? %1 %2) (after? %1 %2))]
    (for [[english swede dane norwegian german :as persons] arrangements
          :when (zero? norwegian)
          [red green white yellow blue :as colors] arrangements
          :when (before? green white)
          :when (= english red)
          :when (after? blue norwegian)
          [tea coffee milk beer water :as drinks] arrangements
          :when (= 2 milk)
          :when (= dane tea)
          :when (= coffee green)
          [pall-mall dunhill blend blue-master prince :as cigs] arrangements
          :when (= german prince)
          :when (= yellow dunhill)
          :when (= blue-master beer)
          :when (after? blend water)
          [dog birds cats horse zebra :as pets] arrangements
          :when (= swede dog)
          :when (= pall-mall birds)
          :when (next-to? blend cats)
          :when (after? horse dunhill)]
      (->> [[:english :swede :dane :norwegian :german]
            [:red :green :white :yellow :blue]
            [:tea :coffee :milk :beer :water]
            [:pall-mall :dunhill :blend :blue-master :prince]
            [:dog :birds :cats :horse :zebra]]
           (map zipmap [persons colors drinks cigs pets])))))


(defn -main [& _]
  (doseq [[[persons _ _ _ pets :as solution] i]
          (map vector (solve) (iterate inc 1))
          :let [zebra-house (some #(when (= :zebra (val %)) (key %)) pets)]]
    (println "solution" i)
    (println "The" (persons zebra-house) "owns the zebra.")
    (println "house nationality color   drink   cig          pet")
    (println "----- ----------- ------- ------- ------------ ------")
    (dotimes [i 5]
      (println (apply format "%5s %-11s %-7s %-7s %-12s %-6s"
                      (map #(% i) (cons inc solution)))))))

```

{{output}}

```txt
user=> (time (zebra/-main))
solution 1
The :german owns the zebra.
house nationality color   drink   cig          pet
----- ----------- ------- ------- ------------ ------
    1 :norwegian  :yellow :water  :dunhill     :cats
    2 :dane       :blue   :tea    :blend       :horse
    3 :english    :red    :milk   :pall-mall   :birds
    4 :german     :green  :coffee :prince      :zebra
    5 :swede      :white  :beer   :blue-master :dog
"Elapsed time: 10.555482 msecs"
nil

```



## Crystal

{{trans|Ruby}}

```ruby
CONTENT = {House:       [""],
           Nationality: %i[English Swedish Danish Norwegian German],
           Colour:      %i[Red Green White Blue Yellow],
           Pet:         %i[Dog Birds Cats Horse Zebra],
           Drink:       %i[Tea Coffee Milk Beer Water],
           Smoke:       %i[PallMall Dunhill BlueMaster Prince Blend]}

def adjacent?(n, i, g, e)
  (0..3).any? { |x| (n[x] == i && g[x + 1] == e) || (n[x + 1] == i && g[x] == e) }
end

def leftof?(n, i, g, e)
  (0..3).any? { |x| n[x] == i && g[x + 1] == e }
end

def coincident?(n, i, g, e)
  n.each_index.any? { |x| n[x] == i && g[x] == e }
end

def solve_zebra_puzzle
  CONTENT[:Nationality].each_permutation { |nation|
    next unless nation.first == :Norwegian # 10
    CONTENT[:Colour].each_permutation { |colour|
      next unless leftof?(colour, :Green, colour, :White)      # 5
      next unless coincident?(nation, :English, colour, :Red)  # 2
      next unless adjacent?(nation, :Norwegian, colour, :Blue) # 15
      CONTENT[:Pet].each_permutation { |pet|
        next unless coincident?(nation, :Swedish, pet, :Dog) # 3
        CONTENT[:Drink].each_permutation { |drink|
          next unless drink[2] == :Milk                           # 9
          next unless coincident?(nation, :Danish, drink, :Tea)   # 4
          next unless coincident?(colour, :Green, drink, :Coffee) # 6
          CONTENT[:Smoke].each_permutation { |smoke|
            next unless coincident?(smoke, :PallMall, pet, :Birds)    # 7
            next unless coincident?(smoke, :Dunhill, colour, :Yellow) # 8
            next unless coincident?(smoke, :BlueMaster, drink, :Beer) # 13
            next unless coincident?(smoke, :Prince, nation, :German)  # 14
            next unless adjacent?(smoke, :Blend, pet, :Cats)          # 11
            next unless adjacent?(smoke, :Blend, drink, :Water)       # 16
            next unless adjacent?(smoke, :Dunhill, pet, :Horse)       # 12
            print_out(nation, colour, pet, drink, smoke)
          }
        }
      }
    }
  }
end

def print_out(nation, colour, pet, drink, smoke)
  width = CONTENT.map { |k, v| {k.to_s.size, v.max_of { |y| y.to_s.size }}.max }
  fmt = width.map { |w| "%-#{w}s" }.join(" ")
  national = nation[pet.index(:Zebra).not_nil!]
  puts "The Zebra is owned by the man who is #{national}", ""
  puts fmt % CONTENT.keys, fmt % width.map { |w| "-" * w }
  [nation, colour, pet, drink, smoke].transpose.each.with_index(1) { |x, n| puts fmt % ([n] + x) }
end

solve_zebra_puzzle
```



## Curry

{{Works with|PAKCS}}

```curry
import Constraint (allC, anyC)
import Findall (findall)


data House  =  H Color Man Pet Drink Smoke

data Color  =  Red    | Green | Blue  | Yellow | White
data Man    =  Eng    | Swe   | Dan   | Nor    | Ger
data Pet    =  Dog    | Birds | Cats  | Horse  | Zebra
data Drink  =  Coffee | Tea   | Milk  | Beer   | Water
data Smoke  =  PM     | DH    | Blend | BM     | Prince


houses :: [House] -> Success
houses hs@[H1,_,H3,_,_] =                         --  1
    H  _ _ _ Milk _  =:=  H3                      --  9
  & H  _ Nor _ _ _   =:=  H1                      -- 10
  & allC (`member` hs)
  [ H  Red Eng _ _ _                              --  2
  , H  _ Swe Dog _ _                              --  3
  , H  _ Dan _ Tea _                              --  4
  , H  Green _ _ Coffee _                         --  6
  , H  _ _ Birds _ PM                             --  7
  , H  Yellow _ _ _ DH                            --  8
  , H  _ _ _ Beer BM                              -- 13
  , H  _ Ger _ _ Prince                           -- 14
  ]
  & H  Green _ _ _ _  `leftTo`  H  White _ _ _ _  --  5
  & H  _ _ _ _ Blend  `nextTo`  H  _ _ Cats _ _   -- 11
  & H  _ _ Horse _ _  `nextTo`  H  _ _ _ _ DH     -- 12
  & H  _ Nor _ _ _    `nextTo`  H  Blue _ _ _ _   -- 15
  & H  _ _ _ Water _  `nextTo`  H  _ _ _ _ Blend  -- 16
 where
    x `leftTo` y = _ ++ [x,y] ++ _ =:= hs
    x `nextTo` y = x `leftTo` y
                 ? y `leftTo` x


member :: a -> [a] -> Success
member = anyC . (=:=)


main = findall $ \(hs,who) -> houses hs & H _ who Zebra _ _ `member` hs
```

{{Output}} Using [http://www-ps.informatik.uni-kiel.de/~pakcs/webpakcs/main.cgi web interface].

```txt
Execution time: 180 msec. / elapsed: 180 msec.
[([H Yellow Nor Cats Water DH,H Blue Dan Horse Tea Blend,H Red Eng Birds Milk PM,H Green Ger Zebra Coffee Prince,H White Swe Dog Beer BM],Ger)]
```



## D

{{trans|Ada}}
Most foreach loops in this program are static.

```d
import std.stdio, std.traits, std.algorithm, std.math;

enum Content { Beer, Coffee, Milk, Tea, Water,
               Danish, English, German, Norwegian, Swedish,
               Blue, Green, Red, White, Yellow,
               Blend, BlueMaster, Dunhill, PallMall, Prince,
               Bird, Cat, Dog, Horse, Zebra }
enum Test { Drink, Person, Color, Smoke, Pet }
enum House { One, Two, Three, Four, Five }

alias TM = Content[EnumMembers!Test.length][EnumMembers!House.length];

bool finalChecks(in ref TM M) pure nothrow @safe @nogc {
  int diff(in Content a, in Content b, in Test ca, in Test cb)
  nothrow @safe @nogc {
    foreach (immutable h1; EnumMembers!House)
      foreach (immutable h2; EnumMembers!House)
        if (M[ca][h1] == a && M[cb][h2] == b)
          return h1 - h2;
    assert(0); // Useless but required.
  }

  with (Content) with (Test)
    return abs(diff(Norwegian, Blue, Person, Color)) == 1 &&
           diff(Green, White, Color, Color) == -1 &&
           abs(diff(Horse, Dunhill, Pet, Smoke)) == 1 &&
           abs(diff(Water, Blend, Drink, Smoke)) == 1 &&
           abs(diff(Blend, Cat, Smoke, Pet)) == 1;
}

bool constrained(in ref TM M, in Test atest) pure nothrow @safe @nogc {
  with (Content) with (Test) with (House)
    final switch (atest) {
      case Drink:
        return M[Drink][Three] == Milk;
      case Person:
        foreach (immutable h; EnumMembers!House)
          if ((M[Person][h] == Norwegian && h != One) ||
              (M[Person][h] == Danish && M[Drink][h] != Tea))
            return false;
        return true;
      case Color:
        foreach (immutable h; EnumMembers!House)
          if ((M[Person][h] == English && M[Color][h] != Red) ||
              (M[Drink][h] == Coffee && M[Color][h] != Green))
            return false;
        return true;
      case Smoke:
        foreach (immutable h; EnumMembers!House)
          if ((M[Color][h] == Yellow && M[Smoke][h] != Dunhill) ||
              (M[Smoke][h] == BlueMaster && M[Drink][h] != Beer) ||
              (M[Person][h] == German && M[Smoke][h] != Prince))
            return false;
        return true;
      case Pet:
        foreach (immutable h; EnumMembers!House)
          if ((M[Person][h] == Swedish && M[Pet][h] != Dog) ||
              (M[Smoke][h] == PallMall && M[Pet][h] != Bird))
            return false;
        return finalChecks(M);
    }
}

void show(in ref TM M) {
  foreach (h; EnumMembers!House) {
    writef("%5s: ", h);
    foreach (immutable t; EnumMembers!Test)
      writef("%10s ", M[t][h]);
    writeln;
  }
}

void solve(ref TM M, in Test t, in size_t n) {
  if (n == 1 && constrained(M, t)) {
    if (t < 4) {
      solve(M, [EnumMembers!Test][t + 1], 5);
    } else {
      show(M);
      return;
    }
  }
  foreach (immutable i; 0 .. n) {
    solve(M, t, n - 1);
    swap(M[t][n % 2 ? 0 : i], M[t][n - 1]);
  }
}

void main() {
  TM M;
  foreach (immutable t; EnumMembers!Test)
    foreach (immutable h; EnumMembers!House)
      M[t][h] = EnumMembers!Content[t * 5 + h];

  solve(M, Test.Drink, 5);
}
```

{{out}}

```txt
  One:      Water  Norwegian     Yellow    Dunhill        Cat
  Two:        Tea     Danish       Blue      Blend      Horse
Three:       Milk    English        Red   PallMall       Bird
 Four:     Coffee     German      Green     Prince      Zebra
 Five:       Beer    Swedish      White BlueMaster        Dog
```

Run-time about 0.04 seconds.


### Alternative Version

{{trans|Python}}
This requires the module of the first D entry from the Permutations Task.

```d
import std.stdio, std.math, std.traits, std.typecons, std.typetuple, permutations1;

uint factorial(in uint n) pure nothrow @nogc @safe
in {
    assert(n <= 12);
} body {
    uint result = 1;
    foreach (immutable i; 1 .. n + 1)
        result *= i;
    return result;
}

enum Number { One,      Two,     Three,  Four,       Five   }
enum Color  { Red,      Green,   Blue,   White,      Yellow }
enum Drink  { Milk,     Coffee,  Water,  Beer,       Tea    }
enum Smoke  { PallMall, Dunhill, Blend,  BlueMaster, Prince }
enum Pet    { Dog,      Cat,     Zebra,  Horse,      Bird   }
enum Nation { British,  Swedish, Danish, Norvegian,  German }

enum size_t M = EnumMembers!Number.length;

auto nullableRef(T)(ref T item) pure nothrow @nogc {
    return NullableRef!T(&item);
}

bool isPossible(NullableRef!(immutable Number[M]) number,
                NullableRef!(immutable Color[M])  color=null,
                NullableRef!(immutable Drink[M])  drink=null,
                NullableRef!(immutable Smoke[M])  smoke=null,
                NullableRef!(immutable Pet[M])    pet=null) pure nothrow @safe @nogc {
  if ((!number.isNull && number[Nation.Norvegian] != Number.One) ||
      (!color.isNull  && color[Nation.British]    != Color.Red) ||
      (!drink.isNull  && drink[Nation.Danish]     != Drink.Tea) ||
      (!smoke.isNull  && smoke[Nation.German]     != Smoke.Prince) ||
      (!pet.isNull    && pet[Nation.Swedish]      != Pet.Dog))
    return false;

  if (number.isNull || color.isNull || drink.isNull || smoke.isNull ||
      pet.isNull)
    return true;

  foreach (immutable i; 0 .. M) {
    if ((color[i]  == Color.Green      && drink[i]  != Drink.Coffee) ||
        (smoke[i]  == Smoke.PallMall   && pet[i]    != Pet.Bird) ||
        (color[i]  == Color.Yellow     && smoke[i]  != Smoke.Dunhill) ||
        (number[i] == Number.Three     && drink[i]  != Drink.Milk) ||
        (smoke[i]  == Smoke.BlueMaster && drink[i]  != Drink.Beer)||
        (color[i]  == Color.Blue       && number[i] != Number.Two))
      return false;

    foreach (immutable j; 0 .. M) {
      if (color[i] == Color.Green && color[j] == Color.White &&
          number[j] - number[i] != 1)
        return false;

      immutable diff = abs(number[i] - number[j]);
      if ((smoke[i] == Smoke.Blend && pet[j]   == Pet.Cat       && diff != 1) ||
          (pet[i]   == Pet.Horse   && smoke[j] == Smoke.Dunhill && diff != 1) ||
          (smoke[i] == Smoke.Blend && drink[j] == Drink.Water   && diff != 1))
        return false;
    }
  }

  return true;
}

alias N = nullableRef; // At module level scope to be used with UFCS.

void main() {
  enum size_t FM = M.factorial;

  static immutable Number[M][FM] numberPerms = [EnumMembers!Number].permutations;
  static immutable Color[M][FM]  colorPerms =  [EnumMembers!Color].permutations;
  static immutable Drink[M][FM]  drinkPerms =  [EnumMembers!Drink].permutations;
  static immutable Smoke[M][FM]  smokePerms =  [EnumMembers!Smoke].permutations;
  static immutable Pet[M][FM]    petPerms =    [EnumMembers!Pet].permutations;

  // You can reduce the compile-time computations using four casts like this:
  // static colorPerms = cast(immutable Color[M][FM])numberPerms;

  static immutable Nation[M] nation = [EnumMembers!Nation];

  foreach (immutable ref number; numberPerms)
    if (isPossible(number.N))
      foreach (immutable ref color; colorPerms)
        if (isPossible(number.N, color.N))
          foreach (immutable ref drink; drinkPerms)
            if (isPossible(number.N, color.N, drink.N))
              foreach (immutable ref smoke; smokePerms)
                if (isPossible(number.N, color.N, drink.N, smoke.N))
                  foreach (immutable ref pet; petPerms)
                    if (isPossible(number.N, color.N, drink.N, smoke.N, pet.N)) {
                      writeln("Found a solution:");
                      foreach (x; TypeTuple!(nation, number, color, drink, smoke, pet))
                        writefln("%6s: %12s%12s%12s%12s%12s",
                                 (Unqual!(typeof(x[0]))).stringof,
                                 x[0], x[1], x[2], x[3], x[4]);
                      writeln;
                  }
}
```

{{out}}

```txt
Found a solution:
Nation:      British     Swedish      Danish   Norvegian      German
Number:        Three        Five         Two         One        Four
 Color:          Red       White        Blue      Yellow       Green
 Drink:         Milk        Beer         Tea       Water      Coffee
 Smoke:     PallMall  BlueMaster       Blend     Dunhill      Prince
   Pet:         Bird         Dog       Horse         Cat       Zebra

```

Run-time about 0.76 seconds with the dmd compiler.


### Short Version

{{trans|PicoLisp}}
This requires the module of the second D entry from the Permutations Task.

```d
void main() {
 import std.stdio, std.algorithm, permutations2;

 enum E { Red,      Green,   Blue,   White,      Yellow,
          Milk,     Coffee,  Water,  Beer,       Tea,
          PallMall, Dunhill, Blend,  BlueMaster, Prince,
          Dog,      Cat,     Zebra,  Horse,      Birds,
          British,  Swedish, Danish, Norvegian,  German }

 enum has =    (E[] a, E x,  E[] b, E y) => a.countUntil(x) == b.countUntil(y);
 enum leftOf = (E[] a, E x,  E[] b, E y) => a.countUntil(x) == b.countUntil(y) + 1;
 enum nextTo = (E[] a, E x,  E[] b, E y) => leftOf(a, x, b, y) || leftOf(b, y, a, x);

 with (E) foreach (houses; [Red, Blue, Green, Yellow, White].permutations)
  if (leftOf(houses, White, houses, Green))
   foreach (persons; [Norvegian, British, Swedish, German, Danish].permutations)
    if (has(persons, British, houses, Red) && persons[0] == Norvegian &&
        nextTo(persons, Norvegian, houses, Blue))
     foreach (drinks; [Tea, Coffee, Milk, Beer, Water].permutations)
      if (has(drinks, Tea, persons, Danish) &&
          has(drinks, Coffee, houses, Green) && drinks[$ / 2] == Milk)
       foreach (pets; [Dog, Birds, Cat, Horse, Zebra].permutations)
        if (has(pets, Dog, persons, Swedish))
         foreach (smokes; [PallMall, Dunhill, Blend, BlueMaster, Prince].permutations)
          if (has(smokes, PallMall, pets, Birds) &&
              has(smokes, Dunhill, houses, Yellow) &&
              nextTo(smokes, Blend, pets, Cat) &&
              nextTo(smokes, Dunhill, pets, Horse) &&
              has(smokes, BlueMaster, drinks, Beer) &&
              has(smokes, Prince, persons, German) &&
              nextTo(drinks, Water, smokes, Blend))
           writefln("%(%10s\n%)\n", [houses, persons, drinks, pets, smokes]);
}
```

{{out}}

```txt
[    Yellow,       Blue,        Red,      Green,      White]
[ Norvegian,     Danish,    British,     German,    Swedish]
[     Water,        Tea,       Milk,     Coffee,       Beer]
[       Cat,      Horse,      Birds,      Zebra,        Dog]
[   Dunhill,      Blend,   PallMall,     Prince, BlueMaster]
```

The run-time is 0.03 seconds or less.


## EchoLisp

We use the '''amb''' library to solve the puzzle. The number of tries - calls to zebra-puzzle - is only 1900, before finding all solutions. Note that there are no declarations for things (cats, tea, ..) or categories (animals, drinks, ..) which are discovered when reading the constraints.

```scheme

(lib 'hash)
(lib 'amb)

;; return #f or house# for thing/category
;; houses := (0 1 2 3 4)
(define (house-get H  category thing houses)
        (for/or ((i houses)) #:continue (!equal? (hash-ref (vector-ref H i) category) thing)
        i))

 ;; return house # for thing (eg cat) in category (eq animals)
 ;; add thing if not already here
(define-syntax-rule (house-set thing category)
    	(or
    	 (house-get H 'category 'thing houses)
         (dispatch H 'category 'thing context houses )))

;; we know that thing/category is in a given house
(define-syntax-rule (house-force thing category house)
        (dispatch H 'category 'thing context houses  house))

;; return house# or fail if impossible
(define (dispatch H category thing  context houses  (forced #f))
        (define house (or forced  (amb context houses))) ;; get a house number
        (when (hash-ref (vector-ref H house) category) (amb-fail)) ;; fail if occupied
        (hash-set (vector-ref H house) category thing) ;; else remember house contents
        house)

(define (house-next h1 h2)
 	(amb-require (or (= h1 (1+ h2)) (= h1 (1- h2)))))

(define (zebra-puzzle context houses  )
    (define H (build-vector 5 make-hash)) ;; house[i] :=  hash(category) -> thing
; In the middle house they drink milk.
    (house-force milk drinks 2)
;The Norwegian lives in the first house.
    (house-force norvegian people 0)
;  The English man lives in the red house.
    (house-force red colors(house-set english people))
; The Swede has a dog.
    (house-force dog animals (house-set swede people))
;  The Dane drinks tea.
    (house-force tea drinks (house-set dane people))
;  The green house is immediately to the left of the white house.
   (amb-require (=   (house-set green colors) (1- (house-set white colors))))
;  They drink coffee in the green house.
    (house-force coffee drinks (house-set green colors))
;  The man who smokes Pall Mall has birds.
    (house-force birds  animals (house-set pallmall smoke))
;  In the yellow house they smoke Dunhill.
    (house-force dunhill smoke (house-set yellow colors))
;  The Norwegian lives next to the blue house.
    (house-next (house-set norvegian people) (house-set blue colors))
;  The man who smokes Blend lives in the house next to the house with cats.
    (house-next (house-set blend smoke) (house-set cats  animals))
; In a house next to the house where they have a horse, they smoke Dunhill.
    (house-next (house-set horse animals) (house-set dunhill smoke))
; The man who smokes Blue Master drinks beer.
    (house-force beer drinks (house-set bluemaster smoke))
; The German smokes Prince.
    (house-force prince smoke (house-set german people))
; They drink water in a house next to the house where they smoke Blend.
    (house-next (house-set water drinks) (house-set blend smoke))

;; Finally .... the zebra 🐴
    (house-set 🐴 animals)

    (for ((i houses))
    (writeln i (hash-values (vector-ref H i))))
    (writeln '----------)

    (amb-fail) ;; will ensure ALL solutions are printed
)

```

{{out}}

```scheme

(define (task)
    (amb-run zebra-puzzle  (amb-make-context) (iota 5)))

(task)
   →
0     (norvegian yellow dunhill cats water)
1     (dane tea blue blend horse)
2     (milk english red pallmall birds)
3     (green coffee german prince 🐴)
4     (swede dog white bluemaster beer)
----------
  → #f


```



## Elixir

{{trans|Ruby}}

```elixir
defmodule ZebraPuzzle do
  defp adjacent?(n,i,g,e) do
    Enum.any?(0..3, fn x ->
      (Enum.at(n,x)==i and Enum.at(g,x+1)==e) or (Enum.at(n,x+1)==i and Enum.at(g,x)==e)
    end)
  end

  defp leftof?(n,i,g,e) do
    Enum.any?(0..3, fn x -> Enum.at(n,x)==i and Enum.at(g,x+1)==e end)
  end

  defp coincident?(n,i,g,e) do
    Enum.with_index(n) |> Enum.any?(fn {x,idx} -> x==i and Enum.at(g,idx)==e end)
  end

  def solve(content) do
    colours = permutation(content[:Colour])
    pets    = permutation(content[:Pet])
    drinks  = permutation(content[:Drink])
    smokes  = permutation(content[:Smoke])
    Enum.each(permutation(content[:Nationality]), fn nation ->
      if hd(nation) == :Norwegian, do:                                      # 10
        Enum.each(colours, fn colour ->
          if leftof?(colour, :Green, colour, :White)      and               # 5
             coincident?(nation, :English, colour, :Red)  and               # 2
             adjacent?(nation, :Norwegian, colour, :Blue), do:              # 15
            Enum.each(pets, fn pet ->
              if coincident?(nation, :Swedish, pet, :Dog), do:              # 3
                Enum.each(drinks, fn drink ->
                  if Enum.at(drink,2) == :Milk                   and        # 9
                     coincident?(nation, :Danish, drink, :Tea)   and        # 4
                     coincident?(colour, :Green, drink, :Coffee), do:       # 6
                    Enum.each(smokes, fn smoke ->
                      if coincident?(smoke, :PallMall, pet, :Birds)    and  # 7
                         coincident?(smoke, :Dunhill, colour, :Yellow) and  # 8
                         coincident?(smoke, :BlueMaster, drink, :Beer) and  # 13
                         coincident?(smoke, :Prince, nation, :German)  and  # 14
                         adjacent?(smoke, :Blend, pet, :Cats)          and  # 11
                         adjacent?(smoke, :Blend, drink, :Water)       and  # 16
                         adjacent?(smoke, :Dunhill, pet, :Horse), do:       # 12
                        print_out(content, transpose([nation, colour, pet, drink, smoke]))
    end)end)end)end)end)
  end

  defp permutation([]), do: [[]]
  defp permutation(list) do
    for x <- list, y <- permutation(list -- [x]), do: [x|y]
  end

  defp transpose(lists) do
    List.zip(lists) |> Enum.map(&Tuple.to_list/1)
  end

  defp print_out(content, result) do
    width = for {k,v}<-content, do: Enum.map([k|v], &length(to_char_list &1)) |> Enum.max
    fmt = Enum.map_join(width, " ", fn w -> "~-#{w}s" end) <> "~n"
    nation = Enum.find(result, fn x -> :Zebra in x end) |> hd
    IO.puts "The Zebra is owned by the man who is #{nation}\n"
    :io.format fmt, Keyword.keys(content)
    :io.format fmt, Enum.map(width, fn w -> String.duplicate("-", w) end)
    fmt2 = String.replace(fmt, "s", "w", global: false)
    Enum.with_index(result)
    |> Enum.each(fn {x,i} -> :io.format fmt2, [i+1 | x] end)
  end
end

content = [ House:       '',
            Nationality: ~w[English Swedish Danish Norwegian German]a,
            Colour:      ~w[Red Green White Blue Yellow]a,
            Pet:         ~w[Dog Birds Cats Horse Zebra]a,
            Drink:       ~w[Tea Coffee Milk Beer Water]a,
            Smoke:       ~w[PallMall Dunhill BlueMaster Prince Blend]a ]

ZebraPuzzle.solve(content)
```


{{out}}

```txt

The Zebra is owned by the man who is German

House Nationality Colour Pet   Drink  Smoke
----- ----------- ------ ----- ------ ----------
1     Norwegian   Yellow Cats  Water  Dunhill
2     Danish      Blue   Horse Tea    Blend
3     English     Red    Birds Milk   PallMall
4     German      Green  Zebra Coffee Prince
5     Swedish     White  Dog   Beer   BlueMaster

```



## Erlang

This solution generates all houses that fits the rules for single houses, then it checks multi-house rules. It would be faster to check multi-house rules while generating the houses. I have not added this complexity since the current program takes just a few seconds.

```Erlang

-module( zebra_puzzle ).

-export( [task/0] ).

-record( house, {colour, drink, nationality, number, pet, smoke} ).
-record( sorted_houses, {house_1s=[], house_2s=[], house_3s=[], house_4s=[], house_5s=[]} ).

task() ->
	Houses = [#house{colour=C, drink=D, nationality=N, number=Nr, pet=P, smoke=S} || C <- all_colours(), D <- all_drinks(), N <- all_nationalities(), Nr <- all_numbers(), P <- all_pets(), S <- all_smokes(), is_all_single_house_rules_ok(C, D, N, Nr, P, S)],
	Sorted_houses = lists:foldl( fun house_number_sort/2, #sorted_houses{}, Houses ),
	Streets = [[H1, H2, H3, H4, H5] || H1 <- Sorted_houses#sorted_houses.house_1s, H2 <- Sorted_houses#sorted_houses.house_2s, H3 <- Sorted_houses#sorted_houses.house_3s, H4 <- Sorted_houses#sorted_houses.house_4s, H5 <- Sorted_houses#sorted_houses.house_5s, is_all_multi_house_rules_ok(H1, H2, H3, H4, H5)],
	[Nationality] = [N || #house{nationality=N, pet=zebra} <- lists:flatten(Streets)],
	io:fwrite( "~p owns the zebra~n", [Nationality] ),
	io:fwrite( "All solutions ~p~n", [Streets] ),
	io:fwrite( "Number of solutions ~p~n", [erlang:length(Streets)] ).



all_colours() -> [blue, green, red, white, yellow].

all_drinks() -> [beer, coffe, milk, tea, water].

all_nationalities() -> [danish, english, german, norveigan, swedish].

all_numbers() -> [1, 2, 3, 4, 5].

all_pets() -> [birds, cats, dog, horse, zebra].

all_smokes() -> [blend, 'blue master', dunhill, 'pall mall', prince].

house_number_sort( #house{number=1}=House, #sorted_houses{house_1s=Houses_1s}=Sorted_houses ) -> Sorted_houses#sorted_houses{house_1s=[House | Houses_1s]};
house_number_sort( #house{number=2}=House, #sorted_houses{house_2s=Houses_2s}=Sorted_houses ) -> Sorted_houses#sorted_houses{house_2s=[House | Houses_2s]};
house_number_sort( #house{number=3}=House, #sorted_houses{house_3s=Houses_3s}=Sorted_houses ) -> Sorted_houses#sorted_houses{house_3s=[House | Houses_3s]};
house_number_sort( #house{number=4}=House, #sorted_houses{house_4s=Houses_4s}=Sorted_houses ) -> Sorted_houses#sorted_houses{house_4s=[House | Houses_4s]};
house_number_sort( #house{number=5}=House, #sorted_houses{house_5s=Houses_5s}=Sorted_houses ) -> Sorted_houses#sorted_houses{house_5s=[House | Houses_5s]}.

is_all_different( [_H] ) -> true;
is_all_different( [H | T] ) -> not lists:member( H, T ) andalso is_all_different( T ).

is_all_multi_house_rules_ok( House1, House2, House3, House4, House5 ) ->
	is_rule_1_ok( House1, House2, House3, House4, House5 )
	andalso is_rule_5_ok( House1, House2, House3, House4, House5 )
	andalso is_rule_11_ok( House1, House2, House3, House4, House5 )
	andalso is_rule_12_ok( House1, House2, House3, House4, House5 )
	andalso is_rule_15_ok( House1, House2, House3, House4, House5 )
	andalso is_rule_16_ok( House1, House2, House3, House4, House5 ).

is_all_single_house_rules_ok( Colour, Drink, Nationality, Number, Pet, Smoke ) ->
	is_rule_ok( {rule_number, 2}, {Nationality, english}, {Colour, red})
	andalso is_rule_ok( {rule_number, 3}, {Nationality, swedish}, {Pet, dog})
	andalso is_rule_ok( {rule_number, 4}, {Nationality, danish}, {Drink, tea})
	andalso is_rule_ok( {rule_number, 6}, {Drink, coffe}, {Colour, green})
	andalso is_rule_ok( {rule_number, 7}, {Smoke, 'pall mall'}, {Pet, birds})
	andalso is_rule_ok( {rule_number, 8}, {Colour, yellow}, {Smoke, dunhill})
	andalso is_rule_ok( {rule_number, 9}, {Number, 3}, {Drink, milk})
	andalso is_rule_ok( {rule_number, 10}, {Nationality, norveigan}, {Number, 1})
	andalso is_rule_ok( {rule_number, 13}, {Smoke, 'blue master'}, {Drink, beer})
	andalso is_rule_ok( {rule_number, 14}, {Nationality, german}, {Smoke, prince}).

is_rule_ok( _Rule_number, {A, A}, {B, B} ) -> true;
is_rule_ok( _Rule_number, _A, {B, B} ) -> false;
is_rule_ok( _Rule_number, {A, A}, _B ) -> false;
is_rule_ok( _Rule_number, _A, _B ) -> true.

is_rule_1_ok( #house{number=1}=H1,  #house{number=2}=H2,  #house{number=3}=H3,  #house{number=4}=H4,  #house{number=5}=H5  ) ->
	is_all_different( [H1#house.colour, H2#house.colour, H3#house.colour, H4#house.colour, H5#house.colour] )
	andalso is_all_different( [H1#house.drink, H2#house.drink, H3#house.drink, H4#house.drink, H5#house.drink] )
	andalso is_all_different( [H1#house.nationality, H2#house.nationality, H3#house.nationality, H4#house.nationality, H5#house.nationality] )
	andalso is_all_different( [H1#house.pet, H2#house.pet, H3#house.pet, H4#house.pet, H5#house.pet] )
	andalso is_all_different( [H1#house.smoke, H2#house.smoke, H3#house.smoke, H4#house.smoke, H5#house.smoke] );
is_rule_1_ok( _House1,  _House2,  _House3,  _House4,  _House5  ) -> false.

is_rule_5_ok( #house{colour=green},  #house{colour=white},  _House3,  _House4,  _House5  ) -> true;
is_rule_5_ok( _House1,  #house{colour=green},  #house{colour=white},  _House4,  _House5  ) -> true;
is_rule_5_ok( _House1,  _House2,  #house{colour=green},  #house{colour=white},  _House5  ) -> true;
is_rule_5_ok( _House1,  _House2,  _House3,  #house{colour=green},  #house{colour=white}  ) -> true;
is_rule_5_ok( _House1,  _House2,  _House3,  _House4,  _House5  ) -> false.

is_rule_11_ok( #house{smoke=blend},  #house{pet=cats},  _House3,  _House4,  _House5  ) -> true;
is_rule_11_ok( _House1,  #house{smoke=blend},  #house{pet=cats},  _House4,  _House5  ) -> true;
is_rule_11_ok( _House1,  _House2,  #house{smoke=blend},  #house{pet=cats},  _House5  ) -> true;
is_rule_11_ok( _House1,  _House2,  _House3,  #house{smoke=blend},  #house{pet=cats}  ) -> true;
is_rule_11_ok( #house{pet=cats},  #house{smoke=blend},  _House3,  _House4,  _House5  ) -> true;
is_rule_11_ok( _House1,  #house{pet=cats},  #house{smoke=blend},  _House4,  _House5  ) -> true;
is_rule_11_ok( _House1,  _House2,  #house{pet=cats},  #house{smoke=blend},  _House5  ) -> true;
is_rule_11_ok( _House1,  _House2,  _House3,  #house{pet=cats},  #house{smoke=blend}  ) -> true;
is_rule_11_ok( _House1,  _House2,  _House3,  _House4,  _House5  ) -> false.

is_rule_12_ok( #house{smoke=dunhill},  #house{pet=horse},  _House3,  _House4,  _House5  ) -> true;
is_rule_12_ok( _House1,  #house{smoke=dunhill},  #house{pet=horse},  _House4,  _House5  ) -> true;
is_rule_12_ok( _House1,  _House2,  #house{smoke=dunhill},  #house{pet=horse},  _House5  ) -> true;
is_rule_12_ok( _House1,  _House2,  _House3,  #house{smoke=dunhill},  #house{pet=horse}  ) -> true;
is_rule_12_ok( #house{pet=horse},  #house{smoke=dunhill},  _House3,  _House4,  _House5  ) -> true;
is_rule_12_ok( _House1,  #house{pet=horse},  #house{smoke=dunhill},  _House4,  _House5  ) -> true;
is_rule_12_ok( _House1,  _House2,  #house{pet=horse},  #house{smoke=dunhill},  _House5  ) -> true;
is_rule_12_ok( _House1,  _House2,  _House3,  #house{pet=horse},  #house{smoke=dunhill}  ) -> true;
is_rule_12_ok( _House1,  _House2,  _House3,  _House4,  _House5  ) -> false.

is_rule_15_ok( #house{nationality=norveigan},  #house{colour=blue},  _House3,  _House4,  _House5  ) -> true;
is_rule_15_ok( _House1,  #house{nationality=norveigan},  #house{colour=blue},  _House4,  _House5  ) -> true;
is_rule_15_ok( _House1,  _House2,  #house{nationality=norveigan},  #house{colour=blue},  _House5  ) -> true;
is_rule_15_ok( _House1,  _House2,  _House3,  #house{nationality=norveigan},  #house{colour=blue}  ) -> true;
is_rule_15_ok( #house{colour=blue},  #house{nationality=norveigan},  _House3,  _House4,  _House5  ) -> true;
is_rule_15_ok( _House1,  #house{colour=blue},  #house{nationality=norveigan},  _House4,  _House5  ) -> true;
is_rule_15_ok( _House1,  _House2,  #house{drink=water},  #house{nationality=norveigan},  _House5  ) -> true;
is_rule_15_ok( _House1,  _House2,  _House3,  #house{drink=water},  #house{nationality=norveigan}  ) -> true;
is_rule_15_ok( _House1,  _House2,  _House3,  _House4,  _House5  ) -> false.

is_rule_16_ok( #house{smoke=blend},  #house{drink=water},  _House3,  _House4,  _House5  ) -> true;
is_rule_16_ok( _House1,  #house{smoke=blend},  #house{drink=water},  _House4,  _House5  ) -> true;
is_rule_16_ok( _House1,  _House2,  #house{smoke=blend},  #house{drink=water},  _House5  ) -> true;
is_rule_16_ok( _House1,  _House2,  _House3,  #house{smoke=blend},  #house{drink=water}  ) -> true;
is_rule_16_ok( #house{drink=water},  #house{smoke=blend},  _House3,  _House4,  _House5  ) -> true;
is_rule_16_ok( _House1,  #house{drink=water},  #house{smoke=blend},  _House4,  _House5  ) -> true;
is_rule_16_ok( _House1,  _House2,  #house{drink=water},  #house{smoke=blend},  _House5  ) -> true;
is_rule_16_ok( _House1,  _House2,  _House3,  #house{drink=water},  #house{smoke=blend}  ) -> true;
is_rule_16_ok( _House1,  _House2,  _House3,  _House4,  _House5  ) -> false.

```

{{out}}

```txt

10> zebra_puzzle:task().
german owns the zebra
All solutions [[{house,yellow,water,norveigan,1,cats,dunhill},
                {house,blue,tea,danish,2,horse,blend},
                {house,red,milk,english,3,birds,'pall mall'},
                {house,green,coffe,german,4,zebra,prince},
                {house,white,beer,swedish,5,dog,'blue master'}]]
Number of solutions: 1

```



## ERRE


```ERRE

PROGRAM ZEBRA_PUZZLE

DIM DRINK$[4],NATION$[4],COLR$[4],SMOKE$[4],ANIMAL$[4]
DIM PERM$[120],X$[4]

PROCEDURE PERMUTATION(X$[]->X$[],OK)
    LOCAL I%,J%
    FOR I%=UBOUND(X$,1)-1 TO 0 STEP -1 DO
       EXIT IF X$[I%]<X$[I%+1]
    END FOR
    IF I%<0 THEN OK=FALSE  EXIT PROCEDURE END IF
    J%=UBOUND(X$,1)
    WHILE X$[J%]<=X$[I%] DO
         J%=J%-1
    END WHILE
    SWAP(X$[I%],X$[J%])
    I%=I%+1
    J%=UBOUND(X$,1)
    WHILE I%<J% DO
        SWAP(X$[I%],X$[J%])
        I%=I%+1
        J%=J%-1
    END WHILE
    OK=TRUE
END PROCEDURE

BEGIN

! The names (only used for printing the results)

    DATA("Beer","Coffee","Milk","Tea","Water")
    DATA("Denmark","England","Germany","Norway","Sweden")
    DATA("Blue","Green","Red","White","Yellow")
    DATA("Blend","BlueMaster","Dunhill","PallMall","Prince")
    DATA("Birds","Cats","Dog","Horse","Zebra")

    FOR I%=0 TO 4 DO READ(DRINK$[I%])   END FOR
    FOR I%=0 TO 4 DO READ(NATION$[I%])  END FOR
    FOR I%=0 TO 4 DO READ(COLR$[I%])    END FOR
    FOR I%=0 TO 4 DO READ(SMOKE$[I%])   END FOR
    FOR I%=0 TO 4 DO READ(ANIMAL$[I%])  END FOR

! Some single-character tags:
    A$="A"  B$="B"  c$="C"  d$="D"  e$="E"

! ERRE doesn't have enumerations!
    Beer$=A$  Coffee$=B$  Milk$=c$  TeA$=d$  Water$=e$
    Denmark$=A$  England$=B$  Germany$=c$  Norway$=d$  Sweden$=e$
    Blue$=A$  Green$=B$  Red$=c$  White$=d$  Yellow$=e$
    Blend$=A$  BlueMaster$=B$  Dunhill$=c$  PallMall$=d$  Prince$=e$
    Birds$=A$  Cats$=B$  Dog$=c$  Horse$=d$  ZebrA$=e$

    PRINT(CHR$(12);)

! Create the 120 permutations of 5 objects:

    X$[0]=A$  X$[1]=B$  X$[2]=C$  X$[3]=D$  X$[4]=E$

    REPEAT
       P%=P%+1
       PERM$[P%]=X$[0]+X$[1]+X$[2]+X$[3]+X$[4]
       PERMUTATION(X$[]->X$[],OK)
    UNTIL NOT OK

! Solve:
    SOLUTIONS%=0
    T1=TIMER
    FOR NATION%=1 TO 120 DO
        NATION$=PERM$[NATION%]
        IF LEFT$(NATION$,1)=Norway$ THEN
             FOR COLR%=1 TO 120 DO
                COLR$=PERM$[COLR%]
                IF INSTR(COLR$,Green$+White$)<>0 AND INSTR(NATION$,England$)=INSTR(COLR$,Red$) AND ABS(INSTR(NATION$,Norway$)-INSTR(COLR$,Blue$))=1 THEN
                    FOR DRINK%=1 TO 120 DO
                       DRINK$=PERM$[DRINK%]
                       IF MID$(DRINK$,3,1)=Milk$ AND INSTR(NATION$,Denmark$)=INSTR(DRINK$,TeA$) AND INSTR(DRINK$,Coffee$)=INSTR(COLR$,Green$) THEN
                           FOR SmOKe%=1 TO 120 DO
                              SmOKe$=PERM$[SMOKE%]
                              IF INSTR(NATION$,Germany$)=INSTR(SmOKe$,Prince$) AND INSTR(SmOKe$,BlueMaster$)=INSTR(DRINK$,Beer$) AND ABS(INSTR(SmOKe$,Blend$)-INSTR(DRINK$,Water$))=1 AND INSTR(SmOKe$,Dunhill$)=INSTR(COLR$,Yellow$) THEN
                                  FOR ANIMAL%=1 TO 120 DO
                                     ANIMAL$=PERM$[ANIMAL%]
                                     IF INSTR(NATION$,Sweden$)=INSTR(ANIMAL$,Dog$) AND INSTR(SmOKe$,PallMall$)=INSTR(ANIMAL$,Birds$) AND ABS(INSTR(SmOKe$,Blend$)-INSTR(ANIMAL$,Cats$))=1 AND ABS(INSTR(SmOKe$,Dunhill$)-INSTR(ANIMAL$,Horse$))=1 THEN
                                         PRINT("House    Drink  Nation Colour Smoke  Animal")
                                         PRINT("---------------------------------------------------------------------------")
                                         FOR house%=1 TO 5 DO
                                            PRINT(house%;)
                                            PRINT(TAB(10);DRINK$[ASC(MID$(DRINK$,house%))-65];)
                                            PRINT(TAB(25);NATION$[ASC(MID$(NATION$,house%))-65];)
                                            PRINT(TAB(40);COLR$[ASC(MID$(COLR$,house%))-65];)
                                            PRINT(TAB(55);SMOKE$[ASC(MID$(SmOKe$,house%))-65];)
                                            PRINT(TAB(70);ANIMAL$[ASC(MID$(ANIMAL$,house%))-65])
                                         END FOR
                                         SOLUTIONS%=SOLUTIONS%+1
                                     END IF
                                  END FOR ! ANIMAL%
                              END IF
                           END FOR ! SmOKe%
                       END IF
                    END FOR ! DRINK%
                END IF
             END FOR ! COLR%
        END IF
    END FOR ! NATION%
    PRINT("Number of solutions=";SOLUTIONS%)
    PRINT("Solved in ";TIMER-T1;" seconds")
END PROGRAM
```

{{out}}

```txt

House    Drink  Nation Colour Smoke  Animal
---------------------------------------------------------------------------
 1       Water          Norway         Yellow         Dunhill        Cats
 2       Tea            Denmark        Blue           Blend          Horse
 3       Milk           England        Red            PallMall       Birds
 4       Coffee         Germany        Green          Prince         Zebra
 5       Beer           Sweden         White          BlueMaster     Dog
Number of solutions= 1
Solved in  .109375  seconds
```


=={{header|F_Sharp|F#}}==
This task uses [http://www.rosettacode.org/wiki/Permutations_by_swapping#F.23]

```fsharp

(*Here I solve the Zebra puzzle using Plain Changes, definitely a challenge to some campanoligist to solve it using Grandsire Doubles.
  Nigel Galloway: January 27th., 2017 *)
type N = |English |Swedish|Danish    |German|Norwegian
type I = |Tea     |Coffee |Milk      |Beer  |Water
type G = |Dog     |Birds  |Cats      |Horse |Zebra
type E = |Red     |Green  |White     |Blue  |Yellow
type L = |PallMall|Dunhill|BlueMaster|Prince|Blend
type NIGELz={Nz:N[];Iz:I[];Gz:G[];Ez:E[];Lz:L[]}
let fn (i:'n[]) g (e:'g[]) l =                            //coincident?
  let rec _fn = function
    |5                                -> false
    |ig when (i.[ig]=g && e.[ig]=l)   -> true
    |ig                               -> _fn (ig+1)
  _fn 0
let fi (i:'n[]) g (e:'g[]) l =                            //leftof?
  let rec _fn = function
    |4                                -> false
    |ig when (i.[ig]=g && e.[ig+1]=l) -> true
    |ig                               -> _fn (ig+1)
  _fn 0
let fg (i:'n[]) g (e:'g[]) l = (fi i g e l || fi e l i g) //adjacent?
let  n = Ring.PlainChanges [|N.English; N.Swedish; N.Danish;    N.German; N.Norwegian|]|>Seq.filter(fun n->match n with |Some(n)->(n.[0]=N.Norwegian)   |_->false)//#10
let  i = Ring.PlainChanges [|I.Tea;     I.Coffee;  I.Milk;      I.Beer;   I.Water    |]|>Seq.filter(fun n->match n with |Some(n)->(n.[2]=I.Milk)        |_->false)//# 9
let  g = Ring.PlainChanges [|G.Dog;     G.Birds;   G.Cats;      G.Horse;  G.Zebra    |]|>Seq.filter(fun n->match n with |Some(n)->true                  |_->false)
let  e = Ring.PlainChanges [|E.Red;     E.Green;   E.White;     E.Blue;   E.Yellow   |]|>Seq.filter(fun n->match n with |Some(n)->fi n E.Green n E.White|_->false)//# 5
let  l = Ring.PlainChanges [|L.PallMall;L.Dunhill; L.BlueMaster;L.Prince; L.Blend    |]|>Seq.filter(fun n->match n with |Some(n)->true                  |_->false)
match n|>Seq.map(fun n->{Nz=Option.get n;Iz=[||];Gz=[||];Ez=[||];Lz=[||]})
       |>Seq.collect(fun n->i|>Seq.map(fun i->{n with Iz=Option.get i}))|>Seq.filter(fun n-> fn n.Nz N.Danish    n.Iz I.Tea)                                      //# 4
       |>Seq.collect(fun n->g|>Seq.map(fun i->{n with Gz=Option.get i}))|>Seq.filter(fun n-> fn n.Nz N.Swedish   n.Gz G.Dog)                                      //# 3
       |>Seq.collect(fun n->e|>Seq.map(fun i->{n with Ez=Option.get i}))|>Seq.filter(fun n-> fn n.Nz N.English   n.Ez E.Red   &&                                  //# 2
                                                                                             fn n.Ez E.Green     n.Iz I.Coffee&&                                  //# 6
                                                                                             fg n.Nz N.Norwegian n.Ez E.Blue)                                     //#15
       |>Seq.collect(fun n->l|>Seq.map(fun i->{n with Lz=Option.get i}))|>Seq.tryFind(fun n->fn n.Lz L.PallMall  n.Gz G.Birds &&                                  //# 7
                                                                                             fg n.Lz L.Blend     n.Gz G.Cats  &&                                  //#11
                                                                                             fn n.Lz L.Prince    n.Nz N.German&&                                  //#14
                                                                                             fg n.Lz L.Blend     n.Iz I.Water &&                                  //#16
                                                                                             fg n.Lz L.Dunhill   n.Gz G.Horse &&                                  //#12
                                                                                             fn n.Lz L.Dunhill   n.Ez E.Yellow&&                                  //# 8
                                                                                             fn n.Iz I.Beer      n.Lz L.BlueMaster)  with                         //#13
|Some(nn) -> nn.Gz |> Array.iteri(fun n g -> if (g = G.Zebra) then printfn "\nThe man who owns a zebra is %A\n" nn.Nz.[n]); printfn "%A" nn
|None    -> printfn "No solution found"

```

{{out}}

```txt

The man who owns a zebra is German

{Nz = [|Norwegian; Danish; English; German; Swedish|];
 Iz = [|Water; Tea; Milk; Coffee; Beer|];
 Gz = [|Cats; Horse; Birds; Zebra; Dog|];
 Ez = [|Yellow; Blue; Red; Green; White|];
 Lz = [|Dunhill; Blend; PallMall; Prince; BlueMaster|];}

```



## FormulaOne


```FormulaOne

// First, let's give some variables some values:
Nationality = Englishman | Swede   | Dane       | Norwegian | German
Colour      = Red        | Green   | Yellow     | Blue      | White
Cigarette   = PallMall   | Dunhill | BlueMaster | Blend     | Prince
Domestic    = Dog        | Bird    | Cat        | Zebra     | Horse
Beverage    = Tea        | Coffee  | Milk       | Beer      | Water
HouseOrder  = First      | Second  | Third      | Fourth    | Fifth

{
We use injections to make the array-elements unique.
Example: 'Pet' is an array of unique elements of type 'Domestic', indexed by 'Nationality'.
In the predicate 'Zebra', we use this injection 'Pet' to define the array-variable 'pet'.
The symbol used is the '->>'. 'Nationality->>Domestic' can be read as 'Domestic(Nationality)' in "plain array-speak";
the difference being that the elements are by definition unique.

So, in FormulaOne we use a formula like: 'pet(Swede) = Dog', which simply means that the 'Swede' (type 'Nationality')
has a 'pet' (type 'Pet', which is of type 'Domestic', indexed by 'Nationality'), which appears to be a 'Dog' (type 'Domestic').
Or, one could say that the 'Swede' has been mapped to the 'Dog' (Oh, well...).
}

Pet          = Nationality->>Domestic
Drink        = Nationality->>Beverage
HouseColour  = Nationality->>Colour
Smoke        = Nationality->>Cigarette
Order        = HouseOrder->>Nationality

pred Zebra(houseColour::HouseColour, pet::Pet, smoke::Smoke, drink::Drink, order::Order) iff

// For convenience sake, some temporary place_holder variables are used.
// An underscore distinguishes them:

     houseColour(green_house) = Green &
     houseColour(white_house) = White &
     houseColour(yellow_house) = Yellow &
     smoke(pallmall_smoker) = PallMall &
     smoke(blend_smoker) = Blend &
     smoke(dunhill_smoker) = Dunhill &
     smoke(bluemaster_smoker) = BlueMaster &
     pet(cat_keeper) = Cat &
     pet(neighbour_dunhill_smoker) = Horse &

{ 2. The English man lives in the red house: }
     houseColour(Englishman) = Red &

{ 3. The Swede has a dog: }
     pet(Swede) = Dog &

{ 4. The Dane drinks tea: }
     drink(Dane) = Tea &

    { 'smoke' and 'drink' are both nouns, like the other variables.
      One could read the formulas like: 'the colour of the Englishman's house is Red' ->
     'the Swede's pet is a dog' -> 'the Dane's drink is tea'.
    }

{ 5. The green house is immediately to the left of the white house: }
     { The local predicate 'LeftOf' determines the order: }
       LeftOf(green_house, white_house, order) &

{ 6. They drink coffee in the green house: }
     drink(green_house) = Coffee &

{ 7. The man who smokes Pall Mall has birds: }
     pet(pallmall_smoker) = Bird &

{ 8. In the yellow house they smoke Dunhill: }
     smoke(yellow_house) = Dunhill &

{ 9. In the middle house they drink milk: }
     drink(order(Third)) = Milk &

{10. The Norwegian lives in the first house: }
     order(First) = Norwegian &

{11. The man who smokes Blend lives in the house next to the house with cats: }
     { Another local predicate 'Neighbour' makes them neighbours:}
       Neighbour(blend_smoker, cat_keeper, order) &

{12. In a house next to the house where they have a horse, they smoke Dunhill: }
     Neighbour(dunhill_smoker, neighbour_dunhill_smoker, order) &

{13. The man who smokes Blue Master drinks beer: }
     drink(bluemaster_smoker) = Beer &

{14. The German smokes Prince: }
     smoke(German) = Prince &

{15. The Norwegian lives next to the blue house: }
     {10. The Norwegian lives in the first house,
          so the blue house is the second house }
          houseColour(order(Second)) = Blue &

{16. They drink water in a house next to the house where they smoke Blend: }
     drink(neighbour_blend_smoker) = Water &
     Neighbour(blend_smoker, neighbour_blend_smoker, order)

{  A simplified solution would number the houses 1, 2, 3, 4, 5
   which makes it easier to order the houses.
   'right in the center' would become 3; 'in the first house', 1
   But we stick to the original puzzle and use some local predicates.
}

local pred Neighbour(neighbour1::Nationality, neighbour2::Nationality, order::Order)iff
   neighbour1 <> neighbour2 &
   order(house1) = neighbour1 &
   order(house2) = neighbour2 &
   ( house1 = house2 + 1 |
     house1 = house2 - 1 )

local pred LeftOf(neighbour1::Nationality, neighbour2::Nationality, order::Order) iff
   neighbour1 <> neighbour2 &
   order(house1) = neighbour1 &
   order(house2) = neighbour2 &
   house1 = house2 - 1

{
The 'all'-query in FormulaOne:
     all Zebra(houseColour, pet, smokes, drinks, order)
gives, of course, only one solution, so it can be replaced by:
     one Zebra(houseColour, pet, smokes, drinks, order)
}

// The compacted version:

Nationality = Englishman | Swede   | Dane       | Norwegian | German
Colour      = Red        | Green   | Yellow     | Blue      | White
Cigarette   = PallMall   | Dunhill | BlueMaster | Blend     | Prince
Domestic    = Dog        | Bird    | Cat        | Zebra     | Horse
Beverage    = Tea        | Coffee  | Milk       | Beer      | Water
HouseOrder  = First      | Second  | Third      | Fourth    | Fifth

Pet          = Nationality->>Domestic
Drink        = Nationality->>Beverage
HouseColour  = Nationality->>Colour
Smoke        = Nationality->>Cigarette
Order        = HouseOrder->>Nationality

pred Zebra(houseColour::HouseColour, pet::Pet, smoke::Smoke, drink::Drink, order::Order) iff

  houseColour(green_house) = Green &
  houseColour(white_house) = White &
  houseColour(yellow_house) = Yellow &
  smoke(pallmall_smoker) = PallMall &
  smoke(blend_smoker) = Blend &
  smoke(dunhill_smoker) = Dunhill &
  smoke(bluemaster_smoker) = BlueMaster &
  pet(cat_keeper) = Cat &
  pet(neighbour_dunhill_smoker) = Horse &

  houseColour(Englishman) = Red &
  pet(Swede) = Dog &
  drink(Dane) = Tea &
  LeftOf(green_house, white_house, order) &
  drink(green_house) = Coffee &
  pet(pallmall_smoker) = Bird &
  smoke(yellow_house) = Dunhill &
  drink(order(Third)) = Milk &
  order(First) = Norwegian &
  Neighbour(blend_smoker, cat_keeper, order) &
  Neighbour(dunhill_smoker, neighbour_dunhill_smoker, order) &
  drink(bluemaster_smoker) = Beer &
  smoke(German) = Prince &
  houseColour(order(Second)) = Blue &
  drink(neighbour_blend_smoker) = Water &
  Neighbour(blend_smoker, neighbour_blend_smoker, order)

local pred Neighbour(neighbour1::Nationality, neighbour2::Nationality, order::Order)iff
   neighbour1 <> neighbour2 &
   order(house1) = neighbour1 & order(house2) = neighbour2 &
   ( house1 = house2 + 1 | house1 = house2 - 1 )

local pred LeftOf(neighbour1::Nationality, neighbour2::Nationality, order::Order) iff
   neighbour1 <> neighbour2 &
   order(house1) = neighbour1 & order(house2) = neighbour2 &
   house1 = house2 - 1

```

{{out}}

```txt

houseColour  = [ {Englishman} Red, {Swede}  White, {Dane}  Blue, {Norwegian}  Yellow, {German} Green ]
pet = [ {Englishman} Birds, {Swede} Dog, {Dane} Horse, {Norwegian} Cats, {German}  Zebra ]
smokes = [ {Englishman} PallMall, {Swede} BlueMaster, {Dane} Blend, {Norwegian} Dunhill, {German} Prince ]
drinks = [ {Englishman} Milk, {Swede} Beer, {Dane} Tea, {Norwegian}  Water,  {German} Coffee ]
order = [ {First} Norwegian, {Second} Dane, {Third} Englishman, {Fourth} German,  {Fifth}, Swede ]

```


## GAP


```gap
leftOf  :=function(setA, vA, setB, vB)
local i;
for i in [1..4] do
   if ( setA[i] = vA) and  (setB[i+1] = vB) then return true ;fi;
od;
 return false;
end;

nextTo  :=function(setA, vA, setB, vB)
local i;
for i in [1..4] do
   if ( setA[i] = vA) and  (setB[i+1] = vB) then return true ;fi;
   if ( setB[i] = vB) and  (setA[i+1] = vA) then return true ;fi;
od;
return false;
end;


requires := function(setA, vA, setB, vB)
    local i;
      for i in [1..5] do
        if ( setA[i] = vA) and  (setB[i] = vB) then return true ;fi;
      od;
 return false;
end;


pcolors :=PermutationsList(["white" ,"yellow" ,"blue" ,"red" ,"green"]);
pcigars :=PermutationsList(["blends", "pall_mall", "prince", "bluemasters", "dunhill"]);
pnats:=PermutationsList(["german", "swedish", "british", "norwegian", "danish"]);
pdrinks :=PermutationsList(["beer", "water", "tea", "milk", "coffee"]);
ppets  :=PermutationsList(["birds", "cats", "horses", "fish", "dogs"]);


for colors in pcolors do
if not (leftOf(colors,"green",colors,"white")) then continue ;fi;
for nats in pnats do
if not (requires(nats,"british",colors,"red")) then  continue ;fi;
if not (nats[1]="norwegian") then continue ;fi;
if not (nextTo(nats,"norwegian",colors,"blue")) then continue ;fi;
for pets in ppets do
if not (requires(nats,"swedish",pets,"dogs")) then  continue ;fi;
for drinks in pdrinks do
if not (drinks[3]="milk") then continue ;fi;
if not (requires(colors,"green",drinks,"coffee")) then continue ;fi;
if not (requires(nats,"danish",drinks,"tea")) then  continue ;fi;
for cigars in pcigars do
if not (nextTo(pets,"horses",cigars,"dunhill")) then continue ;fi;
if not (requires(cigars,"pall_mall",pets,"birds")) then  continue ;fi;
if not (nextTo(cigars,"blends",drinks,"water")) then  continue ;fi;
if not (nextTo(cigars,"blends",pets,"cats")) then  continue ;fi;
if not (requires(nats,"german",cigars,"prince")) then  continue ;fi;
if not (requires(colors,"yellow",cigars,"dunhill")) then continue ;fi;
if not (requires(cigars,"bluemasters",drinks,"beer")) then  continue ;fi;
Print(colors,"\n");
Print(nats,"\n");
Print(drinks,"\n");
Print(pets,"\n");
Print(cigars,"\n");
od;od;od;od;od;

```



## Go


```go
package main

import (
        "fmt"
        "log"
        "strings"
)

// Define some types

type HouseSet [5]*House
type House struct {
        n Nationality
        c Colour
        a Animal
        d Drink
        s Smoke
}
type Nationality int8
type Colour int8
type Animal int8
type Drink int8
type Smoke int8

// Define the possible values

const (
        English Nationality = iota
        Swede
        Dane
        Norwegian
        German
)
const (
        Red Colour = iota
        Green
        White
        Yellow
        Blue
)
const (
        Dog Animal = iota
        Birds
        Cats
        Horse
        Zebra
)
const (
        Tea Drink = iota
        Coffee
        Milk
        Beer
        Water
)
const (
        PallMall Smoke = iota
        Dunhill
        Blend
        BlueMaster
        Prince
)

// And how to print them

var nationalities = [...]string{"English", "Swede", "Dane", "Norwegian", "German"}
var colours = [...]string{"red", "green", "white", "yellow", "blue"}
var animals = [...]string{"dog", "birds", "cats", "horse", "zebra"}
var drinks = [...]string{"tea", "coffee", "milk", "beer", "water"}
var smokes = [...]string{"Pall Mall", "Dunhill", "Blend", "Blue Master", "Prince"}

func (n Nationality) String() string { return nationalities[n] }
func (c Colour) String() string      { return colours[c] }
func (a Animal) String() string      { return animals[a] }
func (d Drink) String() string       { return drinks[d] }
func (s Smoke) String() string       { return smokes[s] }
func (h House) String() string {
        return fmt.Sprintf("%-9s  %-6s  %-5s  %-6s  %s", h.n, h.c, h.a, h.d, h.s)
}
func (hs HouseSet) String() string {
        lines := make([]string, 0, len(hs))
        for i, h := range hs {
                s := fmt.Sprintf("%d  %s", i, h)
                lines = append(lines, s)
        }
        return strings.Join(lines, "\n")
}

// Simple brute force solution

func simpleBruteForce() (int, HouseSet) {
        var v []House
        for n := range nationalities {
                for c := range colours {
                        for a := range animals {
                                for d := range drinks {
                                        for s := range smokes {
                                                h := House{
                                                        n: Nationality(n),
                                                        c: Colour(c),
                                                        a: Animal(a),
                                                        d: Drink(d),
                                                        s: Smoke(s),
                                                }
                                                if !h.Valid() {
                                                        continue
                                                }
                                                v = append(v, h)
                                        }
                                }
                        }
                }
        }
        n := len(v)
        log.Println("Generated", n, "valid houses")

        combos := 0
        first := 0
        valid := 0
        var validSet HouseSet
        for a := 0; a < n; a++ {
                if v[a].n != Norwegian { // Condition 10:
                        continue
                }
                for b := 0; b < n; b++ {
                        if b == a {
                                continue
                        }
                        if v[b].anyDups(&v[a]) {
                                continue
                        }
                        for c := 0; c < n; c++ {
                                if c == b || c == a {
                                        continue
                                }
                                if v[c].d != Milk { // Condition 9:
                                        continue
                                }
                                if v[c].anyDups(&v[b], &v[a]) {
                                        continue
                                }
                                for d := 0; d < n; d++ {
                                        if d == c || d == b || d == a {
                                                continue
                                        }
                                        if v[d].anyDups(&v[c], &v[b], &v[a]) {
                                                continue
                                        }
                                        for e := 0; e < n; e++ {
                                                if e == d || e == c || e == b || e == a {
                                                        continue
                                                }
                                                if v[e].anyDups(&v[d], &v[c], &v[b], &v[a]) {
                                                        continue
                                                }
                                                combos++
                                                set := HouseSet{&v[a], &v[b], &v[c], &v[d], &v[e]}
                                                if set.Valid() {
                                                        valid++
                                                        if valid == 1 {
                                                                first = combos
                                                        }
                                                        validSet = set
                                                        //return set
                                                }
                                        }
                                }
                        }
                }
        }
        log.Println("Tested", first, "different combinations of valid houses before finding solution")
        log.Println("Tested", combos, "different combinations of valid houses in total")
        return valid, validSet
}

// anyDups returns true if h as any duplicate attributes with any of the specified houses
func (h *House) anyDups(list ...*House) bool {
        for _, b := range list {
                if h.n == b.n || h.c == b.c || h.a == b.a || h.d == b.d || h.s == b.s {
                        return true
                }
        }
        return false
}

func (h *House) Valid() bool {
        // Condition 2:
        if h.n == English && h.c != Red || h.n != English && h.c == Red {
                return false
        }
        // Condition 3:
        if h.n == Swede && h.a != Dog || h.n != Swede && h.a == Dog {
                return false
        }
        // Condition 4:
        if h.n == Dane && h.d != Tea || h.n != Dane && h.d == Tea {
                return false
        }
        // Condition 6:
        if h.c == Green && h.d != Coffee || h.c != Green && h.d == Coffee {
                return false
        }
        // Condition 7:
        if h.a == Birds && h.s != PallMall || h.a != Birds && h.s == PallMall {
                return false
        }
        // Condition 8:
        if h.c == Yellow && h.s != Dunhill || h.c != Yellow && h.s == Dunhill {
                return false
        }
        // Condition 11:
        if h.a == Cats && h.s == Blend {
                return false
        }
        // Condition 12:
        if h.a == Horse && h.s == Dunhill {
                return false
        }
        // Condition 13:
        if h.d == Beer && h.s != BlueMaster || h.d != Beer && h.s == BlueMaster {
                return false
        }
        // Condition 14:
        if h.n == German && h.s != Prince || h.n != German && h.s == Prince {
                return false
        }
        // Condition 15:
        if h.n == Norwegian && h.c == Blue {
                return false
        }
        // Condition 16:
        if h.d == Water && h.s == Blend {
                return false
        }
        return true
}

func (hs *HouseSet) Valid() bool {
        ni := make(map[Nationality]int, 5)
        ci := make(map[Colour]int, 5)
        ai := make(map[Animal]int, 5)
        di := make(map[Drink]int, 5)
        si := make(map[Smoke]int, 5)
        for i, h := range hs {
                ni[h.n] = i
                ci[h.c] = i
                ai[h.a] = i
                di[h.d] = i
                si[h.s] = i
        }
        // Condition 5:
        if ci[Green]+1 != ci[White] {
                return false
        }
        // Condition 11:
        if dist(ai[Cats], si[Blend]) != 1 {
                return false
        }
        // Condition 12:
        if dist(ai[Horse], si[Dunhill]) != 1 {
                return false
        }
        // Condition 15:
        if dist(ni[Norwegian], ci[Blue]) != 1 {
                return false
        }
        // Condition 16:
        if dist(di[Water], si[Blend]) != 1 {
                return false
        }

        // Condition 9: (already tested elsewhere)
        if hs[2].d != Milk {
                return false
        }
        // Condition 10: (already tested elsewhere)
        if hs[0].n != Norwegian {
                return false
        }
        return true
}

func dist(a, b int) int {
        if a > b {
                return a - b
        }
        return b - a
}

func main() {
        log.SetFlags(0)
        n, sol := simpleBruteForce()
        fmt.Println(n, "solution found")
        fmt.Println(sol)
}
```

{{out}}

```txt
Generated 51 valid houses
Tested 652 different combinations of valid houses before finding solution
Tested 750 different combinations of valid houses in total
1 solution found
0  Norwegian  yellow  cats   water   Dunhill
1  Dane       blue    horse  tea     Blend
2  English    red     birds  milk    Pall Mall
3  German     green   zebra  coffee  Prince
4  Swede      white   dog    beer    Blue Master

```

Benchmark (not included but just calling simpleBruteForce):

```txt

BenchmarkBruteForce         1000           2687946 ns/op          550568 B/op       7560 allocs/op

```



## Haskell


```haskell
module Main where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (foldM, forM_)
import Data.List ((\\))

-- types
data House = House
    { color :: Color      -- <trait> :: House -> <Trait>
    , man   :: Man
    , pet   :: Pet
    , drink :: Drink
    , smoke :: Smoke
    }
    deriving (Eq, Show)

data Color = Red | Green | Blue | Yellow | White
    deriving (Eq, Show, Enum, Bounded)

data Man = Eng | Swe | Dan | Nor | Ger
    deriving (Eq, Show, Enum, Bounded)

data Pet = Dog | Birds | Cats | Horse | Zebra
    deriving (Eq, Show, Enum, Bounded)

data Drink = Coffee | Tea | Milk | Beer | Water
    deriving (Eq, Show, Enum, Bounded)

data Smoke = PallMall | Dunhill | Blend | BlueMaster | Prince
    deriving (Eq, Show, Enum, Bounded)

type Solution = [House]

main :: IO ()
main = do
  forM_ solutions $ \sol -> mapM_ print sol
                            >> putStrLn "----"
  putStrLn "No More Solutions"


solutions :: [Solution]
solutions = filter finalCheck . map reverse $ foldM next [] [1..5]
    where
      -- NOTE: list of houses is generated in reversed order
      next :: Solution -> Int -> [Solution]
      next sol pos = [h:sol | h <- newHouses sol, consistent h pos]


newHouses :: Solution -> Solution
newHouses sol =    -- all combinations of traits not yet used
    House <$> new color <*> new man <*> new pet <*> new drink <*> new smoke
    where
      new trait = [minBound ..] \\ map trait sol  -- :: [<Trait>]


consistent :: House -> Int -> Bool
consistent house pos = and                  -- consistent with the rules:
    [ man   `is` Eng     <=>   color `is` Red              --  2
    , man   `is` Swe     <=>   pet   `is` Dog              --  3
    , man   `is` Dan     <=>   drink `is` Tea              --  4
    , color `is` Green   <=>   drink `is` Coffee           --  6
    , pet   `is` Birds   <=>   smoke `is` PallMall         --  7
    , color `is` Yellow  <=>   smoke `is` Dunhill          --  8
    , const (pos == 3)   <=>   drink `is` Milk             --  9
    , const (pos == 1)   <=>   man   `is` Nor              -- 10
    , drink `is` Beer    <=>   smoke `is` BlueMaster       -- 13
    , man   `is` Ger     <=>   smoke `is` Prince           -- 14
    ]
    where
      infix 4 <=>
      p <=> q  =  p house == q house   -- both True or both False


is :: Eq a => (House -> a) -> a -> House -> Bool
(trait `is` value) house  =  trait house == value


finalCheck :: [House] -> Bool
finalCheck solution = and                    -- fulfills the rules:
    [ (color `is` Green)   `leftOf` (color `is` White)  --  5
    , (smoke `is` Blend  ) `nextTo` (pet   `is` Cats )  -- 11
    , (smoke `is` Dunhill) `nextTo` (pet   `is` Horse)  -- 12
    , (color `is` Blue   ) `nextTo` (man   `is` Nor  )  -- 15
    , (smoke `is` Blend  ) `nextTo` (drink `is` Water)  -- 16
    ]
    where
      nextTo :: (House -> Bool) -> (House -> Bool) -> Bool
      nextTo p q = leftOf p q || leftOf q p
      leftOf p q
          | (_:h:_) <- dropWhile (not . p) solution = q h
          | otherwise                               = False
```

{{out}}

```txt
House {color = Yellow, man = Nor, pet = Cats, drink = Water, smoke = Dunhill}
House {color = Blue, man = Dan, pet = Horse, drink = Tea, smoke = Blend}
House {color = Red, man = Eng, pet = Birds, drink = Milk, smoke = PallMall}
House {color = Green, man = Ger, pet = Zebra, drink = Coffee, smoke = Prince}
House {color = White, man = Swe, pet = Dog, drink = Beer, smoke = BlueMaster}
----
No More Solutions
```

[http://ideone.com/ZaKfWC Runs in: time: 0.01s.]

=== LP-like version ===

(a little faster version)


```haskell
import Control.Monad
import Data.List

values :: (Bounded a, Enum a) => [[a]]
values = permutations [minBound..maxBound]

data Nation = English   | Swede     | Dane  | Norwegian     | German
    deriving (Bounded, Enum, Eq, Show)
data Color  = Red       | Green     | White | Yellow        | Blue
    deriving (Bounded, Enum, Eq, Show)
data Pet    = Dog       | Birds     | Cats  | Horse         | Zebra
    deriving (Bounded, Enum, Eq, Show)
data Drink  = Tea       | Coffee    | Milk  | Beer          | Water
    deriving (Bounded, Enum, Eq, Show)
data Smoke  = PallMall  | Dunhill   | Blend | BlueMaster    | Prince
    deriving (Bounded, Enum, Eq, Show)

answers = do
    color <- values
    leftOf  color  Green       color White    -- 5

    nation <- values
    first   nation Norwegian                  -- 10
    same    nation English     color Red      -- 2
    nextTo  nation Norwegian   color Blue     -- 15

    drink <- values
    middle  drink  Milk                       -- 9
    same    nation Dane        drink Tea      -- 4
    same    drink  Coffee      color Green    -- 6

    pet <- values
    same    nation Swede       pet   Dog      -- 3

    smoke <- values
    same    smoke  PallMall    pet   Birds    -- 7
    same    color  Yellow      smoke Dunhill  -- 8
    nextTo  smoke  Blend       pet   Cats     -- 11
    nextTo  pet    Horse       smoke Dunhill  -- 12
    same    nation German      smoke Prince   -- 14
    same    smoke  BlueMaster  drink Beer     -- 13
    nextTo  drink  Water       smoke Blend    -- 16

    return $ zip5 nation color pet drink smoke

  where
    same    xs x  ys y  =  guard $ (x, y) `elem` zip xs ys
    leftOf  xs x  ys y  =  same  xs x  (tail ys) y
    nextTo  xs x  ys y  =  leftOf  xs x  ys y  `mplus`
                           leftOf  ys y  xs x
    middle  xs x        =  guard $ xs !! 2 == x
    first   xs x        =  guard $ head xs == x

main = do
    forM_ answers $ (\answer ->  -- for answer in answers:
      do
        mapM_ print answer
        print [nation | (nation, _, Zebra, _, _) <- answer]
        putStrLn "" )
    putStrLn "No more solutions!"
```


Output:

```txt
(Norwegian,Yellow,Cats,Water,Dunhill)
(Dane,Blue,Horse,Tea,Blend)
(English,Red,Birds,Milk,PallMall)
(German,Green,Zebra,Coffee,Prince)
(Swede,White,Dog,Beer,BlueMaster)
[German]

No more solutions!
```



## J


Propositions 1 .. 16 without 9,10 and15

```j
ehs=: 5$a:

cr=: (('English';'red') 0 3} ehs);<('Dane';'tea') 0 2}ehs
cr=: cr, (('German';'Prince') 0 4}ehs);<('Swede';'dog') 0 1 }ehs

cs=: <('PallMall';'birds') 4 1}ehs
cs=: cs, (('yellow';'Dunhill') 3 4}ehs);<('BlueMaster';'beer') 4 2}ehs

lof=: (('coffee';'green')2 3}ehs);<(<'white')3}ehs

next=: <((<'Blend') 4 }ehs);<(<'water')2}ehs
next=: next,<((<'Blend') 4 }ehs);<(<'cats')1}ehs
next=: next,<((<'Dunhill') 4}ehs);<(<'horse')1}ehs
```

'''Example'''

```j
   lof
┌─────────────────┬───────────┐
│┌┬┬──────┬─────┬┐│┌┬┬┬─────┬┐│
││││coffee│green│││││││white│││
│└┴┴──────┴─────┴┘│└┴┴┴─────┴┘│
└─────────────────┴───────────┘
```


Collections of all variants of the propositions:

```j
hcr=: (<ehs),. (A.~i.@!@#)cr
hcs=:~. (A.~i.@!@#)cs,2$<ehs
hlof=:(-i.4) |."0 1 lof,3$<ehs
hnext=: ,/((i.4) |."0 1 (3$<ehs)&,)"1 ;(,,:|.)&.> next
```


We start the row of houses with fixed properties 9, 10 and 15.

```j
houses=: ((<'Norwegian') 0}ehs);((<'blue') 3 }ehs);((<'milk') 2}ehs);ehs;<ehs
```

{{out}}

```j
   houses
┌───────────────┬──────────┬──────────┬──────┬──────┐
│┌─────────┬┬┬┬┐│┌┬┬┬────┬┐│┌┬┬────┬┬┐│┌┬┬┬┬┐│┌┬┬┬┬┐│
││Norwegian││││││││││blue││││││milk││││││││││││││││││
│└─────────┴┴┴┴┘│└┴┴┴────┴┘│└┴┴────┴┴┘│└┴┴┴┴┘│└┴┴┴┴┘│
└───────────────┴──────────┴──────────┴──────┴──────┘
```

Set of proposition variants:

```j>constraints=: hcr;hcs;hlof;<hnext</lang

The worker and its helper verbs

```j
select=: ~.@(,: #~ ,&(0~:#))
filter=: #~*./@:(2>#S:0)"1
compose=: [: filter f. [: ,/ select f. L:0"1"1 _

solve=: 4 :0
h=. ,:x
whilst. 0=# z do.
  for_e. y do. h=. h compose > e end.
  z=.(#~1=[:+/"1 (0=#)S:0"1) h=.~. h
end.
)
```

{{out}}

```j>
"0 houses solve constraints
┌─────────┬─────┬──────┬──────┬──────────┐
│Norwegian│cats │water │yellow│Dunhill   │
├─────────┼─────┼──────┼──────┼──────────┤
│Dane     │horse│tea   │blue  │Blend     │
├─────────┼─────┼──────┼──────┼──────────┤
│English  │birds│milk  │red   │PallMall  │
├─────────┼─────┼──────┼──────┼──────────┤
│German   │     │coffee│green │Prince    │
├─────────┼─────┼──────┼──────┼──────────┤
│Swede    │dog  │beer  │white │BlueMaster│
└─────────┴─────┴──────┴──────┴──────────┘
```

So, the German owns the zebra.

'''Alternative'''

A longer running solver by adding the zebra variants.

```j
zebra=: (-i.5)|."0 1 (<(<'zebra') 1}ehs),4$<ehs

solve3=: 4 :0
p=. *./@:((0~:#)S:0)
f=. [:~.&.> [: compose&.>~/y&, f.
z=. f^:(3>[:#(#~p"1)&>)^:_ <,:x
>"0 (#~([:*./[:;[:<@({.~:}.)\.;)"1)(#~p"1); z
)
```

{{out}}

```j
   houses solve3 constraints,<zebra
┌─────────┬─────┬──────┬──────┬──────────┐
│Norwegian│cats │water │yellow│Dunhill   │
├─────────┼─────┼──────┼──────┼──────────┤
│Dane     │horse│tea   │blue  │Blend     │
├─────────┼─────┼──────┼──────┼──────────┤
│English  │birds│milk  │red   │PallMall  │
├─────────┼─────┼──────┼──────┼──────────┤
│German   │zebra│coffee│green │Prince    │
├─────────┼─────┼──────┼──────┼──────────┤
│Swede    │dog  │beer  │white │BlueMaster│
└─────────┴─────┴──────┴──────┴──────────┘
```



## Java

This Java solution includes 3 classes. I developed and tested on a Win7 machine with JDK 1.8 platform, but it should work with earlier version 1.7 also.

First, create a class which express the puzzle line.


```Java

package zebra;

public class LineOfPuzzle implements Cloneable{

    private Integer order;
    private String nation;
    private String color;
    private String animal;
    private String drink;
    private String cigarette;

    private LineOfPuzzle rightNeighbor;
    private LineOfPuzzle leftNeighbor;
    private PuzzleSet<LineOfPuzzle> undefNeighbors;

    public LineOfPuzzle (Integer order, String nation, String color,
                         String animal, String drink, String cigarette){

        this.animal=animal;
        this.cigarette=cigarette;
        this.color=color;
        this.drink=drink;
        this.nation=nation;
        this.order=order;
    }

    public Integer getOrder() {
        return order;
    }

    public void setOrder(Integer order) {
        this.order = order;
    }

    public String getNation() {
        return nation;
    }

    public void setNation(String nation) {
        this.nation = nation;
    }

    public String getColor() {
        return color;
    }

    public void setColor(String color) {
        this.color = color;
    }

    public String getAnimal() {
        return animal;
    }

    public void setAnimal(String animal) {
        this.animal = animal;
    }

    public String getDrink() {
        return drink;
    }

    public void setDrink(String drink) {
        this.drink = drink;
    }

    public String getCigarette() {
        return cigarette;
    }

    public void setCigarette(String cigarette) {
        this.cigarette = cigarette;
    }

    /**
     * Overrides object equal method
     * @param obj
     * @return
     */
    @Override
    public boolean equals(Object obj) {
        if (obj instanceof LineOfPuzzle){
            LineOfPuzzle searchLine = (LineOfPuzzle)obj;
            return  this.getWholeLine().equalsIgnoreCase(searchLine.getWholeLine());
        }
        else
            return false;
    }

    public int getFactsCount(){
        int facts = 0;
        facts+=this.getOrder()!=null?1:0;
        facts+=this.getNation()!=null?1:0;
        facts+=this.getColor()!=null?1:0;
        facts+=this.getAnimal()!=null?1:0;
        facts+=this.getCigarette()!=null?1:0;
        facts+=this.getDrink()!=null?1:0;
        return facts;
    }

    public int getCommonFactsCount(LineOfPuzzle lineOfFacts){
        int ordrCmp = (this.order!=null && lineOfFacts.getOrder()!= null &&
                       this.order.intValue()== lineOfFacts.getOrder().intValue())?1:0;

        int natnCmp = (this.nation!=null && lineOfFacts.getNation()!= null &&
                       this.nation.equalsIgnoreCase(lineOfFacts.getNation()))?1:0;

        int colrCmp = (this.color!=null && lineOfFacts.getColor()!= null &&
                       this.color.equalsIgnoreCase(lineOfFacts.getColor()))?1:0;

        int petsCmp = (this.animal!=null && (lineOfFacts.getAnimal()!= null &&
                       this.animal.equalsIgnoreCase(lineOfFacts.getAnimal())))?1:0;

        int cigrCmp = (this.cigarette!=null && lineOfFacts.getCigarette()!= null &&
                       this.cigarette.equalsIgnoreCase(lineOfFacts.getCigarette()))?1:0;

        int drnkCmp = (this.drink!=null && lineOfFacts.getDrink()!= null &&
                       this.drink.equalsIgnoreCase(lineOfFacts.getDrink()))?1:0;

        int result = (ordrCmp + natnCmp + colrCmp + petsCmp + cigrCmp + drnkCmp);

        return result;
    }

    public void addUndefindedNeighbor(LineOfPuzzle newNeighbor){
        if (this.undefNeighbors==null)
            this.undefNeighbors = new PuzzleSet<>();

        this.undefNeighbors.add(newNeighbor);
    }

    public boolean hasUndefNeighbors(){
        return (this.undefNeighbors!=null);
    }

    public PuzzleSet<LineOfPuzzle> getUndefNeighbors(){
        return this.undefNeighbors;
    }

    public void setLeftNeighbor(LineOfPuzzle leftNeighbor){
        this.leftNeighbor = leftNeighbor;
        this.leftNeighbor.setOrder(this.order - 1);
    }

    public void setRightNeighbor(LineOfPuzzle rightNeighbor){
        this.rightNeighbor=rightNeighbor;
        this.rightNeighbor.setOrder(this.order + 1);
    }

    public boolean hasLeftNeighbor(){
        return (leftNeighbor!=null);
    }

    public LineOfPuzzle getLeftNeighbor(){
        return this.leftNeighbor;
    }

    public boolean hasNeighbor(int direction){
        if (direction < 0)
            return (leftNeighbor!=null);
        else
            return (rightNeighbor!=null);
    }

    public boolean hasRightNeighbor(){
        return (rightNeighbor!=null);
    }

    public LineOfPuzzle getRightNeighbor(){
        return this.rightNeighbor;
    }

    public LineOfPuzzle getNeighbor(int direction){
        if (direction < 0)
            return this.leftNeighbor;
        else
            return this.rightNeighbor;
    }

    public String getWholeLine() {
        String sLine = this.order + " - " +
                       this.nation + " - " +
                       this.color + " - " +
                       this.animal + " - " +
                       this.drink + " - " +
                       this.cigarette;
            return sLine;
    }

    @Override
    public int hashCode() {
        int sLine = (this.order + " - " +
                     this.nation + " - " +
                     this.color + " - " +
                     this.animal + " - " +
                     this.drink + " - " +
                     this.cigarette
                ).hashCode();
        return sLine;
    }

    public void merge(LineOfPuzzle mergedLine){
        if (this.order == null) this.order = mergedLine.order;
        if (this.nation == null) this.nation = mergedLine.nation;
        if (this.color == null) this.color = mergedLine.color;
        if (this.animal == null) this.animal = mergedLine.animal;
        if (this.drink == null) this.drink = mergedLine.drink;
        if (this.cigarette == null) this.cigarette = mergedLine.cigarette;
    }


    public LineOfPuzzle clone() {
        try {
            return (LineOfPuzzle) super.clone();
        } catch (CloneNotSupportedException e) {
            e.printStackTrace();
            throw new RuntimeException();
        }
    }
}

```


Second, create a class which express a set of puzzle lines.


```Java

package zebra;

import java.util.Iterator;
import java.util.LinkedHashSet;

public class PuzzleSet<T extends LineOfPuzzle> extends LinkedHashSet{
    private T t;

    private int countOfOne=0;
    private int countOfTwo=0;
    private int countOfThree=0;
    private int countOfFour=0;
    private int countOfFive=0;

    PuzzleSet() {
        super();
    }

    public void set(T t) { this.t = t; }

    public T get(int index) {
        return ((T)this.toArray()[index]);
    }

    public PuzzleSet<T> getSimilarLines(T searchLine) {
        PuzzleSet<T> puzzleSubSet = new PuzzleSet<>();
        for (Iterator<T> it = this.iterator(); it.hasNext();) {
            T lineOfPuzzle = it.next();

            if(lineOfPuzzle.getCommonFactsCount(searchLine) == searchLine.getFactsCount())
                puzzleSubSet.add(lineOfPuzzle);
        }
        if (puzzleSubSet.isEmpty())
            return null;

        return puzzleSubSet;
    }

    public boolean contains(T searchLine) {
        for (Iterator<T> it = this.iterator(); it.hasNext();) {
            T puzzleLine = it.next();

            if(puzzleLine.getCommonFactsCount(searchLine) == searchLine.getFactsCount())
                return true;
        }
        return false;
    }

    public boolean accepts(T searchLine) {
        int passed=0;
        int notpassed=0;

        for (Iterator<T> it = this.iterator(); it.hasNext();) {
            T puzzleSetLine = it.next();

            int lineFactsCnt = puzzleSetLine.getFactsCount();
            int comnFactsCnt = puzzleSetLine.getCommonFactsCount(searchLine);

            if( lineFactsCnt != comnFactsCnt && lineFactsCnt !=0 && comnFactsCnt !=0){
                notpassed++;
            }

            if( lineFactsCnt == comnFactsCnt)
                passed++;
        }
        return (passed >= 0 && notpassed == 0);
    }

    public void riseLineCountFlags(int lineOrderId){
         switch (lineOrderId){
             case 1: countOfOne++; break;
             case 2: countOfTwo++; break;
             case 3: countOfThree++; break;
             case 4: countOfFour++; break;
             case 5: countOfFive++; break;
             default:;
         }
     }

    public void clearLineCountFlags(){
        countOfOne=0;
        countOfTwo=0;
        countOfThree=0;
        countOfFour=0;
        countOfFive=0;
     }

    public int getLineCountByOrderId(int lineOrderId){
         switch (lineOrderId){
             case 1: return countOfOne;
             case 2: return countOfTwo;
             case 3: return countOfThree;
             case 4: return countOfFour;
             case 5: return countOfFive;
             default:return -1;
         }
    }
}

```


Third, create the main run class where there is a recursive call to check for rule sets.

```Java

package zebra;

import java.util.ArrayList;
import java.util.Iterator;

public class Puzzle {
    private static final ArrayList<Integer> orders = new ArrayList<>(5);
    private static final ArrayList<String> nations = new ArrayList<>(5);
    private static final ArrayList<String> animals = new ArrayList<>(5);
    private static final ArrayList<String> drinks = new ArrayList<>(5);
    private static final ArrayList<String> cigarettes = new ArrayList<>(5);
    private static final ArrayList<String> colors = new ArrayList<>(5);
    private static PuzzleSet<LineOfPuzzle> puzzleTable;
    static
    {
        // House orders
        orders.add(1);
        orders.add(2);
        orders.add(3);
        orders.add(4);
        orders.add(5);
        // Man nations
        nations.add("English");
        nations.add("Danish");
        nations.add("German");
        nations.add("Swedesh");
        nations.add("Norwegian");
        //Animals
        animals.add("Zebra");
        animals.add("Horse");
        animals.add("Birds");
        animals.add("Dog");
        animals.add("Cats");
        //Drinks
        drinks.add("Coffee");
        drinks.add("Tea");
        drinks.add("Beer");
        drinks.add("Water");
        drinks.add("Milk");
        //Smokes
        cigarettes.add("Pall Mall");
        cigarettes.add("Blend");
        cigarettes.add("Blue Master");
        cigarettes.add("Prince");
        cigarettes.add("Dunhill");
        //Colors
        colors.add("Red");
        colors.add("Green");
        colors.add("White");
        colors.add("Blue");
        colors.add("Yellow");
    }

    public static void main (String[] args){
        boolean validLine=true;
        puzzleTable = new PuzzleSet<>();

        //Rules
        LineOfPuzzle rule2 = new LineOfPuzzle(null, "English", "Red", null, null, null);
        LineOfPuzzle rule3 = new LineOfPuzzle(null, "Swedesh", null, "Dog", null, null);
        LineOfPuzzle rule4 = new LineOfPuzzle(null, "Danish", null, null, "Tea", null);
        LineOfPuzzle rule6 = new LineOfPuzzle(null, null, "Green", null, "Coffee", null);
        LineOfPuzzle rule7 = new LineOfPuzzle(null, null, null, "Birds", null, "Pall Mall");
        LineOfPuzzle rule8 = new LineOfPuzzle(null, null, "Yellow", null, null, "Dunhill");
        LineOfPuzzle rule9 = new LineOfPuzzle(3, null, null, null, "Milk", null);
        LineOfPuzzle rule10 = new LineOfPuzzle(1, "Norwegian", null, null, null, null);
        LineOfPuzzle rule13 = new LineOfPuzzle(null, null, null, null, "Beer", "Blue Master");
        LineOfPuzzle rule14 = new LineOfPuzzle(null, "German", null, null, null, "Prince");
        LineOfPuzzle rule15 = new LineOfPuzzle(2, null, "Blue", null, null, null);

        PuzzleSet<LineOfPuzzle> ruleSet = new PuzzleSet<>();
        ruleSet.add(rule2);
        ruleSet.add(rule3);
        ruleSet.add(rule4);
        ruleSet.add(rule6);
        ruleSet.add(rule7);
        ruleSet.add(rule8);
        ruleSet.add(rule9);
        ruleSet.add(rule10);
        ruleSet.add(rule13);
        ruleSet.add(rule14);
        ruleSet.add(rule15);

        //Creating all possible combination of a puzzle line.
        //The maximum number of lines is 5^^6 (15625).
        //Each combination line is checked against a set of knowing facts, thus
        //only a small number of line result at the end.
        for (Integer orderId : Puzzle.orders) {
            for (String nation : Puzzle.nations) {
                for (String color : Puzzle.colors) {
                    for (String animal : Puzzle.animals) {
                        for (String drink : Puzzle.drinks) {
                            for (String cigarette : Puzzle.cigarettes) {
                                LineOfPuzzle pzlLine = new LineOfPuzzle(orderId,
                                                                     nation,
                                                                     color,
                                                                     animal,
                                                                     drink,
                                                                     cigarette);
                                // Checking against a set of knowing facts
                                if (ruleSet.accepts(pzlLine)){
                                        // Adding rules of neighbors
                                        if (cigarette.equalsIgnoreCase("Blend")
                                                && (animal.equalsIgnoreCase("Cats")
                                                || drink.equalsIgnoreCase("Water")))
                                            validLine = false;

                                        if (cigarette.equalsIgnoreCase("Dunhill")
                                                && animal.equalsIgnoreCase("Horse"))
                                            validLine = false;

                                        if (validLine){
                                            puzzleTable.add(pzlLine);

                                            //set neighbors constraints
                                            if (color.equalsIgnoreCase("Green")){
                                                pzlLine.setRightNeighbor(new LineOfPuzzle(null, null, "White", null, null, null));
                                            }
                                            if (color.equalsIgnoreCase("White")){
                                                pzlLine.setLeftNeighbor(new LineOfPuzzle(null, null, "Green", null, null, null));
                                            }
                                            //
                                            if (animal.equalsIgnoreCase("Cats")
                                                    && !cigarette.equalsIgnoreCase("Blend") ){
                                                pzlLine.addUndefindedNeighbor(new LineOfPuzzle(null, null, null, null, null, "Blend"));
                                            }
                                            if (cigarette.equalsIgnoreCase("Blend")
                                                    && !animal.equalsIgnoreCase("Cats")){
                                                pzlLine.addUndefindedNeighbor(new LineOfPuzzle(null, null, null, "Cats", null, null));
                                            }
                                            //
                                            if (drink.equalsIgnoreCase("Water")
                                                    && !animal.equalsIgnoreCase("Cats")
                                                    && !cigarette.equalsIgnoreCase("Blend")){
                                                pzlLine.addUndefindedNeighbor(new LineOfPuzzle(null, null, null, null, null, "Blend"));
                                            }

                                            if (cigarette.equalsIgnoreCase("Blend")
                                                    && !drink.equalsIgnoreCase("Water")){
                                                pzlLine.addUndefindedNeighbor(new LineOfPuzzle(null, null, null, null, "Water", null));
                                            }
                                            //
                                            if (animal.equalsIgnoreCase("Horse")
                                                    && !cigarette.equalsIgnoreCase("Dunhill")){
                                                pzlLine.addUndefindedNeighbor(new LineOfPuzzle(null, null, null, null, null, "Dunhill"));
                                            }
                                            if (cigarette.equalsIgnoreCase("Dunhill")
                                                    && !animal.equalsIgnoreCase("Horse")){
                                                pzlLine.addUndefindedNeighbor(new LineOfPuzzle(null, null, null, "Horse", null, null));
                                            }
                                        }
                                        validLine = true;
                                }
                            } //cigarette end
                        } //drinks end
                    } //animal end
                } //color end
            } //nations end
        } //order end

        System.out.println("After general rule set validation, remains "+
                                                puzzleTable.size() + " lines.");

        for (Iterator<LineOfPuzzle> it = puzzleTable.iterator(); it.hasNext();){
            validLine=true;

            LineOfPuzzle lineOfPuzzle = it.next();

            if (lineOfPuzzle.hasLeftNeighbor()){
                LineOfPuzzle neighbor = lineOfPuzzle.getLeftNeighbor();
                if (neighbor.getOrder()<1 || neighbor.getOrder()>5){
                    validLine=false;
                    it.remove();

                }
            }
            if (validLine && lineOfPuzzle.hasRightNeighbor()){
                LineOfPuzzle neighbor = lineOfPuzzle.getRightNeighbor();
                if (neighbor.getOrder()<1 || neighbor.getOrder()>5){
                    it.remove();
                }
            }
        }

        System.out.println("After removing out of bound neighbors, remains " +
                                                puzzleTable.size() + " lines.");

        //Setting left and right neighbors
        for (Iterator<LineOfPuzzle> it = puzzleTable.iterator(); it.hasNext();) {
            LineOfPuzzle puzzleLine = it.next();

            if (puzzleLine.hasUndefNeighbors()){
                for (Iterator<LineOfPuzzle> it1 = puzzleLine.getUndefNeighbors().iterator(); it1.hasNext();) {
                    LineOfPuzzle leftNeighbor = it1.next();
                    LineOfPuzzle rightNeighbor = leftNeighbor.clone();

                    //make it left neighbor
                    leftNeighbor.setOrder(puzzleLine.getOrder()-1);
                    if (puzzleTable.contains(leftNeighbor)){
                        if (puzzleLine.hasLeftNeighbor())
                            puzzleLine.getLeftNeighbor().merge(leftNeighbor);
                        else
                            puzzleLine.setLeftNeighbor(leftNeighbor);
                    }
                    rightNeighbor.setOrder(puzzleLine.getOrder()+1);
                    if (puzzleTable.contains(rightNeighbor)){
                        if (puzzleLine.hasRightNeighbor())
                            puzzleLine.getRightNeighbor().merge(rightNeighbor);
                        else
                            puzzleLine.setRightNeighbor(rightNeighbor);
                    }
                }
            }
        }

        int iteration=1;
        int lastSize=0;

        //Recursively validate against neighbor rules
        while (puzzleTable.size()>5 && lastSize != puzzleTable.size()) {
            lastSize = puzzleTable.size();
            puzzleTable.clearLineCountFlags();

            recursiveSearch(null, puzzleTable, -1);

            ruleSet.clear();
            // Assuming we'll get at leas one valid line each iteration, we create
            // a set of new rules with lines which have no more then one instance of same OrderId.
            for (int i = 1; i < 6; i++) {
                if (puzzleTable.getLineCountByOrderId(i)==1)
                   ruleSet.addAll(puzzleTable.getSimilarLines(new LineOfPuzzle(i, null, null, null, null, null)));
            }

            for (Iterator<LineOfPuzzle> it = puzzleTable.iterator(); it.hasNext();) {
                LineOfPuzzle puzzleLine = it.next();

                if (!ruleSet.accepts(puzzleLine))
                    it.remove();
            }
            //
            System.out.println("After " + iteration + " recursive iteration, remains "
                                                + puzzleTable.size() + " lines");
            iteration+=1;
        }

        // Print the results
        System.out.println("-------------------------------------------");
        if (puzzleTable.size()==5){
            for (Iterator<LineOfPuzzle> it = puzzleTable.iterator(); it.hasNext();) {
            LineOfPuzzle puzzleLine = it.next();
                System.out.println(puzzleLine.getWholeLine());
            }
        }else
            System.out.println("Sorry, solution not found!");
    }

    // Recursively checks the input set to ensure each line has right neighbor.
    // Neighbors can be of three type, left, right or undefined.
    // Direction: -1 left, 0 undefined, 1 right
    private static boolean recursiveSearch(LineOfPuzzle pzzlNodeLine,
                                           PuzzleSet puzzleSet, int direction){
        boolean validLeaf = false;
        boolean hasNeighbor = false;
        PuzzleSet<LineOfPuzzle> puzzleSubSet = null;

        for (Iterator<LineOfPuzzle> it = puzzleSet.iterator(); it.hasNext();) {
            LineOfPuzzle pzzlLeafLine = it.next();
            validLeaf = false;

            hasNeighbor = pzzlLeafLine.hasNeighbor(direction);

            if (hasNeighbor){
                puzzleSubSet = puzzleTable.getSimilarLines(pzzlLeafLine.getNeighbor(direction));
                if (puzzleSubSet != null){
                    if (pzzlNodeLine !=null)
                        validLeaf = puzzleSubSet.contains(pzzlNodeLine);
                    else
                        validLeaf = recursiveSearch(pzzlLeafLine, puzzleSubSet, -1*direction);
                }
                else
                    validLeaf = false;
            }

            if (!validLeaf && pzzlLeafLine.hasNeighbor(-1*direction)){
                hasNeighbor = true;
                if (hasNeighbor){
                    puzzleSubSet = puzzleTable.getSimilarLines(pzzlLeafLine.getNeighbor(-1*direction));
                    if (puzzleSubSet != null){
                        if (pzzlNodeLine !=null)
                            validLeaf = puzzleSubSet.contains(pzzlNodeLine);
                        else
                            validLeaf = recursiveSearch(pzzlLeafLine, puzzleSubSet, direction);
                    }
                    else
                        validLeaf = false;
                }
            }

            if (pzzlNodeLine != null && validLeaf)
                return validLeaf;

            if (pzzlNodeLine == null && hasNeighbor && !validLeaf){
                it.remove();
            }

            if (pzzlNodeLine == null){
                if (hasNeighbor && validLeaf){
                     puzzleSet.riseLineCountFlags(pzzlLeafLine.getOrder());
                }
                if (!hasNeighbor){
                    puzzleSet.riseLineCountFlags(pzzlLeafLine.getOrder());
                }
            }
        }
        return validLeaf;
    }
}

```

{{out}}

```txt

After general rule set validation, remains 60 lines.
After removing out of bound neighbors, remains 52 lines.
After 1 recursive iteration, remains 17 lines
After 2 recursive iteration, remains 6 lines
After 3 recursive iteration, remains 5 lines
-------------------------------------------
1 - Norwegian - Yellow - Cats - Water - Dunhill
2 - Danish - Blue - Horse - Tea - Blend
3 - English - Red - Birds - Milk - Pall Mall
4 - German - Green - Zebra - Coffee - Prince
5 - Swedesh - White - Dog - Beer - Blue Master

```

--[[User:Vvalache|VValache]] ([[User talk:Vvalache|talk]]) 14:14, 3 October 2014 (UTC)


## jq

{{works with|jq|1.4}}
The main function presented here, zebra, generates all possible solutions.  By letting it run to
completion, we can see that there is only one solution to the
puzzle.  The program is fast because pruning takes place.  That is, the program implements a "generate-and-prune" strategy.

'''Part 1''': Generic filters for unification and matching

```jq

# Attempt to unify the input object with the specified object
def unify( object ):
  # Attempt to unify the input object with the specified tag:value
  def unify2(tag; value):
    if . == null then null
    elif .[tag] == value then .
    elif .[tag] == null then .[tag] = value
    else null
    end;
  reduce (object|keys[]) as $key
    (.; unify2($key; object[$key]) );

# Input: an array
# Output: if the i-th element can be made to satisfy the condition,
# then the updated array, otherwise empty.
def enforce(i; cond):
  if 0 <= i and i < length
  then
    (.[i] | cond) as $ans
    | if $ans then .[i] = $ans else empty end
  else empty
  end ;
```

'''Part 2''': Zebra Puzzle

```jq
# Each house is a JSON object of the form:
# { "number": _, "nation": _, "owns": _, "color": _, "drinks": _, "smokes": _}

# The list of houses is represented by an array of five such objects.

# Input: an array of objects representing houses.
# Output: [i, solution] where i is the entity unified with obj
# and solution is the updated array
def solve_with_index( obj ):
  . as $Houses
  | range(0; length) as $i
  | ($Houses[$i] | unify(obj)) as $H
  | if $H then $Houses[$i] = $H else empty end
  | [ $i, .] ;

def solve( object ):
  solve_with_index( object )[1];

def adjacent( obj1; obj2 ):
  solve_with_index(obj1) as $H
  | $H[1]
  | (enforce(  $H[0] - 1; unify(obj2) ),
     enforce(  $H[0] + 1; unify(obj2) )) ;

def left_right( obj1; obj2 ):
  solve_with_index(obj1) as $H
  | $H[1]
  | enforce(  $H[0] + 1; unify(obj2) ) ;


# All solutions by generate-and-test
def zebra:
  [range(0;5)] | map({"number": .})                        # Five houses

  | enforce( 0; unify( {"nation": "norwegian"} ) )
  | enforce( 2; unify( {"drinks": "milk"} ) )

  | solve( {"nation": "englishman",  "color": "red"} )
  | solve( {"nation": "swede", "owns": "dog"} )
  | solve( {"nation": "dane", "drinks": "tea"} )

  | left_right( {"color": "green"}; {"color": "white"})

  | solve( {"drinks": "coffee", "color": "green"} )
  | solve( {"smokes": "Pall Mall", "owns": "birds"} )
  | solve( {"color": "yellow", "smokes":  "Dunhill"} )

  | adjacent( {"smokes": "Blend" }; {"owns": "cats"} )
  | adjacent( {"owns": "horse"}; {"smokes": "Dunhill"})

  | solve( {"drinks": "beer", "smokes": "Blue Master"} )
  | solve( {"nation": "german", "smokes": "Prince"})

  | adjacent( {"nation": "norwegian"}; {"color": "blue"})
  | adjacent( {"drinks": "water"}; {"smokes": "Blend"})

  | solve( {"owns": "zebra"} )
;

zebra
```

{{Out}}
<div style="overflow:scroll; height:400px;">

```sh
$ time jq -n -f zebra.jq
[
  {
    "number": 0,
    "nation": "norwegian",
    "color": "yellow",
    "smokes": "Dunhill",
    "owns": "cats",
    "drinks": "water"
  },
  {
    "number": 1,
    "drinks": "tea",
    "nation": "dane",
    "smokes": "Blend",
    "owns": "horse",
    "color": "blue"
  },
  {
    "number": 2,
    "drinks": "milk",
    "color": "red",
    "nation": "englishman",
    "owns": "birds",
    "smokes": "Pall Mall"
  },
  {
    "number": 3,
    "color": "green",
    "drinks": "coffee",
    "nation": "german",
    "smokes": "Prince",
    "owns": "zebra"
  },
  {
    "number": 4,
    "nation": "swede",
    "owns": "dog",
    "color": "white",
    "drinks": "beer",
    "smokes": "Blue Master"
  }
]

# Times include compilation:
real	0m0.284s
user	0m0.260s
sys	0m0.005s
```
</div>


## Julia



```Julia

# Julia 1.0
using Combinatorics
function make(str, test )
  filter(test, collect( permutations(split(str))) )
end

men = make("danish english german norwegian swedish",
           x -> "norwegian" == x[1])

drinks = make("beer coffee milk tea water", x -> "milk" == x[3])

#Julia 1.0 compatible
colors = make("blue green red white yellow",
              x -> 1 == findfirst(c -> c == "white", x) - findfirst(c -> c == "green",x))

pets = make("birds cats dog horse zebra")

smokes = make("blend blue-master dunhill pall-mall prince")

function eq(x, xs, y, ys)
  findfirst(xs, x) == findfirst(ys, y)
end

function adj(x, xs, y, ys)
  1 == abs(findfirst(xs, x) - findfirst(ys, y))
end

function print_houses(n, pet, nationality, colors, drink, smokes)
  println("$n, $pet,    $nationality       $colors       $drink    $smokes")
end

for m = men, c = colors
  if eq("red",c, "english",m) && adj("norwegian",m, "blue",c)
    for d = drinks
      if eq("danish",m, "tea",d) && eq("coffee",d,"green",c)
        for s = smokes
          if eq("yellow",c,"dunhill",s) &&
             eq("blue-master",s,"beer",d) &&
             eq("german",m,"prince",s)
            for p = pets
              if eq("birds",p,"pall-mall",s) &&
                 eq("swedish",m,"dog",p) &&
                 adj("blend",s,"cats",p) &&
                 adj("horse",p,"dunhill",s)
                println("Zebra is owned by ", m[findfirst(c -> c == "zebra", p)])
                println("Houses:")
                for house_num in 1:5
                  print_houses(house_num,p[house_num],m[house_num],c[house_num],d[house_num],s[house_num])
                end
              end
            end
          end
        end
      end
    end
  end
end


```


{{out}}

```txt

Zebra is owned by german
Houses:
1, cats,    norwegian       yellow       water    dunhill
2, horse,    danish       blue       tea    blend
3, birds,    english       red       milk    pall-mall
4, zebra,    german       green       coffee    prince
5, dog,    swedish       white       beer    blue-master

```



## Kotlin


```scala
// version 1.1.3

fun nextPerm(perm: IntArray): Boolean {
    val size = perm.size
    var k = -1
    for (i in size - 2 downTo 0) {
        if (perm[i] < perm[i + 1]) {
            k = i
            break
        }
    }
    if (k == -1) return false  // last permutation
    for (l in size - 1 downTo k) {
        if (perm[k] < perm[l]) {
           val temp = perm[k]
           perm[k] = perm[l]
           perm[l] = temp
           var m = k + 1
           var n = size - 1
           while (m < n) {
               val temp2 = perm[m]
               perm[m++] = perm[n]
               perm[n--] = temp2
           }
           break
        }
    }
    return true
}

fun check(a1: Int, a2: Int, v1: Int, v2: Int): Boolean {
    for (i in 0..4)
        if (p[a1][i] == v1) return p[a2][i] == v2
    return false
}

fun checkLeft(a1: Int, a2: Int, v1: Int, v2: Int): Boolean {
    for (i in 0..3)
        if (p[a1][i] == v1) return p[a2][i + 1] == v2
    return false
}

fun checkRight(a1: Int, a2: Int, v1: Int, v2: Int): Boolean {
    for (i in 1..4)
        if (p[a1][i] == v1) return p[a2][i - 1] == v2
    return false
}

fun checkAdjacent(a1: Int, a2: Int, v1: Int, v2: Int): Boolean {
    return checkLeft(a1, a2, v1, v2) || checkRight(a1, a2, v1, v2)
}

val colors  = listOf("Red", "Green", "White", "Yellow", "Blue")
val nations = listOf("English", "Swede", "Danish", "Norwegian", "German")
val animals = listOf("Dog", "Birds", "Cats", "Horse", "Zebra")
val drinks  = listOf("Tea", "Coffee", "Milk", "Beer", "Water")
val smokes  = listOf("Pall Mall", "Dunhill", "Blend", "Blue Master", "Prince")

val p = Array(120) { IntArray(5) { -1 } } //  stores all permutations of numbers 0..4

fun fillHouses(): Int {
    var solutions = 0
    for (c in 0..119) {
        if (!checkLeft(c, c, 1, 2)) continue                      // C5 : Green left of white
        for (n in 0..119) {
            if (p[n][0] != 3) continue                            // C10: Norwegian in First
            if (!check(n, c, 0, 0)) continue                      // C2 : English in Red
            if (!checkAdjacent(n, c, 3, 4)) continue              // C15: Norwegian next to Blue
            for (a in 0..119) {
                if (!check(a, n, 0, 1)) continue                  // C3 : Swede has Dog
                for (d in 0..119) {
                    if (p[d][2] != 2) continue                    // C9 : Middle drinks Milk
                    if (!check(d, n, 0, 2)) continue              // C4 : Dane drinks Tea
                    if (!check(d, c, 1, 1)) continue              // C6 : Green drinks Coffee
                    for (s in 0..119) {
                        if (!check(s, a, 0, 1)) continue          // C7 : Pall Mall has Birds
                        if (!check(s, c, 1, 3)) continue          // C8 : Yellow smokes Dunhill
                        if (!check(s, d, 3, 3)) continue          // C13: Blue Master drinks Beer
                        if (!check(s, n, 4, 4)) continue          // C14: German smokes Prince
                        if (!checkAdjacent(s, a, 2, 2)) continue  // C11: Blend next to Cats
                        if (!checkAdjacent(s, a, 1, 3)) continue  // C12: Dunhill next to Horse
                        if (!checkAdjacent(s, d, 2, 4)) continue  // C16: Blend next to Water
                        solutions++
                        printHouses(c, n, a, d, s)
                    }
                }
            }
        }
    }
    return solutions
}

fun printHouses(c: Int, n: Int, a: Int, d: Int, s: Int) {
    var owner: String = ""
    println("House  Color   Nation     Animal  Drink   Smokes")
    println("
### ==  ======  =========  ======  ======  ========
")
    for (i in 0..4) {
        val f = "%3d    %-6s  %-9s  %-6s  %-6s  %-11s\n"
        System.out.printf(f, i + 1, colors[p[c][i]], nations[p[n][i]], animals[p[a][i]], drinks[p[d][i]], smokes[p[s][i]])
        if (animals[p[a][i]] == "Zebra") owner = nations[p[n][i]]
    }
    println("\nThe $owner owns the Zebra\n")
}

fun main(args: Array<String>) {
    val perm = IntArray(5) { it }
    for (i in 0..119) {
        for (j in 0..4) p[i][j] = perm[j]
        nextPerm(perm)
    }
    val solutions = fillHouses()
    val plural = if (solutions == 1) "" else "s"
    println("$solutions solution$plural found")
}
```


{{out}}

```txt

House  Color   Nation     Animal  Drink   Smokes

### ==  ======  =========  ======  ======  ========

  1    Yellow  Norwegian  Cats    Water   Dunhill
  2    Blue    Danish     Horse   Tea     Blend
  3    Red     English    Birds   Milk    Pall Mall
  4    Green   German     Zebra   Coffee  Prince
  5    White   Swede      Dog     Beer    Blue Master

The German owns the Zebra

1 solution found

```



## Logtalk

The Logtalk distribution includes a solution for a variant of this puzzle (here reproduced with permission):


```logtalk

/* Houses logical puzzle: who owns the zebra and who drinks water?

     1) Five colored houses in a row, each with an owner, a pet, cigarettes, and a drink.
     2) The English lives in the red house.
     3) The Spanish has a dog.
     4) They drink coffee in the green house.
     5) The Ukrainian drinks tea.
     6) The green house is next to the white house.
     7) The Winston smoker has a serpent.
     8) In the yellow house they smoke Kool.
     9) In the middle house they drink milk.
    10) The Norwegian lives in the first house from the left.
    11) The Chesterfield smoker lives near the man with the fox.
    12) In the house near the house with the horse they smoke Kool.
    13) The Lucky Strike smoker drinks juice.
    14) The Japanese smokes Kent.
    15) The Norwegian lives near the blue house.

Who owns the zebra and who drinks water?
*/

:- object(houses).

    :- public(houses/1).
    :- mode(houses(-list), one).
    :- info(houses/1, [
        comment is 'Solution to the puzzle.',
        argnames is ['Solution']
    ]).

    :- public(print/1).
    :- mode(print(+list), one).
    :- info(print/1, [
        comment is 'Pretty print solution to the puzzle.',
        argnames is ['Solution']
    ]).

    houses(Solution) :-
        template(Solution),                                                 %  1
        member(h(english, _, _, _, red), Solution),                         %  2
        member(h(spanish, dog, _, _, _), Solution),                         %  3
        member(h(_, _, _, coffee, green), Solution),                        %  4
        member(h(ukrainian, _, _, tea, _), Solution),                       %  5
        next(h(_, _, _, _, green), h(_, _, _, _, white), Solution),         %  6
        member(h(_, snake, winston, _, _), Solution),                       %  7
        member(h(_, _, kool, _, yellow), Solution),                         %  8
        Solution = [_, _, h(_, _, _, milk, _), _, _],                       %  9
        Solution = [h(norwegian, _, _, _, _)| _],                           % 10
        next(h(_, fox, _, _, _), h(_, _, chesterfield, _, _), Solution),    % 11
        next(h(_, _, kool, _, _), h(_, horse, _, _, _), Solution),          % 12
        member(h(_, _, lucky, juice, _), Solution),                         % 13
        member(h(japonese, _, kent, _, _), Solution),                       % 14
        next(h(norwegian, _, _, _, _), h(_, _, _, _, blue), Solution),      % 15
        member(h(_, _, _, water, _), Solution),      % one of them drinks water
        member(h(_, zebra, _, _, _), Solution).      % one of them owns a zebra

    print([]).
    print([House| Houses]) :-
        write(House), nl,
        print(Houses).

    % h(Nationality, Pet, Cigarette, Drink, Color)
    template([h(_, _, _, _, _), h(_, _, _, _, _), h(_, _, _, _, _), h(_, _, _, _, _), h(_, _, _, _, _)]).

    member(A, [A, _, _, _, _]).
    member(B, [_, B, _, _, _]).
    member(C, [_, _, C, _, _]).
    member(D, [_, _, _, D, _]).
    member(E, [_, _, _, _, E]).

    next(A, B, [A, B, _, _, _]).
    next(B, C, [_, B, C, _, _]).
    next(C, D, [_, _, C, D, _]).
    next(D, E, [_, _, _, D, E]).

:- end_object.

```


Sample query:

```text

| ?- houses::(houses(S), print(S)).
h(norwegian,fox,kool,water,yellow)
h(ukrainian,horse,chesterfield,tea,blue)
h(english,snake,winston,milk,red)
h(japonese,zebra,kent,coffee,green)
h(spanish,dog,lucky,juice,white)

S = [h(norwegian,fox,kool,water,yellow),h(ukrainian,horse,chesterfield,tea,blue),h(english,snake,winston,milk,red),h(japonese,zebra,kent,coffee,green),h(spanish,dog,lucky,juice,white)]

```



## Mathematica

This creates a table that has 5 columns, and 25 rows. We fill the table; each column is the same and equal to all the options joined together in blocks of 5:

```txt
 		1		2		3		4		5
colors		Blue		Blue		Blue		Blue		Blue
colors		Green		Green		Green		Green		Green
colors		Red		Red		Red		Red		Red
colors		White		White		White		White		White
colors		Yellow		Yellow		Yellow		Yellow		Yellow
nationality	Dane		Dane		Dane		Dane		Dane
nationality	English		English		English		English		English
nationality	German		German		German		German		German
nationality	Norwegian	Norwegian	Norwegian	Norwegian	Norwegian
nationality	Swede		Swede		Swede		Swede		Swede
beverage	Beer		Beer		Beer		Beer		Beer
beverage	Coffee		Coffee		Coffee		Coffee		Coffee
beverage	Milk		Milk		Milk		Milk		Milk
beverage	Tea		Tea		Tea		Tea		Tea
beverage	Water		Water		Water		Water		Water
animal		Birds		Birds		Birds		Birds		Birds
animal		Cats		Cats		Cats		Cats		Cats
animal		Dog		Dog		Dog		Dog		Dog
animal		Horse		Horse		Horse		Horse		Horse
animal		Zebra		Zebra		Zebra		Zebra		Zebra
smoke		Blend		Blend		Blend		Blend		Blend
smoke		Blue Master	Blue Master	Blue Master	Blue Master	Blue Master
smoke		Dunhill		Dunhill		Dunhill		Dunhill		Dunhill
smoke		Pall Mall	Pall Mall	Pall Mall	Pall Mall	Pall Mall
smoke		Prince		Prince		Prince		Prince		Prince
```

This should be read as follows: Each column shows (in blocks of 5) the possible candidates of each kind (beverage, animal, smoke...)
We solve it now in a 'sudoku' way: We remove candidates iteratively until we are left with 1 candidate of each kind for each house.

```Mathematica
ClearAll[EliminatePoss, FilterPuzzle]
EliminatePoss[ct_, key1_, key2_] := Module[{t = ct, poss1, poss2, poss, notposs},
  poss1 = Position[t, key1];
  poss2 = Position[t, key2];
  poss = Intersection[Last /@ poss1, Last /@ poss2];
  notposs = Complement[Range[5], poss];
  poss1 = Select[poss1, MemberQ[notposs, Last[#]] &];
  poss2 = Select[poss2, MemberQ[notposs, Last[#]] &];
  t = ReplacePart[t, poss1 -> Null];
  t = ReplacePart[t, poss2 -> Null];
  t
]
FilterPuzzle[tbl_] := Module[{t = tbl, poss1, poss2, poss, notposs, rows, columns, vals, sets, delpos},
  t = EliminatePoss[t, "English", "Red"]; (*2. The English man lives in the red house. *)
  t = EliminatePoss[t, "Swede", "Dog"]; (* 3. The Swede has a dog. *)
  t = EliminatePoss[t, "Dane", "Tea"]; (* 4. The Dane drinks tea. *)
  t = EliminatePoss[t, "Green", "Coffee"]; (* 6. They drink coffee in the green house. *)
  t = EliminatePoss[t, "Pall Mall", "Birds"]; (* 7. The man who smokes Pall Mall has birds.*)
  t = EliminatePoss[t, "Yellow", "Dunhill"]; (* 8. In the yellow house they smoke Dunhill. *)
  t = EliminatePoss[t, "Blue Master", "Beer"]; (*13. The man who smokes Blue Master drinks beer. *)
  t = EliminatePoss[t, "German", "Prince"]; (* 14. The German smokes Prince. *)

  (* 9. In the middle house they drink milk. *)
  poss = Position[t, "Milk"];
  delpos = Select[poss, #[[2]] != 3 &];
  t = ReplacePart[t, delpos -> Null];

  (* 10. The Norwegian lives in the first house. *)
  poss = Position[t, "Norwegian"];
  delpos = Select[poss, #[[2]] != 1 &];
  t = ReplacePart[t, delpos -> Null];

  (* 15. The Norwegian lives next to the blue house.*)
  poss1 = Position[t, "Norwegian"];
  poss2 = Position[t, "Blue"];
  poss = Tuples[{poss1, poss2}];
  poss = Select[poss, #[[1, 2]] + 1 == #[[2, 2]] \[Or] #[[1, 2]] - 1 == #[[2, 2]] &]\[Transpose];
  delpos = Complement[poss1, poss[[1]]];
  t = ReplacePart[t, delpos -> Null];
  delpos = Complement[poss2, poss[[2]]];
  t = ReplacePart[t, delpos -> Null];

  (* 5. The green house is immediately to the left of the white house. *)
  poss1 = Position[t, "Green"];
  poss2 = Position[t, "White"];
  poss = Tuples[{poss1, poss2}];
  poss = Select[poss, #[[1, 2]] + 1 == #[[2, 2]] &]\[Transpose];
  delpos = Complement[poss1, poss[[1]]];
  t = ReplacePart[t, delpos -> Null];
  delpos = Complement[poss2, poss[[2]]];
  t = ReplacePart[t, delpos -> Null];

  (*11. The man who smokes Blend lives in the house next to the house with cats.*)
  poss1 = Position[t, "Blend"];
  poss2 = Position[t, "Cats"];
  poss = Tuples[{poss1, poss2}];
  poss = Select[poss, #[[1, 2]] + 1 == #[[2, 2]] \[Or] #[[1, 2]] - 1 == #[[2, 2]] &]\[Transpose];
  delpos = Complement[poss1, poss[[1]]];
  t = ReplacePart[t, delpos -> Null];
  delpos = Complement[poss2, poss[[2]]];
  t = ReplacePart[t, delpos -> Null];

  (* 12. In a house next to the house where they have a horse, they smoke Dunhill. *)
  poss1 = Position[t, "Horse"];
  poss2 = Position[t, "Dunhill"];
  poss = Tuples[{poss1, poss2}];
  poss = Select[poss, #[[1, 2]] + 1 == #[[2, 2]] \[Or] #[[1, 2]] - 1 == #[[2, 2]] &]\[Transpose];
  delpos = Complement[poss1, poss[[1]]];
  t = ReplacePart[t, delpos -> Null];
  delpos = Complement[poss2, poss[[2]]];
  t = ReplacePart[t, delpos -> Null];

  (* 16. They drink water in a house next to the house where they smoke Blend. *)
  poss1 = Position[t, "Water"];
  poss2 = Position[t, "Blend"];
  poss = Tuples[{poss1, poss2}];
  poss = Select[poss, #[[1, 2]] + 1 == #[[2, 2]] \[Or] #[[1, 2]] - 1 == #[[2, 2]] &]\[Transpose];
  delpos = Complement[poss1, poss[[1]]];
  t = ReplacePart[t, delpos -> Null];
  delpos = Complement[poss2, poss[[2]]];
  t = ReplacePart[t, delpos -> Null];

  (*General rule 1 in a line => cross out vertical and horizontal lines*)
  (* 1 in a row*)
  vals = Select[t, Count[#, Null] == 4 &];
  vals = DeleteCases[Flatten[vals], Null];
  poss = Flatten[Position[t, #] & /@ vals, 1];
  delpos = With[{r = First[#], c = Last[#]}, {#, c} & /@ (Range[-4, 0] + Ceiling[r, 5])] & /@ poss; (*delete in columns*)
  delpos = Flatten[MapThread[DeleteCases, {delpos, poss}], 1];
  t = ReplacePart[t, delpos -> Null];

  (* 1 in a column*)
  sets = Flatten[Table[{i + k*5, j}, {k, 0, 4}, {j, 1, 5}, {i, 1, 5}],1];
  sets = {#, Extract[t, #]} & /@ sets;
  sets = Select[sets, Count[#[[2]], Null] == 4 &];
  sets = Flatten[Transpose /@ sets, 1];
  sets = DeleteCases[sets, {{_, _}, Null}];
  delpos = sets[[All, 1]];(*delete in rows*)
  delpos = With[{r = First[#], c = Last[#]}, {r, #} & /@ (DeleteCases[Range[5], c])] & /@ delpos;
  delpos = Flatten[delpos, 1];
  t = ReplacePart[t, delpos -> Null];

  t
]
colors = {"Blue", "Green", "Red", "White", "Yellow"};
nationality = {"Dane", "English", "German", "Norwegian", "Swede"};
beverage = {"Beer", "Coffee", "Milk", "Tea", "Water"};
animal = {"Birds", "Cats", "Dog", "Horse", "Zebra"};
smoke = {"Blend", "Blue Master", "Dunhill", "Pall Mall", "Prince"};
vals = {colors, nationality, beverage, animal, smoke};
bigtable = Join @@ (ConstantArray[#, 5]\[Transpose] & /@ vals);

bigtable = FixedPoint[FilterPuzzle, bigtable];
TableForm[DeleteCases[bigtable\[Transpose], Null, \[Infinity]], TableHeadings -> {Range[5], None}]
```

Using the command FixedPoint, we iteratively filter out these candidates, until we are (hopefully) left with 1 candidate per kind per house.
{{out}}

```txt
1		Yellow		Norwegian		Water		Cats		Dunhill
2		Blue		Dane		Tea		Horse		Blend
3		Red		English		Milk		Birds		Pall Mall
4		Green		German		Coffee		Zebra		Prince
5		White		Swede		Beer		Dog		Blue Master
```



## MiniZinc


```MiniZinc

%Solve Zebra Puzzle. Nigel Galloway, August 27th., 2019
include "alldifferent.mzn";
enum N={English,Swedish,Danish,German,Norwegian};
enum I={Tea,Coffee,Milk,Beer,Water};
enum G={Dog,Birds,Cats,Horse,Zebra};
enum E={Red,Green,White,Blue,Yellow};
enum L={PallMall,Dunhill,BlueMaster,Prince,Blend};
array[1..5] of var N: Nz; constraint alldifferent(Nz); constraint Nz[1]=Norwegian;                   %The Norwegian lives in the first house.
array[1..5] of var I: Iz; constraint alldifferent(Iz); constraint Iz[3]=Milk;                        %In the middle house they drink milk.
array[1..5] of var G: Gz; constraint alldifferent(Gz);
array[1..5] of var E: Ez; constraint alldifferent(Ez);
array[1..5] of var L: Lz; constraint alldifferent(Lz);
constraint exists(n in 1..5)(Nz[n]=English /\ Ez[n]=Red);                                            %The English man lives in the red house
constraint exists(n in 1..5)(Nz[n]=Swedish /\ Gz[n]=Dog);                                            %The Swede has a dog.
constraint exists(n in 1..5)(Nz[n]=Danish  /\ Iz[n]=Tea);                                            %The Dane drinks tea.
constraint exists(n in 1..4)(Ez[n]=Green /\ Ez[n+1]=White);                                          %The green house is immediately to the left of the white house.
constraint exists(n in 1..5)(Ez[n]=Green /\ Iz[n]=Coffee);                                           %They drink coffee in the green house.
constraint exists(n in 1..5)(Lz[n]=PallMall /\ Gz[n]=Birds);                                         %The man who smokes Pall Mall has birds
constraint exists(n in 1..5)(Ez[n]=Yellow /\ Lz[n]=Dunhill);                                         %In the yellow house they smoke Dunhill.
constraint exists(n in 1..4)((Lz[n]=Blend /\ Gz[n+1]=Cats) \/ (Lz[n+1]=Blend /\ Gz[n]=Cats));        %The man who smokes Blend lives in the house next to the house with cats.
constraint exists(n in 1..4)((Gz[n]=Horse /\ Lz[n+1]=Dunhill) \/ (Gz[n+1]=Horse /\ Lz[n]=Dunhill));  %In a house next to the house where they have a horse, they smoke Dunhill.
constraint exists(n in 1..5)(Lz[n]=BlueMaster /\ Iz[n]=Beer);                                        %The man who smokes Blue Master drinks beer.
constraint exists(n in 1..5)(Nz[n]=German /\ Lz[n]=Prince);                                          %The German smokes Prince.
constraint exists(n in 1..4)((Nz[n]=Norwegian /\ Ez[n+1]=Blue) \/ (Nz[n+1]=Norwegian /\ Ez[n]=Blue));%The Norwegian lives next to the blue house.
constraint exists(n in 1..4)((Lz[n]=Blend /\ Iz[n+1]=Water) \/ (Lz[n+1]=Blend /\ Iz[n]=Water));      %They drink water in a house next to the house where they smoke Blend.
var 1..5: n;
constraint Gz[n]=Zebra;
solve satisfy;
output ["The "++show(Nz[n])++" owns the zebra"++"\n\n"++show(Nz)++"\n"++show(Iz)++"\n"++show(Gz)++"\n"++show(Ez)++"\n"++show(Lz)++"\n"];

```

{{out}}

```txt

The German owns the zebra

[Norwegian, Danish, English, German, Swedish]
[Water, Tea, Milk, Coffee, Beer]
[Cats, Horse, Birds, Zebra, Dog]
[Yellow, Blue, Red, Green, White]
[Dunhill, Blend, PallMall, Prince, BlueMaster]
----------

```


## Nial


{{works with|Q'Nial Version 6.3}}

I put a backslash ( \ ) at the end of many lines so that the code can
be pasted into the REPL.


```Nial

remove is op x xs {filter (not (x =)) xs}

append_map is transformer func op seq { \
  reduce (op x xs { (func x) link xs}) (seq append []) }

permutations is op seq { \
  if empty seq then [[]] else \
    (append_map \
      (op head {each (op tail {head hitch tail}) \
                     (permutations (remove head seq))}) \
      seq) \
  endif}

f is find
tokenize is op str{string_split ' ' str}
mk is tr pred op str {filter pred permutations tokenize str}
eq is op x xs y ys{f x xs = f y ys}
adj is op x xs y ys{1 = abs(f x xs - f y ys)}

run is  { \
  men := mk (op xs {0 = f 'norwegian' xs}) \
    'danish english german norwegian swedish'; \
  colors := mk (op xs {1 = ((f 'white' xs) - (f 'green' xs))}) \
    'blue green red white yellow'; \
  drinks := mk (op xs {2 = f 'milk' xs}) 'beer coffee milk tea water'; \
  pets := mk (op xs {l}) 'birds cats dog horse zebra'; \
  smokes := mk (op xs {l}) 'blend blue-master dunhill pall-mall prince'; \
  for m with men do \
    for c with colors do \
      if  (eq 'english' m 'red' c) and \
          (adj 'norwegian' m 'blue' c) then \
        for d with drinks do \
          if  (eq 'danish' m 'tea' d) and \
              (eq 'coffee' d 'green' c) then \
            for s with smokes do \
              if  (eq 'yellow' c 'dunhill' s) and \
                  (eq 'blue-master' s 'beer' d) and \
                  (eq 'german' m 'prince' s) then \
                for p with pets do \
                  if  (eq 'birds' p 'pall-mall' s) and \
                      (eq 'swedish' m 'dog' p) and \
                      (adj 'blend' s 'cats' p) and \
                      (adj 'horse' p 'dunhill' s) then \
                    write (0 blend (p m c d s)) \
                  endif \
                endfor \
              endif \
            endfor \
          endif \
        endfor \
      endif \
    endfor \
  endfor }

abs(time - (run; time))

```


{{out}}

```txt

+-----------------------------------------+
|cats |norwegian|yellow|water |dunhill    |
+-----+---------+------+------+-----------|
|horse|danish   |blue  |tea   |blend      |
+-----+---------+------+------+-----------|
|birds|english  |red   |milk  |pall-mall  |
+-----+---------+------+------+-----------|
|zebra|german   |green |coffee|prince     |
+-----+---------+------+------+-----------|
|dog  |swedish  |white |beer  |blue-master|
+-----------------------------------------+
0.03

```


## Pari/Gp


```pari/gp

perm(arr) = {
n=#arr;i=n-1;
while(i > -1,if (arr[i] < arr[i+1],break);i--);
j=n;
while(arr[j]<= arr[i],j -=1);
tmp = arr[i] ;arr[i]=arr[j];arr[j]=tmp;
i +=1; j = n;
while(i < j ,tmp = arr[i] ;arr[i]=arr[j];arr[j]=tmp;
i +=1; j -=1);
return(arr);
}
perms(arr)={
n=#arr;
result = List();
listput(result,arr);
for(i=1,n!-1,arr=perm(arr);listput(result,arr));
return(result);
}

adj(x,xs,y,ys)={
  abs(select(z->z==x,xs,1)[1] - select(z->z==y,ys,1)[1])==1;
}
eq(x,xs,y,ys)={
   select(z->z==x,xs,1) == select(z->z==y,ys,1);
}

colors =Vec(perms( ["Blue", "Green", "Red", "White", "Yellow"]));;
drinks  =Vec(perms( ["Beer", "Coffee", "Milk", "Tea", "Water"]));;
nations =Vec(perms( ["Denmark", "England", "Germany", "Norway", "Sweden"]));;
smokes  =Vec(perms( ["Blend", "BlueMaster", "Dunhill", "PallMall", "Prince"]));;
pets =Vec(perms( ["Birds", "Cats", "Dog", "Horse", "Zebra"]));;;
colors= select(x->select(z->z=="White",x,1)[1] - select(z->z=="Green",x,1)[1]==1,colors);
drinks=select(x->x[3]=="Milk",drinks);
nations=select(x->x[1]=="Norway",nations);

for(n=1,#nations,for(c=1,#colors,\
if(eq("Red",colors[c],"England",nations[n]) && adj("Norway",nations[n],"Blue",colors[c]),\
for(d=1,#drinks,\
if(eq("Denmark",nations[n],"Tea",drinks[d])&& eq("Coffee",drinks[d],"Green",colors[c]),\
for(s=1,#smokes,\
if(eq("Yellow",colors[c],"Dunhill",smokes[s]) &&\
eq("BlueMaster",smokes[s],"Beer",drinks[d]) &&\
eq("Germany",nations[n],"Prince",smokes[s]),\
for(p=1,#pets,\
if(eq("Birds",pets[p],"PallMall",smokes[s]) &&\
eq("Sweden",nations[n],"Dog",pets[p]) &&\
adj("Blend",smokes[s],"Cats",pets[p]) &&\
adj("Horse",pets[p],"Dunhill",smokes[s]),\
print("Zebra is owned by ",nations[n][select(z->z=="Zebra",pets[p],1)[1]]);print();\
for(i=1,5,printf("House:%s %6s %10s %10s %10s %10s\n",i,colors[c][i],nations[n][i],pets[p][i],drinks[d][i],smokes[s][i]));\
)))))))));

```


{{out}}

```txt

Zebra is owned by Germany

House:1 Yellow     Norway       Cats      Water    Dunhill
House:2   Blue    Denmark      Horse        Tea      Blend
House:3    Red    England      Birds       Milk   PallMall
House:4  Green    Germany      Zebra     Coffee     Prince
House:5  White     Sweden        Dog       Beer BlueMaster


```



## Perl

Basically the same idea as C, though of course it's much easier to have Perl generate Perl code.

```perl
#!/usr/bin/perl

use utf8;
use strict;
binmode STDOUT, ":utf8";

my (@tgt, %names);
sub setprops {
	my %h = @_;
	my @p = keys %h;
	for my $p (@p) {
		my @v = @{ $h{$p} };
		@tgt = map(+{idx=>$_-1, map{ ($_, undef) } @p}, 1 .. @v)
			unless @tgt;
		$names{$_} = $p for @v;
	}
}

my $solve = sub {
	for my $i (@tgt) {
		printf("%12s", ucfirst($i->{$_} // "¿Qué?"))
				for reverse sort keys %$i;
		print "\n";
	}
	"there is only one"  # <--- change this to a false value to find all solutions (if any)
};

sub pair {
	my ($a, $b, @v) = @_;
	if ($a =~ /^(\d+)$/) {
		$tgt[$1]{ $names{$b} } = $b;
		return;
	}

	@v = (0) unless @v;
	my %allowed;
	$allowed{$_} = 1 for @v;

	my ($p1, $p2) = ($names{$a}, $names{$b});

	my $e = $solve;
	$solve = sub {		# <--- sorta like how TeX \let...\def macro
		my ($x, $y);

		($x) = grep { $_->{$p1} eq $a } @tgt;
		($y) = grep { $_->{$p2} eq $b } @tgt;

		$x and $y and
			return $allowed{ $x->{idx} - $y->{idx} } && $e->();

		my $try_stuff = sub {
			my ($this, $p, $v, $sign) = @_;
			for (@v) {
				my $i = $this->{idx} + $sign * $_;
				next unless $i >= 0 && $i < @tgt && !$tgt[$i]{$p};
				local $tgt[$i]{$p} = $v;
				$e->() and return 1;
			}
			return
		};

		$x and return $try_stuff->($x, $p2, $b, 1);
		$y and return $try_stuff->($y, $p1, $a, -1);

		for $x (@tgt) {
			next if $x->{$p1};
			local $x->{$p1} = $a;
			$try_stuff->($x, $p2, $b, 1) and return 1;
		}
	};
}

# ---- above should be generic for all similar puzzles ---- #

# ---- below: per puzzle setup ---- #
# property names and values
setprops (
	# Svensk n. a Swede, not a swede (kålrot).
	# AEnglisk (from middle Viking "Æŋløsåksen") n. a Brit.
	'Who'	=> [ qw(Deutsch Svensk Norske Danske AEnglisk) ],
	'Pet'	=> [ qw(birds dog horse zebra cats) ],
	'Drink'	=> [ qw(water tea milk beer coffee) ],
	'Smoke'	=> [ qw(dunhill blue_master prince blend pall_mall) ],
	'Color'	=> [ qw(red green yellow white blue) ]
);

# constraints
pair qw( AEnglisk red );
pair qw( Svensk dog );
pair qw( Danske tea );
pair qw( green white 1 );	# "to the left of" can mean either 1 or -1: ambiguous
pair qw( coffee green );
pair qw( pall_mall birds );
pair qw( yellow dunhill );
pair qw( 2 milk );
pair qw( 0 Norske );
pair qw( blend cats -1 1 );
pair qw( horse dunhill -1 1 );
pair qw( blue_master beer );	# Nicht das Deutsche Bier trinken? Huh.
pair qw( Deutsch prince );
pair qw( Norske blue -1 1 );
pair qw( water blend -1 1 );

$solve->();
```


Incidentally, the same logic can be used to solve the dwelling problem, if somewhat awkwardly:

```perl
...
# property names and values
setprops
	'Who'	=> [ qw(baker cooper fletcher miller smith) ],
	'Level'	=> [ qw(one two three four five) ];

# constraints
pair qw(0 one);
pair qw(1 two);
pair qw(2 three);
pair qw(3 four);
pair qw(4 five);
pair qw(baker five -4 -3 -2 -1 1 2 3 4);
pair qw(cooper one -4 -3 -2 -1 1 2 3 4);
pair qw(fletcher one -4 -3 -2 -1 1 2 3 4);
pair qw(fletcher five -4 -3 -2 -1 1 2 3 4);
pair qw(miller cooper -1 -2 -3 -4);
pair qw(smith fletcher 4 3 2 -2 -3 -4);
pair qw(cooper fletcher 4 3 2 -2 -3 -4);

$solve->();
```



## Perl 6

A rule driven approach:

```perl6
my Hash @houses = (1 .. 5).map: { %(:num($_)) }; # 1 there are five houses

my @facts = (
    { :nat<English>, :color<red> },      # 2 The English man lives in the red house.
    { :nat<Swede>, :pet<dog> },          # 3 The Swede has a dog.
    { :nat<Dane>, :drink<tea> },         # 4 The Dane drinks tea.
    { :color<green>, :Left-Of(:color<white>) }, # 5 the green house is immediately to the left of the white house
    { :drink<coffee>, :color<green> },   # 6 They drink coffee in the green house.
    { :smoke<Pall-Mall>, :pet<birds> },  # 7 The man who smokes Pall Mall has birds.
    { :color<yellow>, :smoke<Dunhill> }, # 8 In the yellow house they smoke Dunhill.
    { :num(3), :drink<milk> },           # 9 In the middle house they drink milk.
    { :num(1), :nat<Norwegian> },        # 10 The Norwegian lives in the first house.
    { :smoke<Blend>, :Next-To(:pet<cats>) }, # 11 The man who smokes Blend lives in the house next to the house with cats.
    { :pet<horse>, :Next-To(:smoke<Dunhill>) }, # 12 In a house next to the house where they have a horse, they smoke Dunhill.
    { :smoke<Blue-Master>, :drink<beer> }, # 13 The man who smokes Blue Master drinks beer.
    { :nat<German>, :smoke<Prince> },      # 14 The German smokes Prince.
    { :nat<Norwegian>, :Next-To(:color<blue>) }, # 15 The Norwegian lives next to the blue house.
    { :drink<water>, :Next-To(:smoke<Blend>) },  # 16 They drink water in a house next to the house where they smoke Blend.
    { :pet<zebra> }, # who owns this?
);

sub MAIN {
    for gather solve(@houses, @facts) {
        #-- output
        say .[0].pairs.sort.map(*.key.uc.fmt("%-9s")).join(' | ');
        say .pairs.sort.map(*.value.fmt("%-9s")).join(' | ')
            for .list;
        last; # stop after first solution
    }
}

#| found a solution that fits all the facts
multi sub solve(@solution, @facts [ ]) {
    take @solution;
}

#| extend a scenario to cover the next fact
multi sub solve(@scenario, [ $fact, *@facts ] is copy) {
    for gather choices(@scenario, |$fact) {
        solve(@$_, @facts)
    }
}

#| find all possible solutions for pairs of houses with
#| %a attributes, left of a house  with %b attributes
multi sub choices(@houses, :Left-Of(%b)!, *%a) {
    my @scenarios;
    for @houses {
        my $idx = .<num> - 1;
        if $idx > 0 && plausible(@houses[$idx-1], %a) && plausible(@houses[$idx], %b) {
            my @scenario = @houses.clone;
            @scenario[$idx-1] = %( %(@houses[$idx-1]), %a );
            @scenario[$idx] = %( %(@houses[$idx]), %b );
            take @scenario;
        }
    }
}

#| find all possible pairs of houses with %a attributes, either side
#! of a house  with %b attributes
multi sub choices(@houses, :Next-To(%b)!, *%a ) {
    choices(@houses, |%a, :Left-Of(%b) );
    choices(@houses, |%b, :Left-Of(%a) );
}

#| find all possible houses that match the given attributes
multi sub choices(@houses, *%fact) {
    for @houses.grep({plausible($_, %fact)}) -> $house {
        my @scenario = @houses.clone;
        my $idx = $house<num> - 1;
        @scenario[$idx] = %( %$house, %fact );
        take @scenario;
    }
}

#| plausible if doesn't conflict with anything
sub plausible(%house, %atts) {
    all %atts.keys.map: { (%house{$_}:!exists) || %house{$_} eq %atts{$_} };
}

```

{{out}}

```txt

COLOR     | DRINK     | NAT       | NUM       | PET       | SMOKE
yellow    | water     | Norwegian | 1         | cats      | Dunhill
blue      | tea       | Dane      | 2         | horse     | Blend
red       | milk      | English   | 3         | birds     | Pall-Mall
green     | coffee    | German    | 4         | zebra     | Prince
white     | beer      | Swede     | 5         | dog       | Blue-Master

```


Note: Facts can be shuffled by changing line 3 to <code>my @facts = pick *, (</code>. It seems to reliably find solutions, although execution times will vary (from under 1 sec up to about 10sec).


## Phix


```Phix
enum Colour, Nationality, Drink, Smoke, Pet
constant Colours = {"red","white","green","yellow","blue"},
         Nationalities = {"English","Swede","Dane","Norwegian","German"},
         Drinks = {"tea","coffee","milk","beer","water"},
         Smokes = {"Pall Mall","Dunhill","Blend","Blue Master","Prince"},
         Pets = {"dog","birds","cats","horse","zebra"},
         Sets = {Colours,Nationalities,Drinks,Smokes,Pets}

constant tagset5 = tagset(5)        -- {1,2,3,4,5}, oft-permuted
sequence perm = repeat(tagset5,5)   -- perm[1] is Colour of each house, etc

function house(integer i, string name)
    return find(find(name,Sets[i]),perm[i])
end function

function left_of(integer h1, integer h2)
    return (h1-h2)==1
end function

function next_to(integer h1, integer h2)
    return abs(h1-h2)==1
end function

procedure print_house(integer i)
    printf(1,"%d:%s,%s,%s,%s,%s\n",{i,
                                    Colours[perm[Colour][i]],
                                    Nationalities[perm[Nationality][i]],
                                    Drinks[perm[Drink][i]],
                                    Smokes[perm[Smoke][i]],
                                    Pets[perm[Pet][i]]})
end procedure

integer solutions = 0
sequence solperms = {}
atom t0 = time()
constant factorial5 = factorial(5)
for C=1 to factorial5 do
    perm[Colour] = permute(C,tagset5)
    if left_of(house(Colour,"green"),house(Colour,"white")) then
        for N=1 to factorial5 do
            perm[Nationality] = permute(N,tagset5)
            if house(Nationality,"Norwegian")==1
            and house(Nationality,"English")==house(Colour,"red")
            and next_to(house(Nationality,"Norwegian"),house(Colour,"blue")) then
                for D=1 to factorial5 do
                    perm[Drink] = permute(D,tagset5)
                    if house(Nationality,"Dane")==house(Drink,"tea")
                    and house(Drink,"coffee")==house(Colour,"green")
                    and house(Drink,"milk")==3 then
                        for S=1 to factorial5 do
                            perm[Smoke] = permute(S,tagset5)
                            if house(Colour,"yellow")==house(Smoke,"Dunhill")
                            and house(Nationality,"German")==house(Smoke,"Prince")
                            and house(Smoke,"Blue Master")==house(Drink,"beer")
                            and next_to(house(Drink,"water"),house(Smoke,"Blend")) then
                                for P=1 to factorial5 do
                                    perm[Pet] = permute(P,tagset5)
                                    if house(Nationality,"Swede")==house(Pet,"dog")
                                    and house(Smoke,"Pall Mall")==house(Pet,"birds")
                                    and next_to(house(Smoke,"Blend"),house(Pet,"cats"))
                                    and next_to(house(Pet,"horse"),house(Smoke,"Dunhill")) then
                                        for i=1 to 5 do
                                            print_house(i)
                                        end for
                                        solutions += 1
                                        solperms = append(solperms,perm)
                                    end if
                                end for
                            end if
                        end for
                    end if
                end for
            end if
        end for
    end if
end for
printf(1,"%d solution%s found (%3.3fs).\n",{solutions,iff(solutions>1,"s",""),time()-t0})
for i=1 to length(solperms) do
    perm = solperms[i]
    printf(1,"The %s owns the Zebra\n",{Nationalities[house(Pet,"zebra")]})
end for
```

{{out}}

```txt

1:yellow,Norwegian,water,Dunhill,cats
2:blue,Dane,tea,Blend,horse
3:red,English,milk,Pall Mall,birds
4:green,German,coffee,Prince,zebra
5:white,Swede,beer,Blue Master,dog
1 solution found (0.016s).
The German owns the Zebra

```



## PicoLisp


```PicoLisp
(be match (@House @Person @Drink @Pet @Cigarettes)
   (permute (red blue green yellow white) @House)
   (left-of @House white  @House green)

   (permute (Norwegian English Swede German Dane) @Person)
   (has @Person English  @House red)
   (equal @Person (Norwegian . @))
   (next-to @Person Norwegian  @House blue)

   (permute (tea coffee milk beer water) @Drink)
   (has @Drink tea  @Person Dane)
   (has @Drink coffee  @House green)
   (equal @Drink (@ @ milk . @))

   (permute (dog birds cats horse zebra) @Pet)
   (has @Pet dog  @Person Swede)

   (permute (Pall-Mall Dunhill Blend Blue-Master Prince) @Cigarettes)
   (has @Cigarettes Pall-Mall  @Pet birds)
   (has @Cigarettes Dunhill  @House yellow)
   (next-to @Cigarettes Blend  @Pet cats)
   (next-to @Cigarettes Dunhill  @Pet horse)
   (has @Cigarettes Blue-Master  @Drink beer)
   (has @Cigarettes Prince  @Person German)

   (next-to @Drink water  @Cigarettes Blend) )

(be has ((@A . @X) @A (@B . @Y) @B))
(be has ((@ . @X) @A (@ . @Y) @B)
   (has @X @A @Y @B) )

(be right-of ((@A . @X) @A (@ @B . @Y) @B))
(be right-of ((@ . @X) @A (@ . @Y) @B)
   (right-of @X @A @Y @B) )

(be left-of ((@ @A . @X) @A (@B . @Y) @B))
(be left-of ((@ . @X) @A (@ . @Y) @B)
   (left-of @X @A @Y @B) )

(be next-to (@X @A @Y @B) (right-of @X @A @Y @B))
(be next-to (@X @A @Y @B) (left-of @X @A @Y @B))
```

Test:

```PicoLisp
(pilog '((match @House @Person @Drink @Pet @Cigarettes))
   (let Fmt (-8 -11 -8 -7 -11)
      (tab Fmt "HOUSE" "PERSON" "DRINKS" "HAS" "SMOKES")
      (mapc '(@ (pass tab Fmt))
         @House @Person @Drink @Pet @Cigarettes ) ) )
```

Output:

```txt
HOUSE   PERSON     DRINKS  HAS    SMOKES
yellow  Norwegian  water   cats   Dunhill
blue    Dane       tea     horse  Blend
red     English    milk    birds  Pall-Mall
green   German     coffee  zebra  Prince
white   Swede      beer    dog    Blue-Master
```



## Prolog


In Prolog we can specify the domain by selecting elements from it, making mutually exclusive choices for efficiency:


```Prolog
select([A|As],S):- select(A,S,S1),select(As,S1).
select([],_).

next_to(A,B,C):- left_of(A,B,C) ; left_of(B,A,C).
left_of(A,B,C):- append(_,[A,B|_],C).

zebra(Owns, HS):-  % color,nation,pet,drink,smokes
      HS =    [ h(_,norwegian,_,_,_), _,  h(_,_,_,milk,_), _, _],
      select( [ h(red,englishman,_,_,_),  h(_,swede,dog,_,_),
                h(_,dane,_,tea,_),        h(_,german,_,_,prince) ], HS),
      select( [ h(_,_,birds,_,pallmall),  h(yellow,_,_,_,dunhill),
                h(_,_,_,beer,bluemaster) ],                         HS),
      left_of(  h(green,_,_,coffee,_),    h(white,_,_,_,_),         HS),
      next_to(  h(_,_,_,_,dunhill),       h(_,_,horse,_,_),         HS),
      next_to(  h(_,_,_,_,blend),         h(_,_,cats, _,_),         HS),
      next_to(  h(_,_,_,_,blend),         h(_,_,_,water,_),         HS),
      next_to(  h(_,norwegian,_,_,_),     h(blue,_,_,_,_),          HS),
      member(   h(_,Owns,zebra,_,_), HS).

:- ?- time(( zebra(Who, HS), maplist(writeln,HS), nl, write(Who), nl, nl, fail
             ; write('No more solutions.') )).
```


Output:

```txt

h(yellow,norwegian, cats,  water, dunhill)
h(blue,  dane,      horse, tea,   blend)
h(red,   englishman,birds, milk,  pallmall)
h(green, german,    zebra, coffee,prince)
h(white, swede,     dog,   beer,  bluemaster)

german

No more solutions.
% 5,959 inferences, 0.000 CPU in 0.060 seconds (0% CPU, Infinite Lips)
true.

```


Works [http://ideone.com/Es8DV with SWI-Prolog]. More verbose [http://ideone.com/6PzbZ translation] of the specification works as well.


### Direct rule by rule translation


Using extensible records, letting the houses' attributes be discovered from rules, ''not'' predetermined by a programmer (as any ''"a house has five attributes"'' solution is doing).


```Prolog
% attribute store is 'Name - Value' pairs with unique names
attrs( H, [N-V | R]) :- !, memberchk( N-X, H), X = V, (R = [], ! ; attrs( H, R)).
attrs( HS, AttrsL) :- maplist( attrs, HS, AttrsL).

in(    HS, Attrs) :- in( member, HS, Attrs).
in( G, HS, Attrs) :- call( G, A, HS), attrs( A, Attrs).

left_of( [A,B], HS) :- append( _, [A,B | _], HS).
next_to( [A,B], HS) :- left_of( [A,B], HS) ; left_of( [B,A], HS).

zebra( Owner, Houses):-
    Houses = [A,_,C,_,_],                                               % 1
    maplist( in(Houses), [ [ nation-englishman,   color-red          ]  % 2
                         , [ nation-swede,        owns -dog          ]  % 3
                         , [ nation-dane,         drink-tea          ]  % 4
                         , [ drink -coffee,       color-green        ]  % 6
                         , [ smoke -'Pall Mall',  owns -birds        ]  % 7
                         , [ color -yellow,       smoke-'Dunhill'    ]  % 8
                         , [ drink -beer,         smoke-'Blue Master']  % 13
                         , [ nation-german,       smoke-'Prince'     ]  % 14
                         ] ),
    in( left_of, Houses,   [[color -green    ],  [color -white    ]]),  % 5
    in( left_of,  [C,A],   [[drink -milk     ],  [nation-norwegian]]),  % 9, 10
    maplist( in( next_to, Houses),
                         [ [[smoke -'Blend'  ],  [owns -cats      ]]    % 11
                         , [[owns  -horse    ],  [smoke-'Dunhill' ]]    % 12
                         , [[nation-norwegian],  [color-blue      ]]    % 15
                         , [[drink -water    ],  [smoke-'Blend'   ]]    % 16
                         ] ),
    in( Houses, [owns-zebra, nation-Owner]).
```


[http://ideone.com/wcwXfZ Output]:

```Prolog
?- time(( zebra(Z,HS), (maplist(length,HS,_) -> maplist(sort,HS,S),
             maplist(writeln,S),nl,writeln(Z)), false ; writeln('No More Solutions'))).

[color-yellow,drink-water, nation-norwegian, owns-cats,  smoke-Dunhill    ]
[color-blue,  drink-tea,   nation-dane,      owns-horse, smoke-Blend      ]
[color-red,   drink-milk,  nation-englishman,owns-birds, smoke-Pall Mall  ]
[color-green, drink-coffee,nation-german,    owns-zebra, smoke-Prince     ]
[color-white, drink-beer,  nation-swede,     owns-dog,   smoke-Blue Master]

german
No More Solutions
% 263,486 inferences, 0.047 CPU in 0.063 seconds (74% CPU, 5630007 Lips)
```



### Alternative version

{{Works with|GNU Prolog}}
{{Works with|SWI Prolog}}

```prolog
:- initialization(main).


zebra(X) :-
    houses(Hs), member(h(_,X,zebra,_,_), Hs)
  , findall(_, (member(H,Hs), write(H), nl), _), nl
  , write('the one who keeps zebra: '), write(X), nl
  .


houses(Hs) :-
    Hs = [_,_,_,_,_]                         %  1
  , H3 = h(_,_,_,milk,_), Hs = [_,_,H3,_,_]  %  9
  , H1 = h(_,nvg,_,_,_ ), Hs = [H1|_]        % 10

  , maplist( flip(member,Hs),
       [ h(red,eng,_,_,_)                    %  2
       , h(_,swe,dog,_,_)                    %  3
       , h(_,dan,_,tea,_)                    %  4
       , h(green,_,_,coffe,_)                %  6
       , h(_,_,birds,_,pm)                   %  7
       , h(yellow,_,_,_,dh)                  %  8
       , h(_,_,_,beer,bm)                    % 13
       , h(_,ger,_,_,pri)                    % 14
       ])

  , infix([ h(green,_,_,_,_)
          , h(white,_,_,_,_) ], Hs)          %  5

  , maplist( flip(nextto,Hs),
      [ [h(_,_,_,_,bl   ), h(_,_,cats,_,_)]  % 11
      , [h(_,_,horse,_,_), h(_,_,_,_,dh  )]  % 12
      , [h(_,nvg,_,_,_  ), h(blue,_,_,_,_)]  % 15
      , [h(_,_,_,water,_), h(_,_,_,_,bl  )]  % 16
      ])
  .


flip(F,X,Y) :- call(F,Y,X).

infix(Xs,Ys) :- append(Xs,_,Zs) , append(_,Zs,Ys).
nextto(P,Xs) :- permutation(P,R), infix(R,Xs).


main :- findall(_, (zebra(_), nl), _), halt.

```

{{Output}}

```txt
h(yellow,nvg,cats,water,dh)
h(blue,dan,horse,tea,bl)
h(red,eng,birds,milk,pm)
h(green,ger,zebra,coffe,pri)
h(white,swe,dog,beer,bm)

the one who keeps zebra: ger

```



### Constraint Programming version

{{Works with|SWI Prolog}}
Original source code is 'ECLiPSe ( http://eclipseclp.org/ )' example: http://eclipseclp.org/examples/zebra.ecl.txt

```prolog
:- use_module(library(clpfd)).

zebra :-
    Nation = [Englishman, Spaniard, Japanese,     Ukrainian,   Norwegian ],
    Color  = [Red,        Green,    White,        Yellow,      Blue      ],
    Smoke  = [Oldgold,    Kools,    Chesterfield, Luckystrike, Parliament],
    Pet    = [Dog,        Snails,   Fox,          Horse,       Zebra     ],
    Drink  = [Tea,        Coffee,   Milk,         Orangejuice, Water     ],

    % house numbers 1 to 5
    Nation ins 1..5,
    Color  ins 1..5,
    Smoke  ins 1..5,
    Pet    ins 1..5,
    Drink  ins 1..5,

    % the values in each list are exclusive
    all_different(Nation),
    all_different(Color),
    all_different(Smoke),
    all_different(Pet),
    all_different(Drink),

    % actual constraints
    Englishman    #= Red,
    Spaniard      #= Dog,
    Green         #= Coffee,
    Ukrainian     #= Tea,
    Green         #= White + 1,
    Oldgold       #= Snails,
    Yellow        #= Kools,
    Milk          #= 3,
    Norwegian     #= 1,
    (Chesterfield #= Fox   - 1 #\/ Chesterfield #= Fox   + 1),
    (Kools        #= Horse - 1 #\/ Kools        #= Horse + 1),
    Luckystrike   #= Orangejuice,
    Japanese      #= Parliament,
    (Norwegian    #= Blue  - 1 #\/ Norwegian    #= Blue  + 1),

    % get solution
    flatten([Nation, Color, Smoke, Pet, Drink], List), label(List),

    % print the answers
    sort([Englishman-englishman, Spaniard-spaniard, Japanese-japanese,         Ukrainian-ukrainian,     Norwegian-norwegian],   NationNames),
    sort([Red-red,               Green-green,       White-white,               Yellow-yellow,           Blue-blue],             ColorNames),
    sort([Oldgold-oldgold,       Kools-kools,       Chesterfield-chesterfield, Luckystrike-luckystrike, Parliament-parliament], SmokeNames),
    sort([Dog-dog,               Snails-snails,     Fox-fox,                   Horse-horse,             Zebra-zebra],           PetNames),
    sort([Tea-tea,               Coffee-coffee,     Milk-milk,                 Orangejuice-orangejuice, Water-water],           DrinkNames),
    Fmt = '~w~16|~w~32|~w~48|~w~64|~w~n',
    format(Fmt, NationNames),
    format(Fmt, ColorNames),
    format(Fmt, SmokeNames),
    format(Fmt, PetNames),
    format(Fmt, DrinkNames).

```

{{Output}}

```txt

1-norwegian     2-ukrainian     3-englishman    4-spaniard      5-japanese
1-yellow        2-blue          3-red           4-white         5-green
1-kools         2-chesterfield  3-oldgold       4-luckystrike   5-parliament
1-fox           2-horse         3-snails        4-dog           5-zebra
1-water         2-tea           3-milk          4-orangejuice   5-coffee

```



## Python

{{trans|Clojure}}
Using 'logpy': https://github.com/logpy/logpy

```python

from logpy import *
from logpy.core import lall
import time

def lefto(q, p, list):
	# give me q such that q is left of p in list
	# zip(list, list[1:]) gives a list of 2-tuples of neighboring combinations
	# which can then be pattern-matched against the query
	return membero((q,p), zip(list, list[1:]))

def nexto(q, p, list):
	# give me q such that q is next to p in list
	# match lefto(q, p) OR lefto(p, q)
	# requirement of vector args instead of tuples doesn't seem to be documented
	return conde([lefto(q, p, list)], [lefto(p, q, list)])

houses = var()

zebraRules = lall(
	# there are 5 houses
	(eq, 		(var(), var(), var(), var(), var()), houses),
	# the Englishman's house is red
	(membero,	('Englishman', var(), var(), var(), 'red'), houses),
	# the Swede has a dog
	(membero,	('Swede', var(), var(), 'dog', var()), houses),
	# the Dane drinks tea
	(membero,	('Dane', var(), 'tea', var(), var()), houses),
	# the Green house is left of the White house
	(lefto,		(var(), var(), var(), var(), 'green'),
				(var(), var(), var(), var(), 'white'), houses),
	# coffee is the drink of the green house
	(membero,	(var(), var(), 'coffee', var(), 'green'), houses),
	# the Pall Mall smoker has birds
	(membero,	(var(), 'Pall Mall', var(), 'birds', var()), houses),
	# the yellow house smokes Dunhills
	(membero,	(var(), 'Dunhill', var(), var(), 'yellow'), houses),
	# the middle house drinks milk
	(eq,		(var(), var(), (var(), var(), 'milk', var(), var()), var(), var()), houses),
	# the Norwegian is the first house
	(eq,		(('Norwegian', var(), var(), var(), var()), var(), var(), var(), var()), houses),
	# the Blend smoker is in the house next to the house with cats
	(nexto,		(var(), 'Blend', var(), var(), var()),
				(var(), var(), var(), 'cats', var()), houses),
	# the Dunhill smoker is next to the house where they have a horse
	(nexto,		(var(), 'Dunhill', var(), var(), var()),
				(var(), var(), var(), 'horse', var()), houses),
	# the Blue Master smoker drinks beer
	(membero,	(var(), 'Blue Master', 'beer', var(), var()), houses),
	# the German smokes Prince
	(membero,	('German', 'Prince', var(), var(), var()), houses),
	# the Norwegian is next to the blue house
	(nexto,		('Norwegian', var(), var(), var(), var()),
				(var(), var(), var(), var(), 'blue'), houses),
	# the house next to the Blend smoker drinks water
	(nexto,		(var(), 'Blend', var(), var(), var()),
				(var(), var(), 'water', var(), var()), houses),
	# one of the houses has a zebra--but whose?
	(membero,	(var(), var(), var(), 'zebra', var()), houses)
)

t0 = time.time()
solutions = run(0, houses, zebraRules)
t1 = time.time()
dur = t1-t0

count = len(solutions)
zebraOwner = [house for house in solutions[0] if 'zebra' in house][0][0]

print "%i solutions in %.2f seconds" % (count, dur)
print "The %s is the owner of the zebra" % zebraOwner
print "Here are all the houses:"
for line in solutions[0]:
	print str(line)

```



### Alternative Version

{{trans|D}}

```python
import psyco; psyco.full()

class Content: elems= """Beer Coffee Milk Tea Water
                         Danish English German Norwegian Swedish
                         Blue Green Red White Yellow
                         Blend BlueMaster Dunhill PallMall Prince
                         Bird Cat Dog Horse Zebra""".split()
class Test: elems= "Drink Person Color Smoke Pet".split()
class House: elems= "One Two Three Four Five".split()

for c in (Content, Test, House):
  c.values = range(len(c.elems))
  for i, e in enumerate(c.elems):
    exec "%s.%s = %d" % (c.__name__, e, i)

def finalChecks(M):
  def diff(a, b, ca, cb):
    for h1 in House.values:
      for h2 in House.values:
        if M[ca][h1] == a and M[cb][h2] == b:
          return h1 - h2
    assert False

  return abs(diff(Content.Norwegian, Content.Blue,
                 Test.Person, Test.Color)) == 1 and \
         diff(Content.Green, Content.White,
              Test.Color, Test.Color) == -1 and \
         abs(diff(Content.Horse, Content.Dunhill,
                  Test.Pet, Test.Smoke)) == 1 and \
         abs(diff(Content.Water, Content.Blend,
                  Test.Drink, Test.Smoke)) == 1 and \
         abs(diff(Content.Blend, Content.Cat,
                  Test.Smoke, Test.Pet)) == 1

def constrained(M, atest):
      if atest == Test.Drink:
        return M[Test.Drink][House.Three] == Content.Milk
      elif atest == Test.Person:
        for h in House.values:
          if ((M[Test.Person][h] == Content.Norwegian and
               h != House.One) or
              (M[Test.Person][h] == Content.Danish and
               M[Test.Drink][h] != Content.Tea)):
            return False
        return True
      elif atest == Test.Color:
        for h in House.values:
          if ((M[Test.Person][h] == Content.English and
               M[Test.Color][h] != Content.Red) or
              (M[Test.Drink][h] == Content.Coffee and
               M[Test.Color][h] != Content.Green)):
            return False
        return True
      elif atest == Test.Smoke:
        for h in House.values:
          if ((M[Test.Color][h] == Content.Yellow and
               M[Test.Smoke][h] != Content.Dunhill) or
              (M[Test.Smoke][h] == Content.BlueMaster and
               M[Test.Drink][h] != Content.Beer) or
              (M[Test.Person][h] == Content.German and
               M[Test.Smoke][h] != Content.Prince)):
            return False
        return True
      elif atest == Test.Pet:
        for h in House.values:
          if ((M[Test.Person][h] == Content.Swedish and
               M[Test.Pet][h] != Content.Dog) or
              (M[Test.Smoke][h] == Content.PallMall and
               M[Test.Pet][h] != Content.Bird)):
            return False
        return finalChecks(M)

def show(M):
  for h in House.values:
    print "%5s:" % House.elems[h],
    for t in Test.values:
      print "%10s" % Content.elems[M[t][h]],
    print

def solve(M, t, n):
  if n == 1 and constrained(M, t):
    if t < 4:
      solve(M, Test.values[t + 1], 5)
    else:
      show(M)
      return

  for i in xrange(n):
    solve(M, t, n - 1)
    M[t][0 if n % 2 else i], M[t][n - 1] = \
      M[t][n - 1], M[t][0 if n % 2 else i]

def main():
  M = [[None] * len(Test.elems) for _ in xrange(len(House.elems))]
  for t in Test.values:
    for h in House.values:
      M[t][h] = Content.values[t * 5 + h]

  solve(M, Test.Drink, 5)

main()
```

{{out}}

```txt
  One:      Water  Norwegian     Yellow    Dunhill        Cat
  Two:        Tea     Danish       Blue      Blend      Horse
Three:       Milk    English        Red   PallMall       Bird
 Four:     Coffee     German      Green     Prince      Zebra
 Five:       Beer    Swedish      White BlueMaster        Dog
```

Runtime about 0.18 seconds.

### Alternative Version


```python
from itertools import permutations

class Number:elems= "One Two Three Four Five".split()
class Color: elems= "Red Green Blue White Yellow".split()
class Drink: elems= "Milk Coffee Water Beer Tea".split()
class Smoke: elems= "PallMall Dunhill Blend BlueMaster Prince".split()
class Pet:   elems= "Dog Cat Zebra Horse Bird".split()
class Nation:elems= "British Swedish Danish Norvegian German".split()

for c in (Number, Color, Drink, Smoke, Pet, Nation):
  for i, e in enumerate(c.elems):
    exec "%s.%s = %d" % (c.__name__, e, i)

def show_row(t, data):
  print "%6s: %12s%12s%12s%12s%12s" % (
    t.__name__, t.elems[data[0]],
    t.elems[data[1]], t.elems[data[2]],
    t.elems[data[3]], t.elems[data[4]])

def main():
  perms = list(permutations(range(5)))
  for number in perms:
    if number[Nation.Norvegian] == Number.One: # Constraint 10
      for color in perms:
        if color[Nation.British] == Color.Red: # Constraint 2
          if number[color.index(Color.Blue)] == Number.Two: # Constraint 15+10
            if number[color.index(Color.White)] - number[color.index(Color.Green)] == 1: # Constraint 5
              for drink in perms:
                if drink[Nation.Danish] == Drink.Tea: # Constraint 4
                  if drink[color.index(Color.Green)] == Drink.Coffee:  # Constraint 6
                    if drink[number.index(Number.Three)] == Drink.Milk: # Constraint 9
                      for smoke in perms:
                        if smoke[Nation.German] == Smoke.Prince: # Constraint 14
                          if drink[smoke.index(Smoke.BlueMaster)] == Drink.Beer: # Constraint 13
                            if smoke[color.index(Color.Yellow)] == Smoke.Dunhill: # Constraint 8
                              if number[smoke.index(Smoke.Blend)] - number[drink.index(Drink.Water)] in (1, -1): # Constraint 16
                                for pet in perms:
                                  if pet[Nation.Swedish] == Pet.Dog: # Constraint 3
                                    if pet[smoke.index(Smoke.PallMall)] == Pet.Bird: # Constraint 7
                                      if number[pet.index(Pet.Horse)] - number[smoke.index(Smoke.Dunhill)] in (1, -1): # Constraint 12
                                        if number[smoke.index(Smoke.Blend)] - number[pet.index(Pet.Cat)] in (1, -1): # Constraint 11
                                          print "Found a solution:"
                                          show_row(Nation, range(5))
                                          show_row(Number, number)
                                          show_row(Color, color)
                                          show_row(Drink, drink)
                                          show_row(Smoke, smoke)
                                          show_row(Pet, pet)
                                          print

main()
```

Output:

```txt
Found a solution:
Nation:      British     Swedish      Danish   Norvegian      German
Number:        Three        Five         Two         One        Four
 Color:          Red       White        Blue      Yellow       Green
 Drink:         Milk        Beer         Tea       Water      Coffee
 Smoke:     PallMall  BlueMaster       Blend     Dunhill      Prince
   Pet:         Bird         Dog       Horse         Cat       Zebra
```

Runtime about 0.0013 seconds

### Constraint Programming Version

Using 'python-constraint': http://labix.org/python-constraint,
Original source code is 'ECLiPSe ( http://eclipseclp.org/ )' example: http://eclipseclp.org/examples/zebra.ecl.txt

```python
from constraint import *

problem = Problem()

Nation = ["Englishman", "Spaniard", "Japanese",     "Ukrainian",   "Norwegian" ]
Color  = ["Red",        "Green",    "White",        "Yellow",      "Blue"      ]
Smoke  = ["Oldgold",    "Kools",    "Chesterfield", "Luckystrike", "Parliament"]
Pet    = ["Dog",        "Snails",   "Fox",          "Horse",       "Zebra"     ]
Drink  = ["Tea",        "Coffee",   "Milk",         "Orangejuice", "Water"     ]

# add variables: house numbers 1 to 5
problem.addVariables(Nation, range(1,5+1))
problem.addVariables(Color,  range(1,5+1))
problem.addVariables(Smoke,  range(1,5+1))
problem.addVariables(Pet,    range(1,5+1))
problem.addVariables(Drink,  range(1,5+1))

# add constraint: the values in each list are exclusive
problem.addConstraint(AllDifferentConstraint(), Nation)
problem.addConstraint(AllDifferentConstraint(), Color)
problem.addConstraint(AllDifferentConstraint(), Smoke)
problem.addConstraint(AllDifferentConstraint(), Pet)
problem.addConstraint(AllDifferentConstraint(), Drink)

# add constraint: actual constraints
problem.addConstraint(lambda a, b: a == b,                   ["Englishman",   "Red"        ])
problem.addConstraint(lambda a, b: a == b,                   ["Spaniard",     "Dog"        ])
problem.addConstraint(lambda a, b: a == b,                   ["Green",        "Coffee"     ])
problem.addConstraint(lambda a, b: a == b,                   ["Ukrainian",    "Tea"        ])
problem.addConstraint(lambda a, b: a == b + 1,               ["Green",        "White"      ])
problem.addConstraint(lambda a, b: a == b,                   ["Oldgold",      "Snails"     ])
problem.addConstraint(lambda a, b: a == b,                   ["Yellow",       "Kools"      ])
problem.addConstraint(lambda a: a == 3,                      ["Milk"                       ])
problem.addConstraint(lambda a: a == 1,                      ["Norwegian"                  ])
problem.addConstraint(lambda a, b: a == b - 1 or a == b + 1, ["Chesterfield", "Fox"        ])
problem.addConstraint(lambda a, b: a == b - 1 or a == b + 1, ["Kools",        "Horse"      ])
problem.addConstraint(lambda a, b: a == b,                   ["Luckystrike",  "Orangejuice"])
problem.addConstraint(lambda a, b: a == b,                   ["Japanese",     "Parliament" ])
problem.addConstraint(lambda a, b: a == b - 1 or a == b + 1, ["Norwegian",    "Blue"       ])

# get solution
sol = problem.getSolution()

# print the answers
nation = ["Nation" if i == 0 else ""  for i in range(6)]
color  = ["Color"  if i == 0 else ""  for i in range(6)]
smoke  = ["Smoke"  if i == 0 else ""  for i in range(6)]
pet    = ["Pet"    if i == 0 else ""  for i in range(6)]
drink  = ["Drink"  if i == 0 else ""  for i in range(6)]
for n in Nation:
    nation[sol[n]] = n
for n in Color:
    color[sol[n]] = n
for n in Smoke:
    smoke[sol[n]] = n
for n in Pet:
    pet[sol[n]] = n
for n in Drink:
    drink[sol[n]] = n
for d in [nation, color, smoke, pet, drink]:
    print("%6s: %14s%14s%14s%14s%14s" % (d[0], d[1], d[2], d[3], d[4], d[5]))

```

Output:

```txt
Nation:      Norwegian     Ukrainian    Englishman      Spaniard      Japanese
 Color:         Yellow          Blue           Red         White         Green
 Smoke:          Kools  Chesterfield       Oldgold   Luckystrike    Parliament
   Pet:            Fox         Horse        Snails           Dog         Zebra
 Drink:          Water           Tea          Milk   Orangejuice        Coffee
```



## R

{{libheader|combinat}}
```R


library(combinat)

col <- factor(c("Red","Green","White","Yellow","Blue"))
own <- factor(c("English","Swedish","Danish","German","Norwegian"))
pet <- factor(c("Dog","Birds","Cats","Horse","Zebra"))
drink <- factor(c("Coffee","Tea","Milk","Beer","Water"))
smoke <- factor(c("PallMall", "Blend", "Dunhill", "BlueMaster", "Prince"))

col_p <- permn(levels(col))
own_p <- permn(levels(own))
pet_p <- permn(levels(pet))
drink_p <- permn(levels(drink))
smoke_p <- permn(levels(smoke))

imright <- function(h1,h2){
  return(h1-h2==1)
}

nextto <- function(h1,h2){
  return(abs(h1-h2)==1)
}

house_with <- function(f,val){
  return(which(levels(f)==val))
}

for (i in seq(length(col_p))){
  col <- factor(col, levels=col_p[[i]])

  if (imright(house_with(col,"Green"),house_with(col,"White"))) {
    for (j in seq(length(own_p))){
      own <- factor(own, levels=own_p[[j]])

      if(house_with(own,"English") == house_with(col,"Red")){
        if(house_with(own,"Norwegian") == 1){
          if(nextto(house_with(own,"Norwegian"),house_with(col,"Blue"))){
            for(k in seq(length(drink_p))){
              drink <- factor(drink, levels=drink_p[[k]])

              if(house_with(drink,"Coffee") == house_with(col,"Green")){
                if(house_with(own,"Danish") == house_with(drink,"Tea")){
                  if(house_with(drink,"Milk") == 3){
                    for(l in seq(length(smoke_p))){
                      smoke <- factor(smoke, levels=smoke_p[[l]])

                      if(house_with(smoke,"Dunhill") == house_with(col,"Yellow")){
                        if(house_with(smoke,"BlueMaster") == house_with(drink,"Beer")){
                          if(house_with(own,"German") == house_with(smoke,"Prince")){
                            if(nextto(house_with(smoke,"Blend"),house_with(drink,"Water"))){
                              for(m in seq(length(pet_p))){
                                pet <- factor(pet, levels=pet_p[[m]])

                                if(house_with(own,"Swedish") == house_with(pet,"Dog")){
                                  if(house_with(smoke,"PallMall") == house_with(pet,"Birds")){
                                    if(nextto(house_with(smoke,"Blend"),house_with(pet,"Cats"))){
                                      if(nextto(house_with(smoke,"Dunhill"),house_with(pet,"Horse"))){
                                        res <- sapply(list(own,col,pet,smoke,drink),levels)
                                        colnames(res) <- c("Nationality","Colour","Pet","Drink","Smoke")
                                        print(res)
                                      }
                                    }
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }

}
```

{{out}}

```txt

     Nationality Colour   Pet     Drink        Smoke
[1,] "Norwegian" "Yellow" "Cats"  "Dunhill"    "Water"
[2,] "Danish"    "Blue"   "Horse" "Blend"      "Tea"
[3,] "English"   "Red"    "Birds" "PallMall"   "Milk"
[4,] "Swedish"   "White"  "Dog"   "BlueMaster" "Beer"
[5,] "German"    "Green"  "Zebra" "Prince"     "Coffee"

```



## Racket


{{trans|Prolog}}


```racket
#lang racket

(require racklog)

(define %select
  (%rel (x xs S S1)
    [(x (cons x xs) xs)]
    [(x (cons S xs) (cons S S1)) (%select x xs S1)]
    [((cons x xs) S)
     (%select x S S1)
     (%select xs S1)]
    [('() (_))]))

(define %next-to
  (%rel (A B C)
    [(A B C)
     (%or (%left-of A B C)
          (%left-of B A C))]))

(define %left-of
  (%rel (A B C)
    [(A B C) (%append (_) (cons A (cons B (_))) C)]))

(define %zebra
  (%rel (Owns HS)
    [(Owns HS)
     (%is HS (list (list (_) 'norwegian (_) (_) (_))
                   (_)
                   (list (_) (_) (_) 'milk (_))
                   (_) (_)))
     (%select (list (list 'red 'englishman (_) (_) (_))
                    (list (_) 'swede 'dog (_) (_))
                    (list (_) 'dane (_) 'tea (_))
                    (list (_) 'german (_) (_) 'prince))
              HS)
     (%select (list (list (_) (_) 'birds (_) 'pallmall)
                    (list 'yellow (_) (_) (_) 'dunhill)
                    (list (_) (_) (_) 'beer 'bluemaster))
              HS)
     (%left-of (list 'green (_) (_) 'coffee (_))
               (list 'white (_) (_) (_) (_))
               HS)
     (%next-to (list (_) (_) (_) (_) 'dunhill)
               (list (_) (_) 'horse (_) (_))
               HS)
     (%next-to (list (_) (_) (_) (_) 'blend)
               (list (_) (_) 'cats (_) (_))
               HS)
     (%next-to (list (_) (_) (_) (_) 'blend)
               (list (_) (_) (_) 'water (_))
               HS)
     (%next-to (list (_) 'norwegian (_) (_) (_))
               (list 'blue (_) (_) (_) (_))
               HS)
     (%member (list (_) Owns 'zebra (_) (_)) HS)]))

(%which (Who HS) (%zebra Who HS))
```


Output:

```txt

'((Who . german)
  (HS
   (yellow norwegian cats water dunhill)
   (blue dane horse tea blend)
   (red englishman birds milk pallmall)
   (green german zebra coffee prince)
   (white swede dog beer bluemaster)))

```



## REXX

{{trans|BBC BASIC}}
Permutations algorithm taken from REXX

```rexx
/* REXX ---------------------------------------------------------------
* Solve the Zebra Puzzle
*--------------------------------------------------------------------*/
  Call mk_perm    /* compute all permutations                        */
  Call encode     /* encode the elements of the specifications       */
  /* ex2 .. eg16     the formalized specifications                   */
  solutions=0
  Call time 'R'
  Do nation_i = 1 TO 120
    Nations = perm.nation_i
    IF ex10() Then Do
      Do color_i = 1 TO 120
        Colors = perm.color_i
        IF ex5() & ex2() & ex15() Then Do
          Do drink_i = 1 TO 120
            Drinks = perm.drink_i
            IF ex9() & ex4() & ex6() Then Do
              Do smoke_i = 1 TO 120
                Smokes = perm.smoke_i
                IF ex14() & ex13() & ex16() & ex8() Then Do
                   Do animal_i = 1 TO 120
                    Animals = perm.animal_i
                    IF ex3() & ex7() & ex11() & ex12() Then Do
                      /* Call out 'Drinks =' Drinks  54321 Wat Tea Mil Cof Bee */
                      /* Call out 'Nations=' Nations 41235 Nor Den Eng Ger Swe */
                      /* Call out 'Colors =' Colors  51324 Yel Blu Red Gre Whi */
                      /* Call out 'Smokes =' Smokes  31452 Dun Ble Pal Pri Blu */
                      /* Call out 'Animals=' Animals 24153 Cat Hor Bir Zeb Dog */
                      Call out 'House   Drink      Nation     Colour'||,
                                                         '     Smoke      Animal'
                      Do i=1 To 5
                        di=substr(drinks,i,1)
                        ni=substr(nations,i,1)
                        ci=substr(colors,i,1)
                        si=substr(smokes,i,1)
                        ai=substr(animals,i,1)
                        ol.i=right(i,3)'     '||left(drink.di,11),
                                              ||left(nation.ni,11),
                                              ||left(color.ci,11),
                                              ||left(smoke.si,11),
                                              ||left(animal.ai,11)
                        Call out ol.i
                        End
                      solutions=solutions+1
                      End
                    End /* animal_i */
                  End
                End /* smoke_i */
              End
            End /* drink_i */
          End
        End /* color_i */
      End
    End /* nation_i */
  Say 'Number of solutions =' solutions
  Say 'Solved in' time('E') 'seconds'
Exit

/*------------------------------------------------------------------------------
      #There are five houses.
ex2:  #The English man lives in the red house.
ex3:  #The Swede has a dog.
ex4:  #The Dane drinks tea.
ex5:  #The green house is immediately to the left of the white house.
ex6:  #They drink coffee in the green house.
ex7:  #The man who smokes Pall Mall has birds.
ex8:  #In the yellow house they smoke Dunhill.
ex9:  #In the middle house they drink milk.
ex10: #The Norwegian lives in the first house.
ex11: #The man who smokes Blend lives in the house next to the house with cats.
ex12: #In a house next to the house where they have a horse, they smoke Dunhill.
ex13: #The man who smokes Blue Master drinks beer.
ex14: #The German smokes Prince.
ex15: #The Norwegian lives next to the blue house.
ex16: #They drink water in a house next to the house where they smoke Blend.
------------------------------------------------------------------------------*/
ex2:  Return pos(England,Nations)=pos(Red,Colors)
ex3:  Return pos(Sweden,Nations)=pos(Dog,Animals)
ex4:  Return pos(Denmark,Nations)=pos(Tea,Drinks)
ex5:  Return pos(Green,Colors)=pos(White,Colors)-1
ex6:  Return pos(Coffee,Drinks)=pos(Green,Colors)
ex7:  Return pos(PallMall,Smokes)=pos(Birds,Animals)
ex8:  Return pos(Dunhill,Smokes)=pos(Yellow,Colors)
ex9:  Return substr(Drinks,3,1)=Milk
ex10: Return left(Nations,1)=Norway
ex11: Return abs(pos(Blend,Smokes)-pos(Cats,Animals))=1
ex12: Return abs(pos(Dunhill,Smokes)-pos(Horse,Animals))=1
ex13: Return pos(BlueMaster,Smokes)=pos(Beer,Drinks)
ex14: Return pos(Germany,Nations)=pos(Prince,Smokes)
ex15: Return abs(pos(Norway,Nations)-pos(Blue,Colors))=1
ex16: Return abs(pos(Blend,Smokes)-pos(Water,Drinks))=1

mk_perm: Procedure Expose perm.
/*---------------------------------------------------------------------
* Make all permutations of 12345 in perm.*
*--------------------------------------------------------------------*/
perm.=0
n=5
Do pop=1 For n
  p.pop=pop
  End
Call store
Do While nextperm(n,0)
  Call store
  End
Return

nextperm: Procedure Expose p. perm.
  Parse Arg n,i
  nm=n-1
  Do k=nm By-1 For nm
    kp=k+1
    If p.k<p.kp Then Do
      i=k
      Leave
      End
    End
  Do j=i+1 While j<n
    Parse Value p.j p.n With p.n p.j
    n=n-1
    End
  If i>0 Then Do
    Do j=i+1 While p.j<p.i
      End
    Parse Value p.j p.i With p.i p.j
    End
  Return i>0

store: Procedure Expose p. perm.
  z=perm.0+1
    _=''
    Do j=1 To 5
      _=_||p.j
      End
  perm.z=_
  perm.0=z
  Return

encode:
  Beer=1         ; Drink.1='Beer'
  Coffee=2       ; Drink.2='Coffee'
  Milk=3         ; Drink.3='Milk'
  Tea=4          ; Drink.4='Tea'
  Water=5        ; Drink.5='Water'
  Denmark=1      ; Nation.1='Denmark'
  England=2      ; Nation.2='England'
  Germany=3      ; Nation.3='Germany'
  Norway=4       ; Nation.4='Norway'
  Sweden=5       ; Nation.5='Sweden'
  Blue=1         ; Color.1='Blue'
  Green=2        ; Color.2='Green'
  Red=3          ; Color.3='Red'
  White=4        ; Color.4='White'
  Yellow=5       ; Color.5='Yellow'
  Blend=1        ; Smoke.1='Blend'
  BlueMaster=2   ; Smoke.2='BlueMaster'
  Dunhill=3      ; Smoke.3='Dunhill'
  PallMall=4     ; Smoke.4='PallMall'
  Prince=5       ; Smoke.5='Prince'
  Birds=1        ; Animal.1='Birds'
  Cats=2         ; Animal.2='Cats'
  Dog=3          ; Animal.3='Dog'
  Horse=4        ; Animal.4='Horse'
  Zebra=5        ; Animal.5='Zebra'
  Return

out:
  Say arg(1)
  Return
```

{{out}}

```txt
House   Drink      Nation     Colour     Smoke      Animal
  1     Water      Norway     Yellow     Dunhill    Cats
  2     Tea        Denmark    Blue       Blend      Horse
  3     Milk       England    Red        PallMall   Birds
  4     Coffee     Germany    Green      Prince     Zebra
  5     Beer       Sweden     White      BlueMaster Dog
Number of solutions = 1
Solved in 0.063000 seconds
```



## Ruby


```ruby
CONTENT = { House:       '',
            Nationality: %i[English Swedish Danish Norwegian German],
            Colour:      %i[Red Green White Blue Yellow],
            Pet:         %i[Dog Birds Cats Horse Zebra],
            Drink:       %i[Tea Coffee Milk Beer Water],
            Smoke:       %i[PallMall Dunhill BlueMaster Prince Blend] }

def adjacent? (n,i,g,e)
  (0..3).any?{|x| (n[x]==i and g[x+1]==e) or (n[x+1]==i and g[x]==e)}
end

def leftof? (n,i,g,e)
  (0..3).any?{|x| n[x]==i and g[x+1]==e}
end

def coincident? (n,i,g,e)
  n.each_index.any?{|x| n[x]==i and g[x]==e}
end

def solve_zebra_puzzle
  CONTENT[:Nationality].permutation{|nation|
    next unless nation.first == :Norwegian                              # 10
    CONTENT[:Colour].permutation{|colour|
      next unless leftof?(colour, :Green, colour, :White)               # 5
      next unless coincident?(nation, :English, colour, :Red)           # 2
      next unless adjacent?(nation, :Norwegian, colour, :Blue)          # 15
      CONTENT[:Pet].permutation{|pet|
        next unless coincident?(nation, :Swedish, pet, :Dog)            # 3
        CONTENT[:Drink].permutation{|drink|
          next unless drink[2] == :Milk                                 # 9
          next unless coincident?(nation, :Danish, drink, :Tea)         # 4
          next unless coincident?(colour, :Green, drink, :Coffee)       # 6
          CONTENT[:Smoke].permutation{|smoke|
            next unless coincident?(smoke, :PallMall, pet, :Birds)      # 7
            next unless coincident?(smoke, :Dunhill, colour, :Yellow)   # 8
            next unless coincident?(smoke, :BlueMaster, drink, :Beer)   # 13
            next unless coincident?(smoke, :Prince, nation, :German)    # 14
            next unless adjacent?(smoke, :Blend, pet, :Cats)            # 11
            next unless adjacent?(smoke, :Blend, drink, :Water)         # 16
            next unless adjacent?(smoke, :Dunhill,pet, :Horse)          # 12
            print_out(nation, colour, pet, drink, smoke)
  } } } } }
end

def print_out (nation, colour, pet, drink, smoke)
  width = CONTENT.map{|x| x.flatten.map{|y|y.size}.max}
  fmt = width.map{|w| "%-#{w}s"}.join(" ")
  national = nation[ pet.find_index(:Zebra) ]
  puts "The Zebra is owned by the man who is #{national}",""
  puts fmt % CONTENT.keys, fmt % width.map{|w| "-"*w}
  [nation,colour,pet,drink,smoke].transpose.each.with_index(1){|x,n| puts fmt % [n,*x]}
end

solve_zebra_puzzle
```


{{out}}

```txt

The Zebra is owned by the man who is German

House Nationality Colour Pet   Drink  Smoke
----- ----------- ------ ----- ------ ----------
1     Norwegian   Yellow Cats  Water  Dunhill
2     Danish      Blue   Horse Tea    Blend
3     English     Red    Birds Milk   PallMall
4     German      Green  Zebra Coffee Prince
5     Swedish     White  Dog   Beer   BlueMaster

```



### Another approach


```ruby

class String; def brk; split(/(?=[A-Z])/); end; end
men,drinks,colors,pets,smokes = "NorwegianGermanDaneSwedeEnglish
  MilkTeaBeerWaterCoffeeGreenWhiteRedYellowBlueZebraDogCatsHorseBirds
  PallmallDunhillBlendBluemasterPrince".delete(" \n").
  brk.each_slice(5).map{|e| e.permutation.to_a};
men.select!{|x| "Norwegian"==x[0]};
drinks.select!{|x| "Milk"==x[2]};
colors.select!{|x| x.join[/GreenWhite/]};

dis = proc{|s,*a| s.brk.map{|w| a.map{|p| p.index(w)}.
  compact[0]}.each_slice(2).map{|a,b| (a-b).abs}}

men.each{|m| colors.each{|c|
  next unless dis["RedEnglishBlueNorwegian",c,m]==[0,1]
  drinks.each{|d| next unless dis["DaneTeaCoffeeGreen",m,d,c]==[0,0]
    smokes.each{|s|
      next unless dis["YellowDunhillBluemasterBeerGermanPrince",
                      c,s,d,m]==[0,0,0]
      pets.each{|p|
        next unless dis["SwedeDogBirdsPallmallCatsBlendHorseDunhill",
                        m,p,s]==[0,0,1,1]
        x = [p,m,c,d,s].transpose
        puts "The #{x.find{|y|y[0]=="Zebra"}[1]} owns the zebra.",
          x.map{|y| y.map{|z| z.ljust(11)}.join}}}}}}

```

Output:

```txt

The German owns the zebra.
Cats       Norwegian  Yellow     Water      Dunhill
Horse      Dane       Blue       Tea        Blend
Birds      English    Red        Milk       Pallmall
Zebra      German     Green      Coffee     Prince
Dog        Swede      White      Beer       Bluemaster

```



## Scala

===Idiomatic (for comprehension)===

```Scala
/* Note to the rules:
 *
 * It can further concluded that:
 * 5a: The green house cannot be at the h1 position
 * 5b: The white house cannot be at the h5 position
 *
 * 16: This rule is redundant.
 */

object Einstein extends App {
  val possibleMembers = for { // pair clues results in 78 members
    nationality <- List("Norwegian", "German", "Dane", "Englishman", "Swede")
    color <- List("Red", "Green", "Yellow", "White", "Blue")
    beverage <- List("Milk", "Coffee", "Tea", "Beer", "Water")
    animal <- List("Dog", "Horse", "Birds", "Cats", "Zebra")
    brand <- List("Blend", "Pall Mall", "Prince", "Blue Master", "Dunhill")
    if (color == "Red") == (nationality == "Englishman") // #2
    if (nationality == "Swede") == (animal == "Dog") // #3
    if (nationality == "Dane") == (beverage == "Tea") // #4
    if (color == "Green") == (beverage == "Coffee") // #6
    if (brand == "Pall Mall") == (animal == "Birds") // #7
    if (brand == "Dunhill") == (color == "Yellow") // #8
    if (brand == "Blue Master") == (beverage == "Beer") // #13
    if (brand == "Prince") == (nationality == "German") // #14
  } yield new House(nationality, color, beverage, animal, brand)
  val members = for { // Neighborhood clues
    h1 <- housesLeftOver().filter(p => (p.nationality == "Norwegian" /* #10 */) && (p.color != "Green") /* #5a */) // 28
    h3 <- housesLeftOver(h1).filter(p => p.beverage == "Milk") // #9 // 24
    h2 <- housesLeftOver(h1, h3).filter(_.color == "Blue") // #15
    if matchMiddleBrandAnimal(h1, h2, h3, "Blend", "Cats") // #11
    if matchCornerBrandAnimal(h1, h2, "Horse", "Dunhill") // #12
    h4 <- housesLeftOver(h1, h2, h3).filter(_.checkAdjacentWhite(h3) /* #5 */)
    h5 <- housesLeftOver(h1, h2, h3, h4)

    //  Redundant tests
    if h2.checkAdjacentWhite(h1)
    if h3.checkAdjacentWhite(h2)
    if matchCornerBrandAnimal(h5, h4, "Horse", "Dunhill")
    if matchMiddleBrandAnimal(h2, h3, h4, "Blend", "Cats")
    if matchMiddleBrandAnimal(h3, h4, h5, "Blend", "Cats")
  } yield Seq(h1, h2, h3, h4, h5)

  def matchMiddleBrandAnimal(home1: House, home2: House, home3: House, brand: String, animal: String) =
    (home1.animal == animal || home2.brand != brand || home3.animal == animal) &&
      (home1.brand == brand || home2.animal != animal || home3.brand == brand)

  def matchCornerBrandAnimal(corner: House, inner: House, animal: String, brand: String) =
    (corner.brand != brand || inner.animal == animal) && (corner.animal == animal || inner.brand != brand)

  def housesLeftOver(pickedHouses: House*): List[House] = {
    possibleMembers.filter(house => pickedHouses.forall(_.totalUnEqual(house)))
  }

  class House(val nationality: String, val color: String, val beverage: String, val animal: String, val brand: String) {
    override def toString = {
      f"$nationality%10s, ${color + ", "}%-8s$beverage,\t$animal,\t$brand."
    }

    def totalUnEqual(home2: House) =
      this.animal != home2.animal &&
        this.beverage != home2.beverage &&
        this.brand != home2.brand &&
        this.color != home2.color &&
        this.nationality != home2.nationality

    //** Checks if the this green house is next to the other white house*/
    def checkAdjacentWhite(home2: House) = (this.color == "Green") == (home2.color == "White") // #5
  }

  { // Main program
    val beest = "Zebra"
    members.flatMap(p => p.filter(p => p.animal == beest)).
      foreach(s => println(s"The ${s.nationality} is the owner of the ${beest.toLowerCase}."))

    println(s"The ${members.size} solution(s) are:")
    members.foreach(solution => solution.zipWithIndex.foreach(h => println(s"House ${h._2 + 1} ${h._1}")))
  }
} // loc 58
```

{{Out}}See it in running in your browser by [https://scalafiddle.io/sf/h5Y98te/0 ScalaFiddle (JavaScript executed in browser)] or by [https://scastie.scala-lang.org/fJZRog4xQ9aAV4D3crM1gQ Scastie (remote JVM)].{{out}}

```txt
 The German is the owner of the zebra.
 The 1 solution(s) are:
 House 1  Norwegian, Yellow, Water,	Cats,	Dunhill.
 House 2       Dane, Blue,   Tea,	Horse,	Blend.
 House 3 Englishman, Red,    Milk,	Birds,	Pall Mall.
 House 4      Swede, White,  Beer,	Dog,	Blue Master.
 House 5     German, Green,  Coffee,	Zebra,	Prince.

```


===Scala Alternate Version (Over-engineered)===

```scala
import scala.util.Try

object Einstein extends App {

  // The strategy here is to mount a brute-force attack on the solution space, pruning very aggressively.
  // The scala standard `permutations` method is extremely helpful here. It turns out that by pruning
  // quickly and smartly we can solve this very quickly (45ms on my machine) compared to days or weeks
  // required to fully enumerate the solution space.

  // We set up a for comprehension with an enumerator for each of the 5 variables, with if clauses to
  // prune. The hard part is the pruning logic, which is basically just translating the rules to code
  // and the data model. The data model is basically Seq[Seq[String]]

  // Rules are encoded as for comprehension filters. There is a natural cascade of rules from
  // depending on more or less criteria. The rules about smokes are the most complex and depend
  // on the most other factors

  // 4. The green house is just to the left of the white one.
  def colorRules(colors: Seq[String]) = Try(colors(colors.indexOf("White") - 1) == "Green").getOrElse(false)

  // 1. The Englishman lives in the red house.
  // 9. The Norwegian lives in the first house.
  // 14. The Norwegian lives next to the blue house.
  def natRules(colors: Seq[String], nats: Seq[String]) =
    nats.head == "Norwegian" && colors(nats.indexOf("Brit")) == "Red" &&
      (Try(colors(nats.indexOf("Norwegian") - 1) == "Blue").getOrElse(false) ||
        Try(colors(nats.indexOf("Norwegian") + 1) == "Blue").getOrElse(false))

  // 3. The Dane drinks tea.
  // 5. The owner of the green house drinks coffee.
  // 8. The man in the center house drinks milk.
  def drinkRules(colors: Seq[String], nats: Seq[String], drinks: Seq[String]) =
    drinks(nats.indexOf("Dane")) == "Tea" &&
      drinks(colors.indexOf("Green")) == "Coffee" &&
      drinks(2) == "Milk"

  // 2. The Swede keeps dogs.
  def petRules(nats: Seq[String], pets: Seq[String]) = pets(nats.indexOf("Swede")) == "Dogs"

  // 6. The Pall Mall smoker keeps birds.
  // 7. The owner of the yellow house smokes Dunhills.
  // 10. The Blend smoker has a neighbor who keeps cats.
  // 11. The man who smokes Blue Masters drinks bier.
  // 12. The man who keeps horses lives next to the Dunhill smoker.
  // 13. The German smokes Prince.
  // 15. The Blend smoker has a neighbor who drinks water.
  def smokeRules(colors: Seq[String], nats: Seq[String], drinks: Seq[String], pets: Seq[String], smokes: Seq[String]) =
    pets(smokes.indexOf("Pall Mall")) == "Birds" &&
      smokes(colors.indexOf("Yellow")) == "Dunhill" &&
      (Try(pets(smokes.indexOf("Blend") - 1) == "Cats").getOrElse(false) ||
        Try(pets(smokes.indexOf("Blend") + 1) == "Cats").getOrElse(false)) &&
      drinks(smokes.indexOf("BlueMaster")) == "Beer" &&
      (Try(smokes(pets.indexOf("Horses") - 1) == "Dunhill").getOrElse(false) ||
        Try(pets(pets.indexOf("Horses") + 1) == "Dunhill").getOrElse(false)) &&
      smokes(nats.indexOf("German")) == "Prince" &&
      (Try(drinks(smokes.indexOf("Blend") - 1) == "Water").getOrElse(false) ||
        Try(drinks(smokes.indexOf("Blend") + 1) == "Water").getOrElse(false))

  // once the rules are created it, the actual solution is simple: iterate brute force, pruning early.
  val solutions = for {
    colors <- Seq("Red", "Blue", "White", "Green", "Yellow").permutations if colorRules(colors)
    nats <- Seq("Brit", "Swede", "Dane", "Norwegian", "German").permutations if natRules(colors, nats)
    drinks <- Seq("Tea", "Coffee", "Milk", "Beer", "Water").permutations if drinkRules(colors, nats, drinks)
    pets <- Seq("Dogs", "Birds", "Cats", "Horses", "Fish").permutations if petRules(nats, pets)
    smokes <- Seq("BlueMaster", "Blend", "Pall Mall", "Dunhill", "Prince").permutations if smokeRules(colors, nats, drinks, pets, smokes)
  } yield Seq(colors, nats, drinks, pets, smokes)

  // There *should* be just one solution...
  solutions.foreach { solution =>
    // so we can pretty-print, find out the maximum string length of all cells
    val maxLen = solution.flatten.map(_.length).max

    def pretty(str: String): String = str + (" " * (maxLen - str.length + 1))

    // a labels column
    val labels = ("" +: Seq("Color", "Nation", "Drink", "Pet", "Smoke").map(_ + ":")).toIterator

    // print each row including a column header
    ((1 to 5).map(n => s"House $n") +: solution).map(_.map(pretty)).map(x => (pretty(labels.next) +: x).mkString(" ")).foreach(println)

    println(s"\nThe ${solution(1)(solution(3).indexOf("Fish"))} owns the Fish")
  }

}// loc 38
```

{{Out}}Experience running it in your browser by [https://scalafiddle.io/sf/hJgtjYG/1 ScalaFiddle (JavaScript executed in browser)] or by [https://scastie.scala-lang.org/ajeuRgDSQKyVv4Jd6SPfVQ Scastie (remote JVM)].
{{out}}

```txt
            House 1     House 2     House 3     House 4     House 5
Color:      Yellow      Blue        Red         Green       White
Nation:     Norwegian   Dane        Brit        German      Swede
Drink:      Water       Tea         Milk        Coffee      Beer
Pet:        Cats        Horses      Birds       Fish        Dogs
Smoke:      Dunhill     Blend       Pall Mall   Prince      BlueMaster

The German owns the Fish

```



## Sidef

{{trans|Ruby}}

```ruby
var CONTENT = Hash(
            :House       => nil,
            :Nationality => [:English, :Swedish, :Danish, :Norwegian, :German],
            :Colour      => [:Red, :Green, :White, :Blue, :Yellow],
            :Pet         => [:Dog, :Birds, :Cats, :Horse, :Zebra],
            :Drink       => [:Tea, :Coffee, :Milk, :Beer, :Water],
            :Smoke       => [:PallMall, :Dunhill, :BlueMaster, :Prince, :Blend]
)

func adjacent(n,i,g,e) {
  (0..3).any {|x| (n[x]==i && g[x+1]==e) || (n[x+1]==i && g[x]==e) }
}

func leftof(n,i,g,e) {
  (0..3).any {|x| n[x]==i && g[x+1]==e }
}

func coincident(n,i,g,e) {
  n.indices.any {|x| n[x]==i && g[x]==e }
}

func solve {
  CONTENT{:Nationality}.permutations{|*nation|
    nation.first == :Norwegian ->
      && CONTENT{:Colour}.permutations {|*colour|
          leftof(colour,:Green,colour,:White) ->
       && coincident(nation,:English,colour,:Red) ->
       && adjacent(nation,:Norwegian,colour,:Blue) ->
       && CONTENT{:Pet}.permutations {|*pet|
             coincident(nation,:Swedish,pet,:Dog) ->
          && CONTENT{:Drink}.permutations {|*drink|
               drink[2] == :Milk ->
            && coincident(nation,:Danish,drink,:Tea) ->
            && coincident(colour,:Green,drink,:Coffee) ->
            && CONTENT{:Smoke}.permutations {|*smoke|
                coincident(smoke,:PallMall,pet,:Birds) ->
             && coincident(smoke,:Dunhill,colour,:Yellow) ->
             && coincident(smoke,:BlueMaster,drink,:Beer) ->
             && coincident(smoke,:Prince,nation,:German) ->
             && adjacent(smoke,:Blend,pet,:Cats) ->
             && adjacent(smoke,:Blend,drink,:Water) ->
             && adjacent(smoke,:Dunhill,pet,:Horse) ->
             && return [nation,colour,pet,drink,smoke]
} } } } } }

var res = solve();
var keys = [:House, :Nationality, :Colour, :Pet, :Drink, :Smoke]
var width = keys.map{ .len }
var fmt = width.map{|w| "%-#{w+2}s" }.join(" ")
say "The Zebra is owned by the man who is #{res[0][res[2].first_index(:Zebra)]}\n"
say (fmt % keys..., "\n", fmt % width.map{|w| "-"*w }...)
res[0].indices.map{|i| res.map{|a| a[i] }}.each_kv {|k,v| say fmt%(k,v...) }
```

{{out}}

```txt

The Zebra is owned by the man who is German

House   Nationality   Colour   Pet   Drink   Smoke
-----   -----------   ------   ---   -----   -----
0       Norwegian     Yellow   Cats  Water   Dunhill
1       Danish        Blue     Horse Tea     Blend
2       English       Red      Birds Milk    PallMall
3       German        Green    Zebra Coffee  Prince
4       Swedish       White    Dog   Beer    BlueMaster

```



## Standard ML

{{works with|SML/NJ}}
{{trans|C++}}(This implementation uses the search algorithm of the C++ implementation, but the rules check algorithm is different.)

```sml

(* Attributes and values *)
val str_attributes = Vector.fromList ["Color",    "Nation",  "Drink", "Pet",        "Smoke"]
val str_colors     = Vector.fromList ["Red",      "Green",   "White", "Yellow",     "Blue"]
val str_nations    = Vector.fromList ["English",  "Swede",   "Dane",  "German",     "Norwegian"]
val str_drinks     = Vector.fromList ["Tea",      "Coffee",  "Milk",  "Beer",       "Water"]
val str_pets       = Vector.fromList ["Dog",      "Birds",   "Cats",  "Horse",      "Zebra"]
val str_smokes     = Vector.fromList ["PallMall", "Dunhill", "Blend", "BlueMaster", "Prince"]

val (Color, Nation, Drink, Pet, Smoke)             = (0, 1, 2, 3, 4)	(* Attributes *)
val (Red, Green, White, Yellow, Blue)              = (0, 1, 2, 3, 4)	(* Color      *)
val (English, Swede, Dane, German, Norwegian)      = (0, 1, 2, 3, 4)	(* Nation     *)
val (Tea, Coffee, Milk, Beer, Water)               = (0, 1, 2, 3, 4)	(* Drink      *)
val (Dog, Birds, Cats, Horse, Zebra)               = (0, 1, 2, 3, 4)	(* Pet        *)
val (PallMall, Dunhill, Blend, BlueMaster, Prince) = (0, 1, 2, 3, 4)	(* Smoke      *)

type attr    = int
type value   = int
type houseno = int

(* Rules *)
datatype rule =
	  AttrPairRule of (attr * value) * (attr * value)
	| NextToRule   of (attr * value) * (attr * value)
	| LeftOfRule   of (attr * value) * (attr * value)

(* Conditions *)
val rules = [
AttrPairRule ((Nation, English), (Color, Red)),		(* #02 *)
AttrPairRule ((Nation, Swede), (Pet, Dog)),		(* #03 *)
AttrPairRule ((Nation, Dane), (Drink, Tea)), 		(* #04 *)
LeftOfRule   ((Color, Green), (Color, White)),		(* #05 *)
AttrPairRule ((Color, Green), (Drink, Coffee)),		(* #06 *)
AttrPairRule ((Smoke, PallMall), (Pet, Birds)),		(* #07 *)
AttrPairRule ((Smoke, Dunhill), (Color, Yellow)),	(* #08 *)
NextToRule   ((Smoke, Blend), (Pet, Cats)),		(* #11 *)
NextToRule   ((Smoke, Dunhill), (Pet, Horse)),		(* #12 *)
AttrPairRule ((Smoke, BlueMaster), (Drink, Beer)),	(* #13 *)
AttrPairRule ((Nation, German), (Smoke, Prince)),	(* #14 *)
NextToRule   ((Nation, Norwegian), (Color, Blue)),	(* #15 *)
NextToRule   ((Smoke, Blend), (Drink, Water))]		(* #16 *)


type house = value option * value option * value option * value option * value option

fun houseval ((a, b, c, d, e) : house, 0 : attr) = a
  | houseval ((a, b, c, d, e) : house, 1 : attr) = b
  | houseval ((a, b, c, d, e) : house, 2 : attr) = c
  | houseval ((a, b, c, d, e) : house, 3 : attr) = d
  | houseval ((a, b, c, d, e) : house, 4 : attr) = e
  | houseval _ = raise Domain

fun sethouseval ((a, b, c, d, e) : house, 0 : attr, a2 : value option) = (a2, b,  c,  d,  e )
  | sethouseval ((a, b, c, d, e) : house, 1 : attr, b2 : value option) = (a,  b2, c,  d,  e )
  | sethouseval ((a, b, c, d, e) : house, 2 : attr, c2 : value option) = (a,  b,  c2, d,  e )
  | sethouseval ((a, b, c, d, e) : house, 3 : attr, d2 : value option) = (a,  b,  c,  d2, e )
  | sethouseval ((a, b, c, d, e) : house, 4 : attr, e2 : value option) = (a,  b,  c,  d,  e2)
  | sethouseval _ = raise Domain

fun getHouseVal houses (no, attr) = houseval (Array.sub (houses, no), attr)
fun setHouseVal houses (no, attr, newval) =
	Array.update (houses, no, sethouseval (Array.sub (houses, no), attr, newval))


fun match (house, (rule_attr, rule_val)) =
	let
	  val value = houseval (house, rule_attr)
	in
	  isSome value andalso valOf value = rule_val
	end

fun matchNo houses (no, rule) =
	 match (Array.sub (houses, no), rule)

fun compare (house1, house2, ((rule_attr1, rule_val1), (rule_attr2, rule_val2))) =
	let
	  val val1 = houseval (house1, rule_attr1)
	  val val2 = houseval (house2, rule_attr2)
	in
	  if isSome val1 andalso isSome val2
	  then (valOf val1 = rule_val1 andalso valOf val2 <> rule_val2)
	         orelse
	       (valOf val1 <> rule_val1 andalso valOf val2 = rule_val2)
	  else false
	end

fun compareNo houses (no1, no2, rulepair) =
	compare (Array.sub (houses, no1), Array.sub (houses, no2), rulepair)


fun invalid houses no (AttrPairRule rulepair) =
	compareNo houses (no, no, rulepair)

  | invalid houses no (NextToRule rulepair) =
  	(if no > 0
	 then compareNo houses (no, no-1, rulepair)
	 else true)
	andalso
	(if no < 4
	 then compareNo houses (no, no+1, rulepair)
	 else true)

  | invalid houses no (LeftOfRule rulepair) =
  	if no > 0
	then compareNo houses (no-1, no, rulepair)
	else matchNo houses (no, #1rulepair)


(*
 * val checkRulesForNo : house vector -> houseno -> bool
 * Check all rules for a house;
 * Returns true, when one rule was invalid.
 *)
fun checkRulesForNo (houses : house array) no =
	let
	  exception RuleError
	in
	  (map (fn rule => if invalid houses no rule then raise RuleError else ()) rules;
	   false)
	  handle RuleError => true
	end

(*
 * val checkAll : house vector -> bool
 * Check all rules;
 * return true if everything is ok.
 *)
fun checkAll (houses : house array) =
	let
	  exception RuleError
	in
	  (map (fn no => if checkRulesForNo houses no then raise RuleError else ()) [0,1,2,3,4];
	   true)
	  handle RuleError => false
	end


(*
 *
 * House printing for debugging
 *
 *)

fun valToString (0, SOME a) = Vector.sub (str_colors,  a)
  | valToString (1, SOME b) = Vector.sub (str_nations, b)
  | valToString (2, SOME c) = Vector.sub (str_drinks,  c)
  | valToString (3, SOME d) = Vector.sub (str_pets,    d)
  | valToString (4, SOME e) = Vector.sub (str_smokes,  e)
  | valToString _ = "-"

(*
 * Note:
 * Format needs SML NJ
 *)
fun printHouse no ((a, b, c, d, e) : house) =
	(
	  print (Format.format "%12d" [Format.LEFT (12, Format.INT no)]);
	  print (Format.format "%12s%12s%12s%12s%12s"
	  	(map (fn (x, y) => Format.LEFT (12, Format.STR (valToString (x, y))))
			[(0,a), (1,b), (2,c), (3,d), (4,e)]));
	  print ("\n")
	)

fun printHouses houses =
	(
	  print (Format.format "%12s" [Format.LEFT (12, Format.STR "House")]);
	  Vector.map (fn a => print (Format.format "%12s" [Format.LEFT (12, Format.STR a)]))
	  	str_attributes;
	  print "\n";
	  Array.foldli (fn (no, house, _) => printHouse no house) () houses
	)

(*
 *
 * Solving
 *
 *)

exception SolutionFound

fun search (houses : house array, used : bool Array2.array) (no : houseno, attr : attr) =
	let
	  val i = ref 0
	  val (nextno, nextattr) = if attr < 4 then (no, attr + 1) else (no + 1, 0)
	in
	  if isSome (getHouseVal houses (no, attr))
	  then
	  (
	    search (houses, used) (nextno, nextattr)
	  )
	  else
	  (
	    while (!i < 5)
	    do
	    (
	      if Array2.sub (used, attr, !i) then ()
	      else
	      (
	          Array2.update (used, attr, !i, true);
	          setHouseVal houses (no, attr, SOME (!i));

	          if checkAll houses then
	          (
	            if no = 4 andalso attr = 4
	            then raise SolutionFound
	            else search (houses, used) (nextno, nextattr)
	          )
	          else ();
	          Array2.update (used, attr, !i, false)
	      ); (* else *)
	      i := !i + 1
	    ); (* do *)
	    setHouseVal houses (no, attr, NONE)
	  ) (* else *)
	end

fun init () =
	let
	  val unknown : house = (NONE, NONE, NONE, NONE, NONE)
	  val houses  = Array.fromList [unknown, unknown, unknown, unknown, unknown]
	  val used    = Array2.array (5, 5, false)
	in
	  (houses, used)
	end

fun solve () =
	let
	  val (houses, used) = init()
	in
	  setHouseVal houses (2, Drink, SOME Milk);		(* #09 *)
	  Array2.update (used, Drink, Milk, true);
	  setHouseVal houses (0, Nation, SOME Norwegian);	(* #10 *)
	  Array2.update (used, Nation, Norwegian, true);
	  (search (houses, used) (0, 0); NONE)
	  handle SolutionFound => SOME houses
	end

(*
 *
 * Execution
 *
 *)

fun main () = let
	  val solution = solve()
	in
	  if isSome solution
	  then printHouses (valOf solution)
	  else print "No solution found!\n"
	end

```


{{out}}

```txt

- main();
House       Color       Nation      Drink       Pet         Smoke
0           Yellow      Norwegian   Water       Cats        Dunhill
1           Blue        Dane        Tea         Horse       Blend
2           Red         English     Milk        Birds       PallMall
3           Green       German      Coffee      Zebra       Prince
4           White       Swede       Beer        Dog         BlueMaster
val it = () : unit

```



## Tcl

{{trans|Python}}
{{tcllib|struct::list}}
```tcl
package require struct::list

# Implements the constants by binding them directly into the named procedures.
# This is much faster than the alternatives!
proc initConstants {args} {
    global {}
    set remap {}
    foreach {class elems} {
	Number {One Two Three Four Five}
	Color {Red Green Blue White Yellow}
	Drink {Milk Coffee Water Beer Tea}
	Smoke {PallMall Dunhill Blend BlueMaster Prince}
	Pet {Dog Cat Horse Bird Zebra}
	Nation {British Swedish Danish Norwegian German}
    } {
	set i -1
	foreach e $elems {lappend remap "\$${class}($e)" [incr i]}
	set ($class) $elems
    }
    foreach procedure $args {
	proc $procedure [info args $procedure] \
	    [string map $remap [info body $procedure]]
    }
}

proc isPossible {number color drink smoke pet} {
    if {[llength $number] && [lindex $number $Nation(Norwegian)] != $Number(One)} {
	return false
    } elseif {[llength $color] && [lindex $color $Nation(British)] != $Color(Red)} {
	return false
    } elseif {[llength $drink] && [lindex $drink $Nation(Danish)] != $Drink(Tea)} {
	return false
    } elseif {[llength $smoke] && [lindex $smoke $Nation(German)] != $Smoke(Prince)} {
	return false
    } elseif {[llength $pet] && [lindex $pet $Nation(Swedish)] != $Pet(Dog)} {
	return false
    }

    if {!([llength $number] && [llength $color] && [llength $drink] && [llength $smoke] && [llength $pet])} {
	return true
    }

    for {set i 0} {$i < 5} {incr i} {
	if {[lindex $color $i] == $Color(Green) && [lindex $drink $i] != $Drink(Coffee)} {
	    return false
	} elseif {[lindex $smoke $i] == $Smoke(PallMall) && [lindex $pet $i] != $Pet(Bird)} {
	    return false
	} elseif {[lindex $color $i] == $Color(Yellow) && [lindex $smoke $i] != $Smoke(Dunhill)} {
	    return false
	} elseif {[lindex $number $i] == $Number(Three) && [lindex $drink $i] != $Drink(Milk)} {
	    return false
	} elseif {[lindex $smoke $i] == $Smoke(BlueMaster) && [lindex $drink $i] != $Drink(Beer)} {
	    return false
	} elseif {[lindex $color $i] == $Color(Blue) && [lindex $number $i] != $Number(Two)} {
	    return false
	}

	for {set j 0} {$j < 5} {incr j} {
	    if {[lindex $color $i] == $Color(Green) && [lindex $color $j] == $Color(White) && [lindex $number $j] - [lindex $number $i] != 1} {
		return false
	    }

	    set diff [expr {abs([lindex $number $i] - [lindex $number $j])}]
	    if {[lindex $smoke $i] == $Smoke(Blend) && [lindex $pet $j] == $Pet(Cat) && $diff != 1} {
		return false
	    } elseif {[lindex $pet $i] == $Pet(Horse) && [lindex $smoke $j] == $Smoke(Dunhill) && $diff != 1} {
		return false
	    } elseif {[lindex $smoke $i] == $Smoke(Blend) && [lindex $drink $j] == $Drink(Water) && $diff != 1} {
		return false
	    }
	}
    }

    return true
}

proc showRow {t data} {
    upvar #0 ($t) elems
    puts [format "%6s: %12s%12s%12s%12s%12s" $t \
	      [lindex $elems [lindex $data 0]] \
	      [lindex $elems [lindex $data 1]] \
	      [lindex $elems [lindex $data 2]] \
	      [lindex $elems [lindex $data 3]] \
	      [lindex $elems [lindex $data 4]]]
}

proc main {} {
    set perms [struct::list permutations {0 1 2 3 4}]
    foreach number $perms {
	if {![isPossible $number {} {} {} {}]} continue
	foreach color $perms {
	    if {![isPossible $number $color {} {} {}]} continue
	    foreach drink $perms {
		if {![isPossible $number $color $drink {} {}]} continue
		foreach smoke $perms {
		    if {![isPossible $number $color $drink $smoke {}]} continue
		    foreach pet $perms {
			if {[isPossible $number $color $drink $smoke $pet]} {
			    puts "Found a solution:"
			    showRow Nation {0 1 2 3 4}
			    showRow Number $number
			    showRow Color  $color
			    showRow Drink  $drink
			    showRow Smoke  $smoke
			    showRow Pet    $pet
			    puts ""
			}
		    }
		}
	    }
	}
    }
}

initConstants isPossible
main
```

{{out}}

```txt

 Found a solution:
 Nation:      British     Swedish      Danish   Norwegian      German
 Number:        Three        Five         Two         One        Four
  Color:          Red       White        Blue      Yellow       Green
  Drink:         Milk        Beer         Tea       Water      Coffee
  Smoke:     PallMall  BlueMaster       Blend     Dunhill      Prince
    Pet:         Bird         Dog       Horse         Cat       Zebra

```



## VBA

{{trans|Phix}}

```vb
Option Base 1
Public Enum attr
    Colour = 1
    Nationality
    Beverage
    Smoke
    Pet
End Enum
Public Enum Drinks_
    Beer = 1
    Coffee
    Milk
    Tea
    Water
End Enum
Public Enum nations
    Danish = 1
    English
    German
    Norwegian
    Swedish
End Enum
Public Enum colors
    Blue = 1
    Green
    Red
    White
    Yellow
End Enum
Public Enum tobaccos
    Blend = 1
    BlueMaster
    Dunhill
    PallMall
    Prince
End Enum
Public Enum animals
    Bird = 1
    Cat
    Dog
    Horse
    Zebra
End Enum
Public permutation As New Collection
Public perm(5) As Variant
Const factorial5 = 120
Public Colours As Variant, Nationalities As Variant, Drinks As Variant, Smokes As Variant, Pets As Variant

Private Sub generate(n As Integer, A As Variant)
    If n = 1 Then
        permutation.Add A
    Else
        For i = 1 To n
            generate n - 1, A
            If n Mod 2 = 0 Then
                tmp = A(i)
                A(i) = A(n)
                A(n) = tmp
            Else
                tmp = A(1)
                A(1) = A(n)
                A(n) = tmp
            End If
        Next i
    End If
End Sub

Function house(i As Integer, name As Variant) As Integer
    Dim x As Integer
    For x = 1 To 5
        If perm(i)(x) = name Then
            house = x
            Exit For
        End If
    Next x
End Function

Function left_of(h1 As Integer, h2 As Integer) As Boolean
    left_of = (h1 - h2) = -1
End Function

Function next_to(h1 As Integer, h2 As Integer) As Boolean
    next_to = Abs(h1 - h2) = 1
End Function

Private Sub print_house(i As Integer)
    Debug.Print i & ": "; Colours(perm(Colour)(i)), Nationalities(perm(Nationality)(i)), _
        Drinks(perm(Beverage)(i)), Smokes(perm(Smoke)(i)), Pets(perm(Pet)(i))
End Sub
Public Sub Zebra_puzzle()
    Colours = [{"blue","green","red","white","yellow"}]
    Nationalities = [{"Dane","English","German","Norwegian","Swede"}]
    Drinks = [{"beer","coffee","milk","tea","water"}]
    Smokes = [{"Blend","Blue Master","Dunhill","Pall Mall","Prince"}]
    Pets = [{"birds","cats","dog","horse","zebra"}]
    Dim solperms As New Collection
    Dim solutions As Integer
    Dim b(5) As Integer, i As Integer
    For i = 1 To 5: b(i) = i: Next i
    'There are five houses.
    generate 5, b
    For c = 1 To factorial5
        perm(Colour) = permutation(c)
        'The green house is immediately to the left of the white house.
        If left_of(house(Colour, Green), house(Colour, White)) Then
            For n = 1 To factorial5
                perm(Nationality) = permutation(n)
                'The Norwegian lives in the first house.
                'The English man lives in the red house.
                'The Norwegian lives next to the blue house.
                If house(Nationality, Norwegian) = 1 _
                    And house(Nationality, English) = house(Colour, Red) _
                    And next_to(house(Nationality, Norwegian), house(Colour, Blue)) Then
                    For d = 1 To factorial5
                        perm(Beverage) = permutation(d)
                        'The Dane drinks tea.
                        'They drink coffee in the green house.
                        'In the middle house they drink milk.
                        If house(Nationality, Danish) = house(Beverage, Tea) _
                            And house(Beverage, Coffee) = house(Colour, Green) _
                            And house(Beverage, Milk) = 3 Then
                            For s = 1 To factorial5
                                perm(Smoke) = permutation(s)
                                'In the yellow house they smoke Dunhill.
                                'The German smokes Prince.
                                'The man who smokes Blue Master drinks beer.
                                'They Drink water in a house next to the house where they smoke Blend.
                                If house(Colour, Yellow) = house(Smoke, Dunhill) _
                                    And house(Nationality, German) = house(Smoke, Prince) _
                                    And house(Smoke, BlueMaster) = house(Beverage, Beer) _
                                    And next_to(house(Beverage, Water), house(Smoke, Blend)) Then
                                    For p = 1 To factorial5
                                        perm(Pet) = permutation(p)
                                        'The Swede has a dog.
                                        'The man who smokes Pall Mall has birds.
                                        'The man who smokes Blend lives in the house next to the house with cats.
                                        'In a house next to the house where they have a horse, they smoke Dunhill.
                                        If house(Nationality, Swedish) = house(Pet, Dog) _
                                            And house(Smoke, PallMall) = house(Pet, Bird) _
                                            And next_to(house(Smoke, Blend), house(Pet, Cat)) _
                                            And next_to(house(Pet, Horse), house(Smoke, Dunhill)) Then
                                            For i = 1 To 5
                                                print_house i
                                            Next i
                                            Debug.Print
                                            solutions = solutions + 1
                                            solperms.Add perm
                                        End If
                                    Next p
                                End If
                            Next s
                        End If
                    Next d
                End If
            Next n
        End If
    Next c
    Debug.Print Format(solutions, "@"); " solution" & IIf(solutions > 1, "s", "") & " found"
    For i = 1 To solperms.Count
        For j = 1 To 5
            perm(j) = solperms(i)(j)
        Next j
        Debug.Print "The " & Nationalities(perm(Nationality)(house(Pet, Zebra))) & " owns the Zebra"
    Next i
End Sub
```
{{out}}

```txt
1: yellow     Norwegian     water         Dunhill       cats
2: blue       Dane          tea           Blend         horse
3: red        English       milk          Pall Mall     birds
4: green      German        coffee        Prince        zebra
5: white      Swede         beer          Blue Master   dog

1 solution found
The German owns the Zebra
```


## zkl

The solution space is too large to brute force search so solutions are built up by solving for one or two variables, then solving that set for a one or two more variables until all the variables & constraints are tested.

Lists are used as associated arrays rather than hash tables. With only five entries, it doesn't really matter.

```zkl
var people,drinks,houses,smokes,pets; // lists treated as associated arrays
fcn c2 { people.find(English)==houses.find(Red) }
fcn c3 { people.find(Swede)==pets.find(Dog) }
fcn c4 { people.find(Dane)==drinks.find(Tea) }
fcn c5 { (houses.find(Green) + 1)==houses.find(White) }
fcn c5a{ houses.find(Green)!=4 }	// deduced constraint (from c5)
fcn c5b{ houses.find(White)!=0 }	// deduced constraint (from c5)
fcn c6 { drinks.find(Coffee)==houses.find(Green) }
fcn c7 { smokes.find(PallMall)==pets.find(Bird) }
fcn c8 { houses.find(Yellow)==smokes.find(Dunhill) }
fcn c9 { drinks[2]==Milk }  // 0,1,2,3,4
fcn c10{ people[0]==Norwegian }
fcn c11{ (smokes.find(Blend) - pets.find(Cat)).abs()==1 }
fcn c12{ (pets.find(Horse) - smokes.find(Dunhill)).abs()==1 }
fcn c13{ smokes.find(BlueMaster)==drinks.find(Beer) }
fcn c14{ people.find(German)==smokes.find(Prince) }
fcn c15{ (people.find(Norwegian) - houses.find(Blue)).abs()==1 }
fcn c16{ (drinks.find(Water) - smokes.find(Blend)).abs()==1 }
#<<<#//////////////////////////////////////////////////////////////////////
Showing a solution to c2,c5,c10,c15:
	|0	   1       2         3         4
--------+-------------------------------------------
houses:	|Yellow    Blue    Red       Green     White
people:	|Norwegian Dane    English   German    Swede
#<<<#//////////////////////////////////////////////////////////////////////

const Blue =0,Green     =1,Red    =2,White    =3,Yellow=4,
      Dane =0,English   =1,German =2,Norwegian=3,Swede =4,
      Beer =0,Coffee    =1,Milk   =2,Tea      =3,Water =4,
      Blend=0,BlueMaster=1,Dunhill=2,PallMall =3,Prince=4,
      Bird =0,Cat       =1,Dog    =2,Horse    =3,Zebra =4;
perm5:=T(0,1,2,3,4) : Utils.Helpers.permute(_); // 120 sets

constraints:=T(c2,c3,c4,c5,c5a,c5b,c6,c7,c8,c9,c10,c11,c12,c13,c14,c15,c16);
constraints1:=T(c2,c5,c10,c15); // houses,people: 12 solutions
constraints2:=T(c4,c6,c9);      // houses,people,drinks: down to 8 solutions
foreach _houses,_people in (perm5,perm5){ houses,people=_houses,_people;
   if(not constraints1.runNFilter(False)){ // all constraints are True
      foreach _drinks in (perm5){ drinks=_drinks;
	 if(not constraints2.runNFilter(False)){
	    foreach _smokes,_pets in (perm5,perm5){ smokes,pets=_smokes,_pets;
	       if(not constraints.runNFilter(False)) printSolution();
	    }// smokes,pets
	 }
      } // drinks
   } // houses,people
}
fcn printSolution{
   var titles=T("Houses:","People:","Drinks:","Smokes:","Pets:"),
       names=T(
          T("Blue", "Green",      "Red",    "White",    "Yellow",),
	  T("Dane", "English",    "German", "Norwegian","Swede",),
	  T("Beer", "Coffee",     "Milk",   "Tea",      "Water",),
	  T("Blend","Blue Master","Dunhill","Pall Mall","Prince",),
	  T("Bird", "Cat",        "Dog",    "Horse",    "Zebra",) ),
   ;
   fmt:=("%-7s " + "%-11s "*5).fmt;
   foreach list,title,names in (T(houses,people,drinks,smokes,pets)
	   .zip(titles,names))
      { println(list.apply(names.get):fmt(title,_.xplode())) }
}
```

{{out}}

```txt

Houses: Yellow      Blue        Red         Green       White
People: Norwegian   Dane        English     German      Swede
Drinks: Water       Tea         Milk        Coffee      Beer
Smokes: Dunhill     Blend       Pall Mall   Prince      Blue Master
Pets:   Cat         Horse       Bird        Zebra       Dog

```

