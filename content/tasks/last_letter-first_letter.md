+++
title = "Last letter-first letter"
description = ""
date = 2019-04-22T21:05:28Z
aliases = []
[extra]
id = 9878
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "ada",
  "bacon",
  "basic256",
  "bbc_basic",
  "bracmat",
  "c",
  "clojure",
  "common_lisp",
  "csharp",
  "d",
  "delphi",
  "elixir",
  "erlang",
  "go",
  "haskell",
  "j",
  "java",
  "javascript",
  "jq",
  "julia",
  "kotlin",
  "mathematica",
  "oorexx",
  "openedge_progress",
  "perl",
  "perl_6",
  "phix",
  "picolisp",
  "prolog",
  "python",
  "racket",
  "rexx",
  "ring",
  "ruby",
  "scala",
  "seed7",
  "sidef",
  "tcl",
  "ursala",
  "vbscript",
  "yabasic",
  "zkl",
]
+++

A certain children's game involves starting with a word in a particular category.   Each participant in turn says a word, but that word must begin with the final letter of the previous word.   Once a word has been given, it cannot be repeated.   If an opponent cannot give a word in the category, they fall out of the game.


For example, with   "animals"   as the category,

```txt

Child 1: dog
Child 2: goldfish
Child 1: hippopotamus
Child 2: snake
...

```



## Task

Take the following selection of 70 English Pokemon names   (extracted from   [[wp:List of Pokémon|Wikipedia's list of Pokemon]])   and generate the/a sequence with the highest possible number of Pokemon names where the subsequent name starts with the final letter of the preceding name.

No Pokemon name is to be repeated.


```txt

audino bagon baltoy banette bidoof braviary bronzor carracosta charmeleon
cresselia croagunk darmanitan deino emboar emolga exeggcute gabite
girafarig gulpin haxorus heatmor heatran ivysaur jellicent jumpluff kangaskhan
kricketune landorus ledyba loudred lumineon lunatone machamp magnezone mamoswine
nosepass petilil pidgeotto pikachu pinsir poliwrath poochyena porygon2
porygonz registeel relicanth remoraid rufflet sableye scolipede scrafty seaking
sealeo silcoon simisear snivy snorlax spoink starly tirtouga trapinch treecko
tyrogue vigoroth vulpix wailord wartortle whismur wingull yamask

```



Extra brownie points for dealing with the full list of   646   names.





## Ada



```Ada
with Ada.Containers.Indefinite_Vectors, Ada.Text_IO;

procedure Lalefile is

   package Word_Vec is new Ada.Containers.Indefinite_Vectors
     (Index_Type   => Positive,
      Element_Type => String);

   use type Word_Vec.Vector, Ada.Containers.Count_Type;

   type Words_Type is array (Character) of Word_Vec.Vector;

   procedure Read(Words: out Words_Type) is
      F: Ada.Text_IO.File_Type;
   begin
      Ada.Text_IO.Open(File => F,
                       Name => "pokemon70.txt",
                       Mode => Ada.Text_IO.In_File);
      loop
         declare
            Word: String := Ada.Text_IO.Get_Line(F);
         begin
            exit when Word = "";
            Words(Word(Word'First)).Append(Word);
         end;
      end loop;
   exception
      when Ada.Text_IO.End_Error => null;
   end Read;

   procedure Write (List: Word_Vec.Vector; Prefix: String := "   ") is
      Copy: Word_Vec.Vector := List;
   begin
      loop
         exit when Copy.Is_Empty;
         Ada.Text_IO.Put_Line(Prefix & Copy.First_Element);
         Copy.Delete_First;
      end loop;
   end Write;

   function Run(Start: Character; Words: Words_Type) return Word_Vec.Vector is
      Result: Word_Vec.Vector := Word_Vec.Empty_Vector;
   begin
      for I in Words(Start).First_Index .. Words(Start).Last_Index loop
         declare
            Word: String := Words(Start).Element(I);
            Dupl: Words_Type := Words;
            Alternative : Word_Vec.Vector;
         begin
            Dupl(Start).Delete(I);
            Alternative := Word & Run(Word(Word'Last), Dupl);
            if Alternative.Length > Result.Length then
               Result := Alternative;
            end if;
         end;
      end loop;
      return Result;
   end Run;

   W: Words_Type;
   A_Vector: Word_Vec.Vector;
   Best: Word_Vec.Vector := Word_Vec.Empty_Vector;

begin
   Read(W);
   Ada.Text_IO.Put("Processing ");
   for Ch in Character range 'a' .. 'z' loop
      Ada.Text_IO.Put(Ch & ", ");
      A_Vector := Run(Ch, W);
      if A_Vector.Length > Best.Length then
         Best := A_Vector;
      end if;
   end loop;
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line("Length of longest Path:" &
                          Integer'Image(Integer(Best.Length)));
   Ada.Text_IO.Put_Line("One such path:");
   Write(Best);
end Lalefile;
```


Output:
```txt
Processing a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z,
Length of longest Path: 23
One such path:
   machamp
   petilil
   landorus
   scrafty
   yamask
   kricketune
   emboar
   registeel
   loudred
   darmanitan
   nosepass
   simisear
   relicanth
   heatmor
   rufflet
   trapinch
   haxorus
   seaking
   girafarig
   gabite
   exeggcute
   emolga
   audino
```



## BaCon

Naive implementation showing the algorithm.

```freebasic
all$ = "audino bagon baltoy banette bidoof braviary bronzor carracosta charmeleon " \
"cresselia croagunk darmanitan deino emboar emolga exeggcute gabite " \
"girafarig gulpin haxorus heatmor heatran ivysaur jellicent jumpluff kangaskhan " \
"kricketune landorus ledyba loudred lumineon lunatone machamp magnezone mamoswine " \
"nosepass petilil pidgeotto pikachu pinsir poliwrath poochyena porygon2 " \
"porygonz registeel relicanth remoraid rufflet sableye scolipede scrafty seaking " \
"sealeo silcoon simisear snivy snorlax spoink starly tirtouga trapinch treecko " \
"tyrogue vigoroth vulpix wailord wartortle whismur wingull yamask"

SUB check(list$, rest$)

    LOCAL x

    FOR x = 1 TO AMOUNT(rest$)
        IF RIGHT$(list$, 1) = LEFT$(TOKEN$(rest$, x), 1) THEN check(APPEND$(list$, 0, TOKEN$(rest$, x)), DEL$(rest$, x))
    NEXT

    IF AMOUNT(list$) > total THEN
        total = AMOUNT(list$)
        result$ = list$
    END IF

END SUB

FOR z = 1 TO AMOUNT(all$)
    CALL check(TOKEN$(all$, z), DEL$(all$,z))
NEXT

PRINT total, ": ", result$

PRINT NL$, "Speed: ", TIMER, " msecs."
```

```txt

23: machamp petilil landorus scrafty yamask kricketune emboar registeel loudred darmanitan nosepass simisear relicanth heatmor rufflet trapinch haxorus seaking girafarig gabite exeggcute emolga audino

Speed: 662734 msecs.

```

Optimized implementation. The idea is to quantify the equations.

```freebasic
all$ = "audino bagon baltoy banette bidoof braviary bronzor carracosta charmeleon " \
"cresselia croagunk darmanitan deino emboar emolga exeggcute gabite " \
"girafarig gulpin haxorus heatmor heatran ivysaur jellicent jumpluff kangaskhan " \
"kricketune landorus ledyba loudred lumineon lunatone machamp magnezone mamoswine " \
"nosepass petilil pidgeotto pikachu pinsir poliwrath poochyena porygon2 " \
"porygonz registeel relicanth remoraid rufflet sableye scolipede scrafty seaking " \
"sealeo silcoon simisear snivy snorlax spoink starly tirtouga trapinch treecko " \
"tyrogue vigoroth vulpix wailord wartortle whismur wingull yamask"

SPLIT all$ TO name$ SIZE MaxSize

DECLARE start, end, used, result ARRAY MaxSize

FOR y = 0 TO MaxSize-1
    start[y] = ASC(LEFT$(name$[y], 1))
    end[y] = ASC(RIGHT$(name$[y], 1))
    used[y] = -1
NEXT

FOR i = 0 TO MaxSize-1
    used[i] = 0
    CALL check(used, i, 1)
    used[i] = -1
NEXT

PRINT maxtotal, ": ";
FOR a = 0 TO maxtotal-1
    FOR y = 0 TO MaxSize-1
        IF result[y] = a THEN PRINT name$[y]," ";
    NEXT
NEXT

PRINT NL$, "Speed: ", TIMER, " msecs."

SUB check(ya[], ultim, nr)

    LOCAL x

    FOR x = 0 TO MaxSize-1
        IF end[ultim] = start[x] AND ya[x] = -1 THEN
            ya[x] = nr
            CALL check(ya, x, nr+1)
            ya[x] = -1
        END IF

        IF nr > maxtotal THEN
            maxtotal = nr
            OPTION MEMTYPE long
            COPY ya TO result SIZE MaxSize
        END IF
    NEXT

END SUB

```

```txt

23: machamp petilil landorus scrafty yamask kricketune emboar registeel loudred darmanitan nosepass simisear relicanth heatmor rufflet trapinch haxorus seaking girafarig gabite exeggcute emolga audino

Speed: 818 msecs.

```



## BASIC256


```basic256
dim names$(1)
names$ = { "audino", "bagon", "baltoy", "banette", "bidoof", "braviary", "bronzor", "carracosta", "charmeleon", "cresselia", "croagunk", "darmanitan", "deino", "emboar", "emolga", "exeggcute", "gabite", "girafarig", "gulpin", "haxorus", "heatmor", "heatran", "ivysaur", "jellicent", "jumpluff", "kangaskhan", "kricketune", "landorus", "ledyba", "loudred", "lumineon", "lunatone", "machamp", "magnezone", "mamoswine", "nosepass", "petilil", "pidgeotto", "pikachu", "pinsir", "poliwrath", "poochyena", "porygon2", "porygonz", "registeel", "relicanth", "remoraid", "rufflet", "sableye", "scolipede", "scrafty", "seaking", "sealeo", "silcoon", "simisear", "snivy", "snorlax", "spoink", "starly", "tirtouga", "trapinch", "treecko", "tyrogue", "vigoroth", "vulpix", "wailord", "wartortle", "whismur", "wingull", "yamask" }

global names$, lnames, index, maxlen, first, last
maxlen = 0

lnames = names$[?]-1
dim index(names$[?])
dim first(names$[?])
dim last(names$[?])
for t = 0 to lnames
	index[t] = t
	last[t] = asc(right(names$[t],1))
	first[t] = asc(left(names$[t],1))
next t


# try each name as the first name on the list
for t = 0 to lnames
   call swapindex(0,t)
   call downlevel(1)
   call swapindex(0,t)
next t

end

subroutine downlevel(lev)
   #print n$[?] + " " + lev
   if lev <= lnames then
      for t = lev to lnames
         if last[index[lev-1]] = first[index[t]] then
            call swapindex(lev,t)
            if lev >= maxlen then
               maxlen = lev
               call showsolution(lev)
            end if
            call downlevel(lev+1)
            call swapindex(lev,t)
         end if
      next t
   end if
end subroutine

subroutine showsolution(l)
   print l+1;
   for t = 0 to l
      print " " + names$[index[t]];
   next t
   print
end subroutine

subroutine swapindex(a, b)
   # swap element a and bin in the array index (used to swap names$)
   t = index[a]
   index[a] = index[b]
   index[b] = t
end subroutine

```


Output:

```txt
2 bagon nosepass
3 bagon nosepass sableye
4 bagon nosepass sableye emboar
5 bagon nosepass sableye emboar registeel
...
23 machamp pinsir rufflet trapinch heatmor remoraid darmanitan nosepass starly yamask kricketune emboar relicanth haxorus simisear registeel landorus seaking girafarig gabite exeggcute emolga audino
23 machamp pinsir rufflet trapinch heatmor remoraid darmanitan nosepass starly yamask kricketune exeggcute emboar registeel landorus simisear relicanth haxorus seaking girafarig gabite emolga audino
23 machamp pinsir rufflet trapinch heatmor remoraid darmanitan nosepass starly yamask kricketune exeggcute emboar relicanth haxorus simisear registeel landorus seaking girafarig gabite emolga audino
```



## BBC BASIC

```bbcbasic
      DIM names$(69)
      names$() = "audino", "bagon", "baltoy", "banette", \
      \ "bidoof", "braviary", "bronzor", "carracosta", "charmeleon", \
      \ "cresselia", "croagunk", "darmanitan", "deino", "emboar", \
      \ "emolga", "exeggcute", "gabite", "girafarig", "gulpin", \
      \ "haxorus", "heatmor", "heatran", "ivysaur", "jellicent", \
      \ "jumpluff", "kangaskhan", "kricketune", "landorus", "ledyba", \
      \ "loudred", "lumineon", "lunatone", "machamp", "magnezone", \
      \ "mamoswine", "nosepass", "petilil", "pidgeotto", "pikachu", \
      \ "pinsir", "poliwrath", "poochyena", "porygon2", "porygonz", \
      \ "registeel", "relicanth", "remoraid", "rufflet", "sableye", \
      \ "scolipede", "scrafty", "seaking", "sealeo", "silcoon", \
      \ "simisear", "snivy", "snorlax", "spoink", "starly", "tirtouga", \
      \ "trapinch", "treecko", "tyrogue", "vigoroth", "vulpix", \
      \ "wailord", "wartortle", "whismur", "wingull", "yamask"

      maxPathLength% = 0
      maxPathLengthCount% = 0
      maxPathExample$ = ""

      FOR i% = 0 TO DIM(names$(),1)
        SWAP names$(0), names$(i%)
        PROClastfirst(names$(), 1)
        SWAP names$(0), names$(i%)
      NEXT
      PRINT "Maximum length = " ; maxPathLength%
      PRINT "Number of solutions with that length = " ; maxPathLengthCount%
      PRINT "One such solution: " ' maxPathExample$
      END

      DEF PROClastfirst(names$(), offset%)
      LOCAL i%, l%
      IF offset% > maxPathLength% THEN
        maxPathLength% = offset%
        maxPathLengthCount% = 1
      ELSE IF offset% = maxPathLength% THEN;
        maxPathLengthCount% += 1
        maxPathExample$ = ""
        FOR i% = 0 TO offset%-1
          maxPathExample$ += names$(i%) + CHR$13 + CHR$10
        NEXT
      ENDIF
      l% = ASCRIGHT$(names$(offset% - 1))
      FOR i% = offset% TO DIM(names$(),1)
        IF ASCnames$(i%) = l% THEN
          SWAP names$(i%), names$(offset%)
          PROClastfirst(names$(), offset%+1)
          SWAP names$(i%), names$(offset%)
        ENDIF
      NEXT
      ENDPROC
```

'''Output:'''

```txt

Maximum length = 23
Number of solutions with that length = 1248
One such solution:
machamp
pinsir
rufflet
trapinch
heatmor
remoraid
darmanitan
nosepass
starly
yamask
kricketune
exeggcute
emboar
relicanth
haxorus
simisear
registeel
landorus
seaking
girafarig
gabite
emolga
audino

```



## Bracmat


### Naive


```bracmat
(   audino bagon baltoy banette bidoof braviary bronzor
    carracosta charmeleon cresselia croagunk darmanitan deino
    emboar emolga exeggcute gabite girafarig gulpin haxorus
    heatmor heatran ivysaur jellicent jumpluff kangaskhan
    kricketune landorus ledyba loudred lumineon lunatone machamp
    magnezone mamoswine nosepass petilil pidgeotto pikachu
    pinsir poliwrath poochyena porygon2 porygonz registeel
    relicanth remoraid rufflet sableye scolipede scrafty seaking
    sealeo silcoon simisear snivy snorlax spoink starly
    tirtouga trapinch treecko tyrogue vigoroth vulpix wailord
    wartortle whismur wingull yamask
  : ?names
& 0:?max
& :?sequence
& ( lalefile
  =   done todo A M Z Length first
    .   !arg:(!done.)&!done:?sequence
      |   !arg:(.?todo)
        & (   !todo
            :   ?A
                %@?M
                (?Z&lalefile$(!M.!A !Z)&~)
          |
          )
      |   !arg:(@(%:? @?first) ?:?done.?todo)
        & :?M
        & (   !todo
            :   ?A
                @(%:!first ?:?M)
                ( ?Z
                & lalefile$(!M !done.!A !Z)
                & ~
                )
          |   !M:
            & !done:? [?Length
            & !Length:>!max:?max
            & !done:?sequence
          |
          )
  )
& lalefile$(.!names)
& out$("Length:" !max "Sequence:" !sequence)
);
```

Output (read from bottom to top):

```txt
  Length:
  23
  Sequence:
  audino
  emolga
  exeggcute
  gabite
  girafarig
  seaking
  haxorus
  trapinch
  rufflet
  heatmor
  relicanth
  simisear
  nosepass
  darmanitan
  loudred
  registeel
  emboar
  kricketune
  yamask
  scrafty
  landorus
  petilil
  machamp
```


### Optimized

Optimizations:

The <code>whl</code> loop transforms the flat list of names to, conceptually, a search tree with nodes at three levels. The lowest level contains the names. The top level contains the word's first letter and the second level contains its last letter. Words starting with a specific letter are all children of one single top node, speeding up search for candidates. Under a second level node all words have the same letter at the start and the same letter at the end. When looking for candidates it always suffices to take the first word and ignore the rest. This optimization eliminates all solutions that merely are the result of swapping pairs of words with the same begin and end. Notice that the tree is built using the 'smart' binary operators <code>*</code>, <code>^</code>, <code>+</code> and <code>\L</code> (logarithm). Bracmat uses the commutative, distributive and associative laws to transform expressions containing these operators to canonical forms that fit the requiremens of the search tree. For example, the words in the list <code>sableye scolipede scrafty seaking sealeo silcoon simisear snivy snorlax spoink starly tirtouga trapinch treecko tyrogue</code> end up in this tree:

```txt
    s
  ^ ( (.e)\L(sableye*scolipede)
    + (.g)\Lseaking
    + (.k)\Lspoink
    + (.n)\Lsilcoon
    + (.o)\Lsealeo
    + (.r)\Lsimisear
    + (.x)\Lsnorlax
    + (.y)\L(scrafty*snivy*starly)
    )
*   t
  ^ ( (.a)\Ltirtouga
    + (.e)\Ltyrogue
    + (.h)\Ltrapinch
    + (.o)\Ltreecko
    )
```


Another, less important, optimization is the way in which the last letter of a name is found, using the position pattern <code>[</code> rather than the pattern that matches at most one letter, <code>@</code>.

The optimized version is about 4.5 times faster than the naive version.


```bracmat
(   audino bagon baltoy banette bidoof braviary bronzor
    carracosta charmeleon cresselia croagunk darmanitan deino
    emboar emolga exeggcute gabite girafarig gulpin haxorus
    heatmor heatran ivysaur jellicent jumpluff kangaskhan
    kricketune landorus ledyba loudred lumineon lunatone machamp
    magnezone mamoswine nosepass petilil pidgeotto pikachu
    pinsir poliwrath poochyena porygon2 porygonz registeel
    relicanth remoraid rufflet sableye scolipede scrafty seaking
    sealeo silcoon simisear snivy snorlax spoink starly
    tirtouga trapinch treecko tyrogue vigoroth vulpix wailord
    wartortle whismur wingull yamask
  : ?names
& 1:?newnames
&   whl
  ' ( !names:@(%?name:%@?first ? @?last) ?names
    & !first^(.!last)\L!name*!newnames:?newnames
    )
& !newnames:?names
& 0:?max
& :?sequence
& ( lalefile
  =     done todo A M Z Length first
      , Ms a z last candidates
    .   !arg:(!done.)&!done:?sequence
      |   !arg:(.?todo)
        & (   !todo
            :   ?A
              * %?first^?candidates
              * ( ?Z
                &   !candidates
                  :   ?a
                    + ?last\L(%?M*?Ms)
                    + ( ?z
                      & lalefile$(!M.!A*!first^(!a+!last\L!Ms+!z)*!Z)
                      & ~
                      )
                )
          |
          )
      |   !arg:(@(%:? [-2 ?first) ?:?done.?todo)
        & :?M
        & (   !todo:?A*!first^%?candidates*?Z
            &   !candidates
              :   ?a
                + ?last\L(%?M*?Ms)
                + ( ?z
                  &   lalefile
                    $ (!M !done.!A*!first^(!a+!last\L!Ms+!z)*!Z)
                  & ~
                  )
          |   !M:
            & !done:? [?Length
            & !Length:>!max:?max
            & !done:?sequence
          |
          )
  )
& lalefile$(.!names)
& out$("Length:" !max "Sequence:" !sequence)
);
```

Output (read from bottom to top):

```txt
  Length:
  23
  Sequence:
  audino
  emolga
  kricketune
  yamask
  scrafty
  haxorus
  trapinch
  rufflet
  simisear
  landorus
  registeel
  heatmor
  relicanth
  emboar
  exeggcute
  gabite
  girafarig
  seaking
  nosepass
  darmanitan
  loudred
  petilil
  machamp
```



## C

From the D version.

```cpp
#include <iostream>
#include <string.h>
#include <stdio.h>
#include <inttypes.h>

typedef struct {
    uint16_t index;
    char last_char, first_char;
} Ref;

Ref* longest_path_refs;
size_t longest_path_refs_len;

Ref* refs;
size_t refs_len;

size_t n_solutions;

const char** longest_path;
size_t longest_path_len;


/// tally statistics
void search(size_t curr_len) {
    if (curr_len == longest_path_refs_len) {
        n_solutions++;
    } else if (curr_len > longest_path_refs_len) {
        n_solutions = 1;
        longest_path_refs_len = curr_len;
        memcpy(longest_path_refs, refs, curr_len * sizeof(Ref));
    }

    // recursive search
    intptr_t last_char = refs[curr_len - 1].last_char;
    for (size_t i = curr_len; i < refs_len; i++)
        if (refs[i].first_char == last_char) {
            Ref aux = refs[curr_len];
            refs[curr_len] = refs[i];
            refs[i] = aux;
            search(curr_len + 1);
            refs[i] = refs[curr_len];
            refs[curr_len] = aux;
        }
}

void find_longest_chain(const char* items[],
                        size_t items_len) {
    refs_len = items_len;
    refs = calloc(refs_len, sizeof(Ref));

    // enough space for all items
    longest_path_refs_len = 0;
    longest_path_refs = calloc(refs_len, sizeof(Ref));

    for (size_t i = 0; i < items_len; i++) {
        size_t itemsi_len = strlen(items[i]);
        if (itemsi_len <= 1)
            exit(1);
        refs[i].index = (uint16_t)i;
        refs[i].last_char = items[i][itemsi_len - 1];
        refs[i].first_char = items[i][0];
    }

    // try each item as possible start
    for (size_t i = 0; i < items_len; i++) {
        Ref aux = refs[0];
        refs[0] = refs[i];
        refs[i] = aux;
        search(1);
        refs[i] = refs[0];
        refs[0] = aux;
    }

    longest_path_len = longest_path_refs_len;
    longest_path = calloc(longest_path_len, sizeof(const char*));
    for (size_t i = 0; i < longest_path_len; i++)
        longest_path[i] = items[longest_path_refs[i].index];

    free(longest_path_refs);
    free(refs);
}

int main() {
    const char* pokemon[] = {"audino", "bagon", "baltoy", "banette",
    "bidoof", "braviary", "bronzor", "carracosta", "charmeleon",
    "cresselia", "croagunk", "darmanitan", "deino", "emboar",
    "emolga", "exeggcute", "gabite", "girafarig", "gulpin",
    "haxorus", "heatmor", "heatran", "ivysaur", "jellicent",
    "jumpluff", "kangaskhan", "kricketune", "landorus", "ledyba",
    "loudred", "lumineon", "lunatone", "machamp", "magnezone",
    "mamoswine", "nosepass", "petilil", "pidgeotto", "pikachu",
    "pinsir", "poliwrath", "poochyena", "porygon2", "porygonz",
    "registeel", "relicanth", "remoraid", "rufflet", "sableye",
    "scolipede", "scrafty", "seaking", "sealeo", "silcoon",
    "simisear", "snivy", "snorlax", "spoink", "starly", "tirtouga",
    "trapinch", "treecko", "tyrogue", "vigoroth", "vulpix",
    "wailord", "wartortle", "whismur", "wingull", "yamask"};
    size_t pokemon_len = sizeof(pokemon) / sizeof(pokemon[0]);

    find_longest_chain(pokemon, pokemon_len);
    printf("Maximum path length: %u\n", longest_path_len);
    printf("Paths of that length: %u\n", n_solutions);
    printf("Example path of that length:\n");
    for (size_t i = 0; i < longest_path_len; i += 7) {
        printf("  ");
        for (size_t j = i; j < (i+7) && j < longest_path_len; j++)
            printf("%s ", longest_path[j]);
        printf("\n");
    }

    free(longest_path);

    return 0;
}
```

Output:

```txt
Maximum path length: 23
Paths of that length: 1248
Example path of that length:
  machamp petilil landorus scrafty yamask kricketune emboar
  registeel loudred darmanitan nosepass simisear relicanth heatmor
  rufflet trapinch haxorus seaking girafarig gabite exeggcute
  emolga audino
```

Runtime: about 0.49 seconds, gcc compiler.


### Approximate

For dealing with full list (646 names), here's an approximate method.  Names are restricted to begin and end with a lower case letter, so for example in my input file "porygon2" is written as "porygon-two".  It finds some chains with 300-odd length for 646 names, and found a chain with 23 for the 70 names (by luck, that is), and since it's basically a one-pass method, running time is next to none.  C99 code.

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define forall(i, n) for (int i = 0; i < n; i++)
typedef struct edge { char s, e, *str; struct edge *lnk; } edge;
typedef struct { edge* e[26]; int nin, nout, in[26], out[26];} node;
typedef struct { edge *e, *tail; int len, has[26]; } chain;

node nodes[26];
edge *names, **tmp;
int n_names;

/* add edge to graph */
void store_edge(edge *g)
{
	if (!g) return;
	int i = g->e, j = g->s;
	node *n = nodes + j;

	g->lnk = n->e[i];

	n->e[i] = g, n->out[i]++, n->nout++;
	n = nodes + i, n->in[j]++, n->nin++;
}

/* unlink an edge between nodes i and j, and return the edge */
edge* remove_edge(int i, int j)
{
	node *n = nodes + i;
	edge *g = n->e[j];
	if (g) {
		n->e[j] = g->lnk;
		g->lnk = 0;
		n->out[j]--, n->nout--;

		n = nodes + j;
		n->in[i]--;
		n->nin--;
	}
	return g;
}

void read_names()
{
	FILE *fp = fopen("poke646", "rt");
	int i, len;
	char *buf;
	edge *p;

	if (!fp) abort();

	fseek(fp, 0, SEEK_END);
	len = ftell(fp);
	buf = malloc(len + 1);
	fseek(fp, 0, SEEK_SET);
	fread(buf, 1, len, fp);
	fclose(fp);

	buf[len] = 0;
	for (n_names = i = 0; i < len; i++)
		if (isspace(buf[i]))
			buf[i] = 0, n_names++;

	if (buf[len-1]) n_names++;

	memset(nodes, 0, sizeof(node) * 26);
	tmp = calloc(n_names, sizeof(edge*));

	p = names = malloc(sizeof(edge) * n_names);
	for (i = 0; i < n_names; i++, p++) {
		if (i)	p->str = names[i-1].str + len + 1;
		else	p->str = buf;

		len = strlen(p->str);
		p->s = p->str[0] - 'a';
		p->e = p->str[len-1] - 'a';
		if (p->s < 0 || p->s >= 26 || p->e < 0 || p->e >= 26) {
			printf("bad name %s: first/last char must be letter\n",
				p->str);
			abort();
		}
	}
	printf("read %d names\n", n_names);
}

void show_chain(chain *c)
{
	printf("%d:", c->len);
	for (edge * e = c->e; e || !putchar('\n'); e = e->lnk)
		printf(" %s", e->str);
}

/* Which next node has most enter or exit edges. */
int widest(int n, int out)
{
	if (nodes[n].out[n]) return n;

	int mm = -1, mi = -1;
	forall(i, 26) {
		if (out) {
			if (nodes[n].out[i] && nodes[i].nout > mm)
				mi = i, mm = nodes[i].nout;
		} else {
			if (nodes[i].out[n] && nodes[i].nin > mm)
				mi = i, mm = nodes[i].nin;
		}
	}

	return mi;
}

void insert(chain *c, edge *e)
{
	e->lnk = c->e;
	if (!c->tail) c->tail = e;
	c->e = e;
	c->len++;
}

void append(chain *c, edge *e)
{
	if (c->tail) c->tail->lnk = e;
	else c->e = e;
	c->tail = e;
	c->len++;
}

edge * shift(chain *c)
{
	edge *e = c->e;
	if (e) {
		c->e = e->lnk;
		if (!--c->len) c->tail = 0;
	}
	return e;
}

chain* make_chain(int s)
{
	chain *c = calloc(1, sizeof(chain));

	/* extend backwards */
	for (int i, j = s; (i = widest(j, 0)) >= 0; j = i)
		insert(c, remove_edge(i, j));

	/* extend forwards */
	for (int i, j = s; (i = widest(j, 1)) >= 0; j = i)
		append(c, remove_edge(j, i));

	for (int step = 0;; step++) {
		edge *e = c->e;

		for (int i = 0; i < step; i++)
			if (!(e = e->lnk)) break;
		if (!e) return c;

		int n = 0;
		for (int i, j = e->s; (i = widest(j, 0)) >= 0; j = i) {
			if (!(e = remove_edge(i, j))) break;
			tmp[n++] = e;
		}

		if (n > step) {
			forall(i, step) store_edge(shift(c));
			forall(i, n) insert(c, tmp[i]);
			step = -1;
		} else while (--n >= 0)
			store_edge(tmp[n]);
	}
	return c;
}

int main(void)
{
	int best = 0;
	read_names();

	forall(i, 26) {
		/* rebuild the graph */
		memset(nodes, 0, sizeof(nodes));
		forall(j, n_names) store_edge(names + j);

		/* make a chain from node i */
		chain *c = make_chain(i);
		if (c->len > best) {
			show_chain(c);
			best = c->len;
		}
		free(c);
	}

	printf("longest found: %d\n", best);
	return 0;
}
```
output<lang>read 646 names
307: voltorb breloom magikarp palpito...
308: voltorb bayleef forretress swinub b...
310: voltorb bayleef forretress sw...
312: voltorb breloom mandibuzz zek...
320: voltorb beldum mandibuzz zekrom m...
322: voltorb beldum mandibuzz zekrom murk...
323: voltorb breloom mandibuzz zekr...
longest found: 323
```



## Clojure


```clojure
(ns rosetta-code.last-letter-first-letter
  (:require clojure.string))

(defn by-first-letter
  "Returns a map from letters to a set of words that start with that letter"
  [words]
  (into {} (map (fn [[k v]]
                  [k (set v)]))
        (group-by first words)))

(defn longest-path-from
  "Find a longest path starting at word, using only words-by-first-letter for successive words.
  Returns a pair of [length list-of-words] to describe the path."
  [word words-by-first-letter]
  (let [words-without-word (update words-by-first-letter (first word)
                                   disj word)
        next-words (words-without-word (last word))]
    (if (empty? next-words)
      [1 [word]]
      (let [sub-paths (map #(longest-path-from % words-without-word) next-words)
            [length words-of-path] (apply max-key first sub-paths)]
        [(inc length) (cons word words-of-path)]))))

(defn longest-word-chain
  "Find a longest path among the words in word-list, by performing a longest path search
  starting at each word in the list."
  [word-list]
  (let [words-by-letter (by-first-letter word-list)]
    (apply max-key first
           (pmap #(longest-path-from % words-by-letter)
                 word-list))))

(defn word-list-from-file [file-name]
  (let [contents (slurp file-name)
        words (clojure.string/split contents #"[ \n]")]
    (filter #(not (empty? %)) words)))

(time (longest-word-chain (word-list-from-file "pokemon.txt")))
```

Evaluating the last line:

```clojure
"Elapsed time: 2867.337816 msecs"
[23
 ("machamp"
  "pinsir"
  "relicanth"
  "heatmor"
  "registeel"
  "landorus"
  "seaking"
  "girafarig"
  "gabite"
  "exeggcute"
  "emboar"
  "rufflet"
  "trapinch"
  "haxorus"
  "simisear"
  "remoraid"
  "darmanitan"
  "nosepass"
  "scrafty"
  "yamask"
  "kricketune"
  "emolga"
  "audino")]
```

It initially ran in about 5 seconds, then I changed <code>map</code> to <code>pmap</code> (parallel map) in <code>longest-word-chain</code>.
This gave a nice speedup for a dual core laptop; the speedup for parallel searches was over 3x on a server.


## C#


```c#
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace ConsoleApplication1
{
    class Program
    {
        static void Main(string[] args)
        {
            string pokemon_names = @"audino bagon baltoy banette bidoof braviary bronzor carracosta charmeleon
cresselia croagunk darmanitan deino emboar emolga exeggcute gabite
girafarig gulpin haxorus heatmor heatran ivysaur jellicent jumpluff kangaskhan
kricketune landorus ledyba loudred lumineon lunatone machamp magnezone mamoswine
nosepass petilil pidgeotto pikachu pinsir poliwrath poochyena porygon2
porygonz registeel relicanth remoraid rufflet sableye scolipede scrafty seaking
sealeo silcoon simisear snivy snorlax spoink starly tirtouga trapinch treecko
tyrogue vigoroth vulpix wailord wartortle whismur wingull yamask";

            string[] pokemon = pokemon_names.Split(new char[]{' ','\n'});
            List<string> chain = new List<string>(pokemon.Length);

            for (int i = 0; i < pokemon.Length; i++)
            {
                swap(ref pokemon[0], ref pokemon[i]);
                Search( pokemon, chain, 1 );
                swap(ref pokemon[0], ref pokemon[i]);
            }

            foreach (string s in chain)
                Console.WriteLine(s);

            Console.ReadKey();
        }

        static void Search(string[] pokemon, List<string> longest_chain, int len )
        {
            if (len > longest_chain.Count)
            {
                longest_chain.Clear();
                for (int i = 0; i < len; i++)
                    longest_chain.Add(pokemon[i]);
            }

            char lastchar = pokemon[len - 1][pokemon[len-1].Length - 1];
            for (int i = len; i < pokemon.Length; i++)
            {
                if (pokemon[i][0] == lastchar)
                {
                    swap(ref pokemon[i], ref pokemon[len]);
                    Search(pokemon, longest_chain, len + 1);
                    swap(ref pokemon[i], ref pokemon[len]);
                }
            }
        }

        static void swap(ref string s1, ref string s2)
        {
            string tmp = s1;
            s1 = s2;
            s2 = tmp;
        }
    }
}
```

```txt
machamp
petilil
landorus
sableye
emboar
registeel
loudred
darmanitan
nosepass
simisear
relicanth
heatmor
rufflet
trapinch
haxorus
scrafty
yamask
kricketune
exeggcute
emolga
audino
```



## Common Lisp

Pretty brute force here.  Takes a couple of seconds to run.

```lisp
;;; return all the words that start with an initial letter

(defun filter-with-init (words init)
  (remove-if-not (lambda (word) (eql init (aref word 0))) words))

;;; produce a hash table whose key is the initial letter of a word and whose value is
;;; a list of the words that start with that initial letter

(defun group-by-first-letter (words)
  (let ((map_letters (make-hash-table))
        (inits (remove-duplicates (mapcar (lambda (word) (aref word 0)) words))))
    (dolist (init inits map_letters)
      (setf (gethash init map_letters) (filter-with-init words init)))
    ))

;;; Get the last letter in a word or array

(defun last-element (array) (aref array (- (length array) 1)))

;;; Produce a hash table whose key is a word and whose value is a list of the
;;; words that can follow that word

(defun get-followers (words)
  (let ((map-word-to-followers (make-hash-table :test 'equal))
        (init_hash (group-by-first-letter words)))
    (dolist (word words map-word-to-followers)
      (setf
       (gethash word map-word-to-followers)
       (gethash (last-element word) init_hash)))))

;;; Retrieve all the keys from a hash table

(defun keys (hashtbl)
  (let ((allkeys ()))
    (maphash #'(lambda (key val) (setf allkeys (cons key allkeys))) hashtbl)
    allkeys))

;;; Find the words which can follow a word and haven't been used yet.  The parameters are:
;;;    word - word being tested
;;;    followers - the hash table returned from get-followers
;;;    available - hash table with word as key and boolean indicating whether that word
;;;                has been used previously as value

(defun get-available-followers (word followers available)
  (if (null word)
      (keys followers)
      (remove-if-not #'(lambda (word) (gethash word available)) (gethash word followers))))

;;; Find the best in a list using an arbitrary test

(defun best (lst test)
  (let ((top (car lst)))
    (do
        ((rest (cdr lst) (cdr rest)))
        ((null rest) top)
      (if (funcall test (car rest) top) (setf top (car rest))))))

;;; Find the best path in a list

(defun best-list-path (paths)
  (best paths #'(lambda (path1 path2) (> (length path1) (length path2)))))

;;; Find the best path given all the supporting information we need

(defun best-path-from-available (word followers available depth path available-followers)
  (let ((results
         (mapcar #'(lambda (new-word)
                   (dfs-recurse new-word followers available (+ 1 depth) (cons word path)))
           available-followers)))
    (best-list-path results)))

;;; Recurse to find the best available path - the meat of the algorithm

(defun dfs-recurse (word followers available depth path)
  (let ((ret))
    ; Mark the word as unavailable
    (setf (gethash word available) nil)

    ; Find the longest path starting with word
    (let ((available-followers (get-available-followers word followers available)))
        (setf ret
         (if (null available-followers)
             (cons word path)
           (best-path-from-available word followers available depth path available-followers))))

    ; Mark the word as available again
    (setf (gethash word available) t)

    ; Return our longest path
    ret))

;;; Create the availability table

(defun make-available-table (words)
  (let
      ((available (make-hash-table)))
    (dolist (word words available) (setf (gethash word available) t))))

;;; Find the best path for a set of words

(defun best-path (words)
  (let
      ((followers (get-followers words))
       (available (make-available-table words)))
    (cdr (reverse (dfs-recurse nil followers available 0 nil)))))

;;; set up the words as a set of strings
(setf *words* (mapcar #'symbol-name
'(audino bagon baltoy banette bidoof braviary bronzor carracosta charmeleon
cresselia croagunk darmanitan deino emboar emolga exeggcute gabite
girafarig gulpin haxorus heatmor heatran ivysaur jellicent jumpluff kangaskhan
kricketune landorus ledyba loudred lumineon lunatone machamp magnezone mamoswine
nosepass petilil pidgeotto pikachu pinsir poliwrath poochyena porygon2
porygonz registeel relicanth remoraid rufflet sableye scolipede scrafty seaking
sealeo silcoon simisear snivy snorlax spoink starly tirtouga trapinch treecko
tyrogue vigoroth vulpix wailord wartortle whismur wingull yamask)))

(setf *path* (best-path *words*))
```

Output:<lang>("MACHAMP" "PETILIL" "LANDORUS" "SCRAFTY" "YAMASK" "KRICKETUNE" "EMBOAR" "REGISTEEL"
 "LOUDRED" "DARMANITAN" "NOSEPASS" "SIMISEAR" "RELICANTH" "HEATMOR" "RUFFLET"
 "TRAPINCH" "HAXORUS" "SEAKING" "GIRAFARIG" "GABITE" "EXEGGCUTE" "EMOLGA" "AUDINO")
```


## D


### Simple Version

Modified from the Go version:

```d
import std.stdio, std.algorithm, std.string;

void trySwap(string[] items, ref string item, in size_t len, ref string[] longest)
pure nothrow @safe {
    swap(items[len], item);
    search(items, len + 1, longest);
    swap(items[len], item);
}

void search(string[] items, in size_t len, ref string[] longest)
pure nothrow @safe {
    if (len > longest.length)
        longest = items[0 .. len].dup;
    immutable lastChar = items[len - 1][$ - 1];
    foreach (ref item; items[len .. $])
        if (item[0] == lastChar)
            trySwap(items, item, len, longest);
}

void main() @safe {
    auto pokemon = "audino bagon baltoy banette bidoof braviary
bronzor carracosta charmeleon cresselia croagunk darmanitan deino
emboar emolga exeggcute gabite girafarig gulpin haxorus heatmor
heatran ivysaur jellicent jumpluff kangaskhan kricketune landorus
ledyba loudred lumineon lunatone machamp magnezone mamoswine nosepass
petilil pidgeotto pikachu pinsir poliwrath poochyena porygon2
porygonz registeel relicanth remoraid rufflet sableye scolipede
scrafty seaking sealeo silcoon simisear snivy snorlax spoink starly
tirtouga trapinch treecko tyrogue vigoroth vulpix wailord wartortle
whismur wingull yamask".split;

    string[] solution;
    foreach (ref name; pokemon)
        trySwap(pokemon, name, 0, solution);

    writefln("%-(%s\n%)", solution);
}
```

Output:

```txt
machamp
petilil
landorus
scrafty
yamask
kricketune
emboar
registeel
loudred
darmanitan
nosepass
simisear
relicanth
heatmor
rufflet
trapinch
haxorus
seaking
girafarig
gabite
exeggcute
emolga
audino
```

The run-time is about 0.9 seconds with the dmd compiler and about 0.55 seconds with the ldc2 compiler.


### Improved Version

With two small changes the code gets faster. Here the names are represented as in C (so swapping them means just swapping a pointer, instead of swapping a pointer+length as before), and during the computation their last char is swapped with their second char (so there's no need to keep the string lengths around or use strlen).

```d
import std.stdio, std.algorithm, std.string, std.array, std.conv;

void search(immutable(char)*[] items, in int len,
            ref immutable(char)*[] longest) pure {
    if (len > longest.length)
        longest = items[0 .. len].dup;
    immutable lastChar = items[len - 1][1];
    foreach (ref item; items[len .. $])
        if (item[0] == lastChar) {
            swap(items[len], item);
            search(items, len + 1, longest);
            swap(items[len], item);
        }
}

void main() {
    static immutable(char)* prep(in string s) pure {
        assert(s.length > 1);
        auto sd = s.dup;
        swap(sd[1], sd[$ - 1]);
        return sd.toStringz;
    }

     static string unprep(immutable(char)* sd) pure {
        auto ms = sd.to!(char[]);
        swap(ms[1], ms[$ - 1]);
        return ms;
    }

    auto pokemon = "audino bagon baltoy banette bidoof braviary
bronzor carracosta charmeleon cresselia croagunk darmanitan deino
emboar emolga exeggcute gabite girafarig gulpin haxorus heatmor
heatran ivysaur jellicent jumpluff kangaskhan kricketune landorus
ledyba loudred lumineon lunatone machamp magnezone mamoswine nosepass
petilil pidgeotto pikachu pinsir poliwrath poochyena porygon2
porygonz registeel relicanth remoraid rufflet sableye scolipede
scrafty seaking sealeo silcoon simisear snivy snorlax spoink starly
tirtouga trapinch treecko tyrogue vigoroth vulpix wailord wartortle
whismur wingull yamask".split.map!prep.array;

    immutable(char)*[] solution;
    foreach (ref name; pokemon) {
        swap(pokemon[0], name);
        search(pokemon, 1, solution);
        swap(pokemon[0], name);
    }

    writefln("%-(%s\n%)", solution.map!unprep);
}
```


This leads to tight enough code for the foreach loop in the search function:


```asm
LBB0_4:
    movl (%esi), %eax
    movb 19(%esp), %cl
    cmpb %cl, (%eax)
jne LBB0_6
    movl (%edi,%ebx,4), %ecx
    movl %eax, (%edi,%ebx,4)
    movl %ecx, (%esi)
    movl %edi, 8(%esp)
    movl 48(%esp), %eax
    movl %eax, 4(%esp)
    movl 12(%esp), %eax
    movl %eax, (%esp)
    movl 20(%esp), %eax
    calll __D25last_letter_first_letter26searchFNaAPyaxiKAPyaZv
    subl $12, %esp
    movl (%edi,%ebx,4), %eax
    movl (%esi), %ecx
    movl %ecx, (%edi,%ebx,4)
    movl %eax, (%esi)
LBB0_6:
    addl $4, %esi
    decl %ebp
    jne LBB0_4
```


The run-time is about 0.65 seconds with LDC2 compiler. The output is similar.


### Faster Version


```d
import std.stdio, std.algorithm, std.string, std.range, std.typecons;

Tuple!(uint, string[]) findLongestChain(in string[] words)
pure nothrow {
    static struct Pair { string word; bool unused; }
    uint nSolutions;

    void search(Pair[][] sequences, in size_t minHead,
                in string currWord, string[] currentPath,
                size_t currentPathLen,
                ref string[] longestPath) nothrow {
        currentPath[currentPathLen] = currWord;
        currentPathLen++;

        if (currentPathLen == longestPath.length) {
            nSolutions++;
        }  else if (currentPathLen > longestPath.length) {
            nSolutions = 1;
            longestPath = currentPath[0 .. currentPathLen].dup;
        }

        // Recursive search.
        immutable size_t lastCharIndex = currWord[$ - 1] - minHead;
        if (lastCharIndex < sequences.length)
            foreach (ref pair; sequences[lastCharIndex])
                if (pair.unused) {
                    pair.unused = false;
                    search(sequences, minHead, pair.word, currentPath,
                           currentPathLen, longestPath);
                    pair.unused = true;
                }
    }

    if (words.empty)
        typeof(return)(0, null);
    immutable heads = words.map!q{ a[0] }.array;
    immutable size_t minHead = reduce!min(heads[0],
                                          heads[1.. $].representation);
    immutable size_t maxHead = reduce!max(heads[0],
                                          heads[1.. $].representation);

    auto sequences = new Pair[][](maxHead - minHead + 1, 0);
    foreach (const word; words)
        sequences[word[0] - minHead] ~= Pair(word, true);

    auto currentPath = new string[words.length];
    string[] longestPath;

    // Try each item as possible start.
    foreach (seq; sequences)
        foreach (ref pair; seq) {
            pair.unused = false;
            search(sequences, minHead, pair.word,
                   currentPath, 0, longestPath);
            pair.unused = true;
       }

    return typeof(return)(nSolutions, longestPath);
}


void main() {
    auto pokemon = "audino bagon baltoy banette bidoof braviary
bronzor carracosta charmeleon cresselia croagunk darmanitan deino
emboar emolga exeggcute gabite girafarig gulpin haxorus heatmor
heatran ivysaur jellicent jumpluff kangaskhan kricketune landorus
ledyba loudred lumineon lunatone machamp magnezone mamoswine nosepass
petilil pidgeotto pikachu pinsir poliwrath poochyena porygon2
porygonz registeel relicanth remoraid rufflet sableye scolipede
scrafty seaking sealeo silcoon simisear snivy snorlax spoink starly
tirtouga trapinch treecko tyrogue vigoroth vulpix wailord wartortle
whismur wingull yamask".toLower.split;

    // Remove duplicates.
    pokemon.length -= pokemon.sort().uniq.copy(pokemon).length;

    const sol = pokemon.findLongestChain;
    writeln("Maximum path length: ", sol[1].length);
    writeln("Paths of that length: ", sol[0]);
    writeln("Example path of that length:");
    writefln("%(  %-(%s %)\n%)", sol[1].chunks(7));
}
```

```txt
Maximum path length: 23
Paths of that length: 1248
Example path of that length:
  machamp petilil landorus scrafty yamask kricketune emboar
  registeel loudred darmanitan nosepass simisear relicanth heatmor
  rufflet trapinch haxorus seaking girafarig gabite exeggcute
  emolga audino
```

Runtime: about 0.20 seconds with dmd compiler, about 0.15 seconds with ldc2 compiler.


### Alternative Version

```d
import std.stdio, std.algorithm, std.array, std.typecons,
       std.container, std.range;

auto findChain(in string[] seq) pure nothrow /*@safe*/ {
    const adj = seq
                .map!(item => tuple(item, seq
                                          .filter!(x => x[0] == item[$ - 1])
                                          .array))
                .assocArray;
    SList!string res;

    foreach (immutable item; adj.byKey) {
        void inner(in string it, SList!string lst) nothrow {
            lst.insertFront(it);
            if (lst[].walkLength > res[].walkLength)
                res = lst;
            foreach (immutable x; adj[it])
                if (!lst[].canFind(x))
                    inner(x, lst);
        }

        inner(item, SList!string());
    }

    return res.array.retro;
}

void main() /*@safe*/ {
    "audino bagon baltoy banette bidoof braviary
    bronzor carracosta charmeleon cresselia croagunk darmanitan deino
    emboar emolga exeggcute gabite girafarig gulpin haxorus heatmor
    heatran ivysaur jellicent jumpluff kangaskhan kricketune landorus
    ledyba loudred lumineon lunatone machamp magnezone mamoswine
    nosepass petilil pidgeotto pikachu pinsir poliwrath poochyena
    porygon2 porygonz registeel relicanth remoraid rufflet sableye
    scolipede scrafty seaking sealeo silcoon simisear snivy snorlax
    spoink starly tirtouga trapinch treecko tyrogue vigoroth vulpix
    wailord wartortle whismur wingull yamask".split.findChain.writeln;
}
```

```txt
["machamp", "petilil", "landorus", "scrafty", "yamask", "kricketune", "emboar", "registeel", "loudred", "darmanitan", "nosepass", "simisear", "relicanth", "heatmor", "rufflet", "trapinch", "haxorus", "seaking", "girafarig", "gabite", "exeggcute", "emolga", "audino"]
```

Run-time is about 3.1 seconds with ldc2 compiler.


## Delphi

Visual implementation, this unit is a VCL Form with a Memo, a Button, a Checkbox, a DataGrid, a DBMemo, a DataSource and a ClientDataSet with tree fields (length integer,count integer,list memo):

```delphi
unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, DBCtrls, DB, DBClient, Grids, DBGrids, ExtCtrls;

type
  TLastLFirstL = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    Memo1: TMemo;
    DataSource1: TDataSource;
    ClientDataSet1: TClientDataSet;
    ClientDataSet1Longitud: TIntegerField;
    ClientDataSet1Cantidad: TIntegerField;
    ClientDataSet1Lista: TMemoField;
    Panel2: TPanel;
    DBMemo1: TDBMemo;
    DBGrid1: TDBGrid;
    Splitter1: TSplitter;
    CheckBox1: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    FPokemons:TStrings; //internal list of words, taken from memo
    FIndex:TStrings; //index of words, based on starting letter
    FCurrList:TStrings; //current list of words being made
    FMax:integer; //max length of list found so far
    FCount:array of array[boolean]of integer; //counting of lists length ocurrences
  protected
    procedure BuildIndex; //build FIndex based on FPokemons contents
    procedure ClearIndex; //empty FIndex
    procedure PokeChain(starting:Char;mylevel:integer); //recursive procedure that builds words lists
    procedure BuildChains; //starts the lists building, by calling PokeChain for every FPokemons
    procedure AddCurrList; //called each time a list is "finished" (no more words to add to it)
  public
    { Public declarations }
  end;

var
  LastLFirstL: TLastLFirstL;

implementation

{$R *.dfm}

{ TForm1 }

{ if the actual list is the longest found so far it is added to
the dataset, otherwise its ocurrence is just counted}
procedure TLastLFirstL.AddCurrList;
var
  i,cc: integer;
  foundit:boolean;
begin
  with ClientDataSet1 do begin
    cc := FCurrList.Count;
    if cc <= FMax then begin //count it
      foundit := false;
      for i := 0 to High(FCount) do begin
        foundit := FCount[i][false] = cc;
        if foundit then begin
          FCount[i][true] := FCount[i][true] + 1;
          break;
        end;
      end;
      if not foundit then begin
        //length that we never add to the dataset
        i := High(FCount);
        SetLength(FCount,i+2);
        Inc(i);
        FCount[i][false] := cc;
        FCount[i][true] := 1;
      end;
      exit;
    end;
    //new longest list is FCurrList, add it to the dataset
    FMax := cc;
    SetLength(FCount,High(Fcount)+2); //make room for ocurrence count
    FCount[High(FCount)][false] := cc;
    FCount[High(FCount)][true] := 1;
    //actual dataset adding
    Append;
    Fields[0].AsInteger := cc;
    Fields[1].AsInteger := 0;
    Fields[2].AsString := FCurrList.Text; //first one is example one
    Post;
  end;
end;

{}
procedure TLastLFirstL.BuildChains;
var
  stSeen:array of array[boolean] of char;
  poke:string;
  i:integer;
  tc:int64;
  filteqs:boolean;
  k: Integer;
begin
  //do some cleaning before starting
  while not ClientDataSet1.IsEmpty do
    ClientDataSet1.Delete;
  Finalize(FCount);
  FMax := 0;
  filteqs := CheckBox1.Checked;
  //measure time
  tc := gettickcount;
  //each word is given the opportunity of starting a list
  if filteqs then begin
    //ignore words with same start and end as others already seen
    filteqs := False;
    for i := 0 to FPokemons.Count - 1 do begin
      poke := FPokemons[i];
      for k := 0 to High(stSeen) do begin
        filteqs := (stSeen[k][false] = poke[1]) and (stSeen[k][true] = poke[length(poke)]);
        if filteqs then
          break;
      end;
      if filteqs then //already seen equivalent
        continue;
      FPokemons.Objects[i] := Pointer(1);
      FCurrList.Clear; //new list of words
      FCurrList.Add(poke);
      PokeChain(poke[length(poke)],2); //continue the list
      //register as seen, for future equivalents
      k := High(stSeen);
      SetLength(stSeen,k+2);
      Inc(k);
      stSeen[k][false] := poke[1];
      stSeen[k][true] := poke[length(poke)];
      FPokemons.Objects[i] := nil;
    end;
    Finalize(stSeen);
  end else begin
    for i := 0 to FPokemons.Count - 1 do begin
      poke := FPokemons[i];
      FPokemons.Objects[i] := Pointer(1);
      FCurrList.Clear; //new list of words
      FCurrList.Add(poke);
      PokeChain(poke[length(poke)],2); //continue the list
      FPokemons.Objects[i] := nil;
    end;
  end;
  tc := gettickcount - tc; //don't consider dataset counting as part of the process
  //set actual counting of ocurrences on the dataset
  for i := 0 to High(FCount) do with ClientDataSet1 do begin
    if Locate('Longitud',FCount[i][false],[]) then
      Edit
    else begin
      Append;
      Fields[0].AsInteger := FCount[i][false];
      Fields[2].AsString := 'No example preserved';
    end;
    Fields[1].AsInteger := FCount[i][true];
    Post;
  end;
  ClientDataSet1.IndexFieldNames := 'Longitud';
  //show time taken
  Panel1.Caption := IntToStr(tc div 1000) + '.' + IntToStr(tc - (tc div 1000) * 1000) + ' segs.';
end;

{ builds an index based on the first letter of every word in consideration,
because all we care about is the first and the last letter of every word.
The index is a TStrings where each element is the starting letter and the
corresponding object is a TList with all the indices of the words that
starts with that letter. }
procedure TLastLFirstL.BuildIndex;
var
  i,ii: Integer;
  poke:string;
  st,ed:char;
  List:TList;
  k: Integer;
  found:boolean;
begin
  ClearIndex; //just in case is not the first execution
  if not Assigned(FIndex) then // just in case IS the first execution
    FIndex := TStringList.Create;
  for i := 0 to FPokemons.Count - 1 do begin
    poke := FPokemons[i];
    st := poke[1];
    ed := poke[Length(poke)];
    ii := FIndex.IndexOf(st);
    if ii<0 then //first time we see this starting letter
      ii := FIndex.AddObject(st,TList.Create);
    List := TList(FIndex.Objects[ii]);
    found := false;
    if CheckBox1.Checked then begin //ignore equivalent words (same start, same end)
      //all the List are words with the same start, so lets check the end
      for k := 0 to List.Count - 1 do begin
        poke := FPokemons[integer(List[k])];
        found := poke[Length(poke)] = ed;
        if found then
          break;
      end;
    end;
    if not found then // not checking equivalents, or firts time this end is seen
      List.Add(Pointer(i));
  end;
end;

{ do your thing! }
procedure TLastLFirstL.Button1Click(Sender: TObject);
begin
  Panel1.Caption := 'Calculating..';
  FPokemons.Assign(Memo1.Lines); //words in the game
  BuildIndex;
  BuildChains;
end;

{ frees all the TList used by the index, clears the index }
procedure TLastLFirstL.ClearIndex;
var
  i:integer;
begin
  if not Assigned(FIndex) then
    exit;
  for i := 0 to FIndex.Count - 1 do begin
    TList(FIndex.Objects[i]).Free;
  end;
  FIndex.Clear;
end;

procedure TLastLFirstL.FormCreate(Sender: TObject);
begin
  FPokemons := TStringList.Create;
  FCurrList := TStringList.Create;
end;

procedure TLastLFirstL.FormDestroy(Sender: TObject);
begin
  FCurrList.Free;
  FPokemons.Free;
  ClearIndex; //IMPORTANT!
  FIndex.Free;
end;

{where the magic happens.
Recursive procedure that adds a word to the current list of words.
Receives the starting letter of the word to add, and the "position"
of the word in the chain.
The position is used to ensure a word is not used twice for the list. }
procedure TLastLFirstL.PokeChain(starting: Char;mylevel:integer);
var
  i,ii,plevel:integer;
  List:TList;
  didit:boolean;
begin
  application.processMessages; //don't let the interface die..
  didit := False; //if we can't add another word, then we have reached the maximun length for the list
  ii := FIndex.IndexOf(starting);
  if ii >= 0 then begin //there are words with this starting letter
    List := TList(FIndex.Objects[ii]);
    for i := 0 to List.Count - 1 do begin
      ii := integer(List[i]);
      plevel := integer(FPokemons.Objects[ii]); // if the integer stored in the Object property is lower than mylevel, then this word is already in the list
      if (plevel > mylevel) or (plevel = 0) then begin // you can use the word
        //a try finally would be a good thing here, but...
        FCurrList.Add(FPokemons[ii]); //add the word to the list
        FPokemons.Objects[ii] := Pointer(mylevel); //signal is already in the list
        PokeChain(FPokemons[ii][length(FPokemons[ii])],mylevel+1); //add more words to the list
        FcurrList.Delete(FCurrList.Count-1); //already did my best, lets try with another word
        FPokemons.Objects[ii] := nil; //unsignal it, so it can be used "later"
        didit := True; //we did add one word to the list
      end;
    end;
  end;
  if not didit then //there is no way of making the list longer, process it
    AddCurrList;
end;

end.
```

Runtime varies depending if you run the "optimized" version or not. Ranges from 6 to 18 seconds.

NOTE: "optimized" version is actually a different algorithm, but in most cases returns the same results.


## Elixir

```elixir
defmodule LastLetter_FirstLetter do
  def search(names) do
    first = Enum.group_by(names, &String.first/1)
    sequences = Enum.reduce(names, [], fn name,acc -> add_name(first, acc, [name]) end)
    max = Enum.max_by(sequences, &length/1) |> length
    max_seqs = Enum.filter(sequences, fn seq -> length(seq) == max end)
    IO.puts "there are #{length(sequences)} possible sequences"
    IO.puts "the longest is #{max} names long"
    IO.puts "there are #{length(max_seqs)} such sequences. one is:"
    hd(max_seqs) |> Enum.with_index |>
    Enum.each(fn {name, idx} ->
      :io.fwrite "  ~2w ~s~n", [idx+1, name]
    end)
  end

  defp add_name(first, sequences, seq) do
    last_letter = String.last(hd(seq))
    potentials = Map.get(first, last_letter, []) -- seq
    if potentials == [] do
      [Enum.reverse(seq) | sequences]
    else
      Enum.reduce(potentials, sequences, fn name, acc -> add_name(first, acc, [name | seq]) end)
    end
  end
end

names = ~w(
  audino bagon baltoy banette bidoof braviary bronzor carracosta charmeleon
  cresselia croagunk darmanitan deino emboar emolga exeggcute gabite
  girafarig gulpin haxorus heatmor heatran ivysaur jellicent jumpluff kangaskhan
  kricketune landorus ledyba loudred lumineon lunatone machamp magnezone mamoswine
  nosepass petilil pidgeotto pikachu pinsir poliwrath poochyena porygon2
  porygonz registeel relicanth remoraid rufflet sableye scolipede scrafty seaking
  sealeo silcoon simisear snivy snorlax spoink starly tirtouga trapinch treecko
  tyrogue vigoroth vulpix wailord wartortle whismur wingull yamask
)

LastLetter_FirstLetter.search(names)
```


```txt

there are 2076396 possible sequences
the longest is 23 names long
there are 1248 such sequences. one is:
   1 machamp
   2 petilil
   3 landorus
   4 scrafty
   5 yamask
   6 kricketune
   7 emboar
   8 registeel
   9 loudred
  10 darmanitan
  11 nosepass
  12 simisear
  13 relicanth
  14 heatmor
  15 rufflet
  16 trapinch
  17 haxorus
  18 seaking
  19 girafarig
  20 gabite
  21 exeggcute
  22 emolga
  23 audino

```



## Erlang

This is a parallel version. It takes 2.1 seconds. The (commented out) serial version takes 7.1 seconds. Both times measured on my (low end) Mac Mini. I thought parallel code would help in getting the brownie points. But even a small increase to 100 Pokemons makes the code run for more than the few spare hours I have in the evening.

```Erlang

-module( last_letter_first_letter ).

-export( [solve/1, task/0] ).

solve( Names ) ->
    Dict = lists:foldl( fun dict_append/2, dict:new(), Names ),
    Chains = construct_chains_in_parallel( Dict ),
%    Chains = [construct_chain_from_key(Dict, X) || X <- dict:fetch_keys(Dict)],
    lists:foldl( fun construct_chain_longest/2, [], Chains ).

task() -> solve( binary:split(names(), <<" ">>, [global]) ).



construct_chains_in_parallel( Dict ) ->
    My_pid = erlang:self(),
    Pids = [erlang:spawn( fun() -> My_pid ! {erlang:self(), construct_chain_from_key(Dict, X)} end) || X <- dict:fetch_keys(Dict)],
    [receive {X, Chain} -> Chain end || X <- Pids].

construct_chain_from_key( Dict, First_letter ) ->
	Names = construct_chain_names( dict:find(First_letter, Dict) ),
	construct_chain_from_names( Names, Dict, [] ).

construct_chain_from_names( [], _Dict, Best_chain ) -> Best_chain;
construct_chain_from_names( [{Name, Last_letter} | T], Dict, Best_chain ) ->
        New_dict = dict_delete( Name, Dict ),
        New_chain = [Name | construct_chain_from_key( New_dict, Last_letter )],
	construct_chain_from_names( T, Dict, construct_chain_longest(Best_chain, New_chain) ).

construct_chain_longest( Chain1, Chain2 ) when length(Chain1) > length(Chain2) -> Chain1;
construct_chain_longest( _Chain1, Chain2 ) -> Chain2.

construct_chain_names( {ok, {Name, Last_letter}} ) -> [{Name, Last_letter}];
construct_chain_names(	{ok, Values} ) -> Values;
construct_chain_names( error ) -> [].

dict_append( Name, Acc ) ->
	{First_letter, {Name, Last_letter}} = dict_item( Name ),
	dict:append( First_letter, {Name, Last_letter}, Acc ).

dict_item( <<First_letter, _T/binary>>=Name ) ->
	Until_last_letter = erlang:byte_size( Name ) - 1,
	<<_H:Until_last_letter/binary, Last_letter>> = Name,
	{First_letter, {Name, Last_letter}}.

dict_delete( <<First_letter, _T/binary>>=Name, Dict ) ->
	Name_last_letters = dict:fetch(First_letter, Dict),
	dict:store( First_letter, lists:keydelete(Name, 1, Name_last_letters), Dict ).

names() -> <<"audino bagon baltoy banette bidoof braviary bronzor carracosta charmeleon cresselia croagunk darmanitan deino emboar emolga exeggcute gabite girafarig gulpin haxorus heatmor heatran ivysaur jellicent jumpluff kangaskhan kricketune landorus ledyba loudred lumineon lunatone machamp magnezone mamoswine nosepass petilil pidgeotto pikachu pinsir poliwrath poochyena porygon2 porygonz registeel relicanth remoraid rufflet sableye scolipede scrafty seaking sealeo silcoon simisear snivy snorlax spoink starly tirtouga trapinch treecko tyrogue vigoroth vulpix wailord wartortle whismur wingull yamask">>.

```

```txt

11> last_letter_first_letter:task().
[<<"machamp">>,<<"pinsir">>,<<"rufflet">>,<<"trapinch">>,
  <<"heatmor">>,<<"remoraid">>,<<"darmanitan">>,
  <<"nosepass">>,<<"starly">>,<<"yamask">>,<<"kricketune">>,
  <<"exeggcute">>,<<"emboar">>,<<"relicanth">>,<<"haxorus">>,
  <<"simisear">>,<<"registeel">>,<<"landorus">>,<<"seaking">>,
  <<"girafarig">>,<<"gabite">>,<<"emolga">>,<<"audino">>]

```



## Go

Depth first, starting with each possible name.

```go
package main

import (
    "fmt"
    "strings"
)

var pokemon = `audino bagon baltoy...67 names omitted...`

func main() {
    // split text into slice representing directed graph
    var d []string
    for _, l := range strings.Split(pokemon, "\n") {
        d = append(d, strings.Fields(l)...)
    }
    fmt.Println("searching", len(d), "names...")
    // try each name as possible start
    for i := range d {
        d[0], d[i] = d[i], d[0]
        search(d, 1, len(d[0]))
        d[0], d[i] = d[i], d[0]
    }
    fmt.Println("maximum path length:", len(ex))
    fmt.Println("paths of that length:", nMax)
    fmt.Print("example path of that length:")
    for i, n := range ex {
        if i%6 == 0 {
            fmt.Print("\n   ")
        }
        fmt.Print(n, " ")
    }
    fmt.Println()
}

var ex []string
var nMax int

func search(d []string, i, ncPath int) {
    // tally statistics
    if i == len(ex) {
        nMax++
    } else if i > len(ex) {
        nMax = 1
        ex = append(ex[:0], d[:i]...)
    }
    // recursive search
    lastName := d[i-1]
    lastChar := lastName[len(lastName)-1]
    for j := i; j < len(d); j++ {
        if d[j][0] == lastChar {
            d[i], d[j] = d[j], d[i]
            search(d, i+1, ncPath+1+len(d[i]))
            d[i], d[j] = d[j], d[i]
        }
    }
}
```

Output:

```txt

searching 70 names...
maximum path length: 23
paths of that length: 1248
example path of that length:
   machamp petilil landorus scrafty yamask kricketune
   emboar registeel loudred darmanitan nosepass simisear
   relicanth heatmor rufflet trapinch haxorus seaking
   girafarig gabite exeggcute emolga audino

```



## Haskell

Note: This takes ~80 seconds to complete on my machine.

```Haskell
import Data.List
import qualified Data.ByteString.Char8 as B

allPokemon :: [B.ByteString]
allPokemon = map B.pack $ words
    "audino bagon baltoy banette bidoof braviary bronzor carracosta charmeleon \
    \cresselia croagunk darmanitan deino emboar emolga exeggcute gabite \
    \girafarig gulpin haxorus heatmor heatran ivysaur jellicent jumpluff kangaskhan \
    \kricketune landorus ledyba loudred lumineon lunatone machamp magnezone mamoswine \
    \nosepass petilil pidgeotto pikachu pinsir poliwrath poochyena porygon2 \
    \porygonz registeel relicanth remoraid rufflet sableye scolipede scrafty seaking \
    \sealeo silcoon simisear snivy snorlax spoink starly tirtouga trapinch treecko \
    \tyrogue vigoroth vulpix wailord wartortle whismur wingull yamask"

growChains :: [[B.ByteString]] -> [B.ByteString]
growChains pcs
    | nextChainSet == [] = head pcs
    | otherwise = growChains nextChainSet
  where nextChainSet = pcs >>= findLinks
        findLinks pc = map (\x -> pc ++ [x]) $ filter (isLink $ last pc) (allPokemon \\ pc)
        isLink pl pr = B.last pl == B.head pr

main = mapM_ B.putStrLn $ growChains $ map (\x -> [x]) allPokemon
```

Output:

```txt
machamp
petilil
landorus
scrafty
yamask
kricketune
emboar
registeel
loudred
darmanitan
nosepass
simisear
relicanth
heatmor
rufflet
trapinch
haxorus
seaking
girafarig
gabite
exeggcute
emolga
audino
```

A simpler version (no ByteString), about 2.4 times slower (GHC -O3), same output:

```Haskell
import Data.List

allPokemon = words
    "audino bagon baltoy banette bidoof braviary bronzor carracosta charmeleon \
    \cresselia croagunk darmanitan deino emboar emolga exeggcute gabite \
    \girafarig gulpin haxorus heatmor heatran ivysaur jellicent jumpluff kangaskhan \
    \kricketune landorus ledyba loudred lumineon lunatone machamp magnezone mamoswine \
    \nosepass petilil pidgeotto pikachu pinsir poliwrath poochyena porygon2 \
    \porygonz registeel relicanth remoraid rufflet sableye scolipede scrafty seaking \
    \sealeo silcoon simisear snivy snorlax spoink starly tirtouga trapinch treecko \
    \tyrogue vigoroth vulpix wailord wartortle whismur wingull yamask"

growChains :: [[String]] -> [String]
growChains pcs
    | nextChainSet == [] = head pcs
    | otherwise = growChains nextChainSet
  where nextChainSet = pcs >>= findLinks
        findLinks pc = map (\x -> pc ++ [x]) $ filter (isLink $ last pc) (allPokemon \\ pc)
        isLink pl pr = last pl == head pr

main = mapM_ putStrLn $ growChains $ map (\x -> [x]) allPokemon
```


=={{header|Icon}} and {{header|Unicon}}==

Works in both languages (brute force):


```unicon
global words

procedure main()
    words := table()
    every word := genwords(&input) do {
        /words[word[1]] := []
        put(words[word[1]], word)
        }
    bP := []
    every p := getPath(!!words,[]) do if *\p > *bP then bP := copy(p)
    write("Longest: ",*bP)
    every writes((!bP||" ")|"\n")
end

procedure getPath(word, p)
    if word == !p then return p
    if /words[word[-1]] then suspend p <- p ||| [word]
    else suspend getPath(!words[word[-1]], p <- p ||| [word])
end

procedure genwords(f)
    while l := !f do
        l ? while tab(upto(&letters)) do suspend tab(many(&letters))\1
end
```


Sample run on sample data:

```txt

->llfl <llfl.in
Longest: 23
machamp petilil landorus scrafty yamask kricketune emboar registeel loudred darmanitan nosepass simisear relicanth heatmor rufflet trapinch haxorus seaking girafarig gabite exeggcute emolga audino
->

```



## J

Here, we use a brute force breadth-first search.  Unless we know ahead of time how long "longest" is, we must try all possibilities to ensure that an unchecked possibility is not longer than a possibility which we have found.


```j
pokenames=: ;:0 :0-.LF
 audino bagon baltoy banette bidoof braviary bronzor carracosta charmeleon
 cresselia croagunk darmanitan deino emboar emolga exeggcute gabite
 girafarig gulpin haxorus heatmor heatran ivysaur jellicent jumpluff kangaskhan
 kricketune landorus ledyba loudred lumineon lunatone machamp magnezone mamoswine
 nosepass petilil pidgeotto pikachu pinsir poliwrath poochyena porygon2
 porygonz registeel relicanth remoraid rufflet sableye scolipede scrafty seaking
 sealeo silcoon simisear snivy snorlax spoink starly tirtouga trapinch treecko
 tyrogue vigoroth vulpix wailord wartortle whismur wingull yamask
)

seqs=: 3 :0
  links=. <@I. _1 =/&({&>&y) 0
  next=. ,.i.#links
  while.#next do.
     r=. next
     assert. 1e9>*/8,$r
     next=. (#~ (-: ~.)"1) >;<@(] <@,"1 0 links {::~ {:)"1 r
  end.
  r
)
```


The line      <code>assert. 1e9>*/8,$r</code>   was added to avoid a very bad behavior from microsoft windows which appeared on different arguments, when intermediate results became too large (the machine would have to be rebooted when intermediate results became an order of magnitude larger than the available physical memory).  By ensuring that the program would end before consuming that much virtual memory, this behavior from the operating system can be avoided.  Note that [http://www.jsoftware.com/help/dictionary/dx009.htm 9!:21 and/or 9!:33] could also be used to prevent OS instability triggered by requesting too many resources.

With this procedure we are able to conduct the entire search for this list of names:


```j
$R=: seqs pokenames
1248 23
```


With this data set, we have 1248 sequences of names which have the longest possible length, and those sequences are 23 names long.  Here's one of them:


```j
>
pokenames {~{.R
machamp
petilil
landorus
scrafty
yamask
kricketune
emboar
registeel
loudred
darmanitan
nosepass
simisear
relicanth
heatmor
rufflet
trapinch
haxorus
seaking
girafarig
gabite
exeggcute
emolga
audino
```



## Java


```java
// derived from C
final class LastLetterFirstLetter {
    static int maxPathLength = 0;
    static int maxPathLengthCount = 0;
    static final StringBuffer maxPathExample = new StringBuffer(500);

    static final String[] names = {"audino", "bagon", "baltoy", "banette",
        "bidoof", "braviary", "bronzor", "carracosta", "charmeleon",
        "cresselia", "croagunk", "darmanitan", "deino", "emboar",
        "emolga", "exeggcute", "gabite", "girafarig", "gulpin",
        "haxorus", "heatmor", "heatran", "ivysaur", "jellicent",
        "jumpluff", "kangaskhan", "kricketune", "landorus", "ledyba",
        "loudred", "lumineon", "lunatone", "machamp", "magnezone",
        "mamoswine", "nosepass", "petilil", "pidgeotto", "pikachu",
        "pinsir", "poliwrath", "poochyena", "porygon2", "porygonz",
        "registeel", "relicanth", "remoraid", "rufflet", "sableye",
        "scolipede", "scrafty", "seaking", "sealeo", "silcoon",
        "simisear", "snivy", "snorlax", "spoink", "starly", "tirtouga",
        "trapinch", "treecko", "tyrogue", "vigoroth", "vulpix",
        "wailord", "wartortle", "whismur", "wingull", "yamask"};

    static void recursive(String[] part, int offset) {
        if (offset > maxPathLength) {
            maxPathLength = offset;
            maxPathLengthCount = 1;
        } else if (offset == maxPathLength) {
            maxPathLengthCount++;
            maxPathExample.setLength(0);
            for (int i = 0; i < offset; i++) {
                maxPathExample.append((i % 5 == 0 ? "\n  " : " "));
                maxPathExample.append(part[i]);
            }
        }
        final char lastChar = part[offset - 1].charAt(part[offset - 1].length()-1);
        for (int i = offset; i < part.length; i++) {
            if (part[i].charAt(0) == lastChar) {
                String tmp = names[offset];
                names[offset] = names[i];
                names[i] = tmp;
                recursive(names, offset+1);
                names[i] = names[offset];
                names[offset] = tmp;
            }
        }
    }

    public static void main(String[] args) {
        for (int i = 0; i < names.length; i++) {
            String tmp = names[0];
            names[0] = names[i];
            names[i] = tmp;
            recursive(names, 1);
            names[i] = names[0];
            names[0] = tmp;
       }
       System.out.println("maximum path length        : " + maxPathLength);
       System.out.println("paths of that length       : " + maxPathLengthCount);
       System.out.println("example path of that length:" + maxPathExample);
    }
}

```


Output:
```txt
maximum path length        : 23
paths of that length       : 1248
example path of that length:
  machamp pinsir rufflet trapinch heatmor
  remoraid darmanitan nosepass starly yamask
  kricketune exeggcute emboar relicanth haxorus
  simisear registeel landorus seaking girafarig
  gabite emolga audino

```



## JavaScript

{{Works with|Node.js}} (Required for the ''process'' object)

```javascript
/**
 * Find the letter the word ends on
 * @param {string} word
 * @returns {string}
 */
const endsWith = word => word[word.length - 1];

/**
 * Remove the used elements from the candidate elements
 * @param {Array<string>} words Candidate words
 * @param {Array<string>} used Used words
 * @returns {*}
 */
const getCandidates = (words, used) => words.filter(e => !used.includes(e));

/**
 * Build a map of letters to words that start with that letter
 * @param {Array<string>} words
 * @returns {Map<string, Array<string>>}
 */
const buildLookup = words => {
  const lookup = new Map();
  words.forEach(e => {
    const start = e[0];
    lookup.set(start, [...(lookup.get(start) || []), e]);
  });
  return lookup;
};


/**
 * Main function
 * @param {Array<string>} names
 */
const findPaths = names => {
  const t0 = process.hrtime();
  console.log('Checking:', names.length, 'names');
  const lookup = buildLookup(names);

  let maxNum = 0;
  let maxPaths = [];

  const parseResult = arr => {
    if (typeof arr[0] === 'object') {
      arr.forEach(el => parseResult(el))
    } else {
      if (arr.length > maxNum) {
        maxNum = arr.length;
        maxPaths = [arr];
      }
      if (arr.length === maxNum) {
        maxPaths.push(arr)
      }
    }
  };

  const searchWords = (word, res) => {
    const cs = getCandidates(lookup.get(endsWith(word)) || [], res);
    return cs.length ? cs.map(e => searchWords(e, [...res, e])) : res;
  };

  names.forEach(word => {
    const res = searchWords(word, [word]);
    parseResult(res);
  });

  const t1 = process.hrtime(t0);
  console.info('Execution time (hr): %ds %dms', t1[0], t1[1] / 1000000);
  console.log('Max Path:', maxNum);
  console.log('Matching Paths:', maxPaths.length);
  console.log('Example Path:', maxPaths[0]);

};

const pokimon = ["audino", "bagon", "baltoy", "banette",
  "bidoof", "braviary", "bronzor", "carracosta", "charmeleon",
  "cresselia", "croagunk", "darmanitan", "deino", "emboar",
  "emolga", "exeggcute", "gabite", "girafarig", "gulpin",
  "haxorus", "heatmor", "heatran", "ivysaur", "jellicent",
  "jumpluff", "kangaskhan", "kricketune", "landorus", "ledyba",
  "loudred", "lumineon", "lunatone", "machamp", "magnezone",
  "mamoswine", "nosepass", "petilil", "pidgeotto", "pikachu",
  "pinsir", "poliwrath", "poochyena", "porygon2", "porygonz",
  "registeel", "relicanth", "remoraid", "rufflet", "sableye",
  "scolipede", "scrafty", "seaking", "sealeo", "silcoon",
  "simisear", "snivy", "snorlax", "spoink", "starly", "tirtouga",
  "trapinch", "treecko", "tyrogue", "vigoroth", "vulpix",
  "wailord", "wartortle", "whismur", "wingull", "yamask"];

findPaths(pokimon);

```



```txt

Checking: 70 names
Execution time (hr): 2s 121.778223ms
Max Path: 23
Matching Paths: 1249
Example Path: [
  'machamp',
  'petilil',
  'landorus',
  'scrafty',
  'yamask',
  'kricketune',
  'emboar',
  'registeel',
  'loudred',
  'darmanitan',
  'nosepass',
  'simisear',
  'relicanth',
  'heatmor',
  'rufflet',
  'trapinch',
  'haxorus',
  'seaking',
  'girafarig',
  'gabite',
  'exeggcute',
  'emolga',
  'audino' ]

```



## jq

Short and sweet, with a nod to efficiency in the form of a dictionary for lookup of still-available words.

jq's "debug" filter is used to illustrate how progress can be monitored.
It writes to stderr.  If your jq does not include "debug", simply remove or comment-out the entire line.

'''Utility functions''':

```jq
# convert a list of unique words to a dictionary
def dictionary:
  reduce .[] as $word ({}; .[$word[0:1]] += [$word]) ;

# remove "word" from the input dictionary assuming the key is already there:
def remove(word):
 .[word[0:1]] -= [word];
```


'''The last-letter/first-letter game''':

```ja
# left-right admissibility
def admissible:
  .[0][-1:] == .[1][0:1];

# input:  [word, dictionary_of_available_words_excluding_word]
# output: a (possibly empty) stream of admissible values: [next_word, updated_dictionary],
#         where next_word can follow the given word.
def next:
  .[0] as $word
   | if $word == null then empty
     else .[1] as $dictionary
     | $word[-1:] as $last
     | (($dictionary[$last] // []) | .[]) as $next
     | [ $next, ($dictionary | remove($next)) ]
     end ;

# Produce an array representing a thread starting at "word":
# Input: [word, dictionary_of_available_words]
def thread:
  if .[1] == [] then [ .[0] ]
  else (next // null) as $next
  | [.[0]] + (if $next then ($next | thread) else [] end)
  end ;

# Input: list of words
# Output: [ maximal_length, maximal_thread]
def maximal:
  def maximum(start):
    . as $dictionary
    | reduce ( [start, ($dictionary | remove(start))] | thread ) as $thread
        ([0, null];
         ($thread|length) as $l
         | if $l > .[0] then [$l, $thread] else . end );

  dictionary as $dictionary
  | reduce .[] as $name
      ( [0,null];
        ($dictionary | maximum($name)) as $ans
	# If your jq does not include "debug", simply remove or comment-out the following line:
	| ([$name, $ans[0]] | debug) as $debug
        | if $ans[0] > .[0] then $ans else . end );
```

'''Example''':

```jq
def names:
  ["audino", "bagon", "baltoy", "banette",
       "bidoof", "braviary", "bronzor", "carracosta", "charmeleon",
       "cresselia", "croagunk", "darmanitan", "deino", "emboar",
       "emolga", "exeggcute", "gabite", "girafarig", "gulpin",
       "haxorus", "heatmor", "heatran", "ivysaur", "jellicent",
       "jumpluff", "kangaskhan", "kricketune", "landorus", "ledyba",
       "loudred", "lumineon", "lunatone", "machamp", "magnezone",
       "mamoswine", "nosepass", "petilil", "pidgeotto", "pikachu",
       "pinsir", "poliwrath", "poochyena", "porygon2", "porygonz",
       "registeel", "relicanth", "remoraid", "rufflet", "sableye",
       "scolipede", "scrafty", "seaking", "sealeo", "silcoon",
       "simisear", "snivy", "snorlax", "spoink", "starly", "tirtouga",
       "trapinch", "treecko", "tyrogue", "vigoroth", "vulpix",
       "wailord", "wartortle", "whismur", "wingull", "yamask" ] ;

names | maximal
```


{{out}} (scrollable)
The "DEBUG" lines are included to illustrate how progress can be monitored. They show the maximal length for the indicated initial word.
<div style="overflow:scroll; height:400px;">

```sh
$ jq -n -f Last_letter-first_letter.jq
["DEBUG:",["audino",1]]
["DEBUG:",["bagon",20]]
["DEBUG:",["baltoy",21]]
["DEBUG:",["banette",20]]
["DEBUG:",["bidoof",1]]
["DEBUG:",["braviary",21]]
["DEBUG:",["bronzor",22]]
["DEBUG:",["carracosta",2]]
["DEBUG:",["charmeleon",20]]
["DEBUG:",["cresselia",2]]
["DEBUG:",["croagunk",21]]
["DEBUG:",["darmanitan",20]]
["DEBUG:",["deino",1]]
["DEBUG:",["emboar",19]]
["DEBUG:",["emolga",2]]
["DEBUG:",["exeggcute",19]]
["DEBUG:",["gabite",20]]
["DEBUG:",["girafarig",20]]
["DEBUG:",["gulpin",20]]
["DEBUG:",["haxorus",20]]
["DEBUG:",["heatmor",20]]
["DEBUG:",["heatran",20]]
["DEBUG:",["ivysaur",22]]
["DEBUG:",["jellicent",21]]
["DEBUG:",["jumpluff",1]]
["DEBUG:",["kangaskhan",20]]
["DEBUG:",["kricketune",20]]
["DEBUG:",["landorus",21]]
["DEBUG:",["ledyba",2]]
["DEBUG:",["loudred",21]]
["DEBUG:",["lumineon",20]]
["DEBUG:",["lunatone",20]]
["DEBUG:",["machamp",23]]
["DEBUG:",["magnezone",20]]
["DEBUG:",["mamoswine",20]]
["DEBUG:",["nosepass",19]]
["DEBUG:",["petilil",22]]
["DEBUG:",["pidgeotto",1]]
["DEBUG:",["pikachu",1]]
["DEBUG:",["pinsir",22]]
["DEBUG:",["poliwrath",21]]
["DEBUG:",["poochyena",2]]
["DEBUG:",["porygon2",1]]
["DEBUG:",["porygonz",1]]
["DEBUG:",["registeel",21]]
["DEBUG:",["relicanth",21]]
["DEBUG:",["remoraid",21]]
["DEBUG:",["rufflet",21]]
["DEBUG:",["sableye",20]]
["DEBUG:",["scolipede",20]]
["DEBUG:",["scrafty",21]]
["DEBUG:",["seaking",21]]
["DEBUG:",["sealeo",1]]
["DEBUG:",["silcoon",20]]
["DEBUG:",["simisear",21]]
["DEBUG:",["snivy",21]]
["DEBUG:",["snorlax",1]]
["DEBUG:",["spoink",21]]
["DEBUG:",["starly",21]]
["DEBUG:",["tirtouga",2]]
["DEBUG:",["trapinch",20]]
["DEBUG:",["treecko",1]]
["DEBUG:",["tyrogue",20]]
["DEBUG:",["vigoroth",21]]
["DEBUG:",["vulpix",1]]
["DEBUG:",["wailord",21]]
["DEBUG:",["wartortle",20]]
["DEBUG:",["whismur",22]]
["DEBUG:",["wingull",22]]
["DEBUG:",["yamask",20]]
[
  23,
  [
    "machamp",
    "petilil",
    "landorus",
    "scrafty",
    "yamask",
    "kricketune",
    "emboar",
    "registeel",
    "loudred",
    "darmanitan",
    "nosepass",
    "simisear",
    "relicanth",
    "heatmor",
    "rufflet",
    "trapinch",
    "haxorus",
    "seaking",
    "girafarig",
    "gabite",
    "exeggcute",
    "emolga",
    "audino"
  ]
]
```
</div>


## Julia

```julia
using IterTools.groupby

orderwords(words::Vector) = Dict(w[1][1] => Set(w) for w in groupby(first, words))
longest(a, b) = ifelse(length(a) > length(b), a, b)
function linkfirst(byfirst::Dict, sofar::Vector)
    @assert(!isempty(sofar))
    chmatch = sofar[end][end]
    if ! haskey(byfirst, chmatch) return sofar end
    options = setdiff(byfirst[chmatch], sofar)
    if isempty(options)
        return sofar
    else
        alternatives = ( linkfirst(byfirst, vcat(sofar, word)) for word in options )
        mx = reduce(longest, alternatives)
        return mx
    end
end
function llfl(words)
    byfirst = orderwords(words)
    alternatives = ( linkfirst(byfirst, [word]) for word in words )
    return reduce(longest, alternatives)
end

pokemon = String.(unique(split("""
    audino bagon baltoy banette bidoof braviary bronzor carracosta charmeleon
    cresselia croagunk darmanitan deino emboar emolga exeggcute gabite
    girafarig gulpin haxorus heatmor heatran ivysaur jellicent jumpluff kangaskhan
    kricketune landorus ledyba loudred lumineon lunatone machamp magnezone mamoswine
    nosepass petilil pidgeotto pikachu pinsir poliwrath poochyena porygon2
    porygonz registeel relicanth remoraid rufflet sableye scolipede scrafty seaking
    sealeo silcoon simisear snivy snorlax spoink starly tirtouga trapinch treecko
    tyrogue vigoroth vulpix wailord wartortle whismur wingull yamask
    """)))

l = llfl(pokemon)
println("Example of longest seq.:\n", join(l, ", "))
println("Max length: ", length(l)
```


```txt
Example of longest seq.:
machamp, pinsir, relicanth, heatmor, registeel, landorus, starly, yamask, kricketune, emboar, rufflet, trapinch, haxorus, simisear, remoraid, darmanitan, nosepass, seaking, girafarig, gabite, exeggcute, emolga, audino
Max length: 23
```



## Kotlin

```scala
// version 1.1.2

var maxPathLength = 0
var maxPathLengthCount = 0
val maxPathExample = StringBuilder(500)

val names = arrayOf(
    "audino", "bagon", "baltoy", "banette", "bidoof",
    "braviary", "bronzor", "carracosta", "charmeleon", "cresselia",
    "croagunk", "darmanitan", "deino", "emboar", "emolga",
    "exeggcute", "gabite", "girafarig", "gulpin", "haxorus",
    "heatmor", "heatran", "ivysaur", "jellicent", "jumpluff",
    "kangaskhan", "kricketune", "landorus", "ledyba", "loudred",
    "lumineon", "lunatone", "machamp", "magnezone", "mamoswine",
    "nosepass", "petilil", "pidgeotto", "pikachu", "pinsir",
    "poliwrath", "poochyena", "porygon2", "porygonz", "registeel",
    "relicanth", "remoraid", "rufflet", "sableye", "scolipede",
    "scrafty", "seaking", "sealeo", "silcoon", "simisear",
    "snivy", "snorlax", "spoink", "starly", "tirtouga",
    "trapinch", "treecko", "tyrogue", "vigoroth", "vulpix",
    "wailord", "wartortle", "whismur", "wingull", "yamask"
)

fun search(part: Array<String>, offset: Int) {
    if (offset > maxPathLength) {
        maxPathLength = offset
        maxPathLengthCount = 1
    }
    else if (offset == maxPathLength) {
        maxPathLengthCount++
        maxPathExample.setLength(0)
        for (i in 0 until offset) {
            maxPathExample.append(if (i % 5 == 0) "\n  " else " ")
            maxPathExample.append(part[i])
        }
    }
    val lastChar = part[offset - 1].last()
    for (i in offset until part.size) {
        if (part[i][0] == lastChar) {
            val tmp = names[offset]
            names[offset] = names[i]
            names[i] = tmp
            search(names, offset + 1)
            names[i] = names[offset]
            names[offset] = tmp
        }
    }
}

fun main(args: Array<String>) {
    for (i in 0 until names.size) {
        val tmp = names[0]
        names[0] = names[i]
        names[i] = tmp
        search(names, 1)
        names[i] = names[0]
        names[0] = tmp
    }
    println("Maximum path length         : $maxPathLength")
    println("Paths of that length        : $maxPathLengthCount")
    println("Example path of that length : $maxPathExample")
}
```


```txt

Maximum path length         : 23
Paths of that length        : 1248
Example path of that length :
  machamp pinsir rufflet trapinch heatmor
  remoraid darmanitan nosepass starly yamask
  kricketune exeggcute emboar relicanth haxorus
  simisear registeel landorus seaking girafarig
  gabite emolga audino

```



## Mathematica


```Mathematica
longestChain[list_] :=
  NestWhileList[
    Append @@@
      Select[DeleteDuplicatesBy[
        Tuples[{#, list}], {#[[1, 1]], #[[2]]} &], ! MemberQ @@ # &&
         StringTake[#[[1, -1]], -1] == StringTake[#[[2]], 1] &] &,
    List /@ list, # != {} &][[-2, 1]];
Print[longestChain[{"audino", "bagon", "baltoy", "banette", "bidoof",
    "braviary", "bronzor", "carracosta", "charmeleon", "cresselia",
    "croagunk", "darmanitan", "deino", "emboar", "emolga",
    "exeggcute", "gabite", "girafarig", "gulpin", "haxorus",
    "heatmor", "heatran", "ivysaur", "jellicent", "jumpluff",
    "kangaskhan", "kricketune", "landorus", "ledyba", "loudred",
    "lumineon", "lunatone", "machamp", "magnezone", "mamoswine",
    "nosepass", "petilil", "pidgeotto", "pikachu", "pinsir",
    "poliwrath", "poochyena", "porygon2", "porygonz", "registeel",
    "relicanth", "remoraid", "rufflet", "sableye", "scolipede",
    "scrafty", "seaking", "sealeo", "silcoon", "simisear", "snivy",
    "snorlax", "spoink", "starly", "tirtouga", "trapinch", "treecko",
    "tyrogue", "vigoroth", "vulpix", "wailord", "wartortle",
    "whismur", "wingull", "yamask"}]];
```

Uses the tactic of only checking chains with the same starting and ending values.
```txt
{baltoy, yamask, kangaskhan, nosepass, sableye, emboar, registeel, landorus, scolipede, emolga, audino}
```



## ooRexx


```ooRexx

-- create the searcher and run it
searcher = .chainsearcher~new

::class chainsearcher
::method init
  expose max searchsize currentlongest

  pokemon_names = "audino bagon baltoy banette bidoof braviary bronzor carracosta charmeleon" -
                  "cresselia croagunk darmanitan deino emboar emolga exeggcute gabite" -
                  "girafarig gulpin haxorus heatmor heatran ivysaur jellicent jumpluff kangaskhan" -
                  "kricketune landorus ledyba loudred lumineon lunatone machamp magnezone mamoswine" -
                  "nosepass petilil pidgeotto pikachu pinsir poliwrath poochyena porygon2" -
                  "porygonz registeel relicanth remoraid rufflet sableye scolipede scrafty seaking" -
                  "sealeo silcoon simisear snivy snorlax spoink starly tirtouga trapinch treecko" -
                  "tyrogue vigoroth vulpix wailord wartortle whismur wingull yamask"

  pokemon = pokemon_names~makearray(" ")
  searchsize = pokemon~items
  currentlongest = 0
  say "searching" searchsize "names..."
  longestchain = .array~new
  -- run the sequence using each possible starting pokemon
  loop i = 1 to pokemon~items
      -- swap the ith name to the front of our list
      self~swap(pokemon, 1, i)
      -- run the chain from here
      self~searchChain(pokemon, longestchain, 2)
      -- swap the name back so we have the list in original form
      self~swap(pokemon, 1, i)
  end
  say "maximum path length:" longestchain~items
  say "paths of that length:" max
  say "example path of that length:"

  loop name over longestchain
      say "    "name
  end

::method swap
  use arg list, a, b
  tmp = list[a]
  list[a] = list[b]
  list[b] = tmp

-- recursive search routine for adding to the chain
::method searchChain
  expose max searchsize currentlongest
  use arg pokemon, longestchain, currentchain

  -- get the last character
  lastchar = pokemon[currentchain - 1]~right(1)
  -- now we search through all of the permutations of remaining
  -- matches to see if we can find a longer chain
  loop i = currentchain to searchsize
      -- for every candidate name from here, recursively extend the chain.
      if pokemon[i]~left(1) == lastchar then do
          if currentchain == currentLongest then max += 1
          -- have we now gone deeper than the current longest chain?
          else if currentchain > currentLongest then do
             -- chuck this result and refill with current set
             longestchain~empty
             longestchain~appendall(pokemon~section(1, currentchain - 1))
             longestchain~append(pokemon[i])
             max = 1
             currentLongest = currentchain
          end
          -- perform the swap again
          self~swap(pokemon, currentchain, i)
          -- run the chain from here
          self~searchChain(pokemon, longestchain, currentchain + 1)
          -- swap the name back so we have the list in original form
          self~swap(pokemon, currentchain, i)
      end
  end

```


```txt

searching 70 names...
maximum path length: 23
paths of that length: 1248
example path of that length:
    machamp
    petilil
    landorus
    scrafty
    yamask
    kricketune
    emboar
    registeel
    loudred
    darmanitan
    nosepass
    simisear
    relicanth
    heatmor
    rufflet
    trapinch
    haxorus
    seaking
    girafarig
    gabite
    exeggcute
    emolga
    audino

```



## OpenEdge/Progress

The following gets the job done, but the time taken (40 minutes) is somewhat worrying when compared to other language solutions. So I am not going after the brownie points just yet...


```progress
DEFINE VARIABLE cpokemon AS CHARACTER INITIAL "audino bagon baltoy banette bidoof braviary bronzor carracosta charmeleon ~
cresselia croagunk darmanitan deino emboar emolga exeggcute gabite ~
girafarig gulpin haxorus heatmor heatran ivysaur jellicent jumpluff kangaskhan ~
kricketune landorus ledyba loudred lumineon lunatone machamp magnezone mamoswine ~
nosepass petilil pidgeotto pikachu pinsir poliwrath poochyena porygon2 ~
porygonz registeel relicanth remoraid rufflet sableye scolipede scrafty seaking ~
sealeo silcoon simisear snivy snorlax spoink starly tirtouga trapinch treecko ~
tyrogue vigoroth vulpix wailord wartortle whismur wingull yamask".

DEFINE TEMP-TABLE tt NO-UNDO
   FIELD cname    AS CHARACTER
   FIELD cfirst   AS CHARACTER
   FIELD clast    AS CHARACTER
   FIELD lused    AS LOGICAL
   FIELD ilength  AS INTEGER
   FIELD imax     AS INTEGER
   FIELD cchain   AS CHARACTER
INDEX ttname   cname
INDEX ttfirst  cfirst lused
INDEX ttlast   clast  lused
.

DEFINE VARIABLE ii AS INTEGER NO-UNDO.

DO  ii = 1 TO NUM-ENTRIES( cpokemon, " " ):
   CREATE tt.
   ASSIGN
      tt.cname    =  ENTRY( ii, cpokemon, " " )
      tt.cfirst   =  SUBSTRING( tt.cname, 1, 1 )
      tt.clast    =  SUBSTRING( tt.cname, LENGTH( tt.cname ), 1 )
      .
END.

FUNCTION getChain RETURNS INTEGER (
   i_cname     AS CHARACTER,
   i_clast     AS CHARACTER,
   i_ilength   AS INTEGER,
   i_cchain    AS CHARACTER
):
   DEFINE BUFFER tt FOR tt.

   DEFINE VARIABLE lend_of_chain AS LOGICAL     NO-UNDO INITIAL TRUE.

   FOR EACH tt
      WHERE tt.cfirst   =  i_clast
      AND   tt.lused    =  FALSE
      OR    i_clast     =  ""
   :
      lend_of_chain = FALSE.
      tt.lused = TRUE.
      getChain( tt.cname, tt.clast, i_ilength + 1, i_cchain + tt.cname + " " ).
      tt.lused = FALSE.
   END.
   IF lend_of_chain THEN DO:
      FIND tt WHERE tt.cname = ENTRY( 1, i_cchain, " " ).
      IF i_ilength = tt.ilength THEN
         tt.imax = tt.imax + 1.
      ELSE IF i_ilength > tt.ilength THEN
         ASSIGN
            tt.ilength  =  i_ilength
            tt.cchain   =  i_cchain
            tt.imax     =  1
            .
   END.

END FUNCTION. /* getChain */

DEFINE VARIABLE itime      AS INTEGER     NO-UNDO EXTENT 2.
DEFINE VARIABLE lcontinue  AS LOGICAL     NO-UNDO.

itime[1] = ETIME.
getChain( "", "", 0, "" ).
itime[2] = ETIME.

FOR EACH tt BY tt.ilength DESCENDING:
   MESSAGE
      "Maximum path length:"  tt.ilength SKIP
      "Paths of that length:" tt.imax SKIP(1)
      "Example path of that length:" tt.cchain SKIP(1)
      "Time taken:" STRING( INTEGER( ( itime[2] - itime[1] ) / 1000 ), "HH:MM:SS" )
   VIEW-AS ALERT-BOX BUTTONS YES-NO TITLE tt.cname UPDATE lcontinue.
   IF lcontinue = FALSE THEN
      STOP.
END.
```


Output:


```txt
---------------------------
machamp
---------------------------
Maximum path length: 23
Paths of that length: 1248

Example path of that length: machamp petilil landorus scrafty yamask kricketune emboar registeel loudred darmanitan nosepass simisear relicanth heatmor rufflet trapinch haxorus seaking girafarig gabite exeggcute emolga audino

Time taken: 00:40:09
---------------------------
Yes   No
---------------------------
```



## PicoLisp


```PicoLisp
(de pokemonChain (File)
   (let Names (make (in File (while (read) (link @))))
      (for Name Names
         (let C (last (chop Name))
            (set Name
               (filter '((Nm) (pre? C Nm)) Names) ) ) )
      (let Res NIL
         (for Name Names
            (let Lst NIL
               (recur (Name Lst)
                  (if (or (memq Name Lst) (not (val (push 'Lst Name))))
                     (when (> (length Lst) (length Res))
                        (setq Res Lst) )
                     (mapc recurse (val Name) (circ Lst)) ) ) ) )
         (flip Res) ) ) )
```

Test:

```txt

: (pokemonChain "pokemon.list")
-> (machamp petilil landorus scrafty yamask kricketune emboar registeel loudred
darmanitan nosepass simisear relicanth heatmor rufflet trapinch haxorus seaking
girafarig gabite exeggcute emolga audino)
: (length @)
-> 23

```



## Perl

This is rather 'one liner' code, not to be used in production.

The idea is to try all possible variants recursively.

*First, it creates the map-like structure: first letter → array of (name + last letter).
*During the cycle it uses @w as stack;
*@m keeps the longest sequence which is copied from @w;
*to prevent the words from appearing twice, they are (temporarily) deleted from the structure keeping the value in a stack variable.

```perl
use strict;
my(%f,@m);

/^(.).*(.)$/,$f{$1}{$_}=$2 for qw(
audino bagon baltoy banette bidoof braviary bronzor carracosta charmeleon
cresselia croagunk darmanitan deino emboar emolga exeggcute gabite
girafarig gulpin haxorus heatmor heatran ivysaur jellicent jumpluff kangaskhan
kricketune landorus ledyba loudred lumineon lunatone machamp magnezone mamoswine
nosepass petilil pidgeotto pikachu pinsir poliwrath poochyena porygon2
porygonz registeel relicanth remoraid rufflet sableye scolipede scrafty seaking
sealeo silcoon simisear snivy snorlax spoink starly tirtouga trapinch treecko
tyrogue vigoroth vulpix wailord wartortle whismur wingull yamask
);

sub poke {
    our @w;
    my $h = $f{$_[0]};
    for my $word (keys %$h) {
	my $v = $h->{$word};
	delete $h->{$word};
	push @w, $word;
	@m = @w if @w > @m;
	poke($v);
	pop @w;
	$h->{$word} = $v;
    }
}

poke($_) for keys %f;
print @m.": @m\n";
```

```txt
23: machamp petilil landorus seaking girafarig gabite emboar registeel loudred darmanitan nosepass simisear relicanth heatmor rufflet trapinch haxorus scrafty yamask kricketune exeggcute emolga audino
```



## Perl 6

A breadth-first search that uses disk files to avoid memory exhaustion.  Each candidate sequence is encoded at one character per name, so to avoid reuse of names we merely have to make sure there are no repeat characters in our encoded string.  (The encoding starts at ASCII space for the first name, so newline is not among the encoded characters.)

```perl6
my @names = <
    audino bagon baltoy banette bidoof braviary bronzor carracosta charmeleon
    cresselia croagunk darmanitan deino emboar emolga exeggcute gabite
    girafarig gulpin haxorus heatmor heatran ivysaur jellicent jumpluff kangaskhan
    kricketune landorus ledyba loudred lumineon lunatone machamp magnezone mamoswine
    nosepass petilil pidgeotto pikachu pinsir poliwrath poochyena porygon2
    porygonz registeel relicanth remoraid rufflet sableye scolipede scrafty seaking
    sealeo silcoon simisear snivy snorlax spoink starly tirtouga trapinch treecko
    tyrogue vigoroth vulpix wailord wartortle whismur wingull yamask
>;

my @last = @names.map: {.substr(*-1,1).ord }
my @succs = [] xx 128;
for @names.kv -> $i, $name {
    my $ix = $name.ord; # $name.substr(0,1).ord
    push @succs[$ix], $i;
}

my $OUT = open "llfl.new", :w orelse .die;
$OUT.print: chr($_ + 32),"\n" for 0 ..^ @names;
close $OUT;
my $new = +@names;
my $len = 1;

while $new {
    say "Length { $len++ }: $new candidates";
    shell 'mv llfl.new llfl.known';
    my $IN = open "llfl.known" orelse .die;
    my $OUT = open "llfl.new", :w orelse .die;
    $new = 0;

    loop {
	my $cand = $IN.get // last;
	for @succs[@last[$cand.ord - 32]][] -> $i {
	    my $ic = chr($i + 32);
	    next if $cand ~~ /$ic/;
	    $OUT.print: $ic,$cand,"\n";
	    $new++;
	}
    }
    $IN.close;
    $OUT.close;
}

my $IN = open "llfl.known" orelse .die;
my $eg = $IN.lines.pick;
say "Length of longest: ", $eg.chars;
say join ' ', $eg.ords.reverse.map: { @names[$_ - 32] }
```

```txt
Length 1: 70 candidates
Length 2: 172 candidates
Length 3: 494 candidates
Length 4: 1288 candidates
Length 5: 3235 candidates
Length 6: 7731 candidates
Length 7: 17628 candidates
Length 8: 37629 candidates
Length 9: 75122 candidates
Length 10: 139091 candidates
Length 11: 236679 candidates
Length 12: 367405 candidates
Length 13: 516210 candidates
Length 14: 650916 candidates
Length 15: 733915 candidates
Length 16: 727566 candidates
Length 17: 621835 candidates
Length 18: 446666 candidates
Length 19: 260862 candidates
Length 20: 119908 candidates
Length 21: 40296 candidates
Length 22: 10112 candidates
Length 23: 1248 candidates
Length of longest: 23
machamp petilil loudred darmanitan nosepass simisear rufflet trapinch heatmor registeel landorus starly yamask kricketune exeggcute emboar relicanth haxorus seaking girafarig gabite emolga audino
```



## Phix

Using simple start-with-same-letter word chains to minimise the number of elements we have to consider:

```Phix
constant words = {"audino","bagon","baltoy","banette","bidoof","braviary","bronzor","carracosta","charmeleon","cresselia","croagunk",
                  "darmanitan","deino","emboar","emolga","exeggcute","gabite","girafarig","gulpin","haxorus","heatmor","heatran",
                  "ivysaur","jellicent","jumpluff","kangaskhan","kricketune","landorus","ledyba","loudred","lumineon","lunatone",
                  "machamp","magnezone","mamoswine","nosepass","petilil","pidgeotto","pikachu","pinsir","poliwrath","poochyena",
                  "porygon2","porygonz","registeel","relicanth","remoraid","rufflet","sableye","scolipede","scrafty","seaking",
                  "sealeo","silcoon","simisear","snivy","snorlax","spoink","starly","tirtouga","trapinch","treecko","tyrogue",
                  "vigoroth","vulpix","wailord","wartortle","whismur","wingull","yamask"}

function word_chains()
sequence first = repeat(0,256),             -- start of chains for a given letter
                                            -- first['a']=1, first['b']=2, first['c']=8, etc.
         snext = repeat(0,length(words))    -- chains of words starting with the same letter
                                            -- a: snext[1]=0, b: snext[2..7]={3,4,5,6,7,0}, etc.
    for i=1 to length(words) do
        integer ch = words[i][1]
        if first[ch]=0 then
            first[ch] = i
        end if
        for j=i+1 to length(words) do
            if words[j][1]=ch then
                snext[i] = j
                exit
            end if
        end for
    end for
    return {first,snext}
end function

constant {first,snext} = word_chains()

-- maintain words already taken as a linked list:
integer tstart
sequence taken = repeat(0,length(words))    -- 0=no, -1=end of chain, +ve=next

-- and keep a copy of the best for later
integer bstart
sequence best
integer maxn = 0
integer count

procedure try(integer ch, integer last, integer n)
    integer next = first[ch]
    while next!=0 do
        if taken[next]=0 then
            taken[last] = next
            taken[next] = -1
            try(words[next][$],next,n+1)
            taken[last] = -1
            taken[next] = 0
        end if
        next = snext[next]
    end while
    if n>maxn then
        bstart = tstart
        best = taken
        maxn = n
        count = 1
    elsif n=maxn then
        count += 1
    end if
end procedure

atom t0=time()

for i=1 to length(words) do
    tstart = i
    taken[i] = -1
    try(words[i][$],i,1)
    taken[i] = 0
end for

printf(1,"Runtime: %2.3f seconds. Max length:%d, found %d of such, one of which is:\n",{time()-t0,maxn,count})
while 1 do
    printf(1,"%s ",words[bstart])
    bstart = best[bstart]
    if bstart=-1 then exit end if
end while
```

```txt

Runtime: 0.656 seconds. Max length:23, found 1248 of such, one of which is:
machamp petilil landorus scrafty yamask kricketune emboar registeel loudred darmanitan nosepass simisear
relicanth heatmor rufflet trapinch haxorus seaking girafarig gabite exeggcute emolga audino

```

My claim for the extra brownie points, using the list from the Racket entry:

```txt

Runtime: 0.000 seconds. Max length:2, found 9 of such, one of which is:
Porygon-Z Zubat

```

Which is quite correct: 9 names begin with 'Z', and plenty with 'T' but none with 't' ;-)

Adding a couple of upper(), it found a 302 in a couple of minutes but was still on permutations starting
with word[1] when I got bored and killed it, quickly calculating at the very least another 4.5 days and
quite probably not finishing within my lifetime...


## Prolog

Works with SWI-Prolog and module '''lambda.pl''' written  by '''Ulrich Neumerkel''' found there http://www.complang.tuwien.ac.at/ulrich/Prolog-inedit/lambda.pl

```Prolog
:- use_module(library(lambda)).

:- dynamic res/3.

last_first(Len, Nb, L) :-
	retractall(res(_,_,_)),
	assert(res(0, 0, [])),
	% compute all the lists of connected words
	last_first,
	res(Len, Nb, L1),
	% to have only the words
	maplist(\X^Y^(X = [Y, _, _]), L1, L).

% create  the lists of connected words (initiate the first word)
last_first :-
	init(L),
	forall(select(Word, L, L1),
	       \+lance_p([Word | L1])).

% compute all the lists beginning with a word
% memorize the longest
lance_p(L) :-
	p(LF, L),
	retract(res(Len, Nb, Lst)),
	length(LF, Len1),
	(  Len1 > Len
	->  assert(res(Len1, 1, LF))
	;   Len1 = Len
	->  Nb1 is Nb + 1,
	    assert(res(Len, Nb1, Lst))
	;   assert(res(Len, Nb, Lst))),
	fail.

% describe the property of the list of connected words
p([A | T], [A | L]) :-
	select(B, L, L1),
	p0(A,B),
	T = [B | T1],
	p([B | T1], [B | L1]).

% a list with one element is valid
p([_], _).


% are words conected ?
p0([_, _, W], [_, W, _]).

% each word is associated with its first and last letters
% audino --> [audino, a, o]
init(L) :-

	L0 = [ audino, bagon, baltoy, banette, bidoof, braviary, bronzor,
	     carracosta, charmeleon, cresselia, croagunk, darmanitan, deino,
	     emboar, emolga, exeggcute, gabite, girafarig, gulpin, haxorus,
	     heatmor, heatran, ivysaur, jellicent, jumpluff, kangaskhan,
	     kricketune, landorus, ledyba, loudred, lumineon, lunatone,
	     machamp, magnezone, mamoswine, nosepass, petilil, pidgeotto,
	     pikachu, pinsir, poliwrath, poochyena, porygon2, porygonz,
	     registeel, relicanth, remoraid, rufflet, sableye, scolipede,
	     scrafty, seaking, sealeo, silcoon, simisear, snivy, snorlax,
	     spoink, starly, tirtouga, trapinch, treecko, tyrogue, vigoroth,
	     vulpix, wailord, wartortle, whismur, wingull, yamask],
	maplist(init_, L0, L).

% audino --> [audino, a, o]
init_(W, [W, F, L]) :-
	first_letter(W, F),
	last_letter(W, L).


first_letter(A, F) :-
	atom_chars(A, [F | _]).

last_letter(A, L) :-
	atom_chars(A, LC),
	reverse(LC, [L | _]).

```

Output :

```txt
?- time(last_first(Len, Nb, L)).
% 592,161,339 inferences, 125.690 CPU in 128.264 seconds (98% CPU, 4711284 Lips)
Len = 23,
Nb = 1248,
L = [machamp,petilil,landorus,scrafty,yamask,kricketune,emboar,registeel,loudred,darmanitan,nosepass,simisear,relicanth,heatmor,rufflet,trapinch,haxorus,seaking,girafarig,gabite,exeggcute,emolga,audino].

```



## Python


```python
from collections import defaultdict

def order_words(words):
    byfirst = defaultdict(set)
    for word in words:
        byfirst[word[0]].add( word )
    #byfirst = dict(byfirst)
    return byfirst

def linkfirst(byfirst, sofar):
    '''\
    For all words matching last char of last word in sofar as FIRST char and not in sofar,
    return longest chain as sofar + chain
    '''

    assert sofar
    chmatch = sofar[-1][-1]
    options = byfirst[chmatch] - set(sofar)
    #print('  linkfirst options: %r %r' % (chmatch, options))
    if not options:
        return sofar
    else:
        alternatives = ( linkfirst(byfirst, list(sofar) + [word])
                         for word in options )
        mx = max( alternatives, key=len )
        #input('linkfirst: %r' % mx)
        return mx

def llfl(words):

    byfirst = order_words(words)
    return max( (linkfirst(byfirst, [word]) for word in words), key=len )

if __name__ == '__main__':
    pokemon = '''audino bagon baltoy banette bidoof braviary bronzor carracosta charmeleon
cresselia croagunk darmanitan deino emboar emolga exeggcute gabite
girafarig gulpin haxorus heatmor heatran ivysaur jellicent jumpluff kangaskhan
kricketune landorus ledyba loudred lumineon lunatone machamp magnezone mamoswine
nosepass petilil pidgeotto pikachu pinsir poliwrath poochyena porygon2
porygonz registeel relicanth remoraid rufflet sableye scolipede scrafty seaking
sealeo silcoon simisear snivy snorlax spoink starly tirtouga trapinch treecko
tyrogue vigoroth vulpix wailord wartortle whismur wingull yamask'''
    pokemon = pokemon.strip().lower().split()
    pokemon = sorted(set(pokemon))
    l = llfl(pokemon)
    for i in range(0, len(l), 8): print(' '.join(l[i:i+8]))
    print(len(l))
```


;Sample output

```txt
audino bagon baltoy banette bidoof braviary bronzor carracosta
charmeleon cresselia croagunk darmanitan deino emboar emolga exeggcute
gabite girafarig gulpin haxorus heatmor heatran ivysaur jellicent
23
```


### Alternative version

Adapted from the D version. This uses Psyco.

```python
import psyco

nsolutions = 0

def search(sequences, ord_minc, curr_word, current_path,
           current_path_len, longest_path):
    global nsolutions

    current_path[current_path_len] = curr_word
    current_path_len += 1

    if current_path_len == len(longest_path):
        nsolutions += 1
    elif current_path_len > len(longest_path):
        nsolutions = 1
        longest_path[:] = current_path[:current_path_len]

    # recursive search
    last_char_index = ord(curr_word[-1]) - ord_minc
    if last_char_index >= 0 and last_char_index < len(sequences):
        for pair in sequences[last_char_index]:
            if not pair[1]:
                pair[1] = True
                search(sequences, ord_minc, pair[0], current_path,
                       current_path_len, longest_path)
                pair[1] = False


def find_longest_chain(words):
    ord_minc = ord(min(word[0] for word in words))
    ord_maxc = ord(max(word[0] for word in words))
    sequences = [[] for _ in xrange(ord_maxc - ord_minc + 1)]
    for word in words:
        sequences[ord(word[0]) - ord_minc].append([word, False])

    current_path = [None] * len(words)
    longest_path = []

    # try each item as possible start
    for seq in sequences:
        for pair in seq:
            pair[1] = True
            search(sequences, ord_minc, pair[0],
                   current_path, 0, longest_path)
            pair[1] = False

    return longest_path


def main():
    global nsolutions

    pokemon = """audino bagon baltoy banette bidoof braviary
bronzor carracosta charmeleon cresselia croagunk darmanitan deino
emboar emolga exeggcute gabite girafarig gulpin haxorus heatmor
heatran ivysaur jellicent jumpluff kangaskhan kricketune landorus
ledyba loudred lumineon lunatone machamp magnezone mamoswine nosepass
petilil pidgeotto pikachu pinsir poliwrath poochyena porygon2
porygonz registeel relicanth remoraid rufflet sableye scolipede
scrafty seaking sealeo silcoon simisear snivy snorlax spoink starly
tirtouga trapinch treecko tyrogue vigoroth vulpix wailord wartortle
whismur wingull yamask""".lower().split()

    # remove duplicates
    pokemon = sorted(set(pokemon))

    sol = find_longest_chain(pokemon)
    print "Maximum path length:", len(sol)
    print "Paths of that length:", nsolutions
    print "Example path of that length:"
    for i in xrange(0, len(sol), 7):
        print " ", " ".join(sol[i : i+7])

psyco.full()
main()
```

Output:

```txt
Maximum path length: 23
Paths of that length: 1248
Example path of that length:
  machamp petilil landorus scrafty yamask kricketune emboar
  registeel loudred darmanitan nosepass simisear relicanth heatmor
  rufflet trapinch haxorus seaking girafarig gabite exeggcute
  emolga audino
```

Run time: about 0.44 seconds with Psyco and Python 2.6.6.


## Racket

This is a naive solution, which works fast enough as is (takes about 5 seconds on an old machine):

```racket
#lang racket

(define names "
  audino bagon baltoy banette bidoof braviary bronzor carracosta charmeleon
  cresselia croagunk darmanitan deino emboar emolga exeggcute gabite girafarig
  gulpin haxorus heatmor heatran ivysaur jellicent jumpluff kangaskhan
  kricketune landorus ledyba loudred lumineon lunatone machamp magnezone
  mamoswine nosepass petilil pidgeotto pikachu pinsir poliwrath poochyena
  porygon2 porygonz registeel relicanth remoraid rufflet sableye scolipede
  scrafty seaking sealeo silcoon simisear snivy snorlax spoink starly tirtouga
  trapinch treecko tyrogue vigoroth vulpix wailord wartortle whismur wingull
  yamask")

(struct word (first last string) #:prefab)
(define words
  (for/list ([str (string-split names)])
    (word (string->symbol (substring str 0 1))
          (string->symbol (substring str (sub1 (string-length str))))
          str)))

(define (find-longest last words)
  (for/fold ([best '()])
            ([w (in-list words)]
             #:when (or (not last) (eq? last (word-first w))))
    (define long (cons w (find-longest (word-last w) (remq w words))))
    (if (> (length long) (length best)) long best)))

(define longest (find-longest #f words))
(printf "Longest chain found has ~a words:\n  ~a\n"
        (length longest) (string-join (map word-string longest) " -> "))

```

```txt

Longest chain found has 23 words:
  machamp -> petilil -> landorus -> scrafty -> yamask -> kricketune -> emboar -> registeel -> loudred -> darmanitan -> nosepass -> simisear -> relicanth -> heatmor -> rufflet -> trapinch -> haxorus -> seaking -> girafarig -> gabite -> exeggcute -> emolga -> audino

```


This is a (probably over-)complex version which tries to construct longest
chains using three different combination/relinking functions. Not that the
definiton of `word` is slightly different here.

```racket
#lang racket
(require "pokemon-names.rkt")

;;; Some fundamentals... finding the first (henceforth referred to as "a") and last ("z")
;;; letters of a word can be computationally intensive... look at symbol->word, and you'll
;;; see that when we have to deal with a name like: "Mime Jr.", the last alphabetic letter
;;; is not the last character. And the first and last characters (at least) have to be
;;; case-normalised so they can be compared with char=? (it's not particulary helpful to
;;; map them down to integer character codes; we'll want to see them for debugging).
(define-struct word (sym char-a char-z) #:prefab)

;;; names are input as symbols both for ease of comparsion, and because it's easier to type
(define (symbol->word sym)
  (let* ((str (symbol->string sym))
         (chrs (string->list str))
         (fst (for/first ((c chrs) #:when (char-alphabetic? c)) (char-downcase c)))
         (lst (for/last ((c chrs) #:when (char-alphabetic? c)) (char-downcase c))))
    (make-word sym fst lst)))

;;; We're sometimes not interested in debugging a chain of; just in knowing how long it is
;;; and what its extreme characters are. This does the trick.
(define (summarise-chain c)
  (format "(~a.~a.~a)" (word-char-a (first c)) (sub1 (length c)) (word-char-z (last c))))

;; Test the above (run `raco test last_letter-first_letter-common.rkt`)
(define-syntax-rule (hash-set-or-remove hsh key val remove-pred?)
  (let ((v val))
    (if (remove-pred? v)
        (hash-remove hsh key)
        (hash-set hsh key v))))

(define-syntax-rule (find-a-in-chain-pool chains a dont-match-sym)
  (for/first ((c chains)
              (i (in-naturals)) ;; usually need an index for regenerating chains pool
              ;; a word can only exist in one chain, so this compares chains' identities
              #:unless (eq? (word-sym (first c)) dont-match-sym)
              #:when (char=? (word-char-a (first c)) a))
    (cons i c)))

(define-syntax-rule (copy-list-ignoring-indices lst i1 i2)
  (for/list ((idx (in-naturals))
             (e (in-list lst))
             #:unless (= idx i1)
             #:unless (= idx i2))
    e))

;; Simple ... find a chain that can be put on the end of c... append it, and
;; reiterate
(define (append-ab..bz-chains chain chain-idx chains)
  (let* ((a1.chain-a (find-a-in-chain-pool chains (word-char-z (last chain)) (word-sym (first chain)))))
    (and a1.chain-a
         (let ((a1.chain-idx (first a1.chain-a))
               (a1.chain-chain (rest a1.chain-a)))
           (cons (append chain a1.chain-chain)
                 (copy-list-ignoring-indices chains chain-idx a1.chain-idx))))))

;; If chain has an a..a loop in it, then we see if we can take that loop, and
;; place it in a longer chain at a point where a is used.
;;
;; `chain` is the shorter chain containing the loop
(define (merge-chain-into-chain-accepting-a..a-in-chain chain chain-idx chains)
  ;; for a..a loops in chain, returns a hash from the looped char, to the longest
  ;; found loop
  (define (find-a..a-loops chain)
    (let ((chain-length (length chain)))
      (for*/fold
          ((hsh (hash)))
        ((sub-range-start (in-range chain-length))
         (aa (in-value (word-char-a (list-ref chain sub-range-start))))
         (sub-range-end (in-range sub-range-start chain-length))
         #:when (eq? aa (word-char-z (list-ref chain sub-range-end)))
         (range-length (in-value (add1 (- sub-range-end sub-range-start)))))
        (hash-update
         hsh aa
         (lambda (longest-range)
           (if (and longest-range (> (third longest-range) range-length))
               longest-range
               (list sub-range-start sub-range-end range-length)))
         #f))))

  (let* ((chain-first-name (word-sym (first chain)))
         (chain-length (length chain))
         (a..a-list (sort (hash->list (find-a..a-loops chain)) > #:key third)))
    (for*/first (((chain2 chain2-idx) (in-parallel (in-list chains) (in-naturals)))
                 #:unless (eq? chain-first-name (word-sym (car chain2)))
                 (chain2-length (in-value (length chain2)))
                 #:when (>= chain2-length chain-length) ; only move the largest a..a-subchain into a larger chain
                 (a..a (in-list a..a-list))
                 ((insertion-point-word insertion-point-idx) (in-parallel (in-list chain2) (in-naturals)))
                 #:when (eq? (car a..a) (word-char-a insertion-point-word)))
      (let* ((new-chain (append (take chain (second a..a)) (drop chain (add1 (third a..a)))))
             (a..a-chain (take (drop chain (second a..a)) (fourth a..a)))
             (new-chain2 (append (take chain2 insertion-point-idx) a..a-chain (drop chain2 insertion-point-idx))))
        (let ((new-chains (copy-list-ignoring-indices chains chain-idx chain2-idx)))
          (if (null? new-chain) (cons new-chain2 new-chains)
              (cons new-chain (cons new-chain2 new-chains))))))))

;; this is a bit more combinatorially intensive... for all c2, substitute a
;; subrange in c2 that is longer than an equivalent subrange in c
(define (merge-subranges-of-chains-into-chain chain chain-idx chains)
  (let ((chain-first-name (word-sym (first chain)))
        (chain-length (length chain))
        (chain-first-a (word-char-a (first chain)))
        (chain-last-z (word-char-z (last chain))))
    (for*/first ; try to replace a subrange in c2 with c
        (((chain2 chain2-idx) (in-parallel (in-list chains) (in-naturals)))
         (chain2-length (in-value (length chain2)))
         #:unless (eq? chain-first-name (word-sym (car chain2)))
         (c2-sub-range-start (in-range chain2-length))
         (c2-sub-range-end (in-range c2-sub-range-start
                                     (min chain2-length (+ c2-sub-range-start chain-length))))
         #:unless (and (= c2-sub-range-start 0) (= c2-sub-range-end (sub1 chain2-length)))
         #:when (or (zero? c2-sub-range-start)
                    (eq? (word-char-a (list-ref chain2 c2-sub-range-start))
                         chain-first-a))
         #:when (or (= (sub1 chain2-length) c2-sub-range-end)
                    (eq? (word-char-z (list-ref chain2 c2-sub-range-end))
                         chain-last-z))
         (c2-sub-range-len (in-value (add1 (- c2-sub-range-end c2-sub-range-start))))
         #:when (> chain-length c2-sub-range-len)
         (c2-sub-range (in-value (take (drop chain2 c2-sub-range-start) c2-sub-range-len)))
         (new-c2 (in-value (append (take chain2 c2-sub-range-start)
                                   chain
                                   (drop chain2 (add1 c2-sub-range-end))))))
      (cons c2-sub-range ; put the subrange back into the chains pool
            (cons new-c2 ; put the modified onto the chains pool
                  (copy-list-ignoring-indices chains chain-idx chain2-idx))))))

(define (longest-chain/constructive names #:known-max (known-max +inf.0))
  (define names-list (map symbol->word names))

  (define (link-chains chains)
    (let
        ((new-chains
          (for*/first
              (((chain chain-idx) (in-parallel (in-list chains) (in-naturals)))
               (new-chains
                (in-value
                 (or
                  (append-ab..bz-chains chain chain-idx chains)
                  (merge-chain-into-chain-accepting-a..a-in-chain chain chain-idx chains)
                  (merge-subranges-of-chains-into-chain chain chain-idx chains))))
               #:when new-chains)
            new-chains)))
      (if new-chains (link-chains new-chains) chains)))

  (define (keep-trying
           (run-count 0)
           (linked-chains (link-chains (map list (shuffle names-list))))
           (previous-best null)
           (previous-best-length 0)
           (this-run-best-length #f))
    (let* ((longest-chain (argmax length linked-chains))
           (longest-chain-len (length longest-chain))
           (new-best? (< previous-best-length longest-chain-len))
           (best-length (if new-best? longest-chain-len previous-best-length))
           (best (if new-best? longest-chain previous-best)))
      (when new-best? (newline)
        (displayln (list (map word-sym longest-chain) longest-chain-len))
        (flush-output))
      (if (and new-best? (>= best-length known-max))
          (displayln "terminating: known max reached or exceeded")
          (begin
            (when (zero? (modulo (add1 run-count) 250)) (eprintf ".") (flush-output (current-error-port)))
            (if (= run-count 1000)
                (keep-trying 0 (link-chains (map list (shuffle names-list))) best best-length 0)
                (let ((sorted-linked-chains (sort linked-chains #:key length >)))
                  (keep-trying (if new-best? 0 (add1 run-count))
                               (link-chains
                                (cons (car sorted-linked-chains)
                                      (map list (shuffle (apply append (cdr sorted-linked-chains))))))
                               best best-length
                               (and this-run-best-length
                                    (if new-best? #f
                                        (if (< this-run-best-length longest-chain-len)
                                            (begin (eprintf ">~a" longest-chain-len)
                                                   (flush-output (current-error-port))
                                                   longest-chain-len)
                                            this-run-best-length))))))))))
  (keep-trying))

(time (longest-chain/constructive names-70 #:known-max 23))
(longest-chain/constructive names-646)
```


Run with ''racket -t last_letter-first_letter-randomised.rkt 2>&1'', to redirect standard error to
standard out.

```txt

((machamp petilil landorus seaking girafarig gabite exeggcute emboar relicanth heatran nosepass simisear registeel loudred deino) 15)

((machamp petilil landorus seaking girafarig gabite exeggcute emboar relicanth haxorus snivy yamask kangaskhan nosepass simisear registeel lunatone emolga audino) 19)

((machamp petilil landorus seaking girafarig gabite exeggcute emboar rufflet trapinch heatmor relicanth haxorus snivy yamask kangaskhan nosepass simisear registeel lunatone emolga audino) 22)
....>10>17>19....>14>17>20>21....>15>18>21>22....>16>17>22....>15>17>18>21....>19>20....>16>18>20>22....>14>21
((machamp petilil landorus seaking girafarig gabite emboar rufflet trapinch heatmor relicanth haxorus simisear registeel loudred darmanitan nosepass starly yamask kricketune exeggcute emolga audino) 23)
terminating: known max reached or exceeded
cpu time: 24077 real time: 24070 gc time: 36
```

As an aside... I've had this take 40 s; and I've had it run in 35 ms. It really is the luck of the draw!

```

((Jellicent Tangrowth Hippopotas Slowking Gyarados Spiritomb Breloom Mandibuzz Zweilous Shellos Stoutland Deoxys Swoobat Togekiss Seedot Toxicroak Krokorok Kabutops Seadra Anorith Heracross Slakoth Haxorus Shelmet Tauros Skiploom Manectric Castform Milotic Chingling Golem Mothim Meganium Metapod Ducklett Tornadus Swellow Whismur Rufflet Taillow Wormadam Medicham Mismagius Snubbull Latias Solosis Sandshrew Woobat Teddiursa Abomasnow Wingull Lapras Seel Lillipup Paras Sandslash Hoppip Prinplup Piplup Petilil Lickilicky Yanmega Abra Ariados Solrock Klink Kingler Relicanth Hariyama Altaria Azumarill Luvdisc Charmander Rotom Maractus Skuntank Kirlia Azelf Farfetch'd Delibird Dratini Illumise Exploud Diglett Torkoal Lampent Turtwig Girafarig Golbat Tepig Grumpig Granbull Larvesta Azurill Loudred Dialga Amoonguss Sigilyph Herdier Rampardos Seaking Golett Timburr Raichu Umbreon Noctowl Latios Sawk Kyurem Miltank Klinklang Gorebyss Slaking Gliscor Rhyhorn Nidorina Alakazam Machamp Pidgeot Trubbish Hitmontop Politoed Dewgong Gardevoir Regigigas Swampert Tympole Electrode Exeggcute Eelektrik Kingdra Accelgor Remoraid Dewott Tentacool Litwick Koffing Gigalith Hypno Octillery Yanma Arcanine Elekid Dusknoir Reshiram Mesprit Tropius Swalot Tranquill Luxray Yamask Kyogre Eevee Electivire Emolga Alomomola Ampharos Spinarak Kricketune Exeggutor Roselia Arbok Krookodile Escavalier Rayquaza Aron Ninetales Serperior Registeel Lileep Patrat Throh Hitmonlee Espeon Nidoqueen Ninjask Kakuna Axew Wigglytuff Frillish Houndour Regice Eelektross Swinub Budew Whiscash Hoothoot Torterra Aipom Mareep Poliwag Grimer Roggenrola Armaldo Omastar Rhyperior Ralts Scolipede Excadrill Luxio Oshawott Togetic Crawdaunt Torchic Cofagrigus Shroomish Houndoom Marill Larvitar Raticate Electrike Ekans Starmie Emboar Regirock Karrablast Tangela Aerodactyl Lairon Nidoran Natu Unfezant Trapinch Horsea Archen Nidoran Nosepass Staravia Ambipom Murkrow Weezing Gothita Absol Lotad Darmanitan Nincada Arceus Spheal Lilligant Tynamo Omanyte Elgyem Mew Watchog Golduck Kabuto Oddish Ho-Oh Heatran Nidoking Golurk Klang Garchomp Porygon-Z Zekrom Meowth Hippowdon Nuzleaf Feraligatr Rapidash Haunter Reuniclus Seviper Raikou Ursaring Glalie Entei Igglybuff Froslass Spearow Wailord Duskull Ludicolo Onix Xatu Unown Numel Landorus Seismitoad Deerling Grovyle Empoleon Nidorino) 283)
```

'''...'''

```txt
((Voltorb Beartic Croconaw Wynaut Tangrowth Hippopotas Smoochum Moltres Shuppet Thundurus Slowking Gyarados Spiritomb Breloom Mandibuzz Zweilous Shellos Stoutland Deoxys Swoobat Togekiss Seedot Toxicroak Krokorok Kabutops Snorunt Tirtouga Anorith Heracross Slakoth Haxorus Shelmet Tauros Skiploom Manectric Castform Milotic Chingling Golem Mothim Meganium Metapod Ducklett Tornadus Swellow Whismur Rufflet Taillow Wormadam Medicham Mismagius Snubbull Latias Solosis Sandshrew Woobat Teddiursa Abomasnow Wingull Lapras Seel Lillipup Paras Sandslash Hoppip Prinplup Piplup Probopass Stunfisk Krabby Yanmega Abra Ariados Solrock Klink Kingler Relicanth Hariyama Altaria Azumarill Luvdisc Chatot Tyranitar Rotom Maractus Skuntank Kirlia Azelf Farfetch'd Delibird Dratini Ivysaur Riolu Uxie Electabuzz Zapdos Sawsbuck Kyogre Exploud Diglett Torkoal Lampent Turtwig Girafarig Golbat Tepig Grumpig Granbull Larvesta Azurill Loudred Dialga Amoonguss Sigilyph Herdier Rampardos Seaking Golett Timburr Raichu Umbreon Noctowl Latios Sawk Kyurem Miltank Klinklang Gorebyss Slaking Gigalith Huntail Ledyba Aggron Nidorina Alakazam Machamp Pidgeot Trubbish Hitmontop Politoed Dewgong Gardevoir Regigigas Swampert Tentacruel Lickitung Glameow Whirlipede Electrode Exeggcute Eelektrik Kingdra Accelgor Remoraid Dewott Tentacool Litwick Koffing Gloom Marshtomp Psyduck Kadabra Articuno Octillery Yanma Arcanine Elekid Dusknoir Reshiram Mesprit Tropius Swalot Tranquill Luxray Yamask Kricketot Tyrogue Eevee Electivire Emolga Alomomola Ampharos Sentret Togepi Infernape Exeggutor Roselia Arbok Krookodile Escavalier Rayquaza Aron Ninetales Serperior Registeel Lileep Patrat Throh Honchkrow Wobbuffet Totodile Espeon Nidoqueen Ninjask Kakuna Axew Wigglytuff Frillish Houndour Regice Eelektross Swinub Budew Whiscash Hoothoot Torterra Aipom Mareep Poliwag Grimer Roggenrola Armaldo Omastar Rhyperior Ralts Surskit Tympole Excadrill Lugia Audino Oshawott Togetic Crawdaunt Torchic Cofagrigus Shroomish Houndoom Mudkip Ponyta Archeops Spinarak Kricketune Electrike Ekans Simisear Raticate Emboar Regirock Karrablast Tangela Aerodactyl Liepard Dusclops Spoink Kecleon Nidoran Natu Unfezant Trapinch Horsea Archen Nidoran Nosepass Snover Rattata Ambipom Murkrow Weezing Gothita Absol Lotad Durant Terrakion Nincada Arceus Spheal Lilligant Tynamo Omanyte Elgyem Mew Watchog Golduck Kabuto Oddish Ho-Oh Heatran Nidoking Golurk Klang Garchomp Porygon-Z Zekrom Magikarp Pawniard Deerling Gurdurr Rhydon Nuzleaf Foongus Shellder Rapidash Haunter Reuniclus Seviper Raikou Ursaring Garbodor Roserade Entei Igglybuff Froslass Spearow Wailord Duskull Ludicolo Onix Xatu Unown Numel Landorus Seismitoad Drifblim Machop Palpitoad Darkrai Illumise Empoleon Nidorino) 329)
```

Never terminates. The longest chain I have found (so far?) is 333 long:

```txt
((Voltorb Breloom Medicham Machop Piplup Palpitoad Deerling Golett Turtwig
  Glameow Wingull Lilligant Timburr Registeel Lotad Duskull Latias Serperior
  Reuniclus Spinarak Krokorok Klinklang Golem Mew Whiscash Huntail Lugia Accelgor
  Rattata Aerodactyl Luvdisc Chingling Grumpig Garchomp Prinplup Petilil Loudred
  Ducklett Tornadus Spearow Whimsicott Tangrowth Hoothoot Tentacruel Lampent
  Togekiss Skiploom Marshtomp Patrat Thundurus Solosis Sneasel Lillipup Poliwhirl
  Lapras Sandslash Honchkrow Wobbuffet Torkoal Latios Slaking Gligar Rampardos
  Shellos Sentret Tropius Sigilyph Herdier Remoraid Dusclops Stoutland Drilbur
  Reshiram Misdreavus Slowking Golduck Klink Koffing Gyarados Shuppet Tauros
  Swellow Wynaut Torchic Castform Maractus Spiritomb Bayleef Furret Tangela
  Altaria Aipom Murkrow Wigglytuff Forretress Spheal Lickitung Gigalith Heracross
  Snorunt Totodile Exeggcute Electrode Ekans Solrock Klang Girafarig Gorebyss
  Swalot Trapinch Ho-Oh Hitmontop Porygon-Z Zapdos Shroomish Houndour Relicanth
  Hoppip Poliwrath Houndoom Mareep Poliwag Golurk Kirlia Axew Wormadam Mandibuzz
  Zweilous Swinub Beldum Manectric Cherrim Moltres Staraptor Rayquaza Azurill
  Lairon Nidoqueen Noctowl Leavanny Yamask Kricketune Eevee Escavalier Rufflet
  Torterra Archen Ninjask Kakuna Absol Lunatone Excadrill Ludicolo Octillery
  Yanma Archeops Spoink Kabuto Onix Xatu Unown Nidoran Ninetales Sawsbuck Kyurem
  Magikarp Politoed Dewott Trubbish Hippopotas Seel Linoone Empoleon Nincada
  Amoonguss Swoobat Teddiursa Audino Oshawott Tepig Gloom Metagross Shelmet
  Tentacool Larvesta Ariados Skorupi Illumise Exploud Durant Tynamo Omastar
  Roggenrola Arbok Kricketot Taillow Weezing Grimer Rhyperior Rapidash Haxorus
  Skuntank Kingdra Alomomola Azelf Ferroseed Drifblim Marowak Kingler Roselia
  Azumarill Lanturn Natu Ursaring Golbat Toxicroak Kecleon Numel Landorus
  Samurott Togepi Infernape Electrike Electivire Electabuzz Zekrom Metang
  Galvantula Arceus Seedot Tirtouga Aron Nosepass Seismitoad Dialga Articuno
  Oddish Horsea Alakazam Meganium Marill Liepard Deoxys Smoochum Mudkip Probopass
  Stunfisk Krookodile Elekid Delibird Dratini Igglybuff Foongus Swampert
  Tyranitar Raikou Uxie Eelektross Slakoth Hariyama Abra Anorith Haunter Raichu
  Umbreon Nidorina Aggron Nuzleaf Floatzel Litwick Kyogre Exeggutor Raticate
  Emboar Regirock Kabutops Sandshrew Watchog Gurdurr Roserade Entei Ivysaur
  Regice Emolga Abomasnow Wailord Diglett Tyrogue Espeon Nidoran Nidoking
  Granbull Ledyba Arcanine Elgyem Mothim Mismagius Sawk Kadabra Ampharos Snubbull
  Larvitar Riolu Unfezant Togetic Charizard Dewgong Gothita Ambipom Machamp Paras
  Simisear Ralts Seaking Gliscor Rotom Milotic Cyndaquil Lileep Pawniard
  Dragonair Regigigas Surskit Tranquill Ledian Nidorino Omanyte Eelektrik
  Karrablast Throh Happiny Yanmega Armaldo) 333)
```

'''File: pokemon-names.rkt'''

```racket
#lang racket
(provide names-646 names-70)
(define names-70
  '(audino bagon baltoy banette bidoof braviary bronzor carracosta charmeleon cresselia croagunk darmanitan deino emboar
    emolga exeggcute gabite girafarig gulpin haxorus heatmor heatran ivysaur jellicent jumpluff kangaskhan kricketune
    landorus ledyba loudred lumineon lunatone machamp magnezone mamoswine nosepass petilil pidgeotto pikachu pinsir
    poliwrath poochyena porygon2 porygonz registeel relicanth remoraid rufflet sableye scolipede scrafty seaking sealeo
    silcoon simisear snivy snorlax spoink starly tirtouga trapinch treecko tyrogue vigoroth vulpix wailord wartortle
    whismur wingull yamask))
; If this were true, then the 2 would be ignored, which seems a bit wrong; so wordigy the "2" -> "two"
(define Porygon2 (if #f 'Porygon2 'Porygon-two))
; note that Porygon2 is used with a comma in the list below
(define names-646
  `(Bulbasaur Ivysaur Venusaur Charmander Charmeleon Charizard Squirtle Wartortle Blastoise Caterpie Metapod Butterfree
    Weedle Kakuna Beedrill Pidgey Pidgeotto Pidgeot Rattata Raticate Spearow Fearow Ekans Arbok Pikachu Raichu Sandshrew
    Sandslash Nidoran Nidorina Nidoqueen Nidoran Nidorino Nidoking Clefairy Clefable Vulpix Ninetales Jigglypuff
    Wigglytuff Zubat Golbat Oddish Gloom Vileplume Paras Parasect Venonat Venomoth Diglett Dugtrio Meowth Persian
    Psyduck Golduck Mankey Primeape Growlithe Arcanine Poliwag Poliwhirl Poliwrath Abra Kadabra Alakazam Machop Machoke
    Machamp Bellsprout Weepinbell Victreebel Tentacool Tentacruel Geodude Graveler Golem Ponyta Rapidash Slowpoke
    Slowbro Magnemite Magneton |Farfetch'd| Doduo Dodrio Seel Dewgong Grimer Muk Shellder Cloyster Gastly Haunter Gengar
    Onix Drowzee Hypno Krabby Kingler Voltorb Electrode Exeggcute Exeggutor Cubone Marowak Hitmonlee Hitmonchan
    Lickitung Koffing Weezing Rhyhorn Rhydon Chansey Tangela Kangaskhan Horsea Seadra Goldeen Seaking Staryu Starmie
    |Mr. Mime| Scyther Jynx Electabuzz Magmar Pinsir Tauros Magikarp Gyarados Lapras Ditto Eevee Vaporeon Jolteon
    Flareon Porygon Omanyte Omastar Kabuto Kabutops Aerodactyl Snorlax Articuno Zapdos Moltres Dratini Dragonair
    Dragonite Mewtwo Mew Chikorita Bayleef Meganium Cyndaquil Quilava Typhlosion Totodile Croconaw Feraligatr Sentret
    Furret Hoothoot Noctowl Ledyba Ledian Spinarak Ariados Crobat Chinchou Lanturn Pichu Cleffa Igglybuff Togepi Togetic
    Natu Xatu Mareep Flaaffy Ampharos Bellossom Marill Azumarill Sudowoodo Politoed Hoppip Skiploom Jumpluff Aipom
    Sunkern Sunflora Yanma Wooper Quagsire Espeon Umbreon Murkrow Slowking Misdreavus Unown Wobbuffet Girafarig Pineco
    Forretress Dunsparce Gligar Steelix Snubbull Granbull Qwilfish Scizor Shuckle Heracross Sneasel Teddiursa Ursaring
    Slugma Magcargo Swinub Piloswine Corsola Remoraid Octillery Delibird Mantine Skarmory Houndour Houndoom Kingdra
    Phanpy Donphan ,Porygon2 Stantler Smeargle Tyrogue Hitmontop Smoochum Elekid Magby Miltank Blissey Raikou Entei
    Suicune Larvitar Pupitar Tyranitar Lugia Ho-Oh Celebi Treecko Grovyle Sceptile Torchic Combusken Blaziken Mudkip
    Marshtomp Swampert Poochyena Mightyena Zigzagoon Linoone Wurmple Silcoon Beautifly Cascoon Dustox Lotad Lombre
    Ludicolo Seedot Nuzleaf Shiftry Taillow Swellow Wingull Pelipper Ralts Kirlia Gardevoir Surskit Masquerain Shroomish
    Breloom Slakoth Vigoroth Slaking Nincada Ninjask Shedinja Whismur Loudred Exploud Makuhita Hariyama Azurill Nosepass
    Skitty Delcatty Sableye Mawile Aron Lairon Aggron Meditite Medicham Electrike Manectric Plusle Minun Volbeat Illumise
    Roselia Gulpin Swalot Carvanha Sharpedo Wailmer Wailord Numel Camerupt Torkoal Spoink Grumpig Spinda Trapinch Vibrava
    Flygon Cacnea Cacturne Swablu Altaria Zangoose Seviper Lunatone Solrock Barboach Whiscash Corphish Crawdaunt Baltoy
    Claydol Lileep Cradily Anorith Armaldo Feebas Milotic Castform Kecleon Shuppet Banette Duskull Dusclops Tropius
    Chimecho Absol Wynaut Snorunt Glalie Spheal Sealeo Walrein Clamperl Huntail Gorebyss Relicanth Luvdisc Bagon Shelgon
    Salamence Beldum Metang Metagross Regirock Regice Registeel Latias Latios Kyogre Groudon Rayquaza Jirachi Deoxys
    Turtwig Grotle Torterra Chimchar Monferno Infernape Piplup Prinplup Empoleon Starly Staravia Staraptor Bidoof Bibarel
    Kricketot Kricketune Shinx Luxio Luxray Budew Roserade Cranidos Rampardos Shieldon Bastiodon Burmy Wormadam Mothim
    Combee Vespiquen Pachirisu Buizel Floatzel Cherubi Cherrim Shellos Gastrodon Ambipom Drifloon Drifblim Buneary
    Lopunny Mismagius Honchkrow Glameow Purugly Chingling Stunky Skuntank Bronzor Bronzong Bonsly |Mime Jr.| Happiny
    Chatot Spiritomb Gible Gabite Garchomp Munchlax Riolu Lucario Hippopotas Hippowdon Skorupi Drapion Croagunk Toxicroak
    Carnivine Finneon Lumineon Mantyke Snover Abomasnow Weavile Magnezone Lickilicky Rhyperior Tangrowth Electivire
    Magmortar Togekiss Yanmega Leafeon Glaceon Gliscor Mamoswine Porygon-Z Gallade Probopass Dusknoir Froslass Rotom Uxie
    Mesprit Azelf Dialga Palkia Heatran Regigigas Giratina Cresselia Phione Manaphy Darkrai Shaymin Arceus Victini Snivy
    Servine Serperior Tepig Pignite Emboar Oshawott Dewott Samurott Patrat Watchog Lillipup Herdier Stoutland Purrloin
    Liepard Pansage Simisage Pansear Simisear Panpour Simipour Munna Musharna Pidove Tranquill Unfezant Blitzle Zebstrika
    Roggenrola Boldore Gigalith Woobat Swoobat Drilbur Excadrill Audino Timburr Gurdurr Conkeldurr Tympole Palpitoad
    Seismitoad Throh Sawk Sewaddle Swadloon Leavanny Venipede Whirlipede Scolipede Cottonee Whimsicott Petilil Lilligant
    Basculin Sandile Krokorok Krookodile Darumaka Darmanitan Maractus Dwebble Crustle Scraggy Scrafty Sigilyph Yamask
    Cofagrigus Tirtouga Carracosta Archen Archeops Trubbish Garbodor Zorua Zoroark Minccino Cinccino Gothita Gothorita
    Gothitelle Solosis Duosion Reuniclus Ducklett Swanna Vanillite Vanillish Vanilluxe Deerling Sawsbuck Emolga
    Karrablast Escavalier Foongus Amoonguss Frillish Jellicent Alomomola Joltik Galvantula Ferroseed Ferrothorn Klink
    Klang Klinklang Tynamo Eelektrik Eelektross Elgyem Beheeyem Litwick Lampent Chandelure Axew Fraxure Haxorus Cubchoo
    Beartic Cryogonal Shelmet Accelgor Stunfisk Mienfoo Mienshao Druddigon Golett Golurk Pawniard Bisharp Bouffalant
    Rufflet Braviary Vullaby Mandibuzz Heatmor Durant Deino Zweilous Hydreigon Larvesta Volcarona Cobalion Terrakion
    Virizion Tornadus Thundurus Reshiram Zekrom Landorus Kyurem))
```



## REXX

### brute force version

(This program is modeled after the ooRexx version, with a bug fix.)


This REXX version allows a limit on the word scan (very useful for testing and debugging), and

also has various speed optimizations.

```rexx
/*REXX program finds the  longest path of word's   last─letter ───► first─letter.       */
@='audino bagon baltoy banette bidoof braviary bronzor carracosta charmeleon cresselia croagunk darmanitan',
  'deino emboar emolga exeggcute gabite girafarig gulpin haxorus heatmor heatran ivysaur jellicent',
  'jumpluff kangaskhan kricketune landorus ledyba loudred lumineon lunatone machamp magnezone mamoswine',
  'nosepass petilil pidgeotto pikachu pinsir poliwrath poochyena porygon2 porygonz registeel relicanth',
  'remoraid rufflet sableye scolipede scrafty seaking sealeo silcoon simisear snivy snorlax spoink',
  'starly tirtouga trapinch treecko tyrogue vigoroth vulpix wailord wartortle whismur wingull yamask'
#= words(@)     @.=;    ig= 0;     !.= 0         /*nullify array and the longest path.  */
parse arg limit .;  if limit\==''  then #=limit  /*allow user to specify a scan limit.  */
call build@                                      /*build a stemmed array from the @ list*/
                do v=#  by -1  for #             /*scrub the @ list for unusable words. */
                parse var @.v  F  2  ''  -1  L   /*obtain first and last letter of word.*/
                if !.1.F>1 | !.9.L>1  then iterate                /*is this a dead word?*/
                say 'ignoring dead word:'   @.v;      ig= ig+1;        @= delword(@, v, 1)
                end   /*v*/                      /*delete dead word from  @ ──┘         */
$$$=                                             /*nullify the possible longest path.   */
if ig\==0  then do;   call build@;   say;   say 'ignoring'   ig   "dead word"s(ig).;   say
                end
MP= 0;  MPL= 0                                   /*the initial   Maximum Path Length.   */
                do j=1  for #                    /*              ─       ─    ─         */
                parse  value  @.1 @.j   with   @.j @.1;          call scan $$$, 2
                parse  value  @.1 @.j   with   @.j @.1
                end   /*j*/
g= words($$$)
say 'Of'    #    "words,"    MP    'path's(MP)    "have the maximum path length of"   g'.'
say;    say 'One example path of that length is: '   word($$$, 1)
            do m=2  to g;      say left('', 36)      word($$$, m);       end   /*m*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
s:    if arg(1)==1  then return arg(3);    return word( arg(2) 's', 1)   /*a pluralizer.*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
build@:     do i=1  for #;     @.i=word(@, i)    /*build a stemmed array from the list. */
            F= left(@.i, 1);   !.1.F= !.1.F + 1  /*F:  1st char; !.1.F=count of 1st char*/
            L=right(@.i, 1);   !.9.L= !.9.L + 1  /*L: last   "   !.9.L=  "    " last  " */
            end   /*i*/;       return
/*──────────────────────────────────────────────────────────────────────────────────────*/
scan: procedure expose @. # !. $$$ MP MPL;    parse arg $$$,!;                  p=! - 1
      parse var  @.p  ''  -1  LC                 /*obtain last character of previous @. */
      if !.1.LC==0  then return                  /*is this a  dead─end  word?           */
                                                 /* [↓]  PARSE obtains first char of @.i*/
         do i=!  to #;  parse var  @.i  p  2     /*scan for the longest word path.      */
         if p==LC  then do                       /*is the  first─character ≡ last─char? */
                        if !==MPL  then MP= MP+1 /*bump the  Maximum Paths  Counter.    */
                                   else if !>MPL  then do; $$$=@.1           /*rebuild. */
                                                             do n=2  for !-2;  $$$=$$$ @.n
                                                             end   /*n*/
                                                           $$$=$$$   @.i     /*add last.*/
                                                           MP=1;   MPL=!     /*new path.*/
                                                       end
                        parse value  @.! @.i   with   @.i @.!;          call scan $$$, !+1
                        parse value  @.! @.i   with   @.i @.!
                        end
         end    /*i*/;             return        /*exhausted this particular scan.      */
```

```txt

Of 70 words, 1248 paths have the maximum path length of 23.

One example path of that length is:  machamp
                                     petilil
                                     landorus
                                     scrafty
                                     yamask
                                     kricketune
                                     emboar
                                     registeel
                                     loudred
                                     darmanitan
                                     nosepass
                                     simisear
                                     relicanth
                                     heatmor
                                     rufflet
                                     trapinch
                                     haxorus
                                     seaking
                                     girafarig
                                     gabite
                                     exeggcute
                                     emolga
                                     audino

```



### optimized version

This optimized version has two major improvements:
::*   removes dead words (words that cannot be used in a path)
::*   stops scanning when a dead-end word is encountered.
With the full list of words being used, there are no dead words   (but there are when a limit is used to shorten the list).

In the   '''scan'''   subroutine, a check is made to see if the word being used is a dead-end word, and if so, the rest of

the recursive scan is aborted and the the next word is scanned.


The optimized version is around   '''25%'''   faster than the brute-force version.

```rexx
/*REXX program finds the  longest path of word's   last─letter ───► first─letter.       */
@='audino bagon baltoy banette bidoof braviary bronzor carracosta charmeleon cresselia croagunk darmanitan',
  'deino emboar emolga exeggcute gabite girafarig gulpin haxorus heatmor heatran ivysaur jellicent',
  'jumpluff kangaskhan kricketune landorus ledyba loudred lumineon lunatone machamp magnezone mamoswine',
  'nosepass petilil pidgeotto pikachu pinsir poliwrath poochyena porygon2 porygonz registeel relicanth',
  'remoraid rufflet sableye scolipede scrafty seaking sealeo silcoon simisear snivy snorlax spoink',
  'starly tirtouga trapinch treecko tyrogue vigoroth vulpix wailord wartortle whismur wingull yamask'
#= words(@);  @.=;   @s=;   ig=0;   og=0;   !.=0 /*nullify array and the longest path.  */
parse arg limit .;  if limit\==''  then #=limit  /*allow user to specify a scan limit.  */
call build@                                      /*build a stemmed array from the @ list*/
                do v=#  by -1  for #             /*scrub the @ list for unusable words. */
                parse var @.v  F  2  ''  -1  L   /*obtain first and last letter of word.*/
                if !.1.F>1 | !.9.L>1  then iterate                /*is this a dead word?*/
                say 'ignoring dead word:'   @.v;       ig=ig+1;         @=delword(@, v, 1)
                end   /*v*/                      /*delete dead word from  @ ──┘         */
#= words(@)                                      /*recalculate the number of words in @.*/
                do v=#  by -1  for #             /*scrub the @ list for stoppable words.*/
                parse var @.v  F  2  ''  -1  L   /*obtain first and last letter of word.*/
                if !.1.L>0  then iterate         /*is this a stop word?                 */
                say 'removing stop word:'  @.v;  og=og+1;  @=delword(@, v, 1);   @s=@s @.v
                end   /*v*/                      /*delete dead word from  @ ──┘         */

if og\==0  then do;   call build@;   say;   say 'ignoring'   og   "stop word"s(og).
                say 'stop words: '   @s;    say
                end
$$$=                                             /*nullify the possible longest path.   */
MP= 0;  MPL= 0                                   /*the initial   Maximum Path Length.   */
                do j=1  for #                    /*              ─       ─    ─         */
                parse  value  @.1 @.j   with   @.j @.1;          call scan $$$, 2
                parse  value  @.1 @.j   with   @.j @.1
                end   /*j*/
g= words($$$)
say 'Of'    #    "words,"    MP    'path's(MP)    "have the maximum path length of"   g'.'
say;    say 'One example path of that length is: '   word($$$, 1)
            do m=2  to g;      say left('', 36)      word($$$, m);       end   /*m*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
s:    if arg(1)==1  then return arg(3);    return word( arg(2) 's', 1)   /*a pluralizer.*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
build@:     do i=1  for #;     @.i= word(@, i)   /*build a stemmed array from the list. */
            F= left(@.i, 1);   !.1.F= !.1.F + 1  /*F:  1st char; !.1.F=count of 1st char*/
            L=right(@.i, 1);   !.9.L= !.9.L + 1  /*L: last   "   !.9.L=  "    " last  " */
            end   /*i*/;       return
/*──────────────────────────────────────────────────────────────────────────────────────*/
scan: procedure expose @. # !. $$$ MP MPL;   parse arg $$$,!;                    p= ! - 1
      parse var  @.p  ''  -1  LC                 /*obtain last character of previous @. */
      if !.1.LC==0  then return                  /*is this a  dead─end  word?           */
                                                 /* [↓]  PARSE obtains first char of @.i*/
         do i=!  to #;  parse var  @.i  p  2     /*scan for the longest word path.      */
         if p==LC  then do                       /*is the  first─character ≡ last─char? */
                        if !==MPL  then MP= MP+1 /*bump the  Maximum Paths  Counter.    */
                                   else if !>MPL  then do; $$$= @.1          /*rebuild. */
                                                             do n=2  for !-2;  $$$=$$$ @.n
                                                             end   /*n*/
                                                           $$$= $$$  @.i     /*add last.*/
                                                           MP=1;   MPL=!     /*new path.*/
                                                       end
                        parse value  @.! @.i   with   @.i @.!;          call scan $$$, !+1
                        parse value  @.! @.i   with   @.i @.!
                        end
         end    /*i*/;             return        /*exhausted this particular scan.      */
```

## Ring


```ring

# Project : Last letter-first letter

ready = []
names = ["audino", "bagon", "baltoy", "banette",
               "bidoof", "braviary", "bronzor", "carracosta", "charmeleon",
               "cresselia", "croagunk", "darmanitan", "deino", "emboar",
               "emolga", "exeggcute", "gabite", "girafarig", "gulpin",
               "haxorus", "heatmor", "heatran", "ivysaur", "jellicent",
               "jumpluff", "kangaskhan", "kricketune", "landorus", "ledyba",
               "loudred", "lumineon", "lunatone", "machamp", "magnezone",
               "mamoswine", "nosepass", "petilil", "pidgeotto", "pikachu",
               "pinsir", "poliwrath", "poochyena", "porygon2", "porygonz",
               "registeel", "relicanth", "remoraid", "rufflet", "sableye",
               "scolipede", "scrafty", "seaking", "sealeo", "silcoon",
               "simisear", "snivy", "snorlax", "spoink", "starly", "tirtouga",
               "trapinch", "treecko", "tyrogue", "vigoroth", "vulpix",
               "wailord", "wartortle", "whismur", "wingull", "yamask"]
strbegin = "gabite"
strdir = "first"
nr = 1
add(ready,strbegin)
see strbegin + nl
while true
          if strdir = "first"
             strc = right(strbegin, 1)
             flag = 1
             nr = nr + 1
             if nr <= len(names)
                if left(names[nr],1) = strc
                   for n = 1 to len(ready)
                         if names[nr] = ready[n]
                            flag = 0
                         ok
                   next
                   if flag = 1
                      add(ready,names[nr])
                      see names[nr] + nl
                      strbegin = names[nr]
                      nr = 0
                      strdir = "last"
                      loop
                   ok
                ok
              ok
          else
             strc = right(strbegin, 1)
             flag = 1
             nr = nr + 1
             if nr <= len(names)
                if left(names[nr],1) = strc
                   for n = 1 to len(ready)
                         if names[nr] = ready[n]
                            flag = 0
                         ok
                   next
                   if flag = 1
                      add(ready,names[nr])
                      see names[nr] + nl
                      strbegin = names[nr]
                      nr = 0
                      strdir = "first"
                      loop
                   ok
                ok
             ok
          ok
end

```

Output:

```txt

gabite
emboar
registeel
landorus
sableye
emolga
audino

```



## Ruby


```ruby
class LastL_FirstL
  def initialize(names)
    @names = names.dup
    @first = names.group_by {|name| name[0]}
    @sequences = []
  end

  def add_name(seq)
    last_letter = seq[-1][-1]
    potentials = @first.include?(last_letter) ? (@first[last_letter] - seq) : []
    if potentials.empty?
      @sequences << seq
    else
      potentials.each {|name| add_name(seq + [name])}
    end
  end

  def search
    @names.each {|name| add_name [name]}
    max = @sequences.max_by {|seq| seq.length}.length
    max_seqs = @sequences.select {|seq| seq.length == max}
    puts "there are #{@sequences.length} possible sequences"
    puts "the longest is #{max} names long"
    puts "there are #{max_seqs.length} such sequences. one is:"
    max_seqs.last.each_with_index {|name, idx| puts "  %2d %s" % [idx+1, name]}
  end
end

names = %w{
  audino bagon baltoy banette bidoof braviary bronzor carracosta charmeleon
  cresselia croagunk darmanitan deino emboar emolga exeggcute gabite
  girafarig gulpin haxorus heatmor heatran ivysaur jellicent jumpluff kangaskhan
  kricketune landorus ledyba loudred lumineon lunatone machamp magnezone mamoswine
  nosepass petilil pidgeotto pikachu pinsir poliwrath poochyena porygon2
  porygonz registeel relicanth remoraid rufflet sableye scolipede scrafty seaking
  sealeo silcoon simisear snivy snorlax spoink starly tirtouga trapinch treecko
  tyrogue vigoroth vulpix wailord wartortle whismur wingull yamask
}

lf = LastL_FirstL.new(names)
lf.search
```

outputs

```txt
there are 2076396 possible sequences
the longest is 23 names long
there are 1248 such sequences. one is:
   1 machamp
   2 pinsir
   3 rufflet
   4 trapinch
   5 heatmor
   6 remoraid
   7 darmanitan
   8 nosepass
   9 starly
  10 yamask
  11 kricketune
  12 exeggcute
  13 emboar
  14 relicanth
  15 haxorus
  16 simisear
  17 registeel
  18 landorus
  19 seaking
  20 girafarig
  21 gabite
  22 emolga
  23 audino
```


## Scala


### Naive


```Scala
object LastLetterFirstLetterNaive extends App {
  def solve(names: Set[String]) = {
    def extend(solutions: List[List[String]]): List[List[String]] = {
      val more = solutions.flatMap{solution =>
        val lastLetter = solution.head takeRight 1
        (names -- solution).filter(_.take(1) equalsIgnoreCase lastLetter).map(_ :: solution)
      }
      if (more.isEmpty) solutions else extend(more)
    }

    extend(names.toList.map(List(_))).map(_.reverse)
  }

  val names70 = Set("audino", "bagon", "baltoy", "banette", "bidoof", "braviary", "bronzor", "carracosta", "charmeleon", "cresselia", "croagunk", "darmanitan", "deino", "emboar", "emolga", "exeggcute", "gabite", "girafarig", "gulpin", "haxorus", "heatmor", "heatran", "ivysaur", "jellicent", "jumpluff", "kangaskhan", "kricketune", "landorus", "ledyba", "loudred", "lumineon", "lunatone", "machamp", "magnezone", "mamoswine", "nosepass", "petilil", "pidgeotto", "pikachu", "pinsir", "poliwrath", "poochyena", "porygon2", "porygonz", "registeel", "relicanth", "remoraid", "rufflet", "sableye", "scolipede", "scrafty", "seaking", "sealeo", "silcoon", "simisear", "snivy", "snorlax", "spoink", "starly", "tirtouga", "trapinch", "treecko", "tyrogue", "vigoroth", "vulpix", "wailord", "wartortle", "whismur", "wingull", "yamask")

  val solutions = solve(names70)
  println(s"Maximum path length: ${solutions.head.length}")
  println(s"Paths of that length: ${solutions.size}")
  println("Example path of that length:")
  println(solutions.head.sliding(7,7).map(_.mkString(" ")).map("  "+_).mkString("\n"))
}
```

Output:

```txt
Maximum path length: 23
Paths of that length: 1248
Example path of that length:
  machamp petilil loudred darmanitan nosepass snivy yamask
  kricketune emboar rufflet trapinch heatmor relicanth haxorus
  simisear registeel landorus seaking girafarig gabite exeggcute
  emolga audino
```

===With Lookup Table (Faster)===

```Scala
object LastLetterFirstLetterLookup extends App {
  def solve(names: Set[String]) = {
    val transitions = {
      val startsWith = names.groupBy(_.take(1).toLowerCase).withDefaultValue(Set.empty)
      names.map{name => name -> startsWith(name.takeRight(1).toLowerCase)}.toMap
    }

    def extend(solutions: List[List[String]]): List[List[String]] =
      solutions.flatMap{solution =>
        (transitions(solution.head) -- solution).map(_ :: solution)
      } match {
        case Nil => solutions
        case more => extend(more)
      }

    extend(names.toList.map(List(_))).map(_.reverse)
  }

  val names70 = Set("audino", "bagon", "baltoy", "banette", "bidoof", "braviary", "bronzor", "carracosta", "charmeleon", "cresselia", "croagunk", "darmanitan", "deino", "emboar", "emolga", "exeggcute", "gabite", "girafarig", "gulpin", "haxorus", "heatmor", "heatran", "ivysaur", "jellicent", "jumpluff", "kangaskhan", "kricketune", "landorus", "ledyba", "loudred", "lumineon", "lunatone", "machamp", "magnezone", "mamoswine", "nosepass", "petilil", "pidgeotto", "pikachu", "pinsir", "poliwrath", "poochyena", "porygon2", "porygonz", "registeel", "relicanth", "remoraid", "rufflet", "sableye", "scolipede", "scrafty", "seaking", "sealeo", "silcoon", "simisear", "snivy", "snorlax", "spoink", "starly", "tirtouga", "trapinch", "treecko", "tyrogue", "vigoroth", "vulpix", "wailord", "wartortle", "whismur", "wingull", "yamask")

  val solutions = solve(names70)
  println(s"Maximum path length: ${solutions.head.length}")
  println(s"Paths of that length: ${solutions.size}")
  println("Example path of that length:")
  println(solutions.head.sliding(7,7).map(_.mkString(" ")).map("  "+_).mkString("\n"))
}
```

===Parallelised With Lookup Table (Faster)===

```Scala
object LastLetterFirstLetterLookupParallel extends App {
  def solve(names: Set[String]) = {
    val transitions = {
      val startsWith = names.groupBy(_.take(1).toLowerCase).withDefaultValue(Set.empty)
      names.map{name => name -> startsWith(name.takeRight(1).toLowerCase)}.toMap
    }

    import scala.collection.parallel.immutable.ParSeq
    def extend(solutions: ParSeq[List[String]]): ParSeq[List[String]] =
      solutions.flatMap{solution =>
        (transitions(solution.head) -- solution).map(_ :: solution)
      } match {
        case seq if seq.isEmpty => solutions
        case more => extend(more)
      }

    extend(names.toList.map(List(_)).par).map(_.reverse)
  }

  val names70 = Set("audino", "bagon", "baltoy", "banette", "bidoof", "braviary", "bronzor", "carracosta", "charmeleon", "cresselia", "croagunk", "darmanitan", "deino", "emboar", "emolga", "exeggcute", "gabite", "girafarig", "gulpin", "haxorus", "heatmor", "heatran", "ivysaur", "jellicent", "jumpluff", "kangaskhan", "kricketune", "landorus", "ledyba", "loudred", "lumineon", "lunatone", "machamp", "magnezone", "mamoswine", "nosepass", "petilil", "pidgeotto", "pikachu", "pinsir", "poliwrath", "poochyena", "porygon2", "porygonz", "registeel", "relicanth", "remoraid", "rufflet", "sableye", "scolipede", "scrafty", "seaking", "sealeo", "silcoon", "simisear", "snivy", "snorlax", "spoink", "starly", "tirtouga", "trapinch", "treecko", "tyrogue", "vigoroth", "vulpix", "wailord", "wartortle", "whismur", "wingull", "yamask")

  val solutions = solve(names70)
  println(s"Maximum path length: ${solutions.head.length}")
  println(s"Paths of that length: ${solutions.size}")
  println("Example path of that length:")
  println(solutions.head.sliding(7,7).map(_.mkString(" ")).map("  "+_).mkString("\n"))
}
```



## Seed7


```seed7
$ include "seed7_05.s7i";

var integer: maxPathLength is 0;
var integer: maxPathLengthCount is 0;
var string: maxPathExample is "";

var array string: names is [] (
    "audino", "bagon", "baltoy", "banette", "bidoof", "braviary", "bronzor",
    "carracosta", "charmeleon", "cresselia", "croagunk", "darmanitan", "deino",
    "emboar", "emolga", "exeggcute", "gabite", "girafarig", "gulpin", "haxorus",
    "heatmor", "heatran", "ivysaur", "jellicent", "jumpluff", "kangaskhan",
    "kricketune", "landorus", "ledyba", "loudred", "lumineon", "lunatone",
    "machamp", "magnezone", "mamoswine", "nosepass", "petilil", "pidgeotto",
    "pikachu", "pinsir", "poliwrath", "poochyena", "porygon2", "porygonz",
    "registeel", "relicanth", "remoraid", "rufflet", "sableye", "scolipede",
    "scrafty", "seaking", "sealeo", "silcoon", "simisear", "snivy", "snorlax",
    "spoink", "starly", "tirtouga", "trapinch", "treecko", "tyrogue", "vigoroth",
    "vulpix", "wailord", "wartortle", "whismur", "wingull", "yamask");

const proc: recursive (in array string: part, in integer: offset) is func
  local
    var integer: index is 0;
    var char: lastChar is ' ';
    var string: tmp is "";
  begin
    if pred(offset) > maxPathLength then
      maxPathLength := pred(offset);
      maxPathLengthCount := 1;
    elsif pred(offset) = maxPathLength then
      incr(maxPathLengthCount);
      maxPathExample := "";
      for index range 1 to pred(offset) do
        if pred(index) rem 5 = 0 then
          maxPathExample &:= "\n  ";
        else
          maxPathExample &:= " ";
        end if;
        maxPathExample &:= part[index];
      end for;
    end if;
    lastChar := part[pred(offset)][length(part[pred(offset)])];
    for index range offset to length(part) do
      if part[index][1] = lastChar then
        tmp := names[offset];
        names[offset] := names[index];
        names[index] := tmp;
        recursive(names, succ(offset));
        names[index] := names[offset];
        names[offset] := tmp;
      end if;
    end for;
  end func;

const proc: main is func
  local
    var integer: index is 0;
    var string: tmp is "";
  begin
    for index range 1 to length(names) do
      tmp := names[1];
      names[1] := names[index];
      names[index] := tmp;
      recursive(names, 2);
      names[index] := names[1];
      names[1] := tmp;
    end for;
    writeln("maximum path length:  " <& maxPathLength lpad 4);
    writeln("paths of that length: " <& maxPathLengthCount lpad 4);
    writeln("example path of that length:"  <& maxPathExample);
  end func;
```


Output:

```txt

maximum path length:    23
paths of that length: 1248
example path of that length:
  machamp pinsir rufflet trapinch heatmor
  remoraid darmanitan nosepass starly yamask
  kricketune exeggcute emboar relicanth haxorus
  simisear registeel landorus seaking girafarig
  gabite emolga audino

```



## Sidef

```ruby
class LLFL(Array words) {

    has f = Hash()

    method init {
        words.each {|w|
            var m = w.match(/^(.).*(.)$/)
            f{m[0]}{w} = m[1]
        }
    }

    method longest_chain {

        var stack   = []
        var longest = []

        func poke(c) {
            var h = f{c}

            h.each_k { |word|
                var v = h.delete(word)
                stack.push(word)
                longest[] = stack[] if (stack.len > longest.len)
                __FUNC__(v) if f.exists(v)
                stack.pop
                h{word} = v
            }
        }

        f.each_k { poke(_) }
        return longest
    }
}

var words = %w(
  audino bagon baltoy banette bidoof braviary bronzor carracosta charmeleon
  cresselia croagunk darmanitan deino emboar emolga exeggcute gabite
  girafarig gulpin haxorus heatmor heatran ivysaur jellicent jumpluff kangaskhan
  kricketune landorus ledyba loudred lumineon lunatone machamp magnezone mamoswine
  nosepass petilil pidgeotto pikachu pinsir poliwrath poochyena porygon2
  porygonz registeel relicanth remoraid rufflet sableye scolipede scrafty seaking
  sealeo silcoon simisear snivy snorlax spoink starly tirtouga trapinch treecko
  tyrogue vigoroth vulpix wailord wartortle whismur wingull yamask
)

var obj = LLFL(words)
var longest = obj.longest_chain()

say "#{longest.len}: #{longest.join(' ')}"
```

```txt

23: machamp petilil landorus scrafty yamask kricketune exeggcute emboar registeel loudred darmanitan nosepass simisear rufflet trapinch heatmor relicanth haxorus seaking girafarig gabite emolga audino

```



## Tcl


```tcl
proc search {path arcs} {
    set solutions {}
    set c [string index [lindex $path end] end]
    set i -1
    foreach arc $arcs {
	incr i
	if {[string index $arc 0] ne $c} continue
	set soln [search [concat $path [list $arc]] [lreplace $arcs $i $i]]
	lappend solutions [list [llength $soln] $soln]
    }
    if {[llength $solutions]} {
	return [lindex [lsort -integer -decreasing -index 0 $solutions] 0 1]
    } else {
	return $path
    }
}
proc firstlast names {
    set solutions {}
    set i -1
    foreach initial $names {
	incr i
	set soln [search [list $initial] [lreplace $names $i $i]]
	lappend solutions [list [llength $soln] $soln]
    }
    return [lindex [lsort -integer -decreasing -index 0 $solutions] 0 1]
}

set names {
    audino bagon baltoy banette bidoof braviary bronzor carracosta charmeleon
    cresselia croagunk darmanitan deino emboar emolga exeggcute gabite
    girafarig gulpin haxorus heatmor heatran ivysaur jellicent jumpluff
    kangaskhan kricketune landorus ledyba loudred lumineon lunatone machamp
    magnezone mamoswine nosepass petilil pidgeotto pikachu pinsir poliwrath
    poochyena porygon2 porygonz registeel relicanth remoraid rufflet sableye
    scolipede scrafty seaking sealeo silcoon simisear snivy snorlax spoink
    starly tirtouga trapinch treecko tyrogue vigoroth vulpix wailord wartortle
    whismur wingull yamask
}
set path [firstlast $names]
puts "Path (length: [llength $path]): $path"
```

Output:

```txt

Path (length 23): machamp petilil landorus scrafty yamask kricketune emboar registeel loudred darmanitan nosepass simisear relicanth heatmor rufflet trapinch haxorus seaking girafarig gabite exeggcute emolga audino

```



## Ursala



```Ursala
#import std

mon =

~&*~ sep`  mat`  -[
   audino bagon baltoy banette bidoof braviary bronzor carracosta charmeleon
   cresselia croagunk darmanitan deino emboar emolga exeggcute gabite
   girafarig gulpin haxorus heatmor heatran ivysaur jellicent jumpluff kangaskhan
   kricketune landorus ledyba loudred lumineon lunatone machamp magnezone mamoswine
   nosepass petilil pidgeotto pikachu pinsir poliwrath poochyena porygon2
   porygonz registeel relicanth remoraid rufflet sableye scolipede scrafty seaking
   sealeo silcoon simisear snivy snorlax spoink starly tirtouga trapinch treecko
   tyrogue vigoroth vulpix wailord wartortle whismur wingull yamask]-

poke = @iiDrzhK16rlXOASK24PiX ~&llrHiFPrYX^=rxS^|\~&iNCS *=+ ~&rlwNrlCQ^*D/~&+ @h

#show+

example = ~&h poke mon
```
output:

```txt
machamp
petilil
landorus
scrafty
yamask
kricketune
emboar
registeel
loudred
darmanitan
nosepass
simisear
relicanth
heatmor
rufflet
trapinch
haxorus
seaking
girafarig
gabite
exeggcute
emolga
audino

```



## VBScript

```vb
' Last letter-first letter - VBScript - 11/03/2019
    names = array( _
        "audino", "bagon", "baltoy", "banette", _
        "bidoof", "braviary", "bronzor", "carracosta", "charmeleon", _
        "cresselia", "croagunk", "darmanitan", "deino", "emboar", _
        "emolga", "exeggcute", "gabite", "girafarig", "gulpin", _
        "haxorus", "heatmor", "heatran", "ivysaur", "jellicent", _
        "jumpluff", "kangaskhan", "kricketune", "landorus", "ledyba", _
        "loudred", "lumineon", "lunatone", "machamp", "magnezone", _
        "mamoswine", "nosepass", "petilil", "pidgeotto", "pikachu", _
        "pinsir", "poliwrath", "poochyena", "porygon2", "porygonz", _
        "registeel", "relicanth", "remoraid", "rufflet", "sableye", _
        "scolipede", "scrafty", "seaking", "sealeo", "silcoon", _
        "simisear", "snivy", "snorlax", "spoink", "starly", "tirtouga", _
        "trapinch", "treecko", "tyrogue", "vigoroth", "vulpix", _
        "wailord", "wartortle", "whismur", "wingull", "yamask")

    maxPathLength = 0
    maxPathLengthCount = 0
    maxPathExample = ""
    t1=timer

    For i = 0 To ubound(names)
        'swap names(0), names(i)
        temp=names(0): names(0)=names(i): names(i)=temp
        Call lastfirst(names, 1)
        'swap names(0), names(i)
        temp=names(0): names(0)=names(i): names(i)=temp
    Next 'i
    buf = buf & "Maximum length = " & maxPathLength & vbCrLf
    buf = buf & "Number of solutions with that length = " & maxPathLengthCount & vbCrLf
    buf = buf & "One such solution: "  & vbCrLf & maxPathExample & vbCrLf
    t2=timer
    MsgBox buf,,"Last letter-first letter - " & Int(t2-t1) & " sec"

Sub lastfirst(names, offset)
    dim i, l
    If offset > maxPathLength Then
        maxPathLength = offset
        maxPathLengthCount = 1
    ElseIf offset = maxPathLength Then
        maxPathLengthCount = maxPathLengthCount + 1
        maxPathExample = ""
        For i = 0 To offset-1
            maxPathExample = maxPathExample & names(i) & vbCrLf
        Next 'i
    End If
    l = Right(names(offset - 1),1)
    For i = offset To ubound(names)
        If Left(names(i),1) = l Then
            'swap names(i), names(offset)
            temp=names(offset): names(offset)=names(i): names(i)=temp
            Call lastfirst(names, offset+1)
            'swap names(i), names(offset)
            temp=names(offset): names(offset)=names(i): names(i)=temp
        End If
    Next 'i
End Sub 'lastfirst
```

```txt

Maximum length = 23
Number of solutions with that length = 1248
One such solution:
machamp
pinsir
rufflet
trapinch
heatmor
remoraid
darmanitan
nosepass
starly
yamask
kricketune
exeggcute
emboar
relicanth
haxorus
simisear
registeel
landorus
seaking
girafarig
gabite
emolga
audino

```

Duration : 69 sec


## Yabasic

```Yabasic
all$ = "audino bagon baltoy banette bidoof braviary bronzor carracosta charmeleon "
all$ = all$ + "cresselia croagunk darmanitan deino emboar emolga exeggcute gabite "
all$ = all$ + "girafarig gulpin haxorus heatmor heatran ivysaur jellicent jumpluff kangaskhan "
all$ = all$ + "kricketune landorus ledyba loudred lumineon lunatone machamp magnezone mamoswine "
all$ = all$ + "nosepass petilil pidgeotto pikachu pinsir poliwrath poochyena porygon2 "
all$ = all$ + "porygonz registeel relicanth remoraid rufflet sableye scolipede scrafty seaking "
all$ = all$ + "sealeo silcoon simisear snivy snorlax spoink starly tirtouga trapinch treecko "
all$ = all$ + "tyrogue vigoroth vulpix wailord wartortle whismur wingull yamask"

dim word$(1)

lnames = token(all$, word$())

dim first(256), snext(lnames)


for i = 1 to lnames
    ch = asc(left$(word$(i), 1))
    if first(ch)=0 then
        first(ch) = i
    end if
    for j=i+1 to lnames
        if asc(left$(word$(j), 1))=ch then
            snext(i) = j
            break
        end if
    next
next

dim taken(lnames), best(lnames)

sub try(ch, last, n)
    local nex, i

    nex = first(ch)
    while(nex <> 0)
        if taken(nex)=0 then
            taken(last) = nex
            taken(nex) = -1
            try(asc(right$(word$(nex), 1)),nex,n+1)
            taken(last) = -1
            taken(nex) = 0
        end if
        nex = snext(nex)
    wend
    if n>maxn then
        bstart = tstart
        for i = 1 to lnames
            best(i) = taken(i)
        next
        maxn = n
        count = 1
    elsif n=maxn then
        count = count + 1
    end if
end sub

for i=1 to lnames
    tstart = i
    taken(i) = -1
    try(asc(right$(word$(i), 1)),i,1)
    taken(i) = 0
next

print "Runtime: ", peek("millisrunning")/1000, " seconds. Max length: ", maxn, ", found ", count, " of such, one of which is:\n"
do
    print word$(bstart), " ";
    bstart = best(bstart)
    if bstart = -1 break
loop
```



## zkl

This solution creates a hash table to all the possible word/following words to minimize search times.

No speed records were approached but 25sec seems fine for a one off walk the entire tree.


```zkl
pokemon:=("audino bagon baltoy banette bidoof braviary "
   "bronzor carracosta charmeleon cresselia croagunk darmanitan deino "
   ...
   "whismur wingull yamask").split();

tree:=pokemon.pump(Dictionary(),'wrap(name){ //--> Hash("aB":("Bc","Bd",,,) )
   T( name, pokemon.filter('wrap(nm){ name[-1]==nm[0] }) )
});

fcn maxPath([(a,_)]ab,[(c,_)]cd){ if(a>c) ab else cd }
fcn walk(name,pathLen,path,tree){  //-->longest path for name
   names:=tree.find(name,T); tree[name]=T; // nuke cycle
   np:=names.reduce('wrap(np,name){
      maxPath(np,walk(name,pathLen+1,String(path," ",name),tree));
   },T(0,""));
   tree[name]=names;
   if(np[0]>pathLen) return(np);
   return(pathLen,path);
}

pokemon.reduce('wrap(np,name){ maxPath(walk(name,1,name,tree),np) },T(0,""))
.println();
```

```txt

L(23,"machamp pinsir rufflet trapinch heatmor remoraid darmanitan nosepass
   starly yamask kricketune exeggcute emboar relicanth haxorus simisear
   registeel landorus seaking girafarig gabite emolga audino")

```

