+++
title = "Permutations"
description = ""
date = 2019-10-12T09:49:18Z
aliases = []
[extra]
id = 8379
[taxonomies]
categories = ["task", "Discrete math"]
tags = []
languages = [
  "360_assembly",
  "abap",
  "ada",
  "aime",
  "algol_68",
  "applescript",
  "arturo",
  "autohotkey",
  "batch_file",
  "bbc_basic",
  "bracmat",
  "c",
  "clojure",
  "coffeescript",
  "common_lisp",
  "cpp",
  "crystal",
  "csharp",
  "curry",
  "d",
  "delphi",
  "eiffel",
  "elixir",
  "erlang",
  "euphoria",
  "factor",
  "fortran",
  "freebasic",
  "gap",
  "generic_version",
  "glee",
  "go",
  "groovy",
  "haskell",
  "j",
  "java",
  "javascript",
  "jq",
  "julia",
  "k",
  "kotlin",
  "langur",
  "lfe",
  "liberty_basic",
  "logtalk",
  "lua",
  "m2000_interpreter",
  "maple",
  "mathematica",
  "maxima",
  "mercury",
  "microsoft_small_basic",
  "netrexx",
  "nim",
  "ocaml",
  "openedge_progress",
  "pari_gp",
  "pascal",
  "perl",
  "perl_6",
  "permdatalist_permutation_void",
  "phix",
  "picolisp",
  "powerbasic",
  "powershell",
  "prolog",
  "purebasic",
  "python",
  "qi",
  "r",
  "racket",
  "related_tasks",
  "rexx",
  "ring",
  "ruby",
  "run_basic",
  "rust",
  "sas",
  "scala",
  "scheme",
  "seed7",
  "shen",
  "sidef",
  "simple_version",
  "smalltalk",
  "stata",
  "swift",
  "tailspin",
  "tcl",
  "ursala",
  "vba",
  "xpl0",
  "zkl",
]
+++

## Task

Write a program that generates all   [[wp:Permutation|permutations]]   of   '''n'''   different objects.   (Practically numerals!)


## Related tasks

*   [[Find the missing permutation]]
*   [[Permutations/Derangements]]


## 360 Assembly

```360asm
*        Permutations              26/10/2015
PERMUTE  CSECT
         USING  PERMUTE,R15        set base register
         LA     R9,TMP-A           n=hbound(a)
         SR     R10,R10            nn=0
LOOP     LA     R10,1(R10)         nn=nn+1
         LA     R11,PG             pgi=@pg
         LA     R6,1               i=1
LOOPI1   CR     R6,R9              do i=1 to n
         BH     ELOOPI1
         LA     R2,A-1(R6)         @a(i)
         MVC    0(1,R11),0(R2)     output a(i)
         LA     R11,1(R11)         pgi=pgi+1
         LA     R6,1(R6)           i=i+1
         B      LOOPI1
ELOOPI1  XPRNT  PG,80
         LR     R6,R9              i=n
LOOPUIM  BCTR   R6,0               i=i-1
         LTR    R6,R6              until i=0
         BE     ELOOPUIM
         LA     R2,A-1(R6)         @a(i)
         LA     R3,A(R6)           @a(i+1)
         CLC    0(1,R2),0(R3)      or until a(i)<a(i+1)
         BNL    LOOPUIM
ELOOPUIM LR     R7,R6              j=i
         LA     R7,1(R7)           j=i+1
         LR     R8,R9              k=n
LOOPWJ   CR     R7,R8              do while j<k
         BNL    ELOOPWJ
         LA     R2,A-1(R7)         r2=@a(j)
         LA     R3,A-1(R8)         r3=@a(k)
         MVC    TMP,0(R2)          tmp=a(j)
         MVC    0(1,R2),0(R3)      a(j)=a(k)
         MVC    0(1,R3),TMP        a(k)=tmp
         LA     R7,1(R7)           j=j+1
         BCTR   R8,0               k=k-1
         B      LOOPWJ
ELOOPWJ  LTR    R6,R6              if i>0
         BNP    ILE0
         LR     R7,R6              j=i
         LA     R7,1(R7)           j=i+1
LOOPWA   LA     R2,A-1(R7)         @a(j)
         LA     R3,A-1(R6)         @a(i)
         CLC    0(1,R2),0(R3)      do while a(j)<a(i)
         BNL    AJGEAI
         LA     R7,1(R7)           j=j+1
         B      LOOPWA
AJGEAI   LA     R2,A-1(R7)         r2=@a(j)
         LA     R3,A-1(R6)         r3=@a(i)
         MVC    TMP,0(R2)          tmp=a(j)
         MVC    0(1,R2),0(R3)      a(j)=a(i)
         MVC    0(1,R3),TMP        a(i)=tmp
ILE0     LTR    R6,R6              until i<>0
         BNE    LOOP
         XR     R15,R15            set return code
         BR     R14                return to caller
A        DC     C'ABCD'            <== input
TMP      DS     C                  temp for swap
PG       DC     CL80' '            buffer
         YREGS
         END    PERMUTE
```

<pre style="height:40ex;overflow:scroll">
ABCD
ABDC
ACBD
ACDB
ADBC
ADCB
BACD
BADC
BCAD
BCDA
BDAC
BDCA
CABD
CADB
CBAD
CBDA
CDAB
CDBA
DABC
DACB
DBAC
DBCA
DCAB
DCBA

```



## ABAP


```ABAP
data: lv_flag type c,
      lv_number type i,
      lt_numbers type table of i.

append 1 to lt_numbers.
append 2 to lt_numbers.
append 3 to lt_numbers.

do.
  perform permute using lt_numbers changing lv_flag.
  if lv_flag = 'X'.
    exit.
  endif.
  loop at lt_numbers into lv_number.
    write (1) lv_number no-gap left-justified.
    if sy-tabix <> '3'.
      write ', '.
    endif.
  endloop.
  skip.
enddo.

" Permutation function - this is used to permute:
" Can be used for an unbounded size set.
form permute using iv_set like lt_numbers
             changing ev_last type c.
  data: lv_len     type i,
        lv_first   type i,
        lv_third   type i,
        lv_count   type i,
        lv_temp    type i,
        lv_temp_2  type i,
        lv_second  type i,
        lv_changed type c,
        lv_perm    type i.
  describe table iv_set lines lv_len.

  lv_perm = lv_len - 1.
  lv_changed = ' '.
  " Loop backwards through the table, attempting to find elements which
  " can be permuted. If we find one, break out of the table and set the
  " flag indicating a switch.
  do.
    if lv_perm <= 0.
      exit.
    endif.
    " Read the elements.
    read table iv_set index lv_perm into lv_first.
    add 1 to lv_perm.
    read table iv_set index lv_perm into lv_second.
    subtract 1 from lv_perm.
    if lv_first < lv_second.
      lv_changed = 'X'.
      exit.
    endif.
    subtract 1 from lv_perm.
  enddo.

  " Last permutation.
  if lv_changed <> 'X'.
    ev_last = 'X'.
    exit.
  endif.

  " Swap tail decresing to get a tail increasing.
  lv_count = lv_perm + 1.
  do.
    lv_first = lv_len + lv_perm - lv_count + 1.
    if lv_count >= lv_first.
      exit.
    endif.

    read table iv_set index lv_count into lv_temp.
    read table iv_set index lv_first into lv_temp_2.
    modify iv_set index lv_count from lv_temp_2.
    modify iv_set index lv_first from lv_temp.
    add 1 to lv_count.
  enddo.

  lv_count = lv_len - 1.
  do.
    if lv_count <= lv_perm.
      exit.
    endif.

    read table iv_set index lv_count into lv_first.
    read table iv_set index lv_perm into lv_second.
    read table iv_set index lv_len into lv_third.
    if ( lv_first < lv_third ) and ( lv_first > lv_second ).
      lv_len = lv_count.
    endif.

    subtract 1 from lv_count.
  enddo.

  read table iv_set index lv_perm into lv_temp.
  read table iv_set index lv_len into lv_temp_2.
  modify iv_set index lv_perm from lv_temp_2.
  modify iv_set index lv_len from lv_temp.
endform.
```

```txt

1,  3,  2

2,  1,  3

2,  3,  1

3,  1,  2

3,  2,  1

```



## Ada


We split the task into two parts: The first part is to represent permutations, to initialize them and to go from one permutation to another one, until the last one has been reached. This can be used elsewhere, e.g., for the Topswaps [[http://rosettacode.org/wiki/Topswops]] task. The second part is to read the N from the command line, and to actually print all permutations over 1 .. N.

===The generic package Generic_Perm===
When given N, this package defines the Element and Permutation types and exports procedures to set a permutation P to the first one, and to change P into the next one:

```ada
generic
   N: positive;
package Generic_Perm is
   subtype Element is Positive range 1 .. N;
   type Permutation is array(Element) of Element;

   procedure Set_To_First(P: out Permutation; Is_Last: out Boolean);
   procedure Go_To_Next(P: in out Permutation; Is_Last: out Boolean);
end Generic_Perm;
```


Here is the implementation of the package:

```ada
package body Generic_Perm is


   procedure Set_To_First(P: out Permutation; Is_Last: out Boolean) is
   begin
      for I in P'Range loop
	 P (I) := I;
      end loop;
      Is_Last := P'Length = 1;
      -- if P has a single element, the fist permutation is the last one
   end Set_To_First;

   procedure Go_To_Next(P: in out Permutation; Is_Last: out Boolean) is

      procedure Swap (A, B : in out Integer) is
         C : Integer := A;
      begin
         A := B;
         B := C;
      end Swap;

      I, J, K : Element;
   begin
      -- find longest tail decreasing sequence
      -- after the loop, this sequence is I+1 .. n,
      -- and the ith element will be exchanged later
      -- with some element of the tail
      Is_Last := True;
      I := N - 1;
      loop
	 if P (I) < P (I+1)
	 then
	    Is_Last := False;
	    exit;
	 end if;

	 -- next instruction will raise an exception if I = 1, so
	 -- exit now (this is the last permutation)
	 exit when I = 1;
	 I := I - 1;
      end loop;

      -- if all the elements of the permutation are in
      -- decreasing order, this is the last one
      if Is_Last then
	 return;
      end if;

      -- sort the tail, i.e. reverse it, since it is in decreasing order
      J := I + 1;
      K := N;
      while J < K loop
	 Swap (P (J), P (K));
	 J := J + 1;
	 K := K - 1;
      end loop;

      -- find lowest element in the tail greater than the ith element
      J := N;
      while P (J) > P (I) loop
	 J := J - 1;
      end loop;
      J := J + 1;

      -- exchange them
      -- this will give the next permutation in lexicographic order,
      -- since every element from ith to the last is minimum
      Swap (P (I), P (J));
   end Go_To_Next;

end Generic_Perm;
```


===The procedure Print_Perms===

```ada
with Ada.Text_IO, Ada.Command_Line, Generic_Perm;

procedure Print_Perms is
   package CML renames Ada.Command_Line;
   package TIO renames Ada.Text_IO;
begin
   declare
      package Perms is new Generic_Perm(Positive'Value(CML.Argument(1)));
      P : Perms.Permutation;
      Done : Boolean := False;

      procedure Print(P: Perms.Permutation) is
      begin
         for I in P'Range loop
            TIO.Put (Perms.Element'Image (P (I)));
         end loop;
         TIO.New_Line;
      end Print;
   begin
      Perms.Set_To_First(P, Done);
      loop
         Print(P);
         exit when Done;
         Perms.Go_To_Next(P, Done);
      end loop;
   end;
exception
   when Constraint_Error
     => TIO.Put_Line ("*** Error: enter one numerical argument n with n >= 1");
end Print_Perms;
```


```txt
>./print_perms 3
 1 2 3
 1 3 2
 2 1 3
 2 3 1
 3 1 2
 3 2 1
 3 2 1
```



## Aime


```aime
void
f1(record r, ...)
{
    if (~r) {
        for (text s in r) {
            r.delete(s);
            rcall(f1, -2, 0, -1, s);
            r[s] = 0;
        }
    } else {
        ocall(o_, -2, 1, -1, " ", ",");
        o_newline();
    }
}

main(...)
{
    record r;

    ocall(r_put, -2, 1, -1, r, 0);
    f1(r);

    0;
}
```

```txt
aime permutations -a Aaa Bb C
 Aaa, Bb, C,
 Aaa, C, Bb,
 Bb, Aaa, C,
 Bb, C, Aaa,
 C, Aaa, Bb,
 C, Bb, Aaa,
```



## ALGOL 68

'''File: prelude_permutations.a68'''
```algol68
# -*- coding: utf-8 -*- #

COMMENT REQUIRED BY "prelude_permutations.a68"
  MODE PERMDATA = ~;
PROVIDES:
# PERMDATA*=~* #
# perm*=~ list* #
END COMMENT

MODE PERMDATALIST = REF[]PERMDATA;
MODE PERMDATALISTYIELD = PROC(PERMDATALIST)VOID;

# Generate permutations of the input data list of data list #
PROC perm gen permutations = (PERMDATALIST data list, PERMDATALISTYIELD yield)VOID: (
# Warning: this routine does not correctly handle duplicate elements #
  IF LWB data list = UPB data list THEN
    yield(data list)
  ELSE
    FOR elem FROM LWB data list TO UPB data list DO
      PERMDATA first = data list[elem];
      data list[LWB data list+1:elem] := data list[:elem-1];
      data list[LWB data list] := first;
    # FOR PERMDATALIST next data list IN # perm gen permutations(data list[LWB data list+1:] # ) DO #,
    ##   (PERMDATALIST next)VOID:(
        yield(data list)
    # OD #));
      data list[:elem-1] := data list[LWB data list+1:elem];
      data list[elem] := first
    OD
  FI
);

SKIP
```
'''File: test_permutations.a68'''
```algol68
#!/usr/bin/a68g --script #
# -*- coding: utf-8 -*- #

CO REQUIRED BY "prelude_permutations.a68" CO
  MODE PERMDATA = INT;
#PROVIDES:#
# PERM*=INT* #
# perm *=int list *#
PR READ "prelude_permutations.a68" PR;

main:(
  FLEX[0]PERMDATA test case := (1, 22, 333, 44444);

  INT upb data list = UPB test case;
  FORMAT
    data fmt := $g(0)$,
    data list fmt := $"("n(upb data list-1)(f(data fmt)", ")f(data fmt)")"$;

# FOR DATALIST permutation IN # perm gen permutations(test case#) DO (#,
##   (PERMDATALIST permutation)VOID:(
    printf((data list fmt, permutation, $l$))
# OD #))

)
```
'''Output:'''

```txt

(1, 22, 333, 44444)
(1, 22, 44444, 333)
(1, 333, 22, 44444)
(1, 333, 44444, 22)
(1, 44444, 22, 333)
(1, 44444, 333, 22)
(22, 1, 333, 44444)
(22, 1, 44444, 333)
(22, 333, 1, 44444)
(22, 333, 44444, 1)
(22, 44444, 1, 333)
(22, 44444, 333, 1)
(333, 1, 22, 44444)
(333, 1, 44444, 22)
(333, 22, 1, 44444)
(333, 22, 44444, 1)
(333, 44444, 1, 22)
(333, 44444, 22, 1)
(44444, 1, 22, 333)
(44444, 1, 333, 22)
(44444, 22, 1, 333)
(44444, 22, 333, 1)
(44444, 333, 1, 22)
(44444, 333, 22, 1)

```




## AppleScript


### Recursive

(Functional ES6 version)

Recursively, in terms of concatMap and delete:

```AppleScript
-- PERMUTATIONS --------------------------------------------------------------

-- permutations :: [a] -> [[a]]
on permutations(xs)
    script go
        on |λ|(xs)
            script h
                on |λ|(x)
                    script ts
                        on |λ|(ys)
                            {{x} & ys}
                        end |λ|
                    end script
                    concatMap(ts, go's |λ|(|delete|(x, xs)))
                end |λ|
            end script

            if {} ≠ xs then
                concatMap(h, xs)
            else
                {{}}
            end if
        end |λ|
    end script
    go's |λ|(xs)
end permutations


-- TEST ----------------------------------------------------------------------
on run

    permutations({"aardvarks", "eat", "ants"})

end run


-- GENERIC FUNCTIONS ---------------------------------------------------------

-- concatMap :: (a -> [b]) -> [a] -> [b]
on concatMap(f, xs)
    set lst to {}
    set lng to length of xs
    tell mReturn(f)
        repeat with i from 1 to lng
            set lst to (lst & |λ|(contents of item i of xs, i, xs))
        end repeat
    end tell
    return lst
end concatMap

-- delete :: a -> [a] -> [a]
on |delete|(x, xs)
    if length of xs > 0 then
        set {h, t} to uncons(xs)
        if x = h then
            t
        else
            {h} & |delete|(x, t)
        end if
    else
        {}
    end if
end |delete|


-- Lift 2nd class handler function into 1st class script wrapper
-- mReturn :: Handler -> Script
on mReturn(f)
    if class of f is script then
        f
    else
        script
            property |λ| : f
        end script
    end if
end mReturn

-- uncons :: [a] -> Maybe (a, [a])
on uncons(xs)
    if length of xs > 0 then
        {item 1 of xs, rest of xs}
    else
        missing value
    end if
end uncons
```

```txt
{{"aardvarks", "eat", "ants"}, {"aardvarks", "ants", "eat"},
{"eat", "aardvarks", "ants"}, {"eat", "ants", "aardvarks"},
{"ants", "aardvarks", "eat"}, {"ants", "eat", "aardvarks"}}
```


(Fast recursive Heap's algorithm)

```AppleScript
to DoPermutations(aList, n)
    --> Heaps's algorithm (Permutation by interchanging pairs) AppleScript by Jean.O.matiC
    if n = 1 then
        tell (a reference to Permlist) to copy aList to its end
        -- or: copy aList as text (for concatenated results)
    else
        repeat with i from 1 to n
            DoPermutations(aList, n - 1)
            if n mod 2 = 0 then -- n is even
                tell aList to set [item i, item n] to [item n, item i] -- swaps items i and n of aList
            else
                tell aList to set [item 1, item n] to [item n, item 1] -- swaps items 1 and n of aList
            end if
            set i to i + 1
        end repeat
    end if
    return (a reference to Permlist) as list
end DoPermutations

--> Example 1 (list of words)
set [SourceList, Permlist] to [{"Good", "Johnny", "Be"}, {}]
DoPermutations(SourceList, SourceList's length)
--> result (value of Permlist)
{{"Good", "Johnny", "Be"}, {"Johnny", "Good", "Be"}, {"Be", "Good", "Johnny"}, ¬
    {"Good", "Be", "Johnny"}, {"Johnny", "Be", "Good"}, {"Be", "Johnny", "Good"}}

--> Example 2 (characters with concatenated results)
set [SourceList, Permlist] to [{"X", "Y", "Z"}, {}]
DoPermutations(SourceList, SourceList's length)
--> result (value of Permlist)
{"XYZ", "YXZ", "ZXY", "XZY", "YZX", "ZYX"}

--> Example 3 (Integers)
set [SourceList, Permlist] to [{1, 2, 3}, {}]
DoPermutations(SourceList, SourceList's length)
--> result (value of Permlist)
--> Example 4 (Integers with concatenated results)
set [SourceList, Permlist] to [{1, 2, 3}, {}]
DoPermutations(SourceList, SourceList's length)
--> result (value of Permlist)
{"123", "213", "312", "132", "231", "321"}
```


===Non-recursive===
As a right fold (which turns out to be significantly faster than recurse + delete):

```applescript
-- permutations :: [a] -> [[a]]
on permutations(xs)
    script go
        on |λ|(x, a)
            script
                on |λ|(ys)
                    script infix
                        on |λ|(n)
                            if ys ≠ {} then
                                take(n, ys) & {x} & drop(n, ys)
                            else
                                {x}
                            end if
                        end |λ|
                    end script
                    map(infix, enumFromTo(0, (length of ys)))
                end |λ|
            end script
            concatMap(result, a)
        end |λ|
    end script
    foldr(go, {{}}, xs)
end permutations


-- TEST ---------------------------------------------------
on run

    permutations({1, 2, 3})

    --> {{1, 2, 3}, {2, 1, 3}, {2, 3, 1}, {1, 3, 2}, {3, 1, 2}, {3, 2, 1}}
end run


-- GENERIC ------------------------------------------------

-- concatMap :: (a -> [b]) -> [a] -> [b]
on concatMap(f, xs)
    set lng to length of xs
    set acc to {}
    tell mReturn(f)
        repeat with i from 1 to lng
            set acc to acc & |λ|(item i of xs, i, xs)
        end repeat
    end tell
    return acc
end concatMap

-- drop :: Int -> [a] -> [a]
on drop(n, xs)
    if n < length of xs then
        items (1 + n) thru -1 of xs
    else
        {}
    end if
end drop

-- enumFromTo :: Int -> Int -> [Int]
on enumFromTo(m, n)
    if m ≤ n then
        set lst to {}
        repeat with i from m to n
            set end of lst to i
        end repeat
        return lst
    else
        return {}
    end if
end enumFromTo

-- foldr :: (a -> b -> b) -> b -> [a] -> b
on foldr(f, startValue, xs)
    tell mReturn(f)
        set v to startValue
        set lng to length of xs
        repeat with i from lng to 1 by -1
            set v to |λ|(item i of xs, v, i, xs)
        end repeat
        return v
    end tell
end foldr

-- Lift 2nd class handler function into 1st class script wrapper
-- mReturn :: First-class m => (a -> b) -> m (a -> b)
on mReturn(f)
    if class of f is script then
        f
    else
        script
            property |λ| : f
        end script
    end if
end mReturn

-- map :: (a -> b) -> [a] -> [b]
on map(f, xs)
    tell mReturn(f)
        set lng to length of xs
        set lst to {}
        repeat with i from 1 to lng
            set end of lst to |λ|(item i of xs, i, xs)
        end repeat
        return lst
    end tell
end map

-- min :: Ord a => a -> a -> a
on min(x, y)
    if y < x then
        y
    else
        x
    end if
end min

-- take :: Int -> [a] -> [a]
-- take :: Int -> String -> String
on take(n, xs)
    if 0 < n then
        items 1 thru min(n, length of xs) of xs
    else
        {}
    end if
end take
```

```txt
```



## AutoHotkey

from the forum topic http://www.autohotkey.com/forum/viewtopic.php?t=77959

```AutoHotkey
#NoEnv
StringCaseSense On

o := str := "Hello"

Loop
{
   str := perm_next(str)
   If !str
   {
      MsgBox % clipboard := o
      break
   }
   o.= "`n" . str
}

perm_Next(str){
   p := 0, sLen := StrLen(str)
   Loop % sLen
   {
      If A_Index=1
         continue
      t := SubStr(str, sLen+1-A_Index, 1)
      n := SubStr(str, sLen+2-A_Index, 1)
      If ( t < n )
      {
         p := sLen+1-A_Index, pC := SubStr(str, p, 1)
         break
      }
   }
   If !p
      return false
   Loop
   {
      t := SubStr(str, sLen+1-A_Index, 1)
      If ( t > pC )
      {
         n := sLen+1-A_Index, nC := SubStr(str, n, 1)
         break
      }
   }
   return SubStr(str, 1, p-1) . nC . Reverse(SubStr(str, p+1, n-p-1) . pC .  SubStr(str, n+1))
}

Reverse(s){
   Loop Parse, s
      o := A_LoopField o
   return o
}
```

<pre style="height:40ex;overflow:scroll">Hello
Helol
Heoll
Hlelo
Hleol
Hlleo
Hlloe
Hloel
Hlole
Hoell
Holel
Holle
eHllo
eHlol
eHoll
elHlo
elHol
ellHo
elloH
eloHl
elolH
eoHll
eolHl
eollH
lHelo
lHeol
lHleo
lHloe
lHoel
lHole
leHlo
leHol
lelHo
leloH
leoHl
leolH
llHeo
llHoe
lleHo
lleoH
lloHe
lloeH
loHel
loHle
loeHl
loelH
lolHe
loleH
oHell
oHlel
oHlle
oeHll
oelHl
oellH
olHel
olHle
oleHl
olelH
ollHe
olleH
```



### Alternate Version

Alternate version to produce numerical permutations of combinations.

```ahk
P(n,k="",opt=0,delim="",str="") { ; generate all n choose k permutations lexicographically
	;1..n = range, or delimited list, or string to parse
	;	to process with a different min index, pass a delimited list, e.g. "0`n1`n2"
	;k = length of result
	;opt 0 = no repetitions
	;opt 1 = with repetitions
	;opt 2 = run for 1..k
	;opt 3 = run for 1..k with repetitions
	;str = string to prepend (used internally)
	;returns delimited string, error message, or (if k > n) a blank string
	i:=0
	If !InStr(n,"`n")
		If n in 2,3,4,5,6,7,8,9
			Loop, %n%
				n := A_Index = 1 ? A_Index : n "`n" A_Index
		Else
			Loop, Parse, n, %delim%
				n := A_Index = 1 ? A_LoopField : n "`n" A_LoopField
	If (k = "")
		RegExReplace(n,"`n","",k), k++
	If k is not Digit
		Return "k must be a digit."
	If opt not in 0,1,2,3
		Return "opt invalid."
	If k = 0
		Return str
	Else
		Loop, Parse, n, `n
			If (!InStr(str,A_LoopField) || opt & 1)
				s .= (!i++ ? (opt & 2 ? str "`n" : "") : "`n" )
					. P(n,k-1,opt,delim,str . A_LoopField . delim)
		Return s
}
```

```ahk
MsgBox % P(3)
```

<pre style="height:40ex;overflow:scroll">---------------------------
permute.ahk
---------------------------
123
132
213
231
312
321
---------------------------
OK
---------------------------
```


```ahk
MsgBox % P("Hello",3)
```

<pre style="height:40ex;overflow:scroll">---------------------------
permute.ahk
---------------------------
Hel
Hel
Heo
Hle
Hlo
Hle
Hlo
Hoe
Hol
Hol
eHl
eHl
eHo
elH
elo
elH
elo
eoH
eol
eol
lHe
lHo
leH
leo
loH
loe
lHe
lHo
leH
leo
loH
loe
oHe
oHl
oHl
oeH
oel
oel
olH
ole
olH
ole
---------------------------
OK
---------------------------
```


```ahk
MsgBox % P("2`n3`n4`n5",2,3)
```

<pre style="height:40ex;overflow:scroll">---------------------------
permute.ahk
---------------------------

2
22
23
24
25
3
32
33
34
35
4
42
43
44
45
5
52
53
54
55
---------------------------
OK
---------------------------
```


```ahk
MsgBox % P("11 a text ] u+z",3,0," ")
```

<pre style="height:40ex;overflow:scroll">---------------------------
permute.ahk
---------------------------
11 a text
11 a ]
11 a u+z
11 text a
11 text ]
11 text u+z
11 ] a
11 ] text
11 ] u+z
11 u+z a
11 u+z text
11 u+z ]
a 11 text
a 11 ]
a 11 u+z
a text 11
a text ]
a text u+z
a ] 11
a ] text
a ] u+z
a u+z 11
a u+z text
a u+z ]
text 11 a
text 11 ]
text 11 u+z
text a 11
text a ]
text a u+z
text ] 11
text ] a
text ] u+z
text u+z 11
text u+z a
text u+z ]
] 11 a
] 11 text
] 11 u+z
] a 11
] a text
] a u+z
] text 11
] text a
] text u+z
] u+z 11
] u+z a
] u+z text
u+z 11 a
u+z 11 text
u+z 11 ]
u+z a 11
u+z a text
u+z a ]
u+z text 11
u+z text a
u+z text ]
u+z ] 11
u+z ] a
u+z ] text
---------------------------
OK
---------------------------
```


## Arturo


```arturo
print $(permutations #(1 2 3))
```

```txt
#(#(1 2 3) #(1 3 2) #(2 1 3) #(2 3 1) #(3 1 2) #(3 2 1))
```



## Batch File

Recursive permutation generator.

```Batch File

@echo off
setlocal enabledelayedexpansion
set arr=ABCD
set /a n=4
:: echo !arr!
call :permu  %n% arr
goto:eof

:permu num  &arr
setlocal
if %1 equ 1 call echo(!%2! & exit /b
set /a "num=%1-1,n2=num-1"
set arr=!%2!
for /L %%c in (0,1,!n2!) do (
   call:permu !num! arr
   set /a  n1="num&1"
   if !n1! equ 0 (call:swapit !num! 0 arr) else (call:swapit !num! %%c arr)
   )
   call:permu !num! arr
endlocal & set %2=%arr%
exit /b

:swapit  from  to  &arr
setlocal
set arr=!%3!
set temp1=!arr:~%~1,1!
set temp2=!arr:~%~2,1!
set arr=!arr:%temp1%=@!
set arr=!arr:%temp2%=%temp1%!
set arr=!arr:@=%temp2%!
:: echo %1 %2 !%~3! !arr!
endlocal & set %3=%arr%
exit /b

```

```txt

ABCD
BACD
CABD
ACBD
BCAD
CBAD
DBAC
BDAC
ADBC
DABC
BADC
ABDC
ACDB
CADB
DACB
ADCB
CDAB
DCAB
DCBA
CDBA
BDCA
DBCA
CBDA
BCDA

```



## BBC BASIC

The procedure PROC_NextPermutation() will give the next lexicographic permutation of an integer array.

```bbcbasic
      DIM List%(3)
      List%() = 1, 2, 3, 4
      FOR perm% = 1 TO 24
        FOR i% = 0 TO DIM(List%(),1)
          PRINT List%(i%);
        NEXT
        PRINT
        PROC_NextPermutation(List%())
      NEXT
      END

      DEF PROC_NextPermutation(A%())
      LOCAL first, last, elementcount, pos
      elementcount = DIM(A%(),1)
      IF elementcount < 1 THEN ENDPROC
      pos = elementcount-1
      WHILE A%(pos) >= A%(pos+1)
        pos -= 1
        IF pos < 0 THEN
          PROC_Permutation_Reverse(A%(), 0, elementcount)
          ENDPROC
        ENDIF
      ENDWHILE
      last = elementcount
      WHILE A%(last) <= A%(pos)
        last -= 1
      ENDWHILE
      SWAP A%(pos), A%(last)
      PROC_Permutation_Reverse(A%(), pos+1, elementcount)
      ENDPROC

      DEF PROC_Permutation_Reverse(A%(), first, last)
      WHILE first < last
        SWAP A%(first), A%(last)
        first += 1
        last -= 1
      ENDWHILE
      ENDPROC
```

'''Output:'''

```txt

         1         2         3         4
         1         2         4         3
         1         3         2         4
         1         3         4         2
         1         4         2         3
         1         4         3         2
         2         1         3         4
         2         1         4         3
         2         3         1         4
         2         3         4         1
         2         4         1         3
         2         4         3         1
         3         1         2         4
         3         1         4         2
         3         2         1         4
         3         2         4         1
         3         4         1         2
         3         4         2         1
         4         1         2         3
         4         1         3         2
         4         2         1         3
         4         2         3         1
         4         3         1         2
         4         3         2         1

```



## Bracmat


```bracmat
  ( perm
  =   prefix List result original A Z
    .   !arg:(?.)
      |   !arg:(?prefix.?List:?original)
        & :?result
        &   whl
          ' ( !List:%?A ?Z
            & !result perm$(!prefix !A.!Z):?result
            & !Z !A:~!original:?List
            )
        & !result
  )
& out$(perm$(.a 2 "]" u+z);
```

Output:

```txt
  (a 2 ] u+z.)
  (a 2 u+z ].)
  (a ] u+z 2.)
  (a ] 2 u+z.)
  (a u+z 2 ].)
  (a u+z ] 2.)
  (2 ] u+z a.)
  (2 ] a u+z.)
  (2 u+z a ].)
  (2 u+z ] a.)
  (2 a ] u+z.)
  (2 a u+z ].)
  (] u+z a 2.)
  (] u+z 2 a.)
  (] a 2 u+z.)
  (] a u+z 2.)
  (] 2 u+z a.)
  (] 2 a u+z.)
  (u+z a 2 ].)
  (u+z a ] 2.)
  (u+z 2 ] a.)
  (u+z 2 a ].)
  (u+z ] a 2.)
  (u+z ] 2 a.)
```



## C


### version 1

Non-recursive algorithm to generate all permutations. It prints objects in lexicographical order.

```c

#include <stdio.h>
int main (int argc, char *argv[]) {
//here we check arguments
	if (argc < 2) {
        printf("Enter an argument. Example 1234 or dcba:\n");
        return 0;
	}
//it calculates an array's length
        int x;
        for (x = 0; argv[1][x] != '\0'; x++);
//buble sort the array
	int f, v, m;
	 for(f=0; f < x; f++) {
    	 for(v = x-1; v > f; v-- ) {
     	 if (argv[1][v-1] > argv[1][v]) {
	m=argv[1][v-1];
	argv[1][v-1]=argv[1][v];
	argv[1][v]=m;
    }
  }
}

//it calculates a factorial to stop the algorithm
    char a[x];
	int k=0;
	int fact=k+1;
             while (k!=x) {
                   a[k]=argv[1][k];
               	   k++;
		  fact = k*fact;
                   }
                   a[k]='\0';
//Main part: here we permutate
           int i, j;
           int y=0;
           char c;
          while (y != fact) {
          printf("%s\n", a);
          i=x-2;
          while(a[i] > a[i+1] ) i--;
          j=x-1;
          while(a[j] < a[i] ) j--;
      c=a[j];
      a[j]=a[i];
      a[i]=c;
i++;
for (j = x-1; j > i; i++, j--) {
  c = a[i];
  a[i] = a[j];
  a[j] = c;
      }
y++;
   }
}

```



### version 2

Non-recursive algorithm to generate all permutations. It prints them from right to left.

```c


#include <stdio.h>
int main() {
        char a[] = "4321";  //array
           int i, j;
           int f=24; 	    //factorial
           char c;          //buffer
          while (f--) {
          printf("%s\n", a);
          i=1;
          while(a[i] > a[i-1]) i++;
          j=0;
          while(a[j] < a[i])j++;
      c=a[j];
      a[j]=a[i];
      a[i]=c;
i--;
for (j = 0; j < i; i--, j++) {
  c = a[i];
  a[i] = a[j];
  a[j] = c;
      }
   }
}


```



### version 3

See [[wp:Permutation#Systematic_generation_of_all_permutations|lexicographic generation]] of permutations.

```c
#include <stdio.h>
#include <stdlib.h>

/* print a list of ints */
int show(int *x, int len)
{
	int i;
	for (i = 0; i < len; i++)
		printf("%d%c", x[i], i == len - 1 ? '\n' : ' ');
	return 1;
}

/* next lexicographical permutation */
int next_lex_perm(int *a, int n) {
#	define swap(i, j) {t = a[i]; a[i] = a[j]; a[j] = t;}
	int k, l, t;

	/* 1. Find the largest index k such that a[k] < a[k + 1]. If no such
	      index exists, the permutation is the last permutation. */
	for (k = n - 1; k && a[k - 1] >= a[k]; k--);
	if (!k--) return 0;

	/* 2. Find the largest index l such that a[k] < a[l]. Since k + 1 is
	   such an index, l is well defined */
	for (l = n - 1; a[l] <= a[k]; l--);

	/* 3. Swap a[k] with a[l] */
	swap(k, l);

	/* 4. Reverse the sequence from a[k + 1] to the end */
	for (k++, l = n - 1; l > k; l--, k++)
		swap(k, l);
	return 1;
#	undef swap
}

void perm1(int *x, int n, int callback(int *, int))
{
	do {
		if (callback) callback(x, n);
	} while (next_lex_perm(x, n));
}

/* Boothroyd method; exactly N! swaps, about as fast as it gets */
void boothroyd(int *x, int n, int nn, int callback(int *, int))
{
	int c = 0, i, t;
	while (1) {
		if (n > 2) boothroyd(x, n - 1, nn, callback);
		if (c >= n - 1) return;

		i = (n & 1) ? 0 : c;
		c++;
		t = x[n - 1], x[n - 1] = x[i], x[i] = t;
		if (callback) callback(x, nn);
	}
}

/* entry for Boothroyd method */
void perm2(int *x, int n, int callback(int*, int))
{
	if (callback) callback(x, n);
	boothroyd(x, n, n, callback);
}

/* same as perm2, but flattened recursions into iterations */
void perm3(int *x, int n, int callback(int*, int))
{
	/* calloc isn't strictly necessary, int c[32] would suffice
	   for most practical purposes */
	int d, i, t, *c = calloc(n, sizeof(int));

	/* curiously, with GCC 4.6.1 -O3, removing next line makes
	   it ~25% slower */
	if (callback) callback(x, n);
	for (d = 1; ; c[d]++) {
		while (d > 1) c[--d] = 0;
		while (c[d] >= d)
			if (++d >= n) goto done;

		t = x[ i = (d & 1) ? c[d] : 0 ], x[i] = x[d], x[d] = t;
		if (callback) callback(x, n);
	}
done:	free(c);
}

#define N 4

int main()
{
	int i, x[N];
	for (i = 0; i < N; i++) x[i] = i + 1;

	/* three different methods */
	perm1(x, N, show);
	perm2(x, N, show);
	perm3(x, N, show);

	return 0;
}

```



### version 4

See [[wp:Permutation#Systematic_generation_of_all_permutations|lexicographic generation]] of permutations.

```c
#include <stdio.h>
#include <stdlib.h>

/* print a list of ints */
int show(int *x, int len)
{
	int i;
	for (i = 0; i < len; i++)
		printf("%d%c", x[i], i == len - 1 ? '\n' : ' ');
	return 1;
}

/* next lexicographical permutation */
int next_lex_perm(int *a, int n) {
#	define swap(i, j) {t = a[i]; a[i] = a[j]; a[j] = t;}
	int k, l, t;

	/* 1. Find the largest index k such that a[k] < a[k + 1]. If no such
	      index exists, the permutation is the last permutation. */
	for (k = n - 1; k && a[k - 1] >= a[k]; k--);
	if (!k--) return 0;

	/* 2. Find the largest index l such that a[k] < a[l]. Since k + 1 is
	   such an index, l is well defined */
	for (l = n - 1; a[l] <= a[k]; l--);

	/* 3. Swap a[k] with a[l] */
	swap(k, l);

	/* 4. Reverse the sequence from a[k + 1] to the end */
	for (k++, l = n - 1; l > k; l--, k++)
		swap(k, l);
	return 1;
#	undef swap
}

void perm1(int *x, int n, int callback(int *, int))
{
	do {
		if (callback) callback(x, n);
	} while (next_lex_perm(x, n));
}

/* Boothroyd method; exactly N! swaps, about as fast as it gets */
void boothroyd(int *x, int n, int nn, int callback(int *, int))
{
	int c = 0, i, t;
	while (1) {
		if (n > 2) boothroyd(x, n - 1, nn, callback);
		if (c >= n - 1) return;

		i = (n & 1) ? 0 : c;
		c++;
		t = x[n - 1], x[n - 1] = x[i], x[i] = t;
		if (callback) callback(x, nn);
	}
}

/* entry for Boothroyd method */
void perm2(int *x, int n, int callback(int*, int))
{
	if (callback) callback(x, n);
	boothroyd(x, n, n, callback);
}

/* same as perm2, but flattened recursions into iterations */
void perm3(int *x, int n, int callback(int*, int))
{
	/* calloc isn't strictly necessary, int c[32] would suffice
	   for most practical purposes */
	int d, i, t, *c = calloc(n, sizeof(int));

	/* curiously, with GCC 4.6.1 -O3, removing next line makes
	   it ~25% slower */
	if (callback) callback(x, n);
	for (d = 1; ; c[d]++) {
		while (d > 1) c[--d] = 0;
		while (c[d] >= d)
			if (++d >= n) goto done;

		t = x[ i = (d & 1) ? c[d] : 0 ], x[i] = x[d], x[d] = t;
		if (callback) callback(x, n);
	}
done:	free(c);
}

#define N 4

int main()
{
	int i, x[N];
	for (i = 0; i < N; i++) x[i] = i + 1;

	/* three different methods */
	perm1(x, N, show);
	perm2(x, N, show);
	perm3(x, N, show);

	return 0;
}

```



## C++

The C++ standard library provides for this in the form of <code>std::next_permutation</code> and <code>std::prev_permutation</code>.

```cpp
#include <algorithm>
#include <string>
#include <vector>
#include <iostream>

template<class T>
void print(const std::vector<T> &vec)
{
    for (typename std::vector<T>::const_iterator i = vec.begin(); i != vec.end(); ++i)
    {
        std::cout << *i;
        if ((i + 1) != vec.end())
            std::cout << ",";
    }
    std::cout << std::endl;
}

int main()
{
    //Permutations for strings
    std::string example("Hello");
    std::sort(example.begin(), example.end());
    do {
        std::cout << example << '\n';
    } while (std::next_permutation(example.begin(), example.end()));

    // And for vectors
    std::vector<int> another;
    another.push_back(1234);
    another.push_back(4321);
    another.push_back(1234);
    another.push_back(9999);

    std::sort(another.begin(), another.end());
    do {
        print(another);
    } while (std::next_permutation(another.begin(), another.end()));

    return 0;
}
```

```txt

Hello
Helol
Heoll
Hlelo
Hleol
Hlleo
Hlloe
Hloel
Hlole
Hoell
Holel
Holle
eHllo
eHlol
eHoll
elHlo
elHol
ellHo
elloH
eloHl
elolH
eoHll
eolHl
eollH
lHelo
lHeol
lHleo
lHloe
lHoel
lHole
leHlo
leHol
lelHo
leloH
leoHl
leolH
llHeo
llHoe
lleHo
lleoH
lloHe
lloeH
loHel
loHle
loeHl
loelH
lolHe
loleH
oHell
oHlel
oHlle
oeHll
oelHl
oellH
olHel
olHle
oleHl
olelH
ollHe
olleH
1234,1234,4321,9999
1234,1234,9999,4321
1234,4321,1234,9999
1234,4321,9999,1234
1234,9999,1234,4321
1234,9999,4321,1234
4321,1234,1234,9999
4321,1234,9999,1234
4321,9999,1234,1234
9999,1234,1234,4321
9999,1234,4321,1234
9999,4321,1234,1234
```


## C#
Recursive Linq
```csharp>public static IEnumerable<IEnumerable<T>> Permutations<T
(this IEnumerable<T> values)
{
     if (values.Count() == 1)
         return new [] {values};
     return values.SelectMany(v => Permutations(values.Where(x=> x != v)),(v, p) => p.Prepend(v));
}
```

Usage

```sharp
Enumerable.Range(0,5).Permutations()
```

A recursive Iterator. Runs under C#2 (VS2005), i.e. no `var`, no lambdas,...

```csharp>public class Permutations<T

{
    public static System.Collections.Generic.IEnumerable<T[]> AllFor(T[] array)
    {
        if (array == null || array.Length == 0)
        {
            yield return new T[0];
        }
        else
        {
            for (int pick = 0; pick < array.Length; ++pick)
            {
                T item = array[pick];
                int i = -1;
                T[] rest = System.Array.FindAll<T>(
                    array, delegate(T p) { return ++i != pick; }
                );
                foreach (T[] restPermuted in AllFor(rest))
                {
                    i = -1;
                    yield return System.Array.ConvertAll<T, T>(
                        array,
                        delegate(T p) {
                            return ++i == 0 ? item : restPermuted[i - 1];
                        }
                    );
                }
            }
        }
    }
}
```

Usage:

```c#
namespace Permutations_On_RosettaCode
{
    class Program
    {
        static void Main(string[] args)
        {
            string[] list = "a b c d".Split();
            foreach (string[] permutation in Permutations<string>.AllFor(list))
            {
                System.Console.WriteLine(string.Join(" ", permutation));
            }
        }
    }
}
```





Recursive version

```c#
using System;
class Permutations
{
  static int n = 4;
  static int [] buf = new int [n];
  static bool [] used = new bool [n];

  static void Main()
  {
    for (int i = 0; i < n; i++) used [i] = false;
    rec(0);
  }

  static void rec(int ind)
  {
    for (int i = 0; i < n; i++)
    {
      if (!used [i])
      {
        used [i] = true;
        buf [ind] = i;
	if (ind + 1 < n) rec(ind + 1);
        else Console.WriteLine(string.Join(",", buf));
	used [i] = false;
      }
    }
  }
}
```


Alternate recursive version


```c#

using System;
class Permutations
{
  static int n = 4;
  static int [] buf = new int [n];
  static int [] next = new int [n+1];

  static void Main()
  {
    for (int i = 0; i < n; i++) next [i] = i + 1;
    next[n] = 0;
    rec(0);
  }

  static void rec(int ind)
  {
    for (int i = n; next[i] != n; i = next[i])
    {
      buf [ind] = next[i];
      next[i]=next[next[i]];
      if (ind < n - 1) rec(ind + 1);
      else Console.WriteLine(string.Join(",", buf));
      next[i] = buf [ind];
    }
  }
}

```



## Clojure


### Library function

In an REPL:


```clojure

user=> (require 'clojure.contrib.combinatorics)
nil
user=> (clojure.contrib.combinatorics/permutations [1 2 3])
((1 2 3) (1 3 2) (2 1 3) (2 3 1) (3 1 2) (3 2 1))
```



### Explicit

Replacing the call to the combinatorics library function by its real implementation.

```clojure

(defn- iter-perm [v]
  (let [len (count v),
	j (loop [i (- len 2)]
	     (cond (= i -1) nil
		   (< (v i) (v (inc i))) i
		   :else (recur (dec i))))]
    (when j
      (let [vj (v j),
	    l (loop [i (dec len)]
		(if (< vj (v i)) i (recur (dec i))))]
	(loop [v (assoc v j (v l) l vj), k (inc j), l (dec len)]
	  (if (< k l)
	    (recur (assoc v k (v l) l (v k)) (inc k) (dec l))
	    v))))))


(defn- vec-lex-permutations [v]
  (when v (cons v (lazy-seq (vec-lex-permutations (iter-perm v))))))

(defn lex-permutations
  "Fast lexicographic permutation generator for a sequence of numbers"
  [c]
  (lazy-seq
   (let [vec-sorted (vec (sort c))]
     (if (zero? (count vec-sorted))
       (list [])
       (vec-lex-permutations vec-sorted)))))

(defn permutations
  "All the permutations of items, lexicographic by index"
  [items]
  (let [v (vec items)]
    (map #(map v %) (lex-permutations (range (count v))))))

(println (permutations [1 2 3]))


```



## CoffeeScript


```coffeescript
# Returns a copy of an array with the element at a specific position
# removed from it.
arrayExcept = (arr, idx) ->
	res = arr[0..]
	res.splice idx, 1
	res

# The actual function which returns the permutations of an array-like
# object (or a proper array).
permute = (arr) ->
	arr = Array::slice.call arr, 0
	return [[]] if arr.length == 0

	permutations = (for value,idx in arr
		[value].concat perm for perm in permute arrayExcept arr, idx)

	# Flatten the array before returning it.
	[].concat permutations...
```

This implementation utilises the fact that the permutations of an array could be defined recursively, with the fixed point being the permutations of an empty array.
```coffeescript>coffee
 console.log (permute "123").join "\n"
1,2,3
1,3,2
2,1,3
2,3,1
3,1,2
3,2,1
```



## Common Lisp


```lisp
(defun permute (list)
  (if list
    (mapcan #'(lambda (x)
		(mapcar #'(lambda (y) (cons x y))
			(permute (remove x list))))
	    list)
    '(()))) ; else

(print (permute '(A B Z)))
```

```txt
((A B Z) (A Z B) (B A Z) (B Z A) (Z A B) (Z B A))
```

Lexicographic next permutation:

```lisp
(defun next-perm (vec cmp)  ; modify vector
  (declare (type (simple-array * (*)) vec))
  (macrolet ((el (i) `(aref vec ,i))
             (cmp (i j) `(funcall cmp (el ,i) (el ,j))))
    (loop with len = (1- (length vec))
       for i from (1- len) downto 0
       when (cmp i (1+ i)) do
         (loop for k from len downto i
            when (cmp i k) do
              (rotatef (el i) (el k))
              (setf k (1+ len))
              (loop while (< (incf i) (decf k)) do
                   (rotatef (el i) (el k)))
              (return-from next-perm vec)))))

;;; test code
(loop for a = "1234" then (next-perm a #'char<) while a do
     (write-line a))
```



## Crystal


```Ruby
puts [1, 2, 3].permutations
```

```txt
[[1, 2, 3], [1, 3, 2], [2, 1, 3], [2, 3, 1], [3, 1, 2], [3, 2, 1]]
```



## Curry



```curry

insert :: a -> [a] -> [a]
insert x xs  = x : xs
insert x (y:ys) = y : insert x ys

permutation :: [a] -> [a]
permutation []     = []
permutation (x:xs) = insert x $ permutation xs

```



## D


### Simple Eager version

Compile with -version=permutations1_main to see the output.

```d
T[][] permutations(T)(T[] items) pure nothrow {
    T[][] result;

    void perms(T[] s, T[] prefix=[]) nothrow {
        if (s.length)
            foreach (immutable i, immutable c; s)
               perms(s[0 .. i] ~ s[i+1 .. $], prefix ~ c);
        else
            result ~= prefix;
    }

    perms(items);
    return result;
}

version (permutations1_main) {
    void main() {
        import std.stdio;
        writefln("%(%s\n%)", [1, 2, 3].permutations);
    }
}
```

```txt
[1, 2, 3]
[1, 3, 2]
[2, 1, 3]
[2, 3, 1]
[3, 1, 2]
[3, 2, 1]
```



### Fast Lazy Version

Compiled with <code>-version=permutations2_main</code> produces its output.

```d
import std.algorithm, std.conv, std.traits;

struct Permutations(bool doCopy=true, T) if (isMutable!T) {
    private immutable size_t num;
    private T[] items;
    private uint[31] indexes;
    private ulong tot;

    this (T[] items) pure nothrow @safe @nogc
    in {
        static enum string L = indexes.length.text;
        assert(items.length >= 0 && items.length <= indexes.length,
               "Permutations: items.length must be >= 0 && < " ~ L);
    } body {
        static ulong factorial(in size_t n) pure nothrow @safe @nogc {
            ulong result = 1;
            foreach (immutable i; 2 .. n + 1)
                result *= i;
            return result;
        }

        this.num = items.length;
        this.items = items;
        foreach (immutable i; 0 .. cast(typeof(indexes[0]))this.num)
            this.indexes[i] = i;
        this.tot = factorial(this.num);
    }

    @property T[] front() pure nothrow @safe {
        static if (doCopy) {
            return items.dup;
        } else
            return items;
    }

    @property bool empty() const pure nothrow @safe @nogc {
        return tot == 0;
    }

    @property size_t length() const pure nothrow @safe @nogc {
        // Not cached to keep the function pure.
        typeof(return) result = 1;
        foreach (immutable x; 1 .. items.length + 1)
            result *= x;
        return result;
    }

    void popFront() pure nothrow @safe @nogc {
        tot--;
        if (tot > 0) {
            size_t j = num - 2;

            while (indexes[j] > indexes[j + 1])
                j--;
            size_t k = num - 1;
            while (indexes[j] > indexes[k])
                k--;
            swap(indexes[k], indexes[j]);
            swap(items[k], items[j]);

            size_t r = num - 1;
            size_t s = j + 1;
            while (r > s) {
                swap(indexes[s], indexes[r]);
                swap(items[s], items[r]);
                r--;
                s++;
            }
        }
    }
}

Permutations!(doCopy,T) permutations(bool doCopy=true, T)
                                    (T[] items)
pure nothrow if (isMutable!T) {
    return Permutations!(doCopy, T)(items);
}

version (permutations2_main) {
    void main() {
        import std.stdio, std.bigint;
        alias B = BigInt;
        foreach (p; [B(1), B(2), B(3)].permutations)
            assert((p[0] + 1) > 0);
        [1, 2, 3].permutations!false.writeln;
        [B(1), B(2), B(3)].permutations!false.writeln;
    }
}
```



### Standard Version


```d
void main() {
    import std.stdio, std.algorithm;

    auto items = [1, 2, 3];
    do
        items.writeln;
    while (items.nextPermutation);
}
```



## Delphi


```Delphi
program TestPermutations;

{$APPTYPE CONSOLE}

type
  TItem = Integer;                // declare ordinal type for array item
  TArray = array[0..3] of TItem;

const
  Source: TArray = (1, 2, 3, 4);

procedure Permutation(K: Integer; var A: TArray);
var
  I, J: Integer;
  Tmp: TItem;

begin
  for I:= Low(A) + 1 to High(A) + 1 do begin
    J:= K mod I;
    Tmp:= A[J];
    A[J]:= A[I - 1];
    A[I - 1]:= Tmp;
    K:= K div I;
  end;
end;

var
  A: TArray;
  I, K, Count: Integer;
  S, S1, S2: ShortString;

begin
  Count:= 1;
  I:= Length(A);
  while I > 1 do begin
    Count:= Count * I;
    Dec(I);
  end;

  S:= '';
  for K:= 0 to Count - 1 do begin
    A:= Source;
    Permutation(K, A);
    S1:= '';
    for I:= Low(A) to High(A) do begin
      Str(A[I]:1, S2);
      S1:= S1 + S2;
    end;
    S:= S + '  ' + S1;
    if Length(S) > 40 then begin
      Writeln(S);
      S:= '';
    end;
  end;

  if Length(S) > 0 then Writeln(S);
  Readln;
end.
```

```txt

  4123  4213  4312  4321  4132  4231  3421
  3412  2413  1423  2431  1432  3142  3241
  2341  1342  2143  1243  3124  3214  2314
  1324  2134  1234

```



## Eiffel


```Eiffel

class
	APPLICATION

create
	make

feature {NONE}

	make
		do
			test := <<2, 5, 1>>
			permute (test, 1)
		end

	test: ARRAY [INTEGER]

	permute (a: ARRAY [INTEGER]; k: INTEGER)
			-- All permutations of 'a'.
		require
			count_positive: a.count > 0
			k_valid_index: k > 0
		local
			t: INTEGER
		do
			if k = a.count then
				across
					a as ar
				loop
					io.put_integer (ar.item)
				end
				io.new_line
			else
				across
					k |..| a.count as c
				loop
					t := a [k]
					a [k] := a [c.item]
					a [c.item] := t
					permute (a, k + 1)
					t := a [k]
					a [k] := a [c.item]
					a [c.item] := t
				end
			end
		end

end


```

```txt

251
215
521
512
152
125

```



## Elixir

```elixir
defmodule RC do
  def permute([]), do: [[]]
  def permute(list) do
    for x <- list, y <- permute(list -- [x]), do: [x|y]
  end
end

IO.inspect RC.permute([1, 2, 3])
```


```txt

[[1, 2, 3], [1, 3, 2], [2, 1, 3], [2, 3, 1], [3, 1, 2], [3, 2, 1]]

```



## Erlang

Shortest form:

```Erlang
-module(permute).
-export([permute/1]).

permute([]) -> [[]];
permute(L) -> [[X|Y] || X<-L, Y<-permute(L--[X])].
```

Y-combinator (for shell):

```Erlang
F = fun(L) -> G = fun(_, []) -> [[]]; (F, L) -> [[X|Y] || X<-L, Y<-F(F, L--[X])] end, G(G, L) end.
```

More efficient zipper implementation:

```Erlang
-module(permute).

-export([permute/1]).

permute([]) -> [[]];
permute(L) -> zipper(L, [], []).

% Use zipper to pick up first element of permutation
zipper([], _, Acc) -> lists:reverse(Acc);
zipper([H|T], R, Acc) ->
  % place current member in front of all permutations
  % of rest of set - both sides of zipper
  prepend(H, permute(lists:reverse(R, T)),
    % pass zipper state for continuation
    T, [H|R], Acc).

prepend(_, [], T, R, Acc) -> zipper(T, R, Acc); % continue in zipper
prepend(X, [H|T], ZT, ZR, Acc) -> prepend(X, T, ZT, ZR, [[X|H]|Acc]).
```

Demonstration (escript):

```Erlang
main(_) -> io:fwrite("~p~n", [permute:permute([1,2,3])]).
```

```txt
[[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]]
```



## Euphoria

```euphoria
function reverse(sequence s, integer first, integer last)
    object x
    while first < last do
        x = s[first]
        s[first] = s[last]
        s[last] = x
        first += 1
        last -= 1
    end while
    return s
end function

function nextPermutation(sequence s)
    integer pos, last
    object x
    if length(s) < 1 then
        return 0
    end if

    pos = length(s)-1
    while compare(s[pos], s[pos+1]) >= 0 do
        pos -= 1
        if pos < 1 then
            return -1
        end if
    end while

    last = length(s)
    while compare(s[last], s[pos]) <= 0 do
        last -= 1
    end while
    x = s[pos]
    s[pos] = s[last]
    s[last] = x

    return reverse(s, pos+1, length(s))
end function

object s
s = "abcd"
puts(1, s & '\t')
while 1 do
    s = nextPermutation(s)
    if atom(s) then
        exit
    end if
    puts(1, s & '\t')
end while
```

```txt
abcd    abdc    acbd    acdb    adbc    adcb    bacd    badc    bcad    bcda
bdac    bdca    cabd    cadb    cbad    cbda    cdab    cdba    dabc    dacb
dbac    dbca    dcab    dcba
```


=={{header|F Sharp|F#}}==

```fsharp

let rec insert left x right = seq {
    match right with
    | [] -> yield left @ [x]
    | head :: tail ->
        yield left @ [x] @ right
        yield! insert (left @ [head]) x tail
    }

let rec perms permute =
    seq {
        match permute with
        | [] -> yield []
        | head :: tail -> yield! Seq.collect (insert [] head) (perms tail)
    }

[<EntryPoint>]
let main argv =
    perms (Seq.toList argv)
    |> Seq.iter (fun x -> printf "%A\n" x)
    0

```



```txt

&gt;RosettaPermutations 1 2 3
["1"; "2"; "3"]
["2"; "1"; "3"]
["2"; "3"; "1"]
["1"; "3"; "2"]
["3"; "1"; "2"]
["3"; "2"; "1"]

```


Translation of Haskell "insertion-based approach" (last version)

```fsharp

let permutations xs =
    let rec insert x = function
        | [] -> [[x]]
        | head :: tail -> (x :: (head :: tail)) :: (List.map (fun l -> head :: l) (insert x tail))
    List.fold (fun s e -> List.collect (insert e) s) [[]] xs

```



## Factor

The all-permutations word is part of factor's standard library. See http://docs.factorcode.org/content/word-all-permutations,math.combinatorics.html


## Fortran


```fortran
program permutations

  implicit none
  integer, parameter :: value_min = 1
  integer, parameter :: value_max = 3
  integer, parameter :: position_min = value_min
  integer, parameter :: position_max = value_max
  integer, dimension (position_min : position_max) :: permutation

  call generate (position_min)

contains

  recursive subroutine generate (position)

    implicit none
    integer, intent (in) :: position
    integer :: value

    if (position > position_max) then
      write (*, *) permutation
    else
      do value = value_min, value_max
        if (.not. any (permutation (: position - 1) == value)) then
          permutation (position) = value
          call generate (position + 1)
        end if
      end do
    end if

  end subroutine generate

end program permutations
```

```txt
           1           2           3
           1           3           2
           2           1           3
           2           3           1
           3           1           2
           3           2           1
```



###  Alternate solution


Instead of looking up unused values, this program starts from [1, ..., n] and does only swaps, hence the array always represents a valid permutation.
The values need to be "swapped back" after the recursive call.


```fortran
program allperm
    implicit none
    integer :: n, i
    integer, allocatable :: a(:)
    read *, n
    allocate(a(n))
    a = [ (i, i = 1, n) ]
    call perm(1)
    deallocate(a)
contains
    recursive subroutine perm(i)
        integer :: i, j, t
        if (i == n) then
            print *, a
        else
            do j = i, n
                t = a(i)
                a(i) = a(j)
                a(j) = t
                call perm(i + 1)
                t = a(i)
                a(i) = a(j)
                a(j) = t
            end do
        end if
    end subroutine
end program
```




###  Fortran Speed Test

So ... what is the fastest algorithm?

Here below is the speed test for a couple of algorithms of permutation. We can add more algorithms into this frame-work. When they work in the same circumstance, we can see which is the fastest one.


```fortran
   program testing_permutation_algorithms

   implicit none
   integer :: nmax
   integer, dimension(:),allocatable :: ida
   logical :: mtc
   logical :: even
   integer :: i
   integer(8) :: ic
   integer :: clock_rate, clock_max, t1, t2
   real(8) :: dt
   integer :: pos_min, pos_max
!
!
!  Beginning:
!
   write(*,*) 'INPUT N:'
   read *, nmax
   write(*,*) 'N =', nmax
   allocate ( ida(1:nmax) )
!
!
!  (1) Starting:
!
   do i  =  1, nmax
      ida(i) = i
   enddo
!
   ic = 0
   call system_clock ( t1, clock_rate, clock_max )
!
   mtc = .false.
!
   do
      call subnexper ( nmax, ida, mtc, even )
!
!     1) counting the number of permutatations
!
      ic = ic + 1
!
!     2) writing out the result:
!
!     do i  =  1, nmax
!        write (100,"(i3,',')",advance = "no") ida(i)
!     enddo
!     write(100,*)
!
!     repeat if not being finished yet, otherwise exit.
!
      if (mtc) then
         cycle
      else
         exit
      endif
!
   enddo
!
   call system_clock ( t2, clock_rate, clock_max )
   dt =  ( dble(t2) - dble(t1) )/ dble(clock_rate)
!
!  Finishing (1)
!
   write(*,*) "1) subnexper:"
   write(*,*) 'Total permutations :', ic
   write(*,*) 'Total time elapsed :', dt
!
!
!  (2) Starting:
!
   do i  =  1, nmax
      ida(i) = i
   enddo
!
   pos_min = 1
   pos_max = nmax
!
   ic = 0
   call system_clock ( t1, clock_rate, clock_max )
!
   call generate ( pos_min )
!
   call system_clock ( t2, clock_rate, clock_max )
   dt =  ( dble(t2) - dble(t1) )/ dble(clock_rate)
!
!  Finishing (2)
!
   write(*,*) "2) generate:"
   write(*,*) 'Total permutations :', ic
   write(*,*) 'Total time elapsed :', dt
!
!
!  (3) Starting:
!
   do i  =  1, nmax
      ida(i) = i
   enddo
!
   ic = 0
   call system_clock ( t1, clock_rate, clock_max )
!
   i = 1
   call perm ( i )
!
   call system_clock ( t2, clock_rate, clock_max )
   dt =  ( dble(t2) - dble(t1) )/ dble(clock_rate)
!
!  Finishing (3)
!
   write(*,*) "3) perm:"
   write(*,*) 'Total permutations :', ic
   write(*,*) 'Total time elapsed :', dt
!
!
!  (4) Starting:
!
   do i  =  1, nmax
      ida(i) = i
   enddo
!
   ic = 0
   call system_clock ( t1, clock_rate, clock_max )
!
   do
!
!     1) counting the number of permutatations
!
      ic = ic + 1
!
!     2) writing out the result:
!
!     do i  =  1, nmax
!        write (100,"(i3,',')",advance = "no") ida(i)
!     enddo
!     write(100,*)
!
!     repeat if not being finished yet, otherwise exit.
!
      if ( nextp(nmax,ida) ) then
         cycle
      else
         exit
      endif
!
   enddo
!
   call system_clock ( t2, clock_rate, clock_max )
   dt =  ( dble(t2) - dble(t1) )/ dble(clock_rate)
!
!  Finishing (4)
!
   write(*,*) "4) nextp:"
   write(*,*) 'Total permutations :', ic
   write(*,*) 'Total time elapsed :', dt
!
!
!  What's else?
!  ...
!
!==
   deallocate(ida)
!
   stop
!==
   contains
!==
!     Modified version of SUBROUTINE NEXPER from the book of
!     Albert Nijenhuis and Herbert S. Wilf, "Combinatorial
!     Algorithms For Computers and Calculators", 2nd Ed, p.59.
!
      subroutine subnexper ( n, a, mtc, even )
      implicit none
      integer,intent(in)    ::  n
      integer,dimension(n),intent(inout)  :: a
      logical,intent(inout) :: mtc, even
!
!     local varialbes:
!
      integer,save :: nm3
      integer :: ia, i, s, d, i1, l, j, m
!
      if (mtc) goto 10

      nm3 = n-3

      do i = 1,n
         a(i) = i
      enddo

      mtc  = .true.
5     even = .true.

      if ( n .eq. 1 ) goto 8

6     if ( a(n) .ne. 1 .or. a(1) .ne. 2+mod(n,2) ) return

      if ( n .le. 3 ) goto 8

      do i = 1,nm3
         if( a(i+1) .ne. a(i)+1 ) return
      enddo

8     mtc = .false.

      return

10    if ( n .eq. 1 ) goto 27

      if( .not. even ) goto 20

      ia   = a(1)
      a(1) = a(2)
      a(2) = ia
      even = .false.

      goto 6

20    s = 0

      do i1 = 2,n
         ia = a(i1)
         i = i1-1
         d = 0
         do j = 1,i
            if ( a(j) .gt. ia ) d = d+1
         enddo
         s = d+s
         if ( d .ne. i*mod(s,2) ) goto 35
      enddo

27    a(1) = 0

      goto 8

35    m = mod(s+1,2)*(n+1)

      do j = 1,i
         if(isign(1,a(j)-ia) .eq. isign(1,a(j)-m)) cycle
         m = a(j)
         l = j
      enddo

      a(l) = ia
      a(i1) = m
      even = .true.

      return
      end subroutine
!=====
!
!     http://rosettacode.org/wiki/Permutations#Fortran
!
      recursive subroutine generate (pos)

      implicit none
      integer,intent(in) :: pos
      integer :: val

      if (pos > pos_max) then
!
!        1) counting the number of permutatations
!
         ic = ic + 1
!
!        2) writing out the result:
!
!        write (*,*) permutation
!
      else
         do val = 1, nmax
            if (.not. any (ida( : pos-1) == val)) then
               ida(pos) = val
               call generate (pos + 1)
            endif
         enddo
      endif

      end subroutine
!=====
!
!     http://rosettacode.org/wiki/Permutations#Fortran
!
      recursive subroutine perm (i)
      implicit none
      integer,intent(inout) :: i
!
      integer :: j, t, ip1
!
      if (i == nmax) then
!
!        1) couting the number of permutatations
!
         ic = ic + 1
!
!        2) writing out the result:
!
!        write (*,*) a
!
      else
         ip1 = i+1
         do j = i, nmax
            t = ida(i)
            ida(i) = ida(j)
            ida(j) = t
            call perm ( ip1 )
            t = ida(i)
            ida(i) = ida(j)
            ida(j) = t
         enddo
      endif
      return
      end subroutine
!=====
!
!     http://rosettacode.org/wiki/Permutations#Fortran
!
      function nextp ( n, a )
      logical :: nextp
      integer,intent(in) :: n
      integer,dimension(n),intent(inout) :: a
!
!     local variables:
!
      integer i,j,k,t
!
      i = n-1
   10 if ( a(i) .lt. a(i+1) ) goto 20
      i = i-1
      if ( i .eq. 0 ) goto 20
      goto 10
   20 j = i+1
      k = n
   30 t = a(j)
      a(j) = a(k)
      a(k) = t
      j = j+1
      k = k-1
      if ( j .lt. k ) goto 30
      j = i
      if (j .ne. 0 ) goto 40
!
      nextp = .false.
!
      return
!
   40 j = j+1
      if ( a(j) .lt. a(i) ) goto 40
      t = a(i)
      a(i) = a(j)
      a(j) = t
!
      nextp = .true.
!
      return
      end function
!=====
!
!     What's else ?
!     ...

!=====
   end program
```


An example of performance:

1) Compiled with GNU fortran compiler:

gfortran -O3  testing_permutation_algorithms.f90 ; ./a.out

 INPUT N:
10
 N =          10
 1) subnexper:
 Total permutations :              3628800
 Total time elapsed :   4.9000000000000002E-002
 2) generate:
 Total permutations :              3628800
 Total time elapsed :  0.84299999999999997
 3) perm:
 Total permutations :              3628800
 Total time elapsed :   5.6000000000000001E-002
 4) nextp:
 Total permutations :              3628800
 Total time elapsed :   2.9999999999999999E-002

b) Compiled with Intel compiler:

ifort -O3  testing_permutation_algorithms.f90 ; ./a.out

INPUT N:
10
 N =          10
 1) subnexper:
 Total permutations :               3628800
 Total time elapsed :  8.240000000000000E-002
 2) generate:
 Total permutations :               3628800
 Total time elapsed :  0.616200000000000
 3) perm:
 Total permutations :               3628800
 Total time elapsed :  5.760000000000000E-002
 4) nextp:
 Total permutations :               3628800
 Total time elapsed :  3.600000000000000E-002

So far, we have conclusion from the above performance:
1) subnexper is the 3rd fast with ifort and the 2nd with gfortran.
2) generate is the slowest one with not only ifort but gfortran.
3) perm is the 2nd fast one with ifort and the 3rd one with gfortran.
4) nextp is the fastest one with both ifort and gfortran (the winner in this test).

Note: It is worth mentioning that the performance of this test is dependent not only on algorithm, but also on computer where the test runs. Therefore we should run the test on our own computer and make conclusion by ourselves.


###  Fortran 77

Here is an alternate, iterative version in Fortran 77.
```fortran
      program nptest
      integer n,i,a
      logical nextp
      external nextp
      parameter(n=4)
      dimension a(n)
      do i=1,n
      a(i)=i
      enddo
   10 print *,(a(i),i=1,n)
      if(nextp(n,a)) go to 10
      end

      function nextp(n,a)
      integer n,a,i,j,k,t
      logical nextp
      dimension a(n)
      i=n-1
   10 if(a(i).lt.a(i+1)) go to 20
      i=i-1
      if(i.eq.0) go to 20
      go to 10
   20 j=i+1
      k=n
   30 t=a(j)
      a(j)=a(k)
      a(k)=t
      j=j+1
      k=k-1
      if(j.lt.k) go to 30
      j=i
      if(j.ne.0) go to 40
      nextp=.false.
      return
   40 j=j+1
      if(a(j).lt.a(i)) go to 40
      t=a(i)
      a(i)=a(j)
      a(j)=t
      nextp=.true.
      end
```


## FreeBASIC


```freebasic
' version 07-04-2017
' compile with: fbc -s console

' Heap's algorithm non-recursive
Sub perms(n As Long)

    Dim As ULong i, j, count = 1
    Dim As ULong a(0 To n -1), c(0 To n -1)

    For j = 0 To n -1
        a(j) = j +1
        Print a(j);
    Next
    Print " ";

    i = 0
    While i < n
        If c(i) < i Then
            If (i And 1) = 0 Then
                Swap a(0), a(i)
            Else
                Swap a(c(i)), a(i)
            End If
            For j = 0 To n -1
                Print a(j);
            Next
            count += 1
            If count = 12 Then
                Print
                count = 0
            Else
                Print " ";
            End If
            c(i) += 1
            i = 0
        Else
            c(i) = 0
            i += 1
        End If
    Wend

End Sub

' ------=< MAIN >=------

perms(4)

' empty keyboard buffer
While Inkey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

```txt
1234 2134 3124 1324 2314 3214 4213 2413 1423 4123 2143 1243
1342 3142 4132 1432 3412 4312 4321 3421 2431 4231 3241 2341
```



## GAP

GAP can handle permutations and groups. Here is a straightforward implementation : for each permutation p in S(n) (symmetric group),
compute the images of 1 .. n by p. As an alternative, List(SymmetricGroup(n)) would yield the permutations as GAP ''Permutation'' objects,
which would probably be more manageable in later computations.

```gap>gap
List(SymmetricGroup(4), p -> Permuted([1 .. 4], p));
perms(4);
[ [ 1, 2, 3, 4 ], [ 4, 2, 3, 1 ], [ 2, 4, 3, 1 ], [ 3, 2, 4, 1 ], [ 1, 4, 3, 2 ], [ 4, 1, 3, 2 ], [ 2, 1, 3, 4 ],
  [ 3, 1, 4, 2 ], [ 1, 3, 4, 2 ], [ 4, 3, 1, 2 ], [ 2, 3, 1, 4 ], [ 3, 4, 1, 2 ], [ 1, 2, 4, 3 ], [ 4, 2, 1, 3 ],
  [ 2, 4, 1, 3 ], [ 3, 2, 1, 4 ], [ 1, 4, 2, 3 ], [ 4, 1, 2, 3 ], [ 2, 1, 4, 3 ], [ 3, 1, 2, 4 ], [ 1, 3, 2, 4 ],
  [ 4, 3, 2, 1 ], [ 2, 3, 4, 1 ], [ 3, 4, 2, 1 ] ]
```

GAP has also built-in functions to get permutations

```gap
# All arrangements of 4 elements in 1 .. 4
Arrangements([1 .. 4], 4);
# All permutations of 1 .. 4
PermutationsList([1 .. 4]);
```

Here is an implementation using a function to compute next permutation in lexicographic order:

```gap
NextPermutation := function(a)
   local i, j, k, n, t;
   n := Length(a);
   i := n - 1;
   while i > 0 and a[i] > a[i + 1] do
      i := i - 1;
   od;
   j := i + 1;
   k := n;
   while j < k do
      t := a[j];
      a[j] := a[k];
      a[k] := t;
      j := j + 1;
      k := k - 1;
   od;
   if i = 0 then
      return false;
   else
      j := i + 1;
      while a[j] < a[i] do
         j := j + 1;
      od;
      t := a[i];
      a[i] := a[j];
      a[j] := t;
      return true;
   fi;
end;

Permutations := function(n)
   local a, L;
   a := List([1 .. n], x -> x);
   L := [ ];
   repeat
      Add(L, ShallowCopy(a));
   until not NextPermutation(a);
   return L;
end;

Permutations(3);
[ [ 1, 2, 3 ], [ 1, 3, 2 ],
  [ 2, 1, 3 ], [ 2, 3, 1 ],
  [ 3, 1, 2 ], [ 3, 2, 1 ] ]
```




## Glee


```glee
$$ n !! k    dyadic: Permutations for k out of n elements (in this case k = n)
$$ #s        monadic: number of elements in s
$$ ,,        monadic: expose with space-lf separators
$$ s[n]      index n of s

'Hello' 123 7.9 '•'=>s;
s[s# !! (s#)],,
```


Result:

```glee
Hello 123 7.9 •
Hello 123 • 7.9
Hello 7.9 123 •
Hello 7.9 • 123
Hello • 123 7.9
Hello • 7.9 123
123 Hello 7.9 •
123 Hello • 7.9
123 7.9 Hello •
123 7.9 • Hello
123 • Hello 7.9
123 • 7.9 Hello
7.9 Hello 123 •
7.9 Hello • 123
7.9 123 Hello •
7.9 123 • Hello
7.9 • Hello 123
7.9 • 123 Hello
• Hello 123 7.9
• Hello 7.9 123
• 123 Hello 7.9
• 123 7.9 Hello
• 7.9 Hello 123
• 7.9 123 Hello
```



## Go



###  recursive



```go
package main

import "fmt"

func main() {
    demoPerm(3)
}

func demoPerm(n int) {
    // create a set to permute.  for demo, use the integers 1..n.
    s := make([]int, n)
    for i := range s {
        s[i] = i + 1
    }
    // permute them, calling a function for each permutation.
    // for demo, function just prints the permutation.
    permute(s, func(p []int) { fmt.Println(p) })
}

// permute function.  takes a set to permute and a function
// to call for each generated permutation.
func permute(s []int, emit func([]int)) {
    if len(s) == 0 {
        emit(s)
        return
    }
    // Steinhaus, implemented with a recursive closure.
    // arg is number of positions left to permute.
    // pass in len(s) to start generation.
    // on each call, weave element at pp through the elements 0..np-2,
    // then restore array to the way it was.
    var rc func(int)
    rc = func(np int) {
        if np == 1 {
            emit(s)
            return
        }
        np1 := np - 1
        pp := len(s) - np1
        // weave
        rc(np1)
        for i := pp; i > 0; i-- {
            s[i], s[i-1] = s[i-1], s[i]
            rc(np1)
        }
        // restore
        w := s[0]
        copy(s, s[1:pp+1])
        s[pp] = w
    }
    rc(len(s))
}
```

```txt
[1 2 3]
[1 3 2]
[3 1 2]
[2 1 3]
[2 3 1]
[3 2 1]
```


=== non-recursive, lexicographical order ===


```go
package main

import "fmt"

func main() {
        var a = []int{1, 2, 3}
        fmt.Println(a)
        var n = len(a) - 1
        var i, j int
        for c := 1; c < 6; c++ { // 3! = 6:
                i = n - 1
                j = n
                for a[i] > a[i+1] {
                        i--
                }
                for a[j] < a[i] {
                        j--
                }
                a[i], a[j] = a[j], a[i]
                j = n
                i += 1
                for i < j {
                        a[i], a[j] = a[j], a[i]
                        i++
                        j--
                }
                fmt.Println(a)
        }
}
```


```txt
[1 2 3]
[1 3 2]
[2 1 3]
[2 3 1]
[3 1 2]
[3 2 1]

```



## Groovy

Solution:

```groovy
def makePermutations = { l -> l.permutations() }
```

Test:

```groovy
def list = ['Crosby', 'Stills', 'Nash', 'Young']
def permutations = makePermutations(list)
assert permutations.size() == (1..<(list.size()+1)).inject(1) { prod, i -> prod*i }
permutations.each { println it }
```

<pre style="height:30ex;overflow:scroll;">[Young, Crosby, Stills, Nash]
[Crosby, Stills, Young, Nash]
[Nash, Crosby, Young, Stills]
[Stills, Nash, Crosby, Young]
[Young, Stills, Crosby, Nash]
[Stills, Crosby, Nash, Young]
[Stills, Crosby, Young, Nash]
[Stills, Young, Nash, Crosby]
[Nash, Stills, Young, Crosby]
[Crosby, Young, Nash, Stills]
[Crosby, Nash, Young, Stills]
[Crosby, Nash, Stills, Young]
[Nash, Young, Stills, Crosby]
[Young, Nash, Stills, Crosby]
[Nash, Young, Crosby, Stills]
[Young, Stills, Nash, Crosby]
[Crosby, Stills, Nash, Young]
[Stills, Young, Crosby, Nash]
[Young, Nash, Crosby, Stills]
[Nash, Stills, Crosby, Young]
[Young, Crosby, Nash, Stills]
[Nash, Crosby, Stills, Young]
[Crosby, Young, Stills, Nash]
[Stills, Nash, Young, Crosby]

```



## Haskell


```haskell
import Data.List (permutations)

main = mapM_ print (permutations [1,2,3])
```


A simple implementation, that assumes elements are unique and support equality:

```haskell
import Data.List (delete)

permutations :: Eq a => [a] -> [[a]]
permutations [] = [[]]
permutations xs = [ x:ys | x <- xs, ys <- permutations (delete x xs)]
```


A slightly more efficient implementation that doesn't have the above restrictions:

```haskell
permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations xs = [ y:zs | (y,ys) <- select xs, zs <- permutations ys]
  where select []     = []
        select (x:xs) = (x,xs) : [ (y,x:ys) | (y,ys) <- select xs ]
```


The above are all selection-based approaches. The following is an insertion-based approach:

```haskell
permutations :: [a] -> [[a]]
permutations = foldr (concatMap . insertEverywhere) [[]]
  where insertEverywhere :: a -> [a] -> [[a]]
        insertEverywhere x [] = [[x]]
        insertEverywhere x l@(y:ys) = (x:l) : map (y:) (insertEverywhere x ys)
```


A serialized version:
```haskell
permutations :: [a] -> [[a]]
permutations =
  foldr (\x ac -> ac >>= (fmap . ins x) <*> (enumFromTo 0 . length)) [[]]
  where
    ins x xs n =
      let (a, b) = splitAt n xs
      in a ++ x : b

main :: IO ()
main = print $ permutations [1, 2, 3]
```

```txt
[[1,2,3],[2,3,1],[3,1,2],[2,1,3],[1,3,2],[3,2,1]]
```


=={{header|Icon}} and {{header|Unicon}}==

```unicon
procedure main(A)
    every p := permute(A) do every writes((!p||" ")|"\n")
end

procedure permute(A)
    if *A <= 1 then return A
    suspend [(A[1]<->A[i := 1 to *A])] ||| permute(A[2:0])
end
```

```txt
->permute Aardvarks eat ants
Aardvarks eat ants
Aardvarks ants eat
eat Aardvarks ants
eat ants Aardvarks
ants eat Aardvarks
ants Aardvarks eat
->
```


=={{header|IS-BASIC}}==
<lang IS-BASIC>100 PROGRAM "Permutat.bas"
110 LET N=4 ! Number of elements
120 NUMERIC T(1 TO N)
130 FOR I=1 TO N
140   LET T(I)=I
150 NEXT
160 LET S=0
170 CALL PERM(N)
180 PRINT "Number of permutations:";S
190 END
200 DEF PERM(I)
210   NUMERIC J,X
220   IF I=1 THEN
230     FOR X=1 TO N
240       PRINT T(X);
250     NEXT
260     PRINT :LET S=S+1
270   ELSE
280     CALL PERM(I-1)
290     FOR J=1 TO I-1
300       LET C=T(J):LET T(J)=T(I):LET T(I)=C
310       CALL PERM(I-1)
320       LET C=T(J):LET T(J)=T(I):LET T(I)=C
330     NEXT
340   END IF
350 END DEF
```



## J


```j
perms=: A.&i.~ !
```

```j
   perms 2
0 1
1 0
   ({~ perms@#)&.;: 'some random text'
some random text
some text random
random some text
random text some
text some random
text random some
```



## Java

Using the code of Michael Gilleland.

```java
public class PermutationGenerator {
    private int[] array;
    private int firstNum;
    private boolean firstReady = false;

    public PermutationGenerator(int n, int firstNum_) {
        if (n < 1) {
            throw new IllegalArgumentException("The n must be min. 1");
        }
        firstNum = firstNum_;
        array = new int[n];
        reset();
    }

    public void reset() {
        for (int i = 0; i < array.length; i++) {
            array[i] = i + firstNum;
        }
        firstReady = false;
    }

    public boolean hasMore() {
        boolean end = firstReady;
        for (int i = 1; i < array.length; i++) {
            end = end && array[i] < array[i-1];
        }
        return !end;
    }

    public int[] getNext() {

        if (!firstReady) {
            firstReady = true;
            return array;
        }

        int temp;
        int j = array.length - 2;
        int k = array.length - 1;

        // Find largest index j with a[j] < a[j+1]

        for (;array[j] > array[j+1]; j--);

        // Find index k such that a[k] is smallest integer
        // greater than a[j] to the right of a[j]

        for (;array[j] > array[k]; k--);

        // Interchange a[j] and a[k]

        temp = array[k];
        array[k] = array[j];
        array[j] = temp;

        // Put tail end of permutation after jth position in increasing order

        int r = array.length - 1;
        int s = j + 1;

        while (r > s) {
            temp = array[s];
            array[s++] = array[r];
            array[r--] = temp;
        }

        return array;
    } // getNext()

    // For testing of the PermutationGenerator class
    public static void main(String[] args) {
        PermutationGenerator pg = new PermutationGenerator(3, 1);

        while (pg.hasMore()) {
            int[] temp =  pg.getNext();
            for (int i = 0; i < temp.length; i++) {
                System.out.print(temp[i] + " ");
            }
            System.out.println();
        }
    }

} // class
```

```txt

1 2 3
1 3 2
2 1 3
2 3 1
3 1 2
3 2 1

```

'''optimized'''

Following needs: [[User:Margusmartsepp/Contributions/Java/Utils.java|Utils.java]]

```java
public class Permutations {
	public static void main(String[] args) {
		System.out.println(Utils.Permutations(Utils.mRange(1, 3)));
	}
}
```

```txt
[[1, 2, 3], [1, 3, 2], [2, 1, 3], [2, 3, 1], [3, 1, 2], [3, 2, 1]]
```



## JavaScript


### ES5


### =Iteration=


Copy the following as an HTML file and load in a browser.

```javascript><html><head><title>Permutations</title></head

<body><pre id="result">
```

<script type="text/javascript">
var d = document.getElementById('result');

function perm(list, ret)
{
    if (list.length == 0) {
        var row = document.createTextNode(ret.join(' ') + '\n');
        d.appendChild(row);
        return;
    }
    for (var i = 0; i < list.length; i++) {
        var x = list.splice(i, 1);
        ret.push(x);
        perm(list, ret);
        ret.pop();
        list.splice(i, 0, x);
    }
}

perm([1, 2, 'A', 4], []);
</script></body></html>
```


Alternatively: 'Genuine' js code, assuming no duplicate.


```JavaScript

function perm(a) {
    if (a.length < 2) return [a];
    var c, d, b = [];
    for (c = 0; c < a.length; c++) {
        var e = a.splice(c, 1),
            f = perm(a);
        for (d = 0; d < f.length; d++) b.push([e].concat(f[d]));
        a.splice(c, 0, e[0])
    } return b
}

console.log(perm(['Aardvarks', 'eat', 'ants']).join("\n"));

```


```JavaScript
Aardvarks,eat,ants
Aardvarks,ants,eat
eat,Aardvarks,ants
eat,ants,Aardvarks
ants,Aardvarks,eat
ants,eat,Aardvarks
```



### =Functional composition=


(Simple version – assuming a unique list of objects comparable by the JS === operator)


```JavaScript
(function () {
    'use strict';

    // permutations :: [a] -> [[a]]
    var permutations = function (xs) {
        return xs.length ? concatMap(function (x) {
            return concatMap(function (ys) {
                return [[x].concat(ys)];
            }, permutations(delete_(x, xs)));
        }, xs) : [[]];
    };

    // GENERIC FUNCTIONS

    // concatMap :: (a -> [b]) -> [a] -> [b]
    var concatMap = function (f, xs) {
        return [].concat.apply([], xs.map(f));
    };

    // delete :: Eq a => a -> [a] -> [a]
    var delete_ = function (x, xs) {
        return deleteBy(function (a, b) {
            return a === b;
        }, x, xs);
    };

    // deleteBy :: (a -> a -> Bool) -> a -> [a] -> [a]
    var deleteBy = function (f, x, xs) {
        return xs.length > 0 ? f(x, xs[0]) ? xs.slice(1) :
        [xs[0]].concat(deleteBy(f, x, xs.slice(1))) : [];
    };

    // TEST
    return permutations(['Aardvarks', 'eat', 'ants']);
})();
```


```JavaScript
[["Aardvarks", "eat", "ants"], ["Aardvarks", "ants", "eat"],
 ["eat", "Aardvarks", "ants"], ["eat", "ants", "Aardvarks"],
["ants", "Aardvarks", "eat"], ["ants", "eat", "Aardvarks"]]
```



### ES6

Recursively, in terms of concatMap and delete:

```JavaScript
(() => {
    'use strict';

    // permutations :: [a] -> [[a]]
    const permutations = xs => {
        const go = xs => xs.length ? (
            concatMap(
                x => concatMap(
                    ys => [[x].concat(ys)],
                    go(delete_(x, xs))), xs
                )
        ) : [[]];
        return go(xs);
    };

    // GENERIC FUNCTIONS ----------------------------------

    // concatMap :: (a -> [b]) -> [a] -> [b]
    const concatMap = (f, xs) =>
        xs.reduce((a, x) => a.concat(f(x)), []);


    // delete :: Eq a => a -> [a] -> [a]
    const delete_ = (x, xs) => {
        const go = xs => {
            return 0 < xs.length ? (
                (x === xs[0]) ? (
                    xs.slice(1)
                ) : [xs[0]].concat(go(xs.slice(1)))
            ) : [];
        }
        return go(xs);
    };

    // TEST
    return JSON.stringify(
        permutations(['Aardvarks', 'eat', 'ants'])
    );
})();
```

```JavaScript
[["Aardvarks", "eat", "ants"], ["Aardvarks", "ants", "eat"],
 ["eat", "Aardvarks", "ants"], ["eat", "ants", "Aardvarks"],
["ants", "Aardvarks", "eat"], ["ants", "eat", "Aardvarks"]]
```



Or, without recursion, in terms of concatMap and reduce:

```javascript
(() => {
    'use strict';

    // permutations :: [a] -> [[a]]
    const permutations = xs =>
        xs.reduceRight(
            (a, x) => concatMap(
                xs => enumFromTo(0, xs.length)
                .map(n => xs.slice(0, n)
                    .concat(x)
                    .concat(xs.slice(n))
                ),
                a
            ),
            [[]]
        );

    // GENERIC FUNCTIONS ----------------------------------

    // concatMap :: (a -> [b]) -> [a] -> [b]
    const concatMap = (f, xs) =>
        xs.reduce((a, x) => a.concat(f(x)), []);

    // ft :: Int -> Int -> [Int]
    const enumFromTo = (m, n) =>
        Array.from({
            length: 1 + n - m
        }, (_, i) => m + i);

    // showLog :: a -> IO ()
    const showLog = (...args) =>
        console.log(
            args
            .map(JSON.stringify)
            .join(' -> ')
        );

    // TEST -----------------------------------------------
    showLog(
        permutations([1, 2, 3])
    );
})();
```

```txt
[[1,2,3],[2,1,3],[2,3,1],[1,3,2],[3,1,2],[3,2,1]]
```



## jq

"permutations" generates a stream of the permutations of the input array.

```jq
def permutations:
  if length == 0 then []
  else
    range(0;length) as $i
    | [.[$i]] + (del(.[$i])|permutations)
  end ;

```

'''Example 1''': list them
 [range(0;3)] | permutations
 [0,1,2]
 [0,2,1]
 [1,0,2]
 [1,2,0]
 [2,0,1]
 [2,1,0]

'''Example 2''': count them
 [[range(0;3)] | permutations] | length
 6

Or more efficiently:

 def count(s): reduce s as $i (0;.+1);

 [range(0;3)] | count(permutations)
 6

'''Example 3''': 10!
 [range(0;10)] | count(permutations)
 3628800


## Julia

Julia has support for permutation creation and processing via the <tt>Combinatorics</tt> package. <tt>permutations(v)</tt> creates an iterator over all permutations of <tt>v</tt>. Julia 0.7 and 1.0+ require the line global i inside the for to update the i variable.

```Julia

using Combinatorics

term = "RCode"
i = 0
pcnt = factorial(length(term))
print("All the permutations of ", term, " (", pcnt, "):\n    ")
for p in permutations(split(term, ""))
    global i
    print(join(p), " ")
    i += 1
    i %= 12
    i != 0 || print("\n    ")
end
println()

```


```txt

All the permutations of RCode (120):
    RCode RCoed RCdoe RCdeo RCeod RCedo RoCde RoCed RodCe RodeC RoeCd RoedC
    RdCoe RdCeo RdoCe RdoeC RdeCo RdeoC ReCod ReCdo ReoCd ReodC RedCo RedoC
    CRode CRoed CRdoe CRdeo CReod CRedo CoRde CoRed CodRe CodeR CoeRd CoedR
    CdRoe CdReo CdoRe CdoeR CdeRo CdeoR CeRod CeRdo CeoRd CeodR CedRo CedoR
    oRCde oRCed oRdCe oRdeC oReCd oRedC oCRde oCRed oCdRe oCdeR oCeRd oCedR
    odRCe odReC odCRe odCeR odeRC odeCR oeRCd oeRdC oeCRd oeCdR oedRC oedCR
    dRCoe dRCeo dRoCe dRoeC dReCo dReoC dCRoe dCReo dCoRe dCoeR dCeRo dCeoR
    doRCe doReC doCRe doCeR doeRC doeCR deRCo deRoC deCRo deCoR deoRC deoCR
    eRCod eRCdo eRoCd eRodC eRdCo eRdoC eCRod eCRdo eCoRd eCodR eCdRo eCdoR
    eoRCd eoRdC eoCRd eoCdR eodRC eodCR edRCo edRoC edCRo edCoR edoRC edoCR

```


<lang>
# Generate all permutations of size t from an array a with possibly duplicated elements.
collect(Combinatorics.multiset_permutations([1,1,0,0,0],3))

```

```txt

7-element Array{Array{Int64,1},1}:
 [1, 1, 0]
 [1, 0, 1]
 [1, 0, 0]
 [0, 1, 1]
 [0, 1, 0]
 [0, 0, 1]
 [0, 0, 0]

```



## K

```K
   perm:{:[1<x;,/(>:'(x,x)#1,x#0)[;0,'1+_f x-1];,!x]}
   perm 2
(0 1
 1 0)

   `0:{1_,/" ",/:x}'r@perm@#r:("some";"random";"text")
some random text
some text random
random some text
random text some
text some random
text random some
```


Alternative:

```K

   perm:{x@m@&n=(#?:)'m:!n#n:#x}

   perm[!3]
(0 1 2
 0 2 1
 1 0 2
 1 2 0
 2 0 1
 2 1 0)

   perm "abc"
("abc"
 "acb"
 "bac"
 "bca"
 "cab"
 "cba")

   `0:{1_,/" ",/: $x}' perm `$" "\"some random text"
some random text
some text random
random some text
random text some
text some random
text random some

```



## Kotlin

Translation of C# recursive 'insert' solution in Wikipedia article on Permutations:

```scala
// version 1.1.2

fun <T> permute(input: List<T>): List<List<T>> {
    if (input.size == 1) return listOf(input)
    val perms = mutableListOf<List<T>>()
    val toInsert = input[0]
    for (perm in permute(input.drop(1))) {
        for (i in 0..perm.size) {
            val newPerm = perm.toMutableList()
            newPerm.add(i, toInsert)
            perms.add(newPerm)
        }
    }
    return perms
}

fun main(args: Array<String>) {
    val input = listOf('a', 'b', 'c', 'd')
    val perms = permute(input)
    println("There are ${perms.size} permutations of $input, namely:\n")
    for (perm in perms) println(perm)
}
```


```txt

There are 24 permutations of [a, b, c, d], namely:

[a, b, c, d]
[b, a, c, d]
[b, c, a, d]
[b, c, d, a]
[a, c, b, d]
[c, a, b, d]
[c, b, a, d]
[c, b, d, a]
[a, c, d, b]
[c, a, d, b]
[c, d, a, b]
[c, d, b, a]
[a, b, d, c]
[b, a, d, c]
[b, d, a, c]
[b, d, c, a]
[a, d, b, c]
[d, a, b, c]
[d, b, a, c]
[d, b, c, a]
[a, d, c, b]
[d, a, c, b]
[d, c, a, b]
[d, c, b, a]

```



## Langur

This follows the Go language non-recursive example, but is not limited to integers, or even to numbers.

```Langur
val .factorial = f if(.x < 2: 1; .x x self(.x - 1))

val .permute = f(.arr) {
    if not isArray(.arr) {
        throw "expected array"
    }

    val .limit = 10
    if len(.arr) > .limit {
        throw $"permutation limit exceeded (currently \.limit;)"
    }

    var .elements = .arr
    var .ordinals = series len .elements
    var .arrOf = [.arr]

    val .n = len(.ordinals)
    var (.i, .j)

    for of .factorial(len .arr)-1 {
        .i = .n - 1
        .j = .n
        for .ordinals[.i] > .ordinals[.i+1] {
            .i -= 1
        }
        for .ordinals[.j] < .ordinals[.i] {
            .j -=1
        }

        (.ordinals[.i], .ordinals[.j]) = (.ordinals[.j], .ordinals[.i])
        (.elements[.i], .elements[.j]) = (.elements[.j], .elements[.i])

        .j = .n
        .i += 1
        for ; .i < .j; .i+=1, .j-=1 {
            (.ordinals[.i], .ordinals[.j]) = (.ordinals[.j], .ordinals[.i])
            (.elements[.i], .elements[.j]) = (.elements[.j], .elements[.i])
        }
        .arrOf = more .arrOf, .elements
    }

    return .arrOf
}

for .e in .permute([1, 3.14, 7]) {
    writeln .e
}
```


```txt
[1, 3.14, 7]
[1, 7, 3.14]
[3.14, 1, 7]
[3.14, 7, 1]
[7, 1, 3.14]
[7, 3.14, 1]
```



## LFE



```lisp

(defun permute
  (('())
    '(()))
  ((l)
    (lc ((<- x l)
         (<- y (permute (-- l `(,x)))))
        (cons x y))))

```

REPL usage:

```lisp

> (permute '(1 2 3))
((1 2 3) (1 3 2) (2 1 3) (2 3 1) (3 1 2) (3 2 1))

```



## Liberty BASIC

Permuting numerical array (non-recursive):
```lb

n=3
dim a(n+1)  '+1 needed due to bug in LB that checks loop condition
    '   until (i=0) or (a(i)<a(i+1))
    'before executing i=i-1 in loop body.
for i=1 to n: a(i)=i: next
do
  for i=1 to n: print a(i);: next: print
  i=n
  do
    i=i-1
  loop until (i=0) or (a(i)<a(i+1))
  j=i+1
  k=n
  while j<k
    'swap a(j),a(k)
    tmp=a(j): a(j)=a(k): a(k)=tmp
    j=j+1
    k=k-1
  wend
  if i>0 then
    j=i+1
    while a(j)<a(i)
      j=j+1
    wend
    'swap a(i),a(j)
    tmp=a(j): a(j)=a(i): a(i)=tmp
  end if
loop until i=0

```


```txt

123
132
213
231
312
321

```

Permuting string (recursive):

```lb

n = 3

s$=""
for i = 1 to n
    s$=s$;i
next

res$=permutation$("", s$)

Function permutation$(pre$, post$)
    lgth = Len(post$)
    If lgth < 2 Then
        print pre$;post$
    Else
        For i = 1 To lgth
            tmp$=permutation$(pre$+Mid$(post$,i,1),Left$(post$,i-1)+Right$(post$,lgth-i))
        Next i
    End If
End Function


```


```txt

123
132
213
231
312
321

```


## Logtalk


```logtalk
:- object(list).

    :- public(permutation/2).

    permutation(List, Permutation) :-
        same_length(List, Permutation),
        permutation2(List, Permutation).

    permutation2([], []).
    permutation2(List, [Head| Tail]) :-
        select(Head, List, Remaining),
        permutation2(Remaining, Tail).

    same_length([], []).
    same_length([_| Tail1], [_| Tail2]) :-
        same_length(Tail1, Tail2).

    select(Head, [Head| Tail], Tail).
    select(Head, [Head2| Tail], [Head2| Tail2]) :-
        select(Head, Tail, Tail2).

:- end_object.
```

```logtalk
| ?- forall(list::permutation([1, 2, 3], Permutation), (write(Permutation), nl)).

[1,2,3]
[1,3,2]
[2,1,3]
[2,3,1]
[3,1,2]
[3,2,1]
yes
```



## Lua



```lua

local function permutation(a, n, cb)
	if n == 0 then
		cb(a)
	else
		for i = 1, n do
			a[i], a[n] = a[n], a[i]
			permutation(a, n - 1, cb)
			a[i], a[n] = a[n], a[i]
		end
	end
end

--Usage
local function callback(a)
	print('{'..table.concat(a, ', ')..'}')
end
permutation({1,2,3}, 3, callback)

```

```txt

{2, 3, 1}
{3, 2, 1}
{3, 1, 2}
{1, 3, 2}
{2, 1, 3}
{1, 2, 3}

```



```lua


-- Iterative version
function ipermutations(a,b)
    if a==0 then return end
    local taken = {} local slots = {}
    for i=1,a do slots[i]=0 end
    for i=1,b do taken[i]=false end
    local index = 1
    while index > 0 do repeat
        repeat slots[index] = slots[index] + 1
        until slots[index] > b or not taken[slots[index]]
        if slots[index] > b then
            slots[index] = 0
            index = index - 1
            if index > 0 then
                taken[slots[index]] = false
            end
            break
        else
            taken[slots[index]] = true
        end
        if index == a then
            for i=1,a do io.write(slots[i]) io.write(" ") end
            io.write("\n")
            taken[slots[index]] = false
            break
        end
        index = index + 1
    until true end
end

ipermutations(3, 3)

```



```txt

1 2 3
1 3 2
2 1 3
2 3 1
3 1 2
3 2 1

```



## M2000 Interpreter


### All permutations in one module


```M2000 Interpreter

Module Checkit {
      Global a$
      Document a$
      Module Permutations (s){
            Module Level (n, s, h)   {
                  If n=1 then {
                        while Len(s) {
                              m1=each(h)
                              while m1 {
                                    Print Array$(m1);" ";
                              }
                               Print Array$(S)
                               ToClipBoard()
                               s=cdr(s)
                         }
                  } Else {
                        for i=1 to len(s) {
                              call Level n-1, cdr(s),  cons(h, car(s))
                              s=cons(cdr(s), car(s))
                        }
                  }
                  Sub ToClipBoard()
                        local m=each(h)
                        Local b$=""
                        While m {
                              b$+=If$(Len(b$)<>0->" ","")+Array$(m)+" "
                        }
                        b$+=If$(Len(b$)<>0->" ","")+Array$(s,0)+" "+{
                        }
                        a$<=b$   ' assign to global need <=
                  End Sub
            }
            If len(s)=0 then Error
            Head=(,)
            Call Level Len(s),  s, Head
      }
      Clear a$
      Permutations (1,2,3,4)
      Permutations (100, 200, 500)
      Permutations ("A", "B", "C","D")
      Permutations ("DOG", "CAT", "BAT")
      ClipBoard a$
}
Checkit

```


### Step by step Generator


```M2000 Interpreter

Module StepByStep {
      Function PermutationStep (a) {
            c1=lambda (&f, a) ->{
                  =car(a)
                  f=true
            }
            m=len(a)
            c=c1
            while m>1 {
                  c1=lambda c2=c,p, m=(,) (&f, a) ->{
                        if len(m)=0 then m=a
                        =cons(car(m),c2(&f, cdr(m)))
                        if f then f=false:p++:  m=cons(cdr(m), car(m)) : if p=len(m) then p=0 : m=(,):: f=true
                  }
                  c=c1
                  m--
            }
            =lambda c, a (&f) -> {
                  =c(&f, a)
            }
      }
      k=false
      StepA=PermutationStep((1,2,3,4))
      while not k {
                 Print StepA(&k)
      }
      k=false
      StepA=PermutationStep((100,200,300))
      while not k {
                 Print StepA(&k)
      }
      k=false
      StepA=PermutationStep(("A", "B", "C", "D"))
      while not k {
                 Print StepA(&k)
      }
      k=false
      StepA=PermutationStep(("DOG", "CAT", "BAT"))
      while not k {
                 Print StepA(&k)
      }
}
StepByStep


```

<pre style="height:30ex;overflow:scroll">
1  2  3  4
1  2  4  3
1  3  4  2
1  3  2  4
1  4  2  3
1  4  3  2
2  3  4  1
2  3  1  4
2  4  1  3
2  4  3  1
2  1  3  4
2  1  4  3
3  4  1  2
3  4  2  1
3  1  2  4
3  1  4  2
3  2  4  1
3  2  1  4
4  1  2  3
4  1  3  2
4  2  3  1
4  2  1  3
4  3  1  2
4  3  2  1
100  200  500
100  500  200
200  500  100
200  100  500
500  100  200
500  200  100
A  B  C  D
A  B  D  C
A  C  D  B
A  C  B  D
A  D  B  C
A  D  C  B
B  C  D  A
B  C  A  D
B  D  A  C
B  D  C  A
B  A  C  D
B  A  D  C
C  D  A  B
C  D  B  A
C  A  B  D
C  A  D  B
C  B  D  A
C  B  A  D
D  A  B  C
D  A  C  B
D  B  C  A
D  B  A  C
D  C  A  B
D  C  B  A
DOG  CAT  BAT
DOG  BAT  CAT
CAT  BAT  DOG
CAT  DOG  BAT
BAT  DOG  CAT
BAT  CAT  DOG

</pre >


## Maple


```Maple

> combinat:-permute( 3 );
   [[1, 2, 3], [1, 3, 2], [2, 1, 3], [2, 3, 1], [3, 1, 2], [3, 2, 1]]

> combinat:-permute( [a,b,c] );
   [[a, b, c], [a, c, b], [b, a, c], [b, c, a], [c, a, b], [c, b, a]]

```



## Mathematica

Note: The built-in version will have better performance.


### Version from scratch



```Mathematica

(***Standard list functions:*)
fold[f_, x_, {}] := x
fold[f_, x_, {h_, t___}] := fold[f, f[x, h], {t}]
insert[L_, x_, n_] := Join[L[[;; n - 1]], {x}, L[[n ;;]]]

(***Generate all permutations of a list S:*)

permutations[S_] :=
 fold[Join @@ (Function[{L},
       Table[insert[L, #2, k + 1], {k, 0, Length[L]}]] /@ #1) &, {{}},
   S]

```


```txt
{{4, 3, 2, 1}, {3, 4, 2, 1}, {3, 2, 4, 1}, {3, 2, 1, 4}, {4, 2, 3,
  1}, {2, 4, 3, 1}, {2, 3, 4, 1}, {2, 3, 1, 4}, {4, 2, 1, 3}, {2, 4,
  1, 3}, {2, 1, 4, 3}, {2, 1, 3, 4}, {4, 3, 1, 2}, {3, 4, 1, 2}, {3,
  1, 4, 2}, {3, 1, 2, 4}, {4, 1, 3, 2}, {1, 4, 3, 2}, {1, 3, 4,
  2}, {1, 3, 2, 4}, {4, 1, 2, 3}, {1, 4, 2, 3}, {1, 2, 4, 3}, {1, 2,
  3, 4}}
```


===Built-in version===

```Mathematica
Permutations[{1,2,3,4}]
```

```txt
{{1, 2, 3, 4}, {1, 2, 4, 3}, {1, 3, 2, 4}, {1, 3, 4, 2}, {1, 4, 2, 3}, {1, 4, 3, 2}, {2, 1, 3, 4}, {2, 1, 4, 3}, {2, 3, 1, 4}, {2, 3,
  4, 1}, {2, 4, 1, 3}, {2, 4, 3, 1}, {3, 1, 2, 4}, {3, 1, 4, 2}, {3, 2, 1, 4}, {3, 2, 4, 1}, {3, 4, 1, 2}, {3, 4, 2, 1}, {4, 1, 2,
  3}, {4, 1, 3, 2}, {4, 2, 1, 3}, {4, 2, 3, 1}, {4, 3, 1, 2}, {4, 3, 2, 1}}
```


=={{header|MATLAB}} / {{header|Octave}}==

```MATLAB
perms([1,2,3,4])
```

```txt
4321
4312
4231
4213
4123
4132
3421
3412
3241
3214
3124
3142
2341
2314
2431
2413
2143
2134
1324
1342
1234
1243
1423
1432
```



## Maxima


```maxima
next_permutation(v) := block([n, i, j, k, t],
   n: length(v), i: 0,
   for k: n - 1 thru 1 step -1 do (if v[k] < v[k + 1] then (i: k, return())),
   j: i + 1, k: n,
   while j < k do (t: v[j], v[j]: v[k], v[k]: t, j: j + 1, k: k - 1),
   if i = 0 then return(false),
   j: i + 1,
   while v[j] < v[i] do j: j + 1,
   t: v[j], v[j]: v[i], v[i]: t,
   true
)$

print_perm(n) := block([v: makelist(i, i, 1, n)],
   disp(v),
   while next_permutation(v) do disp(v)
)$

print_perm(3);
/* [1, 2, 3]
   [1, 3, 2]
   [2, 1, 3]
   [2, 3, 1]
   [3, 1, 2]
   [3, 2, 1] */
```



### Builtin version


```maxima

(%i1) permutations([1, 2, 3]);
(%o1) {[1, 2, 3], [1, 3, 2], [2, 1, 3], [2, 3, 1], [3, 1, 2], [3, 2, 1]}

```



## Mercury


```mercury

:- module permutations2.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.


:- import_module list.
:- import_module set_ordlist.
:- import_module set.
:- import_module solutions.

%% permutationSet(List, Set) is true if List is a permutation of Set:
:- pred permutationSet(list(A)::out,set(A)::in) is nondet.

%% Two ways to compute all permutations of a given list (using backtracking):
:- func all_permutations1(list(int))=set_ordlist.set_ordlist(list(int)).
:- func all_permutations2(list(int))=set_ordlist.set_ordlist(list(int)).

:- implementation.


permutationSet([],set.init).
permutationSet([H|T], S) :- set.member(H,S), permutationSet(T,set.delete(S,H)).

all_permutations1(L) =
    solutions_set(pred(X::out) is nondet:-permutationSet(X,set.from_list(L))).

%%Alternatively, using the imported list.perm predicate:
all_permutations2(L) =
    solutions_set(pred(X::out) is nondet:-perm(L,X)).

main(!IO) :-
    print(all_permutations1([1,2,3,4]),!IO),
    nl(!IO),
    print(all_permutations2([1,2,3,4]),!IO).

```


```txt
>./permutations2

sol([[1, 2, 3, 4], [1, 2, 4, 3], [1, 3, 2, 4], [1, 3, 4, 2], [1, 4, 2, 3], [1, 4, 3, 2], [2, 1, 3, 4], [2, 1, 4, 3], [2, 3, 1, 4], [2, 3, 4, 1], [2, 4, 1, 3], [2, 4, 3, 1], [3, 1, 2, 4], [3, 1, 4, 2], [3, 2, 1, 4], [3, 2, 4, 1], [3, 4, 1, 2], [3, 4, 2, 1], [4, 1, 2, 3], [4, 1, 3, 2], [4, 2, 1, 3], [4, 2, 3, 1], [4, 3, 1, 2], [4, 3, 2, 1]])
sol([[1, 2, 3, 4], [1, 2, 4, 3], [1, 3, 2, 4], [1, 3, 4, 2], [1, 4, 2, 3], [1, 4, 3, 2], [2, 1, 3, 4], [2, 1, 4, 3], [2, 3, 1, 4], [2, 3, 4, 1], [2, 4, 1, 3], [2, 4, 3, 1], [3, 1, 2, 4], [3, 1, 4, 2], [3, 2, 1, 4], [3, 2, 4, 1], [3, 4, 1, 2], [3, 4, 2, 1], [4, 1, 2, 3], [4, 1, 3, 2], [4, 2, 1, 3], [4, 2, 3, 1], [4, 3, 1, 2], [4, 3, 2, 1]]) 

```



## Microsoft Small Basic

```smallbasic
'Permutations - sb
  n=4
  printem = "True"
  For i = 1 To n
    p[i] = i
  EndFor
  count = 0
  Last = "False"
  While Last = "False"
    If printem Then
      For t = 1 To n
        TextWindow.Write(p[t])
      EndFor
      TextWindow.WriteLine("")
    EndIf
    count = count + 1
    Last = "True"
    i = n - 1
    While i > 0
      If p[i] < p[i + 1] Then
        Last = "False"
        Goto exitwhile
      EndIf
      i = i - 1
    EndWhile
    exitwhile:
    j = i + 1
    k = n
    While j < k
      t = p[j]
      p[j] = p[k]
      p[k] = t
      j = j + 1
      k = k - 1
    EndWhile
    j = n
    While p[j] > p[i]
      j = j - 1
    EndWhile
    j = j + 1
    t = p[i]
    p[i] = p[j]
    p[j] = t
  EndWhile
  TextWindow.WriteLine("Number of permutations: "+count)
```

```txt

1234
1243
1324
1342
1423
1432
2134
2143
2314
2341
2413
2431
3124
3142
3214
3241
3412
3421
4123
4132
4213
4231
4312
4321
Number of permutations: 24

```


=={{header|Modula-2}}==
<lang Modula-2>MODULE 	Permute;

FROM	Terminal
IMPORT	Read, Write, WriteLn;

FROM	Terminal2
IMPORT	WriteString;

CONST	MAXIDX = 6;
	MINIDX = 1;

TYPE	TInpCh = ['a'..'z'];
	TChr   = SET OF TInpCh;

VAR	n,
	nl:	INTEGER;
	ch:	CHAR;
	a:	ARRAY[MINIDX..MAXIDX] OF CHAR;
	kt:     TChr = TChr{'a'..'f'};

PROCEDURE output;
VAR	i:	INTEGER;
BEGIN
	FOR i := MINIDX TO n DO Write(a[i]) END;
	WriteString(" | ");
END output;

PROCEDURE exchange(VAR x, y : CHAR);
VAR	z:	CHAR;
BEGIN z := x; x := y; y := z
END exchange;

PROCEDURE permute(k: INTEGER);
VAR	i:	INTEGER;
BEGIN
	IF k = 1 THEN
		output;
		INC(nl);
		IF (nl MOD 8 = 1) THEN WriteLn END;
	ELSE
		permute(k-1);
		FOR i := MINIDX TO k-1 DO
			exchange(a[i], a[k]);
			permute(k-1);
			exchange(a[i], a[k]);
		END
	END
END permute;

BEGIN
	n := 0;	nl := 1; WriteString("Input {a,b,c,d,e,f} >");
	REPEAT
		Read(ch);
		IF ch IN kt THEN INC(n); a[n] := ch; Write(ch) END
	UNTIL (ch <= " ") OR (n > MAXIDX);

	WriteLn;
	IF n > 0 THEN permute(n) END;
	(*Wait*)
END Permute.
```


=={{header|Modula-3}}==

=
## Simple version
=
This implementation merely prints out the orbit of the list (1, 2, ..., n) under the action of <i>S<sub>n</sub></i>. It shows off Modula-3's built-in <code>Set</code> type and uses the standard <code>IntSeq</code> library module.


```modula2
MODULE Permutations EXPORTS Main;

IMPORT IO, IntSeq;

CONST n = 3;

TYPE Domain = SET OF [ 1.. n ];

VAR

  chosen: IntSeq.T;
  values := Domain { };

PROCEDURE GeneratePermutations(VAR chosen: IntSeq.T; remaining: Domain) =
(*
  Recursively generates all the permutations of elements
  in the union of "chosen" and "values".
  Values in "chosen" have already been chosen;
  values in "remaining" can still be chosen.
  If "remaining" is empty, it prints the sequence and returns.
  Otherwise, it picks each element in "remaining", removes it,
  adds it to "chosen", recursively calls itself,
  then removes the last element of "chosen" and adds it back to "remaining".
*)
BEGIN
  FOR i := 1 TO n DO
    (* check if each element is in "remaining" *)
    IF i IN remaining THEN
      (* if so, remove from "remaining" and add to "chosen" *)
      remaining := remaining - Domain { i };
      chosen.addhi(i);
      IF remaining # Domain { } THEN
        (* still something to process? do it *)
        GeneratePermutations(chosen, remaining);
      ELSE
        (* otherwise, print what we've chosen *)
        FOR j := 0 TO chosen.size() - 2 DO
          IO.PutInt(chosen.get(j)); IO.Put(", ");
        END;
        IO.PutInt(chosen.gethi());
        IO.PutChar('\n');
      END;
      (* add "i" back to "remaining" and remove from "chosen" *)
      remaining := remaining + Domain { i };
      EVAL chosen.remhi();
    END;
  END;
END GeneratePermutations;

BEGIN

  (* initial setup *)
  chosen := NEW(IntSeq.T).init(n);
  FOR i := 1 TO n DO values := values + Domain { i }; END;

  GeneratePermutations(chosen, values);

END Permutations.
```


For reasons of space, we show only the elements of <i>S</i><sub>3</sub>, but we have tested it with higher.

```txt

1, 2, 3
1, 3, 2
2, 1, 3
2, 3, 1
3, 1, 2
3, 2, 1

```


=
## Generic version
=

This version works on any type, and requires the library's <code>Set</code> and <code>Sequence</code>. As usual in Modula-3, the generic instance will need to be instantiated for whatever type you want to use, and you will also need to instantiate a set of, sequence of, and sequence of sequences of the domain elements. This will have to be taken care of by the <code>m3makefile</code>.

;interface

Suppose that <code>D</code> is the domain of elements to be permuted. This module requires a <code>DomainSeq</code> (<code>Sequence</code> of <code>D</code>), a <code>DomainSet</code> (<code>Set</code> of <code>D</code>), and a <code>DomainSeqSeq</code> (<code>Sequence</code> of <code>Sequence</code>s of <code>Domain</code>).


```modula3
GENERIC INTERFACE GenericPermutations(DomainSeq, DomainSet, DomainSeqSeq);

(*
  "Domain" is where the things to permute come from (unused in interface).
  "DomainSeq" is a "Sequence" of "Domain".
  "DomainSet" is a "Set" of "Domain".
  "DomainSeqSeq" is a "Sequence" of "DomainSeq".
*)

PROCEDURE GeneratePermutations(
  READONLY chosen: DomainSeq.T;
  READONLY remaining: DomainSet.T;
  READONLY result: DomainSeqSeq.T
);
(*
  Recursively generates all the permutations of elements
  in the union of "chosen" and "remaining".
  Values in "chosen" have already been chosen;
  values in "remaining" can still be chosen.
  If "remaining" is empty, it adds the permutation to "result".
  Otherwise, it picks each element in "remaining", removes it,
  adds it to "chosen", recursively calls itself,
  then removes the last element of "chosen" and adds it back to "remaining".
  Although the parameters are modified, we can describe them as "READONLY"
  because we do not re-assign them.
*)

END GenericPermutations.
```


;implementation

In addition to the interface's specifications, this requires a generic <code>Domain</code>. Some implementations of a set are not safe to iterate over while modifying (e.g., a tree), so this copies the values and iterates over them.


```modula3
GENERIC MODULE GenericPermutations(Domain, DomainSeq, DomainSet, DomainSeqSeq);

(*
  "Domain" is where the things to permute come from.
  "DomainSeq" is a "Sequence" of "Domain".
  "DomainSet" is a "Set" of "Domain".
  "DomainSeqSeq" is a "Sequence" of "DomainSeq".
*)

PROCEDURE GeneratePermutations(
  READONLY chosen: DomainSeq.T;
  READONLY remaining: DomainSet.T;
  READONLY result: DomainSeqSeq.T
) =

(*
  Recursively generates all the permutations of elements
  in the union of "chosen" and "remaining".
  Values in "chosen" have already been chosen;
  values in "remaining" can still be chosen.
  If "remaining" is empty, it adds the permutation to "result".
  Otherwise, it picks each element in "remaining", removes it,
  adds it to "chosen", recursively calls itself,
  then removes the last element of "chosen" and adds it back to "remaining".
*)

VAR

  r: Domain.T; (* element added to permutation *)

  iterator := remaining.iterate(); (* to iterate through remaining elements *)

  values := NEW(DomainSeq.T).init(remaining.size());
  (* used to store values for iteration *)

BEGIN

  (* cannot safely modify a set while iterating, so we'll store the values *)
  WHILE iterator.next(r) DO values.addhi(r); END;

  (* now loop through the stored values *)
  FOR i := 0 TO values.size() - 1 DO

    (* remove from "remaining" and add to "chosen" *)
    r := values.get(i);
    EVAL remaining.delete(r);
    chosen.addhi(r);

    (* if this is not the last remaining elements, call recursively *)
    IF remaining.size() # 0 THEN
      GeneratePermutations(chosen, remaining, result);
    ELSE
      (* we have a new permutation; add a copy to the set *)
      VAR newPerm := NEW(DomainSeq.T).init(chosen.size());
      BEGIN
        FOR i := 0 TO chosen.size() - 1 DO
          newPerm.addhi(chosen.get(i));
        END;
        result.addhi(newPerm);
      END;
    END;

    (* move r back from chosen *)
    EVAL remaining.insert(chosen.remhi());

  END;

END GeneratePermutations;

BEGIN
END GenericPermutations.
```


;Sample Usage

Here the domain is <code>Integer</code>, but the interface doesn't require that, so we "merely" need <code>IntSeq</code> (a <code>Sequence</code> of <code>Integer</code>), <code>IntSetTree</code> (a set type I use, but you could use <code>SetDef</code> or <code>SetList</code> if you prefer; I've tested it and it works), <code>IntSeqSeq</code> (a <code>Sequence</code> of <code>Sequence</code>s of <code>Integer</code>), and <code>IntPermutations</code>, which is <code>GenericPermutations</code> instantiated for <code>Integer</code>.


```modula3
MODULE GPermutations EXPORTS Main;

IMPORT IO, IntSeq, IntSetTree, IntSeqSeq, IntPermutations;

CONST

  n = 7;

VAR

  chosen: IntSeq.T;
  remaining: IntSetTree.T;
  result: IntSeqSeq.T;

PROCEDURE Factorial(n: CARDINAL): CARDINAL =
VAR result := 1;
BEGIN
  FOR i := 2 TO n DO
    result := result * i;
  END;
  RETURN result;
END Factorial;

BEGIN

  (* initial setup *)
  chosen := NEW(IntSeq.T).init(n);
  remaining := NEW(IntSetTree.T).init();
  result := NEW(IntSeqSeq.T).init(Factorial(n));
  FOR i := 1 TO n DO EVAL remaining.insert(i); END;

  IntPermutations.GeneratePermutations(chosen, remaining, result);

  IO.Put("Printing "); IO.PutInt(result.size());
  IO.Put(" permutations of "); IO.PutInt(n); IO.Put(" elements \n");
  FOR i := 0 TO result.size() - 1 DO
    FOR j := 0 TO result.get(i).size() - 1 DO
      IO.PutInt(result.get(i).get(j)); IO.PutChar(' ');
    END;
    IO.PutChar('\n');
  END;

END GPermutations.
```


{{out}} (somewhat edited!)

```txt
Printing 5040 permutations of 7 elements
1 2 3 4 5 6 7
1 2 3 4 5 7 6
1 2 3 4 6 5 7
...
7 6 5 4 2 3 1
7 6 5 4 3 1 2
7 6 5 4 3 2 1

```



## NetRexx


```NetRexx
/* NetRexx */
options replace format comments java crossref symbols nobinary

import java.util.List
import java.util.ArrayList

--
### =======================================================================

/**
 * Permutation Iterator
 * <br />
 * <br />
 * Algorithm by E. W. Dijkstra, "A Discipline of Programming", Prentice-Hall, 1976, p.71
 */
class RPermutationIterator implements Iterator

  -- ---------------------------------------------------------------------------
  properties indirect
    perms = List
    permOrders = int[]
    maxN
    currentN
    first = boolean

  -- ---------------------------------------------------------------------------
  properties constant
    isTrue  = boolean (1 == 1)
    isFalse = boolean (1 \= 1)

  -- ---------------------------------------------------------------------------
  method RPermutationIterator(initial = List) public
    setUp(initial)
    return

  -- ---------------------------------------------------------------------------
  method RPermutationIterator(initial = Object[]) public
    init = ArrayList(initial.length)
    loop elmt over initial
      init.add(elmt)
      end elmt
    setUp(init)
    return

  -- ---------------------------------------------------------------------------
  method RPermutationIterator(initial = Rexx[]) public
    init = ArrayList(initial.length)
    loop elmt over initial
      init.add(elmt)
      end elmt
    setUp(init)
    return

  -- ---------------------------------------------------------------------------
  method setUp(initial = List) private
    setFirst(isTrue)
    setPerms(initial)
    setPermOrders(int[getPerms().size()])
    setMaxN(getPermOrders().length)
    setCurrentN(0)
    po = getPermOrders()
    loop i_ = 0 while i_ < po.length
      po[i_] = i_
      end i_
    return

  -- ---------------------------------------------------------------------------
  method hasNext() public returns boolean
    status = isTrue
    if getCurrentN() == factorial(getMaxN()) then status = isFalse
    setCurrentN(getCurrentN() + 1)
    return status

  -- ---------------------------------------------------------------------------
  method next() public returns Object
    if isFirst() then setFirst(isFalse)
    else do
      po = getPermOrders()
      i_ = getMaxN() - 1
      loop while po[i_ - 1] >= po[i_]
        i_ = i_ - 1
        end

      j_ = getMaxN()
      loop while po[j_ - 1] <= po[i_ - 1]
        j_ = j_ - 1
        end

      swap(i_ - 1, j_ - 1)

      i_ = i_ + 1
      j_ = getMaxN()
      loop while i_ < j_
        swap(i_ - 1, j_ - 1)
        i_ = i_ + 1
        j_ = j_ - 1
        end
      end
    return reorder()

  -- ---------------------------------------------------------------------------
  method remove() public signals UnsupportedOperationException
    signal UnsupportedOperationException()

  -- ---------------------------------------------------------------------------
  method swap(i_, j_) private
    po = getPermOrders()
    save   = po[i_]
    po[i_] = po[j_]
    po[j_] = save
    return

  -- ---------------------------------------------------------------------------
  method reorder() private returns List
    result = ArrayList(getPerms().size())
    loop ix over getPermOrders()
      result.add(getPerms().get(ix))
      end ix
    return result

  -- ---------------------------------------------------------------------------
  /**
   * Calculate n factorial: {@code n! = 1 * 2 * 3 .. * n}
   * @param n
   * @return n!
   */
  method factorial(n) public static
    fact = 1
    if n > 1 then loop i = 1 while i <= n
      fact = fact * i
      end i
    return fact

  -- ---------------------------------------------------------------------------
  method main(args = String[]) public static
    thing02 = RPermutationIterator(['alpha', 'omega'])
    thing03 = RPermutationIterator([String 'one', 'two', 'three'])
    thing04 = RPermutationIterator(Arrays.asList([Integer(1), Integer(2), Integer(3), Integer(4)]))
    things = [thing02, thing03, thing04]
    loop thing over things
      N = thing.getMaxN()
      say 'Permutations:' N'! =' factorial(N)
      loop lineCount = 1 while thing.hasNext()
        prm = thing.next()
        say lineCount.right(8)':' prm.toString()
        end lineCount
      say 'Permutations:' N'! =' factorial(N)
      say
      end thing
    return

```

<pre style="height:55ex;overflow:scroll">
Permutations: 2! = 2
       1: [alpha, omega]
       2: [omega, alpha]
Permutations: 2! = 2

Permutations: 3! = 6
       1: [one, two, three]
       2: [one, three, two]
       3: [two, one, three]
       4: [two, three, one]
       5: [three, one, two]
       6: [three, two, one]
Permutations: 3! = 6

Permutations: 4! = 24
       1: [1, 2, 3, 4]
       2: [1, 2, 4, 3]
       3: [1, 3, 2, 4]
       4: [1, 3, 4, 2]
       5: [1, 4, 2, 3]
       6: [1, 4, 3, 2]
       7: [2, 1, 3, 4]
       8: [2, 1, 4, 3]
       9: [2, 3, 1, 4]
      10: [2, 3, 4, 1]
      11: [2, 4, 1, 3]
      12: [2, 4, 3, 1]
      13: [3, 1, 2, 4]
      14: [3, 1, 4, 2]
      15: [3, 2, 1, 4]
      16: [3, 2, 4, 1]
      17: [3, 4, 1, 2]
      18: [3, 4, 2, 1]
      19: [4, 1, 2, 3]
      20: [4, 1, 3, 2]
      21: [4, 2, 1, 3]
      22: [4, 2, 3, 1]
      23: [4, 3, 1, 2]
      24: [4, 3, 2, 1]
Permutations: 4! = 24

```



## Nim

```nim
# iterative Boothroyd method
iterator permutations[T](ys: openarray[T]): seq[T] =
  var
    d = 1
    c = newSeq[int](ys.len)
    xs = newSeq[T](ys.len)

  for i, y in ys: xs[i] = y
  yield xs

  block outer:
    while true:
      while d > 1:
        dec d
        c[d] = 0
      while c[d] >= d:
        inc d
        if d >= ys.len: break outer

      let i = if (d and 1) == 1: c[d] else: 0
      swap xs[i], xs[d]
      yield xs
      inc c[d]

var x = @[1,2,3]

for i in permutations(x):
  echo i
```

Output:

```txt
@[1, 2, 3]
@[2, 1, 3]
@[3, 1, 2]
@[1, 3, 2]
@[2, 3, 1]
@[3, 2, 1]
```


```nim
# nim implementation of the (very fast) Go example
# http://rosettacode.org/wiki/Permutations#Go
# implementing a recursive https://en.wikipedia.org/wiki/Steinhaus–Johnson–Trotter_algorithm

proc perm( s: openArray[int], emit: proc(emit:openArray[int]) ) =
    var s = @s
    if s.len == 0:
        emit(s)
        return

    var rc : proc(np: int)
    rc = proc(np: int) =

        if np == 1:
            emit(s)
            return

        var
            np1 = np - 1
            pp = s.len - np1

        rc(np1) # recurs prior swaps

        for i in countDown(pp, 1):
            swap s[i], s[i-1]
            rc(np1) # recurs swap

        let w = s[0]
        s[0..<pp] = s[1..pp]
        s[pp] = w

    rc(s.len)

var se = @[0, 1, 2, 3] #, 4, 5, 6, 7, 8, 9, 10]

perm(se, proc(seq: openArray[int])=
    echo seq
    )
```



## OCaml


```ocaml
(* Iterative, though loops are implemented as auxiliary recursive functions.
   Translation of Ada version. *)
let next_perm p =
	let n = Array.length p in
	let i = let rec aux i =
		if (i < 0) || (p.(i) < p.(i+1)) then i
		else aux (i - 1) in aux (n - 2) in
	let rec aux j k = if j < k then
		let t = p.(j) in
			p.(j) <- p.(k);
			p.(k) <- t;
			aux (j + 1) (k - 1)
	else () in aux (i + 1) (n - 1);
	if i < 0 then false else
		let j = let rec aux j =
			if p.(j) > p.(i) then j
			else aux (j + 1) in aux (i + 1) in
		let t = p.(i) in
			p.(i) <- p.(j);
			p.(j) <- t;
			true;;

let print_perm p =
	let n = Array.length p in
	for i = 0 to n - 2 do
		print_int p.(i);
		print_string " "
	done;
	print_int p.(n - 1);
	print_newline ();;

let print_all_perm n =
	let p = Array.init n (function i -> i + 1) in
	print_perm p;
	while next_perm p do
		print_perm p
	done;;

print_all_perm 3;;
(* 1 2 3
   1 3 2
   2 1 3
   2 3 1
   3 1 2
   3 2 1 *)
```

Permutations can also be defined on lists recursively:

```OCaml
let rec permutations l =
   let n = List.length l in
   if n = 1 then [l] else
   let rec sub e = function
      | [] -> failwith "sub"
      | h :: t -> if h = e then t else h :: sub e t in
   let rec aux k =
      let e = List.nth l k in
      let subperms = permutations (sub e l) in
      let t = List.map (fun a -> e::a) subperms in
      if k < n-1 then List.rev_append t (aux (k+1)) else t in
   aux 0;;

let print l = List.iter (Printf.printf " %d") l; print_newline() in
List.iter print (permutations [1;2;3;4])
```

or permutations indexed independently:

```OCaml
let rec pr_perm k n l =
   let a, b = let c = k/n in c, k-(n*c) in
   let e = List.nth l b in
   let rec sub e = function
      | [] -> failwith "sub"
      | h :: t -> if h = e then t else h :: sub e t in
   (Printf.printf " %d" e; if n > 1 then pr_perm a (n-1) (sub e l))

let show_perms l =
   let n = List.length l in
   let rec fact n = if n < 3 then n else n * fact (n-1) in
   for i = 0 to (fact n)-1 do
      pr_perm i n l;
      print_newline()
   done

let () = show_perms [1;2;3;4]
```



## OpenEdge/Progress


```OpenEdge/Progress

DEFINE VARIABLE charArray AS CHARACTER EXTENT 3 INITIAL ["A","B","C"].
DEFINE VARIABLE sizeofArray AS INTEGER.

sizeOfArray = EXTENT(charArray).

RUN GetPermutations(1).

PROCEDURE GetPermutations:
    DEFINE INPUT PARAMETER n AS INTEGER.

    DEFINE VARIABLE i AS INTEGER.
    DEFINE VARIABLE j AS INTEGER.
    DEFINE VARIABLE currentPermutation AS CHARACTER.

    REPEAT i = n TO sizeOfArray:
        RUN swapValues(i,n).
        RUN GetPermutations(n + 1).
        RUN swapValues(i,n).
    END.
    IF n = sizeOfArray THEN DO:
        DO j = 1 TO EXTENT(charArray):
            currentPermutation = currentPermutation + charArray[j].
        END.
        DISPLAY currentPermutation WITH FRAME A DOWN.
    END.
END PROCEDURE.

PROCEDURE swapValues:
    DEFINE INPUT PARAMETER a AS INTEGER.
    DEFINE INPUT PARAMETER b AS INTEGER.
    DEFINE VARIABLE temp AS CHARACTER.
    temp = charArray[a].
    charArray[a] = charArray[b].
    charArray[b] = temp.
END PROCEDURE.
```

```txt
ABC
ACB
BAC
BCA
CAB
CBA
```



## PARI/GP


```parigp
vector(n!,k,numtoperm(n,k))
```



## Pascal


```pascal
program perm;

var
	p: array[1 .. 12] of integer;
	is_last: boolean;
	n: integer;

procedure next;
var i, j, k, t: integer;
begin
is_last := true;
i := n - 1;
while i > 0 do
	begin
	if p[i] < p[i + 1] then
		begin
		is_last := false;
		break;
		end;
	i := i - 1;
	end;

if not is_last then
	begin
	j := i + 1;
	k := n;
	while j < k do
		begin
		t := p[j];
		p[j] := p[k];
		p[k] := t;
		j := j + 1;
		k := k - 1;
		end;

	j := n;
	while p[j] > p[i] do j := j - 1;
	j := j + 1;

	t := p[i];
	p[i] := p[j];
	p[j] := t;
	end;
end;

procedure print;
var i: integer;
begin
for i := 1 to n do write(p[i], ' ');
writeln;
end;

procedure init;
var i: integer;
begin
n := 0;
while (n < 1) or (n > 10) do
	begin
	write('Enter n (1 <= n <= 10): ');
	readln(n);
	end;
for i := 1 to n do p[i] := i;
end;

begin
init;
repeat
	print;
	next;
until is_last;
end.
```


### alternative

a little bit more speed.I take n = 12.
The above version takes more than 5 secs.My permlex takes 2.8s, but in the depth of my harddisk I found a version, creating all permutations using k places out of n.The cpu loves it! 1.33 s.
But you have to use the integers [1..n] directly or as Index to your data.
1 to n are in lexicographic order.

```pascal
{$IFDEF FPC}
  {$MODE DELPHI}
{$ELSE}
  {$APPTYPE CONSOLE}
{$ENDIF}
uses
  sysutils;
type
  tPermfield  =  array[0..15] of Nativeint;
var
  permcnt: NativeUint;

procedure DoSomething(k: NativeInt;var x:tPermfield);
var
  i:integer;
  kk:string;
begin
  kk:='';
  for i:=1 to k do kk:=kk+inttostr(x[i])+' ';
    writeln(kk);
end;

procedure PermKoutOfN(k,n: nativeInt);
var
  x,y:tPermfield;
  i,yi,tmp:NativeInt;
begin
  //initialise
  permcnt:= 1;
  if k>n then
    k:=n;
  if k=n then
    k:=k-1;
  for i:=1 to n do x[i]:=i;
  for i:=1 to k do y[i]:=i;

//  DoSomething(k,x);
  i := k;
  repeat
    yi:=y[i];
    if yi <n then
    begin
      inc(permcnt);
      inc(yi);
      y[i]:=yi;
      tmp:=x[i];x[i]:=x[yi];x[yi]:=tmp;
      i:=k;
//      DoSomething(k,x);
    end
    else
    begin
      repeat
        tmp:=x[i];x[i]:=x[yi];x[yi]:=tmp;
        dec(yi);
      until yi<=i;
      y[i]:=yi;
      dec(i);
    end;
  until (i=0);
end;

var
  t1,t0 : TDateTime;
Begin
  permcnt:= 0;
  T0 := now;
  PermKoutOfN(12,12);
  T1 := now;
  writeln(permcnt);
  writeln(FormatDateTime('HH:NN:SS.zzz',T1-T0));
end.
```

{fpc 2.64/3.0 32Bit or 3.1 64 Bit i4330 3.5 Ghz same timings.
//PermKoutOfN(12,12);

```txt

479001600 //= 12!
00:00:01.328
```



## Perl

A simple recursive implementation.

```perl
sub permutation {
	my ($perm,@set) = @_;
	print "$perm\n" || return unless (@set);
	permutation($perm.$set[$_],@set[0..$_-1],@set[$_+1..$#set]) foreach (0..$#set);
}
my @input = (qw/a b c d/);
permutation('',@input);
```

```txt
abcd
abdc
acbd
acdb
adbc
adcb
bacd
badc
bcad
bcda
bdac
bdca
cabd
cadb
cbad
cbda
cdab
cdba
dabc
dacb
dbac
dbca
dcab
dcba
```


For better performance, use a module like <code>ntheory</code> or <code>Algorithm::Permute</code>.
```perl
use ntheory qw/forperm/;
my @tasks = (qw/party sleep study/);
forperm {
  print "@tasks[@_]\n";
} @tasks;
```

```txt

party sleep study
party study sleep
sleep party study
sleep study party
study party sleep
study sleep party

```



## Perl 6

First, you can just use the built-in method on any list type.

```Perl6>.say for <a b c>.permutations</lang

```txt
a b c
a c b
b a c
b c a
c a b
c b a
```


Here is some generic code that works with any ordered type.  To force lexicographic ordering, change <tt>after</tt> to <tt>gt</tt>.  To force numeric order, replace it with <tt>&gt;</tt>.

```perl6
sub next_perm ( @a is copy ) {
    my $j = @a.end - 1;
    return Nil if --$j < 0 while @a[$j] after @a[$j+1];

    my $aj = @a[$j];
    my $k  = @a.end;
    $k-- while $aj after @a[$k];
    @a[ $j, $k ] .= reverse;

    my $r = @a.end;
    my $s = $j + 1;
    @a[ $r--, $s++ ] .= reverse while $r > $s;
    return @a;
}

.say for [<a b c>], &next_perm ...^ !*;
```

```txt
a b c
a c b
b a c
b c a
c a b
c b a

```

Here is another non-recursive implementation, which returns a lazy list. It also works with any type.

```perl6
sub permute(+@items) {
   my @seq := 1..+@items;
   gather for (^[*] @seq) -> $n is copy {
      my @order;
      for @seq {
         unshift @order, $n mod $_;
         $n div= $_;
      }
      my @i-copy = @items;
      take map { |@i-copy.splice($_, 1) }, @order;
   }
}
.say for permute( 'a'..'c' )
```

```txt
(a b c)
(a c b)
(b a c)
(b c a)
(c a b)
(c b a)
```

Finally, if you just want zero-based numbers, you can call the built-in function:

```perl6
.say for permutations(3);
```

```txt
0 1 2
0 2 1
1 0 2
1 2 0
2 0 1
2 1 0
```



## Phix

The distribution includes builtins\permute.e, which is reproduced below. This can be used to retrieve all possible permutations, in
no particular order. The elements can be any type. It is just as fast to generate the (n!)th permutation as the first, so some applications may benefit by storing
an integer key rather than duplicating all the elements of the given set.

```Phix
global function permute(integer n, sequence set)
--
-- return the nth permute of the given set.
-- n should be an integer in the range 1 to factorial(length(set))
--
sequence res
integer w
    n -= 1
    res = set
    for i=length(set) to 1 by -1 do
        w = remainder(n,i)+1
        res[i] = set[w]
        set[w] = set[i]
        n = floor(n/i)
    end for
    return res
end function
```

Example use:

```Phix
function permutes(sequence set)
sequence res = repeat(0,factorial(length(set)))
    for i=1 to length(res) do
        res[i] = permute(i,set)
    end for
    return res
end function
?permutes("abcd")
```

```txt

{"bcda","dcab","bdac","bcad","cdba","cadb","dabc","cabd","bdca","dacb","badc","bacd","cbda","cdab","dbac","cbad","dcba","acdb","adbc","acbd","dbca","adcb","abdc","abcd"}

```



## PicoLisp


```PicoLisp
(load "@lib/simul.l")

(permute (1 2 3))
```

```txt
-> ((1 2 3) (1 3 2) (2 1 3) (2 3 1) (3 1 2) (3 2 1))
```



## PowerBASIC

```ada
  #COMPILE EXE
  #DIM ALL
  GLOBAL a, i, j, k, n  AS INTEGER
  GLOBAL d, ns, s AS STRING 'dynamic string
  FUNCTION PBMAIN () AS LONG
  ns = INPUTBOX$("   n =",, "3") 'input n
  n = VAL(ns)
  DIM a(1 TO n) AS INTEGER
  FOR i = 1 TO n: a(i)= i: NEXT
  DO
    s = " "
    FOR i = 1 TO n
      d = STR$(a(i))
      s = BUILD$(s, d) '  s & d concatenate
    NEXT
    ? s  'print and pause
    i = n
    DO
     DECR i
    LOOP UNTIL i = 0 OR a(i) < a(i+1)
    j = i+1
    k = n
    DO WHILE j < k
      SWAP a(j), a(k)
      INCR j
      DECR k
    LOOP
    IF i > 0 THEN
      j = i+1
      DO WHILE a(j) < a(i)
        INCR j
      LOOP
      SWAP a(i), a(j)
    END IF
  LOOP UNTIL i = 0
  END FUNCTION
```

```txt

 1 2 3
 1 3 2
 2 1 3
 2 3 1
 3 1 2
 3 2 1

```


## PowerShell


```PowerShell

function permutation ($array) {
    function generate($n, $array, $A) {
        if($n -eq 1) {
            $array[$A] -join ' '
        }
        else{
            for( $i = 0; $i -lt ($n - 1); $i += 1) {
                generate ($n - 1) $array $A
                if($n % 2 -eq 0){
                    $i1, $i2 = $i, ($n-1)
                    $A[$i1], $A[$i2] = $A[$i2], $A[$i1]
                }
                else{
                    $i1, $i2 = 0, ($n-1)
                    $A[$i1], $A[$i2] = $A[$i2], $A[$i1]
                }
            }
            generate ($n - 1) $array $A
        }
    }
    $n = $array.Count
    if($n -gt 0) {
        (generate $n $array (0..($n-1)))
    } else {$array}
}
permutation @('A','B','C')

```

<b>Output:</b>

```txt

A B C
B A C
C A B
A C B
B C A
C B A

```



## Prolog

Works with SWI-Prolog and library clpfd,

```Prolog
:- use_module(library(clpfd)).

permut_clpfd(L, N) :-
    length(L, N),
    L ins 1..N,
    all_different(L),
    label(L).
```

```Prolog
?- permut_clpfd(L, 3), writeln(L), fail.
[1,2,3]
[1,3,2]
[2,1,3]
[2,3,1]
[3,1,2]
[3,2,1]
false.

```

A declarative way of fetching permutations:

```Prolog
% permut_Prolog(P, L)
% P is a permutation of L

permut_Prolog([], []).
permut_Prolog([H | T], NL) :-
	select(H, NL, NL1),
	permut_Prolog(T, NL1).
```

```Prolog
 ?- permut_Prolog(P, [ab, cd, ef]), writeln(P), fail.
[ab,cd,ef]
[ab,ef,cd]
[cd,ab,ef]
[cd,ef,ab]
[ef,ab,cd]
[ef,cd,ab]
false.
```



## PureBasic

The procedure nextPermutation() takes an array of integers as input and transforms its contents into the next lexicographic permutation of it's elements (i.e. integers).  It returns #True if this is possible.  It returns #False if there are no more lexicographic permutations left and arranges the elements into the lowest lexicographic permutation.  It also returns #False if there is less than 2 elemetns to permute.

The integer elements could be the addresses of objects that are pointed at instead.  In this case the addresses will be permuted without respect to what they are pointing to (i.e. strings, or structures) and the lexicographic order will be that of the addresses themselves.

```PureBasic
Macro reverse(firstIndex, lastIndex)
  first = firstIndex
  last = lastIndex
  While first < last
    Swap cur(first), cur(last)
    first + 1
    last - 1
  Wend
EndMacro

Procedure nextPermutation(Array cur(1))
  Protected first, last, elementCount = ArraySize(cur())
  If elementCount < 1
    ProcedureReturn #False ;nothing to permute
  EndIf

  ;Find the lowest position pos such that [pos] < [pos+1]
  Protected pos = elementCount - 1
  While cur(pos) >= cur(pos + 1)
    pos - 1
    If pos < 0
      reverse(0, elementCount)
      ProcedureReturn #False ;no higher lexicographic permutations left, return lowest one instead
    EndIf
  Wend

  ;Swap [pos] with the highest positional value that is larger than [pos]
  last = elementCount
  While cur(last) <= cur(pos)
    last - 1
  Wend
  Swap cur(pos), cur(last)

  ;Reverse the order of the elements in the higher positions
  reverse(pos + 1, elementCount)
  ProcedureReturn #True ;next lexicographic permutation found
EndProcedure

Procedure display(Array a(1))
  Protected i, fin = ArraySize(a())
  For i = 0 To fin
    Print(Str(a(i)))
    If i = fin: Continue: EndIf
    Print(", ")
  Next
  PrintN("")
EndProcedure

If OpenConsole()
  Dim a(2)
  a(0) = 1: a(1) = 2: a(2) =  3
  display(a())
  While nextPermutation(a()): display(a()): Wend

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
```

```txt
1, 2, 3
1, 3, 2
2, 1, 3
2, 3, 1
3, 1, 2
3, 2, 1
```



## Python



### Standard library function

```python
import itertools
for values in itertools.permutations([1,2,3]):
    print (values)
```

```txt

(1, 2, 3)
(1, 3, 2)
(2, 1, 3)
(2, 3, 1)
(3, 1, 2)
(3, 2, 1)

```



###  Recursive implementation


The follwing functions start from a list [0 ... n-1] and exchange elements to always have a valid permutation. This is done recursively: first exchange a[0] with all the other elements, then a[1] with a[2] ... a[n-1], etc. thus yielding all permutations.


```python
def perm1(n):
    a = list(range(n))
    def sub(i):
        if i == n - 1:
            yield tuple(a)
        else:
            for k in range(i, n):
                a[i], a[k] = a[k], a[i]
                yield from sub(i + 1)
                a[i], a[k] = a[k], a[i]
    yield from sub(0)

def perm2(n):
    a = list(range(n))
    def sub(i):
        if i == n - 1:
            yield tuple(a)
        else:
            for k in range(i, n):
                a[i], a[k] = a[k], a[i]
                yield from sub(i + 1)
            x = a[i]
            for k in range(i + 1, n):
                a[k - 1] = a[k]
            a[n - 1] = x
    yield from sub(0)
```


These two solutions make use of a generator, and "yield from" introduced in [https://www.python.org/dev/peps/pep-0380/ PEP-380]. They are slightly different: the latter produces permutations in lexicographic order, because the "remaining" part of a (that is, a[i+1:]) is always sorted, whereas the former always reverses the exchange just after the recursive call.

On three elements, the difference can be seen on the last two permutations:


```python
for u in perm1(3): print(u)
(0, 1, 2)
(0, 2, 1)
(1, 0, 2)
(1, 2, 0)
(2, 1, 0)
(2, 0, 1)

for u in perm2(3): print(u)
(0, 1, 2)
(0, 2, 1)
(1, 0, 2)
(1, 2, 0)
(2, 0, 1)
(2, 1, 0)
```



###  Iterative implementation


Given a permutation, one can easily compute the ''next'' permutation in some order, for example lexicographic order, here. Then to get all permutations, it's enough to start from [0, 1, ... n-1], and store the next permutation until [n-1, n-2, ... 0], which is the last in lexicographic order.


```python
def nextperm(a):
    n = len(a)
    i = n - 1
    while i > 0 and a[i - 1] > a[i]:
        i -= 1
    j = i
    k = n - 1
    while j < k:
        a[j], a[k] = a[k], a[j]
        j += 1
        k -= 1
    if i == 0:
        return False
    else:
        j = i
        while a[j] < a[i - 1]:
            j += 1
        a[i - 1], a[j] = a[j], a[i - 1]
        return True

def perm3(n):
    if type(n) is int:
        if n < 1:
            return []
        a = list(range(n))
    else:
        a = sorted(n)
    u = [tuple(a)]
    while nextperm(a):
        u.append(tuple(a))
    return u

for p in perm3(3): print(p)
(0, 1, 2)
(0, 2, 1)
(1, 0, 2)
(1, 2, 0)
(2, 0, 1)
(2, 1, 0)
```



###  Implementation using destructive list updates


```python

def permutations(xs):
    ac = [[]]
    for x in xs:
        ac_new = []
        for ts in ac:
            for n in range(0,ts.__len__()+1):
                new_ts = ts[:]  #(shallow) copy of ts
                new_ts.insert(n,x)
                ac_new.append(new_ts)
        ac=ac_new
    return ac

print(permutations([1,2,3,4]))

```



### Functional

The '''itertools.permutations''' function is polymorphic in its inputs but not in its outputs – it discards the type of input lists and strings, coercing all inputs to tuples.

In this type-preserving variant, permutation is defined (without the need for mutating name-bindings) in terms of two universal abstractions: '''reduce''' and '''concatMap''':

```python
'''Permutations of a list, string or tuple'''

from functools import (reduce)
from itertools import (chain)


# permutations :: [a] -> [[a]]
def permutations(xs):
    '''Type-preserving permutations of xs.
    '''
    ps = reduce(
        lambda a, x: concatMap(
            lambda xs: (
                xs[n:] + [x] + xs[0:n] for n in range(0, 1 + len(xs)))
        )(a),
        xs, [[]]
    )
    t = type(xs)
    return ps if list == t else (
        [''.join(x) for x in ps] if str == t else [
            t(x) for x in ps
        ]
    )


# TEST ----------------------------------------------------

# main :: IO ()
def main():
    '''Permutations of lists, strings and tuples.'''

    print(
        fTable(__doc__ + ':\n')(repr)(showList)(
            permutations
        )([
            [1, 2, 3],
            'abc',
            (1, 2, 3),
        ])
    )


# GENERIC -------------------------------------------------

# concatMap :: (a -> [b]) -> [a] -> [b]
def concatMap(f):
    '''A concatenated list over which a function has been mapped.
       The list monad can be derived by using a function f which
       wraps its output in a list,
       (using an empty list to represent computational failure).'''
    return lambda xs: list(
        chain.from_iterable(map(f, xs))
    )


# FORMATTING ----------------------------------------------

# fTable :: String -> (a -> String) ->
#                     (b -> String) -> (a -> b) -> [a] -> String
def fTable(s):
    '''Heading -> x display function -> fx display function ->
                     f -> xs -> tabular string.
    '''
    def go(xShow, fxShow, f, xs):
        ys = [xShow(x) for x in xs]
        w = max(map(len, ys))
        return s + '\n' + '\n'.join(map(
            lambda x, y: y.rjust(w, ' ') + ' -> ' + fxShow(f(x)),
            xs, ys
        ))
    return lambda xShow: lambda fxShow: lambda f: lambda xs: go(
        xShow, fxShow, f, xs
    )


# showList :: [a] -> String
def showList(xs):
    '''Stringification of a list.'''
    return '[' + ','.join(showList(x) for x in xs) + ']' if (
        isinstance(xs, list)
    ) else repr(xs)


# MAIN ---
if __name__ == '__main__':
    main()
```

```txt
[1, 2, 3] -> [[1,2,3],[2,3,1],[3,1,2],[2,1,3],[1,3,2],[3,2,1]]
    'abc' -> ['abc','bca','cab','bac','acb','cba']
(1, 2, 3) -> [(1, 2, 3),(2, 3, 1),(3, 1, 2),(2, 1, 3),(1, 3, 2),(3, 2, 1)]
```



## Qi

```qi

(define insert
  L      0 E -> [E|L]
  [L|Ls] N E -> [L|(insert Ls (- N 1) E)])

(define seq
  Start Start -> [Start]
  Start End   -> [Start|(seq (+ Start 1) End)])

(define append-lists
  []    -> []
  [A|B] -> (append A (append-lists B)))

(define permutate
  []    -> [[]]
  [H|T] -> (append-lists (map (/. P
                                  (map (/. N
                                           (insert P N H))
                                       (seq 0 (length P))))
                              (permute T))))
```



## R


### Iterative version


```r
next.perm <- function(a) {
  n <- length(a)
  i <- n
  while (i > 1 && a[i - 1] >= a[i]) i <- i - 1
  if (i == 1) {
    NULL
  } else {
    j <- i
    k <- n
    while (j < k) {
      s <- a[j]
      a[j] <- a[k]
      a[k] <- s
      j <- j + 1
      k <- k - 1
    }
    s <- a[i - 1]
    j <- i
    while (a[j] <= s) j <- j + 1
    a[i - 1] <- a[j]
    a[j] <- s
    a
  }
}

perm <- function(n) {
  e <- NULL
  a <- 1:n
  repeat {
    e <- cbind(e, a)
    a <- next.perm(a)
    if (is.null(a)) break
  }
  unname(e)
}
```


'''Example'''

<lang>> perm(3)
     [,1] [,2] [,3] [,4] [,5] [,6]
[1,]    1    1    2    2    3    3
[2,]    2    3    1    3    1    2
[3,]    3    2    3    1    2    1
```



### Recursive version


```r
# list of the vectors by inserting x in s at position 0...end.
linsert <- function(x,s) lapply(0:length(s), function(k) append(s,x,k))

# list of all permutations of 1:n
perm <- function(n){
    if (n == 1) list(1)
    else unlist(lapply(perm(n-1), function(s) linsert(n,s)),
                recursive = F)}

# permutations of a vector s
permutation <- function(s) lapply(perm(length(s)), function(i) s[i])

```


Output:

```r>
 permutation(letters[1:3])
[[1]]
[1] "c" "b" "a"

[[2]]
[1] "b" "c" "a"

[[3]]
[1] "b" "a" "c"

[[4]]
[1] "c" "a" "b"

[[5]]
[1] "a" "c" "b"

[[6]]
[1] "a" "b" "c"
```



## Racket


```racket

#lang racket

;; using a builtin
(permutations '(A B C))
;; -> '((A B C) (B A C) (A C B) (C A B) (B C A) (C B A))

;; a random simple version (which is actually pretty good for a simple version)
(define (perms l)
  (let loop ([l l] [tail '()])
    (if (null? l) (list tail)
        (append-map (λ(x) (loop (remq x l) (cons x tail))) l))))
(perms '(A B C))
;; -> '((C B A) (B C A) (C A B) (A C B) (B A C) (A B C))

;; permutations in lexicographic order
(define (lperms s)
  (cond [(empty? s) '()]
        [(empty? (cdr s)) (list s)]
        [else
         (let splice ([l '()][m (car s)][r (cdr s)])
           (append
            (map (lambda (x) (cons m x)) (lperms (append l r)))
            (if (empty? r) '()
                (splice (append l (list m)) (car r) (cdr r)))))]))
(display (lperms '(A B C)))
;; -> ((A B C) (A C B) (B A C) (B C A) (C A B) (C B A))

;; permutations in lexicographical order using generators
(require racket/generator)
(define (splice s)
  (generator ()
             (let outer-loop ([l '()][m (car s)][r (cdr s)])
               (let ([permuter (lperm (append l r))])
                 (let inner-loop ([p (permuter)])
                   (when (not (void? p))
                     (let ([q (cons m p)])
                       (yield q)
                       (inner-loop (permuter))))))
               (if (not (empty? r))
                   (outer-loop (append l (list m)) (car r) (cdr r))
                   (void)))))
(define (lperm s)
  (generator ()
             (cond [(empty? s) (yield '())]
                   [(empty? (cdr s)) (yield s)]
                   [else
                    (let ([splicer (splice s)])
                      (let loop ([q (splicer)])
                        (when (not (void? q))
                          (begin
                            (yield q)
                            (loop (splicer))))))])
             (void)))
(let ([permuter (lperm '(A B C))])
  (let next-perm ([p (permuter)])
    (when (not (void? p))
      (begin
        (display p)
        (next-perm (permuter))))))
;; -> (A B C)(A C B)(B A C)(B C A)(C A B)(C B A)

```



## REXX


### using names

This program could be simplified quite a bit if the "things" were just restricted to numbers (numerals),

but that would make it specific to numbers and not "things" or objects.

```rexx
/*REXX program generates and displays  all  permutations  of    N    different objects. */
parse arg things bunch inbetweenChars names

                       /* inbetweenChars  (optional)   defaults to a  [null].           */
                       /*          names  (optional)   defaults to digits (and letters).*/

call permSets things, bunch, inbetweenChars, names
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
p:  return word(arg(1),1)                        /*P  function (Pick first arg of many).*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
permSets: procedure; parse arg x,y,between,uSyms /*X    things taken    Y    at a time. */
          @.=;   sep=                            /*X  can't be  >  length(@0abcs).      */
          @abc  = 'abcdefghijklmnopqrstuvwxyz';     @abcU=@abc;         upper @abcU
          @abcS = @abcU || @abc;                    @0abcS=123456789 || @abcS

            do k=1  for x                        /*build a list of permutation symbols. */
            _=p(word(uSyms,k)  p(substr(@0abcS,k,1) k))      /*get or generate a symbol.*/
            if length(_)\==1  then sep='_'       /*if not 1st character,  then use sep. */
            $.k=_                                /*append the character to symbol list. */
            end   /*k*/

          if between==''  then between=sep       /*use the appropriate separator chars. */
          call .permset 1                        /*start with the  first  permuation.   */
          return
.permset: procedure expose $. @. between x y;     parse arg ?
          if ?>y then do; _=@.1;   do j=2  to y;  _=_ || between || @.j; end;  say _;  end
                 else do q=1  for x              /*build the permutation recursively.   */
                          do k=1  for ?-1;  if @.k==$.q  then iterate q;  end  /*k*/
                      @.?=$.q;              call .permset ?+1
                      end    /*q*/
          return
```

'''output'''   when the following was used for input:   <tt> 3   3 </tt>

```txt

123
132
213
231
312
321

```

'''output'''   when the following was used for input:   <tt> 4   4   ---   A   B   C   D </tt>

```txt

A---B---C---D
A---B---D---C
A---C---B---D
A---C---D---B
A---D---B---C
A---D---C---B
B---A---C---D
B---A---D---C
B---C---A---D
B---C---D---A
B---D---A---C
B---D---C---A
C---A---B---D
C---A---D---B
C---B---A---D
C---B---D---A
C---D---A---B
C---D---B---A
D---A---B---C
D---A---C---B
D---B---A---C
D---B---C---A
D---C---A---B
D---C---B---A

```

'''output''' when the following was used for input:   <tt> 4   3   ~   aardvark gnu stegosaurus platypus </tt>

```txt

aardvark~gnu~stegosaurus
aardvark~gnu~platypus
aardvark~stegosaurus~gnu
aardvark~stegosaurus~platypus
aardvark~platypus~gnu
aardvark~platypus~stegosaurus
gnu~aardvark~stegosaurus
gnu~aardvark~platypus
gnu~stegosaurus~aardvark
gnu~stegosaurus~platypus
gnu~platypus~aardvark
gnu~platypus~stegosaurus
stegosaurus~aardvark~gnu
stegosaurus~aardvark~platypus
stegosaurus~gnu~aardvark
stegosaurus~gnu~platypus
stegosaurus~platypus~aardvark
stegosaurus~platypus~gnu
platypus~aardvark~gnu
platypus~aardvark~stegosaurus
platypus~gnu~aardvark
platypus~gnu~stegosaurus
platypus~stegosaurus~aardvark
platypus~stegosaurus~gnu

```



### using numbers

This version is modeled after the   '''Maxima'''   program   (as far as output).

It doesn't have the formatting capabilities of the REXX version 1,   nor can it handle taking   '''X'''   items taken   '''Y'''   at-a-time.

```rexx
/*REXX program displays  permutations  of   N   number of  objects  (1, 2, 3, ···).     */
parse arg n .;    if n=='' | n==","  then n=3    /*Not specified?  Then use the default.*/
                                                 /* [↓]  populate the first permutation.*/
         do pop=1  for n;            @.pop=pop  ;     end  /*pop  */;          call tell n
         do  while nPerm(n, 0);      call tell n;     end  /*while*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
nPerm: procedure expose @.;     parse arg n,i;    nm=n-1
         do k=nm  by -1  for nm;  kp=k+1; if @.k<@.kp  then do; i=k; leave; end; end /*k*/
         do j=i+1  while j<n;  parse value  @.j  @.n   with   @.n  @.j;   n=n-1; end /*j*/
         if i==0  then return 0
                                                     do m=i+1  while  @.m<@.i;  end  /*m*/
         parse value  @.m  @.i  with  @.i  @.m
         return 1
/*──────────────────────────────────────────────────────────────────────────────────────*/
tell:    procedure expose @.;  _=;    do j=1  for arg(1);  _=_ @.j;  end;   say _;  return
```

'''output'''   when using the default input:

```txt

 1 2 3
 1 3 2
 2 1 3
 2 3 1
 3 1 2
 3 2 1

```



## Ring


```ring

list = [1, 2, 3, 4]
for perm = 1 to 24
    for i = 1 to len(list)
        see list[i] + " "
    next
    see nl
    nextPermutation(list)
next

func nextPermutation a
     elementcount = len(a)
     if elementcount < 1 then return ok
     pos = elementcount-1
     while a[pos] >= a[pos+1]
           pos -= 1
           if pos <= 0 permutationReverse(a, 1, elementcount)
              return ok
     end
     last = elementcount
     while a[last] <= a[pos]
           last -= 1
     end
     temp = a[pos]
     a[pos] = a[last]
     a[last] = temp
     permutationReverse(a, pos+1, elementcount)

 func permutationReverse a, first, last
      while first < last
            temp = a[first]
            a[first] = a[last]
            a[last] = temp
            first += 1
            last -= 1
      end

```

Output:

```txt

1234
1243
1324
1342
1423
1432
2134
2143
2314
2341
2413
2431
3124
3142
3214
3241
3412
3421
4123
4132
4213
4231
4312
4321

```



## Ruby


```ruby
p [1,2,3].permutation.to_a
```

```txt

[[1, 2, 3], [1, 3, 2], [2, 1, 3], [2, 3, 1], [3, 1, 2], [3, 2, 1]]

```



## Run BASIC

Works with Run BASIC, Liberty BASIC and Just BASIC

```Runbasic
list$ = "h,e,l,l,o"		' supply list seperated with comma's

while word$(list$,d+1,",") <> ""  'Count how many in the list
d = d + 1
wend

dim theList$(d)			' place list in array
for i = 1 to d
  theList$(i) = word$(list$,i,",")
next i

for i = 1 to d			' print the Permutations
 for j = 2 to d
   perm$ = ""
   for k = 1 to d
    perm$ = perm$ + theList$(k)
   next k
   if instr(perm2$,perm$+",") = 0 then print perm$ ' only list 1 time
   perm2$ 	 = perm2$ + perm$ + ","
   h$		 = theList$(j)
   theList$(j)	 = theList$(j - 1)
   theList$(j - 1) = h$
  next j
next i
end
```
Output:

```txt
hello
ehllo
elhlo
ellho
elloh
leloh
lleoh
lloeh
llohe
lolhe
lohle
lohel
olhel
ohlel
ohell
hoell
heoll
helol
```



## Rust


### Iterative

Uses Heap's algorithm. An in-place version is possible but is incompatible with <code>Iterator</code>.

```rust
pub fn permutations(size: usize) -> Permutations {
    Permutations { idxs: (0..size).collect(), swaps: vec![0; size], i: 0 }
}

pub struct Permutations {
    idxs: Vec<usize>,
    swaps: Vec<usize>,
    i: usize,
}

impl Iterator for Permutations {
    type Item = Vec<usize>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.i > 0 {
            loop {
                if self.i >= self.swaps.len() { return None; }
                if self.swaps[self.i] < self.i { break; }
                self.swaps[self.i] = 0;
                self.i += 1;
            }
            self.idxs.swap(self.i, (self.i & 1) * self.swaps[self.i]);
            self.swaps[self.i] += 1;
        }
        self.i = 1;
        Some(self.idxs.clone())
    }
}

fn main() {
    let perms = permutations(3).collect::<Vec<_>>();
    assert_eq!(perms, vec![
        vec![0, 1, 2],
        vec![1, 0, 2],
        vec![2, 0, 1],
        vec![0, 2, 1],
        vec![1, 2, 0],
        vec![2, 1, 0],
    ]);
}
```



### Recursive


```rust
use std::collections::VecDeque;

fn permute<T, F: Fn(&[T])>(used: &mut Vec<T>, unused: &mut VecDeque<T>, action: &F) {
    if unused.is_empty() {
        action(used);
    } else {
        for _ in 0..unused.len() {
            used.push(unused.pop_front().unwrap());
            permute(used, unused, action);
            unused.push_back(used.pop().unwrap());
        }
    }
}

fn main() {
    let mut queue = (1..4).collect::<VecDeque<_>>();
    permute(&mut Vec::new(), &mut queue, &|perm| println!("{:?}", perm));
}
```



## SAS

<!-- oh god this code -->

```sas
/* Store permutations in a SAS dataset. Translation of Fortran 77 */
data perm;
  n=6;
  array a{6} p1-p6;
  do i=1 to n;
    a(i)=i;
  end;
L1:
  output;
  link L2;
  if next then goto L1;
  stop;
L2:
  next=0;
  i=n-1;
L10:
  if a(i)<a(i+1) then goto L20;
  i=i-1;
  if i=0 then goto L20;
  goto L10;
  L20:
  j=i+1;
  k=n;
L30:
  t=a(j);
  a(j)=a(k);
  a(k)=t;
  j=j+1;
  k=k-1;
  if j<k then goto L30;
  j=i;
  if j=0 then return;
L40:
  j=j+1;
  if a(j)<a(i) then goto L40;
  t=a(i);
  a(i)=a(j);
  a(j)=t;
  next=1;
  return;
  keep p1-p6;
run;
```



## Scala

There is a built-in function in the Scala collections library, that is part of the language's standard library.  The permutation function is available on any sequential collection.  It could be used as follows given a list of numbers:


```scala
List(1, 2, 3).permutations.foreach(println)
```


  List(1, 2, 3)
  List(1, 3, 2)
  List(2, 1, 3)
  List(2, 3, 1)
  List(3, 1, 2)
  List(3, 2, 1)

The following function returns all the permutations of a list:


```scala
  def permutations[T]: List[T] => Traversable[List[T]] = {
    case Nil => List(Nil)
    case xs => {
      for {
        (x, i) <- xs.zipWithIndex
        ys <- permutations(xs.take(i) ++ xs.drop(1 + i))
      } yield {
        x :: ys
      }
    }
  }
```


If you need the unique permutations, use <code>distinct</code> or <code>toSet</code> on either the result or on the input.


## Scheme

```scheme
(define (insert l n e)
  (if (= 0 n)
      (cons e l)
      (cons (car l)
            (insert (cdr l) (- n 1) e))))

(define (seq start end)
  (if (= start end)
      (list end)
      (cons start (seq (+ start 1) end))))

(define (permute l)
  (if (null? l)
      '(())
      (apply append (map (lambda (p)
                           (map (lambda (n)
                                  (insert p n (car l)))
                                (seq 0 (length p))))
                         (permute (cdr l))))))
```

```scheme
; translation of ocaml : mostly iterative, with auxiliary recursive functions for some loops
(define (vector-swap! v i j)
(let ((tmp (vector-ref v i)))
(vector-set! v i (vector-ref v j))
(vector-set! v j tmp)))

(define (next-perm p)
(let* ((n (vector-length p))
	(i (let aux ((i (- n 2)))
	(if (or (< i 0) (< (vector-ref p i) (vector-ref p (+ i 1))))
		i (aux (- i 1))))))
(let aux ((j (+ i 1)) (k (- n 1)))
	(if (< j k) (begin (vector-swap! p j k) (aux (+ j 1) (- k 1)))))
(if (< i 0) #f (begin
	(vector-swap! p i (let aux ((j (+ i 1)))
		(if (> (vector-ref p j) (vector-ref p i)) j (aux (+ j 1)))))
	#t))))

(define (print-perm p)
(let ((n (vector-length p)))
(do ((i 0 (+ i 1))) ((= i n)) (display (vector-ref p i)) (display " "))
(newline)))

(define (print-all-perm n)
(let ((p (make-vector n)))
(do ((i 0 (+ i 1))) ((= i n)) (vector-set! p i i))
(print-perm p)
(do ( ) ((not (next-perm p))) (print-perm p))))

(print-all-perm 3)
; 0 1 2
; 0 2 1
; 1 0 2
; 1 2 0
; 2 0 1
; 2 1 0

;a more recursive implementation
(define (permute p i)
(let ((n (vector-length p)))
(if (= i (- n 1)) (print-perm p)
(begin
	(do ((j i (+ j 1))) ((= j n))
		(vector-swap! p i j)
		(permute p (+ i 1)))
	(do ((j (- n 1) (- j 1))) ((< j i))
		(vector-swap! p i j))))))


(define (print-all-perm-rec n)
(let ((p (make-vector n)))
(do ((i 0 (+ i 1))) ((= i n)) (vector-set! p i i))
(permute p 0)))

(print-all-perm-rec 3)
; 0 1 2
; 0 2 1
; 1 0 2
; 1 2 0
; 2 0 1
; 2 1 0
```

Completely recursive on lists:

```lisp
(define (perm s)
  (cond ((null? s) '())
	((null? (cdr s)) (list s))
	(else ;; extract each item in list in turn and perm the rest
	  (let splice ((l '()) (m (car s)) (r (cdr s)))
	    (append
	      (map (lambda (x) (cons m x)) (perm (append l r)))
	      (if (null? r) '()
		(splice (cons m l) (car r) (cdr r))))))))

(display (perm '(1 2 3)))
```



## Seed7


```seed7
$ include "seed7_05.s7i";

const type: permutations is array array integer;

const func permutations: permutations (in array integer: items) is func
  result
    var permutations: permsList is 0 times 0 times 0;
  local
    const proc: perms (in array integer: sequence, in array integer: prefix) is func
      local
        var integer: element is 0;
        var integer: index is 0;
      begin
        if length(sequence) <> 0 then
          for element key index range sequence do
            perms(sequence[.. pred(index)] & sequence[succ(index) ..], prefix & [] (element));
          end for;
        else
          permsList &:= prefix;
        end if;
      end func;
  begin
    perms(items, 0 times 0);
  end func;

const proc: main is func
  local
    var array integer: perm is 0 times 0;
    var integer: element is 0;
  begin
    for perm range permutations([] (1, 2, 3)) do
      for element range perm do
        write(element <& " ");
      end for;
      writeln;
    end for;
  end func;
```

```txt

1 2 3
1 3 2
2 1 3
2 3 1
3 1 2
3 2 1

```



## Shen


```Shen

(define permute
[] -> []
[X] -> [[X]]
X -> (permute-helper [] X))

(define permute-helper
_ [] -> []
Done [X|Rest] -> (append (prepend-all X (permute (append Done Rest))) (permute-helper [X|Done] Rest))
)

(define prepend-all
_ [] -> []
X [Next|Rest] -> [[X|Next]|(prepend-all X Rest)]
)

(set *maximum-print-sequence-size* 50)

(permute [a b c d])

```

```txt

[[a b c d] [a b d c] [a c b d] [a c d b] [a d c b] [a d b c] [b a c d] [b a d c] [b c a d] [b c d a] [b d c a] [b d a c] [c b a d] [c b d a] [c a b d] [c a d b] [c d a b] [c d b a] [d c b a] [d c a b] [d b c a] [d b a c] [d a b c] [d a c b]]

```

For lexical order, make a small change:

```Shen

(define permute-helper
_ [] -> []
Done [X|Rest] -> (append (prepend-all X (permute (append Done Rest))) (permute-helper (append Done [X]) Rest))
)

```



## Sidef

===Built-in===

```ruby
[0,1,2].permutations { |p|
    say p
}
```



### Iterative


```ruby
func forperm(callback, n) {
    var idx = @^n

    loop {
        callback([idx...])

        var p = n-1
        while (idx[p-1] > idx[p]) {--p}
        p == 0 && return()

        var d = p
        idx += idx.splice(p).reverse

        while (idx[p-1] > idx[d]) {++d}
        idx.swap(p-1, d)
    }

    return()
}

forperm({|p| say p }, 3)
```



### Recursive


```ruby
func permutations(callback, set, perm=[]) {
    set.is_empty && callback(perm)
    for i in ^set {
        __FUNC__(callback, [
            set[(0 ..^ i)..., (i+1 ..^ set.len)...]
        ], [perm..., set[i]])
    }
    return()
}

permutations({|p| say p }, [0,1,2])
```

```txt

[0, 1, 2]
[0, 2, 1]
[1, 0, 2]
[1, 2, 0]
[2, 0, 1]
[2, 1, 0]

```



## Smalltalk

```smalltalk
(1 to: 4) permutationsDo: [ :x |
	Transcript show: x printString; cr ].
```

```smalltalk

ArrayedCollection extend [

    permuteAndDo: aBlock
        ["Permute receiver in-place, and call aBlock.
        Requires integer keys."
        self permuteUpto: self size andDo: aBlock]

    permuteUpto: n andDo: aBlock
        [n = 0 ifTrue: [^aBlock value].
        1 to: n do:
            [:i |
            self swap: i with: n.
            self permuteUpto: n-1 andDo: aBlock.
            self swap: i with: n]]
]

SequenceableCollection extend [

    permutations
        ["Answer a ReadStream of permuted shallow copies of receiver."
        | c |
        c := MappedCollection
            collection: self
            map: self keys asArray.
        ^Generator on:
            [:g |
            c map permuteAndDo: [g yield: (c copyFrom: 1 to: c size)]]]

```


Use example:

```Smalltalk

st> 'Abc' permutations contents
('bcA' 'cbA' 'cAb' 'Acb' 'bAc' 'Abc' )

```



## Stata

Program to build a dataset containing all permutations of 1...n. Each permutation is stored as an observation.

For instance:


```stata>perm 4</lang


'''Program'''


```stata
program perm
	local n=`1'
	local r=1
	forv i=1/`n' {
		local r=`r'*`i'
	}
	clear
	qui set obs `r'
	forv i=1/`n' {
		gen p`i'=0
	}
	mata: genperm()
end

mata
void genperm() {
	real scalar n, i, j, k, s, p
	real rowvector u
	st_view(a=., ., .)
	n = cols(a)
	u = 1..n
	p = 1
	do {
		a[p++, .] = u
		for (i = n; i > 1; i--) {
			if (u[i-1] < u[i]) break
		}
		if (i > 1) {
			j = i
			k = n
			while (j < k) u[(j++, k--)] = u[(k, j)]

			s = u[i-1]
			for (j = i; u[j] < s; j++) {
			}
			u[i-1] = u[j]
			u[j] = s
		}
	} while (i > 1)
}
end
```



## Swift


```swift>func perms<T
(var ar: [T]) -> [[T]] {
  return heaps(&ar, ar.count)
}

func heaps<T>(inout ar: [T], n: Int) -> [[T]] {
  return n == 1 ? [ar] :
    Swift.reduce(0..<n, [[T]]()) {
      (var shuffles, i) in
      shuffles.extend(heaps(&ar, n - 1))
      swap(&ar[n % 2 == 0 ? i : 0], &ar[n - 1])
      return shuffles
  }
}

perms([1, 2, 3]) // [[1, 2, 3], [2, 1, 3], [3, 1, 2], [1, 3, 2], [2, 3, 1], [3, 2, 1]]
```



## Tailspin

This solution seems to be the same as the Kotlin solution.

```tailspin

templates permutations
  <1> [1] !
  <>
    def n: $;
    templates expand
      def p: $;
      1..$n -> (def k: $;
        [$p(1..$k-1)..., $n, $p($k..-1)...] !) !
    end expand
    $n - 1 -> permutations -> expand !
end permutations

def alpha: ['ABCD'...];
[ $alpha::length -> permutations -> '$alpha($)...;' ] -> !OUT::write

```

```txt

[DCBA, CDBA, CBDA, CBAD, DBCA, BDCA, BCDA, BCAD, DBAC, BDAC, BADC, BACD, DCAB, CDAB, CADB, CABD, DACB, ADCB, ACDB, ACBD, DABC, ADBC, ABDC, ABCD]

```


With a little more careful work we can output permutations in lexical order

```tailspin

templates lexicalPermutations
  <1> [1] !
  <>
    def n: $;
    def p: [ $n - 1 -> lexicalPermutations ];
    1..$n -> (def k: $;
      def tail: [1..$n -> (<~$k> $ !)];
      $p... -> [ $k, $tail($)...] !) !
end lexicalPermutations

def alpha: ['ABCD'...];
[ $alpha::length -> lexicalPermutations -> '$alpha($)...;' ] -> !OUT::write

```

```txt

[ABCD, ABDC, ACBD, ACDB, ADBC, ADCB, BACD, BADC, BCAD, BCDA, BDAC, BDCA, CABD, CADB, CBAD, CBDA, CDAB, CDBA, DABC, DACB, DBAC, DBCA, DCAB, DCBA]

```


The solutions above create a lot of new arrays at various stages. We can also use mutable state and just emit a copy for each generated solution.

```tailspin

templates perms
  templates findPerms
    <$@perms::length..> $@perms !
    <>
      def index: $;
      $index..$@perms::length
      -> (
          @perms([$, $index]): $@perms([$index, $])...;
          $index + 1 -> findPerms !
      ) !
      @perms([$@perms::length, $index..~$@perms::length]): $@perms($index..-1)...;
  end findPerms
  @: [1..$];
  1 -> findPerms !
end perms

def alpha: ['ABCD'...];
[4 -> perms -> '$alpha($)...;' ] -> !OUT::write

```

```txt

[ABCD, ABDC, ACBD, ACDB, ADBC, ADCB, BACD, BADC, BCAD, BCDA, BDAC, BDCA, CABD, CADB, CBAD, CBDA, CDAB, CDBA, DABC, DACB, DBAC, DBCA, DCAB, DCBA]

```



## Tcl

```tcl
package require struct::list

# Make the sequence of digits to be permuted
set n [lindex $argv 0]
for {set i 1} {$i <= $n} {incr i} {lappend sequence $i}

# Iterate over the permutations, printing as we go
struct::list foreachperm p $sequence {
    puts $p
}
```

Testing with <code>tclsh listPerms.tcl 3</code> produces this output:

```txt

1 2 3
1 3 2
2 1 3
2 3 1
3 1 2
3 2 1

```



## Ursala

In practice there's no need to write this because it's in the standard library.

```Ursala
#import std

permutations =

~&itB^?a(                     # are both the input argument list and its tail non-empty?
   @ahPfatPRD *= refer ^C(      # yes, recursively generate all permutations of the tail, and for each one
      ~&a,                        # insert the head at the first position
      ~&ar&& ~&arh2falrtPXPRD),   # if the rest is non-empty, recursively insert at all subsequent positions
   ~&aNC)                       # no, return the singleton list of the argument
```

test program:

```Ursala
#cast %nLL

test = permutations <1,2,3>
```

```txt
<
   <1,2,3>,
   <2,1,3>,
   <2,3,1>,
   <1,3,2>,
   <3,1,2>,
   <3,2,1>>
```



## VBA

```VB
Public Sub Permute(n As Integer, Optional printem As Boolean = True)
'Generate, count and print (if printem is not false) all permutations of first n integers

Dim P() As Integer
Dim t As Integer, i As Integer, j As Integer, k As Integer
Dim count As Long
Dim Last As Boolean

If n <= 1 Then

  Debug.Print "Please give a number greater than 1"
  Exit Sub

End If

'Initialize
ReDim P(n)

For i = 1 To n
  P(i) = i
Next

count = 0
Last = False

Do While Not Last
   'print?
   If printem Then

      For t = 1 To n
        Debug.Print P(t);
      Next

      Debug.Print

   End If

count = count + 1

Last = True
i = n - 1

   Do While i > 0

     If P(i) < P(i + 1) Then

       Last = False
       Exit Do

     End If

     i = i - 1
   Loop

  j = i + 1
  k = n

  While j < k
    ' Swap p(j) and p(k)
    t = P(j)
    P(j) = P(k)
    P(k) = t
    j = j + 1
    k = k - 1
  Wend

  j = n

  While P(j) > P(i)
    j = j - 1
  Wend

  j = j + 1
  'Swap p(i) and p(j)
  t = P(i)
  P(i) = P(j)
  P(j) = t
Loop 'While not last

Debug.Print "Number of permutations: "; count

End Sub
```

```txt

permute 1
give a number greater than 1!
permute 2
 1  2
 2  1
Number of permutations:  2
permute 4
 1  2  3  4
 1  2  4  3
 1  3  2  4
 1  3  4  2
 1  4  2  3
 1  4  3  2
 2  1  3  4
 2  1  4  3
 2  3  1  4
 2  3  4  1
 2  4  1  3
 2  4  3  1
 3  1  2  4
 3  1  4  2
 3  2  1  4
 3  2  4  1
 3  4  1  2
 3  4  2  1
 4  1  2  3
 4  1  3  2
 4  2  1  3
 4  2  3  1
 4  3  1  2
 4  3  2  1
Number of permutations:  24
permute 10,False
Number of permutations:  3628800

```



## XPL0


```XPL0
code ChOut=8, CrLf=9;
def  N=4;                       \number of objects (letters)
char S0, S1(N);

proc Permute(D);                \Display all permutations of letters in S0
int D;                          \depth of recursion
int I, J;
[if D=N then
        [for I:= 0 to N-1 do ChOut(0, S1(I));
        CrLf(0);
        return;
        ];
for I:= 0 to N-1 do
        [for J:= 0 to D-1 do    \check if object (letter) already used
                if S1(J) = S0(I) then J:=100;
        if J<100 then
                [S1(D):= S0(I); \object (letter) not used so append it
                Permute(D+1);   \recurse next level deeper
                ];
        ];
];

[S0:= "rose ";                  \N different objects (letters)
Permute(0);                     \(space char avoids MSb termination)
]
```


Output:

```txt

rose
roes
rsoe
rseo
reos
reso
orse
ores
osre
oser
oers
oesr
sroe
sreo
sore
soer
sero
seor
eros
erso
eors
eosr
esro
esor

```



## zkl

Using the solution from task [[Permutations by swapping#zkl]]:

```zkl
zkl: Utils.Helpers.permute("rose").apply("concat")
L("rose","roes","reos","eros","erso","reso","rseo","rsoe","sroe","sreo",...)

zkl: Utils.Helpers.permute("rose").len()
24

zkl: Utils.Helpers.permute(T(1,2,3,4))
L(L(1,2,3,4),L(1,2,4,3),L(1,4,2,3),L(4,1,2,3),L(4,1,3,2),L(1,4,3,2),L(1,3,4,2),L(1,3,2,4),...)
```

