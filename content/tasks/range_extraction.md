+++
title = "Range extraction"
description = ""
date = 2019-08-06T20:43:23Z
aliases = []
[extra]
id = 7772
[taxonomies]
categories = ["task"]
tags = []
+++

## Task

* Create a function that takes a list of integers in increasing order and returns a correctly formatted string in the range format.
* Use the function to compute and print the range formatted version of the following ordered list of integers. (The correct answer is: <code>0-2,4,6-8,11,12,14-25,27-33,35-39</code>).


     0,  1,  2,  4,  6,  7,  8, 11, 12, 14,
    15, 16, 17, 18, 19, 20, 21, 22, 23, 24,
    25, 27, 28, 29, 30, 31, 32, 33, 35, 36,
    37, 38, 39
* Show the output of your program.


## Related tasks

*   [[Range expansion]]





## Ada

The provided solutions return an empty string, if the Sequence of integers is empty.
Ranges with negative bounds are represented as '''-9--4''', as the task requires.
For real-life applications it is better to use the notation '''-9..-4'''.


### Iterative Solution


Since we don't know in advance how long the output will be,
the iterative solution uses Unbounded_Strings.


```Ada
with Ada.Text_IO;            use Ada.Text_IO;
with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;
with Ada.Strings.Fixed;      use Ada.Strings.Fixed;

procedure Range_Extraction is
   type Sequence is array (Positive range <>) of Integer;
   function Image (S : Sequence) return String is
      Result : Unbounded_String;
      From   : Integer;
      procedure Flush (To : Integer) is
      begin
         if Length (Result) > 0 then
            Append (Result, ',');
         end if;
         Append (Result, Trim (Integer'Image (From), Ada.Strings.Left));
         if From < To then
            if From+1 = To then
               Append (Result, ',');
            else
               Append (Result, '-');
            end if;
            Append (Result, Trim (Integer'Image (To), Ada.Strings.Left));
         end if;
      end Flush;
   begin
      if S'Length > 0 then
         From := S (S'First);
         for I in S'First + 1..S'Last loop
            if S (I - 1) + 1 /= S (I) then
               Flush (S (I - 1));
               From := S (I);
            end if;
         end loop;
         Flush (S (S'Last));
      end if;
      return To_String (Result);
   end Image;
begin
   Put_Line
     (  Image
          (  (  0,  1,  2,  4,  6,  7,  8, 11, 12, 14,
                15, 16, 17, 18, 19, 20, 21, 22, 23, 24,
                25, 27, 28, 29, 30, 31, 32, 33, 35, 36,
                37, 38, 39
             )  )  );
end Range_Extraction;
```




### Recursive Solution


The recursive solution avoids the usage of unbounded strings.


```Ada
with Ada.Text_IO, Ada.Strings.Fixed;

procedure Range_Extract is
   type Sequence is array (Positive range <>) of Integer;

   function Img(I: Integer) return String is -- the image of an Integer
   begin
      return
        Ada.Strings.Fixed.Trim(Integer'Image(I), Ada.Strings.Left);
   end Img;

   function Img(S: Sequence) return String is -- the image of a Sequence

      function X(S : Sequence) return String is -- recursive eXtract
         Idx: Positive := S'First;
      begin
         if S'Length = 0 then return
           ""; -- return nothing if Sequence is empty
         else
            while Idx < S'Last and then S(Idx+1) = S(Idx) + 1 loop
               Idx := Idx + 1;
            end loop;
            if Idx = S'First then return
              "," & Img(S(Idx)) & X(S(Idx+1 .. S'Last));
            elsif Idx = S'First+1 then return
              "," & Img(S(S'First)) & ',' & Img(S(Idx)) & X(S(Idx+1 .. S'Last));
            else return
              "," & Img(S(S'First)) & '-' & Img(S(Idx)) & X(S(Idx+1 .. S'Last));
            end if;
         end if;
      end X;

   begin -- function Img(S: Sequence) return String
      if S'Length = 0 then return
        "";
      else return
        Img(S(S'First)) & X(S(S'First+1 .. S'Last));
      end if;
   end Img;

begin -- main
   Ada.Text_IO.Put_Line(Img( ( 0,  1,  2,  4,  6,  7,  8, 11, 12, 14, 15, 16,
                               17, 18, 19, 20, 21, 22, 23, 24, 25, 27, 28, 29,
                               30, 31, 32, 33, 35, 36, 37, 38, 39) ));
end Range_Extract;
```


The sample output is exactly the same, for both solutions:

```txt
0-2,4,6-8,11,12,14-25,27-33,35-39
```



## Aime



```aime
rp(list l)
{
    integer a, i;
    data b;
    index x;

    a = l[0];
    x[a] = a;
    for (, a in l) {
        x[a == x.back + 1 ? x.high : a] = a;
    }
    for (i, a in x) {
        b.form(a - i < 2 ? a - i ? "~,~," : "~," : "~-~,", i, a);
    }

    b.delete(-1);
}

main(void)
{
    o_(rp(list(0, 1, 2, 4, 6, 7, 8, 11, 12, 14, 15, 16, 17, 18, 19, 20, 21, 22,
               23, 24, 25, 27, 28, 29, 30, 31, 32, 33, 35, 36, 37, 38, 39)),
       "\n");

    0;
}
```


```txt
0-2,4,6-8,11,12,14-25,27-33,35-39
```



## ALGOL 68

Note: The following [[Range extraction#Iterative|Iterative]] code specimen is the "unrolled" version of the [[Range extraction#Generative|Generative]] code specimen below. Together they provided as a comparison of the two different methods.


###  Iterative

* The closest concept that ''Algol 68'' has to [[wp:duck typing|duck typing]] is the [[wp:tagged union|tagged union]].  This is used to define '''mode''' '''urange''' = '''union'''('''int''', '''struct'''('''int''' lwb, upb)). If ''duck typing'' was available it could reduced the size of the code specimen, but would have lost some of <i>Algol 68</i>'s strong type ''data security''.
'''File: Template_Range_extraction_Base.a68'''

```algol68
###
  REQUIRES(MODE SCALAR, OP(SCALAR,SCALAR)BOOL =, OP(SCALAR,SCALAR)SCALAR +);
###
MODE SCALARLIST = FLEX[0]SCALAR;
MODE YIELDINT = PROC(SCALAR)VOID;

################################################################
# Declarations for manipulating lists of range pairs [lwb:upb] #
################################################################
MODE RANGE = STRUCT(SCALAR lwb, upb);
MODE RANGELIST = FLEX[0]RANGE;
MODE YIELDRANGE = PROC(RANGE)VOID;

PROC range repr = (RANGE range)STRING: (
  STRING lwb := whole(lwb OF range,0);
  IF lwb OF range = upb OF range THEN
    lwb
  ELSE
    # "["+lwb+":"+whole(upb OF range,0)+"]"  #
    lwb+"-"+whole(upb OF range,0)
  FI
);

# OP REPR = (RANGE range)STRING: range repr(range); # # firmly related to UNIRANGE #

######################################################################
# Declarations for manipulating lists containing pairs AND lone INTs #
######################################################################
MODE UNIRANGE = UNION(SCALAR, RANGE);
MODE UNIRANGELIST = FLEX[0]UNIRANGE;
MODE YIELDUNIRANGE = PROC(UNIRANGE)VOID;

PROC unirange repr = (UNIRANGE unirange)STRING:
  CASE unirange IN
    (RANGE range): range repr(range),
    (SCALAR scalar): whole(scalar,0)
  ESAC;

OP (UNIRANGE)STRING REPR = unirange repr; # alias #

# The closest thing Algol68 has to inheritance is the UNION #
MODE UNIRANGELISTS = UNION(UNIRANGELIST, RANGELIST, SCALARLIST);

PROC unirange list repr = (UNIRANGELIST unirange list)STRING: (
### Produce a STRING representation of a UNIRANGELIST ###
  STRING out # := "("#, sep := "";
  FOR key FROM LWB unirange list TO UPB unirange list DO
    out +:= sep + REPR unirange list[key];
    sep := "," # +" " #
  OD;
  out # +")" #
);

OP (UNIRANGELIST)STRING REPR = unirange list repr; # alias #
```
'''File: Template_Range_extraction_Iterative.a68'''

```algol68
###
  REQUIRES(MODE SCALAR, OP(SCALAR,SCALAR)BOOL =, OP(SCALAR,SCALAR)SCALAR +);
###
PR READ "Template_Range_extraction_Base.a68" PR

OP (UNIRANGELISTS)UNIRANGELIST INITUNIRANGE = init unirange list; # alias #

PROC init unirange list = (UNIRANGELISTS unirange list)UNIRANGELIST: (
### Take a []SCALAR, []RANGE or []UNIRANGE, and return a normalised []UNIRANGE ###

  INT len = UPB unirange list-LWB unirange list+1;
  [LWB unirange list: LWB unirange list+len*2]UNIRANGE out unirange list;
  SCALAR upb out unirange list := LWB out unirange list - 1;
  UNION(VOID, RANGE) prev range := EMPTY;

  PROC out unirange list append = (RANGE value)VOID:(

    IF lwb OF value = upb OF value THEN
      out unirange list[upb out unirange list+:=1] := lwb OF value
    ELIF lwb OF value + 1 = upb OF value THEN
      out unirange list[upb out unirange list+:=1] := lwb OF value;
      out unirange list[upb out unirange list+:=1] := upb OF value
    ELSE
      out unirange list[upb out unirange list+:=1] := value
    FI
  );

  FOR key FROM LWB unirange list TO UPB unirange list DO
    UNIRANGE value = CASE unirange list IN
                       (SCALARLIST list):list[key],
                       (RANGELIST list):list[key],
                       (UNIRANGELIST list):list[key]
                     ESAC;

    RANGE next range := CASE value IN
        (RANGE range): range,
        (SCALAR value): RANGE(value, value)
      ESAC;

    prev range :=
      CASE prev range IN
        (VOID): next range,
        (RANGE prev range):
          IF upb OF prev range + 1 = lwb OF next range THEN
            RANGE(lwb OF prev range, upb OF next range) # merge the range #
          ELSE
            out unirange list append(prev range);
            next range
          FI
        OUT SKIP
      ESAC

  OD;

  CASE prev range IN
    (RANGE last range): out unirange list append(last range)
  ESAC;

  out unirange list[:upb out unirange list]
);
```
'''File: test_Range_extraction_Integer.a68'''
```algol68
#!/usr/local/bin/a68g --script #
############################
# some simple test cases:  #
############################

MODE SCALAR = INT;
PR READ "Template_Range_extraction_Iterative.a68" PR
#PR READ "Template_Range_extraction_Generative.a68" PR#
MODE RANGEINT = UNIRANGE;

test: BEGIN
  []INT int list = ( # unnormalised #
    0,  1,  2,  4,  6,  7,  8, 11, 12, 14,
    15, 16, 17, 18, 19, 20, 21, 22, 23, 24,
    25, 27, 28, 29, 30, 31, 32, 33, 35, 36,
    37, 38, 39);

  []RANGE range list = ( # unnormalised #
    (0,0),  (1,1),  (2,2),  (4,4),  (6,6),  (7,7),  (8,8), (11,11), (12,12), (14,14),
    (15,15), (16,16), (17,17), (18,18), (19,19), (20,20), (21,21), (22,22), (23,23), (24,24),
    (25,25), (27,27), (28,28), (29,29), (30,30), (31,31), (32,32), (33,33), (35,35), (36,36),
    (37,37), (38,38), (39,39));

  []RANGEINT list a = ( # unnormalised #
    RANGE(0,2), 4, RANGE(6,8), RANGE(11,12),
    RANGE(14,25), RANGE(27,33), RANGE(35,39));

  []RANGEINT list b = ( # unnormalised #
    0,  1,  2,  4,  6,  7,  8, 11, 12, 14,
    15, 16, 17, 18, 19, 20, 21, 22, 23, 24,
    25, 27, 28, 29, 30, 31, 32, 33, 35, 36,
    37, 38, 39);

  []RANGEINT list c = INITUNIRANGE(list b); # normalised #

# compare manipulation of various types of argument lists #
  printf(($gl$,
    REPR INITUNIRANGE int list,
    REPR INITUNIRANGE range list,
    REPR INITUNIRANGE list a,
    REPR INITUNIRANGE list b,
    REPR list c
  ))
END
```


<pre style="height:15ex;overflow:scroll">
0-2,4,6-8,11,12,14-25,27-33,35-39
0-2,4,6-8,11,12,14-25,27-33,35-39
0-2,4,6-8,11,12,14-25,27-33,35-39
0-2,4,6-8,11,12,14-25,27-33,35-39
0-2,4,6-8,11,12,14-25,27-33,35-39

```



###  Generative

* The following code a set of helper functions/generators that can be used to manipulate a ''list''s of ranges.  They can manipulate either [[wp:array|array]]s ''or'' [[wp:iterators|iterator]].  And they can handle data of type '''int''' ''or'' '''range''' ''and'' both these types '''unioned'''.

These chained iterators do the following steps:
# Iterate through three different types of initial arrays - []'''int''', []'''range''' and []'''unirange''' with ''gen range'', yielding '''range'''(''lwb'',''upb'')
# Iterate with ''gen range merge'' yielding <u>merged</u> '''range'''(''lwb'',''upb'')
# Iterate with ''gen unirange merge'', merging and yielding a '''union''' of '''int''' and '''range'''
# Finally iterate with ''unirange list init'' '''exiting''' with an array of '''union''' of '''int''' and '''range'''.
'''File: Template_Range_extraction_Generative.a68'''

```algol68
###
  REQUIRES(MODE SCALAR, OP(SCALAR,SCALAR)BOOL =, OP(SCALAR,SCALAR)SCALAR +);
###
PR READ "Template_Range_extraction_Base.a68" PR

PROC gen range = (UNIRANGELISTS unirange list, YIELDRANGE yield range)VOID:
### Take a []SCALAR, []RANGE or []URANGE, and generatively yield an unnormalised RANGE ###

  FOR key FROM LWB unirange list TO UPB unirange list DO
# Note: Algol 68RS cannot handle LWB and UPB of a UNION in the following: #
    UNIRANGE value = CASE unirange list IN
                       (SCALARLIST list):list[key],
                       (RANGELIST list):list[key],
                       (UNIRANGELIST list):list[key]
                     ESAC;
    yield range(
      CASE value IN
        (RANGE range): range,
        (SCALAR value): (value, value)
      ESAC
    )
  OD;

PROC gen range merge = (UNIRANGELISTS unirange list, YIELDRANGE yield)VOID: (
### Take a []SCALAR, []RANGE or []URANGE , and generatively yield a normalised RANGE ###

  UNION(VOID, RANGE) prev range := EMPTY;

# FOR RANGE next range IN # gen range(unirange list, # ) DO #
##   (RANGE next range)VOID:
# if the ranges cannot be merge, then yield 1st, and return 2nd #
    prev range :=
      CASE prev range IN
        (VOID): next range,
        (RANGE prev range):
          IF upb OF prev range + 1 = lwb OF next range THEN
            RANGE(lwb OF prev range, upb OF next range) # merge the range #
          ELSE
            #IF lwb OF prev range <= upb OF prev range THEN#
              yield(prev range);
            #FI;#
            next range
          FI
        OUT SKIP
      ESAC
# OD # );

  CASE prev range IN (RANGE last range): yield(last range) ESAC
);

PROC gen unirange merge = (UNIRANGELISTS unirange list, YIELDUNIRANGE yield)VOID: (
### Take a []SCALAR, []RANGE or []UNIRANGE and generatively yield a normalised UNIRANGE ###

  PROC unpack = (RANGE value)VOID:(
    IF lwb OF value = upb OF value THEN
      yield(lwb OF value)
    ELIF lwb OF value + 1 = upb OF value THEN
      yield(lwb OF value);
      yield(upb OF value)
    ELSE
      yield(value)
    FI
  );

  gen range merge(unirange list, unpack)
);

PROC unirange list init = (UNIRANGELISTS unirange list)UNIRANGELIST: (
### Take a []SCALAR, []RANGE or []UNIRANGE and return a static []UNIRANGE ###

  INT len = UPB unirange list - LWB unirange list + 1;
  [LWB unirange list: LWB unirange list + len * 2]UNIRANGE out unirange list; # estimate #
  SCALAR upb out unirange list := LWB out unirange list - 1;

# FOR UNIRANGE unirange IN # gen unirange merge(unirange list, # ) DO #
##   (UNIRANGE unirange)VOID:
    out unirange list[upb out unirange list+:=1] := unirange
# OD # );

  out unirange list[:upb out unirange list]
);

OP (UNIRANGELISTS)UNIRANGELIST INITUNIRANGE = unirange list init; # alias #
```

<pre style="height:15ex;overflow:scroll">
0-2,4,6-8,11,12,14-25,27-33,35-39
0-2,4,6-8,11,12,14-25,27-33,35-39
0-2,4,6-8,11,12,14-25,27-33,35-39
0-2,4,6-8,11,12,14-25,27-33,35-39
0-2,4,6-8,11,12,14-25,27-33,35-39

```




## AppleScript

```AppleScript
-- rangeFormat :: [Int] -> String
on rangeFormat(xs)
    script rangeString
        on |λ|(xs)
            if length of xs > 2 then
                (item 1 of xs as string) & "-" & (item -1 of xs as string)
            else
                intercalate(",", xs)
            end if
        end |λ|
    end script

    script nonConsec
        on |λ|(a, b)
            b - a > 1
        end |λ|
    end script

    intercalate(",", map(rangeString, splitBy(nonConsec, xs)))
end rangeFormat


--TEST ------------------------------------------------------------------------
on run
    set xs to {0, 1, 2, 4, 6, 7, 8, 11, 12, 14, 15, 16, ¬
        17, 18, 19, 20, 21, 22, 23, 24, 25, 27, 28, 29, 30, 31, 32, ¬
        33, 35, 36, 37, 38, 39}

    rangeFormat(xs)

    --> "0-2,4,6-8,11,12,14-25,27-33,35-39"
end run


-- GENERIC FUNCTIONS ----------------------------------------------------------

-- splitBy :: (a -> a -> Bool) -> [a] -> [[a]]
on splitBy(f, xs)
    set mf to mReturn(f)

    if length of xs < 2 then
        {xs}
    else
        script p
            on |λ|(a, x)
                set {acc, active, prev} to a
                if mf's |λ|(prev, x) then
                    {acc & {active}, {x}, x}
                else
                    {acc, active & x, x}
                end if
            end |λ|
        end script

        set h to item 1 of xs
        set lstParts to foldl(p, {{}, {h}, h}, items 2 thru -1 of xs)
        item 1 of lstParts & {item 2 of lstParts}
    end if
end splitBy

-- foldl :: (a -> b -> a) -> a -> [b] -> a
on foldl(f, startValue, xs)
    tell mReturn(f)
        set v to startValue
        set lng to length of xs
        repeat with i from 1 to lng
            set v to |λ|(v, item i of xs, i, xs)
        end repeat
        return v
    end tell
end foldl

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

-- intercalate :: Text -> [Text] -> Text
on intercalate(strText, lstText)
    set {dlm, my text item delimiters} to {my text item delimiters, strText}
    set strJoined to lstText as text
    set my text item delimiters to dlm
    return strJoined
end intercalate

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
```

```txt
0-2,4,6-8,11,12,14-25,27-33,35-39
```



## AutoHotkey


```AutoHotkey
msgbox % extract("0,1,2,4,6,7,8,11,12,14,15,16,17,18,19,20,21,22,23,24,25,27,28,29,30,31,32,33,35,36,37,38,39")

extract( list ) {
    loop, parse, list, `,, %A_Tab%%A_Space%`r`n
    {
        if (A_LoopField+0 != p+1)
            ret .= (f!=p ? (p>f+1 ? "-" : ",") p : "") "," f := A_LoopField
        p := A_LoopField
    }
    return SubStr(ret (f!=p ? (p>f+1 ? "-" : ",") p : ""), 2)
}
```

```txt
---------------------------
Range extraction.ahk
---------------------------
0-2,4,6-8,11,12,14-25,27-33,35-39
---------------------------
OK
---------------------------
```



## AWK


AWK is a primitive bird that prefers global scope for arrays.

Local variables for functions are declared in the parameters and,
by convention, separated from the expected ones by extra space.


```awk
#!/usr/bin/awk -f

BEGIN {
    delete sequence
    delete range

    seqStr = "0,1,2,4,6,7,8,11,12,14,15,16,17,18,19,20,21,22,23,24,"
    seqStr = seqStr "25,27,28,29,30,31,32,33,35,36,37,38,39"
    print "Sequence: " seqStr
    fillSequence(seqStr)
    rangeExtract()
    showRange()
    exit
}

function rangeExtract(    runStart, runLen) {
    delete range
    runStart = 1
    while(runStart <= length(sequence)) {
        runLen = getSeqRunLen(runStart)
        addRange(runStart, runLen)
        runStart += runLen
    }
}

function getSeqRunLen(startPos,    pos) {
    for (pos = startPos; pos < length(sequence); pos++) {
        if (sequence[pos] + 1 != sequence[pos + 1]) break;
    }
    return pos - startPos + 1;
}

function addRange(startPos, len,    str) {
    if (len == 1) str = sequence[startPos]
    else if (len == 2) str = sequence[startPos] "," sequence[startPos + 1]
    else str = sequence[startPos] "-" sequence[startPos + len - 1]
    range[length(range) + 1] = str
}

function showRange(    r) {
    printf "  Ranges: "
    for (r = 1; r <= length(range); r++) {
        if (r > 1) printf ","
        printf range[r]
    }
    printf "\n"
}

function fillSequence(seqStr,    n, s) {
    n = split(seqStr,a,/[,]+/)
    for (s = 1; s <= n; s++) {
        sequence[s] = a[s]
    }
}
```


```txt

 Sequence: 0,1,2,4,6,7,8,11,12,14,15,16,17,18,19,20,21,22,23,24,25,27,28,29,30,31,32,33,35,36,37,38,39
   Ranges: 0-2,4,6-8,11,12,14-25,27-33,35-39

```



## BBC BASIC

```bbcbasic
      range$ = " 0,  1,  2,  4,  6,  7,  8, 11, 12, 14, " + \
      \        "15, 16, 17, 18, 19, 20, 21, 22, 23, 24, " + \
      \        "25, 27, 28, 29, 30, 31, 32, 33, 35, 36, " + \
      \        "37, 38, 39"
      PRINT FNrangeextract(range$)
      END

      DEF FNrangeextract(r$)
      LOCAL f%, i%, r%, t%, t$
      f% = VAL(r$)
      REPEAT
        i% = INSTR(r$, ",", i%+1)
        t% = VALMID$(r$, i%+1)
        IF t% = f% + r% + 1 THEN
          r% += 1
        ELSE
          CASE r% OF
            WHEN 0: t$ += STR$(f%) + ","
            WHEN 1: t$ += STR$(f%) + "," + STR$(f% + r%) + ","
            OTHERWISE: t$ += STR$(f%) + "-" + STR$(f% + r%) + ","
          ENDCASE
          r% = 0
          f% = t%
        ENDIF
      UNTIL i% = 0
      = LEFT$(t$)
```

```txt

0-2,4,6-8,11,12,14-25,27-33,35-39

```



## Bracmat


```bracmat
  ( rangeExtract
  =     accumulator firstInRange nextInRange
      , accumulate fasten rangePattern
    .   ( accumulate
        =     !accumulator
              (!accumulator:|?&",")
              !firstInRange
              (   !firstInRange+1:<>!nextInRange
                &   ( !firstInRange+2:!nextInRange&","
                    | "-"
                    )
                    -1+!nextInRange
              |
              )
          : ?accumulator
        )
      & ( fasten
        = [%( !accumulate
            & (!sjt:?firstInRange)+1:?nextInRange
            )
        )
      & ( rangePattern
        =   (
            |   ?
                ( !nextInRange
                & 1+!nextInRange:?nextInRange
                )
            )
            ( &!accumulate
            | (#<>!nextInRange:!fasten) !rangePattern
            )
        )
      & :?accumulator:?firstInRange
      & !arg:(|#!fasten !rangePattern)
      & str$!accumulator
  )
& ( test
  =   L A
    .   put$(!arg " ==>\n",LIN)
      & (   !arg:(?,?)
          & whl'(!arg:(?A,?arg)&(!A,!L):?L)
          & whl'(!L:(?A,?L)&!A !arg:?arg)
        |
        )
      & out$(rangeExtract$!arg)
  )
&   test
  $ (0,  1,  2,  4,  6,  7,  8, 11, 12, 14,
    15, 16, 17, 18, 19, 20, 21, 22, 23, 24,
    25, 27, 28, 29, 30, 31, 32, 33, 35, 36,
    37, 38, 39)
```

```txt
(0,1,2,4,6,7,8,11,12,14,15,16,17,18,19,20,21,22,23,24,25,27,28,29,30,31,32,33,35,36,37,38,39)  ==>
0-2,4,6-8,11,12,14-25,27-33,35-39
```



## C

Using the fine tradition of <code>snprintf</code>, <code>rprint</code> is not responsible for allocating output buffer.
It prints the range only if supplied a non-null pointer,
but always returns the output length sans the terminating null,
so caller can allocate buffer.

```c
#include <stdio.h>
#include <stdlib.h>

size_t rprint(char *s, int *x, int len)
{
#define sep (a > s ? "," : "") /* use comma except before first output */
#define ol (s ? 100 : 0)       /* print only if not testing for length */
	int i, j;
	char *a = s;
	for (i = j = 0; i < len; i = ++j) {
		for (; j < len - 1 && x[j + 1] == x[j] + 1; j++);

		if (i + 1 < j)
			a += snprintf(s?a:s, ol, "%s%d-%d", sep, x[i], x[j]);
		else
			while (i <= j)
				a += snprintf(s?a:s, ol, "%s%d", sep, x[i++]);
	}
	return a - s;
#undef sep
#undef ol
}

int main()
{
	int x[] = {	0,  1,  2,  4,  6,  7,  8, 11, 12, 14,
			15, 16, 17, 18, 19, 20, 21, 22, 23, 24,
			25, 27, 28, 29, 30, 31, 32, 33, 35, 36,
			37, 38, 39 };

	char *s = malloc(rprint(0, x, sizeof(x) / sizeof(int)) + 1);
	rprint(s, x, sizeof(x) / sizeof(int));
	printf("%s\n", s);

	return 0;
}
```

```txt
0-2,4,6-8,11,12,14-25,27-33,35-39
```



## C++



```cpp

#include <iostream>
#include <iterator>
#include <cstddef>

template<typename InIter>
 void extract_ranges(InIter begin, InIter end, std::ostream& os)
{
  if (begin == end)
    return;

  int current = *begin++;
  os << current;
  int count = 1;

  while (begin != end)
  {
    int next = *begin++;
    if (next == current+1)
      ++count;
    else
    {
      if (count > 2)
        os << '-';
      else
        os << ',';
      if (count > 1)
        os << current << ',';
      os << next;
      count = 1;
    }
    current = next;
  }

  if (count > 1)
    os << (count > 2? '-' : ',') << current;
}

template<typename T, std::size_t n>
 T* end(T (&array)[n])
{
  return array+n;
}

int main()
{
  int data[] = { 0,  1,  2,  4,  6,  7,  8, 11, 12, 14,
                 15, 16, 17, 18, 19, 20, 21, 22, 23, 24,
                 25, 27, 28, 29, 30, 31, 32, 33, 35, 36,
                 37, 38, 39 };

  extract_ranges(data, end(data), std::cout);
  std::cout << std::endl;
}

```

```txt

 0-2,4,6-8,11,12,14-25,27-33,35-39

```



## C#


```c#
using System;
using System.Collections.Generic;
using System.Linq;

class RangeExtraction
{
    static void Main()
    {
        const string testString = "0,  1,  2,  4,  6,  7,  8, 11, 12, 14,15, 16, 17, 18, 19, 20, 21, 22, 23, 24,25, 27, 28, 29, 30, 31, 32, 33, 35, 36,37, 38, 39";
        var result = String.Join(",", RangesToStrings(GetRanges(testString)));
        Console.Out.WriteLine(result);
    }

    public static IEnumerable<IEnumerable<int>> GetRanges(string testString)
    {
        var numbers = testString.Split(new[] { ',' }).Select(x => Convert.ToInt32(x));
        var current = new List<int>();
        foreach (var n in numbers)
        {
            if (current.Count == 0)
            {
                current.Add(n);
            }
            else
            {
                if (current.Max() + 1 == n)
                {
                    current.Add(n);
                }
                else
                {
                    yield return current;
                    current = new List<int> { n };
                }
            }
        }
        yield return current;
    }

    public static IEnumerable<string> RangesToStrings(IEnumerable<IEnumerable<int>> ranges)
    {
        foreach (var range in ranges)
        {
            if (range.Count() == 1)
            {
                yield return range.Single().ToString();
            }
            else if (range.Count() == 2)
            {
                yield return range.Min() + "," + range.Max();
            }
            else
            {
                yield return range.Min() + "-" + range.Max();
            }
        }
    }
}

```


```txt
0-2,4,6-8,11,12,14-25,27-33,35-39
```



### C#: Alternate Version


```c#
using System;
using System.Collections.Generic;
using System.Linq;

public class RangeExtraction
{
    public static void Main()
    {
        string s = "0,1,2,4,6,7,8,11,12,14,15, 16, 17, 18, 19, 20, 21, 22, 23, 24,25, 27, 28, 29, 30, 31, 32, 33, 35, 36,37, 38, 39";
        Console.WriteLine(string.Join(",", Ranges(s.Split(',').Select(int.Parse))
            .Select(r => r.end == r.start ? $"{r.start}" : $"{r.start}-{r.end}")));
    }

    static IEnumerable<(int start, int end)> Ranges(IEnumerable<int> numbers) {
        if (numbers == null) yield break;
        var e = numbers.GetEnumerator();
        if (!e.MoveNext()) yield break;

        int start = e.Current;
        int end = start;
        while (e.MoveNext()) {
            if (e.Current - end != 1) {
                if (end - start == 1) {
                    yield return (start, start);
                    yield return (end, end);
                } else {
                    yield return (start, end);
                }
                start = e.Current;
            }
            end = e.Current;
        }
        yield return (start, end);
    }

}
```

```txt

0-2,4,6-8,11,12,14-25,27-33,35-39

```



## Ceylon


```ceylon
shared void run() {

	value numbers = [
		0,  1,  2,  4,  6,  7,  8, 11, 12, 14,
		15, 16, 17, 18, 19, 20, 21, 22, 23, 24,
		25, 27, 28, 29, 30, 31, 32, 33, 35, 36,
		37, 38, 39
	];

	function asRangeFormattedString<Value>([Value*] values)
			given Value satisfies Enumerable<Value> {

		value builder = StringBuilder();

		void append(Range<Value> range) {
			if(!builder.empty) {
				builder.append(",");
			}
			if(1 <= range.size < 3) {
				builder.append(",".join(range));
			} else {
				builder.append("``range.first``-``range.last``");
			}
		}

		if(nonempty values) {
			variable value currentRange = values.first..values.first;
			for(val in values.rest) {
				if(currentRange.last.successor == val) {
					currentRange = currentRange.first..val;
				} else {
					append(currentRange);
					currentRange = val..val;
				}
			}
			append(currentRange);
		}
		return builder.string;
	}

	value rangeString = asRangeFormattedString(numbers);
	assert(rangeString == "0-2,4,6-8,11,12,14-25,27-33,35-39");
	print(rangeString);
}
```

```txt
0-2,4,6-8,11,12,14-25,27-33,35-39
```



## Clojure

```clojure
(use '[flatland.useful.seq :only (partition-between)])

(defn nonconsecutive? [[x y]]
  (not= (inc x) y))

(defn string-ranges [coll]
  (let [left (first coll)
        size (count coll)]
    (cond
      (> size 2) (str left "-" (last coll))
      (= size 2) (str left "," (last coll))
      :else (str left))))

(defn format-with-ranges [coll]
  (println (clojure.string/join ","
    (map string-ranges (partition-between nonconsecutive? coll)))))
```


```txt

=> (format-with-ranges [0 1 2 4 6 7 8 11 12 14 15 16 17 18 19 20 21 22 23 24 25 27 28 29 30 31 32 33 35 36 37 38 39])
0-2,4,6-8,11,12,14-25,27-33,35-39

```



## COBOL

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. extract-range-task.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  data-str                PIC X(200) VALUE "0,  1,  2,  4,  6,"
           & " 7,  8, 11, 12, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, "
           & "24, 25, 27, 28, 29, 30, 31, 32, 33, 35, 36, 37, 38, 39".

       01  result                  PIC X(200).

       PROCEDURE DIVISION.
           CALL "extract-range" USING CONTENT data-str, REFERENCE result
           DISPLAY FUNCTION TRIM(result)

           GOBACK
           .
       END PROGRAM extract-range-task.


       IDENTIFICATION DIVISION.
       PROGRAM-ID. extract-range.

       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       COPY "nums-table.cpy".

       01  difference              PIC 999.

       01  rng-begin               PIC S999.
       01  rng-end                 PIC S999.

       01  num-trailing            PIC 999.

       01  trailing-comma-pos      PIC 999.

       LINKAGE SECTION.
       01  nums-str                PIC X(200).
       01  extracted-range         PIC X(200).

       01  extracted-range-len     CONSTANT LENGTH extracted-range.

       PROCEDURE DIVISION USING nums-str, extracted-range.
           CALL "split-nums" USING CONTENT nums-str, ", ",
               REFERENCE nums-table

           *> Process the table
           MOVE nums (1) TO rng-begin
           PERFORM VARYING nums-idx FROM 2 BY 1
                   UNTIL num-nums < nums-idx
               SUBTRACT nums (nums-idx - 1) FROM nums (nums-idx)
                   GIVING difference

               *> If number is more than one away from the previous one
               *> end the range and start a new one.
               IF difference > 1
                   MOVE nums (nums-idx - 1) TO rng-end
                   CALL "add-next-range" USING CONTENT rng-begin,
                       rng-end, REFERENCE extracted-range
                   MOVE nums (nums-idx) TO rng-begin
               END-IF
           END-PERFORM

           *> Process the last number
           MOVE nums (num-nums) TO rng-end
           CALL "add-next-range" USING CONTENT rng-begin,
               rng-end, REFERENCE extracted-range

           *> Remove trailing comma.
           CALL "find-num-trailing-spaces"
               USING CONTENT extracted-range, REFERENCE num-trailing
           COMPUTE trailing-comma-pos =
               extracted-range-len - num-trailing
           MOVE SPACE TO extracted-range (trailing-comma-pos:1)

           GOBACK
           .

       IDENTIFICATION DIVISION.
       PROGRAM-ID. split-nums INITIAL.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  num-len                 PIC 9.
       01  next-num-pos            PIC 999.

       LINKAGE SECTION.
       01  str                     PIC X(200).
       01  delim                   PIC X ANY LENGTH.

       COPY "nums-table.cpy".

       PROCEDURE DIVISION USING str, delim, nums-table.
           INITIALIZE num-nums

           PERFORM UNTIL str = SPACES
               INITIALIZE num-len
               INSPECT str TALLYING num-len FOR CHARACTERS BEFORE delim

               ADD 1 TO num-nums

               *> If there are no more instances of delim in the string,
               *> add the rest of the string to the last element of the
               *> table.
               IF num-len = 0
                   MOVE str TO nums (num-nums)
                   EXIT PERFORM
               ELSE
                   MOVE str (1:num-len) TO nums (num-nums)
                   ADD 3 TO num-len GIVING next-num-pos
                   MOVE str (next-num-pos:) TO str
               END-IF
           END-PERFORM
           .
       END PROGRAM split-nums.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. add-next-range INITIAL.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  num-trailing            PIC 999.
       01  start-pos               PIC 999.

       01  range-len               PIC 999.

       01  begin-edited            PIC -ZZ9.
       01  end-edited              PIC -ZZ9.

       LINKAGE SECTION.
       01  rng-begin               PIC S999.
       01  rng-end                 PIC S999.

       01  extracted-range         PIC X(200).

       01  extracted-range-len     CONSTANT LENGTH extracted-range.

       PROCEDURE DIVISION USING rng-begin, rng-end, extracted-range.
           CALL "find-num-trailing-spaces"
               USING CONTENT extracted-range, REFERENCE num-trailing
           COMPUTE start-pos = extracted-range-len - num-trailing + 1

           SUBTRACT rng-begin FROM rng-end GIVING range-len

           MOVE rng-begin TO begin-edited
           MOVE rng-end TO end-edited

           EVALUATE TRUE
               WHEN rng-begin = rng-end
                   STRING FUNCTION TRIM(begin-edited), ","
                       INTO extracted-range (start-pos:)

               WHEN range-len = 1
                   STRING FUNCTION TRIM(begin-edited), ",",
                       FUNCTION TRIM(end-edited), ","
                       INTO extracted-range (start-pos:)

               WHEN OTHER
                   STRING FUNCTION TRIM(begin-edited), "-",
                         FUNCTION TRIM(end-edited), ","
                         INTO extracted-range (start-pos:)
           END-EVALUATE
           .
       END PROGRAM add-next-range.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. find-num-trailing-spaces.

       DATA DIVISION.
       LINKAGE SECTION.
       01  str                     PIC X(200).
       01  num-trailing            PIC 999.

       PROCEDURE DIVISION USING str, num-trailing.
           INITIALIZE num-trailing
           INSPECT str TALLYING num-trailing FOR TRAILING SPACES
           .
       END PROGRAM find-num-trailing-spaces.

       END PROGRAM extract-range.
```


nums-table.cpy:

```cobol
       01  nums-table.
           03  num-nums            PIC 999.
           03  nums-area.
               05  nums            PIC S999 OCCURS 1 TO 100 TIMES
                                   DEPENDING ON num-nums
                                   INDEXED BY nums-idx.
```


```txt

0-2,4,6-8,11,12,14-25,27-33,35-39

```



## Common Lisp


```lisp
(defun format-with-ranges (list)
  (unless list (return ""))
  (with-output-to-string (s)
    (let ((current (first list))
          (list    (rest list))
          (count   1))
      (princ current s)
      (dolist (next list)
        (if (= next (1+ current))
            (incf count)
            (progn (princ (if (> count 2) "-" ",") s)
                   (when (> count 1)
                     (princ current s)
                     (princ "," s))
                   (princ next s)
                   (setf count 1)))
        (setf current next))
      (when (> count 1)
        (princ (if (> count 2) "-" ",") s)
        (princ current s)))))

CL-USER> (format-with-ranges (list 0  1  2  4  6  7  8 11 12 14
                                   15 16 17 18 19 20 21 22 23 24
                                   25 27 28 29 30 31 32 33 35 36
                                   37 38 39))
"0-2,4,6-8,11,12,14-25,27-33,35-39"

```



## D


```d
import std.stdio, std.conv, std.string, std.algorithm, std.range;

string rangeExtraction(in int[] items)
in {
    assert(items.isSorted);
} body {
    if (items.empty)
        return null;
    auto ranges = [[items[0].text]];

    foreach (immutable x, immutable y; items.zip(items[1 .. $]))
        if (x + 1 == y)
            ranges[$ - 1] ~= y.text;
        else
            ranges ~= [y.text];

    return ranges
           .map!(r => r.length > 2 ? r[0] ~ "-" ~ r.back : r.join(','))
           .join(',');
}

void main() {
    foreach (data; [[-8, -7, -6, -3, -2, -1, 0, 1, 3, 4, 5, 7, 8, 9,
                     10, 11, 14, 15, 17, 18, 19, 20],
                    [0, 0, 0, 1, 1],
                    [0, 1, 2, 4, 6, 7, 8, 11, 12, 14, 15, 16, 17, 18,
                     19, 20, 21, 22, 23, 24, 25, 27, 28, 29, 30, 31,
                     32, 33, 35, 36, 37, 38, 39]])
        data.rangeExtraction.writeln;
}
```

```txt
-8--6,-3-1,3-5,7-11,14,15,17-20
0,0,0,1,1
0-2,4,6-8,11,12,14-25,27-33,35-39
```



## DWScript


```delphi
procedure ExtractRanges(const values : array of Integer);
begin
   var i:=0;
   while i<values.Length do begin
      if i>0 then
         Print(',');
      Print(values[i]);
      var j:=i+1;
      while (j<values.Length) and (values[j]=values[j-1]+1) do
         Inc(j);
      Dec(j);
      if j>i then begin
         if j=i+1 then
            Print(',')
         else Print('-');
         Print(values[j]);
      end;
      i:=j+1;
   end;
end;

ExtractRanges([ 0,  1,  2,  4,  6,  7,  8, 11, 12, 14,
               15, 16, 17, 18, 19, 20, 21, 22, 23, 24,
               25, 27, 28, 29, 30, 31, 32, 33, 35, 36,
               37, 38, 39]);
```

```txt
0-2,4,6-8,11,12,14-25,27-33,35-39
```



## Dyalect


```dyalect
func rangeFormat(a) {
    if a.len() == 0 {
        return ""
    }
    var parts = []
    var n1 = 0
    while true {
        var n2 = n1 + 1
        while n2 < a.len() && a[n2] == a[n2-1]+1 {
            n2 += 1
        }
        var s = a[n1].toString()
        if n2 == n1+2 {
            s += "," + a[n2-1]
        } else if n2 > n1+2 {
            s += "-" + a[n2-1]
        }
        parts.add(s)
        if n2 == a.len() {
            break
        }
        if a[n2] == a[n2-1] {
            throw "Sequence repeats value \(a[n2])"
        }
        if a[n2] < a[n2-1] {
            throw "Sequence not ordered: \(a[n2]) < \(a[n2-1])"
        }
        n1 = n2
    }
    return String.join(values: parts)
}

var rf = rangeFormat([
    0, 1, 2, 4, 6, 7, 8, 11, 12, 14,
    15, 16, 17, 18, 19, 20, 21, 22, 23, 24,
    25, 27, 28, 29, 30, 31, 32, 33, 35, 36,
    37, 38, 39
])
print("range format: \(rf)")
```


```txt
range format: 0-2,4,6-8,11,12,14-25,27-33,35-39
```



## E

Cheeky solution: relying on the standard library for finding ranges,
and just formatting them ourselves.


```e
def rex(numbers :List[int]) {
    var region := 0..!0
    for n in numbers { region |= n..n }
    var ranges := []
    for interval in region.getSimpleRegions() {
        def a := interval.getOptStart()
        def b := interval.getOptBound() - 1
        ranges with= if (b > a + 1) {
                         `$a-$b`
                     } else if (b <=> a + 1) {
                         `$a,$b`
                     } else { # b <=> a
                         `$a`
                     }
    }
    return ",".rjoin(ranges)
}
```



```e
? rex([
>    0,  1,  2,  4,  6,  7,  8, 11, 12, 14,
>    15, 16, 17, 18, 19, 20, 21, 22, 23, 24,
>    25, 27, 28, 29, 30, 31, 32, 33, 35, 36,
>    37, 38, 39])
# value: "0-2,4,6-8,11,12,14-25,27-33,35-39"

```



## EchoLisp


```scheme

(define task '(0 1 2 4 6 7 8 11 12 14 15 16 17 18 19 20 21 22 23 24 25 27 28 29 30 31 32 33 35 36 37 38 39))

;; 1- GROUPING
(define (group-range item acc)
(if
    (or (empty? acc) (!= (caar acc) (1- item)))
      (cons (cons item item) acc)
      (begin  (set-car! (car acc) item) acc)))

;; intermediate result
;; (foldl group-range () task)
;; → ((39 . 35) (33 . 27) (25 . 14) (12 . 11) (8 . 6) (4 . 4) (2 . 0))

;; 2- FORMATTING
(define (range->string range)
(let ((from (rest range)) (to (first range)))
		(cond
		((= from to) (format "%d " from))
		((= to (1+ from))  (format "%d, %d " from to))
		(else (format "%d-%d " from to)))))

;; 3 - FINAL
(string-join (map range->string  (reverse (foldl group-range () task))) ",")
    → "0-2 ,4 ,6-8 ,11, 12 ,14-25 ,27-33 ,35-39 "

```



## Eiffel


```Eiffel

class
	RANGE

create
	make

feature
	make
		local
			extended_range: STRING
		do
			extended_range := "0,  1,  2,  4,  6,  7,  8, 11, 12, 14, " +
				"15, 16, 17, 18, 19, 20, 21, 22, 23, 24, " +
				"25, 27, 28, 29, 30, 31, 32, 33, 35, 36, " +
                		"37, 38, 39"
			print("Extended range: " + extended_range + "%N")
			print("Extracted range: " + extracted_range(extended_range) + "%N%N")
		end

feature
	extracted_range(sequence: STRING): STRING
		local
			elements: LIST[STRING]
			first, curr: STRING
			subrange_size, index: INTEGER
		do
			sequence.replace_substring_all (", ", ",")
			elements := sequence.split (',')
			from
				index := 2
				first := elements.at (1)
				subrange_size := 0
				Result := ""
			until
				index > elements.count
			loop
				curr := elements.at (index)
				if curr.to_integer - first.to_integer - subrange_size = 1
				then
					subrange_size := subrange_size + 1
				else
					Result.append(first)
					if (subrange_size <= 1)
					then
						Result.append (", ")
					else
						Result.append (" - ")
					end
					if (subrange_size >= 1)
					then
						Result.append ((first.to_integer + subrange_size).out)
						Result.append (", ")
					end

					first := curr
					subrange_size := 0
				end
				index := index + 1
			end
			Result.append(first)
			if (subrange_size <= 1)
			then
				Result.append (", ")
			else
				Result.append (" - ")
			end
			if (subrange_size >= 1)
			then
				Result.append ((first.to_integer + subrange_size).out)
			end
		end
end

```

```txt

Extended range: 0,  1,  2,  4,  6,  7,  8, 11, 12, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 27, 28, 29, 30, 31, 32, 33, 35, 36, 37, 38, 39
Extracted range: 0 - 2,  4,  6 - 8, 11, 12, 14 - 25, 27 - 33, 35 - 39

```



## Elixir

```elixir
defmodule RC do
  def range_extract(list) do
    max = Enum.max(list) + 2
    sorted = Enum.sort([max|list])
    candidate_number = hd(sorted)
    current_number = hd(sorted)
    extract(tl(sorted), candidate_number, current_number, [])
  end

  defp extract([], _, _, range), do: Enum.reverse(range) |> Enum.join(",")
  defp extract([next|rest], candidate, current, range) when current+1 >= next do
    extract(rest, candidate, next, range)
  end
  defp extract([next|rest], candidate, current, range) when candidate == current do
    extract(rest, next, next, [to_string(current)|range])
  end
  defp extract([next|rest], candidate, current, range) do
    separator = if candidate+1 == current, do: ",", else: "-"
    str = "#{candidate}#{separator}#{current}"
    extract(rest, next, next, [str|range])
  end
end

list = [
   0,  1,  2,  4,  6,  7,  8, 11, 12, 14,
  15, 16, 17, 18, 19, 20, 21, 22, 23, 24,
  25, 27, 28, 29, 30, 31, 32, 33, 35, 36,
  37, 38, 39
]
IO.inspect RC.range_extract(list)
```


```txt

"0-2,4,6-8,11,12,14-25,27-33,35-39"

```



## Emacs Lisp


### version 1

<lang Emacs Lisp}>
(require 'gnus-range)
(defun rangext (lst)
  (mapconcat (lambda (item)
               (if (consp item)
		   (if (= (+ 1 (car item) ) (cdr item) )
		       (format "%d,%d" (car item) (cdr item) )
		     (format "%d-%d" (car item) (cdr item) ))
                 (format "%d" item)))
             (gnus-compress-sequence lst)
             ","))

(insert (rangext '(0  1  2  4  6  7  8 11 12 14
           15 16 17 18 19 20 21 22 23 24
           25 27 28 29 30 31 32 33 35 36
           37 38 39) ))

```


<b>Output:</b>

```txt

0-2,4,6-8,11,12,14-25,27-33,35-39

```



### version 2

<lang Emacs Lisp}>
(setq max-lisp-eval-depth 10000)

(defun ab (a ls)
  (if ls (if (= (+ a 1) (car ls) )
	     (abc a (car ls) (cdr ls) )
	   (format "%d,%s" a (ab (car ls) (cdr ls) )))
    (format "%d" a) ))

(defun abc (a b ls)
  (if ls (if (= (+ b 1) (car ls) )
	     (abcd a (car ls) (cdr ls) )
	   (format "%d,%d,%s" a b (ab (car ls) (cdr ls) )))
    (format "%d,%d" a b) ))

(defun abcd (a c ls)
  (if ls (if (= (+ c 1) (car ls) )
	     (abcd a (car ls) (cdr ls) )
	   (format "%d-%d,%s" a c (ab (car ls) (cdr ls) )))
    (format "%d-%d" a c) ))

(defun rangext (ls)
  (if ls (ab (car ls) (cdr ls) ) ""))

(insert (rangext '(0  1  2  4  6  7  8 11 12 14
           15 16 17 18 19 20 21 22 23 24
           25 27 28 29 30 31 32 33 35 36
           37 38 39) ))

```


<b>Output:</b>

```txt

0-2,4,6-8,11,12,14-25,27-33,35-39

```



## Erlang


```Erlang

-module( range ).

-export( [extraction/1, task/0] ).

extraction( [H | T] ) when is_integer(H) ->
        Reversed_extracts = extraction_acc( lists:foldl(fun extraction/2, {H, []}, T) ),
        string:join( lists:reverse(Reversed_extracts), "," ).

task() ->
    io:fwrite( "~p~n", [extraction([0, 1, 2, 4, 6, 7, 8, 11, 12, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 27, 28, 29, 30, 31, 32, 33, 35, 36, 37, 38, 39])] ).



extraction( N, {Start, Acc} ) when N =:= Start + 1 -> {Start, N, Acc};
extraction( N, {Start, Acc} )  -> {N, extraction_acc( {Start, Acc} )};
extraction( N, {Start, Stop, Acc} ) when N =:= Stop + 1 -> {Start, N, Acc};
extraction( N, {Start, Stop, Acc} ) -> {N, extraction_acc( {Start, Stop, Acc} )}.

extraction_acc( {N, Acc} ) -> [erlang:integer_to_list(N) | Acc];
extraction_acc( {Start, Stop, Acc} ) when Stop > Start + 1 -> [erlang:integer_to_list(Start) ++ "-" ++ erlang:integer_to_list(Stop) | Acc];
extraction_acc( {Start, Stop, Acc} ) -> [erlang:integer_to_list(Stop), erlang:integer_to_list(Start) | Acc]. % Reversed

```

```txt

19> range:task().
"0-2,4,6-8,11,12,14-25,27-33,35-39"

```



## Euphoria


```euphoria
function extract_ranges(sequence s)
    integer first
    sequence out
    out = ""
    if length(s) = 0 then
        return out
    end if
    first = 1
    for i = 2 to length(s) do
        if s[i] != s[i-1]+1 then
            if first = i-1 then
                out &= sprintf("%d,", s[first])
            elsif first = i-2 then
                out &= sprintf("%d,%d,", {s[first],s[i-1]})
            else
                out &= sprintf("%d-%d,", {s[first],s[i-1]})
            end if
            first = i
        end if
    end for
    if first = length(s) then
        out &= sprintf("%d", s[first])
    elsif first = length(s)-1 then
        out &= sprintf("%d,%d", {s[first],s[$]})
    else
        out &= sprintf("%d-%d", {s[first],s[$]})
    end if
    return out
end function

puts(1, extract_ranges({0, 1, 2, 4, 6, 7, 8, 11, 12, 14, 15, 16, 17, 18, 19,
    20, 21, 22, 23, 24, 25, 27, 28, 29, 30, 31, 32, 33, 35, 36, 37, 38, 39}))
```


```txt
0-2,4,6-8,11,12,14-25,27-33,35-39
```


=={{header|F_Sharp|F#}}==

```fsharp
let extractRanges = function
  | []    -> Seq.empty
  | x::xr ->
      let rec loop ys first last = seq {
        match ys with
        | y::yr when y = last + 1 -> yield! loop yr first y  // add to current range
        | y::yr                   -> yield (first, last)     // finish current range
                                     yield! loop yr y y      //  and start next
        | []                      -> yield (first, last) }   // finish final range
      loop xr x x


let rangeToString (s,e) =
  match e-s with
  | 0 -> sprintf "%d" s
  | 1 -> sprintf "%d,%d" s e
  | _ -> sprintf "%d-%d" s e


let extract = extractRanges >> Seq.map rangeToString >> String.concat ","


printfn "%s" (extract [ 0; 1; 2; 4; 6; 7; 8; 11; 12; 14; 15; 16; 17; 18; 19; 20; 21;
                        22; 23; 24; 25; 27; 28; 29; 30; 31; 32; 33; 35; 36; 37; 38; 39 ])
```


```txt
0-2,4,6-8,11,12,14-25,27-33,35-39
```



## Factor

The <code>monotonic-split</code> word enables us to split the input sequence into sub-sequences of contiguous integers. From there, we make ranges out of sequences greater than 2 in length and list members of sequences less than or equal to 2 in length.

```factor
USING: formatting io kernel math math.parser sequences
splitting.monotonic ;
IN: rosetta-code.range-extraction

: make-range ( seq -- str )
    [ first ] [ last ] bi "%d-%d" sprintf ;

: make-atomic ( seq -- str ) [ number>string ] map "," join ;

: extract-range ( seq -- str )
    [ - -1 = ] monotonic-split
    [ dup length 2 > [ make-range ] [ make-atomic ] if ] map
    "," join ;

{
    0 1 2 4 6 7 8 11 12 14 15 16 17 18 19 20 21 22
    23 24 25 27 28 29 30 31 32 33 35 36 37 38 39
} extract-range print
```

```txt

0-2,4,6-8,11,12,14-25,27-33,35-39

```



## Forth


```forth
create values
here
    0 ,  1 ,  2 ,  4 ,  6 ,  7 ,  8 , 11 , 12 , 14 ,
   15 , 16 , 17 , 18 , 19 , 20 , 21 , 22 , 23 , 24 ,
   25 , 27 , 28 , 29 , 30 , 31 , 32 , 33 , 35 , 36 ,
   37 , 38 , 39 ,
here swap - 1 cells / constant /values

: clip 1- swap cell+ swap ;            \ reduce array
: .range2 0 .r ." -" 0 .r ;            \ difference two or more
: .range1 0 .r ." , " 0 .r ;           \ difference one
: .range0 drop 0 .r ;                  \ no difference
                                       \ select printing routine
create .range ' .range0 , ' .range1 , ' .range2 ,
  does> >r over over - 2 min cells r> + @ execute ;

: .ranges                              ( a n --)
  over @ dup >r >r                     \ setup first value
  begin
    clip dup                           \ check length array
  while
    over @ dup r@ 1+ =                 \ check if range breaks
    if r> drop >r else r> r> .range ." , " dup >r >r then
  repeat 2drop r> r> .range cr         \ print last range
;

values /values .ranges
```


```txt
0-2, 4, 6-8, 11, 12, 14-25, 27-33, 35-39
```



## Fortran

There was some initial confusion as to whether the list was to be supplied as an array of integer values, or as a text string from which integer values were to be extracted. The consensus is a text string. In principle the text string could be parsed to find the starting and stopping positions of each number so that any size integers could be processed merely by copying the texts around without reading the values into integer variables of limited capacity, but that would be complicated by the possible presence of signs. So, it was simpler to take advantage of the free-format data reading protocol that would handle signs without difficulty and on output any spurious +signs would be omitted. This however immediately raises the question: how many numbers are there to be read? A very useful input style is to start with the number of values to read followed by the values; then something like <code>READ(IN,*) N,A(1:N)</code> works nicely. But this is not the given style of input, so a fallback: count how many commas appear to deduce how many numbers there are to be read. The free-format style allows either commas or spaces between numbers (and if there is a comma, any spaces also present are passed by), so the layout is easy. Data errors could still be encountered, so a more complete version would have <code>READ (TEXT,*,ERR=''label'') VAL(1:N)</code> to catch these, but the specification does not call for checking.

The standard problem is "how long is a piece of string?" - arrays normally must be given a specific bound. With F90, it is possible to allocate an array of a size determined at run time via some tedious gibberish, but for this example, LOTS will suffice. More seriously, the specification calls for a function returning the text representation of the list, but unfortunately, a function must have a specified size as in <code>CHARACTER*66 FUNCTION IRANGE(TEXT)</code> where the 66 is fixed at compile time. With Fortran 2003, there are facilities for the run-time sizing of character variables, but not in F90/95 though they could be devised with a great deal of blather. In any case, the size required is not known until the end, so successively reallocating space of size 1, 2, 3, 4, ... and each time copying the existing text into the larger text area would soon be painful. A largeish value for the size of the result could be used but instead, a subroutine, which returns its result via modifying its parameter. It is up to the caller to provide a parameter of sufficient size.

Although Pascal offers a Str procedure for converting a variable to a text string, maddeningly, it is a procedure not a function and so cannot be used within a compound statement. Fortran could offer access to the FORMAT facility via something like a function FMT(x) which returns the text representation of variable x with no leading or trailing spaces (whereby FMT(-6) would return "-6" and so forth) but alas, does not. Such a function cannot be written in ordinary Fortran until such time as it is possible to return varying-sized character results. The I0 format code standardised in F90 comes close but of course it must be used in a complex environment. All in all, it is easier to devise a subroutine SPLOT(n) to write the value of an integer (with possible leading hyphen if negative) to a scratchpad and then EMIT its text character by character to the output variable character until stopped by a space. Subroutines EMIT and SPLOT could be normal separate subroutines, but as servants of IRANGE it is easier to take advantage of the F90 facility whereby they can be "contained" inside IRANGE and thereby gain access to its internal context. Otherwise, there would have to be additional parameters or usage of COMMON variables for such communication.

The method grinds through the list of values, looking ahead for consecutive continuations (relying on the value of a DO-loop's index variable being available on exit from the loop) and thereby placing in its output string either a range of numbers or a single number. This could be done by using WRITE with suitable FORMAT statements to appropriate portions of the output string via careful counting of positions, but using EMIT and SPLOT avoids the requisite cogitations. A fancier method would be to devise a list of numbers to be output along with a suitable FORMAT statement that would supply the commas and hyphens as appropriate. Of course, one would again face the question "how long is a FORMAT string?", so, grinding stepwise it is.
```Fortran
      SUBROUTINE IRANGE(TEXT)	!Identifies integer ranges in a list of integers.
Could make this a function, but then a maximum text length returned would have to be specified.
       CHARACTER*(*) TEXT	!The list on input, the list with ranges on output.
       INTEGER LOTS		!Once again, how long is a piece of string?
       PARAMETER (LOTS = 666)	!This should do, at least for demonstrations.
       INTEGER VAL(LOTS)	!The integers of the list.
       INTEGER N		!Count of numbers.
       INTEGER I,I1		!Steppers.
        N = 1		!Presume there to be one number.
        DO I = 1,LEN(TEXT)	!Then by noticing commas,
          IF (TEXT(I:I).EQ.",") N = N + 1	!Determine how many more there are.
        END DO			!Step alonmg the text.
        IF (N.LE.2) RETURN	!One comma = two values. Boring.
        IF (N.GT.LOTS) STOP "Too many values!"
        READ (TEXT,*) VAL(1:N)	!Get the numbers, with free-format flexibility.
        TEXT = ""		!Scrub the parameter!
        L = 0			!No text has been placed.
        I1 = 1			!Start the scan.
   10   IF (L.GT.0) CALL EMIT(",")	!A comma if there is prior text.
        CALL SPLOT(VAL(I1))		!The first number always appears.
        DO I = I1 + 1,N			!Now probe ahead
          IF (VAL(I - 1) + 1 .NE. VAL(I)) EXIT	!While values are consecutive.
        END DO				!Up to the end of the remaining list.
        IF (I - I1 .GT. 2) THEN		!More than two consecutive values seen?
          CALL EMIT("-")		!Yes!
          CALL SPLOT(VAL(I - 1))	!The ending number of a range.
          I1 = I			!Finger the first beyond the run.
         ELSE			!But if too few to be worth a span,
          I1 = I1 + 1			!Just finger the next number.
        END IF			!So much for that starter.
        IF (I.LE.N) GO TO 10	!Any more?
       CONTAINS		!Some assistants to save on repetition.
        SUBROUTINE EMIT(C)	!Rolls forth one character.
         CHARACTER*1 C		!The character.
          L = L + 1		!Advance the finger.
          IF (L.GT.LEN(TEXT)) STOP "Ran out of text!"	!Maybe not.
          TEXT(L:L) = C		!And place the character.
        END SUBROUTINE EMIT	!That was simple.
        SUBROUTINE SPLOT(N)	!Rolls forth a signed number.
         INTEGER N		!The number.
         CHARACTER*12 FIELD	!Sufficient for 32-bit integers.
         INTEGER I		!A stepper.
          WRITE (FIELD,"(I0)") N!Roll the number, with trailing spaces.
          DO I = 1,12		!Now transfer the text of the number.
            IF (FIELD(I:I).LE." ") EXIT	!Up to the first space.
            CALL EMIT(FIELD(I:I))	!One by one.
          END DO		!On to the end.
        END SUBROUTINE SPLOT	!Not so difficult either.
      END	!So much for IRANGE.

      PROGRAM POKE
      CHARACTER*(200) SOME
      SOME = "  0,  1,  2,  4,  6,  7,  8, 11, 12, 14,  "
     1      //"  15, 16, 17, 18, 19, 20, 21, 22, 23, 24,"
     2      //"25, 27, 28, 29, 30, 31, 32, 33, 35, 36,  "
     3      //"37, 38, 39                               "
      CALL IRANGE(SOME)
      WRITE (6,*) SOME
      END
```


Output: spaces after the commas could be added easily enough.
```txt
 0-2,4,6-8,11,12,14-25,27-33,35-39
```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Function formatRange (a() As Integer) As String
  Dim lb As Integer = LBound(a)
  Dim ub As Integer = UBound(a)
  If ub = - 1 Then Return ""
  If lb = ub Then Return Str(a(lb))
  Dim rangeCount As Integer = 1
  Dim range As String = Str(a(lb))
  For i As Integer = lb + 1 To ub
    If a(i) = a(i - 1) + 1 Then
      rangeCount += 1
    ElseIf rangeCount = 1 Then
      range += "," + Str(a(i))
    ElseIf rangeCount = 2 Then
      rangeCount = 1
      range += "," + Str(a(i-1)) + "," + Str(a(i))
    Else
      rangeCount = 1
      range += "-" + Str(a(i-1)) + "," + Str(a(i))
    End If
  Next
  If rangeCount = 2 Then
    range += "," + Str(a(ub))
  ElseIf rangeCount > 2 Then
    range += "-" + Str(a(ub))
  End If
  Return range
End Function

Dim a(1 To 20) As Integer = {-6, -3, -2, -1, 0, 1, 3, 4, 5, 7, 8, 9, 10, 11, 14, 15, 17, 18, 19, 20}
Print formatRange(a())
Print

Dim b(1 To 33) As Integer => _
{ _
   0,  1,  2,  4,  6,  7,  8, 11, 12, 14, _
  15, 16, 17, 18, 19, 20, 21, 22, 23, 24, _
  25, 27, 28, 29, 30, 31, 32, 33, 35, 36, _
  37, 38, 39 _
}

Print formatRange(b())
Print
Print "Press any key to continue"
Sleep
```


```txt

-6,-3-1,3-5,7-11,14,15,17-20

0-2,4,6-8,11,12,14-25,27-33,35-39

```



## Gambas

'''[https://gambas-playground.proko.eu/?gist=49f362e3de9725fbf3c56f2381abf8a4 Click this link to run this code]'''

```gambas
siInput As New Short[]
siInput1 As Short[] = [0, 1, 2, 4, 6, 7, 8, 11, 12, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 27, 28, 29, 30, 31, 32, 33, 35, 36, 37, 38, 39]
siInput2 As Short[] = [-6, -3, -2, -1, 0, 1, 3, 4, 5, 7, 8, 9, 10, 11, 14, 15, 17, 18, 19, 20]
sOutput As New String[]
siCount As Short
siNum As Short
'__________________
Public Sub Main()
Dim siLoop As Short

For siLoop = 0 To 1
  If siLoop = 0 Then siInput = siInput1.Copy() Else siInput = siInput2.Copy()
  siCount = 0
  siNum = 0
    Repeat
      If siInput[siCount + 1] = siInput[siCount] + 1 Then
        Inc siCount
      Else
        GetOutput
      Endif
    Until siCount = siInput.Max

  GetOutput
  Print sOutput.join(", ")
  sOutput.clear
Next

End
'__________________
Public Sub GetOutput()

If siNum = siCount Then
  sOutput.add(siInput[siNum])
  Inc siCount
  siNum = siCount
End If

If siNum <> siCount Then
  If siNum = siCount - 1 Then
    sOutput.add(siInput[siNum])
    sOutput.add(siInput[siNum + 1])
    siCount += 2
    siNum += 2
    Return
  End If
  sOutput.Add(siInput[siNum] & "-" & siInput[siCount])
  Inc siCount
  siNum = siCount
End If

End
```

Output:

```txt

0-2, 4, 6-8, 11, 12, 14-25, 27-33, 35-39
-6, -3-1, 3-5, 7-11, 14, 15, 17-20

```



## Go


```go
package main

import (
    "errors"
    "fmt"
    "strconv"
    "strings"
)

func main() {
    rf, err := rangeFormat([]int{
        0, 1, 2, 4, 6, 7, 8, 11, 12, 14,
        15, 16, 17, 18, 19, 20, 21, 22, 23, 24,
        25, 27, 28, 29, 30, 31, 32, 33, 35, 36,
        37, 38, 39,
    })
    if err != nil {
        fmt.Println(err)
        return
    }
    fmt.Println("range format:", rf)
}

func rangeFormat(a []int) (string, error) {
    if len(a) == 0 {
        return "", nil
    }
    var parts []string
    for n1 := 0; ; {
        n2 := n1 + 1
        for n2 < len(a) && a[n2] == a[n2-1]+1 {
            n2++
        }
        s := strconv.Itoa(a[n1])
        if n2 == n1+2 {
            s += "," + strconv.Itoa(a[n2-1])
        } else if n2 > n1+2 {
            s += "-" + strconv.Itoa(a[n2-1])
        }
        parts = append(parts, s)
        if n2 == len(a) {
            break
        }
        if a[n2] == a[n2-1] {
            return "", errors.New(fmt.Sprintf(
                "sequence repeats value %d", a[n2]))
        }
        if a[n2] < a[n2-1] {
            return "", errors.New(fmt.Sprintf(
                "sequence not ordered: %d < %d", a[n2], a[n2-1]))
        }
        n1 = n2
    }
    return strings.Join(parts, ","), nil
}
```

```txt

range format: 0-2,4,6-8,11,12,14-25,27-33,35-39

```



## Groovy

Ad Hoc Solution:

```groovy
def range = { s, e -> s == e ? "${s}," : s == e - 1 ? "${s},${e}," : "${s}-${e}," }

def compressList = { list ->
    def sb, start, end
    (sb, start, end) = [''<<'', list[0], list[0]]
    for (i in list[1..-1]) {
        (sb, start, end) = i == end + 1 ? [sb, start, i] : [sb << range(start, end), i, i]
    }
    (sb << range(start, end))[0..-2].toString()
}

def compressRanges = { expanded -> compressList(Eval.me('[' + expanded + ']')) }
```


Test:

```groovy
def s = '''
    0,  1,  2,  4,  6,  7,  8, 11, 12, 14,
   15, 16, 17, 18, 19, 20, 21, 22, 23, 24,
   25, 27, 28, 29, 30, 31, 32, 33, 35, 36,
   37, 38, 39
'''
println (compressRanges(s))
```


```txt
0-2,4,6-8,11,12,14-25,27-33,35-39
```



## Haskell


### direct recursion


```haskell
import Data.List (intercalate)

extractRange :: [Int] -> String
extractRange = intercalate "," . f
  where f :: [Int] -> [String]
        f (x1 : x2 : x3 : xs) | x1 + 1 == x2 && x2 + 1 == x3
             = (show x1 ++ '-' : show xn) : f xs'
          where (xn, xs') = g (x3 + 1) xs
                g a (n : ns) | a == n    = g (a + 1) ns
                             | otherwise = (a - 1, n : ns)
                g a []                   = (a - 1, [])
        f (x : xs)            = show x : f xs
        f []                  = []
```



```text>
 extractRange $ [0..2] ++ 4 : [6..8] ++ 11 : 12 : [14..25] ++ [27..33] ++ [35..39]
"0-2,4,6-8,11,12,14-25,27-33,35-39"
```



### splitBy

We can, alternatively, define a reusable splitBy function, which returns a list of lists (split wherever the relationship between two consecutive items matches a supplied predicate function).
Delegating to splitBy allows a reasonably clean definition of range formatting:


```haskell
import Data.List (intercalate)
import Data.Function (on)
import Data.Bool (bool)

-- RANGE FORMAT -------------------------------------------
rangeFormat :: [Int] -> String
rangeFormat = intercalate "," . fmap rangeString . splitBy ((/=) . succ)

rangeString xs
  | 2 < length xs = x ++ '-' : last t
  | otherwise = intercalate "," ps
  where
    ps@(x:t) = show <$> xs


-- GENERIC FUNCTION ---------------------------------------

-- Split wherever a supplied predicate matches the
-- relationship between two consecutive items.
splitBy :: (a -> a -> Bool) -> [a] -> [[a]]
splitBy _ [] = []
splitBy _ [x] = [[x]]
splitBy f xs@(_:t) = active : acc
  where
    (active, acc) =
      foldr
        (\(x, prev) (active, acc) ->
            let current = bool active [prev] (null active)
            in bool (x : current, acc) ([x], current : acc) (f x prev))
        ([], [])
        (zip xs t)

-- TEST ---------------------------------------------------
main :: IO ()
main =
  print $
  rangeFormat
    [ 0
    , 1
    , 2
    , 4
    , 6
    , 7
    , 8
    , 11
    , 12
    , 14
    , 15
    , 16
    , 17
    , 18
    , 19
    , 20
    , 21
    , 22
    , 23
    , 24
    , 25
    , 27
    , 28
    , 29
    , 30
    , 31
    , 32
    , 33
    , 35
    , 36
    , 37
    , 38
    , 39
    ]
```

```txt
"0-2,4,6-8,11,12,14-25,27-33,35-39"
```



### chop

Or, we can pass a span-chopping function to Data.List.Split '''chop'''.


```haskell
import Data.List (intercalate, groupBy, isPrefixOf)
import Data.List.Split (chop)
import Data.Bool (bool)

rangeFormat :: [Int] -> String
rangeFormat xs =
  intercalate "," $
  (head . ((bool <*> tail) <*> (> 1) . length)) <$>
  groupBy isPrefixOf (rangeString <$> chop succSpan (zip xs (tail xs)))

rangeString [] = ""
rangeString xxs@(x:xs)
  | null xs = show (snd x)
  | otherwise = intercalate "-" (show <$> [fst x, snd (last xs)])

succSpan [] = ([], [])
succSpan (xxs@(x:xs))
  | null ys = ([x], xs)
  | otherwise = (ys, zs)
  where
    (ys, zs) = span (uncurry ((==) . succ)) xxs

main :: IO ()
main =
  putStrLn $
  rangeFormat [ 0, 1, 2, 4, 6, 7, 8, 11, 12, 14,
      15, 16, 17, 18, 19, 20, 21, 22, 23, 24,
      25, 27, 28, 29, 30, 31, 32, 33, 35, 36,
      37, 38, 39 ]
```

```txt
0-2,4,6-8,11,12,14-25,27-33,35-39
```


=={{header|Icon}} and {{header|Unicon}}==

```Icon
procedure main()

   R := [  0,  1,  2,  4,  6,  7,  8, 11, 12, 14,
          15, 16, 17, 18, 19, 20, 21, 22, 23, 24,
          25, 27, 28, 29, 30, 31, 32, 33, 35, 36,
          37, 38, 39 ]

   write("Input list      := ",list2string(R))
   write("Extracted sting := ",s := range_extract(R)  | "FAILED")
end

procedure range_extract(R)         #: return string/range representation of a list of unique integers
local s,sep,low,high,x

   every if integer(x:= !R) ~= x then fail                  # ensure all are integers,
   R := sort(set(R))                                        # unique, and sorted

   s := sep := ""
   while s ||:= sep || ( low := high := get(R) ) do {       # lower bound of range
      sep := ","
      while high := ( R[1] = high + 1 ) do get(R)           # find the end of range
      if high > low+1 then s ||:= "-" || high               # - record range of 3+
      else if high = low+1 then push(R,high)                # - range of 2, high becomes new low
      }
   return s
end

procedure list2string(L)          #: helper to convert list to string
local s

   every (s := "[ ") ||:= !L || " "
   return s || "]"
end
```

```txt
Input list      := [ 0 1 2 4 6 7 8 11 12 14 15 16 17 18 19 20 21 22 23 24 25 27
28 29 30 31 32 33 35 36 37 38 39 ]
Extracted sting := 0-2,4,6-8,11,12,14-25,27-33,35-39
```



## J


Older versions of J will also need <code> require 'strings'</code>.


```j
fmt=: [: ;@(8!:0) [`]`({. ; (',-' {~ 2 < #) ; {:)@.(2 <. #)
group=: <@fmt;.1~ 1 ~: 0 , 2 -~/\ ]
extractRange=: ',' joinstring group
```


Example use:


```j
   extractRange 0 1 2 4 6 7 8 11 12 14 15 16 17 18 19 20 21 22 23 24 25 27 28 29 30 31 32 33 35 36 37 38 39
0-2,4,6-8,11,12,14-25,27-33,35-39
```


and


```j
   extractRange (-6, 3, 2, 1), 0, 1, 3, 4, 5, 7, 8, 9, 10, 11, 14, 15, 17, 18, 19, 20
-6,-3-1,3-5,7-11,14,15,17-20
```


Other examples:


```J
   extractRange i.101
0-100
```


The first 101 non-negative integers


```J

   extractRange (-. p:) i.101
0,1,4,6,8-10,12,14-16,18,20-22,24-28,30,32-36,38-40,42,44-46,48-52,54-58,60,62-66,68-70,72,74-78,80-82,84-88,90-96,98-100
```


Excluding those which are prime


```J

   extractRange 2}. (-. p:) i.101
4,6,8-10,12,14-16,18,20-22,24-28,30,32-36,38-40,42,44-46,48-52,54-58,60,62-66,68-70,72,74-78,80-82,84-88,90-96,98-100
```


Also excluding the first two non-negative integers (which are neither prime nor the product of non-empty lists of primes).


## Java


```java
public class RangeExtraction {

    public static void main(String[] args) {
        int[] arr = {0, 1, 2, 4, 6, 7, 8, 11, 12, 14,
            15, 16, 17, 18, 19, 20, 21, 22, 23, 24,
            25, 27, 28, 29, 30, 31, 32, 33, 35, 36,
            37, 38, 39};

        int len = arr.length;
        int idx = 0, idx2 = 0;
        while (idx < len) {
            while (++idx2 < len && arr[idx2] - arr[idx2 - 1] == 1);
            if (idx2 - idx > 2) {
                System.out.printf("%s-%s,", arr[idx], arr[idx2 - 1]);
                idx = idx2;
            } else {
                for (; idx < idx2; idx++)
                    System.out.printf("%s,", arr[idx]);
            }
        }
    }
}
```

```txt
0-2,4,6-8,11,12,14-25,27-33,35-39,
```



## JavaScript


### ES5


### =Imperative=


```javascript
function rangeExtraction(list) {
  var len = list.length;
  var out = [];
  var i, j;

  for (i = 0; i < len; i = j + 1) {
    // beginning of range or single
    out.push(list[i]);

    // find end of range
    for (var j = i + 1; j < len && list[j] == list[j-1] + 1; j++);
    j--;

    if (i == j) {
      // single number
      out.push(",");
    } else if (i + 1 == j) {
      // two numbers
      out.push(",", list[j], ",");
    } else {
      // range
      out.push("-", list[j], ",");
    }
  }
  out.pop(); // remove trailing comma
  return out.join("");
}

// using print function as supplied by Rhino standalone
print(rangeExtraction([
  0,  1,  2,  4,  6,  7,  8, 11, 12, 14,
  15, 16, 17, 18, 19, 20, 21, 22, 23, 24,
  25, 27, 28, 29, 30, 31, 32, 33, 35, 36,
  37, 38, 39
]));
```



### =Functional=

```JavaScript
(function () {
    'use strict';

    // rangeFormat :: [Int] -> String
    var rangeFormat = function (xs) {
        return splitBy(function (a, b) {
                return b - a > 1;
            }, xs)
            .map(rangeString)
            .join(',');
    };

    // rangeString :: [Int] -> String
    var rangeString = function (xs) {
        return xs.length > 2 ? [head(xs), last(xs)].map(show)
            .join('-') : xs.join(',');
    };

    // GENERIC FUNCTIONS

    // Splitting not on a delimiter, but whenever the relationship between
    // two consecutive items matches a supplied predicate function

    // splitBy :: (a -> a -> Bool) -> [a] -> [[a]]
    var splitBy = function (f, xs) {
        if (xs.length < 2) return [xs];
        var h = head(xs),
            lstParts = xs.slice(1)
            .reduce(function (a, x) {
                var acc = a[0],
                    active = a[1],
                    prev = a[2];

                return f(prev, x) ? (
                    [acc.concat([active]), [x], x]
                ) : [acc, active.concat(x), x];
            }, [
                [],
                [h], h
            ]);
        return lstParts[0].concat([lstParts[1]]);
    };

    // head :: [a] -> a
    var head = function (xs) {
        return xs.length ? xs[0] : undefined;
    };

    // last :: [a] -> a
    var last = function (xs) {
        return xs.length ? xs.slice(-1)[0] : undefined;
    };

    // show :: a -> String
    var show = function (x) {
        return JSON.stringify(x);
    };

    // TEST
    return rangeFormat([0, 1, 2, 4, 6, 7, 8, 11, 12, 14, 15, 16,
        17, 18, 19, 20, 21, 22, 23, 24, 25, 27, 28, 29, 30, 31, 32,
        33, 35, 36, 37, 38, 39
    ]);
})();
```


```txt
"0-2,4,6-8,11,12,14-25,27-33,35-39"
```



### ES6

Defining the range format in terms of a reusable '''splitBy''' function:

```JavaScript
(() => {
    'use strict';

    // rangeFormat :: [Int] -> String
    const rangeFormat = xs =>
        splitBy((a, b) => b - a > 1, xs)
        .map(rangeString)
        .join(',');

    // rangeString :: [Int] -> String
    const rangeString = xs =>
        xs.length > 2 ? (
            [head(xs), last(xs)].map(show)
            .join('-')
        ) : xs.join(',')


    // GENERIC FUNCTIONS

    // Splitting not on a delimiter, but whenever the relationship between
    // two consecutive items matches a supplied predicate function

    // splitBy :: (a -> a -> Bool) -> [a] -> [[a]]
    const splitBy = (f, xs) => {
        if (xs.length < 2) return [xs];
        const
            h = head(xs),
            lstParts = xs.slice(1)
            .reduce(([acc, active, prev], x) =>
                f(prev, x) ? (
                    [acc.concat([active]), [x], x]
                ) : [acc, active.concat(x), x], [
                    [],
                    [h],
                    h
                ]);
        return lstParts[0].concat([lstParts[1]]);
    };

    // head :: [a] -> a
    const head = xs => xs.length ? xs[0] : undefined;

    // last :: [a] -> a
    const last = xs => xs.length ? xs.slice(-1)[0] : undefined;

    // show :: a -> String
    const show = x => JSON.stringify(x);

    // TEST
    return rangeFormat([0, 1, 2, 4, 6, 7, 8, 11, 12, 14,
        15, 16, 17, 18, 19, 20, 21, 22, 23, 24,
        25, 27, 28, 29, 30, 31, 32, 33, 35, 36,
        37, 38, 39
    ]);
})();
```

```txt
0-2,4,6-8,11,12,14-25,27-33,35-39
```



## jq


```kq
# Input should be an array
def extract:
  reduce .[] as $i
    # state is an array with integers or [start, end] ranges
    ([];
     if length == 0 then [ $i ]
     else ( .[-1]) as $last
            | if ($last|type) == "array" then
                if ($last[1] + 1) == $i then setpath([-1,1]; $i)
                else . + [ $i ]
                end
              elif ($last + 1) == $i then setpath([-1]; [$last, $i])
              else . + [ $i ]
              end
     end)
     | map( if type == "number" then tostring
       elif .[0] == .[1] -1
         then  "\(.[0]),\(.[1])"  # satisfy special requirement
       else "\(.[0])-\(.[1])" end )
     | join(",") ;
```


```txt

$ jq -n -f extract_range.jq input.txt
"0-2,4,6-8,11,12,14-25,27-33,35-39"
```



## Jsish

From Javascript ES5 Imperative solution.

```javascript
/* Range Extraction, in Jsish */
function rangeExtraction(list) {
  var len = list.length;
  var out = [];
  var i, j;

  for (i = 0; i < len; i = j + 1) {
    // beginning of range or single
    out.push(list[i]);

    // find end of range
    for (j = i + 1; j < len && list[j] == list[j-1] + 1; j++);
    j--;

    if (i == j) {
      // single number
      out.push(",");
    } else if (i + 1 == j) {
      // two numbers
      out.push(",", list[j], ",");
    } else {
      // range
      out.push("-", list[j], ",");
    }
  }
  out.pop(); // remove trailing comma
  return out.join("");
}

var arr = [ 0,  1,  2,  4,  6,  7,  8, 11, 12, 14,
           15, 16, 17, 18, 19, 20, 21, 22, 23, 24,
           25, 27, 28, 29, 30, 31, 32, 33, 35, 36,
           37, 38, 39 ];

puts(arr);
puts(rangeExtraction(arr));
```


```txt
prompt$ jsish rangeExtraction.jsi
[ 0, 1, 2, 4, 6, 7, 8, 11, 12, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 27, 28, 29, 30, 31, 32, 33, 35, 36, 37, 38, 39 ]
0-2,4,6-8,11,12,14-25,27-33,35-39
```



## Julia

This is perhaps an idiosyncratic solution.  Numbers inside of runs are replaced with Xs, the list is converted into a comma separated string, and then Xs and extra commas are replaced with the range character via a regular expression.

```Julia

function sprintfrange{T<:Integer}(a::Array{T,1})
    len = length(a)
    0 < len || return ""
    dropme = falses(len)
    dropme[2:end-1] = Bool[a[i-1]==a[i]-1 && a[i+1]==a[i]+1 for i in 2:(len-1)]
    s = [string(i) for i in a]
    s[dropme] = "X"
    s = join(s, ",")
    replace(s, r",[,X]+,", "-")
end

testa = [ 0,  1,  2,  4,  6,  7,  8, 11, 12, 14,
         15, 16, 17, 18, 19, 20, 21, 22, 23, 24,
         25, 27, 28, 29, 30, 31, 32, 33, 35, 36,
         37, 38, 39]

println("Testing range-style formatting.")
println("   ", testa, "\n       =>\n   ", sprintfrange(testa))

```


```txt

   [0,1,2,4,6,7,8,11,12,14,15,16,17,18,19,20,21,22,23,24,25,27,28,29,30,31,32,33,35,36,37,38,39]
       =>
   0-2,4,6-8,11,12,14-25,27-33,35-39

```



## K


```k
grp : {(&~1=0,-':x)_ x}
fmt : {:[1=#s:$x;s;(*s),:[3>#s;",";"-"],*|s]}
erng: {{x,",",y}/,//'fmt'grp x}
```


```txt
  erng 0 1 2 4 6 7 8 11 12 14 15 16 17 18 19 20 21 22 23 24 25 27 28 29 30 31 32 33 35 36 37 38 39
"0-2,4,6-8,11,12,14-25,27-33,35-39"
```



## Kotlin


```scala
// version 1.0.6

fun extractRange(list: List<Int>): String {
    if (list.isEmpty()) return ""
    val sb = StringBuilder()
    var first = list[0]
    var prev  = first

    fun append(index: Int) {
        if (first == prev) sb.append(prev)
        else if (first == prev - 1) sb.append(first, ",", prev)
        else sb.append(first, "-", prev)
        if (index < list.size - 1) sb.append(",")
    }

    for (i in 1 until list.size) {
        if (list[i] == prev + 1) prev++
        else {
            append(i)
            first = list[i]
            prev  = first
        }
    }
    append(list.size - 1)
    return sb.toString()
}

fun main(args: Array<String>) {
    val list1 = listOf(-6, -3, -2, -1, 0, 1, 3, 4, 5, 7, 8, 9, 10, 11, 14, 15, 17, 18, 19, 20)
    println(extractRange(list1))
    println()
    val list2 = listOf(0,  1,  2,  4,  6,  7,  8, 11, 12, 14,
                      15, 16, 17, 18, 19, 20, 21, 22, 23, 24,
                      25, 27, 28, 29, 30, 31, 32, 33, 35, 36,
                      37, 38, 39)
    println(extractRange(list2))
}
```


```txt

-6,-3-1,3-5,7-11,14,15,17-20

0-2,4,6-8,11,12,14-25,27-33,35-39

```



## Liberty BASIC


```lb

s$ = "0,  1,  2,  4,  6,  7,  8, 11, 12, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24," + _
     "25, 27, 28, 29, 30, 31, 32, 33, 35, 36, 37, 38, 39"
print ExtractRange$( s$)
end

function ExtractRange$( range$)
    n = 1
    count = ItemCount( range$, ",")
    while n <= count
        startValue = val( word$( range$, n, ","))
        m = n + 1
        while m <= count
            nextValue = val( word$( range$, m, ","))
            if nextValue - startValue <> m - n then exit while
            m = m + 1
        wend
        if m - n > 2 then
            ExtractRange$ = ExtractRange$ + str$( startValue) + "-" + str$( startValue + m - n - 1) + ","
        else
            for i = n to m - 1
                ExtractRange$ = ExtractRange$ + str$( startValue + i - n) + ","
            next i
        end if
        n = m
    wend
    ExtractRange$ = left$( ExtractRange$, len( ExtractRange$) - 1)
end function

function ItemCount( list$, separator$)
    while word$( list$, ItemCount + 1, separator$) <> ""
        ItemCount = ItemCount + 1
    wend
end function

```

```txt
    0-2,4,6-8,11,12,14-25,27-33,35-39

```



## LiveCode

Inefficient as it takes 2 passes

```LiveCode
function rangeExtract nums
    local prevNum, znums, rangedNums
    set itemDelimiter to ", "
    put the first item of nums into prevNum
    repeat for each item n in nums
        if n is (prevNum + 1) then
            put n into prevNum
            put "#" & n after znums
        else
            put n into prevNum
            put return & n after znums
        end if
    end repeat
    set itemDelimiter to "#"
    repeat for each line z in znums
        if z is empty then next repeat
        switch the number of items of z
            case 1
                put z & "," after rangedNums
                break
            case 2
                put item 1 of z & "," & item -1 of z & "," after rangedNums
                break
            default
                put item 1 of z & "-" & item -1 of z & "," after rangedNums
        end switch
    end repeat
    return char 1 to -2 of rangedNums  --strip off trailing comma
end rangeExtract

```

Test

```LiveCode
command testRangeExtract
    local numbers
    put "0, 1, 2, 4, 6, 7, 8, 11, 12, 14," \
          && "15, 16, 17, 18, 19, 20, 21, 22, 23, 24," \
          && "25, 27, 28, 29, 30, 31, 32, 33, 35, 36," \
          && "37, 38, 39" into numbers
    put rangeExtract(numbers)
end testRangeExtract
```

Output:
```LiveCode
0-2,4,6-8,11,12,14-25,27-33,35-39
```



## Lua


```Lua
function extractRange (rList)
    local rExpr, startVal = ""
    for k, v in pairs(rList) do
        if rList[k + 1] == v + 1 then
            if not startVal then startVal = v end
        else
            if startVal then
                if v == startVal + 1 then
                    rExpr = rExpr .. startVal .. "," .. v .. ","
                else
                    rExpr = rExpr .. startVal .. "-" .. v .. ","
                end
                startVal = nil
            else
                rExpr = rExpr .. v .. ","
            end
        end
    end
    return rExpr:sub(1, -2)
end

local intList = {
    0,  1,  2,  4,  6,  7,  8, 11, 12, 14,
   15, 16, 17, 18, 19, 20, 21, 22, 23, 24,
   25, 27, 28, 29, 30, 31, 32, 33, 35, 36,
   37, 38, 39
}
print(extractRange(intList))
```

```txt
0-2,4,6-8,11,12,14-25,27-33,35-39
```



## Maple


```Maple
lst := [0, 1, 2, 4, 6, 7, 8, 11, 12, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24,
25, 27, 28, 29, 30, 31, 32, 33, 35, 36, 37, 38, 39]:
r1,r2:= lst[1],lst[1]:
for i from 2 to numelems(lst) do
	if lst[i] - lst[i-1] = 1 then  #consecutive
		r2 := lst[i]:
	else #break
		printf(piecewise(r2-r1=1, "%d,%d,", r2-r1>1,"%d-%d,", "%d,"), r1, r2):
		r1,r2:= lst[i],lst[i]:
	fi:
od:
printf(piecewise(r2-r1=1, "%d,%d", r2-r1>1,"%d-%d", "%d"), r1, r2):
```

```txt
0-2,4,6-8,11,12,14-25,27-33,35-39
```



## Mathematica


```Mathematica

rangeExtract[data_List] := ToString[Row[
                               Riffle[
                                  Flatten[Split[Sort[data], #2 - #1 == 1 &] /. {a_Integer, __, b_} :> Row[{a, "-", b}]],
                                     ","]
                                     ]];

```


Example:

```txt

rangeExtract[{0,1,2,4,6,7,8,11,12,14,15,16,17,18,19,20,21,22,23,24,25,27,28,29,30,31,32,33,35,36,37,38,39}]

"0-2,4,6-8,11,12,14-25,27-33,35-39"

```



## Mercury


```Mercury
:- module range_extraction.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int, list, ranges, string.

main(!IO) :-
    print_ranges(numbers, !IO).

:- pred print_ranges(list(int)::in, io::di, io::uo) is det.

print_ranges(Nums, !IO) :-
    Ranges = ranges.from_list(Nums),
    ranges.range_foldr(add_range_string, Ranges, [], RangeStrs),
    io.write_list(RangeStrs, ",", io.write_string, !IO).

:- pred add_range_string(int::in, int::in,
     list(string)::in, list(string)::out) is det.

add_range_string(L, H, !Strs) :-
   ( if L = H then
      !:Strs = [int_to_string(L) | !.Strs]
   else if L + 1 = H then
      !:Strs = [int_to_string(L), int_to_string(H) | !.Strs]
   else
      !:Strs = [string.format("%d-%d", [i(L), i(H)]) | !.Strs]
   ).

:- func numbers = list(int).

numbers = [
   0,  1,  2,  4,  6,  7,  8, 11, 12, 14,
   15, 16, 17, 18, 19, 20, 21, 22, 23, 24,
   25, 27, 28, 29, 30, 31, 32, 33, 35, 36,
   37, 38, 39].

```


=={{header|MATLAB}} / {{header|Octave}}==

```MATLAB
function S=range_extraction(L)
    % Range extraction
    L(end+1) = NaN;
    S = int2str(L(1));
    k = 1;
    while (k < length(L)-1)
        if (L(k)+1==L(k+1) && L(k)+2==L(k+2) )
            m = 2;
            while (L(k)+m==L(k+m))
                m = m+1;
            end
            k = k+m-1;
            S = [S,'-',int2str(L(k))];
        else
            k = k+1;
            S = [S,',',int2str(L(k))];
        end
    end
end

disp(range_extraction([0,  1,  2,  4,  6,  7,  8, 11, 12, 14, 15, ...
                       16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 27, ...
                       28, 29, 30, 31, 32, 33, 35, 36, 37, 38, 39]))
```


```txt
0-2,4,6-8,11,12,14-25,27-33,35-39
```



## MUMPS


```MUMPS
RANGCONT(X) ;Integer range contraction
 NEW Y,I,CONT,NOTFIRST,CURR,PREV,NEXT,SEQ SET Y="",SEQ=0,PREV="",CONT=0
 FOR I=1:1:$LENGTH(X,",") DO
 .SET NOTFIRST=$LENGTH(Y),CURR=$PIECE(X,",",I),NEXT=$PIECE(X,",",I+1)
 .FOR  Q:$EXTRACT(CURR)'=" "  S CURR=$EXTRACT(CURR,2,$LENGTH(CURR))  ;clean up leading spaces
 .S SEQ=((CURR-1)=PREV)&((CURR+1)=NEXT)
 .IF 'NOTFIRST SET Y=CURR
 .IF NOTFIRST DO
 ..;Order matters due to flags
 ..IF CONT&SEQ ;Do nothing
 ..IF 'CONT&'SEQ SET Y=Y_","_CURR
 ..IF CONT&'SEQ SET Y=Y_CURR,CONT=0
 ..IF 'CONT&SEQ SET Y=Y_"-",CONT=1
 .SET PREV=CURR
 IF CONT SET Y=Y_PREV
 K I,CONT,NOTFIRST,CURR,PREV,NEXT,SEQ
 QUIT Y
```

Example:

```txt
USER>SET S="0,1,2,4,6,7,8,11,12,14,15,16,17,18,19,20,21,22,23,24,25,27,28,29,30,31,32,33,35,36,37,38,39"

USER>W $$RANGCONT^ROSETTA(S)
0-2,4,6-8,11,12,14-25,27-33,35-39
```



## NetRexx


### NetRexx Ver. 1


```netrexx
/*NetRexx program to test range extraction. ***************************
* 07.08.2012 Walter Pachl derived from my Rexx Version
* Changes: line continuation in aaa assignment changed
*          1e99 -> 999999999
*          Do -> Loop
*          words(aaa) -> aaa.words()
*          word(aaa,i) -> aaa.word(i)
**********************************************************************/
Say 'NetRexx program derived from Rexx'
aaa='0 1 2 4 6 7 8 11 12 14 15 16 17 18 19 20 21 22 23 24 25 27 28 29'
aaa=aaa' 30 31 32 33 35 36 37 38 39'
say 'old='aaa;
aaa=aaa 999999999                   /* artificial number at the end  */
i=0                                 /* initialize index              */
ol=''                               /* initialize output string      */
comma=''                            /* will become a ',' lateron     */
inrange=0
Loop While i<=aaa.words             /* loop for all numbers          */
  i=i+1                             /* index of next number          */
  n=aaa.word(i)                     /* the now current number        */
  If n=999999999 Then Leave         /* we are at the end             */
  If inrange Then Do                /* range was opened              */
    If aaa.word(i+1)<>n+1 Then Do   /* following word not in range   */
      ol=ol||n                      /* so this number is the end     */
      inrange=0                     /* and the range is over         */
      End                           /* else ignore current number    */
    End
  Else Do                           /* not in a range                */
    ol=ol||comma||n                 /* add number (with comma)       */
    comma=','                       /* to the output string          */
    If aaa.word(i+2)=n+2 Then Do    /* if the nr after the next fits */
      inrange=1                     /* open a range                  */
      ol=ol'-'                      /* append the range connector    */
      End
    End
  End
Say 'new='ol
```

```txt

NetRexx program derived from Rexx
old=0 1 2 4 6 7 8 11 12 14 15 16 17 18 19 20 21 22 23 24 25 27 28 29 30 31 32 33 35 36 37 38 39
new=0-2,4,6-8,11,12,14-25,27-33,35-39

```



### NetRexx Ver. 2

```NetRexx
/* NetRexx */
options replace format comments java crossref symbols nobinary

runSample(arg)
return

-- ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
-- Compact a list of numbers by reducing ranges
method compact(expanded) public static
  nums = expanded.changestr(',', ' ').space -- remove possible commas & clean up the string
  rezult = ''

  RANGE = 0
  FIRST = nums.word(1) -- set starting value
  loop i_ = 2 to nums.words -- each word in the string is a number to examine
    LOCAL = nums.word(i_)
    if LOCAL - FIRST - RANGE == 1 then do
      -- inside a range
      RANGE = RANGE + 1
      end
    else do
      -- not inside a range
      if RANGE \= 0 then do
        -- we have a range of numbers so collect this and reset
        rezult = rezult || FIRST || delim(RANGE) || FIRST + RANGE || ','
        RANGE = 0
        end
      else do
        -- just collect this number
        rezult = rezult || FIRST || ','
        end
      FIRST = LOCAL -- bump new starting value
      end
    end i_

  if RANGE \= 0 then do
    -- terminating value is a range
    rezult = rezult || FIRST || delim(RANGE) || FIRST + RANGE
    end
  else do
    -- terminating value is a single number
    rezult = rezult || FIRST
    end

  return rezult.space(1, ',') -- format and return result string

-- ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
-- determine if the range delimiter should be a comma or dash
method delim(range) private static
  if range == 1 then dlm = ','
  else               dlm = '-'
  return dlm

-- ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
-- sample driver
method runSample(arg) public static

parse arg userInput
td = 0
if userInput.words > 0 then do
  -- use input from command line
  td[0] = td[0] + 1; r_ = td[0]; td[r_] = userInput
  end
else do
  -- use canned test data
  td[0] = td[0] + 1; r_ = td[0]; td[r_] = ' -6,  -3,  -2,  -1,   0,   1,   3,  4,  5,   7,  8,  9, 10, 11, 14, 15, 17, 18, 19, 20'
  td[0] = td[0] + 1; r_ = td[0]; td[r_] = '  0,   1,   2,   4,   6,   7,   8, 11,  12, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 27, 28, 29, 30, 31, 32, 33, 35, 36, 37, 38, 39'
  td[0] = td[0] + 1; r_ = td[0]; td[r_] = ' -4,  -3,  -2,   0,   1,   2,   4,  6,  7,   8, 11, 12, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 27, 28, 29, 30, 31, 32, 33, 35, 36, 37, 38, 39'
  end

loop r_ = 1 to td[0]
  say 'Original: ' td[r_].changestr(',', ' ').space(1, ',')
  say 'Compacted:' compact(td[r_])
  say
  end r_
return

```

```txt

Original:  -6,-3,-2,-1,0,1,3,4,5,7,8,9,10,11,14,15,17,18,19,20
Compacted: -6,-3-1,3-5,7-11,14,15,17-20

Original:  0,1,2,4,6,7,8,11,12,14,15,16,17,18,19,20,21,22,23,24,25,27,28,29,30,31,32,33,35,36,37,38,39
Compacted: 0-2,4,6-8,11,12,14-25,27-33,35-39

Original:  -4,-3,-2,0,1,2,4,6,7,8,11,12,14,15,16,17,18,19,20,21,22,23,24,25,27,28,29,30,31,32,33,35,36,37,38,39
Compacted: -4--2,0-2,4,6-8,11,12,14-25,27-33,35-39

```



## Nim


```nim
import parseutils, re, strutils

proc extractRange(input: string): string =
  var list = input.replace(re"\s+").split(',').map(parseInt)
  var ranges: seq[string] = @[]
  var i = 0
  while i < list.len:
    var first = list[i] # first element in the current range
    var offset = i
    while True: # skip ahead to the end of the current range
      if i + 1 >= list.len:
        # reached end of the list
        break
      if list[i + 1] - (i + 1) != first - offset:
        # next element isn't in the current range
        break
      i.inc
    var last = list[i] # last element in the current range
    case last - first
      of 0: ranges.add($first)
      of 1: ranges.add("$1,$2".format([$first, $last]))
      else: ranges.add("$1-$2".format([$first, $last]))
    i.inc
  return ranges.join(",")

echo("""
    0,  1,  2,  4,  6,  7,  8, 11, 12, 14,
   15, 16, 17, 18, 19, 20, 21, 22, 23, 24,
   25, 27, 28, 29, 30, 31, 32, 33, 35, 36,
   37, 38, 39""".extractRange)
```


```txt
0-2, 4, 6-8, 11, 12, 14-25, 27-33, 35-39
```



## Objeck

```objeck
class IdentityMatrix {
  function : Main(args : String[]) ~ Nil {
    Compress2Range("-6, -3, -2, -1, 0, 1, 3, 4, 5, 7, 8, 9, 10, 11, 14, 15, 17, 18, 19, 20")->PrintLine();

    Compress2Range("0,  1,  2,  4,  6,  7,  8, 11, 12, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 27, 28, 29, 30, 31, 32, 33, 35, 36, 37, 38, 39")->PrintLine();
  }

  function : Compress2Range(expanded : String) ~ String {
    result := "";
    nums := expanded->ReplaceAll(" ", "")->Split(",");
    firstNum := nums[0]->ToInt();
    rangeSize := 0;
    for(i:= 1; i < nums->Size(); i += 1;) {
      thisNum := nums[i]->ToInt();
            if(thisNum - firstNum - rangeSize = 1) {
                rangeSize += 1;
            }
      else{
        if(rangeSize <> 0){
          result->Append(firstNum);
          result->Append((rangeSize = 1) ? ",": "-");
          result->Append(firstNum+rangeSize);
          result->Append(",");
                    rangeSize := 0;
                }
        else {
          result->Append(firstNum);
          result->Append(",");
        };
        firstNum := thisNum;
      };
    };

    if(rangeSize <> 0){
      result->Append(firstNum);
      result->Append((rangeSize = 1) ? "," : "-");
      result->Append(firstNum + rangeSize);
      rangeSize := 0;
    }
    else {
      result->Append(firstNum);
    };

    return result;
  }
}

```


=={{header|Oberon-2}}==
Oxford Oberon-2

```oberon2

MODULE RangeExtraction;
IMPORT Out;

PROCEDURE Range(s: ARRAY OF INTEGER);
VAR
	i,j: INTEGER;

	PROCEDURE Emit(sep: CHAR);
	BEGIN
		IF i > 2 THEN
			Out.Int(s[j],3);Out.Char('-');Out.Int(s[j + i - 1],3);Out.Char(sep);
			INC(j,i)
		ELSE
			Out.Int(s[j],3);Out.Char(sep);
			INC(j)
		END;
	END Emit;

BEGIN
	j := 0;i := -1;
	LOOP
		INC(i);
		IF j + i >= LEN(s) THEN
			Emit(0AX);
			EXIT
		ELSIF s[j + i] # (s[j] + i) THEN
			Emit(',');
			i := 0;
		END
	END
END Range;

VAR
	seq0: ARRAY 33 OF INTEGER;
	seq1: ARRAY 20 OF INTEGER;
BEGIN
	seq0[0] := 0;
	seq0[1] := 1;
	seq0[2] := 2;
	seq0[3] := 4;
	seq0[4] := 6;
	seq0[5] := 7;
	seq0[6] := 8;
	seq0[7] := 11;
	seq0[8] := 12;
	seq0[9] := 14;
	seq0[10] := 15;
	seq0[11] := 16;
	seq0[12] := 17;
	seq0[13] := 18;
	seq0[14] := 19;
	seq0[15] := 20;
	seq0[16] := 21;
	seq0[17] := 22;
	seq0[18] := 23;
	seq0[19] := 24;
	seq0[20] := 25;
	seq0[21] := 27;
	seq0[22] := 28;
	seq0[23] := 29;
	seq0[24] := 30;
	seq0[25] := 31;
	seq0[26] := 32;
	seq0[27] := 33;
	seq0[28] := 35;
	seq0[29] := 36;
	seq0[30] := 37;
	seq0[31] := 38;
	seq0[32] := 39;
	Range(seq0);
	seq1[0] := -6;
	seq1[1] := -3;
	seq1[2] := -2;
	seq1[3] := -1;
	seq1[4] := 0;
	seq1[5] := 1;
	seq1[6] := 3;
	seq1[7] := 4;
	seq1[8] := 5;
	seq1[9] := 7;
	seq1[10] := 8;
	seq1[11] := 9;
	seq1[12] := 10;
	seq1[13] := 11;
	seq1[14] := 14;
	seq1[15] := 15;
	seq1[16] := 17;
	seq1[17] := 18;
	seq1[18] := 19;
	seq1[19] := 20;
	Range(seq1)
END RangeExtraction.

```

```txt

  0-  2,  4,  6-  8, 11, 12, 14- 25, 27- 33, 35- 39
 -6, -3-  1,  3-  5,  7- 11, 14, 15, 17- 20

```


=={{header|Objective-C}}==
We can use <code>NSIndexSet</code> to do this.
However, it only works for non-negative integers.
```objc>#import <Foundation/Foundation.h


NSString *extractRanges(NSArray *nums) {
  NSMutableIndexSet *indexSet = [[NSMutableIndexSet alloc] init];
  for (NSNumber *n in nums) {
    if ([n integerValue] < 0)
      @throw [NSException exceptionWithName:NSInvalidArgumentException reason:@"negative number not supported" userInfo:nil];
    [indexSet addIndex:[n unsignedIntegerValue]];
  }
  NSMutableString *s = [[NSMutableString alloc] init];
  [indexSet enumerateRangesUsingBlock:^(NSRange range, BOOL *stop) {
    if (s.length)
      [s appendString:@","];
    if (range.length == 1)
      [s appendFormat:@"%lu", range.location];
    else if (range.length == 2)
      [s appendFormat:@"%lu,%lu", range.location, range.location+1];
    else
      [s appendFormat:@"%lu-%lu", range.location, range.location+range.length-1];
  }];
  return s;
}

int main() {
  @autoreleasepool {

    NSLog(@"%@", extractRanges(@[@0, @1, @2, @4, @6, @7, @8, @11, @12, @14,
                                 @15, @16, @17, @18, @19, @20, @21, @22, @23, @24,
                                 @25, @27, @28, @29, @30, @31, @32, @33, @35, @36,
                                 @37, @38, @39]));

  }
  return 0;
}
```

```txt

0-2,4,6-8,11,12,14-25,27-33,35-39

```



## OCaml


```ocaml
let range_extract = function
  | [] -> []
  | x::xs ->
    let f (i,j,ret) k =
      if k = succ j then (i,k,ret) else (k,k,(i,j)::ret) in
    let (m,n,ret) = List.fold_left f (x,x,[]) xs in
    List.rev ((m,n)::ret)

let string_of_range rng =
  let str (a,b) =
    if a = b then string_of_int a
    else Printf.sprintf "%d%c%d" a (if b = succ a then ',' else '-') b in
  String.concat "," (List.map str rng)

let () =
  let li =
    [ 0; 1; 2; 4; 6; 7; 8; 11; 12; 14; 15; 16; 17; 18; 19; 20; 21;
      22; 23; 24; 25; 27; 28; 29; 30; 31; 32; 33; 35; 36; 37; 38; 39 ]
  in
  let rng = range_extract li in
  print_endline(string_of_range rng)
```


```txt
0-2,4,6-8,11,12,14-25,27-33,35-39
```



## ooRexx

```ooRexx
/* Rexx */

parse arg userInput
call runSample userInput
return
exit

-- ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
-- Compact a list of numbers by reducing ranges
compact:
procedure
--trace ?r;nop
  parse arg expanded
  nums = expanded~changestr(',', ' ')~space -- remove possible commas & clean up the string
  rezult = ''

  RANGE = 0
  FIRST = nums~word(1) -- set starting value
  loop i_ = 2 to nums~words -- each word in the string is a number to examine
    LOCAL = nums~word(i_)
    if LOCAL - FIRST - RANGE == 1 then do
      -- inside a range
      RANGE += 1
      end
    else do
      -- not inside a range
      if RANGE \= 0 then do
        -- we have a range of numbers so collect this and reset
        rezult = rezult || FIRST || delim(RANGE) || FIRST + RANGE || ','
        RANGE = 0
        end
      else do
        -- just collect this number
        rezult = rezult || FIRST || ','
        end
      FIRST = LOCAL -- bump new starting value
      end
    end i_
  if RANGE \= 0 then do
    -- collect terminating value (a range)
    rezult = rezult || FIRST || delim(RANGE) || FIRST + RANGE
    end
  else do
    -- collect terminating value (a single number)
    rezult = rezult || FIRST
    end

  return rezult~space(1, ',') -- format and return result string

-- ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
-- determine if the range delimiter should be a comma or dash
delim:
procedure
  parse arg range .
  if range == 1 then dlm = ','
  else               dlm = '-'
  return dlm

-- ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
-- sample driver
runSample:
procedure
parse arg userInput
td. = 0
if userInput~words > 0 then do
  td.0 += 1; r_ = td.0; td.r_ = userInput
  end
else do
  td.0 += 1; r_ = td.0; td.r_ = '-6 -3 -2 -1 0 1 3 4 5 7 8 9 10 11 14 15 17 18 19 20'
  td.0 += 1; r_ = td.0; td.r_ = '0,  1,  2,  4,  6,  7,  8, 11, 12, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 27, 28, 29, 30, 31, 32, 33, 35, 36, 37, 38, 39'
  td.0 += 1; r_ = td.0; td.r_ = '-4, -3, -2, 0, 1, 2, 4, 6, 7, 8, 11, 12, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 27, 28, 29, 30, 31, 32, 33, 35, 36, 37, 38, 39'
  end

loop r_ = 1 to td.0
  say 'Original: ' td.r_~changestr(',', ' ')~space(1, ',')
  say 'Compacted:' compact(td.r_)
  say
  end r_
return

```

```txt

Original:  -6,-3,-2,-1,0,1,3,4,5,7,8,9,10,11,14,15,17,18,19,20
Compacted: -6,-3-1,3-5,7-11,14,15,17-20

Original:  0,1,2,4,6,7,8,11,12,14,15,16,17,18,19,20,21,22,23,24,25,27,28,29,30,31,32,33,35,36,37,38,39
Compacted: 0-2,4,6-8,11,12,14-25,27-33,35-39

Original:  -4,-3,-2,0,1,2,4,6,7,8,11,12,14,15,16,17,18,19,20,21,22,23,24,25,27,28,29,30,31,32,33,35,36,37,38,39
Compacted: -4--2,0-2,4,6-8,11,12,14-25,27-33,35-39

```



## OxygenBasic

```oxygenbasic

   dim sys ints(100)
   ints=>
   0,  1,  2,  4,  6,  7,  8, 11, 12, 14,
   15, 16, 17, 18, 19, 20, 21, 22, 23, 24,
   25, 27, 28, 29, 30, 31, 32, 33, 35, 36,
   37, 38, 39

  function ShowRange(sys*i) as string
  '
### ============================

  pr=""
  n=0
  e=0
  j=0
  k=-1
  do
    j++
    n=i(j)
    e=i(j+1)
    if e<j then
      exit do
    end if
    if e=n+1 and i(j+2)=n+2 then 'LOOKAHEAD
      if k=-1 then k=n
    else
      if k>=0 then
        pr+=k "-" i(j+1) ", " 'RANGE OF VALUES
        j++
        k=-1
      else
        pr+=n ", " 'SINGLE VALUES
      end if
    end if
  end do
  return left pr, len(pr)-2
  end function


  print ShowRange ints

```



## Oz


```oz
declare
  fun {Extract Xs}
     {CommaSeparated
      {Map {ExtractRanges Xs} RangeToString}}
  end

  fun {ExtractRanges Xs}
     fun {Loop Ys Start End}
        case Ys
        of Y|Yr andthen Y == End+1 then {Loop Yr Start Y}
        [] Y|Yr                    then Start#End|{Loop Yr Y Y}
        [] nil                     then [Start#End]
        end
     end
  in
     case Xs
     of X|Xr then {Loop Xr X X}
     [] nil then nil
     end
  end

  fun {RangeToString S#E}
     if E-S >= 2 then
        {VirtualString.toString S#"-"#E}
     else
        {CommaSeparated
         {Map {List.number S E 1} Int.toString}}
     end
  end

  fun {CommaSeparated Xs}
     {Flatten {Intersperse "," Xs}}
  end

  fun {Intersperse Sep Xs}
     case Xs of X|Y|Xr then
        X|Sep|{Intersperse Sep Y|Xr}
     else
        Xs
     end
  end
in
  {System.showInfo
   {Extract [ 0 1 2 4 6 7 8 11 12 14
              15 16 17 18 19 20 21 22 23 24
              25 27 28 29 30 31 32 33 35 36
              37 38 39 ]}}
```


```txt
0-2,4,6-8,11,12,14-25,27-33,35-39
```



## Pascal


```Pascal
program RangeExtraction;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  SysUtils;

function RangeExtraction(const Seq: array of integer): String;
const
  SubSeqLen = 3; // minimal length of the range, can be changed.
var
  i, j: Integer;
  Separator: string = '';
begin
  Result := '';
  i := Low(Seq);
  while i <= High(Seq) do
  begin
    j := i;
    // All subsequent values, starting from i, up to High(Seq) possibly
    while ((j < High(Seq)) and ((Seq[j+1]-Seq[j]) = 1)) do
      Inc(j);
    // is it a range ?
    if ((j-i) >= (SubSeqLen-1)) then
    begin
      Result := Result + Format(Separator+'%d-%d',[Seq[i],Seq[j]]);
      i := j+1; // Next value to be processed
      Separator := ',';
    end
    else
    begin
      // Loop, to process the case SubSeqLen > 3
      while i<=j do
      begin
        Result := Result + Format(Separator+'%d',[Seq[i]]);
        Inc(i); // Next value to be processed
        Separator := ',';
      end;
    end;
  end;
End;

procedure DisplayRange(const Seq: array of integer);
var
  i: Integer;
begin
  Write(Format('[%d', [Seq[Low(Seq)]]));
  for i := Low(Seq) + 1 to High(Seq) do
    Write(Format(',%d', [Seq[i]]));
  WriteLn('] => ' + RangeExtraction(Seq));
  WriteLn;
End;

begin
  DisplayRange([0]);
  DisplayRange([0,1]);
  DisplayRange([0,2]);
  DisplayRange([0,1,2]);
  DisplayRange([0,1,2,3]);
  DisplayRange([0,1,2,3,4,5,6,7]);
  DisplayRange([0,2,3,4,5,6,7,9]);
  DisplayRange([0,2,4,6,8,10]);
  DisplayRange([0,1,2,3,4,5,6,7,9]);
  DisplayRange([0,1,2,3,4,6,9,10,11,12]);

  DisplayRange([
      0,  1,  2,  4,  6,  7,  8, 11, 12, 14,
     15, 16, 17, 18, 19, 20, 21, 22, 23, 24,
     25, 27, 28, 29, 30, 31, 32, 33, 35, 36,
     37, 38, 39]);
  ReadLn;
end.

```


```txt
[0] => 0

[0,1] => 0,1

[0,2] => 0,2

[0,1,2] => 0-2

[0,1,2,3] => 0-3

[0,1,2,3,4,5,6,7] => 0-7

[0,2,3,4,5,6,7,9] => 0,2-7,9

[0,2,4,6,8,10] => 0,2,4,6,8,10

[0,1,2,3,4,5,6,7,9] => 0-7,9

[0,1,2,3,4,6,9,10,11,12] => 0-4,6,9-12

[0,1,2,4,6,7,8,11,12,14,15,16,17,18,19,20,21,22,23,24,25,27,28,29,30,31,32,33,35
,36,37,38,39] => 0-2,4,6-8,11,12,14-25,27-33,35-39
```



## Perl

Using regexes. Also handles +/- and negative integer ranges.


```Perl
sub rangext {
    my $str = join ' ', @_;
    1 while $str =~ s{([+-]?\d+) ([+-]?\d+)}
        {$1.(abs($2 - $1) == 1 ? '~' : ',').$2}eg; # abs for neg ranges
    $str =~ s/(\d+)~(?:[+-]?\d+~)+([+-]?\d+)/$1-$2/g;
    $str =~ tr/~/,/;
    return $str;
}

# Test and display
my @test = qw(0  1  2  4  6  7  8 11 12 14
             15 16 17 18 19 20 21 22 23 24
             25 27 28 29 30 31 32 33 35 36
             37 38 39);
print rangext(@test), "\n";
```


```txt
0-2,4,6-8,11,12,14-25,27-33,35-39
```


```Perl
use Set::IntSpan;
sub rangext { return Set::IntSpan->new(@_) . '' }  # stringized
```


```Perl
use Set::IntSpan::Fast;
sub rangext { return Set::IntSpan::Fast->new(@_)->as_string }
```


<code>Set::IntSpan</code> and <code>Set::IntSpan::Fast</code> are similar.  "Fast" does a binary search for member testing (not part of the task here).  Both accept negatives.


## Perl 6


```perl6
sub range-extraction (*@ints) {
    my $prev = NaN;
    my @ranges;

    for @ints -> $int {
        if $int == $prev + 1 {
            @ranges[*-1].push: $int;
        }
        else {
            @ranges.push: [$int];
        }
        $prev = $int;
    }
    join ',', @ranges.map: -> @r { @r > 2 ?? "@r[0]-@r[*-1]" !! @r }
}

say range-extraction
    -6, -3, -2, -1, 0, 1, 3, 4, 5, 7, 8, 9, 10, 11, 14, 15, 17, 18, 19, 20;

say range-extraction
    0,  1,  2,  4,  6,  7,  8, 11, 12, 14,
    15, 16, 17, 18, 19, 20, 21, 22, 23, 24,
    25, 27, 28, 29, 30, 31, 32, 33, 35, 36,
    37, 38, 39;
```


```txt
-6,-3-1,3-5,7-11,14,15,17-20
0-2,4,6-8,11,12,14-25,27-33,35-39
```



## Phix


```Phix
function spout(integer first, integer this, sequence s)
string res
    if first=this-1 then
        res = sprintf("%d",s[first])
    else
        res = sprintf("%d%s%d",{s[first],iff(first=this-2?',':'-'),s[this-1]})
    end if
    return res
end function

function extract_ranges(sequence s)
integer first = 1
string out = ""
    if length(s)!=0 then
        for i=2 to length(s) do
            if s[i]!=s[i-1]+1 then
                out &= spout(first,i,s)&','
                first = i
            end if
        end for
        out &= spout(first,length(s)+1,s)
    end if
    return out
end function

puts(1,extract_ranges({0,1,2,4,6,7,8,11,12,14,15,16,17,18,19,20,21,22,23,24,25,27,28,29,30,31,32,33,35,36,37,38,39}))
```

```txt

0-2,4,6-8,11,12,14-25,27-33,35-39

```



## PicoLisp


```PicoLisp
(de rangeextract (Lst)
   (glue ","
      (make
         (while Lst
            (let (N (pop 'Lst)  M N)
               (while (= (inc M) (car Lst))
                  (setq M (pop 'Lst)) )
               (cond
                  ((= N M) (link N))
                  ((= (inc N) M) (link N M))
                  (T (link (list N '- M))) ) ) ) ) ) )
```

```txt
: (rangeextract
   (0 1 2 4 6 7 8 11 12 14 15 16 17 18 19 20 21 22
      23 24 25 27 28 29 30 31 32 33 35 36 37 38 39 ) )

-> "0-2,4,6-8,11,12,14-25,27-33,35-39"
```



## PL/I


```pli
/* Modified 19 November 2011 to meet requirement that there be at  */
/* least 3 items in a run.                                         */
range_extraction:                         /* 17 August 2010 */
   procedure options (main);
   declare (c, d) character (1);
   declare (old, new, initial) fixed binary (31);
   declare in file;
   declare out file output;

   open file (in)  title ('/range2.dat,type(text),recsize(80)' );
   open file (out) output title ('/range2.out,type(text),recsize(70)');

   c = ' '; d = ',';
   get file (in) list (old);
   do forever;
      initial = old;
      on endfile (in) begin;
         put file (out) edit (c, trim(old)) (a);
         stop;
      end;
      get file (in) list (new);
      if new = old+1 then
         do; /* we have a run. */
            on endfile (in) begin;
               if old > initial+1 then d = '-';
                  put file (out) edit (c, trim(initial), d, trim(old) ) (a);
               stop;
            end;
            do while (new = old+1);
               old = new;
               get file (in) list (new);
            end;
            /* At this point, old holds the last in a run;           */
            /* initial holds the first in a run.                     */
            /* if there are only two members in a run, don't use the */
            /* range notation.                                       */
            if old > initial+1 then d = '-';
               put file (out) edit (c, trim(initial), d, trim(old) ) (a);
            old = new;
         end;
      else /* we have an isolated value. */
         do;
            put file (out) edit (c, trim(old)) (a);
            old = new;
         end;
      c, d = ',';
   end;
end range_extraction;
```


OUTPUT 17/8/2010:
<lang>
 0-2,4,6-8,11-12,14-25,27-33,35-39

```

```txt

 0-2,4,6-8,11,12,14-25,27-33,35-39

```


## PowerShell


```PowerShell

function range-extraction($arr) {
    if($arr.Count -gt 2) {
        $a, $b, $c, $arr = $arr
        $d = $e = $c
        if((($a + 1) -eq $b) -and (($b + 1) -eq $c)) {
            $test = $true
            while($arr -and $test) {
                $d = $e
                $e, $arr = $arr
                $test = ($d+1) -eq $e
            }
            if($test){"$a-$e"}
            elseif((-not $arr) -and $test){"$a-$d"}
            elseif(-not $arr){"$a-$d,$e"}
            else{"$a-$d," + (range-extraction (@($e)+$arr))}
        }
        elseif(($b + 1) -eq $c) {"$a," + (range-extraction (@($b, $c)+$arr))}
        else {"$a,$b," + (range-extraction (@($c)+$arr))}
    } else {
        switch($arr.Count) {
            0 {""}
            1 {"$arr"}
            2 {"$($arr[0]),$($arr[1])"}
        }
    }
}
range-extraction @(0,  1,  2,  4,  6,  7,  8, 11, 12, 14,
15, 16, 17, 18, 19, 20, 21, 22, 23, 24,
25, 27, 28, 29, 30, 31, 32, 33, 35, 36,
37, 38, 39)

```

<b>Output:</b>

```txt

0-2,4,6-8,11,12,14-25,27-33,35-39

```



## Prolog

Works with SWI-Prolog and library clpfd.<BR>
The code uses three predicates '''extract_Range/2''', '''study_Range/2''' and '''pack_Range/2'''.<BR>
Every predicate works in both directions arg1 towards arg2 and arg2 towards arg1, so that '''Range extraction''' and '''Range expansion''' work with the same predicates but in reverse order.

```Prolog
range_extract :-
	L = [0,  1,  2,  4,  6,  7,  8, 11, 12, 14,
	     15, 16, 17, 18, 19, 20, 21, 22, 23, 24,
	     25, 27, 28, 29, 30, 31, 32, 33, 35, 36,
	     37, 38, 39] ,
	writeln(L),
	pack_Range(L, LP),
	maplist(study_Range, R, LP),
	extract_Range(LA, R),
	atom_chars(A, LA),
	writeln(A).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% extract_Range(?In, ?Out)
% In  : '-6,-3--1,3-5,7-11,14,15,17-20' =>
% Out : [-6], [-3--1], [3-5],[7-11], [14],[15], [17-20]
%
extract_Range([], []).


extract_Range(X , [Range | Y1]) :-
	get_Range(X, U-U, Range, X1),
	extract_Range(X1, Y1).



get_Range([], Range-[], Range, []).
get_Range([','|B], Range-[], Range, B) :- !.

get_Range([A | B], EC, Range, R) :-
	append_dl(EC, [A | U]-U, NEC),
	get_Range(B, NEC, Range, R).


append_dl(X-Y, Y-Z, X-Z).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% study Range(?In, ?Out)
% In  : [-6]
% Out : [-6,-6]
%
% In  : [-3--1]
% Out : [-3, -1]
%
study_Range(Range1, [Deb, Deb]) :-
       catch(number_chars(Deb, Range1), Deb, false).

study_Range(Range1, [Deb, Fin]) :-
       append(A, ['-'|B], Range1),
       A \= [],
       number_chars(Deb, A),
       number_chars(Fin, B).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
:- use_module(library(clpfd)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Pack Range(?In, ?Out)
% In  : -6,
% Out : [-6]
%
% In  : -3, -2,-1
% Out : [-3,-1]
%
pack_Range([],[]).

pack_Range([X|Rest],[[X | V]|Packed]):-
    run(X,Rest, [X|V], RRest),
    pack_Range(RRest,Packed).



run(Fin,[Other|RRest], [Deb, Fin],[Other|RRest]):-
	Fin #\= Deb,
	Fin #\= Deb + 1,
	Other #\= Fin+1.

run(Fin,[],[_Var, Fin],[]).

run(Var,[Var1|LRest],[Deb, Fin], RRest):-
	Fin #\= Deb,
	Fin #\= Deb + 1,
	Var1 #= Var + 1,
	run(Var1,LRest,[Deb, Fin], RRest).

run(Val,[Other|RRest], [Val, Val],[Other|RRest]).

```


```txt
?- range_extract.
[0,1,2,4,6,7,8,11,12,14,15,16,17,18,19,20,21,22,23,24,25,27,28,29,30,31,32,33,35,36,37,38,39]
0-2,4,6-8,11,12,14-25,27-33,35-39
true
```



## PureBasic

Even though the example integer list only includes ascending ranges
this code will also handles descending ranges.

```PureBasic
DataSection
  Data.i  33 ;count of elements to be read
  Data.i  0,  1,  2,  4,  6,  7,  8, 11, 12, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24
  Data.i  25, 27, 28, 29, 30, 31, 32, 33, 35, 36, 37, 38, 39
EndDataSection

NewList values()
;setup list
Define elementCount, i
Read.i elementCount
For i = 1 To elementCount
  AddElement(values()): Read.i values()
Next

Procedure.s rangeExtract(List values())
  Protected listSize = ListSize(values()) - 1
  Protected rangeMarker, rangeStart, rangeIncrement, retraceSteps, rangeSize, endOfRange, output.s, sub.s

  ForEach values()
    rangeStart = values():
    sub = Str(rangeStart)
    If NextElement(values())
      retraceSteps = 1
      rangeIncrement = values() - rangeStart
      If rangeIncrement = 1 Or rangeIncrement = -1
        ;found start of possible range
        If ListIndex(values()) <> listSize
          retraceSteps = 2
          rangeSize = 2
          endOfRange = #False
          rangeMarker = values()
          While NextElement(values())
            If values() - rangeMarker <> rangeIncrement
              endOfRange = #True
              Break
            EndIf
            rangeSize + 1
            rangeMarker = values()
          Wend

          If rangeSize > 2
            sub = Str(rangeStart) + "-" + Str(rangeMarker)
            If Not endOfRange
              retraceSteps = 0 ;at end of list
            Else
              retraceSteps = 1
            EndIf
          EndIf
        EndIf
      EndIf

      ;return to the value before look-aheads
      While retraceSteps > 0
        PreviousElement(values()): retraceSteps - 1
      Wend
    EndIf

    output + sub + ","
  Next

  ProcedureReturn RTrim(output, ",")
EndProcedure

If OpenConsole()
  PrintN(rangeExtract(values()))

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit")
  Input()
  CloseConsole()
EndIf
```

```txt
0-2,4,6-8,11,12,14-25,27-33,35-39
```



## Python


### Procedural


### =Python: for ordered sequences=


```python
def range_extract(lst):
    'Yield 2-tuple ranges or 1-tuple single elements from list of increasing ints'
    lenlst = len(lst)
    i = 0
    while i< lenlst:
        low = lst[i]
        while i <lenlst-1 and lst[i]+1 == lst[i+1]: i +=1
        hi = lst[i]
        if   hi - low >= 2:
            yield (low, hi)
        elif hi - low == 1:
            yield (low,)
            yield (hi,)
        else:
            yield (low,)
        i += 1

def printr(ranges):
    print( ','.join( (('%i-%i' % r) if len(r) == 2 else '%i' % r)
                     for r in ranges ) )

if __name__ == '__main__':
    for lst in [[-8, -7, -6, -3, -2, -1, 0, 1, 3, 4, 5, 7,
                 8, 9, 10, 11, 14, 15, 17, 18, 19, 20],
                [0, 1, 2, 4, 6, 7, 8, 11, 12, 14, 15, 16, 17, 18, 19, 20, 21, 22,
                 23, 24, 25, 27, 28, 29, 30, 31, 32, 33, 35, 36, 37, 38, 39]]:
        #print(list(range_extract(lst)))
        printr(range_extract(lst))
```


```txt
-8--6,-3-1,3-5,7-11,14,15,17-20
0-2,4,6-8,11,12,14-25,27-33,35-39
```


{{out}} if the <code>print'''r'''(...)</code> statement is commented-out instead of the <code>print(...)</code> statement directly above it.

This shows the tuples yielded by generator function <code>range_extract</code>.

```txt
[(-8, -6), (-3, 1), (3, 5), (7, 11), (14,), (15,), (17, 20)]
[(0, 2), (4,), (6, 8), (11,), (12,), (14, 25), (27, 33), (35, 39)]
```



### =Python: For ordered iterables=

A more general method that works on any sequential [https://docs.python.org/3/library/collections.abc.html?highlight=iterable#collections.abc.Iterable Iterable] of integers, not only [https://docs.python.org/3/library/collections.abc.html?highlight=iterable#collections.abc.Sequence Sequences]:


```python
def range_extract(iterable):
    '''Assumes iterable is sorted sequentially. Returns iterator of range tuples.'''
    it = iter(iterable)

    try:
        i = next(it)
    except StopIteration:
        return

    while True:
        low = i

        try:
            j = next(it)
        except StopIteration:
            yield (low, )
            return
        while i + 1 == j:
            i_next = j
            try:
                j = next(it)
            except StopIteration:
                yield (low, j)
                return
            i = i_next

        hi = i

        if   hi - low >= 2:
            yield (low, hi)
        elif hi - low == 1:
            yield (low,)
            yield (hi,)
        else:
            yield (low,)

        i = j

def printr(ranges):
    print( ','.join( (('%i-%i' % r) if len(r) == 2 else '%i' % r)
                     for r in ranges ) )

if __name__ == '__main__':
    for lst in [[-8, -7, -6, -3, -2, -1, 0, 1, 3, 4, 5, 7,
                 8, 9, 10, 11, 14, 15, 17, 18, 19, 20],
                [0, 1, 2, 4, 6, 7, 8, 11, 12, 14, 15, 16, 17, 18, 19, 20, 21, 22,
                 23, 24, 25, 27, 28, 29, 30, 31, 32, 33, 35, 36, 37, 38, 39]]:
        #print(list(range_extract(lst)))
        printr(range_extract(lst))
```


Identical to previous example.

====Python: Using push-able iterator====
Note that for an iterable yielding <code>1,2,3,6,7,8</code> the only way to determine the end of the first section of incremented numbers, <code>1,2,3</code> is to read the next number <code>6</code>, This next example defines an iterator where the <code>6</code> can be pushed back and so more cleanly made available for inclusion in detrmining the next sub-sequence of <code>6,7,8</code>.


```python
class PushableIter():
    "Can push items back on iterable"
    def __init__(self, it):
        self.it = iter(it)
        self.pushed = []

    def push(self, item):
        self.pushed.append(item)

    def pop(self):
        return self.pushed.pop(0) if self.pushed else self.it.__next__()

    def __iter__(self):
        return self

    def __next__(self):
        return self.pop()

def range_extractp(sorted_iterable):
    'Yield 2-tuple ranges or 1-tuple single elements from iter of increasing ints'
    rest = PushableIter(sorted_iterable)
    for this in rest:
        low = hi = last = this
        for nxt in rest:        # Find upper range on incremented values
            if nxt == last + 1:
                last = hi = nxt
            else:       # Out of (sub)-range
                rest.push(nxt)
                break
        if   hi - low >= 2:
            yield (low, hi)
        elif hi - low == 1:
            yield (low,)
            yield (hi,)
        else:
            yield (low,)
```

When substituted for function <code>range_extract</code> in the first Python example it gives the same results.


### Composition of pure functions


### =Python: splitBy=


Defining a general and reusable '''splitBy''' function, which subdivides any list into groups at the points at which the relationship between consecutive items matches some binary predicate:

```python
'''Range extraction'''

from functools import reduce


# rangeFormat :: [Int] -> String
def rangeFormat(xs):
    '''Range-formatted display string for
       a list of integers.
    '''
    return ','.join([
        rangeString(x) for x
        in splitBy(lambda a, b: 1 < b - a)(xs)
    ])


# rangeString :: [Int] -> String
def rangeString(xs):
    '''Start and end of xs delimited by hyphens
       if there are more than two integers.
       Otherwise, comma-delimited xs.
    '''
    ys = [str(x) for x in xs]
    return '-'.join([ys[0], ys[-1]]) if 2 < len(ys) else (
        ','.join(ys)
    )


# TEST ----------------------------------------------------
# main :: IO ()
def main():
    '''Test'''

    xs = [
        0, 1, 2, 4, 6, 7, 8, 11, 12, 14,
        15, 16, 17, 18, 19, 20, 21, 22, 23, 24,
        25, 27, 28, 29, 30, 31, 32, 33, 35, 36,
        37, 38, 39
    ]
    print(
        __doc__ + ':\n[' + '\n'.join(map(
            lambda x: ' ' + repr(x)[1:-1],
            chunksOf(11)(xs)
        )) + " ]\n\n        -> '" + rangeFormat(xs) + "'\n"
    )


# GENERIC -------------------------------------------------

# chunksOf :: Int -> [a] -> [[a]]
def chunksOf(n):
    '''A series of lists of length n,
       subdividing the contents of xs.
       Where the length of xs is not evenly divible,
       the final list will be shorter than n.'''
    return lambda xs: reduce(
        lambda a, i: a + [xs[i:n + i]],
        range(0, len(xs), n), []
    ) if 0 < n else []


# splitBy :: (a -> a -> Bool) -> [a] -> [[a]]
def splitBy(p):
    '''A list split wherever two consecutive
       items match the binary predicate p.
    '''
    # step :: ([[a]], [a], a) -> a -> ([[a]], [a], a)
    def step(acp, x):
        acc, active, prev = acp
        return (acc + [active], [x], x) if p(prev, x) else (
            (acc, active + [x], x)
        )

    # go :: [a] -> [[a]]
    def go(xs):
        if 2 > len(xs):
            return xs
        else:
            h = xs[0]
            ys = reduce(step, xs[1:], ([], [h], h))
            # The accumulated sublists, and the current group.
            return ys[0] + [ys[1]]

    return lambda xs: go(xs)


# MAIN ---
if __name__ == '__main__':
    main()
```

```txt
Range extraction:
[ 0, 1, 2, 4, 6, 7, 8, 11, 12, 14, 15
 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 27
 28, 29, 30, 31, 32, 33, 35, 36, 37, 38, 39 ]

        -> '0-2,4,6-8,11,12,14-25,27-33,35-39'
```



## Qi


```qi

(define make-range
  Start Start -> ["," Start]
  Start End   -> ["," Start "," End] where (= End (+ Start 1))
  Start End   -> ["," Start "-" End])

(define range-extract-0
  Start End []     -> (make-range Start End)
  Start End [A|As] -> (range-extract-0 Start A As) where (= (+ 1 End) A)
  Start End [A|As] -> (append (make-range Start End) (range-extract-0 A A As)))

(define range-extract
  [A |As] -> (FORMAT NIL "~{~a~}" (tail (range-extract-0 A A As))))

(range-extract [ 0  1  2  4  6  7  8 11 12 14
                15 16 17 18 19 20 21 22 23 24
                25 27 28 29 30 31 32 33 35 36
                37 38 39])

```


```txt

0-2,4,6-8,11,12,14-25,27-33,35-39

```



## R



```rsplus
extract.range = function(v)
   {r = c(1, which(diff(v) != 1) + 1, length(v) + 1)
      # 'r' holds the index of the start of each run of sequential
      # elements.
    paste0(collapse = ",", v[head(r, -1)], ifelse(diff(r) == 1, "",
        paste0(
            ifelse(diff(r) == 2, ",", "-"),
            v[r[-1] - 1])))}

print(extract.range(c(
    -6, -3, -2, -1, 0, 1, 3, 4, 5, 7, 8, 9, 10, 11, 14, 15, 17, 18, 19, 20)))
print(extract.range(c(
    0, 1, 2, 4, 6, 7, 8, 11, 12, 14, 15, 16, 17, 18, 19, 20, 21, 22,
    23, 24, 25, 27, 28, 29, 30, 31, 32, 33, 35, 36, 37, 38, 39)))
```



## Racket


```Racket

#lang racket

(define (list->ranges xs)
  (define (R lo hi)
    (if (= lo hi) (~a lo) (~a lo (if (= 1 (- hi lo)) "," "-") hi)))
  (let loop ([xs xs] [lo #f] [hi #f] [r '()])
    (cond [(null? xs) (string-join (reverse (if lo (cons (R lo hi) r) r)) ",")]
          [(not hi) (loop (cdr xs) (car xs) (car xs) r)]
          [(= 1 (- (car xs) hi)) (loop (cdr xs) lo (car xs) r)]
          [else (loop xs #f #f (cons (R lo hi) r))])))

(list->ranges '(0 1 2 4 6 7 8 11 12 14 15 16 17 18 19 20 21 22 23
                24 25 27 28 29 30 31 32 33 35 36 37 38 39))
;; -> "0-2,4,6-8,11,12,14-25,27-33,35-39"

```



## REXX

Note that the two numbers   '''11'''   and   '''12'''   are not considered a range.

### version 1

This REXX version isn't limited to integers.

```rexx
/*REXX program creates a  range extraction  from a  list of numbers  (can be negative.) */
old=0 1 2 4 6 7 8 11 12 14 15 16 17 18 19 20 21 22 23 24 25 27 28 29 30 31 32 33 35 36 37 38 39
#= words(old)                                    /*number of integers in the number list*/
new=                                             /*the new list, possibly with ranges.  */
     do j=1  to  #;              x= word(old, j) /*obtain Jth number in the  old  list. */
     new= new','  x                              /*append  "    "    to  "   new    "   */
     inc= 1                                      /*start with an increment of one  (1). */
              do k=j+1  to #;    y= word(old, k) /*get the Kth number in the number list*/
              if y\==x+inc  then leave           /*is this number not > previous by inc?*/
              inc= inc + 1;      g= y            /*increase the range, assign  G (good).*/
              end   /*k*/
     if k-1=j   |   g=x+1   then iterate         /*Is the range=0│1?  Then keep truckin'*/
     new= new'-'g;               j= k - 1        /*indicate a range of #s;  change index*/
     end            /*j*/
                                                 /*stick a fork in it,  we're all done. */
new= space( substr(new, 2),  0)                  /*elide leading comma, also all blanks.*/
say 'old:'   old                                 /*display the old range of numbers.    */
say 'new:'   new                                 /*   "     "  new  list  "    "        */
```

'''output'''   when using the (internal) list of numbers:

```txt

old: 0 1 2 4 6 7 8 11 12 14 15 16 17 18 19 20 21 22 23 24 25 27 28 29 30 31 32 33 35 36 37 38 39
new: 0-2,4,6-8,11,12,14-25,27-33,35-39

```



### version 1a

The REXX version is the same as above, but doesn't modify a   '''do'''   loop's index   ('''j''').

```rexx
/*REXX program creates a  range extraction  from a  list of numbers  (can be negative.) */
old=0 1 2 4 6 7 8 11 12 14 15 16 17 18 19 20 21 22 23 24 25 27 28 29 30 31 32 33 35 36 37 38 39
#= words(old);        j= 0                       /*number of integers in the number list*/
new=                                             /*the new list, possibly with ranges.  */
     do  while j<#;   j= j + 1;  x= word(old, j) /*get the Jth number in the number list*/
     new=new','  x                               /*append  "    "    to  "   new    "   */
     inc=1                                       /*start with an increment of one  (1). */
              do k=j+1  to #;    y= word(old, k) /*get the Kth number in the number list*/
              if y\==x+inc  then leave           /*is this number not > previous by inc?*/
              inc= inc + 1;      g= y            /*increase the range, assign  G (good).*/
              end   /*k*/
     if k-1=j   |   g=x+1   then iterate         /*Is the range=0│1?  Then keep truckin'*/
     new= new'-'g;               j= k - 1        /*indicate a range of numbers; change J*/
     end            /*while*/
                                                 /*stick a fork in it,  we're all done. */
new= space( substr(new, 2),  0)                  /*elide leading comma, also all blanks.*/
say 'old:'    old                                /*display the old range of numbers.    */
say 'new:'    new                                /*   "     "  new  list  "    "        */
```

'''output'''   is the same as the 1<sup>st</sup> REXX version.




### version 2

Somewhat simplified !?!

```rexx
/*REXX program to test range extraction. ******************************
* 07.08.2012 Walter Pachl
**********************************************************************/
aaa='0 1 2 4 6 7 8 11 12 14 15 16 17 18 19 20 21 22 23 24 25 27 28 29',
    '30 31 32 33 35 36 37 38 39'
say 'old='aaa;
aaa=aaa 1e99                        /* artificial number at the end  */
i=0                                 /* initialize index              */
ol=''                               /* initialize output string      */
comma=''                            /* will become a ',' lateron     */
inrange=0
Do While i<=words(aaa)              /* loop for all numbers          */
  i=i+1                             /* index of next number          */
  n=word(aaa,i)                     /* the now current number        */
  If n=1e99 Then Leave              /* we are at the end             */
  If inrange Then Do                /* range was opened              */
    If word(aaa,i+1)<>n+1 Then Do   /* following word not in range   */
      ol=ol||n                      /* so this number is the end     */
      inrange=0                     /* and the range is over         */
      End                           /* else ignore current number    */
    End
  Else Do                           /* not in a range                */
    ol=ol||comma||n                 /* add number (with comma)       */
    comma=','                       /* to the output string          */
    If word(aaa,i+2)=n+2 Then Do    /* if the nr after the next fits */
      inrange=1                     /* open a range                  */
      ol=ol'-'                      /* append the range connector    */
      End
    End
  End
Say 'new='ol

```

Output is the same as above.


## Ring


```ring

# Project : Range extraction

int = "0,1,2,4,6,7,8,11,12,14,15,16,17,18,19,20,21,22,23,24,25,27,28,29,30,31,32,33,35,36,37,38,39"
int = str2list(substr(int, ",", nl))
sumint = []
intnew = 1
for n=1 to len(int)
     flag = 0
     nr = 0
     intnew = 0
     for m=n to len(int)-1
         if int[m] = int[m+1] - 1
            intnew = m+1
            flag = 1
            nr = nr + 1
         else
            exit
          ok
     next
     if flag = 1 and nr > 1
        if intnew != 0
           add(sumint, [n,intnew])
           n = m
        ok
     else
        add(sumint, [n,""])
     ok
next
showarray(sumint)

func showarray(vect)
       see "["
       svect = ""
       for n = 1 to len(vect)
           if vect[n][2] != ""
              svect = svect +"" + int[vect[n][1]] + "-" + int[vect[n][2]] + ", "
           else
              svect = svect +"" + int[vect[n][1]] + ", "
           ok
       next
       svect = left(svect, len(svect) - 2)
       see svect
       see "]" + nl

```

Output:

```txt

[0-2, 4, 6-8, 11, 12, 14-25, 27-33, 35-39]

```



## Ruby


```ruby
def range_extract(l)
  # pad the list with a big value, so that the last loop iteration will
  # append something to the range
  sorted, range = l.sort.concat([Float::MAX]), []
  canidate_number = sorted.first

  # enumerate over the sorted list in pairs of current number and next by index
  sorted.each_cons(2) do |current_number, next_number|
    # if there is a gap between the current element and its next by index
    if current_number.succ < next_number
      # if current element is our first or our next by index
      if canidate_number == current_number
        # put the first element or next by index into our range as a string
        range << canidate_number.to_s
      else
        # if current element is not the same as the first or next
        # add [first or next, first or next equals current add , else -, current]
        seperator = canidate_number.succ == current_number ? "," : "-"
        range << "%d%s%d" % [canidate_number, seperator, current_number]
      end
      # make the first element the next element
      canidate_number = next_number
    end
  end
  range.join(',')
end

lst = [
    0,  1,  2,  4,  6,  7,  8, 11, 12, 14,
   15, 16, 17, 18, 19, 20, 21, 22, 23, 24,
   25, 27, 28, 29, 30, 31, 32, 33, 35, 36,
   37, 38, 39
]

p rng = range_extract(lst)
```


```txt
"0-2,4,6-8,11,12,14-25,27-33,35-39"
```



Enumerable#slice_when method became usable.

```ruby
ary = [0,1,2,4,6,7,8,11,12,14,15,16,17,18,19,20,21,22,23,24,25,27,28,29,30,31,32,33,35,36,37,38,39]
puts ary.sort.slice_when{|i,j| i+1 != j}.map{|a| a.size<3 ? a : "#{a[0]}-#{a[-1]}"}.join(",")
```


```txt
0-2,4,6-8,11,12,14-25,27-33,35-39
```



## Rust

Iterators are very Rustic. This solution is generic for all numeric types.

```rust
use std::ops::Add;

struct RangeFinder<'a, T: 'a> {
    index: usize,
    length: usize,
    arr: &'a [T],
}

impl<'a, T> Iterator for RangeFinder<'a, T> where T: PartialEq + Add<i8, Output=T> + Copy {
    type Item = (T,  Option<T>);
    fn next(&mut self) -> Option<Self::Item> {
        if self.index == self.length {
            return None;
        }
        let lo = self.index;
        while self.index < self.length - 1 && self.arr[self.index + 1] == self.arr[self.index] + 1 {
            self.index += 1
        }
        let hi = self.index;
        self.index += 1;
        if hi - lo > 1 {
            Some((self.arr[lo], Some(self.arr[hi])))
        } else {
            if hi - lo == 1 {
                self.index -= 1
            }
            Some((self.arr[lo], None))
        }
    }
}

impl<'a, T> RangeFinder<'a, T> {
    fn new(a: &'a [T]) -> Self {
        RangeFinder {
            index: 0,
            arr: a,
            length: a.len(),
        }
    }
}

fn main() {
    let input_numbers : &[i8] = &[0,  1,  2,  4,  6,  7,  8, 11, 12, 14,
                                  15, 16, 17, 18, 19, 20, 21, 22, 23, 24,
                                  25, 27, 28, 29, 30, 31, 32, 33, 35, 36,
                                  37, 38, 39];
    for (i, (lo, hi)) in RangeFinder::new(&input_numbers).enumerate() {
        if i > 0 {print!(",")}
        print!("{}", lo);
        if hi.is_some() {print!("-{}", hi.unwrap())}
    }
    println!("");
}
```


```txt
0-2,4,6-8,11,12,14-25,27-33,35-39
```


Note: You could make the above solution even a little more generic in Nightly Rust (which is version 1.6 at the time of writing) by making the following additions:

Add this to the top of the file:

```rust
#![feature(zero_one)]
use std::num::One;
```


Changing this line:

```rust
 impl<'a, T> Iterator for RangeFinder<'a, T> where T: PartialEq + Add<i8, Output=T> + Copy {
```

to this:

```rust
impl<'a, T> Iterator for RangeFinder<'a, T> where T: PartialEq + Add<T, Output=T> + Copy + One {
```


And this line:

```rust
 while self.index < self.length - 1 && self.arr[self.index + 1] == self.arr[self.index] + 1 {
```

to this:

```rust
 while self.index < self.length - 1 && self.arr[self.index + 1] == self.arr[self.index] + T::one() {
```



## Scala


```scala
object Range {
   def spanRange(ls:List[Int])={
     var last=ls.head
     ls span {x => val b=x<=last+1; last=x; b}
   }

   def toRangeList(ls:List[Int]):List[List[Int]]=ls match {
      case Nil => List()
      case _ => spanRange(ls) match {
         case (range, Nil) => List(range)
         case (range, rest) => range :: toRangeList(rest)
      }
   }

   def toRangeString(ls:List[List[Int]])=ls map {r=>
      if(r.size<3) r mkString ","
      else r.head + "-" + r.last
   } mkString ","

   def main(args: Array[String]): Unit = {
      var l=List(0, 1, 2, 4, 6, 7, 8, 11, 12, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25,
                 27, 28, 29, 30, 31, 32, 33, 35, 36, 37, 38, 39)
      println(toRangeString(toRangeList(l)))
   }
}
```


```txt
0-2,4,6-8,11,12,14-25,27-33,35-39
```



## Scheme

```scheme

(define (make-range start end)
  (cond ((= start end)
         `("," ,start))
        ((= end (+ start 1))
         `("," ,start "," ,end))
        (else
         `("," ,start "-" ,end))))

(define (range-extract-0 start end a)
  (cond ((null? a)
         (make-range start end))
        ((= (+ 1 end) (car a))
         (range-extract-0 start (car a) (cdr a)))
        (else
         (append (make-range start end)
                 (range-extract-0 (car a) (car a) (cdr a))))))

(define (range-extract a)
  (apply string-append (map (lambda (x)
                              (if (number? x)
                                  (number->string x)
                                  x))
                            (cdr (range-extract-0 (car a) (car a) (cdr a))))))

(range-extract '( 0  1  2  4  6  7  8 11 12 14
                 15 16 17 18 19 20 21 22 23 24
                 25 27 28 29 30 31 32 33 35 36
                 37 38 39))

```


```txt

0-2,4,6-8,11,12,14-25,27-33,35-39

```



## Seed7


```seed7
$ include "seed7_05.s7i";

const func string: rangeExtraction (in array integer: numbers) is func
  result
    var string: rangeStri is "";
  local
    var integer: index is 1;
    var integer: index2 is 1;
  begin
    while index <= length(numbers) do
      while index2 <= pred(length(numbers)) and numbers[succ(index2)] = succ(numbers[index2]) do
        incr(index2);
      end while;
      if succ(index) < index2 then
        rangeStri &:= "," <& numbers[index] <& "-" <& numbers[index2];
      else
        while index <= index2 do
          rangeStri &:= "," <& numbers[index];
          incr(index);
	end while;
      end if;
      incr(index2);
      index := index2;
    end while;
    rangeStri := rangeStri[2 ..];
  end func;

const proc: main is func
  begin
    writeln(rangeExtraction([] (0, 1, 2, 4, 6, 7, 8, 11, 12, 14, 15, 16, 17, 18, 19,
        20, 21, 22, 23, 24, 25, 27, 28, 29, 30, 31, 32, 33, 35, 36, 37, 38, 39)));
  end func;
```


```txt

0-2,4,6-8,11,12,14-25,27-33,35-39

```



## SNOBOL4

Handles +/- and negative ranges.


```SNOBOL4
*       # Absolute value
        define('abs(n)') :(abs_end)
abs     abs = ~(abs = lt(n,0) -n) n :(return)
abs_end

        define('rangext(str)d1,d2') :(rangext_end)
rangext num = ('+' | '-' | '') span('0123456789')
rxt1    str ',' span(' ') = ' ' :s(rxt1)
rxt2    str num . d1 ' ' num . d2 =
+           d1 ('~,' ? *eq(abs(d2 - d1),1) '~' | ',') d2 :s(rxt2)
rxt3    str ('~' | '-') num '~' = '-' :s(rxt3)
rxt4    str '~' = ',' :s(rxt4)
        rangext = str :(return)
rangext_end

*       # Test and display
        test =  '0,  1,  2,  4,  6,  7,  8, 11, 12, 14, '
+              '15, 16, 17, 18, 19, 20, 21, 22, 23, 24, '
+              '25, 27, 28, 29, 30, 31, 32, 33, 35, 36, '
+              '37, 38, 39'
        output = rangext(test)
end
```


```txt
0-2,4,6-8,11,12,14-25,27-33,35-39
```



## Swift

```swift

import Darwin

func ranges(from ints:[Int]) -> [(Int, Int)] {

	var range : (Int, Int)?
	var ranges = [(Int, Int)]()
	for this in ints {
		if let (start, end) = range {
			if this == end + 1 {
				range = (start, this)
			}
			else {
				ranges.append(range!)
				range = (this, this)
			}
		}
		else { range = (this, this) }
	}
	ranges.append(range!)

	return ranges
}

func description(from ranges:[(Int, Int)]) -> String {
	var desc = ""
	for (start, end) in ranges {
		desc += desc.isEmpty ? "" : ","
		if start == end {
			desc += "\(start)"
		}
		else if end == start + 1 {
			desc += "\(start),\(end)"
		}
		else {
			desc += "\(start)-\(end)"
		}
	}
	return desc
}


let ex = [-6, -3, -2, -1, 0, 1, 3, 4, 5, 7, 8, 9, 10, 11, 14, 15, 17, 18, 19, 20]
let longer = [0,  1,  2,  4,  6,  7,  8, 11, 12, 14,
	15, 16, 17, 18, 19, 20, 21, 22, 23, 24,
	25, 27, 28, 29, 30, 31, 32, 33, 35, 36,
	37, 38, 39]

print(description(from: ranges(from: ex)))
print(description(from: ranges(from: longer)))

```


```txt
-6,-3-1,3-5,7-11,14,15,17-20
0-2,4,6-8,11,12,14-25,27-33,35-39
```



## Tailspin


```tailspin

templates extract
  templates out
    <{start: <$.end>}> '$.start;' !
    <{start: <$.end+1>}> '$.start;,$.end;' !
    <> '$.start;-$.end;' !
  end out
  @: {start: $(1), end: $(1)};
  [ $(2..-1)... -> #, $@ -> out ] -> '$...;' !
  <$@.end+1> @.end: $;
  <> $@ -> out !
     ',' !
     @: {start: $, end: $};
end extract

[0,  1,  2,  4,  6,  7,  8, 11, 12, 14,
    15, 16, 17, 18, 19, 20, 21, 22, 23, 24,
    25, 27, 28, 29, 30, 31, 32, 33, 35, 36,
    37, 38, 39] -> extract -> !OUT::write

```

```txt

0-2,4,6-8,11-12,14-25,27-33,35-39

```



## Tcl


```tcl
proc rangeExtract list {
    set result [lindex $list 0]
    set first [set last [lindex $list 0]]
    foreach term [lrange $list 1 end] {
	if {$term == $last+1} {
	    set last $term
	    continue
	}
	if {$last > $first} {
	    append result [expr {$last == $first+1 ? "," : "-"}] $last
	}
	append result "," $term
	set first [set last $term]
    }
    if {$last == $first+1} {
	append result "," $last
    } elseif {$last > $first} {
	append result "-" $last
    }
    return $result
}

# Commas already removed so it is a natural Tcl list
puts [rangeExtract {
    0 1 2 4 6 7 8 11 12 14
    15 16 17 18 19 20 21 22 23 24
    25 27 28 29 30 31 32 33 35 36
    37 38 39
}]
```

```txt
0-2,4,6-8,11,12,14-25,27-33,35-39
```



## TXR



```txrlisp
(defun range-extract (numbers)
  `@{(mapcar [iff [callf > length (ret 2)]
                  (ret `@[@1 0]-@[@1 -1]`)
                  (ret `@{@1 ","}`)]
             (mapcar (op mapcar car)
                     (split [window-map 1 :reflect
                                        (op list @2 (- @2 @1))
                                        (sort (uniq numbers))]
                            (op where [chain second (op < 1)])))) ","}`)
```


```txt
$ txr
This is the TXR Lisp interactive listener of TXR 126.
Use the :quit command or type Ctrl-D on empty line to exit.
1> (load "range.tl")
nil
2> (range-extract '(0 1 2 4 6 7 8 11 12 14 15 16 17 18 19 20 21 22 23 24 25 27 28 29 30 31 32 33 35 36 37 38 39))
"0-2,4,6-8,11,12,14-25,27-33,35-39"
```



## TUSCRIPT


```tuscript

$$ MODE TUSCRIPT,{}
MODE DATA
$$ numbers=*
0,  1,  2,  4,  6,  7,  8, 11, 12, 14,
15, 16, 17, 18, 19, 20, 21, 22, 23, 24,
25, 27, 28, 29, 30, 31, 32, 33, 35, 36,
37, 38, 39
$$ MODE TUSCRIPT
numbers=EXCHANGE   (numbers,":,{0-00} :':")
unrangednrs=JOIN   (numbers,"")
rangednrs=COMBINE  (unrangednrs,"")
rangednrs=EXCHANGE (rangednrs,":':,:")
PRINT rangednrs

```

Output:

```txt

0-2,4,6-8,11,12,14-25,27-33,35-39

```


Solution without COMBINE

```tuscript

$$ MODE TUSCRIPT
MODE DATA
$$ numbers=*
0,  1,  2,  4,  6,  7,  8, 11, 12, 14,
15, 16, 17, 18, 19, 20, 21, 22, 23, 24,
25, 27, 28, 29, 30, 31, 32, 33, 35, 36,
37, 38, 39
$$ MODE TUSCRIPT
numbers=EXCHANGE (numbers,":,><<> :':")
unrangednrs=JOIN (numbers,"")

help = APPEND (unrangednrs, "999999999")
rest = REMOVE (help, 1, n_1)
n_2 = n_1, n_3= n_2 + 1,rangednrs= ""
LOOP n= rest
 IF (n!=n_3)  THEN
    rangednrs = APPEND (rangednrs, n_1)
    IF (n_1!=n_2) THEN
    range=n_1+1
      IF (range==n_2) THEN
      rangednrs = APPEND (rangednrs,n_2)
      ELSE
      rangednrs = CONCAT (rangednrs, "-", n_2)
      ENDIF
    ENDIF
    n_1 = n
 ENDIF
 n_2 = n, n_3 = n_2 + 1
ENDLOOP
rangednrs=EXCHANGE (rangednrs,":':,:")
PRINT rangednrs

```

```txt

0-2,4,6-8,11,12,14-25,27-33,35-39

```



## UNIX Shell

```bash
#!/usr/bin/bash

range_contract () (
    add_range () {
        case $(( current - range_start )) in
            0) ranges+=( $range_start )          ;;
            1) ranges+=( $range_start $current ) ;;
            *) ranges+=("$range_start-$current") ;;
        esac
    }

    ranges=()
    range_start=$1
    current=$1
    shift

    for number; do
        if (( number > current+1 )); then
            add_range
            range_start=$number
        fi
        current=$number
    done
    add_range

    x="${ranges[@]}"
    echo ${x// /,}
)

range_contract 0 1 2 4 6 7 8 11 12 14 15 16 17 18 19 20 21 22 23 24 25 27 28 29 30 31 32 33 35 36 37 38 39
```

```txt
0-2,4,6-8,11,12,14-25,27-33,35-39
```



## Ursala


```Ursala
#import std
#import int

x = <0,1,2,4,6,7,8,11,12,14,15,16,17,18,19,20,21,22,23,24,25,27,28,29,30,31,32,33,35,36,37,38,39>

f = mat`,+ ==?(~&l,^|T/~& :/`-)*bhPS+ %zP~~hzX*titZBPiNCSiNCQSL+ rlc ^|E/~& predecessor

#show+

t = <f x>
```

```txt
0-2,4,6-8,11,12,14-25,27-33,35-39
```



## VBA


```vb

Public Function RangeExtraction(AList) As String
'AList is a variant that is an array, assumed filled with numbers in ascending order
Const RangeDelim = "-"          'range delimiter
Dim result As String
Dim InRange As Boolean
Dim Posn, ub, lb, rangestart, rangelen As Integer

result = ""
'find dimensions of AList
ub = UBound(AList)
lb = LBound(AList)
Posn = lb
While Posn < ub
  rangestart = Posn
  rangelen = 0
  InRange = True
  'try to extend the range
  While InRange
    rangelen = rangelen + 1
    If Posn = ub Then
      InRange = False
    Else
      InRange = (AList(Posn + 1) = AList(Posn) + 1)
      Posn = Posn + 1
    End If
  Wend
  If rangelen > 2 Then 'output the range if it has more than 2 elements
    result = result & "," & Format$(AList(rangestart)) & RangeDelim & Format$(AList(rangestart + rangelen - 1))
  Else 'output the separate elements
    For i = rangestart To rangestart + rangelen - 1
      result = result & "," & Format$(AList(i))
    Next
  End If
  Posn = rangestart + rangelen
Wend
RangeExtraction = Mid$(result, 2) 'get rid of first comma!
End Function


Public Sub RangeTest()
'test function RangeExtraction
'first test with a Variant array
Dim MyList As Variant
MyList = Array(0, 1, 2, 4, 6, 7, 8, 11, 12, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 27, 28, 29, 30, 31, 32, 33, 35, 36, 37, 38, 39)
Debug.Print "a) "; RangeExtraction(MyList)

'next test with an array of integers
Dim MyOtherList(1 To 20) As Integer
MyOtherList(1) = -6
MyOtherList(2) = -3
MyOtherList(3) = -2
MyOtherList(4) = -1
MyOtherList(5) = 0
MyOtherList(6) = 1
MyOtherList(7) = 3
MyOtherList(8) = 4
MyOtherList(9) = 5
MyOtherList(10) = 7
MyOtherList(11) = 8
MyOtherList(12) = 9
MyOtherList(13) = 10
MyOtherList(14) = 11
MyOtherList(15) = 14
MyOtherList(16) = 15
MyOtherList(17) = 17
MyOtherList(18) = 18
MyOtherList(19) = 19
MyOtherList(20) = 20
Debug.Print "b) "; RangeExtraction(MyOtherList)
End Sub

```


```txt

RangeTest
a) 0-2,4,6-8,11,12,14-25,27-33,35-39
b) -6,-3-1,3-5,7-11,14,15,17-20

```



## VBScript


```vb
Function Range_Extraction(list)
	num = Split(list,",")
	For i = 0 To UBound(num)
		startnum = CInt(num(i))
		sum = startnum
		Do While i <= UBound(num)
			If sum = CInt(num(i)) Then
				If i = UBound(num) Then
					If startnum <> CInt(num(i)) Then
						If startnum + 1 = CInt(num(i)) Then
							Range_Extraction = Range_Extraction & startnum & "," & num(i) & ","
						Else
							Range_Extraction = Range_Extraction & startnum & "-" & num(i) & ","
						End If
					Else
						Range_Extraction = Range_Extraction & startnum & ","
					End If
                                        Exit Do
				Else
					i = i + 1
					sum = sum + 1
				End If
			Else
				If startnum = CInt(num(i-1)) Then
					Range_Extraction = Range_Extraction & startnum & ","
				Else
					If startnum + 1 = CInt(num(i-1)) Then
						Range_Extraction = Range_Extraction & startnum & "," & num(i-1) & ","
					Else
						Range_Extraction = Range_Extraction & startnum & "-" & num(i-1) & ","
					End If
				End If
				i = i - 1
				Exit Do
			End If
		Loop
	Next
	Range_Extraction = Left(Range_Extraction,Len(Range_Extraction)-1)
End Function

WScript.StdOut.Write Range_Extraction("0,1,2,4,6,7,8,11,12,14,15,16,17,18,19,20,21,22,23,24,25,27,28,29,30,31,32,33,35,36,37,38,39")
```

```txt
0-2,4,6-8,11,12,14-25,27-33,35-39
```



## zkl


```zkl
fcn range(ns){
   fcn(w){
      if (w.atEnd) return(Void.Stop);
      a:=b:=w.next(); n:=0;
      while(b+1 == (c:=w.peekN(n))){ n+=1; b=c }
      if(n>1){do(n){w.next()}; return("%d-%d".fmt(a,b)); }
      a
   } :
   (0).pump(*,List,_.fp(ns.walker().tweak(Void,Void))).concat(",");
}
```

The trick here is to use a modified iterator,
one that can look past the end of the sequence without puking.
The function gathers three or more successive ints (saved as a "a-b" string list element) or just returns the first one (as a number) if it can't.
The resulting list is converted to strings separated by commas.

```zkl
var ns=T(-6,-3,-2,-1,0,1,3,4,5,7,8,9,10,11,14,15,17,18,19,20);
range(ns).println();

ns=T(
    0,  1,  2,  4,  6,  7,  8, 11, 12, 14,
   15, 16, 17, 18, 19, 20, 21, 22, 23, 24,
   25, 27, 28, 29, 30, 31, 32, 33, 35, 36,
   37, 38, 39);
range(ns).println();

range([1..100]).println();
```


```txt

-6,-3-1,3-5,7-11,14,15,17-20
0-2,4,6-8,11,12,14-25,27-33,35-39
1-100

```

