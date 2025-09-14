+++
title = "Sum to 100"
description = ""
date = 2019-09-20T16:50:32Z
aliases = []
[extra]
id = 21263
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "ada",
  "aime",
  "algol_68",
  "applescript",
  "autohotkey",
  "awk",
  "c",
  "common_lisp",
  "cpp",
  "csharp",
  "d",
  "elixir",
  "forth",
  "fortran",
  "frink",
  "go",
  "haskell",
  "j",
  "java",
  "javascript",
  "jq",
  "julia",
  "kotlin",
  "lua",
  "mathematica",
  "nim",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "python",
  "racket",
  "rexx",
  "ruby",
  "scala",
  "sidef",
  "tcl",
  "visual_basic_dotnet",
  "zkl",
]
+++

## Task

Find solutions to the   <big> ''sum to one hundred'' </big>   puzzle.


Add (insert) the mathematical
operators     <big> '''+''' </big>   or   <big><big> '''-''' </big></big>     (plus
or minus)   before any of the digits in the

decimal numeric string   <big> '''123456789''' </big>   such that the
resulting mathematical expression adds up to a

particular sum    (in this iconic case,   '''100''').


Example:
         <b> <big>  123 + 4 - 5 + 67 - 89   =   100  </big> </b>

Show all output here.


:*   Show all solutions that sum to   <big> '''100''' </big>
:*   Show the sum that has the maximum   ''number''   of solutions   (from zero to infinity<sup>*</sup>)
:*   Show the lowest positive sum that   ''can't''   be expressed   (has no solutions), using the rules for this task
:*   Show the ten highest numbers that can be expressed using the rules for this task   (extra credit)



An example of a sum that can't be expressed (within the rules of this task) is:   '''5074'''

(which, of course, isn't the lowest positive sum that can't be expressed).


<sup>*</sup>   (where   ''infinity''   would be a relatively small   123,456,789)




## Ada


===The Package Sum_To===

Between any two consecutive digits, there can be a "+", a "-", or no operator. E.g., the digits "4" and "5" occur in the string as either of the following three substrings: "4+5", "4-5", or "45". For the first digit, we only have two choices: "+1" (written as "1"), and "-1". This makes 2*3^8 (two times (three to the power of eight)) different strings. Essential is the generic function Eval in the package Sum_To calls the procedure Callback for each such string Str, with the number Int holding the sum corresponding to the evaluation of Str. The second generic procedure Print is for convenience. If the Sum fits the condition, i.e., if Print_If(Sum, Number), then Print writes Sum = Str to the output.


```Ada
package Sum_To is

   generic
      with procedure Callback(Str: String; Int: Integer);
   procedure Eval;

   generic
      Number: Integer;
      with function Print_If(Sum, Number: Integer) return Boolean;
   procedure Print(S: String; Sum: Integer);

end Sum_To;
```


The implementation of Eval follows the observation above: Eval calls Rec_Eval with the initial string "1" and "-1". For each call, Rec_Eval recursively evaluates a ternary tree with 3^8 leafs. At each leaf, Rec_Eval calls Callback. The implementation of Print is straightforward.


```Ada
with Ada.Text_IO, Ada.Containers.Ordered_Maps;

package body Sum_To is

   procedure Eval is

      procedure Rec_Eval(Str: String; Previous, Current, Next: Integer) is
	 Next_Image: String := Integer'Image(Next);
	 -- Next_Image(1) holds a blank, Next_Image(2) a digit

	 function Sign(N: Integer) return Integer is
	    (if N<0 then -1 elsif N>0 then 1 else 0);

      begin
	 if Next = 10 then -- end of recursion
	    Callback(Str, Previous+Current);
	 else -- Next < 10
	    Rec_Eval(Str & Next_Image(2), -- concatenate current and Next
		 Previous, Sign(Current)*(10*abs(Current)+Next), Next+1);
	    Rec_Eval(Str & "+" & Next_Image(2), -- add Next
		 Previous+Current, Next, Next+1);
	    Rec_Eval(Str & "-" & Next_Image(2), -- subtract Next
		 Previous+Current, -Next, Next+1);
	 end if;
      end Rec_Eval;

   begin -- Eval
      Rec_Eval("1", 0, 1, 2);  -- unary "+", followed by "1"
      Rec_Eval("-1", 0, -1, 2); -- unary "-", followed by "1"
   end Eval;

   procedure Print(S: String; Sum: Integer) is
      -- print solution (S,N), if N=Number
   begin
      if Print_If(Sum, Number) then
	 Ada.Text_IO.Put_Line(Integer'Image(Sum) & " = " & S & ";");
      end if;
   end Print;

end Sum_To;
```



### The First Subtask


Given the package Sum_To, the solution to the first subtask (print all solution for the sum 100) is trivial: Eval_100 calls Print_100 for all 2*3^8 strings, and Print_100 writes the output if the sum is equal to 100.


```Ada
with Sum_To;

procedure Sum_To_100 is

   procedure Print_100 is new Sum_To.Print(100, "=");
   procedure Eval_100 is new Sum_To.Eval(Print_100);

begin
   Eval_100;
end Sum_To_100;
```


```txt
 100 = 123+45-67+8-9;
 100 = 123+4-5+67-89;
 100 = 123-45-67+89;
 100 = 123-4-5-6-7+8-9;
 100 = 12+3+4+5-6-7+89;
 100 = 12+3-4+5+67+8+9;
 100 = 12-3-4+5-6+7+89;
 100 = 1+23-4+56+7+8+9;
 100 = 1+23-4+5+6+78-9;
 100 = 1+2+34-5+67-8+9;
 100 = 1+2+3-4+5+6+78+9;
 100 = -1+2-3+4+5+6+78+9;
```


===The other subtasks (including the extra credit)===

For the three other subtasks, we maintain an ordered map of sums (as the keys) and counters for the number of solutions (as the elements). The procedure Generate_Map generates the Map by calling the procedure Insert_Solution for all 2*3^8 solutions. Finding (1) the sum with the maximal number of solutions, (2) the first sum>=0 without a solution and (3) the ten largest sums with a solution (extra credit) are done by iterating this map.


```Ada
with Sum_To, Ada.Containers.Ordered_Maps, Ada.Text_IO;
use Ada.Text_IO;

procedure Three_Others is

   package Num_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type => Integer, Element_Type => Positive);
   use Num_Maps;

   Map: Num_Maps.Map;
   -- global Map stores how often a sum did occur

   procedure Insert_Solution(S: String; Sum: Integer) is
      -- inserts a solution into global Map
      use Num_Maps;
      -- use type Num_Maps.Cursor;
      Position: Cursor := Map.Find(Sum);
   begin
      if Position = No_Element then -- first solutions for Sum
	 Map.Insert(Key => Sum, New_Item => 1); -- counter is 1
      else -- increase counter for Sum
	 Map.Replace_Element(Position => Position,
			     New_Item => (Element(Position))+1);
      end if;
   end Insert_Solution;

   procedure Generate_Map is new Sum_To.Eval(Insert_Solution);

   Current: Cursor; -- Points into Map
   Sum: Integer;    -- current Sum of interest
   Max: Natural;
begin
   Generate_Map;

   -- find Sum >= 0  with maximum number of solutions
   Max := 0; -- number of solutions for Sum (so far, none)
   Current := Map.Ceiling(0); -- first element in Map with Sum >= 0
   while Has_Element(Current) loop
      if Element(Current) > Max then
	 Max := Element(Current); -- the maximum of solutions, so far
	 Sum := Key(Current);     -- the Sum with Max solutions
      end if;
      Next(Current);
   end loop;
   Put_Line("Most frequent result:" & Integer'Image(Sum));
   Put_Line("Frequency of" & Integer'Image(Sum) & ":" &
	      Integer'Image(Max));
   New_Line;

   -- find smallest Sum >= 0 with no solution
   Sum := 0;
   while Map.Find(Sum) /= No_Element loop
      Sum := Sum + 1;
   end loop;
   Put_Line("Smallest nonnegative impossible sum:" & Integer'Image(Sum));
   New_Line;

   -- find ten highest numbers with a solution
   Current := Map.Last; -- highest element in Map with a solution
   Put_Line("Highest sum:" & Integer'Image(Key(Current)));
   Put("Next nine:");
   for I in 1 .. 9 loop -- 9 steps backward
      Previous(Current);
      Put(Integer'Image(Key(Current)));
   end loop;
   New_Line;
end Three_others;
```


```txt
Most frequent result: 9
Frequency of 9: 46

Smallest nonnegative impossible sum: 211

Highest sum: 123456789
Next nine: 23456790 23456788 12345687 12345669 3456801 3456792 3456790 3456788 3456786
```



## Aime


```aime
integer b, i, j, k, l, p, s, z;
index r, w;

i = 0;
while (i < 512) {
    b = i.bcount;
    j = 0;
    while (j < 1 << b) {
        data e;

        j += 1;

        k = s = p = 0;
        l = j;
        z = 1;
        while (k < 9) {
            if (i & 1 << k) {
                e.append("-+"[l & 1]);
                s += p * z;
                z = (l & 1) * 2 - 1;
                l >>= 1;
                p = 0;
            }
            e.append('1' + k);
            p = p * 10 + 1 + k;

            k += 1;
        }

        s += p * z;

        if (e[0] != '+') {
            if (s == 100) {
                o_(e, "\n");
            }

            w[s] += 1;
        }
    }

    i += 1;
}

w.wcall(i_fix, 1, 1, r);

o_(r.back, "\n");

k = 0;
for (+k in w) {
    if (!w.key(k + 1)) {
        o_(k + 1, "\n");
        break;
    }
}

i = 10;
for (k of w) {
    o_(k, "\n");
    if (!(i -= 1)) {
        break;
    }
}
```

```txt
123-45-67+89
123+4-5+67-89
12+3+4+5-6-7+89
12-3-4+5-6+7+89
1+23-4+5+6+78-9
1+2+3-4+5+6+78+9
-1+2-3+4+5+6+78+9
123+45-67+8-9
1+2+34-5+67-8+9
12+3-4+5+67+8+9
1+23-4+56+7+8+9
123-4-5-6-7+8-9
9
211
123456789
23456790
23456788
12345687
12345669
3456801
3456792
3456790
3456788
3456786
```



## ALGOL 68


```algol68
BEGIN
    # find the numbers the string 123456789 ( with "+/-" optionally inserted  #
    # before each digit ) can generate                                        #

    # experimentation shows that the largest hundred numbers that can be      #
    # generated are are greater than or equal to 56795                        #
    # as we can't declare an array with bounds -123456789 : 123456789 in      #
    # Algol 68G, we use -60000 : 60000 and keep counts for the top hundred    #

    INT max number = 60 000;
    [ - max number : max number ]STRING solutions;
    [ - max number : max number ]INT    count;
    FOR i FROM LWB solutions TO UPB solutions DO solutions[ i ] := ""; count[ i ] := 0 OD;

    # calculate the numbers ( up to max number ) we can generate and the strings leading to them  #
    # also determine the largest numbers we can generate #
    [ 100 ]INT largest;
    [ 100 ]INT largest count;
    INT impossible number = - 999 999 999;
    FOR i FROM LWB largest TO UPB largest DO
        largest      [ i ] := impossible number;
        largest count[ i ] := 0
    OD;
    [ 1 : 18 ]CHAR sum string := ".1.2.3.4.5.6.7.8.9";
    []CHAR sign char = []CHAR( "-", " ", "+" )[ AT -1 ];
    # we don't distinguish between strings starting "+1" and starting " 1" #
    FOR s1 FROM -1 TO 0 DO
        sum string[  1 ] := sign char[ s1 ];
        FOR s2 FROM -1 TO 1 DO
            sum string[  3 ] := sign char[ s2 ];
            FOR s3 FROM -1 TO 1 DO
                sum string[  5 ] := sign char[ s3 ];
                FOR s4 FROM -1 TO 1 DO
                    sum string[  7 ] := sign char[ s4 ];
                    FOR s5 FROM -1 TO 1 DO
                        sum string[  9 ] := sign char[ s5 ];
                        FOR s6 FROM -1 TO 1 DO
                            sum string[ 11 ] := sign char[ s6 ];
                            FOR s7 FROM -1 TO 1 DO
                                sum string[ 13 ] := sign char[ s7 ];
                                FOR s8 FROM -1 TO 1 DO
                                    sum string[ 15 ] := sign char[ s8 ];
                                    FOR s9 FROM -1 TO 1 DO
                                        sum string[ 17 ] := sign char[ s9 ];
                                        INT number := 0;
                                        INT part   := IF s1 < 0 THEN -1 ELSE 1 FI;
                                        IF s2 = 0 THEN part *:= 10 +:= 2 * SIGN part ELSE number +:= part; part := 2 * s2 FI;
                                        IF s3 = 0 THEN part *:= 10 +:= 3 * SIGN part ELSE number +:= part; part := 3 * s3 FI;
                                        IF s4 = 0 THEN part *:= 10 +:= 4 * SIGN part ELSE number +:= part; part := 4 * s4 FI;
                                        IF s5 = 0 THEN part *:= 10 +:= 5 * SIGN part ELSE number +:= part; part := 5 * s5 FI;
                                        IF s6 = 0 THEN part *:= 10 +:= 6 * SIGN part ELSE number +:= part; part := 6 * s6 FI;
                                        IF s7 = 0 THEN part *:= 10 +:= 7 * SIGN part ELSE number +:= part; part := 7 * s7 FI;
                                        IF s8 = 0 THEN part *:= 10 +:= 8 * SIGN part ELSE number +:= part; part := 8 * s8 FI;
                                        IF s9 = 0 THEN part *:= 10 +:= 9 * SIGN part ELSE number +:= part; part := 9 * s9 FI;
                                        number +:= part;
                                        IF  number >= LWB solutions
                                        AND number <= UPB solutions
                                        THEN
                                            solutions[ number ] +:= ";" + sum string;
                                            count    [ number ] +:= 1
                                        FI;
                                        BOOL inserted := FALSE;
                                        FOR l pos FROM LWB largest TO UPB largest WHILE NOT inserted DO
                                            IF number > largest[ l pos ] THEN
                                                # found a new larger number #
                                                FOR m pos FROM UPB largest BY -1 TO l pos + 1 DO
                                                    largest      [ m pos ] := largest      [ m pos - 1 ];
                                                    largest count[ m pos ] := largest count[ m pos - 1 ]
                                                OD;
                                                largest      [ l pos ] := number;
                                                largest count[ l pos ] := 1;
                                                inserted := TRUE
                                            ELIF number = largest[ l pos ] THEN
                                                # have another way of generating this number #
                                                largest count[ l pos ] +:= 1;
                                                inserted := TRUE
                                            FI
                                        OD
                                    OD
                                OD
                            OD
                        OD
                    OD
                OD
            OD
        OD
    OD;

    # show the solutions for 100 #
    print( ( "100 has ", whole( count[ 100 ], 0 ), " solutions:" ) );
    STRING s := solutions[ 100 ];
    FOR s pos FROM LWB s TO UPB s DO
        IF   s[ s pos ] = ";" THEN print( ( newline, "        " ) )
        ELIF s[ s pos ] /= " " THEN print( ( s[ s pos ] ) )
        FI
    OD;
    print( ( newline ) );
    # find the number with the most solutions #
    INT max solutions := 0;
    INT number with max := LWB count - 1;
    FOR n FROM 0 TO max number DO
        IF count[ n ] > max solutions THEN
            max solutions := count[ n ];
            number with max := n
        FI
    OD;
    FOR n FROM LWB largest count TO UPB largest count DO
        IF largest count[ n ] > max solutions THEN
            max solutions := largest count[ n ];
            number with max := largest[ n ]
        FI
    OD;
    print( ( whole( number with max, 0 ), " has the maximum number of solutions: ", whole( max solutions, 0 ), newline ) );
    # find the smallest positive number that has no solutions #
    BOOL have solutions := TRUE;
    FOR n FROM 0 TO max number
    WHILE IF NOT ( have solutions := count[ n ] > 0 )
          THEN print( ( whole( n, 0 ), " is the lowest positive number with no solutions", newline ) )
          FI;
          have solutions
    DO SKIP OD;
    IF have solutions
    THEN print( ( "All positive numbers up to ", whole( max number, 0 ), " have solutions", newline ) )
    FI;
    print( ( "The 10 largest numbers that can be generated are:", newline ) );
    FOR t pos FROM 1 TO 10 DO
        print( ( " ", whole( largest[ t pos ], 0 ) ) )
    OD;
    print( ( newline ) )

END
```

```txt

100 has 12 solutions:
        -1+2-3+4+5+6+78+9
        12-3-4+5-6+7+89
        123-4-5-6-7+8-9
        123-45-67+89
        123+4-5+67-89
        123+45-67+8-9
        12+3-4+5+67+8+9
        12+3+4+5-6-7+89
        1+23-4+56+7+8+9
        1+23-4+5+6+78-9
        1+2+3-4+5+6+78+9
        1+2+34-5+67-8+9
9 has the maximum number of solutions: 46
211 is the lowest positive number with no solutions
The 10 largest numbers that can be generated are:
 123456789 23456790 23456788 12345687 12345669 3456801 3456792 3456790 3456788 3456786

```



## AppleScript

AppleScript is essentially out of its depth at this scale. The first task (number of distinct paths to 100) is accessible within a few seconds. Subsequent tasks, however, terminate only (if at all) after impractical amounts of time. Note the contrast with the lighter and more optimised JavaScript interpreter, which takes less than half a second to return full results for all the listed tasks.

```AppleScript
use framework "Foundation" -- for basic NSArray sort

property pSigns : {1, 0, -1} --> ( + | unsigned | - )
property plst100 : {"Sums to 100:", ""}
property plstSums : {}
property plstSumsSorted : missing value
property plstSumGroups : missing value

-- data Sign :: [ 1 | 0 | -1 ] = ( Plus | Unsigned | Minus )
-- asSum :: [Sign] -> Int
on asSum(xs)
    script
        on |λ|(a, sign, i)
            if sign ≠ 0 then
                {digits:{}, n:(n of a) + (sign * ((i & digits of a) as string as integer))}
            else
                {digits:{i} & (digits of a), n:n of a}
            end if
        end |λ|
    end script

    set rec to foldr(result, {digits:{}, n:0}, xs)
    set ds to digits of rec
    if length of ds > 0 then
        (n of rec) + (ds as string as integer)
    else
        n of rec
    end if
end asSum

-- data Sign :: [ 1 | 0 | -1 ] = ( Plus | Unisigned | Minus )
-- asString :: [Sign] -> String
on asString(xs)
    script
        on |λ|(a, sign, i)
            set d to i as string
            if sign ≠ 0 then
                if sign > 0 then
                    a & " +" & d
                else
                    a & " -" & d
                end if
            else
                a & d
            end if
        end |λ|
    end script

    foldl(result, "", xs)
end asString

-- sumsTo100 :: () -> String
on sumsTo100()
    -- From first permutation without leading '+' (3 ^ 8) to end of universe (3 ^ 9)
    repeat with i from 6561 to 19683
        set xs to nthPermutationWithRepn(pSigns, 9, i)
        if asSum(xs) = 100 then set end of plst100 to asString(xs)
    end repeat
    intercalate(linefeed, plst100)
end sumsTo100


-- mostCommonSum :: () -> String
on mostCommonSum()
    -- From first permutation without leading '+' (3 ^ 8) to end of universe (3 ^ 9)
    repeat with i from 6561 to 19683
        set intSum to asSum(nthPermutationWithRepn(pSigns, 9, i))
        if intSum ≥ 0 then set end of plstSums to intSum
    end repeat

    set plstSumsSorted to sort(plstSums)
    set plstSumGroups to group(plstSumsSorted)

    script groupLength
        on |λ|(a, b)
            set intA to length of a
            set intB to length of b
            if intA < intB then
                -1
            else if intA > intB then
                1
            else
                0
            end if
        end |λ|
    end script

    set lstMaxSum to maximumBy(groupLength, plstSumGroups)
    intercalate(linefeed, ¬
        {"Most common sum: " & item 1 of lstMaxSum, ¬
            "Number of instances: " & length of lstMaxSum})
end mostCommonSum


-- TEST ----------------------------------------------------------------------
on run
    return sumsTo100()

    -- Also returns a value, but slow:
    -- mostCommonSum()
end run


-- GENERIC FUNCTIONS ---------------------------------------------------------

-- nthPermutationWithRepn :: [a] -> Int -> Int -> [a]
on nthPermutationWithRepn(xs, groupSize, iIndex)
    set intBase to length of xs
    set intSetSize to intBase ^ groupSize

    if intBase < 1 or iIndex > intSetSize then
        {}
    else
        set baseElems to inBaseElements(xs, iIndex)
        set intZeros to groupSize - (length of baseElems)

        if intZeros > 0 then
            replicate(intZeros, item 1 of xs) & baseElems
        else
            baseElems
        end if
    end if
end nthPermutationWithRepn

-- inBaseElements :: [a] -> Int -> [String]
on inBaseElements(xs, n)
    set intBase to length of xs

    script nextDigit
        on |λ|(residue)
            set {divided, remainder} to quotRem(residue, intBase)

            {valid:divided > 0, value:(item (remainder + 1) of xs), new:divided}
        end |λ|
    end script

    reverse of unfoldr(nextDigit, n)
end inBaseElements

-- sort :: [a] -> [a]
on sort(lst)
    ((current application's NSArray's arrayWithArray:lst)'s ¬
        sortedArrayUsingSelector:"compare:") as list
end sort

-- maximumBy :: (a -> a -> Ordering) -> [a] -> a
on maximumBy(f, xs)
    set cmp to mReturn(f)
    script max
        on |λ|(a, b)
            if a is missing value or cmp's |λ|(a, b) < 0 then
                b
            else
                a
            end if
        end |λ|
    end script

    foldl(max, missing value, xs)
end maximumBy

-- group :: Eq a => [a] -> [[a]]
on group(xs)
    script eq
        on |λ|(a, b)
            a = b
        end |λ|
    end script

    groupBy(eq, xs)
end group

-- groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
on groupBy(f, xs)
    set mf to mReturn(f)

    script enGroup
        on |λ|(a, x)
            if length of (active of a) > 0 then
                set h to item 1 of active of a
            else
                set h to missing value
            end if

            if h is not missing value and mf's |λ|(h, x) then
                {active:(active of a) & x, sofar:sofar of a}
            else
                {active:{x}, sofar:(sofar of a) & {active of a}}
            end if
        end |λ|
    end script

    if length of xs > 0 then
        set dct to foldl(enGroup, {active:{item 1 of xs}, sofar:{}}, tail(xs))
        if length of (active of dct) > 0 then
            sofar of dct & {active of dct}
        else
            sofar of dct
        end if
    else
        {}
    end if
end groupBy

-- tail :: [a] -> [a]
on tail(xs)
    if length of xs > 1 then
        items 2 thru -1 of xs
    else
        {}
    end if
end tail


-- intercalate :: Text -> [Text] -> Text
on intercalate(strText, lstText)
    set {dlm, my text item delimiters} to {my text item delimiters, strText}
    set strJoined to lstText as text
    set my text item delimiters to dlm
    return strJoined
end intercalate

--  quotRem :: Integral a => a -> a -> (a, a)
on quotRem(m, n)
    {m div n, m mod n}
end quotRem

-- replicate :: Int -> a -> [a]
on replicate(n, a)
    set out to {}
    if n < 1 then return out
    set dbl to {a}

    repeat while (n > 1)
        if (n mod 2) > 0 then set out to out & dbl
        set n to (n div 2)
        set dbl to (dbl & dbl)
    end repeat
    return out & dbl
end replicate

-- foldr :: (a -> b -> a) -> a -> [b] -> a
on foldr(f, startValue, xs)
    tell mReturn(f)
        set v to startValue
        set lng to length of xs
        repeat with i from lng to 1 by -1
            set v to |λ|(v, item i of xs, i, xs)
        end repeat
        return v
    end tell
end foldr

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

-- unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
on unfoldr(f, v)
    set mf to mReturn(f)
    set lst to {}
    set recM to mf's |λ|(v)
    repeat while (valid of recM) is true
        set end of lst to value of recM
        set recM to mf's |λ|(new of recM)
    end repeat
    lst & value of recM
end unfoldr

-- until :: (a -> Bool) -> (a -> a) -> a -> a
on |until|(p, f, x)
    set mp to mReturn(p)
    set v to x

    tell mReturn(f)
        repeat until mp's |λ|(v)
            set v to |λ|(v)
        end repeat
    end tell
    return v
end |until|

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
Sums to 100:

1 +2 +34 -5 +67 -8 +9
1 +2 +3 -4 +5 +6 +78 +9
1 +23 -4 +5 +6 +78 -9
1 +23 -4 +56 +7 +8 +9
12 +3 +4 +5 -6 -7 +89
12 +3 -4 +5 +67 +8 +9
123 +45 -67 +8 -9
123 +4 -5 +67 -89
123 -45 -67 +89
123 -4 -5 -6 -7 +8 -9
12 -3 -4 +5 -6 +7 +89
 -1 +2 -3 +4 +5 +6 +78 +9
```



## AutoHotkey

{{incomplete|AutoHotkey|

 The output is incomplete, please address the 2<sup>nd</sup> and 3<sup>rd</sup> task requirements.

}}

Inspired by https://autohotkey.com/board/topic/149914-five-challenges-to-do-in-an-hour/

```AutoHotkey
global Matches:=[]
AllPossibilities100()
for eq, val in matches
	res .= eq "`n"
MsgBox % res
return

AllPossibilities100(n:=0,  S:="") {
	if (n = 0)							; First call
		AllPossibilities100(n+1, n)				; Recurse
	else if (n < 10){
		AllPossibilities100(n+1, 	S ",-" n)		; Recurse. Concatenate S, ",-" and n
		AllPossibilities100(n+1, 	S ",+" n)		; Recurse. Concatenate S, ",+" and n
		AllPossibilities100(n+1, 	S n)			; Recurse. Concatenate S and n
	} else 	{							; 10th level recursion
		Loop, Parse, S, CSV					; Total the values of S and check if equal to 100
		{
			SubS := SubStr(A_LoopField, 2)			; The number portion of A_LoopField
			if (A_Index = 1)
				Total := A_LoopField
			else if (SubStr(A_LoopField, 1, 1) = "+")	; If the first character is + add
				Total += SubS
			else						; else subtract
				Total -= SubS
		}
		if (Total = 100)
			matches[LTrim(LTrim(StrReplace(S, ","), "0"),"+")] := true ; remove leading 0's, +'s and all commas
	}
}
```

Outputs:
```txt
-1+2-3+4+5+6+78+9
1+2+3-4+5+6+78+9
1+2+34-5+67-8+9
1+23-4+5+6+78-9
1+23-4+56+7+8+9
12+3+4+5-6-7+89
12+3-4+5+67+8+9
12-3-4+5-6+7+89
123+4-5+67-89
123+45-67+8-9
123-4-5-6-7+8-9
123-45-67+89
```


## AWK

Awk is a weird language: there are no integers, no switch-case (in the standard language version), programs are controlled by data flow, the interpreter speed is moderate. The advantage of Awk are associative arrays, used here for counting how many times we get the same sum as the result of calculations.

```AWK
#
# RossetaCode: Sum to 100, AWK.
#
# Find solutions to the "sum to one hundred" puzzle.

function evaluate(code)
{
    value  = 0
    number = 0
    power  = 1
    for ( k = 9; k >= 1; k-- )
    {
        number = power*k + number
        op = code % 3
        if ( op == 0 ) {
            value = value + number
            number = 0
            power = 1
        } else if (op == 1 ) {
            value = value - number
            number = 0
            power = 1
        } else if ( op == 2) {
            power = power * 10
        } else {
        }
        code = int(code / 3);
    }
    return value;
}

function show(code)
{
    s = ""
    a = 19683
    b = 6561

    for ( k = 1; k <= 9; k++ )
    {
        op = int( (code % a) / b )
        if ( op == 0 && k > 1 )
            s = s "+"
        else if ( op == 1 )
            s = s "-"
        else {
        }
        a = b
        b = int(b / 3)
        s = s  k
    }
    printf "%9d = %s\n", evaluate(code), s;
}


BEGIN {
    nexpr = 13122

    print
    print "Show all solutions that sum to 100"
    print
    for ( i = 0; i < nexpr; i++ ) if ( evaluate(i) == 100 ) show(i);

    print
    print "Show the sum that has the maximum number of solutions"
    print
    for ( i = 0; i < nexpr; i++ ) {
        sum = evaluate(i);
        if ( sum >= 0 )
            stat[sum]++;
    }
    best = (-1);
    for ( sum in stat )
        if ( best < stat[sum] ) {
            best = stat[sum]
            bestSum = sum
        }
    delete stat
    printf "%d has %d solutions\n", bestSum, best

    print
    print "Show the lowest positive number that can't be expressed"
    print
    for ( i = 0; i <= 123456789; i++ ){
        for ( j = 0; j < nexpr; j++ )
            if ( i == evaluate(j) )
                break;
        if ( i != evaluate(j) )
            break;
    }
    printf "%d\n",i

    print
    print "Show the ten highest numbers that can be expressed"
    print
    limit = 123456789 + 1;
    for ( i = 1; i <= 10; i++ )
    {
        best = 0;
        for ( j = 0; j < nexpr; j++ )
        {
            test = evaluate(j);
            if ( test < limit && test > best ) best = test;
        }
        for ( j = 0; j < nexpr; j++ ) if ( evaluate(j) == best ) show(j)
        limit = best
    }
}
```

```txt
Show all solutions that sum to 100

      100 = 1+2+3-4+5+6+78+9
      100 = 1+2+34-5+67-8+9
      100 = 1+23-4+5+6+78-9
      100 = 1+23-4+56+7+8+9
      100 = 12+3+4+5-6-7+89
      100 = 12+3-4+5+67+8+9
      100 = 12-3-4+5-6+7+89
      100 = 123+4-5+67-89
      100 = 123+45-67+8-9
      100 = 123-4-5-6-7+8-9
      100 = 123-45-67+89
      100 = -1+2-3+4+5+6+78+9

Show the sum that has the maximum number of solutions

9 has 46 solutions

Show the lowest positive number that can't be expressed

211

Show the ten highest numbers that can be expressed

123456789 = 123456789
 23456790 = 1+23456789
 23456788 = -1+23456789
 12345687 = 12345678+9
 12345669 = 12345678-9
  3456801 = 12+3456789
  3456792 = 1+2+3456789
  3456790 = -1+2+3456789
  3456788 = 1-2+3456789
  3456786 = -1-2+3456789
```



## C


###  Optimized for speed

Warning: '''this version requires at least four byte integers.'''

```C
/*
 * RossetaCode: Sum to 100, C99, an algorithm using ternary numbers.
 *
 * Find solutions to the "sum to one hundred" puzzle.
 */

#include <stdio.h>
#include <stdlib.h>

/*
 * There are only 13122 (i.e. 2*3**8) different possible expressions,
 * thus we can encode them as positive integer numbers from 0 to 13121.
 */
#define NUMBER_OF_EXPRESSIONS (2 * 3*3*3*3 * 3*3*3*3 )
enum OP { ADD, SUB, JOIN };
typedef int (*cmp)(const void*, const void*);

// Replacing struct Expression and struct CountSum by a tuple like
// struct Pair { int first; int last; } is possible but would make the source
// code less readable.

struct Expression{
    int sum;
    int code;
}expressions[NUMBER_OF_EXPRESSIONS];
int expressionsLength = 0;
int compareExpressionBySum(const struct Expression* a, const struct Expression* b){
    return a->sum - b->sum;
}

struct CountSum{
    int counts;
    int sum;
}countSums[NUMBER_OF_EXPRESSIONS];
int countSumsLength = 0;
int compareCountSumsByCount(const struct CountSum* a, const struct CountSum* b){
    return a->counts - b->counts;
}

int evaluate(int code){
    int value  = 0, number = 0, power  = 1;
    for ( int k = 9; k >= 1; k-- ){
        number = power*k + number;
        switch( code % 3 ){
            case ADD:  value = value + number; number = 0; power = 1; break;
            case SUB:  value = value - number; number = 0; power = 1; break;
            case JOIN: power = power * 10                ; break;
        }
        code /= 3;
    }
    return value;
}

void print(int code){
    static char s[19]; char* p = s;
    int a = 19683, b = 6561;
    for ( int k = 1; k <= 9; k++ ){
        switch((code % a) / b){
            case ADD: if ( k > 1 ) *p++ = '+'; break;
            case SUB:              *p++ = '-'; break;
        }
        a = b;
        b = b / 3;
        *p++ = '0' + k;
    }
    *p = 0;
    printf("%9d = %s\n", evaluate(code), s);
}

void comment(char* string){
    printf("\n\n%s\n\n", string);
}

void init(void){
    for ( int i = 0; i < NUMBER_OF_EXPRESSIONS; i++ ){
        expressions[i].sum = evaluate(i);
        expressions[i].code = i;
    }
    expressionsLength = NUMBER_OF_EXPRESSIONS;
    qsort(expressions,expressionsLength,sizeof(struct Expression),(cmp)compareExpressionBySum);

    int j = 0;
    countSums[0].counts = 1;
    countSums[0].sum = expressions[0].sum;
    for ( int i = 0; i < expressionsLength; i++ ){
        if ( countSums[j].sum != expressions[i].sum ){
            j++;
            countSums[j].counts = 1;
            countSums[j].sum = expressions[i].sum;
        }
        else
            countSums[j].counts++;
    }
    countSumsLength = j + 1;
    qsort(countSums,countSumsLength,sizeof(struct CountSum),(cmp)compareCountSumsByCount);
}

int main(void){

    init();

    comment("Show all solutions that sum to 100");
    const int givenSum = 100;
    struct Expression ex = { givenSum, 0 };
    struct Expression* found;
    if ( found = bsearch(&ex,expressions,expressionsLength,
        sizeof(struct Expression),(cmp)compareExpressionBySum) ){
        while ( found != expressions && (found-1)->sum == givenSum )
            found--;
        while ( found != &expressions[expressionsLength] && found->sum == givenSum )
            print(found++->code);
    }

    comment("Show the positve sum that has the maximum number of solutions");
    int maxSumIndex = countSumsLength - 1;
    while( countSums[maxSumIndex].sum < 0 )
        maxSumIndex--;
    printf("%d has %d solutions\n",
        countSums[maxSumIndex].sum, countSums[maxSumIndex].counts);

    comment("Show the lowest positive number that can't be expressed");
    for ( int value = 0; ; value++ ){
        struct Expression ex = { value, 0 };
        if (!bsearch(&ex,expressions,expressionsLength,
                sizeof(struct Expression),(cmp)compareExpressionBySum)){
            printf("%d\n", value);
            break;
        }
    }

    comment("Show the ten highest numbers that can be expressed");
    for ( int i = expressionsLength-1; i >= expressionsLength-10; i-- )
        print(expressions[i].code);

    return 0;
}
```

```txt

Show all solutions that sum to 100

      100 = 123+4-5+67-89
      100 = 123-4-5-6-7+8-9
      100 = 123-45-67+89
      100 = 1+2+34-5+67-8+9
      100 = 123+45-67+8-9
      100 = 1+2+3-4+5+6+78+9
      100 = 1+23-4+5+6+78-9
      100 = 12-3-4+5-6+7+89
      100 = 12+3+4+5-6-7+89
      100 = -1+2-3+4+5+6+78+9
      100 = 12+3-4+5+67+8+9
      100 = 1+23-4+56+7+8+9


Show the positve sum that has the maximum number of solutions

9 has 46 solutions


Show the lowest positive number that can't be expressed

211


Show the ten highest numbers that can be expressed

123456789 = 123456789
 23456790 = 1+23456789
 23456788 = -1+23456789
 12345687 = 12345678+9
 12345669 = 12345678-9
  3456801 = 12+3456789
  3456792 = 1+2+3456789
  3456790 = -1+2+3456789
  3456788 = 1-2+3456789
  3456786 = -1-2+3456789

```



### Optimized for memory consumption

Warning: '''this program needs at least four byte integers'''.

```C
/*
 * RossetaCode: Sum to 100, C11, MCU friendly.
 *
 * Find solutions to the "sum to one hundred" puzzle.
 *
 * We optimize algorithms for size. Therefore we don't use arrays, but recompute
 * all values again and again. It is a little surprise that the time efficiency
 * is quite acceptable.
 */

#include <stdio.h>

enum OP { ADD, SUB, JOIN };

int evaluate(int code){
    int value  = 0, number = 0, power  = 1;
    for ( int k = 9; k >= 1; k-- ){
        number = power*k + number;
        switch( code % 3 ){
            case ADD:  value = value + number; number = 0; power = 1; break;
            case SUB:  value = value - number; number = 0; power = 1; break;
            case JOIN: power = power * 10                           ; break;
        }
        code /= 3;
    }
    return value;
}

void print(int code){
    static char s[19]; char* p = s;
    int a = 19683, b = 6561;
    for ( int k = 1; k <= 9; k++ ){
        switch((code % a) / b){
            case ADD: if ( k > 1 ) *p++ = '+'; break;
            case SUB:              *p++ = '-'; break;
        }
        a = b;
        b = b / 3;
        *p++ = '0' + k;
    }
    *p = 0;
    printf("%9d = %s\n", evaluate(code), s);
}

int main(void){

    int i,j;
    const int nexpr = 13122;
#define LOOP(K) for (K = 0; K < nexpr; K++)

    puts("\nShow all solutions that sum to 100\n");
    LOOP(i) if ( evaluate(i) == 100 ) print(i);

    puts("\nShow the sum that has the maximum number of solutions\n");
    int best, nbest = (-1);
    LOOP(i){
        int test = evaluate(i);
        if ( test > 0 ){
            int ntest = 0;
            LOOP(j) if ( evaluate(j) == test ) ntest++;
            if ( ntest > nbest ){ best = test; nbest = ntest; }
        }
    }
    printf("%d has %d solutions\n", best,nbest);

    puts("\nShow the lowest positive number that can't be expressed\n");
    for ( i = 0; i <= 123456789; i++ ){
        LOOP(j) if ( i == evaluate(j) ) break;
        if ( i != evaluate(j) ) break;
    }
    printf("%d\n",i);

    puts("\nShow the ten highest numbers that can be expressed\n");
    int limit = 123456789 + 1;
    for ( i = 1; i <= 10; i++ ) {
        int best = 0;
        LOOP(j){
            int test = evaluate(j);
            if ( test < limit && test > best ) best = test;
        }
        LOOP(j) if ( evaluate(j) == best ) print(j);
        limit = best;
    }

    return 0;
}
```

```txt
Show all solutions that sum to 100

      100 = 1+2+3-4+5+6+78+9
      100 = 1+2+34-5+67-8+9
      100 = 1+23-4+5+6+78-9
      100 = 1+23-4+56+7+8+9
      100 = 12+3+4+5-6-7+89
      100 = 12+3-4+5+67+8+9
      100 = 12-3-4+5-6+7+89
      100 = 123+4-5+67-89
      100 = 123+45-67+8-9
      100 = 123-4-5-6-7+8-9
      100 = 123-45-67+89
      100 = -1+2-3+4+5+6+78+9

Show the sum that has the maximum number of solutions

9 has 46 solutions

Show the lowest positive number that can't be expressed

211

Show the ten highest numbers that can be expressed

123456789 = 123456789
 23456790 = 1+23456789
 23456788 = -1+23456789
 12345687 = 12345678+9
 12345669 = 12345678-9
  3456801 = 12+3456789
  3456792 = 1+2+3456789
  3456790 = -1+2+3456789
  3456788 = 1-2+3456789
  3456786 = -1-2+3456789

```



## C++

For each expression of sum s, there is at least one expression whose sum is -s. If the sum s can be represented by n expressions, the sum -s can also be represented by n expressions. The change of all signs in an expression change the sign of the sum of this expression. For example, -1+23-456+789 has the opposite sign than +1-23+456-789. Therefore only the positive sum with the maximum number of solutions is shown. The program does not check uniqueness of this sum. We can easily check (modifying the program) that: sum 9 has 46 solutions; sum -9 has 46 solutions; any other sum has less than 46 solutions.

```Cpp
/*
 * RossetaCode: Sum to 100, C++, STL, OOP.
 * Works with: MSC 16.0 (MSVS2010); GCC 5.1 (use -std=c++11 or -std=c++14 etc.).
 *
 * Find solutions to the "sum to one hundred" puzzle.
 */
#include <iostream>
#include <iomanip>
#include <algorithm>
#include <string>
#include <set>
#include <map>

using namespace std;

class Expression{
    private:
        enum { NUMBER_OF_DIGITS = 9 }; // hack for C++98, use const int in C++11
        enum Op { ADD, SUB, JOIN };
        int code[NUMBER_OF_DIGITS];
    public:
        static const int NUMBER_OF_EXPRESSIONS;
        Expression(){
            for ( int i = 0; i < NUMBER_OF_DIGITS; i++ )
                code[i] = ADD;
        }
        Expression& operator++(int){ // post incrementation
            for ( int i = 0; i < NUMBER_OF_DIGITS; i++ )
                if ( ++code[i] > JOIN ) code[i] = ADD;
                else break;
            return *this;
        }
        operator int() const{
            int value = 0, number = 0, sign = (+1);
            for ( int digit = 1; digit <= 9; digit++ )
                switch ( code[NUMBER_OF_DIGITS - digit] ){
                case ADD: value += sign*number; number = digit; sign = (+1); break;
                case SUB: value += sign*number; number = digit; sign = (-1); break;
                case JOIN:                      number = 10*number + digit;  break;
            }
            return value + sign*number;
        }
        operator string() const{
            string s;
            for ( int digit = 1; digit <= NUMBER_OF_DIGITS; digit++ ){
                switch( code[NUMBER_OF_DIGITS - digit] ){
                    case ADD: if ( digit > 1 ) s.push_back('+'); break;
                    case SUB:                  s.push_back('-'); break;
                }
                s.push_back('0' + digit);
            }
            return s;
        }
};
const int Expression::NUMBER_OF_EXPRESSIONS = 2 * 3*3*3*3 * 3*3*3*3;

ostream& operator<< (ostream& os, Expression& ex){
    ios::fmtflags oldFlags(os.flags());
    os << setw(9) << right << static_cast<int>(ex)    << " = "
       << setw(0) << left  << static_cast<string>(ex) << endl;
    os.flags(oldFlags);
    return os;
}

struct Stat{
    map<int,int> countSum;
    map<int, set<int> > sumCount;
    Stat(){
        Expression expression;
        for ( int i = 0; i < Expression::NUMBER_OF_EXPRESSIONS; i++, expression++ )
            countSum[expression]++;
        for ( auto it = countSum.begin(); it != countSum.end(); it++ )
            sumCount[it->second].insert(it->first);
    }
};

void print(int givenSum){
    Expression expression;
    for ( int i = 0; i < Expression::NUMBER_OF_EXPRESSIONS; i++, expression++ )
        if ( expression == givenSum )
            cout << expression;
}

void comment(string commentString){
    cout << endl << commentString << endl << endl;
}

int main(){
    Stat stat;

    comment( "Show all solutions that sum to 100" );
    const int givenSum = 100;
    print(givenSum);

    comment( "Show the sum that has the maximum number of solutions" );
    auto maxi = max_element(stat.sumCount.begin(),stat.sumCount.end());
    auto it = maxi->second.begin();
    while ( *it < 0 ) it++;
    cout << static_cast<int>(*it) << " has " << maxi->first << " solutions" << endl;

    comment( "Show the lowest positive number that can't be expressed" );
    int value = 0;
    while(stat.countSum.count(value) != 0) value++;
    cout << value << endl;

    comment( "Show the ten highest numbers that can be expressed" );
    auto rit = stat.countSum.rbegin();
    for ( int i = 0; i < 10; i++, rit++ ) print(rit->first);

    return 0;
}
```

```txt

Show all solutions that sum to 100

      100 = 1+2+3-4+5+6+78+9
      100 = 1+2+34-5+67-8+9
      100 = 1+23-4+5+6+78-9
      100 = 1+23-4+56+7+8+9
      100 = 12+3+4+5-6-7+89
      100 = 12+3-4+5+67+8+9
      100 = 12-3-4+5-6+7+89
      100 = 123+4-5+67-89
      100 = 123+45-67+8-9
      100 = 123-4-5-6-7+8-9
      100 = 123-45-67+89
      100 = -1+2-3+4+5+6+78+9

Show the sum that has the maximum number of solutions

9 has 46 solutions

Show the lowest positive number that can't be expressed

211

Show the ten highest numbers that can be expressed

123456789 = 123456789
 23456790 = 1+23456789
 23456788 = -1+23456789
 12345687 = 12345678+9
 12345669 = 12345678-9
  3456801 = 12+3456789
  3456792 = 1+2+3456789
  3456790 = -1+2+3456789
  3456788 = 1-2+3456789
  3456786 = -1-2+3456789

```


## C#

```c#
using System;
using System.Collections.Generic;
using System.Linq;

class Program
{
    static void Main(string[] args)
    {
        // All unique expressions that have a plus sign in front of the 1; calculated in parallel
        var expressionsPlus = Enumerable.Range(0, (int)Math.Pow(3, 8)).AsParallel().Select(i => new Expression(i, 1));
        // All unique expressions that have a minus sign in front of the 1; calculated in parallel
        var expressionsMinus = Enumerable.Range(0, (int)Math.Pow(3, 8)).AsParallel().Select(i => new Expression(i, -1));
        var expressions = expressionsPlus.Concat(expressionsMinus);
        var results = new Dictionary<int, List<Expression>>();
        foreach (var e in expressions)
        {
            if (results.Keys.Contains(e.Value))
                results[e.Value].Add(e);
            else
                results[e.Value] = new List<Expression>() { e };
        }
        Console.WriteLine("Show all solutions that sum to 100");
        foreach (Expression e in results[100])
            Console.WriteLine("  " + e);
        Console.WriteLine("Show the sum that has the maximum number of solutions (from zero to infinity)");
        var summary = results.Keys.Select(k => new Tuple<int, int>(k, results[k].Count));
        var maxSols = summary.Aggregate((a, b) => a.Item2 > b.Item2 ? a : b);
        Console.WriteLine("  The sum " + maxSols.Item1 + " has " + maxSols.Item2 + " solutions.");
        Console.WriteLine("Show the lowest positive sum that can't be expressed (has no solutions), using the rules for this task");
        var lowestPositive = Enumerable.Range(1, int.MaxValue).First(x => !results.Keys.Contains(x));
        Console.WriteLine("  " + lowestPositive);
        Console.WriteLine("Show the ten highest numbers that can be expressed using the rules for this task (extra credit)");
        var highest = from k in results.Keys
                      orderby k descending
                      select k;
        foreach (var x in highest.Take(10))
            Console.WriteLine("  " + x);
    }
}
public enum Operations { Plus, Minus, Join };
public class Expression
{
    protected Operations[] Gaps;
    // 123456789 => there are 8 "gaps" between each number
    ///             with 3 possibilities for each gap: plus, minus, or join
    public int Value; // What this expression sums up to
    protected int _one;

    public Expression(int serial, int one)
    {
        _one = one;
        Gaps = new Operations[8];
        // This represents "serial" as a base 3 number, each Gap expression being a base-three digit
        int divisor = 2187; // == Math.Pow(3,7)
        int times;
        for (int i = 0; i < 8; i++)
        {
            times = Math.DivRem(serial, divisor, out serial);
            divisor /= 3;
            if (times == 0)
                Gaps[i] = Operations.Join;
            else if (times == 1)
                Gaps[i] = Operations.Minus;
            else
                Gaps[i] = Operations.Plus;
        }
        // go ahead and calculate the value of this expression
        // because this is going to be done in a parallel thread (save time)
        Value = Evaluate();
    }
    public override string ToString()
    {
        string ret = _one.ToString();
        for (int i = 0; i < 8; i++)
        {
            switch (Gaps[i])
            {
                case Operations.Plus:
                    ret += "+";
                    break;
                case Operations.Minus:
                    ret += "-";
                    break;
            }
            ret += (i + 2);
        }
        return ret;
    }
    private int Evaluate()
        /* Calculate what this expression equals */
    {
        var numbers = new int[9];
        int nc = 0;
        var operations = new List<Operations>();
        int a = 1;
        for (int i = 0; i < 8; i++)
        {
            if (Gaps[i] == Operations.Join)
                a = a * 10 + (i + 2);
            else
            {
                if (a > 0)
                {
                    if (nc == 0)
                        a *= _one;
                    numbers[nc++] = a;
                    a = i + 2;
                }
                operations.Add(Gaps[i]);
            }
        }
        if (nc == 0)
            a *= _one;
        numbers[nc++] = a;
        int ni = 0;
        int left = numbers[ni++];
        foreach (var operation in operations)
        {
            int right = numbers[ni++];
            if (operation == Operations.Plus)
                left = left + right;
            else
                left = left - right;
        }
        return left;
    }
}
```

```txt

Show all solutions that sum to 100
  123-45-67+89
  123-4-5-6-7+8-9
  123+45-67+8-9
  123+4-5+67-89
  12-3-4+5-6+7+89
  12+3-4+5+67+8+9
  12+3+4+5-6-7+89
  1+23-4+5+6+78-9
  1+23-4+56+7+8+9
  1+2+34-5+67-8+9
  1+2+3-4+5+6+78+9
  -1+2-3+4+5+6+78+9
Show the sum that has the maximum number of solutions (from zero to infinity)
  The sum 9 has 46 solutions.
Show the lowest positive sum that can't be expressed (has no solutions), using the rules for this task
  211
Show the ten highest numbers that can be expressed using the rules for this task (extra credit)
  123456789
  23456790
  23456788
  12345687
  12345669
  3456801
  3456792
  3456790
  3456788
  3456786

```








## Common Lisp


```lisp
(defun f (lst &optional (sum 100) (so-far nil))
 "Takes a list of digits as argument"
  (if (null lst)
    (cond ((= sum 0) (format t "~d = ~{~@d~}~%" (apply #'+ so-far) (reverse so-far)) 1)
          (t 0) )
    (let ((total 0)
          (len (length lst)) )
      (dotimes (i len total)
        (let* ((str1 (butlast lst i))
               (num1 (or (numlist-to-string str1) 0))
               (rem (nthcdr (- len i) lst)) )
          (incf total
            (+ (f rem (- sum num1) (cons num1 so-far))
               (f rem (+ sum num1) (cons (- num1) so-far)) )))))))


(defun numlist-to-string (lst)
 "Convert a list of digits into an integer"
  (when lst
    (parse-integer (format nil "~{~d~}" lst)) ))


```

```txt

>(f '(1 2 3 4 5 6 7 8 9))
100 = +123+45-67+8-9
100 = +123-45-67+89
100 = +123+4-5+67-89
100 = +123-4-5-6-7+8-9
100 = +12+3+4+5-6-7+89
100 = +12+3-4+5+67+8+9
100 = +12-3-4+5-6+7+89
100 = +1+23-4+56+7+8+9
100 = +1+23-4+5+6+78-9
100 = +1+2+34-5+67-8+9
100 = +1+2+3-4+5+6+78+9
100 = -1+2-3+4+5+6+78+9
12

```


The other subtasks are not yet implemented.


## D

```D
import std.stdio;

void main() {
    import std.algorithm : each, max, reduce, sort;
    import std.range : take;

    Stat stat = new Stat();

    comment("Show all solutions that sum to 100");
    immutable givenSum = 100;
    print(givenSum);

    comment("Show the sum that has the maximum number of solutions");
    const int maxCount = reduce!max(stat.sumCount.keys);
    int maxSum;
    foreach(key, entry; stat.sumCount[maxCount]) {
        if (key >= 0) {
            maxSum = key;
            break;
        }
    }
    writeln(maxSum, " has ", maxCount, " solutions");

    comment("Show the lowest positive number that can't be expressed");
    int value = 0;
    while (value in stat.countSum) {
        value++;
    }
    writeln(value);

    comment("Show the ten highest numbers that can be expressed");
    const int n = stat.countSum.keys.length;
    auto sums = stat.countSum.keys;
    sums.sort!"a>b"
        .take(10)
        .each!print;
}

void comment(string commentString) {
    writeln();
    writeln(commentString);
    writeln();
}

void print(int givenSum) {
    Expression expression = new Expression();
    for (int i=0; i<Expression.NUMBER_OF_EXPRESSIONS; i++, expression.next()) {
        if (expression.toInt() == givenSum) {
            expression.print();
        }
    }
}

class Expression {
    private enum NUMBER_OF_DIGITS = 9;
    private enum ADD = 0;
    private enum SUB = 1;
    private enum JOIN = 2;

    enum NUMBER_OF_EXPRESSIONS = 2 * 3 * 3 * 3 * 3 * 3 * 3 * 3 * 3;
    byte[NUMBER_OF_DIGITS] code;

    Expression next() {
        for (int i=0; i<NUMBER_OF_DIGITS; i++) {
            if (++code[i] > JOIN) {
                code[i] = ADD;
            } else {
                break;
            }
        }
        return this;
    }

    int toInt() {
        int value = 0;
        int number = 0;
        int sign = (+1);
        for (int digit=1; digit<=9; digit++) {
            switch (code[NUMBER_OF_DIGITS - digit]) {
                case ADD:
                    value += sign * number;
                    number = digit;
                    sign = (+1);
                    break;
                case SUB:
                    value += sign * number;
                    number = digit;
                    sign = (-1);
                    break;
                case JOIN:
                    number = 10 * number + digit;
                    break;
                default:
                    assert(false);
            }
        }
        return value + sign * number;
    }

    void toString(scope void delegate(const(char)[]) sink) const {
        import std.conv : to;
        import std.format : FormatSpec, formatValue;
        import std.range : put;

        auto fmt = FormatSpec!char("s");
        for (int digit=1; digit<=NUMBER_OF_DIGITS; digit++) {
            switch (code[NUMBER_OF_DIGITS - digit]) {
                case ADD:
                    if (digit > 1) {
                        put(sink, '+');
                    }
                    break;
                case SUB:
                    put(sink, '-');
                    break;
                default:
                    break;
            }
            formatValue(sink, digit, fmt);
        }
    }

    void print() {
        print(stdout);
    }

    void print(File printStream) {
        printStream.writefln("%9d = %s", toInt(), this);
    }
}

class Stat {
    int[int] countSum;
    bool[int][int] sumCount;

    this() {
        Expression expression = new Expression();
        for (int i=0; i<Expression.NUMBER_OF_EXPRESSIONS; i++, expression.next()) {
            int sum = expression.toInt();
            countSum[sum]++;
        }
        foreach (key, entry; countSum) {
            bool[int] set;
            if (entry in sumCount) {
                set = sumCount[entry];
            } else {
                set.clear();
            }
            set[key] = true;
            sumCount[entry] = set;
        }
    }
}
```


```txt
Show all solutions that sum to 100

      100 = 1+2+3-4+5+6+78+9
      100 = 1+2+34-5+67-8+9
      100 = 1+23-4+5+6+78-9
      100 = 1+23-4+56+7+8+9
      100 = 12+3+4+5-6-7+89
      100 = 12+3-4+5+67+8+9
      100 = 12-3-4+5-6+7+89
      100 = 123+4-5+67-89
      100 = 123+45-67+8-9
      100 = 123-4-5-6-7+8-9
      100 = 123-45-67+89
      100 = -1+2-3+4+5+6+78+9

Show the sum that has the maximum number of solutions

9 has 46 solutions

Show the lowest positive number that can't be expressed

211

Show the ten highest numbers that can be expressed

123456789 = 123456789
 23456790 = 1+23456789
 23456788 = -1+23456789
 12345687 = 12345678+9
 12345669 = 12345678-9
  3456801 = 12+3456789
  3456792 = 1+2+3456789
  3456790 = -1+2+3456789
  3456788 = 1-2+3456789
  3456786 = -1-2+3456789
```




## Elixir


```elixir
defmodule Sum do
  def to(val) do
    generate
    |> Enum.map(&{eval(&1), &1})
    |> Enum.filter(fn {v, _s} -> v==val end)
    |> Enum.each(&IO.inspect &1)
  end

  def max_solve do
    generate
    |> Enum.group_by(&eval &1)
    |> Enum.filter_map(fn {k,_} -> k>=0 end, fn {k,v} -> {length(v),k} end)
    |> Enum.max
    |> fn {len,sum} -> IO.puts "sum of #{sum} has the maximum number of solutions : #{len}" end.()
  end

  def min_solve do
    solve = generate |> Enum.group_by(&eval &1)
    Stream.iterate(1, &(&1+1))
    |> Enum.find(fn n -> solve[n]==nil end)
    |> fn sum -> IO.puts "lowest positive sum that can't be expressed : #{sum}" end.()
  end

  def  highest_sums(n\\10) do
    IO.puts "highest sums :"
    generate
    |> Enum.map(&eval &1)
    |> Enum.uniq
    |> Enum.sort_by(fn sum -> -sum end)
    |> Enum.take(n)
    |> IO.inspect
  end

  defp generate do
    x = ["+", "-", ""]
    for a <- ["-", ""], b <- x, c <- x, d <- x, e <- x, f <- x, g <- x, h <- x, i <- x,
        do: "#{a}1#{b}2#{c}3#{d}4#{e}5#{f}6#{g}7#{h}8#{i}9"
  end

  defp eval(str), do: Code.eval_string(str) |> elem(0)
end

Sum.to(100)
Sum.max_solve
Sum.min_solve
Sum.highest_sums
```


```txt

{100, "-1+2-3+4+5+6+78+9"}
{100, "1+2+3-4+5+6+78+9"}
{100, "1+2+34-5+67-8+9"}
{100, "1+23-4+5+6+78-9"}
{100, "1+23-4+56+7+8+9"}
{100, "12+3+4+5-6-7+89"}
{100, "12+3-4+5+67+8+9"}
{100, "12-3-4+5-6+7+89"}
{100, "123+4-5+67-89"}
{100, "123+45-67+8-9"}
{100, "123-4-5-6-7+8-9"}
{100, "123-45-67+89"}
sum of 9 has the maximum number of solutions : 46
lowest positive sum that can't be expressed : 211
highest sums :
[123456789, 23456790, 23456788, 12345687, 12345669, 3456801, 3456792, 3456790,
 3456788, 3456786]

```




## Forth

This solution uses <code>EVALUATE</code> on a string buffer to compute the sum.  Given the copious string manipulations, <code>EVALUATE</code>, and the large byte-array used to keep sum counts, this implementation is optimized neither for speed nor for memory.  On my machine it runs in about 3.8 seconds, compared to the speed-optimized C solution which runs in about 0.005 seconds.

```Forth
CREATE *OPS CHAR + C, CHAR - C, CHAR # C,
CREATE 0OPS CHAR - C, CHAR # C,
CREATE BUFF 43 C, 43 CHARS ALLOT
CREATE PTR CELL ALLOT
CREATE LIMITS 2 C, 3 C, 3 C, 3 C, 3 C, 3 C, 3 C, 3 C, 3 C,
CREATE INDX   0 C, 0 C, 0 C, 0 C, 0 C, 0 C, 0 C, 0 C, 0 C,
CREATE OPS 0OPS , *OPS , *OPS , *OPS , *OPS , *OPS , *OPS , *OPS , *OPS ,
: B0   BUFF 1+ dup  PTR !  43 blank ;
: B, ( c --)  PTR @ C!  1 PTR +! ;
CREATE STATS 123456790 ALLOT  STATS 123456790 ERASE

: inc ( c-addr c-lim u -- t|f)
   1- tuck + >r swap dup rot + ( addr a-addr) ( R: l-addr)
   BEGIN dup C@ 1+ dup r@ C@ =
     IF drop 2dup =
       IF 2drop FALSE rdrop EXIT   \ no inc, contents invalid
       ELSE 0 over C! 1-  r> 1- >r  \ reset and carry
       THEN
     ELSE swap C! drop TRUE rdrop EXIT
     THEN
   AGAIN ;
: INDX+   INDX LIMITS 9 inc 0= ;
: SYNTH   B0  [CHAR] 0 B,  9 0 DO
     INDX I + C@  OPS I CELLS + @ + C@
     dup  [CHAR] # <> IF BL B, B, BL B, ELSE drop THEN
     I [CHAR] 1 + B,
   LOOP  BUFF COUNT ;
: .MOST   cr ." Sum that has the maximum number of solutions" cr 4 spaces
   STATS 0  STATS 1+ 123456789 bounds DO
     dup I c@ <  IF drop drop I I c@ THEN
   LOOP  swap STATS - . ." has " . ." solutions" ;
: .CANT   cr ." Lowest positive sum that can't be expressed" cr 4 spaces
   STATS 1+ ( 0 not positive)  BEGIN dup c@ WHILE 1+ REPEAT  STATS - . ;
: .BEST   cr ." Ten highest numbers that can be expressed" cr 4 spaces
   0 >r  [ STATS 123456789 + ]L
   BEGIN  r@ 10 <  over STATS >= and
   WHILE  dup c@ IF dup STATS - .  r> 1+ >r THEN  1-
   REPEAT  r> drop ;
: .   0 <# #S #> TYPE ;
: .INFX   cr 4 spaces  9 0 DO
     INDX I + C@  OPS I cells + @ + C@
     dup  [char] # <> IF emit ELSE drop THEN  I 1+ .
   LOOP ;
: REPORT ( n)   dup 100 =  IF .INFX THEN
   dup 0> IF STATS + dup  c@ 1+  swap c! ELSE drop THEN ;
: >NUM   0. bl word count >number 2drop d>s ;
: #   10 * + ;   \ numeric concatenation
: +    >NUM + ;  \ infix +
: -    >NUM - ;  \ infix -
: .SOLUTIONS   cr ." Solutions that sum to 100:"
   BEGIN SYNTH EVALUATE REPORT INDX+ UNTIL ;
: SUM100   .SOLUTIONS .MOST .CANT .BEST cr ;
```

Note: must start Gforth with a larger-than-default dictionary size:

```txt
gforth -m 124M sum100.fs -e SUM100
```


```txt
Solutions that sum to 100:
    -1+2-3+4+5+6+78+9
    1+2+3-4+5+6+78+9
    1+2+34-5+67-8+9
    1+23-4+5+6+78-9
    1+23-4+56+7+8+9
    12+3+4+5-6-7+89
    12+3-4+5+67+8+9
    12-3-4+5-6+7+89
    123+4-5+67-89
    123+45-67+8-9
    123-4-5-6-7+8-9
    123-45-67+89
Sum that has the maximum number of solutions
    9 has 46 solutions
Lowest positive sum that can't be expressed
    211
Ten highest numbers that can be expressed
    123456789 23456790 23456788 12345687 12345669 3456801 3456792 3456790 3456788 3456786
```



## Fortran



###  Fortran IV

The program below is written in Fortran IV. Nevertheless, Fortran IV had a variety of dialects. It did not work the same on every type of computer. The source code below is compiled without problems using today's compilers. '''It have not been checked on any old mainframe.''' Sorry, I have no access to CDC6000. The algorithm used is not very fast, but uses little memory. In practice, this program took about 15 seconds to complete the task on PC (in 2017). For comparison: the program written in C ++ (using maps and STL collections) took about 1 second on the same machine.

```txt
C ROSSETACODE: SUM TO 100, FORTRAN IV
C FIND SOLUTIONS TO THE "SUM TO ONE HUNDRED" PUZZLE
C
### ===========================================


      PROGRAM SUMTO100
      DATA NEXPRM1/13121/
      WRITE(6,110)
 110  FORMAT(1X/1X,34HSHOW ALL SOLUTIONS THAT SUM TO 100/)
      DO 10 I = 0,NEXPRM1
  10  IF ( IEVAL(I) .EQ. 100 ) CALL PREXPR(I)

      WRITE(6,120)
 120  FORMAT(1X/1X,
     153HSHOW THE SUM THAT HAS THE MAXIMUM NUMBER OF SOLUTIONS/)
      NBEST = -1
      DO 30 I = 0, NEXPRM1
      ITEST = IEVAL(I)
      IF ( ITEST .LT. 0 ) GOTO 30
      NTEST = 0
      DO 20 J = 0, NEXPRM1
  20  IF ( IEVAL(J) .EQ. ITEST ) NTEST = NTEST + 1
      IF ( NTEST .LE. NBEST ) GOTO 30
      IBEST = ITEST
      NBEST = NTEST
  30  CONTINUE
      WRITE(6,121) IBEST, NBEST
 121  FORMAT(1X,I8,5H HAS ,I8,10H SOLUTIONS/)

      WRITE(6,130)
 130  FORMAT(1X/1X,
     155HSHOW THE LOWEST POSITIVE NUMBER THAT CAN'T BE EXPRESSED/)
      DO 50 I = 0,123456789
      DO 40 J = 0,NEXPRM1
  40  IF ( I .EQ. IEVAL(J) ) GOTO 50
      GOTO 60
  50  CONTINUE
  60  WRITE(6,131) I
 131  FORMAT(1X,I8)

      WRITE(6,140)
 140  FORMAT(1X/1X,
     150HSHOW THE TEN HIGHEST NUMBERS THAT CAN BE EXPRESSED/)
      ILIMIT = 123456789
      DO 90 I = 1,10
      IBEST = 0
      DO 70 J = 0, NEXPRM1
      ITEST = IEVAL(J)
  70  IF( (ITEST .LE. ILIMIT) .AND. (ITEST .GT. IBEST)) IBEST = ITEST
      DO 80 J = 0, NEXPRM1
  80  IF ( IEVAL(J) .EQ. IBEST ) CALL PREXPR(J)
  90  ILIMIT = IBEST - 1
      END

C     EVALUATE THE VALUE OF THE GIVEN ENCODED EXPRESSION
C     --------------------------------------------------
      FUNCTION IEVAL(ICODE)
      IC = ICODE
      IEVAL = 0
      N = 0
      IP = 1
      DO 50 K = 9,1,-1
      N = IP*K + N
      GOTO (10,20,40,30) MOD(IC,3)+1
  10  IEVAL = IEVAL + N
      GOTO 30
  20  IEVAL = IEVAL - N
  30  N = 0
      IP = 1
      GOTO 50
  40  IP = IP * 10
  50  IC = IC / 3
      END

C     PRINT THE ENCODED EXPRESSION IN THE READABLE FORMAT
C     ---------------------------------------------------
      SUBROUTINE PREXPR(ICODE)
      DIMENSION IH(9),IHPMJ(4)
      DATA IHPMJ/1H+,1H-,1H ,1H?/
      IA = 19683
      IB =  6561
      DO 10 K = 1,9
      IH(K) = IHPMJ(MOD(ICODE,IA) / IB+1)
      IA = IB
  10  IB = IB / 3
      IVALUE = IEVAL(ICODE)
      WRITE(6,110) IVALUE, IH
 110  FORMAT(I9,3H = 1A1,1H1,1A1,1H2,1A1,1H3,1A1,1H4,1A1,1H5,1A1,1H6,1A1
     1,1H7,1A1,1H8,1A1,1H9)
      END
```

```txt
SHOW ALL SOLUTIONS THAT SUM TO 100

      100 = +1+2+3-4+5+6+7 8+9
      100 = +1+2+3 4-5+6 7-8+9
      100 = +1+2 3-4+5+6+7 8-9
      100 = +1+2 3-4+5 6+7+8+9
      100 = +1 2+3+4+5-6-7+8 9
      100 = +1 2+3-4+5+6 7+8+9
      100 = +1 2-3-4+5-6+7+8 9
      100 = +1 2 3+4-5+6 7-8 9
      100 = +1 2 3+4 5-6 7+8-9
      100 = +1 2 3-4-5-6-7+8-9
      100 = +1 2 3-4 5-6 7+8 9
      100 = -1+2-3+4+5+6+7 8+9

 SHOW THE SUM THAT HAS THE MAXIMUM NUMBER OF SOLUTIONS

        9 HAS       46 SOLUTIONS


 SHOW THE LOWEST POSITIVE NUMBER THAT CAN'T BE EXPRESSED

      211

 SHOW THE TEN HIGHEST NUMBERS THAT CAN BE EXPRESSED

123456789 = +1 2 3 4 5 6 7 8 9
 23456790 = +1+2 3 4 5 6 7 8 9
 23456788 = -1+2 3 4 5 6 7 8 9
 12345687 = +1 2 3 4 5 6 7 8+9
 12345669 = +1 2 3 4 5 6 7 8-9
  3456801 = +1 2+3 4 5 6 7 8 9
  3456792 = +1+2+3 4 5 6 7 8 9
  3456790 = -1+2+3 4 5 6 7 8 9
  3456788 = +1-2+3 4 5 6 7 8 9
  3456786 = -1-2+3 4 5 6 7 8 9
```



###  Fortran 95

```Fortran
! RossetaCode: Sum to 100, Fortran 95, an algorithm using ternary numbers.
!
! Find solutions to the 'sum to one hundred' puzzle.
!
! We optimize algorithms for size. Therefore we don't use arrays, but recompute
! all values again and again. It is a little surprise that the time efficiency
! is quite acceptable. Actually the code is more compact than the implementation
! in C++ (STL maps and sets). We purposely break DRY and use magic values.
! Nevertheless, it is Fortran 95, free form lines, do-endo etc.

program sumto100

    parameter (nexpr = 13122)

    print *
    print *, 'Show all solutions that sum to 100'
    print *
    do i = 0, nexpr-1
        if ( ievaluate(i) .eq. 100 ) then
            call printexpr(i)
        endif
    enddo

    print *
    print *, 'Show the sum that has the maximum number of solutions'
    print *
    ibest = -1
    nbest = -1
    do i = 0, nexpr-1
        itest = ievaluate(i)
        if ( itest .ge. 0 ) then
            ntest = 0
            do j = 0, nexpr-1
                if ( ievaluate(j) .eq. itest ) then
                    ntest = ntest + 1
                endif
            enddo
            if ( (ntest .gt. nbest) ) then
                ibest = itest
                nbest = ntest
            endif
        endif
    enddo
    print *, ibest, ' has ', nbest, ' solutions'
    print *
!   do i = 0, nexpr-1
!       if ( ievaluate(i) .eq. ibest ) then
!           call printexpr(i)
!       endif
!   enddo

    print *
    print *, 'Show the lowest positive number that can''t be expressed'
    print *
    loop: do i = 0,123456789
        do j = 0,nexpr-1
            if ( i .eq. ievaluate(j) ) then
                cycle loop
            endif
        enddo
        exit
    enddo loop
    print *, i

    print *
    print *, 'Show the ten highest numbers that can be expressed'
    print *
    ilimit = 123456789
    do i = 1,10
        ibest = 0
        do j = 0, nexpr-1
            itest = ievaluate(j)
            if ( (itest .le. ilimit) .and. (itest .gt. ibest ) ) then
                ibest = itest
            endif
        enddo
        do j = 0, nexpr-1
            if ( ievaluate(j) .eq. ibest ) then
                call printexpr(j)
            endif
        enddo
        ilimit = ibest - 1;
    enddo

end

function ievaluate(icode)
    ic = icode
    ievaluate = 0
    n = 0
    ip = 1
    do k = 9,1,-1
        n = ip*k + n
        select case(mod(ic,3))
            case ( 0 )
                ievaluate = ievaluate + n
                n = 0
                ip = 1
            case ( 1 )
                ievaluate = ievaluate - n
                n = 0
                ip = 1
            case ( 2 )
                ip = ip * 10
        end select
        ic = ic / 3
    enddo
end

subroutine printexpr(icode)
    character(len=32) s
    ia = 19683
    ib =  6561
    s = ""
    do k = 1,9
        ic = mod(icode,ia) / ib
        ia = ib
        ib = ib / 3
        select case(mod(ic,3))
            case ( 0 )
                if ( k .gt. 1 ) then
                    s = trim(s) // '+'
                endif
            case ( 1 )
                s = trim(s) // '-'
        end select
        s = trim(s) // char(ichar('0')+k)
    end do
    ivalue = ievaluate(icode)
    print *, ivalue, ' = ', s
end
```

```txt

 Show all solutions that sum to 100

         100  = 1+2+3-4+5+6+78+9
         100  = 1+2+34-5+67-8+9
         100  = 1+23-4+5+6+78-9
         100  = 1+23-4+56+7+8+9
         100  = 12+3+4+5-6-7+89
         100  = 12+3-4+5+67+8+9
         100  = 12-3-4+5-6+7+89
         100  = 123+4-5+67-89
         100  = 123+45-67+8-9
         100  = 123-4-5-6-7+8-9
         100  = 123-45-67+89
         100  = -1+2-3+4+5+6+78+9

 Show the sum that has the maximum number of solutions

           9  has           46  solutions

 Show the lowest positive number that can't be expressed

         211

 Show the ten highest numbers that can be expressed

   123456789  = 123456789
    23456790  = 1+23456789
    23456788  = -1+23456789
    12345687  = 12345678+9
    12345669  = 12345678-9
     3456801  = 12+3456789
     3456792  = 1+2+3456789
     3456790  = -1+2+3456789
     3456788  = 1-2+3456789
     3456786  = -1-2+3456789

```



### Batch processing

By the simple expedient of storing all evaluations in an array (which is not so large) and then sorting the array, the required results appear in a blink. The source is essentially F77 except for the usage of an array assignment of OP = -1, writing out the highest ten results via an array expression instead of a DO-loop, array OPNAME extending from -1 to +1, a CYCLE statement rather than a GO TO, and the use of the I0 format code. Subroutine DEBLANK is straightforward, and omitted. It was only to remove spaces from the text of the expression. Reading the expression from right to left is about as picky as left-to-right.
```Fortran
      INTEGER NDIGITS,TARGET	!Document the shape.
      PARAMETER (NDIGITS = 9, TARGET = 100)
      INTEGER*1 OP(NDIGITS)	!A set of operation codes, associated with each digit.
      INTEGER N,D,P		!Number, digit, power.
      CHARACTER*1 OPNAME(-1:+1)	!Encodement of the operations.
      PARAMETER (OPNAME = (/"-"," ","+"/))	!These will look nice.
      CHARACTER*20 TEXT		!A scratchpad for the expression. Single digits only.
      INTEGER I,L,H,ME		!Assistants.
      LOGICAL CURSE		!Needed for a Comb Sort.
      INTEGER LOOP,NHIT		!Some counters.
      INTEGER ENUFF		!Collect the results.
      PARAMETER (ENUFF = 20000)	!Surely big enough...
      INTEGER VALUE(ENUFF)	!A table.
      INTEGER V,VV,PV,VE	!For scanning the table.
      INTEGER MSG		!I/O unit number.

      MSG = 6		!Standard output.
      WRITE(MSG,1) NDIGITS,TARGET	!Announce.
    1 FORMAT ("To find expressions of ",I0," digits in order, "
     1 "interspersed with + or -, adding to ",I0,/)
      NHIT = 0		!No matches to TARGET.
      LOOP = 0		!Because none have been made.
      OP = -1		!Start the expression sequence.

Calculate the value of the expression given by OP(i) i pairs.
  100 LOOP = LOOP + 1		!Here we go again.
      N = 0			!Clear the number.
      D = 0			!No previous digits have been seen.
      P = 1			!The power for the first digit.
      DO I = NDIGITS,1,-1	!Going backwards sees the digits before the sign.
        D = D + I*P			!Assimilate the digit string backwards.
        IF (OP(I).EQ.0) THEN		!A no-operation?
          P = P*10				!Yes. Prepare the power for the next digit leftwards.
         ELSE     			!Otherwise, add or subtract the digit string's value.
          N = N + SIGN(D,OP(I))			!By transferring the sign to D..
          D = 0					!Clear, ready for the next digit string.
          P = 1					!The starting power, again.
        END IF				!So much for that step.
      END DO			!On to the next.
      IF (OP(1).EQ.0) N = N + D	!Provide an implicit add for an unsigned start.
      VALUE(LOOP) = N		!Save the value for later...
      IF (N.EQ.TARGET) THEN	!Well then?
        NHIT = NHIT + 1			!Yay!
        WRITE (TEXT,101) (OPNAME(OP(I)),I, I = 1,NDIGITS)	!Translate the expression.
  101   FORMAT (10(A1,I1))		!Single-character operation codes, single-digit number parts.
        CALL DEBLANK(TEXT,L)		!Squeeze out the no-operations, so numbers are together.
        WRITE (MSG,102) N,TEXT(1:L)	!Result!
  102   FORMAT (I5,": ",A)		!This should do.
      END IF			!So much for that.

Concoct the next expression, working as if with a bignumber in base three, though offset.
  200 P = NDIGITS		!Start with the low-order digit.
  201 OP(P) = OP(P) + 1		!Add one to it.
      IF (OP(P).GT.1) THEN	!Is a carry needed?
        OP(P) = -1			!Yes. Set the digit back to the start.
        P = P - 1			!Go up a power.
        IF (P.GT.0) GO TO 201		!And augment the next digit up.
      END IF			!Once the carry fizzles, the increment is complete.
      IF (OP(1).LE.0) GO TO 100	!A leading + is equivalent to a leading no-op.

Contemplate the collection.
  300 WRITE (6,301) LOOP,NHIT
  301 FORMAT (/,I0," evaluations, ",I0," hit the target.")
Crank up a comb sort.
      H = LOOP - 1		!Last - First, and not +1.
      IF (H.LE.0) STOP "Huh?"	!Ha ha.
  310 H = MAX(1,H*10/13)	!The special feature.
      IF (H.EQ.9 .OR. H.EQ.10) H = 11	!A twiddle.
      CURSE = .FALSE.		!So far, so good.
      DO I = LOOP - H,1,-1	!If H = 1, this is a BubbleSort.
        IF (VALUE(I) .GT. VALUE(I + H)) THEN	!One compare.
          N = VALUE(I);VALUE(I)=VALUE(I+H);VALUE(I+H)=N	!One swap.
          CURSE = .TRUE.			!One curse.
        END IF				!One test.
      END DO			!One loop.
      IF (CURSE .OR. H.GT.1) GO TO 310	!Work remains?
Chase after some results.
      H = 0		!Hunt the first omitted positive number.
      VE = 0		!No equal values have been seen.
      ME = 0		!So, their maximum run length is short.
      PV = VALUE(1)	!Grab the first value,
      DO I = 2,LOOP	!And scan the successors.
        V = VALUE(I)		!The value of the moment.
        IF (V.LE.0) CYCLE	!Only positive numbers are of interest.
        IF (V.GT.PV + 1) THEN	!Is there a gap?
          IF (H.LE.0) H = PV + 1	!Recall the first such.
        END IF			!Perhaps a list of the first dew?
        IF (V.EQ.PV) THEN	!Is it the same as the one before?
          VE = VE + 1			!Yes. Count up the length of the run.
          IF (VE.GT.ME) THEN		!Is this a longer run?
            ME = VE				!Yes. Remember its length.
            VV = V 				!And its value.
          END IF			!So much for runs of equal values.
         ELSE			!But if it is not the same,
          VE = 0			!A fresh count awaits.
        END IF			!So much for comparing one value to its predecessor.
        PV = V			!Be ready for the next time around.
      END DO		!On to the next.

Cast forth the results.
      IF (ME.GT.1) WRITE (MSG,320) VV,ME + 1	!Counting started with the second occurrence.
  320 FORMAT (I0," has the maximum number of attainments:",I0)
      IF (H.GT.0) WRITE (MSG,321) H		!Surely there will be one.
  321 FORMAT ("The lowest positive sum that can't be expressed is ",I0)
      WRITE (MSG,322) VALUE(LOOP - 9:LOOP)	!Surely LOOP > 9.
  322 FORMAT ("The ten highest sums: ",10(I0:","))
      END	!That was fun!
```


Results:

```txt

To find expressions of 9 digits in order, interspersed with + or -, adding to 100

    1: -1+2-3+4+5+6+78+9
    2: 12-3-4+5-6+7+89
    3: 123-4-5-6-7+8-9
    4: 123-45-67+89
    5: 123+4-5+67-89
    6: 123+45-67+8-9
    7: 12+3-4+5+67+8+9
    8: 12+3+4+5-6-7+89
    9: 1+23-4+56+7+8+9
   10: 1+23-4+5+6+78-9
   11: 1+2+3-4+5+6+78+9
   12: 1+2+34-5+67-8+9

13122 evaluations, 12 hit the target.
9 has the maximum number of attainments:46
The lowest positive sum that can't be expressed is 211
The ten highest sums: 3456786,3456788,3456790,3456792,3456801,12345669,12345687,23456788,23456790,123456789

```



## Frink


```frink

digits = array[1 to 9]
opList = makeArray[[8], ["", " + ", " - "]]
opList.pushFirst[["", "-"]]
countDict = new dict

multifor ops = opList
{
   str = ""
   for d = rangeOf[digits]
      str = str + ops@d + digits@d
   e = eval[str]
   countDict.increment[e, 1]
   if e == 100
      println[str]
}
println[]

// Find the sum that has the maximum number of solutions
freq = toArray[countDict]
sort[freq, {|a,b| -(a@1 <=> b@1)}]
max = freq@0@1
print["Maximum count is $max at: "]
n = 0
while freq@n@1 == max
{
   print[freq@n@0 + " "]
   n = n + 1
}
println[]

// Find the smallest non-representable positive sum
sort[freq, {|a,b| a@0 <=> b@0}]
last = 0
for [num, count] = freq
{
   if num > 0 and last+1 != num
   {
      println["Lowest non-representable positive sum is " + (last+1)]
      break
   }
   last = num
}

// Find highest 10 representable numbers
println["\nHighest representable numbers:"]
size = length[freq]
for i = size-10 to size-1
   println[freq@i@0]

```

```txt

123 + 45 - 67 + 8 - 9
123 + 4 - 5 + 67 - 89
123 - 45 - 67 + 89
123 - 4 - 5 - 6 - 7 + 8 - 9
12 + 3 + 4 + 5 - 6 - 7 + 89
12 + 3 - 4 + 5 + 67 + 8 + 9
12 - 3 - 4 + 5 - 6 + 7 + 89
1 + 23 - 4 + 56 + 7 + 8 + 9
1 + 23 - 4 + 5 + 6 + 78 - 9
1 + 2 + 34 - 5 + 67 - 8 + 9
1 + 2 + 3 - 4 + 5 + 6 + 78 + 9
-1 + 2 - 3 + 4 + 5 + 6 + 78 + 9

Maximum count is 46 at: 9 -9
Lowest non-representable positive sum is 211

Highest representable numbers:
3456786
3456788
3456790
3456792
3456801
12345669
12345687
23456788
23456790
123456789

```


=={{header|F_Sharp|F#}}==

```fsharp

(*
Generate the data set
Nigel Galloway February 22nd., 2017
*)
type N = {n:string; g:int}
let N = seq {
  let rec fn n i g e l = seq {
    match i with
    |9 -> yield {n=l + "-9"; g=g+e-9}
          yield {n=l + "+9"; g=g+e+9}
          yield {n=l +  "9"; g=g+e*10+9*n}
    |_ -> yield! fn -1 (i+1) (g+e) -i (l + string -i)
          yield! fn  1 (i+1) (g+e)  i (l + "+" + string i)
          yield! fn  n (i+1) g (e*10+i*n) (l + string i)
  }
  yield! fn  1 2 0  1  "1"
  yield! fn -1 2 0 -1 "-1"
}

```

```fsharp

N |> Seq.filter(fun n->n.g=100) |> Seq.iter(fun n->printfn "%s" n.n)

```


```txt

1+2+3-4+5+6+78+9
1+2+34-5+67-8+9
1+23-4+5+6+78-9
1+23-4+56+7+8+9
12-3-4+5-6+7+89
12+3-4+5+67+8+9
12+3+4+5-6-7+89
123-4-5-6-7+8-9
123-45-67+89
123+4-5+67-89
123+45-67+8-9
-1+2-3+4+5+6+78+9

```


```fsharp

let n,g = N |> Seq.filter(fun n->n.g>=0) |> Seq.countBy(fun n->n.g) |> Seq.maxBy(snd)
printfn "%d has %d solutions" n g

```


```txt

9 has 46 solutions

```


```fsharp

match N |> Seq.filter(fun n->n.g>=0) |> Seq.distinctBy(fun n->n.g) |> Seq.sortBy(fun n->n.g) |> Seq.pairwise |> Seq.tryFind(fun n->(snd n).g-(fst n).g > 1) with
  |Some(n) -> printfn "least non-value is %d" ((fst n).g+1)
  |None    -> printfn "No non-values found"

```


```txt

least non-value is 211

```


```fsharp

N |> Seq.filter(fun n->n.g>=0) |> Seq.distinctBy(fun n->n.g) |> Seq.sortBy(fun n->(-n.g)) |> Seq.take 10 |> Seq.iter(fun n->printfn "%d" n.g )

```


```txt

123456789
23456790
23456788
12345687
12345669
3456801
3456792
3456790
3456788
3456786

```



## Go

```Go
package main

import (
	"fmt"
	"sort"
)

const pow3_8 = 3 * 3 * 3 * 3 * 3 * 3 * 3 * 3 // 3^8
const pow3_9 = 3 * pow3_8                    // 3^9
const maxExprs = 2 * pow3_8                  // not 3^9 since first op can't be Join

type op uint8

const (
	Add  op = iota // insert a "+"
	Sub            //     or a "-"
	Join           //     or just join together
)

// code is an encoding of [9]op, the nine "operations"
// we do on each each digit. The op for 1 is in
// the highest bits, the op for 9 in the lowest.
type code uint16

// evaluate 123456789 with + - or "" prepended to each as indicated by `c`.
func (c code) evaluate() (sum int) {
	num, pow := 0, 1
	for k := 9; k >= 1; k-- {
		num += pow * k
		switch op(c % 3) {
		case Add:
			sum += num
			num, pow = 0, 1
		case Sub:
			sum -= num
			num, pow = 0, 1
		case Join:
			pow *= 10
		}
		c /= 3
	}
	return sum
}

func (c code) String() string {
	buf := make([]byte, 0, 18)
	a, b := code(pow3_9), code(pow3_8)
	for k := 1; k <= 9; k++ {
		switch op((c % a) / b) {
		case Add:
			if k > 1 {
				buf = append(buf, '+')
			}
		case Sub:
			buf = append(buf, '-')
		}
		buf = append(buf, '0'+byte(k))
		a, b = b, b/3
	}
	return string(buf)
}

type sumCode struct {
	sum  int
	code code
}
type sumCodes []sumCode

type sumCount struct {
	sum   int
	count int
}
type sumCounts []sumCount

// For sorting (could also use sort.Slice with just Less).
func (p sumCodes) Len() int            { return len(p) }
func (p sumCodes) Swap(i, j int)       { p[i], p[j] = p[j], p[i] }
func (p sumCodes) Less(i, j int) bool  { return p[i].sum < p[j].sum }
func (p sumCounts) Len() int           { return len(p) }
func (p sumCounts) Swap(i, j int)      { p[i], p[j] = p[j], p[i] }
func (p sumCounts) Less(i, j int) bool { return p[i].count > p[j].count }

// For printing.
func (sc sumCode) String() string {
	return fmt.Sprintf("% 10d = %v", sc.sum, sc.code)
}
func (sc sumCount) String() string {
	return fmt.Sprintf("% 10d has %d solutions", sc.sum, sc.count)
}

func main() {
	// Evaluate all expressions.
	expressions := make(sumCodes, 0, maxExprs/2)
	counts := make(sumCounts, 0, 1715)
	for c := code(0); c < maxExprs; c++ {
		// All negative sums are exactly like their positive
		// counterpart with all +/- switched, we don't need to
		// keep track of them.
		sum := c.evaluate()
		if sum >= 0 {
			expressions = append(expressions, sumCode{sum, c})
		}
	}
	sort.Sort(expressions)

	// Count all unique sums
	sc := sumCount{expressions[0].sum, 1}
	for _, e := range expressions[1:] {
		if e.sum == sc.sum {
			sc.count++
		} else {
			counts = append(counts, sc)
			sc = sumCount{e.sum, 1}
		}
	}
	counts = append(counts, sc)
	sort.Sort(counts)

	// Extract required results

	fmt.Println("All solutions that sum to 100:")
	i := sort.Search(len(expressions), func(i int) bool {
		return expressions[i].sum >= 100
	})
	for _, e := range expressions[i:] {
		if e.sum != 100 {
			break
		}
		fmt.Println(e)
	}

	fmt.Println("\nThe positive sum with maximum number of solutions:")
	fmt.Println(counts[0])

	fmt.Println("\nThe lowest positive number that can't be expressed:")
	s := 1
	for _, e := range expressions {
		if e.sum == s {
			s++
		} else if e.sum > s {
			fmt.Printf("% 10d\n", s)
			break
		}
	}

	fmt.Println("\nThe ten highest numbers that can be expressed:")
	for _, e := range expressions[len(expressions)-10:] {
		fmt.Println(e)
	}
}
```

```txt

All solutions that sum to 100:
       100 = -1+2-3+4+5+6+78+9
       100 = 1+23-4+5+6+78-9
       100 = 123+45-67+8-9
       100 = 123+4-5+67-89
       100 = 1+2+3-4+5+6+78+9
       100 = 1+2+34-5+67-8+9
       100 = 12+3-4+5+67+8+9
       100 = 1+23-4+56+7+8+9
       100 = 123-45-67+89
       100 = 12-3-4+5-6+7+89
       100 = 12+3+4+5-6-7+89
       100 = 123-4-5-6-7+8-9

The positive sum with maximum number of solutions:
         9 has 46 solutions

The lowest positive number that can't be expressed:
       211

The ten highest numbers that can be expressed:
   3456786 = -1-2+3456789
   3456788 = 1-2+3456789
   3456790 = -1+2+3456789
   3456792 = 1+2+3456789
   3456801 = 12+3456789
  12345669 = 12345678-9
  12345687 = 12345678+9
  23456788 = -1+23456789
  23456790 = 1+23456789
 123456789 = 123456789

```



## Haskell


```Haskell
import Data.Monoid ((<>))
import Data.Ord (comparing)
import Control.Arrow ((&&&))
import Data.Char (intToDigit)
import Control.Monad (replicateM)
import Data.List (nub, group, sort, sortBy, find, intercalate)

data Sign
  = Unsigned
  | Plus
  | Minus
  deriving (Eq, Show)

universe :: [[(Int, Sign)]]
universe =
  zip [1 .. 9] <$>
  filter ((/= Plus) . head) (replicateM 9 [Unsigned, Plus, Minus])

allNonNegativeSums :: [Int]
allNonNegativeSums = sort $ filter (>= 0) (asSum <$> universe)

uniqueNonNegativeSums :: [Int]
uniqueNonNegativeSums = nub allNonNegativeSums

asSum :: [(Int, Sign)] -> Int
asSum xs =
  n +
  (case s of
     [] -> 0
     _ -> read s :: Int)
  where
    (n, s) = foldr readSign (0, []) xs
    readSign :: (Int, Sign) -> (Int, String) -> (Int, String)
    readSign (i, x) (n, s)
      | x == Unsigned = (n, intToDigit i : s)
      | otherwise =
        ( (case x of
             Plus -> (+)
             _ -> (-))
            n
            (read (show i <> s) :: Int)
        , [])

asString :: [(Int, Sign)] -> String
asString = foldr signedDigit []
  where
    signedDigit (i, x) s
      | x == Unsigned = intToDigit i : s
      | otherwise =
        (case x of
           Plus -> " +"
           _ -> " -") <>
        [intToDigit i] <>
        s

main :: IO ()
main =
  putStrLn $
  unlines
    [ "Sums to 100:"
    , unlines (asString <$> filter ((100 ==) . asSum) universe)
    , "\n10 commonest sums (sum, number of routes to it):"
    , show
        ((head &&& length) <$>
         take 10 (sortBy (flip (comparing length)) (group allNonNegativeSums)))
    , "\nFirst positive integer not expressible as a sum of this kind:"
    , maybeReport (find (uncurry (/=)) (zip [0 ..] uniqueNonNegativeSums))
    , "\n10 largest sums:"
    , show (take 10 (sortBy (flip compare) uniqueNonNegativeSums))
    ]
  where
    maybeReport
      :: Show a
      => Maybe (a, b) -> String
    maybeReport (Just (x, _)) = show x
    maybeReport _ = "No gaps found"
```

(Run in Atom editor, through Script package)

```txt
Sums to 100:
123 +45 -67 +8 -9
123 +4 -5 +67 -89
123 -45 -67 +89
123 -4 -5 -6 -7 +8 -9
12 +3 +4 +5 -6 -7 +89
12 +3 -4 +5 +67 +8 +9
12 -3 -4 +5 -6 +7 +89
1 +23 -4 +56 +7 +8 +9
1 +23 -4 +5 +6 +78 -9
1 +2 +34 -5 +67 -8 +9
1 +2 +3 -4 +5 +6 +78 +9
 -1 +2 -3 +4 +5 +6 +78 +9

10 commonest sums [sum, number of routes to it]:
[(9,46),(27,44),(1,43),(15,43),(21,43),(45,42),(3,41),(5,40),(7,39),(17,39)]

First positive integer not expressible as a sum of this kind:
211

10 largest sums:
[123456789,23456790,23456788,12345687,12345669,3456801,3456792,3456790,3456788,3456786]

[Finished in 1.204s]
```



## J

Since J has no verb precedence, -1-2 would evaluate to 1 and not to -3. That's why I decided to multiply each of the partitions of '123456789' (like '123','45', '6', '78', '9') with each possible +1/-1 vectors of length 9 (like 1 1 -1 1 -1 -1 1 1 -1) and to add up the results. This leads to 512*256 results, that of course include a lot of duplicates. To use directly ~. (nub) on the 512x256x9 vector is very slow and that's why I computed a sort of a hash to use it to get only the unique expressions. The rest is trivial - I check which expressions add up to 100; sort the sum vector and find the longest sequence ot repeating sums; get the 10 largest sums and finnaly check which sum differs with more then 1 from the previous one.


```J

p =: ,"2".>(#: (+ i.)2^8) <;.1 '123456789'
m =. (9$_1x)^"1#:i.2^9
s =. 131072 9 $ ,m *"1/ p
s2 =: (~: (10x^i._9)#.s)#s
ss =: +/"1 s2
'100=';<'bp<+>' 8!:2 (I.100=ss){s2
pos =: (0<ss)#ss =: /:~ss
({.;'times';{:)>{.\:~(#,{.) each </.~ ss
'Ten largest:';,.(->:i.10){ss
'First not expressible:';>:pos{~ 1 i.~ 1<|2-/\pos

```

```txt

┌───┬────────────────────────┐
│100│+12 +3 +4 +5 -6 -7 +89  │
│   │+1  +2 +3 -4 +5 +6 +78+9│
│   │+1  +2 +34-5 +67-8 +9   │
│   │+12 +3 -4 +5 +67+8 +9   │
│   │+1  +23-4 +56+7 +8 +9   │
│   │+1  +23-4 +5 +6 +78-9   │
│   │+123+45-67+8 -9         │
│   │+123+4 -5 +67-89        │
│   │+123-45-67+89           │
│   │+12 -3 -4 +5 -6 +7 +89  │
│   │+123-4 -5 -6 -7 +8 -9   │
│   │-1  +2 -3 +4 +5 +6 +78+9│
└───┴────────────────────────┘
┌──┬─────┬─┐
│46│times│9│
└──┴─────┴─┘
┌────────────┬─────────┐
│Ten largest:│123456789│
│            │ 23456790│
│            │ 23456788│
│            │ 12345687│
│            │ 12345669│
│            │  3456801│
│            │  3456792│
│            │  3456790│
│            │  3456788│
│            │  3456786│
└────────────┴─────────┘
┌───────────────────────┬───┐
│First not expressible :│211│
└───────────────────────┴───┘


```



## Java

For each expression of sum s, there is at least one expression whose sum is -s. If the sum s can be represented by n expressions, the sum -s can also be represented by n expressions. The change of all signs in an expression change the sign of the sum of this expression. For example, -1+23-456+789 has the opposite sign than +1-23+456-789. Therefore only the positive sum with the maximum number of solutions is shown. The program does not check uniqueness of this sum. We can easily check (modifying the program) that: sum 9 has 46 solutions; sum -9 has 46 solutions; any other sum has less than 46 solutions.

```Java
/*
 * RossetaCode: Sum to 100, Java 8.
 *
 * Find solutions to the "sum to one hundred" puzzle.
 */
package rosettacode;

import java.io.PrintStream;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

public class SumTo100 implements Runnable {

    public static void main(String[] args) {
        new SumTo100().run();
    }

    void print(int givenSum) {
        Expression expression = new Expression();
        for (int i = 0; i < Expression.NUMBER_OF_EXPRESSIONS; i++, expression.next()) {
            if (expression.toInt() == givenSum) {
                expression.print();
            }
        }
    }

    void comment(String commentString) {
        System.out.println();
        System.out.println(commentString);
        System.out.println();
    }

    @Override
    public void run() {
        final Stat stat = new Stat();

        comment("Show all solutions that sum to 100");
        final int givenSum = 100;
        print(givenSum);

        comment("Show the sum that has the maximum number of solutions");
        final int maxCount = Collections.max(stat.sumCount.keySet());
        int maxSum;
        Iterator<Integer> it = stat.sumCount.get(maxCount).iterator();
        do {
            maxSum = it.next();
        } while (maxSum < 0);
        System.out.println(maxSum + " has " + maxCount + " solutions");

        comment("Show the lowest positive number that can't be expressed");
        int value = 0;
        while (stat.countSum.containsKey(value)) {
            value++;
        }
        System.out.println(value);

        comment("Show the ten highest numbers that can be expressed");
        final int n = stat.countSum.keySet().size();
        final Integer[] sums = stat.countSum.keySet().toArray(new Integer[n]);
        Arrays.sort(sums);
        for (int i = n - 1; i >= n - 10; i--) {
            print(sums[i]);
        }
    }

    private static class Expression {

        private final static int NUMBER_OF_DIGITS = 9;
        private final static byte ADD = 0;
        private final static byte SUB = 1;
        private final static byte JOIN = 2;

        final byte[] code = new byte[NUMBER_OF_DIGITS];
        final static int NUMBER_OF_EXPRESSIONS = 2 * 3 * 3 * 3 * 3 * 3 * 3 * 3 * 3;

        Expression next() {
            for (int i = 0; i < NUMBER_OF_DIGITS; i++) {
                if (++code[i] > JOIN) {
                    code[i] = ADD;
                } else {
                    break;
                }
            }
            return this;
        }

        int toInt() {
            int value = 0;
            int number = 0;
            int sign = (+1);
            for (int digit = 1; digit <= 9; digit++) {
                switch (code[NUMBER_OF_DIGITS - digit]) {
                    case ADD:
                        value += sign * number;
                        number = digit;
                        sign = (+1);
                        break;
                    case SUB:
                        value += sign * number;
                        number = digit;
                        sign = (-1);
                        break;
                    case JOIN:
                        number = 10 * number + digit;
                        break;
                }
            }
            return value + sign * number;
        }

        @Override
        public String toString() {
            StringBuilder s = new StringBuilder(2 * NUMBER_OF_DIGITS + 1);
            for (int digit = 1; digit <= NUMBER_OF_DIGITS; digit++) {
                switch (code[NUMBER_OF_DIGITS - digit]) {
                    case ADD:
                        if (digit > 1) {
                            s.append('+');
                        }
                        break;
                    case SUB:
                        s.append('-');
                        break;
                }
                s.append(digit);
            }
            return s.toString();
        }

        void print() {
            print(System.out);
        }

        void print(PrintStream printStream) {
            printStream.format("%9d", this.toInt());
            printStream.println(" = " + this);
        }
    }

    private static class Stat {

        final Map<Integer, Integer> countSum = new HashMap<>();
        final Map<Integer, Set<Integer>> sumCount = new HashMap<>();

        Stat() {
            Expression expression = new Expression();
            for (int i = 0; i < Expression.NUMBER_OF_EXPRESSIONS; i++, expression.next()) {
                int sum = expression.toInt();
                countSum.put(sum, countSum.getOrDefault(sum, 0) + 1);
            }
            for (Map.Entry<Integer, Integer> entry : countSum.entrySet()) {
                Set<Integer> set;
                if (sumCount.containsKey(entry.getValue())) {
                    set = sumCount.get(entry.getValue());
                } else {
                    set = new HashSet<>();
                }
                set.add(entry.getKey());
                sumCount.put(entry.getValue(), set);
            }
        }
    }
}
```

```txt
Show all solutions that sum to 100

      100 = 1+2+3-4+5+6+78+9
      100 = 1+2+34-5+67-8+9
      100 = 1+23-4+5+6+78-9
      100 = 1+23-4+56+7+8+9
      100 = 12+3+4+5-6-7+89
      100 = 12+3-4+5+67+8+9
      100 = 12-3-4+5-6+7+89
      100 = 123+4-5+67-89
      100 = 123+45-67+8-9
      100 = 123-4-5-6-7+8-9
      100 = 123-45-67+89
      100 = -1+2-3+4+5+6+78+9

Show the sum that has the maximum number of solutions

9 has 46 solutions

Show the lowest positive number that can't be expressed

211

Show the ten highest numbers that can be expressed

123456789 = 123456789
 23456790 = 1+23456789
 23456788 = -1+23456789
 12345687 = 12345678+9
 12345669 = 12345678-9
  3456801 = 12+3456789
  3456792 = 1+2+3456789
  3456790 = -1+2+3456789
  3456788 = 1-2+3456789
  3456786 = -1-2+3456789
```



## JavaScript


### ES5

```JavaScript
(function () {
    'use strict';

    // GENERIC FUNCTIONS ----------------------------------------------------

    // permutationsWithRepetition :: Int -> [a] -> [[a]]
    var permutationsWithRepetition = function (n, as) {
        return as.length > 0 ?
            foldl1(curry(cartesianProduct)(as), replicate(n, as)) : [];
    };

    // cartesianProduct :: [a] -> [b] -> [[a, b]]
    var cartesianProduct = function (xs, ys) {
        return [].concat.apply([], xs.map(function (x) {
            return [].concat.apply([], ys.map(function (y) {
                return [
                    [x].concat(y)
                ];
            }));
        }));
    };

    // curry :: ((a, b) -> c) -> a -> b -> c
    var curry = function (f) {
        return function (a) {
            return function (b) {
                return f(a, b);
            };
        };
    };

    // flip :: (a -> b -> c) -> b -> a -> c
    var flip = function (f) {
        return function (a, b) {
            return f.apply(null, [b, a]);
        };
    };

    // foldl1 :: (a -> a -> a) -> [a] -> a
    var foldl1 = function (f, xs) {
        return xs.length > 0 ? xs.slice(1)
            .reduce(f, xs[0]) : [];
    };

    // replicate :: Int -> a -> [a]
    var replicate = function (n, a) {
        var v = [a],
            o = [];
        if (n < 1) return o;
        while (n > 1) {
            if (n & 1) o = o.concat(v);
            n >>= 1;
            v = v.concat(v);
        }
        return o.concat(v);
    };

    // group :: Eq a => [a] -> [[a]]
    var group = function (xs) {
        return groupBy(function (a, b) {
            return a === b;
        }, xs);
    };

    // groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
    var groupBy = function (f, xs) {
        var dct = xs.slice(1)
            .reduce(function (a, x) {
                var h = a.active.length > 0 ? a.active[0] : undefined,
                    blnGroup = h !== undefined && f(h, x);

                return {
                    active: blnGroup ? a.active.concat(x) : [x],
                    sofar: blnGroup ? a.sofar : a.sofar.concat([a.active])
                };
            }, {
                active: xs.length > 0 ? [xs[0]] : [],
                sofar: []
            });
        return dct.sofar.concat(dct.active.length > 0 ? [dct.active] : []);
    };

    // compare :: a -> a -> Ordering
    var compare = function (a, b) {
        return a < b ? -1 : a > b ? 1 : 0;
    };

    // on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
    var on = function (f, g) {
        return function (a, b) {
            return f(g(a), g(b));
        };
    };

    // nub :: [a] -> [a]
    var nub = function (xs) {
        return nubBy(function (a, b) {
            return a === b;
        }, xs);
    };

    // nubBy :: (a -> a -> Bool) -> [a] -> [a]
    var nubBy = function (p, xs) {
        var x = xs.length ? xs[0] : undefined;

        return x !== undefined ? [x].concat(nubBy(p, xs.slice(1)
            .filter(function (y) {
                return !p(x, y);
            }))) : [];
    };

    // find :: (a -> Bool) -> [a] -> Maybe a
    var find = function (f, xs) {
        for (var i = 0, lng = xs.length; i < lng; i++) {
            if (f(xs[i], i)) return xs[i];
        }
        return undefined;
    };

    // Int -> [a] -> [a]
    var take = function (n, xs) {
        return xs.slice(0, n);
    };

    // unlines :: [String] -> String
    var unlines = function (xs) {
        return xs.join('\n');
    };

    // show :: a -> String
    var show = function (x) {
        return JSON.stringify(x);
    }; //, null, 2);

    // head :: [a] -> a
    var head = function (xs) {
        return xs.length ? xs[0] : undefined;
    };

    // tail :: [a] -> [a]
    var tail = function (xs) {
        return xs.length ? xs.slice(1) : undefined;
    };

    // length :: [a] -> Int
    var length = function (xs) {
        return xs.length;
    };

    // SIGNED DIGIT SEQUENCES  (mapped to sums and to strings)

    // data Sign :: [ 0 | 1 | -1 ] = ( Unsigned | Plus | Minus )
    // asSum :: [Sign] -> Int
    var asSum = function (xs) {
        var dct = xs.reduceRight(function (a, sign, i) {
            var d = i + 1; //  zero-based index to [1-9] positions
            if (sign !== 0) {
                // Sum increased, digits cleared
                return {
                    digits: [],
                    n: a.n + sign * parseInt([d].concat(a.digits)
                        .join(''), 10)
                };
            } else return { // Digits extended, sum unchanged
                digits: [d].concat(a.digits),
                n: a.n
            };
        }, {
            digits: [],
            n: 0
        });
        return dct.n + (
            dct.digits.length > 0 ? parseInt(dct.digits.join(''), 10) : 0
        );
    };

    // data Sign :: [ 0 | 1 | -1 ] = ( Unsigned | Plus | Minus )
    // asString :: [Sign] -> String
    var asString = function (xs) {
        var ns = xs.reduce(function (a, sign, i) {
            var d = (i + 1)
                .toString();
            return sign === 0 ? a + d : a + (sign > 0 ? ' +' : ' -') + d;
        }, '');

        return ns[0] === '+' ? tail(ns) : ns;
    };

    // SUM T0 100 ------------------------------------------------------------

    // universe :: [[Sign]]
    var universe = permutationsWithRepetition(9, [0, 1, -1])
        .filter(function (x) {
            return x[0] !== 1;
        });

    // allNonNegativeSums :: [Int]
    var allNonNegativeSums = universe.map(asSum)
        .filter(function (x) {
            return x >= 0;
        })
        .sort();

    // uniqueNonNegativeSums :: [Int]
    var uniqueNonNegativeSums = nub(allNonNegativeSums);

    return ["Sums to 100:\n", unlines(universe.filter(function (x) {
                return asSum(x) === 100;
            })
            .map(asString)),

        "\n\n10 commonest sums (sum, followed by number of routes to it):\n",
        show(take(10, group(allNonNegativeSums)
            .sort(on(flip(compare), length))
            .map(function (xs) {
                return [xs[0], xs.length];
            }))),

        "\n\nFirst positive integer not expressible as a sum of this kind:\n",
        show(find(function (x, i) {
            return x !== i;
        }, uniqueNonNegativeSums.sort(compare)) - 1), // zero-based index

        "\n10 largest sums:\n",
        show(take(10, uniqueNonNegativeSums.sort(flip(compare))))
    ].join('\n') + '\n';
})();
```


(Run in Atom editor, through Script package)

```txt
Sums to 100:

123 +45 -67 +8 -9
123 +4 -5 +67 -89
123 -45 -67 +89
123 -4 -5 -6 -7 +8 -9
12 +3 +4 +5 -6 -7 +89
12 +3 -4 +5 +67 +8 +9
12 -3 -4 +5 -6 +7 +89
1 +23 -4 +56 +7 +8 +9
1 +23 -4 +5 +6 +78 -9
1 +2 +34 -5 +67 -8 +9
1 +2 +3 -4 +5 +6 +78 +9
 -1 +2 -3 +4 +5 +6 +78 +9


10 commonest sums (sum, followed by number of routes to it):

[[9,46],[27,44],[1,43],[15,43],[21,43],[45,42],[3,41],[5,40],[17,39],[7,39]]


First positive integer not expressible as a sum of this kind:

211

10 largest sums:

[123456789,23456790,23456788,12345687,12345669,3456801,3456792,3456790,3456788,3456786]

[Finished in 0.381s]
```



### ES6

```JavaScript
(() => {
    'use strict';

    // GENERIC FUNCTIONS ----------------------------------------------------

    // permutationsWithRepetition :: Int -> [a] -> [[a]]
    const permutationsWithRepetition = (n, as) =>
        as.length > 0 ? (
            foldl1(curry(cartesianProduct)(as), replicate(n, as))
        ) : [];

    // cartesianProduct :: [a] -> [b] -> [[a, b]]
    const cartesianProduct = (xs, ys) =>
        [].concat.apply([], xs.map(x =>
        [].concat.apply([], ys.map(y => [[x].concat(y)]))));

    // curry :: ((a, b) -> c) -> a -> b -> c
    const curry = f => a => b => f(a, b);

    // flip :: (a -> b -> c) -> b -> a -> c
    const flip = f => (a, b) => f.apply(null, [b, a]);

    // foldl1 :: (a -> a -> a) -> [a] -> a
    const foldl1 = (f, xs) =>
        xs.length > 0 ? xs.slice(1)
        .reduce(f, xs[0]) : [];

    // replicate :: Int -> a -> [a]
    const replicate = (n, a) => {
        let v = [a],
            o = [];
        if (n < 1) return o;
        while (n > 1) {
            if (n & 1) o = o.concat(v);
            n >>= 1;
            v = v.concat(v);
        }
        return o.concat(v);
    };

    // group :: Eq a => [a] -> [[a]]
    const group = xs => groupBy((a, b) => a === b, xs);

    // groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
    const groupBy = (f, xs) => {
        const dct = xs.slice(1)
            .reduce((a, x) => {
                const
                    h = a.active.length > 0 ? a.active[0] : undefined,
                    blnGroup = h !== undefined && f(h, x);

                return {
                    active: blnGroup ? a.active.concat(x) : [x],
                    sofar: blnGroup ? a.sofar : a.sofar.concat([a.active])
                };
            }, {
                active: xs.length > 0 ? [xs[0]] : [],
                sofar: []
            });
        return dct.sofar.concat(dct.active.length > 0 ? [dct.active] : []);
    };

    // compare :: a -> a -> Ordering
    const compare = (a, b) => a < b ? -1 : (a > b ? 1 : 0);

    // on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
    const on = (f, g) => (a, b) => f(g(a), g(b));

    // nub :: [a] -> [a]
    const nub = xs => nubBy((a, b) => a === b, xs);

    // nubBy :: (a -> a -> Bool) -> [a] -> [a]
    const nubBy = (p, xs) => {
        const x = xs.length ? xs[0] : undefined;

        return x !== undefined ? [x].concat(
            nubBy(p, xs.slice(1)
                .filter(y => !p(x, y)))
        ) : [];
    };

    // find :: (a -> Bool) -> [a] -> Maybe a
    const find = (f, xs) => {
        for (var i = 0, lng = xs.length; i < lng; i++) {
            if (f(xs[i], i)) return xs[i];
        }
        return undefined;
    }

    // Int -> [a] -> [a]
    const take = (n, xs) => xs.slice(0, n);

    // unlines :: [String] -> String
    const unlines = xs => xs.join('\n');

    // show :: a -> String
    const show = x => JSON.stringify(x); //, null, 2);

    // head :: [a] -> a
    const head = xs => xs.length ? xs[0] : undefined;

    // tail :: [a] -> [a]
    const tail = xs => xs.length ? xs.slice(1) : undefined;

    // length :: [a] -> Int
    const length = xs => xs.length;


    // SIGNED DIGIT SEQUENCES  (mapped to sums and to strings)

    // data Sign :: [ 0 | 1 | -1 ] = ( Unsigned | Plus | Minus )
    // asSum :: [Sign] -> Int
    const asSum = xs => {
        const dct = xs.reduceRight((a, sign, i) => {
            const d = i + 1; //  zero-based index to [1-9] positions
            if (sign !== 0) { // Sum increased, digits cleared
                return {
                    digits: [],
                    n: a.n + (sign * parseInt([d].concat(a.digits)
                        .join(''), 10))
                };
            } else return { // Digits extended, sum unchanged
                digits: [d].concat(a.digits),
                n: a.n
            };
        }, {
            digits: [],
            n: 0
        });
        return dct.n + (dct.digits.length > 0 ? (
            parseInt(dct.digits.join(''), 10)
        ) : 0);
    };

    // data Sign :: [ 0 | 1 | -1 ] = ( Unsigned | Plus | Minus )
    // asString :: [Sign] -> String
    const asString = xs => {
        const ns = xs.reduce((a, sign, i) => {
            const d = (i + 1)
                .toString();
            return (sign === 0 ? (
                a + d
            ) : (a + (sign > 0 ? ' +' : ' -') + d));
        }, '');

        return ns[0] === '+' ? tail(ns) : ns;
    };


    // SUM T0 100 ------------------------------------------------------------

    // universe :: [[Sign]]
    const universe = permutationsWithRepetition(9, [0, 1, -1])
        .filter(x => x[0] !== 1);

    // allNonNegativeSums :: [Int]
    const allNonNegativeSums = universe.map(asSum)
        .filter(x => x >= 0)
        .sort();

    // uniqueNonNegativeSums :: [Int]
    const uniqueNonNegativeSums = nub(allNonNegativeSums);


    return [
        "Sums to 100:\n",
        unlines(universe.filter(x => asSum(x) === 100)
            .map(asString)),

        "\n\n10 commonest sums (sum, followed by number of routes to it):\n",
        show(take(10, group(allNonNegativeSums)
            .sort(on(flip(compare), length))
            .map(xs => [xs[0], xs.length]))),

        "\n\nFirst positive integer not expressible as a sum of this kind:\n",
        show(find(
            (x, i) => x !== i,
            uniqueNonNegativeSums.sort(compare)
        ) - 1), // i is the the zero-based Array index.

        "\n10 largest sums:\n",
        show(take(10, uniqueNonNegativeSums.sort(flip(compare))))
    ].join('\n') + '\n';
})();
```


(Run in Atom editor, through Script package)

```txt
Sums to 100:

123 +45 -67 +8 -9
123 +4 -5 +67 -89
123 -45 -67 +89
123 -4 -5 -6 -7 +8 -9
12 +3 +4 +5 -6 -7 +89
12 +3 -4 +5 +67 +8 +9
12 -3 -4 +5 -6 +7 +89
1 +23 -4 +56 +7 +8 +9
1 +23 -4 +5 +6 +78 -9
1 +2 +34 -5 +67 -8 +9
1 +2 +3 -4 +5 +6 +78 +9
 -1 +2 -3 +4 +5 +6 +78 +9


10 commonest sums (sum, followed by number of routes to it):

[[9,46],[27,44],[1,43],[15,43],[21,43],[45,42],[3,41],[5,40],[17,39],[7,39]]


First positive integer not expressible as a sum of this kind:

211

10 largest sums:

[123456789,23456790,23456788,12345687,12345669,3456801,3456792,3456790,3456788,3456786]

[Finished in 0.382s]
```


===ES3 (JScript)===
```javascript
SumTo100();

function SumTo100()
{
    var
        ADD  = 0,
        SUB  = 1,
        JOIN = 2;

    var
        nexpr = 13122;

    function out(something)
    {
        WScript.Echo(something);
    }

    function evaluate(code)
    {
        var
            value  = 0,
            number = 0,
            power  = 1;

        for ( var k = 9; k >= 1; k-- )
        {
            number = power*k + number;
            switch( code % 3 )
            {
                case ADD:  value = value + number; number = 0; power = 1; break;
                case SUB:  value = value - number; number = 0; power = 1; break;
                case JOIN: power = power * 10                           ; break;
            }
            code = Math.floor(code/3);
        }
        return value;
    }

    function print(code)
    {
        var
            s = "";
        var
            a = 19683,
            b = 6561;

        for ( var k = 1; k <= 9; k++ )
        {
            switch( Math.floor(  (code % a) / b  ) ){
                case ADD: if ( k > 1 ) s = s + '+'; break;
                case SUB:              s = s + '-'; break;
            }
            a = b;
            b = Math.floor(b/3);
            s = s + String.fromCharCode(0x30+k);
        }
        out(evaluate(code) + " = " + s);
    }

    function comment(commentString)
    {
        out("");
        out(commentString);
        out("");
    }

    comment("Show all solutions that sum to 100");
    for ( var i = 0; i < nexpr; i++)
        if ( evaluate(i) == 100 )
            print(i);

    comment("Show the sum that has the maximum number of solutions");
    var stat = {};
    for ( var i = 0; i < nexpr; i++ )
    {
        var sum = evaluate(i);
        if (stat[sum])
            stat[sum]++;
        else
            stat[sum] = 1;
    }

    var best = 0;
    var nbest = -1;
    for ( var i = 0; i < nexpr; i++ )
    {
        var sum = evaluate(i);
        if ( sum > 0 )
            if ( stat[sum] > nbest )
            {
                best = i;
                nbest = stat[sum];
            }
    }
    out("" + evaluate(best) + " has " + nbest + " solutions");

    comment("Show the lowest positive number that can't be expressed");
    for ( var i = 0; i <= 123456789; i++ )
    {
        for ( var j = 0; j < nexpr; j++)
            if ( i == evaluate(j) ) break;
        if ( i != evaluate(j) ) break;
    }
    out(i);

    comment("Show the ten highest numbers that can be expressed");
    var limit = 123456789 + 1;
    for ( i = 1; i <= 10; i++ )
    {
        var best = 0;
        for ( var j = 0; j < nexpr; j++)
        {
            var test = evaluate(j);
            if ( test < limit && test > best )
                best = test;
        }
        for ( var j = 0; j < nexpr; j++)
            if ( evaluate(j) == best ) print(j);
        limit = best;
    }

}

```

```txt
Show all solutions that sum to 100

100 = 1+2+3-4+5+6+78+9
100 = 1+2+34-5+67-8+9
100 = 1+23-4+5+6+78-9
100 = 1+23-4+56+7+8+9
100 = 12+3+4+5-6-7+89
100 = 12+3-4+5+67+8+9
100 = 12-3-4+5-6+7+89
100 = 123+4-5+67-89
100 = 123+45-67+8-9
100 = 123-4-5-6-7+8-9
100 = 123-45-67+89
100 = -1+2-3+4+5+6+78+9

Show the sum that has the maximum number of solutions

9 has 46 solutions

Show the lowest positive number that can't be expressed

211

Show the ten highest numbers that can be expressed

123456789 = 123456789
23456790 = 1+23456789
23456788 = -1+23456789
12345687 = 12345678+9
12345669 = 12345678-9
3456801 = 12+3456789
3456792 = 1+2+3456789
3456790 = -1+2+3456789
3456788 = 1-2+3456789
3456786 = -1-2+3456789
```



## jq

For ease of understanding, the problems will be solved separately, using the machinery defined in the following section.

'''All possible sums'''

```jq
# Generate a "sum" in the form:  [I, 1, X, 2, X, 3, ..., X, n] where I is "-" or "", and X is "+", "-", or ""
def generate(n):
  def pm: ["+"], ["-"], [""];

  if n == 1 then (["-"], [""]) + [1]
  else generate(n-1) + pm +  [n]
  end;

# The numerical value of a "sum"
def addup:
  reduce .[] as $x ({sum:0, previous: "0"};
     if   $x == "+" then .sum += (.previous|tonumber) | .previous = ""
     elif $x == "-" then .sum += (.previous|tonumber) | .previous = "-"
     elif $x == "" then .
     else .previous += ($x|tostring)
     end)
     | .sum + (.previous | tonumber) ;

# Pretty-print a "sum", e.g. ["",1,"+", 2] => 1 + 2
def pp: map(if . == "+" or . == "-" then " " + . else tostring end) | join("");

```

'''Solutions to "Sum to 100" problem'''

```jq
generate(9) | select(addup == 100) | pp
```

```txt

1 +23 -4 +56 +7 +8 +9
12 +3 -4 +5 +67 +8 +9
1 +2 +34 -5 +67 -8 +9
 -1 +2 -3 +4 +5 +6 +78 +9
1 +2 +3 -4 +5 +6 +78 +9
123 -4 -5 -6 -7 +8 -9
123 +45 -67 +8 -9
1 +23 -4 +5 +6 +78 -9
12 -3 -4 +5 -6 +7 +89
12 +3 +4 +5 -6 -7 +89
123 -45 -67 +89
123 +4 -5 +67 -89
```


'''Helper Functions'''

For brevity, we define an efficient function for computing a histogram in the form of a JSON object, and
a helper function for identifying the values with the n highest frequencies.

```jq
def histogram(s): reduce s as $x ({}; ($x|tostring) as $k | .[$k] += 1);

# Emit an array of [ value, frequency ] pairs
def greatest(n):
  to_entries
  | map( [.key, .value] )
  | sort_by(.[1])
  | .[(length-n):]
  | reverse ;
```


'''Maximum number of solutions'''

```jq
histogram(generate(9) | addup | select(.>0)) | greatest(1)
```

```txt
[["9",46]]
```


'''Ten most frequent sums'''

```jq
histogram(generate(9) | addup | select(.>0)) | greatest(1)
```

```txt
[["9",46],["27",44],["1",43],["21",43],["15",43],["45",42],["3",41],["5",40],["7",39],["17",39]]
```


'''First unsolvable'''

```jq
def first_missing(s):
    first( foreach s as $i (null;
           if . == null or $i == . or $i == .+1 then $i else [.+1] end;
           select(type == "array") | .[0]));

first_missing( [generate(9) | addup | select(.>0) ] | unique[])
```

    211

'''Ten largest sums'''

```jq
[generate(9) | addup | select(.>0)] | unique | .[(length-10):]
```

    [3456786,3456788,3456790,3456792,3456801,12345669,12345687,23456788,23456790,123456789]



## Julia


```julia
# v0.6

using IterTools

expr(p::String...)::String = @sprintf("%s1%s2%s3%s4%s5%s6%s7%s8%s9", p...)
function genexpr()::Vector{String}
    op = ["+", "-", ""]
    return collect(expr(p...) for (p) in product(op, op, op, op, op, op, op, op, op) if p[1] != "+")
end

using DataStructures

function allexpr()::Dict{Int,Int}
    rst = DefaultDict{Int,Int}(0)
    for e in genexpr()
        val = eval(parse(e))
        rst[val] += 1
    end
    return rst
end

sumto(val::Int)::Vector{String} = filter(e -> eval(parse(e)) == val, genexpr())
function maxsolve()::Dict{Int,Int}
    ae = allexpr()
    vmax = maximum(values(ae))
    smax = filter(ae) do v, f
        f == vmax
    end
    return smax
end
function minsolve()::Int
    ae = keys(allexpr())
    for i in 1:typemax(Int)
        if i ∉ ae
            return i
        end
    end
end
function highestsums(n::Int)::Vector{Int}
    sums = collect(keys(allexpr()))
    return sort!(sums; rev=true)[1:n]
end

solutions = sumto(100)
max   = maxsolve()
min   = minsolve()
hsums = highestsums(10)

println("100 =")
foreach(println, solutions)

println("\nMax number of solutions:")
for (v, f) in max
    @printf("%3i -> %2i\n", v, f)
end

println("\nMin number with no solutions: $min")

println("\nHighest sums representable:")
foreach(println, hsums)
```


```txt
100 =
1+23-4+56+7+8+9
12+3-4+5+67+8+9
1+2+34-5+67-8+9
-1+2-3+4+5+6+78+9
1+2+3-4+5+6+78+9
123-4-5-6-7+8-9
123+45-67+8-9
1+23-4+5+6+78-9
12-3-4+5-6+7+89
12+3+4+5-6-7+89
123-45-67+89
123+4-5+67-89

Max number of solutions:
  9 -> 46
 -9 -> 46

Min number with no solutions: 211

Highest sums representable:
123456789
23456790
23456788
12345687
12345669
3456801
3456792
3456790
3456788
3456786
```



## Kotlin

```scala
// version 1.1.51

class Expression {

    private enum class Op { ADD, SUB, JOIN }
    private val code = Array<Op>(NUMBER_OF_DIGITS) { Op.ADD }

    companion object {
        private const val NUMBER_OF_DIGITS = 9
        private const val THREE_POW_4 = 3 * 3 * 3 * 3
        private const val FMT = "%9d"
        const val NUMBER_OF_EXPRESSIONS = 2 * THREE_POW_4 * THREE_POW_4

        fun print(givenSum: Int) {
            var expression = Expression()
            repeat(Expression.NUMBER_OF_EXPRESSIONS) {
                if (expression.toInt() == givenSum) println("${FMT.format(givenSum)} = $expression")
                expression++
            }
        }
    }

    operator fun inc(): Expression {
        for (i in 0 until code.size) {
            code[i] = when (code[i]) {
                Op.ADD  -> Op.SUB
                Op.SUB  -> Op.JOIN
                Op.JOIN -> Op.ADD
            }
            if (code[i] != Op.ADD) break
        }
        return this
    }

    fun toInt(): Int {
        var value = 0
        var number = 0
        var sign = +1
        for (digit in 1..9) {
            when (code[NUMBER_OF_DIGITS - digit]) {
                Op.ADD  -> { value += sign * number; number = digit; sign = +1 }
                Op.SUB  -> { value += sign * number; number = digit; sign = -1 }
                Op.JOIN -> { number = 10 * number + digit }
            }
        }
        return value + sign * number
    }

    override fun toString(): String {
        val sb = StringBuilder()
        for (digit in 1..NUMBER_OF_DIGITS) {
            when (code[NUMBER_OF_DIGITS - digit]) {
                Op.ADD  -> if (digit > 1) sb.append(" + ")
                Op.SUB  -> sb.append(" - ")
                Op.JOIN -> {}
            }
            sb.append(digit)
        }
        return sb.toString().trimStart()
    }
}

class Stat {

    val countSum = mutableMapOf<Int, Int>()
    val sumCount = mutableMapOf<Int, MutableSet<Int>>()

    init {
        var expression = Expression()
        repeat (Expression.NUMBER_OF_EXPRESSIONS) {
            val sum = expression.toInt()
            countSum.put(sum, 1 + (countSum[sum] ?: 0))
            expression++
        }
        for ((k, v) in countSum) {
            val set = if (sumCount.containsKey(v))
                sumCount[v]!!
            else
                mutableSetOf<Int>()
            set.add(k)
            sumCount.put(v, set)
        }
    }
}

fun main(args: Array<String>) {
    println("100 has the following solutions:\n")
    Expression.print(100)

    val stat = Stat()
    val maxCount = stat.sumCount.keys.max()
    val maxSum = stat.sumCount[maxCount]!!.max()
    println("\n$maxSum has the maximum number of solutions, namely $maxCount")

    var value = 0
    while (stat.countSum.containsKey(value)) value++
    println("\n$value is the lowest positive number with no solutions")

    println("\nThe ten highest numbers that do have solutions are:\n")
    stat.countSum.keys.toIntArray().sorted().reversed().take(10).forEach { Expression.print(it) }
}
```


```txt

100 has the following solutions:

      100 = 1 + 2 + 3 - 4 + 5 + 6 + 78 + 9
      100 = 1 + 2 + 34 - 5 + 67 - 8 + 9
      100 = 1 + 23 - 4 + 5 + 6 + 78 - 9
      100 = 1 + 23 - 4 + 56 + 7 + 8 + 9
      100 = 12 + 3 + 4 + 5 - 6 - 7 + 89
      100 = 12 + 3 - 4 + 5 + 67 + 8 + 9
      100 = 12 - 3 - 4 + 5 - 6 + 7 + 89
      100 = 123 + 4 - 5 + 67 - 89
      100 = 123 + 45 - 67 + 8 - 9
      100 = 123 - 4 - 5 - 6 - 7 + 8 - 9
      100 = 123 - 45 - 67 + 89
      100 = - 1 + 2 - 3 + 4 + 5 + 6 + 78 + 9

9 has the maximum number of solutions, namely 46

211 is the lowest positive number with no solutions

The ten highest numbers that do have solutions are:

123456789 = 123456789
 23456790 = 1 + 23456789
 23456788 = - 1 + 23456789
 12345687 = 12345678 + 9
 12345669 = 12345678 - 9
  3456801 = 12 + 3456789
  3456792 = 1 + 2 + 3456789
  3456790 = - 1 + 2 + 3456789
  3456788 = 1 - 2 + 3456789
  3456786 = - 1 - 2 + 3456789

```



## Lua

```lua
local expressionsLength = 0
function compareExpressionBySum(a, b)
    return a.sum - b.sum
end

local countSumsLength = 0
function compareCountSumsByCount(a, b)
    return a.counts - b.counts
end

function evaluate(code)
    local value = 0
    local number = 0
    local power = 1
    for k=9,1,-1 do
        number = power*k + number
        local mod = code % 3
        if mod == 0 then
            -- ADD
            value = value + number
            number = 0
            power = 1
        elseif mod == 1 then
            -- SUB
            value = value - number
            number = 0
            power = 1
        elseif mod == 2 then
            -- JOIN
            power = 10 * power
        else
            print("This should not happen.")
        end
        code = math.floor(code / 3)
    end
    return value
end

function printCode(code)
    local a = 19683
    local b = 6561
    local s = ""
    for k=1,9 do
        local temp = math.floor((code % a) / b)
        if temp == 0 then
            -- ADD
            if k>1 then
                s = s .. '+'
            end
        elseif temp == 1 then
            -- SUB
            s = s .. '-'
        end
        a = b
        b = math.floor(b/3)
        s = s .. tostring(k)
    end
    print("\t"..evaluate(code).." = "..s)
end

-- Main
local nexpr = 13122

print("Show all solutions that sum to 100")
for i=0,nexpr-1 do
    if evaluate(i) == 100 then
        printCode(i)
    end
end
print()

print("Show the sum that has the maximum number of solutions")
local nbest = -1
for i=0,nexpr-1 do
    local test = evaluate(i)
    if test>0 then
        local ntest = 0
        for j=0,nexpr-1 do
            if evaluate(j) == test then
                ntest = ntest + 1
            end
            if ntest > nbest then
                best = test
                nbest = ntest
            end
        end
    end
end
print(best.." has "..nbest.." solutions\n")

print("Show the lowest positive number that can't be expressed")
local code = -1
for i=0,123456789 do
    for j=0,nexpr-1 do
        if evaluate(j) == i then
            code = j
            break
        end
    end
    if evaluate(code) ~= i then
        code = i
        break
    end
end
print(code.."\n")

print("Show the ten highest numbers that can be expressed")
local limit = 123456789 + 1
for i=1,10 do
    local best=0
    for j=0,nexpr-1 do
        local test = evaluate(j)
        if (test<limit) and (test>best) then
            best = test
        end
    end
    for j=0,nexpr-1 do
        if evaluate(j) == best then
            printCode(j)
        end
    end
    limit = best
end
```

```txt
Show all solutions that sum to 100
        100 = 1+2+3-4+5+6+78+9
        100 = 1+2+34-5+67-8+9
        100 = 1+23-4+5+6+78-9
        100 = 1+23-4+56+7+8+9
        100 = 12+3+4+5-6-7+89
        100 = 12+3-4+5+67+8+9
        100 = 12-3-4+5-6+7+89
        100 = 123+4-5+67-89
        100 = 123+45-67+8-9
        100 = 123-4-5-6-7+8-9
        100 = 123-45-67+89
        100 = -1+2-3+4+5+6+78+9

Show the sum that has the maximum number of solutions
9 has 46 solutions

Show the lowest positive number that can't be expressed
211

Show the ten highest numbers that can be expressed
        123456789 = 123456789
        23456790 = 1+23456789
        23456788 = -1+23456789
        12345687 = 12345678+9
        12345669 = 12345678-9
        3456801 = 12+3456789
        3456792 = 1+2+3456789
        3456790 = -1+2+3456789
        3456788 = 1-2+3456789
        3456786 = -1-2+3456789
```



## Mathematica

Defining all possible sums:


```Mathematica
operations =
  DeleteCases[Tuples[{"+", "-", ""}, 9], {x_, y__} /; x == "+"];

sums =
  Map[StringJoin[Riffle[#, CharacterRange["1", "9"]]] &, operations];
```


Sums to 100:


```Mathematica
 TableForm@Select[sums, ToExpression@# == 100 &]
```

```txt
-1+2-3+4+5+6+78+9
1+2+3-4+5+6+78+9
1+2+34-5+67-8+9
1+23-4+5+6+78-9
1+23-4+56+7+8+9
12+3+4+5-6-7+89
12+3-4+5+67+8+9
12-3-4+5-6+7+89
123+4-5+67-89
123+45-67+8-9
123-4-5-6-7+8-9
123-45-67+89
```


Maximum number of solutions:

```Mathematica
 MaximalBy[Counts@ToExpression@sums, Identity]
```

```txt
 <|9 -> 46, -9 -> 46|>
```


First unsolvable:

```Mathematica
 pos = Cases[ToExpression@sums, _?Positive];
n = 1; While[MemberQ[pos, n], ++n];
```

```txt
211
```


Ten largest sums:

```Mathematica
 {#, ToExpression@#}&/@TakeLargestBy[sums, ToExpression, 10]//TableForm
```

```txt
 123456789	123456789
1+23456789	23456790
-1+23456789	23456788
12345678+9	12345687
12345678-9	12345669
12+3456789	3456801
1+2+3456789	3456792
-1+2+3456789	3456790
1-2+3456789	3456788
-1-2+3456789	3456786
```


=={{header|Modula-2}}==
```modula2
MODULE SumTo100;
FROM FormatString IMPORT FormatString;
FROM Terminal IMPORT WriteString,WriteLn,ReadChar;

PROCEDURE Evaluate(code : INTEGER) : INTEGER;
VAR
    value,number,power,k : INTEGER;
BEGIN
    value := 0;
    number := 0;
    power := 1;

    FOR k:=9 TO 1 BY -1 DO
        number := power * k + number;
        IF code MOD 3 = 0 THEN
            (* ADD *)
            value := value + number;
            number := 0;
            power := 1
        ELSIF code MOD 3 = 1 THEN
            (* SUB *)
            value := value - number;
            number := 0;
            power := 1
        ELSE
            (* CAT *)
            power := power * 10
        END;
        code := code / 3
    END;

    RETURN value
END Evaluate;

PROCEDURE Print(code : INTEGER);
VAR
    expr,buf : ARRAY[0..63] OF CHAR;
    a,b,k,p : INTEGER;
BEGIN
    a := 19683;
    b := 6561;
    p := 0;

    FOR k:=1 TO 9 DO
        IF (code MOD a) / b = 0 THEN
            IF k > 1 THEN
                expr[p] := '+';
                INC(p)
            END
        ELSIF (code MOD a) / b = 1 THEN
            expr[p] := '-';
            INC(p)
        END;

        a := b;
        b := b / 3;
        expr[p] := CHR(k + 30H);
        INC(p)
    END;
    expr[p] := 0C;

    FormatString("%9i = %s\n", buf, Evaluate(code), expr);
    WriteString(buf)
END Print;

(* Main *)
CONST nexpr = 13122;
VAR
    i,j : INTEGER;
    best,nbest,test,ntest,limit : INTEGER;
    buf : ARRAY[0..63] OF CHAR;
BEGIN
    WriteString("Show all solution that sum to 100");
    WriteLn;
    FOR i:=0 TO nexpr-1 DO
        IF Evaluate(i) = 100 THEN
            Print(i)
        END
    END;
    WriteLn;

    WriteString("Show the sum that has the maximum number of solutions");
    WriteLn;
    nbest := -1;
    FOR i:=0 TO nexpr-1 DO
        test := Evaluate(i);
        IF test > 0 THEN
            ntest := 0;
            FOR j:=0 TO nexpr-1 DO
                IF Evaluate(j) = test THEN
                    INC(ntest)
                END;
                IF ntest > nbest THEN
                    best := test;
                    nbest := ntest
                END
            END
        END
    END;
    FormatString("%i has %i solutions\n\n", buf, best, nbest);
    WriteString(buf);

    WriteString("Show the lowest positive number that can't be expressed");
    WriteLn;
    FOR i:=0 TO 123456789 DO
        FOR j:=0 TO nexpr-1 DO
            IF i = Evaluate(j) THEN
                BREAK
            END
        END;
        IF i # Evaluate(j) THEN
            BREAK
        END
    END;
    FormatString("%i\n\n", buf, i);
    WriteString(buf);

    WriteString("Show the ten highest numbers that can be expressed");
    WriteLn;
    limit := 123456789 + 1;
    FOR i:=1 TO 10 DO
        best := 0;
        FOR j:=0 TO nexpr-1 DO
            test := Evaluate(j);
            IF (test < limit) AND (test > best) THEN
                best := test
            END
        END;
        FOR j:=0 TO nexpr-1 DO
            IF Evaluate(j) = best THEN
                Print(j)
            END
        END;
        limit := best
    END;

    ReadChar
END SumTo100.
```

```txt
Show all solutions that sum to 100
      100 = 1+2+3-4+5+6+78+9
      100 = 1+2+34-5+67-8+9
      100 = 1+23-4+5+6+78-9
      100 = 1+23-4+56+7+8+9
      100 = 12+3+4+5-6-7+89
      100 = 12+3-4+5+67+8+9
      100 = 12-3-4+5-6+7+89
      100 = 123+4-5+67-89
      100 = 123+45-67+8-9
      100 = 123-4-5-6-7+8-9
      100 = 123-45-67+89
      100 = -1+2-3+4+5+6+78+9

Show the sum that has the maximum number of solutions
9 has 46 solutions

Show the lowest positive number that can't be expressed
211

Show the ten highest numbers that can be expressed
123456789 = 123456789
 23456790 = 1+23456789
 23456788 = -1+23456789
 12345687 = 12345678+9
 12345669 = 12345678-9
  3456801 = 12+3456789
  3456792 = 1+2+3456789
  3456790 = -1+2+3456789
  3456788 = 1-2+3456789
  3456786 = -1-2+3456789
```



## Nim


```Nim

import strutils

var
  ligne: string = ""
  sum: int
  opera: array[0..9, int] = [0,0,1,1,1,1,1,1,1,1]
  curseur: int = 9
  boucle: bool
  tot: array[1..123456789, int]
  pG: int
  plusGrandes: array[1..10, string]

let
  ope: array[0..3, string] = ["-",""," +"," -"]
  aAtteindre = 100

proc calcul(li: string): int =
  var liS: seq[string]
  liS = split(li," ")
  for i in liS:
    result += parseInt(i)

echo "Valeur à atteindre : ",aAtteindre

while opera[1]<2:
  ligne.add(ope[opera[1]])
  ligne.add("1")
  for i in 2..9:
    ligne.add(ope[opera[i]])
    ligne.add($i)
  sum = calcul(ligne)
  if sum == aAtteindre:
    stdout.write(ligne)
    echo " = ",sum
  if sum>0:
    tot[sum] += 1
    pG = 1
    while pG<10:
      if sum>calcul(plusGrandes[pG]):
        for k in countdown(10,pG+1):
          plusGrandes[k]=plusGrandes[k-1]
        plusGrandes[pG]=ligne
        pG = 11
      pG += 1
  ligne = ""
  boucle = true
  while boucle:
    opera[curseur] += 1
    if opera[curseur] == 4:
      opera[curseur]=1
      curseur -= 1
    else:
      curseur = 9
      boucle = false

echo "Valeur atteinte ",tot[aAtteindre]," fois."
echo ""

var
  min0: int = 0
  max: int = 0
  valmax: int = 0

for i in 1..123456789:
  if tot[i]==0 and min0 == 0:
    min0 = i
  if tot[i]>max:
    max = tot[i]
    valmax = i

echo "Plus petite valeur ne pouvant pas être atteinte : ",min0
echo "Valeur atteinte le plus souvent : ",valmax,", atteinte ",max," fois."
echo ""
echo "Plus grandes valeurs pouvant être atteintes :"
for i in 1..10:
  echo calcul(plusGrandes[i])," = ",plusGrandes[i]
```

```txt
Valeur à atteindre : 100
-1 +2 -3 +4 +5 +6 +78 +9 = 100
123 +45 -67 +8 -9 = 100
123 +4 -5 +67 -89 = 100
123 -45 -67 +89 = 100
123 -4 -5 -6 -7 +8 -9 = 100
12 +3 +4 +5 -6 -7 +89 = 100
12 +3 -4 +5 +67 +8 +9 = 100
12 -3 -4 +5 -6 +7 +89 = 100
1 +23 -4 +56 +7 +8 +9 = 100
1 +23 -4 +5 +6 +78 -9 = 100
1 +2 +34 -5 +67 -8 +9 = 100
1 +2 +3 -4 +5 +6 +78 +9 = 100
Valeur atteinte 12 fois.

Plus petite valeur ne pouvant pas être atteinte : 211
Valeur atteinte le plus souvent : 9, atteinte 46 fois.

Plus grandes valeurs pouvant être atteintes :
123456789 = 123456789
23456790 = 1 +23456789
23456788 = -1 +23456789
12345687 = 12345678 +9
12345669 = 12345678 -9
3456801 = 12 +3456789
3456792 = 1 +2 +3456789
3456790 = -1 +2 +3456789
3456788 = 1 -2 +3456789
3456786 = -1 -2 +3456789

```



## Pascal

```Pascal
{ RossetaCode: Sum to 100, Pascal.

  Find solutions to the "sum to one hundred" puzzle.

  We don't use arrays, but recompute all values again and again.
  It is a little surprise that the time efficiency is quite acceptable. }

program sumto100;

const
  ADD = 0; SUB = 1; JOIN = 2; { opcodes inserted between digits }
  NEXPR = 13122;              { the total number of expressions }
var
  i, j: integer;
  loop: boolean;
  test, ntest, best, nbest, limit: integer;

  function evaluate(code: integer): integer;
  var
    k: integer;
    value, number, power: integer;
  begin
    value  := 0;
    number := 0;
    power  := 1;
    for  k := 9 downto 1 do
    begin
      number := power * k + number;
      case code mod 3 of
        ADD: begin value := value + number; number := 0; power := 1; end;
        SUB: begin value := value - number; number := 0; power := 1; end;
        JOIN:                                            power := power * 10
      end;
      code := code div 3
    end;
    evaluate := value
  end;

  procedure print(code: integer);
  var
    k: integer;
    a, b: integer;
  begin
    a := 19683;
    b := 6561;
    write( evaluate(code):9 );
    write(' = ');
    for  k := 1 to 9 do
    begin
      case ((code mod a) div b) of
        ADD: if k > 1 then write('+');
        SUB: { always }    write('-');
      end;
      a := b;
      b := b div 3;
      write( k:1 )
    end;
    writeln
  end;

begin
  writeln;
  writeln('Show all solutions that sum to 100');
  writeln;
  for i := 0 to NEXPR - 1 do
    if evaluate(i) = 100 then
      print(i);

  writeln;
  writeln('Show the sum that has the maximum number of solutions');
  writeln;
  nbest := (-1);
  for i := 0 to NEXPR - 1 do
  begin
    test := evaluate(i);
    if test > 0 then
    begin
      ntest := 0;
      for j := 0 to NEXPR - 1 do
        if evaluate(j) = test then
          ntest := ntest + 1;
      if ntest > nbest then
      begin
        best := test;
        nbest := ntest;
      end
    end
  end;
  writeln(best, ' has ', nbest, ' solutions');

  writeln;
  writeln('Show the lowest positive number that can''t be expressed');
  writeln;
  i := 0;
  loop := TRUE;
  while (i <= 123456789) and loop do
  begin
    j := 0;
    while (j < NEXPR - 1) and (i <> evaluate(j)) do
      j := j + 1;
    if i <> evaluate(j) then
      loop := FALSE
    else
      i := i + 1;
  end;
  writeln(i);

  writeln;
  writeln('Show the ten highest numbers that can be expressed');
  writeln;
  limit := 123456789 + 1;
  for i := 1 to 10 do
  begin
    best := 0;
    for j := 0 to NEXPR - 1 do
    begin
      test := evaluate(j);
      if (test < limit) and (test > best) then
        best := test;
    end;
    for j := 0 to NEXPR - 1 do
      if evaluate(j) = best then
        print(j);
    limit := best;
  end
end.
```

```txt
Show all solutions that sum to 100

      100 = 1+2+3-4+5+6+78+9
      100 = 1+2+34-5+67-8+9
      100 = 1+23-4+5+6+78-9
      100 = 1+23-4+56+7+8+9
      100 = 12+3+4+5-6-7+89
      100 = 12+3-4+5+67+8+9
      100 = 12-3-4+5-6+7+89
      100 = 123+4-5+67-89
      100 = 123+45-67+8-9
      100 = 123-4-5-6-7+8-9
      100 = 123-45-67+89
      100 = -1+2-3+4+5+6+78+9

Show the sum that has the maximum number of solutions

9 has 46 solutions

Show the lowest positive number that can't be expressed

211

Show the ten highest numbers that can be expressed

123456789 = 123456789
 23456790 = 1+23456789
 23456788 = -1+23456789
 12345687 = 12345678+9
 12345669 = 12345678-9
  3456801 = 12+3456789
  3456792 = 1+2+3456789
  3456790 = -1+2+3456789
  3456788 = 1-2+3456789
  3456786 = -1-2+3456789

```



## Perl

```perl
#!/usr/bin/perl
use warnings;
use strict;
use feature qw{ say };

my $string = '123456789';
my $length = length $string;
my @possible_ops = ("" , '+', '-');

{
    my @ops;
    sub Next {
        return @ops = (0) x ($length) unless @ops;

        my $i = 0;
        while ($i < $length) {
            if ($ops[$i]++ > $#possible_ops - 1) {
                $ops[$i++] = 0;
                next
            }
            # + before the first number
            next if 0 == $i && '+' eq $possible_ops[ $ops[0] ];

            return @ops
        }
        return
    }
}

sub evaluate {
    my ($expression) = @_;
    my $sum;
    $sum += $_ for $expression =~ /([-+]?[0-9]+)/g;
    return $sum
}

my %count = ( my $max_count = 0 => 0 );

say 'Show all solutions that sum to 100';

while (my @ops = Next()) {
    my $expression = "";
    for my $i (0 .. $length - 1) {
        $expression .= $possible_ops[ $ops[$i] ];
        $expression .= substr $string, $i, 1;
    }
    my $sum = evaluate($expression);
    ++$count{$sum};
    $max_count = $sum if $count{$sum} > $count{$max_count};
    say $expression if 100 == $sum;
}

say 'Show the sum that has the maximum number of solutions';
say "sum: $max_count; solutions: $count{$max_count}";

my $n = 1;
++$n until ! exists $count{$n};
say "Show the lowest positive sum that can't be expressed";
say $n;

say 'Show the ten highest numbers that can be expressed';
say for (sort { $b <=> $a } keys %count)[0 .. 9];
```

```txt
Show all solutions that sum to 100
123-45-67+89
12-3-4+5-6+7+89
12+3+4+5-6-7+89
123+4-5+67-89
-1+2-3+4+5+6+78+9
1+2+3-4+5+6+78+9
12+3-4+5+67+8+9
1+23-4+56+7+8+9
1+2+34-5+67-8+9
1+23-4+5+6+78-9
123+45-67+8-9
123-4-5-6-7+8-9
Show the sum that has the maximum number of solutions
sum: 9; solutions: 46
Show the lowest positive sum that can't be expressed
211
Show the ten highest numbers that can be expressed
123456789
23456790
23456788
12345687
12345669
3456801
3456792
3456790
3456788
3456786
```




### oneliner version

The first task posed can be solved simply with (pay attention to doublequotes around the program: adjust for you OS):

```perl

perl -E "say for grep{eval $_ == 100} glob '{-,}'.join '{+,-,}',1..9"

```


While the whole task can be solved by:

```perl

perl -MList::Util="first" -E "@c[0..10**6]=(0..10**6);say for grep{$e=eval;$c[$e]=undef if $e>=0;$h{$e}++;eval $_==100}glob'{-,}'.join'{+,-,}',1..9;END{say for(sort{$h{$b}<=>$h{$a}}grep{$_>=0}keys %h)[0],first{defined $_}@c;say for(sort{$b<=>$a}grep{$_>0}keys %h)[0..9]}"

```

which outputs

```txt

-1+2-3+4+5+6+78+9
1+2+3-4+5+6+78+9
1+2+34-5+67-8+9
1+23-4+5+6+78-9
1+23-4+56+7+8+9
12+3+4+5-6-7+89
12+3-4+5+67+8+9
12-3-4+5-6+7+89
123+4-5+67-89
123+45-67+8-9
123-4-5-6-7+8-9
123-45-67+89
9
211
123456789
23456790
23456788
12345687
12345669
3456801
3456792
3456790
3456788
3456786

```



## Perl 6

```perl6
my $sum = 100;
my $N   = 10;
my @ops = ['-', ''], |( [' + ', ' - ', ''] xx 8 );
my @str = [X~] map { .Slip }, ( @ops Z 1..9 );
my %sol = @str.classify: *.subst( ' - ', ' -', :g )\
                          .subst( ' + ',  ' ', :g ).words.sum;

my %count.push: %sol.map({ .value.elems => .key });

my $max-solutions    = %count.max( + *.key );
my $first-unsolvable = first { %sol{$_} :!exists }, 1..*;
sub n-largest-sums (Int $n) { %sol.sort(-*.key)[^$n].fmt: "%8s => %s\n" }

given %sol{$sum}:p {
    say "{.value.elems} solutions for sum {.key}:";
    say "    $_" for .value.list;
}

.say for :$max-solutions, :$first-unsolvable, "$N largest sums:", n-largest-sums($N);
```

```txt
12 solutions for sum 100:
    -1 + 2 - 3 + 4 + 5 + 6 + 78 + 9
    1 + 2 + 3 - 4 + 5 + 6 + 78 + 9
    1 + 2 + 34 - 5 + 67 - 8 + 9
    1 + 23 - 4 + 5 + 6 + 78 - 9
    1 + 23 - 4 + 56 + 7 + 8 + 9
    12 + 3 + 4 + 5 - 6 - 7 + 89
    12 + 3 - 4 + 5 + 67 + 8 + 9
    12 - 3 - 4 + 5 - 6 + 7 + 89
    123 + 4 - 5 + 67 - 89
    123 + 45 - 67 + 8 - 9
    123 - 4 - 5 - 6 - 7 + 8 - 9
    123 - 45 - 67 + 89
max-solutions => 46 => [-9 9]
first-unsolvable => 211
10 largest sums:
123456789 => 123456789
 23456790 => 1 + 23456789
 23456788 => -1 + 23456789
 12345687 => 12345678 + 9
 12345669 => 12345678 - 9
  3456801 => 12 + 3456789
  3456792 => 1 + 2 + 3456789
  3456790 => -1 + 2 + 3456789
  3456788 => 1 - 2 + 3456789
  3456786 => -1 - 2 + 3456789
```



## Phix

This is just a trivial count in base 3, with a leading '+' being irrelevant, so from 0(3)000_000_000 to 0(3)122_222_222 which is only (in decimal) 13,122 ...

Admittedly, categorising them into 3429 bins is slightly more effort, but otherwise I am somewhat bemused by all the applescript/javascript/Haskell shenanegins.


```Phix
enum SUB=-1, NOP=0, ADD=1

function eval(sequence s)
integer res = 0, this = 0, op = ADD
    for i=1 to length(s) do
        if s[i]=NOP then
            this = this*10+i
        else
            res += op*this
            this = i
            op = s[i]
        end if
    end for
    return res + op*this
end function

procedure show(sequence s)
string res = ""
    for i=1 to length(s) do
        if s[i]!=NOP then
            res &= ','-s[i]
        end if
        res &= '0'+i
    end for
    puts(1,res&" = ")
end procedure

-- Logically this intersperses -/nop/+ between each digit, but you do not actually need the digit.
sequence s = repeat(SUB,9)  -- (==> ..nop+add*8)

bool done = false
integer maxl = 0, maxr
integer count = 0
while not done do
    count += 1
    integer r = eval(s), k = getd_index(r)
    sequence solns = iff(k=0?{s}:append(getd_by_index(k),s))
    setd(r,solns)
    if r>0 and maxl<length(solns) then
        maxl = length(solns)
        maxr = r
    end if
    for i=length(s) to 1 by -1 do
        if i=1 and s[i]=NOP then
            done = true
            exit
        elsif s[i]!=ADD then
            s[i] += 1
            exit
        end if
        s[i] = SUB
    end for
end while

printf(1,"%d solutions considered (dictionary size: %d)\n",{count,dict_size()})

sequence s100 = getd(100)
printf(1,"There are %d sums to 100:\n",{length(s100)})
for i=1 to length(s100) do
    show(s100[i])
    ?100
end for

printf(1,"The positive sum of %d has the maximum number of solutions: %d\n",{maxr,maxl})

integer prev = 0
function missing(integer key, sequence /*data*/, integer /*pkey*/, object /*user_data=-2*/)
    if key!=prev+1 then
        return 0
    end if
    prev = key
    return 1
end function
traverse_dict_partial_key(routine_id("missing"),1)
printf(1,"The lowest positive sum that cannot be expressed: %d\n",{prev+1})

sequence highest = {}
function top10(integer key, sequence /*data*/, object /*user_data*/)
    highest &= key
    return length(highest)<10
end function
traverse_dict(routine_id("top10"),rev:=1)
printf(1,"The 10 highest sums: ") ?highest
```

```txt

13122 solutions considered (dictionary size: 3429)
There are 12 sums to 100:
-1+2-3+4+5+6+78+9 = 100
12-3-4+5-6+7+89 = 100
123-4-5-6-7+8-9 = 100
123-45-67+89 = 100
123+4-5+67-89 = 100
123+45-67+8-9 = 100
12+3-4+5+67+8+9 = 100
12+3+4+5-6-7+89 = 100
1+23-4+56+7+8+9 = 100
1+23-4+5+6+78-9 = 100
1+2+3-4+5+6+78+9 = 100
1+2+34-5+67-8+9 = 100
The positive sum of 9 has the maximum number of solutions: 46
The lowest positive sum that cannot be expressed: 211
The 10 highest sums: {123456789,23456790,23456788,12345687,12345669,3456801,3456792,3456790,3456788,3456786}

```



## Python



```python
from itertools import product, islice


def expr(p):
    return "{}1{}2{}3{}4{}5{}6{}7{}8{}9".format(*p)


def gen_expr():
    op = ['+', '-', '']
    return [expr(p) for p in product(op, repeat=9) if p[0] != '+']


def all_exprs():
    values = {}
    for expr in gen_expr():
        val = eval(expr)
        if val not in values:
            values[val] = 1
        else:
            values[val] += 1
    return values


def sum_to(val):
    for s in filter(lambda x: x[0] == val, map(lambda x: (eval(x), x), gen_expr())):
        print(s)


def max_solve():
    print("Sum {} has the maximum number of solutions: {}".
          format(*max(all_exprs().items(), key=lambda x: x[1])))


def min_solve():
    values = all_exprs()
    for i in range(123456789):
        if i not in values:
            print("Lowest positive sum that can't be expressed: {}".format(i))
            return


def highest_sums(n=10):
    sums = map(lambda x: x[0],
               islice(sorted(all_exprs().items(), key=lambda x: x[0], reverse=True), n))
    print("Highest Sums: {}".format(list(sums)))


sum_to(100)
max_solve()
min_solve()
highest_sums()
```


```txt
(100, '-1+2-3+4+5+6+78+9')
(100, '1+2+3-4+5+6+78+9')
(100, '1+2+34-5+67-8+9')
(100, '1+23-4+5+6+78-9')
(100, '1+23-4+56+7+8+9')
(100, '12+3+4+5-6-7+89')
(100, '12+3-4+5+67+8+9')
(100, '12-3-4+5-6+7+89')
(100, '123+4-5+67-89')
(100, '123+45-67+8-9')
(100, '123-4-5-6-7+8-9')
(100, '123-45-67+89')
Sum 9 has the maximum number of solutions: 46
Lowest positive sum that can't be expressed: 211
Highest Sums: [123456789, 23456790, 23456788, 12345687, 12345669, 3456801, 3456792, 3456790, 3456788, 3456786]
```



###  Alternate solution

Mostly the same algorithm, but both shorter and faster.


```python
import itertools
from collections import defaultdict, Counter

s = "123456789"
h = defaultdict(list)
for v in itertools.product(["+", "-", ""], repeat=9):
    if v[0] != "+":
        e = "".join("".join(u) for u in zip(v, s))
        h[eval(e)].append(e)

print("Solutions for 100")
for e in h[100]:
    print(e)

c = Counter({k: len(v) for k, v in h.items() if k >= 0})

k, m = c.most_common(1)[0]
print("Maximum number of solutions for %d (%d solutions)" % (k, m))

v = sorted(c.keys())

for i in range(v[-1]):
    if i not in c:
        print("Lowest impossible sum: %d" % i)
        break

print("Ten highest sums")
for k in reversed(v[-10:]):
    print(k)
```


```txt
Solutions for 100
-1+2-3+4+5+6+78+9
1+2+3-4+5+6+78+9
1+2+34-5+67-8+9
1+23-4+5+6+78-9
1+23-4+56+7+8+9
12+3+4+5-6-7+89
12+3-4+5+67+8+9
12-3-4+5-6+7+89
123+4-5+67-89
123+45-67+8-9
123-4-5-6-7+8-9
123-45-67+89
Maximum number of solutions for 9 (46 solutions)
Lowest impossible sum: 211
Ten highest sums
123456789
23456790
23456788
12345687
12345669
3456801
3456792
3456790
3456788
3456786
```



## Racket



```racket
#lang racket

(define list-partitions
  (match-lambda
    [(list) (list null)]
    [(and L (list _)) (list (list L))]
    [(list L ...)
     (for*/list
          ((i (in-range 1 (add1 (length L))))
           (r (in-list (list-partitions (drop L i)))))
        (cons (take L i) r))]))

(define digits->number (curry foldl (λ (dgt acc) (+ (* 10 acc) dgt)) 0))

(define partition-digits-to-numbers
  (let ((memo (make-hash)))
    (λ (dgts)
      (hash-ref! memo dgts
                 (λ ()
                   (map (λ (p) (map digits->number p))
                        (list-partitions dgts)))))))

(define (fold-sum-to-ns digits kons k0)
  (define (get-solutions nmbrs acc chain k)
    (match nmbrs
      [(list)
       (kons (cons acc (let ((niahc (reverse chain)))
                         (if (eq? '+ (car niahc)) (cdr niahc) niahc)))
             k)]
      [(cons a d)
       (get-solutions d (- acc a) (list* a '- chain)
                      (get-solutions d (+ acc a) (list* a '+ chain) k))]))
  (foldl (λ (nmbrs k) (get-solutions nmbrs 0 null k)) k0 (partition-digits-to-numbers digits)))

(define sum-to-ns/hash-promise
  (delay (fold-sum-to-ns
          '(1 2 3 4 5 6 7 8 9)
          (λ (a.s d) (hash-update d (car a.s) (λ (x) (cons (cdr a.s) x)) list))
          (hash))))

(module+ main
  (define S (force sum-to-ns/hash-promise))
  (displayln "Show all solutions that sum to 100")
  (pretty-print (hash-ref S 100))

  (displayln "Show the sum that has the maximum number of solutions (from zero to infinity*)")
  (let-values (([k-max v-max]
                (for/fold ((k-max #f) (v-max 0))
                          (([k v] (in-hash S)) #:when (> (length v) v-max))
                  (values k (length v)))))
    (printf "~a has ~a solutions~%" k-max v-max))

  (displayln "Show the lowest positive sum that can't be expressed (has no solutions),
 using the rules for this task")
  (for/first ((n (in-range 1 (add1 123456789))) #:unless (hash-has-key? S n)) n)

  (displayln "Show the ten highest numbers that can be expressed using the rules for this task")
  (take (sort (hash-keys S) >) 10))

(module+ test
  (require rackunit)
  (check-equal? (list-partitions null) '(()))
  (check-equal? (list-partitions '(1)) '(((1))))
  (check-equal? (list-partitions '(1 2)) '(((1) (2)) ((1 2))))
  (check-equal? (partition-digits-to-numbers '()) '(()))
  (check-equal? (partition-digits-to-numbers '(1)) '((1)))
  (check-equal? (partition-digits-to-numbers '(1 2)) '((1 2) (12))))
```


```txt
Show all solutions that sum to 100
'((123 - 45 - 67 + 89)
  (123 + 45 - 67 + 8 - 9)
  (123 + 4 - 5 + 67 - 89)
  (123 - 4 - 5 - 6 - 7 + 8 - 9)
  (12 + 3 - 4 + 5 + 67 + 8 + 9)
  (12 - 3 - 4 + 5 - 6 + 7 + 89)
  (12 + 3 + 4 + 5 - 6 - 7 + 89)
  (1 + 23 - 4 + 56 + 7 + 8 + 9)
  (1 + 23 - 4 + 5 + 6 + 78 - 9)
  (1 + 2 + 34 - 5 + 67 - 8 + 9)
  (- 1 + 2 - 3 + 4 + 5 + 6 + 78 + 9)
  (1 + 2 + 3 - 4 + 5 + 6 + 78 + 9))
Show the sum that has the maximum number of solutions (from zero to infinity*)
9 has 46 solutions
Show the lowest positive sum that can't be expressed (has no solutions),
 using the rules for this task
211
Show the ten highest numbers that can be expressed using the rules for this task
'(123456789 23456790 23456788 12345687 12345669 3456801 3456792 3456790 3456788 3456786)
```



## REXX


```rexx
/*REXX pgm solves a puzzle:  using the string 123456789, insert  -  or  +  to sum to 100*/
parse arg LO HI .                                /*obtain optional arguments from the CL*/
if LO=='' | LO==","  then LO=100                 /*Not specified?  Then use the default.*/
if HI=='' | HI==","  then HI=LO                  /* "      "         "   "   "     "    */
if LO==00            then HI=123456789           /*LOW specified as zero with leading 0.*/
ops= '+-';             L=length(ops) + 1         /*define operators (and their length). */
@.=;        do i=1  to L-1;  @.i=substr(ops,i,1) /*   "   some handy-dandy REXX literals*/
            end   /*i*/                          /*   "   individual operators for speed*/
mx=0;  mn=999999                                 /*initialize the minimums and maximums.*/
mxL=;  mnL=;       do j=LO  to HI  until LO==00  &  mn==0   /*solve with a range of sums*/
                   z=solve(j)                               /*find # of solutions for J.*/
                   if z> mx  then mxL=                      /*see if this is a new max. */
                   if z>=mx  then do; mxL=mxL j; mx=z; end  /*remember this new maximum.*/
                   if z< mn  then mnL=                      /*see if this is a new min. */
                   if z<=mn  then do; mnL=mnL j; mn=z; end  /*remember this new minimum.*/
                   end   /*j*/
if LO==HI then exit                                         /*don't display max & min ? */
@@= 'number of solutions: ';   say
_=words(mxL);  say 'sum's(_)   "of"   mxL  ' 's(_,"have",'has')   'the maximum'    @@   mx
_=words(mnL);  say 'sum's(_)   "of"   mnL  ' 's(_,"have",'has')   'the minimum'    @@   mn
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
s:     if arg(1)==1  then return arg(3);  return word(arg(2) "s",1)  /*simple pluralizer*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
solve: parse arg answer;         # =0            /*obtain the answer (sum) to the puzzle*/
          do a=L-1  to L;        aa=      @.a'1' /*choose one  of  ─       or  nothing. */
           do b=1  for L;        bb=aa || @.b'2' /*   "    "    "  ─   +,  or  abutment.*/
            do c=1  for L;       cc=bb || @.c'3' /*   "    "    "  "   "    "      "    */
             do d=1  for L;      dd=cc || @.d'4' /*   "    "    "  "   "    "      "    */
              do e=1  for L;     ee=dd || @.e'5' /*   "    "    "  "   "    "      "    */
               do f=1  for L;    ff=ee || @.f'6' /*   "    "    "  "   "    "      "    */
                do g=1  for L;   gg=ff || @.g'7' /*   "    "    "  "   "    "      "    */
                 do h=1  for L;  hh=gg || @.h'8' /*   "    "    "  "   "    "      "    */
                  do i=1  for L; ii=hh || @.i'9' /*   "    "    "  "   "    "      "    */
                  interpret '$=' ii              /*calculate the sum of modified string.*/
                  if $\==answer  then iterate    /*Is sum not equal to answer? Then skip*/
                  #=#+1;         if LO==HI  then say 'solution: '    $    " ◄───► "     ii
                  end   /*i*/
                 end    /*h*/
                end     /*g*/
               end      /*f*/
              end       /*e*/
             end        /*d*/
            end         /*c*/
           end          /*b*/
          end           /*a*/
       y=#                                       /* [↓]  adjust the number of solutions?*/
       if y==0  then y='no'                      /* [↓]  left justify plural of solution*/
       if LO\==00  then say right(y, 9)           'solution's(#, , " ")   'found for'  ,
                            right(j, length(HI) )                         left('', #, "─")
       return #                                  /*return the number of solutions found.*/
```

```txt

solution:  100  ◄───►  -1+2-3+4+5+6+78+9
solution:  100  ◄───►  1+2+3-4+5+6+78+9
solution:  100  ◄───►  1+2+34-5+67-8+9
solution:  100  ◄───►  1+23-4+5+6+78-9
solution:  100  ◄───►  1+23-4+56+7+8+9
solution:  100  ◄───►  12+3+4+5-6-7+89
solution:  100  ◄───►  12+3-4+5+67+8+9
solution:  100  ◄───►  12-3-4+5-6+7+89
solution:  100  ◄───►  123+4-5+67-89
solution:  100  ◄───►  123+45-67+8-9
solution:  100  ◄───►  123-4-5-6-7+8-9
solution:  100  ◄───►  123-45-67+89
       12 solutions found for 100

```

```txt

sum of  9  has the maximum number of solutions:  46
sum of  211  has the minimum number of solutions:  0

```



## Ruby

```ruby
def gen_expr
  x = ['-', '']
  y = ['+', '-', '']
  x.product(y,y,y,y,y,y,y,y)
   .map do |a,b,c,d,e,f,g,h,i|
      "#{a}1#{b}2#{c}3#{d}4#{e}5#{f}6#{g}7#{h}8#{i}9"
    end
end

def sum_to(val)
  gen_expr.map{|expr| [eval(expr), expr]}.select{|v,expr| v==val}.each{|x| p x}
end

def max_solve
  n,size = gen_expr.group_by{|expr| eval(expr)}
                   .select{|val,_| val>=0}
                   .map{|val,exprs| [val, exprs.size]}
                   .max_by{|_,size| size}
  puts "sum of #{n} has the maximum number of solutions : #{size}"
end

def min_solve
  solves = gen_expr.group_by{|expr| eval(expr)}
  n = 0.step{|i| break i unless solves[i]}
  puts "lowest positive sum that can't be expressed : #{n}"
end

def highest_sums(n=10)
  n = gen_expr.map{|expr| eval(expr)}.uniq.sort.reverse.take(n)
  puts "highest sums : #{n}"
end

sum_to(100)
max_solve
min_solve
highest_sums
```


```txt

[100, "-1+2-3+4+5+6+78+9"]
[100, "1+2+3-4+5+6+78+9"]
[100, "1+2+34-5+67-8+9"]
[100, "1+23-4+5+6+78-9"]
[100, "1+23-4+56+7+8+9"]
[100, "12+3+4+5-6-7+89"]
[100, "12+3-4+5+67+8+9"]
[100, "12-3-4+5-6+7+89"]
[100, "123+4-5+67-89"]
[100, "123+45-67+8-9"]
[100, "123-4-5-6-7+8-9"]
[100, "123-45-67+89"]
sum of 9 has the maximum number of solutions : 46
lowest positive sum that can't be expressed : 211
highest sums : [123456789, 23456790, 23456788, 12345687, 12345669, 3456801, 3456792, 3456790, 3456788, 3456786]

```



## Scala


```scala
object SumTo100 {
  def main(args: Array[String]): Unit = {
    val exps = expressions(9).map(str => (str, eval(str)))
    val sums = exps.map(_._2).sortWith(_>_)

    val s1 = exps.filter(_._2 == 100)
    val s2 = sums.distinct.map(s => (s, sums.count(_ == s))).maxBy(_._2)
    val s3 = sums.distinct.reverse.filter(_>0).zipWithIndex.dropWhile{case (n, i) => n == i + 1}.head._2 + 1
    val s4 = sums.distinct.take(10)

    println(s"""All ${s1.size} solutions that sum to 100:
               |${s1.sortBy(_._1.length).map(p => s"${p._2} = ${p._1.tail}").mkString("\n")}
               |
               |Most common sum: ${s2._1} (${s2._2})
               |Lowest unreachable sum: $s3
               |Highest 10 sums: ${s4.mkString(", ")}""".stripMargin)
  }

  def expressions(l: Int): LazyList[String] = configurations(l).map(p => p.zipWithIndex.map{case (op, n) => s"${opChar(op)}${n + 1}"}.mkString)
  def configurations(l: Int): LazyList[Vector[Int]] = LazyList.range(0, math.pow(3, l).toInt).map(config(l)).filter(_.head != 0)
  def config(l: Int)(num: Int): Vector[Int] = Iterator.iterate((num%3, num/3)){case (_, n) => (n%3, n/3)}.map(_._1 - 1).take(l).toVector

  def eval(exp: String): Int = (exp.headOption, exp.tail.takeWhile(_.isDigit), exp.tail.dropWhile(_.isDigit)) match{
    case (Some(op), n, str) => doOp(op, n.toInt) + eval(str)
    case _ => 0
  }

  def doOp(sel: Char, n: Int): Int = if(sel == '-') -n else n
  def opChar(sel: Int): String = sel match{
    case -1 => "-"
    case 1 => "+"
    case _ => ""
  }
}
```

```txt
All 12 solutions that sum to 100:
100 = 123-45-67+89
100 = 123+45-67+8-9
100 = 123+4-5+67-89
100 = 1+23-4+5+6+78-9
100 = 123-4-5-6-7+8-9
100 = 12+3+4+5-6-7+89
100 = 12-3-4+5-6+7+89
100 = 1+2+34-5+67-8+9
100 = 12+3-4+5+67+8+9
100 = 1+23-4+56+7+8+9
100 = 1+2+3-4+5+6+78+9
100 = 1+2-3+4+5+6+78+9

Most common sum: 9 (46)
Lowest unreachable sum: 211
Highest 10 sums: 123456789, 23456790, 23456788, 12345687, 12345669, 3456801, 3456792, 3456790, 3456788, 3456786
```



## Sidef

```ruby
func gen_expr() is cached {
    var x = ['-', '']
    var y = ['+', '-', '']

    gather {
        cartesian([x,y,y,y,y,y,y,y,y], {|a,b,c,d,e,f,g,h,i|
            take("#{a}1#{b}2#{c}3#{d}4#{e}5#{f}6#{g}7#{h}8#{i}9")
        })
    }
}

func eval_expr(expr) is cached {
    expr.scan(/([-+]?\d+)/).sum_by { Num(_) }
}

func sum_to(val) {
    gen_expr().grep { eval_expr(_) == val }
}

func max_solve() {
    gen_expr().grep     { eval_expr(_) >= 0 } \
              .group_by { eval_expr(_)      } \
              .max_by   {|_,v| v.len        }
}

func min_solve() {
    var h = gen_expr().group_by { eval_expr(_) }
    for i in (0..Inf) { h.exists(i) || return i }
}

func highest_sums(n=10) {
    gen_expr().map { eval_expr(_) }.uniq.sort.reverse.first(n)
}

sum_to(100).each { say "100 = #{_}" }

var (n, solutions) = max_solve()...
say "Sum of #{n} has the maximum number of solutions: #{solutions.len}"
say "Lowest positive sum that can't be expressed : #{min_solve()}"
say "Highest sums: #{highest_sums()}"
```

```txt

100 = -1+2-3+4+5+6+78+9
100 = 1+2+3-4+5+6+78+9
100 = 1+2+34-5+67-8+9
100 = 1+23-4+5+6+78-9
100 = 1+23-4+56+7+8+9
100 = 12+3+4+5-6-7+89
100 = 12+3-4+5+67+8+9
100 = 12-3-4+5-6+7+89
100 = 123+4-5+67-89
100 = 123+45-67+8-9
100 = 123-4-5-6-7+8-9
100 = 123-45-67+89
Sum of 9 has the maximum number of solutions: 46
Lowest positive sum that can't be expressed : 211
Highest sums: [123456789, 23456790, 23456788, 12345687, 12345669, 3456801, 3456792, 3456790, 3456788, 3456786]

```



## Tcl


```Tcl
proc sum_to_100 {} {
    for {set i 0} {$i <= 13121} {incr i} {
	set i3 [format %09d [dec2base 3 $i]]
	set form ""
	set subs {"" - +}
	foreach a [split $i3 ""] b [split 123456789 ""] {
	    append form [lindex $subs $a] $b
	}
	lappend R([expr $form]) $form
    }
    puts "solutions for sum=100:\n[join [lsort $R(100)] \n]"
    set max -1
    foreach key [array names R] {
	if {[llength $R($key)] > $max} {
	    set max [llength $R($key)]
	    set maxkey $key
	}
    }
    puts "max solutions: $max for $maxkey"
    for {set i 0} {$i <= 123456789} {incr i} {
	if ![info exists R($i)] {
	    puts "first unsolvable: $i"
	    break
	}
    }
    puts "highest 10:\n[lrange [lsort -integer -decr [array names R]] 0 9]"
}
proc dec2base {base dec} {
    set res ""
    while {$dec > 0} {
	set res [expr $dec%$base]$res
	set dec [expr $dec/$base]
    }
    if {$res eq ""} {set res 0}
    return $res
}
sum_to_100
```


```txt

~ $ ./sum_to_100.tcl
solutions for sum=100:
-1+2-3+4+5+6+78+9
1+2+3-4+5+6+78+9
1+2+34-5+67-8+9
1+23-4+5+6+78-9
1+23-4+56+7+8+9
12+3+4+5-6-7+89
12+3-4+5+67+8+9
12-3-4+5-6+7+89
123+4-5+67-89
123+45-67+8-9
123-4-5-6-7+8-9
123-45-67+89
max solutions: 46 for 9
first unsolvable: 211
highest 10:
123456789 23456790 23456788 12345687 12345669 3456801 3456792 3456790 3456788 3456786

```



## Visual Basic .NET

Of course, one could just code-convert the existing C# example, but I thought this could be written with some simpler constructs.  The point of doing this is to make the code more compatible with other BASIC languages.  Not every language has something similar to the <i>Enumerable Range</i> construct.  I also found the <i>Dictionary</i> construct could be implemented with something more primitive.

Another interesting thing this program can do is solve for other sets of numbers easily, as neither the number of digits, nor the digit sequence itself, is hard-coded.  You could solve for the digits 1 through 8, for example, or the digits starting at 9 and going down to 1.  One can even override the target sum (of 100) parameter, if you happen to be interested in another number.

```vbnet
' Recursively iterates (increments) iteration array, returns -1 when out of "digits".
Function plusOne(iAry() As Integer, spot As Integer) As Integer
    Dim spotLim As Integer = If(spot = 0, 1, 2) ' The first "digit" has a lower limit.
    If iAry(spot) = spotLim Then ' Check if spot has reached limit
        If spot = 0 Then Return -1 ' No previous spot to increment, so indicate completed.
        iAry(spot) = 0 ' Reset current spot, and
        Return plusOne(iAry, spot - 1) ' Increment previous spot.
    Else
        iAry(spot) += 1 ' Increment current spot.
    End If
    Return spot
End Function

' Returns string sequence of operations from iAry and terms string
Function generate(iAry() As Integer, terms As String) As String
    Dim operations As String() = {"", "-", "+"} ' Possible operations.
    generate = ""
    For i As Integer = 0 To iAry.Count - 1
        generate &= operations(iAry(i)) & Mid(terms, i + 1, 1).ToString()
    Next
End Function

' Returns evaluation of string sequence
Function eval(sequence As String) As Integer
    eval = 0
    Dim term As Integer = 0, operation As Integer = 1
    For Each ch As Char In sequence
        Select Case ch
            Case "-", "+" ' New operation detected, apply previous operation to term,
                eval += If(operation = 0, -term, term) : term = 0 ' and reset term.
                operation = If(ch = "-", 0, 1) ' Note next operation.
            Case Else ' Digit detected, increase term.
                term = term * 10 + Val(ch)
        End Select
    Next
    eval += If(operation = 0, -term, term) ' Apply final term.
End Function

' Sorts a pair of List(Of Integer) by the first
Sub reSort(ByRef first As List(Of Integer), ByRef second As List(Of Integer))
    Dim lou As New List(Of ULong) ' Temporary list of ULong for sorting.
    For i As Integer = 0 To first.Count - 1
        lou.Add((CULng(first(i)) << 32) + second(i)) ' "Pack" list items.
    Next
    lou.Sort()
    For k As Integer = 0 To first.Count - 1
        first(k) = lou(k) >> 32 ' "Unpack" first list item.
        second(k) = lou(k) And &H7FFFFFFF ' "Unpack" second list item.
    Next
End Sub

' Returns first result not in sequence, assumes passed list is sorted before call,
'  uses binary search algo.
Function firstMiss(loi As List(Of Integer))
    Dim low As Integer = 0, high As Integer = loi.Count - 1, middle = (low + high) \ 2
    Do
        If loi(middle) = middle Then low = middle + 1 Else high = middle - 1
        middle = (low + high) \ 2
    Loop Until high <= low
    Return middle + If(loi(middle) = middle, 1, 0)
End Function

' Iterates through all possible operations,
'  uses a pair of List (of Integer) to tabulate solutions.
Sub Solve100(Optional terms As String = "123456789",
             Optional targSum As Integer = 100,
             Optional highNums As Integer = 10)
    Dim lastDig As Integer = Len(terms) - 1 ' The final "digit".
    Dim iAry() As Integer = New Integer(lastDig) {} ' Iterations array.
    Dim seq As String ' Sequence of numbers and operations.
    Dim sVal As Integer ' Sequence value.
    Dim sCnt As Integer = 1 ' Solution count (targSum).
    Dim res As New List(Of Integer) ' List of results.
    Dim tally As New List(Of Integer) ' Tally of results.
    Console.WriteLine("List of solutions that evaluate to 100:")
    Do ' Tabulate results until digits are exhausted.
        seq = generate(iAry, terms) ' Obtain next expression.
        sVal = eval(seq) ' Obtain next evaluation.
        If sVal >= 0 Then ' Don't bother saving the negative results.
            If res.Contains(sVal) Then tally(res.IndexOf(sVal)) += 1 _
                                  Else res.Add(sVal) : tally.Add(1)
            If sVal = targSum Then _
                Console.WriteLine(" {0,2} {1}", sCnt, seq) : sCnt += 1
        End If
    Loop Until plusOne(iAry, lastDig) < 0
    reSort(tally, res) ' Sort by tally to find result with the most solutions.
    Console.WriteLine("The sum that has the the most solutions is {0}, (at {1}).",
                      res.Last, tally.Last)
    reSort(res, tally) ' Sort by result to find first missing result and top results.
    Console.WriteLine("The lowest positive sum that can't be expressed is {0}.",
                      firstMiss(res))
    Console.WriteLine("The ten highest numbers that can be expressed are:")
    res.Reverse() ' To let us take the last items for output.
    sCnt = 0 ' Keep track of items displayed (for formatting).
    For Each item As Integer In res.Take(highNums)
        Console.Write("{0, -11}", item)
        sCnt = (sCnt + 1) Mod 5 : If sCnt = 0 Then Console.WriteLine()
    Next
End Sub

Sub Main()
    Solve100() ' if interested, try this: Solve100("987654321")
End Sub
```

```txt
List of solutions that evaluate to 100:
  1 123-45-67+89
  2 123-4-5-6-7+8-9
  3 123+45-67+8-9
  4 123+4-5+67-89
  5 12-3-4+5-6+7+89
  6 12+3-4+5+67+8+9
  7 12+3+4+5-6-7+89
  8 1+23-4+56+7+8+9
  9 1+23-4+5+6+78-9
 10 1+2+34-5+67-8+9
 11 1+2+3-4+5+6+78+9
 12 -1+2-3+4+5+6+78+9
The sum that has the the most solutions is 9, (at 46).
The lowest positive sum that can't be expressed is 211.
The ten highest numbers that can be expressed are:
123456789  23456790   23456788   12345687   12345669
3456801    3456792    3456790    3456788    3456786
```



## zkl

Taking a big clue from Haskell and just calculate the world.

```zkl
var all =  // ( (1,12,123...-1,-12,...), (2,23,...) ...)
   (9).pump(List,fcn(n){ split("123456789"[n,*]) })       // 45
   .apply(fcn(ns){ ns.extend(ns.copy().apply('*(-1))) }); // 90
fcn calcAllSums{  // calculate all 6572 sums (1715 unique)
   fcn(n,sum,soFar,r){
      if(n==9) return();
      foreach b in (all[n]){
	 if(sum+b>=0 and b.abs()%10==9) r.appendV(sum+b,"%s%+d".fmt(soFar,b));
	 self.fcn(b.abs()%10,sum + b,"%s%+d".fmt(soFar,b),r);
      }
   }(0,0,"",r:=Dictionary());
   r
}
    // "123" --> (1,12,123)
fcn split(nstr){ (1).pump(nstr.len(),List,nstr.get.fp(0),"toInt") }
```


```zkl
fcn showSums(allSums,N=100,printSolutions=2){
   slns:=allSums.find(N,T);
   if(printSolutions)    println("%d solutions for N=%d".fmt(slns.len(),N));
   if(printSolutions==2) println(slns.concat("\n"));
   println();
}

allSums:=calcAllSums();
showSums(allSums);
showSums(allSums,0,1);

println("Smallest postive integer with no solution: ",
   [1..].filter1('wrap(n){ Void==allSums.find(n) }));

println("5 commonest sums (sum, number of ways to calculate to it):");
ms:=allSums.values.apply("len").sort()[-5,*];	        // 5 mostest sums
allSums.pump(List,					// get those pairs
   'wrap([(k,v)]){ v=v.len(); ms.holds(v) and T(k.toInt(),v) or Void.Skip })
.sort(fcn(kv1,kv2){ kv1[1]>kv2[1] })			// and sort
.println();
```

```txt

12 solutions for N=100
+1+2+3-4+5+6+78+9
+1+2+34-5+67-8+9
+1+23-4+5+6+78-9
+1+23-4+56+7+8+9
+12+3+4+5-6-7+89
+12+3-4+5+67+8+9
+12-3-4+5-6+7+89
+123+4-5+67-89
+123+45-67+8-9
+123-4-5-6-7+8-9
+123-45-67+89
-1+2-3+4+5+6+78+9

22 solutions for N=0

Smallest postive integer with no solution: 211

5 commonest sums (sum, number of ways to calculate to it):
L(L(9,46),L(27,44),L(15,43),L(1,43),L(21,43))

```

