+++
title = "Floyd's triangle"
description = ""
date = 2019-10-10T07:24:58Z
aliases = []
[extra]
id = 11906
[taxonomies]
categories = []
tags = []
+++

{{task}}

[[wp:Floyd's triangle|Floyd's triangle]]   lists the natural numbers in a right triangle aligned to the left where 
* the first row is   '''1'''     (unity)
* successive rows start towards the left with the next number followed by successive naturals listing one more number than the line above.


The first few lines of a Floyd triangle looks like this:

```txt

 1
 2  3
 4  5  6
 7  8  9 10
11 12 13 14 15

```



;Task:
:# Write a program to generate and display here the first   n   lines of a Floyd triangle. 
(Use   n=5   and   n=14   rows).
:# Ensure that when displayed in a mono-space font, the numbers line up in vertical columns as shown and that only one space separates numbers of the last row.





## 360 Assembly

A very concise coding, an illustration of CISC power of the S/360 operation codes. Also an example of the use of EDMK and EX instructions. 
For macro usage see [[360_Assembly_macros#360_Assembly_Structured_Macros|Structured Macros]] .

```360asm
*        Floyd's triangle          21/06/2018
FLOYDTRI PROLOG
         L      R5,NN              nn
         BCTR   R5,0               -1
         M      R4,NN              nn*(nn-1)
         SRA    R5,1               /2
         A      R5,NN              m=(nn*(nn-1))/2+nn; max_value
         CVD    R5,XDEC            binary to packed decimal (PL8)
         EDMK   ZN,XDEC+4          packed dec (PL4) to char (CL8)
         S      R1,=A(ZN)          r1=number of spaces
         L      R9,=A(L'ZN+1)      length(zn08)+1
         SR     R9,R1              s=length(m)+1
         SR     R8,R8              k=0
         LA     R6,1               i=1
       DO WHILE=(C,R6,LE,NN)       do i=1 to nn
         LA     R10,PG               pgi=0
         LA     R7,1                 j=1
       DO WHILE=(CR,R7,LE,R6)          do j=1 to i
         LA     R8,1(R8)               k=k+1
         XDECO  R8,XDEC                k
         LA     R11,XDEC+12            +12
         SR     R11,R9                 -s
         LR     R2,R9                  s
         BCTR   R2,0                   -1
         EX     R2,MVCX                mvc @PG+pgi,@XDEC+12-s,LEN=s
         AR     R10,R9                 pgi+=s
         LA     R7,1(R7)               j++
       ENDDO    ,                    enddo j
         XPRNT  PG,L'PG              print buffer
         LA     R6,1(R6)             i++
       ENDDO    ,                  enddo i
         EPILOG
MVCX     MVC    0(0,R10),0(R11)    mvc PG,XDEC
NN       DC     F'14'              number of rows
PG       DC     CL80' '            buffer
XDEC     DS     CL12               temp
ZN       DC     X'4020202020202020'  mask CL8 7num
         YREGS
         END    FLOYDTRI
```

{{out}}

```txt

  1
  2  3
  4  5  6
  7  8  9 10
 11 12 13 14 15

```

{{out}}

```txt

   1
   2   3
   4   5   6
   7   8   9  10
  11  12  13  14  15
  16  17  18  19  20  21
  22  23  24  25  26  27  28
  29  30  31  32  33  34  35  36
  37  38  39  40  41  42  43  44  45
  46  47  48  49  50  51  52  53  54  55
  56  57  58  59  60  61  62  63  64  65  66
  67  68  69  70  71  72  73  74  75  76  77  78
  79  80  81  82  83  84  85  86  87  88  89  90  91
  92  93  94  95  96  97  98  99 100 101 102 103 104 105

```



## Ada



```Ada

with Ada.Text_IO, Ada.Integer_Text_IO, Ada.Command_Line;

procedure Floyd_Triangle is
  rows : constant Natural := Natural'Value(Ada.Command_Line.Argument(1));
begin
  for r in 1..rows loop
    for i in 1..r loop 
      Ada.Integer_Text_IO.put (r*(r-1)/2+i, Width=> Natural'Image(rows*(rows-1)/2+i)'Length); 
    end loop;
    Ada.Text_IO.New_Line;
  end loop;
end Floyd_Triangle;

```

{{out}}


```txt
> ./floyd_triangle_triangle 5
  1
  2  3
  4  5  6
  7  8  9 10
 11 12 13 14 15


> ./floyd_triangle 14
  1
  2  3
  4  5  6
  7  8  9 10
 11 12 13 14 15
 16 17 18 19 20 21
 22 23 24 25 26 27 28
 29 30 31 32 33 34 35 36
 37 38 39 40 41 42 43 44  45
 46 47 48 49 50 51 52 53  54  55
 56 57 58 59 60 61 62 63  64  65  66
 67 68 69 70 71 72 73 74  75  76  77  78
 79 80 81 82 83 84 85 86  87  88  89  90  91
 92 93 94 95 96 97 98 99 100 101 102 103 104 105

```



## ALGOL 68

{{works with|ALGOL 68G|Any - tested with release 2.8.win32}}

```algol68
# procedure to print a Floyd's Triangle with n lines                      #
PROC floyds triangle = ( INT n )VOID:
BEGIN

    # calculate the number of the highest number that will be printed     #
    # ( the sum of the integers 1, 2, ... n )                             #
    INT max number = ( n * ( n + 1 ) ) OVER 2;

    # determine the widths required to print the numbers of the final row #
    [ n ]INT widths;
    INT number := max number + 1;
    FOR col FROM n BY -1 TO 1 DO
        widths[ col ] := - ( UPB whole( number -:= 1, 0 ) + 1 )
    OD;

    # print the triangle                                                  #
    INT element := 0;
    FOR row TO n DO
        FOR col TO row DO
            print( ( whole( element +:= 1, widths[ col ] ) ) )
        OD;
        print( ( newline ) )
    OD

END; # floyds triangle #

main: (

    floyds triangle(  5 );
    print( ( newline ) );
    floyds triangle( 14 )

)
```

{{out}}

```txt

  1
  2  3
  4  5  6
  7  8  9 10
 11 12 13 14 15

  1
  2  3
  4  5  6
  7  8  9 10
 11 12 13 14 15
 16 17 18 19 20 21
 22 23 24 25 26 27 28
 29 30 31 32 33 34 35 36
 37 38 39 40 41 42 43 44  45
 46 47 48 49 50 51 52 53  54  55
 56 57 58 59 60 61 62 63  64  65  66
 67 68 69 70 71 72 73 74  75  76  77  78
 79 80 81 82 83 84 85 86  87  88  89  90  91
 92 93 94 95 96 97 98 99 100 101 102 103 104 105

```



## ALGOL W

{{trans| ALgOL_68}}

```algolw
begin
    % prints a Floyd's Triangle with n lines                                  %
    procedure floydsTriangle ( integer value n ) ;
    begin
        % the triangle should be left aligned with the individual numbers     %
        % right-aligned with only one space before the number in the final    %
        % row                                                                 %
        % calculate the highest number that will be printed                   %
        % ( the sum of the integeregers 1, 2, ... n )                         %
        integer array widths( 1 :: n );
        integer maxNumber, number;
        maxNumber := ( n * ( n + 1 ) ) div 2;
        % determine the widths required to print the numbers of the final row %
        number := maxNumber;
        for col := n step -1 until 1 do begin
            integer v, w;
            w      := 0;
            v      := number;
            number := number - 1;
            while v > 0 do begin
                w  := w + 1;
                v  := v div 10
            end while_v_gt_0 ;
            widths( col ) := w
        end for_col;
        % print the triangle                                                  %
        number := 0;
        for row := 1 until n do begin
            for col := 1 until row do begin
                number := number + 1;
                writeon( i_w := widths( col ), s_w := 0, " ", number )
            end for_col ;
            write()
        end for_row 
    end; % floyds triangle %

    floydsTriangle(  5 );
    write();
    floydsTriangle( 14 )

end.
```

{{out}}

```txt

  1
  2  3
  4  5  6
  7  8  9 10
 11 12 13 14 15

  1
  2  3
  4  5  6
  7  8  9 10
 11 12 13 14 15
 16 17 18 19 20 21
 22 23 24 25 26 27 28
 29 30 31 32 33 34 35 36
 37 38 39 40 41 42 43 44  45
 46 47 48 49 50 51 52 53  54  55
 56 57 58 59 60 61 62 63  64  65  66
 67 68 69 70 71 72 73 74  75  76  77  78
 79 80 81 82 83 84 85 86  87  88  89  90  91
 92 93 94 95 96 97 98 99 100 101 102 103 104 105

```



## AppleScript

{{Trans|JavaScript}}
{{Trans|Haskell}} (mapAccumL versions)

```AppleScript
-- FLOYDs TRIANGLE -----------------------------------------------------------

-- floyd :: Int -> [[Int]]
on floyd(n)
    script floydRow
        on |λ|(start, row)
            {start + row + 1, enumFromTo(start, start + row)}
        end |λ|
    end script
    
    snd(mapAccumL(floydRow, 1, enumFromTo(0, n - 1)))
end floyd

-- showFloyd :: [[Int]] -> String
on showFloyd(xss)
    set ws to map(compose({my succ, my |length|, my show}), |last|(xss))
    
    script aligned
        on |λ|(xs)
            script pad
                on |λ|(w, x)
                    justifyRight(w, space, show(x))
                end |λ|
            end script
            
            concat(zipWith(pad, ws, xs))
        end |λ|
    end script
    
    unlines(map(aligned, xss))
end showFloyd


-- TEST ----------------------------------------------------------------------
on run
    script test
        on |λ|(n)
            showFloyd(floyd(n)) & linefeed
        end |λ|
    end script
    
    unlines(map(test, {5, 14}))
end run


-- GENERIC FUNCTIONS ---------------------------------------------------------

-- compose :: [(a -> a)] -> (a -> a)
on compose(fs)
    script
        on |λ|(x)
            script
                on |λ|(f, a)
                    mReturn(f)'s |λ|(a)
                end |λ|
            end script
            
            foldr(result, x, fs)
        end |λ|
    end script
end compose

-- concat :: [[a]] -> [a] | [String] -> String
on concat(xs)
    if length of xs > 0 and class of (item 1 of xs) is string then
        set acc to ""
    else
        set acc to {}
    end if
    repeat with i from 1 to length of xs
        set acc to acc & item i of xs
    end repeat
    acc
end concat

-- enumFromTo :: Int -> Int -> [Int]
on enumFromTo(m, n)
    if n < m then
        set d to -1
    else
        set d to 1
    end if
    set lst to {}
    repeat with i from m to n by d
        set end of lst to i
    end repeat
    return lst
end enumFromTo

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

-- foldr :: (b -> a -> a) -> a -> [b] -> a
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

-- intercalate :: Text -> [Text] -> Text
on intercalate(strText, lstText)
    set {dlm, my text item delimiters} to {my text item delimiters, strText}
    set strJoined to lstText as text
    set my text item delimiters to dlm
    return strJoined
end intercalate

-- justifyRight :: Int -> Char -> Text -> Text
on justifyRight(n, cFiller, strText)
    if n > length of strText then
        text -n thru -1 of ((replicate(n, cFiller) as text) & strText)
    else
        strText
    end if
end justifyRight

-- last :: [a] -> a
on |last|(xs)
    if length of xs > 0 then
        item -1 of xs
    else
        missing value
    end if
end |last|

-- length :: [a] -> Int
on |length|(xs)
    length of xs
end |length|

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

-- 'The mapAccumL function behaves like a combination of map and foldl;
-- it applies a function to each element of a list, passing an
-- accumulating parameter from left to right, and returning a final
-- value of this accumulator together with the new list.' (see Hoogle)

-- mapAccumL :: (acc -> x -> (acc, y)) -> acc -> [x] -> (acc, [y])
on mapAccumL(f, acc, xs)
    script
        on |λ|(a, x)
            tell mReturn(f) to set pair to |λ|(item 1 of a, x)
            [item 1 of pair, (item 2 of a) & {item 2 of pair}]
        end |λ|
    end script
    
    foldl(result, [acc, []], xs)
end mapAccumL

-- min :: Ord a => a -> a -> a
on min(x, y)
    if y < x then
        y
    else
        x
    end if
end min

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

-- Egyptian multiplication - progressively doubling a list, appending
-- stages of doubling to an accumulator where needed for binary
-- assembly of a target length

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

-- snd :: (a, b) -> b
on snd(xs)
    if class of xs is list and length of xs > 1 then
        item 2 of xs
    else
        missing value
    end if
end snd

-- show :: a -> String
on show(e)
    set c to class of e
    if c = list then
        script serialized
            on |λ|(v)
                show(v)
            end |λ|
        end script
        
        "{" & intercalate(", ", map(serialized, e)) & "}"
    else if c = record then
        script showField
            on |λ|(kv)
                set {k, v} to kv
                k & ":" & show(v)
            end |λ|
        end script
        
        "{" & intercalate(", ", ¬
            map(showField, zip(allKeys(e), allValues(e)))) & "}"
    else if c = date then
        ("date \"" & e as text) & "\""
    else if c = text then
        "\"" & e & "\""
    else
        try
            e as text
        on error
            ("«" & c as text) & "»"
        end try
    end if
end show

-- succ :: Int -> Int
on succ(x)
    x + 1
end succ

-- unlines :: [String] -> String
on unlines(xs)
    intercalate(linefeed, xs)
end unlines

-- zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
on zipWith(f, xs, ys)
    set lng to min(length of xs, length of ys)
    set lst to {}
    tell mReturn(f)
        repeat with i from 1 to lng
            set end of lst to |λ|(item i of xs, item i of ys)
        end repeat
        return lst
    end tell
end zipWith
```

{{Out}}

```txt
  1
  2  3
  4  5  6
  7  8  9 10
 11 12 13 14 15

  1
  2  3
  4  5  6
  7  8  9 10
 11 12 13 14 15
 16 17 18 19 20 21
 22 23 24 25 26 27 28
 29 30 31 32 33 34 35 36
 37 38 39 40 41 42 43 44  45
 46 47 48 49 50 51 52 53  54  55
 56 57 58 59 60 61 62 63  64  65  66
 67 68 69 70 71 72 73 74  75  76  77  78
 79 80 81 82 83 84 85 86  87  88  89  90  91
 92 93 94 95 96 97 98 99 100 101 102 103 104 105
```



Or, defining only the relationship between successive terms:


```applescript
-- floyd :: [Int] -> [Int]
on floyd(xs)
    set n to succ(length of xs)
    if n < 2 then
        {1}
    else
        enumFromTo(succ(n * (pred(n)) div 2), n * (succ(n)) div 2)
    end if
end floyd


-- floydN :: Int -> [[Int]]
on floydN(n)
    take(n, iterate(floyd, {1}))
end floydN


-- showFloyd :: [[Int]] -> String
on showFloyd(xs)
    script
        on |λ|(ns)
            script
                on |λ|(n)
                    justifyRight(4, space, n as string)
                end |λ|
            end script
            concat(map(result, ns))
        end |λ|
    end script
    unlines(map(result, xs))
end showFloyd


-- TEST -------------------------------------------------------------
on run
    
    showFloyd(floydN(5))
    
end run


-- GENERIC ABSTRACTIONS ---------------------------------------

-- concat :: [[a]] -> [a]
-- concat :: [String] -> String
on concat(xs)
    set lng to length of xs
    if 0 < lng and string is class of (item 1 of xs) then
        set acc to ""
    else
        set acc to {}
    end if
    repeat with i from 1 to lng
        set acc to acc & item i of xs
    end repeat
    acc
end concat

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

-- iterate :: (a -> a) -> a -> Gen [a]
on iterate(f, x)
    script
        property v : missing value
        property g : mReturn(f)'s |λ|
        on |λ|()
            if missing value is v then
                set v to x
            else
                set v to g(v)
            end if
            return v
        end |λ|
    end script
end iterate


-- justifyRight :: Int -> Char -> String -> String
on justifyRight(n, cFiller, strText)
    if n > length of strText then
        text -n thru -1 of ((replicate(n, cFiller) as text) & strText)
    else
        strText
    end if
end justifyRight


-- length :: [a] -> Int
on |length|(xs)
    set c to class of xs
    if list is c or string is c then
        length of xs
    else
        (2 ^ 29 - 1) -- (maxInt - simple proxy for non-finite)
    end if
end |length|

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

-- pred :: Int -> Int
on pred(x)
    (-1) + x
end pred

-- Egyptian multiplication - progressively doubling a list, appending
-- stages of doubling to an accumulator where needed for binary 
-- assembly of a target length
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

-- succ :: Int -> Int
on succ(x)
    1 + x
end succ

-- take :: Int -> [a] -> [a]
-- take :: Int -> String -> String
on take(n, xs)
    set c to class of xs
    if list is c then
        if 0 < n then
            items 1 thru min(n, length of xs) of xs
        else
            {}
        end if
    else if string is c then
        if 0 < n then
            text 1 thru min(n, length of xs) of xs
        else
            ""
        end if
    else if script is c then
        set ys to {}
        repeat with i from 1 to n
            set v to xs's |λ|()
            if missing value is v then
                return ys
            else
                set end of ys to v
            end if
        end repeat
        return ys
    else
        missing value
    end if
end take

-- unlines :: [String] -> String
on unlines(xs)
    set {dlm, my text item delimiters} to ¬
        {my text item delimiters, linefeed}
    set str to xs as text
    set my text item delimiters to dlm
    str
end unlines
```

{{Out}}

```txt
   1
   2   3
   4   5   6
   7   8   9  10
  11  12  13  14  15
```



## AutoHotkey


```AutoHotkey
Floyds_triangle(row){
	i = 0
	loop %row% 
	{
		n := A_Index
		loop, %n%
		{
			m := n, j := i, i++
			while (m<row) 
				j += m , m++
			res .= spaces(StrLen(j+1)-StrLen(i) +(A_Index=1?0:1)) i
		}
		if (A_Index < row)
			res .= "`r`n"
	}	
	return res
}
Spaces(no){
	loop, % no
		res.=" "
	return % res
}
```

Examples:
```AutoHotkey
MsgBox % Floyds_triangle(14)
```

Outputs:
```txt
 1
 2  3
 4  5  6
 7  8  9 10
11 12 13 14 15
16 17 18 19 20 21
22 23 24 25 26 27 28
29 30 31 32 33 34 35 36
37 38 39 40 41 42 43 44  45
46 47 48 49 50 51 52 53  54  55
56 57 58 59 60 61 62 63  64  65  66
67 68 69 70 71 72 73 74  75  76  77  78
79 80 81 82 83 84 85 86  87  88  89  90  91
92 93 94 95 96 97 98 99 100 101 102 103 104 105
```



## AWK


```AWK
#!/bin/awk -f

BEGIN {
	if (rows !~ /^[0-9]+$/ || rows < 0) {
		print "invalid rows or missing from command line"
		print "syntax: awk -v rows=14 -f floyds_triangle.awk"
		exit 1
	}

	for (row=cols=1; row<=rows; row++ cols++) {
		width[row] = length(row + (rows * (rows-1))/2)
		for (col=1; col<=cols; col++)
			printf("%*d%c", width[col], ++n, row == col ? "\n" : " ")
	}
}

```

<p>output from: awk -f floyds_triangle.awk -v rows=5</p>

```txt

 1
 2  3
 4  5  6
 7  8  9 10
11 12 13 14 15

```

<p>output from: awk -f floyds_triangle.awk -v rows=14</p>

```txt

 1
 2  3
 4  5  6
 7  8  9 10
11 12 13 14 15
16 17 18 19 20 21
22 23 24 25 26 27 28
29 30 31 32 33 34 35 36
37 38 39 40 41 42 43 44  45
46 47 48 49 50 51 52 53  54  55
56 57 58 59 60 61 62 63  64  65  66
67 68 69 70 71 72 73 74  75  76  77  78
79 80 81 82 83 84 85 86  87  88  89  90  91
92 93 94 95 96 97 98 99 100 101 102 103 104 105

```



## Batch File


```dos

@echo off

call:floyd 5
echo.
call:floyd 14
pause>nul
exit /b

:floyd
setlocal enabledelayedexpansion
set iterations=%1
set startn=1
set endn=1

for /l %%i in (1,1,%iterations%) do (
  for /l %%j in (!startn!,1,!endn!) do (
    set lastnum=%%j
    set /a startn=%%j+1
  )
  set /a endn=!startn!+%%i
)

call:getlength %startn%
set digits=%errorlevel%

set startn=1
set endn=1

for /l %%i in (1,1,%iterations%) do (
  set "line="
  for /l %%j in (!startn!,1,!endn!) do (
    set "space="
    call:getlength %%j
    set /a sparespace=%digits%-!errorlevel!
    for /l %%k in (0,1,!sparespace!) do set "space=!space! "
      
    set line=!line!!space!%%j
    set /a startn=%%j+1
  )
  echo !line!
  set /a endn=!startn!+%%i
)
exit /b

:getlength
setlocal enabledelayedexpansion
set offset=0
set string=%1
:floydloop
if "!string:~%offset%,1!"=="" endlocal && exit /b %offset%
set /a offset+=1
goto floydloop

```

{{out}}

```txt

  1
  2  3
  4  5  6
  7  8  9 10
 11 12 13 14 15

   1
   2   3
   4   5   6
   7   8   9  10
  11  12  13  14  15
  16  17  18  19  20  21
  22  23  24  25  26  27  28
  29  30  31  32  33  34  35  36
  37  38  39  40  41  42  43  44  45
  46  47  48  49  50  51  52  53  54  55
  56  57  58  59  60  61  62  63  64  65  66
  67  68  69  70  71  72  73  74  75  76  77  78
  79  80  81  82  83  84  85  86  87  88  89  90  91
  92  93  94  95  96  97  98  99 100 101 102 103 104 105

```




## BASIC

=
## Applesoft BASIC
=
Line <code>150,160</code> creates a vector of the length of all entries is the last row.  These values are used in line <code>210,220</code> to put the cursor at the correct horizontal position.

```basic

 100 :
 110  REM  FLOYD'S TRIANGLE
 120 :
 130  DEF  FN Q(A) =  INT ( LOG (A) /  LOG (10)) + 1                
 140 N = 14
 150  DIM P(N): P(0) =  - 1: FOR J = 1 TO N: I = (N * N - N) / 2 + J                              
 160 P(J) = P(J - 1) + FN Q(I) + 1: NEXT J                          
 200  FOR R = 1 TO N: FOR C = 1 TO R                                  
 210 NR = NR + 1:COL = P(C) - ( FN Q(NR) - 1)                         
 220  HTAB COL: PRINT NR;: NEXT C
 230  PRINT : NEXT R                    

```

{{out}}

```txt
]RUN
 1
 2  3
 4  5  6
 7  8  9 10
11 12 13 14 15
```

                                                                                

```txt
]RUN
 1
 2  3
 4  5  6
 7  8  9 10
11 12 13 14 15
16 17 18 19 20 21
22 23 24 25 26 27 28
29 30 31 32 33 34 35 36
37 38 39 40 41 42 43 44  45
46 47 48 49 50 51 52 53  54  55
56 57 58 59 60 61 62 63  64  65  66
67 68 69 70 71 72 73 74  75  76  77  78
79 80 81 82 83 84 85 86  87  88  89  90  91
92 93 94 95 96 97 98 99 100 101 102 103 104 105
```

=
## BBC BASIC
=

```bbcbasic
      n = 14
      num = 1
      last = (n^2 - n + 2) DIV 2
      FOR row = 1 TO n
        col = last
        FOR num = num TO num + row - 1
          @% = LEN(STR$(col)) + 1 : REM set column width
          PRINT num ;
          col += 1
        NEXT
        PRINT
      NEXT row
```

Output for n = 5:

```txt

  1
  2  3
  4  5  6
  7  8  9 10
 11 12 13 14 15

```

Output for n = 14:

```txt

  1
  2  3
  4  5  6
  7  8  9 10
 11 12 13 14 15
 16 17 18 19 20 21
 22 23 24 25 26 27 28
 29 30 31 32 33 34 35 36
 37 38 39 40 41 42 43 44  45
 46 47 48 49 50 51 52 53  54  55
 56 57 58 59 60 61 62 63  64  65  66
 67 68 69 70 71 72 73 74  75  76  77  78
 79 80 81 82 83 84 85 86  87  88  89  90  91
 92 93 94 95 96 97 98 99 100 101 102 103 104 105

```


==={{header|IS-BASIC}}===
<lang IS-BASIC>100 PROGRAM "FloydT.bas"
110 LET N=14:LET J=1
120 TEXT 80
130 FOR I=1 TO N
140   FOR J=J TO J+I-1
150     PRINT USING " ###":J;
160   NEXT
170   PRINT
180 NEXT
```


=
## MasmBasic
=
'''[http://www.webalice.it/jj2006/Masm32_Tips_Tricks_and_Traps.htm Builds with Masm, UAsm or AsmC plus the MasmBasic library]'''

```MasmBasic
include \masm32\MasmBasic\MasmBasic.inc
  SetGlobals rows, columns, ct, maxrows=4
  Init
  .Repeat
	For_ rows=0 To maxrows
		For_ columns=0 To rows
			inc ct
			Print Str$("%__i", ct)
			.if columns>6
				Print " "
			.endif
		Next
		Print
	Next
	Print
	Clr ct
	add maxrows, 9	; 4+9=13
  .Until maxrows>13
  Inkey
EndOfCode
```


{{out}}

```txt


  1
  2  3
  4  5  6
  7  8  9 10
 11 12 13 14 15

  1
  2  3
  4  5  6
  7  8  9 10
 11 12 13 14 15
 16 17 18 19 20 21
 22 23 24 25 26 27 28
 29 30 31 32 33 34 35 36
 37 38 39 40 41 42 43 44  45
 46 47 48 49 50 51 52 53  54  55
 56 57 58 59 60 61 62 63  64  65  66
 67 68 69 70 71 72 73 74  75  76  77  78
 79 80 81 82 83 84 85 86  87  88  89  90  91
 92 93 94 95 96 97 98 99 100 101 102 103 104 105
```



## Befunge



```Befunge
0" :seniL">:#,_&>:!#@_55+,:00p::1+*2/1v
vv+1:\-1p01g5-\g00<v`*9"o"\+`"c"\`9:::_
$>>\:::9`\"c"`+\9v:>>+00g1-:00p5p1-00g^
<v\*84-\g01+`*"o"<^<<p00:+1\+1/2*+1:::\
^>:#\1#,-#:\_$$.\:#^_$$>>1+\1-55+,:!#@_
```


{{out}}

```txt
Lines: 5

 1
 2  3
 4  5  6
 7  8  9 10
11 12 13 14 15
```



```txt
Lines: 14

 1
 2  3
 4  5  6
 7  8  9 10
11 12 13 14 15
16 17 18 19 20 21
22 23 24 25 26 27 28
29 30 31 32 33 34 35 36
37 38 39 40 41 42 43 44  45
46 47 48 49 50 51 52 53  54  55
56 57 58 59 60 61 62 63  64  65  66
67 68 69 70 71 72 73 74  75  76  77  78
79 80 81 82 83 84 85 86  87  88  89  90  91
92 93 94 95 96 97 98 99 100 101 102 103 104 105
```



## Bracmat


```bracmat
  ( ( floyd
    =   lowerLeftCorner lastInColumn lastInRow row i W w
      .   put$(str$("Floyd " !arg ":\n"))
        &   !arg*(!arg+-1)*1/2+1
          : ?lowerLeftCorner
          : ?lastInColumn
        & 1:?lastInRow:?row:?i
        &   whl
          ' ( !row:~>!arg
            & @(!lastInColumn:? [?W)
            & @(!i:? [?w)
            & whl'(!w+1:~>!W:?w&put$" ")
            & put$!i
            & (   !i:<!lastInRow
                & put$" "
                & 1+!lastInColumn:?lastInColumn
              |   put$\n
                & (1+!row:?row)+!lastInRow:?lastInRow
                & !lowerLeftCorner:?lastInColumn
              )
            & 1+!i:?i
            )
    )
  & floyd$5
  & floyd$14
  );
```

Output:

```txt
Floyd 5:
 1
 2  3
 4  5  6
 7  8  9 10
11 12 13 14 15
Floyd 14:
 1
 2  3
 4  5  6
 7  8  9 10
11 12 13 14 15
16 17 18 19 20 21
22 23 24 25 26 27 28
29 30 31 32 33 34 35 36
37 38 39 40 41 42 43 44  45
46 47 48 49 50 51 52 53  54  55
56 57 58 59 60 61 62 63  64  65  66
67 68 69 70 71 72 73 74  75  76  77  78
79 80 81 82 83 84 85 86  87  88  89  90  91
92 93 94 95 96 97 98 99 100 101 102 103 104 105
```



## C


```c>#include <stdio.h


void t(int n)
{
	int i, j, c, len;

	i = n * (n - 1) / 2;
	for (len = c = 1; c < i; c *= 10, len++);
	c -= i; // c is the col where width changes

#define SPEED_MATTERS 0
#if SPEED_MATTERS	// in case we really, really wanted to print huge triangles often
	char tmp[32], s[4096], *p;

	sprintf(tmp, "%*d", len, 0);

	inline void inc_numstr(void) {
		int k = len;

	redo:	if (!k--) return;

		if (tmp[k] == '9') {
			tmp[k] = '0';
			goto redo;
		}

		if (++tmp[k] == '!')
			tmp[k] = '1';
	}

	for (p = s, i = 1; i <= n; i++) {
		for (j = 1; j <= i; j++) {
			inc_numstr();
			__builtin_memcpy(p, tmp + 1 - (j >= c), len - (j < c));
			p += len - (j < c);

			*(p++) = (i - j)? ' ' : '\n';

			if (p - s + len >= 4096) {
				fwrite(s, 1, p - s, stdout);
				p = s;
			}
		}
	}

	fwrite(s, 1, p - s, stdout);
#else // NO_IT_DOESN'T
	int num;
	for (num = i = 1; i <= n; i++)
		for (j = 1; j <= i; j++)
			printf("%*d%c",	len - (j < c), num++, i - j ? ' ':'\n');
#endif
}

int main(void)
{
	t(5), t(14);

	// maybe not 
	// t(10000);
	return 0;
}
```

Output identical to D's.


## C++


```cpp

#include <windows.h>
#include <sstream>
#include <iostream>

//--------------------------------------------------------------------------------------------------
using namespace std;

//--------------------------------------------------------------------------------------------------
class floyds_tri
{
public:
    floyds_tri()  { lastLineLen = 0; }
    ~floyds_tri() { killArray(); }

    void create( int rows )
    {
	_rows = rows;
	calculateLastLineLen();
	display();
    }

private:
    void killArray()
    {
	if( lastLineLen ) 
	    delete [] lastLineLen;
    }

    void calculateLastLineLen()
    {
	killArray();
	lastLineLen = new BYTE[_rows];

	int s = 1 + ( _rows * ( _rows - 1 ) ) / 2;

	for( int x = s, ix = 0; x < s + _rows; x++, ix++ )
	{
	    ostringstream cvr;
	    cvr << x;
	    lastLineLen[ix] = static_cast<BYTE>( cvr.str().size() );
	}
    }

    void display()
    {
	cout << endl << "Floyd\'s Triangle - " << _rows << " rows" << endl << "
### =========================================
" << endl;
	int number = 1;
	for( int r = 0; r < _rows; r++ )
	{
	    for( int c = 0; c <= r; c++ )
	    {
		ostringstream cvr;
		cvr << number++;
		string str = cvr.str();
		while( str.length() < lastLineLen[c] )
		    str = " " + str;
		cout << str << " ";
	    }
	    cout << endl;
	}
    }

    int _rows;
    BYTE* lastLineLen;
};
//--------------------------------------------------------------------------------------------------
int main( int argc, char* argv[] )
{
    floyds_tri t;
    int s;
    while( true )
    {
	cout << "Enter the size of the triangle ( 0 to QUIT ): "; cin >> s;
	if( !s ) return 0;
	if( s > 0 ) t.create( s );

	cout << endl << endl;
	system( "pause" );
    }

    return 0;
}
//--------------------------------------------------------------------------------------------------
```

{{out}}

```txt
Floyd's Triangle - 5 rows

### =========================================

 1
 2  3
 4  5  6
 7  8  9 10
11 12 13 14 15


Floyd's Triangle - 14 rows

### =========================================

 1
 2  3
 4  5  6
 7  8  9 10
11 12 13 14 15
16 17 18 19 20 21
22 23 24 25 26 27 28
29 30 31 32 33 34 35 36
37 38 39 40 41 42 43 44  45
46 47 48 49 50 51 52 53  54  55
56 57 58 59 60 61 62 63  64  65  66
67 68 69 70 71 72 73 74  75  76  77  78
79 80 81 82 83 84 85 86  87  88  89  90  91
92 93 94 95 96 97 98 99 100 101 102 103 104 105
```



## C sharp

{{Trans|Perl}}

```csharp
using System;
using System.Text;

public class FloydsTriangle
{
    internal static void Main(string[] args)
    {
        int count;
        if (args.Length >= 1 && int.TryParse(args[0], out count) && count > 0)
        {
            Console.WriteLine(MakeTriangle(count));
        }
        else
        {
            Console.WriteLine(MakeTriangle(5));
            Console.WriteLine();
            Console.WriteLine(MakeTriangle(14));
        }
    }

    public static string MakeTriangle(int rows)
    {
        int maxValue = (rows * (rows + 1)) / 2;
        int digit = 0;
        StringBuilder output = new StringBuilder();

        for (int row = 1; row <= rows; row++)
        {
            for (int column = 0; column < row; column++)
            {
                int colMaxDigit = (maxValue - rows) + column + 1;
                if (column > 0)
                {
                    output.Append(' ');
                }

                digit++;
                output.Append(digit.ToString().PadLeft(colMaxDigit.ToString().Length));
            }

            output.AppendLine();
        }

        return output.ToString();
    }
}
```




## Clojure

I didn't translete this, it's from my own creation.

```clojure

(defn TriangleList [n]
  (let [l (map inc (range))]
    (loop [l l x 1 nl []]
      (if (= n (count nl))
        nl
        (recur (drop x l) (inc x) (conj nl (take x l)))))))

(defn TrianglePrint [n]
  (let [t (TriangleList n)
        m (count (str (last (last t))))
        f (map #(map str %) t)
        l (map #(map (fn [x] (if (> m (count x))
                               (str (apply str (take (- m (count x))
                                                     (repeat " "))) x)
                               x)) %) f)
        e (map #(map (fn [x] (str " " x)) %) l)]
    (map #(println (apply str %)) e)))

```

By Average-user.

{{out}}

```txt

(TrianglePrint 5)
  1
  2  3
  4  5  6
  7  8  9 10
 11 12 13 14 15

(TrianglePrint 14)
   1
   2   3
   4   5   6
   7   8   9  10
  11  12  13  14  15
  16  17  18  19  20  21
  22  23  24  25  26  27  28
  29  30  31  32  33  34  35  36
  37  38  39  40  41  42  43  44  45
  46  47  48  49  50  51  52  53  54  55
  56  57  58  59  60  61  62  63  64  65  66
  67  68  69  70  71  72  73  74  75  76  77  78
  79  80  81  82  83  84  85  86  87  88  89  90  91
  92  93  94  95  96  97  98  99 100 101 102 103 104 105

```



## CoffeeScript

{{trans|Kotlin}}

```coffeescript
triangle = (array) -> for n in array
    console.log "#{n} rows:"
    printMe = 1
    printed = 0
    row = 1
    to_print = ""
    while row <= n
        cols = Math.ceil(Math.log10(n * (n - 1) / 2 + printed + 2.0))
        p = ("" + printMe).length
        while p++ <= cols
            to_print += ' '
        to_print += printMe + ' '
        if ++printed == row
            console.log to_print
            to_print = ""
            row++
            printed = 0
        printMe++

triangle [5, 14]
```

Output as Kotlin.


## Common Lisp


### Version 1


```lisp
;;;using flet to define local functions and storing precalculated column widths in array
;;;verbose, but more readable and efficient than version 2

(defun floydtriangle (rows)
       (let (column-widths)
         (setf column-widths (make-array rows :initial-element nil))
           (flet (
             (lazycat (n)
              (/ (+ (expt n 2) n 2) 2))
             (width (v)
              (+ 1 (floor (log v 10)))))
            (dotimes (i rows)
             (setf (aref column-widths i)(width (+ i (lazycat (- rows 1))))))
            (dotimes (row rows)
             (dotimes (col (+ 1 row))
               (format t "~vd " (aref column-widths col)(+ col (lazycat row))))
             (format t "~%")))))
```


===Version 2 - any base===

```lisp
;;; more concise than version 1 but less efficient for a large triangle
;;;optional "base" parameter will allow use of any base from 2 to 36

(defun floydtriangle (rows &optional (base 10))
       (dotimes (row rows)
         (dotimes (column (+ 1 row))
           (format t "~v,vr " base (length (format nil "~vr" base (+ column (/ (+ (expt (- rows 1) 2) (- rows 1) 2) 2)))) (+ column (/ (+ (expt row 2) row 2) 2))))
         (format t "~%")))
```


{{out}}

```txt

(floydtriangle 5)
 1
 2  3
 4  5  6
 7  8  9 10
11 12 13 14 15

(floydtriangle 14)
 1
 2  3
 4  5  6
 7  8  9 10
11 12 13 14 15
16 17 18 19 20 21
22 23 24 25 26 27 28
29 30 31 32 33 34 35 36
37 38 39 40 41 42 43 44  45
46 47 48 49 50 51 52 53  54  55
56 57 58 59 60 61 62 63  64  65  66
67 68 69 70 71 72 73 74  75  76  77  78
79 80 81 82 83 84 85 86  87  88  89  90  91
92 93 94 95 96 97 98 99 100 101 102 103 104 105

(floydtriangle 5 2)
   1 
  10   11 
 100  101  110 
 111 1000 1001 1010 
1011 1100 1101 1110 1111 

(floydtriangle 14 36)
 1 
 2  3 
 4  5  6 
 7  8  9  A 
 B  C  D  E  F 
 G  H  I  J  K  L 
 M  N  O  P  Q  R  S 
 T  U  V  W  X  Y  Z 10 
11 12 13 14 15 16 17 18 19 
1A 1B 1C 1D 1E 1F 1G 1H 1I 1J 
1K 1L 1M 1N 1O 1P 1Q 1R 1S 1T 1U 
1V 1W 1X 1Y 1Z 20 21 22 23 24 25 26 
27 28 29 2A 2B 2C 2D 2E 2F 2G 2H 2I 2J 
2K 2L 2M 2N 2O 2P 2Q 2R 2S 2T 2U 2V 2W 2X

```



## D


```d
import std.stdio, std.conv;

void floydTriangle(in uint n) {
    immutable lowerLeftCorner = n * (n - 1) / 2 + 1;
    foreach (r; 0 .. n)
        foreach (c; 0 .. r + 1)
            writef("%*d%c",
                   text(lowerLeftCorner + c).length,
                   r * (r + 1) / 2 + c + 1,
                   c == r ? '\n' : ' ');
}

void main() {
    floydTriangle(5);
    floydTriangle(14);
}
```

{{out}}

```txt
 1
 2  3
 4  5  6
 7  8  9 10
11 12 13 14 15
 1
 2  3
 4  5  6
 7  8  9 10
11 12 13 14 15
16 17 18 19 20 21
22 23 24 25 26 27 28
29 30 31 32 33 34 35 36
37 38 39 40 41 42 43 44  45
46 47 48 49 50 51 52 53  54  55
56 57 58 59 60 61 62 63  64  65  66
67 68 69 70 71 72 73 74  75  76  77  78
79 80 81 82 83 84 85 86  87  88  89  90  91
92 93 94 95 96 97 98 99 100 101 102 103 104 105
```



## Elixir


```elixir
defmodule Floyd do
  def triangle(n) do
    max = trunc(n * (n + 1) / 2)
    widths = for m <- (max - n + 1)..max, do: (m |> Integer.to_string |> String.length) + 1
    format = Enum.map(widths, fn wide -> "~#{wide}w" end) |> List.to_tuple
    line(n, 0, 1, format)
  end
  
  def line(n, n, _, _), do: :ok
  def line(n, i, count, format) do
    Enum.each(0..i, fn j -> :io.fwrite(elem(format,j), [count+j]) end)
    IO.puts ""
    line(n, i+1, count+i+1, format)
  end
end

Floyd.triangle(5)
Floyd.triangle(14)
```


{{out}}

```txt

  1
  2  3
  4  5  6
  7  8  9 10
 11 12 13 14 15
  1
  2  3
  4  5  6
  7  8  9 10
 11 12 13 14 15
 16 17 18 19 20 21
 22 23 24 25 26 27 28
 29 30 31 32 33 34 35 36
 37 38 39 40 41 42 43 44  45
 46 47 48 49 50 51 52 53  54  55
 56 57 58 59 60 61 62 63  64  65  66
 67 68 69 70 71 72 73 74  75  76  77  78
 79 80 81 82 83 84 85 86  87  88  89  90  91
 92 93 94 95 96 97 98 99 100 101 102 103 104 105

```



## Erlang


```erlang

-module( floyds_triangle ).

-export( [integers/1, print/1, strings/1, task/0] ).

integers( N ) ->
          lists:reverse( integers_reversed(N) ).

print( N ) ->
       [io:fwrite("~s~n", [lists:flatten(X)]) || X <- strings(N)].

strings( N ) ->
        Strings_reversed = [strings_from_integers(X) || X <- integers_reversed(N)],
        Paddings = paddings( [lengths(X) || X <- Strings_reversed] ),
        [formats(X, Y) || {X, Y} <- lists:zip(Paddings, lists:reverse(Strings_reversed))].

task() ->
       print( 5	),
       print( 14 ).



formats( Paddings, Strings ) -> [lists:flatten(io_lib:format(" ~*s", [X, Y])) || {X, Y} <- lists:zip(Paddings, Strings)].

integers_reversed( N ) ->
          {_End, Integers_reversed} = lists:foldl( fun integers_reversed/2, {1, []}, lists:seq(0, N - 1) ),
          Integers_reversed.

integers_reversed( N, {Start, Acc} ) ->
          End = Start + N,
          {End + 1, [lists:seq(Start, End) | Acc]}.

lengths( Strings ) -> [string:len(X) || X <- Strings].

paddings( [Last_line | T] ) ->
          {[], Paddings} = lists:foldl( fun paddings/2, {paddings_lose_last(Last_line), [Last_line]}, lists:seq(1, erlang:length(T)) ),
          Paddings.

paddings( _N, {Current,	Acc} ) -> {paddings_lose_last(Current),	[Current | Acc]}.

paddings_lose_last( List ) ->
	[_H | T]	= lists:reverse( List ),
	lists:reverse( T ).

strings_from_integers( Integers ) -> [erlang:integer_to_list(X) || X <- Integers].

```

{{out}}

```txt

  1
  2  3
  4  5  6
  7  8  9 10
 11 12 13 14 15
  1
  2  3
  4  5  6
  7  8  9 10
 11 12 13 14 15
 16 17 18 19 20 21
 22 23 24 25 26 27 28
 29 30 31 32 33 34 35 36
 37 38 39 40 41 42 43 44  45
 46 47 48 49 50 51 52 53  54  55
 56 57 58 59 60 61 62 63  64  65  66
 67 68 69 70 71 72 73 74  75  76  77  78
 79 80 81 82 83 84 85 86  87  88  89  90  91
 92 93 94 95 96 97 98 99 100 101 102 103 104 105

```



## ERRE


```ERRE

PROGRAM FLOYD

!
! for rosettacode.org
!

BEGIN
  N=14
  NUM=1
  LAST=(N^2-N+2) DIV 2
  FOR ROW=1 TO N DO
    FOR J=1 TO ROW DO
       US$=STRING$(LEN(STR$(LAST-1+J))-1,"#")
       WRITE(US$;NUM;)
       PRINT(" ";)
       NUM+=1
    END FOR
    PRINT
  END FOR
END PROGRAM

```

Example for n=14
{{out}}

```txt

 1
 2  3
 4  5  6
 7  8  9 10
11 12 13 14 15
16 17 18 19 20 21
22 23 24 25 26 27 28
29 30 31 32 33 34 35 36
37 38 39 40 41 42 43 44  45
46 47 48 49 50 51 52 53  54  55
56 57 58 59 60 61 62 63  64  65  66
67 68 69 70 71 72 73 74  75  76  77  78
79 80 81 82 83 84 85 86  87  88  89  90  91
92 93 94 95 96 97 98 99 100 101 102 103 104 105

```


=={{header|F_Sharp|F#}}==

```fsharp
open System

[<EntryPoint>]
let main argv = 
    // columns and rows are 0-based, so the input has to be decremented:
    let maxRow =
        match UInt32.TryParse(argv.[0]) with
        | (true, v) when v > 0u -> int (v - 1u)
        | (_, _) -> failwith "not a positive integer"

    let len (n: int) = int (Math.Floor(Math.Log10(float n)))
    let col0 row = row * (row + 1) / 2 + 1
    let col0maxRow = col0 maxRow
    for row in [0 .. maxRow] do
        for col in [0 .. row] do
            let value = (col0 row) + col
            let pad = String(' ', (len (col0maxRow + col) - len (value) + 1))
            printf "%s%d" pad value
        printfn ""
    0
```

Output for 5 and 14 (via command line argument)
<pre style="float:left">  1
  2  3
  4  5  6
  7  8  9 10
 11 12 13 14 15
```


```txt
  1
  2  3
  4  5  6
  7  8  9 10
 11 12 13 14 15
 16 17 18 19 20 21
 22 23 24 25 26 27 28
 29 30 31 32 33 34 35 36
 37 38 39 40 41 42 43 44  45
 46 47 48 49 50 51 52 53  54  55
 56 57 58 59 60 61 62 63  64  65  66
 67 68 69 70 71 72 73 74  75  76  77  78
 79 80 81 82 83 84 85 86  87  88  89  90  91
 92 93 94 95 96 97 98 99 100 101 102 103 104 105
```



## Factor


```factor
USING: io kernel math math.functions math.ranges prettyprint
sequences ;
IN: rosetta-code.floyds-triangle

: floyd. ( n -- )
    [ dup 1 - * 2 / 1 + dup 1 ] [ [1,b] ] bi
    [
        [
            2dup [ log10 1 + >integer ] bi@ -
            [ " " write ] times dup pprint bl [ 1 + ] bi@
        ] times nl [ drop dup ] dip
    ] each nl 3drop ;

5 14 [ floyd. ] bi@
```

{{out}}

```txt

 1 
 2  3 
 4  5  6 
 7  8  9 10 
11 12 13 14 15 

 1 
 2  3 
 4  5  6 
 7  8  9 10 
11 12 13 14 15 
16 17 18 19 20 21 
22 23 24 25 26 27 28 
29 30 31 32 33 34 35 36 
37 38 39 40 41 42 43 44  45 
46 47 48 49 50 51 52 53  54  55 
56 57 58 59 60 61 62 63  64  65  66 
67 68 69 70 71 72 73 74  75  76  77  78 
79 80 81 82 83 84 85 86  87  88  89  90  91 
92 93 94 95 96 97 98 99 100 101 102 103 104 105 

```



## Forth


```forth
: lastn ( rows -- n ) dup 1- * 2/ ;
: width ( n -- n )  s>f flog ftrunc f>s 2 + ;

: triangle ( rows -- )
  dup lastn 0 rot ( last 0 rows )
  0 do
    over cr
    i 1+ 0 do
      1+ swap 1+ swap
      2dup width u.r
    loop
    drop
  loop
  2drop ;

```



## Fortran


Please find compilation instructions on GNU/linux system at the beginning of the source.  There, also, are the example output triangles produced by running the program.  The environment variable setting and command line argument are vestigial.  Ignore them.  The code demonstrates writing to an in memory buffer, an old feature of FORTRAN.

```FORTRAN

!-*- mode: compilation; default-directory: "/tmp/" -*-
!Compilation started at Tue May 21 22:55:08
!
!a=./f && make $a && OMP_NUM_THREADS=2 $a 1223334444
!gfortran -std=f2008 -Wall -ffree-form -fall-intrinsics f.f08 -o f
!  1 
!  2  3 
!  4  5  6 
!  7  8  9 10 
! 11 12 13 14 15 
!
!
!  1 
!  2  3 
!  4  5  6 
!  7  8  9 10 
! 11 12 13 14 15 
! 16 17 18 19 20 21 
! 22 23 24 25 26 27 28 
! 29 30 31 32 33 34 35 36 
! 37 38 39 40 41 42 43 44  45 
! 46 47 48 49 50 51 52 53  54  55 
! 56 57 58 59 60 61 62 63  64  65  66 
! 67 68 69 70 71 72 73 74  75  76  77  78 
! 79 80 81 82 83 84 85 86  87  88  89  90  91 
! 92 93 94 95 96 97 98 99 100 101 102 103 104 105 
!
!
!
!Compilation finished at Tue May 21 22:55:08


program p
  integer, dimension(2) :: examples = [5, 14]
  integer :: i
  do i=1, size(examples)
    call floyd(examples(i))
    write(6, '(/)')
  end do

contains

  subroutine floyd(rows)
    integer, intent(in) :: rows
    integer :: n, i, j, k
    integer, dimension(60) :: L
    character(len=504) :: fmt
    n = (rows*(rows+1))/2 ! Gauss's formula
    do i=1,rows ! compute format of final row
      L(i) = 2+int(log10(real(n-rows+i)))
    end do
    k = 0
    do i=1,rows
      do j=1,i
        k = k+1
        write(fmt,'(a2,i1,a1)')'(i',L(j),')'
        write(6,fmt,advance='no') k
      enddo
      write(6,*) ''
    end do
  end subroutine floyd
  
end program p

```



## FreeBASIC


```freebasic
' version 19-09-2015
' compile with: fbc -s console

Sub pascal_triangle(n As UInteger)

    Dim As UInteger a = 1, b, i, j, switch = n + 1
    Dim As String frmt, frmt_1, frmt_2

    ' last number of the last line
    i = (n * (n + 1)) \ 2
    frmt_2 = String(Len(Str(i)) + 1, "#")
    ' first number of the last line
    i = ((n - 1) * n) \ 2 + 1
    frmt_1 = String(Len(Str(i)) + 1, "#")

    ' we have 2 different formats strings
    ' find the point where we have to make the switch
    If frmt_1 <> frmt_2 Then
        j = i + 1
        While Len(Str(i)) = Len(Str(J))
            j =  j + 1
        Wend
        switch = j - i
    End If

    Print "output for "; Str(n) : Print
    For i = 1 To n
        frmt = frmt_1
        b = (i * (i + 1)) \ 2
        For j = a To b
            ' if we have the switching point change format string
            If j - a = switch Then frmt = frmt_2
            Print Using frmt; j;
        Next j
        Print
        a = b + 1
    Next i
    Print

End Sub

' ------=< MAIN >=------

pascal_triangle(5)

pascal_triangle(14)


' empty keyboard buffer
While Inkey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

{{out}}

```txt
output for 5             output for 14  
                         
  1                        1
  2  3                     2  3
  4  5  6                  4  5  6
  7  8  9 10               7  8  9 10
 11 12 13 14 15           11 12 13 14 15
                          16 17 18 19 20 21
                          22 23 24 25 26 27 28
                          29 30 31 32 33 34 35 36
                          37 38 39 40 41 42 43 44  45
                          46 47 48 49 50 51 52 53  54  55
                          56 57 58 59 60 61 62 63  64  65  66
                          67 68 69 70 71 72 73 74  75  76  77  78
                          79 80 81 82 83 84 85 86  87  88  89  90  91
                          92 93 94 95 96 97 98 99 100 101 102 103 104 105
```



## Gambas

'''[https://gambas-playground.proko.eu/?gist=57ab1f58785b7e07765881657e4589ab Click this link to run this code]'''

```gambas
Public Sub Main()
Dim siCount, siNo, siCounter As Short
Dim siLine As Short = 1
Dim siInput As Short[] = [5, 14]

For siCount = 0 To siInput.Max
  Print "Floyd's triangle to " & siInput[siCount] & " lines"
  Do
    Inc siNo
    Inc siCounter
    Print Format(siNo, "####");
      If siLine = siCounter Then 
        Print 
        Inc siLine
        siCounter = 0
      End If
    If siLine - 1 = siInput[siCount] Then Break
  Loop
  siLine = 1
  siCounter = 0
  siNo = 0
  Print
Next

End
```

Output:

```txt

Floyd's triangle to 5 lines
   1
   2   3
   4   5   6
   7   8   9  10
  11  12  13  14  15

Floyd's triangle to 14 lines
   1
   2   3
   4   5   6
   7   8   9  10
  11  12  13  14  15
  16  17  18  19  20  21
  22  23  24  25  26  27  28
  29  30  31  32  33  34  35  36
  37  38  39  40  41  42  43  44  45
  46  47  48  49  50  51  52  53  54  55
  56  57  58  59  60  61  62  63  64  65  66
  67  68  69  70  71  72  73  74  75  76  77  78
  79  80  81  82  83  84  85  86  87  88  89  90  91
  92  93  94  95  96  97  98  99 100 101 102 103 104 105

```



## Go


```go
package main

import "fmt"

func main() {
    floyd(5)
    floyd(14)
}

func floyd(n int) {
    fmt.Printf("Floyd %d:\n", n)
    lowerLeftCorner := n*(n-1)/2 + 1
    lastInColumn := lowerLeftCorner
    lastInRow := 1
    for i, row := 1, 1; row <= n; i++ {
        w := len(fmt.Sprint(lastInColumn))
        if i < lastInRow {
            fmt.Printf("%*d ", w, i)
            lastInColumn++
        } else {
            fmt.Printf("%*d\n", w, i)
            row++
            lastInRow += row
            lastInColumn = lowerLeftCorner
        }
    }
}
```

{{out}}

```txt

Floyd 5:
 1
 2  3
 4  5  6
 7  8  9 10
11 12 13 14 15
Floyd 14:
 1
 2  3
 4  5  6
 7  8  9 10
11 12 13 14 15
16 17 18 19 20 21
22 23 24 25 26 27 28
29 30 31 32 33 34 35 36
37 38 39 40 41 42 43 44  45
46 47 48 49 50 51 52 53  54  55
56 57 58 59 60 61 62 63  64  65  66
67 68 69 70 71 72 73 74  75  76  77  78
79 80 81 82 83 84 85 86  87  88  89  90  91
92 93 94 95 96 97 98 99 100 101 102 103 104 105

```



## Haskell


'''Program'''

```haskell
import Control.Monad (liftM2)

floydTriangle =
  liftM2
    (zipWith (liftM2 (.) enumFromTo ((pred .) . (+))))
    (scanl (+) 1)
    id
    [1 ..]

alignR :: Int -> Integer -> String
alignR n = (\s -> replicate (n - length s) ' ' ++ s) . show

formatFT :: Int -> IO ()
formatFT n = mapM_ (putStrLn . unwords . zipWith alignR ws) t
  where
    t = take n floydTriangle
    ws = map (length . show) $ last t
```


'''Output''':

```haskell
*Main> formatFT 5
 1
 2  3
 4  5  6
 7  8  9 10
11 12 13 14 15

*Main> formatFT 14
 1
 2  3
 4  5  6
 7  8  9 10
11 12 13 14 15
16 17 18 19 20 21
22 23 24 25 26 27 28
29 30 31 32 33 34 35 36
37 38 39 40 41 42 43 44  45
46 47 48 49 50 51 52 53  54  55
56 57 58 59 60 61 62 63  64  65  66
67 68 69 70 71 72 73 74  75  76  77  78
79 80 81 82 83 84 85 86  87  88  89  90  91
92 93 94 95 96 97 98 99 100 101 102 103 104 105
```



Or, simplifying a little by delegating the recursion scheme to '''mapAccumL'''


```haskell
import Data.List (mapAccumL)
import Control.Arrow ((&&&))

floyd :: Int -> [[Int]]
floyd n =
  snd $
  mapAccumL
    (\a x ->
        ((succ &&& enumFromTo a) (a + x)))
    1
    [0 .. pred n]

-- TEST -------------------------------------
showFloyd :: [[Int]] -> String
showFloyd xs =
  let padRight n = length >>= (<$> mappend (replicate n ' ')) . drop
  in unlines $
     (concat .
      zipWith ((. show) . padRight) ((succ . length . show) <$> last xs)) <$>
     xs

main :: IO ()
main = mapM_ putStrLn $ (showFloyd . floyd) <$> [5, 14]
```

{{Out}}

```txt
  1
  2  3
  4  5  6
  7  8  9 10
 11 12 13 14 15

  1
  2  3
  4  5  6
  7  8  9 10
 11 12 13 14 15
 16 17 18 19 20 21
 22 23 24 25 26 27 28
 29 30 31 32 33 34 35 36
 37 38 39 40 41 42 43 44  45
 46 47 48 49 50 51 52 53  54  55
 56 57 58 59 60 61 62 63  64  65  66
 67 68 69 70 71 72 73 74  75  76  77  78
 79 80 81 82 83 84 85 86  87  88  89  90  91
 92 93 94 95 96 97 98 99 100 101 102 103 104 105
```



Or, defining just the relationship between successive terms:

```haskell
floyd :: [Int] -> [Int]
floyd xs
  | n < 2 = [1]
  | otherwise = [succ (div (n * pred n) 2) .. div (n * succ n) 2]
  where
    n = succ (length xs)

floydN :: Int -> [[Int]]
floydN n = take n (iterate floyd [1])

main :: IO ()
main = mapM_ print $ floydN 5
```

{{Out}}

```txt
[1]
[2,3]
[4,5,6]
[7,8,9,10]
[11,12,13,14,15]
```


=={{header|Icon}} and {{header|Unicon}}==

The following solution works in both languages:

```unicon
procedure main(a)
    n := integer(a[1]) | 5
    w := ((n*(n-1))/2)-n
    c := create seq()
    every row := 1 to n do {
        every col := 1 to row do {
            width := *(w+col)+1
            every writes(right(@c,width))
            }
        write()
        }
end
```


Sample outputs:

```txt

->ft
  1
  2  3
  4  5  6
  7  8  9 10
 11 12 13 14 15
->

```



```txt

->ft 14
  1
  2  3
  4  5  6
  7  8  9 10
 11 12 13 14 15
 16 17 18 19 20 21
 22 23 24 25 26 27 28
 29 30 31 32 33 34 35 36
 37 38 39 40 41 42 43 44  45
 46 47 48 49 50 51 52 53  54  55
 56 57 58 59 60 61 62 63  64  65  66
 67 68 69 70 71 72 73 74  75  76  77  78
 79 80 81 82 83 84 85 86  87  88  89  90  91
 92 93 94 95 96 97 98 99 100 101 102 103 104 105
->

```



## J


Note: <code>require 'strings'</code> does nothing in J7, but is harmless (strings is already incorporated in J7).


```J
require 'strings'
floyd=: [: rplc&(' 0';'  ')"1@":@(* ($ $ +/\@,)) >:/~@:i.
```


Note, the parenthesis around ($ $ +/\@,) is optional, and only included for emphasis.

Example use:


```J
   floyd 5
 1            
 2  3         
 4  5  6      
 7  8  9 10   
11 12 13 14 15
   floyd 14
 1                                             
 2  3                                          
 4  5  6                                       
 7  8  9 10                                    
11 12 13 14 15                                 
16 17 18 19 20 21                              
22 23 24 25 26 27 28                           
29 30 31 32 33 34 35 36                        
37 38 39 40 41 42 43 44  45                    
46 47 48 49 50 51 52 53  54  55                
56 57 58 59 60 61 62 63  64  65  66            
67 68 69 70 71 72 73 74  75  76  77  78        
79 80 81 82 83 84 85 86  87  88  89  90  91    
92 93 94 95 96 97 98 99 100 101 102 103 104 105
```


How it works:

First, we create a square lower triangular matrix with our argument as the length of one side.  We have 1s along the diagonal and the lower triangle, and 0s for the upper triangle.

Second, we create a running sum of these values (treating rows as being adjacent horizontally for this purpose).  Then, we multiply this result by our lower triangular matrix (forcing the upper triangle to be 0s).

Then, we format the matrix as text (which gives us the required vertical alignment), and in each row we replace each space followed by a zero with two spaces.

Efficiency note: In a measurement of time used: in floyd 100, 80% the time here goes into the string manipulations -- sequential additions and multiplications are cheap.  In floyd 1000 this jumps to 98% of the time.  Here's a faster version (about 3x on floyd 1000) courtesy of Aai of the J forums:


```J
floyd=: [: ({.~ i.&1@E.~&' 0')"1@":@(* ($ $ +/\@,)) >:/~@:i.
```



## Java


```java

public class Floyd {
	public static void main(String[] args){
		printTriangle(5);
		printTriangle(14);
	}
	
	private static void printTriangle(int n){
		System.out.println(n + " rows:");
		for(int rowNum = 1, printMe = 1, numsPrinted = 0;
				rowNum <= n; printMe++){
			int cols = (int)Math.ceil(Math.log10(n*(n-1)/2 + numsPrinted + 2));
			System.out.printf("%"+cols+"d ", printMe);
			if(++numsPrinted == rowNum){
				System.out.println();
				rowNum++;
				numsPrinted = 0;
			}
		}
	}
}
```

Output:

```txt
5 rows:
 1 
 2  3 
 4  5  6 
 7  8  9 10 
11 12 13 14 15 
14 rows:
 1 
 2  3 
 4  5  6 
 7  8  9 10 
11 12 13 14 15 
16 17 18 19 20 21 
22 23 24 25 26 27 28 
29 30 31 32 33 34 35 36 
37 38 39 40 41 42 43 44  45 
46 47 48 49 50 51 52 53  54  55 
56 57 58 59 60 61 62 63  64  65  66 
67 68 69 70 71 72 73 74  75  76  77  78 
79 80 81 82 83 84 85 86  87  88  89  90  91 
92 93 94 95 96 97 98 99 100 101 102 103 104 105 
```



## JavaScript


###  ES5 

(In a functional idiom of JavaScript)

Two main functions:
:#An expression of the Floyd triangle  as a list of lists (a function of the number of rows),
:#and a mapping of that expression to a formatted string.


```JavaScript
(function () {
    'use strict';

    // FLOYD's TRIANGLE -------------------------------------------------------

    // floyd :: Int -> [[Int]]
    function floyd(n) {
        return snd(mapAccumL(function (start, row) {
            return [start + row + 1, enumFromTo(start, start + row)];
        }, 1, enumFromTo(0, n - 1)));
    };

    // showFloyd :: [[Int]] -> String
    function showFloyd(xss) {
        var ws = map(compose([succ, length, show]), last(xss));
        return unlines(map(function (xs) {
            return concat(zipWith(function (w, x) {
                return justifyRight(w, ' ', show(x));
            }, ws, xs));
        }, xss));
    };


    // GENERIC FUNCTIONS ------------------------------------------------------

    // compose :: [(a -> a)] -> (a -> a)
    function compose(fs) {
        return function (x) {
            return fs.reduceRight(function (a, f) {
                return f(a);
            }, x);
        };
    };

    // concat :: [[a]] -> [a] | [String] -> String
    function concat(xs) {
        if (xs.length > 0) {
            var unit = typeof xs[0] === 'string' ? '' : [];
            return unit.concat.apply(unit, xs);
        } else return [];
    };

    // enumFromTo :: Int -> Int -> [Int]
    function enumFromTo(m, n) {
        return Array.from({
            length: Math.floor(n - m) + 1
        }, function (_, i) {
            return m + i;
        });
    };

    // justifyRight :: Int -> Char -> Text -> Text
    function justifyRight(n, cFiller, strText) {
        return n > strText.length ? (cFiller.repeat(n) + strText)
            .slice(-n) : strText;
    };

    // last :: [a] -> a
    function last(xs) {
        return xs.length ? xs.slice(-1)[0] : undefined;
    };

    // length :: [a] -> Int
    function length(xs) {
        return xs.length;
    };

    // map :: (a -> b) -> [a] -> [b]
    function map(f, xs) {
        return xs.map(f);
    };

    // 'The mapAccumL function behaves like a combination of map and foldl;
    // it applies a function to each element of a list, passing an accumulating
    // parameter from left to right, and returning a final value of this
    // accumulator together with the new list.' (See hoogle )

    // mapAccumL :: (acc -> x -> (acc, y)) -> acc -> [x] -> (acc, [y])
    function mapAccumL(f, acc, xs) {
        return xs.reduce(function (a, x) {
            var pair = f(a[0], x);

            return [pair[0], a[1].concat([pair[1]])];
        }, [acc, []]);
    };

    // show ::
    // (a -> String) f,  Num n =>
    // a -> maybe f -> maybe n -> String
    var show = JSON.stringify;

    // snd :: (a, b) -> b
    function snd(tpl) {
        return Array.isArray(tpl) ? tpl[1] : undefined;
    };

    // succ :: Int -> Int
    function succ(x) {
        return x + 1;
    };

    // unlines :: [String] -> String
    function unlines(xs) {
        return xs.join('\n');
    };

    // zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
    function zipWith(f, xs, ys) {
        var ny = ys.length;
        return (xs.length <= ny ? xs : xs.slice(0, ny))
            .map(function (x, i) {
                return f(x, ys[i]);
            });
    };

    // TEST ( n=5 and n=14 rows ) ---------------------------------------------

    return unlines(map(function (n) {
        return showFloyd(floyd(n)) + '\n';
    }, [5, 14]));
})();
```

{{Out}}

```txt
  1
  2  3
  4  5  6
  7  8  9 10
 11 12 13 14 15

  1
  2  3
  4  5  6
  7  8  9 10
 11 12 13 14 15
 16 17 18 19 20 21
 22 23 24 25 26 27 28
 29 30 31 32 33 34 35 36
 37 38 39 40 41 42 43 44  45
 46 47 48 49 50 51 52 53  54  55
 56 57 58 59 60 61 62 63  64  65  66
 67 68 69 70 71 72 73 74  75  76  77  78
 79 80 81 82 83 84 85 86  87  88  89  90  91
 92 93 94 95 96 97 98 99 100 101 102 103 104 105
```



### ES6

{{Trans|Haskell}} (mapAccumL version)

```JavaScript
(() => {
    'use strict';

    // FLOYD's TRIANGLE -------------------------------------------------------

    // floyd :: Int -> [[Int]]
    const floyd = n => snd(mapAccumL(
        (start, row) => [start + row + 1, enumFromTo(start, start + row)],
        1, enumFromTo(0, n - 1)
    ));

    // showFloyd :: [[Int]] -> String
    const showFloyd = xss => {
        const ws = map(compose([succ, length, show]), last(xss));
        return unlines(
            map(xs => concat(zipWith(
                    (w, x) => justifyRight(w, ' ', show(x)), ws, xs
                )),
                xss
            )
        );
    };

    // GENERIC FUNCTIONS ------------------------------------------------------

    // compose :: [(a -> a)] -> (a -> a)
    const compose = fs => x => fs.reduceRight((a, f) => f(a), x);

    // concat :: [[a]] -> [a] | [String] -> String
    const concat = xs => {
        if (xs.length > 0) {
            const unit = typeof xs[0] === 'string' ? '' : [];
            return unit.concat.apply(unit, xs);
        } else return [];
    };

    // enumFromTo :: Int -> Int -> [Int]
    const enumFromTo = (m, n) =>
        Array.from({
            length: Math.floor(n - m) + 1
        }, (_, i) => m + i);

    // justifyRight :: Int -> Char -> Text -> Text
    const justifyRight = (n, cFiller, strText) =>
        n > strText.length ? (
            (cFiller.repeat(n) + strText)
            .slice(-n)
        ) : strText;

    // last :: [a] -> a
    const last = xs => xs.length ? xs.slice(-1)[0] : undefined;

    // length :: [a] -> Int
    const length = xs => xs.length;

    // map :: (a -> b) -> [a] -> [b]
    const map = (f, xs) => xs.map(f)

    // 'The mapAccumL function behaves like a combination of map and foldl;
    // it applies a function to each element of a list, passing an accumulating
    // parameter from left to right, and returning a final value of this
    // accumulator together with the new list.' (See hoogle )

    // mapAccumL :: (acc -> x -> (acc, y)) -> acc -> [x] -> (acc, [y])
    const mapAccumL = (f, acc, xs) =>
        xs.reduce((a, x) => {
            const pair = f(a[0], x);

            return [pair[0], a[1].concat([pair[1]])];
        }, [acc, []]);

    // show ::
    // (a -> String) f,  Num n =>
    // a -> maybe f -> maybe n -> String
    const show = JSON.stringify;

    // snd :: (a, b) -> b
    const snd = tpl => Array.isArray(tpl) ? tpl[1] : undefined;

    // succ :: Int -> Int
    const succ = x => x + 1

    // unlines :: [String] -> String
    const unlines = xs => xs.join('\n');


    // zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
    const zipWith = (f, xs, ys) => {
        const ny = ys.length;
        return (xs.length <= ny ? xs : xs.slice(0, ny))
            .map((x, i) => f(x, ys[i]));
    };

    // TEST ( n=5 and n=14 rows ) ---------------------------------------------

    return unlines(map(n => showFloyd(floyd(n)) + '\n', [5, 14]))
})();
```

{{Out}}

```txt
  1
  2  3
  4  5  6
  7  8  9 10
 11 12 13 14 15

  1
  2  3
  4  5  6
  7  8  9 10
 11 12 13 14 15
 16 17 18 19 20 21
 22 23 24 25 26 27 28
 29 30 31 32 33 34 35 36
 37 38 39 40 41 42 43 44  45
 46 47 48 49 50 51 52 53  54  55
 56 57 58 59 60 61 62 63  64  65  66
 67 68 69 70 71 72 73 74  75  76  77  78
 79 80 81 82 83 84 85 86  87  88  89  90  91
 92 93 94 95 96 97 98 99 100 101 102 103 104 105
```



###  Spidermonkey 


(Used TCL example as a starting point.)


```javascript
#!/usr/bin/env js

function main() {
    print('Floyd 5:');
    floyd(5);
    print('\nFloyd 14:');
    floyd(14);
}


function padLeft(s, w) {
    for (s = String(s); s.length < w; s = ' ' + s);
    return s;
}


function floyd(nRows) {
    var lowerLeft = nRows * (nRows - 1) / 2 + 1;
    var lowerRight = nRows * (nRows + 1) / 2;
    
    var colWidths = [];
    for (var col = lowerLeft; col <= lowerRight; col++) {
        colWidths.push(String(col).length);
    }

    var  num = 1;
    for (var row = 0; row < nRows; row++) {
        var line = [];
        for (var col = 0; col <= row; col++, num++) {
            line.push(padLeft(num, colWidths[col]));
        }
        print(line.join(' '));
    }
}

main();
```


{{out}}

```txt
 Floyd 5:
  1
  2  3
  4  5  6
  7  8  9 10
 11 12 13 14 15
 
 Floyd 14:
  1
  2  3
  4  5  6
  7  8  9 10
 11 12 13 14 15
 16 17 18 19 20 21
 22 23 24 25 26 27 28
 29 30 31 32 33 34 35 36
 37 38 39 40 41 42 43 44  45
 46 47 48 49 50 51 52 53  54  55
 56 57 58 59 60 61 62 63  64  65  66
 67 68 69 70 71 72 73 74  75  76  77  78
 79 80 81 82 83 84 85 86  87  88  89  90  91
 92 93 94 95 96 97 98 99 100 101 102 103 104 105
```



## jq


```jq
# floyd(n) creates an n-row floyd's triangle
def floyd(n):
  def lpad(len): tostring | (((len - length) * " ")  + .);

  # Construct an array of widths.
  # Assuming N is the last integer on the last row (i.e. (n+1)*n/2),
  # the last row has n entries from (1+N-n) through N:
  def widths:
    ((n+1)*n/2) as $N
    | [range(1 + $N - n; $N + 1) | tostring | length];

  # emit line k assuming it starts with the integer "start"
  def line(start; k; widths):
    reduce range(start; start+k) as $i
      (""; . + ($i|lpad(widths[$i - start])) + " ");

  widths as $widths
  | (reduce range(0;n) as $row
      ( [0, ""];   # state: i, string
        (.[0] + 1) as $i | .[1] as $string
        | [ ($i + $row),
            ($string + "\n" + line($i; $row + 1; $widths )) ] )
    | .[1] ) ;
```

'''Task:'''

```jq
(5,14) | "floyd(\(.)): \(floyd(.))\n"
```

{{out}}

```sh
$ jq -M -r -n -f floyds_triangle.jq > floyds_triangle.out
floyd(5): 
 1 
 2  3 
 4  5  6 
 7  8  9 10 
11 12 13 14 15 

floyd(14): 
 1 
 2  3 
 4  5  6 
 7  8  9 10 
11 12 13 14 15 
16 17 18 19 20 21 
22 23 24 25 26 27 28 
29 30 31 32 33 34 35 36 
37 38 39 40 41 42 43 44  45 
46 47 48 49 50 51 52 53  54  55 
56 57 58 59 60 61 62 63  64  65  66 
67 68 69 70 71 72 73 74  75  76  77  78 
79 80 81 82 83 84 85 86  87  88  89  90  91 
92 93 94 95 96 97 98 99 100 101 102 103 104 105 
```



## Julia


```julia
function floydtriangle(rows)
    r = collect(1:div(rows *(rows + 1), 2))
    spacing = Int(ceil(log10(r[end] + 1))) + 1
    for i in 1:rows
        for _ in 1:i
            print(lpad(popfirst!(r), spacing))
        end
        println()
    end
end
 
floydtriangle(5); println(); floydtriangle(14)

```
{{out}}

```txt

  1
  2  3
  4  5  6
  7  8  9 10
 11 12 13 14 15

   1
   2   3
   4   5   6
   7   8   9  10
  11  12  13  14  15
  16  17  18  19  20  21
  22  23  24  25  26  27  28
  29  30  31  32  33  34  35  36
  37  38  39  40  41  42  43  44  45
  46  47  48  49  50  51  52  53  54  55
  56  57  58  59  60  61  62  63  64  65  66
  67  68  69  70  71  72  73  74  75  76  77  78
  79  80  81  82  83  84  85  86  87  88  89  90  91
  92  93  94  95  96  97  98  99 100 101 102 103 104 105

```



## Kotlin

{{trans|Java}}

```scala
fun main(args: Array<String>) = args.forEach { Triangle(it.toInt()) }

internal class Triangle(n: Int) {
    init {
        println("$n rows:")
        var printMe = 1
        var printed = 0
        var row = 1
        while (row <= n) {
            val cols = Math.ceil(Math.log10(n * (n - 1) / 2 + printed + 2.0)).toInt()
            print("%${cols}d ".format(printMe))
            if (++printed == row) { println(); row++; printed = 0 }
            printMe++
        }
    }
}
```

Output as Java.


## Lasso

{{Output?|Lasso|There should only be one space between the numbers on the last row.}}

```Lasso
define floyds_triangle(n::integer) => {
	local(out = array(array(1)),comp = array, num = 1)
	while(#out->size < #n) => {
		local(new = array)
		loop(#out->last->size + 1) => {
			#num++
			#new->insert(#num)
		}
		#out->insert(#new)
	}
	local(pad = #out->last->last->asString->size)
	with line in #out do => {
		local(lineout = string)
		with i in #line do => {
			#i != #line->first ? #lineout->append(' ')
			#lineout->append((' '*(#pad - #i->asString->size))+#i)
		}
		#comp->insert(#lineout)
	}
	return #comp->join('\r')
}
floyds_triangle(5)
'\r\r'
floyds_triangle(14)
```

{{out}}

```txt
 1
 2  3
 4  5  6
 7  8  9 10
11 12 13 14 15

  1
  2   3
  4   5   6
  7   8   9  10
 11  12  13  14  15
 16  17  18  19  20  21
 22  23  24  25  26  27  28
 29  30  31  32  33  34  35  36
 37  38  39  40  41  42  43  44  45
 46  47  48  49  50  51  52  53  54  55
 56  57  58  59  60  61  62  63  64  65  66
 67  68  69  70  71  72  73  74  75  76  77  78
 79  80  81  82  83  84  85  86  87  88  89  90  91
 92  93  94  95  96  97  98  99 100 101 102 103 104 105
```


=={{Header|Liberty BASIC}}==

```lb
input "Number of rows needed:- "; rowsNeeded

dim colWidth(rowsNeeded)    '    5 rows implies 5 columns

for col=1 to rowsNeeded
    colWidth(col) = len(str$(col + rowsNeeded*(rowsNeeded-1)/2))
next

currentNumber =1

for row=1 to rowsNeeded
    for col=1 to row
        print right$( "  "+str$( currentNumber), colWidth(col)); " ";
        currentNumber = currentNumber + 1
    next
    print
next
```

{{out}}

```txt
Number of rows needed:- 5
 1 
 2  3 
 4  5  6 
 7  8  9 10 
11 12 13 14 15 

Number of rows needed:- 14
 1 
 2  3 
 4  5  6 
 7  8  9 10 
11 12 13 14 15 
16 17 18 19 20 21 
22 23 24 25 26 27 28 
29 30 31 32 33 34 35 36 
37 38 39 40 41 42 43 44  45 
46 47 48 49 50 51 52 53  54  55 
56 57 58 59 60 61 62 63  64  65  66 
67 68 69 70 71 72 73 74  75  76  77  78 
79 80 81 82 83 84 85 86  87  88  89  90  91 
92 93 94 95 96 97 98 99 100 101 102 103 104 105 
```


=={{Header|Lua}}==

```lua
function print_floyd(rows)
	local c = 1
	local h = rows*(rows-1)/2
	for i=1,rows do
		local s = ""
		for j=1,i do
			for k=1, #tostring(h+j)-#tostring(c) do
				s = s .. " "
			end
			if j ~= 1 then s = s .. " " end
			s = s .. tostring(c)
			c = c + 1
		end
		print(s)
	end
end

print_floyd(5)
print_floyd(14)
```


Output:

```txt

 1
 2  3
 4  5  6
 7  8  9 10
11 12 13 14 15
 1
 2  3
 4  5  6
 7  8  9 10
11 12 13 14 15
16 17 18 19 20 21
22 23 24 25 26 27 28
29 30 31 32 33 34 35 36
37 38 39 40 41 42 43 44  45
46 47 48 49 50 51 52 53  54  55
56 57 58 59 60 61 62 63  64  65  66
67 68 69 70 71 72 73 74  75  76  77  78
79 80 81 82 83 84 85 86  87  88  89  90  91
92 93 94 95 96 97 98 99 100 101 102 103 104 105
```



## Maple


```maple
floyd := proc(rows)
	local num, numRows, numInRow, i, digits;
	digits := Array([]);
	for i to 2 do
		num := 1;
		numRows := 1;
		numInRow := 1;
		while numRows <= rows do
			if i = 2 then
				printf(cat("%", digits[numInRow], "a "), num);
			end if;
			num := num + 1;
			if i = 1 and numRows = rows then
				digits(numInRow) := StringTools[Length](convert(num-1, string));
			end if;
			if numInRow >= numRows then
				if i = 2 then
					printf("\n");
				end if;
				numInRow := 1;
				numRows := numRows + 1;
			else
				numInRow := numInRow +1;
			end if;
		end do;
	end do;
	return NULL;
end proc:

floyd(5);
floyd(14);
```

{{out}}

```txt

 1 
 2  3 
 4  5  6 
 7  8  9 10 
11 12 13 14 15 
 1 
 2  3 
 4  5  6 
 7  8  9 10 
11 12 13 14 15 
16 17 18 19 20 21 
22 23 24 25 26 27 28 
29 30 31 32 33 34 35 36 
37 38 39 40 41 42 43 44  45 
46 47 48 49 50 51 52 53  54  55 
56 57 58 59 60 61 62 63  64  65  66 
67 68 69 70 71 72 73 74  75  76  77  78 
79 80 81 82 83 84 85 86  87  88  89  90  91 
92 93 94 95 96 97 98 99 100 101 102 103 104 105 

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica

f=Function[n,
	Most/@(Range@@@Partition[FindSequenceFunction[{1,2,4,7,11}]/@Range[n+1],2,1])]
TableForm[f@5,TableAlignments->Right,TableSpacing->{1,1}]
TableForm[f@14,TableAlignments->Right,TableSpacing->{1,1}]

```

Output:

```txt

  1
  2  3
  4  5  6
  7  8  9 10
 11 12 13 14 15

  1
  2  3
  4  5  6
  7  8  9 10
 11 12 13 14 15
 16 17 18 19 20 21
 22 23 24 25 26 27 28
 29 30 31 32 33 34 35 36
 37 38 39 40 41 42 43 44  45
 46 47 48 49 50 51 52 53  54  55
 56 57 58 59 60 61 62 63  64  65  66
 67 68 69 70 71 72 73 74  75  76  77  78
 79 80 81 82 83 84 85 86  87  88  89  90  91
 92 93 94 95 96 97 98 99 100 101 102 103 104 105
```


=={{header|MATLAB}} / {{header|Octave}}== 


```Matlab
function floyds_triangle(n)
  s = 1;
  for k = 1 : n
    disp(s : s + k - 1)
    s = s + k;
  end
```

{{out}}:

```txt

octave:22> floyds_triangle(5)
  1
  2  3
  4  5  6
  7  8  9 10
 11 12 13 14 15

```


=={{header|Modula-2}}==

```modula2
MODULE FloydTriangle;
FROM FormatString IMPORT FormatString;
FROM Terminal IMPORT WriteString,WriteLn,ReadChar;

PROCEDURE WriteInt(n : INTEGER);
VAR buf : ARRAY[0..9] OF CHAR;
BEGIN
    FormatString("%4i", buf, n);
    WriteString(buf)
END WriteInt;

PROCEDURE Print(r : INTEGER);
VAR n,i,limit : INTEGER;
BEGIN
    IF r<0 THEN RETURN END;

    n := 1;
    limit := 1;
    WHILE r#0 DO
        FOR i:=1 TO limit DO
            WriteInt(n);
            INC(n)
        END;
        WriteLn;

        DEC(r);
        INC(limit)
    END
END Print;

BEGIN
    Print(5);
    WriteLn;
    Print(14);

    ReadChar
END FloydTriangle.
```

{{out}}

```txt
  1
  2  3
  4  5  6
  7  8  9 10
 11 12 13 14 15

  1
  2  3
  4  5  6
  7  8  9 10
 11 12 13 14 15
 16 17 18 19 20 21
 22 23 24 25 26 27 28
 29 30 31 32 33 34 35 36
 37 38 39 40 41 42 43 44  45
 46 47 48 49 50 51 52 53  54  55
 56 57 58 59 60 61 62 63  64  65  66
 67 68 69 70 71 72 73 74  75  76  77  78
 79 80 81 82 83 84 85 86  87  88  89  90  91
 92 93 94 95 96 97 98 99 100 101 102 103 104 105
```



## NetRexx

Both [[#REXX|REXX]] versions lend themselves very well to conversion into NetRexx programs with few changes.

### Version 1

{{Trans|REXX}}

```NetRexx
/* NetRexx */
options replace format comments java crossref symbols binary
/* REXX ***************************************************************
* 12.07.2012 Walter Pachl  - translated from Python
**********************************************************************/
Parse Arg rowcount .
if rowcount.length() == 0 then rowcount = 1
say 'Rows:' rowcount
say
col = 0
len = Rexx ''
ll = ''                               -- last line of triangle
Loop j = rowcount * (rowcount - 1) / 2 + 1 to rowcount * (rowcount + 1) / 2
  col = col + 1                       -- column number
  ll = ll j                           -- build last line
  len[col] = j.length()               -- remember length of column
  End j
Loop i = 1 To rowcount - 1            -- now do and output the rest
  ol = ''
  col = 0
  Loop j = i * (i - 1) / 2 + 1 to i * (i + 1) / 2 -- elements of line i
    col = col + 1
    ol=ol j.right(len[col])           -- element in proper length
    end
  Say ol                              -- output ith line
  end i
Say ll                                -- output last line

```

'''Output:

```txt

Rows: 5 
 
  1 
  2  3 
  4  5  6 
  7  8  9 10 
 11 12 13 14 15 

Rows: 14 
 
  1 
  2  3 
  4  5  6 
  7  8  9 10 
 11 12 13 14 15 
 16 17 18 19 20 21 
 22 23 24 25 26 27 28 
 29 30 31 32 33 34 35 36 
 37 38 39 40 41 42 43 44  45 
 46 47 48 49 50 51 52 53  54  55 
 56 57 58 59 60 61 62 63  64  65  66 
 67 68 69 70 71 72 73 74  75  76  77  78 
 79 80 81 82 83 84 85 86  87  88  89  90  91 
 92 93 94 95 96 97 98 99 100 101 102 103 104 105 
```



### Version 2

{{Trans|REXX}}

```NetRexx
/* NetRexx */
options replace format comments java crossref symbols binary
/*REXX program constructs & displays Floyd's triangle for any number of rows.*/
parse arg numRows .
if numRows == '' then numRows = 1     -- assume 1 row if not given
maxVal = numRows * (numRows + 1) % 2  -- calculate the max value.
say 'displaying a' numRows "row Floyd's triangle:"
say
digit = 1
loop row = 1 for numRows
  col = 0
  output = ''
  loop digit = digit for row
    col = col + 1
    colMaxDigit = maxVal - numRows + col
    output = output Rexx(digit).right(colMaxDigit.length())
    end digit
  say output
  end row

```


'''Output:

```txt

displaying a 5 row Floyd's triangle: 
 
  1 
  2  3 
  4  5  6 
  7  8  9 10 
 11 12 13 14 15
  
displaying a 14 row Floyd's triangle: 
 
  1 
  2  3 
  4  5  6 
  7  8  9 10 
 11 12 13 14 15 
 16 17 18 19 20 21 
 22 23 24 25 26 27 28 
 29 30 31 32 33 34 35 36 
 37 38 39 40 41 42 43 44  45 
 46 47 48 49 50 51 52 53  54  55 
 56 57 58 59 60 61 62 63  64  65  66 
 67 68 69 70 71 72 73 74  75  76  77  78 
 79 80 81 82 83 84 85 86  87  88  89  90  91 
 92 93 94 95 96 97 98 99 100 101 102 103 104 105

```



## Nim

{{trans|Python}}

```nim
import strutils

proc floyd(rowcount = 5): seq[seq[int]] =
  result = @[@[1]]
  while result.len < rowcount:
    let n = result[result.high][result.high] + 1
    var row = newSeq[int]()
    for i in n .. n + result[result.high].len:
      row.add i
    result.add row

proc pfloyd(rows: seq[seq[int]]) =
  var colspace = newSeq[int]()
  for n in rows[rows.high]: colspace.add(($n).len)
  for row in rows:
    for i, x in row:
      stdout.write align($x, colspace[i])," "
    echo ""

echo floyd()

for i in [5, 14]:
  pfloyd(floyd(i))
  echo ""
```

Output:

```txt
@[@[1], @[2, 3], @[4, 5, 6], @[7, 8, 9, 10], @[11, 12, 13, 14, 15]]
 1 
 2  3 
 4  5  6 
 7  8  9 10 
11 12 13 14 15 

 1 
 2  3 
 4  5  6 
 7  8  9 10 
11 12 13 14 15 
16 17 18 19 20 21 
22 23 24 25 26 27 28 
29 30 31 32 33 34 35 36 
37 38 39 40 41 42 43 44  45 
46 47 48 49 50 51 52 53  54  55 
56 57 58 59 60 61 62 63  64  65  66 
67 68 69 70 71 72 73 74  75  76  77  78 
79 80 81 82 83 84 85 86  87  88  89  90  91 
92 93 94 95 96 97 98 99 100 101 102 103 104 105 
```



## OCaml



```ocaml
let ( |> ) f g x = g (f x)
let rec last = function x::[] -> x | _::tl -> last tl | [] -> raise Not_found
let rec list_map2 f l1 l2 =
  match (l1, l2) with
  | ([], _) | (_, []) -> []
  | (x::xs, y::ys) -> (f x y) :: list_map2 f xs ys

let floyd n =
  let rec aux acc cur len i j =
    if (List.length acc) = n then (List.rev acc) else
      if j = len
      then aux ((List.rev cur)::acc) [] (succ len) i 0
      else aux acc (i::cur) len (succ i) (succ j)
  in
  aux [] [] 1 1 0

let print_floyd f =
  let lens = List.map (string_of_int |> String.length) (last f) in
  List.iter (fun row ->
    print_endline (
      String.concat " " (
        list_map2 (Printf.sprintf "%*d") lens row))
  ) f

let () =
  print_floyd (floyd (int_of_string Sys.argv.(1)))
```



## OxygenBasic

{{output?|OxygenBasic}}

```oxygenbasic

function Floyd(sys n) as string
sys i,t
for i=1 to n
  t+=i
next
string s=str t
sys le=1+len s
string cr=chr(13,10)
sys lc=len cr
string buf=space(le*t+n*lc)
sys j,o,p=1
t=0
for i=1 to n
  for j=1 to i
    t++
    s=str t
    o=le-len(s)-1 'right justify
    mid buf,p+o,str t
    p+=le
  next
  mid buf,p,cr
  p+=lc
next
return left buf,p-1
end function

putfile "s.txt",Floyd(5)+floyd(14)

```



## PARI/GP


{{incorrect|PARI/GP|It does not ensure that there is exactly one space between the columns in the last row.}}


```parigp
F(n)=my(fmt=Str("%"1+#Str(n*(n+1)/2)"d"),t);for(i=1,n,for(j=1,i,printf(fmt,t++));print)
F(5)
F(14)
```

{{out}}

```txt
  1
  2  3
  4  5  6
  7  8  9 10
 11 12 13 14 15
   1
   2   3
   4   5   6
   7   8   9  10
  11  12  13  14  15
  16  17  18  19  20  21
  22  23  24  25  26  27  28
  29  30  31  32  33  34  35  36
  37  38  39  40  41  42  43  44  45
  46  47  48  49  50  51  52  53  54  55
  56  57  58  59  60  61  62  63  64  65  66
  67  68  69  70  71  72  73  74  75  76  77  78
  79  80  81  82  83  84  85  86  87  88  89  90  91
  92  93  94  95  96  97  98  99 100 101 102 103 104 105
```



## Pascal

{{works with|Free_Pascal}}

```pascal
Program FloydDemo (input, output);

function digits(number: integer): integer;
  begin
    digits := trunc(ln(number) / ln(10)) + 1;
  end;

procedure floyd1 (numberOfLines: integer);
{ variant with repeat .. until loop }
  var
    i, j, numbersInLine, startOfLastlLine: integer;
 
  begin
    startOfLastlLine := (numberOfLines - 1) * numberOfLines div 2 + 1;
    i := 1;
    j := 1;
    numbersInLine := 1;
    repeat
      repeat
        write(i: digits(startOfLastlLine - 1 + j), ' ');
        inc(i);
	inc(j);
      until (j > numbersInLine);
      writeln;
      j := 1;
      inc(numbersInLine);
    until (numbersInLine > numberOfLines);
  end;
 
procedure floyd2 (numberOfLines: integer);
{ Variant with for .. do loop }
  var
    i, j, numbersInLine, startOfLastlLine: integer;
 
  begin
    startOfLastlLine := (numberOfLines - 1) * numberOfLines div 2 + 1;
    i := 1;
    for numbersInLine := 1 to numberOfLines do
    begin
      for j := 1 to numbersInLine do
      begin
        write(i: digits(startOfLastlLine - 1 + j), ' ');
        inc(i);
      end;
      writeln;
    end;
  end;
 
begin
  writeln ('*** Floyd 5 ***');
  floyd1(5);
  writeln;
  writeln ('*** Floyd 14 ***');
  floyd2(14);
end.
```

Output:

```txt
% ./Floyd
*** Floyd 5 ***
 1 
 2  3 
 4  5  6 
 7  8  9 10 
11 12 13 14 15 

*** Floyd 14 ***
 1 
 2  3 
 4  5  6 
 7  8  9 10 
11 12 13 14 15 
16 17 18 19 20 21 
22 23 24 25 26 27 28 
29 30 31 32 33 34 35 36 
37 38 39 40 41 42 43 44  45 
46 47 48 49 50 51 52 53  54  55 
56 57 58 59 60 61 62 63  64  65  66 
67 68 69 70 71 72 73 74  75  76  77  78 
79 80 81 82 83 84 85 86  87  88  89  90  91 
92 93 94 95 96 97 98 99 100 101 102 103 104 105 
```



## Perl

{{Trans|NetRexx}}

```perl
#!/usr/bin/env perl
use strict;
use warnings;

sub displayFloydTriangle {
  my $numRows = shift;
  print "\ndisplaying a $numRows row Floyd's triangle:\n\n";
  my $maxVal = int($numRows * ($numRows + 1) / 2); # calculate the max value.
  my $digit = 0;
  foreach my $row (1 .. $numRows) {
    my $col = 0;
    my $output = '';
    foreach (1 .. $row) {
      ++$digit;
      ++$col;
      my $colMaxDigit = $maxVal - $numRows + $col;
      $output .= sprintf " %*d", length($colMaxDigit), $digit;
    }
    print "$output\n";
  }
  return;
}

# 
### = Main =============================================

my @counts;
@counts = @ARGV;
@counts = (5, 14) unless @ARGV;

foreach my $count (@counts) {
  displayFloydTriangle($count);
}

0;
__END__

```

'''Output:

```txt

displaying a 5 row Floyd's triangle:

  1
  2  3
  4  5  6
  7  8  9 10
 11 12 13 14 15

displaying a 14 row Floyd's triangle:

  1
  2  3
  4  5  6
  7  8  9 10
 11 12 13 14 15
 16 17 18 19 20 21
 22 23 24 25 26 27 28
 29 30 31 32 33 34 35 36
 37 38 39 40 41 42 43 44  45
 46 47 48 49 50 51 52 53  54  55
 56 57 58 59 60 61 62 63  64  65  66
 67 68 69 70 71 72 73 74  75  76  77  78
 79 80 81 82 83 84 85 86  87  88  89  90  91
 92 93 94 95 96 97 98 99 100 101 102 103 104 105

```



## Perl 6

Here's two ways of doing it.

```perl6
constant @floyd1 = (1..*).rotor(1..*);
constant @floyd2 = gather for 1..* -> $s { take [++$ xx $s] }

sub format-rows(@f) {
    my @table;
    my @formats = @f[@f-1].map: {"%{.chars}s"}
    for @f -> @row {
        @table.push: (@row Z @formats).map: -> ($i, $f) { $i.fmt($f) }
    }
    join "\n", @table;
}

say format-rows(@floyd1[^5]);
say '';
say format-rows(@floyd2[^14]);
```

{{out}}

```txt
 1 
 2  3 
 4  5  6 
 7  8  9 10 
11 12 13 14 15
 
 1 
 2  3 
 4  5  6 
 7  8  9 10 
11 12 13 14 15 
16 17 18 19 20 21 
22 23 24 25 26 27 28 
29 30 31 32 33 34 35 36 
37 38 39 40 41 42 43 44  45 
46 47 48 49 50 51 52 53  54  55 
56 57 58 59 60 61 62 63  64  65  66 
67 68 69 70 71 72 73 74  75  76  77  78 
79 80 81 82 83 84 85 86  87  88  89  90  91 
92 93 94 95 96 97 98 99 100 101 102 103 104 105
```



## Phix


```Phix
procedure Floyds_triangle(integer n)
sequence widths = repeat(0,n)
    integer k = (n * (n-1))/2
    for i=1 to n do
        widths[i] = sprintf("%%%dd",length(sprintf("%d",i+k))+1)
    end for
    k = 1
    for i=1 to n do
        for j=1 to i do
            printf(1,widths[j],k)
            k += 1
        end for
        printf(1,"\n")
    end for
end procedure
Floyds_triangle(5)
Floyds_triangle(14)
```

{{out}}
<pre style="float:left">
  1
  2  3
  4  5  6
  7  8  9 10
 11 12 13 14 15

```


```txt

  1
  2  3
  4  5  6
  7  8  9 10
 11 12 13 14 15
 16 17 18 19 20 21
 22 23 24 25 26 27 28
 29 30 31 32 33 34 35 36
 37 38 39 40 41 42 43 44  45
 46 47 48 49 50 51 52 53  54  55
 56 57 58 59 60 61 62 63  64  65  66
 67 68 69 70 71 72 73 74  75  76  77  78
 79 80 81 82 83 84 85 86  87  88  89  90  91
 92 93 94 95 96 97 98 99 100 101 102 103 104 105

```



## PHP



```php

<?php
floyds_triangle(5);
floyds_triangle(14);

function floyds_triangle($n) {
    echo "n = " . $n . "\r\n";

    for($r = 1, $i = 1, $c = 0; $r <= $n; $i++) {
        $cols = ceil(log10($n*($n-1)/2 + $c + 2));
        printf("%".$cols."d ", $i);
        if(++$c == $r) {
            echo "\r\n";
            $r++;
            $c = 0;
        }
    }
?>

```

{{out}}

```txt
 
n = 5
 1 
 2  3 
 4  5  6 
 7  8  9 10 
11 12 13 14 15 
n = 14
 1 
 2  3 
 4  5  6 
 7  8  9 10 
11 12 13 14 15 
16 17 18 19 20 21 
22 23 24 25 26 27 28 
29 30 31 32 33 34 35 36 
37 38 39 40 41 42 43 44  45 
46 47 48 49 50 51 52 53  54  55 
56 57 58 59 60 61 62 63  64  65  66 
67 68 69 70 71 72 73 74  75  76  77  78 
79 80 81 82 83 84 85 86  87  88  89  90  91 
92 93 94 95 96 97 98 99 100 101 102 103 104 105 

```



## PicoLisp


### Calculate widths relative to lower left corner


```PicoLisp
(de floyd (N)
   (let LLC (/ (* N (dec N)) 2)
      (for R N
         (for C R
            (prin
               (align
                  (length (+ LLC C))
                  (+ C (/ (* R (dec R)) 2)) ) )
            (if (= C R) (prinl) (space)) ) ) ) )
```

===Pre-calculate all rows, and take format from last one===

```PicoLisp
(de floyd (N)
   (let
      (Rows
         (make
            (for ((I . L) (range 1 (/ (* N (inc N)) 2))  L)
               (link (cut I 'L)) ) )
         Fmt (mapcar length (last Rows)) )
      (map inc (cdr Fmt))
      (for R Rows
         (apply tab R Fmt) ) ) )
```

Output in both cases:

```txt
: (floyd 5)
 1
 2  3
 4  5  6
 7  8  9 10
11 12 13 14 15

: (floyd 14)
 1
 2  3
 4  5  6
 7  8  9 10
11 12 13 14 15
16 17 18 19 20 21
22 23 24 25 26 27 28
29 30 31 32 33 34 35 36
37 38 39 40 41 42 43 44  45
46 47 48 49 50 51 52 53  54  55
56 57 58 59 60 61 62 63  64  65  66
67 68 69 70 71 72 73 74  75  76  77  78
79 80 81 82 83 84 85 86  87  88  89  90  91
92 93 94 95 96 97 98 99 100 101 102 103 104 105
```



## PL/I


```pli
(fofl, size):
floyd: procedure options (main); /* Floyd's Triangle. Wiki 12 July 2012 */

   declare (i, m, n) fixed (10), (j, k, w, nr) fixed binary;

   put list ('How many rows do you want?');
   get list (nr);   /* the number of rows   */
   n = nr*(nr+1)/2; /* the total number of values */

   j,k = 1; m = n - nr + 1;
   do i = 1 to n;
      put edit (i) ( x(1), f(length(trim(m))) );
      if k > 1 then do; k = k - 1; m = m + 1; end;
      else do; k,j = j + 1; m = n - nr + 1; put skip; end;
   end;

end floyd;
```


{{out}}

```txt

How many rows do you want?
  1
  2  3
  4  5  6
  7  8  9 10
 11 12 13 14 15

How many rows do you want? 
  1
  2  3
  4  5  6
  7  8  9 10
 11 12 13 14 15
 16 17 18 19 20 21
 22 23 24 25 26 27 28
 29 30 31 32 33 34 35 36
 37 38 39 40 41 42 43 44  45
 46 47 48 49 50 51 52 53  54  55
 56 57 58 59 60 61 62 63  64  65  66
 67 68 69 70 71 72 73 74  75  76  77  78
 79 80 81 82 83 84 85 86  87  88  89  90  91
 92 93 94 95 96 97 98 99 100 101 102 103 104 105

Final row for n=45:
 991 992 993 994 995 996 997 998 999 1000 1001 1002 1003 1004 1005 1006 1007 1008 1009 1010 1011 1012 1013 1014 1015 1016 1017 1018 1019 1020 1021 1022 1023 1024 1025 1026 1027 1028 1029 1030 1031 1032 1033 1034 1035

```



## Prolog

Works with SWI-Prolog version 6.5.3

```Prolog
floyd(N) :-
	forall(between(1, N, I),
	       (   forall(between(1,I, J),
			  (   Last is N * (N-1)/2+J,
			      V is I * (I-1) /2 + J,
			      get_column(Last, C),
			      sformat(AR, '~~t~~w~~~w| ', [C]),
			      sformat(AF, AR, [V]),
			      writef(AF))),
	       nl)).

get_column(Last, C) :-
	name(Last, N1), length(N1,C).

```

Output :

```txt
 ?- floyd(5).
 1 
 2  3 
 4  5  6 
 7  8  9 10 
11 12 13 14 15 
true.

 ?- floyd(14).
 1 
 2  3 
 4  5  6 
 7  8  9 10 
11 12 13 14 15 
16 17 18 19 20 21 
22 23 24 25 26 27 28 
29 30 31 32 33 34 35 36 
37 38 39 40 41 42 43 44  45 
46 47 48 49 50 51 52 53  54  55 
56 57 58 59 60 61 62 63  64  65  66 
67 68 69 70 71 72 73 74  75  76  77  78 
79 80 81 82 83 84 85 86  87  88  89  90  91 
92 93 94 95 96 97 98 99 100 101 102 103 104 105 
true.

```



## PureBasic



```PureBasic
Procedure.i sumTo(n) 
  Protected r,i
  For i=1 To n 
    r+i
  Next 
  ProcedureReturn r.i
EndProcedure
 
; [1]
; array rsA(n)... string-lengths of the numbers
; in the bottom row
 
; [2]
; sumTo(i-1)+1    to     sumTo(i)
           ; 11 12 13 14 15
  ; here k is the column-index for array rsA(k)
 
Procedure.s FloydsTriangle(n)
  Protected r.s,s.s,t.s,i,j,k
  ; [1]
  Dim rsA(n)
  i=0
  For j=sumTo(n-1)+1 To sumTo(n)
    i+1
    rsA(i)=Len(Str(j))
  Next
  ; [2]
  For i=1 To n 
    t.s="":k=0
    For j=sumTo(i-1)+1 To sumTo(i)
      k+1:t.s+RSet(Str(j),rsA(k)," ")+" "
    Next 
    r.s+RTrim(t.s)+Chr(13)+Chr(10)
  Next 
  r.s=Left(r.s,Len(r.s)-2)
  ProcedureReturn r.s
EndProcedure
 
If OpenConsole() 
  n=5
  r.s=FloydsTriangle(n)
  PrintN(r.s)
  
  n=14
  r.s=FloydsTriangle(n)
  PrintN(r.s)
  
  Print(#crlf$ + #crlf$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
```


'''Sample Output'''

```txt

 1
 2  3
 4  5  6
 7  8  9 10
11 12 13 14 15
 1
 2  3
 4  5  6
 7  8  9 10
11 12 13 14 15
16 17 18 19 20 21
22 23 24 25 26 27 28
29 30 31 32 33 34 35 36
37 38 39 40 41 42 43 44  45
46 47 48 49 50 51 52 53  54  55
56 57 58 59 60 61 62 63  64  65  66
67 68 69 70 71 72 73 74  75  76  77  78
79 80 81 82 83 84 85 86  87  88  89  90  91
92 93 94 95 96 97 98 99 100 101 102 103 104 105

```



## Python


### Procedural


```python>>>
 def floyd(rowcount=5):
	rows = [[1]]
	while len(rows) < rowcount:
		n = rows[-1][-1] + 1
		rows.append(list(range(n, n + len(rows[-1]) + 1)))
	return rows

>>> floyd()
[[1], [2, 3], [4, 5, 6], [7, 8, 9, 10], [11, 12, 13, 14, 15]]
>>> def pfloyd(rows=[[1], [2, 3], [4, 5, 6], [7, 8, 9, 10]]):
	colspace = [len(str(n)) for n in rows[-1]]
	for row in rows:
		print( ' '.join('%*i' % space_n for space_n in zip(colspace, row)))

		
>>> pfloyd()
1
2 3
4 5 6
7 8 9 10
>>> pfloyd(floyd(5))
 1
 2  3
 4  5  6
 7  8  9 10
11 12 13 14 15
>>> pfloyd(floyd(14))
 1
 2  3
 4  5  6
 7  8  9 10
11 12 13 14 15
16 17 18 19 20 21
22 23 24 25 26 27 28
29 30 31 32 33 34 35 36
37 38 39 40 41 42 43 44  45
46 47 48 49 50 51 52 53  54  55
56 57 58 59 60 61 62 63  64  65  66
67 68 69 70 71 72 73 74  75  76  77  78
79 80 81 82 83 84 85 86  87  88  89  90  91
92 93 94 95 96 97 98 99 100 101 102 103 104 105
>>> 
```



### Functional


Using the mathematical formula for each row directly,
either in a list comprehension:

```python
def floyd(rowcount=5):
    return [list(range(i * (i - 1) // 2 + 1, i * (i + 1) // 2 + 1))
            for i in range(1, rowcount + 1)]
```


or in terms of concatMap:
{{Works with|Python|3}}

```python
'''Floyd triangle in terms of concatMap'''

from itertools import chain


# floyd :: Int -> [[Int]]
def floyd(n):
    '''n rows of a Floyd triangle.'''
    def f(i):
        return [
            enumFromTo(i * pred(i) // 2 + 1)(
                i * succ(i) // 2
            )
        ]
    return concatMap(f)(enumFromTo(1)(n))


# main :: IO ()
def main():
    '''Test'''
    print(unlines(
        map(str, floyd(5))
    ))


# GENERIC FUNCTIONS ---------------------------------------


# enumFromTo :: (Int, Int) -> [Int]
def enumFromTo(m):
    '''Integer enumeration from m to n.'''
    return lambda n: list(range(m, 1 + n))


# concatMap :: (a -> [b]) -> [a] -> [b]
def concatMap(f):
    '''Concatenated list over which a function has been mapped.
       The list monad can be derived by using a function f which
       wraps its output in a list,
       (using an empty list to represent computational failure).'''
    return lambda xs: list(
        chain.from_iterable(
            map(f, xs)
        )
    )


# pred ::  Enum a => a -> a
def pred(x):
    '''The predecessor of a value. For numeric types, (- 1).'''
    return x - 1 if isinstance(x, int) else (
        chr(ord(x) - 1)
    )


# succ :: Enum a => a -> a
def succ(x):
    '''The successor of a value. For numeric types, (1 +).'''
    return 1 + x if isinstance(x, int) else (
        chr(1 + ord(x))
    )


# unlines :: [String] -> String
def unlines(xs):
    '''A single string derived by the intercalation
       of a list of strings with the newline character.'''
    return '\n'.join(xs)


if __name__ == '__main__':
    main()
```


Or alternatively, defining just the relationship between successive terms:
{{Works with|Python|3}}

```python
'''Floyd triangle in terms of iterate(f)(x)'''

from itertools import islice


# floyd :: Int -> [[Int]]
def floyd(n):
    '''n rows of a Floyd triangle.'''
    return take(n)(iterate(nextFloyd)([1]))


# nextFloyd :: [Int] -> [Int]
def nextFloyd(xs):
    '''A Floyd triangle row derived from
       the preceding row.'''
    n = succ(len(xs))
    return [1] if n < 2 else (
        enumFromTo(succ(n * pred(n) // 2))(
            n * succ(n) // 2
        )
    )


# showFloyd :: [[Int]] -> String
def showFloyd(xs):
    '''A stringification of Floyd triangle rows.'''
    return unlines(str(x) for x in xs)


# main :: IO ()
def main():
    '''Test'''
    print(showFloyd(
        floyd(5)
    ))


# GENERIC ABSTRACTIONS ------------------------------------

# enumFromTo :: (Int, Int) -> [Int]
def enumFromTo(m):
    '''Integer enumeration from m to n.'''
    return lambda n: list(range(m, 1 + n))


# iterate :: (a -> a) -> a -> Gen [a]
def iterate(f):
    '''An infinite list of repeated applications of f to x.'''
    def go(x):
        v = x
        while True:
            yield v
            v = f(v)
    return lambda x: go(x)


# pred ::  Enum a => a -> a
def pred(x):
    '''The predecessor of a value. For numeric types, (- 1).'''
    return x - 1 if isinstance(x, int) else (
        chr(ord(x) - 1)
    )


# succ :: Enum a => a -> a
def succ(x):
    '''The successor of a value. For numeric types, (1 +).'''
    return 1 + x if isinstance(x, int) else (
        chr(1 + ord(x))
    )


# take :: Int -> [a] -> [a]
# take :: Int -> String -> String
def take(n):
    '''The prefix of xs of length n,
       or xs itself if n > length xs.'''
    return lambda xs: (
        xs[0:n]
        if isinstance(xs, list)
        else list(islice(xs, n))
    )


# unlines :: [String] -> String
def unlines(xs):
    '''A single string derived by the intercalation
       of a list of strings with the newline character.'''
    return '\n'.join(xs)


# MAIN ----------------------------------------------------
if __name__ == '__main__':
    main()
```

{{Out}}

```txt
[1]
[2, 3]
[4, 5, 6]
[7, 8, 9, 10]
[11, 12, 13, 14, 15]
```



## Racket


```racket

#lang racket
(require math)

(define (tri n)
  (if (zero? n) 0 (triangle-number n)))

(define (floyd n)
  (define (width x) (string-length (~a x)))
  (define (~n x c) (~a x 
                       #:width (width (+ (tri (- n 1)) 1 c))
                       #:align 'right #:left-pad-string " "))
  (for ([r n])
    (for ([c (+ r 1)]) 
      (display (~a (~n (+ (tri r) 1 c) c) " ")))
    (newline)))

(floyd 5)
(floyd 14)

```

Output:

```txt

 1 
 2  3 
 4  5  6 
 7  8  9 10 
11 12 13 14 15 
 1 
 2  3 
 4  5  6 
 7  8  9 10 
11 12 13 14 15 
16 17 18 19 20 21 
22 23 24 25 26 27 28 
29 30 31 32 33 34 35 36 
37 38 39 40 41 42 43 44  45 
46 47 48 49 50 51 52 53  54  55 
56 57 58 59 60 61 62 63  64  65  66 
67 68 69 70 71 72 73 74  75  76  77  78 
79 80 81 82 83 84 85 86  87  88  89  90  91 
92 93 94 95 96 97 98 99 100 101 102 103 104 105 

```



## REXX


### version 1


```rexx
  
/* REXX *************************************************************** 
* Parse Arg rowcount                                                    
* 12.07.2012 Walter Pachl  - translated from Python                     
**********************************************************************/ 
Parse Arg rowcount                                                      
col=0                               
ll=''                               /* last line of triangle         */ 
Do j=rowcount*(rowcount-1)/2+1 to rowcount*(rowcount+1)/2               
  col=col+1                         /* column number                 */ 
  ll=ll j                           /* build last line               */ 
  len.col=length(j)                 /* remember length of column     */ 
  End                                                                   
Do i=1 To rowcount-1                /* now do and output the rest    */ 
  ol=''                                                                 
  col=0                                                                 
  Do j=i*(i-1)/2+1 to i*(i+1)/2     /* elements of line i            */ 
    col=col+1                                                           
    ol=ol right(j,len.col)          /* element in proper length      */ 
    end                                                                 
  Say ol                            /* output ith line               */ 
  end                                                                   
Say ll                              /* output last line              */ 

```

Output:

```txt

n=5
  1
  2  3
  4  5  6
  7  8  9 10
 11 12 13 14 15 

n=14  
  1
  2  3
  4  5  6
  7  8  9 10
 11 12 13 14 15
 16 17 18 19 20 21
 22 23 24 25 26 27 28
 29 30 31 32 33 34 35 36
 37 38 39 40 41 42 43 44  45
 46 47 48 49 50 51 52 53  54  55
 56 57 58 59 60 61 62 63  64  65  66
 67 68 69 70 71 72 73 74  75  76  77  78
 79 80 81 82 83 84 85 86  87  88  89  90  91
 92 93 94 95 96 97 98 99 100 101 102 103 104 105 

```


===version 2, simple formula===
This REXX version uses a simple formula to calculate the maximum value (triangle element) displayed.

```rexx
/*REXX program constructs & displays  Floyd's triangle for any number of specified rows.*/
parse arg N .;    if N=='' | N==","  then N= 5   /*Not specified?  Then use the default.*/
mx= N * (N+1) % 2  - N                           /*calculate the maximum of any value.  */
say 'displaying a '  N  " row Floyd's triangle:" /*show the header for Floyd's triangle.*/
say                                              /*display a blank line below the title.*/
#=1;    do    r=1  for N;           i= 0;     _= /*construct Floyd's triangle row by row*/
           do #=#  for r;           i= i + 1     /*start to construct a row of triangle.*/
           _= _ right(#, length( mx+i ) )        /*build a row of the Floyd's triangle. */
           end   /*#*/                           /*calculate the max length on the fly. */
        say substr(_, 2)                         /*remove 1st leading blank in the line.*/
        end      /*r*/                           /*stick a fork in it,  we're all done. */
```

{{out|output|text=  when using the default input:}}

```txt

displaying a  5  row Floyd's triangle:

 1
 2  3
 4  5  6
 7  8  9 10
11 12 13 14 15

```

{{out|output|text=  when using the default input of:   <tt> 14 </tt>}}

```txt

displaying a  14  row Floyd's triangle:

 1
 2  3
 4  5  6
 7  8  9 10
11 12 13 14 15
16 17 18 19 20 21
22 23 24 25 26 27 28
29 30 31 32 33 34 35 36
37 38 39 40 41 42 43 44  45
46 47 48 49 50 51 52 53  54  55
56 57 58 59 60 61 62 63  64  65  66
67 68 69 70 71 72 73 74  75  76  77  78
79 80 81 82 83 84 85 86  87  88  89  90  91
92 93 94 95 96 97 98 99 100 101 102 103 104 105

```

{{out|output|text=  (only showing the last row) when using the input of:   <tt> 45 </tt>}}

```txt

  ··· 44 rows not shown ··· 
991  992  993  994  995  996  997  998  999 1000 1001 1002 1003 1004 1005 1006 1007 1008 1009 1010 1011 1012 1013 1014 1015 1016 1017 1018 1019 1020 1021 1022 1023 1024 1025 1026 1027 1028 1029 1030 1031 1032 1033 1034 1035

```


===version 3, hexadecimal===

```rexx
/*REXX program constructs & displays Floyd's triangle for any number of rows in base 16.*/
parse arg N .;  if N=='' | N==","  then N=6      /*Not specified?  Then use the default.*/
mx=N * (N+1) % 2  -  N                           /*calculate maximum value of any value.*/
say 'displaying a '    N    " row Floyd's triangle in base 16:"  /*show triangle header.*/
say
#=1;  do     r=1  for N;     i=0;         _=     /*construct Floyd's triangle row by row*/
          do #=#  for r;     i=i+1               /*start to construct a row of triangle.*/
          _=_ right( d2x(#), length( d2x(mx+i))) /*build a row of the Floyd's triangle. */
          end   /*#*/
      say substr(_, 2)                           /*remove 1st leading blank in the line.*/
      end       /*r*/                            /*stick a fork in it,  we're all done. */
```

{{out|output|text=  when using the default input:}}

```txt

displaying a  6  row Floyd's triangle in base 16:

 1
 2  3
 4  5  6
 7  8  9  A
 B  C  D  E  F
10 11 12 13 14 15

```

{{out|output|text=  when using the input of:   <tt> 23 </tt>}}

```txt

displaying a  23  row Floyd's triangle in base 16:

 1
 2  3
 4  5   6
 7  8   9   A
 B  C   D   E   F
10 11  12  13  14  15
16 17  18  19  1A  1B  1C
1D 1E  1F  20  21  22  23  24
25 26  27  28  29  2A  2B  2C  2D
2E 2F  30  31  32  33  34  35  36  37
38 39  3A  3B  3C  3D  3E  3F  40  41  42
43 44  45  46  47  48  49  4A  4B  4C  4D  4E
4F 50  51  52  53  54  55  56  57  58  59  5A  5B
5C 5D  5E  5F  60  61  62  63  64  65  66  67  68  69
6A 6B  6C  6D  6E  6F  70  71  72  73  74  75  76  77  78
79 7A  7B  7C  7D  7E  7F  80  81  82  83  84  85  86  87  88
89 8A  8B  8C  8D  8E  8F  90  91  92  93  94  95  96  97  98  99
9A 9B  9C  9D  9E  9F  A0  A1  A2  A3  A4  A5  A6  A7  A8  A9  AA  AB
AC AD  AE  AF  B0  B1  B2  B3  B4  B5  B6  B7  B8  B9  BA  BB  BC  BD  BE
BF C0  C1  C2  C3  C4  C5  C6  C7  C8  C9  CA  CB  CC  CD  CE  CF  D0  D1  D2
D3 D4  D5  D6  D7  D8  D9  DA  DB  DC  DD  DE  DF  E0  E1  E2  E3  E4  E5  E6  E7
E8 E9  EA  EB  EC  ED  EE  EF  F0  F1  F2  F3  F4  F5  F6  F7  F8  F9  FA  FB  FC  FD
FE FF 100 101 102 103 104 105 106 107 108 109 10A 10B 10C 10D 10E 10F 110 111 112 113 114

```


===version 4, up to base 90===
This REXX version could be extended to even higher bases, all that is needed is to append more viewable characters to express "higher" numerals   ("digits" in base '''X'''). 

This version of the '''base''' function has some boilerplate for signed numbers and various error checking.

```rexx
/*REXX program constructs/shows Floyd's triangle for any number of rows in any base ≤90.*/
parse arg N radx .                               /*obtain optional arguments from the CL*/
if    N=='' |    N==","  then    N= 5            /*Not specified?  Then use the default.*/
if radx=='' | radx==","  then radx=10            /* "      "         "   "   "     "    */
mx=N * (N+1) % 2  -  N                           /*calculate maximum value of any value.*/
say 'displaying a '  N   " row Floyd's triangle in base"  radx':'  /*display the header.*/
say
#=1;  do     r=1  for N;   i=0;            _=    /*construct Floyd's triangle row by row*/
         do #=#  for r;    i=i+1                 /*start to construct a row of triangle.*/
         _=_ right(base(#, radx),  length( base(mx+i, radx) ) )    /*build triangle row.*/
         end   /*#*/
      say substr(_, 2)                           /*remove 1st leading blank in the line,*/
      end      /*r*/                             /* [↑]   introduced by first abutment. */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
base: procedure; parse arg x 1 ox,toB,inB              /*obtain number, toBase, inBase. */
      @abc= 'abcdefghijklmnopqrstuvwxyz'               /*lowercase Latin alphabet.      */
      @abcU=@abc;        upper @abcU                   /*go whole hog and extend 'em.   */
      @@@= '0123456789'@abc || @abcU                   /*prefix 'em with numeric digits.*/
      @@@=@@@'<>[]{}()?~!@#$%^&*_=|\/;:¢¬≈'            /*add some special chars as well.*/
                             /* [↑]  handles up to base 90,  all chars must be viewable.*/
      numeric digits 3000                              /*what the hey, support gihugeics*/
      mxB=length(@@@)                                  /*max base (radix) supported here*/
      if toB=='' | toB=="," then toB=10                /*if skipped, assume default (10)*/
      if inB=='' | inB=="," then inB=10                /* "    "        "      "      " */
      if inB<2   | inb>mxB  then call erb 'inBase',inB /*invalid/illegal arg:   inBase. */
      if toB<2   | tob>mxB  then call erb 'toBase',toB /*    "      "     "     toBase. */
      if x==''              then call erm              /*    "      "     "     number. */
             sigX=left(x, 1)                           /*obtain a possible leading sign.*/
      if pos(sigX, '-+')\==0  then x=substr(x, 2)      /*X  number has a leading sign?  */
                              else sigX=               /*           ··· no leading sign.*/
      #=0
            do j=1  for length(x);  _=substr(x, j, 1)  /*convert X, base inB ──► base 10*/
            v=pos(_, @@@)                              /*get the value of this "digit". */
            if v==0 | v>inB  then call erd x,j,inB     /*is this an illegal "numeral" ? */
            #=# * inB + v - 1                          /*construct new num, dig by dig. */
            end   /*j*/
      y=
            do  while  # >= toB                        /*convert #, base 10 ──► base toB*/
            y=substr(@@@, (# // toB) + 1, 1)y          /*construct the number for output*/
            #=# % toB                                  /* ··· and whittle  #  down also.*/
            end   /*while*/

      y=sigX || substr(@@@, #+1, 1)y                   /*prepend the sign if it existed.*/
      return y                                         /*return the number in base  toB.*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
erb:  call ser  'illegal'   arg(2)   "base: "   arg(1)   "must be in range:  2──► "    mxB
erd:  call ser  'illegal "digit" in'            x":"     _
erm:  call ser  'no argument specified.'
ser:  say; say  '***error***';             say arg(1);     say;      exit 13
```

{{out|output|text=  when using the input of:   <tt> 6   2 </tt>}}

```txt

displaying a  6  row Floyd's triangle in base 2:
 
    1
   10    11
  100   101   110
  111  1000  1001  1010
 1011  1100  1101  1110  1111
10000 10001 10010 10011 10100 10101

```

{{out|output|text=  when using the input of:   <tt> 23   2 </tt>}}

```txt

displaying a  12  row Floyd's triangle in base 2:

      1
     10      11
    100     101     110
    111    1000    1001    1010
   1011    1100    1101    1110    1111
  10000   10001   10010   10011   10100   10101
  10110   10111   11000   11001   11010   11011   11100
  11101   11110   11111  100000  100001  100010  100011  100100
 100101  100110  100111  101000  101001  101010  101011  101100  101101
 101110  101111  110000  110001  110010  110011  110100  110101  110110  110111
 111000  111001  111010  111011  111100  111101  111110  111111 1000000 1000001 1000010
1000011 1000100 1000101 1000110 1000111 1001000 1001001 1001010 1001011 1001100 1001101 1001110

```

{{out|output|text=  when using the input of:   <tt> 40   81 </tt>}}

```txt

displaying a  40  row Floyd's triangle in base 81:

 1
 2  3
 4  5  6
 7  8  9  a
 b  c  d  e  f
 g  h  i  j  k  l
 m  n  o  p  q  r  s
 t  u  v  w  x  y  z  A
 B  C  D  E  F  G  H  I  J
 K  L  M  N  O  P  Q  R  S  T
 U  V  W  X  Y  Z  <  >  [  ]  {
 }  (  )  ?  ~  !  @  #  $  %  ^  &
 *  _ 10 11 12 13 14 15 16 17 18 19 1a
1b 1c 1d 1e 1f 1g 1h 1i 1j 1k 1l 1m 1n 1o
1p 1q 1r 1s 1t 1u 1v 1w 1x 1y 1z 1A 1B 1C 1D
1E 1F 1G 1H 1I 1J 1K 1L 1M 1N 1O 1P 1Q 1R 1S 1T
1U 1V 1W 1X 1Y 1Z 1< 1> 1[ 1] 1{ 1} 1( 1) 1? 1~ 1!
1@ 1# 1$ 1% 1^ 1& 1* 1_ 20 21 22 23 24 25 26 27 28 29
2a 2b 2c 2d 2e 2f 2g 2h 2i 2j 2k 2l 2m 2n 2o 2p 2q 2r 2s
2t 2u 2v 2w 2x 2y 2z 2A 2B 2C 2D 2E 2F 2G 2H 2I 2J 2K 2L 2M
2N 2O 2P 2Q 2R 2S 2T 2U 2V 2W 2X 2Y 2Z 2< 2> 2[ 2] 2{ 2} 2( 2)
2? 2~ 2! 2@ 2# 2$ 2% 2^ 2& 2* 2_ 30 31 32 33 34 35 36 37 38 39 3a
3b 3c 3d 3e 3f 3g 3h 3i 3j 3k 3l 3m 3n 3o 3p 3q 3r 3s 3t 3u 3v 3w 3x
3y 3z 3A 3B 3C 3D 3E 3F 3G 3H 3I 3J 3K 3L 3M 3N 3O 3P 3Q 3R 3S 3T 3U 3V
3W 3X 3Y 3Z 3< 3> 3[ 3] 3{ 3} 3( 3) 3? 3~ 3! 3@ 3# 3$ 3% 3^ 3& 3* 3_ 40 41
42 43 44 45 46 47 48 49 4a 4b 4c 4d 4e 4f 4g 4h 4i 4j 4k 4l 4m 4n 4o 4p 4q 4r
4s 4t 4u 4v 4w 4x 4y 4z 4A 4B 4C 4D 4E 4F 4G 4H 4I 4J 4K 4L 4M 4N 4O 4P 4Q 4R 4S
4T 4U 4V 4W 4X 4Y 4Z 4< 4> 4[ 4] 4{ 4} 4( 4) 4? 4~ 4! 4@ 4# 4$ 4% 4^ 4& 4* 4_ 50 51
52 53 54 55 56 57 58 59 5a 5b 5c 5d 5e 5f 5g 5h 5i 5j 5k 5l 5m 5n 5o 5p 5q 5r 5s 5t 5u
5v 5w 5x 5y 5z 5A 5B 5C 5D 5E 5F 5G 5H 5I 5J 5K 5L 5M 5N 5O 5P 5Q 5R 5S 5T 5U 5V 5W 5X 5Y
5Z 5< 5> 5[ 5] 5{ 5} 5( 5) 5? 5~ 5! 5@ 5# 5$ 5% 5^ 5& 5* 5_ 60 61 62 63 64 65 66 67 68 69 6a
6b 6c 6d 6e 6f 6g 6h 6i 6j 6k 6l 6m 6n 6o 6p 6q 6r 6s 6t 6u 6v 6w 6x 6y 6z 6A 6B 6C 6D 6E 6F 6G
6H 6I 6J 6K 6L 6M 6N 6O 6P 6Q 6R 6S 6T 6U 6V 6W 6X 6Y 6Z 6< 6> 6[ 6] 6{ 6} 6( 6) 6? 6~ 6! 6@ 6# 6$
6% 6^ 6& 6* 6_ 70 71 72 73 74 75 76 77 78 79 7a 7b 7c 7d 7e 7f 7g 7h 7i 7j 7k 7l 7m 7n 7o 7p 7q 7r 7s
7t 7u 7v 7w 7x 7y 7z 7A 7B 7C 7D 7E 7F 7G 7H 7I 7J 7K 7L 7M 7N 7O 7P 7Q 7R 7S 7T 7U 7V 7W 7X 7Y 7Z 7< 7>
7[ 7] 7{ 7} 7( 7) 7? 7~ 7! 7@ 7# 7$ 7% 7^ 7& 7* 7_ 80 81 82 83 84 85 86 87 88 89 8a 8b 8c 8d 8e 8f 8g 8h 8i
8j 8k 8l 8m 8n 8o 8p 8q 8r 8s 8t 8u 8v 8w 8x 8y 8z 8A 8B 8C 8D 8E 8F 8G 8H 8I 8J 8K 8L 8M 8N 8O 8P 8Q 8R 8S 8T
8U 8V 8W 8X 8Y 8Z 8< 8> 8[ 8] 8{ 8} 8( 8) 8? 8~ 8! 8@ 8# 8$ 8% 8^ 8& 8* 8_ 90 91 92 93 94 95 96 97 98 99 9a 9b 9c
9d 9e 9f 9g 9h 9i 9j 9k 9l 9m 9n 9o 9p 9q 9r 9s 9t 9u 9v 9w 9x 9y 9z 9A 9B 9C 9D 9E 9F 9G 9H 9I 9J 9K 9L 9M 9N 9O 9P
9Q 9R 9S 9T 9U 9V 9W 9X 9Y 9Z 9< 9> 9[ 9] 9{ 9} 9( 9) 9? 9~ 9! 9@ 9# 9$ 9% 9^ 9& 9* 9_ a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 aa

```



## Ring


```ring

rows = 10
n = 0
for r = 1 to rows   
    for c = 1 to r  
        n = n + 1
        see string(n) + " " 
    next 
    see nl
next


```

Output:

```txt

1 
2 3 
4 5 6 
7 8 9 10 
11 12 13 14 15 
16 17 18 19 20 21 
22 23 24 25 26 27 28 
29 30 31 32 33 34 35 36 
37 38 39 40 41 42 43 44 45 
46 47 48 49 50 51 52 53 54 55 
56 57 58 59 60 61 62 63 64 65 66 
67 68 69 70 71 72 73 74 75 76 77 78 
79 80 81 82 83 84 85 86 87 88 89 90 91 
92 93 94 95 96 97 98 99 100 101 102 103 104 105 

```



## Ruby


```ruby
def floyd(rows)
  max = (rows * (rows + 1)) / 2
  widths = ((max - rows + 1)..max).map {|n| n.to_s.length + 1}
  n = 0
  rows.times do |r|
    puts (0..r).map {|i| n += 1; "%#{widths[i]}d" % n}.join
  end
end

floyd(5)
floyd(14)
```


{{out}}

```txt

  1
  2  3
  4  5  6
  7  8  9 10
 11 12 13 14 15
  1
  2  3
  4  5  6
  7  8  9 10
 11 12 13 14 15
 16 17 18 19 20 21
 22 23 24 25 26 27 28
 29 30 31 32 33 34 35 36
 37 38 39 40 41 42 43 44  45
 46 47 48 49 50 51 52 53  54  55
 56 57 58 59 60 61 62 63  64  65  66
 67 68 69 70 71 72 73 74  75  76  77  78
 79 80 81 82 83 84 85 86  87  88  89  90  91
 92 93 94 95 96 97 98 99 100 101 102 103 104 105

```



## Run BASIC


```runbasic
input "Number of rows: "; rows
dim colSize(rows)   
for col=1 to rows
    colSize(col) = len(str$(col + rows * (rows-1)/2))
next
 
thisNum = 1
for r = 1 to rows
    for col = 1 to r
        print right$( "  "+str$(thisNum), colSize(col)); " ";
        thisNum = thisNum + 1
    next
    print
next
```


```txt
Number of rows: ?14
 1 
 2  3 
 4  5  6 
 7  8  9 10 
11 12 13 14 15 
16 17 18 19 20 21 
22 23 24 25 26 27 28 
29 30 31 32 33 34 35 36 
37 38 39 40 41 42 43 44  45 
46 47 48 49 50 51 52 53  54  55 
56 57 58 59 60 61 62 63  64  65  66 
67 68 69 70 71 72 73 74  75  76  77  78 
79 80 81 82 83 84 85 86  87  88  89  90  91 
92 93 94 95 96 97 98 99 100 101 102 103 104 105 
```



## Rust


```rust
fn main() {
    floyds_triangle(5);
    floyds_triangle(14);
}

fn floyds_triangle(n: u32) {
    let mut triangle: Vec<Vec<String>> = Vec::new();
    let mut current = 0;
    for i in 1..=n {
        let mut v = Vec::new();
        for _ in 0..i {
            current += 1;
            v.push(current);
        }
        let row = v.iter().map(|x| x.to_string()).collect::<Vec<_>>();
        triangle.push(row);
    }

    for row in &triangle {
        let arranged_row: Vec<_> = row
            .iter()
            .enumerate()
            .map(|(i, number)| {
                let space_len = triangle.last().unwrap()[i].len() - number.len() + 1;
                let spaces = " ".repeat(space_len);
                let mut padded_number = spaces;
                padded_number.push_str(&number);
                padded_number
            })
            .collect();
        println!("{}", arranged_row.join(""))
    }
}


```



## Scala


```scala
def floydstriangle( n:Int ) { 
  val s = (1 to n)
  val t = s map {i => (s take(i-1) sum) + 1}
  
  (s zip t) foreach { n => 
    var m = n._2; 
	  
    for( i <- 0 until n._1 ) {
      val w = (t.last + i).toString.length + 1  // Column width from last row
      print("           " + m takeRight w )
      m+=1
    }
	  
    print("\n")
  }
}

// Test
floydstriangle(5)
floydstriangle(14)
```

{{out}}

```txt

  1
  2  3
  4  5  6
  7  8  9 10
 11 12 13 14 15

  1
  2  3
  4  5  6
  7  8  9 10
 11 12 13 14 15
 16 17 18 19 20 21
 22 23 24 25 26 27 28
 29 30 31 32 33 34 35 36
 37 38 39 40 41 42 43 44  45
 46 47 48 49 50 51 52 53  54  55
 56 57 58 59 60 61 62 63  64  65  66
 67 68 69 70 71 72 73 74  75  76  77  78
 79 80 81 82 83 84 85 86  87  88  89  90  91
 92 93 94 95 96 97 98 99 100 101 102 103 104 105


```



## Seed7


```seed7
$ include "seed7_05.s7i";

const proc: writeFloyd (in integer: rows) is func
  local
    var integer: number is 1;
    var integer: numBeforeLastLine is 0;
    var integer: line is 0;
    var integer: column is 0;
  begin
    numBeforeLastLine := rows * pred(rows) div 2;
    for line range 1 to rows do
      for column range 1 to line do
        if column <> 1 then
          write(" ");
        end if;
        write(number lpad length(str(numBeforeLastLine + column)));
        incr(number);
      end for;
      writeln;
    end for;
  end func;

const proc: main is func
  begin
    writeFloyd(5);
    writeFloyd(14);
  end func;
```


Output:

```txt

 1
 2  3
 4  5  6
 7  8  9 10
11 12 13 14 15
 1
 2  3
 4  5  6
 7  8  9 10
11 12 13 14 15
16 17 18 19 20 21
22 23 24 25 26 27 28
29 30 31 32 33 34 35 36
37 38 39 40 41 42 43 44  45
46 47 48 49 50 51 52 53  54  55
56 57 58 59 60 61 62 63  64  65  66
67 68 69 70 71 72 73 74  75  76  77  78
79 80 81 82 83 84 85 86  87  88  89  90  91
92 93 94 95 96 97 98 99 100 101 102 103 104 105

```



## Sidef


```ruby
func floyd(rows, n=1) {
    var max = Math.range_sum(1, rows)
    var widths = (max-rows .. max-1 -> map{.+n->to_s.len})
    { |r|
        say %'#{1..r -> map{|i| "%#{widths[i-1]}d" % n++}.join(" ")}'
    } << 1..rows
}

floyd(5)     # or: floyd(5, 88)
floyd(14)    # or: floyd(14, 900)
```

{{out}}

```txt

 1
 2  3
 4  5  6
 7  8  9 10
11 12 13 14 15
 1
 2  3
 4  5  6
 7  8  9 10
11 12 13 14 15
16 17 18 19 20 21
22 23 24 25 26 27 28
29 30 31 32 33 34 35 36
37 38 39 40 41 42 43 44  45
46 47 48 49 50 51 52 53  54  55
56 57 58 59 60 61 62 63  64  65  66
67 68 69 70 71 72 73 74  75  76  77  78
79 80 81 82 83 84 85 86  87  88  89  90  91
92 93 94 95 96 97 98 99 100 101 102 103 104 105

```



## SPL


```spl
floyd(5)
floyd(14)

floyd(n)=
  k = 0
  > r, 1..n
    s = ""
    > j, 1..r
      k += 1
      f = ">"+#.upper(#.log10((n-1)*n/2+j+1)+1)+">"
      s += #.str(k,f)
    <
    #.output(s)
  <
.
```

{{out}}

```txt

  1
  2  3
  4  5  6
  7  8  9 10
 11 12 13 14 15
  1
  2  3
  4  5  6
  7  8  9 10
 11 12 13 14 15
 16 17 18 19 20 21
 22 23 24 25 26 27 28
 29 30 31 32 33 34 35 36
 37 38 39 40 41 42 43 44  45
 46 47 48 49 50 51 52 53  54  55
 56 57 58 59 60 61 62 63  64  65  66
 67 68 69 70 71 72 73 74  75  76  77  78
 79 80 81 82 83 84 85 86  87  88  89  90  91
 92 93 94 95 96 97 98 99 100 101 102 103 104 105

```



## Tcl


```tcl
proc floydTriangle n {
    # Compute the column widths
    for {set i [expr {$n*($n-1)/2+1}]} {$i <= $n*($n+1)/2} {incr i} {
	lappend w [string length $i]
    }
    # Print the triangle
    for {set i 0; set j 1} {$j <= $n} {incr j} {
	for {set p -1; set k 0} {$k < $j} {incr k} {
	    puts -nonewline [format "%*d " [lindex $w [incr p]] [incr i]]
	}
	puts ""
    }
}

# Demonstration
puts "Floyd 5:"
floydTriangle 5
puts "Floyd 14:"
floydTriangle 14
```

{{out}}

```txt

Floyd 5:
 1 
 2  3 
 4  5  6 
 7  8  9 10 
11 12 13 14 15 
Floyd 14:
 1 
 2  3 
 4  5  6 
 7  8  9 10 
11 12 13 14 15 
16 17 18 19 20 21 
22 23 24 25 26 27 28 
29 30 31 32 33 34 35 36 
37 38 39 40 41 42 43 44  45 
46 47 48 49 50 51 52 53  54  55 
56 57 58 59 60 61 62 63  64  65  66 
67 68 69 70 71 72 73 74  75  76  77  78 
79 80 81 82 83 84 85 86  87  88  89  90  91 
92 93 94 95 96 97 98 99 100 101 102 103 104 105 

```



## TXR



```txrlisp
(defun flotri (n)
  (let* ((last (trunc (* n (+ n 1)) 2))
         (colw (mapcar [chain tostring length]
                       (range (- last n -1) last)))
         (x 0))
    (each ((r (range* 0 n)))
      (each ((c (range 0 r)))
        (format t " ~*a" [colw c] (inc x)))
      (put-line))))

(defun usage (msg)
  (put-line `error: @msg`)
  (put-line `usage:\n@(ldiff *full-args* *args*) <smallish-positive-integer>`)
  (exit 1))

(tree-case *args*
  ((num blah . etc) (usage "too many arguments"))
  ((num) (flotri (int-str num)))
  (() (usage "need an argument")))
```


{{out}}


```txt
$ txr floyds-triangle.tl
error: need an argument
usage:
txr floyds-triangle.tl <smallish-positive-integer>
$ txr floyds-triangle.txr 1 2
error: too many arguments
usage:
txr floyds-triangle.tl <smallish-positive-integer>
$ txr floyds-triangle.tl 5
  1
  2  3
  4  5  6
  7  8  9 10
 11 12 13 14 15
$ txr floyds-triangle.tl 14
  1
  2  3
  4  5  6
  7  8  9 10
 11 12 13 14 15
 16 17 18 19 20 21
 22 23 24 25 26 27 28
 29 30 31 32 33 34 35 36
 37 38 39 40 41 42 43 44  45
 46 47 48 49 50 51 52 53  54  55
 56 57 58 59 60 61 62 63  64  65  66
 67 68 69 70 71 72 73 74  75  76  77  78
 79 80 81 82 83 84 85 86  87  88  89  90  91
 92 93 94 95 96 97 98 99 100 101 102 103 104 105
```


## VBA

Solution in Microsoft Office Word. Based on VBScript

```VB
Option Explicit
Dim o As String
Sub floyd(L As Integer)
    Dim r, c, m, n As Integer
    n = L * (L - 1) / 2
    m = 1
    For r = 1 To L
        o = o & vbCrLf
        For c = 1 To r
            o = o & Space(Len(CStr(n + c)) - Len(CStr(m))) & m & " "
            m = m + 1
        Next
    Next
End Sub
Sub triangle()
    o = "5 lines"
    Call floyd(5)
    o = o & vbCrLf & "14 lines"
    Call floyd(14)
    With Selection
        .Font.Name = "Courier New"
        .TypeText Text:=o
    End With
End Sub
```

{{out}}
<lang>5 lines
 1 
 2  3 
 4  5  6 
 7  8  9 10 
11 12 13 14 15 

14 lines
 1 
 2  3 
 4  5  6 
 7  8  9 10 
11 12 13 14 15 
16 17 18 19 20 21 
22 23 24 25 26 27 28 
29 30 31 32 33 34 35 36 
37 38 39 40 41 42 43 44  45 
46 47 48 49 50 51 52 53  54  55 
56 57 58 59 60 61 62 63  64  65  66 
67 68 69 70 71 72 73 74  75  76  77  78 
79 80 81 82 83 84 85 86  87  88  89  90  91 
92 93 94 95 96 97 98 99 100 101 102 103 104 105
```



## VBScript

{{works with|Windows Script Host|*}}

```VBScript

' Read the number of rows to use..
intRows = WScript.StdIn.ReadLine

' Get the first number of the final row so we can calculate widths...
intLastRowStart = (intRows ^ 2 - intRows) \ 2 + 1

For i = 1 To intRows
	intLastRow = intLastRowStart
	For j = 1 To i
		k = k + 1
		WScript.StdOut.Write Space(Len(intLastRow) - Len(k)) & k & " "
		intLastRow = intLastRow + 1
	Next
	WScript.StdOut.WriteLine ""
Next

```



## Visual Basic .NET

{{trans|C#}}

```vbnet
Imports System.Text

Module Module1

    Function MakeTriangle(rows As Integer) As String
        Dim maxValue As Integer = (rows * (rows + 1)) / 2
        Dim digit = 0
        Dim output As New StringBuilder

        For row = 1 To rows
            For column = 0 To row - 1
                Dim colMaxDigit = (maxValue - rows) + column + 1
                If column > 0 Then
                    output.Append(" ")
                End If

                digit = digit + 1
                output.Append(digit.ToString().PadLeft(colMaxDigit.ToString().Length))
            Next

            output.AppendLine()
        Next

        Return output.ToString()
    End Function

    Sub Main()
        Dim args = Environment.GetCommandLineArgs()
        Dim count As Integer

        If args.Length > 1 AndAlso Integer.TryParse(args(1), count) AndAlso count > 0 Then
            Console.WriteLine(MakeTriangle(count))
        Else
            Console.WriteLine(MakeTriangle(5))
            Console.WriteLine()
            Console.WriteLine(MakeTriangle(14))
        End If
    End Sub

End Module
```

{{out}}

```txt
 1
 2  3
 4  5  6
 7  8  9 10
11 12 13 14 15


 1
 2  3
 4  5  6
 7  8  9 10
11 12 13 14 15
16 17 18 19 20 21
22 23 24 25 26 27 28
29 30 31 32 33 34 35 36
37 38 39 40 41 42 43 44  45
46 47 48 49 50 51 52 53  54  55
56 57 58 59 60 61 62 63  64  65  66
67 68 69 70 71 72 73 74  75  76  77  78
79 80 81 82 83 84 85 86  87  88  89  90  91
92 93 94 95 96 97 98 99 100 101 102 103 104 105
```




## XPL0


```XPL0
include c:\cxpl\codes;  \include 'code' declarations

func IntLen(N);         \Return number of digits in a positive integer
int     N;
int     I;
for I:= 1 to 20 do
    [N:= N/10;  if N=0 then return I];

proc Floyd(N);          \Display Floyd's triangle
int N;
int M, Row, Col;
real F;
[M:= (N-1+1)*(N-1)/2;   \last Floyd number on second to last row
F:= 1.0;                \Floyd number counter
for Row:= 1 to N do
    [for Col:= 1 to Row do
        [Format(IntLen(M+Col)+1, 0);  RlOut(0, F);  F:= F+1.0];
    CrLf(0);
    ];
]; \Floyd

[Floyd(5);
Floyd(14);
]
```


Output:

```txt

  1
  2  3
  4  5  6
  7  8  9 10
 11 12 13 14 15
  1
  2  3
  4  5  6
  7  8  9 10
 11 12 13 14 15
 16 17 18 19 20 21
 22 23 24 25 26 27 28
 29 30 31 32 33 34 35 36
 37 38 39 40 41 42 43 44  45
 46 47 48 49 50 51 52 53  54  55
 56 57 58 59 60 61 62 63  64  65  66
 67 68 69 70 71 72 73 74  75  76  77  78
 79 80 81 82 83 84 85 86  87  88  89  90  91
 92 93 94 95 96 97 98 99 100 101 102 103 104 105

```



## zkl

Format last line and then fit each line to that format (which is wider than terminal width before formating breaks down (at 10 digit numbers)):

```zkl
fcn lcNum(row){(row*(row+1)/2+1)}   // lazy caterer's sequence
fcn floydsTriangle(rows){
   fmt:=[lcNum(rows-1)..lcNum(rows)-1].pump(String,fcn(n){
      String("%",n.toString().len(),"d ")}); // eg "%2d %2d %3d %3d"
   foreach row in (rows){
     ns:=[lcNum(row)..lcNum(row+1)-1].walk(); // eg L(4.5,6)
     fmt[0,ns.len()*4].fmt(ns.xplode()).println(); // eg "%2d %2d %2d ".fmt(4,5,6)
   }
}
floydsTriangle(5); println();
floydsTriangle(14);
```

{{out}}

```txt

 1 
 2  3 
 4  5  6 
 7  8  9 10 
11 12 13 14 15 

 1 
 2  3 
 4  5  6 
 7  8  9 10 
11 12 13 14 15 
16 17 18 19 20 21 
22 23 24 25 26 27 28 
29 30 31 32 33 34 35 36 
37 38 39 40 41 42 43 44  45 
46 47 48 49 50 51 52 53  54  55 
56 57 58 59 60 61 62 63  64  65  66 
67 68 69 70 71 72 73 74  75  76  77  78 
79 80 81 82 83 84 85 86  87  88  89  90  91 
92 93 94 95 96 97 98 99 100 101 102 103 104 105 

```



## ZX Spectrum Basic


```zxbasic
10 LET n=10: LET j=1: LET col=1
20 FOR r=1 TO n
30 FOR j=j TO j+r-1
40 PRINT TAB (col);j;
50 LET col=col+3
60 NEXT j
70 PRINT 
80 LET col=1
90 NEXT r
```

