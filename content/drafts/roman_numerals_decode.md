+++
title = "Roman numerals/Decode"
description = ""
date = 2019-08-26T10:29:57Z
aliases = []
[extra]
id = 9587
[taxonomies]
categories = []
tags = []
+++

{{task}}

;Task:
Create a function that takes a Roman numeral as its argument and returns its value as a numeric decimal integer.

You don't need to validate the form of the Roman numeral.

Modern Roman numerals are written by expressing each decimal digit of the number to be encoded separately,

starting with the leftmost decimal digit and skipping any '''0'''s   (zeroes).

'''1990''' is rendered as   '''MCMXC'''     (1000 = M,   900 = CM,   90 = XC)     and

'''2008''' is rendered as   '''MMVIII'''       (2000 = MM,   8 = VIII).

The Roman numeral for '''1666''',   '''MDCLXVI''',   uses each letter in descending order.





## 360 Assembly


```360asm
*        Roman numerals Decode -   17/04/2019
ROMADEC  CSECT
         USING  ROMADEC,R13        base register
         B      72(R15)            skip savearea
         DC     17F'0'             savearea
         SAVE   (14,12)            save previous context
         ST     R13,4(R15)         link backward
         ST     R15,8(R13)         link forward
         LR     R13,R15            set addressability
         LA     R6,1               i=1
       DO WHILE=(C,R6,LE,=A(NV))   do i=1 to hbound(vals)
         LR     R1,R6                i
         SLA    R1,3                 ~
         LA     R4,VALS-L'VALS(R1)   @vals(i)
         MVC    X,0(R4)              x=vals(i)
         SR     R9,R9                prev=0
         ST     R9,Y                 y=0
         LA     R7,L'X               j=1
       DO WHILE=(C,R7,GE,=A(1))      do j=length(x) to 1 by -1
         LA     R4,X-1                 @x
         AR     R4,R7                  +j
         MVC    C,0(R4)                c=substr(x,j,1)
       IF   CLI,C,NE,C' ' THEN         if c^=' ' then
         SR     R1,R1                  r1=0
         LA     R2,1                   k=1
       DO WHILE=(C,R2,LE,=A(L'ROMAN))   do k=1 to length(roman)
         LA     R3,ROMAN-1               @roman
         AR     R3,R2                    +k
       IF   CLC,0(L'C,R3),EQ,C THEN      if substr(roman,k,1)=c
         LR     R1,R2                      index=k
         B      REINDEX                    leave k
       ENDIF    ,                        endif
         LA     R2,1(R2)                 k=k+1
       ENDDO    ,                      enddo k
REINDEX  EQU    *                      r1=index(roman,c)
         SLA    R1,2                   ~
         L      R8,DECIM-4(R1)         n=decim(index(roman,c))
       IF    CR,R8,LT,R9 THEN          if n<prev then
         LCR    R8,R8                    n=-n
       ENDIF    ,                      endif
         L      R2,Y                     y
         AR     R2,R8                    +n
         ST     R2,Y                     y=y+n
         LR     R9,R8                    prev=n
       ENDIF    ,                      endif
         BCTR   R7,0                   j--
       ENDDO    ,                    enddo j
         MVC    PG(8),X              x
         L      R1,Y                 y
         XDECO  R1,XDEC              edit y
         MVC    PG+12(4),XDEC+8      output y
         XPRNT  PG,L'PG              print buffer
         LA     R6,1(R6)             i++
       ENDDO    ,                  enddo i
         L      R13,4(0,R13)       restore previous savearea pointer
         RETURN (14,12),RC=0       restore registers from calling sav
NV       EQU    (X-VALS)/L'VALS
ROMAN    DC     CL7'MDCLXVI'
DECIM    DC     F'1000',F'500',F'100',F'50',F'10',F'5',F'1'
VALS     DC     CL8'XIV',CL8'CMI',CL8'MIC',CL8'MCMXC',CL8'MDCLXVI'
         DC     CL8'MMVIII',CL8'MMXIX',CL8'MMMCMXCV'
X        DS     CL(L'VALS)
Y        DS     F
C        DS     CL1
PG       DC     CL80'........ -> ....'
XDEC     DS     CL12
         REGEQU
         END    ROMADEC
```

{{out}}

```txt

XIV      ->   14
CMI      ->  901
MIC      -> 1099
MCMXC    -> 1990
MDCLXVI  -> 1666
MMVIII   -> 2008
MMXIX    -> 2019
MMMCMXCV -> 3995

```




## Ada



```Ada
Pragma Ada_2012;
Pragma Assertion_Policy( Check );

With
Unchecked_Conversion,
Ada.Text_IO;

Procedure Test_Roman_Numerals is

    -- We create an enumeration of valid characters, note that they are
    -- character-literals, this is so that we can use literal-strings,
    -- and that their size is that of Integer.
    Type Roman_Digits is ('I', 'V', 'X', 'L', 'C', 'D', 'M' )
    with Size => Integer'Size;

    -- We use a representation-clause ensure the proper integral-value
    -- of each individual character.
    For Roman_Digits use
      (
    'I' => 1,
    'V' => 5,
    'X' => 10,
    'L' => 50,
    'C' => 100,
    'D' => 500,
    'M' => 1000
      );

    -- To convert a Roman_Digit to an integer, we now only need to
    -- read its value as an integer.
    Function Convert is new Unchecked_Conversion
      (	Source => Roman_Digits,	Target => Integer );

    -- Romena_Numeral is a string of Roman_Digit.
    Type Roman_Numeral is array (Positive range <>) of Roman_Digits;

    -- The Numeral_List type is used herein only for testing
    -- and verification-data.
    Type Numeral_List is array (Positive range <>) of
      not null access Roman_Numeral;

    -- The Test_Cases subtype ensures that Test_Data and Validation_Data
    -- both contain the same number of elements, and that the indecies
    -- are the same; essentially the same as:
    --
    --    pragma Assert( Test_Data'Length = Validation_Data'Length
    --		   AND Test_Data'First = Validation_Data'First);

    subtype Test_Cases is Positive range 1..14;

    Test_Data : constant Numeral_List(Test_Cases):=
      (
       New Roman_Numeral'("III"),	-- 3
       New Roman_Numeral'("XXX"),	-- 30
       New Roman_Numeral'("CCC"),	-- 300
       New Roman_Numeral'("MMM"),	-- 3000

       New Roman_Numeral'("VII"),	-- 7
       New Roman_Numeral'("LXVI"),	-- 66
       New Roman_Numeral'("CL"),	-- 150
       New Roman_Numeral'("MCC"),	-- 1200

       New Roman_Numeral'("IV"),	-- 4
       New Roman_Numeral'("IX"),	-- 9
       New Roman_Numeral'("XC"),	-- 90

       New Roman_Numeral'("ICM"),	-- 901
       New Roman_Numeral'("CIM"),	-- 899

       New Roman_Numeral'("MDCLXVI")	-- 1666
      );

    Validation_Data : constant array(Test_Cases) of Natural:=
      (	3, 30, 300, 3000,
	7, 66, 150, 1200,
	4, 9, 90,
	901, 899,
	1666
      );


    -- In Roman numerals, the subtractive form [IV = 4] was used
    -- very infrequently, the most common form was the addidive
    -- form [IV = 6]. (Consider military logistics and squads.)

    -- SUM returns the Number, read in the additive form.
    Function Sum( Number : Roman_Numeral ) return Natural is
    begin
	Return Result : Natural:= 0 do
	    For Item of Number loop
		    Result:= Result + Convert( Item );
	    end loop;
	End Return;
    end Sum;

    -- EVAL returns Number read in the subtractive form.
    Function Eval( Number : Roman_Numeral ) return Natural is
	Current : Roman_Digits:= 'I';
    begin
	Return Result : Natural:= 0 do
	    For Item of Number loop
		if Current < Item then
		    Result:= Convert(Item) - Result;
		    Current:= Item;
		else
		    Result:= Result + Convert(Item);
		end if;
	    end loop;
	End Return;
    end Eval;

    -- Display the given Roman_Numeral via Text_IO.
    Procedure Put( S: Roman_Numeral ) is
    begin
	For Ch of S loop
	    declare
		-- The 'Image attribute returns the character inside
		-- single-quotes; so we select the character itself.
		C : Character renames Roman_Digits'Image(Ch)(2);
	    begin
		Ada.Text_IO.Put( C );
	    end;
	end loop;
    end;

    -- This displays pass/fail dependant on the parameter.
    Function PF ( Value : Boolean ) Return String is
    begin
	Return Result : String(1..4):= ( if Value then"pass"else"fail" );
    End PF;

Begin
    Ada.Text_IO.Put_Line("Starting Test:");

    for Index in Test_Data'Range loop
	declare
	    Item  : Roman_Numeral renames Test_Data(Index).all;
	    Value : constant Natural := Eval(Item);
	begin
	    Put( Item );

	    Ada.Text_IO.Put( ASCII.HT & "= ");
	    Ada.Text_IO.Put( Value'Img );
	    Ada.Text_IO.Put_Line( ASCII.HT & '[' &
			  PF( Value = Validation_Data(Index) )& ']');
	end;
    end loop;


    Ada.Text_IO.Put_Line("Testing complete.");
End Test_Roman_Numerals;

```


{{out}}

```txt
Starting Test:
III	=  3	[pass]
XXX	=  30	[pass]
CCC	=  300	[pass]
MMM	=  3000	[pass]
VII	=  7	[pass]
LXVI	=  66	[pass]
CL	=  150	[pass]
MCC	=  1200	[pass]
IV	=  4	[pass]
IX	=  9	[pass]
XC	=  90	[pass]
ICM	=  901	[pass]
CIM	=  899	[pass]
MDCLXVI	=  1666	[pass]
Testing complete.
```



## ALGOL 68

{{works with|ALGOL 68G|Any - tested with release 2.2.0}}
Note: roman to int will handle multiple subtraction, e.g. IIIIX for 6.

```Algol68
    PROC roman to int = (STRING roman) INT:
    BEGIN
        PROC roman digit value = (CHAR roman digit) INT:
            (roman digit = "M" | 1000 |:
             roman digit = "D" |  500 |:
             roman digit = "C" |  100 |:
             roman digit = "L" |   50 |:
             roman digit = "X" |   10 |:
             roman digit = "V" |    5 |:
             roman digit = "I" |    1);

        INT result := 0, previous value := 0, run := 0;

        FOR i FROM LWB roman TO UPB roman
        DO
            INT value = roman digit value(roman[i]);
            IF previous value = value THEN
                run +:= value
            ELSE
                IF previous value < value THEN
                    result -:= run
                ELSE
                    result +:= run
                FI;
                run := previous value := value
            FI
        OD;

        result +:= run
    END;

    MODE TEST = STRUCT (STRING input, INT expected output);

    [] TEST roman test = (
        ("MMXI",    2011), ("MIM",     1999),
        ("MCMLVI",  1956), ("MDCLXVI", 1666),
        ("XXCIII",    83), ("LXXIIX",    78),
        ("IIIIX",      6)
    );

    print(("Test input  Value   Got", newline, "--------------------------", newline));
    FOR i FROM LWB roman test TO UPB roman test
    DO
        INT output = roman to int(input OF roman test[i]);
        printf(($g, n (12 - UPB input OF roman test[i]) x$, input OF roman test[i]));
        printf(($g(5), 1x, g(5), 1x$, expected output OF roman test[i], output));
        printf(($b("ok", "not ok"), 1l$, output = expected output OF roman test[i]))
    OD
```



## ALGOL W


```algolw
begin
    % decodes a roman numeral into an integer                                %
    % there must be at least one blank after the numeral                     %
    % This takes a lenient view on roman numbers so e.g. IIXX is 18 - see    %
    % the Discussion                                                         %
    integer procedure romanToDecimal ( string(32) value roman ) ;
    begin
        integer   decimal, rPos, currDigit, nextDigit, seqValue;
        string(1) rDigit;

        % the roman number is a sequence of sequences of roman digits        %
        % if the previous sequence is of higher value digits than the next,  %
        %    the higher value is added to the overall value                  %
        % if the previous seequence is of lower value, it is subtracted      %
        % e.g. MCMLXII                                                       %
        %      the sequences are M, C, M, X, II                              %
        %          M is added, C subtracted, M added, X added and II added   %

        % get the value of a sequence of roman digits                        %
        integer procedure getSequence ;
            if rDigit = " " then begin
                % end of the number                                          %
                0
                end
            else begin
                % have another sequence                                      %
                integer sValue;
                sValue := 0;
                while roman( rPos // 1 ) = rDigit do begin
                    sValue := sValue + currDigit;
                    rPos   := rPos + 1;
                end while_have_same_digit ;
                % remember the next digit                                    %
                rDigit     := roman( rPos // 1 );
                % result is the sequence value                               %
                sValue
             end getSequence ;

        % convert a roman digit into its decimal equivalent                  %
        % an invalid digit will terminate the program, " " is 0              %
        integer procedure getValue( string(1) value romanDigit ) ;
            if      romanDigit = "m" or romanDigit = "M" then 1000
            else if romanDigit = "d" or romanDigit = "D" then  500
            else if romanDigit = "c" or romanDigit = "C" then  100
            else if romanDigit = "l" or romanDigit = "L" then   50
            else if romanDigit = "x" or romanDigit = "X" then   10
            else if romanDigit = "v" or romanDigit = "V" then    5
            else if romanDigit = "i" or romanDigit = "I" then    1
            else if romanDigit = " "                     then    0
            else begin
                write( s_w := 0, "Invalid roman digit: """, romanDigit, """" );
                assert false;
                0
            end getValue ;

        % get the first sequence                                             %
        decimal   := 0;
        rPos      := 0;
        rDigit    := roman( rPos // 1 );
        currDigit := getValue( rDigit );
        seqValue  := getSequence;

        % handle the sequences                                               %
        while rDigit not = " " do begin
            % have another sequence                                          %
            nextDigit := getValue( rDigit );
            if currDigit < nextDigit
            then % prev digit is lower  % decimal := decimal - seqValue
            else % prev digit is higher % decimal := decimal + seqValue
            ;
            currDigit := nextDigit;
            seqValue  := getSequence;
        end while_have_a_roman_digit ;

        % add the final sequence                                             %
        decimal + seqValue
    end roman ;

    % test the romanToDecimal routine                                        %

    procedure testRoman ( string(32) value romanNumber ) ;
        write( i_w := 5, romanNumber, romanToDecimal( romanNumber ) );

    testRoman( "I"        );    testRoman( "II"       );
    testRoman( "III"      );    testRoman( "IV"       );
    testRoman( "V"        );    testRoman( "VI"       );
    testRoman( "VII"      );    testRoman( "VIII"     );
    testRoman( "IX"       );    testRoman( "IIXX"     );
    testRoman( "XIX"      );    testRoman( "XX"       );
    write( "..." );
    testRoman( "MCMXC"    );
    testRoman( "MMVIII"   );
    testRoman( "MDCLXVI"  );

end.
```

{{out}}

```txt

I                                   1
II                                  2
III                                 3
IV                                  4
V                                   5
VI                                  6
VII                                 7
VIII                                8
IX                                  9
IIXX                               18
XIX                                19
XX                                 20
...
MCMXC                            1990
MMVIII                           2008
MDCLXVI                          1666

```



## ANTLR

[[File:Rn.PNG|left|Roman Numeral]]
[[File:Hundreds.PNG|left|Roman Numeral]]
[[File:H5.PNG|left|Roman Numeral]]
[[File:H9.PNG|left|Roman Numeral]]
[[File:Tens.PNG|left|Roman Numeral]]
[[File:T5.PNG|left|Roman Numeral]]
[[File:T9.PNG|left|Roman Numeral]]
[[File:Units.PNG|left|Roman Numeral]]
[[File:U5.PNG|left|Roman Numeral]]
[[File:U9.PNG|left|Roman Numeral]]
<br clear=both>


### Java


```java
/* Parse Roman Numerals

   Nigel Galloway March 16th., 2012
*/
grammar ParseRN ;

options {
	language = Java;
}
@members {
int rnValue;
int ONE;
}

parseRN:	({rnValue = 0;} rn NEWLINE {System.out.println($rn.text + " = " + rnValue);})*
	;

rn	:	(Thousand {rnValue += 1000;})* hundreds? tens? units?;

hundreds:	{ONE = 0;} (h9 | h5) {if (ONE > 3) System.out.println ("Too many hundreds");};
h9	:	Hundred {ONE += 1;} (FiveHund {rnValue += 400;}| Thousand {rnValue += 900;}|{rnValue += 100;} (Hundred {rnValue += 100; ONE += 1;})*);
h5	:	FiveHund {rnValue += 500;} (Hundred {rnValue += 100; ONE += 1;})*;

tens	:	{ONE = 0;} (t9 | t5) {if (ONE > 3) System.out.println ("Too many tens");};
t9	:	Ten {ONE += 1;} (Fifty {rnValue += 40;}| Hundred {rnValue += 90;}|{rnValue += 10;} (Ten {rnValue += 10; ONE += 1;})*);
t5	:	Fifty {rnValue += 50;} (Ten {rnValue += 10; ONE += 1;})*;

units	:	{ONE = 0;} (u9 | u5) {if (ONE > 3) System.out.println ("Too many ones");};
u9	:	One {ONE += 1;} (Five {rnValue += 4;}| Ten {rnValue += 9;}|{rnValue += 1;} (One {rnValue += 1; ONE += 1;})*);
u5	:	Five {rnValue += 5;} (One {rnValue += 1; ONE += 1;})*;

One	:	'I';
Five	:	'V';
Ten	:	'X';
Fifty	:	'L';
Hundred:	'C';
FiveHund:	'D';
Thousand:	'M' ;
NEWLINE:	'\r'? '\n' ;
```

Using this test data:

```txt

MMXI
MCMLVI
XXCIII
MCMXC
MMVIII
MDCLXVI
IIIIX
MIM
MDCLXVI
LXXIIX
M
MCXI
CMXI
MCM
MMIX
MCDXLIV
MMXII

```

Produces:

```txt

MMXI = 2011
MCMLVI = 1956
line 3:2 missing NEWLINE at 'C'
XX = 20
CIII = 103

```

Note that this implementation does not accept XXC as eighty. The error is detected and ANTLR attempts to continue by inserting the expected NEWLINE after XX and treating CIII as a new Number.<!--[[User:Nigel Galloway|Nigel Galloway]] 14:55, 17 March 2012 (UTC)--> <!-- contributions not normally signed visually; info is in history -->

```txt

MCMXC = 1990
MMVIII = 2008
MDCLXVI = 1666
Too many ones
line 7:4 extraneous input 'X' expecting NEWLINE
IIII = 4

```

An implementation above thinks IIIIX is 6. It isn't. ANTLR detects the surfiet of 'I' reports the errors and tries to carry on.<!--[[User:Nigel Galloway|Nigel Galloway]] 14:55, 17 March 2012 (UTC)--> <!-- contributions not normally signed visually; info is in history -->

```txt

line 8:2 no viable alternative at input 'M'
MIM = 1000
MDCLXVI = 1666
line 10:5 extraneous input 'X' expecting NEWLINE
LXXII = 72
M = 1000
MCXI = 1111
CMXI = 911
MCM = 1900
MMIX = 2009
MCDXLIV = 1444
MMXII = 2012

```



## AppleScript


### =isPrefixOf=

{{trans|JavaScript}}
(Functional ES5 version)
{{trans|Haskell}}

```AppleScript
-- romanValue :: String -> Int
on romanValue(s)
    script roman
        property mapping : [["M", 1000], ["CM", 900], ["D", 500], ["CD", 400], ¬
            ["C", 100], ["XC", 90], ["L", 50], ["XL", 40], ["X", 10], ["IX", 9], ¬
            ["V", 5], ["IV", 4], ["I", 1]]

        -- Value of first Roman glyph + value of remaining glyphs
        -- toArabic :: [Char] -> Int
        on toArabic(xs)
            script transcribe
                -- If this glyph:value pair matches the head of the list
                -- return the value and the tail of the list
                -- transcribe :: (String, Number) -> Maybe (Number, [String])
                on |λ|(lstPair)
                    set lstR to characters of (item 1 of lstPair)
                    if isPrefixOf(lstR, xs) then
                        -- Value of this matching glyph, with any remaining glyphs
                        {item 2 of lstPair, drop(length of lstR, xs)}
                    else
                        {}
                    end if
                end |λ|
            end script

            if length of xs > 0 then
                set lstParse to concatMap(transcribe, mapping)
                (item 1 of lstParse) + toArabic(item 2 of lstParse)
            else
                0
            end if
        end toArabic
    end script

    toArabic(characters of s) of roman
end romanValue

-- TEST -----------------------------------------------------------------------
on run
    map(romanValue, {"MCMXC", "MDCLXVI", "MMVIII"})

    --> {1990, 1666, 2008}
end run


-- GENERIC FUNCTIONS ----------------------------------------------------------

-- concatMap :: (a -> [b]) -> [a] -> [b]
on concatMap(f, xs)
    set lst to {}
    set lng to length of xs
    tell mReturn(f)
        repeat with i from 1 to lng
            set lst to (lst & |λ|(item i of xs, i, xs))
        end repeat
    end tell
    return lst
end concatMap

--  drop :: Int -> a -> a
on drop(n, a)
    if n < length of a then
        if class of a is text then
            text (n + 1) thru -1 of a
        else
            items (n + 1) thru -1 of a
        end if
    else
        {}
    end if
end drop

-- isPrefixOf :: [a] -> [a] -> Bool
on isPrefixOf(xs, ys)
    if length of xs = 0 then
        true
    else
        if length of ys = 0 then
            false
        else
            set {x, xt} to uncons(xs)
            set {y, yt} to uncons(ys)
            (x = y) and isPrefixOf(xt, yt)
        end if
    end if
end isPrefixOf

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

-- uncons :: [a] -> Maybe (a, [a])
on uncons(xs)
    if length of xs > 0 then
        {item 1 of xs, rest of xs}
    else
        missing value
    end if
end uncons
```

{{Out}}

```AppleScript
{1990, 1666, 2008}
```



====Fold right – subtracting or adding====
{{Works with|Yosemite onwards}}
{{trans|Haskell}}

```AppleScript
use framework "Foundation"

-- INTEGER VALUE OF ROMAN NUMBER STRING ---------------------------------------

-- fromRoman :: String -> Int
on fromRoman(s)
    script subtractIfLower
        on |λ|(rn, L)
            set {r, n} to rn
            if L ≥ r then  -- Digit values that increase (right to left),
                {L, n + L} -- are added
            else
                {L, n - L} -- Digit values that go down, are subtracted.
            end if
        end |λ|
    end script

    snd(foldr(subtractIfLower, {0, 0}, map(my charVal, characters of s)))
end fromRoman

-- charVal :: Char -> Int
on charVal(C)
    set V to keyValue({I:1, V:5, X:10, L:50, C:100, D:500, M:1000}, ¬
        toUpper(C))
    if nothing of V then
        0
    else
        just of V
    end if
end charVal

-- TEST -----------------------------------------------------------------------
on run
    map(fromRoman, {"MDCLXVI", "MCMXC", "MMVIII", "MMXVI", "MMXVII"})

    --> {1666, 1990, 2008, 2016, 2017}
end run


-- GENERIC FUNCTIONS ----------------------------------------------------------

-- foldr :: (a -> b -> a) -> a -> [b] -> a
on foldr(f, startValue, xs)
    tell mReturn(f)
        set V to startValue
        set lng to length of xs
        repeat with I from lng to 1 by -1
            set V to |λ|(V, item I of xs, I, xs)
        end repeat
        return V
    end tell
end foldr

-- keyValue :: Record -> String -> Maybe String
on keyValue(rec, strKey)
    set ca to current application
    set V to (ca's NSDictionary's dictionaryWithDictionary:rec)'s objectForKey:strKey
    if V is not missing value then
        {nothing:false, just:item 1 of ((ca's NSArray's arrayWithObject:V) as list)}
    else
        {nothing:true}
    end if
end keyValue

-- map :: (a -> b) -> [a] -> [b]
on map(f, xs)
    tell mReturn(f)
        set lng to length of xs
        set lst to {}
        repeat with I from 1 to lng
            set end of lst to |λ|(item I of xs, I, xs)
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

-- snd :: (a, b) -> b
on snd(xs)
    if class of xs is list and length of xs = 2 then
        item 2 of xs
    else
        missing value
    end if
end snd

-- toUpper :: String -> String
on toUpper(str)
    set ca to current application
    ((ca's NSString's stringWithString:(str))'s ¬
        uppercaseStringWithLocale:(ca's NSLocale's currentLocale())) as text
end toUpper
```

{{Out}}

```AppleScript
{1666, 1990, 2008, 2016, 2017}
```



## AutoHotkey

{{works with|AutoHotkey_L}}

```AHK
Roman_Decode(str){
	res := 0
	Loop Parse, str
	{
		n := {M: 1000, D:500, C:100, L:50, X:10, V:5, I:1}[A_LoopField]
		If ( n > OldN ) && OldN
			res -= 2*OldN
		res += n, oldN := n
	}
	return res
}

test = MCMXC|MMVIII|MDCLXVI
Loop Parse, test, |
   res .= A_LoopField "`t= " Roman_Decode(A_LoopField) "`r`n"
clipboard := res
```

{{out}}

```txt
MCMXC	= 1990
MMVIII	= 2008
MDCLXVI	= 1666
```



## AWK


```AWK
# syntax: GAWK -f ROMAN_NUMERALS_DECODE.AWK
BEGIN {
    leng = split("MCMXC MMVIII MDCLXVI",arr," ")
    for (i=1; i<=leng; i++) {
      n = arr[i]
      printf("%s = %s\n",n,roman2arabic(n))
    }
    exit(0)
}
function roman2arabic(r,  a,i,p,q,u,ua,una,unr) {
    r = toupper(r)
    unr = "MDCLXVI" # each Roman numeral in descending order
    una = "1000 500 100 50 10 5 1" # and its Arabic equivalent
    split(una,ua," ")
    i = split(r,u,"")
    a = ua[index(unr,u[i])]
    while (--i) {
      p = index(unr,u[i])
      q = index(unr,u[i+1])
      a += ua[p] * ((p>q) ? -1 : 1)
    }
    return( (a>0) ? a : "" )
}
```

{{out}}

```txt

MCMXC = 1990
MMVIII = 2008
MDCLXVI = 1666

```



## BBC BASIC


```bbcbasic
      PRINT "MCMXCIX", FNromandecode("MCMXCIX")
      PRINT "MMXII", FNromandecode("MMXII")
      PRINT "MDCLXVI", FNromandecode("MDCLXVI")
      PRINT "MMMDCCCLXXXVIII", FNromandecode("MMMDCCCLXXXVIII")
      END

      DEF FNromandecode(roman$)
      LOCAL i%, j%, p%, n%, r%()
      DIM r%(7) : r%() = 0,1,5,10,50,100,500,1000
      FOR i% = LEN(roman$) TO 1 STEP -1
        j% = INSTR("IVXLCDM", MID$(roman$,i%,1))
        IF j%=0 ERROR 100, "Invalid character"
        IF j%>=p% n% += r%(j%) ELSE n% -= r%(j%)
        p% = j%
      NEXT
      = n%
```

{{out}}

```txt

MCMXCIX         1999
MMXII           2012
MDCLXVI         1666
MMMDCCCLXXXVIII           3888

```



## Batch File

{{trans|Fortran}}

```dos
@echo off
setlocal enabledelayedexpansion

::Testing...
call :toArabic MCMXC
echo MCMXC = !arabic!
call :toArabic MMVIII
echo MMVIII = !arabic!
call :toArabic MDCLXVI
echo MDCLXVI = !arabic!
call :toArabic CDXLIV
echo CDXLIV = !arabic!
call :toArabic XCIX
echo XCIX = !arabic!
pause>nul
exit/b 0

::The "function"...
:toArabic
set roman=%1
set arabic=
set lastval=
	%== Alternative for counting the string length ==%
set leng=-1
for /l %%. in (0,1,1000) do set/a leng+=1&if "!roman:~%%.,1!"=="" goto break
:break
set /a last=!leng!-1
for /l %%i in (!last!,-1,0) do (
	set n=0
	if /i "!roman:~%%i,1!"=="M" set n=1000
	if /i "!roman:~%%i,1!"=="D" set n=500
	if /i "!roman:~%%i,1!"=="C" set n=100
	if /i "!roman:~%%i,1!"=="L" set n=50
	if /i "!roman:~%%i,1!"=="X" set n=10
	if /i "!roman:~%%i,1!"=="V" set n=5
	if /i "!roman:~%%i,1!"=="I" set n=1

	if !n! lss !lastval! (
		set /a arabic-=n
	) else (
		set /a arabic+=n
	)
	set lastval=!n!
)
goto :EOF
```

{{Out}}

```txt
MCMXC = 1990
MMVIII = 2008
MDCLXVI = 1666
CDXLIV = 444
XCIX = 99
```



## Bracmat

{{trans|Icon and Unicon}}

```bracmat
  ( unroman
  =   nbr,lastVal,val
    .     0:?nbr:?lastVal
        & @( low$!arg
           :   ?
               %@?L
               ( ?
               &     (m.1000)
                     (d.500)
                     (c.100)
                     (l.50)
                     (x.10)
                     (v.5)
                     (i.1)
                 : ? (!L.?val) ?
               &     (!val:~>!lastVal|!val+-2*!lastVal)
                   + !nbr
                 : ?nbr
               & !val:?lastVal
               & ~
               )
           )
      | !nbr
  )
&     (M.1000)
      (MCXI.1111)
      (CMXI.911)
      (MCM.1900)
      (MCMXC.1990)
      (MMVIII.2008)
      (MMIX.2009)
      (MCDXLIV.1444)
      (MDCLXVI.1666)
      (MMXII.2012)
  : ?years
& (test=.out$(!arg unroman$!arg))
& (   !years
    : ? (?L.?D) (?&test$!L&~)
  | done
  );
```

{{out}}

```txt
M 1000
MCXI 1111
CMXI 911
MCM 1900
MCMXC 1990
MMVIII 2008
MMIX 2009
MCDXLIV 1444
MDCLXVI 1666
MMXII 2012
```



## C

Note: the code deliberately did not distinguish between "I", "J" or "U", "V", doing what Romans did for fun.

```c
#include <stdio.h>

int digits[26] = { 0, 0, 100, 500, 0, 0, 0, 0, 1, 1, 0, 50, 1000, 0, 0, 0, 0, 0, 0, 0, 5, 5, 0, 10, 0, 0 };

/* assuming ASCII, do upper case and get index in alphabet. could also be
        inline int VALUE(char x) { return digits [ (~0x20 & x) - 'A' ]; }
   if you think macros are evil */
#define VALUE(x) digits[(~0x20 & (x)) - 'A']

int decode(const char * roman)
{
        const char *bigger;
        int current;
        int arabic = 0;
        while (*roman != '\0') {
                current = VALUE(*roman);
                /*      if (!current) return -1;
                        note: -1 can be used as error code; Romans didn't even have zero
                */
                bigger = roman;

                /* look for a larger digit, like IV or XM */
                while (VALUE(*bigger) <= current && *++bigger != '\0');

                if (*bigger == '\0')
                        arabic += current;
                else {
                        arabic += VALUE(*bigger);
                        while (roman < bigger)
                                arabic -= VALUE(* (roman++) );
                }

                roman ++;
        }
        return arabic;
}

int main()
{
        const char * romans[] = { "MCmxC", "MMVIII", "MDClXVI", "MCXLUJ" };
        int i;

        for (i = 0; i < 4; i++)
                printf("%s\t%d\n", romans[i], decode(romans[i]));

        return 0;
}
```



## C++


```cpp

#include <exception>
#include <string>
#include <iostream>
using namespace std;

namespace Roman
{
	int ToInt(char c)
	{
		switch (c)
		{
			case 'I':  return 1;
			case 'V':  return 5;
			case 'X':  return 10;
			case 'L':  return 50;
			case 'C':  return 100;
			case 'D':  return 500;
			case 'M':  return 1000;
		}
		throw exception("Invalid character");
	}

	int ToInt(const string& s)
	{
		int retval = 0, pvs = 0;
		for (auto pc = s.rbegin(); pc != s.rend(); ++pc)
		{
			const int inc = ToInt(*pc);
			retval += inc < pvs ? -inc : inc;
			pvs = inc;
		}
		return retval;
	}
}

int main(int argc, char* argv[])
{
	try
	{
		cout << "MCMXC = " << Roman::ToInt("MCMXC") << "\n";
		cout << "MMVIII = " << Roman::ToInt("MMVIII") << "\n";
		cout << "MDCLXVI = " << Roman::ToInt("MDCLXVI") << "\n";
	}
	catch (exception& e)
	{
		cerr << e.what();
		return -1;
	}
	return 0;
}

```

{{out}}
<PRE>MCMXC = 1990
MMVIII = 2008
MDCLXVI = 1666</PRE>

## C#

```c#
using System;
using System.Collections.Generic;

namespace Roman
{
    internal class Program
    {
        private static void Main(string[] args)
        {
            // Decode and print the numerals.
            Console.WriteLine("{0}: {1}", "MCMXC", Decode("MCMXC"));
            Console.WriteLine("{0}: {1}", "MMVIII", Decode("MMVIII"));
            Console.WriteLine("{0}: {1}", "MDCLXVI", Decode("MDCLXVI"));
        }

        // Dictionary to hold our numerals and their values.
        private static readonly Dictionary<char, int> RomanDictionary = new Dictionary<char, int>
                                                                            {
                                                                                {'I', 1},
                                                                                {'V', 5},
                                                                                {'X', 10},
                                                                                {'L', 50},
                                                                                {'C', 100},
                                                                                {'D', 500},
                                                                                {'M', 1000}
                                                                            };

        private static int Decode(string roman)
        {
            /* Make the input string upper-case,
             * because the dictionary doesn't support lower-case characters. */
            roman = roman.ToUpper();

            /* total = the current total value that will be returned.
             * minus = value to subtract from next numeral. */
            int total = 0, minus = 0;

            for (int i = 0; i < roman.Length; i++) // Iterate through characters.
            {
                // Get the value for the current numeral. Takes subtraction into account.
                int thisNumeral = RomanDictionary[roman[i]] - minus;

                /* Checks if this is the last character in the string, or if the current numeral
                 * is greater than or equal to the next numeral. If so, we will reset our minus
                 * variable and add the current numeral to the total value. Otherwise, we will
                 * subtract the current numeral from the next numeral, and continue. */
                if (i >= roman.Length - 1 ||
                    thisNumeral + minus >= RomanDictionary[roman[i + 1]])
                {
                    total += thisNumeral;
                    minus = 0;
                }
                else
                {
                    minus = thisNumeral;
                }
            }

            return total; // Return the total.
        }
    }
}
```

{{out}}

```txt
MCMXC: 1990
MMVIII: 2008
MDCLXVI: 1666

```



## Ceylon


```ceylon
shared void run() {

	value numerals = map {
		'I' -> 1,
		'V' -> 5,
		'X' -> 10,
		'L' -> 50,
		'C' -> 100,
		'D' -> 500,
		'M' -> 1000
	};

	function toHindu(String roman) {
		variable value total = 0;
		for(i->c in roman.indexed) {
			assert(exists currentValue = numerals[c]);
			/* Look at the next letter to see if we're looking
			 at a IV or CM or whatever. If so subtract the
			 current number from the total. */
			if(exists next = roman[i + 1],
				exists nextValue = numerals[next],
				currentValue < nextValue) {
				total -= currentValue;
			} else {
				total += currentValue;
			}
		}
		return total;
	}

	assert(toHindu("I") == 1);
	assert(toHindu("II") == 2);
	assert(toHindu("IV") == 4);
	assert(toHindu("MDCLXVI") == 1666);
	assert(toHindu("MCMXC") == 1990);
	assert(toHindu("MMVIII") == 2008);
}
```



## Clojure


```clojure

;; Incorporated some improvements from the alternative implementation below
(defn ro2ar [r]
  (->> (reverse (.toUpperCase r))
       (map {\M 1000 \D 500 \C 100 \L 50 \X 10 \V 5 \I 1})
       (partition-by identity)
       (map (partial apply +))
       (reduce #(if (< %1 %2) (+ %1 %2) (- %1 %2)))))

;; alternative
(def numerals { \I 1, \V 5, \X 10, \L 50, \C 100, \D 500, \M 1000})
(defn from-roman [s]
  (->> s .toUpperCase
    (map numerals)
    (reduce (fn [[sum lastv] curr] [(+ sum curr (if (< lastv curr) (* -2 lastv) 0)) curr]) [0,0])
    first))
```


{{out}}

```txt
(map ro2ar ["MDCLXVI" "MMMCMXCIX" "XLVIII" "MMVIII"])
(1666 3999 48 2008)
```



## COBOL


```COBOL

      IDENTIFICATION DIVISION.
      PROGRAM-ID.  UNROMAN.
      DATA DIVISION.
      WORKING-STORAGE SECTION.
      01  filler.
        03  i              pic 9(02) comp.
        03  j              pic 9(02) comp.
        03  k              pic 9(02) comp.
        03  l              pic 9(02) comp.
      01  inp-roman.
        03  inp-rom-ch     pic x(01) occurs 20 times.
      01  inp-roman-digits.
        03  inp-rom-digit  pic 9(01) occurs 20 times.
      01  ws-search-idx        pic 9(02) comp.
      01  ws-tbl-table-def.
        03  filler pic x(05) value '1000M'.
        03  filler pic x(05) value '0500D'.
        03  filler pic x(05) value '0100C'.
        03  filler pic x(05) value '0050L'.
        03  filler pic x(05) value '0010X'.
        03  filler pic x(05) value '0005V'.
        03  filler pic x(05) value '0001I'.
      01  filler redefines ws-tbl-table-def.
        03  ws-tbl-roman      occurs 07 times indexed by rx.
          05  ws-tbl-rom-val  pic 9(04).
          05  ws-tbl-rom-ch   pic x(01).
      01  ws-number           pic s9(05) value 0.
      01  ws-number-pic       pic zzzz9-.

      PROCEDURE DIVISION.
          accept inp-roman
          perform
          until inp-roman = ' '
            move zeroes to inp-roman-digits
            perform
            varying i from 1 by +1 until inp-rom-ch (i) = ' '
              set rx to 1
              search ws-tbl-roman
                at end
                  move 0 to inp-rom-digit (i)
                when ws-tbl-rom-ch (rx) = inp-rom-ch (i)
                  set inp-rom-digit (i) to rx
              end-search
            end-perform
            compute l = i - 1
            move 0 to ws-number
            perform
            varying i from 1 by +1
            until i > l or inp-rom-digit (i) = 0
              compute j = inp-rom-digit (i)
              compute k = inp-rom-digit (i + 1)
              if ws-tbl-rom-val (k)
              >  ws-tbl-rom-val (j)
                compute ws-number
                =      ws-number
                -      ws-tbl-rom-val (j)
              else
                compute ws-number
                =      ws-number
                +      ws-tbl-rom-val (j)
              end-if
            end-perform
            move ws-number to ws-number-pic
            display '----------'
            display 'roman=' inp-roman
            display 'arabic=' ws-number-pic
            if i < l or ws-number = 0
              display 'invalid/incomplete roman numeral at pos 'i
                      ' found ' inp-rom-ch (i)
            end-if
            accept inp-roman
          end-perform
          stop run
          .
      END PROGRAM UNROMAN.

```

{{out}} input was supplied via STDIN

```txt

----------
roman=MCMLXXXVIII

arabic= 1988
----------
roman=MIX

arabic= 1009
----------
roman=MDCCCLXXXVII

arabic= 1887
----------
roman=IX

arabic=    9
----------
roman=MMMDCCCLXXXVIII

arabic= 3888
----------
roman=K

arabic=    0
invalid/incomplete roman numeral at pos 01 found K
----------
roman=MIXT

arabic= 1009
invalid/incomplete roman numeral at pos 04 found T
----------
roman=MCMB

arabic= 1900
invalid/incomplete roman numeral at pos 04 found B

```



## CoffeeScript


```coffeescript
roman_to_demical = (s) ->
  # s is well-formed Roman Numeral >= I
  numbers =
    M: 1000
    D: 500
    C: 100
    L: 50
    X: 10
    V: 5
    I: 1

  result = 0
  for c in s
    num = numbers[c]
    result += num
    if old_num < num
      # If old_num exists and is less than num, then
      # we need to subtract it twice, once because we
      # have already added it on the last pass, and twice
      # to conform to the Roman convention that XC = 90,
      # not 110.
      result -= 2 * old_num
    old_num = num
  result

tests =
  IV: 4
  XLII: 42
  MCMXC: 1990
  MMVIII: 2008
  MDCLXVI: 1666

for roman, expected of tests
  dec = roman_to_demical(roman)
  console.log "error" if dec != expected
  console.log "#{roman} = #{dec}"
```



## Common Lisp


```lisp

(defun mapcn (chars nums string)
  (loop as char across string as i = (position char chars) collect (and i (nth i nums))))

(defun parse-roman (R)
  (loop with nums = (mapcn "IVXLCDM" '(1 5 10 50 100 500 1000) R)
        as (A B) on nums if A sum (if (and B (< A B)) (- A) A)))

```


Description:


```txt
Mapcn is a function to map characters to numbers.  It uses the mapping between its first two arguments, chars and nums,
to map its 3rd argument, string, to a list of numbers.  If a character of string is missing from chars, its number will be
nil.

Parse-roman uses mapcn to map R to a list of numbers, then iterates that list with A and B, adding A to the total whenever
it's not less than B, and subtracting it when it is.  If A is nil, it's skipped.  Such as when the character is not Roman.
If B is nil, A is added and not subtracted.  Such as at the end of the list, or when a non-Roman character, such as a space,
is embedded in the Roman.
```


Test code:


```lisp
(dolist (r '("MCMXC" "MDCLXVI" "MMVIII"))
  (format t "~a:~10t~d~%" r (parse-roman r)))
```

{{out}}

```txt
MCMXC:    1990
MDCLXVI:  1666
MMVIII:   2008
```



## D


```d
import std.regex, std.algorithm;

int toArabic(in string s) /*pure nothrow*/ {
    static immutable weights = [1000, 900, 500, 400, 100,
                                90, 50, 40, 10, 9, 5, 4, 1];
    static immutable symbols = ["M","CM","D","CD","C","XC",
                                "L","XL","X","IX","V","IV","I"];

    int arabic;
    foreach (m; s.matchAll("CM|CD|XC|XL|IX|IV|[MDCLXVI]".regex))
        arabic += weights[symbols.countUntil(m.hit)];
    return arabic;
}

void main() {
    assert("MCMXC".toArabic == 1990);
    assert("MMVIII".toArabic == 2008);
    assert("MDCLXVI".toArabic == 1666);
}
```

Alternative more functional version:

```d
import std.regex, std.algorithm;

immutable uint[string] w2s;

pure nothrow static this() {
    w2s = ["IX":  9, "C":  100, "D":  500, "CM": 900, "I":   1,
           "XC": 90, "M": 1000, "L":   50, "CD": 400, "XL": 40,
           "V":   5, "X":   10, "IV":   4];
}

uint toArabic(in string s) /*pure nothrow*/ @safe /*@nogc*/ {
    return s
           .matchAll("CM|CD|XC|XL|IX|IV|[MDCLXVI]".regex)
           .map!(m => w2s[m.hit])
           .sum;
}

void main() {
    assert("MCMXC".toArabic == 1990);
    assert("MMVIII".toArabic == 2008);
    assert("MDCLXVI".toArabic == 1666);
}
```


=={{header|Delphi}}/{{header|Pascal}}==

```delphi
program RomanNumeralsDecode;

{$APPTYPE CONSOLE}

function RomanToInteger(const aRoman: string): Integer;
  function DecodeRomanDigit(aChar: Char): Integer;
  begin
    case aChar of
      'M', 'm': Result := 1000;
      'D', 'd': Result := 500;
      'C', 'c': Result := 100;
      'L', 'l': Result := 50;
      'X', 'x': Result := 10;
      'V', 'v': Result := 5;
      'I', 'i': Result := 1
    else
      Result := 0;
    end;
  end;

var
  i: Integer;
  lCurrVal: Integer;
  lLastVal: Integer;
begin
  Result := 0;

  lLastVal := 0;
  for i := Length(aRoman) downto 1 do
  begin
    lCurrVal := DecodeRomanDigit(aRoman[i]);
    if lCurrVal < lLastVal then
      Result := Result - lCurrVal
    else
      Result := Result + lCurrVal;
    lLastVal := lCurrVal;
  end;
end;

begin
  Writeln(RomanToInteger('MCMXC'));    // 1990
  Writeln(RomanToInteger('MMVIII'));   // 2008
  Writeln(RomanToInteger('MDCLXVI'));  // 1666
end.
```



## ECL

The best declarative approach:

```ECL

MapChar(STRING1 c) := CASE(c,'M'=>1000,'D'=>500,'C'=>100,'L'=>50,'X'=>10,'V'=>5,'I'=>1,0);

RomanDecode(STRING s) := FUNCTION
  dsS := DATASET([{s}],{STRING Inp});
  R := { INTEGER2 i; };

  R Trans1(dsS le,INTEGER pos) := TRANSFORM
    SELF.i := MapChar(le.Inp[pos]) * IF ( MapChar(le.Inp[pos]) < MapChar(le.Inp[pos+1]), -1, 1 );
  END;

  RETURN SUM(NORMALIZE(dsS,LENGTH(TRIM(s)),Trans1(LEFT,COUNTER)),i);
END;

RomanDecode('MCMLIV');   //1954
RomanDecode('MCMXC');    //1990
RomanDecode('MMVIII');   //2008
RomanDecode('MDCLXVI');  //1666
RomanDecode('MDLXVI');   //1566
```

Here's an alternative that emulates the wat procedural code would approach the problem:

```ECL
IMPORT STD;
RomanDecode(STRING s) := FUNCTION
  SetWeights := [1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1];
  SetSymbols := ['M', 'CM', 'D', 'CD', 'C', 'XC', 'L', 'XL', 'X', 'IX', 'V', 'IV', 'I'];
  ProcessRec := RECORD
    UNSIGNED val;
    STRING Roman;
  END;
  dsSymbols := DATASET(13,TRANSFORM(ProcessRec,SELF.Roman := s, SELF := []));

  RECORDOF(dsSymbols) XF(dsSymbols L, dsSymbols R, INTEGER C) := TRANSFORM
    ThisRoman := IF(C=1,R.Roman,L.Roman);
    IsDone := ThisRoman = '';
    Repeatable := C IN [1,5,9,13];
    SymSize := IF(C % 2 = 0, 2, 1);
    IsNext := STD.Str.StartsWith(ThisRoman,SetSymbols[C]);
    SymLen := IF(IsNext,
                 IF(NOT Repeatable,
                    SymSize,
                    MAP(NOT IsDone AND ThisRoman[1] = ThisRoman[2] AND ThisRoman[1] = ThisRoman[3] => 3,
                        NOT IsDone AND ThisRoman[1] = ThisRoman[2] => 2,
                        NOT IsDone  => 1,
                        0)),
                 0);

    SymbolWeight(STRING s) := IF(NOT Repeatable,
                                 SetWeights[C],
                                 CHOOSE(LENGTH(s),SetWeights[C],SetWeights[C]*2,SetWeights[C]*3,0));

    SELF.Roman := IF(IsDone,ThisRoman,ThisRoman[SymLen+1..]);
    SELF.val   := IF(IsDone,L.val,L.Val + IF(IsNext,SymbolWeight(ThisRoman[1..SymLen]),0));
  END;
  i := ITERATE(dsSymbols,XF(LEFT,RIGHT,COUNTER));
  RETURN i[13].val;
END;

RomanDecode('MCMLIV');   //1954
RomanDecode('MCMXC');    //1990
RomanDecode('MMVIII');   //2008
RomanDecode('MDCLXVI');  //1666
RomanDecode('MDLXVI');   //1566
```




## Eiffel


This solution is case insensitive. It performs no input validation other than checking that all Roman digits in the input string are one of <tt>M</tt>, <tt>D</tt>, <tt>C</tt>, <tt>L</tt>, <tt>X</tt>, <tt>V</tt>, and <tt>I</tt>.


```Eiffel
class
	APPLICATION

create
	make

feature {NONE} -- Initialization

	make
		local
			numbers: ARRAY [STRING]
		do
			numbers := <<"MCMXC", "MMVIII", "MDCLXVI",
			           -- 1990     2008      1666
			              "MMMCLIX", "MCMLXXVII", "MMX">>
			           -- 3159       1977         2010
			across numbers as n loop
				print (n.item +
                                       " in Roman numerals is " +
				       roman_to_decimal (n.item).out +
                                       " in decimal Arabic numerals.")
				print ("%N")
			end
		end

feature -- Roman numerals

	roman_to_decimal (a_str: STRING): INTEGER
		-- Decimal representation of Roman numeral `a_str'
		require
			is_roman (a_str)
		local
			l_pos: INTEGER
			cur: INTEGER -- Value of the digit read in the current iteration
			prev: INTEGER -- Value of the digit read in the previous iteration
		do
			from
				l_pos := 0
				Result := 0
				prev := 1 + max_digit_value
			until
				l_pos = a_str.count
			loop
				l_pos := l_pos + 1
				cur := roman_digit_to_decimal (a_str.at (l_pos))
				if cur <= prev then
					-- Add nonincreasing digit
					Result := Result + cur
				else
					-- Subtract previous digit from increasing digit
					Result := Result - prev + (cur - prev)
				end
				prev := cur
			end
		ensure
			Result >= 0
		end

	is_roman (a_string: STRING): BOOLEAN
		-- Is `a_string' a valid sequence of Roman digits?
		do
			Result := across a_string as c all is_roman_digit (c.item) end
		end

feature {NONE} -- Implementation

	max_digit_value: INTEGER = 1000

	is_roman_digit (a_char: CHARACTER): BOOLEAN
		-- Is `a_char' a valid Roman digit?
		local
			l_char: CHARACTER
		do
			l_char := a_char.as_upper
			inspect l_char
				when 'I', 'V', 'X', 'L', 'C', 'D', 'M' then
					Result := True
				else
					Result := False
			end
		end

	roman_digit_to_decimal (a_char: CHARACTER): INTEGER
		-- Decimal representation of Roman digit `a_char'
		require
			is_roman_digit (a_char)
		local
			l_char: CHARACTER
		do
			l_char := a_char.as_upper
			inspect l_char
				when 'I' then
					Result := 1
				when 'V' then
					Result := 5
				when 'X' then
					Result := 10
				when 'L' then
					Result := 50
				when 'C' then
					Result := 100
				when 'D' then
					Result := 500
				when 'M' then
					Result := 1000
			end
		ensure
			Result > 0
		end

end
```


## Elena

ELENA 4.x :

```elena
import extensions;
import system'collections;
import system'routines;

static RomanDictionary = new Dictionary()
                            .setAt("I".toChar(), 1)
                            .setAt("V".toChar(), 5)
                            .setAt("X".toChar(), 10)
                            .setAt("L".toChar(), 50)
                            .setAt("C".toChar(), 100)
                            .setAt("D".toChar(), 500)
                            .setAt("M".toChar(), 1000);

extension op : String
{
    toRomanInt()
    {
        var minus := 0;
        var s := self.upperCase();
        var total := 0;

        for(int i := 0, i < s.Length, i += 1)
        {
            var thisNumeral := RomanDictionary[s[i]] - minus;
            if (i >= s.Length - 1 || thisNumeral + minus >= RomanDictionary[s[i + 1]])
            {
                total += thisNumeral;
                minus := 0
            }
            else
            {
                minus := thisNumeral
            }
        };

        ^ total
    }
}

public program()
{
    console.printLine("MCMXC:  ", "MCMXC".toRomanInt());
    console.printLine("MMVIII: ", "MMVIII".toRomanInt());
    console.printLine("MDCLXVI:", "MDCLXVI".toRomanInt())
}
```

{{out}}

```txt

MCMXC:  1990
MMVIII: 2008
MDCLXVI:1666

```



## Elixir

{{trans|Erlang}}

```elixir
defmodule Roman_numeral do
  def decode([]), do: 0
  def decode([x]), do: to_value(x)
  def decode([h1, h2 | rest]) do
    case {to_value(h1), to_value(h2)} do
      {v1, v2} when v1 < v2 -> v2 - v1 + decode(rest)
      {v1, _} -> v1 + decode([h2 | rest])
    end
  end

  defp to_value(?M), do: 1000
  defp to_value(?D), do:  500
  defp to_value(?C), do:  100
  defp to_value(?L), do:   50
  defp to_value(?X), do:   10
  defp to_value(?V), do:    5
  defp to_value(?I), do:    1
end

Enum.each(['MCMXC', 'MMVIII', 'MDCLXVI', 'IIIID'], fn clist ->
  IO.puts "#{clist}\t: #{Roman_numeral.decode(clist)}"
end)
```


{{out}}

```txt

MCMXC   : 1990
MMVIII  : 2008
MDCLXVI : 1666

```



## Emacs Lisp


```lisp

(defun ro2ar (RN)
  "translate a roman number RN into arabic number.
   Its argument RN is wether a symbol, wether a list.
   Returns the arabic number. (ro2ar 'C) gives 100,
   (ro2ar '(X X I V)) gives 24"
  (cond
   ((eq RN 'M) 1000)
   ((eq RN 'D) 500)
   ((eq RN 'C) 100)
   ((eq RN 'L) 50)
   ((eq RN 'X) 10)
   ((eq RN 'V) 5)
   ((eq RN 'I) 1)
   ((null (cdr RN)) (ro2ar (car RN))) ;; stop recursion
   ((< (ro2ar (car RN)) (ro2ar (car (cdr RN)))) (- (ro2ar (cdr RN)) (ro2ar (car RN)))) ;; "IV" -> 5-1=4
   (t (+ (ro2ar (car RN)) (ro2ar (cdr RN)))))) ;; "VI" -> 5+1=6

```


{{out}}

```txt

(ro2ar '(M D C L X V I)) -> 1666

```



## Erlang

Putting the character X into a list, [X], creates a string with a single character.


```Erlang

-module( roman_numerals ).

-export( [decode_from_string/1]).

to_value($M) -> 1000;
to_value($D) ->  500;
to_value($C) ->  100;
to_value($L) ->   50;
to_value($X) ->   10;
to_value($V) ->    5;
to_value($I) ->    1.

decode_from_string([]) -> 0;
decode_from_string([H1]) -> to_value(H1);
decode_from_string([H1, H2 |Rest]) ->
    case {to_value(H1), to_value(H2)} of
        {V1, V2} when V1 < V2 -> V2 - V1 + decode_from_string(Rest);
        {V1, V1} -> V1 + V1 + decode_from_string(Rest);
        {V1, _} -> V1 + decode_from_string([H2|Rest])
    end.

```


{{out}}

```txt

10> roman_numerals:decode_from_string("MCMXC").
1990
11> roman_numerals:decode_from_string("MMVIII").
2008
12> roman_numerals:decode_from_string("MDCLXVI").
1666

```



## ERRE


```ERRE

PROGRAM ROMAN2ARAB

DIM R%[7]

PROCEDURE TOARAB(ROMAN$->ANS%)
      LOCAL I%,J%,P%,N%
      FOR I%=LEN(ROMAN$) TO 1 STEP -1 DO
        J%=INSTR("IVXLCDM",MID$(ROMAN$,I%,1))
        IF J%=0 THEN
            ANS%=-9999  ! illegal character
            EXIT PROCEDURE
        END IF
        IF J%>=P% THEN
            N%+=R%[J%]
          ELSE
            N%-=R%[J%]
        END IF
        P%=J%
      END FOR
      ANS%=N%
END PROCEDURE

BEGIN
  R%[]=(0,1,5,10,50,100,500,1000)
  TOARAB("MCMXCIX"->ANS%)         PRINT(ANS%)
  TOARAB("MMXII"->ANS%)           PRINT(ANS%)
  TOARAB("MDCLXVI"->ANS%)         PRINT(ANS%)
  TOARAB("MMMDCCCLXXXVIII"->ANS%) PRINT(ANS%)
END PROGRAM

```

If the answer is -9999, roman number is illegal.


## Euphoria

{{trans|PureBasic}}

```euphoria
constant symbols = "MDCLXVI", weights = {1000,500,100,50,10,5,1}
function romanDec(sequence roman)
    integer n, lastval, arabic
    lastval = 0
    arabic = 0
    for i = length(roman) to 1 by -1 do
        n = find(roman[i],symbols)
        if n then
            n = weights[n]
        end if
        if n < lastval then
            arabic -= n
        else
            arabic += n
        end if
        lastval = n
    end for
    return arabic
end function

? romanDec("MCMXCIX")
? romanDec("MDCLXVI")
? romanDec("XXV")
? romanDec("CMLIV")
? romanDec("MMXI")
```

{{out}}

```txt
1999
1666
25
954
2011
```


=={{header|F Sharp|F#}}==
This implementation uses tail recursion. The accumulator (arabic) and the last roman digit (lastval) are recursively passed as each element of the list is consumed.

```fsharp
let decimal_of_roman roman =
    let rec convert arabic lastval = function
        | head::tail ->
            let n = match head with
                    | 'M' | 'm' -> 1000
                    | 'D' | 'd' -> 500
                    | 'C' | 'c' -> 100
                    | 'L' | 'l' -> 50
                    | 'X' | 'x' -> 10
                    | 'V' | 'v' -> 5
                    | 'I' | 'i' -> 1
                    | _ -> 0
            let op = if n > lastval then (-) else (+)
            convert (op arabic lastval) n tail
        | _ -> arabic + lastval
    convert 0 0 (Seq.toList roman)
;;
```


Here is an alternative implementation that uses Seq(uence).fold. It threads a Tuple of the state (accumulator, last roman digit) through the list of characters.

```fsharp
let decimal_of_roman roman =
    let convert (arabic,lastval) c =
        let n = match c with
                | 'M' | 'm' -> 1000
                | 'D' | 'd' -> 500
                | 'C' | 'c' -> 100
                | 'L' | 'l' -> 50
                | 'X' | 'x' -> 10
                | 'V' | 'v' -> 5
                | 'I' | 'i' -> 1
                | _ -> 0
        let op = if n > lastval then (-) else (+)
        (op arabic lastval, n)
    let (arabic, lastval) = Seq.fold convert (0,0) roman
    arabic + lastval
;;
```


Test code:

```fsharp
let tests = ["MCMXC"; "MMVIII"; "MDCLXVII"; "MMMCLIX"; "MCMLXXVII"; "MMX"]
for test in tests do Printf.printf "%s: %d\n" test (decimal_of_roman test)
;;
```


{{out}}

```txt
MCMXC: 1990
MMVIII: 2008
MDCLXVII: 1667
MMMCLIX: 3159
MCMLXXVII: 1977
MMX: 2010
```



## Factor

A roman numeral library ships with Factor.

```factor
USE: roman
( scratchpad ) "MMMCCCXXXIII" roman> .
3333
```


Implementation for decoding:


```factor
CONSTANT: roman-digits
    { "m" "cm" "d" "cd" "c" "xc" "l" "xl" "x" "ix" "v" "iv" "i" }

CONSTANT: roman-values
    { 1000 900 500 400 100 90 50 40 10 9 5 4 1 }

: roman> ( str -- n )
  >lower [ roman-digit>= ] monotonic-split
  [ roman-value ] map-sum ;

: roman-digit>= ( ch1 ch2 -- ? ) [ roman-digit-index ] bi@ >= ;

: roman-digit-index ( ch -- n ) 1string roman-digits index ;

: roman-value (seq -- n )
  [ [ roman-digit-value ] map ] [ all-eq? ] bi
  [ sum ] [ first2 swap - ] if ;

: roman-digit-value ( ch -- n )
  roman-digit-index roman-values nth ;
```



## Forth


```forth
create (arabic)
  1000 128 * char M + ,
   500 128 * char D + ,
   100 128 * char C + ,
    50 128 * char L + ,
    10 128 * char X + ,
     5 128 * char V + ,
     1 128 * char I + ,
does>
  7 cells bounds do
    i @ over over 127 and = if nip 7 rshift leave else drop then
  1 cells +loop dup
;

: >arabic
  0 dup >r >r
  begin
    over over
  while
    c@ dup (arabic) rot <>
  while
    r> over r> over over > if 2* negate + else drop then + swap >r >r 1 /string
  repeat then drop 2drop r> r> drop
;

s" MCMLXXXIV" >arabic .
```




```forth
\ decode roman numerals using Forth methodology
\ create words to describe and solve the problem
\ ANS/ISO Forth

\ state holders
VARIABLE OLDNDX
VARIABLE CURNDX
VARIABLE NEGFLAG

DECIMAL
CREATE VALUES ( -- addr) 0 , 1 , 5 , 10 , 50 , 100 , 500 , 1000 ,

: NUMERALS ( -- addr len)  S"  IVXLCDM" ;        \ 1st char is a blank
: []       ( n addr -- addr') SWAP CELLS +  ;    \ array address calc.
: INIT     ( -- )         CURNDX OFF  OLDNDX OFF  NEGFLAG OFF ;
: REMEMBER ( ndx -- ndx ) CURNDX @ OLDNDX !  DUP CURNDX !  ;
: ]VALUE@  ( ndx -- n )   REMEMBER VALUES [] @ ;
HEX
: TOUPPER ( char -- char ) 05F AND ;

DECIMAL
: >INDEX   ( char -- ndx) TOUPPER >R  NUMERALS TUCK R> SCAN NIP -
                          DUP 7 > ABORT" Invalid Roman numeral" ;

: >VALUE   ( char -- n ) >INDEX ]VALUE@ ;
: ?ILLEGAL ( ndx --  )   CURNDX @ OLDNDX @ =  NEGFLAG @ AND ABORT" Illegal format" ;

: ?NEGATE ( n -- +n | -n) \ conditional NEGATE
           CURNDX @ OLDNDX @ <
           IF   NEGFLAG ON  NEGATE
           ELSE ?ILLEGAL  NEGFLAG OFF
           THEN ;

: >ARABIC  ( addr len -- n )
           INIT
           0  -ROT            \ accumulator under the stack string args
           1- BOUNDS          \ convert addr len to two addresses
           SWAP DO            \ index the string from back to front
                  I C@ >VALUE ?NEGATE +
          -1 +LOOP ;
</LANG>

Alternative Version Forth Console Test

```txt
\ test code  ok
S" i"         >ARABIC . 1  ok
S" ii"        >ARABIC . 2  ok
S" iv"        >ARABIC . 4  ok
S" mdclxvi"   >ARABIC . 1666  ok
S" mm"        >ARABIC . 2000  ok
S" mmm"       >ARABIC . 3000  ok
S" MCMLIV"    >ARABIC . 1954  ok
S" mcmxlvi"   >ARABIC . 1946  ok
S" mdc"       >ARABIC . 1600  ok
S" mdcl"      >ARABIC . 1650  ok
S" mdclxvi"   >ARABIC . 1666  ok
S" mcmlxxxiv" >ARABIC . 1984  ok
</PRE>


## Fortran

{{works with|Fortran|90 and later}}

```fortran
program Roman_decode
  implicit none

  write(*,*) decode("MCMXC"), decode("MMVIII"), decode("MDCLXVI")

contains

function decode(roman) result(arabic)
  character(*), intent(in) :: roman
  integer :: i, n, lastval, arabic

  arabic = 0
  lastval = 0
  do i = len(roman), 1, -1
    select case(roman(i:i))
      case ('M','m')
        n = 1000
      case ('D','d')
        n = 500
      case ('C','c')
        n = 100
      case ('L','l')
        n = 50
      case ('X','x')
        n = 10
      case ('V','v')
        n = 5
      case ('I','i')
        n = 1
      case default
        n = 0
    end select
    if (n < lastval) then
      arabic = arabic - n
    else
      arabic = arabic + n
    end if
    lastval = n
  end do
end function decode
end program Roman_decode
```

{{out}}

```txt
        1990        2008        1666
```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Function romanDecode(roman As Const String) As Integer
  If roman = "" Then Return 0  '' zero denotes invalid roman number
  Dim roman1(0 To 2) As String = {"MMM", "MM", "M"}
  Dim roman2(0 To 8) As String = {"CM", "DCCC", "DCC", "DC", "D", "CD", "CCC", "CC", "C"}
  Dim roman3(0 To 8) As String = {"XC", "LXXX", "LXX", "LX", "L", "XL", "XXX", "XX", "X"}
  Dim roman4(0 To 8) As String = {"IX", "VIII", "VII", "VI", "V", "IV", "III", "II", "I"}
  Dim As Integer i, value = 0, length = 0
  Dim r As String = UCase(roman)

  For i = 0 To 2
    If Left(r, Len(roman1(i))) = roman1(i) Then
      value += 1000 * (3 - i)
      length = Len(roman1(i))
      r = Mid(r, length + 1)
      length = 0
      Exit For
    End If
  Next

  For i = 0 To 8
    If Left(r, Len(roman2(i))) = roman2(i) Then
      value += 100 * (9 - i)
      length = Len(roman2(i))
      r = Mid(r, length + 1)
      length = 0
      Exit For
    End If
  Next

  For i = 0 To 8
    If Left(r, Len(roman3(i))) = roman3(i) Then
      value += 10 * (9 - i)
      length = Len(roman3(i))
      r = Mid(r, length + 1)
      length = 0
      Exit For
    End If
  Next

  For i = 0 To 8
    If Left(r, Len(roman4(i))) = roman4(i) Then
      value += 9 - i
      length = Len(roman4(i))
      Exit For
    End If
  Next

  ' Can't be a valid roman number if there are any characters left
  If Len(r) > length Then Return 0
  Return value
End Function

Dim a(2) As String = {"MCMXC", "MMVIII" , "MDCLXVI"}
For i As Integer = 0 To 2
  Print a(i); Tab(8); " =>"; romanDecode(a(i))
Next

Print
Print "Press any key to quit"
Sleep
```


{{out}}

```txt

MCMXC   => 1990
MMVIII  => 2008
MDCLXVI => 1666

```



## FutureBasic


```futurebasic

local fn RomantoDecimal( roman as Str15 ) as short
dim as short i, n, preNum, num

preNum = 0 : num = 0

for i = roman[0] to 1 step -1
   n = 0
   if roman[i] = _"M" then n = 1000
   if roman[i] = _"D" then n = 500
   if roman[i] = _"C" then n = 100
   if roman[i] = _"L" then n = 50
   if roman[i] = _"X" then n = 10
   if roman[i] = _"V" then n = 5
   if roman[i] = _"I" then n = 1
   if n < preNum then num = num - n else num = num + n
   preNum = n
next
end fn = num

print "  MCMXC ="; fn RomantoDecimal( "MCMXC"   )
print " MMVIII ="; fn RomantoDecimal( "MMVIII"  )
print "  MMXVI ="; fn RomantoDecimal( "MMXVI"   )
print "MDCLXVI ="; fn RomantoDecimal( "MDCLXVI" )
print " MCMXIV ="; fn RomantoDecimal( "MCMXIV"  )
print "  DXIII ="; fn RomantoDecimal( "DXIII"   )
print "      M ="; fn RomantoDecimal( "M"       )
print "  DXIII ="; fn RomantoDecimal( "DXIII"   )
print " XXXIII ="; fn RomantoDecimal( "XXXIII"  )

```


Output:

```txt

 MCMXC = 1990
 MMVIII = 2008
  MMXVI = 2016
MDCLXVI = 1666
 MCMXIV = 1914
  DXIII = 513
      M = 1000
  DXIII = 513
 XXXIII = 33

```



## Gambas


```gambas
'This code will create a GUI Form and Objects and carry out the Roman Numeral convertion as you type
'The input is case insensitive
'A basic check for invalid charaters is made

hTextBox As TextBox                                           'To allow the creation of a TextBox
hValueBox As ValueBox                                         'To allow the creation of a ValueBox

Public Sub Form_Open()                                        'Form opens..

SetUpForm                                                     'Go to the SetUpForm Routine
hTextBox.text = "MCMXC"                                       'Put a Roman numeral in the TextBox

End

Public Sub TextBoxInput_Change()                              'Each time the TextBox text changes..
Dim cRomanN As Collection = ["M": 1000, "D": 500, "C": 100, "L": 50, "X": 10, "V": 5, "I": 1]  'Collection of nemerals e.g 'M' = 1000
Dim cMinus As Collection = ["IV": -2, "IX": -2, "XL": -20, "XC": - 20, "CD": -200, "CM": -200] 'Collection of the 'one less than' numbers e.g. 'IV' = 4
Dim sClean, sTemp As String                                   'Various string variables
Dim siCount As Short                                          'Counter
Dim iTotal As Integer                                         'Stores the total of the calculation

hTextBox.Text = UCase(hTextBox.Text)                          'Make any text in the TextBox upper case

For siCount = 1 To Len(hTextBox.Text)                         'Loop through each character in the TextBox
  If InStr("MDCLXVI", Mid(hTextBox.Text, siCount, 1)) Then    'If a Roman numeral exists then..
    sClean &= Mid(hTextBox.Text, siCount, 1)                  'Put it in 'sClean' (Stops input of non Roman numerals)
  End If
Next

hTextBox.Text = sClean                                        'Put the now clean text in the TextBox

For siCount = 1 To Len(hTextBox.Text)                         'Loop through each character in the TextBox
  iTotal += cRomanN[Mid(hTextBox.Text, siCount, 1)]           'Total up all the characters, note 'IX' will = 11 not 9
Next

For Each sTemp In cMinus                                      'Loop through each item in the cMinus Collection
  If InStr(sClean, cMinus.Key) > 0 Then iTotal += Val(sTemp)  'If a 'Minus' value is in the string e.g. 'IX' which has been calculated at 11 subtract 2 = 9
Next

hValueBox.text = iTotal                                       'Display the total

End

Public Sub SetUpForm()                                        'Create the Objects for the Form
Dim hLabel1, hLabel2 As Label                                 'For 2 Labels

Me.height = 150                                               'Form Height
Me.Width = 300                                                'Form Width
Me.Padding = 20                                               'Form padding (border)
Me.Text = "Roman Numeral converter"                           'Text in Form header
Me.Arrangement = Arrange.Vertical                             'Form arrangement

hLabel1 = New Label(Me)                                       'Create a Label
hLabel1.Height = 21                                           'Label Height
hLabel1.expand = True                                         'Expand the Label
hLabel1.Text = "Enter a Roman numeral"                        'Put text in the Label

hTextBox = New TextBox(Me) As "TextBoxInput"                  'Set up a TextBox with an Event Label
hTextBox.Height = 21                                          'TextBox height
hTextBox.expand = True                                        'Expand the TextBox

hLabel2 = New Label(Me)                                       'Create a Label
hLabel2.Height = 21                                           'Label Height
hLabel2.expand = True                                         'Expand the Label
hLabel2.Text = "The decimal equivelent is: -"                 'Put text in the Label

hValueBox = New ValueBox(Me)                                  'Create a ValueBox
hValueBox.Height = 21                                         'ValuBox Height
hValueBox.expand = True                                       'Expand the ValueBox
hValueBox.ReadOnly = True                                     'Set ValueBox to Read Only

End
```

'''[http://www.cogier.com/gambas/Roman%20Numeral%20converter.png Click here for image of running code]'''


## Go

For fluff, the unicode overbar is recognized as a factor of 1000, [http://en.wikipedia.org/wiki/Roman_numerals#Large_numbers as described in WP].

```go
package main

import (
    "errors"
    "fmt"
)

var m = map[rune]int{
    'I': 1,
    'V': 5,
    'X': 10,
    'L': 50,
    'C': 100,
    'D': 500,
    'M': 1000,
}

func parseRoman(s string) (r int, err error) {
    if s == "" {
        return 0, errors.New("Empty string")
    }
    is := []rune(s) // easier to convert string up front
    var c0 rune     // c0: roman character last read
    var cv0 int     // cv0: value of cv

    // the key to the algorithm is to process digits from right to left
    for i := len(is) - 1; i >= 0; i-- {
        // read roman digit
        c := is[i]
        k := c == '\u0305' // unicode overbar combining character
        if k {
            if i == 0 {
                return 0, errors.New(
                    "Overbar combining character invalid at position 0")
            }
            i--
            c = is[i]
        }
        cv := m[c]
        if cv == 0 {
            if c == 0x0305 {
                return 0, fmt.Errorf(
                    "Overbar combining character invalid at position %d", i)
            } else {
                return 0, fmt.Errorf(
                    "Character unrecognized as Roman digit: %c", c)
            }
        }
        if k {
            c = -c // convention indicating overbar
            cv *= 1000
        }

        // handle cases of new, same, subtractive, changed, in that order.
        switch {
        default: // case 4: digit change
            fallthrough
        case c0 == 0: // case 1: no previous digit
            c0 = c
            cv0 = cv
        case c == c0: // case 2: same digit
        case cv*5 == cv0 || cv*10 == cv0: // case 3: subtractive
            // handle next digit as new.
            // a subtractive digit doesn't count as a previous digit.
            c0 = 0
            r -= cv  // subtract...
            continue // ...instead of adding
        }
        r += cv // add, in all cases except subtractive
    }
    return r, nil
}

func main() {
    // parse three numbers mentioned in task description
    for _, r := range []string{"MCMXC", "MMVIII", "MDCLXVI"} {
        v, err := parseRoman(r)
        if err != nil {
            fmt.Println(err)
        } else {
            fmt.Println(r, "==", v)
        }
    }
}
```

{{out}}

```txt

MCMXC == 1990
MMVIII == 2008
MDCLXVI == 1666

```


Simpler:

```go
package main

import (
	"fmt"
)

var m = map[rune]int{
	'I': 1,
	'V': 5,
	'X': 10,
	'L': 50,
	'C': 100,
	'D': 500,
	'M': 1000,
}

// function, per task description
func from_roman(roman string) (arabic int) {
	last_digit := 1000
	for _, r := range roman {
		digit := m[r]
		if last_digit < digit {
			arabic -= 2 * last_digit
		}
		last_digit = digit
		arabic += digit
	}

	return arabic
}

func main() {
	// parse three numbers mentioned in task description
	for _, roman_digit := range []string{"MCMXC", "MMVIII", "MDCLXVI"} {
		fmt.Printf("%-10s == %d\n", roman_digit, from_roman(roman_digit))
	}
}
```



## Golo


```golo
#!/usr/bin/env golosh
----
This module converts a Roman numeral into a decimal number.
----
module Romannumeralsdecode

augment java.lang.Character {

  function decode = |this| -> match {
    when this == 'I' then 1
    when this == 'V' then 5
    when this == 'X' then 10
    when this == 'L' then 50
    when this == 'C' then 100
    when this == 'D' then 500
    when this == 'M' then 1000
    otherwise 0
  }
}

augment java.lang.String {

  function decode = |this| {
    var accumulator = 0
    foreach i in [0..this: length()] {
      let currentChar = this: charAt(i)
      let nextChar = match {
        when i + 1 < this: length() then this: charAt(i + 1)
        otherwise null
      }
      if (currentChar: decode() < (nextChar?: decode() orIfNull 0)) {
        # if this is something like IV or IX or whatever
        accumulator = accumulator - currentChar: decode()
      } else {
        accumulator = accumulator + currentChar: decode()
      }
    }
    return accumulator
  }
}

function main = |args| {
  println("MCMXC = " + "MCMXC": decode())
  println("MMVIII = " + "MMVIII": decode())
  println("MDCLXVI = " + "MDCLXVI": decode())
}

```



## Groovy

Solution:

```groovy
enum RomanDigits {
    I(1), V(5), X(10), L(50), C(100), D(500), M(1000);

    private magnitude;
    private RomanDigits(magnitude) { this.magnitude = magnitude }

    String toString() { super.toString() + "=${magnitude}" }

    static BigInteger parse(String numeral) {
        assert numeral != null && !numeral.empty
        def digits = (numeral as List).collect {
            RomanDigits.valueOf(it)
        }
        def L = digits.size()
        (0..<L).inject(0g) { total, i ->
            def sign = (i == L - 1 || digits[i] >= digits[i+1]) ? 1 : -1
            total + sign * digits[i].magnitude
        }
    }
}
```

Test:

```groovy
println """
Digit Values = ${RomanDigits.values()}
M       => ${RomanDigits.parse('M')}
MCXI    => ${RomanDigits.parse('MCXI')}
CMXI    => ${RomanDigits.parse('CMXI')}
MCM     => ${RomanDigits.parse('MCM')}
MCMXC   => ${RomanDigits.parse('MCMXC')}
MMVIII  => ${RomanDigits.parse('MMVIII')}
MMIX    => ${RomanDigits.parse('MMIX')}
MCDXLIV => ${RomanDigits.parse('MCDXLIV')}
MDCLXVI => ${RomanDigits.parse('MDCLXVI')}
"""
```

{{out}}

```txt
Digit Values = [I=1, V=5, X=10, L=50, C=100, D=500, M=1000]
M       => 1000
MCXI    => 1111
CMXI    => 911
MCM     => 1900
MCMXC   => 1990
MMVIII  => 2008
MMIX    => 2009
MCDXLIV => 1444
MDCLXVI => 1666
```



## Haskell



### =Simple declarative idiom=


Compiles with GHC.


```Haskell

module Main where

------------------------
--  DECODER FUNCTION  --
------------------------

decodeDigit :: Char -> Int
decodeDigit 'I' = 1
decodeDigit 'V' = 5
decodeDigit 'X' = 10
decodeDigit 'L' = 50
decodeDigit 'C' = 100
decodeDigit 'D' = 500
decodeDigit 'M' = 1000
decodeDigit _ = error "invalid digit"

--  We process a Roman numeral from right to left, digit by digit, adding the value.
--  If a digit is lower than the previous then its value is negative.
--  The first digit is always positive.

decode roman = decodeRoman startValue startValue rest
  where
    (first:rest) = reverse roman
    startValue = decodeDigit first

decodeRoman :: Int -> Int -> [Char] -> Int
decodeRoman lastSum _ [] = lastSum
decodeRoman lastSum lastValue (digit:rest) = decodeRoman updatedSum digitValue rest
  where
    digitValue = decodeDigit digit
    updatedSum = (if digitValue < lastValue then (-) else (+)) lastSum digitValue

------------------
--  TEST SUITE  --
------------------

main = do
  test "MCMXC" 1990
  test "MMVIII" 2008
  test "MDCLXVI" 1666

test roman expected = putStrLn (roman ++ " = " ++ (show (arabic)) ++ remark)
  where
    arabic = decode roman
    remark = " (" ++ (if arabic == expected then "PASS" else ("FAIL, expected " ++ (show expected))) ++ ")"

```


{{Out}}

```txt

MCMXC = 1990 (PASS)
MMVIII = 2008 (PASS)
MDCLXVI = 1666 (PASS)

```



### =Same logic as above but in a functional idiom=



```Haskell

module Main where

------------------------
--  DECODER FUNCTION  --
------------------------

decodeDigit :: Char -> Int
decodeDigit 'I' = 1
decodeDigit 'V' = 5
decodeDigit 'X' = 10
decodeDigit 'L' = 50
decodeDigit 'C' = 100
decodeDigit 'D' = 500
decodeDigit 'M' = 1000
decodeDigit _ = error "invalid digit"

--  We process a Roman numeral from right to left, digit by digit, adding the value.
--  If a digit is lower than the previous then its value is negative.
--  The first digit is always positive.

decode roman = fst (foldl addValue (0, 0) (reverse roman))
  where
    addValue (lastSum, lastValue) digit = (updatedSum, value)
      where
        value = decodeDigit digit;
        updatedSum = (if value < lastValue then (-) else (+)) lastSum value

------------------
--  TEST SUITE  --
------------------

main = do
  test "MCMXC" 1990
  test "MMVIII" 2008
  test "MDCLXVI" 1666

test roman expected = putStrLn (roman ++ " = " ++ (show (arabic)) ++ remark)
  where
    arabic = decode roman
    remark = " (" ++ (if arabic == expected then "PASS" else ("FAIL, expected " ++ (show expected))) ++ ")"

```



### =List comprehension=


```Haskell
import Data.List (isPrefixOf)

mapping = [("M",1000),("CM",900),("D",500),("CD",400),("C",100),("XC",90),
           ("L",50),("XL",40),("X",10),("IX",9),("V",5),("IV",4),("I",1)]

toArabic :: String -> Int
toArabic "" = 0
toArabic str = num + toArabic xs
    where (num, xs):_ = [ (num, drop (length n) str) | (n,num) <- mapping, isPrefixOf n str ]
```

Usage:

```txt

ghci> toArabic "MCMXC"
1990
ghci> toArabic "MMVIII"
2008
ghci> toArabic "MDCLXVI"
1666
```



### =mapAccum=

Or, expressing '''romanValue''' in terms of '''mapAccumL''' (avoiding recursive descent, and visiting each k v pair just once)

```Haskell
import Data.List (mapAccumL, isPrefixOf)
import Control.Arrow ((***))

romanValue :: String -> Int
romanValue =
  let tr s (k, v) =
        until (not . isPrefixOf k . fst) (drop (length k) *** (v +)) (s, 0)
  in sum .
     snd .
     flip
       (mapAccumL tr)
       [ ("M", 1000)
       , ("CM", 900)
       , ("D", 500)
       , ("CD", 400)
       , ("C", 100)
       , ("XC", 90)
       , ("L", 50)
       , ("XL", 40)
       , ("X", 10)
       , ("IX", 9)
       , ("V", 5)
       , ("IV", 4)
       , ("I", 1)
       ]

main :: IO ()
main =
  mapM_ (print . romanValue) ["MDCLXVI", "MCMXC", "MMVIII", "MMXVI", "MMXVII"]
```


Or, in a '''mapAccumR''' version:

```Haskell
import Data.List (mapAccumR)
import Data.Map.Strict as M
import Data.Maybe (maybe)

fromRoman :: String -> Maybe Int
fromRoman cs =
  let go l r
        | l > r = (-r, l)
        | otherwise = (r, l)
  in traverse (`M.lookup` mapRoman) cs >>=
     (Just . sum . ((:) <$> fst <*> snd) . mapAccumR go 0)

mapRoman :: Map Char Int
mapRoman = M.fromList $ zip "MDCLXVI " [1000, 500, 100, 50, 10, 5, 1, 0]

-- TEST ---------------------------------------------------
main :: IO ()
main =
  putStrLn $
  fTable
    "Decoding Roman numbers:\n"
    show
    (maybe "Unrecognised character" show)
    fromRoman
    ["MDCLXVI", "MCMXC", "MMVIII", "MMXVI", "MMXVIII", "MMXBIII"]

-- FORMATTING ---------------------------------------------
fTable :: String -> (a -> String) -> (b -> String) -> (a -> b) -> [a] -> String
fTable s xShow fxShow f xs =
  let w = maximum (length . xShow <$> xs)
      rjust n c = drop <$> length <*> (replicate n c ++)
  in unlines $
     s : fmap (((++) . rjust w ' ' . xShow) <*> ((" -> " ++) . fxShow . f)) xs
```

{{Out}}

```txt
Decoding Roman numbers:

"MDCLXVI" -> 1666
  "MCMXC" -> 1990
 "MMVIII" -> 2008
  "MMXVI" -> 2016
"MMXVIII" -> 2018
"MMXBIII" -> Unrecognised character
```



### =Fold=

An alternative solution using a fold.  (This turns out to be the fastest of the four approaches here) {{Trans|F#}}


```Haskell
import qualified Data.Map.Strict as M

fromRoman :: String -> Int
fromRoman xs = partialSum + lastDigit
  where
    (partialSum, lastDigit) = foldl accumulate (0, 0) (evalRomanDigit <$> xs)
    accumulate (partial, lastDigit) newDigit
      | newDigit <= lastDigit = (partial + lastDigit, newDigit)
      | otherwise = (partial - lastDigit, newDigit)

mapRoman :: M.Map Char Int
mapRoman =
  M.fromList
    [ ('I', 1)
    , ('V', 5)
    , ('X', 10)
    , ('L', 50)
    , ('C', 100)
    , ('D', 500)
    , ('M', 1000)
    ]

evalRomanDigit :: Char -> Int
evalRomanDigit c =
  let mInt = M.lookup c mapRoman
  in case mInt of
       Just x -> x
       _ -> error $ c : " is not a roman digit"

main :: IO ()
main = print $ fromRoman <$> ["MDCLXVI", "MCMXC", "MMVIII", "MMXVI", "MMXVII"]
```


Where the left fold above could also be rewritten [http://wiki.haskell.org/Foldr_Foldl_Foldl%27 | as a right fold].

```Haskell
import qualified Data.Map.Strict as M
import Data.Maybe (maybe)
import Data.Bool (bool)


mapRoman :: M.Map Char Int
mapRoman = M.fromList $ zip "IVXLCDM" $ scanl (*) 1 (cycle [5, 2])


fromRoman :: String -> Maybe Int
fromRoman cs =
  traverse (`M.lookup` mapRoman) cs >>=
  (Just . snd . foldr (\l (r, n) -> (l, bool (-) (+) (l >= r) n l)) (0, 0))


-- TEST ---------------------------------------------------
main :: IO ()
main =
  putStrLn $
  fTable
    "Roman numeral decoding as a right fold:\n"
    show
    (maybe "(Unrecognised character seen)" show)
    fromRoman
    ["MDCLXVI", "MCMXC", "MMVIII", "MMXVI", "MMXVII", "QQXVII"]

-- FORMATTING ---------------------------------------------

fTable :: String -> (a -> String) -> (b -> String) -> (a -> b) -> [a] -> String
fTable s xShow fxShow f xs =
  let w = maximum (length . xShow <$> xs)
      rjust n c = drop <$> length <*> (replicate n c ++)
  in unlines $
     s : fmap (((++) . rjust w ' ' . xShow) <*> ((" -> " ++) . fxShow . f)) xs
```

{{Out}}

```txt
Roman numeral decoding as a right fold:

"MDCLXVI" -> 1666
  "MCMXC" -> 1990
 "MMVIII" -> 2008
  "MMXVI" -> 2016
 "MMXVII" -> 2017
 "QQXVII" -> (Unrecognised character seen)
```



### =sum . catMaybes=


Summing a list of Map.lookup results on indexed [Char, Char] pairs.

(Probably more trouble than it's worth in practice, but at least an illustration of some Data.Maybe and Data.Map functions)


```Haskell
import qualified Data.Map.Strict as M (Map, fromList, lookup)
import Data.Maybe (isNothing, isJust, fromJust, catMaybes)
import Data.List (mapAccumL)

mapRoman :: M.Map String Int
mapRoman =
  M.fromList
    [ ("M", 1000)
    , ("CM", 900)
    , ("D", 500)
    , ("CD", 400)
    , ("C", 100)
    , ("XC", 90)
    , ("L", 50)
    , ("XL", 40)
    , ("X", 10)
    , ("IX", 9)
    , ("V", 5)
    , ("IV", 4)
    , ("I", 1)
    ]

fromRoman :: String -> Int
fromRoman s =
  let value k = M.lookup k mapRoman
  in sum . catMaybes . snd $
     mapAccumL
       (\mi (l, r, i) ->
           let mValue = value [l, r] -- mapRoman lookup of [left, right] Chars
               (lastPair, pairValue)
                 | isJust mValue = (Just i, mValue) -- Pair match: index updated
                 | isNothing mi || i - fromJust mi > 1 = (mi, value [l])
                 | otherwise = (mi, Nothing) -- Left Char was counted in pair
           in (lastPair, pairValue))
       Nothing -- Accumulator – maybe Index to last matched Char pair
       (zip3 s (tail s ++ " ") [0 ..]) -- Indexed character pairs

main :: IO ()
main = print $ fromRoman <$> ["MDCLXVI", "MCMXC", "MMVIII", "MMXVI", "MMXVII"]
```

{{Out}}

```txt
[1666,1990,2008,2016,2017]
```


=={{header|Icon}} and {{header|Unicon}}==

```Icon
link numbers

procedure main()
every R := "MCMXC"|"MDCLXVI"|"MMVIII" do
   write(R, " = ",unroman(R))
end
```

{{libheader|Icon Programming Library}}
[http://www.cs.arizona.edu/icon/library/src/procs/numbers.icn numbers.icn provides unroman]

The code for this procedure is copied below:

```Icon
procedure unroman(s)		#: convert Roman numeral to integer
   local nbr,lastVal,val

   nbr := lastVal := 0
   s ? {
      while val := case map(move(1)) of {
	 "m": 1000
	 "d": 500
	 "c": 100
	 "l": 50
	 "x": 10
	 "v": 5
	 "i": 1
	 } do {
	 nbr +:= if val <= lastVal then val else val - 2 * lastVal
	 lastVal := val
	 }
      }
   return nbr
end
```

{{out}}

```txt
MCMXC = 1990
MDCLXVI = 1666
MMVIII = 2008
```



## J


```j
rom2d=: [: (+/ .*  _1^ 0,~ 2</\ ]) 1 5 10 50 100 500 1000 {~ 'IVXLCDM'&i.
```

Example use:

```j
   rom2d 'MCMXC'
1990
   rom2d 'MDCLXVI'
1666
   rom2d 'MMVIII'
2008
```



## Java

{{works with|Java|1.5+}}

```java5
public class Roman {
	private static int decodeSingle(char letter) {
		switch(letter) {
			case 'M': return 1000;
			case 'D': return 500;
			case 'C': return 100;
			case 'L': return 50;
			case 'X': return 10;
			case 'V': return 5;
			case 'I': return 1;
			default: return 0;
		}
	}
	public static int decode(String roman) {
		int result = 0;
		String uRoman = roman.toUpperCase(); //case-insensitive
		for(int i = 0;i < uRoman.length() - 1;i++) {//loop over all but the last character
			//if this character has a lower value than the next character
			if (decodeSingle(uRoman.charAt(i)) < decodeSingle(uRoman.charAt(i+1))) {
				//subtract it
				result -= decodeSingle(uRoman.charAt(i));
			} else {
				//add it
				result += decodeSingle(uRoman.charAt(i));
			}
		}
		//decode the last character, which is always added
		result += decodeSingle(uRoman.charAt(uRoman.length()-1));
		return result;
	}

	public static void main(String[] args) {
		System.out.println(decode("MCMXC")); //1990
		System.out.println(decode("MMVIII")); //2008
		System.out.println(decode("MDCLXVI")); //1666
	}
}
```

{{out}}

```txt
1990
2008
1666
```

{{works with|Java|1.8+}}

```java5
import java.util.Set;
import java.util.EnumSet;
import java.util.Collections;
import java.util.stream.Collectors;
import java.util.stream.LongStream;

public interface RomanNumerals {
  public enum Numeral {
    M(1000), CM(900), D(500), CD(400), C(100), XC(90), L(50), XL(40), X(10), IX(9), V(5), IV(4), I(1);

    public final long weight;

    private static final Set<Numeral> SET = Collections.unmodifiableSet(EnumSet.allOf(Numeral.class));

    private Numeral(long weight) {
      this.weight = weight;
    }

    public static Numeral getLargest(long weight) {
      return SET.stream()
        .filter(numeral -> weight >= numeral.weight)
        .findFirst()
        .orElse(I)
      ;
    }
  };

  public static String encode(long n) {
    return LongStream.iterate(n, l -> l - Numeral.getLargest(l).weight)
      .limit(Numeral.values().length)
      .filter(l -> l > 0)
      .mapToObj(Numeral::getLargest)
      .map(String::valueOf)
      .collect(Collectors.joining())
    ;
  }

  public static long decode(String roman) {
    long result =  new StringBuilder(roman.toUpperCase()).reverse().chars()
      .mapToObj(c -> Character.toString((char) c))
      .map(numeral -> Enum.valueOf(Numeral.class, numeral))
      .mapToLong(numeral -> numeral.weight)
      .reduce(0, (a, b) -> a + (a <= b ? b : -b))
    ;
    if (roman.charAt(0) == roman.charAt(1)) {
      result += 2 * Enum.valueOf(Numeral.class, roman.substring(0, 1)).weight;
    }
    return result;
  }

  public static void test(long n) {
    System.out.println(n + " = " + encode(n));
    System.out.println(encode(n) + " = " + decode(encode(n)));
  }

  public static void main(String[] args) {
    LongStream.of(1999, 25, 944).forEach(RomanNumerals::test);
  }
}
```

{{out}}

```txt
1999 = MCMXCIX
MCMXCIX = 1999
25 = XXV
XXV = 25
944 = CMXLIV
CMXLIV = 944
```



## JavaScript


### ES5


### =Imperative=

{{works with|Rhino}}
{{works with|SpiderMonkey}}

```javascript
var Roman = {
  Values: [['CM', 900],  ['CD', 400], ['XC',  90], ['XL',  40], ['IV', 4],
           ['IX',   9], ['V',   5], ['X',   10], ['L',  50],
           ['C',  100], ['M', 1000], ['I',    1], ['D',  500]],
  UnmappedStr : 'Q',
  parse: function(str) {
    var result = 0
    for (var i=0; i<Roman.Values.length; ++i) {
      var pair = Roman.Values[i]
      var key = pair[0]
      var value = pair[1]
      var regex = RegExp(key)
      while (str.match(regex)) {
        result += value
        str = str.replace(regex, Roman.UnmappedStr)
      }
    }
    return result
  }
}

var test_data = ['MCMXC', 'MDCLXVI', 'MMVIII']
for (var i=0; i<test_data.length; ++i) {
  var test_datum = test_data[i]
  print(test_datum + ": " + Roman.parse(test_datum))
}
```

{{out}}

```txt
MCMXC: 1990
MDCLXVI: 1666
MMVIII: 2008

```


### =Functional=

{{Trans|Haskell}}
(isPrefixOf example)

```JavaScript
(function (lstTest) {

    var mapping = [["M", 1000], ["CM", 900], ["D", 500], ["CD", 400], ["C", 100], [
        "XC", 90], ["L", 50], ["XL", 40], ["X", 10], ["IX", 9], ["V", 5], ["IV",
        4], ["I", 1]];

    // s -> n
    function romanValue(s) {
        // recursion over list of characters
        // [c] -> n
        function toArabic(lst) {
            return lst.length ? function (xs) {
                var lstParse = chain(mapping, function (lstPair) {
                    return isPrefixOf(
                        lstPair[0], xs
                    ) ? [lstPair[1], drop(lstPair[0].length, xs)] : []
                });
                return lstParse[0] + toArabic(lstParse[1]);
            }(lst) : 0
        }
        return toArabic(s.split(''));
    }

    // Monadic bind (chain) for lists
    function chain(xs, f) {
        return [].concat.apply([], xs.map(f));
    }

    // [a] -> [a] -> Bool
    function isPrefixOf(lstFirst, lstSecond) {
        return lstFirst.length ? (
            lstSecond.length ?
            lstFirst[0] === lstSecond[0] && isPrefixOf(
                lstFirst.slice(1), lstSecond.slice(1)
            ) : false
        ) : true;
    }

    // Int -> [a] -> [a]
    function drop(n, lst) {
        return n <= 0 ? lst : (
            lst.length ? drop(n - 1, lst.slice(1)) : []
        );
    }

    return lstTest.map(romanValue);

})(['MCMXC', 'MDCLXVI', 'MMVIII']);
```

{{Out}}

```JavaScript
[1990, 1666, 2008]
```


or, more natively:

```JavaScript
(function (lstTest) {

    function romanValue(s) {
        return s.length ? function () {
            var parse = [].concat.apply([], glyphs.map(function (g) {
                return 0 === s.indexOf(g) ? [trans[g], s.substr(g.length)] : [];
            }));
            return parse[0] + romanValue(parse[1]);
        }() : 0;
    }

    var trans = {
            M: 1E3,
            CM: 900,
            D: 500,
            CD: 400,
            C: 100,
            XC: 90,
            L: 50,
            XL: 40,
            X: 10,
            IX: 9,
            V: 5,
            IV: 4,
            I: 1
        },
        glyphs = Object.keys(trans);

    return lstTest.map(romanValue);

})(["MCMXC", "MDCLXVI", "MMVIII", "MMMM"]);
```

{{Out}}

```JavaScript
[1990, 1666, 2008]
```



### ES6


### =Recursion=


```JavaScript
(() => {
    // romanValue :: String -> Int
    const romanValue = s =>
        s.length ? (() => {
            const parse = [].concat(
                ...glyphs.map(g => 0 === s.indexOf(g) ? (
                    [dctTrans[g], s.substr(g.length)]
                ) : [])
            );
            return parse[0] + romanValue(parse[1]);
        })() : 0;

    // dctTrans :: {romanKey: Integer}
    const dctTrans = {
        M: 1E3,
        CM: 900,
        D: 500,
        CD: 400,
        C: 100,
        XC: 90,
        L: 50,
        XL: 40,
        X: 10,
        IX: 9,
        V: 5,
        IV: 4,
        I: 1
    };

    // glyphs :: [romanKey]
    const glyphs = Object.keys(dctTrans);

    // TEST -------------------------------------------------------------------
    return ["MCMXC", "MDCLXVI", "MMVIII", "MMMM"].map(romanValue);
})();
```

{{Out}}

```JavaScript
[1990,1666,2008,4000]
```




### =Folding from the right=

{{Trans|Haskell}}
(fold and foldr examples)

```JavaScript
(() => {

    // Folding from right to left,
    // lower leftward characters are subtracted,
    // others are added.

    // fromRoman :: String -> Int
    const fromRoman = s =>
        snd(foldr(
            ([r, n], l) => [l, l >= r ? n + l : n - l], [0, 0],
            map(charVal, stringChars(s))
        ));

    // charVal :: Char -> Maybe Int
    const charVal = k => {
        const v = {
            I: 1,
            V: 5,
            X: 10,
            L: 50,
            C: 100,
            D: 500,
            M: 1000
        }[k];
        return v !== undefined ? v : 0;
    };

    // GENERIC FUNCTIONS ------------------------------------------------------

    // foldr (a -> b -> b) -> b -> [a] -> b
    const foldr = (f, a, xs) => xs.reduceRight(f, a);

    // map :: (a -> b) -> [a] -> [b]
    const map = (f, xs) => xs.map(f);

    // snd :: (a, b) -> b
    const snd = tpl => Array.isArray(tpl) ? tpl[1] : undefined;

    // stringChars :: String -> [Char]
    const stringChars = s => s.split('');

    // show :: a -> String
    const show = (...x) =>
        JSON.stringify.apply(
            null, x.length > 1 ? [x[1], null, x[0]] : x
        );

    // TEST -------------------------------------------------------------------
    return show(
        map(fromRoman, ["MDCLXVI", "MCMXC", "MMVIII", "MMXVI", "MMXVII"])
    );
})();
```

{{Out}}

```txt
[1666,1990,2008,2016,2017]
```



## jq

 {{works with|jq|1.4}}
This version requires the Roman numerals to be presented in upper case.

```jq
def fromRoman:
  def addRoman(n):
    if length == 0 then n
    elif startswith("M")  then .[1:] | addRoman(1000 + n)
    elif startswith("CM") then .[2:] | addRoman(900 + n)
    elif startswith("D")  then .[1:] | addRoman(500 + n)
    elif startswith("CD") then .[2:] | addRoman(400 + n)
    elif startswith("C")  then .[1:] | addRoman(100 + n)
    elif startswith("XC") then .[2:] | addRoman(90 + n)
    elif startswith("L")  then .[1:] | addRoman(50 + n)
    elif startswith("XL") then .[2:] | addRoman(40 + n)
    elif startswith("X")  then .[1:] | addRoman(10 + n)
    elif startswith("IX") then .[2:] | addRoman(9 + n)
    elif startswith("V")  then .[1:] | addRoman(5 + n)
    elif startswith("IV") then .[2:] | addRoman(4 + n)
    elif startswith("I")  then .[1:] | addRoman(1 + n)
    else
      error("invalid Roman numeral: " + tostring)
    end;
  addRoman(0);
```

'''Example:'''

```jq
[ "MCMXC", "MMVIII", "MDCLXVI" ] | map("\(.) => \(fromRoman)") | .[]
```

{{out}}

```sh
$ jq -n -f -r fromRoman.jq
MCMXC => 1990
MMVIII => 2008
MDCLXVI => 1666
```



## Jsish

Duplicate of the Jsish module used in [[Roman_numerals/Encode#Jsish]].

{{out}}

```txt
prompt$ jsish -e 'require("Roman"); puts(Roman.fromRoman("MDCLXVI"));'
1666
```



## Julia

{{works with|Julia|0.6}}
'''The Function''':

```julia
function parseroman(rnum::AbstractString)
    romandigits = Dict('I' => 1, 'V' => 5, 'X' => 10, 'L' => 50,
                       'C' => 100, 'D' => 500, 'M' => 1000)
    mval = accm = 0
    for d in reverse(uppercase(rnum))
        val = try
            romandigits[d]
        catch
            throw(DomainError())
        end
        if val > mval maxval = val end
        if val < mval
            accm -= val
        else
            accm += val
        end
    end
    return accm
end
```


This function is rather permissive.  There are no limitations on the numbers of Roman numerals nor on their order.  Because of this and because any out of order numerals subtract from the total represented, it is possible to represent zero and negative integers.  Also mixed case representations are allowed.  The function does throw an error if the string contains any invalid characters.

'''Test the code''':

```julia
test = ["I", "III", "IX", "IVI", "IIM",
        "CMMDXL", "icv", "cDxLiV", "MCMLD", "ccccccd",
        "iiiiiv", "MMXV", "MCMLXXXIV", "ivxmm", "SPQR"]
for rnum in test
    @printf("%15s → %s\n", rnum, try parseroman(rnum) catch "not valid" end)
end
```


{{out}}

```txt
              I → 1
            III → 3
             IX → 11
            IVI → 7
            IIM → 1002
         CMMDXL → 2660
            icv → 106
         cDxLiV → 666
          MCMLD → 2650
        ccccccd → 1100
         iiiiiv → 10
           MMXV → 2015
      MCMLXXXIV → 2186
          ivxmm → 2016
           SPQR → not valid
```



## K

{{trans|J}}

```k
  romd: {v:1 5 10 50 100 500 1000@"IVXLCDM"?/:x; +/v*_-1^(>':v),0}
```

'''Example:'''

```k
  romd'("MCMXC";"MMVIII";"MDCLXVI")
1990 2008 1666
```



## Kotlin

As specified in the task description, there is no attempt to validate the form of the Roman number in the following program - invalid characters and ordering are simply ignored:

```scala
// version 1.0.6

fun romanDecode(roman: String): Int {
    if (roman.isEmpty()) return 0
    var n = 0
    var last = 'O'
    for (c in roman) {
        when (c) {
            'I' -> n += 1
            'V' -> if (last == 'I') n += 3   else n += 5
            'X' -> if (last == 'I') n += 8   else n += 10
            'L' -> if (last == 'X') n += 30  else n += 50
            'C' -> if (last == 'X') n += 80  else n += 100
            'D' -> if (last == 'C') n += 300 else n += 500
            'M' -> if (last == 'C') n += 800 else n += 1000
        }
        last = c
    }
    return n
}

fun main(args: Array<String>) {
    val romans = arrayOf("I", "III", "IV", "VIII", "XLIX", "CCII", "CDXXXIII", "MCMXC", "MMVIII", "MDCLXVI")
    for (roman in romans) println("${roman.padEnd(10)} = ${romanDecode(roman)}")
}
```


{{out}}

```txt

I          = 1
III        = 3
IV         = 4
VIII       = 8
XLIX       = 49
CCII       = 202
CDXXXIII   = 433
MCMXC      = 1990
MMVIII     = 2008
MDCLXVI    = 1666

```



## Lasso


```Lasso>define br =
 '\r'
//decode roman
define decodeRoman(roman::string)::integer => {
	local(ref = array('M'=1000, 'CM'=900, 'D'=500, 'CD'=400, 'C'=100, 'XC'=90, 'L'=50, 'XL'=40, 'X'=10, 'IX'=9, 'V'=5, 'IV'=4, 'I'=1))
	local(out = integer)
	while(#roman->size) => {
		// need to use neset while instead of query expr to utilize loop_abort
		while(loop_count <= #ref->size) => {
			if(#roman->beginswith(#ref->get(loop_count)->first)) => {
				#out += #ref->get(loop_count)->second
				#roman->remove(1,#ref->get(loop_count)->first->size)
				loop_abort
			}
		}
	}
	return #out
}

'MCMXC as integer is '+decodeRoman('MCMXC')
br
'MMVIII as integer is '+decodeRoman('MMVIII')
br
'MDCLXVI as integer is '+decodeRoman('MDCLXVI')
```



## Liberty BASIC

As Fortran & PureBasic.

```lb
  print "MCMXCIX = "; romanDec( "MCMXCIX") '1999
  print "MDCLXVI = "; romanDec( "MDCLXVI") '1666
  print "XXV     = "; romanDec( "XXV")     '25
  print "CMLIV   = "; romanDec( "CMLIV")   '954
  print "MMXI    = "; romanDec( "MMXI")    '2011

  end

function romanDec( roman$)
  arabic  =0
  lastval =0

  for i = len( roman$) to 1 step -1
    select case upper$( mid$( roman$, i, 1))
      case "M"
        n = 1000
      case "D"
        n = 500
      case "C"
        n = 100
      case "L"
        n = 50
      case "X"
        n = 10
      case "V"
        n = 5
      case "I"
        n = 1
      case else
        n = 0
    end select

    if n <lastval then
      arabic =arabic -n
    else
      arabic =arabic +n
    end if

    lastval =n
  next

  romanDec =arabic
end function
```


```txt

MCMXCIX = 1999
MDCLXVI = 1666
XXV     = 25
CMLIV   = 954
MMXI    = 2011

```



## LiveScript



```livescript
require! 'prelude-ls': {fold, sum}

# String → Number
decimal_of_roman = do
  # [Number, Number] → String → [Number, Number]
  _convert = ([acc, last_value], ch) ->
    current_value = { M:1000 D:500 C:100 L:50 X:10 V:5 I:1 }[ch] ? 0
    op = if last_value < current_value then (-) else (+)
    [op(acc, last_value), current_value]
  # fold the string and sum the resulting tuple (array)
  fold(_convert, [0, 0]) >> sum

{[rom, decimal_of_roman rom] for rom in <[ MCMXC MMVII MDCLXVII MMMCLIX MCMLXXVII MMX ]>}
```


Output:

```txt
{"MCMXC":1990,"MMVII":2007,"MDCLXVII":1667,"MMMCLIX":3159,"MCMLXXVII":1977,"MMX":2010}
```



## Logo


```logo
; Roman numeral decoder

; First, some useful substring utilities
to starts_with? :string :prefix
  if empty? :prefix [output "true]
  if empty? :string [output "false]
  if not equal? first :string first :prefix [output "false]
  output starts_with? butfirst :string butfirst :prefix
end

to remove_prefix :string :prefix
  if or empty? :prefix not starts_with? :string :prefix [output :string]
  output remove_prefix butfirst :string butfirst :prefix
end

; Our list of Roman numeral values
make "values [[M 1000] [CM 900] [D  500] [CD 400] [C  100] [XC 90] [L 50]
              [XL 40]  [X  10]  [IX 9]   [V  5]   [IV 4]   [I  1]]

; Function to do the work
to from_roman :str
 local "n make "n 0
 foreach :values [
   local "s make "s first ?
   local "v make "v last ?
   while [starts_with? :str :s] [
     make "n sum n :v
     make "str remove_prefix :str :s
   ]
 ]
 output :n
end

foreach [MCMXC MDCLXVI MMVIII] [print (sentence (word ? "|: |) from_roman ?)]
bye
```

{{out}}

```txt
MCMXC:  1990
MDCLXVI:  1666
MMVIII:  2008

```



## Lua


```lua
function ToNumeral( roman )
    local Num = { ["M"] = 1000, ["D"] = 500, ["C"] = 100, ["L"] = 50, ["X"] = 10, ["V"] = 5, ["I"] = 1 }
    local numeral = 0

    local i = 1
    local strlen = string.len(roman)
    while i < strlen do
        local z1, z2 = Num[ string.sub(roman,i,i) ], Num[ string.sub(roman,i+1,i+1) ]
        if z1 < z2 then
            numeral = numeral + ( z2 - z1 )
            i = i + 2
        else
            numeral = numeral + z1
            i = i + 1
        end
    end

    if i <= strlen then numeral = numeral + Num[ string.sub(roman,i,i) ] end

    return numeral
end

print( ToNumeral( "MCMXC" ) )
print( ToNumeral( "MMVIII" ) )
print( ToNumeral( "MDCLXVI" ) )
```


```txt
1990
2008
1666
```



## M2000 Interpreter

Maximum Roman number is MMMCMXCIX (3999)


```M2000 Interpreter

Module RomanNumbers {
      flush  ' empty current stack
      gosub Initialize
      document Doc$
      while not empty
            read rom$
            print rom$;"=";RomanEval$(rom$)
            Doc$=rom$+"="+RomanEval$(rom$)+{
            }
      end while
      Clipboard Doc$
      end
Initialize:
      function RomanEval$(rom$) {
            Flush
            ="invalid"
            if filter$(rom$,"MDCLXVI")<>"" Then Exit
            \\ "Y" is in top of stack
            Push "CM", "MD", "Q"
            Push "CD", "MD","W"
            Push "XC", "DL", "E"
            Push "XL", "X","R"
            Push "IX","V","T"
            Push  "IV","I","Y"
            \\ stack flush to doublerom
            doublerom=[]
            \\  "M" is in top of stack
            Data "M", 1000, "Q",900
            Data "D", 500,"W", 400
            Data "C",100,"E",90
            Data "L",50,"R", 40
            Data "X", 10, "T", 9
            Data "V", 5, "Y", 4, "I",1
            \\ stack flush to singlerom
            singlerom=[]
            acc=0
            value=0
            count=0
            stack doublerom {
                  if empty then exit
                  read rep$,exclude$,cc$
                  i=instr(rom$,cc$)
                  if i >0 then
                        tmp$=mid$(rom$,i+2)
                        L=Len(tmp$)
                        if L>0 then if Len(filter$(tmp$, exclude$))<>L then rom$="A": exit
                        if Instr(rom$,mid$(rom$,i,1))<i then rom$="A": exit
                        insert i,2 rom$=rep$  ' replace at pos i with rep$ and place a space to i+1
                  end if
                  loop
            }
            rom$=filter$(rom$," ") ' remove spaces if exist

            stack singlerom {
                  if empty then exit
                  read cc$, value
                  count=0
                  while left$(rom$,1)=cc$
                         insert 1, 1 rom$=""
                         count++
                         acc+=value
                  end while
                  if count>3 then exit
                  loop
            }
            if len(rom$)>0  or count>3 Else
                  =Str$(acc,1033)
            end if
      }
      data "MMMCMXCIX", "LXXIIX", "MMXVII", "LXXIX", "CXCIX","MCMXCIX","MMMDCCCLXXXVIII"
      data "CMXI","M","MCDXLIV","CCCC","IXV", "XLIXL","LXXIIX","IVM"
      data "XXXIX", "XXXX", "XIXX","IVI", "XLIX","XCIX","XCIV","XLVIII"
      return
}
RomanNumbers


```


{{out}}

```txt
MMMCMXCIX=3999
LXXIIX=invalid
MMXVII=2017
LXXIX=79
CXCIX=199
MCMXCIX=1999
MMMDCCCLXXXVIII=3888
CMXI=911
M=1000
MCDXLIV=1444
CCCC=invalid
IXV=invalid
XLIXL=invalid
LXXIIX=invalid
IVM=invalid
XXXIX=39
XXXX=invalid
XIXX=invalid
IVI=invalid
XLIX=49
XCIX=99
XCIV=94
XLVIII=48
```


## Maple


```maple
f := n -> convert(n, arabic):
seq(printf("%a\n", f(i)), i in [MCMXC, MMVIII, MDCLXVI]);
```

{{out}}

```txt
1990
2008
1666
```



## Mathematica


```Mathematica
FromRomanNumeral["MMCDV"]
```

returns 2405


## MATLAB


```Matlab
function x = rom2dec(s)
% ROM2DEC converts Roman numbers to decimal

% store Roman digits values: I=1, V=5, X=10, L=50, C=100, D=500, M=1000
digitsValues = [0 0 100 500 0 0 0 0 1 0 0 50 1000 0 0 0 0 0 0 0 0 5 0 10 0 0];
% convert Roman number to array of values
values = digitsValues(s-'A'+1);
% change sign if next value is bigger
x = sum(values .* [sign(diff(-values)+eps),1]);

end
```

Here is a test:

```Matlab
romanNumbers = {'MMMCMXCIX', 'XLVIII', 'MMVIII'};
for n = 1 : numel(romanNumbers)
  fprintf('%10s = %4d\n',romanNumbers{n}, rom2dec(romanNumbers{n}));
end
```

{{out}}

```txt

 MMMCMXCIX = 3999
    XLVIII =   48
    MMVIII = 2008

```




## Mercury


```Mercury
:- module test_roman.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module char.
:- import_module exception.
:- import_module int.
:- import_module list.
:- import_module string.

:- type conversion_error --->  not_a_roman_number.

:- func build_int(list(char), int, int) = int.
:- func from_roman(string) = int.
:- pred roman_to_int(char::in, int::out) is semidet.

from_roman(Roman) = Decimal :-
  List = reverse(to_char_list(Roman)),
  Decimal = build_int(List, 0, 0).

build_int([], LastValue, Accumulator) = LastValue + Accumulator.
build_int([Digit|Rest], LastValue, Accumulator) = Sum :-
  ( roman_to_int(Digit, Value) ->
      ( Value < LastValue ->
          Sum = build_int(Rest, Value, Accumulator - LastValue)
      ;   Sum = build_int(Rest, Value, Accumulator + LastValue) )
  ;   throw(not_a_roman_number) ).

roman_to_int('I', 1).
roman_to_int('V', 5).
roman_to_int('X', 10).
roman_to_int('L', 50).
roman_to_int('C', 100).
roman_to_int('D', 500).
roman_to_int('M', 1000).

main(!IO) :-
    command_line_arguments(Args, !IO),
    foldl((pred(Arg::in, !.IO::di, !:IO::uo) is det :-
              format("%s => %d\n", [s(Arg), i(from_roman(Arg))], !IO)),
          Args, !IO).

:- end_module test_roman.
```



## NetRexx


```NetRexx
/* NetRexx */
options replace format comments java crossref savelog symbols binary

           /* 1990  2008   1666    */
years = Rexx('MCMXC MMVIII MDCLXVI')

loop y_ = 1 to years.words
    Say years.word(y_).right(10) || ':' decode(years.word(y_))
    end y_

return

method decode(arg) public static returns int signals IllegalArgumentException

  parse arg.upper roman .
  if roman.verify('MDCLXVI') \= 0 then signal IllegalArgumentException

  -- always insert the value of the least significant numeral
  decnum = rchar(roman.substr(roman.length, 1))
  loop d_ = 1 to roman.length - 1
    if rchar(roman.substr(d_, 1)) < rchar(roman.substr(d_ + 1, 1)) then do
      -- Handle cases where numerals are not in descending order
      --   subtract the value of the numeral
      decnum = decnum - rchar(roman.substr(d_, 1))
      end
    else do
      -- Normal case
      --   add the value of the numeral
      decnum = decnum + rchar(roman.substr(d_, 1))
      end
    end d_

  return decnum

method rchar(arg) public static returns int

  parse arg.upper ch +1 .
  select case ch
    when 'M' then digit = 1000
    when 'D' then digit =  500
    when 'C' then digit =  100
    when 'L' then digit =   50
    when 'X' then digit =   10
    when 'V' then digit =    5
    when 'I' then digit =    1
    otherwise     digit =    0
    end

  return digit
```

{{out}}

```txt

     MCMXC: 1990
    MMVIII: 2008
   MDCLXVI: 1666

```



## Nim

{{trans|Python}}

```nim
import tables

let rdecode = {'M': 1000, 'D': 500, 'C': 100, 'L': 50, 'X': 10, 'V': 5, 'I': 1}.toTable

proc decode(roman): int =
  for i in 0 .. <roman.high:
    let (rd, rd1) = (rdecode[roman[i]], rdecode[roman[i+1]])
    result += (if rd < rd1: -rd else: rd)
  result += rdecode[roman[roman.high]]

for r in ["MCMXC", "MMVIII", "MDCLXVI"]:
  echo r, " ", decode(r)
```



## OCaml


```ocaml
let decimal_of_roman roman =
  let arabic = ref 0 in
  let lastval = ref 0 in
  for i = (String.length roman) - 1 downto 0 do
    let n =
      match roman.[i] with
      | 'M' | 'm' -> 1000
      | 'D' | 'd' -> 500
      | 'C' | 'c' -> 100
      | 'L' | 'l' -> 50
      | 'X' | 'x' -> 10
      | 'V' | 'v' -> 5
      | 'I' | 'i' -> 1
      | _ -> 0
    in
    if n < !lastval
    then arabic := !arabic - n
    else arabic := !arabic + n;
    lastval := n
  done;
  !arabic

let () =
  Printf.printf " %d\n" (decimal_of_roman "MCMXC");
  Printf.printf " %d\n" (decimal_of_roman "MMVIII");
  Printf.printf " %d\n" (decimal_of_roman "MDCLXVI");
;;
```


###  Another implementation

Another implementation, a bit more OCaml-esque: no mutable variables, and a recursive function instead of a for loop.
{{works with|OCaml|4.03+}}

```ocaml

(* Scan the roman number from right to left. *)
(* When processing a roman digit, if the previously processed roman digit was
 * greater than the current one, we must substract the latter from the current
 * total, otherwise add it.
 * Example:
 * - MCMLXX read from right to left is XXLMCM
 *          the sum is 10 + 10 + 50 + 1000 - 100 + 1000 *)
let decimal_of_roman roman =
  (* Use 'String.uppercase' for OCaml 4.02 and previous. *)
  let rom = String.uppercase_ascii roman in
  (* A simple association list. IMHO a Hashtbl is a bit overkill here. *)
  let romans = List.combine ['I'; 'V'; 'X'; 'L'; 'C'; 'D'; 'M']
                            [1; 5; 10; 50; 100; 500; 1000] in
  let compare x y =
    if x < y then -1 else 1
  in
  (* Scan the string from right to left using index i, and keeping track of
   * the previously processed roman digit in prevdig. *)
  let rec doloop i prevdig =
    if i < 0 then 0
    else
      try
        let currdig = List.assoc rom.[i] romans in
        (currdig * compare currdig prevdig) + doloop (i - 1) currdig
      with
        (* Ignore any incorrect roman digit and just process the next one. *)
        Not_found -> doloop (i - 1) 0
  in
  doloop (String.length rom - 1) 0


(* Some simple tests. *)
let () =
  let testit roman decimal =
    let conv = decimal_of_roman roman in
    let status = if conv = decimal then "PASS" else "FAIL" in
    Printf.sprintf "[%s] %s\tgives %d.\tExpected: %d.\t"
                   status roman conv decimal
  in
  print_endline ">>> Usual roman numbers.";
  print_endline (testit "MCMXC" 1990);
  print_endline (testit "MMVIII" 2008);
  print_endline (testit "MDCLXVI" 1666);
  print_newline ();

  print_endline ">>> Roman numbers with lower case letters are OK.";
  print_endline (testit "McmXC" 1990);
  print_endline (testit "MMviii" 2008);
  print_endline (testit "mdCLXVI" 1666);
  print_newline ();

  print_endline ">>> Incorrect roman digits are ignored.";
  print_endline (testit "McmFFXC" 1990);
  print_endline (testit "MMviiiPPPPP" 2008);
  print_endline (testit "mdCLXVI_WHAT_NOW" 1666);
  print_endline (testit "2 * PI ^ 2" 1);  (* The I in PI... *)
  print_endline (testit "E = MC^2" 1100)

```

Output:

```txt

>>> Usual roman numbers.
[PASS] MCMXC	gives 1990.	Expected: 1990.
[PASS] MMVIII	gives 2008.	Expected: 2008.
[PASS] MDCLXVI	gives 1666.	Expected: 1666.

>>> Roman numbers with lower case letters are OK.
[PASS] McmXC	gives 1990.	Expected: 1990.
[PASS] MMviii	gives 2008.	Expected: 2008.
[PASS] mdCLXVI	gives 1666.	Expected: 1666.

>>> Incorrect roman digits are ignored.
[PASS] McmFFXC	gives 1990.	Expected: 1990.
[PASS] MMviiiPPPPP	gives 2008.	Expected: 2008.
[PASS] mdCLXVI_WHAT_NOW	gives 1666.	Expected: 1666.
[PASS] 2 * PI ^ 2	gives 1.	Expected: 1.
[PASS] E = MC^2	gives 1100.	Expected: 1100.

```



## PARI/GP


```parigp
fromRoman(s)={
  my(v=Vecsmall(s),key=vector(88),cur,t=0,tmp);
  key[73]=1;key[86]=5;key[88]=10;key[76]=50;key[67]=100;key[68]=500;key[77]=1000;
  cur=key[v[1]];
  for(i=2,#v,
    tmp=key[v[i]];
    if(!cur, cur=tmp; next);
    if(tmp>cur,
      t+=tmp-cur;
      cur=0
    ,
      t+=cur;
      cur=tmp
    )
  );
  t+cur
};
```



## Perl


```Perl
use 5.10.0;

{
 my @trans = (
                [M  => 1000],     [CM => 900],
                [D  => 500],      [CD => 400],
                [C  => 100],      [XC => 90],
                [L  => 50],       [XL => 40],
                [X  => 10],       [IX => 9],
                [V  => 5],        [IV => 4],
                [I  => 1],
 );

  sub from_roman {
        my $r = shift;
        my $n = 0;
        foreach my $pair (@trans) {
          my ($k, $v) = @$pair;
          $n += $v while $r =~ s/^$k//i;
        }
        return $n
  }
}

say "$_: ", from_roman($_) for qw(MCMXC MDCLXVI MMVIII);
```

{{out}}

```txt
MCMXC: 1990
MDCLXVI: 1666
MMVIII: 2008
```


###  Alternate


```perl
#!/usr/bin/perl

use strict;
use warnings;

sub roman2decimal
  {
  (local $_, my $sum, my $zeros) = (shift, 0, '');
  $zeros .= 0 while
    $sum -= s/I(?=[VX])// - s/V// * 5 - s/I//g . $zeros,
    tr/MDCLX/CLXVI/;
  return $sum;
  }

print s/$/ ": " . roman2decimal($_) /er while <DATA>;

__DATA__
MCMXC
MMVIII
MDCLXVI
```

{{out}}

```txt

MCMXC: 1990
MMVIII: 2008
MDCLXVI: 1666

```


###  Another Alternate


```perl
#!/usr/bin/perl

use strict;
use warnings;

sub roman2decimal
  {
  my $sum = 0;
  $sum += $^R while $_[0] =~
    / M  (?{1000})
    | D  (?{ 500})
    | C  (?{ 100}) (?= [MD] (?{-100}) )?
    | L  (?{  50})
    | X  (?{  10}) (?= [CL] (?{ -10}) )?
    | V  (?{   5})
    | I  (?{   1}) (?= [XV] (?{  -1}) )?
    /gx;
  return $sum;
  }

print s/$/ ": " . roman2decimal($_) /er while <DATA>;

__DATA__
MCMXC
MMVIII
MDCLXVI
```

{{out}}

```txt

MCMXC: 1990
MMVIII: 2008
MDCLXVI: 1666

```



## Perl 6

A non-validating version:

```perl6
sub rom-to-num($r) {
    [+] gather $r.uc ~~ /
        ^
        [
        | M  { take 1000 }
        | CM { take 900 }
        | D  { take 500 }
        | CD { take 400 }
        | C  { take 100 }
        | XC { take 90 }
        | L  { take 50 }
        | XL { take 40 }
        | X  { take 10 }
        | IX { take 9 }
        | V  { take 5 }
        | IV { take 4 }
        | I  { take 1 }
        ]+
        $
    /;
}

say "$_ => &rom-to-num($_)" for <MCMXC MDCLXVI MMVIII>;
```

{{out}}

```txt
MCMXC => 1990
MDCLXVI => 1666
MMVIII => 2008
```

A validating version.  Also handles older forms such as 'IIXX' and "IIII".

```perl6
sub rom-to-num($r) {
    [+] gather $r.uc ~~ /
        ^
        ( (C*)M { take 1000 - 100 * $0.chars } )*
        ( (C*)D { take  500 - 100 * $0.chars } )?
        ( (X*)C { take  100 -  10 * $0.chars } )*
        ( (X*)L { take   50 -  10 * $0.chars } )?
        ( (I*)X { take   10 -       $0.chars } )*
        ( (I*)V { take    5 -       $0.chars } )?
        (     I { take    1                  } )*
        [ $ || { return NaN } ]
    /;
}

say "$_ => ", rom-to-num($_) for <MCMXC mdclxvi MMViii IIXX ILL>;
```

{{out}}

```txt
MCMXC => 1990
mdclxvi => 1666
MMViii => 2008
IIXX => 18
ILL => NaN
```



## Phix


```Phix
constant romans = "MDCLXVI",
         decmls = {1000,500,100,50,10,5,1}

function romanDec(string s)
integer n, prev = 0, res = 0
    for i=length(s) to 1 by -1 do
        n = decmls[find(s[i],romans)]
        if n<prev then n = 0-n end if
        res += n
        prev = n
    end for
    return res
end function
```



## PicoLisp


```PicoLisp
(de roman2decimal (Rom)
   (let L (replace (chop Rom) 'M 1000 'D 500 'C 100 'L 50 'X 10 'V 5 'I 1)
      (sum '((A B) (if (>= A B) A (- A))) L (cdr L)) ) )
```

Test:

```txt
: (roman2decimal "MCMXC")
-> 1990

: (roman2decimal "MMVIII")
-> 2008

: (roman2decimal "MDCLXVI")
-> 1666
```



## PHP


```PHP
<?php
/**
 * @author Elad Yosifon
 */
$roman_to_decimal = array(
	'I' => 1,
	'V' => 5,
	'X' => 10,
	'L' => 50,
	'C' => 100,
	'D' => 500,
	'M' => 1000,
);

/**
 * @param $number
 * @return int
 */
function roman2decimal($number)
{
	global $roman_to_decimal;

	// breaks the string into an array of chars
	$digits = str_split($number);
	$lastIndex = count($digits)-1;
	$sum = 0;

	foreach($digits as $index => $digit)
	{
		if(!isset($digits[$index]))
		{
			continue;
		}

		if(isset($roman_to_decimal[$digit]))
		{
			if($index < $lastIndex)
			{
				$left = $roman_to_decimal[$digits[$index]];
				$right = $roman_to_decimal[$digits[$index+1]];
				if($left < $right)
				{
					$sum += ($right - $left);
					unset($digits[$index+1],$left, $right);
					continue;
				}
				unset($left, $right);
			}
		}
		$sum += $roman_to_decimal[$digit];
	}

	return $sum;
}

/*
### ========== OUTPUT ==========
*/
header('Content-Type: text/plain');

$tests = array(
	"I" => array(roman2decimal('I'), 1),
	"II" => array(roman2decimal('II'), 2),
	"III" => array(roman2decimal('III'), 3),
	"IV" => array(roman2decimal('IV'), 4),
	"V" => array(roman2decimal('V'), 5),
	"VI" => array(roman2decimal('VI'), 6),
	"VII" => array(roman2decimal('VII'), 7),
	"IX" => array(roman2decimal('IX'), 9),
	"X" => array(roman2decimal('X'), 10),
	"XI" => array(roman2decimal('XI'), 11),
	"XIV" => array(roman2decimal('XIV'), 14),
	"XV" => array(roman2decimal('XV'), 15),
	"XVI" => array(roman2decimal('XVI'), 16),
	"XVIV" => array(roman2decimal('XVIV'), 19),
	"XIX" => array(roman2decimal('XIX'), 19),
	"MDCLXVI" => array(roman2decimal('MDCLXVI'), 1666),
	"MCMXC" => array(roman2decimal('MCMXC'), 1990),
	"MMVIII" => array(roman2decimal('MMVIII'), 2008),
	"MMMCLIX" => array(roman2decimal('MMMCLIX'), 3159),
	"MCMLXXVII" => array(roman2decimal('MCMLXXVII'), 1977),
);


foreach($tests as $key => $value)
{
	echo "($key == {$value[0]}) => " . ($value[0] === $value[1] ? "true" : "false, should be {$value[1]}.") . "\n";
}
```

{{out}}

```txt

(I == 1) => true
(II == 2) => true
(III == 3) => true
(IV == 4) => true
(V == 5) => true
(VI == 6) => true
(VII == 7) => true
(IX == 9) => true
(X == 10) => true
(XI == 11) => true
(XIV == 14) => true
(XV == 15) => true
(XVI == 16) => true
(XVIV == 19) => true
(XIX == 19) => true
(MDCLXVI == 1666) => true
(MCMXC == 1990) => true
(MMVIII == 2008) => true
(MMMCLIX == 3159) => true
(MCMLXXVII == 1977) => true

```



## PL/I


```PL/I

test_decode: procedure options (main); /* 28 January 2013 */
   declare roman character (20) varying;

   do roman = 'i', 'ii', 'iii', 'iv', 'v', 'vi', 'vii', 'viii', 'iix',
              'ix', 'x', 'xi', 'xiv', 'MCMLXIV', 'MCMXC', 'MDCLXVI',
              'MIM', 'MM', 'MMXIII';
      put skip list (roman, decode(roman));
   end;

decode: procedure (roman) returns (fixed(15));
   declare roman character (*) varying;
   declare (current, previous) character (1);
   declare n fixed (15);
   declare i fixed binary;

   previous = ''; n = 0;
   do i = length(roman) to 1 by -1;
      current = substr(roman, i, 1);
      if digit_value(current) < digit_value(previous) then
         n = n - digit_value(current);
      else if digit_value(current) > digit_value(previous) then
         do;
            n = n + digit_value(current);
            previous = current;
         end;
      else
         n = n + digit_value(current);
   end;
   return (n);
end decode;

digit_value: procedure (roman_char) returns (fixed);
   declare roman_char character(1);
      select (roman_char);
         when ('M', 'm') return (1000);
         when ('D', 'd') return (500);
         when ('C', 'c') return (100);
         when ('L', 'l') return (50);
         when ('X', 'x') return (10);
         when ('V', 'v') return (5);
         when ('I', 'i') return (1);
         otherwise       return (0);
      end;
end digit_value;

end test_decode;

```


```txt

i                                        1
ii                                       2
iii                                      3
iv                                       4
v                                        5
vi                                       6
vii                                      7
viii                                     8
iix                                      8
ix                                       9
x                                       10
xi                                      11
xiv                                     14
MCMLXIV                               1964
MCMXC                                 1990
MDCLXVI                               1666
MIM                                   1999
MM                                    2000
MMXIII                                2013

```



## PL/SQL



```PL/SQL

/*****************************************************************
 * $Author: Atanas Kebedjiev $
 *****************************************************************
 * PL/SQL code can be run as anonymous block.
 * To test, execute the whole script or create the functions and then e.g. 'select rdecode('2012') from dual;
 * Please note that task definition does not describe fully some current rules, such as
 * * subtraction - IX XC CM are the valid subtraction combinations
 * * A subtraction character cannot be repeated: 8 is expressed as VIII and not as IIX
 * * V L and D cannot be used for subtraction
 * * Any numeral cannot be repeated more than 3 times: 1910 should be MCMX and not MDCCCCX
 * Code below does not validate the Roman numeral itself and will return a result even for a non-compliant number
 * E.g. both MCMXCIX and IMM will return 1999 but the first one is the correct notation
 */

DECLARE

FUNCTION rvalue(c IN CHAR) RETURN NUMBER IS
    i INTEGER;
BEGIN
    i := 0;
    CASE (c)
        when 'M' THEN i := 1000;
        when 'D' THEN i := 500;
        when 'C' THEN i := 100;
        when 'L' THEN i := 50;
        when 'X' THEN i := 10;
        when 'V' THEN i := 5;
        when 'I' THEN i := 1;
    END CASE;
    RETURN i;
END;


FUNCTION decode(rn IN VARCHAR2) RETURN NUMBER IS
   i  INTEGER;
   l  INTEGER;
   cr CHAR;   -- current Roman numeral as substring from r
   cv INTEGER; -- value of current Roman numeral

   gr CHAR;   -- next Roman numeral
   gv NUMBER; --  value of the next numeral;

   dv NUMBER; -- decimal value to return
BEGIN
           l := length(rn);
           i := 1;
           dv := 0;
           while (i <= l)
           LOOP
                cr := substr(rn,i,1);
                cv := rvalue(cr);

   /* Look for a larger numeral in next position, like IV or CM
      The number to subtract should be at least 1/10th of the bigger number
      CM and XC are valid, but IC and XM are not */
                IF (i < l) THEN
                   gr := substr(rn,i+1,1);
                   gv := rvalue(gr);
                   IF (cv < gv ) THEN
                      dv := dv - cv;
                   ELSE
                      dv := dv + cv;
                   END IF;
                ELSE
                   dv := dv + cv;
                END IF;  -- need to add the last value unconditionally

                i := i + 1;
            END LOOP;

RETURN dv;

END;

BEGIN

    DBMS_OUTPUT.PUT_LINE ('MMXII      = ' || rdecode('MMXII'));       -- 2012
    DBMS_OUTPUT.PUT_LINE ('MCMLI      = ' || rdecode('MCMLI'));       -- 1951
    DBMS_OUTPUT.PUT_LINE ('MCMLXXXVII = ' || rdecode('MCMLXXXVII'));  -- 1987
    DBMS_OUTPUT.PUT_LINE ('MDCLXVI    = ' || rdecode('MDCLXVI'));     -- 1666
    DBMS_OUTPUT.PUT_LINE ('MCMXCIX    = ' || rdecode('MCMXCIX'));     -- 1999

END;

```



## PowerShell


```PowerShell

function ConvertFrom-RomanNumeral
{
  <#
    .SYNOPSIS
        Converts a roman numeral to a number.
    .DESCRIPTION
        Converts a roman numeral - in the range of I..MMMCMXCIX - to a number.
    .PARAMETER Numeral
        A roman numeral in the range I..MMMCMXCIX (1..3,999).
    .INPUTS
        System.String
    .OUTPUTS
        System.Int32
    .NOTES
        Requires PowerShell version 3.0
    .EXAMPLE
        ConvertFrom-RomanNumeral -Numeral MMXIV
    .EXAMPLE
        "MMXIV" | ConvertFrom-RomanNumeral
  #>
    [CmdletBinding()]
    [OutputType([int])]
    Param
    (
        [Parameter(Mandatory=$true,
                   HelpMessage="Enter a roman numeral in the range I..MMMCMXCIX",
                   ValueFromPipeline=$true,
                   Position=0)]
        [ValidatePattern("(?x)^
                         M{0,3}  # Thousands
                (CM|CD|D?C{0,3}) # Hundreds
                (XC|XL|L?X{0,3}) # Tens
                (IX|IV|V?I{0,3}) # Ones
                $")]
        [string]
        $Numeral
    )

    Begin
    {
        # This must be an [ordered] hashtable
        $RomanToDecimal = [ordered]@{
            M  = 1000
            CM =  900
            D  =  500
            CD =  400
            C  =  100
            XC =   90
            L  =   50
            XL =   40
            X  =   10
            IX =    9
            V  =    5
            IV =    4
            I  =    1
        }
    }
    Process
    {
        $roman = $Numeral + '$'
        $value = 0

        do
        {
            foreach ($key in $RomanToDecimal.Keys)
            {
                if ($key.Length -eq 1)
                {
                    if ($key -match $roman.Substring(0,1))
                    {
                        $value += $RomanToDecimal.$key
                        $roman  = $roman.Substring(1)
                        break
                    }
                }
                else
                {
                    if ($key -match $roman.Substring(0,2))
                    {
                        $value += $RomanToDecimal.$key
                        $roman  = $roman.Substring(2)
                        break
                    }
                }
            }
        }
        until ($roman -eq '$')

        $value
    }
}

```


```PowerShell

-split "MM MMI MMII MMIII MMIV MMV MMVI MMVII MMVIII MMIX MMX MMXI MMXII MMXIII MMXIV MMXV MMXVI" | ConvertFrom-RomanNumeral

```

{{Out}}

```txt

2000
2001
2002
2003
2004
2005
2006
2007
2008
2009
2010
2011
2012
2013
2014
2015
2016

```



## Prolog


```Prolog
decode_digit(i, 1).
decode_digit(v, 5).
decode_digit(x, 10).
decode_digit(l, 50).
decode_digit(c, 100).
decode_digit(d, 500).
decode_digit(m, 1000).

decode_string(Sum, _, [], Sum).

decode_string(LastSum, LastValue, [Digit|Rest], NextSum) :-
   decode_digit(Digit, Value),
   Value < LastValue,
   Sum is LastSum - Value,
   decode_string(Sum, Value, Rest, NextSum).

decode_string(LastSum, LastValue, [Digit|Rest], NextSum) :-
   decode_digit(Digit, Value),
   Value >= LastValue,
   Sum is LastSum + Value,
   decode_string(Sum, Value, Rest, NextSum).

decode_string(Atom, Value) :-
   atom_chars(Atom, String),
   reverse(String, [Last|Rest]),
   decode_digit(Last, Start),
   decode_string(Start, Start, Rest, Value).

test :-
   decode_string(mcmxc, 1990),
   decode_string(mmviii, 2008),
   decode_string(mdclxvi, 1666).
```

The program above contains its own test predicate.
The respective goal succeeds.
Therefore the test passes.


## PureBasic


```PureBasic
Procedure romanDec(roman.s)
  Protected i, n, lastval, arabic

  For i = Len(roman) To 1 Step -1
    Select UCase(Mid(roman, i, 1))
      Case "M"
        n = 1000
      Case "D"
        n = 500
      Case "C"
        n = 100
      Case "L"
        n = 50
      Case "X"
        n = 10
      Case "V"
        n = 5
      Case "I"
        n = 1
      Default
        n = 0
    EndSelect
    If (n < lastval)
      arabic - n
    Else
      arabic + n
    EndIf
    lastval = n
  Next

  ProcedureReturn arabic
EndProcedure

If OpenConsole()
  PrintN(Str(romanDec("MCMXCIX"))) ;1999
  PrintN(Str(romanDec("MDCLXVI"))) ;1666
  PrintN(Str(romanDec("XXV")))     ;25
  PrintN(Str(romanDec("CMLIV")))   ;954
  PrintN(Str(romanDec("MMXI")))    ;2011

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
```

{{out}}

```txt
1999
1666
25
954
2011
```



## Python


### Imperative


```python
_rdecode = dict(zip('MDCLXVI', (1000, 500, 100, 50, 10, 5, 1)))

def decode( roman ):
    result = 0
    for r, r1 in zip(roman, roman[1:]):
        rd, rd1 = _rdecode[r], _rdecode[r1]
        result += -rd if rd < rd1 else rd
    return result + _rdecode[roman[-1]]

if __name__ == '__main__':
    for r in 'MCMXC MMVIII MDCLXVI'.split():
        print( r, decode(r) )
```

{{out}}

```txt
MCMXC 1990
MMVIII 2008
MDCLXVI 1666
```


Another version, which I believe has clearer logic:

```python
roman_values = (('I',1), ('IV',4), ('V',5), ('IX',9),('X',10),('XL',40),('L',50),('XC',90),('C',100),
                    ('CD', 400), ('D', 500), ('CM', 900), ('M',1000))

def roman_value(roman):
    total=0
    for symbol,value in reversed(roman_values):
        while roman.startswith(symbol):
            total += value
            roman = roman[len(symbol):]
    return total

if __name__=='__main__':
    for value in "MCMXC", "MMVIII", "MDCLXVI":
        print('%s = %i' % (value, roman_value(value)))

```

{{out}}

```txt

MCMXC = 1990
MMVIII = 2008
MDCLXVI = 1666

```
<!--[[User:Tonyjv|Tonyjv]] 16:29, 20 September 2011 (UTC)--> <!-- contributions not normally signed visually; info is in history -->



### Declarative

Less clear, but a 'one liner':

```python
numerals = { 'M' : 1000, 'D' : 500, 'C' : 100, 'L' : 50, 'X' : 10, 'V' : 5, 'I' : 1 }
def romannumeral2number(s):
        return reduce(lambda x, y: -x + y if x < y else x + y, map(lambda x: numerals.get(x, 0), s.upper()))
```



Or, defining '''intFromRoman''' as a fold or reduction,
and annotating a little more fully:
{{Trans|Haskell}}
{{Works with|Python|3}}

```python
'''Roman numerals decoded'''

from operator import mul
from functools import reduce
from collections import defaultdict
from itertools import accumulate, chain, cycle


# intFromRoman :: String -> Maybe Int
def intFromRoman(s):
    '''Just the integer represented by a Roman
       numeral string, or Nothing if any
       characters are unrecognised.
    '''
    def go(mb, x):
        '''Just a letter value added to or
           subtracted from a total, or Nothing
           if no letter value is defined.
        '''
        if mb.get('Nothing') or None is x:
            return Nothing()
        else:
            r, total = mb.get('Just')
            return Just((
                x,
                total + (-x if x < r else x)
            ))

    dct = defaultdict(
        lambda: None,
        zip(
            'IVXLCDM',
            accumulate(chain([1], cycle([5, 2])), mul)
        )
    )
    return bindMay(
        reduce(
            go,
            [dct[k.upper()] for k in reversed(list(s))],
            Just((0, 0))
        )
    )(compose(Just)(snd))


# TEST ----------------------------------------------------
def main():
    '''Testing a sample of dates.'''

    print(
        fTable(__doc__ + ':\n')(str)(
            maybe('(Contains unknown character)')(str)
        )(
            intFromRoman
        )([
            "MDCLXVI", "MCMXC", "MMVIII",
            "MMXVI", "MMXVIII", "MMZZIII"
        ])
    )


# GENERIC -------------------------------------------------

# Just :: a -> Maybe a
def Just(x):
    '''Constructor for an inhabited Maybe (option type) value.'''
    return {'type': 'Maybe', 'Nothing': False, 'Just': x}


# Nothing :: Maybe a
def Nothing():
    '''Constructor for an empty Maybe (option type) value.'''
    return {'type': 'Maybe', 'Nothing': True}


# bindMay (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
def bindMay(m):
    '''Injection operator for the Maybe monad.
       If m is Nothing, it is passed straight through.
       If m is Just(x), the result is an application
       of the (a -> Maybe b) function (mf) to x.'''
    return lambda mf: (
        m if m.get('Nothing') else mf(m.get('Just'))
    )


# compose (<<<) :: (b -> c) -> (a -> b) -> a -> c
def compose(g):
    '''Right to left function composition.'''
    return lambda f: lambda x: g(f(x))


# maybe :: b -> (a -> b) -> Maybe a -> b
def maybe(v):
    '''Either the default value v, if m is Nothing,
       or the application of f to x,
       where m is Just(x).
    '''
    return lambda f: lambda m: v if m.get('Nothing') else (
        f(m.get('Just'))
    )


# snd :: (a, b) -> b
def snd(tpl):
    '''Second member of a pair.'''
    return tpl[1]


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


# MAIN ---
if __name__ == '__main__':
    main()
```

{{Out}}

```txt
Roman numerals decoded:

MDCLXVI -> 1666
  MCMXC -> 1990
 MMVIII -> 2008
  MMXVI -> 2016
MMXVIII -> 2018
MMZZIII -> (Contains unknown character)
```



## Racket


```Racket
#lang racket
(define (decode/roman number)
  (define letter-values
    (map cons '(#\M #\D #\C #\L #\X #\V #\I) '(1000 500 100 50 10 5 1)))
  (define (get-value letter)
    (cdr (assq letter letter-values)))
  (define lst (map get-value (string->list number)))
  (+ (last lst)
     (for/fold ((sum 0))
       ((i (in-list lst)) (i+1 (in-list (cdr lst))))
       (+ sum
          (if (> i+1 i)
              (- i)
              i)))))

(map decode/roman '("MCMXC" "MMVIII" "MDCLXVI"))
;-> '(1990 2008 1666)
```



## R



### version 1

Modelled along the lines of other decode routines on this page, but using a vectorised approach

```R
romanToArabic <- function(roman) {
  romanLookup <- c(I=1L, V=5L, X=10L, L=50L, C=100L, D=500L, M=1000L)
  rSplit <- strsplit(toupper(roman), character(0)) # Split input vector into characters
  toArabic <- function(item) {
    digits <- romanLookup[item]
    if (length(digits) > 1L) {
      smaller <- (digits[-length(digits)] < digits[-1L])
      digits[smaller] <- - digits[smaller]
    }
    sum(digits)
  }
  vapply(rSplit, toArabic, integer(1))
}
```


Example usage:

```R
romanToArabic(c("MCMXII", "LXXXVI"))
```



### version 2

Using built-in functionality in R


```R
as.integer(as.roman(c("MCMXII", "LXXXVI"))
```



## Red


### version 1



```Red
Red [
    Purpose: "Arabic <-> Roman numbers converter"
    Author: "Didier Cadieu"
    Date: "07-Oct-2016"
]

table-r2a: reverse [1000 "M" 900 "CM" 500 "D" 400 "CD" 100 "C" 90 "XC" 50 "L" 40 "XL" 10 "X" 9 "IX" 5 "V" 4 "IV" 1 "I"]

roman-to-arabic: func [r [string!] /local a b e] [
	a: 0
	parse r [any [b: ["I" ["V" | "X" | none] | "X" ["L" | "C" | none] | "C" ["D" | "M" | none] | "V" | "L" | "D" | "M"] e: (a: a + select table-r2a copy/part b e)]]
	a
]

; Example usage:
print roman-to-arabic "XXXIII"
print roman-to-arabic "MDCCCLXXXVIII"
print roman-to-arabic "MMXVI"

```



## REXX


### version 1

{{Trans|NetRexx}}
{{Works with|Regina}}
{{Works with|ooRexx}}


```REXX
/* Rexx */

Do
        /* 1990  2008   1666    */
  years = 'MCMXC MMVIII MDCLXVI'

  Do y_ = 1 to words(years)
    Say right(word(years, y_), 10) || ':' decode(word(years, y_))
    End y_

  Return
End
Exit

decode:
  Procedure
Do
  Parse upper arg roman .

  If verify(roman, 'MDCLXVI') = 0 then Do

    /* always insert the value of the least significant numeral */
    decnum = rchar(substr(roman, length(roman), 1))
    Do d_ = 1 to length(roman) - 1
      If rchar(substr(roman, d_, 1)) < rchar(substr(roman, d_ + 1, 1)) then Do
        /* Handle cases where numerals are not in descending order */
        /*   subtract the value of the numeral */
        decnum = decnum - rchar(substr(roman, d_, 1))
        End
      else Do
        /* Normal case */
        /*   add the value of the numeral */
        decnum = decnum + rchar(substr(roman, d_, 1))
        End
      End d_
    End
  else Do
    decnum = roman 'contains invalid roman numerals'
    End

  Return decnum
End
Exit

rchar:
  Procedure
Do
  Parse upper arg ch +1 .

  select
    when ch = 'M' then digit = 1000
    when ch = 'D' then digit =  500
    when ch = 'C' then digit =  100
    when ch = 'L' then digit =   50
    when ch = 'X' then digit =   10
    when ch = 'V' then digit =    5
    when ch = 'I' then digit =    1
    otherwise          digit =    0
    end

  Return digit
End
Exit
```

{{out}}

```txt

     MCMXC: 1990
    MMVIII: 2008
   MDCLXVI: 1666

```



### version 2

This version of the (above) REXX program:
:::*   removes 3 sets of superfluous   '''do──end'''   statements
:::*   removes dead code (3 REXX statements that can't be executed)
:::*   replaced   '''substr(xxx, length(xxx), 1)'''     with     '''right(xxx,1)'''
:::*   removes a useless   '''parse'''   statement
:::*   compresses '''63''' lines to '''29''' lines
:::*   reordered   '''if'''   statements by most likely to occur
This REXX version   '''won't'''   handle:
:::*   Roman numbers like   '''IIXX'''
:::*   the   '''j'''   and   '''u'''   numerals
:::*   (deep) parenthesis type Roman numbers

```rexx
/*REXX program  converts  Roman numeral number(s)  ───►  Arabic numerals  (or numbers). */
rYear = 'MCMXC'    ;      say right(rYear, 9)":"     rom2dec(rYear)
rYear = 'mmviii'   ;      say right(rYear, 9)":"     rom2dec(rYear)
rYear = 'MDCLXVI'  ;      say right(rYear, 9)":"     rom2dec(rYear)
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
rom2dec: procedure;  arg roman .                 /*obtain the Roman numeral number.     */
if verify(roman, 'MDCLXVI')\==0  then return  "***error***  invalid Roman number:"   roman
#=rChar(right(roman, 1))                         /*start with the last Roman numeral.   */
            do j=1  for length(roman) - 1
            x=rChar( substr(roman, j  , 1) )     /*extract the  current  Roman numeral. */
            y=rChar( substr(roman, j+1, 1) )     /*extract the     next  Roman numeral. */
            if x<y  then # = #-x                 /*Is   x<y ?         Then subtract it. */
                    else # = #+x                 /*Is   x≥y ?           "    add     "  */
            end   /*j*/
return #
/*──────────────────────────────────────────────────────────────────────────────────────*/
rChar: procedure;  arg _                         /*convert Roman number to Arabic digits*/
       if _=='I'  then return    1
       if _=='V'  then return    5
       if _=='X'  then return   10
       if _=='L'  then return   50
       if _=='C'  then return  100
       if _=='D'  then return  500
       if _=='M'  then return 1000
                       return    0               /*indicate an  invalid  Roman numeral. */
```



### version 3

This REXX version allows the use of    '''j'''   which was being used in the later part of the Holy Roman Empire

(as a trailing   ''' i '''   in Roman numerals).

Also, this program converts   ''' IIXX '''   correctly.     (Note:   this Roman numeral was actually chiseled on

some Roman monuments, archways, and tombs/crypts.)

Also supported are larger numbers such as   '''(M)'''   which is a Roman numeral(s) within a set of grouping

symbols,   in this case, a set of parenthesis   (brackets and/or braces can also be used).

Deep parentheses are also supported:   '''(MM)'''   is two million,   '''((MMM))'''   is three billion.

Normally, the Romans used an overbar (vinculum) for larger numbers (such as   '''{{overline|XL}}'''   for forty-thousand),

but the use of such a character is very problematic for computers to deal with,   so parenthesis are used

instead.

The Romans also had symbols for some fractions which would be a good addition to this task.

Also, lowercase   '''u'''   was also used for lowercase   '''v'''

Also note that   '''IIII'''   is a legal Roman numeral construct;   (as demonstrated by almost any old clock or

"dialed" wristwatch that has Roman numerals).

```rexx
/*REXX program  converts  Roman numeral number(s)  ───►  Arabic numerals  (or numbers). */
numeric digits 1000                              /*so we can handle the big numbers.    */
parse arg z                                      /*obtain optional arguments from the CL*/
if z=''  then z= "MCMXC mmviii IIXX LU MDCLXVI MDWLXVI ((mmm)) [[[[[D]]]]]"  /*defaults.*/

     do j=1  for words(z);   y=word(z, j)        /*process each of the Roman numbers.   */
     say  right(y, 20)':'    rom2dec(y)          /*display original and decimal version.*/
     end   /*j*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
rom2dec:  procedure;   h='0'x;   #=0;   $=1;   arg n .         /*"ARG"  uppercases  N.  */
n=translate(n, '()()', "[]{}");  _=verify(n, 'MDCLXVUIJ()')    /*trans grouping symbols.*/
if _\==0  then return '***error*** invalid Roman numeral:'  substr(n,_,1)   /*tell error*/
@.=1; @.m=1000; @.d=500; @.c=100; @.l=50; @.x=10; @.u=5; @.v=5 /*Roman numeral values.  */
                                                               /* [↓]  convert number.  */
   do k=length(n)  to 1  by -1;  _=substr(n, k, 1)             /*examine a Roman numeral*/
                                                               /* [↑]  scale up or down.*/
   if _=='(' | _==")"  then  do;  $=$*1000; if _=='(' then $=1 /* (≡scale ↑;  )≡scale ↓ */
                                  iterate                      /*go & process next digit*/
                             end
   _=@._*$                                                     /*scale it if necessary. */
   if _>h  then h=_                                            /*remember Roman numeral.*/
   if _<h  then #=#-_                                          /*char>next?  Then sub.  */
           else #=#+_                                          /*            else add.  */
   end   /*k*/
return #                                                       /*return Arabic number.  */
```

'''output'''   when using the default inputs:

```txt

               MCMXC: 1990
              mmviii: 2008
                IIXX: 18
                  LU: 55
             MDCLXVI: 1666
             MDWLXVI: ***error*** invalid Roman numeral: W
             ((mmm)): 3000000000
         [[[[[D]]]]]: 500000000000000000

```



## Ring


```ring

symbols = "MDCLXVI"
weights = [1000,500,100,50,10,5,1]

see "MCMXCIX = " + romanDec("MCMXCIX") + nl
see "MDCLXVI =" + romanDec("MDCLXVI") + nl
see "XXV = " + romanDec("XXV") + nl
see "CMLIV = " + romanDec("CMLIV") + nl
see "MMXI = " + romanDec("MMXI") + nl

func romanDec roman
     n = 0
     lastval = 0
     arabic = 0
     for i = len(roman) to 1 step -1
         n = substr(symbols,roman[i])
         if n > 0 n = weights[n] ok
         if n < lastval arabic = arabic - n
         else arabic = arabic + n ok
         lastval = n
     next
     return arabic

```


## Ruby


```ruby
def fromRoman(roman)
  r = roman.upcase
  n = 0
  until r.empty? do
    case
    when r.start_with?('M')  then v = 1000; len = 1
    when r.start_with?('CM') then v = 900;  len = 2
    when r.start_with?('D')  then v = 500;  len = 1
    when r.start_with?('CD') then v = 400;  len = 2
    when r.start_with?('C')  then v = 100;  len = 1
    when r.start_with?('XC') then v = 90;   len = 2
    when r.start_with?('L')  then v = 50;   len = 1
    when r.start_with?('XL') then v = 40;   len = 2
    when r.start_with?('X')  then v = 10;   len = 1
    when r.start_with?('IX') then v = 9;    len = 2
    when r.start_with?('V')  then v = 5;    len = 1
    when r.start_with?('IV') then v = 4;    len = 2
    when r.start_with?('I')  then v = 1;    len = 1
    else
      raise ArgumentError.new("invalid roman numerals: " + roman)
    end
    n += v
    r.slice!(0,len)
  end
  n
end

[ "MCMXC", "MMVIII", "MDCLXVI" ].each {|r| p r => fromRoman(r)}
```


{{out}}

```txt

{"MCMXC"=>1990}
{"MMVIII"=>2008}
{"MDCLXVI"=>1666}

```

or

```ruby
SYMBOLS = [ ['M', 1000], ['CM', 900], ['D', 500], ['CD', 400], ['C', 100], ['XC', 90],
            ['L', 50], ['XL', 40], ['X', 10], ['IX', 9], ['V', 5], ['IV', 4], ['I', 1] ]

def parseRoman(roman)
  r = roman.upcase
  n = 0
  SYMBOLS.each { |sym, val| n += val while r.sub!(/^#{sym}/, "") }
  n
end

[ "MCMXC", "MMVIII", "MDCLXVI" ].each {|r| puts "%8s :%5d" % [r, parseRoman(r)]}
```


{{out}}

```txt

   MCMXC : 1990
  MMVIII : 2008
 MDCLXVI : 1666

```



## Run BASIC


```runbasic
print "MCMXCIX = "; romToDec( "MCMXCIX") '1999
print "MDCLXVI = "; romToDec( "MDCLXVI") '1666
print "XXV     = "; romToDec( "XXV")     '25
print "CMLIV   = "; romToDec( "CMLIV")   '954
print "MMXI    = "; romToDec( "MMXI")    '2011

function romToDec(roman$)
  for i  = len(roman$) to 1 step -1
   x$    = mid$(roman$, i, 1)
   n     = 0
   if x$ = "M" then n = 1000
   if x$ = "D" then n = 500
   if x$ = "C" then n = 100
   if x$ = "L" then n = 50
   if x$ = "X" then n = 10
   if x$ = "V" then n = 5
   if x$ = "I" then n = 1

  if n < preNum then num = num - n else num = num + n
  preNum = n
  next

  romToDec =num
end function
```



## Rust


```rust
struct RomanNumeral {
    symbol: &'static str,
    value: u32
}

const NUMERALS: [RomanNumeral; 13] = [
    RomanNumeral {symbol: "M",  value: 1000},
    RomanNumeral {symbol: "CM", value: 900},
    RomanNumeral {symbol: "D",  value: 500},
    RomanNumeral {symbol: "CD", value: 400},
    RomanNumeral {symbol: "C",  value: 100},
    RomanNumeral {symbol: "XC", value: 90},
    RomanNumeral {symbol: "L",  value: 50},
    RomanNumeral {symbol: "XL", value: 40},
    RomanNumeral {symbol: "X",  value: 10},
    RomanNumeral {symbol: "IX", value: 9},
    RomanNumeral {symbol: "V",  value: 5},
    RomanNumeral {symbol: "IV", value: 4},
    RomanNumeral {symbol: "I",  value: 1}
];

fn to_hindu(roman: &str) -> u32 {
    match NUMERALS.iter().find(|num| roman.starts_with(num.symbol)) {
        Some(num) => num.value + to_hindu(&roman[num.symbol.len()..]),
        None => 0, // if string empty, add nothing
    }
}

fn main() {
    let roms = ["MMXIV", "MCMXCIX", "XXV", "MDCLXVI", "MMMDCCCLXXXVIII"];
    for &r in &roms {
        // 15 is minimum formatting width of the first argument, there for alignment
        println!("{:2$} = {}", r, to_hindu(r), 15);
    }
}
```

{{out}}

```txt
MMXIV           = 2014
MCMXCIX         = 1999
XXV             = 25
MDCLXVI         = 1666
MMMDCCCLXXXVIII = 3888
```



## Scala


```Scala
def fromRoman( r:String ) : Int = {
  val arabicNumerals = List("CM"->900,"M"->1000,"CD"->400,"D"->500,"XC"->90,"C"->100,
                            "XL"->40,"L"->50,"IX"->9,"X"->10,"IV"->4,"V"->5,"I"->1)

  var s = r
  arabicNumerals.foldLeft(0){ (n,t) => {
    val l = s.length; s = s.replaceAll(t._1,""); val c = (l - s.length)/t._1.length  // Get the frequency
    n + (c*t._2)  // Add the arabic numerals up
  } }
}

// Here is a another version that does a simple running sum:
def fromRoman2(s: String) : Int = {
    val numerals = Map('I' -> 1, 'V' -> 5, 'X' -> 10, 'L' -> 50, 'C' -> 100, 'D' -> 500, 'M' -> 1000)

    s.toUpperCase.map(numerals).foldLeft((0,0)) {
      case ((sum, last), curr) =>  (sum + curr + (if (last < curr) -2*last else 0), curr) }._1
  }
}

// A small test
def test( roman:String ) = println( roman + " => " + fromRoman( roman ) )

test("MCMXC")
test("MMVIII")
test("MDCLXVI")
```

{{out}}

```txt
MCMXC => 1990
MMVIII => 2008
MDCLXVI => 1666
```




## Scheme


{{works with|Gauche Scheme}}


```Scheme
(use gauche.collection) ;; for fold2

(define (char-val char)
  (define i (string-scan "IVXLCDM" char))
  (* (expt 10 (div i 2)) (expt 5 (mod i 2))))

(define (decode roman)
  (fold2
    (lambda (n sum prev-val)
      (values ((if (< n prev-val) - +) sum n) (max n prev-val)))
    0 0
    (map char-val (reverse (string->list roman)))))

```


<b>Testing:</b>

```Scheme
(for-each
  (^s (format #t "~7d: ~d\n" s (decode s)))
  '("MCMLVI" "XXC" "MCMXC" "XXCIII" "IIIIX" "MIM" "LXXIIX"))

```

{{out}}

```txt

 MCMLVI: 1956
    XXC: 80
  MCMXC: 1990
 XXCIII: 83
  IIIIX: 6
    MIM: 1999
 LXXIIX: 78

```



## Seed7


```seed7
$ include "seed7_05.s7i";

const func integer: ROMAN parse (in string: roman) is func
  result
    var integer: arabic is 0;
  local
    var integer: index is 0;
    var integer: number is 0;
    var integer: lastval is 0;
  begin
    for index range length(roman) downto 1 do
      case roman[index] of
        when {'M', 'm'}: number := 1000;
        when {'D', 'd'}: number :=  500;
        when {'C', 'c'}: number :=  100;
        when {'L', 'l'}: number :=   50;
        when {'X', 'x'}: number :=   10;
        when {'V', 'v'}: number :=    5;
        when {'I', 'i'}: number :=    1;
        otherwise:       raise RANGE_ERROR;
      end case;
      if number < lastval then
        arabic -:= number;
      else
        arabic +:= number;
      end if;
      lastval := number;
    end for;
  end func;

const proc: main is func
  begin
    writeln(ROMAN parse "MCMXC");
    writeln(ROMAN parse "MMVIII");
    writeln(ROMAN parse "MDCLXVI");
  end func;
```

Original source: [http://seed7.sourceforge.net/algorith/puzzles.htm#decode_roman_numerals]
{{out}}

```txt

1990
2008
1666

```



## Sidef


```ruby
func roman2arabic(roman) {
 
    var arabic = 0
    var last_digit = 1000
 
    static m = Hash(
        I =>    1,
        V =>    5,
        X =>   10,
        L =>   50,
        C =>  100,
        D =>  500,
        M => 1000,
    )
 
    roman.uc.chars.map{m{_} \\ 0}.each { |digit|
        if (last_digit < digit) {
            arabic -= (2 * last_digit)
        }
        arabic += (last_digit = digit)
    }
 
    return arabic
}
 
%w(MCMXC MMVIII MDCLXVI).each { |roman_digit|
    "%-10s == %d\n".printf(roman_digit, roman2arabic(roman_digit))
}
```

{{out}}

```txt

MCMXC      == 1990
MMVIII     == 2008
MDCLXVI    == 1666

```


Simpler solution:

```ruby
func roman2arabic(digit) {
    digit.uc.trans([
        :M:  '1000+',
        :CM:  '900+',
        :D:   '500+',
        :CD:  '400+',
        :C:   '100+',
        :XC:   '90+',
        :L:    '50+',
        :XL:   '40+',
        :X:    '10+',
        :IX:    '9+',
        :V:     '5+',
        :IV:    '4+',
        :I:     '1+',
    ]).split('+').map{.to_i}.sum;
}
 
%w(MCMXC MMVIII MDCLXVI).each { |roman_num|
    say "#{roman_num}\t-> #{roman2arabic(roman_num)}";
}
```

{{out}}

```txt

MCMXC   -> 1990
MMVIII  -> 2008
MDCLXVI -> 1666

```


## Simula


```simula
BEGIN

    INTEGER PROCEDURE FROMROMAN(S); TEXT S;
    BEGIN
        PROCEDURE P(INTVAL, NUM); INTEGER INTVAL; TEXT NUM;
        BEGIN
            INTEGER NLEN;
            NLEN := NUM.LENGTH;
            WHILE INDEX + NLEN - 1 <= SLEN AND THEN
                S.SUB(INDEX, NLEN) = NUM DO
            BEGIN
                RESULT := RESULT + INTVAL;
                INDEX := INDEX + NLEN;
            END WHILE;
        END P;
        INTEGER RESULT, INDEX, SLEN;
        SLEN := S.LENGTH;
        INDEX := 1;
        P( 1000, "M"  );
        P(  900, "CM" );
        P(  500, "D"  );
        P(  400, "CD" );
        P(  100, "C"  );
        P(   90, "XC" );
        P(   50, "L"  );
        P(   40, "XL" );
        P(   10, "X"  );
        P(    9, "IX" );
        P(    5, "V"  );
        P(    4, "IV" );
        P(    1, "I"  );
        FROMROMAN := RESULT;
    END FROMROMAN;

    TEXT T;
    FOR T :- "MCMXC", "MMVIII", "MDCLXVI" DO
    BEGIN
        OUTTEXT("ROMAN """);
        OUTTEXT(T);
        OUTTEXT(""" => ");
        OUTINT(FROMROMAN(T), 0);
        OUTIMAGE;
    END FOR;

END PROGRAM;

```

{{out}}

```txt

ROMAN "MCMXC" => 1990
ROMAN "MMVIII" => 2008
ROMAN "MDCLXVI" => 1666

```



## SPL


```SPL
r2a(r)=
  n = [1,5,10,50,100,500,1000]
  a,m = 0
  > i, #.size(r)..1, -1
    v,c = n[#.pos("IVXLCDM",#.mid(r,i))]
    ? v<m, v = -v
    ? c>m, m = c
    a += v
  <
  <= a
.

t = ["MMXI","MIM","MCMLVI","MDCLXVI","XXCIII","LXXIIX","IIIIX"]
> i, 1..#.size(t,1)
  #.output(t[i]," = ",r2a(t[i]))
<
```

{{out}}

```txt

MMXI = 2011
MIM = 1999
MCMLVI = 1956
MDCLXVI = 1666
XXCIII = 83
LXXIIX = 78
IIIIX = 6

```



## TechBASIC


```techBASIC


Main:
!------------------------------------------------
! CALLS THE romToDec FUNCTION WITH THE ROMAN
! NUMERALS AND RETURNS ITS DECIMAL EQUIVELENT.
!

   PRINT "MCMXC   = "; romToDec("MCMXC")   !1990
   PRINT "MMVIII  = "; romToDec("MMVIII")  !2008
   PRINT "MDCLXVI = "; romToDec("MDCLXVI") !1666
   PRINT:PRINT
   PRINT "Here are other solutions not from the TASK:"
   PRINT "MCMXCIX = "; romToDec("MCMXCIX") !1999
   PRINT "XXV     = "; romToDec("XXV")     !25
   PRINT "CMLIV   = "; romToDec("CMLIV")   !954
   PRINT "MMXI    = "; romToDec("MMXI")    !2011
   PRINT:PRINT
   PRINT "Without error checking, this also is 2011, but is wrong"
   PRINT "MMIIIX  = "; romToDec("MMIIIX")  !INVAID, 2011

STOP


FUNCTION romToDec(roman AS STRING) AS INTEGER
!------------------------------------------------------
! FUNCTION THAT CONVERTS ANY ROMAN NUMERAL TO A DECIMAL
!
    prenum=0!num=0
    ln=LEN(roman)
    FOR i=ln TO 1 STEP -1
        x$=MID(roman,i,1)
        n=1000
        SELECT CASE x$
               CASE "M":n=n/1
               CASE "D":n=n/2
               CASE "C":n=n/10
               CASE "L":n=n/20
               CASE "X":n=n/100
               CASE "V":n=n/200
               CASE "I":n=n/n
               CASE ELSE:n=0
        END SELECT
        IF n < preNum THEN num=num-n ELSE num=num+n
        preNum=n
   next i

   romToDec=num

END FUNCTION

```


{{out}}

```txt

MCMXC   = 1990
MMVIII  = 2008
MDCLXVI = 1666


Here are other solutions not from the TASK:
MCMXCIX = 1999
XXV     = 25
CMLIV   = 954
MMXI    = 2011


Without error checking, this also is 2011, but is wrong
MMIIIX  = 2011

```



## SNOBOL4


```SNOBOL4
*        Roman to Arabic
        define('arabic(n)s,ch,val,sum,x') :(arabic_end)
arabic  s = 'M1000 D500 C100 L50 X10 V5 I1 '
        n = reverse(n)
arab1   n len(1) . ch = :f(arab2)
        s ch break(' ') . val
        val = lt(val,x) (-1 * val)
        sum = sum + val; x = val        :(arab1)
arab2   arabic = sum                    :(return)
arabic_end

*        Test and display
        tstr = 'MMX MCMXCIX MCDXCII MLXVI CDLXXVI "
tloop   tstr break(' ') . r span(' ') = :f(out)
        astr = astr r '=' arabic(r) ' ' :(tloop)
out     output = astr
end
```

{{out}}

```txt
MMX=2010 MCMXCIX=1999 MCDXCII=1492 MLXVI=1066 CDLXXVI=476
```

Here's an alternative version, which is maybe more SNOBOL4-idiomatic and less like one might program it in a more common language:

```SNOBOL4
*   Roman to Arabic
	define("arabic1(romans,arabic1)rdigit,adigit,b4")
	romans1 = " 0 IX9 IV4 III3 II2 I1 VIII8 VII7 VI6 V5"  :(arabic1_end)
arabic1 ident(romans)                             :s(return)
	romans (break("IV") | rem) . b4 rem . rdigit = b4
        romans1 " " rdigit any("0123456789") . adigit
	arabic1 = adigit arabic1
        romans = replace(romans,"MDCLX","CLXVI")  :(arabic1)
arabic1_end
*   Test and display
        tstr = "MMX MCMXCIX MCDXCII MLXVI CDLXXVI "
tloop   tstr break(' ') . r span(' ') =           :f(out)
        astr = astr r '=' arabic1(r) ' '          :(tloop)
out     output = astr
end
```

The output is the same as in the earlier version.

The following version takes advantage of some of the so-called "SPITBOL extensions", which are to be found in most modern implementations.
This allows removing several labels and explicit transfers of control, and moves some of the looping into the pattern matcher.
Again, the output is the same.

```SNOBOL4
*   Roman to Arabic
	define("arabic1(romans,arabic1)rdigit,adigit,b4")
	romans1 = " 0 IX9 IV4 III3 II2 I1 VIII8 VII7 VI6 V5"  :(arabic1_end)
arabic1 ident(romans)                             :s(return)
	romans (break("IV") | rem) . b4 rem . rdigit = replace(b4,"MDCLX","CLXVI")
        romans1 " " rdigit any("0123456789") . adigit
	arabic1 = adigit arabic1                  :(arabic1)
arabic1_end
*   Test and display
        tstr = " MMX MCMXCIX MCDXCII MLXVI CDLXXVI "
        tstr span(' ') break(' ') $ r *?(astr = astr r '=' arabic1(r) ' ') fail
        output = astr
end
```



## Swift


```swift
extension Int {
    init(romanNumerals: String) {
        let values = [
            ( "M", 1000),
            ("CM",  900),
            ( "D",  500),
            ("CD",  400),
            ( "C",  100),
            ("XC",   90),
            ( "L",   50),
            ("XL",   40),
            ( "X",   10),
            ("IX",    9),
            ( "V",    5),
            ("IV",    4),
            ( "I",    1),
        ]

        self = 0
        var raw = romanNumerals
        for (digit, value) in values {
            while raw.hasPrefix(digit) {
                self += value
                raw.removeFirst(digit.count)
            }
        }
    }
}

```

{{output}}

```swift
Int(romanNumerals: "MDCLXVI") // 1666
```



## Tcl

As long as we assume that we have a valid roman number, this is most easily done by transforming the number into a sum and evaluating the expression:

```tcl
proc fromRoman rnum {
    set map {M 1000+ CM 900+ D 500+ CD 400+ C 100+ XC 90+ L 50+ XL 40+ X 10+ IX 9+ V 5+ IV 4+ I 1+}
    expr [string map $map $rnum]0}
}
```

Demonstrating:

```tcl
foreach r {MCMXC MDCLXVI MMVIII} {
    puts "$r\t-> [fromRoman $r]"
}
```

{{out}}

```txt
MCMXC	-> 1990
MDCLXVI	-> 1666
MMVIII	-> 2008
```


=={{header|TI-83 BASIC}}==
Using the Rom‣Dec function "real(21," from [http://www.detachedsolutions.com/omnicalc/ Omnicalc].

```ti83b
PROGRAM:ROM2DEC
:Input Str1
:Disp real(21,Str1)
```


Using TI-83 BASIC

```ti83b
PROGRAM:ROM2DEC
:Input "ROMAN:",Str1
:{1000,500,100,50,10,5,1}➞L1
:0➞P
:0➞Y
:For(I,length(Str1),1,-1)
  :inString("MDCLXVI",sub(Str1,I,1))➞X
  :If X≤0:Then
    :Disp "BAD NUMBER"
    :Stop
  :End
  :L1(x)➞N
  :If N<P:Then
    :Y–N➞Y
  :Else
    :Y+N➞Y
  :End
  :N➞P
:End
:Disp Y
```



## TUSCRIPT


```tuscript
$$ MODE TUSCRIPT
LOOP roman_number="MCMXC'MMVIII'MDCLXVI"
arab_number=DECODE (roman_number,ROMAN)
PRINT "Roman number ",roman_number," equals ", arab_number
ENDLOOP
```

{{out}}

```txt

Roman number MCMXC equals 1990
Roman number MMVIII equals 2008
Roman number MDCLXVI equals 1666

```


## UNIX Shell


```bash

#!/bin/bash

roman_to_dec() {
  local rnum=$1
  local n=0
  local prev=0

  for ((i=${#rnum}-1;i>=0;i--))
  do
    case "${rnum:$i:1}" in
    M)  a=1000 ;;
    D)  a=500 ;;
    C)  a=100 ;;
    L)  a=50 ;;
    X)  a=10 ;;
    V)  a=5 ;;
    I)  a=1 ;;
    esac

    if [[ $a -lt $prev ]]
    then
       let n-=a
    else
       let n+=a
    fi

    prev=$a
  done

  echo "$rnum = $n"
}

roman_to_dec MCMXC
roman_to_dec MMVIII
roman_to_dec MDCLXVI

```



## VBA

Convert Romans (i.e : XVI) in integers

```vb

Option Explicit

Sub Main_Romans_Decode()
Dim Arr(), i&

    Arr = Array("III", "XXX", "CCC", "MMM", "VII", "LXVI", "CL", "MCC", "IV", "IX", "XC", "ICM", "DCCCXCIX", "CMI", "CIM", "MDCLXVI", "MCMXC", "MMXVII")
    For i = 0 To UBound(Arr)
        Debug.Print Arr(i) & "   >>> " & lngConvert(CStr(Arr(i)))
    Next
End Sub

Function Convert(Letter As String) As Long
Dim Romans(), DecInt(), Pos As Integer

    Romans = Array("M", "D", "C", "L", "X", "V", "I")
    DecInt = Array(1000, 500, 100, 50, 10, 5, 1)
    Pos = -1
    On Error Resume Next
    Pos = Application.Match(Letter, Romans, 0) - 1
    On Error GoTo 0
    If Pos <> -1 Then Convert = DecInt(Pos)
End Function

Function lngConvert(strRom As String) 'recursive function
Dim i As Long, iVal As Integer

    If Len(strRom) = 1 Then
        lngConvert = Convert(strRom)
    Else
        iVal = Convert(Mid(strRom, 1, 1))
        If iVal < Convert(Mid(strRom, 2, 1)) Then iVal = iVal * (-1)
        lngConvert = iVal + lngConvert(Mid(strRom, 2, Len(strRom) - 1))
    End If
End Function

```

{{out}}

```txt
III   >>> 3
XXX   >>> 30
CCC   >>> 300
MMM   >>> 3000
VII   >>> 7
LXVI   >>> 66
CL   >>> 150
MCC   >>> 1200
IV   >>> 4
IX   >>> 9
XC   >>> 90
ICM   >>> 899
DCCCXCIX   >>> 899
CMI   >>> 901
CIM   >>> 1099
MDCLXVI   >>> 1666
MCMXC   >>> 1990
MMXVII   >>> 2017
```



## VBScript

{{trans|360 Assembly}}

```vb
' Roman numerals Encode - Visual Basic - 18/04/2019

Function toRoman(ByVal value)
    Dim arabic
    Dim roman
    Dim i, result
    arabic = Array(1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1)
    roman = Array("M", "CM", "D", "CD", "C", "XC", "L", "XL", "X", "IX", "V", "IV", "I")
    For i = 0 To 12
        Do While value >= arabic(i)
            result = result + roman(i)
            value = value - arabic(i)
        Loop
    Next 'i
    toRoman = result
End Function 'toRoman

    n=InputBox("Number, please","Roman numerals/Encode")
    code=MsgBox(n & vbCrlf & toRoman(n),vbOKOnly+vbExclamation,"Roman numerals/Encode")
	If code=vbOK Then ok=1

```

{{out}}

```txt

III   >>> 3
XXX   >>> 30
CCC   >>> 300
MMM   >>> 3000
VII   >>> 7
LXVI   >>> 66
CL   >>> 150
MCC   >>> 1200
IV   >>> 4
IX   >>> 9
XC   >>> 90
ICM   >>> 901
DCCCXCIX   >>> 899
CMI   >>> 901
CIM   >>> 1099
MDCLXVI   >>> 1666
MCMXC   >>> 1990
MMXVII   >>> 2017
I   >>> 1
XIV   >>> 14
MMMDCCCLXXXVIII   >>> 3888
MMMCMXCIX   >>> 3999

```




## zsh


```zsh

#!/bin/zsh
function parseroman () {
  local max=0 sum i j
  local -A conv
  conv=(I 1 V 5 X 10 L 50 C 100 D 500 M 1000)
  for j in ${(Oas::)1}; do
    i=conv[$j]
    if (( i >= max )); then
      (( sum+=i ))
      (( max=i ))
    else
      (( sum-=i ))
    fi
  done
  echo $sum
}

parseroman MCMXC
parseroman MMVIII
parseroman MDCLXVI

```



## Vedit macro language


```vedit
// Main program for testing the function
//
do {
    Get_Input(10, "Enter a roman numeral: ", NOCR+STATLINE)
    Call("Roman_to_Arabic")
    Reg_Type(10) Message("	= ") Num_Type(#1)
} while(#1)
Return

// Convert Roman numeral into numeric value
//  in:  @10 = Roman numeral
//  out: #1  = numeric value
//
:Roman_to_Arabic:
    Buf_Switch(Buf_Free)
    Ins_Text("M1000 D500 C100 L50 X10 V5 I1") Ins_Newline
    Reg_Ins(10) Ins_Char(' ')
    #1 = #2 = 0

    Repeat(ALL) {
        #3 = #2                                  // #3 = previous character
        Goto_Line(2)                             // roman numeral to be converted
        if (At_EOL) {
            Break                                // all done
        }
        Reg_Copy_Block(11, CP, CP+1, DELETE)     // next character in roman numeral
        if (Search(@11, BEGIN+ADVANCE+NOERR)) {  // find character from the table
            #2 = Num_Eval(SUPPRESS)              // corresponding numeric value
            if (#2 > #3) {                       // larger than previous digit?
                #1 -= #3                         // substract previous digit
            } else {
                #1 += #3                         // add previous digit
            }
        }
    }
    Reg_Empty(11)
    Buf_Quit(OK)
Return
```

{{out}}

```txt
iv	=     4
xii	=    12
MDCLXVI	=  1666
MCMXC	=  1990
MMXI	=  2011
```



## XLISP

Uses basic list processing and recursion. Probably not amazingly fast, but quite concise and hopefully clear.

```lisp
(defun decode (r)
    (define roman '((#\m 1000) (#\d 500) (#\c 100) (#\l 50) (#\x 10) (#\v 5) (#\i 1)))
    (defun to-arabic (rn rs a)
        (cond
            ((null rn) a)
            ((eqv? (car rn) (caar rs)) (to-arabic (cdr rn) roman (if (and (not (eqv? (car rn) (cadr rn))) (< (cadar rs) (to-arabic (cdr rn) roman 0)))
                (- a (cadar rs))
                (+ a (cadar rs)) ) ) )
            (t (to-arabic rn (cdr rs) a)) ) )
    (to-arabic (string->list r) roman 0) )
```

Test it in a REPL:

```lisp
[1] (mapcar decode '("mcmxc" "mmviii" "mdclxvi"))

(1990 2008 1666)
```



## XPL0


```XPL0
string 0;                       \use zero-terminated strings
code CrLf=9, IntOut=11;

func Roman(Str);                \Convert Roman numeral string to decimal value
char Str;
int  I, Val, Val0, Sum;
[I:= 0;  Sum:= 0;  Val0:= 5000;
loop    [case Str(I) of
          ^M: Val:= 1000;
          ^D: Val:= 500;
          ^C: Val:= 100;
          ^L: Val:= 50;
          ^X: Val:= 10;
          ^V: Val:= 5;
          ^I: Val:= 1
        other return Sum;       \zero string terminator
        I:= I+1;
        Sum:= Sum + Val;
        if Val > Val0 then Sum:= Sum - 2*Val0;
        Val0:= Val;
        ];
];

[IntOut(0, Roman("MCMXC"));    CrLf(0);
 IntOut(0, Roman("MMVIII"));   CrLf(0);
 IntOut(0, Roman("MDCLXVI"));  CrLf(0);
]
```


{{out}}

```txt

1990
2008
1666

```



## Yabasic


```Yabasic
romans$ = "MDCLXVI"
decmls$ = "1000,500,100,50,10,5,1"

sub romanDec(s$)
    local i, n, prev, res, decmls$(1)

    n = token(decmls$, decmls$(), ",")

    for i = len(s$) to 1 step -1
        n = val(decmls$(instr(romans$, mid$(s$, i, 1))))
        if n < prev n = 0 - n
        res = res + n
        prev = n
    next i
    return res
end sub

? romanDec("MCMXCIX")               // 1999
? romanDec("MDCLXVI")               // 1666
? romanDec("XXV")                   // 25
? romanDec("XIX")                   // 19
? romanDec("XI")                    // 11
? romanDec("CMLIV")                 // 954
? romanDec("MMXI")                  // 2011
? romanDec("CD")                    // 400
? romanDec("MCMXC")                 // 1990
? romanDec("MMVIII")                // 2008
? romanDec("MMIX")                  // 2009
? romanDec("MDCLXVI")               // 1666
? romanDec("MMMDCCCLXXXVIII")       // 3888
```



## zkl


```zkl
var romans = L(
   L("M", 1000), L("CM", 900), L("D",  500), L("CD", 400), L("C",  100),
   L("XC",  90), L("L",   50), L("XL",  40), L("X",   10), L("IX",   9),
   L("V",    5), L("IV",   4), L("I",    1));
fcn toArabic(romanNumber){	// romanNumber needs to be upper case
   if (not RegExp("^[CDILMVX]+$").matches(romanNumber))
      throw(Exception.ValueError("Not a Roman number: %s".fmt(romanNumber)));
   reg value = 0;
   foreach R,N in (romans){
      while (0 == romanNumber.find(R)){
	 value += N;
	 romanNumber = romanNumber[R.len(),*];
      }
   }
   return(value);
}
```


```txt

toArabic("MCMXC")   //-->1990
toArabic("MMVIII")  //-->2008
toArabic("MDCLXVI") //-->1666

```


{{omit from|GUISS}}
