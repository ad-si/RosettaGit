+++
title = "Comma quibbling"
description = ""
date = 2019-10-12T08:49:32Z
aliases = []
[extra]
id = 16446
[taxonomies]
categories = ["String manipulation", "Text processing", "task"]
tags = []
languages = [
  "11l",
  "360_assembly",
  "ada",
  "algol_68",
  "algol_w",
  "applescript",
  "arturo",
  "astro",
  "autohotkey",
  "awk",
  "batch_file",
  "bracmat",
  "c",
  "clojure",
  "cobol",
  "coffeescript",
  "common_lisp",
  "cpp",
  "csharp",
  "d",
  "dcl",
  "echolisp",
  "eiffel",
  "elixir",
  "erlang",
  "factor",
  "forth",
  "fortran",
  "freebasic",
  "fsharp",
  "gambas",
  "go",
  "groovy",
  "haskell",
  "j",
  "java",
  "javascript",
  "jq",
  "julia",
  "kotlin",
  "lasso",
  "liberty_basic",
  "logo",
  "lua",
  "m2000_interpreter",
  "maple",
  "maxscript",
  "netrexx",
  "nim",
  "objeck",
  "ocaml",
  "oforth",
  "ol",
  "pari_gp",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "php",
  "picolisp",
  "pl_i",
  "powershell",
  "prolog",
  "purebasic",
  "python",
  "racket",
  "rebol",
  "rexx",
  "ring",
  "ruby",
  "run_basic",
  "rust",
  "scala",
  "scheme",
  "sed",
  "seed7",
  "sidef",
  "standard_ml",
  "swift",
  "tcl",
  "txr",
  "unix_shell",
  "vba",
  "vbscript",
  "visual_basic_dotnet",
  "xlisp",
  "xpl0",
  "zkl",
  "zx_spectrum_basic",
]
+++

## Task
Comma quibbling is a task originally set by Eric Lippert in his [http://blogs.msdn.com/b/ericlippert/archive/2009/04/15/comma-quibbling.aspx blog].


### Requirements

Write a function to generate a string output which is the concatenation of input words from a list/sequence where:
# An input of no words produces the output string of just the two brace characters "{}".
# An input of just one word, e.g. ["ABC"], produces the output string of the word inside the two braces, e.g. "{ABC}".
# An input of two words, e.g. ["ABC", "DEF"], produces the output string of the two words inside the two braces with the words separated by the string " and ", e.g. "{ABC and DEF}".
# An input of three or more words, e.g. ["ABC", "DEF", "G", "H"], produces the output string of all but the last word separated by ", " with the last word separated by " and " and all within braces; e.g. "{ABC, DEF, G and H}".



Test your function with the following series of inputs showing your output here on this page:
* []                       # (No input words).
* ["ABC"]
* ["ABC", "DEF"]
* ["ABC", "DEF", "G", "H"]



Note: Assume words are non-empty strings of uppercase characters for this task.





## 11l


```11l
F quibble(words)
   R S words.len
      0
         ‘{}’
      1
         ‘{’words[0]‘}’
      E
         ‘{’words[0.<(len)-1].join(‘, ’)‘ and ’words.last‘}’

print(quibble([‘’] * 0))
print(quibble([‘ABC’]))
print(quibble([‘ABC’, ‘DEF’]))
print(quibble([‘ABC’, ‘DEF’, ‘G’, ‘H’]))
```

{{out}}

```txt

{}
{ABC}
{ABC and DEF}
{ABC, DEF, G and H}

```



## 360 Assembly


```360asm
*        Comma quibbling           13/03/2017
COMMAQUI CSECT
         USING  COMMAQUI,R13       base register
         B      72(R15)            skip savearea
         DC     17F'0'             savearea
         STM    R14,R12,12(R13)    save previous context
         ST     R13,4(R15)         link backward
         ST     R15,8(R13)         link forward
         LR     R13,R15            set addressability
         LA     R6,1               i=1
       DO WHILE=(C,R6,LE,=A(N))    do i=1 to hbound(t)
         LR     R1,R6                i
         SLA    R1,5                 *32
         LA     R2,T-32              @t(0)
         AR     R1,R2                @t(i)
         MVC    S1,0(R1)             s1=t(i)
         MVC    S2,=CL32'{'          s2='{'
         LA     R8,S2+1              s2ins=1
         MVC    I2,=F'0'             i2=0
         LA     R7,1                 j=1
       DO WHILE=(C,R7,LE,=A(L'T))    do j=1 to length(t)
         LA     R1,S1                  @s1
         BCTR   R1,0                   @s1-1
         AR     R1,R7                  @s1-1+j
         MVC    CJ,0(R1)               cj=mid(s1,j,1)
         CLI    CJ,C' '                if cj=' '
         BE     EXITJ                  then goto exitj
       IF CLI,CJ,EQ,C',' THEN          if cj="," then
         MVC    0(2,R8),=C', '           s2=s2||", "
         LA     R8,2(R8)                 s2ins=s2ins+2
         LR     R0,R8                    s2ins
         LA     R1,S2+1                  @s2+1
         SR     R0,R1                    len(s2)-1
         ST     R0,I2                    i2=len(s2)-1
       ELSE     ,                      else
         MVC    0(1,R8),CJ               s2=s2||cj
         LA     R8,1(R8)                 s2ins=s2ins+1
       ENDIF    ,                      endif
         LA     R7,1(R7)               j++
       ENDDO    ,                    enddo j
EXITJ    MVI    0(R8),C'}'           s2=s2||"}"
         LA     R8,1(R8)             s2ins=s2ins+1
         L      R0,I2                i2
       IF LTR,R0,NZ,R0 THEN          if i2<>0 then
         MVC    S2B,S2                 s2b=mid(s2,1,i2-1)
         LA     R1,S2B-1               @s2b-1
         A      R1,I2                  +i2
         MVC    0(5,R1),=C' and '      s2b||" and "
         LA     R1,5(R1)               +5
         LA     R2,S2+1                @s2+1
         A      R2,I2                  +i2
         LR     R3,R8                  s2ins
         LA     R0,S2+1                @s2+1
         SR     R3,R0                  s2ins-(@s2+1)
         S      R3,I2                  -i2
         BCTR   R3,0                   -1
         EX     R3,XMVC                s2b||=mid(s2,i2+2)
         MVC    S2,S2B     s2=mid(s2,1,i2-1)||" and "||mid(s2,i2+2)
       ENDIF    ,                    endif
         XPRNT  S2,L'S2              print s2
         LA     R6,1(R6)             i++
       ENDDO    ,                  enddo i
         L      R13,4(0,R13)       restore previous savearea pointer
         LM     R14,R12,12(R13)    restore previous context
         XR     R15,R15            rc=0
         BR     R14                exit
XMVC     MVC    0(0,R1),0(R2)      mvc @r1,@r2
N        EQU    (TEND-T)/L'T       items of t
T        DC     CL32' ',CL32'ABC',CL32'ABC,DEF',CL32'ABC,DEF,G,H'
TEND     DS     0C
I2       DS     F
S1       DS     CL(L'T)
S2       DS     CL(L'T)
S2B      DS     CL(L'T)
CJ       DS     CL1
         YREGS
         END    COMMAQUI
```

{{out}}

```txt

{}
{ABC}
{ABC and DEF}
{ABC, DEF, G and H}

```




## Ada


```Ada
with Ada.Text_IO, Ada.Command_Line; use Ada.Command_Line;

procedure Comma_Quibble is

begin
   case Argument_Count is
      when 0 => Ada.Text_IO.Put_Line("{}");
      when 1 => Ada.Text_IO.Put_Line("{" & Argument(1) & "}");
      when others =>
	 Ada.Text_IO.Put("{");
	 for I in 1 .. Argument_Count-2 loop
	    Ada.Text_IO.Put(Argument(I) & ", ");
	 end loop;
	 Ada.Text_IO.Put(Argument(Argument_Count-1) & " and " &
		         Argument(Argument_Count) & "}");
   end case;
end Comma_Quibble;
```


{{out}}


```txt
./comma_quibble
{}
./comma_quibble abc
{abc}
./comma_quibble abc def
{abc and def}
./comma_quibble abc def g h
{abc, def, g and h}
```



## ALGOL 68

{{works with|ALGOL 68G|Any - tested with release 2.8.win32}}

```algol68
# returns a string ( assumed to be of space-separated words ) with the words  #
# separated by ", ", except for the last which is separated from the rest by  #
# " and ". The list is enclosed by braces                                     #
PROC to list = ( STRING words ) STRING:
    BEGIN
        # count the number of words                                           #
        INT  word count := 0;
        BOOL in word    := FALSE;
        FOR char pos FROM LWB words TO UPB words
        DO
            IF NOT is upper( words[ char pos ] )
            THEN
                # not an upper-case letter, possibly a word has been ended    #
                in word := FALSE
            ELSE
                # not a delimitor, possibly the start of a word               #
                IF NOT in word
                THEN
                    # we are starting a new word                              #
                    word count +:= 1;
                    in word     := TRUE
                FI
            FI
        OD;

        # format the result                                                   #
        STRING result    := "{";
        in word          := FALSE;
        INT  word number := 0;
        FOR char pos FROM LWB words TO UPB words
        DO
            IF NOT is upper( words[ char pos ] )
            THEN
                # not an upper-case letter, possibly a word has been ended    #
                in word := FALSE
            ELSE
                # not a delimitor, possibly the start of a word               #
                IF NOT in word
                THEN
                    # we are starting a new word                              #
                    word number +:= 1;
                    in word      := TRUE;
                    IF word number > 1
                    THEN
                        # second or subsequent word - need a separator        #
                        result +:= IF word number = word count
                                   THEN # final word                          #
                                       " and "
                                   ELSE # non-final word                      #
                                       ", "
                                   FI
                    FI
                FI;
                # add the character to the result                             #
                result +:= words[ char pos ]
            FI
        OD;

        result + "}"
    END # to list # ;


    # procedure to test the to list PROC                                      #
    PROC test to list = ( STRING words ) VOID:
        print( ( ( words
                 + ": "
                 + to list( words )
                 )
               , newline
               )
             );

    # test the to list PROC                                                   #
    test to list( "" );
    test to list( "ABC" );
    test to list( "ABC DEF" );
    test to list( "ABC DEF G H" )
```

{{out}}

```txt
: {}
ABC: {ABC}
ABC DEF: {ABC and DEF}
ABC DEF G H: {ABC, DEF, G and H}

```



## ALGOL W


```algolw
begin

    % returns a list of the words contained in wordString, separated by ", ", %
    % except for the last which is separated from the rest by " and ".        %
    % The words are enclosed by braces                                        %
    string(256) procedure toList ( string(256) value  words ) ;
    begin
        string(256) list;
        integer     wordCount, wordNumber, listPos;
        logical     inWord;

        % returns true if ch is an upper-case letter, false otherwise         %
        %         assumes the letters are consecutive in the character set    %
        %         (as in ascii) would not be correct if the character set was %
        %         ebcdic (as in the original implementations of Algol W)      %
        logical procedure isUpper ( string(1) value ch ) ; ch >= "A" and ch <= "Z" ;

        % adds a character to the result                                      %
        procedure addChar( string(1) value ch ) ;
        begin
            list( listPos // 1 ) := ch;
            listPos := listPos + 1;
        end addChar ;

        % adds a string to the result                                        %
        procedure addString( string(256) value str
                           ; integer     value len
                           ) ;
            for strPos := 0 until len - 1 do addChar( str( strPos // 1 ) );

        % count the number of words                                           %

        wordCount := 0;
        inWord    := false;
        for charPos := 0 until 255
        do begin
            if isUpper( words( charPos // 1 ) ) then begin
                % not an upper-case letter, possibly a word has been ended    %
                inWord := false
                end
            else begin
                % not a delimitor, possibly the start of a word               %
                if not inWord then begin
                    % we are starting a new word                              %
                    wordCount := wordCount + 1;
                    inWord    := true
                end if_not_inWord
            end
        end for_charPos;

        % format the result                                                   %

        list       := "";
        listPos    := 0;
        inWord     := false;
        wordNumber := 0;

        addChar( "{" );

        for charPos := 0 until 255
        do begin
            if not isUpper( words( charPos // 1 ) ) then begin
                % not an upper-case letter, possibly a word has been ended    %
                inWord := false
                end
            else begin
                % not a delimitor, possibly the start of a word               %
                if not inWord then begin
                    % we are starting a new word                              %
                    wordNumber := wordNumber + 1;
                    inWord     := true;
                    if wordNumber > 1 then begin
                        % second or subsequent word - need a separator        %
                        if wordNumber = wordCount then addString( " and ", 5 ) % final word %
                                                  else addString( ", ",    2 ) % non-final word %
                    end
                end;
                % add the character to the result                             %
                addChar( words( charPos // 1 ) )
            end
        end for_charPos ;

        addChar( "}" );

        list
    end toList ;


    % procedure to test the toList procedure                                 %
    procedure testToList ( string(256) value words ) ;
    begin
        string(256) list;
        list := toList( words );
        write( s_w := 0
             , words( 0 // 32 )
             , ": "
             , list(  0 // 32 )
             )
    end testToList ;

    % test the toList procedure                                              %
    testToList( "" );
    testToList( "ABC" );
    testToList( "ABC DEF" );
    testToList( "ABC DEF G H" );

end.
```

{{out}}

```txt

                                : {}
ABC                             : {ABC}
ABC DEF                         : {ABC and DEF}
ABC DEF G H                     : {ABC, DEF, G and H}
```




## AppleScript

{{Trans|JavaScript}}

```AppleScript
-- quibble :: [String] -> String
on quibble(xs)
    if length of xs > 1 then
        set applyCommas to ¬
            compose([curry(my intercalate)'s |λ|(", "), my |reverse|, my tail])

        intercalate(" and ", ap({applyCommas, my head}, {|reverse|(xs)}))
    else
        concat(xs)
    end if
end quibble

-- TEST -----------------------------------------------------------------------
on run
    script braces
        on |λ|(x)
            "{" & x & "}"
        end |λ|
    end script

    unlines(map(compose({braces, quibble}), ¬
        append({{}, {"ABC"}, {"ABC", "DEF"}, {"ABC", "DEF", "G", "H"}}, ¬
            map(|words|, ¬
                {"One two three four", "Me myself I", "Jack Jill", "Loner"}))))
end run


-- GENERIC FUNCTIONS ----------------------------------------------------------

-- A list of functions applied to a list of arguments
-- (<*> | ap) :: [(a -> b)] -> [a] -> [b]
on ap(fs, xs)
    set {intFs, intXs} to {length of fs, length of xs}
    set lst to {}
    repeat with i from 1 to intFs
        tell mReturn(item i of fs)
            repeat with j from 1 to intXs
                set end of lst to |λ|(contents of (item j of xs))
            end repeat
        end tell
    end repeat
    return lst
end ap

-- (++) :: [a] -> [a] -> [a]
on append(xs, ys)
    xs & ys
end append

-- compose :: [(a -> a)] -> (a -> a)
on compose(fs)
    script
        on |λ|(x)
            script
                on |λ|(a, f)
                    mReturn(f)'s |λ|(a)
                end |λ|
            end script

            foldr(result, x, fs)
        end |λ|
    end script
end compose

-- concat :: [[a]] -> [a] | [String] -> String
on concat(xs)
    script append
        on |λ|(a, b)
            a & b
        end |λ|
    end script

    if length of xs > 0 and class of (item 1 of xs) is string then
        set unit to ""
    else
        set unit to {}
    end if
    foldl(append, unit, xs)
end concat

-- curry :: (Script|Handler) -> Script
on curry(f)
    script
        on |λ|(a)
            script
                on |λ|(b)
                    |λ|(a, b) of mReturn(f)
                end |λ|
            end script
        end |λ|
    end script
end curry

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

-- head :: [a] -> a
on head(xs)
    if length of xs > 0 then
        item 1 of xs
    else
        missing value
    end if
end head

-- intercalate :: Text -> [Text] -> Text
on intercalate(strText, lstText)
    set {dlm, my text item delimiters} to {my text item delimiters, strText}
    set strJoined to lstText as text
    set my text item delimiters to dlm
    return strJoined
end intercalate

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

-- |reverse| :: [a] -> [a]
on |reverse|(xs)
    if class of xs is text then
        (reverse of characters of xs) as text
    else
        reverse of xs
    end if
end |reverse|

-- tail :: [a] -> [a]
on tail(xs)
    if length of xs > 1 then
        items 2 thru -1 of xs
    else
        {}
    end if
end tail

-- unlines :: [String] -> String
on unlines(xs)
    intercalate(linefeed, xs)
end unlines

-- words :: String -> [String]
on |words|(s)
    words of s
end |words|
```

{{Out}}

```txt
{}
{ABC}
{ABC and DEF}
{ABC, DEF, G and H}
{One, two, three and four}
{Me, myself and I}
{Jack and Jill}
{Loner}
```



## Arturo



```arturo
quibble [seq]{
	if $(size seq)=0 { return "{}" } {
		if $(size seq)=1 { return "{`seq.0`}" } {
			return "{" + $(join $(slice seq 0 $(size seq)-1) ", ") + " and " + $(last seq) + "}"
		}
	}
}

loop #(#() #("ABC") #("ABC" "DEF") #("ABC" "DEF" "G" "H")) {
	print $(quibble &)
}
```


{{out}}


```txt
{}
{ABC}
{ABC and DEF}
{ABC, DEF, G and H}
```



## Astro


```python
fun quibble(s):
    let result = s.join(' and ').replace(|| and ||, ", ", length(s) - 1)
    return "{ $result }"

let s = [
    []
    ["ABC"]
    ["ABC", "DEF"]
    ["ABC", "DEF", "G", "H"]
]

for i in s:
    print(quibble i)
```



## AutoHotkey


```autohotkey
MsgBox % quibble([])
MsgBox % quibble(["ABC"])
MsgBox % quibble(["ABC", "DEF"])
MsgBox % quibble(["ABC", "DEF", "G", "H"])

quibble(d) {
	s:=""
	for i, e in d
	{
		if (i<d.MaxIndex()-1)
			s:= s . e . ", "
		else if (i=d.MaxIndex()-1)
			s:= s . e . " and "
		else
			s:= s . e
	}
	return "{" . s . "}"
}
```

{{out}}

```txt
{}
{ABC}
{ABC and DEF}
{ABC, DEF, G and H}
```



## AWK


```awk
function quibble(a, n,    i, s) {
	for (i = 1; i < n - 1; i++) s = s a[i] ", "
	i = n - 1; if (i > 0) s = s a[i] " and "
	if (n > 0) s = s a[n]
	return "{" s "}"
}

BEGIN {
	print quibble(a, 0)
	n = split("ABC", b); print quibble(b, n)
	n = split("ABC DEF", c); print quibble(c, n)
	n = split("ABC DEF G H", d); print quibble(d, n)
}
```

{{out}}

```txt
{}
{ABC}
{ABC and DEF}
{ABC, DEF, G and H}
```



## Batch File


```dos
@echo off
setlocal enabledelayedexpansion

::THE MAIN THING...
echo.
set inp=[]
call :quibble
set inp=["ABC"]
call :quibble
set inp=["ABC","DEF"]
call :quibble
set inp=["ABC","DEF","G","H"]
call :quibble
echo.
pause
exit /b
::/THE MAIN THING...

::THE FUNCTION
:quibble
set cont=0
set proc=%inp:[=%
set proc=%proc:]=%

for %%x in (%proc%) do (
	set /a cont+=1
	set x=%%x
	set str!cont!=!x:"=!
)
set /a bef=%cont%-1
set output=%str1%
if %cont%==2 (set output=%str1% and %str2%)
if %cont% gtr 2 (
	for /l %%y in (2,1,%bef%) do (
		set output=!output!^, !str%%y!
	)
	set output=!output! and !str%cont%!
)
echo {!output!}
goto :EOF
::/THE FUNCTION
```

{{out}}

```txt
{}
{ABC}
{ABC and DEF}
{ABC, DEF, G and H}

Press any key to continue . . .
```



## Bracmat


```bracmat
( :?L1
& ABC:?L2
& ABC DEF:?L3
& ABC DEF G H:?L4
& L1 L2 L3 L4:?names
& ( quibble
  =   w
    .     !arg:%?w (% %:?arg)
        & !w ", " quibble$!arg
      | !arg:%?w %?arg&!w " and " quibble$!arg
      | !arg
  )
& (concat=.str$("{" quibble$!arg "}"))
&   whl
  ' (!names:%?name ?names&out$(!name concat$!!name))
);
```

{{out}}

```txt
L1 {}
L2 {ABC}
L3 {ABC and DEF}
L4 {ABC, DEF, G and H}
```



## C


```c
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

char *quib(const char **strs, size_t size)
{

    size_t len = 3 + ((size > 1) ? (2 * size + 1) : 0);
    size_t i;

    for (i = 0; i < size; i++)
        len += strlen(strs[i]);

    char *s = malloc(len * sizeof(*s));
    if (!s)
    {
        perror("Can't allocate memory!\n");
        exit(EXIT_FAILURE);
    }

    strcpy(s, "{");
    switch (size) {
        case 0:  break;
        case 1:  strcat(s, strs[0]);
                 break;
        default: for (i = 0; i < size - 1; i++)
                 {
                     strcat(s, strs[i]);
                     if (i < size - 2)
                         strcat(s, ", ");
                     else
                         strcat(s, " and ");
                 }
                 strcat(s, strs[i]);
                 break;
    }
    strcat(s, "}");
    return s;
}

int main(void)
{
    const char *test[] = {"ABC", "DEF", "G", "H"};
    char *s;

    for (size_t i = 0; i < 5; i++)
    {
        s = quib(test, i);
        printf("%s\n", s);
        free(s);
    }
    return EXIT_SUCCESS;
}
```

{{Out}}

```txt
{}
{ABC}
{ABC and DEF}
{ABC, DEF and G}
{ABC, DEF, G and H}
```


## C#

```c#
using System;
using System.Linq;

namespace CommaQuibbling
{
    internal static class Program
    {
        #region Static Members
	private static string Quibble(string[] input)
	{
            return
                String.Format("{{{0}}}",
                    String.Join("",
                        input.Reverse().Zip(
                            new [] { "", " and " }.Concat(Enumerable.Repeat(", ", int.MaxValue)),
                            (x, y) => x + y).Reverse()));
	}


        private static void Main()
        {
            Console.WriteLine( Quibble( new string[] {} ) );
            Console.WriteLine( Quibble( new[] {"ABC"} ) );
            Console.WriteLine( Quibble( new[] {"ABC", "DEF"} ) );
            Console.WriteLine( Quibble( new[] {"ABC", "DEF", "G", "H"} ) );

            Console.WriteLine( "< Press Any Key >" );
            Console.ReadKey();
        }

        #endregion
    }
}
```

{{out}}

```txt

{}
{ABC}
{ABC and DEF}
{ABC, DEF, G and H}
< Press Any Key >
```



## C++


```cpp
#include <iostream>

template<class T>
void quibble(std::ostream& o, T i, T e) {
  o << "{";
  if (e != i) {
    T n = i++;
    const char* more = "";
    while (e != i) {
      o << more << *n;
      more = ", ";
      n = i++;
    }
    o << (*more?" and ":"") << *n;
  }
  o << "}";
}

int main(int argc, char** argv) {
  char const* a[] = {"ABC","DEF","G","H"};
  for (int i=0; i<5; i++) {
    quibble(std::cout, a, a+i);
    std::cout << std::endl;
  }
  return 0;
}
```

{{out}}

```txt

{}
{ABC}
{ABC and DEF}
{ABC, DEF and G}
{ABC, DEF, G and H}

```



## Clojure


```clojure
(defn quibble [sq]
  (let [sep (if (pos? (count sq)) " and " "")]
    (apply str
      (concat "{" (interpose ", " (butlast sq)) [sep (last sq)] "}"))))

; Or, using clojure.pprint's cl-format, which implements common lisp's format:
(defn quibble-f [& args]
  (clojure.pprint/cl-format nil "{~{~a~#[~; and ~:;, ~]~}}" args))

(def test
  #(doseq [sq [[]
               ["ABC"]
               ["ABC", "DEF"]
               ["ABC", "DEF", "G", "H"]]]
     ((comp println %) sq)))

(test quibble)
(test quibble-f)
```

{{out}}

```txt
{}
{ABC}
{ABC and DEF}
{ABC, DEF, G and H}

```



## COBOL

{{works with|OpenCOBOL|2.0}}

```cobol>       >
SOURCE FORMAT IS FREE
IDENTIFICATION DIVISION.
PROGRAM-ID. comma-quibbling-test.

ENVIRONMENT DIVISION.
CONFIGURATION SECTION.
REPOSITORY.
    FUNCTION comma-quibbling
    .
DATA DIVISION.
WORKING-STORAGE SECTION.
01  strs-area.
    03  strs-len                  PIC 9.
    03  strs                      PIC X(5)
                                  OCCURS 0 TO 9 TIMES
                                  DEPENDING ON strs-len.

PROCEDURE DIVISION.
    MOVE "ABC" TO strs (1)
    MOVE "DEF" TO strs (2)
    MOVE "G" TO strs (3)
    MOVE "H" TO strs (4)

    PERFORM VARYING strs-len FROM 0 BY 1 UNTIL strs-len > 4
        DISPLAY FUNCTION comma-quibbling(strs-area)
    END-PERFORM
    .
END PROGRAM comma-quibbling-test.


IDENTIFICATION DIVISION.
FUNCTION-ID. comma-quibbling.

DATA DIVISION.
LOCAL-STORAGE SECTION.
01  i                             PIC 9.

01  num-extra-words               PIC 9.

LINKAGE SECTION.
01  strs-area.
    03  strs-len                  PIC 9.
    03  strs                      PIC X(5)
                                  OCCURS 0 TO 9 TIMES
                                  DEPENDING ON strs-len.

01  str                           PIC X(50).

PROCEDURE DIVISION USING strs-area RETURNING str.
    EVALUATE strs-len
        WHEN ZERO
            MOVE "{}" TO str
            GOBACK

        WHEN 1
            MOVE FUNCTION CONCATENATE("{", FUNCTION TRIM(strs (1)), "}")
                TO str
            GOBACK
    END-EVALUATE

    MOVE FUNCTION CONCATENATE(FUNCTION TRIM(strs (strs-len - 1)),
        " and ", FUNCTION TRIM(strs (strs-len)), "}")
        TO str

    IF strs-len > 2
        SUBTRACT 2 FROM strs-len GIVING num-extra-words
        PERFORM VARYING i FROM num-extra-words BY -1 UNTIL i = 0
            MOVE FUNCTION CONCATENATE(FUNCTION TRIM(strs (i)), ", ", str)
                TO str
        END-PERFORM
    END-IF

    MOVE FUNCTION CONCATENATE("{", str) TO str
    .
END FUNCTION comma-quibbling.
```


{{out}}

```txt

{}
{ABC}
{ABC and DEF}
{ABC, DEF and G}
{ABC, DEF, G and H}

```



## CoffeeScript


```coffeescript
quibble = ([most..., last]) ->
  '{' +
    (most.join ', ') +
    (if most.length then ' and ' else '')  +
    (last or '') +
  '}'

console.log quibble(s) for s in [ [], ["ABC"], ["ABC", "DEF"],
                                  ["ABC", "DEF", "G", "H" ]   ]

```

{{out}}

```txt
{}
{ABC}
{ABC and DEF}
{ABC, DEF, G and H}
```



## Common Lisp


```lisp

(defun quibble (&rest args)
  (format t "{~{~a~#[~; and ~:;, ~]~}}" args))

(quibble)
(quibble "ABC")
(quibble "ABC" "DEF")
(quibble "ABC" "DEF" "G" "H")

```

{{out}}

```txt
{}
{ABC}
{ABC and DEF}
{ABC, DEF, G and H}
```



## D


```d
import std.stdio, std.string;

string quibbler(in string[] seq) pure /*nothrow*/ {
    if (seq.length <= 1)
        return format("{%-(%s, %)}", seq);
    else
        return format("{%-(%s, %) and %s}", seq[0 .. $-1], seq[$-1]);
}

void main() {
    //foreach (immutable test; [[],
    foreach (const test; [[],
                          ["ABC"],
                          ["ABC", "DEF"],
                          ["ABC", "DEF", "G", "H"]])
        test.quibbler.writeln;
}
```

{{out}}

```txt
{}
{ABC}
{ABC and DEF}
{ABC, DEF, G and H}
```



### Alternative Version


```d
import std.stdio, std.string, std.algorithm, std.conv, std.array;

enum quibbler = (in string[] a) pure =>
    "{%-(%s and %)}".format(a.length < 2 ? a :
                            [a[0 .. $-1].join(", "), a.back]);

void main() {
    [[], ["ABC"], ["ABC", "DEF"], ["ABC", "DEF", "G", "H"]]
    .map!quibbler.writeln;
}
```

{{out}}

```txt
["{}", "{ABC}", "{ABC and DEF}", "{ABC, DEF, G and H}"]
```


## DCL


```DCL
$ list = "[]"
$ gosub comma_quibbling
$ write sys$output return_string
$
$ list = "[""ABC""]"
$ gosub comma_quibbling
$ write sys$output return_string
$
$ list = "[""ABC"", ""DEF""]"
$ gosub comma_quibbling
$ write sys$output return_string
$
$ list = "[""ABC"", ""DEF"", ""G"", ""H""]"
$ gosub comma_quibbling
$ write sys$output return_string
$
$ exit
$
$ comma_quibbling:
$ list = list - "[" - "]"
$ return_string = "{}"
$ if list .eqs. "" then $ return
$ return_string = "{" + f$element( 0, ",", list ) - """" - """"
$ if f$locate( ",", list ) .eq. f$length( list ) then $ goto done2
$ i = 1
$ loop:
$  word = f$element( i, ",", list ) - """" - """"
$  if word .eqs. "," then $ goto done1
$  return_string = return_string - "^" + "^," + word
$  i = i + 1
$  goto loop
$ done1:
$ return_string = f$element( 0, "^", return_string ) + " and" + ( f$element( 1, "^", return_string ) - "," )
$ done2:
$ return_string = return_string + "}"
$ return
```

{}out}}

```txt
$ @comma_quibbling
{}
{ABC}
{ABC and DEF}
{ABC, DEF, G and H}
```


=={{header|Déjà Vu}}==

```dejavu
comma-quibble lst:
	"}" )
	if lst:
		pop-from lst
		if lst:
			" and "
			pop-from lst
			for item in lst:
				item ", "
	concat( "{"

!. comma-quibble []
!. comma-quibble [ "ABC" ]
!. comma-quibble [ "ABC" "DEF" ]
!. comma-quibble [ "ABC" "DEF" "G" "H" ]
```

{{out}}

```txt
"{}"
"{ABC}"
"{ABC and DEF}"
"{ABC, DEF, G and H}"
```



## EchoLisp


```scheme

(lib 'match)

(define (quibble words)
    (match words
         [ null "{}"]
         [ (a)  (format "{ %a }" a)]
         [ (a b) (format "{ %a and %a }" a b)]
         [( a ... b c) (format "{ %a %a and %a }" (for/string ([w a]) (string-append w ", "))  b c)]
         [else 'bad-input]))


;; output

 (for ([t '(() ("ABC") ("ABC" "DEF") ("ABC" "DEF" "G" "H"))])
    (writeln t '----> (quibble t)))

null     ---->     "{}"
("ABC")     ---->     "{ ABC }"
("ABC" "DEF")     ---->     "{ ABC and DEF }"
("ABC" "DEF" "G" "H")     ---->     "{ ABC, DEF, G and H }"

```




## Eiffel


```Eiffel

class
	APPLICATION

create
	make

feature

		make
			-- Test of the feature comma_quibbling.
		local
			l: LINKED_LIST [STRING]
		do
			create l.make
			io.put_string (comma_quibbling (l) + "%N")
			l.extend ("ABC")
			io.put_string (comma_quibbling (l) + "%N")
			l.extend ("DEF")
			io.put_string (comma_quibbling (l) + "%N")
			l.extend ("G")
			l.extend ("H")
			io.put_string (comma_quibbling (l) + "%N")
		end

	comma_quibbling (l: LINKED_LIST [STRING]): STRING
			-- Elements of 'l' seperated by a comma or an and where appropriate.
		require
			l_not_void: l /= Void
		do
			create Result.make_empty
			Result.extend ('{')
			if l.is_empty then
				Result.append ("}")
			elseif l.count = 1 then
				Result.append (l [1] + "}")
			else
				Result.append (l [1])
				across
					2 |..| (l.count - 1) as c
				loop
					Result.append (", " + l [c.item])
				end
				Result.append (" and " + l [l.count] + "}")
			end
		end

end

```

{{out}}

```txt

{}
{ABC}
{ABC and DEF}
{ABC, DEF, G and H}

```



## Elixir

{{trans|Erlang}}

```elixir
defmodule RC do
  def generate( list ), do: "{#{ generate_content(list) }}"

  defp generate_content( [] ), do: ""
  defp generate_content( [x] ), do: x
  defp generate_content( [x1, x2] ), do: "#{x1} and #{x2}"
  defp generate_content( xs ) do
    [last, second_to_last | t] = Enum.reverse( xs )
    with_commas = for x <- t, do: x <> ","
    Enum.join(Enum.reverse([last, "and", second_to_last | with_commas]), " ")
  end
end

Enum.each([[], ["ABC"], ["ABC", "DEF"], ["ABC", "DEF", "G", "H"]], fn list ->
  IO.inspect RC.generate(list)
end)
```


{{out}}

```txt

"{}"
"{ABC}"
"{ABC and DEF}"
"{ABC, DEF, G and H}"

```



## Erlang

<lang>
-module( comma_quibbling ).

-export( [task/0] ).

task() -> [generate(X) || X <- [[], ["ABC"], ["ABC", "DEF"], ["ABC", "DEF", "G", "H"]]].



generate( List ) -> "{" ++ generate_content(List) ++ "}".

generate_content( [] ) -> "";
generate_content( [X] ) -> X;
generate_content( [X1, X2] ) -> string:join( [X1, "and", X2], " " );
generate_content( Xs ) ->
	[Last, Second_to_last | T] = lists:reverse( Xs ),
	With_commas = [X ++ "," || X <- T],
	string:join(lists:reverse([Last, "and", Second_to_last | With_commas]), " ").

```

{{out}}

```txt

36> comma_quibbling:task().
["{}","{ABC}","{ABC and DEF}","{ABC, DEF, G and H}"]

```



## F#


### One Way


```fsharp
let quibble list =
    let rec inner = function
        | [] -> ""
        | [x] -> x
        | [x;y] -> sprintf "%s and %s" x y
        | h::t -> sprintf "%s, %s" h (inner t)
    sprintf "{%s}" (inner list)

// test interactively
quibble []
quibble ["ABC"]
quibble ["ABC"; "DEF"]
quibble ["ABC"; "DEF"; "G"]
quibble ["ABC"; "DEF"; "G"; "H"]
```

Output from testing (in F# Interactive 3.0, Open Source version):
<lang>
> quibble [];;
val it : string = "{}"
> quibble ["ABC"];;
val it : string = "{ABC}"
> quibble ["ABC"; "DEF"];;
val it : string = "{ABC and DEF}"
> quibble ["ABC"; "DEF"; "G"];;
val it : string = "{ABC, DEF and G}"
> quibble ["ABC"; "DEF"; "G"; "H"];;
val it : string = "{ABC, DEF, G and H}"
```


### or Another


### =The Function=


```fsharp

let quibble quibbler quibblee = Seq.zip quibblee quibbler //Sorry, just too good a line to miss, back in my Latin classes

```


### =The Task=


```fsharp

let fN n = quibble (List.mapi(fun n _->match n with 0->"" |1-> " and " |_->", ") n |> List.rev) n
printf "{"; fN ["ABC"; "DEF"; "G"; "H"] |> Seq.iter(fun(n,g)->printf "%s%s" n g); printfn"}"
printf "{"; fN ["ABC"; "DEF"; "G"] |> Seq.iter(fun(n,g)->printf "%s%s" n g); printfn"}"
printf "{"; fN ["ABC"; "DEF"] |> Seq.iter(fun(n,g)->printf "%s%s" n g); printfn"}"
printf "{"; fN ["ABC"] |> Seq.iter(fun(n,g)->printf "%s%s" n g); printfn"}"
printf "{"; fN [] |> Seq.iter(fun(n,g)->printf "%s%s" n g); printfn"}"

```

{{out}}

```txt

{ABC, DEF, G and H}
{ABC, DEF and G}
{ABC and DEF}
{ABC}
{}

```



## Factor

This example uses the <code>inverse</code> vocabulary, which builds on the concept of invertible quotations as the basis for pattern matching. It is discussed at length in this approachable [http://micsymposium.org/mics_2009_proceedings/mics2009_submission_72.pdf paper].

```factor
USING: inverse qw sequences ;

: (quibble) ( seq -- seq' )
    {
        { [ { } ] [ "" ] }
        { [ 1array ] [ ] }
        { [ 2array ] [ " and " glue ] }
        [ unclip swap (quibble) ", " glue ]
    } switch ;

: quibble ( seq -- str ) (quibble) "{%s}" sprintf ;

{ } qw{ ABC } qw{ ABC DEF } qw{ ABC DEF G H }
[ quibble print ] 4 napply
```

{{out}}

```txt

{}
{ABC}
{ABC and DEF}
{ABC, DEF, G and H}

```



## Forth

Forth is a set of very low level routines (WORDs) that are concatenated to make higher level WORDs.
Programming Forth is like making a custom language for the problem.
Arguments are passed explicitly on the hardware stack.
As the program is written the language level goes higher.
This demonstration uses the Forth parser to break the input stream into separate strings and a string stack to collect the input strings. The string stack can also be read as an indexed array.

Stack comments show in/out arguments after a word executes.
Example: ( input -- output)

<lang>\ string primitives operate on addresses passed on the stack
: C+!      ( n addr -- )        dup >R  C@ +  R> C! ;                     \ increment a byte at addr by n
: APPEND   ( addr1 n addr2 -- ) 2DUP 2>R  COUNT +  SWAP MOVE 2R> C+! ;    \ append u bytes at addr1 to addr2
: PLACE    ( addr1 n addr2 -- )  2DUP 2>R  1+  SWAP  MOVE  2R> C! ;       \ copy n bytes at addr to addr2
: ,'       ( -- )               [CHAR] ' WORD  c@ 1+ ALLOT ALIGN ;        \ Parse input stream until ' and write into next
                                                                          \ available memory

\ use ,' to create some counted string literals with mnemonic names
create '"{}"' ( -- addr) ,' "{}"'                                         \ counted strings return the address of the 1st byte
create '"{'   ( -- addr) ,' "{'
create '}"'   ( -- addr) ,' }"'
create ','    ( -- addr) ,' , '
create 'and'  ( -- addr) ,'  and '
create "]     ( -- addr) ,' "]'

create null$ ( -- addr)  0 ,

HEX
\ build a string stack/array to hold input strings
100 constant ss-width                                                     \ string stack width
variable $DEPTH                                                           \ the string stack pointer

create $stack ( -- addr) 20 ss-width * allot

DECIMAL
: new:   ( -- )    1 $DEPTH +! ;                                          \ incr. string stack pointer
: ]stk$  ( ndx -- addr) ss-width * $stack + ;                             \ calc string stack element address from ndx
: TOP$   ( -- addr) $DEPTH @ ]stk$ ;                                      \ returns address of the top string on string stack
: collapse ( -- )     $DEPTH off ;                                        \ reset string stack pointer

\ used primitives to build counted string functions
: move$    ( $1 $2 -- ) >r COUNT R> PLACE ;                               \ copy $1 to $2
: push$    ( $ -- )     new: top$ move$ ;                                 \ push $ onto string stack
: +$       ( $1 $2 --  top$ ) swap push$ count TOP$ APPEND top$ ;         \ concatentate $2 to $1, Return result in TOP$
: LEN      ( $1 -- length)  c@ ;                                          \ char fetch the first byte returns the string length
: compare$ ( $1 $2 -- -n:0:n )  count rot count compare ;                 \ compare is an ANS Forth word. returns 0 if $1=$2
: =$       ( $1 $2 -- flag )    compare$ 0= ;
: [""]     ( -- )  null$  push$ ;                                         \ put a null string on the string stack

: ["                                                                      \ collects input strings onto string stack
           COLLAPSE
           begin
              bl word dup "] =$ not                                       \ parse input stream and terminate at "]
           while
              push$
           repeat
           drop
           $DEPTH @ 0= if [""] then ;                                      \ minimally leave a null string on the string stack


: ]stk$+   ( dest$ n -- top$)  ]stk$  +$  ;                                \ concatenate  n ]stk$ to DEST$

: writeln  ( $ -- )  cr count type collapse ;                              \ print string on new line and collapse string stack

\ write the solution with the new words
: 1-input    ( -- )
            1 ]stk$ LEN 0=                                                 \ check for empty string length
            if
                 '"{}"' writeln                                            \ return the null string output
            else
                 '"{'  push$                                               \ create a new string beginning with '{'
                 TOP$  1 ]stk$+ '}"' +$  writeln                           \ concatenate the pieces for 1 input

            then  ;

: 2-inputs ( -- )
           '"{'  push$
           TOP$  1 ]stk$+  'and' +$   2 ]stk$+  '}"' +$ writeln ;

: 3+inputs ( -- )
           $DEPTH @ dup >R                                                \ save copy of the number of inputs on the return stack
           '"{'  push$
           ( n) 1- 1                                                      \ loop indices for 1 to 2nd last string
           DO   TOP$  I ]stk$+  ',' +$   LOOP                             \ create all but the last 2 strings in a loop with comma
           ( -- top$) R@ 1- ]stk$+  'and' +$                              \ concatenate the 2nd last string to Top$ + 'and'
           R> ]stk$+  '}"' +$ writeln                                     \ use the copy of $DEPTH to get the final string index
           2drop ;                                                        \ clean the parameter stack

: quibble ( -- )
           $DEPTH @
           case
             1 of  1-input    endof
             2 of  2-inputs   endof
                   3+inputs                                               \ default case
           endcase ;


\ interpret this test code after including the above code
[""] QUIBBLE
[" "] QUIBBLE
[" ABC "] QUIBBLE
[" ABC DEF "] QUIBBLE
[" ABC DEF GHI BROWN FOX "] QUIBBLE

```

{{out}}

```txt
"{}"
"{}"
"{ABC}"
"{ABC and DEF}"
"{ABC, DEF, GHI, BROWN and FOX}" ok
```


Works with any ANS Forth

Needs the FMS-SI (single inheritance) library code located here:
http://soton.mpeforth.com/flag/fms/index.html

```forth
 include FMS-SI.f
include FMS-SILib.f

: foo { l | s -- }
  cr ." {"
  l size: dup 1- to s
    0 ?do
    i l at: p:
    s i - 1 >
     if ." , "
     else s i <> if ."  and " then
     then
    loop
  ." }" l <free ;

${ } foo
\ {}
${ ABC } foo
\ {ABC}
${ ABC DEF } foo
\ {ABC and DEF}
${ ABC DEF G } foo
\ {ABC, DEF and G}
${ ABC DEF G H } foo
\ {ABC, DEF, G and H}
${ ABC DEF G H I } foo
\ {ABC, DEF, G, H and I}

```



## Fortran

The usual problem of "How long is a piece of string?" is answered in the usual way with a declaration that is "surely long enough", at least for anticipated problems. Thus, variable TEXT is declared as 666 characters long. The input statement reads up to that number of characters, or the length of the record if shorter, and supplies trailing spaces to pad the recipient variable to its full length. There is unfortunately no read feature that will create a recipient storage area that matches the size of the record being read. There is such a facility in pl/i, except that the recipient variable still has a pre-specified upper bound to its size.

Subroutine QUIBBLE doesn't have to worry about this because it works with TEXT as a parameter, whatever its size (various integer limits apply) however, it too has the same problem because it locates the start and end positions of each word, and, how many words are going to be found? So once again, the arrays are made "surely large enough" for the expected class of problem. The first stage is to locate the words separated by any amount of "white space", which, thanks to the inability to rely on the evaluation of compound boolean expressions (of the form <code>IF (''in bounds'' & ''Array indexing'')</code>) in the "shortcut" manner, employs a battery of IF-statements. Fortran does not offer a data type "list of ..." so there is no prospect of placing the words into such an entity then inserting commas and "and" elements into the list to taste. Instead, the list of words is represented by a sequence of values in ordinary arrays.

The source style is Fortran 77, thus the use of COMMON to pass some I/O unit numbers. The plan initially was to engage in trickery with the variable FORMAT features, of the form <''expression''>(blah blah) to signify some number of repetitions of (blah blah), which number might be ''zero'', but alas, although <0>X works, it proved not to work for grouped items in place of a format code. So the <..> extension had to be abandoned, and plainer F77 results.
```Fortran
      SUBROUTINE QUIBBLE(TEXT,OXFORDIAN)	!Punctuates a list with commas and stuff.
       CHARACTER*(*) TEXT	!The text, delimited by spaces.
       LOGICAL OXFORDIAN	!Just so.
       INTEGER IST(6),LST(6)	!Start and stop positions.
       INTEGER N,L,I		!Counters.
       INTEGER L1,L2		!Fingers for the scan.
       INTEGER MSG		!Output unit.
       COMMON /IODEV/MSG	!Share.
Chop the text into words.
        N = 0		!No words found.
        L = LEN(TEXT)	!Multiple trailing spaces - no worries.
        L2 = 0		!Syncopation: where the previous chomp ended.
   10   L1 = L2		!Thus, where a fresh scan should follow.
   11   L1 = L1 + 1		!Advance one.
        IF (L1.GT.L) GO TO 20		!Finished yet?
        IF (TEXT(L1:L1).LE." ") GO TO 11	!No. Skip leading spaces.
        L2 = L1			!Righto, L1 is the first non-blank.
   12   L2 = L2 + 1		!Scan through the non-blanks.
        IF (L2.GT.L) GO TO 13	!Is it safe to look?
        IF (TEXT(L2:L2).GT." ") GO TO 12	!Yes. Speed through non-blanks.
   13   N = N + 1			!Righto, a word is found in TEXT(L1:L2 - 1)
        IST(N) = L1		!So, recall its first character.
        LST(N) = L2 - 1		!And its last.
        IF (L2.LT.L) GO TO 10	!Perhaps more text follows.
Comma time...
   20   WRITE (MSG,21) "{"	!Start the output.
   21   FORMAT (A,$)		!The $, obviously, specifies that the line is not finished.
        DO I = 1,N		!Step through the texts, there possibly being none.
          IF (I.GT.1) THEN		!If there has been a predecessor, supply separators.
            IF (I.LT.N) THEN			!Up to the last two, it's easy.
              WRITE (MSG,21) ", "			!Always just a comma.
            ELSE IF (OXFORDIAN) THEN		!But after the penultimate item, what?
              WRITE (MSG,21) ", and "			!Supply the comma omitted above: a double-power separator.
            ELSE				!One fewer comma, with possible ambiguity arising.
              WRITE (MSG,21) " and "			!A single separator.
            END IF				!So much for the style.
          END IF			!Enough with the separation.
          WRITE (MSG,21) TEXT(IST(I):LST(I))	!The text at last!
        END DO			!On to the next text.
        WRITE (MSG,"('}')")	!End the line, marking the end of the text.
      END		!That was fun.

      PROGRAM ENCOMMA	!Punctuate a list with commas.
      CHARACTER*(666) TEXT	!Holds the text. Easily long enough.
      INTEGER KBD,MSG,INF	!Now for some messing.
      COMMON /IODEV/MSG,KBD	!Pass the word.
      KBD = 5	!Standard input.
      MSG = 6	!Standard output.
      INF = 10	!Suitable for a disc file.
      OPEN (INF,FILE="List.txt",ACTION = "READ")	!Attach one.

   10 WRITE (MSG,11) "To insert commas into lists..."	!Announce.
   11 FORMAT (A)			!Just the text.
   12 READ (INF,11,END = 20) TEXT	!Grab the text, with trailing spaces to fill out TEXT.
      CALL QUIBBLE(TEXT,.FALSE.)	!One way to quibble.
      GO TO 12				!Try for another.

   20 REWIND (INF)			!Back to the start of the file.
      WRITE (MSG,11)			!Set off a bit.
      WRITE (MSG,11) "Oxford style..."	!Announce the proper style.
   21 READ (INF,11,END = 30) TEXT	!Grab the text.
      CALL QUIBBLE(TEXT,.TRUE.)		!The other way to quibble.
      GO TO 21				!Have another try.

Closedown
   30 END	!All files are closed by exiting.
```

Output:

```txt

To insert commas into lists...
{}
{ABC}
{ABC and DEF}
{ABC, DEF, G and H}

Oxford style...
{}
{ABC}
{ABC, and DEF}
{ABC, DEF, G, and H}

```



## FreeBASIC


```freebasic

' FB 1.05.0 Win64

Sub Split(s As String, sep As String, result() As String)
  Dim As Integer i, j, count = 0
  Dim temp As String
  Dim As Integer position(Len(s) + 1)
  position(0) = 0
  For i = 0 To Len(s) - 1
    For j = 0 To Len(sep) - 1
      If s[i] = sep[j] Then
        count += 1
        position(count) = i + 1
      End If
    Next j
  Next i
  position(count + 1) = Len(s) + 1
  Redim result(count)
  For i = 1 To count + 1
    result(i - 1) = Mid(s, position(i - 1) + 1, position(i) - position(i - 1) - 1)
  Next
End Sub

Function CommaQuibble(s As String) As String
  Dim i As Integer
  Dim As String result
  Dim As String words()
  s = Trim(s, Any "[]""")
  ' Now remove internal quotes
  Split s, """", words()
  s = ""
  For i = 0 To UBound(words)
     s &= words(i)
  Next
  ' Now split 's' using the comma as separator
  Erase words
  Split s, ",", words()
  ' And re-assemble the string in the desired format
  result = "{"
  For i = 0 To UBound(words)
    If i = 0 Then
      result &= words(i)
    ElseIf i = UBound(words) Then
      result &= " and " & words(i)
    Else
      result &= ", " + words(i)
    EndIf
  Next
  Return result & "}"
End Function

' As 3 of the strings contain embedded quotes these need to be doubled in FB
Print CommaQuibble("[]")
Print CommaQuibble("[""ABC""]")
Print CommaQuibble("[""ABC"",""DEF""]")
Print CommaQuibble("[""ABC"",""DEF"",""G"",""H""]")
Print
Print "Press any key to quit the program"
Sleep

```


{{out}}

```txt

{}
{ABC}
{ABC and DEF}
{ABC, DEF, G and H}

```



## Gambas

'''[https://gambas-playground.proko.eu/?gist=8edc63b206a1de50dd104cd12486ac03 Click this link to run this code]'''

```gambas
Public Sub Main()
Dim sInput As String[] = ["", "ABC", "ABC DEF", "ABC DEF G H"]
Dim sTemp As String

For Each sTemp In sInput
  Print sTemp & " = ";
  sTemp = Replace(sTemp, " ", ",")
  If RInStr(sTemp, ",") > 0 Then
    sTemp = Mid(sTemp, 1, RInStr(sTemp, ",") - 1) & " and " & Mid(sTemp, RInStr(sTemp, ",") + 1)
  End If
  sTemp = "{" & sTemp & "}"
  Print sTemp
Next

End
```

Output:

```txt

 = {}
ABC = {ABC}
ABC DEF = {ABC and DEF}
ABC DEF G H = {ABC,DEF,G and H}

```



## Go

The blog mentioned code maintenence.  The idea here is to make the code easy for maintainers to understand by making it correspond as directly as possible to the problem description.

```go
package main

import (
    "fmt"
    "strings"
)

func q(s []string) string {
    switch len(s) {
    case 0:
        return "{}"
    case 1:
        return "{" + s[0] + "}"
    case 2:
        return "{" + s[0] + " and " + s[1] + "}"
    default:
        return "{" +
            strings.Join(s[:len(s)-1], ", ") +
            " and " +
            s[len(s)-1] +
            "}"
    }
}

func main() {
    fmt.Println(q([]string{}))
    fmt.Println(q([]string{"ABC"}))
    fmt.Println(q([]string{"ABC", "DEF"}))
    fmt.Println(q([]string{"ABC", "DEF", "G", "H"}))
}
```

{{out}}

```txt

{}
{ABC}
{ABC and DEF}
{ABC, DEF, G and H}

```



## Groovy


```groovy
def commaQuibbling = { it.size() < 2 ? "{${it.join(', ')}}" : "{${it[0..-2].join(', ')} and ${it[-1]}}" }
```

'''Testing:'''

```groovy
['{}': [], '{ABC}': ['ABC'], '{ABC and DEF}': ['ABC', 'DEF'], '{ABC, DEF, G and H}': ['ABC', 'DEF', 'G', 'H']].each { expected, input ->
    println "Verifying commaQuibbling($input) == $expected"
    assert commaQuibbling(input) == expected
}
```

{{out}}

```txt
Verifying commaQuibbling([]) == {}
Verifying commaQuibbling([ABC]) == {ABC}
Verifying commaQuibbling([ABC, DEF]) == {ABC and DEF}
Verifying commaQuibbling([ABC, DEF, G, H]) == {ABC, DEF, G and H}
```



## Haskell


```haskell
quibble ws = "{" ++ quibbles ws ++ "}"
  where quibbles [] = ""
        quibbles [a] = a
        quibbles [a,b] = a ++ " and " ++ b
        quibbles (a:bs) = a ++ ", " ++ quibbles bs

main = mapM_ (putStrLn . quibble) $
  [[], ["ABC"], ["ABC", "DEF"], ["ABC", "DEF", "G", "H"]] ++
  (map words ["One two three four", "Me myself I", "Jack Jill", "Loner" ])

```

{{Out}}

```txt

{}
{ABC}
{ABC and DEF}
{ABC, DEF, G and H}
{One, two, three and four}
{Me, myself and I}
{Jack and Jill}
{Loner}

```


Or, defining just two cases, and drawing more on standard libraries than on hand-crafted pattern-matching and recursion:

```Haskell
import Data.List (intercalate)

quibble :: [String] -> String
quibble ws
  | length ws > 1 =
    intercalate
      " and "
      ([intercalate ", " . reverse . tail, head] <*> [reverse ws])
  | otherwise = concat ws

main :: IO ()
main =
  mapM_ (putStrLn . (`intercalate` ["{", "}"]) . quibble) $
  [[], ["ABC"], ["ABC", "DEF"], ["ABC", "DEF", "G", "H"]] ++
  (words <$> ["One two three four", "Me myself I", "Jack Jill", "Loner"])

```

{{Out}}

```txt
{}
{ABC}
{ABC and DEF}
{ABC, DEF, G and H}
{One, two, three and four}
{Me, myself and I}
{Jack and Jill}
{Loner}
```


=={{header|Icon}} and {{header|Unicon}}==

The following works in both languages:

```unicon
procedure main()
    every write(quibble([] | ["ABC"] | ["ABC","DEF"] | ["ABC","DEF","G","H"]))
end

procedure quibble(A)
    join := s := ""
    while s := pull(A)||join||s do join := if *join = 0 then " and " else ", "
    return "{"||s||"}"
end
```


Sample run:


```txt

->cq
{}
{ABC}
{ABC and DEF}
{ABC, DEF, G and H}
->

```



## J


```j
quibLast2=: ' and ' joinstring (2 -@<. #) {. ]
withoutLast2=: ([: # _2&}.) {. ]
quibble=: '{', '}' ,~ ', ' joinstring withoutLast2 , <@quibLast2
```


'''Testing:'''

```j
   Tests=: (<'');(<'ABC');('ABC';'DEF');<('ABC';'DEF';'G';'H')
   quibble every Tests
{}
{ABC}
{ABC and DEF}
{ABC, DEF, G and H}
```


Alternative implementation:


```j
commaand=: 1 ;@}.&, ] ,.~ 1 |.!.(<' and ') (<', ')"0
quibble=: '{','}',~ commaand
```


(same results)


## Java


```Java
public class Quibbler {

	public static String quibble(String[] words) {
		String qText = "{";
		for(int wIndex = 0; wIndex < words.length; wIndex++) {
			qText += words[wIndex] + (wIndex == words.length-1 ? "" :
						  wIndex == words.length-2 ? " and " :
						  ", ";
		}
		qText += "}";
		return qText;
	}

	public static void main(String[] args) {
		System.out.println(quibble(new String[]{}));
		System.out.println(quibble(new String[]{"ABC"}));
		System.out.println(quibble(new String[]{"ABC", "DEF"}));
		System.out.println(quibble(new String[]{"ABC", "DEF", "G"}));
		System.out.println(quibble(new String[]{"ABC", "DEF", "G", "H"}));
	}
}
```


{{out}}

```txt
{}
{ABC}
{ABC and DEF}
{ABC, DEF, G and H}

```



## JavaScript


### ES5


```javascript
function quibble(words) {
  return "{" +
    words.slice(0, words.length-1).join(",") +
   (words.length > 1 ? " and " : "") +
   (words[words.length-1] || '') +
  "}";
}

[[], ["ABC"], ["ABC", "DEF"], ["ABC", "DEF", "G", "H"]].forEach(
  function(s) {
    console.log(quibble(s));
  }
);
```

{{out}}

```txt
{}
{ABC}
{ABC and DEF}
{ABC,DEF,G and H}

```



### ES6

{{Trans|Haskell}}
Composing from a set of generic functions:

```JavaScript
(() => {
    'use strict';

    // COMMA QUIBBLING -------------------------------------------------------

    // quibble :: [String] -> String
    const quibble = xs =>
        (xs.length > 1) ? (
            intercalate(
                " and ",
                ap(
                    [compose([intercalate(", "), reverse, tail]), head], //
                    [reverse(xs)]
                )
            )
        ) : concat(xs);


    // GENERIC FUNCTIONS -----------------------------------------------------

    // A list of functions applied to a list of arguments
    // <*> :: [(a -> b)] -> [a] -> [b]
    const ap = (fs, xs) => //
        [].concat.apply([], fs.map(f => //
            [].concat.apply([], xs.map(x => [f(x)]))));

    // curry :: Function -> Function
    const curry = (f, ...args) => {
        const go = xs => xs.length >= f.length ? (f.apply(null, xs)) :
            function () {
                return go(xs.concat([].slice.apply(arguments)));
            };
        return go([].slice.call(args, 1));
    };

    // intercalate :: String -> [a] -> String
    const intercalate = curry((s, xs) => xs.join(s));

    // concat :: [[a]] -> [a] | [String] -> String
    const concat = xs => {
        if (xs.length > 0) {
            const unit = typeof xs[0] === 'string' ? '' : [];
            return unit.concat.apply(unit, xs);
        } else return [];
    };

    // compose :: [(a -> a)] -> (a -> a)
    const compose = fs => x => fs.reduceRight((a, f) => f(a), x);

    // map :: (a -> b) -> [a] -> [b]
    const map = curry((f, xs) => xs.map(f));

    // reverse :: [a] -> [a]
    const reverse = xs =>
        typeof xs === 'string' ? (
            xs.split('')
            .reverse()
            .join('')
        ) : xs.slice(0)
        .reverse();

    // head :: [a] -> a
    const head = xs => xs.length ? xs[0] : undefined;

    // tail :: [a] -> [a]
    const tail = xs => xs.length ? xs.slice(1) : undefined;

    // (++) :: [a] -> [a] -> [a]
    const append = (xs, ys) => xs.concat(ys);

    // words :: String -> [String]
    const words = s => s.split(/\s+/);

    // unlines :: [String] -> String
    const unlines = xs => xs.join('\n');


    // TEST ------------------------------------------------------------------
    return unlines(
        map(
            compose([x => '{' + x + '}', quibble]),
            append([
                [],
                ["ABC"],
                ["ABC", "DEF"],
                ["ABC", "DEF", "G", "H"]
            ], map(
                words, [
                    "One two three four", "Me myself I", "Jack Jill", "Loner"
                ]
            ))
        ));
})();
```

{{Out}}

```txt
{}
{ABC}
{ABC and DEF}
{ABC, DEF, G and H}
{One, two, three and four}
{Me, myself and I}
{Jack and Jill}
{Loner}
```



## jq

{{works with|jq|1.4}}

```jq
def quibble:
  if length == 0 then ""
  elif length == 1 then .[0]
  else (.[0:length-1] | join(", ")) + " and " + .[length-1]
  end
  | "{" + . + "}";
```

'''Example''':

```jq
( [], ["ABC"],  ["ABC", "DEF"],  ["ABC", "DEF", "G", "H"]) | quibble
```

{{Out}}

```sh
jq -n -r -f Comma_quibbling.jq
{}
{ABC}
{ABC and DEF}
{ABC, DEF, G and H}

```



## Julia

{{works with|Julia|0.6}}

```Julia
function quibble(arr::Array)
    if isempty(arr) rst = "" else rst = "$(arr[end])" end
    if length(arr) > 1 rst = join(arr[1:end-1], ", ") * " and " * rst end
    return "{" * rst * "}"
end

@show quibble([])
@show quibble(["ABC"])
@show quibble(["ABC", "DEF"])
@show quibble(["ABC", "DEF", "G", "H"])
```


{{Out}}

```txt
quibble([]) = "{}"
quibble(["ABC"]) = "{ABC}"
quibble(["ABC", "DEF"]) = "{ABC and DEF}"
quibble(["ABC", "DEF", "G", "H"]) = "{ABC, DEF, G and H}"
```



## Kotlin


```scala
// version 1.0.6

fun commaQuibble(s: String): String {
    val t = s.trim('[', ']').replace(" ", "").replace("\"", "")
    val words = t.split(',')
    val sb = StringBuilder("{")
    for (i in 0 until words.size) {
        sb.append(when (i) {
            0                -> ""
            words.lastIndex  -> " and "
            else             -> ", "
        })
        sb.append(words[i])
    }
    return sb.append("}").toString()
}

fun main(args: Array<String>) {
    val inputs = arrayOf(
        """[]""",
        """["ABC"]""",
        """["ABC", "DEF"]""",
        """["ABC", "DEF", "G", "H"]"""
    )
    for (input in inputs) println("${input.padEnd(24)}  ->  ${commaQuibble(input)}")
}
```


{{out}}

```txt

[]                        ->  {}
["ABC"]                   ->  {ABC}
["ABC", "DEF"]            ->  {ABC and DEF}
["ABC", "DEF", "G", "H"]  ->  {ABC, DEF, G and H}

```



## Lasso


```Lasso
#!/usr/bin/lasso9

local(collection =
	array(
		array,
		array("ABC"),
		array("ABC", "DEF"),
		array("ABC", "DEF", "G", "H")
	)
)

with words in #collection do {
	if(#words -> size > 1) => {
		local(last = #words -> last)
		#words -> removelast
		stdoutnl('{' + #words -> join(', ') + ' and ' + #last'}')
	else(#words -> size == 1)
		stdoutnl('{' + #words -> first + '}')
	else
		stdoutnl('{}')
	}

}
```


Output:

```txt
{}
{ABC}
{ABC and DEF}
{ABC, DEF, G and H}

```



## Liberty BASIC


```lb

do
        read in$
        if in$ ="END" then wait
        w =wordCount( in$)
        select case w
            case 0
                o$ ="{}"
            case 1
                o$ ="{" +in$ +"}"
            case 2
                o$ ="{" +word$( in$, 1) +" and " +word$( in$, 2) +"}"
            case else
                o$ ="{"
                o$ =o$ +word$( in$, 1)
                for k =2 to w -1
                    o$ =o$ +", " +word$( in$, k)
                next k
                o$ =o$ +" and " +word$( in$, w) +"}"
        end select
        if w =1 then
            print "'"; in$; "'"; " held "; w; " word. "; tab( 30); o$
        else
            print "'"; in$; "'"; " held "; w; " words. "; tab( 30); o$
        end if
    loop until 0

    wait

    function wordCount( IN$)
        wordCount =1
        for i =1 to len( IN$)
            if mid$( IN$, i, 1) =" " then wordCount =wordCount +1
        next i
    end function

    end

    data ""                 'No input words.
    data "ABC"              'One input word.
    data "ABC DEF"          'Two words.
    data "ABC DEF G"        'Three words.
    data "ABC DEF G H"      'Four words.

    data "END"              'Sentinel for EOD.

```

{{Out}}

```txt

'' held 1 word.              {}
'ABC' held 1 word.           {ABC}
'ABC DEF' held 2 words.      {ABC and DEF}
'ABC DEF G' held 3 words.    {ABC, DEF and G}
'ABC DEF G H' held 4 words.  {ABC, DEF, G and H}
```



## Logo


```Logo
to join :delimiter :list [:result []]
  output cond [
    [ [empty? :list]   :result ]
    [ [empty? :result] (join :delimiter butfirst :list first :list) ]
    [ else             (join :delimiter butfirst :list
                                        (word :result :delimiter first :list)) ]
  ]
end

to quibble :list
  local "length
  make "length count :list
  make "text (
    ifelse [:length <= 2] [
      (join "\ and\  :list)
    ] [
      (join "\ and\  (sentence join ",\  butlast :list last :list))
    ])
  output ifelse [empty? :text] "\{\} [(word "\{ :text "\})]
end

foreach [ [] [ABC] [ABC DEF] [ABC DEF G H] ] [
  print quibble ?
]

bye
```

{{out}}

```txt
{}
{ABC}
{ABC and DEF}
{ABC, DEF, G and H}

```



## Lua


```Lua
function quibble (strTab)
    local outString, join = "{"
    for strNum = 1, #strTab do
        if strNum == #strTab then
            join = ""
        elseif strNum == #strTab - 1 then
            join = " and "
        else
            join = ", "
        end
        outString = outString .. strTab[strNum] .. join
    end
    return outString .. '}'
end

local testCases = {
    {},
    {"ABC"},
    {"ABC", "DEF"},
    {"ABC", "DEF", "G", "H"}
}
for _, input in pairs(testCases) do print(quibble(input)) end
```

{{out}}

```txt
{}
{ABC}
{ABC and DEF}
{ABC, DEF, G and H}
```




## M2000 Interpreter


### Using string as argument


```M2000 Interpreter

Module Checkit {
      function f$ {
            what$=mid$(trim$(letter$),2)
            what$=Left$(what$, len(what$)-1)
            flush ' erase any argument from stack
            Data param$(what$)
            m=stack.size
            document resp$="{"
             if m>2 then {
                  shift m-1, 2    ' get last two as first two
                  push letter$+" and "+letter$
                  m--   ' one less
                  shiftback m   ' move to last position
            }
            while not empty {
                   resp$=letter$+if$(not empty->", ", "")
             }
            =resp$+"}"

      }
      \\ we use ? for Print
      ? f$({[]})
      ? f$({["ABC"]})
      ? f$({["ABC", "DEF"]})
      ? f$({["ABC","DEF", "G", "H"]})
}
Checkit

```



### Using String functions only



```M2000 Interpreter

Module Checkit {
      function f$ {
            what$=filter$(trim$(letter$), chr$(34))
            what$=Mid$(what$, 2, len(what$)-2)
            count=Len(what$)-Len(filter$(what$,","))
            if count>2 then m=rinstr(what$, ", ")  :  insert m, 2 what$=" and "
            ="{"+what$+"}"
      }
      ? f$({[]})
      ? f$({["ABC"]})
      ? f$({["ABC", "DEF"]})
      ? f$({["ABC","DEF", "G", "H"]})
}
Checkit

```



### Using array as argument


```M2000 Interpreter

Module Checkit {
      function f$(ar) {
            flush
            Data ! ar
            m=stack.size
            document resp$="{"
             if m>2 then {
                  shift m-1, 2    ' get last two as first two
                  push letter$+" and "+letter$
                  m--   ' one less
                  shiftback m   ' move to last position
            }
            while not empty {
                   resp$=letter$+if$(not empty->", ", "")
             }
            =resp$+"}"
      }
      ? f$((,))
      ? f$(("ABC",))
      ? f$(("ABC", "DEF"))
      ? f$(("ABC","DEF", "G", "H"))
}
Checkit

```

{{out}}
<pre >
{}
{ABC}
{ABC, DEF}
{ABC, DEF, G and H}
</pre >


## Maple


```Maple
Quibble := proc( los )
  uses  StringTools;
  Fence( proc()
        if los = [] then
          ""
        elif numelems( los ) = 1 then
          los[ 1 ]
        else
          cat( Join( los[ 1 .. -2 ], ", " ), " and ", los[ -1 ] )
        end if
  end(), "{", "}" )
end proc:
```


Check it on the required inputs:

```Maple>
 Quibble([]);
                                  "{}"

> Quibble( [ "ABC" ] );
                                "{ABC}"

> Quibble( [ "ABC", "DEF" ] );
                            "{ABC and DEF}"

> Quibble( ["ABC", "DEF", "G", "H"] );
                         "{ABC, DEF, G and H}"

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
quibble[words___] :=
    ToString@{StringJoin@@
        Replace[Riffle[{words}, ", "],
            {most__, ", ", last_} -> {most, " and ", last}]}
```

{{out}}

```txt

In[2]:= quibble[]
Out[2]= {}

In[3]:= quibble["ABC"]
Out[3]= {ABC}

In[4]:= quibble["ABC","DEF"]
Out[4]= {ABC and DEF}

In[5]:= quibble["ABC","DEF","G","H"]
Out[5]= {ABC, DEF, G and H}

```



## MAXScript


```MAXScript

fn separate words: =
(
	if words == unsupplied or words == undefined or classof words != array then return "{}"
		else
		(
			local toReturn = "{"
			local pos = 1
			while pos <= words.count do
			(
				if pos == 1 then (append toReturn words[pos]; pos+=1)
				else
				(
					if pos <= words.count-1 then (append toReturn (", "+words[pos]); pos+=1)
						else
						(
							append toReturn (" and " + words[pos])
							pos +=1
						)
				)
			)
			return (toReturn+"}")
		)
)

```

Output:

```MAXScript

separate words:#()
"{}"
separate words:#("ABC")
"{ABC}"
separate words:#("ABC","DEF")
"{ABC and DEF}"
separate words:#("ABC","DEF","G","H")
"{ABC, DEF, G and H}"

```



## NetRexx


```NetRexx
/* NetRexx */
options replace format comments java crossref symbols nobinary

runSample(arg)
return

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method quibble(arg) public static
  parse arg '[' lst ']'
  lst = lst.changestr('"', '').space(1)
  lc = lst.lastpos(',')
  if lc > 0 then
    lst = lst.insert('and', lc).overlay(' ', lc)
  return '{'lst'}'

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method runSample(arg) private static
  lists = ['[]', -                     -- {}
           '["ABC"]', -                -- {ABC}
           '["ABC", "DEF"]', -         -- {ABC and DEF}
           '["ABC", "DEF", "G", "H"]'] -- {ABC, DEF, G and H}
  loop lst over lists
    say lst.right(30) ':' quibble(lst)
    end lst
  return

```

{{out}}

```txt

                            [] : {}
                       ["ABC"] : {ABC}
                ["ABC", "DEF"] : {ABC and DEF}
      ["ABC", "DEF", "G", "H"] : {ABC, DEF, G and H}

```



## Nim


```nim
proc commaQuibble(s: openArray[string]): string =
  result = ""
  for i, c in s:
    if i > 0: result.add (if i < s.high: ", " else: " and ")
    result.add c
  result = "{" & result & "}"

var s = @[@[], @["ABC"], @["ABC", "DEF"], @["ABC", "DEF", "G", "H"]]
for i in s:
  echo commaQuibble(i)
```


=={{header|Oberon-2}}==
{{works with|oo2c}}

```oberon2

MODULE CommaQuibbling;
IMPORT
  NPCT:Args,
  Strings,
  Out;

VAR
  str: ARRAY 256 OF CHAR;

  PROCEDURE Do(VAR s: ARRAY OF CHAR);
  VAR
    aux: ARRAY 128 OF CHAR;
    i,params: LONGINT;
  BEGIN
    params := Args.Number() - 1;
    CASE params OF
       0:
        COPY("{}",s)
      |1:
        Args.At(1,aux);
        Strings.Append("{",s);
        Strings.Append(aux,s);
        Strings.Append("}",s);
      ELSE
        Strings.Append("{",s);
        FOR i := 1 TO params - 1 DO
          Args.At(i,aux);
          Strings.Append(aux,s);
          IF i # params - 1 THEN
            Strings.Append(", ",s)
          ELSE
            Strings.Append(" and ", s)
          END
        END;
        Args.At(params,aux);
        Strings.Append(aux,s);
        Strings.Append("}",s)
    END;

  END Do;

BEGIN
  Do(str);
  Out.String(":> ");Out.String(str);Out.Ln
END CommaQuibbling.

```

{{out}}

```txt

$ bin/CommaQuibbling
:> {}
$ bin/CommaQuibbling ABC
:> {ABC}
$ bin/CommaQuibbling ABC DEF
:> {ABC and DEF}
$ bin/CommaQuibbling ABC DEF G
:> {ABC, DEF and G}
$ bin/CommaQuibbling ABC DEF G H
:> {ABC, DEF, G and H}

```



## Objeck



```objeck
class Quibbler {
  function : Quibble(words : String[]) ~ String {
    text := "{";

    each(i : words) {
      text += words[i];
      if(i < words->Size() - 2) {
        text += ", ";
      }
      else if(i = words->Size() - 2) {
        text += " and ";
      };
    };
    text += "}";

    return text;
  }

  function : Main(args : String[]) ~ Nil {
    words := String->New[0];
    Quibble(words)->PrintLine();

    words := ["ABC"];
    Quibble(words)->PrintLine();

    words := ["ABC", "DEF"];
    Quibble(words)->PrintLine();

    words := ["ABC", "DEF", "G", "H"];
    Quibble(words)->PrintLine();
  }
}
```


Output:

```txt

{}
{ABC}
{ABC and DEF}
{ABC, DEF, G and H}

```



## OCaml



```ocaml
open Printf

let quibble list =
  let rec aux = function
    | a :: b :: c :: d :: rest -> a ^ ", " ^ aux (b :: c :: d :: rest)
    | [a; b; c] -> sprintf "%s, %s and %s}" a b c
    | [a; b] -> sprintf "%s and %s}" a b
    | [a] -> sprintf "%s}" a
    | [] -> "}" in
  "{" ^ aux list

let test () =
  [[];
   ["ABC"];
   ["ABC"; "DEF"];
   ["ABC"; "DEF"; "G"; "H"]]
  |> List.iter (fun list -> print_endline (quibble list))
```


{{works with|Core|v0.9.116.03+91}}

```ocaml
open Core

let quibble = function
  | [| |] -> "{}"
  | [| a |] -> sprintf "{%s}" a
  | array ->
    let last, rest = Array.last array, Array.slice array 0 (-1) in
    sprintf "{%s and %s}" (String.concat_array ~sep:", " rest) last

let test () =
  [[||];
   [|"ABC"|];
   [|"ABC"; "DEF"|];
   [|"ABC"; "DEF"; "G"; "H"|]]
  |> List.iter ~f:(fun list -> print_endline (quibble list))
```


{{out}}

```txt
# test ();;
{}
{ABC}
{ABC and DEF}
{ABC, DEF, G and H}

```



## Oforth



```Oforth
: quibbing(l) -- string
| i s |
   StringBuffer new "{" <<
   l size dup 1- ->s loop: i [
      l at(i) <<
      i s < ifTrue: [ ", " << continue ]
      i s == ifTrue: [ " and " << ]
      ]
   "}" << dup freeze ;
```


{{out}}

```txt

[ [], ["ABC"], ["ABC", "DEF"], ["ABC", "DEF", "G", "H"] ] map(#quibbing) .
[{}, {ABC}, {ABC and DEF}, {ABC, DEF, G and H}]

```



## Ol


```scheme

(define (quibble . args)
   (display "{")
   (let loop ((args args))
      (unless (null? args) (begin
         (display (car args))
         (cond
            ((= 1 (length args)) #t)
            ((= 2 (length args))
               (display " and "))
            (else
               (display ", ")))
         (loop (cdr args)))))
   (print "}"))

; testing =>
(quibble)
(quibble "ABC")
(quibble "ABC" "DEF")
(quibble "ABC" "DEF" "G" "H")

```


{{out}}

```txt

{}
{ABC}
{ABC and DEF}
{ABC, DEF, G and H}

```



## PARI/GP



```parigp
comma(v)={
  if(#v==0, return("{}"));
  if(#v==1, return(Str("{"v[1]"}")));
  my(s=Str("{",v[1]));
  for(i=2,#v-1,s=Str(s,", ",v[i]));
  Str(s," and ",v[#v],"}")
};
comma([])
comma(["ABC"])
comma(["ABC", "DEF"])
comma(["ABC", "DEF", "G", "H"])
```

Output:

```txt
%1 = "{}"
%2 = "{ABC}"
%3 = "{ABC and DEF}"
%4 = "{ABC, DEF, G and H}"
```



## Pascal


<Lang Pascal>
program CommaQuibbling;
uses Classes, StrUtils;

const OuterBracket=['[',']'];

type

  { TCommaQuibble }

  TCommaQuibble = class(TStringList)
  private
    function GetCommaquibble: string;
    procedure SetCommaQuibble(AValue: string);
  public
    property CommaQuibble: string read GetCommaquibble write SetCommaQuibble;
  end;

{ TCommaQuibble }

procedure TCommaQuibble.SetCommaQuibble(AValue: string);
begin
  AValue:=ExtractWord(1,AValue,OuterBracket);
  commatext:=Avalue;
end;

function TCommaQuibble.GetCommaquibble: string;
var x: Integer;
    Del: String;
begin
  result:='';
  Del:=', ';
  for x:=0 to Count-1 do
  begin
    result+=Strings[x];
    if x=Count-2 then Del:=' and '
    else if x=count-1 then Del:='';
    result+=del;
  end;
  result:='{'+result+'}';
end;

const TestData: array [0..7] of string=( '[]',
                                         '["ABC"]',
                                         '["ABC", "DEF"]',
                                         '["ABC", "DEF", "G", "H"]',
                                         '',
                                         '"ABC"',
                                         '"ABC", "DEF"',
                                         '"ABC", "DEF", "G", "H"');
var Quibble: TCommaQuibble;
    TestString: String;
begin
  Quibble:=TCommaQuibble.Create;

  for TestString in TestData do
  begin
    Quibble.CommaQuibble:=TestString;
    writeln(Quibble.CommaQuibble);
  end;

end.

</Lang>
Output:
<Pre>
{}
{ABC}
{ABC and DEF}
{ABC, DEF, G and H}
{}
{ABC}
{ABC and DEF}
{ABC, DEF, G and H}
</Pre>


## Perl

{{trans|Perl 6}}

```perl
sub comma_quibbling(@) {
    return "{$_}" for
        @_ < 2 ? "@_" :
        join(', ', @_[0..@_-2]) . ' and ' . $_[-1];
}

print comma_quibbling(@$_), "\n" for
    [], [qw(ABC)], [qw(ABC DEF)], [qw(ABC DEF G H)];
```

{{out}}

```txt
{}
{ABC}
{ABC and DEF}
{ABC, DEF, G and H}
```


'''Perl 5.01 version and other approach:'''


```perl
use 5.01;
sub comma_quibbling{
  my $last = pop // '';
  return '{'. (@_ ? (join ', ', @_).' and '.$last : $last).'}';
}

say for map {comma_quibbling(@$_)}
  [], [qw(ABC)], [qw(ABC DEF)], [qw(ABC DEF G H)];
```

{{out}}

```txt
{}
{ABC}
{ABC and DEF}
{ABC, DEF, G and H}
```



## Perl 6


```perl6
sub comma-quibbling(@A) {
    <{ }>.join: @A < 2 ?? @A !! "@A[0..*-2].join(', ') and @A[*-1]";
}

say comma-quibbling($_) for
    [], [<ABC>], [<ABC DEF>], [<ABC DEF G H>];
```

{{out}}

```txt
{}
{ABC}
{ABC and DEF}
{ABC, DEF, G and H}
```



## Phix


```Phix
function quibble(sequence words)
    if length(words)>=2 then
        words[-2..-1] = {words[-2]&" and "&words[-1]}
    end if
    return "{"&join(words,", ")&"}"
end function

constant tests = {{},
                  {"ABC"},
                  {"ABC","DEF"},
                  {"ABC","DEF","G","H"}}

for i=1 to length(tests) do
    ?quibble(tests[i])
end for
```

{{out}}

```txt

"{}"
"{ABC}"
"{ABC and DEF}"
"{ABC, DEF, G and H}"

```



## PHP



```php
<?php

function quibble($arr){

  $words = count($arr);

  if($words == 0){
    return '{}';
  }elseif($words == 1){
    return '{'.$arr[0].'}';
  }elseif($words == 2){
    return '{'.$arr[0].' and '.$arr[1].'}';
  }else{
    return '{'.implode(', ',  array_splice($arr, 0, -1) ). ' and '.$arr[0].'}';
  }

}


$tests = [
  [],
  ["ABC"],
  ["ABC", "DEF"],
  ["ABC", "DEF", "G", "H"]
];

foreach ($tests as $test) {
  echo quibble($test) . PHP_EOL;
}
```

{{out}}

```txt
{}
{ABC}
{ABC and DEF}
{ABC, DEF, G and H}
```



## PicoLisp


```PicoLisp
(for L '([] ["ABC"] ["ABC", "DEF"] ["ABC", "DEF", "G", "H"])
   (let H (head -1 L)
      (prinl
         "{"
         (glue ", " H)
         (and H " and ")
         (last L)
         "}" ) ) )
```

Output:

```txt
{}
{ABC}
{ABC and DEF}
{ABC, DEF, G and H}
```




## PL/I


```pli
*process or(!);
 quib: Proc Options(main);
 /*********************************************************************
 * 06.10.2013 Walter Pachl
 *********************************************************************/
   put Edit*process or(!);
 quib: Proc Options(main);
 /*********************************************************************
 * 06.10.2013 Walter Pachl
 * 07.10.2013 -"- change "Oxford comma" to and
 *********************************************************************/
   put Edit(quibbling(''))(Skip,a);
   put Edit(quibbling('ABC'))(Skip,a);
   put Edit(quibbling('ABC DEF'))(Skip,a);
   put Edit(quibbling('ABC DEF G H'))(Skip,a);
   return;

 quibbling: proc(s) Returns(Char(100) Var);
   Dcl s Char(*);
   Dcl result   Char(100) Var Init('');
   Dcl word(10) Char(100) Var;
   Dcl (wi,p) Bin Fixed(31);
   If s='' Then result='';
   Else Do;
     Do wi=1 By 1 While(s^='');
       p=index(s,' ');
       if p=0 Then Do;
         word(wi)=s;
         s='';
         End;
       Else Do;
         word(wi)=left(s,p-1);
         s=substr(s,p+1);
         End;
       end;
     wn=wi-1;
     result=word(1);
     Do i=2 To wn-1;
       result=result!!', '!!word(i);
       End;
     If wn>1 Then
       result=result!!' and '!!word(wn);
     End;
   Return('{'!!result!!'}');
   End;
 End;
```

{{out}}

```txt

{}
{ABC}
{ABC and DEF}
{ABC, DEF, G and H}

```



## PowerShell


```PowerShell

function Out-Quibble
{
    [OutputType([string])]
    Param
    (
        # Zero or more strings.
        [Parameter(Mandatory=$false, Position=0)]
        [AllowEmptyString()]
        [string[]]
        $Text = ""
    )

    # If not null or empty...
    if ($Text)
    {
        # Remove empty strings from the array.
        $text = "$Text".Split(" ", [StringSplitOptions]::RemoveEmptyEntries)
    }
    else
    {
        return "{}"
    }

    # Build a format string.
    $outStr = ""
    for ($i = 0; $i -lt $text.Count; $i++)
    {
        $outStr += "{$i}, "
    }
    $outStr = $outStr.TrimEnd(", ")

    # If more than one word, insert " and" at last comma position.
    if ($text.Count -gt 1)
    {
        $cIndex = $outStr.LastIndexOf(",")
        $outStr = $outStr.Remove($cIndex,1).Insert($cIndex," and")
    }

    # Output the formatted string.
    "{" + $outStr -f $text + "}"
}

```


```PowerShell

Out-Quibble
Out-Quibble "ABC"
Out-Quibble "ABC", "DEF"
Out-Quibble "ABC", "DEF", "G", "H"

```

{{Out}}

```txt

{}
{ABC}
{ABC and DEF}
{ABC, DEF, G and H}

```

What it might look like when working with a file:

```PowerShell

$file = @'

ABC
ABC, DEF
ABC, DEF, G, H
'@ -split [Environment]::NewLine

foreach ($line in $file)
{
    Out-Quibble -Text ($line -split ", ")
}

```

{{Out}}

```txt

{}
{ABC}
{ABC and DEF}
{ABC, DEF, G and H}

```



## Prolog


{{works with|SWI-Prolog|7.1}}


```prolog
words_series(Words, Bracketed) :-
    words_serialized(Words, Serialized),
    atomics_to_string(["{",Serialized,"}"], Bracketed).

words_serialized([], "").
words_serialized([Word], Word) :- !.
words_serialized(Words, Serialized) :-
    append(Rest, [Last], Words),                                  %% Splits the list of *Words* into the *Last* word and the *Rest*
    atomics_to_string(Rest, ", ", WithCommas),
    atomics_to_string([WithCommas, " and ", Last], Serialized).



test :-
    forall( member(Words, [[], ["ABC"], ["ABC", "DEF"], ["ABC", "DEF", "G", "H"]]),
            ( words_series(Words, Series),
              format('~w ~15|=> ~w~n', [Words, Series]))
          ).
```


{{Out}}

```prolog
?- test.
[]             => {}
[ABC]          => {ABC}
[ABC,DEF]      => {ABC and DEF}
[ABC,DEF,G,H]  => {ABC, DEF, G and H}
true.
```



## PureBasic


```PureBasic

EnableExplicit

Procedure.s CommaQuibble(Input$)
  Protected i, count
  Protected result$, word$
  Input$ = RemoveString(Input$, "[")
  Input$ = RemoveString(Input$, "]")
  Input$ = RemoveString(Input$, #DQUOTE$)
  count = CountString(Input$, ",") + 1
  result$ = "{"
  For i = 1 To count
    word$ = StringField(Input$, i, ",")
    If i = 1
      result$ + word$
    ElseIf Count = i
      result$ + " and " + word$
    Else
      result$ + ", " + word$
    EndIf
  Next
  ProcedureReturn result$ + "}"
EndProcedure

If OpenConsole()
  ; As 3 of the strings contain embedded quotes these need to be escaped with '\' and the whole string preceded by '~'
  PrintN(CommaQuibble("[]"))
  PrintN(CommaQuibble(~"[\"ABC\"]"))
  PrintN(CommaQuibble(~"[\"ABC\",\"DEF\"]"))
  PrintN(CommaQuibble(~"[\"ABC\",\"DEF\",\"G\",\"H\"]"))
  PrintN("")
  PrintN("Press any key to close the console")
  Repeat: Delay(10) : Until Inkey() <> ""
  CloseConsole()
EndIf

```


{{out}}

```txt

{}
{ABC}
{ABC and DEF}
{ABC, DEF, G and H}

```



## Python

===Python: Replace() whilst reversed===
replace(..) can only replace the first X occurrences not the last
hence the replace is done on the reverse of the intermediate string
then reversed back.

```python>>>
 def strcat(sequence):
    return '{%s}' % ', '.join(sequence)[::-1].replace(',', 'dna ', 1)[::-1]

>>> for seq in ([], ["ABC"], ["ABC", "DEF"], ["ABC", "DEF", "G", "H"]):
    print('Input: %-24r -> Output: %r' % (seq, strcat(seq)))


Input: []                       -> Output: '{}'
Input: ['ABC']                  -> Output: '{ABC}'
Input: ['ABC', 'DEF']           -> Output: '{ABC and DEF}'
Input: ['ABC', 'DEF', 'G', 'H'] -> Output: '{ABC, DEF, G and H}'
>>>
```



### Python: Counted replacement

(Possible){{trans|Tcl}}
replace() will replace nothing if the count of items to replace is zero, (and negative integer counts act to replace all occurrences).
This combines with the length of the input sequence to allow this to work:

```python
def commaQuibble(s):
    return '{%s}' % ' and '.join(s).replace(' and ', ', ', len(s) - 2)

for seq in ([], ["ABC"], ["ABC", "DEF"], ["ABC", "DEF", "G", "H"]):
	print('Input: %-24r -> Output: %r' % (seq, commaQuibble(seq)))
```


{{out}}

```txt
Input: []                       -> Output: '{}'
Input: ['ABC']                  -> Output: '{ABC}'
Input: ['ABC', 'DEF']           -> Output: '{ABC and DEF}'
Input: ['ABC', 'DEF', 'G', 'H'] -> Output: '{ABC, DEF, G and H}'
```



### Python: Functional


```python>>>
 def quibble(s):
    return ('{' +
                (', '.join(s[:-1]) + ' and ' if len(s) > 1 else '') +
	        (s[-1] if s else '') +
	    '}')

>>> for seq in ([], ["ABC"], ["ABC", "DEF"], ["ABC", "DEF", "G", "H"]):
	print('Input: %-24r -> Output: %r' % (seq, quibble(seq)))


Input: []                       -> Output: '{}'
Input: ['ABC']                  -> Output: '{ABC}'
Input: ['ABC', 'DEF']           -> Output: '{ABC and DEF}'
Input: ['ABC', 'DEF', 'G', 'H'] -> Output: '{ABC, DEF, G and H}'
>>>
```



## Racket


```Racket
(define (quibbling words)
  (define (sub-quibbling words)
    (match words
      ['() ""]
      [(list a) a]
      [(list a b) (format "~a and ~a" a b)]
      [(list a b ___) (format "~a, ~a" a (sub-quibbling b))]))
  (format "{~a}" (sub-quibbling words)))

(for ((input '([] ["ABC"] ["ABC" "DEF"] ["ABC" "DEF" "G" "H"])))
  (printf "~s\t->\t~a~%" input (quibbling input)))
```

{{out}}

```txt
()	->	{}
("ABC")	->	{ABC}
("ABC" "DEF")	->	{ABC and DEF}
("ABC" "DEF" "G" "H")	->	{ABC, DEF, G and H}
```



## REBOL


### Straightforward implementation


```REBOL
Rebol []

comma-quibbling: func [block] [
    rejoin [
        "^{"

        to-string use [s] [
            s: copy block
            s: next s
            forskip s 2 [insert s either tail? next s [" and "] [", "]]
            s: head s
        ]

        "^}"
    ]
]

foreach t [[] [ABC] [ABC DEF] [ABC DEF G H]] [print comma-quibbling t]

```

{{out}}

```txt
{}
{ABC}
{ABC and DEF}
{ABC, DEF, G and H}
```

===Alternative (more efficient) version with oxford comma switch===

```REBOL
Rebol []

; builds string instead of using an intermediate block

comma-quibbling: func [block /oxford /local s length] [
    length: length? block
    rejoin [
        "^{"

        either length < 2 [to-string block] [
            s: to-string block/1
            for n 2 (length - 1) 1 [repend s [", " pick block n]]
            if all [oxford (length > 2)] [append s ","]
            repend s [" and " last block]
        ]

        "^}"
    ]
]

test: [[] [ABC] [ABC DEF] [ABC DEF G H]]
foreach t test [print comma-quibbling t]
print "Now with Oxford comma"
foreach t test [print comma-quibbling/oxford t]

```

{{out}}

```txt
{}
{ABC}
{ABC and DEF}
{ABC, DEF, G and H}
Now with Oxford comma
{}
{ABC}
{ABC and DEF}
{ABC, DEF, G, and H}
```



## REXX


### version 1:


```rexx
say quibbling('')
say quibbling('ABC')
say quibbling('ABC DEF')
say quibbling('ABC DEF G H')
exit

quibbling: procedure
    parse arg list
    Select
      When list='' Then result=''
      When words(list)=1 then result=word(list,1)
      Otherwise result=translate(strip(subword(list,1,words(list)-1)),',',' '),
        'and' word(list,words(list))
      End
    Return '{'result'}'
```

{{out}}

```txt

{}
{ABC}
{ABC and DEF}
{ABC,DEF,G and H}

```


### version 2:


```rexx
say quibbling('')
say quibbling('ABC')
say quibbling('ABC DEF')
say quibbling('ABC DEF G H')
exit
quibbling:
  parse arg list
  If list='' Then result=''
  Else Do
    Do wi=1 By 1 while list<>''
      Parse Var list word.wi ' ' list
      End
    wn=wi-1
    result=word.1
    Do wi=2 To wn-1
      result=result', 'word.wi
      End
    If wn>1 Then
      result=result 'and' word.wn
    End
  Return '{'result'}'
```

{{output}}

```txt

{}
{ABC}
{ABC and DEF}
{ABC, DEF, G and H}

```


### version 3:

{{trans|NetRexx}}

```Rexx
/* Rexx */

i_ = 0
i_ = i_ + 1; lists.0 = i_; lists.i_ = '[]'
i_ = i_ + 1; lists.0 = i_; lists.i_ = '["ABC"]'
i_ = i_ + 1; lists.0 = i_; lists.i_ = '["ABC", ''DEF'']'
i_ = i_ + 1; lists.0 = i_; lists.i_ = '[ABC, DEF, G, H]'

say
do i_ = 1 to lists.0
  list = lists.i_
  say right(list, 30) ':' quibbling03(list)
  end i_
exit

quibbling03:
procedure
  parse arg '[' lst ']'
  lst = changestr('"', changestr("'", lst, ''), '') /* remove double & single quotes */
  lc = lastpos(',', lst)
  if lc > 0 then
    lst = overlay(' ', insert('and', lst, lc), lc)
  lst = space(lst, 1) -- remove extra spaces
  return '{'lst'}'
```

{{out}}

```txt

                            [] : {}
                       ["ABC"] : {ABC}
                ["ABC", 'DEF'] : {ABC and DEF}
              [ABC, DEF, G, H] : {ABC, DEF, G and H}

```



## Ring


```ring

# Project : Comma Quibbling

text = list(4)
text[1] = "{}"
text[2] = "ABC"
text[3] = "ABC,DEF"
text[4] = "ABC,DEF,G,H"
comma(text)

func comma(text)
       listtext = []
       for n = 1 to 4
            listtext = str2list(substr(text[n], ",", nl))
            if n = 2
               see "{" + list2str(listtext) + "}" + nl
               loop
            ok
            if len(listtext) = 1
               see "{}" + nl
               loop
            ok
            str = "{"
            for m = 1 to len(listtext)-1
                if len(listtext) = 2
                   str = str + listtext[m] + " "
                else
                   str = str + listtext[m] + ", "
                ok
            next
            if len(listtext) = 2
               str = left(str, len(str)-1)
            else
               str = left(str, len(str)-2)
            ok
            if len(listtext) = 2
               str = str + " " + listtext[len(listtext)] + "}"
            else
               str = str + " and " + listtext[len(listtext)] + "}"
            ok
            see str + nl
     next

```

Output:

```txt

{}
{ABC}
{ABC DEF}
{ABC, DEF, G and H}

```



## Ruby

{{trans|Perl 6}}

```ruby
def comma_quibbling(a)
  %w<{ }>.join(a.length < 2 ? a.first :
               "#{a[0..-2].join(', ')} and #{a[-1]}")
end

[[], %w<ABC>, %w<ABC DEF>, %w<ABC DEF G H>].each do |a|
  puts comma_quibbling(a)
end
```

{{out}}

```txt
{}
{ABC}
{ABC and DEF}
{ABC, DEF, G and H}
```




## Run BASIC


```runbasic
wrds$ = "[]
[""ABC""]
[""ABC"", ""DEF""]
[""ABC"", ""DEF"", ""G"", ""H""]
"
while  word$(wrds$,j+1,chr$(13)) <> ""
  a$ = word$(wrds$,j+1,chr$(13))
  print a$;" ==> ";
  a$ = "{"+mid$(a$,2,len(a$)-2)+"}"
  j = j + 1
  for i = len(a$) to 1 step -1
    if mid$(a$,i,1) = "," then
       a$ =  left$(a$,i-1) + " and " + mid$(a$,i+2)
       exit for
    end if
  next i
  print a$
WEND
```

{{out}}

```txt
[] ==> {}
["ABC"] ==> {"ABC"}
["ABC", "DEF"] ==> {"ABC" and  "DEF"}
["ABC", "DEF", "G", "H"] ==> {"ABC", "DEF", "G" and  "H"}
```



## Rust


```rust

fn quibble(seq: &[&str]) -> String {
    match seq.len() {
        0 => "{}".to_string(),
        1 => format!("{{{}}}", seq[0]),
        _ => {
            format!("{{{} and {}}}",
                    seq[..seq.len() - 1].join(", "),
                    seq.last().unwrap())
        }
    }
}

fn main() {
    println!("{}", quibble(&[]));
    println!("{}", quibble(&["ABC"]));
    println!("{}", quibble(&["ABC", "DEF"]));
    println!("{}", quibble(&["ABC", "DEF", "G", "H"]));
}

```

{{out}}

```txt

{}
{ABC}
{ABC and DEF}
{ABC, DEF, G and H}

```



## Scala


```scala
def quibble( s:List[String] ) = s match {
  case m if m.isEmpty => "{}"
  case m if m.length < 3 => m.mkString("{", " and ", "}")
  case m => "{" + m.init.mkString(", ") + " and " + m.last + "}"
}

// A little test...
{
  println( quibble( List() ) )
  println( quibble( List("ABC") ) )
  println( quibble( List("ABC","DEF") ) )
  println( quibble( List("ABC","DEF","G","H") ) )
}
```

{{out}}

```txt
{}
{ABC}
{ABC and DEF}
{ABC, DEF, G and H}
```



## Scheme



```scheme

(define (quibble . args)
  (display "{")
  (do ((rem args (cdr rem)))
    ((null? rem) (display "}\n"))
    (display (car rem))
    (cond ((= 1 (length rem)) )
          ((= 2 (length rem))
           (display " and "))
          (else
            (display ", ")))))

(quibble)
(quibble "ABC")
(quibble "ABC" "DEF")
(quibble "ABC" "DEF" "G" "H")

```


{{out}}

```txt

{}
{ABC}
{ABC and DEF}
{ABC, DEF, G and H}

```



## Sed

script-file:

```sed
s/#.*$//g
y/[/{/
y/]/}/
s/"//g
s/ [A-Z][A-Z]*}/ and&/g
s/, and/ and/
```

test.txt:

```text
[] # (No input words).
["ABC"]
["ABC", "DEF"]
["ABC", "DEF", "G", "H"]
```

sed -f script-file test.txt
{{out}}

```txt
{}
{ABC}
{ABC and DEF}
{ABC, DEF, G and H}
```



## Seed7


```Seed7
$ include "seed7_05.s7i";

const func string: quibble (in array string: input) is func
  result
    var string: quibble is "{";
  begin
    case length(input) of
      when {0}:  quibble &:= "}";
      when {1}:  quibble &:= input[1] & "}";
      otherwise: quibble &:= join(input[.. pred(length(input))], ", ") &
                             " and " & input[length(input)] & "}";
    end case;
  end func;

const proc: main is func
  begin
    writeln(quibble(0 times ""));
    writeln(quibble([] ("ABC")));
    writeln(quibble([] ("ABC", "DEF")));
    writeln(quibble([] ("ABC", "DEF", "G", "H")));
  end func;
```


{{out}}

```txt

{}
{ABC}
{ABC and DEF}
{ABC, DEF, G and H}

```



## Sidef


```ruby
func comma_quibbling(words) {
    '{' + ([words.ft(0, -2).join(', ')]-[''] + [words.last] -> join(' and ')) + '}';
}

[<>, <ABC>, <ABC DEF>, <ABC DEF G H>].each { |w|
    say comma_quibbling(w);
}
```

{{out}}

```txt
{}
{ABC}
{ABC and DEF}
{ABC, DEF, G and H}
```



## Standard ML


```sml
local
  fun quib []      = ""
    | quib [x]     = x
    | quib [x0,x1] = x0 ^ " and " ^ x1
    | quib (x::xs) = x ^ ", " ^ quib xs
in
  fun quibble xs = "{" ^ quib xs ^ "}"
end

(* Tests: *)
val t_quibble_0 = quibble [] = "{}"
val t_quibble_1 = quibble ["ABC"] = "{ABC}"
val t_quibble_2 = quibble ["ABC", "DEF"] = "{ABC and DEF}"
val t_quibble_3 = quibble ["ABC", "DEF", "G", "H"] = "{ABC, DEF, G and H}"

```


## Swift


```Swift
let inputs = [[], ["ABC"], ["ABC", "DEF"], ["ABC", "DEF", "G", "H"]]

func quibbling(var words:[String]) {
    if words.count == 0 {
        println("{}")
    } else if words.count == 1 {
        println("{\(words[0])}")
    } else if words.count == 2 {
        println("{\(words[0]) and \(words[1])}")
    } else {
        var output = "{"
        while words.count != 2 {
            output += words.removeAtIndex(0) + ", "
        }
        output += "\(words.removeAtIndex(0)) and \(words.removeAtIndex(0))}"

        println(output)
    }
}

for word in inputs {
    quibbling(word)
}
```

{{out}}

```txt

{}
{ABC}
{ABC and DEF}
{ABC, DEF, G and H}
```



## Tcl


```tcl
proc commaQuibble {lst} {
    return \{[join [lreplace $lst end-1 end [join [lrange $lst end-1 end] " and "]] ", "]\}
}

foreach input { {} {"ABC"} {"ABC" "DEF"} {"ABC" "DEF" "G" "H"} } {
    puts [commaQuibble $input]
}
```

{{out}}

```txt

{}
{ABC}
{ABC and DEF}
{ABC, DEF, G and H}

```



## TXR



```txrlisp
(defun quib (list)
  (tree-bind (: last . lead) (reverse list)
    `{@{(nreverse lead) ", "}@(if lead " and ")@last}`))
```



## UNIX Shell

{{trans|AWK}}

```bash
quibble() {
	# Here awk(1) is easier than sed(1).
	awk 'BEGIN {
		for (i = 1; i < ARGC - 2; i++) s = s ARGV[i] ", "
		i = ARGC - 2; if (i > 0) s = s ARGV[i] " and "
		i = ARGC - 1; if (i > 0) s = s ARGV[i]
		printf "{%s}\n", s
		exit 0
	}' "$@"
}

quibble
quibble ABC
quibble ABC DEF
quibble ABC DEF G H
```

{{out}}

```txt
{}
{ABC}
{ABC and DEF}
{ABC, DEF, G and H}
```



## VBA


```vb
Option Explicit

Sub Main()
   Debug.Print Quibbling("")
   Debug.Print Quibbling("ABC")
   Debug.Print Quibbling("ABC, DEF")
   Debug.Print Quibbling("ABC, DEF, G, H")
   Debug.Print Quibbling("ABC, DEF, G, H, IJKLM, NO, PQRSTUV")
End Sub

Private Function Quibbling(MyString As String) As String
Dim s As String, n As Integer
   s = "{" & MyString & "}": n = InStrRev(s, ",")
   If n > 0 Then s = Left(s, n - 1) & " and " & Right(s, Len(s) - (n + 1))
   Quibbling = s
End Function
```

{{out}}

```txt
{}
{ABC}
{ABC and DEF}
{ABC, DEF, G and H}
{ABC, DEF, G, H, IJKLM, NO and PQRSTUV}
```



## VBScript


```vb
Function Quibble(s)
	arr = Split(s,",")
	If s = "" Then
		Quibble = "{}"
	ElseIf UBound(arr) = 0 Then
		Quibble = "{" & arr(0) & "}"
	Else
		Quibble = "{"
		For i = 0 To UBound(arr)
			If i = UBound(arr) - 1 Then
				Quibble = Quibble & arr(i) & " and " & arr(i + 1) & "}"
				Exit For
			Else
				Quibble = Quibble & arr(i) & ", "
			End If
		Next
	End If
End Function

WScript.StdOut.Write Quibble("")
WScript.StdOut.WriteLine
WScript.StdOut.Write Quibble("ABC")
WScript.StdOut.WriteLine
WScript.StdOut.Write Quibble("ABC,DEF")
WScript.StdOut.WriteLine
WScript.StdOut.Write Quibble("ABC,DEF,G,H")
WScript.StdOut.WriteLine
```

{{out}}

```txt
{}
{ABC}
{ABC and DEF}
{ABC, DEF, G and H}
```



## Visual Basic .NET


FormatEnumerable() accepts an IEnumerable(Of String), as per Lippert's original specification. FormatArray() contains an alternative implementation for String().


```vbnet
Option Explicit On
Option Infer On
Option Strict On

Module Program
    Function FormatEnumerable(source As IEnumerable(Of String)) As String
        Dim res As New Text.StringBuilder("{")

        Using en = source.GetEnumerator()
            Dim moreThanOne As Boolean = False
            Dim nxt = If(en.MoveNext(), en.Current, String.Empty)

            Do While en.MoveNext()
                If moreThanOne Then res.Append(", ")
                moreThanOne = True

                res.Append(nxt)
                nxt = en.Current
            Loop

            Dim lastItem = If(moreThanOne, " and ", "") & nxt
            Return res.ToString() & lastItem & "}"
        End Using
    End Function

    Function FormatArray(source As String()) As String
        Select Case source.Length
            Case 0 : Return "{}"
            Case 1 : Return "{" & source(0) & "}"
            Case Else : Return "{" & String.Join(", ", source.Take(source.Length - 1)) & " and " & source(source.Length - 1) & "}"
        End Select
    End Function

    Sub Main()
        Dim cases As String()() = {Array.Empty(Of String), New String() {"ABC"}, New String() {"ABC", "DEF"}, New String() {"ABC", "DEF", "G", "H"}}
        For Each c In cases
            Console.WriteLine(FormatArray(c))
            Console.WriteLine(FormatEnumerable(c))
        Next
    End Sub
End Module

```


{{out}}

```txt
{}
{}
{ABC}
{ABC}
{ABC and DEF}
{ABC and DEF}
{ABC, DEF, G and H}
{ABC, DEF, G and H}
```



## XLISP

I like the Oxford comma; but specifications are specifications. So this implementation produces the required output by default. It also, however, allows an optional <tt>OXFORD-COMMA</tt> parameter: pass a true value, and you won't find yourself saying things like "I want to thank my parents, Ayn Rand and God".

```lisp
(defun quibble (inputs &optional oxford-comma)
    (define final
        (if (and (caddr inputs) oxford-comma)
            ", and "
            " and " ) )
    (defun comma-quibble (words)
        (cond
            ((null words) "")
            ((null (cdr words)) (car words))
            (t (begin
                (string-append (car words)
                    (if (caddr words)
                        (string-append ", " (comma-quibble (cdr words)))
                        (string-append final (cadr words))) ) ) ) ) )
    (string-append "{" (comma-quibble inputs) "}") )

; test cases:
(print (quibble '())) ; empty list
(print (quibble '("ABC")))
(print (quibble '("ABC" "DEF")))
(print (quibble '("ABC" "DEF" "G" "H")))

(newline)

; test cases using the Oxford comma:
(print (quibble '() t))
(print (quibble '("ABC") t))
(print (quibble '("ABC" "DEF") t))
(print (quibble '("ABC" "DEF" "G" "H") t))
```

{{out}}

```txt
"{}"
"{ABC}"
"{ABC and DEF}"
"{ABC, DEF, G and H}"

"{}"
"{ABC}"
"{ABC and DEF}"
"{ABC, DEF, G, and H}"
```



## XPL0


```XPL0
include c:\cxpl\codes;

proc Quibble(N, S);
int  N, S;
int  I;
[ChOut(0, ^{);
for I:= 0 to N-1 do
    [Text(0, S(I));
    if I<N-2 then Text(0, ", ");
    if I=N-2 then Text(0, " and ");
    ];
ChOut(0, ^});
];

int I;
for I:= 0 to 4 do
    if I#3 then [Quibble(I, ["ABC", "DEF", "G", "H"]);  CrLf(0)]

```


{{out}}

```txt

{}
{ABC}
{ABC and DEF}
{ABC, DEF, G and H}

```



## zkl

This is a cheese ball solution that replies on no commas in the inputs

```zkl
fcn quib(list){ text:=("{"+list.toString(*)[2,-1]+"}").replace("\"","");
   if(list.len()<2) text;
   else{
      z:=(text=text.replace(",",", ")).rfind(",");
      String(text[0,z]," and ",text[z+2,*])
   }
}
```

List.toString("*") converts List(1,2,3) to "L(1,2,3)" with all elements; without the *, long lists are shortened to L(1,2,3,...)
{{out}}

```txt

quib(List)     //-->"{}"
quib(L("ABC")) //-->"{ABC}"
quib(L("ABC", "DEF")) //-->"{ABC and DEF}"
quib(L("ABC", "DEF", "G", "H")) //-->"{ABC, DEF, G and H}"

```



## ZX Spectrum Basic


```zxbasic
10 DATA 0
20 DATA 1,"ABC"
30 DATA 2,"ABC","DEF"
40 DATA 4,"ABC","DEF","G","H"
50 FOR n=10 TO 40 STEP 10
60 RESTORE n: GO SUB 1000
70 NEXT n
80 STOP
1000 REM quibble
1010 LET s$=""
1020 READ j
1030 IF j=0 THEN GO TO 1100
1040 FOR i=1 TO j
1050 READ a$
1060 LET s$=s$+a$
1070 IF (i+1)=j THEN LET s$=s$+" and ": GO TO 1090
1080 IF (i+1)<j THEN LET s$=s$+", "
1090 NEXT i
1100 PRINT "{";s$;"}"
1110 RETURN
```

