+++
title = "Longest string challenge"
description = ""
date = 2019-10-03T12:22:11Z
aliases = []
[extra]
id = 10309
[taxonomies]
categories = []
tags = []
+++

{{task|Programming Challenge}}

;Background:
This "longest string challenge" is inspired by a problem that used to be given to students learning Icon. Students were expected to try to solve the problem in Icon and another language with which the student was already familiar. The basic problem is quite simple; the challenge and fun part came through the introduction of restrictions. Experience has shown that the original restrictions required some adjustment to bring out the intent of the challenge and make it suitable for Rosetta Code.


;Basic problem statement
Write a program that reads lines from standard input and, upon end of file, writes the longest line to standard output.
If there are ties for the longest line, the program writes out all the lines that tie.
If there is no input, the program should produce no output.


;Task  
Implement a solution to the basic problem that adheres to the spirit of the restrictions (see below).

Describe how you circumvented or got around these 'restrictions' and met the 'spirit' of the challenge. Your supporting description may need to describe any challenges to interpreting the restrictions and how you made this interpretation. You should state any assumptions, warnings, or other relevant points. The central idea here is to make the task a bit more interesting by thinking outside of the box and perhaps by showing off the capabilities of your language in a creative way. Because there is potential for considerable variation between solutions, the description is key to helping others see what you've done.

This task is likely to encourage a variety of different types of solutions. They should be substantially different approaches.

Given the input:

```txt

a
bb
ccc
ddd
ee
f
ggg

```


the output should be (possibly rearranged):

```txt

ccc
ddd
ggg

```



;Original list of restrictions

# No comparison operators may be used.
# No arithmetic operations, such as addition and subtraction, may be used.
# The only datatypes you may use are integer and string. In particular, you may not use lists.
# Do not re-read the input file. Avoid using files as a replacement for lists (this restriction became apparent in the discussion).


;Intent of restrictions:
Because of the variety of languages on Rosetta Code and the wide variety of concepts used in them, there needs to be a bit of clarification and guidance here to get to the spirit of the challenge and the intent of the restrictions.

The basic problem can be solved very conventionally, but that's boring and pedestrian. The original intent here wasn't to unduly frustrate people with interpreting the restrictions, it was to get people to think outside of their particular box and have a bit of fun doing it.

The guiding principle here should be to be creative in demonstrating some of the capabilities of the programming language being used. If you need to bend the restrictions a bit, explain why and try to follow the intent. If you think you've implemented a 'cheat', call out the fragment yourself and ask readers if they can spot why. If you absolutely can't get around one of the restrictions, explain why in your description.

Now having said that, the restrictions require some elaboration.

* In general, the restrictions are meant to avoid the explicit use of these features.
* "No comparison operators may be used" - At some level there must be some test that allows the solution to get at the length and determine if one string is longer. Comparison operators, in particular any less/greater comparison should be avoided.  <u>Representing the length of any string as a number should also be avoided.</u> Various approaches allow for detecting the end of a string. Some of these involve implicitly using equal/not-equal; however, explicitly using equal/not-equal should be acceptable.
* "No arithmetic operations" - Again, at some level something may have to advance through the string. Often there are ways a language can do this implicitly advance a cursor or pointer without explicitly using a +, - , ++, --, add, subtract, etc.
* The datatype restrictions are amongst the most difficult to reinterpret. In the language of the original challenge strings are atomic datatypes and structured datatypes like lists are quite distinct and have many different operations that apply to them. This becomes a bit fuzzier with languages with a different programming paradigm. The intent would be to avoid using an easy structure to accumulate the longest strings and spit them out. There will be some natural reinterpretation here.


To make this a bit more concrete, here are a couple of specific examples:
In C, a string is an array of chars, so using a couple of arrays as strings is in the spirit while using a second array in a non-string like fashion would violate the intent.
In APL or J, arrays are the core of the language so ruling them out is unfair. Meeting the spirit will come down to how they are used.

Please keep in mind these are just examples and you may hit new territory finding a solution. There will be other cases like these. Explain your reasoning. You may want to open a discussion on the talk page as well.
* The added "No rereading" restriction is for practical reasons, re-reading stdin should be broken. I haven't outright banned the use of other files but I've discouraged them as it is basically another form of a list. Somewhere there may be a language that just sings when doing file manipulation and where that makes sense; however, for most there should be a way to accomplish without resorting to an externality.


At the end of the day for the implementer this should be a bit of fun. As an implementer you represent the expertise in your language, the reader may have no knowledge of your language. For the reader it should give them insight into how people think outside the box in other languages. Comments, especially for non-obvious (to the reader) bits will be extremely helpful. While the implementations may be a bit artificial in the context of this task, the general techniques may be useful elsewhere.



    

## Ada


This first solution is prepended to the following earlier solutions that did not fully comply with the restrictions, namely the use of arithmatic operators and language features that return numbers used with them.

In order to comply with the avoidance of greater-than or less-than comparisons and iterations with operators, only Constraint_Error exception handling is used to obtain comparison of less-than, equal-to, or greater-than.  With these, only the strings that are longer than previous strings and the succeeding strings, and equal in length to each other are printed.  They are printed in reverse order, but this is specifically allowed by the instructions.  In order to be clear to the reader, all cases of less-than, equal to, and greater-than are manually iterated through.  Since equal-to/not-equal-to testing is allowed, there should be no question that a "case" statement is also allowed.


```Ada
with Ada.Text_IO;

procedure Longest_Strings is
   use Ada.Text_IO;
   
   -- first, in order to strictly use integer, I use integer in
   -- place of an enumeration type: -1 => not-equal
   --                                0 => shorter - ignore, no print current string
   --                                1 => equal - print current and up-stream
   --                                2 => longer - no print upstream, only current and equal subsequent
   --                           others => null; -- must never happen.
   --
   -- Anything else that is tested or used that is not a string or integer
   -- is not used explicitly by me, but is a standard part of the language
   -- as provided in the standard libraries (like boolean "End_Of_File").
   
   function Measure_And_Print_N (O : String := ""; -- original/old string
                                 N : String := ""  -- next/new string
                                ) return Integer is
      T1 : String := O;
      T2 : String := N;
      L  : Integer := 1; -- Length defaults to the same;
      function Test_Length (O : in out String; -- original/old string
                            N : in out String) -- new/test-subject string
                            return Integer is
         function Test_Equal (O : in out String; N : in out String)
                              return Integer is
         begin
            O := N;
            return 1;
         exception
            when Constraint_Error =>
               return -1;
         end;
      begin
         case Test_Equal (O, N) is
         when -1 =>
            O (N'Range) := N;
            return 0;
         when 1 =>
            return 1;
         when others =>
            return -1;
         end case;
      exception
         when Constraint_Error =>
            return 2;
      end;
   begin
         case Test_Length (T1, T2) is
         when 0 =>
            
            -- N < O, so return "shorter"  do not print N
            
            if End_Of_File
            then
               return 0;
            else
               case Measure_And_Print_N (O, Get_Line) is
                  when 0 =>
                     return 0;
                  when 1 =>
                     return 0;
                  when 2 =>
                     return 2; -- carry up any subsequent canceling of print.
                  when others =>
                     raise Numeric_Error;
               end case;
            end if;
         when 1 =>
            
            -- O = N, so return "equal"  print N if all subsequent values are
            -- less than or equal to N
            
            if End_Of_File
            then
               Put_Line (N);
               return 1;
            else
               case Measure_And_Print_N (O, Get_Line) is
                  when 0 =>
                     Put_Line (N);
                     return 1;
                  when 1 =>
                     Put_Line (N);
                     return 1;
                  when 2 =>  -- carry up the subsequent canceling of print.
                     null;
                     return 2;
                  when others =>
                     raise Numeric_Error;
               end case;
            end if;
         when 2 =>
            
            -- N > O, so return "longer" to cancel printing all previous values
            -- and print N if it is also equal to or greater than descendant
            -- values.
            
            if End_Of_File
            then
               Put_Line (N);
               return 2;
            else
               case Measure_And_Print_N (N, Get_Line) is
                  when 0 =>
                     Put_Line (N);
                     return 2;
                  when 1 =>
                     Put_Line (N);
                     return 2;
                  when 2 =>  -- printing N cancelled by subsequent input.
                     null;
                     return 2;
                  when others =>
                     raise Numeric_Error;
               end case;
            end if;
         when others =>
            
            -- This should never happen - raise exception
            
            raise Numeric_Error;
         end case;
   end;
begin
   if End_Of_File
   then
      null;
   else
      case Measure_And_Print_N ("", Get_Line) is
         when 0 =>
            Put_Line (Current_Error,
                      "Error, Somehow the input line is calculated as less than zero!");
         when 1 =>
            Put_Line (Current_Error,
                      "All input lines appear to be blank.");
         when 2 =>
            null;
         when others =>
            raise Numeric_Error;
      end case;
   end if;
end;

```


The output, given the above example input, is:


```txt
ggg
ddd
ccc
```


Keeping the input in order means either appending to a long string with varying segments and a separater character, like below, or possible using a list feature that has been specifically disallowed.

Next follows previous attempts:

--------

How to bypass the restrictions:

* All lines of potential output are stored in an (unbounded) string, named Buffer. On special character (Latin_1.Nul) is used to separate between different lines. 

* We can't directly compare the lengths of two strings. So instead, we assign the difference of the lengths to a variable of type Natural (*). If the result is outside of Natural, this raises an exception. If there is no exception, we assign the result to a variable of type Positive (*), which raises an exception if the result is outside of Positive. 

--------
(*) Technically, Natural and Positive are not types but subtypes of Integer: Natural ranges from 0 to Integer'Last, Positive from 1 to Integer'Last. 


So this is the first solution. 


```Ada
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO, Ada.Characters.Latin_1;

procedure Longest_String_Challenge is
   function "+"(S: String) return Unbounded_String renames To_Unbounded_String;
   Separator: constant Character := Ada.Characters.Latin_1.NUL;

   procedure Funny_Stuff(B, L: in out Unbounded_String; N: Unbounded_String) is
      -- B holds a list of all longest strings, separated by Separator
      -- L holds longest string so far
      -- N is the next string to be considered 
      Nat: Natural;
   begin
      Nat := Length(N) - Length(L); 
        -- (1) this raises exception if L longer then N 
      declare
         Pos: Positive;
      begin
         Pos := Nat; -- (2) this raises exception if L at least as long as N
                     -- at this point, we know N is longer then L
         B   := N; 
         L   := N;
      exception
         when Constraint_Error -- come from (2)
            -- at this point, we know L and N are of the same length
            => B := B & Separator & N; -- add N to the set of solutions
      end;
   exception
      when Constraint_Error => null; -- come from (1)
        -- at this point, we know L is longer then N 
   end Funny_Stuff;

   Buffer: Unbounded_String := +"";
   Longest: Unbounded_String := +"";
   Next: Unbounded_String;

begin
   while True loop
      Next := + Ada.Text_IO.Get_Line;
        -- (3) raises exception when trying to read beyond the end of file
      Funny_Stuff(Buffer, Longest, Next);
   end loop;
exception
   when others => -- come from (3)
      for I in To_String(Buffer)'Range loop
         if To_String(Buffer)(I) = Separator then
            Ada.Text_IO.New_Line;
         else
            Ada.Text_IO.Put(To_String(Buffer)(I));
         end if;
      end loop;
end Longest_String_Challenge;
```


Output, when run with its own source code as the input:

```txt
   function "+"(S: String) return Unbounded_String renames To_Unbounded_String;
   procedure Funny_Stuff(B, L: in out Unbounded_String; N: Unbounded_String) is
```


Here is the second solution. It also makes heavy use of exceptions, but it does not require to compute the difference (which is an arithmetic operation, i.e., a bit of a cheat). Instead, the procedure Funny_Stuff carries some auxiliary strings S, T. If they are unequal and neither is empty, it recursively calls itself with the same strings shortened by 1. At some point of time, either S is empty, or T is empty, or both are empty.  


```Ada
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO, Ada.Characters.Latin_1;
 
procedure Longest_String_Challenge is
   function "+"(S: String) return Unbounded_String renames To_Unbounded_String;
   function "-"(U: Unbounded_String) return String renames To_String;
   Separator: constant Character := Ada.Characters.Latin_1.NUL;
 
   procedure Funny_Stuff(B, L: in out Unbounded_String;
                         N: Unbounded_String;
                         S, T: String) is
      C: Character;
   begin
      C:= T(T'First); -- (1) raises Constraint_Error if T is empty
      begin
	 C := S(S'First); -- (2) raises Constraint_Error if S is empty
	   -- at this point, we know that neither S nor T are empty
         Funny_Stuff(B,L,N,S(S'First+1 .. S'Last), T(T'First+1..T'Last)); 
      exception
         when Constraint_Error => -- come from (2), S is empty, T is not empty!
	    B   := N;
	    L   := N;
      end;
   exception
      when Constraint_Error => -- come from (1), T is empty
	 begin
	    C := S(S'First); -- (3) raises Constraint_Error if S is empty
	    -- at this point, we know that T is empty and S isn't
	    null;
	exception 
	    when Constraint_Error => -- come from (3); both S and T are empty
	    B := B & Separator & N;
	end;
   end Funny_Stuff;
 
   Buffer: Unbounded_String := +"";
   Longest: Unbounded_String := +"";
   Next: Unbounded_String;
 
begin
   while True loop
      Next := + Ada.Text_IO.Get_Line;
        -- (4) raises exception when trying to read beyond end of file
      Funny_Stuff(Buffer, Longest, Next, -Longest, -Next);
   end loop;
exception
   when others => -- come from (4)
      for I in To_String(Buffer)'Range loop
         if To_String(Buffer)(I) = Separator then
            Ada.Text_IO.New_Line;
         else
            Ada.Text_IO.Put(To_String(Buffer)(I));
         end if;
      end loop;
end Longest_String_Challenge;
```


The output, when given its own source code as the input:


```txt
   function "+"(S: String) return Unbounded_String renames To_Unbounded_String;
         when Constraint_Error => -- come from (2), S is empty, T is not empty!
```



## ALGOL 68


Empty loops are used for comparison for equality; STRING slicing is used to implement subtraction; slicing and concatenation implements addition.

The STRING 'buffer' holds all the input strings and separating newline characters.  A dummy zero-length string is tacked on the end so that the final loop terminates cleanly.   The start of the STRING 'mask' is filled in with "1" characters at the position of the terminal character in each input string, overwriting any "0" that may have been there.  A final scan of 'mask' identifies the maximum string length seen on the input.

The 'char in string' function, here used to slice up the buffer into the original inputs, is an Algol 68 Genie extension.  The 'Pedantry' comment draws attention to the explicit voiding of the function result; not strictly necessary but it avoids a complaint if the --pedantic option is given to a68g.

Warts are that all input strings must be shorter than (bound -1) characters and it is assumed that ABS "1" > ABS "0"; this true for every known implementation of Algol 68.

```algol68

BEGIN
   INT bound = 1000000;			 CO Arbitrary upper limit on string lengths CO
   INT max;				 CO Length of longest string CO
   INT len;				 CO Length of string under examination CO
   STRING buffer := "";			 CO All characters read from stand in CO
   STRING mask := bound * "0";		 CO High water mark of string length seen so far CO
CO Standard boiler plate CO
   on file end (stand in, (REF FILE f) BOOL: (close (f); GOTO finished));
   DO
      STRING line;
      read ((line, newline));
      buffer PLUSAB line + REPR 10;	CO Concatenate string and newline CO
      mask[UPB line] := "1"		CO And set mask where character exists in line CO
   OD;
finished:
   buffer PLUSAB REPR 10;		CO Guarantee there's a zero-length string at the end CO
CO
   Scan backwards through mask looking for highest index used which is equal to the length
   of the longest string with its terminating newline.
CO
   FOR i FROM bound BY -1 TO 1
   DO
      FROM ABS mask[i] TO ABS "0" DO max := i OD	CO Exploit ABS "1" > ABS "0" CO
   OD;
   FROM 1 TO UPB buffer
   DO							CO Null loop if buffer is empty CO
      VOID (char in string (REPR 10, len, buffer));	CO Pedantry and Algol68 Genie extension CO
      FROM max TO len
      DO						CO Null loop if len < max CO
	 FOR i FROM 1 TO max
	 DO
	    printf (($a$, buffer[i]))			CO Print string and newline CO
	 OD
      OD;
      buffer := buffer[len : UPB buffer];		CO Step over string CO
      buffer := buffer[2 : UPB buffer]			CO Step over newline CO
   OD
END
```

{{out}}

```txt
printf "a\nbb\nccc\nddd\nee\nf\nggg\n" | a68g Longest_String.a68
ccc
ddd
ggg

```


Alternative recursive solution - the only standard operators used are string concatenation and array upper bound and SIGN (the signum function). Operators ATLEASTASLONGAS and LONGERTHAN are defined without the use of any comparison or arithmetic operators. The input file is processed recursively so this would run out of stack space for a large input file. Exploits some features of Algol 68 STRING slicing.

```algol68
# The standard SIGN operator returns -1 if its operand is < 0 #
#                                  ,  0 if its operand is   0 #
#                                  ,  1 if its operand is > 0 #
# This array maps he results of SIGN to FALSE or TRUE for the #
# ATLEASTASLONGAS operator defined below                      #
[ -1 : 1 ]BOOL not shorter;
not shorter[ -1 ] := FALSE;
not shorter[  0 ] := FALSE;
not shorter[  1 ] := TRUE;

# Set the priorities for the dyadic operators defined below   #
# 9 is the highest priority, so a LOMGERTHAN b AND ...        #
# is parsed correctly                                         #
PRIO ATLEASTASLONGAS = 9
   , LONGERTHAN      = 9
   ;


OP   NONEMPTYSTRING  = ( STRING a )STRING: " " + a[ AT 1 ];

# STRING x is at least as long as STRING y if the substring   #
# of x from the upper bound of y to the end of x is at least  #
# one character long                                          #
# Note that Algol 68 doesn't raise an error if the substring  #
# start position is after the upper bound of the string, but  #
# does object if the start position is before the lower bound #
# - hence the need for the NONEMPTYSTRING operator to ensure  #
#   we don't try executing a[ 0 : ] when b is ""              #
OP   ATLEASTASLONGAS = ( STRING x, STRING y )BOOL:
     BEGIN
        STRING a = NONEMPTYSTRING x;
        STRING b = NONEMPTYSTRING y;
        not shorter[ SIGN UPB a[ UPB b : ] ]
     END # ATLEASTASLONGAS # ;

# x is longer than y if x is at least as long as y and        #
# y is not at least as long as x                              #
OP   LONGERTHAN      = ( STRING x, STRING y )BOOL: x ATLEASTASLONGAS y AND NOT ( y ATLEASTASLONGAS x );
# additional LONGERTHAN operators to handle single chatracter #
# STRINGs which are actually CHAR values in Algol 68          #
# Not needed for the task, but useful for testing LONGERTHAN  #
OP   LONGERTHAN      = ( CHAR   x, CHAR   y )BOOL: FALSE;
OP   LONGERTHAN      = ( CHAR   x, STRING y )BOOL: STRING( x ) LONGERTHAN y;
OP   LONGERTHAN      = ( STRING x, CHAR   y )BOOL: x LONGERTHAN STRING( y );

COMMENT # basic test of LONGERTHAN: # C-MMENT
print( ( "abc" LONGERTHAN "bbcd", "ABC" LONGERTHAN "", "" LONGERTHAN "abc", "DEF" LONGERTHAN "DEF", "abcd" LONGERTHAN "a", newline ) );
C-MMENT COMMENT

PROC read line = ( REF FILE f )STRING:
     BEGIN
         STRING line;
         get( f, ( line, newline ) );
         IF at eof THEN "" ELSE line FI
     END # read line # ;

# EOF handler for standard input                              #
BOOL at eof := FALSE;
on logical file end( stand in, ( REF FILE f )BOOL:
                               BEGIN
                                   at eof := TRUE;
                                   TRUE
                               END
                   );


# recursively find the longest line(s) in the specified file  #
# and print them                                              #
PROC print longest lines = ( REF FILE f, STRING longest so far )STRING:
     BEGIN
        IF at eof THEN
            longest so far
        ELSE
            STRING s = read line( f );
            STRING t = IF s LONGERTHAN longest so far
                       THEN
                           print longest lines( f, s )
                       ELSE
                           print longest lines( f, longest so far )
                       FI;
            IF s ATLEASTASLONGAS t AND t ATLEASTASLONGAS s
            THEN
                # this line is as long as the longest          #
                print( ( s, newline ) );
                s
            ELSE
                # shorter line - return the longest            #
                t
            FI
        FI
    END # print longest lines # ;

# find the logest lines from standard inoout                   #
VOID( print longest lines( stand in, read line( stand in ) ) )

```



## AutoHotkey

This was fun to implement. How I bypassed the restrictions:
SubStr() returns part of a string starting from somewhere. If it goes past the end of a string, it returns "" which can be treated as false.
StrLen() returns the length of a string. Thus:
If this line contains a character at position "longestLength" then append it to the output.
If the top line of the output does not contain a character at the position of the last character in this line of the input, then reset the output to be this line *and* set "LongestLength" to be the length of this line.
did I break any rules?

```AutoHotkey
input =
(
a
bb
ccc
ddd
ee
f
ggg
)
longestLen := 0, buffer := ""
Loop Parse, input, `n
{
   top := SubStr(buffer, 1, InStr(buffer, "`n"))
   StringReplace, top, top, `n
   If SubStr(A_LoopField, LongestLen) ; at least as long
      buffer .= A_LoopField "`n"
   If !SubStr(top, StrLen(A_LoopField)) ; longer
      buffer := A_LoopField "`n", LongestLen := StrLen(A_LoopField)
}
MsgBox % buffer
```


## AWK



```awk
#!/usr/bin/awk -f 
BEGIN {
    maxlen = 0; 
    lenList = 0;
}

{
   if (length($0)>maxlen) {
	lenList = 1;
	List[lenList] = $0;
	maxlen = length($0);
   } else if (length($0)==maxlen) 
	List[++lenList]=$0;
}	

END {
   for (k=1; k <= lenList; k++) print List[k];
}
```


Output: 


```txt
ccc
ddd
ggg
```



## BBC BASIC

Key to this solution are the functions '''FNcmp''', which compares the lengths of two strings without using comparison operators, and '''FNinc''', which increments an integer without using arithmetic operators.  It also strictly adheres to the requirement to use only integer and string data types (no arrays or pointers) and avoids the use of LEN.

```bbcbasic
      DIM buffer% 65535
      bufptr% = buffer%
      longest$ = " "
      
      ON ERROR PRINT $$buffer%; : END
      
      REPEAT
        READ A$
        IF FNcmp(A$, longest$) THEN
          IF FNcmp(longest$, A$) ELSE bufptr% = buffer%
          longest$ = A$
          $bufptr% = A$
          WHILE ?bufptr%
            bufptr% = FNinc(bufptr%)
          ENDWHILE
          ?bufptr% = 10
          bufptr% = FNinc(bufptr%)
        ENDIF
      UNTIL FALSE : REM Loops until 'Out of data' error
      END
      
      DATA a, bb, ccc, ddd, ee, f, ggg
      
      DEF FNcmp(a$, b$) : REM Returns LEN(a$)>=LEN(b$) [if b$<>""]
      LEFT$(a$, 65535) = b$
      = INSTR(a$, b$)
      
      DEF FNinc(i%) : REM Returns i%+1
      FOR i% = i% TO i% : NEXT
      = i%
```

Output:

```txt

ccc
ddd
ggg

```



## C


```c>#include <stdio.h

#include <string.h>
 
int cmp(const char *p, const char *q)
{
	while (*p && *q) p = &p[1], q = &q[1];
	return *p;
}
 
int main()
{
	char line[65536];
	char buf[1000000] = {0};
	char *last = buf;
	char *next = buf;
 
	while (gets(line)) {
		strcat(line, "\n");
		if (cmp(last, line)) continue;
		if (cmp(line, last)) next = buf;
		last = next;
		strcpy(next, line);
		while (*next) next = &next[1];
	}
 
	printf("%s", buf);
	return 0;
}
```
Running it:<lang>% printf "a\nbb\nccc\nddd\nee\nf\nggg" | ./a.out
ccc
ddd
ggg
```

Note that the above code never checked for memory bounds and long input can overrun the buffers.  It's intentionally made this way to keep it simple, please don't complicate it by adding safety features: if you are really concerned with that, below is a second method that can handle arbitrary length input.
```c>#include <stdio.h

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
 
int inc(int x) { return (int)&((char *)x)[1]; }
int dec(int x) { return (int)&((char *)x)[-1]; }
int gt(int x, int y)
{
	while (y && x) y = dec(y), x = dec(x);
	return x;
}

int eq(int x, int y)
{
	return !gt(x, y) && !gt(y, x);
}

int add(int x, int y)
{
	while(y) x = inc(x), y = dec(y);
	return x;
}

/* strlen(a) + 1 */
int length(const char *a)
{
	char *x = 0; // assuming (int)(char*)0 == 0
	if (!a) return 0;
	while (*a) a++, x++;
	return (int)x;
}

char *str_cat(char *a, const char *b)
{
	int len = add(1, add(length(a), length(b)));
	if (!(a = realloc(a, len))) abort();
	return strcat(a, b);
}

char *get_line(char *l, FILE *fp)
{
	int c, len = 0;
	char tmp[2] = {0};

	*l = 0;
	while ((c = fgetc(fp)) != EOF) {
		*tmp = c;
		len = inc(len);

		l = str_cat(l, tmp);
		if (eq(*tmp, '\n')) return l;
	}

	*tmp = '\n';
	return len ? str_cat(l, tmp) : l;
}

int main()
{
	int l1, l2;
	char *line = malloc(1), *buf = malloc(1), *longest = malloc(1);
	while (1) {
		line = get_line(line, stdin);

		if (!(l1 = length(line))) break;
		l2 = length(longest);

		if (gt(l1, l2)) {
			*buf = *longest = 0;
			longest = str_cat(longest, line);
		} else if (gt(l2, l1)) continue;

		buf = str_cat(buf, line);
	}
	printf("%s", buf);

	free(buf);
	free(longest);
	free(line);

	return 0;
}
```


Here is a more concise variation which exits (with a non-zero return code) if it encounters a buffer overflow:


```c>#include <stdio.h

#include <stdlib.h>
#include <string.h>

int longer(const char *p, const char *q) {
        while (*p && *q) p = &p[1], q = &q[1];
        return *p;
}

int main() {
        char line[100000];
        char buf[1100001];
        char *linend= &line[99999];
        char *bufend= &buf[1000000];
        char *last = buf;
        char *next = buf;

        memset(line, 1, 100000);
        memset(buf, 1, 1100001);
        buf[0]= buf[1100000]= 0;
        while (fgets(line, 100000, stdin)) {
                if (!*linend) exit(1);
                if (longer(last, line)) continue;
                if (!longer(bufend, line)) exit(1);
                if (longer(line, last)) next = buf;
                last = next;
                strcpy(next, line);
                while (*next) next = &next[1];
        }

        printf("%s", buf);
        exit(0);
}
```



## Clojure

{{trans|Python}}

```lisp
ns longest-string
  (:gen-class))

(defn longer [a b]
  " if a is longer, it returns the characters in a after length b characters have been removed
    otherwise it returns nil "
  (if (or (empty? a) (empty? b))
    (not-empty a)
    (recur (rest a) (rest b))))

(defn get-input []
  " Gets the data from standard input as a lazy-sequence of lines (i.e. reads lines as needed by caller
    Input is terminated by a zero length line (i.e. line with just <CR> "
  (let [line (read-line)]
    (if (> (count line) 0)
      (lazy-seq (cons line (get-input)))
      nil)))

(defn process []
  " Returns list of longest lines "
  (first                                                             ; takes lines from [lines longest]
    (reduce (fn [[lines longest] x]
             (cond
              (longer x longest) [x x]                               ; new longer line
              (not (longer longest x)) [(str lines "\n" x) longest] ; append x to previous longest
              :else [lines longest]))                               ; keep previous lines & longest
            ["" ""] (get-input))))

(println "Input text:")
(println "Output:\n" (process))

```

{{out}}

```txt

Input text:
a
bb
ccc
ddd
ee
f
ggg

Output:
 ccc
ddd
ggg
```



## D

{{trans|Python}}

```d
import std.stdio, std.array;

/// Return a.length - b.length if positive, 0 otherwise.
int longer(string a, string b) {
    while (!a.empty && !b.empty)
        a.popFront(), b.popFront();
    return a.length;
}

void main() {
    string longest, lines;
    foreach (string line; stdin.lines())
        if (longer(line, longest))
            lines = longest = line;
        else if (!longer(longest, line))
            lines ~= line;

    writeln(lines);
}
```

{{out}}

```txt
ccc
ddd
ggg
```



## Go


```go
package main

import (
    "bufio"
    "os"
)

func main() {
    in := bufio.NewReader(os.Stdin)
    var blankLine = "\n"
    var printLongest func(string) string
    printLongest = func(candidate string) (longest string) {
        longest = candidate
        s, err := in.ReadString('\n')
        defer func() {
            recover()
            defer func() {
                recover()
            }()
            _ = blankLine[0]
            func() {
                defer func() {
                    recover()
                }()
                _ = s[len(longest)]
                longest = s
            }()
            longest = printLongest(longest)
            func() {
                defer func() {
                    recover()
                    os.Stdout.WriteString(s)
                }()
                _ = longest[len(s)]
                s = ""
            }()
        }()
        _ = err.(error)
        os.Stdout.WriteString(blankLine)
        blankLine = ""
        return
    }
    printLongest("")
}
```

Description:  It's basically the recursion+exceptions solution used by others, but staying close to the restrictions.

Restriction 1.  The program actually has no operators at all, much less comparison operators.  By the Go language specification, assignment is a statement, for example, and an index into a string is simply an "index."  For this program, comparisons that control program flow are ones that happen during expression evaluation and then either do or do not trigger a run-time panic, the rough equivalent of throwing an exception in other languages.

Restriction 2.  No arithmetic is done on numeric types, and in fact there are no variables of any numeric type in the program.  While numeric values do appear at points during expression evaluation (the len function, for example) no arithmetic is explicitly done with them.  The compiler certainly generates arithmetic instructions for address calculations underlying an index expression, for example; but the source code here simply supplies numbers as indexes, and relies on the compiler to figure out what arithmetic is appropriate.

Restriction 3.  Other than integer and string, data types used are:
* Function:  Main is a function, and there is extensive use of function literals in the program.
* os.File:  os.Stdin and os.Stdout are predefined in package os.
* bufio.Reader:  Used to wrap os.Stdin so the convenient ReadString function can be used to get individual input strings.
* error:  A predefined type in Go, returned by many library functions (such as bufio.ReadString.)
* byte:  While there are no variables of type byte in the program, single byte values appear at various points in expression evaluation.  A byte is the result of indexing into a string, for example.

The spirit of the challenge seems to be prohibiting easy ways of doing things until the only ways left are considered novel.  I don't consider recursion a particularly novel way of implementing a list, but it's obviously allowed as a solution so I used it.  Avoiding arithmetic was fairly easy using the fact that the Go len function returns the length of a string, but that strings are zero based.  Thus,

```txt

if a[len(b)] panics, it means that len(a) <= len(b)
if a[len(b)] does not panic, it means that len(a) > len(b)

```

The above expressions avoid arithmetic, but not all comparisons, because error values are typically tested and branched on.  Eliminating all comparisons leaves no boolean values in the program and no way to use if statements, which in Go require a boolean condition.  Conditional flow control is implemented with the following device:

```go
func() {
    // 1. statements executed in either case
    // 2. func below is a closure that captures free variables
    //    now, although the defer statement keeps the function
    //    from running until later
    defer func() {
        // 5.  function runs either when panic happens, or
        //     at the time of a normal function return.
        recover()  // this stops panic mode
        // 6.  statements executed in either case, just
        //     before function returns
    }()
    // 3. more statements executed in either case
    // 4. an expression that may or may not panic
    // 4a.  conditional code. executed only if no panic happens
    return // 7. function return happens in either case
}()
```

A complication of course is that sometimes you want to conditionally execute code if the expression panics.  Without a boolean value to invert, this case requires introducing an extra layer of func..defer..recover with a different expression contrived that will panic with the opposite effect.


## Groovy

Solution: recursive

```groovy
def longer = { a, b ->
    def aa = a, bb = b
    while (bb && aa) {
        bb = bb.substring(1)
        aa = aa.substring(1)
    }
    aa ? a : b
}

def longestStrings 
longestStrings = { BufferedReader source, String longest = '' ->
    String current = source.readLine()
    def finalLongest = current == null \
        ? longest \
        : longestStrings(source,longer(current,longest))
    if (longer(finalLongest, current) == current) {
        println current
    }
    return finalLongest
}
```


Test:

```groovy
def source = new BufferedReader(new StringReader('''a
bb
ccc
ddd
ee
f
ggg'''))

longestStrings(source)
```


Output:

```txt
ggg
ddd
ccc
```



## Haskell


Even though lists of strings were disallowed in the rules, I have used them instead of a file handle, mainly to keep my functions pure, and to avoid the hassle of using the IO monad for something more trivial without it.

Another use of lists in the code is for Strings, which are lists of Chars in Haskell by default, which made it easy to compare them by length.

No operators were used except for string/list concatenation.


```Haskell

module Main where

import           System.Environment

cmp :: String -> String -> Ordering
cmp [] []         = EQ
cmp [] (_:_)      = LT
cmp (_:_) []      = GT
cmp (_:xs) (_:ys) = cmp xs ys

longest :: String -> String
longest = longest' "" "" . lines
  where
    longest' acc l []         = acc
    longest' [] l (x:xs)      = longest' x x xs
    longest' acc l (x:xs) = case cmp l x of
                                   LT -> longest' x x xs
                                   EQ -> longest' (acc ++ '\n':x) l xs
                                   GT -> longest' acc l xs

main :: IO ()
main = do
  (file:_) <- getArgs
  contents <- readFile file
  putStrLn $ longest contents

```


=={{header|Icon}} and {{header|Unicon}}==

###  String Scanning / Pattern Matching Solution 


```Icon
procedure main(arglist)
    local b  # the current buffer (string)
    local l  # the last string 
    local L  # a \n delimited accumulation of all the longest strings
    
    while b := read() do {
        /l := b      # primes l on first pass
        b ? ( move(*l), if move(1) then L := (l := b) || "\n" else if move(0) then L := (\L|"") || b || "\n") 
            #       move(*l) - fails if b is not l characters long
            #       move(1)  - succeeds/fails if the string is longer and triggers a reset of L
        }
    
    write(\L)
end
```


Sample Output:
```txt
ccc
ddd
ggg

```


###  Recursive Solution 

Here is a recursive solution using only single character substring-ing (no string scanning/pattern matching).

```Icon
procedure main()
    longest(".")   # needs a single character seed to throw away
end

procedure longest(Longest)
    Line := read() | return Longest        # read until we can return the longest strings
    if Line[*Longest] then Longest := Line # prime/reset Longest
    Longest := longest(Longest)            # use stack to hold multiples
    if Line[*Longest] then write(Line)     # write only longest strings, 
                                           # Line must be at least as long as Longest
    return Longest                         # feed back longest for length
end
```


Sample Output:
```txt
ggg
ddd
ccc
```



## J



```j
   isempty =. (0 [ 0&{) :: 1:  NB. 0=#
   compare =. ($:&}.)`((0 1,:_1 0) {~ <@,&isempty)@.(+.&isempty) NB. *@-&#
   add =. ,`(,:@[)`] @. (compare {:)
   > add&.>/ (}: , ,:&.>@{:) ;: 'a bb ccc ddd ee f ggg'
ccc
ddd
ggg
```


Description:

isempty fetches the first element from an array, and traps the error. The result is false (0) if there was a first element, true (1) otherwise.

compare uses isempty recursively (dropping an element from the array each time) until one (or both) are empty. Once one of the lists is empty a result the pair of numbers from isempty on the two lists is used to pull a value out of an table - the sign of this value indicates which of the two lists was greater. In other words, you get a result of _1 0 or 1, depending on whether one is larger, or neither is larger or the other is larger.

add uses the result from compare to pick from a list of functions which do the right thing when combining lists (add to the current list of results or replace it).

The final expression sets things up for add to function properly and then extracts the result when add is done. (You might think of this as being somewhat like a Haskell monad - though perhaps that analogy should be avoided since a J monad is something very different. J's use of the word "monad" came about decades ago and is related to the use of the word in music theory.)


## Java

Translation of Python via D

```java
import java.io.File;
import java.util.Scanner;

public class LongestStringChallenge {

    public static void main(String[] args) throws Exception {
        String lines = "", longest = "";
        try (Scanner sc = new Scanner(new File("lines.txt"))) {
            while(sc.hasNext()) {
                String line = sc.nextLine();
                if (longer(longest, line))
                    lines = longest = line;
                else if (!longer(line, longest))
                    lines = lines.concat("\n").concat(line);
            }
        }
        System.out.println(lines);
    }

    static boolean longer(String a, String b) {
        try {
            String dummy = a.substring(b.length());
        } catch (StringIndexOutOfBoundsException e) {
            return true;
        }
        return false;
    }
}
```



```txt
ccc
ddd
ggg
```



## Julia

{{works with|Julia|0.6}}


```julia
function longer(a, b)
    try
        b[endof(a)]
    catch
        return true
    end
    return false
end

function printlongest(io::IO)
    lines = longest = ""
    while !eof(io)
        line = readline(io)
        if longer(line, longest)
            longest = lines = line
        elseif !longer(longest, line)
            lines *= "\n" * line
        end
    end
    println(lines)
end
printlongest(str::String) = printlongest(IOBuffer(str))

printlongest("a\nbb\nccc\nddd\nee\nf\nggg")
```


{{out}}

```txt
ccc
ddd
ggg
```



## Kotlin

{{trans|Java}}

```scala
// version 1.1.0

import java.io.File
import java.util.*

fun longer(a: String, b: String): Boolean = 
    try {
        a.substring(b.length)
        false
    } 
    catch (e: StringIndexOutOfBoundsException) {
        true
    }

fun main(args: Array<String>) {
    var lines = ""
    var longest = ""
    val sc = Scanner(File("lines.txt"))    
    while(sc.hasNext()) {
        val line = sc.nextLine()
        if (longer(longest, line)) {
            longest = line
            lines = longest
        }
        else if (!longer(line, longest))
            lines = lines.plus("\n").plus(line) // using 'plus' to avoid using '+'
    }
    sc.close()
    println(lines);
    println()

    // alternatively (but cheating as library functions will use comparisons and lists under the hood)
    println(File("lines.txt").readLines().groupBy { it.length }.maxBy { it.key }!!.value.joinToString("\n"))
}
```


{{out}}

```txt

ccc
ddd
ggg

ccc
ddd
ggg

```



## Lua



```lua
function longer(s1, s2)
    while true do
        s1 = s1:sub(1, -2)
        s2 = s2:sub(1, -2)
        if s1:find('^$') and not s2:find('^$') then
           return false
        elseif s2:find('^$') then
           return true
        end
    end
end

local output = ''
local longest = ''

for line in io.lines() do
    local islonger = longer(line, longest)
    if islonger and longer(longest, line) then
        output = output .. line .. '\n'
    elseif islonger then
        longest = line
        output = line .. '\n'
    end
end

print(output)
```


This solution is ispired by the Python one, but since in Lua even an empty string has a boolean true value, it had to be slightly altered. Testing whether a string is empty is done by searching for the Lua string ''pattern'' <code>'^$'</code>. If it is found, i.e. the examined string is empty, <code>string.find</code> returns the position of the match, or <code>nil> if it didn't match. Since in Lua any number is <code>true</code>, we just test for the boolean value of the result.

Note that the <code>longer</code> function returns <code>true</code> even if both strings have the same length, so the return value can either be <code>true</code> or <code>false</code> and we can avoid using a comparison or equality operator in interpreting this return value.

The <code>longer</code> function also avoids using the length operator (<code>#</code>), because in the comments on the restrictions of this task it is stated that ''"Representing the length of any string as a number should also be avoided."''. Otherwise it could have been written shorter and faster like this:


```lua
function longer(s1, s2)
    if s1:sub(#s2):find('^$') then
       return false
    else
       return true
    end
end

```



## Mathematica


```mathematica
FixedPoint[
 StringReplace[#, 
   x : "\n" | StartOfString ~~ a : Except["\n"] ... ~~ "\n" ~~ 
     b : Except["\n"] ... ~~ y : "\n" | EndOfString :> 
    x <> Switch[((#1 + #2) + Abs[#1 - #2])/2 &[StringLength@a, 
       StringLength@b], Except[StringLength@a], b, 
      Except[StringLength@b], a, _, a <> "\n" <> b] <> y] &, "a
 bb
 ccc
 ddd
 ee
 f
 ggg"]
```

{{Out}}

```txt
ccc
ddd
ggg
```


=={{header|MATLAB}} / {{header|Octave}}==


```MATLAB
function longestString(file);
  fid = fopen(file);
  maxlen = 0; L = {};
  while ~feof(fid)
    line = fgetl(fid); 
    if (length(line)>maxlen) 
      maxlen = length(line); 
      L = {line};
    elseif (length(line)==maxlen) 
      L{end+1} = line; 
    end; 
  end; 
  fclose(fid);
  disp(L);
end; 

```


Output: 

```txt
 L = {
  [1,1] = ccc
  [1,2] = ddd
  [1,3] = ggg
} 
```



## OCaml


Without the restrictions, the standard way to solve the task would be to iterate the lines, compare their length, and accumulate the longest lines in a list. Here we bypass the restriction of not using comparison operators (in the function <code>cmp</code>) by taking advantage that characters of strings are indexed from 0 to [length - 1] and trying to access to a character which index is the length of the other string (if it is beyond the end of the string an exception is raised). The other restriction of not using lists is easily bypassed by concatenating the results instead of accumulating it in a list.


```ocaml
let input_line_opt ic =
  try Some (input_line ic)
  with End_of_file -> None

let cmp s1 s2 =
  try ignore(s1.[String.length s2]); 1     (* s1 is longer *)
  with _ ->
    try ignore(s2.[String.length s1]); -1  (* s2 is longer *)
    with _ -> 0                            (* both same length *)

let () =
  let ic = open_in Sys.argv.(1) in
  let rec loop longest acc =
    match input_line_opt ic with
    | Some line ->
      ( match cmp line longest with
        | 1 -> loop line (line ^ "\n")
        | 0 -> loop line (acc ^ line ^ "\n")
        | _ -> loop longest acc )
    | None ->
        close_in ic;
        print_string acc
  in
  loop "" ""
```



## Pascal

{{works with|Free_Pascal}}
In this version, inc() is used instead of additions. It still has a comparison.

```pascal
program LongestStringChallenge_1(input, output);

var
  Line: string;
  Lines: array of string;
  position, len: integer;

begin
  if not eoln(input) then
  begin
    len := 1;
    position := 0;
    readln (line);
    setlength(lines, len);
    lines[position] := line;
    while not eoln(input) do
    begin
      readln (line);
      if length(line) = length(lines[0]) then
      begin
        inc(position);
        inc(len);
        setlength(lines, len);
        lines[position] := line;
      end;
      if length(line) > length(lines[0]) then
      begin
        position := 0;
        len := 1;
        setlength(lines, 1);
        lines[0] := line;
      end;
    end;
    for position := low(lines) to high(lines) do
      writeln (lines[position]);
  end;
end.
```

Output:

```txt
% ./LongestStringChallenge_1
a
b
ccc
ddd
ee
f
ggg

ccc
ddd
ggg

```

{{works with|Free_Pascal}}
{{libheader|SysUtils}}
This version uses range check exceptions for the comparisons of string lengths. The range checks are compiler specific. With FreePascal its requires the use of the type ANSIstring instead of "classic" type string.

```pascal
program LongestStringChallenge_2(input, output);

{$mode ObjFPC}
{$rangechecks on}

uses
  SysUtils;
 
var
  Line: ANSIstring;
  Lines: array of ANSIstring;
  position: integer;
  tester: char;
 
begin
  if not eoln(input) then
  begin
    readln (line);
    position := 0;
    setlength(lines, 1);
    lines[0] := line;
    while not eoln(input) do
    begin
      readln (line);
      try
        tester := lines[0][length(line)];
	try
	  tester := line[length(lines[0])];
	  inc(position);
	  setlength(lines, succ(position));
	  lines[position] := line;
	except
	end;
      except
        position := 0;
        setlength(lines, 1);
        lines[0] := line;      
      end;
    end;
    for position := low(lines) to high(lines) do
      writeln (lines[position]);
  end;
end.
```

Output:

```txt
% ./LongestStringChallenge_2
a
b
ccc
ddd
ee
f
ggg

ccc
ddd
ggg

```



## Perl


```perl
#!/usr/bin/perl -n
END{ print $all }

substr($_, length($l)) and $all = $l = $_
	or substr($l, length) or $all .= $_;
```



## Perl 6



```perl6
my $l = '';  # Sample longest string seen.
my $a = '';  # Accumulator to save longest strings.

while get() -> $s {
   my $n = "$s\n";
   if $n.substr($l.chars) {     # Is new string longer?
       $a = $l = $n;            # Reset accumulator.
   }
   elsif !$l.substr($n.chars) { # Same length?
      $a ~= $n;                 # Accumulate it.
   }
}
print $a;
```


Given the example input, returns:

```txt
ccc
ddd
ggg
```



## Phix

A recursive/substring based approach, I can't see any explicit maths/comparisons anyway:

```Phix
integer fn = open(command_line()[2],"r") -- (reading the source file)

function allx(string line)
    line[1..-1] = 'x'
    return line
end function

function longest(string mask)
object line = gets(fn)
string newmask
    if atom(line) then return mask end if
    newmask = allx(line)
    if not match(mask,newmask) then return longest(mask) end if
    mask = longest(newmask)
    if match(mask,newmask) then
        puts(1,line)
    end if
    return mask
end function

?longest("x")

close(fn)

```

Of course it is just a thinly disguised version of:

```Phix
function longest(integer l)
object line = gets(fn)
    if line=-1 then return l end if
    if l>length(line) then return longest(l) end if
    l = longest(length(line))
    if l=length(line) then
        puts(1,line)
    end if
    return l
end function

?longest(0)
```



## PicoLisp

Not sure if this meets the spirit. I would implement it the same way if there were no "restrictions":

```PicoLisp
(mapc prinl
   (maxi '((L) (length (car L)))
      (by length group
         (in NIL
            (make (until (eof) (link (line)))) ) ) ) )
```

Another solution avoids 'group', and builds an associative buffer of lines instead:

```PicoLisp
(let Buf NIL
   (in NIL
      (until (eof)
         (let (Line (line)  Len (length Line))
            (if (assoc Len Buf)
               (conc @ (cons Line))
               (push 'Buf (cons Len (cons Line))) ) ) ) )
   (mapc prinl (cdr (maxi car Buf))) )
```



## Pike

things of note:
the comparison of strings is done by converting the string into an array of indices: (<code>"abc"</code> becomes <code>({ 1,2,3 })</code>)
the - operation is the set operation <math>A/B</math> and not a numerical subtraction. it removes all the elements in the second array from the first.
if there are any left, we know that the string is longer.

now, once a longer string is found we call <code>write()</code> to print it.
however we don't write it out directly, but instead we store the call in queue of pikes backend. the backend is used to handle callbacks for non-blocking I/O, and it provides a facility to call functions after a delay of time. (<code>call_out(write, 5, "foo");</code> calls write after 5 seconds with the argument foo)

before we add the new call to write, we remove all older calls to write since we don't want them anymore.

<code>return -1;</code> starts the backend, which allows pike to execute the remaining call_outs and exit.

```Pike
int main(int argc, array argv) 
{ 
    string longest = ""; 
    foreach(Stdio.stdin.line_iterator();; string line) 
    { 
        if( sizeof(indices(line) - indices(longest))) 
        { 
            while(!zero_type(remove_call_out(write))); 

            longest = line; 
            call_out(write, 0, line+"\n"); 
        } 
        else if( !sizeof(indices(longest) - indices(line))) 
        { 
            call_out(write, 0, line+"\n"); 
        } 
    } 
    call_out(exit, 0.01, 0); 
    return -1; 
}
```



## PL/I


```PL/I

read: procedure options (main);  /* 18 January 2012. */
   declare line character (100) varying controlled;
   declare text character (100) varying;
   declare max_length fixed binary;
   declare in file input;

   on endfile (in) go to done;

   open file (in) title ('/readline.pli,type(text),recsize(100)');

   max_length = 0;
   do forever;
      get file (in) edit (text) (L);
      put skip list (text);
      if length (text) > max_length then
         do;
            max_length = length(text);
            /* empty the stack */
            do while (allocation(line) > 0);
               free line;
            end;
            allocate line;
            line = text;
         end;
      else if length(text) = max_length then
         do;
            allocate line;
            line = text;
         end;
   end;

done:
   put skip list (max_length || ' is the length of the longest line(s)' );
   do while (allocation(line) > 0);
      put skip list (line);
      free line;
   end;

end read;

```

output (the above file plus the following 3 lines):

```txt

       74 is the length of the longest line(s) 
   put skip list (max_length || ' is the length of the longest line(s)' ); 
/* Read lines of a file, and print the longest. (If there is more than  */ 

```



## PowerShell

{{works with|PowerShell|2}}

```PowerShell

#  Get-Content strips out any type of line break and creates an array of strings
#  We'll join them back together and put a specific type of line break back in
$File = ( Get-Content C:\Test\File.txt ) -join "`n"
 
$LongestString = $LongestStrings = ''
 
#  While the file string still still exists
While ( $File )
    {
    #  Set the String to the first string and File to any remaining strings
    $String, $File = $File.Split( "`n", 2 )
 
    #  Strip off characters until one or both strings are zero length
    $A = $LongestString
    $B = $String
    While ( $A -and $B )
        {
        $A = $A.Substring( 1 )
        $B = $B.Substring( 1 )
        }
 
    #  If A is zero length...
    If ( -not $A )
        {
        #  If $B is not zero length (and therefore String is longer than LongestString)...
        If ( $B )
            {
            $LongestString = $String
            $LongestStrings = $String
            }
        #  Else ($B is also zero length, and therefore String is the same length as LongestString)...
        Else
            {
            $LongestStrings = $LongestStrings, $String -join "`n"
            }
        }
    }
 
#  Output longest strings
$LongestStrings.Split( "`n" )

```



### PowerShell Alternate Version

The list restrictions should not apply here because this is essentially one line of code using only the input and no variables. 

```PowerShell

@'
a
bb
ccc
ddd
ee
f
ggg
'@ -split "`r`n" |
    Group-Object  -Property Length |
    Sort-Object   -Property Name -Descending |
    Select-Object -Property Count, @{Name="Length"; Expression={[int]$_.Name}}, Group -First 1

```

{{Out}}

```txt

Count Length Group          
----- ------ -----          
    3      3 {ccc, ddd, ggg}

```



## PureBasic


```purebasic


Procedure.i ConsoleWrite(t.s)  ; compile using /CONSOLE option
        OpenConsole()
        PrintN (t.s)
        CloseConsole()
        ProcedureReturn 1
EndProcedure

Procedure.i StdOut(t.s)  ; compile using /CONSOLE option
        OpenConsole()
        Print(t.s)
        CloseConsole()
        ProcedureReturn 1
EndProcedure


DataSection
s:
Data.s "a"
Data.s "bb"
Data.s "ccc"
Data.s "ddd"
Data.s "ee"
Data.s "f"
Data.s "ggg"
Data.s "~"	; the tilda is only to keep the code compact
e:		; and easy to understand
EndDataSection

l$=""		; memory allocation for strings is automatic
a$=""		; in fact these two lines are unnecessary

Restore s

Repeat
Read.s s$
If s$="~":Break:EndIf
s$+#CRLF$
s=Len(s$):l=Len(l$)	; using s$ allows the use of s as an integer type
If     s>l		:l$=s$:a$=l$
ElseIf s=l		:a$+s$
EndIf
Forever

StdOut(a$)


```


;Output:


```txt


 ; Directory of C:\_sys\temp

; 07/14/2012  03:04 PM             4,608 LongestStringChallenge.exe
               ; 1 File(s)          4,608 bytes
               ; 0 Dir(s)  434,768,625,664 bytes free

; C:\_sys\temp>LongestStringChallenge.exe
; ccc
; ddd
; ggg



```



## Python


```python
import fileinput

# This returns True if the second string has a value on the 
# same index as the last index of the first string. It runs
# faster than trimming the strings because it runs len once
# and is a single index lookup versus slicing both strings 
# one character at a time.
def longer(a, b):
    try:
        b[len(a)-1]
        return False
    except:
        return True

longest, lines = '', ''
for x in fileinput.input():
    if longer(x, longest):
        lines, longest = x, x
    elif not longer(longest, x):
        lines += x

print(lines, end='')
```


;Sample runs:

```txt


paddy@paddy-VirtualBox:~$ cat <<! | python3.2 longlines.py 
a
bb
ccc
ddd
ee
f
ggg
!
ccc
ddd
ggg
paddy@paddy-VirtualBox:~$ touch nothing.txt
paddy@paddy-VirtualBox:~$ cat nothing.txt  | python3.2 longlines.py 
paddy@paddy-VirtualBox:~$ 


```



## Racket

This is an attempt to follow the problem restrictions: use just one list of the complete output (so it's not used as a container of strings), and the work is done by manipulating this list vs the input instead of direct comparisons.


```Racket

#lang racket

(define (newline? c) (equal? c #\newline))
(define eof? eof-object?)

(let loop ([O  '()] [C  '(#\newline)] [rI '()] [rO '()] [rC '()])
  (let* ([i (read-char)] [o (car C)] [i:rI (cons i rI)] [i:rC (cons i rC)])
    (cond [(eof? i) (for-each write-char O)]
          [(and (newline? o) (newline? i))
           (let ([O (reverse i:rC)]) (loop O O '() i:rC i:rC))]
          [(newline? i) (loop O O       '()  rO rO)]
          [(newline? o) (loop O C       i:rI rO i:rI)]
          [else         (loop O (cdr C) i:rI rO i:rC)])))

```



## REXX

In the REXX language,   ''everything''   is a string (characters).

### read file until not ready

This REXX version adheres to spirit (and letter) of all the restrictions for this task:
::*   no comparators are used,   including those within:
::::*       '''if'''            (statements)
::::*                 '''when'''                    (statements)
::::*                 '''until'''              (clauses)      and 
::::*                 '''while'''              (clauses)  
::*   no output is produced when the file is empty   (or contains all null strings),
::*   no arrays or lists are used, 
::*   no additions or subtractions are used,   and
::*   no variables are used to hold the length of (any) record.

```rexx
/*REXX program reads a file  and  displays  the  longest [widest]  record(s) [line(s)]. */
signal on notReady                               /*when E-O-F is reached,  jump/branch. */
iFID= 'LONGEST.TXT'                              /*the default file identifier for input*/
parse arg fid .                                  /*obtain optional argument from the CL.*/
    do #=1  to length(fid);          iFID=fid    /*Specified?   Then use what's given.  */
    end   /*#*/
!=                                               /*the maximum width  (so far).         */
    do forever;    _=linein(iFID);   ?=_         /*read a line from the input file.     */
    t=0                                          /*don't do the initialization next time*/
        do #=t  for t;    !=?;       ?=;        $=. || _;       end  /*just do 1st time.*/
        do #=length(!' ')  to length(?) for 1;  $=;             end  /*found widest rec.*/
        do #=length(!)     to length(?) for 1;  $=$'a0d'x || _; end  /*append it to  $. */
                                                 /* [↑]  variable  #  isn't really used.*/
    !=left(.,  max( length(!), length(?) ) )     /*!:  is the max length record, so far.*/
    end   /*forever*/
                                                 /* [↓]  comes here when file gets E─O─F*/
notReady:   do j=length(!)  to length(!)  for length(!)    /*handle the case of no input*/
            say substr($, 2)                     /*display (all) the longest records.   */
            end   /*j*/                          /*stick a fork in it,  we're all done. */
```

{{out|input|text=file   '''LONGEST.TXT''':}}

```txt

a
bb
ccc
ddd
ee
f
ggg

```

{{out|output|text=  when using the default input:}}

```txt

ccc
ddd
ggg

```


===Dual code (works on TSO and PC)===

```rexx
/* REXX ***************************************************************
* 27.10.2010 Walter Pachl
**********************************************************************/
Parse Arg fid
If fid='' Then Do
  "ALLOC FI(IN) DA('N561985.PRIV.V100(LL)') SHR REUSE"
  'EXECIO * DISKR IN (STEM L. FINIS'   /* read all lines             */
  'FREE FI(IN)'
  End
Else Do
  Do i=1 By 1 While lines(fid)>0
    l.i=linein(fid)
    End
  l.0=i-1
  End
maxl = 0                               /* initialize maximum length  */
Do i=1 To l.0                          /* loop through all lines     */
  linl=length(l.i)                     /* length of current line     */
  Select
    When linl>maxl Then Do             /* line longer than preceding */
      maxl=linl                        /* initialize maximum length  */
      mem.0=1                          /* memory has one entry       */
      mem.1=l.i                        /* the current line           */
      lin.1=i                          /* its line number            */
      End
    When linl=maxl Then Do             /* line as long as maximum    */
      z=mem.0+1                        /* new memory index           */
      mem.z=l.i                        /* the current line           */
      lin.z=i                          /* its line number            */
      mem.0=z                          /* memory size                */
      End
    Otherwise                          /* line is shorter than max.  */
      Nop                              /* ignore                     */
    End
  End
If mem.0>0 Then Do
  Say 'Maximum line length='maxl
  Say ' Line Contents'
  Do i=1 To mem.0
    Say right(lin.i,5) mem.i
    End
  End
Else
  Say 'No lines in input file or file does not exist' 
```


```txt

Maximum line length=5
 Line Contents
    1 99999
    3 +++++        

```



## Ring


```ring

# Project : Longest string challenge

load "stdlib.ring"

test = ["a", "bb", "ccc", "ddd", "ee", "f", "ggg"]
test1 = [] 
test2 = []

for n = 1 to len(test)
    add(test1, [test[n], len(test[n])])
next
sortFirstSecond(test1, 2)

for n = len(test1) to 2 step -1
    if test1[n][2] = test1[n-1][2]
      add(test2, test1[n][1])
    else
      add(test2, test1[n][1])
      exit
    ok
next
test2 = sort(test2)
see test2 + nl

```

Output:

```txt

ccc
ddd
ggg

```



## Ruby


```ruby
# Without restrictions
BEGIN {
   v = [ ]
   m = 0
}

n = $_.length
if n == m then
   v <<= $_
elsif n > m then
   v = [$_]
   m = n
end

END {
   v.each { |s| puts s }
}
```


Then ''ruby -n longest.rb < file.txt''

```ruby
h = $stdin.group_by(&:size)
puts h.max.last  unless h.empty?
```

This uses a hash with arrays as values - but not explicit.


## Run BASIC

Uses in memory database

```runbasic
sqliteconnect #mem, ":memory:"                                                    ' Create in memory DB
#mem execute("CREATE TABLE data(str)")                                            ' And fields to hold the string data

strings$ = "a bb ccc ddd ee f ggg"                                                ' The given string data

while word$(strings$,i + 1," ") <> ""
 i = i + 1
 #mem execute("INSERT INTO data VALUES('";word$(strings$,i," ");"')")             ' insert the strings in to the DB
wend

#mem execute("SELECT length(str) as leng, str FROM data ORDER BY leng desc,str")  ' pull data in reverse lenght sequence
WHILE #mem hasanswer()
      #row = #mem #nextrow()
      leng = #row leng()
      str$ = #row str$()
print leng;" ";str$                                                               ' print the data
WEND
```


Using a simple sort method


```runbasic
strings$ = "a bb ccc ddd ee f ggg"                     ' The given string data

while word$(strings$,numWords + 1," ") <> ""           ' Count the words
 numWords = numWords + 1
wend

dim string$(numWords)                                 ' Dimension the string with the word cound
for j = 1 to numWords
  string$(j) = word$(strings$,j," ")                  ' put the words from the string into the string array
next j

h$ = "1"
while h$ <> ""                                        ' The good old simple bubble sort
h$ = ""
  for i = 1 to numWords -1
    if len(string$(i)) < len(string$(i+1)) then       ' sort by length descending
      h$           = string$(i)
      string$(i)   = string$(i+1)
      string$(i+1) = h$
    end if
  next i
wend

for i = 1 to numWords                               
print len(string$(i));" ";string$(i)                 ' print out the words in length descending sequence
next i
```


```txt
3 ccc
3 ddd
3 ggg
2 bb
2 ee
1 a
1 f
```



## Rust


```Rust
use std::cmp::Ordering;
use std::io::BufRead;

/// Compares the length of two strings by iterating over their characters
/// together until either string has run out.
fn compare(a: &str, b: &str) -> Ordering {
    let mut a = a.chars();
    let mut b = b.chars();
    loop {
        match (a.next(), b.next()) {
            (None, None) => return Ordering::Equal,
            (Some(_), None) => return Ordering::Greater,
            (None, Some(_)) => return Ordering::Less,
            (Some(_), Some(_)) => {}
        }
    }
}

/// Returns the longest lines of the input, separated by newlines.
fn longest<I: IntoIterator<Item = String>>(input: I) -> String {
    let mut longest = String::new();
    let mut output = String::new();

    for line in input {
        match compare(&line, &longest) {
            // A longer string replaces the output and longest.
            Ordering::Greater => {
                output.clear();
                output.push_str(&line);
                longest = line;
            }
            // A string of the same length is appended to the output.
            Ordering::Equal => {
                output.push('\n');
                output.push_str(&line);
            }
            // A shorter string is ignored.
            Ordering::Less => {}
        }
    }

    output
}

fn main() {
    let stdin = std::io::stdin();
    let lines = stdin.lock().lines().map(|l| l.expect("Failed to read."));
    
    println!("{}", longest(lines))
}
```



## Scala


```Scala
val longest = scala.io.Source.fromFile(args.head).getLines.toIterable.groupBy(_.length).max._2
println(longest mkString "\n")
```



## Sidef


```ruby
var l = '';  # Sample longest string seen.
var a = '';  # Accumulator to save longest strings.

STDIN.each { |n|
    n.substr(l.len) ? (a = n; l = n)
                    : (!l.substr(n.len) && a.concat!(n));
}

print a;
```



## Tcl

Uses only string comparisons for equality and glob-style matching


```tcl
#!/usr/bin/env tclsh

set longest z
set output ""
while {[gets stdin line] != -1} {
    set comparison [string repeat z [string length $line]]
    if {$longest eq $comparison} {
        # this line is equally long
        append output $line \n
    } elseif {[string match ${longest}z* $comparison]} {
        # this line is longer
        set longest $comparison
        set output "$line\n"
    }
}
puts -nonewline $output
```


Test:


```txt
$ ./longest.tcl <<END
> a
> bb
> ccc
> ddd
> ee
> f
> ggg
> END
ccc
ddd
ggg
$ ./longest.tcl </dev/null
$ 
```


{{omit from|Brlcad}}
{{omit from|GUISS}}
{{omit from|Openscad}}


## VBScript

The solution uses the Mid function to compare string lengths.


```vb

'Read the input file.  This assumes that the file is in the same
'directory as the script.
Set objfso = CreateObject("Scripting.FileSystemObject")
Set objfile = objfso.OpenTextFile(objfso.GetParentFolderName(WScript.ScriptFullName) &_
	"\input.txt",1)

list = ""
previous_line = ""
l = Len(previous_line)

Do Until objfile.AtEndOfStream
	current_line = objfile.ReadLine
	If Mid(current_line,l+1,1) <> "" Then
		list = current_line & vbCrLf
		previous_line = current_line
		l = Len(previous_line)
	ElseIf Mid(current_line,l,1) <> ""  And Mid(current_line,(l+1),1) = "" Then
		list = list & current_line & vbCrLf
	End If
Loop

WScript.Echo list

objfile.Close
Set objfso = Nothing

```


{{Out}}

```txt

ccc
ddd
ggg

```



## XQuery


Port of XSLT solution, this time using a string sequence.


```xquery

let $seq as xs:string+ := ("a","bb","ccc","ddd","ee","f","ggg")
for $l in max( 
               for $s in $seq
               return string-length($s)
             )
return $seq[string-length(.) eq $l]

```


Result:

```txt

("ccc","ddd","ggg")

```



## XSLT 2.0

This XSLT 2.0 style-sheet...
<lang><xsl:stylesheet version="2.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output indent="yes" encoding="UTF-8" omit-xml-declaration="yes" />     
<xsl:template match="/*">
  <t><xsl:copy-of select="for $l in max( for $s in s return string-length($s))
	  return s[string-length(.) eq $l]" /></t>
</xsl:template>
</xsl:stylesheet>
```


...when applied to this input...
<lang><t>
 <s>a</s>
 <s>bb</s>
 <s>ccc</s>
 <s>ddd</s>
 <s>ee</s>
 <s>f</s>
 <s>ggg</s>
</t>
```


...yields...
<lang><t>
   <s>ccc</s>
   <s>ddd</s>
   <s>ggg</s>
</t>
```



## zkl

The ops used are: remove a character from the beginning of a string, testing if a string is "" or not, , string concatenation, integer equality.

To decide which of two strings is longer, a character is removed from each until one is empty. If one still has text, it is longer.

```zkl
fcn longer(a,b){ //-->0,1,2 (same, a longer, b longer)
   while(a and b){a=a.del(0); b=b.del(0);}
   if (not a and not b) return(0); // a & b same length
   if(a) return(1); 	// a is longer
   2			// b is longer
}

text:=a:=ask("text: ").strip(); 
while(b:=ask("text: ").strip()){
   switch(longer(a,b)){
      case(0){ text=String(text,"\n",b) }  // a.len()==b.len()
      case(1){ }		// a.len>b.len()
      case(2){ text=a=b }	// a.len>b.len()
   } //switch
}
println(text);
```

{{out}}

```txt

text: a
text: bb
text: ccc
text: ddd
text: ee
text: f
text: ggg
text: 
The longest line(s) are:
ccc
ddd
ggg

```

{{out}}

```txt

text: 
text: 
The longest line(s) are:

```



[[Category:Handicap]]
