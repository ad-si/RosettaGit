+++
title = "String comparison"
description = ""
date = 2019-10-16T13:52:44Z
aliases = []
[extra]
id = 12964
[taxonomies]
categories = []
tags = []
+++

{{task|Basic Data Operations}}{{basic data operation}} [[Category:Basic language learning]] [[Category:Simple]]
The task is to demonstrate how to compare two strings from within the language and how to achieve a lexical comparison.

The task should demonstrate:
* Comparing two strings for exact equality
* Comparing two strings for inequality (i.e., the inverse of exact equality)
* Comparing two strings to see if one is lexically ordered before than the other
* Comparing two strings to see if one is lexically ordered after than the other
* How to achieve both case sensitive comparisons and case insensitive comparisons within the language
* How the language handles comparison of numeric strings if these are not treated lexically
* Demonstrate any other kinds of string comparisons that the language provides, particularly as it relates to your type system.   For example, you might demonstrate the difference between generic/polymorphic comparison and coercive/allomorphic comparison if your language supports such a distinction.



Here "generic/polymorphic" comparison means that the function or operator you're using doesn't always do string comparison, but bends the actual semantics of the comparison depending on the types one or both arguments; with such an operator, you achieve string comparison only if the arguments are sufficiently string-like in type or appearance.

In contrast, a "coercive/allomorphic" comparison function or operator has fixed string-comparison semantics regardless of the argument type;   instead of the operator bending, it's the arguments that are forced to bend instead and behave like strings if they can,   and the operator simply fails if the arguments cannot be viewed somehow as strings.   A language may have one or both of these kinds of operators;   see the Perl 6 entry for an example of a language with both kinds of operators.


;Related tasks:
*   [[Integer comparison]]
*   [[String matching]]
*   [[Compare a list of strings]]





## Ada

Ada uses the usual comparison operators ("=" for equality, "/=" for not being equal, etc.) for strings. One uses the same comparison operators to compare variables of other types (integers, floating point numbers, etc.).
But, as Ada is strongly typed, comparing two objects of different type is not possible.
To compare, say, a string and an integer, one would need to call an explicit type conversion for one of these objects.

String comparisons are case sensitive. Case insensitive comparisons have to use some conversion operation, such as Ada.Characters.Handling.To_Lower from the standard library, cf. [[http://rosettacode.org/wiki/String_case#Ada]]


```Ada
with Ada.Text_IO, Ada.Strings.Equal_Case_Insensitive;

procedure String_Compare is

   procedure Print_Comparison (A, B : String) is
   begin
      Ada.Text_IO.Put_Line
         ("""" & A & """ and """ & B & """: " &
          (if A = B then
              "equal, "
           elsif Ada.Strings.Equal_Case_Insensitive (A, B) then
              "case-insensitive-equal, "
           else "not equal at all, ")                   &
          (if A /= B then "/=, "     else "")           &
          (if A <  B then "before, " else "")           &
          (if A >  B then "after, "  else "")           &
          (if A <= B then "<=, "     else "(not <=), ") &
          (if A >= B then ">=. "     else "(not >=)."));
   end Print_Comparison;
begin
   Print_Comparison ("this", "that");
   Print_Comparison ("that", "this");
   Print_Comparison ("THAT", "That");
   Print_Comparison ("this", "This");
   Print_Comparison ("this", "this");
   Print_Comparison ("the", "there");
   Print_Comparison ("there", "the");
end String_Compare;
```


{{out}}

```txt
"this" and "that": not equal at all, /=, after, (not <=), and >=.
"that" and "this": not equal at all, /=, before, <=, and (not >=).
"THAT" and "That": case-insensitive-equal, /=, before, <=, and (not >=).
"this" and "This": case-insensitive-equal, /=, after, (not <=), and >=.
"this" and "this": equal, <=, and >=.
"the" and "there": not equal at all, /=, before, <=, and (not >=).
"there" and "the": not equal at all, /=, after, (not <=), and >=.
```



## Aime


```aime
text s, t;

s = "occidental";
t = "oriental";

# operator case sensitive comparison
o_form("~ vs ~ (==, !=, <, <=, >=, >): ~ ~ ~ ~ ~ ~\n", s, t, s == t, s != t, s < t, s <= t, s >= t, s > t);

s = "Oriental";
t = "oriental";

# case sensitive comparison
o_form("~ vs ~ (==, !=, <, >): ~ ~ ~ ~\n", s, t, !compare(s, t), compare(s, t), compare(s, t) < 0, 0 < compare(s, t));

# case insensitive comparison
o_form("~ vs ~ (==, !=, <, >): ~ ~ ~ ~\n", s, t, !icompare(s, t), icompare(s, t), icompare(s, t) < 0, 0 < icompare(s, t));
```

{{out}}

```txt
occidental vs oriental (==, !=, <, <=, >=, >): 0 -15 1 1 0 0
Oriental vs oriental (==, !=, <, >): 0 -32 1 0
Oriental vs oriental (==, !=, <, >): 1 0 0 0
```



## Apex


Unlike Java, Apex Strings support using the comparison operators ==, !=, <, <=, >, and >=.
Comparisons can be done also using the equals(), equalsIgnoreCase() and compareTo() methods.


```java
public class Compare
{
	/**
	 * Test in the developer console:
	 * Compare.compare('Hello', 'Hello');
	 * Compare.compare('5', '5.0');
	 * Compare.compare('java', 'Java');
	 * Compare.compare('ĴÃVÁ', 'ĴÃVÁ');
	*/

    public static void compare (String A, String B)
    {
        if (A.equals(B))
            System.debug(A + ' and  ' + B + ' are lexically equal.');
        else
            System.debug(A + ' and  ' + B + ' are not lexically equal.');

        if (A.equalsIgnoreCase(B))
            System.debug(A + ' and  ' + B + ' are case-insensitive lexically equal.');
        else
            System.debug(A + ' and  ' + B + ' are not case-insensitive lexically equal.');

        if (A.compareTo(B) < 0)
            System.debug(A + ' is lexically before ' + B);
        else if (A.compareTo(B) > 0)
            System.debug(A + ' is lexically after ' + B);

        if (A.compareTo(B) >= 0)
            System.debug(A + ' is not lexically before ' + B);
        if (A.compareTo(B) <= 0)
            System.debug(A + ' is not lexically after ' + B);

        System.debug('The lexical relationship is: ' + A.compareTo(B));
    }
}
```


{{Out}}


```txt

'Hello' and 'Hello' are lexically equal.
'Hello' and 'Hello' are case-insensitive lexically equal.
'Hello' is not lexically before 'Hello'.
'Hello' is not lexically after 'Hello'.
The lexical relationship is: 0

'5' and '5.0' are not lexically equal.
'5' and '5.0' are not case-insensitive lexically equal.
'5' is lexically before '5.0'.
'5' is not lexically after '5.0'.
The lexical relationship is: -2

'java' and 'Java' are not lexically equal.
'java' and 'Java' are case-insensitive lexically equal.
'java' is lexically after 'Java'.
'java' is not lexically before 'Java'.
The lexical relationship is: 32

'ĴÃVÁ' and 'ĴÃVÁ' are lexically equal.
'ĴÃVÁ' and 'ĴÃVÁ' are case-insensitive lexically equal.
'ĴÃVÁ' is not lexically before 'ĴÃVÁ'.
'ĴÃVÁ' is not lexically after 'ĴÃVÁ'.
The lexical relationship is: 0

```



## AppleScript


```AppleScript
--Comparing two strings for exact equality
set s1 to "this"
set s2 to "that"
if s1 is s2 then
	-- strings are equal
end if

--Comparing two strings for inequality (i.e., the inverse of exact equality)
if s1 is not s2 then
	-- string are not equal
end if

-- Comparing two strings to see if one is lexically ordered before than the other
if s1 < s2 then
	-- s1 is lexically ordered before s2
end if

-- Comparing two strings to see if one is lexically ordered after than the other
if s1 > s2 then
	-- s1 is lexically ordered after s2
end if

-- How to achieve both case sensitive comparisons and case insensitive comparisons within the language
set s1 to "this"
set s2 to "This"

considering case
	if s1 is s2 then
		-- strings are equal with case considering
	end if
end considering

ignoring case -- default
	if s2 is s2 then
		-- string are equal without case considering
	end if
end ignoring

-- Demonstrate any other kinds of string comparisons that the language provides, particularly as it relates to your type system. For example, you might demonstrate the difference between generic/polymorphic comparison and coercive/allomorphic comparison if your language supports such a distinction.

-- When comparing the right object is coerced into the same type as the object left from the operator. This implicit coercion enables to compare integers with strings (containining integer values).

set s1 to "3"
set int1 to 2

if s1 < int1 then
	-- comparison is lexically
end if

if int1 < s1 then
	-- comparison is numeric
end if
```



## ALGOL 68

{{works with|ALGOL 68G|Any - tested with release 2.8.win32}}

```algol68
STRING a := "abc   ", b := "ABC ";

# when comparing strings, Algol 68 ignores trailing blanks                    #
# so e.g. "a" = "a " is true                                                  #

# test procedure, prints message if condition is TRUE                         #
PROC test = ( BOOL condition, STRING message )VOID:
    IF condition THEN print( ( message, newline ) ) FI;

# equality?                                                                   #
test( a = b, "a = b" );
# inequality?                                                                 #
test( a /= b, "a not = b" );

# lexically ordered before?                                                   #
test( a < b, "a < b" );

# lexically ordered after?                                                    #
test( a > b, "a > b" );

# Algol 68's builtin string comparison operators are case-sensitive.          #
# To perform case insensitive comparisons, procedures or operators            #
# would need to be written                                                    #
# e.g.                                                                        #

# compare two strings, ignoring case                                          #
# Note the "to upper" PROC is an Algol 68G extension                          #
# It could be written in standard Algol 68 (assuming ASCII) as e.g.           #
#    PROC to upper = ( CHAR c )CHAR:                                          #
#         IF c < "a" OR c > "z" THEN c                                        #
#         ELSE REPR ( ( ABS c - ABS "a" ) + ABS "A" ) FI;                     #
PROC caseless comparison = ( STRING a, b )INT:
     BEGIN
         INT a max   = UPB a, b max  = UPB b;
         INT a pos  := LWB a, b pos := LWB b;
         INT result := 0;
         WHILE result = 0
           AND ( a pos <= a max OR b pos <= b max )
         DO
             CHAR a char := to upper( IF a pos <= a max THEN a[ a pos ] ELSE " " FI );
             CHAR b char := to upper( IF b pos <= b max THEN b[ b pos ] ELSE " " FI );
             result := ABS a char - ABS b char;
             a pos +:= 1;
             b pos +:= 1
         OD;
         IF result < 0 THEN -1 ELIF result > 0 THEN 1 ELSE 0 FI
     END ; # caseless comparison #

# compare two strings for equality, ignoring case                             #
PROC equal ignoring case = ( STRING a, b )BOOL: caseless comparison( a, b ) = 0;
# similar procedures for inequality and lexical ording ...                    #

test( equal ignoring case( a, b ), "a = b (ignoring case)" );


# Algol 68 is strongly typed - strings cannot be compared to e.g. integers    #
# unless procedures or operators are written, e.g.                            #
# e.g. OP = = ( STRING a, INT b )BOOL: a = whole( b, 0 );                     #
#      OP = = ( INT a, STRING b )BOOL: b = a;                                 #
# etc.                                                                        #

# Algol 68 also has <= and >= comparison operators for testing for            #
# "lexically before or equal" and "lexically after or equal"                  #
test( a <= b, "a <= b" );
test( a >= b, "a >= b" );

# there are no other forms of string comparison builtin to Algol 68           #
```

{{out}}

```txt

a not = b
a > b
a = b (ignoring case)
a >= b

```



## ALGOL W


```algolw
begin
    string(10) a;
    string(12) b;

    a := "abc";
    b := "ABC";

    % when comparing strings, Algol W ignores trailing blanks                 %
    % so e.g. "a" = "a " is true                                              %

    % equality?                                                               %
    if a = b then write( "a = b" );
    % inequality?                                                             %
    if a not = b then write( "a not = b" );

    % lexically ordered before?                                               %
    if a < b then write( "a < b" );

    % lexically ordered after?                                                %
    if a > b then write( "a > b" );

    % Algol W string comparisons are case-sensitive. To perform case          %
    % insensitive comparisons, procedures would need to be written            %
    % e.g. as in the following block (assuming the character set is ASCII)    %
    begin

        % convert a character to upper-case                                   %
        integer procedure toupper( integer value c ) ;
            if c < decode( "a" ) or c > decode( "z" ) then c
            else ( c - decode( "a" ) ) + decode( "A" );

        % compare two strings, ignoring case                                  %
        % note that strings can be at most 256 characters long in Algol W     %
        integer procedure caselessComparison ( string(256) value a, b ) ;
            begin
                integer comparisonResult, pos;
                comparisonResult := pos := 0;
                while pos < 256 and comparisonResult = 0 do begin
                    comparisonResult := toupper( decode( a(pos//1) ) )
                                      - toupper( decode( b(pos//1) ) );
                    pos := pos + 1
                end;
                if      comparisonResult < 0 then -1
                else if comparisonResult > 0 then  1
                else                               0
            end caselessComparison ;

        % compare two strings for equality, ignoring case                     %
        logical procedure equalIgnoringCase ( string(256) value a, b ) ;
            ( caselessComparison( a, b ) = 0 );

        % similar procedures for inequality and lexical ording ...           %

        if equalIgnoringCase( a, b ) then write( "a = b (ignoring case)" )
    end caselessComparison ;

    % Algol W is strongly typed - strings cannot be compared to e.g. integers %
    % e.g. "if a = 23 then ..." would be a syntax error                       %

    % Algol W also has <= and >= comparison operators for testing for         %
    % "lexically before or equal" and "lexically after or equal"              %
    if a <= b then write( "a <= b" );
    if a >= b then write( "a >= b" );

    % there are no other forms of string comparison builtin to Algol W        %

end.
```

{{out}}

```txt

a not = b
a > b
a = b (ignoring case)
a >= b

```



## ARM Assembly

{{works with|as|Raspberry Pi}}

```ARM Assembly

/* ARM assembly Raspberry PI  */
/*  program comparString.s   */

/* Constantes    */
.equ STDOUT, 1     @ Linux output console
.equ EXIT,   1     @ Linux syscall
.equ WRITE,  4     @ Linux syscall
/* Initialized data */
.data
szMessStringEqu: .asciz "The strings are equals.\n"
szMessStringNotEqu: .asciz "The strings are not equals.\n"
szCarriageReturn:  .asciz "\n"

szString1:  .asciz "ABCDE"
szString2:  .asciz "ABCDE"
szString3:  .asciz "ABCFG"
szString4:   .asciz "ABC"
szString5:   .asciz "abcde"

/* UnInitialized data */
.bss

/*  code section */
.text
.global main
main:                /* entry of program  */
    push {fp,lr}    /* saves 2 registers */

    ldr r0,iAdrszString1
    ldr r1,iAdrszString2
    bl Comparaison

    ldr r0,iAdrszString1
    ldr r1,iAdrszString3
    bl Comparaison

    ldr r0,iAdrszString1
    ldr r1,iAdrszString4
    bl Comparaison

    @ case sensitive comparisons ABCDE et abcde
    ldr r0,iAdrszString1
    ldr r1,iAdrszString5
    bl Comparaison

    @ case insensitive comparisons  ABCDE et abcde
    ldr r0,iAdrszString1
    ldr r1,iAdrszString5
    bl comparStringsInsensitive
    cmp r0,#0
    bne 1f
    ldr r0,iAdrszMessStringEqu
    bl affichageMess
    b 2f
1:
    ldr r0,iAdrszMessStringNotEqu
    bl affichageMess

2:

100:   /* standard end of the program */
    mov r0, #0                  @ return code
    pop {fp,lr}                 @restaur 2 registers
    mov r7, #EXIT              @ request to exit program
    swi 0                       @ perform the system call
iAdrszString1: .int szString1
iAdrszString2: .int szString2
iAdrszString3: .int szString3
iAdrszString4: .int szString4
iAdrszString5: .int szString5
iAdrszMessStringEqu:  .int szMessStringEqu
iAdrszMessStringNotEqu:  .int szMessStringNotEqu
iAdrszCarriageReturn:  .int  szCarriageReturn
/*********************************************/
/* comparaison                               */
/*********************************************/
/* r0 contains address String 1           */
/* r1 contains address String 2         */
Comparaison:
    push {fp,lr}    			/* save  registres */
    bl comparStrings
    cmp r0,#0
    bne 1f
    ldr r0,iAdrszMessStringEqu
    bl affichageMess
    b 2f
1:
    ldr r0,iAdrszMessStringNotEqu
    bl affichageMess

2:
    pop {fp,lr}    				/* restaur des  2 registres */
    bx lr	        			/* return  */
/******************************************************************/
/*     display text with size calculation                         */
/******************************************************************/
/* r0 contains the address of the message */
affichageMess:
    push {fp,lr}    			/* save  registres */
    push {r0,r1,r2,r7}    		/* save others registers */
    mov r2,#0   				/* counter length */
1:      	/* loop length calculation */
    ldrb r1,[r0,r2]  			/* read octet start position + index */
    cmp r1,#0       			/* if 0 its over */
    addne r2,r2,#1   			/* else add 1 in the length */
    bne 1b          			/* and loop */
                                /* so here r2 contains the length of the message */
    mov r1,r0        			/* address message in r1 */
    mov r0,#STDOUT      		/* code to write to the standard output Linux */
    mov r7, #WRITE             /* code call system "write" */
    swi #0                      /* call systeme */
    pop {r0,r1,r2,r7}     		/* restaur others registers */
    pop {fp,lr}    				/* restaur des  2 registres */
    bx lr	        			/* return  */
/************************************/
/* Strings case sensitive comparisons  */
/************************************/
/* r0 et r1 contains the address of strings */
/* return 0 in r0 if equals */
/* return -1 if string r0 < string r1 */
/* return 1  if string r0 > string r1 */
comparStrings:
    push {r1-r4}  /* save des registres */
    mov r2,#0   /* counter */
1:
    ldrb r3,[r0,r2]   /* byte string 1 */
    ldrb r4,[r1,r2]   /* byte string 2 */
    cmp r3,r4
    movlt r0,#-1	 /* small */
    movgt r0,#1	 /* greather */
    bne 100f     /* not equals */
    cmp r3,#0   /* 0 end string */
    moveq r0,#0    /* equals */
    beq 100f     /*  end string */
    add r2,r2,#1 /* else add 1 in counter */
    b 1b         /* and loop */
100:
    pop {r1-r4}
    bx lr

/************************************/
/* Strings case insensitive comparisons    */
/************************************/
/* r0 et r1 contains the address of strings */
/* return 0 in r0 if equals */
/* return -1 if string r0 < string r1 */
/* return 1  if string r0 > string r1 */
comparStringsInsensitive:
    push {r1-r4}  /* save des registres */
    mov r2,#0   /* counter */

1:
    ldrb r3,[r0,r2]   /* byte string 1 */
    ldrb r4,[r1,r2]   /* byte string 2 */
    @ majuscules --> minuscules  byte 1
    cmp r3,#65
    blt 2f
    cmp r3,#90
    bgt 2f
    add r3,#32
2:   	@ majuscules --> minuscules  byte 2
    cmp r4,#65
    blt 3f
    cmp r4,#90
    bgt 3f
    add r4,#32
3:
    cmp r3,r4
    movlt r0,#-1	 /* small */
    movgt r0,#1	 /* greather */
    bne 100f     /* not equals */
    cmp r3,#0   /* 0 end string */
    moveq r0,#0    /* equal */
    beq 100f     /* end strings */
    add r2,r2,#1 /* else add 1 in counter */
    b 1b         /* and loop */
100:
    pop {r1-r4}
    bx lr   	/* end procedure */


```



## Astro


```python
fun compare(a, b):
    print("\n$a is of type ${typeof(a)} and $b is of type ${typeof(b)}")
    if a < b: print("$a is strictly less than $b")
    if a <= b: print("$a is less than or equal to $b")
    if a >  b: print("$a is strictly greater than $b")
    if a >= b: print("$a is greater than or equal to $b")
    if a == b: print("$a is equal to $b")
    if a != b: print("$a is not equal to $b")
    if a is b: print("$a has object identity with $b")
    if a is not b: print("$a has negated object identity with $b")

compare("YUP", "YUP")
compare('a', 'z')
compare("24", "123")
compare(24, 123)
compare(5.0, 5)

```



## AWK

In awk, the string matching operators are case sensitive, and the behaviour of the comparative operators depends on the locale being used.

Be very careful with numeric strings, because whether they will be treated as numeric values or strings depends on how the values were obtained, and on which awk interpreter is being used.

Numeric strings obtained from the input source, will be treated as numeric values, when compared with other strings containing numeric values.

Strings valued defined as constants using doublequote enclosures will be treated as strings of characters and compared lexically.

The behaviour of the operators when one value is considered to be numeric (eg. from the input source), but the other value has been defined explicitly as a numeric string by using doublequote enclosures may also vary depending on which awk interpreter is being used.


```awk
BEGIN {
  a="BALL"
  b="BELL"

  if (a == b) { print "The strings are equal" }
  if (a != b) { print "The strings are not equal" }
  if (a  > b) { print "The first string is lexically after than the second" }
  if (a  < b) { print "The first string is lexically before than the second" }
  if (a >= b) { print "The first string is not lexically before than the second" }
  if (a <= b) { print "The first string is not lexically after than the second" }

  # to make a case insensitive comparison convert both strings to the same lettercase:
  a="BALL"
  b="ball"
  if (tolower(a) == tolower(b)) { print "The first and second string are the same disregarding letter case" }

}
```



## BASIC


```basic
10 LET "A$="BELL"
20 LET B$="BELT"
30 IF A$ = B$ THEN PRINT "THE STRINGS ARE EQUAL": REM TEST FOR EQUALITY
40 IF A$ <> B$ THEN PRINT "THE STRINGS ARE NOT EQUAL": REM TEST FOR INEQUALITY
50 IF A$ > B$ THEN PRINT A$;" IS LEXICALLY HIGHER THAN ";B$: REM TEST FOR LEXICALLY HIGHER
60 IF A$ < B$ THEN PRINT A$;" IS LEXICALLY LOWER THAN ";B$: REM TEST FOR LEXICALLY LOWER
70 IF A$ <= B$ THEN PRINT A$;" IS NOT LEXICALLY HIGHER THAN ";B$
80 IF A$ >= B$ THEN PRINT A$;" IS NOT LEXICALLY LOWER THAN ";B$
90 END
```


On a platform that supports both uppercase and lowercase characters, the string comparitive operators are case sensitive. To perform case insensitive matching, make sure both strings are converted to the same lettercase. Here we assume that the BASIC has the UPPER$ and LOWER$ keyword pair for case conversion. If not, then some number crunching based on the character codes is required. (In Ascii add 32 to uppercase letter codes to get the lowercase equivalent). Note that any whitespace within the strings must also match exactly for the strings to be considered equal.


```basic
10 LET A$="BELT"
20 LET B$="belt"
30 IF UPPER$(A$)=UPPER$(B$) THEN PRINT "Disregarding lettercase, the strings are the same."
```


=
## Applesoft BASIC
=
For case sensitive comparisons {{works with|Applesoft BASIC}}
Applesoft BASIC does not have a built in UPPER$ function.


## BBC BASIC


```bbcbasic>REM
strcomp
shav$ = "Shaw, George Bernard"
shakes$ = "Shakespeare, William"
:
REM test equality
IF shav$ = shakes$ THEN PRINT "The two strings are equal" ELSE PRINT "The two strings are not equal"
:
REM test inequality
IF shav$ <> shakes$ THEN PRINT "The two strings are unequal" ELSE PRINT "The two strings are not unequal"
:
REM test lexical ordering
IF shav$ > shakes$ THEN PRINT shav$; " is lexically higher than "; shakes$ ELSE PRINT shav$; " is not lexically higher than "; shakes$
IF shav$ < shakes$ THEN PRINT shav$; " is lexically lower than "; shakes$ ELSE PRINT shav$; " is not lexically lower than "; shakes$
REM the >= and <= operators can also be used, & behave as expected
:
REM string comparison is case-sensitive by default, and BBC BASIC
REM  does not provide built-in functions to convert to all upper
REM or all lower case; but it is easy enough to define one
:
IF FN_upper(shav$) = FN_upper(shakes$) THEN PRINT "The two strings are equal (disregarding case)" ELSE PRINT "The two strings are not equal (even disregarding case)"
END
:
DEF FN_upper(s$)
LOCAL i%, ns$
ns$ = ""
FOR i% = 1 TO LEN s$
  IF ASC(MID$(s$, i%, 1)) >= ASC "a" AND ASC(MID$(s$, i%, 1)) <= ASC "z" THEN ns$ += CHR$(ASC(MID$(s$, i%, 1)) - &20) ELSE ns$ += MID$(s$, i%, 1)
NEXT
= ns$
```

{{out}}

```txt
The two strings are not equal
The two strings are unequal
Shaw, George Bernard is lexically higher than Shakespeare, William
Shaw, George Bernard is not lexically lower than Shakespeare, William
The two strings are not equal (even disregarding case)
```



## Bracmat

String comparison in Bracmat is performed by string pattern matching using an atomic pattern. Bracmat has two pattern matching regimes. Originally, pattern matching was only done on tree structures, with patterns mimicking the subject tree to match. Later string pattern matching was introduced. String pattern matching is discernible from the original pattern matching by the prefix <code>@</code>. String pattern matching requires that the subject is atomic. Patterns for string matching can be as complex as patterns used for matching structures. String comparison is a very simple string pattern matching operation requiring just an atomic pattern, combined with some prefixes if needed.
The atomic pattern can be prefixed with <code>&lt;</code> (less than), <code>&gt;</code> (greater than), <code>~</code> (not) or <code>%</code> (coerces string matching) or combinations thereof. If both sides of the match operator <code>:</code> are numbers, Bracmat does a numerice comparison, unless the pattern (the rhs) has the prefix <code>%</code>.

```bracmat
( {Comparing two strings for exact equality}
& ( ( @(abc:abc)
    & @(123:%123)
    {Previous pairs of strings are exactly equal}
    )
  & ( @(abc:Abc)
    | @(123:%246/2)
    | @(abc:ab)
    | @(123:%12)
    | {Previous pairs of strings are not exactly equal}
    )
  )
  {Comparing two strings for inequality (i.e., the inverse of exact equality)}
& ( ( @(abc:~<>abc)
    & @(abc:~<>Abc)
      {Previous pairs of strings are more or less equal}
    )
  & ( @(abc:~<>ab)
    | {Previous pairs of strings are not more or less equal}
    )
  )
  {Comparing two strings to see if one is lexically ordered before than the other}
& ( ( @(Abc:<abc)
    & @(Abc:<a)
    & @(123:<%246/2)
    & @(123:<%2)
    & @(12:<%123)
    & @(ab:<abc)
      {Previous pairs of strings are lexically ordered one before the other}
    )
  & ( @(abc:<abc)
    | @(abc:<Abc)
    | @(246/2:<%123)
    | @(abc:<ab)
    | @(123:<%12)
    | @(123:<%123)
    | {Previous pairs of strings are not lexically ordered one before the other}
    )
  )
  {Comparing two strings to see if one is lexically ordered after than the other}
& ( ( @(abc:>Abc)
    & @(a:>Abc)
    & @(246/2:>%123)
    & @(2:>%123)
    & @(123:>%12)
    & @(abc:>ab)
      {Previous pairs of strings are lexically ordered one after the other}
    )
  & ( @(abc:>abc)
    | @(Abc:>abc)
    | @(123:>%246/2)
    | @(ab:>abc)
    | @(12:>%123)
    | @(123:>%123)
    | {Previous pairs of strings are not lexically ordered one after the other}
    )
  )
  {How to achieve both case sensitive comparisons and case insensitive comparisons within
   the language}
& ( ( @(abc:~<>abc)
    & @(abc:~<>Abc)
    & @(БЪЛГАРСКИ:~<>български)
      {Previous pairs of strings are more or less equal}
    )
  & ( @(abc:~<>ab)
    | {Previous pairs of strings are not more or less equal}
    )
  )
  {How the language handles comparison of numeric strings if these are not treated lexically}
& ( ( @(246/2:123)
    & @(2:<123)
    & @(123:>12)
    & @(123:246/2)
    & @(12:<123)
      {Previous numeric string comparisons succeed}
    )
  & ( @(123:<246/2)
    | @(12:>123)
    | @(123:>123)
    | @(123:~123)
    | {Previous numeric string comparisons fail}
    )
  )
  {Demonstrate any other kinds of string comparisons that the language provides, particularly
as it relates to your type system. For example, you might demonstrate the difference between
generic/polymorphic comparison and coercive/allomorphic comparison if your language supports
such a distinction.}
& ( ( @(246/2:>12--3)
    & @(2:>123kg)
    & @(123:<12d)
    & @(123:~24/6/2)
    & @(12a:>123)
      {Previous coercive string comparisons succeed}
    )
  & ( @(2013-05-01:20130501)
    | @(246/2a:123a)
    | @(1239:<123-)
    | {Previous coercive string comparisons fail}
    )
  )
& done
);
```



## Burlesque



```burlesque

blsq ) "abc""abc"==
1
blsq ) "abc""abc"!=
0
blsq ) "abc""Abc"cm
1
blsq ) "ABC""Abc"cm
-1

```


''cm'' is used for comparision which returns 1,0,-1 like C's strcmp. ''=='' is Equal and ''!='' is NotEqual.


## C

'''Solution'''
C provides the strcmp and strcasecmp functions for lexical comparison of ASCIIz strings, with declarations found in string.h .  strcmp causes a good deal of confusion because it returns 0 when the strings are equal.  Hence the likely looking common mistake

```c

/* WRONG! */
if (strcmp(a,b)) action_on_equality();

```

Wrapping strcmp with macros or functions makes good sense.  c has other functions to compare binary data, version strings, wide character strings, and strings in current locale.  These behave similarly.

```c

/*
  compilation and test in bash
  $ a=./c && make $a && $a ball bell ball ball YUP YEP     ball BELL ball BALL YUP yep
  cc -Wall -c -o c.o c.c
  	eq , ne , gt , lt , ge , le
  ball 0 1 0 1 0 1 bell
  ball 0 1 0 1 0 1 bell ignoring case
  ball 1 0 0 0 1 1 ball
  ball 1 0 0 0 1 1 ball ignoring case
  YUP 0 1 1 0 1 0 YEP
  YUP 0 1 1 0 1 0 YEP ignoring case
  ball 0 1 1 0 1 0 BELL
  ball 0 1 0 1 0 1 BELL ignoring case
  ball 0 1 1 0 1 0 BALL
  ball 1 0 0 0 1 1 BALL ignoring case
  YUP 0 1 0 1 0 1 yep
  YUP 0 1 1 0 1 0 yep ignoring case
*/

#include<string.h>

#define STREQ(A,B) (0==strcmp((A),(B)))
#define STRNE(A,B) (!STREQ(A,B))
#define STRLT(A,B) (strcmp((A),(B))<0)
#define STRLE(A,B) (strcmp((A),(B))<=0)
#define STRGT(A,B) STRLT(B,A)
#define STRGE(A,B) STRLE(B,A)

#define STRCEQ(A,B) (0==strcasecmp((A),(B)))
#define STRCNE(A,B) (!STRCEQ(A,B))
#define STRCLT(A,B) (strcasecmp((A),(B))<0)
#define STRCLE(A,B) (strcasecmp((A),(B))<=0)
#define STRCGT(A,B) STRCLT(B,A)
#define STRCGE(A,B) STRCLE(B,A)

#include<stdio.h>

void compare(const char*a, const char*b) {
  printf("%s%2d%2d%2d%2d%2d%2d %s\n",
	 a,
	 STREQ(a,b), STRNE(a,b), STRGT(a,b), STRLT(a,b), STRGE(a,b), STRLE(a,b),
	 b
	 );
}
void comparecase(const char*a, const char*b) {
  printf("%s%2d%2d%2d%2d%2d%2d %s ignoring case\n",
	 a,
	 STRCEQ(a,b), STRCNE(a,b), STRCGT(a,b), STRCLT(a,b), STRCGE(a,b), STRCLE(a,b),
	 b
	 );
}
int main(int ac, char*av[]) {
  char*a,*b;
  puts("\teq , ne , gt , lt , ge , le");
  while (0 < (ac -= 2)) {
    a = *++av, b = *++av;
    compare(a, b);
    comparecase(a, b);
  }
  return 0;
}

```



## C++


```cpp
#include <algorithm>
#include <iostream>
#include <sstream>
#include <string>

template <typename T>
void demo_compare(const T &a, const T &b, const std::string &semantically) {
    std::cout << a << " and " << b << " are " << ((a == b) ? "" : "not ")
              << "exactly " << semantically << " equal." << std::endl;

    std::cout << a << " and " << b << " are " << ((a != b) ? "" : "not ")
              << semantically << "inequal." << std::endl;

    std::cout << a << " is " << ((a < b) ? "" : "not ") << semantically
              << " ordered before " << b << '.' << std::endl;

    std::cout << a << " is " << ((a > b) ? "" : "not ") << semantically
              << " ordered after " << b << '.' << std::endl;
}

int main(int argc, char *argv[]) {
    // Case-sensitive comparisons.
    std::string a((argc > 1) ? argv[1] : "1.2.Foo");
    std::string b((argc > 2) ? argv[2] : "1.3.Bar");
    demo_compare<std::string>(a, b, "lexically");

    // Case-insensitive comparisons by folding both strings to a common case.
    std::transform(a.begin(), a.end(), a.begin(), ::tolower);
    std::transform(b.begin(), b.end(), b.begin(), ::tolower);
    demo_compare<std::string>(a, b, "lexically");

    // Numeric comparisons; here 'double' could be any type for which the
    // relevant >> operator is defined, eg int, long, etc.
    double numA, numB;
    std::istringstream(a) >> numA;
    std::istringstream(b) >> numB;
    demo_compare<double>(numA, numB, "numerically");
    return (a == b);
}
```

{{out}}

```txt
1.2.Foo and 1.3.Bar are not exactly lexically equal.
1.2.Foo and 1.3.Bar are lexicallyinequal.
1.2.Foo is lexically ordered before 1.3.Bar.
1.2.Foo is not lexically ordered after 1.3.Bar.
1.2.foo and 1.3.bar are not exactly lexically equal.
1.2.foo and 1.3.bar are lexicallyinequal.
1.2.foo is lexically ordered before 1.3.bar.
1.2.foo is not lexically ordered after 1.3.bar.
1.2 and 1.3 are not exactly numerically equal.
1.2 and 1.3 are numericallyinequal.
1.2 is numerically ordered before 1.3.
1.2 is not numerically ordered after 1.3.
```



## Clipper

We will compare two strings, ''s1'' and ''s2''. The following comparisons are case sensitive.

```clipper
   IF s1 == s2
      ? "The strings are equal"
   ENDIF
   IF .NOT. (s1 == s2)
      ? "The strings are not equal"
   ENDIF
   IF s1 > s2
      ? "s2 is lexically ordered before than s1"
   ENDIF
   IF s1 < s2
      ? "s2 is lexically ordered after than s1"
   ENDIF
```

To achieve case insensitive comparisons, we should use Upper() or Lower() functions:

```clipper
   IF Upper(s1) == Upper(s2)
      ? "The strings are equal"
   ENDIF

```



## COBOL

Strings can be compared using the normal conditional syntax, like so:

```cobol
"hello" = "hello"   *> equality
"helloo" <> "hello" *> inequality
"aello" < "hello"   *> lexical ordering
```


COBOL 2002 introduced the intrinsic functions <code>LOCALE-COMPARE</code> and <code>STANDARD-COMPARE</code>, which return one of the strings <code>"="</code>, <code>">"</code> or <code>"<"</code> depending on their parameters.

```cobol
FUNCTION STANDARD-COMPARE("hello", "hello") *> "="
FUNCTION STANDARD-COMPARE("aello", "hello") *> "<"
FUNCTION STANDARD-COMPARE("hello", "aello") *> ">"
```


Trailing spaces in strings are removed when strings are compared. However, if the strings are then of unequal length, then the shorter string is padded with spaces.

```cobol
"hello  " = "hello" *> True
X"00" > X"0000" *> True
```



## ColdFusion


* '''Less than:''' LT
* '''Less than or equal to:''' LTE
* '''Greater than:''' GT
* '''Greater than or equal to:''' GTE
* '''Equal to:''' EQ
* '''Not equal to:''' NEQ


### In CFML



```cfm
<cffunction name="CompareString">
    <cfargument name="String1" type="string">
    <cfargument name="String2" type="string">
    <cfset VARIABLES.Result = "" >
    <cfif ARGUMENTS.String1 LT ARGUMENTS.String2 >
	    <cfset VARIABLES.Result = VARIABLES.Result & "('" & ARGUMENTS.String1 & "' is less than '" & ARGUMENTS.String2 & "')" >
    </cfif>
    <cfif ARGUMENTS.String1 LTE ARGUMENTS.String2 >
	    <cfset VARIABLES.Result = VARIABLES.Result & "('" & ARGUMENTS.String1 & "' is less than or equal to '" & ARGUMENTS.String2 & "')" >
    </cfif>
    <cfif ARGUMENTS.String1 GT ARGUMENTS.String2 >
	    <cfset VARIABLES.Result = VARIABLES.Result & "('" & ARGUMENTS.String1 & "' is greater than '" & ARGUMENTS.String2 & "')" >
    </cfif>
    <cfif ARGUMENTS.String1 GTE ARGUMENTS.String2 >
	    <cfset VARIABLES.Result = VARIABLES.Result & "('" & ARGUMENTS.String1 & "' is greater than or equal to '" & ARGUMENTS.String2 & "')" >
    </cfif>
    <cfif ARGUMENTS.String1 EQ ARGUMENTS.String2 >
	    <cfset VARIABLES.Result = VARIABLES.Result & "('" & ARGUMENTS.String1 & "' is equal to '" & ARGUMENTS.String2 & "')" >
    </cfif>
    <cfif ARGUMENTS.String1 NEQ ARGUMENTS.String2 >
	    <cfset VARIABLES.Result = VARIABLES.Result & "('" & ARGUMENTS.String1 & "' is not equal to '" & ARGUMENTS.String2 & "')" >
    </cfif>
    <cfreturn VARIABLES.Result >
</cffunction>
```



### In CFScript



```cfm><cfscript

	function CompareString( String1, String2 ) {
		VARIABLES.Result = "";
		if ( ARGUMENTS.String1 LT ARGUMENTS.String2 ) {
			VARIABLES.Result = VARIABLES.Result & "('" & ARGUMENTS.String1 & "' is less than '" & ARGUMENTS.String2 & "')";
		}
		if ( ARGUMENTS.String1 LTE ARGUMENTS.String2 ) {
			VARIABLES.Result = VARIABLES.Result & "('" & ARGUMENTS.String1 & "' is less than or equal to '" & ARGUMENTS.String2 & "')";
		}
		if ( ARGUMENTS.String1 GT ARGUMENTS.String2 ) {
			VARIABLES.Result = VARIABLES.Result & "('" & ARGUMENTS.String1 & "' is greater than '" & ARGUMENTS.String2 & "')";
		}
		if ( ARGUMENTS.String1 GTE ARGUMENTS.String2 ) {
			VARIABLES.Result = VARIABLES.Result & "('" & ARGUMENTS.String1 & "' is greater than or equal to '" & ARGUMENTS.String2 & "')";
		}
		if ( ARGUMENTS.String1 EQ ARGUMENTS.String2 ) {
			VARIABLES.Result = VARIABLES.Result & "('" & ARGUMENTS.String1 & "' is equal to '" & ARGUMENTS.String2 & "')";
		}
		if ( ARGUMENTS.String1 NEQ ARGUMENTS.String2 ) {
			VARIABLES.Result = VARIABLES.Result & "('" & ARGUMENTS.String1 & "' is not equal to '" & ARGUMENTS.String2 & "')";
		}
		return VARIABLES.Result;
	}
</cfscript>
```



## Common Lisp

There are case-sensitive and case-insensitive comparison functions. All inequality comparison functions return an integer instead of simply <code>T</code> (for true) which is the position of the first character at which the strings differ.

Case-sensitive comparison functions:

```lisp>
(string= "foo" "foo")
T
> (string= "foo" "FOO")
NIL
> (string/= "foo" "bar")
0
> (string/= "bar" "baz")
2
> (string/= "foo" "foo")
NIL
> (string> "foo" "Foo")
0
> (string< "foo" "Foo")
NIL
> (string>= "FOo" "Foo")
NIL
> (string<= "FOo" "Foo")
1
```


Case-insensitive comparison functions:

```lisp>
 (string-equal "foo" "FOo")
T
> (string-not-equal "foo" "FOO")
NIL
> (string-greaterp "foo" "Foo")
NIL
> (string-lessp "BAR" "foo")
0
> (string-not-greaterp "foo" "Foo")
3
> (string-not-lessp "baz" "bAr")
2
```


Numeric strings are always compared lexically:

```lisp>
 (string> "45" "12345")
0
> (string> "45" "9")
NIL
```



## Component Pascal

BlackBox Component Builder

```oberon2
MODULE StringComparision;
IMPORT StdLog,Strings;

PROCEDURE Do*;
VAR
	str1,str2,aux1,aux2: ARRAY 128 OF CHAR;
BEGIN
	str1 := "abcde";str2 := "abcde";
	StdLog.String(str1+" equals " + str2  + ":> ");StdLog.Bool(str1 = str2);StdLog.Ln;
	str2 := "abcd";
	StdLog.String(str1+" equals " + str2  + ":> ");StdLog.Bool(str1 = str2);StdLog.Ln;
	StdLog.String(str1+" greater than " + str2  + ":> ");StdLog.Bool(str1 > str2);StdLog.Ln;
	StdLog.String(str1+" lower than " + str2  + ":> ");StdLog.Bool(str1 < str2);StdLog.Ln;

	str2 := "ABCDE";
	StdLog.String(str1+" equals " + str2  + ":> ");StdLog.Bool(str1 = str2);StdLog.Ln;
	StdLog.String(str1+" greater than " + str2  + ":> ");StdLog.Bool(str1 > str2);StdLog.Ln;
	StdLog.String(str1+" lower than " + str2  + ":> ");StdLog.Bool(str1 < str2);StdLog.Ln;

	Strings.ToLower(str1,aux1);Strings.ToLower(str2,aux2);
	StdLog.String(str1+" equals (case insensitive) " + str2  + ":> ");StdLog.Bool(aux1 = aux2);StdLog.Ln;

	str1 := "01234";str2 := "01234";
	StdLog.String(str1+" equals " + str2  + ":> ");StdLog.Bool(str1 = str2);StdLog.Ln;
	str2 := "0123";
	StdLog.String(str1+" equals " + str2  + ":> ");StdLog.Bool(str1 = str2);StdLog.Ln;
	StdLog.String(str1+" greater than " + str2  + ":> ");StdLog.Bool(str1 > str2);StdLog.Ln;
	StdLog.String(str1+" lower than " + str2  + ":> ");StdLog.Bool(str1 < str2);StdLog.Ln;
END Do;

END StringComparision.
```

Execute: ^Q StringComparision.Do<br/>
Output:

```txt
abcde equals abcde:>  $TRUE
abcde equals abcd:>  $FALSE
abcde greater than abcd:>  $TRUE
abcde lower than abcd:>  $FALSE
abcde equals ABCDE:>  $FALSE
abcde greater than ABCDE:>  $TRUE
abcde lower than ABCDE:>  $FALSE
abcde equals (case insensitive) ABCDE:>  $TRUE
01234 equals 01234:>  $TRUE
01234 equals 0123:>  $FALSE
01234 greater than 0123:>  $TRUE
01234 lower than 0123:>  $FALSE
```



## D

See also [[Empty_string]]

```d
import std.stdio, std.string, std.algorithm;

void main() {
    auto s = "abcd";

    /* Comparing two strings for exact equality */
    assert (s == "abcd"); // same object

    /* Comparing two strings for inequality */
    assert(s != "ABCD"); // different objects

    /* Comparing the lexical order of two strings;
    -1 means smaller, 0 means equal, 1 means larger */

    assert(s.icmp("Bcde") == -1); // case insensitive
    assert(s.cmp("Bcde") == 1); // case sensitive

    assert(s.icmp("Aabc") == 1); // case insensitive
    assert(s.cmp("Aabc") == 1); // case sensitive

    assert(s.icmp("ABCD") == 0); // case insensitive
    assert(s.cmp("ABCD") == 1); // case sensitive
}
```


=={{Header|Dyalect}}==

{{trans|Swift}}


```Dyalect
func compare(a, b) {
  if a == b {
    print("'\(a)' and '\(b)' are lexically equal.")
  }
  if a != b {
    print("'\(a)' and '\(b)' are not lexically equal.")
  }

  if a < b {
    print("'\(a)' is lexically before '\(b)'.")
  }
  if a > b {
    print("'\(a)' is lexically after '\(b)'.")
  }

  if a >= b {
    print("'\(a)' is not lexically before '\(b)'.")
  }
  if a <= b {
    print("'\(a)' is not lexically after '\(b)'.")
  }
}
compare("cat", "dog")
```

{{Out}}

```txt
'cat' and 'dog' are not lexically equal.
'cat' is lexically before 'dog'.
'cat' is not lexically after 'dog'.

```



## Elena

ELENA 4.x:

```elena
import extensions;

compareStrings = (val1,val2)
{
    if (val1 == val2) { console.printLine("The strings ",val1," and ",val2," are equal") };
    if (val1 != val2) { console.printLine("The strings ",val1," and ",val2," are not equal") };
    if (val1  > val2) { console.printLine("The string ",val1," is lexically after than ",val2) };
    if (val1  < val2) { console.printLine("The string ",val1," is lexically before than ",val2) };
    if (val1 >= val2) { console.printLine("The string ",val1," is not lexically before than ",val2) };
    if (val1 <= val2) { console.printLine("The string ",val1," is not lexically after than ",val2) }
};

public program()
{
    var s1 := "this";
    var s2 := "that";
    compareStrings(s1,s2);

    console.readChar()
}
```



## Elixir


```elixir
s = "abcd"
s == "abcd"         #=> true
s == "abce"         #=> false
s != "abcd"         #=> false
s != "abce"         #=> true
s >  "abcd"         #=> false
s <  "abce"         #=> true
s >= "abce"         #=> false
s <= "abce"         #=> true
```



## Erlang

Examples from Erlang shell:


```Erlang

10> V = "abcd".
"abcd"
11> V =:= "abcd".
true
12> V =/= "abcd".
false
13> V < "b".
true
15> V > "aa".
true
16> string:to_lower(V) =:= string:to_lower("ABCD").
true

```



## Fortran

Early Fortran offered no facilities for manipulating text, only numbers, though the FORMAT statement could present text via the "Hollerith" format code of ''n''H, where ''n'' characters follow the H, as in
```Fortran
      PRINT 42,N
   42 FORMAT (14HThe answer is ,I9)
```
 - though the use of lower-case here is anachronistic. There was an odd facility whereby using such a FORMAT statement in a READ statement would cause the Hollerith text to be replaced by what was read in, and this new text could be written out by a later PRINT statement - but the program could not inspect the text at all. So no string comparison.

Fortran IV introduced the A''w'' format code, where ''w'' was an integer such as one or two, and this transferred the bit pattern "as is" to or from a variable in a READ or WRITE statement. A sixteen-bit integer would suit either A1 or A2, a thirty-two bit floating-point variable could hold up to four eight-bit character codes, and so on, though with caution because some computers had eighteen-bit words and others forty-eight bit words, not just powers of two. An array of integers might be used to hold a line of text, and A1 format (one character per integer) would be easier for manipulation, while A2 would use less storage. The variables could be compared as numerical values and so string comparison was possible. However, the numerical values would be quite strange, because A1 format would place the bit pattern at the high-order end of the word (where the sign bit would be found in integers), and with floating-point variables the resulting values would be even more surprising. On the B6700, the high-order bit of a 48-bit word was not employed in arithmetic at all. Even so, in this period, interpreters for SNOBOL (surely the epitome of string-manipulation languages) were often written in Fortran, because of its portability. So, string comparison in the same way as number comparison.

Fortran 66 introduced the LOGICAL type and the logical-IF statement, that used comparison operations: mnemonics "stropped" by periods: <code>.LT.</code> <code>.LE.</code> <code>.EQ.</code> <code>.NE.</code> <code>.GE.</code> <code>.GT.</code> and more flexible compilers (F90 and later) also recognise respectively <code><</code> <code><=</code> <code>==</code> (a single = being committed to representing assignment) <code>/=</code> (most ASCII keyboards lacking a ¬ character, present on IBM keyboards for EBCDIC) <code>>=</code> <code>></code>. The scope of these operations was extended when Fortran 77 introduced the CHARACTER*''n'' TEXT type, whereby a variable was declared to have a fixed amount of space, of ''n'' characters, and trailing spaces were usual. There is no string "length" attribute and LEN(TEXT) does not report the current length of the string but its size, which remains ''n''. F90 however introduced facilities whereby a character variable could be redefined to have the required size each time it has a value assigned to it, and this scheme became part of the language with F2003.

Character string comparison is straightforward when both entities are character, and the usage has the same form as when both are numeric. There is no facility for comparing a numeric value such as 12345 to a character sequence "12345" because these are of incompatible types with very different bit patterns. You would have to convert the number to a character sequence, or the character sequence to a number - which last is complicated by the possibility of character sequences not presenting a valid number, as in "12three45". Thus, the comparison operators are polymorphic in application (to characters or to numbers) but unbending in use as a comparison can only be made of the same type entities. Similarly, although the existence of > ''etc.'' implies the operation of subtraction, this is not allowed for character variables and so the three-way choice of result via the arithmetic-IF is unavailable.

Exact matching is problematic, because trailing spaces are disregarded so that <code>"blah" .EQ. "blah   "</code> yields ''true''. Thus, the quality of equality is strained. To test for "exact" equality the lengths would have to be compared also, somewhat as in <code>TEXT1.EQ.TEXT2 .AND. LEN(TEXT1).EQ.LEN(TEXT2)</code>

All character comparisons are literal: case counts. There is no facility for case insensitive comparison (though in principle a compiler could offer to do so via non-standard mnemonics) and there often are no library routines for case conversion. The usual procedure is to copy both items to scratch variables (with all the annoyance of "how big?"), convert both to upper case (or both to lower case) and then compare. Or, rather than copying the strings, one might code for a character-by-character comparison and handling case differences one character at a time. With single-character comparison one can use ICHAR(''c'') to obtain the numerical value of the character code, and this enables the use of the three-way test of the arithmetical-IF as in <code>IF (ICHAR(TEXT1(L:L)) - ICHAR(TEXT2(L:L))) ''negative'',''equal'',''positive''</code>, where ''negative'' would be the statement label jumped to should character L of TEXT2 be greater than character L of TEXT1. With equality, one would increment L and after checking that TEXT1 and TEXT2 had another character L available, test afresh.

To accommodate case insensitivity, one could use an AND operation to mask off the bits distinguishing a lower case letter from an upper case letter, but should non-letter characters be involved, this may mask other differences as well. Instead, prepare an array of 256 integers, say UC, where UC(''i'') = ''i'' except for those indices corresponding to the character code values of the lower case letters, for which the array has instead the value of the corresponding upper case letter. Then the comparison might be <code>IF (UC(ICHAR(TEXT1(L:L))) - UC(ICHAR(TEXT2(L:L)))) ''negative'',''equal'',''positive''</code> so that case insensitivity is achieved without the annoyance of multiple testing or case conversion but at the cost of array access. And by re-arranging values in array UC, a custom ordering of the character codes could be achieved at no extra cost.


## FreeBASIC


```freebasic
' FB 1.05.0

' Strings in FB natively support the relational operators which compare lexically on a case-sensitive basis.
' There are no special provisions for numerical strings.
' There are no other types of string comparison for the built-in types though 'user defined types'
' can specify their own comparisons by over-loading the relational operators.

Function StringCompare(s1 As Const String, s2 As Const String, ignoreCase As Boolean = false) As String
  Dim As String s, t ' need new string variables as the strings passed in can't be changed
  If ignoreCase Then
    s = LCase(s1)
    t = LCase(s2)
  Else
    s = s1
    t = s2
  End If
  If s < t Then Return " comes before "
  If s = t Then Return " is equal to "
  Return " comes after "
End Function

Dim As Integer result
Dim As String s1, s2, s3
s1 = "Dog" : s2 = "Dog"
Print s1; StringCompare(s1, s2); s2
s2 = "Cat"
Print s1; StringCompare(s1, s2); s2
s2 = "Rat"
Print s1; StringCompare(s1, s2); s2
s2 = "dog"
Print s1; StringCompare(s1, s2); s2
Print s1; StringCompare(s1, s2, True); s2; " if case is ignored"
s1  = "Dog" : s2 = "Pig"
s3 = StringCompare(s1, s2)
If s3 <> " is equal to " Then
  Print s1; " is not equal to "; s2
End If
Print
Print "Press any key to quit"
Sleep
```


{{out}}

```txt

c:\FreeBasic>stringcompare
Dog is equal to Dog
Dog comes after Cat
Dog comes before Rat
Dog comes before dog
Dog is equal to dog if case is ignored
Dog is not equal to Pig

```


=={{header|F_Sharp|F#}}==
As a .NET language F# can make use of the [http://msdn.microsoft.com/en-us/library/system.string System.String] class.
As strict strongly typed language F# never coerces any other type to string.
<tt>System.String</tt> implements <tt>Compare</tt> function variants which are told by a
[http://msdn.microsoft.com/en-us/library/system.stringcomparison StringComparison] enumeration value how to compare, which might be "culture sensitive" or use an "ordinal comparison".
Both of these might also be of the <tt>IgnoreCase</tt> variant.

```fsharp
open System

// self defined operators for case insensitive comparison
let (<~) a b  = String.Compare(a, b, StringComparison.OrdinalIgnoreCase) < 0
let (<=~) a b = String.Compare(a, b, StringComparison.OrdinalIgnoreCase) <= 0
let (>~) a b  = String.Compare(a, b, StringComparison.OrdinalIgnoreCase) > 0
let (>=~) a b = String.Compare(a, b, StringComparison.OrdinalIgnoreCase) >= 0
let (=~) a b  = String.Compare(a, b, StringComparison.OrdinalIgnoreCase) = 0
let (<>~) a b = String.Compare(a, b, StringComparison.OrdinalIgnoreCase) <> 0

let compare a b =   // standard operators:
    if a <  b then printfn "%s is strictly less than %s" a b
    if a <= b then printfn "%s is less than or equal to %s" a b
    if a >  b then printfn "%s is strictly greater than %s" a b
    if a >= b then printfn "%s is greater than or equal to %s" a b
    if a =  b then printfn "%s is equal to %s" a b
    if a <> b then printfn "%s is not equal to %s" a b
    // and our case insensitive self defined operators:
    if a <~  b then printfn "%s is strictly less than %s (case insensitive)" a b
    if a <=~ b then printfn "%s is less than or equal to %s (case insensitive)" a b
    if a >~  b then printfn "%s is strictly greater than %s (case insensitive)" a b
    if a >=~ b then printfn "%s is greater than or equal to %s (case insensitive)" a b
    if a =~  b then printfn "%s is equal to %s (case insensitive)" a b
    if a <>~ b then printfn "%s is not equal to %s (case insensitive)" a b


[<EntryPoint>]
let main argv =
    compare "YUP" "YUP"
    compare "BALL" "BELL"
    compare "24" "123"
    compare "BELL" "bELL"
    0
```

Output
<pre style="font-size:smaller">YUP is less than or equal to YUP
YUP is greater than or equal to YUP
YUP is equal to YUP
YUP is less than or equal to YUP (case insensitive)
YUP is greater than or equal to YUP (case insensitive)
YUP is equal to YUP (case insensitive)
BALL is strictly less than BELL
BALL is less than or equal to BELL
BALL is not equal to BELL
BALL is strictly less than BELL (case insensitive)
BALL is less than or equal to BELL (case insensitive)
BALL is not equal to BELL (case insensitive)
24 is strictly greater than 123
24 is greater than or equal to 123
24 is not equal to 123
24 is strictly greater than 123 (case insensitive)
24 is greater than or equal to 123 (case insensitive)
24 is not equal to 123 (case insensitive)
BELL is strictly less than bELL
BELL is less than or equal to bELL
BELL is not equal to bELL
BELL is less than or equal to bELL (case insensitive)
BELL is greater than or equal to bELL (case insensitive)
BELL is equal to bELL (case insensitive)
```



## Factor

Strings in Factor are just sequences of unicode code points, so the usual sequence operations apply to strings. The <tt><=></tt> word from the <tt>math.order</tt> vocabulary can be used to lexically compare strings, and Factor includes the <tt>human<=></tt> word in the <tt>sorting.human</tt> vocabulary for comparing numeric strings like a human would.


```factor
USING: ascii math.order sorting.human ;

IN: scratchpad "foo" "bar" = . ! compare for equality
f
IN: scratchpad "foo" "bar" = not . ! compare for inequality
t
IN: scratchpad "foo" "bar" before? . ! lexically ordered before?
f
IN: scratchpad "foo" "bar" after? . ! lexically ordered after?
t
IN: scratchpad "Foo" "foo" <=> . ! case-sensitive comparison
+lt+
IN: scratchpad "Foo" "foo" [ >lower ] bi@ <=> . ! case-insensitive comparison
+eq+
IN: scratchpad "a1" "a03" <=> . ! comparing numeric strings
+gt+
IN: scratchpad "a1" "a03" human<=> . ! comparing numeric strings like a human
+lt+
```




## Falcon

'''VBA/Python programmer's approach.  I'm just a junior Falconeer but this code seems to go the falcon way''

```falcon

/* created by Aykayayciti Earl Lamont Montgomery
April 9th, 2018 */

e = "early"
l = "toast"
g = "cheese"
b = "cheese"
e2 = "early"
num1 = 123
num2 = 456

> e == e2 ? @ "$e equals $e2" : @ "$e does not equal $e2"
> e != e2 ? @ "$e does not equal $e2": @ "$e equals $e2"
// produces -1 for less than
> b.cmpi(l) == 1 ? @ "$b is grater than $l" : @ "$l is grater than $b"
// produces 1 for greater than
> l.cmpi(b) == 1 ? @ "$l is grater than $b" : @ "$b is grater than $l"
// produces 0 for equal (but could be greater than or equal)
> b.cmpi(g) == 1 or b.cmpi(g) == 0 ? @ "$b is grater than or equal to $g" : @ "$b is not >= $g"
// produces 0 for equal (but could be less than or equal)
>b.cmpi(g) == -1 or b.cmpi(g) == 0 ? @ "$b is less than or equal to $g" : @ "$b is not <= $g"

function NumCompare(num1, num2)
	if num1 < num2
		ans = " < "
	elif num1 > num2
		ans =  " > "
	else
		ans =  " = "
	end
	return ans
end

result = NumCompare(num1, num2)
> @ "$num1 $result $num2"

```

{{out}}

```txt

early equals early
early equals early
toast is grater than cheese
toast is grater than cheese
cheese is grater than or equal to cheese
cheese is less than or equal to cheese
123  <  456
[Finished in 0.2s]

```



## Forth

The ANS Forth standard has the word COMPARE to lexically compare two strings, with the same behavior as the C standard library strcmp() function.


```Forth
: str-eq  ( str len str len -- ? ) compare 0= ;
: str-neq ( str len str len -- ? ) compare 0<> ;
: str-lt  ( str len str len -- ? ) compare 0< ;
: str-gt  ( str len str len -- ? ) compare 0> ;
: str-le  ( str len str len -- ? ) compare 0<= ;
: str-ge  ( str len str len -- ? ) compare 0>= ;
```


Although many Forths allow case-insensitive lookup of ASCII dictionary names for function and variable names (FIND, SEARCH-WORDLIST), this capability is not exposed for other uses in a standard way.


## Go


```go
package main

import (
    "fmt"
    "strings"
)

func main() {
    // Go language string comparison operators:
    c := "cat"
    d := "dog"
    if c == d {
        fmt.Println(c, "is bytewise identical to", d)
    }
    if c != d {
        fmt.Println(c, "is bytewise different from", d)
    }
    if c > d {
        fmt.Println(c, "is lexically bytewise greater than", d)
    }
    if c < d {
        fmt.Println(c, "is lexically bytewise less than", d)
    }
    if c >= d {
        fmt.Println(c, "is lexically bytewise greater than or equal to", d)
    }
    if c <= d {
        fmt.Println(c, "is lexically bytewise less than or equal to", d)
    }
    // Go is strongly typed and will not directly compare a value of string
    // type to a value of numeric type.

    // A case insensitive compare can be done with a function in the strings
    // package in the Go standard library:
    eqf := `when interpreted as UTF-8 and compared under Unicode
simple case folding rules.`
    if strings.EqualFold(c, d) {
        fmt.Println(c, "equal to", d, eqf)
    } else {
        fmt.Println(c, "not equal to", d, eqf)
    }

    // Seeing that the built in operators work bytewise and the library
    // case folding functions interpret UTF-8, you might then ask about
    // other equality and inequality tests that interpret UTF-8.
    // Functions for this are not in the Go standard library but are in
    // the Go "sub repository" at golang.org/x/text.  There is support
    // for Unicode normalization, collation tables, and locale sensitive
    // comparisons.
}
```

{{out}}

```txt

cat is bytewise different from dog
cat is lexically bytewise less than dog
cat is lexically bytewise less than or equal to dog
cat not equal to dog when interpreted as UTF-8 and compared under Unicode
simple case folding rules.

```



## Harbour

We will compare two strings, ''s1'' and ''s2''. The following comparisons are case sensitive.

```visualfoxpro
IF s1 == s2
   ? "The strings are equal"
ENDIF
IF !( s1 == s2 )
   ? "The strings are not equal"
ENDIF
IF s1 > s2
   ? "s2 is lexically ordered before than s1"
ENDIF
IF s1 < s2
   ? "s2 is lexically ordered after than s1"
ENDIF
```

To achieve case insensitive comparisons, we should use Upper() or Lower() functions:

```visualfoxpro
IF Upper( s1 ) == Upper( s2 )
   ? "The strings are equal"
ENDIF
```



## Haskell

Examples from the Haskell shell:

```haskell

> "abc" == "abc"
True
> "abc" /= "abc"
False
> "abc" <= "abcd"
True
> "abc" <= "abC"
False
> "HELLOWORLD" == "HelloWorld"
False
> :m +Data.Char
> map toLower "ABC"
"abc"
> map toLower "HELLOWORLD" == map toLower "HelloWorld"
True

```


=={{header|Icon}} and {{header|Unicon}}==

Same in both languages.


```unicon
procedure main(A)
    s1 := A[1] | "a"
    s2 := A[2] | "b"
    # These first four are case-sensitive
    s1 == s2        # Are they equal?
    s1 ~== s2       # Are they unequal?
    s1 << s2         # Does s1 come before s2?
    s1 >> s2         # Does s1 come after s2?
    map(s1) == map(s2)  # Caseless comparison
    "123" >> "12"    # Lexical comparison
    "123" > "12"     # Numeric comparison
    "123" >> 12      # Lexical comparison (12 coerced into "12")
    "123" > 12       # Numeric comparison ("123" coerced into 123)
end
```



## J

'''Solution:'''
The primitive <code>-:</code> can be used to determine whether two strings are equivalent, but J doesn't have other inbuilt lexical comparison operators. They can defined as follows:

```j
eq=: -:                         NB. equal
ne=: -.@-:                      NB. not equal
gt=: {.@/:@,&boxopen *. ne      NB. lexically greater than
lt=: -.@{.@/:@,&boxopen *. ne   NB. lexically less than
ge=: {.@/:@,&boxopen +. eq      NB. lexically greater than or equal to
le=: -.@{.@/:@,&boxopen         NB. lexically less than or equal to
```


'''Usage:'''

```j
   'ball' (eq , ne , gt , lt , ge , le) 'bell'
0 1 0 1 0 1
   'ball' (eq , ne , gt , lt , ge , le) 'ball'
1 0 0 0 1 1
   'YUP' (eq , ne , gt , lt , ge , le) 'YEP'
0 1 1 0 1 0
```


=={{Header|Java}}==
A String object in Java represents a UTF-16 string.
Comparisons are done using the equals(), equalsIgnoreCase(), compareTo(), and compareToIgnoreCase() methods.

```java
public class Compare
{
    public static void main (String[] args)
    {
        compare("Hello", "Hello");
        compare("5", "5.0");
        compare("java", "Java");
        compare("ĴÃVÁ", "ĴÃVÁ");
        compare("ĴÃVÁ", "ĵãvá");
    }
    public static void compare (String A, String B)
    {
        if (A.equals(B))
            System.out.printf("'%s' and '%s' are lexically equal.", A, B);
        else
            System.out.printf("'%s' and '%s' are not lexically equal.", A, B);
        System.out.println();

        if (A.equalsIgnoreCase(B))
            System.out.printf("'%s' and '%s' are case-insensitive lexically equal.", A, B);
        else
            System.out.printf("'%s' and '%s' are not case-insensitive lexically equal.", A, B);
        System.out.println();

        if (A.compareTo(B) < 0)
            System.out.printf("'%s' is lexically before '%s'.\n", A, B);
        else if (A.compareTo(B) > 0)
            System.out.printf("'%s' is lexically after '%s'.\n", A, B);

        if (A.compareTo(B) >= 0)
            System.out.printf("'%s' is not lexically before '%s'.\n", A, B);
        if (A.compareTo(B) <= 0)
            System.out.printf("'%s' is not lexically after '%s'.\n", A, B);

        System.out.printf("The lexical relationship is: %d\n", A.compareTo(B));
        System.out.printf("The case-insensitive lexical relationship is: %d\n\n", A.compareToIgnoreCase(B));
    }
}
```

{{Out}}

```txt
'Hello' and 'Hello' are lexically equal.
'Hello' and 'Hello' are case-insensitive lexically equal.
'Hello' is not lexically before 'Hello'.
'Hello' is not lexically after 'Hello'.
The lexical relationship is: 0
The case-insensitive lexical relationship is: 0

'5' and '5.0' are not lexically equal.
'5' and '5.0' are not case-insensitive lexically equal.
'5' is lexically before '5.0'.
'5' is not lexically after '5.0'.
The lexical relationship is: -2
The case-insensitive lexical relationship is: -2

'java' and 'Java' are not lexically equal.
'java' and 'Java' are case-insensitive lexically equal.
'java' is lexically after 'Java'.
'java' is not lexically before 'Java'.
The lexical relationship is: 32
The case-insensitive lexical relationship is: 0

'ĴÃVÁ' and 'ĴÃVÁ' are lexically equal.
'ĴÃVÁ' and 'ĴÃVÁ' are case-insensitive lexically equal.
'ĴÃVÁ' is not lexically before 'ĴÃVÁ'.
'ĴÃVÁ' is not lexically after 'ĴÃVÁ'.
The lexical relationship is: 0
The case-insensitive lexical relationship is: 0

'ĴÃVÁ' and 'ĵãvá' are not lexically equal.
'ĴÃVÁ' and 'ĵãvá' are case-insensitive lexically equal.
'ĴÃVÁ' is lexically before 'ĵãvá'.
'ĴÃVÁ' is not lexically after 'ĵãvá'.
The lexical relationship is: -1
The case-insensitive lexical relationship is: 0
```



## jq

jq strings are JSON strings.  The jq comparison operators (==, !=, <, <=, >=, >) can be used to compare strings or any JSON entities. Similarly, jq's <tt>sort</tt> and <tt>unique</tt> filters can be used to sort strings. The ordering of strings is determined by the Unicode codepoints.


```jq
# Comparing two strings for exact equality:
"this" == "this"     # true
"this" == "This"     # false

# != is the inverse of ==

# Comparing two strings to see if one is lexically ordered before the other:
"alpha" < "beta"     # true
"beta" < "alpha"     # false

# > is the inverse of <
```

Currently, jq does not have any "toupper" or "tolower" case conversion, but it is easy to define jq equivalents of ruby's downcase and upcase:
```jq

# Only characters A to Z are affected
def downcase:
  explode | map( if 65 <= . and . <= 90 then . + 32  else . end) | implode;

# Only characters a to z are affected
def upcase:
  explode | map( if 97 <= . and . <= 122 then . - 32  else . end) | implode;
```

With the caveat that these are what they are, case-insensitive comparisons can be achieved as illustrated by this example:

```jq
("AtoZ" | upcase) == ("atoz" | upcase)   # true
```

Numeric strings are treated as any other JSON strings.

jq has an extensive library of built-in functions for handling strings.  The most recent versions of jq (since 1.4) also have extensive support for PCRE regular expressions (regex), including named captures. Please see [http://stedolan.github.io/jq/manual/#Builtinoperatorsandfunctions|jq Builtin Operators and Functions] for details.


## Julia

Notes:
* Julia is strongly typed and not coherce Numbers to/from Chars neither Chars to/from Strings;
* In Julia /constant/ chars and strings are differently enclosed in ' for chars and " for strings;
* Julia can handle both ASCII and non-ASCII chars;
{{trans|Python}}

```julia
function compare(a, b)
    println("\n$a is of type $(typeof(a)) and $b is of type $(typeof(b))")
    if a <  b println("$a is strictly less than $b") end
    if a <= b println("$a is less than or equal to $b") end
    if a >  b println("$a is strictly greater than $b") end
    if a >= b println("$a is greater than or equal to $b") end
    if a == b println("$a is equal to $b") end
    if a != b println("$a is not equal to $b") end
    if a === b println("$a has object identity with $b") end
    if a !== b println("$a has negated object identity with $b") end
end

compare("YUP", "YUP")
compare('a', 'z')
compare("24", "123")
compare(24, 123)
compare(5.0, 5)
```


{{out}}

```txt
YUP is of type String and YUP is of type String
YUP is less than or equal to YUP
YUP is greater than or equal to YUP
YUP is equal to YUP
YUP has negated object identity with YUP

a is of type Char and z is of type Char
a is strictly less than z
a is less than or equal to z
a is not equal to z
a has negated object identity with z

24 is of type String and 123 is of type String
24 is strictly greater than 123
24 is greater than or equal to 123
24 is not equal to 123
24 has negated object identity with 123

24 is of type Int64 and 123 is of type Int64
24 is strictly less than 123
24 is less than or equal to 123
24 is not equal to 123
24 has negated object identity with 123

5.0 is of type Float64 and 5 is of type Int64
5.0 is less than or equal to 5
5.0 is greater than or equal to 5
5.0 is equal to 5
5.0 has negated object identity with 5
```



## Kotlin


```scala
// version 1.0.6

fun main(args: Array<String>) {
    val k1 = "kotlin"
    val k2 = "Kotlin"
    println("Case sensitive comparisons:\n")
    println("kotlin and Kotlin are equal     = ${k1 == k2}")
    println("kotlin and Kotlin are not equal = ${k1 != k2}")
    println("kotlin comes before Kotlin      = ${k1 < k2}")
    println("kotlin comes after Kotlin       = ${k1 > k2}")
    println("\nCase insensitive comparisons:\n")
    println("kotlin and Kotlin are equal     = ${k1 == k2.toLowerCase()}")
    println("kotlin and Kotlin are not equal = ${k1 != k2.toLowerCase()}")
    println("kotlin comes before Kotlin      = ${k1 < k2.toLowerCase()}")
    println("kotlin comes after Kotlin       = ${k1 > k2.toLowerCase()}")
}
```


{{out}}

```txt

Case sensitive comparisons:

kotlin and Kotlin are equal     = false
kotlin and Kotlin are not equal = true
kotlin comes before Kotlin      = false
kotlin comes after Kotlin       = true

Case insensitive comparisons:

kotlin and Kotlin are equal     = true
kotlin and Kotlin are not equal = false
kotlin comes before Kotlin      = false
kotlin comes after Kotlin       = false

```



## Lasso


```Lasso
// Comparing two strings for exact equality
"'this' == 'this': " + ('this' == 'this') // true
"'this' == 'This': " + ('this' == 'This') // true, as it's case insensitive

// Comparing two strings for inequality (i.e., the inverse of exact equality)
"'this' != 'this': " + ('this' != 'this')// false
"'this' != 'that': " + ('this' != 'that') // true

// Comparing two strings to see if one is lexically ordered before than the other
"'alpha' < 'beta': " + ('alpha' < 'beta') // true
"'beta' < 'alpha': " + ('beta' < 'alpha') // false

// Comparing two strings to see if one is lexically ordered after than the other
"'alpha' > 'beta': " + ('alpha' > 'beta') // false
"'beta' > 'alpha': " + ('beta' > 'alpha') // true

// How to achieve both case sensitive comparisons and case insensitive comparisons within the language
"case sensitive - 'this'->equals('This',-case=true): " + ('this'->equals('This',-case=true)) // false
"case insensitive - 'this'->equals('This',-case=true): " + ('this'->equals('This')) // true

// How the language handles comparison of numeric strings if these are not treated lexically
"'01234' == '01234': "+ ('01234' == '01234') // true
"'01234' == '0123': " + ('01234' == '0123') // false
"'01234' > '0123': " + ('01234' > '0123') // true
"'01234' < '0123': " + ('01234' < '0123') //false

// Additional string comparisons
"'The quick brown fox jumps over the rhino' >> 'fox' (contains): " +
    ('The quick brown fox jumps over the rhino' >> 'fox') // true
"'The quick brown fox jumps over the rhino' >> 'cat' (contains): " +
    ('The quick brown fox jumps over the rhino' >> 'cat') // false
"'The quick brown fox jumps over the rhino'->beginswith('rhino'): " +
    ('The quick brown fox jumps over the rhino'->beginswith('rhino')) // false
"'The quick brown fox jumps over the rhino'->endswith('rhino'): " +
    ('The quick brown fox jumps over the rhino'->endswith('rhino')) // true

```


{{out}}

```txt
'this' == 'this': true
'this' == 'This': true

'this' != 'this': false
'this' != 'that': true

'alpha' < 'beta': true
'beta' < 'alpha': false
'alpha' > 'beta': false
'beta' > 'alpha': true

case sensitive - 'this'->equals('This',-case=true): false
case insensitive - 'this'->equals('This',-case=true): true

'01234' == '01234': true
'01234' == '0123': false
'01234' > '0123': true
'01234' < '0123': false

'The quick brown fox jumps over the rhino' >> 'fox' (contains): true
'The quick brown fox jumps over the rhino' >> 'cat' (contains): false
'The quick brown fox jumps over the rhino'->beginswith('rhino'): false
'The quick brown fox jumps over the rhino'->endswith('rhino'): true
```



## Lingo

Lingo's built-in string comparison is case-insensitive:

```lingo
put "abc"="ABC"
-- 1

put "abc"<>"def"
-- 1

put "abc"<"def"
-- 1

put "abc">"def"
-- 0
```


Case-sensitive string comparison could be implemented e.g. like this:

```lingo
-- Returns -1 if str1 is less than str2
-- Returns 1 if str1 is greater than str2
-- Returns 0 if str1 and str2 are equal
on strcmp (str1, str2)
  h1 = bytearray(str1).toHexString(1, str1.length)
  h2 = bytearray(str2).toHexString(1, str2.length)
  if h1<h2 then return -1
  else if h1>h2 then return 1
  return 0
end
```



## Lua

{{trans|Python}}
* Lua coerces numbers to strings and vice-versa if possible, but it never does this during comparisons or table indexing.
* Case-insensitivity can be accomplished by using <code>string.upper</code> or <code>string.lower</code> on both strings prior to comparing them.
* Lua does not have a dedicated identity operator as == already plays that role. If two strings have equal contents, they are the same object and therefore equal.

```lua
function compare(a, b)
    print(("%s is of type %s and %s is of type %s"):format(
        a, type(a),
        b, type(b)
    ))
    if a <  b then print(('%s is strictly less than %s'):format(a, b)) end
    if a <= b then print(('%s is less than or equal to %s'):format(a, b)) end
    if a >  b then print(('%s is strictly greater than %s'):format(a, b)) end
    if a >= b then print(('%s is greater than or equal to %s'):format(a, b)) end
    if a == b then print(('%s is equal to %s'):format(a, b)) end
    if a ~= b then print(('%s is not equal to %s'):format(a, b)) end
    print ""
end

compare('YUP', 'YUP')
compare('BALL', 'BELL')
compare('24', '123')
compare(24, 123)
compare(5.0, 5)
```


{{out}}

```txt

YUP is of type string and YUP is of type string
YUP is less than or equal to YUP
YUP is greater than or equal to YUP
YUP is equal to YUP

BALL is of type string and BELL is of type string
BALL is strictly less than BELL
BALL is less than or equal to BELL
BALL is not equal to BELL

24 is of type string and 123 is of type string
24 is strictly greater than 123
24 is greater than or equal to 123
24 is not equal to 123

24 is of type number and 123 is of type number
24 is strictly less than 123
24 is less than or equal to 123
24 is not equal to 123

5 is of type number and 5 is of type number
5 is less than or equal to 5
5 is greater than or equal to 5
5 is equal to 5

```




## Mathematica


```Mathematica
compare[x_, y_] := Module[{},
  If[x == y,
   Print["Comparing for equality (case sensitive): " <> x <> " and " <> y <> " ARE equal"],
   Print["Comparing for equality (case sensitive): " <> x <> " and " <> y <> " are NOT equal" ]] ;
  If[x != y,
   Print["Comparing for inequality (case sensitive): " <> x <> " and " <> y <> " are NOT equal"],
   Print["Comparing for inequality (case sensitive): " <> x <> " and " <> y <> " ARE equal" ]] ;
  Switch[Order[x, y],
    1, Print["Comparing for order (case sensitive): " <> x <> " comes before " <> y],
   -1, Print["Comparing for order (case sensitive): " <> x <> " comes after " <> y],
    0, Print["Comparing for order (case sensitive): " <> x <> " comes in the same spot as " <> y]];
  If[ToLowerCase[x] == ToLowerCase[y],
   Print["Comparing for equality (case insensitive): " <> x <> " and " <> y <> " ARE equal"],
   Print["Comparing for equality (case insensitive): " <> x <> " and " <> y <> " are NOT equal" ]] ;
  Print[];
  ]
compare["Hello", "Hello"]
compare["3.1", "3.14159"]
compare["mathematica", "Mathematica"]
```

{{out}}

```txt
Comparing for equality (case sensitive): Hello and Hello ARE equal
Comparing for inequality (case sensitive): Hello and Hello ARE equal
Comparing for order (case sensitive): Hello comes in the same spot as Hello
Comparing for equality (case insensitive): Hello and Hello ARE equal

Comparing for equality (case sensitive): 3.1 and 3.14159 are NOT equal
Comparing for inequality (case sensitive): 3.1 and 3.14159 are NOT equal
Comparing for order (case sensitive): 3.1 comes before 3.14159
Comparing for equality (case insensitive): 3.1 and 3.14159 are NOT equal

Comparing for equality (case sensitive): mathematica and Mathematica are NOT equal
Comparing for inequality (case sensitive): mathematica and Mathematica are NOT equal
Comparing for order (case sensitive): mathematica comes before Mathematica
Comparing for equality (case insensitive): mathematica and Mathematica ARE equal
```



## MiniScript


```MiniScript
string1 = input("Please enter a string.")
string2 = input("Please enter a second string.")

//Comparing two strings for exact equality

if string1 == string2 then
    print "Strings are equal."
end if

//Comparing two strings for inequality

if string1 != string2 then
    print "Strings are NOT equal."
end if

//Comparing two strings to see if one is lexically ordered before than the other

if string1 > string2 then
    print string1 + " is lexically ordered AFTER " + string2

//Comparing two strings to see if one is lexically ordered after than the other

else if string1 < string2 then
    print string1 + " is lexically ordered BEFORE " + string2
end if

//How to achieve case sensitive comparisons

//Comparing two strings for exact equality (case sensitive)
if string1 == string2 then
    print "Strings are equal. (case sensitive)"
end if

//Comparing two strings for inequality (case sensitive)
if string1 != string2 then
    print "Strings are NOT equal. (case sensitive)"
end if

//How to achieve case insensitive comparisons within the language

//Comparing two strings for exact equality (case insensitive)
if string1.lower == string2.lower then
    print "Strings are equal. (case insensitive)"
end if

//Comparing two strings for inequality (case insensitive)
if string1.lower != string2.lower then
    print "Strings are NOT equal. (case insensitive)"
end if
```



## NetRexx

{{trans|REXX}}
The only change to the [[#REXX|REXX]] program to make this work in [[NetRexx]] was to change '''&quot;<tt>!=</tt>&quot;''' to '''&quot;<tt>\=</tt>&quot; for the '''NOT EQUAL''' comparison.  (Incidentally; the form shown here will function equally well as a [[REXX]] program: '''&quot;<tt>\=</tt>&quot;''' is valid [[REXX]] syntax for '''NOT EQUAL''' in most dialects.)

{{works with|NetRexx}}
{{works with|ooRexx}}
{{works with|Regina}}
{{works with|z/VM CMS Rexx (US codepage)}}
{{works with|z/OS TSO Rexx (US codepage)}}
{{works with|z/OS UNIX System Services}}

Will not work with TSO REXX on some codepages.

Changing <strong><tt>\=</tt></strong> to <strong><tt>&lt;&gt;</tt></strong> would make it work everywhere. Unfortunately there is no such &quot;cure&quot; for <strong><tt>\==</tt></strong>.

See also [[#ooRexx|ooRexx]] and [[#version 2 (additional aspects)|REXX version 2]] for caseless comparison and comparison of numbers.



```NetRexx
animal = 'dog'
if animal = 'cat' then
  say animal "is lexically equal to cat"
if animal \= 'cat' then
  say animal "is not lexically equal cat"
if animal > 'cat' then
  say animal "is lexically higher than cat"
if animal < 'cat' then
  say animal "is lexically lower than cat"
if animal >= 'cat' then
  say animal "is not lexically lower than cat"
if animal <= 'cat' then
  say animal "is not lexically higher than cat"
/* The above comparative operators do not consider
   leading and trailing whitespace when making comparisons. */
if '  cat  ' = 'cat' then
  say "this will print because whitespace is stripped"

/* To consider all whitespace in a comparison
   we need to use strict comparative operators */

if '  cat  ' == 'cat' then
  say "this will not print because comparison is strict"

```

The list of strict comparison operators described in the [[#REXX|REXX]] sample apply to [[NetRexx]] too.


## Nim


```nim
import strutils

var s1: string = "The quick brown"
var s2: string = "The Quick Brown"
echo("== : ", s1 == s2)
echo("!= : ", s1 != s2)
echo("< : ", s1 < s2)
echo("<= : ", s1 <= s2)
echo("> : ", s1 > s2)
echo(">= : ", s1 >= s2)
```

{{out}}

```txt
== : false
!= : true
< : false
<= : false
> : true
>= : true
```



## Oforth



```Oforth
"abcd" "abcd" ==
"abcd" "abce" <>
"abcd" "abceed" <=
"abce" "abcd" >
"abcEEE" toUpper "ABCeee" toUpper ==
```



## ooRexx

See the [[#NetRexx|NetRexx]] and/or the [[#REXX|REXX]] implementation.

There is a way to "caseless" compare array elements:
<lang>a=.array~of('A 1','B 2','a 3','b 3','A 5')
a~sortwith(.caselesscomparator~new)
Do i=1 To 5
  Say a[i]
  End
```

Output:

```txt
A 1
a 3
A 5
B 2
b 3
```



## PARI/GP

Strings are compared for equality and inequality with <code>==</code> and <code>!=</code> and are compared with <code>cmp</code> or with the usual <code>< > <= >=</code>. Case-insensitive comparison is not built in.


## Perl


Scalar variables are weakly typed in Perl, and there are two sets of comparison operators that can be used on them: One set for (coercive) numeric comparison, and one set for (coercive) lexical string comparison. The second set is demonstrated in the following:


```perl
use v5.16;  # ...for fc(), which does proper Unicode casefolding.
            # With older Perl versions you can use lc() as a poor-man's substitute.

sub compare {
    my ($a, $b) = @_;
    my $A = "'$a'";
    my $B = "'$b'";

    print "$A and $B are lexically equal.\n"     if $a eq $b;
    print "$A and $B are not lexically equal.\n" if $a ne $b;

    print "$A is lexically before $B.\n"         if $a lt $b;
    print "$A is lexically after $B.\n"          if $a gt $b;

    print "$A is not lexically before $B.\n"     if $a ge $b;
    print "$A is not lexically after $B.\n"      if $a le $b;

    print "The lexical relationship is: ", $a cmp $b, "\n";
    print "The case-insensitive lexical relationship is: ", fc($a) cmp fc($b), "\n";
    print "\n";
}

compare('Hello', 'Hello');
compare('5', '5.0');
compare('perl', 'Perl');
```


{{out}}

```txt

'Hello' and 'Hello' are lexically equal.
'Hello' is not lexically before 'Hello'.
'Hello' is not lexically after 'Hello'.
The lexical relationship is: 0
The case-insensitive lexical relationship is: 0

'5' and '5.0' are not lexically equal.
'5' is lexically before '5.0'.
'5' is not lexically after '5.0'.
The lexical relationship is: -1
The case-insensitive lexical relationship is: -1

'perl' and 'Perl' are not lexically equal.
'perl' is lexically after 'Perl'.
'perl' is not lexically before 'Perl'.
The lexical relationship is: 1
The case-insensitive lexical relationship is: 0

```



## Perl 6

Perl 6 uses strong typing dynamically (and gradual typing statically), but normal string and numeric comparisons are coercive.  (You may use generic comparison operators if you want polymorphic comparison—but usually you don't. :)

String comparisons never do case folding because that's a very complicated subject in the modern world of Unicode.  (You can explicitly apply an appropriate case-folding function to the arguments before doing the comparison, or for "equality" testing you can do matching with a case-insensitive regex, assuming Unicode's language-neutral case-folding rules are okay.)

```perl6
sub compare($a,$b) {
    my $A = "{$a.WHAT.^name} '$a'";
    my $B = "{$b.WHAT.^name} '$b'";

    if $a eq $b { say "$A and $B are lexically equal" }
    if $a ne $b { say "$A and $B are not lexically equal" }

    if $a gt $b { say "$A is lexically after $B" }
    if $a lt $b { say "$A is lexically before than $B" }

    if $a ge $b { say "$A is not lexically before $B" }
    if $a le $b { say "$A is not lexically after $B" }

    if $a === $b { say "$A and $B are identical objects" }
    if $a !=== $b { say "$A and $B are not identical objects" }

    if $a eqv $b { say "$A and $B are generically equal" }
    if $a !eqv $b { say "$A and $B are not generically equal" }

    if $a before $b { say "$A is generically after $B" }
    if $a after $b { say "$A is generically before $B" }

    if $a !after $b { say "$A is not generically before $B" }
    if $a !before $b { say "$A is not generically after $B" }

    say "The lexical relationship of $A and $B is { $a leg $b }" if $a ~~ Stringy;
    say "The generic relationship of $A and $B is { $a cmp $b }";
    say "The numeric relationship of $A and $B is { $a <=> $b }" if $a ~~ Numeric;
    say '';
}

compare 'YUP', 'YUP';
compare 'BALL', 'BELL';
compare 24, 123;
compare 5.1, 5;
compare 5.1e0, 5 + 1/10;
```

{{out}}

```txt
Str 'YUP' and Str 'YUP' are lexically equal
Str 'YUP' is not lexically before Str 'YUP'
Str 'YUP' is not lexically after Str 'YUP'
Str 'YUP' and Str 'YUP' are identical objects
Str 'YUP' and Str 'YUP' are generically equal
Str 'YUP' is not generically before Str 'YUP'
Str 'YUP' is not generically after Str 'YUP'
The lexical relationship of Str 'YUP' and Str 'YUP' is Same
The generic relationship of Str 'YUP' and Str 'YUP' is Same

Str 'BALL' and Str 'BELL' are not lexically equal
Str 'BALL' is lexically before than Str 'BELL'
Str 'BALL' is not lexically after Str 'BELL'
Str 'BALL' and Str 'BELL' are not identical objects
Str 'BALL' and Str 'BELL' are not generically equal
Str 'BALL' is generically after Str 'BELL'
Str 'BALL' is not generically before Str 'BELL'
The lexical relationship of Str 'BALL' and Str 'BELL' is Increase
The generic relationship of Str 'BALL' and Str 'BELL' is Increase

Int '24' and Int '123' are not lexically equal
Int '24' is lexically after Int '123'
Int '24' is not lexically before Int '123'
Int '24' and Int '123' are not identical objects
Int '24' and Int '123' are not generically equal
Int '24' is generically after Int '123'
Int '24' is not generically before Int '123'
The generic relationship of Int '24' and Int '123' is Increase
The numeric relationship of Int '24' and Int '123' is Increase

Rat '5.1' and Int '5' are not lexically equal
Rat '5.1' is lexically after Int '5'
Rat '5.1' is not lexically before Int '5'
Rat '5.1' and Int '5' are not identical objects
Rat '5.1' and Int '5' are not generically equal
Rat '5.1' is generically before Int '5'
Rat '5.1' is not generically after Int '5'
The generic relationship of Rat '5.1' and Int '5' is Decrease
The numeric relationship of Rat '5.1' and Int '5' is Decrease

Num '5.1' and Rat '5.1' are lexically equal
Num '5.1' is not lexically before Rat '5.1'
Num '5.1' is not lexically after Rat '5.1'
Num '5.1' and Rat '5.1' are not identical objects
Num '5.1' and Rat '5.1' are not generically equal
Num '5.1' is not generically before Rat '5.1'
Num '5.1' is not generically after Rat '5.1'
The generic relationship of Num '5.1' and Rat '5.1' is Same
The numeric relationship of Num '5.1' and Rat '5.1' is Same
```



## Phix


```Phix
if name=="pete" then ?"The strings are equal" end if
if name!="pete" then ?"The strings are not equal" end if
if name<"pete" then ?"name is lexically first" end if
if name>"pete" then ?"name is lexically last" end if
if upper(name)=upper("pete") then ?"case insensitive match" end if
if match("pete",lower(name)) then ?"petes in there somewhere" end if
```



## PicoLisp


```PicoLisp
(setq
   str= =
   str< <
   str> > )

(println
   (str= (lowc "Foo") (lowc "foo") (lowc "fOO"))
   (str= "f" "foo")
   (str= "foo" "foo" "foo")
   (str= "" "") )

(println
   (str< "abc" "def")
   (str> "abc" "def")
   (str< "" "")
   (str< "12" "45") )

(bye)
```



## PowerShell


```PowerShell

"a" -lt "b"  # lower than
"a" -eq "b"  # equal
"a" -gt "b"  # greater than
"a" -le "b"  # lower than or equal
"a" -ne "b"  # not equal
"a" -ge "b"  # greater than or equal

```

<b>Output:</b>

```txt

True
False
False
True
True
False

```

By default operators are case insensitive.
Preceed them by the letter "c" to make them case sensitive like this:

```PowerShell

"a" -eq "A"
"a" -ceq "A"

```


```txt

True
False

```



## PureBasic


```purebasic
Macro StrTest(Check,tof)
  Print("Test "+Check+#TAB$)
  If tof=1 : PrintN("true") : Else : PrintN("false") : EndIf
EndMacro

Procedure.b StrBool_eq(a$,b$)   :   ProcedureReturn Bool(a$=b$)             :   EndProcedure
Procedure.b StrBool_n_eq(a$,b$) :   ProcedureReturn Bool(a$<>b$)            :   EndProcedure
Procedure.b StrBool_a(a$,b$)    :   ProcedureReturn Bool(a$>b$)             :   EndProcedure
Procedure.b StrBool_b(a$,b$)    :   ProcedureReturn Bool(a$<b$)             :   EndProcedure
Procedure.b NumBool_eq(a$,b$)   :   ProcedureReturn Bool(Val(a$)=Val(b$))   :   EndProcedure
Procedure.b NumBool_n_eq(a$,b$) :   ProcedureReturn Bool(Val(a$)<>Val(b$))  :   EndProcedure
Procedure.b NumBool_a(a$,b$)    :   ProcedureReturn Bool(Val(a$)>Val(b$))   :   EndProcedure
Procedure.b NumBool_b(a$,b$)    :   ProcedureReturn Bool(Val(a$)<Val(b$))   :   EndProcedure

Procedure Compare(a$,b$,cs.b=1,num.b=0)
  If Not cs : a$=UCase(a$) : b$=UCase(b$) : EndIf
  PrintN("a = "+a$) : PrintN("b = "+b$)
  If Not num  :   StrTest(" a=b ",StrBool_eq(a$,b$))     :   Else  :   StrTest(" a=b ",NumBool_eq(a$,b$))     :   EndIf
  If Not num  :   StrTest(" a<>b ",StrBool_n_eq(a$,b$))  :   Else  :   StrTest(" a<>b ",NumBool_n_eq(a$,b$))  :   EndIf
  If Not num  :   StrTest(" a>b ",StrBool_a(a$,b$))      :   Else  :   StrTest(" a>b ",NumBool_a(a$,b$))      :   EndIf
  If Not num  :   StrTest(" a<b ",StrBool_b(a$,b$))      :   Else  :   StrTest(" a<b ",NumBool_b(a$,b$))      :   EndIf
EndProcedure

If OpenConsole()
  PrintN("String comparison - ")
  a$="Abcd" : b$="abcd"
  PrintN(#CRLF$+"- case sensitive:")
  Compare(a$,b$)
  PrintN(#CRLF$+"- case insensitive:")
  Compare(a$,b$,0)
  a$="1241" : b$="222"
  PrintN(#CRLF$+"- num-string; lexically compared:")
  Compare(a$,b$)
  PrintN(#CRLF$+"- num-string; numerically compared:")
  Compare(a$,b$,1,1)
  Input()
EndIf
```

{{out}}

```txt

String comparison -

- case sensitive:
a = Abcd
b = abcd
Test  a=b       false
Test  a<>b      true
Test  a>b       false
Test  a<b       true

- case insensitive:
a = ABCD
b = ABCD
Test  a=b       true
Test  a<>b      false
Test  a>b       false
Test  a<b       false

- num-string; lexically compared:
a = 1241
b = 222
Test  a=b       false
Test  a<>b      true
Test  a>b       false
Test  a<b       true

- num-string; numerically compared:
a = 1241
b = 222
Test  a=b       false
Test  a<>b      true
Test  a>b       true
Test  a<b       false

```



## Python

Notes:
* Python is strongly typed. The string '24' is never coerced to a number, (or vice versa).
* Python does not have case-insensitive string comparison operators, instead use <code>name.upper()</code> or <code>name.lower()</code> to coerce strings to the same case and compare the results.

```python
def compare(a, b):
    print("\n%r is of type %r and %r is of type %r"
          % (a, type(a), b, type(b)))
    if a <  b:      print('%r is strictly less than  %r' % (a, b))
    if a <= b:      print('%r is less than or equal to %r' % (a, b))
    if a >  b:      print('%r is strictly greater than  %r' % (a, b))
    if a >= b:      print('%r is greater than or equal to %r' % (a, b))
    if a == b:      print('%r is equal to %r' % (a, b))
    if a != b:      print('%r is not equal to %r' % (a, b))
    if a is b:      print('%r has object identity with %r' % (a, b))
    if a is not b:  print('%r has negated object identity with %r' % (a, b))

compare('YUP', 'YUP')
compare('BALL', 'BELL')
compare('24', '123')
compare(24, 123)
compare(5.0, 5)
```


{{out}}

```txt
'YUP' is of type <class 'str'> and 'YUP' is of type <class 'str'>
'YUP' is less than or equal to 'YUP'
'YUP' is greater than or equal to 'YUP'
'YUP' is equal to 'YUP'
'YUP' has object identity with 'YUP'

'BALL' is of type <class 'str'> and 'BELL' is of type <class 'str'>
'BALL' is strictly less than  'BELL'
'BALL' is less than or equal to 'BELL'
'BALL' is not equal to 'BELL'
'BALL' has negated object identity with 'BELL'

'24' is of type <class 'str'> and '123' is of type <class 'str'>
'24' is strictly greater than  '123'
'24' is greater than or equal to '123'
'24' is not equal to '123'
'24' has negated object identity with '123'

24 is of type <class 'int'> and 123 is of type <class 'int'>
24 is strictly less than  123
24 is less than or equal to 123
24 is not equal to 123
24 has negated object identity with 123

5.0 is of type <class 'float'> and 5 is of type <class 'int'>
5.0 is less than or equal to 5
5.0 is greater than or equal to 5
5.0 is equal to 5
5.0 has negated object identity with 5
```



## R

{{trans|Python}}

```rsplus
compare <- function(a, b)
{
  cat(paste(a, "is of type", class(a), "and", b, "is of type", class(b), "\n"))

  if (a < b) cat(paste(a, "is strictly less than", b, "\n"))
  if (a <= b) cat(paste(a, "is less than or equal to", b, "\n"))
  if (a > b) cat(paste(a, "is strictly greater than", b, "\n"))
  if (a >= b) cat(paste(a, "is greater than or equal to", b, "\n"))
  if (a == b) cat(paste(a, "is equal to", b, "\n"))
  if (a != b) cat(paste(a, "is not equal to", b, "\n"))

  invisible()
}

compare('YUP', 'YUP')
compare('BALL', 'BELL')
compare('24', '123')
compare(24, 123)
compare(5.0, 5)
```


{{out}}

```txt
1> compare('YUP', 'YUP')
YUP is of type character and YUP is of type character
YUP is less than or equal to YUP
YUP is greater than or equal to YUP
YUP is equal to YUP
1> compare('BALL', 'BELL')
BALL is of type character and BELL is of type character
BALL is strictly less than BELL
BALL is less than or equal to BELL
BALL is not equal to BELL
1> compare('24', '123')
24 is of type character and 123 is of type character
24 is strictly greater than 123
24 is greater than or equal to 123
24 is not equal to 123
1> compare(24, 123)
24 is of type numeric and 123 is of type numeric
24 is strictly less than 123
24 is less than or equal to 123
24 is not equal to 123
1> compare(5.0, 5)
5 is of type numeric and 5 is of type numeric
5 is less than or equal to 5
5 is greater than or equal to 5
5 is equal to 5
```


And a more ridiculous version:

```rsplus
compare <- function(a, b)
{
  cat(paste(a, "is of type", class(a), "and", b, "is of type", class(b), "\n"))

  printer <- function(a, b, msg) cat(paste(a, msg, b, "\n"))

  op <- c(`<`, `<=`, `>`, `>=`, `==`, `!=`)
  msgs <- c(
    "is strictly less than",
    "is less than or equal to",
    "is strictly greater than",
    "is greater than or equal to",
    "is equal to",
    "is not equal to"
  )

  sapply(1:length(msgs), function(i) if(op[[i]](a, b)) printer(a, b, msgs[i]))

  invisible()
}
```



## Racket



```racket

#lang racket

;; Comparing two strings for exact equality
(string=? "foo" "foo")

;; Comparing two strings for inequality
(not (string=? "foo" "bar"))

;; Comparing two strings to see if one is lexically ordered before than the other
(string<? "abc" "def")

;; Comparing two strings to see if one is lexically ordered after than the other
(string>? "def" "abc")

;; How to achieve both case sensitive comparisons and case insensitive comparisons within the language
(string-ci=? "foo" "FOO")

```



## REXX


### version 1

Note that   '''Dog'''   may or may not be higher than   '''cat''',   depending upon the underlying hardware

(the order of lowercase and uppercase isn't defined by the REXX language, but rather on ''how'' the

characters are represented).


In   '''ASCII''',   uppercase letters are   ''lower'' than lowercase, and

in '''EBCDIC''', uppercase letters are ''higher'' than lowercase.


Here is a list of some of the strict comparative operators and their meaning:
:::*   '''=='''     Strictly Equal To
:::*   '''<<'''     Strictly Less Than
:::*   '''>>'''     Strictly Greater Than
:::*   '''<<='''         Strictly Less Than or Equal To
:::*   '''>>='''         Strictly Greater Than or Equal To
:::*   '''\<<'''         Strictly Not Less Than
:::*   '''\>>'''         Strictly Not Greater Than
Note that some REXXes can use (support) characters other than a backslash   ['''\''']   for a logical not   ['''¬'''].

Still other REXX support the use of a tilde   ['''~''']   for a logical not.

```rexx
/*REXX program shows  different ways to  compare  two character strings.*/
say 'This is an '      word('ASCII EBCDIC', 1+(1=='f1'))        ' system.'
say
      cat = 'cat'
   animal = 'dog'
if animal =  cat  then say $(animal) "is lexically equal to"        $(cat)
if animal \= cat  then say $(animal) "is not lexically equal to"    $(cat)
if animal >  cat  then say $(animal) "is lexically higher than"     $(cat)
if animal <  cat  then say $(animal) "is lexically lower than"      $(cat)
if animal >  cat  then say $(animal) "is not lexically lower than"  $(cat)
if animal <  cat  then say $(animal) "is not lexically higher than" $(cat)

                      /*──── [↑]  The above comparative operators don't */
                      /*────consider any leading and/or trailing white- */
                      /*────space when making comparisons, but the case */
                      /*────is honored  (uppercase, lowercase).         */

fatcat='  cat  '      /*pad the cat with leading and trailing blanks.   */
if fatcat =  cat  then say $(fatcat) " is equal to"                 $(cat)

                      /*────To consider any whitespace in a comparison, */
                      /*────we need to use strict comparative operators.*/

if fatcat == cat  then say $(fatcat) "is strictly equal to"         $(cat)

                      /*────To perform caseless comparisons, the easiest*/
                      /*────method would be to uppercase a copy of both */
                      /*────operands.  Uppercasing is only done for the */
                      /*────Latin (or Roman) alphabet in REXX.    [↓]   */
kat='cAt'
if caselessComp(cat,kat)  then  say $(cat) 'and' $(kat) "are equal caseless"
exit                                   /*stick a fork in it, we're done.*/
/*──────────────────────────────────$ subroutine────────────────────────*/
$:  return '──►'arg(1)'◄──'            /*bracket the string with ──►α◄──*/
/*──────────────────────────────────CASELESSCOMP subroutine─────────────*/
caselessComp:  procedure;  arg a,b     /*ARG  uppercases the A & B args.*/
               return a==b             /*if exactly equal, return  1.   */

```

Programming note:

If the   ''caselessComp''   subroutine (above) didn't care about superfluous leading and/or trailing blanks,

the following REXX statement could've be used:
:::::* return a=b
where REXX will then ignore any leading and/or trailing blanks in comparing the   '''a'''   and   '''b'''   strings.

This is equivalent to:
:::::* return strip(a)==strip(b)

'''output'''   (when executed on an ASCII system):

```txt

This is an  ASCII  system.

──►dog◄── is not lexically equal to ──►cat◄──
──►dog◄── is lexically higher than ──►cat◄──
──►dog◄── is not lexically lower than ──►cat◄──
──►  cat  ◄──  is equal to ──►cat◄──
──►cat◄── and ──►cAt◄── are equal caseless

```


===version 2 (additional aspects)===
(a) if both operands are NUMBERS (normal, non-strict) comparisons will always be done arithmetically.

(b) to implement caseless comparison one can proceed as follows:

```rexx
/* REXX ***************************************************************
* 16.05.2013 Walter Pachl
**********************************************************************/
Call test 'A','<','a'
Call test 'A','=',' a'
Call test 'A','==',' a'
Call test 'Walter','<',' Wolter'
Exit

test: Procedure
Parse Arg o1,op,o2
Say q(o1) op q(o2) '->' clcompare(o1,op,o2)
Return

clcompare: Procedure
/* caseless comparison of the operands */
Parse Arg opd1,op,opd2
opd1u=translate(opd1)
opd2u=translate(opd2)
Interpret 'res=opd1u' op 'opd2u'
Return res

q: Return '"'arg(1)'"'
```

Output:

```txt
"A" < "a" -> 0
"A" = " a" -> 1
"A" == " a" -> 0
"Walter" < " Wolter" -> 1
```



## Ring


```ring


   if s1 = s2
      See "The strings are equal"
   ok
   if not (s1 = s2)
      See "The strings are not equal"
   ok
   if strcmp(s1,s2) > 0
      see "s2 is lexically ordered before than s1"
   ok
   if strcmp(s1,s2) < 0
      see "s2 is lexically ordered after than s1"
   ok

To achieve case insensitive comparisons, we should use Upper() or Lower() functions:

   if Upper(s1) = Upper(s2)
      see "The strings are equal"
   ok


```



## Robotic

We can use "if" statements to compare strings, albeit in a simple way.

Things to note:
Numeric strings are lexicographically compared the same way as a regular string.
There is no opposite of the case-sensitive comparison.


```robotic

set "$str1" to "annoy"
set "$str2" to "annoy"
: "loop"
if "$str1" === "$str2" then "case_equal"
if "$str1" = "$str2" then "equal"
if "$str1" > "$str2" then "greater_than"
if "$str1" < "$str2" then "less_than"
end

: "case_equal"
* "&$str1& is case equal to &$str2&"
set "$str2" to "ANNOY"
goto "loop"

: "equal"
* "&$str1& is equal to &$str2&"
set "$str2" to "allow"
wait for 100
goto "loop"

: "greater_than"
* "&$str1& is lexicographically greater than &$str2&"
set "$str1" to "aardvark"
wait for 100
goto "loop"

: "less_than"
* "&$str1& is lexicographically less than &$str2&"
end

```



## Ruby


```ruby
method_names = [:==,:!=, :>, :>=, :<, :<=, :<=>, :casecmp]
[["YUP", "YUP"], ["YUP", "Yup"], ["bot","bat"], ["aaa", "zz"]].each do |str1, str2|
  method_names.each{|m| puts "%s %s %s\t%s" % [str1, m, str2, str1.send(m, str2)]}
  puts
end
```

{{output}}

```txt

YUP == YUP	true
YUP != YUP	false
YUP > YUP	false
YUP >= YUP	true
YUP < YUP	false
YUP <= YUP	true
YUP <=> YUP	0
YUP casecmp YUP	0

YUP == Yup	false
YUP != Yup	true
YUP > Yup	false
YUP >= Yup	false
YUP < Yup	true
YUP <= Yup	true
YUP <=> Yup	-1
YUP casecmp Yup	0

bot == bat	false
bot != bat	true
bot > bat	true
bot >= bat	true
bot < bat	false
bot <= bat	false
bot <=> bat	1
bot casecmp bat	1

aaa == zz	false
aaa != zz	true
aaa > zz	false
aaa >= zz	false
aaa < zz	true
aaa <= zz	true
aaa <=> zz	-1
aaa casecmp zz	-1


```



## Run BASIC


```runbasic
a$	= "dog"
b$	= "cat"
if a$ =  b$ then print "the strings are equal" 			' test for equalitY
if a$ <> b$ then print "the strings are not equal" 		' test for inequalitY
if a$ >  b$ then print a$;" is lexicallY higher than ";b$ 	' test for lexicallY higher
if a$ <  b$ then print a$;" is lexicallY lower than ";b$ 	' test for lexicallY lower
if a$ <= b$ then print a$;" is not lexicallY higher than ";b$
if a$ >= b$ then print a$;" is not lexicallY lower than ";b$
end
```



## Rust

Comparisons are case sensitive by default, all (Ascii) uppercase letters are treated as lexically before all lowercase letters.
For case-insensitive comparisons, use Ascii Extensions. In general, case is not a concept that applies to all unicode symbols.

```rust
use std::ascii::AsciiExt; // for case insensitives only

fn main() {
    // only same types can be compared
    // String and String or &str and &str
    // exception: strict equality and inequality also work on &str and String
    let a: &str = "abc";
    let b: String = "Bac".to_owned();

    // Strings are coerced to &str when borrowed and needed
    if a == b { println!("The strings are equal") }
    if a != b { println!("The strings are not equal") }
    if a  > &b { println!("The first string is lexically after the second") }
    if a  < &b { println!("The first string is lexically before the second") }
    if a >= &b { println!("The first string is not lexically before the second") }
    if a <= &b { println!("The first string is not lexically after the second") }

    // case-insensitives:

    // equality
    // this avoids new allocations
    if a.eq_ignore_ascii_case(&b) { println!("Both strings are equal when ignoring case") }

    // everything else, create owned Strings, then compare as above
    let a2 = a.to_ascii_uppercase();
    let b2 = b.to_ascii_uppercase();

    // repeat checks
}
```


{{out}}

```txt
The strings are not equal
The first string is lexically after the second
The first string is not lexically before the second
```



## Scala

{{libheader|Scala}}
```Scala
object Compare extends App {
  def compare(a: String, b: String) {
    if (a == b) println(s"'$a' and '$b' are lexically equal.")
    else println(s"'$a' and '$b' are not lexically equal.")

    if (a.equalsIgnoreCase(b)) println(s"'$a' and '$b' are case-insensitive lexically equal.")
    else println(s"'$a' and '$b' are not case-insensitive lexically equal.")

    if (a.compareTo(b) < 0) println(s"'$a' is lexically before '$b'.")
    else if (a.compareTo(b) > 0) println(s"'$a' is lexically after '$b'.")

    if (a.compareTo(b) >= 0) println(s"'$a' is not lexically before '$b'.")
    if (a.compareTo(b) <= 0) println(s"'$a' is not lexically after '$b'.")

    println(s"The lexical relationship is: ${a.compareTo(b)}")
    println(s"The case-insensitive lexical relationship is: ${a.compareToIgnoreCase(b)}\n")
  }

  compare("Hello", "Hello")
  compare("5", "5.0")
  compare("java", "Java")
  compare("ĴÃVÁ", "ĴÃVÁ")
  compare("ĴÃVÁ", "ĵãvá")
}
```

{{out}}

```txt
'Hello' and 'Hello' are lexically equal.
'Hello' and 'Hello' are case-insensitive lexically equal.
'Hello' is not lexically before 'Hello'.
'Hello' is not lexically after 'Hello'.
The lexical relationship is: 0
The case-insensitive lexical relationship is: 0

'5' and '5.0' are not lexically equal.
'5' and '5.0' are not case-insensitive lexically equal.
'5' is lexically before '5.0'.
'5' is not lexically after '5.0'.
The lexical relationship is: -2
The case-insensitive lexical relationship is: -2

'java' and 'Java' are not lexically equal.
'java' and 'Java' are case-insensitive lexically equal.
'java' is lexically after 'Java'.
'java' is not lexically before 'Java'.
The lexical relationship is: 32
The case-insensitive lexical relationship is: 0

'ĴÃVÁ' and 'ĴÃVÁ' are lexically equal.
'ĴÃVÁ' and 'ĴÃVÁ' are case-insensitive lexically equal.
'ĴÃVÁ' is not lexically before 'ĴÃVÁ'.
'ĴÃVÁ' is not lexically after 'ĴÃVÁ'.
The lexical relationship is: 0
The case-insensitive lexical relationship is: 0

'ĴÃVÁ' and 'ĵãvá' are not lexically equal.
'ĴÃVÁ' and 'ĵãvá' are case-insensitive lexically equal.
'ĴÃVÁ' is lexically before 'ĵãvá'.
'ĴÃVÁ' is not lexically after 'ĵãvá'.
The lexical relationship is: -1
The case-insensitive lexical relationship is: 0
```



## Scheme


```scheme

;; Comparing two strings for exact equality
(string=? "hello" "hello")

;; Comparing two strings for inequality
(not (string=? "hello" "Hello"))

;; Checking if the first string is lexically ordered before the second
(string<? "bar" "foo")

;; Checking if the first string is lexically ordered after the second
(string>? "foo" "bar")

;; case insensitive comparison
(string-ci=? "hello" "Hello")
```



## Seed7

Seed7 uses the string comparison operators [http://seed7.sourceforge.net/libraries/string.htm#%28in_string%29=%28in_string%29 =],
[http://seed7.sourceforge.net/libraries/string.htm#%28in_string%29%3C%3E%28in_string%29 <>],
[http://seed7.sourceforge.net/libraries/string.htm#%28in_string%29%3C%28in_string%29 <],
[http://seed7.sourceforge.net/libraries/string.htm#%28in_string%29%3E%28in_string%29 >],
[http://seed7.sourceforge.net/libraries/string.htm#%28in_string%29%3C=%28in_string%29 <=] and
[http://seed7.sourceforge.net/libraries/string.htm#%28in_string%29%3E=%28in_string%29 >=].
The function [http://seed7.sourceforge.net/libraries/string.htm#compare%28in_string,in_string%29 compare]
returns -1, 0 or 1 if the first argument is considered to be respectively less than, equal to, or greater than the second.
All string comparisons work case sensitive. The functions [http://seed7.sourceforge.net/libraries/string.htm#upper%28in_string%29 upper] and
[http://seed7.sourceforge.net/libraries/string.htm#lower%28in_string%29 lower] can be used to do an insensitive comparison.


```seed7
$ include "seed7_05.s7i";

const proc: showComparisons (in string: a, in string: b) is func
  begin
    writeln("compare " <& literal(a) <& " with " <& literal(b) <&":");
    writeln("a = b  returns: " <& a = b);
    writeln("a <> b returns: " <& a <> b);
    writeln("a < b  returns: " <& a < b);
    writeln("a > b  returns: " <& a > b);
    writeln("a <= b returns: " <& a <= b);
    writeln("a >= b returns: " <& a >= b);
    writeln("compare(a, b)               returns: " <& compare(a, b));
    writeln("compare(lower(a), lower(b)) returns: " <& compare(a, b));
  end func;

const proc: main is func
  begin
    showComparisons("this", "that");
    showComparisons("that", "this");
    showComparisons("THAT", "That");
    showComparisons("this", "This");
    showComparisons("this", "this");
    showComparisons("the", "there");
    showComparisons("there", "the");
  end func;
```


The function below compares strings, which may contain digit sequences. The digit sequences are compared numerically.


```seed7
include "scanstri.s7i";

const func integer: cmpNumeric (in var string: stri1, in var string: stri2) is func
  result
    var integer: signumValue is 0;
  local
    var string: part1 is "";
    var string: part2 is "";
  begin
    while signumValue = 0 and (stri1 <> "" or stri2 <> "") do
      part1 := getDigits(stri1);
      part2 := getDigits(stri2);
      if part1 <> "" and part2 <> "" then
        signumValue := compare(part1 lpad0 length(part2), part2 lpad0 length(part1));
        if signumValue = 0 then
          signumValue := compare(length(part1), length(part2));
        end if;
      elsif part1 <> "" then
        signumValue := compare(part1, stri2);
      elsif part2 <> "" then
        signumValue := compare(stri1, part2);
      end if;
      if signumValue = 0 then
        part1 := getNonDigits(stri1);
        part2 := getNonDigits(stri2);
        if part1 <> "" and part2 <> "" then
          signumValue := compare(part1, part2);
        elsif part1 <> "" then
          signumValue := compare(part1, stri2);
        elsif part2 <> "" then
          signumValue := compare(stri1, part2);
        end if;
      end if;
    end while;
  end func;
```


Original source: [http://seed7.sourceforge.net/algorith/string.htm#cmpNumeric]


## Sidef

{{trans|Ruby}}

```ruby
var methods = %w(== != > >= < <= <=>)
for s1, s2 in [<YUP YUP>,<YUP Yup>,<bot bat>,<aaa zz>] {
    methods.each{|m| "%s %s %s\t%s\n".printf(s1, m, s2, s1.(m)(s2))}
    print "\n"
}
```


=={{Header|SNOBOL4}}==

```SNOBOL4
      s1 = 'mnopqrs'
      s2 = 'mnopqrs'
      s3 = 'mnopqr'
      s4 = 'nop'
      s5 = 'nOp'

      OUTPUT = 'Case sensitive comparisons:'
      OUTPUT = LEQ(s1, s2) s1 ' and ' s2 ' are equal (LEQ).'
      OUTPUT = IDENT(s1, s2) s1 ' and ' s2 ' are equal (IDENT).'

      OUTPUT =
      OUTPUT = LNE(s1, s3) s1 ' and ' s3 ' are not equal (LNE).'
      OUTPUT = ~LEQ(s1, s3) s1 ' and ' s3 ' are not equal (~LEQ).'
      OUTPUT = DIFFER(s1, s3) s1 ' and ' s3 ' are not equal (DIFFER).'

      OUTPUT =
      OUTPUT = LGE(s1, s3) s1 ' is greater than or equal to ' s3 ' (LGE).'
      OUTPUT = LLE(s3, s1) s3 ' is less than or equal to ' s1 ' (LLE).'

      OUTPUT =
      OUTPUT = LGT(s4, s1) s4 ' is greater than ' s1 ' (LGT).'
      OUTPUT = LLT(s1, s4) s1 ' is less than ' s4 ' (LLT).'

      OUTPUT =
      OUTPUT = "Case insensitive comparison:"
      OUTPUT = LEQ(s4, REPLACE(s5, &UCASE, &LCASE)) s4 ' and ' s5 ' are equal.'

      OUTPUT =
      OUTPUT = 'String and numeric conversions and comparisons:'
      OUTPUT = EQ('1234', 1234) '"1234" and 1234 are equal (coerce to integer).'
      OUTPUT = LEQ('1234', 1234) '"1234" and 1234 are equal (coerce to string).'
      OUTPUT =
      OUTPUT = GT('1234', 1233) '"1234" is greater than 1233 (numeric comparison).'
      OUTPUT = LT('1233', 1234) '"1233" is less than 1234 (numeric comparison).'
END
```

{{out}}

```txt

Case sensitive comparisons:
mnopqrs and mnopqrs are equal (LEQ).
mnopqrs and mnopqrs are equal (IDENT).

mnopqrs and mnopqr are not equal (LNE).
mnopqrs and mnopqr are not equal (~LEQ).
mnopqrs and mnopqr are not equal (DIFFER).

mnopqrs is greater than or equal to mnopqr (LGE).
mnopqr is less than or equal to mnopqrs (LLE).

nop is greater than mnopqrs (LGT).
mnopqrs is less than nop (LLT).

Case insensitive comparison:
nop and nOp are equal.

String and numeric conversions and comparisons:
"1234" and 1234 are equal (coerce to integer).
"1234" and 1234 are equal (coerce to string).

"1234" is greater than 1233 (numeric comparison).
"1233" is less than 1234 (numeric comparison).
```


=={{Header|Swift}}==

```swift
func compare (a: String, b: String) {
  if a == b {
    println("'\(a)' and '\(b)' are lexically equal.")
  }
  if a != b {
    println("'\(a)' and '\(b)' are not lexically equal.")
  }

  if a < b {
    println("'\(a)' is lexically before '\(b)'.")
  }
  if a > b {
    println("'\(a)' is lexically after '\(b)'.")
  }

  if a >= b {
    println("'\(a)' is not lexically before '\(b)'.")
  }
  if a <= b {
    println("'\(a)' is not lexically after '\(b)'.")
  }
}
compare("cat", "dog")
```

{{Out}}

```txt

'cat' and 'dog' are not lexically equal.
'cat' is lexically before 'dog'.
'cat' is not lexically after 'dog'.

```



## Tcl

The best way to compare two strings in Tcl for equality is with the <code>eq</code> and <code>ne</code> expression operators:

```tcl
if {$a eq $b} {
    puts "the strings are equal"
}
if {$a ne $b} {
    puts "the strings are not equal"
}
```

The numeric <code>==</code> and <code>!=</code> operators also mostly work, but can give somewhat unexpected results when the both the values ''look'' numeric. The <code>string equal</code> command is equally suited to equality-testing (and generates the same bytecode).

For ordering, the <code>&lt;</code> and <code>&gt;</code> operators may be used, but again they are principally numeric operators. For guaranteed string ordering, the result of the <code>string compare</code> command should be used instead (which uses the unicode codepoints of the string):

```tcl
if {[string compare $a $b] < 0} {
    puts "first string lower than second"
}
if {[string compare $a $b] > 0} {
    puts "first string higher than second"
}
```

Greater-or-equal and less-or-equal operations can be done by changing what exact comparison is used on the result of the <code>string compare</code>.

Tcl also can do a prefix-equal (approximately the same as <code>strncmp()</code> in [[C]]) through the use of the <tt>-length</tt> option:

```tcl
if {[string equal -length 3 $x "abc123"]} {
    puts "first three characters are equal"
}
```

And case-insensitive equality is (orthogonally) enabled through the <tt>-nocase</tt> option. These options are supported by both <code>string equal</code> and <code>string compare</code>, but not by the expression operators.


## UNIX Shell

Traditional bourne shell (which used the 'test' command for comparisons) had no way of doing lexical comparisons.


```sh
#!/bin/sh

A=Bell
B=Ball

# Traditional test command implementations test for equality and inequality
# but do not have a lexical comparison facility
if [ $A = $B ] ; then
  echo 'The strings are equal'
fi
if [ $A != $B ] ; then
  echo 'The strings are not equal'
fi

# All variables in the shell are strings, so numeric content cause no lexical problems
# 0 , -0 , 0.0 and 00 are all lexically different if tested using the above methods.

# However this may not be the case if other tools, such as awk are the slave instead of test.
```


Bash and other POSIX shells do support lexical comparisons:


```bash

#!/bin/bash

isint() {
  printf "%d" $1 >/dev/null 2>&1
}

compare() {
  local a=$1
  local b=$2

  [[ $a = $b ]] && echo "'$a' and '$b' are lexically equal"
  [[ $a != $b ]] && echo "'$a' and '$b' are not lexically equal"

  [[ $a > $b ]] && echo "'$a' is lexically after '$b'"
  [[ $a < $b ]] && echo "'$a' is lexically before '$b'"

  shopt -s nocasematch # Turn on case insensitivity

  [[ $a = $b ]] && echo "'$a' and '$b' are equal with case insensitivity"

  shopt -u nocasematch # Turn off case insensitivity

  # If args are numeric, perform some numeric comparisions
  if isint $a && isint $b
  then
     [[ $a -eq $b ]] && echo "$a is numerically equal to $b"
     [[ $a -gt $b ]] && echo "$a is numerically greater than $b"
     [[ $a -lt $b ]] && echo "$a is numerically less than $b"
  fi

  echo
}


compare foo foo
compare foo bar
compare FOO foo
compare 24 123
compare 50 20

```


{{out}}

```txt

'foo' and 'foo' are lexically equal
'foo' and 'foo' are equal with case insensitivity

'foo' and 'bar' are not lexically equal
'foo' is lexically after 'bar'

'FOO' and 'foo' are not lexically equal
'FOO' is lexically before 'foo'
'FOO' and 'foo' are equal with case insensitivity

'24' and '123' are not lexically equal
'24' is lexically after '123'
24 is numerically less than 123

'50' and '20' are not lexically equal
'50' is lexically after '20'
50 is numerically greater than 20

```



## WDTE


```WDTE
== 'example1' 'example2' -- io.writeln io.stdout; # Test for exact equality.
== 'example1' 'example2' -> ! -- io.writeln io.stdout; # Test for inequality.
< 'example1' 'example2' -- io.writeln io.stdout; # Test for lexical before.
> 'example1' 'example2' -- io.writeln io.stdout; # Test for lexical after.

# Case insensitive equality check by converting both to lowercase.
let str => import 'strings';
== (str.lower 'eXaMpLe') (str.lower 'ExAmPlE') -- io.writeln io.stdout;

# This is false. Strings are not coerced to numbers and vice-versa.
== '3' 3 -- io.writeln io.stdout;
```



## zkl


```zkl
"foo" == "foo"        //True
"foo" == "FOO"        //False
"foo" == "foobar"     //False
Op("==")("foo","foo") //True
Op("==","foo")("foo") //True

"abc"<"cde"       //True
"abc">"cde"       //False

"foo" == "FOO"         //False
"abc".toUpper()=="ABC" //True

123=="123"         //False
123=="123".toInt() //True

123<"123"   //False, int on left forces "123".toInt()
123<"1234"  //True
2345<"1234" //False
```



{{omit from|bc|No string operations in bc}}
{{omit from|Brlcad}}
{{omit from|dc|No string operations in dc}}
{{omit from|GUISS}}
{{omit from|Lilypond}}
{{omit from|Openscad}}
