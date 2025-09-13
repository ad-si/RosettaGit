+++
title = "Balanced brackets"
description = ""
date = 2019-10-15T08:00:03Z
aliases = []
[extra]
id = 9279
[taxonomies]
categories = ["task"]
tags = []
+++

## Task

'''Task''':
* Generate a string with   '''N'''   opening brackets   <big>'''['''</big>   and with   '''N'''   closing brackets   <big>''']'''</big>,   in some arbitrary order.
* Determine whether the generated string is ''balanced''; that is, whether it consists entirely of pairs of opening/closing brackets (in that order), none of which mis-nest.



;Examples:
    (empty)      OK
    []           OK
    [][]         OK
    [[][]]       OK
    ][         NOT OK
    ][][       NOT OK
    []][[]     NOT OK





## 11l

{{trans|Python}}

```11l
F gen(n)
   V txt = [‘[’, ‘]’] * n
   random:shuffle(&txt)
   R txt.join(‘’)

F is_balanced(s)
   V nesting_level = 0
   L(c) s
      S c
         ‘[’
            nesting_level++
         ‘]’
            I --nesting_level < 0
               R 0B
   R 1B

L(n) 0..9
   V s = gen(n)
   print(s‘’(‘ ’ * (20 - s.len))‘is ’(I is_balanced(s) {‘balanced’} E ‘not balanced’))
```

{{out}}

```txt

                    is balanced
[]                  is balanced
[]][                is not balanced
][[[]]              is not balanced
[]][][[]            is not balanced
][[][[[]]]          is not balanced
[[]]][[][]][        is not balanced
[[]][[]]]][[][      is not balanced
[]]][[[[]]]]][[[    is not balanced
]][]]][[[[[]][]][[  is not balanced

```



## 360 Assembly


```360asm
*        Balanced brackets         28/04/2016
BALANCE  CSECT
         USING  BALANCE,R13        base register and savearea pointer
SAVEAREA B      STM-SAVEAREA(R15)
         DC     17F'0'
STM      STM    R14,R12,12(R13)
         ST     R13,4(R15)
         ST     R15,8(R13)
         LR     R13,R15            establish addressability
         LA     R8,1               i=1
LOOPI    C      R8,=F'20'          do i=1 to 20
         BH     ELOOPI
         MVC    C(20),=CL20' '     c=' '
         LA     R1,1
         LA     R2,10
         BAL    R14,RANDOMX
         LR     R11,R0             l=randomx(1,10)
         SLA    R11,1              l=l*2
         LA     R10,1              j=1
LOOPJ    CR     R10,R11            do j=1 to 2*l
         BH     ELOOPJ
         LA     R1,0
         LA     R2,1
         BAL    R14,RANDOMX
         LR     R12,R0             m=randomx(0,1)
         LTR    R12,R12            if m=0
         BNZ    ELSEM
         MVI    Q,C'['             q='['
         B      EIFM
ELSEM    MVI    Q,C']'             q=']'
EIFM     LA     R14,C-1(R10)       @c(j)
         MVC    0(1,R14),Q         c(j)=q
         LA     R10,1(R10)         j=j+1
         B      LOOPJ
ELOOPJ   BAL    R14,CHECKBAL
         LR     R2,R0
         C      R2,=F'1'           if checkbal=1
         BNE    ELSEC
         MVC    PG+24(2),=C'ok'    rep='ok'
         B      EIFC
ELSEC    MVC    PG+24(2),=C'? '    rep='? '
EIFC     XDECO  R8,XDEC            i
         MVC    PG+0(2),XDEC+10
         MVC    PG+3(20),C
         XPRNT  PG,26
         LA     R8,1(R8)           i=i+1
         B      LOOPI
ELOOPI   L      R13,4(0,R13)
         LM     R14,R12,12(R13)
         XR     R15,R15            set return code to 0
         BR     R14 -------------- end
CHECKBAL CNOP   0,4 -------------- checkbal
         SR     R6,R6              n=0
         LA     R7,1               k=1
LOOPK    C      R7,=F'20'          do k=1 to 20
         BH     ELOOPK
         LR     R1,R7              k
         LA     R4,C-1(R1)         @c(k)
         MVC    CI(1),0(R4)        ci=c(k)
         CLI    CI,C'['            if ci='['
         BNE    NOT1
         LA     R6,1(R6)           n=n+1
NOT1     CLI    CI,C']'            if ci=']'
         BNE    NOT2
         BCTR   R6,0               n=n-1
NOT2     LTR    R6,R6              if n<0
         BNM    NSUP0
         SR     R0,R0              return(0)
         B      RETCHECK
NSUP0    LA     R7,1(R7)           k=k+1
         B      LOOPK
ELOOPK   LTR    R6,R6              if n=0
         BNZ    ELSEN
         LA     R0,1               return(1)
         B      RETCHECK
ELSEN    SR     R0,R0              return(0)
RETCHECK BR     R14 -------------- end checkbal
RANDOMX  CNOP   0,4 -------------- randomx
         LR     R3,R2              i2
         SR     R3,R1              ii=i2-i1
         L      R5,SEED
         M      R4,=F'1103515245'
         A      R5,=F'12345'
         SRDL   R4,1               shift to improve the algorithm
         ST     R5,SEED            seed=(seed*1103515245+12345)>>1
         LR     R6,R3              ii
         LA     R6,1(R6)           ii+1
         L      R5,SEED            seed
         LA     R4,0               clear
         DR     R4,R6              seed//(ii+1)
         AR     R4,R1              +i1
         LR     R0,R4              return(seed//(ii+1)+i1)
         BR     R14 -------------- end randomx
SEED     DC     F'903313037'
C        DS     20CL1
Q        DS     CL1
CI       DS     CL1
PG       DC     CL80' '
XDEC     DS     CL12
         REGS
         END    BALANCE
```

{{out}}

```txt

 1 ][[[][[]             ?
 2 ]][]][][[[[][[       ?
 3 []                   ok
 4 ][                   ?
 5 ][[][]]]]]           ?
 6 ][]]                 ?
 7 ]][][][]]]           ?
 8 [[[[][[[[][[]]       ?
 9 ][[]][[[[[[[[[]]][   ?
10 ]]]][][[][]]][][[[   ?
11 [][[]][][][[[]       ?
12 ]]                   ?
13 [][[[]]]             ok
14 ][                   ?
15 []][                 ?
16 ][[]][]]][[]         ?
17 ][][]]               ?
18 []                   ok
19 [[[[[[][[[[[][][     ?
20 [[][[][]             ?

```



## ABAP


```ABAP

CLASS lcl_balanced_brackets DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      class_constructor,

      are_brackets_balanced
        IMPORTING
          seq                            TYPE string
        RETURNING
          VALUE(r_are_brackets_balanced) TYPE abap_bool,

      get_random_brackets_seq
        IMPORTING
          n                    TYPE i
        RETURNING
          VALUE(r_bracket_seq) TYPE string.

  PRIVATE SECTION.
    CLASS-DATA: random_int TYPE REF TO cl_abap_random_int.

    CLASS-METHODS:
      _split_string
        IMPORTING
          i_text         TYPE string
        RETURNING
          VALUE(r_chars) TYPE stringtab,

      _rand_bool
        RETURNING
          VALUE(r_bool) TYPE i.
ENDCLASS.

CLASS lcl_balanced_brackets IMPLEMENTATION.
  METHOD class_constructor.
    random_int = cl_abap_random_int=>create( seed = CONV #( sy-uzeit )
                                             min  = 0
                                             max  = 1 ).
  ENDMETHOD.

  METHOD are_brackets_balanced.
    DATA: open_bracket_count TYPE i.

    DATA(chars) = _split_string( seq ).

    r_are_brackets_balanced = abap_false.

    LOOP AT chars ASSIGNING FIELD-SYMBOL(<c>).
      IF <c> = ']' AND open_bracket_count = 0.
        RETURN.
      ENDIF.

      IF <c> = ']'.
        open_bracket_count = open_bracket_count - 1.
      ENDIF.

      IF <c> = '['.
        open_bracket_count = open_bracket_count + 1.
      ENDIF.
    ENDLOOP.

    IF open_bracket_count > 0.
      RETURN.
    ENDIF.

    r_are_brackets_balanced = abap_true.
  ENDMETHOD.

  METHOD get_random_brackets_seq.
    DATA(itab) = VALUE stringtab( FOR i = 1 THEN i + 1 WHILE i <= n
                                     ( COND #( WHEN _rand_bool( ) = 0 THEN '['
                                               ELSE ']' ) ) ).
    r_bracket_seq = concat_lines_of( itab ).
  ENDMETHOD.

  METHOD _rand_bool.
    r_bool = random_int->get_next( ).
  ENDMETHOD.

  METHOD _split_string.
    DATA: off TYPE i VALUE 0.

    DO strlen( i_text ) TIMES.
      INSERT i_text+off(1) INTO TABLE r_chars.
      off = off + 1.
    ENDDO.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DO 10 TIMES.
    DATA(seq) = lcl_balanced_brackets=>get_random_brackets_seq( 10 ).
    cl_demo_output=>write( |{ seq } => { COND string( WHEN lcl_balanced_brackets=>are_brackets_balanced( seq ) = abap_true THEN 'OK'
                                                      ELSE 'NOT OK' ) }| ).
  ENDDO.
  cl_demo_output=>display( ).


```


{{out}}

```txt

[[]][[]]]] => NOT OK

][]][[][][ => NOT OK

[][]]][[[] => NOT OK

][][[[][[] => NOT OK

[[[][]]][] => OK

][][]][[[[ => NOT OK

][][[[[]][ => NOT OK

][[][]][[] => NOT OK

[[]][[]][] => OK

[][]]][]]] => NOT OK


```



## Acurity Architect


```txt

Using #HASH-OFF

```


```Acurity Architect

FUNCTION bBRACKETS_MATCH(zStringWithBrackets: STRING): STRING
  VAR sCount: SHORT
  VAR sBracketCounter: SHORT
  VAR zOK: STRING
  //
  SET zOK = "NOT OK"
  DO sCount = 1 TO LENGTH(zStringWithBrackets)
    CASE SUBSTR(zStringWithBrackets, sCount, 1)
      VALUE "["
        SET sBracketCounter = sBracketCounter + 1
      VALUE "]"
        SET sBracketCounter = sBracketCounter - 1
    ENDCASE
  ENDDO
  IF sBracketCounter = 0
    SET zOK = "OK"
  ENDIF
  RETURN zOK
ENDFUNCTION

```

{{out}}

```txt

bBRACKETS_MATCH("[][][][][][][[[[[]]]]]") = "OK"
bBRACKETS_MATCH("sdapasd[a]dfa[fdf][f]a[era[d]as[a]sd[as][da]s[") = "NOT OK"
bBRACKETS_MATCH("3r acwf4[a][ a]sg5s]4t[5e4][taw][ra][r] c[ra]2[r]") = "NOT OK"
bBRACKETS_MATCH("234 rq4ctac3rc[q2 ]r[q4tq4 ]t[4v5 y7 [e6y[]a[45 rv[2q]5q[2q3") = "NOT OK"

```



## Ada

brackets.adb:

```Ada
with Ada.Numerics.Discrete_Random;
with Ada.Text_IO;
with Ada.Strings.Fixed;
procedure Brackets is
   package Random_Positive is new Ada.Numerics.Discrete_Random (Positive);
   Positive_Generator : Random_Positive.Generator;
   procedure Swap (Left, Right : in out Character) is
      Temp : constant Character := Left;
   begin
      Left := Right;
      Right := Temp;
   end Swap;
   function Generate_Brackets (Bracket_Count : Natural;
                               Opening_Bracket : Character := '[';
                               Closing_Bracket : Character := ']')
            return String is
      use Ada.Strings.Fixed;
      All_Brackets : String := Bracket_Count * Opening_Bracket & Bracket_Count * Closing_Bracket;
   begin
      for I in All_Brackets'Range loop
         Swap (All_Brackets (I), All_Brackets (Random_Positive.Random (Positive_Generator) mod (Bracket_Count * 2) + 1));
      end loop;
      return All_Brackets;
   end Generate_Brackets;

   function Check_Brackets (Test : String;
                            Opening_Bracket : Character := '[';
                            Closing_Bracket : Character := ']')
            return Boolean is
      Open : Natural := 0;
   begin
      for I in Test'Range loop
         if Test (I) = Opening_Bracket then
            Open := Open + 1;
         elsif Test (I) = Closing_Bracket then
            if Open = 0 then
               return False;
            else
               Open := Open - 1;
            end if;
         end if;
      end loop;
      return True;
   end Check_Brackets;
begin
   Random_Positive.Reset (Positive_Generator);
   Ada.Text_IO.Put_Line ("Brackets");
   for I in 0 .. 4 loop
      for J in 0 .. I loop
         declare
            My_String : constant String := Generate_Brackets (I);
         begin
            Ada.Text_IO.Put_Line (My_String & ": " & Boolean'Image (Check_Brackets (My_String)));
         end;
      end loop;
   end loop;
end Brackets;
```


Output:

```txt
Brackets
: TRUE
[]: TRUE
][: FALSE
[]][: FALSE
][][: FALSE
][][: FALSE
[[]]][: FALSE
[][][]: TRUE
[]]][[: FALSE
][][][: FALSE
]]][[[[]: FALSE
[[[]][]]: TRUE
[][][]][: FALSE
[[][]]][: FALSE
[]]]][[[: FALSE
```



## Aime


```aime
unbalanced(data s)
{
    integer b, i;

    b = i = 0;
    while (i + ~s && -1 < b) {
        b += s[i -= 1] == '[' ? -1 : 1;
    }

    b;
}

generate(data b, integer d)
{
    if (d) {
        d.times(l_bill, list(), -1, '[', ']').l_rand().ucall(b_append, 1, b);
    }
}

main(void)
{
    integer i;

    i = 0;
    while (i < 10) {
        data s;

        generate(s, i);
        o_(s, " is ", unbalanced(s) ? "un" : "", "balanced\n");

        i += 1;
    }

    0;
}
```

Sample output:

```txt
 is balanced
][ is unbalanced
]][[ is unbalanced
]]][[[ is unbalanced
[[]][][] is balanced
[[][]][][] is balanced
[]]]][][[][[ is unbalanced
][[[]][[][]]][ is unbalanced
[][[[][[]]]]][][ is unbalanced
[[][[[[][]]][][]]] is balanced
```



## ALGOL 68

{{works with|ALGOL 68G|Any - tested with release 2.8.win32}}

```algol68
# generates a string of random opening and closing brackets. The number of    #
# each type of brackets is speccified in length                               #
PROC get brackets = ( INT length ) STRING:
    BEGIN
        INT   result length = length * 2;
        [ 1 : result length ]CHAR result;
        # initialise the brackets to all open brackets                        #
        FOR char pos TO result length DO result[ char pos ] := "[" OD;
        # set half of the brackets to close brackets                          #
        INT   close count := 0;
        WHILE close count < length
        DO
            INT   random pos = 1 + ENTIER ( next random * result length );
            IF result[ random pos ] = "["
            THEN
                close count          +:= 1;
                result[ random pos ]  := "]"
            FI
        OD;
        result
    END # get brackets # ;

# returns TRUE if the brackets string contains a correctly nested sequence    #
# of brackets, FALSE otherwise                                                #
PROC check brackets = ( STRING brackets ) BOOL:
    BEGIN
        INT depth := 0;
        FOR char pos FROM LWB brackets TO UPB brackets
        WHILE
            IF brackets[ char pos ] = "["
            THEN
                depth +:= 1
            ELSE
                depth -:= 1
            FI;
            depth >= 0
        DO
            SKIP
        OD;
        # depth will be 0 if we reached the end of the string and it was     #
        # correct, non-0 otherwise                                           #
        depth = 0
    END # check brackets # ;

# procedure to test check brackets                                           #
PROC test check brackets = ( STRING brackets ) VOID:
    print( ( ( brackets
             + ": "
             + IF check brackets( brackets ) THEN "ok" ELSE "not ok" FI
             )
           , newline
           )
         ) ;

# test the bracket generation and checking PROCs                             #
test check brackets( get brackets( 0 ) );
FOR length TO 12
DO
    TO 2
    DO
        test check brackets( get brackets( length ) )
    OD
OD
```

{{out}}

```txt

: ok
[]: ok
][: not ok
[][]: ok
[]][: not ok
[]]][[: not ok
[[]][]: ok
]]][[][[: not ok
[[][[]]]: ok
[][]]][[][: not ok
[[[[]]]][]: ok
]][]]][[[[][: not ok
[[[][[[]]]]]: ok
[[[]]][[][[]]]: ok
[][[][][][][]]: ok
[]]][][]][[[[]][: not ok
]][]][][[[][][][: not ok
][][]][]][]][[[[[]: not ok
]]][[][][][[[][]][: not ok
[[[[][][]][]]][[]]][: not ok
][]][]]][][][][][[[[: not ok
][][]][[][[[[]][][[]]]: not ok
]][[[]][[[[]]]][[[]]][: not ok
]][][]]][]][][]][[[[[[][: not ok
]]][][][]][][[]][[[][][[: not ok

```



## ANTLR

[[File:Bb.png|left|BalancedBrackets]]
[[File:BalancedBrackets.png|left|BalancedBrackets]]
<br clear=both>
=
## Java
=
<lang>
grammar balancedBrackets ;

options {
	language = Java;
}

bb	:	{System.out.print("input is: ");} (balancedBrackets {System.out.print($balancedBrackets.text);})* NEWLINE {System.out.println();}
	;
balancedBrackets
	:	OpenBracket balancedBrackets* CloseBracket
	;
OpenBracket
	:	'['
	;
CloseBracket
	:	']'
	;
NEWLINE	:	'\r'? '\n'
	;

```

Produces:

```txt


input is:
[]
input is: []
][
input is: line 1:0 missing NEWLINE at ']'
[][]
input is: [][]
][][
input is: line 1:0 missing NEWLINE at ']'
[[][]]
input is: [[][]]
[]][[]
input is: []line 1:2 missing NEWLINE at ']'

```




## AppleScript


{{trans|JavaScript}}
(ES6 functionally composed version)


```AppleScript
-- CHECK NESTING OF SQUARE BRACKET SEQUENCES ---------------------------------

-- Zero-based index of the first problem (-1 if none found):

-- imbalance :: String -> Integer
on imbalance(strBrackets)
    script
        on errorIndex(xs, iDepth, iIndex)
            set lngChars to length of xs
            if lngChars > 0 then
                set iNext to iDepth + cond(item 1 of xs = "[", 1, -1)

                if iNext < 0 then -- closing bracket unmatched
                    iIndex
                else
                    if lngChars > 1 then -- continue recursively
                        errorIndex(items 2 thru -1 of xs, iNext, iIndex + 1)
                    else -- end of string
                        cond(iNext = 0, -1, iIndex)
                    end if
                end if
            else
                cond(iDepth = 0, -1, iIndex)
            end if
        end errorIndex
    end script

    result's errorIndex(characters of strBrackets, 0, 0)
end imbalance

-- TEST ----------------------------------------------------------------------

-- Random bracket sequences for testing
-- brackets :: Int -> String
on randomBrackets(n)
    -- bracket :: () -> String
    script bracket
        on |λ|(_)
            cond((random number) < 0.5, "[", "]")
        end |λ|
    end script
    intercalate("", map(bracket, enumFromTo(1, n)))
end randomBrackets

on run
    set nPairs to 6

    -- report :: Int -> String
    script report
        property strPad : concat(replicate(nPairs * 2 + 4, space))

        on |λ|(n)
            set w to n * 2
            set s to randomBrackets(w)
            set i to imbalance(s)
            set blnOK to (i = -1)

            set strStatus to cond(blnOK, "OK", "problem")

            set strLine to "'" & s & "'" & ¬
                (items (w + 2) thru -1 of strPad) & strStatus

            set strPointer to cond(blnOK, ¬
                "", linefeed & concat(replicate(i + 1, space)) & "^")

            intercalate("", {strLine, strPointer})
        end |λ|
    end script

    linefeed & ¬
        intercalate(linefeed, ¬
            map(report, enumFromTo(1, nPairs))) & linefeed
end run


-- GENERIC FUNCTIONS ---------------------------------------------------------

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

-- Text -> [Text] -> Text
on intercalate(strText, lstText)
    set {dlm, my text item delimiters} to {my text item delimiters, strText}
    set strJoined to lstText as text
    set my text item delimiters to dlm
    return strJoined
end intercalate

-- concat :: [[a]] -> [a] | [String] -> String
on concat(xs)
    script append
        on |λ|(a, b)
            a & b
        end |λ|
    end script

    if length of xs > 0 and class of (item 1 of xs) is string then
        set empty to ""
    else
        set empty to {}
    end if
    foldl(append, empty, xs)
end concat

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

-- Value of one of two expressions
-- cond ::  Bool -> a -> b -> c
on cond(bln, f, g)
    if bln then
        set e to f
    else
        set e to g
    end if
    if class of e is handler then
        mReturn(e)'s |λ|()
    else
        e
    end if
end cond

-- enumFromTo :: Int -> Int -> [Int]
on enumFromTo(m, n)
    if m > n then
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

'''Sample output:'''

```txt
']['             problem
 ^
'[][['           problem
    ^
'[[][]]'         OK
'][][[][['       problem
 ^
'[]][][[][]'     problem
   ^
'[[[][]]]][]['   problem
         ^
```



## ARM Assembly

<lang ARM_Assembly>
.data

balanced_message:
  .ascii "OK\n"

unbalanced_message:
  .ascii "NOT OK\n"


.text

.equ balanced_msg_len, 3
.equ unbalanced_msg_len, 7


BalancedBrackets:

  mov r1, #0
  mov r2, #0
  mov r3, #0

  process_bracket:
    ldrb r2, [r0, r1]

    cmp r2, #0
    beq evaluate_balance

    cmp r2, #'['
    addeq r3, r3, #1

    cmp r2, #']'
    subeq r3, r3, #1

    cmp r3, #0
    blt unbalanced

    add r1, r1, #1
    b process_bracket

  evaluate_balance:
    cmp r3, #0
    beq balanced

    unbalanced:
       ldr r1, =unbalanced_message
       mov r2, #unbalanced_msg_len
       b display_result

    balanced:
       ldr r1, =balanced_message
       mov r2, #balanced_msg_len

    display_result:
      mov r7, #4
      mov r0, #1
      svc #0

      mov pc, lr


```



## Arturo



```arturo
isBalanced [s]{
	cnt 0

	loop $(characters s) [c]{
		if c="]" {
			if $(cnt cnt-1)<0 { return false }
		} {
			if c="[" { cnt cnt+1 }
		}
    }

    return cnt=0
}

loop $(range 1 9) [i]{
	str $(join $(map $(range 0 2*i-1) { sample #("[" "]") }) "")
	print str true
	if $(isBalanced str) { print " OK" } { print " Not OK" }
}
```


{{out}}


```txt
[] OK
[[]] OK
[[]]][ Not OK
]]][[]]] Not OK
[[][][][[] Not OK
[[][[[[]]]]] OK
[[][[][[]][[][ Not OK
]]][[[][[]][[[]] Not OK
]]][][[[][][][]]]] Not OK
```



## AutoHotkey


```AutoHotkey
; Generate 10 strings with equal left and right brackets
Loop, 5
{
	B = %A_Index%
	loop 2
	{
		String =
		Loop % B
			String .= "[`n"
		Loop % B
			String .= "]`n"
		Sort, String, Random
		StringReplace, String, String,`n,,All
		Example .= String " - " IsBalanced(String) "`n"
	}
}
	MsgBox % Example
return

IsBalanced(Str)
{
	Loop, PARSE, Str
	{
		If A_LoopField = [
			i++
		Else if A_LoopField = ]
			i--
		If i < 0
			return "NOT OK"
	}
	Return "OK"
}
```

Output:

```txt

][ - NOT OK
][ - NOT OK
[][] - OK
[[]] - OK
[]][[] - NOT OK
]][[[] - NOT OK
][[]][][ - NOT OK
[][[[]]] - OK
[][]]][[[] - NOT OK
[[][[]]][] - OK

```


A second example repeatedly replacing []:

```AutoHotkey
Loop, 5
{
	B = %A_Index%
	loop 2
	{
		String =
		Loop % B
			String .= "[`n"
		Loop % B
			String .= "]`n"
		Sort, String, Random
		StringReplace, String, String,`n,,All
		Example .= String " - " IsBalanced(String) "`n"
	}
}
	MsgBox % Example
return

IsBalanced(Str){
	While (Instr(Str,"[]"))
		StringReplace, Str, Str,[],,All
	Return Str ? "False" : "True"
}
```

Sample output:

```txt

[] - True
][ - False
]][[ - False
[]][ - False
[[]]][ - False
]][[[] - False
[][]]][[ - False
[]][][][ - False
[[[][]][]] - True
[][][[[]]] - True

```



## AutoIt


```AutoIt

#include <Array.au3>
Local $Array[1]
_ArrayAdd($Array, "[]")
_ArrayAdd($Array, "[][]")
_ArrayAdd($Array, "[[][]]")
_ArrayAdd($Array, "][")
_ArrayAdd($Array, "][][")
_ArrayAdd($Array, "[]][[]")

For $i = 0 To UBound($Array) -1
	Balanced_Brackets($Array[$i])
	If @error Then
		ConsoleWrite($Array[$i] &" = NOT OK"&@CRLF)
	Else
		ConsoleWrite($Array[$i] &" = OK"&@CRLF)
	EndIf
Next

Func Balanced_Brackets($String)
	Local $cnt = 0
	$Split = Stringsplit($String, "")
	For $i = 1 To $Split[0]
		If $split[$i] = "[" Then $cnt += 1
		If $split[$i] = "]" Then $cnt -= 1
		If $cnt < 0 Then Return SetError(1,0,0)
	Next
	Return 1
EndFunc

```



## AWK



```AWK
#!/usr/bin/awk -f
BEGIN {
   print isbb("[]")
   print isbb("][")
   print isbb("][][")
   print isbb("[][]")
   print isbb("[][][]")
   print isbb("[]][[]")
}

function isbb(x) {
   s = 0
   for (k=1; k<=length(x); k++) {
	c = substr(x,k,1)
	if (c=="[") {s++}
	else { if (c=="]") s--	}

        if (s<0) {return 0}
   }
   return (s==0)
}

```

Output:

```txt
1
0
0
1
1
0
```



## BASIC

{{works with|QBasic}}


```qbasic
DECLARE FUNCTION checkBrackets% (brackets AS STRING)
DECLARE FUNCTION generator$ (length AS INTEGER)

RANDOMIZE TIMER

DO
    x$ = generator$ (10)
    PRINT x$,
    IF checkBrackets(x$) THEN
        PRINT "OK"
    ELSE
        PRINT "NOT OK"
    END IF
LOOP WHILE LEN(x$)

FUNCTION checkBrackets% (brackets AS STRING)
    'returns -1 (TRUE) if everything's ok, 0 (FALSE) if not
    DIM L0 AS INTEGER, sum AS INTEGER

    FOR L0 = 1 TO LEN(brackets)
        SELECT CASE MID$(brackets, L0, 1)
            CASE "["
                sum = sum + 1
            CASE "]"
                sum = sum - 1
        END SELECT
        IF sum < 0 THEN
            checkBrackets% = 0
            EXIT FUNCTION
        END IF
    NEXT

    IF 0 = sum THEN
        checkBrackets% = -1
    ELSE
        checkBrackets% = 0
    END IF
END FUNCTION

FUNCTION generator$ (length AS INTEGER)
    z = INT(RND * length)
    IF z < 1 THEN generator$ = "": EXIT FUNCTION
    REDIM x(z * 2) AS STRING
    FOR i = 0 TO z STEP 2
        x(i) = "["
        x(i + 1) = "]"
    NEXT
    FOR i = 1 TO UBOUND(x)
        z = INT(RND * 2)
        IF z THEN SWAP x(i), x(i - 1)
    NEXT
    xx$ = ""
    FOR i = 0 TO UBOUND(x)
        xx$ = xx$ + x(i)
    NEXT
    generator$ = xx$
END FUNCTION
```


Sample output:
 [][[][][]]    OK
 ][[[]]        NOT OK
 [][]          OK
 []][][][[]    NOT OK
 [][[]][[]]    OK
 ][][[]        NOT OK
 ][[]          NOT OK
 ][][[]][][    NOT OK
 ][[[][]]      NOT OK
 ][][[[]]      NOT OK
 ][[]][        NOT OK
               OK

=
## Commodore BASIC
=
Based on ZX Spectrum BASIC implementation

```basic
10 PRINT CHR$(147): REM CLEAR SCREEN
20 FOR N=1 TO 7
30   READ S$
40   IF S$="" THEN PRINT"(EMPTY)";: GOTO 60
50   PRINT S$;
60   PRINT TAB(20);
70   GOSUB 1000
80 NEXT N
90 END
100 REM ********************************
1000 S = 0
1010 FOR K=1 TO LEN(S$)
1020   C$ = MID$(S$,K,1)
1030   IF C$="[" THEN S = S+1
1040   IF C$="]" THEN S = S-1
1050   IF S<0 THEN PRINT "NOT OK": RETURN
1060 NEXT K
1070 IF S=0 THEN PRINT "OK": RETURN
1090 PRINT "NOT OK"
1100 RETURN
2000 DATA , [], ][, [][], ][][, [[][]], []][[]
```



## Batch File

Uses a Markov algorithm/code <code>"[]" -> null</code> to check if brackets are balanced.

```dos
:: Balanced Brackets Task from Rosetta Code Wiki
:: Batch File Implementation

@echo off
setlocal enabledelayedexpansion

::The Main Thing...
set numofpairs=10
set howmanystrings=10
cls
for /l %%. in (1,1,%howmanystrings%) do (
	call :generate
	call :checkforbalance
)
echo.&pause&exit /b
::/The Main Thing.

::Generate strings of brackets...
:generate
	set i=0&set j=%numofpairs%&set samp=
	set /a toss=%random%%%2
	set put1=[&set put2=]
	if %toss%==1 (set put1=]&set put2=[)
	for /l %%x in (1,1,%numofpairs%) do (
		set samp=!samp!%put1%
	)
	:add
	if not %i%==%numofpairs% (
		set /a rnd=%random%%%%j%+1
		set /a oppos=%j%-!rnd!
		::A new trick for substitution of delayed variables...
		for /f "tokens=1-2" %%A in ("!rnd! !oppos!") do (
			set str1=!samp:~-%%A!
			set str2=!samp:~0,%%B!
		)
		set samp=!str2!%put2%!str1!
		set /a "j+=1","i+=1"
		goto :add
	)
goto :EOF
::/Generate strings of brackets.

::Check for Balance...
::Uses Markov Algorithm.
:checkforbalance
set "changes=!samp!"
:check_loop
if "!changes!"=="" goto itsbal
if "!input!"=="!changes!" goto notbal

set input=!changes!
set "changes=!input:[]=!"
goto check_loop

:itsbal
echo.
echo %samp% is Balanced.
goto :EOF
:notbal
echo.
echo %samp% is NOT Balanced.
goto :EOF
::/Check for Balance.
```

{{out}}

```txt

[][][[]][][]]][][][[ is NOT Balanced.

[[[[[]][[]][]][][]]] is Balanced.

[[[[[]]]]][][][]][][ is NOT Balanced.

][[[[[[[]]][]]][][]] is NOT Balanced.

[][[[[]]]][[[[]]]][] is Balanced.

[[[][[[]][][]]]][][] is Balanced.

[[][[]][[]]]]][][[[] is NOT Balanced.

[][][]][][]]][][][[[ is NOT Balanced.

[[]][][][[][[[]]]][] is Balanced.

[[[[[][[][[]]]]][]]] is Balanced.

Press any key to continue . . .
```



## BBC BASIC


```bbcbasic
FOR x%=1 TO 10
test$=FNgenerate(RND(10))
  PRINT "Bracket string ";test$;" is ";FNvalid(test$)
NEXT x%
END
:
DEFFNgenerate(n%)
LOCAL l%,r%,t%,output$
WHILE l%<n% AND r%<n%
  CASE RND(2) OF
    WHEN 1:
      l%+=1
      output$+="["
    WHEN 2:
      r%+=1
      output$+="]"
  ENDCASE
ENDWHILE
IF l%=n% THEN output$+=STRING$(n%-r%,"]") ELSE output$+=STRING$(n%-l%,"[")
=output$
:
DEFFNvalid(q$)
LOCAL x%,count%
IF LEN(q$)=0 THEN ="OK."
FOR x%=1 TO LEN(q$)
  IF MID$(q$,x%,1)="[" THEN count%+=1 ELSE count%-=1
  IF count%<0 THEN ="not OK."
NEXT x%
="OK."
```


```txt
Bracket string [[[][]]] is OK.
Bracket string [[[]][[[][[][]]]]] is OK.
Bracket string ][][]][[ is not OK.
Bracket string [][][] is OK.
Bracket string [][]][]][[]]][[[ is not OK.
Bracket string ]][[[[]]]][]]][[[[ is not OK.
Bracket string [[][[[]]][]] is OK.
Bracket string []][][][[[]] is not OK.
Bracket string ][]][[ is not OK.
Bracket string []][][][[] is not OK.
```



## Befunge

{{works with|befungee}}
This code implements the second part of the task: it reads from standard input an arbitrary string of opening and closing brackets, and checks whether it's balanced or not.

```Befunge>v
 "KO TON" ,,,,,,   v
> ~ : 25*- #v_ $ |                   > 25*, @
                 > "KO" ,,           ^
            > : 1991+*+- #v_ v
                          > \ : 1991+*+- #v_v
                                          \ $
^                            <            <$<
```



## Bracmat

Bracmat has no 'random' function, so the shuffle is a bit improvised. A variable <code>someNumber</code> is initialised with a big number is repeatedly divided by the number of '['s in the test string until zero. The remainders are used as index to partition and swap the first half of the test string. Then the second half and first half are also swapped. The test whether the test string is balanced is simple, but not very efficient.

```bracmat
( (bal=|"[" !bal "]" !bal)
& ( generate
  =   a j m n z N S someNumber
    .   !arg:<1&
      |   11^123+13^666+17^321:?someNumber
        & (!arg:?n)+1:?N
        & :?S
        &   whl
          ' (!n+-1:~<0:?n&"[" "]" !S:?S)
        &   whl
          ' ( !someNumber:>0
            & mod$(!someNumber.!N):?j
            & div$(!someNumber.!N):?someNumber
            & !S:?a [!j ?m [!N ?z
            & !z !m !a:?S
            )
        & !S
  )
& 0:?L
&   whl
  ' ( generate$!L:?S
    & put$(str$(!S ":"))
    &   out
      $ (!S:!bal&Balanced|"Not balanced")
    & !L+1:<11:?L
    )
);
```

Output:

```txt
:Balanced
][:Not balanced
[][]:Balanced
[][[]]:Balanced
[[[]]]][:Not balanced
]]]][][[[[:Not balanced
[[][[[]]][]]:Balanced
[][]][]]]][[[[:Not balanced
[][]][[[]]][][][:Not balanced
[][[[[]][]]][[[]]]:Balanced
[][[][]][]]][][[[]][:Not balanced
```



## Brat


```brat
string.prototype.balanced? = {
  brackets = []
  balanced = true

  my.dice.each_while { c |
    true? c == "["
      { brackets << c }
      { true? c == "]"
        { last = brackets.pop
          false? last == "["
          { balanced = false }
        }
      }

    balanced
  }

  true? brackets.empty?
    { balanced }
    { false }
}

generate_brackets = { n | (n.of("[") + n.of("]")).shuffle.join }

1.to 10 { n |
  test = generate_brackets n

  true? test.balanced?
    { p "#{test} is balanced" }
    { p "#{test} is not balanced" }
}
```


Output:

```txt
[] is balanced
][][ is not balanced
[[]][] is balanced
[[[]][]] is balanced
[[[[]]]]][ is not balanced
][[][]][[[]] is not balanced
]][[][][[]][][ is not balanced
[[[][[]]][][][]] is balanced
][]][]]]][][[[][[[ is not balanced
][[[[][][][[][][]]]] is not balanced
```



## C


```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int isBal(const char*s,int l){
    signed c=0;
    while(l--)
	if(s[l]==']') ++c;
	else if(s[l]=='[') if(--c<0) break;
    return !c;
}

void shuffle(char*s,int h){
    int x,t,i=h;
    while(i--){
	t=s[x=rand()%h];
	s[x]=s[i];
	s[i]=t;
    }
}

void genSeq(char*s,int n){
    if(n){
	memset(s,'[',n);
	memset(s+n,']',n);
	shuffle(s,n*2);
    }
    s[n*2]=0;
}

void doSeq(int n){
    char s[64];
    const char *o="False";
    genSeq(s,n);
    if(isBal(s,n*2)) o="True";
    printf("'%s': %s\n",s,o);
}

int main(){
    int n=0;
    while(n<9) doSeq(n++);
    return 0;
}
```
result:<lang>'': True
'[]': True
']][[': False
'[][][]': True
'[]][[]][': False
'[]][[[[]]]': False
']]]][[[]][[[': False
']]]]]][][[[[[[': False
'[][]][[][[[]]][]': False
```


## C#

```c#
using System;
using System.Linq;

class Program
{
    static bool IsBalanced(string text, char open = '[', char close = ']')
    {
        var level = 0;
        foreach (var character in text)
        {
            if (character == close)
            {
                if (level == 0)
                {
                    return false;
                }
                level--;
            }
            if (character == open)
            {
                level++;
            }
        }
        return level == 0;
    }

    static string RandomBrackets(int count, char open = '[', char close = ']')
    {
        var random = new Random();
        return string.Join(string.Empty,
                (new string(open, count) + new string(close, count)).OrderBy(c => random.Next()));
    }

    static void Main()
    {
        for (var count = 0; count < 9; count++)
        {
            var text = RandomBrackets(count);
            Console.WriteLine("\"{0}\" is {1}balanced.", text, IsBalanced(text) ? string.Empty : "not ");
        }
    }
}
```

Sample output:

```txt
"" is balanced.
"[]" is balanced.
"[]][" is not balanced.
"[][][]" is balanced.
"[[[]][]]" is balanced.
"[][[][[]]]" is balanced.
"[]][][][[][]" is not balanced.
"[]]][][]][[][[" is not balanced.
"[]]][]]][[][[][[" is not balanced.
```


```c#

                       // simple solution
                       string input = Console.ReadLine();
			if (input.Length % 2 != 0)
			{
				Console.WriteLine("Not Okay");
				return;
			}
			for (int i = 0; i < input.Length; i++)
			{
				if (i < input.Length - 1)
				{
					if (input[i] == '[' && input[i + 1] == ']')
					{
						input = input.Remove(i, 2);
						i = -1;
					}
				}

			}
			if (input.Length == 0)
				Console.WriteLine("Okay");
			else
				Console.WriteLine("Not Okay");


```



## C++


```cpp
#include <algorithm>
#include <iostream>
#include <string>

std::string generate(int n, char left = '[', char right = ']')
{
    std::string str(std::string(n, left) + std::string(n, right));
    std::random_shuffle(str.begin(), str.end());
    return str;
}

bool balanced(const std::string &str, char left = '[', char right = ']')
{
    int count = 0;
    for (std::string::const_iterator it = str.begin(); it != str.end(); ++it)
    {
        if (*it == left)
            count++;
        else if (*it == right)
            if (--count < 0) return false;
    }
    return count == 0;
}

int main()
{
    srand(time(NULL)); // seed rng
    for (int i = 0; i < 9; ++i)
    {
        std::string s(generate(i));
        std::cout << (balanced(s) ? " ok: " : "bad: ") << s << "\n";
    }
}
```

Output:

```txt
 ok:
 ok: []
 ok: [][]
bad: []][[]
 ok: [[[]][]]
bad: ][[[[]][]]
 ok: [[[]][[]][]]
bad: ]][[]][[[[][]]
bad: [[]]]][]][[][[[]

```



## Ceylon


```Ceylon
import com.vasileff.ceylon.random.api {
    platformRandom,
    Random
}
"""Run the example code for Rosetta Code ["Balanced brackets" task] (http://rosettacode.org/wiki/Balanced_brackets)."""
shared void run() {
    value rnd = platformRandom();
    for (len in (0..10)) {
        value c = generate(rnd, len);
        print("``c.padTrailing(20)`` - ``if (balanced(c)) then "OK" else "NOT OK" ``");
    }
}

String generate(Random rnd, Integer count)
        => if (count == 0) then ""
           else let(length = 2*count,
                    brackets = zipEntries(rnd.integers(length).take(length),
                                          "[]".repeat(count))
                            .sort((a,b) => a.key<=>b.key)
                            .map(Entry.item))
                String(brackets);

Boolean balanced(String input)
        => let (value ints = { for (c in input) if (c == '[') then 1 else -1 })
           ints.filter((i) => i != 0)
               .scan(0)(plus<Integer>)
               .every((i) => i >= 0);
```


Output:


```txt
                     - OK
[]                   - OK
][[]                 - NOT OK
][[]][               - NOT OK
[]]][[][             - NOT OK
[[[][][]]]           - OK
[[][]][[]][]         - OK
[[][[[]]][]][]       - OK
]][[][][[][]][[]     - NOT OK
[[]]]]]]][[[[][][[   - NOT OK
[[]][[]][[]]]][[[]][ - NOT OK
```



## Clojure


```Clojure
(defn gen-brackets [n]
  (->> (concat (repeat n \[) (repeat n \]))
       shuffle
       (apply str ,)))

(defn balanced? [s]
  (loop [[first & coll] (seq s)
	 stack '()]
    (if first
      (if (= first \[)
	(recur coll (conj stack \[))
	(when (= (peek stack) \[)
	  (recur coll (pop stack))))
      (zero? (count stack)))))
```


There are other ways to express the <code>balanced?</code> function.

* We can use <code>reduce</code> to consume the sequence:
:
```Clojure
(defn balanced? [s]
  (empty?
    (reduce
      (fn [stack first]
        (case first
          \[ (conj stack \[)
          \] (if (seq stack)
               (pop stack)
               (reduced [:UNDERFLOW]))))
      '()
      s)))
```


* Only <code>[</code>s are put on the stack. We can just count the unmatched ones.
:
```Clojure
(defn balanced? [s]
  (let [opens-closes (->> s
                          (map {\[ 1, \] -1})
                          (reductions + 0))]
    (and (not-any? neg? opens-closes) (zero? (last opens-closes)))))
```


Output:


```txt
user> (->> (range 10)
     (map gen-brackets ,)
     (map (juxt identity balanced?) ,)
     vec)
[["" true]
 ["[]" true]
 ["[[]]" true]
 ["[][[]]" true]
 ["[]][][][" nil]
 ["[[[[[]]]]]" true]
 ["]][[][][[[]]" nil]
 ["[]]]][[[[]][][" nil]
 ["][][[]]][[][][][" nil]
 ["][][]]][]][[[][[[]" nil]
```



## COBOL

{{works with|OpenCOBOL}}

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. test-balanced-brackets.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  True-Val  CONSTANT 0.
       01  False-Val CONSTANT 1.

       LOCAL-STORAGE SECTION.
       01  current-time        PIC 9(10).

       01  bracket-type        PIC 9.
           88 add-open-bracket VALUE 1.

       01  bracket-string-area.
           03  bracket-string  PIC X(10) OCCURS 10 TIMES.

       01  i                   PIC 999.
       01  j                   PIC 999.

       PROCEDURE DIVISION.
           *> Seed RANDOM().
           MOVE FUNCTION CURRENT-DATE (7:10) TO current-time
           MOVE FUNCTION RANDOM(current-time) TO current-time


           *> Generate random strings of brackets.
           PERFORM VARYING i FROM 1 BY 1 UNTIL 10 < i
               PERFORM VARYING j FROM 1 BY 1 UNTIL i < j
                   COMPUTE bracket-type =
                       FUNCTION REM(FUNCTION RANDOM * 1000, 2)

                   IF add-open-bracket
                       MOVE "[" TO bracket-string (i) (j:1)
                   ELSE
                       MOVE "]" TO bracket-string (i) (j:1)
                   END-IF
               END-PERFORM
           END-PERFORM

           *> Display if the strings are balanced or not.
           PERFORM VARYING i FROM 1 BY 1 UNTIL 10 < i
               CALL "check-if-balanced" USING bracket-string (i)
               IF RETURN-CODE = True-Val
                   DISPLAY FUNCTION TRIM(bracket-string (i))
                       " is balanced."
               ELSE
                   DISPLAY FUNCTION TRIM(bracket-string (i))
                       " is not balanced."
               END-IF
           END-PERFORM

           GOBACK
           .

       END PROGRAM test-balanced-brackets.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. check-if-balanced.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  True-Val  CONSTANT 0.
       01  False-Val CONSTANT 1.

       LOCAL-STORAGE SECTION.
       01  nesting-level  PIC S999.
       01  i              PIC 999.

       LINKAGE SECTION.
       01  bracket-string PIC X(100).

       PROCEDURE DIVISION USING bracket-string.
           PERFORM VARYING i FROM 1 BY 1
                   UNTIL (100 < i)
                      OR (bracket-string (i:1) = SPACE)
                      OR (nesting-level < 0)
               IF bracket-string (i:1) = "["
                   ADD 1 TO nesting-level
               ELSE
                   SUBTRACT 1 FROM nesting-level
                   IF nesting-level < 0
                       MOVE False-Val TO RETURN-CODE
                       GOBACK
                   END-IF
               END-IF
           END-PERFORM

           IF nesting-level = 0
               MOVE True-Val TO RETURN-CODE
           ELSE
               MOVE False-Val TO RETURN-CODE
           END-IF

           GOBACK
           .

       END PROGRAM check-if-balanced.
```



## CoffeeScript


```coffeescript

isBalanced = (brackets) ->
  openCount = 0
  for bracket in brackets
    openCount += if bracket is '[' then 1 else -1
    return false if openCount < 0
  openCount is 0

bracketsCombinations = (n) ->
  for i in [0...Math.pow 2, n]
    str = i.toString 2
    str = '0' + str while str.length < n
    str.replace(/0/g, '[').replace(/1/g, ']')

for brackets in bracketsCombinations 4
  console.log brackets, isBalanced brackets

```

output
<lang>
> coffee balanced.coffee
[[[[ false
[[[] false
[[][ false
[[]] true
[][[ false
[][] true
[]][ false
[]]] false
][[[ false
][[] false
][][ false
][]] false
]][[ false
]][] false
]]][ false
]]]] false

```




## Common Lisp


```lisp

(defun string-of-brackets (n)
  (let* ((len (* 2 n))
         (res (make-string len))
         (opening (/ len 2))
         (closing (/ len 2)))
    (dotimes (i len res)
      (setf (aref res i)
            (cond ((zerop opening) #\])
                  ((zerop closing) #\[)
                  (t (if (= (random 2) 0)
                         (progn (decf opening) #\[)
                         (progn (decf closing) #\]))))))))

(defun balancedp (string)
  (zerop (reduce (lambda (nesting bracket)
                   (ecase bracket
                     (#\] (if (= nesting 0)
                              (return-from balancedp nil)
                              (1- nesting)))
                     (#\[ (1+ nesting))))
                 string
                 :initial-value 0)))

(defun show-balanced-brackets ()
  (dotimes (i 10)
    (let ((s (string-of-brackets i)))
      (format t "~3A: ~A~%" (balancedp s) s))))

```


Output:


```txt

CL-USER> (show-balanced-brackets)
T  :
NIL: ][
T  : [[]]
NIL: []]][[
T  : [][][][]
NIL: []][]][[[]
NIL: []]]]][][[[[
NIL: ][]]]]][[[[][[
T  : [[[[[[][[]]]]]]]
NIL: ]][[[[][]][[[[]]]]

```


## Component Pascal

BlackBox Component Builder

```oberon2

MODULE Brackets;
IMPORT StdLog, Args, Stacks (* See Task Stacks *);
TYPE

	Character = POINTER TO RECORD (Stacks.Object)
		c: CHAR
	END;

PROCEDURE NewCharacter(c: CHAR): Character;
VAR
	n: Character;
BEGIN
	NEW(n);n.c:= c;RETURN n
END NewCharacter;

PROCEDURE (c: Character) Show*;
BEGIN
	StdLog.String("Character(");StdLog.Char(c.c);StdLog.String(");");StdLog.Ln
END Show;

PROCEDURE CheckBalance(str: ARRAY OF CHAR): BOOLEAN;
VAR
	s: Stacks.Stack;
	n,x: ANYPTR;
	i: INTEGER;
	c : CHAR;
BEGIN
	i := 0; s := Stacks.NewStack();
	WHILE (i < LEN(str$)) & (~Args.IsBlank(str[i])) & (str[i] # 0X) DO
		IF s.Empty() THEN
			s.Push(NewCharacter(str[i]));
		ELSE
			n := s.top.data;
			WITH
				n :  Character DO
				IF (str[i] = ']')& (n.c = '[') THEN
					x := s.Pop();
				ELSE
					s.Push(NewCharacter(str[i]))
				END;
			ELSE RETURN FALSE;
			END;
		END;
		INC(i)
	END;
	RETURN s.Empty();
END CheckBalance;

PROCEDURE Do*;
VAR
	p : Args.Params;
	i: INTEGER;
BEGIN
	Args.Get(p); (* Get Params *)
	FOR i := 0 TO p.argc - 1 DO
		StdLog.String(p.args[i] + ":>");StdLog.Bool(CheckBalance(p.args[i]));StdLog.Ln
	END
END Do;

END Brackets.

```

Execute: ^Q Brackets.Do [] [][] [[][]] ][ ][][ []][[]~<br/>
Output:

```txt

[] :> $TRUE
[][] :> $TRUE
[[][]] :> $TRUE
][ :> $FALSE
][][ :> $FALSE
[]][[]:> $FALSE

```


## D


### Standard Version

D standard library has a [http://www.digitalmars.com/d/2.0/phobos/std_algorithm.html#balancedParens function] for this.

```d
import std.stdio, std.algorithm, std.random, std.range;

void main() {
    foreach (immutable i; 1 .. 9) {
        immutable s = iota(i * 2).map!(_ => "[]"[uniform(0, 2)]).array;
        writeln(s.balancedParens('[', ']') ? " OK: " : "bad: ", s);
    }
}
```

{{out}}

```txt
 OK: []
bad: []][
 OK: [][][]
bad: [][]]][[
 OK: [[[]][]][]
bad: ][][[[][][]]
bad: [[]][[]]]]][[[
bad: ][]]][[[[][][][]
```



### Imperative Version

{{trans|Perl 6}}

```d
import std.stdio, std.random, std.range, std.algorithm;

bool isBalanced(in string txt) pure nothrow {
    auto count = 0;

    foreach (immutable c; txt) {
        if (c == ']') {
            count--;
            if (count < 0)
                return false;
        } else if (c == '[')
            count++;
    }

    return count == 0;
}

void main() {
    foreach (immutable i; 1 .. 9) {
        immutable s = iota(i * 2).map!(_ => "[]"[uniform(0, 2)]).array;
        writeln(s.isBalanced ? " OK " : "Bad ", s);
    }
}
```

The output is similar.


### Functional Style

{{trans|Haskell}}

```d
import std.stdio, std.random, std.range, std.algorithm;

bool isBalanced(in string s, in char[2] pars="[]") pure nothrow @safe @nogc {
    bool bal(in string t, in int nb = 0) pure nothrow @safe @nogc {
        if (!nb && t.empty) return true;
        if (t.empty || nb < 0) return false;
        if (t[0] == pars[0]) return bal(t.dropOne, nb + 1);
        if (t[0] == pars[1]) return bal(t.dropOne, nb - 1);
        return bal(t.dropOne, nb); // Ignore char.
    }
    return bal(s);
}

void main() {
    foreach (immutable i; 1 .. 9) {
        immutable s = iota(i * 2).map!(_ => "[]"[uniform(0, $)]).array;
        writeln(s.isBalanced ? " OK " : "Bad ", s);
    }
}
```



## Delphi



```Delphi
procedure Balanced_Brackets;

var   BracketsStr : string;
      TmpStr      : string;
      I,J         : integer;

begin
  Randomize;
  for I := 1 to 9 do
    begin
      { Create a random string of 2*N chars with N*"[" and N*"]" }
      TmpStr  := '';
      for J := 1 to I do
        TmpStr := '['+TmpStr+']';
      BracketsStr := '';
      while TmpStr > '' do
        begin
          J := Random(Length(TmpStr))+1;
          BracketsStr := BracketsStr+TmpStr[J];
          Delete(TmpStr,J,1);
        end;
      TmpStr := BracketsStr;
      { Test for balanced brackets }
      while Pos('[]',TmpStr) > 0 do
        Delete(TmpStr,Pos('[]',TmpStr),2);
      if TmpStr = '' then
        writeln(BracketsStr+': OK')
      else
        writeln(BracketsStr+': not OK');
    end;
end;
```



```txt

[]: OK
[[]]: OK
[][][]: OK
[[[]][]]: OK
]]]][[[[[]: not OK
][[][][[[]]]: not OK
[][[]]][[][[]]: not OK
[[[[[]][]][[]]]]: OK
[]]][][[[[[]][]][]: not OK

```


=={{header|Déjà Vu}}==

```dejavu
matching?:
	swap 0
	for c in chars:
		if = c "]":
			++
		elseif = c "[":
			if not dup:
				drop
				return false
			--
	not

!. matching? ""
!. matching? "[]"
!. matching? "[][]"
!. matching? "[[][]]"
!. matching? "]["
!. matching? "][]["
!. matching? "[]][[]"
```

{{out}}

```txt
true
true
true
true
false
false
false
```



## EchoLisp


```scheme

(define (balance str)
	(for/fold (closed 0) ((par str))
	 #:break (< closed 0 ) => closed
	(+ closed
	(cond
		((string=? par "[")  1)
		((string=? par "]") -1)
		(else 0)))))

(define (task N)
(define str (list->string (append (make-list N "[") (make-list N "]"))))
	(for ((i 10))
	(set! str (list->string (shuffle (string->list str))))
	(writeln  (if (zero? (balance str)) '👍  '❌ ) str)))

(task 4)

❌     "[]]][[]["
❌     "]][][[[]"
❌     "][[[]]]["
👍     "[][[[]]]"
❌     "]][[][]["
❌     "][][[[]]"
👍     "[][][[]]"
❌     "]][[][[]"
❌     "[[]]][[]"
❌     "[[][]]]["


```



## Elena

ELENA 4.x :

```elena
import system'routines;
import extensions;
import extensions'text;

randomBrackets(len)
{
    if (0 == len)
    {
        ^emptyString
    }
    else
    {
        var brackets :=
            Array.allocate(len).populate:(i => $91)
            +
            Array.allocate(len).populate:(i => $93);

        brackets := brackets.randomize(len * 2);

        ^ brackets.summarize(new StringWriter()).toString()
    }
}

extension op
{
    get isBalanced()
    {
        var counter := new Integer(0);

        self.seekEach:(ch => counter.append((ch==$91).iif(1,-1)) < 0);

        ^ (0 == counter)
    }
}

public program()
{
    for(int len := 0, len < 9, len += 1)
    {
        var str := randomBrackets(len);

        console.printLine("""",str,"""",str.isBalanced ? " is balanced" : " is not balanced")
    };

    console.readChar()
}
```

{{out}}

```txt

"" is balanced
"[]" is balanced
"][[]" is not balanced
"[[[]]]" is balanced
"][[[]]][" is not balanced
"[]]]][[[][" is not balanced
"[[]][][[]]][" is not balanced
"[][]]]]][[[[][" is not balanced
"][]]][[[[][[][]]" is not balanced
"][]][][[]]][[[]][[" is not balanced

```



## Elixir

{{trans|Erlang}}
{{works with|Elixir|1.1}}

```elixir
defmodule Balanced_brackets do
  def task do
    Enum.each(0..5, fn n ->
      brackets = generate(n)
      result = is_balanced(brackets) |> task_balanced
      IO.puts "#{brackets} is #{result}"
    end)
  end

  defp generate( 0 ), do: []
  defp generate( n ) do
    for _ <- 1..2*n, do: Enum.random ["[", "]"]
  end

  def is_balanced( brackets ), do: is_balanced_loop( brackets, 0 )

  defp is_balanced_loop( _, n ) when n < 0, do: false
  defp is_balanced_loop( [], 0 ), do: true
  defp is_balanced_loop( [], _n ), do: false
  defp is_balanced_loop( ["[" | t], n ), do: is_balanced_loop( t, n + 1 )
  defp is_balanced_loop( ["]" | t], n ), do: is_balanced_loop( t, n - 1 )

  defp task_balanced( true ), do: "OK"
  defp task_balanced( false ), do: "NOT OK"
end

Balanced_brackets.task
```


{{out}}

```txt

 is OK
[[ is NOT OK
[][] is OK
]][]][ is NOT OK
[[][][]] is OK
[][[][[]]] is OK

```



## Erlang


```Erlang

-module( balanced_brackets ).
-export( [generate/1, is_balanced/1, task/0] ).

generate( N ) ->
	[generate_bracket(random:uniform()) || _X <- lists:seq(1, 2*N)].

is_balanced( String ) -> is_balanced_loop( String, 0 ).

task() ->
	lists:foreach( fun (N) ->
			String = generate( N ),
			Result = is_balanced( String ),
			io:fwrite( "~s is ~s~n", [String, task_balanced(Result)] )
		end,
		lists:seq(0, 5) ).



is_balanced_loop( _String, N ) when N < 0 -> false;
is_balanced_loop( [], 0 ) -> true;
is_balanced_loop( [], _N ) -> false;
is_balanced_loop( [$[ | T], N ) -> is_balanced_loop( T, N + 1 );
is_balanced_loop( [$] | T], N ) -> is_balanced_loop( T, N - 1 ).

generate_bracket( N ) when N =< 0.5 -> $[;
generate_bracket( N ) when N > 0.5 -> $].

task_balanced( true ) -> "OK";
task_balanced( false ) -> "NOT OK".

```

{{out}}

```txt

47> balanced_brackets:task().
 is OK
[[ is NOT OK
[][] is OK
[[[[][ is NOT OK
[]]]]][[ is NOT OK
[[[][[[[]] is NOT OK

```



## Euphoria


```euphoria
function check_brackets(sequence s)
    integer level
    level = 0
    for i = 1 to length(s) do
        if s[i] = '[' then
            level += 1
        elsif s[i] = ']' then
            level -= 1
            if level < 0 then
                return 0
            end if
        end if
    end for
    return level = 0
end function

function generate_brackets(integer n)
    integer opened,closed,r
    sequence s
    opened = n
    closed = n
    s = ""
    for i = 1 to n*2 do
        r = rand(opened+closed)
        if r<=opened then
            s &= '['
            opened -= 1
        else
            s &= ']'
            closed -= 1
        end if
    end for
    return s
end function

sequence s
for i = 1 to 10 do
    s = generate_brackets(3)
    puts(1,s)
    if check_brackets(s) then
        puts(1," OK\n")
    else
        puts(1," NOT OK\n")
    end if
end for
```


Sample output:
 ]]][[[ NOT OK
 [[[]]] OK
 [[]][] OK
 [][][] OK
 ]][[][ NOT OK
 [][[]] OK
 [[[]]] OK
 [[]][] OK
 []]][[ NOT OK
 [][[]] OK

=={{header|F_Sharp|F#}}==

```fsharp
let isBalanced str =
  let rec loop count = function
    | ']'::_  when count = 0 -> false
    | '['::xs                -> loop (count+1) xs
    | ']'::xs                -> loop (count-1) xs
    | []                     -> count = 0
    | _::_                   -> false

  str |> Seq.toList |> loop 0


let shuffle arr =
    let rnd = new System.Random()
    Array.sortBy (fun _ -> rnd.Next()) arr

let generate n =
  new string( String.replicate n "[]" |> Array.ofSeq |> shuffle )


for n in 1..10 do
  let s = generate n
  printfn "\"%s\" is balanced: %b" s (isBalanced s)
```


Output:

```txt
"[]" is balanced: true
"][][" is balanced: false
"][[]][" is balanced: false
"[][[]][]" is balanced: true
"[[]][[]][]" is balanced: true
"[[]][[[]][]]" is balanced: true
"][[[]][[[]][]]" is balanced: false
"][[][][]][[]][[]" is balanced: false
"][[]][][]][[]][[[]" is balanced: false
"][[]]][[][]][[]][[[]" is balanced: false
```



## Factor

This code implements the second part of the task: it reads from standard input an arbitrary string of opening and closing brackets, and checks whether it's balanced or not.

```Factor
USING: io formatting locals kernel math sequences unicode.case ;
IN: balanced-brackets

:: balanced ( str -- )
   0 :> counter!
   1 :> ok!
   str
   [ dup length 0 > ]
   [ 1 cut swap
     "[" = [ counter 1 + counter! ] [ counter 1 - counter! ] if
     counter 0 < [ 0 ok! ] when
   ]
   while
   drop
   ok 0 =
   [ "NO" ]
   [ counter 0 > [ "NO" ] [ "YES" ] if ]
   if
   print ;

readln
balanced
```


Some more idiomatic solution might be as follows:


```Factor
USING: io formatting locals kernel math sequences unicode.case ;
IN: balanced-brackets

: map-braces ( -- qout )
  [
    {
      { "[" [ drop  1 ] }
      { "]" [ drop -1 ] }
            [ drop  0 ]
    } case
  ]
;

: balanced? ( str -- ? )
  map-braces map sum 0 =
;

"[1+2*[3+4*[5+6]-3]*4-[3*[3+3]]]" balanced?
-- Data stack:
t

```



## Fantom



```fantom

class Main
{
  static Bool matchingBrackets (Str[] brackets)
  {
    Int opened := 0
    Int i := 0
    while (i < brackets.size)
    {
      if (brackets[i] == "[")
        opened += 1
      else
        opened -= 1
      if (opened < 0) return false
      i += 1
    }
    return true
  }

  public static Void main (Str[] args)
  {
    if (args.size == 1 && Int.fromStr(args[0], 10, false) != null)
    {
      n := Int.fromStr(args[0])
      Str[] brackets := [,]
      20.times
      {
        brackets = [,]
        // create a random set of brackets
        n.times { brackets.addAll (["[", "]"]) }
        n.times { brackets.swap(Int.random(0..<2*n), Int.random(0..<2*n)) }
        // report if matching or not
        if (matchingBrackets(brackets))
          echo (brackets.join(" ") + " Matching")
        else
          echo (brackets.join(" ") + " not matching")
      }
    }
  }
}

```


Output (for n=3):

```txt

[ ] [ ] [ ] Matching
[ [ [ ] ] ] Matching
] [ [ ] ] [ not matching
[ ] ] ] [ [ not matching
] ] [ ] [ [ not matching
[ ] ] [ [ ] not matching
[ ] ] [ [ ] not matching
[ ] [ ] ] [ not matching
[ [ ] ] [ ] Matching
[ [ [ ] ] ] Matching
[ ] ] [ [ ] not matching
] ] [ [ [ ] not matching
[ ] ] [ [ ] not matching
[ [ ] [ ] ] Matching
[ ] [ ] [ ] Matching
[ ] [ [ ] ] Matching
[ [ [ ] ] ] Matching
[ ] ] [ [ ] not matching
] ] [ ] [ [ not matching
[ ] ] [ [ ] not matching

```



## Forth

{{works with|4tH|3.61.1}}

```forth
include lib/choose.4th                 ( n1 -- n2)
include lib/ctos.4th                   ( n -- a 1)

 10 constant /[]                       \ maximum number of brackets
/[] string    []                       \ string with brackets
                                       \ create string with brackets
: make[]                               ( --)
  0 dup [] place /[] choose 0 ?do 2 choose 2* [char] [ + c>s [] +place loop
;                                      \ empty string, fill with brackets
                                       \ evaluate string
: eval[]                               ( --)
  [] count 2dup over chars + >r swap type 0
  begin                                \ setup string and count
    over r@ <                          \ reached end of string?
  while                                \ if not ..
    dup 0< 0=                          \ unbalanced ]?
  while                                \ if not ..
    over c@ [char] \ - negate + swap char+ swap
  repeat                               \ evaluate, goto next character
  r> drop if ."  NOT" then ."  OK" cr drop
;                                      \ evaluate string and print result

make[] eval[]
```

'''Examples''':
```txt
[][[]] OK
[[[[]][[ NOT OK
][[[]][] NOT OK
]][[[][ NOT OK
[]]]][][[ NOT OK
[]]][[]]] NOT OK
 OK
[[[[]]]] OK
[[]] OK
[[[[]]]] OK
[][[]] OK
[] OK
```



## Fortran

Please see the compilation and program execution result as comments at the top of this source:

```fortran

! $ gfortran -g -O0 -std=f2008 -Wall f.f08 -o f.exe
! $ ./f
!  compiles             syntax error
! :
! :                     ][
! :                     ]][[
! :[[[]]]
! :                     ][[][]][
! :                     ][[]]][[[]
! :                     ]]]][]][[[[[
! :                     ]]]][][]][[[[[
! :                     ][[[]]]]][]][[[[
! :                     [[][]]][]]][[[][[]
! :                     ]]][[][[[[[[[[]]]]]]
! :[[][[[][]]][]]
! :[[[][]][][[[]][]][]]

program balanced_brackets
  implicit none
  integer :: N
  character(len=20) :: brackets, fmt
  write(6,*)'compiles             syntax error'
  call random_seed
  do N=0, 10
     call generate(N, brackets)
     if (balanced(brackets)) then
        fmt = '(a,a20)'
     else
        fmt = '(a,21x,a20)'
     end if
     write(6,fmt)':',brackets
  end do

  brackets = '[[][[[][]]][]]'
  if (balanced(brackets)) then
     fmt = '(a,a20)'
  else
     fmt = '(a,21x,a20)'
  end if
  write(6,fmt)':',brackets

  N = 10
  call generate(N, brackets)
  do while (.not. balanced(brackets)) ! show a balanced set
     call generate(N, brackets)
  end do
  fmt = '(a,a20)'
  write(6,fmt)':',brackets

contains

  logical function balanced(s)
    implicit none
    character(len=*), intent(in) :: s
    integer :: i, a, n
    n = len_trim(s)
    a = 0
    balanced = .true.
    do i=1, n
       if (s(i:i) == '[') then
          a = a+1
       else
          a = a-1
       end if
       balanced = balanced .and. (0 <= a)
    end do
  end function balanced

  subroutine generate(N, s)
    implicit none
    integer, intent(in) :: N
    character(len=*), intent(out) :: s
    integer :: L, R, i
    real, dimension(2*N) :: harvest
    character :: c
    i = 1
    L = 0
    R = 0
    s = ' '
    call random_number(harvest)
    do while ((L < N) .and. (R < N))
       if (harvest(i) < 0.5) then
          L = L+1
          s(i:i) = '['
       else
          R = R+1
          s(i:i) = ']'
       end if
       i = i+1
    end do
    c = merge('[', ']', L < N)
    do while (i <= 2*N)
       s(i:i) = c
       i = i+1
    end do
  end subroutine generate
end program balanced_brackets

```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Function isBalanced(s As String) As Boolean
  If s = "" Then Return True
  Dim countLeft As Integer = 0 '' counts number of left brackets so far unmatched
  Dim c As String
  For i As Integer = 1 To Len(s)
    c = Mid(s, i, 1)
    If  c = "[" Then
      countLeft += 1
    ElseIf countLeft > 0 Then
      countLeft -= 1
    Else
      Return False
    End If
  Next
  Return countLeft = 0
End Function

' checking examples in task description
Dim brackets(1 To 7) As String = {"", "[]", "][", "[][]", "][][", "[[][]]", "[]][[]"}
For i As Integer = 1 To 7
  Print IIf(brackets(i) <> "", brackets(i), "(empty)"); Tab(10); IIf(isBalanced(brackets(i)), "OK", "NOT OK")
Next

' checking 7 random strings of brackets of length 8 say
Randomize
Dim r As Integer '' 0 will signify "[" and 1 will signify "]"
Dim s As String
For i As Integer = 1 To 7
  s = Space(8)
  For j As Integer = 1 To 8
    r = Int(Rnd * 2)
    If r = 0 Then
      Mid(s, j) = "["
    Else
      Mid(s, j) = "]"
    End If
  Next j
  Print s; Tab(10); IIf(isBalanced(s), "OK", "NOT OK")
Next i

Print
Print "Press any key to quit"
Sleep
```

Sample output (last 7 lines random) :
{{out}}

```txt

(empty)  OK
[]       OK
][       NOT OK
[][]     OK
][][     NOT OK
[[][]]   OK
[]][[]   NOT OK
][]][[[[ NOT OK
[]][][]] NOT OK
][][[[]] NOT OK
[[[[]]]] OK
[][[[[][ NOT OK
][[[]][] NOT OK
[][[[[][ NOT OK

```



## Gambas

'''[https://gambas-playground.proko.eu/?gist=8960fb267af43f0549d2cfe04288a2d4 Click this link to run this code]'''

```gambas
'Altered to prevent lines starting with ']' or ending with '[' being generated as they can't work

siNumberOfBrackets As Short = 20              'Maximum amount of brackets in a line
siNumberOfLines As Short = 20                 'Amount of lines to test

'----

Public Sub Main()
Dim sBrks As String[] = GenerateBrackets()    'Get random array to check
Dim sTemp, sHold, sWork As String             'Working variables
Dim siCount As Short                          'Counter

For Each sTemp In sBrks                       'For each line in the sBrk array (e.g. '[][][][[[[]][]]]')
  sWork = sTemp                               'Make sWork = sTemp
  Repeat                                      'Repeat
    sHold = sWork                             'Make sHold = sWork
    sWork = Replace(sWork, "[]", "")          'Remove all brackets that match '[]'
  Until sHold = sWork                         'If sHold = sWork then there are no more '[]' matches

  If sWork = "" Then                          'So if all the brackets 'Nested' correctly sWork will be empty
      Print "    OK ";                        'Print 'OK'
  Else                                        'Else they did not all match
    Print "NOT OK ";                          'So print 'NOT OK'
  Endif

  For siCount = 1 To Len(sTemp)               'Loop through the line of brackets
    Print Mid(sTemp, siCount, 1) & " ";       'Print each bracket + a space to make it easier to read
  Next
  Print                                       'Print a new line
Next

End

'----

Public Sub GenerateBrackets() As String[]     'Generates an array of random quantities of '[' and ']'
Dim siQty As New Short[]                      'To store the random number (of brackets) to put in a line
Dim sBrk As New String[]                      'To store the lines of brackets
Dim siNum, siEnd, siLoop As Short             'Various counters
Dim sTemp As String                           'Temp string

Repeat                                        'Repeat
  siNum = Rand(0, siNumberOfBrackets)         'Pick a number between 0 and the total number of brackets requested
  If Even(siNum) Then siQty.Add(siNum)        'If the number is even then add the number to siQty
Until siQty.Count = siNumberOfLines           'Keep going until we have the number of lines requested

For Each siNum In siQty                       'For each number in siQty..(e.g. 6)
  Do
    siEnd = Rand(0, 1)                        'Generate a 0 or a 1
    If siEnd = 0 Then sTemp &= "["            'If '0' then add a '[' bracket
    If siEnd = 1 Then sTemp &= "]"            'If '1' then add a ']' bracket

    If siNum = 0 Then                         'If siNum = 0 then..
      sBrk.Add("")                            'Add '0' to the array
        sTemp = ""                            'Clear sTemp
        Break                                 'Exit the Do Loop
    Endif

    If Len(sTemp) = siNum Then                'If the length of sTemp = the required amount then..
      If sTemp Not Begins "]" And sTemp Not Ends "[" Then  'Check to see that sTemp does not start with "]" and does not end with a "["
        sBrk.Add(sTemp)                       'Add it to the array
        sTemp = ""                            'Clear sTemp
        Break                                 'Exit the Do Loop
      Else                                    'Else
        sTemp = ""                            'Clear sTemp
      End If                                  'Try again!
    Endif
  Loop
Next

Return sBrk                                   'Return the sBrk array

End
```

Output:

```txt

NOT OK [ ] ] [ [ ] [ [ ] [ [ [ ] [ [ ] [ ]
NOT OK [ [ [ ] [ [ ] [ [ ] ] [ ] ]
NOT OK [ ] [ [ [ ] ] [ [ ] ] ] [ ] ] [ ] ] ] ]
NOT OK [ [ [ ] [ ] ] ] ] ] [ [ ] ] [ ] [ [ ] ]
NOT OK [ [ ] ] ] ]
NOT OK [ ] ] [ [ ] [ ] [ ]
NOT OK [ [ [ ] [ ]
    OK [ ]
NOT OK [ ] ] ] [ ]
NOT OK [ [ ] ] ] ]
    OK [ [ [ [ ] ] [ ] [ ] ] ]
    OK [ ]
    OK [ ] [ ]
NOT OK [ ] ] ] [ ] [ [ [ [ ] [ [ ] [ ]
NOT OK [ ] ] ] [ [ ] ] ] [ ] ] ] ]
NOT OK [ ] ] ] ] ] [ [ ] [ ] ] ] [ ] [ [ [ [ ]

```



## GAP


```gap
Balanced := function(L)
    local c, r;
    r := 0;
    for c in L do
        if c = ']' then
            r := r - 1;
            if r < 0 then
                return false;
            fi;
        elif c = '[' then
            r := r + 1;
        fi;
    od;
    return r = 0;
end;

Balanced("");
# true

Balanced("[");
# false

Balanced("]");
# false

Balanced("[]");
# true

Balanced("][");
# false

Balanced("[[][]]");
# true

Balanced("[[[]][]]]");
# false
```



## Go


```go
package main

import (
    "bytes"
    "fmt"
    "math/rand"
    "time"
)

func init() {
    rand.Seed(time.Now().UnixNano())
}

func generate(n uint) string {
    a := bytes.Repeat([]byte("[]"), int(n))
    for i := len(a) - 1; i >= 1; i-- {
        j := rand.Intn(i + 1)
        a[i], a[j] = a[j], a[i]
    }
    return string(a)
}

func testBalanced(s string) {
    fmt.Print(s + ": ")
    open := 0
    for _,c := range s {
        switch c {
        case '[':
            open++
        case ']':
            if open == 0 {
                fmt.Println("not ok")
                return
            }
            open--
        default:
            fmt.Println("not ok")
            return
        }
    }
    if open == 0 {
        fmt.Println("ok")
    } else {
        fmt.Println("not ok")
    }
}

func main() {
    rand.Seed(time.Now().UnixNano())
    for i := uint(0); i < 10; i++ {
        testBalanced(generate(i))
    }
    testBalanced("()")
}
```

Output:

```txt

: ok
][: not ok
]][[: not ok
[[][]]: ok
[][[][]]: ok
[[[][][]]]: ok
]][[]]][[[[]: not ok
]][[[]][[][[]]: not ok
[[[[[]]][]]][]][: not ok
[][[][[]][][[][]]]: ok
(): not ok

```



## Groovy

Generate Arbitrary String of Bracket Pairs:

```groovy
def random = new Random()

def factorial = { (it > 1) ? (2..it).inject(1) { i, j -> i*j } : 1 }

def makePermutation;
makePermutation = { string, i ->
    def n = string.size()
    if (n < 2) return string
    def fact = factorial(n-1)
    assert i < fact*n

    def index = i.intdiv(fact)
    string[index] + makePermutation(string[0..<index] + string[(index+1)..<n], i % fact)
}

def randomBrackets = { n ->
    if (n == 0) return ''
    def base = '['*n + ']'*n
    def p = random.nextInt(factorial(n*2))
    makePermutation(base, p)
}
```


Check Balance of Bracket String:

```groovy
boolean balancedBrackets(String brackets, int depth=0) {
    if (brackets == null || brackets.empty) return depth == 0
    switch (brackets[0]) {
        case '[':
            return brackets.size() > 1  &&  balancedBrackets(brackets[1..-1], depth + 1)
        case ']':
            return depth > 0  &&  (brackets.size() == 1  ||  balancedBrackets(brackets[1..-1], depth - 1))
        default:
            return brackets.size() == 1  ?  depth == 0  :  balancedBrackets(brackets[1..-1], depth)
    }
}
```


Test:

```groovy
Set brackets = []
(0..100).each {
    (0..8).each { r ->
        brackets << randomBrackets(r)
    }
}

brackets.sort { a, b ->
    a.size() <=> b.size() ?: a <=> b
} .each {
    def bal = balancedBrackets(it) ? "balanced:   " : "unbalanced: "
    println "${bal} ${it}"
}
```


Output:
<pre style="height:30ex;overflow:scroll;">balanced:
balanced:    []
unbalanced:  ][
balanced:    [[]]
balanced:    [][]
unbalanced:  []][
unbalanced:  ][[]
unbalanced:  ][][
unbalanced:  ]][[
balanced:    [[[]]]
balanced:    [[][]]
balanced:    [[]][]
unbalanced:  [[]]][
balanced:    [][[]]
balanced:    [][][]
unbalanced:  [][]][
unbalanced:  []][[]
unbalanced:  []][][
unbalanced:  []]][[
unbalanced:  ][[[]]
unbalanced:  ][[][]
unbalanced:  ][[]][
unbalanced:  ][][[]
unbalanced:  ][][][
unbalanced:  ][]][[
unbalanced:  ]][[[]
unbalanced:  ]][[][
unbalanced:  ]][][[
unbalanced:  ]]][[[
balanced:    [[[[]]]]
balanced:    [[[][]]]
balanced:    [[[]][]]
balanced:    [[[]]][]
unbalanced:  [[[]]]][
balanced:    [[][[]]]
balanced:    [[][]][]
unbalanced:  [[][]]][
balanced:    [[]][[]]
balanced:    [[]][][]
unbalanced:  [[]][]][
unbalanced:  [[]]][[]
unbalanced:  [[]]]][[
balanced:    [][[][]]
balanced:    [][[]][]
unbalanced:  [][]][[]
unbalanced:  [][]][][
unbalanced:  [][]]][[
unbalanced:  []][[][]
unbalanced:  []][[]][
unbalanced:  []][][[]
unbalanced:  []][][][
unbalanced:  []][]][[
unbalanced:  []]][[[]
unbalanced:  []]][[][
unbalanced:  []]]][[[
unbalanced:  ][[[[]]]
unbalanced:  ][[[]][]
unbalanced:  ][[[]]][
unbalanced:  ][[][][]
unbalanced:  ][[][]][
unbalanced:  ][][[[]]
unbalanced:  ][][[][]
unbalanced:  ][][[]][
unbalanced:  ][][][[]
unbalanced:  ][][][][
unbalanced:  ][][]][[
unbalanced:  ][]][[[]
unbalanced:  ][]][[][

... <SOME OF THE SAME CUT>

balanced:    [[]][[][][[]]]
unbalanced:  [[]][[][]][]][
balanced:    [[]][[]][][[]]
balanced:    [[]][][[][[]]]
balanced:    [[]][][][[][]]
unbalanced:  [[]][][][]]][[
unbalanced:  [[]][][]][[]][
unbalanced:  [[]][]][[[][]]
unbalanced:  [[]][]][[][[]]
unbalanced:  [[]][]][[][][]
unbalanced:  [[]][]][][[][]
unbalanced:  [[]][]][][]][[
unbalanced:  [[]][]]][[][][
unbalanced:  [[]][]]][][[][
unbalanced:  [[]]][[[]][][]
unbalanced:  [[]]][[][[][]]
unbalanced:  [[]]][[][[]]][
unbalanced:  [[]]][][[[[]]]
unbalanced:  [[]]][][[]]][[
unbalanced:  [[]]][][]][[[]
unbalanced:  [[]]][]][[[]][
unbalanced:  [[]]][]][[][][
unbalanced:  [[]]]][[][[]][
unbalanced:  [[]]]][][[[]][
unbalanced:  [[]]]][][[][[]
unbalanced:  [[]]]][][]][[[
unbalanced:  [[]]]]][[[[]][
unbalanced:  [[]]]]][][[[[]
unbalanced:  [[]]]]]][][[[[
unbalanced:  [[]]]]]]][[[[[
balanced:    [[[[[[[]]]]][]]]
unbalanced:  [[[[[[[]]]]]]]][
balanced:    [[[[[[][]]]][]]]
unbalanced:  [[[[[[][]]]]]]][
balanced:    [[[[[[]]][][]]]]
balanced:    [[[[[[]]]][[]]]]
balanced:    [[[[[[]]]][]][]]
balanced:    [[[[[[]]]][]]][]
balanced:    [[[[[[]]]]][][]]
unbalanced:  [[[[[[]]]]][]]][
balanced:    [[[[[[]]]]]][][]
unbalanced:  [[[[[[]]]]]][]][
balanced:    [[[[[][][[]]]]]]
balanced:    [[[[[][][]][]]]]
balanced:    [[[[[][]][[]]]]]
balanced:    [[[[[][]][]][]]]
balanced:    [[[[[][]]]]][][]
unbalanced:  [[[[[][]]]]]]][[
balanced:    [[[[[]][[][]]]]]
balanced:    [[[[[]][[]]]]][]
balanced:    [[[[[]][][[]]]]]
balanced:    [[[[[]][]][[]]]]
balanced:    [[[[[]][]]]][[]]
balanced:    [[[[[]]][[[]]]]]
balanced:    [[[[[]]][[][]]]]
unbalanced:  [[[[[]]][[]]]]][
balanced:    [[[[[]]][][][]]]
balanced:    [[[[[]]][]][[]]]
balanced:    [[[[[]]][]][]][]
unbalanced:  [[[[[]]][]][]]][
balanced:    [[[[[]]][]]][[]]
unbalanced:  [[[[[]]]][]][]][
balanced:    [[[[[]]]]][[]][]
balanced:    [[[[[]]]]][][[]]
balanced:    [[[[[]]]]][][][]
unbalanced:  [[[[[]]]]][]][[]
unbalanced:  [[[[[]]]]]][[]][
unbalanced:  [[[[[]]]]]][][[]
balanced:    [[[[][[[[]]]]]]]
balanced:    [[[[][[[][]]]]]]
balanced:    [[[[][[]][[]]]]]
balanced:    [[[[][[]][][]]]]
balanced:    [[[[][[]]]]][][]
balanced:    [[[[][][][][]]]]
balanced:    [[[[][][][]]][]]
balanced:    [[[[][][]][][]]]
balanced:    [[[[][][]]][][]]
unbalanced:  [[[[][][]]][]]][
balanced:    [[[[][][]]]][][]
balanced:    [[[[][]][[[]]]]]
balanced:    [[[[][]][[]]]][]
balanced:    [[[[][]][][]][]]
unbalanced:  [[[[][]]][]]]][[
balanced:    [[[[][]]]][[[]]]
balanced:    [[[[][]]]][][[]]
unbalanced:  [[[[][]]]]][][][
unbalanced:  [[[[][]]]]]]][[[
balanced:    [[[[]][[][]][]]]
balanced:    [[[[]][[]]]][[]]
balanced:    [[[[]][][[][]]]]
unbalanced:  [[[[]][][[]]]]][
balanced:    [[[[]][][][]][]]
balanced:    [[[[]][][]][[]]]
balanced:    [[[[]][][]][][]]
balanced:    [[[[]][][]][]][]
balanced:    [[[[]][][]]][[]]
unbalanced:  [[[[]][]][]][]][
balanced:    [[[[]][]]][[]][]
unbalanced:  [[[[]][]]]][[][]
unbalanced:  [[[[]][]]]][[]][
unbalanced:  [[[[]][]]]]][[[]
balanced:    [[[[]]][[]]][[]]
balanced:    [[[[]]][[]]][][]
balanced:    [[[[]]][][][[]]]
balanced:    [[[[]]][]][[]][]
unbalanced:  [[[[]]][]][[]]][
unbalanced:  [[[[]]][]]][[]][
unbalanced:  [[[[]]][]]][]][[
unbalanced:  [[[[]]][]]]][[[]
balanced:    [[[[]]]][[[[]]]]
balanced:    [[[[]]]][[[][]]]
balanced:    [[[[]]]][[]][][]
unbalanced:  [[[[]]]][[]][]][
unbalanced:  [[[[]]]][][]]][[
unbalanced:  [[[[]]]][]][[[]]
unbalanced:  [[[[]]]][]][[]][
unbalanced:  [[[[]]]][]]][][[
unbalanced:  [[[[]]]][]]]][[[
unbalanced:  [[[[]]]]][][][[]
unbalanced:  [[[[]]]]][][]][[
unbalanced:  [[[[]]]]]][[]][[
unbalanced:  [[[[]]]]]][][][[
```



## Haskell


```haskell

import Control.Monad
import System.Random
import Text.Printf
import VShuffle

-- Return whether a string contains balanced brackets.  Nothing indicates a
-- balanced string, while (Just i) means an imbalance was found at, or just
-- after, the i'th bracket.  We assume the string contains only brackets.
isBalanced :: String -> Maybe Int
isBalanced = bal (-1) 0
    where bal :: Int -> Int -> String -> Maybe Int
          bal _   0      []  = Nothing
          bal i   _      []  = Just i
          bal i (-1)      _  = Just i
          bal i   n ('[':bs) = bal (i+1) (n+1) bs
          bal i   n (']':bs) = bal (i+1) (n-1) bs

-- Print a string, indicating whether it contains balanced brackets.  If not,
-- indicate the bracket at which the imbalance was found.
check :: String -> IO ()
check s = maybe (good s) (bad s) (isBalanced s)
    where good s   = printf "Good \"%s\"\n" s
          bad  s n = printf "Bad  \"%s\"\n%*s^\n" s (n+6) " "

main :: IO ()
main = do
  let bs = cycle "[]"
  rs <- replicateM 10 newStdGen
  zipWithM_ (\n r -> check $ shuffle (take n bs) r) [0,2..] rs

```

We put our list shuffling function in a separate module.  For efficiency we use ''mutable'' vectors, although for the short lists in our example it doesn't really matter.

```haskell

module VShuffle (shuffle) where

import Data.List (mapAccumL)
import System.Random
import Control.Monad.ST
import qualified Data.Vector as V
import qualified Data.Vector.Generic.Mutable as M

-- Generate a list of array index pairs, each corresponding to a swap.
pairs :: (Enum a, Random a, RandomGen g) => a -> a -> g -> [(a, a)]
pairs l u r = snd $ mapAccumL step r [l..pred u]
    where step r i = let (j, r') = randomR (i, u) r in (r', (i, j))

-- Return a random permutation of the list.  We use the algorithm described in
-- http://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle#The_modern_algorithm.
shuffle :: (RandomGen g) => [a] -> g -> [a]
shuffle xs r = V.toList . runST $ do
                 v <- V.unsafeThaw $ V.fromList xs
                 mapM_ (uncurry $ M.swap v) $ pairs 0 (M.length v - 1) r
                 V.unsafeFreeze v

```

Here's some sample output.

```txt

Good ""
Bad  "]["
      ^
Good "[[]]"
Bad  "[]][]["
        ^
Bad  "[]]][][["
        ^
Bad  "][][[[]][]"
      ^
Bad  "[[][][]]][]["
              ^
Bad  "][]][[[][][[]]"
      ^
Good "[[][[][[]]][[]]]"
Bad  "]]][[[][][][[][]]["
      ^

```


=={{header|Icon}} and {{header|Unicon}}==

```Icon
procedure main(arglist)
every s := genbs(!arglist) do
   write(image(s), if isbalanced(s) then " is balanced." else " is unbalanced")
end

procedure isbalanced(s) # test if a string is balanced re: []
return (s || " ") ? (bal(,'[',']') = *s+1)
end

procedure genbs(i)  # generate strings of i pairs of []
s := ""
every 1 to i do s ||:= "[]"       # generate i pairs
every !s := ?s                    # shuffle
return s
end
```


Output:
```txt

> isbal.exe 2 3 3 3 3 3 3 3 4 4 4

"[[]]" is balanced.
"]]]]]]" is unbalanced
"]]]]]]" is unbalanced
"[][][]" is balanced.
"]][[[]" is unbalanced
"[[[][[" is unbalanced
"]]]]]]" is unbalanced
"[]]]]]" is unbalanced
"]][]][]]" is unbalanced
"[[[[[][[" is unbalanced
"[[[[[][]" is unbalanced
```



## J

'''Solution''':
```j
bracketDepth    =:  '[]' -&(+/\)/@:(=/) ]
checkBalanced   =:  _1 -.@e. bracketDepth
genBracketPairs =:  (?~@# { ])@#"0 1&'[]'          NB. bracket pairs in arbitrary order
```

'''Examples''':
```j
   (, ' ' ,  ('bad';'OK') {::~ checkBalanced)"1 genBracketPairs i. 10
                   OK
][                 bad
][[]               bad
[[[]]]             OK
[][[]][]           OK
[][[[][]]]         OK
[]][]][]][[[       bad
[[]][[][][]][]     OK
]]]][[][][[[[]][   bad
[]]][][][[[[]][[]] bad
```

'''Comments''':  This task highlights the versatility and usefulness of J's scanning modifiers, <code>/</code> and <code>\</code>.

The <code>checkBalanced</code> verb would need modification (<code> checkBalanced =: ((0 = {:) *. _1 -.@e. ])@bracketDepth </code>) if the task were extended to include uneven numbers of opening and closing brackets.


## Java

{{works with|Java|1.5+}}

```java5
public class BalancedBrackets {

    public static boolean hasBalancedBrackets(String str) {
        int brackets = 0;
        for (char ch : str.toCharArray()) {
            if (ch == '[') {
                brackets++;
            } else if (ch == ']') {
                brackets--;
            } else {
                return false;   // non-bracket chars
            }
            if (brackets < 0) {   // closing bracket before opening bracket
                return false;
            }
        }
        return brackets == 0;
    }

    public static String generateBalancedBrackets(int n) {
        assert n % 2 == 0;   // if n is odd we can't match brackets
        char[] ans = new char[n];
        int openBracketsLeft = n / 2;
        int unclosed = 0;
        for (int i = 0; i < n; i++) {
            if (Math.random() >= 0.5 && openBracketsLeft > 0 || unclosed == 0) {
                ans[i] = '[';
                openBracketsLeft--;
                unclosed++;
            } else {
                ans[i] = ']';
                unclosed--;
            }
        }
        return String.valueOf(ans);
    }

    public static void main(String[] args) {
        for (int i = 0; i <= 16; i += 2) {
            String brackets = generateBalancedBrackets(i);
            System.out.println(brackets + ": " + hasBalancedBrackets(brackets));
        }

        String[] tests = {"", "[]", "][", "[][]", "][][", "[[][]]", "[]][[]"};
        for (String test : tests) {
            System.out.println(test + ": " + hasBalancedBrackets(test));
        }
    }
}
```

Sample output (generate uses random numbers, so it should not be the same every time):

```txt
: true
[]: true
[[]]: true
[[]][]: true
[][][][]: true
[][[][[]]]: true
[[][[][][]]]: true
[[][][]][[[]]]: true
[][[[][[][[]]]]]: true
: true
[]: true
][: false
[][]: true
][][: false
[[][]]: true
[]][[]: false
```



## JavaScript



### ES5


### =Iterative=



```JavaScript
function shuffle(str) {
  var a = str.split(''), b, c = a.length, d
  while (c) b = Math.random() * c-- | 0, d = a[c], a[c] = a[b], a[b] = d
  return a.join('')
}

function isBalanced(str) {
  var a = str, b
  do { b = a, a = a.replace(/\[\]/g, '') } while (a != b)
  return !a
}

var M = 20
while (M-- > 0) {
  var N = Math.random() * 10 | 0, bs = shuffle('['.repeat(N) + ']'.repeat(N))
  console.log('"' + bs + '" is ' + (isBalanced(bs) ? '' : 'un') + 'balanced')
}
```


Sample output:


```txt
"[]" is balanced
"]][[]][[[]" is unbalanced
"]][[[][][][]][" is unbalanced
"[][[[[][][[[]]]]]]" is balanced
"][" is unbalanced
"[[[]]]][[]" is unbalanced
"][[]" is unbalanced
"][[][]][[[]]" is unbalanced
"[[][]]][" is unbalanced
"[[[]]][[]]]][][[" is unbalanced
"[]][[]]][[[[][]]" is unbalanced
"[]" is balanced
"][]][[][" is unbalanced
"[[]][[][]]" is balanced
"[[]]" is balanced
"]][]][[]][[[" is unbalanced
"][]][][[" is unbalanced
"[[]]" is balanced
"][][" is unbalanced
"[[]]][][][[]][" is unbalanced
```



### ES6


### =Functional=

With visual indication of where the balance fails:

```JavaScript
(() => {
    'use strict';

    // Int -> String
    let randomBrackets = n => range(1, n)
        .map(() => Math.random() < 0.5 ? '[' : ']')
        .join('');

    // imbalance :: String -> Integer
    let imbalance = strBrackets => {

        // iDepth: initial nesting depth (0 = closed)
        // iIndex: starting character position

        // errorIndex :: [Char] -> Int -> Int -> Int
        let errorIndex = (xs, iDepth, iIndex) => {
            if (xs.length > 0) {
                let tail = xs.slice(1),
                    iNext = iDepth + (xs[0] === '[' ? 1 : -1);

                if (iNext < 0) return iIndex; // unmatched closing bracket
                else return tail.length ? errorIndex(
                        tail, iNext, iIndex + 1
                    ) : iNext === 0 ? -1 : iIndex; // balanced ? problem index ?

            } else return iDepth === 0 ? -1 : iIndex;
        };

        return errorIndex(strBrackets.split(''), 0, 0);
    };


    // GENERIC FUNCTION

    // range :: Int -> Int -> [Int]
    let range = (m, n) =>
        Array.from({
            length: Math.floor(n - m) + 1
        }, (_, i) => m + i);

    // TESTING AND FORMATTING OUTPUT

    let lngPairs = 6,
        strPad = Array(lngPairs * 2 + 4)
        .join(' ');

    return range(0, lngPairs)
        .map(n => {
            let w = n * 2,
                s = randomBrackets(w),
                i = imbalance(s),
                blnOK = i === -1;

            return "'" + s + "'" + strPad.slice(w + 2) +
                (blnOK ? 'OK' : 'problem') +
                (blnOK ? '' : '\n' + Array(i + 2)
                    .join(' ') + '^');
        })
        .join('\n');
})();

```


{{Out}}

```txt
''             OK
'[]'           OK
']]]['         problem
 ^
'[]]][['       problem
   ^
'[[]]]]]['     problem
     ^
'[][[][]][]'   OK
']][[[][[[]]]' problem
 ^
```



## Julia


```julia
using Printf

function balancedbrackets(str::AbstractString)
    i = 0
    for c in str
        if c == '[' i += 1 elseif c == ']' i -=1 end
        if i < 0 return false end
    end
    return i == 0
end

brackets(n::Integer) = join(shuffle(collect(Char, "[]" ^ n)))

for (test, pass) in map(x -> (x, balancedbrackets(x)), collect(brackets(i) for i = 0:8))
    @printf("%22s%10s\n", test, pass ? "pass" : "fail")
end
```


{{out}}

```txt
                            pass
                    ][      fail
                  [][]      pass
                ][[]][      fail
              [[[]]][]      pass
            ]]]][[[][[      fail
          ]]][[][][[[]      fail
        ]][]][][[[][][      fail
      []][]]]][[[][[[]      fail
```


'''One-line version''':

```julia
balancedbrackets(str::AbstractString) = foldl((x, y) -> x < 0 ? -1 : x + y, 0, collect((x == '[') - (x == ']') for x in str)) == 0
```



## K


```K

  gen_brackets:{"[]"@x _draw 2}
  check:{r:(-1;1)@"["=x; *(0=+/cs<'0)&(0=-1#cs:+\r)}

  {(x;check x)}' gen_brackets' 2*1+!10
(("[[";0)
 ("[][]";1)
 ("][][]]";0)
 ("[[][[][]";0)
 ("][]][[[[[[";0)
 ("]]][[]][]]][";0)
 ("[[[]][[[][[[][";0)
 ("[[]][[[]][]][][]";1)
 ("][[][[]]][[]]]][][";0)
 ("]][[[[]]]][][][[]]]]";0))

```



## Kotlin

{{trans|FreeBASIC}}

```scala
import java.util.Random

fun isBalanced(s: String): Boolean {
    if (s.isEmpty()) return true
    var countLeft = 0  // number of left brackets so far unmatched
    for (c in s) {
        if (c == '[') countLeft++
        else if (countLeft > 0) countLeft--
        else return false
    }
    return countLeft == 0
}

fun main(args: Array<String>) {
    println("Checking examples in task description:")
    val brackets = arrayOf("", "[]", "][", "[][]", "][][", "[[][]]", "[]][[]")
    for (b in brackets) {
        print(if (b != "") b else "(empty)")
        println("\t  " + if (isBalanced(b)) "OK" else "NOT OK")
    }
    println()

    println("Checking 7 random strings of brackets of length 8:")
    val r = Random()
    (1..7).forEach {
        var s = ""
        for (j in 1..8) {
            s += if (r.nextInt(2) == 0) '[' else ']'
        }
        println("$s  " + if (isBalanced(s)) "OK" else "NOT OK")
    }
}
```

Sample output (last 7 lines random) :
{{out}}

```txt

Checking examples in task description:
(empty)	  OK
[]	  OK
][	  NOT OK
[][]	  OK
][][	  NOT OK
[[][]]	  OK
[]][[]	  NOT OK

Checking 7 random strings of brackets of length 8:
[[[[]]][  NOT OK
[][[][]]  OK
[[[[[]]]  NOT OK
[[[[[]]]  NOT OK
[[[]]][]  OK
]]]][[][  NOT OK
]][]][][  NOT OK

```



## L++


```lisp
(include "string")

(defn bool balanced (std::string s)
  (def bal 0)
  (foreach c s
    (if (== c #\[) (++ bal)
      (if (== c #\]) (-- bal)))
    (if (< bal 0) (return false)))
  (return (== bal 0)))

(main
  (decl std::string (at tests) |{"", "[]", "[][]", "[[][]]", "][", "][][", "[]][[]"}|)
  (pr std::boolalpha)
  (foreach x tests
    (prn x "\t" (balanced x))))
```



## Lasso


```Lasso
define randomparens(num::integer,open::string='[',close::string=']') => {
    local(out) = array

    with i in 1 to #num do {
        #out->insert(']', integer_random(1,#out->size || 1))
        #out->insert('[', integer_random(1,#out->size || 1))
    }
    return #out->join
}

define validateparens(input::string,open::string='[',close::string=']') => {
    local(i) = 0
    #input->foreachcharacter => {
        #1 == #open ? #i++
        #1 == #close && --#i < 0 ? return false
    }
    return #i == 0 ? true | false
}

with i in 1 to 10
let input = randomparens(#i)
select #input + ' = ' + validateparens(#input)
```


{{out}}

```txt
[] = true
][[] = false
]][[[] = false
][][[][] = false
[[[]][[]]] = true
]]]][[[[][[] = false
[[[[[[]]]]]][] = true
[[]][][]][]][[[] = false
[[[]][[[]][]]][[]] = true
```



## Liberty BASIC


```lb

print "Supplied examples"
for i =1 to 7
    read test$
    print "The string '"; test$; "' is "; validString$( test$)
next i
print
data "", "[]", "][","[][]","][][","[[][]]","[]][[]"

print "Random generated examples"
for example =1 to 10
    test$ =generate$( int( 1 +10 *rnd(1)))
    print "The string '"; test$; "' is "; validString$( test$)
next example
end

function validString$( in$)
    if left$( in$, 1) <>"[" and len( test$) <>0 then
        validString$ ="not OK. Opens wrongly."
        exit function
    end if
    paired =0
    for i =1 to len( in$)
        c$ =mid$( in$, i, 1)
        if c$ ="[" then paired =paired +1
        if c$ ="]" then paired =paired -1
        if paired <0 then
            exit for
        end if
    next i
    if ( lBr =rBr) and ( paired >=0) then validString$ ="OK." else validString$ ="not OK. Unbalanced."
end function

function generate$( N)
    lBr =0
    rBr =0
    '   choose at random until N of one type generated
    while ( lBr <N) and ( rBr <N)
        select case int( 1.5 +rnd( 1))
            case 1
                lBr =lBr +1
                generate$ =generate$ +"["
            case 2
                rBr =rBr +1
                generate$ =generate$ +"]"
        end select
    wend
    '   now pad with the remaining other brackets
    if lBr =N then
        generate$ =generate$ +string$( N -rBr, "]")
    else
        generate$ =generate$ +string$( N -lBr, "[")
    end if
end function

function string$( n, c$)
    for i =1 to n
        op$ =op$ +c$
    next i
    string$ =op$
end function

end

```

 Supplied examples
 The string '' is OK.
 The string '[]' is OK.
 The string '][' is not OK. Unbalanced.
 The string '[][]' is OK.
 The string '][][' is not OK. Unbalanced.
 The string '[[][]]' is OK.
 The string '[]][[]' is not OK. Unbalanced.

 Random generated examples
 The string '[[][[[][]]]]' is OK.
 The string ']]]][[[[' is not OK. Unbalanced.
 The string '[[]][]' is OK.
 The string '[][[][[]][]]' is OK.
 The string '][[[]]][][' is not OK. Unbalanced.
 The string ']]]]][[[[[' is not OK. Unbalanced.
 The string '[[[]]]' is OK.
 The string ']][][[' is not OK. Unbalanced.
 The string '[[]]][][[][]' is not OK. Unbalanced.
 The string '][[][[][][]]][[]' is not OK. Unbalanced.



## Lua


```Lua

function isBalanced(s)
  --Lua pattern matching has a 'balanced' pattern that matches sets of balanced characters.
  --Any two characters can be used.
  return s:gsub('%b[]','')=='' and true or false
end

function randomString()
  math.randomseed(os.time())
  math.random()math.random()math.random()math.random()
  local tokens={'[',']'}
  local result={}
  for i=1,8 do
    table.insert(result,tokens[math.random(1,2)])
  end
  return table.concat(result)
end

local RS=randomString()
print(RS)
print(isBalanced(RS))

```



## Maple

This functionality is provided by Maple.

```Maple

> use StringTools in
>       IsBalanced( "", "[", "]" );
>       IsBalanced( "[", "[", "]" );
>       IsBalanced( "]", "[", "]" );
>       IsBalanced( "[]", "[", "]" );
>       IsBalanced( "][", "[", "]" );
>       IsBalanced( "[][]", "[", "]" );
>       IsBalanced( "[[][]]", "[", "]" );
>       IsBalanced( "[[[]][]]]", "[", "]" );
>       s := Random( 20, "[]" );
>       IsBalanced( s, "[", "]" )
> end use;
                                  true

                                 false

                                 false

                                  true

                                 false

                                  true

                                  true

                                 false

                      s := "[[]][[[[[[[[[]][][]]"

                                 false

```

Furthermore, Maple can check whether multiple fences are balanced in the same string.

```Maple

> StringTools:-IsBalanced( "[()()]", "[(", "])" );
                                  true

```



## Mathematica


```mathematica

(* Generate open/close events. *)
gen[n_] := RandomSample[Table[{1, -1}, {n}] // Flatten]

(* Check balance. *)
check[lst_] := And @@ (# >= 0 & /@ Accumulate[lst])

(* Do task for string with n opening and n closing brackets. *)
doString[n_] := (
  lst = gen[n];
  str = StringJoin[lst /. {1 -> "[", -1 -> "]"}];
  Print[str <> If[match[lst, 0],
     "  is balanced.",
     "  is not balanced."]])

```


=={{header|MATLAB}} / {{header|Octave}}==


```matlab
function x = isbb(s)
   t = cumsum((s=='[') - (s==']'));
   x = all(t>=0) && (t(end)==0);
end;

```

Output:

```txt

octave:9> isbb('[]')
ans =  1
octave:10> isbb('][')
ans = 0
octave:11> isbb('][][')
ans = 0
octave:12> isbb('[][]')
ans =  1
octave:13> isbb('[][][]')
ans =  1
octave:14> isbb('[]][[]')
ans = 0

```



## Maxima


```maxima
brack(s) := block(
   [n: slength(s), r: 0, c],
   catch(
      for i thru n do (
         if cequal(c: charat(s, i), "]") then (if (r: r - 1) < 0 then throw(false))
         elseif cequal(c, "[") then r: r + 1
      ),
      is(r = 0)
   )
)$

brack("");
true

brack("[");
false

brack("]");
false

brack("[]");
true

brack("][");
false

brack("[[][]]");
true

brack("[[[]][]]]");
false
```



## Mercury


```Mercury

:- module balancedbrackets.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.


:- import_module list, random, char.

:- pred brackets(int::in,list(char)::out,supply::mdi,supply::muo) is det.

:- pred imbalance(list(char)::in,int::out) is semidet.
:- pred balanced(list(char)::in) is semidet.

:- implementation.

:- import_module int.

imbalance([],0).
imbalance(['['|T],N) :- imbalance(T,N+1).
imbalance([']'|T],N) :- N > 0, imbalance(T,N-1).

balanced(S) :- imbalance(S,0).

brackets(N,S,!RS) :-
    (
	N < 1 -> S is []
    ;   random(0,2,R,!RS),
	( R is 0 -> S is ['['|T], brackets(N-1,T,!RS)
	; S is [']'|T], brackets(N-1,T,!RS))).

main(!IO) :-
    random.init(0,RS),
    brackets(4,S,RS,_),
    print(S,!IO),
    (
	balanced(S) -> print(" is balanced\n",!IO)
    ;   print(" is unbalanced\n", !IO)
    ).

```


## Nim


```nim

from random import random, randomize, shuffle
from strutils import repeat

randomize()

proc gen(n: int): string =
  result = "[]".repeat(n)
  shuffle(result)

proc balanced(txt: string): bool =
  var b = 0
  for c in txt:
    case c
    of '[':
      inc(b)
    of ']':
      dec(b)
      if b < 0: return false
    else: discard
  b == 0

for n in 0..9:
  let s = gen(n)
  echo "'", s, "' is ", (if balanced(s): "balanced" else: "not balanced")
```

Output:

```txt
'' is balanced
'][' is not balanced
'][[]' is not balanced
'[][[]]' is balanced
'[[]][][]' is balanced
']][][[[][]' is not balanced
'][]][][][[][' is not balanced
'[[[[[[]]]][]]]' is balanced
'][[][]]]][[[[][]' is not balanced
'][][][][][[][[]]][' is not balanced
```


=={{header|Oberon-2}}==
{{works with|oo2c version 2}}

```oberon2

MODULE BalancedBrackets;
IMPORT
  Object,
  Object:Boxed,
  ADT:LinkedList,
  ADT:Storable,
  IO,
  Out := NPCT:Console;

TYPE
  (* CHAR is not boxed in the standard lib *)
  (* so make a boxed char *)
  Character* = POINTER TO CharacterDesc;
  CharacterDesc* = RECORD
    (Boxed.ObjectDesc)
    c: CHAR;
  END;

(* Method for a boxed char *)
PROCEDURE (c: Character) INIT*(x: CHAR);
BEGIN
  c.c := x;
END INIT;

PROCEDURE NewCharacter*(c: CHAR): Character;
VAR
  x: Character;
BEGIN
    NEW(x);x.INIT(c);RETURN x
END NewCharacter;

PROCEDURE (c: Character) ToString*(): STRING;
BEGIN
  RETURN Object.NewLatin1Char(c.c);
END ToString;

PROCEDURE (c: Character) Load*(r: Storable.Reader) RAISES IO.Error;
BEGIN
  r.ReadChar(c.c);
END Load;

PROCEDURE (c: Character) Store*(w: Storable.Writer) RAISES IO.Error;
BEGIN
  w.WriteChar(c.c);
END Store;

PROCEDURE (c: Character) Cmp*(o: Object.Object): LONGINT;
BEGIN
  IF c.c < o(Character).c THEN RETURN -1
  ELSIF c.c = o(Character).c THEN RETURN 0
  ELSE RETURN 1
  END
END Cmp;
(* end of methods for a boxed char *)

PROCEDURE CheckBalance(str: STRING): BOOLEAN;
VAR
  s: LinkedList.LinkedList(Character);
  chars: Object.CharsLatin1;
  n, x: Boxed.Object;
  i,len: LONGINT;
BEGIN
  i := 0;
  chars := str(Object.String8).CharsLatin1();
  len := str.length;
  s := NEW(LinkedList.LinkedList(Character));
  WHILE (i < len) & (chars[i] # 0X) DO
    IF s.IsEmpty() THEN
      s.Append(NewCharacter(chars[i]))  (* Push character *)
    ELSE
      n := s.GetLast(); (* top character *)
      WITH
        n: Character DO
          IF (chars[i] = ']') & (n.c = '[') THEN
            x := s.RemoveLast(); (* Pop character *)
            x := NIL
          ELSE
            s.Append(NewCharacter(chars[i]))
          END
        ELSE RETURN FALSE
      END (* WITH *)
    END;
    INC(i)
  END;
  RETURN s.IsEmpty()
END CheckBalance;

PROCEDURE Do;
VAR
  str: STRING;
BEGIN
  str := "[]";Out.String(str + ":> "); Out.Bool(CheckBalance(str));Out.Ln;
  str := "[][]";Out.String(str + ":> ");Out.Bool(CheckBalance(str));Out.Ln;
  str := "[[][]]";Out.String(str + ":> ");Out.Bool(CheckBalance(str));Out.Ln;
  str := "][";Out.String(str + ":> ");Out.Bool(CheckBalance(str));Out.Ln;
  str := "][][";Out.String(str + ":> ");Out.Bool(CheckBalance(str));Out.Ln;
  str := "[]][[]";Out.String(str + ":> ");Out.Bool(CheckBalance(str));Out.Ln;
END Do;

BEGIN
  Do
END BalancedBrackets.

```

{{out}}

```txt

[]:> TRUE
[][]:> TRUE
[[][]]:> TRUE
][:> FALSE
][][:> FALSE
[]][[]:> FALSE

```



## Objeck


```objeck

bundle Default {
  class Balanced {
    function : IsBalanced(text : String) ~ Bool {
      level := 0;
      each(i : text) {
        character := text->Get(i);
        if(character = ']') {
          if(level = 0) {
            return false;
          };
          level -= 1;
        };

        if(character = '[') {
          level += 1;
        };
      };

      return level = 0;
    }

    function : Main(args : String[]) ~ Nil {
      ": "->Print(); IsBalanced("")->PrintLine();
      "[]: "->Print(); IsBalanced("[]")->PrintLine();
      "[][]: "->Print(); IsBalanced("[][]")->PrintLine();
      "[[][]]: "->Print(); IsBalanced("[[][]]")->PrintLine();
      "][: "->Print(); IsBalanced("][")->PrintLine();
      "][][: "->Print(); IsBalanced("][][")->PrintLine();
      "[]][[]: "->Print(); IsBalanced("[]][[]")->PrintLine();
    }
  }
}

```


```txt

: true
[]: true
[][]: true
[[][]]: true
][: false
][][: false
[]][[]: false

```



## OCaml



```ocaml
let generate_brackets n =
  let rec aux i acc =
    if i <= 0 then acc else
      aux (pred i) ('['::']'::acc)
  in
  let brk = aux n [] in
  List.sort (fun _ _ -> (Random.int 3) - 1) brk

let is_balanced brk =
  let rec aux = function
    | [], 0 -> true
    | '['::brk, level -> aux (brk, succ level)
    | ']'::brk, 0 -> false
    | ']'::brk, level -> aux (brk, pred level)
    | _ -> assert false
  in
  aux (brk, 0)

let () =
  let n = int_of_string Sys.argv.(1) in
  Random.self_init();
  let brk = generate_brackets n in
  List.iter print_char brk;
  Printf.printf " %B\n" (is_balanced brk);
;;
```



```txt

$ ocaml balanced_brackets.ml 3
[]][[] false
$ ocaml balanced_brackets.ml 3
[[]][] true

```



## Oforth



```Oforth
String method: isBalanced
| c |
   0 self forEach: c [
      c '[' == ifTrue: [ 1+ continue ]
      c ']' <> ifTrue: [ continue ]
      1- dup 0 < ifTrue: [ drop false return ]
      ]
   0 == ;

: genBrackets(n)
   "" #[ "[" "]" 2 rand 2 == ifTrue: [ swap ] rot + swap + ] times(n) ;
```


{{out}}

```txt

#[ genBrackets(5) dup print " -->" print isBalanced println ] times(10)
[[][[]][]] -->-1
][][][][][ -->0
][[][][]][ -->0
][[]][[]][ -->0
[][][][][] -->-1
]]][][][[[ -->0
[[[[[]]]]] -->-1
[[[[[]]]]] -->-1
][][][][][ -->0
]]][][][[[ -->0

```



## ooRexx


```ooRexx

tests = .array~of("", "[]", "][", "[][]", "][][", "[[][]]", "[]][[]")

-- add some randomly generated tests
loop i = 1 to 8
    tests~append(generateBrackets(i))
end

loop test over tests
    say test":" checkbrackets(test)
end

::routine checkBrackets
  use arg input
  -- counter of bracket groups.  Must be 0 at end to be valid
  groups = 0

  -- loop over all of the characters
  loop c over input~makearray("")
      if c == '[' then groups += 1
      else if c == ']' then groups -= 1
      else return .false  -- non-bracket char found
      -- check for a close occurring before an open
      if groups < 0 then return .false
  end
  -- should be zero at the end
  return groups == 0

-- generate a string with n pairs of brackets
::routine generateBrackets
  use arg n

  answer = .mutablebuffer~new(,2*n)

  openBracketsNeeded = n
  unclosedBrackets = 0
  loop while answer~length < 2 * n
      if random(0, 1) & openBracketsNeeded > 0 | unclosedBrackets == 0 then do
          answer~append('[')
          openBracketsNeeded -= 1
          unclosedBrackets += 1
      end
      else do
          answer~append(']')
          unclosedBrackets -= 1
      end
  end
  return answer~string

```

Sample output (uses randomly generated groupings, so it should be different on each run):

```txt

: 1
[]: 1
][: 0
[][]: 1
][][: 0
[[][]]: 1
[]][[]: 0
[]: 1
[[]]: 1
[][[]]: 1
[][[][]]: 1
[][][[[]]]: 1
[[]][[][]][]: 1
[][][[][[][]]]: 1
[][[][][[[][]]]]: 1

```



## OxygenBasic


```oxygenbasic
function CheckBrackets(string s) as bool
'
### =================================

  sys co, le=len s
  byte b at strptr s
  indexbase 0
  for i=0 to <le
    select b(i)
    case "[" : co++
    case "]" : co--
    end select
    if co<0 then return 0
  next
  if co=0 then return 1
end function


'TEST
'====

print CheckBrackets ""    '1
print CheckBrackets "["   '0
print CheckBrackets "]"   '0
print CheckBrackets "[]"  '1
print CheckBrackets "[[]" '0
print CheckBrackets "[]]" '0
print CheckBrackets "[][]"'1
print CheckBrackets "]["  '0

```



## PARI/GP


```parigp
balanced(s)={
  my(n=0,v=Vecsmall(s));
  for(i=1,#v,
    if(v[i]==91,
      n++
    ,
      if(v[i]==93 && n, n--, return(0))
    )
  );
  !n
};
rnd(n)=Strchr(vectorsmall(n,i,if(random(2),91,93)))
forstep(n=0,10,2,s=rnd(n);print(s"\t"if(balanced(s),"true","false")))
```



## Pascal

See [[Balanced_brackets#Delphi | Delphi]]


## Perl


Idiomatic solution, using a regex that performs subpattern recursion ''(works with Perl 5.10 and newer)'':


```Perl
sub generate {
    my $n = shift;
    my $str = '[' x $n;
    substr($str, rand($n + $_), 0) = ']' for 1..$n;
    return $str;
}

sub balanced {
    shift =~ /^ (\[ (?1)* \])* $/x;
}

for (0..8) {
    my $input = generate($_);
    print balanced($input) ? " ok:" : "bad:", " '$input'\n";
}
```


{{out}}

```txt

 ok: ''
 ok: '[]'
bad: '[]]['
bad: ']][][['
 ok: '[[]][[]]'
bad: '[[[][]]]]['
bad: '][[[]][]][[]'
 ok: '[[]][[[][[]]]]'
bad: ']][]]][[][][[][['

```


If input strings are allowed to contain unrelated characters, this can be extended to:


```Perl
sub balanced {
    shift =~ /^ ( [^\[\]]++ | \[ (?1)* \] )* $/x;
}
```


<code>Regexp::Common::balanced</code> can give such a regexp too (non-bracket chars allowed).  Its recent versions use the subpattern recursion and are hence also only for Perl 5.10 and up.


```Perl
use Regexp::Common 'balanced';
my $re = qr/^$RE{balanced}{-parens=>'[]'}$/;
sub balanced {
  return shift =~ $re;
}
```


Alternative implementation, using straightforward depth counting:


```Perl
sub balanced {
    my $depth = 0;
    for (split //, shift) {
        if    ($_ eq '[') { ++$depth }
        elsif ($_ eq ']') { return if --$depth < 0 }
    }
    return !$depth
}
```



## Perl 6

There's More Than One Way To Do It.

### Depth counter


{{works with|Rakudo|2015.12}}


```perl6
sub balanced($s) {
    my $l = 0;
    for $s.comb {
        when "]" {
            --$l;
            return False if $l < 0;
        }
        when "[" {
            ++$l;
        }
    }
    return $l == 0;
}

my $n = prompt "Number of brackets";
my $s = (<[ ]> xx $n).flat.pick(*).join;
say "$s {balanced($s) ?? "is" !! "is not"} well-balanced"
```



### FP oriented

Here's a more idiomatic solution using a hyperoperator to compare all the characters to a backslash (which is between the brackets in ASCII), a triangle reduction to return the running sum, a <tt>given</tt> to make that list the topic, and then a topicalized junction and a topicalized subscript to test the criteria for balance.

```perl6
sub balanced($s) {
    .none < 0 and .[*-1] == 0
        given ([\+] '\\' «leg« $s.comb).cache;
}

my $n = prompt "Number of bracket pairs: ";
my $s = <[ ]>.roll($n*2).join;
say "$s { balanced($s) ?? "is" !! "is not" } well-balanced"
```



### String munging

Of course, a Perl 5 programmer might just remove as many inner balanced pairs as possible and then see what's left.
{{works with|Rakudo|2015.12}}

```perl6
sub balanced($_ is copy) {
    Nil while s:g/'[]'//;
    $_ eq '';
}

my $n = prompt "Number of bracket pairs: ";
my $s = <[ ]>.roll($n*2).join;
say "$s is", ' not' x not balanced($s), " well-balanced";
```



### Parsing with a grammar

{{works with|Rakudo|2015.12}}

```perl6
grammar BalBrack { token TOP { '[' <TOP>* ']' } }

my $n = prompt "Number of bracket pairs: ";
my $s = ('[' xx $n, ']' xx $n).flat.pick(*).join;
say "$s { BalBrack.parse($s) ?? "is" !! "is not" } well-balanced";
```



## Phix


```Phix
function check_brackets(sequence s)
integer level = 0
    for i=1 to length(s) do
        switch s[i]
            case '[': level += 1
            case ']': level -= 1
                      if level<0 then exit end if
        end switch
    end for
    return (level=0)
end function

sequence s
constant ok = {"not ok","ok"}

for i=1 to 10 do
    for j=1 to 2 do
        s = shuffle(join(repeat("[]",i-1),""))
        printf(1,"%s %s\n",{s,ok[check_brackets(s)+1]})
    end for
end for
```

{{out}}

```txt

 ok
 ok
[] ok
][ not ok
[][] ok
][][ not ok
[]][][ not ok
][][[] not ok
][[][]][ not ok
[][[][]] ok
[]]][[[]][ not ok
][]][[[]][ not ok
][[]]]][[][[ not ok
[][[]][[]][] ok
[]][][]]][[[[] not ok
[[][][]][][[]] ok
[[][]][][[]][[]] ok
][][[[[][]]][]][ not ok
[[[[]]][][[[][]]]] ok
[[[]]][[[[][]]][]] ok

```



## PHP

The sample is given as unix shell script, you need to have ''php-cli'' (or what your package manager calls it) installed.


```PHP
#!/usr/bin/php
<?php

# brackets generator
function bgenerate ($n) {
    if ($n==0) return '';
    $s = str_repeat('[', $n) . str_repeat(']', $n);
    return str_shuffle($s);
}

function printbool($b) {return ($b) ? 'OK' : 'NOT OK';}

function isbalanced($s) {
    $bal = 0;
    for ($i=0; $i < strlen($s); $i++) {
        $ch = substr($s, $i, 1);
        if ($ch == '[') {
            $bal++;
        } else {
            $bal--;
        }
        if ($bal < 0) return false;
    }
    return ($bal == 0);
}

# test parameters are N (see spec)
$tests = array(0, 2,2,2, 3,3,3, 4,4,4,4);

foreach ($tests as $v) {
    $s = bgenerate($v);
    printf("%s\t%s%s", $s, printbool(isbalanced($s)), PHP_EOL);
}

```

Sample run:

```txt

        OK
[][]    OK
[[]]    OK
[]][    NOT OK
][][[]  NOT OK
[][[]]  OK
][[[]]  NOT OK
]][[][][        NOT OK
[]][][[]        NOT OK
][]]][[[        NOT OK
[[[][]]]        OK

```



## PicoLisp


```PicoLisp
(load "@lib/simul.l")  # For 'shuffle'

(de generateBrackets (N)
   (shuffle (make (do N (link "[" "]")))) )

(de checkBrackets (S)
   (let N 0
      (for C S
         (if (= C "[")
            (inc 'N)
            (if2 (= C "]") (=0 N)
               (off N)
               (dec 'N) ) ) )
      (=0 N) ) )

(for N 10
   (prinl (if (checkBrackets (prin (generateBrackets N))) " OK" "not OK")) )
```

Output:

```txt
[] OK
[[]] OK
]]][[[not OK
[[[][]]] OK
[][][[[]]] OK
[]][[[][[]]]not OK
[[[]]][][][][] OK
]][][[[[]][]]][[not OK
[]][][[[][[]]][]][not OK
[[[][]]]]][][[]]][[[not OK
```



## PL/I


```pli
*process m or(!) s attributes source;
 cb: Proc Options(main);
 /* PL/I program to check for balanced brackets [] ********************
 * 07.12.2013 Walter Pachl translated from REXX Version 2
 *********************************************************************/
 Dcl v Char(20) Var;
 Dcl (i,j) Bin Fixed(31);
 Dcl r Bin Float(53);

 Call testbal('');                  /* first some user written tests */
 Call testbal('[][][][[]]');
 Call testbal('[][][][[]]][');
 Call testbal('[');
 Call testbal(']');
 Call testbal('[]');
 Call testbal('][');
 Call testbal('][][');
 Call testbal('[[]]');
 Call testbal('[[[[[[[]]]]]]]');
 Call testbal('[[[[[]]]][]');
 Call testbal('[][]');
 Call testbal('[]][[]');
 Call testbal(']]][[[[]');
 Call testbal('[[a]][b]');
 Put Edit(' ')(Skip,a);
 r=random(12345);                      /* then some generated ones   */
 Do i=1 To 10;
   v='';
   Do j=1 To 10;
     r=random();
     If r>0.5 Then v=v!!']';
              Else v=v!!'[';
     End;
   Call testbal(v);
   End;
 Return;

 testbal: Proc(s);          /* test the given string and show result */
 Dcl s Char(*);
 Dcl yesno(0:1) Char(20) Var Init('unbalanced','  balanced');
 Put Edit(yesno(checkbal(s)),''''!!s!!'''')(Skip,a,x(1),a);
 End;

 checkBal: proc(s) Returns(Bin Fixed(31));
                                    /*check for balanced brackets [] */
 Dcl s Char(*);
 Dcl nest Bin Fixed(31) Init(0);
 Dcl i Bin Fixed(31);
 Do i=1 To length(s);
   Select(substr(s,i,1));
     When('[') nest+=1;
     When(']') Do;
       If nest=0 Then return(0);
       nest-=1;
       End;
     Otherwise;
     End;
   End;
 Return(nest=0);
 End;

 End;
```

Output:

```txt
  balanced ''
  balanced '[][][][[]]'
unbalanced '[][][][[]]]['
unbalanced '['
unbalanced ']'
  balanced '[]'
unbalanced ']['
unbalanced '][]['
  balanced '[[]]'
  balanced '[[[[[[[]]]]]]]'
unbalanced '[[[[[]]]][]'
  balanced '[][]'
unbalanced '[]][[]'
unbalanced ']]][[[[]'
  balanced '[[a]][b]'

unbalanced '][[][[[[[]'
  balanced '[[]][[[]]]'
unbalanced ']][[[[][[['
unbalanced '[[[][][[]]'
unbalanced ']]][[[[[]]'
  balanced '[[[][][]]]'
unbalanced '[][][][[]['
unbalanced '[[]]]][[]['
unbalanced '[]][]]][[]'
unbalanced '][[][[[[[]'
```



## PowerShell

{{works with|PowerShell|2}}

```PowerShell

function Get-BalanceStatus ( $String )
    {
    $Open = 0
    ForEach ( $Character in [char[]]$String )
        {
        switch ( $Character )
            {
            "["     { $Open++ }
            "]"     { $Open-- }
            default { $Open = -1 }
            }
        #  If Open drops below zero (close before open or non-allowed character)
        #    Exit loop
        If ( $Open -lt 0 ) { Break }
        }
    $Status = ( "NOT OK", "OK" )[( $Open -eq 0 )]
    return $Status
    }

```


```PowerShell

#  Test
$Strings = @( "" )
$Strings += 1..5 | ForEach { ( [char[]]("[]" * $_) | Get-Random -Count ( $_ * 2 ) ) -join "" }

ForEach ( $String in $Strings )
    {
    $String.PadRight( 12, " " ) + (Get-BalanceStatus $String)
    }

```

{{out}}

```txt

            OK
[]          OK
]][[        NOT OK
]][][[      NOT OK
[[[][]]]    OK
][[[]][][]  NOT OK

```



===PowerShell (Regex Version)===

```PowerShell

function Test-BalancedBracket
{
  <#
    .SYNOPSIS
        Tests a string for balanced brackets.
    .DESCRIPTION
        Tests a string for balanced brackets. ("<>", "[]", "{}" or "()")
    .EXAMPLE
        Test-BalancedBracket -Bracket Brace -String '{abc(def[0]).xyz}'
        Test a string for balanced braces.
    .EXAMPLE
        Test-BalancedBracket -Bracket Curly -String '{abc(def[0]).xyz}'
        Test a string for balanced curly braces.
    .EXAMPLE
        Test-BalancedBracket -Bracket Curly -String ([System.IO.File]::ReadAllText('.\Foo.ps1'))
        Test a file for balanced curly braces.
    .LINK
        http://go.microsoft.com/fwlink/?LinkId=133231
  #>
    [CmdletBinding()]
    [OutputType([bool])]
    Param
    (
        [Parameter(Mandatory=$true)]
        [ValidateSet("Angle", "Brace", "Curly", "Paren")]
        [string]
        $Bracket,

        [Parameter(Mandatory=$true)]
        [AllowEmptyString()]
        [string]
        $String
    )

    $notFound = -1

    $brackets = @{
        Angle = @{Left="<"; Right=">"; Regex="^[^<>]*(?>(?>(?'pair'\<)[^<>]*)+(?>(?'-pair'\>)[^<>]*)+)+(?(pair)(?!))$"}
        Brace = @{Left="["; Right="]"; Regex="^[^\[\]]*(?>(?>(?'pair'\[)[^\[\]]*)+(?>(?'-pair'\])[^\[\]]*)+)+(?(pair)(?!))$"}
        Curly = @{Left="{"; Right="}"; Regex="^[^{}]*(?>(?>(?'pair'\{)[^{}]*)+(?>(?'-pair'\})[^{}]*)+)+(?(pair)(?!))$"}
        Paren = @{Left="("; Right=")"; Regex="^[^()]*(?>(?>(?'pair'\()[^()]*)+(?>(?'-pair'\))[^()]*)+)+(?(pair)(?!))$"}
    }

    if ($String.IndexOf($brackets.$Bracket.Left)  -eq $notFound -and
        $String.IndexOf($brackets.$Bracket.Right) -eq $notFound -or  $String -eq [String]::Empty)
    {
        return $true
    }

    $String -match $brackets.$Bracket.Regex
}


'', '[]', '][', '[][]', '][][', '[[][]]', '[]][[]' | ForEach-Object {
    if ($_ -eq "") { $s = "(Empty)" } else { $s = $_ }
    "{0}: {1}" -f  $s.PadRight(8), "$(if (Test-BalancedBracket Brace $s) {'Is balanced.'} else {'Is not balanced.'})"
}

```

{{Out}}

```txt

(Empty) : Is balanced.
[]      : Is balanced.
][      : Is not balanced.
[][]    : Is balanced.
][][    : Is not balanced.
[[][]]  : Is balanced.
[]][[]  : Is not balanced.

```



## Prolog

DCG are very usefull for this kind of exercice !

```Prolog
rosetta_brackets :-
	test_brackets([]),
	test_brackets(['[',']']),
	test_brackets(['[',']','[',']']),
	test_brackets(['[','[',']','[',']',']']),
	test_brackets([']','[']),
	test_brackets([']','[',']','[']),
	test_brackets(['[',']',']','[','[',']']).

balanced_brackets :-
	gen_bracket(2,  B1, []), test_brackets(B1),
	gen_bracket(4,  B2, []), test_brackets(B2),
	gen_bracket(4,  B3, []), test_brackets(B3),
	gen_bracket(6,  B4, []), test_brackets(B4),
	gen_bracket(6,  B5, []), test_brackets(B5),
	gen_bracket(8,  B6, []), test_brackets(B6),
	gen_bracket(8,  B7, []), test_brackets(B7),
	gen_bracket(10, B8, []), test_brackets(B8),
	gen_bracket(10, B9, []), test_brackets(B9).

test_brackets(Goal) :-
	(   Goal = [] -> write('(empty)'); maplist(write, Goal)),
	(   balanced_brackets(Goal, []) ->
	    writeln(' succeed')
	;
	    writeln(' failed')
	).

% grammar of balanced brackets
balanced_brackets --> [].

balanced_brackets -->
	['['],
	balanced_brackets,
	[']'].

balanced_brackets -->
	['[',']'],
	balanced_brackets.


% generator of random brackets
gen_bracket(0) --> [].

gen_bracket(N) -->
	{N1 is N - 1,
	 R is random(2)},
	bracket(R),
	gen_bracket(N1).

bracket(0) --> ['['].
bracket(1) --> [']'].

```

Sample output :

```txt
 ?- balanced_brackets.
[[ failed
[[][ failed
[[]] succeed
[[][]] succeed
[][[][ failed
][]][[[] failed
[[[[]][] failed
[[[[[][[]] failed
[]][[[][]] failed
true .

```

Test with Rosetta examples :

```txt
 ?- rosetta_brackets.
(empty) succeed
[] succeed
[][] succeed
[[][]] succeed
][ failed
][][ failed
[]][[] failed
true.

```



## PureBasic


```PureBasic
Procedure.s Generate(N)
  For i=1 To N
    sample$+"[]"
  Next
  For i=Len(sample$)-1 To 2 Step -1
    r=Random(i-1)+1
    If r<>i
      a.c=PeekC(@sample$+r*SizeOf(Character))
      b.c=PeekC(@sample$+i*SizeOf(Character))
      PokeC(@sample$+r*SizeOf(Character), b)
      PokeC(@sample$+i*SizeOf(Character), a)
    EndIf
  Next
  ProcedureReturn sample$
EndProcedure

Procedure Balanced(String$)
  Protected *p.Character, cnt
  *p=@String$
  While *p\c
    If *p\c='['
      cnt+1
    ElseIf *p\c=']'
      cnt-1
      If cnt<0: Break: EndIf
    EndIf
    *p+SizeOf(Character)
  Wend
  If cnt=0
    ProcedureReturn #True
  EndIf
EndProcedure

;- Test code
OpenConsole()
For i=1 To 5
  TestString$ = Generate(i)
  Print(TestString$)
  If Balanced(TestString$)
    PrintN(" is balanced.")
  Else
    PrintN(" is not balanced")
  EndIf
Next
```

Output sample

```txt

 [] is balanced.
 [[]] is balanced.
 [[[]]] is balanced.
 [][]]][[ is not balanced
 [][][][]][ is not balanced

```



## Python


### Procedural


```python>>>
 def gen(N):
...     txt = ['[', ']'] * N
...     random.shuffle( txt )
...     return ''.join(txt)
...
>>> def balanced(txt):
...     braced = 0
...     for ch in txt:
...         if ch == '[': braced += 1
...         if ch == ']':
...             braced -= 1
...             if braced < 0: return False
...     return braced == 0
...
>>> for txt in (gen(N) for N in range(10)):
...     print ("%-22r is%s balanced" % (txt, '' if balanced(txt) else ' not'))
...
''                     is balanced
'[]'                   is balanced
'[][]'                 is balanced
'][[[]]'               is not balanced
'[]][[][]'             is not balanced
'[][[][]]]['           is not balanced
'][]][][[]][['         is not balanced
'[[]]]]][]][[[['       is not balanced
'[[[[]][]]][[][]]'     is balanced
'][[][[]]][]]][[[[]'   is not balanced
```



###  Functional

{{works with|Python|3.2}}
Rather than explicitly track the count, we can just write the per-element test and use stdlib functions to turn it into a whole-sequence test. It's straightforwardly declarative, and hard to get wrong, but whether it's actually easier to understand depends on how familiar the reader is with thinking in `itertools` style.


```python>>>
 from itertools import accumulate
>>> from random import shuffle
>>> def gen(n):
...     txt = list('[]' * n)
...     shuffle(txt)
...     return ''.join(txt)
...
>>> def balanced(txt):
...     brackets = ({'[': 1, ']': -1}.get(ch, 0) for ch in txt)
...     return all(x>=0 for x in accumulate(brackets))
...
>>> for txt in (gen(N) for N in range(10)):
...     print ("%-22r is%s balanced" % (txt, '' if balanced(txt) else ' not'))
...
''                     is balanced
']['                   is not balanced
'[]]['                 is not balanced
']][[[]'               is not balanced
'][[][][]'             is not balanced
'[[[][][]]]'           is balanced
'][[[][][]][]'         is not balanced
'][]][][[]][[]['       is not balanced
'][[]]][][[]][[[]'     is not balanced
'][[][[]]]][[[]][]['   is not balanced
```



###  Array Programming

{{libheader|NumPy}}
The numpy library gives us a way to write just the elementwise tests and automatically turn them into whole-sequence tests, although it can be a bit clumsy to use for character rather than numeric operations. The simplicity of the final expression probably doesn't make up for all that extra clumsiness in this case.


```python>>>
 import numpy as np
>>> from random import shuffle
>>> def gen(n):
...     txt = list('[]' * n)
...     shuffle(txt)
...     return ''.join(txt)
...
>>> m = np.array([{'[': 1, ']': -1}.get(chr(c), 0) for c in range(128)])
>>> def balanced(txt):
...     a = np.array(txt, 'c').view(np.uint8)
...     return np.all(m[a].cumsum() >= 0)
...
>>> for txt in (gen(N) for N in range(10)):
...     print ("%-22r is%s balanced" % (txt, '' if balanced(txt) else ' not'))
...
''                     is balanced
']['                   is not balanced
'[[]]'                 is balanced
'[]][]['               is not balanced
']][]][[['             is not balanced
'[[]][[][]]'           is balanced
'[][[]][[]]]['         is not balanced
'[][[[]][[]]][]'       is balanced
'[[][][[]]][[[]]]'     is balanced
'][]][][[]][]][][[['   is not balanced
```



## Qi



```qi
(define balanced-brackets-0
  []      0   -> true
  []      _   -> false
  [#\[|R] Sum -> (balanced-brackets-0 R (+ Sum 1))
  _       0   -> false
  [_  |R] Sum -> (balanced-brackets-0 R (- Sum 1)))


(define balanced-brackets
  "" -> true
  S  -> (balanced-brackets-0 (explode (INTERN S)) 0))

(balanced-brackets "")

(balanced-brackets "[]")
(balanced-brackets "[][]")
(balanced-brackets "[[][]]")

(balanced-brackets "][")
(balanced-brackets "][][")
(balanced-brackets "[]][[]")


```




## R



```r
balanced <- function(str){
  str <- strsplit(str, "")[[1]]
  str <- ifelse(str=='[', 1, -1)
  all(cumsum(str) >= 0) && sum(str) == 0
}
```


Alternately, using perl 5.10-compatible regexps,


```r
balanced <- function(str) {
  regexpr('^(\\[(?1)*\\])*$', str, perl=TRUE) > -1
}
```


To generate some some examples:


```R
rand.parens <- function(n) paste(sample(c("[","]"),2*n,replace=T),collapse="")

as.data.frame(within(list(), {
  parens <- replicate(10, rand.parens(sample.int(10,size=1)))
  balanced <- sapply(parens, balanced)
}))
```


Output:

```r
   balanced             parens
1     FALSE               ][][
2     FALSE [][[]]][[]][]]][[[
3     FALSE     ][[][][]][][[]
4     FALSE     ][][][][][][][
5      TRUE [[[][]]][[[][][]]]
6      TRUE                 []
7     FALSE           ]][[][[]
8     FALSE     []]]][[[]][[[]
9      TRUE     [[[[][[][]]]]]
10     TRUE                 []
```



## Racket



```Racket

#lang racket

(define (generate n)
  (list->string (shuffle (append* (make-list n '(#\[ #\]))))))

(define (balanced? str)
  (let loop ([l (string->list str)] [n 0])
    (or (null? l)
        (if (eq? #\[ (car l))
          (loop (cdr l) (add1 n))
          (and (> n 0) (loop (cdr l) (sub1 n)))))))

(define (try n)
  (define s (generate n))
  (printf "~a => ~a\n" s (if (balanced? s) "OK" "NOT OK")))

(for ([n 10]) (try n))

```



## Red


```Red
; Functional code
balanced-brackets: [#"[" any balanced-brackets #"]"]
rule: [any balanced-brackets end]
balanced?: func [str][parse str rule]

; Tests
tests: [
	good: ["" "[]" "[][]" "[[]]" "[[][]]" "[[[[[]]][][[]]]]"]
	bad:  ["[" "]" "][" "[[]" "[]]" "[]][[]" "[[[[[[]]]]]]]"]
]

foreach str tests/good [
	if not balanced? str [print [mold str "failed!"]]
]
foreach str tests/bad [
	if balanced? str [print [mold str "failed!"]]
]

repeat i 10 [
	str: random copy/part "[][][][][][][][][][]" i * 2
	print [mold str "is" either balanced? str ["balanced"]["unbalanced"]]
]
```



## REXX


### with 40 examples


```rexx
/*REXX program checks for balanced brackets     [  ]      ─── some fixed, others random.*/
parse arg seed .                                 /*obtain optional argument from the CL.*/
if datatype(seed,'W')  then call random ,,seed   /*if specified, then use as RANDOM seed*/
@.=0;          yesNo.0= right('not OK', 50)      /*for bad expressions, indent 50 spaces*/
               yesNo.1=           'OK'           /* [↓]  the 14 "fixed"  ][  expressions*/
q=                     ;          call checkBal  q;           say yesNo.result  '«null»'
q= '[][][][[]]'        ;          call checkBal  q;           say yesNo.result  q
q= '[][][][[]]]['      ;          call checkBal  q;           say yesNo.result  q
q= '['                 ;          call checkBal  q;           say yesNo.result  q
q= ']'                 ;          call checkBal  q;           say yesNo.result  q
q= '[]'                ;          call checkBal  q;           say yesNo.result  q
q= ']['                ;          call checkBal  q;           say yesNo.result  q
q= '][]['              ;          call checkBal  q;           say yesNo.result  q
q= '[[]]'              ;          call checkBal  q;           say yesNo.result  q
q= '[[[[[[[]]]]]]]'    ;          call checkBal  q;           say yesNo.result  q
q= '[[[[[]]]][]'       ;          call checkBal  q;           say yesNo.result  q
q= '[][]'              ;          call checkBal  q;           say yesNo.result  q
q= '[]][[]'            ;          call checkBal  q;           say yesNo.result  q
q= ']]][[[[]'          ;          call checkBal  q;           say yesNo.result  q
#=0                                                    /*# additional random expressions*/
          do j=1  until  #==26                         /*gen 26 unique bracket strings. */
          q=translate( rand( random(1,10) ), '][', 10) /*generate random bracket string.*/
          call checkBal q; if result==-1  then iterate /*skip if duplicated expression. */
          say yesNo.result  q                          /*display the result to console. */
          #=#+1                                        /*bump the  expression  counter. */
          end   /*j*/                            /* [↑]  generate 26 random "Q" strings.*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
?:        ?=random(0,1);                                     return ? || \?   /*REXX BIF*/
rand:     $=copies(?()?(),arg(1));  _=random(2,length($));   return left($,_-1)substr($,_)
/*──────────────────────────────────────────────────────────────────────────────────────*/
checkBal: procedure expose @.; parse arg y       /*obtain the   "bracket"   expression. */
          if @.y  then return -1                 /*Done this expression before?  Skip it*/
          @.y=1                                  /*indicate expression was processed.   */
          !=0;         do j=1  for length(y);      _=substr(y,j,1)    /*get a character.*/
                       if _=='[' then      !=!+1                      /*bump the nest #.*/
                                 else do;  !=!-1;  if !<0  then return 0;   end
                       end   /*j*/
          return !==0                            /* [↑]  "!" is the nested  ][  counter.*/
```

'''output'''   using the (some internal, others random) expressions:

```txt

OK «null»
OK [][][][[]]
                                            not OK [][][][[]]][
                                            not OK [
                                            not OK ]
OK []
                                            not OK ][
                                            not OK ][][
OK [[]]
OK [[[[[[[]]]]]]]
                                            not OK [[[[[]]]][]
OK [][]
                                            not OK []][[]
                                            not OK ]]][[[[]
                                            not OK []][
OK [][][][][][][][][][][][]
                                            not OK []][[]][[]][[]][[]][[]][[]][[]][
                                            not OK []][[]][[]][[]][[]][[]][[]][[]][[]][[]][
                                            not OK ][[]][[]
                                            not OK ][][][][][][][][][][][][][][][][
                                            not OK ][[]][[]][[]
                                            not OK []][[]][
                                            not OK ][][][][][][
OK [][][][][][]
OK [][][][][][][][][][][][][][]
OK [][][][][][][][][][][][][][][][][][][][]
                                            not OK []][[]][[]][[]][
                                            not OK ][[]
                                            not OK []][[]][[]][[]][[]][[]][[]][
                                            not OK ][[]][[]][[]][[]][[]
                                            not OK []][[]][[]][[]][[]][[]][
OK [][][][][][][][][][][][][][][][]
                                            not OK ][[]][[]][[]][[]][[]][[]][[]][[]
                                            not OK ][][][][][][][][][][][][
                                            not OK ][[]][[]][[]][[]][[]][[]][[]
                                            not OK ][[]][[]][[]][[]][[]][[]][[]][[]][[]][[]
                                            not OK ][][][][][][][][][][][][][][][][][][
OK [][][][][][][][]
                                            not OK ][[]][[]][[]][[]
                                            not OK ][][][][][][][][][][][][][][][][][][][][

```



### with examples + 30 permutations


```rexx

/*REXX program to check for balanced brackets [] **********************
* test strings and random string generation copied from Version 1
* the rest restructured (shortened) to some extent
* and output made reproducible (random with a seed)
* 10.07.2012 Walter Pachl
**********************************************************************/
yesno.0 = 'unbalanced'
yesno.1 = '  balanced'
done.=0                           /* memory what's been done         */
n=0                               /* number of tests                 */
Call testbal '[][][][[]]'         /* first some user written tests   */
Call testbal '[][][][[]]]['
Call testbal '['
Call testbal ']'
Call testbal '[]'
Call testbal ']['
Call testbal '][]['
Call testbal '[[]]'
Call testbal '[[[[[[[]]]]]]]'
Call testbal '[[[[[]]]][]'
Call testbal '[][]'
Call testbal '[]][[]'
Call testbal ']]][[[[]'
Call testbal ']'
Call testbal '['
                                  /* then some random generated ones */
Call random 1,2,12345             /* call random with a seed         */
                                  /* makes test reproducible         */
do Until n=30                     /* up to 30 tests                  */
  s=rand(random(1,8))             /* a 01 etc. string of length 4-32 */
  q=translate(s,'[]',01)          /* turn digits into brackets       */
  if done.q then                  /* string was already here         */
    iterate                       /* don't test again                */
  call testbal q                  /* test balance                    */
  End
exit

testbal:                    /* test the given string and show result */
  n=n+1                           /* number of tests                 */
  Parse Arg q                     /* get string to be tested         */
  done.q=1                        /* mark as done                    */
  call checkBal q                 /* test balance                    */
  lq=format(length(q),2)
  say right(n,2) lq yesno.result q/* show result and string          */
  Return

/*-----------------------------------PAND subroutine-----------------*/
pand: p=random(0,1);    return p || \p
/*-----------------------------------RAND subroutine-----------------*/
rand: pp=pand();   pp=pand()pp;    pp=copies(pp,arg(1))
      i=random(2,length(pp));      pp=left(pp,i-1)substr(pp,i)
return pp

checkBal: procedure               /*check for balanced brackets ()   */
  Parse arg y
  nest=0;
  do While y<>''
    Parse Var y c +1 y            /*pick off one character at a time */
    if c='[' then                 /* opening bracket                 */
      nest=nest+1                 /* increment nesting               */
    else do                       /* closing bracket                 */
      if nest=0 then              /* not allowed                     */
        return 0;                 /* no success                      */
      nest=nest-1                 /* decrement nesting               */
      end
    end
  return nest=0                   /* nest=0 -> balanced              */

```

{{out}}

```txt

 1 10   balanced [][][][[]]
 2 12 unbalanced [][][][[]]][
 3  1 unbalanced [
 4  1 unbalanced ]
 5  2   balanced []
 6  2 unbalanced ][
 7  4 unbalanced ][][
 8  4   balanced [[]]
 9 14   balanced [[[[[[[]]]]]]]
10 11 unbalanced [[[[[]]]][]
11  4   balanced [][]
12  6 unbalanced []][[]
13  8 unbalanced ]]][[[[]
14  1 unbalanced ]
15  1 unbalanced [
16 20 unbalanced ][][][][][][][][][][
17 24 unbalanced ][][][][][][][][][][][][
18 20 unbalanced []][[]][[]][[]][[]][
19 20   balanced [][][][][][][][][][]
20 24   balanced [][][][][][][][][][][][]
21 24 unbalanced []][[]][[]][[]][[]][[]][
22 12   balanced [][][][][][]
23 32   balanced [][][][][][][][][][][][][][][][]
24  8 unbalanced []][[]][
25 32 unbalanced ][[]][[]][[]][[]][[]][[]][[]][[]
26  4 unbalanced ][[]
27 28 unbalanced ][[]][[]][[]][[]][[]][[]][[]
28 32 unbalanced ][][][][][][][][][][][][][][][][
29 28 unbalanced []][[]][[]][[]][[]][[]][[]][
30  4 unbalanced []][

```


===with over 125,000 permutations===
This REXX version generates over one hundred thousand unique permutations of strings that contain an equal

amount of left   <big>[</big>   and right   <big>]</big>   brackets.

All   ''possible''   strings of twenty or less characters (legal bracket expressions) are generated.

This eliminates the possibility of missing a particular character string permutation that may not be generated

via a random generator.

Use is made of the   '''countstr'''   function   (which is a BIF for newer REXX interpreters), but a RYO version is

included here for older REXXes that don't contain that BIF   ('''B'''uilt   '''I'''n   '''F'''unction).

Naturally, each of the one hundred thousand character strings aren't displayed (for balanced/not-balanced),

but a count is displayed, as anyone can generate the same strings in other languages and compare results.

```rexx
/*REXX program checks for around 125,000 generated balanced brackets expressions  [ ]   */
bals=0
#=0;      do j=1  until  L>20                    /*generate lots of bracket permutations*/
          q=translate( strip( x2b( d2x(j) ), 'L', 0),  "][", 01)        /*convert ──► ][*/
          L=length(q)
          if countStr(']', q) \== countstr('[', q)  then iterate        /*not compliant?*/
          #=#+1                                                         /*bump legal Q's*/
          !=0;     do k=1  for L;      parse var q ? 2 q
                   if ?=='['  then     !=!+1
                              else do; !=!-1;  if !<0  then iterate j;  end
                   end   /*k*/

          if !==0  then bals=bals+1
          end  /*j*/                             /*done all 20─character possibilities? */

say #   " expressions were checked, "     bals    ' were balanced, ' ,
                                        #-bals    " were unbalanced."
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
countStr: procedure;   parse arg n,h,s;    if s==''  then s=1;       w=length(n)
               do r=0  until _==0;   _=pos(n,h,s);   s=_+w;   end;      return r
```

'''output'''   when using the default input:

```txt

125476  expressions were checked,  23713  were balanced,  101763  were unbalanced.

```



## Ring


```ring

nr = 0
while nr < 10
      nr += 1
      test=generate(random(9)+1)
      see "bracket string " + test + " is " + valid(test) + nl
end

func generate n
l = 0 r = 0 output = ""
while l<n and r<n
      switch random(2)
      on 1 l+=1 output+="["
      on 2 r+=1 output+="]"
      off
end
if l=n output+=copy("]",n-r) else output+=copy("]",n-l) ok
return output

func valid q
count = 0
if len(q)=0 return "ok." ok
for x=1 to len(q)
    if substr(q,x,1)="[" count+=1 else count-=1 ok
    if count<0 return "not ok." ok
next
return "ok."

```

Output:

```txt

bracket string ]][[][[[[]]] is not ok.
bracket string [[[]]] is ok.
bracket string ]][[[[[[[]]]]] is not ok.
bracket string [][[[][[][]]][]] is ok.
bracket string [[]][][[][[][[]]]] is ok.
bracket string ]] is not ok.
bracket string [[[]]] is ok.
bracket string [][[]] is ok.
bracket string [[]] is ok.
bracket string ]]]][]]]]]]]]] is not ok.

```



## Ruby

{{trans|D}}
{{works with|Ruby|1.9}}

```ruby
re = /\A     # beginning of string
  (?<bb>     # begin capture group <bb>
    \[       #   literal [
    \g<bb>*  #   zero or more <bb>
    \]       #   literal ]
  )*         # end group, zero or more such groups
\z/x         # end of string

10.times do |i|
  s = (%w{[ ]} * i).shuffle.join
  puts (s =~ re ? " OK: " : "bad: ") + s
end

["[[]", "[]]", "a[ letters[-1] ].xyz[0]"].each do |s|
  t = s.gsub(/[^\[\]]/, "")
  puts (t =~ re ? " OK: " : "bad: ") + s
end
```


One output:
```txt

 OK:
 OK: []
bad: []][
 OK: [[][]]
bad: []]][][[
bad: ][]][[[[]]
bad: []]][[]][[[]
bad: ][[][]][][[]][
 OK: [][[][[[]][]][]]
bad: []][][]][[[[[][]]]
bad: [[]
bad: []]
 OK: a[ letters[-1] ].xyz[0]

```



## Run BASIC


```runbasic
dim brk$(10)
brk$(1)  = "[[[][]]]"
brk$(2)  = "[[[]][[[][[][]]]]]"
brk$(3)  = "][][]][["
brk$(4)  = "[][][]"
brk$(5)  = "[][]][]][[]]][[["
brk$(6)  = "]][[[[]]]][]]][[[["
brk$(7)  = "[[][[[]]][]]"
brk$(8)  = "[]][][][[[]]"
brk$(9)  = "][]][["
brk$(10) = "[]][][][[]"

for i = 0 to 10
  b$ = brk$(i)
  while instr(b$,"[]") <> 0
    x = instr(b$,"[]")
    if x > 0 then b$ = left$(b$,x - 1) + mid$(b$,x + 2)
  wend
  if trim$(b$) = "" then print "    OK "; else print "Not OK ";
  print brk$(i)
next i
```


One output:
```txt
    OK
    OK [[[][]]]
    OK [[[]][[[][[][]]]]]
Not OK ][][]][[
    OK [][][]
Not OK [][]][]][[]]][[[
Not OK ]][[[[]]]][]]][[[[
    OK [[][[[]]][]]
Not OK []][][][[[]]
Not OK ][]][[
Not OK []][][][[]

```



## Rust


{{libheader|rand}}

```rust
extern crate rand;

trait Balanced {
    /// Returns true if the brackets are balanced
    fn is_balanced(&self) -> bool;
}

impl<'a> Balanced for str {
    fn is_balanced(&self) -> bool {
        let mut count = 0;

        for bracket in self.chars() {
            let change = match bracket {
                '[' => 1,
                ']' => -1,
                _ => panic!("Strings should only contain brackets")
            };

            count += change;
            if count < 0 { return false; }
        }

        count == 0
    }
}

/// Generates random brackets
fn generate_brackets(num: usize) -> String {
    use rand::random;

    (0..num).map(|_| if random() { '[' } else { ']' }).collect()
}

fn main() {
    for i in (0..10) {
        let brackets = generate_brackets(i);

        println!("{}    {}", brackets, brackets.is_balanced())
    }
}
```

Output:
```txt

    true
[    false
]]    false
][]    false
[[[[    false
]][[[    false
[][[]]    true
[]]][[]    false
[[[[[[][    false
][[[[][]]    false

```



## Scala


If you are new to Scala you might want to jump to Version 2.


###  Scala Version 1

{{works with|Scala|2.9.1}}

```scala
import scala.collection.mutable.ListBuffer
import scala.util.Random

object BalancedBrackets extends App {

  val random = new Random()
  def generateRandom: List[String] = {
    import scala.util.Random._
    val shuffleIt: Int => String = i => shuffle(("["*i+"]"*i).toList).foldLeft("")(_+_)
    (1 to 20).map(i=>(random.nextDouble*100).toInt).filter(_>2).map(shuffleIt(_)).toList
  }

  def generate(n: Int): List[String] = {
    val base = "["*n+"]"*n
    var lb = ListBuffer[String]()
    base.permutations.foreach(s=>lb+=s)
    lb.toList.sorted
  }

  def checkBalance(brackets: String):Boolean = {
    def balI(brackets: String, depth: Int):Boolean = {
      if (brackets == "") depth == 0
      else brackets(0) match {
        case '[' => ((brackets.size > 1) && balI(brackets.substring(1), depth + 1))
        case ']' => (depth > 0) && ((brackets.size == 1) || balI(brackets.substring(1), depth -1))
        case _   => false
      }
    }
    balI(brackets, 0)
  }

  println("arbitrary random order:")
  generateRandom.map(s=>Pair(s,checkBalance(s))).foreach(p=>println((if(p._2) "balanced:   " else "unbalanced: ")+p._1))
  println("\n"+"check all permutations of given length:")
  (1 to 5).map(generate(_)).flatten.map(s=>Pair(s,checkBalance(s))).foreach(p=>println((if(p._2) "balanced:   " else "unbalanced: ")+p._1))
}
```


<pre style="height:30ex;overflow:scroll">arbitrary random order:
unbalanced: ][[[]][][[]]][][]]]][[]]]][[][]][[]]][[][]]][[[]][[][[]]][[]]]]][][]]][[][]]]][[][[][[][[][][][[][]][][[][[[][]]][[]]]][][]][[][]]][[][][[[][[[][[[[[][[]][[[[[[[][]][[]][]]][[[]][[][[]][][]]
unbalanced: [[][][[[[][]]][][[][]][][[[]]]][]][]][]][][]][[]]]][[][[[]][][[][[[[[][[][][[]]][]]]
unbalanced: [[]][][][[[[][][][[[[[[[][[[]]][[[[]]][[]]]][]][]]]][]][]]][]]]]][][][[]]][]][]][]]][[]][][]][]][[[[[[][[[[][[[]][[][[[][[]][]][[[][[][]]][]]][[[[][][[[]][][][[[[]]][]]][][]]]][][][][]][
unbalanced: []]][]][[]]][]][]]]][]]]][]]]][][[][[][[[[][][[[[[[[[][[[]
unbalanced: [][[[]]][]]][[]]]]]][[[[[][][][][]]][[]]]]]][][[[]][[[[][][[][]][]][[[[][[[[[[[[][]]]]]][]][][[][]]][[
unbalanced: []][[][[]]
unbalanced: []][[[[][]][][[]][][[[]]]][[]][[[[[][][[[[][]]]]][]][]]][[[]][[[[[]]][[]][][[][][][]]][]]]][][][[[[]]][[]][][]][[]]]][[[[][[][][[[]][[]]]]]]][[]][[[][[][]][][[]][[]][]]][][[][][][[[[]][]][][
unbalanced: ][][[]][[[][[]][[][[[]]]]]][
unbalanced: [][]][][]][[
unbalanced: [[[][[]]]][][][][][][][]]]][[][][]][]]]]][[]]][][[]][]]][[][[][[][]][[]]]]][[[[][[][[]]][]]][][]][]]][][[[[]]][][[[][[[[]]][][]][][[[[[][][[][[][[][[][[[]
unbalanced: ][][[]][]]]][[][[][[][]]]][[[[
balanced:   [[[]]][[[[]][]]][][[[]]][][]
unbalanced: [[[]][]]]][][[[[[[[[[[[[][[]][[[[[[]][]]]][[]]]][]]]]][]]]]][[][[][][][[][[]][]]][[][[]][[[][]]]][]]]]][]][[[[
unbalanced: [[]][[]][[[[][][][][]]]][]]][][[][]]]][[[[
unbalanced: [][[]]][]]][[]]][[[]]][[][[]]]]][[[][]][[[[[[[[][[]]][][][[[]]][[][[][][]][]]][][]][]][]]][]][]][[[[[][[][[]
unbalanced: ][][[][][[[[][][[][[[][]]]]]][][][[][][[][[][[]]][[[[]]]][][[][[]][]]][[[][[[][][[[][]][[]]]][[][[]]]]]][[[][]][[[]]]]]]
unbalanced: ][[][[][][]][]][][[][][]][][][[[[][]]][[]]][[]
unbalanced: ]][][[]]]]][][[[][][]][[[[]][]]][]][[[]]][]]][[[[][[]]]][]][]][]][][[][[[]]][][][[][][[[[]][]]][]][[[[][]]]][][[][[][[[[[][[][]][[]][]]][][[]][]]][[[[][]][][]][[][]][[][[[[][

check all permutations of given length:
balanced:   []
unbalanced: ][
balanced:   [[]]
balanced:   [][]
unbalanced: []][
unbalanced: ][[]
unbalanced: ][][
unbalanced: ]][[
balanced:   [[[]]]
balanced:   [[][]]
balanced:   [[]][]
unbalanced: [[]]][
balanced:   [][[]]
balanced:   [][][]
unbalanced: [][]][
unbalanced: []][[]
unbalanced: []][][
unbalanced: []]][[
unbalanced: ][[[]]
unbalanced: ][[][]
unbalanced: ][[]][
unbalanced: ][][[]
unbalanced: ][][][
unbalanced: ][]][[
unbalanced: ]][[[]
unbalanced: ]][[][
unbalanced: ]][][[
unbalanced: ]]][[[
balanced:   [[[[]]]]
balanced:   [[[][]]]
balanced:   [[[]][]]
balanced:   [[[]]][]
unbalanced: [[[]]]][
balanced:   [[][[]]]
balanced:   [[][][]]
balanced:   [[][]][]
unbalanced: [[][]]][
balanced:   [[]][[]]
balanced:   [[]][][]
unbalanced: [[]][]][
unbalanced: [[]]][[]
unbalanced: [[]]][][
unbalanced: [[]]]][[
balanced:   [][[[]]]
balanced:   [][[][]]
balanced:   [][[]][]
unbalanced: [][[]]][
balanced:   [][][[]]
balanced:   [][][][]
unbalanced: [][][]][
unbalanced: [][]][[]
unbalanced: [][]][][
unbalanced: [][]]][[
unbalanced: []][[[]]
unbalanced: []][[][]
unbalanced: []][[]][
unbalanced: []][][[]
unbalanced: []][][][
unbalanced: []][]][[
unbalanced: []]][[[]
unbalanced: []]][[][
unbalanced: []]][][[
unbalanced: []]]][[[
unbalanced: ][[[[]]]
unbalanced: ][[[][]]
unbalanced: ][[[]][]
unbalanced: ][[[]]][
unbalanced: ][[][[]]
unbalanced: ][[][][]
unbalanced: ][[][]][
unbalanced: ][[]][[]
unbalanced: ][[]][][
unbalanced: ][[]]][[
unbalanced: ][][[[]]
unbalanced: ][][[][]
unbalanced: ][][[]][
unbalanced: ][][][[]
unbalanced: ][][][][
unbalanced: ][][]][[
unbalanced: ][]][[[]
unbalanced: ][]][[][
unbalanced: ][]][][[
unbalanced: ][]]][[[
unbalanced: ]][[[[]]
unbalanced: ]][[[][]
unbalanced: ]][[[]][
unbalanced: ]][[][[]
unbalanced: ]][[][][
unbalanced: ]][[]][[
unbalanced: ]][][[[]
unbalanced: ]][][[][
unbalanced: ]][][][[
unbalanced: ]][]][[[
unbalanced: ]]][[[[]
unbalanced: ]]][[[][
unbalanced: ]]][[][[
unbalanced: ]]][][[[
unbalanced: ]]]][[[[
balanced:   [[[[[]]]]]
balanced:   [[[[][]]]]
balanced:   [[[[]][]]]
balanced:   [[[[]]][]]
balanced:   [[[[]]]][]
unbalanced: [[[[]]]]][
balanced:   [[[][[]]]]
balanced:   [[[][][]]]
balanced:   [[[][]][]]
balanced:   [[[][]]][]
unbalanced: [[[][]]]][
balanced:   [[[]][[]]]
balanced:   [[[]][][]]
balanced:   [[[]][]][]
unbalanced: [[[]][]]][
balanced:   [[[]]][[]]
balanced:   [[[]]][][]
unbalanced: [[[]]][]][
unbalanced: [[[]]]][[]
unbalanced: [[[]]]][][
unbalanced: [[[]]]]][[
balanced:   [[][[[]]]]
balanced:   [[][[][]]]
balanced:   [[][[]][]]
balanced:   [[][[]]][]
unbalanced: [[][[]]]][
balanced:   [[][][[]]]
balanced:   [[][][][]]
balanced:   [[][][]][]
unbalanced: [[][][]]][
balanced:   [[][]][[]]
balanced:   [[][]][][]
unbalanced: [[][]][]][
unbalanced: [[][]]][[]
unbalanced: [[][]]][][
unbalanced: [[][]]]][[
balanced:   [[]][[[]]]
balanced:   [[]][[][]]
balanced:   [[]][[]][]
unbalanced: [[]][[]]][
balanced:   [[]][][[]]
balanced:   [[]][][][]
unbalanced: [[]][][]][
unbalanced: [[]][]][[]
unbalanced: [[]][]][][
unbalanced: [[]][]]][[
unbalanced: [[]]][[[]]
unbalanced: [[]]][[][]
unbalanced: [[]]][[]][
unbalanced: [[]]][][[]
unbalanced: [[]]][][][
unbalanced: [[]]][]][[
unbalanced: [[]]]][[[]
unbalanced: [[]]]][[][
unbalanced: [[]]]][][[
unbalanced: [[]]]]][[[
balanced:   [][[[[]]]]
balanced:   [][[[][]]]
balanced:   [][[[]][]]
balanced:   [][[[]]][]
unbalanced: [][[[]]]][
balanced:   [][[][[]]]
balanced:   [][[][][]]
balanced:   [][[][]][]
unbalanced: [][[][]]][
balanced:   [][[]][[]]
balanced:   [][[]][][]
unbalanced: [][[]][]][
unbalanced: [][[]]][[]
unbalanced: [][[]]][][
unbalanced: [][[]]]][[
balanced:   [][][[[]]]
balanced:   [][][[][]]
balanced:   [][][[]][]
unbalanced: [][][[]]][
balanced:   [][][][[]]
balanced:   [][][][][]
unbalanced: [][][][]][
unbalanced: [][][]][[]
unbalanced: [][][]][][
unbalanced: [][][]]][[
unbalanced: [][]][[[]]
unbalanced: [][]][[][]
unbalanced: [][]][[]][
unbalanced: [][]][][[]
unbalanced: [][]][][][
unbalanced: [][]][]][[
unbalanced: [][]]][[[]
unbalanced: [][]]][[][
unbalanced: [][]]][][[
unbalanced: [][]]]][[[
unbalanced: []][[[[]]]
unbalanced: []][[[][]]
unbalanced: []][[[]][]
unbalanced: []][[[]]][
unbalanced: []][[][[]]
unbalanced: []][[][][]
unbalanced: []][[][]][
unbalanced: []][[]][[]
unbalanced: []][[]][][
unbalanced: []][[]]][[
unbalanced: []][][[[]]
unbalanced: []][][[][]
unbalanced: []][][[]][
unbalanced: []][][][[]
unbalanced: []][][][][
unbalanced: []][][]][[
unbalanced: []][]][[[]
unbalanced: []][]][[][
unbalanced: []][]][][[
unbalanced: []][]]][[[
unbalanced: []]][[[[]]
unbalanced: []]][[[][]
unbalanced: []]][[[]][
unbalanced: []]][[][[]
unbalanced: []]][[][][
unbalanced: []]][[]][[
unbalanced: []]][][[[]
unbalanced: []]][][[][
unbalanced: []]][][][[
unbalanced: []]][]][[[
unbalanced: []]]][[[[]
unbalanced: []]]][[[][
unbalanced: []]]][[][[
unbalanced: []]]][][[[
unbalanced: []]]]][[[[
unbalanced: ][[[[[]]]]
unbalanced: ][[[[][]]]
unbalanced: ][[[[]][]]
unbalanced: ][[[[]]][]
unbalanced: ][[[[]]]][
unbalanced: ][[[][[]]]
unbalanced: ][[[][][]]
unbalanced: ][[[][]][]
unbalanced: ][[[][]]][
unbalanced: ][[[]][[]]
unbalanced: ][[[]][][]
unbalanced: ][[[]][]][
unbalanced: ][[[]]][[]
unbalanced: ][[[]]][][
unbalanced: ][[[]]]][[
unbalanced: ][[][[[]]]
unbalanced: ][[][[][]]
unbalanced: ][[][[]][]
unbalanced: ][[][[]]][
unbalanced: ][[][][[]]
unbalanced: ][[][][][]
unbalanced: ][[][][]][
unbalanced: ][[][]][[]
unbalanced: ][[][]][][
unbalanced: ][[][]]][[
unbalanced: ][[]][[[]]
unbalanced: ][[]][[][]
unbalanced: ][[]][[]][
unbalanced: ][[]][][[]
unbalanced: ][[]][][][
unbalanced: ][[]][]][[
unbalanced: ][[]]][[[]
unbalanced: ][[]]][[][
unbalanced: ][[]]][][[
unbalanced: ][[]]]][[[
unbalanced: ][][[[[]]]
unbalanced: ][][[[][]]
unbalanced: ][][[[]][]
unbalanced: ][][[[]]][
unbalanced: ][][[][[]]
unbalanced: ][][[][][]
unbalanced: ][][[][]][
unbalanced: ][][[]][[]
unbalanced: ][][[]][][
unbalanced: ][][[]]][[
unbalanced: ][][][[[]]
unbalanced: ][][][[][]
unbalanced: ][][][[]][
unbalanced: ][][][][[]
unbalanced: ][][][][][
unbalanced: ][][][]][[
unbalanced: ][][]][[[]
unbalanced: ][][]][[][
unbalanced: ][][]][][[
unbalanced: ][][]]][[[
unbalanced: ][]][[[[]]
unbalanced: ][]][[[][]
unbalanced: ][]][[[]][
unbalanced: ][]][[][[]
unbalanced: ][]][[][][
unbalanced: ][]][[]][[
unbalanced: ][]][][[[]
unbalanced: ][]][][[][
unbalanced: ][]][][][[
unbalanced: ][]][]][[[
unbalanced: ][]]][[[[]
unbalanced: ][]]][[[][
unbalanced: ][]]][[][[
unbalanced: ][]]][][[[
unbalanced: ][]]]][[[[
unbalanced: ]][[[[[]]]
unbalanced: ]][[[[][]]
unbalanced: ]][[[[]][]
unbalanced: ]][[[[]]][
unbalanced: ]][[[][[]]
unbalanced: ]][[[][][]
unbalanced: ]][[[][]][
unbalanced: ]][[[]][[]
unbalanced: ]][[[]][][
unbalanced: ]][[[]]][[
unbalanced: ]][[][[[]]
unbalanced: ]][[][[][]
unbalanced: ]][[][[]][
unbalanced: ]][[][][[]
unbalanced: ]][[][][][
unbalanced: ]][[][]][[
unbalanced: ]][[]][[[]
unbalanced: ]][[]][[][
unbalanced: ]][[]][][[
unbalanced: ]][[]]][[[
unbalanced: ]][][[[[]]
unbalanced: ]][][[[][]
unbalanced: ]][][[[]][
unbalanced: ]][][[][[]
unbalanced: ]][][[][][
unbalanced: ]][][[]][[
unbalanced: ]][][][[[]
unbalanced: ]][][][[][
unbalanced: ]][][][][[
unbalanced: ]][][]][[[
unbalanced: ]][]][[[[]
unbalanced: ]][]][[[][
unbalanced: ]][]][[][[
unbalanced: ]][]][][[[
unbalanced: ]][]]][[[[
unbalanced: ]]][[[[[]]
unbalanced: ]]][[[[][]
unbalanced: ]]][[[[]][
unbalanced: ]]][[[][[]
unbalanced: ]]][[[][][
unbalanced: ]]][[[]][[
unbalanced: ]]][[][[[]
unbalanced: ]]][[][[][
unbalanced: ]]][[][][[
unbalanced: ]]][[]][[[
unbalanced: ]]][][[[[]
unbalanced: ]]][][[[][
unbalanced: ]]][][[][[
unbalanced: ]]][][][[[
unbalanced: ]]][]][[[[
unbalanced: ]]]][[[[[]
unbalanced: ]]]][[[[][
unbalanced: ]]]][[[][[
unbalanced: ]]]][[][[[
unbalanced: ]]]][][[[[
unbalanced: ]]]]][[[[[
```



###  Scala Version 2

{{works with|Scala|2.10.1}}

```scala
import scala.util.Random.shuffle

object BalancedBracketsApp extends App {

  for (length <- 0 until 10) {
    val str = randomBrackets(length)
    if (is_balanced(str))
      println(s"$str - ok")
    else
      println(s"$str - NOT ok")
  }

  def randomBrackets(length: Int): String =
    shuffle(("[]" * length).toSeq).mkString

  def isBalanced(bracketString: String): Boolean = {
    var balance = 0
    for (char <- bracketString) {
      char match {
        case '[' => balance += 1
        case ']' => balance -= 1
      }
      if (balance < 0) return false;
    }
    balance == 0
  }

}
```


Alternate implementation of "isBalanced" using tail-recursion instead of var and return:


```scala
import scala.util.Random.shuffle
import scala.annotation.tailrec

  // ...

  def isBalanced(str: String): Boolean = isBalanced(str.toList, balance = 0)

  @tailrec
  def isBalanced(str: List[Char], balance: Int = 0): Boolean =
    str match {
      case _ if (balance < 0) => false
      case Nil => balance == 0
      case char :: rest =>
        val newBalance = char match {
          case '[' => balance + 1
          case ']' => balance -1
        }
        isBalanced(rest, newBalance)
    }

```


Slightly modified implementation of "isBalanced" using tail-recursion
{{works with|Scala|2.11.7}}

```scala

@scala.annotation.tailrec
final def isBalanced(
  str: List[Char],
  // accumulator|indicator|flag
  balance: Int = 0,
  options_Map: Map[Char, Int] = Map(('[' -> 1), (']' -> -1))
): Boolean = if (balance < 0) {
  // base case
  false
} else {
  if (str.isEmpty){
    // base case
    balance == 0
  } else {
    // recursive step
    isBalanced(str.tail, balance + options_Map(str.head))
  }
}

```


Sample output:

```txt

 - ok
[ - NOT ok
[] - ok
[][ - NOT ok
][][ - NOT ok
[][][ - NOT ok
[][][] - ok
[[[]][] - NOT ok
[[][][]] - ok
[[[][]][] - NOT ok

```



## Scheme


```scheme
(define (balanced-brackets string)
  (define (b chars sum)
    (cond ((and (null? chars) (= 0 sum))
           #t)
          ((null? chars)
           #f)
          ((char=? #\[ (car chars))
           (b (cdr chars) (+ sum 1)))
          ((= sum 0)
           #f)
          (else
           (b (cdr chars) (- sum 1)))))
  (b (string->list string) 0))

(balanced-brackets "")

(balanced-brackets "[]")
(balanced-brackets "[][]")
(balanced-brackets "[[][]]")

(balanced-brackets "][")
(balanced-brackets "][][")
(balanced-brackets "[]][[]")

```



## Scilab

{{trans|MATLAB}}
<lang>function varargout=isbb(s)
    st=strsplit(s);
    t=cumsum((st=='[')-(st==']'));
    balanced=and(t>=0) & t(length(t))==0;
    varargout=list(balanced)
endfunction
```

{{out}}
The following code was used to generate random strings of length 5, 16, and 22 chars. It also displays the generated string, and the output (true of false) of <code>isbb()</code>.
<lang>for j=[5 16 22]
    s=[];
    for i=1:j
        p=rand();
        if p>0.5 then
            s=s+"[";
        else
            s=s+"]";
        end
    end
    disp(s);
    x=isbb(s);
    disp(x);
end
```

Console output:

```txt
 ][]][

 F

 [[[[][[[][]]]]]]

 T

 ][[][]]]][]][[]][][]]]

 F
```



## Seed7


```seed7
$ include "seed7_05.s7i";

const func string: generateBrackets (in integer: count) is func
  result
    var string: stri is "";
  local
    var integer: index is 0;
    var integer: pos is 0;
    var char: ch is ' ';
  begin
    stri := "[" mult count & "]" mult count;
    for index range 1 to length(stri) do
      pos := rand(1, length(stri));
      ch := stri[index];
      stri @:= [index] stri[pos];
      stri @:= [pos] ch;
    end for;
  end func;

const func boolean: checkBrackets (in string: test) is func
  result
    var boolean: okay is TRUE;
  local
    var char: ch is ' ';
    var integer: open is 0;
  begin
    for ch range test do
      if ch = '[' then
        incr(open);
      elsif ch = ']' then
        if open = 0 then
          okay := FALSE;
        else
          decr(open);
        end if;
      end if;
    end for;
    okay := open = 0;
  end func;

const proc: main is func
  local
    var integer: n is 0;
    var integer: count is 0;
    var string: stri is "";
  begin
    for n range 0 to 4 do
      for count range 1 to 3 do
        stri := generateBrackets(n);
        writeln(stri <& ": " <& checkBrackets(stri));
      end for;
    end for;
  end func;
```


Output:

```txt

: TRUE
: TRUE
: TRUE
[]: TRUE
][: FALSE
][: FALSE
][[]: FALSE
[[]]: TRUE
[[]]: TRUE
[][][]: TRUE
[[][]]: TRUE
[]]][[: FALSE
[][][]][: FALSE
[][][][]: TRUE
]][][[][: FALSE

```



## Sidef


```ruby
func balanced (str) {

    var depth = 0
    str.each { |c|
           if(c=='['){ ++depth }
        elsif(c==']'){ --depth < 0 && return false }
    }

    return !depth
}

for str [']','[','[[]','][]','[[]]','[[]]]][][]]','x[ y [ [] z ]][ 1 ][]abcd'] {
    printf("%sbalanced\t: %s\n", balanced(str) ? "" : "NOT ", str)
}
```


{{out}}

```txt

NOT balanced	: ]
NOT balanced	: [
NOT balanced	: [[]
NOT balanced	: ][]
balanced	: [[]]
NOT balanced	: [[]]]][][]]
balanced	: x[ y [ [] z ]][ 1 ][]abcd

```


## Simula


```simula
BEGIN
    INTEGER U;
    U := ININT;
    BEGIN

        TEXT PROCEDURE GENERATE(N); INTEGER N;
        BEGIN
            INTEGER R;
            TEXT T;
            T :- NOTEXT;
            WHILE N > 0 DO BEGIN
                R := RANDINT(1,2,U);
                T :- T & (IF R = 1 THEN "[" ELSE "]");
                N := N - 1;
            END;
            GENERATE :- T;
        END GENERATE;

        BOOLEAN PROCEDURE BALANCED(T); TEXT T;
        BEGIN
            INTEGER LEVEL;
            CHARACTER BRACE;
            BOOLEAN DONE;
            T.SETPOS(1);
            WHILE T.MORE AND NOT DONE DO BEGIN
                BRACE := T.GETCHAR;
                IF BRACE = '[' THEN LEVEL := LEVEL + 1;
                IF BRACE = ']' THEN LEVEL := LEVEL - 1;
                IF LEVEL < 0 THEN DONE := TRUE;
            END;
            BALANCED := LEVEL = 0;
        END BALANCED;

        INTEGER I,M;
        TEXT T;
        FOR I := 1 STEP 1 UNTIL 40 DO BEGIN
            M := RANDINT(0,10,U);
            T :- GENERATE(M);
            IF BALANCED(T) THEN OUTTEXT("    ") ELSE OUTTEXT(" NOT");
            OUTTEXT(" BALANCED: ");
            OUTTEXT(T);
            OUTIMAGE;
        END;

    END;
END
```

{{in}}

```txt
710
```

{{out}}

```txt

 NOT BALANCED: [[[[[[][
 NOT BALANCED: [[[[]
 NOT BALANCED: ][
 NOT BALANCED: [][[[[
     BALANCED: [][[]]
 NOT BALANCED: [[]
 NOT BALANCED: ][
 NOT BALANCED: ]][[]
 NOT BALANCED: []]]
 NOT BALANCED: ][]][[]
 NOT BALANCED: ]]][
 NOT BALANCED: ]][
 NOT BALANCED: ][]]]]][]
 NOT BALANCED: [[][[[]][[
 NOT BALANCED: ]][]][]]
     BALANCED:
 NOT BALANCED: ][[
 NOT BALANCED: []]][[]
 NOT BALANCED: ]]]][[]]][
 NOT BALANCED: ]]
     BALANCED: [[][[[]]]]
 NOT BALANCED: ][][]]
     BALANCED:
 NOT BALANCED: [[[[[]][[]
 NOT BALANCED: []][[[][
 NOT BALANCED: []]]][][]
 NOT BALANCED: ][]][][
 NOT BALANCED: []]
 NOT BALANCED: ]]][[
 NOT BALANCED: [
     BALANCED: []
 NOT BALANCED: ][]]]
 NOT BALANCED: [[[[[[]][
 NOT BALANCED: [][[][
 NOT BALANCED: ]]
 NOT BALANCED: ]][
 NOT BALANCED: [[[[[[
 NOT BALANCED: ]]]]]
 NOT BALANCED: ]][[]
 NOT BALANCED: ][][][][

```



## Standard ML

{{works with|PolyML}}


```sml
fun isBalanced s = checkBrackets 0 (String.explode s)
and checkBrackets 0 [] = true
  | checkBrackets _ [] = false
  | checkBrackets ~1 _ = false
  | checkBrackets counter (#"["::rest) = checkBrackets (counter + 1) rest
  | checkBrackets counter (#"]"::rest) = checkBrackets (counter - 1) rest
  | checkBrackets counter (_::rest) = checkBrackets counter rest
```


An example of usage


```sml
val () =
    List.app print
        (List.map
            (* Turn `true' and `false' to `OK' and `NOT OK' respectively *)
            (fn s => if isBalanced s
                then s ^ "\t\tOK\n"
                else s ^ "\t\tNOT OK\n"
            )
            (* A set of strings to test *)
            ["", "[]", "[][]", "[[][]]", "][", "][][", "[]][[]"]
        )
```


Output:

```txt

		OK
[]		OK
[][]		OK
[[][]]		OK
][		NOT OK
][][		NOT OK
[]][[]		NOT OK

```



## Stata



```stata
mata
function random_brackets(n) {
	return(invtokens(("[","]")[runiformint(1,2*n,1,2)],""))
}

function is_balanced(s) {
	n = strlen(s)
	if (n==0) return(1)
	a = runningsum(92:-ascii(s))
	return(all(a:>=0) & a[n]==0)
}
end
```


'''Test'''

```txt
: is_balanced("")
  1

: is_balanced("[]")
  1

: is_balanced("[][]")
  1

: is_balanced("[[][]]")
  1

: is_balanced("][")
  0

: is_balanced("][][")
  0

: is_balanced("[]][[]")
```



## Swift


Checks balance function:


```swift
import Foundation

func isBal(str: String) -> Bool {

  var count = 0

  return !str.characters.contains { ($0 == "["  ? ++count : --count) < 0 } && count == 0

}

```
output:
```swift

isBal("[[[]]]") // true

isBal("[]][[]") // false


```
Random Bracket function:
```swift


func randBrack(n: Int) -> String {

  var bracks: [Character] = Array(Repeat(count: n, repeatedValue: "["))

  for i in UInt32(n+1)...UInt32(n + n) {

    bracks.insert("]", atIndex: Int(arc4random_uniform(i)))

  }

  return String(bracks)

}


```
output:
```swift


randBrack(2) // "]][["


```
Random check balance function:
```swift


func randIsBal(n: Int) {

  let (bal, un) = ("", "un")

  for str in (1...n).map(randBrack) {

    print("\(str) is \(isBal(str) ? bal : un)balanced\n")

  }
}

randIsBal(4)


```
output:
```swift


//    ][ is unbalanced
//
//    ]][[ is unbalanced
//
//    []][[] is unbalanced
//
//    [][][[]] is balanced
```



## Tcl


```tcl
proc generate {n} {
    if {!$n} return
    set l [lrepeat $n "\[" "\]"]
    set len [llength $l]
    while {$len} {
	set tmp [lindex $l [set i [expr {int($len * rand())}]]]
	lset l $i [lindex $l [incr len -1]]
	lset l $len $tmp
    }
    return [join $l ""]
}

proc balanced s {
    set n 0
    foreach c [split $s ""] {
	# Everything unmatched is ignored, which is what we want
	switch -exact -- $c {
	    "\[" {incr n}
	    "\]" {if {[incr n -1] < 0} {return false}}
	}
    }
    expr {!$n}
}

for {set i 0} {$i < 15} {incr i} {
    set s [generate $i]
    puts "\"$s\"\t-> [expr {[balanced $s] ? {OK} : {NOT OK}}]"
}
```

Sample output:

```txt

""	-> OK
"]["	-> NOT OK
"]][["	-> NOT OK
"]]][[["	-> NOT OK
"[][][[]]"	-> OK
"[[[][[]]]]"	-> OK
"[][][[][]][]"	-> OK
"[[]][]]]][[[]["	-> NOT OK
"][][[][][][[]]]["	-> NOT OK
"][[[][]][]]][][[]["	-> NOT OK
"]][][]][[][[][[]][]["	-> NOT OK
"[[[][[][]]][]]][[]]][["	-> NOT OK
"[[]]][]][[[[]]][[][][[]]"	-> NOT OK
"][[][][]][[[]][[[[][]]]][]"	-> NOT OK
"]][[][[][[[[]][[][]][[]]]]]["	-> NOT OK

```


### Constructing correctly balanced strings

It is, of course, possible to directly construct such a balanced string, this being much more useful as the length of the string to generate grows longer. This is done by conceptually building a random tree (or forest) and then walking the tree, with open brackets being appended when a node is entered from its root and close brackets being appended when a node is left for its root. This is equivalent to inserting a balanced pair of brackets at a random place in an initially-empty string <math>n</math> times, which might be done like this:

```tcl
proc constructBalancedString {n} {
    set s ""
    for {set i 0} {$i < $n} {incr i} {
	set x [expr {int(rand() * ([string length $s] + 1))}]
	set s "[string range $s 0 [expr {$x-1}]]\[\][string range $s $x end]"
    }
    return $s
}
```

As noted, because the generated string is guaranteed to be balanced, it requires no further filtering and this results in much more efficient generation of balanced strings at longer lengths (because there's no need to backtrack).


## TUSCRIPT


```tuscript

$$ MODE TUSCRIPT

SECTION gen_brackets
values="[']",brackets=""
LOOP n=1,12
 brackets=APPEND (brackets,"","~")
 LOOP m=1,n
 a=RANDOM_NUMBERS (1,2,1),br=SELECT(values,#a)
 brackets=APPEND(brackets,"",br)
 b=RANDOM_NUMBERS (1,2,1),br=SELECT(values,#b)
 brackets=APPEND(brackets,"",br)
 ENDLOOP
ENDLOOP
brackets=SPLIT (brackets,":~:")
ENDSECTION

MODE DATA
$$ BUILD X_TABLE brackets=*
 [[[[ (4 ]]]] )4
 [[[ (3 ]]] )3
 [[ (2 ]] )2
 [ (1 ] )1

$$ MODE TUSCRIPT
DO gen_brackets
LOOP b=brackets
status=CHECK_BRACKETS (b,brackets,a1,e1,a2,e2)
PRINT b," ",status
ENDLOOP

```

Output:

```txt

 OK
]] ERROR
[[]] OK
[][[]] OK
[]]]][[] ERROR
[]][]][][[ ERROR
[[]][][]]]]] ERROR
[]][[][[]][[][ ERROR
[][[[][[[[[[]][[ ERROR
]]][[]][]][[[][][[ ERROR
][][[]]][[[[[]]][[][ ERROR
[[[][][]][]]]][[[[[[]] ERROR
][[[]][[][[[[[[[[[[[]]]] ERROR

```



## TXR



```txr
@(define paren)@(maybe)[@(coll)@(paren)@(until)]@(end)]@(end)@(end)
@(do (defvar r (make-random-state nil))

     (defun generate-1 (count)
       (let ((bkt (repeat "[]" count)))
         (cat-str (shuffle bkt))))

     (defun generate-list (num count)
       [[generate tf (op generate-1 count)] 0..num]))
@(next :list @(generate-list 22 6))
@(output)
INPUT           MATCHED         REST
@(end)
@  (collect)
@    (all)
@parens
@    (and)
@{matched (paren)}@mismatched
@    (end)
@  (output)
@{parens 15} @{matched 15} @{mismatched 15}
@  (end)
@(end)
```


The recursive pattern function <code>@(paren)</code> gives rise to a grammar which matches parentheses:


```txt
@(define paren)@(maybe)[@(coll)@(paren)@(until)]@(end)]@(end)@(end)
```


A string of balanced parentheses is an optional unit (<code>@(maybe) ... @(end)</code>) that begins with <code>[</code>, followed by zero or more such balanced strings, followed by <code>]</code>.

Sample run:


```txt
$ ./txr paren.txr
INPUT           MATCHED         REST
][[[]][][[]]                    ][[[]][][[]]
[]][[]][][[]    []              ][[]][][[]
[][[[[]]]]][    []              [[[[]]]]][
][[][[]]][][                    ][[][[]]][][
[[[][[]]][]]    [[[][[]]][]]
]][]][[[][[]                    ]][]][[[][[]
[[]][]][[[]]    [[]]            []][[[]]
]][]][]][[[[                    ]][]][]][[[[
]][[]]][][[[                    ]][[]]][][[[
]]]][[]][[[[                    ]]]][[]][[[[
][[[[][[]]]]                    ][[[[][[]]]]
][]][]]][[[[                    ][]][]]][[[[
]][][[][][[]                    ]][][[][][[]
]][][]][[][[                    ]][][]][[][[
[][[]][]]][[    []              [[]][]]][[
[[]]]]][[[[]    [[]]            ]]][[[[]
]][[[[[[]]]]                    ]][[[[[[]]]]
][][][[[]][]                    ][][][[[]][]
[]][]][][][[    []              ][]][][][[
]][[[][]][[]                    ]][[[][]][[]
][[[[]]]][][                    ][[[[]]]][][
[[]]]]][[][[    [[]]            ]]][[][[
```



## UNIX Shell

{{works with|bash}}

```bash
generate() {
    local b=()
    local i j tmp
    for ((i=1; i<=$1; i++)); do
        b+=( '[' ']')
    done
    for ((i=${#b[@]}-1; i>0; i--)); do
        j=$(rand $i)
        tmp=${b[j]}
        b[j]=${b[i]}
        b[i]=$tmp
    done
    local IFS=
    echo "${b[*]}"
}

# a random number in the range [0,n)
rand() {
    echo $(( $RANDOM % $1 ))
}

balanced() {
    local -i lvl=0
    local i
    for ((i=0; i<${#1}; i++)); do
        case ${1:i:1} in
            '[') ((lvl++));;
            ']') (( --lvl < 0 )) && return 1;;
        esac
    done
    (( lvl == 0 )); return $?
}

for ((i=0; i<=10; i++)); do
    test=$(generate $i)
    balanced "$test" && result=OK || result="NOT OK"
    printf "%s\t%s\n" "$test" "$result"
done
```


{{output}}

```txt
	OK
][	NOT OK
[]][	NOT OK
[[][]]	OK
]][[]][[	NOT OK
[[]][[][]]	OK
[]][[[[]]]][	NOT OK
[[]][]][]][[[]	NOT OK
[][][[[[][][]]]]	OK
[][][[[[]]][[][]]]	OK
][[]][][][[[]]][[]][	NOT OK
```



## Ursala



```Ursala
#import std
#import nat

balanced = @NiX ~&irB->ilZB ~&rh?/~&lbtPB ~&NlCrtPX

#cast %bm

main = ^(2-$'[]'*,balanced)* eql@ZFiFX*~ iota64
```

output:

```txt
<
   '': true,
   '[]': true,
   '][[]': false,
   '[][]': true,
   '[[]]': true,
   ']][[[]': false,
   '][][[]': false,
   '[]][[]': false,
   '][[][]': false,
   '[][][]': true,
   '[[]][]': true,
   '][[[]]': false,
   '[][[]]': true,
   '[[][]]': true,
   '[[[]]]': true>
```


## VBA


```vb

Public Function checkBrackets(s As String) As Boolean
'function checks strings for balanced brackets
Dim Depth As Integer
Dim ch As String * 1

Depth = 0
For i = 1 To Len(s)
  ch = Mid$(s, i, 1)
  If ch = "[" Then Depth = Depth + 1
  If ch = "]" Then
    If Depth = 0 Then 'not balanced
      checkBrackets = False
      Exit Function
    Else
      Depth = Depth - 1
    End If
  End If
Next
checkBrackets = (Depth = 0)
End Function

Public Function GenerateBrackets(N As Integer) As String
'generate a string with N opening and N closing brackets in random order
Dim s As String
Dim N2 As Integer, j As Integer
Dim Brackets() As String * 1
Dim temp As String * 1

'catch trivial value
If N <= 0 Then
  GenerateBrackets = ""
  Exit Function
End If

N2 = N + N
ReDim Brackets(1 To N2)
For i = 1 To N2 Step 2
 Brackets(i) = "["
 Brackets(i + 1) = "]"
Next i
'shuffle.
For i = 1 To N2
  j = 1 + Int(Rnd() * N2)
  'swap brackets i and j
  temp = Brackets(i)
  Brackets(i) = Brackets(j)
  Brackets(j) = temp
Next i
'generate string
s = ""
For i = 1 To N2
  s = s & Brackets(i)
Next i
GenerateBrackets = s
End Function

Public Sub BracketsTest()
Dim s As String
Dim i As Integer

For i = 0 To 10
 s = GenerateBrackets(i)
 Debug.Print """" & s & """: ";
 If checkBrackets(s) Then Debug.Print " OK" Else Debug.Print " Not OK"
Next
End Sub

```


sample output:

```txt

BracketsTest
"":  OK
"][":  Not OK
"[][]":  OK
"][[]][":  Not OK
"][]][[[]":  Not OK
"[[][]]][[]":  Not OK
"]][[[[[]]]][":  Not OK
"[[[]][][][][]]":  OK
"]][[[]][[[]][]][":  Not OK
"[[][]][]]][[[[]][]":  Not OK
"]][][[[][]]][][[][][":  Not OK

```

{{omit from|GUISS}}


## VBScript


```vb
For n = 1 To 10
	sequence = Generate_Sequence(n)
	WScript.Echo sequence & " is " & Check_Balance(sequence) & "."
Next

Function Generate_Sequence(n)
	For i = 1 To n
		j = Round(Rnd())
		If j = 0 Then
			Generate_Sequence = Generate_Sequence & "["
		Else
			Generate_Sequence = Generate_Sequence & "]"
		End If
	Next
End Function

Function Check_Balance(s)
	Set Stack = CreateObject("System.Collections.Stack")
	For i = 1 To Len(s)
		char = Mid(s,i,1)
		If i = 1 Or char = "[" Then
			Stack.Push(char)
		ElseIf Stack.Count <> 0 Then
			If char = "]" And Stack.Peek = "[" Then
				Stack.Pop
			End If
		Else
			Stack.Push(char)
		End If
	Next
	If Stack.Count > 0 Then
		Check_Balance = "Not Balanced"
	Else
		Check_Balance = "Balanced"
	End If
End Function
```


{{out}}
Note:  For some reason, the function to generate the bracket sequence, Generate_Sequence, does not produce a balanced one.  But the
	function to check if it is balanced or not, Check_Balance, works if a balanced argument is passed manually.

```txt
] is Not Balanced.
]] is Not Balanced.
[[] is Not Balanced.
[]]] is Not Balanced.
[[]][ is Not Balanced.
]][][] is Not Balanced.
][][[]] is Not Balanced.
[[]]]]][ is Not Balanced.
]][][]][] is Not Balanced.
[[][[[[[]] is Not Balanced.
```



## Visual Basic .NET


```vbnet
Module Module1

    Private rand As New Random

    Sub Main()
        For numInputs As Integer = 1 To 10 '10 is the number of bracket sequences to test.
            Dim input As String = GenerateBrackets(rand.Next(0, 5)) '5 represents the number of pairs of brackets (n)
            Console.WriteLine(String.Format("{0} : {1}", input.PadLeft(10, CChar(" ")), If(IsBalanced(input) = True, "OK", "NOT OK")))
        Next
        Console.ReadLine()
    End Sub

    Private Function GenerateBrackets(n As Integer) As String

        Dim randomString As String = ""
        Dim numOpen, numClosed As Integer

        Do Until numOpen = n And numClosed = n
            If rand.Next(0, 501) Mod 2 = 0 AndAlso numOpen < n Then
                randomString = String.Format("{0}{1}", randomString, "[")
                numOpen += 1
            ElseIf rand.Next(0, 501) Mod 2 <> 0 AndAlso numClosed < n Then
                randomString = String.Format("{0}{1}", randomString, "]")
                numClosed += 1
            End If
        Loop
        Return randomString
    End Function

    Private Function IsBalanced(brackets As String) As Boolean

        Dim numOpen As Integer = 0
        Dim numClosed As Integer = 0

        For Each character As Char In brackets
            If character = "["c Then numOpen += 1
            If character = "]"c Then
                numClosed += 1
                If numClosed > numOpen Then Return False
            End If
        Next
        Return numOpen = numClosed
    End Function
End Module
```


{{out}}

```txt

  ][[][]][ : NOT OK
  []]][[[] : NOT OK
    [[]][] : OK
        [] : OK
    [[[]]] : OK
        [] : OK
    []][][ : NOT OK
    ]][[[] : NOT OK
           : OK
        [] : OK

```



## X86 Assembly


```X86Assembly

section .data

MsgBalanced: db "OK", 10
MsgBalancedLen: equ 3

MsgUnbalanced: db "NOT OK", 10
MsgUnbalancedLen: equ 7

MsgBadInput: db "BAD INPUT", 10
MsgBadInputLen: equ 10

Open: equ '['
Closed: equ ']'

section .text

BalancedBrackets:

  xor rcx, rcx
  mov rsi, rdi
  cld

  processBracket:
    lodsb
    cmp al, 0
    je determineBalance

    cmp al, Open
    je processOpenBracket

    cmp al, Closed
    je processClosedBracket

    mov rsi, MsgBadInput
    mov rdx, MsgBadInputLen
    jmp displayResult

    processOpenBracket:
      add rcx, 1
      jmp processBracket

    processClosedBracket:
      cmp rcx, 0
      je unbalanced

      sub rcx, 1
      jmp processBracket


  determineBalance:
    cmp rcx, 0
    jne unbalanced

    mov rsi, MsgBalanced
    mov rdx, MsgBalancedLen
    jmp displayResult

  unbalanced:
    mov rsi, MsgUnbalanced
    mov rdx, MsgUnbalancedLen

  displayResult:
    mov rax, 1
    mov rdi, 1
    syscall
    ret

```



## XPL0


```XPL0
include c:\cxpl\codes;       \intrinsic code declarations

int     N, I, C, Nest;
char    Str;
[\Generate a string with N open brackets and N close brackets in arbitrary order
N:= IntIn(0);                           \get number of brackets/2 from keyboard
Str:= Reserve(2*N);
for I:= 0 to 2*N-1 do Str(I):= ^[;
C:= 0;                                  \initialize count of "]"
repeat  I:= Ran(2*N);                   \change N random locations to "]"
        if Str(I) # ^] then [Str(I):= ^]; C:= C+1];
until   C>=N;

\Determine whether string consists of nested pairs of open/close brackets
I:= 0;  C:= 0;  Nest:= false;
while I<2*N do
        [if Str(I) = ^[ then C:= C+1 else C:= C-1;
        if C<0 then Nest:= true;
        ChOut(0,Str(I));
        I:= I+1;
        ];
ChOut(0,9\tab\);
if Nest then Text(0,"NOT ");
Text(0,"OK
");
]
```


Example output:

```txt

2
[]][    NOT OK
[][]    OK
3
[[][]]  OK

```




## Ya


```Ya
@Balanced[]s // each source must be started by specifying its file name; std extension .Ya could be ommitted and auto added by compiler

// all types are prefixed by `
// definition of anything new is prefixed by \, like \MakeNew_[]s and \len
// MakeNew_[]s is Ok ident in Ya: _ starts sign part of ident, which must be ended by _ or alphanum
`Char[^] \MakeNew_[]s(`Int+ \len) // `Char[^] and `Char[=] are arrays that owns their items, like it's usally in other langs;
	// yet in assignment of `Char[^] to `Char[=] the allocated memory is moved from old to new owner, and old string becomes empty
	// there are tabs at starts of many lines; these tabs specify what in C++ is {} blocks, just like in Python
	len & 1 ==0 ! // it's a call to postfix function '!' which is an assert: len must be even
	`Char[=] \r(len) // allocate new string of length len
	// most statements are analogous to C++ but written starting by capital letter: For If Switch Ret
	For `Char[] \eye = r; eye // // `Char[] is a simplest array of chars, which does not hold a memory used by array items; inc part of For loop is missed: it's Ok, and condition and init could also be missed
		*eye++ = '['; *eye++ = '[' // fill r by "[][][]...". The only place with ; as statement delemiter: required because the statement is not started at new line.
	// below is a shuffle of "[][][]..." array
	For `Char[] \eye = r; ++eye // var eye is already defined, but being the same `Char[] it's Ok by using already exisiting var. ++eye is used: it allows use of eye[-1] inside
		`Int+ \at = Random(eye/Length) // `Int+ is C++'s unsigned int. eye/Length: / is used for access to field, like in file path
		eye[-1],eye[at] = eye[at],eye[-1] // swap using tuples; eye[-1] accesses char that is out of current array, yet it's allowed
	Ret r // Ret is C's return
`Bool \AreBalanced(`Char[] \brackets)
	`Int+ \extra = 0
	For ;brackets ;++brackets
		Switch *brackets
			'[' // it's a C++'s 'case': both 'case' and ':' are skipped being of no value; but the code for a case should be in block, which is here specifyed by tabs at next line start
				++extra
			']'
				If !!extra // '!!' is `Bool not, like all other `Bool ops: && || ^^
					Ret No // No and False are C's false; Yes and True are C's true
				--extra
		// There is no default case, which is written as ':' - so if no case is Ok then it will fail just like if being written as on the next line
		//	: { 0! } // C's default: assert(0);
	Ret extra == 0
// function ala 'main' is not used: all global code from all modules are executed; so below is what typically is in ala 'main'
For `Int \n=10; n; --n
	// below note that new var 'brackets' is created inside args of func call
	//@Std/StdIO/ is used here to use Print function; else it maybe changed to Use @Std/StdIO at global level before this For loop
	@Std/StdIO/Print(; "%s : %s\n" ;`Char[=] \brackets = MakeNew_[]s(10) /* all bracket strings are of length 10 */; AreBalanced(brackets) ? "Ok" : "bad")
	// note that starting arg of Print is missed by using ';' - default arg value is allowed to use for any arg, even if next args are written
```



## Yabasic


```Yabasic
sub check_brackets(s$)
    local level, i

    for i = 1 to len(s$)
        switch mid$(s$, i, 1)
            case "[": level = level + 1 : break
            case "]": level = level - 1 : if level < 0 break 2
        end switch
    next i
    return level = 0
end sub

s$ = "[[]][]"

print s$, " = ";

if not check_brackets(s$) print "not ";
print "ok"
```



## zkl


```zkl
fcn bb(bs){ while(a:=bs.span("[","]")) {bs=bs[a[1],*]} (Void!=a) }
```

The span method finds the start and length of a balanced span. This algorithm assumes the string only contains brackets; a matched span is chopped off the front of the string and a new balanced span is searched for. Stops when the string is empty or unbalanced (span returns Void).

```txt

zkl: bb("")
True
zkl: bb("[]")
True
zkl: bb("[][]")
True
zkl: bb("[[][]]")
True
zkl: bb("][")
False
zkl: bb("][][")
False
zkl: bb("[]][[]")
False

```



## ZX Spectrum Basic

{{trans|AWK}}

```zxbasic
10 FOR n=1 TO 7
20 READ s$
25 PRINT "The sequence ";s$;" is ";
30 GO SUB 1000
40 NEXT n
50 STOP
1000 LET s=0
1010 FOR k=1 TO LEN s$
1020 LET c$=s$(k)
1030 IF c$="[" THEN LET s=s+1
1040 IF c$="]" THEN LET s=s-1
1050 IF s<0 THEN PRINT "Bad!": RETURN
1060 NEXT k
1070 IF s=0 THEN PRINT "Good!": RETURN
1090 PRINT "Bad!"
1100 RETURN
2000 DATA "[]","][","][][","[][]","[][][]","[]][[]","[[[[[]]]]][][][]][]["

```

