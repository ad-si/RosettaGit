+++
title = "Greatest element of a list"
description = ""
date = 2019-10-18T10:33:13Z
aliases = []
[extra]
id = 2865
[taxonomies]
categories = ["task", "Arithmetic operations"]
tags = []
languages = [
  "8th",
  "acl2",
  "actionscript",
  "ada",
  "aime",
  "algol_68",
  "algol_w",
  "antlang",
  "apl",
  "applescript",
  "applesoft_basic",
  "arm_assembly",
  "arturo",
  "autohotkey",
  "awk",
  "axe",
  "bacon",
  "basic",
  "batch_file",
  "bbc_basic",
  "bc",
  "befunge",
  "bracmat",
  "brat",
  "burlesque",
  "c",
  "clojure",
  "cmake",
  "cobol",
  "coffeescript",
  "coldfusion",
  "common_lisp",
  "component_pascal",
  "cpp",
  "csharp",
  "d",
  "dart",
  "dc",
  "dcl",
  "delphi",
  "dyalect",
  "e",
  "echolisp",
  "ecl",
  "efene",
  "eiffel",
  "ela",
  "elena",
  "elixir",
  "emacs_lisp",
  "erlang",
  "erre",
  "euler_math_toolbox",
  "euphoria",
  "excel",
  "factor",
  "fancy",
  "fantom",
  "forth",
  "fortran",
  "freebasic",
  "frink",
  "funl",
  "futhark",
  "gap",
  "go",
  "golfscript",
  "groovy",
  "haskell",
  "hexiscript",
  "hicest",
  "hoon",
  "i",
  "j",
  "java",
  "javascript",
  "jq",
  "julia",
  "k",
  "klong",
  "kotlin",
  "lasso",
  "lfe",
  "liberty_basic",
  "lingo",
  "livecode",
  "logo",
  "logtalk",
  "lua",
  "m2000_interpreter",
  "maple",
  "matlab",
  "maxima",
  "maxscript",
  "metafont",
  "min",
  "montilang",
  "mumps",
  "neko",
  "nemerle",
  "netrexx",
  "newlisp",
  "nial",
  "nim",
  "objeck",
  "ocaml",
  "octave",
  "oforth",
  "ol",
  "oorexx",
  "oz",
  "pari_gp",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "php",
  "picolisp",
  "pl_i",
  "postscript",
  "powerbasic",
  "powershell",
  "prolog",
  "purebasic",
  "python",
  "q",
  "r",
  "racket",
  "rapidq",
  "rascal",
  "raven",
  "rebol",
  "red",
  "rexx",
  "ring",
  "ruby",
  "run_basic",
  "rust",
  "scala",
  "scheme",
  "seed7",
  "self",
  "sidef",
  "slate",
  "smalltalk",
  "snobol4",
  "standard_ml",
  "stata",
  "swift",
  "tailspin",
  "tcl",
  "trith",
  "tuscript",
  "ubasic_4th",
  "unix_shell",
  "ursa",
  "ursala",
  "v",
  "vba",
  "vbscript",
  "vim_script",
  "visual_basic",
  "wart",
  "wdte",
  "wortel",
  "xpl0",
  "xslt",
  "yabasic",
  "yacas",
  "yorick",
  "zkl",
  "zx_spectrum_basic",
]
+++

## Task

Create a function that returns the maximum value in a provided set of values,

where the number of values may not be known until run-time.





## 8th


```forth

[ 1.0, 2.3, 1.1, 5.0, 3, 2.8, 2.01, 3.14159 ] ' n:max 0 a:reduce . cr

```

Output: 5


## ACL2


```Lisp
(defun maximum (xs)
   (if (endp (rest xs))
       (first xs)
       (max (first xs)
            (maximum (rest xs)))))
```



## ActionScript


```ActionScript
function max(... args):Number
{
	var curMax:Number = -Infinity;
	for(var i:uint = 0; i < args.length; i++)
		curMax = Math.max(curMax, args[i]);
	return curMax;
}
```



## Ada

The keys for this task are initializing the compared value to the 'First value of the element type, and use of an unconstrained array type.

```ada
with Ada.Text_Io;

procedure Max_Test isco
   -- substitute any array type with a scalar element
   type Flt_Array is array (Natural range <>) of Float;

   -- Create an exception for the case of an empty array
   Empty_Array : Exception;

   function Max(Item : Flt_Array) return Float is
      Max_Element : Float := Float'First;
   begin
      if Item'Length = 0 then
         raise Empty_Array;
      end if;

      for I in Item'range loop
         if Item(I) > Max_Element then
            Max_Element := Item(I);
         end if;
      end loop;
      return Max_Element;
   end Max;

   Buf : Flt_Array := (-275.0, -111.19, 0.0, -1234568.0, 3.14159, -3.14159);
begin
   Ada.Text_IO.Put_Line(Float'Image(Max(Buf)));
end Max_Test;
```

A generic function Max to deal with any floating-point type.

```ada
generic
   type Item is digits <>;
   type Items_Array is array (Positive range <>) of Item;
function Generic_Max (List : Items_Array) return Item;
```

Implementation of:

```ada
function Generic_Max (List : Items_Array) return Item is
   Result : Item := List (List'First);
begin
   for Index in List'First + 1..List'Last loop
      Result := Item'Max (Result, List (Index));
   end loop;
   return Result;
end Generic_Max;
```

When the argument array is empty, Constraint_Error exception is propagated, because array indexing is checked in Ada. Note also use of the floating-type attribute Max.


## Aime


```aime
integer
lmax(list l)
{
    integer max, x;

    max = l[0];

    for (, x in l) {
        if (max < x) {
            max = x;
        }
    }

    max;
}
```

or

```aime
integer
lmax(list l)
{
    integer max;

    max = l[0];
    l.ucall(max_i, 1, max);

    max;
}
```



## ALGOL 68

```algol68
# substitute any array type with a scalar element #
MODE FLT = REAL;

# create an exception for the case of an empty array #
PROC raise empty array = VOID:(
  GO TO except empty array
);

PROC max = ([]FLT item)FLT:
BEGIN
   IF LWB item > UPB item THEN
      raise empty array; SKIP
   ELSE
     FLT max element := item[LWB item];

     FOR i FROM LWB item + 1 TO UPB item DO
       IF item[i] > max element THEN
         max element := item[i]
       FI
     OD;
     max element
   FI
END # max #;

test:(
  []FLT buf = (-275.0, -111.19, 0.0, -1234568.0, pi, -pi);
  print((max(buf),new line)) EXIT
  except empty array:
    SKIP
)
```

```txt

+3.14159265358979e  +0

```



## ALGOL W


```algolw
begin
    % simple list type                                                       %
    record IntList( integer val; reference(IntList) next );

    % find the maximum element of an IntList, returns 0 for an empty list    %
    integer procedure maxElement( reference(IntList) value list ) ;
        begin
            integer maxValue;
            reference(IntList) listPos;
            maxValue := 0;
            listPos  := list;
            if listPos not = null then begin
                % non-empty list                                             %
                maxValue := val(listPos);
                listPos  := next(listPos);
                while listPos not = null do begin
                    if val(listPos) > maxValue then maxValue := val(listPos);
                    listPos := next(listPos)
                end while_listPos_ne_null ;
            end if_listPos_ne_null ;
            maxValue
        end maxElement ;

    % test the maxElement procedure                                          %
    write( maxElement( IntList( -767, IntList( 2397, IntList( 204, null ) ) ) ) )

end.
```

```txt

          2397

```



## AntLang


```AntLang
max|range[10]
```



## APL


```apl
LIST←2 4 6 3 8
⌈/LIST
```

```txt
8
```



## AppleScript


```AppleScript

max({1, 2, 3, 4, 20, 6, 11, 3, 9, 7})

on max(aList)
	set _curMax to first item of aList
	repeat with i in (rest of aList)
		if i > _curMax then set _curMax to contents of i
	end repeat
	return _curMax
end max

```



To find the greatest elements of lists which may contain data types other than numbers, we can write a more generic '''maximumBy''' function, which returns the maximum value from an array containing a series of any consistent data type, and which takes a type-specific comparison function as an argument.

```AppleScript

-- maximumByMay :: (a -> a -> Ordering) -> [a] -> Maybe a
on maximumByMay(f, xs)
    set cmp to mReturn(f)
    script max
        on |λ|(a, b)
            if cmp's |λ|(a, b) < 0 then
                b
            else
                a
            end if
        end |λ|
    end script

    foldl1May(max, xs)
end maximumByMay

-- TEST -----------------------------------------------------------------------
on run

    set lstWords to ["alpha", "beta", "gamma", "delta", "epsilon", ¬
        "zeta", "eta", "theta", "iota", "kappa", "|λ|", "mu"]

    set lstCities to [{name:"Shanghai", population:24.15}, ¬
        {name:"Karachi", population:23.5}, ¬
        {name:"Beijing", population:21.5}, ¬
        {name:"Tianjin", population:14.7}, ¬
        {name:"Istanbul", population:14.4}, ¬
        {name:"Lagos", population:13.4}, ¬
        {name:"Tokyo", population:13.3}]

    script population
        on |λ|(x)
            population of x
        end |λ|
    end script


    return catMaybes({¬
        maximumByMay(comparing(|length|), lstWords), ¬
        maximumByMay(comparing(|length|), {}), ¬
        maximumByMay(comparing(population), lstCities)})

    --> {"epsilon", {name:"Shanghai", population:24.15}}

end run


-- GENERIC FUNCTIONS ----------------------------------------------------------

-- catMaybes :: [Maybe a] -> [a]
on catMaybes(mbs)
    script emptyOrListed
        on |λ|(m)
            if nothing of m then
                {}
            else
                {just of m}
            end if
        end |λ|
    end script
    concatMap(emptyOrListed, mbs)
end catMaybes

-- comparing :: (a -> b) -> (a -> a -> Ordering)
on comparing(f)
    set mf to mReturn(f)
    script
        on |λ|(a, b)
            set x to mf's |λ|(a)
            set y to mf's |λ|(b)
            if x < y then
                -1
            else
                if x > y then
                    1
                else
                    0
                end if
            end if
        end |λ|
    end script
end comparing

-- concatMap :: (a -> [b]) -> [a] -> [b]
on concatMap(f, xs)
    set acc to {}
    tell mReturn(f)
        repeat with x in xs
            set acc to acc & |λ|(contents of x)
        end repeat
    end tell
    return acc
end concatMap

-- foldl1May :: (a -> a -> a) -> [a] -> Maybe a
on foldl1May(f, xs)
    set lng to length of xs
    if lng > 0 then
        if lng > 1 then
            tell mReturn(f)
                set v to item 1 of xs
                set lng to length of xs
                repeat with i from 2 to lng
                    set v to |λ|(v, item i of xs, i, xs)
                end repeat
                return just(v)
            end tell
        else
            just(item 1 of xs)
        end if
    else
        nothing("Empty list")
    end if
end foldl1May

-- just :: a -> Just a
on just(x)
    {nothing:false, just:x}
end just

-- length :: [a] -> Int
on |length|(xs)
    length of xs
end |length|

-- max :: Ord a => a -> a -> a
on max(x, y)
    if x > y then
        x
    else
        y
    end if
end max

-- nothing :: () -> Nothing
on nothing(msg)
    {nothing:true, msg:msg}
end nothing

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

```AppleScript
{"epsilon", {name:"Shanghai", population:24.15}}
```



## Applesoft BASIC


```ApplesoftBASIC
 100 REMMAX
 110 R$ = "":E$ = ""
 120 L =  LEN (L$)
 130 IF L = 0 THEN  RETURN
 140 FOR I = 1 TO L
 150     C$ =  MID$ (L$,I,1)
 160     SP = C$ = " "
 170     IF SP THEN  GOSUB 200
 180     E$ = E$ + C$
 190 NEXT I
 200 C$ = ""
 210 IF E$ = "" THEN  RETURN
 220 V =  VAL (E$):V$ = R$
 230 E$ = "":E = V$ = ""
 240 IF E AND V = 0 THEN  RETURN
 250 R$ =  STR$ (V)
 260 IF E THEN  RETURN
 270 R =  VAL (V$)
 280 IF R < V THEN  RETURN
 290 R$ = V$: RETURN
```


```ApplesoftBASIC
L$ = "1 2 3 4 20 6 11 3 9 7"
GOSUB 100MAX
PRINT R$
```

```txt
20
```



## ARM Assembly

```ARM Assembly


/* ARM assembly Raspberry PI  */
/*  program rechMax.s   */

/* Constantes    */
.equ STDOUT, 1     @ Linux output console
.equ EXIT,   1     @ Linux syscall
.equ WRITE,  4     @ Linux syscall

/*********************************/
/* Initialized data              */
/*********************************/
.data
szMessResult:  .ascii "Max number is = "      @ message result
sMessValeur:   .fill 12, 1, ' '
                  .ascii " rank = "
sMessRank:   .fill 12, 1, ' '
                  .ascii " address (hexa) = "
sMessAddress:   .fill 12, 1, ' '
                   .asciz "\n"

tTableNumbers:    .int   50
                      .int 12
                      .int -1000
                      .int 40
                      .int 255
                      .int 60
                      .int 254
.equ NBRANKTABLE,   (. - tTableNumbers) / 4  @ number table posts

/*********************************/
/* UnInitialized data            */
/*********************************/
.bss
/*********************************/
/*  code section                 */
/*********************************/
.text
.global main
main:                @ entry of program
    push {fp,lr}      @ saves 2 registers

    ldr r1,iAdrtTableNumbers
    mov r2,#0
    ldr r4,[r1,r2,lsl #2]
    mov r3,r2
    add r2,#1
1:
    cmp r2,#NBRANKTABLE
	bge 2f
	ldr r0,[r1,r2,lsl #2]
	cmp r0,r4
	movgt r4,r0
	movgt r3,r2
	add r2,#1
	b 1b

2:
    mov r0,r4
    ldr r1,iAdrsMessValeur
    bl conversion10S       @ call conversion
    mov r0,r3
    ldr r1,iAdrsMessRank
    bl conversion10       @ call conversion
    ldr r0,iAdrtTableNumbers
    add r0,r3,lsl #2
    ldr r1,iAdrsMessAddress
    bl conversion16       @ call conversion
    ldr r0,iAdrszMessResult
    bl affichageMess            @ display message




100:   @ standard end of the program
    mov r0, #0                  @ return code
    pop {fp,lr}                 @restaur 2 registers
    mov r7, #EXIT              @ request to exit program
    swi 0                       @ perform the system call
iAdrtTableNumbers:    .int  tTableNumbers
iAdrsMessValeur:      .int sMessValeur
iAdrsMessRank:         .int sMessRank
iAdrsMessAddress:     .int sMessAddress
iAdrszMessResult:     .int szMessResult

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
/******************************************************************/
/*     Converting a register to hexadecimal                      */
/******************************************************************/
/* r0 contains value and r1 address area   */
conversion16:
    push {r1-r4,lr}    /* save registers */
    mov r2,#28         @ start bit position
    mov r4,#0xF0000000    @ mask
    mov r3,r0      @ save entry value
1:	   @ start loop
    and r0,r3,r4   @value register and mask
    lsr r0,r2      @ move right
    cmp r0,#10      @ compare value
    addlt r0,#48        @ <10  ->digit
    addge r0,#55        @ >10  ->letter A-F
    strb r0,[r1],#1  @ store digit on area and + 1 in area address
    lsr r4,#4       @ shift mask 4 positions
    subs r2,#4         @  counter bits - 4 <= zero  ?
    bge 1b	          @  no -> loop
    @end
    pop {r1-r4,lr}    @ restaur registres
    bx lr             @return
/******************************************************************/
/*     Converting a register to a decimal                                 */
/******************************************************************/
/* r0 contains value and r1 address area   */
conversion10:
    push {r1-r4,lr}    /* save registers */
    mov r3,r1
    mov r2,#10

1:	   @ start loop
    bl divisionpar10 @ r0 <- dividende. quotient ->r0 reste -> r1
    add r1,#48        @ digit
    strb r1,[r3,r2]  @ store digit on area
    sub r2,#1         @ previous position
    cmp r0,#0         @ stop if quotient = 0 */
    bne 1b	          @ else loop
    @ and move spaves in first on area
    mov r1,#' '   @ space
2:
    strb r1,[r3,r2]  @ store space in area
    subs r2,#1       @ @ previous position
    bge 2b           @ loop if r2 >= zéro

100:
    pop {r1-r4,lr}    @ restaur registres
    bx lr	          @return
/***************************************************/
/*  Converting a register to a signed decimal      */
/***************************************************/
/* r0 contains value and r1 area address    */
conversion10S:
    push {r0-r4,lr}    @ save registers
    mov r2,r1       /* debut zone stockage */
    mov r3,#'+'     /* par defaut le signe est + */
    cmp r0,#0       @ negative number ?
    movlt r3,#'-'   @ yes
    mvnlt r0,r0     @ number inversion
    addlt r0,#1
    mov r4,#10       @ length area
1:  @ start loop
    bl divisionpar10
    add r1,#48   @ digit
    strb r1,[r2,r4]  @ store digit on area
    sub r4,r4,#1      @ previous position
    cmp r0,#0          @ stop if quotient = 0
    bne 1b

    strb r3,[r2,r4]  @ store signe
    subs r4,r4,#1    @ previous position
    blt  100f        @ if r4 < 0 -> end

    mov r1,#' '   @ space
2:
    strb r1,[r2,r4]  @store byte space
    subs r4,r4,#1    @ previous position
    bge 2b           @ loop if r4 > 0
100:
    pop {r0-r4,lr}   @ restaur registers
    bx lr
/***************************************************/
/*   division par 10   signé                       */
/* Thanks to http://thinkingeek.com/arm-assembler-raspberry-pi/*
/* and   http://www.hackersdelight.org/            */
/***************************************************/
/* r0 dividende   */
/* r0 quotient */
/* r1 remainder  */
divisionpar10:
  /* r0 contains the argument to be divided by 10 */
   push {r2-r4}   /* save registers  */
   mov r4,r0
   ldr r3, .Ls_magic_number_10 /* r1 <- magic_number */
   smull r1, r2, r3, r0   /* r1 <- Lower32Bits(r1*r0). r2 <- Upper32Bits(r1*r0) */
   mov r2, r2, ASR #2     /* r2 <- r2 >> 2 */
   mov r1, r0, LSR #31    /* r1 <- r0 >> 31 */
   add r0, r2, r1         /* r0 <- r2 + r1 */
   add r2,r0,r0, lsl #2   /* r2 <- r0 * 5 */
   sub r1,r4,r2, lsl #1   /* r1 <- r4 - (r2 * 2)  = r4 - (r0 * 10) */
   pop {r2-r4}
   bx lr                  /* leave function */
   .align 4
.Ls_magic_number_10: .word 0x66666667



```


## Arturo


```arturo
arr #(5 4 2 9 7 3)

print $(max arr)
```

```txt
9
```


## AutoHotkey


###  CSV Data


```AutoHotkey
list = 1,5,17,-2
Loop Parse, list, `,
   x := x < A_LoopField ? A_LoopField : x
MsgBox Max = %x%
```

=== Pseudo-arrays ===

```AHK
list = 1,5,17,-2
StringSplit, list, list,`, ; creates a pseudo-array
Loop % List0
   x := x < List%A_Index% ? List%A_Index% : x
MsgBox Max = %x%
```


###  True arrays

```AHK
List := [1,5,17,-2]
For each, value in List
   x := x < value ? value : x
MsgBox Max = %x%
```



## AWK

One-liner:

```awk
$ awk 'func max(a){for(i in a)if(a[i]>r)r=a[i];return r}BEGIN{a[0]=42;a[1]=33;a[2]=21;print max(a)}'
42
```


More readable version:

```awk

# Usage: awk -f greatest_list_element.awk
#
function max(a) {
    for(i in a) if(a[i]>r) r=a[i];
    return r
}
#
BEGIN { a[0]=42;
        a[1]=33;
        a[2]=21;
        print max(a)
      }

```



## Axe

This example assumes the array is null-terminated so that the program can stop at the end of the data.

```axe
Lbl MAX
0→M
While {r₁}
 {r₁}>M?{r₁}→M
End
M
Return
```



## BASIC

```qbasic
DECLARE SUB addVal (value AS INTEGER)
DECLARE FUNCTION findMax% ()

REDIM SHARED vals(0) AS INTEGER
DIM SHARED valCount AS INTEGER
DIM x AS INTEGER, y AS INTEGER

valCount = -1

'''''begin test run
RANDOMIZE TIMER
FOR x = 1 TO 10
    y = INT(RND * 100)
    addVal y
    PRINT y; " ";
NEXT
PRINT ": "; findMax
'''''end test run

SUB addVal (value AS INTEGER)
    DIM tmp AS INTEGER
    IF valCount > -1 THEN
        'this is needed for BASICs that don't support REDIM PRESERVE
        REDIM v2(valCount) AS INTEGER
        FOR tmp = 0 TO valCount
            v2(tmp) = vals(tmp)
        NEXT
    END IF
    valCount = valCount + 1
    REDIM vals(valCount)
    IF valCount > 0 THEN
        'also needed for BASICs that don't support REDIM PRESERVE
        FOR tmp = 0 TO valCount - 1
            vals(tmp) = v2(tmp)
        NEXT
    END IF
    vals(valCount) = value
END SUB

FUNCTION findMax%
    DIM tmp1 AS INTEGER, tmp2 AS INTEGER
    FOR tmp1 = 0 TO valCount
        IF vals(tmp1) > tmp2 THEN tmp2 = vals(tmp1)
    NEXT
    findMax = tmp2
END FUNCTION
```


```txt

 8162   5139   7004   7393   5151   4476   577   4419   3333   4649  :  8162

```


=
## BaCon
=

```freebasic
' Greatest element from list
' Populate sample array of numbers
READ elements
DECLARE numbers TYPE NUMBER ARRAY elements
FOR i = 0 TO elements - 1
    READ numbers[i]
NEXT
DATA 6
DATA 100,-2,300,4,500,6

' Demonstrate the function
PRINT greatest(elements, numbers)
END

' Return greatest element given count and list of numbers
FUNCTION greatest(n, NUMBER a[])
    LOCAL mx = a[0]
    FOR i = 1 TO n - 1
        mx = MAX(mx, a[i])
    NEXT
    RETURN mx
END FUNCTION
```


```txt
prompt$  ./greatest-element
500
```


'''See also:''' [[#BBC BASIC|BBC BASIC]], [[#Liberty BASIC|Liberty BASIC]], [[#PowerBASIC|PowerBASIC]], [[#PureBasic|PureBasic]], [[#Run BASIC|Run BASIC]], [[#TI-89 BASIC|TI-89 BASIC]], [[#Visual Basic|Visual Basic]]


## Batch File


```dos
::max.cmd
@echo off
setlocal enabledelayedexpansion
set a=.%~1
if "%a%" equ "." set /p a="Input stream: "
call :max res %a%
echo %res%
endlocal
goto :eof

:max
set %1=%2
:loop
shift /2
if "%2" equ "" goto :eof
if %2 gtr !%1! set res=%2
goto loop
```


''Invocation from command line or from internal prompt''


```dos>
max "123 456 3 234243 12"
234243

>max
Input stream: 5 4 3 2 67 1
67
```



## BBC BASIC


```bbcbasic
      ListOfValues$ = "13, 0, -6, 2, 37, -10, 12"
      PRINT "Maximum value = " ; FNmax(ListOfValues$)
      END

      DEF FNmax(list$)
      LOCAL index%, number, max
      max = VAL(list$)
      REPEAT
        index% = INSTR(list$, ",", index%+1)
        number = VAL(MID$(list$, index%+1))
        IF number > max THEN max = number
      UNTIL index% = 0
      = max
```



## bc


```bc
define m(a[], n) {
    auto m, i

    m = a[0]
    for (i = 1; i < n; i++) {
        if (a[i] > m) m = a[i]
    }
    return(m)
}
```


## Befunge


```befunge
001pv            <
    >&:01g`#v_1+#^_01g.@
    ^p10    <
```

Only works with positive integers. List must be terminated with -1.


## Bracmat

When comparing two rational numbers, Bracmat compares numerically. In all other cases Bracmat compares lexically.
<lang>  ( biggest
  =   max
    .   !arg:
      |   !arg:%?max ?arg
        & !arg:? (%@:>!max:?max) (?&~)
      | !max
  )
& out$("1:" biggest$(5 100000 -5 aap 3446 NOOT mies 0))
& out$("2:" biggest$)
&   out
  $ ( "3:"
        biggest
      $ (5 100000 -5 43756243978569758/13 3365864921428443 87512487957139516/27 3446)
    )
```

```txt
1: mies
2:
3: 3365864921428443
```



## Brat

Arrays have a max function, but here's a manual implementation.

```brat
max = { list |
  list.reduce { n, max |
    true? n > max
      { max = n }
      { max }
  }
}

p max [3 4 1 2]
```



## Burlesque



```burlesque

blsq ) {88 99 77 66 55}>]
99

```



## C

This works well with floats.  Replace with double, int or what-have-you before passing a different data type.

```c
#include <assert.h>

float max(unsigned int count, float values[]) {
     assert(count > 0);
     unsigned int idx;
     float themax = values[0];
     for(i = 1; i < count; ++i) {
          themax = values[i] > themax ? values[i] : themax;
     }
     return themax;
}
```


The following macro can be used with any number and type of arguments, provided that the arguments are ''simple'', i.e. must not contain subexpressions where commas appear (this is because of the way the arguments are counted; the macro can be modified so that it is up to the caller to count the number of arguments passed). <!-- You might wanna look at the macro from here which can count the number of arguments without parsing commas: http://groups.google.com/group/comp.std.c/browse_thread/thread/77ee8c8f92e4a3fb/346fc464319b1ee5 -->

```c
#include <stdarg.h>

#define MAX(A,...) ({ inline __typeof__ (A) _max_(__typeof__ (A) a, ...) {\
  va_list l; int i,c; const char *s = #__VA_ARGS__; __typeof__ (A) max = a;\
  __typeof__ (A) t;\
  for(c=1;*s!=0;s++) if (*s==',') c++;\
  va_start(l, a);\
  for(i=0;i<=c;i++) {\
  if ((t=va_arg(l,__typeof__ (A))) > max) max = t;\
  }\
  va_end(l); return max;\
}\
_max_((A),__VA_ARGS__);\
})
```



## C++

A simple wrapper around the standard library function <tt>max_element()</tt>.
Requires C++17.

```cpp
#include <algorithm>  //std::max_element
#include <iterator>   //std::begin and std::end
#include <functional> //std::less

template<class It, class Comp = std::less<>>
    //requires ForwardIterator<It> && Compare<Comp>
constexpr auto max_value(It first, It last, Comp compare = std::less{})
{
    //Precondition: first != last
    return *std::max_element(first, last, compare);
}

template<class C, class Comp = std::less<>>
    //requires Container<C> && Compare<Comp>
constexpr auto max_value(const C& container, Comp compare = std::less{})
{
    //Precondition: !container.empty()
    using std::begin; using std::end;
    return max_value(begin(container), end(container), compare);
}
```


## C#

C# already has a "Maximum Value" function.


```c#
using System.Linq;

values.Max();
```



## Clojure

The Clojure.core function max returns the max of its arguments.

```clojure
(max 1 2 3 4) ; evaluates to 4
;; If the values are already in a collection, use apply:
(apply max [1 2 3 4]) ; evaluates to 4
```



## CMake

Only for lists of integers.


```cmake
# max(var [value1 value2...]) sets var to the maximum of a list of
# integers. If list is empty, sets var to NO.
function(max var)
  set(first YES)
  set(choice NO)
  foreach(item ${ARGN})
    if(first)
      set(choice ${item})
      set(first NO)
    elseif(choice LESS ${item})
      set(choice ${item})
    endif()
  endforeach(item)
  set(${var} ${choice} PARENT_SCOPE)
endfunction(max)

set(list 33 11 44 22 66 55)
max(maximum ${list})
message(STATUS "maximum of ${list} => ${maximum}")
```



```txt
-- maximum of 33;11;44;22;66;55 => 66
```



## COBOL

This is already built into the language for tables of numbers.

```cobol
DISPLAY FUNCTION MAX(nums (ALL))
```


A sample implementation:

```cobol
       IDENTIFICATION DIVISION.
       FUNCTION-ID. greatest-elt.

       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       01  idx                     USAGE INDEX.

       01  Table-Len               CONSTANT 50.

       LINKAGE SECTION.
       01  num-table-area.
           03  num-table           PIC 9(8) OCCURS Table-Len TIMES.

       01  max-elt                 PIC 9(8).

       PROCEDURE DIVISION USING VALUE num-table-area RETURNING max-elt.
           PERFORM VARYING idx FROM 1 BY 1 UNTIL idx > Table-Len
               IF num-table (idx) > max-elt
                   MOVE num-table (idx) TO max-elt
               END-IF
           END-PERFORM

           GOBACK
           .
       END FUNCTION greatest-elt.
```



## CoffeeScript


```coffeescript

# using Math library
max1 = (list) ->
 Math.max.apply null, list

# using no libraries
max2 = (list) ->
 maxVal = list[0]
 for value in list
  maxVal = value if value > maxVal
 maxVal



# Test it
a = [0,1,2,5,4];
alert(max1(a)+". The answer is "+max2(a));

```



## ColdFusion


```cfm

<Cfset theList = '1, 1000, 250, 13'>
<Cfparam name="maxNum" default=0>
<Cfloop list="#theList#" index="i">
  <Cfif i gt maxNum><Cfset maxNum = i></Cfif>
</Cfloop>
<Cfoutput>#maxNum#</Cfoutput>

```



```cfm

<Cfset theList = '1, 1000, 250, 13'>
<Cfset maxNum = ListFirst(ListSort(thelist, "numeric", "desc"))>
<Cfoutput>#maxNum#</Cfoutput>

```



## Common Lisp

The built-in Common Lisp function <tt>max</tt> takes the max of all its arguments.

```lisp
(max 1 2 3 4)
(reduce #'max values) ; find max of a list
(loop for x in values
      maximize x) ; alternative way to find max of a list
```



## Component Pascal

BalckBox Component Builder

```oberon2

MODULE Operations;
IMPORT StdLog,Args,Strings;

PROCEDURE Max(s: ARRAY OF INTEGER): INTEGER;
VAR
	i: INTEGER;
	max: INTEGER;
BEGIN
	max := MIN(INTEGER);
	FOR i := 0 TO LEN(s) - 1 DO
		max := MAX(max,s[i]);
	END;
	RETURN max
END Max;

PROCEDURE DoMax*;
VAR
	sq: POINTER TO ARRAY OF INTEGER;
	p: Args.Params;
	i,n,done: INTEGER;
BEGIN
	Args.Get(p);
	IF p.argc > 0 THEN
		NEW(sq,p.argc);
		FOR i := 0 TO p.argc - 1 DO
			Strings.StringToInt(p.args[i],n,done);
			sq[i] := n
		END;
		StdLog.String("max:> ");StdLog.Int(Max(sq));StdLog.Ln
	END
END DoMax;

END Operations.

```

Execute: ^Q Operations..DoMax 23 12 3 45 34 54 84 ~<br/>
```txt

max:>  84

```



## D


D already has a "Maximum Element" function.


```d
void main()
{
    import std.algorithm.searching : maxElement;
    import std.stdio : writeln;

    [9, 4, 3, 8, 5].maxElement.writeln;
}
```

```txt
9
```



## Dart


```Dart
/*This is a function which returns the greatest element in a list of numbers */
num findGreatestElement(List<num> list){
  num greatestElement = list[0];
  for (num element in list){
    if (element>greatestElement) {
      greatestElement = element;
    }
  }
  return greatestElement;
}
/* and this is a shorter version */
import 'dart:math';
num findGreatestElement(List<num> list){
  return list.reduce(max);
}

```



## dc


```dc
[sm llx] sg
[lm p q] sq
[d lm <u s_ z 0 =q llx] sl
[d sm] su

["Put list of numbers on the stack starting here, then execute g"] s_

3.14159265358979 sp

_275.0 _111.19 0.0 _1234568.0 lp lp _1 *

lgx
```


```txt
3.14159265358979
```



## DCL


```DCL
$ list = "45,65,81,12,0,13,-56,123,-123,888,12,0"
$ max = f$integer( f$element( 0, ",", list ))
$ i = 1
$ loop:
$  element = f$element( i, ",", list )
$  if element .eqs. "," then $ goto done
$  element = f$integer( element )
$  if element .gt. max then $ max = element
$  i = i + 1
$  goto loop
$ done:
$ show symbol max
```

```txt
$ @greatest
  MAX = 888   Hex = 00000378  Octal = 00000001570
```



## Delphi


```Delphi

program GElemLIst;
{$IFNDEF FPC}
  {$Apptype Console}
{$ENDIF}

uses
  math;
const
  MaxCnt = 10000;
var
  IntArr : array of integer;
  fltArr : array of double;
  i: integer;
begin
  setlength(fltArr,MaxCnt); //filled with 0
  setlength(IntArr,MaxCnt); //filled with 0.0
  randomize;
  i := random(MaxCnt);      //choose a random place
  IntArr[i] := 1;
  fltArr[i] := 1.0;
  writeln(Math.MaxIntValue(IntArr)); // Array of Integer
  writeln(Math.MaxValue(fltArr));
end.

```


=={{header|Déjà Vu}}==

```dejavu
max lst:
    lst! 0
    for item in copy lst:
         if > item dup:
              item drop

!. max [ 10 300 999 9 ]
```

```txt
999
```



## Dyalect


```dyalect
func max(xs) {
    var y
    for x in xs {
        if y == nil || x > y {
            y = x
        }
    }
    y
}

var xs = [1..10]
```



## E


This function works for any value which responds to <code>[http://wiki.erights.org/wiki/Category:Message_max/1 max/1]</code>:


```e
pragma.enable("accumulator") # non-finalized syntax feature

def max([first] + rest) {
    return accum first for x in rest { _.max(x) }
}
```



```e
? max([1, 2, 3])
# value: 3
```


To require only the comparison protocol, one needs to write out the algorithm a little more explicitly:


```e
def max([var bestSoFar] + rest) {
    for x ? (x > bestSoFar) in rest {
        bestSoFar := x
    }
    return bestSoFar
}
```



```e
? max([1, 3, 2])
# value: 3

? max([[1].asSet(), [2].asSet(), [1, 2].asSet()])
# value: [1, 2].asSet()
```



## EchoLisp


```lisp

;; a random length list of random values
(define L (map random (make-list (random 50) 100))) → L
L → (24 60 83 8 24 60 31 97 96 65 9 41 64 24 22 57 73 17 6 28 77 58 18 13 27 22 41 69 85)

;; find max
(apply max L) → 97

```



## ECL


```ECL

MaxVal(SET OF INTEGER s) := MAX(s);

//example usage

SetVals := [4,8,16,2,1];
MaxVal(SetVals) //returns 16;

```



## Efene



```efene
list_max = fn ([Head:Rest]) {
  list_max(Rest, Head)
}

list_max = fn ([], Res) {
  Res
}
fn ([Head:Rest], Max) when Head > Max {
  list_max(Rest, Head)
}
fn ([_Head:Rest], Max) {
  list_max(Rest, Max)
}

list_max1 = fn ([H:T]) {
  lists.foldl(fn erlang.max:2, H, T)
}

@public
run = fn () {
    io.format("~p~n", [list_max([9, 4, 3, 8, 5])])
    io.format("~p~n", [list_max1([9, 4, 3, 8, 5])])
}

```



## Eiffel

The GREATEST_ELEMENT class:

```eiffel

class
	GREATEST_ELEMENT [G -> COMPARABLE]

create
	make

feature {NONE} --Implementation

	is_max (element: G maximum: G): BOOLEAN
		do
			Result := maximum >= element
		end

	max (list: ARRAY [G]): G
		require
			not_empty: not list.is_empty
		do
			Result := list [list.lower]
			across
				list as i
			loop
				Result := i.item.max (Result)
			end
		ensure
			is_part_of_array: list.has (Result)
			is_maximum: list.for_all (agent is_max(?, Result))
		end

feature -- Initialization

	make
		do
		end

	greatest_element (a: ARRAY [G]): G
		do
			Result := max (a)
		end

end

```

A test application:

```eiffel

class
	APPLICATION

create
	make

feature {NONE} -- Initialization

	make
			-- Run application.
		local
			numbers: ARRAY [INTEGER]
			greatest: GREATEST_ELEMENT [INTEGER]
		do
			create greatest.make
			numbers := <<1, 2, 3, 4, 5, 6, 7, 8, 9>>
			print (greatest.greatest_element (numbers))
		end

end

```



## Ela



```ela
open list

findBy p (x::xs) = foldl (\x y | p x y -> x | else -> y) x xs
maximum = findBy (>)

maximum [1..10]
```


## Elena

ELENA 4.1 :

```elena
import extensions;

extension op
{
    get Maximal()
    {
        auto en := cast Enumerator(self.enumerator());

        object maximal := nil;
        while (en.next())
        {
            var item := en.get();
            if (nil == maximal)
            {
                maximal := item
            }
            else if (maximal < item)
            {
                maximal := item
            }
        };

        ^ maximal
    }
}

public program()
{
    console.printLine(new int[]::(1,2,3,4,20,10,9,8).Maximal)
}
```

```txt

20

```



## Elixir


```elixir
iex(1)> Enum.max([3,1,4,1,5,9,2,6,5,3])
9
```



## Emacs Lisp


```lisp
(defun max (first-arg &rest more-args)
  (if more-args
      (let ((max-rest (apply 'max more-args)))
	(if (> first-arg max-rest)
	    first-arg
	  max-rest))
    first-arg))
```

Example use:

```lisp
(max 2 7 5)
7
```

Alternative Built-In:

```lisp

(cl-loop for el in '(2 7 5) maximize el)

```



## Erlang

Builtin. Using it from the Erlang shell:

```erlang>
lists:max([9,4,3,8,5]).
9
```



## ERRE


```ERRE

PROGRAM MAXLIST

!
! for rosettacode.org
!

! VAR L$,EL$,CH$,I%,MAX

BEGIN
  PRINT(CHR$(12);) ! CLS
  INPUT("Lista",L$)
  L$=L$+CHR$(32)
  MAX=-1.7E+38
  FOR I%=1 TO LEN(L$) DO
    CH$=MID$(L$,I%,1)
    IF CH$<>CHR$(32) THEN ! blank is separator
       EL$=EL$+CH$
     ELSE
       IF VAL(EL$)>MAX THEN MAX=VAL(EL$) END IF
       EL$=""
    END IF
  END FOR
  PRINT("Max list element is";MAX)
END PROGRAM

```

Note: The limit of this program is string variable lenght (255 chars). The advantage is no array use.


## Euler Math Toolbox


<lang>
>v=random(1,100);
>max(v)
 0.997492478596

```



## Euphoria


### Applying a function to each element of an array


```Euphoria
function aeval( sequence sArr, integer id )
    for i = 1 to length( sArr ) do
        sArr[ i ] = call_func( id, { sArr[ i ] } )
    end for
    return sArr
end function

object biggun
function biggest( object elem )
    if compare(elem, biggun) > 0 then
        biggun = elem
    end if
    return elem
end function

biggun = 0
object a
a = aeval( {1,1234,62,234,12,34,6}, routine_id("biggest") )
printf( 1, "%d\n", biggun )

sequence s
s = {"antelope", "dog", "cat", "cow", "wolf", "wolverine", "aardvark"}
biggun = "ant"
a = aeval( s, routine_id("biggest") )
printf( 1, "%s\n", {biggun} )
```

```txt

1234
wolverine

```



### More trivial example


```euphoria
function get_biggest(sequence s)
    object biggun
    biggun = s[1]
    for i = 2 to length(s) do
        if compare(s[i], biggun) > 0 then
            biggun = s[i]
        end if
    end for
    return biggun
end function

constant numbers = {1,1234,62,234,12,34,6}
printf(1,"%d\n",get_biggest(numbers))

constant animals = {"ant", "antelope", "dog", "cat", "cow", "wolf", "wolverine", "aardvark"}
printf(1,"%s\n",{get_biggest(animals)})
```


```txt

1234
wolverine

```



## Excel

Use the function MAX


```Excel

=MAX(3;2;1;4;5;23;1;2)

```

```txt

23

```

=={{header|F_Sharp|F#}}==

I generate a list of 10 random numbers at runtime then use F#'s built in function to find the maximum value of the list.


```fsharp

let N = System.Random()
let G = List.init 10 (fun _->N.Next())
List.iter (printf "%d ") G
printfn "\nMax value of list is %d" (List.max G)

```

```txt

401566008 1378437959 1806806326 2010005455 1973773308 1216833747 268836584 1963610340 2120237482 1412806752
Max value of list is 2120237482

```



## Factor

The following word is in factor's standard library.

```factor
: supremum ( seq -- elt ) [ ] [ max ] map-reduce ;
```



## Fancy


```fancy
[1,-2,2,4,6,-4,-1,5] max println  # => 6
```



## Fantom


Has a built-in method to get maximum from a list.


```fantom

class Greatest
{
  public static Void main ()
  {
    Int[] values := [1,2,3,4,5,6,7,8,9]
    Int greatest := values.max
    echo (greatest)
  }
}

```


=={{header|Fōrmulæ}}==

In [https://wiki.formulae.org/Greatest_element_of_a_list this] page you can see the solution of this task.

Fōrmulæ programs are not textual, visualization/edition of programs is done showing/manipulating structures but not text ([http://wiki.formulae.org/Editing_F%C5%8Drmul%C3%A6_expressions more info]). Moreover, there can be multiple visual representations of the same program. Even though it is possible to have textual representation &mdash;i.e. XML, JSON&mdash; they are intended for transportation effects more than visualization and edition.

The option to show Fōrmulæ programs and their results is showing images. Unfortunately images cannot be uploaded in Rosetta Code.


## Forth


```forth
: array-max ( addr len -- max )
  dup 0= if nip exit then
  over @  rot cell+  rot 1-
  cells bounds ?do  i @ max  cell +loop ;

: stack-max ( n ... m count -- max ) 1 ?do max loop ;
```



## Fortran

The intrinsic function <tt>maxval</tt> returns the maximum value of the elements in an integer or real array:


```fortran
program test_maxval

integer,dimension(5),parameter :: x = [10,100,7,1,2]
real,dimension(5),parameter :: y = [5.0,60.0,1.0,678.0,0.0]

write(*,'(I5)') maxval(x)
write(*,'(F5.1)') maxval(y)

end program test_maxval
```


```txt

 100
 678.0

```


The intrinsic function <tt>max</tt> accepts any number of arguments.
The type of these arguments can be integer, real, character, string of characters or arrays of these.

```fortran
program test_max

  implicit none

  write (*, '(i0)') &
    & max (1, 2, 3)
  write (*, '(f3.1)') &
    & max (1.0, 2.0, 3.0)
  write (*, '(a)') &
    & max ('a', 'b', 'c')
  write (*, '(a)') &
    & max ('abc', 'bca', 'cab')
  write (*, '(i0, 2 (1x, i0))') &
    & max ([1, 8, 6], [7, 5, 3], [4, 2, 9])
  write (*, '(f3.1, 2 (1x, f3.1))') &
    & max ([1.0, 8.0, 6.0], [7.0, 5.0, 3.0], [4.0, 2.0, 9.0])
  write (*, '(a, 2 (1x, a))') &
    & max (['a', 'h', 'f'], ['g', 'e', 'c'], ['d', 'b', 'i'])
  write (*, '(a, 2 (1x, a))') &
    & max (['abc', 'hig', 'fde'], ['ghi', 'efd', 'cab'], ['def', 'bca', 'igh'])

end program test_max
```

```txt

 3
 3.0
 c
 cab
 7 8 9
 7.0 8.0 9.0
 g h i
 ghi hig igh

```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Function MaxElement(a() As Double) As Double
  Dim max As Double = a(LBound(a))
  For i As Integer = LBound(a) + 1 To UBound(a)
    If a(i) > max Then max = a(i)
  Next
  Return max
End Function

Dim As Integer i, n
Input "How many values are to be input ";  n
If n < 1 Then End
Dim a(1 To n) As Double
For i = 1 To n
  Print "  Value"; i; " : ";
  Input "", a(i)
Next
Dim max As Double = MaxElement(a())
Print
Print "The greatest value is"; max
Print
Print "Press any key to quit"
Sleep

```


Example of use :
```txt

How many values are to be input ? 4
  Value 1 : 70.5
  Value 2 : 23.67
  Value 3 : 150.2
  Value 4 : 145

The greatest value is 150.2

```



## Frink


```frink

println[max[[1,2,3,5,10,20]]]

```



## FunL

Using the pre-defined function <code>max</code>:

```funl
println( max([1,2,3,-1,0]) )
```

```txt

3

```


It can be implemented as:

```funl
def
  maximum( xs ) =
    def
      max( a, b )             = if a <= b then b else a

      foldl( f, z, [] )       = z
      foldl( f, z, x:xs )     = foldl( f, f(z, x), xs )

      foldl1( f, x:xs )       = foldl( f, x, xs )
      foldl1( _, [] )         = error( "foldl1: empty list" )

    foldl1( max, xs )
```



## Futhark



```futhark
let main (xs: []f64) = reduce f64.max (-f64.inf) xs
```


This parallel formulation exploits the fact that negative infinity constitutes a neutral element for the maximum operator.


## GAP


```gap
# Built-in

L := List([1 .. 100], n -> Random(1, 10));

MaximumList(L);
# 10
```



## Go

;Library

```go
package main

import (
    "fmt"

    "github.com/gonum/floats"
)

func main() {
    fmt.Println(floats.Max([]float64{3, 1, 4, 1}))
}
```

```txt

4

```

;List
The task title says list.  This solution uses a Go slice as a list.

```go
package main

import (
    "fmt"
    "math/rand"
    "time"
)

// function, per task description
func largest(a []int) (lg int, ok bool) {
    if len(a) == 0 {
        return
    }
    lg = a[0]
    for _, e := range a[1:] {
        if e > lg {
            lg = e
        }
    }
    return lg, true
}

func main() {
    // random size slice
    rand.Seed(time.Now().UnixNano())
    a := make([]int, rand.Intn(11))
    for i := range a {
        a[i] = rand.Intn(101) - 100 // fill with random numbers
    }

    fmt.Println(a)
    lg, ok := largest(a)
    if ok {
        fmt.Println(lg)
    } else {
        fmt.Println("empty list.  no maximum.")
    }
}
```

;Set
The task description says set.  This solution uses a Go map as a set.

```go
package main

import (
    "fmt"
    "math"
    "math/rand"
    "time"
)

// Function, per task description.  Interesting with the float64 type because
// of the NaN value.  NaNs do not compare to other values, so the result of
// a "largest" function on a set containing a NaN might be open to
// interpretation.  The solution provided here is to return the largest
// of the non-NaNs, and also return a bool indicating the presense of a NaN.
func largest(s map[float64]bool) (lg float64, ok, nan bool) {
    if len(s) == 0 {
        return
    }
    for e := range s {
        switch {
        case math.IsNaN(e):
            nan = true
        case !ok || e > lg:
            lg = e
            ok = true
        }
    }
    return
}

func main() {
    rand.Seed(time.Now().UnixNano())
    // taking "set" literally from task description
    s := map[float64]bool{}
    // pick number of elements to add to set
    n := rand.Intn(11)
    // add random numbers, also throw in an occasional NaN or Inf.
    for i := 0; i < n; i++ {
        switch rand.Intn(10) {
        case 0:
            s[math.NaN()] = true
        case 1:
            s[math.Inf(1)] = true
        default:
            s[rand.ExpFloat64()] = true
        }
    }

    fmt.Print("s:")
    for e := range s {
        fmt.Print(" ", e)
    }
    fmt.Println()
    switch lg, ok, nan := largest(s); {
    case ok && !nan:
        fmt.Println("largest:", lg)
    case ok:
        fmt.Println("largest:", lg, "(NaN present in data)")
    case nan:
        fmt.Println("no largest, all data NaN")
    default:
        fmt.Println("no largest, empty set")
    }
}
```



## Golfscript


```golfscript
{$-1=}:max;
[1 4 8 42 6 3]max # Example usage
```



## Groovy


```groovy
println ([2,4,0,3,1,2,-12].max())
```


```txt
4
```



## Haskell

The built-in Haskell function <tt>maximum</tt> returns a maximum based on default comparison between members of an ordered type.

```haskell
my_max = maximum
```

It can alternately be defined as a "fold" on the built-in two-argument <tt>max</tt> function.

```haskell
my_max = foldl1 max
```


More generally, '''maximum''' is a special case of '''maximumBy''', which allows us to define or supply our own comparison function, and define the particular type of maximum that we need:

```haskell
import Data.List (maximumBy)
import Data.Ord (comparing)

wds :: [String]
wds = ["alpha", "beta", "gamma", "delta", "epsilon", "zeta"]

main :: IO ()
main = print $ maximumBy (comparing length) wds
```


As a fold, maximumBy could be defined along the lines of:


```haskell
maximumBy
  :: Foldable t
  => (a -> a -> Ordering) -> t a -> a
maximumBy cmp =
  let max_ x y =
        case cmp x y of
          GT -> x
          _ -> y
  in foldr1 max_
```



## hexiscript


```hexiscript
fun greatest a
  let l len a
  let max a[0]
  for let i 1; i < l; i++
    if max < a[i]
      let max a[i]
    endif
  endfor
  return max
endfun
```



## HicEst


```hicest

   max_value = MAX( -123,  234.56, 345.678, -456E3, -455) ! built-in function MAX(...)

! or for an array:
   max_value = MAX( array_of_values )

! or to find a maximum value in a file named filename:
   CHARACTER List, filename='Greatest element of a list.hic' ! filename contains this script
   REAL values(1) ! unknown number of values, allocate more below

   OPEN(FIle=filename, BINary, LENgth=len)
   ALLOCATE(values, len/2) ! number of values <= half byte count of file
   ! read all values, returns item count in values_found:
   READ(FIle=filename, ItemS=values_found, CLoSe=1) values ! no Format needed for plain text numbers

   max_value = MAX(values)

   ! write values found in filename and result to spreadsheet type dialog window:
   DLG(Text=values, Text=max_value, TItle=values_found)

   WRITE(ClipBoard, Name) max_value, values_found, values ! pasted to line below
   ! max_value=345.678; values_found=30; values(1)=-123; values(2)=234.56; values(3)=345.678; values(4)=-456E3; values(5)=-455; values(6)=1; values(7)=2; values(8)=1; values(9)=0; values(10)=0; ...truncated
 END

```



## Hoon


```Hoon
:-  %say
|=  [^ [a=(list ,@) ~] ~]
:-  %noun
  (snag 0 (sort a gte))
```

Usage: Add to a file gen/max.hoon

```txt

> +max [1 2 3 ~]
3

```



## i


```i
concept largest(l) {
	large = l[0]
	for element in l
		if element > large
			large = element
		end
	end
	return large
}

software {
	print(largest([23, 1313, 21, 35757, 4, 434, 232, 2, 2342]))
}
```



=={{header|Icon}} and {{header|Unicon}}==

```icon
procedure main()
   local l
   l := [7,8,6,9,4,5,2,3,1]
   write(max(l))
end

procedure max(l)
   local max
   max := l[1]
   every max <:= !l
   return max
end
```


=={{header|IS-BASIC}}==
<lang IS-BASIC>1000 DEF FINDMAX(REF ARR)
1010   LET MX=ARR(LBOUND(ARR))
1020   FOR I=LBOUND(ARR)+1 TO UBOUND(ARR)
1030     LET MX=MAX(MX,ARR(I))
1040   NEXT
1050   LET FINDMAX=MX
1060 END DEF
```



## J

'''Solution''':
```j
   >./
```

'''Example''':
```j
>
./ 1 2 3 2 1
3
   >./''  NB.  Maximum value of an empty list = identity element (or neutral) of max = -∞
__
```



## Java

The first function works with arrays of floats.  Replace with arrays of double, int, or other primitive data type.

```java
public static float max(float[] values) throws NoSuchElementException {
    if (values.length == 0)
        throw new NoSuchElementException();
    float themax = values[0];
    for (int idx = 1; idx < values.length; ++idx) {
        if (values[idx] > themax)
            themax = values[idx];
    }
    return themax;
}
```


Optionally, if it is OK to rearrange the contents of the original array:

```java
public static float max(float[] values) throws NoSuchElementException {
    if (values.length == 0)
        throw new NoSuchElementException();
    Arrays.sort(values);//sorts the values in ascending order
    return values[values.length-1];
}
```


The following functions work with Lists or arrays of reference types, respectively. Note that the type is required to implement Comparable, to ensure we can compare them. For Lists, there is a utility method <tt>Collections.max()</tt> that already does this. For arrays, we can just use the <tt>Arrays.asList()</tt> wrapper to wrap it into a list and then use the function for lists.

```java
import java.util.List;
import java.util.Collections;
import java.util.Arrays;

public static <T extends Comparable<? super T>> T max(List<T> values) {
    return Collections.max(values);
}

public static <T extends Comparable<? super T>> T max(T[] values) {
    return Collections.max(Arrays.asList(values));
}
```



## JavaScript

===ES3-5===

```javascript
Math.max.apply(null, [ 0, 1, 2, 5, 4 ]); // 5
```



### ES 5 maxima beyond simple numeric data types


Math.max() serves well with simple numeric types, but for less restricted use we can write a generic '''maximumBy''' function which returns the maximum value from an array containing a series of any consistent data type, and which takes a type-specific comparison function as an argument.


```JavaScript
(function () {

    // (a -> a -> Ordering) -> [a] -> a
    function maximumBy(f, xs) {
        return xs.reduce(function (a, x) {
            return a === undefined ? x : (
                f(x, a) > 0 ? x : a
            );
        }, undefined);
    }

    // COMPARISON FUNCTIONS FOR SPECIFIC DATA TYPES

    //Ordering: (LT|EQ|GT)
    //  GT: 1 (or other positive n)
    //  EQ: 0
    //  LT: -1 (or other negative n)

    function wordSortFirst(a, b) {
        return a < b ? 1 : (a > b ? -1 : 0)
    }

    function wordSortLast(a, b) {
        return a < b ? -1 : (a > b ? 1 : 0)
    }

    function wordLongest(a, b) {
        return a.length - b.length;
    }

    function cityPopulationMost(a, b) {
        return a.population - b.population;
    }

    function cityPopulationLeast(a, b) {
        return b.population - a.population;
    }

    function cityNameSortFirst(a, b) {
        var strA = a.name,
            strB = b.name;

        return strA < strB ? 1 : (strA > strB ? -1 : 0);
    }

    function cityNameSortLast(a, b) {
        var strA = a.name,
            strB = b.name;

        return strA < strB ? -1 : (strA > strB ? 1 : 0);
    }

    var lstWords = [
            'alpha', 'beta', 'gamma', 'delta', 'epsilon', 'zeta', 'eta',
            'theta', 'iota', 'kappa', 'lambda'
        ];

    var lstCities = [
        {
            name: 'Shanghai',
            population: 24.15
            }, {
            name: 'Karachi',
            population: 23.5
            }, {
            name: 'Beijing',
            population: 21.5
            }, {
            name: 'Tianjin',
            population: 14.7
            }, {
            name: 'Istanbul',
            population: 14.4
            }, , {
            name: 'Lagos',
            population: 13.4
            }, , {
            name: 'Tokyo',
            population: 13.3
            }
        ];

    return [
        maximumBy(wordSortFirst, lstWords),
        maximumBy(wordSortLast, lstWords),
        maximumBy(wordLongest, lstWords),
        maximumBy(cityPopulationMost, lstCities),
        maximumBy(cityPopulationLeast, lstCities),
        maximumBy(cityNameSortFirst, lstCities),
        maximumBy(cityNameSortLast, lstCities)
    ]

})();
```



```JavaScript
[
  "alpha",
  "zeta",
  "epsilon",
  {
    "name": "Shanghai",
    "population": 24.15
  },
  {
    "name": "Tokyo",
    "population": 13.3
  },
  {
    "name": "Beijing",
    "population": 21.5
  },
  {
    "name": "Tokyo",
    "population": 13.3
  }
]
```



### ES6

For, numbers, a method of the standard Math object:

```javascript
Math.max(...[ 0, 1, 2, 5, 4 ]); // 5
```


and for orderings defined over other datatypes:

```JavaScript
(() => {
    'use strict';

    // MAXIMUM BY ... --------------------------------------------------------

    // Ordering: (LT|EQ|GT):
    //  GT: 1 (or other positive n)
    //  EQ: 0
    //  LT: -1 (or other negative n)
    // maximumByMay :: (a -> a -> Ordering) -> [a] -> Maybe a
    const maximumByMay = (f, xs) =>
        xs.length > 0 ? (
            just(xs.slice(1)
                .reduce((a, x) => f(x, a) > 0 ? x : a, xs[0]))
        ) : nothing('Empty list');


    // GENERIC FUNCTIONS -----------------------------------------------------

    // comparing :: (a -> b) -> (a -> a -> Ordering)
    const comparing = f =>
        (x, y) => {
            const
                a = f(x),
                b = f(y);
            return a < b ? -1 : a > b ? 1 : 0
        };

    // catMaybes :: [Maybe a] -> [a]
    const catMaybes = mbs =>
        concatMap(m => m.nothing ? [] : [m.just], mbs);

    // concatMap :: (a -> [b]) -> [a] -> [b]
    const concatMap = (f, xs) =>
        xs.length > 0 ? [].concat.apply([], xs.map(f)) : [];

    // just :: a -> Just a
    const just = x => ({
        nothing: false,
        just: x
    });

    // nothing :: () -> Nothing
    const nothing = (optionalMsg) => ({
        nothing: true,
        msg: optionalMsg
    });

    // show :: Int -> a -> Indented String
    // show :: a -> String
    const show = (...x) =>
        JSON.stringify.apply(
            null, x.length > 1 ? [x[1], null, x[0]] : x
        );

    // TEST ------------------------------------------------------------------
    const words = ["alpha", "beta", "gamma", "delta", "epsilon", "zeta", "eta"];
    const cities = [{
        k: 'Bejing',
        n: 21.5
    }, {
        k: 'Delhi',
        n: 16.7
    }, {
        k: 'Karachi',
        n: 23.5
    }, {
        k: 'Lagos',
        n: 16.0
    }, {
        k: 'Shanghai',
        n: 24.3
    }, {
        k: 'Tokyo',
        n: 13.2
    }];

    // length :: [a] -> Int
    const length = xs => xs.length;

    // population :: {k: String, n: Float}
    const population = dct => dct.n;

    // OUTPUT ----------------------------------------------------------------
    const maxima = ([
        maximumByMay(comparing(length), words),
        maximumByMay(comparing(length), []),
        maximumByMay(comparing(population), cities)
    ]);

    return show(2,
        catMaybes(maxima)
    );
})();
```

```txt
[
  "epsilon",
  {
    "k": "Shanghai",
    "n": 24.3
  }
]
```



## jq

jq defines a total ordering of all JSON entities, and the <tt>max</tt> filter can accordingly be used on any JSON array:

```jq
[1, 3, 1.0] | max # => 3

[ {"a": 1},  {"a":3}, {"a":1.0}] | max  # => {"a": 3}
```

Warning: both "[null]|max" and "[]|max" yield null.

Given a stream, s, of JSON values, the following filter will emit null if the stream is empty, or the maximum value otherwise:

```jq
reduce s as $x (null; if $x > . then $x else . end)
```



## Julia

Using the built-in <code>maximum</code> function:

```julia
julia>
 maximum([1,3,3,7])
7

julia> maximum([pi,e+2/5,cos(6)/5,sqrt(91/10)])
3.141592653589793

julia> maximum([1,6,Inf])
Inf

julia> maximum(Float64[])
maximum: argument is empty
at In[138]:1
 in maximum at abstractarray.jl:1591

```

(Note that it throws an exception on an empty array.)


## K


```k
  |/ 6 1 7 4
7
```



## Klong


```k
list::[ 1.0 2.3 1.1 5.0 3 2.8 2.01 3.14159 77 ]
|/list
|/ [ 1.0 2.3 1.1 5.0 3 2.8 2.01 3.14159 66 ]
|/ 1.0,2.3,1.1,5.0,3,2.8,2.01,3.14159,55
```

```txt
77
66
55

```



## Kotlin

Kotlin already has a 'max' function in its standard library so we use that:

```scala
// version 1.0.5-2
fun main(args: Array<String>) {
    print("Number of values to be input = ")
    val n = readLine()!!.toInt()
    val array = DoubleArray(n)
    for (i in 0 until n) {
         print("Value ${i + 1} = ")
         array[i] = readLine()!!.toDouble()
    }
    println("\nThe greatest element is ${array.max()}")
}
```

Example of use:
```txt

Number of values to be input = 4
Value 1 = 70.5
Value 2 = 23.67
Value 3 = 150.2
Value 4 = 145

The greatest element is 150.2

```



## Lasso


```Lasso
define greatest(a::array) => {
	return (#a->sort&)->last
}

local(x = array(556,1,7344,4,7,52,22,55,88,122,55,99,1222,578))
greatest(#x)
```


```txt
7344
```



## LFE

Builtin. Using it from the LFE shell:

```lisp
>
(: lists max '[9 4 3 8 5])
9
```



## Liberty BASIC


```lb
aList$= "1 15 -5 6 39 1.5 14"

maxVal = val(word$(aList$, 1))
token$ = "?"
while token$ <> ""
    index = index + 1
    token$ = word$(aList$, index)
    aVal = val(token$)
    if aVal > maxVal then maxVal = aVal
wend

print "maxVal = ";maxVal
```



## Lingo


```lingo
l = [1,7,5]
put max(l)
-- 7
```



## LiveCode


Max is built-in
```livecode
put max(2,3,6,7,4,1)
```

Result: 7

To be fair to the exercise, an alternative implementation could be
```liveCode
function max2 lst
    local maxNum
    put item 1 of lst into maxNum
    repeat with n = 1 to the number of items of lst
        if item n of lst > maxNum then
            put item n of lst into maxNum
        end if
    end repeat
    return maxNum
end max2
on mouseUp
 answer max2("1,2,5,6,7,4,3,2")
end mouseUp
```



## Logo

If the template is, like SUM, the name of a procedure that is
capable of accepting arbitrarily many inputs, it is more efficient
to use APPLY instead of REDUCE.  The latter is good for associative
procedures that have been written to accept exactly two inputs:

```logo
to max :a :b
output ifelse :a > :b [:a] [:b]
end

print reduce "max [...]
```


Alternatively, REDUCE can be used to write MAX as a procedure
that accepts any number of inputs, as SUM does:

```logo
to max [:inputs] 2
if emptyp :inputs ~
   [(throw "error [not enough inputs to max])]
output reduce [ifelse ?1 > ?2 [?1] [?2]] :inputs
end
```



## Logo

```logo
to bigger :a :b
  output ifelse [greater? :a :b] [:a] [:b]
end

to max :lst
  output reduce "bigger :lst
end
```



## Logtalk


```logtalk

max([X| Xs], Max) :-
    max(Xs, X, Max).

max([], Max, Max).
max([X| Xs], Aux, Max) :-
    (   X @> Aux ->
        max(Xs, X, Max)
    ;   max(Xs, Aux, Max)
    ).
```



## Lua


```lua
-- Table to store values
local values = {}
-- Read in the first number from stdin
local new_val = io.read"*n"
-- Append all numbers passed in
-- until there are no more numbers (io.read'*n' = nil)
while new_val do
  values[#values+1] = new_val
  new_val = io.read"*n"
end

-- Print the max
print(math.max(unpack(values)))

```


## M2000 Interpreter


```M2000 Interpreter

Module TestThis {
      Print "Search a tuple type list (is an array also)"
      A=(,)
      For i=1 to Random(1,10)
      Append A, (Random(1,100),)
      Next
      Print Len(A)
      Print A
      Print A#max()

      Print "Search an array"
      B=lambda->Random(1,100)
      Rem Dim A(1 to Random(1,10))<<B()
      Dim A(1 to Random(1,10))<<lambda->{=Random(1,100)}()
      Print Len(A())
      Print A()
      Print A()#max()

      \\ #max() skip non numeric values
      Rem Print (1,"100",3)#max()=3

      Print "Search an inventory list"
      Inventory C
      for i=1 to Random(1,10)
            do
                  key=random(10000)
            until not exist(c, key)
            \\ we can put a number as string
            if random(1,2)=1 then Append c, key:=B() else Append c, key:=str$(B())
      Next

      \\ if inventory item is string with a number work fine
      Function MaxItem(a) {
            k=each(a,2)
            val=a(0!)
            while k
                  \\ using stack of values
                  \\ over -equal to over 1 - copy value from 1 to top, means double the top value
                  \\ number - pop top value
                  \\ drop -equal to drop 1 : drop top value
                  Push a(k^!): Over : If Number>val then Read Val else drop
                  Rem If a(k^!)>Val Then Val=a(k^!)
            end while
            =val
      }
      Print Len(C)
      Print C
      Print MaxItem(C)

      Print "Search a stack object"
      \\ a stack object is the same as the stack of values
      \\ which always is present
      D=stack
      I=0
      J=Random(1,10)
      \\ Stack stackobjext {}
      \\ hide current stack and attach the D stack
      Stack D {
            Push B() : I++ : IF I>J Else Loop
      }
      \\ if stack item isn't numeric we get a run time error
      Function MaxItemStack(a) {
            Stack a {env$=envelope$()}
            if replace$("N","", env$)<>"" then error "only numbers allowed"
            k=each(a,2)
            val=Stackitem(a,1)
            while k
                  If Stackitem(k)>val then Val=stackitem(k)
            end while
            =val
      }
      Print Len(D)
      Print D
      Print MaxItemStack(D)
}
TestThis

```



## Maple

This is a built-in, polymorphic procedure in Maple.

```Maple>
 max( { 1, 2,  Pi, exp(1) } ); # set
                                   Pi

> max( [ 1, 2,  Pi, exp(1) ] ); # list
                                   Pi

> max( 1, 2,  Pi, exp(1) ); # sequence
                                   Pi

> max( Array( [ 1, 2,  Pi, exp(1) ] ) ); # Array
                                   Pi
```

For numeric data in (multi-dimensional) rtables, a particularly flexible and powerful method for finding the maximum (and many other things) is the use of "rtable_scanblock".  The maximum of an Array is a built-in rtable_scanblock operation and can be found as follows.

```Maple>
 A := Array([1,2,4/5,3,11]): rtable_scanblock( A, [rtable_dims(A)], Maximum );
                                   11
```


=={{header|Mathematica}} / {{header|Wolfram Language}}==
Input:

```Mathematica
Max[1, 3, 3, 7]
Max[Pi,E+2/5,17 Cos[6]/5,Sqrt[91/10]]
Max[1,6,Infinity]
Max[]
```

```txt
 7
 17 Cos[6]/5
 Infinity
-Infinity
```

Note that Max returns minus infinity if supplied with no arguments; as it should:

```Mathematica
Max[Max[],Max[a,b,c]]
Max[Max[a],Max[b,c]]
Max[Max[a,b],Max[c]]
Max[Max[a,b,c],Max[]]
```

should all give the same results, therefore max[] should give -Infinity. If it WOULD give 0 strange this can happen:

```Mathematica
Max[Max[], Max[-4, -3]]
```

WOULD give 0 instead of -3


## MATLAB


```Matlab
function [maxValue] = findmax(setOfValues)
   maxValue = max(setOfValues);
```



## Maxima


```maxima
u : makelist(random(1000), 50)$

/* Three solutions */
lreduce(max, u);

apply(max, u);

lmax(u);
```



## MAXScript

MAXScript has a built-in function called amax(), which will return the maximum of an array or the values supplied to it.
The following custom function will return the maximum of the array supplied to it, or 'undefined' if an empty array is supplied.

```MAXScript
fn MaxValue AnArray =
(
	if AnArray.count != 0 then
	(
		local maxVal = 0
		for i in AnArray do if i > maxVal then maxVal = i
		maxVal
	)
	else undefined
)
```



## Metafont


The <code>max</code> macro (in the base set of macro for Metafont) accepts any number of arguments, and accepts both numerics (numbers), pairs (bidimensional vectors), and strings (not mixed).


```metafont
show max(4,5,20,1);
show max((12,3), (10,10), (25,5));
show max("hello", "world", "Hello World");
```



## min

```min
(
  'bool                             ;does the list have any elements?
  (-inf ('> 'pop 'nip if) reduce)   ;do if so
  ({"empty seq" :error "Cannot find the maximum element of an empty sequence" :message} raise)  ;do if not
  if
) :seq-max
```


=={{header|MK-61/52}}==
<lang>П0	С/П	x=0	07	ИП0	x<0	00	max	БП	00
```


or

<lang>П0	ИП0	С/П	-	x<0	01	Вx	П0	БП	01
```


=={{header|Modula-3}}==
Modula-3 provides a builtin <code>MAX</code> function, but it only works on two elements (or enumerations) but not arrays or sets.

We provide a generic Maximum implementation:

```modula3
GENERIC INTERFACE Maximum(Elem);

EXCEPTION Empty;

PROCEDURE Max(READONLY a: ARRAY OF Elem.T): Elem.T RAISES {Empty};

END Maximum.
```



```modula3
GENERIC MODULE Maximum(Elem);

PROCEDURE Max(READONLY arr: ARRAY OF Elem.T): Elem.T RAISES {Empty} =
  VAR max := FIRST(Elem.T);
  BEGIN
    IF NUMBER(arr) = 0 THEN
      RAISE Empty;
    END;
    FOR i := FIRST(arr) TO LAST(arr) DO
      IF arr[i] > max THEN
        max := arr[i];
      END;
    END;
    RETURN max;
  END Max;

BEGIN
END Maximum.
```


<code>Elem</code> can be instantiated to any type (any type that can be compared with the '>' function).  For convenience Modula-3 provides interfaces/modules for the built in types, like Integer, Real, LongReal, etc, which contain type definitions as well as properties specific to the type.

To make a generic interface/module for a specific type, you must instantiate it:

```modula3
INTERFACE RealMax = Maximum(Real) END RealMax.
```


```modula3
MODULE RealMax = Maximum(Real) END RealMax.
```


Now we can import <code>RealMax</code> into our source and use the <code>Max</code> function:

```modula3
MODULE Main;

IMPORT RealMax, IO, Fmt;

VAR realarr := ARRAY [1..5] OF REAL {1.1, 1.0, 0.0, 2.4, 3.3};

BEGIN
  IO.Put(Fmt.Real(RealMax.Max(realarr)) & "\n");
END Main.
```



## MontiLang

MontiLang has a builtin statement <code>MAX</code> which finds the maximum of the top two items on the stack. By looping through an array and pushing to the stack, the largest item in an array can be found.

```MontiLang
2 5 3 12 9 9 56 2 ARR

LEN VAR l .
0 VAR i .
FOR l
    GET i SWAP
    i 1 + VAR i .
ENDFOR .
STKLEN 1 - VAR st .
FOR st
    MAX
ENDFOR PRINT
```


Another way to do it.


```MontiLang
2 5 3 12 9 9 56 2 ARR
print
LEN VAR l .
0 VAR i .
0
FOR l
    swap
    GET i rot max
    i 1 + VAR i .
ENDFOR
|Greatest number in the list: | out . print
|Press ENTER to exit | input
clear
```



## MUMPS


```MUMPS

MV(A,U)
 ;A is a list of values separated by the string U
 NEW MAX,T,I
 FOR I=1:1 SET T=$PIECE(A,U,I) QUIT:T=""  S MAX=$SELECT(($DATA(MAX)=0):T,(MAX<T):T,(MAX>=T):MAX)
 QUIT MAX

```

Usage:

```txt

USER>SET V=","

USER>SET B="-1,-1000,1000,2.3E5,8A,""A"",F"

USER>W $$MV^ROSETTA(B,V)
2.3E5

```



## Neko


```ActionScript
/**
 greatest element from a list (Neko Array)
 Tectonics:
   nekoc greatest-element.neko
   neko greatest-element
*/

var greatest = function(list) {
  var max, element;
  var pos = 1;

  if $asize(list) > 0  max = list[0];

  while pos < $asize(list) {
    element = list[pos];
    if max < element  max = element;
    pos += 1;
  }

  return max;
}

$print(greatest($array(5, 1, 3, 5)), "\n");
$print(greatest($array("abc", "123", "zyx", "def")), "\n");
```


```txt
prompt$ nekoc greatest-element.neko
prompt$ neko ./greatest-element.n
5
zyx
```



## Nemerle


```Nemerle
using System;
using Nemerle.Collections;
using System.Linq;
using System.Console;

module SeqMax
{
    SeqMax[T, U] (this seq : T) : U
      where T : Seq[U]
      where U : IComparable
    {
        $[s | s in seq].Fold(seq.First(), (x, y) => {if (x.CompareTo(y) > 0) x else y})
    }

    Main() : void
    {
        def numbers = [1, 12, 3, -5, 6, 23];
        def letters = ['s', 'p', 'a', 'm'];

        // using SeqMax() method (as task says to "create a function")
        WriteLine($"numbers.SeqMax() = $(numbers.SeqMax())");
        WriteLine($"letters.SeqMax() = $(letters.SeqMax())");

        // using the already available Max() method
        WriteLine($"numbers.Max() = $(numbers.Max())");
        WriteLine($"letters.Max() = $(letters.Max())")
    }
}
```


```txt
numbers.SeqMax() = 23
letters.SeqMax() = s
numbers.Max() = 23
letters.Max() = s
```



## NetRexx


```NetRexx
/* NetRexx */

options replace format comments java crossref savelog symbols binary

rn = Random()
maxElmts = 100
dlist = double[maxElmts]
rlist = Rexx[maxElmts]
loop r_ = 0 to maxElmts - 1
  nr = rn.nextGaussian * 100.0
  dlist[r_] = nr
  rlist[r_] = Rexx(nr)
  end r_

say 'Max double:' Rexx(getMax(dlist)).format(4, 9)
say 'Max   Rexx:' getMax(rlist).format(4, 9)

return

method getMax(dlist = double[]) public static binary returns double
  dmax = Double.MIN_VALUE
  loop n_ = 0 to dlist.length - 1
    if dlist[n_] > dmax then dmax = dlist[n_]
    end n_
  return dmax

method getMax(dlist = Rexx[]) public static binary returns Rexx
  dmax = Rexx(Double.MIN_VALUE)
  loop n_ = 0 to dlist.length - 1
    dmax = dlist[n_].max(dmax)
    end n_
  return dmax

```

```txt

Max double:  274.457568703
Max   Rexx:  274.457568703

```



## NewLISP


```NewLISP
(max 1 2 3 5 2 3 4)
```



```NEWLISP
(apply max '(1 2 3 5 2 3 4)) ; apply to list
; Added by Nehal-Singhal 2018-05-29
```



## Nial

The behavior of multi-dimensional arrays is like J


```nial
max 1 2 3 4
=4
```



## Nim


```Nim
echo max([2,3,4,5,6,1])
```


```txt
6
```


=={{header|Oberon-2}}==
```oberon2

MODULE GreatestElement1;
IMPORT
  ADT:ArrayList,
  Object:Boxed,
  Out;

VAR
  a: ArrayList.ArrayList(Boxed.LongInt);
  max: Boxed.LongInt;


  PROCEDURE Max(al: ArrayList.ArrayList(Boxed.LongInt)): Boxed.LongInt;
  VAR
    i: LONGINT;
    item, max: Boxed.LongInt;
  BEGIN
    max := NEW(Boxed.LongInt,MIN(LONGINT));

    i := 0;
    WHILE (i < al.size) DO
      item := al.Get(i);
      IF item.value > max.value THEN max := item END;
      INC(i)
    END;
    RETURN max
  END Max;

BEGIN
  a := NEW(ArrayList.ArrayList(Boxed.LongInt),5);
  a.Append(NEW(Boxed.LongInt,10));
  a.Append(NEW(Boxed.LongInt,32));
  a.Append(NEW(Boxed.LongInt,4));
  a.Append(NEW(Boxed.LongInt,43));
  a.Append(NEW(Boxed.LongInt,9));

  max := Max(a);
  Out.String("Max: ");Out.LongInt(max.value,4);Out.Ln
END GreatestElement1.

```


Simple version

```oberon2

MODULE GreatestElement2;
IMPORT
  Out;
VAR
  a: ARRAY 10 OF LONGINT;

  PROCEDURE Max(a: ARRAY OF LONGINT): LONGINT;
  VAR
    i, max: LONGINT;
  BEGIN
    max := MIN(LONGINT);
    FOR i := 0 TO LEN(a) - 1 DO
      IF a[i] > max THEN max := a[i] END;
    END;

    RETURN max
  END Max;
BEGIN
  a[0] := 10;
  a[1] := 32;
  a[2] := 4;
  a[3] := 43;
  a[4] := 9;

  Out.String("Max: ");Out.LongInt(Max(a),4);Out.Ln
END GreatestElement2.

```

{{out}}(in both programs):

```txt

Max:   43

```



## Objeck

The language has a "Max" method for vectors.

```objeck

values := IntVector->New([4, 1, 42, 5]);
values->Max()->PrintLine();

```


=={{header|Objective-C}}==
This code "extends" (through Objective-C categories) the NSArray adding the method
<code>maximumValue</code>; this one iterates over the objects of the collection
calling the method <code>compare</code>, if it exists for the object of the
collection. Since normally comparing makes sense between numbers, the code
also check if the objects being compared are of "kind" NSNumber. If one eliminates
this check (substituting it maybe with one that checks if the two object are of
the same "kind"), the code is able to get a maximum value for any objects for
which make sense a compare method (e.g. strings), that must be implemented.

If there's no a known way of comparing two objects of the collection (or if the
objects are not "NSNumber"), the the method return nil (the void object).



```objc
#import <Foundation/Foundation.h>


@interface NSArray (WithMaximum)
- (id)maximumValue;
@end

@implementation NSArray (WithMaximum)
- (id)maximumValue
{
  if ( [self count] == 0 ) return nil;
  id maybeMax = self[0];
  for ( id el in self ) {
    if ( [maybeMax respondsToSelector: @selector(compare:)] &&
	 [el respondsToSelector: @selector(compare:)]       &&
	 [el isKindOfClass: [NSNumber class]]               &&
	 [maybeMax isKindOfClass: [NSNumber class]] ) {
      if ( [maybeMax compare: el] == NSOrderedAscending )
	maybeMax = el;
    } else { return nil; }
  }
  return maybeMax;
}
@end
```


This example mixes integers with a double value, just to show that
everything is fine until they are NSNumber.


```objc
int main()
{
  @autoreleasepool {
    NSArray *collection = @[@1, @2, @10, @5, @10.5];

    NSLog(@"%@", [collection maximumValue]);
  }
  return 0;
}
```



## OCaml


```ocaml
let my_max = function
    [] -> invalid_arg "empty list"
  | x::xs -> List.fold_left max x xs
```


 # my_max [4;3;5;9;2;3] ;;
 - : int = 9


## Octave


Octave's <code>max</code> accepts a vector (and can return also the index of the maximum value in the vector)


```octave
m = max( [1,2,3,20,10,9,8] );       % m = 20
[m, im] = max( [1,2,3,20,10,9,8] ); % im = 4
```



## Oforth



```Oforth
[1, 2.3, 5.6, 1, 3, 4 ] reduce(#max)
```



## Ol

Basics:

```scheme

; builtin function
(max 1 2 3 4 5) ; 5

(define x '(1 2 3 4 5))

; using to numbers list
(apply max x) ; 5

; using list reducing
(fold max (car x) x) ; 5

; manual lambda-comparator
(print (fold (lambda (a b)
   (if (less? a b) b a))
   (car x) x)) ; 5

```



## ooRexx


### version


```ooRexx

-- routine that will work with any ordered collection or sets and bags containing numbers.
::routine listMax
  use arg list
  items list~makearray   -- since we're dealing with different collection types, reduce to an array
  if items~isEmpty then return .nil   -- return a failure indicator.  could also raise an error, if desired
  largest = items[1]


  -- note, this method does call max one extra time.  This could also use the
  -- do i = 2 to items~size to avoid this
  do item over items
     largest = max(item, largest)
  end

  return largest

```



### version 2 works with any strings

<lang>/* REXX ***************************************************************
* 30.07.2013 Walter Pachl as for REXX
**********************************************************************/
s=.list~of('Walter','lives','in','Vienna')
say listMax(s)
-- routine that will work with any ordered collection or sets and bags.
::routine listMax
  use arg list
  items=list~makearray   -- since we're dealing with different collection types, reduce to an array
  if items~isEmpty then return .nil   -- return a failure indicator.  could also raise an error, if desired
  largest = items[1]
  -- note, this method uses one extra comparison.  It could use
  -- do i = 2 to items~size to avoid this
  do item over items
     If item>>largest Then
       largest = item
  end
  return largest
```



## Oz


```oz
declare
  fun {Maximum X|Xr}         %% pattern-match on argument to make sure the list is not empty
     {FoldL Xr Value.max X}  %% fold the binary function Value.max over the list
  end
in
  {Show {Maximum [1 2 3 4 3]}}
```



## PARI/GP


```parigp
vecmax(v)
```



## Pascal

See [[Greatest_element_of_a_list#Delphi | Delphi]]
or try this, for it shows the according position

```pascal
program GElemLIst;
{$IFNDEF FPC}
  {$Apptype Console}
{$else}
  {$Mode Delphi}
{$ENDIF}

uses
  sysutils;
const
  MaxCnt = 1000000;
type
   tMaxIntPos= record
                  mpMax,
                  mpPos : integer;
                end;
   tMaxfltPos= record
                  mpMax : double;
                  mpPos : integer;
                end;


function FindMaxInt(const ia: array of integer):tMaxIntPos;
//delivers the highest Element and position of integer array
var
  i  : NativeInt;
  tmp,max,ps: integer;
Begin
  max := -MaxInt-1;
  ps := -1;
  //i = index of last Element
  i := length(ia)-1;
  IF i>=0 then Begin
    max := ia[i];
    ps := i;
    dec(i);
    while i> 0 do begin
      tmp := ia[i];
      IF max< tmp then begin
        max := tmp;
        ps := i;
        end;
      dec(i);
      end;
    end;
  result.mpMax := Max;
  result.mpPos := ps;
end;

function FindMaxflt(const ia: array of double):tMaxfltPos;
//delivers the highest Element and position of double array
var
  i,
  ps: NativeInt;
  max : double;
  tmp : ^double;//for 32-bit version runs faster

Begin
  max := -MaxInt-1;
  ps := -1;
  //i = index of last Element
  i := length(ia)-1;
  IF i>=0 then Begin
    max := ia[i];
    ps := i;
    dec(i);
    tmp := @ia[i];
    while i> 0 do begin
      IF tmp^>max  then begin
        max := tmp^;
        ps := i;
        end;
      dec(i);
      dec(tmp);
      end;
    end;
  result.mpMax := Max;
  result.mpPos := ps;
end;

var
  IntArr : array of integer;
  fltArr : array of double;
  ErgInt : tMaxINtPos;
  ErgFlt : tMaxfltPos;
  i: NativeInt;
begin
  randomize;
  setlength(fltArr,MaxCnt); //filled with 0
  setlength(IntArr,MaxCnt); //filled with 0.0
  For i := High(fltArr) downto 0 do
    fltArr[i] := MaxCnt*random();
  For i := High(IntArr) downto 0 do
    IntArr[i] := round(fltArr[i]);

  ErgInt := FindMaxInt(IntArr);
  writeln('FindMaxInt ',ErgInt.mpMax,' @ ',ErgInt.mpPos);

  Ergflt := FindMaxflt(fltArr);
  writeln('FindMaxFlt ',Ergflt.mpMax:0:4,' @ ',Ergflt.mpPos);
end.
```

Out, because of the searchdirection Position of FindMaxFlt is below FindMaxInt


```txt

FindMaxInt 999999 @ 691620
FindMaxFlt 999999.0265 @ 14824
```



## Perl


```perl
sub max {
    my $max = shift;
    for (@_) { $max = $_ if $_ > $max }
    return $max;
}
```


It is already implemented in the module <tt>List::Util</tt>'s <tt>max()</tt> function:

```perl
use List::Util qw(max);

max(@values);
```



## Perl 6

The built-in function works with any type that defines ordering.

```perl6
say max 10, 4, 5, -2, 11;
say max <zero one two three four five six seven eight nine>;

# Even when the values and number of values aren't known until runtime
my @list = flat(0..9,'A'..'H').roll((^60).pick).rotor(4,:partial)».join.words;
say @list, ': ', max @list;

```

```txt
11
zero
[6808 013C 6D5B 4219 29G9 DC13 CA4F 55F3 AA06 0AGF DAB0 2]: DC13
```



## Phix


```Phix
?max({1,1234,62,234,12,34,6})
?max({"ant", "antelope", "dog", "cat", "cow", "wolf", "wolverine", "aardvark"})
```

```txt

1234
"wolverine"

```



## PHP

The built-in PHP function <tt>max()</tt> already does this.

```php
max($values)
```



## PicoLisp


```PicoLisp
: (max 2 4 1 3)               # Return the maximal argument
-> 4
: (apply max (2 4 1 3))       # Apply to a list
-> 4
: (maxi abs (2 -4 -1 3))      # Maximum according to given function
-> -4
```



## PL/I


```pli

maximum = A(lbound(A,1));
do i = lbound(A,1)+1 to hbound(A,1);
  if maximum < A(i) then maximum = A(i);
end;

```



## PostScript


Ghostscript has a <code>max</code> built-in:

```postscript
/findmax {
  dup 0 get exch    % put the first element underneath the array
  {max} forall      % replace it by the respective larger value if necessary
} def
```


If not using Ghostscript this gets a bit longer:


```postscript
/findmax {
  dup 0 get exch    % put the first element underneath the array
  {
    dup             % duplicate the current item
    2 index         % duplicate the current maximum value
    gt              % if the current item is larger
    {exch} if       % swap the two items so the previous maximum is now the top of the stack
    pop             % remove it
  } forall
} def
```


```postscript

[1 2 3 4 5 4 3 2 1] uncons exch {max} fold

```



## PowerBASIC



```powerbasic
FUNCTION PBMAIN()
    DIM x AS LONG, y AS LONG, z AS LONG
    RANDOMIZE TIMER

    FOR x = 1 TO 10
        y = INT(RND * 10000)
        z = MAX(y, z)
    NEXT

    ? STR$(z) & " was the highest value"
END FUNCTION
```


```txt

 8104 was the highest value

```



## PowerShell

The <code>Measure-Object</code> cmdlet in PowerShell already has this capability:

```powershell
function Get-Maximum ($a) {
    return ($a | Measure-Object -Maximum).Maximum
}
```



## Prolog

SWI-Prolog already knows max_list.

```Prolog
 ?- max_list([1, 2, 10, 3, 0, 7, 9, 5], M).
M = 10.
```


can be implemented like this:


```Prolog
max_list(L, V) :-
	select(V, L, R), \+((member(X, R), X > V)).

```



## PureBasic


```PureBasic
Procedure.f Max (Array a.f(1))
   Protected last, i, ret.f

   ret = a(0)
   last = ArraySize(a())
   For i = 1 To last
      If ret < a(i)
         ret = a(i)
      EndIf
   Next

   ProcedureReturn ret
EndProcedure
```


### PureBasic: another solution


```purebasic
Procedure.f maxelement(List tl.f())
  ForEach tl() : mx.f=mx*Bool(mx>=tl())+tl()*Bool(mx<tl()) : Next
  ProcedureReturn mx
EndProcedure

NewList testlist.f() : OpenConsole()
For i=0 To 99 : AddElement(testlist()) : testlist()=Sqr(Random(1000)) : Next
Print("Greatest element = "+StrF(maxelement(testlist()),8)) : Input()
```

```txt
Greatest element = 31.59113884
```



## Python

The built-in Python function <tt>max()</tt> already does this.

```python
max(values)
```


Of course this assumes we have a list or tuple (or other sequence like object).  (One can even find the ''max()'' or ''min()'' character of a string since that's treated as a sequence of characters and there are "less than" and "greater than" operations (object methods) associate with those characters).

If we truly were receiving a stream of data then in Python, such streams are usually iterable, meaning they have a way of generating one item at a time from the stream.

max(), (and min()), can take iterables and a key argument which takes a function that can transform each item into a type that we can compare, for example, if the stream were returning string representations of integers, one to a line, you could do

```python
>>>
 floatstrings = ['1\n', ' 2.3\n', '4.5e-1\n', '0.01e4\n', '-1.2']
>>> max(floatstrings, key = float)
'0.01e4\n'
>>>
```

Normally we would want the converted form as the maximum and we could just as easily write:

```python
>>>
 max(float(x) for x in floatstrings)
100.0
>>>
```

Or you can write your own functional version, of the maximum function, using reduce and lambda

```python
>>>
 mylist = [47, 11, 42, 102, 13]
>>> reduce(lambda a,b: a if (a > b) else b, mylist)
102
```



## Q


```Q
q)l:2 9 3 8 4 7
q)max l
9
```



## R


```R
v <- c(1, 2, 100, 50, 0)
print(max(v)) # 100
```



## Racket

The "max" function it built in and takes an arbitrary amount of arguments.

```racket
(max 12 9 8 17 1)
```

```txt
17
```


To use with a list, there is <tt>apply</tt>:

```racket
(apply max '(12 9 8 17 1))
```


However, if you want to write the function yourself:

```racket

(define (my-max l)
  (define (max-h l greatest)
    (cond [(empty? l) greatest]
          [(> (first l) greatest) (max-h (rest l) (first l))]
          [else (max-h (rest l) greatest)]))
  (if (empty? l) empty (max-h l (first l))))

```


or with a "for" loop:

```racket

(define (my-max l)
  (for/fold ([max #f]) ([x l])
    (if (and max (> max x)) max x)))

```



## RapidQ


```vb
functioni FindMax(...) as double
    dim x as integer

    for x = 1 to ParamValCount
        IF ParamVal(x) > Result THEN Result = ParamVal(x)
    next
End functioni

Print FindMax(50, 20, 65, 20, 105)

```



## Rascal

Rascal has a built-in function that gives the greatest element of a list

```rascal

rascal>import List;
ok

rascal>max([1,2,3,4]);
int: 4

```



## Raven


```Raven
[ 1 2 3 4 ] max "%d\n" print
```

```txt
4
```


'''Randomly generated list size and elements'''

```Raven
100 choose as $cnt
[ ]  as $lst
0 $cnt 1 range each drop 100 choose $lst push
$lst print
$lst max "max value: %d\n" print
```



## REBOL


```REBOL
REBOL [
    Title: "Maximum Value"
    URL: http://rosettacode.org/wiki/Maximum_Value
]

max: func [
	"Find maximum value in a list."
	values [series!] "List of values."
] [
	first maximum-of values
]

print ["Max of"  mold d: [5 4 3 2 1]  "is"  max d]
print ["Max of"  mold d: [-5 -4 -3 -2 -1]  "is"  max d]
```


```txt
Max of [5 4 3 2 1] is 5
Max of [-5 -4 -3 -2 -1] is -1
```



## Red


```Red
Red []
list: [1 2 3 5 4]
print  last sort list

```


## REXX

The numbers in the list may be any valid REXX number   (integer, negative, floating point, etc.)

### using a list


```rexx
/*REXX program finds the  greatest element  in a list (of the first 25 reversed primes).*/
$ = reverse(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97)
say 'list of numbers = '  $                      /*show the original list of numbers.   */
big=word($, 1)                                   /*choose an initial biggest number.    */
# = words($);        do j=2  to #                /*traipse through the list,  find max. */
                     big=max(big, word($, j) )   /*use the  MAX  BIF to find the biggie.*/
                     end   /*j*/
say                                              /*stick a fork in it,  we're all done. */
say 'the biggest value in a list of '     #      " numbers is: "     big
```

```txt

list of numbers =  79 98 38 97 37 17 76 16 95 35 74 34 14 73 13 92 32 91 71 31 11 7 5 3 2

the biggest value in a list of  25  numbers is:  98

```



### using an array


```rexx
/*REXX program finds the  greatest element  in a list (of the first 25 reversed primes).*/
@.=;       @.1 = 2;    @.2 = 3;    @.3 = 5;    @.4 = 7;    @.5 =11;    @.6 =31;    @.7 =71
           @.8 =91;    @.9 =32;    @.10=92;    @.11=13;    @.12=73;    @.13=14;    @.14=34
           @.15=74;    @.16=35;    @.17=95;    @.18=16;    @.19=76;    @.20=17;    @.21=37
           @.22=97;    @.23=38;    @.24=98;    @.25=79
big=@.1                                          /*choose an initial biggest number.    */
                do #=2  while @.#\==''           /*traipse through whole array of nums. */
                big = max(big, @.#)              /*use a BIF to find the biggest number.*/
                end   /*#*/
                                                 /*stick a fork in it,  we're all done. */
say 'the biggest value in an array of '      #-1       " elements is: "           big
```

```txt

the biggest value in an array of  25  elements is:  98

```



### using a list from the terminal


```rexx
/*REXX program finds the  greatest element  in a list of numbers entered at the terminal*/
say '──────────────────  Please enter a list of numbers  (separated by blanks or commas):'
parse pull $;           #=words($)               /*get a list of numbers from terminal. */
$=translate($, , ',')                            /*change all commas  (,)  to  blanks.  */
big=word($,1);          do j=2  to #             /*traipse through the list of numbers. */
                        big=max(big, word($,j))  /*use a BIF for finding the max number.*/
                        end   /*j*/
say                                              /*stick a fork in it,  we're all done. */
say '────────────────── The biggest value in the list of '    #    " elements is: "    big
```

Programming note:   the   '''max'''   BIF normalizes the number returned (eliding the leading superfluous zeroes).

If this is undesirable, then the   '''do'''   loop (shown above) can be replaced with:

```rexx
···;                    do j=2  to #;  _=word($,j)
                        if _>big  then big=_
                        end   /*j*/
```

```txt

──────────────────  Please enter a list of numbers  (separated by blanks or commas):
-12  -3  0  1.1  1e2  99.2  00245                   ◄■■■■■■■■■■ user input

────────────────── The biggest value in the list of  7  elements is:  245

```



### list of any strings


```rexx
/* REXX ***************************************************************
* If the list contains any character strings, the following will work
* Note the use of >> (instead of >) to avoid numeric comparison
* Note that max() overrides the builtin function MAX
* 30.07.2013 Walter Pachl
**********************************************************************/
list='Walter Pachl living in Vienna'
Say max(list)
list='8 33 -12'
Say max(list)
Exit
max: Procedure
Parse Arg l
max=word(l,1)
Do i=2 To words(l)
  If word(l,i)>>max Then
    max=word(l,i)
  End
Return max
```

```txt

living
8

```

```txt

Walter
8

```

'''output''' when using a ''list'' which is:   <tt> 12 111111 1 </tt>
(lexigraphically 12 is greater than 111111)

```txt

12

```



## Ring


```ring
aList = [1,2,4,5,10,6,7,8,9]
see max(aList)
```

```txt

10

```



## Ruby

<tt>max</tt> is a method of all Enumerables

```ruby
values.max
```



## Run BASIC


```Runbasic
list$= "1 12 -55 46 41 3.66 19"
while word$(list$,i+1," ") <> ""
  mx = max(mx,val(word$(list$,i+1," ")))
  i = i + 1
wend
print mx
```



## Rust

This is built in functionality for everything that can be iterated over. It returns an Option<T>, meaning Some(e) if there are elements in the iterator and None if it is empty.

```rust
fn main() {
    let nums = [1,2,39,34,20];
    println!("{:?}", nums.iter().max());
    println!("{}", nums.iter().max().unwrap());
}
```


```txt
Some(39)
39
```


=={{header|S-lang}}==
Starting w/an array, this is trivial:
<lang S-lang>variable a = [5, -2, 0, 4, 666, 7];
print(max(a));
```


output:
666

If a is a list instead of an array, then:
<lang S-lang>a = {5, -2, 0, 4, 666, 7};
print(max(list_to_array(a)));
```



## Scala

```Scala
def noSweat(list: Int*) = list.max
// Test
assert(noSweat(1, 3, 12, 7) == 12)
```



## Scheme

The built-in Scheme function <tt>max</tt> takes the max of all its arguments.

```scheme
(max 1 2 3 4)
(apply max values) ; find max of a list
```



## Seed7


```seed7
$ include "seed7_05.s7i";

const func integer: max (in array integer: values) is func
  result
    var integer: max is 0;
  local
    var integer: index is 0;
  begin
    max := values[1];
    for index range 2 to length(values) do
      if values[index] > max then
        max := values[index];
      end if;
    end for;
  end func;

const proc: main is func
  begin
    writeln(max([] (1, 2, 6, 4, 3)));
  end func;
```


```txt

6

```



## Self

Using ''reduceWith:'' it is very simple to find the maximum value among a collection.


```self
(1 & 2 & 3 & 4 & 20 & 10 & 9 & 8) asVector reduceWith: [:a :b | a max: b] "returns 20"
```


Or, since it's "built-in", you can simply do:


```self
(1 & 2 & 3 & 4 & 20 & 10 & 9 & 8) asVector max. "returns 20"
```



## Sidef

''max'' method returns the greatest element in a list. It works only if the array's elements have the same type (e.g.: strings, numbers).

```ruby
values.max;
```



## Slate


```slate
#(1 2 3 4 20 10 9 8) reduce: [| :a :b | a max: b]
```



## Smalltalk

Using ''fold'' it is very simple to find the maximum value among a collection.


```smalltalk
#(1 2 3 4 20 10 9 8) fold: [:a :b | a max: b] "returns 20"
```


Or, since it's "built-in", you can simply do:

```smalltalk
#(1 2 3 4 20 10 9 8) max. "returns 20"
```


using #inject:into:

```smalltalk

| list |
list := #(1 2 3 4 20 10 9 8).
list inject: (list at: 1) into: [ :number :each |
  number max: each ]

```



## SNOBOL4


```snobol4
while   a = trim(input)     :f(stop)
        max = gt(a,max) a   :(while)
stop    output = max
end
```



## Standard ML

Comparisons are specific for each type. Here is a max function for a list of ints:

```sml
fun max_of_ints [] = raise Empty
  | max_of_ints (x::xs) = foldl Int.max x xs
```


 - max_of_ints [4,3,5,9,2,3];
 val it = 9 : int


## Stata


Use the '''[https://www.stata.com/help.cgi?summarize summarize]''' command to compute the maximum value of a variable:


```stata
qui sum x
di r(max)
```


Mata has also several [https://www.stata.com/help.cgi?mf_minmax functions] to compute minimum or maximum of a vactor or matrix:


```stata
a = 1,5,3,4,2,7,9,8
max(a)
```




## Swift

```swift
if let x = [4,3,5,9,2,3].maxElement() {
  print(x) // prints 9
}
```

```swift
let x = maxElement([4,3,5,9,2,3])
println(x) // prints 9
```



## Tailspin


```tailspin

templates max
  @: $(1);
  $(2..-1)... -> #
  $@!
  <$@..> @: $;
end max

[1, 5, 20, 3, 9, 7] -> max -> !OUT::write
// outputs 20

```

Can also be written as an inline templates

```tailspin

[1, 5, 20, 3, 9, 7] -> (@: $(1); $(2..-1)... -> # $@ ! <$@..> @: $;) -> !OUT::write
// outputs 20

```

Or we can do just the matching in an inline templates referencing the outer state

```tailspin

templates max
  @: $(1);
  $(2..-1)... -> (<$@max..> @max: $;) -> !VOID
  $@!
end max

[1, 5, 20, 3, 9, 7] -> max -> !OUT::write
// outputs 20

```



## Tcl

Use the <code>{*}</code> expansion operator to substitute the list value with its constituent elements

```tcl
package require Tcl 8.5

set values {4 3 2 7 8 9}
::tcl::mathfunc::max {*}$values ;# ==> 9
```


=={{header|TI-83 BASIC}}==

The builtin <code>max</code> function can be applied to lists. <code style="font-family:'TI Uni'">max({1, 3, 2</code>.

=={{header|TI-89 BASIC}}==

The builtin <code>max</code> function can be applied to lists. <code style="font-family:'TI Uni'">max({1, 3, 2})</code> = 3.


## Trith


```trith
[1 -2 3.1415 0 42 7] [max] foldl1
```



## TUSCRIPT


```tuscript

$$ MODE TUSCRIPT
LOOP n,list="2'4'0'3'1'2'-12"
IF (n==1)          greatest=VALUE(list)
IF (list>greatest) greatest=VALUE(list)
ENDLOOP
PRINT greatest

```

```txt

4

```



## uBasic/4tH

Since uBasic/4tH has a stack, it's only logical to use it.
<lang>Push 13, 0, -6, 2, 37, -10, 12         ' Push values on the stack
Print "Maximum value = " ; FUNC(_FNmax(7))
End                                    ' We pushed seven values

_FNmax Param(1)
  Local(3)

  d@ = -(2^31)                         ' Set maximum to a tiny value

  For b@ = 1 To a@                     ' Get all values from the stack
    c@ = Pop()
    If c@ > d@ THEN d@ = c@            ' Change maximum if required
  Next
Return (d@)                            ' Return the maximum
```


## UNIX Shell

```bash
max() {
  local m=$1
  shift
  while [ $# -gt 0 ]
  do
    [ "$m" -lt "$1" ] && m=$1
    shift
  done
  echo "$m"
}

max 10 9 11 57 1 12
```


```bash
max() {
  m=$1  # Bourne Shell has no local command.
  shift
  while [ $# -gt 0 ]
  do
    [ "$m" -lt "$1" ] && m=$1
    shift
  done
  echo "$m"
}
```



## Ursa

The <code>max</code> function:

```ursa
def max (int<> list)
	decl int max i
	set max list<0>

	for (set i 1) (< i (- (size list) 1)) (inc i)
		if (> list<i> max)
			set max list<i>
		end if
	end for

	return max
end max
```


In use: (assuming the function is in the file <code>max.u</code>)

```txt
cygnus/x ursa v0.75 (default, release 0)
[Oracle Corporation JVM 1.8.0_51 on Mac OS X 10.10.5 x86_64]
> import "max.u"
> decl int<> list
> append 5 1 7 3 9 2 list
> out (max list) endl console
9
> _
```



## Ursala

The built-in <code>$^</code> operator takes a binary predicate of any type to a
function extracting the maximum value from a non-empty list of that type. In this
case it is used with <code>fleq</code>, the partial order relation on floating point
numbers.

```Ursala
#import flo

#cast %e

example = fleq$^ <-1.,-2.,0.,5.,4.,6.,1.,-5.>
```

```txt
6.000000e+00
```



## V

Assuming it is a list of positive numbers

```v
[4 3 2 7 8 9] 0 [max] fold
=9
```


If it is not

```v
[4 3 2 7 8 9] dup first [max] fold
```

 =9


## VBA


```vb
Option Explicit

Sub Main()
Dim a
   a = Array(1, 15, 19, 25, 13, 0, -125, 9)
   Debug.Print Max_VBA(a)
End Sub

Function Max_VBA(Arr As Variant) As Long
Dim i As Long, temp As Long
   temp = Arr(LBound(Arr))
   For i = LBound(Arr) + 1 To UBound(Arr)
      If Arr(i) > temp Then temp = Arr(i)
   Next i
   Max_VBA = temp
End Function
```


```txt
25
```



## VBScript


```vb

Function greatest_element(arr)
	tmp_num = 0
	For i = 0 To UBound(arr)
		If i = 0 Then
			tmp_num = arr(i)
		ElseIf arr(i) > tmp_num Then
			tmp_num = arr(i)
		End If
	Next
	greatest_element = tmp_num
End Function

WScript.Echo greatest_element(Array(1,2,3,44,5,6,8))

```


```txt
44
```



## Vim Script


for numbers (not floats):

```vim
max([1, 3, 2])
```

result: 3

for strings (with configurable ignore-case):

```vim
function! Max(list, ...)
    " {list}	list of strings
    " {a:1}	'i': ignore case, 'I': match case, otherwise use 'ignorecase' option
    if empty(a:list)
	return 0
    endif
    let gt_op = a:0>=1 ? get({'i': '>?', 'I': '>#'}, a:1, '>') : '>'
    let cmp_expr = printf('a:list[idx] %s maxval', gt_op)
    let maxval = a:list[0]
    let len = len(a:list)
    let idx = 1
    while idx < len
	if eval(cmp_expr)
	    let maxval = a:list[idx]
	endif
	let idx += 1
    endwhile
    return maxval
endfunction
```



## Visual Basic


```vb
Public Function ListMax(anArray())
    'return the greatest element in array anArray
    'use LBound and UBound to find its length
    n0 = LBound(anArray)
    n = UBound(anArray)
    theMax = anArray(n0)
    For i = (n0 + 1) To n
        If anArray(i) > theMax Then theMax = anArray(i)
    Next
    ListMax = theMax
End Function


Public Sub ListMaxTest()
    Dim b()
    'test function ListMax
    'fill array b with some numbers:
    b = Array(5992424433449#, 4534344439984#, 551344678, 99800000#)
    'print the greatest element
    Debug.Print "Greatest element is"; ListMax(b())
End Sub
```


Result:

```txt
ListMaxTest
Greatest element is 5992424433449
```



## Wart

Wart defines <code>max</code> in terms of the more general <code>best</code>.

```python
def (best f seq)
  if seq
    ret winner car.seq
      each elem cdr.seq
        if (f elem winner)
          winner <- elem

def (max ... args)
  (best (>) args)
```


<code>(&gt;)</code> is <code>&gt;</code> while suppressing infix expansion.


## WDTE


```wdte
let s =>
 import 'stream';
let a => import 'arrays';

let max list =>
  a.stream list
  -> s.extent 1 >
  -> at 0
  ;
```


<code>extent</code> is a standard library function that returns a sorted list of the elements of a stream that fit the given function best, so <code>&gt;</code> results in the maximum element.


## Wortel

The <code>@maxl</code> returns the maximum value of a list:

```wortel
@maxl [1 6 4 6 4 8 6 3] ; returns 8
```



## XPL0

The set of values is the lengths of the lines of text in the input file.


```XPL0
include c:\cxpl\codes;                  \include 'code' declarations

def  Tab=$09, LF=$0A, CR=$0D, EOF=$1A;

int  CpuReg, Hand;
char CmdTail($80);
int  I, Max, C;

[\Copy file name on command line, which is in the Program Segment Prefix (PSP)
\ ES=CpuReg(11), to the CmdTail array, which is in our Data Segment = CpuReg(12)
CpuReg:= GetReg;                        \point to copy of CPU registers
Blit(CpuReg(11), $81, CpuReg(12), CmdTail, $7F);
Hand:= FOpen(CmdTail, 0);               \open file for input and get its handle
FSet(Hand, ^I);                         \assign handle to device 3
OpenI(3);                               \initialize file for input

Max:= 0;                                \scan file for longest line
repeat  I:= 0;
        repeat  C:= ChIn(3);
                case C of
                  CR, LF, EOF:  [];     \don't count these characters
                  Tab:  [I:= I+8 & ~7]  \(every 8th column)
                other   I:= I+1;        \count all other characters
        until   C=LF or C=EOF;
        if I > Max then Max:= I;
until   C = EOF;
Text(0, "Longest line = ");  IntOut(0, Max);  CrLf(0);
]
```


Example of running the program on its source code:


```txt

maxline maxline.xpl
Longest line = 80

```



## XSLT


The desired value is the first in a sequence that has been sorted numerically in descending order.

```xml
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
	<xsl:output method="text"/>

	<xsl:template match="/">
		<xsl:for-each select="/*/*">
			<!-- without data-type="number", items are sorted alphabetically -->
			<xsl:sort data-type="number" order="descending"/>
			<xsl:if test="position() = 1">
				<xsl:value-of select="."/>
			</xsl:if>
		</xsl:for-each>
	</xsl:template>
</xsl:stylesheet>
```


Sample input:


```xml
<numbers>

	<number>3</number>
	<number>1</number>
	<number>12</number>
	<number>7</number>
</numbers>
```


```txt

 12

```



## Yabasic


```Yabasic
l$ = "1,1234,62,234,12,34,6"

dim n$(1)

n = token(l$, n$(), ", ")

for i = 1 to n
	t$ = n$(i)
	if t$ > m$ then m$ = t$ end if            // or: if t$ > m$ m$ = t$
	if val(t$) > m then m = val(t$) end if	  // or: if val(t$) > m m = val(t$)
next

print "Alphabetic order: ", m$, ", numeric order: ", m
```



## Yacas

Input:

```Yacas
Max({1, 3, 3, 7})
Max({Pi,Exp(1)+2/5,17*Cos(6)/5,Sqrt(91/10)})
Max({1,6,Infinity})
Max({})
```

```txt
 7
(17*Cos(2*Pi-6))/5
Infinity
Undefined
```


## Yorick

The built-in function ''max'' does this.
Interactive example:

```txt
> foo = [4, 3, 2, 7, 8, 9]
> max(foo)
9
```



## zkl


```zkl
(1).max(1,2,3)     //-->3
(66).max(1,2,3.14) //-->66
```

If given a list, the max of the list is returned. The number/object just selects the method to call. Notice the difference between Int.max and Float.max.

```zkl
(66).max(T(1,2,3)) //-->3
(66).max(T(1,2,3.14)) //-->3
(6.6).max(T(1,2,3.14)) //-->3.14
```

For other object types, you could use:

```zkl
fcn max{ vm.arglist.reduce(fcn(p,n){ if(p < n) n else p }) }
```


```txt
max(2,1,-40,50,2,4,2) //-->50
max(2) //-->2
max("foo","bar") //-->"foo"
max("3",4,"5") //-->"5" only if strings contain only digits

```



## ZX Spectrum Basic


```zxbasic
10 PRINT "Values"''
20 LET z=0
30 FOR x=1 TO INT (RND*10)+1
40 LET y=RND*10-5
50 PRINT y
60 LET z=(y AND y>z)+(z AND y<z)
70 NEXT x
80 PRINT '"Max. value = ";z
```

