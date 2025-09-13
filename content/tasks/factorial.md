+++
title = "Factorial"
description = ""
date = 2019-10-20T04:00:30Z
aliases = []
[extra]
id = 2989
[taxonomies]
categories = ["task", "Arithmetic operations"]
tags = []
+++

;Definitions:
:*   The factorial of   '''0'''   (zero)   is [[wp:Factorial#Definition|defined]] as being   1   (unity).
:*   The   '''Factorial Function'''   of a positive integer,   <big> ''n'', </big>   is defined as the product of the sequence:
                 <big><big> ''n'',   ''n''-1,   ''n''-2,   ...   1 </big></big>


## Task

Write a function to return the factorial of a number.

Solutions can be iterative or recursive.

Support for trapping negative   <big> ''n'' </big>   errors is optional.


## Related tasks

*   [[Primorial numbers]]





## 0815

This is an iterative solution which outputs the factorial of each number supplied on standard input.


```0815
}:r:        Start reader loop.
  |~  	    Read n,
  #:end:    if n is 0 terminates
  >=        enqueue it as the initial product, reposition.
  }:f:      Start factorial loop.
    x<:1:x- Decrement n.
    {=*>    Dequeue product, position n, multiply, update product.
  ^:f:
  {+%       Dequeue incidental 0, add to get Y into Z, output fac(n).
  <:a:~$    Output a newline.
^:r:
```


```txt
seq 6 | 0815 fac.0
1
2
6
18
78
2d0
```



## 360 Assembly

For maximum compatibility, this program uses only the basic instruction set.

```360asm
FACTO    CSECT
         USING  FACTO,R13
SAVEAREA B      STM-SAVEAREA(R15)
         DC     17F'0'
         DC     CL8'FACTO'
STM      STM    R14,R12,12(R13)
         ST     R13,4(R15)
         ST     R15,8(R13)
         LR     R13,R15         base register and savearea pointer
         ZAP    N,=P'1'         n=1
LOOPN    CP     N,NN            if n>nn
         BH     ENDLOOPN        then goto endloop
         LA     R1,PARMLIST
         L      R15,=A(FACT)
         BALR   R14,R15         call fact(n)
	 ZAP    F,0(L'R,R1)     f=fact(n)
DUMP     EQU    *
	 MVC    S,MASK
	 ED     S,N
         MVC    WTOBUF+5(2),S+30
	 MVC    S,MASK
	 ED     S,F
         MVC    WTOBUF+9(32),S
         WTO    MF=(E,WTOMSG)
	 AP     N,=P'1'         n=n+1
	 B      LOOPN
ENDLOOPN EQU    *
RETURN   EQU    *
         L      R13,4(0,R13)
         LM     R14,R12,12(R13)
         XR     R15,R15
         BR     R14
FACT     EQU    *               function FACT(l)
         L      R2,0(R1)
         L      R3,12(R2)
         ZAP    L,0(L'N,R2)     l=n
         ZAP    R,=P'1'         r=1
         ZAP    I,=P'2'         i=2
LOOP     CP     I,L             if i>l
         BH     ENDLOOP         then goto endloop
	 MP     R,I             r=r*i
	 AP     I,=P'1'         i=i+1
	 B      LOOP
ENDLOOP  EQU    *
         LA     R1,R            return r
         BR     R14             end function FACT
         DS     0D
NN       DC     PL16'29'
N        DS     PL16
F        DS     PL16
C        DS     CL16
II       DS     PL16
PARMLIST DC     A(N)
S        DS     CL33
MASK     DC     X'40',29X'20',X'212060'  CL33
WTOMSG   DS     0F
         DC     H'80',XL2'0000'
WTOBUF   DC     CL80'FACT(..)=................................ '
L        DS     PL16
R        DS     PL16
I        DS     PL16
         LTORG
         YREGS
         END    FACTO
```

```txt
FACT(29)= 8841761993739701954543616000000
```



## ABAP


### Iterative


```ABAP
form factorial using iv_val type i.
  data: lv_res type i value 1.
  do iv_val times.
    multiply lv_res by sy-index.
  enddo.

  iv_val = lv_res.
endform.
```


### Recursive


```ABAP
form fac_rec using iv_val type i.
  data: lv_temp type i.

  if iv_val = 0.
    iv_val = 1.
  else.
    lv_temp = iv_val - 1.
    perform fac_rec using lv_temp.
    multiply iv_val by lv_temp.
  endif.
endform.
```



## ActionScript


### Iterative


```actionscript
public static function factorial(n:int):int
{
    if (n < 0)
        return 0;

    var fact:int = 1;
    for (var i:int = 1; i <= n; i++)
        fact *= i;

    return fact;
}
```


### Recursive


```actionscript
public static function factorial(n:int):int
{
   if (n < 0)
       return 0;

   if (n == 0)
       return 1;

   return n * factorial(n - 1);
}
```



## Ada


### Iterative


```ada
function Factorial (N : Positive) return Positive is
   Result : Positive := N;
   Counter : Natural := N - 1;
begin
   for I in reverse 1..Counter loop
      Result := Result * I;
   end loop;
   return Result;
end Factorial;
```


### Recursive


```ada
function Factorial(N : Positive) return Positive is
   Result : Positive := 1;
begin
   if N > 1 then
      Result := N * Factorial(N - 1);
   end if;
   return Result;
end Factorial;
```


### Numerical Approximation


```ada
with Ada.Numerics.Generic_Complex_Types;
with Ada.Numerics.Generic_Complex_Elementary_Functions;
with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Text_IO.Complex_Io;
with Ada.Text_Io; use Ada.Text_Io;

procedure Factorial_Numeric_Approximation is
   type Real is digits 15;
   package Complex_Pck is new Ada.Numerics.Generic_Complex_Types(Real);
   use Complex_Pck;
   package Complex_Io is new Ada.Text_Io.Complex_Io(Complex_Pck);
   use Complex_IO;
   package Cmplx_Elem_Funcs is new Ada.Numerics.Generic_Complex_Elementary_Functions(Complex_Pck);
   use Cmplx_Elem_Funcs;

   function Gamma(X : Complex) return Complex is
      package Elem_Funcs is new Ada.Numerics.Generic_Elementary_Functions(Real);
      use Elem_Funcs;
      use Ada.Numerics;
      -- Coefficients used by the GNU Scientific Library
      G : Natural := 7;
      P : constant array (Natural range 0..G + 1) of Real := (
         0.99999999999980993, 676.5203681218851, -1259.1392167224028,
         771.32342877765313, -176.61502916214059, 12.507343278686905,
         -0.13857109526572012, 9.9843695780195716e-6, 1.5056327351493116e-7);
      Z : Complex := X;
      Cx : Complex;
      Ct : Complex;
   begin
      if Re(Z) < 0.5 then
         return Pi / (Sin(Pi * Z) * Gamma(1.0 - Z));
      else
         Z := Z - 1.0;
         Set_Re(Cx, P(0));
         Set_Im(Cx, 0.0);
         for I in 1..P'Last loop
            Cx := Cx + (P(I) / (Z + Real(I)));
         end loop;
         Ct := Z + Real(G) + 0.5;
         return Sqrt(2.0 * Pi) * Ct**(Z + 0.5) * Exp(-Ct) * Cx;
      end if;
   end Gamma;

   function Factorial(N : Complex) return Complex is
   begin
      return Gamma(N + 1.0);
   end Factorial;
   Arg : Complex;
begin
   Put("factorial(-0.5)**2.0 = ");
   Set_Re(Arg, -0.5);
   Set_Im(Arg, 0.0);
   Put(Item => Factorial(Arg) **2.0, Fore => 1, Aft => 8, Exp => 0);
   New_Line;
   for I in 0..9 loop
      Set_Re(Arg, Real(I));
      Set_Im(Arg, 0.0);
      Put("factorial(" & Integer'Image(I) & ") = ");
      Put(Item => Factorial(Arg), Fore => 6, Aft => 8, Exp => 0);
      New_Line;
   end loop;
end Factorial_Numeric_Approximation;
```

```txt

factorial(-0.5)**2.0 = (3.14159265,0.00000000)
factorial( 0) = (     1.00000000,     0.00000000)
factorial( 1) = (     1.00000000,     0.00000000)
factorial( 2) = (     2.00000000,     0.00000000)
factorial( 3) = (     6.00000000,     0.00000000)
factorial( 4) = (    24.00000000,     0.00000000)
factorial( 5) = (   120.00000000,     0.00000000)
factorial( 6) = (   720.00000000,     0.00000000)
factorial( 7) = (  5040.00000000,     0.00000000)
factorial( 8) = ( 40320.00000000,     0.00000000)
factorial( 9) = (362880.00000000,     0.00000000)

```



## Agda


```Agda
factorial : ℕ → ℕ
factorial zero = 1
factorial (suc n) = suc n * factorial n
```



## Aime


### Iterative


```aime
integer
factorial(integer n)
{
    integer i, result;

    result = 1;
    i = 1;
    while (i < n) {
        i += 1;
        result *= i;
    }

    return result;
}
```



## ALGOL 68


### Iterative


```algol68
PROC factorial = (INT upb n)LONG LONG INT:(
  LONG LONG INT z := 1;
  FOR n TO upb n DO z *:= n OD;
  z
); ~
```



### Numerical Approximation

```algol68
INT g = 7;
[]REAL p = []REAL(0.99999999999980993, 676.5203681218851,   -1259.1392167224028,
                771.32342877765313,   -176.61502916214059,     12.507343278686905,
                 -0.13857109526572012,   9.9843695780195716e-6, 1.5056327351493116e-7)[@0];

PROC complex gamma = (COMPL in z)COMPL: (
  # Reflection formula #
  COMPL z := in z;
  IF re OF z < 0.5 THEN
    pi / (complex sin(pi*z)*complex gamma(1-z))
  ELSE
    z -:= 1;
    COMPL x := p[0];
    FOR i TO g+1 DO x +:= p[i]/(z+i) OD;
    COMPL t := z + g + 0.5;
    complex sqrt(2*pi) * t**(z+0.5) * complex exp(-t) * x
  FI
);

OP ** = (COMPL z, p)COMPL: ( z=0|0|complex exp(complex ln(z)*p) );
PROC factorial = (COMPL n)COMPL: complex gamma(n+1);

FORMAT compl fmt = $g(-16, 8)"⊥"g(-10, 8)$;

test:(
  printf(($q"factorial(-0.5)**2="f(compl fmt)l$, factorial(-0.5)**2));
  FOR i TO 9 DO
    printf(($q"factorial("d")="f(compl fmt)l$, i, factorial(i)))
  OD
)

```

```txt

 factorial(-0.5)**2=      3.14159265⊥0.00000000
 factorial(1)=      1.00000000⊥0.00000000
 factorial(2)=      2.00000000⊥0.00000000
 factorial(3)=      6.00000000⊥0.00000000
 factorial(4)=     24.00000000⊥0.00000000
 factorial(5)=    120.00000000⊥0.00000000
 factorial(6)=    720.00000000⊥0.00000000
 factorial(7)=   5040.00000000⊥0.00000000
 factorial(8)=  40320.00000000⊥0.00000000
 factorial(9)= 362880.00000000⊥0.00000000

```



### Recursive


```algol68
PROC factorial = (INT n)LONG LONG INT:
  CASE n+1 IN
    1,1,2,6,24,120,720 # a brief lookup #
  OUT
    n*factorial(n-1)
  ESAC
; ~
```


=={{header|ALGOL-M}}==
<lang>INTEGER FUNCTION FACTORIAL( N ); INTEGER N;
BEGIN
    INTEGER I, FACT;
    FACT := 1;
    FOR I := 2 STEP 1 UNTIL N DO
        FACT := FACT * I;
    FACTORIAL := FACT;
END;
```



## ALGOL W

Iterative solution

```algolw
begin
    % computes factorial n iteratively                                       %
    integer procedure factorial( integer value n ) ;
        if n < 2
        then 1
        else begin
            integer f;
            f := 2;
            for i := 3 until n do f := f * i;
            f
        end factorial ;

    for t := 0 until 10 do write( "factorial: ", t, factorial( t ) );

end.
```



## AmigaE

Recursive solution:

```amigae
PROC fact(x) IS IF x>=2 THEN x*fact(x-1) ELSE 1

PROC main()
  WriteF('5! = \d\n', fact(5))
ENDPROC
```


Iterative:

```amigae
PROC fact(x)
  DEF r, y
  IF x < 2 THEN RETURN 1
  r := 1; y := x;
  FOR x := 2 TO y DO r := r * x
ENDPROC r
```



## AntLang

AntLang is a functional language, but it isn't made for recursion - it's made for list processing.

```AntLang
factorial:{1 */ 1+range[x]} /Call: factorial[1000]
```



## Apex


### Iterative


```apex
public static long fact(final Integer n) {
    if (n < 0) {
        System.debug('No negative numbers');
        return 0;
    }
    long ans = 1;
    for (Integer i = 1; i <= n; i++) {
        ans *= i;
    }
    return ans;
}
```


### Recursive


```apex
public static long factRec(final Integer n) {
    if (n < 0){
        System.debug('No negative numbers');
        return 0;
    }
    return (n < 2) ? 1 : n * fact(n - 1);
}
```



## APL

APL provides a factorial function:

```apl
      !6
720
```

But, if we want to reimplement it, we can start by noting that <i>n</i>! is found by multiplying together a vector of integers 1, 2... <i>n</i>. This definition ('multiply'—'together'—'integers from 1 to'—'<i>n</i>') can be expressed directly in APL notation:

```apl
      FACTORIAL←{×/⍳⍵}
```

And the resulting function can then be used instead of the (admittedly more convenient) builtin one:

```apl
      FACTORIAL 6
720
```



## AppleScript


### Iteration


```AppleScript
on factorial(x)
    if x < 0 then return 0
    set R to 1
    repeat while x > 1
        set {R, x} to {R * x, x - 1}
    end repeat
    return R
end factorial
```



### Recursion


Curiously, this recursive version executes a little faster than the iterative version above.
(Perhaps because the iterative code is making use of list splats)


```AppleScript
-- factorial :: Int -> Int
on factorial(x)
    if x > 1 then
        x * (factorial(x - 1))
    else if x = 1 then
        1
    else
        0
    end if
end factorial
```



### Fold

We can also define factorial as '''foldl(product, 1, enumFromTo(1, x))'''


```AppleScript
-- FACTORIAL -----------------------------------------------------------------

-- factorial :: Int -> Int
on factorial(x)
    script product
        on |λ|(a, b)
            a * b
        end |λ|
    end script

    foldl(product, 1, enumFromTo(1, x))
end factorial


-- TEST ----------------------------------------------------------------------
on run

    factorial(11)

    --> 39916800

end run


-- GENERIC FUNCTIONS ---------------------------------------------------------

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
39916800
```



## Applesoft BASIC


### Iterative


```ApplesoftBasic
100 N = 4 : GOSUB 200"FACTORIAL
110 PRINT N
120 END

200 N = INT(N)
210 IF N > 1 THEN FOR I = N - 1 TO 2 STEP -1 : N = N * I : NEXT I
220 RETURN
```


### Recursive


```ApplesoftBasic
 10 A = 768:L = 7
 20  DATA 165,157,240,3
 30  DATA 32,149,217,96
 40  FOR I = A TO A + L
 50  READ B: POKE I,B: NEXT
 60 H = 256: POKE 12,A / H
 70  POKE 11,A -  PEEK (12) * H
 80  DEF  FN FA(N) =  USR (N < 2) + N *  FN FA(N - 1)
 90  PRINT  FN FA(4)
```
http://hoop-la.ca/apple2/2013/usr-if-recursive-fn/


## Arendelle


```txt
&lt; n &gt;

{ @n = 0 ,
   ( return , 1 )
,
   ( return ,
       @n * !factorial( @n - ! )
   )
}

```



## ARM Assembly

```ARM Assembly


/* ARM assembly Raspberry PI  */
/*  program factorial.s   */

/* Constantes    */
.equ STDOUT, 1     @ Linux output console
.equ EXIT,   1     @ Linux syscall
.equ WRITE,  4     @ Linux syscall

/*********************************/
/* Initialized data              */
/*********************************/
.data
szMessLargeNumber:   .asciz "Number N to large. \n"
szMessNegNumber:      .asciz "Number N is negative. \n"

szMessResult:  .ascii "Resultat = "      @ message result
sMessValeur:   .fill 12, 1, ' '
                   .asciz "\n"
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

    mov r0,#-5
    bl factorial
    mov r0,#10
    bl factorial
    mov r0,#20
    bl factorial


100:   @ standard end of the program
    mov r0, #0                  @ return code
    pop {fp,lr}                 @restaur 2 registers
    mov r7, #EXIT              @ request to exit program
    swi 0                       @ perform the system call


/********************************************/
/*     calculation                         */
/********************************************/
/* r0 contains number N */
factorial:
    push {r1,r2,lr}    	@ save  registres
    cmp r0,#0
    blt 99f
    beq 100f
    cmp r0,#1
    beq 100f
    bl calFactorial
    cmp r0,#-1          @ overflow ?
    beq 98f
    ldr r1,iAdrsMessValeur
    bl conversion10       @ call function with 2 parameter (r0,r1)
    ldr r0,iAdrszMessResult
    bl affichageMess            @ display message
    b 100f

98:   @ display error message
    ldr r0,iAdrszMessLargeNumber
    bl affichageMess
    b 100f
99:  @ display error message
    ldr r0,iAdrszMessNegNumber
    bl affichageMess

100:
    pop {r1,r2,lr}    			@ restaur registers
    bx lr	        			@ return
iAdrszMessNegNumber:       .int szMessNegNumber
iAdrszMessLargeNumber:	    .int szMessLargeNumber
iAdrsMessValeur:            .int sMessValeur
iAdrszMessResult:          .int szMessResult
/******************************************************************/
/*     calculation                         */
/******************************************************************/
/* r0 contains the number N */
calFactorial:
    cmp r0,#1          @ N = 1 ?
    bxeq lr           @ yes -> return
    push {fp,lr}    		@ save  registers
    sub sp,#4           @ 4 byte on the stack
    mov fp,sp           @ fp <- start address stack
    str r0,[fp]                    @ fp contains  N
    sub r0,#1          @ call function with N - 1
    bl calFactorial
    cmp r0,#-1         @ error overflow ?
    beq 100f         @ yes -> return
    ldr r1,[fp]       @ load N
    umull r0,r2,r1,r0   @ multiply result by N
    cmp r2,#0           @ r2 is the hi rd  if <> 0 overflow
    movne r0,#-1      @ if overflow  -1 -> r0

100:
    add sp,#4            @ free 4 bytes on stack
    pop {fp,lr}    			@ restau2 registers
    bx lr	        		@ return

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


### Recursive



```arturo
Factorial [n]{
	if n>0 {
		n * $(Factorial n-1)
	} { 1 }
}
```



### Fold



```arturo
factorial [n] {
	fold $(range 1 n) 1 { &0*&1 }
}
```



### Product



```arturo
factorial [n]{ product $(range 1 n) }
```



## AsciiDots



```AsciiDots

/---------*--~-$#-&
| /--;---\| [!]-\
| *------++--*#1/
| | /1#\ ||
[*]*{-}-*~<+*?#-.
*-------+-</
\-#0----/

```



## ATS



### Iterative


```ATS

fun
fact
(
  n: int
) : int = res where
{
  var n: int = n
  var res: int = 1
  val () = while (n > 0) (res := res * n; n := n - 1)
}

```



### Recursive


```ATS

fun
factorial
  (n:int): int =
  if n > 0 then n * factorial(n-1) else 1
// end of [factorial]

```


===Tail-recursive===

```ATS

fun
factorial
  (n:int): int = let
  fun loop(n: int, res: int): int =
    if n > 0 then loop(n-1, n*res) else res
in
  loop(n, 1)
end // end of [factorial]

```



## AutoHotkey


### Iterative


```AutoHotkey
MsgBox % factorial(4)

factorial(n)
{
  result := 1
  Loop, % n
    result *= A_Index
  Return result
}
```



### Recursive


```AutoHotkey
MsgBox % factorial(4)

factorial(n)
{
  return n > 1 ? n-- * factorial(n) : 1
}
```



## AutoIt


### Iterative


```AutoIt
;AutoIt Version: 3.2.10.0
MsgBox (0,"Factorial",factorial(6))
Func factorial($int)
    If $int < 0 Then
      Return 0
   EndIf
   $fact = 1
   For $i = 1 To $int
        $fact = $fact * $i
   Next
   Return $fact
EndFunc
```


### Recursive


```AutoIt
;AutoIt Version: 3.2.10.0
MsgBox (0,"Factorial",factorial(6))
Func factorial($int)
   if $int < 0 Then
      return 0
   Elseif $int == 0 Then
      return 1
   EndIf
   return $int * factorial($int - 1)
EndFunc
```



## AWK

'''Recursive'''

```awk
function fact_r(n)
{
  if ( n <= 1 ) return 1;
  return n*fact_r(n-1);
}
```


'''Iterative'''

```awk
function fact(n)
{
  if ( n < 1 ) return 1;
  r = 1
  for(m = 2; m <= n; m++) {
    r *= m;
  }
  return r
}
```



## Axe

'''Iterative'''

```axe
Lbl FACT
1→R
For(I,1,r₁)
 R*I→R
End
R
Return
```


'''Recursive'''

```axe
Lbl FACT
r₁??1,r₁*FACT(r₁-1)
Return
```




## Babel



### Iterative


```babel
((main
    {(0 1 2 3 4 5 6 7 8 9 10)
    {fact ! %d nl <<}
    each})

(fact
       {({dup 0 =}{ zap 1 }
         {dup 1 =}{ zap 1 }
         {1      }{ <- 1 {iter 1 + *} -> 1 - times })
        cond}))
```



### Recursive


```babel
((main
    {(0 1 2 3 4 5 6 7 8 9 10)
    {fact ! %d nl <<}
    each})

(fact
       {({dup 0 =}{ zap 1 }
         {dup 1 =}{ zap 1 }
         {1      }{ dup 1 - fact ! *})
        cond}))

```


When run, either code snippet generates the following
```txt

1
1
2
6
24
120
720
5040
40320
362880
3628800
```



## BaCon

Overflow occurs at 21 or greater.  Negative values treated as 0.

```freebasic
' Factorial
FUNCTION factorial(NUMBER n) TYPE NUMBER
    IF n <= 1 THEN
        RETURN 1
    ELSE
        RETURN n * factorial(n - 1)
    ENDIF
END FUNCTION

n = VAL(TOKEN$(ARGUMENT$, 2))
PRINT n, factorial(n) FORMAT "%ld! = %ld\n"
```


```txt
prompt$ ./factorial 0
0! = 1
prompt$ ./factorial 20
20! = 2432902008176640000
```



## bash


### Recursive


```bash
factorial()
{
  if [ $1 -le 1 ]
  then
    echo 1
  else
    result=$(factorial $[$1-1])
    echo $((result*$1))
  fi
}

```



## BASIC


### Iterative

```freebasic
FUNCTION factorial (n AS Integer) AS Integer
    DIM f AS Integer, i AS Integer
    f = 1
    FOR  i = 2 TO n
        f = f*i
    NEXT i
    factorial = f
END FUNCTION
```



### Recursive

```freebasic
FUNCTION factorial (n AS Integer) AS Integer
    IF n < 2 THEN
        factorial = 1
    ELSE
        factorial = n * factorial(n-1)
    END IF
END FUNCTION
```


=
## Commodore BASIC
=

```commodorebasic
10 REM FACTORIAL
20 REM COMMODORE BASIC 2.0
30 N = 10 : GOSUB 100
40 PRINT N"! ="F
50 END
100 REM FACTORIAL CALC USING SIMPLE LOOP
110 F = 1
120 FOR I=1 TO N
130   F = F*I
140 NEXT
150 RETURN
```


==={{header|IS-BASIC}}===
<lang IS-BASIC>100 DEF FACT(N)
110   LET F=1
120   FOR I=2 TO N
130     LET F=F*I
140   NEXT
150   LET FACT=F
160 END DEF
```


=
## Sinclair ZX81 BASIC
=

### =Iterative=


```basic
 10 INPUT N
 20 LET FACT=1
 30 FOR I=2 TO N
 40 LET FACT=FACT*I
 50 NEXT I
 60 PRINT FACT
```

```txt
13
```

```txt
6227020800
```


### =Recursive=

A <code>GOSUB</code> is just a procedure call that doesn't pass parameters.

```basic
 10 INPUT N
 20 LET FACT=1
 30 GOSUB 60
 40 PRINT FACT
 50 STOP
 60 IF N=0 THEN RETURN
 70 LET FACT=FACT*N
 80 LET N=N-1
 90 GOSUB 60
100 RETURN
```

```txt
13
```

```txt
6227020800
```



## BASIC256


### Iterative


```vb
print "enter a number, n = ";
input n
print string(n) + "! = " + string(factorial(n))

function factorial(n)
   factorial = 1
   if n > 0 then
      for p = 1 to n
      factorial *= p
      next p
   end if
end function
```



### Recursive


```BASIC256
print "enter a number, n = ";
input n
print string(n) + "! = " + string(factorial(n))

function factorial(n)
   if n > 0 then
      factorial = n * factorial(n-1)
   else
      factorial = 1
   end if
end function
```



## Batch File


```dos
@echo off
set /p x=
set /a fs=%x%-1
set y=%x%
FOR /L %%a IN (%fs%, -1, 1) DO SET /a y*=%%a
if %x% EQU 0 set y=1
echo %y%
pause
exit
```



## BBC BASIC

18! is the largest that doesn't overflow.

```bbcbasic
      *FLOAT64
      @% = &1010

      PRINT FNfactorial(18)
      END

      DEF FNfactorial(n)
      IF n <= 1 THEN = 1 ELSE = n * FNfactorial(n-1)
```

```txt
6402373705728000
```



## bc


```bc
#! /usr/bin/bc -q

define f(x) {
  if (x <= 1) return (1); return (f(x-1) * x)
}
f(1000)
quit
```




## beeswax


Infinite loop for entering <code>n</code> and getting the result <code>n!</code>:


```beeswax
        p      <
_>1FT"pF>M"p~.~d
      >Pd  >~{Np
 d             <
```


Calculate <code>n!</code> only once:


```beeswax
       p      <
_1FT"pF>M"p~.~d
     >Pd  >~{;
```


Limits for UInt64 numbers apply to both examples.

Examples:
<code>i</code> indicates that the program expects the user to enter an integer.


```julia>julia
 beeswax("factorial.bswx")
i0
1
i1
1
i2
2
i3
6
i10
3628800
i22
17196083355034583040
```


Input of negative numbers forces the program to quit with an error message.


## Befunge


```befunge
&1\>  :v v *<
   ^-1:_$>\:|
         @.$<
```



## Bracmat

Compute 10! and checking that it is 3628800, the esoteric way

```bracmat
      (
      =
        .   !arg:0&1
          |   !arg
            *   ( (
                  =   r
                    .   !arg:?r
                      &
                        ' (
                          .   !arg:0&1
                            | !arg*(($r)$($r))$(!arg+-1)
                          )
                  )
                $ (
                  =   r
                    .   !arg:?r
                      &
                        ' (
                          .   !arg:0&1
                            | !arg*(($r)$($r))$(!arg+-1)
                          )
                  )
                )
              $ (!arg+-1)
      )
    $ 10
  : 3628800

```


This recursive lambda function is made in the following way (see http://en.wikipedia.org/wiki/Lambda_calculus):

Recursive lambda function for computing factorial.

    g := λr. λn.(1, if n = 0; else n × (r r (n-1)))
    f := g g

or, translated to Bracmat, and computing 10!


```bracmat
      ( (=(r.!arg:?r&'(.!arg:0&1|!arg*(($r)$($r))$(!arg+-1)))):?g
    & (!g$!g):?f
    & !f$10
    )
```


The following is a straightforward recursive solution. Stack overflow occurs at some point, above 4243! in my case (Win XP).

   factorial=.!arg:~>1|!arg*factorial$(!arg+-1)

   factorial$4243
   (13552 digits, 2.62 seconds) 52254301882898638594700346296120213182765268536522926.....0000000

Lastly, here is an iterative solution


```bracmat
(factorial=
  r
.   !arg:?r
  &   whl
    ' (!arg:>1&(!arg+-1:?arg)*!r:?r)
  & !r
);
```


    factorial$5000
    (16326 digits) 422857792660554352220106420023358440539078667462664674884978240218135805270810820069089904787170638753708474665730068544587848606668381273 ... 000000

=={{header|Brainfuck}}==
Prints sequential factorials in an infinite loop.
<lang Brainfuck>>++++++++++>>>+>+[>>>+[-[<<<<<[+<<<<<]>>[[-]>[<<+>+>-]<[>+<-]<[>+<-[>+<-[>
+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>[-]>>>>+>+<<<<<<-[>+<-]]]]]]]]]]]>[<+>-
]+>>>>>]<<<<<[<<<<<]>>>>>>>[>>>>>]++[-<<<<<]>>>>>>-]+>>>>>]<[>++<-]<<<<[<[
>+<-]<<<<]>>[->[-]++++++[<++++++++>-]>>>>]<<<<<[<[>+>+<<-]>.<<<<<]>.>>>>]
```



## Brat


```brat
factorial = { x |
  true? x == 0 1 { x * factorial(x - 1)}
}
```



## Burlesque


Using the builtin ''Factorial'' function:


```burlesque

blsq ) 6?!
720

```


Burlesque does not have functions nor is it iterative. Burlesque's strength are its implicit loops.

Following examples display other ways to calculate the factorial function:


```burlesque

blsq ) 1 6r@pd
720
blsq ) 1 6r@{?*}r[
720
blsq ) 2 6r@(.*)\/[[1+]e!.*
720
blsq ) 1 6r@p^{.*}5E!
720
blsq ) 6ropd
720
blsq ) 7ro)(.*){0 1 11}die!
720

```




## embedded C for AVR MCU


###  Iterative


```c
long factorial(int n) {
    long result = 1;
    do {
        result *= n;
    while(--n);
    return result;
}
```



## C


###  Iterative


```c
int factorial(int n) {
    int result = 1;
    for (int i = 1; i <= n; ++i)
        result *= i;
    return result;
}

```


Handle negative n (returning -1)


```c
int factorialSafe(int n) {
    int result = 1;
    if(n<0)
        return -1;
    for (int i = 1; i <= n; ++i)
        result *= i;
    return result;
}

```



###  Recursive


```c
int factorial(int n) {
    return n == 0 ? 1 : n * factorial(n - 1);
}

```


Handle negative n (returning -1).


```c
int factorialSafe(int n) {
    return n<0 ? -1 : n == 0 ? 1 : n * factorialSafe(n - 1);
}

```



###  Tail Recursive

Safe with some compilers (for example: GCC with -O2, LLVM's clang)

```c
int fac_aux(int n, int acc) {
    return n < 1 ? acc : fac_aux(n - 1, acc * n);
}

int fac_auxSafe(int n, int acc) {
    return n<0 ? -1 : n < 1 ? acc : fac_aux(n - 1, acc * n);
}

int factorial(int n) {
    return fac_aux(n, 1);
}
```



### Obfuscated

This is simply beautiful, [http://www.ioccc.org/1995/savastio.c 1995 IOCCC winning entry by Michael Savastio], largest factorial possible : 429539!

```C

#include <stdio.h>

#define l11l 0xFFFF
#define ll1 for
#define ll111 if
#define l1l1 unsigned
#define l111 struct
#define lll11 short
#define ll11l long
#define ll1ll putchar
#define l1l1l(l) l=malloc(sizeof(l111 llll1));l->lll1l=1-1;l->ll1l1=1-1;
#define l1ll1 *lllll++=l1ll%10000;l1ll/=10000;
#define l1lll ll111(!l1->lll1l){l1l1l(l1->lll1l);l1->lll1l->ll1l1=l1;}\
lllll=(l1=l1->lll1l)->lll;ll=1-1;
#define llll 1000




                                                     l111 llll1 {
                                                     l111 llll1 *
      lll1l,*ll1l1        ;l1l1                      lll11 lll [
      llll];};main      (){l111 llll1                *ll11,*l1l,*
      l1, *ll1l, *    malloc ( ) ; l1l1              ll11l l1ll ;
      ll11l l11,ll  ,l;l1l1 lll11 *lll1,*            lllll; ll1(l
      =1-1 ;l< 14; ll1ll("\t\"8)>l\"9!.)>vl"         [l]^'L'),++l
      );scanf("%d",&l);l1l1l(l1l) l1l1l(ll11         ) (l1=l1l)->
      lll[l1l->lll[1-1]     =1]=l11l;ll1(l11         =1+1;l11<=l;
      ++l11){l1=ll11;         lll1 = (ll1l=(         ll11=l1l))->
      lll; lllll =(            l1l=l1)->lll;         ll=(l1ll=1-1
      );ll1(;ll1l->             lll1l||l11l!=        *lll1;){l1ll
      +=l11**lll1++             ;l1ll1 ll111         (++ll>llll){
      l1lll lll1=(              ll1l =ll1l->         lll1l)->lll;
      }}ll1(;l1ll;              ){l1ll1 ll111        (++ll>=llll)
      { l1lll} } *              lllll=l11l;}
      ll1(l=(ll=1-              1);(l<llll)&&
      (l1->lll[ l]              !=l11l);++l);        ll1 (;l1;l1=
      l1->ll1l1,l=              llll){ll1(--l        ;l>=1-1;--l,
      ++ll)printf(              (ll)?((ll%19)        ?"%04d":(ll=
      19,"\n%04d")              ):"%4d",l1->         lll[l] ) ; }
                                                     ll1ll(10); }

```


## C#

### Iterative


```c#
using System;

class Program
{
    static int Factorial(int number)
    {
        if(number < 0)
            throw new ArgumentOutOfRangeException(nameof(number), number, "Must be zero or a positive number.");

        var accumulator = 1;
        for (var factor = 1; factor <= number; factor++)
        {
            accumulator *= factor;
        }
        return accumulator;
    }

    static void Main()
    {
        Console.WriteLine(Factorial(10));
    }
}
```


### Recursive


```c#
using System;

class Program
{
    static int Factorial(int number)
    {
        if(number < 0)
            throw new ArgumentOutOfRangeException(nameof(number), number, "Must be zero or a positive number.");

        return number == 0 ? 1 : number * Factorial(number - 1);
    }

    static void Main()
    {
        Console.WriteLine(Factorial(10));
    }
}
```


### Tail Recursive


```c#
using System;

class Program
{
    static int Factorial(int number)
    {
        if(number < 0)
            throw new ArgumentOutOfRangeException(nameof(number), number, "Must be zero or a positive number.");

        return Factorial(number, 1);
    }

    static int Factorial(int number, int accumulator)
    {
        if(number < 0)
            throw new ArgumentOutOfRangeException(nameof(number), number, "Must be zero or a positive number.");
        if(accumulator < 1)
            throw new ArgumentOutOfRangeException(nameof(accumulator), accumulator, "Must be a positive number.");

        return number == 0 ? accumulator : Factorial(number - 1, number * accumulator);
    }

    static void Main()
    {
        Console.WriteLine(Factorial(10));
    }
}
```


### Functional


```c#
using System;
using System.Linq;

class Program
{
    static int Factorial(int number)
    {
        return Enumerable.Range(1, number).Aggregate((accumulator, factor) => accumulator * factor);
    }

    static void Main()
    {
        Console.WriteLine(Factorial(10));
    }
}
```


### Arbitrary Precision

Can calculate 250000! in under a minute.

```c#
using System;
using System.Numerics;
using System.Linq;
class Program
{
    static BigInteger factorial(int n) // iterative
    {
        BigInteger acc = 1; for (int i = 1; i <= n; i++) acc *= i; return acc;
    }

    static public BigInteger Factorial(int number) // functional
    {
        return Enumerable.Range(1, number).Aggregate(new BigInteger(1), (acc, num) => acc * num);
    }

    static void Main(string[] args)
    {
        Console.WriteLine(Factorial(250));
    }
}
```

```txt
3232856260909107732320814552024368470994843717673780666747942427112823747555111209488817915371028199450928507353189432926730931712808990822791030279071281921676527240189264733218041186261006832925365133678939089569935713530175040513178760077247933065402339006164825552248819436572586057399222641254832982204849137721776650641276858807153128978777672951913990844377478702589172973255150283241787320658188482062478582659808848825548800000000000000000000000000000000000000000000000000000000000000
```



## C++

The C versions work unchanged with C++, however, here is another possibility using the STL and boost:

```cpp
#include <boost/iterator/counting_iterator.hpp>
#include <algorithm>

int factorial(int n)
{
  // last is one-past-end
  return std::accumulate(boost::counting_iterator<int>(1), boost::counting_iterator<int>(n+1), 1, std::multiplies<int>());
}
```



### Iterative

This version of the program is iterative, with a while loop.

```cpp
//iteration with while
long long int factorial(long long int n)
{
   long long int r = 1;
   while(1<n)
       r *= n--;
   return r;
}
```



### Template


```cpp
template <int N>

struct Factorial
{
    enum { value = N * Factorial<N - 1>::value };
};

template <>
struct Factorial<0>
{
    enum { value = 1 };
};

// Factorial<4>::value == 24
// Factorial<0>::value == 1
void foo()
{
    int x = Factorial<4>::value; // == 24
    int y = Factorial<0>::value; // == 1
}
```


===Compare all Solutions (except the meta)===

```cpp
#include <iostream>
#include <chrono>
#include <vector>
#include <numeric>
#include <algorithm>
#include <boost/iterator/counting_iterator.hpp>

//bad style do-while and wrong for Factorial1(0LL) -> 0 !!!
long long int Factorial1(long long int m_nValue)
{
   long long int result=m_nValue;
   long long int result_next;
   long long int pc = m_nValue;
   do
   {
       result_next = result*(pc-1);
       result = result_next;
       pc--;
   }while(pc>2);
   m_nValue = result;
   return m_nValue;
}

//iteration with while
long long int Factorial2(long long int n)
{
   long long int r = 1;
   while(1<n)
       r *= n--;
   return r;
}

//recrusive
long long int Factorial3(long long int n)
{
   return n<2 ? 1 : n*Factorial3(n-1);
}

//tail recursive
inline long long int _fac_aux(long long int n, long long int acc) {
    return n < 1 ? acc : _fac_aux(n - 1, acc * n);
}
long long int Factorial4(long long int n)
{
   return _fac_aux(n,1);
}

//accumulate with functor
long long int Factorial5(long long int n)
{
  // last is one-past-end
  return std::accumulate(boost::counting_iterator<long long int>(1LL),
                         boost::counting_iterator<long long int>(n+1LL), 1LL,
                         std::multiplies<long long int>() );
}

//accumulate with lamda
long long int Factorial6(long long int n)
{
  // last is one-past-end
  return std::accumulate(boost::counting_iterator<long long int>(1LL),
                         boost::counting_iterator<long long int>(n+1LL), 1LL,
                         [](long long int a, long long int b) { return a*b; } );
}

int main()
{
    int v = 55;
    {
        auto t1 = std::chrono::high_resolution_clock::now();
        auto result = Factorial1(v);
        auto t2 = std::chrono::high_resolution_clock::now();
        std::chrono::duration<double, std::milli> ms = t2 - t1;
        std::cout << std::fixed << "do-while(1)              result " << result
                  << " took " << ms.count() << " ms\n";
    }

    {
        auto t1 = std::chrono::high_resolution_clock::now();
        auto result = Factorial2(v);
        auto t2 = std::chrono::high_resolution_clock::now();
        std::chrono::duration<double, std::milli> ms = t2 - t1;
        std::cout << std::fixed << "while(2)                 result " << result
                  << " took " << ms.count() << " ms\n";
    }

    {
        auto t1 = std::chrono::high_resolution_clock::now();
        auto result = Factorial3(v);
        auto t2 = std::chrono::high_resolution_clock::now();
        std::chrono::duration<double, std::milli> ms = t2 - t1;
        std::cout << std::fixed << "recusive(3)              result " << result
                  << " took " << ms.count() << " ms\n";
    }

    {
        auto t1 = std::chrono::high_resolution_clock::now();
        auto result = Factorial3(v);
        auto t2 = std::chrono::high_resolution_clock::now();
        std::chrono::duration<double, std::milli> ms = t2 - t1;
        std::cout << std::fixed << "tail recusive(4)         result " << result
                  << " took " << ms.count() << " ms\n";
    }

    {
        auto t1 = std::chrono::high_resolution_clock::now();
        auto result = Factorial5(v);
        auto t2 = std::chrono::high_resolution_clock::now();
        std::chrono::duration<double, std::milli> ms = t2 - t1;
        std::cout << std::fixed << "std::accumulate(5)       result " << result
                  << " took " << ms.count() << " ms\n";
    }

    {
        auto t1 = std::chrono::high_resolution_clock::now();
        auto result = Factorial6(v);
        auto t2 = std::chrono::high_resolution_clock::now();
        std::chrono::duration<double, std::milli> ms = t2 - t1;
        std::cout << std::fixed << "std::accumulate lamda(6) result " << result
                  << " took " << ms.count() << " ms\n";
    }
}
```


```txt

do-while(1)              result 6711489344688881664 took 0.000808 ms
while(2)                 result 6711489344688881664 took 0.000725 ms
recusive(3)              result 6711489344688881664 took 0.000730 ms
tail recusive(4)         result 6711489344688881664 took 0.000705 ms
std::accumulate(5)       result 6711489344688881664 took 0.000705 ms
std::accumulate lamda(6) result 6711489344688881664 took 0.000722 ms

```



## Cat

Taken direct from the Cat manual:

```Cat
define rec_fac
      { dup 1 <= [pop 1] [dec rec_fac *] if }
```



## Ceylon


```ceylon
shared void run() {

	Integer? recursiveFactorial(Integer n) =>
			switch(n <=> 0)
			case(smaller) null
			case(equal) 1
			case(larger) if(exists f = recursiveFactorial(n - 1)) then n * f else null;


	Integer? iterativeFactorial(Integer n) =>
			switch(n <=> 0)
			case(smaller) null
			case(equal) 1
			case(larger) (1:n).reduce(times);

	for(Integer i in 0..10) {
		print("the iterative factorial of     ``i`` is ``iterativeFactorial(i) else "negative"``
		       and the recursive factorial of ``i`` is ``recursiveFactorial(i) else "negative"``\n");
	}
}

```



## Chapel


```chapel
proc fac(n) {
	var r = 1;
	for i in 1..n do
		r *= i;

	return r;
}
```



## Chef


```Chef
Caramel Factorials.

Only reads one value.

Ingredients.
1 g Caramel
2 g Factorials

Method.
Take Factorials from refrigerator.
Put Caramel into 1st mixing bowl.
Verb the Factorials.
Combine Factorials into 1st mixing bowl.
Verb Factorials until verbed.
Pour contents of the 1st mixing bowl into the 1st baking dish.

Serves 1.
```



## ChucK


### Recursive

<lang>
0 => int total;
fun int factorial(int i)
{
    if (i == 0) return 1;
    else
    {
        i * factorial(i - 1) => total;
    }
    return total;
}

```


### Iterative

<lang>
1 => int total;
fun int factorial(int i)
{
    while(i > 0)
    {
        total * i => total;
        1 -=> i;
    }
    return total;
}

```



## Clay

Obviously there’s more than one way to skin a cat. Here’s a selection — recursive, iterative, and “functional” solutions.

```Clay
factorialRec(n) {
    if (n == 0) return 1;
    return n * factorialRec(n - 1);
}

factorialIter(n) {
    for (i in range(1, n))
        n *= i;
    return n;
}

factorialFold(n) {
    return reduce(multiply, 1, range(1, n + 1));
}
```


We could also do it at compile time, because — hey — why not?


```Clay
[n|n > 0] factorialStatic(static n) = n * factorialStatic(static n - 1);
overload factorialStatic(static 0) = 1;
```


Because a literal 1 has type Int32, these functions receive and return numbers of that type. We must be a bit more careful if we wish to permit other numeric types (e.g. for larger integers).


```Clay
[N|Integer?(N)] factorial(n: N) {
    if (n == 0) return N(1);
    return n * factorial(n - 1);
}
```


And testing:


```Clay
main() {
    println(factorialRec(5));           // 120
    println(factorialIter(5));          // 120
    println(factorialFold(5));          // 120
    println(factorialStatic(static 5)); // 120
    println(factorial(Int64(20)));      // 2432902008176640000
}
```



## CLIPS


```lisp
 (deffunction factorial (?a)
    (if (or (not (integerp ?a)) (< ?a 0)) then
        (printout t "Factorial Error!" crlf)
     else
        (if (= ?a 0) then
            1
         else
            (* ?a (factorial (- ?a 1))))))
```



## Clio



###  Recursive


```clio

fn factorial n:
  if n <= 1: n
  else:
    n * (n - 1 -> factorial)

10 -> factorial -> print

```



## Clojure



###  Folding


```lisp
(defn factorial [x]
  (apply * (range 2 (inc x))))
```



###  Recursive


```lisp
(defn factorial [x]
  (if (< x 2)
      1
      (* x (factorial (dec x)))))
```



###  Tail recursive


```lisp
(defn factorial [x]
  (loop [x x
         acc 1]
    (if (< x 2)
        acc
        (recur (dec x) (* acc x)))))
```



## CMake


```cmake
function(factorial var n)
  set(product 1)
  foreach(i RANGE 2 ${n})
    math(EXPR product "${product} * ${i}")
  endforeach(i)
  set(${var} ${product} PARENT_SCOPE)
endfunction(factorial)

factorial(f 12)
message("12! = ${f}")
```



## COBOL

The following functions have no need to check if their parameters are negative because they are unsigned.


###  Intrinsic Function

COBOL includes an intrinsic function which returns the factorial of its argument.

```cobol
MOVE FUNCTION FACTORIAL(num) TO result
```



###  Iterative


```cobol
       IDENTIFICATION DIVISION.
       FUNCTION-ID. factorial.

       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       01  i      PIC 9(10).

       LINKAGE SECTION.
       01  n      PIC 9(10).
       01  ret    PIC 9(10).

       PROCEDURE DIVISION USING BY VALUE n RETURNING ret.
           MOVE 1 TO ret

           PERFORM VARYING i FROM 2 BY 1 UNTIL n < i
               MULTIPLY i BY ret
           END-PERFORM

           GOBACK
           .
```



###  Recursive

```cobol
       IDENTIFICATION DIVISION.
       FUNCTION-ID. factorial.

       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       01  prev-n PIC 9(10).

       LINKAGE SECTION.
       01  n      PIC 9(10).
       01  ret    PIC 9(10).

       PROCEDURE DIVISION USING BY VALUE n RETURNING ret.
           IF n = 0
               MOVE 1 TO ret
           ELSE
               SUBTRACT 1 FROM n GIVING prev-n
               MULTIPLY n BY fac(prev-n) GIVING ret
           END-IF

           GOBACK
           .
```



## CoffeeScript

Several solutions are possible in JavaScript:


###  Recursive


```coffeescript
fac = (n) ->
  if n <= 1
    1
  else
    n * fac n-1
```



###  Functional

{{works with|JavaScript|1.8}} (See [https://developer.mozilla.org/en/Core_JavaScript_1.5_Reference/Objects/Array/reduce MDC])

```javascript
fac = (n) ->
  [1..n].reduce (x,y) -> x*y
```



## Comal

Recursive:

```Comal
  PROC Recursive(n) CLOSED
    r:=1
    IF n>1 THEN
      r:=n*Recursive(n-1)
    ENDIF
    RETURN r
  ENDPROC Recursive
```



## Comefrom0x10


This is iterative; recursion is not possible in Comefrom0x10.


```cf0x10
n = 5 # calculates n!
acc = 1

factorial
  comefrom

  comefrom accumulate if n < 1

accumulate
  comefrom factorial
  acc = acc * n
  comefrom factorial if n is 0
  n = n - 1

acc # prints the result
```



## Common Lisp

Recursive:

```lisp
(defun factorial (n)
  (if (zerop n) 1 (* n (factorial (1- n)))))
```


Tail Recursive:

```lisp
(defun factorial (n &optional (m 1))
  (if (zerop n) m (factorial (1- n) (* m n))))
```


Iterative:

```lisp
(defun factorial (n)
  "Calculates N!"
  (loop for result = 1 then (* result i)
     for i from 2 to n
     finally (return result)))
```


Functional:

```lisp
(defun factorial (n)
    (reduce #'* (loop for i from 1 to n collect i)))
```



### Alternate solution

I use [https://franz.com/downloads/clp/survey Allegro CL 10.1]


```lisp

;; Project : Factorial

(defun factorial (n)
          (cond ((= n 1) 1)
          (t (* n (factorial (- n 1))))))
(format t "~a" "factorial of 8: ")
(factorial 8)

```

Output:

```txt

factorial of 8: 40320

```



## Computer/zero Assembly

Both these programs find <math>x</math>!. Values of <math>x</math> higher than 5 are not supported, because their factorials will not fit into an unsigned byte.

### Iterative


```czasm
        LDA  x
        BRZ  done_i   ; 0! = 1

        STA  i

loop_i: LDA  fact
        STA  n

        LDA  i
        SUB  one
        BRZ  done_i

        STA  j

loop_j: LDA  fact
        ADD  n
        STA  fact

        LDA  j
        SUB  one
        BRZ  done_j

        STA  j
        JMP  loop_j

done_j: LDA  i
        SUB  one
        STA  i

        JMP  loop_i

done_i: LDA  fact
        STP

one:         1

fact:        1

i:           0
j:           0
n:           0

x:           5
```



### Lookup

Since there is only a small range of possible values of <math>x</math>, storing the answers and looking up the one we want is much more efficient than actually calculating them. This lookup version uses 5 bytes of code and 7 bytes of data and finds 5! in 5 instructions, whereas the iterative solution uses 23 bytes of code and 6 bytes of data and takes 122 instructions to find 5!.

```czasm
        LDA  load
        ADD  x
        STA  load

load:   LDA  fact
        STP

fact:        1
             1
             2
             6
             24
             120

x:           5
```



## Crystal


### Iterative


```crystal
def factorial(x : Int)
    ans = 1
    (1..x).each do |i|
        ans *= i
    end
    return ans
end
```



### Recursive


```crystal
def factorial(x : Int)
    if x <= 1
        return 1
    end
    return x * factorial(x - 1)
end
```



## D


### Iterative Version


```d
uint factorial(in uint n) pure nothrow @nogc
in {
    assert(n <= 12);
} body {
    uint result = 1;
    foreach (immutable i; 1 .. n + 1)
        result *= i;
    return result;
}

// Computed and printed at compile-time.
pragma(msg, 12.factorial);

void main() {
    import std.stdio;

    // Computed and printed at run-time.
    12.factorial.writeln;
}
```

```txt
479001600u
479001600
```



### Recursive Version


```d
uint factorial(in uint n) pure nothrow @nogc
in {
    assert(n <= 12);
} body {
    if (n == 0)
        return 1;
    else
        return n * factorial(n - 1);
}

// Computed and printed at compile-time.
pragma(msg, 12.factorial);

void main() {
    import std.stdio;

    // Computed and printed at run-time.
    12.factorial.writeln;
}
```

(Same output.)


### Functional Version


```d
import std.stdio, std.algorithm, std.range;

uint factorial(in uint n) pure nothrow @nogc
in {
    assert(n <= 12);
} body {
    return reduce!q{a * b}(1u, iota(1, n + 1));
}

// Computed and printed at compile-time.
pragma(msg, 12.factorial);

void main() {
    // Computed and printed at run-time.
    12.factorial.writeln;
}
```

(Same output.)

===Tail Recursive (at run-time, with DMD) Version===

```d
uint factorial(in uint n) pure nothrow
in {
    assert(n <= 12);
} body {
    static uint inner(uint n, uint acc) pure nothrow @nogc {
        if (n < 1)
            return acc;
        else
            return inner(n - 1, acc * n);
    }
    return inner(n, 1);
}

// Computed and printed at compile-time.
pragma(msg, 12.factorial);

void main() {
    import std.stdio;

    // Computed and printed at run-time.
    12.factorial.writeln;
}
```

(Same output.)


## Dart


### Recursive


```dart
int fact(int n) {
  if(n<0) {
    throw new IllegalArgumentException('Argument less than 0');
  }
  return n==0 ? 1 : n*fact(n-1);
}

main() {
  print(fact(10));
  print(fact(-1));
}
```


### Iterative


```dart
int fact(int n) {
  if(n<0) {
    throw new IllegalArgumentException('Argument less than 0');
  }
  int res=1;
  for(int i=1;i<=n;i++) {
    res*=i;
  }
  return res;
}

main() {
  print(fact(10));
  print(fact(-1));
}
```



## dc

This factorial uses tail recursion to iterate from ''n'' down to 2. Some implementations, like [[OpenBSD dc]], optimize the tail recursion so the call stack never overflows, though ''n'' might be large.

```dc
[*
 * (n) lfx -- (factorial of n)
 *]sz
[
 1 Sp           [product = 1]sz
 [              [Loop while 1 < n:]sz
  d lp * sp      [product = n * product]sz
  1 -            [n = n - 1]sz
  d 1 <f
 ]Sf d 1 <f
 Lfsz           [Drop loop.]sz
 sz             [Drop n.]sz
 Lp             [Push product.]sz
]sf

[*
 * For example, print the factorial of 50.
 *]sz
50 lfx psz
```


=={{header|Déjà Vu}}==

### Iterative


```dejavu
factorial:
    1
    while over:
        * over
        swap -- swap
    drop swap
```


### Recursive


```dejavu
factorial:
    if dup:
        * factorial -- dup
    else:
        1 drop
```



## Delphi


### Iterative


```Delphi
program Factorial1;

{$APPTYPE CONSOLE}

function FactorialIterative(aNumber: Integer): Int64;
var
  i: Integer;
begin
  Result := 1;
  for i := 1 to aNumber do
    Result := i * Result;
end;

begin
  Writeln('5! = ', FactorialIterative(5));
end.
```



### Recursive


```Delphi
program Factorial2;

{$APPTYPE CONSOLE}

function FactorialRecursive(aNumber: Integer): Int64;
begin
  if aNumber < 1 then
    Result := 1
  else
    Result := aNumber * FactorialRecursive(aNumber - 1);
end;

begin
  Writeln('5! = ', FactorialRecursive(5));
end.
```



### Tail Recursive


```Delphi
program Factorial3;

{$APPTYPE CONSOLE}

function FactorialTailRecursive(aNumber: Integer): Int64;

  function FactorialHelper(aNumber: Integer; aAccumulator: Int64): Int64;
  begin
    if aNumber = 0 then
      Result := aAccumulator
    else
      Result := FactorialHelper(aNumber - 1, aNumber * aAccumulator);
    end;

begin
  if aNumber < 1 then
    Result := 1
  else
    Result := FactorialHelper(aNumber, 1);
end;

begin
  Writeln('5! = ', FactorialTailRecursive(5));
end.
```



## Dragon

<lang>select "std"
factorial = 1
n = readln()
for(i=1,i<=n,++i)
        {
            factorial = factorial * i
        }
           showln "factorial of " + n + " is " + factorial

```



## DWScript

Note that ''Factorial'' is part of the standard DWScript maths functions.

### Iterative


```delphi
function IterativeFactorial(n : Integer) : Integer;
var
   i : Integer;
begin
   Result := 1;
   for i := 2 to n do
      Result *= i;
end;
```


### Recursive


```delphi
function RecursiveFactorial(n : Integer) : Integer;
begin
   if n>1 then
      Result := RecursiveFactorial(n-1)*n
   else Result := 1;
end;
```



## Dyalect



```Dyalect
func factorial(n) {
    if n < 2 {
       1
    } else {
       n * factorial(n - 1)
    }
}
```



## Dylan



###  Functional



```dylan

define method factorial (n)
  if (n < 1)
    error("invalid argument");
  else
    reduce1(\*, range(from: 1, to: n))
  end
end method;

```



###  Iterative



```dylan

define method factorial (n)
  if (n < 1)
    error("invalid argument");
  else
    let total = 1;
    for (i from n to 2 by -1)
      total := total * i;
    end;
    total
  end
end method;

```



###  Recursive



```dylan

define method factorial (n)
  if (n < 1)
    error("invalid argument");
  end;
  local method loop (n)
          if (n <= 2)
            n
          else
            n * loop(n - 1)
          end
        end;
  loop(n)
end method;

```



###  Tail recursive



```dylan

define method factorial (n)
  if (n < 1)
    error("invalid argument");
  end;
  // Dylan implementations are required to perform tail call optimization so
  // this is equivalent to iteration.
  local method loop (n, total)
          if (n <= 2)
            total
          else
            let next = n - 1;
            loop(next, total * next)
          end
        end;
  loop(n, n)
end method;

```



## E


```e
pragma.enable("accumulator")
def factorial(n) {
  return accum 1 for i in 2..n { _ * i }
}
```



## EasyLang


<lang>func factorial n . r .
  r = 1
  i = 2
  while i <= n
    r = r * i
    i += 1
  .
.
call factorial 7 r
print r
```



## EchoLisp


### Iterative


```scheme

(define (fact n)
    (for/product ((f (in-range 2 (1+ n)))) f))
(fact 10)
    → 3628800

```


### Recursive with memoization


```scheme

(define (fact n)
    (if (zero? n) 1
    (* n (fact (1- n)))))
(remember 'fact)
(fact 10)
    → 3628800

```


### Tail recursive


```scheme

(define (fact n (acc 1))
(if (zero? n) acc
    (fact (1- n) (* n acc))))
(fact 10)
    → 3628800

```


### Primitive


```scheme

(factorial 10)
    → 3628800

```


### Numerical approximation


```scheme

(lib 'math)
math.lib v1.13 ® EchoLisp
(gamma 11)
    → 3628800.0000000005

```



## EGL


### Iterative


```EGL

function fact(n int in) returns (bigint)
    if (n < 0)
        writestdout("No negative numbers");
        return (0);
    end
    ans bigint = 1;
    for (i int from 1 to n)
        ans *= i;
    end
    return (ans);
end

```


### Recursive


```EGL

function fact(n int in) returns (bigint)
    if (n < 0)
        SysLib.writeStdout("No negative numbers");
        return (0);
    end
    if (n < 2)
    	return (1);
    else
    	return (n * fact(n - 1));
    end
end

```



## Eiffel



```Eiffel

note
	description: "recursive and iterative factorial example of a positive integer."

class
	FACTORIAL_EXAMPLE

create
	make

feature -- Initialization

	make
		local
			n: NATURAL
		do
			n := 5
			print ("%NFactorial of " + n.out + " = ")
			print (recursive_factorial (n))
		end

feature -- Access

	recursive_factorial (n: NATURAL): NATURAL
			-- factorial of 'n'
		do
			if n = 0 then
				Result := 1
			else
				Result := n * recursive_factorial (n - 1)
			end
		end

	iterative_factorial (n: NATURAL): NATURAL
			-- factorial of 'n'
		local
			v: like n
		do
			from
				Result := 1
				v := n
			until
				v <= 1
			loop
				Result := Result * v
				v := v - 1
			end
		end

end

```



## Ela

Tail recursive version:


```Ela
fact = fact' 1L
       where fact' acc 0 = acc
             fact' acc n = fact' (n * acc) (n - 1)
```



## Elixir


```elixir
defmodule Factorial do
  # Simple recursive function
  def fac(0), do: 1
  def fac(n) when n > 0, do: n * fac(n - 1)

  # Tail recursive function
  def fac_tail(0), do: 1
  def fac_tail(n), do: fac_tail(n, 1)
  def fac_tail(1, acc), do: acc
  def fac_tail(n, acc) when n > 1, do: fac_tail(n - 1, acc * n)

  # Tail recursive function with default parameter
  def fac_default(n, acc \\ 1)
  def fac_default(0, acc), do: acc
  def fac_default(n, acc) when n > 0, do: fac_default(n - 1, acc * n)

  # Using Enumeration features
  def fac_reduce(0), do: 1
  def fac_reduce(n) when n > 0, do: Enum.reduce(1..n, 1, &*/2)

  # Using Enumeration features with pipe operator
  def fac_pipe(0), do: 1
  def fac_pipe(n) when n > 0, do: 1..n |> Enum.reduce(1, &*/2)

end
```



## Elm


=
## Recursive
=


```elm

factorial : Int -> Int
factorial n =
  if n < 1 then 1 else n*factorial(n-1)

```


=
## Tail Recursive
=


```elm

factorialAux : Int -> Int -> Int
factorialAux a acc =
    if a < 2 then acc else factorialAux (a - 1) (a * acc)

factorial : Int -> Int
factorial a =
    factorialAux a 1

```


=
## Functional
=


```elm

import List exposing (product, range)

factorial : Int -> Int
factorial a =
    product (range 1 a)

```



## Emacs Lisp


```lisp
(defun fact (n)
  "n is an integer, this function returns n!, that is n * (n - 1)
* (n - 2)....* 4 * 3 * 2 * 1"
  (cond
   ((= n 1) 1)
   (t (* n (fact (1- n))))))
```



```lisp
(defun fact (n) (apply '* (number-sequence 1 n)))
```


The <code>calc</code> package (which comes with Emacs) has a builtin <code>fact()</code>.  It automatically uses the bignums implemented by <code>calc</code>.


```lisp
(require 'calc)
(calc-eval "fact(30)")
=>
"265252859812191058636308480000000"
```



## Erlang

With a fold:

```erlang
lists:foldl(fun(X,Y) -> X*Y end, 1, lists:seq(1,N)).
```


With a recursive function:

```erlang
fac(1) -> 1;
fac(N) -> N * fac(N-1).
```


With a tail-recursive function:

```erlang
fac(N) -> fac(N-1,N).
fac(1,N) -> N;
fac(I,N) -> fac(I-1,N*I).
```



## ERRE

You must use a procedure to implement factorial because ERRE has one-line FUNCTION only.

'''Iterative procedure:'''

```ERRE

    PROCEDURE FACTORIAL(X%->F)
      F=1
      IF X%<>0 THEN
        FOR I%=X% TO 2 STEP Ä1 DO
          F=F*X%
        END FOR
      END IF
    END PROCEDURE

```


'''Recursive procedure:'''

```ERRE

    PROCEDURE FACTORIAL(FACT,X%->FACT)
       IF X%>1 THEN FACTORIAL(X%*FACT,X%-1->FACT)
       END IF
    END PROCEDURE

```

Procedure call is for example FACTORIAL(1,5->N)


## Euphoria

Straight forward methods

### Iterative


```Euphoria
function factorial(integer n)
  atom f = 1
  while n > 1 do
    f *= n
    n -= 1
  end while

  return f
end function
```



### Recursive


```Euphoria
function factorial(integer n)
  if n > 1 then
    return factorial(n-1) * n
  else
    return 1
  end if
end function
```



### Tail Recursive

```Euphoria
function factorial(integer n, integer acc = 1)
  if n <= 0 then
    return acc
  else
    return factorial(n-1, n*acc)
  end if
end function
```


==='Paper tape' / Virtual Machine version===
Another 'Paper tape' / Virtual Machine version, with as much as possible happening in the tape itself. Some command line handling as well.


```Euphoria
include std/mathcons.e

enum MUL_LLL,
	TESTEQ_LIL,
	TESTLT_LIL,
	TRUEGO_LL,
	MOVE_LL,
	INCR_L,
	TESTGT_LLL,
	GOTO_L,
	OUT_LI,
	OUT_II,
	STOP

global sequence tape = {
	1,
	1,
	0,
	0,
	0,
	{TESTLT_LIL, 5, 0, 4},
	{TRUEGO_LL, 4, 22},
	{TESTEQ_LIL, 5, 0, 4},
	{TRUEGO_LL, 4, 20},
	{MUL_LLL, 1, 2, 3},
	{TESTEQ_LIL, 3, PINF, 4},
	{TRUEGO_LL, 4, 18},
	{MOVE_LL, 3, 1},
	{INCR_L, 2},
	{TESTGT_LLL, 2, 5, 4 },
	{TRUEGO_LL, 4, 18},
	{GOTO_L, 10},
	{OUT_LI, 3, "%.0f\n"},
	{STOP},
	{OUT_II, 1, "%.0f\n"},
	{STOP},
	{OUT_II, "Negative argument", "%s\n"},
	{STOP}
}

global integer ip = 1

procedure eval( sequence cmd )
	atom i = 1
	while i <= length( cmd ) do
		switch cmd[ i ] do
			case MUL_LLL then -- multiply location location giving location
				tape[ cmd[ i + 3 ] ] = tape[ cmd[ i + 1 ] ] * tape[ cmd[ i + 2 ] ]
				i += 3
			case TESTEQ_LIL then -- test if location eq value giving location
				tape[ cmd[ i + 3 ]] = ( tape[ cmd[ i + 1 ] ] = cmd[ i + 2 ] )
				i += 3
			case TESTLT_LIL then -- test if location eq value giving location
				tape[ cmd[ i + 3 ]] = ( tape[ cmd[ i + 1 ] ] < cmd[ i + 2 ] )
				i += 3
			case TRUEGO_LL then -- if true in location, goto location
				if tape[ cmd[ i + 1 ] ] then
					ip = cmd[ i + 2 ] - 1
				end if
				i += 2
			case MOVE_LL then -- move value at location to location
				tape[ cmd[ i + 2 ] ] = tape[ cmd[ i + 1 ] ]
				i += 2
			case INCR_L then -- increment value at location
				tape[ cmd[ i + 1 ] ] += 1
				i += 1
			case TESTGT_LLL then -- test if location gt location giving location
				tape[ cmd[ i + 3 ]] = ( tape[ cmd[ i + 1 ] ] > tape[ cmd[ i + 2 ] ] )
				i += 3
			case GOTO_L then -- goto location
				ip = cmd[ i + 1 ] - 1
				i += 1
			case OUT_LI then -- output location using format
				printf( 1, cmd[ i + 2], tape[ cmd[ i + 1 ] ] )
				i += 2
			case OUT_II then -- output immediate using format
				if sequence( cmd[ i + 1 ] ) then
					printf( 1, cmd[ i + 2], { cmd[ i + 1 ] } )
				else
					printf( 1, cmd[ i + 2], cmd[ i + 1 ] )
				end if
				i += 2
			case STOP then -- stop
				abort(0)
		end switch
		i += 1
	end while
end procedure

include std/convert.e

sequence cmd = command_line()
if length( cmd ) > 2 then
	puts( 1, cmd[ 3 ] & "! = " )
	tape[ 5 ] = to_number(cmd[3])
else
	puts( 1, "eui fact.ex <number>\n" )
	abort(1)
end if

while 1 do
	if sequence( tape[ ip ] ) then
		eval( tape[ ip ] )
	end if
	ip += 1
end while
```



## Excel


Choose a cell and write in the function bar on the top :


```excel

=fact(5)

```


The result is shown as :


```txt

120

```



## Ezhil

Recursive
<lang src="Python">
நிரல்பாகம்  fact ( n )
  @( n == 0 ) ஆனால்
            பின்கொடு  1
     இல்லை
            பின்கொடு    n*fact( n - 1 )
    முடி
முடி

பதிப்பி fact ( 10 )

```


=={{header|F_Sharp|F#}}==

```fsharp
//val inline factorial :
//   ^a ->  ^a
//    when  ^a : (static member get_One : ->  ^a) and
//          ^a : (static member ( + ) :  ^a *  ^a ->  ^a) and
//          ^a : (static member ( * ) :  ^a *  ^a ->  ^a)
let inline factorial n = Seq.reduce (*) [ LanguagePrimitives.GenericOne .. n ]
```

<div style="width:full;overflow:scroll">
 > factorial 8;;
 val it : int = 40320
 > factorial 800I;;
 val it : bigint = 771053011335386004144639397775028360595556401816010239163410994033970851827093069367090769795539033092647861224230677444659785152639745401480184653174909762504470638274259120173309701702610875092918816846985842150593623718603861642063078834117234098513725265045402523056575658860621238870412640219629971024686826624713383660963127048195572279707711688352620259869140994901287895747290410722496106151954257267396322405556727354786893725785838732404646243357335918597747405776328924775897564519583591354080898117023132762250714057271344110948164029940588827847780442314473200479525138318208302427727803133219305210952507605948994314345449325259594876385922128494560437296428386002940601874072732488897504223793518377180605441783116649708269946061380230531018291930510748665577803014523251797790388615033756544830374909440162270182952303329091720438210637097105616258387051884030288933650309756289188364568672104084185529365727646234588306683493594765274559497543759651733699820639731702116912963247441294200297800087061725868223880865243583365623482704395893652711840735418799773763054887588219943984673401051362280384187818611005035187862707840912942753454646054674870155072495767509778534059298038364204076299048072934501046255175378323008217670731649519955699084482330798811049166276249251326544312580289357812924825898217462848297648349400838815410152872456707653654424335818651136964880049831580548028614922852377435001511377656015730959254647171290930517340367287657007606177675483830521499707873449016844402390203746633086969747680671468541687265823637922007413849118593487710272883164905548707198762911703545119701275432473548172544699118836274377270607420652133092686282081777383674487881628800801928103015832821021286322120460874941697199487758769730544922012389694504960000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000I
</div>


## Factor

```factor
USING: math.ranges sequences ;

: factorial ( n -- n ) [1,b] product ;
```


The ''[1,b]'' word takes a number from the stack and pushes a range, which is then passed to ''product''.


## FALSE


```false
[1\[$][$@*\1-]#%]f:
^'0- f;!.
```

Recursive:

```false
[$1=~[$1-f;!*]?]f:
```



## Fancy


```fancy
def class Number {
  def factorial {
    1 upto: self . product
  }
}

# print first ten factorials
1 upto: 10 do_each: |i| {
  i to_s ++ "! = " ++ (i factorial) println
}
```



## Fantom

The following uses 'Ints' to hold the computed factorials, which limits results to a 64-bit signed integer.

```fantom
class Main
{
  static Int factorialRecursive (Int n)
  {
    if (n <= 1)
      return 1
    else
      return n * (factorialRecursive (n - 1))
  }

  static Int factorialIterative (Int n)
  {
    Int product := 1
    for (Int i := 2; i <=n ; ++i)
    {
      product *= i
    }
    return product
  }

  static Int factorialFunctional (Int n)
  {
    (1..n).toList.reduce(1) |a,v|
    {
      v->mult(a) // use a dynamic invoke
      // alternatively, cast a:  v * (Int)a
    }
  }

  public static Void main ()
  {
    echo (factorialRecursive(20))
    echo (factorialIterative(20))
    echo (factorialFunctional(20))
  }
}
```



## Forth


### Single Precision


```forth
: fac ( n -- n! ) 1 swap 1+ 1 ?do i * loop ;
```


### Double Precision

On a 64 bit computer, can compute up to 33!  Also does error checking.  In gforth, error code -24 is "invalid numeric argument."

```forth
: factorial ( n -- d )
    dup 33 u> -24 and throw
    dup 2 < IF
        drop 1.
    ELSE
        1.
        rot 1+ 2 DO
            i 1 m*/
        LOOP
    THEN ;

33 factorial d. 8683317618811886495518194401280000000  ok
-5 factorial d.
:2: Invalid numeric argument

```



## Fortran


### Fortran 90

A simple one-liner is sufficient.

```fortran
nfactorial = PRODUCT((/(i, i=1,n)/))
```


Recursive functions were added in Fortran 90, allowing the following:

```fortran
INTEGER RECURSIVE FUNCTION RECURSIVE_FACTORIAL(X) RESULT(ANS)
    INTEGER, INTENT(IN) :: X

    IF (X <= 1) THEN
        ANS = 1
    ELSE
        ANS = X * RECURSIVE_FACTORIAL(X-1)
    END IF

END FUNCTION RECURSIVE_FACTORIAL
```



### FORTRAN 77


```fortran
     FUNCTION FACT(N)
     INTEGER N,I,FACT
     FACT=1
     DO 10 I=1,N
  10 FACT=FACT*I
     END
```



## FPr

FP-Way

```fpr
fact==((1&),iota)\(1*2)&
```

Recursive

```fpr
fact==(id<=1&)->(1&);id*fact°id-1&
```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Function Factorial_Iterative(n As Integer) As Integer
  Var result = 1
  For i As Integer = 2 To n
    result *= i
  Next
  Return result
End Function

Function Factorial_Recursive(n As Integer) As Integer
  If n = 0 Then Return 1
  Return n * Factorial_Recursive(n - 1)
End Function

For i As Integer = 1 To 5
  Print i; " =>"; Factorial_Iterative(i)
Next

For i As Integer = 6 To 10
  Print Using "##"; i;
  Print " =>"; Factorial_Recursive(i)
Next

Print
Print "Press any key to quit"
Sleep
```


```txt

 1 => 1
 2 => 2
 3 => 6
 4 => 24
 5 => 120
 6 => 720
 7 => 5040
 8 => 40320
 9 => 362880
10 => 3628800

```



## friendly interactive shell

Asterisk is quoted to prevent globbing.


### Iterative


```fishshell

function factorial
	set x $argv[1]
	set result 1
	for i in (seq $x)
		set result (expr $i '*' $result)
	end
	echo $result
end

```



### Recursive


```fishshell

function factorial
	set x $argv[1]
	if [ $x -eq 1 ]
		echo 1
	else
		expr (factorial (expr $x - 1)) '*' $x
	end
end

```



## Frink

Frink has a built-in factorial operator that creates arbitrarily-large numbers and caches results.

```frink

factorial[x] := x!

```

If you want to roll your own, you could do:

```frink

factorial2[x] := product[1 to x]

```



## FunL


###  Procedural


```funl
def factorial( n ) =
  if n < 0
    error( 'factorial: n should be non-negative' )
  else
    res = 1

    for i <- 2..n
      res *= i

    res
```



###  Recursive


```funl
def
  factorial( (0|1) ) = 1
  factorial( n )
    | n > 0 = n*factorial( n - 1 )
    | otherwise = error( 'factorial: n should be non-negative' )
```


=== Tail-recursive ===

```funl
def factorial( n )
  | n >= 0 =
    def
      fact( acc, 0 ) = acc
      fact( acc, n ) = fact( acc*n, n - 1 )

    fact( 1, n )
  | otherwise = error( 'factorial: n should be non-negative' )
```



###  Using a library function


```funl
def factorial( n )
  | n >= 0 = product( 1..n )
  | otherwise = error( 'factorial: n should be non-negative' )
```



## Futhark

###  Recursive



```Futhark

fun fact(n: int): int =
  if n == 0 then 1
            else n * fact(n-1)

```


== Iterative ==


```Futhark

fun fact(n: int): int =
  loop (out = 1) = for i < n do
    out * (i+1)
  in out

```



## FutureBasic


```futurebasic

include "ConsoleWindow"

local fn factorialIterative( n as long ) as double
dim as double f
dim as long i

if ( n > 1 )
  f = 1
  for i = 2 To n
    f = f * i
  next i
else
  f = 1
end if
end fn = f

local fn factorialRecursive( n as long ) as double
dim as double f

if ( n < 2 )
 f = 1
else
 f = n * fn factorialRecursive( n -1 )
end if
end fn = f

dim as long i

for i = 0 to 12
  print "Iterative:"; using "####"; i; " ="; fn factorialIterative( i )
  print "Recursive:"; using "####"; i; " ="; fn factorialRecursive( i )
  print
next i

```

Output:

```txt

Iterative:   0 = 1
Recursive:   0 = 1

Iterative:   1 = 1
Recursive:   1 = 1

Iterative:   2 = 2
Recursive:   2 = 2

Iterative:   3 = 6
Recursive:   3 = 6

Iterative:   4 = 24
Recursive:   4 = 24

Iterative:   5 = 120
Recursive:   5 = 120

Iterative:   6 = 720
Recursive:   6 = 720

Iterative:   7 = 5040
Recursive:   7 = 5040

Iterative:   8 = 40320
Recursive:   8 = 40320

Iterative:   9 = 362880
Recursive:   9 = 362880

Iterative:  10 = 3628800
Recursive:  10 = 3628800

Iterative:  11 = 39916800
Recursive:  11 = 39916800

Iterative:  12 = 479001600
Recursive:  12 = 479001600

```



## Gambas


```Gambas

' Task: Factorial
' Language: Gambas
' Author: Sinuhe Masan (2019)
' Function factorial iterative
Function factorial_iter(num As Integer) As Long
  Dim fact As Long
  Dim i As Integer

  fact = 1
  If num > 1 Then
    For i = 2 To num
      fact = fact * i

    Next

  Endif

  Return fact

End

' Function factorial recursive
Function factorial_rec(num As Integer) As Long

  If num <= 1 Then
    Return 1

  Else
    Return num * factorial_rec(num - 1)

  Endif

End


Public Sub Main()
  Print factorial_iter(6)
  Print factorial_rec(7)

End

```

Output:

```txt

720
5040

```



## GAP


```gap
# Built-in
Factorial(5);

# An implementation
fact := n -> Product([1 .. n]);
```



## Genyris


```genyris
def factorial (n)
    if (< n 2) 1
      * n
        factorial (- n 1)
```



## GML


```GML
n = argument0
j = 1
for(i = 1; i <= n; i += 1)
    j *= i
return j
```



## gnuplot

Gnuplot has a builtin <code>!</code> factorial operator for use on integers.

```gnuplot
set xrange [0:4.95]
set key left
plot int(x)!
```


If you wanted to write your own it can be done recursively.


```gnuplot
# Using int(n) allows non-integer "n" inputs with the factorial
# calculated on int(n) in that case.
# Arranging the condition as "n>=2" avoids infinite recursion if
# n==NaN, since any comparison involving NaN is false.  Could change
# "1" to an expression like "n*0+1" to propagate a NaN input to the
# output too, if desired.
#
factorial(n) = (n >= 2 ? int(n)*factorial(n-1) : 1)
set xrange [0:4.95]
set key left
plot factorial(x)
```



## Go


### Iterative

Sequential, but at least handling big numbers:

```go
package main

import (
    "fmt"
    "math/big"
)

func main() {
    fmt.Println(factorial(800))
}

func factorial(n int64) *big.Int {
    if n < 0 {
        return nil
    }
    r := big.NewInt(1)
    var f big.Int
    for i := int64(2); i <= n; i++ {
        r.Mul(r, f.SetInt64(i))
    }
    return r
}
```

===Built in, exact===
Built in function currently uses a simple divide and conquer technique.  It's a step up from sequential multiplication.

```go
package main

import (
    "math/big"
    "fmt"
)

func factorial(n int64) *big.Int {
    var z big.Int
    return z.MulRange(1, n)
}

func main() {
    fmt.Println(factorial(800))
}
```


### Efficient exact

For a bigger step up, an algorithm fast enough to compute factorials of numbers up to a million or so, see [[Factorial/Go]].
===Built in, Gamma===

```go
package main

import (
    "fmt"
    "math"
)

func factorial(n float64) float64 {
    return math.Gamma(n + 1)
}

func main() {
    for i := 0.; i <= 10; i++ {
        fmt.Println(i, factorial(i))
    }
    fmt.Println(100, factorial(100))
}
```

```txt

0 1
1 1
2 2
3 6
4 24
5 120
6 720
7 5040
8 40320
9 362880
10 3.6288e+06
100 9.332621544394405e+157

```

===Built in, Lgamma===

```go
package main

import (
    "fmt"
    "math"
    "math/big"
)

func lfactorial(n float64) float64 {
    l, _ := math.Lgamma(n + 1)
    return l
}

func factorial(n float64) *big.Float {
    i, frac := math.Modf(lfactorial(n) * math.Log2E)
    z := big.NewFloat(math.Exp2(frac))
    return z.SetMantExp(z, int(i))
}

func main() {
    for i := 0.; i <= 10; i++ {
        fmt.Println(i, factorial(i))
    }
    fmt.Println(100, factorial(100))
    fmt.Println(800, factorial(800))
}
```

```txt

0 1
1 1
2 2
3 6
4 24
5 119.99999999999994
6 720.0000000000005
7 5039.99999999999
8 40320.000000000015
9 362880.0000000001
10 3.6288000000000084e+06
100 9.332621544394454e+157
800 7.710530113351238e+1976

```



## Golfscript

'''Iterative''' (uses folding)

```golfscript
{.!{1}{,{)}%{*}*}if}:fact;
5fact puts # test
```

or

```golfscript
{),(;{*}*}:fact;
```

'''Recursive'''

```golfscript
{.1<{;1}{.(fact*}if}:fact;
```



## GridScript


```gridscript

#FACTORIAL.

@width 14
@height 8

(1,3):START
(7,1):CHECKPOINT 0
(3,3):INPUT INT TO n
(5,3):STORE n
(7,3):GO EAST
(9,3):DECREMENT n
(11,3):SWITCH n
(11,5):MULTIPLY BY n
(11,7):GOTO 0
(13,3):PRINT

```


## Groovy


###  Recursive

A recursive closure must be ''pre-declared''.

```groovy
def rFact
rFact = { (it > 1) ? it * rFact(it - 1) : 1 as BigInteger }
```



###  Iterative


```groovy
def iFact = { (it > 1) ? (2..it).inject(1 as BigInteger) { i, j -> i*j } : 1 }
```


Test Program:

```groovy
def time = { Closure c ->
    def start = System.currentTimeMillis()
    def result = c()
    def elapsedMS = (System.currentTimeMillis() - start)/1000
    printf '(%6.4fs elapsed)', elapsedMS
    result
}

def dashes = '---------------------'
print "   n!       elapsed time   "; (0..15).each { def length = Math.max(it - 3, 3); printf " %${length}d", it }; println()
print "--------- -----------------"; (0..15).each { def length = Math.max(it - 3, 3); print " ${dashes[0..<length]}" }; println()
[recursive:rFact, iterative:iFact].each { name, fact ->
    printf "%9s ", name
    def factList = time { (0..15).collect {fact(it)} }
    factList.each { printf ' %3d', it }
    println()
}
```


```txt
   n!       elapsed time      0   1   2   3   4   5   6    7     8      9      10       11        12         13          14           15
--------- ----------------- --- --- --- --- --- --- --- ---- ----- ------ ------- -------- --------- ---------- ----------- ------------
recursive (0.0040s elapsed)   1   1   2   6  24 120 720 5040 40320 362880 3628800 39916800 479001600 6227020800 87178291200 1307674368000
iterative (0.0060s elapsed)   1   1   2   6  24 120 720 5040 40320 362880 3628800 39916800 479001600 6227020800 87178291200 1307674368000
```



## Haskell

The simplest description: factorial is the product of the numbers from 1 to n:

```haskell
factorial n = product [1..n]
```

Or, using composition and omitting the argument ([https://www.haskell.org/haskellwiki/Partial_application partial application]):

```haskell>factorial = product . enumFromTo 1</lang

Or, written explicitly as a fold:

```haskell
factorial n = foldl (*) 1 [1..n]
```

''See also: [http://www.willamette.edu/~fruehr/haskell/evolution.html The Evolution of a Haskell Programmer]''

Or, if you wanted to generate a list of all the factorials:

```haskell
factorials = scanl (*) 1 [1..]
```


Or, written without library functions:

```haskell
factorial :: Integral -> Integral
factorial 0 = 1
factorial n = n * factorial (n-1)
```


Tail-recursive, checking the negative case:

```haskell
fac n
    | n >= 0    = go 1 n
    | otherwise = error "Negative factorial!"
        where go acc 0 = acc
              go acc n = go (acc * n) (n - 1)
```


Using postfix notation:

```haskell
{-# LANGUAGE PostfixOperators #-}

(!) 0 = 1
(!) n = n * ((n-1)!)

main = do
  print (5!)
  print ((4!)!)
```



## hexiscript


### Iterative


```hexiscript
fun fac n
  let acc 1
  while n > 0
    let acc (acc * n--)
  endwhile
  return acc
endfun
```


### Recursive


```hexiscript
fun fac n
  if n <= 0
    return 1
  else
    return n * fac (n - 1)
  endif
endfun
```



## HicEst


```hicest
WRITE(Clipboard) factorial(6)  ! pasted: 720

FUNCTION factorial(n)
   factorial = 1
   DO i = 2, n
      factorial = factorial * i
   ENDDO
END
```



## HolyC


###  Iterative


```holyc
U64 Factorial(U64 n) {
  U64 i, result = 1;
  for (i = 1; i <= n; ++i)
    result *= i;
  return result;
}

Print("1:  %d\n", Factorial(1));
Print("10: %d\n", Factorial(10));
```


Note: Does not support negative numbers.


###  Recursive


```holyc
I64 Factorial(I64 n) {
  if (n == 0)
    return 1;
  if (n < 0)
    return -1 * ((-1 * n) * Factorial((-1 * n) - 1));
  return n * Factorial(n - 1));
}

Print("+1:  %d\n", Factorial(1));
Print("+10: %d\n", Factorial(10));
Print("-10: %d\n", Factorial(-10));
```



## Hy


```clojure
(defn ! [n]
  (reduce *
    (range 1 (inc n))
    1))

(print (! 6))  ; 720
(print (! 0))  ; 1
```



## i


```i
concept factorial(n) {
	return n!
}

software {
	print(factorial(-23))
	print(factorial(0))
	print(factorial(1))
	print(factorial(2))
	print(factorial(3))
	print(factorial(22))
}

```


=={{header|Icon}} and {{header|Unicon}}==

### Recursive


```Icon
procedure factorial(n)
   n := integer(n) | runerr(101, n)
   if n < 0 then fail
   return if n = 0 then 1 else n*factorial(n-1)
end
```


### Iterative

The {{libheader|Icon Programming Library}} [http://www.cs.arizona.edu/icon/library/src/procs/factors.icn factors] provides the following iterative procedure which can be included with 'link factors':

```Icon
procedure factorial(n)			#: return n! (n factorial)
   local i
   n := integer(n) | runerr(101, n)
   if n < 0 then fail
   i := 1
   every i *:= 1 to n
   return i
end
```



## IDL


```idl
function fact,n
   return, product(lindgen(n)+1)
end
```



## Inform 6


```inform6
[ factorial n;
  if(n == 0)
    return 1;
  else
    return n * factorial(n - 1);
];
```



## Io

Factorials are built-in to Io:

```io>3 factorial</lang



## J


###  Operator


```j
  ! 8             NB.  Built in factorial operator
40320
```


###  Iterative / Functional


```j
   */1+i.8
40320
```


###  Recursive


```j
  (*$:@:<:)^:(1&<) 8
40320
```



###  Generalization

Factorial, like most of J's primitives, is generalized (mathematical generalization is often something to avoid in application code while being something of a curated virtue in utility code):
<div style="width:full;overflow:scroll">

```j
  ! 8 0.8 _0.8    NB.  Generalizes as 1 + the gamma function
40320 0.931384 4.59084
  ! 800x          NB.  Also arbitrarily large
7710530113353860041446393977750283605955564018160102391634109940339708518270930693670907697955390330926478612242306774446597851526397454014801846531749097625044706382742591201733097017026108750929188168469858421505936237186038616420630788341172340985137252...
```

</div>


## Java


### Iterative


```java5

package programas;

import java.math.BigInteger;
import java.util.InputMismatchException;
import java.util.Scanner;

public class IterativeFactorial {

  public BigInteger factorial(BigInteger n) {
    if ( n == null ) {
      throw new IllegalArgumentException();
    }
    else if ( n.signum() == - 1 ) {
      // negative
      throw new IllegalArgumentException("Argument must be a non-negative integer");
    }
    else {
      BigInteger factorial = BigInteger.ONE;
      for ( BigInteger i = BigInteger.ONE; i.compareTo(n) < 1; i = i.add(BigInteger.ONE) ) {
        factorial = factorial.multiply(i);
      }
      return factorial;
    }
  }

  public static void main(String[] args) {
    Scanner scanner = new Scanner(System.in);
    BigInteger number, result;
    boolean error = false;
    System.out.println("FACTORIAL OF A NUMBER");
    do {
      System.out.println("Enter a number:");
      try {
        number = scanner.nextBigInteger();
        result = new IterativeFactorial().factorial(number);
        error = false;
        System.out.println("Factorial of " + number + ": " + result);
      }
      catch ( InputMismatchException e ) {
        error = true;
        scanner.nextLine();
      }

      catch ( IllegalArgumentException e ) {
        error = true;
        scanner.nextLine();
      }
    }
    while ( error );
    scanner.close();
  }

}


```



### Recursive


```java5

package programas;

import java.math.BigInteger;
import java.util.InputMismatchException;
import java.util.Scanner;

public class RecursiveFactorial {

  public BigInteger factorial(BigInteger n) {
    if ( n == null ) {
      throw new IllegalArgumentException();
    }

    else if ( n.equals(BigInteger.ZERO) ) {
      return BigInteger.ONE;
    }
    else if ( n.signum() == - 1 ) {
      // negative
      throw new IllegalArgumentException("Argument must be a non-negative integer");
    }
    else {
      return n.equals(BigInteger.ONE)
          ? BigInteger.ONE
          : factorial(n.subtract(BigInteger.ONE)).multiply(n);
    }
  }

  public static void main(String[] args) {
    Scanner scanner = new Scanner(System.in);
    BigInteger number, result;
    boolean error = false;
    System.out.println("FACTORIAL OF A NUMBER");
    do {
      System.out.println("Enter a number:");
      try {
        number = scanner.nextBigInteger();
        result = new RecursiveFactorial().factorial(number);
        error = false;
        System.out.println("Factorial of " + number + ": " + result);
      }
      catch ( InputMismatchException e ) {
        error = true;
        scanner.nextLine();
      }

      catch ( IllegalArgumentException e ) {
        error = true;
        scanner.nextLine();
      }
    }
    while ( error );
    scanner.close();

  }

}


```



## JavaScript



### Iterative



```javascript
function factorial(n) {
  //check our edge case
  if (n < 0) { throw "Number must be non-negative"; }

  var sum = 1;
  //we skip zero and one since both are 1 and are identity
  while (n > 1) {
    sum *= n;
    n--;
  }
  return sum;
}
```



### Recursive


====ES5 (memoized )====


```JavaScript
(function(x) {

  var memo = {};

  function factorial(n) {
    return n < 2 ? 1 : memo[n] || (memo[n] = n * factorial(n - 1));
  }

  return factorial(x);

})(18);
```


```JavaScript>6402373705728000</lang


Or, assuming that we have some sort of integer range function, we can memoize using the accumulator of a fold/reduce:


```JavaScript
(function () {
    'use strict';

    // factorial :: Int -> Int
    function factorial(x) {

        return range(1, x)
            .reduce(function (a, b) {
                return a * b;
            }, 1);
    }



    // range :: Int -> Int -> [Int]
    function range(m, n) {
        var a = Array(n - m + 1),
            i = n + 1;

        while (i-- > m) a[i - m] = i;
        return a;
    }


    return factorial(18);

})();
```


```JavaScript>6402373705728000</lang




### =ES6=


```javascript>var factorial = n =
 (n < 2) ? 1 : n * factorial(n - 1);
```



Or, as an alternative to recursion, we can fold/reduce a product function over the range of integers 1..n


```JavaScript
(() => {
    'use strict';

    // factorial :: Int -> Int
    const factorial = n =>
        enumFromTo(1, n)
        .reduce(product, 1);


    const test = () =>
        factorial(18);
    // --> 6402373705728000


    // GENERIC FUNCTIONS ----------------------------------

    // product :: Num -> Num -> Num
    const product = (a, b) => a * b;

    // range :: Int -> Int -> [Int]
    const enumFromTo = (m, n) =>
        Array.from({
            length: (n - m) + 1
        }, (_, i) => m + i);

    // MAIN ------
    return test();
})();
```

```txt
6402373705728000
```



## JOVIAL


```JOVIAL
PROC FACTORIAL(ARG) U;
    BEGIN
    ITEM ARG U;
    ITEM TEMP U;
    TEMP = 1;
    FOR I:2 BY 1 WHILE I<=ARG;
        TEMP = TEMP*I;
    FACTORIAL = TEMP;
    END
```



## Joy


```Joy
DEFINE factorial == [0 =] [pop 1] [dup 1 - factorial *] ifte.
```



## jq

An efficient and idiomatic definition in jq is simply to multiply the first n integers:
```jq
def fact:
  reduce range(1; .+1) as $i (1; . * $i);
```

Here is a rendition in jq of the standard recursive definition of the factorial function, assuming n is non-negative:
```jq
def fact(n):
  if n <= 1 then n
  else n * fact(n-1)
  end;

```
Recent versions of jq support tail recursion optimization for 0-arity filters, so here is an implementation that would would benefit from this optimization.  The helper function, <tt>_fact</tt>, is defined here as a subfunction of the main function, which is a filter that accepts the value of n from its input.
```jq
def fact:
  def _fact:
    # Input: [accumulator, counter]
    if .[1] <= 1 then .
    else [.[0] * .[1], .[1] - 1]|  _fact
    end;
  # Extract the accumulated value from the output of _fact:
  [1, .] | _fact | .[0] ;
```



## Jsish


```javascript
/* Factorial, in Jsish */

/* recursive */
function fact(n) { return ((n < 2) ? 1 : n * fact(n - 1)); }

/* iterative */
function factorial(n:number) {
    if (n < 0) throw format("factorial undefined for negative values: %d", n);

    var fac = 1;
    while (n > 1) fac *= n--;
    return fac;
}

if (Interp.conf('unitTest') > 0) {
;fact(18);
;fact(1);

;factorial(18);
;factorial(42);
try { factorial(-1); } catch (err) { puts(err); }
}
```


```txt
prompt$ jsish --U factorial.jsi
fact(18) ==> 6402373705728000
fact(1) ==> 1
factorial(18) ==> 6402373705728000
factorial(42) ==> 1.40500611775288e+51
factorial undefined for negative values: -1
```



## Julia

'''Built-in version''':

```txt
help?> factorial
search: factorial Factorization factorize

  factorial(n)

  Factorial of n. If n is an Integer, the factorial is computed as an integer (promoted to at
  least 64 bits). Note that this may overflow if n is not small, but you can use factorial(big(n))
  to compute the result exactly in arbitrary precision. If n is not an Integer, factorial(n) is
  equivalent to gamma(n+1).

  julia> factorial(6)
  720

  julia> factorial(21)
  ERROR: OverflowError()
  [...]

  julia> factorial(21.0)
  5.109094217170944e19

  julia> factorial(big(21))
  51090942171709440000
```


'''Dynamic version''':

```julia
function fact(n::Integer)
    n < 0 && return zero(n)
    f = one(n)
    for i in 2:n
        f *= i
    end
    return f
end

for i in 10:20
	println("$i -> ", fact(i))
end
```


```txt
10 -> 3628800
11 -> 39916800
12 -> 479001600
13 -> 6227020800
14 -> 87178291200
15 -> 1307674368000
16 -> 20922789888000
17 -> 355687428096000
18 -> 6402373705728000
19 -> 121645100408832000
20 -> 2432902008176640000
```


'''Alternative version''':

```julia
fact2(n::Integer) = prod(Base.OneTo(n))
@show fact2(20)
```

```txt
fact2(20) = 2432902008176640000
```



## K


### Iterative


```K
  facti:*/1+!:
  facti 5
120
```


### Recursive


```K
  factr:{:[x>1;x*_f x-1;1]}
  factr 6
720
```



## Klong

Based on the K examples above.

```k

    factRecursive::{:[x>1;x*.f(x-1);1]}
    factIterative::{*/1+!x}

```



## KonsolScript


```KonsolScript
function factorial(Number n):Number {
  Var:Number ret;
  if (n >= 0) {
    ret = 1;
    Var:Number i = 1;
    for (i = 1; i <= n; i++) {
      ret = ret * i;
    }
  } else {
    ret = 0;
  }
  return ret;
}
```



## Kotlin


```scala
fun facti(n: Int) = when {
    n < 0 -> throw IllegalArgumentException("negative numbers not allowed")
    else  -> {
        var ans = 1L
        for (i in 2..n) ans *= i
        ans
    }
}

fun factr(n: Int): Long = when {
    n < 0 -> throw IllegalArgumentException("negative numbers not allowed")
    n < 2 -> 1L
    else  -> n * factr(n - 1)
}

fun main(args: Array<String>) {
    val n = 20
    println("$n! = " + facti(n))
    println("$n! = " + factr(n))
}
```

```txt
20! = 2432902008176640000
20! = 2432902008176640000
```



## Lang5


### Folding


```lang5
  : fact iota 1 + '* reduce ;
  5 fact
120

```



### Recursive


```lang5

  : fact dup 2 < if else dup 1 - fact * then ;
  5 fact
120

```



## Langur


###  Folding


```Langur
val .factorial = f(.n) fold(f .a x .b, series 2 to .n)
writeln .factorial(7)
```



###  Recursive


```Langur
val .factorial = f if(.x < 2: 1; .x x self(.x - 1))
writeln .factorial(7)
```



###  Iterative


```Langur
val .factorial = f(.i) {
    var .answer = 1
    for .x in 2 to .i {
        .answer x= .x
    }
    .answer
}
writeln .factorial(7)
```


```txt
5040
```



## Lasso


### Iterative


```lasso
define factorial(n) => {
  local(x = 1)
  with i in generateSeries(2, #n)
  do {
    #x *= #i
  }
  return #x
}
```



### Recursive


```lasso
define factorial(n) => #n < 2 ? 1 | #n * factorial(#n - 1)
```



## Latitude



### Functional


```latitude
factorial := {
  1 upto ($1 + 1) product.
}.
```



### Recursive


```latitude
factorial := {
  takes '[n].
  if { n == 0. } then {
    1.
  } else {
    n * factorial (n - 1).
  }.
}.
```



### Iterative


```latitude
factorial := {
  local 'acc = 1.
  1 upto ($1 + 1) do {
    acc = acc * $1.
  }.
  acc.
}.
```



## LFE


===Non-Tail-Recursive Versions===

The non-tail-recursive versions of this function are easy to read: they look like the math textbook definitions. However, they will cause the Erlang VM to throw memory errors when passed very large numbers. To avoid such errors, use the tail-recursive version below.

''Using the'' <tt>cond</tt> ''form'':

```lisp

(defun factorial (n)
  (cond
    ((== n 0) 1)
    ((> n 0) (* n (factorial (- n 1))))))

```


''Using guards (with the'' <tt>when</tt> ''form)'':

```lisp

(defun factorial
  ((n) (when (== n 0)) 1)
  ((n) (when (> n 0))
    (* n (factorial (- n 1)))))

```


''Using pattern matching and a guard'':

```lisp

(defun factorial
  ((0) 1)
  ((n) (when (> n 0))
    (* n (factorial (- n 1)))))

```


===Tail-Recursive Version===


```lisp

(defun factorial (n)
  (factorial n 1))

(defun factorial
  ((0 acc) acc)
  ((n acc) (when (> n 0))
    (factorial (- n 1) (* n acc))))

```


Example usage in the REPL:

```lisp

> (lists:map #'factorial/1 (lists:seq 10 20))
(3628800
 39916800
 479001600
 6227020800
 87178291200
 1307674368000
 20922789888000
 355687428096000
 6402373705728000
 121645100408832000
 2432902008176640000)

```


Or, using <tt>io:format</tt> to print results to <tt>stdout</tt>:

```lisp

> (lists:foreach
    (lambda (x)
      (io:format '"~p~n" `(,(factorial x))))
    (lists:seq 10 20))
3628800
39916800
479001600
6227020800
87178291200
1307674368000
20922789888000
355687428096000
6402373705728000
121645100408832000
2432902008176640000
ok

```


Note that the use of <tt>progn</tt> above was simply to avoid the list of <tt>ok</tt>s that are generated as a result of calling <tt>io:format</tt> inside a <tt>lists:map</tt>'s anonymous function.


## Liberty BASIC


```lb
    for i =0 to 40
        print " FactorialI( "; using( "####", i); ") = "; factorialI( i)
        print " FactorialR( "; using( "####", i); ") = "; factorialR( i)
    next i

    wait

    function factorialI( n)
        if n >1 then
            f =1
            For i = 2 To n
                f = f * i
            Next i
        else
            f =1
        end if
    factorialI =f
    end function

    function factorialR( n)
        if n <2 then
            f =1
        else
            f =n *factorialR( n -1)
        end if
    factorialR =f
    end function

    end
```



## Lingo


### Recursive


```lingo
on fact (n)
  if n<=1 then return 1
  return n * fact(n-1)
end
```



### Iterative


```lingo
on fact (n)
  res = 1
  repeat with i = 2 to n
    res = res*i
  end repeat
  return res
end
```



## Lisaac


```Lisaac
- factorial x : INTEGER : INTEGER <- (
  + result : INTEGER;
  (x <= 1).if {
    result := 1;
  } else {
    result := x * factorial(x - 1);
  };
  result
);
```



## LiveCode


```LiveCode
// recursive
function factorialr n
    if n < 2 then
        return 1
    else
        return n * factorialr(n-1)
    end if
end factorialr

// using accumulator
function factorialacc n acc
    if n = 0 then
        return acc
    else
        return factorialacc(n-1, n * acc)
    end if
end factorialacc

function factorial n
    return factorialacc(n,1)
end factorial

// iterative
function factorialit n
    put 1 into f
    if n > 1 then
        repeat with i = 1 to n
            multiply f by i
        end repeat
    end if
    return f
end factorialit
```



## LLVM


```llvm
; ModuleID = 'factorial.c'
; source_filename = "factorial.c"
; target datalayout = "e-m:w-i64:64-f80:128-n8:16:32:64-S128"
; target triple = "x86_64-pc-windows-msvc19.21.27702"

; This is not strictly LLVM, as it uses the C library function "printf".
; LLVM does not provide a way to print values, so the alternative would be
; to just load the string into memory, and that would be boring.

; Additional comments have been inserted, as well as changes made from the output produced by clang such as putting more meaningful labels for the jumps

$"\01??_C@_04PEDNGLFL@?$CFld?6?$AA@" = comdat any

@"\01??_C@_04PEDNGLFL@?$CFld?6?$AA@" = linkonce_odr unnamed_addr constant [5 x i8] c"%ld\0A\00", comdat, align 1

;--- The declaration for the external C printf function.
declare i32 @printf(i8*, ...)

; Function Attrs: noinline nounwind optnone uwtable
define i32 @factorial(i32) #0 {
;-- local copy of n
  %2 = alloca i32, align 4
;-- long result
  %3 = alloca i32, align 4
;-- int i
  %4 = alloca i32, align 4
;-- local n = parameter n
  store i32 %0, i32* %2, align 4
;-- result = 1
  store i32 1, i32* %3, align 4
;-- i = 1
  store i32 1, i32* %4, align 4
  br label %loop

loop:
;-- i <= n
  %5 = load i32, i32* %4, align 4
  %6 = load i32, i32* %2, align 4
  %7 = icmp sle i32 %5, %6
  br i1 %7, label %loop_body, label %exit

loop_body:
;-- result *= i
  %8 = load i32, i32* %4, align 4
  %9 = load i32, i32* %3, align 4
  %10 = mul nsw i32 %9, %8
  store i32 %10, i32* %3, align 4
  br label %loop_increment

loop_increment:
;-- ++i
  %11 = load i32, i32* %4, align 4
  %12 = add nsw i32 %11, 1
  store i32 %12, i32* %4, align 4
  br label %loop

exit:
;-- return result
  %13 = load i32, i32* %3, align 4
  ret i32 %13
}

; Function Attrs: noinline nounwind optnone uwtable
define i32 @main() #0 {
;-- factorial(5)
  %1 = call i32 @factorial(i32 5)
;-- printf("%ld\n", factorial(5))
  %2 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([5 x i8], [5 x i8]* @"\01??_C@_04PEDNGLFL@?$CFld?6?$AA@", i32 0, i32 0), i32 %1)
;-- return 0
  ret i32 0
}

attributes #0 = { noinline nounwind optnone uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.module.flags = !{!0, !1}
!llvm.ident = !{!2}

!0 = !{i32 1, !"wchar_size", i32 2}
!1 = !{i32 7, !"PIC Level", i32 2}
!2 = !{!"clang version 6.0.1 (tags/RELEASE_601/final)"}
```

```txt
120
```



## Logo


### Recursive



```logo
to factorial :n
  if :n < 2 [output 1]
  output :n * factorial :n-1
end
```



### Iterative


NOTE: Slight code modifications may needed in order to run this as each Logo implementation differs in various ways.


```logo
to factorial :n
	make "fact 1
	make "i 1
	repeat :n [make "fact :fact * :i make "i :i + 1]
	print :fact
end
```



## LOLCODE


```lolcode
HAI 1.3

HOW IZ I Faktorial YR Number
  BOTH SAEM 1 AN BIGGR OF Number AN 1
  O RLY?
   YA RLY
    FOUND YR 1
   NO WAI
    FOUND YR PRODUKT OF Number AN I IZ Faktorial YR DIFFRENCE OF Number AN 1 MKAY
  OIC
IF U SAY SO

IM IN YR Loop UPPIN YR Index WILE DIFFRINT Index AN 13
  VISIBLE Index "! = " I IZ Faktorial YR Index MKAY
IM OUTTA YR Loop
KTHXBYE
```

```txt
0! = 1
1! = 1
2! = 2
3! = 6
4! = 24
5! = 120
6! = 720
7! = 5040
8! = 40320
9! = 362880
10! = 3628800
11! = 39916800
12! = 479001600
```



## Lua


### Recursive


```lua
function fact(n)
  return n > 0 and n * fact(n-1) or 1
end
```


### Tail Recursive


```lua
function fact(n, acc)
  acc = acc or 1
  if n == 0 then
    return acc
  end
  return fact(n-1, n*acc)
end
```


### Memoization

The memoization table can be accessed directly (eg. <code>fact[10]</code>) and will return the memoized value,
or <code>nil</code> if the value has not been memoized yet.

If called as a function (eg. <code>fact(10)</code>), the value will be calculated, memoized and returned.

```Lua
fact = setmetatable({[0] = 1}, {
  __call = function(t,n)
    if n < 0 then return 0 end
    if not t[n] then t[n] = n * t(n-1) end
    return t[n]
  end
})
```




## M2000 Interpreter

M2000 Interpreter running in M2000 Environment, a Visual Basic 6.0 application. So we use Decimals, for output.

Normal Print overwrite console screen, and at the last line scroll up on line, feeding a new clear line. Some time needed to print over and we wish to erase the line before doing that. Here we use another aspect of this variant of Print. Any special formatting function $() are kept local, so after the end of statement formatting return to whatever has before.

We want here to change width of column. Normally column width for all columns are the same. For this statement (Print Over) this not hold, we can change column width as print with it. Also we can change justification, and we can choose on column the use of proportional or non proportional text rendering (console use any font as non proportional by default, and if it is proportional font then we can use it as proportional too). Because no new line append to end of this statement, we need to use a normal Print to send new line.

1@ is 1 in Decimal type (27 digits).


```M2000 Interpreter

Module CheckIt {
      Locale 1033 ' ensure #,### print with comma
      Function factorial (n){
            If n<0 then Error "Factorial Error!"
            If n>27 then Error "Overflow"

            m=1@:While n>1 {m*=n:n--}:=m
      }
      Const Proportional=4
      Const ProportionalLeftJustification=5
      Const NonProportional=0
      Const NonProportionalLeftJustification=1
      For i=1 to 27
      \\ we can print over (erasing line first), without new line at the end
      \\ and we can change how numbers apears, and the with of columns
      \\ numbers by default have right justification
      \\ all $() format have temporary use in this kind of print.
      Print Over $(Proportional),$("\f\a\c\t\o\r\i\a\l\(#\)\=",15), i, $(ProportionalLeftJustification), $("#,###",40), factorial(i)
      Print        \\ new line
      Next i
}
Checkit

```

<pre style="height:30ex;overflow:scroll">
                factorial(1)= 1
                factorial(2)= 2
                factorial(3)= 6
                factorial(4)= 24
                factorial(5)= 120
                factorial(6)= 720
                factorial(7)= 5,040
                factorial(8)= 40,320
                factorial(9)= 362,880
               factorial(10)= 3,628,800
               factorial(11)= 39,916,800
               factorial(12)= 479,001,600
               factorial(13)= 6,227,020,800
               factorial(14)= 87,178,291,200
               factorial(15)= 1,307,674,368,000
               factorial(16)= 20,922,789,888,000
               factorial(17)= 355,687,428,096,000
               factorial(18)= 6,402,373,705,728,000
               factorial(19)= 121,645,100,408,832,000
               factorial(20)= 2,432,902,008,176,640,000
               factorial(21)= 51,090,942,171,709,440,000
               factorial(22)= 1,124,000,727,777,607,680,000
               factorial(23)= 25,852,016,738,884,976,640,000
               factorial(24)= 620,448,401,733,239,439,360,000
               factorial(25)= 15,511,210,043,330,985,984,000,000
               factorial(26)= 403,291,461,126,605,635,584,000,000
               factorial(27)= 10,888,869,450,418,352,160,768,000,000
</pre >


## M4


```M4
define(`factorial',`ifelse(`$1',0,1,`eval($1*factorial(decr($1)))')')dnl
dnl
factorial(5)
```


```txt

120

```



## MANOOL


Recursive version, MANOOLish &ldquo;cascading&rdquo; notation:

```MANOOL

{ let rec
  { Fact = -- compile-time constant binding
    { proc { N } as -- precondition: N.IsI48[] & (N >= 0)
    : if N == 0 then 1 else
      N * Fact[N - 1]
    }
  }
  in -- use Fact here or just make the whole expression to evaluate to it:
  Fact
}

```


Conventional notation (equivalent to the above up to AST):

```MANOOL

{ let rec
  { Fact = -- compile-time constant binding
    { proc { N } as -- precondition: N.IsI48[] & (N >= 0)
      { if N == 0 then 1 else
        N * Fact[N - 1]
      }
    }
  }
  in -- use Fact here or just make the whole expression to evaluate to it:
  Fact
}

```


Iterative version (in MANOOL, probably more appropriate in this particular case):

```MANOOL

{ let
  { Fact = -- compile-time constant binding
    { proc { N } as -- precondition: N.IsI48[] & (N >= 0)
    : var { Res = 1 } in -- variable binding
    : do Res after -- return result
    : while N <> 0 do -- loop while N does not equal to zero
      Res = N * Res; N = N - 1
    }
  }
  in -- use Fact here or just make the whole expression to evaluate to it:
  Fact
}

```



## Maple

Builtin

```Maple

> 5!;
                                  120

```

Recursive

```Maple
RecFact := proc( n :: nonnegint )
        if n = 0 or n = 1 then
                1
        else
                n * thisproc( n -  1 )
        end if
end proc:

```


```Maple

> seq( RecFact( i ) = i!, i = 0 .. 10 );
1 = 1, 1 = 1, 2 = 2, 6 = 6, 24 = 24, 120 = 120, 720 = 720, 5040 = 5040,

    40320 = 40320, 362880 = 362880, 3628800 = 3628800

```

Iterative

```Maple

IterFact := proc( n :: nonnegint )
        local   i;
        mul( i, i = 2 .. n )
end proc:

```


```Maple

> seq( IterFact( i ) = i!, i = 0 .. 10 );
1 = 1, 1 = 1, 2 = 2, 6 = 6, 24 = 24, 120 = 120, 720 = 720, 5040 = 5040,

    40320 = 40320, 362880 = 362880, 3628800 = 3628800

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==
Note that Mathematica already comes with a factorial function, which can be used as e.g. <tt>5!</tt> (gives 120). So the following implementations are only of pedagogical value.

###  Recursive


```mathematica
factorial[n_Integer] := n*factorial[n-1]
factorial[0] = 1
```

=== Iterative (direct loop) ===

```mathematica
factorial[n_Integer] :=
  Block[{i, result = 1}, For[i = 1, i <= n, ++i, result *= i]; result]
```

=== Iterative (list) ===

```mathematica
factorial[n_Integer] := Block[{i}, Times @@ Table[i, {i, n}]]
```



## MATLAB

=== Built-in ===
The factorial function is built-in to MATLAB. The built-in function is only accurate for N <= 21 due to the precision limitations of floating point numbers.

```matlab
answer = factorial(N)
```


###  Recursive


```matlab
function f=fac(n)
    if n==0
        f=1;
        return
    else
        f=n*fac(n-1);
    end
```


###  Iterative

A possible iterative solution:

```matlab
  function b=factorial(a)
	b=1;
	for i=1:a
	    b=b*i;
	end
```



## Maude


```Maude

fmod FACTORIAL is

	protecting INT .

	op undefined : -> Int .
	op _! : Int -> Int .

	var n : Int .

	eq 0 ! = 1 .
	eq n ! = if n < 0 then undefined else n * (sd(n, 1) !) fi .

endfm

red 11 ! .

```



## Maxima

=== Built-in ===

```maxima
n!
```


###  Recursive


```maxima
fact(n) := if n < 2 then 1 else n * fact(n - 1)$
```


###  Iterative


```maxima
fact2(n) := block([r: 1], for i thru n do r: r * i, r)$
```



## MAXScript


###  Iterative


```maxscript
fn factorial n =
(
    if n == 0 then return 1
    local fac = 1
    for i in 1 to n do
    (
        fac *= i
    )
    fac
)
```


###  Recursive


```maxscript
fn factorial_rec n =
(
    local fac = 1
    if n > 1 then
    (
        fac = n * factorial_rec (n - 1)
    )
    fac
)
```



## Mercury

=== Recursive (using arbitrary large integers and memoisation) ===

```Mercury
:- module factorial.

:- interface.
:- import_module integer.

:- func factorial(integer) = integer.

:- implementation.

:- pragma memo(factorial/1).

factorial(N) =
    (   N =< integer(0)
    ->  integer(1)
    ;   factorial(N - integer(1)) * N
    ).
```


A small test program:

```Mercury
:- module test_factorial.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module factorial.
:- import_module char, integer, list, string.

main(!IO) :-
    command_line_arguments(Args, !IO),
    filter(is_all_digits, Args, CleanArgs),
    Arg1 = list.det_index0(CleanArgs, 0),
    Number = integer.det_from_string(Arg1),
    Result = factorial(Number),
    Fmt = integer.to_string,
    io.format("factorial(%s) = %s\n", [s(Fmt(Number)), s(Fmt(Result))], !IO).
```


Example output:

```Bash
factorial(100) = 93326215443944152681699238856266700490715968264381621468592963895217599993229915608941463976156518286253697920827223758251185210916864000000000000000000000000
```



## Microsoft Small Basic


```smallbasic
'Factorial - smallbasic - 05/01/2019
For n = 1 To 25
    f = 1
    For i = 1 To n
        f = f * i
    EndFor
    TextWindow.WriteLine("Factorial(" + n + ")=" + f)
EndFor
```

```txt

Factorial(25)=15511210043330985984000000

```



## MiniScript


###  Iterative


```MiniScript
factorial = function(n)
    result = 1
    for i in range(2,n)
        result = result * i
    end for
    return result
end function

print factorial(10)
```



###  Recursive


```MiniScript
factorial = function(n)
    if n <= 0 then return 1 else return n * factorial(n-1)
end function

print factorial(10)
```

```txt
3628800
```



## MIPS Assembly


###  Iterative


```mips

##################################
# Factorial; iterative           #
# By Keith Stellyes :)           #
# Targets Mars implementation    #
# August 24, 2016                #
##################################

# This example reads an integer from user, stores in register a1
# Then, it uses a0 as a multiplier and target, it is set to 1

# Pseudocode:
# a0 = 1
# a1 = read_int_from_user()
# while(a1 > 1)
# {
# a0 = a0*a1
# DECREMENT a1
# }
# print(a0)

.text ### PROGRAM BEGIN ###
	### GET INTEGER FROM USER ###
	li $v0, 5 #set syscall arg to READ_INTEGER
	syscall #make the syscall
	move $a1, $v0 #int from READ_INTEGER is returned in $v0, but we need $v0
	              #this will be used as a counter

	### SET $a1 TO INITAL VALUE OF 1 AS MULTIPLIER ###
	li $a0,1

	### Multiply our multiplier, $a1 by our counter, $a0 then store in $a1 ###
loop:	ble $a1,1,exit # If the counter is greater than 1, go back to start
	mul $a0,$a0,$a1 #a1 = a1*a0

	subi $a1,$a1,1 # Decrement counter

	j loop # Go back to start

exit:
	### PRINT RESULT ###
	li $v0,1 #set syscall arg to PRINT_INTEGER
	#NOTE: syscall 1 (PRINT_INTEGER) takes a0 as its argument. Conveniently, that
	#      is our result.
	syscall  #make the syscall

	#exit
	li $v0, 10 #set syscall arg to EXIT
	syscall #make the syscall

```


###  Recursive


```mips

#reference code
#int factorialRec(int n){
#    if(n<2){return 1;}
#    else{ return n*factorial(n-1);}
#}
.data
	n:	.word 5
	result:	.word
.text
main:
	la	$t0, n
	lw	$a0, 0($t0)
	jal	factorialRec
	la	$t0, result
	sw	$v0, 0($t0)
	addi	$v0, $0, 10
	syscall

factorialRec:
	addi	$sp, $sp, -8	#calling convention
	sw	$a0, 0($sp)
	sw	$ra, 4($sp)

	addi	$t0, $0, 2	#if (n < 2) do return 1
	slt	$t0, $a0, $t0	#else return n*factorialRec(n-1)
	beqz	$t0, anotherCall

	lw	$ra, 4($sp)	#recursive anchor
	lw	$a0, 0($sp)
	addi	$sp, $sp, 8
	addi	$v0, $0, 1
	jr	$ra

anotherCall:
	addi	$a0, $a0, -1
	jal	factorialRec

	lw	$ra, 4($sp)
	lw	$a0, 0($sp)
	addi	$sp, $sp, 8
	mul	$v0, $a0, $v0
	jr	$ra

```



## Mirah


```mirah
def factorial_iterative(n:int)
    2.upto(n-1) do |i|
        n *= i
    end
    n
end

puts factorial_iterative 10
```


=={{header|MK-61/52}}==

```txt

ВП	П0	1	ИП0	*	L0	03	С/П

```



## ML/I


### Iterative


```ML/I
MCSKIP "WITH" NL
"" Factorial - iterative
MCSKIP MT,<>
MCINS %.
MCDEF FACTORIAL WITHS ()
AS <MCSET T1=%A1.
MCSET T2=1
MCSET T3=1
%L1.MCGO L2 IF T3 GR T1
MCSET T2=T2*T3
MCSET T3=T3+1
MCGO L1
%L2.%T2.>
fact(1) is FACTORIAL(1)
fact(2) is FACTORIAL(2)
fact(3) is FACTORIAL(3)
fact(4) is FACTORIAL(4)
```


### Recursive


```ML/I
MCSKIP "WITH" NL
"" Factorial - recursive
MCSKIP MT,<>
MCINS %.
MCDEF FACTORIAL WITHS ()
AS <MCSET T1=%A1.
MCGO L1 UNLESS T1 EN 0
1<>MCGO L0
%L1.%%T1.*FACTORIAL(%T1.-1).>
fact(1) is FACTORIAL(1)
fact(2) is FACTORIAL(2)
fact(3) is FACTORIAL(3)
fact(4) is FACTORIAL(4)
```


=={{header|Modula-2}}==

```modula2
MODULE Factorial;
FROM FormatString IMPORT FormatString;
FROM Terminal IMPORT WriteString,ReadChar;

PROCEDURE Factorial(n : CARDINAL) : CARDINAL;
VAR result : CARDINAL;
BEGIN
    result := 1;
    WHILE n#0 DO
        result := result * n;
        DEC(n)
    END;
    RETURN result
END Factorial;

VAR
    buf : ARRAY[0..63] OF CHAR;
    n : CARDINAL;
BEGIN
    FOR n:=0 TO 10 DO
        FormatString("%2c! = %7c\n", buf, n, Factorial(n));
        WriteString(buf)
    END;

    ReadChar
END Factorial.
```


=={{header|Modula-3}}==

### Iterative


```modula3
PROCEDURE FactIter(n: CARDINAL): CARDINAL =
  VAR
    result := n;
    counter := n - 1;

  BEGIN
    FOR i := counter TO 1 BY -1 DO
      result := result * i;
    END;
    RETURN result;
  END FactIter;
```


### Recursive


```modula3
PROCEDURE FactRec(n: CARDINAL): CARDINAL =
  VAR result := 1;

  BEGIN
    IF n > 1 THEN
      result := n * FactRec(n - 1);
    END;
    RETURN result;
  END FactRec;
```



## MUMPS


### Iterative


```MUMPS
factorial(num)	New ii,result
	If num<0 Quit "Negative number"
	If num["." Quit "Not an integer"
	Set result=1 For ii=1:1:num Set result=result*ii
	Quit result

Write $$factorial(0) ; 1
Write $$factorial(1) ; 1
Write $$factorial(2) ; 2
Write $$factorial(3) ; 6
Write $$factorial(10) ; 3628800
Write $$factorial(-6) ; Negative number
Write $$factorial(3.7) ; Not an integer
```



### Recursive


```MUMPS
factorial(num)	;
	If num<0 Quit "Negative number"
	If num["." Quit "Not an integer"
	If num<2 Quit 1
	Quit num*$$factorial(num-1)

Write $$factorial(0) ; 1
Write $$factorial(1) ; 1
Write $$factorial(2) ; 2
Write $$factorial(3) ; 6
Write $$factorial(10) ; 3628800
Write $$factorial(-6) ; Negative number
Write $$factorial(3.7) ; Not an integer
```



## MyrtleScript


```MyrtleScript
func factorial args: int a : returns: int {
    int factorial = a
    repeat int i = (a - 1) : i == 0 : i-- {
        factorial *= i
    }
    return factorial
}
```



## Neko


```neko
var factorial = function(number) {
	var i = 1;
	var result = 1;

	while(i <= number) {
		result *= i;
		i += 1;
	}

	return result;
};

$print(factorial(10));
```



## Nemerle

Here's two functional programming ways to do this and an iterative example translated from the C# above.
Using '''long''', we can only use '''number <= 20''', I just don't like the scientific notation output from using a '''double'''.
Note that in the iterative example, variables whose values change are explicitly defined as mutable; the default in Nemerle is immutable values, encouraging a more functional approach.

```Nemerle
using System;
using System.Console;

module Program
{
  Main() : void
  {
      WriteLine("Factorial of which number?");
      def number = long.Parse(ReadLine());
      WriteLine("Using Fold : Factorial of {0} is {1}", number, FactorialFold(number));
      WriteLine("Using Match: Factorial of {0} is {1}", number, FactorialMatch(number));
      WriteLine("Iterative  : Factorial of {0} is {1}", number, FactorialIter(number));
  }

  FactorialFold(number : long) : long
  {
      $[1L..number].FoldLeft(1L, _ * _ )
  }

  FactorialMatch(number : long) : long
  {
      |0L => 1L
      |n  => n * FactorialMatch(n - 1L)
  }

  FactorialIter(number : long) : long
  {
      mutable accumulator = 1L;
      for (mutable factor = 1L; factor <= number; factor++)
      {
          accumulator *= factor;
      }
      accumulator  //implicit return
  }
}
```



## NetRexx


```NetRexx
/* NetRexx */

options replace format comments java crossref savelog symbols nobinary

numeric digits 64 -- switch to exponential format when numbers become larger than 64 digits

say 'Input a number: \-'
say
do
  n_ = long ask -- Gets the number, must be an integer

  say n_'! =' factorial(n_) '(using iteration)'
  say n_'! =' factorial(n_, 'r') '(using recursion)'

  catch ex = Exception
    ex.printStackTrace
end

return

method factorial(n_ = long, fmethod = 'I') public static returns Rexx signals IllegalArgumentException

  if n_ < 0 then -
    signal IllegalArgumentException('Sorry, but' n_ 'is not a positive integer')

  select
    when fmethod.upper = 'R' then -
      fact = factorialRecursive(n_)
    otherwise -
      fact = factorialIterative(n_)
    end

  return fact

method factorialIterative(n_ = long) private static returns Rexx

  fact = 1
  loop i_ = 1 to n_
    fact = fact * i_
    end i_

  return fact

method factorialRecursive(n_ = long) private static returns Rexx

  if n_ > 1 then -
    fact = n_ * factorialRecursive(n_ - 1)
  else -
   fact = 1

  return fact
```

```txt

Input a number:
49
49! = 608281864034267560872252163321295376887552831379210240000000000 (using iteration)
49! = 608281864034267560872252163321295376887552831379210240000000000 (using recursion)

```



## newLISP


```newLISP>
 (define (factorial n) (exp (gammaln (+ n 1))))
(lambda (n) (exp (gammaln (+ n 1))))
> (factorial 4)
24
```



## Nial

(from Nial help file)

```nial
fact is recur [ 0 =, 1 first, pass, product, -1 +]
```

Using it

```nial
|fact 4
=24
```



## Nickle

Factorial is a built-in operator in Nickle.  To more correctly satisfy the task, it is wrapped in a function here, but does not need to be.  Inputs of 1 or below, return 1.


```c
int fact(int n) { return n!; }
```

```txt
prompt$ nickle
> load "fact.5c"
> fact(66)
544344939077443064003729240247842752644293064388798874532860126869671081148416000000000000000
> fact(-5)
1
> -5!
-120
> fact(1.1)
Unhandled exception invalid_argument ("Incompatible argument", 0, 1.1)
<stdin>:11:     fact ((11/10));
```


Note the precedence of factorial before negation, (-5)! is 1 in Nickle, -5! is the negation of 5!, -120.

Also note how the input of 1.1 is internally managed as 11/10 in the error message.


## Nim


### Library


```nim

import math
let i:int = fac(x)

```


### Recursive


```nim
proc factorial(x): int =
  if x > 0: x * factorial(x - 1)
  else: 1
```


### Iterative


```nim
proc factorial(x: int): int =
  result = 1
  for i in 2..x:
    result *= i
```



## Niue


### Recursive


```Niue>[ dup 1
 [ dup 1 - factorial * ] when ] 'factorial ;

( test )
4 factorial . ( => 24 )
10 factorial . ( => 3628800 )
```



## Oberon

```oberon2

MODULE Factorial;
IMPORT
  Out;

VAR
  i: INTEGER;

  PROCEDURE Iterative(n: LONGINT): LONGINT;
  VAR
    i, r: LONGINT;
  BEGIN
    ASSERT(n >= 0);
    r := 1;
    FOR i := n TO 2 BY -1 DO
      r := r * i
    END;
    RETURN r
  END Iterative;

  PROCEDURE Recursive(n: LONGINT): LONGINT;
  VAR
    r: LONGINT;
  BEGIN
    ASSERT(n >= 0);
    r := 1;
    IF n > 1 THEN
      r := n * Recursive(n - 1)
    END;
    RETURN r
  END Recursive;

BEGIN
  FOR i := 0 TO 9 DO
    Out.String("Iterative ");Out.Int(i,0);Out.String('! =');Out.Int(Iterative(i),0);Out.Ln;
  END;
  Out.Ln;
  FOR i := 0 TO 9 DO
    Out.String("Recursive ");Out.Int(i,0);Out.String('! =');Out.Int(Recursive(i),0);Out.Ln;
  END
END Factorial.

```

```txt

Iterative 0! =1
Iterative 1! =1
Iterative 2! =2
Iterative 3! =6
Iterative 4! =24
Iterative 5! =120
Iterative 6! =720
Iterative 7! =5040
Iterative 8! =40320
Iterative 9! =362880

Recursive 0! =1
Recursive 1! =1
Recursive 2! =2
Recursive 3! =6
Recursive 4! =24
Recursive 5! =120
Recursive 6! =720
Recursive 7! =5040
Recursive 8! =40320
Recursive 9! =362880

```



## Objeck


### Iterative


```objeck
bundle Default {
  class Fact {
    function : Main(args : String[]) ~ Nil {
      5->Factorial()->PrintLine();
    }
  }
}
```



## OCaml


### Recursive


```ocaml
let rec factorial n =
  if n <= 0 then 1
  else n * factorial (n-1)
```

The following is tail-recursive, so it is effectively iterative:

```ocaml
let factorial n =
  let rec loop i accum =
    if i > n then accum
    else loop (i + 1) (accum * i)
  in loop 1 1
```



### Iterative

It can be done using explicit state, but this is usually discouraged in a functional language:

```ocaml
let factorial n =
  let result = ref 1 in
  for i = 1 to n do
    result := !result * i
  done;
  !result
```



### Bignums

All of the previous examples use normal OCaml ints, so on a 64-bit platform the factorial of 100 will be equal to 0, rather than to a 158-digit number.

The following code uses the Zarith package to calculate the factorials of larger numbers:

```ocaml
let rec factorial n =
  let rec loop acc = function
    | 0 -> acc
    | n -> loop (Z.mul (Z.of_int n) acc) (n - 1)
  in loop Z.one n

let () =
  if not !Sys.interactive then
    begin
      Sys.argv.(1) |> int_of_string |> factorial |> Z.print;
      print_newline ()
    end
```


```txt
$ ocamlfind ocamlopt -package zarith zarith.cmxa fact.ml -o fact
$ ./fact 100
93326215443944152681699238856266700490715968264381621468592963895217599993229915608941463976156518286253697920827223758251185210916864000000000000000000000000
```



## Octave


```octave
% built in factorial
printf("%d\n", factorial(50));

% let's define our recursive...
function fact = my_fact(n)
  if ( n <= 1 )
    fact = 1;
  else
    fact = n * my_fact(n-1);
  endif
endfunction

printf("%d\n", my_fact(50));

% let's define our iterative
function fact = iter_fact(n)
  fact = 1;
  for i = 2:n
    fact = fact * i;
  endfor
endfunction

printf("%d\n", iter_fact(50));
```


```txt
30414093201713018969967457666435945132957882063457991132016803840
30414093201713375576366966406747986832057064836514787179557289984
30414093201713375576366966406747986832057064836514787179557289984
```


(Built-in is fast but use an approximation for big numbers)

''Suggested correction:'' Neither of the three (two) results above is exact.
The exact result (computed with Haskell) should be:

```txt
30414093201713378043612608166064768844377641568960512000000000000
```

In fact, all results given by Octave are precise up to their 16th digit, the rest seems to be "random" in all cases. Apparently, this is a consequence of Octave not being capable of arbitrary precision operation.



## Oforth


Recursive :

```Oforth
: fact(n)  n ifZero: [ 1 ] else: [ n n 1- fact * ] ;
```


Imperative :

```Oforth
: fact | i | 1 swap loop: i [ i * ] ;
```


```txt

>50 fact .s
[1] (Integer) 30414093201713378043612608166064768844377641568960512000000000000
ok

```



## Order

Simple recursion:

```c
#include <order/interpreter.h>

#define ORDER_PP_DEF_8fac                     \
ORDER_PP_FN(8fn(8N,                           \
                8if(8less_eq(8N, 0),          \
                    1,                        \
                    8mul(8N, 8fac(8dec(8N))))))

ORDER_PP(8to_lit(8fac(8)))    // 40320
```

Tail recursion:

```c
#include <order/interpreter.h>
```


#define ORDER_PP_DEF_8fac                                                                         \
ORDER_PP_FN(8fn(8N,                                                                               \
                8let((8F, 8fn(8I, 8A, 8G,                                                         \
                              8if(8greater(8I, 8N),                                               \
                                  8A,                                                             \
                                  8apply(8G, 8seq_to_tuple(8seq(8inc(8I), 8mul(8A, 8I), 8G)))))), \
                      8apply(8F, 8seq_to_tuple(8seq(1, 1, 8F))))))

ORDER_PP(8to_lit(8fac(8)))    // 40320
```



## Oz


###  Folding


```oz
fun {Fac1 N}
   {FoldL {List.number 1 N 1} Number.'*' 1}
end
```


###  Tail recursive


```oz
fun {Fac2 N}
   fun {Loop N Acc}
      if N < 1 then Acc
      else
	 {Loop N-1 N*Acc}
      end
   end
in
   {Loop N 1}
end
```


###  Iterative


```oz
fun {Fac3 N}
   Result = {NewCell 1}
in
   for I in 1..N do
      Result := @Result * I
   end
   @Result
end
```



## PARI/GP

All of these versions include bignum support.  The recursive version is limited by the operating system's stack size; it may not be able to compute factorials larger than twenty thousand digits. The gamma function method is reliant on precision; to use it for large numbers increase <code>default(realprecision)</code> as needed.


### Recursive


```parigp
fact(n)=if(n<2,1,n*fact(n-1))
```



### Iterative

This is an improvement on the naive recursion above, being faster and not limited by stack space.

```parigp
fact(n)=my(p=1);for(k=2,n,p*=k);p
```



### Binary splitting

PARI's <code>factorback</code> automatically uses binary splitting, preventing subproducts from growing overly large. This function is dramatically faster than the above.

```parigp
fact(n)=factorback([2..n])
```



### Recursive 1

Even faster

```parigp
f( a, b )={
	my(c);
	if( b == a, return(a));
	if( b-a > 1,
		c=(b + a) >> 1;
		return(f(a, c) * f(c+1, b))
	);
	return( a * b );
}

fact(n) = f(1, n)
```


===Built-in===
Uses binary splitting.  According to the source, this was found to be faster than prime decomposition methods. This is, of course, faster than the above.

```parigp
fact(n)=n!
```



### Gamma

Note also the presence of <code>factorial</code> and <code>lngamma</code>.

```parigp
fact(n)=round(gamma(n+1))
```


===Moessner's algorithm===
Not practical, just amusing. Note the lack of <code>*</code> or <code>^</code>. A variant of an algorithm presented in
:Alfred Moessner, "Eine Bemerkung über die Potenzen der natürlichen Zahlen." ''S.-B. Math.-Nat. Kl. Bayer. Akad. Wiss.'' '''29''':3 (1952).
This is very slow but should be able to compute factorials until it runs out of memory (usage is about <math>n^2\log n</math> bits to compute n!); a machine with 1 GB of RAM and unlimited time could, in theory, find 100,000-digit factorials.

```parigp
fact(n)={
  my(v=vector(n+1,i,i==1));
  for(i=2,n+1,
    forstep(j=i,2,-1,
      for(k=2,j,v[k]+=v[k-1])
    )
  );
  v[n+1]
};
```



## Panda


```panda
fun fac(n) type integer->integer
  product{{1..n}}

1..10.fac

```



## Pascal


###  Iterative


```pascal
function factorial(n: integer): integer;
 var
  i, result: integer;
 begin
  result := 1;
  for i := 2 to n do
   result := result * i;
  factorial := result
 end;
```


###  Recursive


```pascal
function factorial(n: integer): integer;
 begin
  if n = 0
   then
    factorial := 1
   else
    factorial := n*factorial(n-1)
 end;
```



## Peloton

Peloton has an opcode for factorial so there's not much point coding one.

```sgml><@ SAYFCTLIT>5</@></lang

However, just to prove that it can be done, here's one possible implementation:

```sgml><@ DEFUDOLITLIT
FAT|__Transformer|<@ LETSCPLIT>result|1</@><@ ITEFORPARLIT>1|<@ ACTMULSCPPOSFOR>result|...</@></@><@ LETRESSCP>...|result</@></@>
<@ SAYFATLIT>123</@>
```



## Perl


###  Iterative


```perl
sub factorial
{
  my $n = shift;
  my $result = 1;
  for (my $i = 1; $i <= $n; ++$i)
  {
    $result *= $i;
  };
  $result;
}

# using a .. range
sub factorial {
    my $r = 1;
    $r *= $_ for 1..shift;
    $r;
}
```


###  Recursive


```perl
sub factorial
{
  my $n = shift;
  ($n == 0)? 1 : $n*factorial($n-1);
}
```


###  Functional


```perl
use List::Util qw(reduce);
sub factorial
{
  my $n = shift;
  reduce { $a * $b } 1, 1 .. $n
}
```


###  Modules

Each of these will print 35660, the number of digits in 10,000!.
```perl
use ntheory qw/factorial/;
# factorial returns a UV (native unsigned int) or Math::BigInt depending on size
say length(  factorial(10000)  );
```


```perl
use bigint;
say length(  10000->bfac  );
```


```perl
use Math::GMP;
say length(  Math::GMP->new(10000)->bfac  );
```


```perl
use Math::Pari qw/ifact/;
say length(  ifact(10000)  );
```



## Perl 6

=== via User-defined Postfix Operator ===
<tt>[*]</tt> is a reduction operator that multiplies all the following values together. Note that we don't need to start at 1, since the degenerate case of <tt>[*]()</tt> correctly returns 1, and multiplying by 1 to start off with is silly in any case.
```perl6
sub postfix:<!> (Int $n) { [*] 2..$n }
say 5!;
```

```txt
120
```



###  via Memoized Constant Sequence

This approach is much more efficient for repeated use, since it automatically caches.  <tt>[\*]</tt> is the so-called triangular version of [*].  It returns the intermediate results as a list.  Note that Perl 6 allows you to define constants lazily, which is rather helpful when your constant is of infinite size...
```perl6
constant fact = 1, |[\*] 1..*;
say fact[5]
```

```txt
120
```



## Phix

standard iterative factorial builtin, reproduced below. returns inf for 171 and above, and is not accurate above 22 on 32-bit, or 25 on 64-bit.

```Phix
global function factorial(integer n)
atom res = 1
    while n>1 do
        res *= n
        n -= 1
    end while
    return res
end function
```


###  gmp

For seriously big numbers, with perfect accuracy, use the mpz_fac_ui() routine. For a bit of fun, we'll see just how far we can push it.

```Phix
include mpfr.e
mpz f = mpz_init()
integer n = 2
bool still_running = true,
     still_printing = true
while still_running do
    atom t0 = time()
    mpz_fac_ui(f, n)
    still_running = (time()-t0)<10 -- (stop once over 10s)
    string ct = elapsed(time()-t0), res, what, pt
    t0 = time()
    if still_printing then
        res = shorten(mpz_get_str(f))
        what = "printed"
        still_printing = (time()-t0)<10 -- (stop once over 10s)
    else
        res = sprintf("%,d digits",mpz_sizeinbase(f,10))
        what = "size in base"
    end if
    pt = elapsed(time()-t0)
    printf(1,"factorial(%d):%s, calculated in %s, %s in %s\n",
             {n,res,ct,what,pt})
    n *= 2
end while
```

```txt

factorial(2):2, calculated in 0.0s, printed in 0.0s
factorial(4):24, calculated in 0s, printed in 0s
factorial(8):40320, calculated in 0s, printed in 0s
factorial(16):20922789888000, calculated in 0s, printed in 0s
factorial(32):263130836933693530167218012160000000, calculated in 0s, printed in 0s
factorial(64):1268869321858841641...4230400000000000000 (90 digits), calculated in 0s, printed in 0s
factorial(128):3856204823625804217...0000000000000000000 (216 digits), calculated in 0s, printed in 0s
factorial(256):8578177753428426541...0000000000000000000 (507 digits), calculated in 0s, printed in 0s
factorial(512):3477289793132605363...0000000000000000000 (1,167 digits), calculated in 0s, printed in 0s
factorial(1024):5418528796058857283...0000000000000000000 (2,640 digits), calculated in 0s, printed in 0s
factorial(2048):1672691931910011705...0000000000000000000 (5,895 digits), calculated in 0s, printed in 0s
factorial(4096):3642736389457041931...0000000000000000000 (13,020 digits), calculated in 0s, printed in 0s
factorial(8192):1275885799409419815...0000000000000000000 (28,504 digits), calculated in 0s, printed in 0s
factorial(16384):1207246711959629373...0000000000000000000 (61,937 digits), calculated in 0s, printed in 0.0s
factorial(32768):9092886296374209477...0000000000000000000 (133,734 digits), calculated in 0s, printed in 0.1s
factorial(65536):5162948523097509165...0000000000000000000 (287,194 digits), calculated in 0.0s, printed in 0.2s
factorial(131072):2358150556532892503...0000000000000000000 (613,842 digits), calculated in 0.0s, printed in 0.8s
factorial(262144):1396355768630047926...0000000000000000000 (1,306,594 digits), calculated in 0.1s, printed in 3.1s
factorial(524288):5578452507102649524...0000000000000000000 (2,771,010 digits), calculated in 0.3s, printed in 13.4s
factorial(1048576):5,857,670 digits, calculated in 0.7s, size in base in 0.2s
factorial(2097152):12,346,641 digits, calculated in 1.7s, size in base in 0.5s
factorial(4194304):25,955,890 digits, calculated in 3.6s, size in base in 1.0s
factorial(8388608):54,436,999 digits, calculated in 8.1s, size in base in 2.2s
factorial(16777216):113,924,438 digits, calculated in 17.7s, size in base in 4.9s

```



## PHP


###  Iterative


```php
<?php
function factorial($n) {
  if ($n < 0) {
    return 0;
  }

  $factorial = 1;
  for ($i = $n; $i >= 1; $i--) {
    $factorial = $factorial * $i;
  }

  return $factorial;
}
?>
```


###  Recursive


```php
<?php
function factorial($n) {
  if ($n < 0) {
    return 0;
  }

  if ($n == 0) {
    return 1;
  }

  else {
    return $n * factorial($n-1);
  }
}
?>
```

=== One-Liner ===

```php
<?php
function factorial($n) { return $n == 0 ? 1 : array_product(range(1, $n)); }
?>
```



###  Library

Requires the GMP library to be compiled in:

```php
gmp_fact($n)
```



## PicoLisp


```PicoLisp
(de fact (N)
   (if (=0 N)
      1
      (* N (fact (dec N))) ) )
```


or:

```PicoLisp
(de fact (N)
   (apply * (range 1 N) ) )
```

which only works for 1 and bigger.


## Piet

[[File:Pietfactorialv2.gif]]

Codel width: 25

This is the text code. It is a  bit difficult to write as there are some loops and loops doesn't really show well when I write it down as there is no way to explicitly write a loop in the language. I have tried to comment as best to show how it works

```pseudocode
push 1
not
in(number)
duplicate
not        // label a
pointer    // pointer 1
duplicate
push 1
subtract
push 1
pointer
push 1
noop
pointer
duplicate  // the next op is back at label a

push 1     // this part continues from pointer 1
noop
push 2     // label b
push 1
rot 1 2
duplicate
not
pointer    // pointer 2
multiply
push 3
pointer
push 3
pointer
push 3
push 3
pointer
pointer    // back at label b

pop        // continues from pointer 2
out(number)
exit
```



## PL/I


```pli
factorial: procedure (N) returns (fixed decimal (30));
   declare N fixed binary nonassignable;
   declare i fixed decimal (10);
   declare F fixed decimal (30);

   if N < 0 then signal error;
   F = 1;
   do i = 2 to N;
      F = F * i;
   end;
   return (F);
end factorial;
```



## PostScript


### Recursive


```postscript
/fact {
  dup 0 eq     % check for the argument being 0
  {
    pop 1      % if so, the result is 1
  }
  {
    dup
    1 sub
    fact       % call recursively with n - 1
    mul        % multiply the result with n
  } ifelse
} def
```


### Iterative


```postscript
/fact {
  1            % initial value for the product
  1 1          % for's start value and increment
  4 -1 roll    % bring the argument to the top as for's end value
  { mul } for
} def
```


### Combinator

```postscript
/myfact {{dup 0 eq} {pop 1} {dup pred} {mul} linrec}.
```



## PowerBASIC


```powerbasic
function fact1#(n%)
local i%,r#
r#=1
for i%=1 to n%
r#=r#*i%
next
fact1#=r#
end function

function fact2#(n%)
if n%<=2 then fact2#=n% else fact2#=fact2#(n%-1)*n%
end function

for i%=1 to 20
print i%,fact1#(i%),fact2#(i%)
next
```



## PowerShell


### Recursive


```powershell
function Get-Factorial ($x) {
    if ($x -eq 0) {
        return 1
    }
    return $x * (Get-Factorial ($x - 1))
}
```


### Iterative


```powershell
function Get-Factorial ($x) {
    if ($x -eq 0) {
        return 1
    } else {
        $product = 1
        1..$x | ForEach-Object { $product *= $_ }
        return $product
    }
}
```


### Evaluative

This one first builds a string, containing <code>1*2*3...</code> and then lets PowerShell evaluate it. A bit of mis-use but works.

```powershell
function Get-Factorial ($x) {
    if ($x -eq 0) {
        return 1
    }
    return (Invoke-Expression (1..$x -join '*'))
}
```



## Processing


```processing

int fact(int n){
	if(n <= 1){
		return 1;
	} else{
		return n*fact(n-1);
	}
}

```


```txt

returns the appropriate value as an int

```



## Prolog

### Recursive


```prolog
fact(X, 1) :- X<2.
fact(X, F) :- Y is X-1, fact(Y,Z), F is Z*X.
```


### Tail recursive


```prolog
fact(N, NF) :-
	fact(1, N, 1, NF).

fact(X, X, F, F) :- !.
fact(X, N, FX, F) :-
	X1 is X + 1,
	FX1 is FX * X1,
	fact(X1, N, FX1, F).
```



### Fold

We can simulate foldl.

```prolog
% foldl(Pred, Init, List, R).
%
foldl(_Pred, Val, [], Val).
foldl(Pred, Val, [H | T], Res) :-
	call(Pred, Val, H, Val1),
	foldl(Pred, Val1, T, Res).

% factorial
p(X, Y, Z) :- Z is X * Y).

fact(X, F) :-
	numlist(2, X, L),
	foldl(p, 1, L, F).
```


### Fold with anonymous function

Using the module lambda written by Ulrich Neumerkel found there http://www.complang.tuwien.ac.at/ulrich/Prolog-inedit/lambda.pl, we can use anonymous functions and write :

```prolog
:- use_module(lambda).

% foldl(Pred, Init, List, R).
%
foldl(_Pred, Val, [], Val).
foldl(Pred, Val, [H | T], Res) :-
	call(Pred, Val, H, Val1),
	foldl(Pred, Val1, T, Res).

fact(N, F) :-
	numlist(2, N, L),
	foldl(\X^Y^Z^(Z is X * Y), 1, L, F).
```


### Continuation passing style

Works with SWI-Prolog and module lambda written by <b>Ulrich Neumerkel</b> found there http://www.complang.tuwien.ac.at/ulrich/Prolog-inedit/lambda.pl.

```prolog
:- use_module(lambda).

fact(N, FN) :-
	cont_fact(N, FN, \X^Y^(Y = X)).

cont_fact(N, F, Pred) :-
	(   N = 0 ->
	    call(Pred, 1, F)
	;   N1 is N - 1,

	    P =  \Z^T^(T is Z * N),
	    cont_fact(N1, FT, P),
	    call(Pred, FT, F)
	).
```



## Pure


### Recursive


```pure
fact n = n*fact (n-1) if n>0;
       = 1 otherwise;
let facts = map fact (1..10); facts;
```


### Tail Recursive


```pure
fact n = loop 1 n with
  loop p n = if n>0 then loop (p*n) (n-1) else p;
end;
```



## PureBasic


### Iterative


```PureBasic
Procedure factorial(n)
  Protected i, f = 1
  For i = 2 To n
    f = f * i
  Next
  ProcedureReturn f
EndProcedure
```


### Recursive


```PureBasic
Procedure Factorial(n)
  If n < 2
    ProcedureReturn 1
  Else
    ProcedureReturn n * Factorial(n - 1)
  EndIf
EndProcedure
```



## Python


### Library

```python
import math
math.factorial(n)
```


### Iterative


```python
def factorial(n):
    result = 1
    for i in range(1, n+1):
        result *= i
    return result
```


### Functional


```python
from operator import mul
from functools import reduce

def factorial(n):
    return reduce(mul, range(1,n+1), 1)
```


or

```python
from itertools import (accumulate, chain)
from operator import mul

# factorial :: Integer
def factorial(n):
    return list(
        accumulate(chain([1], range(1, 1 + n)), mul)
    )[-1]
```


or including the sequence that got us there:

```python
from itertools import (accumulate, chain)
from operator import mul


# factorials :: [Integer]
def factorials(n):
    return list(
        accumulate(chain([1], range(1, 1 + n)), mul)
    )

print(factorials(5))

# -> [1, 1, 2, 6, 24, 120]
```


or

```python
from numpy import prod

def factorial(n):
    return prod(range(1, n + 1), dtype=int)
```



### Recursive


```python
def factorial(n):
    z=1
    if n>1:
        z=n*factorial(n-1)
    return z
```


```txt
>>> for i in range(6):
    print(i, factorial(i))

0 1
1 1
2 2
3 6
4 24
5 120
>>>
```



### Numerical Approximation

The following sample uses Lanczos approximation from [[wp:Lanczos_approximation]] to approximate the gamma function.

The gamma function Γ(x) extends the domain of the factorial function, while maintaining the relationship that factorial(x) = Γ(x+1).

```python
from cmath import *

# Coefficients used by the GNU Scientific Library
g = 7
p = [0.99999999999980993, 676.5203681218851, -1259.1392167224028,
     771.32342877765313, -176.61502916214059, 12.507343278686905,
     -0.13857109526572012, 9.9843695780195716e-6, 1.5056327351493116e-7]

def gamma(z):
  z = complex(z)
  # Reflection formula
  if z.real < 0.5:
    return pi / (sin(pi*z)*gamma(1-z))
  else:
    z -= 1
    x = p[0]
    for i in range(1, g+2):
      x += p[i]/(z+i)
    t = z + g + 0.5
    return sqrt(2*pi) * t**(z+0.5) * exp(-t) * x

def factorial(n):
  return gamma(n+1)

print "factorial(-0.5)**2=",factorial(-0.5)**2
for i in range(10):
  print "factorial(%d)=%s"%(i,factorial(i))
```

```txt

factorial(-0.5)**2= (3.14159265359+0j)
factorial(0)=(1+0j)
factorial(1)=(1+0j)
factorial(2)=(2+0j)
factorial(3)=(6+0j)
factorial(4)=(24+0j)
factorial(5)=(120+0j)
factorial(6)=(720+0j)
factorial(7)=(5040+0j)
factorial(8)=(40320+0j)
factorial(9)=(362880+0j)

```



## Q


### Iterative

====Point-free====

```Q
f:(*/)1+til@
```

or

```Q
f:(*)over 1+til@
```

or

```Q>f:prd 1+til@</lang


### =As a function=


```Q
f:{(*/)1+til x}
```


### Recursive


```Q
f:{$[x=1;1;x*.z.s x-1]}
```



## QB64


```QB64

REDIM fac#(0)
Factorial fac#(), 655, 10, power#
PRINT power#
SUB Factorial (fac#(), n&, numdigits%, power#)
power# = 0
fac#(0) = 1
remain# = 0
stx& = 0
slog# = 0
NumDiv# = 10 ^ numdigits%
FOR fac# = 1 TO n&
    slog# = slog# + LOG(fac#) / LOG(10)
    FOR x& = 0 TO stx&
        fac#(x&) = fac#(x&) * fac# + remain#
        tx# = fac#(x&) MOD NumDiv#
        remain# = (fac#(x&) - tx#) / NumDiv#
        fac#(x&) = tx#
    NEXT
    IF remain# > 0 THEN
        stx& = UBOUND(fac#) + 1
        REDIM _PRESERVE fac#(stx&)
        fac#(stx&) = remain#
        remain# = 0
    END IF
NEXT

scanz& = LBOUND(fac#)
DO
    IF scanz& < UBOUND(fac#) THEN
        IF fac#(scanz&) THEN
            EXIT DO
        ELSE
            scanz& = scanz& + 1
        END IF
    ELSE
        EXIT DO
    END IF
LOOP

FOR x& = UBOUND(fac#) TO scanz& STEP -1
    m$ = LTRIM$(RTRIM$(STR$(fac#(x&))))
    IF x& < UBOUND(fac#) THEN
        WHILE LEN(m$) < numdigits%
            m$ = "0" + m$
        WEND
    END IF
    PRINT m$; " ";
    power# = power# + LEN(m$)
NEXT
power# = power# + (scanz& * numdigits%) - 1
PRINT slog#
END SUB

```




## R


###  Recursive


```R
fact <- function(n) {
  if ( n <= 1 ) 1
  else n * fact(n-1)
}
```


### Iterative


```R
factIter <- function(n) {
  f = 1
  for (i in 2:n) f <- f * i
  f
}
```



### Numerical Approximation

R has a native gamma function and a wrapper for that function that can produce factorials.  E.g.

```R
print(factorial(50)) # 3.041409e+64
```



## Racket


###  Recursive

The standard recursive style:

```Racket
(define (factorial n)
  (if (= 0 n)
      1
      (* n (factorial (- n 1)))))
```


However, it is inefficient. It's more efficient to use an accumulator.


```Racket
(define (factorial n)
  (define (fact n acc)
    (if (= 0 n)
        acc
        (fact (- n 1) (* n acc))))
  (fact n 1))
```



###  Fold

We can also define factorial as for/fold (product startvalue) (range) (operation))


```Racket
(define (factorial n)
  (for/fold ([pro 1]) ([i (in-range 1 (+ n 1))]) (* pro i)))
```


Or quite simpler by an for/product


```Racket
(define (factorial n)
  (for/product ([i (in-range 1 (+ n 1))]) i))
```



## Rapira


###  Iterative


```rapira
Фун Факт(n)
  f := 1
  для i от 1 до n
        f := f * i
  кц
  Возврат f
Кон Фун
```



###  Recursive


```rapira
Фун Факт(n)
  Если n = 1
    Возврат 1
  Иначе
    Возврат n * Факт(n - 1)
  Всё
Кон Фун

Проц Старт()
  n := ВводЦел('Введите число (n <= 12) :')
  печать 'n! = '
  печать Факт(n)
Кон проц
```



## Rascal


### Iterative

The standard implementation:

```rascal
public int factorial_iter(int n){
	result = 1;
	for(i <- [1..n])
		result *= i;
	return result;
}
```

However, Rascal supports an even neater solution.
By using a [http://tutor.rascal-mpl.org/Courses/Rascal/Libraries/lang/xml/DOM/xmlPretty/xmlPretty.html#/Courses/Rascal/Expressions/Reducer/Reducer.html reducer] we can write this code on one short line:

```rascal
public int factorial_iter2(int n) = (1 | it*e | int e <- [1..n]);
```

```txt
rascal>factorial_iter(10)
int: 3628800

rascal>factorial_iter2(10)
int: 3628800
```



### Recursive


```rascal
public int factorial_rec(int n){
	if(n>1) return n*factorial_rec(n-1);
		else return 1;
}
```

```txt
rascal>factorial_rec(10)
int: 3628800
```



## REBOL


```REBOL
REBOL [
    Title: "Factorial"
    URL: http://rosettacode.org/wiki/Factorial_function
]

; Standard recursive implementation.

factorial: func [n][
	either n > 1 [n * factorial n - 1] [1]
]

; Iteration.

ifactorial: func [n][
	f: 1
	for i 2 n 1 [f: f * i]
	f
]

; Automatic memoization.
; I'm just going to say up front that this is a stunt. However, you've
; got to admit it's pretty nifty. Note that the 'memo' function
; works with an unlimited number of arguments (although the expected
; gains decrease as the argument count increases).

memo: func [
	"Defines memoizing function -- keeps arguments/results for later use."
	args [block!] "Function arguments. Just specify variable names."
	body [block!] "The body block of the function."
	/local m-args m-r
][
	do compose/deep [
		func [
			(args)
			/dump "Dump memory."
		][
			m-args: []
			if dump [return m-args]

			if m-r: select/only m-args reduce [(args)] [return m-r]

			m-r: do [(body)]
			append m-args reduce [reduce [(args)] m-r]
			m-r
		]
	]
]

mfactorial: memo [n][
	either n > 1 [n * mfactorial n - 1] [1]
]

; Test them on numbers zero to ten.

for i 0 10 1 [print [i ":" factorial i  ifactorial i  mfactorial i]]
```

```txt
0 : 1 1 1
1 : 1 1 1
2 : 2 2 2
3 : 6 6 6
4 : 24 24 24
5 : 120 120 120
6 : 720 720 720
7 : 5040 5040 5040
8 : 40320 40320 40320
9 : 362880 362880 362880
10 : 3628800 3628800 3628800
```

* See also [[wp:Memoization|more on memoization...]]


## Retro

A recursive implementation from the benchmarking code.

```Retro>: <factorial
 dup 1 = if; dup 1- <factorial> * ;
: factorial dup 0 = [ 1+ ] [ <factorial> ] if ;
```



## REXX


### simple version

This version of the REXX program calculates the exact value of factorial of numbers up to   25,000.


 <big> 25,000'''!''' </big>   is exactly   <big> 99,094 </big>   decimal digits.


Most REXX interpreters can handle eight million decimal digits.

```rexx
/*REXX program  computes  the  factorial of a  non-negative integer.                    */
numeric digits 100000                            /*100k digits:  handles  N  up to  25k.*/
parse arg n                                      /*obtain optional argument from the CL.*/
if n=''                   then call er  'no argument specified.'
if arg()>1 | words(n)>1   then call er  'too many arguments specified.'
if \datatype(n,'N')       then call er  "argument isn't numeric: "          n
if \datatype(n,'W')       then call er  "argument isn't a whole number: "   n
if n<0                    then call er  "argument can't be negative: "      n
!=1                                              /*define the factorial product (so far)*/
      do j=2  to n;       !=!*j                  /*compute the factorial the hard way.  */
      end   /*j*/                                /* [↑]  where da rubber meets da road. */

say n'!  is  ['length(!) "digits]:"              /*display number of digits in factorial*/
say                                              /*add some whitespace to the output.   */
say !                                            /*display the factorial product.       */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
er:    say;       say '***error***';      say;      say arg(1);      say;          exit 13
```

'''output'''   when the input is:   <tt> 100 </tt>

```txt

100!  is  [158 digits]:

93326215443944152681699238856266700490715968264381621468592963895217599993229915608941463976156518286253697920827223758251185210916864000000000000000000000000

```


===precision auto-correction===
This version of the REXX program allows the use of (practically) unlimited digits.

```txt

     ╔═══════════════════════════════════════════════════════════════════════════╗
     ║                   ───── Some factorial lengths ─────                      ║
     ║                                                                           ║
     ║                     10 !  =           7  digits                           ║
     ║                     20 !  =          19  digits                           ║
     ║                     52 !  =          68  digits   (a  1  card deck shoe.) ║
     ║                    104 !  =         167  digits    "  2    "    "    "    ║
     ║                    208 !  =         394  digits    "  4    "    "    "    ║
     ║                    416 !  =         911  digits    "  8    "    "    "    ║
     ║                                                                           ║
     ║                     1k !  =       2,568  digits                           ║
     ║                    10k !  =      35,660  digits                           ║
     ║                   100k !  =     456,574  digits                           ║
     ║                                                                           ║
     ║                     1m !  =   5,565,709  digits                           ║
     ║                    10m !  =  65,657,060  digits                           ║
     ║                   100m !  = 756,570,556  digits                           ║
     ║                                                                           ║
     ║  Only one result is shown below for practical reasons.                    ║
     ║                                                                           ║
     ║  This version of the  Regina REXX  interpreter is essentially limited to  ║
     ║  around  8  million digits,  but with some programming tricks,  it could  ║
     ║  yield a result up to  ≈ 16  million decimal digits.                      ║
     ║                                                                           ║
     ║  Also,  the Regina REXX interpreter is limited to an   exponent   of  9   ║
     ║  decimal digits.        I.E.:     9.999...999e+999999999                  ║
     ╚═══════════════════════════════════════════════════════════════════════════╝

```


```rexx
/*REXX program computes the factorial of a  non-negative integer, and it automatically  */
/*────────────────────── adjusts the number of decimal digits to accommodate the answer.*/
numeric digits 99                                /*99 digits initially,  then expanded. */
parse arg n                                      /*obtain optional argument from the CL.*/
if n=''                   then call er  'no argument specified'
if arg()>1 | words(n)>1   then call er  'too many arguments specified.'
if \datatype(n,'N')       then call er  "argument isn't numeric: "          n
if \datatype(n,'W')       then call er  "argument isn't a whole number: "   n
if n<0                    then call er  "argument can't be negative: "      n
!=1                                              /*define the factorial product (so far)*/
     do j=2 to n;    !=!*j                       /*compute  the factorial the hard way. */
     if pos(.,!)==0  then iterate                /*is the  !  in exponential notation?  */
     parse var ! 'E' digs                        /*extract exponent of the factorial,   */
     numeric digits  digs+digs%10                /*  ··· and increase it by ten percent.*/
     end   /*j*/                                 /* [↑]  where da rubber meets da road. */

say n'!  is  ['length(!) "digits]:"              /*display number of digits in factorial*/
say                                              /*add some whitespace to the output.   */
say !/1                                          /*normalize the factorial product.     */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
er:    say;      say '***error!***';      say;       say arg(1);      say;         exit 13
```

'''output'''   when the input is:   <tt> 1000 </tt>

```txt

1000! is  [2568 digits]:

4023872600770937735437024339230039857193748642107146325437999104299385123986290205920442084869694048004799886101971960586316668729948085589013238296699445909974245040870737599188236277271887325197795059509952761208749754624970436014182780946464962910563938874378864873371191810458257836478499770124766328898359557354325131853239584630755574091142624174743493475534286465766116677973966688202912073791438537195882498081268678383745597317461360853795345242215865932019280908782973084313928444032812315586110369768013573042161687476096758713483120254785893207671691324484262361314125087802080002616831510273418279777047846358681701643650241536
9139828126481021309276124489635992870511496497541990934222156683257208082133318611681155361583654698404670897560290095053761647584772842188967964624494516076535340819890138544248798495995331910172335555660213945039973628075013783761530712776192684903435262520001588853514733161170210396817592151090778801939317811419454525722386554146106289218796022383897147608850627686296714667469756291123408243920816015378088989396451826324367161676217916890977991190375403127462228998800519544441428201218736174599264295658174662830295557029902432415318161721046583203678690611726015878352075151628422554026517048330422614397428693306169089796848259012
5458327168226458066526769958652682272807075781391858178889652208164348344825993266043367660176999612831860788386150279465955131156552036093988180612138558600301435694527224206344631797460594682573103790084024432438465657245014402821885252470935190620929023136493273497565513958720559654228749774011413346962715422845862377387538230483865688976461927383814900140767310446640259899490222221765904339901886018566526485061799702356193897017860040811889729918311021171229845901641921068884387121855646124960798722908519296819372388642614839657382291123125024186649353143970137428531926649875337218940694281434118520158014123344828015051399694290
1534830776445690990731524332782882698646027898643211390835062170950025973898635542771967428222487575867657523442202075736305694988250879689281627538488633969099598262809561214509948717012445164612603790293091208890869420285106401821543994571568059418727489980942547421735824010636774045957417851608292301353580818400969963725242305608559037006242712434169090041536901059339838357779394109700277534720000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000

```


===rehydration (trailing zero replacement)===
This version of the REXX program takes advantage of the fact that the decimal version of factorials (≥5) have trailing zeroes,

so it simply strips them   (thereby reducing the magnitude of the factorial).

When the factorial is finished computing, the trailing zeroes are simply concatenated to the (dehydrated) factorial product.

This technique will allow other programs to extend their range, especially those that use decimal or floating point decimal,

but can work with binary numbers as well --- albeit you'd most probably convert the number to decimal when a multiplier

is a multiple of five [or some other method], strip the trailing zeroes, and then convert it back to binary -- although it

wouldn't be necessary to convert to/from base ten for checking for trailing zeros (in decimal).

```rexx
/*REXX program  computes  the  factorial  of an  integer,  striping trailing zeroes.    */
numeric digits 200                               /*start with two hundred digits.       */
parse arg N .;     if N==''  then N=0            /*obtain the optional argument from CL.*/

!=1                                              /*define the factorial product so far. */
    do j=2  to N                                 /*compute factorial the hard way.      */
    old!=!                                       /*save old product in case of overflow.*/
    !=!*j                                        /*multiple the old factorial with   J. */
    if pos(.,!) \==0  then do                    /*is the   !   in exponential notation?*/
                           d=digits()            /*D   temporarily stores number digits.*/
                           numeric digits d+d%10 /*add  10%  to the   decimal digits.   */
                           !=old! * j            /*re─calculate for the  "lost"  digits.*/
                           end                   /*IFF ≡ if and only if.  [↓]           */
    parse var !  '' -1 _                         /*obtain the right-most digit of  !    */
    if _==0  then !=strip(!,,0)                  /*strip trailing zeroes  IFF  the ...  */
    end   /*j*/                                  /* [↑]  ...  right-most digit is zero. */
z=0                                              /*the number of trailing zeroes in  !  */
    do v=5  by 0  while v<=N                     /*calculate number of trailing zeroes. */
    z=z + N%v                                    /*bump   Z   if multiple power of five.*/
    v=v*5                                        /*calculate the next power of five.    */
    end   /*v*/                                  /* [↑]  we only advance  V  by ourself.*/

!=! || copies(0, z)                              /*add water to rehydrate the product.  */
if z==0  then z='no'                             /*use gooder English for the message.  */
say N'!  is      ['length(!)        " digits  with "        z        ' trailing zeroes]:'
say                                              /*display blank line  (for whitespace).*/
say !                                            /*display the factorial product.       */
                                                 /*stick a fork in it,  we're all done. */
```

<!-- The word  ''gooder''  is a humorous use of the non-word. -->
'''output'''   when the input is:   <tt> 100 </tt>

```txt

100!  is      [158  digits  with  24  trailing zeroes]:

93326215443944152681699238856266700490715968264381621468592963895217599993229915608941463976156518286253697920827223758251185210916864000000000000000000000000

```

'''output''' when the input is:    <tt> 10000 </tt>

(Output is shown at   <big> '''<sup>4</sup>/<sub>5</sub>''' </big>   size.)
<pre style="font-size:80%;height:80ex">
10000!  is      [35660  digits  with  2499  trailing zeroes]:

284625968091705451890641321211986889014805140170279923079417999427441134000376444377299078675778477581588406214231752883004233994015351873905242116138271617481982419982759241828925978789812425312059465996259867065601615720360323979263287367170557419759620994797203461536981198970926112775004841988454104755446424421365733030767036288258035489674611170973695786036701910715127305872810411586405612811653853259684258259955846881464304255898366493170592517172042765974074461334000541940524623034368691540594040662278282483715120383221786446271838229238996389928272218797024593876938030946273322925705554596900278752822425443480211275590
191694254290289169072190970836905398737474524833728995218023632827412170402680867692104515558405671725553720158521328290342799898184493136106403814893044996215999993596708929801903369984844046654192362584249471631789611920412331082686510713545168455409360330096072103469443779823494307806260694223026818852275920570292308431261884976065607425862794488271559568315334405344254466484168945804257094616736131876052349822863264529215294234798706033442907371586884991789325806914831688542519560061723726363239744207869246429560123062887201226529529640915083013366309827338063539729015065818225742954758943997651138655412081257886837042392
087644847615690012648892715907063064096616280387840444851916437908071861123706221334154150659918438759610239267132765469861636577066264386380298480519527695361952592409309086144719073907685857559347869817207343720931048254756285677776940815640749622752549933841128092896375169902198704924056175317863469397980246197370790418683299310165541507423083931768783669236948490259996077296842939774275362631198254166815318917632348391908210001471789321842278051351817349219011462468757698353734414560131226152213911787596883673640872079370029920382791980387023720780391403123689976081528403060511167094847222248703891999934420713958369830639
622320791156240442508089199143198371204455983440475567594892121014981524545435942854143908435644199842248554785321636240300984428553318292531542065512370797058163934602962476970103887422064415366267337154287007891227493406843364428898471008406416000936239352612480379752933439287643983163903127764507224792678517008266695983895261507590073492151975926591927088732025940663821188019888547482660483422564577057439731222597006719360617635135795298217942907977053272832675014880244435286816450261656628375465190061718734422604389192985060715153900311066847273601358167064378617567574391843764796581361005996386895523346487817461432435732
248643267984819814584327030358955084205347884933645824825920332880890257823882332657702052489709370472102142484133424652682068067323142144838540741821396218468701083595829469652356327648704757183516168792350683662717437119157233611430701211207676086978515597218464859859186436417168508996255168209107935702311185181747750108046225855213147648974906607528770828976675149510096823296897320006223928880566580361403112854659290840780339749006649532058731649480938838161986588508273824680348978647571166798904235680183035041338757319726308979094357106877973016339180878684749436335338933735869064058484178280651962758264344292580584222129
476494029486226707618329882290040723904037331682074174132516566884430793394470192089056207883875853425128209573593070181977083401638176382785625395168254266446149410447115795332623728154687940804237185874230262002642218226941886262121072977766574010183761822801368575864421858630115398437122991070100940619294132232027731939594670067136953770978977781182882424429208648161341795620174718316096876610431404979581982364458073682094040222111815300514333870766070631496161077711174480595527643483333857440402127570318515272983774359218785585527955910286644579173620072218581433099772947789237207179428577562713009239823979219575811972647
426428782666823539156878572716201461922442662667084007656656258071094743987401107728116699188062687266265655833456650078903090506560746330780271585308176912237728135105845273265916262196476205714348802156308152590053437211410003030392428664572073284734817120341681863289688650482873679333984439712367350845273401963094276976526841701749907569479827578258352299943156333221074391315501244590053247026803129123922979790304175878233986223735350546426469135025039510092392865851086820880706627347332003549957203970864880660409298546070063394098858363498654661367278807487647007024587901180465182961112770906090161520221114615431583176699
570609746180853593904000678928785488278509386373537039040494126846189912728715626550012708330399502578799317054318827526592258149489507466399760073169273108317358830566126147829976631880700630446324291122606919312788815662215915232704576958675128219909389426866019639044897189185974729253103224802105438410443258284728305842978041624051081103269140019005687843963415026965210489202721402321602348985888273714286953396817551062874709074737181880142234872484985581984390946517083643689943061896502432883532796671901845276205510857076262042445096233232047447078311904344993514426255017017710173795511247461594717318627015655712662958551
250777117383382084197058933673237244532804565371785149603088025802840678478094146418386592266528068679788432506605379430462502871051049293472674712674998926346273581671469350604951103407554046581703934810467584856259677679597682994093340263872693783653209122877180774511526226425487718354611088863608432728062277766430972838790567286180360486334648933714394152502594596525015209595361579771355957949657297756509026944280884797612766648470036196489060437619346942704440702153179435838310514049154626087284866787505416741467316489993563813128669314276168635373056345866269578945682750658102359508148887789550739393653419373657008483185
044756822154440675992031380770735399780363392673345495492966687599225308938980864306065329617931640296124926730806380318739125961511318903593512664808185683667702865377423907465823909109555171797705807977892897524902307378017531426803639142447202577288917849500781178893366297504368042146681978242729806975793917422294566831858156768162887978706245312466517276227582954934214836588689192995874020956960002435603052898298663868920769928340305497102665143223061252319151318438769038237062053992069339437168804664297114767435644863750268476981488531053540633288450620121733026306764813229315610435519417610507124490248732772731120919458
651374931909651624976916575538121985664322079786663003989386602386073578581143947158728008933741650337929658326184360731333275260236051155242272284472514638632693697637625101967143801256912277844284269994408291522159046944372824986580852051865762929927755088331286726384187132777808744466438753526447335624411394476287809746506839529821081749679588364522733446948737934717907100649782364660166805720342979292074468223228486658395222114468595728584038633772780302275915304978658739195136502462741958990883743873315942873720297706202071202130385721759332111624133304227737424163535535879770653096476858860773014327782903288947958184043
788585677729320944767786693575374600481423767411941826716368704810569111562156143575162905273512243500806046536689174581965494826086122607502930627614788132689552807361490225258196828150510333181321296596649581590304212387756459909732967280666838491662579497479229053618455637410347914307715611686504842924902811029925296787352987678292690407887784802624792227507359484058174390862518779468900459420601686051427722444862724699111462001498806627235388378093806285443847630532350701320280294883920081321354464500561349870178342711061581772898192906564986880810455622337030672542512772773302834984335957725759562247037077933871465930330
886296994403183326657975146765027173462988837773978482187007180267412659971587280354404784324786749071279216728985235884869435466922551013376063779151645972542571169684773399511589983490818882812639844005055462100669887926145582145653196969098272539345157604086134762587781658672944107753588241623157790825380547469335405824697176743245234514984830271703965438877376373581917365824542733474904242629460112998819165637138471118491569150547681404117498014542657123942044254410280758060013881986506137592885390389226443229479902864828400995986759635809991126953676015271730868527565721475835071222982965295649178350717508357413622825450
556202709694174767992592297748886274113145876761475314568953280931170526964864101874076732969866492364373825654750228164719268155598831966298483077766668406223143158843849105190582818167407644630333001197102930364558665946518690744752508378419876229904159117936827997606541860887216266548864923443910309232569106337759697390517811227646684867917360494043937033393519006093872683972992464784837272747709774666935997848571201567890002419472692209749841273231474015499809203814598214164811763571478015542315996678385348544864069364105569135313352311840535813489409381918218986948253839609899428220275993396352062177053435720733962505742
167694651016084956014393032443042715760995273086846092044222261031542299844448021100981613338248273752189987382053151649271344981059501599748005715919122021544877487501034732461906339413030308923994119850062259021841644099881732143244221085542486208962502606043981801890263177811466174549997714406652328638463638470016556181538610981881111817341913055050248603458567555856375117297742993290749442365796683327009183673389773479017592488856603799527715405690830173117238941403261596122929122251910959487438056733812785386164918427869384175568980471008598683720336151751580970225662752001609561922299254017598785220385459137717839763898
111984858032910487516669211951045148966777615982494687274206634375932078526189226872855276713248832677941529128391654079683441902390948036766887078380113670427539713962014247849351967353014444040378235266744375567408830252257452738062099804512331881027290120429979890054231262179681352377580411625114591759932791341765072928267622368972919605282896752235214252342172478418693173974604118776346046256371353098015906177367587153368039585590548273618761121513846734328843250900456453581866819051087317913462157303395405809871720138443770992795327976755310993813658404035567957318941419765114363255262706397431465263481200327200967556677
019262425850577706178937982310969867884485466595273270616703089182772064325519193936735913460377570831931808459295651588752445976017294557205055950859291755065101156650755216351423181535481768841960320850508714962704940176841839805825940381825939864612602759542474333762262562871539160690250989850707986606217322001635939386114753945614066356757185266170314714535167530074992138652077685238248846006237358966080549516524064805472958699186943588111978336801414880783212134571523601240659222085089129569078353705767346716678637809088112834503957848122121011172507183833590838861875746612013172982171310729447376562651723106948844254983
695141473838924777423209402078312008072353262880539062660181860504249387886778724955032554242842265962710506926460717674675023378056718934501107373770341193461133740338653646751367336613947315502114571046711614452533248501979010834316419899984140450449011301637595206757155675094852435802691040776372109986716242547953853128528899309565707292186735232166660978749896353626105298214725694827999962208257758409884584842503911894476087296851849839763679182422665711671665801579145008116571922002337597653174959223978849828147055061906892756252104621856613058002556079746097267150333270323100252746404287555565468837658388025432274035074
316842786206376970547917264843781744463615205709332285872843156907562555693055588188226035900067393399525043798874709350792761811162763097712579839759965266121203174958820594357548838622825084014088857205839924009712192125480740977529742787759125660264434827136472318491251808662787086261166999896348124058036847945873648201246536632288890116365722708877577361520034501022688901891016735720586614100117236647626578353963642978190116470561702796319223322942287393092333307482589376261989975965300841353832411258996396294451290828020232254989366275064995308389256322467946959606690469066862926450062197401217828998729797048590217750600
928933289572723920195899944719451473608507704007257174393181484619094062695452850305263410005650222261523093648828871220464542677005771489943351471625042523651737102660686472534581201866832739536825474565365535975466857887000569883602866864507402569930874834410940860863037079082952405767316849418558104824753047589233928015713028241062349999459323905214098565595656613460033961505151647588527422147325179995489779928495227460298556667008118712008561550164574004841702103030389963392533374665568178244107374093369192941046323077319947598263073834996007703724104462854146487041162738956498345551621656851145513838220470054839966717062
464675661012913820489091211172293862442531589130669874620455872448060528293781483026221645422804217577607623654598282230708155034694049383177550533050946989994761194192312807218072169643784333136067606769651871383943387724854936890618457005720436966664650807344958144959663062466986798328725863000642152202101718139173252751736722626214549454685060063346927138383117158497530926432524869602200590998026637653862254632651684149633063695480865511012567577178906166947583440434862184853695916021720304561834975241620399264413316518847686068306420048585579244733402901425888764037125186422290163336915850632737271995963629127833447862188
878710095337535510546889802363782637149269132895643394408994701214521345721177156575914517348951950168006213539271754198438761635434798069208866662270995123717062419249142825764531257699397353416730468645851819796682320156937926849269999839924135719414968822737040228208051718080034004806152617920139789451862952905584407037383005335524211539033851858293667791906101163062336731444192028938572018555695963308336154502904248223092970871247880020173830720604826801566753975937899317935157999589295621563073384162945999002767308328277165950642179665231904392505432267537318117553154767807394703389311851072977243183789726749574557781833
454959423173535582910469673153912759756872818616911610831563372326399688814905439432611971822749967911766285534018601983158096299817911072088049922920160620590672712735994618716349457749958053379471871054564525793960242102591364155283983952017730127125148920510617082280083399856657866469207371142696823017704163248294794095586946990893791651910063051853521023451897981276191430618643627030819771249927510567329094812020577471006877033797089342292071839037441675034938188363422292849467906602856742932516425690443634730876567970565956772852910812427331544065801998027115791262541727974528625748659219332938059152395247355188871198603
913196542875762901905039640835602462775343144091556421817294599415960619796226332427158634259779473486820748020215387347297079997533329877855310538201621697918803807530063343507661477371359393626519052222425281410847470452956886477579135021609220403484491499507787431071896557254926512826934895157950754861723413946103651766167503299486422440396595118822649813159250801851263866353086222234910946290593178294081956404847024565383054320565069244226718632553076407618720867803917113563635012695250912910204960428232326289965027589510528443681774157309418748944280654275614309758281276981249369933130289466705604140843089422311409127222
381484703643410196304136307367710600381595908297464101144213583210425743583502207371732197450890355731873504458272387707282714061629979196293572241044771550516525358675441093950792183690152611384403826800541509243465117114364778994445539936536677275895657139875055429908245856095100369346631006737147080299276569334355009271898540501099174749799915543920319089619676154446860481754006956894714639282453838070104441810455061713051605843558175210323384658292010710300611242834074586070060601948305513648670210203647084708074227043718937069656887956179287130452245168420274020219664156052803350612935587390793935244040925842483806071774
446099640352218910229619090325690423813744924949068923143308842243996313963915458540652863264688075811487483714082841764552263863135202648940162624948023885682315991029526203371264492799019382111345184463875445163912393779741905766499117642376377222828023184657380501212778096803156914772649102575035087587922481102235445244108724485657007551871321465920935485045528291707495967754044507794948363717560623269257574128131102419103733380804343253108846948315557294022653949729138175813386194570577995618087559514136449076131096171559283765858400364893740768222575239359887310816896676882874038371928276904315141069976783038190856907130
919313408460195111474827663507246765349220400586266776329355166319396224989799127080044659822648991252268131243005281049950585956765271235914944426125544376186450292028813585828717895772241163808151618316031297287969874801398286216456291961530963583373136197247733323530254665711969026112373806290302429042757945490300226608474465131617416919168517464649454596960053308852527920834724952354731106741090992235410555062996876421539512493559863113466617251168907856333289355691504494851891134883018763651006385025659164330219285655962639143828950683248387271656165601115315170552229557659449724547888155323164174532671679788611411653555
975883319796380709629988807673036169403177364481404278677842512324499746934213482171795951906982046029971720011748573038897192055974147424530111358697662566077709702256332617011084637847955552585045780588794407560649741279745309184184052075585264622088214836467546522376092107875391904546848523497599860449433228280731206799224024775075141058907746273343190912554513522253292759138420473846030561631542365529353122783897594465157873373434631722800010313804254814040220905804050560038609374034350688630814346838489007089385650500275690596780694046984351845351341410316151336830437147866429253897171659786290107284007589397003883177426
481637251132773699268277094653425835961118819550924620621539781211972447626237715344520480698190825249439639622511138311774289785358255908324904804975160471042575697534425515157798156003708472306034847539775136883904043160174862488713393118185230294254256762024856883939708367487884537891725741451559179190353985350772009005949793529394596312134455033682606900598287177235333752219419155473037420623432628929683970150588921911120492498647920534108723491154309871821600557622090757323046261065977449476583463130255986363150299596723524769439754625302067881933043722848002093053541556406648385693781446031386975634592002334626069959555
134847541478911808303298164215874529229526789379256477520290526753493566737442931826733745716424654077482679010467787590854081305314471764558698941696689404364899524652474439883495838712062964854133575538134195004987438133690627039738745866042968715958207157665998266073170056244655417630245013491595672889426197461444969086716558597827292287027237748350973629010191304178127357730377818040815891360052073158069410343050031843493423602692447330600138611197817744726696089283210525431164960334201020326038636725328896483334058622048436165753620014684054766496664735669795729533948091382637033242209308393669549806882404916220631479114
946420425000224504134255585619374429052572524363200544874415243073052150704910204340765724768650957511741254137295316445217655772353486018215668333525205328300001083440087622668438170232356056451582569541773591978136499755596019125677449427179863600458474052092900893973152760243049516538644313881478769775414787574326101598797097588556258067661979730984724607694848211279484279765366070550516391044150225544203297212920330093533566872945959123279658863764868941884336405484940095749657916576872139273301535550978651147679473996906231848783775154626138236516659563372093457082083018404827970057280714329257275774362295870473616416097
318172415942042703660664040897402455215307252273886372418596464552236732604111645984640200102169208233151553888210715271912678765317950719082045251004478212913185440548144941518671142071036938911291250127508534663377177493760165434546963900427111298292550968304206657253642794722000208353138837087816499571897176293387948542712768826520037663259245616148687448974715193662192756658524621144574070106753804275641844408348052038382650526016985840600847884224218878569278977518104428054744272294551674203356864606099779731249504333214252050536757904995207835976504153790011325795360406551726548790221735954441511394292316489506631778130
390574620824491719213118641296337046614064569001789423567387755231309527859127745332418554424844844936642107313488191806401892223173021566458134731864499979057816620914698707180393888857812807402263636022941143548698714021435720559477308928086536789202019351026053615679244832767494761178583160718657103108422005602595451151913913091195444478443610327418761023388433916875892334237908598419682665256106287512375723184914749519459857288979349817917618226524804082371281097907726388642860679170822885758527034708397145616199262478447946927949968459456323827022973641735034307831941156982478200132908512028784748058601889600459017459740
556307327144876790852888679788099706952406810066256114400149834135808897372468440649488570741676879164132242053736540673301863924979109154747859591638655975070905811759248995022147992509456355825143158144640601342834904227983579396592589852007638456466816407326819283460077672858762849000688745646392749644159040340336723378144915970329417872941550610541295154001593938516639293256774295575494800466582735796539909402335436446493768272725418736275475329768081903253361410864330842377717389952215367630953020459024386946327028952939944830135775890812148845584938198745059209140672095224690962630769417533409836988593637003149737289779
963600186265001749292900879311899978229637123066422979961635825726001122889836476514180459757700421208339493646596473364642890444993253962270919073737057720513228159578632275919127860542978629531886155598047281607108641328035854001600555756868557917859778991979026565926212830072253514015259735693007290153922111168685047404021721744420517380002513610004945341193243316683442431259630988123969622023588583955878316851948331266535773532443799356832152691770422490345745348589138125826813669089294768090526355606381196613060639369384118177135459298843172329122362624588683942028899816935611698654298847765131182276625267399788088160104
706515423350156713537448170862343146625311902910401522629271040992850724188433290072777947541116375521765635893163266360493812184018375128188847711689754794837676640848427536230740195421832179854962606665903479258163423926709478399070629231665350372850197513248138038370708946389254708870390857235810061306286466647100061043521157789266134322146553114118825969429262845221090266884149757633415549211355812546165580782734701158140060083457621331303899878432706537199567095708473857860926491888583787392391655542635773012922436416040625517368923356365688543658516462078218757417243645258141434876327613417527073767549222762877822647651
543153415857137735227303354033763642042580342572647496862178236669513534106773784211313711319873732228918052750628122777164124944124012071259543199917465747458925826137128255555350804041439445572959945546356084872513394629363589408320989648016195831304297209647941285393889962653689282638076771687595885022164645824309401650096887973661577335603168367103868952282709415095452227440027354992536702147159940565448138421863801287999008209335763207363694059914242637182940006137419005795130962985453307481978025683010896728738022348204888629731303696898826406579047815623897784853650256910642317957360253309087632717849111897484322468680
863403839641761276057886465744722848249326874430625512205069551684646694771836819114328735448158363505481464110999601433905957997662906468812950250391509236330110760706328633173933781496933802475800350527897827557509286040394205063429393270646361610318228792481526793068627492372756318522256542660085568494977202859091509304954259674736483314372363495554489015986684083621769135596560395196704253688634823695871294625247590317768131849775882765767404825581365021036495855057032592199576753342642237837235860585094035839771034766706447886408311096503025652156074640196527169997323734652371734565955145594930981666440062115993491331801
351505286518421788280263433259347558507611686977091255800561856837105408560812495194031480646187194025776632852670196983875675615246967590281068648968692933159543520976875271372016161609311742501997092896849400346962423256884106651133043774122561762586589412367281711455264238945126317178347902769211714528873529550193367592189080060486337377867281806102547825704367884495035189257874998366947859086129755430841226770609543476121337174331567837901620123372370233383164147064285921859776101582327219979150628718681867509816655377450130208803339043536397702633638090985264945326281465580655465048234864294953906132574004969128883405182
229336444766838550379679758096199835758070277595359687882261946596122230445492756002749551685835425822953360428344263184780688253954507466918778977654060384325128438128113168562046086172894082296586261744207669202974279300881295198546787135486232366104132165812792671515459615943525934567574459923078892055195400823164097195912500254552375031067356397488355424804496813830306718519314913357892021236053081999520205845034234999321509626349778124566583046805818245635248146258493319261954068848184464452484294860630161694766632426252314763223711096953694838244823164103962245076754056142874682678357237048956069906527926884558445120466
548533785340266466450423396384882577198749536113004942155937355452119261867214782654168856040949282900566168838076376566905107408925105491652229688786769686316525149177014999000666373445461202627807019256987062255409289451947187780043061300218282874258670487484808269485734447782440787341027108248702695238308049109604820139012940246312448001593366702126583176778797529659634725768943265404358892672939506878608306262662632873920873273025479100999321133889778078143367287914487683736864677485287777374035474728716442177678207129645062708809786379281440711925051411480049070556080972292997924414710628522470298706998692276763417735132
586029089038757074543680778764223853337006920896163510092335873039865439060718809525575533803647258950073067721225280781794710564811713785574510576910443229254290241494335883960936793213616969542512997310310328044369545019298438208423831212658257405945094269427773071248021769157818357200871705387732560179871330055059113778238417916402808414096238208476373930139307784285545452223675598246662506087542848761041456613622276424059143044555808563181809352304077938916149021162924005150749140684432032303656099548786209991943065644553325471355573653185160117003215506907877167520628815278858971494103209869840830489665243510305024446799
317791476591034289491290541203616016956712221408063694059403045521862128799330928562310224184463652890974446401519866231838819624448225907835859140436861930190414589626938789070349821698686969344480862139905345917928266543047982072196341347556465254831437711566784590777971965107724680002935815462676463102242790073136313525220670629511259358744731341864924972827847966445854489629329052620580652485887070208793891344760833446531709392424082493280089157313195413483118209277524868805487339433158675626661221793550511906099929113794456349956273918984590290217131557060962678816733029401984642373904450980280309489759812592520558509735
374365568257803136819020071516756938272818188245875417107211808065564480391225045370894226953583821925350756928340956398592655997403913167092900439962759768303752175033608790282956730688622630777297335338536826687345190357097096873223237383004940901232392743187590465263270951784062672648288936468965932191695211063617297570743761480616013311049116922713186094041450148428664236347169828924181804843652305388645598098392738364906854808230142678031439374404318078226787794940062064891512489525165430056344483750467517542070433133724868706332375616452323604819320243775968909147833721795536769926032357151855133910984027390637532807023
133017557542693962026294239109453235379101259489649418125636729929670842506675998034562734555985596285122814145825560248417833056452405084500659887559875186013358606249327844877720068422965919455165395629829605916100465789072148420548618304181756045598151680880317830802614459944446779180124321464009836106786834129748725967292587868062230801158220262890143644590023016458236667092655712645599257906223047452356255751117707915120027893809757754685461210173075227992414070263081377929719094614131458020810877381216245398587696973714258818361526050693809269177120873219150058319771133227935723850719406127612918725720994049302502777481
566140213274347438819664133300526342290829064009279449248085561311834401618048013570325078363239389215676431596204426128097009441077761306389090712944563940566015592460254542047711861404201552333712705013771210345700095780093892653293857204785765087771496634030035623805957571916093821713122228104658583889435071764319399730126615914238371702844001203994858809962318594724748587765843550770069340992203403787721927283703013808381443941149849717307661629613420591050148142839497006959516769390415579028563569110555473126845714974496353205546779407751840566676372229690903461287068298871042787610900909991604438217945117636208353797161
618331243644312678554355508005079861246643977241355021282380267267199149897272485129812872836974892764207928686669701772597944078581559093325085541312999465811185276916524647908191193842332758976995730120981030091710016957187916169422700795289151919125210538918385389593151674005057238174010306210043802430111879777042523280732365751296093724560536800375165961642361477093303912244097528717320679761281204280267392565573056759315126457500478757565318548258214115740304731474925119108356157657320025461096867018903076485313738329126824817411813590328266250825493132114314789533523170439890539285349466428860742683718249024980924794872
266336868237995808756370408086556493219054896377855495311673979352707994704523991532975343586905141058640965345141828964744393671828527118435607992858959781765439501130888484191635166732136928608309567445028018003737164580091680829727087156091850386540534366600455049856246873760225570415958002501740953618392876434580036708649540579417200851363571271637683234931342307038212744845014405295416953743819454594565331651409909937227228010196546527262278315121034676861668261314718436100255178632479501500229536954663177395893441314814858346943745239811599546660712059977943634401850783608991089480734196339392593189739409431100421167291
201997226266098719270140241058055153151001098049960441472910394510303126641147267368399733150350367427415469926331652704329406752374490750567395089296747791158008643999925648172088474292508215462798560791277686119460862103494055358501344721902445438245210892844094981327170106739664711149318967899776615954881861931769001750279017838246243878738314832795008790264339925770265880058497789846242956603212769458108243481296908409725506710547324713172549971919010395533058470407280816931586260938860191476899441376736214320836073751315743763167546664791867538965715551008506268100051198274868077805926677656541008347785710242501332533915
873847610241297947367510011634989778037459300254576098706710921535971151782520142812166475430340751286002402970384286159842898166021434298490889173596821922844691230359043298772318433099141872646746075583187257131388323560158090095941825302077993976484625979018833417938309209658414635744119858782964758509430530081483418217478266037737622529977034687529035173107920832200380808092121643465868179898105042743753857867891863505177175016065318264069288832501359195171785376878658817523664215340109612957630747626480703127573657877623528590571539324845765039443904966680877118991924989338965248523955367958275306141671317579157563866060
048399941795487058682092011951549520312945624513154225065748586291616065237966430101726939502822946674896817468211639967949502942840130992359012782504374281925576345332175761622927511105983682715672297786200537229323140828870587494440601162365216277175585030134514714527658418642770717699684354996202575474318119948833858067596923595806221658324640920953506483579358177429030183153512900143214955181774569083887193206977696956577717544991499114313689508361606925396064698933748709429332191856012991085644702562571635055086206892402975896847142836786847354555335834776525361565781899969830686546717364459963431364681954274204904724330
646750014426975083223690130838954926370667784065313286648860801295137717208475811577194910123451417749414827735800414326673323796177169656985827858323005052658835022478680506482014445705931973433829238600726016965109032589809099128376522753814935298450994149669338628155680313069810645251927038185158726486917625632394414252161184277691450677184117357143966810056154839524431549448642383842989003998261133224689633465221046925451379692760097196453389553321055842456401874486110509591117668289427116400540105037704203460525213182280458929986379035723506651087823500433499423912852363088965109892466410563315841711428853041437722866298
323189708690304003013259514767742375161588409158380591516735045191311781939434284829222723040614225820780278291480704267616293025392283210849177599842005951053121647318184094931398004440728473259026091697309981538539390312808788239029480015790080000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000

```



## Ring


```ring

give n
x = fact(n)
see n + " factorial is : " + x

func fact nr if nr = 1 return 1 else return nr * fact(nr-1) ok

```



## Robotic


### Iterative


```robotic

input string "Enter a number:"
set "in" to "('ABS('input')')"
if "in" <= 1 then "one"
set "result" to 1

: "factorial"
set "result" to "('result' * 'in')"
dec "in" by 1
if "in" > 1 then "factorial"
* "('result')"
end

: "one"
* "1"
end

```



## Rockstar

Here's the "minimized" Rockstar:

```Rockstar

Factorial takes a number
If a number is 0
Give back 1.

Put a number into the first
Knock a number down
Give back the first times Factorial taking a number

```


And here's a more "idiomatic" version:

```Rockstar

Real Love takes a heart
A page is a memory.
Put A page over A page into the book
If a heart is nothing
Give back the book

Put a heart into my hands
Knock my hands down
Give back a heart of Real Love taking my hands

```



## Ruby

Beware of recursion! Iterative solutions are better for large n.
* With large n, the recursion can overflow the call stack and raise a SystemStackError. So factorial_recursive(10000) might fail.
* [[MRI]] does not optimize tail recursion. So factorial_tail_recursive(10000) might also fail.


```ruby
# Recursive
def factorial_recursive(n)
  n.zero? ? 1 : n * factorial_recursive(n - 1)
end

# Tail-recursive
def factorial_tail_recursive(n, prod = 1)
  n.zero? ? prod : factorial_tail_recursive(n - 1, prod * n)
end

# Iterative with Range#each
def factorial_iterative(n)
  (2...n).each { |i| n *= i }
  n.zero? ? 1 : n
end

# Iterative with Range#inject
def factorial_inject(n)
  (1..n).inject(1){ |prod, i| prod * i }
end

# Iterative with Range#reduce, requires Ruby 1.8.7
def factorial_reduce(n)
  (2..n).reduce(1, :*)
end


require 'benchmark'

n = 400
m = 10000

Benchmark.bm(16) do |b|
  b.report('recursive:')       {m.times {factorial_recursive(n)}}
  b.report('tail recursive:')  {m.times {factorial_tail_recursive(n)}}
  b.report('iterative:')       {m.times {factorial_iterative(n)}}
  b.report('inject:')          {m.times {factorial_inject(n)}}
  b.report('reduce:')          {m.times {factorial_reduce(n)}}
end
```

The benchmark depends on the Ruby implementation.
With [[MRI]], <code>#factorial_reduce</code> seems slightly faster than others.
This might happen because <code>(1..n).reduce(:*)</code> loops through fast C code, and avoids interpreted Ruby code.

```txt
                       user     system      total        real
recursive:         2.350000   0.260000   2.610000 (  2.610410)
tail recursive:    2.710000   0.270000   2.980000 (  2.996830)
iterative:         2.250000   0.250000   2.500000 (  2.510037)
inject:            2.500000   0.130000   2.630000 (  2.641898)
reduce:            2.110000   0.230000   2.340000 (  2.338166)
```



## Run BASIC


```runbasic
for i = 0 to 100
   print " fctrI(";right$("00";str$(i),2); ") = "; fctrI(i)
   print " fctrR(";right$("00";str$(i),2); ") = "; fctrR(i)
next i
end

function fctrI(n)
fctrI = 1
 if n >1 then
  for i = 2 To n
    fctrI = fctrI * i
  next i
 end if
end function

function fctrR(n)
fctrR = 1
if n > 1 then fctrR = n * fctrR(n -1)
end function
```



## Rust


```rust
fn factorial_recursive (n: u64) -> u64 {
    match n {
        0 => 1,
        _ => n * factorial_recursive(n-1)
    }
}

fn factorial_iterative(n: u64) -> u64 {
    (1..n+1).fold(1, |p, n| p*n)
}

fn main () {
    for i in 1..10 {
        println!("{}", factorial_recursive(i))
    }
    for i in 1..10 {
        println!("{}", factorial_iterative(i))
    }
}

```



## SASL

Copied from SASL manual, page 3

```SASL

fac 4
where fac 0 = 1
      fac n = n * fac (n - 1)
?

```



## Sather


```sather
class MAIN is

  -- recursive
  fact(a: INTI):INTI is
    if a < 1.inti then return 1.inti; end;
    return a * fact(a - 1.inti);
  end;

  -- iterative
  fact_iter(a:INTI):INTI is
    s ::= 1.inti;
    loop s := s * a.downto!(1.inti); end;
    return s;
  end;

  main is
    a :INTI := 10.inti;
    #OUT + fact(a) + " = " + fact_iter(a) + "\n";
  end;
end;
```



## Scala



### Imperative

An imperative style using a mutable variable:

```scala
def factorial(n: Int)={
  var res = 1
  for(i <- 1 to n)
    res *= i
  res
}
```



### Recursive

Using naive recursion:

```scala
def factorial(n: Int): Int =
  if (n == 0) 1
  else        n * factorial(n-1)
```


Using tail recursion with a helper function:

```scala
def factorial(n: Int) = {
  @tailrec def fact(x: Int, acc: Int): Int = {
    if (x < 2) acc else fact(x - 1, acc * x)
  }
  fact(n, 1)
}
```



### Stdlib .product

Using standard library builtin:

```scala

def factorial(n: Int) = (2 to n).product

```



### Folding

Using folding:

```scala
def factorial(n: Int) =
  (2 to n).foldLeft(1)(_ * _)
```



### Using implicit functions to extend the Int type

Enriching the integer type to support unary exclamation mark operator and implicit conversion to big integer:

```scala
implicit def IntToFac(i : Int) = new {
  def ! = (2 to i).foldLeft(BigInt(1))(_ * _)
}
```


```txt

scala> 20!
res0: scala.math.BigInt = 2432902008176640000

scala> 100!
res1: scala.math.BigInt = 93326215443944152681699238856266700490715968264381621468592963895217599993229915608941463976156518286253697920827223758251185210916864000000000000000000000000

```



## Scheme


### Recursive


```scheme
(define (factorial n)
  (if (<= n 0)
      1
      (* n (factorial (- n 1)))))
```

The following is tail-recursive, so it is effectively iterative:

```scheme
(define (factorial n)
  (let loop ((i 1)
             (accum 1))
    (if (> i n)
        accum
        (loop (+ i 1) (* accum i)))))
```


### Iterative


```scheme
(define (factorial n)
  (do ((i 1 (+ i 1))
       (accum 1 (* accum i)))
      ((> i n) accum)))
```


### Folding


```scheme
;Using a generator and a function that apply generated values to a function taking two arguments

;A generator knows commands 'next? and 'next
(define (range a b)
(let ((k a))
(lambda (msg)
(cond
	((eq? msg 'next?) (<= k b))
	((eq? msg 'next)
	(cond
		((<= k b) (set! k (+ k 1)) (- k 1))
		(else 'nothing-left)))))))

;Similar to List.fold_left in OCaml, but uses a generator
(define (fold fun a gen)
(let aux ((a a))
	(if (gen 'next?) (aux (fun a (gen 'next))) a)))

;Now the factorial function
(define (factorial n) (fold * 1 (range 1 n)))

(factorial 8)
;40320
```



## Scilab

===Built-in===
The factorial function is built-in to Scilab.
The built-in function is only accurate for <math>N \leq 21</math> due to the precision limitations of floating point numbers, but if we want to stay in integers, <math>N \leq 13</math> because <math>N! > 2^31-1</math>.

```Scilab
answer = factorial(N)
```



### Iterative

<lang>function f=factoriter(n)
    f=1
    for i=2:n
        f=f*i
    end
endfunction
```



### Recursive

<lang>function f=factorrec(n)
    if n==0 then f=1
            else f=n*factorrec(n-1)
    end
endfunction
```



### Numerical approximation

The gamma function, <math>\Gamma(z)=\int_0^\infty t^{z-1} e^{-t}\, \mathrm{d}t \!</math>, can be used to calculate factorials, for <math>n! = \Gamma(n+1)</math>.
<lang>function f=factorgamma(n)
    f = gamma(n+1)
endfunction
```



## Seed7

Seed7 defines the prefix operator [http://seed7.sourceforge.net/libraries/integer.htm#!%28in_integer%29 !] ,
which computes a factorial of an [http://seed7.sourceforge.net/libraries/integer.htm integer].
The maximum representable number of an integer is
[http://seed7.sourceforge.net/libraries/integer.htm#(attr_integer)._last 9223372036854775807].
This limits the maximum factorial for integers to factorial(20)=2432902008176640000.
Because of this limitations factorial is a very bad example to show the performance advantage of an iterative solution.
To avoid this limitations the functions below use [http://seed7.sourceforge.net/libraries/bigint.htm bigInteger]:

### Iterative


```seed7
const func bigInteger: factorial (in bigInteger: n) is func
  result
    var bigInteger: fact is 1_;
  local
    var bigInteger: i is 0_;
  begin
    for i range 1_ to n do
      fact *:= i;
    end for;
  end func;
```


Original source: [http://seed7.sourceforge.net/algorith/math.htm#iterative_fib]

### Recursive


```seed7
const func bigInteger: factorial (in bigInteger: n) is func
  result
    var bigInteger: fact is 1_;
  begin
    if n > 1_ then
      fact := n * factorial(pred(n));
    end if;
  end func;
```


Original source: [http://seed7.sourceforge.net/algorith/math.htm#fib]


## Self

Built in:

```self>n factorial</lang

Iterative version:

```self
factorial: n = (|r <- 1| 1 to: n + 1 Do: [|:i| r: r * i]. r)

```

Recursive version:

```self
factorial: n = (n <= 1 ifTrue: 1 False: [n * (factorial: n predecessor)])
```

Factorial is product of list of numbers from 1 to n.
(Vector indexes start at 0)

```self
factorial: n = (((vector copySize: n) mapBy: [|:e. :i| i + 1]) product)
```



## SequenceL

The simplest description: factorial is the product of the numbers from 1 to n:

```sequencel
factorial(n) := product(1 ... n);
```


Or, if you wanted to generate a list of all the factorials:

```sequencel
factorials(n)[i] := product(1 ... i) foreach i within 1 ... n;
```


Or, written recursively:

```sequencel
factorial: int -> int;
factorial(n) :=
		1 when n <= 0
	else
		n * factorial(n-1);
```


Tail-recursive:

```sequencel
factorial(n) :=
	factorialHelper(1, n);

factorialHelper(acc, n) :=
		acc when n <= 0
	else
		factorialHelper(acc * n, n-1);
```




## SETL


```setl
$ Recursive
proc fact(n);
    if (n < 2) then
        return 1;
    else
        return n * fact(n - 1);
    end if;
end proc;

$ Iterative
proc factorial(n);
    v := 1;
    for i in {2..n} loop
        v *:= i;
    end loop;
    return v;
end proc;
```



## Shen


```shen
(define factorial
    0 -> 1
    X -> (* X (factorial (- X 1))))
```



## Sidef

Recursive:

```ruby
func factorial_recursive(n) {
    n == 0 ? 1 : (n * __FUNC__(n-1))
}
```

 
Catamorphism:

```ruby
func factorial_reduce(n) {
    1..n -> reduce({|a,b| a * b }, 1)
}
```

 
Iterative:

```ruby
func factorial_iterative(n) {
    var f = 1
    {|i| f *= i } << 2..n
    return f
}
```

 
Built-in:

```ruby
say 5!
```



## Simula


```pascal
begin
    integer procedure factorial(n);
    integer n;
    begin
        integer fact, i;
        fact := 1;
        for i := 2 step 1 until n do
            fact := fact * i;
        factorial := fact
    end;
    integer f; outtext("factorials:"); outimage;
    for f := 0, 1, 2, 6, 9 do begin
        outint(f, 2); outint(factorial(f), 8); outimage
    end
end
```

```txt
factorials:
 0       1
 1       1
 2       2
 6     720
 9  362880
```



## Sisal

Solution using a fold:

```sisal
define main

function main(x : integer returns integer)

  for a in 1, x
    returns
      value of product a
  end for

end function
```

Simple example using a recursive function:

```sisal
define main

function main(x : integer returns integer)

  if x = 0 then
    1
  else
    x * main(x - 1)
  end if

end function
```



## Slate

This is already implemented in the core language as:

```slate
n@(Integer traits) factorial
"The standard recursive definition."
[
  n isNegative ifTrue: [error: 'Negative inputs to factorial are invalid.'].
  n <= 1
    ifTrue: [1]
    ifFalse: [n * ((n - 1) factorial)]
].
```

Here is another way to implement it:

```slate
n@(Integer traits) factorial2
[
  n isNegative ifTrue: [error: 'Negative inputs to factorial are invalid.'].
  (1 upTo: n by: 1) reduce: [|:a :b| a * b]
].
```

```txt
slate[5]> 100 factorial.
93326215443944152681699238856266700490715968264381621468592963895217599993229915608941463976156518286253697920827223758251185210916864000000000000000000000000
```



## Smalltalk

Smalltalk Number class already has a <tt>factorial</tt> method; however, let's see how we can implement it by ourselves.

### Iterative with fold

```smalltalk
Number extend [
  my_factorial [
    (self < 2)
        ifTrue: [ ^1 ]
        ifFalse: [
	    ^ (2 to: self) fold: [ :a :b | a * b ]
        ]
  ]
].

7 factorial printNl.
7 my_factorial printNl.
```



### Recursive


```smalltalk
Number extend [
  my_factorial [
    self < 0 ifTrue: [ self error: 'my_factorial is defined for natural numbers' ].
    self isZero ifTrue: [ ^1 ].
    ^self * ((self - 1) my_factorial)
  ]
].
```

===Recursive (functional)===

```smalltalk
  |fac|

  fac := [:n |
    n < 0 ifTrue: [ self error: 'fac is defined for natural numbers' ].
    n <= 1
        ifTrue: [ 1 ]
        ifFalse: [ n * (fac value:(n - 1)) ]
  ].
  fac value:1000.
].
```

```smalltalk
| fac |
fac := [ :n | (1 to: n) inject: 1 into: [ :prod :next | prod * next ] ].
fac value: 10.
"3628800"
```

```smalltalk
fac := [:n | (1 to: n) product].
fac value:40
-> 815915283247897734345611269596115894272000000000
```



## SNOBOL4

Note: Snobol4+ overflows after 7! because of signed short int limitation.

### Recursive


```SNOBOL4
        define('rfact(n)') :(rfact_end)
rfact   rfact = le(n,0) 1 :s(return)
        rfact = n * rfact(n - 1) :(return)
rfact_end
```

===Tail-recursive===

```SNOBOL4
        define('trfact(n,f)') :(trfact_end)
trfact  trfact = le(n,0) f :s(return)
        trfact = trfact(n - 1, n * f) :(return)
trfact_end
```


### Iterative


```SNOBOL4
        define('ifact(n)') :(ifact_end)
ifact   ifact = 1
if1     ifact = gt(n,0) n * ifact :f(return)
        n = n - 1 :(if1)
ifact_end
```

Test and display factorials 0 .. 10

```SNOBOL4
loop    i = le(i,10) i + 1 :f(end)
        output = rfact(i) ' ' trfact(i,1) ' ' ifact(i) :(loop)
end
```

```txt
1 1 1
2 2 2
6 6 6
24 24 24
120 120 120
720 720 720
5040 5040 5040
40320 40320 40320
362880 362880 362880
3628800 3628800 3628800
39916800 39916800 39916800
```




## Spin

```spin
con
  _clkmode = xtal1 + pll16x
  _clkfreq = 80_000_000

obj
  ser : "FullDuplexSerial.spin"

pub main | i
  ser.start(31, 30, 0, 115200)

  repeat i from 0 to 10
    ser.dec(fac(i))
    ser.tx(32)

  waitcnt(_clkfreq + cnt)
  ser.stop
  cogstop(0)

pub fac(n) : f
  f := 1
  repeat while n > 0
    f *= n
    n -= 1
```

```txt
1 1 2 6 24 120 720 5040 40320 362880 3628800
```



## SPL


```spl
fact(n)=
  ? n!>1, <=1
  <= n*fact(n-1)
.
```



## SSEM

The factorial function gets large quickly: so quickly that 13! already overflows a 32-bit integer. For any real-world algorithm that may require factorials, therefore, the most economical approach on a machine comparable to the SSEM would be to store the values of 0! to 12! and simply look up the one we want. This program does that. (Note that what we actually store is the two's complement of each value: this is purely because the SSEM cannot load a number from storage without negating it, so providing the data pre-negated saves some tiresome juggling between accumulator and storage.) If word 21 holds <i>n</i>, the program will halt with the accumulator storing <i>n</i>!; as an example, we shall find 10!

```ssem
11100000000000100000000000000000   0. -7 to c
10101000000000010000000000000000   1. Sub. 21
10100000000001100000000000000000   2. c to 5
10100000000000100000000000000000   3. -5 to c
10100000000001100000000000000000   4. c to 5
00000000000000000000000000000000   5. generated at run time
00000000000001110000000000000000   6. Stop
00010000000000100000000000000000   7. -8 to c
11111111111111111111111111111111   8. -1
11111111111111111111111111111111   9. -1
01111111111111111111111111111111  10. -2
01011111111111111111111111111111  11. -6
00010111111111111111111111111111  12. -24
00010001111111111111111111111111  13. -120
00001100101111111111111111111111  14. -720
00001010001101111111111111111111  15. -5040
00000001010001101111111111111111  16. -40320
00000001011011100101111111111111  17. -362880
00000000100001010001001111111111  18. -3628800
00000000110101110111100110111111  19. -39916800
00000000001000001100111011000111  20. -479001600
01010000000000000000000000000000  21. 10
```



## Standard ML


### Recursive


```sml
fun factorial n =
  if n <= 0 then 1
  else n * factorial (n-1)
```

The following is tail-recursive, so it is effectively iterative:

```sml
fun factorial n = let
  fun loop (i, accum) =
    if i > n then accum
    else loop (i + 1, accum * i)
in
  loop (1, 1)
end
```



## Stata

Mata has the built-in '''factorial''' function. Here are two implementations.


```stata
mata
real scalar function fact1(real scalar n) {
	if (n<2) return(1)
	else return(fact1(n-1)*n)
}

real scalar function fact2(real scalar n) {
	a=1
	for (i=2;i<=n;i++) a=a*i
	return(a)
}

printf("%f\n",fact1(8))
printf("%f\n",fact2(8))
printf("%f\n",factorial(8))
```


## Swift


### Iterative


```Swift
func factorial(_ n: Int) -> Int {
	return n < 2 ? 1 : (2...n).reduce(1, *)
}
```



### Recursive


```Swift
func factorial(_ n: Int) -> Int {
	return n < 2 ? 1 : n * factorial(n - 1)
}
```



## Tcl

Use Tcl 8.5 for its built-in arbitrary precision integer support.

### Iterative


```tcl
proc ifact n {
    for {set i $n; set sum 1} {$i >= 2} {incr i -1} {
        set sum [expr {$sum * $i}]
    }
    return $sum
}
```


### Recursive


```tcl
proc rfact n {
    expr {$n < 2 ? 1 : $n * [rfact [incr n -1]]}
}
```

The recursive version is limited by the default stack size to roughly 850!

When put into the ''tcl::mathfunc'' namespace, the recursive call stays inside the ''expr'' language, and thus looks clearer:

```Tcl
proc tcl::mathfunc::fact n {expr {$n < 2? 1: $n*fact($n-1)}}
```


### Iterative with caching


```tcl
proc ifact_caching n {
    global fact_cache
    if { ! [info exists fact_cache]} {
        set fact_cache {1 1}
    }
    if {$n < [llength $fact_cache]} {
        return [lindex $fact_cache $n]
    }
    set i [expr {[llength $fact_cache] - 1}]
    set sum [lindex $fact_cache $i]
    while {$i < $n} {
        incr i
        set sum [expr {$sum * $i}]
        lappend fact_cache $sum
    }
    return $sum
}
```


### Performance Analysis


```tcl
puts [ifact 30]
puts [rfact 30]
puts [ifact_caching 30]

set n 400
set iterations 10000
puts "calculate $n factorial $iterations times"
puts "ifact: [time {ifact $n} $iterations]"
puts "rfact: [time {rfact $n} $iterations]"
# for the caching proc, reset the cache between each iteration so as not to skew the results
puts "ifact_caching: [time {ifact_caching $n; unset -nocomplain fact_cache} $iterations]"
```

```txt
265252859812191058636308480000000
265252859812191058636308480000000
265252859812191058636308480000000
calculate 400 factorial 10000 times
ifact: 661.4324 microseconds per iteration
rfact: 654.7593 microseconds per iteration
ifact_caching: 613.1989 microseconds per iteration
```


===Using the &Gamma; Function===
Note that this only works correctly for factorials that produce correct representations in double precision floating-point numbers.
```tcl
package require math::special

proc gfact n {
    expr {round([::math::special::Gamma [expr {$n+1}]])}
}
```


=={{header|TI-83 BASIC}}==
TI-83 BASIC has a built-in factorial operator: x! is the factorial of x.
An other way is to use a combination of prod() and seq() functions:

```ti89b
10→N
N! 			---> 362880
prod(seq(I,I,1,N)) 	---> 362880
```

Note: maximum integer value is:
 13!                     ---> 6227020800

=={{header|TI-89 BASIC}}==
TI-89 BASIC also has the factorial function built in: x! is the factorial of x.

```ti89b
factorial(x)
Func
  Return Π(y,y,1,x)
EndFunc
```


Π is the standard product operator: <math>\overbrace{\Pi(\mathrm{expr},i,a,b)}^{\mathrm{TI-89}} = \overbrace{\prod_{i=a}^b \mathrm{expr}}^{\text{Math notation}}</math>


## TorqueScript


###  Iterative


```Torque
function Factorial(%num)
{
    if(%num < 2)
        return 1;
    for(%a = %num-1; %a > 1; %a--)
        %num *= %a;
    return %num;
}
```



###  Recursive


```Torque
function Factorial(%num)
{
    if(%num < 2)
        return 1;
    return %num * Factorial(%num-1);
}
```



## TransFORTH


```forth
: FACTORIAL
1 SWAP
1 + 1 DO
I * LOOP ;
```



## TUSCRIPT


```tuscript
$$ MODE TUSCRIPT
LOOP num=-1,12
 IF (num==0,1) THEN
  f=1
 ELSEIF (num<0) THEN
  PRINT num," is negative number"
  CYCLE
 ELSE
  f=VALUE(num)
  LOOP n=#num,2,-1
   f=f*(n-1)
  ENDLOOP
 ENDIF
formatnum=CENTER(num,+2," ")
PRINT "factorial of ",formatnum," = ",f
ENDLOOP
```

<pre style='height:30ex;overflow:scroll'>
-1 is negative number
factorial of  0 = 1
factorial of  1 = 1
factorial of  2 = 2
factorial of  3 = 6
factorial of  4 = 24
factorial of  5 = 120
factorial of  6 = 720
factorial of  7 = 5040
factorial of  8 = 40320
factorial of  9 = 362880
factorial of 10 = 3628800
factorial of 11 = 39916800
factorial of 12 = 479001600

```



## TXR


===Built-in===

Via nPk function:


```sh
$ txr -p '(n-perm-k 10 10)'
3628800
```



### Functional



```sh
$ txr -p '[reduce-left * (range 1 10) 1]'
3628800
```



## UNIX Shell


###  Iterative

```bash
factorial() {
  set -- "$1" 1
  until test "$1" -lt 2; do
    set -- "`expr "$1" - 1`" "`expr "$2" \* "$1"`"
  done
  echo "$2"
}
```


If <code>expr</code> uses 32-bit signed integers, then this function overflows after <code>factorial 12</code>.

Or in [[Korn Shell|Korn style]]:
```bash
function factorial {
  typeset n=$1 f=1 i
  for ((i=2; i < n; i++)); do
    (( f *= i ))
  done
  echo $f
}
```


* bash and zsh use 64-bit signed integers, overflows after <code>factorial 20</code>.
* ksh93 uses floating-point numbers, prints <code>factorial 19</code> as an integer, prints <code>factorial 20</code> in floating-point exponential format.

###  Recursive

These solutions fork many processes, because each level of recursion spawns a subshell to capture the output.
```bash
factorial ()
{
  if [ $1 -eq 0 ]
    then echo 1
    else echo $(($1 * $(factorial $(($1-1)) ) ))
  fi
}
```


Or in [[Korn Shell|Korn style]]:
```bash
function factorial {
  typeset n=$1
  (( n < 2 )) && echo 1 && return
  echo $(( n * $(factorial $((n-1))) ))
}
```

=
## C Shell
=
This is an iterative solution. ''csh'' uses 32-bit signed integers, so this alias overflows after <code>factorial 12</code>.

```csh
alias factorial eval \''set factorial_args=( \!*:q )	\\
	@ factorial_n = $factorial_args[2]		\\
	@ factorial_i = 1				\\
	while ( $factorial_n >= 2 )			\\
		@ factorial_i *= $factorial_n		\\
		@ factorial_n -= 1			\\
	end						\\
	@ $factorial_args[1] = $factorial_i		\\
'\'

factorial f 12
echo $f
# => 479001600
```



## Ursa

### Iterative


```ursa
def factorial (int n)
	decl int result
	set result 1
	decl int i
	for (set i 1) (< i (+ n 1)) (inc i)
		set result (* result i)
	end
	return result
end

```


### Recursive


```ursa
def factorial (int n)
      decl int z
      set z 1
      if (> n 1)
              set z (* n (factorial (- n 1)))
      end if
      return z
end
```



## Ursala

There is already a library function for factorials, but they can be defined anyway like this. The good method treats natural numbers as an abstract type, and the better method factors out powers of 2 by bit twiddling.

```Ursala
#import nat

good_factorial   = ~&?\1! product:-1^lrtPC/~& iota
better_factorial = ~&?\1! ^T(~&lSL,@rS product:-1)+ ~&Z-~^*lrtPC/~& iota
```

test program:

```Ursala
#cast %nL

test = better_factorial* <0,1,2,3,4,5,6,7,8>
```

```txt
<1,1,2,6,24,120,720,5040,40320>
```



## VBA


```vb
Public Function factorial(n As Integer) As Long
    factorial = WorksheetFunction.Fact(n)
End Function
```


## Verbexx


```verbexx
// ----------------
// recursive method  (requires INTV_T input parm)
// ----------------

fact_r @FN [n]
{
    @CASE
      when:(n <  0iv) {-1iv                 }
      when:(n == 0iv) { 1iv                 }
      else:           { n * (@fact_r n-1iv) }
};


// ----------------
// iterative method  (requires INTV_T input parm)
// ----------------

fact_i @FN [n]
{
    @CASE
      when:(n <  0iv) {-1iv }
      when:(n == 0iv) { 1iv }
      else:           {
                        @VAR i fact = 1iv 1iv;
                        @LOOP while:(i <= n) { fact *= i++ };
                      }
};


// ------------------
// Display factorials
// ------------------

@VAR i = -1iv;
@LOOP times:15
{
     @SAY «recursive  » i «! = » (@fact_r i) between:"";
     @SAY «iterative  » i «! = » (@fact_i i) between:"";

     i = 5iv * i / 4iv + 1iv;
};


/]
### ===================================================================================


Output:

recursive  -1! = -1
iterative  -1! = -1
recursive  0! = 1
iterative  0! = 1
recursive  1! = 1
iterative  1! = 1
recursive  2! = 2
iterative  2! = 2
recursive  3! = 6
iterative  3! = 6
recursive  4! = 24
iterative  4! = 24
recursive  6! = 720
iterative  6! = 720
recursive  8! = 40320
iterative  8! = 40320
recursive  11! = 39916800
iterative  11! = 39916800
recursive  14! = 87178291200
iterative  14! = 87178291200
recursive  18! = 6402373705728000
iterative  18! = 6402373705728000
recursive  23! = 25852016738884976640000
iterative  23! = 25852016738884976640000
recursive  29! = 8841761993739701954543616000000
iterative  29! = 8841761993739701954543616000000
recursive  37! = 13763753091226345046315979581580902400000000
iterative  37! = 13763753091226345046315979581580902400000000
recursive  47! = 258623241511168180642964355153611979969197632389120000000000
iterative  47! = 258623241511168180642964355153611979969197632389120000000000
```



## Vim Script


```vim
function! Factorial(n)
  if a:n < 2
    return 1
  else
    return a:n * Factorial(a:n-1)
  endif
endfunction
```



## VBA

For numbers < 170 only

```vb
Option Explicit

Sub Main()
Dim i As Integer
For i = 1 To 17
    Debug.Print "Factorial " & i & " , recursive : " & FactRec(i) & ", iterative : " & FactIter(i)
Next
Debug.Print "Factorial 120, recursive : " & FactRec(120) & ", iterative : " & FactIter(120)
End Sub

Private Function FactRec(Nb As Integer) As String
If Nb > 170 Or Nb < 0 Then FactRec = 0: Exit Function
    If Nb = 1 Or Nb = 0 Then
        FactRec = 1
    Else
        FactRec = Nb * FactRec(Nb - 1)
    End If
End Function

Private Function FactIter(Nb As Integer)
If Nb > 170 Or Nb < 0 Then FactIter = 0: Exit Function
Dim i As Integer, F
    F = 1
    For i = 1 To Nb
        F = F * i
    Next i
    FactIter = F
End Function
```

```txt
Factorial 1 , recursive : 1, iterative : 1
Factorial 2 , recursive : 2, iterative : 2
Factorial 3 , recursive : 6, iterative : 6
Factorial 4 , recursive : 24, iterative : 24
Factorial 5 , recursive : 120, iterative : 120
Factorial 6 , recursive : 720, iterative : 720
Factorial 7 , recursive : 5040, iterative : 5040
Factorial 8 , recursive : 40320, iterative : 40320
Factorial 9 , recursive : 362880, iterative : 362880
Factorial 10 , recursive : 3628800, iterative : 3628800
Factorial 11 , recursive : 39916800, iterative : 39916800
Factorial 12 , recursive : 479001600, iterative : 479001600
Factorial 13 , recursive : 6227020800, iterative : 6227020800
Factorial 14 , recursive : 87178291200, iterative : 87178291200
Factorial 15 , recursive : 1307674368000, iterative : 1307674368000
Factorial 16 , recursive : 20922789888000, iterative : 20922789888000
Factorial 17 , recursive : 355687428096000, iterative : 355687428096000
Factorial 120, recursive : 6,68950291344919E+198, iterative : 6,68950291344912E+198
```



## VBScript

Optimized with memoization, works for numbers up to 170 (because of the limitations on Doubles), exits if -1 is input

```vb
Dim lookupTable(170), returnTable(170), currentPosition, input
currentPosition = 0

Do While True
	input = InputBox("Please type a number (-1 to quit):")
	MsgBox "The factorial of " & input & " is " & factorial(CDbl(input))
Loop

Function factorial (x)
	If x = -1 Then
		WScript.Quit 0
	End If
	Dim temp
	temp = lookup(x)
	If x <= 1 Then
		factorial = 1
	ElseIf temp <> 0 Then
		factorial = temp
	Else
		temp = factorial(x - 1) * x
		store x, temp
		factorial = temp
	End If
End Function

Function lookup (x)
	Dim i
	For i = 0 To currentPosition - 1
		If lookupTable(i) = x Then
			lookup = returnTable(i)
			Exit Function
		End If
	Next
	lookup = 0
End Function

Function store (x, y)
	lookupTable(currentPosition) = x
	returnTable(currentPosition) = y
	currentPosition = currentPosition + 1
End Function
```



## VHDL


```VHDL
LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;

ENTITY Factorial IS
	GENERIC (
			Nbin : INTEGER := 3 ; -- number of bit to input number
			Nbou : INTEGER := 13) ; -- number of bit to output factorial

	PORT (
		clk : IN STD_LOGIC ; -- clock of circuit
		sr  : IN STD_LOGIC_VECTOR(1 DOWNTO 0); -- set and reset
		N   : IN STD_LOGIC_VECTOR(Nbin-1 DOWNTO 0) ; -- max number
	    Fn  : OUT STD_LOGIC_VECTOR(Nbou-1 DOWNTO 0)); -- factorial of "n"

END Factorial ;

ARCHITECTURE Behavior OF Factorial IS
---------------------- Program Multiplication --------------------------------
	FUNCTION Mult ( CONSTANT MFa : IN UNSIGNED ;
					CONSTANT MI   : IN UNSIGNED ) RETURN UNSIGNED IS
	VARIABLE Z : UNSIGNED(MFa'RANGE) ;
	VARIABLE U : UNSIGNED(MI'RANGE) ;
	BEGIN
	Z := TO_UNSIGNED(0, MFa'LENGTH) ; -- to obtain the multiplication
	U := MI ; -- regressive counter
		LOOP
			Z := Z + MFa ; -- make multiplication
			U := U - 1 ;
			EXIT WHEN U = 0 ;
		END LOOP ;
		RETURN Z ;
	END Mult ;
-------------------Program Factorial ---------------------------------------
	FUNCTION Fact (CONSTANT Nx : IN NATURAL ) RETURN UNSIGNED IS
	VARIABLE C  : NATURAL RANGE 0 TO 2**Nbin-1 ;
	VARIABLE I  : UNSIGNED(Nbin-1 DOWNTO 0) ;
	VARIABLE Fa : UNSIGNED(Nbou-1 DOWNTO 0) ;
	BEGIN
		C := 0 ; -- counter
		I :=  TO_UNSIGNED(1, Nbin) ;
		Fa := TO_UNSIGNED(1, Nbou) ;
		LOOP
			EXIT WHEN C = Nx ; -- end loop
			C := C + 1 ;  -- progressive couter
			Fa := Mult (Fa , I ); -- call function to make a multiplication
			I := I + 1 ; --
		END LOOP ;
		RETURN Fa ;
	END Fact ;
--------------------- Program TO Call Factorial Function ------------------------------------------------------
	TYPE Table IS ARRAY (0 TO 2**Nbin-1) OF UNSIGNED(Nbou-1 DOWNTO 0) ;
	FUNCTION Call_Fact RETURN Table IS
	VARIABLE Fc : Table ;
	BEGIN
		FOR c IN 0 TO 2**Nbin-1 LOOP
			Fc(c) := Fact(c) ;
		END LOOP ;
		RETURN Fc ;
	END FUNCTION Call_Fact;

	CONSTANT Result : Table := Call_Fact ;
 ------------------------------------------------------------------------------------------------------------
SIGNAL Nin : STD_LOGIC_VECTOR(N'RANGE) ;
BEGIN    -- start of architecture


Nin <= N               WHEN RISING_EDGE(clk) AND sr = "10" ELSE
       (OTHERS => '0') WHEN RISING_EDGE(clk) AND sr = "01" ELSE
	   UNAFFECTED;

Fn <= STD_LOGIC_VECTOR(Result(TO_INTEGER(UNSIGNED(Nin)))) WHEN RISING_EDGE(clk) ;

END Behavior ;
```



## Visual Basic

```vb

Option Explicit

Sub Main()
    Dim i As Variant
    For i = 1 To 27
        Debug.Print "Factorial(" & i & ")= , recursive : " & Format$(FactRec(i), "#,###") & " - iterative : " & Format$(FactIter(i), "#,####")
    Next
End Sub 'Main

Private Function FactRec(n As Variant) As Variant
    n = CDec(n)
    If n = 1 Then
        FactRec = 1#
    Else
        FactRec = n * FactRec(n - 1)
    End If
End Function 'FactRec

Private Function FactIter(n As Variant)
    Dim i As Variant, f As Variant
    f = 1#
    For i = 1# To CDec(n)
        f = f * i
    Next i
    FactIter = f
End Function 'FactIter

```

<pre style="height:30ex;overflow:scroll">
Factorial(1)= , recursive : 1 - iterative : 1
Factorial(2)= , recursive : 2 - iterative : 2
Factorial(3)= , recursive : 6 - iterative : 6
Factorial(4)= , recursive : 24 - iterative : 24
Factorial(5)= , recursive : 120 - iterative : 120
Factorial(6)= , recursive : 720 - iterative : 720
Factorial(7)= , recursive : 5,040 - iterative : 5,040
Factorial(8)= , recursive : 40,320 - iterative : 40,320
Factorial(9)= , recursive : 362,880 - iterative : 362,880
Factorial(10)= , recursive : 3,628,800 - iterative : 3,628,800
Factorial(11)= , recursive : 39,916,800 - iterative : 39,916,800
Factorial(12)= , recursive : 479,001,600 - iterative : 479,001,600
Factorial(13)= , recursive : 6,227,020,800 - iterative : 6,227,020,800
Factorial(14)= , recursive : 87,178,291,200 - iterative : 87,178,291,200
Factorial(15)= , recursive : 1,307,674,368,000 - iterative : 1,307,674,368,000
Factorial(16)= , recursive : 20,922,789,888,000 - iterative : 20,922,789,888,000
Factorial(17)= , recursive : 355,687,428,096,000 - iterative : 355,687,428,096,000
Factorial(18)= , recursive : 6,402,373,705,728,000 - iterative : 6,402,373,705,728,000
Factorial(19)= , recursive : 121,645,100,408,832,000 - iterative : 121,645,100,408,832,000
Factorial(20)= , recursive : 2,432,902,008,176,640,000 - iterative : 2,432,902,008,176,640,000
Factorial(21)= , recursive : 51,090,942,171,709,440,000 - iterative : 51,090,942,171,709,440,000
Factorial(22)= , recursive : 1,124,000,727,777,607,680,000 - iterative : 1,124,000,727,777,607,680,000
Factorial(23)= , recursive : 25,852,016,738,884,976,640,000 - iterative : 25,852,016,738,884,976,640,000
Factorial(24)= , recursive : 620,448,401,733,239,439,360,000 - iterative : 620,448,401,733,239,439,360,000
Factorial(25)= , recursive : 15,511,210,043,330,985,984,000,000 - iterative : 15,511,210,043,330,985,984,000,000
Factorial(26)= , recursive : 403,291,461,126,605,635,584,000,000 - iterative : 403,291,461,126,605,635,584,000,000
Factorial(27)= , recursive : 10,888,869,450,418,352,160,768,000,000 - iterative : 10,888,869,450,418,352,160,768,000,000



```



## Visual Basic .NET

Various type implementations follow.  No error checking, so don't try to evaluate a number less than zero, or too large of a number.

```vbnet
Imports System
Imports System.Numerics
Imports System.Linq

Module Module1

    ' Type Double:

    Function DofactorialI(n As Integer) As Double ' Iterative
        DofactorialI = 1 : For i As Integer = 1 To n : DofactorialI *= i : Next
    End Function

    ' Type Unsigned Long:

    Function ULfactorialI(n As Integer) As ULong ' Iterative
        ULfactorialI = 1 : For i As Integer = 1 To n : ULfactorialI *= i : Next
    End Function

    ' Type Decimal:

    Function DefactorialI(n As Integer) As Decimal ' Iterative
        DefactorialI = 1 : For i As Integer = 1 To n : DefactorialI *= i : Next
    End Function

    ' Extends precision by "dehydrating" and "rehydrating" the powers of ten
    Function DxfactorialI(n As Integer) As String ' Iterative
        Dim factorial as Decimal = 1, zeros as integer = 0
        For i As Integer = 1 To n : factorial *= i
            If factorial Mod 10 = 0 Then factorial /= 10 : zeros += 1
        Next : Return factorial.ToString() & New String("0", zeros)
    End Function

    ' Arbitrary Precision:

    Function FactorialI(n As Integer) As BigInteger ' Iterative
        factorialI = 1 : For i As Integer = 1 To n : factorialI *= i : Next
    End Function

    Function Factorial(number As Integer) As BigInteger ' Functional
        Return Enumerable.Range(1, number).Aggregate(New BigInteger(1),
            Function(acc, num) acc * num)
    End Function

    Sub Main()
        Console.WriteLine("Double  : {0}! = {1:0}", 20, DoFactorialI(20))
        Console.WriteLine("ULong   : {0}! = {1:0}", 20, ULFactorialI(20))
        Console.WriteLine("Decimal : {0}! = {1:0}", 27, DeFactorialI(27))
        Console.WriteLine("Dec.Ext : {0}! = {1:0}", 32, DxFactorialI(32))
        Console.WriteLine("Arb.Prec: {0}! = {1}", 250, Factorial(250))
    End Sub
End Module
```

Note that the first four are the maximum possible for their type without causing a run-time error.

```txt
Double  : 20! = 2432902008176640000
ULong   : 20! = 2432902008176640000
Decimal : 27! = 10888869450418352160768000000
Dec.Ext : 32! = 263130836933693530167218012160000000
Arb.Prec: 250! = 3232856260909107732320814552024368470994843717673780666747942427112823747555111209488817915371028199450928507353189432926730931712808990822791030279071281921676527240189264733218041186261006832925365133678939089569935713530175040513178760077247933065402339006164825552248819436572586057399222641254832982204849137721776650641276858807153128978777672951913990844377478702589172973255150283241787320658188482062478582659808848825548800000000000000000000000000000000000000000000000000000000000000

```



## Vlang

Imperative solution:

```Vlang

const (
    MAX = 10
)

fn main() {
	mut facs := [1; MAX+1]
	facs[0] = 1
  println('The 0-th Factorial number is: 1')

	for i:= 1; i <= MAX; i++ {
		facs[i] = i * facs[i-1]
    num := facs[i]
    println('The $i-th Factorial number is: $num')
	}
}

```


Recursive solution:

```Vlang

const (
    MAX = 10
)

fn main() {
	for i := 0; i <= MAX; i++ {
		println('factorial($i) is: ${fac(i)}')
	}
}

fn fac(n int) int {
	if n == 0 {
		return 1
	}
	return n * fac(n - 1)
}

```


Memoized solution:

```Vlang

const (
    MAX = 10
)

struct Cache {
  mut:
    values []int
}

fn fac_cached(n int, cache mut Cache) int {
  is_in_cache := cache.values.len > n
  if is_in_cache {
    return cache.values[n]
  }

  fac_n := if n == 0 {
    1
  } else {
    n * fac_cached(n - 1, mut cache)
  }

  cache.values << fac_n

  return fac_n
}

fn main() {
  mut c := Cache{}
  for n := 0; n <=  MAX; n++ {
    fac_n := fac_cached(n, mut c)
    println('The $n-th Factorial is: $fac_n')
  }
}

```



## Wart


===Recursive, all at once===

```python
def (fact n)
  if (n = 0)
    1
    (n * (fact n-1))
```


===Recursive, using cases and pattern matching===

```python
def (fact n)
  (n * (fact n-1))

def (fact 0)
  1
```


===Iterative, with an explicit loop===

```python
def (fact n)
  ret result 1
    for i 1 (i <= n) ++i
      result <- result*i
```


===Iterative, with a pseudo-generator===

```python
# a useful helper to generate all the natural numbers until n
def (nums n)
  collect+for i 1 (i <= n) ++i
    yield i

def (fact n)
  (reduce (*) nums.n 1)
```



## WDTE



### Recursive


```WDTE>let max a b =
 a { < b => b };

let ! n => n { > 1 => - n 1 -> ! -> * n } -> max 1;
```



### Iterative


```WDTE>let s =
 import 'stream';

let ! n => s.range 1 (+ n 1) -> s.reduce 1 *;
```



## Wortel

Operator:

```wortel>@fac 10</lang

Number expression:

```wortel
!#~F 10
```

Folding:

```wortel
!/^* @to 10
; or
@prod @to 10
```

Iterative:

```wortel
~!10 &n [
  @var r 1
  @for x to n
    :!*r x
  r
]
```

Recursive:

```wortel
@let {
  fac &{fac n}?{
    <n 2 n
    *n !fac -n 1
  }

  ; memoized
  facM @mem &n?{
    <n 2 n
    *n !facM -n 1
  }

  [[!fac 10 !facM 10]]
}
```



## Wrapl


### Product


```wrapl
DEF fac(n) n <= 1 | PROD 1:to(n);
```


### Recursive


```wrapl
DEF fac(n) n <= 0 => 1 // n * fac(n - 1);
```


### Folding


```wrapl
DEF fac(n) n <= 1 | :"*":foldl(ALL 1:to(n));
```



## x86 Assembly

### Iterative


```asm
global factorial
section .text

; Input in ECX register (greater than 0!)
; Output in EAX register
factorial:
  mov   eax, 1
.factor:
  mul   ecx
  loop  .factor
  ret
```


### Recursive


```asm
global fact
section .text

; Input and output in EAX register
fact:
  cmp    eax, 1
  je    .done   ; if eax == 1 goto done

  ; inductive case
  push  eax  ; save n (ie. what EAX is)
  dec   eax  ; n - 1
  call  fact ; fact(n - 1)
  pop   ebx  ; fetch old n
  mul   ebx  ; multiplies EAX with EBX, ie. n * fac(n - 1)
  ret

.done:
  ; base case: return 1
  mov   eax, 1
  ret
```


### Tail Recursive


```asm
global factorial
section .text

; Input in ECX register
; Output in EAX register
factorial:
  mov   eax, 1  ; default argument, store 1 in accumulator

.base_case:
  test  ecx, ecx
  jnz   .inductive_case  ; return accumulator if n == 0
  ret

.inductive_case:
  mul   ecx         ; accumulator *= n
  dec   ecx         ; n -= 1
  jmp   .base_case  ; tail call
```



## XL


```XL
0! -> 1
N! -> N * (N-1)!
```



## XLISP


```lisp
(defun factorial (x)
	(if (< x 0)
		nil
		(if (<= x 1)
			1
			(* x (factorial (- x 1))) ) ) )
```



## XPL0


```XPL0
func FactIter(N);       \Factorial of N using iterative method
int N;                  \range: 0..12
int F, I;
[F:= 1;
for I:= 2 to N do F:= F*I;
return F;
];

func FactRecur(N);      \Factorial of N using recursive method
int N;                  \range: 0..12
return if N<2 then 1 else N*FactRecur(N-1);
```



## Yabasic


```Yabasic
// recursive
sub factorial(n)
    if n > 1 then return n * factorial(n - 1) else return 1 end if
end sub

//iterative
sub factorial2(n)
    local i, t

    t = 1
    for i = 1 to n
        t = t * i
    next
    return t
end sub

for n = 0 to 9
    print "Factorial(", n, ") = ", factorial(n)
next
```



## zkl


```zkl
fcn fact(n){[2..n].reduce('*,1)}
fcn factTail(n,N=1) {  // tail recursion
   if (n == 0) return(N);
   return(self.fcn(n-1,n*N));
}
```


```zkl
fact(6).println();
var BN=Import("zklBigNum");
factTail(BN(42)) : "%,d".fmt(_).println();  // built in as BN(42).factorial()
```

```txt

720
1,405,006,117,752,879,898,543,142,606,244,511,569,936,384,000,000,000

```

The [..] notation understands int, float and string but not big int so fact(BN) doesn't work but tail recursion is just a loop so the two versions are pretty much the same.


## ZX Spectrum Basic


### Iterative


```zxbasic
10 LET x=5: GO SUB 1000: PRINT "5! = ";r
999 STOP
1000 REM *************
1001 REM * FACTORIAL *
1002 REM *************
1010 LET r=1
1020 IF x<2 THEN RETURN
1030 FOR i=2 TO x: LET r=r*i: NEXT i
1040 RETURN
```

```txt

5! = 120

```



### Recursive

Using VAL for delayed evaluation and AND's ability to return given string or empty,
we can now control the program flow within an expression in a manner akin to LISP's cond:

```zxbasic
DEF FN f(n) = VAL (("1" AND n<=0) + ("n*FN f(n-1)" AND n>0))
```

But, truth be told, the parameter n does not withstand recursive calling.
Changing the order of the product gives naught:

```zxbasic
DEF FN f(n) = VAL (("1" AND n<=0) + ("FN f(n-1)*n" AND n>0))
```

