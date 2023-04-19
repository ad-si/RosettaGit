+++
title = "Factors of an integer"
description = ""
date = 2019-10-15T00:04:09Z
aliases = []
[extra]
id = 4737
[taxonomies]
categories = []
tags = []
+++

{{Task|Basic language learning}}
{{basic data operation}}
[[Category:Arithmetic operations]]
[[Category:Mathematical_operations]]
[[Category:Prime Numbers]]

;Task:
Compute the   [[wp:Divisor|factors]]   of a positive integer.

These factors are the positive integers by which the number being factored can be divided to yield a positive integer result.

(Though the concepts function correctly for zero and negative integers, the set of factors of zero has countably infinite members, and the factors of negative integers can be obtained from the factors of related positive numbers without difficulty;   this task does not require handling of either of these cases).

Note that every prime number has two factors:   '''1'''   and itself.


;Related tasks:
*   [[count in factors]]
*   [[prime decomposition]]
*   [[Sieve of Eratosthenes]]
*   [[primality by trial division]]
*   [[factors of a Mersenne number]]
*   [[trial factoring of a Mersenne number]]
*   [[partition an integer X into N primes]]
*   [[sequence of primes by Trial Division]]






## 0815


```0815

<:1:~>|~#:end:>~x}:str:/={^:wei:~%x<:a:x=$~
=}:wei:x<:1:+{>~>x=-#:fin:^:str:}:fin:{{~%

```



## 360 Assembly

Very compact version.

```360asm
*        Factors of an integer -   07/10/2015
FACTOR   CSECT
         USING  FACTOR,R15         set base register
         LA     R7,PG              pgi=@pg
         LA     R6,1               i
         L      R3,N               loop count
LOOP     L      R5,N               n
         LA     R4,0
         DR     R4,R6              n/i
         LTR    R4,R4              if mod(n,i)=0
         BNZ    NEXT
         XDECO  R6,PG+120          edit i
         MVC    0(6,R7),PG+126     output i
         LA     R7,6(R7)           pgi=pgi+6
NEXT     LA     R6,1(R6)           i=i+1
         BCT    R3,LOOP            loop
         XPRNT  PG,120             print buffer
         XR     R15,R15            set return code
         BR     R14                return to caller
N        DC     F'12345'           <== input value
PG       DC     CL132' '           buffer
         YREGS
         END    FACTOR
```

{{out}}

```txt

     1     3     5    15   823  2469  4115 12345

```



## ACL2


```Lisp
(defun factors-r (n i)
   (declare (xargs :measure (nfix (- n i))))
   (cond ((zp (- n i))
          (list n))
         ((= (mod n i) 0)
          (cons i (factors-r n (1+ i))))
         (t (factors-r n (1+ i)))))

(defun factors (n)
   (factors-r n 1))
```



## ActionScript


```ActionScript
function factor(n:uint):Vector.<uint>
{
	var factors:Vector.<uint> = new Vector.<uint>();
	for(var i:uint = 1; i <= n; i++)
		if(n % i == 0)factors.push(i);
	return factors;
}
```



## Ada


```Ada
with Ada.Text_IO;
with Ada.Command_Line;
procedure Factors is
   Number  : Positive;
   Test_Nr : Positive := 1;
begin
   if Ada.Command_Line.Argument_Count /= 1 then
      Ada.Text_IO.Put (Ada.Text_IO.Standard_Error, "Missing argument!");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      return;
   end if;
   Number := Positive'Value (Ada.Command_Line.Argument (1));
   Ada.Text_IO.Put ("Factors of" & Positive'Image (Number) & ": ");
   loop
      if Number mod Test_Nr = 0 then
         Ada.Text_IO.Put (Positive'Image (Test_Nr) & ",");
      end if;
      exit when Test_Nr ** 2 >= Number;
      Test_Nr := Test_Nr + 1;
   end loop;
   Ada.Text_IO.Put_Line (Positive'Image (Number) & ".");
end Factors;
```



## Aikido


```aikido
import math

function factor (n:int) {
    var result = []
    function append (v) {
        if (!(v in result)) {
            result.append (v)
        }
    }
    var sqrt = cast<int>(Math.sqrt (n))
    append (1)
    for (var i = n-1 ; i >= sqrt ; i--) {
        if ((n % i) == 0) {
            append (i)
            append (n/i)
        }
    }
    append (n)
    return result.sort()
}

function printvec (vec) {
    var comma = ""
    print ("[")
    foreach v vec {
        print (comma + v)
        comma = ", "
    }
    println ("]")
}

printvec (factor (45))
printvec (factor (25))
printvec (factor (100))
```



## ALGOL 68

{{works with|ALGOL 68|Revision 1 - no extensions to language used}}

{{works with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-1.18.0/algol68g-1.18.0-9h.tiny.el5.centos.fc11.i386.rpm/download 1.18.0-9h.tiny]}}

{{works with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8-8d]}}

Note: The following implements generators, eliminating the need of declaring arbitrarily long '''int''' arrays for caching.

```algol68
MODE YIELDINT = PROC(INT)VOID;

PROC gen factors = (INT n, YIELDINT yield)VOID: (
  FOR i FROM 1 TO ENTIER sqrt(n) DO
    IF n MOD i = 0 THEN
      yield(i);
      INT other = n OVER i;
      IF i NE other THEN yield(n OVER i) FI
    FI
  OD
);

[]INT nums2factor = (45, 53, 64);

FOR i TO UPB nums2factor DO
  INT num = nums2factor[i];
  STRING sep := ": ";
  print(num);
# FOR INT j IN # gen factors(num, # ) DO ( #
##   (INT j)VOID:(
       print((sep,whole(j,0)));
       sep:=", "
# OD # ));
  print(new line)
OD
```

{{out}}

```txt

        +45: 1, 45, 3, 15, 5, 9
        +53: 1, 53
        +64: 1, 64, 2, 32, 4, 16, 8

```


=={{header|ALGOL-M}}==
Instead of displaying 1 and the number itself as factors, prime numbers are explicitly reported as such. To reduce the number of test divisions, only odd divisors are tested if an initial check shows the number to be factored is not even. The upper limit of divisors is set at N/2 or N/3, depending on whether N is even or odd, and is continuously reduced to N divided by the next potential divisor until the first factor is found. For a prime number the resulting limit will be the square root of N, which avoids the necessity of explicitly calculating that value. (ALGOL-M does not have a built-in square root function.)

```algol

BEGIN

COMMENT ALGOL-M PROGRAM TO DISPLAY THE FACTORS OF AN INTEGER

INTEGER I, N, LIMIT, FOUND, START, DELTA;
STRING(1) ANOTHER;

COMMENT COMPUTE P MOD Q;
INTEGER FUNCTION MOD (P, Q);
INTEGER P, Q;
BEGIN
    MOD := P - Q * (P / Q);
END;

COMMENT MAIN PROGRAM BEGINS HERE;
ANOTHER := "Y";
WHILE ANOTHER = "Y" OR ANOTHER = "y" DO
  BEGIN
    WRITE ("Number to factor:");
    READ (N);
    WRITE ("The factors are:");

    COMMENT CHECK WHETHER NUMBER IS EVEN OR ODD;
    IF MOD(N, 2) = 0 THEN
      BEGIN
        START := 2;
        DELTA := 1;
      END
    ELSE
      BEGIN
        START := 3;
        DELTA := 2;
      END;

    COMMENT TEST POTENTIAL DIVISORS;
    FOUND := 0;
    I := START;
    LIMIT := N / START;
    WHILE I <= LIMIT DO
      BEGIN
        IF MOD(N, I) = 0 THEN
          BEGIN
            WRITE (I);
            FOUND := FOUND + 1;
          END;
        I := I + DELTA;
        IF FOUND = 0 THEN LIMIT := N / I;
      END;

    IF FOUND = 0 THEN WRITE ("None - the number is prime.");
    WRITE(" ");
    WRITE("Do another (y/n)?");
    READ (ANOTHER);
  END;
WRITE (" ");
WRITE ("Goodbye");
END

```



## ALGOL W


```algolw
begin
    % return the factors of n ( n should be >= 1 ) in the array factor       %
    % the bounds of factor should be 0 :: len (len must be at least 1)       %
    % the number of factors will be returned in factor( 0 )                  %
    procedure getFactorsOf ( integer value n
                           ; integer array factor( * )
                           ; integer value len
                           ) ;
    begin
        for i := 0 until len do factor( i ) := 0;
        if n >= 1 and len >= 1 then begin
            integer pos, lastFactor;
            factor( 0 ) := factor( 1 ) := pos := 1;
            % find the factors up to sqrt( n )                               %
            for f := 2 until truncate( sqrt( n ) ) + 1 do begin
                if ( n rem f ) = 0 and pos <= len then begin
                    % found another factor and there's room to store it      %
                    pos           := pos + 1;
                    factor( 0   ) := pos;
                    factor( pos ) := f
                end if_found_factor
            end for_f;
            % find the factors above sqrt( n )                               %
            lastFactor := factor( factor( 0 ) );
            for f := factor( 0 ) step -1 until 1 do begin
                integer newFactor;
                newFactor := n div factor( f );
                if newFactor > lastFactor and pos <= len then begin
                    % found another factor and there's room to store it      %
                    pos           := pos + 1;
                    factor( 0   ) := pos;
                    factor( pos ) := newFactor
                end if_found_factor
            end for_f;
        end if_params_ok
    end getFactorsOf ;


    % prpocedure to test getFactorsOf                                        %
    procedure testFactorsOf( integer value n ) ;
    begin
        integer array factor( 0 :: 100 );
        getFactorsOf( n, factor, 100 );
        i_w := 1; s_w := 0; % set output format                              %
        write( n, " has ", factor( 0 ), " factors:" );
        for f := 1 until factor( 0 ) do writeon( " ", factor( f ) )
    end testFactorsOf ;

    % test the factorising                                                   %
    for i := 1 until 100 do testFactorsOf( i )

end.
```

{{out}}

```txt

1 has 1 factors: 1
2 has 2 factors: 1 2
3 has 2 factors: 1 3
4 has 3 factors: 1 2 4
...
96 has 12 factors: 1 2 3 4 6 8 12 16 24 32 48 96
97 has 2 factors: 1 97
98 has 6 factors: 1 2 7 14 49 98
99 has 6 factors: 1 3 9 11 33 99
100 has 9 factors: 1 2 4 5 10 20 25 50 100

```



## APL


```APL
      factors←{(0=(⍳⍵)|⍵)/⍳⍵}
      factors 12345
1 3 5 15 823 2469 4115 12345
      factors 720
1 2 3 4 5 6 8 9 10 12 15 16 18 20 24 30 36 40 45 48 60 72 80 90 120 144 180 240 360 720
```





## AppleScript

{{Trans|JavaScript}}

```AppleScript
-- integerFactors :: Int -> [Int]
on integerFactors(n)
    if n = 1 then
        {1}
    else
        set realRoot to n ^ (1 / 2)
        set intRoot to realRoot as integer
        set blnPerfectSquare to intRoot = realRoot

        -- isFactor :: Int -> Bool
        script isFactor
            on |λ|(x)
                (n mod x) = 0
            end |λ|
        end script

        -- Factors up to square root of n,
        set lows to filter(isFactor, enumFromTo(1, intRoot))

        -- integerQuotient :: Int -> Int
        script integerQuotient
            on |λ|(x)
                (n / x) as integer
            end |λ|
        end script

        -- and quotients of these factors beyond the square root.
        lows & map(integerQuotient, ¬
            items (1 + (blnPerfectSquare as integer)) thru -1 of reverse of lows)
    end if
end integerFactors

-- TEST ------------------------------------------------------------------------
on run

    integerFactors(120)

    --> {1, 2, 3, 4, 5, 6, 8, 10, 12, 15, 20, 24, 30, 40, 60, 120}
end run


-- GENERIC FUNCTIONS -----------------------------------------------------------

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

-- filter :: (a -> Bool) -> [a] -> [a]
on filter(f, xs)
    tell mReturn(f)
        set lst to {}
        set lng to length of xs
        repeat with i from 1 to lng
            set v to item i of xs
            if |λ|(v, i, xs) then set end of lst to v
        end repeat
        return lst
    end tell
end filter

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

{{Out}}

```AppleScript
{1, 2, 3, 4, 5, 6, 8, 10, 12, 15, 20, 24, 30, 40, 60, 120}
```



## Arc


```Arc

(= divisor (fn (num)
   (= dlist '())
   (when (is 1 num) (= dlist '(1 0)))
   (when (is 2 num) (= dlist '(2 1)))
   (unless (or (is 1 num) (is 2 num))
   (up i 1 (+ 1 (/ num 2))
     (if (is 0 (mod num i))
         (push i dlist)))
   (= dlist (cons num dlist)))
   dlist))

(map [rev _] (map [divisor _] '(45 53 60 64)))

```


{{Out}}

```Arc

'(
(1 3 5 9 15 45)
(1 53)
(1 2 3 4 5 6 10 12 15 20 30 60)
(1 2 4 8 16 32 64)
)

```




## ARM Assembly

{{works with|as|Raspberry Pi}}

```ARM Assembly

/* ARM assembly Raspberry PI  */
/*  program factorst.s   */

/* Constantes    */
.equ STDOUT, 1     @ Linux output console
.equ EXIT,   1     @ Linux syscall
.equ WRITE,  4     @ Linux syscall
/* Initialized data */
.data
szMessDeb: .ascii "Factors of :"
sMessValeur:   .fill 12, 1, ' '
                   .asciz "are : \n"
sMessFactor:   .fill 12, 1, ' '
                   .asciz "\n"
szCarriageReturn:  .asciz "\n"

/* UnInitialized data */
.bss

/*  code section */
.text
.global main
main:                /* entry of program  */
    push {fp,lr}    /* saves 2 registers */

    mov r0,#100
    bl factors
    mov r0,#97
    bl factors
    ldr r0,iNumber
    bl factors


100:   /* standard end of the program */
    mov r0, #0                  @ return code
    pop {fp,lr}                 @restaur 2 registers
    mov r7, #EXIT              @ request to exit program
    swi 0                       @ perform the system call

iNumber: .int 32767
iAdrszCarriageReturn:  .int  szCarriageReturn
/******************************************************************/
/*     calcul factors of number                                  */
/******************************************************************/
/* r0 contains the number */
factors:
    push {fp,lr}    			/* save  registres */
    push {r1-r6}    		/* save others registers */
    mov r5,r0    @ limit calcul
    ldr r1,iAdrsMessValeur   @ conversion register in decimal string
    bl conversion10S
    ldr r0,iAdrszMessDeb     @ display message
    bl affichageMess
    mov r6,#1    @ counter loop
1:   @ loop
    mov r0,r5    @ dividende
    mov r1,r6    @ divisor
    bl division
    cmp r3,#0    @ remainder = zero ?
    bne 2f
    @ display result if yes
    mov r0,r6
    ldr r1,iAdrsMessFactor
    bl conversion10S
    ldr r0,iAdrsMessFactor
    bl affichageMess
2:
    add r6,#1      @ add 1 to loop counter
    cmp r6,r5      @ <=  number ?
    ble 1b        @ yes loop
100:
    pop {r1-r6}     		/* restaur others registers */
    pop {fp,lr}    				/* restaur des  2 registres */
    bx lr	        			/* return  */
iAdrsMessValeur: .int sMessValeur
iAdrszMessDeb: .int szMessDeb
iAdrsMessFactor: .int sMessFactor
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
/*
### =======================================
*/
/* division integer unsigned                */
/*
### ======================================
*/
division:
    /* r0 contains N */
    /* r1 contains D */
    /* r2 contains Q */
    /* r3 contains R */
    push {r4, lr}
    mov r2, #0                 /* r2 ? 0 */
    mov r3, #0                 /* r3 ? 0 */
    mov r4, #32                /* r4 ? 32 */
    b 2f
1:
    movs r0, r0, LSL #1    /* r0 ? r0 << 1 updating cpsr (sets C if 31st bit of r0 was 1) */
    adc r3, r3, r3         /* r3 ? r3 + r3 + C. This is equivalent to r3 ? (r3 << 1) + C */

    cmp r3, r1             /* compute r3 - r1 and update cpsr */
    subhs r3, r3, r1       /* if r3 >= r1 (C=1) then r3 ? r3 - r1 */
    adc r2, r2, r2         /* r2 ? r2 + r2 + C. This is equivalent to r2 ? (r2 << 1) + C */
2:
    subs r4, r4, #1        /* r4 ? r4 - 1 */
    bpl 1b            /* if r4 >= 0 (N=0) then branch to .Lloop1 */

    pop {r4, lr}
    bx lr

/***************************************************/
/*   conversion register in string décimal signed  */
/***************************************************/
/* r0 contains the register   */
/* r1 contains address of conversion area */
conversion10S:
    push {fp,lr}    /* save registers frame and return */
    push {r0-r5}   /* save other registers  */
    mov r2,r1       /* early storage area */
    mov r5,#'+'     /* default sign is + */
    cmp r0,#0       /* négatif number ? */
    movlt r5,#'-'     /* yes sign is - */
    mvnlt r0,r0       /* and inverse in positive value */
    addlt r0,#1
    mov r4,#10   /* area length */
1: /* conversion loop */
    bl divisionpar10 /* division  */
    add r1,#48        /* add 48 at remainder for conversion ascii */
    strb r1,[r2,r4]  /* store byte area r5 + position r4 */
    sub r4,r4,#1      /* previous position */
    cmp r0,#0
    bne 1b	       /* loop if quotient not equal zéro */
    strb r5,[r2,r4]  /* store sign at current position  */
    subs r4,r4,#1   /* previous position */
    blt  100f         /* if r4 < 0  end  */
    /* else complete area with space */
    mov r3,#' '   /* character space */
2:
    strb r3,[r2,r4]  /* store  byte  */
    subs r4,r4,#1   /* previous position */
    bge 2b        /* loop if r4 greather or equal zero */
100:  /*  standard end of function  */
    pop {r0-r5}   /*restaur others registers */
    pop {fp,lr}   /* restaur des  2 registers frame et return  */
    bx lr

/***************************************************/
/*   division par 10   signé                       */
/* Thanks to http://thinkingeek.com/arm-assembler-raspberry-pi/*
/* and   http://www.hackersdelight.org/            */
/***************************************************/
/* r0 contient le dividende   */
/* r0 retourne le quotient */
/* r1 retourne le reste  */
divisionpar10:
  /* r0 contains the argument to be divided by 10 */
   push {r2-r4}   /* save autres registres  */
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
factors [num] {
	filter $(range 1 36) { num%&=0 }
}

print $(factors 36)
```


{{out}}


```txt
#(1 2 3 4 6 9 12 18 36)
```



## AutoHotkey


```AutoHotkey
msgbox, % factors(45) "`n" factors(53) "`n" factors(64)

Factors(n)
{  Loop, % floor(sqrt(n))
   {  v := A_Index = 1 ? 1 "," n : mod(n,A_Index) ? v : v "," A_Index "," n//A_Index
   }
   Sort, v, N U D,
   Return, v
}
```


{{out}}

```txt

1,3,5,9,15,45
1,53
1,2,4,8,16,32,64
```



## AutoIt


```AutoIt
;AutoIt Version: 3.2.10.0
$num = 45
MsgBox (0,"Factors", "Factors of " & $num & " are: " & factors($num))
consolewrite ("Factors of " & $num & " are: " & factors($num))
Func factors($intg)
   $ls_factors=""
   For $i = 1 to $intg/2
      if ($intg/$i - int($intg/$i))=0 Then
	 $ls_factors=$ls_factors&$i &", "
      EndIf
   Next
   Return $ls_factors&$intg
EndFunc
```


{{out}}

```txt

Factors of 45 are: 1, 3, 5, 9, 15, 45

```



## AWK


```AWK

# syntax: GAWK -f FACTORS_OF_AN_INTEGER.AWK
BEGIN {
    print("enter a number or C/R to exit")
}
{   if ($0 == "") { exit(0) }
    if ($0 !~ /^[0-9]+$/) {
      printf("invalid: %s\n",$0)
      next
    }
    n = $0
    printf("factors of %s:",n)
    for (i=1; i<=n; i++) {
      if (n % i == 0) {
        printf(" %d",i)
      }
    }
    printf("\n")
}

```


{{out}}

```txt

enter a number or C/R to exit
invalid: -1
factors of 0:
factors of 1: 1
factors of 2: 1 2
factors of 11: 1 11
factors of 64: 1 2 4 8 16 32 64
factors of 100: 1 2 4 5 10 20 25 50 100
factors of 32766: 1 2 3 6 43 86 127 129 254 258 381 762 5461 10922 16383 32766
factors of 32767: 1 7 31 151 217 1057 4681 32767

```



## BASIC

{{works with|QBasic}}
This example stores the factors in a shared array (with the original number as the last element) for later retrieval.

Note that this will error out if you pass 32767 (or higher).

```qbasic
DECLARE SUB factor (what AS INTEGER)

REDIM SHARED factors(0) AS INTEGER

DIM i AS INTEGER, L AS INTEGER

INPUT "Gimme a number"; i

factor i

PRINT factors(0);
FOR L = 1 TO UBOUND(factors)
    PRINT ","; factors(L);
NEXT
PRINT

SUB factor (what AS INTEGER)
    DIM tmpint1 AS INTEGER
    DIM L0 AS INTEGER, L1 AS INTEGER

    REDIM tmp(0) AS INTEGER
    REDIM factors(0) AS INTEGER
    factors(0) = 1

    FOR L0 = 2 TO what
        IF (0 = (what MOD L0)) THEN
            'all this REDIMing and copying can be replaced with:
            'REDIM PRESERVE factors(UBOUND(factors)+1)
            'in languages that support the PRESERVE keyword
            REDIM tmp(UBOUND(factors)) AS INTEGER
            FOR L1 = 0 TO UBOUND(factors)
                tmp(L1) = factors(L1)
            NEXT
            REDIM factors(UBOUND(factors) + 1)
            FOR L1 = 0 TO UBOUND(factors) - 1
                factors(L1) = tmp(L1)
            NEXT
            factors(UBOUND(factors)) = L0
        END IF
    NEXT
END SUB
```


{{out}}

```txt

 Gimme a number? 17
  1 , 17
 Gimme a number? 12345
  1 , 3 , 5 , 15 , 823 , 2469 , 4115 , 12345
 Gimme a number? 32765
  1 , 5 , 6553 , 32765
 Gimme a number? 32766
  1 , 2 , 3 , 6 , 43 , 86 , 127 , 129 , 254 , 258 , 381 , 762 , 5461 , 10922 ,
  16383 , 32766

```


==={{header|IS-BASIC}}===
<lang IS-BASIC>100 PROGRAM "Factors.bas"
110 INPUT PROMPT "Number: ":N
120 FOR I=1 TO INT(N/2)
130   IF MOD(N,I)=0 THEN PRINT I;
140 NEXT
150 PRINT N
```


=
## Sinclair ZX81 BASIC
=

```basic
10 INPUT N
20 FOR I=1 TO N
30 IF N/I=INT (N/I) THEN PRINT I;" ";
40 NEXT I
```

{{in}}

```txt
315
```

{{out}}

```txt
1 3 5 7 9 15 35 45 63 105 315
```



## Batch File

Command line version:

```dos
@echo off
set res=Factors of %1:
for /L %%i in (1,1,%1) do call :fac %1 %%i
echo %res%
goto :eof

:fac
set /a test = %1 %% %2
if %test% equ 0 set res=%res% %2
```


{{out}}

```txt
>factors 32767
Factors of 32767: 1 7 31 151 217 1057 4681 32767

>factors 45
Factors of 45: 1 3 5 9 15 45

>factors 53
Factors of 53: 1 53

>factors 64
Factors of 64: 1 2 4 8 16 32 64

>factors 100
Factors of 100: 1 2 4 5 10 20 25 50 100
```


Interactive version:

```dos
@echo off
set /p limit=Gimme a number:
set res=Factors of %limit%:
for /L %%i in (1,1,%limit%) do call :fac %limit% %%i
echo %res%
goto :eof

:fac
set /a test = %1 %% %2
if %test% equ 0 set res=%res% %2
```


{{out}}

```txt
>factors
Gimme a number:27
Factors of 27: 1 3 9 27

>factors
Gimme a number:102
Factors of 102: 1 2 3 6 17 34 51 102
```



## BBC BASIC

{{works with|BBC BASIC for Windows}}

```bbcbasic
      INSTALL @lib$+"SORTLIB"
      sort% = FN_sortinit(0, 0)

      PRINT "The factors of 45 are " FNfactorlist(45)
      PRINT "The factors of 12345 are " FNfactorlist(12345)
      END

      DEF FNfactorlist(N%)
      LOCAL C%, I%, L%(), L$
      DIM L%(32)
      FOR I% = 1 TO SQR(N%)
        IF (N% MOD I% = 0) THEN
          L%(C%) = I%
          C% += 1
          IF (N% <> I%^2) THEN
            L%(C%) = (N% DIV I%)
            C% += 1
          ENDIF
        ENDIF
      NEXT I%
      CALL sort%, L%(0)
      FOR I% = 0 TO C%-1
        L$ += STR$(L%(I%)) + ", "
      NEXT
      = LEFT$(LEFT$(L$))
```


{{out}}

```txt
The factors of 45 are 1, 3, 5, 9, 15, 45
The factors of 12345 are 1, 3, 5, 15, 823, 2469, 4115, 12345
```



## bc


```bc
/* Calculate the factors of n and return their count.
 * This function mutates the global array f[] which will
 * contain all factors of n in ascending order after the call!
 */
define f(n) {
    auto i, d, h, h[], l, o
    /* Local variables:
     * i: Loop variable.
     * d: Complementary (higher) factor to i.
     * h: Will always point to the last element of h[].
     * h[]: Array to hold the greater factor of the pair (x, y), where
     *      x * y == n. The factors are stored in descending order.
     * l: Will always point to the next free spot in f[].
     * o: For saving the value of scale.
     */

    /* Use integer arithmetic */
    o = scale
    scale = 0

    /* Two factors are 1 and n (if n != 1) */
    f[l++] = 1
    if (n == 1) return(1)
    h[0] = n

    /* Main loop */
    for (i = 2; i < h[h]; i++) {
        if (n % i == 0) {
            d = n / i
            if (d != i) {
                h[++h] = d
            }
            f[l++] = i
        }
    }

    /* Append the values in h[] to f[] */
    while (h >= 0) {
        f[l++] = h[h--]
    }

    scale = o
    return(l)
}
```



## Befunge


```Befunge
10:p&v:      >:0:g%#v_0:g\:0:g/\v
     >:0:g:*`|      >           >0:g1+0:p
             >:0:g:*-#v_0:g\>$>:!#@_.v
                      >     ^ ^  ," "<
```



## Burlesque


```burlesque
blsq ) 32767 fc
{1 7 31 151 217 1057 4681 32767}
```



## C


```c
#include <stdio.h>
#include <stdlib.h>

typedef struct {
    int *list;
    short count;
} Factors;

void xferFactors( Factors *fctrs, int *flist, int flix )
{
    int ix, ij;
    int newSize = fctrs->count + flix;
    if (newSize > flix)  {
        fctrs->list = realloc( fctrs->list, newSize * sizeof(int));
    }
    else {
        fctrs->list = malloc(  newSize * sizeof(int));
    }
    for (ij=0,ix=fctrs->count; ix<newSize; ij++,ix++) {
        fctrs->list[ix] = flist[ij];
    }
    fctrs->count = newSize;
}

Factors *factor( int num, Factors *fctrs)
{
    int flist[301], flix;
    int dvsr;
    flix = 0;
    fctrs->count = 0;
    free(fctrs->list);
    fctrs->list = NULL;
    for (dvsr=1; dvsr*dvsr < num; dvsr++) {
        if (num % dvsr != 0) continue;
        if ( flix == 300) {
            xferFactors( fctrs, flist, flix );
            flix = 0;
        }
        flist[flix++] = dvsr;
        flist[flix++] = num/dvsr;
    }
    if (dvsr*dvsr == num)
        flist[flix++] = dvsr;
    if (flix > 0)
        xferFactors( fctrs, flist, flix );

    return fctrs;
}

int main(int argc, char*argv[])
{
    int nums2factor[] = { 2059, 223092870, 3135, 45 };
    Factors ftors = { NULL, 0};
    char sep;
    int i,j;

    for (i=0; i<4; i++) {
        factor( nums2factor[i], &ftors );
        printf("\nfactors of %d are:\n  ", nums2factor[i]);
        sep = ' ';
        for (j=0; j<ftors.count; j++) {
            printf("%c %d", sep, ftors.list[j]);
            sep = ',';
        }
        printf("\n");
    }
    return 0;
}
```


### Prime factoring


```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* 65536 = 2^16, so we can factor all 32 bit ints */
char bits[65536];

typedef unsigned long ulong;
ulong primes[7000], n_primes;

typedef struct { ulong p, e; } prime_factor; /* prime, exponent */

void sieve()
{
	int i, j;
	memset(bits, 1, 65536);
	bits[0] = bits[1] = 0;
	for (i = 0; i < 256; i++)
		if (bits[i])
			for (j = i * i; j < 65536; j += i)
				bits[j] = 0;

	/* collect primes into a list. slightly faster this way if dealing with large numbers */
	for (i = j = 0; i < 65536; i++)
		if (bits[i]) primes[j++] = i;

	n_primes = j;
}

int get_prime_factors(ulong n, prime_factor *lst)
{
	ulong i, e, p;
	int len = 0;

	for (i = 0; i < n_primes; i++) {
		p = primes[i];
		if (p * p > n) break;
		for (e = 0; !(n % p); n /= p, e++);
		if (e) {
			lst[len].p = p;
			lst[len++].e = e;
		}
	}

	return n == 1 ? len : (lst[len].p = n, lst[len].e = 1, ++len);
}

int ulong_cmp(const void *a, const void *b)
{
	return *(const ulong*)a < *(const ulong*)b ? -1 : *(const ulong*)a > *(const ulong*)b;
}

int get_factors(ulong n, ulong *lst)
{
	int n_f, len, len2, i, j, k, p;
	prime_factor f[100];

	n_f = get_prime_factors(n, f);

	len2 = len = lst[0] = 1;
	/* L = (1); L = (L, L * p**(1 .. e)) forall((p, e)) */
	for (i = 0; i < n_f; i++, len2 = len)
		for (j = 0, p = f[i].p; j < f[i].e; j++, p *= f[i].p)
			for (k = 0; k < len2; k++)
				lst[len++] = lst[k] * p;

	qsort(lst, len, sizeof(ulong), ulong_cmp);
	return len;
}

int main()
{
	ulong fac[10000];
	int len, i, j;
	ulong nums[] = {3, 120, 1024, 2UL*2*2*2*3*3*3*5*5*7*11*13*17*19 };

	sieve();

	for (i = 0; i < 4; i++) {
		len = get_factors(nums[i], fac);
		printf("%lu:", nums[i]);
		for (j = 0; j < len; j++)
			printf(" %lu", fac[j]);
		printf("\n");
	}

	return 0;
}
```


{{out}}

```txt
3: 1 3
120: 1 2 3 4 5 6 8 10 12 15 20 24 30 40 60 120
1024: 1 2 4 8 16 32 64 128 256 512 1024
3491888400: 1 2 3 4 5 6 7 8 9 10 11 ...(>1900 numbers)... 1163962800 1745944200 3491888400
```



## C++


```cpp
#include <iostream>
#include <vector>
#include <algorithm>
#include <iterator>

std::vector<int> GenerateFactors(int n)
{
    std::vector<int> factors;
    factors.push_back(1);
    factors.push_back(n);
    for(int i = 2; i * i <= n; ++i)
    {
        if(n % i == 0)
        {
            factors.push_back(i);
            if(i * i != n)
                factors.push_back(n / i);
        }
    }

    std::sort(factors.begin(), factors.end());
    return factors;
}

int main()
{
    const int SampleNumbers[] = {3135, 45, 60, 81};

    for(size_t i = 0; i < sizeof(SampleNumbers) / sizeof(int); ++i)
    {
        std::vector<int> factors = GenerateFactors(SampleNumbers[i]);
        std::cout << "Factors of " << SampleNumbers[i] << " are:\n";
        std::copy(factors.begin(), factors.end(), std::ostream_iterator<int>(std::cout, "\n"));
        std::cout << std::endl;
    }
}
```


## C#
C# 3.0

```c#
using System;
using System.Linq;
using System.Collections.Generic;

public static class Extension
{
    public static List<int> Factors(this int me)
    {
        return Enumerable.Range(1, me).Where(x => me % x == 0).ToList();
    }
}

class Program
{
    static void Main(string[] args)
    {
        Console.WriteLine(String.Join(", ", 45.Factors()));
    }
}
```


C# 1.0

```c#
static void Main(string[] args)
{
	do
	{
		Console.WriteLine("Number:");
		Int64 p = 0;
		do
		{
			try
			{
				p = Convert.ToInt64(Console.ReadLine());
				break;
			}
			catch (Exception)
			{ }

		} while (true);

		Console.WriteLine("For 1 through " + ((int)Math.Sqrt(p)).ToString() + "");
		for (int x = 1; x <= (int)Math.Sqrt(p); x++)
		{
			if (p % x == 0)
				Console.WriteLine("Found: " + x.ToString() + ". " + p.ToString() + " / " + x.ToString() + " = " + (p / x).ToString());
		}

		Console.WriteLine("Done.");
	} while (true);
}
```


{{out}}

```txt
Number:
32434243
For 1 through 5695
Found: 1. 32434243 / 1 = 32434243
Found: 307. 32434243 / 307 = 105649
Done.
```



## Ceylon


```ceylon
shared void run() {
	{Integer*} getFactors(Integer n) =>
		(1..n).filter((Integer element) => element.divides(n));

	for(Integer i in 1..100) {
		print("the factors of ``i`` are ``getFactors(i)``");
	}
}
```



## Chapel

Inspired by the Clojure solution:

```chapel
iter factors(n) {
	for i in 1..floor(sqrt(n)):int {
		if n % i == 0 then {
			yield i;
			yield n / i;
		}
	}
}
```



## Clojure


```lisp
(defn factors [n]
	(filter #(zero? (rem n %)) (range 1 (inc n))))

(print (factors 45))
```

 (1 3 5 9 15 45)

Improved version. Considers small factors from 1 up to (sqrt n) -- we increment it because range does not include the end point. Pair each small factor with its co-factor, flattening the results, and put them into a sorted set to get the factors in order.

```lisp
(defn factors [n]
  (into (sorted-set)
    (mapcat (fn [x] [x (/ n x)])
      (filter #(zero? (rem n %)) (range 1 (inc (Math/sqrt n)))) )))
```


Same idea, using for comprehensions.

```lisp
(defn factors [n]
  (into (sorted-set)
    (reduce concat
      (for [x (range 1 (inc (Math/sqrt n))) :when (zero? (rem n x))]
        [x (/ n x)]))))
```



## COBOL


```cobol

       IDENTIFICATION DIVISION.
       PROGRAM-ID. FACTORS.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  CALCULATING.
           03  NUM  USAGE BINARY-LONG VALUE ZERO.
           03  LIM  USAGE BINARY-LONG VALUE ZERO.
           03  CNT  USAGE BINARY-LONG VALUE ZERO.
           03  DIV  USAGE BINARY-LONG VALUE ZERO.
           03  REM  USAGE BINARY-LONG VALUE ZERO.
           03  ZRS  USAGE BINARY-SHORT VALUE ZERO.

       01  DISPLAYING.
           03  DIS  PIC 9(10) USAGE DISPLAY.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "Factors of? " WITH NO ADVANCING
           ACCEPT NUM
           DIVIDE NUM BY 2 GIVING LIM.

           PERFORM VARYING CNT FROM 1 BY 1 UNTIL CNT > LIM
               DIVIDE NUM BY CNT GIVING DIV REMAINDER REM
               IF REM = 0
                   MOVE CNT TO DIS
                   PERFORM SHODIS
               END-IF
           END-PERFORM.

           MOVE NUM TO DIS.
           PERFORM SHODIS.
           STOP RUN.

       SHODIS.
           MOVE ZERO TO ZRS.
           INSPECT DIS TALLYING ZRS FOR LEADING ZERO.
           DISPLAY DIS(ZRS + 1:)
           EXIT PARAGRAPH.

       END PROGRAM FACTORS.

```



## CoffeeScript


```coffeescript
# Reference implementation for finding factors is slow, but hopefully
# robust--we'll use it to verify the more complicated (but hopefully faster)
# algorithm.
slow_factors = (n) ->
  (i for i in [1..n] when n % i == 0)

# The rest of this code does two optimizations:
#   1) When you find a prime factor, divide it out of n (smallest_prime_factor).
#   2) Find the prime factorization first, then compute composite factors from those.

smallest_prime_factor = (n) ->
  for i in [2..n]
    return n if i*i > n
    return i if n % i == 0

prime_factors = (n) ->
  return {} if n == 1
  spf = smallest_prime_factor n
  result = prime_factors(n / spf)
  result[spf] or= 0
  result[spf] += 1
  result

fast_factors = (n) ->
  prime_hash = prime_factors n
  exponents = []
  for p of prime_hash
    exponents.push
      p: p
      exp: 0
  result = []
  while true
    factor = 1
    for obj in exponents
      factor *= Math.pow obj.p, obj.exp
    result.push factor
    break if factor == n
    # roll the odometer
    for obj, i in exponents
      if obj.exp < prime_hash[obj.p]
        obj.exp += 1
        break
      else
        obj.exp = 0

  return result.sort (a, b) -> a - b

verify_factors = (factors, n) ->
  expected_result = slow_factors n
  throw Error("wrong length") if factors.length != expected_result.length
  for factor, i in expected_result
    console.log Error("wrong value") if factors[i] != factor


for n in [1, 3, 4, 8, 24, 37, 1001, 11111111111, 99999999999]
  factors = fast_factors n
  console.log n, factors
  if n < 1000000
    verify_factors factors, n
```


{{out}}

```txt
> coffee factors.coffee
1 [ 1 ]
3 [ 1, 3 ]
4 [ 1, 2, 4 ]
8 [ 1, 2, 4, 8 ]
24 [ 1, 2, 3, 4, 6, 8, 12, 24 ]
37 [ 1, 37 ]
1001 [ 1, 7, 11, 13, 77, 91, 143, 1001 ]
11111111111 [ 1, 21649, 513239, 11111111111 ]
99999999999 [ 1,
  3,
  9,
  21649,
  64947,
  194841,
  513239,
  1539717,
  4619151,
  11111111111,
  33333333333,
  99999999999 ]
```



## Common Lisp

We iterate in the range <code>1..sqrt(n)</code> collecting ‘low’ factors and corresponding ‘high’ factors, and combine at the end to produce an ordered list of factors.

```lisp
(defun factors (n &aux (lows '()) (highs '()))
  (do ((limit (1+ (isqrt n))) (factor 1 (1+ factor)))
      ((= factor limit)
       (when (= n (* limit limit))
         (push limit highs))
       (remove-duplicates (nreconc lows highs)))
    (multiple-value-bind (quotient remainder) (floor n factor)
      (when (zerop remainder)
        (push factor lows)
        (push quotient highs)))))
```



## D


### Procedural Style


```d
import std.stdio, std.math, std.algorithm;

T[] factors(T)(in T n) pure nothrow {
    if (n == 1)
        return [n];

    T[] res = [1, n];
    T limit = cast(T)real(n).sqrt + 1;
    for (T i = 2; i < limit; i++) {
        if (n % i == 0) {
            res ~= i;
            immutable q = n / i;
            if (q > i)
                res ~= q;
        }
    }

    return res.sort().release;
}

void main() {
    writefln("%(%s\n%)", [45, 53, 64, 1111111].map!factors);
}
```

{{out}}

```txt
[1, 3, 5, 9, 15, 45]
[1, 53]
[1, 2, 4, 8, 16, 32, 64]
[1, 239, 4649, 1111111]
```



### Functional Style


```d
import std.stdio, std.algorithm, std.range;

auto factors(I)(I n) {
    return iota(1, n + 1).filter!(i => n % i == 0);
}

void main() {
    36.factors.writeln;
}
```

{{out}}

```txt
[1, 2, 3, 4, 6, 9, 12, 18, 36]
```




## Dart


```txt

import 'dart:math';

factors(n)
{
 var factorsArr = [];
 factorsArr.add(n);
 factorsArr.add(1);
 for(var test = n - 1; test >= sqrt(n).toInt(); test--)
  if(n % test == 0)
  {
   factorsArr.add(test);
   factorsArr.add(n / test);
  }
 return factorsArr;
}

void main() {
  print(factors(5688));
}

```



## Dyalect



```Dyalect
func Iterator.where(pred) {
    for x in this when pred(x) {
        yield x
    }
}

func Integer.factors() {
    (1..this).where(this % $0 == 0)
}

for x in 45.factors() {
    print(x)
}
```


Output:


```txt
1
3
5
9
15
45
```



## E

{{improve|E|Use a cleverer algorithm such as in the Common Lisp example.}}

```e
def factors(x :(int > 0)) {
    var xfactors := []
    for f ? (x % f <=> 0) in 1..x {
      xfactors with= f
    }
    return xfactors
}
```



## EasyLang

<lang>n = 720
for i = 1 to n
  if n mod i = 0
    factors[] &= i
  .
.
print factors[]
```



## EchoLisp

'''prime-factors''' gives the list of n's prime-factors. We mix them to get all the factors.

```scheme

;; ppows
;; input : a list g of grouped prime factors ( 3 3 3 ..)
;; returns (1 3 9 27 ...)

(define (ppows g (mult 1))
	(for/fold (ppows '(1)) ((a g))
	    (set! mult (* mult a))
	    (cons mult ppows)))

;; factors
;; decomp n into ((2 2 ..) ( 3 3 ..)  ) prime factors groups
;; combines (1 2 4 8 ..) (1 3 9 ..) lists

(define (factors n)
   (list-sort <
   (if (<= n 1) '(1)
        (for/fold (divs'(1)) ((g (map  ppows (group (prime-factors n)))))
		    (for*/list ((a divs) (b g)) (* a b))))))

```

{{out}}

```scheme

(lib 'bigint)
(factors 666)
   → (1 2 3 6 9 18 37 74 111 222 333 666)

(length (factors 108233175859200))
   → 666 ;; 💀

(define huge 1200034005600070000008900000000000000000)
(time ( length (factors huge)))
    → (394ms 7776)

```



## Ela


===Using higher-order function===

```ela
open list

factors m = filter (\x -> m % x == 0) [1..m]
```



### Using comprehension


```ela
factors m = [x \\ x <- [1..m] | m % x == 0]
```



## Elixir


```elixir
defmodule RC do
  def factor(1), do: [1]
  def factor(n) do
    (for i <- 1..div(n,2), rem(n,i)==0, do: i) ++ [n]
  end

  # Recursive (faster version);
  def divisor(n), do: divisor(n, 1, []) |> Enum.sort

  defp divisor(n, i, factors) when n < i*i    , do: factors
  defp divisor(n, i, factors) when n == i*i   , do: [i | factors]
  defp divisor(n, i, factors) when rem(n,i)==0, do: divisor(n, i+1, [i, div(n,i) | factors])
  defp divisor(n, i, factors)                 , do: divisor(n, i+1, factors)
end

Enum.each([45, 53, 60, 64], fn n ->
  IO.puts "#{n}: #{inspect RC.factor(n)}"
end)

IO.puts "\nRange: #{inspect range = 1..10000}"
funs = [ factor:  &RC.factor/1,
         divisor: &RC.divisor/1 ]
Enum.each(funs, fn {name, fun} ->
  {time, value} = :timer.tc(fn -> Enum.count(range, &length(fun.(&1))==2) end)
  IO.puts "#{name}\t prime count : #{value},\t#{time/1000000} sec"
end)

```


{{out}}

```txt

45: [1, 3, 5, 9, 15, 45]
53: [1, 53]
60: [1, 2, 3, 4, 5, 6, 10, 12, 15, 20, 30, 60]
64: [1, 2, 4, 8, 16, 32, 64]

Range: 1..10000
factor   prime count : 1229,    7.316 sec
divisor  prime count : 1229,    0.265 sec

```



## Erlang


### with Built in fuctions


```erlang
factors(N) ->
    [I || I <- lists:seq(1,trunc(N/2)), N rem I == 0]++[N].
```



### Recursive

Another, less concise, but faster version

```erlang


-module(divs).
-export([divs/1]).

divs(0) -> [];
divs(1) -> [];
divs(N) -> lists:sort(divisors(1,N))++[N].

divisors(1,N) ->
     [1] ++ divisors(2,N,math:sqrt(N)).

divisors(K,_N,Q) when K > Q -> [];
divisors(K,N,_Q) when N rem K =/= 0 ->
    [] ++ divisors(K+1,N,math:sqrt(N));
divisors(K,N,_Q) when K * K  == N ->
    [K] ++ divisors(K+1,N,math:sqrt(N));
divisors(K,N,_Q) ->
    [K, N div K] ++ divisors(K+1,N,math:sqrt(N)).

```

{{out}}

```txt

58> timer:tc(divs, factors, [20000]).
{2237,
 [1,2,4,5,8,10,16,20,25,32,40,50,80,100,125,160,200,250,400,
  500,625,800,1000,1250,2000,2500,4000|...]}
59> timer:tc(divs, divs, [20000]).
{106,
 [1,2,4,5,8,10,16,20,25,32,40,50,80,100,125,160,200,250,400,
  500,625,800,1000,1250,2000,2500,4000|...]}

```


The first number is milliseconds. I'v ommitted repeating the first fuction.


## ERRE


```ERRE

PROGRAM FACTORS

!$DOUBLE

PROCEDURE FACTORLIST(N->L$)

      LOCAL C%,I,FLIPS%,I%
      LOCAL DIM L[32]
      FOR I=1 TO SQR(N) DO
        IF N=I*INT(N/I) THEN
          L[C%]=I
          C%=C%+1
          IF N<>I*I THEN
            L[C%]=INT(N/I)
            C%=C%+1
          END IF
        END IF
      END FOR

      ! BUBBLE SORT ARRAY L[]
      FLIPS%=1
      WHILE FLIPS%>0 DO
         FLIPS%=0
         FOR I%=0 TO C%-2 DO
            IF L[I%]>L[I%+1] THEN SWAP(L[I%],L[I%+1]) FLIPS%=1
         END FOR
      END WHILE

      L$=""
      FOR I%=0 TO C%-1 DO
        L$=L$+STR$(L[I%])+","
      END FOR
      L$=LEFT$(L$,LEN(L$)-1)

END PROCEDURE

BEGIN
    PRINT(CHR$(12);) ! CLS
    FACTORLIST(45->L$)
    PRINT("The factors of 45 are ";L$)
    FACTORLIST(12345->L$)
    PRINT("The factors of 12345 are ";L$)
END PROGRAM

```

{{out}}

```txt

The factors of 45 are  1, 3, 5, 9, 15, 45
The factors of 12345 are  1, 3, 5, 15, 823, 2469, 4115, 12345

```


=={{header|F Sharp|F#}}==
If number % divisor = 0 then both divisor AND number / divisor are factors.

So, we only have to search till sqrt(number).

Also, this is lazily evaluated.

```fsharp
let factors number = seq {
    for divisor in 1 .. (float >> sqrt >> int) number do
    if number % divisor = 0 then
        yield divisor
        if number <> 1 then yield number / divisor //special case condition: when number=1 then divisor=(number/divisor), so don't repeat it
}
```



### Prime factoring


```fsharp

[6;120;2048;402642;1206432] |> Seq.iter(fun n->printf "%d :" n; [1..n]|>Seq.filter(fun g->n%g=0)|>Seq.iter(fun n->printf " %d" n); printfn "");;
```


{{out}}

```txt

OUTPUT :
6 : 1  2  3  6
120 : 1  2  3  4  5  6  8  10  12  15  20  24  30  40  60  120
2048 : 1  2  4  8  16  32  64  128  256  512  1024  2048
402642 : 1  2  3  6  9  18  22369  44738  67107  134214  201321  402642
120643200 : 1  2  3  4  6  8  9  12  16  18  24  32  36  48  59  71  72  96  118  142  144  177  213  236  284  288  354  426  472  531  568  639  708  852  944  1062  1136  12
78  1416  1704  1888  2124  2272  2556  2832  3408  4189  4248  5112  5664  6816  8378  8496  10224  12567  16756  16992  20448  25134  33512  37701  50268  67024  75402  10053
6  134048  150804  201072  301608  402144  603216  1206432

```


=={{Header|Factor}}==
    USE: math.primes.factors
    ( scratchpad ) 24 divisors .
    { 1 2 3 4 6 8 12 24 }


## FALSE


```false
[1[\$@$@-][\$@$@$@$@\/*=[$." "]?1+]#.%]f:
45f;! 53f;! 64f;!
```



## Fish


```Fish
0v
 >i:0(?v'0'%+a*
       >~a,:1:>r{%        ?vr:nr','ov
              ^:&:;?(&:+1r:<        <

```

Must be called with pre-polulated value (Positive Integer) in the input stack. Try at Fish Playground[https://fishlanguage.com/playground/onD7KN6YK3XMzLFdr].
For Input Number :
```txt
 120
```

The following output was generated:

```txt
1,2,3,4,5,6,8,10,12,15,20,24,30,40,60,120,
```


=={{Header|Forth}}==
This is a slightly optimized algorithm, since it realizes there are no factors between n/2 and n. The values are saved on the stack and - in true Forth fashion - printed in descending order.

```Forth
: factors dup 2/ 1+ 1 do dup i mod 0= if i swap then loop ;
: .factors factors begin dup dup . 1 <> while drop repeat drop cr ;

45 .factors
53 .factors
64 .factors
100 .factors
```


=={{Header|Fortran}}==
{{works with|Fortran|90 and later}}

```fortran
program Factors
  implicit none
  integer :: i, number

  write(*,*) "Enter a number between 1 and 2147483647"
  read*, number

  do i = 1, int(sqrt(real(number))) - 1
    if (mod(number, i) == 0) write (*,*) i, number/i
  end do

  ! Check to see if number is a square
  i = int(sqrt(real(number)))
  if (i*i == number) then
     write (*,*) i
  else if (mod(number, i) == 0) then
     write (*,*) i, number/i
  end if

end program
```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Sub printFactors(n As Integer)
  If n < 1 Then Return
  Print n; " =>";
  For i As Integer = 1 To n / 2
    If n Mod i = 0 Then Print i; " ";
  Next i
  Print n
End Sub

printFactors(11)
printFactors(21)
printFactors(32)
printFactors(45)
printFactors(67)
printFactors(96)
Print
Print "Press any key to quit"
Sleep
```


{{out}}

```txt

 11 => 1  11
 21 => 1  3  7  21
 32 => 1  2  4  8  16  32
 45 => 1  3  5  9  15  45
 67 => 1  67
 96 => 1  2  3  4  6  8  12  16  24  32  48  96

```


=={{Header|Frink}}==
Frink has built-in factoring functions which use wheel factoring, trial division, Pollard p-1 factoring, and Pollard rho factoring. It also recognizes some special forms (e.g. Mersenne numbers) and handles them efficiently.  Integers can either be decomposed into prime factors or all factors.

The <CODE>factors[<I>n</I>]</CODE> function will return the prime decomposition of <CODE><I>n</I></CODE>.

The <CODE>allFactors[<i>n</i>, <i>include1=true</i>, <i>includeN=true</i>, <i>sort=true</i>, <i>onlyToSqrt=false</i>]</CODE> function will return all factors of <CODE><I>n</I></CODE>. The optional arguments <code>include1</code> and <code>includeN</code> indicate if the numbers 1 and n are to be included in the results.  If the optional argument <code>sort</code> is true, the results will be sorted. If the optional argument <code>onlyToSqrt</code>=true, then only the factors less than or equal to the square root of the number will be produced.

The following produces all factors of n, including 1 and n:


```frink
allFactors[n]
```



## FunL

Function to compute set of factors:

```funl
def factors( n ) = {d | d <- 1..n if d|n}
```


Test:

```funl
for x <- [103, 316, 519, 639, 760]
  println( 'The set of factors of ' + x + ' is ' + factors(x) )
```


{{out}}


```txt

The set of factors of 103 is {1, 103}
The set of factors of 316 is {158, 4, 79, 1, 2, 316}
The set of factors of 519 is {1, 3, 173, 519}
The set of factors of 639 is {9, 639, 71, 213, 1, 3}
The set of factors of 760 is {8, 19, 4, 40, 152, 5, 10, 76, 1, 95, 190, 760, 20, 2, 38, 380}

```




## FutureBasic


```futurebasic

include "ConsoleWindow"

clear local mode
local fn IntegerFactors( f as long ) as Str255
dim as long i, s, l(100), c : c = 0
dim as Str255 factorStr

for i = 1 to sqr(f)
  if ( f mod i == 0 )
    l(c) = i
    c++
      if ( f <> i ^ 2 )
        l(c) = ( f / i )
        c++
      end if
  end if
next i
s = 1
while ( s = 1 )
s = 0
  for i = 0 to c-1
    if l(i) > l(i+1) and l(i+1) <> 0
      swap l(i), l(i+1)
      s = 1
    end if
  next i
wend
for i = 0 to c-1
  if ( i < c -1 )
    factorStr = factorStr + str$(l(i)) + ","
  else
    factorStr = factorStr + str$(l(i))
  end if
next
end fn = factorStr

print "Factors of 25 are:"; fn IntegerFactors( 25 )
print "Factors of 45 are:"; fn IntegerFactors( 45 )
print "Factors of 103 are:"; fn IntegerFactors( 103 )
print "Factors of 760 are:"; fn IntegerFactors( 760 )
print "Factors of 12345 are:"; fn IntegerFactors( 12345 )
print "Factors of 32766 are:"; fn IntegerFactors( 32766 )
print "Factors of 32767 are:"; fn IntegerFactors( 32767 )
print "Factors of 57097 are:"; fn IntegerFactors( 57097 )
print "Factors of 12345678 are:"; fn IntegerFactors( 12345678 )
print "Factors of 32434243 are:"; fn IntegerFactors( 32434243 )

```


Output:

```txt

Factors of 25 are: 1, 5, 25
Factors of 45 are: 1, 3, 5, 9, 15, 45
Factors of 103 are: 1, 103
Factors of 760 are: 1, 2, 4, 5, 8, 10, 19, 20, 38, 40, 76, 95, 152, 190, 380, 760
Factors of 12345 are: 1, 3, 5, 15, 823, 2469, 4115, 12345
Factors of 32766 are: 1, 2, 3, 6, 43, 86, 127, 129, 254, 258, 381, 762, 5461, 10922, 16383, 32766
Factors of 32767 are: 1, 7, 31, 151, 217, 1057, 4681, 32767
Factors of 57097 are: 1, 57097
Factors of 12345678 are: 1, 2, 3, 6, 9, 18, 47, 94, 141, 282, 423, 846, 14593, 29186, 43779, 87558, 131337, 262674, 685871, 1371742, 2057613, 4115226, 6172839, 12345678
Factors of 32434243 are: 1, 307, 105649, 32434243

```



## GAP


```gap
# Built-in function
DivisorsInt(Factorial(5));
# [ 1, 2, 3, 4, 5, 6, 8, 10, 12, 15, 20, 24, 30, 40, 60, 120 ]

# A possible implementation, not suitable to large n
div := n -> Filtered([1 .. n], k -> n mod k = 0);

div(Factorial(5));
# [ 1, 2, 3, 4, 5, 6, 8, 10, 12, 15, 20, 24, 30, 40, 60, 120 ]

# Another implementation, usable for large n (if n can be factored quickly)
div2 := function(n)
  local f, p;
  f := Collected(FactorsInt(n));
  p := List(f, v -> List([0 .. v[2]], k -> v[1]^k));
  return SortedList(List(Cartesian(p), Product));
end;

div2(Factorial(5));
# [ 1, 2, 3, 4, 5, 6, 8, 10, 12, 15, 20, 24, 30, 40, 60, 120 ]
```



## Go

Trial division, no prime number generator, but with some optimizations.  It's good enough to factor any 64 bit integer, with large primes taking several seconds.

```go
package main

import "fmt"

func main() {
    printFactors(-1)
    printFactors(0)
    printFactors(1)
    printFactors(2)
    printFactors(3)
    printFactors(53)
    printFactors(45)
    printFactors(64)
    printFactors(600851475143)
    printFactors(999999999999999989)
}

func printFactors(nr int64) {
    if nr < 1 {
        fmt.Println("\nFactors of", nr, "not computed")
        return
    }
    fmt.Printf("\nFactors of %d: ", nr)
    fs := make([]int64, 1)
    fs[0] = 1
    apf := func(p int64, e int) {
        n := len(fs)
        for i, pp := 0, p; i < e; i, pp = i+1, pp*p {
            for j := 0; j < n; j++ {
                fs = append(fs, fs[j]*pp)
            }
        }
    }
    e := 0
    for ; nr & 1 == 0; e++ {
        nr >>= 1
    }
    apf(2, e)
    for d := int64(3); nr > 1; d += 2 {
        if d*d > nr {
            d = nr
        }
        for e = 0; nr%d == 0; e++ {
            nr /= d
        }
        if e > 0 {
            apf(d, e)
        }
    }
    fmt.Println(fs)
    fmt.Println("Number of factors =", len(fs))
}
```


{{out}}

```txt
Factors of -1 not computed

Factors of 0 not computed

Factors of 1: [1]
Number of factors = 1

Factors of 2: [1 2]
Number of factors = 2

Factors of 3: [1 3]
Number of factors = 2

Factors of 53: [1 53]
Number of factors = 2

Factors of 45: [1 3 9 5 15 45]
Number of factors = 6

Factors of 64: [1 2 4 8 16 32 64]
Number of factors = 7

Factors of 600851475143: [1 71 839 59569 1471 104441 1234169 87625999 6857 486847 5753023 408464633 10086647 716151937 8462696833 600851475143]
Number of factors = 16

Factors of 999999999999999989: [1 999999999999999989]
Number of factors = 2
```



## Gosu


```gosu
var numbers = {11, 21, 32, 45, 67, 96}
numbers.each(\ number -> printFactors(number))

function printFactors(n: int) {
  if (n < 1) return
  var result ="${n} => "
  (1 .. n/2).each(\ i -> {result += n % i == 0 ? "${i} " : ""})
  print("${result}${n}")
}
```


{{out}}

```txt

11 => 1 11
21 => 1 3 7 21
32 => 1 2 4 8 16 32
45 => 1 3 5 9 15 45
67 => 1 67
96 => 1 2 3 4 6 8 12 16 24 32 48 96

```


=={{Header|Groovy}}==
A straight brute force approach up to the square root of ''N'':

```groovy
def factorize = { long target ->

    if (target == 1) return [1L]

    if (target < 4) return [1L, target]

    def targetSqrt = Math.sqrt(target)
    def lowfactors = (2L..targetSqrt).grep { (target % it) == 0 }
    if (lowfactors == []) return [1L, target]
    def nhalf = lowfactors.size() - ((lowfactors[-1] == targetSqrt) ? 1 : 0)

    [1] + lowfactors + (0..<nhalf).collect { target.intdiv(lowfactors[it]) }.reverse() + [target]
}
```


Test:

```groovy
((1..30) + [333333]).each { println ([number:it, factors:factorize(it)]) }
```

{{out}}

```txt
[number:1, factors:[1]]
[number:2, factors:[1, 2]]
[number:3, factors:[1, 3]]
[number:4, factors:[1, 2, 4]]
[number:5, factors:[1, 5]]
[number:6, factors:[1, 2, 3, 6]]
[number:7, factors:[1, 7]]
[number:8, factors:[1, 2, 4, 8]]
[number:9, factors:[1, 3, 9]]
[number:10, factors:[1, 2, 5, 10]]
[number:11, factors:[1, 11]]
[number:12, factors:[1, 2, 3, 4, 6, 12]]
[number:13, factors:[1, 13]]
[number:14, factors:[1, 2, 7, 14]]
[number:15, factors:[1, 3, 5, 15]]
[number:16, factors:[1, 2, 4, 8, 16]]
[number:17, factors:[1, 17]]
[number:18, factors:[1, 2, 3, 6, 9, 18]]
[number:19, factors:[1, 19]]
[number:20, factors:[1, 2, 4, 5, 10, 20]]
[number:21, factors:[1, 3, 7, 21]]
[number:22, factors:[1, 2, 11, 22]]
[number:23, factors:[1, 23]]
[number:24, factors:[1, 2, 3, 4, 6, 8, 12, 24]]
[number:25, factors:[1, 5, 25]]
[number:26, factors:[1, 2, 13, 26]]
[number:27, factors:[1, 3, 9, 27]]
[number:28, factors:[1, 2, 4, 7, 14, 28]]
[number:29, factors:[1, 29]]
[number:30, factors:[1, 2, 3, 5, 6, 10, 15, 30]]
[number:333333, factors:[1, 3, 7, 9, 11, 13, 21, 33, 37, 39, 63, 77, 91, 99, 111, 117, 143, 231, 259, 273, 333, 407, 429, 481, 693, 777, 819, 1001, 1221, 1287, 1443, 2331, 2849, 3003, 3367, 3663, 4329, 5291, 8547, 9009, 10101, 15873, 25641, 30303, 37037, 47619, 111111, 333333]]
```


=={{Header|Haskell}}==
Using [https://web.archive.org/web/20121130222921/http://www.polyomino.f2s.com/david/haskell/codeindex.html D. Amos'es Primes module] for finding prime factors

```Haskell
import HFM.Primes (primePowerFactors)
import Control.Monad (mapM)
import Data.List (product)

-- primePowerFactors :: Integer -> [(Integer,Int)]

factors = map product .
          mapM (\(p,m)-> [p^i | i<-[0..m]]) . primePowerFactors
```


Returns list of factors out of order, e.g.:

<Lang haskell>~> factors 42
[1,7,3,21,2,14,6,42]
```


Or, [[Prime_decomposition#Haskell|prime decomposition task]] can be used (although, a trial division-only version will become very slow for large primes),


```haskell
import Data.List (group)
primePowerFactors = map (\x-> (head x, length x)) . group . factorize
```


The above function can also be found in the package [http://hackage.haskell.org/package/arithmoi <code>arithmoi</code>], as <code>Math.NumberTheory.Primes.factorise :: Integer -> [(Integer, Int)]</code>, [http://hackage.haskell.org/package/arithmoi-0.4.2.0/docs/Math-NumberTheory-Primes-Factorisation.html which performs] "factorisation of Integers by the elliptic curve algorithm after Montgomery" and "is best suited for numbers of up to 50-60 digits".

Or, deriving cofactors from factors up to the square root:


```Haskell
import Control.Arrow ((&&&))
import Data.Bool (bool)

integerFactors :: Int -> [Int]
integerFactors n =
  bool -- For perfect squares, `tail` excludes cofactor of square root
    (lows ++ (quot n <$> bool id tail (n == intSquared) (reverse lows)))
    []
    (n < 1)
  where
    (intSquared, lows) =
      (^ 2) &&& (filter ((0 ==) . rem n) . enumFromTo 1) $
      floor (sqrt $ fromIntegral n)

main :: IO ()
main = print $ integerFactors 600
```


{{Out}}

```txt
[1,2,3,4,5,6,8,10,12,15,20,24,25,30,40,50,60,75,100,120,150,200,300,600]
```



###  List comprehension

Naive, functional, no import, in increasing order:

```Haskell
factorsNaive n =
  [ i
  | i <- [1 .. n]
  , mod n i == 0 ]
```


```Haskell
~> factorsNaive 25
[1,5,25]
```


Factor, ''cofactor''. Get the list of factor&ndash;cofactor pairs sorted, for a quadratic speedup:

```Haskell
import Data.List (sort)

factorsCo n =
  sort
    [ i
    | i <- [1 .. floor (sqrt (fromIntegral n))]
    , (d, 0) <- [divMod n i]
    , i <-
       i :
       [ d
       | d > i ] ]
```


A version of the above without the need for sorting, making it to be ''online'' (i.e. productive immediately, which can be seen in GHCi); factors in increasing order:

```Haskell
factorsO n =
  ds ++
  [ r
  | (d, 0) <- [divMod n r]
  , r <-
     r :
     [ d
     | d > r ] ] ++
  reverse (map (n `div`) ds)
  where
    r = floor (sqrt (fromIntegral n))
    ds =
      [ i
      | i <- [1 .. r - 1]
      , mod n i == 0 ]
```

Testing:

```Haskell
*Main> :set +s
~> factorsO 120
[1,2,3,4,5,6,8,10,12,15,20,24,30,40,60,120]
(0.00 secs, 0 bytes)

~> factorsO 12041111117
[1,7,41,287,541,3787,22181,77551,155267,542857,3179591,22257137,41955091,2936856
37,1720158731,12041111117]
(0.09 secs, 50758224 bytes)
```



## HicEst


```hicest
 DLG(NameEdit=N, TItle='Enter an integer')

 DO i = 1, N^0.5
   IF( MOD(N,i) == 0) WRITE() i, N/i
 ENDDO

END
```


=={{header|Icon}} and {{header|Unicon}}==

```Icon
procedure main(arglist)
numbers := arglist ||| [ 32767, 45, 53, 64, 100]    # combine command line provided and default set of values
every writes(lf,"factors of ",i := !numbers,"=") & writes(divisors(i)," ") do lf := "\n"
end

link factors
```


{{out}}

```txt
factors of 32767=1 7 31 151 217 1057 4681 32767
factors of 45=1 3 5 9 15 45
factors of 53=1 53
factors of 64=1 2 4 8 16 32 64
factors of 100=1 2 4 5 10 20 25 50 100
```


{{libheader|Icon Programming Library}} [http://www.cs.arizona.edu/icon/library/src/procs/factors.icn divisors]


## J

The "brute force" approach is the most concise:


```J
foi=: [: I. 0 = (|~ i.@>:)
```


Example use:


```J
   foi 40
1 2 4 5 8 10 20 40
```


Basically we test every non-negative integer up through the number itself to see if it divides evenly.

However, this becomes very slow for large numbers. So other approaches can be worthwhile.

J has a primitive, q: which returns its argument's prime factors.

```J
q: 40
 2 2 2 5
```


Alternatively, q: can produce provide a table of the exponents of the unique relevant prime factors

```J
   __ q: 420
2 3 5 7
2 1 1 1
```


With this, we can form lists of each of the potential relevant powers of each of these prime factors

```J
   (^ i.@>:)&.>/ __ q: 420
┌─────┬───┬───┬───┐
│1 2 4│1 3│1 5│1 7│
└─────┴───┴───┴───┘
```


From here, it's a simple matter (<code>*/&>@{</code>) to compute all possible factors of the original number

```J
factrs=: */&>@{@((^ i.@>:)&.>/)@q:~&__
   factrs 40
 1  5
 2 10
 4 20
 8 40
```


However, a data structure which is organized around the prime decomposition of the argument can be hard to read.  So, for reader convenience, we should probably arrange them in a monotonically increasing list:


```J
   factors=: [: /:~@, */&>@{@((^ i.@>:)&.>/)@q:~&__
   factors 420
1 2 3 4 5 6 7 10 12 14 15 20 21 28 30 35 42 60 70 84 105 140 210 420
```


A less efficient, but concise variation on this theme:


```J
    ~.,*/&> { 1 ,&.> q: 40
1 5 2 10 4 20 8 40
```


This computes 2^n intermediate values where n is the number of prime factors of the original number.

That said, note that we get a representation issue when dealing with large numbers:


```J
   factors 568474220
1 2 4 5 10 17 20 34 68 85 170 340 1.67198e6 3.34397e6 6.68793e6 8.35992e6 1.67198e7 2.84237e7 3.34397e7 5.68474e7 1.13695e8 1.42119e8 2.84237e8 5.68474e8
```


One approach here (if we don't want to explicitly format the result) is to use an arbitrary precision (aka "extended") argument. This propagates through into the result:


```J
   factors 568474220x
1 2 4 5 10 17 20 34 68 85 170 340 1671983 3343966 6687932 8359915 16719830 28423711 33439660 56847422 113694844 142118555 284237110 568474220
```


Another less efficient approach, in which remainders are examined up to the square root, larger factors obtained as fractions, and the combined list nubbed and sorted might be:

```J
factorsOfNumber=: monad define
  Y=. y"_
  /:~ ~. ( , Y%]) ( #~ 0=]|Y) 1+i.>.%:y
)

   factorsOfNumber 40
1 2 4 5 8 10 20 40
```


Another approach:


```J
odometer =: #: i.@(*/)
factors=: (*/@:^"1 odometer@:>:)/@q:~&__
```


See http://www.jsoftware.com/jwiki/Essays/Odometer


## Java

{{works with|Java|5+}}

```java5>public static TreeSet<Long
 factors(long n)
{
 TreeSet<Long> factors = new TreeSet<Long>();
 factors.add(n);
 factors.add(1L);
 for(long test = n - 1; test >= Math.sqrt(n); test--)
  if(n % test == 0)
  {
   factors.add(test);
   factors.add(n / test);
  }
 return factors;
}
```



## JavaScript



### Imperative



```javascript
function factors(num)
{
 var
  n_factors = [],
  i;

 for (i = 1; i <= Math.floor(Math.sqrt(num)); i += 1)
  if (num % i === 0)
  {
   n_factors.push(i);
   if (num / i !== i)
    n_factors.push(num / i);
  }
 n_factors.sort(function(a, b){return a - b;});  // numeric sort
 return n_factors;
}

factors(45);  // [1,3,5,9,15,45]
factors(53);  // [1,53]
factors(64);  // [1,2,4,8,16,32,64]
```



### Functional



### =ES5=


Translating the naive list comprehension  example from Haskell, using a list monad for the comprehension


```JavaScript
// Monadic bind (chain) for lists
function chain(xs, f) {
  return [].concat.apply([], xs.map(f));
}

// [m..n]
function range(m, n) {
  return Array.apply(null, Array(n - m + 1)).map(function (x, i) {
    return m + i;
  });
}

function factors_naive(n) {
  return chain( range(1, n), function (x) {       // monadic chain/bind
    return n % x ? [] : [x];                      // monadic fail or inject/return
  });
}

factors_naive(6)
```


Output:

```JavaScript
[1, 2, 3, 6]
```


Translating the Haskell (lows and highs) example


```JavaScript
console.log(
  (function (lstTest) {

    // INTEGER FACTORS
    function integerFactors(n) {
      var rRoot = Math.sqrt(n),
        intRoot = Math.floor(rRoot),

        lows = range(1, intRoot).filter(function (x) {
          return (n % x) === 0;
        });

      // for perfect squares, we can drop the head of the 'highs' list
      return lows.concat(lows.map(function (x) {
        return n / x;
      }).reverse().slice((rRoot === intRoot) | 0));
    }

    // [m .. n]
    function range(m, n) {
      return Array.apply(null, Array(n - m + 1)).map(function (x, i) {
        return m + i;
      });
    }

    /*************************** TESTING *****************************/

    // TABULATION OF RESULTS IN SPACED AND ALIGNED COLUMNS
    function alignedTable(lstRows, lngPad, fnAligned) {
      var lstColWidths = range(0, lstRows.reduce(function (a, x) {
        return x.length > a ? x.length : a;
      }, 0) - 1).map(function (iCol) {
        return lstRows.reduce(function (a, lst) {
          var w = lst[iCol] ? lst[iCol].toString().length : 0;
          return (w > a) ? w : a;
        }, 0);
      });

      return lstRows.map(function (lstRow) {
        return lstRow.map(function (v, i) {
          return fnAligned(v, lstColWidths[i] + lngPad);
        }).join('')
      }).join('\n');
    }

    function alignRight(n, lngWidth) {
      var s = n.toString();
      return Array(lngWidth - s.length + 1).join(' ') + s;
    }

    // TEST
    return '\nintegerFactors(n)\n\n' + alignedTable(
      lstTest.map(integerFactors).map(function (x, i) {
        return [lstTest[i], '-->'].concat(x);
      }), 2, alignRight
    ) + '\n';

  })([25, 45, 53, 64, 100, 102, 120, 12345, 32766, 32767])
);
```


Output:


```JavaScript
integerFactors(n)

     25  -->  1   5  25
     45  -->  1   3   5    9   15    45
     53  -->  1  53
     64  -->  1   2   4    8   16    32    64
    100  -->  1   2   4    5   10    20    25     50  100
    102  -->  1   2   3    6   17    34    51    102
    120  -->  1   2   3    4    5     6     8     10   12   15   20   24    30     40     60    120
  12345  -->  1   3   5   15  823  2469  4115  12345
  32766  -->  1   2   3    6   43    86   127    129  254  258  381  762  5461  10922  16383  32766
  32767  -->  1   7  31  151  217  1057  4681  32767

```




### =ES6=



```JavaScript
(function (lstTest) {
    'use strict';

    // INTEGER FACTORS

    // integerFactors :: Int -> [Int]
    let integerFactors = (n) => {
            let rRoot = Math.sqrt(n),
                intRoot = Math.floor(rRoot),

                lows = range(1, intRoot)
                .filter(x => (n % x) === 0);

            // for perfect squares, we can drop
            // the head of the 'highs' list
            return lows.concat(lows
                .map(x => n / x)
                .reverse()
                .slice((rRoot === intRoot) | 0)
            );
        },

        // range :: Int -> Int -> [Int]
        range = (m, n) => Array.from({
            length: (n - m) + 1
        }, (_, i) => m + i);





    /*************************** TESTING *****************************/

    // TABULATION OF RESULTS IN SPACED AND ALIGNED COLUMNS
    let alignedTable = (lstRows, lngPad, fnAligned) => {
            var lstColWidths = range(
                    0, lstRows
                    .reduce(
                        (a, x) => (x.length > a ? x.length : a),
                        0
                    ) - 1
                )
                .map((iCol) => lstRows
                    .reduce((a, lst) => {
                        let w = lst[iCol] ? lst[iCol].toString()
                            .length : 0;
                        return (w > a) ? w : a;
                    }, 0));

            return lstRows.map((lstRow) =>
                    lstRow.map((v, i) => fnAligned(
                        v, lstColWidths[i] + lngPad
                    ))
                    .join('')
                )
                .join('\n');
        },

        alignRight = (n, lngWidth) => {
            let s = n.toString();
            return Array(lngWidth - s.length + 1)
                .join(' ') + s;
        };

    // TEST
    return '\nintegerFactors(n)\n\n' + alignedTable(lstTest
        .map(integerFactors)
        .map(
            (x, i) => [lstTest[i], '-->'].concat(x)
        ), 2, alignRight
    ) + '\n';

})([25, 45, 53, 64, 100, 102, 120, 12345, 32766, 32767]);
```


{{Out}}

```txt

integerFactors(n)

     25  -->  1   5  25
     45  -->  1   3   5    9   15    45
     53  -->  1  53
     64  -->  1   2   4    8   16    32    64
    100  -->  1   2   4    5   10    20    25     50  100
    102  -->  1   2   3    6   17    34    51    102
    120  -->  1   2   3    4    5     6     8     10   12   15   20   24    30     40     60    120
  12345  -->  1   3   5   15  823  2469  4115  12345
  32766  -->  1   2   3    6   43    86   127    129  254  258  381  762  5461  10922  16383  32766
  32767  -->  1   7  31  151  217  1057  4681  32767

```



## jq

{{Works with|jq|1.4}}

```jq
# This implementation uses "sort" for tidiness
def factors:
  . as $num
  | reduce range(1; 1 + sqrt|floor) as $i
      ([];
       if ($num % $i) == 0 then
         ($num / $i) as $r
         | if $i == $r then . + [$i] else . + [$i, $r] end
       else .
       end )
  | sort;

def task:
  (45, 53, 64) | "\(.): \(factors)" ;

task
```

{{Out}}
 $ jq -n -M -r -c -f factors.jq
 45: [1,3,5,9,15,45]
 53: [1,53]
 64: [1,2,4,8,16,32,64]


## Julia


```julia
using Primes

function factors(n)
    f = [one(n)]
    for (p,e) in factor(n)
        f = reduce(vcat, [f*p^j for j in 1:e], init=f)
    end
    return length(f) == 1 ? [one(n), n] : sort!(f)
end

const examples = [28, 45, 53, 64, 6435789435768]

for n in examples
    @time println("The factors of $n are: $(factors(n))")
end

```

{{out}}

```txt

The factors of 28 are: [1, 2, 4, 7, 14, 28]
  0.330684 seconds (784.75 k allocations: 39.104 MiB, 3.17% gc time)
The factors of 45 are: [1, 3, 5, 9, 15, 45]
  0.000117 seconds (56 allocations: 2.672 KiB)
The factors of 53 are: [1, 53]
  0.000102 seconds (35 allocations: 1.516 KiB)
The factors of 64 are: [1, 2, 4, 8, 16, 32, 64]
  0.000093 seconds (56 allocations: 3.172 KiB)
The factors of 6435789435768 are: [1, 2, 3, 4, 6, 7, 8, 11, 12, 14, 21, 22, 24, 28,
33, 42, 44, 56, 66, 77, 84, 88, 132, 154, 168, 191, 231, 264, 308, 382, 462, 573,
616, 764, 924, 1146, 1337, 1528, 1848, 2101, 2292, 2674, 4011, 4202, 4584, 5348,
6303, 8022, 8404, 10696, 12606, 14707, 16044, 16808, 25212, 29414, 32088, 44121,
50424, 58828, 88242, 117656, 176484, 352968, 18233351, 36466702, 54700053, 72933404,
109400106, 127633457, 145866808, 200566861, 218800212, 255266914, 382900371,
401133722, 437600424, 510533828, 601700583, 765800742, 802267444, 1021067656,
1203401166, 1403968027, 1531601484, 1604534888, 2406802332, 2807936054, 3063202968,
3482570041, 4211904081, 4813604664, 5615872108, 6965140082, 8423808162, 10447710123,
11231744216, 13930280164, 16847616324, 20895420246, 24377990287, 27860560328,
33695232648, 38308270451, 41790840492, 48755980574, 73133970861, 76616540902,
83581680984, 97511961148, 114924811353, 146267941722, 153233081804, 195023922296,
229849622706, 268157893157, 292535883444, 306466163608, 459699245412, 536315786314,
585071766888, 804473679471, 919398490824, 1072631572628, 1608947358942, 2145263145256,
3217894717884, 6435789435768]
  0.000249 seconds (451 allocations: 24.813 KiB)

```



## K


```K
 f:{i:{y[&x=y*x div y]}[x;1+!_sqrt x];?i,x div|i}
equivalent to:
q)f:{i:{y where x=y*x div y}[x ; 1+ til floor sqrt x]; distinct i,x div reverse i}

   f 120
1 2 3 4 5 6 8 10 12 15 20 24 30 40 60 120

   f 1024
1 2 4 8 16 32 64 128 256 512 1024

   f 600851475143
1 71 839 1471 6857 59569 104441 486847 1234169 5753023 10086647 87625999 408464633 716151937 8462696833 600851475143

   #f 3491888400 / has 1920 factors
1920

   / Number of factors for 3491888400 .. 3491888409
   #:'f' 3491888400+!10
1920 16 4 4 12 16 32 16 8 24
```



## Kotlin


```scala
fun printFactors(n: Int) {
    if (n < 1) return
    print("$n => ")
    (1..n / 2)
        .filter { n % it == 0 }
        .forEach { print("$it ") }
    println(n)
}

fun main(args: Array<String>) {
    val numbers = intArrayOf(11, 21, 32, 45, 67, 96)
    for (number in numbers) printFactors(number)
}
```


{{out}}

```txt

11 => 1 11
21 => 1 3 7 21
32 => 1 2 4 8 16 32
45 => 1 3 5 9 15 45
67 => 1 67
96 => 1 2 3 4 6 8 12 16 24 32 48 96

```



## LFE



### Using List Comprehensions


This following function is elegant looking and concise. However, it will not handle large numbers well: it will consume a great deal of memory (on one large number, the function consumed 4.3GB of memory on my desktop machine):

```lisp

(defun factors (n)
  (list-comp
    ((<- i (when (== 0 (rem n i))) (lists:seq 1 (trunc (/ n 2)))))
    i))

```


===Non-Stack-Consuming===

This version will not consume the stack (this function only used 18MB of memory on my machine with a ridiculously large number):

```lisp

(defun factors (n)
  "Tail-recursive prime factors function."
  (factors n 2 '()))

(defun factors
  ((1 _ acc) (++ acc '(1)))
  ((n _ acc) (when (=< n 0))
    #(error undefined))
  ((n k acc) (when (== 0 (rem n k)))
    (factors (div n k) k (cons k acc)))
  ((n k acc)
    (factors n (+ k 1) acc)))

```


{{out}}

```txt

> (factors 10677106534462215678539721403561279)
(104729 104729 104729 98731 98731 32579 29269 1)

```



## Liberty BASIC


```lb
num = 10677106534462215678539721403561279
maxnFactors = 1000
dim primeFactors(maxnFactors),  nPrimeFactors(maxnFactors)
global nDifferentPrimeNumbersFound, nFactors, iFactor


print "Start finding all factors of ";num; ":"

nDifferentPrimeNumbersFound=0
dummy = factorize(num,2)
nFactors = showPrimeFactors(num)
dim factors(nFactors)
dummy = generateFactors(1,1)
sort factors(), 0, nFactors-1
for i=1 to nFactors
   print i;"     ";factors(i-1)
next i

print "done"

wait


function factorize(iNum,offset)
    factorFound=0
    i = offset
    do
        if (iNum MOD i)=0 _
        then
            if primeFactors(nDifferentPrimeNumbersFound) = i _
            then
               nPrimeFactors(nDifferentPrimeNumbersFound) = nPrimeFactors(nDifferentPrimeNumbersFound) + 1
            else
               nDifferentPrimeNumbersFound = nDifferentPrimeNumbersFound + 1
               primeFactors(nDifferentPrimeNumbersFound) = i
               nPrimeFactors(nDifferentPrimeNumbersFound) = 1
            end if
            if iNum/i<>1 then dummy = factorize(iNum/i,i)
            factorFound=1
         end if
         i=i+1
    loop while factorFound=0 and i<=sqr(iNum)
    if factorFound=0 _
    then
       nDifferentPrimeNumbersFound = nDifferentPrimeNumbersFound + 1
       primeFactors(nDifferentPrimeNumbersFound) = iNum
       nPrimeFactors(nDifferentPrimeNumbersFound) = 1
    end if
end function


function showPrimeFactors(iNum)
   showPrimeFactors=1
   print iNum;" = ";
   for i=1 to nDifferentPrimeNumbersFound
      print primeFactors(i);"^";nPrimeFactors(i);
      if i<nDifferentPrimeNumbersFound then print " * "; else print ""
      showPrimeFactors = showPrimeFactors*(nPrimeFactors(i)+1)
   next i
   end function


function generateFactors(product,pIndex)
   if pIndex>nDifferentPrimeNumbersFound _
   then
      factors(iFactor) = product
      iFactor=iFactor+1
   else
      for i=0 to nPrimeFactors(pIndex)
         dummy = generateFactors(product*primeFactors(pIndex)^i,pIndex+1)
      next i
   end if
   end function
```


{{out}}

```lb
Start finding all factors of 10677106534462215678539721403561279:
10677106534462215678539721403561279 = 29269^1 * 32579^1 * 98731^2 * 104729^3
1 1
2 29269
3 32579
4 98731
5 104729
6 953554751
7 2889757639
8 3065313101
9 3216557249
10 3411966091
11 9747810361
12 10339998899
13 10968163441
14 94145414120981
15 99864835517479
16 285308661456109
17 302641427774831
18 317573913751019
19 321027175754629
20 336866824130521
21 357331796744339
22 1020878431297169
23 1082897744693371
24 1148684789012489
25 9295070881578575111
26 9859755075476219149
27 10458744358910058191
28 29880090805636839461
29 31695334089430275799
30 33259198413230468851
31 33620855089606540541
32 35279725624365333809
33 37423001741237879131
34 106915577231321212201
35 113410797903992051459
36 973463478356842592799919
37 1032602289299548955255621
38 1095333837964291484285239
39 3129312029983540559911069
40 3319420643851943354153471
41 3483202590619213772296379
42 3694810384914157044482761
43 11197161487859039232598529
44 101949856624833767901342716951
45 108143405156052462534965931709
46 327729719588146219298926345301
47 364792324112959639158827476291
48 10677106534462215678539721403561279
done
```



### A Simpler Approach

This is a somewhat simpler approach for finding the factors of smaller numbers (less than one million).


```lb

print "ROSETTA CODE - Factors of an integer"
'A simpler approach for smaller numbers
[Start]
print
input "Enter an integer (< 1,000,000): "; n
n=abs(int(n)): if n=0 then goto [Quit]
if n>999999 then goto [Start]
FactorCount=FactorCount(n)
select case FactorCount
    case 1: print "The factor of 1 is: 1"
    case else
        print "The "; FactorCount; " factors of "; n; " are: ";
        for x=1 to FactorCount
            print " "; Factor(x);
        next x
        if FactorCount=2 then print " (Prime)" else print
end select
goto [Start]

[Quit]
print "Program complete."
end

function FactorCount(n)
    dim Factor(100)
    for y=1 to n
        if y>sqr(n) and FactorCount=1 then
'If no second factor is found by the square root of n, then n is prime.
            FactorCount=2: Factor(FactorCount)=n: exit function
        end if
        if (n mod y)=0 then
            FactorCount=FactorCount+1
            Factor(FactorCount)=y
        end if
    next y
end function

```


{{out}}

```txt

ROSETTA CODE - Factors of an integer

Enter an integer (< 1,000,000): 1
The factor of 1 is: 1

Enter an integer (< 1,000,000): 2
The 2 factors of 2 are:  1 2 (Prime)

Enter an integer (< 1,000,000): 4
The 3 factors of 4 are:  1 2 4

Enter an integer (< 1,000,000): 6
The 4 factors of 6 are:  1 2 3 6

Enter an integer (< 1,000,000): 999999
The 64 factors of 999999 are:  1 3 7 9 11 13 21 27 33 37 39 63 77 91 99 111 117 143 189 231 259 273 297 333 351 407 429 481 693 777 819 999 1001 1221 1287 1443 2079 2331 2457 2849 3003 3367 3663 3861 4329 5291 6993 8547 9009 10101 10989 129
87 15873 25641 27027 30303 37037 47619 76923 90909 111111 142857 333333 999999

Enter an integer (< 1,000,000):
Program complete.

```



## Lingo


```lingo
on factors(n)
  res = [1]
  repeat with i = 2 to n/2
    if n mod i = 0 then res.add(i)
  end repeat
  res.add(n)
  return res
end
```


```lingo
put factors(45)
-- [1, 3, 5, 9, 15, 45]
put factors(53)
-- [1, 53]
put factors(64)
-- [1, 2, 4, 8, 16, 32, 64]
```



## Logo


```logo
to factors :n
  output filter [equal? 0 modulo :n ?] iseq 1 :n
end

show factors 28       ; [1 2 4 7 14 28]
```



## Lua


```lua
function Factors( n )
    local f = {}

    for i = 1, n/2 do
        if n % i == 0 then
            f[#f+1] = i
        end
    end
    f[#f+1] = n

    return f
end
```


## M2000 Interpreter


```M2000 Interpreter

\\ Factors of an integer
\\ For act as BASIC's FOR (if N<1 no loop start)
FORM 60,40
SET SWITCHES "+FOR"
MODULE LikeBasic {
      10 INPUT N%
      20 FOR I%=1 TO N%
      30 IF N%/I%=INT(N%/I%) THEN PRINT I%,
      40 NEXT I%
      50 PRINT
}
CALL LikeBasic
SET SWITCHES "-FOR"
MODULE LikeM2000 {
      DEF DECIMAL N%, I%
      INPUT N%
      IF N%<1 THEN EXIT
      FOR I%=1 TO N% {
          IF N% MOD I%=0 THEN PRINT I%,
      }
      PRINT
}
CALL LikeM2000


```



## Maple



```Maple

numtheory:-divisors(n);

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
Factorize[n_Integer] := Divisors[n]
```


=={{header|MATLAB}} / {{header|Octave}}==

```Matlab
  function fact(n);
    f = factor(n);	% prime decomposition
    K = dec2bin(0:2^length(f)-1)-'0';   % generate all possible permutations
    F = ones(1,2^length(f));
    for k = 1:size(K)
      F(k) = prod(f(~K(k,:)));		% and compute products
    end;
    F = unique(F);	% eliminate duplicates
    printf('There are %i factors for %i.\n',length(F),n);
    disp(F);
  end;

```


{{out}}

```txt

>> fact(12)
There are 6 factors for 12.
    1    2    3    4    6   12
>> fact(28)
There are 6 factors for 28.
    1    2    4    7   14   28
>> fact(64)
There are 7 factors for 64.
    1    2    4    8   16   32   64
>>fact(53)
There are 2 factors for 53.
    1   53

```



## Maxima

The builtin <code>divisors</code> function does this.

```maxima
(%i96) divisors(100);
(%o96) {1,2,4,5,10,20,25,50,100}
```


Such a function could be implemented like so:

```maxima
divisors2(n) := map( lambda([l], lreduce("*", l)),
    apply( cartesian_product,
    map( lambda([fac],
        setify(makelist(fac[1]^i, i, 0, fac[2]))),
    ifactors(n))));
```


## MAXScript


```MAXScript

fn factors n =
(
	return (for i = 1 to n+1 where mod n i == 0 collect i)
)

```


{{out}}

```MAXScript

factors 3
#(1, 3)
factors 7
#(1, 7)
factors 14
#(1, 2, 7, 14)
factors 60
#(1, 2, 3, 4, 5, 6, 10, 12, 15, 20, 30, 60)
factors 54
#(1, 2, 3, 6, 9, 18, 27, 54)

```



## Mercury

Mercury is both a logic language and a functional language. As such there are two possible interfaces for calculating the factors of an integer. This code shows both styles of implementation. Note that much of the code here is ceremony put in place to have this be something which can actually compile. The actual factoring is contained in the predicate <code>factor/2</code> and in the function <code>factor/1</code>.  The function form is implemented in terms of the predicate form rather than duplicating all of the predicate code.

The predicates main/2 and factor/2 are shown with the combined type and mode statement (e.g. int::in) as is the usual case for simple predicates with only one mode.  This makes the code more immediately understandable.  The predicate factor/5, however, has its mode broken out onto a separate line both to show Mercury's mode statement (useful for predicates which can have varying instantiation of parameters) and to stop the code from extending too far to the right.  Finally the function factor/1 has its mode statements removed (shown underneath in a comment for illustration purposes) because good coding style (and the default of the compiler!) has all parameters "in"-moded and the return value "out"-moded.

This implementation of factoring works as follows:
# The input number itself and 1 are both considered factors.
# The numbers between 2 and the square root of the input number are checked for even division.
# If the incremental number divides evenly into the input number, both the incremental number and the quotient are added to the list of factors.

This implementation makes use of Mercury's "state variable notation" to keep a pair of variables for accumulation, thus allowing the implementation to be tail recursive.  !Accumulator is syntax sugar for a *pair* of variables.  One of them is an "in"-moded variable and the other is an "out"-moded variable.  !:Accumulator is the "out" portion and !.Accumulator is the "in" portion in the ensuing code.

Using the state variable notation avoids having to keep track of strings of variables unified in the code named things like Acc0, Acc1, Acc2, Acc3, etc.


### fac.m


```Mercury
:- module fac.

:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module float, int, list, math, string.

main(!IO) :-
    io.command_line_arguments(Args, !IO),
    list.filter_map(string.to_int, Args, CleanArgs),
    list.foldl((pred(Arg::in, !.IO::di, !:IO::uo) is det :-
                    factor(Arg, X),
                    io.format("factor(%d, [", [i(Arg)], !IO),
                    io.write_list(X, ",", io.write_int, !IO),
                    io.write_string("])\n", !IO)
               ), CleanArgs, !IO).

:- pred factor(int::in, list(int)::out) is det.
factor(N, Factors) :-
    Limit = float.truncate_to_int(math.sqrt(float(N))),
	factor(N, 2, Limit, [], Unsorted),
    list.sort_and_remove_dups([1, N | Unsorted], Factors).

:- pred factor(int, int, int, list(int), list(int)).
:- mode factor(in,  in,  in,  in,        out) is det.
factor(N, X, Limit, !Accumulator) :-
    ( if X  > Limit
          then true
          else ( if 0 = N mod X
                     then !:Accumulator = [X, N / X | !.Accumulator]
                     else true ),
               factor(N, X + 1, Limit, !Accumulator) ).

:- func factor(int) = list(int).
%:- mode factor(in) = out is det.
factor(N) = Factors :- factor(N, Factors).

:- end_module fac.
```



### Use and output

Use of the code looks like this:


```txt
<nowiki>$ mmc fac.m && ./fac 100 999 12345678 booger
factor(100, [1,2,4,5,10,20,25,50,100])
factor(999, [1,3,9,27,37,111,333,999])
factor(12345678, [1,2,3,6,9,18,47,94,141,282,423,846,14593,29186,43779,87558,131337,262674,685871,1371742,2057613,4115226,6172839,12345678])</nowiki>
```


=={{header|MK-61/52}}==


```txt

П9	1	П6	КИП6	ИП9	ИП6	/	П8	^	[x]
x#0	21	-	x=0	03	ИП6	С/П	ИП8	П9	БП
04	1	С/П	БП	21

```



## MUMPS


```MUMPS
factors(num)	New fctr,list,sep,sqrt
	If num<1 Quit "Too small a number"
	If num["." Quit "Not an integer"
	Set sqrt=num**0.5\1
	For fctr=1:1:sqrt Set:num/fctr'["." list(fctr)=1,list(num/fctr)=1
	Set (list,fctr)="",sep="[" For  Set fctr=$Order(list(fctr)) Quit:fctr=""  Set list=list_sep_fctr,sep=","
	Quit list_"]"

w $$factors(45) ; [1,3,5,9,15,45]
w $$factors(53) ; [1,53]
w $$factors(64) ; [1,2,4,8,16,32,64]
```



## NetRexx

{{trans|REXX}}

```NetRexx
/* NetRexx ***********************************************************
* 21.04.2013 Walter Pachl
* 21.04.2013 add method main to accept argument(s)
*********************************************************************/
options replace format comments java crossref symbols nobinary
class divl
  method main(argwords=String[]) static
    arg=Rexx(argwords)
    Parse arg a b
    Say a b
    If a='' Then Do
      help='java divl low [high] shows'
      help=help||' divisors of all numbers between low and high'
      Say help
      Return
      End
    If b='' Then b=a
    loop x=a To b
      say x '->' divs(x)
      End

method divs(x) public static returns Rexx
  if x==1 then return 1               /*handle special case of 1     */
  lo=1
  hi=x
  odd=x//2                            /* 1 if x is odd               */
  loop j=2+odd By 1+odd While j*j<x   /*divide by numbers<sqrt(x)    */
    if x//j==0 then Do                /*Divisible?  Add two divisors:*/
      lo=lo j                         /* list low divisors           */
      hi=x%j hi                       /* list high divisors          */
      End
    End
  If j*j=x Then                       /*for a square number as input */
    lo=lo j                           /* add its square root         */
  return lo hi                        /* return both lists           */
```


{{out}}

```txt
java divl 1 10
1 -> 1
2 -> 1 2
3 -> 1 3
4 -> 1 2 4
5 -> 1 5
6 -> 1 2 3 6
7 -> 1 7
8 -> 1 2 4 8
9 -> 1 3 9
10 -> 1 2 5 10
```



## Nim


```nim
import intsets, math, algorithm

proc factors(n): seq[int] =
  var fs = initIntSet()
  for x in 1 .. int(sqrt(float(n))):
    if n mod x == 0:
      fs.incl(x)
      fs.incl(n div x)

  result = @[]
  for x in fs:
    result.add(x)
  sort(result, system.cmp[int])

echo factors(45)
```



## Niue


```Niue
[ 'n ; [ negative-or-zero [ , ] if
       [ n not-factor [ , ] when ] else ] n times n ] 'factors ;

[ dup 0 <= ] 'negative-or-zero ;
[ swap dup rot swap mod 0 = not ] 'not-factor ;

( tests )
100 factors .s .clr ( => 1 2 4 5 10 20 25 50 100 ) newline
53 factors .s .clr ( => 1 53 ) newline
64 factors .s .clr ( => 1 2 4 8 16 32 64 ) newline
12 factors .s .clr ( => 1 2 3 4 6 12 )
```


=={{header|Oberon-2}}==
Oxford Oberon-2

```oberon2

MODULE Factors;
IMPORT Out,SYSTEM;
TYPE
	LIPool = POINTER TO ARRAY OF LONGINT;
	LIVector= POINTER TO LIVectorDesc;
	LIVectorDesc = RECORD
		cap: INTEGER;
		len: INTEGER;
		LIPool: LIPool;
	END;

	PROCEDURE New(cap: INTEGER): LIVector;
	VAR
		v: LIVector;
	BEGIN
		NEW(v);
		v.cap := cap;
		v.len := 0;
		NEW(v.LIPool,cap);
		RETURN v
	END New;

	PROCEDURE (v: LIVector) Add(x: LONGINT);
	VAR
		newLIPool: LIPool;
	BEGIN
		IF v.len = LEN(v.LIPool^) THEN
			(* run out of space *)
			v.cap := v.cap + (v.cap DIV 2);
			NEW(newLIPool,v.cap);
			SYSTEM.MOVE(SYSTEM.ADR(v.LIPool^),SYSTEM.ADR(newLIPool^),v.cap * SIZE(LONGINT));
			v.LIPool := newLIPool
		END;
		v.LIPool[v.len] := x;
		INC(v.len)
	END Add;

	PROCEDURE (v: LIVector) At(idx: INTEGER): LONGINT;
	BEGIN
		RETURN v.LIPool[idx];
	END At;


PROCEDURE Factors(n:LONGINT): LIVector;
VAR
	j: LONGINT;
	v: LIVector;
BEGIN
	v := New(16);
	FOR j := 1 TO n DO
		IF (n MOD j) = 0 THEN v.Add(j) END;
	END;
	RETURN v
END Factors;

VAR
	v: LIVector;
	j: INTEGER;
BEGIN
	v := Factors(123);
	FOR j := 0 TO v.len - 1 DO
		Out.LongInt(v.At(j),4);Out.Ln
	END;
	Out.Int(v.len,6);Out.String(" factors");Out.Ln
END Factors.

```

{{out}}

```txt

   1
   3
  41
 123
     4 factors

```


## Objeck


```objeck
use IO;
use Structure;

bundle Default {
  class Basic {
    function : native : GenerateFactors(n : Int)  ~ IntVector {
      factors := IntVector->New();
      factors-> AddBack(1);
      factors->AddBack(n);

      for(i := 2; i * i <= n; i += 1;) {
        if(n % i = 0) {
          factors->AddBack(i);
          if(i * i <> n) {
            factors->AddBack(n / i);
          };
        };
      };
      factors->Sort();


      return factors;
    }

    function : Main(args : String[]) ~ Nil {
      numbers := [3135, 45, 60, 81];
      for(i := 0; i < numbers->Size(); i += 1;) {
        factors := GenerateFactors(numbers[i]);

        Console->GetInstance()->Print("Factors of ")->Print(numbers[i])->PrintLine(" are:");
        each(i : factors) {
          Console->GetInstance()->Print(factors->Get(i))->Print(", ");
        };
        "\n\n"->Print();
      };
    }
  }
}
```



## OCaml


```ocaml
let rec range = function 0 -> [] | n -> range(n-1) @ [n]

let factors n =
  List.filter (fun v -> (n mod v) = 0) (range n)
```



## Oforth



```Oforth
Integer method: factors  self seq filter(#[ self isMultiple ]) ;

120 factors println
```


{{out}}

```txt

[1, 2, 3, 4, 5, 6, 8, 10, 12, 15, 20, 24, 30, 40, 60, 120]

```



## Oz


```oz
declare
  fun {Factors N}
     Sqr = {Float.toInt {Sqrt {Int.toFloat N}}}

     Fs = for X in 1..Sqr append:App do
             if N mod X == 0 then
                CoFactor = N div X
             in
                if CoFactor == X then %% avoid duplicate factor
                   {App [X]}          %% when N is a square number
                else
                   {App [X CoFactor]}
                end
             end
          end
  in
     {Sort Fs Value.'<'}
  end
in
  {Show {Factors 53}}
```



## PARI/GP


```parigp
divisors(n)
```



## Panda

Panda has a factor function already, it's defined as:

```panda
fun factor(n) type integer->integer
   f where n.mod(1..n=>f)==0

45.factor
```



## Pascal

{{trans|Fortran}}
{{works with|Free Pascal|2.6.2}}

```pascal
program Factors;
var
  i, number: integer;
begin
  write('Enter a number between 1 and 2147483647: ');
  readln(number);

  for i := 1 to round(sqrt(number)) - 1 do
    if number mod i = 0 then
      write (i, ' ',  number div i, ' ');

  // Check to see if number is a square
  i := round(sqrt(number));
  if i*i = number then
     write(i)
  else if number mod i = 0 then
     write(i, number/i);
  writeln;
end.
```

{{out}}

```txt

Enter a number between 1 and 2147483647: 49
1 49 7

Enter a number between 1 and 2147483647: 353435
1 25755 3 8585 5 5151 15 1717 17 1515 51 505 85 303 101 255


```


### small improvement

the factors are in ascending order.
{{works with|Free Pascal}}

```pascal
program factors;
{Looking for extreme composite numbers:
http://wwwhomes.uni-bielefeld.de/achim/highly.txt}

const
  MAXFACTORCNT = 1920; //number := 3491888400;

var
  FaktorList : array[0..MAXFACTORCNT] of LongWord;
  i, number,quot,cnt: LongWord;
begin
  writeln('Enter a number between 1 and 4294967295: ');
  write('3491888400 is a nice choice ');
  readln(number);

  cnt := 0;
  i := 1;
  repeat
    quot := number div i;
    if quot *i-number = 0 then
    begin
      FaktorList[cnt] := i;
      FaktorList[MAXFACTORCNT-cnt] := quot;
      inc(cnt);
    end;
    inc(i);
  until i> quot;
  writeln(number,' has ',2*cnt,' factors');
  dec(cnt);
  For i := 0 to cnt do
    write(FaktorList[i],' ,');
  For i := cnt downto 1 do
    write(FaktorList[MAXFACTORCNT-i],' ,');
{ the last without ','}
  writeln(FaktorList[MAXFACTORCNT]);
end.
```


{{out}}

```txt
Enter a number between 1 and 4294967295:
3491888400 is a nice choice 120
120 has 16 factors
1 ,2 ,3 ,4 ,5 ,6 ,8 ,10 ,12 ,15 ,20 ,24 ,30 ,40 ,60 ,120
```



## Perl


```perl
sub factors
{
        my($n) = @_;
        return grep { $n % $_ == 0 }(1 .. $n);
}
print join ' ',factors(64), "\n";
```


Or more intelligently:


```perl
sub factors {
  my $n = shift;
  $n = -$n if $n < 0;
  my @divisors;
  for (1 .. int(sqrt($n))) {  # faster and less memory than map/grep
    push @divisors, $_ unless $n % $_;
  }
  # Return divisors including top half, without duplicating a square
  @divisors, map { $_*$_ == $n ? () : int($n/$_) } reverse @divisors;
}
print join " ", factors(64), "\n";
```


One could also use a module, e.g.:
{{libheader|ntheory}}

```perl
use ntheory qw/divisors/;
print join " ", divisors(12345678), "\n";
# Alternately something like:  fordivisors { say } 12345678;
```



## Perl 6

{{works with|Rakudo|2015.12}}

```perl6
sub factors (Int $n) { squish sort ($_, $n div $_ if $n %% $_ for 1 .. sqrt $n) }
```



## Phix

There is a builtin factors(n), which takes an optional second parameter to include 1 and n, so eg ?factors(12345,1) displays
{{out}}

```txt

{1,3,5,15,823,2469,4115,12345}

```

You can find the implementation of factors() and prime_factors() in builtins\pfactors.e


## PHP


```PHP
function GetFactors($n){
   $factors = array(1, $n);
   for($i = 2; $i * $i <= $n; $i++){
      if($n % $i == 0){
         $factors[] = $i;
         if($i * $i != $n)
            $factors[] = $n/$i;
      }
   }
   sort($factors);
   return $factors;
}
```



## PicoLisp


```PicoLisp
(de factors (N)
   (filter
      '((D) (=0 (% N D)))
      (range 1 N) ) )
```



## PILOT


```pilot
T  :Enter a number.
A  :#n
C  :factor = 1
T  :The factors of #n are:
*Loop
C  :remainder = n % factor
T ( remainder = 0 )  :#factor
J ( factor = n )     :*Finished
C  :factor = factor + 1
J  :*Loop
*Finished
END:
```



## PL/I


```PL/I
do i = 1 to n;
   if mod(n, i) = 0 then put skip list (i);
end;
```



## PowerShell


### Straightforward but slow


```powershell
function Get-Factor ($a) {
    1..$a | Where-Object { $a % $_ -eq 0 }
}
```

This one uses a range of integers up to the target number and just filters it using the <code>Where-Object</code> cmdlet. It's very slow though, so it is not very usable for larger numbers.

### A little more clever


```powershell
function Get-Factor ($a) {
    1..[Math]::Sqrt($a) `
        | Where-Object { $a % $_ -eq 0 } `
        | ForEach-Object { $_; $a / $_ } `
        | Sort-Object -Unique
}
```

Here the range of integers is only taken up to the square root of the number, the same filtering applies. Afterwards the corresponding larger factors are calculated and sent down the pipeline along with the small ones found earlier.


## ProDOS

Uses the math module:

```ProDOS
editvar /newvar /value=a /userinput=1 /title=Enter an integer:
do /delimspaces %% -a- >b
printline Factors of -a-: -b-
```



## Prolog


'''Simple Brute Force Implementation'''

```Prolog

brute_force_factors( N , Fs ) :-
  integer(N) ,
  N > 0 ,
  setof( F , ( between(1,N,F) , N mod F =:= 0 ) , Fs )
  .

```


'''A Slightly Smarter Implementation'''

```Prolog

smart_factors(N,Fs) :-
  integer(N) ,
  N > 0 ,
  setof( F , factor(N,F) , Fs )
  .

factor(N,F) :-
  L is floor(sqrt(N)) ,
  between(1,L,X) ,
  0 =:= N mod X ,
  ( F = X ; F is N // X )
  .

```


Not every Prolog has <code>between/3</code>: you might need this:


```Prolog


between(X,Y,Z) :-
  integer(X) ,
  integer(Y) ,
  X =< Z ,
  between1(X,Y,Z)
  .

between1(X,Y,X) :-
  X =< Y
  .
between1(X,Y,Z) :-
  X < Y ,
  X1 is X+1 ,
  between1(X1,Y,Z)
  .

```


{{out}}

```txt

?- N=36 ,( brute_force_factors(N,Factors) ; smart_factors(N,Factors) ).
N = 36, Factors = [1, 2, 3, 4, 6, 9, 12, 18, 36] ;
N = 36, Factors = [1, 2, 3, 4, 6, 9, 12, 18, 36] .

?- N=53,( brute_force_factors(N,Factors) ; smart_factors(N,Factors) ).
N = 53, Factors = [1, 53] ;
N = 53, Factors = [1, 53] .

?- N=100,( brute_force_factors(N,Factors);smart_factors(N,Factors) ).
N = 100, Factors = [1, 2, 4, 5, 10, 20, 25, 50, 100] ;
N = 100, Factors = [1, 2, 4, 5, 10, 20, 25, 50, 100] .

?- N=144,( brute_force_factors(N,Factors);smart_factors(N,Factors) ).
N = 144, Factors = [1, 2, 3, 4, 6, 8, 9, 12, 16, 18, 24, 36, 48, 72, 144] ;
N = 144, Factors = [1, 2, 3, 4, 6, 8, 9, 12, 16, 18, 24, 36, 48, 72, 144] .

?- N=32765,( brute_force_factors(N,Factors);smart_factors(N,Factors) ).
N = 32765, Factors = [1, 5, 6553, 32765] ;
N = 32765, Factors = [1, 5, 6553, 32765] .

?- N=32766,( brute_force_factors(N,Factors);smart_factors(N,Factors) ).
N = 32766, Factors = [1, 2, 3, 6, 43, 86, 127, 129, 254, 258, 381, 762, 5461, 10922, 16383, 32766] ;
N = 32766, Factors = [1, 2, 3, 6, 43, 86, 127, 129, 254, 258, 381, 762, 5461, 10922, 16383, 32766] .

38 ?- N=32767,( brute_force_factors(N,Factors);smart_factors(N,Factors) ).
N = 32767, Factors = [1, 7, 31, 151, 217, 1057, 4681, 32767] ;
N = 32767, Factors = [1, 7, 31, 151, 217, 1057, 4681, 32767] .

```



## PureBasic


```PureBasic
Procedure PrintFactors(n)
  Protected i, lim=Round(sqr(n),#PB_Round_Up)
  NewList F.i()
  For i=1 To lim
    If n%i=0
      AddElement(F()): F()=i
      AddElement(F()): F()=n/i
    EndIf
  Next
  ;- Present the result
  SortList(F(),#PB_Sort_Ascending)
  ForEach F()
    Print(str(F())+" ")
  Next
EndProcedure

If OpenConsole()
  Print("Enter integer to factorize: ")
  PrintFactors(Val(Input()))
  Print(#CRLF$+#CRLF$+"Press ENTER to quit."): Input()
EndIf
```


{{out}}

```txt

 Enter integer to factorize: 96
 1 2 3 4 6 8 12 16 24 32 48 96

```



## Python

Naive and slow but simplest (check all numbers from 1 to n):

```python>>>
 def factors(n):
      return [i for i in range(1, n + 1) if not n%i]
```


Slightly better (realize that there are no factors between n/2 and n):

```python>>>
 def factors(n):
      return [i for i in range(1, n//2 + 1) if not n%i] + [n]

>>> factors(45)
[1, 3, 5, 9, 15, 45]
```


Much better (realize that factors come in pairs, the smaller of which is no bigger than sqrt(n)):

```python>>>
 from math import sqrt
>>> def factor(n):
      factors = set()
      for x in range(1, int(sqrt(n)) + 1):
        if n % x == 0:
          factors.add(x)
          factors.add(n//x)
      return sorted(factors)

>>> for i in (45, 53, 64): print( "%i: factors: %s" % (i, factor(i)) )

45: factors: [1, 3, 5, 9, 15, 45]
53: factors: [1, 53]
64: factors: [1, 2, 4, 8, 16, 32, 64]
```


More efficient when factoring many numbers:

```python
from itertools import chain, cycle, accumulate # last of which is Python 3 only

def factors(n):
    def prime_powers(n):
        # c goes through 2, 3, 5, then the infinite (6n+1, 6n+5) series
        for c in accumulate(chain([2, 1, 2], cycle([2,4]))):
            if c*c > n: break
            if n%c: continue
            d,p = (), c
            while not n%c:
                n,p,d = n//c, p*c, d + (p,)
            yield(d)
        if n > 1: yield((n,))

    r = [1]
    for e in prime_powers(n):
        r += [a*b for a in r for b in e]
    return r
```



## R


```R
factors <- function(n)
{
   if(length(n) > 1)
   {
      lapply(as.list(n), factors)
   } else
   {
      one.to.n <- seq_len(n)
      one.to.n[(n %% one.to.n) == 0]
   }
}
factors(60)
```

 1  2  3  4  5  6 10 12 15 20 30 60

```R
factors(c(45, 53, 64))
```


```txt

[[1]]
[1]  1  3  5  9 15 45
[[2]]
[1]  1 53
[[3]]
[1]  1  2  4  8 16 32 64

```



## Racket



```Racket

#lang racket

;; a naive version
(define (naive-factors n)
  (for/list ([i (in-range 1 (add1 n))] #:when (zero? (modulo n i))) i))
(naive-factors 120) ; -> '(1 2 3 4 5 6 8 10 12 15 20 24 30 40 60 120)

;; much better: use `factorize' to get prime factors and construct the
;; list of results from that
(require math)
(define (factors n)
  (sort (for/fold ([l '(1)]) ([p (factorize n)])
          (append (for*/list ([e (in-range 1 (add1 (cadr p)))] [x l])
                    (* x (expt (car p) e)))
                  l))
        <))
(naive-factors 120) ; -> same

;; to see how fast it is:
(define huge 1200034005600070000008900000000000000000)
(time (length (factors  huge)))
;; I get 42ms for getting a list of 7776 numbers

;; but actually the math library comes with a `divisors' function that
;; does the same, except even faster
(divisors 120) ; -> same

(time (length (divisors huge)))
;; And this one clocks at 17ms

```



## REALbasic


```vb
Function factors(num As UInt64) As UInt64()
  'This function accepts an unsigned 64 bit integer as input and returns an array of unsigned 64 bit integers
  Dim result() As UInt64
  Dim iFactor As UInt64 = 1
  While iFactor <= num/2 'Since a factor will never be larger than half of the number
    If num Mod iFactor = 0 Then
      result.Append(iFactor)
    End If
    iFactor = iFactor + 1
  Wend
  result.Append(num) 'Since a given number is always a factor of itself
  Return result
End Function
```



## REXX


### optimized version

This REXX version has no effective limits on the number of decimal digits in the number to be factored   [by adjusting the number of digits (precision)].

This REXX version also supports negative integers and zero.

It also indicates   '''primes'''   in the output listing as well as the number of factors.

It also displays a final count of the number of primes found.

```rexx
/*REXX program  displays  divisors  of any [negative/zero/positive]  integer or a range.*/
parse arg LO HI inc .                                         /*obtain the optional args*/
HI=word(HI LO 20, 1);  LO=word(LO 1, 1);  inc=word(inc 1, 1)  /*define the range options*/
w=length(high)+2;      numeric digits max(9, w-2);    $='∞'   /*decimal digits for  //  */
@.=left('',7);  @.1="{unity}"; @.2='[prime]'; @.$="  {"$'}  ' /*define some literals.   */
say center('n', w)    "#divisors"    center('divisors', 60)   /*display the  header.    */
say copies('═', w)    "═════════"    copies('═'       , 60)   /*   "     "   separator. */
p#=0                                                          /*count of prime numbers. */
     do n=LO  to HI  by inc; divs=divisors(n); #=words(divs)  /*get list of divs; # divs*/
     if divs==$  then do;  #=$ ; divs= '  (infinite)';  end   /*handle case for infinity*/
     p=@.#;      if n<0  then if n\==-1  then p=@..           /*   "     "   "  negative*/
     if p==@.2  then p#=p#+1                                  /*Prime? Then bump counter*/
     say center(n, w)      center('['#"]", 9)       "──► "        p      ' '       divs
     end   /*n*/                                 /* [↑]   process a range of integers.  */
say
say left('', 17)     p#    ' primes were found.' /*display the number of primes found.  */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
divisors: procedure; parse arg x 1 b;     a=1    /*set  X  and  B  to the 1st argument. */
if x<2  then do; x=abs(x);  if x==1  then return 1;  if x==0  then return '∞';  b=x;  end
odd=x//2                                         /* [↓]  process EVEN or ODD ints.   ___*/
        do j=2+odd  by 1+odd  while j*j<x        /*divide by all the integers up to √ x */
        if x//j==0  then do; a=a j; b=x%j b; end /*÷?  Add factors to  α  and  ß  lists.*/
        end   /*j*/                              /* [↑]  %  ≡  integer division.     ___*/
if j*j==x  then  return  a j b                   /*Was  X  a square?   Then insert  √ x */
                 return  a   b                   /*return the divisors of both lists.   */
```

'''output'''   when the input used is:   <tt> -6   200 </tt>
<pre style="height:65ex">
  n    #divisors                           divisors
══════ ═════════ ════════════════════════════════════════════════════════════
  -6      [4]    ──►            1 2 3 6
  -5      [2]    ──►            1 5
  -4      [3]    ──►            1 2 4
  -3      [2]    ──►            1 3
  -2      [2]    ──►            1 2
  -1      [1]    ──►  {unity}   1
  0       [∞]    ──►    {∞}       (infinite)
  1       [1]    ──►  {unity}   1
  2       [2]    ──►  [prime]   1 2
  3       [2]    ──►  [prime]   1 3
  4       [3]    ──►            1 2 4
  5       [2]    ──►  [prime]   1 5
  6       [4]    ──►            1 2 3 6
  7       [2]    ──►  [prime]   1 7
  8       [4]    ──►            1 2 4 8
  9       [3]    ──►            1 3 9
  10      [4]    ──►            1 2 5 10
  11      [2]    ──►  [prime]   1 11
  12      [6]    ──►            1 2 3 4 6 12
  13      [2]    ──►  [prime]   1 13
  14      [4]    ──►            1 2 7 14
  15      [4]    ──►            1 3 5 15
  16      [5]    ──►            1 2 4 8 16
  17      [2]    ──►  [prime]   1 17
  18      [6]    ──►            1 2 3 6 9 18
  19      [2]    ──►  [prime]   1 19
  20      [6]    ──►            1 2 4 5 10 20
  21      [4]    ──►            1 3 7 21
  22      [4]    ──►            1 2 11 22
  23      [2]    ──►  [prime]   1 23
  24      [8]    ──►            1 2 3 4 6 8 12 24
  25      [3]    ──►            1 5 25
  26      [4]    ──►            1 2 13 26
  27      [4]    ──►            1 3 9 27
  28      [6]    ──►            1 2 4 7 14 28
  29      [2]    ──►  [prime]   1 29
  30      [8]    ──►            1 2 3 5 6 10 15 30
  31      [2]    ──►  [prime]   1 31
  32      [6]    ──►            1 2 4 8 16 32
  33      [4]    ──►            1 3 11 33
  34      [4]    ──►            1 2 17 34
  35      [4]    ──►            1 5 7 35
  36      [9]    ──►            1 2 3 4 6 9 12 18 36
  37      [2]    ──►  [prime]   1 37
  38      [4]    ──►            1 2 19 38
  39      [4]    ──►            1 3 13 39
  40      [8]    ──►            1 2 4 5 8 10 20 40
  41      [2]    ──►  [prime]   1 41
  42      [8]    ──►            1 2 3 6 7 14 21 42
  43      [2]    ──►  [prime]   1 43
  44      [6]    ──►            1 2 4 11 22 44
  45      [6]    ──►            1 3 5 9 15 45
  46      [4]    ──►            1 2 23 46
  47      [2]    ──►  [prime]   1 47
  48     [10]    ──►            1 2 3 4 6 8 12 16 24 48
  49      [3]    ──►            1 7 49
  50      [6]    ──►            1 2 5 10 25 50
  51      [4]    ──►            1 3 17 51
  52      [6]    ──►            1 2 4 13 26 52
  53      [2]    ──►  [prime]   1 53
  54      [8]    ──►            1 2 3 6 9 18 27 54
  55      [4]    ──►            1 5 11 55
  56      [8]    ──►            1 2 4 7 8 14 28 56
  57      [4]    ──►            1 3 19 57
  58      [4]    ──►            1 2 29 58
  59      [2]    ──►  [prime]   1 59
  60     [12]    ──►            1 2 3 4 5 6 10 12 15 20 30 60
  61      [2]    ──►  [prime]   1 61
  62      [4]    ──►            1 2 31 62
  63      [6]    ──►            1 3 7 9 21 63
  64      [7]    ──►            1 2 4 8 16 32 64
  65      [4]    ──►            1 5 13 65
  66      [8]    ──►            1 2 3 6 11 22 33 66
  67      [2]    ──►  [prime]   1 67
  68      [6]    ──►            1 2 4 17 34 68
  69      [4]    ──►            1 3 23 69
  70      [8]    ──►            1 2 5 7 10 14 35 70
  71      [2]    ──►  [prime]   1 71
  72     [12]    ──►            1 2 3 4 6 8 9 12 18 24 36 72
  73      [2]    ──►  [prime]   1 73
  74      [4]    ──►            1 2 37 74
  75      [6]    ──►            1 3 5 15 25 75
  76      [6]    ──►            1 2 4 19 38 76
  77      [4]    ──►            1 7 11 77
  78      [8]    ──►            1 2 3 6 13 26 39 78
  79      [2]    ──►  [prime]   1 79
  80     [10]    ──►            1 2 4 5 8 10 16 20 40 80
  81      [5]    ──►            1 3 9 27 81
  82      [4]    ──►            1 2 41 82
  83      [2]    ──►  [prime]   1 83
  84     [12]    ──►            1 2 3 4 6 7 12 14 21 28 42 84
  85      [4]    ──►            1 5 17 85
  86      [4]    ──►            1 2 43 86
  87      [4]    ──►            1 3 29 87
  88      [8]    ──►            1 2 4 8 11 22 44 88
  89      [2]    ──►  [prime]   1 89
  90     [12]    ──►            1 2 3 5 6 9 10 15 18 30 45 90
  91      [4]    ──►            1 7 13 91
  92      [6]    ──►            1 2 4 23 46 92
  93      [4]    ──►            1 3 31 93
  94      [4]    ──►            1 2 47 94
  95      [4]    ──►            1 5 19 95
  96     [12]    ──►            1 2 3 4 6 8 12 16 24 32 48 96
  97      [2]    ──►  [prime]   1 97
  98      [6]    ──►            1 2 7 14 49 98
  99      [6]    ──►            1 3 9 11 33 99
 100      [9]    ──►            1 2 4 5 10 20 25 50 100
 101      [2]    ──►  [prime]   1 101
 102      [8]    ──►            1 2 3 6 17 34 51 102
 103      [2]    ──►  [prime]   1 103
 104      [8]    ──►            1 2 4 8 13 26 52 104
 105      [8]    ──►            1 3 5 7 15 21 35 105
 106      [4]    ──►            1 2 53 106
 107      [2]    ──►  [prime]   1 107
 108     [12]    ──►            1 2 3 4 6 9 12 18 27 36 54 108
 109      [2]    ──►  [prime]   1 109
 110      [8]    ──►            1 2 5 10 11 22 55 110
 111      [4]    ──►            1 3 37 111
 112     [10]    ──►            1 2 4 7 8 14 16 28 56 112
 113      [2]    ──►  [prime]   1 113
 114      [8]    ──►            1 2 3 6 19 38 57 114
 115      [4]    ──►            1 5 23 115
 116      [6]    ──►            1 2 4 29 58 116
 117      [6]    ──►            1 3 9 13 39 117
 118      [4]    ──►            1 2 59 118
 119      [4]    ──►            1 7 17 119
 120     [16]    ──►            1 2 3 4 5 6 8 10 12 15 20 24 30 40 60 120
 121      [3]    ──►            1 11 121
 122      [4]    ──►            1 2 61 122
 123      [4]    ──►            1 3 41 123
 124      [6]    ──►            1 2 4 31 62 124
 125      [4]    ──►            1 5 25 125
 126     [12]    ──►            1 2 3 6 7 9 14 18 21 42 63 126
 127      [2]    ──►  [prime]   1 127
 128      [8]    ──►            1 2 4 8 16 32 64 128
 129      [4]    ──►            1 3 43 129
 130      [8]    ──►            1 2 5 10 13 26 65 130
 131      [2]    ──►  [prime]   1 131
 132     [12]    ──►            1 2 3 4 6 11 12 22 33 44 66 132
 133      [4]    ──►            1 7 19 133
 134      [4]    ──►            1 2 67 134
 135      [8]    ──►            1 3 5 9 15 27 45 135
 136      [8]    ──►            1 2 4 8 17 34 68 136
 137      [2]    ──►  [prime]   1 137
 138      [8]    ──►            1 2 3 6 23 46 69 138
 139      [2]    ──►  [prime]   1 139
 140     [12]    ──►            1 2 4 5 7 10 14 20 28 35 70 140
 141      [4]    ──►            1 3 47 141
 142      [4]    ──►            1 2 71 142
 143      [4]    ──►            1 11 13 143
 144     [15]    ──►            1 2 3 4 6 8 9 12 16 18 24 36 48 72 144
 145      [4]    ──►            1 5 29 145
 146      [4]    ──►            1 2 73 146
 147      [6]    ──►            1 3 7 21 49 147
 148      [6]    ──►            1 2 4 37 74 148
 149      [2]    ──►  [prime]   1 149
 150     [12]    ──►            1 2 3 5 6 10 15 25 30 50 75 150
 151      [2]    ──►  [prime]   1 151
 152      [8]    ──►            1 2 4 8 19 38 76 152
 153      [6]    ──►            1 3 9 17 51 153
 154      [8]    ──►            1 2 7 11 14 22 77 154
 155      [4]    ──►            1 5 31 155
 156     [12]    ──►            1 2 3 4 6 12 13 26 39 52 78 156
 157      [2]    ──►  [prime]   1 157
 158      [4]    ──►            1 2 79 158
 159      [4]    ──►            1 3 53 159
 160     [12]    ──►            1 2 4 5 8 10 16 20 32 40 80 160
 161      [4]    ──►            1 7 23 161
 162     [10]    ──►            1 2 3 6 9 18 27 54 81 162
 163      [2]    ──►  [prime]   1 163
 164      [6]    ──►            1 2 4 41 82 164
 165      [8]    ──►            1 3 5 11 15 33 55 165
 166      [4]    ──►            1 2 83 166
 167      [2]    ──►  [prime]   1 167
 168     [16]    ──►            1 2 3 4 6 7 8 12 14 21 24 28 42 56 84 168
 169      [3]    ──►            1 13 169
 170      [8]    ──►            1 2 5 10 17 34 85 170
 171      [6]    ──►            1 3 9 19 57 171
 172      [6]    ──►            1 2 4 43 86 172
 173      [2]    ──►  [prime]   1 173
 174      [8]    ──►            1 2 3 6 29 58 87 174
 175      [6]    ──►            1 5 7 25 35 175
 176     [10]    ──►            1 2 4 8 11 16 22 44 88 176
 177      [4]    ──►            1 3 59 177
 178      [4]    ──►            1 2 89 178
 179      [2]    ──►  [prime]   1 179
 180     [18]    ──►            1 2 3 4 5 6 9 10 12 15 18 20 30 36 45 60 90 180
 181      [2]    ──►  [prime]   1 181
 182      [8]    ──►            1 2 7 13 14 26 91 182
 183      [4]    ──►            1 3 61 183
 184      [8]    ──►            1 2 4 8 23 46 92 184
 185      [4]    ──►            1 5 37 185
 186      [8]    ──►            1 2 3 6 31 62 93 186
 187      [4]    ──►            1 11 17 187
 188      [6]    ──►            1 2 4 47 94 188
 189      [8]    ──►            1 3 7 9 21 27 63 189
 190      [8]    ──►            1 2 5 10 19 38 95 190
 191      [2]    ──►  [prime]   1 191
 192     [14]    ──►            1 2 3 4 6 8 12 16 24 32 48 64 96 192
 193      [2]    ──►  [prime]   1 193
 194      [4]    ──►            1 2 97 194
 195      [8]    ──►            1 3 5 13 15 39 65 195
 196      [9]    ──►            1 2 4 7 14 28 49 98 196
 197      [2]    ──►  [prime]   1 197
 198     [12]    ──►            1 2 3 6 9 11 18 22 33 66 99 198
 199      [2]    ──►  [prime]   1 199
 200     [12]    ──►            1 2 4 5 8 10 20 25 40 50 100 200

Primes that were found:  46

```



### Alternate Version


```REXX
/* REXX ***************************************************************
* Program to calculate and show divisors of positive integer(s).
* 03.08.2012 Walter Pachl  simplified the above somewhat
*            in particular I see no benefit from divAdd procedure
* 04.08.2012 the reference to 'above' is no longer valid since that
*            was meanwhile changed for the better.
* 04.08.2012 took over some improvements from new above
**********************************************************************/
Parse arg low high .
Select
  When low=''  Then Parse Value '1 200' with low high
  When high='' Then high=low
  Otherwise Nop
  End
do j=low to high
  say '   n = ' right(j,6) "   divisors = " divs(j)
  end
exit

divs: procedure; parse arg x
  if x==1 then return 1               /*handle special case of 1     */
  Parse Value '1' x With lo hi        /*initialize lists: lo=1 hi=x  */
  odd=x//2                            /* 1 if x is odd               */
  Do j=2+odd By 1+odd While j*j<x     /*divide by numbers<sqrt(x)    */
    if x//j==0 then Do                /*Divisible?  Add two divisors:*/
      lo=lo j                         /* list low divisors           */
      hi=x%j hi                       /* list high divisors          */
      End
    End
  If j*j=x Then                       /*for a square number as input */
    lo=lo j                           /* add its square root         */
  return lo hi                        /* return both lists           */
```



## Ring


```ring

nArray = list(100)
n = 45
j = 0
for i = 1 to n
    if n % i = 0 j = j + 1 nArray[j] = i ok
next

see "Factors of " + n + " = "
for i = 1 to j
    see "" + nArray[i] + " "
next

```



## Ruby


```ruby
class Integer
  def factors() (1..self).select { |n| (self % n).zero? } end
end
p 45.factors
```

 [1, 3, 5, 9, 15, 45]

As we only have to loop up to <math>\sqrt{n}</math>, we can write

```ruby
class Integer
  def factors
    1.upto(Math.sqrt(self)).select {|i| (self % i).zero?}.inject([]) do |f, i|
      f << self/i unless i == self/i
      f << i
    end.sort
  end
end
[45, 53, 64].each {|n| puts "#{n} : #{n.factors}"}
```

{{out}}

```txt

45 : [1, 3, 5, 9, 15, 45]
53 : [1, 53]
64 : [1, 2, 4, 8, 16, 32, 64]
```



### Using the prime library


```ruby

require 'prime'

def factors m
  return [1] if 1==m
  primes, powers = Prime.prime_division(m).transpose
  ranges = powers.map{|n| (0..n).to_a}
  ranges[0].product( *ranges[1..-1] ).
  map{|es| primes.zip(es).map{|p,e| p**e}.reduce :*}.
  sort
end

[1, 7, 45, 100].each{|n| p factors n}

```

Output:

```txt

[1]
[1, 7]
[1, 3, 5, 9, 15, 45]
[1, 2, 4, 5, 10, 20, 25, 50, 100]

```



## Run BASIC


```runbasic
PRINT "Factors of 45 are ";factorlist$(45)
PRINT "Factors of 12345 are "; factorlist$(12345)
END

function factorlist$(f)
DIM L(100)
FOR i = 1 TO SQR(f)
  IF (f MOD i) = 0 THEN
    L(c) = i
    c = c + 1
    IF (f <> i^2) THEN
      L(c) = (f / i)
      c = c + 1
    END IF
  END IF
NEXT i
s = 1
while s = 1
s = 0
for i = 0 to c-1
 if L(i) > L(i+1) and L(i+1) <> 0 then
  t = L(i)
  L(i) = L(i+1)
  L(i+1) = t
  s      = 1
 end if
next i
wend
FOR i = 0 TO c-1
  factorlist$ = factorlist$ + STR$(L(i)) + ", "
NEXT
end function
```


{{out}}

```txt
Factors of 45 are 1, 3, 5, 9, 15, 45,
Factors of 12345 are 1, 3, 5, 15, 823, 2469, 4115, 12345,
```



## Rust



```rust
fn main() {
    assert_eq!(vec![1, 2, 4, 5, 10, 10, 20, 25, 50, 100], factor(100)); // asserts that two expressions are equal to each other
    assert_eq!(vec![1, 101], factor(101));

}

fn factor(num: i32) -> Vec<i32> {
    let mut factors: Vec<i32> = Vec::new(); // creates a new vector for the factors of the number

    for i in 1..((num as f32).sqrt() as i32 + 1) {
        if num % i == 0 {
            factors.push(i); // pushes smallest factor to factors
            factors.push(num/i); // pushes largest factor to factors
        }
    }
    factors.sort(); // sorts the factors into numerical order for viewing purposes
    factors // returns the factors
}
```


Alternative functional version:


```rust

fn factor(n: i32) -> Vec<i32> {
    (1..=n).filter(|i| n % i == 0).collect()
}

```



## Sather

{{trans|C++}}

```sather
class MAIN is

  factors(n :INT):ARRAY{INT} is
    f:ARRAY{INT};
    f := #;
    f := f.append(|1|);
    f := f.append(|n|);
    loop i ::= 2.upto!( n.flt.sqrt.int );
      if n%i = 0 then
        f := f.append(|i|);
	if (i*i) /= n then f := f.append(|n / i|); end;
      end;
    end;
    f.sort;
    return f;
  end;

  main is
    a :ARRAY{INT} := |3135, 45, 64, 53, 45, 81|;
    loop l ::= a.elt!;
      #OUT + "factors of " + l + ": ";
      r ::= factors(l);
      loop ri ::= r.elt!;
        #OUT + ri + " ";
      end;
      #OUT + "\n";
    end;
  end;
end;
```



## Scala


```Scala

Brute force approach:

  def factors(num: Int) = {
    (1 to num).filter { divisor =>
      num % divisor == 0
    }
  }
Since factors can't be higher than sqrt(num), the code above can be edited as follows
  def factors(num: Int) = {
    (1 to sqrt(num)).filter { divisor =>
      num % divisor == 0
    }
  }

```



## Scheme

This implementation uses a naive trial division algorithm.

```scheme
(define (factors n)
  (define (*factors d)
    (cond ((> d n) (list))
          ((= (modulo n d) 0) (cons d (*factors (+ d 1))))
          (else (*factors (+ d 1)))))
  (*factors 1))

(display (factors 1111111))
(newline)
```


{{out}}

```txt

 (1 239 4649 1111111)

```



## Seed7


```seed7
$ include "seed7_05.s7i";

const proc: writeFactors (in integer: number) is func
  local
    var integer: testNum is 0;
  begin
    write("Factors of " <& number <& ": ");
    for testNum range 1 to sqrt(number) do
      if number rem testNum = 0 then
        if testNum <> 1 then
          write(", ");
        end if;
        write(testNum);
        if testNum <> number div testNum then
          write(", " <& number div testNum);
        end if;
      end if;
    end for;
    writeln;
  end func;

const proc: main is func
  local
    const array integer: numsToFactor is [] (45, 53, 64);
    var integer: number is 0;
  begin
    for number range numsToFactor do
      writeFactors(number);
    end for;
  end func;
```


{{out}}

```txt

Factors of 45: 1, 45, 3, 15, 5, 9
Factors of 53: 1, 53
Factors of 64: 1, 64, 2, 32, 4, 16, 8

```



## SequenceL

'''Brute Force Method'''

A simple brute force method using an indexed partial function as a filter.

```sequencel
Factors(num(0))[i] := i when num mod i = 0 foreach i within 1 ... num;
```


'''Slightly More Efficient Method'''

A slightly more efficient method, only going up to the sqrt(n).

```sequencel
Factors(num(0)) :=
	let
		factorPairs[i] :=
				[i] when i = sqrt(num)
			else
				[i, num/i] when num mod i = 0
			foreach i within 1 ... floor(sqrt(num));
	in
		join(factorPairs);
```



## Sidef


```ruby
func factors(n) {
  gather {
    { |d|
        take(d, n//d) if d.divides(n)
    } << 1..n.isqrt
  }.sort.uniq
}
 
for n [53, 64, 32766] {
    say "factors(#{n}): #{factors(n)}"
}
```

{{out}}

```txt

factors(53): [1, 53]
factors(64): [1, 2, 4, 8, 16, 32, 64]
factors(32766): [1, 2, 3, 6, 43, 86, 127, 129, 254, 258, 381, 762, 5461, 10922, 16383, 32766]

```



## Slate


```slate
n@(Integer traits) primeFactors
[
  [| :result |
   result nextPut: 1.
   n primesDo: [| :prime | result nextPut: prime]] writingAs: {}
].
```

where <tt>primesDo:</tt> is a part of the standard numerics library:

```slate
n@(Integer traits) primesDo: block
"Decomposes the Integer into primes, applying the block to each (in increasing
order)."
[| div next remaining |
  div: 2.
  next: 3.
  remaining: n.
  [[(remaining \\ div) isZero]
     whileTrue:
       [block applyTo: {div}.
	remaining: remaining // div].
   remaining = 1] whileFalse:
     [div: next.
      next: next + 2] "Just looks at the next odd integer."
].
```



## Smalltalk


Copied from the Python example, but code added to the Integer built in class:


```smalltalk>Integer>
factors
	| a |
	a := OrderedCollection new.
	1 to: (self / 2) do: [ :i |
		((self \\ i) = 0) ifTrue: [ a add: i ] ].
	a add: self.
	^a
```


Then use as follows:


```smalltalk

59 factors -> an OrderedCollection(1 59)
120 factors -> an OrderedCollection(1 2 3 4 5 6 8 10 12 15 20 24 30 40 60 120)

```



## Standard ML

Need to print the list because Standard ML truncates the display of
longer returned lists.

```Standard ML
fun printIntList ls =
  (
    List.app (fn n => print(Int.toString n ^ " ")) ls;
    print "\n"
  );

fun factors n =
  let
    fun factors'(n, k) =
      if k > n then
        []
      else if n mod k = 0 then
        k :: factors'(n, k+1)
      else
        factors'(n, k+1)
  in
    factors'(n,1)
  end;

```

Call:

```Standard ML
printIntList(factors 12345)
printIntList(factors 120)
```

{{out}}

```txt
1 3 5 15 823 2469 4115 12345
1 2 3 4 5 6 8 10 12 15 20 24 30 40 60

```



## Swift

Simple implementation:

```Swift
func factors(n: Int) -> [Int] {

    return filter(1...n) { n % $0 == 0 }
}
```

More efficient implementation:

```Swift
import func Darwin.sqrt

func sqrt(x:Int) -> Int { return Int(sqrt(Double(x))) }

func factors(n: Int) -> [Int] {

    var result = [Int]()

    for factor in filter (1...sqrt(n), { n % $0 == 0 }) {

        result.append(factor)

        if n/factor != factor { result.append(n/factor) }
    }

    return sorted(result)

}
```

Call:

```Swift
println(factors(4))
println(factors(1))
println(factors(25))
println(factors(63))
println(factors(19))
println(factors(768))
```

{{out}}

```txt
[1, 2, 4]
[1]
[1, 5, 25]
[1, 3, 7, 9, 21, 63]
[1, 19]
[1, 2, 3, 4, 6, 8, 12, 16, 24, 32, 48, 64, 96, 128, 192, 256, 384, 768]

```



## Tcl


```tcl
proc factors {n} {
    set factors {}
    for {set i 1} {$i <= sqrt($n)} {incr i} {
        if {$n % $i == 0} {
            lappend factors $i [expr {$n / $i}]
        }
    }
    return [lsort -unique -integer $factors]
}
puts [factors 64]
puts [factors 45]
puts [factors 53]
```


{{out}}

```txt
1 2 4 8 16 32 64
1 3 5 9 15 45
1 53
```



## UNIX Shell

This should work in all Bourne-compatible shells, assuming the system has both <tt>sort</tt> and at least one of <tt>bc</tt> or <tt>dc</tt>.

<lang>factor() {
  r=`echo "sqrt($1)" | bc` # or `echo $1 v p | dc`
  i=1
  while [ $i -lt $r ]; do
    if [ `expr $1 % $i` -eq 0 ]; then
      echo $i
      expr $1 / $i
    fi
    i=`expr $i + 1`
  done | sort -nu
}

```



## Ursa

This program takes an integer from the command line and outputs its factors.

```ursa
decl int n
set n (int args<1>)

decl int i
for (set i 1) (< i (+ (/ n 2) 1)) (inc i)
        if (= (mod n i) 0)
                out i "  " console
        end if
end for
out n endl console
```



## Ursala

The simple way:

```Ursala
#import std
#import nat

factors "n" = (filter not remainder/"n") nrange(1,"n")
```

The complicated way:

```Ursala
factors "n" = nleq-<&@s <.~&r,quotient>*= "n"-* (not remainder/"n")*~ nrange(1,root("n",2))
```

Another idea would be to approximate an upper bound for the square root of <code>"n"</code> with some bit twiddling such as <code>&!*K31 "n"</code>, which evaluates to a binary number of all 1's half the width of "n" rounded up, and another would be to use the <code>division</code> function to get the quotient and remainder at the same time. Combining these ideas, losing the dummy variable, and cleaning up some other cruft, we have

```Ursala
factors = nleq-<&@rrZPFLs+ ^(~&r,division)^*D/~& nrange/1+ &!*K31
```

where <code>nleq-<&</code> isn't strictly necessary unless an ordered list is required.

```Ursala
#cast %nL

example = factors 100
```


{{out}}

```txt
<1,2,4,5,10,20,25,50,100>
```



## VBA


```vb
Function Factors(x As Integer) As String
 Application.Volatile
 Dim i As Integer
 Dim cooresponding_factors As String
 Factors = 1
 corresponding_factors = x
 For i = 2 To Sqr(x)
  If x Mod i = 0 Then
   Factors = Factors & ", " & i
   If i <> x / i Then corresponding_factors = x / i & ", " & corresponding_factors
  End If
 Next i
 If x <> 1 Then Factors = Factors & ", " & corresponding_factors
End Function
```

{{out}}

```txt
cell formula is "=Factors(840)"
resultant value is "1, 2, 3, 4, 5, 6, 7, 8, 10, 12, 14, 15, 20, 21, 24, 28, 30, 35, 40, 42, 56, 60, 70, 84, 105, 120, 140, 168, 210, 280, 420, 840"
```



## Wortel


```wortel
@let {
  factors1      &n !-\%%n @to n
  factors_tacit @(\\%% !- @to)
  [[
    !factors1 10
    !factors_tacit 100
    !factors1 720
  ]]
}
```

Returns:
```txt
[
  [1 2 5 10]
  [1 2 4 5 10 20 25 50 100]
  [1 2 3 4 5 6 8 9 10 12 15 16 18 20 24 30 36 40 45 48 60 72 80 90 120 144 180 240 360 720]
]
```



## XPL0


```XPL0
include c:\cxpl\codes;
int     N0, N, F;
[N0:= 1;
repeat  IntOut(0, N0);  Text(0, " = ");
        F:= 2;  N:= N0;
        repeat  if rem(N/F) = 0 then
                        [if N # N0 then Text(0, " * ");
                        IntOut(0, F);
                        N:= N/F;
                        ]
                else F:= F+1;
        until   F>N;
        if N0=1 then IntOut(0, 1);      \1 = 1
        CrLf(0);
        N0:= N0+1;
until   KeyHit;
]
```


{{out}}

```txt

1 = 1
2 = 2
3 = 3
4 = 2 * 2
5 = 5
6 = 2 * 3
7 = 7
8 = 2 * 2 * 2
9 = 3 * 3
10 = 2 * 5
11 = 11
12 = 2 * 2 * 3
13 = 13
14 = 2 * 7
15 = 3 * 5
16 = 2 * 2 * 2 * 2
17 = 17
18 = 2 * 3 * 3
. . .
57086 = 2 * 17 * 23 * 73
57087 = 3 * 3 * 6343
57088 = 2 * 2 * 2 * 2 * 2 * 2 * 2 * 2 * 223
57089 = 57089
57090 = 2 * 3 * 5 * 11 * 173
57091 = 37 * 1543
57092 = 2 * 2 * 7 * 2039
57093 = 3 * 19031
57094 = 2 * 28547
57095 = 5 * 19 * 601
57096 = 2 * 2 * 2 * 3 * 3 * 13 * 61
57097 = 57097

```



## zkl

{{trans|Chapel}}

```zkl
fcn f(n){ (1).pump(n.toFloat().sqrt(), List,
   'wrap(m){((n % m)==0) and T(m,n/m) or Void.Skip}) }
fcn g(n){ [[(m); [1..n.toFloat().sqrt()],'{n%m==0}; '{T(m,n/m)} ]] }  // list comprehension
```

{{out}}

```txt

zkl: f(45)
L(L(1,45),L(3,15),L(5,9))

zkl: g(45)
L(L(1,45),L(3,15),L(5,9))

```



## ZX Spectrum Basic

{{trans|AWK}}

```zxbasic
10 INPUT "Enter a number or 0 to exit: ";n
20 IF n=0 THEN STOP
30 PRINT "Factors of ";n;": ";
40 FOR i=1 TO n
50 IF FN m(n,i)=0 THEN PRINT i;" ";
60 NEXT i
70 DEF FN m(a,b)=a-INT (a/b)*b
```

