+++
title = "Sieve of Eratosthenes"
description = ""
date = 2019-10-21T14:43:02Z
aliases = []
[extra]
id = 2174
[taxonomies]
categories = []
tags = []
+++

{{task|Prime Numbers}}
{{clarified-review}}

The [[wp:Sieve_of_Eratosthenes|Sieve of Eratosthenes]] is a simple algorithm that finds the prime numbers up to a given integer.


;Task:
Implement the   Sieve of Eratosthenes   algorithm, with the only allowed optimization that the outer loop can stop at the square root of the limit, and the inner loop may start at the square of the prime just found.

That means especially that you shouldn't optimize by using pre-computed ''wheels'', i.e. don't assume you need only to cross out odd numbers (wheel based on 2), numbers equal to 1 or 5 modulo 6 (wheel based on 2 and 3), or similar wheels based on low primes.

If there's an easy way to add such a wheel based optimization, implement it as an alternative version.


;Note:
* It is important that the sieve algorithm be the actual algorithm used to find prime numbers for the task.


;Related tasks:
*   [[Emirp primes]]
*   [[count in factors]]
*   [[prime decomposition]]
*   [[factors of an integer]]
*   [[extensible prime generator]]
*   [[primality by trial division]]
*   [[factors of a Mersenne number]]
*   [[trial factoring of a Mersenne number]]
*   [[partition an integer X into N primes]]
*   [[sequence of primes by Trial Division]]





## 360 Assembly

For maximum compatibility, this program uses only the basic instruction set.
<lang 360_Assembly>*        Sieve of Eratosthenes
ERATOST  CSECT
         USING  ERATOST,R12
SAVEAREA B      STM-SAVEAREA(R15)
         DC     17F'0'
         DC     CL8'ERATOST'
STM      STM    R14,R12,12(R13) save calling context
         ST     R13,4(R15)
         ST     R15,8(R13)
         LR     R12,R15         set addessability
*        ----   CODE
         LA     R4,1            I=1
         LA     R6,1            increment
         L      R7,N            limit
LOOPI    BXH    R4,R6,ENDLOOPI  do I=2 to N
         LR     R1,R4           R1=I
         BCTR   R1,0
         LA     R14,CRIBLE(R1)
         CLI    0(R14),X'01'
         BNE    ENDIF           if not CRIBLE(I)
         LR     R5,R4           J=I
         LR     R8,R4
         LR     R9,R7
LOOPJ    BXH    R5,R8,ENDLOOPJ  do J=I*2 to N by I
         LR     R1,R5           R1=J
         BCTR   R1,0
         LA     R14,CRIBLE(R1)
         MVI    0(R14),X'00'    CRIBLE(J)='0'B
         B      LOOPJ
ENDLOOPJ EQU    *
ENDIF    EQU    *
         B      LOOPI
ENDLOOPI EQU    *
         LA     R4,1            I=1
         LA     R6,1
         L      R7,N
LOOP     BXH    R4,R6,ENDLOOP   do I=1 to N
         LR     R1,R4           R1=I
         BCTR   R1,0
         LA     R14,CRIBLE(R1)
         CLI    0(R14),X'01'
         BNE    NOTPRIME        if not CRIBLE(I)
         CVD    R4,P            P=I
         UNPK   Z,P             Z=P
         MVC    C,Z             C=Z
         OI     C+L'C-1,X'F0'   zap sign
         MVC    WTOBUF(8),C+8
         WTO    MF=(E,WTOMSG)
NOTPRIME EQU    *
         B      LOOP
ENDLOOP  EQU    *
RETURN   EQU    *
         LM     R14,R12,12(R13) restore context
         XR     R15,R15         set return code to 0
         BR     R14             return to caller
*        ----   DATA
I        DS     F
J        DS     F
         DS     0F
P        DS     PL8             packed
Z        DS     ZL16            zoned
C        DS     CL16            character
WTOMSG   DS     0F
         DC     H'80'           length of WTO buffer
         DC     H'0'            must be binary zeroes
WTOBUF   DC     80C' '
         LTORG
N        DC     F'100000'
CRIBLE   DC     100000X'01'
         YREGS
         END    ERATOST
```

{{out}}
<pre style="height:20ex">
00000002
00000003
00000005
00000007
00000011
00000013
00000017
00000019
00000023
00000029
00000031
00000037
00000041
00000043
00000047
00000053
00000059
00000061
00000067
...
00099767
00099787
00099793
00099809
00099817
00099823
00099829
00099833
00099839
00099859
00099871
00099877
00099881
00099901
00099907
00099923
00099929
00099961
00099971
00099989
00099991

```



## 6502 Assembly

If this subroutine is called with the value of <i>n</i> in the accumulator, it will store an array of the primes less than <i>n</i> beginning at address 1000 hex and return the number of primes it has found in the accumulator.

```6502asm
ERATOS: STA  $D0      ; value of n
        LDA  #$00
        LDX  #$00
SETUP:  STA  $1000,X  ; populate array
        ADC  #$01
        INX
        CPX  $D0
        BPL  SET
        JMP  SETUP
SET:    LDX  #$02
SIEVE:  LDA  $1000,X  ; find non-zero
        INX
        CPX  $D0
        BPL  SIEVED
        CMP  #$00
        BEQ  SIEVE
        STA  $D1      ; current prime
MARK:   CLC
        ADC  $D1
        TAY
        LDA  #$00
        STA  $1000,Y
        TYA
        CMP  $D0
        BPL  SIEVE
        JMP  MARK
SIEVED: LDX  #$01
        LDY  #$00
COPY:   INX
        CPX  $D0
        BPL  COPIED
        LDA  $1000,X
        CMP  #$00
        BEQ  COPY
        STA  $2000,Y
        INY
        JMP  COPY
COPIED: TYA           ; how many found
        RTS
```



## 68000 Assembly

'''Algorithm somewhat optimized:''' array omits 1, 2, all higher odd numbers.
Optimized for storage: uses bit array for prime/composite flags.

{{works with|http://www.easy68k.com/ EASy68K v5.13.00}}
Some of the macro code is derived from the examples included with EASy68K.
See 68000 "100 Doors" listing for additional information.

```68000devpac
*-----------------------------------------------------------
* Title      : BitSieve
* Written by : G. A. Tippery
* Date       : 2014-Feb-24, 2013-Dec-22
* Description: Prime number sieve
*-----------------------------------------------------------
    	ORG    $1000

**	---- Generic macros ----	**
PUSH	MACRO
	MOVE.L	\1,-(SP)
	ENDM

POP	MACRO
	MOVE.L	(SP)+,\1
	ENDM

DROP	MACRO
	ADDQ	#4,SP
	ENDM

PUTS	MACRO
	** Print a null-terminated string w/o CRLF **
	** Usage: PUTS stringaddress
	** Returns with D0, A1 modified
	MOVEQ	#14,D0	; task number 14 (display null string)
	LEA	\1,A1	; address of string
	TRAP	#15	; display it
	ENDM

GETN	MACRO
	MOVEQ	#4,D0	; Read a number from the keyboard into D1.L.
	TRAP	#15
	ENDM

**	---- Application-specific macros ----	**

val	MACRO		; Used by bit sieve. Converts bit address to the number it represents.
	ADD.L	\1,\1	; double it because odd numbers are omitted
	ADDQ	#3,\1	; add offset because initial primes (1, 2) are omitted
	ENDM

* **
### ==========================================================================
 **
* ** Integer square root routine, bisection method **
* ** IN: D0, should be 0<D0<$10000 (65536) -- higher values MAY work, no guarantee
* ** OUT: D1
*
SquareRoot:
*
	MOVEM.L	D2-D4,-(SP)	; save registers needed for local variables
*	DO == n
*	D1 == a
*	D2 == b
*	D3 == guess
*	D4 == temp
*
*		a = 1;
*		b = n;
	MOVEQ	#1,D1
	MOVE.L	D0,D2
*		do {
	REPEAT
*		guess = (a+b)/2;
	MOVE.L	D1,D3
	ADD.L	D2,D3
	LSR.L	#1,D3
*		if (guess*guess > n) {	// inverse function of sqrt is square
	MOVE.L	D3,D4
	MULU	D4,D4		; guess^2
	CMP.L	D0,D4
	BLS	.else
*		b = guess;
	MOVE.L	D3,D2
	BRA	.endif
*		} else {
.else:
*		a = guess;
	MOVE.L	D3,D1
*		} //if
.endif:
*		} while ((b-a) > 1);	; Same as until (b-a)<=1 or until (a-b)>=1
	MOVE.L	D2,D4
	SUB.L	D1,D4	; b-a
	UNTIL.L	  D4 <LE> #1 DO.S
*		return (a)	; Result is in D1
*		} //LongSqrt()
	MOVEM.L	(SP)+,D2-D4	; restore saved registers
	RTS
*
* **
### ==========================================================================
 **


**
### =================================================================
 **
*
**  Prime-number Sieve of Eratosthenes routine using a big bit field for flags  **
*  Enter with D0 = size of sieve (bit array)
*  Prints found primes 10 per line
*  Returns # prime found in D6
*
*   Register usage:
*
*	D0 == n
*	D1 == prime
*	D2 == sqroot
*	D3 == PIndex
*	D4 == CIndex
*	D5 == MaxIndex
*	D6 == PCount
*
*	A0 == PMtx[0]
*
*   On return, all registers above except D0 are modified. Could add MOVEMs to save and restore D2-D6/A0.
*

**	------------------------	**

GetBit:		** sub-part of Sieve subroutine **
		** Entry: bit # is on TOS
		** Exit: A6 holds the byte number, D7 holds the bit number within the byte
		** Note: Input param is still on TOS after return. Could have passed via a register, but
                **  wanted to practice with stack. :)
*
	MOVE.L	(4,SP),D7	; get value from (pre-call) TOS
	ASR.L	#3,D7	; /8
	MOVEA	D7,A6	; byte #
	MOVE.L	(4,SP),D7	; get value from (pre-call) TOS
	AND.L	#$7,D7	; bit #
	RTS

**	------------------------	**

Sieve:
	MOVE	D0,D5
	SUBQ	#1,D5
	JSR	SquareRoot	; sqrt D0 => D1
	MOVE.L	D1,D2
	LEA	PArray,A0
	CLR.L	D3
*
PrimeLoop:
	MOVE.L	D3,D1
	val	D1
	MOVE.L	D3,D4
	ADD.L	D1,D4
*
CxLoop:		; Goes through array marking multiples of d1 as composite numbers
	CMP.L	D5,D4
	BHI	ExitCx
	PUSH	D4	; set D7 as bit # and A6 as byte pointer for D4'th bit of array
	JSR GetBit
	DROP
	BSET	D7,0(A0,A6.L)	; set bit to mark as composite number
	ADD.L	D1,D4	; next number to mark
	BRA	CxLoop
ExitCx:
	CLR.L	D1	; Clear new-prime-found flag
	ADDQ	#1,D3	; Start just past last prime found
PxLoop:		; Searches for next unmarked (not composite) number
	CMP.L	D2,D3	; no point searching past where first unmarked multiple would be past end of array
	BHI	ExitPx	; if past end of array
	TST.L	D1
	BNE	ExitPx	; if flag set, new prime found
	PUSH D3		; check D3'th bit flag
	JSR	GetBit	; sets D7 as bit # and A6 as byte pointer
	DROP		; drop TOS
	BTST	D7,0(A0,A6.L)	; read bit flag
	BNE	IsSet	; If already tagged as composite
	MOVEQ	#-1,D1	; Set flag that we've found a new prime
IsSet:
	ADDQ	#1,D3	; next PIndex
	BRA	PxLoop
ExitPx:
	SUBQ	#1,D3	; back up PIndex
	TST.L	D1	; Did we find a new prime #?
	BNE	PrimeLoop	; If another prime # found, go process it
*
		; fall through to print routine

**	------------------------	**

* Print primes found
*
*	D4 == Column count
*
*	Print header and assumed primes (#1, #2)
    	PUTS	Header	; Print string @ Header, no CR/LF
	MOVEQ	#2,D6	; Start counter at 2 because #1 and #2 are assumed primes
	MOVEQ	#2,D4
*
	MOVEQ	#0,D3
PrintLoop:
	CMP.L	D5,D3
	BHS	ExitPL
	PUSH	D3
	JSR	GetBit	; sets D7 as bit # and A6 as byte pointer
	DROP		; drop TOS
	BTST	D7,0(A0,A6.L)
	BNE		NotPrime
*		printf(" %6d", val(PIndex)
	MOVE.L	D3,D1
	val	D1
	AND.L	#$0000FFFF,D1
	MOVEQ	#6,D2
	MOVEQ	#20,D0	; display signed RJ
	TRAP	#15
	ADDQ	#1,D4
	ADDQ	#1,D6
*	*** Display formatting ***
*		if((PCount % 10) == 0) printf("\n");
	CMP	#10,D4
	BLO	NoLF
	PUTS	CRLF
	MOVEQ	#0,D4
NoLF:
NotPrime:
	ADDQ	#1,D3
	BRA	PrintLoop
ExitPL:
	RTS

**
### =================================================================
 **

N	EQU	5000	; *** Size of boolean (bit) array ***
SizeInBytes	EQU	(N+7)/8
*
START:                  	; first instruction of program
	MOVE.L	#N,D0	; # to test
	JSR	Sieve
*		printf("\n %d prime numbers found.\n", D6); ***
	PUTS	Summary1,A1
	MOVE	#3,D0	; Display signed number in D1.L in decimal in smallest field.
	MOVE.W	D6,D1
	TRAP	#15
	PUTS	Summary2,A1

	SIMHALT             	; halt simulator

**
### =================================================================
 **

* Variables and constants here

	ORG	$2000
CR	EQU	13
LF	EQU	10
CRLF	DC.B	CR,LF,$00

PArray:	DCB.B	SizeInBytes,0

Header:	DC.B	CR,LF,LF,' Primes',CR,LF,' ======',CR,LF
		DC.B	'     1     2',$00

Summary1:	DC.B	CR,LF,' ',$00
Summary2:	DC.B	' prime numbers found.',CR,LF,$00

    END    START        	; last line of source
```



## ABAP


```Lisp

PARAMETERS: p_limit TYPE i OBLIGATORY DEFAULT 100.

AT SELECTION-SCREEN ON p_limit.
  IF p_limit LE 1.
    MESSAGE 'Limit must be higher then 1.' TYPE 'E'.
  ENDIF.

START-OF-SELECTION.
  FIELD-SYMBOLS: <fs_prime> TYPE flag.
  DATA: gt_prime TYPE TABLE OF flag,
        gv_prime TYPE flag,
        gv_i     TYPE i,
        gv_j     TYPE i.

  DO p_limit TIMES.
    IF sy-index > 1.
      gv_prime = abap_true.
    ELSE.
      gv_prime = abap_false.
    ENDIF.

    APPEND gv_prime TO gt_prime.
  ENDDO.

  gv_i = 2.
  WHILE ( gv_i <= trunc( sqrt( p_limit ) ) ).
    IF ( gt_prime[ gv_i ] EQ abap_true ).
      gv_j =  gv_i ** 2.
      WHILE ( gv_j <= p_limit ).
        gt_prime[ gv_j ] = abap_false.
        gv_j = ( gv_i ** 2 ) + ( sy-index * gv_i ).
      ENDWHILE.
    ENDIF.
    gv_i = gv_i + 1.
  ENDWHILE.

  LOOP AT gt_prime INTO gv_prime.
    IF gv_prime = abap_true.
      WRITE: / sy-tabix.
    ENDIF.
  ENDLOOP.

```


## ACL2


```Lisp
(defun nats-to-from (n i)
   (declare (xargs :measure (nfix (- n i))))
   (if (zp (- n i))
       nil
       (cons i (nats-to-from n (+ i 1)))))

(defun remove-multiples-up-to-r (factor limit xs i)
   (declare (xargs :measure (nfix (- limit i))))
   (if (or (> i limit)
           (zp (- limit i))
           (zp factor))
       xs
       (remove-multiples-up-to-r
        factor
        limit
        (remove i xs)
        (+ i factor))))

(defun remove-multiples-up-to (factor limit xs)
   (remove-multiples-up-to-r factor limit xs (* factor 2)))

(defun sieve-r (factor limit)
   (declare (xargs :measure (nfix (- limit factor))))
   (if (zp (- limit factor))
       (nats-to-from limit 2)
       (remove-multiples-up-to factor (+ limit 1)
                               (sieve-r (1+ factor) limit))))

(defun sieve (limit)
   (sieve-r 2 limit))
```



## ActionScript

Works with ActionScript 3.0 (this is utilizing the actions panel, not a separated class file)

```actionscript
function eratosthenes(limit:int):Array
{
	var primes:Array = new Array();
	if (limit >= 2) {
		var sqrtlmt:int = int(Math.sqrt(limit) - 2);
		var nums:Array = new Array(); // start with an empty Array...
		for (var i:int = 2; i <= limit; i++) // and
			nums.push(i); // only initialize the Array once...
		for (var j:int = 0; j <= sqrtlmt; j++) {
			var p:int = nums[j]
			if (p)
				for (var t:int = p * p - 2; t < nums.length; t += p)
					nums[t] = 0;
		}
		for (var m:int = 0; m < nums.length; m++) {
			var r:int = nums[m];
			if (r)
				primes.push(r);
		}
	}
	return primes;
}
var e:Array = eratosthenes(1000);
trace(e);
```

Output:
{{out}}

```txt

2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97,101,103,107,109,113,127,131,137,139,149,151,157,163,167,173,179,181,191,193,197,199,211,223,227,229,233,239,241,251,257,263,269,271,277,281,283,293,307,311,313,317,331,337,347,349,353,359,367,373,379,383,389,397,401,409,419,421,431,433,439,443,449,457,461,463,467,479,487,491,499,503,509,521,523,541,547,557,563,569,571,577,587,593,599,601,607,613,617,619,631,641,643,647,653,659,661,673,677,683,691,701,709,719,727,733,739,743,751,757,761,769,773,787,797,809,811,821,823,827,829,839,853,857,859,863,877,881,883,887,907,911,919,929,937,941,947,953,967,971,977,983,991,997

```



## Ada



```Ada
with Ada.Text_IO, Ada.Command_Line;

procedure Eratos is

   Last: Positive := Positive'Value(Ada.Command_Line.Argument(1));
   Prime: array(1 .. Last) of Boolean := (1 => False, others => True);
   Base: Positive := 2;
   Cnt: Positive;
begin
   loop
      exit when Base * Base > Last;
      if Prime(Base) then
         Cnt := Base + Base;
         loop
            exit when Cnt > Last;
            Prime(Cnt) := False;
            Cnt := Cnt + Base;
         end loop;
      end if;
      Base := Base + 1;
   end loop;
   Ada.Text_IO.Put("Primes less or equal" & Positive'Image(Last) &" are:");
   for Number in Prime'Range loop
      if Prime(Number) then
         Ada.Text_IO.Put(Positive'Image(Number));
      end if;
   end loop;
end Eratos;
```


{{out}}

```txt
> ./eratos 31
Primes less or equal 31 are : 2 3 5 7 11 13 17 19 23 29 31
```



## Agda



```agda

-- imports
open import Data.Nat as ℕ     using (ℕ; suc; zero; _+_; _∸_)
open import Data.Vec as Vec   using (Vec; _∷_; []; tabulate; foldr)
open import Data.Fin as Fin   using (Fin; suc; zero)
open import Function          using (_∘_; const; id)
open import Data.List as List using (List; _∷_; [])
open import Data.Maybe        using (Maybe; just; nothing)

-- Without square cutoff optimization
module Simple where
  primes : ∀ n → List (Fin n)
  primes zero = []
  primes (suc zero) = []
  primes (suc (suc zero)) = []
  primes (suc (suc (suc m))) = sieve (tabulate (just ∘ suc))
    where
    sieve : ∀ {n} → Vec (Maybe (Fin (2 + m))) n → List (Fin (3 + m))
    sieve [] = []
    sieve (nothing ∷ xs) =         sieve xs
    sieve (just x  ∷ xs) = suc x ∷ sieve (foldr B remove (const []) xs x)
      where
      B = λ n → ∀ {i} → Fin i → Vec (Maybe (Fin (2 + m))) n

      remove : ∀ {n} → Maybe (Fin (2 + m)) → B n → B (suc n)
      remove _ ys zero    = nothing ∷ ys x
      remove y ys (suc z) = y       ∷ ys z

-- With square cutoff optimization
module SquareOpt where
  primes : ∀ n → List (Fin n)
  primes zero = []
  primes (suc zero) = []
  primes (suc (suc zero)) = []
  primes (suc (suc (suc m))) = sieve 1 m (Vec.tabulate (just ∘ Fin.suc ∘ Fin.suc))
    where
    sieve : ∀ {n} → ℕ → ℕ → Vec (Maybe (Fin (3 + m))) n → List (Fin (3 + m))
    sieve _ zero = List.mapMaybe id ∘ Vec.toList
    sieve _ (suc _) [] = []
    sieve i (suc l) (nothing ∷ xs) =     sieve (suc i) (l ∸ i ∸ i) xs
    sieve i (suc l) (just x  ∷ xs) = x ∷ sieve (suc i) (l ∸ i ∸ i) (Vec.foldr B remove (const []) xs i)
      where
      B = λ n → ℕ → Vec (Maybe (Fin (3 + m))) n

      remove : ∀ {i} → Maybe (Fin (3 + m)) → B i → B (suc i)
      remove _ ys zero    = nothing ∷ ys i
      remove y ys (suc j) = y       ∷ ys j

```



## Agena

Tested with Agena 2.9.5 Win32

```agena
# Sieve of Eratosthenes

# generate and return a sequence containing the primes up to sieveSize
sieve := proc( sieveSize :: number ) :: sequence is
    local sieve, result;

    result := seq(); # sequence of primes - initially empty
    create register sieve( sieveSize ); # "vector" to be sieved

    sieve[ 1 ] := false;
    for sPos from 2 to sieveSize do sieve[ sPos ] := true od;

    # sieve the primes
    for sPos from 2 to entier( sqrt( sieveSize ) ) do
        if sieve[ sPos ] then
            for p from sPos * sPos to sieveSize by sPos do
                sieve[ p ] := false
            od
        fi
    od;

    # construct the sequence of primes
    for sPos from 1 to sieveSize do
        if sieve[ sPos ] then insert sPos into result fi
    od

return result
end; # sieve


# test the sieve proc
for i in sieve( 100 ) do write( " ", i ) od; print();
```

{{out}}

```txt

 2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97

```



## ALGOL 60

'''Based on the 1962 Revised Repport''':
 '''comment''' Sieve of Eratosthenes;
 '''begin'''
    '''integer array''' t[0:1000];
    '''integer''' i,j,k;
    '''for''' i:=0 '''step''' 1 '''until''' 1000 '''do''' t[i]:=1;
    t[0]:=0; t[1]:=0; i:=0;
    '''for''' i:=i '''while''' i<1000 '''do'''
    '''begin'''
        '''for''' i:=i '''while''' i<1000 '''and''' t[i]=0 '''do''' i:=i+1;
        '''if''' i<1000 '''then'''
        '''begin'''
            j:=2;
            k:=j*i;
            '''for''' k:=k '''while''' k<1000 '''do'''
            '''begin'''
                t[k]:=0;
                j:=j+1;
                k:=j*i
            '''end''';
            i:=i+1
        '''end'''
    '''end''';
    '''for''' i:=0 '''step''' 1 '''until''' 999 '''do'''
    '''if''' t[i]≠0 '''then''' print(i,ꞌ is primeꞌ)
 '''end'''

'''An 1964 Implementation''':

'''Works with:''' ALGOL 60 for OS/360

```algol60
'BEGIN'
    'INTEGER' 'ARRAY' CANDIDATES(/0..1000/);
    'INTEGER' I,J,K;
    'COMMENT' SET LINE-LENGTH=120,SET LINES-PER-PAGE=62,OPEN;
    SYSACT(1,6,120); SYSACT(1,8,62); SYSACT(1,12,1);
    'FOR' I := 0 'STEP' 1 'UNTIL' 1000 'DO'
    'BEGIN'
        CANDIDATES(/I/) := 1;
    'END';
    CANDIDATES(/0/) := 0;
    CANDIDATES(/1/) := 0;
    I := 0;
    'FOR' I := I 'WHILE' I 'LESS' 1000 'DO'
    'BEGIN'
        'FOR' I := I 'WHILE' I 'LESS' 1000
          'AND' CANDIDATES(/I/) 'EQUAL' 0 'DO'
            I := I+1;
        'IF' I 'LESS' 1000 'THEN'
        'BEGIN'
            J := 2;
            K := J*I;
            'FOR' K := K 'WHILE' K 'LESS' 1000 'DO'
            'BEGIN'
                CANDIDATES(/K/) := 0;
                J := J + 1;
                K := J*I;
            'END';
            I := I+1;
            'END'
        'END';
        'FOR' I := 0 'STEP' 1 'UNTIL' 999 'DO'
        'IF' CANDIDATES(/I/) 'NOTEQUAL' 0  'THEN'
        'BEGIN'
            OUTINTEGER(1,I);
            OUTSTRING(1,'(' IS PRIME')');
            'COMMENT' NEW LINE;
            SYSACT(1,14,1)
        'END'
    'END'
'END'
```



## ALGOL 68


```algol68
BOOL prime = TRUE, non prime = FALSE;
PROC eratosthenes = (INT n)[]BOOL:
(
  [n]BOOL sieve;
  FOR i TO UPB sieve DO sieve[i] := prime OD;
  INT m = ENTIER sqrt(n);
  sieve[1] := non prime;
  FOR i FROM 2 TO m DO
    IF sieve[i] = prime THEN
      FOR j FROM i*i BY i TO n DO
        sieve[j] := non prime
      OD
    FI
  OD;
  sieve
);

 print((eratosthenes(80),new line))
```

{{out}}

```txt

FTTFTFTFFFTFTFFFTFTFFFTFFFFFTFTFFFFFTFFFTFTFFFTFFFFFTFFFFFTFTFFFFFTFFFTFTFFFFFTF

```


=={{header|ALGOL-M}}==

```algol

BEGIN

COMMENT
  FIND PRIMES UP TO THE SPECIFIED LIMIT (HERE 1,000) USING
  THE SIEVE OF ERATOSTHENES;

% CALCULATE INTEGER SQUARE ROOT %
INTEGER FUNCTION ISQRT(N);
INTEGER N;
BEGIN
    INTEGER R1, R2;
    R1 := N;
    R2 := 1;
    WHILE R1 > R2 DO
        BEGIN
            R1 := (R1+R2) / 2;
            R2 := N / R1;
        END;
    ISQRT := R1;
END;

INTEGER LIMIT, I, J, FALSE, TRUE, COL, COUNT;
INTEGER ARRAY FLAGS[1:1000];

LIMIT := 1000;
FALSE := 0;
TRUE := 1;

WRITE("FINDING PRIMES FROM 2 TO",LIMIT);

% INITIALIZE TABLE %
FOR I := 1 STEP 1 UNTIL LIMIT DO
  FLAGS[I] := TRUE;

% SIEVE FOR PRIMES %
FOR I := 2 STEP 1 UNTIL ISQRT(LIMIT) DO
  BEGIN
    IF FLAGS[I] = TRUE THEN
        FOR J := (I * I) STEP I UNTIL LIMIT DO
          FLAGS[J] := FALSE;
  END;

% WRITE OUT THE PRIMES EIGHT PER LINE %
COUNT := 0;
COL := 1;
WRITE(" ");
FOR I := 2 STEP 1 UNTIL LIMIT DO
  BEGIN
    IF FLAGS[I] = TRUE THEN
      BEGIN
         WRITEON(I);
         COUNT := COUNT + 1;
         COL := COL + 1;
         IF COL > 8 THEN
           BEGIN
             WRITE(" ");
             COL := 1;
           END;
      END;
  END;

WRITE(" ");
WRITE(COUNT, " primes were found.");

END

```



## ALGOL W


```algolw
begin

    % implements the sieve of Eratosthenes                                   %
    %     s(i) is set to true if i is prime, false otherwise                 %
    %     algol W doesn't have a upb operator, so we pass the size of the    %
    %     array in n                                                         %
    procedure sieve( logical array s ( * ); integer value n ) ;
    begin

        % start with everything flagged as prime                             %
        for i := 1 until n do s( i ) := true;

        % sieve out the non-primes                                           %
        s( 1 ) := false;
        for i := 2 until truncate( sqrt( n ) )
        do begin
            if s( i )
            then begin
                for p := i * i step i until n do s( p ) := false
            end if_s_i
        end for_i ;

    end sieve ;

    % test the sieve procedure                                               %

    integer sieveMax;

    sieveMax := 100;
    begin

        logical array s ( 1 :: sieveMax );

        i_w := 2; % set output field width                                   %
        s_w := 1; % and output separator width                               %

        % find and display the primes                                        %
        sieve( s, sieveMax );
        for i := 1 until sieveMax do if s( i ) then writeon( i );

    end

end.
```

{{out}}

```txt

 2  3  5  7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97

```


=={{Header|APL}}==

All these versions requires<tt> ⎕io←0 </tt>(index origin 0).

It would have been better to require a result of the boolean mask rather than the actual list of primes.
The list of primes obtains readily from the mask by application of a simple function (here<tt> {⍵/⍳⍴⍵}</tt>).
Other related computations (such as the number of primes < n) obtain readily from the mask,
easier than producing the list of primes.

=== Non-Optimized Version ===


```apl
sieve2←{
  b←⍵⍴1
  b[⍳2⌊⍵]←0
  2≥⍵:b
  p←{⍵/⍳⍴⍵}∇⌈⍵*0.5
  m←1+⌊(⍵-1+p×p)÷p
  b ⊣ p {b[⍺×⍺+⍳⍵]←0}¨ m
}

primes2←{⍵/⍳⍴⍵}∘sieve2
```


The required list of prime divisors obtains by recursion (<tt>{⍵/⍳⍴⍵}∇⌈⍵*0.5</tt>).


###  Optimized Version



```apl
sieve←{
  b←⍵⍴{∧⌿↑(×/⍵)⍴¨~⍵↑¨1}2 3 5
  b[⍳6⌊⍵]←(6⌊⍵)⍴0 0 1 1 0 1
  49≥⍵:b
  p←3↓{⍵/⍳⍴⍵}∇⌈⍵*0.5
  m←1+⌊(⍵-1+p×p)÷2×p
  b ⊣ p {b[⍺×⍺+2×⍳⍵]←0}¨ m
}

primes←{⍵/⍳⍴⍵}∘sieve
```


The optimizations are as follows:
* Multiples of<tt> 2 3 5 </tt>are marked by initializing<tt> b </tt>with<tt> ⍵⍴{∧⌿↑(×/⍵)⍴¨~⍵↑¨1}2 3 5 </tt>rather than with<tt> ⍵⍴1</tt>.
* Subsequently, only odd multiples of primes > 5 are marked.
* Multiples of a prime to be marked start at its square.


###  Examples



```apl
   primes 100
2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97

   primes¨ ⍳14
┌┬┬┬─┬───┬───┬─────┬─────┬───────┬───────┬───────┬───────┬──────────┬──────────┐
││││2│2 3│2 3│2 3 5│2 3 5│2 3 5 7│2 3 5 7│2 3 5 7│2 3 5 7│2 3 5 7 11│2 3 5 7 11│
└┴┴┴─┴───┴───┴─────┴─────┴───────┴───────┴───────┴───────┴──────────┴──────────┘

   sieve 13
0 0 1 1 0 1 0 1 0 0 0 1 0

   +/∘sieve¨ 10*⍳10
0 4 25 168 1229 9592 78498 664579 5761455 50847534
```


The last expression computes the number of primes < 1e0 1e1 ... 1e9.
The last number 50847534 can perhaps be called the anti-Bertelsen's number (http://mathworld.wolfram.com/BertelsensNumber.html).

=={{Header|AppleScript}}==
{{incorrect|AppleScript|This version uses rem testing and so is a trial division algorithm, not a sieve of Eratosthenes.}}
Note: This version of Trial Division has something like O(n^(3/2) asymptotic execution complexity rather than O(n log (log n)) for the true sieve.


```applescript
to sieve(N)
	script
		on array()
			set L to {}

			repeat with i from 1 to N
				set end of L to i
			end repeat

			L
		end array
	end script

	set L to result's array()
	set item 1 of L to false

	repeat with x in L
		repeat with y in L
			try
				if (x < y ^ 2) then exit repeat
				if (x mod y = 0) then
					set x's contents to false
					exit repeat
				end if
			end try
		end repeat
	end repeat

	numbers in L
end sieve


sieve(1000)
```


{{out}}

```txt

{2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 127, 131, 137, 139, 149, 151, 157, 163, 167, 173, 179, 181, 191, 193, 197, 199, 211, 223, 227, 229, 233, 239, 241, 251, 257, 263, 269, 271, 277, 281, 283, 293, 307, 311, 313, 317, 331, 337, 347, 349, 353, 359, 367, 373, 379, 383, 389, 397, 401, 409, 419, 421, 431, 433, 439, 443, 449, 457, 461, 463, 467, 479, 487, 491, 499, 503, 509, 521, 523, 541, 547, 557, 563, 569, 571, 577, 587, 593, 599, 601, 607, 613, 617, 619, 631, 641, 643, 647, 653, 659, 661, 673, 677, 683, 691, 701, 709, 719, 727, 733, 739, 743, 751, 757, 761, 769, 773, 787, 797, 809, 811, 821, 823, 827, 829, 839, 853, 857, 859, 863, 877, 881, 883, 887, 907, 911, 919, 929, 937, 941, 947, 953, 967, 971, 977, 983, 991, 997}

```


=={{Header|AutoHotkey}}==
{{AutoHotkey case}}Source: [http://www.autohotkey.com/forum/topic44657.html AutoHotkey forum] by Laszlo

```autohotkey
MsgBox % "12345678901234567890`n" Sieve(20)

Sieve(n) { ; Sieve of Eratosthenes => string of 0|1 chars, 1 at position k: k is prime
   Static zero := 48, one := 49 ; Asc("0"), Asc("1")
   VarSetCapacity(S,n,one)
   NumPut(zero,S,0,"char")
   i := 2
   Loop % sqrt(n)-1 {
      If (NumGet(S,i-1,"char") = one)
         Loop % n//i
            If (A_Index > 1)
               NumPut(zero,S,A_Index*i-1,"char")
      i += 1+(i>2)
   }
   Return S
}
```

=={{Header|AutoIt}}==

```autoit>#include <Array.au3

$M = InputBox("Integer", "Enter biggest Integer")
Global $a[$M], $r[$M], $c = 1
For $i = 2 To $M -1
	If Not $a[$i] Then
		$r[$c] = $i
		$c += 1
		For $k = $i To $M -1 Step $i
			$a[$k] = True
		Next
	EndIf
Next
$r[0] = $c - 1
ReDim $r[$c]
_ArrayDisplay($r)
```



## AWK

An initial array holds all numbers 2..max (which is entered on stdin);
then all products of integers are deleted from it;
the remaining are displayed in the unsorted appearance of a hash table.
Here, the script is entered directly on the commandline,
and input entered on stdin:
 $ awk '{for(i=2;i<=$1;i++) a[i]=1;
 >       for(i=2;i<=sqrt($1);i++) for(j=2;j<=$1;j++) delete a[i*j];
 >       for(i in a) printf i" "}'
 100
 71 53 17 5 73 37 19 83 47 29 7 67 59 11 97 79 89 31 13 41 23 2 61 43 3

The following variant does not unset non-primes, but sets them to 0,
to preserve order in output:
 $ awk '{for(i=2;i<=$1;i++) a[i]=1;
 >       for(i=2;i<=sqrt($1);i++) for(j=2;j<=$1;j++) a[i*j]=0;
 >       for(i=2;i<=$1;i++) if(a[i])printf i" "}'
 100
 2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97

Now with the script from a file,
input from commandline as well as stdin,
and input is checked for valid numbers:

```awk

# usage:  gawk  -v n=101  -f sieve.awk

function sieve(n) { # print n,":"
	for(i=2; i<=n;      i++) a[i]=1;
	for(i=2; i<=sqrt(n);i++) for(j=2;j<=n;j++) a[i*j]=0;
	for(i=2; i<=n;      i++) if(a[i]) printf i" "
	print ""
}

BEGIN	{ print "Sieve of Eratosthenes:"
	  if(n>1) sieve(n)
	}

	{ n=$1+0 }
n<2	{ exit }
	{ sieve(n) }

END	{ print "Bye!" }

```


Here is an alternate version that uses an associative array to record composites with a prime dividing it.  It can be considered a slow version, as it does not cross out composites until needed.  This version assumes enough memory to hold all primes up to ULIMIT.  It prints out noncomposites greater than 1.

```awk

BEGIN {  ULIMIT=100

for ( n=1 ; (n++) < ULIMIT ; )
    if (n in S) {
        p = S[n]
        delete S[n]
        for ( m = n ; (m += p) in S ; )  { }
        S[m] = p
        }
    else  print ( S[(n+n)] = n )
}

```


==Bash==
''See solutions at [[{{FULLPAGENAME}}#UNIX Shell|UNIX Shell]].''


## BASIC

{{works with|FreeBASIC}}
{{works with|RapidQ}}

```freebasic
DIM n AS Integer, k AS Integer, limit AS Integer

INPUT "Enter number to search to: "; limit
DIM flags(limit) AS Integer

FOR n = 2 TO SQR(limit)
    IF flags(n) = 0 THEN
        FOR k = n*n TO limit STEP n
            flags(k) = 1
        NEXT k
    END IF
NEXT n

' Display the primes
FOR n = 2 TO limit
    IF flags(n) = 0 THEN PRINT n; ", ";
NEXT n
```


=
## Applesoft BASIC
=

```basic
10  INPUT "ENTER NUMBER TO SEARCH TO: ";LIMIT
20  DIM FLAGS(LIMIT)
30  FOR N = 2 TO SQR (LIMIT)
40  IF FLAGS(N) < > 0 GOTO 80
50  FOR K = N * N TO LIMIT STEP N
60  FLAGS(K) = 1
70  NEXT K
80  NEXT N
90  REM  DISPLAY THE PRIMES
100  FOR N = 2 TO LIMIT
110  IF FLAGS(N) = 0 THEN PRINT N;", ";
120  NEXT N
```


==={{header|IS-BASIC}}===
<lang IS-BASIC>100 PROGRAM "Sieve.bas"
110 LET LIMIT=100
120 NUMERIC T(1 TO LIMIT)
130 FOR I=1 TO LIMIT
140   LET T(I)=0
150 NEXT
160 FOR I=2 TO SQR(LIMIT)
170   IF T(I)<>1 THEN
180     FOR K=I*I TO LIMIT STEP I
190       LET T(K)=1
200     NEXT
210   END IF
220 NEXT
230 FOR I=2 TO LIMIT ! Display the primes
240   IF T(I)=0 THEN PRINT I;
250 NEXT
```


=
## Locomotive Basic
=

```locobasic
10 DEFINT a-z
20 INPUT "Limit";limit
30 DIM f(limit)
40 FOR n=2 TO SQR(limit)
50 IF f(n)=1 THEN 90
60 FOR k=n*n TO limit STEP n
70 f(k)=1
80 NEXT k
90 NEXT n
100 FOR n=2 TO limit
110 IF f(n)=0 THEN PRINT n;",";
120 NEXT
```


=
## MSX Basic
=

```MSX Basic
5 Rem MSX BRRJPA
10 INPUT "Search until: ";L
20 DIM p(L)
30 FOR n=2 TO SQR (L+1000)
40 IF p(n)<>0 THEN goto 80
50 FOR k=n*n TO L STEP n
60 LET p(k)=1
70 NEXT k
80 NEXT n
90 FOR n=2 TO L
100 IF p(n)=0 THEN PRINT n;", ";
110 NEXT n
```



=
## Sinclair ZX81 BASIC
=
If you only have 1k of RAM, this program will work—but you will only be able to sieve numbers up to 101. The program is therefore more useful if you have more memory available.

A note on <code>FAST</code> and <code>SLOW</code>: under normal circumstances the CPU spends about 3/4 of its time driving the display and only 1/4 doing everything else. Entering <code>FAST</code> mode blanks the screen (which we do not want to update anyway), resulting in substantially improved performance; we then return to <code>SLOW</code> mode when we have something to print out.

```basic
 10 INPUT L
 20 FAST
 30 DIM N(L)
 40 FOR I=2 TO SQR L
 50 IF N(I) THEN GOTO 90
 60 FOR J=I+I TO L STEP I
 70 LET N(J)=1
 80 NEXT J
 90 NEXT I
100 SLOW
110 FOR I=2 TO L
120 IF NOT N(I) THEN PRINT I;" ";
130 NEXT I
```


=
## ZX Spectrum Basic
=

```zxbasic
10 INPUT "Enter number to search to: ";l
20 DIM p(l)
30 FOR n=2 TO SQR l
40 IF p(n)<>0 THEN NEXT n
50 FOR k=n*n TO l STEP n
60 LET p(k)=1
70 NEXT k
80 NEXT n
90 REM Display the primes
100 FOR n=2 TO l
110 IF p(n)=0 THEN PRINT n;", ";
120 NEXT n
```



## BBC BASIC


```bbcbasic
      limit% = 100000
      DIM sieve% limit%

      prime% = 2
      WHILE prime%^2 < limit%
        FOR I% = prime%*2 TO limit% STEP prime%
          sieve%?I% = 1
        NEXT
        REPEAT prime% += 1 : UNTIL sieve%?prime%=0
      ENDWHILE

      REM Display the primes:
      FOR I% = 1 TO limit%
        IF sieve%?I% = 0 PRINT I%;
      NEXT
```



## Batch File


```dos
:: Sieve of Eratosthenes for Rosetta Code - PG
@echo off
setlocal ENABLEDELAYEDEXPANSION
setlocal ENABLEEXTENSIONS
rem echo on
set /p n=limit:
rem set n=100
for /L %%i in (1,1,%n%) do set crible.%%i=1
for /L %%i in (2,1,%n%) do (
  if !crible.%%i! EQU 1 (
    set /A w = %%i * 2
    for /L %%j in (!w!,%%i,%n%) do (
	  set crible.%%j=0
	)
  )
)
for /L %%i in (2,1,%n%) do (
  if !crible.%%i! EQU 1 echo %%i
)
pause
```

{{Out}}
<pre style="height:20ex">limit: 100
2
3
5
7
11
13
17
19
23
29
31
37
41
43
47
53
59
61
67
71
73
79
83
89
97
```



## Befunge

 2>:3g" "-!v\  g30          <
  |!`"O":+1_:.:03p>03g+:"O"`|
  @               ^  p3\" ":<
 2 234567890123456789012345678901234567890123456789012345678901234567890123456789


## Bracmat

This solution does not use an array. Instead, numbers themselves are used as variables. The numbers that are not prime are set (to the silly value "nonprime"). Finally all numbers up to the limit are tested for being initialised. The uninitialised (unset) ones must be the primes.

```bracmat
( ( eratosthenes
  =   n j i
    .   !arg:?n
      & 1:?i
      &   whl
        ' ( (1+!i:?i)^2:~>!n:?j
          & ( !!i
            |   whl
              ' ( !j:~>!n
                & nonprime:?!j
                & !j+!i:?j
                )
            )
          )
      & 1:?i
      &   whl
        ' ( 1+!i:~>!n:?i
          & (!!i|put$(!i " "))
          )
  )
& eratosthenes$100
)
```

{{out}}
2  3  5  7  11  13  17  19  23  29  31  37  41  43  47  53  59  61  67  71  73  79  83  89  97


## C

Plain sieve, without any optimizations:
```cpp
#include <iostream>
#include <math.h>

char*
eratosthenes(int n, int *c)
{
	char* sieve;
	int i, j, m;

	if(n < 2)
		return NULL;

	*c = n-1;     /* primes count */
	m = (int) sqrt((double) n);

	/* calloc initializes to zero */
	sieve = calloc(n+1,sizeof(char));
	sieve[0] = 1;
	sieve[1] = 1;
	for(i = 2; i <= m; i++)
		if(!sieve[i])
			for (j = i*i; j <= n; j += i)
				if(!sieve[j]){
					sieve[j] = 1;
					--(*c);
				}
  	return sieve;
}
```
Possible optimizations include sieving only odd numbers (or more complex wheels), packing the sieve into bits to improve locality (and allow larger sieves), etc.

'''Another example:'''

We first fill ones into an array and assume all numbers are prime.
Then, in a loop, fill zeroes into those places where i * j is less than or equal to n (number of primes requested), which means they have multiples!
To understand this better, look at the output of the following example.
To print this back, we look for ones in the array and only print those spots.
```c
#include <stdio.h>
#include <malloc.h>
void sieve(int *, int);

int main(int argc, char *argv)
{
    int *array, n=10;
    array =(int *)malloc((n + 1) * sizeof(int));
    sieve(array,n);
    return 0;
}

void sieve(int *a, int n)
{
    int i=0, j=0;

    for(i=2; i<=n; i++) {
        a[i] = 1;
    }

    for(i=2; i<=n; i++) {
        printf("\ni:%d", i);
        if(a[i] == 1) {
            for(j=i; (i*j)<=n; j++) {
                printf ("\nj:%d", j);
                printf("\nBefore a[%d*%d]: %d", i, j, a[i*j]);
                a[(i*j)] = 0;
                printf("\nAfter a[%d*%d]: %d", i, j, a[i*j]);
            }
        }
    }

    printf("\nPrimes numbers from 1 to %d are : ", n);
    for(i=2; i<=n; i++) {
        if(a[i] == 1)
            printf("%d, ", i);
    }
    printf("\n\n");
}
```
{{out}}
```Shell
i:2
j:2
Before a[2*2]: 1
After a[2*2]: 0
j:3
Before a[2*3]: 1
After a[2*3]: 0
j:4
Before a[2*4]: 1
After a[2*4]: 0
j:5
Before a[2*5]: 1
After a[2*5]: 0
i:3
j:3
Before a[3*3]: 1
After a[3*3]: 0
i:4
i:5
i:6
i:7
i:8
i:9
i:10
Primes numbers from 1 to 10 are : 2, 3, 5, 7,
```



## C++


### Standard Library


This implementation follows the standard library pattern of [http://en.cppreference.com/w/cpp/algorithm/iota std::iota]. The start and end iterators are provided for the container. The destination container is used for marking primes and then filled with the primes which are less than the container size. This method requires no memory allocation inside the function.


```cpp
#include <iostream>
#include <algorithm>
#include <vector>

// requires Iterator satisfies RandomAccessIterator
template <typename Iterator>
size_t prime_sieve(Iterator start, Iterator end)
{
    if (start == end) return 0;
    // clear the container with 0
    std::fill(start, end, 0);
    // mark composites with 1
    for (Iterator prime_it = start + 1; prime_it != end; ++prime_it)
    {
        if (*prime_it == 1) continue;
        // determine the prime number represented by this iterator location
        size_t stride = (prime_it - start) + 1;
        // mark all multiples of this prime number up to max
        Iterator mark_it = prime_it;
        while ((end - mark_it) > stride)
        {
            std::advance(mark_it, stride);
            *mark_it = 1;
        }
    }
    // copy marked primes into container
    Iterator out_it = start;
    for (Iterator it = start + 1; it != end; ++it)
    {
        if (*it == 0)
        {
            *out_it = (it - start) + 1;
            ++out_it;
        }
    }
    return out_it - start;
}

int main(int argc, const char* argv[])
{
    std::vector<int> primes(100);
    size_t count = prime_sieve(primes.begin(), primes.end());
    // display the primes
    for (size_t i = 0; i < count; ++i)
        std::cout << primes[i] << " ";
    std::cout << std::endl;
    return 1;
}
```



###  Boost



```cpp
// yield all prime numbers less than limit.
template<class UnaryFunction>
void primesupto(int limit, UnaryFunction yield)
{
  std::vector<bool> is_prime(limit, true);

  const int sqrt_limit = static_cast<int>(std::sqrt(limit));
  for (int n = 2; n <= sqrt_limit; ++n)
    if (is_prime[n]) {
	yield(n);

	for (unsigned k = n*n, ulim = static_cast<unsigned>(limit); k < ulim; k += n)
      //NOTE: "unsigned" is used to avoid an overflow in `k+=n` for `limit` near INT_MAX
	  is_prime[k] = false;
    }

  for (int n = sqrt_limit + 1; n < limit; ++n)
    if (is_prime[n])
	yield(n);
}
```


Full program:

{{works with|Boost}}
```cpp
/**
   $ g++ -I/path/to/boost sieve.cpp -o sieve && sieve 10000000
 */
#include <inttypes.h> // uintmax_t
#include <limits>
#include <cmath>
#include <iostream>
#include <sstream>
#include <vector>

#include <boost/lambda/lambda.hpp>

int main(int argc, char *argv[])
{
  using namespace std;
  using namespace boost::lambda;

  int limit = 10000;
  if (argc == 2) {
    stringstream ss(argv[--argc]);
    ss >> limit;

    if (limit < 1 or ss.fail()) {
      cerr << "USAGE:\n  sieve LIMIT\n\nwhere LIMIT in the range [1, "
	   << numeric_limits<int>::max() << ")" << endl;
      return 2;
    }
  }

  // print primes less then 100
  primesupto(100, cout << _1 << " ");
  cout << endl;

  // find number of primes less then limit and their sum
  int count = 0;
  uintmax_t sum = 0;
  primesupto(limit, (var(sum) += _1, var(count) += 1));

  cout << "limit sum pi(n)\n"
       << limit << " " << sum << " " << count << endl;
}
```


=={{header|C sharp|C#}}==
{{works with|C sharp|C#|2.0+}}

```csharp
using System;
using System.Collections;
using System.Collections.Generic;

namespace SieveOfEratosthenes
{
    class Program
    {
        static void Main(string[] args)
        {
            int maxprime = int.Parse(args[0]);
            var primelist = GetAllPrimesLessThan(maxprime);
            foreach (int prime in primelist)
            {
                Console.WriteLine(prime);
            }
            Console.WriteLine("Count = " + primelist.Count);
            Console.ReadLine();
        }

        private static List<int> GetAllPrimesLessThan(int maxPrime)
        {
            var primes = new List<int>();
            var maxSquareRoot = (int)Math.Sqrt(maxPrime);
            var eliminated = new BitArray(maxPrime + 1);

            for (int i = 2; i <= maxPrime; ++i)
            {
                if (!eliminated[i])
                {
                    primes.Add(i);
                    if (i <= maxSquareRoot)
                    {
                        for (int j = i * i; j <= maxPrime; j += i)
                        {
                            eliminated[j] = true;
                        }
                    }
                }
            }
            return primes;
        }
    }
}
```



### Unbounded


'''Richard Bird Sieve'''

{{trans|F#}}

To show that C# code can be written in somewhat functional paradigms, the following in an implementation of the Richard Bird sieve from the Epilogue of [Melissa E. O'Neill's definitive article](http://www.cs.hmc.edu/~oneill/papers/Sieve-JFP.pdf) in Haskell:

```csharp
using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using PrimeT = System.UInt32;
  class PrimesBird : IEnumerable<PrimeT> {
    private struct CIS<T> {
      public T v; public Func<CIS<T>> cont;
      public CIS(T v, Func<CIS<T>> cont) {
        this.v = v; this.cont = cont;
      }
    }
    private CIS<PrimeT> pmlts(PrimeT p) {
      Func<PrimeT, CIS<PrimeT>> fn = null;
      fn = (c) => new CIS<PrimeT>(c, () => fn(c + p));
      return fn(p * p);
    }
    private CIS<CIS<PrimeT>> allmlts(CIS<PrimeT> ps) {
      return new CIS<CIS<PrimeT>>(pmlts(ps.v), () => allmlts(ps.cont())); }
    private CIS<PrimeT> merge(CIS<PrimeT> xs, CIS<PrimeT> ys) {
      var x = xs.v; var y = ys.v;
      if (x < y) return new CIS<PrimeT>(x, () => merge(xs.cont(), ys));
      else if (y < x) return new CIS<PrimeT>(y, () => merge(xs, ys.cont()));
      else return new CIS<PrimeT>(x, () => merge(xs.cont(), ys.cont()));
    }
    private CIS<PrimeT> cmpsts(CIS<CIS<PrimeT>> css) {
      return new CIS<PrimeT>(css.v.v, () => merge(css.v.cont(), cmpsts(css.cont()))); }
    private CIS<PrimeT> minusat(PrimeT n, CIS<PrimeT> cs) {
      var nn = n; var ncs = cs;
      for (; ; ++nn) {
        if (nn >= ncs.v) ncs = ncs.cont();
        else return new CIS<PrimeT>(nn, () => minusat(++nn, ncs));
      }
    }
    private CIS<PrimeT> prms() {
      return new CIS<PrimeT>(2, () => minusat(3, cmpsts(allmlts(prms())))); }
    public IEnumerator<PrimeT> GetEnumerator() {
      for (var ps = prms(); ; ps = ps.cont()) yield return ps.v;
    }
    IEnumerator IEnumerable.GetEnumerator() { return (IEnumerator)GetEnumerator(); }
  }
```


'''Tree Folding Sieve'''

{{trans|F#}}

The above code can easily be converted to "'''odds-only'''" and a infinite tree-like folding scheme with the following minor changes:

```csharp
using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using PrimeT = System.UInt32;
  class PrimesTreeFold : IEnumerable<PrimeT> {
    private struct CIS<T> {
      public T v; public Func<CIS<T>> cont;
      public CIS(T v, Func<CIS<T>> cont) {
        this.v = v; this.cont = cont;
      }
    }
    private CIS<PrimeT> pmlts(PrimeT p) {
      var adv = p + p;
      Func<PrimeT, CIS<PrimeT>> fn = null;
      fn = (c) => new CIS<PrimeT>(c, () => fn(c + adv));
      return fn(p * p);
    }
    private CIS<CIS<PrimeT>> allmlts(CIS<PrimeT> ps) {
      return new CIS<CIS<PrimeT>>(pmlts(ps.v), () => allmlts(ps.cont()));
    }
    private CIS<PrimeT> merge(CIS<PrimeT> xs, CIS<PrimeT> ys) {
      var x = xs.v; var y = ys.v;
      if (x < y) return new CIS<PrimeT>(x, () => merge(xs.cont(), ys));
      else if (y < x) return new CIS<PrimeT>(y, () => merge(xs, ys.cont()));
      else return new CIS<PrimeT>(x, () => merge(xs.cont(), ys.cont()));
    }
    private CIS<CIS<PrimeT>> pairs(CIS<CIS<PrimeT>> css) {
      var nxtcss = css.cont();
      return new CIS<CIS<PrimeT>>(merge(css.v, nxtcss.v), () => pairs(nxtcss.cont())); }
    private CIS<PrimeT> cmpsts(CIS<CIS<PrimeT>> css) {
      return new CIS<PrimeT>(css.v.v, () => merge(css.v.cont(), cmpsts(pairs(css.cont()))));
    }
    private CIS<PrimeT> minusat(PrimeT n, CIS<PrimeT> cs) {
      var nn = n; var ncs = cs;
      for (; ; nn += 2) {
        if (nn >= ncs.v) ncs = ncs.cont();
        else return new CIS<PrimeT>(nn, () => minusat(nn + 2, ncs));
      }
    }
    private CIS<PrimeT> oddprms() {
      return new CIS<PrimeT>(3, () => minusat(5, cmpsts(allmlts(oddprms()))));
    }
    public IEnumerator<PrimeT> GetEnumerator() {
      yield return 2;
      for (var ps = oddprms(); ; ps = ps.cont()) yield return ps.v;
    }
    IEnumerator IEnumerable.GetEnumerator() { return (IEnumerator)GetEnumerator(); }
  }
```


The above code runs over ten times faster than the original Richard Bird algorithm.

'''Priority Queue Sieve'''

{{trans|F#}}

First, an implementation of a Min Heap Priority Queue is provided as extracted from the entry at [http://rosettacode.org/wiki/Priority_queue#C.23 RosettaCode], with only the necessary methods duplicated here:

```csharp
namespace PriorityQ {
  using KeyT = System.UInt32;
  using System;
  using System.Collections.Generic;
  using System.Linq;
  class Tuple<K, V> { // for DotNet 3.5 without Tuple's
    public K Item1; public V Item2;
    public Tuple(K k, V v) { Item1 = k; Item2 = v; }
    public override string ToString() {
      return "(" + Item1.ToString() + ", " + Item2.ToString() + ")";
    }
  }
  class MinHeapPQ<V> {
    private struct HeapEntry {
      public KeyT k; public V v;
      public HeapEntry(KeyT k, V v) { this.k = k; this.v = v; }
    }
    private List<HeapEntry> pq;
    private MinHeapPQ() { this.pq = new List<HeapEntry>(); }
    private bool mt { get { return pq.Count == 0; } }
    private Tuple<KeyT, V> pkmn {
      get {
        if (pq.Count == 0) return null;
        else {
          var mn = pq[0];
          return new Tuple<KeyT, V>(mn.k, mn.v);
        }
      }
    }
    private void psh(KeyT k, V v) { // add extra very high item if none
      if (pq.Count == 0) pq.Add(new HeapEntry(UInt32.MaxValue, v));
      var i = pq.Count; pq.Add(pq[i - 1]); // copy bottom item...
      for (var ni = i >> 1; ni > 0; i >>= 1, ni >>= 1) {
        var t = pq[ni - 1];
        if (t.k > k) pq[i - 1] = t; else break;
      }
      pq[i - 1] = new HeapEntry(k, v);
    }
    private void siftdown(KeyT k, V v, int ndx) {
      var cnt = pq.Count - 1; var i = ndx;
      for (var ni = i + i + 1; ni < cnt; ni = ni + ni + 1) {
        var oi = i; var lk = pq[ni].k; var rk = pq[ni + 1].k;
        var nk = k;
        if (k > lk) { i = ni; nk = lk; }
        if (nk > rk) { ni += 1; i = ni; }
        if (i != oi) pq[oi] = pq[i]; else break;
      }
      pq[i] = new HeapEntry(k, v);
    }
    private void rplcmin(KeyT k, V v) {
      if (pq.Count > 1) siftdown(k, v, 0); }
    public static MinHeapPQ<V> empty { get { return new MinHeapPQ<V>(); } }
    public static Tuple<KeyT, V> peekMin(MinHeapPQ<V> pq) { return pq.pkmn; }
    public static MinHeapPQ<V> push(KeyT k, V v, MinHeapPQ<V> pq) {
      pq.psh(k, v); return pq; }
    public static MinHeapPQ<V> replaceMin(KeyT k, V v, MinHeapPQ<V> pq) {
      pq.rplcmin(k, v); return pq; }
}
```


The following code implements an improved version of the '''odds-only''' O'Neil algorithm, which provides the improvements of only adding base prime composite number streams to the queue when the sieved number reaches the square of the base prime (saving a huge amount of memory and considerable execution time, including not needing as wide a range of a type for the internal prime numbers) as well as minimizing stream processing using fusion:

```csharp
using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using PrimeT = System.UInt32;
  class PrimesPQ : IEnumerable<PrimeT> {
    private IEnumerator<PrimeT> nmrtr() {
      MinHeapPQ<PrimeT> pq = MinHeapPQ<PrimeT>.empty;
      PrimeT bp = 3; PrimeT q = 9;
      IEnumerator<PrimeT> bps = null;
      yield return 2; yield return 3;
      for (var n = (PrimeT)5; ; n += 2) {
        if (n >= q) { // always equal or less...
          if (q <= 9) {
            bps = nmrtr();
            bps.MoveNext(); bps.MoveNext(); } // move to 3...
          bps.MoveNext(); var nbp = bps.Current; q = nbp * nbp;
          var adv = bp + bp; bp = nbp;
          pq = MinHeapPQ<PrimeT>.push(n + adv, adv, pq);
        }
        else {
          var pk = MinHeapPQ<PrimeT>.peekMin(pq);
          var ck = (pk == null) ? q : pk.Item1;
          if (n >= ck) {
            do { var adv = pk.Item2;
                  pq = MinHeapPQ<PrimeT>.replaceMin(ck + adv, adv, pq);
                  pk = MinHeapPQ<PrimeT>.peekMin(pq); ck = pk.Item1;
            } while (n >= ck);
          }
          else yield return n;
        }
      }
    }
    public IEnumerator<PrimeT> GetEnumerator() { return nmrtr(); }
    IEnumerator IEnumerable.GetEnumerator() { return (IEnumerator)GetEnumerator(); }
  }
```


The above code is at least about 2.5 times faster than the Tree Folding version.

'''Dictionary (Hash table) Sieve'''

The above code adds quite a bit of overhead in having to provide a version of a Priority Queue for little advantage over a Dictionary (hash table based) version as per the code below:

```csharp
using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using PrimeT = System.UInt32;
  class PrimesDict : IEnumerable<PrimeT> {
    private IEnumerator<PrimeT> nmrtr() {
      Dictionary<PrimeT, PrimeT> dct = new Dictionary<PrimeT, PrimeT>();
      PrimeT bp = 3; PrimeT q = 9;
      IEnumerator<PrimeT> bps = null;
      yield return 2; yield return 3;
      for (var n = (PrimeT)5; ; n += 2) {
        if (n >= q) { // always equal or less...
          if (q <= 9) {
            bps = nmrtr();
            bps.MoveNext(); bps.MoveNext();
          } // move to 3...
          bps.MoveNext(); var nbp = bps.Current; q = nbp * nbp;
          var adv = bp + bp; bp = nbp;
          dct.Add(n + adv, adv);
        }
        else {
          if (dct.ContainsKey(n)) {
            PrimeT nadv; dct.TryGetValue(n, out nadv); dct.Remove(n); var nc = n + nadv;
            while (dct.ContainsKey(nc)) nc += nadv;
            dct.Add(nc, nadv);
          }
          else yield return n;
        }
      }
    }
    public IEnumerator<PrimeT> GetEnumerator() { return nmrtr(); }
    IEnumerator IEnumerable.GetEnumerator() { return (IEnumerator)GetEnumerator(); }
  }
```


The above code runs in about three quarters of the time as the above Priority Queue based version for a range of a million primes which will fall even further behind for increasing ranges due to the Dictionary providing O(1) access times as compared to the O(log n) access times for the Priority Queue; the only slight advantage of the PQ based version is at very small ranges where the constant factor overhead of computing the table hashes becomes greater than the "log n" factor for small "n".

'''Page Segmented Array Sieve'''

All of the above unbounded versions are really just an intellectual exercise as with very little extra lines of code above the fastest Dictionary based version, one can have an bit-packed page-segmented array based version as follows:

```csharp
using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using PrimeT = System.UInt32;
  class PrimesPgd : IEnumerable<PrimeT> {
    private const int PGSZ = 1 << 14; // L1 CPU cache size in bytes
    private const int BFBTS = PGSZ * 8; // in bits
    private const int BFRNG = BFBTS * 2;
    public IEnumerator<PrimeT> nmrtr() {
      IEnumerator<PrimeT> bps = null;
      List<uint> bpa = new List<uint>();
      uint[] cbuf = new uint[PGSZ / 4]; // 4 byte words
      yield return 2;
      for (var lowi = (PrimeT)0; ; lowi += BFBTS) {
        for (var bi = 0; ; ++bi) {
          if (bi < 1) {
            if (bi < 0) { bi = 0; yield return 2; }
            PrimeT nxt = 3 + lowi + lowi + BFRNG;
            if (lowi <= 0) { // cull very first page
              for (int i = 0, p = 3, sqr = 9; sqr < nxt; i++, p += 2, sqr = p * p)
                if ((cbuf[i >> 5] & (1 << (i & 31))) == 0)
                  for (int j = (sqr - 3) >> 1; j < BFBTS; j += p)
                    cbuf[j >> 5] |= 1u << j;
            }
            else { // cull for the rest of the pages
              Array.Clear(cbuf, 0, cbuf.Length);
              if (bpa.Count == 0) { // inite secondar base primes stream
                bps = nmrtr(); bps.MoveNext(); bps.MoveNext();
                bpa.Add((uint)bps.Current); bps.MoveNext();
              } // add 3 to base primes array
              // make sure bpa contains enough base primes...
              for (PrimeT p = bpa[bpa.Count - 1], sqr = p * p; sqr < nxt; ) {
                p = bps.Current; bps.MoveNext(); sqr = p * p; bpa.Add((uint)p);
              }
              for (int i = 0, lmt = bpa.Count - 1; i < lmt; i++) {
                var p = (PrimeT)bpa[i]; var s = (p * p - 3) >> 1;
                // adjust start index based on page lower limit...
                if (s >= lowi) s -= lowi;
                else {
                  var r = (lowi - s) % p;
                  s = (r != 0) ? p - r : 0;
                }
                for (var j = (uint)s; j < BFBTS; j += p)
                  cbuf[j >> 5] |= 1u << ((int)j);
              }
            }
          }
          while (bi < BFBTS && (cbuf[bi >> 5] & (1 << (bi & 31))) != 0) ++bi;
          if (bi < BFBTS) yield return 3 + (((PrimeT)bi + lowi) << 1);
          else break; // outer loop for next page segment...
        }
      }
    }
    public IEnumerator<PrimeT> GetEnumerator() { return nmrtr(); }
    IEnumerator IEnumerable.GetEnumerator() { return (IEnumerator)GetEnumerator(); }
  }
```


The above code is about 25 times faster than the Dictionary version at computing the first about 50 million primes (up to a range of one billion), with the actual enumeration of the result sequence now taking longer than the time it takes to cull the composite number representation bits from the arrays, meaning that it is over 50 times faster at actually sieving the primes.  The code owes its speed as compared to a naive "one huge memory array" algorithm to using an array size that is the size of the CPU L1 or L2 caches and using bit-packing to fit more number representations into this limited capacity; in this way RAM memory access times are reduced by a factor of from about four to about 10 (depending on CPU and RAM speed) as compared to those naive implementations, and the minor computational cost of the bit manipulations is compensated by a large factor in total execution time.

The time to enumerate the result primes sequence can be reduced somewhat (about a second) by removing the automatic iterator "yield return" statements and converting them into a "rull-your-own" IEnumerable<PrimeT> implementation, but for page segmentation of '''odds-only''', this iteration of the results will still take longer than the time to actually cull the composite numbers from the page arrays.

In order to make further gains in speed, custom methods must be used to avoid using iterator sequences.  If this is done, then further gains can be made by extreme wheel factorization (up to about another about four times gain in speed) and multi-processing (with another gain in speed proportional to the actual independent CPU cores used).

Note that all of these gains in speed are not due to C# other than it compiles to reasonably efficient machine code, but rather to proper use of the Sieve of Eratosthenes algorithm.

All of the above unbounded code can be tested by the following "main" method (replace the name "PrimesXXX" with the name of the class to be tested):

```csharp
    static void Main(string[] args) {
      Console.WriteLine(PrimesXXX().ElementAt(1000000 - 1)); // zero based indexing...
    }
```


To produce the following output for all tested versions (although some are considerably faster than others):
{{output}}

```txt
15485863
```



## Chapel

This solution uses nested iterators to create new wheels at run time:

```chapel
// yield prime and remove all multiples of it from children sieves
iter sieve(prime):int {

        yield prime;

        var last = prime;
        label candidates for candidate in sieve(prime+1) do {
                for composite in last..candidate by prime do {

                        // candidate is a multiple of this prime
                        if composite == candidate then {
                                // remember size of last composite
                                last = composite;
                                // and try the next candidate
                                continue candidates;
                        }
                }

                // candidate cannot need to be removed by this sieve
                // yield to parent sieve for checking
                yield candidate;
        }
}
```
The topmost sieve needs to be started with 2 (the smallest prime):

```chapel
config const N = 30;
for p in sieve(2) {
        write(" ", p);
        if p > N then {
                writeln();
                break;
        }
}
```



## Clojure

{{incorrect|Clojure|The first version uses rem testing and so is a trial division algorithm, not a sieve of Eratosthenes.}}
Note: this is not a Sieve of Eratosthenes; it is just an inefficient version (because it doesn't trial just the found primes <= the square root of the range) trial division.

''primes<'' is a functional interpretation of the Sieve of Eratosthenes. It merely removes the set of composite numbers from the set of odd numbers (wheel of 2) leaving behind only prime numbers. It uses a transducer internally with "into #{}".

```clojure

(defn primes< [n]
  (if (<= n 2)
    ()
    (remove (into #{}
                  (mapcat #(range (* % %) n %))
                  (range 3 (Math/sqrt n) 2))
            (cons 2 (range 3 n 2)))))

```


Calculates primes up to and including ''n'' using a mutable boolean array but otherwise entirely functional code.

```clojure

(defn primes-to
  "Computes lazy sequence of prime numbers up to a given number using sieve of Eratosthenes"
  [n]
  (let [root (-> n Math/sqrt long),
        cmpsts (boolean-array (inc n)),
        cullp (fn [p]
                (loop [i (* p p)]
                  (if (<= i n)
                    (do (aset cmpsts i true)
                        (recur (+ i p))))))]
    (do (dorun (map #(cullp %) (filter #(not (aget cmpsts %))
                                       (range 2 (inc root)))))
        (filter #(not (aget cmpsts %)) (range 2 (inc n))))))

```


'''Alternative implementation using Clojure's side-effect oriented list comprehension.'''


```clojure

(defn primes-to
  "Returns a lazy sequence of prime numbers less than lim"
  [lim]
  (let [refs (boolean-array (+ lim 1) true)
        root (int (Math/sqrt lim))]
    (do (doseq [i (range 2 lim)
                :while (<= i root)
                :when (aget refs i)]
          (doseq [j (range (* i i) lim i)]
            (aset refs j false)))
        (filter #(aget refs %) (range 2 lim)))))

```


'''Alternative implementation using Clojure's side-effect oriented list comprehension. Odds only.'''

```clojure

(defn primes-to
  "Returns a lazy sequence of prime numbers less than lim"
  [lim]
  (let [max-i (int (/ (- lim 1) 2))
        refs (boolean-array max-i true)
        root (/ (dec (int (Math/sqrt lim))) 2)]
    (do (doseq [i (range 1 (inc root))
                :when (aget refs i)]
          (doseq [j (range (* (+ i i) (inc i)) max-i (+ i i 1))]
            (aset refs j false)))
        (cons 2 (map #(+ % % 1) (filter #(aget refs %) (range 1 max-i)))))))

```

This implemantation is about twice fast than previous one and use only half memory.
From the index of array calculates the value it represents as (2*i + 1), the step between two index that represents
the multiples of primes to mark as composite is also (2*i + 1).
The index of the square of the prime to start composite marking is 2*i*(i+1).

'''Alternative very slow entirely functional implementation using lazy sequences'''


```clojure

(defn primes-to
  "Computes lazy sequence of prime numbers up to a given number using sieve of Eratosthenes"
  [n]
  (letfn [(nxtprm [cs] ; current candidates
            (let [p (first cs)]
              (if (> p (Math/sqrt n)) cs
                (cons p (lazy-seq (nxtprm (-> (range (* p p) (inc n) p)
                                              set (remove cs) rest)))))))]
    (nxtprm (range 2 (inc n)))))

```


The reason that the above code is so slow is that it has has a high constant factor overhead due to using a (hash) set to remove the composites from the future composites stream, each prime composite stream removal requires a scan across all remaining composites (compared to using an array or vector where only the culled values are referenced, and due to the slowness of Clojure sequence operations as compared to iterator/sequence operations in other languages.

'''Version based on immutable Vector's'''

Here is an immutable boolean vector based non-lazy sequence version other than for the lazy sequence operations to output the result:

```clojure

(defn primes-to
  "Computes lazy sequence of prime numbers up to a given number using sieve of Eratosthenes"
  [max-prime]
  (let [sieve (fn [s n]
                (if (<= (* n n) max-prime)
                  (recur (if (s n)
                           (reduce #(assoc %1 %2 false) s (range (* n n) (inc max-prime) n))
                           s)
                         (inc n))
                  s))]
    (->> (-> (reduce conj (vector-of :boolean) (map #(= % %) (range (inc max-prime))))
             (assoc 0 false)
             (assoc 1 false)
             (sieve 2))
         (map-indexed #(vector %2 %1)) (filter first) (map second))))

```


The above code is still quite slow due to the cost of the immutable copy-on-modify operations.

'''Odds only bit packed mutable array based version'''

The following code implements an odds-only sieve using a mutable bit packed long array, only using a lazy sequence for the output of the resulting primes:

```clojure

(set! *unchecked-math* true)

(defn primes-to
  "Computes lazy sequence of prime numbers up to a given number using sieve of Eratosthenes"
  [n]
  (let [root (-> n Math/sqrt long),
        rootndx (long (/ (- root 3) 2)),
        ndx (long (/ (- n 3) 2)),
        cmpsts (long-array (inc (/ ndx 64))),
        isprm #(zero? (bit-and (aget cmpsts (bit-shift-right % 6))
                               (bit-shift-left 1 (bit-and % 63)))),
        cullp (fn [i]
                (let [p (long (+ i i 3))]
	                (loop [i (bit-shift-right (- (* p p) 3) 1)]
	                  (if (<= i ndx)
	                    (do (let [w (bit-shift-right i 6)]
	                    (aset cmpsts w (bit-or (aget cmpsts w)
	                                           (bit-shift-left 1 (bit-and i 63)))))
	                        (recur (+ i p))))))),
        cull (fn [] (loop [i 0] (if (<= i rootndx)
                                  (do (if (isprm i) (cullp i)) (recur (inc i))))))]
    (letfn [(nxtprm [i] (if (<= i ndx)
                          (cons (+ i i 3) (lazy-seq (nxtprm (loop [i (inc i)]
                                                              (if (or (> i ndx) (isprm i)) i
                                                                (recur (inc i)))))))))]
      (if (< n 2) nil
        (cons 3 (if (< n 3) nil (do (cull) (lazy-seq (nxtprm 0)))))))))

```


The above code is about as fast as any "one large sieving array" type of program in any computer language with this level of wheel factorization other than the lazy sequence operations are quite slow:  it takes about ten times as long to enumerate the results as it does to do the actual sieving work of culling the composites from the sieving buffer array.  The slowness of sequence operations is due to nested function calls, but primarily due to the way Clojure implements closures by "boxing" all arguments (and perhaps return values) as objects in the heap space, which then need to be "un-boxed" as primitives as necessary for integer operations.  Some of the facilities provided by lazy sequences are not needed for this algorithm, such as the automatic memoization which means that each element of the sequence is calculated only once; it is not necessary for the sequence values to be retraced for this algorithm.

If further levels of wheel factorization were used, the time to enumerate the resulting primes would be an even higher overhead as compared to the actual composite number culling time, would get even higher if page segmentation were used to limit the buffer size to the size of the CPU L1 cache for many times better memory access times, most important in the culling operations, and yet higher again if multi-processing were used to share to page segment processing across CPU cores.

The following code overcomes many of those limitations by using an internal (OPSeq) "deftype" which implements the ISeq interface as well as the Counted interface to provide immediate count returns (based on a pre-computed total), as well as the IReduce interface which can greatly speed come computations based on the primes sequence (eased greatly using facilities provided by Clojure 1.7.0 and up):

```clojure

(defn primes-tox
  "Computes lazy sequence of prime numbers up to a given number using sieve of Eratosthenes"
  [n]
  (let [root (-> n Math/sqrt long),
        rootndx (long (/ (- root 3) 2)),
        ndx (max (long (/ (- n 3) 2)) 0),
        lmt (quot ndx 64),
        cmpsts (long-array (inc lmt)),
        cullp (fn [i]
                (let [p (long (+ i i 3))]
	                (loop [i (bit-shift-right (- (* p p) 3) 1)]
	                  (if (<= i ndx)
	                    (do (let [w (bit-shift-right i 6)]
                            (aset cmpsts w (bit-or (aget cmpsts w)
                                                   (bit-shift-left 1 (bit-and i 63)))))
                          (recur (+ i p))))))),
        cull (fn [] (do (aset cmpsts lmt (bit-or (aget cmpsts lmt)
                                                 (bit-shift-left -2 (bit-and ndx 63))))
                        (loop [i 0]
                          (when (<= i rootndx)
                            (when (zero? (bit-and (aget cmpsts (bit-shift-right i 6))
                                                   (bit-shift-left 1 (bit-and i 63))))
                              (cullp i))
                            (recur (inc i))))))
        numprms (fn []
                  (let [w (dec (alength cmpsts))] ;; fast results count bit counter
                    (loop [i 0, cnt (bit-shift-left (alength cmpsts) 6)]
                      (if (> i w) cnt
                        (recur (inc i)
                               (- cnt (java.lang.Long/bitCount (aget cmpsts i))))))))]
    (if (< n 2) nil
      (cons 2 (if (< n 3) nil
                (do (cull)
                    (deftype OPSeq [^long i ^longs cmpsa ^long cnt ^long tcnt] ;; for arrays maybe need to embed the array so that it doesn't get garbage collected???
                      clojure.lang.ISeq
                        (first [_] (if (nil? cmpsa) nil (+ i i 3)))
                        (next [_] (let [ncnt (inc cnt)] (if (>= ncnt tcnt) nil
                                                          (OPSeq.
                                                            (loop [j (inc i)]
                                                              (let [p? (zero? (bit-and (aget cmpsa (bit-shift-right j 6))
                                                                                       (bit-shift-left 1 (bit-and j 63))))]
                                                                (if p? j (recur (inc j)))))
                                                            cmpsa ncnt tcnt))))
                        (more [this] (let [ncnt (inc cnt)] (if (>= ncnt tcnt) (OPSeq. 0 nil tcnt tcnt)
                                                             (.next this))))
                        (cons [this o] (clojure.core/cons o this))
                        (empty [_] (if (= cnt tcnt) nil (OPSeq. 0 nil tcnt tcnt)))
                        (equiv [this o] (if (or (not= (type this) (type o))
                                                (not= cnt (.cnt ^OPSeq o)) (not= tcnt (.tcnt ^OPSeq o))
                                                (not= i (.i ^OPSeq o))) false true))
                        clojure.lang.Counted
                          (count [_] (- tcnt cnt))
                        clojure.lang.Seqable
                          (clojure.lang.Seqable/seq [this] (if (= cnt tcnt) nil this))
                        clojure.lang.IReduce
                          (reduce [_ f v] (let [c (- tcnt cnt)]
                                            (if (<= c 0) nil
                                              (loop [ci i, n c, rslt v]
                                                (if (zero? (bit-and (aget cmpsa (bit-shift-right ci 6))
                                                                    (bit-shift-left 1 (bit-and ci 63))))
                                                  (let [rrslt (f rslt (+ ci ci 3)),
                                                        rdcd (reduced? rrslt),
                                                        nrslt (if rdcd @rrslt rrslt)]
                                                    (if (or (<= n 1) rdcd) nrslt
                                                      (recur (inc ci) (dec n) nrslt)))
                                                  (recur (inc ci) n rslt))))))
                          (reduce [this f] (if (nil? i) (f) (if (= (.count this) 1) (+ i i 3)
                                                              (.reduce ^clojure.lang.IReduce (.next this) f (+ i i 3)))))
                        clojure.lang.Sequential
                        Object
                          (toString [this] (if (= cnt tcnt) "()"
                                             (.toString (seq (map identity this))))))
                    (->OPSeq 0 cmpsts 0 (numprms))))))))

```


'(time (count (primes-tox 10000000)))' takes about 40 milliseconds (compiled) to produce 664579.

Due to the better efficiency of the custom CIS type, the primes to the above range can be enumerated in about the same 40 milliseconds that it takes to cull and count the sieve buffer array.

Under Clojure 1.7.0, one can use '(time (reduce (fn [] (+ (long sum) (long v))) 0 (primes-tox 2000000)))' to find "142913828922" as the sum of the primes to two million as per [https://projecteuler.net/problem=10 Euler Problem 10] in about 40 milliseconds total with about half the time used for sieving the array and half for computing the sum.

To show how sensitive Clojure is to forms of expression of functions, the simple form '(time (reduce + (primes-tox 2000000)))' takes about twice as long even though it is using the same internal routine for most of the calculation due to the function not having the type coercion's.

Before one considers that this code is suitable for larger ranges, it is still lacks the improvements of page segmentation with pages about the size of the CPU L1/L2 caches (produces about a four times speed up), maximal wheel factorization (to make it another about four times faster), and the use of multi-processing (for a further gain of about 4 times for a multi-core desktop CPU such as an Intel i7), will make the sieving/counting code about 50 times faster than this, although there will only be a moderate improvement in the time to enumerate/process the resulting primes.  Using these techniques, the number of primes to one billion can be counted in a small fraction of a second.


### Unbounded Versions

For some types of problems such as finding the nth prime (rather than the sequence of primes up to m), a prime sieve with no upper bound is a better tool.

The following variations on an incremental Sieve of Eratosthenes are based on or derived from the Richard Bird sieve as described in the Epilogue of [http://www.cs.hmc.edu/~oneill/papers/Sieve-JFP.pdf Melissa E. O'Neill's definitive paper]:

'''A Clojure version of Richard Bird's Sieve using Lazy Sequences (sieves odds only)'''

```clojure

(defn primes-Bird
  "Computes the unbounded sequence of primes using a Sieve of Eratosthenes algorithm by Richard Bird."
  []
  (letfn [(mltpls [p] (let [p2 (* 2 p)]
                        (letfn [(nxtmltpl [c]
                                  (cons c (lazy-seq (nxtmltpl (+ c p2)))))]
                          (nxtmltpl (* p p))))),
          (allmtpls [ps] (cons (mltpls (first ps)) (lazy-seq (allmtpls (next ps))))),
          (union [xs ys] (let [xv (first xs), yv (first ys)]
                           (if (< xv yv) (cons xv (lazy-seq (union (next xs) ys)))
                             (if (< yv xv) (cons yv (lazy-seq (union xs (next ys))))
                               (cons xv (lazy-seq (union (next xs) (next ys)))))))),
          (mrgmltpls [mltplss] (cons (first (first mltplss))
                                     (lazy-seq (union (next (first mltplss))
                                                      (mrgmltpls (next mltplss)))))),
          (minusStrtAt [n cmpsts] (loop [n n, cmpsts cmpsts]
                                    (if (< n (first cmpsts))
                                      (cons n (lazy-seq (minusStrtAt (+ n 2) cmpsts)))
                                      (recur (+ n 2) (next cmpsts)))))]
    (do (def oddprms (cons 3 (lazy-seq (let [cmpsts (-> oddprms (allmtpls) (mrgmltpls))]
                                         (minusStrtAt 5 cmpsts)))))
        (cons 2 (lazy-seq oddprms)))))

```


The above code is quite slow due to both that the data structure is a linear merging of prime multiples and due to the slowness of the Clojure sequence operations.

'''A Clojure version of the tree folding sieve using Lazy Sequences'''

The following code speeds up the above code by merging the linear sequence of sequences as above by pairs into a right-leaning tree structure:

```clojure

(defn primes-treeFolding
  "Computes the unbounded sequence of primes using a Sieve of Eratosthenes algorithm modified from Bird."
  []
  (letfn [(mltpls [p] (let [p2 (* 2 p)]
                        (letfn [(nxtmltpl [c]
                                  (cons c (lazy-seq (nxtmltpl (+ c p2)))))]
                          (nxtmltpl (* p p))))),
          (allmtpls [ps] (cons (mltpls (first ps)) (lazy-seq (allmtpls (next ps))))),
          (union [xs ys] (let [xv (first xs), yv (first ys)]
                           (if (< xv yv) (cons xv (lazy-seq (union (next xs) ys)))
                             (if (< yv xv) (cons yv (lazy-seq (union xs (next ys))))
                               (cons xv (lazy-seq (union (next xs) (next ys)))))))),
          (pairs [mltplss] (let [tl (next mltplss)]
                             (cons (union (first mltplss) (first tl))
                                   (lazy-seq (pairs (next tl)))))),
          (mrgmltpls [mltplss] (cons (first (first mltplss))
                                     (lazy-seq (union (next (first mltplss))
                                                      (mrgmltpls (pairs (next mltplss))))))),
          (minusStrtAt [n cmpsts] (loop [n n, cmpsts cmpsts]
                                    (if (< n (first cmpsts))
                                      (cons n (lazy-seq (minusStrtAt (+ n 2) cmpsts)))
                                      (recur (+ n 2) (next cmpsts)))))]
    (do (def oddprms (cons 3 (lazy-seq (let [cmpsts (-> oddprms (allmtpls) (mrgmltpls))]
                                         (minusStrtAt 5 cmpsts)))))
        (cons 2 (lazy-seq oddprms)))))

```


The above code is still slower than it should be due to the slowness of Clojure's sequence operations.

'''A Clojure version of the above tree folding sieve using a custom Co Inductive Sequence'''

The following code uses a custom "deftype" non-memoizing Co Inductive Stream/Sequence (CIS) implementing the ISeq interface to make the sequence operations more efficient and is about four times faster than the above code:

```clojure
(deftype CIS [v cont]
  clojure.lang.ISeq
    (first [_] v)
    (next [_] (if (nil? cont) nil (cont)))
    (more [this] (let [nv (.next this)] (if (nil? nv) (CIS. nil nil) nv)))
    (cons [this o] (clojure.core/cons o this))
    (empty [_] (if (and (nil? v) (nil? cont)) nil (CIS. nil nil)))
    (equiv [this o] (loop [cis1 this, cis2 o] (if (nil? cis1) (if (nil? cis2) true false)
                                                (if (or (not= (type cis1) (type cis2))
                                                        (not= (.v cis1) (.v ^CIS cis2))
                                                        (and (nil? (.cont cis1))
                                                              (not (nil? (.cont ^CIS cis2))))
                                                        (and (nil? (.cont ^CIS cis2))
                                                              (not (nil? (.cont cis1))))) false
                                                  (if (nil? (.cont cis1)) true
                                                    (recur ((.cont cis1)) ((.cont ^CIS cis2))))))))
    (count [this] (loop [cis this, cnt 0] (if (or (nil? cis) (nil? (.cont cis))) cnt
                                            (recur ((.cont cis)) (inc cnt)))))
  clojure.lang.Seqable
    (seq [this] (if (and (nil? v) (nil? cont)) nil this))
  clojure.lang.Sequential
  Object
    (toString [this] (if (and (nil? v) (nil? cont)) "()" (.toString (seq (map identity this))))))

(defn primes-treeFoldingx
  "Computes the unbounded sequence of primes using a Sieve of Eratosthenes algorithm modified from Bird."
  []
  (letfn [(mltpls [p] (let [p2 (* 2 p)]
                        (letfn [(nxtmltpl [c]
                                  (->CIS c (fn [] (nxtmltpl (+ c p2)))))]
                          (nxtmltpl (* p p))))),
          (allmtpls [^CIS ps] (->CIS (mltpls (.v ps)) (fn [] (allmtpls ((.cont ps)))))),
          (union [^CIS xs ^CIS ys] (let [xv (.v xs), yv (.v ys)]
                                     (if (< xv yv) (->CIS xv (fn [] (union ((.cont xs)) ys)))
                                       (if (< yv xv) (->CIS yv (fn [] (union xs ((.cont ys)))))
                                         (->CIS xv (fn [] (union (next xs) ((.cont ys))))))))),
          (pairs [^CIS mltplss] (let [^CIS tl ((.cont mltplss))]
                                  (->CIS (union (.v mltplss) (.v tl))
                                         (fn [] (pairs ((.cont tl))))))),
          (mrgmltpls [^CIS mltplss] (->CIS (.v ^CIS (.v mltplss))
                                           (fn [] (union ((.cont ^CIS (.v mltplss)))
                                                         (mrgmltpls (pairs ((.cont mltplss)))))))),
          (minusStrtAt [n ^CIS cmpsts] (loop [n n, cmpsts cmpsts]
                                         (if (< n (.v cmpsts))
                                           (->CIS n (fn [] (minusStrtAt (+ n 2) cmpsts)))
                                           (recur (+ n 2) ((.cont cmpsts))))))]
    (do (def oddprms (->CIS 3 (fn [] (let [cmpsts (-> oddprms (allmtpls) (mrgmltpls))]
                                       (minusStrtAt 5 cmpsts)))))
        (->CIS 2 (fn [] oddprms)))))
```


'(time (count (take-while #(<= (long %) 10000000) (primes-treeFoldingx))))' takes about 3.4 seconds for a range of 10 million.

The above code is useful for ranges up to about fifteen million primes, which is about the first million primes; it is comparable in speed to all of the bounded versions except for the fastest bit packed version which can reasonably be used for ranges about 100 times as large.

'''Incremental Hash Map based unbounded "odds-only" version'''

The following code is a version of the O'Neill Haskell code but does not use wheel factorization other than for sieving odds only (although it could be easily added) and uses a Hash Map (constant amortized access time) rather than a Priority Queue (log n access time for combined remove-and-insert-anew operations, which are the majority used for this algorithm) with a lazy sequence for output of the resulting primes; the code has the added feature that it uses a secondary base primes sequence generator and only adds prime culling sequences to the composites map when they are necessary, thus saving time and limiting storage to only that required for the map entries for primes up to the square root of the currently sieved number:

```clojure

(defn primes-hashmap
  "Infinite sequence of primes using an incremental Sieve or Eratosthenes with a Hashmap"
  []
  (letfn [(nxtoddprm [c q bsprms cmpsts]
            (if (>= c q) ;; only ever equal
              (let [p2 (* (first bsprms) 2), nbps (next bsprms), nbp (first nbps)]
                (recur (+ c 2) (* nbp nbp) nbps (assoc cmpsts (+ q p2) p2)))
              (if (contains? cmpsts c)
                (recur (+ c 2) q bsprms
                       (let [adv (cmpsts c), ncmps (dissoc cmpsts c)]
                         (assoc ncmps
                                (loop [try (+ c adv)] ;; ensure map entry is unique
                                  (if (contains? ncmps try)
                                    (recur (+ try adv)) try)) adv)))
                (cons c (lazy-seq (nxtoddprm (+ c 2) q bsprms cmpsts))))))]
    (do (def baseoddprms (cons 3 (lazy-seq (nxtoddprm 5 9 baseoddprms {}))))
        (cons 2 (lazy-seq (nxtoddprm 3 9 baseoddprms {}))))))

```


The above code is slower than the best tree folding version due to the added constant factor overhead of computing the hash functions for every hash map operation even though it has computational complexity of (n log log n) rather than the worse (n log n log log n) for the previous incremental tree folding sieve.  It is still about 100 times slower than the sieve based on the bit-packed mutable array due to these constant factor hashing overheads.

There is almost no benefit of converting the above code to use a CIS as most of the time is expended in the hash map functions.

'''Incremental Priority Queue based unbounded "odds-only" version'''

In order to implement the O'Neill Priority Queue incremental Sieve of Eratosthenes algorithm, one requires an efficient implementation of a Priority Queue, which is not part of standard Clojure.  For this purpose, the most suitable Priority Queue is a binary tree heap based MinHeap algorithm.  The following code implements a purely functional (using entirely immutable state) MinHeap Priority Queue providing the required functions of (emtpy-pq) initialization, (getMin-pq pq) to examinte the minimum key/value pair in the queue, (insert-pq pq k v) to add entries to the queue, and (replaceMinAs-pq pq k v) to replaace the minimum entry with a key/value pair as given (it is more efficient that if functions were provided to delete and then re-insert entries in the queue; there is therefore no "delete" or other queue functions supplied as the algorithm does not requrie them:

```clojure
(deftype PQEntry [k, v]
  Object
    (toString [_] (str "<" k "," v ">")))
(deftype PQNode [ntry, lft, rght]
  Object
    (toString [_] (str "<" ntry " left: " (str lft) " right: " (str rght) ">")))

(defn empty-pq [] nil)

(defn getMin-pq [^PQNode pq]
  (if (nil? pq)
    nil
    (.ntry pq)))

(defn insert-pq [^PQNode opq ok v]
  (loop [^PQEntry kv (->PQEntry ok v), pq opq, cont identity]
    (if (nil? pq)
      (cont (->PQNode kv nil nil))
      (let [k (.k kv),
            ^PQEntry kvn (.ntry pq), kn (.k kvn),
            l (.lft pq), r (.rght pq)]
        (if (<= k kn)
          (recur kvn r #(cont (->PQNode kv % l)))
          (recur kv r #(cont (->PQNode kvn % l))))))))

(defn replaceMinAs-pq [^PQNode opq k v]
  (let [^PQEntry kv (->PQEntry k v)]
    (if (nil? opq) ;; if was empty or just an entry, just use current entry
      (->PQNode kv nil nil)
      (loop [pq opq, cont identity]
        (let [^PQNode l (.lft pq), ^PQNode r (.rght pq)]
          (cond ;; if left us empty, right must be too
            (nil? l)
              (cont (->PQNode kv nil nil)),
            (nil? r) ;; we only have a left...
              (let [^PQEntry kvl (.ntry l), kl (.k kvl)]
                    (if (<= k kl)
                      (cont (->PQNode kv l nil))
                      (recur l #(cont (->PQNode kvl % nil))))),
            :else (let [^PQEntry kvl (.ntry l), kl (.k kvl),
                        ^PQEntry kvr (.ntry r), kr (.k kvr)] ;; we have both
                    (if (and (<= k kl) (<= k kr))
                      (cont (->PQNode kv l r))
                      (if (<= kl kr)
                        (recur l #(cont (->PQNode kvl % r)))
                        (recur r #(cont (->PQNode kvr l %))))))))))))
```


Note that the above code is written partially using continuation passing style so as to leave the "recur" calls in tail call position as required for efficient looping in Clojure; for practical sieving ranges, the algorithm could likely use just raw function recursion as recursion depth is unlikely to be used beyond a depth of about ten or so, but raw recursion is said to be less code efficient.

The actual incremental sieve using the Priority Queue is as follows, which code uses the same optimizations of postponing the addition of prime composite streams to the queue until the square root of the currently sieved number is reached and using a secondary base primes stream to generate the primes composite stream markers in the queue as was used for the Hash Map version:

```clojure
(defn primes-pq
  "Infinite sequence of primes using an incremental Sieve or Eratosthenes with a Priority Queue"
  []
  (letfn [(nxtoddprm [c q bsprms cmpsts]
            (if (>= c q) ;; only ever equal
              (let [p2 (* (first bsprms) 2), nbps (next bsprms), nbp (first nbps)]
                (recur (+ c 2) (* nbp nbp) nbps (insert-pq cmpsts (+ q p2) p2)))
              (let [mn (getMin-pq cmpsts)]
                (if (and mn (>= c (.k mn))) ;; never greater than
                  (recur (+ c 2) q bsprms
                         (loop [adv (.v mn), cmps cmpsts] ;; advance repeat composites for value
                           (let [ncmps (replaceMinAs-pq cmps (+ c adv) adv),
                                 nmn (getMin-pq ncmps)]
                             (if (and nmn (>= c (.k nmn)))
                               (recur (.v nmn) ncmps)
                               ncmps))))
                  (cons c (lazy-seq (nxtoddprm (+ c 2) q bsprms cmpsts)))))))]
    (do (def baseoddprms (cons 3 (lazy-seq (nxtoddprm 5 9 baseoddprms (empty-pq)))))
        (cons 2 (lazy-seq (nxtoddprm 3 9 baseoddprms (empty-pq)))))))
```


The above code is faster than the Hash Map version up to about a sieving range of fifteen million or so, but gets progressively slower for larger ranges due to having (n log n log log n) computational complexity rather than the (n log log n) for the Hash Map version, which has a higher constant factor overhead that is overtaken by the extra "log n" factor.

It is slower that the fastest of the tree folding versions (which has the same computational complexity) due to the higher constant factor overhead of the Priority Queue operations (although perhaps a more efficient implementation of the MinHeap Priority Queue could be developed).

Again, these non-mutable array based sieves are about a hundred times slower than even the "one large memory buffer array" version as implemented in the bounded section; a page segmented version of the mutable bit-packed memory array would be several times faster.

All of these algorithms will respond to maximum wheel factorization, getting up to approximately four times faster if this is applied as compared to the the "odds-only" versions.

It is difficult if not impossible to apply efficient multi-processing to the above versions of the unbounded sieves as the next values of the primes sequence are dependent on previous changes of state for the Bird and Tree Folding versions; however, with the addition of a "update the whole Priority Queue (and reheapify)" or "update the Hash Map" to a given page start state functions, it is possible to do for these letter two algorithms; however, even though it is possible and there is some benefit for these latter two implementations, the benefit is less than using mutable arrays due to that the results must be enumerated into a data structure of some sort in order to be passed out of the page function whereas they can be directly enumerated from the array for the mutable array versions.

'''Bit packed page segmented array unbounded "odds-only" version'''

To show that Clojure does not need to be particularly slow, the following version runs about twice as fast as the non-segmented unbounded array based version above (extremely fast compared to the non-array based versions) and only a little slower than other equivalent versions running on virtual machines: C# or F# on DotNet or Java and Scala on the JVM:


```clojure
(set! *unchecked-math* true)

(def PGSZ (bit-shift-left 1 14)) ;; size of CPU cache
(def PGBTS (bit-shift-left PGSZ 3))
(def PGWRDS (bit-shift-right PGBTS 5))
(def BPWRDS (bit-shift-left 1 7)) ;; smaller page buffer for base primes
(def BPBTS (bit-shift-left BPWRDS 5))
(defn- count-pg
  "count primes in the culled page buffer, with test for limit"
  [lmt ^ints pg]
  (let [pgsz (alength pg),
        pgbts (bit-shift-left pgsz 5),
        cntem (fn [lmtw]
                (let [lmtw (long lmtw)]
	          (loop [i (long 0), c (long 0)]
	            (if (>= i lmtw) (- (bit-shift-left lmtw 5) c)
	              (recur (inc i)
	              (+ c (java.lang.Integer/bitCount (aget pg i))))))))]
    (if (< lmt pgbts)
      (let [lmtw (bit-shift-right lmt 5),
            lmtb (bit-and lmt 31)
            msk (bit-shift-left -2 lmtb)]
        (+ (cntem lmtw)
           (- 32 (java.lang.Integer/bitCount (bit-or (aget pg lmtw)
                                                      msk)))))
      (- pgbts
         (areduce pg i ret (long 0) (+ ret (java.lang.Integer/bitCount (aget pg i))))))))
;;      (cntem pgsz))))
(defn- primes-pages
  "unbounded Sieve of Eratosthenes producing a lazy sequence of culled page buffers."
  []
  (letfn [(make-pg [lowi pgsz bpgs]
            (let [lowi (long lowi),
                  pgbts (long (bit-shift-left pgsz 5)),
                  pgrng (long (+ (bit-shift-left (+ lowi pgbts) 1) 3)),
                  ^ints pg (int-array pgsz),
                  cull (fn [bpgs']
                         (loop [i (long 0), bpgs' bpgs']
	                         (let [^ints fbpg (first bpgs'),
	                               bpgsz (long (alength fbpg))]
	                           (if (>= i bpgsz)
	                             (recur 0 (next bpgs'))
	                             (let [p (long (aget fbpg i)),
	                                   sqr (long (* p p))]
	                               (if (< sqr pgrng) (do
                   (loop [j (long (let [s (long (bit-shift-right (- sqr 3) 1))]
                                     (if (>= s lowi) (- s lowi)
                                       (let [m (long (rem (- lowi s) p))]
                                         (if (zero? m)
                                           0
                                           (- p m))))))]
                     (if (< j pgbts) ;; fast inner culling loop where most time is spent
                       (do
                         (let [w (bit-shift-right j 5)]
                           (aset pg w (int (bit-or (aget pg w)
                                                   (bit-shift-left 1 (bit-and j 31))))))
                         (recur (+ j p)))))
                     (recur (inc i) bpgs'))))))))]
              (do (if (nil? bpgs)
                    (letfn [(mkbpps [i]
                              (if (zero? (bit-and (aget pg (bit-shift-right i 5))
                                                  (bit-shift-left 1 (bit-and i 31))))
                                (cons (int-array 1 (+ i i 3)) (lazy-seq (mkbpps (inc i))))
                                (recur (inc i))))]
                      (cull (mkbpps 0)))
                    (cull bpgs))
                  pg))),
          (page-seq [lowi pgsz bps]
            (letfn [(next-seq [lwi]
                      (cons (make-pg lwi pgsz bps)
                            (lazy-seq (next-seq (+ lwi (bit-shift-left pgsz 5))))))]
              (next-seq lowi)))
          (pgs->bppgs [ppgs]
            (letfn [(nxt-pg [lowi pgs]
                      (let [^ints pg (first pgs),
                            cnt (count-pg BPBTS pg),
                            npg (int-array cnt)]
                        (do (loop [i 0, j 0]
                              (if (< i BPBTS)
                                (if (zero? (bit-and (aget pg (bit-shift-right i 5))
                                                    (bit-shift-left 1 (bit-and i 31))))
                                  (do (aset npg j (+ (bit-shift-left (+ lowi i) 1) 3))
                                      (recur (inc i) (inc j)))
                                  (recur (inc i) j))))
                            (cons npg (lazy-seq (nxt-pg (+ lowi BPBTS) (next pgs)))))))]
              (nxt-pg 0 ppgs))),
          (make-base-prms-pgs []
            (pgs->bppgs (cons (make-pg 0 BPWRDS nil)
                              (lazy-seq (page-seq BPBTS BPWRDS (make-base-prms-pgs))))))]
    (page-seq 0 PGWRDS (make-base-prms-pgs))))
(defn primes-paged
  "unbounded Sieve of Eratosthenes producing a lazy sequence of primes"
  []
  (do (deftype CIS [v cont]
        clojure.lang.ISeq
          (first [_] v)
          (next [_] (if (nil? cont) nil (cont)))
          (more [this] (let [nv (.next this)] (if (nil? nv) (CIS. nil nil) nv)))
          (cons [this o] (clojure.core/cons o this))
          (empty [_] (if (and (nil? v) (nil? cont)) nil (CIS. nil nil)))
          (equiv [this o] (loop [cis1 this, cis2 o] (if (nil? cis1) (if (nil? cis2) true false)
                                                      (if (or (not= (type cis1) (type cis2))
                                                              (not= (.v cis1) (.v ^CIS cis2))
                                                              (and (nil? (.cont cis1))
                                                                   (not (nil? (.cont ^CIS cis2))))
                                                              (and (nil? (.cont ^CIS cis2))
                                                                   (not (nil? (.cont cis1))))) false
                                                        (if (nil? (.cont cis1)) true
                                                          (recur ((.cont cis1)) ((.cont ^CIS cis2))))))))
          (count [this] (loop [cis this, cnt 0] (if (or (nil? cis) (nil? (.cont cis))) cnt
                                                  (recur ((.cont cis)) (inc cnt)))))
        clojure.lang.Seqable
          (seq [this] (if (and (nil? v) (nil? cont)) nil this))
        clojure.lang.Sequential
        Object
          (toString [this] (if (and (nil? v) (nil? cont)) "()" (.toString (seq (map identity this))))))
		  (letfn [(next-prm [lowi i pgseq]
		            (let [lowi (long lowi),
                      i (long i),
                      ^ints pg (first pgseq),
		                  pgsz (long (alength pg)),
		                  pgbts (long (bit-shift-left pgsz 5)),
		                  ni (long (loop [j (long i)]
		                             (if (or (>= j pgbts)
		                                     (zero? (bit-and (aget pg (bit-shift-right j 5))
		                                               (bit-shift-left 1 (bit-and j 31)))))
		                               j
		                               (recur (inc j)))))]
		              (if (>= ni pgbts)
		                (recur (+ lowi pgbts) 0 (next pgseq))
		                (->CIS (+ (bit-shift-left (+ lowi ni) 1) 3)
		                       (fn [] (next-prm lowi (inc ni) pgseq))))))]
		    (->CIS 2 (fn [] (next-prm 0 0 (primes-pages)))))))
(defn primes-paged-count-to
  "counts primes generated by page segments by Sieve of Eratosthenes to the top limit"
  [top]
  (cond (< top 2) 0
        (< top 3) 1
        :else (letfn [(nxt-pg [lowi pgseq cnt]
                        (let [topi (bit-shift-right (- top 3) 1)
                              nxti (+ lowi PGBTS),
                              pg (first pgseq)]
                          (if (> nxti topi)
                            (+ cnt (count-pg (- topi lowi) pg))
                            (recur nxti
                                   (next pgseq)
                                   (+ cnt (count-pg PGBTS pg))))))]
                (nxt-pg 0 (primes-pages) 1))))
```


The above code runs just as fast as other virtual machine languages when run on a 64-bit JVM; however, when run on a 32-bit JVM it runs almost five times slower.  This is likely due to Clojure only using 64-bit integers for integer operations and these operations getting JIT compiled to use library functions to simulate those operations using combined 32-bit operations under a 32-bit JVM whereas direct CPU operations can be used on a 64-bit JVM

Clojure does one thing very slowly, just as here:  it enumerates extremely slowly as compared to using a more imperative iteration interface; it helps to use a roll-your-own ISeq interface as here, where enumeration of the primes reduces the time from about four times as long as the composite culling operations for those primes to only about one and a half times as long, although one must also write their own sequence handling functions (can't use "take-while" or "count", for instance) in order to enjoy that benefit.  That is why the "primes-paged-count-to" function is provided so it takes a negligible percentage of the time to count the primes over a range as compared to the time for the composite culling operations.

The practical range of the above sieve is about 16 million due to the fixed size of the page buffers; in order to extend the range, a larger page buffer could be used up to the size of the CPU L2 or L3 caches.  If a 2^20 buffer were used (one Megabyte, as many modern dexktop CPU's easily have in their L3 cache), then the range would be increased up to about 10^14 at a cost of about a factor of two or three in slower memory accesses per composite culling operation loop.  The base primes culling page size is already adequate for this range.  One could make the culling page size automatically expand with growing range by about the square root of the current prime range with not too many changes to the code.

As for many implementations of unbounded sieves, the base primes less than the square root of the current range are generated by a secondary generated stream of primes; in this case it is done recursively, so another secondary stream generates the base primes for the base primes and so on down to where the innermost generator has only one page in the stream; this only takes one or two recursions for this type of range.

The base primes culling page size is reduced from the page size for the main primes so that there is less overhead for smaller primes ranges; otherwise excess base primes are generated for fairly small sieve ranges.


## CMake


```cmake
function(eratosthenes var limit)
  # Check for integer overflow. With CMake using 32-bit signed integer,
  # this check fails when limit > 46340.
  if(NOT limit EQUAL 0)         # Avoid division by zero.
    math(EXPR i "(${limit} * ${limit}) / ${limit}")
    if(NOT limit EQUAL ${i})
      message(FATAL_ERROR "limit is too large, would cause integer overflow")
    endif()
  endif()

  # Use local variables prime_2, prime_3, ..., prime_${limit} as array.
  # Initialize array to y => yes it is prime.
  foreach(i RANGE 2 ${limit})
    set(prime_${i} y)
  endforeach(i)

  # Gather a list of prime numbers.
  set(list)
  foreach(i RANGE 2 ${limit})
    if(prime_${i})
      # Append this prime to list.
      list(APPEND list ${i})

      # For each multiple of i, set n => no it is not prime.
      # Optimization: start at i squared.
      math(EXPR square "${i} * ${i}")
      if(NOT square GREATER ${limit})   # Avoid fatal error.
        foreach(m RANGE ${square} ${limit} ${i})
          set(prime_${m} n)
        endforeach(m)
      endif()
    endif(prime_${i})
  endforeach(i)
  set(${var} ${list} PARENT_SCOPE)
endfunction(eratosthenes)
```

 # Print all prime numbers through 100.
 eratosthenes(primes 100)
 message(STATUS "${primes}")


## COBOL


```cobol
*> Please ignore the asterisks in the first column of the next comments,
*> which are kludges to get syntax highlighting to work.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Sieve-Of-Eratosthenes.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  Max-Number       USAGE UNSIGNED-INT.
       01  Max-Prime        USAGE UNSIGNED-INT.

       01  Num-Group.
           03  Num-Table PIC X VALUE "P"
                   OCCURS 1 TO 10000000 TIMES DEPENDING ON Max-Number
                   INDEXED BY Num-Index.
               88  Is-Prime VALUE "P" FALSE "N".

       01  Current-Prime    USAGE UNSIGNED-INT.

       01  I                USAGE UNSIGNED-INT.

       PROCEDURE DIVISION.
           DISPLAY "Enter the limit: " WITH NO ADVANCING
           ACCEPT Max-Number
           DIVIDE Max-Number BY 2 GIVING Max-Prime

*          *> Set Is-Prime of all non-prime numbers to false.
           SET Is-Prime (1) TO FALSE
           PERFORM UNTIL Max-Prime < Current-Prime
*              *> Set current-prime to next prime.
               ADD 1 TO Current-Prime
               PERFORM VARYING Num-Index FROM Current-Prime BY 1
                   UNTIL Is-Prime (Num-Index)
               END-PERFORM
               MOVE Num-Index TO Current-Prime

*              *> Set Is-Prime of all multiples of current-prime to
*              *> false, starting from current-prime sqaured.
               COMPUTE Num-Index = Current-Prime ** 2
               PERFORM UNTIL Max-Number < Num-Index
                   SET Is-Prime (Num-Index) TO FALSE
                   SET Num-Index UP BY Current-Prime
               END-PERFORM
           END-PERFORM

*          *> Display the prime numbers.
           PERFORM VARYING Num-Index FROM 1 BY 1
                   UNTIL Max-Number < Num-Index
               IF Is-Prime (Num-Index)
                   DISPLAY Num-Index
               END-IF
           END-PERFORM

           GOBACK
           .
```



## Common Lisp


```lisp
(defun sieve-of-eratosthenes (maximum)
  (loop
     with sieve = (make-array (1+ maximum)
                              :element-type 'bit
                              :initial-element 0)
     for candidate from 2 to maximum
     when (zerop (bit sieve candidate))
     collect candidate
     and do (loop for composite from (expt candidate 2)
               to maximum by candidate
               do (setf (bit sieve composite) 1))))
```


Working with odds only (above twice speedup), and marking composites only for primes up to the square root of the maximum:


```lisp
(defun sieve-odds (maximum)
  "Prime numbers sieve for odd numbers.
   Returns a list with all the primes that are less than or equal to maximum."
  (loop :with maxi = (ash (1- maximum) -1)
        :with stop = (ash (isqrt maximum) -1)
        :with sieve = (make-array (1+ maxi) :element-type 'bit :initial-element 0)
        :for i :from 1 :to maxi
        :for odd-number = (1+ (ash i 1))
        :when (zerop (sbit sieve i))
          :collect odd-number :into values
        :when (<= i stop)
          :do (loop :for j :from (* i (1+ i) 2) :to maxi :by odd-number
                    :do (setf (sbit sieve j) 1))
        :finally (return (cons 2 values))))
```


The indexation scheme used here interprets each index <code>i</code> as standing for the value <code>2i+1</code>. Bit <code>0</code> is unused, a small price to pay for the simpler index calculations compared with the <code>2i+3</code> indexation scheme. The multiples of a given odd prime <code>p</code> are enumerated in increments of <code>2p</code>, which corresponds to the index increment of <code>p</code> on the sieve array. The starting point <code>p*p = (2i+1)(2i+1) = 4i(i+1)+1</code> corresponds to the index <code>2i(i+1)</code>.

While formally a ''wheel'', odds are uniformly spaced and do not require any special processing except for value translation. Wheels proper aren't uniformly spaced and are thus trickier.


## D


### Simpler Version

Prints all numbers less than the limit.
```d
import std.stdio, std.algorithm, std.range, std.functional;

uint[] sieve(in uint limit) nothrow @safe {
    if (limit < 2)
        return [];
    auto composite = new bool[limit];

    foreach (immutable uint n; 2 .. cast(uint)(limit ^^ 0.5) + 1)
        if (!composite[n])
            for (uint k = n * n; k < limit; k += n)
                composite[k] = true;

    //return iota(2, limit).filter!(not!composite).array;
    return iota(2, limit).filter!(i => !composite[i]).array;
}

void main() {
    50.sieve.writeln;
}
```

{{out}}

```txt
[2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47]
```



### Faster Version

This version uses an array of bits (instead of booleans, that are represented with one byte), and skips even numbers. The output is the same.

```d
import std.stdio, std.math, std.array;

size_t[] sieve(in size_t m) pure nothrow @safe {
    if (m < 3)
        return null;
    immutable size_t n = m - 1;
    enum size_t bpc = size_t.sizeof * 8;
    auto F = new size_t[((n + 2) / 2) / bpc + 1];
    F[] = size_t.max;

    size_t isSet(in size_t i) nothrow @safe @nogc {
        immutable size_t offset = i / bpc;
        immutable size_t mask = 1 << (i % bpc);
        return F[offset] & mask;
    }

    void resetBit(in size_t i) nothrow @safe @nogc {
        immutable size_t offset = i / bpc;
        immutable size_t mask = 1 << (i % bpc);
        if ((F[offset] & mask) != 0)
            F[offset] = F[offset] ^ mask;
    }

    for (size_t i = 3; i <= sqrt(real(n)); i += 2)
        if (isSet((i - 3) / 2))
            for (size_t j = i * i; j <= n; j += 2 * i)
                resetBit((j - 3) / 2);

    Appender!(size_t[]) result;
    result ~= 2;
    for (size_t i = 3; i <= n; i += 2)
        if (isSet((i - 3) / 2))
            result ~= i;
    return result.data;
}

void main() {
    50.sieve.writeln;
}
```



### Extensible Version

(This version is used in the task [[Extensible prime generator#D|Extensible prime generator]].)

```d
/// Extensible Sieve of Eratosthenes.
struct Prime {
    uint[] a = [2];

    private void grow() pure nothrow @safe {
        immutable p0 = a[$ - 1] + 1;
        auto b = new bool[p0];

        foreach (immutable di; a) {
            immutable uint i0 = p0 / di * di;
            uint i = (i0 < p0) ? i0 + di - p0 : i0 - p0;
            for (; i < b.length; i += di)
                b[i] = true;
        }

        foreach (immutable uint i, immutable bi; b)
            if (!b[i])
                a ~= p0 + i;
    }

    uint opCall(in uint n) pure nothrow @safe {
        while (n >= a.length)
            grow;
        return a[n];
    }
}

version (sieve_of_eratosthenes3_main) {
    void main() {
        import std.stdio, std.range, std.algorithm;

        Prime prime;
        uint.max.iota.map!prime.until!q{a > 50}.writeln;
    }
}
```

To see the output (that is the same), compile with <code>-version=sieve_of_eratosthenes3_main</code>.


## Dart


```dart
// helper function to pretty print an Iterable
String iterableToString(Iterable seq) {
  String str = "[";
  Iterator i = seq.iterator;
  if (i.moveNext()) str += i.current.toString();
  while(i.moveNext()) {
    str += ", " + i.current.toString();
  }
  return str + "]";
}

main() {
  int limit = 1000;
  int strt = new DateTime.now().millisecondsSinceEpoch;
  Set<int> sieve = new Set<int>();

  for(int i = 2; i <= limit; i++) {
    sieve.add(i);
  }
  for(int i = 2; i * i <= limit; i++) {
   if(sieve.contains(i)) {
     for(int j = i * i; j <= limit; j += i) {
       sieve.remove(j);
     }
   }
  }
  var sortedValues = new List<int>.from(sieve);
  int elpsd = new DateTime.now().millisecondsSinceEpoch - strt;
  print("Found " + sieve.length.toString() + " primes up to " + limit.toString() +
      " in " + elpsd.toString() + " milliseconds.");
  print(iterableToString(sortedValues)); // expect sieve.length to be 168 up to 1000...
//  Expect.equals(168, sieve.length);
}
```

{{out}}
```txt

Found 168 primes up to 1000 in 9 milliseconds.
[2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 127, 131, 137, 139, 149, 151, 157, 163, 167, 173, 179, 181, 191, 193, 197, 199, 211, 223, 227, 229, 233, 239, 241, 251, 257, 263, 269, 271, 277, 281, 283, 293, 307, 311, 313, 317, 331, 337, 347, 349, 353, 359, 367, 373, 379, 383, 389, 397, 401, 409, 419, 421, 431, 433, 439, 443, 449, 457, 461, 463, 467, 479, 487, 491, 499, 503, 509, 521, 523, 541, 547, 557, 563, 569, 571, 577, 587, 593, 599, 601, 607, 613, 617, 619, 631, 641, 643, 647, 653, 659, 661, 673, 677, 683, 691, 701, 709, 719, 727, 733, 739, 743, 751, 757, 761, 769, 773, 787, 797, 809, 811, 821, 823, 827, 829, 839, 853, 857, 859, 863, 877, 881, 883, 887, 907, 911, 919, 929, 937, 941, 947, 953, 967, 971, 977, 983, 991, 997]

```

Although it has the characteristics of a true Sieve of Eratosthenes, the above code isn't very efficient due to the remove/modify operations on the Set.  Due to these, the computational complexity isn't close to linear with increasing range and it is quite slow for larger sieve ranges compared to compiled languages, taking an average of about 22 thousand CPU clock cycles for each of the 664579 primes (about 4 seconds on a 3.6 Gigahertz CPU) just to sieve to ten million.

===faster bit-packed array odds-only solution===

```dart
import 'dart:typed_data';
import 'dart:math';

Iterable<int> soeOdds(int limit) {
  if (limit < 3) return limit < 2 ? Iterable.empty() : [2];
  int lmti = (limit - 3) >> 1;
  int bfsz = (lmti >> 3) + 1;
  int sqrtlmt = (sqrt(limit) - 3).floor() >> 1;
  Uint32List cmpsts = Uint32List(bfsz);
  for (int i = 0; i <= sqrtlmt; ++i)
    if ((cmpsts[i >> 5] & (1 << (i & 31))) == 0) {
      int p = i + i + 3;
      for (int j = (p * p - 3) >> 1; j <= lmti; j += p)
        cmpsts[j >> 5] |= 1 << (j & 31);
    }
  return
    [2].followedBy(
      Iterable.generate(lmti + 1)
      .where((i) => cmpsts[i >> 5] & (1 << (i & 31)) == 0)
      .map((i) => i + i + 3) );
}

void main() {
  final int range = 100000000;
  String s = "( ";
  primesPaged().take(25).forEach((p)=>s += "$p "); print(s + ")");
  print("There are ${countPrimesTo(1000000)} primes to 1000000.");
  final start = DateTime.now().millisecondsSinceEpoch;
  final answer = soeOdds(range).length;
  final elapsed = DateTime.now().millisecondsSinceEpoch - start;
  print("There were $answer primes found up to $range.");
  print("This test bench took $elapsed milliseconds.");
}
```

{{output}}

```txt
( 2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 )
There are 78498 primes to 1000000.
There were 5761455 primes found up to 100000000.
This test bench took 4604 milliseconds.
```


The above code is somewhat faster at about 1.5 thousand CPU cycles per prime here run on a 1.92 Gigahertz low end Intel x5-Z8350 CPU or about 2.5 seconds on a 3.6 Gigahertz CPU using the Dart VM to sieve to 100 million.


### Unbounded infinite iterators/generators of primes


'''Infinite generator using a (hash) Map (sieves odds-only)'''

The following code will have about O(n log (log n)) performance due to a hash table having O(1) average performance and is only somewhat slow due to the constant overhead of processing hashes:


```dart>Iterable<int
 primesMap() {
    Iterable<int> oddprms() sync* {
      yield(3); yield(5); // need at least 2 for initialization
      final Map<int, int> bpmap = {9: 6};
      final Iterator<int> bps = oddprms().iterator;
      bps.moveNext(); bps.moveNext(); // skip past 3 to 5
      int bp = bps.current;
      int n = bp;
      int q = bp * bp;
      while (true) {
        n += 2;
        while (n >= q || bpmap.containsKey(n)) {
          if (n >= q) {
            final int inc = bp << 1;
            bpmap[bp * bp + inc] = inc;
            bps.moveNext(); bp = bps.current; q = bp * bp;
          } else {
            final int inc = bpmap.remove(n);
            int next = n + inc;
            while (bpmap.containsKey(next)) {
              next += inc;
            }
            bpmap[next] = inc;
          }
          n += 2;
        }
        yield(n);
      }
    }
    return [2].followedBy(oddprms());
}

void main() {
  final int range = 100000000;
  String s = "( ";
  primesMap().take(25).forEach((p)=>s += "$p "); print(s + ")");
  print("There are ${primesMap().takeWhile((p)=>p<=1000000).length} preimes to 1000000.");
  final start = DateTime.now().millisecondsSinceEpoch;
  final answer = primesMap().takeWhile((p)=>p<=range).length;
  final elapsed = DateTime.now().millisecondsSinceEpoch - start;
  print("There were $answer primes found up to $range.");
  print("This test bench took $elapsed milliseconds.");
}
```

{{output}}

```txt
( 2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 )
There are 78498 preimes to 1000000.
There were 5761455 primes found up to 100000000.
This test bench took 16086 milliseconds.
```


This takes about 5300 CPU clock cycles per prime or about 8.4 seconds if run on a 3.6 Gigahertz CPU, which is slower than the above fixed bit-packed array version but has the advantage that it runs indefinitely, (at least on 64-bit machines; on 32 bit machines it can only be run up to the 32-bit number range, or just about a factor of 20 above as above).

Due to the constant execution overhead this is only reasonably useful for ranges up to tens of millions anyway.

'''Fast page segmented array infinite generator (sieves odds-only)'''

The following code also theoretically has a O(n log (log n)) execution speed performance and the same limited use on 32-bit execution platformas, but won't realize the theoretical execution complexity for larger primes due to the cache size increasing in size beyond its limits; but as the CPU L2 cache size that it automatically grows to use isn't any slower than the basic culling loop speed, it won't slow down much above that limit up to ranges of about 2.56e14, which will take in the order of weeks:

{{trans|Kotlin}}

```dart
import 'dart:typed_data';
import 'dart:math';
import 'dart:collection';

// a lazy list
typedef _LazyList _Thunk();
class _LazyList<T> {
  final T head;
  _Thunk thunk;
  _LazyList<T> _rest;
  _LazyList(T this.head, _Thunk this.thunk);
  _LazyList<T> get rest {
    if (this.thunk != null) {
      this._rest = this.thunk();
      this.thunk = null;
    }
    return this._rest;
  }
}

class _LazyListIterable<T> extends IterableBase<T> {
  _LazyList<T> _first;
  _LazyListIterable(_LazyList<T> this._first);
  @override Iterator<T> get iterator {
    Iterable<T> inner() sync* {
      _LazyList<T> current = this._first;
      while (true) {
        yield(current.head);
        current = current.rest;
      }
    }
    return inner().iterator;
  }
}

// zero bit population count Look Up Table for 16-bit range...
final Uint8List CLUT =
  Uint8List.fromList(
    Iterable.generate(65536)
    .map((i) {
      final int v0 = ~i & 0xFFFF;
      final int v1 = v0 - ((v0 & 0xAAAA) >> 1);
      final int v2 = (v1 & 0x3333) + ((v1 & 0xCCCC) >> 2);
      return (((((v2 & 0x0F0F) + ((v2 & 0xF0F0) >> 4)) * 0x0101)) >> 8) & 31;
    })
    .toList());

int _countComposites(Uint8List cmpsts) {
  Uint16List buf = Uint16List.view(cmpsts.buffer);
  int lmt = buf.length;
  int count = 0;
  for (var i = 0; i < lmt; ++i) {
    count += CLUT[buf[i]];
  }
  return count;
}

// converts an entire sieved array of bytes into an array of UInt32 primes,
// to be used as a source of base primes...
Uint32List _composites2BasePrimeArray(int low, Uint8List cmpsts) {
  final int lmti = cmpsts.length << 3;
  final int len = _countComposites(cmpsts);
  final Uint32List rslt = Uint32List(len);
  int j = 0;
  for (int i = 0; i < lmti; ++i) {
    if (cmpsts[i >> 3] & 1 << (i & 7) == 0) {
        rslt[j++] = low + i + i;
    }
  }
  return rslt;
}

// do sieving work based on low starting value for the given buffer and
// the given lazy list of base prime arrays...
void _sieveComposites(int low, Uint8List buffer, Iterable<Uint32List> bpas) {
  final int lowi = (low - 3) >> 1;
  final int len = buffer.length;
  final int lmti = len << 3;
  final int nxti = lowi + lmti;
  for (var bpa in bpas) {
    for (var bp in bpa) {
      final int bpi = (bp - 3) >> 1;
      int strti = ((bpi * (bpi + 3)) << 1) + 3;
      if (strti >= nxti) return;
      if (strti >= lowi) strti = strti - lowi;
      else {
        strti = (lowi - strti) % bp;
        if (strti != 0) strti = bp - strti;
      }
      if (bp <= len >> 3 && strti <= lmti - bp << 6) {
        final int slmti = min(lmti, strti + bp << 3);
        for (var s = strti; s < slmti; s += bp) {
          final int msk = 1 << (s & 7);
          for (var c = s >> 3; c < len; c += bp) {
              buffer[c] |= msk;
          }
        }
      }
      else {
        for (var c = strti; c < lmti; c += bp) {
            buffer[c >> 3] |= 1 << (c & 7);
        }
      }
    }
  }
}

// starts the secondary base primes feed with minimum size in bits set to 4K...
// thus, for the first buffer primes up to 8293,
// the seeded primes easily cover it as 97 squared is 9409...
Iterable<Uint32List> _makeBasePrimeArrays() {
  var cmpsts = Uint8List(512);
  _LazyList<Uint32List> _nextelem(int low, Iterable<Uint32List> bpas) {
    // calculate size so that the bit span is at least as big as the
    // maximum culling prime required, rounded up to minsizebits blocks...
    final int rqdsz = 2 + sqrt((1 + low).toDouble()).toInt();
    final sz = (((rqdsz >> 12) + 1) << 9); // size in bytes
    if (sz > cmpsts.length) cmpsts = Uint8List(sz);
    cmpsts.fillRange(0, cmpsts.length, 0);
    _sieveComposites(low, cmpsts, bpas);
    final arr = _composites2BasePrimeArray(low, cmpsts);
    final nxt = low + (cmpsts.length << 4);
    return _LazyList(arr, () => _nextelem(nxt, bpas));
  }
  // pre-seeding breaks recursive race,
  // as only known base primes used for first page...
  final preseedarr = Uint32List.fromList( [ // pre-seed to 100, can sieve to 10,000...
    3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41
    , 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97 ] );
  return _LazyListIterable(
           _LazyList(preseedarr,
           () => _nextelem(101, _makeBasePrimeArrays()))
         );
}

// an iterable sequence over successive sieved buffer composite arrays,
// returning a tuple of the value represented by the lowest possible prime
// in the sieved composites array and the array itself;
// the array has a 16 Kilobytes minimum size (CPU L1 cache), but
// will grow so that the bit span is larger than the
// maximum culling base prime required, possibly making it larger than
// the L1 cache for large ranges, but still reasonably efficient using
// the L2 cache: very efficient up to about 16e9 range;
// reasonably efficient to about 2.56e14 for two Megabyte L2 cache = > 1 day...
Iterable<List> _makeSievePages() sync*  {
  final bpas = _makeBasePrimeArrays(); // secondary source of base prime arrays
  int low = 3;
  Uint8List cmpsts = Uint8List(16384);
  _sieveComposites(3, cmpsts, bpas);
  while (true) {
    yield([low, cmpsts]);
    final rqdsz = 2 + sqrt((1 + low).toDouble()).toInt(); // problem with sqrt not exact past about 10^12!!!!!!!!!
    final sz = ((rqdsz >> 17) + 1) << 14; // size iin bytes
    if (sz > cmpsts.length) cmpsts = Uint8List(sz);
    cmpsts.fillRange(0, cmpsts.length, 0);
    low += cmpsts.length << 4;
    _sieveComposites(low, cmpsts, bpas);
  }
}

int countPrimesTo(int range) {
  if (range < 3) { if (range < 2) return 0; else return 1; }
  var count = 1;
  for (var sp in _makeSievePages()) {
    int low = sp[0]; Uint8List cmpsts = sp[1];
    if ((low + (cmpsts.length << 4)) > range) {
      int lsti = (range - low) >> 1;
      var lstw = (lsti >> 4); var lstb = lstw << 1;
      var msk = (-2 << (lsti & 15)) & 0xFFFF;
      var buf = Uint16List.view(cmpsts.buffer, 0, lstw);
      for (var i = 0; i < lstw; ++i)
        count += CLUT[buf[i]];
      count += CLUT[(cmpsts[lstb + 1] << 8) | cmpsts[lstb] | msk];
      break;
    } else {
      count += _countComposites(cmpsts);
    }
  }
  return count;
}

// sequence over primes from above page iterator;
// unless doing something special with individual primes, usually unnecessary;
// better to do manipulations based on the composites bit arrays...
// takes at least as long to enumerate the primes as sieve them...
Iterable<int> primesPaged() sync* {
  yield(2);
  for (var sp in _makeSievePages()) {
    int low = sp[0]; Uint8List cmpsts = sp[1];
    var szbts = cmpsts.length << 3;
    for (var i = 0; i < szbts; ++i) {
        if (cmpsts[i >> 3].toInt() & (1 << (i & 7)) != 0) continue;
        yield(low + i + i);
    }
  }
}

void main() {
  final int range = 1000000000;
  String s = "( ";
  primesPaged().take(25).forEach((p)=>s += "$p "); print(s + ")");
  print("There are ${countPrimesTo(1000000)} primes to 1000000.");
  final start = DateTime.now().millisecondsSinceEpoch;
  final answer = countPrimesTo(range); // fast way
//  final answer = primesPaged().takeWhile((p)=>p<=range).length; // slow way using enumeration
  final elapsed = DateTime.now().millisecondsSinceEpoch - start;
  print("There were $answer primes found up to $range.");
  print("This test bench took $elapsed milliseconds.");
}
```

{{output}}

```txt
( 2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 )
There are 78498 primes to 1000000.
There were 50847534 primes found up to 1000000000.
This test bench took 9385 milliseconds.
```


This version counts the primes up to one billion in about five seconds at 3.6 Gigahertz (a low end 1.92 Gigahertz CPU used here) or about 350 CPU clock cycles per prime under the Dart Virtual Machine (VM).

Note that it takes about four times as long to do this using the provided primes generator/enumerator as noted in the code, which is normal for all languages that it takes longer to actually enumerate the primes than it does to sieve in culling the composite numbers, but Dart is somewhat slower than most for this.

The algorithm can be sped up by a factor of four by extreme wheel factorization and (likely) about a factor of the effective number of CPU cores by using multi-processing isolates, but there isn't much point if one is to use the prime generator for output.  For most purposes, it is better to use custom functions that directly manipulate the culled bit-packed page segments as `countPrimesTo` does here.


## Delphi


```delphi
program erathostenes;

{$APPTYPE CONSOLE}

type
  TSieve = class
  private
    fPrimes: TArray<boolean>;
    procedure InitArray;
    procedure Sieve;
    function getNextPrime(aStart: integer): integer;
    function getPrimeArray: TArray<integer>;
  public
    function getPrimes(aMax: integer): TArray<integer>;
  end;

  { TSieve }

function TSieve.getNextPrime(aStart: integer): integer;
begin
  result := aStart;
  while not fPrimes[result] do
    inc(result);
end;

function TSieve.getPrimeArray: TArray<integer>;
var
  i, n: integer;
begin
  n := 0;
  setlength(result, length(fPrimes)); // init array with maximum elements
  for i := 2 to high(fPrimes) do
  begin
    if fPrimes[i] then
    begin
      result[n] := i;
      inc(n);
    end;
  end;
  setlength(result, n); // reduce array to actual elements
end;

function TSieve.getPrimes(aMax: integer): TArray<integer>;
begin
  setlength(fPrimes, aMax);
  InitArray;
  Sieve;
  result := getPrimeArray;
end;

procedure TSieve.InitArray;
begin
  for i := 2 to high(fPrimes) do
    fPrimes[i] := true;
end;

procedure TSieve.Sieve;
var
  i, n, max: integer;
begin
  max := length(fPrimes);
  i := 2;
  while i < sqrt(max) do
  begin
    n := sqr(i);
    while n < max do
    begin
      fPrimes[n] := false;
      inc(n, i);
    end;
    i := getNextPrime(i + 1);
  end;
end;

var
  i: integer;
  Sieve: TSieve;

begin
  Sieve := TSieve.Create;
  for i in Sieve.getPrimes(100) do
    write(i, ' ');
  Sieve.Free;
  readln;
end.
```

Output:

```txt
2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97
```




## DWScript



```delphi
function Primes(limit : Integer) : array of Integer;
var
   n, k : Integer;
   sieve := new Boolean[limit+1];
begin
   for n := 2 to Round(Sqrt(limit)) do begin
      if not sieve[n] then begin
         for k := n*n to limit step n do
            sieve[k] := True;
      end;
   end;

   for k:=2 to limit do
      if not sieve[k] then
         Result.Add(k);
end;

var r := Primes(50);
var i : Integer;
for i:=0 to r.High do
   PrintLn(r[i]);
```



## Dylan

With outer to sqrt and inner to p^2 optimizations:

```dylan
define method primes(n)
  let limit = floor(n ^ 0.5) + 1;
  let sieve = make(limited(<simple-vector>, of: <boolean>), size: n + 1, fill: #t);
  let last-prime = 2;

  while (last-prime < limit)
    for (x from last-prime ^ 2 to n by last-prime)
      sieve[x] := #f;
    end for;
    block (found-prime)
      for (n from last-prime + 1 below limit)
        if (sieve[n] = #f)
          last-prime := n;
          found-prime()
        end;
      end;
      last-prime := limit;
    end block;
  end while;

  for (x from 2 to n)
    if (sieve[x]) format-out("Prime: %d\n", x); end;
  end;
end;
```




## E


E's standard library doesn't have a step-by-N numeric range, so we'll define one, implementing the standard iteration protocol.

 def rangeFromBelowBy(start, limit, step) {
   return def stepper {
     to iterate(f) {
       var i := start
       while (i < limit) {
         f(null, i)
         i += step
       }
     }
   }
 }

The sieve itself:

 def eratosthenes(limit :(int > 2), output) {
   def composite := [].asSet().diverge()
   for i ? (!composite.contains(i)) in 2..!limit {
     output(i)
     composite.addAll(rangeFromBelowBy(i ** 2, limit, i))
   }
 }

Example usage:

 ? eratosthenes(12, println)
 # stdout: 2
 #         3
 #         5
 #         7
 #         11


## EasyLang

<lang>len prims[] 100
max = floor sqrt len prims[]
tst = 2
while tst <= max
  if prims[tst] = 0
    i = tst * tst
    while i < len prims[]
      prims[i] = 1
      i += tst
    .
  .
  tst += 1
.
i = 2
while i < len prims[]
  if prims[i] = 0
    print i
  .
  i += 1
.
```



## eC

{{incorrect|eC|It uses rem testing and so is a trial division algorithm, not a sieve of Eratosthenes.}}
Note: this is not a Sieve of Eratosthenes; it is just trial division.

```cpp

public class FindPrime
{
   Array<int> primeList { [ 2 ], minAllocSize = 64 };
   int index;

   index = 3;

   bool HasPrimeFactor(int x)
   {
      int max = (int)floor(sqrt((double)x));

      for(i : primeList)
      {
         if(i > max) break;
         if(x % i == 0) return true;
      }
      return false;
   }

   public int GetPrime(int x)
   {
      if(x > primeList.count - 1)
      {
         for (; primeList.count != x; index += 2)
            if(!HasPrimeFactor(index))
            {
               if(primeList.count >= primeList.minAllocSize) primeList.minAllocSize *= 2;
               primeList.Add(index);
            }
      }
      return primeList[x-1];
   }
}

class PrimeApp : Application
{
   FindPrime fp { };
   void Main()
   {
      int num = argc > 1 ? atoi(argv[1]) : 1;
      PrintLn(fp.GetPrime(num));
   }
}

```


## EchoLisp


### Sieve


```lisp
(require 'types) ;; bit-vector

;; converts sieve->list for integers in [nmin .. nmax[
(define (s-range sieve nmin nmax (base 0))
	(for/list ([ i (in-range nmin nmax)]) #:when (bit-vector-ref sieve i) (+ i base)))

;; next prime in sieve > p, or #f
(define (s-next-prime sieve p ) ;;
		(bit-vector-scan-1 sieve (1+ p)))


;; returns a bit-vector - sieve- all numbers in [0..n[
(define (eratosthenes n)
  (define primes (make-bit-vector-1 n ))
  (bit-vector-set! primes 0 #f)
  (bit-vector-set! primes 1 #f)
  (for ([p (1+ (sqrt n))])
  		 #:when (bit-vector-ref primes  p)
         (for ([j (in-range (* p p) n p)])
    (bit-vector-set! primes j #f)))
   primes)

(define s-primes (eratosthenes 10_000_000))

(s-range s-primes 0 100)
   → (2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97)
(s-range s-primes 1_000_000 1_000_100)
   → (1000003 1000033 1000037 1000039 1000081 1000099)
(s-next-prime s-primes 9_000_000)
   → 9000011
```



### Segmented sieve

Allow to extend the basis sieve (n)  up to n^2. Memory requirement is O(√n)

```scheme
;; ref :  http://research.cs.wisc.edu/techreports/1990/TR909.pdf
;; delta multiple of  sqrt(n)
;; segment is [left .. left+delta-1]


(define (segmented sieve left delta  (p 2) (first 0))
	(define segment (make-bit-vector-1 delta))
	(define right (+ left (1- delta)))
	 (define pmax (sqrt right))
	  (while p
	  #:break (> p pmax)
	 (set! first (+ left (modulo (- p (modulo left p)) p )))

 	(for   [(q (in-range first (1+ right) p))]
	(bit-vector-set! segment (- q left) #f))
        (set! p (bit-vector-scan-1 sieve (1+ p))))
	segment)

(define (seg-range nmin delta)
    (s-range (segmented s-primes nmin delta) 0 delta nmin))


(seg-range 10_000_000_000 1000) ;; 15 milli-sec

    → (10000000019 10000000033 10000000061 10000000069 10000000097 10000000103 10000000121
       10000000141 10000000147 10000000207 10000000259 10000000277 10000000279 10000000319
       10000000343 10000000391 10000000403 10000000469 10000000501 10000000537 10000000583
       10000000589 10000000597 10000000601 10000000631 10000000643 10000000649 10000000667
       10000000679 10000000711 10000000723 10000000741 10000000753 10000000793 10000000799
       10000000807 10000000877 10000000883 10000000889 10000000949 10000000963 10000000991
       10000000993 10000000999)

;; 8 msec using the native (prime?) function
(for/list ((p (in-range 1_000_000_000 1_000_001_000))) #:when (prime? p) p)
```



### Wheel

A 2x3 wheel gives a 50% performance gain.

```scheme
;; 2x3 wheel
(define (weratosthenes n)
  (define primes (make-bit-vector n )) ;; everybody to #f (false)
  (bit-vector-set! primes 2 #t)
  (bit-vector-set! primes 3 #t)
  (bit-vector-set! primes 5 #t)

  (for ([i  (in-range 6 n 6) ]) ;; set candidate primes
  		(bit-vector-set! primes (1+ i) #t)
  		(bit-vector-set! primes (+ i 5) #t)
  		)

  (for ([p  (in-range 5 (1+ (sqrt n)) 2 ) ])
  		 #:when (bit-vector-ref primes  p)
         (for ([j (in-range (* p p) n p)])
    (bit-vector-set! primes j #f)))
   primes)
```



## Eiffel

{{works with|EiffelStudio|6.6 beta (with provisional loop syntax)}}

```eiffel
class
    APPLICATION

create
    make

feature
       make
            -- Run application.
        do
            across primes_through (100) as ic loop print (ic.item.out + " ") end
        end

    primes_through (a_limit: INTEGER): LINKED_LIST [INTEGER]
            -- Prime numbers through `a_limit'
        require
            valid_upper_limit: a_limit >= 2
        local
            l_tab: ARRAY [BOOLEAN]
        do
            create Result.make
            create l_tab.make_filled (True, 2, a_limit)
            across
                l_tab as ic
            loop
                if ic.item then
                    Result.extend (ic.target_index)
                    across ((ic.target_index * ic.target_index) |..| l_tab.upper).new_cursor.with_step (ic.target_index) as id
                    loop
                        l_tab [id.item] := False
                    end
                end
            end
        end
end
```


Output:

```txt

2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97

```



## Elixir


```elixir
defmodule Prime do
  def eratosthenes(limit \\ 1000) do
    sieve = [false, false | Enum.to_list(2..limit)] |> List.to_tuple
    check_list = [2 | Stream.iterate(3, &(&1+2)) |> Enum.take(round(:math.sqrt(limit)/2))]
    Enum.reduce(check_list, sieve, fn i,tuple ->
      if elem(tuple,i) do
        clear_num = Stream.iterate(i*i, &(&1+i)) |> Enum.take_while(fn x -> x <= limit end)
        clear(tuple, clear_num)
      else
        tuple
      end
    end)
  end

  defp clear(sieve, list) do
    Enum.reduce(list, sieve, fn i, acc -> put_elem(acc, i, false) end)
  end
end

limit = 199
sieve = Prime.eratosthenes(limit)
Enum.each(0..limit, fn n ->
  if x=elem(sieve, n), do: :io.format("~3w", [x]), else: :io.format("  .")
  if rem(n+1, 20)==0, do: IO.puts ""
end)
```


{{out}}

```txt

  .  .  2  3  .  5  .  7  .  .  . 11  . 13  .  .  . 17  . 19
  .  .  . 23  .  .  .  .  . 29  . 31  .  .  .  .  . 37  .  .
  . 41  . 43  .  .  . 47  .  .  .  .  . 53  .  .  .  .  . 59
  . 61  .  .  .  .  . 67  .  .  . 71  . 73  .  .  .  .  . 79
  .  .  . 83  .  .  .  .  . 89  .  .  .  .  .  .  . 97  .  .
  .101  .103  .  .  .107  .109  .  .  .113  .  .  .  .  .  .
  .  .  .  .  .  .  .127  .  .  .131  .  .  .  .  .137  .139
  .  .  .  .  .  .  .  .  .149  .151  .  .  .  .  .157  .  .
  .  .  .163  .  .  .167  .  .  .  .  .173  .  .  .  .  .179
  .181  .  .  .  .  .  .  .  .  .191  .193  .  .  .197  .199

```


Shorter version (but slow):


```elixir

defmodule Sieve do
  def primes_to(limit), do: sieve(Enum.to_list(2..limit))

  defp sieve([h|t]), do: [h|sieve(t -- for n <- 1..length(t), do: h*n)]
  defp sieve([]), do: []
end

```


'''Alternate much faster odds-only version more suitable for immutable data structures using a (hash) Map'''

The above code has a very limited useful range due to being very slow:  for example, to sieve to a million, even changing the algorithm to odds-only, requires over 800 thousand "copy-on-update" operations of the entire saved immutable tuple ("array") of 500 thousand bytes in size, making it very much a "toy" application.  The following code overcomes that problem by using a (immutable/hashed) Map to store the record of the current state of the composite number chains resulting from each of the secondary streams of base primes, which are only 167 in number up to this range; it is a functional "incremental" Sieve of Eratosthenes implementation:

```elixir
defmodule PrimesSoEMap do
  @typep stt :: {integer, integer, integer, Enumerable.integer, %{integer => integer}}

  @spec advance(stt) :: stt
  defp advance {n, bp, q, bps?, map} do
    bps = if bps? === nil do Stream.drop(oddprms(), 1) else bps? end
    nn = n + 2
    if nn >= q do
      inc = bp + bp
      nbps = bps |> Stream.drop(1)
      [nbp] = nbps |> Enum.take(1)
      advance {nn, nbp, nbp * nbp, nbps, map |> Map.put(nn + inc, inc)}
    else if Map.has_key?(map, nn) do
      {inc, rmap} = Map.pop(map, nn)
      [next] =
        Stream.iterate(nn + inc, &(&1 + inc))
          |> Stream.drop_while(&(Map.has_key?(rmap, &1))) |> Enum.take(1)
      advance {nn, bp, q, bps, Map.put(rmap, next, inc)}
    else
      {nn, bp, q, bps, map}
    end end
  end

  @spec oddprms() :: Enumerable.integer
  defp oddprms do # put first base prime cull seq in Map so never empty
    # advance base odd primes to 5 when initialized
    init = {7, 5, 25, nil, %{9 => 6}}
    [3, 5] # to avoid race, preseed with the first 2 elements...
      |> Stream.concat(
            Stream.iterate(init, &(advance &1))
              |> Stream.map(fn {p,_,_,_,_} -> p end))
  end

  @spec primes() :: Enumerable.integer
  def primes do
    Stream.concat([2], oddprms())
  end

end

range = 1000000
IO.write "The first 25 primes are:\n( "
PrimesSoEMap.primes() |> Stream.take(25) |> Enum.each(&(IO.write "#{&1} "))
IO.puts ")"
testfunc =
  fn () ->
    ans =
      PrimesSoEMap.primes() |> Stream.take_while(&(&1 <= range)) |> Enum.count()
    ans end
:timer.tc(testfunc)
  |> (fn {t,ans} ->
    IO.puts "There are #{ans} primes up to #{range}."
    IO.puts "This test bench took #{t} microseconds." end).()
```

{{output}}

```txt
The first 25 primes are:
( 2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 )
There are 78498 primes up to 1000000.
This test bench took 3811957 microseconds.
```


The output time of about 3.81 seconds to one million is on a 1.92 Gigahertz CPU meaning that it takes about 93 thousand CPU clock cycles per prime which is still quite slow compared to mutable data structure implementations but comparable to "functional" implementations in other languages and is slow due to the time to calculate the required hashes.  One advantage that it has is that it is O(n log (log n)) asymptotic computational complexity meaning that it takes not much more than ten times as long to sieve a range ten times higher.

This algorithm could be easily changed to use a Priority Queue (preferably Min-Heap based for the least constant factor computational overhead) to save some of the computation time, but then it will have the same computational complexity as the following code and likely about the same execution time.

'''Alternate faster odds-only version more suitable for immutable data structures using lazy Streams of Co-Inductive Streams'''

In order to save the computation time of computing the hashes, the following version uses a deferred execution Co-Inductive Stream type (constructed using Tuple's) in an infinite tree folding structure (by the `pairs` function):

```elixir
defmodule PrimesSoETreeFolding do
  @typep cis :: {integer, (() -> cis)}
  @typep ciss :: {cis, (() -> ciss)}

  @spec merge(cis, cis) :: cis
  defp merge(xs, ys) do
    {x, restxs} = xs; {y, restys} = ys
    cond do
      x < y -> {x, fn () -> merge(restxs.(), ys) end}
      y < x -> {y, fn () -> merge(xs, restys.()) end}
      true -> {x, fn () -> merge(restxs.(), restys.()) end}
    end
  end

  @spec smlt(integer, integer) :: cis
  defp smlt(c, inc) do
    {c, fn () -> smlt(c + inc, inc) end}
  end

  @spec smult(integer) :: cis
  defp smult(p) do
    smlt(p * p, p + p)
  end
P
  @spec allmults(cis) :: ciss
  defp allmults {p, restps} do
    {smult(p), fn () -> allmults(restps.()) end}
  end

  @spec pairs(ciss) :: ciss
  defp pairs {cs0, restcss0} do
    {cs1, restcss1} = restcss0.()
    {merge(cs0, cs1), fn () -> pairs(restcss1.()) end}
  end

  @spec cmpsts(ciss) :: cis
  defp cmpsts {cs, restcss} do
    {c, restcs} = cs
    {c, fn () -> merge(restcs.(), cmpsts(pairs(restcss.()))) end}
  end

  @spec minusat(integer, cis) :: cis
  defp minusat(n, cmps) do
    {c, restcs} = cmps
    if n < c do
      {n, fn () -> minusat(n + 2, cmps) end}
    else
      minusat(n + 2, restcs.())
    end
  end

  @spec oddprms() :: cis
  defp oddprms() do
    {3, fn () ->
      {5, fn () -> minusat(7, cmpsts(allmults(oddprms()))) end}
    end}
  end

  @spec primes() :: Enumerable.t
  def primes do
    [2] |> Stream.concat(
      Stream.iterate(oddprms(), fn {_, restps} -> restps.() end)
        |> Stream.map(fn {p, _} -> p end)
    )
  end

end

range = 1000000
IO.write "The first 25 primes are:\n( "
PrimesSoETreeFolding.primes() |> Stream.take(25) |> Enum.each(&(IO.write "#{&1} "))
IO.puts ")"
testfunc =
  fn () ->
    ans =
      PrimesSoETreeFolding.primes() |> Stream.take_while(&(&1 <= range)) |> Enum.count()
    ans end
:timer.tc(testfunc)
  |> (fn {t,ans} ->
    IO.puts "There are #{ans} primes up to #{range}."
    IO.puts "This test bench took #{t} microseconds." end).()
```


It's output is identical to the previous version other than the time required is less than half; however, it has a O(n (log n) (log (log n))) asymptotic computation complexity meaning that it gets slower with range faster than the above version.  That said, it would take sieving to billions taking hours before the two would take about the same time.


## Emacs Lisp


```lisp

(defun sieve-set (limit)
  (let ((xs (make-vector (1+ limit) 0)))
    (loop for i from 2 to limit
          when (zerop (aref xs i))
          collect i
          and do (loop for m from (* i i) to limit by i
                       do (aset xs m 1)))))

```


Straightforward implementation of [http://en.wikipedia.org/wiki/Sieve_of_Eratosthenes#Implementation sieve of Eratosthenes], 2 times faster:


```lisp

(defun sieve (limit)
  (let ((xs (vconcat [0 0] (number-sequence 2 limit))))
    (loop for i from 2 to (sqrt limit)
          when (aref xs i)
          do (loop for m from (* i i) to limit by i
                   do (aset xs m 0)))
    (remove 0 xs)))

```



## Erlang


### Erlang using Dicts

{{incorrect|Erlang|See talk page.}}

```Erlang

-module( sieve_of_eratosthenes ).

-export( [primes_upto/1] ).

primes_upto( N ) ->
	Ns = lists:seq( 2, N ),
	Dict = dict:from_list( [{X, potential_prime} || X <- Ns] ),
	{Upto_sqrt_ns, _T} = lists:split( erlang:round(math:sqrt(N)), Ns ),
	{N, Prime_dict} = lists:foldl( fun find_prime/2, {N, Dict}, Upto_sqrt_ns ),
	lists:sort( dict:fetch_keys(Prime_dict) ).



find_prime( N, {Max, Dict} ) -> find_prime( dict:find(N, Dict), N, {Max, Dict} ).

find_prime( error, _N, Acc ) -> Acc;
find_prime( {ok, _Value}, N, {Max, Dict} ) -> {Max, lists:foldl( fun dict:erase/2, Dict, lists:seq(N*N, Max, N) )}.

```

{{out}}

```txt

35> sieve_of_eratosthenes:primes_upto( 20 ).
[2,3,5,7,11,13,17,19]

```


===Erlang Lists of Tuples, Sloww===

A much slower, perverse method, using only lists of tuples. Especially evil is the P = lists:filtermap operation which yields a list for every iteration of the X * M row.
Has the virtue of working for any -> N :)


```Erlang

-module( sieve ).
-export( [main/1,primes/2] ).

main(N) -> io:format("Primes: ~w~n", [ primes(2,N) ]).

primes(M,N) -> primes(M, N,lists:seq( M, N ),[]).

primes(M,N,_Acc,Tuples) when M > N/2-> out(Tuples);

primes(M,N,Acc,Tuples) when length(Tuples) < 1 ->
        primes(M,N,Acc,[{X, X} || X <- Acc]);

primes(M,N,Acc,Tuples) ->
        {SqrtN, _T} = lists:split( erlang:round(math:sqrt(N)), Acc ),
        F = Tuples,
        Ms = lists:filtermap(fun(X) -> if X > 0 -> {true, X * M}; true -> false end end, SqrtN),
        P = lists:filtermap(fun(T) ->
            case lists:keymember(T,1,F) of true ->
            {true, lists:keyreplace(T,1,F,{T,0})};
             _-> false end end,  Ms),
        AA = mergeT(P,lists:last(P),1 ),
        primes(M+1,N,Acc,AA).

mergeT(L,M,Acc) when Acc == length(L) -> M;
mergeT(L,M,Acc) ->
        A = lists:nth(Acc,L),
        B = M,
        Mer = lists:zipwith(fun(X, Y) -> if X < Y -> X; true -> Y end end, A, B),
        mergeT(L,Mer,Acc+1).

out(Tuples) ->
        Primes = lists:filter( fun({_,Y}) -> Y > 0 end,  Tuples),
        [ X || {X,_} <- Primes ].

```

{{out}}

```txt

109> sieve:main(20).
Primes: [2,3,5,7,11,13,17,19]
ok
110> timer:tc(sieve, main, [20]).
Primes: [2,3,5,7,11,13,17,19]
{129,ok}

```



###  Erlang with ordered sets

Since I had written a really odd and slow one, I thought I'd best do a better performer. Inspired by an example from https://github.com/jupp0r


```Erlang


-module(ossieve).
-export([main/1]).

sieve(Candidates,SearchList,Primes,_Maximum) when length(SearchList) == 0 ->
    ordsets:union(Primes,Candidates);
sieve(Candidates,SearchList,Primes,Maximum)  ->
     H = lists:nth(1,string:substr(Candidates,1,1)),
     Reduced1 = ordsets:del_element(H, Candidates),
     {Reduced2, ReducedSearch} = remove_multiples_of(H, Reduced1, SearchList),
     NewPrimes = ordsets:add_element(H,Primes),
     sieve(Reduced2, ReducedSearch, NewPrimes, Maximum).

remove_multiples_of(Number,Candidates,SearchList) ->
    NewSearchList = ordsets:filter( fun(X) -> X >= Number * Number end, SearchList),
    RemoveList = ordsets:filter( fun(X) -> X rem Number == 0 end, NewSearchList),
    {ordsets:subtract(Candidates, RemoveList), ordsets:subtract(NewSearchList, RemoveList)}.

main(N) ->
    io:fwrite("Creating Candidates...~n"),
    CandidateList = lists:seq(3,N,2),
    Candidates = ordsets:from_list(CandidateList),
    io:fwrite("Sieving...~n"),
    ResultSet = ordsets:add_element(2,sieve(Candidates,Candidates,ordsets:new(),N)),
    io:fwrite("Sieved... ~w~n",[ResultSet]).

```

{{out}}

```txt

36> ossieve:main(100).
Creating Candidates...
Sieving...
Sieved... [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97]
ok


```



### Erlang Canonical


A pure list comprehension approach.


```Erlang

-module(sieveof).
-export([main/1,primes/1, primes/2]).

main(X) -> io:format("Primes: ~w~n", [ primes(X) ]).

primes(X) -> sieve(range(2, X)).
primes(X, Y) -> remove(primes(X), primes(Y)).

range(X, X) -> [X];
range(X, Y) -> [X | range(X + 1, Y)].

sieve([X]) -> [X];
sieve([H | T]) -> [H | sieve(remove([H * X || X <-[H | T]], T))].

remove(_, []) -> [];
remove([H | X], [H | Y]) -> remove(X, Y);
remove(X, [H | Y]) -> [H | remove(X, Y)].

```

{out}

```txt

> timer:tc(sieve, main, [100]).
Primes: [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97]
{7350,ok}
61> timer:tc(sieveof, main, [100]).
Primes: [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97]
{363,ok}

```


Clearly not only more elegant, but faster :) Thanks to http://stackoverflow.com/users/113644/g-b


### Erlang ets + cpu distributed implementation

much faster previous erlang examples

```Erlang

#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname p10_4
% vim:syn=erlang

-mode(compile).

main([N0]) ->
    N = list_to_integer(N0),
    ets:new(comp, [public, named_table, {write_concurrency, true} ]),
    ets:new(prim, [public, named_table, {write_concurrency, true}]),
    composite_mc(N),
    primes_mc(N),
    io:format("Answer: ~p ~n", [lists:sort([X||{X,_}<-ets:tab2list(prim)])]).

primes_mc(N) ->
    case erlang:system_info(schedulers) of
        1 -> primes(N);
        C -> launch_primes(lists:seq(1,C), C, N, N div C)
    end.
launch_primes([1|T], C, N, R) -> P = self(), spawn(fun()-> primes(2,R), P ! {ok, prm} end), launch_primes(T, C, N, R);
launch_primes([H|[]], C, N, R)-> P = self(), spawn(fun()-> primes(R*(H-1)+1,N), P ! {ok, prm} end), wait_primes(C);
launch_primes([H|T], C, N, R) -> P = self(), spawn(fun()-> primes(R*(H-1)+1,R*H), P ! {ok, prm} end), launch_primes(T, C, N, R).

wait_primes(0) -> ok;
wait_primes(C) ->
    receive
        {ok, prm} -> wait_primes(C-1)
    after 1000    -> wait_primes(C)
    end.

primes(N) -> primes(2, N).
primes(I,N) when I =< N ->
    case ets:lookup(comp, I) of
        [] -> ets:insert(prim, {I,1})
        ;_ -> ok
    end,
    primes(I+1, N);
primes(I,N) when I > N -> ok.


composite_mc(N) -> composite_mc(N,2,round(math:sqrt(N)),erlang:system_info(schedulers)).
composite_mc(N,I,M,C) when I =< M, C > 0 ->
    C1 = case ets:lookup(comp, I) of
        [] -> comp_i_mc(I*I, I, N), C-1
        ;_ -> C
    end,
    composite_mc(N,I+1,M,C1);
composite_mc(_,I,M,_) when I > M -> ok;
composite_mc(N,I,M,0) ->
    receive
        {ok, cim} -> composite_mc(N,I,M,1)
    after 1000    -> composite_mc(N,I,M,0)
    end.

comp_i_mc(J, I, N) ->
    Parent = self(),
    spawn(fun() ->
        comp_i(J, I, N),
        Parent ! {ok, cim}
    end).

comp_i(J, I, N) when J =< N -> ets:insert(comp, {J, 1}), comp_i(J+I, I, N);
comp_i(J, _, N) when J > N -> ok.

```

{{out}}

```txt

mkh@mkh-xps:~/work/mblog/pr_euler/p10$ ./generator.erl 100
Answer: [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,
         97]

```


another several erlang implementation: http://mijkenator.github.io/2015/11/29/project-euler-problem-10/


## ERRE


```ERRE

PROGRAM SIEVE_ORG
  ! --------------------------------------------------
  ! Eratosthenes Sieve Prime Number Program in BASIC
  ! (da 3 a SIZE*2)   from Byte September 1981
  !---------------------------------------------------
  CONST SIZE%=8190

  DIM FLAGS%[SIZE%]

BEGIN
  PRINT("Only 1 iteration")
  COUNT%=0
  FOR I%=0 TO SIZE% DO
     IF FLAGS%[I%]=TRUE THEN
         !$NULL
       ELSE
         PRIME%=I%+I%+3
         K%=I%+PRIME%
         WHILE NOT (K%>SIZE%) DO
            FLAGS%[K%]=TRUE
            K%=K%+PRIME%
         END WHILE
         PRINT(PRIME%;)
         COUNT%=COUNT%+1
     END IF
  END FOR
  PRINT
  PRINT(COUNT%;" PRIMES")
END PROGRAM

```

{{out}}
last lines of the output screen

```txt

 15749  15761  15767  15773  15787  15791  15797  15803  15809  15817  15823
 15859  15877  15881  15887  15889  15901  15907  15913  15919  15923  15937
 15959  15971  15973  15991  16001  16007  16033  16057  16061  16063  16067
 16069  16073  16087  16091  16097  16103  16111  16127  16139  16141  16183
 16187  16189  16193  16217  16223  16229  16231  16249  16253  16267  16273
 16301  16319  16333  16339  16349  16361  16363  16369  16381
 1899  PRIMES

```



## Euphoria


```euphoria
constant limit = 1000
sequence flags,primes
flags = repeat(1, limit)
for i = 2 to sqrt(limit) do
    if flags[i] then
        for k = i*i to limit by i do
            flags[k] = 0
        end for
    end if
end for

primes = {}
for i = 2 to limit do
    if flags[i] = 1 then
        primes &= i
    end if
end for
? primes
```


Output:

```txt
{2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,
97,101,103,107,109,113,127,131,137,139,149,151,157,163,167,173,179,
181,191,193,197,199,211,223,227,229,233,239,241,251,257,263,269,271,
277,281,283,293,307,311,313,317,331,337,347,349,353,359,367,373,379,
383,389,397,401,409,419,421,431,433,439,443,449,457,461,463,467,479,
487,491,499,503,509,521,523,541,547,557,563,569,571,577,587,593,599,
601,607,613,617,619,631,641,643,647,653,659,661,673,677,683,691,701,
709,719,727,733,739,743,751,757,761,769,773,787,797,809,811,821,823,
827,829,839,853,857,859,863,877,881,883,887,907,911,919,929,937,941,
947,953,967,971,977,983,991,997}
```



## F Sharp


### Short Sweet Functional and Idiotmatic

Well lists may not be lazy, but if you call it a sequence then it's a lazy list!

```fsharp

(*
  An interesting implementation of The Sieve of Eratosthenes.
  Nigel Galloway April 7th., 2017.
*)
let SofE =
  let rec fn n g = seq{ match n with
                        |1 -> yield false; yield! fn g g
                        |_ -> yield  true; yield! fn (n - 1) g}
  let rec fg ng = seq {
    let g = (Seq.findIndex(id) ng) + 2 // decreasingly inefficient with range at O(n)!
    yield g; yield! fn (g - 1) g |> Seq.map2 (&&) ng |> Seq.cache |> fg }
  Seq.initInfinite (fun x -> true) |> fg

```

{{out}}

```txt

> SofE |> Seq.take 10 |> Seq.iter(printfn "%d");;
2
3
5
7
11
13
17
19
23
29

```

Although interesting intellectually, and although the algorithm is more Sieve of Eratosthenes (SoE) than not in that it uses a progression of composite number representations separated by base prime gaps to cull, it isn't really SoE in performance due to several used functions that aren't linear with range, such as the "findIndex" that scans from the beginning of all primes to find the next un-culled value as the next prime in the sequence and the general slowness and inefficiency of F# nested sequence generation.

It is so slow that it takes in the order of seconds just to find the primes to a thousand!

For practical use, one would be much better served by any of the other functional sieves below, which can sieve to a million in less time than it takes this one to sieve to ten thousand.  Those other functional sieves aren't all that many lines of code than this one.


### Functional


'''Richard Bird Sieve'''

This is the idea behind Richard Bird's unbounded code presented in the Epilogue of [http://www.cs.hmc.edu/~oneill/papers/Sieve-JFP.pdf M. O'Neill's article] in Haskell.  It is about twice as much code as the Haskell code because F# does not have a built-in lazy list so that the effect must be constructed using a Co-Inductive Stream (CIS) type since no memoization is required, along with the use of recursive functions in combination with sequences.  The type inference needs some help with the new CIS type (including selecting the generic type for speed).  Note the use of recursive functions to implement multiple non-sharing delayed generating base primes streams, which along with these being non-memoizing means that the entire primes stream is not held in memory as for the original Bird code:

```fsharp
type 'a CIS = CIS of 'a * (unit -> 'a CIS) //'Co Inductive Stream for laziness

let primesBird() =
  let rec (^^) (CIS(x, xtlf) as xs) (CIS(y, ytlf) as ys) = // stream merge function
    if x < y then CIS(x, fun() -> xtlf() ^^ ys)
    elif y < x then CIS(y, fun() -> xs ^^ ytlf())
    else CIS(x, fun() -> xtlf() ^^ ytlf()) // no duplication
  let pmltpls p = let rec nxt c = CIS(c, fun() -> nxt (c + p)) in nxt (p * p)
  let rec allmltps (CIS(p, ptlf)) = CIS(pmltpls p, fun() -> allmltps (ptlf()))
  let rec cmpsts (CIS(CIS(c, ctlf), amstlf)) =
    CIS(c, fun() -> (ctlf()) ^^ (cmpsts (amstlf())))
  let rec minusat n (CIS(c, ctlf) as cs) =
    if n < c then CIS(n, fun() -> minusat (n + 1u) cs)
    else minusat (n + 1u) (ctlf())
  let rec baseprms() = CIS(2u, fun() -> baseprms() |> allmltps |> cmpsts |> minusat 3u)
  Seq.unfold (fun (CIS(p, ptlf)) -> Some(p, ptlf())) (baseprms())
```


The above code sieves all numbers of two and up including all even numbers as per the page specification; the following code makes the very minor changes for an odds-only sieve, with a speedup of over a factor of two:

```fsharp
type 'a CIS = CIS of 'a * (unit -> 'a CIS) //'Co Inductive Stream for laziness

let primesBirdOdds() =
  let rec (^^) (CIS(x, xtlf) as xs) (CIS(y, ytlf) as ys) = // stream merge function
    if x < y then CIS(x, fun() -> xtlf() ^^ ys)
    elif y < x then CIS(y, fun() -> xs ^^ ytlf())
    else CIS(x, fun() -> xtlf() ^^ ytlf()) // no duplication
  let pmltpls p = let adv = p + p
                  let rec nxt c = CIS(c, fun() -> nxt (c + adv)) in nxt (p * p)
  let rec allmltps (CIS(p, ptlf)) = CIS(pmltpls p, fun() -> allmltps (ptlf()))
  let rec cmpsts (CIS(CIS(c, ctlf), amstlf)) =
    CIS(c, fun() -> ctlf() ^^ cmpsts (amstlf()))
  let rec minusat n (CIS(c, ctlf) as cs) =
    if n < c then CIS(n, fun() -> minusat (n + 2u) cs)
    else minusat (n + 2u) (ctlf())
  let rec oddprms() = CIS(3u, fun() -> oddprms() |> allmltps |> cmpsts |> minusat 5u)
  Seq.unfold (fun (CIS(p, ptlf)) -> Some(p, ptlf())) (CIS(2u, fun() -> oddprms()))
```


'''Tree Folding Sieve'''

The above code is still somewhat inefficient as it operates on a linear right extending structure that deepens linearly with increasing base primes (those up to the square root of the currently sieved number); the following code changes the structure into an infinite binary tree-like folding by combining each pair of prime composite streams before further processing as usual - this decreases the processing by approximately a factor of log n:

```fsharp
type 'a CIS = CIS of 'a * (unit -> 'a CIS) //'Co Inductive Stream for laziness

let primesTreeFold() =
  let rec (^^) (CIS(x, xtlf) as xs) (CIS(y, ytlf) as ys) = // stream merge function
    if x < y then CIS(x, fun() -> xtlf() ^^ ys)
    elif y < x then CIS(y, fun() -> xs ^^ ytlf())
    else CIS(x, fun() -> xtlf() ^^ ytlf()) // no duplication
  let pmltpls p = let adv = p + p
                  let rec nxt c = CIS(c, fun() -> nxt (c + adv)) in nxt (p * p)
  let rec allmltps (CIS(p, ptlf)) = CIS(pmltpls p, fun() -> allmltps (ptlf()))
  let rec pairs (CIS(cs0, cs0tlf)) =
    let (CIS(cs1, cs1tlf)) = cs0tlf() in CIS(cs0 ^^ cs1, fun() -> pairs (cs1tlf()))
  let rec cmpsts (CIS(CIS(c, ctlf), amstlf)) =
    CIS(c, fun() -> ctlf() ^^ (cmpsts << pairs << amstlf)())
  let rec minusat n (CIS(c, ctlf) as cs) =
    if n < c then CIS(n, fun() -> minusat (n + 2u) cs)
    else minusat (n + 2u) (ctlf())
  let rec oddprms() = CIS(3u, fun() -> oddprms() |> allmltps |> cmpsts |> minusat 5u)
  Seq.unfold (fun (CIS(p, ptlf)) -> Some(p, ptlf())) (CIS(2u, fun() -> oddprms()))
```


The above code is over four times faster than the "BirdOdds" version (at least 10x faster than the first, "primesBird", producing the millionth prime) and is moderately useful for a range of the first million primes or so.

'''Priority Queue Sieve'''

In order to investigate Priority Queue Sieves as espoused by O'Neill in the referenced article, one must find an equivalent implementation of a Min Heap Priority Queue as used by her.  There is such an purely functional implementation [http://rosettacode.org/wiki/Priority_queue#Functional in RosettaCode translated from the Haskell code she used], from which the essential parts are duplicated here (Note that the key value is given an integer type in order to avoid the inefficiency of F# in generic comparison):

```fsharp>[<RequireQualifiedAccess
]
module MinHeap =

  type HeapEntry<'V> = struct val k:uint32 val v:'V new(k,v) = {k=k;v=v} end
  [<CompilationRepresentation(CompilationRepresentationFlags.UseNullAsTrueValue)>]
  [<NoEquality; NoComparison>]
  type PQ<'V> =
         | Mt
         | Br of HeapEntry<'V> * PQ<'V> * PQ<'V>

  let empty = Mt

  let peekMin = function | Br(kv, _, _) -> Some(kv.k, kv.v)
                         | _            -> None

  let rec push wk wv =
    function | Mt -> Br(HeapEntry(wk, wv), Mt, Mt)
             | Br(vkv, ll, rr) ->
                 if wk <= vkv.k then
                   Br(HeapEntry(wk, wv), push vkv.k vkv.v rr, ll)
                 else Br(vkv, push wk wv rr, ll)

  let private siftdown wk wv pql pqr =
    let rec sift pl pr =
      match pl with
        | Mt -> Br(HeapEntry(wk, wv), Mt, Mt)
        | Br(vkvl, pll, plr) ->
            match pr with
              | Mt -> if wk <= vkvl.k then Br(HeapEntry(wk, wv), pl, Mt)
                      else Br(vkvl, Br(HeapEntry(wk, wv), Mt, Mt), Mt)
              | Br(vkvr, prl, prr) ->
                  if wk <= vkvl.k && wk <= vkvr.k then Br(HeapEntry(wk, wv), pl, pr)
                  elif vkvl.k <= vkvr.k then Br(vkvl, sift pll plr, pr)
                  else Br(vkvr, pl, sift prl prr)
    sift pql pqr

  let replaceMin wk wv = function | Mt -> Mt
                                  | Br(_, ll, rr) -> siftdown wk wv ll rr
```


Except as noted for any individual code, all of the following codes need the following prefix code in order to implement the non-memoizing Co-Inductive Streams (CIS's) and to set the type of particular constants used in the codes to the same time as the "Prime" type:

```fsharp
type CIS<'T> = struct val v: 'T val cont: unit -> CIS<'T> new(v,cont) = {v=v;cont=cont} end
type Prime = uint32
let frstprm = 2u
let frstoddprm = 3u
let inc1 = 1u
let inc = 2u
```


The F# equivalent to O'Neill's "odds-only" code is then implemented as follows, which needs the included changed prefix in order to change the primes type to a larger one to prevent overflow (as well the key type for the MinHeap needs to be changed from uint32 to uint64); it is functionally the same as the O'Neill code other than for minor changes to suit the use of CIS streams and the option output of the "peekMin" function:

```fsharp
type CIS<'T> = struct val v: 'T val cont: unit -> CIS<'T> new(v,cont) = {v=v;cont=cont} end
type Prime = uint64
let frstprm = 2UL
let frstoddprm = 3UL
let inc = 2UL

let primesPQ() =
  let pmult p (xs: CIS<Prime>) = // does map (* p) xs
    let rec nxtm (cs: CIS<Prime>) =
      CIS(p * cs.v, fun() -> nxtm (cs.cont())) in nxtm xs
  let insertprime p xs table =
    MinHeap.push (p * p) (pmult p xs) table
  let rec sieve' (ns: CIS<Prime>) table =
    let nextcomposite = match MinHeap.peekMin table with
                          | None -> ns.v // never happens
                          | Some (k, _) -> k
    let rec adjust table =
      let (n, advs) = match MinHeap.peekMin table with
                        | None -> (ns.v, ns.cont()) // never happens
                        | Some kv -> kv
      if n <= ns.v then adjust (MinHeap.replaceMin advs.v (advs.cont()) table)
      else table
    if nextcomposite <= ns.v then sieve' (ns.cont()) (adjust table)
    else let n = ns.v in CIS(n, fun() ->
           let nxtns = ns.cont() in sieve' nxtns (insertprime n nxtns table))
  let rec sieve (ns: CIS<Prime>) = let n = ns.v in CIS(n, fun() ->
      let nxtns = ns.cont() in sieve' nxtns (insertprime n nxtns MinHeap.empty))
  let odds = // is the odds CIS from 3 up
    let rec nxto i = CIS(i, fun() -> nxto (i + inc)) in nxto frstoddprm
  Seq.unfold (fun (cis: CIS<Prime>) -> Some(cis.v, cis.cont()))
             (CIS(frstprm, fun() -> (sieve odds)))
```


However, that algorithm suffers in speed and memory use due to over-eager adding of prime composite streams to the queue such that the queue used is much larger than it needs to be and a much larger range of primes number must be used in order to avoid numeric overflow on the square of the prime added to the queue.  The following code corrects that by using a secondary (actually a multiple of) base primes streams which are constrained to be based on a prime that is no larger than the square root of the currently sieved number - this permits the use of much smaller Prime types as per the default prefix:

```fsharp
let primesPQx() =
  let rec nxtprm n pq q (bps: CIS<Prime>) =
    if n >= q then let bp = bps.v in let adv = bp + bp
                   let nbps = bps.cont() in let nbp = nbps.v
                   nxtprm (n + inc) (MinHeap.push (n + adv) adv pq) (nbp * nbp) nbps
    else let ck, cv = match MinHeap.peekMin pq with
                        | None -> (q, inc) // only happens until first insertion
                        | Some kv -> kv
         if n >= ck then let rec adjpq ck cv pq =
                             let npq = MinHeap.replaceMin (ck + cv) cv pq
                             match MinHeap.peekMin npq with
                               | None -> npq // never happens
                               | Some(nk, nv) -> if n >= nk then adjpq nk nv npq
                                                 else npq
                         nxtprm (n + inc) (adjpq ck cv pq) q bps
         else CIS(n, fun() -> nxtprm (n + inc) pq q bps)
  let rec oddprms() = CIS(frstoddprm, fun() ->
      nxtprm (frstoddprm + inc) MinHeap.empty (frstoddprm * frstoddprm) (oddprms()))
  Seq.unfold (fun (cis: CIS<Prime>) -> Some(cis.v, cis.cont()))
             (CIS(frstprm, fun() -> (oddprms())))
```


The above code is well over five times faster than the previous translated O'Neill version for the given variety of reasons.

Although slightly faster than the Tree Folding code, this latter code is also limited in practical usefulness to about the first one to ten million primes or so.

All of the above codes can be tested in the F# REPL with the following to produce the millionth prime (the "nth" function is zero based):

```txt
> primesXXX() |> Seq.nth 999999;;
```


where primesXXX() is replaced by the given primes generating function to be tested, and which all produce the following output (after a considerable wait in some cases):

{{output}}

```txt
val it : Prime = 15485863u
```



### Imperative


The following code is written in functional style other than it uses a mutable bit array to sieve the composites:


```fsharp
let primes limit =
  let buf = System.Collections.BitArray(int limit + 1, true)
  let cull p = { p * p .. p .. limit } |> Seq.iter (fun c -> buf.[int c] <- false)
  { 2u .. uint32 (sqrt (double limit)) } |> Seq.iter (fun c -> if buf.[int c] then cull c)
  { 2u .. limit } |> Seq.map (fun i -> if buf.[int i] then i else 0u) |> Seq.filter ((<>) 0u)

[<EntryPoint>]
let main argv =
  if argv = null || argv.Length = 0 then failwith "no command line argument for limit!!!"
  printfn "%A" (primes (System.UInt32.Parse argv.[0]) |> Seq.length)
  0 // return an integer exit code
```


Substituting the following minor changes to the code for the "primes" function will only deal with the odd prime candidates for a speed up of over a factor of two as well as a reduction of the buffer size by a factor of two:


```fsharp
let primes limit =
  let lmtb,lmtbsqrt = (limit - 3u) / 2u, (uint32 (sqrt (double limit)) - 3u) / 2u
  let buf = System.Collections.BitArray(int lmtb + 1, true)
  let cull i = let p = i + i + 3u in let s = p * (i + 1u) + i in
               { s .. p .. lmtb } |> Seq.iter (fun c -> buf.[int c] <- false)
  { 0u .. lmtbsqrt } |> Seq.iter (fun i -> if buf.[int i] then cull i )
  let oddprms = { 0u .. lmtb } |> Seq.map (fun i -> if buf.[int i] then i + i + 3u else 0u)
                |> Seq.filter ((<>) 0u)
  seq { yield 2u; yield! oddprms }
```


The following code uses other functional forms for the inner culling loops of the "primes function" to reduce the use of inefficient sequences so as to reduce the execution time by another factor of almost three:


```fsharp
let primes limit =
  let lmtb,lmtbsqrt = (limit - 3u) / 2u, (uint32 (sqrt (double limit)) - 3u) / 2u
  let buf = System.Collections.BitArray(int lmtb + 1, true)
  let rec culltest i = if i <= lmtbsqrt then
                         let p = i + i + 3u in let s = p * (i + 1u) + i in
                         let rec cullp c = if c <= lmtb then buf.[int c] <- false; cullp (c + p)
                         (if buf.[int i] then cullp s); culltest (i + 1u) in culltest 0u
  seq {yield 2u; for i = 0u to lmtb do if buf.[int i] then yield i + i + 3u }
```


Now much of the remaining execution time is just the time to enumerate the primes as can be seen by turning "primes" into a primes counting function by substituting the following for the last line in the above code doing the enumeration; this makes the code run about a further five times faster:


```fsharp
  let rec count i acc =
    if i > int lmtb then acc else if buf.[i] then count (i + 1) (acc + 1) else count (i + 1) acc
  count 0 1
```


Since the final enumeration of primes is the main remaining bottleneck, it is worth using a "roll-your-own" enumeration implemented as an object expression so as to save many inefficiencies in the use of the built-in seq computational expression by substituting the following code for the last line of the previous codes, which will decrease the execution time by a factor of over three (instead of almost five for the counting-only version, making it almost as fast):


```fsharp
  let nmrtr() =
    let i = ref -2
    let rec nxti() = i:=!i + 1;if !i <= int lmtb && not buf.[!i] then nxti() else !i <= int lmtb
    let inline curr() = if !i < 0 then (if !i= -1 then 2u else failwith "Enumeration not started!!!")
                        else let v = uint32 !i in v + v + 3u
    { new System.Collections.Generic.IEnumerator<_> with
        member this.Current = curr()
      interface System.Collections.IEnumerator with
        member this.Current = box (curr())
        member this.MoveNext() = if !i< -1 then i:=!i+1;true else nxti()
        member this.Reset() = failwith "IEnumerator.Reset() not implemented!!!"a
      interface System.IDisposable with
        member this.Dispose() = () }
  { new System.Collections.Generic.IEnumerable<_> with
      member this.GetEnumerator() = nmrtr()
    interface System.Collections.IEnumerable with
      member this.GetEnumerator() = nmrtr() :> System.Collections.IEnumerator }
```


The various optimization techniques shown here can be used "jointly and severally" on any of the basic versions for various trade-offs between code complexity and performance.  Not shown here are other techniques of making the sieve faster, including extending wheel factorization to much larger wheels such as 2/3/5/7, pre-culling the arrays, page segmentation, and multi-processing.


### Almost functional Unbounded


the following '''odds-only''' implmentations are written in an almost functional style avoiding the use of mutability except for the contents of the data structures uses to hold the state of the and any mutability necessary to implement a "roll-your-own" IEnumberable iterator interface for speed.

'''Unbounded Dictionary (Mutable Hash Table) Based Sieve'''

The following code uses the DotNet Dictionary class instead of the above functional Priority Queue to implement the sieve; as average (amortized) hash table access is O(1) rather than O(log n) as for the priority queue, this implementation is slightly faster than the priority queue version for the first million primes and will always be faster for any range above some low range value:

```fsharp
type Prime = uint32
let frstprm = 2u
let frstoddprm = 3u
let inc = 2u
let primesDict() =
  let dct = System.Collections.Generic.Dictionary()
  let rec nxtprm n q (bps: CIS<Prime>) =
    if n >= q then let bp = bps.v in let adv = bp + bp
                   let nbps = bps.cont() in let nbp = nbps.v
                   dct.Add(n + adv, adv)
                   nxtprm (n + inc) (nbp * nbp) nbps
    else if dct.ContainsKey(n) then
           let adv = dct.[n]
           dct.Remove(n) |> ignore
//           let mutable nn = n + adv // ugly imperative code
//           while dct.ContainsKey(nn) do nn <- nn + adv
//           dct.Add(nn, adv)
           let rec nxtmt k = // advance to next empty spot
             if dct.ContainsKey(k) then nxtmt (k + adv)
             else dct.Add(k, adv) in nxtmt (n + adv)
           nxtprm (n + inc) q bps
         else CIS(n, fun() -> nxtprm (n + inc) q bps)
  let rec oddprms() = CIS(frstoddprm, fun() ->
      nxtprm (frstoddprm + inc) (frstoddprm * frstoddprm) (oddprms()))
  Seq.unfold (fun (cis: CIS<Prime>) -> Some(cis.v, cis.cont()))
             (CIS(frstprm, fun() -> (oddprms())))
```


The above code uses functional forms of code (with the imperative style commented out to show how it could be done imperatively) and also uses a recursive non-sharing secondary source of base primes just as for the Priority Queue version.  As for the functional codes, the Primes type can easily be changed to "uint64" for wider range of sieving.

In spite of having true O(n log log n) Sieve of Eratosthenes computational complexity where n is the range of numbers to be sieved, the above code is still not particularly fast due to the time required to compute the hash values and manipulations of the hash table.

'''Unbounded Page-Segmented Bit-Packed Odds-Only Mutable Array Sieve'''

Note that the following code is used for the F# entry [[Extensible_prime_generator#Unbounded_Mutable_Array_Generator]] of the Extensible prime generator page.

All of the above unbounded implementations including the above Dictionary based version are quite slow due to their large constant factor computational overheads, making them more of an intellectual exercise than something practical, especially when larger sieving ranges are required.  The following code implements an unbounded page segmented version of the sieve in not that many more lines of code, yet runs about 25 times faster than the Dictionary version for larger ranges of sieving such as to one billion; it uses functional forms without mutability other than for the contents of the arrays and the `primes` enumeration generator function that must use mutability for speed:

```fsharp
type Prime = float // use uint64/int64 for regular 64-bit F#
type private PrimeNdx = float // they are slow in JavaScript polyfills

let inline private prime n = float n // match these convenience conversions
let inline private primendx n = float n // with the types above!

let private cPGSZBTS = (1 <<< 14) * 8 // sieve buffer size in bits = CPUL1CACHE

type private SieveBuffer = uint8[]

/// a Co-Inductive Stream (CIS) of an "infinite" non-memoized series...
type private CIS<'T> = CIS of 'T * (unit -> CIS<'T>) //' apostrophe formatting adjustment

/// lazy list (memoized) series of base prime page arrays...
type private BasePrime = uint32
type private BasePrimeArr = BasePrime[]
type private BasePrimeArrs = BasePrimeArrs of BasePrimeArr * Option<Lazy<BasePrimeArrs>>

/// Masking array is faster than bit twiddle bit shifts!
let private cBITMASK = [| 1uy; 2uy; 4uy; 8uy; 16uy; 32uy; 64uy; 128uy |]

let private cullSieveBuffer lwi (bpas: BasePrimeArrs) (sb: SieveBuffer) =
  let btlmt = (sb.Length <<< 3) - 1 in let lmti = lwi + primendx btlmt
  let rec loopbp (BasePrimeArrs(bpa, bpatl) as ibpas) i =
    if i >= bpa.Length then
      match bpatl with
      | None -> ()
      | Some lv -> loopbp lv.Value 0 else
    let bp = prime bpa.[i] in let bpndx = primendx ((bp - prime 3) / prime 2)
    let s = (bpndx * primendx 2) * (bpndx + primendx 3) + primendx 3 in let bpint = int bp
    if s <= lmti then
      let s0 = // page cull start address calculation...
        if s >= lwi then int (s - lwi) else
        let r = (lwi - s) % (primendx bp)
        if r = primendx 0 then 0 else int (bp - prime r)
      let slmt = min btlmt (s0 - 1 + (bpint <<< 3))
      let rec loopc c = // loop "unpeeling" is used so
        if c <= slmt then // a constant mask can be used over the inner loop
          let msk = cBITMASK.[c &&& 7]
          let rec loopw w =
            if w < sb.Length then sb.[w] <- sb.[w] ||| msk; loopw (w + bpint)
          loopw (c >>> 3); loopc (c + bpint)
      loopc s0; loopbp ibpas (i + 1) in loopbp bpas 0

/// fast Counting Look Up Table (CLUT) for pop counting...
let private cCLUT =
  let arr = Array.zeroCreate 65536
  let rec popcnt n cnt = if n > 0 then popcnt (n &&& (n - 1)) (cnt + 1) else uint8 cnt
  let rec loop i = if i < 65536 then arr.[i] <- popcnt i 0; loop (i + 1)
  loop 0; arr

let countSieveBuffer ndxlmt (sb: SieveBuffer): int =
  let lstw = (ndxlmt >>> 3) &&& -2
  let msk = (-2 <<< (ndxlmt &&& 15)) &&& 0xFFFF
  let inline cntem i m =
    int cCLUT.[int (((uint32 sb.[i + 1]) <<< 8) + uint32 sb.[i]) ||| m]
  let rec loop i cnt =
    if i >= lstw then cnt - cntem lstw msk else loop (i + 2) (cnt - cntem i 0)
  loop 0 ((lstw <<< 3) + 16)

/// a CIS series of pages from the given start index with the given SieveBuffer size,
/// and provided with a polymorphic converter function to produce
/// and type of result from the culled page parameters...
let rec private makePrimePages strtwi btsz
                               (cnvrtrf: PrimeNdx -> SieveBuffer -> 'T): CIS<'T> =
  let bpas = makeBasePrimes() in let sb = Array.zeroCreate (btsz >>> 3)
  let rec nxtpg lwi =
    Array.fill sb 0 sb.Length 0uy; cullSieveBuffer lwi bpas sb
    CIS(cnvrtrf lwi sb, fun() -> nxtpg (lwi + primendx btsz))
  nxtpg strtwi

/// secondary feed of lazy list of memoized pages of base primes...
and private makeBasePrimes(): BasePrimeArrs =
  let sb2bpa lwi (sb: SieveBuffer) =
    let bsbp = uint32 (primendx 3 + lwi + lwi)
    let arr = Array.zeroCreate <| countSieveBuffer 255 sb
    let rec loop i j =
      if i < 256 then
        if sb.[i >>> 3] &&& cBITMASK.[i &&& 7] <> 0uy then loop (i + 1) j
        else arr.[j] <- bsbp + uint32 (i + i); loop (i + 1) (j + 1)
    loop 0 0; arr
  // finding the first page as not part of the loop and making succeeding
  // pages lazy breaks the recursive data race!
  let frstsb = Array.zeroCreate 32
  let fkbpas = BasePrimeArrs(sb2bpa (primendx 0) frstsb, None)
  cullSieveBuffer (primendx 0) fkbpas frstsb
  let rec nxtbpas (CIS(bpa, tlf)) = BasePrimeArrs(bpa, Some(lazy (nxtbpas (tlf()))))
  BasePrimeArrs(sb2bpa (primendx 0) frstsb,
                Some(lazy (nxtbpas <| makePrimePages (primendx 256) 256 sb2bpa)))

/// produces a generator of primes; uses mutability for better speed...
let primes(): unit -> Prime =
  let sb2prms lwi (sb: SieveBuffer) = lwi, sb in let mutable ndx = -1
  let (CIS((nlwi, nsb), npgtlf)) = // use page generator function above!
    makePrimePages (primendx 0) cPGSZBTS sb2prms
  let mutable lwi = nlwi in let mutable sb = nsb
  let mutable pgtlf = npgtlf
  let mutable baseprm = prime 3 + prime (lwi + lwi)
  fun() ->
    if ndx < 0 then ndx <- 0; prime 2 else
    let inline notprm i = sb.[i >>> 3] &&& cBITMASK.[i &&& 7] <> 0uy
    while ndx < cPGSZBTS && notprm ndx do ndx <- ndx + 1
    if ndx >= cPGSZBTS then // get next page if over
      let (CIS((nlwi, nsb), npgtlf)) = pgtlf() in ndx <- 0
      lwi <- nlwi; sb <- nsb; pgtlf <- npgtlf
      baseprm <- prime 3 + prime (lwi + lwi)
      while notprm ndx do ndx <- ndx + 1
    let ni = ndx in ndx <- ndx + 1 // ready for next call!
    baseprm + prime (ni + ni)

let countPrimesTo (limit: Prime): int = // much faster!
  if limit < prime 3 then (if limit < prime 2 then 0 else 1) else
  let topndx = (limit - prime 3) / prime 2 |> primendx
  let sb2cnt lwi (sb: SieveBuffer) =
    let btlmt = (sb.Length <<< 3) - 1 in let lmti = lwi + primendx btlmt
    countSieveBuffer
      (if lmti < topndx then btlmt else int (topndx - lwi)) sb, lmti
  let rec loop (CIS((cnt, nxti), tlf)) count =
    if nxti < topndx then loop (tlf()) (count + cnt)
    else count + cnt
  loop <| makePrimePages (primendx 0) cPGSZBTS sb2cnt <| 1

/// sequences are convenient but slow...
let primesSeq() = primes() |> Seq.unfold (fun gen -> Some(gen(), gen))
printfn "The first 25 primes are:  %s"
  ( primesSeq() |> Seq.take 25
      |> Seq.fold (fun s p -> s + string p + " ") "" )
printfn "There are %d primes up to a million."
  ( primesSeq() |> Seq.takeWhile ((>=) (prime 1000000)) |> Seq.length )

let rec cntto gen lmt cnt = // faster than seq's but still slow
  if gen() > lmt then cnt else cntto gen lmt (cnt + 1)

let limit = prime 1_000_000_000
let start = System.DateTime.Now.Ticks
// let answr = cntto (primes()) limit 0 // slower way!
let answr = countPrimesTo limit // over twice as fast way!
let elpsd = (System.DateTime.Now.Ticks - start) / 10000L
printfn "Found %d primes to %A in %d milliseconds." answr limit elpsd
```


{{out}}

```txt
The first 25 primes are:  2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97
There are 78498 primes up to a million.
Found 50847534 primes to 1000000000 in 2161 milliseconds.
```


As with all of the efficient unbounded sieves, the above code uses a secondary enumerator of the base primes less than the square root of the currently culled range, which is this case is a lazy (deferred memoized evaluation) binding by small pages of base primes which also uses the laziness of the deferral of subsequent pages so as to avoid a race condition.

The above code is written to output the "uint64" type for very large ranges of primes since there is little computational cost to doing this for this algorithm when used with 64-bit compilation; however, for the Fable transpiled to JavaScript, the largest contiguous integer that can be represented is the 64-bit floating point mantissa of 52 bits and thus the large numbers can be represented by floats in this case since a 64-bit polyfill is very slow.  As written, the practical range for this sieve is about 16 billion, however, it can be extended to about 10^14 (a week or two of execution time) by setting the "PGSZBTS" constant to the size of the CPU L2 cache rather than the L1 cache (L2 is up to about two Megabytes for modern high end desktop CPU's) at a slight loss of efficiency (a factor of up to two or so) per composite number culling operation due to the slower memory access time.  When the Fable compilation option is used, execution speed is roughly the same as using F# with DotNet Core.

Even with the custom `primes` enumerator generator (the F#/Fable built-in sequence operators are terribly inefficient), the time to enumerate the resulting primes takes longer than the time to actually cull the composite numbers from the sieving arrays.  The time to do the actual culling is thus over 50 times faster than done using the Dictionary version.  The slowness of enumeration, no matter what further tweaks are done to improve it (each value enumerated will always take a function calls and a scan loop that will always take something in the order of 100 CPU clock cycles per value), means that further gains in speed using extreme wheel factorization and multi-processing have little point unless the actual work on the resulting primes is done through use of auxiliary functions not using iteration.  Such a function is provided here to count the primes by pages using a "pop count" look up table to reduce the counting time to only a small fraction of a second.


## Factor

Factor already contains two implementations of the sieve of Eratosthenes in <code>math.primes.erato</code> and <code>math.primes.erato.fast</code>. It is suggested to use one of them for real use, as they use faster types, faster unsafe arithmetic, and/or wheels to speed up the sieve further. Shown here is a more straightforward implementation that adheres to the restrictions given by the task (namely, no wheels).

Factor is pleasantly multiparadigm. Usually, it's natural to write more functional or declarative code in Factor, but this is an instance where it is more natural to write imperative code. Lexical variables are useful here for expressing the necessary mutations in a clean way.

```factor
USING: bit-arrays io kernel locals math math.functions
math.ranges prettyprint sequences ;
IN: rosetta-code.sieve-of-erato

<PRIVATE

: init-sieve ( n -- seq )   ! Include 0 and 1 for easy indexing.
    1 - <bit-array> dup set-bits ?{ f f } prepend ;

! Given the sieve and a prime starting index, create a range of
! values to mark composite. Start at the square of the prime.
: to-mark ( seq n -- range )
    [ length 1 - ] [ dup dup * ] bi* -rot <range> ;

! Mark multiples of prime n as composite.
: mark-nths ( seq n -- )
    dupd to-mark [ swap [ f ] 2dip set-nth ] with each ;

: next-prime ( index seq -- n ) [ t = ] find-from drop ;

PRIVATE>

:: sieve ( n -- seq )
    n sqrt 2 n init-sieve :> ( limit i! s )
    [ i limit < ]             ! sqrt optimization
    [ s i mark-nths i 1 + s next-prime i! ] while t s indices ;

: sieve-demo ( -- )
    "Primes up to 120 using sieve of Eratosthenes:" print
    120 sieve . ;

MAIN: sieve-demo
```


=={{header|Fōrmulæ}}==

In [http://wiki.formulae.org/Sieve_of_Eratosthenes this] page you can see the solution of this task.

Fōrmulæ programs are not textual, visualization/edition of programs is done showing/manipulating structures but not text ([http://wiki.formulae.org/Editing_F%C5%8Drmul%C3%A6_expressions more info]). Moreover, there can be multiple visual representations of the same program. Even though it is possible to have textual representation &mdash;i.e. XML, JSON&mdash; they are intended for transportation effects more than visualization and edition.

The option to show Fōrmulæ programs and their results is showing images. Unfortunately images cannot be uploaded in Rosetta Code.


## Forth

 : prime? ( n -- ? ) here + c@ 0= ;
 : composite! ( n -- ) here + 1 swap c! ;

 : sieve ( n -- )
   here over erase
   2
   begin
     2dup dup * >
   while
     dup prime? if
       2dup dup * do
         i composite!
       dup +loop
     then
     1+
   repeat
   drop
   ." Primes: " 2 do i prime? if i . then loop ;

 100 sieve


## Fortran

{{works with|Fortran|90 and later}}

```fortran
program sieve

  implicit none
  integer, parameter :: i_max = 100
  integer :: i
  logical, dimension (i_max) :: is_prime

  is_prime = .true.
  is_prime (1) = .false.
  do i = 2, int (sqrt (real (i_max)))
    if (is_prime (i)) is_prime (i * i : i_max : i) = .false.
  end do
  do i = 1, i_max
    if (is_prime (i)) write (*, '(i0, 1x)', advance = 'no') i
  end do
  write (*, *)

end program sieve
```

Output:
<lang>2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97
```

Optimised using a pre-computed wheel based on 2:

```fortran
program sieve_wheel_2

  implicit none
  integer, parameter :: i_max = 100
  integer :: i
  logical, dimension (i_max) :: is_prime

  is_prime = .true.
  is_prime (1) = .false.
  is_prime (4 : i_max : 2) = .false.
  do i = 3, int (sqrt (real (i_max))), 2
    if (is_prime (i)) is_prime (i * i : i_max : 2 * i) = .false.
  end do
  do i = 1, i_max
    if (is_prime (i)) write (*, '(i0, 1x)', advance = 'no') i
  end do
  write (*, *)

end program sieve_wheel_2
```

Output:
<lang>2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97
```



## FreeBASIC


```freebasic
' FB 1.05.0

Sub sieve(n As Integer)
  If n < 2 Then Return
  Dim a(2 To n) As Integer
  For i As Integer = 2 To n : a(i) = i : Next
  Dim As Integer p = 2, q
  ' mark non-prime numbers by setting the corresponding array element to 0
  Do
    For j As Integer = p * p To n Step p
      a(j) = 0
    Next j
    ' look for next non-zero element in array after 'p'
    q = 0
    For j As Integer = p + 1 To Sqr(n)
      If a(j) <> 0 Then
        q = j
        Exit For
      End If
    Next j
    If q = 0 Then Exit Do
    p = q
  Loop

  ' print the non-zero numbers remaining i.e. the primes
  For i As Integer = 2 To n
    If a(i) <> 0 Then
      Print Using "####"; a(i);
    End If
  Next
  Print
End Sub

Print "The primes up to 1000 are :"
Print
sieve(1000)
Print
Print "Press any key to quit"
Sleep
```


{{out}}

```txt

The primes up to 1000 are :

   2   3   5   7  11  13  17  19  23  29  31  37  41  43  47  53  59  61  67  71
  73  79  83  89  97 101 103 107 109 113 127 131 137 139 149 151 157 163 167 173
 179 181 191 193 197 199 211 223 227 229 233 239 241 251 257 263 269 271 277 281
 283 293 307 311 313 317 331 337 347 349 353 359 367 373 379 383 389 397 401 409
 419 421 431 433 439 443 449 457 461 463 467 479 487 491 499 503 509 521 523 541
 547 557 563 569 571 577 587 593 599 601 607 613 617 619 631 641 643 647 653 659
 661 673 677 683 691 701 709 719 727 733 739 743 751 757 761 769 773 787 797 809
 811 821 823 827 829 839 853 857 859 863 877 881 883 887 907 911 919 929 937 941
 947 953 967 971 977 983 991 997

```



## Free Pascal


### Basic version

function Sieve returns a list of primes less than or equal to the given aLimit

```pascal

program prime_sieve;
{$mode objfpc}{$coperators on}
uses
  SysUtils, GVector;
type
  TPrimeList = specialize TVector<DWord>;
function Sieve(aLimit: DWord): TPrimeList;
var
  IsPrime: array of Boolean;
  I, SqrtBound: DWord;
  J: QWord;
begin
  Result := TPrimeList.Create;
  Inc(aLimit, Ord(aLimit < High(DWord))); //not a problem because High(DWord) is composite
  SetLength(IsPrime, aLimit);
  FillChar(Pointer(IsPrime)^, aLimit, Byte(True));
  SqrtBound := Trunc(Sqrt(aLimit));
  for I := 2 to aLimit do
    if IsPrime[I] then
      begin
        Result.PushBack(I);
        if I <= SqrtBound then
          begin
            J := I * I;
            repeat
              IsPrime[J] := False;
              J += I;
            until J > aLimit;
          end;
      end;
end;

 //usage

var
  Limit: DWord = 0;
function ReadLimit: Boolean;
var
  Lim: Int64;
begin
  if (ParamCount = 1) and Lim.TryParse(ParamStr(1), Lim) then
    if (Lim >= 0) and (Lim <= High(DWord)) then
      begin
        Limit := DWord(Lim);
        exit(True);
      end;
  Result := False;
end;
procedure PrintUsage;
begin
  WriteLn('Usage: prime_sieve0 Limit');
  WriteLn('  where Limit in the range [0, ', High(DWord), ']');
  Halt;
end;
procedure PrintPrimes(aList: TPrimeList);
var
  I: DWord;
begin
  for I := 0 to aList.Size - 2 do
    Write(aList[I], ', ');
  WriteLn(aList[aList.Size - 1]);
  aList.Free;
end;
begin
  if not ReadLimit then
    PrintUsage;
  try
    PrintPrimes(Sieve(Limit));
  except
    on e: Exception do
      WriteLn('An exception ', e.ClassName, ' occurred with message: ', e.Message);
  end;
end.

```

===Alternative segmented(odds only) version===
function OddSegmentSieve returns a list of primes less than or equal to the given aLimit

```pascal

program prime_sieve;
{$mode objfpc}{$coperators on}
uses
  SysUtils, Math;
type
  TPrimeList = array of DWord;
function OddSegmentSieve(aLimit: DWord): TPrimeList;
  function EstimatePrimeCount(aLimit: DWord): DWord;
  begin
    case aLimit of
      0..1:   Result := 0;
      2..200: Result := Trunc(1.6 * aLimit/Ln(aLimit)) + 1;
    else
      Result := Trunc(aLimit/(Ln(aLimit) - 2)) + 1;
    end;
  end;
  function Sieve(aLimit: DWord; aNeed2: Boolean): TPrimeList;
  var
    IsPrime: array of Boolean;
    I: DWord = 3;
    J, SqrtBound: DWord;
    Count: Integer = 0;
  begin
    if aLimit < 2 then
      exit(nil);
    SetLength(IsPrime, (aLimit - 1) div 2);
    FillChar(Pointer(IsPrime)^, Length(IsPrime), Byte(True));
    SetLength(Result, EstimatePrimeCount(aLimit));
    SqrtBound := Trunc(Sqrt(aLimit));
    if aNeed2 then
      begin
        Result[0] := 2;
        Inc(Count);
      end;
    for I := 0 to High(IsPrime) do
      if IsPrime[I] then
        begin
          Result[Count] := I * 2 + 3;
          if Result[Count] <= SqrtBound then
            begin
              J := Result[Count] * Result[Count];
              repeat
                IsPrime[(J - 3) div 2] := False;
                J += Result[Count] * 2;
              until J > aLimit;
            end;
          Inc(Count);
        end;
    SetLength(Result, Count);
  end;
const
  PAGE_SIZE = $8000;
var
  IsPrime: array[0..Pred(PAGE_SIZE)] of Boolean; //current page
  SmallPrimes: TPrimeList = nil;
  I: QWord;
  J, PageHigh, Prime: DWord;
  Count: Integer;
begin
  if aLimit < PAGE_SIZE div 4 then
    exit(Sieve(aLimit, True));
  I := Trunc(Sqrt(aLimit));
  SmallPrimes := Sieve(I + 1, False);
  Count := Length(SmallPrimes) + 1;
  I += Ord(not Odd(I));
  SetLength(Result, EstimatePrimeCount(aLimit));
  while I <= aLimit do
    begin
      PageHigh := Min(Pred(PAGE_SIZE * 2), aLimit - I);
      FillChar(IsPrime, PageHigh div 2 + 1, Byte(True));
      for Prime in SmallPrimes do
        begin
          J := DWord(I) mod Prime;
          if J <> 0 then
            J := Prime shl (1 - J and 1) - J;
          while J <= PageHigh do
            begin
              IsPrime[J div 2] := False;
              J += Prime * 2;
            end;
        end;
      for J := 0 to PageHigh div 2 do
        if IsPrime[J] then
          begin
            Result[Count] := J * 2 + I;
            Inc(Count);
          end;
      I += PAGE_SIZE * 2;
    end;
  SetLength(Result, Count);
  Result[0] := 2;
  Move(SmallPrimes[0], Result[1], Length(SmallPrimes) * SizeOf(DWord));
end;

  //usage

var
  Limit: DWord = 0;
function ReadLimit: Boolean;
var
  Lim: Int64;
begin
  if (ParamCount = 1) and Lim.TryParse(ParamStr(1), Lim) then
    if (Lim >= 0) and (Lim <= High(DWord)) then
      begin
        Limit := DWord(Lim);
        exit(True);
      end;
  Result := False;
end;
procedure PrintUsage;
begin
  WriteLn('Usage: prime_sieve3 Limit');
  WriteLn('  where Limit in the range [2, ', High(DWord), ']');
  Halt;
end;
procedure PrintPrimes(const aList: TPrimeList);
var
  I: DWord;
begin
  for I := 0 to Length(aList) - 2 do
    Write(aList[I], ', ');
  WriteLn(aList[High(aList)]);
end;
begin
  if not ReadLimit then
    PrintUsage;
  PrintPrimes(OddSegmentSieve(Limit));
end.

```



## Frink


```frink

n = eval[input["Enter highest number: "]]
results = array[sieve[n]]
println[results]
println[length[results] + " prime numbers less than or equal to " + n]

sieve[n] :=
{
   // Initialize array
   array = array[0 to n]
   array@1 = 0

   for i = 2 to ceil[sqrt[n]]
      if array@i != 0
         for j = i^2 to n step i
            array@j = 0

   return select[array, { |x| x != 0 }]
}

```



## FutureBasic


### Basic sieve of array of booleans


```futurebasic

include "ConsoleWindow"

begin globals
dim dynamic gPrimes(1) as Boolean
end globals

local fn SieveOfEratosthenes( n as long )
dim as long i, j

for i = 2 to  n
  for j = i * i to n step i
    gPrimes(j) = _true
  next
  if gPrimes(i) = 0 then print i;
next i
kill gPrimes
end fn

fn SieveOfEratosthenes( 100 )

```

Output:

```txt

 2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97

```



## GAP


```gap
Eratosthenes := function(n)
    local a, i, j;
    a := ListWithIdenticalEntries(n, true);
    if n < 2 then
        return [];
    else
        for i in [2 .. n] do
            if a[i] then
                j := i*i;
                if j > n then
                    return Filtered([2 .. n], i -> a[i]);
                else
                    while j <= n do
                        a[j] := false;
                        j := j + i;
                    od;
                fi;
            fi;
        od;
    fi;
end;

Eratosthenes(100);

[ 2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97 ]
```



## GLBasic


```GLBasic
// Sieve of Eratosthenes (find primes)
// GLBasic implementation


GLOBAL n%, k%, limit%, flags%[]

limit = 100			// search primes up to this number

DIM flags[limit+1]		// GLBasic arrays start at 0

FOR n = 2 TO SQR(limit)
    IF flags[n] = 0
        FOR k = n*n TO limit STEP n
            flags[k] = 1
        NEXT
    ENDIF
NEXT

// Display the primes
FOR n = 2 TO limit
    IF flags[n] = 0 THEN STDOUT n + ", "
NEXT

KEYWAIT

```



## Go


### Basic sieve of array of booleans


```go
package main
import "fmt"

func main() {
    const limit = 201 // means sieve numbers < 201

    // sieve
    c := make([]bool, limit) // c for composite.  false means prime candidate
    c[1] = true              // 1 not considered prime
    p := 2
    for {
        // first allowed optimization:  outer loop only goes to sqrt(limit)
        p2 := p * p
        if p2 >= limit {
            break
        }
        // second allowed optimization:  inner loop starts at sqr(p)
        for i := p2; i < limit; i += p {
            c[i] = true // it's a composite

        }
        // scan to get next prime for outer loop
        for {
            p++
            if !c[p] {
                break
            }
        }
    }

    // sieve complete.  now print a representation.
    for n := 1; n < limit; n++ {
        if c[n] {
            fmt.Print("  .")
        } else {
            fmt.Printf("%3d", n)
        }
        if n%20 == 0 {
            fmt.Println("")
        }
    }
}
```

Output:

```txt

  .  2  3  .  5  .  7  .  .  . 11  . 13  .  .  . 17  . 19  .
  .  . 23  .  .  .  .  . 29  . 31  .  .  .  .  . 37  .  .  .
 41  . 43  .  .  . 47  .  .  .  .  . 53  .  .  .  .  . 59  .
 61  .  .  .  .  . 67  .  .  . 71  . 73  .  .  .  .  . 79  .
  .  . 83  .  .  .  .  . 89  .  .  .  .  .  .  . 97  .  .  .
101  .103  .  .  .107  .109  .  .  .113  .  .  .  .  .  .  .
  .  .  .  .  .  .127  .  .  .131  .  .  .  .  .137  .139  .
  .  .  .  .  .  .  .  .149  .151  .  .  .  .  .157  .  .  .
  .  .163  .  .  .167  .  .  .  .  .173  .  .  .  .  .179  .
181  .  .  .  .  .  .  .  .  .191  .193  .  .  .197  .199  .

```


===Odds-only bit-packed array output-enumerating version===

The above version's output is rather specialized; the following version uses a closure function to enumerate over the culled composite number array, which is bit packed.  By using this scheme for output, no extra memory is required above that required for the culling array:


```go
package main

import (
	"fmt"
	"math"
)

func primesOdds(top uint) func() uint {
	topndx := int((top - 3) / 2)
	topsqrtndx := (int(math.Sqrt(float64(top))) - 3) / 2
	cmpsts := make([]uint, (topndx/32)+1)
	for i := 0; i <= topsqrtndx; i++ {
		if cmpsts[i>>5]&(uint(1)<<(uint(i)&0x1F)) == 0 {
			p := (i << 1) + 3
			for j := (p*p - 3) >> 1; j <= topndx; j += p {
				cmpsts[j>>5] |= 1 << (uint(j) & 0x1F)
			}
		}
	}
	i := -1
	return func() uint {
		oi := i
		if i <= topndx {
			i++
		}
		for i <= topndx && cmpsts[i>>5]&(1<<(uint(i)&0x1F)) != 0 {
			i++
		}
		if oi < 0 {
			return 2
		} else {
			return (uint(oi) << 1) + 3
		}
	}
}

func main() {
	iter := primesOdds(100)
	for v := iter(); v <= 100; v = iter() {
		print(v, " ")
	}
	iter = primesOdds(1000000)
	count := 0
	for v := iter(); v <= 1000000; v = iter() {
		count++
	}
	fmt.Printf("\r\n%v\r\n", count)
}
```

{{output}}

```txt
2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97
78498
```



### Sieve Tree

A fairly odd sieve tree method:

```go
package main
import "fmt"

type xint uint64
type xgen func()(xint)

func primes() func()(xint) {
	pp, psq := make([]xint, 0), xint(25)

	var sieve func(xint, xint)xgen
	sieve = func(p, n xint) xgen {
		m, next := xint(0), xgen(nil)
		return func()(r xint) {
			if next == nil {
				r = n
				if r <= psq {
					n += p
					return
				}

				next = sieve(pp[0] * 2, psq) // chain in
				pp = pp[1:]
				psq = pp[0] * pp[0]

				m = next()
			}
			switch {
			case n < m: r, n = n, n + p
			case n > m: r, m = m, next()
			default:    r, n, m = n, n + p, next()
			}
			return
		}
	}

	f := sieve(6, 9)
	n, p := f(), xint(0)

	return func()(xint) {
		switch {
		case p < 2: p = 2
		case p < 3: p = 3
		default:
			for p += 2; p == n; {
				p += 2
				if p > n {
					n = f()
				}
			}
			pp = append(pp, p)
		}
		return p
	}
}

func main() {
	for i, p := 0, primes(); i < 100000; i++ {
		fmt.Println(p())
	}
}
```


===Concurrent Daisy-chain sieve===
A concurrent prime sieve adopted from the example in the "Go Playground" window at http://golang.org/

```go
package main
import "fmt"

// Send the sequence 2, 3, 4, ... to channel 'out'
func Generate(out chan<- int) {
	for i := 2; ; i++ {
		out <- i                  // Send 'i' to channel 'out'
	}
}

// Copy the values from 'in' channel to 'out' channel,
//   removing the multiples of 'prime' by counting.
// 'in' is assumed to send increasing numbers
func Filter(in <-chan int, out chan<- int, prime int) {
        m := prime + prime                // first multiple of prime
	for {
		i := <- in                // Receive value from 'in'
		for i > m {
			m = m + prime     // next multiple of prime
			}
		if i < m {
			out <- i          // Send 'i' to 'out'
			}
	}
}

// The prime sieve: Daisy-chain Filter processes
func Sieve(out chan<- int) {
	gen := make(chan int)             // Create a new channel
	go Generate(gen)                  // Launch Generate goroutine
	for  {
		prime := <- gen
		out <- prime
		ft := make(chan int)
		go Filter(gen, ft, prime)
		gen = ft
	}
}

func main() {
	sv := make(chan int)              // Create a new channel
	go Sieve(sv)                      // Launch Sieve goroutine
	for i := 0; i < 1000; i++ {
		prime := <- sv
		if i >= 990 {
		    fmt.Printf("%4d ", prime)
		    if (i+1)%20==0 {
			fmt.Println("")
		    }
		}
	}
}
```

The output:

```txt

7841 7853 7867 7873 7877 7879 7883 7901 7907 7919

```


[http://ideone.com/ixhHNO Runs at ~ n^2.1] empirically, producing up to n=3000 primes in under 5 seconds.

===Postponed Concurrent Daisy-chain sieve===
Here we postpone the ''creation'' of filters until the prime's square is seen in the input, to radically reduce the amount of filter channels in the sieve chain.

```go
package main
import "fmt"

// Send the sequence 2, 3, 4, ... to channel 'out'
func Generate(out chan<- int) {
	for i := 2; ; i++ {
		out <- i                  // Send 'i' to channel 'out'
	}
}

// Copy the values from 'in' channel to 'out' channel,
//   removing the multiples of 'prime' by counting.
// 'in' is assumed to send increasing numbers
func Filter(in <-chan int, out chan<- int, prime int) {
        m := prime * prime                // start from square of prime
	for {
		i := <- in                // Receive value from 'in'
		for i > m {
			m = m + prime     // next multiple of prime
			}
		if i < m {
			out <- i          // Send 'i' to 'out'
			}
	}
}

// The prime sieve: Postponed-creation Daisy-chain of Filters
func Sieve(out chan<- int) {
	gen := make(chan int)             // Create a new channel
	go Generate(gen)                  // Launch Generate goroutine
	p := <- gen
	out <- p
	p = <- gen          // make recursion shallower ---->
	out <- p            // (Go channels are _push_, not _pull_)

	base_primes := make(chan int)     // separate primes supply
	go Sieve(base_primes)
	bp := <- base_primes              // 2           <---- here
	bq := bp * bp                     // 4

	for  {
		p = <- gen
		if p == bq {                    // square of a base prime
			ft := make(chan int)
			go Filter(gen, ft, bp)  // filter multiples of bp in gen out
			gen = ft
			bp = <- base_primes     // 3
			bq = bp * bp            // 9
		} else {
			out <- p
		}
	}
}

func main() {
	sv := make(chan int)              // Create a new channel
	go Sieve(sv)                      // Launch Sieve goroutine
	lim := 25000
	for i := 0; i < lim; i++ {
		prime := <- sv
		if i >= (lim-10) {
		    fmt.Printf("%4d ", prime)
		    if (i+1)%20==0 {
			fmt.Println("")
		    }
		}
	}
}
```


The output:

```txt

286999 287003 287047 287057 287059 287087 287093 287099 287107 287117

```


[http://ideone.com/I0AXf5 Runs at ~ n^1.2] empirically, producing up to n=25,000 primes on ideone in under 5 seconds.
===Incremental Odds-only Sieve===
Uses Go's built-in hash tables to store odd composites, and defers adding new known composites until the square is seen.

```go

package main

import "fmt"

func main() {
    primes := make(chan int)
    go PrimeSieve(primes)

    p := <-primes
    for p < 100 {
        fmt.Printf("%d ", p)
        p = <-primes
    }

    fmt.Println()
}

func PrimeSieve(out chan int) {
    out <- 2
    out <- 3

    primes := make(chan int)
    go PrimeSieve(primes)

    var p int
    p = <-primes
    p = <-primes

    sieve := make(map[int]int)
    q := p * p
    n := p

    for {
        n += 2
        step, isComposite := sieve[n]
        if isComposite {
            delete(sieve, n)
            m := n + step
            for sieve[m] != 0 {
                m += step
            }
            sieve[m] = step

        } else if n < q {
            out <- n

        } else {
            step = p + p
            m := n + step
            for sieve[m] != 0 {
                m += step
            }
            sieve[m] = step
            p = <-primes
            q = p * p
        }
    }
}

```

The output:

```txt

2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97

```


## Groovy

This solution uses a BitSet for compactness and speed, but in [[Groovy]], BitSet has full List semantics. It also uses both the "square root of the boundary" shortcut and the "square of the prime" shortcut.

```groovy
def sievePrimes = { bound ->
    def isPrime  = new BitSet(bound)
    isPrime[0..1] = false
    isPrime[2..bound] = true
    (2..(Math.sqrt(bound))).each { pc ->
        if (isPrime[pc]) {
            ((pc**2)..bound).step(pc) { isPrime[it] = false }
        }
    }
    (0..bound).findAll { isPrime[it] }
}
```


Test:

```groovy
println sievePrimes(100)
```


Output:

```txt
[2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97]
```


=={{header|GW-BASIC}}==


```qbasic
10  INPUT "ENTER NUMBER TO SEARCH TO: ";LIMIT
20  DIM FLAGS(LIMIT)
30  FOR N = 2 TO SQR (LIMIT)
40  IF FLAGS(N) < > 0 GOTO 80
50  FOR K = N * N TO LIMIT STEP N
60  FLAGS(K) = 1
70  NEXT K
80  NEXT N
90  REM  DISPLAY THE PRIMES
100  FOR N = 2 TO LIMIT
110  IF FLAGS(N) = 0 THEN PRINT N;", ";
120  NEXT N
```



## Haskell


===Mutable unboxed arrays, odds only===
Mutable array of unboxed <code>Bool</code>s indexed by <code>Int</code>s, representing odds only:


```haskell
import Control.Monad (forM_, when)
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed

sieveUO :: Int -> UArray Int Bool
sieveUO top = runSTUArray $ do
    let m = (top-1) `div` 2
        r = floor . sqrt $ fromIntegral top + 1
    sieve <- newArray (1,m) True          -- :: ST s (STUArray s Int Bool)
    forM_ [1..r `div` 2] $ \i -> do       -- prime(i) = 2i+1
      isPrime <- readArray sieve i        -- ((2i+1)^2-1)`div`2 = 2i(i+1)
      when isPrime $ do
        forM_ [2*i*(i+1), 2*i*(i+2)+1..m] $ \j -> do
          writeArray sieve j False
    return sieve

primesToUO :: Int -> [Int]
primesToUO top | top > 1   = 2 : [2*i + 1 | (i,True) <- assocs $ sieveUO top]
               | otherwise = []
```


This represents ''odds only'' in the array. [http://ideone.com/KwZNc Empirical orders of growth] is ~ <i>n<sup>1.2</sup></i> in ''n'' primes produced, and improving for bigger ''n''&zwj;&thinsp;&zwj;s. Memory consumption is low (array seems to be packed) and growing about linearly with ''n''. Can further be [http://ideone.com/j24jxV significantly sped up] by  re-writing the <code>forM_</code> loops with direct recursion, and using <code>unsafeRead</code> and <code>unsafeWrite</code> operations.


### Immutable arrays

Monolithic sieving array. ''Even'' numbers above 2 are pre-marked as composite, and sieving is done only by ''odd'' multiples of ''odd'' primes:

```haskell
import Data.Array.Unboxed

primesToA m = sieve 3 (array (3,m) [(i,odd i) | i<-[3..m]] :: UArray Int Bool)
  where
    sieve p a
      | p*p > m   = 2 : [i | (i,True) <- assocs a]
      | a!p       = sieve (p+2) $ a//[(i,False) | i <- [p*p, p*p+2*p..m]]
      | otherwise = sieve (p+2) a
```


Its performance sharply depends on compiler optimizations. Compiled with -O2 flag in the presence of the explicit type signature, it is very fast in  producing first few million primes. <code>(//)</code> is an array update operator.

===Immutable arrays, by segments===

Works by segments between consecutive primes' squares. Should be the fastest of non-monadic code. ''Evens'' are entirely ignored:

```haskell
import Data.Array.Unboxed

primesSA = 2 : prs ()
  where
    prs () = 3 : sieve 3 [] (prs ())
    sieve x fs (p:ps) = [i*2 + x | (i,True) <- assocs a]
                        ++ sieve (p*p) fs2 ps
     where
      q     = (p*p-x)`div`2
      fs2   = (p,0) : [(s, rem (y-q) s) | (s,y) <- fs]
      a     :: UArray Int Bool
      a     = accumArray (\ b c -> False) True (1,q-1)
                         [(i,()) | (s,y) <- fs, i <- [y+s, y+s+s..q]]
```


===Basic list-based sieve===
Straightforward implementation of the sieve of Eratosthenes in its original bounded form. This finds primes in gaps between the composites, and composites as an enumeration of each prime's multiples.

```haskell
primesTo m = eratos [2..m] where
   eratos (p : xs)
      | p*p > m   = p : xs
      | otherwise = p : eratos (xs `minus` [p*p, p*p+p..m])
                                    -- map (p*) [p..]
                                    -- map (p*) (p:xs)   -- (Euler's sieve)

minus a@(x:xs) b@(y:ys) = case compare x y of
         LT -> x : minus  xs b
         EQ ->     minus  xs ys
         GT ->     minus  a  ys
minus a        b        = a
```

Its time complexity is similar to that of optimal [[Primality_by_trial_division#Haskell|trial division]] because of limitations of Haskell linked lists, where <code>(minus a b)</code> takes time proportional to <code>length(union a b)</code> and not <code>(length b)</code>, as achieved in imperative setting with direct-access memory. Uses ordered list representation of sets.

This is reasonably useful up to ranges of fifteen million or about the first million primes.


### Unbounded list based sieve

Unbounded, "naive", too eager to subtract (see above for the definition of <code>minus</code>):

```haskell
primesE  = sieve [2..]
           where
           sieve (p:xs) = p : sieve (minus xs [p, p+p..])
-- unfoldr (\(p:xs)-> Just (p, minus xs [p, p+p..])) [2..]
```

This is slow, with complexity increasing as a square law or worse so that it is only moderately useful for the first few thousand primes or so.

The number of active streams can be limited to what's strictly necessary by postponement until the square of a prime is seen, getting a massive complexity improvement to better than <i>~ n<sup>1.5</sup></i> so it can get first million primes or so in a tolerable time:

```haskell
primesPE = 2 : sieve [3..] 4 primesPE
               where
               sieve (x:xs) q (p:t)
                 | x < q     = x : sieve xs q (p:t)
                 | otherwise =     sieve (minus xs [q, q+p..]) (head t^2) t
-- fix $ (2:) . concat
--     . unfoldr (\(p:ps,xs)-> Just . second ((ps,) . (`minus` [p*p, p*p+p..]))
--                                  . span (< p*p) $ xs) . (,[3..])
```


Transposing the workflow, going by segments between the consecutive squares of primes:

```haskell
import Data.List (inits)

primesSE = 2 : sieve 3 4 (tail primesSE) (inits primesSE)
               where
               sieve x q ps (fs:ft) =
                  foldl minus [x..q-1] [[n, n+f..q-1] | f <- fs, let n=div x f * f]
                          -- [i|(i,True) <- assocs ( accumArray (\ b c -> False)
                          --     True (x,q-1) [(i,()) | f <- fs, let n=div(x+f-1)f*f,
                          --         i <- [n, n+f..q-1]] :: UArray Int Bool )]
                  ++ sieve q (head ps^2) (tail ps) ft
```


The basic gradually-deepening left-leaning <code>(((a-b)-c)- ... )</code> workflow of <code>foldl minus a bs</code> above can be rearranged into the right-leaning <code>(a-(b+(c+ ... )))</code> workflow of <code>minus a (foldr union [] bs)</code>. This is the idea behind Richard Bird's unbounded code presented in [http://www.cs.hmc.edu/~oneill/papers/Sieve-JFP.pdf M. O'Neill's article], equivalent to:


```haskell
primesB = _Y ( (2:) . minus [3..] . foldr (\p-> (p*p :) . union [p*p+p, p*p+2*p..]) [] )

--      = _Y ( (2:) . minus [3..] . _LU . map(\p-> [p*p, p*p+p..]) )
-- _LU ((x:xs):t) = x : (union xs . _LU) t             -- linear folding big union

_Y g = g (_Y g)  -- = g (g (g ( ... )))      non-sharing multistage fixpoint combinator
--                  = g . g . g . ...            ... = g^inf
--   = let x = g x in g x -- = g (fix g)     two-stage fixpoint combinator
--   = let x = g x in x   -- = fix g         sharing fixpoint combinator

union a@(x:xs) b@(y:ys) = case compare x y of
         LT -> x : union  xs b
         EQ -> x : union  xs ys
         GT -> y : union  a  ys
```


Using <code>_Y</code> is meant to guarantee the separate supply of primes to be independently calculated, recursively, instead of the same one being reused, corecursively; thus the memory footprint is drastically reduced. This idea was introduced by M. ONeill as a double-staged production, with a separate primes feed.

The above code is also useful to a range of the first million primes or so. The code can be further optimized by fusing <code>minus [3..]</code> into one function, preventing a space leak with the newer GHC versions, getting the function <code>gaps</code> defined below.

====Tree-merging incremental sieve====
Linear merging structure can further be replaced with an wiki.haskell.org/Prime_numbers#Tree_merging indefinitely deepening to the right tree-like structure, <code>(a-(b+((c+d)+( ((e+f)+(g+h)) +  ... ))))</code>.

This merges primes' multiples streams in a ''tree''-like fashion, as a sequence of balanced trees of <code>union</code> nodes, likely achieving theoretical time complexity only a ''log n'' factor above the optimal ''n log n log (log n)'', for ''n'' primes produced. Indeed, empirically it runs at about ''~ n<sup>1.2</sup>'' (for producing first few million primes), similarly to priority-queue&ndash;based version of M. O'Neill's, and with very low  space complexity too (not counting the produced sequence of course):

```haskell
primes :: [Int]
primes = 2 : _Y ( (3:) . gaps 5 . _U . map(\p-> [p*p, p*p+2*p..]) )

gaps k s@(c:cs) | k < c     = k : gaps (k+2) s      -- ~= ([k,k+2..] \\ s)
                | otherwise =     gaps (k+2) cs     --   when null(s\\[k,k+2..])

_U ((x:xs):t) = x : (union xs . _U . pairs) t       -- tree-shaped folding big union
  where                                             --  ~= nub . sort . concat
    pairs (xs:ys:t) = union xs ys : pairs t
```


Works with odds only, the simplest kind of wheel. Here's the [http://ideone.com/qpnqe test entry] on Ideone.com, and a [http://ideone.com/p0e81 comparison with more versions].


### =With Wheel=

Using <code>_U</code> defined above,

```haskell
primesW :: [Int]
primesW = [2,3,5,7] ++ _Y ( (11:) . gapsW 13 (tail wheel) . _U .
                            map (\p->
                              map (p*) . dropWhile (< p) $
                                scanl (+) (p - rem (p-11) 210) wheel) )

gapsW k (d:w) s@(c:cs) | k < c     = k : gapsW (k+d) w s    -- set difference
                       | otherwise =     gapsW (k+d) w cs   --   k==c

wheel = 2:4:2:4:6:2:6:4:2:4:6:6:2:6:4:2:6:4:6:8:4:2:4:2:    -- gaps = (`gapsW` cycle [2])
        4:8:6:4:6:2:4:6:2:6:6:4:2:4:6:2:6:4:2:4:2:10:2:10:wheel
  -- cycle $ zipWith (-) =<< tail $ [i | i <- [11..221], gcd i 210 == 1]
```


Used [[Emirp_primes#List-based|here]] and [[Extensible_prime_generator#List_based|here]].


### Priority Queue based incremental sieve


The above work is derived from the Epilogue of the Melissa E. O'Neill paper which is much referenced with respect to incremental functional sieves; however, that paper is now dated and her comments comparing list based sieves to her original work leading up to a Priority Queue based implementation is no longer current given more recent work such as the above Tree Merging version.  Accordingly, a modern "odd's-only" Priority Queue version is developed here for more current comparisons between the above list based incremental sieves and a continuation of O'Neill's work.

In order to implement a Priority Queue version with Haskell, an efficient Priority Queue, which is not part of the standard Haskell libraries is required.  A Min Heap implementation is likely best suited for this task in providing the most efficient frequently used peeks of the next item in the queue and replacement of the first item in the queue (not using a "pop" followed by a "push) with "pop" operations then not used at all, and "push" operations used relatively infrequently.  Judging by O'Neill's use of an efficient "deleteMinAndInsert" operation which she states "(We provide deleteMinAndInsert becausea heap-based implementation can support this operation with considerably less rearrangement than a deleteMin followed by an insert.)", which statement is true for a Min Heap Priority Queue and not others, and her reference to a priority queue by (Paulson, 1996), the queue she used is likely the one as provided as a simple true functional [http://rosettacode.org/wiki/Priority_queue#Haskell Min Heap implementation on RosettaCode], from which the essential functions are reproduced here:

```haskell
data PriorityQ k v = Mt
                     | Br !k v !(PriorityQ k v) !(PriorityQ k v)
  deriving (Eq, Ord, Read, Show)

emptyPQ :: PriorityQ k v
emptyPQ = Mt

peekMinPQ :: PriorityQ k v -> Maybe (k, v)
peekMinPQ Mt           = Nothing
peekMinPQ (Br k v _ _) = Just (k, v)

pushPQ :: Ord k => k -> v -> PriorityQ k v -> PriorityQ k v
pushPQ wk wv Mt           = Br wk wv Mt Mt
pushPQ wk wv (Br vk vv pl pr)
             | wk <= vk   = Br wk wv (pushPQ vk vv pr) pl
             | otherwise  = Br vk vv (pushPQ wk wv pr) pl

siftdown :: Ord k => k -> v -> PriorityQ k v -> PriorityQ k v -> PriorityQ k v
siftdown wk wv Mt _          = Br wk wv Mt Mt
siftdown wk wv (pl @ (Br vk vv _ _)) Mt
    | wk <= vk               = Br wk wv pl Mt
    | otherwise              = Br vk vv (Br wk wv Mt Mt) Mt
siftdown wk wv (pl @ (Br vkl vvl pll plr)) (pr @ (Br vkr vvr prl prr))
    | wk <= vkl && wk <= vkr = Br wk wv pl pr
    | vkl <= vkr             = Br vkl vvl (siftdown wk wv pll plr) pr
    | otherwise              = Br vkr vvr pl (siftdown wk wv prl prr)

replaceMinPQ :: Ord k => k -> v -> PriorityQ k v -> PriorityQ k v
replaceMinPQ wk wv Mt             = Mt
replaceMinPQ wk wv (Br _ _ pl pr) = siftdown wk wv pl pr
```


The "peekMin" function retrieves both of the key and value in a tuple so processing is required to access whichever is required for further processing.  As well, the output of the peekMin function is a Maybe with the case of an empty queue providing a Nothing output.

The following code is O'Neill's original odds-only code (without wheel factorization) from her paper slightly adjusted as per the requirements of this Min Heap implementation as laid out above; note the `seq` adjustments to the "adjust" function to make the evaluation of the entry tuple more strict for better efficiency:

```haskell
-- (c) 2006-2007 Melissa O'Neill.  Code may be used freely so long as
-- this copyright message is retained and changed versions of the file
-- are clearly marked.
--   the only changes are the names of the called PQ functions and the
--   included processing for the result of the peek function being a maybe tuple.

primesPQ() = 2 : sieve [3,5..]
  where
    sieve [] = []
    sieve (x:xs) = x : sieve' xs (insertprime x xs emptyPQ)
      where
        insertprime p xs table = pushPQ (p*p) (map (* p) xs) table
        sieve' [] table = []
        sieve' (x:xs) table
            | nextComposite <= x = sieve' xs (adjust table)
            | otherwise = x : sieve' xs (insertprime x xs table)
          where
            nextComposite = case peekMinPQ table of
                              Just (c, _) -> c
            adjust table
                | n <= x = adjust (replaceMinPQ n' ns table)
                | otherwise = table
              where (n, n':ns) = case peekMinPQ table of
                                   Just tpl -> tpl
```


The above code is almost four times slower than the version of the Tree Merging sieve above for the first million primes although it is about the same speed as the original Richard Bird sieve with the "odds-only" adaptation as above.  It is slow and uses a huge amount of memory for primarily one reason:  over eagerness in adding prime composite streams to the queue, which are added as the primes are listed rather than when they are required as the output primes stream reaches the square of a given base prime; this over eagerness also means that the processed numbers must have a large range in order to not overflow when squared (as in the default Integer = infinite precision integers as used here and by O'Neill, but Int64's or Word64's would give a practical range) which processing of wide range numbers adds processing and memory requirement overhead.  Although O'Neill's code is elegant, it also loses some efficiency due to the extensive use of lazy list processing, not all of which is required even for a wheel factorization implementation.

The following code is adjusted to reduce the amount of lazy list processing and to add a secondary base primes stream (or a succession of streams when the combinator is used) so as to overcome the above problems and reduce memory consumption to only that required for the primes below the square root of the currently sieved number; using this means that 32-bit Int's are sufficient for a reasonable range and memory requirements become relatively negligible:

```haskell
primesPQx :: () -> [Int]
primesPQx() = 2 : _Y ((3 :) . sieve 5 emptyPQ 9) -- initBasePrms
  where
    _Y g = g (_Y g)        -- non-sharing multi-stage fixpoint combinator OR
--  initBasePrms = 3 : sieve 5 emptyPQ 9 initBasePrms -- single stage
    insertprime p table = let adv = 2 * p in let nv = p * p + adv in
                          nv `seq` pushPQ nv adv table
    sieve n table q bps@(bp:bps')
        | n >= q = let nbp = head bps' in
                   sieve (n + 2) (insertprime bp table) (nbp * nbp) bps'
        | n >= nextComposite = sieve (n + 2) (adjust table) q bps
        | otherwise = n : sieve (n + 2) table q bps
      where
        nextComposite = case peekMinPQ table of
                          Nothing -> q -- at beginning when queue empty
                          Just (c, _) -> c
        adjust table
            | c <= n = let nc = c + adv in
                       nc `seq` adjust (replaceMinPQ nc adv table)
            | otherwise = table
          where (c, adv) = case peekMinPQ table of
                             Just ct -> ct
```


The above code is over five times faster than the previous (O'Neill) Priority Queue code and about half again faster than the Tree Merging code for a range of a million primes, and will always be faster as the Min Heap is slightly more efficient than Tree Merging due to better tree balancing.

All of these codes including the list based ones would enjoy about the same constant factor improvement of up to about four times the speed with the application of maximum wheel factorization.


### Page Segmented Sieve using a mutable unboxed array


All of the above unbounded sieves are quite limited in practical sieving range due to the large constant factor overheads in computation, making them mostly just interesting intellectual exercises other than for small ranges of about the first million to ten million primes; the following '''"odds-only''' page-segmented version using (bit-packed internally) mutable unboxed arrays is about 50 times faster than the fastest of the above algorithms for ranges of about that and higher, making it practical for the first several hundred million primes:

```haskell
import Data.Bits
import Data.Array.Base
import Control.Monad.ST
import Data.Array.ST (runSTUArray, STUArray(..))

type PrimeType = Int
szPGBTS = (2^14) * 8 :: PrimeType -- CPU L1 cache in bits

primesPaged :: () -> [PrimeType]
primesPaged() = 2 : _Y (listPagePrms . pagesFrom 0) where
  _Y g = g (_Y g)        -- non-sharing multi-stage fixpoint combinator
  listPagePrms (hdpg @ (UArray lowi _ rng _) : tlpgs) =
    let loop i = if i >= rng then listPagePrms tlpgs
                 else if unsafeAt hdpg i then loop (i + 1)
                      else let ii = lowi + fromIntegral i in
                           case 3 + ii + ii of
                             p -> p `seq` p : loop (i + 1) in loop 0
  makePg lowi bps = runSTUArray $ do
    let limi = lowi + szPGBTS - 1
    let nxt = 3 + limi + limi -- last candidate in range
    cmpsts <- newArray (lowi, limi) False
    let pbts = fromIntegral szPGBTS
    let cull (p:ps) =
          let sqr = p * p in
          if sqr > nxt then return cmpsts
          else let pi = fromIntegral p in
               let cullp c = if c > pbts then return ()
                             else do
                               unsafeWrite cmpsts c True
                               cullp (c + pi) in
               let a = (sqr - 3) `shiftR` 1 in
               let s = if a >= lowi then fromIntegral (a - lowi)
                       else let r = fromIntegral ((lowi - a) `rem` p) in
                            if r == 0 then 0 else pi - r in
               do { cullp s; cull ps}
    if lowi == 0 then do
      pg0 <- unsafeFreezeSTUArray cmpsts
      cull $ listPagePrms [pg0]
    else cull bps
  pagesFrom lowi bps =
    let cf lwi = case makePg lwi bps of
          pg -> pg `seq` pg : cf (lwi + szPGBTS) in cf lowi
```


The above code is currently implemented to use "Int" as the prime type but one can change the "PrimeType" to "Int64" (importing Data.Int) or "Word64" (importing Data.Word) to extend the range to its maximum practical range of above 10^14 or so.  Note that for larger ranges that one will want to set the "szPGBTS" to something close to the CPU L2 or even L3 cache size (up to 8 Megabytes = 2^23 for an Intel i7) for a slight cost in speed (about a factor of 1.5) but so that it still computes fairly efficiently as to memory access up to those large ranges.  It would be quite easy to modify the above code to make the page array size automatically increase in size with increasing range.

The above code takes only a few tens of milliseconds to compute the first million primes and a few seconds to calculate the first 50 million primes, with over half of those times expended in just enumerating the result lazy list, with even worse times when using 64-bit list processing (especially with 32-bit versions of GHC).  A further improvement to reduce the computational cost of repeated list processing across the base pages for every page segment would be to store the required base primes (or base prime gaps) in an array that gets extended in size by factors of two (by copying the old array to the new extended array) as the number of base primes increases; in that way the scans across base primes per page segment would just be array accesses which are much faster than list enumeration.

Unlike many other other unbounded examples, this algorithm has the true Sieve of Eratosthenes computational time complexity of O(n log log n) where n is the sieving range with no extra "log n" factor while having a very low computational time cost per composite number cull of less than ten CPU clock cycles per cull (well under as in under 4 clock cycles for the Intel i7 using a page buffer size of the CPU L1 cache size).

There are other ways to make the algorithm faster including high degrees of wheel factorization, which can reduce the number of composite culling operations by a factor of about four for practical ranges, and multi-processing which can reduce the computation time proportionally to the number of available independent CPU cores, but there is little point to these optimizations as long as the lazy list enumeration is the bottleneck as it is starting to be in the above code.  To take advantage of those optimizations, functions need to be provided that can compute the desired results without using list processing.

For ranges above about 10^14 where culling spans begin to exceed even an expanded size page array, other techniques need to be adapted such as the use of a "bucket sieve" which tracks the next page that larger base prime culling sequences will "hit" to avoid redundant (and time expensive) start address calculations for base primes that don't "hit" the current page.

However, even with the above code and its limitations for large sieving ranges, the speeds will never come close to as slow as the other "incremental" sieve algorithms, as the time will never exceed about 100 CPU clock cycles per composite number cull, where the fastest of those other algorithms takes many hundreds of CPU clock cycles per cull.

===APL-style===
Rolling set subtraction over the rolling element-wise addition on integers. Basic, slow, worse than quadratic in the number of primes produced, empirically:

```haskell
zipWith (flip (!!)) [0..]    -- or: take n . last . take n ...
     . scanl1 minus
     . scanl1 (zipWith (+)) $ repeat [2..]
```

Or, a wee bit faster:

```haskell
unfoldr (\(a:b:t) -> Just . (head &&& (:t) . (`minus` b)
                                           . tail) $ a)
     . scanl1 (zipWith (+)) $ repeat [2..]
```

A bit optimized, much faster, with better complexity,

```haskell
tail . concat
     . unfoldr (\(a:b:t) -> Just . second ((:t) . (`minus` b))
                                 . span (< head b) $ a)
     . scanl1 (zipWith (+) . tail) $ tails [1..]
  -- $ [ [n*n, n*n+n..] | n <- [1..] ]
```


getting nearer to the functional equivalent of the <code>primesPE</code> above, i.e.

```haskell
fix ( (2:) . concat
      . unfoldr (\(a:b:t) -> Just . second ((:t) . (`minus` b))
                                  . span (< head b) $ a)
      . ([3..] :) . map (\p-> [p*p, p*p+p..]) )
```


An illustration:

```haskell>
 mapM_ (print . take 15) $ take 10 $ scanl1 (zipWith(+)) $ repeat [2..]
[  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16]
[  4,  6,  8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32]
[  6,  9, 12, 15, 18, 21, 24, 27, 30, 33, 36, 39, 42, 45, 48]
[  8, 12, 16, 20, 24, 28, 32, 36, 40, 44, 48, 52, 56, 60, 64]
[ 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80]
[ 12, 18, 24, 30, 36, 42, 48, 54, 60, 66, 72, 78, 84, 90, 96]
[ 14, 21, 28, 35, 42, 49, 56, 63, 70, 77, 84, 91, 98,105,112]
[ 16, 24, 32, 40, 48, 56, 64, 72, 80, 88, 96,104,112,120,128]
[ 18, 27, 36, 45, 54, 63, 72, 81, 90, 99,108,117,126,135,144]
[ 20, 30, 40, 50, 60, 70, 80, 90,100,110,120,130,140,150,160]

> mapM_ (print . take 15) $ take 10 $ scanl1 (zipWith(+) . tail) $ tails [1..]
[  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15]
[  4,  6,  8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32]
[  9, 12, 15, 18, 21, 24, 27, 30, 33, 36, 39, 42, 45, 48, 51]
[ 16, 20, 24, 28, 32, 36, 40, 44, 48, 52, 56, 60, 64, 68, 72]
[ 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95]
[ 36, 42, 48, 54, 60, 66, 72, 78, 84, 90, 96,102,108,114,120]
[ 49, 56, 63, 70, 77, 84, 91, 98,105,112,119,126,133,140,147]
[ 64, 72, 80, 88, 96,104,112,120,128,136,144,152,160,168,176]
[ 81, 90, 99,108,117,126,135,144,153,162,171,180,189,198,207]
[100,110,120,130,140,150,160,170,180,190,200,210,220,230,240]
```



## HicEst


```hicest
REAL :: N=100,  sieve(N)

sieve = $ > 1     ! = 0 1 1 1 1 ...
DO i = 1, N^0.5
  IF( sieve(i) ) THEN
     DO j = i^2, N, i
       sieve(j) = 0
     ENDDO
  ENDIF
ENDDO

DO i = 1, N
  IF( sieve(i) ) WRITE() i
ENDDO
```


=={{header|Icon}} and {{header|Unicon}}==

```Icon
 procedure main()
    sieve(100)
 end

 procedure sieve(n)
    local p,i,j
    p:=list(n, 1)
    every i:=2 to sqrt(n) & j:= i+i to n by i & p[i] == 1
      do p[j] := 0
    every write(i:=2 to n & p[i] == 1 & i)
 end
```


Alternatively using sets

```Icon
 procedure main()
     sieve(100)
 end

 procedure sieve(n)
     primes := set()
     every insert(primes,1 to n)
     every member(primes,i := 2 to n) do
         every delete(primes,i + i to n by i)
     delete(primes,1)
     every write(!sort(primes))
end
```



## J

{{eff note|J|i.&.(p:inv) }}

This problem is a classic example of how J can be used to represent mathematical concepts.

J uses x|y ([http://www.jsoftware.com/help/dictionary/d230.htm residue]) to represent the operation of finding the remainder during integer division of y divided by x


```J
   10|13
3
```


And x|/y gives us a [http://www.jsoftware.com/help/dictionary/d420.htm table] with all possibilities from x and all possibilities from y.


```J
   2 3 4 |/ 2 3 4
0 1 0
2 0 1
2 3 0
```


Meanwhile, |/~y ([http://www.jsoftware.com/help/dictionary/d220v.htm reflex]) copies the right argument and uses it as the left argment.


```J
   |/~ 0 1 2 3 4
0 1 2 3 4
0 0 0 0 0
0 1 0 1 0
0 1 2 0 1
0 1 2 3 0
```


(Bigger examples might make the patterns more obvious but they also take up more space.)

By the way, we can ask J to count out the first N integers for us using i. ([http://www.jsoftware.com/help/dictionary/didot.htm integers]):


```J
   i. 5
0 1 2 3 4
```


Anyways, the 0s in that last table represent the Sieve of Eratosthenes (in a symbolic or mathematical sense), and we can use = ([http://www.jsoftware.com/help/dictionary/d000.htm equal]) to find them.


```J
   0=|/~ i.5
1 0 0 0 0
1 1 1 1 1
1 0 1 0 1
1 0 0 1 0
1 0 0 0 1
```


Now all we need to do is add them up, using / ([http://www.jsoftware.com/help/dictionary/d420.htm insert]) in its single argument role to insert + between each row of that last table.


```J
   +/0=|/~ i.5
5 1 2 2 3
```


The sieve wants the cases where we have two divisors:


```J
   2=+/0=|/~ i.5
0 0 1 1 0
```


And we just want to know the positions of the 1s in that list, which we can find using I. ([http://www.jsoftware.com/help/dictionary/dicapdot.htm indices]):


```J
   I.2=+/0=|/~ i.5
2 3
   I.2=+/0=|/~ i.100
2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97
```


And we might want to express this sentence as a definition of a word that lets us use it with an arbitrary argument. There are a variety of ways of doing this. For example:


```J
sieve0=: verb def 'I.2=+/0=|/~ i.y'
```


That said, this fails with an argument of 2 (instead of giving an empty list of the primes smaller than 2, it gives a list with one element: 0). Working through why this is and why this matters can be an informative exercise. But, assuming this matters, we need to add some guard logic to prevent that problem:


```J
sieve0a=: verb def 'I.(y>2)*2=+/0=|/~ i.y'
```


Of course, we can also express this in an even more elaborate fashion. The elaboration makes more efficient use of resources for large arguments, at the expense of less efficient use of resources for small arguments:


```J
sieve1=: 3 : 0
  m=. <.%:y
  z=. $0
  b=. y{.1
  while. m>:j=. 1+b i. 0 do.
   b=. b+.y$(-j){.1
   z=. z,j
  end.
  z,1+I.-.b
 )
```


"Wheels" may be implemented as follows:


```J
sieve2=: 3 : 0
 m=. <.%:y
 z=. y (>:#]) 2 3 5 7
 b=. 1,}.y$+./(*/z)$&>(-z){.&.>1
 while. m>:j=. 1+b i. 0 do.
  b=. b+.y$(-j){.1
  z=. z,j
 end.
 z,1+I.-.b
)
```


The use of<tt> 2 3 5 7 </tt>as wheels provides a
20% time improvement for<tt> n=1000 </tt>and 2% for<tt> n=1e6</tt> but note that sieve2 is still 25 times slower than i.&.(p:inv) for <tt>n=1e6</tt>. Then again, the value of the sieve of eratosthenes was not efficiency but simplicity. So perhaps we should ignore resource consumption issues and instead focus on intermediate results for reasonably sized example problems?


```J
   0=|/~ i.8
1 0 0 0 0 0 0 0
1 1 1 1 1 1 1 1
1 0 1 0 1 0 1 0
1 0 0 1 0 0 1 0
1 0 0 0 1 0 0 0
1 0 0 0 0 1 0 0
1 0 0 0 0 0 1 0
1 0 0 0 0 0 0 1
```


Columns with two "1" values correspond to prime numbers.

'''Alternate Implementation'''

If you feel that the intermediate results, above, are not enough "sieve-like" another approach could be:


```J
sieve=:verb define
  seq=: 2+i.y-1  NB. 2 thru y
  n=. 2
  l=. #seq
  whilst. -.seq-:prev do.
     prev=. seq
     mask=. l{.1-(0{.~n-1),1}.l$n{.1
     seq=. seq * mask
     n=. {.((n-1)}.seq)-.0
  end.
  seq -. 0
)
```


Example use:


```J
   sieve 100
2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97
```


To see intermediate results, let's show them:


```J
label=:dyad def 'echo x,":y'

sieve=:verb define
  'seq  ' label seq=: 2+i.y-1  NB. 2 thru y
  'n    ' label n=. 2
  'l    ' label l=. #seq
  whilst. -.seq-:prev do.
     prev=. seq
     'mask   ' label mask=. l{.1-(0{.~n-1),1}.l$n{.1
     'seq    ' label seq=. seq * mask
     'n      ' label n=. {.((n-1)}.seq)-.0
  end.
  seq -. 0
)

seq  2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60
n    2
l    59
mask   1 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0
seq    2 3 0 5 0 7 0 9 0 11 0 13 0 15 0 17 0 19 0 21 0 23 0 25 0 27 0 29 0 31 0 33 0 35 0 37 0 39 0 41 0 43 0 45 0 47 0 49 0 51 0 53 0 55 0 57 0 59 0
n      3
mask   1 1 1 1 0 1 1 0 1 1 0 1 1 0 1 1 0 1 1 0 1 1 0 1 1 0 1 1 0 1 1 0 1 1 0 1 1 0 1 1 0 1 1 0 1 1 0 1 1 0 1 1 0 1 1 0 1 1 0
seq    2 3 0 5 0 7 0 0 0 11 0 13 0 0 0 17 0 19 0 0 0 23 0 25 0 0 0 29 0 31 0 0 0 35 0 37 0 0 0 41 0 43 0 0 0 47 0 49 0 0 0 53 0 55 0 0 0 59 0
n      5
mask   1 1 1 1 1 1 1 1 0 1 1 1 1 0 1 1 1 1 0 1 1 1 1 0 1 1 1 1 0 1 1 1 1 0 1 1 1 1 0 1 1 1 1 0 1 1 1 1 0 1 1 1 1 0 1 1 1 1 0
seq    2 3 0 5 0 7 0 0 0 11 0 13 0 0 0 17 0 19 0 0 0 23 0 0 0 0 0 29 0 31 0 0 0 0 0 37 0 0 0 41 0 43 0 0 0 47 0 49 0 0 0 53 0 0 0 0 0 59 0
n      7
mask   1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 0 1 1 1 1 1 1 0 1 1 1 1 1 1 0 1 1 1 1 1 1 0 1 1 1 1 1 1 0 1 1 1 1 1 1 0 1 1 1 1
seq    2 3 0 5 0 7 0 0 0 11 0 13 0 0 0 17 0 19 0 0 0 23 0 0 0 0 0 29 0 31 0 0 0 0 0 37 0 0 0 41 0 43 0 0 0 47 0 0 0 0 0 53 0 0 0 0 0 59 0
n      11
mask   1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1
seq    2 3 0 5 0 7 0 0 0 11 0 13 0 0 0 17 0 19 0 0 0 23 0 0 0 0 0 29 0 31 0 0 0 0 0 37 0 0 0 41 0 43 0 0 0 47 0 0 0 0 0 53 0 0 0 0 0 59 0
n      13
2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59
```


Another variation on this theme would be:


```J
sieve=:verb define
  seq=: 2+i.y-1  NB. 2 thru y
  n=. 1
  l=. #seq
  whilst. -.seq-:prev do.
     prev=. seq
     n=. 1+n+1 i.~ * (n-1)}.seq
     inds=. (2*n)+n*i.(<.l%n)-1
     seq=. 0 inds} seq
  end.
  seq -. 0
)
```


Intermediate results for this variant are left as an exercise for the reader


## Java

{{works with|Java|1.5+}}

```java5
import java.util.LinkedList;

public class Sieve{
       public static LinkedList<Integer> sieve(int n){
               if(n < 2) return new LinkedList<Integer>();
               LinkedList<Integer> primes = new LinkedList<Integer>();
               LinkedList<Integer> nums = new LinkedList<Integer>();

               for(int i = 2;i <= n;i++){ //unoptimized
                       nums.add(i);
               }

               while(nums.size() > 0){
                       int nextPrime = nums.remove();
                       for(int i = nextPrime * nextPrime;i <= n;i += nextPrime){
                               nums.removeFirstOccurrence(i);
                       }
                       primes.add(nextPrime);
               }
               return primes;
       }
}
```


To optimize by testing only odd numbers, replace the loop marked "unoptimized" with these lines:

```java5
nums.add(2);
for(int i = 3;i <= n;i += 2){
       nums.add(i);
}
```


Version using a BitSet:

```java5
import java.util.LinkedList;
import java.util.BitSet;

public class Sieve{
    public static LinkedList<Integer> sieve(int n){
        LinkedList<Integer> primes = new LinkedList<Integer>();
        BitSet nonPrimes = new BitSet(n+1);

        for (int p = 2; p <= n ; p = nonPrimes.nextClearBit(p+1)) {
            for (int i = p * p; i <= n; i += p)
                nonPrimes.set(i);
            primes.add(p);
        }
        return primes;
    }
}
```



### Infinite iterator

An iterator that will generate primes indefinitely (perhaps until it runs out of memory), but very slowly.
{{trans|Python}}
{{works with|Java|1.5+}}

```java5
import java.util.Iterator;
import java.util.PriorityQueue;
import java.math.BigInteger;

// generates all prime numbers
public class InfiniteSieve implements Iterator<BigInteger> {

    private static class NonPrimeSequence implements Comparable<NonPrimeSequence> {
	BigInteger currentMultiple;
	BigInteger prime;

	public NonPrimeSequence(BigInteger p) {
	    prime = p;
	    currentMultiple = p.multiply(p); // start at square of prime
	}
	@Override public int compareTo(NonPrimeSequence other) {
	    // sorted by value of current multiple
	    return currentMultiple.compareTo(other.currentMultiple);
	}
    }

    private BigInteger i = BigInteger.valueOf(2);
    // priority queue of the sequences of non-primes
    // the priority queue allows us to get the "next" non-prime quickly
    final PriorityQueue<NonPrimeSequence> nonprimes = new PriorityQueue<NonPrimeSequence>();

    @Override public boolean hasNext() { return true; }
    @Override public BigInteger next() {
	// skip non-prime numbers
	for ( ; !nonprimes.isEmpty() && i.equals(nonprimes.peek().currentMultiple); i = i.add(BigInteger.ONE)) {
            // for each sequence that generates this number,
            // have it go to the next number (simply add the prime)
            // and re-position it in the priority queue
	    while (nonprimes.peek().currentMultiple.equals(i)) {
		NonPrimeSequence x = nonprimes.poll();
		x.currentMultiple = x.currentMultiple.add(x.prime);
		nonprimes.offer(x);
	    }
	}
	// prime
        // insert a NonPrimeSequence object into the priority queue
	nonprimes.offer(new NonPrimeSequence(i));
	BigInteger result = i;
	i = i.add(BigInteger.ONE);
	return result;
    }

    public static void main(String[] args) {
	Iterator<BigInteger> sieve = new InfiniteSieve();
	for (int i = 0; i < 25; i++) {
	    System.out.println(sieve.next());
	}
    }
}
```

{{out}}

```txt

2
3
5
7
11
13
17
19

```

<!--This code has a very limited range using the int type for reasons given in the [http://rosettacode.org/wiki/Talk:Sieve_of_Eratosthenes#Python_translated_to_Java Discussion Page]-->

===Infinite iterator with a faster algorithm (sieves odds-only)===
The adding of each discovered prime's incremental step information to the mapping should be postponed until the candidate number reaches the primes square, as it is useless before that point. This drastically reduces the space complexity from O(n/log(n)) to O(sqrt(n/log(n))), in n primes produced, and also lowers the run time complexity due to the use of the hash table based HashMap, which is much more efficient for large ranges.

{{trans|Python}}
{{works with|Java|1.5+}}

```java5
import java.util.Iterator;
import java.util.HashMap;

// generates all prime numbers up to about 10 ^ 19 if one can wait 1000's of years or so...
public class SoEInfHashMap implements Iterator<Long> {

  long candidate = 2;
  Iterator<Long> baseprimes = null;
  long basep = 3;
  long basepsqr = 9;
  // HashMap of the sequences of non-primes
  // the hash map allows us to get the "next" non-prime reasonably quickly
  // but further allows re-insertions to take amortized constant time
  final HashMap<Long,Long> nonprimes = new HashMap<>();

  @Override public boolean hasNext() { return true; }
  @Override public Long next() {
    // do the initial primes separately to initialize the base primes sequence
    if (this.candidate <= 5L) if (this.candidate++ == 2L) return 2L; else {
      this.candidate++; if (this.candidate == 5L) return 3L; else {
        this.baseprimes = new SoEInfHashMap();
        this.baseprimes.next(); this.baseprimes.next(); // throw away 2 and 3
        return 5L;
    } }
    // skip non-prime numbers including squares of next base prime
    for ( ; this.candidate >= this.basepsqr || //equals nextbase squared => not prime
              nonprimes.containsKey(this.candidate); candidate += 2) {
      // insert a square root prime sequence into hash map if to limit
      if (candidate >= basepsqr) { // if square of base prime, always equal
        long adv = this.basep << 1;
        nonprimes.put(this.basep * this.basep + adv, adv);
        this.basep = this.baseprimes.next();
        this.basepsqr = this.basep * this.basep;
      }
      // else for each sequence that generates this number,
      // have it go to the next number (simply add the advance)
      // and re-position it in the hash map at an emply slot
      else {
        long adv = nonprimes.remove(this.candidate);
        long nxt = this.candidate + adv;
        while (this.nonprimes.containsKey(nxt)) nxt += adv; //unique keys
        this.nonprimes.put(nxt, adv);
      }
    }
    // prime
    long tmp = candidate; this.candidate += 2; return tmp;
  }

  public static void main(String[] args) {
    int n = 100000000;
    long strt = System.currentTimeMillis();
    SoEInfHashMap sieve = new SoEInfHashMap();
    int count = 0;
    while (sieve.next() <= n) count++;
    long elpsd = System.currentTimeMillis() - strt;
    System.out.println("Found " + count + " primes up to " + n + " in " + elpsd + " milliseconds.");
  }

}
```


{{out}}
```txt
Found 5761455 primes up to 100000000 in 4297 milliseconds.
```


===Infinite iterator with a very fast page segmentation algorithm (sieves odds-only)===

Although somewhat faster than the previous infinite iterator version, the above code is still over 10 times slower than an infinite iterator based on array paged segmentation as in the following code, where the time to enumerate/iterate over the found primes (common to all the iterators) is now about half of the total execution time:

{{trans|JavaScript}}
{{works with|Java|1.5+}}

```java5
import java.util.Iterator;
import java.util.ArrayList;

// generates all prime numbers up to about 10 ^ 19 if one can wait 100's of years or so...
// practical range is about 10^14 in a week or so...
public class SoEPagedOdds implements Iterator<Long> {
  private final int BFSZ = 1 << 16;
  private final int BFBTS = BFSZ * 32;
  private final int BFRNG = BFBTS * 2;
  private long bi = -1;
  private long lowi = 0;
  private final ArrayList<Integer> bpa = new ArrayList<>();
  private Iterator<Long> bps;
  private final int[] buf = new int[BFSZ];

  @Override public boolean hasNext() { return true; }
  @Override public Long next() {
    if (this.bi < 1) {
      if (this.bi < 0) {
        this.bi = 0;
        return 2L;
      }
      //this.bi muxt be 0
      long nxt = 3 + (this.lowi << 1) + BFRNG;
      if (this.lowi <= 0) { // special culling for first page as no base primes yet:
          for (int i = 0, p = 3, sqr = 9; sqr < nxt; i++, p += 2, sqr = p * p)
              if ((this.buf[i >>> 5] & (1 << (i & 31))) == 0)
                  for (int j = (sqr - 3) >> 1; j < BFBTS; j += p)
                      this.buf[j >>> 5] |= 1 << (j & 31);
      }
      else { // after the first page:
        for (int i = 0; i < this.buf.length; i++)
          this.buf[i] = 0; // clear the sieve buffer
        if (this.bpa.isEmpty()) { // if this is the first page after the zero one:
            this.bps = new SoEPagedOdds(); // initialize separate base primes stream:
            this.bps.next(); // advance past the only even prime of two
            this.bpa.add(this.bps.next().intValue()); // get the next prime (3 in this case)
        }
        // get enough base primes for the page range...
        for (long p = this.bpa.get(this.bpa.size() - 1), sqr = p * p; sqr < nxt;
                p = this.bps.next(), this.bpa.add((int)p), sqr = p * p) ;
        for (int i = 0; i < this.bpa.size() - 1; i++) {
          long p = this.bpa.get(i);
          long s = (p * p - 3) >>> 1;
          if (s >= this.lowi) // adjust start index based on page lower limit...
            s -= this.lowi;
          else {
            long r = (this.lowi - s) % p;
            s = (r != 0) ? p - r : 0;
          }
          for (int j = (int)s; j < BFBTS; j += p)
            this.buf[j >>> 5] |= 1 << (j & 31);
        }
      }
    }
    while ((this.bi < BFBTS) &&
           ((this.buf[(int)this.bi >>> 5] & (1 << ((int)this.bi & 31))) != 0))
        this.bi++; // find next marker still with prime status
    if (this.bi < BFBTS) // within buffer: output computed prime
        return 3 + ((this.lowi + this.bi++) << 1);
    else { // beyond buffer range: advance buffer
        this.bi = 0;
        this.lowi += BFBTS;
        return this.next(); // and recursively loop
    }
  }

  public static void main(String[] args) {
    long n = 1000000000;
    long strt = System.currentTimeMillis();
    Iterator<Long> gen = new SoEPagedOdds();
    int count = 0;
    while (gen.next() <= n) count++;
    long elpsd = System.currentTimeMillis() - strt;
    System.out.println("Found " + count + " primes up to " + n + " in " + elpsd + " milliseconds.");
  }

}
```


{{out}}
```txt
Found 50847534 primes up to 1000000000 in 3201 milliseconds.
```



## JavaScript


```javascript
function eratosthenes(limit) {
    var primes = [];
    if (limit >= 2) {
        var sqrtlmt = Math.sqrt(limit) - 2;
        var nums = new Array(); // start with an empty Array...
        for (var i = 2; i <= limit; i++) // and
            nums.push(i); // only initialize the Array once...
        for (var i = 0; i <= sqrtlmt; i++) {
            var p = nums[i]
            if (p)
                for (var j = p * p - 2; j < nums.length; j += p)
                    nums[j] = 0;
        }
        for (var i = 0; i < nums.length; i++) {
            var p = nums[i];
            if (p)
                primes.push(p);
        }
    }
    return primes;
}

var primes = eratosthenes(100);

if (typeof print == "undefined")
    print = (typeof WScript != "undefined") ? WScript.Echo : alert;
print(primes);
```

outputs:

```txt
2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97
```


Substituting the following code for the function for '''an odds-only algorithm using bit packing''' for the array produces code that is many times faster than the above:


```javascript
function eratosthenes(limit) {
    var prms = [];
    if (limit >= 2) prms = [2];
    if (limit >= 3) {
        var sqrtlmt = (Math.sqrt(limit) - 3) >> 1;
        var lmt = (limit - 3) >> 1;
        var bfsz = (lmt >> 5) + 1
        var buf = [];
        for (var i = 0; i < bfsz; i++)
            buf.push(0);
        for (var i = 0; i <= sqrtlmt; i++)
            if ((buf[i >> 5] & (1 << (i & 31))) == 0) {
                var p = i + i + 3;
                for (var j = (p * p - 3) >> 1; j <= lmt; j += p)
                    buf[j >> 5] |= 1 << (j & 31);
            }
        for (var i = 0; i <= lmt; i++)
            if ((buf[i >> 5] & (1 << (i & 31))) == 0)
                prms.push(i + i + 3);
    }
    return prms;
}
```


While the above code is quite fast especially using an efficient JavaScript engine such as Google Chrome's V8, it isn't as elegant as it could be using the features of the new EcmaScript6 specification when it comes out about the end of 2014 and when JavaScript engines including those of browsers implement that standard in that we might choose to implement an incremental algorithm iterators or generators similar to as implemented in Python or F# (yield).  Meanwhile, we can emulate some of those features by using a simulation of an iterator class (which is easier than using a call-back function) for an '''"infinite" generator based on an Object dictionary''' as in the following odds-only code written as a JavaScript class:


```javascript
var SoEIncClass = (function () {
    function SoEIncClass() {
        this.n = 0;
    }
    SoEIncClass.prototype.next = function () {
        this.n += 2;
        if (this.n < 7) { // initialization of sequence to avoid runaway:
            if (this.n < 3) { // only even of two:
                this.n = 1; // odds from here...
                return 2;
            }
            if (this.n < 5)
                return 3;
            this.dict = {}; // n must be 5...
            this.bps = new SoEIncClass(); // new source of base primes
            this.bps.next(); // advance past the even prime of two...
            this.p = this.bps.next(); // first odd prime (3 in this case)
            this.q = this.p * this.p; // set guard
            return 5;
        } else { // past initialization:
            var s = this.dict[this.n]; // may or may not be defined...
            if (!s) { // not defined:
                if (this.n < this.q) // haven't reached the guard:
                    return this.n; // found a prime
                else { // n === q => not prime but at guard, so:
                    var p2 = this.p << 1; // the span odds-only is twice prime
                    this.dict[this.n + p2] = p2; // add next composite of prime to dict
                    this.p = this.bps.next();
                    this.q = this.p * this.p; // get next base prime guard
                    return this.next(); // not prime so advance...
                }
            } else { // is a found composite of previous base prime => not prime
                delete this.dict[this.n]; // advance to next composite of this prime:
                var nxt = this.n + s;
                while (this.dict[nxt]) nxt += s; // find unique empty slot in dict
                this.dict[nxt] = s; // to put the next composite for this base prime
                return this.next(); // not prime so advance...
            }
        }
    };
    return SoEIncClass;
})();
```


The above code can be used to find the nth prime (which would require estimating the required range limit using the previous fixed range code) by using the following code:


```javascript
var gen = new SoEIncClass();
for (var i = 1; i < 1000000; i++, gen.next());
var prime = gen.next();

if (typeof print == "undefined")
    print = (typeof WScript != "undefined") ? WScript.Echo : alert;
print(prime);
```


to produce the following output (about five seconds using Google Chrome's V8 JavaScript engine):


```txt
15485863
```


The above code is considerably slower than the fixed range code due to the multiple method calls and the use of an object as a dictionary, which (using a hash table as its basis for most implementations) will have about a constant O(1) amortized time per operation but has quite a high constant overhead to convert the numeric indices to strings which are then hashed to be used as table keys for the look-up operations as compared to doing this more directly in implementations such as the Python dict with Python's built-in hashing functions for every supported type.

This can be implemented as '''an "infinite" odds-only generator using page segmentation''' for a considerable speed-up with the alternate JavaScript class code as follows:


```javascript
var SoEPgClass = (function () {
    function SoEPgClass() {
        this.bi = -1; // constructor resets the enumeration to start...
    }
    SoEPgClass.prototype.next = function () {
        if (this.bi < 1) {
            if (this.bi < 0) {
                this.bi++;
                this.lowi = 0; // other initialization done here...
                this.bpa = [];
                return 2;
            } else { // bi must be zero:
                var nxt = 3 + (this.lowi << 1) + 262144;
                this.buf = new Array();
                for (var i = 0; i < 4096; i++) // faster initialization:
                    this.buf.push(0);
                if (this.lowi <= 0) { // special culling for first page as no base primes yet:
                    for (var i = 0, p = 3, sqr = 9; sqr < nxt; i++, p += 2, sqr = p * p)
                        if ((this.buf[i >> 5] & (1 << (i & 31))) === 0)
                            for (var j = (sqr - 3) >> 1; j < 131072; j += p)
                                this.buf[j >> 5] |= 1 << (j & 31);
                } else { // after the first page:
                    if (!this.bpa.length) { // if this is the first page after the zero one:
                        this.bps = new SoEPgClass(); // initialize separate base primes stream:
                        this.bps.next(); // advance past the only even prime of two
                        this.bpa.push(this.bps.next()); // get the next prime (3 in this case)
                    }
                    // get enough base primes for the page range...
                    for (var p = this.bpa[this.bpa.length - 1], sqr = p * p; sqr < nxt;
                            p = this.bps.next(), this.bpa.push(p), sqr = p * p) ;
                    for (var i = 0; i < this.bpa.length; i++) {
                        var p = this.bpa[i];
                        var s = (p * p - 3) >> 1;
                        if (s >= this.lowi) // adjust start index based on page lower limit...
                            s -= this.lowi;
                        else {
                            var r = (this.lowi - s) % p;
                            s = (r != 0) ? p - r : 0;
                        }
                        for (var j = s; j < 131072; j += p)
                            this.buf[j >> 5] |= 1 << (j & 31);
                    }
                }
            }
        }
        while (this.bi < 131072 && this.buf[this.bi >> 5] & (1 << (this.bi & 31)))
            this.bi++; // find next marker still with prime status
        if (this.bi < 131072) // within buffer: output computed prime
            return 3 + ((this.lowi + this.bi++) << 1);
        else { // beyond buffer range: advance buffer
            this.bi = 0;
            this.lowi += 131072;
            return this.next(); // and recursively loop
        }
    };
    return SoEPgClass;
})();
```


The above code is about fifty times faster (about five seconds to calculate 50 million primes to about a billion on the Google Chrome V8 JavaScript engine) than the above dictionary based code.

The speed for both of these "infinite" solutions will also respond to further wheel factorization techniques, especially for the dictionary based version where any added overhead to deal with the factorization wheel will be negligible compared to the dictionary overhead.  The dictionary version would likely speed up about a factor of three or a little more with maximum wheel factorization applied; the page segmented version probably won't gain more than a factor of two and perhaps less due to the overheads of array look-up operations.


## JOVIAL


```JOVIAL

START
FILE MYOUTPUT ... $ ''Insufficient information to complete this declaration''
PROC SIEVEE $
    '' define the sieve data structure ''
    ARRAY CANDIDATES 1000 B $
    FOR I =0,1,999 $
    BEGIN
        '' everything is potentially prime until proven otherwise ''
        CANDIDATES($I$) = 1$
    END
    '' Neither 1 nor 0 is prime, so flag them off ''
    CANDIDATES($0$) = 0$
    CANDIDATES($1$) = 0$
    '' start the sieve with the integer 0 ''
    FOR I = 0$
    BEGIN
        IF I GE 1000$
        GOTO DONE$
        '' advance to the next un-crossed out number. ''
        '' this number must be a prime ''
NEXTI.  IF I LS 1000 AND Candidates($I$) EQ 0 $
        BEGIN
            I = I + 1 $
            GOTO NEXTI $
        END
        '' insure against running off the end of the data structure ''
        IF I LT 1000 $
        BEGIN
            '' cross out all multiples of the prime, starting with 2*p. ''
            FOR J=2 $
            FOR K=0 $
            BEGIN
                K = J * I $
                IF K GT 999 $
                GOTO ADV $
                CANDIDATES($K$) = 0 $
                J = J + 1 $
            END
            '' advance to the next candidate ''
ADV.        I = I + 1 $
        END
    END
    '' all uncrossed-out numbers are prime (and only those numbers) ''
    '' print all primes ''
DONE. OPEN OUTPUT MYOUTPUT $
    FOR I=0,1,999$
    BEGIN
        IF CANDIDATES($I$) NQ 0$
        BEGIN
            OUTPUT MYOUTPUT I $
        END
    END
TERM$

```



## jq

{{works with|jq|1.4}}

Short and sweet ...


```jq
# Denoting the input by $n, which is assumed to be a positive integer,
# eratosthenes/0 produces an array of primes less than or equal to $n:
def eratosthenes:

  # erase(i) sets .[i*j] to false for integral j > 1
  def erase(i):
    if .[i] then reduce range(2; (1 + length) / i) as $j (.; .[i * $j] = false)
    else .
    end;

  (. + 1) as $n
  | (($n|sqrt) / 2) as $s
  | [null, null, range(2; $n)]
  | reduce (2, 1 + (2 * range(1; $s))) as $i (.; erase($i))
  | map(select(.));
```

'''Examples''':

```jq
100 | eratosthenes
```

{{out}}

[2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97]

```jq
1e7 | eratosthenes | length
```

{{out}}
664579


## Julia


Started with 2 already in the array, and then test only for odd numbers and push the prime ones onto the array.

```julia
# Returns an array of positive prime numbers less than or equal to lim
function sieve(lim :: Int)
    if lim < 2 return [] end
    limi :: Int = (lim - 1) ÷ 2 # calculate the required array size
    isprime :: Array{Bool} = trues(limi)
    llimi :: Int = (isqrt(lim) - 1) ÷ 2 # and calculate maximum root prime index
    result :: Array{Int} = [2]  #Initial array
    for i in 1:limi
        if isprime[i]
            p = i + i + 1 # 2i + 1
            if i <= llimi
                for j = (p*p-1)>>>1:p:limi # quick shift/divide in case LLVM doesn't optimize divide by 2 away
                    isprime[j] = false
                end
            end
            push!(result, p)
        end
    end
    return result
end
```


Alternate version using <code>findall</code> to get all primes at once in the end


```julia
function sieve(n :: Int)
    isprime = trues(n)
    isprime[1] = false
    for p in 2:n
        if isprime[p]
            j = p * p
            if j > n
                return findall(isprime)
            else
                for k in j:p:n
                  isprime[k] = false
                end
            end
        end
    end
end
```


At about 35 seconds for a range of a billion on my Intel Atom i5-Z8350 CPU at 1.92 GHz (single threaded) or about 70 CPU clock cycles per culling operation, the above examples are two of the very slowest ways to compute the Sieve of Eratosthenes over any kind of a reasonable range due to a couple of factors:
# The output primes are extracted to a result array which takes time (and memory) to construct.
# They use the naive "one huge memory array" method, which has poor memory access speed for larger ranges.


Even though the first uses an odds-only algorithm (not noted in the text as is a requirement of the task) that reduces the number of operations by a factor of about two and a half times, it is not faster than the second, which is not odds-only due to the second being set up to take advantage of the `findall` function to directly output the indices of the remaining true values as the found primes; the second is faster due to the first taking longer to push the found primes singly to the constructed array, whereas internally the second first creates the array to the size of the counted true values and then just fills it.

Also, the first uses more memory than necessary in one byte per `Bool` where using a `BitArray` as in the second reduces this by a factor of eight.

If one is going to "crib" the MatLab algorithm as above, one may as well do it using odds-only as per the MatLab built-in.  The following alternate code improves on the "Alternate" example above by making it sieve odds-only and adjusting the result array contents after to suit:


```julia
function sieve2(n :: Int)
    ni = (n - 1) ÷ 2
    isprime = trues(ni)
    for i in 1:ni
        if isprime[i]
            j = 2i * (i + 1)
            if j > ni
                m = findall(isprime)
                map!((i::Int) -> 2i + 1, m, m)
                return pushfirst!(m, 2)
            else
                p = 2i + 1
                while j <= ni
                  isprime[j] = false
                  j += p
                end
            end
        end
    end
end
```


This takes less about 18.5 seconds or 36 CPU cycles per culling operation to find the primes to a billion, but that is still quite slow compared to what can be done below.  Note that the result array needs to be created then copied, created by the <code>findall</code> function, then modified in place by the <code>map!</code> function to transform the indices to primes, and finally copied by the <code>pushfirst!</code> function to add the only even prime of two to the beginning, but these operations are quire fast.  However, this still consumes a lot of memory, as in about 64 Megabytes for the sieve buffer and over 400 Megabytes for the result (8-byte Int's for 64 bit execution) to sieve to a billion, and culling the huge culling buffer that doesn't fit the CPU cache sizes is what makes it slow.


### Iterator Output


The creation of an output results array is not necessary if the purpose is just to scan across the resulting primes once, they can be output using an iterator (from a `BitArray`) as in the following odds-only code:


```julia
const Prime = UInt64

struct Primes
    rangei :: Int64
    primebits :: BitArray{1}
    function Primes(n :: Int64)
        if n < 3
          if n < 2 return new(-1, falses(0)) # no elements
          else return new((0, trues(0))) end # n = 2: meaning is 1 element of 2
        end
        limi :: Int = (n - 1) ÷ 2 # calculate the required array size
        isprimes :: BitArray = trues(limi)
        @inbounds(
        for i in 1:limi
            p = i + i + 1
            start = (p * p - 1) >>> 1 # shift/divide if LLVM doesn't optimize
            if start > limi
                return new(limi, isprimes)
            end
            if isprimes[i]
                for j in start:p:limi
                  isprimes[j] = false
                end
            end
        end)
    end
end

Base.eltype(::Type{Primes}) = Prime

function Base.length(P::Primes)::Int64
    if P.rangei < 0 return 0 end
    return 1 + count(P.primebits)
end

function Base.iterate(P::Primes, state::Int = 0)::
                                        Union{Tuple{Prime, Int}, Nothing}
    lmt = P.rangei
    if state > lmt return nothing end
    if state <= 0 return (UInt64(2), 1) end
    let
        prmbts = P.primebits
        i = state
        @inbounds(
        while i <= lmt && !prmbts[i] i += 1 end)
        if i > lmt return nothing end
        return (i + i + 1, i + 1)
    end
end
```


for which using the following code:


```julia
function bench()
  @time length(Primes(100)) # warm up JIT
#  println(@time count(x->true, Primes(1000000000))) # about 1.5 seconds slower counting over iteration
  println(@time length(Primes(1000000000)))
end
bench()
```


results in the following output:
{{out}}

```txt
  0.000031 seconds (3 allocations: 160 bytes)
 12.214533 seconds (4 allocations: 59.605 MiB, 0.42% gc time)
50847534
```


This reduces the CPU cycles per culling cycles to about 24.4, but it's still slow due to using the one largish array.  Note that counting each iterated prime takes an additional about one and a half seconds, where if all that is required is the count of primes over a range the specialized length function is much faster.


### Page Segmented Algorithm


For any kind of reasonably large range such as a billion, a page segmented version should be used with the pages sized to the CPU caches for much better memory access times.  As well, the following odds-only example uses a custom bit packing algorithm for a further two times speed-up, also reducing the memory allocation delays by reusing the sieve buffers when possible (usually possible):


```julia
const Prime = UInt64
const BasePrime = UInt32
const BasePrimesArray = Array{BasePrime,1}
const SieveBuffer = Array{UInt8,1}

# contains a lazy list of a secondary base primes arrays feed
# NOT thread safe; needs a Mutex gate to make it so...
abstract type BPAS end # stands in for BasePrimesArrays, not defined yet
mutable struct BasePrimesArrays <: BPAS
    thunk :: Union{Nothing,Function} # problem with efficiency - untyped function!!!!!!!!!
    value :: Union{Nothing,Tuple{BasePrimesArray, BPAS}}
    BasePrimesArrays(thunk::Function) = new(thunk)
end
Base.eltype(::Type{BasePrimesArrays}) = BasePrime
Base.IteratorSize(::Type{BasePrimesArrays}) = Base.SizeUnknown() # "infinite"...
function Base.iterate(BPAs::BasePrimesArrays, state::BasePrimesArrays = BPAs)
    if state.thunk !== nothing
        newvalue :: Union{Nothing,Tuple{BasePrimesArray, BasePrimesArrays}} =
            state.thunk() :: Union{Nothing,Tuple{BasePrimesArray
                                                 , BasePrimesArrays}}
        state.value = newvalue
        state.thunk = nothing
        return newvalue
    end
    state.value
end

# count the number of zero bits (primes) in a byte array,
# also works for part arrays/slices, best used as an `@view`...
function countComposites(cmpsts::AbstractArray{UInt8,1})
    foldl((a, b) -> a + count_zeros(b), cmpsts; init = 0)
end

# converts an entire sieved array of bytes into an array of UInt32 primes,
# to be used as a source of base primes...
function composites2BasePrimesArray(low::Prime, cmpsts::SieveBuffer)
    limiti = length(cmpsts) * 8
    len :: Int = countComposites(cmpsts)
    rslt :: BasePrimesArray = BasePrimesArray(undef, len)
    i :: Int = 0
    j :: Int = 1
    @inbounds(
    while i < limiti
        if cmpsts[i >>> 3 + 1] & (1 << (i & 7)) == 0
            rslt[j] = low + i + i
            j += 1
        end
        i += 1
    end)
    rslt
end

# sieving work done, based on low starting value for the given buffer and
# the given lazy list of base prime arrays...
function sieveComposites(low::Prime, buffer::Array{UInt8,1},
                                     bpas::BasePrimesArrays)
    lowi :: Int = (low - 3) ÷ 2
    len :: Int = length(buffer)
    limiti :: Int = len * 8 - 1
    nexti :: Int = lowi + limiti
    for bpa::BasePrimesArray in bpas
        for bp::BasePrime in bpa
            bpint :: Int = bp
            bpi :: Int = (bpint - 3) >>> 1
            starti :: Int = 2 * bpi * (bpi + 3) + 3
            starti >= nexti && return
            if starti >= lowi starti -= lowi
            else
                r :: Int = (lowi - starti) % bpint
                starti = r == 0 ? 0 : bpint - r
            end
            lmti :: Int = limiti - 40 * bpint
            @inbounds(
            if bpint <= (len >>> 2) starti <= lmti
                for i in 1:8
                    if starti > limiti break end
                    mask = convert(UInt8,1) << (starti & 7)
                    c = starti >>> 3 + 1
                    while c <= len
                        buffer[c] |= mask
                        c += bpint
                    end
                    starti += bpint
                end
            else
                c = starti
                while c <= limiti
                    buffer[c >>> 3 + 1] |= convert(UInt8,1) << (c & 7)
                    c += bpint
                end
            end)
        end
    end
    return
end

# starts the secondary base primes feed with minimum size in bits set to 4K...
# thus, for the first buffer primes up to 8293,
# the seeded primes easily cover it as 97 squared is 9409.
function makeBasePrimesArrays() :: BasePrimesArrays
    cmpsts :: SieveBuffer = Array{UInt8,1}(undef, 512)
    function nextelem(low::Prime, bpas::BasePrimesArrays) ::
                                    Tuple{BasePrimesArray, BasePrimesArrays}
        # calculate size so that the bit span is at least as big as the
        # maximum culling prime required, rounded up to minsizebits blocks...
        reqdsize :: Int = 2 + isqrt(1 + low)
        size :: Int = (reqdsize ÷ 4096 + 1) * 4096 ÷ 8 # size in bytes
        if size > length(cmpsts) cmpsts = Array{UInt8,1}(undef, size) end
        fill!(cmpsts, 0)
        sieveComposites(low, cmpsts, bpas)
        arr :: BasePrimesArray = composites2BasePrimesArray(low, cmpsts)
        next :: Prime = low + length(cmpsts) * 8 * 2
        arr, BasePrimesArrays(() -> nextelem(next, bpas))
    end
    # pre-seeding breaks recursive race,
    # as only known base primes used for first page...
    preseedarr :: BasePrimesArray = # pre-seed to 100, can sieve to 10,000...
        [ 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41
        , 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97
        ]
    nextfunc :: Function = () ->
        (nextelem(convert(Prime,101), makeBasePrimesArrays()))
    firstfunc :: Function = () -> (preseedarr, BasePrimesArrays(nextfunc))
    BasePrimesArrays(firstfunc)
end

# an iterator over successive sieved buffer composite arrays,
# returning a tuple of the value represented by the lowest possible prime
# in the sieved composites array and the array itself;
# the array has a 16 Kilobytes minimum size (CPU L1 cache), but
# will grow so that the bit span is larger than the
# maximum culling base prime required, possibly making it larger than
# the L1 cache for large ranges, but still reasonably efficient using
# the L2 cache: very efficient up to about 16e9 range;
# reasonably efficient to about 2.56e14 for two Megabyte L2 cache = > 1 week...
struct PrimesPages
    baseprimes :: BasePrimesArrays
    PrimesPages() = new(makeBasePrimesArrays())
end
Base.eltype(::Type{PrimesPages}) = SieveBuffer
Base.IteratorSize(::Type{PrimesPages}) = Base.SizeUnknown() # "infinite"...
function Base.iterate(PP::PrimesPages,
                      state :: Tuple{Prime,SieveBuffer} =
                            ( convert(Prime,3), Array{UInt8,1}(undef,16384) ))
    (low, cmpsts) = state
    # calculate size so that the bit span is at least as big as the
    # maximum culling prime required, rounded up to minsizebits blocks...
    reqdsize :: Int = 2 + isqrt(1 + low)
    size :: Int = (reqdsize ÷ 131072 + 1) * 131072 ÷ 8 # size in bytes
    if size > length(cmpsts) cmpsts = Array{UInt8,1}(undef, size) end
    fill!(cmpsts, 0)
    sieveComposites(low, cmpsts, PP.baseprimes)
    newlow :: Prime = low + length(cmpsts) * 8 * 2
    ( low, cmpsts ), ( newlow, cmpsts )
end

function countPrimesTo(range::Prime) :: Int64
    range < 3 && ((range < 2 && return 0) || return 1)
    count :: Int64 = 1
    for ( low, cmpsts ) in PrimesPages() # almost never exits!!!
        if low + length(cmpsts) * 8 * 2 > range
            lasti :: Int = (range - low) ÷ 2
            count += countComposites(@view cmpsts[1:lasti >>> 3])
            count += count_zeros(cmpsts[lasti >>> 3 + 1] |
                                 (0xFE << (lasti & 7)))
            return count
        end
        count += countComposites(cmpsts)
    end
    count
end

# iterator over primes from above page iterator;
# unless doing something special with individual primes, usually unnecessary;
# better to do manipulations based on the composites bit arrays...
# takes at least as long to enumerate the primes as sieve them...
mutable struct PrimesPaged
    primespages :: PrimesPages
    primespageiter :: Tuple{Tuple{Prime,SieveBuffer},Tuple{Prime,SieveBuffer}}
    PrimesPaged() = let PP = PrimesPages(); new(PP, Base.iterate(PP)) end
end
Base.eltype(::Type{PrimesPaged}) = Prime
Base.IteratorSize(::Type{PrimesPaged}) = Base.SizeUnknown() # "infinite"...
function Base.iterate(PP::PrimesPaged, state::Int = -1 )
    state < 0 && return Prime(2), 0
    (low, cmpsts) = PP.primespageiter[1]
    len = length(cmpsts) * 8
    @inbounds(
    while state < len && cmpsts[state >>> 3 + 1] &
                         (UInt8(1) << (state & 7)) != 0
        state += 1
    end)
    if state >= len
        PP.primespageiter = Base.iterate(PP.primespages, PP.primespageiter[2])
        return Base.iterate(PP, 0)
    end
    low + state + state, state + 1
end
```


When tested with the following code:


```julia
function bench()
    print("( ")
    for p in PrimesPaged() p > 100 && break; print(p, " ") end
    println(")")
    countPrimesTo(Prime(100)) # warm up JIT
#=
    println(@time let count = 0
                      for p in PrimesPaged()
                          p > 1000000000 && break
                          count += 1
                      end; count end) # much slower counting over iteration
=#
    println(@time countPrimesTo(Prime(1000000000)))
end
bench()
```


it produces the following:
{{out}}

```txt
( 2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 )
  1.947145 seconds (59 allocations: 39.078 KiB)
50847534
```


Note that "the slow way" as commented out in the code takes an extra about 4.85 seconds to count the primes to a billion, or longer to enumerate the primes than to cull the composites; this makes further work in making this yet faster pointless unless techniques such as the one used here to count the number of found primes by just counting the un-cancelled bit representations in the sieved sieve buffers are used.

This takes about 1.9 seconds to count the primes to a billion (using the fast technique), or about 3.75 clock cycles per culling operation, which is reasonably fast; this is almost 20 times faster the the first naive sieves.  As written, the algorithm maintains its efficiency up to about 16 billion and then slows down as the buffer size increases beyond the CPU L1 cache size into the L2 cache size such that it takes about 436.8 seconds to sieve to 100 billion instead of the expected about 300 seconds; however, an extra feature of "double buffered sieving" could be added so that the buffer is sieved in L1 cache slices followed by a final sweep of the entire buffer by the few remaining cull operations that use the larger primes for only a slight reduction in average cycles per cull up to a range of about 2.56e14 (for this CPU).  For really large ranges above that, another sieving technique known as the "bucket sieve" that sorts the culling operations by page so that processing time is not expended for values that don't "hit" a given page can be used for only a slight additional reduction in efficiency.

Additionally, maximal wheel factorization can reduce the time by about a factor of four, plus multi-processing where the work is shared across the CPU cores can produce a further speed-up by the factor of the number of cores (only three times on this four-core machine due to the clock speed reducing to 75% of the rate when all cores are used), for an additional about 12 times speed-up for this CPU.  These improvements are just slightly too complex to post here.

However, even the version posted shows that the naive "one huge array" implementations should never be used for sieving ranges of over a few million, and that Julia can come very close to the speed of the fastest languages such as C/C++ for the same algorithm.


### Functional Algorithm


One of the best simple purely functional Sieve of Eratosthenes algorithms is the infinite tree folding sequence algorithm as implemented in Haskell.  As Julia does not have a standard LazyList implementation or library and as a full memoizing lazy list is not required for this algorithm, the following odds-only code implements the rudiments of a Co-Inductive Stream (CIS) in its implementation:


```julia
const Thunk = Function # can't define other than as a generalized Function

struct CIS{T}
    head :: T
    tail :: Thunk # produces the next CIS{T}
    CIS{T}(head :: T, tail :: Thunk) where T = new(head, tail)
end
Base.eltype(::Type{CIS{T}}) where T = T
Base.IteratorSize(::Type{CIS{T}}) where T = Base.SizeUnknown()
function Base.iterate(C::CIS{T}, state = C) :: Tuple{T, CIS{T}} where T
    state.head, state.tail()
end

function treefoldingprimes()::CIS{Int}
    function merge(xs::CIS{Int}, ys::CIS{Int})::CIS{Int}
        x = xs.head; y = ys.head
        if x < y CIS{Int}(x, () -> merge(xs.tail(), ys))
        elseif y < x CIS{Int}(y, () -> merge(xs, ys.tail()))
        else CIS{Int}(x, () -> merge(xs.tail(), ys.tail())) end
    end
    function pmultiples(p::Int)::CIS{Int}
        adv :: Int = p + p
        next(c::Int)::CIS{Int} = CIS{Int}(c, () -> next(c + adv)); next(p * p)
    end
    function allmultiples(ps::CIS{Int})::CIS{CIS{Int}}
        CIS{CIS{Int}}(pmultiples(ps.head), () -> allmultiples(ps.tail()))
    end
    function pairs(css :: CIS{CIS{Int}})::CIS{CIS{Int}}
        nextcss = css.tail()
        CIS{CIS{Int}}(merge(css.head, nextcss.head), ()->pairs(nextcss.tail()))
    end
    function composites(css :: CIS{CIS{Int}})::CIS{Int}
        CIS{Int}(css.head.head, ()-> merge(css.head.tail(),
                                            css.tail() |> pairs |> composites))
    end
    function minusat(n::Int, cs::CIS{Int})::CIS{Int}
        if n < cs.head CIS{Int}(n, () -> minusat(n + 2, cs))
        else minusat(n + 2, cs.tail()) end
    end
    oddprimes()::CIS{Int} = CIS{Int}(3, () -> minusat(5, oddprimes()
                                        |> allmultiples |> composites))
    CIS{Int}(2, () -> oddprimes())
end
```


when tested with the following:


```julia
@time let count = 0; for p in treefoldingprimes() p > 1000000 && break; count += 1 end; count end
```


it outputs the following:
{{out}}

```txt
  1.791058 seconds (10.23 M allocations: 290.862 MiB, 3.64% gc time)
78498
```


At about 1.8 seconds or 4000 cycles per culling operation to calculate the number of primes up to a million, this is very slow, but that is not the fault of Julia but rather just that purely functional incremental Sieve of Eratosthenes implementations are much slower than those using mutable arrays and are only useful over quite limited ranges of a few million.  For one thing, incremental algorithms have O(n log n log log n) asymptotic execution complexity rather than O(n log log n) (an extra log n factor) and for another the constant execution overhead is much larger in creating (and garbage collecting) elements in the sequences.

The time for this algorithm is quite comparable to as implemented in other functional languages such as F# and actually faster than implementing the same algorithm in C/C++, but slower than as implemented in purely functional languages such as Haskell or even in only partly functional languages such as Kotlin by a factor of ten or more; this is due to those languages having specialized memory allocation that is very fast at allocating small amounts of memory per allocation as is often a requirement of functional programming.  The majority of the time spent for this algorithm is spent allocating memory, and if future versions of Julia are to be of better use in purely functional programming, improvements need to be made to the memory allocation.

===Infinite (Mutable) Iterator Using (Mutable) Dictionary===

To gain some extra speed above the purely functional algorithm above, the Python'ish version as a mutable iterator embedding a mutable standard base Dictionary can be used.  The following version uses a secondary delayed injection stream of "base" primes defined recursively to provide the successions of composite values in the Dictionary to be used for sieving:


```Julia
const Prime = UInt64
abstract type PrimesDictAbstract end # used for forward reference
mutable struct PrimesDict <: PrimesDictAbstract
    sieve :: Dict{Prime,Prime}
    baseprimes :: PrimesDictAbstract
    lastbaseprime :: Prime
    q :: Prime
    PrimesDict() = new(Dict())
end
Base.eltype(::Type{PrimesDict}) = Prime
Base.IteratorSize(::Type{PrimesDict}) = Base.SizeUnknown() # "infinite"...
function Base.iterate(PD::PrimesDict, state::Prime = Prime(0) )
    if state < 1
        PD.baseprimes = PrimesDict()
        PD.lastbaseprime = Prime(3)
        PD.q = Prime(9)
        return Prime(2), Prime(1)
    end
    dict = PD.sieve
    while true
        state += 2
        if !haskey(dict, state)
            state < PD.q && return state, state
            p = PD.lastbaseprime # now, state = PD.q in all cases
            adv = p + p # since state is at PD.q, advance to next
            dict[state + adv] = adv # adds base prime composite stream
            # following initializes secondary base strea first time
            p <= 3 && Base.iterate(PD.baseprimes)
            p = Base.iterate(PD.baseprimes, p)[1] # next base prime
            PD.lastbaseprime = p
            PD.q = p * p
        else # advance hit composite in dictionary...
            adv = pop!(dict, state)
            next = state + adv
            while haskey(dict, next) next += adv end
            dict[next] = adv # past other composite hits in dictionary
        end
    end
end
```


The above version can be used and tested with similar code as for the functional version, but is about ten times faster at about 400 CPU clock cycles per culling operation, meaning it has a practical range ten times larger although it still has a O(n (log n) (log log n)) asymptotic performance complexity; for larger ranges such as sieving to a billion or more, this is still over a hundred times slower than the page segmented version using a page segmented sieving array.


## Kotlin


```kotlin
fun sieve(limit: Int): List<Int> {
    val primes = mutableListOf<Int>()

    if (limit >= 2) {
        val numbers = Array(limit + 1) { true }
        val sqrtLimit = Math.sqrt(limit.toDouble()).toInt()

        for (factor in 2..sqrtLimit) {
            if (numbers[factor]) {
                for (multiple in (factor * factor)..limit step factor) {
                    numbers[multiple] = false
                }
            }
        }

        numbers.forEachIndexed { number, isPrime ->
            if (number >= 2) {
                if (isPrime) {
                    primes.add(number)
                }
            }
        }
    }

    return primes
}

fun main(args: Array<String>) {
    println(sieve(100))
}
```


{{out}}
[2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97]

'''Alternative much faster odds-only version that outputs an enumeration'''

The above version is quite slow for a lot of reasons:  It includes even number culling even though those will be eliminated on the first pass; It uses a list rather than an array to do the composite culling (both of the above reasons also meaning it takes more memory); It uses enumerations (for..in) to implement loops at a execution time cost per loop.  It also consumes more memory in the final result output as another list.

The following code overcomes most of those problems:  It only culls odd composites; it culls a bit-packed primitive array (also saving memory); It uses tailcall recursive functions for the loops, which are compiled into simple loops.  It also outputs the results as an enumeration, which isn't fast but does not consume any more memory than the culling array.  In this way, the program is only limited in sieving range by the maximum size limit of the culling array, although as it grows larger than the CPU cache sizes, it loses greatly in speed; however, that doesn't matter so much if just enumerating the results.


```kotlin
fun primesOdds(rng: Int): Iterable<Int> {
    val topi = (rng - 3) shr 1
    val lstw = topi shr 5
    val sqrtndx = (Math.sqrt(rng.toDouble()).toInt() - 3) shr 1
    val cmpsts = IntArray(lstw + 1)

    tailrec fun testloop(i: Int) {
        if (i <= sqrtndx) {
            if (cmpsts[i shr 5] and (1 shl (i and 31)) == 0) {
                val p = i + i + 3
                tailrec fun cullp(j: Int) {
                    if (j <= topi) {
                        cmpsts[j shr 5] = cmpsts[j shr 5] or (1 shl (j and 31))
                        cullp(j + p)
                    }
                }
                cullp((p * p - 3) shr 1)
            }
            testloop(i + 1)
        }
    }

    tailrec fun test(i: Int): Int {
        return if (i <= topi && cmpsts[i shr 5] and (1 shl (i and 31)) != 0) {
            test(i + 1)
        } else {
            i
        }
    }

    testloop(0)

    val iter = object : IntIterator() {
        var i = -1
        override fun nextInt(): Int {
            val oi = i
            i = test(i + 1)
            return if (oi < 0) 2 else oi + oi + 3
        }
        override fun hasNext() = i < topi
    }
    return Iterable { -> iter }
}

fun main(args: Array<String>) {
    primesOdds(100).forEach { print("$it ") }
    println()
    println(primesOdds(1000000).count())
}
```

{{output}}

```txt
2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97
78498
```


'''Concise Functional Versions'''

Ah, one might say, for such a trivial range one writes for conciseness and not for speed.  Well, I say, one can still save memory and some time using odds-only and a bit-packed array, but write very clear and concise (but slower) code using nothing but higher order functions and function calling.  The following code using such techniques can use the same "main" function for the same output but is about two times slower, mostly due to the extra time spent making (nested) function calls, including the function calls necessary for enumeration. Note that the effect of using the "(l .. h).forEach { .. }" is the same as the "for i in l .. h { .. }" as both use an iteration across the range but the second is just syntax sugar to make it look more imperative:


```kotlin
fun primesOdds(rng: Int): Iterable<Int> {
    val topi = (rng - 3) / 2 //convert to nearest index
    val size = topi / 32 + 1 //word size to include index
    val sqrtndx = (Math.sqrt(rng.toDouble()).toInt() - 3) / 2
    val cmpsts = IntArray(size)
    fun is_p(i: Int) = cmpsts[i shr 5] and (1 shl (i and 0x1F)) == 0
    fun cull(i: Int) { cmpsts[i shr 5] = cmpsts[i shr 5] or (1 shl (i and 0x1F)) }
    fun cullp(p: Int) = (((p * p - 3) / 2 .. topi).step(p)).forEach { cull(it) }
    (0 .. sqrtndx).filter { is_p(it) }.forEach { cullp(it + it + 3) }
    fun i2p(i: Int) = if (i < 0) 2 else i + i + 3
    val orng = (-1 .. topi).filter { it < 0 || is_p(it) }.map { i2p(it) }
    return Iterable { -> orng.iterator() }
}
```


The trouble with the above version is that, at least for Kotlin version 1.0, the ".filter" and ".map" extension functions for Iterable<Int> create Java "ArrayList"'s as their output (which are wrapped to return the Kotlin "List<Int>" interface), thus take a considerable amount of memory worse than the first version (using an ArrayList to store the resulting primes), since as the calculations are chained to ".map", require a second ArrayList of up to the same size while the mapping is being done.  The following version uses Sequences , which aren't backed by any permanent structure, but it is another small factor slower due to the nested function calls:


```kotlin
fun primesOdds(rng: Int): Iterable<Int> {
    val topi = (rng - 3) / 2 //convert to nearest index
    val size = topi / 32 + 1 //word size to include index
    val sqrtndx = (Math.sqrt(rng.toDouble()).toInt() - 3) / 2
    val cmpsts = IntArray(size)
    fun is_p(i: Int) = cmpsts[i shr 5] and (1 shl (i and 0x1F)) == 0
    fun cull(i: Int) { cmpsts[i shr 5] = cmpsts[i shr 5] or (1 shl (i and 0x1F)) }
    fun iseq(high: Int, low: Int = 0, stp: Int = 1) =
            Sequence { (low .. high step(stp)).iterator() }
    fun cullp(p: Int) = iseq(topi, (p * p - 3) / 2, p).forEach { cull(it) }
    iseq(sqrtndx).filter { is_p(it) }.forEach { cullp(it + it + 3) }
    fun i2p(i: Int) = if (i < 0) 2 else i + i + 3
    val oseq = iseq(topi, -1).filter { it < 0 || is_p(it) }.map { i2p(it) }
    return Iterable { -> oseq.iterator() }
}
```



### Unbounded Versions


'''An incremental odds-only sieve outputting a sequence (iterator)'''

The following Sieve of Eratosthenes is not purely functional in that it uses a Mutable HashMap to store the state of succeeding composite numbers to be skipped over, but embodies the principles of an incremental implementation of the Sieve of Eratosthenes sieving odds-only and is faster than most incremental sieves due to using mutability.  As with the fastest of this kind of sieve, it uses a delayed secondary primes feed as a source of base primes to generate the composite number progressions.  The code as follows:

```kotlin
fun primesHM(): Sequence<Int> = sequence {
    yield(2)
    fun oddprms(): Sequence<Int> = sequence {
        yield(3); yield(5) // need at least 2 for initialization
        val hm = HashMap<Int,Int>()
        hm.put(9, 6)
        val bps = oddprms().iterator(); bps.next(); bps.next() // skip past 5
        yieldAll(generateSequence(SieveState(7, 5, 25)) {
            ss ->
                var n = ss.n; var q = ss.q
                n += 2
                while ( n >= q || hm.containsKey(n)) {
                    if (n >= q) {
                        val inc = ss.bp shl 1
                        hm.put(n + inc, inc)
                        val bp = bps.next(); ss.bp = bp; q = bp * bp
                    }
                    else {
                        val inc = hm.remove(n)!!
                        var next = n + inc
                        while (hm.containsKey(next)) {
                            next += inc
                        }
                        hm.put(next, inc)
                    }
                    n += 2
                }
                ss.n = n; ss.q = q
                ss
        }.map { it.n })
    }
    yieldAll(oddprms())
}
```


At about 370 clock cycles per culling operation (about 3,800 cycles per prime) on my tablet class Intel CPU, this is not blazing fast but adequate for ranges of a few millions to a hundred million and thus fine for doing things like solving Euler problems.  For instance, Euler Problem 10 of summing the primes to two million can be done with the following "one-liner":

```kotlin
primesHM().takeWhile { it <= 2_000_000 }.map { it.toLong() }.sum()
```


to output the correct answer of the following in about 270 milliseconds for my Intel x5-Z8350 at 1.92 Gigahertz:
{{output}}

```txt
142913828922
```


'''A purely functional Incremental Sieve of Eratosthenes that outputs a sequence (iterator)'''

Following is a Kotlin implementation of the Tree Folding Incremental Sieve of Eratosthenes from an adaptation of the algorithm by Richard Bird.  It is based on lazy lists, but in fact the memoization (and cost in execution time) of a lazy list is not required and the following code uses a "roll-your-own" implementation of a Co-Inductive Stream CIS).  The final output is as a Sequence for convenience in using it.  The code is written as purely function in that no mutation is used:

{{trans|Haskell}}


```kotlin>data class CIS<T
(val head: T, val tailf: () -> CIS<T>) {
  fun toSequence() = generateSequence(this) { it.tailf() } .map { it.head }
}

fun primes(): Sequence<Int> {
  fun merge(a: CIS<Int>, b: CIS<Int>): CIS<Int> {
    val ahd = a.head; val bhd = b.head
    if (ahd > bhd) return CIS(bhd) { ->merge(a, b.tailf()) }
    if (ahd < bhd) return CIS(ahd) { ->merge(a.tailf(), b) }
    return CIS(ahd) { ->merge(a.tailf(), b.tailf()) }
  }
  fun bpmults(p: Int): CIS<Int> {
    val inc = p + p
    fun mlts(c: Int): CIS<Int> = CIS(c) { ->mlts(c + inc) }
    return mlts(p * p)
  }
  fun allmults(ps: CIS<Int>): CIS<CIS<Int>> = CIS(bpmults(ps.head)) { allmults(ps.tailf()) }
  fun pairs(css: CIS<CIS<Int>>): CIS<CIS<Int>> {
    val xs = css.head; val yss = css.tailf(); val ys = yss.head
    return CIS(merge(xs, ys)) { ->pairs(yss.tailf()) }
  }
  fun union(css: CIS<CIS<Int>>): CIS<Int> {
    val xs = css.head
    return CIS(xs.head) { -> merge(xs.tailf(), union(pairs(css.tailf()))) }
  }
  tailrec fun minus(n: Int, cs: CIS<Int>): CIS<Int> =
    if (n >= cs.head) minus(n + 2, cs.tailf()) else CIS(n) { ->minus(n + 2, cs) }
  fun oddprms(): CIS<Int> = CIS(3) { -> CIS(5) { ->minus(7, union(allmults(oddprms()))) } }
  return CIS(2) { ->oddprms() } .toSequence()
}

fun main(args: Array<String>) {
  val limit = 1000000
  val strt = System.currentTimeMillis()
  println(primes().takeWhile { it <= limit } .count())
  val stop = System.currentTimeMillis()
  println("Took ${stop - strt} milliseconds.")
}
```


The code is about five times slower than the more imperative hash table based version immediately above due to the costs of the extra levels of function calls in the functional style.  The Haskell version from which this is derived is much faster due to the extensive optimizations it does to do with function/closure "lifting" as well as a Garbage Collector specifically tuned for functional code.

'''An unbounded Page Segmented Sieve of Eratosthenes that can output a sequence (iterator)'''

The very fastest implementations of a primes sieve are all based on bit-packed mutable arrays which can be made unbounded by setting them up so that they are a succession of sieved bit-packed arrays that have been culled of composites.  The following code is an odds=only implementation that, again, uses a secondary feed of base primes that is only expanded as necessary (in this case memoized by a rudimentary lazy list structure to avoid recalculation for every base primes sweep per page segment):

```kotlin
internal typealias Prime = Long
internal typealias BasePrime = Int
internal typealias BasePrimeArray = IntArray
internal typealias SieveBuffer = ByteArray

// contains a lazy list of a secondary base prime arrays feed
internal data class BasePrimeArrays(val arr: BasePrimeArray,
                                     val rest: Lazy<BasePrimeArrays?>)
                                                : Sequence<BasePrimeArray> {
    override fun iterator() =
        generateSequence(this) { it.rest.value }
            .map { it.arr }.iterator()
}

// count the number of zero bits (primes) in a byte array,
fun countComposites(cmpsts: SieveBuffer): Int {
    var cnt = 0
    for (b in cmpsts) {
        cnt += java.lang.Integer.bitCount(b.toInt().and(0xFF))
    }
    return cmpsts.size.shl(3) - cnt
}

// converts an entire sieved array of bytes into an array of UInt32 primes,
// to be used as a source of base primes...
fun composites2BasePrimeArray(low: Int, cmpsts: SieveBuffer)
                                                            : BasePrimeArray {
    val lmti = cmpsts.size.shl(3)
    val len = countComposites(cmpsts)
    val rslt = BasePrimeArray(len)
    var j = 0
    for (i in 0 until lmti) {
        if (cmpsts[i.shr(3)].toInt() and 1.shl(i and 7) == 0) {
            rslt[j] = low + i + i; j++
        }
    }
    return rslt
}

// do sieving work based on low starting value for the given buffer and
// the given lazy list of base prime arrays...
fun sieveComposites(low: Prime, buffer: SieveBuffer,
                             bpas: Sequence<BasePrimeArray>) {
    val lowi = (low - 3L).shr(1)
    val len = buffer.size
    val lmti = len.shl(3)
    val nxti = lowi + lmti.toLong()
    for (bpa in bpas) {
        for (bp in bpa) {
            val bpi = (bp - 3).shr(1).toLong()
            var strti = (bpi * (bpi + 3L)).shl(1) + 3L
            if (strti >= nxti) return
            val s0 =
                if (strti >= lowi) (strti - lowi).toInt()
                else {
                    val r = (lowi - strti) % bp.toLong()
                    if (r.toInt() == 0) 0 else bp - r.toInt()
                }
            if (bp <= len.shr(3) && s0 <= lmti - bp.shl(6)) {
                val slmti = minOf(lmti, s0 + bp.shl(3))
                tailrec fun mods(s: Int) {
                    if (s < slmti) {
                        val msk = 1.shl(s and 7)
                        tailrec fun cull(c: Int) {
                            if (c < len) {
                                buffer[c] = (buffer[c].toInt() or msk).toByte()
                                cull(c + bp)
                            }
                        }
                        cull(s.shr(3)); mods(s + bp)
                    }
                }
                mods(s0)
            }
            else {
                tailrec fun cull(c: Int) {
                    if (c < lmti) {
                        val w = c.shr(3)
                        buffer[w] = (buffer[w].toInt() or 1.shl(c and 7)).toByte()
                        cull(c + bp)
                    }
                }
                cull(s0)
            }
        }
    }
}

// starts the secondary base primes feed with minimum size in bits set to 4K...
// thus, for the first buffer primes up to 8293,
// the seeded primes easily cover it as 97 squared is 9409...
fun makeBasePrimeArrays(): Sequence<BasePrimeArray> {
    var cmpsts = SieveBuffer(512)
    fun nextelem(low: Int, bpas: Sequence<BasePrimeArray>): BasePrimeArrays {
        // calculate size so that the bit span is at least as big as the
        // maximum culling prime required, rounded up to minsizebits blocks...
        val rqdsz = 2 + Math.sqrt((1 + low).toDouble()).toInt()
        val sz = (rqdsz.shr(12) + 1).shl(9) // size iin bytes
        if (sz > cmpsts.size) cmpsts = SieveBuffer(sz)
        cmpsts.fill(0)
        sieveComposites(low.toLong(), cmpsts, bpas)
        val arr = composites2BasePrimeArray(low, cmpsts)
        val nxt = low + cmpsts.size.shl(4)
        return BasePrimeArrays(arr, lazy { ->nextelem(nxt, bpas) })
    }
    // pre-seeding breaks recursive race,
    // as only known base primes used for first page...
    var preseedarr = intArrayOf( // pre-seed to 100, can sieve to 10,000...
        3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41
        , 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97 )
    return BasePrimeArrays(preseedarr, lazy {->nextelem(101, makeBasePrimeArrays())})
}

// a seqence over successive sieved buffer composite arrays,
// returning a tuple of the value represented by the lowest possible prime
// in the sieved composites array and the array itself;
// the array has a 16 Kilobytes minimum size (CPU L1 cache), but
// will grow so that the bit span is larger than the
// maximum culling base prime required, possibly making it larger than
// the L1 cache for large ranges, but still reasonably efficient using
// the L2 cache: very efficient up to about 16e9 range;
// reasonably efficient to about 2.56e14 for two Megabyte L2 cache = > 1 day...
fun makeSievePages(): Sequence<Pair<Prime,SieveBuffer>> {
    val bpas = makeBasePrimeArrays() // secondary source of base prime arrays
    fun init(): SieveBuffer {
        val c = SieveBuffer(16384); sieveComposites(3L, c, bpas); return c }
    return generateSequence(Pair(3L, init())) {
        (low, cmpsts) ->
            // calculate size so that the bit span is at least as big as the
            // max culling prime required, rounded up to minsizebits blocks...
            val rqdsz = 2 + Math.sqrt((1 + low).toDouble()).toInt()
            val sz = (rqdsz.shr(17) + 1).shl(14) // size iin bytes
            val ncmpsts = if (sz > cmpsts.size) SieveBuffer(sz) else cmpsts
            ncmpsts.fill(0)
            val nlow = low + ncmpsts.size.toLong().shl(4)
            sieveComposites(nlow, ncmpsts, bpas)
            Pair(nlow, ncmpsts)
    }
}

fun countPrimesTo(range: Prime): Prime {
    if (range < 3) { if (range < 2) return 0 else return 1 }
    var count = 1L
    for ((low,cmpsts) in makeSievePages()) {
        if (low + cmpsts.size.shl(4) > range) {
            val lsti = (range - low).shr(1).toInt()
            val lstw = lsti.shr(3)
            val msk = -2.shl(lsti.and(7))
            count += 32 + lstw.shl(3)
            for (i in 0 until lstw)
                count -= java.lang.Integer.bitCount(cmpsts[i].toInt().and(0xFF))
            count -= java.lang.Integer.bitCount(cmpsts[lstw].toInt().or(msk))
            break
        } else {
            count += countComposites(cmpsts)
        }
    }
    return count
}

// sequence over primes from above page iterator;
// unless doing something special with individual primes, usually unnecessary;
// better to do manipulations based on the composites bit arrays...
// takes at least as long to enumerate the primes as sieve them...
fun primesPaged(): Sequence<Prime> = sequence {
    yield(2L)
    for ((low,cmpsts) in makeSievePages()) {
        val szbts = cmpsts.size.shl(3)
        for (i in 0 until szbts) {
            if (cmpsts[i.shr(3)].toInt() and 1.shl(i and 7) != 0) continue
            yield(low + i.shl(1).toLong())
        }
    }
}
```


For this implementation, counting the primes to a million is trivial at about 15 milliseconds on the same CPU as above, or almost too short to count.

It shows its speed in solving the Euler Problem 10 above about five times faster at about 50 milliseconds to give the same output:

It can sum the primes to 200 million or a hundred times the range in just over three seconds.

It finds the count of primes to a billion in about 16 seconds or just about 1000 times slower than to sum the primes to a range 1000 times less for an almost linear response to range as it should be.

However, much of the time (about two thirds) is spent iterating over the results rather than doing the actual work of sieving; for this sort of problem such as counting, finding the nth prime, finding occurrences of maximum prime gaps, etc., one should really use specialized function that directly manipulate the output sieve arrays. Such a function is provided by the `countPrimeTo` function, which can count the primes to a billion (50847534) in about 5.65 seconds, or about 10.6 clock cycles per culling operation or about 210 cycles per prime.

Kotlin isn't really fast even as compared to other virtual machine languages such as C# and F# on CLI but that is mostly due to limitations of the Java Virtual Machine (JVM) as to speed of generated Just In Time (JIT) compilation, handling of primitive number operations, enforced array bounds checks, etc.  It will always be much slower than native code producing compilers and the (experimental) native compiler for Kotlin still isn't up to speed (pun intended), producing code that is many times slower than the code run on the JVM (December 2018).


## Liberty BASIC


```lb
    'Notice that arrays are globally visible to functions.
    'The sieve() function uses the flags() array.
    'This is a Sieve benchmark adapted from BYTE 1985
    ' May, page 286

    size = 7000
    dim flags(7001)
    start = time$("ms")
    print sieve(size); " primes found."
    print "End of iteration.  Elapsed time in milliseconds: "; time$("ms")-start
    end

    function sieve(size)
        for i = 0 to size
            if flags(i) = 0 then
                prime = i + i + 3
                k = i + prime
                while k <= size
                    flags(k) = 1
                    k = k + prime
                wend
                sieve = sieve + 1
            end if
        next i
    end function
```



## Limbo


```Go
implement Sieve;

include "sys.m";
	sys: Sys;
	print: import sys;
include "draw.m";
	draw: Draw;

Sieve : module
{
	init : fn(ctxt : ref Draw->Context, args : list of string);
};

init (ctxt: ref Draw->Context, args: list of string)
{
	sys = load Sys Sys->PATH;

	limit := 201;
	sieve : array of int;
	sieve = array [201] of {* => 1};
	(sieve[0], sieve[1]) = (0, 0);

	for (n := 2; n < limit; n++) {
		if (sieve[n]) {
			for (i := n*n; i < limit; i += n) {
				sieve[i] = 0;
			}
		}
	}

	for (n = 1; n < limit; n++) {
		if (sieve[n]) {
			print ("%4d", n);
		} else {
			print("   .");
		};
		if ((n%20) == 0)
			print("\n\n");
	}
}

```



## Lingo


```Lingo
-- parent script "sieve"
property _sieve

----------------------------------------
-- @constructor
----------------------------------------
on new (me)
    me._sieve = []
    return me
end

----------------------------------------
-- Returns list of primes <= n
----------------------------------------
on getPrimes (me, limit)
    if me._sieve.count<limit then me._primeSieve(limit)
    primes = []
    repeat with i = 2 to limit
        if me._sieve[i] then primes.add(i)
    end repeat
    return primes
end

----------------------------------------
-- Sieve of Eratosthenes
----------------------------------------
on _primeSieve (me, limit)
    me._sieve = [0]
    repeat with i = 2 to limit
        me._sieve[i] = 1
    end repeat
    c = sqrt(limit)
    repeat with i = 2 to c
        if (me._sieve[i]=0) then next repeat
        j = i*i -- start with square
        repeat while (j<=limit)
            me._sieve[j] = 0
            j = j + i
        end repeat
    end repeat
end
```



```Lingo
sieve = script("sieve").new()
put sieve.getPrimes(100)
```


{{out}}

```txt

-- [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97]

```



## LiveCode


```LiveCode
function sieveE int
    set itemdel to comma
    local sieve
    repeat with i = 2 to int
        put i into sieve[i]
    end repeat
    put 2 into n
    repeat while n < int
        repeat with p = n to int step n
            if p = n then
                next repeat
            else
                put empty into sieve[p]
            end if
        end repeat
        add 1 to n
    end repeat
    combine sieve with comma
    filter items of sieve without empty
    sort items of sieve ascending numeric
    return sieve
end sieveE
```

Example
```LiveCode
put sieveE(121)
--  2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97,101,103,107,109,113
```



## Logo

 to sieve :limit
   make "a (array :limit 2)     ; initialized to empty lists
   make "p []
   for [i 2 :limit] [
     if empty? item :i :a [
       queue "p :i
       for [j [:i * :i] :limit :i] [setitem :j :a :i]
     ]
   ]
   output :p
 end
 print sieve 100   ; 2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97


## Logtalk

{{incorrect|Logtalk|Not a true Sieve of Eratosthenes but rather a Trial Division Sieve}}
 due to the use of mod (modulo = division) in the filter function.
A coinduction based solution just for fun:

```logtalk

:- object(sieve).

	:- public(primes/2).

	:- coinductive([
		sieve/2, filter/3
	]).

	% computes a coinductive list with all the primes in the 2..N interval
	primes(N, Primes) :-
		generate_infinite_list(N, List),
		sieve(List, Primes).

	% generate a coinductive list with a 2..N repeating patern
	generate_infinite_list(N, List) :-
		sequence(2, N, List, List).

	sequence(Sup, Sup, [Sup| List], List) :-
		!.
	sequence(Inf, Sup, [Inf| List], Tail) :-
		Next is Inf + 1,
		sequence(Next, Sup, List, Tail).

	sieve([H| T], [H| R]) :-
		filter(H, T, F),
		sieve(F, R).

	filter(H, [K| T], L) :-
		(	K > H, K mod H =:= 0 ->
			% throw away the multiple we found
			L = T1
		;	% we must not throw away the integer used for filtering
			% as we must return a filtered coinductive list
			L = [K| T1]
		),
		filter(H, T, T1).

:- end_object.

```

Example query:

```logtalk

?- sieve::primes(20, P).
P = [2, 3|_S1], % where
    _S1 = [5, 7, 11, 13, 17, 19, 2, 3|_S1] .

```



## Lua


```lua
function erato(n)
  if n < 2 then return {} end
  local t = {0} -- clears '1'
  local sqrtlmt = math.sqrt(n)
  for i = 2, n do t[i] = 1 end
  for i = 2, sqrtlmt do if t[i] ~= 0 then for j = i*i, n, i do t[j] = 0 end end end
  local primes = {}
  for i = 2, n do if t[i] ~= 0 then table.insert(primes, i) end end
  return primes
end
```


The following changes the code to '''odds-only''' using the same large array-based algorithm:

```lua
function erato2(n)
  if n < 2 then return {} end
  if n < 3 then return {2} end
  local t = {}
  local lmt = (n - 3) / 2
  local sqrtlmt = (math.sqrt(n) - 3) / 2
  for i = 0, lmt do t[i] = 1 end
  for i = 0, sqrtlmt do if t[i] ~= 0 then
    local p = i + i + 3
    for j = (p*p - 3) / 2, lmt, p do t[j] = 0 end end end
  local primes = {2}
  for i = 0, lmt do if t[i] ~= 0 then table.insert(primes, i + i + 3) end end
  return primes
end
```


The following code implements '''an odds-only "infinite" generator style using a table as a hash table''', including postponing adding base primes to the table:


```lua
function newEratoInf()
  local _cand = 0; local _lstbp = 3; local _lstsqr = 9
  local _composites = {}; local _bps = nil
  local _self = {}
  function _self.next()
    if _cand < 9 then if _cand < 1 then _cand = 1; return 2
                     elseif _cand >= 7 then
                       --advance aux source base primes to 3...
                       _bps = newEratoInf()
                       _bps.next(); _bps.next() end end
    _cand = _cand + 2
    if _composites[_cand] == nil then -- may be prime
      if _cand >= _lstsqr then -- if not the next base prime
        local adv = _lstbp + _lstbp -- if next base prime
        _composites[_lstbp * _lstbp + adv] = adv -- add cull seq
        _lstbp = _bps.next(); _lstsqr = _lstbp * _lstbp -- adv next base prime
        return _self.next()
      else return _cand end -- is prime
    else
      local v = _composites[_cand]
      _composites[_cand] = nil
      local nv = _cand + v
      while _composites[nv] ~= nil do nv = nv + v end
      _composites[nv] = v
      return _self.next() end
  end
  return _self
end

gen = newEratoInf()
count = 0
while gen.next() <= 10000000 do count = count + 1 end -- sieves to 10 million
print(count)

```


which outputs "664579" in about three seconds.  As this code uses much less memory for a given range than the previous ones and retains efficiency better with range, it is likely more appropriate for larger sieve ranges.

## Lucid

{{incorrect|Lucid|Not a true Sieve of Eratosthenes but rather a Trial Division Sieve}}
 prime
  where
     prime = 2 fby (n whenever isprime(n));
     n = 3 fby n+2;
     isprime(n) = not(divs) asa divs or prime*prime > N
                     where
                       N is current n;
                       divs = N mod prime eq 0;
                     end;
  end

### recursive

{{incorrect|Lucid|Not a true Sieve of Eratosthenes but rather a Trial Division Sieve}}
 sieve( N )
    where
     N = 2 fby N + 1;
     sieve( i ) =
       i fby sieve ( i whenever i mod first i ne 0 ) ;
    end


## M2000 Interpreter


```M2000 Interpreter

Module EratosthenesSieve (x) {
      \\ Κόσκινο του Ερατοσθένη
      If x>200000 Then Exit
      Dim i(x+1)
      k=2
      k2=x div 2
      While k<=k2 {
            m=k+k
            While m<=x {
                  i(m)=1
                  m+=k
            }
            k++
      }
      For i=2 to x {
      If i(i)=0 Then Print i,
      }
      Print
}
EratosthenesSieve 1000

```




## M4


```M4
define(`lim',100)dnl
define(`for',
   `ifelse($#,0,
      ``$0'',
      `ifelse(eval($2<=$3),1,
         `pushdef(`$1',$2)$5`'popdef(`$1')$0(`$1',eval($2+$4),$3,$4,`$5')')')')dnl
for(`j',2,lim,1,
   `ifdef(a[j],
      `',
      `j for(`k',eval(j*j),lim,j,
         `define(a[k],1)')')')

```


Output:

```txt

2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97

```



## Maple


```Maple
Eratosthenes := proc(n::posint)
  local numbers_to_check, i, k;
  numbers_to_check := [seq(2 .. n)];
  for i from 2 to floor(sqrt(n)) do
      for k from i by i while k <= n do
          if evalb(k <> i) then
            numbers_to_check[k - 1] := 0;
          end if;
      end do;
  end do;
  numbers_to_check := remove(x -> evalb(x = 0), numbers_to_check);
  return numbers_to_check;
  end proc:

```

{{out}}

```txt

Eratosthenes(100);
[2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97]

```



## Mathematica


```Mathematica
Eratosthenes[n_] := Module[{numbers = Range[n]},
  Do[If[numbers[[i]] != 0, Do[numbers[[i j]] = 0, {j, 2, n/i}]], {i,
    2, Sqrt[n]}];
  Select[numbers, # > 1 &]]

Eratosthenes[100]
```


### Slightly Optimized Version

The below has been improved to not require so many operations per composite number cull for about two thirds the execution time:

```Mathematica
Eratosthenes[n_] := Module[{numbers = Range[n]},
  Do[If[numbers[[i]] != 0, Do[numbers[[j]] = 0, {j,i i,n,i}]],{i,2,Sqrt[n]}];
  Select[numbers, # > 1 &]]

Eratosthenes[100]
```

===Sieving Odds-Only Version===
The below has been further improved to only sieve odd numbers for a further reduction in execution time by a factor of over two:

```Mathematica
Eratosthenes2[n_] := Module[{numbers = Range[3, n, 2], limit = (n - 1)/2},
  Do[c = numbers[[i]]; If[c != 0,
    Do[numbers[[j]] = 0, {j,(c c - 1)/2,limit,c}]], {i,1,(Sqrt[n] - 1)/2}];
  Prepend[Select[numbers, # > 1 &], 2]]

Eratosthenes2[100]
```


## MATLAB


### Somewhat optimized true Sieve of Eratosthenes


```MATLAB
function P = erato(x)        % Sieve of Eratosthenes: returns all primes between 2 and x

    P = [0 2:x];             % Create vector with all ints between 2 and x where
                             %   position 1 is hard-coded as 0 since 1 is not a prime.

    for n = 2:sqrt(x)        % All primes factors lie between 2 and sqrt(x).
       if P(n)               % If the current value is not 0 (i.e. a prime),
          P(n*n:n:x) = 0;    % then replace all further multiples of it with 0.
       end
    end                      % At this point P is a vector with only primes and zeroes.

    P = P(P ~= 0);           % Remove all zeroes from P, leaving only the primes.
end
```
The optimization lies in fewer steps in the for loop, use of MATLAB's built-in array operations and no modulo calculation.

'''Limitation:''' your machine has to be able to allocate enough memory for an array of length x.


### A more efficient Sieve

A more efficient Sieve avoids creating a large double precision vector P, instead using a logical array (which consumes 1/8 the memory of a double array of the same size) and only converting to double those values corresponding to primes.


```MATLAB
function P = sieveOfEratosthenes(x)
    ISP = [false true(1, x-1)]; % 1 is not prime, but we start off assuming all numbers between 2 and x are
    for n = 2:sqrt(x)
        if ISP(n)
            ISP(n*n:n:x) = false; % Multiples of n that are greater than n*n are not primes
        end
    end
    % The ISP vector that we have calculated is essentially the output of the ISPRIME function called on 1:x
    P = find(ISP); % Convert the ISPRIME output to the values of the primes by finding the locations
                   % of the TRUE values in S.
end
```


You can compare the output of this function against the PRIMES function included in MATLAB, which performs a somewhat more memory-efficient Sieve (by not storing even numbers, at the expense of a more complicated indexing expression inside the IF statement.)


## Maxima


```maxima
sieve(n):=block(
   [a:makelist(true,n),i:1,j],
   a[1]:false,
   do (
      i:i+1,
      unless a[i] do i:i+1,
      if i*i>n then return(sublist_indices(a,identity)),
      for j from i*i step i while j<=n do a[j]:false
   )
)$
```


## MAXScript

 fn eratosthenes n =
 (
     multiples = #()
     print 2
     for i in 3 to n do
     (
         if (findItem multiples i) == 0 then
         (
             print i
             for j in (i * i) to n by i do
             (
                 append multiples j
             )
         )
     )
 )

 eratosthenes 100


## Mercury


```Mercury
:- module sieve.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.
:- implementation.
:- import_module bool, array, int.

main(!IO) :-
    sieve(50, Sieve),
    dump_primes(2, size(Sieve), Sieve, !IO).

:- pred dump_primes(int, int, array(bool), io, io).
:- mode dump_primes(in, in, array_di, di, uo) is det.
dump_primes(N, Limit, !.A, !IO) :-
    ( if N < Limit then
        unsafe_lookup(!.A, N, Prime),
        (
            Prime = yes,
            io.write_line(N, !IO)
        ;
            Prime = no
        ),
        dump_primes(N + 1, Limit, !.A, !IO)
    else
        true
    ).

:- pred sieve(int, array(bool)).
:- mode sieve(in, array_uo) is det.
sieve(N, !:A) :-
    array.init(N, yes, !:A),
    sieve(2, N, !A).

:- pred sieve(int, int, array(bool), array(bool)).
:- mode sieve(in, in, array_di, array_uo) is det.
sieve(N, Limit, !A) :-
    ( if N < Limit then
        unsafe_lookup(!.A, N, Prime),
        (
            Prime = yes,
            sift(N + N, N, Limit, !A),
            sieve(N + 1, Limit, !A)
        ;
            Prime = no,
            sieve(N + 1, Limit, !A)
        )
    else
        true
    ).

:- pred sift(int, int, int, array(bool), array(bool)).
:- mode sift(in, in, in, array_di, array_uo) is det.
sift(I, N, Limit, !A) :-
    ( if I < Limit then
        unsafe_set(I, no, !A),
        sift(I + N, N, Limit, !A)
    else
        true
    ).
```



## Microsoft Small Basic

{{trans|GW-BASIC}}

```microsoftsmallbasic

TextWindow.Write("Enter number to search to: ")
limit = TextWindow.ReadNumber()
For n = 2 To limit
  flags[n] = 0
EndFor
For n = 2 To math.SquareRoot(limit)
  If flags[n] = 0 Then
    For K = n * n To limit Step n
      flags[K] = 1
    EndFor
  EndIf
EndFor
' Display the primes
If limit >= 2 Then
  TextWindow.Write(2)
  For n = 3 To limit
    If flags[n] = 0 Then
      TextWindow.Write(", " + n)
    EndIf
  EndFor
  TextWindow.WriteLine("")
EndIf

```


=={{header|Modula-3}}==

### Regular version

This version runs slow because of the way I/O is implemented in the CM3 compiler. Setting <code>ListPrimes = FALSE</code> achieves speed comparable to C on sufficiently high values of <code>LastNum</code> (e.g., 10^6).

```modula3
MODULE Eratosthenes EXPORTS Main;

IMPORT IO;

FROM Math IMPORT sqrt;

CONST
  LastNum    = 1000;
  ListPrimes = TRUE;

VAR
  a: ARRAY[2..LastNum] OF BOOLEAN;

VAR
  n := LastNum - 2 + 1;

BEGIN

  (* set up *)
  FOR i := FIRST(a) TO LAST(a) DO
    a[i] := TRUE;
  END;

  (* declare a variable local to a block *)
  VAR b := FLOOR(sqrt(FLOAT(LastNum, LONGREAL)));

  (* the block must follow immediately *)
  BEGIN

    (* print primes and mark out composites up to sqrt(LastNum) *)
    FOR i := FIRST(a) TO b DO
      IF a[i] THEN
        IF ListPrimes THEN IO.PutInt(i); IO.Put(" "); END;
        FOR j := i*i TO LAST(a) BY i DO
          IF a[j] THEN
            a[j] := FALSE;
            DEC(n);
          END;
        END;
      END;
    END;

    (* print remaining primes *)
    IF ListPrimes THEN
      FOR i := b + 1 TO LAST(a) DO
        IF a[i] THEN
          IO.PutInt(i); IO.Put(" ");
        END;
      END;
    END;

  END;

  (* report *)
  IO.Put("There are ");         IO.PutInt(n);
  IO.Put(" primes from 2 to "); IO.PutInt(LastNum);
  IO.PutChar('\n');

END Eratosthenes.
```



### Advanced version

This version uses more "advanced" types.

```modula3
(* From the CM3 examples folder (comments removed). *)

MODULE Sieve EXPORTS Main;

IMPORT IO;

TYPE
  Number = [2..1000];
  Set = SET OF Number;

VAR
  prime: Set := Set {FIRST(Number) .. LAST(Number)};

BEGIN
  FOR i := FIRST(Number) TO LAST(Number) DO
    IF i IN prime THEN
      IO.PutInt(i);
      IO.Put(" ");

      FOR j := i TO LAST(Number) BY i DO
        prime := prime - Set{j};
      END;
    END;
  END;
  IO.Put("\n");
END Sieve.
```



## MUMPS


```MUMPS
ERATO1(HI)
 ;performs the Sieve of Erotosethenes up to the number passed in.
 ;This version sets an array containing the primes
 SET HI=HI\1
 KILL ERATO1 ;Don't make it new - we want it to remain after we quit the function
 NEW I,J,P
 FOR I=2:1:(HI**.5)\1 FOR J=I*I:I:HI SET P(J)=1
 FOR I=2:1:HI S:'$DATA(P(I)) ERATO1(I)=I
 KILL I,J,P
 QUIT
```

Example:

```txt
USER>SET MAX=100,C=0 DO ERATO1^ROSETTA(MAX)
USER>WRITE !,"PRIMES BETWEEN 1 AND ",MAX,! FOR  SET I=$ORDER(ERATO1(I)) Q:+I<1  WRITE I,", "

PRIMES BETWEEN 1 AND 100
2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73,79, 83, 89, 97,
```



## NetRexx

===Version 1 (slow)===

```Rexx
/* NetRexx */

options replace format comments java crossref savelog symbols binary

parse arg loWatermark hiWatermark .
if loWatermark = '' | loWatermark = '.' then loWatermark = 1
if hiWatermark = '' | hiWatermark = '.' then hiWatermark = 200

do
  if \loWatermark.datatype('w') | \hiWatermark.datatype('w') then -
    signal NumberFormatException('arguments must be whole numbers')
  if loWatermark > hiWatermark then -
    signal IllegalArgumentException('the start value must be less than the end value')

  seive = sieveOfEratosthenes(hiWatermark)
  primes = getPrimes(seive, loWatermark, hiWatermark).strip

  say 'List of prime numbers from' loWatermark 'to' hiWatermark 'via a "Sieve of Eratosthenes" algorithm:'
  say '  'primes.changestr(' ', ',')
  say '  Count of primes:' primes.words
catch ex = Exception
  ex.printStackTrace
end

return

method sieveOfEratosthenes(hn = long) public static binary returns Rexx

  sv = Rexx(isTrue)
  sv[1] = isFalse
  ix = long
  jx = long

  loop ix = 2 while ix * ix <= hn
    if sv[ix] then loop jx = ix * ix by ix while jx <= hn
      sv[jx] = isFalse
      end jx
    end ix

  return sv

method getPrimes(seive = Rexx, lo = long, hi = long) private constant binary returns Rexx

  primes = Rexx('')
  loop p_ = lo to hi
    if \seive[p_] then iterate p_
    primes = primes p_
    end p_

  return primes

method isTrue public constant binary returns boolean
  return 1 == 1

method isFalse public constant binary returns boolean
  return \isTrue

```

;Output
<pre style="overflow:scroll">
List of prime numbers from 1 to 200 via a "Sieve of Eratosthenes" algorithm:
  2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97,101,103,107,109,113,127,131,137,139,149,151,157,163,167,173,179,181,191,193,197,199
  Count of primes: 46

```

===Version 2 (significantly, i.e. 10 times faster)===

```NetRexx
/* NetRexx ************************************************************
* Essential improvements:Use boolean instead of Rexx for sv
*                        and remove methods isTrue and isFalse
* 24.07.2012 Walter Pachl courtesy Kermit Kiser
**********************************************************************/

options replace format comments java crossref savelog symbols binary

parse arg loWatermark hiWatermark .
if loWatermark = '' | loWatermark = '.' then loWatermark = 1
if hiWatermark = '' | hiWatermark = '.' then hiWatermark = 200000

startdate=Date Date()
do
  if \loWatermark.datatype('w') | \hiWatermark.datatype('w') then -
    signal NumberFormatException('arguments must be whole numbers')
  if loWatermark > hiWatermark then -
    signal IllegalArgumentException(-
                 'the start value must be less than the end value')
  sieve = sieveOfEratosthenes(hiWatermark)
  primes = getPrimes(sieve, loWatermark, hiWatermark).strip
  if hiWatermark = 200 Then do
    say 'List of prime numbers from' loWatermark 'to' hiWatermark
    say '  'primes.changestr(' ', ',')
  end
catch ex = Exception
  ex.printStackTrace
end
enddate=Date Date()
Numeric Digits 20
say (enddate.getTime-startdate.getTime)/1000 'seconds elapsed'
say '  Count of primes:' primes.words

return

method sieveOfEratosthenes(hn = int) -
                                  public static binary returns boolean[]
  true  = boolean 1
  false = boolean 0
  sv = boolean[hn+1]
  sv[1] = false

  ix = int
  jx = int

  loop ix=2 to hn
    sv[ix]=true
    end ix

  loop ix = 2 while ix * ix <= hn
    if sv[ix] then loop jx = ix * ix by ix while jx <= hn
      sv[jx] = false
      end jx
    end ix

  return sv

method getPrimes(sieve = boolean[], lo = int, hi = int) -
                                    private constant binary Returns Rexx
  p_ = int
  primes = Rexx('')
  loop p_ = lo to hi
    if \sieve[p_] then iterate p_
    primes = primes p_
    end p_

  return primes
```



## Nial

{{incorrect|Nial|It uses rem testing and so is a trial division algorithm, not a sieve of Eratosthenes.}}
 primes is sublist [ each (2 = sum eachright (0 = mod) [pass,count]), pass ] rest count
Using it
 |primes 10
 =2 3 5 7

## Nim


```nim
from math import sqrt

iterator primesUpto(limit: int): int =
  let sqrtLimit = int(sqrt(float64(limit)))
  var composites = newSeq[bool](limit + 1)
  for n in 2 .. sqrtLimit: # cull to square root of limit
    if not composites[n]: # if prime -> cull its composites
      for c in countup(n *% n, limit +% 1, n): # start at ``n`` squared
        composites[c] = true
  for n in 2 .. limit: # separate iteration over results
    if not composites[n]:
      yield n

stdout.write "The primes up to 100 are:  "
for x in primesUpto(100):
   stdout.write(x, " ")
echo ""

var count = 0
for p in primesUpto(1000000):
  count += 1
echo "There are ", count, " primes up to 1000000."
```

{{out}}

```txt
Primes are:
2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97
There are 78498 primes up to 1000000.
```


'''Alternate odds-only bit-packed version'''

The above version wastes quite a lot of memory by using a sequence of boolean values to sieve the composite numbers and sieving all numbers when two is the only even prime.  The below code uses a bit-packed sequence to save a factor of eight in memory and also sieves only odd primes for another memory saving by a factor of two; it is also over two and a half times faster due to reduced number of culling operations and better use of the CPU cache as a little cache goes a lot further - this better use of cache is more than enough to make up for the extra bit-packing shifting operations:


```nim
iterator isoe_upto(top: uint): uint =
  let topndx = int((top - 3) div 2)
  let sqrtndx = (int(sqrt float64(top)) - 3) div 2
  var cmpsts = newSeq[uint32](topndx div 32 + 1)
  for i in 0 .. sqrtndx: # cull composites for primes
    if (cmpsts[i shr 5] and (1u32 shl (i and 31))) == 0:
      let p = i + i + 3
      for j in countup((p * p - 3) div 2, topndx, p):
        cmpsts[j shr 5] = cmpsts[j shr 5] or (1u32 shl (j and 31))
  yield 2 # separate culling above and iteration here
  for i in 0 .. topndx:
    if (cmpsts[i shr 5] and (1u32 shl (i and 31))) == 0:
      yield uint(i + i + 3)
```


The above code can be used with the same output functions as in the first code, just replacing the name of the iterator "iprimes_upto" with this iterator's name "isoe_upto" in two places.  The output will be identical.

=
## Nim Unbounded Versions
=

For many purposes, one doesn't know the exact upper limit desired to easily use the above versions; in addition, those versions use an amount of memory proportional to the range sieved.  In contrast, unbounded versions continuously update their range as they progress and only use memory proportional to the secondary base primes stream, which is only proportional to the square root of the range.  One of the most basic functional versions is the TreeFolding sieve which is based on merging lazy streams as per Richard Bird's contribution to incremental sieves in Haskell, but which has a much better asymptotic execution complexity due to the added tree folding.  The following code is a version of that in Nim (odds-only):

```nim
from times import epochTime

type PrimeType = int
iterator primesTreeFolding(): PrimeType {.closure.} =
  # needs a Co Inductive Stream - CIS...
  type
    CIS[T] = ref object
      head: T
      tail: () -> CIS[T]

  proc merge(xs, ys: CIS[PrimeType]): CIS[PrimeType] =
    let x = xs.head; let y = ys.head
    if x < y:
      CIS[PrimeType](head: x, tail: () => merge(xs.tail(), ys))
    elif y < x:
      CIS[PrimeType](
        head: y,
        tail: () => merge(xs, ys.tail()))
    else:
      CIS[PrimeType](
        head: x,
        tail: () => merge(xs.tail(), ys.tail()))

  proc pmults(p: PrimeType): CIS[PrimeType] =
    let inc = p + p
    proc mlts(c: PrimeType): CIS[PrimeType] =
      CIS[PrimeType](head: c, tail: () => mlts(c + inc))
    mlts(p * p)

  proc allmults(ps: CIS[PrimeType]): CIS[CIS[PrimeType]] =
    CIS[CIS[PrimeType]](
      head: pmults(ps.head),
      tail: () => allmults(ps.tail()))

  proc pairs(css: CIS[CIS[PrimeType]]): CIS[CIS[PrimeType]] =
    let cs0 = css.head; let rest0 = css.tail()
    CIS[CIS[PrimeType]](
      head: merge(cs0, rest0.head),
      tail: () => pairs(rest0.tail()))

  proc cmpsts(css: CIS[CIS[PrimeType]]): CIS[PrimeType] =
    let cs0 = css.head
    CIS[PrimeType](
      head: cs0.head,
      tail: () => merge(cs0.tail(), css.tail().pairs.cmpsts))

  proc minusAt(n: PrimeType, cs: CIS[PrimeType]): CIS[PrimeType] =
    var nn = n; var ncs = cs
    while nn >= ncs.head: nn += 2; ncs = ncs.tail()
    CIS[PrimeType](head: nn, tail: () => minusAt(nn + 2, ncs))

  proc oddprms(): CIS[PrimeType] =
    CIS[PrimeType](
      head: 3.PrimeType,
      tail: () => minusAt(5.PrimeType, oddprms().allmults.cmpsts))

  var prms =
    CIS[PrimeType](head: 2.PrimeType, tail: () => oddprms())
  while true: yield prms.head; prms = prms.tail()

stdout.write "The first 25 primes are:  "
var counter = 0
for p in primesTreeFolding():
  if counter >= 25: break
  stdout.write(p, " "); counter += 1
echo ""
start = epochTime()
counter = 0
for p in primesTreeFolding():
  if p > range: break else: counter += 1
elapsed = epochTime() - start
echo "There are ", counter, " primes up to 1000000."
echo "This test took ", elapsed, " seconds."
```

{{output}}

```txt
The first 25 primes are:  2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97
There are 78498 primes up to 1000000
This test took 1.753238677978516 seconds.
```


It takes in the order of a second to compute the primes to a million (depending on CPU), and is so slow due to the many small memory allocations/de-allocations required, which is a characteristic of functional forms of code.  Is is purely functional in that everything is immutable other than that Nim does not have Tail Call Optimization (TCO) so that we can freely use function recursion with no execution time cost; therefore, where necessary this is implemented with imperative loops, which is what TCO is generally turned into such forms "under the covers".  It is also slow due to the algorithm being only O(n (log n) (log (log n))) rather than without the extra "log n" factor as some version have.  This slowness makes it only moderately useful for ranges up to a few million.

Since the algorithm does not require the memoization of a full lazy list, it uses an internal Co Inductive Stream of deferred execution states, finally outputting an iterator to enumerate over the lazily computed stream of primes.

'''A faster alternative using a mutable hash table (odds-only)'''

To show the cost of functional forms of code, the following code is written embracing mutability, both by using a mutable hash table to store the state of incremental culling by the secondary stream of base primes and by using mutable values to store the state wherever possible, as per the following code:

```nim
import tables

type PrimeType = int
proc primesHashTable(): iterator(): PrimeType {.closure.} =
  iterator output(): PrimeType {.closure.} =
    # some initial values to avoid race and reduce initializations...
    yield 2.PrimeType; yield 3.PrimeType; yield 5.PrimeType; yield 7.PrimeType
    var h = initTable[PrimeType,PrimeType]()
    var n = 9.PrimeType
    let bps = primesHashTable()
    var bp = bps() # advance past 2
    bp = bps(); var q = bp * bp # to initialize with 3
    while true:
      if n >= q:
        let inc = bp + bp
        h.add(n + inc, inc)
        bp = bps(); q = bp * bp
      elif h.hasKey(n):
        var inc: PrimeType
        discard h.take(n, inc)
        var nxt = n + inc
        while h.hasKey(nxt): nxt += inc # ensure no duplicates
        h.add(nxt, inc)
      else: yield n
      n += 2.PrimeType
  output

stdout.write "The first 25 primes are:  "
var counter = 0
var iter = primesHashTable()
for p in iter():
  if counter >= 25: break else: stdout.write(p, " "); counter += 1
echo ""
let start = epochTime()
counter = 0
iter = primesHashTable()
for p in iter():
  if p > 1000000: break else: counter += 1
let elapsed = epochTime() - start
echo "The number of primes up to a million is:  ", counter
stdout.write("This test took ", elapsed, " seconds.\n")
```


The output is identical to the first unbounded version, other than it is over about 15 times faster sieving to a million.  For larger ranges it will continue to pull further ahead of the above version due to only O(n (log (log n))) performance because of the hash table having an average of O(1) access, and it is only so slow due to the large constant overhead of doing the hashing calculations and look-ups.

'''Very fast Page Segmented version using a bit-packed mutable array (odds-only)'''

Note: This version is used as a very fast alternative in [[Extensible_prime_generator#Nim]]

For the highest speeds, one needs to use page segmented mutable arrays as in the bit-packed version here:

```nim
# a Page Segmented Odd-Only Bit-Packed Sieve of Eratosthenes...

from times import epochTime # for testing
from bitops import popCount

type Prime = uint64

let LIMIT = 1_000_000_000.Prime
let CPUL1CACHE = 16384 # in bytes

const FRSTSVPRM = 3.Prime

type
  BasePrime = uint32
  BasePrimeArray = seq[BasePrime]
  SieveBuffer = seq[byte] # byte size gives the most potential efficiency...

# define a general purpose lazy list to use as secondary base prime arrays feed
# NOT thread safe; needs a Mutex gate to make it so, but not threaded (yet)...
type
  BasePrimeArrayLazyList = ref object
    head: BasePrimeArray
    tailf: proc (): BasePrimeArrayLazyList {.closure.}
    tail: BasePrimeArrayLazyList
template makeBasePrimeArrayLazyList(hd: BasePrimeArray;
                      body: untyped): untyped = # factory constructor
  let thnk = proc (): BasePrimeArrayLazyList {.closure.} = body
  BasePrimeArrayLazyList(head: hd, tailf: thnk)
proc rest(lzylst: sink BasePrimeArrayLazyList): BasePrimeArrayLazyList {.inline.} =
  if lzylst.tailf != nil: lzylst.tail = lzylst.tailf(); lzylst.tailf = nil
  return lzylst.tail
iterator items(lzylst: BasePrimeArrayLazyList): BasePrime {.inline.} =
  var ll = lzylst
  while ll != nil:
    for bp in ll.head: yield bp
    ll = ll.rest

# count the number of zero bits (primes) in a SieveBuffer,
# uses native popCount for extreme speed;
# counts up to the bit index of the last bit to be counted...
proc countSieveBuffer(lsti: int; cmpsts: SieveBuffer): int =
  let lstw = (lsti shr 3) and -8; let lstm = lsti and 63 # last word and bit index!
  result = (lstw shl 3) + 64 # preset for all ones!
  let cmpstsa = cast[int](cmpsts[0].unsafeAddr)
  let cmpstslsta = cmpstsa + lstw
  for csa in countup(cmpstsa, cmpstslsta - 1, 8):
    result -= cast[ptr uint64](csa)[].popCount # subtract number of found ones!
  let msk = (0'u64 - 2'u64) shl lstm # mask for the unused bits in last word!
  result -= (cast[ptr uint64](cmpstslsta)[] or msk).popCount

# a fast fill SieveBuffer routine using pointers...
proc fillSieveBuffer(sb: var SieveBuffer) = zeroMem(sb[0].unsafeAddr, sb.len)

const BITMASK = [1'u8, 2, 4, 8, 16, 32, 64, 128] # faster than shifting!

# do sieving work, based on low starting value for the given buffer and
# the given lazy list of base prime arrays...
proc cullSieveBuffer(lwi: int; bpas: BasePrimeArrayLazyList;
                               sb: var SieveBuffer) =
  let len = sb.len; let szbits = len shl 3; let nxti = lwi + szbits
  for bp in bpas:
    let bpwi = ((bp.Prime - FRSTSVPRM) shr 1).int
    var s = (bpwi shl 1) * (bpwi + FRSTSVPRM.int) + FRSTSVPRM.int
    if s >= nxti: break
    if s >= lwi: s -= lwi
    else:
      let r = (lwi - s) mod bp.int
      s = (if r == 0: 0 else: bp.int - r)
    let clmt = szbits - (bp.int shl 3)
#    if len == CPUL1CACHE: continue
    if s < clmt:
      let slmt = s + (bp.int shl 3)
      while s < slmt:
        let msk = BITMASK[s and 7]
        for c in countup(s shr 3, len - 1, bp.int):
          sb[c] = sb[c] or msk
        s += bp.int
      continue
    while s < szbits:
      let w = s shr 3; sb[w] = sb[w] or BITMASK[s and 7]; s += bp.int # (1'u8 shl (s and 7))

proc makeBasePrimeArrays(): BasePrimeArrayLazyList # forward reference!

# an iterator over successive sieved buffer composite arrays,
# returning whatever type the cnvrtr produces from
# the low index and the culled SieveBuffer...
proc makePrimePages[T](
    strtwi, sz: int; cnvrtrf: proc (li: int; sb: var SieveBuffer): T {.closure.}
      ): (iterator(): T {.closure.}) =
  var lwi = strtwi; let bpas = makeBasePrimeArrays(); var cmpsts = newSeq[byte](sz)
  return iterator(): T {.closure.} =
    while true:
      fillSieveBuffer(cmpsts); cullSieveBuffer(lwi, bpas, cmpsts)
      yield cnvrtrf(lwi, cmpsts); lwi += cmpsts.len shl 3

# starts the secondary base primes feed with minimum size in bits set to 4K...
# thus, for the first buffer primes up to 8293,
# the seeded primes easily cover it as 97 squared is 9409.
proc makeBasePrimeArrays(): BasePrimeArrayLazyList =
  # converts an entire sieved array of bytes into an array of base primes,
  # to be used as a source of base primes as part of the Lazy List...
  proc sb2bpa(li: int; sb: var SieveBuffer): BasePrimeArray =
    let szbits = sb.len shl 3; let len = countSieveBuffer(szbits - 1, sb)
    result = newSeq[BasePrime](len); var j = 0
    for i in 0 ..< szbits:
      if (sb[i shr 3] and BITMASK[i and 7]) == 0'u8:
        result[j] = FRSTSVPRM.BasePrime + ((li + i) shl 1).BasePrime; j.inc
  proc nxtbparr(
      pgen: iterator (): BasePrimeArray {.closure.}): BasePrimeArrayLazyList =
    return makeBasePrimeArrayLazyList(pgen()): nxtbparr(pgen)
  # pre-seeding first array breaks recursive race,
  # dummy primes of all odd numbers starting at FRSTSVPRM (unculled)...
  var cmpsts = newSeq[byte](512)
  let dummybparr = sb2bpa(0, cmpsts)
  let fakebps = makeBasePrimeArrayLazyList(dummybparr): nil # used just once here!
  cullSieveBuffer(0, fakebps, cmpsts)
  return makeBasePrimeArrayLazyList(sb2bpa(0, cmpsts)):
    nxtbparr(makePrimePages(4096, 512, sb2bpa)) # lazy recursive call breaks race!

# iterator over primes from above page iterator;
# takes at least as long to enumerate the primes as sieve them...
iterator primesPaged(): Prime {.inline.} =
  yield 2
  proc mkprmarr(li: int; sb: var SieveBuffer): seq[Prime] =
    let szbits = sb.len shl 3; let low = FRSTSVPRM + (li + li).Prime; var j = 0
    let len = countSieveBuffer(szbits - 1, sb); result = newSeq[Prime](len)
    for i in 0 ..< szbits:
      if (sb[i shr 3] and BITMASK[i and 7]) == 0'u8:
        result[j] = low + (i + i).Prime; j.inc
  let gen = makePrimePages(0, CPUL1CACHE, mkprmarr)
  for prmpg in gen():
    for prm in prmpg: yield prm

proc countPrimesTo(range: Prime): int64 =
  if range < FRSTSVPRM: return (if range < 2: 0 else: 1)
  result = 1; let rngi = ((range - FRSTSVPRM) shr 1).int
  proc cntr(li: int; sb: var SieveBuffer): (int, int) {.closure.} =
    let szbits = sb.len shl 3; let nxti = li + szbits; result = (0, nxti)
    if nxti <= rngi: result[0] += countSieveBuffer(szbits - 1, sb)
    else: result[0] += countSieveBuffer(rngi - li, sb)
  let gen = makePrimePages(0, CPUL1CACHE, cntr)
  for count, nxti in gen():
    result += count; if nxti > rngi: break

# showing results...
echo "Page Segmented Bit-Packed Odds-Only Sieve of Eratosthenes"
echo "Needs at least ", CPUL1CACHE, " bytes of CPU L1 cache memory.\n"

stdout.write "First 25 primes:  "
var counter0 = 0
for p in primesPaged():
  if counter0 >= 25: break
  stdout.write(p, " "); counter0.inc
echo ""

stdout.write "The number of primes up to a million is:  "
var counter1 = 0
for p in primesPaged():
  if p > 1_000_000.Prime: break else: counter1.inc
stdout.write counter1, " - these both found by (slower) enumeration.\n"

let start = epochTime()
#[ # slow way to count primes takes as long to enumerate as sieve!
var counter = 0
for p in primesPaged():
  if p > LIMIT: break else: counter.inc
# ]#
let counter = countPrimesTo LIMIT # the fast way using native popCount!
let elpsd = epochTime() - start

echo "Found ", counter, " primes up to ", LIMIT, " in ", elpsd, " seconds."
```

{{output}}

```txt
Page Segmented Bit-Packed Odds-Only Sieve of Eratosthenes
Needs at least 16384 bytes of CPU L1 cache memory.

First 25 primes:  2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97
The number of primes up to a million is:  78498 - these both found by (slower) enumeration.
Found 50847534 primes up to 1000000000 in 1.399795055389404 seconds.
```


The above version approaches a hundred times faster than the incremental style versions above due to the high efficiency of direct mutable memory operations in modern CPU's, and is useful for ranges of billions.  This version maintains its efficiency using the CPU L1 cache to a range of over 16 billion and then gets a little slower for ranges of a trillion or more using the CPU's L2 cache.  It takes an average of only about 3.5 CPU clock cycles per composite number cull, or about 70 CPU clock cycles per prime found.

Note that the fastest performance is realized by using functions that directly manipulate the output "seq" (array) of culled bit number representations such as the `countPrimesTo` function provided, as enumeration using the `primesPaged` iterator takes about as long to enumerate the found primes as it takes to cull the composites.

Many further improvements in speed can be made, as in tuning the medium ranges to more efficiently use the CPU caches for an improvement in the middle ranges of up to a factor of about two, full maximum wheel factorization for a further improvement of about four, extreme loop unrolling for a further improvement of approximately two, multi-threading for an improvement of the factor of effective CPU cores used, etc.  However, these improvements are of little point when used with enumeration; for instance, if one successfully reduced the time to sieve the composite numbers to zero, it would still take about a second just to enumerate the resulting primes over a range of a billion.


## Niue

{{incorrect|Niue|It uses rem testing and so is a trial division algorithm, not a sieve of Eratosthenes.}}

```Niue
[ dup 2 < ] '<2 ;
[ 1 + 'count ; [ <2 [ , ] when ] count times ] 'fill-stack ;

0 'n ; 0 'v ;

[ .clr 0 'n ; 0 'v ; ] 'reset ;
[ len 1 - n - at 'v ; ] 'set-base ;
[ n 1 + 'n ; ] 'incr-n ;
[ mod 0 = ] 'is-factor ;
[ dup * ] 'sqr ;

[ set-base
  v sqr 2 at > not
  [ [ dup v = not swap v is-factor and ] remove-if incr-n run ] when ] 'run ;

[ fill-stack run ] 'sieve ;

( tests )

10 sieve .s ( => 2 3 5 7 9 ) reset newline
30 sieve .s ( => 2 3 5 7 11 13 17 19 23 29 )
```

=={{header|Oberon-2}}==

```oberon2
MODULE Primes;

   IMPORT Out, Math;

   CONST N = 1000;

   VAR a: ARRAY N OF BOOLEAN;
      i, j, m: INTEGER;

BEGIN
   (* Set all elements of a to TRUE. *)
   FOR i := 1 TO N - 1 DO
      a[i] := TRUE;
   END;

   (* Compute square root of N and convert back to INTEGER. *)
   m := ENTIER(Math.Sqrt(N));

   FOR i := 2 TO m DO
      IF a[i] THEN
         FOR j := 2 TO (N - 1) DIV i DO
            a[i*j] := FALSE;
         END;
      END;
   END;

   (* Print all the elements of a that are TRUE. *)
   FOR i := 2 TO N - 1 DO
      IF a[i] THEN
         Out.Int(i, 4);
      END;
   END;
   Out.Ln;
END Primes.
```


## OCaml


### Imperative


```ocaml
let sieve n =
  let is_prime = Array.create n true in
  let limit = truncate(sqrt (float (n - 1))) in
  for i = 2 to limit do
    if is_prime.(i) then
      let j = ref (i*i) in
      while !j < n do
        is_prime.(!j) <- false;
        j := !j + i;
      done
  done;
  is_prime.(0) <- false;
  is_prime.(1) <- false;
  is_prime
```



```ocaml
let primes n =
  let primes, _ =
    let sieve = sieve n in
    Array.fold_right
      (fun is_prime (xs, i) -> if is_prime then (i::xs, i-1) else (xs, i-1))
      sieve
      ([], Array.length sieve - 1)
  in
  primes
```


in the top-level:
 # primes 100 ;;
 - : int list =
 [2; 3; 5; 7; 11; 13; 17; 19; 23; 29; 31; 37; 41; 43; 47; 53; 59; 61; 67; 71;
  73; 79; 83; 89; 97]


### Functional


```ocaml
(* first define some iterators *)
# let fold_iter f init a b =
    let rec aux acc i =
      if i > b
      then (acc)
      else aux (f acc i) (succ i)
    in
    aux init a ;;
val fold_iter : ('a -> int -> 'a) -> 'a -> int -> int -> 'a = <fun>

# let fold_step f init a b step =
    let rec aux acc i =
      if i > b
      then (acc)
      else aux (f acc i) (i + step)
    in
    aux init a ;;
val fold_step : ('a -> int -> 'a) -> 'a -> int -> int -> int -> 'a = <fun>

(* remove a given value from a list *)
# let remove li v =
    let rec aux acc = function
      | hd::tl when hd = v -> (List.rev_append acc tl)
      | hd::tl -> aux (hd::acc) tl
      | [] -> li
    in
    aux [] li ;;
val remove : 'a list -> 'a -> 'a list = <fun>

(* the main function *)
# let primes n =
    let li =
      (* create a list [from 2; ... until n] *)
      List.rev(fold_iter (fun acc i -> (i::acc)) [] 2 n)
    in
    let limit = truncate(sqrt(float n)) in
    fold_iter (fun li i ->
        if List.mem i li  (* test if (i) is prime *)
        then (fold_step remove li (i*i) n i)
        else li)
      li 2 (pred limit)
  ;;
val primes : int -> int list = <fun>

# primes 200 ;;
- : int list =
[2; 3; 5; 7; 11; 13; 17; 19; 23; 29; 31; 37; 41; 43; 47; 53; 59; 61; 67; 71;
 73; 79; 83; 89; 97; 101; 103; 107; 109; 113; 127; 131; 137; 139; 149; 151;
 157; 163; 167; 173; 179; 181; 191; 193; 197; 199]
```



###  Another functional version


This uses zero to denote struck-out numbers. It is slightly inefficient as it strikes-out multiples above p rather than p<sup>2</sup>


```ocaml
# let rec strike_nth k n l = match l with
  | [] -> []
  | h :: t ->
    if k = 0 then 0 :: strike_nth (n-1) n t
    else h :: strike_nth (k-1) n t;;
val strike_nth : int -> int -> int list -> int list = <fun>

# let primes n =
  let limit = truncate(sqrt(float n)) in
  let rec range a b = if a > b then [] else a :: range (a+1) b in
  let rec sieve_primes l = match l with
    | [] -> []
    | 0 :: t -> sieve_primes t
    | h :: t -> if h > limit then List.filter ((<) 0) l else
        h :: sieve_primes (strike_nth (h-1) h t) in
  sieve_primes (range 2 n) ;;
val primes : int -> int list = <fun>

# primes 200;;
- : int list =
[2; 3; 5; 7; 11; 13; 17; 19; 23; 29; 31; 37; 41; 43; 47; 53; 59; 61; 67; 71;
 73; 79; 83; 89; 97; 101; 103; 107; 109; 113; 127; 131; 137; 139; 149; 151;
 157; 163; 167; 173; 179; 181; 191; 193; 197; 199]
```



## Oforth



```Oforth
: eratosthenes(n)
| i j |
   ListBuffer newSize(n) dup add(null) seqFrom(2, n) over addAll
   2 n sqrt asInteger for: i [
      dup at(i) ifNotNull: [ i sq n i step: j [ dup put(j, null) ] ]
      ]
   filter(#notNull) ;
```


{{out}}

```txt

>100 eratosthenes println
[2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97]

```



## Ol


```scheme

(define all (iota 999 2))

(print
   (let main ((left '()) (right all))
      (if (null? right)
         (reverse left)
         (unless (car right)
            (main left (cdr right))
            (let loop ((l '()) (r right) (n 0) (every (car right)))
               (if (null? r)
                  (let ((l (reverse l)))
                     (main (cons (car l) left) (cdr l)))
                  (if (eq? n every)
                     (loop (cons #false l) (cdr r) 1 every)
                     (loop (cons (car r) l) (cdr r) (+ n 1) every)))))))
)

```


Output:

```txt

(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 101 103 107 109 113 127 131 137 139 149 151 157 163 167 173 179 181 191 193 197 199 211 223 227 229 233 239 241 251 257 263 269 271 277 281 283 293 307 311 313 317 331 337 347 349 353 359 367 373 379 383 389 397 401 409 419 421 431 433 439 443 449 457 461 463 467 479 487 491 499 503 509 521 523 541 547 557 563 569 571 577 587 593 599 601 607 613 617 619 631 641 643 647 653 659 661 673 677 683 691 701 709 719 727 733 739 743 751 757 761 769 773 787 797 809 811 821 823 827 829 839 853 857 859 863 877 881 883 887 907 911 919 929 937 941 947 953 967 971 977 983 991 997)

```



## Oz

{{trans|Haskell}}

```oz
declare
  fun {Sieve N}
     S = {Array.new 2 N true}
     M = {Float.toInt {Sqrt {Int.toFloat N}}}
  in
     for I in 2..M do
	if S.I then
	   for J in I*I..N;I do
	      S.J := false
	   end
	end
     end
     S
  end

  fun {Primes N}
     S = {Sieve N}
  in
     for I in 2..N collect:C do
	if S.I then {C I} end
     end
  end
in
  {Show {Primes 30}}
```



## PARI/GP


```parigp
Eratosthenes(lim)={
  my(v=Vectorsmall(lim\1,unused,1));
  forprime(p=2,sqrt(lim),
    forstep(i=p^2,lim,p,
      v[i]=0
    )
  );
  for(i=1,lim,if(v[i],print1(i", ")))
};
```


An alternate version:


```parigp
Sieve(n)=
{
v=vector(n,unused,1);
for(i=2,sqrt(n),
    if(v[i],
       forstep(j=i^2,n,i,v[j]=0)));
for(i=2,n,if(v[i],print1(i)))
};
```



## Pascal

Note: Some Pascal implementations put quite low limits on the size of a set (e.g. Turbo Pascal doesn't allow more than 256 members). To compile on such an implementation, reduce the constant PrimeLimit accordingly.

```pascal

program primes(output)

const
 PrimeLimit = 1000;

var
 primes: set of 1 .. PrimeLimit;
 n, k: integer;
 needcomma: boolean;

begin
 { calculate the primes }
 primes := [2 .. PrimeLimit];
 for n := 1 to trunc(sqrt(PrimeLimit)) do
  begin
   if n in primes
    then
     begin
      k := n*n;
      while k < PrimeLimit do
       begin
        primes := primes - [k];
        k := k + n
       end
     end
  end;

  { output the primes }
  needcomma := false;
  for n := 1 to PrimeLimit do
   if n in primes
    then
     begin
      if needcomma
       then
        write(', ');
      write(n);
      needcomma := true
     end
end.

```


### alternative using wheel

Using growing wheel to fill array for sieving for minimal unmark operations.
Sieving only with possible-prime factors.

```pascal

program prim(output);
//Sieve of Erathosthenes with fast elimination of multiples of small primes
{$IFNDEF FPC}
  {$APPTYPE CONSOLE}
{$ENDIF}
const
  PrimeLimit = 100*1000*1000;//1;
type
  tLimit = 1..PrimeLimit;
var
  //always initialized with 0 => false at startup
  primes: array [tLimit] of boolean;

function BuildWheel: longInt;
//fill primfield with no multiples of small primes
//returns next sieveprime
//speedup ~1/3
var
  //wheelprimes = 2,3,5,7,11... ;
  //wheelsize = product [i= 0..wpno-1]wheelprimes[i] > Uint64 i> 13
  wheelprimes :array[0..13] of byte;
  wheelSize,wpno,
  pr,pw,i, k: LongWord;
begin
  //the mother of all numbers 1 ;-)
  //the first wheel = generator of numbers
  //not divisible by the small primes first found primes
  pr := 1;
  primes[1]:= true;
  WheelSize := 1;

  wpno := 0;
  repeat
    inc(pr);
    //pw = pr projected in wheel of wheelsize
    pw := pr;
    if pw > wheelsize then
      dec(pw,wheelsize);
    If Primes[pw] then
    begin
//      writeln(pr:10,pw:10,wheelsize:16);
      k := WheelSize+1;
      //turn the wheel (pr-1)-times
      for i := 1 to pr-1 do
      begin
        inc(k,WheelSize);
        if k<primeLimit then
          move(primes[1],primes[k-WheelSize],WheelSize)
        else
        begin
          move(primes[1],primes[k-WheelSize],PrimeLimit-WheelSize*i);
          break;
        end;
      end;
      dec(k);
      IF k > primeLimit then
        k := primeLimit;
      wheelPrimes[wpno] := pr;
      primes[pr] := false;

      inc(wpno);
      //the new wheelsize
      WheelSize := k;

      //sieve multiples of the new found prime
      i:= pr;
      i := i*i;
      while i <= k do
      begin
        primes[i] := false;
        inc(i,pr);
      end;
    end;
  until WheelSize >= PrimeLimit;

  //re-insert wheel-primes
  // 1 still stays prime
  while wpno > 0 do
  begin
    dec(wpno);
    primes[wheelPrimes[wpno]] := true;
  end;
  BuildWheel  := pr+1;
end;

procedure Sieve;
var
  sieveprime,
  fakt : LongWord;
begin
//primes[1] = true is needed to stop for sieveprime = 2
// at //Search next smaller possible prime
  sieveprime := BuildWheel;
//alternative here
  //fillchar(primes,SizeOf(Primes),chr(ord(true)));sieveprime := 2;
  repeat
    if primes[sieveprime] then
    begin
      //eliminate 'possible prime' multiples of sieveprime
      //must go downwards
      //2*2 would unmark 4 -> 4*2 = 8 wouldnt be unmarked
      fakt := PrimeLimit DIV sieveprime;
      IF fakt < sieveprime then
        BREAK;
      repeat
        //Unmark
        primes[sieveprime*fakt] := false;
        //Search next smaller possible prime
        repeat
          dec(fakt);
        until primes[fakt];
      until fakt < sieveprime;
    end;
    inc(sieveprime);
  until false;
  //remove 1
  primes[1] := false;
end;

var
  prCnt,
  i : LongWord;
Begin
  Sieve;
  {count the primes }
  prCnt := 0;
  for i:= 1 to PrimeLimit do
    inc(prCnt,Ord(primes[i]));
  writeln(prCnt,' primes up to ',PrimeLimit);
end.
```


output: ( i3 4330 Haswell 3.5 Ghz fpc 2.6.4 -O3 )

```txt
5761455 primes up to 100000000

real	0m0.204s
user	0m0.193s
sys	0m0.013s


```



## Perl


For highest performance and ease, typically a module would be used, such as [https://metacpan.org/pod/Math::Prime::Util Math::Prime::Util], [https://metacpan.org/pod/Math::Prime::FastSieve Math::Prime::FastSieve], or [https://metacpan.org/pod/Math::Prime::XS Math::Prime::XS].


### Classic Sieve


```perl
sub sieve {
  my $n = shift;
  my @composite;
  for my $i (2 .. int(sqrt($n))) {
    if (!$composite[$i]) {
      for (my $j = $i*$i; $j <= $n; $j += $i) {
        $composite[$j] = 1;
      }
    }
  }
  my @primes;
  for my $i (2 .. $n) {
    $composite[$i] || push @primes, $i;
  }
  @primes;
}
```


===Odds only (faster)===

```perl
sub sieve2 {
  my($n) = @_;
  return @{([],[],[2],[2,3],[2,3])[$n]} if $n <= 4;

  my @composite;
  for (my $t = 3;  $t*$t <= $n;  $t += 2) {
     if (!$composite[$t]) {
        for (my $s = $t*$t;  $s <= $n;  $s += $t*2)
           { $composite[$s]++ }
     }
  }
  my @primes = (2);
  for (my $t = 3;  $t <= $n;  $t += 2) {
     $composite[$t] || push @primes, $t;
  }
  @primes;
}
```


===Odds only, using vectors for lower memory use===

```perl
sub dj_vector {
  my($end) = @_;
  return @{([],[],[2],[2,3],[2,3])[$end]} if $end <= 4;
  $end-- if ($end & 1) == 0; # Ensure end is odd

  my ($sieve, $n, $limit, $s_end) = ( '', 3, int(sqrt($end)), $end >> 1 );
  while ( $n <= $limit ) {
    for (my $s = ($n*$n) >> 1; $s <= $s_end; $s += $n) {
      vec($sieve, $s, 1) = 1;
    }
    do { $n += 2 } while vec($sieve, $n >> 1, 1) != 0;
  }
  my @primes = (2);
  do { push @primes, 2*$_+1 if !vec($sieve,$_,1) } for (1..int(($end-1)/2));
  @primes;
}
```


===Odds only, using strings for best performance===
Compared to array versions, about 2x faster (with 5.16.0 or later) and lower memory.  Much faster than the experimental versions below.  It's possible a mod-6 or mod-30 wheel could give more improvement, though possibly with obfuscation.  The best next step for performance and functionality would be segmenting.

```perl
sub string_sieve {
  my ($n, $i, $s, $d, @primes) = (shift, 7);

  local $_ = '110010101110101110101110111110' .
             '101111101110101110101110111110' x ($n/30);

  until (($s = $i*$i) > $n) {
    $d = $i<<1;
    do { substr($_, $s, 1, '1') } until ($s += $d) > $n;
    1 while substr($_, $i += 2, 1);
  }
  $_ = substr($_, 1, $n);
  # For just the count:  return ($_ =~ tr/0//);
  push @primes, pos while m/0/g;
  @primes;
}
```


This older version uses half the memory, but at the expense of a bit of speed and code complexity:

```perl
sub dj_string {
  my($end) = @_;
  return @{([],[],[2],[2,3],[2,3])[$end]} if $end <= 4;
  $end-- if ($end & 1) == 0;
  my $s_end = $end >> 1;

  my $whole = int( ($end>>1) / 15);    # prefill with 3 and 5 marked
  my $sieve = '100010010010110' . '011010010010110' x $whole;
  substr($sieve, ($end>>1)+1) = '';
  my ($n, $limit, $s) = ( 7, int(sqrt($end)), 0 );
  while ( $n <= $limit ) {
    for ($s = ($n*$n) >> 1; $s <= $s_end; $s += $n) {
      substr($sieve, $s, 1) = '1';
    }
    do { $n += 2 } while substr($sieve, $n>>1, 1);
  }
  # If you just want the count, it's very fast:
  #       my $count = 1 + $sieve =~ tr/0//;
  my @primes = (2);
  push @primes, 2*pos($sieve)-1 while $sieve =~ m/0/g;
  @primes;
}
```



### Experimental

These are examples of golfing or unusual styles.

Golfing a bit, at the expense of speed:

```perl
sub sieve{ my (@s, $i);
	grep { not $s[ $i  = $_ ] and do
		 { $s[ $i += $_ ]++ while $i <= $_[0]; 1 }
	} 2..$_[0]
}

print join ", " => sieve 100;
```


Or with bit strings (much slower than the vector version above):

```perl
sub sieve{ my ($s, $i);
	grep { not vec $s, $i  = $_, 1 and do
		{ (vec $s, $i += $_, 1) = 1 while $i <= $_[0]; 1 }
	} 2..$_[0]
}

print join ", " => sieve 100;
```


A short recursive version:

```perl
sub erat {
    my $p = shift;
    return $p, $p**2 > $_[$#_] ? @_ : erat(grep $_%$p, @_)
}

print join ', ' => erat 2..100000;
```


Regexp (purely an example -- the regex engine limits it to only 32769):
```perl
sub sieve {
	my ($s, $p) = "." . ("x" x shift);

	1 while ++$p
		and $s =~ /^(.{$p,}?)x/g
		and $p = length($1)
		and $s =~ s/(.{$p})./${1}./g
		and substr($s, $p, 1) = "x";
	$s
}

print sieve(1000);
```



### Extensible sieves


Here are two incremental versions, which allows one to create a tied array of primes:

```perl
use strict;
use warnings;
package Tie::SieveOfEratosthenes;

sub TIEARRAY {
	my $class = shift;
	bless \$class, $class;
}

# If set to true, produces copious output.  Observing this output
# is an excellent way to gain insight into how the algorithm works.
use constant DEBUG => 0;

# If set to true, causes the code to skip over even numbers,
# improving runtime.  It does not alter the output content, only the speed.
use constant WHEEL2 => 0;

BEGIN {

	# This is loosely based on the Python implementation of this task,
	# specifically the "Infinite generator with a faster algorithm"

	my @primes = (2, 3);
	my $ps = WHEEL2 ? 1 : 0;
	my $p = $primes[$ps];
	my $q = $p*$p;
	my $incr = WHEEL2 ? 2 : 1;
	my $candidate = $primes[-1] + $incr;
	my %sieve;

	print "Initial: p = $p, q = $q, candidate = $candidate\n" if DEBUG;

	sub FETCH {
		my $n = pop;
		return if $n < 0;
		return $primes[$n] if $n <= $#primes;
		OUTER: while( 1 ) {

			# each key in %sieve is a composite number between
			# p and p-squared.  Each value in %sieve is $incr x the prime
			# which acted as a 'seed' for that key.  We use the value
			# to step through multiples of the seed-prime, until we find
			# an empty slot in %sieve.
			while( my $s = delete $sieve{$candidate} ) {
				print "$candidate a multiple of ".($s/$incr).";\t\t" if DEBUG;
				my $composite = $candidate + $s;
				$composite += $s while exists $sieve{$composite};
				print "The next stored multiple of ".($s/$incr)." is $composite\n" if DEBUG;
				$sieve{$composite} = $s;
				$candidate += $incr;
			}

			print "Candidate $candidate is not in sieve\n" if DEBUG;

			while( $candidate < $q ) {
				print "$candidate is prime\n" if DEBUG;
				push @primes, $candidate;
				$candidate += $incr;
				next OUTER if exists $sieve{$candidate};
			}

			die "Candidate = $candidate, p = $p, q = $q" if $candidate > $q;
			print "Candidate $candidate is equal to $p squared;\t" if DEBUG;

			# Thus, it is now time to add p to the sieve,
			my $step = $incr * $p;
			my $composite = $q + $step;
			$composite += $step while exists $sieve{$composite};
			print "The next multiple of $p is $composite\n" if DEBUG;
			$sieve{$composite} = $step;

			# and fetch out a new value for p from our primes array.
			$p = $primes[++$ps];
			$q = $p * $p;

			# And since $candidate was equal to some prime squared,
			# it's obviously composite, and we need to increment it.
			$candidate += $incr;
			print "p is $p, q is $q, candidate is $candidate\n" if DEBUG;
		} continue {
			return $primes[$n] if $n <= $#primes;
		}
	}

}

if( !caller ) {
	tie my (@prime_list), 'Tie::SieveOfEratosthenes';
	my $limit = $ARGV[0] || 100;
	my $line = "";
	for( my $count = 0; $prime_list[$count] < $limit; ++$count ) {
		$line .= $prime_list[$count]. ", ";
		next if length($line) <= 70;
		if( $line =~ tr/,// > 1 ) {
			$line =~ s/^(.*,) (.*, )/$2/;
			print $1, "\n";
		} else {
			print $line, "\n";
			$line = "";
		}
	}
	$line =~ s/, \z//;
	print $line, "\n" if $line;
}

1;
```

This one is based on the vector sieve shown earlier, but adds to a list as needed, just sieving in the segment.  Slightly faster and half the memory vs. the previous incremental sieve.  It uses the same API -- arguably we should be offset by one so $primes[$n] returns the $n'th prime.

```perl
use strict;
use warnings;
package Tie::SieveOfEratosthenes;

sub TIEARRAY {
  my $class = shift;
  my @primes = (2,3,5,7);
  return bless \@primes, $class;
}

sub prextend { # Extend the given list of primes using a segment sieve
  my($primes, $to) = @_;
  $to-- unless $to & 1; # Ensure end is odd
  return if $to < $primes->[-1];
  my $sqrtn = int(sqrt($to)+0.001);
  prextend($primes, $sqrtn) if $primes->[-1] < $sqrtn;
  my($segment, $startp) = ('', $primes->[-1]+1);
  my($s_beg, $s_len) = ($startp >> 1, ($to>>1) - ($startp>>1));
  for my $p (@$primes) {
    last if $p > $sqrtn;
    if ($p >= 3) {
      my $p2 = $p*$p;
      if ($p2 < $startp) {   # Bump up to next odd multiple of p >= startp
        my $f = 1+int(($startp-1)/$p);
        $p2 = $p * ($f | 1);
      }
      for (my $s = ($p2>>1)-$s_beg; $s <= $s_len; $s += $p) {
        vec($segment, $s, 1) = 1;   # Mark composites in segment
      }
    }
  }
  # Now add all the primes found in the segment to the list
  do { push @$primes, 1+2*($_+$s_beg) if !vec($segment,$_,1) } for 0 .. $s_len;
}

sub FETCHSIZE { 0x7FFF_FFFF }  # Allows foreach to work
sub FETCH {
  my($primes, $n) = @_;
  return if $n < 0;
  # Keep expanding the list as necessary, 5% larger each time.
  prextend($primes, 1000+int(1.05*$primes->[-1])) while $n > $#$primes;
  return $primes->[$n];
}

if( !caller ) {
  tie my @prime_list, 'Tie::SieveOfEratosthenes';
  my $limit = $ARGV[0] || 100;
  print $prime_list[0];
  my $i = 1;
  while ($prime_list[$i] < $limit) { print " ", $prime_list[$i++]; }
  print "\n";
}

1;
```



## Perl 6



```perl6
sub sieve( Int $limit ) {
    my @is-prime = False, False, slip True xx $limit - 1;

    gather for @is-prime.kv -> $number, $is-prime {
        if $is-prime {
            take $number;
            loop (my $s = $number**2; $s <= $limit; $s += $number) {
                @is-prime[$s] = False;
            }
        }
    }
}

(sieve 100).join(",").say;
```


=== A set-based approach ===

More or less the same as the first Python example:

```perl6
sub eratsieve($n) {
    # Requires n(1 - 1/(log(n-1))) storage
    my $multiples = set();
    gather for 2..$n -> $i {
        unless $i (&) $multiples { # is subset
            take $i;
            $multiples (+)= set($i**2, *+$i ... (* > $n)); # union
        }
    }
}

say flat eratsieve(100);
```

This gives:

  (2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97)


###  Using a chain of filters


{{incorrect|Perl6|This version uses modulo (division) testing and so is a trial division algorithm, not a sieve of Eratosthenes.}}


```perl6
sub primes ( UInt $n ) {
    gather {
        # create an iterator from 2 to $n (inclusive)
        my $iterator := (2..$n).iterator;

        loop {
            # If it passed all of the filters it must be prime
            my $prime := $iterator.pull-one;
            # unless it is actually the end of the sequence
            last if $prime =:= IterationEnd;

            take $prime; # add the prime to the `gather` sequence

            # filter out the factors of the current prime
            $iterator := Seq.new($iterator).grep(* % $prime).iterator;
            # (2..*).grep(* % 2).grep(* % 3).grep(* % 5).grep(* % 7)…
        }
    }
}

put primes( 100 );
```

Which prints

 2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97


## Phix

{{Trans|Euphoria}}

```Phix
constant limit = 1000
sequence primes = {}
sequence flags = repeat(1, limit)
for i=2 to floor(sqrt(limit)) do
    if flags[i] then
        for k=i*i to limit by i do
            flags[k] = 0
        end for
    end if
end for
for i=2 to limit do
    if flags[i] then
        primes &= i
    end if
end for
? primes
```

{{out}}

```txt

{2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97,101,103,107,109,113,127,131,137,139,149,151,157,163,167,173,
179,181,191,193,197,199,211,223,227,229,233,239,241,251,257,263,269,271,277,281,283,293,307,311,313,317,331,337,347,349,353,359,367,
373,379,383,389,397,401,409,419,421,431,433,439,443,449,457,461,463,467,479,487,491,499,503,509,521,523,541,547,557,563,569,571,577,
587,593,599,601,607,613,617,619,631,641,643,647,653,659,661,673,677,683,691,701,709,719,727,733,739,743,751,757,761,769,773,787,797,
809,811,821,823,827,829,839,853,857,859,863,877,881,883,887,907,911,919,929,937,941,947,953,967,971,977,983,991,997}

```

See also [[Sexy_primes#Phix]] where the sieve is more useful than a list of primes, and [[Extensible_prime_generator#Phix]] for
a more memory efficient and therefore often faster and more appropriate method.


## PHP


```php

function iprimes_upto($limit)
{
    for ($i = 2; $i < $limit; $i++)
    {
	$primes[$i] = true;
    }

    for ($n = 2; $n < $limit; $n++)
    {
	if ($primes[$n])
	{
	    for ($i = $n*$n; $i < $limit; $i += $n)
	    {
		$primes[$i] = false;
	    }
	}
    }

    return $primes;
}

```



## PicoLisp


```PicoLisp
(de sieve (N)
   (let Sieve (range 1 N)
      (set Sieve)
      (for I (cdr Sieve)
         (when I
            (for (S (nth Sieve (* I I)) S (nth (cdr S) I))
               (set S) ) ) )
      (filter bool Sieve) ) )
```

Output:

```txt
: (sieve 100)
-> (2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97)
```



## PL/I


```pli
eratos: proc options (main) reorder;

dcl i  fixed bin (31);
dcl j  fixed bin (31);
dcl n  fixed bin (31);
dcl sn fixed bin (31);

dcl hbound builtin;
dcl sqrt   builtin;

dcl sysin    file;
dcl sysprint file;

get list (n);
sn = sqrt(n);

begin;
  dcl primes(n) bit (1) aligned init ((*)((1)'1'b));

  i = 2;

  do while(i <= sn);
    do j = i ** 2 by i to hbound(primes, 1);
      /* Adding a test would just slow down processing! */
      primes(j) = '0'b;
     end;

    do i = i + 1 to sn until(primes(i));
    end;
  end;

  do i = 2 to hbound(primes, 1);
    if primes(i) then
      put data(i);
  end;
end;
end eratos;
```



## Pony


```pony
use "time" // for testing
use "collections"

class Primes is Iterator[U32] // returns an Iterator of found primes...
  let _bitmask: Array[U8] = [ 1; 2; 4; 8; 16; 32; 64; 128 ]
  var _lmt: USize
  let _cmpsts: Array[U8]
  var _ndx: USize = 2
  var _curr: U32 = 2

  new create(limit: U32) ? =>
    _lmt = USize.from[U32](limit)
    let sqrtlmt = USize.from[F64](F64.from[U32](limit).sqrt())
    _cmpsts = Array[U8].init(0, (_lmt + 8) / 8) // already zeroed; bit array
    _cmpsts(0)? = 3 // mark 0 and 1 as not prime!
    if sqrtlmt < 2 then return end
    for p in Range[USize](2, sqrtlmt + 1) do
      if (_cmpsts(p >> 3)? and _bitmask(p and 7)?) == 0 then
        var s = p * p // cull start address for p * p!
        let slmt = (s + (p << 3)).min(_lmt + 1)
        while s < slmt do
          let msk = _bitmask(s and 7)?
          var c = s >> 3
          while c < _cmpsts.size() do
            _cmpsts(c)? = _cmpsts(c)? or msk
            c = c + p
          end
          s = s + p
        end
      end
    end

  fun ref has_next(): Bool val => _ndx < (_lmt + 1)

  fun ref next(): U32 ? =>
    _curr = U32.from[USize](_ndx); _ndx = _ndx + 1
    while (_ndx <= _lmt) and ((_cmpsts(_ndx >> 3)? and _bitmask(_ndx and 7)?) != 0) do
      _ndx = _ndx + 1
    end
    _curr

actor Main
  new create(env: Env) =>
    let limit: U32 = 1_000_000_000
    try
      env.out.write("Primes to 100:  ")
      for p in Primes(100)? do env.out.write(p.string() + " ") end
      var count: I32 = 0
      for p in Primes(1_000_000)? do count = count + 1 end
      env.out.print("\nThere are " + count.string() + " primes to a million.")
      let t = Time
      let start = t.millis()
      let prms = Primes(limit)?
      let elpsd = t.millis() - start
      count = 0
      for _ in prms do count = count + 1 end
      env.out.print("Found " + count.string() + " primes to " + limit.string() + ".")
      env.out.print("This took " + elpsd.string() + " milliseconds.")
    end
```

{{out}}

```txt
Primes to 100:  2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97
There are 78498 primes to a million.
Found 50847534 primes to 1000000000.
This took 28123 milliseconds.
```


Note to users:  a naive monolithic sieve (one huge array) isn't really the way to implement this for other than trivial usage in sieving ranges to a few millions as cache locality becomes a very large problem as the size of the array (even bit packed with one bit per number representation as here) limits the maximum range that can be sieved and the "cache thrashing" limits the speed.

For extended ranges, a Page Segmented version should be used.  As well, for any extended ranges in the billions, it is a waste of available computer resources to not use the multi-threading available in a modern CPU, at which Pony would do very well with its built-in Actor concurrency model.

These versions use "loop unpeeling" (not full loop unrolling), which recognizes the repeating modulo pattern of masking the bytes by the base primes less than the square root of the limit so that an "unpeeling" by eight loops can cull by a constant bit mask over the whole range.  For smaller ranges where the speed is not limited by "cache thrashing", this can provide about a factor-of-two speed-up.

===Alternate Odds-Only version of the above===

It is a waste not to do the trivial changes to the above code to sieve odds-only, which is about two and a half times faster due to the decreased number of culling operations; it doesn't really do much about the huge array problem though, other than to reduce it by a factor of two.


```pony
use "time" // for testing
use "collections"

class Primes is Iterator[U32] // returns an Iterator of found primes...
  let _bitmask: Array[U8] = [ 1; 2; 4; 8; 16; 32; 64; 128 ]
  var _lmti: USize
  let _cmpsts: Array[U8]
  var _ndx: USize = 0
  var _curr: U32 = 0

  new create(limit: U32) ? =>
    if limit < 3 then _lmti = 0; _cmpsts = Array[U8](); return end
    _lmti = USize.from[U32]((limit - 3) / 2)
    let sqrtlmti = (USize.from[F64](F64.from[U32](limit).sqrt()) - 3) / 2
    _cmpsts = Array[U8].init(0, (_lmti + 8) / 8) // already zeroed; bit array
    for i in Range[USize](0, sqrtlmti + 1) do
      if (_cmpsts(i >> 3)? and _bitmask(i and 7)?) == 0 then
        let p = i + i + 3
        var s = ((i << 1) * (i + 3)) + 3 // cull start address for p * p!
        let slmt = (s + (p << 3)).min(_lmti + 1)
        while s < slmt do
          let msk = _bitmask(s and 7)?
          var c = s >> 3
          while c < _cmpsts.size() do
            _cmpsts(c)? = _cmpsts(c)? or msk
            c = c + p
          end
          s = s + p
        end
      end
    end

  fun ref has_next(): Bool val => _ndx < (_lmti + 1)

  fun ref next(): U32 ? =>
    if _curr < 1 then _curr = 3; if _lmti == 0 then _ndx = 1 end; return 2 end
    _curr = U32.from[USize](_ndx + _ndx + 3); _ndx = _ndx + 1
    while (_ndx <= _lmti) and ((_cmpsts(_ndx >> 3)? and _bitmask(_ndx and 7)?) != 0) do
      _ndx = _ndx + 1
    end
    _curr

actor Main
  new create(env: Env) =>
    let limit: U32 = 1_000_000_000
    try
      env.out.write("Primes to 100:  ")
      for p in Primes(100)? do env.out.write(p.string() + " ") end
      var count: I32 = 0
      for p in Primes(1_000_000)? do count = count + 1 end
      env.out.print("\nThere are " + count.string() + " primes to a million.")
      let t = Time
      let start = t.millis()
      let prms = Primes(limit)?
      let elpsd = t.millis() - start
      count = 0
      for _ in prms do count = count + 1 end
      env.out.print("Found " + count.string() + " primes to " + limit.string() + ".")
      env.out.print("This took " + elpsd.string() + " milliseconds.")
    end
```

The output is the same as the above except that it is about two and a half times faster due to that many less culling operations.


## Pop11


```txt

define eratostenes(n);
lvars bits = inits(n), i, j;
for i from 2 to n do
   if bits(i) = 0 then
      printf('' >< i, '%s\n');
      for j from 2*i by i to n do
         1 -> bits(j);
      endfor;
   endif;
endfor;
enddefine;

```



## PowerShell



### Basic procedure

It outputs immediately so that the number can be used by the pipeline.

```PowerShell
function Sieve ( [int] $num )
{
    $isprime = @{}
    2..$num | Where-Object {
        $isprime[$_] -eq $null } | ForEach-Object {
        $_
        $isprime[$_] = $true
        $i=$_*$_
        for ( ; $i -le $num; $i += $_ )
        { $isprime[$i] = $false }
    }
}
```


### Another implementation


```PowerShell

function eratosthenes ($n) {
    if($n -ge 1){
        $prime = @(1..($n+1) | foreach{$true})
        $prime[1] = $false
        $m = [Math]::Floor([Math]::Sqrt($n))
        for($i = 2; $i -le $m; $i++) {
            if($prime[$i]) {
                for($j = $i*$i; $j -le $n; $j += $i) {
                    $prime[$j] = $false
                }
            }
        }
        1..$n | where{$prime[$_]}
    } else {
        Write-Warning "$n is less than 1"
    }
}
"$(eratosthenes 100)"

```

<b>Output:</b>

```txt

2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97

```



## Processing

Calculate the primes up to 1000000 with Processing, including a visualisation of the process. As an additional visual effect, the layout of the pixel could be changed from the line-by-line layout to a spiral-like layout starting in the middle of the screen.

```java
int maxx,maxy;
int max;
boolean[] sieve;

void plot(int pos, boolean active) {
  set(pos%maxx,pos/maxx, active?#000000:#ffffff);
}

void setup() {
  size(1000, 1000, P2D);
  frameRate(2);
  maxx=width;
  maxy=height;
  max=width*height;
  sieve=new boolean[max+1];

  sieve[1]=false;
  plot(0,false);
  plot(1,false);
  for(int i=2;i<=max;i++) {
    sieve[i]=true;
    plot(i,true);
  }
}

int i=2;

void draw() {
  if(!sieve[i]) {
    while(i*i<max && !sieve[i]) {
      i++;
    }
  }
  if(sieve[i]) {
    print(i+" ");
    for(int j=i*i;j<=max;j+=i) {
      if(sieve[j]) {
        sieve[j]=false;
        plot(j,false);
      }
    }
  }
  if(i*i<max) {
    i++;
  } else {
    noLoop();
    println("finished");
  }
}
```



## Prolog


### Using lists


### =Basic bounded sieve=


```Prolog
primes(N, L) :- numlist(2, N, Xs),
	        sieve(Xs, L).

sieve([H|T], [H|X]) :- H2 is H + H,
                       filter(H, H2, T, R),
                       sieve(R, X).
sieve([], []).

filter(_, _, [], []).
filter(H, H2, [H1|T], R) :-
    (   H1 < H2 -> R = [H1|R1], filter(H, H2, T, R1)
    ;   H3 is H2 + H,
        (   H1 =:= H2  ->       filter(H, H3, T, R)
        ;                       filter(H, H3, [H1|T], R) ) ).
```


{{out}}

```txt
 ?- time(( primes(7920,X), length(X,N) )).
% 1,131,127 inferences, 0.109 CPU in 0.125 seconds (88% CPU, 10358239 Lips)
X = [2, 3, 5, 7, 11, 13, 17, 19, 23|...],
N = 1000 .

```


====Basic bounded Euler's sieve====

{{trans|Erlang Canonical}}

This is actually the Euler's variant of the sieve of Eratosthenes, generating (and thus removing) each multiple only once, though a sub-optimal implementation.


```Prolog
primes(X, PS) :- X > 1, range(2, X, R), sieve(R, PS).

range(X, X, [X]) :- !.
range(X, Y, [X | R]) :- X < Y, X1 is X + 1, range(X1, Y, R).

mult(A, B, C) :- C is A*B.

sieve([X], [X]) :- !.
sieve([H | T], [H | S]) :- maplist( mult(H), [H | T], MS),
                           remove(MS, T, R), sieve(R, S).

remove( _,       [],      []     ) :- !.
remove( [H | X], [H | Y], R      ) :- !, remove(X, Y, R).
remove( X,       [H | Y], [H | R]) :- remove(X, Y, R).
```


Running in SWI Prolog,

{{out}}

```txt
 ?- time(( primes(7920,X), length(X,N) )).
% 2,087,373 inferences, 0.203 CPU in 0.203 seconds (100% CPU, 10297621 Lips)
X = [2, 3, 5, 7, 11, 13, 17, 19, 23|...],
N = 1000.

```


====Optimized Euler's sieve====
We can stop early, with massive improvement in complexity (below ~ <i>n<sup>1.5</sup></i> inferences, empirically, vs. the ~ <i>n<sup>2</sup></i> of the above, in ''n'' primes produced; showing only the modified predicates):


```Prolog
primes(X, PS) :- X > 1, range(2, X, R), sieve(X, R, PS).

sieve(X, [H | T], [H | T]) :- H*H > X, !.
sieve(X, [H | T], [H | S]) :- maplist( mult(H), [H | T], MS),
                              remove(MS, T, R), sieve(X, R, S).
```


{{out}}

```txt
 ?- time(( primes(7920,X), length(X,N) )).
% 174,437 inferences, 0.016 CPU in 0.016 seconds (100% CPU, 11181787 Lips)
X = [2, 3, 5, 7, 11, 13, 17, 19, 23|...],
N = 1000.

```



### =Bounded sieve=

Optimized by stopping early, traditional sieve of Eratosthenes generating multiples by iterated addition.


```Prolog
primes(X, PS) :- X > 1, range(2, X, R), sieve(X, R, PS).

range(X, X, [X]) :- !.
range(X, Y, [X | R]) :- X < Y, X1 is X + 1, range(X1, Y, R).

sieve(X, [H | T], [H | T]) :- H*H > X, !.
sieve(X, [H | T], [H | S]) :- mults( H, X, MS), remove(MS, T, R), sieve(X, R, S).

mults( H, Lim, MS):- M is H*H, mults( H, M, Lim, MS).
mults( _, M, Lim, []):- M > Lim, !.
mults( H, M, Lim, [M|MS]):- M2 is M+H, mults( H, M2, Lim, MS).

remove( _,       [],      []     ) :- !.
remove( [H | X], [H | Y], R      ) :- !, remove(X, Y, R).
remove( [H | X], [G | Y], R      ) :- H < G, !, remove(X, [G | Y], R).
remove( X,       [H | Y], [H | R]) :- remove(X, Y, R).
```


{{out}}

```txt
 ?- time(( primes(7920,X), length(X,N) )).
% 140,654 inferences, 0.016 CPU in 0.011 seconds (142% CPU, 9016224 Lips)
X = [2, 3, 5, 7, 11, 13, 17, 19, 23|...],
N = 1000.

```



### Using lazy lists


In SWI Prolog and others, where <code>freeze/2</code> is available.


### =Basic variant=



```prolog
primes(PS):- count(2, 1, NS), sieve(NS, PS).

count(N, D, [N|T]):- freeze(T, (N2 is N+D, count(N2, D, T))).

sieve([N|NS],[N|PS]):- N2 is N*N, count(N2,N,A), remove(A,NS,B), freeze(PS, sieve(B,PS)).

take(N, X, A):- length(A, N), append(A, _, X).

remove([A|T],[B|S],R):- A < B -> remove(T,[B|S],R) ;
                        A=:=B -> remove(T,S,R) ;
                        R = [B|R2], freeze(R2, remove([A|T], S, R2)).
```


{{out}}

```txt

 ?- time(( primes(PS), take(1000,PS,R1), length(R,10), append(_,R,R1), writeln(R), false )).
[7841,7853,7867,7873,7877,7879,7883,7901,7907,7919]
% 8,464,518 inferences, 0.702 CPU in 0.697 seconds (101% CPU, 12057641 Lips)
false.

```



### =Optimized by postponed removal=

Showing only changed predicates.

```prolog
primes([2|PS]):-
    freeze(PS, (primes(BPS), count(3, 1, NS), sieve(NS, BPS, 4, PS))).

sieve([N|NS], BPS, Q, PS):-
    N < Q -> PS = [N|PS2], freeze(PS2, sieve(NS, BPS, Q, PS2))
    ;  BPS = [BP,BP2|BPS2], Q2 is BP2*BP2, count(Q, BP, MS),
       remove(MS, NS, R), sieve(R, [BP2|BPS2], Q2, PS).
```


{{out}}

```txt

 ?- time(( primes(PS), take(1000,PS,R1), length(R,10), append(_,R,R1), writeln(R), false )).
[7841,7853,7867,7873,7877,7879,7883,7901,7907,7919]
% 697,727 inferences, 0.078 CPU in 0.078 seconds (100% CPU, 8945161 Lips)
false.       %% odds only: 487,441 inferences

```



###  Using facts to record composite numbers

The first two solutions use Prolog "facts" to record the composite (i.e. already-visited) numbers.

==== Elementary approach: multiplication-free, division-free, mod-free, and cut-free====
The basic Eratosthenes sieve depends on nothing more complex than counting.
In celebration of this simplicity, the first approach to the problem taken here is
free of multiplication and division, as well as Prolog's non-logical "cut".

It defines the predicate between/4 to avoid division, and composite/1
to record integers that are found to be composite.


```Prolog
% %sieve( +N, -Primes ) is true if Primes is the list of consecutive primes
% that are less than or equal to N
sieve( N, [2|Rest]) :-
  retractall( composite(_) ),
  sieve( N, 2, Rest ) -> true.  % only one solution

% sieve P, find the next non-prime, and then recurse:
sieve( N, P, [I|Rest] ) :-
  sieve_once(P, N),
  (P = 2 -> P2 is P+1; P2 is P+2),
  between(P2, N, I),
  (composite(I) -> fail; sieve( N, I, Rest )).

% It is OK if there are no more primes less than or equal to N:
sieve( N, P, [] ).

sieve_once(P, N) :-
  forall( between(P, N, P, IP),
          (composite(IP) -> true ; assertz( composite(IP) )) ).


% To avoid division, we use the iterator
% between(+Min, +Max, +By, -I)
% where we assume that By > 0
% This is like "for(I=Min; I <= Max; I+=By)" in C.
between(Min, Max, By, I) :-
  Min =< Max,
  A is Min + By,
  (I = Min; between(A, Max, By, I) ).


% Some Prolog implementations require the dynamic predicates be
%  declared:

:- dynamic( composite/1 ).

```

The above has been tested with SWI-Prolog and gprolog.


```Prolog
% SWI-Prolog:

?- time( (sieve(100000,P), length(P,N), writeln(N), last(P, LP), writeln(LP) )).
% 1,323,159 inferences, 0.862 CPU in 0.921 seconds (94% CPU, 1534724 Lips)
P = [2, 3, 5, 7, 11, 13, 17, 19, 23|...],
N = 9592,
LP = 99991.

```



### = Optimized approach=

[http://ideone.com/WDC7z Works with SWI-Prolog].


```Prolog
sieve(N, [2|PS]) :-       % PS is list of odd primes up to N
    retractall(mult(_)),
    sieve_O(3,N,PS).

sieve_O(I,N,PS) :-        % sieve odds from I up to N to get PS
    I =< N, !, I1 is I+2,
    (   mult(I) -> sieve_O(I1,N,PS)
    ;   (   I =< N / I ->
            ISq is I*I, DI  is 2*I, add_mults(DI,ISq,N)
        ;   true
        ),
        PS = [I|T],
        sieve_O(I1,N,T)
    ).
sieve_O(I,N,[]) :- I > N.

add_mults(DI,I,N) :-
    I =< N, !,
    ( mult(I) -> true ; assert(mult(I)) ),
    I1 is I+DI,
    add_mults(DI,I1,N).
add_mults(_,I,N) :- I > N.

main(N) :- current_prolog_flag(verbose,F),
  set_prolog_flag(verbose,normal),
  time( sieve( N,P)), length(P,Len), last(P, LP), writeln([Len,LP]),
  set_prolog_flag(verbose,F).

:- dynamic( mult/1 ).
:- main(100000), main(1000000).
```


Running it produces


```Prolog
%% stdout copy
[9592, 99991]
[78498, 999983]

%% stderr copy
% 293,176 inferences, 0.14 CPU in 0.14 seconds (101% CPU, 2094114 Lips)
% 3,122,303 inferences, 1.63 CPU in 1.67 seconds (97% CPU, 1915523 Lips)
```


which indicates <i>~ N<sup>1.1</sup></i> [http://en.wikipedia.org/wiki/Analysis_of_algorithms#Empirical_orders_of_growth empirical orders of growth], which is consistent with the ''O(N log log N)'' theoretical runtime complexity.


###  Using a priority queue


Uses a ariority queue, from the paper "The Genuine Sieve of Eratosthenes" by Melissa O'Neill.  Works with YAP (Yet Another Prolog)


```Prolog
?- use_module(library(heaps)).

prime(2).
prime(N) :- prime_heap(N, _).

prime_heap(3, H) :- list_to_heap([9-6], H).
prime_heap(N, H) :-
    prime_heap(M, H0), N0 is M + 2,
    next_prime(N0, H0, N, H).

next_prime(N0, H0, N, H) :-
    \+ min_of_heap(H0, N0, _),
    N = N0, Composite is N*N, Skip is N+N,
    add_to_heap(H0, Composite, Skip, H).
next_prime(N0, H0, N, H) :-
    min_of_heap(H0, N0, _),
    adjust_heap(H0, N0, H1), N1 is N0 + 2,
    next_prime(N1, H1, N, H).

adjust_heap(H0, N, H) :-
    min_of_heap(H0, N, _),
    get_from_heap(H0, N, Skip, H1),
    Composite is N + Skip, add_to_heap(H1, Composite, Skip, H2),
    adjust_heap(H2, N, H).
adjust_heap(H, N, H) :-
    \+ min_of_heap(H, N, _).
```



## PureBasic



### Basic procedure


```PureBasic
For n=2 To Sqr(lim)
  If Nums(n)=0
    m=n*n
    While m<=lim
      Nums(m)=1
      m+n
    Wend
  EndIf
Next n
```



### Working example


```PureBasic
Dim Nums.i(0)
Define l, n, m, lim

If OpenConsole()

  ; Ask for the limit to search, get that input and allocate a Array
  Print("Enter limit for this search: ")
  lim=Val(Input())
  ReDim Nums(lim)

  ; Use a basic Sieve of Eratosthenes
  For n=2 To Sqr(lim)
    If Nums(n)=#False
      m=n*n
      While m<=lim
        Nums(m)=#True
        m+n
      Wend
    EndIf
  Next n

  ;Present the result to our user
  PrintN(#CRLF$+"The Prims up to "+Str(lim)+" are;")
  m=0: l=Log10(lim)+1
  For n=2 To lim
    If Nums(n)=#False
      Print(RSet(Str(n),l)+" ")
      m+1
      If m>72/(l+1)
        m=0: PrintN("")
      EndIf
    EndIf
  Next

  Print(#CRLF$+#CRLF$+"Press ENTER to exit"): Input()
  CloseConsole()
EndIf
```


Output may look like;
 Enter limit for this search: 750

 The Prims up to 750 are;
    2    3    5    7   11   13   17   19   23   29   31   37   41   43   47
   53   59   61   67   71   73   79   83   89   97  101  103  107  109  113
  127  131  137  139  149  151  157  163  167  173  179  181  191  193  197
  199  211  223  227  229  233  239  241  251  257  263  269  271  277  281
  283  293  307  311  313  317  331  337  347  349  353  359  367  373  379
  383  389  397  401  409  419  421  431  433  439  443  449  457  461  463
  467  479  487  491  499  503  509  521  523  541  547  557  563  569  571
  577  587  593  599  601  607  613  617  619  631  641  643  647  653  659
  661  673  677  683  691  701  709  719  727  733  739  743

 Press ENTER to exit


## Python

Note that the examples use range instead of xrange for Python 3 and Python 2 compatability, but when using Python 2 xrange is the nearest equivalent to Python 3's implementation of range and should be substituted for ranges with a very large number of items.


### Using set lookup

The version below uses a <tt>set</tt> to store the multiples. <tt>set</tt> objects are much faster (usually O(log n)) than <tt>list</tt>s (O(n)) for checking if a given object is a member. Using the <tt>set.update</tt> method
avoids explicit iteration in the interpreter, giving a further speed improvement.


```python
def eratosthenes2(n):
    multiples = set()
    for i in range(2, n+1):
        if i not in multiples:
            yield i
            multiples.update(range(i*i, n+1, i))

print(list(eratosthenes2(100)))
```



### Using array lookup

The version below uses array lookup to test for primality. The function <tt>primes_upto()</tt> is a straightforward implementation of [http://en.wikipedia.org/wiki/Sieve_of_Eratosthenes#Algorithm Sieve of Eratosthenes]algorithm. It returns prime numbers less than or equal to <tt>limit</tt>.

```python
def primes_upto(limit):
    is_prime = [False] * 2 + [True] * (limit - 1)
    for n in range(int(limit**0.5 + 1.5)): # stop at ``sqrt(limit)``
        if is_prime[n]:
            for i in range(n*n, limit+1, n):
                is_prime[i] = False
    return [i for i, prime in enumerate(is_prime) if prime]
```



### Using generator

The following code may be slightly slower than using the array/list as above, but uses no memory for output:

```python
def iprimes_upto(limit):
    is_prime = [False] * 2 + [True] * (limit - 1)
    for n in xrange(int(limit**0.5 + 1.5)): # stop at ``sqrt(limit)``
        if is_prime[n]:
            for i in range(n * n, limit + 1, n): # start at ``n`` squared
                is_prime[i] = False
    for i in xrange(limit + 1):
        if is_prime[i]: yield i
```
{{out|Example}}
```python>>>
 list(iprimes_upto(15))
[2, 3, 5, 7, 11, 13]
```


===Odds-only version of the array sieve above===
The following code is faster than the above array version using only odd composite operations (for a factor of over two) and because it has been optimized to use slice operations for composite number culling to avoid extra work by the interpreter:

```python
def primes2(limit):
    if limit < 2: return []
    if limit < 3: return [2]
    lmtbf = (limit - 3) // 2
    buf = [True] * (lmtbf + 1)
    for i in range((int(limit ** 0.5) - 3) // 2 + 1):
        if buf[i]:
            p = i + i + 3
            s = p * (i + 1) + i
            buf[s::p] = [False] * ((lmtbf - s) // p + 1)
    return [2] + [i + i + 3 for i, v in enumerate(buf) if v]
```


Note that "range" needs to be changed to "xrange" for maximum speed with Python 2.

===Odds-only version of the generator version above===
The following code is faster than the above generator version using only odd composite operations (for a factor of over two) and because it has been optimized to use slice operations for composite number culling to avoid extra work by the interpreter:


```python
def iprimes2(limit):
    yield 2
    if limit < 3: return
    lmtbf = (limit - 3) // 2
    buf = [True] * (lmtbf + 1)
    for i in range((int(limit ** 0.5) - 3) // 2 + 1):
        if buf[i]:
            p = i + i + 3
            s = p * (i + 1) + i
            buf[s::p] = [False] * ((lmtbf - s) // p + 1)
    for i in range(lmtbf + 1):
        if buf[i]: yield (i + i + 3)
```


Note that this version may actually run slightly faster than the equivalent array version with the advantage that the output doesn't require any memory.

Also note that "range" needs to be changed to "xrange" for maximum speed with Python 2.


### Factorization wheel235 version of the generator version

This uses a 235 factorial wheel for further reductions in operations; the same techniques can be applied to the array version as well; it runs slightly faster and uses slightly less memory as compared to the odds-only algorithms:


```python
def primes235(limit):
    yield 2; yield 3; yield 5
    if limit < 7: return
    modPrms = [7,11,13,17,19,23,29,31]
    gaps = [4,2,4,2,4,6,2,6,4,2,4,2,4,6,2,6] # 2 loops for overflow
    ndxs = [0,0,0,0,1,1,2,2,2,2,3,3,4,4,4,4,5,5,5,5,5,5,6,6,7,7,7,7,7,7]
    lmtbf = (limit + 23) // 30 * 8 - 1 # integral number of wheels rounded up
    lmtsqrt = (int(limit ** 0.5) - 7)
    lmtsqrt = lmtsqrt // 30 * 8 + ndxs[lmtsqrt % 30] # round down on the wheel
    buf = [True] * (lmtbf + 1)
    for i in range(lmtsqrt + 1):
        if buf[i]:
            ci = i & 7; p = 30 * (i >> 3) + modPrms[ci]
            s = p * p - 7; p8 = p << 3
            for j in range(8):
                c = s // 30 * 8 + ndxs[s % 30]
                buf[c::p8] = [False] * ((lmtbf - c) // p8 + 1)
                s += p * gaps[ci]; ci += 1
    for i in range(lmtbf - 6 + (ndxs[(limit - 7) % 30])): # adjust for extras
        if buf[i]: yield (30 * (i >> 3) + modPrms[i & 7])
```


Note:  Much of the time (almost two thirds for this last case for Python 2.7.6) for any of these array/list or generator algorithms is used in the computation and enumeration of the final output in the last line(s), so any slight changes to those lines can greatly affect execution time.  For Python 3 this enumeration is about twice as slow as Python 2 (Python 3.3 slow and 3.4 slower) for an even bigger percentage of time spent just outputting the results.  This slow enumeration means that there is little advantage to versions that use even further wheel factorization, as the composite number culling is a small part of the time to enumerate the results.

If just the count of the number of primes over a range is desired, then converting the functions to prime counting functions by changing the final enumeration lines to "return buf.count(True)" will save a lot of time.

Note that "range" needs to be changed to "xrange" for maximum speed with Python 2 where Python 2's "xrange" is a better choice for very large sieve ranges.
 Timings were done primarily in Python 2 although source is Python 2/3 compatible (shows range and not xrange).


### Using numpy

{{libheader|NumPy}}
Below code adapted from [http://en.literateprograms.org/Sieve_of_Eratosthenes_(Python,_arrays)#simple_implementation literateprograms.org] using [http://numpy.scipy.org/ numpy]

```python
import numpy
def primes_upto2(limit):
    is_prime = numpy.ones(limit + 1, dtype=numpy.bool)
    for n in xrange(2, int(limit**0.5 + 1.5)):
        if is_prime[n]:
            is_prime[n*n::n] = 0
    return numpy.nonzero(is_prime)[0][2:]
```

'''Performance note:''' there is no point to add wheels here, due to execution of <tt>p[n*n::n] = 0</tt> and <tt>nonzero()</tt> takes us almost all time.

Also see [http://rebrained.com/?p=458 Prime numbers and Numpy – Python].


### Using wheels with numpy

Version with wheel based optimization:

```python
from numpy import array, bool_, multiply, nonzero, ones, put, resize
#
def makepattern(smallprimes):
    pattern = ones(multiply.reduce(smallprimes), dtype=bool_)
    pattern[0] = 0
    for p in smallprimes:
        pattern[p::p] = 0
    return pattern
#
def primes_upto3(limit, smallprimes=(2,3,5,7,11)):
    sp = array(smallprimes)
    if limit <= sp.max(): return sp[sp <= limit]
    #
    isprime = resize(makepattern(sp), limit + 1)
    isprime[:2] = 0; put(isprime, sp, 1)
    #
    for n in range(sp.max() + 2, int(limit**0.5 + 1.5), 2):
        if isprime[n]:
            isprime[n*n::n] = 0
    return nonzero(isprime)[0]
```


Examples:

```python>>>
 primes_upto3(10**6, smallprimes=(2,3)) # Wall time: 0.17
array([     2,      3,      5, ..., 999961, 999979, 999983])
>>> primes_upto3(10**7, smallprimes=(2,3))            # Wall time: '''2.13'''
array([      2,       3,       5, ..., 9999971, 9999973, 9999991])
>>> primes_upto3(15)
array([ 2,  3,  5,  7, 11, 13])
>>> primes_upto3(10**7, smallprimes=primes_upto3(15)) # Wall time: '''1.31'''
array([      2,       3,       5, ..., 9999971, 9999973, 9999991])
>>> primes_upto2(10**7)                               # Wall time: '''1.39''' <-- version ''without'' wheels
array([      2,       3,       5, ..., 9999971, 9999973, 9999991])
>>> primes_upto3(10**7)                               # Wall time: '''1.30'''
array([      2,       3,       5, ..., 9999971, 9999973, 9999991])
```

The above-mentioned examples demonstrate that the ''given'' wheel based optimization does not show significant performance gain.


### Infinite generator

A generator that will generate primes indefinitely (perhaps until it runs out of memory). Used as a library [[Extensible prime generator#Python|here]].

{{works with|Python|2.6+, 3.x}}

```python
import heapq

# generates all prime numbers
def sieve():
    # priority queue of the sequences of non-primes
    # the priority queue allows us to get the "next" non-prime quickly
    nonprimes = []

    i = 2
    while True:
        if nonprimes and i == nonprimes[0][0]: # non-prime
            while nonprimes[0][0] == i:
                # for each sequence that generates this number,
                # have it go to the next number (simply add the prime)
                # and re-position it in the priority queue
                x = nonprimes[0]
                x[0] += x[1]
                heapq.heapreplace(nonprimes, x)

        else: # prime
            # insert a 2-element list into the priority queue:
            # [current multiple, prime]
            # the first element allows sorting by value of current multiple
            # we start with i^2
            heapq.heappush(nonprimes, [i*i, i])
            yield i

        i += 1
```

Example:

```txt

>>> foo = sieve()
>>> for i in range(8):
...     print(next(foo))
...
2
3
5
7
11
13
17
19

```



### Infinite generator with a faster algorithm


The adding of each discovered prime's incremental step info to the mapping should be '''''postponed''''' until the prime's ''square'' is seen amongst the candidate numbers, as it is useless before that point. This drastically reduces the space complexity from <i>O(n)</i> to <i>O(sqrt(n/log(n)))</i>, in ''<code>n</code>'' primes produced, and also lowers the run time complexity quite low ([http://ideone.com/VXep9F this test entry in Python 2.7] and [http://ideone.com/muuS4H this test entry in Python 3.x] shows about <i>~ n<sup>1.08</sup></i> [http://en.wikipedia.org/wiki/Analysis_of_algorithms#Empirical_orders_of_growth empirical order of growth] which is very close to the theoretical value of <i>O(n log(n) log(log(n)))</i>, in ''<code>n</code>'' primes produced):
{{works with|Python|2.6+, 3.x}}

```python
def primes():
    yield 2; yield 3; yield 5; yield 7;
    bps = (p for p in primes())             # separate supply of "base" primes (b.p.)
    p = next(bps) and next(bps)             # discard 2, then get 3
    q = p * p                               # 9 - square of next base prime to keep track of,
    sieve = {}                              #                       in the sieve dict
    n = 9                                   # n is the next candidate number
    while True:
        if n not in sieve:                  # n is not a multiple of any of base primes,
            if n < q:                       # below next base prime's square, so
                yield n                     # n is prime
            else:
                p2 = p + p                  # n == p * p: for prime p, add p * p + 2 * p
                sieve[q + p2] = p2          #   to the dict, with 2 * p as the increment step
                p = next(bps); q = p * p    # pull next base prime, and get its square
        else:
            s = sieve.pop(n); nxt = n + s   # n's a multiple of some b.p., find next multiple
            while nxt in sieve: nxt += s    # ensure each entry is unique
            sieve[nxt] = s                  # nxt is next non-marked multiple of this prime
        n += 2                              # work on odds only

import itertools
def primes_up_to(limit):
    return list(itertools.takewhile(lambda p: p <= limit, primes()))
```



### Fast infinite generator using a wheel

Although theoretically over three times faster than odds-only, the following code using a 2/3/5/7 wheel is only about 1.5 times faster than the above odds-only code due to the extra overheads in code complexity.  The [http://ideone.com/LFaRnT test link for Python 2.7] and [http://ideone.com/ZAY0T2 test link for Python 3.x] show about the same empirical order of growth as the odds-only implementation above once the range grows enough so the dict operations become amortized to a constant factor.
{{works with|Python|2.6+, 3.x}}

```python
def primes():
    for p in [2,3,5,7]: yield p                 # base wheel primes
    gaps1 = [ 2,4,2,4,6,2,6,4,2,4,6,6,2,6,4,2,6,4,6,8,4,2,4,2,4,8 ]
    gaps = gaps1 + [ 6,4,6,2,4,6,2,6,6,4,2,4,6,2,6,4,2,4,2,10,2,10 ] # wheel2357
    def wheel_prime_pairs():
        yield (11,0); bps = wheel_prime_pairs() # additional primes supply
        p, pi = next(bps); q = p * p            # adv to get 11 sqr'd is 121 as next square to put
        sieve = {}; n = 13; ni = 1              #   into sieve dict; init cndidate, wheel ndx
        while True:
            if n not in sieve:                  # is not a multiple of previously recorded primes
                if n < q: yield (n, ni)         # n is prime with wheel modulo index
                else:
                    npi = pi + 1                # advance wheel index
                    if npi > 47: npi = 0
                    sieve[q + p * gaps[pi]] = (p, npi) # n == p * p: put next cull position on wheel
                    p, pi = next(bps); q = p * p  # advance next prime and prime square to put
            else:
                s, si = sieve.pop(n)
                nxt = n + s * gaps[si]          # move current cull position up the wheel
                si = si + 1                     # advance wheel index
                if si > 47: si = 0
                while nxt in sieve:             # ensure each entry is unique by wheel
                    nxt += s * gaps[si]
                    si = si + 1                 # advance wheel index
                    if si > 47: si = 0
                sieve[nxt] = (s, si)            # next non-marked multiple of a prime
            nni = ni + 1                        # advance wheel index
            if nni > 47: nni = 0
            n += gaps[ni]; ni = nni             # advance on the wheel
    for p, pi in wheel_prime_pairs(): yield p   # strip out indexes
```


Further gains of about 1.5 times in speed can be made using the same code by only changing the tables and a few constants for a further constant factor gain of about 1.5 times in speed by using a 2/3/5/7/11/13/17 wheel (with the gaps list 92160 elements long) computed for a slight constant overhead time as per the [http://ideone.com/4Ld26g test link for Python 2.7] and [http://ideone.com/72Dmyt test link for Python 3.x].  Further wheel factorization will not really be worth it as the gains will be small (if any and not losses) and the gaps table huge - it is already too big for efficient use by 32-bit Python 3 and the wheel should likely be stopped at 13:

```python
def primes():
    whlPrms = [2,3,5,7,11,13,17]                # base wheel primes
    for p in whlPrms: yield p
    def makeGaps():
        buf = [True] * (3 * 5 * 7 * 11 * 13 * 17 + 1) # all odds plus extra for o/f
        for p in whlPrms:
            if p < 3:
                continue              # no need to handle evens
            strt = (p * p - 19) >> 1            # start position (divided by 2 using shift)
            while strt < 0: strt += p
            buf[strt::p] = [False] * ((len(buf) - strt - 1) // p + 1) # cull for p
        whlPsns = [i + i for i,v in enumerate(buf) if v]
        return [whlPsns[i + 1] - whlPsns[i] for i in range(len(whlPsns) - 1)]
    gaps = makeGaps()                           # big wheel gaps
    def wheel_prime_pairs():
        yield (19,0); bps = wheel_prime_pairs() # additional primes supply
        p, pi = next(bps); q = p * p            # adv to get 11 sqr'd is 121 as next square to put
        sieve = {}; n = 23; ni = 1              #   into sieve dict; init cndidate, wheel ndx
        while True:
            if n not in sieve:                  # is not a multiple of previously recorded primes
                if n < q: yield (n, ni)         # n is prime with wheel modulo index
                else:
                    npi = pi + 1                # advance wheel index
                    if npi > 92159: npi = 0
                    sieve[q + p * gaps[pi]] = (p, npi) # n == p * p: put next cull position on wheel
                    p, pi = next(bps); q = p * p  # advance next prime and prime square to put
            else:
                s, si = sieve.pop(n)
                nxt = n + s * gaps[si]          # move current cull position up the wheel
                si = si + 1                     # advance wheel index
                if si > 92159: si = 0
                while nxt in sieve:             # ensure each entry is unique by wheel
                    nxt += s * gaps[si]
                    si = si + 1                 # advance wheel index
                    if si > 92159: si = 0
                sieve[nxt] = (s, si)            # next non-marked multiple of a prime
            nni = ni + 1                        # advance wheel index
            if nni > 92159: nni = 0
            n += gaps[ni]; ni = nni             # advance on the wheel
    for p, pi in wheel_prime_pairs(): yield p   # strip out indexes

```




### Iterative sieve on unbounded count from 2

See [[Extensible_prime_generator#Python:_Iterative_sieve_on_unbounded_count_from_2| Extensible prime generator: Iterative sieve on unbounded count from 2]]


## R


```r
sieve <- function(n) {
  if (n < 2) return(NULL)
  a <- rep(T, n)
  a[1] <- F
  for(i in seq(n)) {
    if (a[i]) {
      j <- i * i
      if (j > n) return(which(a))
      a[seq(j, n, by=i)] <- F
    }
  }
}

sieve(1000)
```



## Racket


### Imperative versions

Ugly imperative version:

```Racket
#lang racket

(define (sieve n)
  (define non-primes '())
  (define primes '())
  (for ([i (in-range 2 (add1 n))])
    (unless (member i non-primes)
      (set! primes (cons i primes))
      (for ([j (in-range (* i i) (add1 n) i)])
        (set! non-primes (cons j non-primes)))))
  (reverse primes))

(sieve 100)
```


A little nicer, but still imperative:

```Racket
#lang racket
(define (sieve n)
  (define primes (make-vector (add1 n) #t))
  (for* ([i (in-range 2 (add1 n))]
         #:when (vector-ref primes i)
         [j (in-range (* i i) (add1 n) i)])
    (vector-set! primes j #f))
  (for/list ([n (in-range 2 (add1 n))]
             #:when (vector-ref primes n))
    n))
(sieve 100)
```


Imperative version using a bit vector:

```Racket
#lang racket
(require data/bit-vector)
;; Returns a list of prime numbers up to natural number limit
(define (eratosthenes limit)
  (define bv (make-bit-vector (+ limit 1) #f))
  (bit-vector-set! bv 0 #t)
  (bit-vector-set! bv 1 #t)
  (for* ([i (in-range (add1 (sqrt limit)))] #:unless (bit-vector-ref bv i)
         [j (in-range (* 2 i) (bit-vector-length bv) i)])
    (bit-vector-set! bv j #t))
  ;; translate to a list of primes
  (for/list ([i (bit-vector-length bv)] #:unless (bit-vector-ref bv i)) i))
(eratosthenes 100)

```


{{output}}
'(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97)


###  Infinite list of primes Using laziness


These examples use infinite lists (streams) to implement the sieve of Eratosthenes in a functional way, and producing all prime numbers. The following functions are used as a prefix for pieces of code that follow:


```Racket
#lang lazy
(define (ints-from i d) (cons i (ints-from (+ i d) d)))
(define (after n l f)
  (if (< (car l) n) (cons (car l) (after n (cdr l) f)) (f l)))
(define (diff l1 l2)
  (let ([x1 (car l1)] [x2 (car l2)])
    (cond [(< x1 x2) (cons x1 (diff (cdr l1)      l2 ))]
          [(> x1 x2)          (diff      l1  (cdr l2)) ]
          [else               (diff (cdr l1) (cdr l2)) ])))
(define (union l1 l2)        ; union of two lists
  (let ([x1 (car l1)] [x2 (car l2)])
    (cond [(< x1 x2) (cons x1 (union (cdr l1)      l2 ))]
          [(> x1 x2) (cons x2 (union      l1  (cdr l2)))]
          [else      (cons x1 (union (cdr l1) (cdr l2)))])))
```



### = Basic sieve =



```Racket
(define (sieve l)
  (define x (car l))
  (cons x (sieve (diff (cdr l) (ints-from (+ x x) x)))))
(define primes (sieve (ints-from 2 1)))
(!! (take 25 primes))
```


Runs at ~ n^2.1 [http://en.wikipedia.org/wiki/Analysis_of_algorithms#Empirical_orders_of_growth empirically], for ''n <= 1500'' primes produced.


### = With merged composites =

Note that the first number, 2, and its multiples stream <code>(ints-from 4 2)</code> are handled separately to ensure that the non-primes list is never empty, which simplifies the code for <code>union</code> which assumes non-empty infinite lists.


```Racket
(define (sieve l non-primes)
  (let ([x (car l)] [np (car non-primes)])
    (cond [(= x np)     (sieve (cdr l) (cdr  non-primes))]    ; else x < np
          [else (cons x (sieve (cdr l) (union (ints-from (* x x) x)
                                               non-primes)))])))
(define primes (cons 2 (sieve (ints-from 3 1) (ints-from 4 2))))
```



### = Basic sieve Optimized with postponed processing =

Since a prime's multiples that count start from its square, we should only start removing them when we reach that square.

```Racket
(define (sieve l prs)
  (define p (car prs))
  (define q (* p p))
  (after q l (λ(t) (sieve (diff t (ints-from q p)) (cdr prs)))))
(define primes (cons 2 (sieve (ints-from 3 1) primes)))
```


Runs at ~ n^1.4 up to n=10,000. The initial 2 in the self-referential primes definition is needed to prevent a "black hole".


### = Merged composites Optimized with postponed processing =

Since prime's multiples that matter start from its square, we should only add them when we reach that square.


```Racket
(define (composites l q primes)
  (after q l
    (λ(t)
      (let ([p (car primes)] [r (cadr primes)])
        (composites (union t (ints-from q p))   ; q = p*p
                    (* r r) (cdr primes))))))
(define primes (cons 2
                 (diff (ints-from 3 1)
                       (composites (ints-from 4 2) 9 (cdr primes)))))
```


==== Implementation of Richard Bird's algorithm ====

Appears in [http://www.cs.hmc.edu/~oneill/papers/Sieve-JFP.pdf M.O'Neill's paper]. Achieves on its own the proper postponement that is specifically arranged for in the version above (with <code>after</code>), and is yet more efficient, because it folds to the right and so builds the right-leaning structure of merges at run time, where the more frequently-producing streams of multiples appear <i>higher</i> in that structure, so the composite numbers produced by them have less <code>merge</code> nodes to percolate through:


```Racket
(define primes
  (cons 2 (diff (ints-from 3 1)
                (foldr (λ(p r) (define q (* p p))
                               (cons q (union (ints-from (+ q p) p) r)))
                       '() primes))))
```



###  Using threads and channels


Same algorithm as [[#With merged composites|"merged composites" above]] (without the postponement optimization), but now using threads and channels to produce a channel of all prime numbers (similar to newsqueak).  The macro at the top is a convenient wrapper around definitions of channels using a thread that feeds them.


```Racket
#lang racket
(define-syntax (define-thread-loop stx)
  (syntax-case stx ()
    [(_ (name . args) expr ...)
     (with-syntax ([out! (datum->syntax stx 'out!)])
       #'(define (name . args)
           (define out (make-channel))
           (define (out! x) (channel-put out x))
           (thread (λ() (let loop () expr ... (loop))))
           out))]))
(define-thread-loop (ints-from i d) (out! i) (set! i (+ i d)))
(define-thread-loop (merge c1 c2)
  (let loop ([x1 (channel-get c1)] [x2 (channel-get c2)])
    (cond [(> x1 x2) (out! x2) (loop x1 (channel-get c2))]
          [(< x1 x2) (out! x1) (loop (channel-get c1) x2)]
          [else      (out! x1) (loop (channel-get c1) (channel-get c2))])))
(define-thread-loop (sieve l non-primes)
  (let loop ([x (channel-get l)] [np (channel-get non-primes)])
    (cond [(> x np) (loop x (channel-get non-primes))]
          [(= x np) (loop (channel-get l) (channel-get non-primes))]
          [else     (out! x)
                    (set! non-primes (merge (ints-from (* x x) x) non-primes))
                    (loop (channel-get l)  np)])))
(define-thread-loop (cons x l)
  (out! x) (let loop () (out! (channel-get l)) (loop)))
(define primes (cons 2 (sieve (ints-from 3 1) (ints-from 4 2))))
(for/list ([i 25] [x (in-producer channel-get eof primes)]) x)
```



###  Using generators


Yet another variation of the same algorithm as above, this time using generators.


```Racket
#lang racket
(require racket/generator)
(define (ints-from i d)
  (generator () (let loop ([i i]) (yield i) (loop (+ i d)))))
(define (merge g1 g2)
  (generator ()
    (let loop ([x1 (g1)] [x2 (g2)])
      (cond [(< x1 x2) (yield x1) (loop (g1) x2)]
            [(> x1 x2) (yield x2) (loop x1 (g2))]
            [else      (yield x1) (loop (g1) (g2))]))))
(define (sieve l non-primes)
  (generator ()
    (let loop ([x (l)] [np (non-primes)])
      (cond [(> x np) (loop x (non-primes))]
            [(= x np) (loop (l) (non-primes))]
            [else (yield x)
                  (set! non-primes (merge (ints-from (* x x) x) non-primes))
                  (loop (l) np)]))))
(define (cons x l) (generator () (yield x) (let loop () (yield (l)) (loop))))
(define primes (cons 2 (sieve (ints-from 3 1) (ints-from 4 2))))
(for/list ([i 25] [x (in-producer primes)]) x)
```



## REXX


### no wheel version

The first three REXX versions make use of a sparse stemmed array:   ['''@.'''].

As the stemmed array gets heavily populated, the number of entries ''may'' slow down the REXX interpreter substantially,

depending upon the efficacy of the hashing technique being used for REXX variables (setting/retrieving).

```REXX
/*REXX program generates and displays primes  via the  sieve of Eratosthenes  algorithm.*/
parse arg H .;   if H=='' | H==","  then H= 200  /*optain optional argument from the CL.*/
w= length(H);    @prime= right('prime', 20)      /*W:   is used for aligning the output.*/
@.=.                                             /*assume all the numbers are  prime.   */
#= 0                                             /*number of primes found  (so far).    */
     do j=2  for H-1;   if @.j==''  then iterate /*all prime integers up to H inclusive.*/
     #= # + 1                                    /*bump the prime number counter.       */
     say  @prime right(#,w)  " ───► " right(j,w) /*display the  prime  to the terminal. */
         do m=j*j  to H  by j;    @.m=;   end    /*strike all multiples as being ¬ prime*/
     end   /*j*/                                 /*       ───                           */
say                                              /*stick a fork in it,  we're all done. */
say  right(#, 1+w+length(@prime) )     'primes found up to and including '       H
```

'''output'''   when using the input default of:   <tt> 200 </tt>
<pre style="height:45ex">
               prime   1  ───►    2
               prime   2  ───►    3
               prime   3  ───►    5
               prime   4  ───►    7
               prime   5  ───►   11
               prime   6  ───►   13
               prime   7  ───►   17
               prime   8  ───►   19
               prime   9  ───►   23
               prime  10  ───►   29
               prime  11  ───►   31
               prime  12  ───►   37
               prime  13  ───►   41
               prime  14  ───►   43
               prime  15  ───►   47
               prime  16  ───►   53
               prime  17  ───►   59
               prime  18  ───►   61
               prime  19  ───►   67
               prime  20  ───►   71
               prime  21  ───►   73
               prime  22  ───►   79
               prime  23  ───►   83
               prime  24  ───►   89
               prime  25  ───►   97
               prime  26  ───►  101
               prime  27  ───►  103
               prime  28  ───►  107
               prime  29  ───►  109
               prime  30  ───►  113
               prime  31  ───►  127
               prime  32  ───►  131
               prime  33  ───►  137
               prime  34  ───►  139
               prime  35  ───►  149
               prime  36  ───►  151
               prime  37  ───►  157
               prime  38  ───►  163
               prime  39  ───►  167
               prime  40  ───►  173
               prime  41  ───►  179
               prime  42  ───►  181
               prime  43  ───►  191
               prime  44  ───►  193
               prime  45  ───►  197
               prime  46  ───►  199

                      46 primes found up to and including  200

```


===wheel version, optional prime list suppression===
This version skips striking the even numbers   (as being not prime),   '''2'''   is handled as a special case.

Also supported is the suppression of listing the primes if the   '''H'''   ('''h'''igh limit)   is negative.

Also added is a final message indicating the number of primes found.

```rexx
/*REXX program generates primes via a  wheeled  sieve of Eratosthenes  algorithm.       */
parse arg H .;   if H==''  then H=200            /*let the highest number be specified. */
tell=h>0;     H=abs(H);    w=length(H)           /*a negative H suppresses prime listing*/
if 2<=H & tell  then say right(1, w+20)'st prime   ───► '      right(2, w)
@.= '0'x                                         /*assume that  all  numbers are prime. */
cw= length(@.)                                   /*the cell width that holds numbers.   */
#= w<=H                                          /*the number of primes found  (so far).*/
!=0                                              /*skips the top part of sieve marking. */
    do j=3  by 2  for (H-2)%2;  b= j%cw          /*odd integers up to   H   inclusive.  */
    if substr(x2b(c2x(@.b)),j//cw+1,1)  then iterate              /*is  J  composite ?  */
    #= # + 1                                     /*bump the prime number counter.       */
    if tell  then say right(#, w+20)th(#)    'prime   ───► '      right(j, w)
    if !     then iterate                        /*should the top part be skipped ?     */
    jj=j * j                                     /*compute the square of  J.         ___*/
    if jj>H  then !=1                            /*indicates skip top part  if  j > √ H */
      do m=jj  to H  by j+j;   call . m;   end   /* [↑]  strike odd multiples  ¬ prime  */
    end   /*j*/                                  /*             ───                     */

say;             say  right(#, w+20)      'prime's(#)    "found up to and including "  H
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────────────*/
.: parse arg n; b=n%cw; r=n//cw+1;_=x2b(c2x(@.b));@.b=x2c(b2x(left(_,r-1)'1'substr(_,r+1)));return
s: if arg(1)==1  then return arg(3);  return word(arg(2) 's',1)            /*pluralizer.*/
th: procedure; parse arg x; x=abs(x); return word('th st nd rd',1+x//10*(x//100%10\==1)*(x//10<4))
```

{{out|output|text=  when using the input default of:     <tt> 200 </tt>}}
<pre style="height:45ex">
                      1st prime   ───►    2
                      2nd prime   ───►    3
                      3rd prime   ───►    5
                      4th prime   ───►    7
                      5th prime   ───►   11
                      6th prime   ───►   13
                      7th prime   ───►   17
                      8th prime   ───►   19
                      9th prime   ───►   23
                     10th prime   ───►   29
                     11th prime   ───►   31
                     12th prime   ───►   37
                     13th prime   ───►   41
                     14th prime   ───►   43
                     15th prime   ───►   47
                     16th prime   ───►   53
                     17th prime   ───►   59
                     18th prime   ───►   61
                     19th prime   ───►   67
                     20th prime   ───►   71
                     21st prime   ───►   73
                     22nd prime   ───►   79
                     23rd prime   ───►   83
                     24th prime   ───►   89
                     25th prime   ───►   97
                     26th prime   ───►  101
                     27th prime   ───►  103
                     28th prime   ───►  107
                     29th prime   ───►  109
                     30th prime   ───►  113
                     31st prime   ───►  127
                     32nd prime   ───►  131
                     33rd prime   ───►  137
                     34th prime   ───►  139
                     35th prime   ───►  149
                     36th prime   ───►  151
                     37th prime   ───►  157
                     38th prime   ───►  163
                     39th prime   ───►  167
                     40th prime   ───►  173
                     41st prime   ───►  179
                     42nd prime   ───►  181
                     43rd prime   ───►  191
                     44th prime   ───►  193
                     45th prime   ───►  197
                     46th prime   ───►  199

                     46 primes found up to and including  200

```

{{out|output|text=  when using the input of:     <tt> -1000 </tt>}}

```txt

                     168 primes found up to and including  1000


```

{{out|output|text=  when using the input of:     <tt> -10000 </tt>}}

```txt

                     1229 primes found up to and including  10000

```

{{out|output|text=  when using the input of:     <tt> -100000 </tt>}}
                     9592 primes found up to and including  100000.

```

{{out|output|text=  when using the input of:     <tt> -1000000 </tt>}}

```txt

                     78498 primes found up to and including  10000000

```

{{out|output|text=  when using the input of:     <tt> -10000000 </tt>}}

```txt

                     664579 primes found up to and including  10000000

```



### wheel version

This version skips striking the even numbers   (as being not prime),   '''2'''   is handled as a special case.

It also uses a short-circuit test for striking out composites   ≤   &radic;{{overline| target }}

```rexx
/*REXX pgm generates and displays primes via a wheeled sieve of Eratosthenes algorithm. */
parse arg H .;  if H=='' | H==","  then H= 200   /*obtain the optional argument from CL.*/
w= length(H);       @prime= right('prime', 20)   /*w:  is used for aligning the output. */
if 2<=H  then  say  @prime  right(1, w)       " ───► "       right(2, w)
#= 2<=H                                          /*the number of primes found  (so far).*/
@.=.                                             /*assume all the numbers are  prime.   */
!=0;  do j=3  by 2  for (H-2)%2                  /*the odd integers up to  H  inclusive.*/
      if @.j==''  then iterate                   /*Is composite?  Then skip this number.*/
      #= # + 1                                   /*bump the prime number counter.       */
      say  @prime right(#,w) " ───► " right(j,w) /*display the prime to the terminal.   */
      if !        then iterate                   /*skip the top part of loop?       ___ */
      if j*j>H     then !=1                      /*indicate skip top part  if  J > √ H  */
          do m=j*j  to H  by j+j;   @.m=;   end  /*strike odd multiples as  not  prime. */
      end   /*j*/                                /*       ───                           */
say                                              /*stick a fork in it,  we're all done. */
say right(#,  1 + w + length(@prime) )    'primes found up to and including '    H
```

{{out|output|text=  is identical to the first (non-wheel) version;   program execution is over   ''twice''   as fast.}}

The addition of the short-circuit test   (using the REXX variable  <big>'''!'''</big>)   makes it about   ''another''   '''20%'''   faster.




### Wheel Version restructured


```rexx
/*REXX program generates primes via sieve of Eratosthenes algorithm.
* 21.07.2012 Walter Pachl derived from above Rexx version
*                       avoid symbols @ and # (not supported by ooRexx)
*                       avoid negations (think positive)
**********************************************************************/
  highest=200                       /*define highest number to use.  */
  is_prime.=1                       /*assume all numbers are prime.  */
  w=length(highest)                 /*width of the biggest number,   */
                                    /*  it's used for aligned output.*/
  Do j=3 To highest By 2,           /*strike multiples of odd ints.  */
               While j*j<=highest   /* up to sqrt(highest)           */
      If is_prime.j Then Do
        Do jm=j*3 To highest By j+j /*start with next odd mult. of J */
          is_prime.jm=0             /*mark odd mult. of J not prime. */
          End
        End
    End
  np=0                              /*number of primes shown         */
  Call tell 2
  Do n=3 To highest By 2            /*list all the primes found.     */
    If is_prime.n Then Do
      Call tell n
      End
    End
  Exit
tell: Parse Arg prime
      np=np+1
      Say '           prime number' right(np,w) " --> " right(prime,w)
      Return
```

'''output''' is mostly identical to the above versions.


## Ring


```ring

limit = 100
sieve = list(limit)
for i = 2 to limit
    for k = i*i to limit step i
        sieve[k] = 1
    next
    if sieve[i] = 0 see "" + i + " " ok
next

```

Output:

```txt

2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97

```



## Ruby

''eratosthenes'' starts with <code>nums = [nil, nil, 2, 3, 4, 5, ..., n]</code>, then marks (　the nil setting　) multiples of <code>2, 3, 5, 7, ...</code> there, then returns all non-nil numbers which are the primes.

```ruby
def eratosthenes(n)
  nums = [nil, nil, *2..n]
  (2..Math.sqrt(n)).each do |i|
    (i**2..n).step(i){|m| nums[m] = nil}  if nums[i]
  end
  nums.compact
end

p eratosthenes(100)
```


```txt
[2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97]
```



### With a wheel

''eratosthenes2'' adds more optimizations, but the code is longer.

* The array <code>nums</code> only tracks odd numbers (skips multiples of 2).
* The array <code>nums</code> holds booleans instead of integers, and every multiple of 3 begins <code>false</code>.
* The outer loop skips multiples of 2 and 3.
* Both inner loops skip multiples of 2 and 3.


```ruby
def eratosthenes2(n)
  # For odd i, if i is prime, nums[i >> 1] is true.
  # Set false for all multiples of 3.
  nums = [true, false, true] * ((n + 5) / 6)
  nums[0] = false  # 1 is not prime.
  nums[1] = true   # 3 is prime.

  # Outer loop and both inner loops are skipping multiples of 2 and 3.
  # Outer loop checks i * i > n, same as i > Math.sqrt(n).
  i = 5
  until (m = i * i) > n
    if nums[i >> 1]
      i_times_2 = i << 1
      i_times_4 = i << 2
      while m <= n
        nums[m >> 1] = false
        m += i_times_2
        nums[m >> 1] = false
        m += i_times_4  # When i = 5, skip 45, 75, 105, ...
      end
    end
    i += 2
    if nums[i >> 1]
      m = i * i
      i_times_2 = i << 1
      i_times_4 = i << 2
      while m <= n
        nums[m >> 1] = false
        m += i_times_4  # When i = 7, skip 63, 105, 147, ...
        nums[m >> 1] = false
        m += i_times_2
      end
    end
    i += 4
  end

  primes = [2]
  nums.each_index {|i| primes << (i * 2 + 1) if nums[i]}
  primes.pop while primes.last > n
  primes
end

p eratosthenes2(100)
```


This simple benchmark compares ''eratosthenes'' with ''eratosthenes2''.


```ruby
require 'benchmark'
Benchmark.bmbm {|x|
  x.report("eratosthenes") { eratosthenes(1_000_000) }
  x.report("eratosthenes2") { eratosthenes2(1_000_000) }
}
```


''eratosthenes2'' runs about 4 times faster than ''eratosthenes''.


###  With the standard library

[[MRI]] 1.9.x implements the sieve of Eratosthenes at file [http://redmine.ruby-lang.org/projects/ruby-19/repository/entry/lib/prime.rb prime.rb], <code>class EratosthensesSeive</code> (around [http://redmine.ruby-lang.org/projects/ruby-19/repository/entry/lib/prime.rb#L421 line 421]). This implementation optimizes for space, by packing the booleans into 16-bit integers. It also hardcodes all primes less than 256.


```ruby
require 'prime'
p Prime::EratosthenesGenerator.new.take_while {|i| i <= 100}
```



## Run BASIC


```runbasic
input "Gimme the limit:"; limit
dim flags(limit)
for i = 2 to  limit
 for k = i*i to limit step i
  flags(k) = 1
 next k
if flags(i) = 0 then print i;", ";
next i
```


```txt
Gimme the limit:?100
2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97,
```



## Rust

[[Category:Rust]]
===Sieve of Eratosthenes - No optimization===

```rust
fn simple_sieve(limit: usize) -> Vec<usize> {

    let mut is_prime = vec![true; limit+1];
    is_prime[0] = false;
    if limit >= 1 { is_prime[1] = false }

    for num in 2..limit+1 {
        if is_prime[num] {
            let mut multiple = num*num;
            while multiple <= limit {
                is_prime[multiple] = false;
                multiple += num;
            }
        }
    }

    is_prime.iter().enumerate()
        .filter_map(|(pr, &is_pr)| if is_pr {Some(pr)} else {None} )
        .collect()
}

fn main() {
    println!("{:?}", simple_sieve(100));
}
```

{{out}}

```txt
[2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97]
```


===Basic Version slightly optimized, Iterator output===

The above code doesn't even do the basic optimizing of only culling composites by primes up to the square root of the range as allowed in the task; it also outputs a vector of resulting primes, which consumes memory.  The following code fixes both of those, outputting the results as an Iterator:


```rust
use std::iter::{empty, once};
use std::time::Instant;

fn basic_sieve(limit: usize) -> Box<Iterator<Item = usize>> {
    if limit < 2 { return Box::new(empty()) }

    let mut is_prime = vec![true; limit+1];
    is_prime[0] = false;
    if limit >= 1 { is_prime[1] = false }
    let sqrtlmt = (limit as f64).sqrt() as usize + 1;

    for num in 2..sqrtlmt {
        if is_prime[num] {
            let mut multiple = num * num;
            while multiple <= limit {
                is_prime[multiple] = false;
                multiple += num;
            }
        }
    }

    Box::new(is_prime.into_iter().enumerate()
                .filter_map(|(p, is_prm)| if is_prm { Some(p) } else { None }))

}

fn main() {
    let n = 1000000;
    let vrslt = basic_sieve(100).collect::<Vec<_>>();
    println!("{:?}", vrslt);
    let strt = Instant::now();

    // do it 1000 times to get a reasonable execution time span...
    let rslt = (1..1000).map(|_| basic_sieve(n)).last().unwrap();

    let elpsd = strt.elapsed();

    let count = rslt.count();
    println!("{}", count);

    let secs = elpsd.as_secs();
    let millis = (elpsd.subsec_nanos() / 1000000) as u64;
    let dur = secs * 1000 + millis;
    println!("Culling composites took {} milliseconds.", dur);
}
```

{{output}}

```txt
[2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97]
78498
Culling composites took 4595 milliseconds.
```


The sieving operation is run for 1000 loops in order to get a reasonable execution time for comparison.

===Odds-only bit-packed array, Iterator output===

The following code improves the above code by sieving only odd composite numbers as 2 is the only even prime for a reduction in number of operations by a factor of about two and a half with reduction of memory use by a factor of two, and bit-packs the composite sieving array for a further reduction of memory use by a factor of eight and with some saving in time due to better CPU cache use for a given sieving range; it also demonstrates how to eliminate the redundant array bounds check:


```rust
fn optimized_sieve(limit: usize) -> Box<Iterator<Item = usize>> {
    if limit < 3 {
        return if limit < 2 { Box::new(empty()) } else { Box::new(once(2)) }
    }

    let ndxlmt = (limit - 3) / 2 + 1;
    let bfsz = ((limit - 3) / 2) / 32 + 1;
    let mut cmpsts = vec![0u32; bfsz];
    let sqrtndxlmt = ((limit as f64).sqrt() as usize - 3) / 2 + 1;

    for ndx in 0..sqrtndxlmt {
        if (cmpsts[ndx >> 5] & (1u32 << (ndx & 31))) == 0 {
            let p = ndx + ndx + 3;
            let mut cullpos = (p * p - 3) / 2;
            while cullpos < ndxlmt {
                unsafe { // avoids array bounds check, which is already done above
	            let cptr = cmpsts.get_unchecked_mut(cullpos >> 5);
	            *cptr |= 1u32 << (cullpos & 31);
                }
//                cmpsts[cullpos >> 5] |= 1u32 << (cullpos & 31); // with bounds check
                cullpos += p;
            }
        }
    }

    Box::new((-1 .. ndxlmt as isize).into_iter().filter_map(move |i| {
                if i < 0 { Some(2) } else {
                    if cmpsts[i as usize >> 5] & (1u32 << (i & 31)) == 0 {
                        Some((i + i + 3) as usize) } else { None } }
    }))
}
```


The above function can be used just by substituting "optimized_sieve" for "basic_sieve" in the previous "main" function, and the outputs are the same except that the time is only 1584 milliseconds, or about three times as fast.

===Unbounded Page-Segmented bit-packed odds-only version with Iterator===

'''Caution!''' This implementation is used in the [[Extensible_prime_generator#Rust|Extensible prime generator task]], so be sure not to break that implementation when changing this code.

While that above code is quite fast, as the range increases above the 10's of millions it begins to lose efficiency due to loss of CPU cache associativity as the size of the one-large-array used for culling composites grows beyond the limits of the various CPU caches.  Accordingly the following page-segmented code where each culling page can be limited to not larger than the L1 CPU cache is about four times faster than the above for the range of one billion:


```rust
use std::iter::{empty, once};
use std::rc::Rc;
use std::cell::RefCell;
use std::time::Instant;

const RANGE: u64 = 1000000000;
const SZ_PAGE_BTS: u64 = (1 << 14) * 8; // this should be the size of the CPU L1 cache
const SZ_BASE_BTS: u64 = (1 << 7) * 8;
static CLUT: [u8; 256] = [
	8, 7, 7, 6, 7, 6, 6, 5, 7, 6, 6, 5, 6, 5, 5, 4, 7, 6, 6, 5, 6, 5, 5, 4, 6, 5, 5, 4, 5, 4, 4, 3,
	7, 6, 6, 5, 6, 5, 5, 4, 6, 5, 5, 4, 5, 4, 4, 3, 6, 5, 5, 4, 5, 4, 4, 3, 5, 4, 4, 3, 4, 3, 3, 2,
	7, 6, 6, 5, 6, 5, 5, 4, 6, 5, 5, 4, 5, 4, 4, 3, 6, 5, 5, 4, 5, 4, 4, 3, 5, 4, 4, 3, 4, 3, 3, 2,
	6, 5, 5, 4, 5, 4, 4, 3, 5, 4, 4, 3, 4, 3, 3, 2, 5, 4, 4, 3, 4, 3, 3, 2, 4, 3, 3, 2, 3, 2, 2, 1,
	7, 6, 6, 5, 6, 5, 5, 4, 6, 5, 5, 4, 5, 4, 4, 3, 6, 5, 5, 4, 5, 4, 4, 3, 5, 4, 4, 3, 4, 3, 3, 2,
	6, 5, 5, 4, 5, 4, 4, 3, 5, 4, 4, 3, 4, 3, 3, 2, 5, 4, 4, 3, 4, 3, 3, 2, 4, 3, 3, 2, 3, 2, 2, 1,
	6, 5, 5, 4, 5, 4, 4, 3, 5, 4, 4, 3, 4, 3, 3, 2, 5, 4, 4, 3, 4, 3, 3, 2, 4, 3, 3, 2, 3, 2, 2, 1,
	5, 4, 4, 3, 4, 3, 3, 2, 4, 3, 3, 2, 3, 2, 2, 1, 4, 3, 3, 2, 3, 2, 2, 1, 3, 2, 2, 1, 2, 1, 1, 0 ];

fn count_page(lmti: usize, pg: &[u32]) -> i64 {
	let pgsz = pg.len(); let pgbts = pgsz * 32;
	let (lmt, icnt) = if lmti >= pgbts { (pgsz, 0) } else {
		let lstw = lmti / 32;
		let msk = 0xFFFFFFFEu32 << (lmti & 31);
		let v = (msk | pg[lstw]) as usize;
		(lstw, (CLUT[v & 0xFF] + CLUT[(v >> 8) & 0xFF]
			+ CLUT[(v >> 16) & 0xFF] + CLUT[v >> 24]) as u32)
	};
	let mut count = 0u32;
	for i in 0 .. lmt {
		let v = pg[i] as usize;
		count += (CLUT[v & 0xFF] + CLUT[(v >> 8) & 0xFF]
					+ CLUT[(v >> 16) & 0xFF] + CLUT[v >> 24]) as u32;
	}
	(icnt + count) as i64
}

fn primes_pages() -> Box<Iterator<Item = (u64, Vec<u32>)>> {
	// a memoized iterable enclosing a Vec that grows as needed from an Iterator...
	type Bpasi = Box<Iterator<Item = (u64, Vec<u32>)>>; // (lwi, base cmpsts page)
	type Bpas = Rc<(RefCell<Bpasi>, RefCell<Vec<Vec<u32>>>)>; // interior mutables
	struct Bps(Bpas); // iterable wrapper for base primes array state
	struct Bpsi<'a>(usize, &'a Bpas); // iterator with current pos, state ref's
	impl<'a> Iterator for Bpsi<'a> {
		type Item = &'a Vec<u32>;
		fn next(&mut self) -> Option<Self::Item> {
			let n = self.0; let bpas = self.1;
			while n >= bpas.1.borrow().len() { // not thread safe
				let nbpg = match bpas.0.borrow_mut().next() {
								Some(v) => v, _ => (0, vec!()) };
				if nbpg.1.is_empty() { return None } // end if no source iter
				bpas.1.borrow_mut().push(cnvrt2bppg(nbpg));
			}
			self.0 += 1; // unsafe pointer extends interior -> exterior lifetime
			// multi-threading might drop following Vec while reading - protect
			let ptr = &bpas.1.borrow()[n] as *const Vec<u32>;
			unsafe { Some(&(*ptr)) }
		}
	}
	impl<'a> IntoIterator for &'a Bps {
		type Item = &'a Vec<u32>;
		type IntoIter = Bpsi<'a>;
		fn into_iter(self) -> Self::IntoIter {
			Bpsi(0, &self.0)
		}
	}
	fn make_page(lwi: u64, szbts: u64, bppgs: &Bpas)
			-> (u64, Vec<u32>) {
		let nxti = lwi + szbts;
		let pbts = szbts as usize;
		let mut cmpsts = vec!(0u32; pbts / 32);
		'outer: for bpg in Bps(bppgs.clone()).into_iter() { // in the inner tight loop...
			let pgsz = bpg.len();
			for i in 0 .. pgsz {
				let p = bpg[i] as u64; let pc = p as usize;
				let s = (p * p - 3) / 2;
				if s >= nxti { break 'outer; } else { // page start address:
					let mut cp = if s >= lwi { (s - lwi) as usize } else {
						let r = ((lwi - s) % p) as usize;
						if r == 0 { 0 } else { pc - r }
					};
					while cp < pbts {
						unsafe { // avoids array bounds check, which is already done above
							let cptr = cmpsts.get_unchecked_mut(cp >> 5);
							*cptr |= 1u32 << (cp & 31); // about as fast as it gets...
						}
//						cmpsts[cp >> 5] |= 1u32 << (cp & 31);
						cp += pc;
					}
				}
			}
		}
		(lwi, cmpsts)
	}
	fn pages_from(lwi: u64, szbts: u64, bpas: Bpas)
			-> Box<Iterator<Item = (u64, Vec<u32>)>> {
		struct Gen(u64,  u64);
		impl Iterator for Gen {
			type Item = (u64, u64);
			#[inline]
			fn next(&mut self) -> Option<(u64, u64)> {
				let v = self.0; let inc = self.1; // calculate variable size here
				self.0 = v + inc;
				Some((v, inc))
			}
		}
		Box::new(Gen(lwi, szbts)
					.map(move |(lwi, szbts)| make_page(lwi, szbts, &bpas)))
	}
	fn cnvrt2bppg(cmpsts: (u64, Vec<u32>)) -> Vec<u32> {
		let (lwi, pg) = cmpsts;
		let pgbts = pg.len() * 32;
		let cnt = count_page(pgbts, &pg) as usize;
		let mut bpv = vec!(0u32; cnt);
		let mut j = 0; let bsp = (lwi + lwi + 3) as usize;
		for i in 0 .. pgbts {
			if (pg[i >> 5] & (1u32 << (i & 0x1F))) == 0u32 {
				bpv[j] = (bsp + i + i) as u32; j += 1;
			}
		}
		bpv
	}
	// recursive Rc/RefCell variable bpas - used only for init, then fixed ...
	// start with just enough base primes to init the first base primes page...
	let base_base_prms = vec!(3u32,5u32,7u32);
	let rcvv = RefCell::new(vec!(base_base_prms));
	let bpas: Bpas = Rc::new((RefCell::new(Box::new(empty())), rcvv));
	let initpg = make_page(0, 32, &bpas); // small base primes page for SZ_BASE_BTS = 2^7 * 8
	*bpas.1.borrow_mut() = vec!(cnvrt2bppg(initpg)); // use for first page
	let frstpg = make_page(0, SZ_BASE_BTS, &bpas); // init bpas for first base prime page
	*bpas.0.borrow_mut() = pages_from(SZ_BASE_BTS, SZ_BASE_BTS, bpas.clone()); // recurse bpas
	*bpas.1.borrow_mut() = vec!(cnvrt2bppg(frstpg)); // fixed for subsequent pages
	pages_from(0, SZ_PAGE_BTS, bpas) // and bpas also used here for main pages
}

fn primes_paged() -> Box<Iterator<Item = u64>> {
	fn list_paged_primes(cmpstpgs: Box<Iterator<Item = (u64, Vec<u32>)>>)
			-> Box<Iterator<Item = u64>> {
		Box::new(cmpstpgs.flat_map(move |(lwi, cmpsts)| {
			let pgbts = (cmpsts.len() * 32) as usize;
			(0..pgbts).filter_map(move |i| {
				if cmpsts[i >> 5] & (1u32 << (i & 31)) == 0 {
					Some((lwi + i as u64) * 2 + 3) } else { None } }) }))
	}
	Box::new(once(2u64).chain(list_paged_primes(primes_pages())))
}

fn count_primes_paged(top: u64) -> i64 {
	if top < 3 { if top < 2 { return 0i64 } else { return 1i64 } }
	let topi = (top - 3u64) / 2;
	primes_pages().take_while(|&(lwi, _)| lwi <= topi)
		.map(|(lwi, pg)| { count_page((topi - lwi) as usize, &pg) })
		.sum::<i64>() + 1
}

fn main() {
	let n = 262146;
	let vrslt = primes_paged()
			.take_while(|&p| p <= 100)
			.collect::<Vec<_>>();
	println!("{:?}", vrslt);

	let strt = Instant::now();

//	let count = primes_paged().take_while(|&p| p <= RANGE).count(); // slow way to count
	let count = count_primes_paged(RANGE); // fast way to count

	let elpsd = strt.elapsed();

	println!("{}", count);

	let secs = elpsd.as_secs();
	let millis = (elpsd.subsec_nanos() / 1000000) as u64;
	let dur = secs * 1000 + millis;
	println!("Culling composites took {} milliseconds.", dur);
}
```


The output is about the same as the previous codes except much faster; as well as cache size improvements mentioned above, it has a population count primes counting function that is able to determine the number of found primes about twice as fast as using the Iterator count() method (commented out and labelled as "the slow way" in the main function).

As listed above, the code maintains its efficiency up to about sixteen billion, and can easily be extended to be useful above that point by having the buffer size dynamically calculated to be proportional to the square root of the current range as commented in the code.

It would also be quite easy to extend the code to use multi-threading per page so that the time would be reduced proportionally to the number of true CPU cores used (not Hyper-Threaded ones) as in four true cores for many common high end desktop CPU's.

Before being extended to truly huge ranges such a 1e14, the code should have maximum wheel factorization added (2;3;5;7 wheels and the culling buffers further pre-culled by the primes (11;13;17; and maybe 19), which would speed it up by another factor of four or so for the range of one billion.  It would also be possible to use extreme loop unrolling techniques such as used by "primesieve" written in C/C++ to increase the speed for this range by another factor of two or so.

The above code demonstrates some techniques to work within the limitations of Rust's ownership/borrowing/lifetime memory model as it:  1) uses a recursive secondary base primes Iterator made persistent by using a Vec that uses its own value as a source of its own page stream, 2) this is done by using a recursive variable that accessed as a Rc reference counted heap value with internal mutability by a pair of RefCell's, 3) note that the above secondary stream is not thread safe and needs to have the Rc changed to an Arc, the RefCell's changed to Mutex'es or (probably preferably RwLock's that enclose/lock all reading and writing operations in the secondary stream "Bpsi"'s next() method, and 4) the use of Iterators where their performance doesn't matter (at the page level) while using tight loops at more inner levels.


## SAS

The following defines an IML routine to compute the sieve, and as an example stores the primes below 1000 in a dataset.

<lang>proc iml;
start sieve(n);
    a = J(n,1);
    a[1] = 0;
    do i = 1 to n;
        if a[i] then do;
            if i*i>n then return(a);
            a[i*(i:int(n/i))] = 0;
        end;
    end;
finish;

a = loc(sieve(1000))`;
create primes from a;
append from a;
close primes;
quit;
```



## SASL

{{incorrect|SASL|These use REM (division) testing and so are Trial Division algorithms, not Sieve of Eratosthenes.}}
Copied from SASL manual, top of page 36. This provides an infinite list.

```SASL

show primes
WHERE
primes = sieve (2...)
sieve (p : x ) = p : sieve {a <- x; a REM p > 0}
?

```


The limited list for the first 1000 numbers

```SASL

show primes
WHERE
primes = sieve (2..1000)
sieve (p : x ) = p : sieve {a <- x; a REM p > 0}
?

```


=={{header|S-BASIC}}==

```basic

comment
   Find primes up to the specified limit (here 1,000) using
   classic Sieve of Eratosthenes
end

$constant limit = 1000
$constant false = 0
$constant true = FFFFH

var i, k, count, col = integer
dim integer flags(limit)

print "Finding primes from 2 to";limit

rem - initialize table
for i = 1 to limit
  flags(i) = true
next i

rem - sieve for primes
for i = 2 to int(sqr(limit))
  if flags(i) = true then
     for k = (i*i) to limit step i
        flags(k) = false
     next k
next i

rem - write out primes 8 per line
count = 0
col = 1
for i = 2 to limit
   if flags(i) = true then
      begin
         print using "####";i;
         count = count + 1
         col = col + 1
         if col > 8 then
            begin
               print
               col = 1
            end
      end
next i
print
print count; " primes were found."

end

```



## Scala



###  Genuine Eratosthenes sieve


```Scala
import scala.annotation.tailrec
import scala.collection.parallel.mutable
import scala.compat.Platform

object GenuineEratosthenesSieve extends App {
  def sieveOfEratosthenes(limit: Int) = {
    val (primes: mutable.ParSet[Int], sqrtLimit) = (mutable.ParSet.empty ++ (2 to limit), math.sqrt(limit).toInt)
    @tailrec
    def prim(candidate: Int): Unit = {
      if (candidate <= sqrtLimit) {
        if (primes contains candidate) primes --= candidate * candidate to limit by candidate
        prim(candidate + 1)
      }
    }
    prim(2)
    primes
  }
  // BitSet toList is shuffled when using the ParSet version. So it has to be sorted before using it as a sequence.

  assert(sieveOfEratosthenes(15099480).size == 976729)
  println(s"Successfully completed without errors. [total ${Platform.currentTime - executionStart} ms]")
}
```


{{out}}
 Successfully completed without errors. [total 39807 ms]

 Process finished with exit code 0

While concise, the above code is quite slow but a little faster not using the ParSet ('''take out the '.par' for speed'''), in which case the sorting ('sorted') is not necessary for an additional small gain in speed; the above code is slow because of all the overhead in processing the bit packed "BitSet" bib-by-bit using complex "higher-order" method calls.

The following [['''odds-only''']] code is written in a very concise functional style (no mutable state other than the contents of the composites buffer and "higher order functions" for clarity), in this case using a Scala mutable BitSet:


```Scala
object SoEwithBitSet {
  def makeSoE_PrimesTo(top: Int): Iterator[Int] = {
    val topNdx = (top - 3) / 2 //odds composite BitSet buffer offset down to 3
    val cmpsts = new scala.collection.mutable.BitSet(topNdx + 1) //size includes topNdx
    @inline def cullPrmCmpsts(prmNdx: Int) = {
      val prm = prmNdx + prmNdx + 3; cmpsts ++= ((prm * prm - 3) >>> 1 to topNdx by prm) }
    (0 to (Math.sqrt(top).toInt - 3) / 2).filterNot { cmpsts }.foreach { cullPrmCmpsts }
    Iterator.single(2) ++ (0 to topNdx).filterNot { cmpsts }.map { pi => pi + pi + 3 } }
}
```


In spite of being very concise, it is very much faster than the above code converted to odds-only due to the use of the BitSet instead of the hash table based Set (or ParSet), taking only a few seconds to enumerate the primes to 100 million as compared to the 10's of seconds to count the primes to above 15 million above.


###  Using tail recursion

The below [['''odds-only''']] code using a primitive array (bit packed) and tail recursion to avoid some of the enumeration delays due to nested complex "higher order" function calls is almost eight times faster than the above more functional code:


```Scala
object SoEwithArray {
  def makeSoE_PrimesTo(top: Int) = {
    import scala.annotation.tailrec
    val topNdx = (top - 3) / 2 + 1 //odds composite BitSet buffer offset down to 3 plus 1 for overflow
    val (cmpsts, sqrtLmtNdx) = (new Array[Int]((topNdx >>> 5) + 1), (Math.sqrt(top).toInt - 3) / 2)

    @inline def isCmpst(ci: Int): Boolean = (cmpsts(ci >>> 5) & (1 << (ci & 31))) != 0

    @inline def setCmpst(ci: Int): Unit = cmpsts(ci >>> 5) |= 1 << (ci & 31)

    @tailrec def forCndNdxsFrom(cndNdx: Int): Unit =
      if (cndNdx <= sqrtLmtNdx) {
        if (!isCmpst(cndNdx)) { //is prime
          val p = cndNdx + cndNdx + 3

          @tailrec def cullPrmCmpstsFrom(cmpstNdx: Int): Unit =
            if (cmpstNdx <= topNdx) { setCmpst(cmpstNdx); cullPrmCmpstsFrom(cmpstNdx + p) }

          cullPrmCmpstsFrom((p * p - 3) >>> 1) }

        forCndNdxsFrom(cndNdx + 1) }; forCndNdxsFrom(0)

    @tailrec def getNxtPrmFrom(cndNdx: Int): Int =
      if ((cndNdx > topNdx) || !isCmpst(cndNdx)) cndNdx + cndNdx + 3 else getNxtPrmFrom(cndNdx + 1)

    Iterator.single(2) ++ Iterator.iterate(3)(p => getNxtPrmFrom(((p + 2) - 3) >>> 1)).takeWhile(_ <= top)
  }
}
```


It can be tested with the following code:


```Scala
object Main extends App {
  import SoEwithArray._
  val top_num = 100000000
  val strt = System.nanoTime()
  val count = makeSoE_PrimesTo(top_num).size
  val end = System.nanoTime()
  println(s"Successfully completed without errors. [total ${(end - strt) / 1000000} ms]")
  println(f"Found $count primes up to $top_num" + ".")
  println("Using one large mutable Array and tail recursive loops.")
}
```


To produce the following output:

{{out}}
 Successfully completed without errors. [total 661 ms]
 Found 5761455 primes up to 100000000.
 Using one large mutable Array and tail recursive loops.

===Odds-only page-segmented "infinite" generator version using tail recursion===
The above code still uses an amount of memory proportional to the range of the sieve (although bit-packed as 8 values per byte).  As well as only sieving odd candidates, the following code uses a fixed range buffer that is about the size of the CPU L2 cache plus only storage for the base primes up to the square root of the range for a large potential saving in RAM memory used as well as greatly reducing memory access times.  The use of innermost tail recursive loops for critical loops where the majority of the execution time is spent rather than "higher order" functions from iterators also greatly reduces execution time, with much of the remaining time used just to enumerate the primes output:


```Scala
object APFSoEPagedOdds {
  import scala.annotation.tailrec

  private val CACHESZ = 1 << 18 //used cache buffer size
  private val PGSZ = CACHESZ / 4 //number of int's in cache
  private val PGBTS = PGSZ * 32 //number of bits in buffer

  //processing output type is a tuple of low bit (odds) address,
  // bit range size, and the actual culled page segment buffer.
  private type Chunk = (Long, Int, Array[Int])

  //produces an iteration of all the primes from an iteration of Chunks
  private def enumChnkPrms(chnks: Stream[Chunk]): Iterator[Long] = {
    def iterchnk(chnk: Chunk) = { //iterating primes per Chunk
      val (lw, rng, bf) = chnk
      @tailrec def nxtpi(i: Int): Int = { //find next prime index not composite
        if (i < rng && (bf(i >>> 5) & (1 << (i & 31))) != 0) nxtpi(i + 1) else i }
      Iterator.iterate(nxtpi(0))(i => nxtpi(i + 1)).takeWhile { _ < rng }
        .map { i => ((lw + i) << 1) + 3 } } //map from index to prime value
    chnks.toIterator.flatMap { iterchnk } }

  //culls the composite number bit representations from the bit-packed page buffer
  //using a given source of a base primes iterator
  private def cullPg(bsprms: Iterator[Long],
                     lowi: Long, buf: Array[Int]): Unit = {
    //cull for all base primes until square >= nxt
    val rng = buf.length * 32; val nxt = lowi + rng
    @tailrec def cull(bps: Iterator[Long]): Unit = {
      //given prime then calculate the base start address for prime squared
      val bp = bps.next(); val s = (bp * bp - 3) / 2
      //almost all of the execution time is spent in the following tight loop
      @tailrec def cullp(j: Int): Unit = { //cull the buffer for given prime
        if (j < rng) { buf(j >>> 5) |= 1 << (j & 31); cullp(j + bp.toInt) } }
      if (s < nxt) { //only cull for primes squared less than max
        //calculate the start address within this given page segment
        val strt = if (s >= lowi) (s - lowi).toInt else {
          val b = (lowi - s) % bp
          if (b == 0) 0 else (bp - b).toInt }
        cullp(strt); if (bps.hasNext) cull(bps) } } //loop for all primes in range
    //for the first page, use own bit pattern as a source of base primes
    //if another source is not given
    if (lowi <= 0 && bsprms.isEmpty)
      cull(enumChnkPrms(Stream((0, buf.length << 5, buf))))
    //otherwise use the given source of base primes
    else if (bsprms.hasNext) cull(bsprms) }

  //makes a chunk given a low address in (odds) bits
  private def mkChnk(lwi: Long): Chunk = {
    val rng = PGBTS; val buf = new Array[Int](rng / 32);
    val bps = if (lwi <= 0) Iterator.empty else enumChnkPrms(basePrms)
    cullPg(bps, lwi, buf); (lwi, rng, buf) }

  //new independent source of base primes in a stream of packed-bit arrays
  //memoized by converting it to a Stream and retaining a reference here
  private val basePrms: Stream[Chunk] =
    Stream.iterate(mkChnk(0)) { case (lw, rng, bf) => { mkChnk(lw + rng) } }

  //produces an infinite iterator over all the chunk results
  private def itrRslts[R](rsltf: Chunk => R): Iterator[R] = {
    def mkrslt(lwi: Long) = { //makes tuple of result and next low index
      val c = mkChnk(lwi); val (_, rng, _) = c; (rsltf(c), lwi + rng) }
    Iterator.iterate(mkrslt(0)) { case (_, nlwi) => mkrslt(nlwi) }
            .map { case (rslt, _) => rslt} } //infinite iteration of results

  //iterates across the "infinite" produced output primes
  def enumSoEPrimes(): Iterator[Long] = //use itrRsltsMP to produce Chunks iteration
    Iterator.single(2L) ++ enumChnkPrms(itrRslts { identity }.toStream)

  //counts the number of remaining primes in a page segment buffer
  //using a very fast bitcount per integer element
  //with special treatment for the last page
  private def countpgto(top: Long, b: Array[Int], nlwp: Long) = {
    val numbts = b.length * 32; val prng = numbts * 2
    @tailrec def cnt(i: Int, c: Int): Int = { //tight int bitcount loop
      if (i >= b.length) c else cnt (i + 1, c - Integer.bitCount(b(i))) }
    if (nlwp > top) { //for top in page, calculate int address containing top
      val bi = ((top - nlwp + prng) >>> 1).toInt
      val w = bi >>> 5; b(w) |= -2 << (bi & 31) //mark all following as composite
      for (i <- w + 1 until b.length) b(i) = -1 } //for all int's to end of buffer
    cnt(0, numbts) } //counting the entire buffer in every case

  //counts all the primes up to a top value
  def countSoEPrimesTo(top: Long): Long = {
    if (top < 2) return 0L else if (top < 3) return 1L //no work necessary
    //count all Chunks using multi-processing
    val gen = itrRslts { case (lwi, rng, bf) =>
      val nlwp = (lwi + rng) * 2 + 3; (countpgto(top, bf, nlwp), nlwp) }
    //a loop to take Chunk's up to including top limit but not past it
    @tailrec def takeUpto(acc: Long): Long = {
      val (cnt, nlwp) = gen.next(); val nacc = acc + cnt
      if (nlwp <= top) takeUpto(nacc) else nacc }; takeUpto(1) }
}
```


As the above and all following sieves are "infinite", they all require an extra range limiting condition to produce a finite output, such as the addition of ".takeWhile(_ <= topLimit)" where "topLimit" is the specified range as is done in the following code:


```Scala
object MainSoEPagedOdds extends App {
  import APFSoEPagedOdds._
  countSoEPrimesTo(100)
  val top = 1000000000
  val strt = System.currentTimeMillis()
  val cnt = enumSoEPrimes().takeWhile { _ <= top }.length
//  val cnt = countSoEPrimesTo(top)
  val elpsd = System.currentTimeMillis() - strt
  println(f"Found $cnt primes up to $top in $elpsd milliseconds.")
}
```


which outputs the following:

{{out}}
 Found 50847534 primes up to 1000000000 in 5867 milliseconds.

While the above code is reasonably fast, much of the execution time is consumed by the use of the built-in functions and iterators for concise code, especially in the use of iterators for primes output.  To show this, the code includes a "countSoEPrimesTo" function/method that can be uncommented in the above code (commenting out the "takeWhile" line) to produce the following output:

{{out}}
 Found 50847534 primes up to 1000000000 in 2623 milliseconds.

This shows that it takes somewhat longer to enumerate the primes than it does to actually produce them; this could be improved with a "roll-your-own" enumeration Iterator implementation at considerable increased complexity, but enumeration time will still be a significant portion of the execution time.  Further improvements to the code using extreme wheel factorization and multi-processing will make enumeration time an even higher percentage of the total; this is why for large ranges one writes functions/methods similar to "countSoEPrimesTo" to (say) sum the primes, to find the nth prime, etc.

===Odds-Only "infinite" generator sieve using Streams and Co-Inductive Streams===
The following code uses delayed recursion via Streams to implement the Richard Bird algorithm mentioned in the last part (the Epilogue) of [http://www.cs.hmc.edu/~oneill/papers/Sieve-JFP.pdf M.O'Neill's paper], which is '''a true incremental Sieve of Eratosthenes'''.  It is nowhere near as fast as the array based solutions due to the overhead of functionally chasing the merging of the prime multiple streams; this also means that the empirical performance is not according to the usual Sieve of Eratosthenes approximations due to this overhead increasing as the log of the sieved range, but it is much better than [[Primality_by_trial_division#Odds-Only_.22infinite.22_primes_generator_using_Streams_and_Co-Inductive_Streams|the "unfaithful" sieve]].


```Scala
  def birdPrimes() = {
    def oddPrimes: Stream[Int] = {
      def merge(xs: Stream[Int], ys: Stream[Int]): Stream[Int] = {
        val (x, y) = (xs.head, ys.head)

        if (y > x) x #:: merge(xs.tail, ys) else if (x > y) y #:: merge(xs, ys.tail) else x #:: merge(xs.tail, ys.tail)
      }

      def primeMltpls(p: Int): Stream[Int] = Stream.iterate(p * p)(_ + p + p)

      def allMltpls(ps: Stream[Int]): Stream[Stream[Int]] = primeMltpls(ps.head) #:: allMltpls(ps.tail)

      def join(ams: Stream[Stream[Int]]): Stream[Int] = ams.head.head #:: merge(ams.head.tail, join(ams.tail))

      def oddPrms(n: Int, composites: Stream[Int]): Stream[Int] =
        if (n >= composites.head) oddPrms(n + 2, composites.tail) else n #:: oddPrms(n + 2, composites)

      //following uses a new recursive source of odd base primes
      3 #:: oddPrms(5, join(allMltpls(oddPrimes)))
    }
    2 #:: oddPrimes
  }
```


Now this algorithm doesn't really need the memoization and full laziness as offered by Streams, so an implementation and use of a Co-Inductive Stream (CIS) class is sufficient and reduces execution time by almost a factor of two:
```scala
  class CIS[A](val start: A, val continue: () => CIS[A])

  def primesBirdCIS: Iterator[Int] = {
    def merge(xs: CIS[Int], ys: CIS[Int]): CIS[Int] = {
      val (x, y) = (xs.start, ys.start)

      if (y > x) new CIS(x, () => merge(xs.continue(), ys))
      else if (x > y) new CIS(y, () => merge(xs, ys.continue()))
      else new CIS(x, () => merge(xs.continue(), ys.continue()))
    }

    def primeMltpls(p: Int): CIS[Int] = {
      def nextCull(cull: Int): CIS[Int] = new CIS[Int](cull, () => nextCull(cull + 2 * p))
      nextCull(p * p)
    }

    def allMltpls(ps: CIS[Int]): CIS[CIS[Int]] =
      new CIS[CIS[Int]](primeMltpls(ps.start), () => allMltpls(ps.continue()))
    def join(ams: CIS[CIS[Int]]): CIS[Int] = {
      new CIS[Int](ams.start.start, () => merge(ams.start.continue(), join(ams.continue())))
    }

    def oddPrimes(): CIS[Int] = {
      def oddPrms(n: Int, composites: CIS[Int]): CIS[Int] = { //"minua"
        if (n >= composites.start) oddPrms(n + 2, composites.continue())
        else new CIS[Int](n, () => oddPrms(n + 2, composites))
      }
      //following uses a new recursive source of odd base primes
      new CIS(3, () => oddPrms(5, join(allMltpls(oddPrimes()))))
    }

    Iterator.single(2) ++ Iterator.iterate(oddPrimes())(_.continue()).map(_.start)
  }
```


Further gains in performance for these last two implementations can be had by using further wheel factorization and "tree folding/merging" as per [http://www.haskell.org/haskellwiki/Primes#Tree_merging_with_Wheel this Haskell implementation].

===Odds-Only "infinite" generator sieve using a hash table (HashMap)===
As per [http://www.cs.hmc.edu/~oneill/papers/Sieve-JFP.pdf the "unfaithful sieve" article linked above], the incremental "infinite" Sieve of Eratosthenes can be implemented using a hash table instead of a Priority Queue or Map (Binary Heap) as were used in that article.  The following implementation postpones the adding of base prime representations to the hash table until necessary to keep the hash table small:


```scala
  def SoEInc: Iterator[Int] = {
    val nextComposites = scala.collection.mutable.HashMap[Int, Int]()
    def oddPrimes: Iterator[Int] = {
      val basePrimes = SoEInc
      basePrimes.next()
      basePrimes.next() // skip the two and three prime factors
      @tailrec def makePrime(state: (Int, Int, Int)): (Int, Int, Int) = {
        val (candidate, nextBasePrime, nextSquare) = state
        if (candidate >= nextSquare) {
          val adv = nextBasePrime << 1
          nextComposites += ((nextSquare + adv) -> adv)
          val np = basePrimes.next()
          makePrime((candidate + 2, np, np * np))
        } else if (nextComposites.contains(candidate)) {
          val adv = nextComposites(candidate)
          nextComposites -= (candidate) += (Iterator.iterate(candidate + adv)(_ + adv)
            .dropWhile(nextComposites.contains(_)).next() -> adv)
          makePrime((candidate + 2, nextBasePrime, nextSquare))
        } else (candidate, nextBasePrime, nextSquare)
      }
      Iterator.iterate((5, 3, 9)) { case (c, p, q) => makePrime((c + 2, p, q)) }
        .map { case (p, _, _) => p }
    }
    List(2, 3).toIterator ++ oddPrimes
  }
```


The above could be implemented using Streams or Co-Inductive Streams to pass the continuation parameters as passed here in a tuple but there would be no real difference in speed and there is no need to use the implied laziness.  As compared to the versions of the Bird (or tree folding) Sieve of Eratosthenes, this has the expected same computational complexity as the array based versions, but is about 20 times slower due to the constant overhead of processing the key value hashing.  Memory use is quite low, only being the hash table entries for each of the base prime values less than the square root of the last prime enumerated multiplied by the size of each hash entry (about 12 bytes in this case) plus a "load factor" percentage overhead in hash table size to minimize hash collisions (about twice as large as entries actually used by default on average).

The Scala implementable of a mutable HashMap is slower than the java.util.HashMap one by a factor of almost two, but the Scala version is used here to keep the code more portable (as to CLR).  One can also quite easily convert this code to use the immutable Scala HashMap, but the code runs about four times slower due to the required "copy on update" operations for immutable objects.

This algorithm is very responsive to further application of wheel factorization, which can make it run up to about four times faster for the composite number culling operations; however, that is not enough to allow it to catch up to the array based sieves.


## Scheme

===Tail-recursive solution===
{{Works with|Scheme|R<math>^5</math>RS}}

```scheme
; Tail-recursive solution :
(define (sieve n)
  (define (aux u v)
    (let ((p (car v)))
      (if (> (* p p) n)
        (let rev-append ((u u) (v v))
          (if (null? u) v (rev-append (cdr u) (cons (car u) v))))
        (aux (cons p u)
          (let wheel ((u '()) (v (cdr v)) (a (* p p)))
            (cond ((null? v) (reverse u))
                  ((= (car v) a) (wheel u (cdr v) (+ a p)))
                  ((> (car v) a) (wheel u v (+ a p)))
                  (else (wheel (cons (car v) u) (cdr v) a))))))))
  (aux '(2)
    (let range ((v '()) (k (if (odd? n) n (- n 1))))
      (if (< k 3) v (range (cons k v) (- k 2))))))

; > (sieve 100)
; (2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97)
; > (length (sieve 10000000))
; 664579
```


===Simpler, non-tail-recursive solution===

```scheme
; Simpler solution, with the penalty that none of 'iota, 'strike or 'sieve is tail-recursive :
(define (iota start stop stride)
  (if (> start stop)
      (list)
      (cons start (iota (+ start stride) stop stride))))

(define (strike lst start stride)
  (cond ((null? lst) lst)
        ((= (car lst) start) (strike (cdr lst) (+ start stride) stride))
        ((> (car lst) start) (strike lst (+ start stride) stride))
        (else (cons (car lst) (strike (cdr lst) start stride)))))

(define (primes limit)
  (let ((stop (sqrt limit)))
    (define (sieve lst)
      (let ((p (car lst)))
        (if (> p stop)
            lst
            (cons p (sieve (strike (cdr lst) (* p p) p))))))
    (sieve (iota 2 limit 1))))

(display (primes 100))
(newline)
```

Output:
<lang>(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97)
```

===Optimised using an odds-wheel===
Optimised using a pre-computed wheel based on 2 (i.e. odds only):

```scheme
(define (primes-wheel-2 limit)
  (let ((stop (sqrt limit)))
    (define (sieve lst)
      (let ((p (car lst)))
        (if (> p stop)
            lst
            (cons p (sieve (strike (cdr lst) (* p p) (* 2 p)))))))
    (cons 2 (sieve (iota 3 limit 2)))))

(display (primes-wheel-2 100))
(newline)
```

Output:
<lang>(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97)
```


===Vector-based===
Vector-based (faster), works with R<math>^5</math>RS:

```scheme
; initialize v to vector of sequential integers
(define (initialize! v)
  (define (iter v n) (if (>= n (vector-length v))
                         (values)
                         (begin (vector-set! v n n) (iter v (+ n 1)))))
  (iter v 0))

; set every nth element of vector v to 0,
; starting with element m
(define (strike! v m n)
  (cond ((>= m (vector-length v)) (values))
        (else (begin
                (vector-set! v m 0)
                (strike! v (+ m n) n)))))

; lowest non-zero index of vector v >= n
(define (nextprime v n)
  (if (zero? (vector-ref v n))
      (nextprime v (+ n 1))
      (vector-ref v n)))

; remove elements satisfying pred? from list lst
(define (remove pred? lst)
  (cond
    ((null? lst) '())
    ((pred? (car lst))(remove pred? (cdr lst)))
    (else (cons (car lst) (remove pred? (cdr lst))))))

; the sieve itself
(define (sieve n)
  (define stop (sqrt n))
  (define (iter v p)
    (cond
      ((> p stop) v)
      (else
       (begin
         (strike! v (* p p) p)
         (iter v (nextprime v (+ p 1)))))))

  (let ((v (make-vector (+ n 1))))
    (initialize! v)
    (vector-set! v 1 0) ; 1 is not a prime
    (remove zero? (vector->list (iter v 2)))))
```


===SICP-style streams===
Using SICP-style ''head''-forced streams. Works with MIT-Scheme, Chez Scheme, &ndash; or any other Scheme, if writing out by hand the expansion of the only macro here, <code>s-cons</code>, with explicit lambda. Common functions:


```scheme
 ;;;; Stream Implementation
 (define (head s) (car s))
 (define (tail s) ((cdr s)))
 (define-syntax s-cons
   (syntax-rules () ((s-cons h t) (cons h (lambda () t)))))

 ;;;; Stream Utility Functions
 (define (from-By x s)
   (s-cons x (from-By (+ x s) s)))
 (define (take n s)
   (cond
     ((> n 1) (cons (head s) (take (- n 1) (tail s))))
     ((= n 1) (list (head s)))      ;; don't force it too soon!!
     (else '())))     ;; so (take 4 (s-map / (from-By 4 -1))) works
 (define (drop n s)
   (cond
     ((> n 0) (drop (- n 1) (tail s)))
     (else s)))
 (define (s-map f s)
   (s-cons (f (head s)) (s-map f (tail s))))
 (define (s-diff s1 s2)
   (let ((h1 (head s1)) (h2 (head s2)))
    (cond
     ((< h1 h2) (s-cons h1 (s-diff  (tail s1)       s2 )))
     ((< h2 h1)            (s-diff        s1  (tail s2)))
     (else                 (s-diff  (tail s1) (tail s2))))))
 (define (s-union s1 s2)
   (let ((h1 (head s1)) (h2 (head s2)))
    (cond
     ((< h1 h2) (s-cons h1 (s-union (tail s1)       s2 )))
     ((< h2 h1) (s-cons h2 (s-union       s1  (tail s2))))
     (else      (s-cons h1 (s-union (tail s1) (tail s2)))))))
```


====The simplest, naive sieve====
Very slow, running at ~ <i>n<sup>2.2</sup></i>, empirically, and worsening:

```scheme
 (define (sieve s)
	(let ((p (head s)))
	  (s-cons p
	          (sieve (s-diff s (from-By p p))))))
 (define primes (sieve (from-By 2 1)))
```


====Bounded, stopping early====
Stops at the square root of the upper limit ''m'', running at about ~ <i>n<sup>1.4</sup></i> in ''n'' primes produced, empirically. Returns infinite stream
of numbers which is only valid up to ''m'', includes composites above it:

```scheme
 (define (primes-To m)
   (define (sieve s)
     (let ((p (head s)))
       (cond ((> (* p p) m) s)
             (else (s-cons p
	             (sieve (s-diff (tail s)
	                     (from-By (* p p) p))))))))
   (sieve (from-By 2 1)))
```



### =Combined multiples sieve=

Archetypal, straightforward approach by Richard Bird, presented in [http://www.cs.hmc.edu/~oneill/papers/Sieve-JFP.pdf Melissa E. O'Neill article]. Uses <code>s-linear-join</code>, i.e. right fold, which is less efficient and of worse time complexity than the ''tree''-folding that follows. Does not attempt to conserve space by arranging for the additional inner feedback loop, as is done in the tree-folding variant below.

```scheme
 (define (primes-stream-ala-Bird)
   (define (mults p) (from-By (* p p) p))
   (define primes                                          ;; primes are
       (s-cons 2 (s-diff (from-By 3 1)                     ;;  numbers > 1, without
                  (s-linear-join (s-map mults primes)))))  ;;   multiples of primes
   primes)

 ;;;; join streams using linear structure
 (define (s-linear-join sts)
   (s-cons (head (head sts))
           (s-union (tail (head sts))
                    (s-linear-join (tail sts)))))
```


Here is a version of the same sieve, which is self contained with all the requisite functions wrapped in the overall function; optimized further. It works with odd primes only, and arranges for a separate primes feed for the base primes separate from the output stream, ''calculated recursively'' by the recursive call to "oddprms" in forming "cmpsts". It also ''"fuses"'' two functions, <code>s-diff</code> and <code>from-By</code>, into one, <code>minusstrtat</code>:


```scheme
(define (birdPrimes)
  (define (mltpls p)
    (define pm2 (* p 2))
    (let nxtmltpl ((cmpst (* p p)))
      (cons cmpst (lambda () (nxtmltpl (+ cmpst pm2))))))
  (define (allmltpls ps)
    (cons (mltpls (car ps)) (lambda () (allmltpls ((cdr ps))))))
  (define (merge xs ys)
    (let ((x (car xs)) (xt (cdr xs)) (y (car ys)) (yt (cdr ys)))
      (cond ((< x y) (cons x (lambda () (merge (xt) ys))))
            ((> x y) (cons y (lambda () (merge xs (yt)))))
            (else (cons x (lambda () (merge (xt) (yt))))))))
  (define (mrgmltpls mltplss)
    (cons (car (car mltplss))
          (lambda () (merge ((cdr (car mltplss)))
                            (mrgmltpls ((cdr mltplss)))))))
  (define (minusstrtat n cmps)
    (if (< n (car cmps))
      (cons n (lambda () (minusstrtat (+ n 2) cmps)))
      (minusstrtat (+ n 2) ((cdr cmps)))))
  (define (cmpsts) (mrgmltpls (allmltpls (oddprms)))) ;; internal define's are mutually recursive
  (define (oddprms) (cons 3 (lambda () (minusstrtat 5 (cmpsts)))))
  (cons 2 (lambda () (oddprms))))
```


It can be tested with the following code:


```scheme
(define (nthPrime n)
  (let nxtprm ((cnt 0) (ps (birdPrimes)))
    (if (< cnt n) (nxtprm (+ cnt 1) ((cdr ps))) (car ps))))
(nthPrime 1000000)
```


{{output}}
15485863

The same code can easily be modified to perform the folded tree case just by writing and integrating a "pairs" function to do the folding along with the merge, which has been done as an alternate tree folding case below.

====Tree-folding====
The most efficient. Finds composites as a tree of unions of each prime's multiples.


```scheme
 ;;;; all primes' multiples are removed, merged through a tree of unions
 ;;;;  runs in ~ n^1.15 run time in producing n = 100K .. 1M primes
 (define (primes-stream)
   (define (mults p) (from-By (* p p) (* 2 p)))
   (define (odd-primes-From from)              ;; odd primes from (odd) f are
       (s-diff (from-By from 2)                ;; all odds from f without the
               (s-tree-join (s-map mults odd-primes))))  ;; multiples of odd primes
   (define odd-primes
       (s-cons 3 (odd-primes-From 5)))         ;; inner feedback loop
   (s-cons 2 (odd-primes-From 3)))             ;; result stream

 ;;;; join an ordered stream of streams (here, of primes' multiples)
 ;;;; into one ordered stream, via an infinite right-deepening tree
 (define (s-tree-join sts)
   (s-cons (head (head sts))
           (s-union (tail (head sts))
                    (s-tree-join (pairs (tail sts))))))

 (define (pairs sts)                        ;; {a.(b.t)} -> (a+b).{t}
     (s-cons (s-cons (head (head sts))
                     (s-union (tail (head sts))
                              (head (tail sts))))
             (pairs (tail (tail sts)))))
```


[http://ideone.com/Uuil5M Print 10 last primes] of the first thousand primes:

 (display (take 10 (drop 990 (primes-stream))))
 ;
 (7841 7853 7867 7873 7877 7879 7883 7901 7907 7919)

This can be also accomplished by the following self contained code which follows the format of the <code>birdPrimes</code> code above with the added "pairs" function integrated into the "mrgmltpls" function:


```scheme
(define (treemergePrimes)
  (define (mltpls p)
    (define pm2 (* p 2))
    (let nxtmltpl ((cmpst (* p p)))
      (cons cmpst (lambda () (nxtmltpl (+ cmpst pm2))))))
  (define (allmltpls ps)
    (cons (mltpls (car ps)) (lambda () (allmltpls ((cdr ps))))))
  (define (merge xs ys)
    (let ((x (car xs)) (xt (cdr xs)) (y (car ys)) (yt (cdr ys)))
      (cond ((< x y) (cons x (lambda () (merge (xt) ys))))
            ((> x y) (cons y (lambda () (merge xs (yt)))))
            (else (cons x (lambda () (merge (xt) (yt))))))))
  (define (pairs mltplss)
    (let ((tl ((cdr mltplss))))
      (cons (merge (car mltplss) (car tl))
            (lambda () (pairs ((cdr tl)))))))
  (define (mrgmltpls mltplss)
    (cons (car (car mltplss))
          (lambda () (merge ((cdr (car mltplss)))
                            (mrgmltpls (pairs ((cdr mltplss))))))))
  (define (minusstrtat n cmps)
    (if (< n (car cmps))
      (cons n (lambda () (minusstrtat (+ n 2) cmps)))
      (minusstrtat (+ n 2) ((cdr cmps)))))
  (define (cmpsts) (mrgmltpls (allmltpls (oddprms)))) ;; internal define's are mutually recursive
  (define (oddprms) (cons 3 (lambda () (minusstrtat 5 (cmpsts)))))
  (cons 2 (lambda () (oddprms))))
```


It can be tested with the same code as the self-contained Richard Bird sieve, just by calling <code>treemergePrimes</code> instead of <code>birdPrimes</code>.


### Generators



```scheme
(define (integers n)
  (lambda ()
    (let ((ans n))
      (set! n (+ n 1))
      ans)))

(define natural-numbers (integers 0))

(define (remove-multiples g n)
  (letrec ((m (+ n n))
           (self
              (lambda ()
                 (let loop ((x (g)))
                    (cond ((< x m) x)
                          ((= x m) (set! m (+ m n)) (self))
                          (else (set! m (+ m n)) (loop x)))))))
     self))

(define (sieve g)
  (lambda ()
    (let ((x (g)))
      (set! g (remove-multiples g x))
      x)))

(define primes (sieve (integers 2)))
```



## Scilab


```scliab
function a = sieve(n)
    a = ~zeros(n, 1)
    a(1) = %f
    for i = 1:n
        if a(i)
            j = i*i
            if j > n
                return
            end
            a(j:i:n) = %f
        end
    end
endfunction

find(sieve(100))
// [2 3 5 ... 97]

sum(sieve(1000))
// 168, the number of primes below 1000
```



## Seed7

The program below computes the number of primes between 1 and 10000000:

```seed7
$ include "seed7_05.s7i";

const func set of integer: eratosthenes (in integer: n) is func
  result
    var set of integer: sieve is EMPTY_SET;
  local
    var integer: i is 0;
    var integer: j is 0;
  begin
    sieve := {2 .. n};
    for i range 2 to sqrt(n) do
      if i in sieve then
        for j range i ** 2 to n step i do
          excl(sieve, j);
        end for;
      end if;
    end for;
  end func;

const proc: main is func
  begin
    writeln(card(eratosthenes(10000000)));
  end func;
```


Original source: [http://seed7.sourceforge.net/algorith/math.htm#sieve_of_eratosthenes]


## Sidef

{{trans|Perl 6}}

```ruby
func sieve(limit) {
    var sieve_arr = [false, false, (limit-1).of(true)...]
    gather {
        sieve_arr.each_kv { |number, is_prime|
            if (is_prime) {
                take(number)
                for i in (number**2 .. limit `by` number) {
                    sieve_arr[i] = false
                }
            }
        }
    }
}

say sieve(100).join(",")
```

{{out}}

```txt

2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97

```


Alternative implementation:

```ruby
func sieve(limit) {
    var composite = []
    for n in (2 .. limit.isqrt) {
        for i in (n**2 .. limit `by` n) {
            composite[i] = true
        }
    }
    2..limit -> grep{ !composite[_] }
}

say sieve(100).join(",")
```



## Simula

{{works with|Simula-67}}

```simula
BEGIN
    INTEGER ARRAY t(0:1000);
    INTEGER i,j,k;
    FOR i:=0 STEP 1 UNTIL 1000 DO t(i):=1;
    t(0):=0; t(1):=0;
    i:=0;
    FOR i:=i WHILE i<1000 DO
    BEGIN
        FOR i:=i WHILE i<1000 AND t(i)=0 DO i:=i+1;
        IF i<1000 THEN
        BEGIN
            j:=2;
            k:=j*i;
            FOR k:=k WHILE k<1000 DO
            BEGIN
                t(k):=0;
                j:=j+1;
                k:=j*i
            END;
            i:=i+1
        END
    END;
    FOR i:=0 STEP 1 UNTIL 999 DO
       IF t(i)<>0  THEN
       BEGIN
           OutInt(i,5); OutImage
       END
END
```

{{out}}
<pre style="height:20ex">    2
    3
    5
    7
   11
   13
   17
   19
   23
   29
...
  937
  941
  947
  953
  967
  971
  977
  983
  991
  997
```


### A Concurrent Prime Sieve


```simula

! A CONCURRENT PRIME SIEVE ;

BEGIN

BOOLEAN DEBUG;

CLASS FILTER(INPUT, OUTPUT, PRIME); REF(FILTER) INPUT, OUTPUT; INTEGER PRIME;
BEGIN
    INTEGER NUM;
    IF PRIME = 0 AND INPUT == NONE THEN
    BEGIN
        ! SEND THE SEQUENCE 2, 3, 4, ... TO CHANNEL 'CH'. ;
        DETACH;
        NUM := 2;
        WHILE TRUE DO
        BEGIN
            IF OUTPUT == NONE THEN
            BEGIN
                IF DEBUG THEN
                BEGIN
                    OUTTEXT("GENERATE SENDS ");
                    OUTINT(NUM, 0);
                    OUTIMAGE;
                END;
                DETACH; ! SEND 'NUM' ;
            END ELSE
            BEGIN
                IF DEBUG THEN
                BEGIN
                    OUTTEXT("GENERATE SENDS ");
                    OUTINT(NUM, 0);
                    OUTTEXT(" TO FILTER("); OUTINT(OUTPUT.PRIME, 0);
                    OUTTEXT(")");
                    OUTIMAGE;
                END;
                OUTPUT.NUM := NUM;
                RESUME(OUTPUT);
            END;
            NUM := NUM + 1;
        END;
    END ELSE
    BEGIN
        ! COPY THE VALUES FROM CHANNEL 'IN' TO CHANNEL 'OUT', ;
        ! REMOVING THOSE DIVISIBLE BY 'PRIME'. ;
        DETACH;
        ! FILTER ;
        WHILE TRUE DO
        BEGIN
            INTEGER I;
            RESUME(INPUT);
            I := INPUT.NUM; ! RECEIVE VALUE FROM 'INPUT'. ;
            IF DEBUG THEN
            BEGIN
                OUTTEXT("FILTER("); OUTINT(PRIME, 0); OUTTEXT(") RECEIVES ");
                OUTINT(I, 0);
                OUTIMAGE;
            END;
            IF NOT MOD(I, PRIME) = 0 THEN
            BEGIN
                IF OUTPUT == NONE THEN
                BEGIN
                    IF DEBUG THEN
                    BEGIN
                        OUTTEXT("FILTER("); OUTINT(PRIME, 0);
                        OUTTEXT(") SENDS ");
                        OUTINT(I, 0);
                        OUTIMAGE;
                    END;
                    DETACH;
                END ELSE
                BEGIN
                    IF DEBUG THEN
                    BEGIN
                        OUTTEXT("FILTER("); OUTINT(PRIME, 0);
                        OUTTEXT(") SENDS ");
                        OUTINT(I, 0);
                        OUTTEXT(" TO FILTER("); OUTINT(OUTPUT.PRIME, 0);
                        OUTTEXT(")");
                        OUTIMAGE;
                    END;
                    OUTPUT.NUM := I; ! SEND 'I' TO 'OUT'. ;
                    RESUME(OUTPUT);
                END;
            END;
        END;
    END;
END;

! THE PRIME SIEVE: DAISY-CHAIN FILTER PROCESSES. ;
! MAIN BLOCK ;
    REF(FILTER) CH;
    INTEGER I, PRIME;
    DEBUG := TRUE;
    CH :- NEW FILTER(NONE, NONE, 0); ! LAUNCH GENERATE GOROUTINE. ;
    FOR I := 1 STEP 1 UNTIL 5 DO
    BEGIN
        REF(FILTER) CH1;
        RESUME(CH);
        PRIME := CH.NUM;
        IF DEBUG THEN OUTTEXT("MAIN BLOCK RECEIVES ");
        OUTINT(PRIME,0);
        OUTIMAGE;
        CH1 :- NEW FILTER(CH, NONE, PRIME);
        CH.OUTPUT :- CH1;
        CH :- CH1;
    END;
END;

```

Output:

```txt

GENERATE SENDS 2
MAIN BLOCK RECEIVES 2
GENERATE SENDS 3 TO FILTER(2)
FILTER(2) RECEIVES 3
FILTER(2) SENDS 3
MAIN BLOCK RECEIVES 3
GENERATE SENDS 4 TO FILTER(2)
FILTER(2) RECEIVES 4
GENERATE SENDS 5 TO FILTER(2)
FILTER(2) RECEIVES 5
FILTER(2) SENDS 5 TO FILTER(3)
FILTER(3) RECEIVES 5
FILTER(3) SENDS 5
MAIN BLOCK RECEIVES 5
GENERATE SENDS 6 TO FILTER(2)
FILTER(2) RECEIVES 6
GENERATE SENDS 7 TO FILTER(2)
FILTER(2) RECEIVES 7
FILTER(2) SENDS 7 TO FILTER(3)
FILTER(3) RECEIVES 7
FILTER(3) SENDS 7 TO FILTER(5)
FILTER(5) RECEIVES 7
FILTER(5) SENDS 7
MAIN BLOCK RECEIVES 7
GENERATE SENDS 8 TO FILTER(2)
FILTER(2) RECEIVES 8
GENERATE SENDS 9 TO FILTER(2)
FILTER(2) RECEIVES 9
FILTER(2) SENDS 9 TO FILTER(3)
FILTER(3) RECEIVES 9
GENERATE SENDS 10 TO FILTER(2)
FILTER(2) RECEIVES 10
GENERATE SENDS 11 TO FILTER(2)
FILTER(2) RECEIVES 11
FILTER(2) SENDS 11 TO FILTER(3)
FILTER(3) RECEIVES 11
FILTER(3) SENDS 11 TO FILTER(5)
FILTER(5) RECEIVES 11
FILTER(5) SENDS 11 TO FILTER(7)
FILTER(7) RECEIVES 11
FILTER(7) SENDS 11
MAIN BLOCK RECEIVES 11

```



## Smalltalk

A simple implementation that you can run in a workspace. It finds all the prime numbers up to and including <i>limit</i>—for the sake of example, up to and including 100.

```smalltalk
| potentialPrimes limit |
limit := 100.
potentialPrimes := Array new: limit.
potentialPrimes atAllPut: true.
2 to: limit sqrt do: [:testNumber |
    (potentialPrimes at: testNumber) ifTrue: [
        (testNumber * 2) to: limit by: testNumber do: [:nonPrime |
            potentialPrimes at: nonPrime put: false
        ]
    ]
].
2 to: limit do: [:testNumber |
    (potentialPrimes at: testNumber) ifTrue: [
        Transcript show: testNumber asString; cr
    ]
]
```



## SNOBOL4


Using strings instead of arrays, and the square/sqrt optimizations.


```SNOBOL4
        define('sieve(n)i,j,k,p,str,res') :(sieve_end)
sieve   i = lt(i,n - 1) i + 1 :f(sv1)
        str = str (i + 1) ' ' :(sieve)
sv1     str break(' ') . j span(' ') = :f(return)
        sieve = sieve j ' '
        sieve = gt(j ^ 2,n) sieve str :s(return) ;* Opt1
        res = ''
        str (arb ' ') @p ((j ^ 2) ' ') ;* Opt2
        str len(p) . res = ;* Opt2
sv2     str break(' ') . k  span(' ') = :f(sv3)
        res = ne(remdr(k,j),0) res k ' ' :(sv2)
sv3     str = res :(sv1)
sieve_end

*       # Test and display
        output = sieve(100)
end
```


Output:

```txt
2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97
```



## Stata

A program to create a dataset with a variable p containing primes up to a given number.

```stata
prog def sieve
	args n
	clear
	qui set obs `n'
	gen long p=_n
	gen byte a=_n>1
	forv i=2/`n' {
		if a[`i'] {
			loc j=`i'*`i'
			if `j'>`n' {
				continue, break
			}
			forv k=`j'(`i')`n' {
				qui replace a=0 in `k'
			}
		}
	}
	qui keep if a
	drop a
end
```


Example call

```stata
sieve 100
list in 1/10 // show only the first ten primes

     +----+
     |  p |
     |----|
  1. |  2 |
  2. |  3 |
  3. |  5 |
  4. |  7 |
  5. | 11 |
     |----|
  6. | 13 |
  7. | 17 |
  8. | 19 |
  9. | 23 |
 10. | 29 |
     +----+
```



###  Mata



```stata
mata
real colvector sieve(real scalar n) {
	real colvector a
	real scalar i, j
	if (n < 2) return(J(0, 1, .))
	a = J(n, 1, 1)
	a[1] = 0
	for (i = 1; i <= n; i++) {
		if (a[i]) {
			j = i*i
			if (j > n) return(select(1::n, a))
			for (; j <= n; j = j+i) a[j] = 0
		}
	}
}

sieve(10)
       1
    +-----+
  1 |  2  |
  2 |  3  |
  3 |  5  |
  4 |  7  |
    +-----+
end
```



## Swift


```swift
import Foundation

func primes(_ n: Int)
    -> UnfoldSequence<Int, (Int?, Bool)> {
  var sieve = Array<Bool>(repeating: true, count: n + 1)
  let lim = Int(sqrt(Double(n)))

  for i in 2...lim {
    if sieve[i] {
      for notPrime in stride(from: i*i, through: n, by: i) {
        sieve[notPrime] = false
      }
    }
  }

  return sequence(first: 2, next: { (p:Int) -> Int? in
    var np = p + 1
    while np <= n && !sieve[np] { np += 1}
    return np > n ? nil : np
  })
}

let range = 100000000

print("The primes up to 100 are:")
primes(100).forEach { print($0, "", terminator: "") }
print()

print("Found \(primes(1000000).reduce(0) { (a, _) in a + 1 }) primes to 1000000.")

let start = NSDate()
var answr = primes(range).reduce(0) { (a, _) in a + 1 }
let elpsd = -start.timeIntervalSinceNow

print("Found \(answr) primes to \(range).")

print(String(format: "This test took %.3f milliseconds.", elpsd * 1000))
```

{{output}}

```txt
The primes up to 100 are:
2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97
Found 78498 primes to 1000000.
Found 5761455 primes to 100000000.
This test took 5406.970 milliseconds.
```


As can be seen, this is quite slow for larger ranges as 100 million here (run on a 1.92 Gigahertz Intel x5-Z8350 for about 1800 CPU clock cycles per prime).

'''Alternative odds-only version'''

The most obvious two improvements are to sieve for only odds as two is the only even prime and to make the sieving array bit-packed so that instead of a whole 8-bit byte per number representation there, each is represented by just one bit; these two changes improved memory use by a factor of 16 and the better CPU cache locality more than compensates for the extra code required to implement bit packing as per the following code:

```swift
func soePackedOdds(_ n: Int) ->
    LazyMapSequence<UnfoldSequence<Int, (Int?, Bool)>, Int> {

  let lmti = (n - 3) / 2
  let size = lmti / 8 + 1
  var sieve = Array<UInt8>(repeating: 0, count: size)
  let sqrtlmti = (Int(sqrt(Double(n))) - 3) / 2

  for i in 0...sqrtlmti {
    if sieve[i >> 3] & (1 << (i & 7)) == 0 {
      let p = i + i + 3
      for c in stride(from: (i*(i+3)<<1)+3, through: lmti, by: p) {
        sieve[c >> 3] |= 1 << (c & 7)
      }
    }
  }

  return sequence(first: -1, next: { (i:Int) -> Int? in
      var ni = i + 1
      while ni <= lmti && sieve[ni >> 3] & (1 << (ni & 7)) != 0 { ni += 1}
      return ni > lmti ? nil : ni
    }).lazy.map { $0 < 0 ? 2 : $0 + $0 + 3 }
}
```


the output for the same testing (with `soePackedOdds` substituted for `primes`) is the same except that it is about 1.5 times faster or only 1200 cycles per prime.

These "one huge sieving array" algorithms are never going to be very fast for extended ranges (past about the CPU L2 cache size for this processor supporting a range of about eight million), and a page segmented approach should be taken as per the last of the unbounded algorithms below.

===Unbounded (Odds-Only) Versions===

To use Swift's "higher order functions" on the generated `Sequence`'s effectively, one needs unbounded (or only by the numeric range chosen for the implementation) sieves.  Many of these are incremental sieves that, instead of buffering a series of culled arrays, records the culling structure of the culling base primes (which should be a secondary stream of primes for efficiency) and produces the primes incrementally through reference and update of that structure.  Various structures may be chosen for this, as in a MinHeap Priority Queue, a Hash Dictionary, or a simple List tree structure as used in the following code:

```swift
import Foundation

func soeTreeFoldingOdds() -> UnfoldSequence<Int, (Int?, Bool)> {
  class CIS<T> {
    let head: T
    let rest: (() -> CIS<T>)
    init(_ hd: T, _ rst: @escaping (() -> CIS<T>)) {
      self.head = hd; self.rest = rst
    }
  }

  func merge(_ xs: CIS<Int>, _ ys: CIS<Int>) -> CIS<Int> {
    let x = xs.head; let y = ys.head
    if x < y { return CIS(x, {() in merge(xs.rest(), ys) }) }
    else { if y < x { return CIS(y, {() in merge(xs, ys.rest()) }) }
    else { return CIS(x, {() in merge(xs.rest(), ys.rest()) }) } }
  }

  func smults(_ p: Int) -> CIS<Int> {
    let inc = p + p
    func smlts(_ c: Int) -> CIS<Int> {
      return CIS(c, { () in smlts(c + inc) })
    }
    return smlts(p * p)
  }

  func allmults(_ ps: CIS<Int>) -> CIS<CIS<Int>> {
    return CIS(smults(ps.head), { () in allmults(ps.rest()) })
  }

  func pairs(_ css: CIS<CIS<Int>>) -> CIS<CIS<Int>> {
    let cs0 = css.head; let ncss = css.rest()
    return CIS(merge(cs0, ncss.head), { () in pairs(ncss.rest()) })
  }

  func cmpsts(_ css: CIS<CIS<Int>>) -> CIS<Int> {
    let cs0 = css.head
    return CIS(cs0.head, { () in merge(cs0.rest(), cmpsts(pairs(css.rest()))) })
  }

  func minusAt(_ n: Int, _ cs: CIS<Int>) -> CIS<Int> {
    var nn = n; var ncs = cs
    while nn >= ncs.head { nn += 2; ncs = ncs.rest() }
    return CIS(nn, { () in minusAt(nn + 2, ncs) })
  }

  func oddprms() -> CIS<Int> {
    return CIS(3, { () in minusAt(5, cmpsts(allmults(oddprms()))) })
  }

  var odds = oddprms()
  return sequence(first: 2, next: { _ in
    let p = odds.head; odds = odds.rest()
    return p
  })
}

let range = 100000000

print("The primes up to 100 are:")
soeTreeFoldingOdds().prefix(while: { $0 <= 100 })
  .forEach { print($0, "", terminator: "") }
print()

print("Found \(soeTreeFoldingOdds().lazy.prefix(while: { $0 <= 1000000 })
                .reduce(0) { (a, _) in a + 1 }) primes to 1000000.")

let start = NSDate()
let answr = soeTreeFoldingOdds().prefix(while: { $0 <= range })
              .reduce(0) { (a, _) in a + 1 }
let elpsd = -start.timeIntervalSinceNow

print("Found \(answr) primes to \(range).")

print(String(format: "This test took %.3f milliseconds.", elpsd * 1000))
```


The output is the same as for the above except that it is much slower at about 56,000 CPU clock cycles per prime even just sieving to ten million due to the many memory allocations/de-allocations.  It also has a O(n (log n) (log (log n))) asymptotic computational complexity (with the extra "log n" factor) that makes it slower with increasing range.  This makes this algorithm only useful up to ranges of a few million although it is adequate to solve trivial problems such as Euler Problem 10 of summing all the primes to two million.

Note that the above code is almost a pure functional version using immutability other than for the use of loops because Swift doesn't support Tail Call Optimization (TCO) in function recursion:  the loops do what TCO usually automatically does "under the covers".

'''Alternative version using a (mutable) Hash Dictionary'''

As the above code is slow due to memory allocations/de-allocations and the inherent extra "log n" term in the complexity, the following code uses a Hash Dictionary which has an average of O(1) access time (without the "log n" and uses mutability for increased seed so is in no way purely functional:

```swift
func soeDictOdds() -> UnfoldSequence<Int, Int> {
  var bp = 5; var q = 25
  var bps: UnfoldSequence<Int, Int>.Iterator? = nil
  var dict = [9: 6] // Dictionary<Int, Int>(9 => 6)
  return sequence(state: 2, next: { n in
    if n < 9 { if n < 3 { n = 3; return 2 }; defer {n += 2}; return n }
    while n >= q || dict[n] != nil {
      if n >= q {
        let inc = bp + bp
        dict[n + inc] = inc
        if bps == nil {
          bps = soeDictOdds().makeIterator()
          bp = (bps?.next())!; bp = (bps?.next())!; bp = (bps?.next())! // skip 2/3/5...
        }
        bp = (bps?.next())!; q = bp * bp // guaranteed never nil
      } else {
        let inc = dict[n] ?? 0
        dict[n] = nil
        var next = n + inc
        while dict[next] != nil { next += inc }
        dict[next] = inc
      }
      n += 2
    }
    defer { n += 2 }; return n
  })
}
```


It can be substituted in the above code just by substituting the `soeDictOdds` in three places in the testing code with the same output other than it is over four times faster or about 12,500 CPU clock cycles per prime.

'''Fast Bit-Packed Page-Segmented Version'''

An unbounded array-based algorithm can be written that combines the excellent cache locality of the second bounded version above but is unbounded by producing a sequence of sieved bit-packed arrays that are CPU cache size as required with a secondary stream of base primes used in culling produced in the same fashion, as in the following code:

```swift
import Foundation

typealias Prime = UInt64
typealias BasePrime = UInt32
typealias SieveBuffer = [UInt8]
typealias BasePrimeArray = [UInt32]

// the lazy list decribed problems don't affect its use here as
// it is only used here for its memoization properties and not consumed...
// In fact a consumed deferred list would be better off to use a CIS as above!

// a lazy list to memoize the progression of base prime arrays...
// there is some bug in Swift 4.2 that generating a LazyList<T> with a
// function and immediately using an extension method on it without
// first storing it to a variable results in mem seg fault for large
// ranges in the order of a million; in order to write a consuming
// function, one must write a function passing in a generator thunk, and
// immediately call a `makeIterator()` on it before storing, then doing a
// iteration on the iterator; doing a for on the immediately produced
// LazyList<T> (without storing it) also works, but this means we have to
// implement the "higher order functions" ourselves.
// this bug may have something to do with "move sematics".
class LazyList<T> : LazySequenceProtocol {
  internal typealias Thunk<T> = () -> T
  let head : T
  internal var _thnk: Thunk<LazyList<T>?>?
  lazy var tail: LazyList<T>? = {
    let tl = self._thnk?(); self._thnk = nil
    return tl
  }()
  init(_ hd: T, _ thnk: @escaping Thunk<LazyList<T>?>) {
    self.head = hd; self._thnk = thnk
  }
  struct LLSeqIter : IteratorProtocol, LazySequenceProtocol {
    @usableFromInline
    internal var _isfirst: Bool = true
    @usableFromInline
    internal var _current: LazyList<T>
    @inlinable // ensure that reference is not released by weak reference
    init(_ base: LazyList<T>) { self._current = base }
    @inlinable // can't be called by multiple threads on same LLSeqIter...
    mutating func next() -> T? {
      let curll = self._current
      if (self._isfirst) { self._isfirst = false; return curll.head }
      let ncur = curll.tail
      if (ncur == nil) { return nil }
      self._current = ncur!
      return ncur!.head
    }
    @inlinable
    func makeIterator() -> LLSeqIter {
      return LLSeqIter(self._current)
    }
  }
  @inlinable
  func makeIterator() -> LLSeqIter {
    return LLSeqIter(self)
  }
}

internal func makeCLUT() -> Array<UInt8> {
  var clut = Array(repeating: UInt8(0), count: 65536)
  for i in 0..<65536 {
    let v0 = ~i & 0xFFFF
    let v1 = v0 - ((v0 & 0xAAAA) >> 1)
    let v2 = (v1 & 0x3333) + ((v1 & 0xCCCC) >> 2)
    let v3 = (((((v2 & 0x0F0F) + ((v2 & 0xF0F0) >> 4)) &* 0x0101)) >> 8) & 31
    clut[i] = UInt8(v3)
  }
  return clut
}

internal let CLUT = makeCLUT()

internal func countComposites(_ cmpsts: SieveBuffer) -> Int {
  let len = cmpsts.count >> 1
  let clutp = UnsafePointer(CLUT) // for faster un-bounds checked access
  var bufp = UnsafeRawPointer(UnsafePointer(cmpsts))
                .assumingMemoryBound(to: UInt16.self)
  let plmt = bufp + len
  var count: Int = 0
  while (bufp < plmt) {
    count += Int(clutp[Int(bufp.pointee)])
    bufp += 1
  }
  return count
}

// converts an entire sieved array of bytes into an array of UInt32 primes,
// to be used as a source of base primes...
internal func composites2BasePrimeArray(_ low: BasePrime, _ cmpsts: SieveBuffer)
                                                          -> BasePrimeArray {
  let lmti = cmpsts.count << 3
  let len = countComposites(cmpsts)
  var rslt = BasePrimeArray(repeating: BasePrime(0), count: len)
  var j = 0
  for i in 0..<lmti {
    if (cmpsts[i >> 3] & (1 << (i & 7)) == UInt8(0)) {
      rslt[j] = low + BasePrime(i + i); j += 1
    }
  }
  return rslt
}

// do sieving work based on low starting value for the given buffer and
// the given lazy list of base prime arrays...
// uses pointers to avoid bounds checking for speed, but bounds are checked in code.
// uses an improved algorithm to maximize simple culling loop speed for
// the majority of cases of smaller base primes, only reverting to normal
// bit-packing operations for larger base primes...
// NOTE: a further optimization of maximum loop unrolling can later be
// implemented when warranted after maximum wheel factorization is implemented.
internal func sieveComposites(
      _ low: Prime, _ buf: SieveBuffer,
      _ bpas: LazyList<BasePrimeArray>) {
  let lowi = Int64((low - 3) >> 1)
  let len = buf.count
  let lmti = Int64(len << 3)
  let bufp = UnsafeMutablePointer(mutating: buf)
  let plen = bufp + len
  let nxti = lowi + lmti
  for bpa in bpas {
    for bp in bpa {
      let bp64 = Int64(bp)
      let bpi64 = (bp64 - 3) >> 1
      var strti = (bpi64 * (bpi64 + 3) << 1) + 3
      if (strti >= nxti) { return }
      if (strti >= lowi) { strti -= lowi }
      else {
        let r = (lowi - strti) % bp64
        strti = r == 0 ? 0 : bp64 - r
      }
      if (bp <= UInt32(len >> 3) && strti <= (lmti - 20 * bp64)) {
        let slmti = min(lmti, strti + (bp64 << 3))
        while (strti < slmti) {
          let msk = UInt8(1 << (strti & 7))
          var cp = bufp + Int(strti >> 3)
          while (cp < plen) {
              cp.pointee |= msk; cp += Int(bp64)
          }
          strti &+= bp64
        }
      }
      else {
        var c = strti
        let nbufp = UnsafeMutableRawPointer(bufp)
                      .assumingMemoryBound(to: Int32.self)
        while (c < lmti) {
            nbufp[Int(c >> 5)] |= 1 << (c & 31)
            c &+= bp64
        }
      }
    }
  }
}

// starts the secondary base primes feed with minimum size in bits set to 4K...
// thus, for the first buffer primes up to 8293,
// the seeded primes easily cover it as 97 squared is 9409...
// following used for fast clearing of SieveBuffer of multiple base size...
internal let clrbpseg = SieveBuffer(repeating: UInt8(0), count: 512)
internal func makeBasePrimeArrays() -> LazyList<BasePrimeArray> {
  var cmpsts = SieveBuffer(repeating: UInt8(0), count: 512)
  func nextelem(_ low: BasePrime, _ bpas: LazyList<BasePrimeArray>)
                                                -> LazyList<BasePrimeArray> {
    // calculate size so that the bit span is at least as big as the
    // maximum culling prime required, rounded up to minsizebits blocks...
    let rqdsz = 2 + Int(sqrt(Double(1 + low)))
    let sz = ((rqdsz >> 12) + 1) << 9 // size in bytes, blocks of 512 bytes
    if (sz > cmpsts.count) {
      cmpsts = SieveBuffer(repeating: UInt8(0), count: sz)
    }
    // fast clearing of the SieveBuffer array?
    for i in stride(from: 0, to: cmpsts.count, by: 512) {
      cmpsts.replaceSubrange(i..<i+512, with: clrbpseg)
    }
    sieveComposites(Prime(low), cmpsts, bpas)
    let arr = composites2BasePrimeArray(low, cmpsts)
    let nxt = low + BasePrime(cmpsts.count << 4)
    return LazyList(arr, { nextelem(nxt, bpas) })
  }
  // pre-seeding breaks recursive race,
  // as only known base primes used for first page...
  let preseedarr: [BasePrime] = [
    3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41
    , 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97 ]
  return
    LazyList(
      preseedarr,
      { nextelem(BasePrime(101), makeBasePrimeArrays()) })
}

// an iterable sequence over successive sieved buffer composite arrays,
// returning a tuple of the value represented by the lowest possible prime
// in the sieved composites array and the array itself;
// the array has a 16 Kilobytes minimum size (CPU L1 cache), but
// will grow so that the bit span is larger than the
// maximum culling base prime required, possibly making it larger than
// the L1 cache for large ranges, but still reasonably efficient using
// the L2 cache: very efficient up to about 16e9 range;
// reasonably efficient to about 2.56e14 for two Megabyte L2 cache = > 1 day...
internal let clrseg = SieveBuffer(repeating: UInt8(0), count: 16384)
func makeSievePages()
    -> UnfoldSequence<(Prime, SieveBuffer), ((Prime, SieveBuffer)?, Bool)> {
  let bpas = makeBasePrimeArrays()
  let cmpsts = SieveBuffer(repeating: UInt8(0), count: 16384)
  let low = Prime(3)
  sieveComposites(low, cmpsts, bpas)
  return sequence(first: (low, cmpsts), next: { (low, cmpsts) in
    var ncmpsts = cmpsts
    let rqdsz = 2 + Int(sqrt(Double(1 + low))) // problem with sqrt not exact past about 10^12!!!!!!!!!
    let sz = ((rqdsz >> 17) + 1) << 14 // size iin bytes, by chunks of 16384
    if (sz > ncmpsts.count) {
      ncmpsts = SieveBuffer(repeating: UInt8(0), count: sz)
    }
    // fast clearing of the SieveBuffer array?
    for i in stride(from: 0, to: ncmpsts.count, by: 16384) {
      ncmpsts.replaceSubrange(i..<i+16384, with: clrseg)
    }
    let nlow = low + Prime(ncmpsts.count << 4)
    sieveComposites(nlow, ncmpsts, bpas)
    return (nlow, ncmpsts)
  })
}

func countPrimesTo(_ range: Prime) -> Int64 {
  if (range < 3) { if (range < 2) { return Int64(0) }
                   else { return Int64(1) } }
  let rngi = Int64(range - 3) >> 1
  let clutp = UnsafePointer(CLUT) // for faster un-bounds checked access
  var count: Int64 = 1
  for sp in makeSievePages() {
    let (low, cmpsts) = sp; let lowi = Int64(low - 3) >> 1
    if ((lowi + Int64(cmpsts.count << 3)) > rngi) {
      let lsti = Int(rngi - lowi); let lstw = lsti >> 4
      let msk = UInt16(-2 << (lsti & 15))
      var bufp = UnsafeRawPointer(UnsafePointer(cmpsts))
                    .assumingMemoryBound(to: UInt16.self)
      let plmt = bufp + lstw
      while (bufp < plmt) {
        count += Int64(clutp[Int(bufp.pointee)]); bufp += 1
      }
      count += Int64(clutp[Int(bufp.pointee | msk)]);
      break;
    } else {
      count += Int64(countComposites(cmpsts))
    }
  }
  return count
}

// iterator of primes from the generated culled page segments...
struct PagedPrimesSeqIter: LazySequenceProtocol, IteratorProtocol {
  @inlinable
  init() {
    self._pgs = makeSievePages().makeIterator()
    self._cmpstsp = UnsafePointer(self._pgs.next()!.1)
  }
  @usableFromInline
  internal var _pgs: UnfoldSequence<(Prime, SieveBuffer), ((Prime, SieveBuffer)?, Bool)>
  @usableFromInline
  internal var _i = -2
  @usableFromInline
  internal var _low = Prime(3)
  @usableFromInline
  internal var _cmpstsp: UnsafePointer<UInt8>
  @usableFromInline
  internal var _lmt = 131072
  @inlinable
  mutating func next() -> Prime? {
    if self._i < -1 { self._i = -1; return Prime(2) }
    while true {
      repeat { self._i += 1 }
      while self._i < self._lmt &&
              (Int(self._cmpstsp[self._i >> 3]) & (1 << (self._i & 7))) != 0
      if self._i < self._lmt { break }
      let pg = self._pgs.next(); self._low = pg!.0
      let cmpsts = pg!.1; self._lmt = cmpsts.count << 3
      self._cmpstsp = UnsafePointer(cmpsts); self._i = -1
    }
    return self._low + Prime(self._i + self._i)
  }
  @inlinable
  func makeIterator() -> PagedPrimesSeqIter {
    return PagedPrimesSeqIter()
  }
  @inlinable
  var elements: PagedPrimesSeqIter {
    return PagedPrimesSeqIter()
  }
}

// sequence over primes using the above prime iterator from page iterator;
// unless doing something special with individual primes, usually unnecessary;
// better to do manipulations based on the composites bit arrays...
// takes at least as long to enumerate the primes as to sieve them...
func primesPaged() -> PagedPrimesSeqIter { return PagedPrimesSeqIter() }

let range = Prime(1000000000)

print("The first 25 primes are:")
primesPaged().prefix(25).forEach { print($0, "", terminator: "") }
print()

let start = NSDate()

let answr =
  countPrimesTo(range) // fast way, following enumeration way is slower...
//  primesPaged().prefix(while: { $0 <= range }).reduce(0, { a, _ in a + 1 })

let elpsd = -start.timeIntervalSinceNow

print("Found \(answr) primes up to \(range).")

print(String(format: "This test took %.3f milliseconds.", elpsd * 1000))
```

{{output}}

```txt
The first 25 primes are:
2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97
Found 50847534 primes up to 1000000000.
This test took 2004.007 milliseconds.
```


This produces similar output but is many many times times faster at about 75 CPU clock cycles per prime as used here to count the primes to a billion.  If one were to produce the answer by enumeration using the commented out `primesPaged()` function, the time to enumerate the results is about the same as the time to actually do the work of culling, so the example `countPrimesTo` function that does high-speed counting of found packed bits is implemented to eliminate the enumeration.  For most problems over larger ranges, this approach is recommended, and could be used for summing the primes, finding first instances of maximum prime gaps, prime pairs, triples, etc.

Further optimizations as in maximum wheel factorization (a further about five times faster), multi-threading (for a further multiple of the effective number of cores used), maximum loop unrolling (about a factor of two for smaller base primes), and other techniques for higher ranges (above 16 billion in this case) can be used with increasing code complexity, but there is little point when using prime enumeration as output:  ie. one could reduce the composite number culling time to zero but it would still take about 2.8 seconds to enumerate the results over the billion range in the case of this processor.


## Tailspin


```tailspin

templates sieve
  def limit: $;
  @: [ 2..$limit ];
  1 -> #
  $@ !

  <..$@::length ?($@($) * $@($) <..$limit>)>
    templates sift
      def prime: $;
      @: $prime * $prime;
      @sieve: [ $@sieve... -> # ];
      <..~$@>
        $ !
      <$@~..>
        @: $@ + $prime;
        $ -> #
    end sift

    $@($) -> sift !
    $ + 1 -> #
end sieve

1000 -> sieve ...->  '$; ' -> !OUT::write

```

{{out}}

```txt

2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 101 103 107 109 113 127 131 137 139 149 151 157 163 167 173 179 181 191 193 197 199 211 223 227 229 233 239 241 251 257 263 269 271 277 281 283 293 307 311 313 317 331 337 347 349 353 359 367 373 379 383 389 397 401 409 419 421 431 433 439 443 449 457 461 463 467 479 487 491 499 503 509 521 523 541 547 557 563 569 571 577 587 593 599 601 607 613 617 619 631 641 643 647 653 659 661 673 677 683 691 701 709 719 727 733 739 743 751 757 761 769 773 787 797 809 811 821 823 827 829 839 853 857 859 863 877 881 883 887 907 911 919 929 937 941 947 953 967 971 977 983 991 997

```


Better version using the mutability of the @-state to just update a primality flag

```tailspin

templates sieve
  def limit: $;
  @: [ 1..$limit -> 1 ];
  @(1): 0;
  2..$limit -> #
  $@ -> [i](<1> $i !) !

  <?($@($) <1>)>
    def prime2: $ * $;
    $prime2..$limit:$ -> @sieve($): 0;
end sieve

1000 -> sieve... ->  '$; ' -> !OUT::write

```

{{out}}

```txt

2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 101 103 107 109 113 127 131 137 139 149 151 157 163 167 173 179 181 191 193 197 199 211 223 227 229 233 239 241 251 257 263 269 271 277 281 283 293 307 311 313 317 331 337 347 349 353 359 367 373 379 383 389 397 401 409 419 421 431 433 439 443 449 457 461 463 467 479 487 491 499 503 509 521 523 541 547 557 563 569 571 577 587 593 599 601 607 613 617 619 631 641 643 647 653 659 661 673 677 683 691 701 709 719 727 733 739 743 751 757 761 769 773 787 797 809 811 821 823 827 829 839 853 857 859 863 877 881 883 887 907 911 919 929 937 941 947 953 967 971 977 983 991 997

```



## Tcl


```tcl
package require Tcl 8.5

proc sieve n {
    if {$n < 2} {return {}}

    # create a container to hold the sequence of numbers.
    # use a dictionary for its speedy access (like an associative array)
    # and for its insertion order preservation (like a list)
    set nums [dict create]
    for {set i 2} {$i <= $n} {incr i} {
        # the actual value is never used
        dict set nums $i ""
    }

    set primes [list]
    while {[set nextPrime [lindex [dict keys $nums] 0]] <= sqrt($n)} {
        dict unset nums $nextPrime
        for {set i [expr {$nextPrime ** 2}]} {$i <= $n} {incr i $nextPrime} {
            dict unset nums $i
        }
        lappend primes $nextPrime
    }
    return [concat $primes [dict keys $nums]]
}

puts [sieve 100]   ;# 2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97
```



Summary :/* {{header|TI-83 BASIC}} */

=={{header|TI-83 BASIC}}==

```ti83b
Input "Limit:",N
N→Dim(L1)
For(I,2,N)
1→L1(I)
End
For(I,2,SQRT(N))
If L1(I)=1
Then
For(J,I*I,N,I)
0→L1(J)
End
End
End
For(I,2,N)
If L1(I)=1
Then
Disp i
End
End
ClrList L1
```



## UNIX Shell


### With array

{{works with|zsh}}

```bash
#!/usr/bin/zsh

function primes() {
	typeset -a a
	typeset i j

	a[1]=""
	for (( i = 2; i <= $1; i++ )); do
		a[$i]=$i
	done

	for (( i = 2; i * i <= $1; i++ )); do
		if [[ ! -z $a[$i] ]]; then
			for (( j = i * i; j <= $1; j += i )); do
				a[$j]=""
			done
		fi
	done
	print $a
}

primes 1000
```


{{works with|bash}}
{{works with|ksh93}}
{{works with|pdksh}}


```bash
function primes {
	typeset a i=2 j m=$1
	# No for (( ... )) loop in pdksh. Use while loop.
	while (( i <= m )); do
		a[$i]=$i
		(( i++ ))
	done

	i=2
	while (( j = i * i, j <= m )); do
		if [[ -n ${a[$i]} ]]; then
			while (( j <= m )); do
				unset a[$j]
				(( j += i ))
			done
		fi
		(( i++ ))
	done
	# No print command in bash. Use echo command.
	echo ${a[*]}
}

primes 1000
```


Both scripts output a single long line.

```txt
2 3 5 7 11 13 17 19 23 ... 971 977 983 991 997
```



### Using variables as fake array

[[Bourne Shell]] and [[Almquist Shell]] have no arrays. This script works with [[bash]] or [[dash]] (standard shell in Ubuntu), but uses no specifics of the shells, so it works with plain Bourne Shell as well.

{{works with|Bourne Shell}}

```bash
#! /bin/sh

LIMIT=1000

# As a workaround for missing arrays, we use variables p2, p3, ...,
# p$LIMIT, to represent the primes. Values are true or false.
#   eval p$i=true     # Set value.
#   eval \$p$i        # Run command: true or false.
#
# A previous version of this script created a temporary directory and
# used files named 2, 3, ..., $LIMIT to represent the primes. We now use
# variables so that a killed script does not leave extra files. About
# performance, variables are about as slow as files.

i=2
while [ $i -le $LIMIT ]
do
    eval p$i=true               # was touch $i
    i=`expr $i + 1`
done

i=2
while
    j=`expr $i '*' $i`
    [ $j -le $LIMIT ]
do
    if eval \$p$i               # was if [ -f $i ]
    then
        while [ $j -le $LIMIT ]
        do
            eval p$j=false      # was rm -f $j
            j=`expr $j + $i`
        done
    fi
    i=`expr $i + 1`
done

# was echo `ls|sort -n`
echo `i=2
      while [ $i -le $LIMIT ]; do
          eval \\$p$i && echo $i
          i=\`expr $i + 1\`
      done`
```



### With piping

{{incorrect|Bash|This version uses rem testing and so is a trial division algorithm, not a sieve of Eratosthenes.}}
Note: McIlroy misunderstood the Sieve of Eratosthenes as did many of his day including David Turner (1975); see  [https://en.m.wikipedia.org/wiki/Sieve_of_Eratosthenes Sieve of Eratosthenes article on Wikipedia]


This is an elegant script by [https://en.wikipedia.org/wiki/Douglas_McIlroy M. Douglas McIlroy], one of the founding fathers of UNIX.

This implementation is explained in his paper [https://www.cs.dartmouth.edu/~doug/sieve/sieve.pdf "Coroutine prime number sieve"] (2014).

{{works with|Bourne Shell}}

```bash
sourc() {
    seq 2 1000
}

cull() {
    while
        read p || exit
    do
        (($p % $1 != 0)) && echo $p
    done
}

sink() {
    read p || exit
    echo $p
    cull $p | sink &
}

sourc | sink
```


This version works by piping 1s and 0s through ''sed''. The string of 1s and 0s represents the array of primes.

{{works with|Bourne Shell}}

```bash
# Fill $1 characters with $2.
fill () {
	# This pipeline would begin
	#   head -c $1 /dev/zero | ...
	# but some systems have no head -c. Use dd.
	dd if=/dev/zero bs=$1 count=1 2>/dev/null | tr '\0' $2
}

filter () {
	# Use sed to put an 'x' after each multiple of $1, remove
	# first 'x', and mark non-primes with '0'.
	sed -e s/$2/\&x/g -e s/x// -e s/.x/0/g | {
		if expr $1 '*' $1 '<' $3 > /dev/null; then
			filter `expr $1 + 1` .$2 $3
		else
			cat
		fi
	}
}

# Generate a sequence of 1s and 0s indicating primality.
oz () {
	fill $1 1 | sed s/1/0/ | filter 2 .. $1
}

# Echo prime numbers from 2 to $1.
prime () {
	# Escape backslash inside backquotes. sed sees one backslash.
	echo `oz $1 | sed 's/./&\\
/g' | grep -n 1 | sed s/:1//`
}

prime 1000
```


=
## C Shell
=
{{trans|CMake}}

```csh
# Sieve of Eratosthenes: Echoes all prime numbers through $limit.
@ limit = 80

if ( ( $limit * $limit ) / $limit != $limit ) then
	echo limit is too large, would cause integer overflow.
	exit 1
endif

# Use $prime[2], $prime[3], ..., $prime[$limit] as array of booleans.
# Initialize values to 1 => yes it is prime.
set prime=( `repeat $limit echo 1` )

# Find and echo prime numbers.
@ i = 2
while ( $i <= $limit )
	if ( $prime[$i] ) then
		echo $i

		# For each multiple of i, set 0 => no it is not prime.
		# Optimization: start at i squared.
		@ m = $i * $i
		while ( $m <= $limit )
			set prime[$m] = 0
			@ m += $i
		end
	endif
	@ i += 1
end
```


## Ursala

{{incorrect|Ursala|It probably (remainder) uses rem testing and so is a trial division algorithm, not a sieve of Eratosthenes.}}
with no optimizations

```Ursala
#import nat

sieve = ~<{0,1}&& iota; @NttPX ~&r->lx ^/~&rhPlC remainder@rlX~|@r
```

test program:

```Ursala
#cast %nL

example = sieve 50
```
{{out}}
 <2,3,5,7,11,13,17,19,23,29,31,37,41,43,47>


## Vala

{{libheader|Gee}}Without any optimizations:

```vala
using Gee;

ArrayList<int> primes(int limit){
	var sieve = new ArrayList<bool>();
	var prime_list = new ArrayList<int>();

	for(int i = 0; i <= limit; i++){
		sieve.add(true);
	}

	sieve[0] = false;
	sieve[1] = false;

	for (int i = 2; i <= limit/2; i++){
		if (sieve[i] != false){
			for (int j = 2; i*j <= limit; j++){
				sieve[i*j] = false;
			}
		}
	}

	for (int i = 0; i < sieve.size; i++){
		if (sieve[i] != false){
			prime_list.add(i);
		}
	}

	return prime_list;
} // end primes

public static void main(){
	var prime_list = primes(50);

	foreach(var prime in prime_list)
		stdout.printf("%s ", prime.to_string());

	stdout.printf("\n");
}
```
{{out}
 2 3 5 7 11 13 17 19 23 29 31 37 41 43 47

## VAX Assembly


```VAX Assembly
                           000F4240  0000     1 n=1000*1000
                               0000  0000     2 .entry	main,0
                            7E 7CFD  0002     3 	clro	-(sp)			;result buffer
                            5E   DD  0005     4 	pushl	sp			;pointer to buffer
                            10   DD  0007     5 	pushl	#16			;descriptor -> len of buffer
                                     0009     6
                            02   DD  0009     7 	pushl	#2			;1st candidate
                                     000B     8 test:
                 09 46'AF   6E   E1  000B     9 	bbc	(sp), b^bits, found	;bc - bit clear
                                     0010    10 next:
           F3 6E   000F4240 8F   F2  0010    11         aoblss  #n, (sp), test		;+1: limit,index
                                 04  0018    12         ret
                                     0019    13 found:
                         04 AE   7F  0019    14 	pushaq	4(sp)			;-> descriptor by ref
                         04 AE   DF  001C    15 	pushal	4(sp)			;-> prime on stack by ref
              00000000'GF   02   FB  001F    16 	calls	#2, g^ots$cvt_l_ti	;convert integer to string
                         04 AE   7F  0026    17 	pushaq	4(sp)			;
              00000000'GF   01   FB  0029    18 	calls	#1, g^lib$put_output	;show result
                                     0030    19
                       53   6E   D0  0030    20 	movl	(sp), r3
                                     0033    21 mult:
    0002 53   6E   000F4240 8F   F1  0033    22 	acbl    #n, (sp), r3, set_mult	;limit,add,index
                            D1   11  003D    23 	brb	next
                                     003F    24 set_mult:				;set bits for multiples
                 EF 46'AF   53   E2  003F    25 	bbss	r3, b^bits, mult	;branch on bit set & set
                            ED   11  0044    26 	brb	mult
                                     0046    27
                           0001E892  0046    28 bits:	.blkl	<n+2+31>/32
                                     E892    29 .end	main
```



## VBScript

To run in console mode with cscript.

```vb

    Dim sieve()
	If WScript.Arguments.Count>=1 Then
	    n = WScript.Arguments(0)
	Else
	    n = 99
	End If
    ReDim sieve(n)
    For i = 1 To n
        sieve(i) = True
    Next
    For i = 2 To n
        If sieve(i) Then
            For j = i * 2 To n Step i
                sieve(j) = False
            Next
        End If
    Next
    For i = 2 To n
        If sieve(i) Then WScript.Echo i
    Next

```



## Vedit macro language


This implementation uses an edit buffer as an array for flags.
After the macro has been run, you can see how the primes are located in the array.
Primes are marked with 'P' and non-primes with '-'. The first character position represents number 0.


```txt

#10 = Get_Num("Enter number to search to: ", STATLINE)
Buf_Switch(Buf_Free)                    // Use edit buffer as flags array
Ins_Text("--")                          // 0 and 1 are not primes
Ins_Char('P', COUNT, #10-1)             // init rest of the flags to "prime"
for (#1 = 2; #1*#1 < #10; #1++) {
    Goto_Pos(#1)
    if (Cur_Char=='P') {                // this is a prime number
        for (#2 = #1*#1; #2 <= #10; #2 += #1) {
            Goto_Pos(#2)
            Ins_Char('-', OVERWRITE)
        }
    }
}

```


Sample output showing numbers in range 0 to 599.

```txt

--PP-P-P---P-P---P-P---P-----P-P-----P---P-P---P-----P-----P
-P-----P---P-P-----P---P-----P-------P---P-P---P-P---P------
-------P---P-----P-P---------P-P-----P-----P---P-----P-----P
-P---------P-P---P-P-----------P-----------P---P-P---P-----P
-P---------P-----P-----P-----P-P-----P---P-P---------P------
-------P---P-P---P-------------P-----P---------P-P---P-----P
-------P-----P-----P---P-----P-------P---P-------P---------P
-P---------P-P-----P---P-----P-------P---P-P---P-----------P
-------P---P-------P---P-----P-----------P-P----------------
-P-----P---------P-----P-----P-P-----P---------P-----P-----P

```


## VBA

Using Excel
```vb
 Sub primes()
'BRRJPA
'Prime calculation for VBA_Excel
'p is the superior limit of the range calculation
'This example calculates from 2 to 100000 and print it
'at the collum A


p = 100000

Dim nprimes(1 To 100000) As Integer
b = Sqr(p)

For n = 2 To b

    For k = n * n To p Step n
        nprimes(k) = 1

    Next k
Next n


For a = 2 To p
    If nprimes(a) = 0 Then
      c = c + 1
      Range("A" & c).Value = a

    End If
 Next a

End Sub </lang >


## Visual Basic

'''Works with:''' VB6

```vb
Sub Eratost()
    Dim sieve() As Boolean
    Dim n As Integer, i As Integer, j As Integer
    n = InputBox("limit:", n)
    ReDim sieve(n)
    For i = 1 To n
        sieve(i) = True
    Next i
    For i = 2 To n
        If sieve(i) Then
            For j = i * 2 To n Step i
                sieve(j) = False
            Next j
        End If
    Next i
    For i = 2 To n
        If sieve(i) Then Debug.Print i
    Next i
End Sub 'Eratost
```



## Visual Basic .NET


```vbnet
Dim n As Integer, k As Integer, limit As Integer
Console.WriteLine("Enter number to search to: ")
limit = Console.ReadLine
Dim flags(limit) As Integer
For n = 2 To Math.Sqrt(limit)
    If flags(n) = 0 Then
        For k = n * n To limit Step n
            flags(k) = 1
        Next k
    End If
Next n

' Display the primes
For n = 2 To limit
    If flags(n) = 0 Then
        Console.WriteLine(n)
    End If
Next n
```


### Alternate

Since the sieves are being removed only above the current iteration, the separate loop for display is unnecessary.  And no '''Math.Sqrt()''' needed.  Also, input is from command line parameter instead of Console.ReadLine.

```vbnet
Module Module1
    Sub Main(args() As String)
        Dim lmt As Integer = 500
        If args.Count > 0 Then Integer.TryParse(args(0), lmt)
        Dim flags(lmt) As Boolean           ' non-primes are true in this array.
        For n = 2 To lmt
            If Not flags(n) Then            ' a prime was found,
                Console.Write($"{n} ")      ' so show it,
                For k = n * n To lmt Step n ' and eliminate any multiple of at it's square and beyond
                    flags(k) = True
                Next
            End If
        Next
    End Sub
End Module
```
{{out}}
```txt
2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 101 103 107 109 113 127 131 137 139 149 151 157 163 167 173 179 181 191 193 197 199 211 223 227 229 233 239 241 251 257 263 269 271 277 281 283 293 307 311 313 317 331 337 347 349 353 359 367 373 379 383 389 397 401 409 419 421 431 433 439 443 449 457 461 463 467 479 487 491 499
```



## Vorpal


```vorpal
self.print_primes = method(m){
   p = new()
   p.fill(0, m, 1, true)

   count = 0
   i = 2
   while(i < m){
      if(p[i] == true){
         p.fill(i+i, m, i, false)
         count = count + 1
      }
      i = i + 1
   }
   ('primes: ' + count + ' in ' + m).print()
   for(i = 2, i < m, i = i + 1){
      if(p[i] == true){
         ('' + i + ', ').put()
      }
   }
   ''.print()
}

self.print_primes(100)
```
{{out|Result}}
 primes: 25 in 100
 2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97,


## WebAssembly

 (module
  (import "js" "print" (func $print (param i32)))
  (memory 4096)

  (func $sieve (export "sieve") (param $n i32)
    (local $i i32)
    (local $j i32)

    (set_local $i (i32.const 0))
    (block $endLoop
      (loop $loop
        (br_if $endLoop (i32.ge_s (get_local $i) (get_local $n)))
        (i32.store8 (get_local $i) (i32.const 1))
        (set_local $i (i32.add (get_local $i) (i32.const 1)))
        (br $loop)))

    (set_local $i (i32.const 2))
    (block $endLoop
      (loop $loop
        (br_if $endLoop (i32.ge_s (i32.mul (get_local $i) (get_local $i))
                                  (get_local $n)))
        (if (i32.eq (i32.load8_s (get_local $i)) (i32.const 1))
          (then
            (set_local $j (i32.mul (get_local $i) (get_local $i)))
            (block $endInnerLoop
              (loop $innerLoop
                (i32.store8 (get_local $j) (i32.const 0))
                (set_local $j (i32.add (get_local $j) (get_local $i)))
                (br_if $endInnerLoop (i32.ge_s (get_local $j) (get_local $n)))
                (br $innerLoop)))))
        (set_local $i (i32.add (get_local $i) (i32.const 1)))
        (br $loop)))

    (set_local $i (i32.const 2))
    (block $endLoop
      (loop $loop
        (if (i32.eq (i32.load8_s (get_local $i)) (i32.const 1))
          (then
            (call $print (get_local $i))))
        (set_local $i (i32.add (get_local $i) (i32.const 1)))
        (br_if $endLoop (i32.ge_s (get_local $i) (get_local $n)))
        (br $loop)))))


## Xojo

Place the following in the '''Run''' event handler of a Console application:

```Xojo
Dim limit, prime, i As Integer
Dim s As String
Dim t As Double
Dim sieve(100000000) As Boolean

REM Get the maximum number
While limit<1 Or limit > 100000000
  Print("Max number? [1 to 100000000]")
  s = Input
  limit = CDbl(s)
Wend

REM Do the calculations
t = Microseconds
prime = 2
While prime^2 < limit
  For i = prime*2 To limit Step prime
    sieve(i) = True
  Next
  Do
    prime = prime+1
  Loop Until Not sieve(prime)
Wend
t = Microseconds-t
Print("Compute time = "+Str(t/1000000)+" sec")
Print("Press Enter...")
s = Input

REM Display the prime numbers
For i = 1 To limit
  If Not sieve(i) Then Print(Str(i))
Next
s = Input
```


{{out}}

```txt
Max number? [1 to 100000000]
1000
Compute time = 0.0000501 sec
Press Enter...

1
2
3
5
7
11
13
17
19
23
29
31
37
41
43
47
...

```


This version uses a dynamic array and can use (a lot) less memory. It's (a lot) slower too.
Since Booleans are manually set to True, the algorithm makes more sense.

```Xojo
Dim limit, prime, i As Integer
Dim s As String
Dim t As Double
Dim sieve() As Boolean

REM Get the maximum number and define array
While limit<1 Or limit > 2147483647
  Print("Max number? [1 to 2147483647]")
  s = Input
  limit = CDbl(s)
Wend
t = Microseconds
For i = 0 To Limit
   Sieve.Append(True)
Next
t = Microseconds-t
Print("Memory allocation time = "+Str(t/1000000)+" sec")

REM Do the calculations
t = Microseconds
prime = 2
While prime^2 < limit
  For i = prime*2 To limit Step prime
    sieve(i) = False
  Next
  Do
    prime = prime+1
  Loop Until sieve(prime)
Wend
t = Microseconds-t
Print("Compute time = "+Str(t/1000000)+" sec")
Print("Press Enter...")
s = Input

REM Display the prime numbers
For i = 1 To limit
  If sieve(i) Then Print(Str(i))
Next
s = Input
```


{{out}}

```txt
Max number? [1 to 2147483647]
1000
Memory allocation time = 0.0000296 sec
Compute time = 0.0000501 sec
Press Enter...

1
2
3
5
7
11
13
17
19
23
29
31
37
41
43
47
...

```



## XPL0


```XPL0
include c:\cxpl\codes;                  \intrinsic 'code' declarations
int  Size, Prime, I, Kill;
char Flag;
[Size:= IntIn(0);
Flag:= Reserve(Size+1);
for I:= 2 to Size do Flag(I):= true;
for I:= 2 to Size do
    if Flag(I) then                     \found a prime
        [Prime:= I;
        IntOut(0, Prime);  CrLf(0);
        Kill:= Prime + Prime;           \first multiple to kill
        while Kill <= Size do
                [Flag(Kill):= false;    \zero a non-prime
                Kill:= Kill + Prime;    \next multiple
                ];
        ];
]
```
{{out|Example output}}
```txt
20
2
3
5
7
11
13
17
19
```



## Yabasic


```Yabasic
#!/usr/bin/yabasic

// ---------------------------
// Prime Sieve Benchmark --
// "Shootout" Version    --
// ---------------------------
// usage:
//     yabasic sieve8k.yab 90000


SIZE = 8192
ONN = 1 : OFF = 0
dim flags(SIZE)

sub main()

    cmd = peek("arguments")
    if cmd = 1 then
       iterations = val(peek$("argument"))
       if iterations = 0 then print "Argument wrong. Done 1000." : iterations = 1000 end if
    else
       print "1000 iterations."
       iterations = 1000
    end if

    for iter = 1 to iterations
        count = 0
        for n= 1 to SIZE : flags(n) = ONN: next n
        for i = 2 to SIZE
            if flags(i) = ONN then
               let k = i + i
               if k < SIZE then
                 for k = k to SIZE step i
                    flags(k) = OFF
                 next k
               end if
               count = count + 1
            end if
        next i
    next iter
    print "Count: ", count  // 1028
end sub

clear screen

print "Prime Sieve Benchmark\n"

main()

t = val(mid$(time$,10))

print "time: ", t, "\n"
print peek("millisrunning")
```



## zkl


```zkl
fcn sieve(limit){
   composite:=Data(limit+1).fill(1);  // bucket of bytes set to 1 (prime)
   (2).pump(limit.toFloat().sqrt()+1, Void,  // Void==no results, just loop
       composite.get, Void.Filter,	// if prime, zero multiples
      'wrap(n){ [n*n..limit,n].pump(Void,composite.set.fp1(0)) }); //composite[n*p]=0
   (2).filter(limit-1,composite.get); // bytes still 1 are prime
}
sieve(53).println();
```

The pump method is just a loop, passing results from action to action
and collecting the results (ie a minimal state machine). Pumping to Void
means don't collect. The Void.Filter action means if result.toBool() is
False, skip else get the source input (pre any action) and pass that to
the next action.
Here, the first filter checks the table if src is prime, if so, the
third action take the prime and does some side effects.
{{out}}
 L(2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53)
