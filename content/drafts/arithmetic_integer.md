+++
title = "Arithmetic/Integer"
description = ""
date = 2019-10-15T07:43:29Z
aliases = []
[extra]
id = 2069
[taxonomies]
categories = []
tags = []
+++

{{Task|Basic language learning}}
[[Category:Arithmetic operations]]
[[Category:Arithmetic]]
{{basic data operation}} [[Category:Simple]]

;Task:
Get two integers from the user,    and then (for those two integers), display their:
::::*   sum
::::*   difference
::::*   product
::::*   integer quotient
::::*   remainder
::::*   exponentiation   (if the operator exists)



Don't include error handling.

For quotient, indicate how it rounds   (e.g. towards zero, towards negative infinity, etc.).

For remainder, indicate whether its sign matches the sign of the first operand or of the second operand, if they are different.





## 0815


```0815

|~>|~#:end:>
<:61:x<:3d:=<:20:$==$~$=${~>%<:2c:~$<:20:~$
<:62:x<:3d:=<:20:$==$~$=${~>%<:a:~$$
<:61:x<:2b:=<:20:$==$~$=$<:62:x<:3d:=<:20:$==$~$=${x{x~>~>~+%<:a:~$
<:61:x<:2d:=<:20:$==$~$=$<:62:x<:3d:=<:20:$==$~$=${x{x~>~>~-%<:a:~$
<:61:x<:2a:=<:20:$==$~$=$<:62:x<:3d:=<:20:$==$~$=${x{x~>~>~*%<:a:~$
<:61:x<:2f:=<:20:$==$~$=$<:62:x<:3d:=<:20:$==$~$=${x{x~>~>~/%<:a:~$
<:61:x<:25:=<:20:$==$~$=$<:62:x<:3d:=<:20:$==$~$=${x{x~>~>~/=%<:a:~$
{~>>{x<:1:-^:u:
<:61:x<:5e:=<:20:$==$~$$=$<:62:x<:3D:=<:20:$==$~$=${{~%#:end:
}:u:=>{x{=>~*>{x<:2:-#:ter:
}:ml:x->{x{=>~*>{x<:1:-#:ter:^:ml:
}:ter:<:61:x<:5e:=<:20:$==$~$$=$<:62:x<:3D:=<:20:$==$~$=${{~%

```

{{Out}}

```txt

a = 6, b = 4

a + b = A
a - b = 2
a * b = 18
a / b = 1
a % b = 2
a ^^ b = 510

```



## 11l


```11l
V a = Int(input())
V b = Int(input())

print(‘a + b = ’(a + b))
print(‘a - b = ’(a - b))
print(‘a * b = ’(a * b))
print(‘a / b = ’(a I/ b))
print(‘a % b = ’(a % b))
print(‘a ^ b = ’(a ^ b))
```



## 360 Assembly

From the principles of operation: Operands are signed and 32 bits long.
Negative quantities are held in two's-complement form.

'''Multiplication:'''

The product of the multiplier (the second operand) and the multiplicand
(the first operand) replaces the multiplicand. Both multiplier and
multiplicand are 32-bit signed integers. The product is always a 64-bit
signed integer and occupies an even/odd register pair.

'''Division:'''

The dividend (first operand) is divided by the divisor (second operand)
and replaced by the quotient and remainder. The dividend is a 64-bit
signed integer and occupies the even/odd pair of registers.
A 32-bit signed remainder and a 32-bit signed quotient replace the dividend
in the even-numbered and odd-numbered registers, respectively.
The sign of the quotient is determined by the rules of algebra.
The remainder has the same sign as the dividend.

```360asm
*        Arithmetic/Integer        04/09/2015
ARITHINT CSECT
         USING  ARITHINT,R12
         LR     R12,R15
ADD      L      R1,A
         A      R1,B               r1=a+b
         XDECO  R1,BUF
         MVI    BUF,C'+'
         XPRNT  BUF,12
SUB      L      R1,A
         S      R1,B               r1=a-b
         XDECO  R1,BUF
         MVI    BUF,C'-'
         XPRNT  BUF,12
MUL      L      R1,A
         M      R0,B               r0r1=a*b
         XDECO  R1,BUF             so r1 has the lower part
         MVI    BUF,C'*'
         XPRNT  BUF,12
DIV      L      R0,A
         SRDA   R0,32              to shift the sign
         D      R0,B               r1=a/b and r0 has the remainder
         XDECO  R1,BUF             so r1 has quotient
         MVI    BUF,C'/'
         XPRNT  BUF,12
MOD      L      R0,A
         SRDA   R0,32              to shift the sign
         D      R0,B               r1=a/b and r0 has the remainder
         XDECO  R0,BUF             so r0 has the remainder
         MVI    BUF,C'R'
         XPRNT  BUF,12
RETURN   XR     R15,R15
         BR     R14
         CNOP   0,4
A        DC     F'53'
B        DC     F'11'
BUF      DC     CL12' '
         YREGS
         END    ARITHINT
```

Inputs are in the code: a=53, b=11
{{out}}

```txt

+         64
-         42
*        583
/          4
R          9

```



## 6502 Assembly

Code is called as a subroutine (i.e. JSR Arithmetic).  Specific OS/hardware routines for user input and printing are left unimplemented.

```6502asm
Arithmetic:	PHA			;push accumulator and X register onto stack
		TXA
		PHA
		JSR GetUserInput	;routine not implemented
		;two integers now in memory locations A and B
		;addition
		LDA A
		CLC
		ADC B
		JSR DisplayAddition	;routine not implemented

		;subtraction
		LDA A
		SEC
		SBC B
		JSR DisplaySubtraction	;routine not implemented

		;multiplication - overflow not handled
		LDA A
		LDX B
Multiply:	CLC
		ADC A
		DEX
		BNE Multiply
		JSR DisplayMultiply	;routine not implemented

		;division	- rounds up
		LDA A
		LDX #0
		SEC
Divide:		INX
		SBC B
		BCS Divide
		TXA			;get result into accumulator
		JSR DisplayDivide	;routine not implemented

		;modulus
		LDA A
		SEC
Modulus:	SBC B
		BCS Modulus
		ADC B
		JSR DisplayModulus	;routine not implemented

		PLA			;restore accumulator and X register from stack
		TAX
		PLA
		RTS			;return from subroutine
```

The 6502 has no opcodes for multiplication, division, or modulus; the routines for multiplication, division, and modulus given above can be heavily optimized at the expense of some clarity.


## ABAP


```ABAP
report zz_arithmetic no standard page heading.

" Read in the two numbers from the user.
selection-screen begin of block input.
  parameters: p_first type i,
              p_second type i.
selection-screen end of block input.

" Set the text value that is displayed on input request.
at selection-screen output.
  %_p_first_%_app_%-text  = 'First Number: '.
  %_p_second_%_app_%-text = 'Second Number: '.

end-of-selection.
  data: lv_result type i.
  lv_result = p_first + p_second.
  write: / 'Addition:', lv_result.
  lv_result = p_first - p_second.
  write: / 'Substraction:', lv_result.
  lv_result = p_first * p_second.
  write: / 'Multiplication:', lv_result.
  lv_result = p_first div p_second.
  write: / 'Integer quotient:', lv_result. " Truncated towards zero.
  lv_result = p_first mod p_second.
  write: / 'Remainder:',  lv_result.
```



## ACL2


```Lisp

:set-state-ok t

(defun get-two-nums (state)
   (mv-let (_ a state)
           (read-object *standard-oi* state)
      (declare (ignore _))
      (mv-let (_ b state)
              (read-object *standard-oi* state)
         (declare (ignore _))
         (mv a b state))))

(defun integer-arithmetic (state)
   (mv-let (a b state)
           (get-two-nums state)
      (mv state
          (progn$ (cw "Sum:        ~x0~%" (+ a b))
                  (cw "Difference: ~x0~%" (- a b))
                  (cw "Product:    ~x0~%" (* a b))
                  (cw "Quotient:   ~x0~%" (floor a b))
                  (cw "Remainder:  ~x0~%" (mod a b))))))
```



## Ada


```ada
with Ada.Text_Io;
with Ada.Integer_Text_IO;

procedure Integer_Arithmetic is
   use Ada.Text_IO;
   use Ada.Integer_Text_Io;

   A, B : Integer;
begin
   Get(A);
   Get(B);
   Put_Line("a+b = " & Integer'Image(A + B));
   Put_Line("a-b = " & Integer'Image(A - B));
   Put_Line("a*b = " & Integer'Image(A * B));
   Put_Line("a/b = " & Integer'Image(A / B));
   Put_Line("a mod b = " & Integer'Image(A mod B)); -- Sign matches B
   Put_Line("remainder of a/b = " & Integer'Image(A rem B)); -- Sign matches A
   Put_Line("a**b = " & Integer'Image(A ** B));

end Integer_Arithmetic;
```



## Aikido


```aikido
var a = 0
var b = 0
stdin -> a    // read int from stdin
stdin -> b    // read int from stdin

println ("a+b=" + (a + b))
println ("a-b=" + (a - b))
println ("a*b=" + (a * b))
println ("a/b=" + (a / b))
println ("a%b=" + (a % b))
```



## ALGOL 68

{{trans|C}}
{{works with|ALGOL 68|Revision 1 - no extensions to language used}}
{{works with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-1.18.0/algol68g-1.18.0-9h.tiny.el5.centos.fc11.i386.rpm/download 1.18.0-9h.tiny]}}
{{wont work with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8-8d] - due to extensive use of FORMATted transput}}

```algol68
main:(
  LONG INT a=355, b=113;
  printf(($"a+b = "gl$, a + b));
  printf(($"a-b = "gl$, a - b));
  printf(($"a*b = a×b = "gl$, a * b));
  printf(($"a/b = "gl$, a / b));
  printf(($"a OVER b = a%b = a÷b = "gl$, a % b));
  printf(($"a MOD b = a%*b = a%×b = a÷×b = a÷*b = "gl$, a %* b));
  printf(($"a UP b = a**b = a↑b = "gl$, a ** b))
)
```

{{out}}

```txt

a+b =                                 +468
a-b =                                 +242
a*b = a×b =                               +40115
a/b = +3.141592920353982300884955752e  +0
a OVER b = a%b = a÷b =                                   +3
a MOD b = a%*b = a%×b = a÷×b = a÷*b =                                  +16
a UP b = a**b = a↑b = +1.499007808785573768814747570e+288

```

[[ALGOL 68R]] has the curious (and consequently non-standard) '/:=' operator.  This operator
is equivalent to the OVERAB operator of the revised report, except it delivers the remainder as a result.
So a '/:=' b sets a to the quotient of a%b and returns the remainder of a%b as a result.
Note that it must be "stropped" i.e. enclosed in single quotes. eg.
 INT quotient:=355, remainder;
 remainder := quotient '/:=' 113;
Giving a quotient of 3, and a remainder of 16.


## ALGOL W

The Algol W integer division operator (called div) truncates towards zero.

The result of the modulo operator (called rem) has the sign of the first operand when the operands have different signs.

```algolw
begin
    integer a, b;
    write( "Enter 2 integers> " );
    read( a, b );
    write( "a  +  b: ", a  +  b ); % addition         %
    write( "a  -  b: ", a  -  b ); % subtraction      %
    write( "a  *  b: ", a  *  b ); % multiplication   %
    write( "a  /  b: ", a div b ); % integer division %
    write( "a mod b: ", a rem b ); % modulo           %
    % the ** operator returns a real result even with integer operands  %
    % ( the right-hand operand must always be an integer, the left-hand %
    % operand can be integer, real or complex )                         %
    write( "a  ^  b: ", round( a ** b ) )
end.
```



## AmigaE


```amigae
PROC main()
  DEF a, b, t
  WriteF('A = ')
  ReadStr(stdin, t)
  a := Val(t)
  WriteF('B = ')
  ReadStr(stdin, t)
  b := Val(t)
  WriteF('A+B=\d\nA-B=\d\n', a+b, a-b)
  WriteF('A*B=\d\nA/B=\d\n', a*b, a/b)
  /* * and / are 16 bit ops; Mul and Div are 32bit ops */
  WriteF('A*B=\d\nA/B=\d\n', Mul(a,b), Div(a,b))
  WriteF('A mod B =\d\n', Mod(a,b))
ENDPROC
```


## APL


```apl
∇res ← integer_arithmetic; l; r
  l ← ⎕
  r ← ⎕
  res ← 6 2 ⍴ 'sum' (l+r) 'diff' (l-r) 'prod' (l×r) 'quot' (⌊l÷r) 'rem' (r|l) 'pow' (l*r)
```


Quotient will round down in this version.


## ARM Assembly

{{works with|as|Raspberry Pi}}

```ARM Assembly


/* ARM assembly Raspberry PI  */
/*  program arith.s   */
/* Constantes    */
.equ STDOUT, 1
.equ WRITE,  4
.equ EXIT,   1

/***********************/
/* Initialized data */
/***********************/
.data
szMessError:      .asciz " Two numbers in command line please ! \n"       @ message
szRetourLigne: .asciz "\n"
szMessResult:  .asciz "Resultat "      @ message result
sMessValeur:   .fill 12, 1, ' '
                   .asciz "\n"
szMessAddition: .asciz "addition :"
szMessSoustraction: .asciz "soustraction :"
szMessMultiplication: .asciz "multiplication :"
szMessDivision: .asciz "division :"
szMessReste: .asciz "reste :"

/***********************/
/* No Initialized data */
/***********************/
.bss
iValeur:  .skip  4     @ reserve 4 bytes in memory

.text
.global main
main:
    push {fp,lr}    /* save des  2 registres */
    add fp,sp,#8    /* fp <- adresse début */
    ldr r0,[fp]                 @ recup number of parameter in command line
    cmp r0,#3
    blt error
    ldr r0,[fp,#8]   @ adresse of 1er number
    bl conversionAtoD
    mov r3,r0
    ldr r0,[fp,#12]   @ adresse of 2eme number
    bl conversionAtoD
    mov r4,r0
    @ addition
    add r0,r3,r4
    ldr r1,iAdrsMessValeur                           @ result in r0
    bl conversion10S       @ call function with 2 parameter (r0,r1)
    ldr r0,iAdrszMessResult
    bl affichageMess            @ display message
    ldr r0,iAdrszMessAddition
    bl affichageMess            @ display message
    ldr r0,iAdrsMessValeur
    bl affichageMess            @ display message
    @ soustraction
    sub r0,r3,r4
    ldr r1,=sMessValeur
    bl conversion10S       @ call function with 2 parameter (r0,r1)
    ldr r0,iAdrszMessResult
    bl affichageMess            @ display message
    ldr r0,iAdrszMessSoustraction
    bl affichageMess            @ display message
    ldr r0,iAdrsMessValeur
    bl affichageMess            @ display message

    @ multiplication
    mul r0,r3,r4
    ldr r1,=sMessValeur
    bl conversion10S       @ call function with 2 parameter (r0,r1)
    ldr r0,iAdrszMessResult
    bl affichageMess            @ display message
    ldr r0,iAdrszMessMultiplication
    bl affichageMess            @ display message
    ldr r0,iAdrsMessValeur
    bl affichageMess            @ display message

    @ division
    mov r0,r3
    mov r1,r4
    bl division
    mov r0,r2           @ quotient
    ldr r1,=sMessValeur
    bl conversion10S       @ call function with 2 parameter (r0,r1)
    ldr r0,iAdrszMessResult
    bl affichageMess            @ display message
    ldr r0,iAdrszMessDivision
    bl affichageMess            @ display message
    ldr r0,iAdrsMessValeur
    bl affichageMess            @ display message

    mov r0,r3           @ remainder
    ldr r1,=sMessValeur
    bl conversion10S       @ call function with 2 parameter (r0,r1)
    ldr r0,iAdrszMessResult
    bl affichageMess            @ display message
    ldr r0,iAdrszMessReste
    bl affichageMess            @ display message
    ldr r0,iAdrsMessValeur
    bl affichageMess            @ display message

    mov r0, #0                  @ return code
    b 100f
error:
    ldr r0,iAdrszMessError
    bl affichageMess            @ call function with 1 parameter (r0)
    mov r0, #1                  @ return code
100: /* end of  program */
    mov r7, #EXIT              @ request to exit program
    swi 0                       @ perform the system call
iAdrsMessValeur: .int sMessValeur
iAdrszMessResult: .int szMessResult
iAdrszMessError: .int szMessError
iAdrszMessAddition: .int szMessAddition
iAdrszMessSoustraction: .int szMessSoustraction
iAdrszMessMultiplication: .int szMessMultiplication
iAdrszMessDivision: .int szMessDivision
iAdrszMessReste: .int szMessReste
/******************************************************************/
/*     affichage des messages   avec calcul longueur              */
/******************************************************************/
/* r0 contient l adresse du message */
affichageMess:
    push {fp,lr}    /* save des  2 registres */
    push {r0,r1,r2,r7}    /* save des autres registres */
    mov r2,#0   /* compteur longueur */
1:       /*calcul de la longueur */
    ldrb r1,[r0,r2]  /* recup octet position debut + indice */
    cmp r1,#0       /* si 0 c est fini */
    beq 1f
    add r2,r2,#1   /* sinon on ajoute 1 */
    b 1b
1:  /* donc ici r2 contient la longueur du message */
    mov r1,r0        /* adresse du message en r1 */
    mov r0,#STDOUT      /* code pour écrire sur la sortie standard Linux */
    mov r7, #WRITE                  /* code de l appel systeme 'write' */
    swi #0                      /* appel systeme */
    pop {r0,r1,r2,r7}     /* restaur des autres registres */
    pop {fp,lr}    /* restaur des  2 registres */
    bx lr	        /* retour procedure */
/***************************************************/
/*   conversion registre en décimal   signé  */
/***************************************************/
/* r0 contient le registre   */
/* r1 contient l adresse de la zone de conversion */
conversion10S:
    push {fp,lr}    /* save des  2 registres frame et retour */
    push {r0-r5}   /* save autres registres  */
    mov r2,r1       /* debut zone stockage */
    mov r5,#'+'     /* par defaut le signe est + */
    cmp r0,#0       /* nombre négatif ? */
    movlt r5,#'-'     /* oui le signe est - */
    mvnlt r0,r0       /* et inversion en valeur positive */
    addlt r0,#1
    mov r4,#10   /* longueur de la zone */
1: /* debut de boucle de conversion */
    bl divisionpar10 /* division  */
    add r1,#48        /* ajout de 48 au reste pour conversion ascii */
    strb r1,[r2,r4]  /* stockage du byte en début de zone r5 + la position r4 */
    sub r4,r4,#1      /* position précedente */
    cmp r0,#0
    bne 1b	       /* boucle si quotient different de zéro */
    strb r5,[r2,r4]  /* stockage du signe à la position courante */
    subs r4,r4,#1   /* position précedente */
    blt  100f         /* si r4 < 0  fin  */
    /* sinon il faut completer le debut de la zone avec des blancs */
    mov r3,#' '   /* caractere espace */
2:
    strb r3,[r2,r4]  /* stockage du byte  */
    subs r4,r4,#1   /* position précedente */
    bge 2b        /* boucle si r4 plus grand ou egal a zero */
100:  /* fin standard de la fonction  */
    pop {r0-r5}   /*restaur des autres registres */
    pop {fp,lr}   /* restaur des  2 registres frame et retour  */
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
/******************************************************************/
/*     Conversion d une chaine en nombre stocké dans un registre  */
/******************************************************************/
/* r0 contient l adresse de la zone terminée par 0 ou 0A */
conversionAtoD:
    push {fp,lr}    /* save des  2 registres */
    push {r1-r7}    /* save des autres registres */
    mov r1,#0
    mov r2,#10   /* facteur */
    mov r3,#0  /* compteur */
    mov r4,r0  /* save de l adresse dans r4 */
    mov r6,#0   /* signe positif par defaut */
    mov r0,#0  /* initialisation à 0 */
1:     /* boucle d élimination des blancs du debut */
    ldrb r5,[r4,r3]  /* chargement dans r5 de l octet situé au debut + la position */
    cmp r5,#0       /* fin de chaine -> fin routine */
    beq 100f
    cmp r5,#0x0A       /* fin de chaine -> fin routine */
    beq 100f
    cmp r5,#' '        /* blanc au début */
    bne 1f           /* non on continue */
    add r3,r3,#1      /* oui on boucle en avançant d un octet */
    b 1b
1:
    cmp r5,#'-'   	/* premier caracteres est -    */
    moveq r6,#1     /* maj du registre r6 avec 1 */
    beq 3f          /* puis on avance à la position suivante */
2:   /* debut de boucle de traitement des chiffres */
    cmp r5,#'0' /* caractere n est pas un chiffre */
    blt 3f
    cmp r5,#'9' /* caractere n est pas un chiffre */
    bgt 3f
    /* caractère est un chiffre */
    sub r5,#48
    ldr r1,iMaxi  /*verifier le dépassement du registre  */
    cmp r0,r1
    bgt 99f
    mul r0,r2,r0	/* multiplier par facteur */
    add r0,r5    /* ajout à r0 */
3:
    add r3,r3,#1   /* avance à la position suivante */
    ldrb r5,[r4,r3]  /* chargement de l octet */
    cmp r5,#0        /* fin de chaine -> fin routine */
    beq 4f
    cmp r5,#10    /* fin de chaine -> fin routine */
    beq 4f
    b 2b   /* boucler */
4:
    cmp r6,#1  /* test du registre r6 pour le signe */
    bne 100f
    mov r1,#-1
    mul r0,r1,r0  /* si negatif, on multiplie par -1 */
    b 100f
99:  /* erreur de dépassement */
    ldr r1,=szMessErrDep
    bl   afficheerreur
    mov r0,#0   /* en cas d'erreur on retourne toujours zero */
100:
    pop {r1-r7}     /* restaur des autres registres */
    pop {fp,lr}    /* restaur des  2 registres */
    bx lr           /* retour procedure */
/* constante programme */
iMaxi: .int 1073741824
szMessErrDep:  .asciz  "Nombre trop grand : dépassement de capacite de 32 bits. :\n"
.align 4
/*
### =======================================
*/
/* division entiere non signée                */
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



```



## Arturo



```arturo
print "give me the first number  : " true, 	a $(toNumber|strip|input)
print "give me the second number : " true, 	b $(toNumber|strip|input)

print "`a` + `b` = " + (a+b)
print "`a` - `b` = " + (a-b)
print "`a` * `b` = " + (a*b)
print "`a` / `b` = " + (a/b)
print "`a` % `b` = " + (a%b)
print "`a` ^ `b` = " + (a^b)
```


{{out}}


```txt
give me the first number  : 33
give me the second number : 6
33 + 6 = 39
33 - 6 = 27
33 * 6 = 198
33 / 6 = 5
33 % 6 = 3
33 ^ 6 = 1291467969
```



## AutoHotkey

The quotient rounds towards 0 if both inputs are integers or towards negative infinity if either input is floating point. The sign of the remainder is always the same as the sign of the first parameter (dividend).

```autohotkey
Gui, Add, Edit, va, 5
Gui, Add, Edit, vb, -3
Gui, Add, Button, Default, Compute
Gui, Show
Return

ButtonCompute:
  Gui, Submit
  MsgBox,%
  (Join`s"`n"
   a "+" b " = " a+b
   a "-" b " = " a-b
   a "*" b " = " a*b
   a "//" b " = " a//b " remainder " Mod(a,b)
   a "**" b " = " a**b
  )
; fallthrough
GuiClose:
  ExitApp
```



## AWK


```awk
/^[ \t]*-?[0-9]+[ \t]+-?[0-9]+[ \t]*$/ {
	print "add:", $1 + $2
	print "sub:", $1 - $2
	print "mul:", $1 * $2
	print "div:", int($1 / $2) # truncates toward zero
	print "mod:", $1 % $2      # same sign as first operand
	print "exp:", $1 ^ $2
	exit }
```


For division and modulus, Awk should act like C.

'''Exponentiation's note:''' With [[nawk]] or [[gawk]], <code>$1 ** $2</code> acts like <code>$1 ^ $2</code>. With [[mawk]], <code>$1 ** $2</code> is a syntax error. Nawk allows <code>**</code>, but its manual page only has <code>^</code>. Gawk's manual warns, ''"The POSIX standard only specifies the use of `^' for exponentiation. For maximum portability, do not use the `**' operator."''


## BASIC

=
## Applesoft BASIC
=
Same code as [[#Commodore_BASIC|Commodore BASIC]]
=
## BaCon
=

```freebasic
' Arthimetic/Integer
DECLARE a%, b%
INPUT "Enter integer A: ", a%
INPUT "Enter integer B: ", b%
PRINT

PRINT a%, " + ", b%, "     is ", a% + b%
PRINT a%, " - ", b%, "     is ", a% - b%
PRINT a%, " * ", b%, "     is ", a% * b%
PRINT a%, " / ", b%, "     is ", a% / b%, ", trucation toward zero"
PRINT "MOD(", a%, ", ", b%, ") is ", MOD(a%, b%), ", same sign as first operand"
PRINT "POW(", a%, ", ", b%, ") is ", INT(POW(a%, b%))
```


=
## Commodore BASIC
=

```basic
10 INPUT "ENTER A NUMBER"; A%
20 INPUT "ENTER ANOTHER NUMBER"; B%
30 PRINT "ADDITION:";A%;"+";B%;"=";A%+B%
40 PRINT "SUBTRACTION:";A%;"-";B%;"=";A%-B%
50 PRINT "MULTIPLICATION:";A%;"*";B%;"=";A%*B%
60 PRINT "INTEGER DIVISION:";A%;"/";B%;"=";INT(A%/B%)
70 PRINT "REMAINDER OR MODULO:";A%;"%";B%;"=";A%-INT(A%/B%)*B%
80 PRINT "POWER:";A%;"^";B%;"=";A%^B%
```


=
## True BASIC
=

```basic

! RosettaCode: Integer Arithmetic
! True BASIC v6.007
! Translated from BaCon example.
PROGRAM Integer_Arithmetic
	INPUT PROMPT "Enter integer A: ": a
	INPUT PROMPT "Enter integer B: ": b
	PRINT
	PRINT a;" + ";b;" is ";a+b
	PRINT a;" - ";b;" is ";a-b
	PRINT a;" * ";b;" is ";a*b
	PRINT a;" / ";b;" is ";INT(a/b);
	PRINT "MOD(";a;", ";b;") is "; MOD(a,b)
	PRINT "POW(";a;", ";b;") is ";INT(a^b)
	GET KEY done
END

```

=
## QBasic
=
{{works with|QuickBasic|4.5}}

```qbasic
function math(a!, b!)
	print a + b
	print a - b
	print a * b
	print a / b
	print a mod b
end function
```

Truncate towards: 0

Remainder sign matches: first operand


## BASIC256


```BASIC256

input "enter a number ?", a
input "enter another number ?", b

print "addition " + a + " + " + b + " = " + (a + b)
print "subtraction " + a + " - " + b + " = " + (a - b)
print "multiplication " + a + " * " + b + " = " + (a * b)
print "integer division " + a + " \ " + b + " = " + (a \ b)
print "remainder or modulo " + a + " % " + b + " = " + (a % b)
print "power " + a + " ^ " + b + " = " + (a ^ b)

```



## Batch File

{{works with|Windows NT|4 or later (includes Windows XP and onward)}}

```dos

@echo off
set /P A=Enter 1st Number :
set /P B=Enter 2nd Number :
set D=%A% + %B% & call :printC
set D=%A% - %B% & call :printC
set D=%A% * %B% & call :printC
set D=%A% / %B% & call :printC & rem truncates toward 0
set D=%A% %% %B% & call :printC & rem matches sign of 1st operand
exit /b

:printC
set /A C=%D%
echo %D% = %C%

```



## BBC BASIC


```bbcbasic
      INPUT "Enter the first integer: " first%
      INPUT "Enter the second integer: " second%

      PRINT "The sum is " ; first% + second%
      PRINT "The difference is " ; first% - second%
      PRINT "The product is " ; first% * second%
      PRINT "The integer quotient is " ; first% DIV second% " (rounds towards 0)"
      PRINT "The remainder is " ; first% MOD second% " (sign matches first operand)"
      PRINT "The first raised to the power of the second is " ; first% ^ second%
```



## bc


```bc
define f(a, b) {
	"add: "; a + b
	"sub: "; a - b
	"mul: "; a * b
	"div: "; a / b  /* truncates toward zero */
	"mod: "; a % b  /* same sign as first operand */
	"pow: "; a ^ b
}
```



## Befunge


```befunge
&&00p"=A",,:."=B ",,,00g.55+,v
        v,+55.+g00:,,,,"A+B="<
        >"=B-A",,,,:00g-.55+,v
        v,+55.*g00:,,,,"A*B="<
        >"=B/A",,,,:00g/.55+,v
         @,+55.%g00,,,,"A%B="<
```



## Bracmat

The remainder returned by mod is non-negative. Furthermore, <code>div$(!a.!d)*!d+mod$(!a.!d):!a</code> for all integer <code>!a</code> and <code>!d</code>, <code>!d:~0</code>.

```Bracmat
  ( enter
  =     put$"Enter two integer numbers, separated by space:"
      & get':(~/#?k_~/#?m|quit:?k)
    |     out
        $ "You must enter two integer numbers! Enter \"quit\" if you don't know how to do that."
      & !enter
  )
& !enter
& !k:~quit
& out$("You entered" !k and !m ". Now look:")
& out$("Sum:" !k+!m)
& out$("Difference:" !k+-1*!m)
& out$("Product:" !k*!m)
& out$("Integer division:" div$(!k.!m))
& out$("Remainder:" mod$(!k.!m))
& out$("Exponentiation:" !k^!m)
& done;

```



## Brat

Inspired by the second VBScript version.

```brat
x = ask("First number: ").to_i
y = ask("Second number: ").to_i

#Division uses floating point
#Remainder uses sign of right hand side
[:+ :- :* :/ :% :^].each { op |
  p "#{x} #{op} #{y} = #{x.call_method op, y}"
```



## C


```c
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char *argv[])
{
  int a, b;
  if (argc < 3) exit(1);
  b = atoi(argv[--argc]);
  if (b == 0) exit(2);
  a = atoi(argv[--argc]);
  printf("a+b = %d\n", a+b);
  printf("a-b = %d\n", a-b);
  printf("a*b = %d\n", a*b);
  printf("a/b = %d\n", a/b); /* truncates towards 0 (in C99) */
  printf("a%%b = %d\n", a%b); /* same sign as first operand (in C99) */
  return 0;
}
```



## C++


```cpp
#include <iostream>

int main()
{
  int a, b;
  std::cin >> a >> b;
  std::cout << "a+b = " << a+b << "\n";
  std::cout << "a-b = " << a-b << "\n";
  std::cout << "a*b = " << a*b << "\n";
  std::cout << "a/b = " << a/b << ", remainder " << a%b << "\n";
  return 0;
}
```


## C#

```c#
using System;

class Program
{
    static void Main(string[] args)
    {
        int a = Convert.ToInt32(args[0]);
        int b = Convert.ToInt32(args[1]);

        Console.WriteLine("{0} + {1} = {2}", a, b, a + b);
        Console.WriteLine("{0} - {1} = {2}", a, b, a - b);
        Console.WriteLine("{0} * {1} = {2}", a, b, a * b);
        Console.WriteLine("{0} / {1} = {2}", a, b, a / b); // truncates towards 0
        Console.WriteLine("{0} % {1} = {2}", a, b, a % b); // matches sign of first operand
        Console.WriteLine("{0} to the power of {1} = {2}", a, b, Math.Pow(a, b));
    }
}
```

{{out}}

```txt
5 + 3 = 8
5 - 3 = 2
5 * 3 = 15
5 / 3 = 1
5 % 3 = 2
5 to the power of 3 = 125
```



## Chef



```Chef
Number Soup.

Only reads single values.

Ingredients.
1 g Numbers
3 g Water
5 g Soup

Method.
Take Numbers from refrigerator.
Take Soup from refrigerator.
Put Numbers into 1st mixing bowl.
Add Soup into the 1st mixing bowl.
Pour contents of the 1st mixing bowl into 1st baking dish.
Clean 1st mixing bowl.
Put Numbers into 1st mixing bowl.
Remove Soup from 1st mixing bowl.
Pour contents of the 1st mixing bowl into 2nd baking dish.
Clean 1st mixing bowl.
Put Numbers into 1st mixing bowl.
Combine Soup into 1st mixing bowl.
Pour contents of the 1st mixing bowl into 3rd baking dish.
Clean 1st mixing bowl.
Put Numbers into 1st mixing bowl.
Divide Soup into 1st mixing bowl.
Pour contents of the 1st mixing bowl into 4th baking dish.
Clean 1st mixing bowl.
Put Water into 1st mixing bowl.
Verb the Soup.
Combine Numbers into 1st mixing bowl.
Verb the Soup until verbed.
Pour contents of the 1st mixing bowl into 5th baking dish.
Clean 1st mixing bowl.

Serves 5.
```



## Clipper


```visualfoxpro
procedure Test( a, b )
   ? "a+b", a + b
   ? "a-b", a - b
   ? "a*b", a * b
   // The quotient isn't integer, so we use the Int() function, which truncates it downward.
   ? "a/b", Int( a / b )
   // Remainder:
   ? "a%b", a % b
   // Exponentiation is also a base arithmetic operation
   ? "a**b", a ** b
   return
```



## Clojure


```clojure
(defn myfunc []
  (println "Enter x and y")
  (let [x (read), y (read)]
    (doseq [op '(+ - * / Math/pow rem)]
      (let [exp (list op x y)]
	(printf "%s=%s\n" exp (eval exp))))))
```



```txt
user=> (myfunc)
Enter x and y
3
6
(+ 3 6)=9
(- 3 6)=-3
(* 3 6)=18
(/ 3 6)=1/2
(Math/pow 3 6)=729.0
(rem 3 6)=3
nil
```



## COBOL


```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Int-Arithmetic.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 A      PIC S9(10).
       01 B      PIC S9(10).
       01 Result PIC S9(10).

       PROCEDURE DIVISION.
           DISPLAY "First number: " WITH NO ADVANCING
           ACCEPT A
           DISPLAY "Second number: " WITH NO ADVANCING
           ACCEPT B

*          *> Note: The various ADD/SUBTRACT/etc. statements can be
*          *> replaced with COMPUTE statements, which allow those
*          *> operations to be defined similarly to other languages,
*          *> e.g. COMPUTE Result = A + B

           ADD A TO B GIVING Result
           DISPLAY "A + B = " Result

           SUBTRACT B FROM A GIVING Result
           DISPLAY "A - B = " Result

           MULTIPLY A BY B GIVING Result
           DISPLAY "A * B = " Result

*          *> Division here truncates towards zero. DIVIDE can take a
*          *> ROUNDED clause, which will round the result to the nearest
*          *> integer.
           DIVIDE A BY B GIVING Result
           DISPLAY "A / B = " Result

           COMPUTE Result = A ^ B
           DISPLAY "A ^ B = " Result

*          *> Matches sign of first argument.
           DISPLAY "A % B = " FUNCTION REM(A, B)

           GOBACK
           .
```



## Common Lisp



```lisp
(defun arithmetic (&optional (a (read *query-io*)) (b (read *query-io*)))
  (mapc
    (lambda (op)
      (format t "~a => ~a~%" (list op a b) (funcall (symbol-function op) a b)))
    '(+ - * mod rem floor ceiling truncate round expt))
  (values))
```


Common Lisp's integer division functions are <code>floor</code>, <code>ceiling</code>, <code>truncate</code>, and <code>round</code>. They differ in how they round their quotient.

{| class="wikitable"
! The function !! rounds its quotient towards
|-
! <code>floor</code>
| negative infinity
|-
! <code>ceiling</code>
| positive infinity
|-
! <code>truncate</code>
| zero
|-
! <code>round</code>
| the nearest integer (preferring the even integer if the mathematical quotient is equidistant from two integers)
|}

Each function also returns a remainder as its secondary value, such that
  quotient * divisor + remainder = dividend .
<code>(mod a b)</code> and <code>(rem a b)</code> return numbers equal to the secondary values of <code>(floor a b)</code> and <code>(truncate a b)</code>, respectively.


## Component Pascal

Works with Gardens Point Component Pascal

```oberon2

MODULE Arithmetic;
IMPORT CPmain,Console,RTS;

VAR
   x,y	  : INTEGER;
   arg	  : ARRAY 128 OF CHAR;
   status : BOOLEAN;


PROCEDURE Error(IN str : ARRAY OF CHAR);
BEGIN
   Console.WriteString(str);Console.WriteLn;
   HALT(1)
END Error;


BEGIN
   IF CPmain.ArgNumber() < 2 THEN Error("Give me two integers!") END;
   CPmain.GetArg(0,arg); RTS.StrToInt(arg,x,status);
   IF ~status THEN Error("Can't convert 	'"+arg+"' to Integer") END;
   CPmain.GetArg(1,arg); RTS.StrToInt(arg,y,status);
   IF ~status THEN Error("Can't convert '"+arg+"' to Integer") END;
   Console.WriteString("x + y >");Console.WriteInt(x + y,6);Console.WriteLn;
   Console.WriteString("x - y >");Console.WriteInt(x - y,6);Console.WriteLn;
   Console.WriteString("x * y >");Console.WriteInt(x * y,6);Console.WriteLn;
   Console.WriteString("x / y >");Console.WriteInt(x DIV y,6);Console.WriteLn;
   Console.WriteString("x MOD y >");Console.WriteInt(x MOD y,6);Console.WriteLn;
END Arithmetic.

```

command: <i>cprun Arithmetic 12 23</i><br/>
{{out}}

```txt

x + y >    35
x - y >   -11
x * y >   276
x / y >     0
x MOD y >    12

```

Works with BlackBox Component Builder

```oberon2

MODULE Arithmetic;
IMPORT StdLog,DevCommanders,TextMappers;

PROCEDURE DoArithmetic(x,y: INTEGER);
BEGIN
        StdLog.String("x + y >");StdLog.Int(x + y);StdLog.Ln;
        StdLog.String("x - y >");StdLog.Int(x - y);StdLog.Ln;
        StdLog.String("x * y >");StdLog.Int(x * y);StdLog.Ln;
        StdLog.String("x / y >");StdLog.Int(x DIV y);StdLog.Ln;
        StdLog.String("x MOD y >");StdLog.Int(x MOD y);StdLog.Ln;
END DoArithmetic;

PROCEDURE Go*;
VAR
                params: DevCommanders.Par;
                s: TextMappers.Scanner;
                p : ARRAY 2 OF INTEGER;
                current: INTEGER;
BEGIN
        current := 0;
        params := DevCommanders.par;
        s.ConnectTo(params.text);
        s.SetPos(params.beg);
        s.Scan;
        WHILE(~s.rider.eot) DO
                IF (s.type = TextMappers.int) THEN
                        p[current] := s.int; INC(current);
                END;
                s.Scan;
        END;
        IF current = 2 THEN DoArithmetic(p[0],p[1]) END;
END Go;
END Arithmetic.

```

Command: Arithmetic.Go 12 23 ~ <br/>
{{out}}

```txt

x + y > 35
x - y > -11
x * y > 276
x / y > 0
x MOD y > 12

```


## D


```d
import std.stdio, std.string, std.conv;

void main() {
    int a = 10, b = 20;
    try {
        a = readln().strip().to!int();
        b = readln().strip().to!int();
    } catch (StdioException e) {}
    writeln("a = ", a, ", b = ", b);

    writeln("a + b = ", a + b);
    writeln("a - b = ", a - b);
    writeln("a * b = ", a * b);
    writeln("a / b = ", a / b);
    writeln("a % b = ", a % b);
    writeln("a ^^ b = ", a ^^ b);
}
```

{{out}}

```txt
a = -16, b = 5
a + b = -11
a - b = -21
a * b = -80
a / b = -3
a % b = -1
a ^^ b = -1048576
```



### Shorter Version

Same output.

```d
import std.stdio, std.string, std.conv, std.meta;

void main() {
    int a = -16, b = 5;
    try {
        a = readln().strip().to!int();
        b = readln().strip().to!int();
    } catch (StdioException e) {}
    writeln("a = ", a, ", b = ", b);

    foreach (op; AliasSeq!("+", "-", "*", "/", "%", "^^"))
        mixin(`writeln("a ` ~ op ~ ` b = ", a` ~ op ~ `b);`);
}
```

Division and modulus are defined as in C99.


## dc


```dc
[Enter 2 integers on 1 line.
  Use whitespace to separate. Example: 2 3
  Use underscore for negative integers. Example: _10
]P ? sb sa
[add: ]P la lb + p sz
[sub: ]P la lb - p sz
[mul: ]P la lb * p sz
[div: ]P la lb / p sz  [truncates toward zero]sz
[mod: ]P la lb % p sz  [sign matches first operand]sz
[pow: ]P la lb ^ p sz
```


## DCL


```DCL
$ inquire a "Enter first number"
$ a = f$integer( a )
$ inquire b "Enter second number"
$ b = f$integer( b )
$ write sys$output "a + b = ", a + b
$ write sys$output "a - b = ", a - b
$ write sys$output "a * b = ", a * b
$ write sys$output "a / b = ", a / b  ! truncates down
```

{{out}}

```txt
$ @arithmetic_integer
Enter first number: 2
Enter second number: 5
a + b = 7
a - b = -3
a * b = 10
a / b = 0
$ @arithmetic_integer
Enter first number: -5
Enter second number: -2
a + b = -7
a - b = -3
a * b = 10
a / b = 2
```



## Delphi


```Delphi
program IntegerArithmetic;

{$APPTYPE CONSOLE}

uses SysUtils, Math;

var
  a, b: Integer;
begin
  a := StrToInt(ParamStr(1));
  b := StrToInt(ParamStr(2));

  WriteLn(Format('%d + %d = %d', [a, b, a + b]));
  WriteLn(Format('%d - %d = %d', [a, b, a - b]));
  WriteLn(Format('%d * %d = %d', [a, b, a * b]));
  WriteLn(Format('%d / %d = %d', [a, b, a div b])); // rounds towards 0
  WriteLn(Format('%d %% %d = %d', [a, b, a mod b])); // matches sign of the first operand
  WriteLn(Format('%d ^ %d = %d', [a, b, Trunc(Power(a, b))]));
end.
```



## DWScript


```delphi
var a := StrToInt(ParamStr(0));
var b := StrToInt(ParamStr(1));

PrintLn(Format('%d + %d = %d', [a, b, a + b]));
PrintLn(Format('%d - %d = %d', [a, b, a - b]));
PrintLn(Format('%d * %d = %d', [a, b, a * b]));
PrintLn(Format('%d / %d = %d', [a, b, a div b]));
PrintLn(Format('%d mod %d = %d', [a, b, a mod b]));
PrintLn(Format('%d ^ %d = %d', [a, b, Trunc(Power(a, b))]));
```



## Dyalect


{{trans|Swift}}

Dyalect has no operator for exponential.


```Dyalect
const a = 6
const b = 4

print("sum =\(a+b)")
print("difference = \(a-b)")
print("product = \(a*b)")
print("Integer quotient = \(a/b)")
print("Remainder = (a%b)")
```



## E



```e
def arithmetic(a :int, b :int) {
  return `$\
   Sum:        ${a + b}
   Difference: ${a - b}
   Product:    ${a * b}
   Quotient:   ${a // b}
   Remainder:  ${a % b}$\n`
}
```



## EasyLang


<lang>a = number input
b = number input
print a + b
print a - b
print a * b
print a / b
print a mod b
```



## ECL


```ECL

ArithmeticDemo(INTEGER A,INTEGER B) := FUNCTION
  ADDit       := A + B;
  SUBTRACTit  := A - B;
  MULTIPLYit  := A * B;
  INTDIVIDEit := A DIV B; //INTEGER DIVISION
  DIVIDEit    := A / B;   //standard division
  Remainder   := A % B;
  EXPit       := POWER(A,B);
  DS          := DATASET([{A,B,'A PLUS B is:',ADDit},
                          {A,B,'A MINUS B is:',SUBTRACTit},
			  {A,B,'A TIMES B is:',MULTIPLYit},
			  {A,B,'A INT DIVIDE BY B is:',INTDIVIDEit},
			  {A,B,'REMAINDER is:',Remainder},
			  {A,B,'A DIVIDE BY B is:',DIVIDEit},
			  {A,B,'A RAISED TO B:',EXPit}],
			  {INTEGER AVal,INTEGER BVal,STRING18 valuetype,STRING val});

  RETURN DS;
  END;

ArithmeticDemo(1,1);
ArithmeticDemo(2,2);
ArithmeticDemo(50,5);
ArithmeticDemo(10,3);
ArithmeticDemo(-1,2);

/* 	NOTE:Division by zero defaults to generating a zero result (0),
   	rather than reporting a “divide by zero” error.
   	This avoids invalid or unexpected data aborting a long job.
   	This default behavior can be changed
*/

```



## Efene



```efene
@public
run = fn () {

    First = io.get_line("First number: ")
    Second = io.get_line("Second number: ")

    A = list_to_integer(lists.delete($\n, First))
    B = list_to_integer(lists.delete($\n, Second))

    io.format("Sum: ~p~n", [A + B])
    io.format("Difference: ~p~n", [A - B])
    io.format("Product: ~p~n", [A * B])
    io.format("Quotient: ~p~n", [A / B])
    io.format("Remainder: ~p~n", [A % B])
}
```



## Eiffel

{{works with|SmartEiffel|2.4}}
In a file called main.e:

```eiffel
class MAIN
    creation make
    feature make is
        local
            a, b: REAL;
        do
            print("a = ");
            io.read_real;
            a := io.last_real;

            print("b = ");
            io.read_real;
            b := io.last_real;

            print("a + b = ");
            io.put_real(a + b);
            print("%Na - b = ");
            io.put_real(a - b);
            print("%Na * b = ");
            io.put_real(a * b);
            print("%Na / b = ");
            io.put_real(a / b);
            print("%Na %% b = ");
            io.put_real(((a / b) - (a / b).floor) * b);
            print("%Na ^ b = ");
            io.put_real(a.pow(b));
            print("%N");
        end
end
```

Note that there actually is a builtin modulo operator (\\). However, it seems impossible to use that instruction with SmartEiffel.


## Elena

ELENA 4.x :

```elena
import system'math;
import extensions;

public program()
{
    var a := console.loadLineTo(new Integer());
    var b := console.loadLineTo(new Integer());

    console.printLine(a," + ",b," = ",a + b);
    console.printLine(a," - ",b," = ",a - b);
    console.printLine(a," * ",b," = ",a * b);
    console.printLine(a," / ",b," = ",a / b);   // truncates towards 0
    console.printLine(a," % ",b," = ",a.mod:b); // matches sign of first operand
}
```



## Elixir

{{works with|Elixir|1.4}}

```Elixir
defmodule Arithmetic_Integer do
  # Function to remove line breaks and convert string to int
  defp get_int(msg) do
    IO.gets(msg) |> String.strip |> String.to_integer
  end

  def task do
    # Get user input
    a = get_int("Enter your first integer: ")
    b = get_int("Enter your second integer: ")

    IO.puts "Elixir Integer Arithmetic:\n"
    IO.puts "Sum:            #{a + b}"
    IO.puts "Difference:     #{a - b}"
    IO.puts "Product:        #{a * b}"
    IO.puts "True Division:  #{a / b}"                  # Float
    IO.puts "Division:       #{div(a,b)}"               # Truncated Towards 0
    IO.puts "Floor Division: #{Integer.floor_div(a,b)}" # floored integer division
    IO.puts "Remainder:      #{rem(a,b)}"               # Sign from first digit
    IO.puts "Modulo:         #{Integer.mod(a,b)}"       # modulo remainder (uses floored division)
    IO.puts "Exponent:       #{:math.pow(a,b)}"         # Float, using Erlang's :math
  end
end

Arithmetic_Integer.task
```


{{out}}
<pre style="height: 80ex; overflow: scroll">
C:\Elixir>elixir Arithmetic_Integer.exs
Enter your first integer: 7
Enter your second integer: 3
Elixir Integer Arithmetic:

Sum:            10
Difference:     4
Product:        21
True Division:  2.3333333333333335
Division:       2
Floor Division: 2
Remainder:      1
Modulo:         1
Exponent:       343.0

C:\Elixir>elixir Arithmetic_Integer.exs
Enter your first integer: -7
Enter your second integer: 3
Elixir Integer Arithmetic:

Sum:            -4
Difference:     -10
Product:        -21
True Division:  -2.3333333333333335
Division:       -2
Floor Division: -3
Remainder:      -1
Modulo:         2
Exponent:       -343.0

C:\Elixir>elixir Arithmetic_Integer.exs
Enter your first integer: 7
Enter your second integer: -3
Elixir Integer Arithmetic:

Sum:            4
Difference:     10
Product:        -21
True Division:  -2.3333333333333335
Division:       -2
Floor Division: -3
Remainder:      1
Modulo:         -2
Exponent:       0.0029154518950437317

C:\Elixir>elixir Arithmetic_Integer.exs
Enter your first integer: -7
Enter your second integer: -3
Elixir Integer Arithmetic:

Sum:            -10
Difference:     -4
Product:        21
True Division:  2.3333333333333335
Division:       2
Floor Division: 2
Remainder:      -1
Modulo:         -1
Exponent:       -0.0029154518950437317

```



## Erlang


```erlang
% Implemented by Arjun Sunel
-module(arith).
-export([start/0]).

start() ->
   case io:fread("","~d~d") of
       {ok, [A,B]} ->
           io:format("Sum = ~w~n",[A+B]),
           io:format("Difference = ~w~n",[A-B]),
           io:format("Product = ~w~n",[A*B]),
           io:format("Quotient = ~w~n",[A div B]),      % truncates towards zero
           io:format("Remainder= ~w~n",[A rem B]),    % same sign as the first operand
           halt()
   end.

```



## ERRE

<lang>
PROGRAM INTEGER_ARITHMETIC

!
! for rosettacode.org
!

!$INTEGER

BEGIN
  INPUT("Enter a number ",A)
  INPUT("Enter another number ",B)

  PRINT("Addition ";A;"+";B;"=";(A+B))
  PRINT("Subtraction ";A;"-";B;"=";(A-B))
  PRINT("Multiplication ";A;"*";B;"=";(A*B))
  PRINT("Integer division ";A;"div";B;"=";(A DIV B))
  PRINT("Remainder or modulo ";A;"mod";B;"=";(A MOD B))
  PRINT("Power ";A;"^";B;"=";(A^B))
END PROGRAM

```

{{out}}

```txt
Enter a number ? 12
Enter another number ? 5
Addition  12 + 5 = 17
Subtraction  12 - 5 = 7
Multiplication  12 * 5 = 60
Integer division  12 div 5 = 2
Remainder or modulo  12 mod 5 = 2
Power  12 ^ 5 = 248832

```

Truncate towards: 0

Remainder sign matches: first operand

In C-64 ERRE version you must use <code>INT(A/B)</code> for division and <code>A-B*INT(A/B)</code> for modulus.


## Euphoria


```euphoria
include get.e

integer a,b

a = floor(prompt_number("a = ",{}))
b = floor(prompt_number("b = ",{}))

printf(1,"a + b = %d\n", a+b)
printf(1,"a - b = %d\n", a-b)
printf(1,"a * b = %d\n", a*b)
printf(1,"a / b = %g\n", a/b) -- does not truncate
printf(1,"remainder(a,b) = %d\n", remainder(a,b)) -- same sign as first operand
printf(1,"power(a,b) = %g\n", power(a,b))
```


{{out}}

```txt
a = 2
b = 3
a + b = 5
a - b = -1
a * b = 6
a / b = 0.666667
remainder(a,b) = 2
power(a,b) = 8
```



## Excel


If the numbers are typed into cells A1 and B1

For sum, type in C1

```excel

=$A1+$B1

```


For difference, type in D1

```excel

=$A1-$B1

```


For product, type in E1

```excel

=$A1*$B1

```


For quotient, type in F1

```excel

=QUOTIENT($A1,$B1)

```


For remainder, type in G1

```excel

=MOD($A1,$B1)

```


For exponentiation, type in H1

```excel

=$A1^$B1

```



## Factor


```factor
USING: combinators io kernel math math.functions math.order
math.parser prettyprint ;

"a=" "b=" [ write readln string>number ] bi@
{
    [ + "sum: " write . ]
    [ - "difference: " write . ]
    [ * "product: " write . ]
    [ / "quotient: " write . ]
    [ /i "integer quotient: " write . ]
    [ rem "remainder: " write . ]
    [ mod "modulo: " write . ]
    [ max "maximum: " write . ]
    [ min "minimum: " write . ]
    [ gcd "gcd: " write . drop ]
    [ lcm "lcm: " write . ]
} 2cleave
```


{{out}}

```txt
a=8
b=12
sum: 20
difference: -4
product: 96
quotient: 2/3
integer quotient: 0
remainder: 8
modulo: 8
maximum: 12
minimum: 8
gcd: 4
lcm: 24
```


This example illustrates the use of cleave and apply combinators to alleviate the usage of shuffle words in a concatenative language.
bi@ applies a quotation to 2 inputs and 2cleave applies a sequence of quotations to 2 inputs.


## FALSE


```false
12 7
\$@$@$@$@$@$@$@$@$@$@\  { 6 copies }
"sum = "+."
difference = "-."
product = "*."
quotient = "/."
modulus = "/*-."
"
```



## Forth

To keep the example simple, the word takes the two numbers from the stack.  '''/mod''' returns two results; the stack effect is ( a b -- a%b a/b ).

```forth
: arithmetic ( a b -- )
  cr ." a=" over . ." b=" dup .
  cr ." a+b=" 2dup + .
  cr ." a-b=" 2dup - .
  cr ." a*b=" 2dup * .
  cr ." a/b=" /mod .
  cr ." a mod b = " . cr ;
```


Different host systems have different native signed division behavior. ANS Forth defines two primitive double-precision signed division operations, from which the implementation may choose the most natural to implement the basic divide operations ( / , /mod , mod , */ ). This is partly due to differing specifications in the two previous standards, Forth-79 and Forth-83.


```forth
FM/MOD ( d n -- mod div )   \ floored
SM/REM ( d n -- rem div )   \ symmetric
M* ( n n -- d )
```


In addition, there are unsigned variants.


```forth
UM/MOD ( ud u -- umod udiv )
UM* ( u u -- ud )
```



## Fortran

In ANSI FORTRAN 77 or later:

```fortran
 INTEGER A, B
 PRINT *, 'Type in two integer numbers separated by white space',
+         ' and press ENTER'
 READ *, A, B
 PRINT *, '   A + B = ', (A + B)
 PRINT *, '   A - B = ', (A - B)
 PRINT *, '   A * B = ', (A * B)
 PRINT *, '   A / B = ', (A / B)
 PRINT *, 'MOD(A,B) = ', MOD(A,B)
 PRINT *
 PRINT *, 'Even though you did not ask, ',
+         'exponentiation is an intrinsic op in Fortran, so...'
 PRINT *, '  A ** B = ', (A ** B)
 END
```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Dim As Integer i, j
Input "Enter two integers separated by a comma"; i, j
Print i;" + "; j; " = "; i + j
Print i;" - "; j; " = "; i - j
Print i;" * "; j; " = "; i * j
Print i;" / "; j; " = "; i \ j
Print i;" % "; j; " = "; i Mod j
Print i;" ^ "; j; " = "; i ^ j
Sleep

' Integer division (for which FB uses the '\' operator) rounds towards zero

' Remainder (for which FB uses the Mod operator) will, if non-zero, match the sign
' of the first operand
```


Sample input and output:-
{{out}}

```txt

Enter two integers separated by a comma? -12, 7
-12 +  7 = -5
-12 -  7 = -19
-12 *  7 = -84
-12 /  7 = -1
-12 %  7 = -5
-12 ^  7 = -35831808

```


=={{header|F_Sharp|F#}}==
As F# is a functional language, we can easily create a list of pairs of the string name of a function and the function itself to iterate over printing the operation and applying the function to obtain the result:

```fsharp

do
  let a, b = int Sys.argv.[1], int Sys.argv.[2]
  for str, f in ["+", ( + ); "-", ( - ); "*", ( * ); "/", ( / ); "%", ( % )] do
    printf "%d %s %d = %d\n" a str b (f a b)

```

For example, the output with the arguments 4 and 3 is:

```fsharp

4 + 3 = 7
4 - 3 = 1
4 * 3 = 12
4 / 3 = 1
4 % 3 = 1

```




## friendly interactive shell


```fishshell

read a
read b
echo 'a + b =' (math "$a + $b") # Sum
echo 'a - b =' (math "$a - $b") # Difference
echo 'a * b =' (math "$a * $b") # Product
echo 'a / b =' (math "$a / $b") # Integer quotient
echo 'a % b =' (math "$a % $b") # Remainder
echo 'a ^ b =' (math "$a ^ $b") # Exponentation

```



## Frink

This demonstrates normal division (which produces rational numbers when possible), <CODE>div</CODE>, and <CODE>mod</CODE>.  <CODE>div</CODE> rounds toward negative infinity (defined as <CODE>floor[x/y]</CODE>).  <CODE>mod</CODE> uses the sign of the second number (defined as <CODE>x - y * floor[x/y]</CODE>).  All operators automatically produce big integers or exact rational numbers when necessary.

```frink

[a,b] = input["Enter numbers",["a","b"]]
ops=["+", "-", "*", "/", "div" ,"mod" ,"^"]
for op = ops
{
   str = "$a $op $b"
   println["$str = " + eval[str]]
}

```


{{out}}

```frink

10 + 20 = 30
10 - 20 = -10
10 * 20 = 200
10 / 20 = 1/2 (exactly 0.5)
10 div 20 = 0
10 mod 20 = 10
10 ^ 20 = 100000000000000000000

```




## FutureBasic


```futurebasic

include "ConsoleWindow"

dim as Str31 a, b
dim as long i1, i2

input "Enter the first integer: "; a
print
input "Enter the second integer: "; b
print : print

i1 = val(a) : i2 = val(b)

print "  Number 1:"; i1
print "  Number 2:"; i2
print
print "      Add: "; i1; " +"; i2; " ="; i1 + i2
print " Subtract: "; i1; " -"; i2; " ="; i1 - i2
print " Multiply: "; i1; " *"; i2; " ="; i1 * i2

if i2 != 0
   print "   Divide: "; i1; " /"; i2; " ="; i1 / i2
   print i1; " mod"; i2; " ="; i1 MOD i2; " remainder"
   print i1; " raised to power of"; i2; " ="; i1 ^ i2
 else
   print "Cannot divide by zero."
end if

```


Output:

```txt

Enter the first integer: 25
Enter the second integer: 53

  Number 1: 25
  Number 2: 53

      Add:  25 + 53 = 78
 Subtract:  25 - 53 =-28
 Multiply:  25 * 53 = 1325
   Divide:  25 / 53 = 0
 25 mod 53 = 25 remainder
 25 raised to power of 53 = 1.23259516e+74

```



## GAP


```gap
run := function()
  local a, b, f;
  f := InputTextUser();
  Print("a =\n");
  a := Int(Chomp(ReadLine(f)));
  Print("b =\n");
  b := Int(Chomp(ReadLine(f)));
  Display(Concatenation(String(a), " + ", String(b), " = ", String(a + b)));
  Display(Concatenation(String(a), " - ", String(b), " = ", String(a - b)));
  Display(Concatenation(String(a), " * ", String(b), " = ", String(a * b)));
  Display(Concatenation(String(a), " / ", String(b), " = ", String(QuoInt(a, b)))); # toward 0
  Display(Concatenation(String(a), " mod ", String(b), " = ", String(RemInt(a, b)))); # nonnegative
  Display(Concatenation(String(a), " ^ ", String(b), " = ", String(a ^ b)));
  CloseStream(f);
end;
```



## Genie

Note: Using ''init:int'' and the ''return'' from the init block was introduced in release 0.43.92, February 2019.


```genie
[indent=4]
/*
   Arithmethic/Integer, in Genie
   valac arithmethic-integer.gs
*/

init:int
    a:int = 0
    b:int = 0
    if args.length > 2 do b = int.parse(args[2])
    if args.length > 1 do a = int.parse(args[1])

    print @"a+b: $a plus  $b is $(a+b)"
    print @"a-b: $a minus $b is $(a-b)"
    print @"a*b: $a times $b is $(a*b)"
    print @"a/b: $a by    $b quotient is  $(a/b)  (rounded mode is TRUNCATION)"
    print @"a%b: $a by    $b remainder is $(a%b)  (sign matches first operand)"

    print "\nGenie does not include a raise to power operator"

    return 0
```


{{out}}

```txt
prompt$ valac arithmetic-integer.gs
prompt$ ./arithmetic-integer -390 100
a+b: -390 plus  100 is -290
a-b: -390 minus 100 is -490
a*b: -390 times 100 is -39000
a/b: -390 by    100 quotient is  -3  (rounded mode is TRUNCATION)
a%b: -390 by    100 remainder is -90  (sign matches first operand)

Genie does not include a raise to power operator
```



## GEORGE


```GEORGE
R (m) ;
R (n) ;
m n + P;
m n - P;
m n × P;
m n div P;
m n rem P;
```



## Go


### int


```go
package main

import "fmt"

func main() {
    var a, b int
    fmt.Print("enter two integers: ")
    fmt.Scanln(&a, &b)
    fmt.Printf("%d + %d = %d\n", a, b, a+b)
    fmt.Printf("%d - %d = %d\n", a, b, a-b)
    fmt.Printf("%d * %d = %d\n", a, b, a*b)
    fmt.Printf("%d / %d = %d\n", a, b, a/b)  // truncates towards 0
    fmt.Printf("%d %% %d = %d\n", a, b, a%b) // same sign as first operand
    // no exponentiation operator
}
```

{{out|Example run}}

```txt

enter two integers: -5 3
-5 + 3 = -2
-5 - 3 = -8
-5 * 3 = -15
-5 / 3 = -1
-5 % 3 = -2

```


### big.Int


```go
package main

import (
    "fmt"
    "math/big"
)

func main() {
    var a, b, c big.Int
    fmt.Print("enter two integers: ")
    fmt.Scan(&a, &b)
    fmt.Printf("%d + %d = %d\n", &a, &b, c.Add(&a, &b))
    fmt.Printf("%d - %d = %d\n", &a, &b, c.Sub(&a, &b))
    fmt.Printf("%d * %d = %d\n", &a, &b, c.Mul(&a, &b))

    // Quo, Rem functions work like Go operators on int:
    // quo truncates toward 0,
    // and a non-zero rem has the same sign as the first operand.
    fmt.Printf("%d quo %d = %d\n", &a, &b, c.Quo(&a, &b))
    fmt.Printf("%d rem %d = %d\n", &a, &b, c.Rem(&a, &b))

    // Div, Mod functions do Euclidean division:
    // the result m = a mod b is always non-negative,
    // and for d = a div b, the results d and m give d*y + m = x.
    fmt.Printf("%d div %d = %d\n", &a, &b, c.Div(&a, &b))
    fmt.Printf("%d mod %d = %d\n", &a, &b, c.Mod(&a, &b))

    // as with int, no exponentiation operator
}
```

{{out|Example run}}

```txt

enter two integers: -5 3
-5 + 3 = -2
-5 - 3 = -8
-5 * 3 = -15
-5 quo 3 = -1
-5 rem 3 = -2
-5 div 3 = -2
-5 mod 3 = 1

```



## Groovy

'''Solution:'''

```groovy
def arithmetic = { a, b ->
    println """
       a + b =        ${a} + ${b} = ${a + b}
       a - b =        ${a} - ${b} = ${a - b}
       a * b =        ${a} * ${b} = ${a * b}
       a / b =        ${a} / ${b} = ${a / b}   !!! Converts to floating point!
(int)(a / b) = (int)(${a} / ${b}) = ${(int)(a / b)}              !!! Truncates downward after the fact
 a.intdiv(b) =  ${a}.intdiv(${b}) = ${a.intdiv(b)}              !!! Behaves as if truncating downward, actual implementation varies
       a % b =        ${a} % ${b} = ${a % b}

Exponentiation is also a base arithmetic operation in Groovy, so:
      a ** b =       ${a} ** ${b} = ${a ** b}
"""
}
```


'''Test:'''

```groovy
arithmetic(5,3)
```


{{out}}

```txt
       a + b =        5 + 3 = 8
       a - b =        5 - 3 = 2
       a * b =        5 * 3 = 15
       a / b =        5 / 3 = 1.6666666667   !!! Converts to floating point!
(int)(a / b) = (int)(5 / 3) = 1              !!! Truncates downward after the fact
 a.intdiv(b) =  5.intdiv(3) = 1              !!! Behaves as if truncating downward, actual implementation varies
       a % b =        5 % 3 = 2

Exponentiation is also a base arithmetic operation in Groovy, so:
      a ** b =       5 ** 3 = 125
```



## Harbour


```visualfoxpro
procedure Test( a, b )
   ? "a+b", a + b
   ? "a-b", a - b
   ? "a*b", a * b
   // The quotient isn't integer, so we use the Int() function, which truncates it downward.
   ? "a/b", Int( a / b )
   // Remainder:
   ? "a%b", a % b
   // Exponentiation is also a base arithmetic operation
   ? "a**b", a ** b
   return
```



## Haskell



```haskell
main = do
  a <- readLn :: IO Integer
  b <- readLn :: IO Integer
  putStrLn $ "a + b = " ++ show (a + b)
  putStrLn $ "a - b = " ++ show (a - b)
  putStrLn $ "a * b = " ++ show (a * b)
  putStrLn $ "a to the power of b = " ++ show (a ** b)
  putStrLn $ "a to the power of b = " ++ show (a ^ b)
  putStrLn $ "a to the power of b = " ++ show (a ^^ b)
  putStrLn $ "a `div` b = "  ++ show (a `div` b)  -- truncates towards negative infinity
  putStrLn $ "a `mod` b = "  ++ show (a `mod` b)  -- same sign as second operand
  putStrLn $ "a `divMod` b = "  ++ show (a `divMod` b)
  putStrLn $ "a `quot` b = " ++ show (a `quot` b) -- truncates towards 0
  putStrLn $ "a `rem` b = "  ++ show (a `rem` b)  -- same sign as first operand
  putStrLn $ "a `quotRem` b = "  ++ show (a `quotRem` b)
```



## Haxe


```haxe
class BasicIntegerArithmetic {
    public static function main() {
        var args =Sys.args();
        if (args.length < 2) return;
        var a = Std.parseFloat(args[0]);
        var b = Std.parseFloat(args[1]);
        trace("a+b = " + (a+b));
        trace("a-b = " + (a-b));
        trace("a*b = " + (a*b));
        trace("a/b = " + (a/b));
        trace("a%b = " + (a%b));
    }
}
```



## HicEst

All numeric is 8-byte-float. Conversions are by INT, NINT, FLOOR, CEILING, or Formatted IO

```hicest
DLG(Edit=A, Edit=B, TItle='Enter numeric A and B')
WRITE(Name) A, B
WRITE() '              A + B = ', A + B
WRITE() '              A - B = ', A - B
WRITE() '              A * B = ', A * B
WRITE() '              A / B = ', A / B          ! no truncation
WRITE() 'truncate      A / B = ', INT(A / B)     ! truncates towards 0
WRITE() 'round next    A / B = ', NINT(A / B)    ! truncates towards next integer
WRITE() 'round down    A / B = ', FLOOR(A / B)   ! truncates towards minus infinity
WRITE() 'round up      A / B = ', CEILING(A / B) ! truncates towards plus infinity
WRITE() 'remainder of  A / B = ', MOD(A, B)      ! same sign as A
WRITE() 'A to the power of B = ', A ^ B
WRITE() 'A to the power of B = ', A ** B
```


```hicest
A=5; B=-4;
              A + B = 1
              A - B = 9
              A * B = -20
              A / B = -1.25
truncate      A / B = -1
round next    A / B = -1
round down    A / B = -2
round up      A / B = -1
remainder of  A / B = 1
A to the power of B = 16E-4
A to the power of B = 16E-4
```



## HolyC


```holyc
I64 *a, *b;
a = Str2I64(GetStr("Enter your first number: "));
b = Str2I64(GetStr("Enter your second number: "));

if (b == 0)
  Print("Error: The second number must not be zero.\n");
else {
  Print("a + b = %d\n", a + b);
  Print("a - b = %d\n", a - b);
  Print("a * b = %d\n", a * b);
  Print("a / b = %d\n", a / b); /* rounds down */
  Print("a % b = %d\n", a % b); /* same sign as first operand */
  Print("a ` b = %d\n", a ` b);
}
```



## i


```i
main
	a $= integer(in(' ')); ignore
	b $= integer(in('\n')); ignore

	print("Sum:"		, a + b)
	print("Difference:", a - b)
	print("Product:"	, a * b)
	print("Quotient:"	, a / b) // rounds towards zero
	print("Modulus:"	, a % b) // same sign as first operand
	print("Exponent:"	, a ^ b)
}
```


=={{header|Icon}} and {{header|Unicon}}==

```Icon
procedure main()
writes("Input 1st integer a := ")
a := integer(read())
writes("Input 2nd integer b := ")
b := integer(read())

write(" a + b = ",a+b)
write(" a - b = ",a-b)
write(" a * b = ",a*b)
write(" a / b = ",a/b, " rounds toward 0")
write(" a % b = ",a%b, " remainder sign matches a")
write(" a ^ b = ",a^b)
end
```



## Inform 7



```inform7
Enter Two Numbers is a room.

Numerically entering is an action applying to one number. Understand "[number]" as numerically entering.

The first number is a number that varies.

After numerically entering for the first time:
	now the first number is the number understood.

After numerically entering for the second time:
	let A be the first number;
	let B be the number understood;
	say "[A] + [B] = [A + B]."; [operator syntax]
	say "[A] - [B] = [A minus B]."; [English syntax]
	let P be given by P = A * B where P is a number; [inline equation]
	say "[A] * [B] = [P].";
	let Q be given by the Division Formula; [named equation]
	say "[A] / [B] = [Q].";
	say "[A] mod [B] = [remainder after dividing A by B].";
	end the story.

Equation - Division Formula
	Q = A / B
where Q is a number, A is a number, and B is a number.
```


This solution shows four syntaxes: mathematical operators, English operators, inline equations, and named equations. Division rounds toward zero, and the remainder has the same sign as the quotient.


## J


```j
calc =:    + , - , * , <.@% , |~ , ^
```

The function <code>calc</code> constructs a list of numeric results for this task. The implementation of integer division we use here (<code><.@%.</code>) rounds down (towards negative infinity), and this is compatible with the remainder implementation we use here.

```j
   17 calc 3
20 14 51 5 2 4913
```


The function <code>bia</code> assembles these results, textually:


```j
labels  =: ];.2 'Sum: Difference: Product: Quotient: Remainder: Exponentiation: '
combine =: ,. ":@,.
bia     =: labels combine calc

   17 bia 3
Sum:              20
Difference:       14
Product:          51
Quotient:          5
Remainder:         2
Exponentiation: 4913
```



## Java


```java
import java.util.Scanner;

public class IntegerArithmetic {
    public static void main(String[] args) {
        // Get the 2 numbers from command line arguments
        Scanner sc = new Scanner(System.in);
        int a = sc.nextInt();
        int b = sc.nextInt();

        int sum = a + b;        // The result of adding 'a' and 'b' (Note: integer addition is discouraged in print statements due to confusion with string concatenation)
        int difference = a - b; // The result of subtracting 'b' from 'a'
        int product = a * b;    // The result of multiplying 'a' and 'b'
        int division = a / b;   // The result of dividing 'a' by 'b' (Note: 'division' does not contain the fractional result)
        int remainder = a % b;  // The remainder of dividing 'a' by 'b'

        System.out.println("a + b = " + sum);
        System.out.println("a - b = " + difference);
        System.out.println("a * b = " + product);
        System.out.println("quotient of a / b = " + division);   // truncates towards 0
        System.out.println("remainder of a / b = " + remainder);   // same sign as first operand
    }
}
```



## JavaScript


### WScript

{{works with|JScript}}
{{works with|SpiderMonkey}}
Note that the operators work the same in all versions of JavaScript; the requirement for specific implementations is in order to get user input.

```javascript
var a = parseInt(get_input("Enter an integer"), 10);
var b = parseInt(get_input("Enter an integer"), 10);

WScript.Echo("a = " + a);
WScript.Echo("b = " + b);
WScript.Echo("sum: a + b = "        + (a + b));
WScript.Echo("difference: a - b = " + (a - b));
WScript.Echo("product: a * b = "    + (a * b));
WScript.Echo("quotient: a / b = "   + (a / b | 0)); // "| 0" casts it to an integer
WScript.Echo("remainder: a % b = "  + (a % b));

function get_input(prompt) {
    output(prompt);
    try {
        return WScript.StdIn.readLine();
    } catch(e) {
        return readline();
    }
}
function output(prompt) {
    try {
        WScript.Echo(prompt);
    } catch(e) {
        print(prompt);
    }
}
```

{{out}}

```txt
Enter an integer
-147
Enter an integer
63
a = -147
b = 63
sum: a + b = -84
difference: a - b = -210
product: a * b = -9261
quotient: a / b = -2
remainder: a % b = -21
```


### Node.JS


```javascript
// Invoked as node script_name.js <a> <b>. Positions 0 and 1 in the argv array contain 'node' and 'script_name.js' respectively
var a = parseInt(process.argv[2], 10);
var b = parseInt(process.argv[3], 10);

var sum = a + b;
var difference = a - b;
var product = a * b;
var division = a / b;
var remainder = a % b;  // This produces the remainder after dividing 'b' into 'a'. The '%' operator is called the 'modulo' operator

console.log('a + b = %d', sum);  // The %d syntax is a placeholder that is replaced by the sum
console.log('a - b = %d', difference);
console.log('a * b = %d', product);
console.log('a / b = %d', division);
console.log('a % b = %d', remainder);
```

{{out}}

```txt
a + b = 17
a - b = 3
a * b = 70
a / b = 1.4285714285714286
a % b = 3
```



## jq


```jq
# Lines which do not have two integers are skipped:

def arithmetic:
  split(" ") | select(length > 0) | map(tonumber)
  | if length > 1 then
    .[0] as $a | .[1] as $b
    | "For a = \($a) and b = \($b):\n" +
      "a + b = \($a + $b)\n" +
      "a - b = \($a - $b)\n" +
      "a * b = \($a * $b)\n" +
      "a/b|floor = \($a / $b | floor)\n" +
      "a % b = \($a % $b)\n" +
      "a | exp = \($a | exp)\n"
    else empty
    end ;

arithmetic

```

{{Out}}

```txt

$ jq -R -r -f arithmetic.jq
7 -2
For a = 7 and b = -2:
a + b = 5
a - b = 9
a * b = -14
a/b|floor = -4
a % b = 1
a | exp = 1096.6331584284585

2 -7
For a = 2 and b = -7:
a + b = -5
a - b = 9
a * b = -14
a/b|floor = -1
a % b = 2
a | exp = 7.38905609893065

-2 -7
For a = -2 and b = -7:
a + b = -9
a - b = 5
a * b = 14
a/b|floor = 0
a % b = -2
a | exp = 0.1353352832366127
```



## Jsish


```javascript
"use strict";
/* Arthimetic/Integer, in Jsish */
var line = console.input();
var nums = line.match(/^\s*([+-]?[0-9]+)\s+([+-]?[0-9]+)\s*/);
var a = Number(nums[1]);
var b = Number(nums[2]);

puts("A is ", a, ", B is ", b);
puts("Sum               A + B is ", a + b);
puts("Difference        A - B is ", a - b);
puts("Product           A * B is ", a * b);
puts("Integer quotient  A / B is ", a / b | 0, " truncates toward 0");
puts("Remainder         A % B is ", a % b, " sign follows first operand");
puts("Exponentiation    A to the power B is ", Math.pow(a, b));

/*
=!INPUTSTART!=
7 4
=!INPUTEND!=
*/


/*
=!EXPECTSTART!=
A is  7 , B is  4
Sum               A + B is  11
Difference        A - B is  3
Product           A * B is  28
Integer quotient  A / B is  1  truncates toward 0
Remainder         A % B is  3  sign follows first operand
Exponentiation    A to the power B is  2401
=!EXPECTEND!=
*/
```


{{out}}

```txt
prompt$ jsish -u arithmeticInteger.jsi
[PASS] arithmeticInteger.jsi
```



## Julia


```Julia
function arithmetic (a = parse(Int, readline()), b = parse(Int, readline()))
  for op in  [+,-,*,div,rem]
    println("a $op b = $(op(a,b))")
  end
end
```

{{Out}}

```txt
julia> arithmetic()
4
5
a + b = 9
a - b = -1
a * b = 20
a div b = 0
a rem b = 4
```



## Kotlin


```scala
// version 1.1

fun main(args: Array<String>) {
    val r = Regex("""-?\d+[ ]+-?\d+""")
    while(true) {
        print("Enter two integers separated by space(s) or q to quit: ")
        val input: String = readLine()!!.trim()
        if (input == "q" || input == "Q") break
        if (!input.matches(r)) {
            println("Invalid input, try again")
            continue
        }
        val index = input.lastIndexOf(' ')
        val a = input.substring(0, index).trimEnd().toLong()
        val b = input.substring(index + 1).toLong()
        println("$a + $b = ${a + b}")
        println("$a - $b = ${a - b}")
        println("$a * $b = ${a * b}")
        if (b != 0L) {
            println("$a / $b = ${a / b}")  // rounds towards zero
            println("$a % $b = ${a % b}")  // if non-zero, matches sign of first operand
        }
        else {
            println("$a / $b = undefined")
            println("$a % $b = undefined")
        }
        val d = Math.pow(a.toDouble(), b.toDouble())
        print("$a ^ $b = ")
        if (d % 1.0 == 0.0) {
            if (d >= Long.MIN_VALUE.toDouble() && d <= Long.MAX_VALUE.toDouble())
                println("${d.toLong()}")
            else
                println("out of range")
        }
        else if (!d.isFinite())
            println("not finite")
        else
            println("not integral")
        println()
    }
}
```


{{out}}

```txt

Enter two integers separated by space(s) or q to quit: 2 63
2 + 63 = 65
2 - 63 = -61
2 * 63 = 126
2 / 63 = 0
2 % 63 = 2
2 ^ 63 = 9223372036854775807

Enter two integers separated by space(s) or q to quit: -3 50
-3 + 50 = 47
-3 - 50 = -53
-3 * 50 = -150
-3 / 50 = 0
-3 % 50 = -3
-3 ^ 50 = out of range

Enter two integers separated by space(s) or q to quit: q

```



## LabVIEW

{{VI snippet}}<br/>
[[File:LabVIEW_Arithmetic_Integer.png]]




## Lasso


```Lasso
local(a = 6, b = 4)
#a + #b // 10
#a - #b // 2
#a * #b // 24
#a / #b // 1
#a % #b // 2
math_pow(#a,#b) // 1296
math_pow(#b,#a) // 4096
```



## LFE



```lisp

(defmodule arith
  (export all))

(defun demo-arith ()
  (case (: io fread '"Please enter two integers: " '"~d~d")
    ((tuple 'ok (a b))
      (: io format '"~p + ~p = ~p~n" (list a b (+ a b)))
      (: io format '"~p - ~p = ~p~n" (list a b (- a b)))
      (: io format '"~p * ~p = ~p~n" (list a b (* a b)))
      (: io format '"~p^~p = ~p~n" (list a b (: math pow a b)))
      ; div truncates towards zero
      (: io format '"~p div ~p = ~p~n" (list a b (div a b)))
      ; rem's result takes the same sign as the first operand
      (: io format '"~p rem ~p = ~p~n" (list a b (rem a b))))))

```


Usage from the LFE REPL:

```lisp

> (slurp '"arith.lfe")
#(ok arith)
> (demo-arith)
Please enter two integers: 2 8
2 + 8 = 10
2 - 8 = -6
2 * 8 = 16
2^8 = 256.0
2 div 8 = 0
2 rem 8 = 2
ok

```



## Liberty BASIC

Note that raising to a power can display very large integers without going to approximate power-of-ten notation.

```lb

input "Enter the first integer:  "; first
input "Enter the second integer: "; second

print "The sum is " ; first + second
print "The difference is " ; first -second
print "The product is " ; first *second
if second <>0 then print "The integer quotient is " ; int( first /second); " (rounds towards 0)" else print "Division by zero not allowed."
print "The remainder is " ; first MOD second; " (sign matches first operand)"
print "The first raised to the power of the second is " ; first ^second

```



## LIL


```tcl
# Arithmetic/Integer, in LIL
write "Enter two numbers separated by space: "
if {[canread]} {set line [readline]}
print

set a [index $line 0]
set b [index $line 1]
print "A is $a"", B is $b"
print "Sum               A + B is [expr $a + $b]"
print "Difference        A - B is [expr $a - $b]"
print "Product           A * B is [expr $a * $b]"
print "Integer Quotient  A \\ B is [expr $a \ $b], truncates toward zero"
print "Remainder         A % B is [expr $a % $b], sign follows first operand"
print "LIL has no exponentiation expression operator"
```


{{out}}

```txt
prompt$ echo '7 4' | lil arithmeticInteger.lil
Enter two numbers separated by space:
A is 7, B is 4
Sum               A + B is 11
Difference        A - B is 3
Product           A * B is 28
Integer Quotient  A \ B is 1, truncates toward zero
Remainder         A % B is 3, sign follows first operand
LIL has no exponentiation expression operator

prompt$ echo '-7 4' | lil arithmeticInteger.lil
Enter two numbers separated by space:
A is -7, B is 4
Sum               A + B is -3
Difference        A - B is -11
Product           A * B is -28
Integer Quotient  A \ B is -1, truncates toward zero
Remainder         A % B is -3, sign follows first operand
LIL has no exponentiation expression operator
```



## Lingo


```Lingo
-- X, Y: 2 editable field members, shown as sprites in the current GUI
x = integer(member("X").text)
y = integer(member("Y").text)

put "Sum: "       , x + y
put "Difference: ", x - y
put "Product: "   , x * y
put "Quotient: "  , x / y   -- Truncated towards zero
put "Remainder: " , x mod y -- Result has sign of left operand
put "Exponent: "  , power(x, y)
```



## Little


```C
# Maybe you need to import the mathematical funcions
# from Tcl with:
# eval("namespace path ::tcl::mathfunc");

void main() {
    int a, b;
    puts("Enter two integers:");
    a = (int)(gets(stdin));
    b = (int)(gets(stdin));
    puts("${a} + ${b} = ${a+b}");
    puts("${a} - ${b} = ${a-b}");
    puts("${a} * ${b} = ${a*b}");
    puts("${a} / ${b} = ${a/b}, remainder ${a%b}");
    puts("${a} to the power of ${b} = ${(int)pow(a,b)}");
}
```



## LiveCode


```LiveCode
ask "enter 2 numbers (comma separated)"
if it is not empty then
    put item 1 of it into n1
    put item 2 of it into n2
    put sum(n1,n2) into ai["sum"]
    put n1 * n2 into ai["product"]
    put n1 div n2 into ai["quotient"]  -- truncates
    put n1 mod n2 into ai["remainder"]
    put n1^n2 into ai["power"]
    combine ai using comma and colon
    put ai
end if
```

Examples<lang>-2,4  - power:16,product:-8,quotient:0,remainder:-2,sum:2
2,-4  - power:0.0625,product:-8,quotient:0,remainder:2,sum:-2
-2,-4 - power:0.0625,product:8,quotient:0,remainder:-2,sum:-6
2,4   - power:16,product:8,quotient:0,remainder:2,sum:6
11,4  - power:14641,product:44,quotient:2,remainder:3,sum:15
```



## Logo


```logo
to operate :a :b
  (print [a =] :a)
  (print [b =] :b)
  (print [a + b =] :a + :b)
  (print [a - b =] :a - :b)
  (print [a * b =] :a * :b)
  (print [a / b =] int :a / :b)
  (print [a mod b =] modulo :a :b)
end
```


Each infix operator also has a prefix synonym (sum, difference, product, quotient). Sum and product can also have arity greater than two when used in parentheses (sum 1 2 3). Infix operators in general have high precedence; you may need to enclose their arguments in parentheses to obtain the correct expression.


## LSE64


```lse64
over : 2 pick
2dup : over over

arithmetic : \
  " A=" ,t over , sp " B=" ,t dup , nl \
  " A+B=" ,t 2dup + , nl \
  " A-B=" ,t 2dup - , nl \
  " A*B=" ,t 2dup * , nl \
  " A/B=" ,t 2dup / , nl \
  " A%B=" ,t      % , nl
```



## Lua


```lua
local x = io.read()
local y = io.read()

print ("Sum: "       , (x + y))
print ("Difference: ", (x - y))
print ("Product: "   , (x * y))
print ("Quotient: "  , (x / y)) -- Does not truncate
print ("Remainder: " , (x % y)) -- Result has sign of right operand
print ("Exponent: "  , (x ^ y))
```



## M2000 Interpreter

We can  use variables with %, which are double inside with no decimal part. These can have 17 digits. Also A%=1.5 make it 2, not 1. This has a tricky situation: A%=1/2 give 1 to A%. We can use FLOOR() or INT() is the same, or CEIL(), and there is a BANK() which is a Banker Round: BANK(2.5)=2 and BANK(3.5)=4.





```M2000 Interpreter

MODULE LikeCommodoreBasic {
      \\ ADDITION: EUCLIDEAN DIV# & MOD# AND ** FOR POWER INCLUDING ^
      10 INPUT "ENTER A NUMBER:"; A%
      20 INPUT "ENTER ANOTHER NUMBER:"; B%
      30 PRINT "ADDITION:";A%;"+";B%;"=";A%+B%
      40 PRINT "SUBTRACTION:";A%;"-";B%;"=";A%-B%
      50 PRINT "MULTIPLICATION:";A%;"*";B%;"=";A%*B%
      60 PRINT "INTEGER DIVISION:";A%;"DIV";B%;"=";A% DIV B%
      65 PRINT "INTEGER EUCLIDEAN DIVISION:";A%;"DIV";B%;"=";A% DIV# B%
      70 PRINT "REMAINDER OR MODULO:";A%;"MOD";B%;"=";A% MOD B%
      75 PRINT "EUCLIDEAN REMAINDER OR MODULO:";A%;"MOD#";B%;"=";A% MOD# B%
      80 PRINT "POWER:";A%;"^";B%;"=";A%^B%
      90 PRINT "POWER:";A%;"**";B%;"=";A%**B%
}
LikeCommodoreBasic


Module IntegerTypes {
      a=12% ' Integer 16 bit
      b=12& ' Long 32 bit
      c=12@' Decimal (29 digits)
      Def ExpType$(x)=Type$(x)
      Print ExpType$(a+1)="Double"
      Print ExpType$(a+1%)="Integer"
      Print ExpType$(a div 5)="Double"
      Print ExpType$(a div 5%)="Double"
      Print ExpType$(a mod 5)="Double"
      Print ExpType$(a mod 5%)="Double"
      Print ExpType$(a**2)="Double"

      Print ExpType$(b+1)="Double"
      Print ExpType$(b+1&)="Long"
      Print ExpType$(b div 5)="Double"
      Print ExpType$(b div 5&)="Double"
      Print ExpType$(b mod 5)="Double"
      Print ExpType$(b mod 5&)="Double"
      Print ExpType$(b**2)="Double"

      Print ExpType$(c+1)="Decimal"
      Print ExpType$(c+1@)="Decimal"
      Print ExpType$(c div 5)="Decimal"
      Print ExpType$(c div 5@)="Decimal"
      Print ExpType$(c mod 5)="Decimal"
      Print ExpType$(c mod 5@)="Decimal"
      Print ExpType$(c**2)="Double"
}
IntegerTypes

```



## M4


Because of the particular nature of M4, the only user-input is the code itself. Anyway the following code can be used:

```m4
eval(A+B)
eval(A-B)
eval(A*B)
eval(A/B)
eval(A%B)
```


once saved in a file, e.g. <tt>operations.m4</tt>:


```txt
m4 -DA=4 -DB=6 operations.m4
```


or using a sort of ''driver'':


```m4
define(`A', 4)dnl
define(`B', 6)dnl
include(`operations.m4')
```



## Maple

These operations are all built-in.  As all operations are exact, there are no rounding issues involved.

```Maple

DoIt := proc()
        local a := readstat( "Input an integer: " ):
        local b := readstat( "Input another integer: " ):
        printf( "Sum = %d\n",  a + b ):
        printf( "Difference = %d\n",  a - b ):
        printf( "Product = %d\n",  a * b ):
        printf( "Quotient = %d\n",  iquo( a, b, 'c' ) ):
        printf( "Remainder = %d\n", c ); # or irem( a, b )
        NULL # quiet return
end proc:

```

Here is an example of calling DoIt.

```Maple

> DoIt();
Input an integer: 15;
Input another integer: 12;
Sum = 27
Difference = 3
Product = 180
Quotient = 1
Remainder = 3
>

```



## Mathematica

Mathematica has all the function built-in to handle this task. Example:

```Mathematica
a = Input["Give me an integer please!"];
b = Input["Give me another integer please!"];
Print["You gave me ", a, " and ", b];
Print["sum: ", a + b];
Print["difference: ", a - b];
Print["product: ", a b];
Print["integer quotient: ", IntegerPart[a/b]];
Print["remainder: ", Mod[a, b]];
Print["exponentiation: ", a^b];
```

gives back for input 17 and 3:
<preMathematica>You gave me 17 and 3
sum: 20
difference: 14
product: 51
integer quotient: 5
remainder: 2
exponentiation: 4913
```


=={{header|MATLAB}} / {{header|Octave}}==

```octave
disp("integer a: "); a = scanf("%d", 1);
disp("integer b: "); b = scanf("%d", 1);
a+b
a-b
a*b
floor(a/b)
mod(a,b)
a^b
```



## Maxima


```maxima
block(
   [a: read("a"), b: read("b")],
   print(a + b),
   print(a - b),
   print(a * b),
   print(a / b),
   print(quotient(a, b)),
   print(remainder(a, b)),
   a^b
);
```



## MAXScript


```maxscript
x = getKBValue prompt:"First number"
y = getKBValue prompt:"Second number:"

format "Sum: %\n" (x + y)
format "Difference: %\n" (x - y)
format "Product: %\n" (x * y)
format "Quotient: %\n" (x / y)
format "Remainder: %\n" (mod x y)
```



## Mercury

<lang>
:- module arith_int.
:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module int, list, string.

main(!IO) :-
    io.command_line_arguments(Args, !IO),
    ( if
        Args = [AStr, BStr],
        string.to_int(AStr, A),
        string.to_int(BStr, B)
      then
        io.format("A + B = %d\n", [i(A + B)], !IO),
        io.format("A - B = %d\n", [i(A - B)], !IO),
        io.format("A * B = %d\n", [i(A * B)], !IO),

        % Division: round towards zero.
        %
        io.format("A / B = %d\n", [i(A / B)], !IO),

        % Division: round towards minus infinity.
        %
        io.format("A div B = %d\n", [i(A div B)], !IO),

        % Modulus: X mod Y = X - (X div Y) * Y.
        %
        io.format("A mod B = %d\n", [i(A mod B)], !IO),

        % Remainder: X rem Y = X - (X / Y) * Y.
        %
        io.format("A rem B = %d\n", [i(A rem B)], !IO),

        % Exponentiation is done using the function int.pow/2.
        %
        io.format("A `pow` B = %d\n", [i(A `pow` B)], !IO)
      else
        io.set_exit_status(1, !IO)
    ).

```



## Metafont



```metafont
string s[];
message "input number a: ";
s1 := readstring;
message "input number b: ";
s2 := readstring;
a := scantokens s1;
b := scantokens s2;

def outp(expr op) =
  message "a " & op & " b = " & decimal(a scantokens(op) b) enddef;

outp("+");
outp("-");
outp("*");
outp("div");
outp("mod");

end
```



## min

{{works with|min|0.19.3}}

```min
(concat dup -> ' prepend "$1 -> $2" swap % puts!) :show

("Enter an integer" ask int) 2 times ' prepend
('+ '- '* 'div 'mod) quote-map ('show concat) map cleave
```

{{out}}

```txt

Enter an integer: -3
Enter an integer: 5
(-3 5 +) -> 2
(-3 5 -) -> -8
(-3 5 *) -> -15
(-3 5 div) -> 0
(-3 5 mod) -> -3

```


=={{header|МК-61/52}}==
<lang>П1	<->	П0
+	С/П
ИП0	ИП1	-	С/П
ИП0	ИП1	*	С/П
ИП0	ИП1	/	[x]	С/П
ИП0	^	ИП1	/	[x]	ИП1	*	-	С/П
ИП1	ИП0	x^y	С/П
```



## ML/I

ML/I will read two integers from 'standard input' or similar,
and then output the results to 'standard output' or similar.


```ML/I
MCSKIP "WITH" NL
"" Arithmetic/Integer
"" assumes macros on input stream 1, terminal on stream 2
MCSKIP MT,<>
MCINS %.
MCDEF SL SPACES NL AS <MCSET T1=%A1.
MCSET T2=%A2.
a + b   = %%T1.+%T2..
a - b   = %%T1.-%T2..
a * b   = %%T1.*%T2..
a / b   = %%T1./%T2..
a rem b = %%T1.-%%%T1./%T2..*%T2...
Division is truncated to the greatest integer
that does not exceed the exact result. Remainder matches
the sign of the second operand, if the signs differ.
```


=={{header|Modula-2}}==

```modula2
MODULE ints;

IMPORT  InOut;

VAR     a, b    : INTEGER;

BEGIN
  InOut.WriteString ("Enter two integer numbers : ");   InOut.WriteBf;
  InOut.ReadInt (a);
  InOut.ReadInt (b);
  InOut.WriteString ("a + b   = ");  InOut.WriteInt (a + b, 9);    InOut.WriteLn;
  InOut.WriteString ("a - b   = ");  InOut.WriteInt (a - b, 9);    InOut.WriteLn;
  InOut.WriteString ("a * b   = ");  InOut.WriteInt (a * b, 9);    InOut.WriteLn;
  InOut.WriteString ("a / b   = ");  InOut.WriteInt (a DIV b, 9);  InOut.WriteLn;
  InOut.WriteString ("a MOD b = ");  InOut.WriteInt (a MOD b, 9);  InOut.WriteLn;
  InOut.WriteLn;
END ints.
```
Producing:
```txt
$$ ints
Enter two integer numbers : 12 7
a + b   =        19
a - b   =         5
a * b   =        84
a / b   =         1
a MOD b =         5

$$ ints
Enter two integer numbers : 123 -111
a + b   =        12
a - b   =       234
a * b   =    -13653
a / b   =        -1
a MOD b =        12
```


=={{header|Modula-3}}==

```modula3
MODULE Arith EXPORTS Main;

IMPORT IO, Fmt;

VAR a, b: INTEGER;

BEGIN
  a := IO.GetInt();
  b := IO.GetInt();
  IO.Put("a+b = " & Fmt.Int(a + b) & "\n");
  IO.Put("a-b = " & Fmt.Int(a - b) & "\n");
  IO.Put("a*b = " & Fmt.Int(a * b) & "\n");
  IO.Put("a DIV b = " & Fmt.Int(a DIV b) & "\n");
  IO.Put("a MOD b = " & Fmt.Int(a MOD b) & "\n");
END Arith.
```



## MUMPS

<p>Note: M[UMPS] has an operator called "modulo".
When both operands are positive numbers, "modulo" has a result that looks a lot like "remainder";
however, there is an important difference.</p>

<p>To better understand the intricacies of "modulo" and how it is
different from "remainder", see Donald Knuth's definition (Volume 1 of the "big books"), or
find out the beauty of cyclic algebra as formulated by Niels Henrik Abel (August 5, 1802 – April 6, 1829).</p>


```MUMPS
Arith(first,second)	; Mathematical operators
	Write "Plus",?12,first,"+",second,?25," = ",first+second,!
	Write "Minus",?12,first,"-",second,?25," = ",first-second,!
	Write "Multiply",?12,first,"*",second,?25," = ",first*second,!
	Write "Divide",?12,first,"/",second,?25," = ",first/second,!
	Write "Int Divide",?12,first,"\",second,?25," = ",first\second,!
	Write "Power",?12,first,"**",second,?25," = ",first**second,!
	Write "Modulo",?12,first,"#",second,?25," = ",first#second,!
	Write "And",?12,first,"&",second,?25," = ",first&second,!
	Write "Or",?12,first,"!",second,?25," = ",first!second,!
	Quit

Do Arith(2,3)
Plus        2+3           = 5
Minus       2-3           = -1
Multiply    2*3           = 6
Divide      2/3           = .6666666666666666667
Int Divide  2\3           = 0
Power       2**3          = 8
Modulo      2#3           = 2
And         2&3           = 1
Or          2!3           = 1

Do Arith(16,0.5)
Plus        16+.5         = 16.5
Minus       16-.5         = 15.5
Multiply    16*.5         = 8
Divide      16/.5         = 32
Int Divide  16\.5         = 32
Power       16**.5        = 4
Modulo      16#.5         = 0
And         16&.5         = 1
Or          16!.5         = 1

Do Arith(0,2)
Plus        0+2           = 2
Minus       0-2           = -2
Multiply    0*2           = 0
Divide      0/2           = 0
Int Divide  0\2           = 0
Power       0**2          = 0
Modulo      0#2           = 0
And         0&2           = 0
Or          0!2           = 1
```




## Nemerle

Adapted nearly verbatim from C# solution above. Note that I've used the exponentiation operator (**), but Math.Pow() as used in the C# solution would also work.

```Nemerle
using System;

class Program
{
    static Main(args : array[string]) : void
    {
        def a = Convert.ToInt32(args[0]);
        def b = Convert.ToInt32(args[1]);

        Console.WriteLine("{0} + {1} = {2}", a, b, a + b);
        Console.WriteLine("{0} - {1} = {2}", a, b, a - b);
        Console.WriteLine("{0} * {1} = {2}", a, b, a * b);
        Console.WriteLine("{0} / {1} = {2}", a, b, a / b); // truncates towards 0
        Console.WriteLine("{0} % {1} = {2}", a, b, a % b); // matches sign of first operand
        Console.WriteLine("{0} ** {1} = {2}", a, b, a ** b);
    }
}
```



## NetRexx

{{trans|REXX}}

```NetRexx
/* NetRexx */

options replace format comments java crossref symbols binary

say "enter 2 integer values separated by blanks"
parse ask a b
say a "+" b "=" a + b
say a "-" b "=" a - b
say a "*" b "=" a * b
say a "/" b "=" a % b "remaining" a // b "(sign from first operand)"
say a "^" b "=" a ** b

return

```

{{out}}
<pre style="height: 15ex; overflow:scroll;">
enter 2 integer values separated by blanks
17 -4
17 + -4 = 13
17 - -4 = 21
17 * -4 = -68
17 / -4 = -4 remaining 1 (sign from first operand)
17 ^ -4 = 0.0000119730367

```



## NewLISP



```NewLISP
; integer.lsp
; oofoe 2012-01-17

(define (aski msg) (print msg) (int (read-line)))
(setq x (aski "Please type in an integer and press [enter]: "))
(setq y (aski "Please type in another integer             : "))

; Note that +, -, *, / and % are all integer operations.
(println)
(println "Sum: " (+ x y))
(println "Difference: " (- x y))
(println "Product: " (* x y))
(println "Integer quotient (rounds to 0): " (/ x y))
(println "Remainder: " (setq r (% x y)))

(println "Remainder sign matches: "
	 (cond ((= (sgn r) (sgn x) (sgn y)) "both")
	       ((= (sgn r) (sgn x))         "first")
	       ((= (sgn r) (sgn y))         "second")))

(println)
(println "Exponentiation: " (pow x y))

(exit) ; NewLisp normally goes to listener after running script.

```


{{out}}

```txt

Please type in an integer and press [enter]: 17
Please type in another integer             : -4

Sum: 13
Difference: 21
Product: -68
Integer quotient (rounds to 0): -4
Remainder: 1
Remainder sign matches: first

Exponentiation: 1.197303672e-005

```



## Nial


Example tested with Q'Nial7.

Define new operator using an atlas of operators:

```nial
     arithmetic is OP A B{[first,last,+,-,*,quotient,mod,power] A B}
```


Test new operator:

```nial
     -23 arithmetic 7
-23 7 -16 -30 -161 -4 5 -3404825447
```


Negative divisors are not accepted for integer quotient <code>quotient</code> or remainder <code>mod</code>, and in both cases the result is an error with the message <code>?negative divisor</code>.

For <code>quotient</code>, if the divisor <code>B</code> is zero, the result is zero.

For <code>mod</code>, if the divisor <code>B</code> is zero, the result is <code>A</code>.

The quotient on division by a positive integer <code>B</code> is always an integer on the same side of the origin as <code>A</code>.

Nial definition of <code>quotient</code>:


```nial
A quotient B =f=  floor (A / B)
```


<code>floor</code> rounds towards negative infinity (next lower integer).


## Nim


```nim


import parseopt, strutils

var
  opt: OptParser = initOptParser()
  str = opt.cmdLineRest.split
  a: int = 0
  b: int = 0

try:
  a = parseInt(str[0])
  b = parseInt(str[1])
except ValueError:
  quit("Invalid params. Two integers are expected.")


echo("a      : " & $a)
echo("b      : " & $b)
echo("a + b  : " & $(a+b))
echo("a - b  : " & $(a-b))
echo("a * b  : " & $(a*b))
echo("a div b: " & $(a div b))
echo("a mod b: " & $(a mod b))

```

Execute: Aritmint 10 23
/
{{out}}

```txt

a      : 10
b      : 23
a + b  : 33
a - b  : -13
a * b  : 230
a div b: 0
a mod b: 10

```



## NSIS

All Arithmetic in NSIS is handled by the [http://nsis.sourceforge.net/Docs/Chapter4.html#4.9.10.2 IntOp] instruction.  It is beyond the scope of this task to implement user input (a fairly involved task), so I will be providing hard-coded values simulating the user input, with the intention of later adding the user-input piece.

```nsis
Function Arithmetic
	Push $0
	Push $1
	Push $2
	StrCpy $0 21
	StrCpy $1 -2

	IntOp $2 $0 + $1
	DetailPrint "$0 + $1 = $2"
	IntOp $2 $0 - $1
	DetailPrint "$0 - $1 = $2"
	IntOp $2 $0 * $1
	DetailPrint "$0 * $1 = $2"
	IntOp $2 $0 / $1
	DetailPrint "$0 / $1 = $2"
	DetailPrint "Rounding is toward negative infinity"
	IntOp $2 $0 % $1
	DetailPrint "$0 % $1 = $2"
	DetailPrint "Sign of remainder matches the first number"

	Pop $2
	Pop $1
	Pop $0
FunctionEnd
```


=={{header|Oberon-2}}==
Oxford Oberon-2

```oberon2

MODULE Arithmetic;
IMPORT In, Out;
VAR
        x,y:INTEGER;
BEGIN
        Out.String("Give two numbers: ");In.Int(x);In.Int(y);
        Out.String("x + y >");Out.Int(x + y,6);Out.Ln;
        Out.String("x - y >");Out.Int(x - y,6);Out.Ln;
        Out.String("x * y >");Out.Int(x * y,6);Out.Ln;
        Out.String("x / y >");Out.Int(x DIV y,6);Out.Ln;
        Out.String("x MOD y >");Out.Int(x MOD y,6);Out.Ln;
END Arithmetic.

```

{{out}}

```txt

Give two numbers: 12 23
x + y >    35
x - y >   -11
x * y >   276
x / y >     0
x MOD y >    12

```


## Objeck


```objeck
bundle Default {
  class Arithmetic {
    function : Main(args : System.String[]) ~ Nil {
      DoArithmetic();
    }

    function : native : DoArithmetic() ~ Nil {
      a := IO.Console->GetInstance()->ReadString()->ToInt();
      b := IO.Console->GetInstance()->ReadString()->ToInt();

      IO.Console->GetInstance()->Print("a+b = ")->PrintLine(a+b);
      IO.Console->GetInstance()->Print("a-b = ")->PrintLine(a-b);
      IO.Console->GetInstance()->Print("a*b = ")->PrintLine(a*b);
      IO.Console->GetInstance()->Print("a/b = ")->PrintLine(a/b);
    }
  }
}
```



## OCaml


```ocaml
let _ =
  let a = read_int ()
  and b = read_int () in

  Printf.printf "a + b = %d\n" (a + b);
  Printf.printf "a - b = %d\n" (a - b);
  Printf.printf "a * b = %d\n" (a * b);
  Printf.printf "a / b = %d\n" (a / b);    (* truncates towards 0 *)
  Printf.printf "a mod b = %d\n" (a mod b) (* same sign as first operand *)
```



## Oforth



```Oforth
: integers (a b -- )
   "a + b ="   . a b + .cr
   "a - b ="   . a b - .cr
   "a * b ="   . a b * .cr
   "a / b ="   . a b / .cr
   "a mod b =" . a b mod .cr
   "a pow b =" . a b pow .cr
;
```


{{out}}

```txt

>12 23 integers
a + b = 35
a - b = -11
a * b = 276
a / b = 0
a mod b = 12
a pow b = 6624737266949237011120128
ok

```



## Ol



```scheme

(define a 8)
(define b 12)

(print "(+ " a " " b ") => "    (+ a b))
(print "(- " a " " b ") => "    (- a b))
(print "(* " a " " b ") => "    (* a b))
(print "(/ " a " " b ") => "    (/ a b))

(print "(quotient " a " " b ") => "  (quot a b)) ; same as (quotient a b)
(print "(remainder " a " " b ") => " (rem  a b)) ; same as (remainder a b)
(print "(modulo " a " " b ") => "    (mod  a b)) ; same as (modulo a b)

(import (owl math-extra))
(print "(expt " a " " b ") => " (expt a b))

```

{{out}}

```txt

(+ 8 12) => 20
(- 8 12) => -4
(* 8 12) => 96
(/ 8 12) => 2/3
(quotient 8 12) => 0
(remainder 8 12) => 8
(modulo 8 12) => 8
(expt 8 12) => 68719476736

```


Additional features:

```ol

; you can use more than two arguments for +,-,*,/ functions
(print (+ 1 3 5 7 9)) ; ==> 25
(print (- 1 3 5 7 9)) ; ==> -23
(print (* 1 3 5 7 9)) ; ==> 945 - same as (1*3*5*7*9)
(print (/ 1 3 5 7 9)) ; ==> 1/945 - same as (((1/3)/5)/7)/9

```



## Onyx



```onyx
# Most of this long script is mere presentation.
# All you really need to do is push two integers onto the stack
# and then execute add, sub, mul, idiv, or pow.

$ClearScreen { # Using ANSI terminal control
  `\e[2J\e[1;1H' print flush
} bind def

$Say { # string Say -
  `\n' cat print flush
} bind def

$ShowPreamble {
`To show how integer arithmetic in done in Onyx,' Say
`we\'ll use two numbers of your choice, which' Say
`we\'ll call A and B.\n' Say
} bind def

$Prompt { # stack: string --
  stdout exch write pop flush
} def

$GetInt { # stack: name -- integer
  dup cvs `Enter integer ' exch cat `: ' cat
  Prompt stdin readline pop cvx eval def
} bind def

$Template { # arithmetic_operator_name label_string Template result_string
  A cvs ` ' B cvs ` ' 5 ncat over cvs ` gives ' 3 ncat exch
  A B dn cvx eval cvs `.' 3 ncat Say
} bind def

$ShowResults {
  $add `Addition: ' Template
  $sub `Subtraction: ' Template
  $mul `Multiplication: ' Template
  $idiv `Division: ' Template
  `Note that the result of integer division is rounded toward zero.' Say
  $pow `Exponentiation: ' Template
  `Note that the result of raising to a negative power always gives a real number.' Say
} bind def

ClearScreen ShowPreamble $A GetInt $B GetInt ShowResults
```


{{out}}

```txt

To show how integer arithmetic in done in Onyx,
we'll use two numbers of your choice, which
we'll call A and B.

Enter integer A: 34
Enter integer B: 2
Addition: 34 2 add gives 36.
Subtraction: 34 2 sub gives 32.
Multiplication: 34 2 mul gives 68.
Division: 34 2 idiv gives 17.
Note that the result of integer division is rounded toward zero.
Exponentiation: 34 2 pow gives 1156.
Note that the result of raising to a negative power always gives a real number.

```



## Openscad



```openscad
echo (a+b);  /* Sum */
echo (a-b);  /* Difference */
echo (a*b);  /* Product */
echo (a/b);  /* Quotient */
echo (a%b);  /* Modulus */
```



## Oz


```oz
declare
  StdIn = {New class $ from Open.file Open.text end init(name:stdin)}

  fun {ReadInt}
     {String.toInt {StdIn getS($)}}
  end

  A = {ReadInt}
  B = {ReadInt}
in
  {ForAll
   ["A+B = "#A+B
    "A-B = "#A-B
    "A*B = "#A*B
    "A/B = "#A div B  %% truncates towards 0
    "remainder "#A mod B  %% has the same sign as A
    "A^B = "#{Pow A B}
   ]
   System.showInfo}
```



## PARI/GP

Integer division with <code>\</code> rounds to <math>-\infty</math>. There also exists the <code>\/</code> round-to-nearest (ties to <math>+\infty</math>) operator.  Ordinary division <code>/</code> does not round but returns rationals if given integers with a non-integral quotient.

```parigp
arith(a,b)={
  print(a+b);
  print(a-b);
  print(a*b);
  print(a\b);
  print(a%b);
  print(a^b);
};
```



## Panda

Use reflection to get all functions defined on numbers taking number and returning number.

```panda
a=3 b=7 func:_bbf__number_number_number =>f.name.<b> '(' a b ')' ' => ' f(a b) nl
```


{{out}}

```txt
atan2 ( 3 7 ) => 0.40489178628508343
divide ( 3 7 ) => 0.42857142857142855
gt ( 3 7 ) => UNDEFINED!
gte ( 3 7 ) => UNDEFINED!
lt ( 3 7 ) => 3
lte ( 3 7 ) => 3
max ( 3 7 ) => 7
min ( 3 7 ) => 3
minus ( 3 7 ) => -4
mod ( 3 7 ) => 3
plus ( 3 7 ) => 10
pow ( 3 7 ) => 2187
```



## Pascal


```pascal
program arithmetic(input, output)

var
 a, b: integer;

begin
 readln(a, b);
 writeln('a+b = ', a+b);
 writeln('a-b = ', a-b);
 writeln('a*b = ', a*b);
 writeln('a/b = ', a div b, ', remainder ', a mod b);
end.
```



## Perl

{{works with|Perl|5.x}}

```perl
my $a = <>;
my $b = <>;

print
    "sum:              ", $a + $b, "\n",
    "difference:       ", $a - $b, "\n",
    "product:          ", $a * $b, "\n",
    "integer quotient: ", int($a / $b), "\n",
    "remainder:        ", $a % $b, "\n",
    "exponent:         ", $a ** $b, "\n"
    ;
```



## Perl 6

{{works with|Rakudo|2015.09}}

```perl6
my Int $a = get.floor;
my Int $b = get.floor;

say 'sum:              ', $a + $b;
say 'difference:       ', $a - $b;
say 'product:          ', $a * $b;
say 'integer quotient: ', $a div $b;
say 'remainder:        ', $a % $b;
say 'exponentiation:   ', $a**$b;
```


Note that <code>div</code> doesn't always do integer division; it performs the operation "most appropriate to the
operand types". [http://perlcabal.org/syn/S03.html#line_729 Synopsis 3] guarantees that <code>div</code> "on built-in integer types is equivalent to taking the floor of a real division". If you want integer division with other types, say <code>floor($a/$b)</code>.


## Phix


```Phix
integer a = floor(prompt_number("a = ",{}))
integer b = floor(prompt_number("b = ",{}))

printf(1,"a + b = %d\n", a+b)
printf(1,"a - b = %d\n", a-b)
printf(1,"a * b = %d\n", a*b)
printf(1,"a / b = %g\n", a/b) -- does not truncate
printf(1,"remainder(a,b) = %d\n", remainder(a,b)) -- same sign as first operand
printf(1,"power(a,b) = %g\n", power(a,b))
```

{{out}}

```txt

a = 2
b = 3
a + b = 5
a - b = -1
a * b = 6
a / b = 0.666667
remainder(a,b) = 2
power(a,b) = 8

```



## PHL



```phl
module arith;

extern printf;
extern scanf;

@Integer main [
	@Pointer<@Integer> a = alloc(4);
	@Pointer<@Integer> b = alloc(4);
	scanf("%i %i", a, b);

	printf("a + b = %i\n", a::get + b::get);
	printf("a - b = %i\n", a::get - b::get);
	printf("a * b = %i\n", a::get * b::get);
	printf("a / b = %i\n", a::get / b::get);
	printf("a % b = %i\n", a::get % b::get);
	printf("a ** b = %i\n", a::get ** b::get);

	return 0;
]
```



## PHP


```php
<?php
$a = fgets(STDIN);
$b = fgets(STDIN);

echo
    "sum:                 ", $a + $b, "\n",
    "difference:          ", $a - $b, "\n",
    "product:             ", $a * $b, "\n",
    "truncating quotient: ", (int)($a / $b), "\n",
    "flooring quotient:   ", floor($a / $b), "\n",
    "remainder:           ", $a % $b, "\n",
    "power:               ", $a ** $b, "\n"; // PHP 5.6+ only
?>
```



## PicoLisp


```PicoLisp
(de math (A B)
   (prinl "Add      " (+ A B))
   (prinl "Subtract " (- A B))
   (prinl "Multiply " (* A B))
   (prinl "Divide   " (/ A B))        # Trucates towards zero
   (prinl "Div/rnd  " (*/ A B))       # Rounds to next integer
   (prinl "Modulus  " (% A B))        # Sign of the first operand
   (prinl "Power    " (** A B)) )
```


## Piet

[[File:PietArithmaticInteger.png]]

 <code>
 command   stack
 in(int)   A
 duplicate AA
 duplicate AAA
 duplicate AAAA
 duplicate AAAAA
 in(int)   BAAAAA
 duplicate BBAAAAA
 duplicate BBBAAAAA
 duplicate BBBBAAAAA
 duplicate BBBBBAAAAA
 push 9    9BBBBBAAAAA
 push 1    19BBBBBAAAAA
 roll      BBBBAAAABA
 push 7    7BBBBAAAABA
 push 1    17BBBBAAAABA
 roll      BBBAAABABA
 push 5    5BBBAAABABA
 push 1    15BBBAAABABA
 roll      BBAABABABA
 push 3    3BBAABABABA
 push 1    13BBAABABABA
 roll      BABABABABA
 add       (A+B)BABABABA
 out(int)  BABABABA
 sub       (A-B)BABABA
 out(int)  BABABA
 mult      (A*B)BABA
 out(int)  BABA
 divide    (A/B)BA
 out(int)  BA
 mod       (A%B)
 out(int)  NULL
 push 1    1
 exit</code>
How rounding is handled is up to the interpreter, but I believe the intent was round towards 0.


## PL/I


```PL/I

get list (a, b);
put skip list (a+b);
put skip list (a-b);
put skip list (a*b);
put skip list (trunc(a/b)); /* truncates towards zero.       */
put skip list (mod(a, b));  /* Remainder is always positive. */
put skip list (rem(a, b));  /* Sign can be negative.         */
```



## Pop11



```pop11
;;; Setup token reader
vars itemrep;
incharitem(charin) -> itemrep;
;;; read the numbers
lvars a = itemrep(), b = itemrep();
;;; Print results
printf(a + b, 'a + b = %p\n');
printf(a - b, 'a - b = %p\n');
printf(a * b, 'a * b = %p\n');
printf(a div b, 'a div b = %p\n');
printf(a mod b, 'a mod b = %p\n');
```



## PostScript


```ps
/arithInteger {
   /x exch def
   /y exch def
   x y add =
   x y sub =
   x y mul =
   x y idiv =
   x y mod =
   x y exp =
} def
```


## PowerShell


```powershell
$a = [int] (Read-Host First Number)
$b = [int] (Read-Host Second Number)

Write-Host "Sum:                              $($a + $b)"
Write-Host "Difference:                       $($a - $b)"
Write-Host "Product:                          $($a * $b)"
Write-Host "Quotient:                         $($a / $b)"
Write-Host "Quotient, round to even:          $([Math]::Round($a / $b))"
Write-Host "Remainder, sign follows first:    $($a % $b)"
```

Numbers are automatically converted to accomodate for the result. This means not only that Int32 will be expanded to Int64 but also that a non-integer quotient will cause the result to be of a floating-point type.

The remainder has the sign of the first operand.

No exponentiation operator exists, but can be worked around with the .NET BCL:

```powershell
[Math]::Pow($a, $b)
```


## ProDOS


```ProDOS
IGNORELINE Note: This example includes the math module.
include arithmeticmodule
:a
editvar /newvar /value=a /title=Enter first integer:
editvar /newvar /value=b /title=Enter second integer:
editvar /newvar /value=c
do add -a-,-b-=-c-
printline -c-
do subtract a,b
printline -c-
do multiply a,b
printline -c-
do divide a,b
printline -c-
do modulus a,b
printline -c-
editvar /newvar /value=d /title=Do you want to calculate more numbers?
if -d- /hasvalue yes goto :a else goto :end
:end
```



```ProDOS
IGNORELINE Note: This example does not use the math module.
:a
editvar /newvar /value=a /title=Enter first integer:
editvar /newvar /value=b /title=Enter second integer:
editvar /newvar /value=-a-+-b-=-c-
printline -c-
editvar /newvar /value=a*b=c
printline -c-
editvar /newvar /value=a/b=c
printline -c-
editvar /newvar /value=a %% b=c
printline -c-
editvar /newvar /value=d /title=Do you want to calculate more numbers?
if -d- /hasvalue yes goto :a else goto :end
:end
```



## Prolog


Integer quotient (`//`) rounds towards 0.

Remainder (`rem`) matches the sign of its first operand.


```prolog


print_expression_and_result(M, N, Operator) :-
    Expression =.. [Operator, M, N],
    Result is Expression,
    format('~w ~8|is ~d~n', [Expression, Result]).

arithmetic_integer :-
    read(M),
    read(N),
    maplist( print_expression_and_result(M, N), [+,-,*,//,rem,^] ).


```


Use thus:


```prolog

?- arithmetic_integer.
|: 5.
|: 7.
5+7     is 12
5-7     is -2
5*7     is 35
5//7    is 0
5 rem 7 is 5
5^7     is 78125
true.

```



## PureBasic


```purebasic
OpenConsole()

Define a, b

Print("Number 1: "): a = Val(Input())
Print("Number 2: "): b = Val(Input())

PrintN("Sum:        " + Str(a + b))
PrintN("Difference: " + Str(a - b))
PrintN("Product:    " + Str(a * b))
PrintN("Quotient:   " + Str(a / b)) ; Integer division (rounding mode=truncate)
PrintN("Remainder: " + Str(a % b))
PrintN("Power:      " + Str(Pow(a, b)))

Input()

CloseConsole()
```



## Python



```python
x = int(raw_input("Number 1: "))
y = int(raw_input("Number 2: "))

print "Sum: %d" % (x + y)
print "Difference: %d" % (x - y)
print "Product: %d" % (x * y)
print "Quotient: %d" % (x / y)     #  or x // y  for newer python versions.
                                   # truncates towards negative infinity
print "Remainder: %d" % (x % y)    # same sign as second operand
print "Quotient: %d with Remainder: %d" % divmod(x, y)
print "Power: %d" % x**y

## Only used to keep the display up when the program ends
raw_input( )
```


Notes: In Python3 ''raw_input()'' will be renamed to ''input()'' (the old ''input()'' built-in will go away, though one could use ''eval(input())'' to emulate the old ... and ill-advised ... behavior).  Also a better program would wrap the attempted ''int()'' conversions in a ''try: ... except ValueError:...'' construct such as:


```python
def getnum(prompt):
    while True: # retrying ...
        try:
            n = int(raw_input(prompt))
        except ValueError:
            print "Input could not be parsed as an integer. Please try again."\
            continue
        break
    return n

x = getnum("Number1: ")
y = getnum("Number2: ")
...
```


(In general it's good practice to perform parsing of all input in exception handling blocks.  This is especially true of interactive user input, but also applies to data read from configuration and other files, and marshaled from other processes via any IPC mechanism).

Python also has the procedure ''divmod'' that returns both quotient and remainder. eg
 quotient, remainder = divmod(355,113)
Giving a quotient of 3, and a remainder of 16.


###  Python 3.0 compatible code


```python
def arithmetic(x, y):
    for op in "+ - * // % **".split():
        expr = "%(x)s %(op)s %(y)s" % vars()
        print("%s\t=> %s" % (expr, eval(expr)))


arithmetic(12, 8)
arithmetic(input("Number 1: "), input("Number 2: "))
```

{{out}}

```txt
12 + 8  => 20
12 - 8  => 4
12 * 8  => 96
12 // 8 => 1
12 % 8  => 4
12 ** 8	=> 429981696
Number 1: 20
Number 2: 4
20 + 4  => 24
20 - 4  => 16
20 * 4  => 80
20 // 4 => 5
20 % 4  => 0
20 ** 4 => 160000
```



## R


```R
cat("insert number ")
a <- scan(nmax=1, quiet=TRUE)
cat("insert number ")
b <- scan(nmax=1, quiet=TRUE)
print(paste('a+b=', a+b))
print(paste('a-b=', a-b))
print(paste('a*b=', a*b))
print(paste('a%/%b=', a%/%b))
print(paste('a%%b=', a%%b))
print(paste('a^b=', a^b))

```



## Racket


```racket

#lang racket/base

(define (arithmetic x y)
  (for ([op (list + - * / quotient remainder modulo max min gcd lcm)])
    (printf "~s => ~s\n" `(,(object-name op) ,x ,y) (op x y))))

(arithmetic 8 12)

```

{{out}}

```txt

(+ 8 12) => 20
(- 8 12) => -4
(* 8 12) => 96
(/ 8 12) => 2/3
(quotient 8 12) => 0
(remainder 8 12) => 8
(modulo 8 12) => 8
(max 8 12) => 12
(min 8 12) => 8
(gcd 8 12) => 4
(lcm 8 12) => 24

```



## Raven



```raven
'  Number 1: ' print expect 0 prefer as x
'  Number 2: ' print expect 0 prefer as y

x y + "       sum: %d\n" print
x y - "difference: %d\n" print
x y * "   product: %d\n" print
x y / "  quotient: %d\n" print
x y % " remainder: %d\n" print
```



## REBOL


```rebol
REBOL [
	Title: "Integer"
	URL: http://rosettacode.org/wiki/Arithmetic/Integer
]

x: to-integer ask "Please type in an integer, and press [enter]: "
y: to-integer ask "Please enter another integer: "
print ""

print ["Sum:"  x + y]
print ["Difference:"  x - y]
print ["Product:" x * y]

print ["Integer quotient (coercion)                          :"  to-integer x / y]
print ["Integer quotient (away from zero)                    :"  round x / y]
print ["Integer quotient (halves round towards even digits)  :"  round/even x / y]
print ["Integer quotient (halves round towards zero)         :"  round/half-down x / y]
print ["Integer quotient (round in negative direction)       :"  round/floor x / y]
print ["Integer quotient (round in positive direction)       :"  round/ceiling x / y]
print ["Integer quotient (halves round in positive direction):"  round/half-ceiling x / y]

print ["Remainder:"  r: x // y]

; REBOL evaluates infix expressions from left to right. There are no
; precedence rules -- whatever is first gets evaluated. Therefore when
; performing this comparison, I put parens around the first term
; ("sign? a") of the expression so that the value of /a/ isn't
; compared to the sign of /b/. To make up for it, notice that I don't
; have to use a specific return keyword. The final value in the
; function is returned automatically.

match?: func [a b][(sign? a) = sign? b]

result: copy []
if match? r x [append result "first"]
if match? r y [append result "second"]

; You can evaluate arbitrary expressions in the middle of a print, so
; I use a "switch" to provide a more readable result based on the
; length of the /results/ list.

print [
	"Remainder sign matches:"
	switch length? result [
		0 ["neither"]
		1 [result/1]
		2 ["both"]
	]
]

print ["Exponentiation:" x ** y]
```


{{out}}

```txt
Please type in an integer, and press [enter]: 17
Please enter another integer: -4

Sum: 13
Difference: 21
Product: -68
Integer quotient (coercion)                          : -4
Integer quotient (away from zero)                    : -4
Integer quotient (halves round towards even digits)  : -4
Integer quotient (halves round towards zero)         : -4
Integer quotient (round in negative direction)       : -5
Integer quotient (round in positive direction)       : -4
Integer quotient (halves round in positive direction): -4
Remainder: 1
Remainder sign matches: first
Exponentiation: 1.19730367213036E-5
```



## Retro

Retro's arithmetic functions are based on those in [[Forth]]. The example is an adaption of the one from Forth.


```Retro
: arithmetic ( ab- )
  over    "\na       = %d" puts
  dup     "\nb       = %d" puts
  2over + "\na + b   = %d" puts
  2over - "\na - b   = %d" puts
  2over * "\na * b   = %d" puts
  /mod    "\na / b   = %d" puts
          "\na mod b = %d\n" puts ;
```



## REXX

All operators automatically produce integers where appropriate   (up to twenty decimal digits in the program below),

or numbers in exponential format when necessary.   (The REXX default is nine decimal digits.)

For division that produces a floating point number, the result is rounded to the nearest number that can be expressed

within the current number of decimal digits   (in the example program below, it is twenty decimal digits).

```rexx
/*REXX program obtains two integers from the C.L. (a prompt);  displays some operations.*/
numeric digits 20                                /*#s are round at 20th significant dig.*/
parse arg x y .                                  /*maybe the integers are on the  C.L.  */

  do while \datatype(x,'W') | \datatype(y,'W')   /*both   X  and  Y   must be integers. */
  say "─────Enter two integer values  (separated by blanks):"
  parse pull x y .                               /*accept two thingys from command line.*/
  end   /*while*/
                                                 /* [↓]  perform this  DO  loop twice.  */
     do j=1  for 2                               /*show  A  oper  B,   then  B  oper  A.*/
     call show 'addition'          ,  "+",   x+y
     call show 'subtraction'       ,  "-",   x-y
     call show 'multiplication'    ,  "*",   x*y
     call show 'int  division'     ,  "%",   x%y,  '    [rounds down]'
     call show 'real division'     ,  "/",   x/y
     call show 'division remainder',  "//",  x//y, '    [sign from 1st operand]'
     call show 'power'             ,  "**",  x**y

     parse value  x  y    with    y  x           /*swap the two values and perform again*/
     if j==1  then say copies('═', 79)           /*display a fence after the 1st round. */
     end   /*j*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
show: parse arg c,o,#,?;   say right(c,25)' '  x  center(o,4)  y  " ───► "  #  ?;   return
```

'''output''' when using the input of:   <tt> 17   -4 </tt>

```txt

                 addition  4  +   -17  ───►  -13
              subtraction  4  -   -17  ───►  21
           multiplication  4  *   -17  ───►  -68
            int  division  4  %   -17  ───►  0     [rounds down]
            real division  4  /   -17  ───►  -0.23529411764705882353
       division remainder  4  //  -17  ───►  4     [sign from 1st operand]
                    power  4  **  -17  ───►  5.8207660913467407227E-11
═══════════════════════════════════════════════════════════════════════════════
                 addition  -17  +   4  ───►  -13
              subtraction  -17  -   4  ───►  -21
           multiplication  -17  *   4  ───►  -68
            int  division  -17  %   4  ───►  -4     [rounds down]
            real division  -17  /   4  ───►  -4.25
       division remainder  -17  //  4  ───►  -1     [sign from 1st operand]
                    power  -17  **  4  ───►  83521

```



## Ring


```ring

func Test a,b
   see "a+b" + ( a + b ) + nl
   see "a-b" + ( a - b ) + nl
   see "a*b" + ( a * b ) + nl
   // The quotient isn't integer, so we use the Ceil() function, which truncates it downward.
   see "a/b" + Ceil( a / b ) + nl
   // Remainder:
   see "a%b" + ( a % b ) + nl
   see "a**b" +  pow(a,b )  + nl

```



## Robotic


```robotic

input string "Enter number 1:"
set "a" to "input"
input string "Enter number 2:"
set "b" to "input"

[ "Sum: ('a' + 'b')"
[ "Difference: ('a' - 'b')"
[ "Product: ('a' * 'b')"
[ "Integer Quotient: ('a' / 'b')"
[ "Remainder: ('a' % 'b')"
[ "Exponentiation: ('a'^'b')"

```



## Ruby



```ruby
puts 'Enter x and y'
x = gets.to_i  # to check errors, use x=Integer(gets)
y = gets.to_i

puts "Sum: #{x+y}",
     "Difference: #{x-y}",
     "Product: #{x*y}",
     "Quotient: #{x/y}",       # truncates towards negative infinity
     "Quotient: #{x.fdiv(y)}", # float
     "Remainder: #{x%y}",      # same sign as second operand
     "Exponentiation: #{x**y}"
```



## Run BASIC


```runbasic
input "1st integer: "; i1
input "2nd integer: "; i2

print "      Sum"; i1 + i2
print "     Diff"; i1 - i2
print "  Product"; i1 * i2
if i2 <>0 then print "  Quotent "; int( i1 / i2); else print "Cannot divide by zero."
print "Remainder"; i1 MOD i2
print "1st raised to power of 2nd"; i1 ^ i2
```



## Rust


Note that this code cannot be run within the [http://play.rust-lang.org Rust playpen] as it does not support console input.

```rust
use std::env;

fn main() {
    let args: Vec<_> = env::args().collect();
    let a = args[1].parse::<i32>().unwrap();
    let b = args[2].parse::<i32>().unwrap();

    println!("sum:              {}", a + b);
    println!("difference:       {}", a - b);
    println!("product:          {}", a * b);
    println!("integer quotient: {}", a / b); // truncates towards zero
    println!("remainder:        {}", a % b); // same sign as first operand
}
```



## Sass/SCSS



```coffeescript

@function arithmetic($a,$b) {
	@return $a + $b, $a - $b, $a * $b, ($a - ($a % $b))/$b, $a % $b;
}

```

Which you use with:

```coffeescript

nth(arithmetic(10,3),1);

```

Or each of the functions separately:

```coffeescript

@function sum($a,$b) {
	@return $a + $b;
}

@function difference($a,$b) {
	@return $a - $b;
}

@function product($a,$b) {
	@return $a * $b;
}

@function integer-division($a,$b) {
	@return ($a - ($a % $b))/$b;
}

@function remainder($a,$b) {
	@return $a % $b;
}

@function float-division($a,$b) {
	@return $a / $b;
}

```



## Scala


```scala
val a = Console.readInt
val b = Console.readInt

val sum = a + b //integer addition is discouraged in print statements due to confusion with String concatenation
println("a + b = " + sum)
println("a - b = " + (a - b))
println("a * b = " + (a * b))
println("quotient of a / b = " + (a / b)) // truncates towards 0
println("remainder of a / b = " + (a % b)) // same sign as first operand
```



## Scheme



```scheme
(define (arithmetic x y)
  (for-each (lambda (op)
              (write  (list op x y))
              (display " => ")
              (write ((eval op) x y))
              (newline))
            '(+ - * / quotient remainder modulo max min gcd lcm)))

(arithmetic 8 12)
```

quotient - truncates towards 0
remainder - same sign as first operand
modulo - same sign as second operand

  prints this:

 (+ 8 12) => 20
 (- 8 12) => -4
 (* 8 12) => 96
 (/ 8 12) => 2/3
 (quotient 8 12) => 0
 (remainder 8 12) => 8
 (modulo 8 12) => 8
 (max 8 12) => 12
 (min 8 12) => 8
 (gcd 8 12) => 4
 (lcm 8 12) => 24


## Seed7


```seed7
$ include "seed7_05.s7i";

const proc: main is func
  local
    var integer: a is 0;
    var integer: b is 0;
  begin
    write("a = ");
    readln(a);
    write("b = ");
    readln(b);

    writeln("a + b = " <& a + b);
    writeln("a - b = " <& a - b);
    writeln("a * b = " <& a * b);
    writeln("a div b = " <& a div b);    # Rounds towards zero
    writeln("a rem b = " <& a rem b);    # Sign of the first operand
    writeln("a mdiv b = " <& a mdiv b);  # Rounds towards negative infinity
    writeln("a mod b = " <& a mod b);    # Sign of the second operand
  end func;
```



## Sidef


```ruby
var a = Sys.scanln("First number: ").to_i;
var b = Sys.scanln("Second number: ").to_i;

%w'+ - * // % ** ^ | & << >>'.each { |op|
    "#{a} #{op} #{b} = #{a.$op(b)}".say;
}
```


{{out}}

```txt

First number: 1234
Second number: 7
1234 + 7 = 1241
1234 - 7 = 1227
1234 * 7 = 8638
1234 // 7 = 176
1234 % 7 = 2
1234 ** 7 = 4357186184021382204544
1234 ^ 7 = 1237
1234 | 7 = 1239
1234 & 7 = 2
1234 << 7 = 157952
1234 >> 7 = 9

```



## Slate


```slate
[| :a :b |
inform: (a + b) printString.
inform: (a - b) printString.
inform: (a * b) printString.
inform: (a / b) printString.
inform: (a // b) printString.
inform: (a \\ b) printString.

] applyTo: {Integer readFrom: (query: 'Enter a: '). Integer readFrom: (query: 'Enter b: ')}.
```



## Smalltalk

{{works with|GNU Smalltalk}}

```smalltalk
| a b |
'Input number a: ' display.
a := (stdin nextLine) asInteger.
'Input number b: ' display.
b := (stdin nextLine) asInteger.
('a+b=%1' % { a + b }) displayNl.
('a-b=%1' % { a - b }) displayNl.
('a*b=%1' % { a * b }) displayNl.
('a/b=%1' % { a // b }) displayNl.
('a%%b=%1' % { a \\ b }) displayNl.
```



## smart BASIC


```qbasic
INPUT "Enter first number.":first
INPUT "Enter second number.":second
PRINT "The sum of";first;"and";second;"is ";first+second&"."
PRINT "The difference between";first;"and";second;"is ";ABS(first-second)&"."
PRINT "The product of";first;"and";second;"is ";first*second&"."
IF second THEN
    PRINT "The integer quotient of";first;"and";second;"is ";INTEG(first/second)&"."
ELSE
    PRINT "Division by zero not cool."
ENDIF
PRINT "The remainder being...";first%second&"."
PRINT STR$(first);"raised to the power of";second;"is ";first^second&"."
```


'''NOTES:''' Some curious aspects of smart BASIC to note in this code example:
<ol>
<li>In smart BASIC, The command INTEG is a true integer function providing only the value of the characteristic. The smart BASIC INT command calculates as a rounding function. This differs from some other versions of BASIC.</li>
<li>smart BASIC automatically inserts spaces ahead of and behind numbers. This can cause unexpected formatting issues when combining output from numeric variables with text. In order to suppress the trailing space, you must use the ampersand (&) to concatenate the numeric value with the following text (in this case, a period at the end of each sentence). In the case of leading spaces, you must convert the numeric value to text using the STR$ command (as with the last line of the code).
</ol>


## SNOBOL4


```snobol4

        output = "Enter first integer:"
  	first = input
	output = "Enter second integer:"
	second = input
	output = "sum  = " first + second
	output = "diff = " first - second
	output = "prod = " first * second
	output = "quot = " (qout = first / second)
	output = "rem  = " first - (qout * second)
end
```



## SNUSP

As a BF derivative, SNUSP only has increment and decrement as native operations. Here are routines for other basic arithmetic upon single digit numbers and results.

''See also: [[Ethiopian Multiplication]]''

```SNUSP
$\
 ,
 @
 \=@@@-@-----#  atoi
 >
 ,
 @
 \=@@@-@-----#
 <
 @     #        4 copies
 \=!/?!/->>+>>+>>+>>+<<<<<<<<?\#
 >  | #\?<<<<<<<<+>>+>>+>>+>>-/
 @  |
 \==/
 \>>>>\
 />>>>/
 @
 \==!/===?\#    add
 <   \>+<-/
 @
 \=@@@+@+++++#  itoa
 .
 <
 @
 \==!/===?\#    subtract
 <   \>-<-/
 @
 \=@@@+@+++++#
 .
 !
 /\
 ?-             multiply
 \/ #/?<<+>+>-==\     /==-<+<+>>?\#    /==-<<+>>?\#
 <   \->+>+<<!/?/#   #\?\!>>+<+<-/    #\?\!>>+<<-/
 @         /==|
### ===
|=====\   /-\    |
 \======<?!/>@/<-?!\>>>@/<<<-?\=>!\?/>!/@/<#
 <         \
### =
|
### ====
/   /-\  |
 @                 \done======>>>!\?/<=/
 \=@@@+@+++++#
 .
 !
 /\
 ?-  zero
 \/
 <              divmod
 @    /-\
 \?\<!\?/#!===+<<<\      /-\
 | \<==@\>@\>>!/?!/=<?\>!\?/<<#
 |      |  |  #\->->+</
 |      \=!\=?!/->>+<<?\#
 @            #\?<<+>>-/
 \=@@@+@+++++#
 .
 <
 @
 \=@@@+@+++++#
 .
 #
```



## SQL

{{works with|Oracle}}

```sql

-- test.sql
-- Tested in SQL*plus

drop table test;

create table test (a integer, b integer);

insert into test values ('&&A','&&B');

commit;

select a-b difference from test;

select a*b product from test;

select trunc(a/b) integer_quotient from test;

select mod(a,b) remainder from test;

select power(a,b) exponentiation from test;

```



```txt

SQL> @test.sql

Table dropped.


Table created.

Enter value for a: 3
Enter value for b: 4
old   1: insert into test values ('&&A','&&B')
new   1: insert into test values ('3','4')

1 row created.


Commit complete.


DIFFERENCE
----------
        -1


   PRODUCT
----------
        12


INTEGER_QUOTIENT
----------------
               0


 REMAINDER
----------
         3


EXPONENTIATION
--------------
            81


```



## SSEM

The only operation that the SSEM supports natively is substraction. This program uses the <tt>001 Sub.</tt> instruction to find the difference between <i>a</i> and <i>b</i>, assuming they are loaded into storage addresses 20 and 21 respectively.

```ssem
00101000000000100000000000000000   0. -20 to c
10100000000001100000000000000000   1. c to 5
10100000000000100000000000000000   2. -5 to c
10101000000000010000000000000000   3. Sub. 21
00000000000001110000000000000000   4. Stop
00000000000000000000000000000000   5. 0
```

The routine is slightly more complicated than it would otherwise be, because the SSEM cannot load a value into the accumulator (<tt>c</tt> register) from storage without negating it in the process—so we have to shuffle the negation of <i>a</i> back out into storage and then negate it again before we can subtract <i>b</i> from it. This does, however, make it easy to implement addition using negation and subtraction. In this program, we first negate <i>a</i>; then subtract <i>b</i>, and store the result; and finally negate that result, thereby obtaining the sum of the two integers.

```ssem
00101000000000100000000000000000   0. -20 to c
10101000000000010000000000000000   1. Sub. 21
10100000000001100000000000000000   2. c to 5
10100000000000100000000000000000   3. -5 to c
00000000000001110000000000000000   4. Stop
00000000000000000000000000000000   5. 0
```

A multiplication program will be found at [[Function definition#SSEM]], and one that performs integer division at [[Loops/For with a specified step#SSEM]].


## Standard ML


```sml
val () = let
  val a = valOf (Int.fromString (valOf (TextIO.inputLine TextIO.stdIn)))
  val b = valOf (Int.fromString (valOf (TextIO.inputLine TextIO.stdIn)))
in
  print ("a + b = "   ^ Int.toString (a + b)   ^ "\n");
  print ("a - b = "   ^ Int.toString (a - b)   ^ "\n");
  print ("a * b = "   ^ Int.toString (a * b)   ^ "\n");
  print ("a div b = " ^ Int.toString (a div b) ^ "\n");         (* truncates towards negative infinity *)
  print ("a mod b = " ^ Int.toString (a mod b) ^ "\n");         (* same sign as second operand *)
  print ("a quot b = " ^ Int.toString (Int.quot (a, b)) ^ "\n");(* truncates towards 0 *)
  print ("a rem b = " ^ Int.toString (Int.rem (a, b)) ^ "\n");  (* same sign as first operand *)
  print ("~a = "      ^ Int.toString (~a)      ^ "\n")          (* unary negation, unusual notation compared to other languages *)
end
```


## Swift


```swift

let a = 6
let b = 4

print("sum =\(a+b)")
print("difference = \(a-b)")
print("product = \(a*b)")
print("Integer quotient = \(a/b)")
print("Remainder = (a%b)")
print("No operator for Exponential")

```



## Tcl


```tcl
puts "Please enter two numbers:"

set x [expr {int([gets stdin])}]; # Force integer interpretation
set y [expr {int([gets stdin])}]; # Force integer interpretation

puts "$x + $y = [expr {$x + $y}]"
puts "$x - $y = [expr {$x - $y}]"
puts "$x * $y = [expr {$x * $y}]"
puts "$x / $y = [expr {$x / $y}]"
puts "$x mod $y = [expr {$x % $y}]"
puts "$x 'to the' $y = [expr {$x ** $y}]"
```


Since Tcl doesn't really know about the "type" of a variable, the "<tt>expr</tt>" command is used to declare whatever follows as an "expression". This means there is no such thing as "integer arithmetic" and hence the kludge with <tt>int([gets stdin])</tt>.

Often, these operations would be performed in a different way from what is shown here. For example, to increase the variable "x" by the value of the variable "y", one would write


```tcl
incr x $y
```


Also, it's important to surround the arguments to the <code>expr</code> in braces, especially when any of the parts of the expression are not literal constants.  Discussion of this is on [http://wiki.tcl.tk/10225 The Tcler's Wiki].

=={{header|TI-83 BASIC}}==
Pauses added due to TI-83's lack of screen size.

```ti83b

Prompt A,B
Disp "SUM"
Pause A+B
Disp "DIFFERENCE"
Pause A-B
Disp "PRODUCT"
Pause AB
Disp "INTEGER QUOTIENT"
Pause int(A/B)
Disp "REMAINDER"
Pause A-B*int(A/B)

```


=={{header|TI-89 BASIC}}==


```ti89b
Local a, b
Prompt a, b
Disp "Sum: " & string(a + b)
Disp "Difference: " & string(a - b)
Disp "Product: " & string(a * b)
Disp "Integer quotient: " & string(intDiv(a, b))
Disp "Remainder: " & string(remain(a, b))
```



## Toka



```toka
[ ( a b -- )
  2dup ." a+b = " + . cr
  2dup ." a-b = " - . cr
  2dup ." a*b = " * . cr
  2dup ." a/b = " / . ." remainder " mod . cr
] is mathops
```



## TUSCRIPT


```tuscript

$$ MODE TUSCRIPT
a=5
b=3
c=a+b
c=a-b
c=a*b
c=a/b
c=a%b

```

{{out}}

```txt

a=5
b=3
c=a+b
c            = 8
c=a-b
c            = 2
c=a*b
c            = 15
c=a/b
c            = 1
c=a%b
c            = 2

```



## UNIX Shell


The Unix shell does not directly support arithmetic operations, so external tools, such as ''expr'' are used to perform arithmetic calculations when required:

{{works with|Bourne Shell}}
{{works with|Almquist SHell}}

```bash
#!/bin/sh
read a; read b;
echo "a+b     = "  `expr $a  +  $b`
echo "a-b     = "  `expr $a  -  $b`
echo "a*b     = "  `expr $a \*  $b`
echo "a/b     = "  `expr $a  /  $b` # truncates towards 0
echo "a mod b = "  `expr $a  %  $b` # same sign as first operand
```


Notes: Using the ` (backtick operators, also available in most Bourne shells via the ''$(...)'' syntax) allows us to keep the results on their labels in the most efficient and portable way. The spaces around the operators in the ''expr'' command line arguments are required and the shell requires us to quote or escape the ''*'' character has shown, to prevent any possible "globbing" --- filename expansion of the ''*'' as a wildcard character.

With SUSv3 parameter expansions:

{{works with|Bourne Again SHell|3.2}}
{{works with|pdksh|5.2.14}}
{{works with|Z SHell}}

```bash
#!/bin/sh
read a; read b;
echo "a+b     = $((a+b))"
echo "a-b     = $((a-b))"
echo "a*b     = $((a*b))"
echo "a/b     = $((a/b))" # truncates towards 0
echo "a mod b = $((a%b))" # same sign as first operand
```


Note: spaces inside the ''$((...))'' are optional and not required; the ''$((...))'' can be inside or outside the double quotes, but the `...` expressions from the previous example can also be inside or outside the double quotes.


## Ursa


```ursa
#
# integer arithmetic
#

decl int x y
out "number 1: " console
set x (in int console)
out "number 2: " console
set y (in int console)

out "\nsum:\t" (int (+ x y)) endl console
out "diff:\t" (int (- x y)) endl console
out "prod:\t" (int (* x y)) endl console
# quotient doesn't round at all, but the int function rounds up
out "quot:\t" (int (/ x y)) endl console
# mod takes the sign of x
out "mod:\t" (int (mod x y)) endl console
```


Sample session:

```txt
number 1: 15
number 2: 7

sum:	22
diff:	8
prod:	105
quot:	2
mod:	1
```



## VBA


```vb

'Arithmetic - Integer
Sub RosettaArithmeticInt()
Dim opr As Variant, a As Integer, b As Integer
On Error Resume Next

a = CInt(InputBox("Enter first integer", "XLSM | Arithmetic"))
b = CInt(InputBox("Enter second integer", "XLSM | Arithmetic"))

Debug.Print "a ="; a, "b="; b, vbCr
For Each opr In Split("+ - * / \ mod ^", " ")
    Select Case opr
        Case "mod":     Debug.Print "a mod b", a; "mod"; b, a Mod b
        Case "\":       Debug.Print "a \ b", a; "\"; b, a \ b
        Case Else:      Debug.Print "a "; opr; " b", a; opr; b, Evaluate(a & opr & b)
    End Select
Next opr
End Sub

```



## VBScript

VBScript's variables are all Variants. What starts out as an integer may be converted to something else if the need arises.


### Implementation


```vb
option explicit
dim a, b
wscript.stdout.write "A? "
a = wscript.stdin.readline
wscript.stdout.write "B? "
b = wscript.stdin.readline

a = int( a )
b = int( b )

wscript.echo "a + b=", a + b
wscript.echo "a - b=", a - b
wscript.echo "a * b=", a * b
wscript.echo "a / b=", a / b
wscript.echo "a \ b=", a \ b
wscript.echo "a mod b=", a mod b
wscript.echo "a ^ b=", a ^ b
```



### Another Implementation

Gives the same output for the same input. Inspired by Python version.

```vb
option explicit
dim a, b
wscript.stdout.write "A? "
a = wscript.stdin.readline
wscript.stdout.write "B? "
b = wscript.stdin.readline

a = int( a )
b = int( b )

dim op
for each op in split("+ - * / \ mod ^", " ")
	wscript.echo "a",op,"b=",eval( "a " & op & " b")
next
```



### Invocation


```txt

C:\foo>arithmetic.vbs
A? 45
B? 11
a + b= 4511
a - b= 34
a * b= 495
a / b= 4.09090909090909
a \ b= 4
a mod b= 1
a ^ b= 1.5322783012207E+18

```



## Vedit macro language


```vedit
#1 = Get_Num("Give number a: ")
#2 = Get_Num("Give number b: ")
Message("a + b = ") Num_Type(#1 + #2)
Message("a - b = ") Num_Type(#1 - #2)
Message("a * b = ") Num_Type(#1 * #2)
Message("a / b = ") Num_Type(#1 / #2)
Message("a % b = ") Num_Type(#1 % #2)
```



## Vim Script


```vim
let a = float2nr(input("Number 1: ") + 0)
let b = float2nr(input("Number 2: ") + 0)
echo "\nSum: " . (a + b)
echo "Difference: " . (a - b)
echo "Product: " . (a * b)
" The result of an integer division is truncated
echo "Quotient: " . (a / b)
" The sign of the result of the remainder operation matches the sign of
" the first operand
echo "Remainder: " . (a % b)
```



## Visual Basic .NET


```vbnet
Imports System.Console
Module Module1
  Sub Main
    Dim a = CInt(ReadLine)
    Dim b = CInt(ReadLine)
    WriteLine("Sum " & a + b)
    WriteLine("Difference " & a - b)
    WriteLine("Product " & a - b)
    WriteLine("Quotient " & a / b)
    WriteLine("Integer Quotient " & a \ b)
    WriteLine("Remainder " & a Mod b)
    WriteLine("Exponent " & a ^ b)
  End Sub
End Module
```



## Wart


```python
a <- (read)
b <- (read)
prn "sum: " a+b
prn "difference: " a-b
prn "product: " a*b
prn "quotient: " a/b
prn "integer quotient: " (int a/b)
prn "remainder: " a%b
prn "exponent: " a^b
```



## Wren


```wren

import "io" for Stdin
var a = Num.fromString(Stdin.readLine())
var b = Num.fromString(Stdin.readLine())
System.print("sum:              %(a + b)")
System.print("difference:       %(a - b)")
System.print("product:          %(a * b)")
System.print("integer quotient: %((a / b).floor)")
System.print("remainder:        %(a % b)")

```



## x86 Assembly

Input and output would be OS-specific and are not implemented. This routine works on the 16-bit 8086, as well as on its 32-bit and 64-bit successors: it could be trivially modified to perform 32-bit or 64-bit arithmetic on machines where those are supported. The quotient is truncated towards zero; the remainder takes its sign from the first operand.

```asm
arithm: mov      cx,          a
        mov      bx,          b
        xor      dx,          dx

        mov      ax,          cx
        add      ax,          bx
        mov      sum,         ax

        mov      ax,          cx
        imul     bx
        mov      product,     ax

        mov      ax,          cx
        sub      ax,          bx
        mov      difference,  ax

        mov      ax,          cx
        idiv     bx
        mov      quotient,    ax
        mov      remainder,   dx

        ret
```



## XLISP


```xlisp
(DEFUN INTEGER-ARITHMETIC ()
    (DISPLAY "Enter two integers separated by a space.")
    (NEWLINE)
    (DISPLAY "> ")
    (DEFINE A (READ))
    (DEFINE B (READ))
    (DISPLAY `(SUM ,(+ A B)))
    (NEWLINE)
    (DISPLAY `(DIFFERENCE ,(- A B)))
    (NEWLINE)
    (DISPLAY `(PRODUCT ,(* A B)))
    (NEWLINE)
    (DISPLAY `(QUOTIENT ,(QUOTIENT A B))) ; truncates towards zero
    (NEWLINE)
    (DISPLAY `(REMAINDER ,(REM A B))) ; takes sign of first operand
    (NEWLINE)
    (DISPLAY `(EXPONENTIATION ,(EXPT A B))))
```



## XPL0


```XPL0
include c:\cxpl\codes;
int A, B;
[A:= IntIn(0);
 B:= IntIn(0);
IntOut(0, A+B); CrLf(0);
IntOut(0, A-B); CrLf(0);
IntOut(0, A*B); CrLf(0);
IntOut(0, A/B); CrLf(0);        \truncates toward zero
IntOut(0, rem(0)); CrLf(0);     \remainder's sign matches first operand (A)
]
```



## XSLT


```xml
<xsl:template name="arithmetic">
  <xsl:param name="a">5</xsl:param>
  <xsl:param name="b">2</xsl:param>
  <fo:block>a + b = <xsl:value-of select="$a + $b"/></fo:block>
  <fo:block>a - b = <xsl:value-of select="$a - $b"/></fo:block>
  <fo:block>a * b = <xsl:value-of select="$a * $b"/></fo:block>
  <fo:block>a / b = <xsl:value-of select="round($a div $b)"/></fo:block>
  <fo:block>a mod b = <xsl:value-of select="$a mod $b"/></fo:block>
 </xsl:template>
```



## Yorick


```yorick
x = y = 0;
read, x, y;
write, "x + y =", x + y;
write, "x - y =", x - y;
write, "x * y =", x * y;
write, "x / y =", x / y; // rounds toward zero
write, "x % y =", x % y; // remainder; matches sign of first operand when operands' signs differ
write, "x ^ y =", x ^ y; // exponentiation
```



## zkl


```zkl
x,y:=ask("Two ints: ").split(" ").apply("toInt");
println("x+y = ",x + y);
println("x-y = ",x - y);
println("x*y = ",x * y);
println("x/y = ",x / y); // rounds toward zero
println("x%y = ",x % y); // remainder; matches sign of first operand when operands' signs differ
println("x.divr(y) = ",x.divr(y)); // (x/y,remainder); sign as above
```


## zonnon


```zonnon

module Main;
var
	i,j: integer;
begin
	write("A integer?:");readln(i);
	write("another?: ");readln(j);
	writeln("sum: ",i + j);
	writeln("difference: ", i - j);
	writeln("product: ", i * j);
	writeln("quotient: ", i div j);
	writeln("remainder: ", i mod j);
end Main.

```

{{Out}}

```txt

A integer?:2
another?: 3
sum:                    5
difference:                   -1
product:                    6
quotient:                    0
remainder:                    2

```


## ZX Spectrum Basic


```zxbasic
5 LET a=5: LET b=3
10 PRINT a;" + ";b;" = ";a+b
20 PRINT a;" - ";b;" = ";a-b
30 PRINT a;" * ";b;" = ";a*b
40 PRINT a;" / ";b;" = ";INT (a/b)
50 PRINT a;" mod ";b;" = ";a-INT (a/b)*b
60 PRINT a;" to the power of ";b;" = ";a^b

```

