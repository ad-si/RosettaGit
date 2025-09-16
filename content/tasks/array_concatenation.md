+++
title = "Array concatenation"
description = ""
date = 2019-10-17T04:14:17Z
aliases = []
[extra]
id = 4833
[taxonomies]
categories = ["Data Structures", "Simple", "task"]
tags = []
languages = [
  "11l",
  "8th",
  "abap",
  "acl2",
  "actionscript",
  "ada",
  "aime",
  "algol_68",
  "algol_w",
  "antlang",
  "apex",
  "apl",
  "applescript",
  "arm_assembly",
  "arturo",
  "autohotkey",
  "autoit",
  "awk",
  "babel",
  "bacon",
  "bash",
  "basic",
  "bbc_basic",
  "bracmat",
  "burlesque",
  "c",
  "ceylon",
  "clojure",
  "cobol",
  "coffeescript",
  "commodore_basic",
  "common_lisp",
  "component_pascal",
  "cpp",
  "csharp",
  "d",
  "delphi",
  "dyalect",
  "e",
  "easylang",
  "echolisp",
  "ecl",
  "efene",
  "egl",
  "ela",
  "elena",
  "elixir",
  "elm",
  "emacs_lisp",
  "erlang",
  "erre",
  "euphoria",
  "factor",
  "fantom",
  "fbsl",
  "forth",
  "fortran",
  "free_pascal",
  "freebasic",
  "frink",
  "funl",
  "futhark",
  "gambas",
  "gap",
  "genie",
  "go",
  "gosu",
  "groovy",
  "haskell",
  "hicest",
  "hy",
  "i",
  "idl",
  "idris",
  "inform_7",
  "ioke",
  "j",
  "java",
  "javascript",
  "jq",
  "julia",
  "k",
  "klong",
  "kotlin",
  "labview",
  "lang5",
  "lasso",
  "lfe",
  "liberty_basic",
  "lil",
  "limbo",
  "lingo",
  "little",
  "logo",
  "lua",
  "m2000_interpreter",
  "maple",
  "maxima",
  "mercury",
  "min",
  "miniscript",
  "neko",
  "nemerle",
  "netrexx",
  "newlisp",
  "nial",
  "nim",
  "objeck",
  "ocaml",
  "oforth",
  "onyx",
  "oorexx",
  "order",
  "oxygenbasic",
  "oz",
  "pari_gp",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "php",
  "picolisp",
  "pl_i",
  "pony",
  "postscript",
  "powershell",
  "prolog",
  "purebasic",
  "python",
  "q",
  "r",
  "racket",
  "rapidq",
  "rebol",
  "red",
  "retro",
  "rexx",
  "ring",
  "rlab",
  "ruby",
  "rust",
  "sasl",
  "scala",
  "scheme",
  "seed7",
  "setl",
  "sidef",
  "simula",
  "slate",
  "smalltalk",
  "snobol4",
  "standard_ml",
  "stata",
  "swift",
  "tcl",
  "trith",
  "unix_shell",
  "ursa",
  "vala",
  "vba",
  "vbscript",
  "visual_basic_.net",
  "wart",
  "wren",
  "yabasic",
  "yacas",
  "yorick",
  "zkl",
  "zonnon",
  "unix_shell",
  "zx_spectrum_basic",
]
+++

## Task

;Task:
Show how to concatenate two arrays in your language.


If this is as simple as <code><var>array1</var> + <var>array2</var></code>, so be it.





## 11l


```11l
V arr1 = [1, 2, 3]
V arr2 = [4, 5, 6]
print(arr1 [+] arr2)
```

{{out}}

```txt

[1, 2, 3, 4, 5, 6]

```



## 8th


```Forth

[1,2,3] [4,5,6] a:+ .

```

{{out}}

```txt

[1,2,3,4,5,6]

```



## ABAP

The concept of arrays does not exist in ABAP, instead internal tables are used. This works in ABAP version 7.40 and above.


```ABAP

report z_array_concatenation.

data(itab1) = value int4_table( ( 1 ) ( 2 ) ( 3 ) ).
data(itab2) = value int4_table( ( 4 ) ( 5 ) ( 6 ) ).

append lines of itab2 to itab1.

loop at itab1 assigning field-symbol(<line>).
    write <line>.
endloop.

```


{{out}}


```txt

         1           2           3           4           5           6

```



## ACL2

This is for lists, not arrays; ACL2's array support is limited.

```Lisp
(append xs ys)
```



## ActionScript


```ActionScript
var array1:Array = new Array(1, 2, 3);
var array2:Array = new Array(4, 5, 6);
var array3:Array = array1.concat(array2); //[1, 2, 3, 4, 5, 6]
```



## Ada

In [[Ada]] arrays are concatenated using the operation &. It works with any one dimensioned array:

```Ada
type T is array (Positive range <>) of Integer;
X : T := (1, 2, 3);
Y : T := X & (4, 5, 6); -- Concatenate X and (4, 5, 6)
```



## Aime


```aime
ac(list a, b)
{
    list o;

    o.copy(a);
    b.ucall(l_append, 1, o);

    o;
}

main(void)
{
    list a, b, c;

    a = list(1, 2, 3, 4);
    b = list(5, 6, 7, 8);

    c = ac(a, b);

    c.ucall(o_, 1, " ");

    0;
}
```

{{Out}}

```txt
 1 2 3 4 5 6 7 8
```



## ALGOL 68

{{works with|ALGOL 68|Standard - no extensions to language used}}

{{works with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-1.18.0/algol68g-1.18.0-9h.tiny.el5.centos.fc11.i386.rpm/download 1.18.0-9h.tiny]}}

<!-- {{not tested with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8.8d.fc9.i386]}} -->
Includes operators for ''appending'' and ''prefixing'' an array to an existing flexible array:

```Algol68
MODE ARGTYPE = INT;
MODE ARGLIST = FLEX[0]ARGTYPE;

OP + = (ARGLIST a, b)ARGLIST: (
  [LWB a:UPB a - LWB a + 1 + UPB b - LWB b + 1 ]ARGTYPE out;
  (
    out[LWB a:UPB a]:=a,
    out[UPB a+1:]:=b
  );
  out
);

# Append #
OP +:=    = (REF ARGLIST lhs, ARGLIST rhs)ARGLIST: lhs := lhs + rhs;
OP PLUSAB = (REF ARGLIST lhs, ARGLIST rhs)ARGLIST: lhs := lhs + rhs;

# Prefix #
OP +=:    = (ARGLIST lhs, REF ARGLIST rhs)ARGLIST: rhs := lhs + rhs;
OP PLUSTO = (ARGLIST lhs, REF ARGLIST rhs)ARGLIST: rhs := lhs + rhs;

ARGLIST a := (1,2),
        b := (3,4,5);

print(("a + b",a + b, new line));

VOID(a +:= b);
print(("a +:= b", a, new line));

VOID(a +=: b);
print(("a +=: b", b, new line))
```


```txt

a + b         +1         +2         +3         +4         +5
a +:= b         +1         +2         +3         +4         +5
a +=: b         +1         +2         +3         +4         +5         +3         +4         +5

```



## ALGOL W

Algol W does not allow procedures to return arrays and has no mechanism for procedures to find the bounds of their parameters, so the caller must supply an array to concatenate into and the bounds of the arrays.

```algolw
begin
    integer array a ( 1 :: 5 );
    integer array b ( 2 :: 4 );
    integer array c ( 1 :: 8 );

    % concatenates the arrays a and b into c                        %
    % the lower and upper bounds of each array must be specified in %
    % the corresponding *Lb and *Ub parameters                      %
    procedure arrayConcatenate ( integer array a ( * )
                               ; integer value aLb, aUb
                               ; integer array b ( * )
                               ; integer value bLb, bUb
                               ; integer array c ( * )
                               ; integer value cLb, cUb
                               ) ;
        begin
            integer cPos;
            assert( ( cUb - cLb ) + 1 >= ( ( aUb + bUb ) - ( aLb + bLb ) ) - 2 );
            cPos := cLb;
            for aPos := aLb until aUb do begin
                c( cPos ) := a( aPos );
                cPos := cPos + 1
            end for_aPos ;
            for bPos := bLb until bUb do begin
                c( cPos ) := b( bPos );
                cPos := cPos + 1
            end for_bPos
        end arrayConcatenate ;

    % test arrayConcatenate                                          %
    for aPos := 1 until 5 do a( aPos ) := aPos;
    for bPos := 2 until 4 do b( bPos ) := - bPos;
    arrayConcatenate( a, 1, 5, b, 2, 4, c, 1, 8 );
    for cPos := 1 until 8 do writeon( i_w := 1, s_w := 1, c( cPos ) )

end.
```

{{out}}

```txt

1 2 3 4 5 -2 -3 -4

```



## AntLang


```AntLang
a:<1; <2; 3>>

b: <"Hello"; 42>
c: a,b
```



## APL


```apl

    1 2 3 , 4 5 6
1 2 3 4 5 6

```



## Apex


```apex
List<String> listA = new List<String
 { 'apple' };
List<String> listB = new List<String> { 'banana' };
listA.addAll(listB);
System.debug(listA); // Prints (apple, banana)
```



## AppleScript


```AppleScript

set listA to {1, 2, 3}
set listB to {4, 5, 6}
return listA & listB

```


{{out}}

```txt

{1, 2, 3, 4, 5, 6}

```



Or, if we treat the concatenation of two lists as a special case of the more general problem of concatenating N lists, we can write:

{{trans|JavaScript}}


```applescript
on run

  concat([["alpha", "beta", "gamma"], ¬
    ["delta", "epsilon", "zeta"], ¬
    ["eta", "theta", "iota"]])

end run


-- concat :: [[a]] -> [a]
on concat(xxs)
  set lst to {}
  repeat with xs in xxs
    set lst to lst & xs
  end repeat
  return lst
end concat


```


{{Out}}


```txt
{"alpha", "beta", "gamma", "delta", "epsilon", "zeta", "eta", "theta", "iota"}
```



## ARM Assembly

{{works with|as|Raspberry Pi}}

```ARM Assembly

/* ARM assembly Raspberry PI  */
/*  program concAreaString.s   */

/* Constantes    */
.equ STDOUT, 1     @ Linux output console
.equ EXIT,   1     @ Linux syscall
.equ WRITE,  4     @ Linux syscall
.equ NBMAXITEMS,  20   @
/* Initialized data */
.data
szMessLenArea: .ascii "The length of area 3  is : "
sZoneconv:		 .fill 12,1,' '
szCarriageReturn:  .asciz "\n"

/* areas strings  */
szString1:  .asciz "Apples"
szString2:  .asciz "Oranges"
szString3:  .asciz "Pommes"
szString4:  .asciz "Raisins"
szString5:  .asciz "Abricots"

/* pointer items area 1*/
tablesPoi1:
pt1_1:		.int szString1
pt1_2:   .int szString2
ptVoid_1:		.int 0

/* pointer items area 2*/
tablesPoi2:
pt2_1:		.int szString3
pt2_2:   	.int szString4
pt2_3:   	.int szString5
ptVoid_2:		.int 0

/* UnInitialized data */
.bss
tablesPoi3:    .skip   4 * NBMAXITEMS

/*  code section */
.text
.global main
main:                /* entry of program  */
    push {fp,lr}    /* saves 2 registers */

    @ copy area 1 ->  area 3
    ldr r1,iAdrtablesPoi1  @ begin pointer area 1
    ldr r3,iAdrtablesPoi3  @ begin pointer area 3
    mov r0,#0    @ counter
1:
    ldr r2,[r1,r0,lsl #2]    @ read string pointer address item r0 (4 bytes by pointer)
    cmp r2,#0                @ is null ?
    strne r2,[r3,r0,lsl #2]    @ no store pointer in area 3
    addne r0,#1             @ increment counter
    bne 1b                  @ and loop
    @ copy area 2 ->  area 3
    ldr r1,iAdrtablesPoi2  @ begin pointer area 2
    ldr r3,iAdrtablesPoi3  @ begin pointer area 3
    mov r4,#0    @ counter area 2
2:        @ r0 contains the first void item in area 3
    ldr r2,[r1,r4,lsl #2]    @ read string pointer address item r0 (4 bytes by pointer)
    cmp r2,#0                @ is null ?
    strne r2,[r3,r0,lsl #2]    @ no store pointer in area 3
    addne r0,#1             @ increment counter
    addne r4,#1             @ increment counter
    bne 2b                  @ and loop

	@ count items number in area 3
    ldr r1,iAdrtablesPoi3  @ begin pointer table
    mov r0,#0    @ counter
3:              @ begin loop
    ldr r2,[r1,r0,lsl #2]    @ read string pointer address item r0 (4 bytes by pointer)
    cmp r2,#0                @ is null ?
    addne r0,#1             @ no increment counter
    bne 3b                  @ and loop

    ldr r1,iAdrsZoneconv   @ conversion decimal
    bl conversion10S
    ldr r0,iAdrszMessLenArea
    bl affichageMess

100:   /* standard end of the program */
    mov r0, #0                  @ return code
    pop {fp,lr}                 @restaur 2 registers
    mov r7, #EXIT              @ request to exit program
    swi 0                       @ perform the system call
iAdrtablesPoi1:		.int tablesPoi1
iAdrtablesPoi2:		.int tablesPoi2
iAdrtablesPoi3:		.int tablesPoi3
iAdrszMessLenArea:  .int szMessLenArea
iAdrsZoneconv:		.int  sZoneconv
iAdrszCarriageReturn:  .int  szCarriageReturn
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

/***************************************************/
/*   conversion register signed décimal     */
/***************************************************/
/* r0 contient le registre   */
/* r1 contient l adresse de la zone de conversion */
conversion10S:
    push {r0-r5,lr}    /* save des registres */
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
    pop {r0-r5,lr}   /*restaur desregistres */
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
   bx lr                  /* leave function */
.Ls_magic_number_10: .word 0x66666667


```



## Arturo



```arturo
arr1 #(1 2 3)
arr2 #("four" "five" "six")

print arr1 + arr2
```


{{out}}


```txt
#(1 2 3 "four" "five" "six")
```



## AutoHotkey


###  True Arrays

{{works with|AutoHotkey_L}}

```AHK
List1 := [1, 2, 3]
List2 := [4, 5, 6]
cList := Arr_concatenate(List1, List2)
MsgBox % Arr_disp(cList) ; [1, 2, 3, 4, 5, 6]

Arr_concatenate(p*) {
    res := Object()
    For each, obj in p
        For each, value in obj
            res.Insert(value)
    return res
}

Arr_disp(arr) {
    for each, value in arr
        res .= ", " value
    return "[" SubStr(res, 3) "]"
}
```


###  Legacy versions

[[AutoHotkey_Basic]] does not have real Arrays, but the user can implement them quite easily. For example:

```AutoHotkey
List1 = 1,2,3
List2 = 4,5,6

List2Array(List1 , "Array1_")
List2Array(List2 , "Array2_")

ConcatArrays("Array1_", "Array2_", "MyArray")
MsgBox, % Array2List("MyArray")


;---------------------------------------------------------------------------
ConcatArrays(A1, A2, A3) { ; concatenates the arrays A1 and A2 to A3
;---------------------------------------------------------------------------
    local i := 0
    %A3%0 := %A1%0 + %A2%0
    Loop, % %A1%0
        i++, %A3%%i% := %A1%%A_Index%
    Loop, % %A2%0
        i++, %A3%%i% := %A2%%A_Index%
}


;---------------------------------------------------------------------------
List2Array(List, Array) { ; creates an array from a comma separated list
;---------------------------------------------------------------------------
    global
    StringSplit, %Array%, List, `,
}


;---------------------------------------------------------------------------
Array2List(Array) { ; returns a comma separated list from an array
;---------------------------------------------------------------------------
    Loop, % %Array%0
        List .= (A_Index = 1 ? "" : ",") %Array%%A_Index%
    Return, List
}
```

Message box shows:

```txt
1,2,3,4,5,6
```



## AutoIt


_ArrayConcatenate is a standard function in Autoit, there´s no need to write it on your own




```AutoIt

_ArrayConcatenate($avArray, $avArray2)
Func _ArrayConcatenate(ByRef $avArrayTarget, Const ByRef $avArraySource, $iStart = 0)
	If Not IsArray($avArrayTarget) Then Return SetError(1, 0, 0)
	If Not IsArray($avArraySource) Then Return SetError(2, 0, 0)
	If UBound($avArrayTarget, 0) <> 1 Then
		If UBound($avArraySource, 0) <> 1 Then Return SetError(5, 0, 0)
		Return SetError(3, 0, 0)
	EndIf
	If UBound($avArraySource, 0) <> 1 Then Return SetError(4, 0, 0)

	Local $iUBoundTarget = UBound($avArrayTarget) - $iStart, $iUBoundSource = UBound($avArraySource)
	ReDim $avArrayTarget[$iUBoundTarget + $iUBoundSource]
	For $i = $iStart To $iUBoundSource - 1
		$avArrayTarget[$iUBoundTarget + $i] = $avArraySource[$i]
	Next

	Return $iUBoundTarget + $iUBoundSource
EndFunc   ;==>_ArrayConcatenate

```



## AWK


```AWK
#!/usr/bin/awk -f
BEGIN {
    split("cul-de-sac",a,"-")
    split("1-2-3",b,"-")
    concat_array(a,b,c)

    for (i in c) {
        print i,c[i]
    }
}

function concat_array(a,b,c) {
    for (i in a) {
        c[++nc]=a[i]
    }
    for (i in b) {
       c[++nc]=b[i]
    }
}
```



## Babel


```babel
[1 2 3] [4 5 6] cat ;
```


{{Out}}

```txt
[val 0x1 0x2 0x3 0x4 0x5 0x6 ]
```



## bash


```bash
x=("1  2" "3  4")
y=(5 6)
sum=( "${x[@]}" "${y[@]}" )

for i in "${sum[@]}" ; do echo "$i" ; done
1  2
3  4
5
6
```



## BASIC


=
## BaCon
=

```freebasic
' Array concatenation
OPTION BASE 1

CONST asize = 2
CONST bsize = 3
DECLARE a[asize] TYPE NUMBER
DECLARE b[bsize] TYPE NUMBER

' BaCon has no array concatenation builtin, it will need to be done by hand
LOCAL c TYPE NUMBER ARRAY asize + bsize
FOR i = 1 TO asize
    c[i] = a[i]
NEXT

FOR i = 1 TO bsize
    c[asize + i] = b[i]
NEXT
```


=
## BBC BASIC
=
{{works with|BBC BASIC for Windows}}

```bbcbasic
      DIM a(3), b(4)
      a() = 1, 2, 3, 4
      b() = 5, 6, 7, 8, 9
      PROCconcat(a(), b(), c())

      FOR i% = 0 TO DIM(c(),1)
        PRINT c(i%)
      NEXT
      END

      DEF PROCconcat(a(), b(), RETURN c())
      LOCAL s%, na%, nb%
      s% = ^a(1) - ^a(0) : REM Size of each array element
      na% = DIM(a(),1)+1 : REM Number of elements in a()
      nb% = DIM(b(),1)+1 : REM Number of elements in b()
      DIM c(na%+nb%-1)
      SYS "RtlMoveMemory", ^c(0), ^a(0), s%*na%
      SYS "RtlMoveMemory", ^c(na%), ^b(0), s%*nb%
      ENDPROC
```


=
## Commodore BASIC
=
(Based on ZX Spectrum BASIC version)

```basic
10 X=4 : Y=5
20 DIM A(X) : DIM B(Y) : DIM C(X+Y)
30 FOR I=1 TO X
40 : A(I) = I
50 NEXT
60 FOR I=1 TO Y
70 : B(I) = I*10
80 NEXT
90 FOR I=1 TO X
100 : C(I) = A(I)
110 NEXT
120 FOR I=1 TO Y
130 : C(X+I) = B(I)
140 NEXT
150 FOR I=1 TO X+Y
160 : PRINT C(I);
170 NEXT
```



## Bracmat

Bracmat concatenates lists composed with the comma, space, addition and multiplication operators. Furthermore, lists composed with the addition and multiplication operators are canonically sorted and like terms or factors are combined algebraically. Lists composed with the space operator automatically delete any elements with zero-length atoms and no prefixes. All these lists except the comma-separated list support a notion of 'array index', but as the underlying datastructure is a linked list and not an array, accessing, say, the millionth element can be slow. Examples of concatenation (entered on the Bracmat command line):

```txt
{?} (a,b,c,d,e),(n,m)
{!} a,b,c,d,e,n,m
{?} (a,m,y),(b,n,y,z)
{!} a,m,y,b,n,y,z
{?} (a m y) (b n y z)
{!} a m y b n y z
{?} (a+m+y)+(b+n+y+z)
{!} a+b+m+n+2*y+z
{?} (a*m*y)*(b*n*y*z)
{!} a*b*m*n*y^2*z
```

Concatenate three lists and split the concatenated list using a position operator:

```txt
{?} (a b c d) (e f g h) (i j k):?A [7 ?Z
{!} a b c d e f g h i j k
{?} !A
{!} a b c d e f g
{?} !Z
{!} h i j k
```



## Burlesque



```burlesque

blsq ) {1 2 3}{4 5 6}_+
{1 2 3 4 5 6}

```



## C

A way to concatenate two C arrays when you know their size (and usually so it is)

```cpp
#include <iostream>
#include <stdio.h>
#include <string.h>

#define ARRAY_CONCAT(TYPE, A, An, B, Bn) \
  (TYPE *)array_concat((const void *)(A), (An), (const void *)(B), (Bn), sizeof(TYPE));

void *array_concat(const void *a, size_t an,
                   const void *b, size_t bn, size_t s)
{
  char *p = malloc(s * (an + bn));
  memcpy(p, a, an*s);
  memcpy(p + an*s, b, bn*s);
  return p;
}

// testing
const int a[] = { 1, 2, 3, 4, 5 };
const int b[] = { 6, 7, 8, 9, 0 };

int main(void)
{
  unsigned int i;

  int *c = ARRAY_CONCAT(int, a, 5, b, 5);

  for(i = 0; i < 10; i++)
    printf("%d\n", c[i]);

  free(c);
  return EXIT_SUCCCESS;
}
```



## C++


```cpp
#include <vector>
#include <iostream>

int main()
{
  std::vector<int> a(3), b(4);
  a[0] = 11; a[1] = 12; a[2] = 13;
  b[0] = 21; b[1] = 22; b[2] = 23; b[3] = 24;

  a.insert(a.end(), b.begin(), b.end());

  for (int i = 0; i < a.size(); ++i)
    std::cout << "a[" << i << "] = " << a[i] << "\n";
}
```


{{works with|C++11}}
Similar to above but using initialization schematics.


```cpp
#include <vector>
#include <iostream>

int main() {
  std::vector<int> a {1, 2, 3, 4};
  std::vector<int> b {5, 6, 7, 8, 9};

  a.insert(a.end(), b.begin(), b.end());

  for(int& i: a) std::cout << i << " ";
  std::cout << std::endl;
  return 0;
}
```


This is another solution with function level templates and pointers.


```cpp
#include <iostream>

using namespace std;

template <typename T1, typename T2>
int* concatArrays( T1& array_1, T2& array_2) {
  int arrayCount_1 = sizeof(array_1) / sizeof(array_1[0]);
  int arrayCount_2 = sizeof(array_2) / sizeof(array_2[0]);
  int newArraySize = arrayCount_1 + arrayCount_2;

  int *p = new int[newArraySize];

  for (int i = 0; i < arrayCount_1; i++) {
    p[i] = array_1[i];
  }

  for (int i = arrayCount_1; i < newArraySize; i++) {
    int newIndex = i-arrayCount_2;

    if (newArraySize % 2 == 1)
	newIndex--;

    p[i] = array_2[newIndex];
    cout << "i: " << i << endl;
    cout << "array_2[i]: " << array_2[newIndex] << endl;
    cout << endl;
  }

  return p;
}

int main() {

  int ary[4] = {1, 2, 3, 123};
  int anotherAry[3] = {4, 5, 6};

  int *r = concatArrays(ary, anotherAry);

  cout << *(r + 0) << endl;
  cout << *(r + 1) << endl;
  cout << *(r + 2) << endl;
  cout << *(r + 3) << endl;
  cout << *(r + 4) << endl;
  cout << *(r + 5) << endl;
  cout << *(r + 6) << endl;

  delete r;

  return 0;
}
```



## C#


```c#
using System;

namespace RosettaCode
{
    class Program
    {
        static void Main(string[] args)
        {
            int[] a = { 1, 2, 3 };
            int[] b = { 4, 5, 6 };

            int[] c = new int[a.Length + b.Length];
            a.CopyTo(c, 0);
            b.CopyTo(c, a.Length);

            foreach(int n in c)
            {
                Console.WriteLine(n.ToString());
            }
        }
    }
}
```


Alternatively, using LINQ extension methods:

{{works with|C sharp|C#|3}}

```c#
using System.Linq;

class Program
{
    static void Main(string[] args)
    {
        int[] a = { 1, 2, 3 };
        int[] b = { 4, 5, 6 };

        int[] c = a.Concat(b).ToArray();
    }
}
```



## Ceylon


```ceylon
shared void arrayConcatenation() {
	value a = Array {1, 2, 3};
	value b = Array {4, 5, 6};
	value c = concatenate(a, b);
	print(c);
}
```



## Clojure


```clojure
(concat [1 2 3] [4 5 6])
```

The inputs can be any collection, including Java arrays, and returns a lazy sequence of the elements.

A vector is the closest Clojure thing to an array. If a vector is wanted, then use

```clojure
(into [1 2 3] [4 5 6])
```



## COBOL


```COBOL
       identification division.
       program-id. array-concat.

       environment division.
       configuration section.
       repository.
           function all intrinsic.

       data division.
       working-storage section.
       01 table-one.
          05 int-field pic 999 occurs 0 to 5 depending on t1.
       01 table-two.
          05 int-field pic 9(4) occurs 0 to 10 depending on t2.

       77 t1           pic 99.
       77 t2           pic 99.

       77 show         pic z(4).

       procedure division.
       array-concat-main.
       perform initialize-tables
       perform concatenate-tables
       perform display-result
       goback.

       initialize-tables.
           move 4 to t1
           perform varying tally from 1 by 1 until tally > t1
               compute int-field of table-one(tally) = tally * 3
           end-perform

           move 3 to t2
           perform varying tally from 1 by 1 until tally > t2
               compute int-field of table-two(tally) = tally * 6
           end-perform
       .

       concatenate-tables.
           perform varying tally from 1 by 1 until tally > t1
               add 1 to t2
               move int-field of table-one(tally)
                 to int-field of table-two(t2)
           end-perform
       .

       display-result.
           perform varying tally from 1 by 1 until tally = t2
               move int-field of table-two(tally) to show
               display trim(show) ", " with no advancing
           end-perform
           move int-field of table-two(tally) to show
           display trim(show)
       .

       end program array-concat.
```

{{out}}

```txt
prompt$ cobc -xjd array-concatenation.cob
6, 12, 18, 3, 6, 9, 12

```



## CoffeeScript


```coffeescript

# like in JavaScript
a = [1, 2, 3]
b = [4, 5, 6]
c = a.concat b

```



## Common Lisp

<code>[http://www.lispworks.com/documentation/HyperSpec/Body/f_concat.htm concatenate]</code> is a general function for concatenating any type of sequence. It takes the type of sequence to produce, followed by any number of sequences of any type.

```lisp
(concatenate 'vector #(0 1 2 3) #(4 5 6 7))
  => #(0 1 2 3 4 5 6 7)
```


### Alternate solution

I use [https://franz.com/downloads/clp/survey Allegro CL 10.1]


```lisp

(setf arr1 (make-array '(3) :initial-contents '(1 2 3)))
(setf arr2 (make-array '(3) :initial-contents '(4 5 6)))
(setf arr3 (make-array '(3) :initial-contents '(7 8 9)))
(setf arr4 (make-array '(6)))
(setf arr5 (make-array '(9)))
(setf arr4 (concatenate `(vector ,(array-element-type arr1)) arr1 arr2))
(format t "~a" "concatenate arr1 and arr2: ")
(write arr4)
(terpri)
(setf arr5 (concatenate `(vector ,(array-element-type arr1)) arr4 arr3))
(format t "~a" "concatenate arr4 and arr3: ")
(write arr5)
(terpri)

```

Output:

```txt

concatenate arr1 and arr2: #(1 2 3 4 5 6)
concatenate arr4 and arr3: #(1 2 3 4 5 6 7 8 9)

```



## Component Pascal

BlackBox Component Builder

```oberon2

MODULE ArrayConcat;
IMPORT StdLog;

PROCEDURE Concat(x: ARRAY OF INTEGER; y: ARRAY OF INTEGER; OUT z: ARRAY OF INTEGER);
VAR
	i: INTEGER;
BEGIN
	ASSERT(LEN(x) + LEN(y) <= LEN(z));
	FOR i := 0 TO LEN(x) - 1 DO z[i] := x[i] END;
	FOR i := 0 TO LEN(y) - 1 DO z[i + LEN(x)] := y[i] END
END Concat;

PROCEDURE Concat2(x: ARRAY OF INTEGER;y: ARRAY OF INTEGER): POINTER TO ARRAY OF INTEGER;
VAR
	z: POINTER TO ARRAY OF INTEGER;
	i: INTEGER;
BEGIN
	NEW(z,LEN(x) + LEN(y));
	FOR i := 0 TO LEN(x) - 1 DO z[i] := x[i] END;
	FOR i := 0 TO LEN(y) - 1 DO z[i + LEN(x)] := y[i] END;
	RETURN z;
END Concat2;

PROCEDURE ShowArray(x: ARRAY OF INTEGER);
VAR
	i: INTEGER;
BEGIN
	i := 0;
	StdLog.Char('[');
	WHILE (i < LEN(x)) DO
		StdLog.Int(x[i]);IF i < LEN(x) - 1 THEN StdLog.Char(',') END;
		INC(i)
	END;
	StdLog.Char(']');StdLog.Ln;
END ShowArray;

PROCEDURE Do*;
VAR
	x: ARRAY 10 OF INTEGER;
	y: ARRAY 15 OF INTEGER;
	z: ARRAY 25 OF INTEGER;
	w: POINTER TO ARRAY OF INTEGER;
	i: INTEGER;
BEGIN
	FOR i := 0 TO LEN(x) - 1 DO x[i] := i END;
	FOR i := 0 TO LEN(y) - 1 DO y[i] := i END;
	Concat(x,y,z);StdLog.String("1> ");ShowArray(z);

	NEW(w,LEN(x) + LEN(y));
	Concat(x,y,z);StdLog.String("2:> ");ShowArray(z);

	StdLog.String("3:> ");ShowArray(Concat2(x,y));
END Do;

END ArrayConcat.

```

Execute: ^Q ArrayConcat.Do <br/>
{{out}}

```txt

1> [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14]
2:> [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14]
3:> [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14]


```



## D


```d
import std.stdio: writeln;

void main() {
    int[] a = [1, 2];
    int[] b = [4, 5, 6];

    writeln(a, " ~ ", b, " = ", a ~ b);
}
```

{{out}}

```txt
[1, 2] ~ [4, 5, 6] = [1, 2, 4, 5, 6]
```



## Delphi


```delphi
type
  TReturnArray = array of integer; //you need to define a type to be able to return it

function ConcatArray(a1,a2:array of integer):TReturnArray;
var
  i,r:integer;
begin
  { Low(array) is not necessarily 0 }
  SetLength(result,High(a1)-Low(a1)+High(a2)-Low(a2)+2); //BAD idea to set a length you won't release, just to show the idea!
  r:=0; //index on the result may be different to indexes on the sources
  for i := Low(a1) to High(a1) do begin
    result[r] := a1[i];
    Inc(r);
  end;
  for i := Low(a2) to High(a2) do begin
    result[r] := a2[i];
    Inc(r);
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  a1,a2:array of integer;
  r1:array of integer;
  i:integer;
begin
  SetLength(a1,4);
  SetLength(a2,3);
  for i := Low(a1) to High(a1) do
    a1[i] := i;
  for i := Low(a2) to High(a2) do
    a2[i] := i;
  TReturnArray(r1) := ConcatArray(a1,a2);
  for i := Low(r1) to High(r1) do
    showMessage(IntToStr(r1[i]));
  Finalize(r1); //IMPORTANT!
  ShowMessage(IntToStr(High(r1)));
end;
```



## Dyalect



```dyalect
var xs = [1,2,3]
var ys = [4,5,6]
var alls = Array.concat(xs, ys)
print(alls)
```


{{out}}


```txt
[1, 2, 3, 4, 5, 6]
```



## E



```e
? [1,2] + [3,4]
# value: [1, 2, 3, 4]
```



## EasyLang


<lang>a[] = [ 1 2 3 ]
b[] = [ 4 5 6 ]
c[] = a[]
for i range len b[]
  c[] &= b[i]
.
```



## EchoLisp

The native operators are '''append''' for lists, and '''vector-append''' for vectors (1-dim arrays).

```scheme

;;;; VECTORS
(vector-append (make-vector 6 42) (make-vector 4 666))
    → #( 42 42 42 42 42 42 666 666 666 666)

;;;; LISTS
(append (iota 5) (iota 6))
   → (0 1 2 3 4 0 1 2 3 4 5)

;; NB - append may also be used with sequences (lazy lists)
(lib 'sequences)
   (take (append [1 .. 7] [7 6 .. 0]) #:all)
   → (1 2 3 4 5 6 7 6 5 4 3 2 1)



```



## ECL


<lang>
   A := [1, 2, 3, 4];
   B := [5, 6, 7, 8];

   C := A + B;
```



## Efene


using the ++ operator and the lists.append function


```efene

@public
run = fn () {
    A = [1, 2, 3, 4]
    B = [5, 6, 7, 8]

    C = A ++ B
    D = lists.append([A, B])

    io.format("~p~n", [C])
    io.format("~p~n", [D])
}
```



## EGL

{{works with|EDT}}

```EGL

program ArrayConcatenation
    function main()
        a int[] = [ 1, 2, 3 ];
	b int[] = [ 4, 5, 6 ];
	c int[];
	c.appendAll(a);
	c.appendAll(b);

	for (i int from 1 to c.getSize())
	    SysLib.writeStdout("Element " :: i :: " = " :: c[i]);
	end
    end
end

```



## Ela


```ela
xs = [1,2,3]
ys = [4,5,6]
xs ++ ys
```

{{out}}
```txt
[1,2,3,4,5,6]
```



## Elena

ELENA 4.1 :

```elena
import extensions;

public program()
{
    var a := new int[]::(1,2,3);
    var b := new int[]::(4,5);

    console.printLine(
        "(",a.asEnumerable(),") + (",b.asEnumerable(),
        ") = (",(a + b).asEnumerable(),")").readChar();
}
```



## Elixir


```elixir
iex(1)> [1, 2, 3] ++ [4, 5, 6]
[1, 2, 3, 4, 5, 6]
iex(2)> Enum.concat([[1, [2], 3], [4], [5, 6]])
[1, [2], 3, 4, 5, 6]
iex(3)> Enum.concat([1..3, [4,5,6], 7..9])
[1, 2, 3, 4, 5, 6, 7, 8, 9]
```



## Elm


```elm
import Element exposing (show, toHtml)  -- elm-package install evancz/elm-graphics
import Html.App exposing (beginnerProgram)
import Array exposing (Array, append, initialize)


xs : Array Int
xs =
  initialize 3 identity  -- [0, 1, 2]

ys : Array Int
ys =
  initialize 3 <| (+) 3  -- [3, 4, 5]

main = beginnerProgram { model = ()
                       , view = \_ -> toHtml (show (append xs ys))
                       , update = \_ _ -> ()
                       }

-- Array.fromList [0,1,2,3,4,5]
```



## Emacs Lisp

The ''vconcat'' function returns a new array containing all the elements of it's arguments.


```lisp
(vconcat '[1 2 3] '[4 5] '[6 7 8 9])
=> [1 2 3 4 5 6 7 8 9]
```



## Erlang


In erlang, you can use the ++ operator or lists:append, which is implemented via ++.

On the shell,

```erlang

1> [1, 2, 3] ++ [4, 5, 6].
[1,2,3,4,5,6]
2> lists:append([1, 2, 3], [4, 5, 6]).
[1,2,3,4,5,6]
3>

```



## ERRE

<lang>
PROGRAM ARRAY_CONCAT

DIM A[5],B[5],C[10]

!
! for rosettacode.org
!

BEGIN
  DATA(1,2,3,4,5)
  DATA(6,7,8,9,0)

  FOR I=1 TO 5 DO  ! read array A[.]
    READ(A[I])
  END FOR
  FOR I=1 TO 5 DO  ! read array B[.]
    READ(B[I])
  END FOR

  FOR I=1 TO 10 DO ! append B[.] to A[.]
    IF I>5 THEN
       C[I]=B[I-5]
     ELSE
       C[I]=A[I]
    END IF
    PRINT(C[I];)   ! print single C value
  END FOR

  PRINT

END PROGRAM

```



## Euphoria


```Euphoria
sequence s1,s2,s3
s1 = {1,2,3}
s2 = {4,5,6}
s3 = s1 & s2
? s3
```


{{out}}
 {1,2,3,4,5,6}

=={{header|F Sharp|F#}}==
Array concatenation.

```fsharp
let a = [|1; 2; 3|]
let b = [|4; 5; 6;|]
let c = Array.append a b
```

List concatenation (@ and List.append are equivalent).

```fsharp
let x = [1; 2; 3]
let y = [4; 5; 6]
let z1 = x @ y
let z2 = List.append x y
```



## FBSL

Array concatenation:

```qbasic
#APPTYPE CONSOLE

DIM aint[] ={1, 2, 3}, astr[] ={"one", "two", "three"}, asng[] ={!1, !2, !3}

FOREACH DIM e IN ARRAYMERGE(aint, astr, asng)
	PRINT e, " ";
NEXT

PAUSE
```

{{out}}

```txt
1 2 3 one two three 1.000000 2.000000 3.000000
Press any key to continue...
```



## Factor


```factor
append>
```


'''Example''':

```factor
( scratchpad ) USE: sequences
( scratchpad ) { 1 2 } { 3 4 } append .
{ 1 2 3 4 }
```



## Fantom


In fansh:


```fantom

> a := [1,2,3]
> b := [4,5,6]
> a.addAll(b)
> a
[1,2,3,4,5,6]

```


Note 'addAll' is destructive.  Write 'a.dup.addAll(b)' to create a fresh list.


## Forth


```Forth
: $!+   ( a u a' -- a'+u )
  2dup + >r swap move r> ;
: cat   ( a2 u2 a1 u1 -- a3 u1+u2 )
  align here dup >r $!+ $!+ r> tuck - dup allot ;

\ TEST
create a1 1 , 2 , 3 ,
create a2 4 , 5 ,
a2 2 cells a1 3 cells cat dump

8018425F0: 01 00 00 00  00 00 00 00 - 02 00 00 00  00 00 00 00  ................
801842600: 03 00 00 00  00 00 00 00 - 04 00 00 00  00 00 00 00  ................
801842610: 05 00 00 00  00 00 00 00 -                           ........

```



## Fortran

{{works with|Fortran|90 and later}}

```fortran
program Concat_Arrays
  implicit none

  ! Note: in Fortran 90 you must use the old array delimiters (/ , /)
  integer, dimension(3) :: a = [1, 2, 3] ! (/1, 2, 3/)
  integer, dimension(3) :: b = [4, 5, 6] ! (/4, 5, 6/)
  integer, dimension(:), allocatable :: c, d

  allocate(c(size(a)+size(b)))
  c(1 : size(a)) = a
  c(size(a)+1 : size(a)+size(b)) = b
  print*, c

  ! alternative
  d = [a, b] ! (/a, b/)
  print*, d
end program Concat_Arrays
```



## FreeBASIC


```freebasic

' FB 1.05.0 Win64

Sub ConcatArrays(a() As String, b() As String, c() As String)
   Dim aSize As Integer = UBound(a) - LBound(a) + 1
   Dim bSize As Integer = UBound(b) - LBound(b) + 1
   Dim cSize As Integer = aSize + bSize
   Redim c(0 To cSize - 1)
   Dim i As Integer
   For i = 0 To aSize - 1
     c(i) = a(LBound(a) + i)
   Next
   For i = 0 To bSize - 1
     c(UBound(a) + i + 1) = b(LBound(b) + i)
   Next
End Sub

Dim a(3) As String = {"The", "quick", "brown", "fox"}
Dim b(4) As String = {"jumped", "over", "the", "lazy", "dog"}
Dim c() As String
ConcatArrays(a(), b(), c())
For i As Integer = LBound(c) To UBound(c)
  Print c(i); " ";
Next
Print : Print
Print "Press any key to quit the program"
Sleep

```


{{out}}

```txt

The quick brown fox jumped over the lazy dog

```



## Free Pascal

Since FPC (Free Pascal compiler) version 3.2.0., the dynamic array concatenation operator <code>+</code> is available, provided <code>{$modeSwitch arrayOperators+}</code> (which is enabled by default in <code>{$mode Delphi}</code>).

```pascal
array0 + array1>
```



## Frink


```frink

a = [1,2]
b = [3,4]
a.pushAll[b]

```



## FunL


```funl
arr1 = array( [1, 2, 3] )
arr2 = array( [4, 5, 6] )
arr3 = array( [7, 8, 9] )

println( arr1 + arr2 + arr3 )
```


{{out}}


```txt

ArraySeq(1, 2, 3, 4, 5, 6, 7, 8, 9)

```



## Futhark


Array concatenation is done with the built-in function <code>concat</code>, which can take any number of arguments:


```Futhark

concat as bs cd

```



## Gambas

'''[https://gambas-playground.proko.eu/?gist=314bea6fba7f177a1cfaec8a7a8b5ccb Click this link to run this code]'''

```gambas
Public Sub Main()
Dim sString1 As String[] = ["The", "quick", "brown", "fox"]
Dim sString2 As String[] = ["jumped", "over", "the", "lazy", "dog"]

sString1.Insert(sString2)

Print sString1.Join(" ")

End
```

Output:

```txt

The quick brown fox jumped over the lazy dog

```



## GAP


```gap
# Concatenate arrays
Concatenation([1, 2, 3], [4, 5, 6], [7, 8, 9]);
# [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ]

# Append to a variable
a := [1, 2, 3];
Append(a, [4, 5, 6);
Append(a, [7, 8, 9]);
a;
# [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ]
```



## Genie


```genie
[indent=4]
/*
   Array concatenation, in Genie
   Tectonics: valac array-concat.gs
*/

/* Creates a new array */
def int_array_concat(x:array of int, y:array of int):array of int
    var a = new Array of int(false, true, 0)  /* (zero-terminated, clear, size) */
    a.append_vals (x, x.length)
    a.append_vals (y, y.length)

    z:array of int = (owned) a.data
    return z

def int_show_array(a:array of int)
    for element in a do stdout.printf("%d ", element)
    stdout.printf("\n")

init
    x: array of int = {1, 2, 3}
    y: array of int = {3, 2, 1, 0, -1}
    z: array of int = int_array_concat(x, y)

    stdout.printf("x: "); int_show_array(x)
    stdout.printf("y: "); int_show_array(y)
    stdout.printf("z: "); int_show_array(z)
    print "%d elements in new array", z.length
```


{{out}}

```txt
prompt$ valac array-concat.gs
prompt$ ./array-concat
x: 1 2 3
y: 3 2 1 0 -1
z: 1 2 3 3 2 1 0 -1
8 elements in new array
```



## Go


```go
package main

import "fmt"

func main() {
    // Example 1:  Idiomatic in Go is use of the append function.
    // Elements must be of identical type.
    a := []int{1, 2, 3}
    b := []int{7, 12, 60} // these are technically slices, not arrays
    c := append(a, b...)
    fmt.Println(c)

    // Example 2:  Polymorphism.
    // interface{} is a type too, one that can reference values of any type.
    // This allows a sort of polymorphic list.
    i := []interface{}{1, 2, 3}
    j := []interface{}{"Crosby", "Stills", "Nash", "Young"}
    k := append(i, j...) // append will allocate as needed
    fmt.Println(k)

    // Example 3:  Arrays, not slices.
    // A word like "array" on RC often means "whatever array means in your
    // language."  In Go, the common role of "array" is usually filled by
    // Go slices, as in examples 1 and 2.  If by "array" you really mean
    // "Go array," then you have to do a little extra work.  The best
    // technique is almost always to create slices on the arrays and then
    // use the copy function.
    l := [...]int{1, 2, 3}
    m := [...]int{7, 12, 60} // arrays have constant size set at compile time
    var n [len(l) + len(m)]int
    copy(n[:], l[:]) // [:] creates a slice that references the entire array
    copy(n[len(l):], m[:])
    fmt.Println(n)

}
```

{{out}}

```txt

[1 2 3 7 12 60]
[1 2 3 Crosby Stills Nash Young]
[1 2 3 7 12 60]

```

Array concatenation needs can vary.  Here is another set of examples that illustrate different techniques.

```go
package main

import (
  "reflect"
  "fmt"
)

// Generic version
// Easier to make the generic version accept any number of arguments,
// and loop trough them. Otherwise there will be lots of code duplication.
func ArrayConcat(arrays ...interface{}) interface{} {
  if len(arrays) == 0 {
    panic("Need at least one arguemnt")
  }
  var vals = make([]*reflect.SliceValue, len(arrays))
  var arrtype *reflect.SliceType
  var totalsize int
  for i,a := range arrays {
    v := reflect.NewValue(a)
    switch t := v.Type().(type) {
    case *reflect.SliceType:
      if arrtype == nil {
        arrtype = t
      } else if t != arrtype {
        panic("Unequal types")
      }
      vals[i] = v.(*reflect.SliceValue)
      totalsize += vals[i].Len()
    default: panic("not a slice")
    }
  }
  ret := reflect.MakeSlice(arrtype,totalsize,totalsize)
  targ := ret
  for _,v := range vals {
    reflect.Copy(targ, v)
    targ = targ.Slice(v.Len(),targ.Len())
  }
  return ret.Interface()
}

// Type specific version
func ArrayConcatInts(a, b []int) []int {
  ret := make([]int, len(a) + len(b))
  copy(ret, a)
  copy(ret[len(a):], b)
  return ret
}

func main() {
  test1_a, test1_b := []int{1,2,3}, []int{4,5,6}
  test1_c := ArrayConcatInts(test1_a, test1_b)
  fmt.Println(test1_a, " + ", test1_b, " = ", test1_c)

  test2_a, test2_b := []string{"a","b","c"}, []string{"d","e","f"}
  test2_c := ArrayConcat(test2_a, test2_b).([]string)
  fmt.Println(test2_a, " + ", test2_b, " = ", test2_c)
}
```

{{out}}

```txt

[1 2 3]  +  [4 5 6]  =  [1 2 3 4 5 6]
[a b c]  +  [d e f]  =  [a b c d e f]

```



## Gosu



```gosu

var listA = { 1, 2, 3 }
var listB = { 4, 5, 6 }

var listC = listA.concat( listB )

print( listC ) // prints [1, 2, 3, 4, 5, 6]

```



## Groovy

Solution:

```groovy
def list = [1, 2, 3] + ["Crosby", "Stills", "Nash", "Young"]
```


Test:

```groovy
println list>
```


{{out}}

```txt
[1, 2, 3, Crosby, Stills, Nash, Young]
```



## Haskell

A list is in Haskell one of the most common composite data types (constructed from other types). In the documentation we read for the append operation ++:

```haskell
(++) :: [a] -> [a] -> [a]
```

Append two lists, i.e.:
```txt

[x1, ..., xm] ++ [y1, ..., yn] == [x1, ..., xm, y1, ..., yn]
[x1, ..., xm] ++ [y1, ...] == [x1, ..., xm, y1, ...]
```

If the first list is not finite, the result is the first list.


## HicEst


```HicEst
REAL :: a(7), b(3), c(10)

c = a
DO i = 1, LEN(b)
   c(i + LEN(a)) = b(i)
ENDDO
```



## Hy


```hy
=
 (setv a [1 2 3])
=> a
[1, 2, 3]

=> (+ a [4 5 6]) ; returns the concatenation
[1, 2, 3, 4, 5, 6]
=> a
[1, 2, 3]

=> (.extend a [7 8 9]) ; modifies the list in place
=> a
[1, 2, 3, 7, 8, 9]

=> (+ [1 2] [3 4] [5 6]) ; can accept multiple arguments
[1, 2, 3, 4, 5, 6]
```



## i


```i
main
	a $= [1, 2, 3]
	b $= [4, 5, 6]

	print(a + b)
}
```


=={{header|Icon}} and {{header|Unicon}}==
Both languages have list concatenation built in. Lists are fully dynamic arrays which can be truncated or extended at either end.

```icon

procedure main()
    L1 := [1, 2, 3, 4]
    L2 := [11, 12, 13, 14]
    L3 := L1 ||| L2

    sep := ""
    every writes(sep, !L3) do
        sep := ", "
    write()
end

```



## IDL


Array concatenation can mean different things, depending on the number of dimensions of the arguments and the result. In the simplest case, with 1-dimensional arrays to begin with, there are two obvious ways to concatenate them. If my arrays are these:

```IDL

 > a = [1,2,3]
 > b = [4,5,6]
 > help,a
      A               INT       = Array[3]
 > help,b
      B               INT       = Array[3]
 > print,a
      1       2       3
 > print,b
      4       5       6

```

Then they can be concatenated "at the ends":

```IDL

 > help,[a,b]
      <Expression>    INT       = Array[6]
 > print,[a,b]
       1       2       3       4       5       6

```

or "at the sides":

```IDL

 > help,[[a],[b]]
      <Expression>    INT       = Array[3, 2]
 > print,[[a],[b]]
       1       2       3
       4       5       6

```

Note that this requires that the arrays have the same size at the side at which they are concatenated:

```IDL

 > b = transpose(b)
 > help,b
      B               INT       = Array[1, 3]
 > print,b
       4
       5
       6
 > print,[a,b]
 Unable to concatenate variables because the dimensions do not agree: B.
 Execution halted at: $MAIN$
 > print,[[a],[b]]
 Unable to concatenate variables because the dimensions do not agree: B.
 Execution halted at: $MAIN$

```

This can get a lot more complicated as a 3x4x5-element three-dimensional array can be concatenated with a 5x2x3-element array at exactly two "surfaces".


## Idris

Idris will disambiguate functions based on type, so both <code>List</code> (arbitrary length) and <code>Vect</code> (fixed length) can be concatenated in the same way:

```txt
Idris> [1, 2] ++ [4, 5, 6]
[1, 2, 3, 4, 5] : List Integer
Idris> :module Data.Vect
*Data/Vect> (the (Vect 2 Nat) [1, 2]) ++ (the (Vect 3 Nat) [3, 4, 5])
[1, 2, 3, 4, 5] : Vect 5 Nat
```



## Inform 7


```inform7
let A be {1, 2, 3};
let B be {4, 5, 6};
add B to A;
```



## Ioke


```ioke
iik>
 [1,2,3] + [3,2,1]
[1,2,3] + [3,2,1]
+> [1, 2, 3, 3, 2, 1]
```



## J

'''Solution''':   <code> , </code>

'''Example''':

```j
   array1 =: 1 2 3
   array2 =: 4 5 6
   array1 , array2
1 2 3 4 5 6
```


Of course, in J, array concatenation works (consistently) on arrays of any rank or dimension.

The verb <code>,</code> concatenates by treating the argument array with the largest number of dimensions as a list. Other primary verbs concatenate along other axes.


```j
   ]ab=: 3 3 $ 'aaabbbccc'
aaa
bbb
ccc
   ]wx=: 3 3 $ 'wxyz'
wxy
zwx
yzw
   ab , wx
aaa
bbb
ccc
wxy
zwx
yzw
   ab ,. wx
aaawxy
bbbzwx
cccyzw
   ab ,: wx
aaa
bbb
ccc

wxy
zwx
yzw
   $ ab , wx    NB. applies to first (highest) axis
6 3
   $ ab ,. wx   NB. applies to last (atomic) axis
3 6
   $ ab ,: wx   NB. applies to new (higher) axis
2 3 3
```



## Java


```java5
public static Object[] concat(Object[] arr1, Object[] arr2) {
    Object[] res = new Object[arr1.length + arr2.length];

    System.arraycopy(arr1, 0, res, 0, arr1.length);
    System.arraycopy(arr2, 0, res, arr1.length, arr2.length);

    return res;
}
```



## JavaScript

The <code>Array.concat()</code> method returns a new array comprised of this array joined with other array(s) and/or value(s).

```javascript
var a = [1,2,3],
    b = [4,5,6],
    c = a.concat(b); //=> [1,2,3,4,5,6]
```



Or, if we consider the concatenation of two arrays as a particular instance of the more general problem of concatenating 2 or more arrays, we can write a generic function:

{{trans|Haskell}}
See, for a function with an analogous type signature, '''concat''' in the Haskell Prelude.


```javascript
(function () {
    'use strict';

    // concat :: [[a]] -> [a]
    function concat(xs) {
        return [].concat.apply([], xs);
    }


   return concat(
      [["alpha", "beta", "gamma"],
      ["delta", "epsilon", "zeta"],
      ["eta", "theta", "iota"]]
  );

})();
```


{{Out}}


```txt
["alpha", "beta", "gamma", "delta", "epsilon", "zeta", "eta", "theta", "iota"]
```



## jq

If a and b are two arrays, then a+b is their concatenation.
Similarly for a+b+c.

To concatenate the component arrays of an array, A, the <tt>add</tt> filter can be used: <tt>A|add</tt>

jq also supports streams, which are somewhat array-like, so it may be worth mentioning that the concatenation of two or more streams can be accomplished using "," instead of "+".
```jq
[1,2] + [3] + [null] # => [1,2,3,null]

[range(1;3), 3, null] # => [1,2,3,null]

```



## Julia


```julia
a = [1,2,3]
b = [4,5,6]
ab = [a;b]
# the above bracket notation simply generates a call to vcat
ab = vcat(a,b)
# hcat is short for `horizontal concatenation`
ab = hcat(a,b) 	#ab -> 3x2 matrix
# the append!(a,b) method is mutating, appending `b` to `a`
append!(a,b)	# a now equals [1,2,3,4,5,6]
```



## K


```K

    a: 1 2 3
    b: 4 5 6
    a,b
1 2 3 4 5 6
```


Concatenations on larger dimensions also use ",", often combined with other operations.


```K

   ab:3 3#"abcdefghi"
("abc"
 "def"
 "ghi")

   dd:3 3#"012345678"
("012"
 "345"
 "678")

   ab,dd
("abc"
 "def"
 "ghi"
 "012"
 "345"
 "678")

   +ab,dd   / flip (transpose) join
("adg036"
 "beh147"
 "cfi258")

   ab,'dd   / eachpair join
("abc012"
 "def345"
 "ghi678")

   +(+ab),dd
("abc036"
 "def147"
 "ghi258")
```



## Klong


```K

    [1 2 3],[4 5 6]               :" join "
[1 2 3 4 5 6]

    [1 2],:\[[3 4] [5 6] [7 8]]   :" join each-left "
[[1 2 3 4] [1 2 5 6] [1 2 7 8]]

    [1 2],:/[[3 4] [5 6] [7 8]]   :" join each-right "
[[3 4 1 2] [5 6 1 2] [7 8 1 2]]

```



## Kotlin

There is no operator or standard library function for concatenating <code>Array</code> types. One option is to convert to <code>Collection</code>s, concatenate, and convert back:

```kotlin
fun main(args: Array<String>) {
    val a: Array<Int> = arrayOf(1, 2, 3) // initialise a
    val b: Array<Int> = arrayOf(4, 5, 6) // initialise b
    val c: Array<Int> = (a.toList() + b.toList()).toTypedArray()
    println(c)
}
```


Alternatively, we can write our own concatenation function:

```kotlin
fun arrayConcat(a: Array<Any>, b: Array<Any>): Array<Any> {
    return Array(a.size + b.size, { if (it in a.indices) a[it] else b[it - a.size] })
}
```


When working directly with <code>Collection</code>s, we can simply use the <code>+</code> operator:

```kotlin
fun main(args: Array<String>) {
    val a: Collection<Int> = listOf(1, 2, 3) // initialise a
    val b: Collection<Int> = listOf(4, 5, 6) // initialise b
    val c: Collection<Int> = a + b
    println(c)
}
```



## LabVIEW

Use the Build Array function.<br/>{{VI snippet}}<br/>
[[File:LabVIEW_Array_concatenation.png]]


## Lang5


```Lang5
[1 2] [3 4] append collapse .
```




## Lasso


```Lasso

local(arr1 = array(1, 2, 3))
local(arr2 = array(4, 5, 6))
local(arr3 = #arr1->asCopy)	//	make arr3 a copy of arr2
#arr3->merge(#arr2)		//	concatenate 2 arrays


Result:

arr1 = array(1, 2, 3)
arr2 = array(4, 5, 6)
arr3 = array(4, 5, 6)
arr3 = array(1, 2, 3, 4, 5, 6)
```



## LFE


```lisp

> (++ '(1 2 3) '(4 5 6))
(1 2 3 4 5 6)
> (: lists append '(1 2 3) '(4 5 6))
(1 2 3 4 5 6)

```



## Liberty BASIC


```lb
    x=10
    y=20
    dim array1(x)
    dim array2(y)

[concatenate]
    dim array3(x + y)
    for i = 1 to x
        array3(i) = array1(i)
    next
    for i = 1 to y
        array3(i + x) = array2(i)
    next

[print]
    for i = 1 to x + y
        print array3(i)
    next
```



## LIL

LIL uses lists instead of arrays.  The builtin '''append''' command could be used as '''append a $b'''. That would add the entire list in variable '''b''' as one item to list '''a'''.  Below '''quote''' is used to flatten the lists into a single new list of all items.


```tcl
##
  Array concatenation in LIL
##
set a [list 1 2 3]
set b [list 4 5 6]
set c [quote $a $b]

print $c
print "[index $c 0] [index $c 3]"
```


{{out}}

```txt
prompt$ lil arrayConcatenation.lil
1 2 3 4 5 6
1 4
```



## Limbo


```limbo
implement Command;

include "sys.m";
sys: Sys;

include "draw.m";

include "sh.m";

init(nil: ref Draw->Context, nil: list of string)
{
	sys = load Sys Sys->PATH;

	a := array[] of {1, 2};
	b := array[] of {3, 4, 5};

	c := array[len a + len b] of int;
	c[:] = a;
	c[len a:] = b;

	for (i := 0; i < len c; i++)
		sys->print("%d\n", c[i]);
}
```



## Lingo


```lingo
a = [1,2]
b = [3,4,5]

repeat with v in b
  a.append(v)
end repeat

put a
-- [1, 2, 3, 4, 5]
```



## Little


```C
void main() {
    int a[] = {0, 1, 2, 3, 4};
    int b[] = {5, 6, 7, 8, 9};
    int c[] = {(expand)a, (expand)b};
    puts(c);
}
```



## Logo

COMBINE is used to combine lists or words. SENTENCE is used to combine lists and words into a single list.

```logo

to combine-arrays :a1 :a2
  output listtoarray sentence arraytolist :a1 arraytolist :a2
end
show combine-arrays {1 2 3} {4 5 6}   ; {1 2 3 4 5 6}

```



## Lua


```lua
a = {1, 2, 3}
b = {4, 5, 6}

for _, v in pairs(b) do
    table.insert(a, v)
end

print(table.concat(a, ", "))
```

{{out}}

```txt

1, 2, 3, 4, 5, 6

```



## Maple

There is a built-in procedure for concatenating arrays (and similar objects such as matrices or vectors).  Arrays can be concatenated along any given dimension, which is specified as the first argument.

```Maple

> A := Array( [ 1, 2, 3 ] );
                             A := [1, 2, 3]

> B := Vector['row']( [ sin( x ), cos( x ), tan( x ) ] );
                     B := [sin(x), cos(x), tan(x)]

> ArrayTools:-Concatenate( 1, A, B ); # stack vertically
                      [  1         2         3   ]
                      [                          ]
                      [sin(x)    cos(x)    tan(x)]

> ArrayTools:-Concatenate( 2, A, B ); # stack horizontally
                   [1, 2, 3, sin(x), cos(x), tan(x)]

> M := << a, b, c ; d, e, f >>; # a matrix
                                [a    b    c]
                           M := [           ]
                                [d    e    f]

> ArrayTools:-Concatenate( 1, M, A );
                             [a    b    c]
                             [           ]
                             [d    e    f]
                             [           ]
                             [1    2    3]

```

Of course, the order of the arguments is important.

```Maple

> ArrayTools:-Concatenate( 1, A, M );
                             [1    2    3]
                             [           ]
                             [a    b    c]
                             [           ]
                             [d    e    f]

```

Lists, in Maple, might be considered to be a kind of "array" (in the sense that they look like arrays in memory), though they are actually immutable objects.  However, they can be concatenated as follows.

```Maple

> L1 := [ 1, 2, 3 ];
                            L1 := [1, 2, 3]

> L2 := [ a, b, c ];
                            L2 := [a, b, c]

> [ op( L1 ), op( L2 ) ];
                           [1, 2, 3, a, b, c]

> [ L1[], L2[] ]; # equivalent, just different syntax
                           [1, 2, 3, a, b, c]

```



## M2000 Interpreter


```M2000 Interpreter

a=(1,2,3,4,5)
b=Cons(a, (6,7,8),a)
Print b
1 2 3 4 5 6 7 8 1 2 3 4 5

```


Adding 2 dimension arrays


```M2000 Interpreter

Dim Base 0, A(2,2)=1, B(1,2)=6
A()=Cons(A(), B(), A(), B())
\\ Restore the dimensions (without erasing items)
Dim A(Dimension(A(),1)/2, 2)
For I=0 to Dimension(A(),1)-1 {
      For j=0 to Dimension(A(),2)-1 {
            Print A(i, j),
      }
      Print
}

```

{{out}}

```txt

    1    1
    1    1
    6    6
    1    1
    1    1
    6    6
</pre >

=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
Join[{1,2,3}, {4,5,6}]

-> {1, 2, 3, 4, 5, 6}
```


=={{header|MATLAB}} / {{header|Octave}}==
Two arrays are concatenated by placing the two arrays between a pair of square brackets. A space between the two array names will concatenate them horizontally, and a semi-colon between array names will concatenate vertically.

```matlab
>
 a = [1 2 3]

a =

     1     2     3

>> b = [4 5 6]

b =

     4     5     6

>> concat = [a b]

concat =

     1     2     3     4     5     6

>> concat = [a;b]

concat =

     1     2     3
     4     5     6
```


For multi-dimensional arrays, there is also the function cat():

```matlab
>
 c = randn([3,4,5]);
>> d = randn([3,4,7]);
>> e = cat(3,c,d);
>> size(e)
   ans =

    3    4   12


```



## Maxima

<lang>u: [1, 2, 3, 4]$
v: [5, 6, 7, 8, 9, 10]$
append(u, v);
/* [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] */

/* There are also functions for matrices */

a: matrix([6, 1, 8],
          [7, 5, 3],
          [2, 9, 4])$

addcol(a, ident(3));
/* matrix([6, 1, 8, 1, 0, 0],
          [7, 5, 3, 0, 1, 0],
          [2, 9, 4, 0, 0, 1]) */

addrow(a, ident(3));
/* matrix([6, 1, 8],
          [7, 5, 3],
          [2, 9, 4],
          [1, 0, 0],
          [0, 1, 0],
          [0, 0, 1]) */
```



## Mercury



```Mercury
A `append` B = C
```


It ''could'' be "as simple as array1 + array2", but the 'array' module names the operation 'append' rather than '+'.  It's tempting to just say that Mercury supports ad-hoc polymorphism - it can infer that a bare '+' refers to 'float.+' or 'int.+' (or that the 'append' above is array.append, rather than list.append), by the types involved - but it also handles other ambiguities in the same way.  For instance, Mercury (like Prolog and Erlang) treats the arity of a function as part of its name, where ''a(1, 2)'' and ''a(1)'' involve the distinct functions a/2 and a/1.  But Mercury also (unlike Prolog and Erlang) supports [[currying]], where ''a(1)'' is a function that accepts a/2's second argument.  So, is ''[a(X), a(Y), a(Z)]'' a list of whatever type a/1 evaluates to, or is it a list of curried a/2?


## min

{{works with|min|0.19.3}}

```min
(1 2 3) (4 "apple" 6) concat print
```

{{out}}

```txt

(1 2 3 4 "apple" 6)

```



## MiniScript


```MiniScript

arrOne = [1, 2, 3]
arrTwo = [4, 5, 6]
print arrOne + arrTwo

```



## Neko


```ActionScript
/*
 Array concatenation, in Neko
*/

var a1 = $array(1,2,3,4)
var a2 = $array("abc", "def")

/* $array(a1, a2) creates an array of two arrays, $aconcat merges to one */
var ac = $aconcat($array(a1, a2))
$print(ac, "\n")
```


{{out}}

```txt
prompt$ nekoc array-concatenation.neko
prompt$ neko array-concatenation.n
[1,2,3,4,abc,def]
```



## Nemerle


```Nemerle
using System.Console;
using Nemerle.Collections;

module ArrayCat
{
    Main() : void
    {
        def arr1 = array[1, 2, 3]; def arr2 = array[4, 5, 6];
        def arr12 = arr1.Append(arr2);                       // <----
        foreach (i in arr12) Write($"$i  ");
    }
}
```



## NetRexx

NetRexx arrays are identical to [[Java|Java's]] so all the techniques described in the [[#Java|Java]] section apply to NetRexx too.  This example uses the <tt>Collection</tt> classes to merge two arrays.

```netrexx
/* NetRexx */
options replace format comments java crossref nobinary

cymru =  [ 'Ogof Ffynnon Ddu', 'Ogof Draenen' ]

dlm = '-'.copies(40)

say dlm
loop c_ = 0 to cymru.length - 1
  say c_ cymru[c_]
  end c_

yorks = [ 'Malham Tarn Pot', 'Greygill Hole' ]

say dlm
loop y_ = 0 to yorks.length - 1
  say y_ yorks[y_]
  end y_

merge = ArrayList()
merge.addAll(Arrays.asList(cymru))
merge.addAll(Arrays.asList(yorks))

say dlm
merged = merge.toArray()
loop m_ = 0 to merged.length - 1
  say m_ merged[m_]
  end m_
```

{{out}}

```txt

----------------------------------------
0 Ogof Ffynnon Ddu
1 Ogof Draenen
----------------------------------------
0 Malham Tarn Pot
1 Greygill Hole
----------------------------------------
0 Ogof Ffynnon Ddu
1 Ogof Draenen
2 Malham Tarn Pot
3 Greygill Hole

```



## NewLISP


```NewLISP
; file:   arraycon.lsp
; url:    http://rosettacode.org/wiki/Array_concatenation
; author: oofoe 2012-01-28

(println "Append lists:  " (append '(3 a 5 3) (sequence 1 9)))

(println "Multi append:  "
         (append '(this is)
                 '(a test)
                 '(of the emergency)
                 (sequence 3 1)))

(println "Append arrays: "
         (append '((x 56) (b 99)) '((z 34) (c 23) (r 88))))

(exit)
```


{{out}}

```txt
Append lists:  (3 a 5 3 1 2 3 4 5 6 7 8 9)
Multi append:  (this is a test of the emergency 3 2 1)
Append arrays: ((x 56) (b 99) (z 34) (c 23) (r 88))

```



## Nial

Examples tested to work with Q'Nial7


```Nial
    a:= 1 2 3
+-+-+-+
|1|2|3|
+-+-+-+
    b:= 4 5 6
+-+-+-+
|4|5|6|
+-+-+-+
```


Table of lists:


```Nial
    a b

+-------+-------+
|+-+-+-+|+-+-+-+|
||1|2|3|||4|5|6||
|+-+-+-+|+-+-+-+|
+-------+-------+
```


Simple concatenation of two arrays/lists:


```Nial
    link a b
+-+-+-+-+-+-+
|1|2|3|4|5|6|
+-+-+-+-+-+-+
```


Convert list of lists to table:


```Nial
    mix a b
+-+-+-+
|1|2|3|
+-+-+-+
|4|5|6|
+-+-+-+
```


Interchange levels of a list of lists:

```Nial
    pack a b
+-----+-----+-----+
|+-+-+|+-+-+|+-+-+|
||1|4|||2|5|||3|6||
|+-+-+|+-+-+|+-+-+|
+-----+-----+-----+
```



## Nim

Dynamic sized Sequences can simply be concatenated:

```nim
var
  x = @[1,2,3,4,5,6]
  y = @[7,8,9,10,11]
  z = x & y
```


Static sized Arrays:

```nim
var
  a = [1,2,3,4,5,6]
  b = [7,8,9,10,11]
  c: array[11, int]

c[0..5] = a
c[6..10] = b
```


=={{header|Oberon-2}}==

```oberon2

MODULE ArrayConcat;
IMPORT
  Out;
TYPE
  IntArray = POINTER TO ARRAY OF INTEGER;
VAR
  x, y, z: IntArray;

  PROCEDURE InitArray(VAR x: IntArray;from: INTEGER);
  VAR
    i: LONGINT;
  BEGIN
    FOR i := 0 TO LEN(x^) - 1 DO
      x[i] := from;
      INC(from)
    END
  END InitArray;

  PROCEDURE Concat(x,y: IntArray; VAR z: IntArray);
  VAR
    i: LONGINT;
  BEGIN
    ASSERT(LEN(x^) + LEN(y^) <= LEN(z^));
    FOR i := 0 TO LEN(x^) - 1 DO z[i] := x[i] END;
    FOR i := 0 TO LEN(y^) - 1 DO z[i + LEN(x^)] := y[i] END
  END Concat;

  PROCEDURE Show(x: IntArray);
  VAR
    i: INTEGER;
  BEGIN
    i := 0;
    Out.Char('[');
    WHILE (i < LEN(x^)) DO
      Out.LongInt(x[i],3);IF i < LEN(x^) - 1 THEN Out.Char(',') END;
      INC(i)
    END;
    Out.Char(']');Out.Ln
  END Show;

BEGIN
  (* Standard types *)
  NEW(x,5);InitArray(x,1);
  NEW(y,10);InitArray(y,6);
  NEW(z,LEN(x^) + LEN(y^));

  Concat(x,y,z);
  Show(z)

END ArrayConcat.

```

{{out}}

```txt

[  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15]

```



## Objeck


```objeck

bundle Default {
  class Arithmetic {
     function : Main(args : String[]) ~ Nil {
       array1 := [3, 5, 7];
       array2 := [2, 4, 6];

       array3 := Copy(array1, array2);
       each(i : array3) {
         array3[i]->PrintLine();
       };
  }

  function : native : Copy(array1 : Int[], array2 : Int[]) ~ Int[] {
     max := array1->Size() + array2->Size();
     array3 := Int->New[max];

     i := 0;
     for(i := i; i < array1->Size(); i += 1;) {
       array3[i] := array1[i];
     };

     j := 0;
     for(i := i; i < max; i += 1;) {
       array3[i] := array2[j];
       j += 1;
     };

      return array3;
    }
  }
}

```


=={{header|Objective-C}}==
with immutable arrays:

```objc
NSArray *arr1 = @[@1, @2, @3];
NSArray *arr2 = @[@4, @5, @6];
NSArray *arr3 = [arr1 arrayByAddingObjectsFromArray:arr2];
```


or adding onto a mutable array:

```objc
NSArray *arr1 = @[@1, @2, @3];
NSArray *arr2 = @[@4, @5, @6];
NSMutableArray *arr3 = [NSMutableArray arrayWithArray:arr1];
[arr3 addObjectsFromArray:arr2];
```



## OCaml

It is more natural in OCaml to use lists instead of arrays:

```ocaml
# let list1 = [1; 2; 3];;
val list1 : int list = [1; 2; 3]
# let list2 = [4; 5; 6];;
val list2 : int list = [4; 5; 6]
# let list1and2 = list1 @ list2;;
val list1and2 : int list = [1; 2; 3; 4; 5; 6]
```


If you want to use arrays:

```ocaml
# let array1 = [|1; 2; 3|];;
val array1 : int array = [|1; 2; 3|]
# let array2 = [|4; 5; 6|];;
val array2 : int array = [|4; 5; 6|]
# let array1and2 = Array.append array1 array2;;
val array1and2 : int array = [|1; 2; 3; 4; 5; 6|]
```



## Oforth



```Oforth
import: mapping

[1, 2, 3 ] [ 4, 5, 6, 7 ] +
```



## Onyx



```onyx
# With two arrays on the stack, cat pops
# them, concatenates them, and pushes the result back
# on the stack. This works with arrays of integers,
# strings, or whatever. For example,

[1 2 3] [4 5 6] cat # result: [1 2 3 4 5 6]
[`abc' `def'] [`ghi' `jkl'] cat # result: [`abc' `def' `ghi' `jkl']

# To concatenate more than two arrays, push the number of arrays
# to concatenate onto the stack and use ncat. For example,

[1 true `a'] [2 false `b'] [`3rd array'] 3 ncat
# leaves [1 true `a' 2 false `b' `3rd array'] on the stack
```



## ooRexx


```ooRexx
a = .array~of(1,2,3)
say "Array a has " a~items "items"
b = .array~of(4,5,6)
say "Array b has " b~items "items"
a~appendall(b)        -- adds all items from b to a
say "Array a now has " a~items "items"
```

{{out}}

```txt
Array a has  3 items
Array b has  3 items
Array a now has  6 items
```



## Order

Order supports two main aggregate types: tuples and sequences (similar to lists in other languages). Most "interesting" operations are limited to sequences, but both support an append operation, and each can be converted to the other.

```c
#include <order/interpreter.h>

ORDER_PP( 8tuple_append(8tuple(1, 2, 3), 8tuple(4, 5, 6), 8pair(7, 8)) )
// -> (1,2,3,4,5,6,7,8)

ORDER_PP( 8seq_append(8seq(1, 2, 3), 8seq(4, 5, 6), 8seq(7, 8)) )
// -> (1)(2)(3)(4)(5)(6)(7)(8)
```



## OxygenBasic


```oxygenbasic

'CREATE DYNAMIC ARRAY SPACES USING STRINGS

string sa=nuls 5* sizeof float
string sb=sa

'MAP ARRAY VARIABLES ONTO STRINGS

float a at *sa
float b at *sb

'ASSIGN SOME VALUES

a<=10,20,30,40,50
b<=60,70,80,90,00

'ADD ARRAY B TO A BY STRING CONCATENATION

sa+=sb

'TEST

print a[7] 'result 70

```


## Oz

List are concatenated with <code>List.append</code> (shortcut: <code>Append</code>). Tuples are concatened with <code>Tuple.append</code>. Arrays do exist in Oz, but are rarely used.

```oz
%% concatenating 2 lists
{Append [a b] [c d]} = [a b c d]

%% concatenating 2 tuples
{Tuple.append t(1 2 3) u(4 5 6)} = u(1 2 3 4 5 6)
```



## PARI/GP


```parigp
concat(u,v)
```



## Pascal

''See [[#Delphi|Delphi]] and [[#Free Pascal|Free Pascal]]''


## Perl

In Perl, arrays placed into list context are flattened:

```perl
my @arr1 = (1, 2, 3);
my @arr2 = (4, 5, 6);
my @arr3 = (@arr1, @arr2);
```


The <code>[http://perldoc.perl.org/functions/push.html push]</code> function appends elements onto an existing array:

```perl
my @arr1 = (1, 2, 3);
my @arr2 = (4, 5, 6);
push @arr1, @arr2;
print "@arr1\n"; # prints "1 2 3 4 5 6"
```



## Perl 6

{{works with|Rakudo|2018.06}}

```perl6
my @array1 = 1, 2, 3;
my @array2 = 4, 5, 6;

# If you want to concatenate two array to form a third,
# either use the slip operator "|", to flatten each array.

my @array3 = |@array1, |@array2;
say @array3;

# or just flatten both arrays in one fell swoop

@array3 = flat @array1, @array2;
say @array3;

# On the other hand, if you just want to add the elements
# of the second array to the first, use the .append method.

say @array1.append: @array2;
```

{{Out}}

```txt
[1 2 3 4 5 6]
[1 2 3 4 5 6]
[1 2 3 4 5 6]
```



## Phix


```Phix
sequence s1 = {1,2,3}, s2 = {4,5,6}
? s1 & s2
```

{{out}}

```txt

 {1,2,3,4,5,6}

```



## PHP



```php
$arr1 = array(1, 2, 3);
$arr2 = array(4, 5, 6);
$arr3 = array_merge($arr1, $arr2);
```



## PicoLisp

PicoLisp has no built-in array data type. Lists are used instead.

There are destructive concatenations:

```PicoLisp
: (setq  A (1 2 3)  B '(a b c))
-> (a b c)
: (conc A B)                        # Concatenate lists in 'A' and 'B'
-> (1 2 3 a b c)
: A
-> (1 2 3 a b c)                    # Side effect: List in 'A' is modified!
```

and non-destructive concatenations:

```PicoLisp
: (setq  A (1 2 3)  B '(a b c))
-> (a b c)
: (append A B)                      # Append lists in 'A' and 'B'
-> (1 2 3 a b c)
: A
-> (1 2 3)
: B
-> (a b c)                          # Arguments are not modified
```



## PL/I

Trivial example requires no computational statement.
Note that the arrays are not in static storage:

```PL/I

   declare x(12) fixed;
   declare b(5) fixed defined x;
   declare c(7) fixed defined x(1sub+5);

```

A more general example using dynamic bounds.
Again, no computation statement is required.
<lang>
   declare x(m+n) fixed;
   declare b(m) fixed defined x;
   declare c(n) fixed defined x(1sub+hbound(b,1));

```


An alternative, that can be used to advantage for matrices
as well as vectors, follows.  This example illustrates
extending a matrix diagonally.  Although fixed array bounds
are used in the declarations, the bounds can be dynamic.
Matrix B is extended by placing matrix C on its diagonal:
<lang>
   declare a(5,6) fixed;
   declare b(3,4) fixed defined a(1sub, 2sub);
   declare c(2,2) fixed defined a(1sub+hbound(b,1), 2sub+hbound(b,2));
   declare (i, j, k) fixed;

   a = 0;
   put skip list ('Please type elements for a 3 x 4 matrix:');
   get list (b);
   put skip list ('Please type elements for a 2 x 2 matrix:');
   get list (c);
   put skip edit (c) ( skip, (hbound(c,2)) f(5,0) );

   put skip list ('Composite matrix:');
   put skip edit (a) ( skip, (hbound(a,2)) f(5,0) );

```

{{out}}
<lang>
Please type elements for a 3 x 4 matrix:

Please type elements for a 2 x 2 matrix:

   13   14
   15   16
Composite matrix:

    1    2    3    4    0    0
    5    6    7    8    0    0
    9   10   11   12    0    0
    0    0    0    0   13   14
    0    0    0    0   15   16


```



## Pony


```pony

actor Main
    new create(env:Env)=>
        var a:Array[I32]=Array[I32](4)
        var b:Array[I32]=Array[I32](2)
        a.push(1)
        a.push(2)
        a.push(3)
        a.push(4)
        b.push(5)
        b.push(6)
        a.concat(b.values())
        for i in a.values() do
            env.out.print(i.string())
        end

```



## PostScript

{{libheader|initlib}}

```postscript

[1 2 3 4] [5 6 7 8] concat

```



## PowerShell


```powershell
$a = 1,2,3
$b = 4,5,6

$c = $a + $b
Write-Host $c
```



## Prolog


```prolog

?- append([1,2,3],[4,5,6],R).
R = [1, 2, 3, 4, 5, 6].

```



## PureBasic


```PureBasic
Procedure displayArray(Array a(1), msg.s)
  Protected i
  Print(msg + " [")
  For i = 0 To ArraySize(a())
    Print(Str(a(i)))
    If i <> ArraySize(a())
      Print(", ")
    EndIf
  Next
  PrintN("]")
EndProcedure

Procedure randomElements(Array a(1), lo, hi)
  Protected i
  For i = 0 To ArraySize(a())
    a(i) = random(hi - lo) + lo
  Next
EndProcedure

Procedure arrayConcat(Array a(1), Array b(1), Array c(1))
  Protected i, newSize = ArraySize(a()) + ArraySize(b()) + 1
  Dim c(newSize)
  For i = 0 To ArraySize(a())
    c(i) = a(i)
  Next
  For i = 0 To ArraySize(b())
    c(i + ArraySize(a()) + 1) = b(i)
  Next
EndProcedure


If OpenConsole()
  Dim a(random(3) + 1)
  Dim b(random(3) + 1)
  Dim c(0) ;array will be resized by arrayConcat()

  randomElements(a(), -5, 5)
  randomElements(b(), -5, 5)
  displayArray(a(), "a:")
  displayArray(b(), "b:")
  arrayConcat(a(), b(), c())
  displayArray(c(), "concat of a[] + b[]:")

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit")
  Input()
  CloseConsole()
EndIf
```

{{out}}

```txt
a: [5, 2, -4, -1, -2]
b: [0, -4, -1]
concat of a[] + b[]: [5, 2, -4, -1, -2, 0, -4, -1]
```



## Python

The <code>[http://docs.python.org/library/stdtypes.html#sequence-types-str-unicode-list-tuple-buffer-xrange +]</code> operator concatenates two lists and returns a new list.
The <code>[http://docs.python.org/library/stdtypes.html#mutable-sequence-types list.extend]</code> method appends elements of another list to the receiver.

```python
arr1 = [1, 2, 3]
arr2 = [4, 5, 6]
arr3 = [7, 8, 9]
arr4 = arr1 + arr2
assert arr4 == [1, 2, 3, 4, 5, 6]
arr4.extend(arr3)
assert arr4 == [1, 2, 3, 4, 5, 6, 7, 8, 9]
```


Note: list.extend is normally accomplished using the += operator like this:

```python
arr5 = [4, 5, 6]
arr6 = [7, 8, 9]
arr6 += arr5
assert arr6 == [7, 8, 9, 4, 5, 6]
```



## Q


```q
list1:1 2 3
list2:4 5 6
list1,list2
```



## R



```R

a1 <- c(1, 2, 3)
a2 <- c(3, 4, 5)
a3 <- c(a1, a2)

```



## Racket


```racket

(vector-append #(1 2 3 4) #(5 6 7) #(8 9 10))

```

{{out}}

```txt

'#(1 2 3 4 5 6 7 8 9 10)

```



## RapidQ


```vb

DEFINT A(1 to 4) = {1, 2, 3, 4}
DEFINT B(1 to 4) = {10, 20, 30, 40}

'Append array B to array A
Redim A(1 to 8) as integer
MEMCPY(varptr(A(5)), varptr(B(1)), Sizeof(integer)*4)

```



## REBOL


```REBOL

a1: [1 2 3]
a2: [4 5 6]
a3: [7 8 9]

append a1 a2 ; -> [1 2 3 4 5 6]

append/only a1 a3 ; -> [1 2 3 4 5 6 [7 8 9]]

```



## Red


```Red
>>
 arr1: ["a" "b" "c"]
>> arr2: ["d" "e" "f"]
>> append arr1 arr2
== ["a" "b" "c" "d" "e" "f"]
>> arr3: [1 2 3]
>> insert arr1 arr3
>> arr1
== [1 2 3 "a" "b" "c" "d" "e" "f"]
>> arr4: [22 33 44]
== [22 33 44]
>> append/only arr1 arr4
== [1 2 3 "a" "b" "c" "d" "e" "f" [22 33 44]]
```



## Retro


```Retro
needs array'

^array'new{ 1 2 3 }  ^array'new{ 4 5 6 }  ^array'append
```



## REXX

REXX doesn't have arrays as such, but it has something that looks, feels, and tastes like arrays:
::::* stemmed variables

Simply, a stemmed array is a variable with an appended dot ('''.''') followed by a symbol (it's normally an integer or an alphanumeric name).

There is no way to preallocate a stemmed variable, REXX just assigns them as they are created (assigned a value).


As such, there isn't an easy way to keep track of the number of "elements" in a REXX "array"   (unless the programmer maintains a list).


Consider:

```rexx
a.1 =  10
a.2 =  22.7
a.7 = -12
```

where now we have three "elements", and they are disjointed (another word for ''sparse'').

There are ways to handle this in REXX however.


When assigning stemmed arrays, it is common to assign "element" zero to the number of values,

assuming that the stemmed variables are sequential.


'''example:'''

```rexx
fact.0=8
fact.1=    1
fact.2=    2
fact.3=    6
fact.4=   24
fact.5=  120
fact.6=  720
fact.7= 5040
fact.8=40320
```

To concat two "arrays" in REXX, the following assumes that the stemmed variables are in order, with no gaps, and none have a "null" value.

```rexx
/*REXX program to demonstrates how to perform array concatenation.*/

p.=                                    /*(below) a short list of primes.*/
p.1=2;    p.2=3;     p.3=5;     p.4=7;     p.5=11;   p.6=13
p.7=17;   p.8=19;    p.9=23;    p.10=27;   p.11=31;  p.12=37

f.=                                    /*(below) a list of Fibonacci #s.*/
f.0=0;f.1=1;f.2=1;f.3=2;f.4=3;f.5=5;f.6=8;f.7=13;f.8=21;f.9=34;f.10=55

             do j=1  while p.j\==''
             c.j=p.j                   /*assign C array with some primes*/
             end   /*j*/
n=j-1
             do k=0  while f.k\=='';   n=n+1
             c.n=f.k                   /*assign C array with fib numbers*/
             end   /*k*/
say 'elements=' n
say
             do m=1  for n
             say 'c.'m"="c.m           /*show a "merged"  C  array nums.*/
             end   /*m*/
                                       /*stick a fork in it, we're done.*/
```

{{out}}

```txt

elements= 23

c.1=2
c.2=3
c.3=5
c.4=7
c.5=11
c.6=13
c.7=17
c.8=19
c.9=23
c.10=27
c.11=31
c.12=37
c.13=0
c.14=1
c.15=1
c.16=2
c.17=3
c.18=5
c.19=8
c.20=13
c.21=21
c.22=34
c.23=55

```



## Ring


```ring

arr1 = [1, 2, 3]
arr2 = [4, 5, 6]
arr3 = [7, 8, 9]
arr4 = arr1 + arr2
see arr4
see nl
arr5 = arr4 + arr3
see arr5

```



## RLaB


In RLaB the matrices can be appended (column-wise) or stacked (row-wise).
Consider few examples:

```RLaB

>> x = [1, 2, 3]
>> y = [4, 5, 6]
// appending matrix 'y' on the right from matrix 'x' is possible if the two matrices have
// the same number of rows:
>> z1 = [x, y]
matrix columns 1 thru 6
           1             2             3             4             5             6
// stacking matrix 'y' below the matrix 'x' is possible if the two matrices have
// the same number of columns:
>> z2  = [x; y]
           1             2             3
           4             5             6
>>

```



## Ruby

The <code>[http://www.ruby-doc.org/core/classes/Array.html#M002209 Array#+]</code> method concatenates two arrays and returns a new array.  The <code>[http://www.ruby-doc.org/core/classes/Array.html#M002166 Array#concat]</code> method appends elements of another array to the receiver.

```ruby
arr1 = [1, 2, 3]
arr2 = [4, 5, 6]
arr3 = [7, 8, 9]
arr4 = arr1 + arr2  # => [1, 2, 3, 4, 5, 6]
arr4.concat(arr3)  # => [1, 2, 3, 4, 5, 6, 7, 8, 9]
```


Or use flatten(1):

```ruby

# concat multiple arrays:
[arr1,arr2,arr3].flatten(1)
# ignore nil:
[arr1,arr2,arr3].compact.flatten(1)

```



## Rust


```rust
fn main() {
    let a_vec = vec![1, 2, 3, 4, 5];
    let b_vec = vec![6; 5];

    let c_vec = concatenate_arrays(&a_vec, &b_vec);

    println!("{:?} ~ {:?} => {:?}", a_vec, b_vec, c_vec);
}

fn concatenate_arrays<T: Clone>(x: &[T], y: &[T]) -> Vec<T> {
    let mut concat = x.to_vec();
    concat.extend_from_slice(y);

    concat
}

```


Or, with iterators:


```rust
fn concatenate_arrays<T: Clone>(x: &[T], y: &[T]) -> Vec<T> {
    x.iter().chain(y).cloned().collect()
}

```


=={{header|S-lang}}==
<lang S-lang>variable a = [1, 2, 3];
variable b = [4, 5, 6], c;
```


a+b is perfectly valid in S-Lang, but instead of the problem's desired effect,
it gives you a new array with each coorresponding element from a and b added.
But because arrays automatically 'flatten' when defined, concatenation is as
simple as:
<lang S-lang>c = [a, b];
```

Use of lists is more traditional; lists don't 'flatten', so we use either
list_concat() to create a new concatenated array:
<lang S-lang>a = {1, 2, 3};
b = {4, 5, 6};
c = list_concat(a, b);
```


or list_join():
<lang S-lang>list_join(a, b);
```

which adds the elements of b onto a.


## SASL

In SASL, the concat operator ++ is built-in

```SASL
(1 2 3) ++ (4 5 6)
```



## Scala


```Scala
val arr1 = Array( 1, 2, 3 )
val arr2 = Array( 4, 5, 6 )
val arr3 = Array( 7, 8, 9 )

arr1 ++ arr2 ++ arr3
//or:
Array concat ( arr1, arr2, arr3 )
// res0: Array[Int] = Array(1, 2, 3, 4, 5, 6, 7, 8, 9)
```



## Scheme


```scheme
; in r5rs, there is append for lists, but we'll need to define vector-append
(define (vector-append . arg) (list->vector (apply append (map vector->list arg))))

(vector-append #(1 2 3 4) #(5 6 7) #(8 9 10))
; #(1 2 3 4 5 6 7 8 9 10)
```


''Note : vector-append is also defined in [http://srfi.schemers.org/srfi-43/srfi-43.html SRFI-43].''

=== Concatening two-dimensional arrays ===
{{works with|Gauche Scheme}}


```Scheme

(use gauche.array)

(define (print-matrix m)
  (define row-num #f)
  (array-for-each-index m
    (lambda (row col)
      (when (and row-num (not (= row-num row))) (newline))
      (format #t "~a " (array-ref m row col))
      (set! row-num row)))
  (newline))

(define a
  #,(<array> (0 3 0 2)
      a b
      c d
      e f))

(define b
  #,(<array> (0 3 0 2)
      1 2
      3 4
      5 6))

(print-matrix (array-concatenate a b))
(print-matrix (array-concatenate a b 1))

```


{{out}}

```txt

a b
c d
e f
1 2
3 4
5 6

a b 1 2
c d 3 4
e f 5 6

```



## Seed7


```seed7
$ include "seed7_05.s7i";

var array integer: a is [] (1, 2, 3, 4);
var array integer: b is [] (5, 6, 7, 8);
var array integer: c is [] (9, 10);

const proc: main is func
  local
    var integer: number is 0;
  begin
    c := a & b;
    for number range c do
      write(number <& " ");
    end for;
    writeln;
  end func;
```


{{out}}

```txt
1 2 3 4 5 6 7 8
```



## SETL


```haskell
A := [1, 2, 3];
B := [3, 4, 5];
print(A + B); -- [1 2 3 3 4 5]
```



## Sidef


```ruby
var arr1 = [1, 2, 3];
var arr2 = [4, 5, 6];
var arr3 = (arr1 + arr2);   # => [1, 2, 3, 4, 5, 6]
```


## Simula


```simula
BEGIN  ! Concatenate arrays - of REAL, here;

    CLASS REAL_ARRAY(N); INTEGER N;
    BEGIN
        REAL ARRAY DATA(1:N);

        ! Return a new REAL_ARRAY containing
        ! the values from this REAL_ARRAY
        ! followed by the values from other;
        REF(REAL_ARRAY) PROCEDURE CONCAT(other);
            REF(REAL_ARRAY) other;
        BEGIN
            REF(REAL_ARRAY) C;
            INTEGER I;

            C :- NEW REAL_ARRAY(N + other.N);

            FOR I := 1 STEP 1 UNTIL N DO
                C.DATA(I) := DATA(I);

            FOR I := 1 STEP 1 UNTIL other.N DO
                C.DATA(N + I) := other.DATA(I);

            CONCAT :- C;
        END;

        ! Fill DATA;
        REF(REAL_ARRAY) PROCEDURE linearFill(start, stride);
            REAL start, stride;
        BEGIN
            linearFillFrom(DATA, 1, N, start, stride);
            linearFill :- this REAL_ARRAY
        END;

        PROCEDURE out(sink); REF(printfile) sink;
        BEGIN
            INTEGER i;
            FOR i := 1 STEP 1 UNTIL N DO
                sink.OUTFIX(DATA(i), 2, 7);
            sink.OUTIMAGE;
        END;
    END REAL_ARRAY;

    ! "The problem" is not array as an input parameter:
    !  I don't know how to
    ! "pass a new ARRAY out of a PROCEDURE";
    REF(REAL_ARRAY) PROCEDURE concatenate(a, b);
        REAL ARRAY a, b;
    BEGIN
        INTEGER i, a_, N, b_, M;
        REF(REAL_ARRAY) c;
        a_ := LOWERBOUND(a, 1) - 1;
        N := UPPERBOUND(a, 1) - a_;
        b_ := LOWERBOUND(a, 1) - 1;
        M := UPPERBOUND(b, 1) - b_;
        c :- NEW REAL_ARRAY(N + M);

        FOR i := 1 STEP 1 UNTIL N DO
            c.DATA(i) := a(a_+i);
        ! for readability, don't
        !  reduce one index expression to a variable
        FOR i := 1 STEP 1 UNTIL M DO
            c.DATA(N + i) := b(b_+i);

        concatenate :- c;
    END concatenate REAL ARRAYs;

    ! two more convenience PROCEDUREs;
    PROCEDURE linearFillFrom(a, from, inclusive, start, stride);
            REAL ARRAY a; ! passed by reference;
            INTEGER from, inclusive;
            REAL start, stride;
    BEGIN
        INTEGER i;
        FOR i := from STEP 1 UNTIL inclusive DO
            a(i) := start + stride * (i - from)
    END;
    PROCEDURE linearFill(a, start, stride);
            REAL ARRAY a;
            REAL start, stride;
        linearFillFrom(a, LOWERBOUND(a, 1), UPPERBOUND(a, 1),
                       start, stride);


    REF(REAL_ARRAY) X;
    REAL ARRAY u(1:3), v(1:4);
    linearFill(u, 3, 7);
    linearFill(v, 0, 5);
    concatenate(u, v).out(SYSOUT);

    X :- NEW REAL_ARRAY(3).linearFill(1, 2);
    X.out(SYSOUT);
    X.CONCAT(NEW REAL_ARRAY(4)
                .linearFill(-1, -3)).out(SYSOUT);
END.
```

{{out}}

```txt
   3.00  10.00  17.00   0.00   5.00  10.00  15.00
   1.00   3.00   5.00
   1.00   3.00   5.00  -1.00  -4.00  -7.00 -10.00
```



## Slate

The binary operation of concatenation is made with the <tt>;</tt> (semi-colon) from the type Sequence. It is also available for appending Sequences to WriteStreams.


```slate

{1. 2. 3. 4. 5} ; {6. 7. 8. 9. 10}

```



## Smalltalk

Concatenation (appending) is made with the method <tt>,</tt> (comma), present in classes SequenceableCollection, ArrayedCollection and their subclasses (e.g. Array, String, OrderedCollection ...)


```smalltalk
|a b c|
a := #(1 2 3 4 5).
b := #(6 7 8 9 10).
c := a,b.
c displayNl.
```



## SNOBOL4


{{works with|Macro Spitbol}}
{{works with|Snobol4+}}
{{works with|CSnobol}}


```SNOBOL4
*       # Concatenate 2 arrays (vectors)
        define('cat(a1,a2)i,j') :(cat_end)
cat     cat = array(prototype(a1) + prototype(a2))
cat1    i = i + 1; cat<i> = a1<i> :s(cat1)
cat2    j = j + 1; cat<i - 1 + j> = a2<j> :s(cat2)f(return)
cat_end

*       # Fill arrays
        str1 = '1 2 3 4 5'; arr1 = array(5)
loop    i = i + 1; str1 len(p) span('0123456789') . arr1<i> @p :s(loop)
        str2 = '6 7 8 9 10'; arr2 = array(5)
loop2   j = j + 1; str2 len(q) span('0123456789') . arr2<j> @q :s(loop2)

*       # Test and display
        arr3 = cat(arr1,arr2)
loop3   k = k + 1; str3 = str3 arr3<k> ' ' :s(loop3)
        output = str1
        output = str2
        output = str3
end
```


{{out}}

```txt
1 2 3 4 5
6 7 8 9 10
1 2 3 4 5 6 7 8 9 10
```



## Standard ML


```Standard ML

val l1 = [1,2,3,4];;
val l2 = [5,6,7,8];;
val l3 = l1 @ l2 (* [1,2,3,4,5,6,7,8] *)

```



## Stata


### Macro language


```stata
. matrix a=2,9,4\7,5,3\6,1,8
. matrix list a

a[3,3]
    c1  c2  c3
r1   2   9   4
r2   7   5   3
r3   6   1   8

. matrix b=I(3)
. matrix list b

symmetric b[3,3]
    c1  c2  c3
r1   1
r2   0   1
r3   0   0   1

. matrix c=a,b
. matrix list c

c[3,6]
    c1  c2  c3  c1  c2  c3
r1   2   9   4   1   0   0
r2   7   5   3   0   1   0
r3   6   1   8   0   0   1

. matrix c=a\b
. matrix list c

c[6,3]
    c1  c2  c3
r1   2   9   4
r2   7   5   3
r3   6   1   8
r1   1   0   0
r2   0   1   0
r3   0   0   1
```


###  Mata


```stata
. mata
: a=2,9,4\7,5,3\6,1,8

: b=I(3)

: a,b
       1   2   3   4   5   6
    +-------------------------+
  1 |  2   9   4   1   0   0  |
  2 |  7   5   3   0   1   0  |
  3 |  6   1   8   0   0   1  |
    +-------------------------+

: a\b
       1   2   3
    +-------------+
  1 |  2   9   4  |
  2 |  7   5   3  |
  3 |  6   1   8  |
  4 |  1   0   0  |
  5 |  0   1   0  |
  6 |  0   0   1  |
    +-------------+

: end
```



## Swift


```Swift
let array1 = [1,2,3]
let array2 = [4,5,6]
let array3 = array1 + array2
```



## Tcl


```tcl
set a {1 2 3}
set b {4 5 6}
set ab [concat $a $b];   # 1 2 3 4 5 6
```

Note that in the Tcl language, “arrays” are hash maps of strings to variables, so the notion of concatenation doesn't really apply. What other languages (usually) call arrays are “lists” in Tcl.

=={{header|TI-89 BASIC}}==

If <var>a</var> and <var>b</var> are lists, <code>augment(a, b)</code> concatenates them in the usual fashion. If <var>a</var> and <var>b</var> are matrices, then <code>augment(a, b)</code> produces a matrix whose columns are the columns of <var>a</var> followed by the columns of <var>b</var>, i.e. an [[wp:augmented matrix|augmented matrix]].

<!--lang ti89b-->
```txt
■ augment({1,2}, {3,4})
    {1,2,3,4}
■ augment([[1][2]], [[3][4]])
    [[1,3][2,4]]
```


That last example as displayed in pretty-printing mode:

<math>\operatorname{augment} \left(\begin{bmatrix}1 \\ 2\end{bmatrix}, \begin{bmatrix}3 \\ 4\end{bmatrix}\right)</math>

:<math>\begin{bmatrix}
1 & 3 \\
2 & 4
\end{bmatrix}</math>

Concatenation in the other direction may of course be done by transposition:

<!--lang ti89b-->
```txt
■ augment([[x][y]], [[z][w]])
    [[x][y][z][w]]
```


<math>\left(\operatorname{augment} \left(\begin{bmatrix}x \\ y\end{bmatrix}^\mathrm T, \begin{bmatrix}z \\ w\end{bmatrix}^\mathrm T\right)\right)^\mathrm T</math>

:<math>\begin{bmatrix} x \\ y \\ z \\ w\end{bmatrix}</math>


## Trith


```trith
[1 2 3] [4 5 6] concat
```



## UNIX Shell


Using proper built-in Bash arrays:

{{works with|bash}}


```bash
array1=( 1 2 3 4 5 )
array2=( 6 7 8 9 10 )
botharrays=( ${array1[@]} ${array2[@]} )
```


Whitespace-delimited strings work in much the same way:

{{works with|bash}}


```bash
array1='1 2 3 4 5'
array2='6 7 8 9 10'

# Concatenated to a Bash array ...
botharrays_a=( $array1 $array2 )

# Concatenated to a string ...
botharrays_s="$array1 $array2"
```



## Ursa


```ursa
# create two streams (the ursa equivalent of arrays)
# a contains the numbers 1-10, b contains 11-20
decl int<> a b
decl int i
for (set i 1) (< i 11) (inc i)
        append i a
end for
for (set i 11) (< i 21) (inc i)
        append i b
end for

# append the values in b to a
append b a

# output a to the console
out a endl console
```



## Vala


```vala
int[] array_concat(int[]a,int[]b){
	int[] c = new int[a.length + b.length];
	Memory.copy(c, a, a.length * sizeof(int));
	Memory.copy(&c[a.length], b, b.length * sizeof(int));
	return c;
}
void main(){
	int[] a = {1,2,3,4,5};
	int[] b = {6,7,8};
	int[] c = array_concat(a,b);
	foreach(int i in c){
		stdout.printf("%d\n",i);
	}
}
```



## VBA



```vb

Option Explicit

Sub MainConcat_Array()
Dim Aray_1() As Variant, Aray_2() As Variant
Dim Result() As Variant

    Aray_1 = Array(1, 2, 3, 4, 5, #11/24/2017#, "azerty")
    Aray_2 = Array("A", "B", "C", 18, "End")
    Result = Concat_Array(Aray_1, Aray_2)
    Debug.Print "With Array 1 : " & Join(Aray_1, ", ")
    Debug.Print "And Array 2 : " & Join(Aray_2, ", ")
    Debug.Print "The result is Array 3 : " & Join(Result, ", ")
End Sub

Function Concat_Array(A1() As Variant, A2() As Variant) As Variant()
Dim TmpA1() As Variant, N As Long, i As Long

    N = UBound(A1) + 1
    TmpA1 = A1
    ReDim Preserve TmpA1(N + UBound(A2))
    For i = N To UBound(TmpA1)
        TmpA1(i) = A2(i - N)
    Next
    Concat_Array = TmpA1
End Function

```

{{out}}

```txt
With Array 1 : 1, 2, 3, 4, 5, 24/11/2017, azerty
And Array 2 : A, B, C, 18, End
The result is Array 3 : 1, 2, 3, 4, 5, 24/11/2017, azerty, A, B, C, 18, End

```



## VBScript


```vb
Function ArrayConcat(arr1, arr2)
    ReDim ret(UBound(arr1) + UBound(arr2) + 1)
    For i = 0 To UBound(arr1)
        ret(i) = arr1(i)
    Next
    offset = Ubound(arr1) + 1
    For i = 0 To UBound(arr2)
        ret(i + offset) = arr2(i)
    Next
    ArrayConcat = ret
End Function

arr1 = array(10,20,30)
arr2 = array(40,50,60)
WScript.Echo "arr1 = array(" & Join(arr1,", ") & ")"
WScript.Echo "arr2 = array(" & Join(arr2,", ") & ")"
arr3 = ArrayConcat(arr1, arr2)
WScript.Echo "arr1 + arr2 = array(" & Join(arr3,", ") & ")"
```


{{out}}

```txt

arr1 = array(10, 20, 30)
arr2 = array(40, 50, 60)
arr1 + arr2 = array(10, 20, 30, 40, 50, 60)

```



## Visual Basic .NET


```vbnet

    Dim iArray1() As Integer = {1, 2, 3}
    Dim iArray2() As Integer = {4, 5, 6}
    Dim iArray3() As Integer = Nothing

    iArray3 = iArray1.Concat(iArray2).ToArray

```



## Wart


Wart doesn't have arrays yet, just lists.


```wart
a <- '(1 2 3)
b <- '(4 5 6)
a+b
# => (1 2 3 4 5 6)
```



## Wren


```wren
var arr1 = [1,2,3]
var arr2 = [4,5,6]
for (e in arr2) {
    arr1.add(e)
}
System.print(arr1)

```

{{Out}}

```txt
[1, 2, 3, 4, 5, 6]
```



## Yabasic


```Yabasic
sub arrayConcatenation(a(), b())
	local ta, tb, nt, i

	ta = arraysize(a(), 1)
	tb = arraysize(b(), 1)

	nt = ta + tb

	redim a(nt)

	for i = ta + 1 to nt
		a(i) = b(i - ta)
	next i

	return nt
end sub

//
### =========================


SIZE = 5

dim a(SIZE)
dim b(SIZE)

for i = 1 to SIZE
	a(i) = i
	b(i) = i + SIZE
next i

nt = arrayConcatenation(a(), b())

for i = 1 to nt
	print a(i);
	if i < nt print ", ";
next i
print
```



## Yacas


```Yacas
Concat({1,2,3}, {4,5,6})

Out> {1, 2, 3, 4, 5, 6}
```



## Yorick


```yorick
a = [1,2,3];
b = [4,5,6];
ab = grow(a, b);
```



## zkl

Lists (both mutable and read only), no built in support for numeric vectors/arrays/matrices

```zkl
T(1,2).extend(T(4,5,6)) //-->L(1,2,4,5,6)
T(1,2).extend(4,5,6)    //-->L(1,2,4,5,6)
```



## zonnon

<lang>
module Main;
import
	System.Collections.ArrayList as Array,
	System.Console as Console;

type
	Vector =  array {math} * of integer;

procedure Concat(x,y: Vector): Vector;
var
	i,k: integer;
	res: Vector;
begin
	res := new Vector(len(x) + len(y));
	k := 0;
	for i := 0 to len(x) - 1 do
		res[k] := x[i];inc(k)
	end;
	for i := 0 to len(y) - 1 do
		res[k] := y[i];inc(k)
	end;
	return res
end Concat;

procedure Concat2(x,y: Array): Array;
var
	i: integer;
	res: Array;
begin
	res := new Array(x.Count + y.Count);
	for i := 0 to x.Count - 1 do
		res.Add(x[i]);
	end;
	for i := 0 to y.Count - 1 do
		res.Add(y[i]);
	end;
	return res
end Concat2;

procedure WriteVec(x: Vector);
var
	i: integer;
begin
	for i := 0 to len(x) - 1 do;
		write(x[i]:3)
	end;
	writeln;
end WriteVec;

procedure WriteAry(x: Array);
var
	i: integer;
begin
	for i := 0 to x.Count - 1 do;
		Console.Write("{0,3}",x[i])
	end;
	writeln;
end WriteAry;

var
	a,b: Vector;
	x,y: Array;
begin
	a := [1,2,3,4];
	b := [6,7,8,9];
	WriteVec(Concat(a,b));

	x := new Array(4);
	y := new Array(4);
	x.Add(2);x.Add(4);x.Add(6);x.Add(8);
	y.Add(3);y.Add(5);y.Add(9);y.Add(11);

	WriteAry(Concat2(x,y));
end Main.
```

{{out}}

```txt
  1  2  3  4  6  7  8  9
  2  4  6  8  3  5  9 11
```


## Zsh

Concatenating arrays.

```zsh
a=(1 2 3)
b=(a b c)

c=($a $b)
```

Pushing a single element into an array.

```zsh
a += 4
```

Pushing another array into an array.

```zsh
a += ($b)
```


## ZX Spectrum Basic

{{trans|Liberty BASIC}}

```zxbasic
10 LET x=10
20 LET y=20
30 DIM a(x)
40 DIM b(y)
50 DIM c(x+y)
60 FOR i=1 TO x
70 LET c(i)=a(i)
80 NEXT i
90 FOR i=1 TO y
100 LET c(x+i)=b(i)
110 NEXT i
120 FOR i=1 TO x+y
130 PRINT c(i);", ";
140 NEXT i

```

