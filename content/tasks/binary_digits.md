+++
title = "Binary digits"
description = ""
date = 2019-10-20T17:11:00Z
aliases = []
[extra]
id = 10041
[taxonomies]
categories = ["Basic language learning", "Radices", "task"]
tags = []
languages = [
  "0815",
  "11l",
  "360_assembly",
  "6502_assembly",
  "8th",
  "acl2",
  "ada",
  "aime",
  "algol_68",
  "algol_w",
  "applescript",
  "applesoft_basic",
  "arm_assembly",
  "autohotkey",
  "autoit",
  "awk",
  "axe",
  "bacon",
  "basic",
  "basic256",
  "batch_file",
  "bbc_basic",
  "bc",
  "befunge",
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
  "crystal",
  "csharp",
  "d",
  "dart",
  "dc",
  "delphi",
  "dyalect",
  "easylang",
  "echolisp",
  "elena",
  "elixir",
  "erlang",
  "euphoria",
  "factor",
  "fbsl",
  "forth",
  "fortran",
  "free_pascal",
  "freebasic",
  "frink",
  "funl",
  "futhark",
  "gambas",
  "go",
  "groovy",
  "haskell",
  "idris",
  "j",
  "java",
  "javascript",
  "joy",
  "jq",
  "julia",
  "k",
  "kotlin",
  "lang5",
  "lfe",
  "liberty_basic",
  "llvm",
  "locomotive_basic",
  "lolcode",
  "lua",
  "m2000_interpreter",
  "maple",
  "maxima",
  "maxscript",
  "mercury",
  "min",
  "miniscript",
  "mlite",
  "netrexx",
  "nickle",
  "nim",
  "objeck",
  "ocaml",
  "oforth",
  "oxygenbasic",
  "panda",
  "pari_gp",
  "pascal",
  "peloton",
  "perl",
  "perl_6",
  "phix",
  "php",
  "picolisp",
  "piet",
  "pl_i",
  "powerbasic",
  "powershell",
  "processing",
  "prolog",
  "purebasic",
  "python",
  "r",
  "racket",
  "rapidq",
  "red",
  "retro",
  "rexx",
  "ring",
  "ruby",
  "run_basic",
  "rust",
  "scala",
  "scheme",
  "seed7",
  "sequencel",
  "sidef",
  "simula",
  "skookumscript",
  "smalltalk",
  "snusp",
  "standard_ml",
  "swift",
  "tcl",
  "ubasic_4th",
  "unix_shell",
  "vba",
  "vedit_macro_language",
  "vim_script",
  "visual_basic",
  "visual_basic_.net",
  "visual_foxpro",
  "whitespace",
  "wortel",
  "x86_assembly",
  "xpl0",
  "zkl",
  "zx_spectrum_basic",
]
+++

## Task

;Task:
Create and display the sequence of binary digits for a given   [[wp:Natural number|non-negative integer]].

    The decimal value      '''5'''   should produce an output of               '''101'''
    The decimal value     '''50'''   should produce an output of            '''110010'''
    The decimal value   '''9000'''   should produce an output of    '''10001100101000'''

The results can be achieved using built-in radix functions within the language   (if these are available),   or alternatively a user defined function can be used.

The output produced should consist just of the binary digits of each number followed by a   ''newline''.

There should be no other whitespace, radix or sign markers in the produced output, and [[wp:Leading zero|leading zeros]] should not appear in the results.





## 0815


```0815
}:r:|~    Read numbers in a loop.
  }:b:    Treat the queue as a stack and
    <:2:= accumulate the binary digits
    /=>&~ of the given number.
  ^:b:
  <:0:->  Enqueue negative 1 as a sentinel.
  {       Dequeue the first binary digit.
  }:p:
    ~%={+ Rotate each binary digit into place and print it.
  ^:p:
  <:a:~$  Output a newline.
^:r:
```


{{out}}

Note that 0815 reads numeric input in hexadecimal.


```bash
echo -e "5\n32\n2329" | 0815 bin.0
101
110010
10001100101001
```



## 11l


```11l
L(n) [0, 5, 50, 9000]
   print(‘#4 = #.’.format(n, bin(n)))
```

{{out}}

```txt

   0 = 0
   5 = 101
  50 = 110010
9000 = 10001100101000

```



## 360 Assembly


```360asm
*        Binary digits             27/08/2015
BINARY   CSECT
         USING  BINARY,R12
         LR     R12,R15            set base register
BEGIN    LA     R10,4
         LA     R9,N
LOOPN    MVC    W,0(R9)
         MVI    FLAG,X'00'
         LA     R8,32
         LA     R2,CBIN
LOOP     TM     W,B'10000000'      test fist bit
         BZ     ZERO               zero
         MVI    FLAG,X'01'         one written
         MVI    0(R2),C'1'         write 1
         B      CONT
ZERO     CLI    FLAG,X'01'         is one written ?
         BNE    BLANK
         MVI    0(R2),C'0'         write 0
         B      CONT
BLANK    BCTR   R2,0               backspace
CONT     L      R3,W
         SLL    R3,1               shilf left
         ST     R3,W
         LA     R2,1(R2)           next bit
         BCT    R8,LOOP            loop on bits
PRINT    CLI    FLAG,X'00'         is '0'
         BNE    NOTZERO
         MVI    0(R2),C'0'         then write 0
NOTZERO  L      R1,0(R9)
         XDECO  R1,CDEC
         XPRNT  CDEC,45
         LA     R9,4(R9)
         BCT    R10,LOOPN          loop on numbers
RETURN   XR     R15,R15            set return code
         BR     R14                return to caller
N        DC     F'0',F'5',F'50',F'9000'
W        DS     F                  work
FLAG     DS     X                  flag for trailing blanks
CDEC     DS     CL12               decimal value
         DC     C' '
CBIN     DC     CL32' '            binary value
         YREGS
         END    BINARY
```

{{out}}

```txt

           0 0
           5 101
          50 110010
        9000 10001100101000

```



## 6502 Assembly

{{works with|http://vice-emu.sourceforge.net/ VICE}}
This example has been written for the C64 and uses some BASIC routines to read the parameter after the SYS command and to print the result.
Compile with the [http://turbo.style64.org/ Turbo Macro Pro cross assembler]:

```txt

tmpx -i dec2bin.s -o dec2bin.prg

```

Use the [http://vice-emu.sourceforge.net/vice_13.html c1541 utility] to create a disk image that can be loaded using VICE x64.
Run with:

```txt

SYS828,x

```

where x is an integer ranging from 0 to 65535 (16 bit int). Floating point numbers are truncated and converted accordingly.
The example can easily be modified to run on the VIC-20, just change the labels as follows:

```txt

chkcom      = $cefd
frmnum      = $cd8a
getadr      = $d7f7
strout      = $cb1e

```


```6502asm

; C64 - Binary digits
;       http://rosettacode.org/wiki/Binary_digits

; *** labels ***

declow      = $fb
dechigh     = $fc
binstrptr   = $fd               ; $fe is used for the high byte of the address
chkcom      = $aefd
frmnum      = $ad8a
getadr      = $b7f7
strout      = $ab1e

; *** main ***

            *=$033c             ; sys828 tbuffer ($033c-$03fb)

            jsr chkcom          ; check for and skip comma
            jsr frmnum          ; evaluate numeric expression
            jsr getadr          ; convert floating point number to two-byte int
            jsr dec2bin         ; convert two-byte int to binary string
            lda #<binstr        ; load the address of the binary string - low
            ldy #>binstr        ; high byte
            jsr skiplz          ; skip leading zeros, return an address in a/y
                                ;   that points to the first "1"
            jsr strout          ; print the result
            rts

; *** subroutines ****

; Converts a 16 bit integer to a binary string.
; Input: y - low byte of the integer
;        a - high byte of the integer
; Output: a 16 byte string stored at 'binstr'
dec2bin     sty declow          ; store the two-byte integer
            sta dechigh
            lda #<binstr        ; store the binary string address on the zero page
            sta binstrptr
            lda #>binstr
            sta binstrptr+1
            ldx #$01            ; start conversion with the high byte
wordloop    ldy #$00            ; bit counter
byteloop    asl declow,x        ; shift left, bit 7 is shifted into carry
            bcs one             ; carry set? jump
            lda #"0"            ; a="0"
            bne writebit
one         lda #"1"            ; a="1"
writebit    sta (binstrptr),y   ; write the digit to the string
            iny                 ; y++
            cpy #$08            ; y==8 all bits converted?
            bne byteloop        ;   no -> convert next bit
            clc                 ; clear carry
            lda #$08            ; a=8
            adc binstrptr       ; add 8 to the string address pointer
            sta binstrptr
            bcc nooverflow      ; address low byte did overflow?
            inc binstrptr+1     ;   yes -> increase the high byte
nooverflow  dex                 ; x--
            bpl wordloop        ; x<0? no -> convert the low byte
            rts                 ;   yes -> conversion finished, return

; Skip leading zeros.
; Input:  a - low byte of the byte string address
;         y - high byte -"-
; Output: a - low byte of string start address without leading zeros
;         y - high byte -"-
skiplz      sta binstrptr       ; store the binary string address on the zero page
            sty binstrptr+1
            ldy #$00            ; byte counter
skiploop    lda (binstrptr),y   ; load a byte from the string
            iny                 ; y++
            cpy #$11            ; y==17
            beq endreached      ;   yes -> end of string reached without a "1"
            cmp #"1"            ; a=="1"
            bne skiploop        ;   no -> take the next byte
            beq add2ptr         ;   yes -> jump
endreached  dey                 ; move the pointer to the last 0
add2ptr     clc
            dey
            tya                 ; a=y
            adc binstrptr       ; move the pointer to the first "1" in the string
            bcc loadhigh        ; overflow?
            inc binstrptr+1     ;  yes -> increase high byte
loadhigh    ldy binstrptr+1
            rts

; *** data ***

binstr      .repeat 16, $00     ; reserve 16 bytes for the binary digits
            .byte $0d, $00      ; newline + null terminator

```

{{out}}

```txt

SYS828,5
101

SYS828,50
110010

SYS828,9000
10001100101000

SYS828,4.7
100

```



## 8th


```forth

2 base drop
#50 . cr

```

{{out}}

```txt

110010

```


## ACL2


```Lisp
(include-book "arithmetic-3/top" :dir :system)

(defun bin-string-r (x)
   (if (zp x)
       ""
       (string-append
        (bin-string-r (floor x 2))
        (if (= 1 (mod x 2))
            "1"
            "0"))))

(defun bin-string (x)
   (if (zp x)
       "0"
       (bin-string-r x)))
```



## Ada



```Ada
with ada.text_io; use ada.text_io;
procedure binary is
  bit : array (0..1) of character := ('0','1');

  function bin_image (n : Natural) return string is
  (if n < 2 then (1 => bit (n)) else bin_image (n / 2) & bit (n mod 2));

  test_values : array (1..3) of Natural := (5,50,9000);
begin
  for test of test_values loop
	put_line ("Output for" & test'img & " is " & bin_image (test));
  end loop;
end binary;
```


{{out}}

```txt

Output for 5 is 101
Output for 50 is 110010
Output for 9000 is 10001100101000

```



## Aime


```aime
o_xinteger(2, 0);
o_byte('\n');
o_xinteger(2, 5);
o_byte('\n');
o_xinteger(2, 50);
o_byte('\n');
o_form("/x2/\n", 9000);
```

{{out}}

```txt
0
101
110010
10001100101000
```



## ALGOL 68

{{works with|ALGOL 68|Revision 1.}}
{{works with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-2.3.3 algol68g-2.3.3].}}
{{wont work with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8-8d] - due to use of '''format'''[ted] ''transput''.}}
'''File: Binary_digits.a68'''
```algol68
#!/usr/local/bin/a68g --script #

printf((
  $g" => "2r3d l$, 5, BIN 5,
  $g" => "2r6d l$, 50, BIN 50,
  $g" => "2r14d l$, 9000, BIN 9000
));

# or coerce to an array of BOOL #
print((
  5, " => ", []BOOL(BIN 5)[bits width-3+1:], new line,
  50, " => ", []BOOL(BIN 50)[bits width-6+1:], new line,
  9000, " => ", []BOOL(BIN 9000)[bits width-14+1:], new line
))
```

{{out}}

```txt

         +5 => 101
        +50 => 110010
      +9000 => 10001100101000
         +5 => TFT
        +50 => TTFFTF
      +9000 => TFFFTTFFTFTFFF

```



## ALGOL W


```algolw
begin
    % prints an integer in binary - the number must be greater than zero     %
    procedure printBinaryDigits( integer value n ) ;
    begin
        if n not = 0 then begin
            printBinaryDigits( n div 2 );
            writeon( if n rem 2 = 1 then "1" else "0" )
        end
    end binaryDigits ;

    % prints an integer in binary - the number must not be negative          %
    procedure printBinary( integer value n ) ;
    begin
        if n = 0 then writeon( "0" )
                 else printBinaryDigits( n )
    end printBinary ;

    % test the printBinaryDigits procedure                                   %
    for i := 5, 50, 9000 do begin
        write();
        printBinary( i );
    end

end.
```




## AppleScript

{{Trans|JavaScript}}
(ES6 version)

(The generic showIntAtBase here, which allows us to specify the digit set used (e.g. upper or lower case in hex, or different regional or other digit sets generally), is a rough translation of Haskell's Numeric.showintAtBase)

```AppleScript
-- showBin :: Int -> String
on showBin(n)
    script binaryChar
        on |λ|(n)
            text item (n + 1) of "01"
        end |λ|
    end script
    showIntAtBase(2, binaryChar, n, "")
end showBin

-- GENERIC FUNCTIONS ----------------------------------------------------------

-- showIntAtBase :: Int -> (Int -> Char) -> Int -> String -> String
on showIntAtBase(base, toChr, n, rs)
    script showIt
        on |λ|(nd_, r)
            set {n, d} to nd_
            set r_ to toChr's |λ|(d) & r
            if n > 0 then
                |λ|(quotRem(n, base), r_)
            else
                r_
            end if
        end |λ|
    end script

    if base ≤ 1 then
        "error: showIntAtBase applied to unsupported base: " & base as string
    else if n < 0 then
        "error: showIntAtBase applied to negative number: " & base as string
    else
        showIt's |λ|(quotRem(n, base), rs)
    end if
end showIntAtBase

--  quotRem :: Integral a => a -> a -> (a, a)
on quotRem(m, n)
    {m div n, m mod n}
end quotRem

-- TEST -----------------------------------------------------------------------
on run
    script
        on |λ|(n)
            intercalate(" -> ", {n as string, showBin(n)})
        end |λ|
    end script

    return unlines(map(result, {5, 50, 9000}))
end run


-- GENERIC FUNCTIONS FOR TEST -------------------------------------------------

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

-- unlines :: [String] -> String
on unlines(xs)
    intercalate(linefeed, xs)
end unlines
```

{{Out}}

```txt
5 -> 101
50 -> 110010
9000 -> 10001100101000
```


Or，using:

```AppleScript
-- showBin :: Int -> String
on showBin(n)
    script binaryChar
        on |λ|(n)
            text item (n + 1) of "〇一"
        end |λ|
    end script
    showIntAtBase(2, binaryChar, n, "")
end showBin
```


```txt
5 -> 一〇一
50 -> 一一〇〇一〇
9000 -> 一〇〇〇一一〇〇一〇一〇〇〇
```



## ARM Assembly

{{works with|as|Raspberry Pi}}

```ARM Assembly


/* ARM assembly Raspberry PI  */
/*  program binarydigit.s   */

/* Constantes    */
.equ STDOUT, 1
.equ WRITE,  4
.equ EXIT,   1
/* Initialized data */
.data

sMessAffBin: .ascii "The decimal value  "
sZoneDec: .space 12,' '
             .ascii " should produce an output of "
sZoneBin: .space 36,' '
              .asciz "\n"

/*  code section */
.text
.global main
main:                /* entry of program  */
    push {fp,lr}    /* save des  2 registres */
    mov r0,#5
    ldr r1,iAdrsZoneDec
    bl conversion10S    @ decimal conversion
    bl conversion2      @ binary conversion and display résult
    mov r0,#50
    ldr r1,iAdrsZoneDec
    bl conversion10S
    bl conversion2
    mov r0,#-1
    ldr r1,iAdrsZoneDec
    bl conversion10S
    bl conversion2
    mov r0,#1
    ldr r1,iAdrsZoneDec
    bl conversion10S
    bl conversion2

100:   /* standard end of the program */
    mov r0, #0                  @ return code
    pop {fp,lr}                 @restaur 2 registers
    mov r7, #EXIT              @ request to exit program
    swi 0                       @ perform the system call
iAdrsZoneDec: .int sZoneDec
/******************************************************************/
/*     register conversion in binary                              */
/******************************************************************/
/* r0 contains the register */
conversion2:
    push {r0,lr}     /* save  registers */
    push {r1-r5} /* save others registers */
    ldr r1,iAdrsZoneBin   @ address reception area
    clz r2,r0    @ number of left zeros bits
    rsb r2,#32   @ number of significant bits
    mov r4,#' '  @ space
    add r3,r2,#1 @ position counter in reception area
1:
    strb r4,[r1,r3]   @ space in other location of reception area
    add r3,#1
    cmp r3,#32         @ end of area ?
    ble 1b            @ no! loop
    mov r3,r2    @ position counter of the written character
2:               @ loop
    lsrs r0,#1    @ shift right one bit with flags
    movcc r4,#48  @ carry clear  => character 0
    movcs r4,#49  @ carry set   => character 1
    strb r4,[r1,r3]  @ character in reception area at position counter
    sub r3,r3,#1     @
    subs r2,r2,#1   @  0 bits ?
    bgt 2b          @ no!  loop

    ldr r0,iAdrsZoneMessBin
    bl affichageMess

100:
    pop {r1-r5}  /* restaur others registers */
    pop {r0,lr}
    bx lr
iAdrsZoneBin: .int sZoneBin
iAdrsZoneMessBin: .int sMessAffBin

/******************************************************************/
/*     display text with size calculation                         */
/******************************************************************/
/* r0 contains the address of the message */
affichageMess:
    push {fp,lr}    			/* save  registres */
    push {r0,r1,r2,r7}    		/* save others registres */
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
    pop {r0,r1,r2,r7}     		/* restaur others registres */
    pop {fp,lr}    				/* restaur des  2 registres */
    bx lr	        			/* return  */
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
    push {r2-r4}   /* save others registers  */
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



## AutoHotkey


```AutoHotkey
MsgBox % NumberToBinary(5) ;101
MsgBox % NumberToBinary(50) ;110010
MsgBox % NumberToBinary(9000) ;10001100101000

NumberToBinary(InputNumber)
{
 While, InputNumber
  Result := (InputNumber & 1) . Result, InputNumber >>= 1
 Return, Result
}
```


## AutoIt


```autoit

ConsoleWrite(IntToBin(50) & @CRLF)

Func IntToBin($iInt)
	$Stack = ObjCreate("System.Collections.Stack")
	Local $b = -1, $r = ""
	While $iInt <> 0
		$b = Mod($iInt, 2)
		$iInt = INT($iInt/2)
		$Stack.Push ($b)
	WEnd
	For $i = 1 TO $Stack.Count
		$r &= $Stack.Pop
	Next
	Return $r
EndFunc   ;==>IntToBin

```



## AWK


```awk
BEGIN {
  print tobinary(5)
  print tobinary(50)
  print tobinary(9000)
}

function tobinary(num) {
  outstr = ""
  l = num
  while ( l ) {
    if ( l%2 == 0 ) {
      outstr = "0" outstr
    } else {
      outstr = "1" outstr
    }
    l = int(l/2)
  }
  # Make sure we output a zero for a value of zero
  if ( outstr == "" ) {
    outstr = "0"
  }
  return outstr
}
```



## Axe

This example builds a string backwards to ensure the digits are displayed in the correct order. It uses bitwise logic to extract one bit at a time.

```axe
Lbl BIN
.Axe supports 16-bit integers, so 16 digits are enough
L₁+16→P
0→{P}
While r₁
 P--
 {(r₁ and 1)▶Hex+3}→P
 r₁/2→r₁
End
Disp P,i
Return
```



## BaCon


```freebasic
' Binary digits
OPTION MEMTYPE int
INPUT n$
IF VAL(n$) = 0 THEN
    PRINT "0"
ELSE
    PRINT CHOP$(BIN$(VAL(n$)), "0", 1)
ENDIF
```



## Batch File

This num2bin.bat file handles non-negative input as per the requirements with no leading zeros in the output. Batch only supports signed integers. This script also handles negative values by printing the appropriate two's complement notation.

```dos
@echo off
:num2bin    IntVal [RtnVar]
  setlocal enableDelayedExpansion
  set /a n=%~1
  set rtn=
  for /l %%b in (0,1,31) do (
    set /a "d=n&1, n>>=1"
    set rtn=!d!!rtn!
  )
  for /f "tokens=* delims=0" %%a in ("!rtn!") do set rtn=%%a
  (endlocal & rem -- return values
    if "%~2" neq "" (set %~2=%rtn%) else echo %rtn%
  )
exit /b
```



## BASIC


=
## Applesoft BASIC
=

```ApplesoftBasic
 0 N = 5: GOSUB 1:N = 50: GOSUB 1:N = 9000: GOSUB 1: END
 1  LET N2 =  ABS ( INT (N))
 2  LET B$ = ""
 3  FOR N1 = N2 TO 0 STEP 0
 4      LET N2 =  INT (N1 / 2)
 5      LET B$ =  STR$ (N1 - N2 * 2) + B$
 6      LET N1 = N2
 7  NEXT N1
 8  PRINT B$
 9  RETURN
```

{{out}}

```txt
101
110010
10001100101000

```


=
## BASIC256
=

```basic256

# DecToBin.bas
# BASIC256 1.1.4.0


dim a(3)                                            #dimension a 3 element array (a)
a = {5, 50, 9000}

for i = 0 to 2
    print a[i] + chr(9) + toRadix(a[i],2)           # radix (decimal, base2)
next i

```

{{out}}

```txt

5	101
50	110010
9000	10001100101000

```


=
## BBC BASIC
=

```bbcbasic
      FOR num% = 0 TO 16
        PRINT FN_tobase(num%, 2, 0)
      NEXT
      END

      REM Convert N% to string in base B% with minimum M% digits:
      DEF FN_tobase(N%,B%,M%)
      LOCAL D%,A$
      REPEAT
        D% = N%MODB%
        N% DIV= B%
        IF D%<0 D% += B%:N% -= 1
        A$ = CHR$(48 + D% - 7*(D%>9)) + A$
        M% -= 1
      UNTIL (N%=FALSE OR N%=TRUE) AND M%<=0
      =A$
```

The above is a generic "Convert to any base" program.
Here is a faster "Convert to Binary" program:

```bbcbasic
PRINT FNbinary(5)
PRINT FNbinary(50)
PRINT FNbinary(9000)
END

DEF FNbinary(N%)
LOCAL A$
REPEAT
  A$ = STR$(N% AND 1) + A$
  N% = N% >>> 1  : REM BBC Basic prior to V5 can use N% = N% DIV 2
UNTIL N% = 0
=A$
```


=
## Commodore BASIC
=

```commodorebasic
10 N = 5 : GOSUB 100
20 N = 50 : GOSUB 100
30 N = 9000 : GOSUB 100
40 END
90 REM *** SUBROUTINE: CONVERT DECIMAL TO BINARY
100 N2 =  ABS(INT(N))
110 B$ = ""
120 FOR N1 = N2 TO 0 STEP 0
130 :  N2 =  INT(N1 / 2)
140 :  B$ =  STR$(N1 - N2 * 2) + B$
150 :  N1 = N2
160 NEXT N1
170 PRINT B$
180 RETURN
```


==={{header|IS-BASIC}}===
<lang IS-BASIC>10 PRINT BIN$(50)
100 DEF BIN$(N)
110   LET N=ABS(INT(N)):LET B$=""
120   DO
140     LET B$=STR$(MOD(N,2))&B$:LET N=INT(N/2)
150   LOOP WHILE N>0
160   LET BIN$=B$
170 END DEF
```



## bc

{{trans|dc}}

```bc
obase = 2
5
50
9000
quit
```


## Befunge

Reads the number to convert from standard input.

```befunge
&>0\55+\:2%68>*#<+#8\#62#%/#2:_$>:#,_$@
```

{{out}}

```txt
9000
10001100101000
```



## Bracmat


```bracmat
  ( dec2bin
  =   bit bits
    .   :?bits
      &   whl
        ' ( !arg:>0
          & mod$(!arg,2):?bit
          & div$(!arg,2):?arg
          & !bit !bits:?bits
          )
      & (str$!bits:~|0)
  )
& 0 5 50 9000 423785674235000123456789:?numbers
&   whl
  ' ( !numbers:%?dec ?numbers
    & put$(str$(!dec ":\n" dec2bin$!dec \n\n))
    )
;
```

{{out}}

```txt
0:
0

5:
101

50:
110010

9000:
10001100101000

423785674235000123456789:
1011001101111010111011110101001101111000000000000110001100000100111110100010101
```


=={{header|Brainfuck}}==

This is almost an exact duplicate of [[Count in octal#Brainfuck]]. It outputs binary numbers until it is forced to terminate or the counter overflows to 0.


```bf
+[            Start with n=1 to kick off the loop
[>>++<<       Set up {n 0 2} for divmod magic
[->+>-        Then
[>+>>]>       do
[+[-<+>]>+>>] the
<<<<<<]       magic
>>>+          Increment n % 2 so that 0s don't break things
>]            Move into n / 2 and divmod that unless it's 0
-<            Set up sentinel ‑1 then move into the first binary digit
[++++++++ ++++++++ ++++++++ Add 47 to get it to ASCII
 ++++++++ ++++++++ +++++++. and print it
[<]<]         Get to a 0; the cell to the left is the next binary digit
>>[<+>-]      Tape is {0 n}; make it {n 0}
>[>+]         Get to the ‑1
<[[-]<]       Zero the tape for the next iteration
++++++++++.   Print a newline
[-]<+]        Zero it then increment n and go again
```



## Burlesque


```burlesque

blsq ) {5 50 9000}{2B!}m[uN
101
110010
10001100101000

```



## C

Converts int to a string.

```c
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

char *bin(uint32_t x);

int main(void)
{
    for (size_t i = 0; i < 20; i++) {
        char *binstr = bin(i);
        printf("%s\n", binstr);
        free(binstr);
    }
}

char *bin(uint32_t x)
{
    size_t bits = (x == 0) ? 1 : log10((double) x)/log10(2) + 1;
    char *ret = malloc((bits + 1) * sizeof (char));
    for (size_t i = 0; i < bits ; i++) {
       ret[bits - i - 1] = (x & 1) ? '1' : '0';
       x >>= 1;
    }
    ret[bits] = '\0';
    return ret;
}
```


{{out}}

```txt
0
1
10
11
100
101
110
111
1000
1001
1010
1011
1100
1101
1110
1111
10000
10001
10010
10011
```



## C++


```cpp
#include <bitset>
#include <iostream>
#include <limits>
#include <string>

void print_bin(unsigned int n) {
  std::string str = "0";

  if (n > 0) {
    str = std::bitset<std::numeric_limits<unsigned int>::digits>(n).to_string();
    str = str.substr(str.find('1')); // remove leading zeros
  }

  std::cout << str << '\n';
}

int main() {
  print_bin(0);
  print_bin(5);
  print_bin(50);
  print_bin(9000);
}

```

{{out}}

```txt

0
101
110010
10001100101000

```

Shorter version using bitset

```cpp
#include <iostream>
#include <bitset>
void printBits(int n) {                     // Use int like most programming languages.
  int iExp = 0;                             // Bit-length
  while (n >> iExp) ++iExp;                 // Could use template <log(x)*1.44269504088896340736>
  for (int at = iExp - 1; at >= 0; at--)    // Reverse iter from the bit-length to 0 - msb is at end
    std::cout << std::bitset<32>(n)[at];    // Show 1's, show lsb, hide leading zeros
  std::cout << '\n';
}
int main(int argc, char* argv[]) {
  printBits(5);
  printBits(50);
  printBits(9000);
} // for testing with n=0 printBits<32>(0);
```

Using >> operator. (1st example is 2.75x longer. Matter of taste.)

```cpp
#include <iostream>
int main(int argc, char* argv[]) {
  unsigned int in[] = {5, 50, 9000};        // Use int like most programming languages
  for (int i = 0; i < 3; i++)               // Use all inputs
    for (int at = 31; at >= 0; at--)        // reverse iteration from the max bit-length to 0, because msb is at the end
      if (int b = (in[i] >> at))            // skip leading zeros. Start output when significant bits are set
         std::cout << ('0' + b & 1) << (!at ? "\n": "");	// '0' or '1'. Add EOL if last bit of num
}

```

To be fair comparison with languages that doesn't declare a function like C++ main(). 3.14x shorter than 1st example.

```cpp
#include <iostream>
int main(int argc, char* argv[]) {                        // Usage: program.exe 5 50 9000
  for (int i = 1; i < argc; i++)                          // argv[0] is program name
    for (int at = 31; at >= 0; at--)                      // reverse iteration from the max bit-length to 0, because msb is at the end
      if (int b = (atoi(argv[i]) >> at))                  // skip leading zeros
         std::cout << ('0' + b & 1) << (!at ? "\n": "");  // '0' or '1'. Add EOL if last bit of num
}

```

Using bitwise operations with recursion.

```cpp

#include <iostream>

std::string binary(int n) {
  return n == 0 ? "" : binary(n >> 1) + std::to_string(n & 1);
}

int main(int argc, char* argv[]) {
  for (int i = 1; i < argc; ++i) {
    std::cout << binary(std::stoi(argv[i])) << std::endl;
  }
}

```

{{out}}

```txt

101
110010
10001100101000

```


## C#

```c#
using System;

class Program
{
    static void Main()
    {
        foreach (var number in new[] { 5, 50, 9000 })
        {
            Console.WriteLine(Convert.ToString(number, 2));
        }
    }
}
```

{{out}}

```txt

101
110010
10001100101000

```



## Ceylon


```ceylon
    shared void run() {

        void printBinary(Integer integer) =>
            print(Integer.format(integer, 2));

        printBinary(5);
        printBinary(50);
        printBinary(9k);
    }
```



## Clojure


```clojure
(Integer/toBinaryString 5)
(Integer/toBinaryString 50)
(Integer/toBinaryString 9000)
```



## COBOL


```COBOL
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SAMPLE.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

         01 binary_number   pic X(21).
         01 str             pic X(21).
         01 binary_digit    pic X.
         01 digit           pic 9.
         01 n               pic 9(7).
         01 nstr            pic X(7).

       PROCEDURE DIVISION.
         accept nstr
         move nstr to n
         perform until n equal 0
           divide n by 2 giving n remainder digit
           move digit to binary_digit
           string binary_digit  DELIMITED BY SIZE
                  binary_number DELIMITED BY SPACE
                  into str
           move str to binary_number
         end-perform.
         display binary_number
         stop run.

```

Free-form, using a reference modifier to index into binary-number.

```cobol
IDENTIFICATION DIVISION.
PROGRAM-ID. binary-conversion.

DATA DIVISION.
WORKING-STORAGE SECTION.
01 binary-number   pic X(21).
01 digit           pic 9.
01 n               pic 9(7).
01 nstr            pic X(7).
01 ptr			   pic 99.

PROCEDURE DIVISION.
	display "Number: " with no advancing.
	accept nstr.
	move nstr to n.
	move zeroes to binary-number.
	move length binary-number to ptr.
	perform until n equal 0
		divide n by 2 giving n remainder digit
		move digit to binary-number(ptr:1)
		subtract 1 from ptr
		if ptr < 1
			exit perform
		end-if
	end-perform.
	display binary-number.
	stop run.
```



## CoffeeScript


```coffeescript
binary = (n) ->
  new Number(n).toString(2)

console.log binary n for n in [5, 50, 9000]
```



## Common Lisp

Just print the number with "~b":

```lisp
(format t "~b" 5)

; or

(write 5 :base 2)
```



## Component Pascal

BlackBox Component Builder

```oberon2

MODULE BinaryDigits;
IMPORT StdLog,Strings;

PROCEDURE Do*;
VAR
	str : ARRAY 33 OF CHAR;
BEGIN
	Strings.IntToStringForm(5,2, 1,'0',FALSE,str);
	StdLog.Int(5);StdLog.String(":> " + str);StdLog.Ln;
	Strings.IntToStringForm(50,2, 1,'0',FALSE,str);
	StdLog.Int(50);StdLog.String(":> " + str);StdLog.Ln;
	Strings.IntToStringForm(9000,2, 1,'0',FALSE,str);
	StdLog.Int(9000);StdLog.String(":> " + str);StdLog.Ln;
END Do;
END BinaryDigits.

```

Execute: ^Q BinaryDigits.Do <br/>
{{out}}

```txt

 5:> 101
 50:> 110010
 9000:> 10001100101000
```



## Crystal

{{trans|Ruby}}
Using an array

```ruby
[5,50,9000].each do |n|
  puts "%b" % n
end
```

Using a tuple

```ruby
{5,50,9000}.each { |n| puts n.to_s(2) }
```

{{out}}

```txt
101
110010
10001100101000
```



## D


```d
void main() {
    import std.stdio;

    foreach (immutable i; 0 .. 16)
        writefln("%b", i);
}
```

{{out}}

```txt
0
1
10
11
100
101
110
111
1000
1001
1010
1011
1100
1101
1110
1111
```



## Dart


```dart
String binary(int n) {
  if(n<0)
    throw new IllegalArgumentException("negative numbers require 2s complement");
  if(n==0) return "0";
  String res="";
  while(n>0) {
    res=(n%2).toString()+res;
    n=(n/2).toInt();
  }
  return res;
}

main() {
  print(binary(0));
  print(binary(1));
  print(binary(5));
  print(binary(10));
  print(binary(50));
  print(binary(9000));
  print(binary(65535));
  print(binary(0xaa5511ff));
  print(binary(0x123456789abcde));
  // fails due to precision limit
  print(binary(0x123456789abcdef));
}
```



## dc


```dc
2o 5p 50p 9000p>
```


{{out}}

```txt
101
110010
10001100101000
```



## Delphi


```Delphi

program BinaryDigit;
{$APPTYPE CONSOLE}
uses
  sysutils;

function IntToBinStr(AInt : LongWord) : string;
begin
  Result := '';
  repeat
    Result := Chr(Ord('0')+(AInt and 1))+Result;
    AInt := AInt div 2;
  until (AInt = 0);
end;

Begin
  writeln('   5: ',IntToBinStr(5));
  writeln('  50: ',IntToBinStr(50));
  writeln('9000: '+IntToBinStr(9000));
end.
```

{{out}}

```txt

   5: 101
  50: 110010
9000: 10001100101000

```



## EchoLisp


```scheme

;; primitive : (number->string number [base]) - default base = 10

(number->string 2 2)
→ 10

(for-each (compose writeln (rcurry number->string 2)) '( 5 50 9000)) →
101
110010
10001100101000

```



## Dyalect


A default <code>toString</code> method of type <code>Integer</code> is overriden and returns a binary representation of a number:


```dyalect
func Integer.toString() {
    var s = ""
    for x in 31..0 {
        if this & (1 << x) != 0 {
            s += "1"
        } else if s != "" {
            s += "0"
        }
    }
    s
}

print("5 == \(5), 50 = \(50), 1000 = \(9000)")
```


{{out}}


```txt
5 == 101, 50 = 110010, 1000 = 10001100101000
```



## EasyLang


<lang>func to2 n . r$ .
  if n > 0
    call to2 n / 2 r$
    if n mod 2 = 0
      r$ &= "0"
    else
      r$ &= "1"
    .
  else
    r$ = ""
  .
.
func pr2 n . .
  call to2 n r$
  if r$ = ""
    print "0"
  else
    print r$
  .
.
call pr2 5
call pr2 50
call pr2 9000
```



```txt

101
110010
10001100101000

```



## Elena

ELENA 4.1 :

```elena
import system'routines;
import extensions;

public program()
{
    new int[]::(5,50,9000).forEach:(n)
    {
        console.printLine(n.toString(2))
    }
}
```

{{out}}

```txt

101
110010
10001100101000

```



## Elixir

Use <code>Integer.to_string</code> with a base of 2:

```Elixir

IO.puts Integer.to_string(5,2)

```

Or, using the pipe operator:

```Elixir

5 |> Integer.to_string(2) |> IO.puts

```


```Elixir

[5,50,9000] |> Enum.each(fn n -> IO.puts Integer.to_string(n,2) end)

```


{{out}}

```txt

101
110010
10001100101000

```



## Erlang


```erlang
lists:map( fun(N) -> io:fwrite("~.2B~n", [N]) end, [5, 50, 9000]).
```

{{out}}

```txt
101
110010
10001100101000
```



## Euphoria


```euphoria
function toBinary(integer i)
    sequence s
    s = {}
    while i do
        s = prepend(s, '0'+and_bits(i,1))
        i = floor(i/2)
    end while
    return s
end function

puts(1, toBinary(5) & '\n')
puts(1, toBinary(50) & '\n')
puts(1, toBinary(9000) & '\n')
```



###  Functional/Recursive


```euphoria
include std/math.e
include std/convert.e

function Bin(integer n, sequence s = "")
  if n > 0 then
   return Bin(floor(n/2),(mod(n,2) + #30) & s)
  end if
  if length(s) = 0 then
   return to_integer("0")
  end if
  return to_integer(s)
end function

printf(1, "%d\n", Bin(5))
printf(1, "%d\n", Bin(50))
printf(1, "%d\n", Bin(9000))
```


=={{header|F Sharp|F#}}==
By translating C#'s approach, using imperative coding style (inflexible):

```FSharp
open System
for i in [5; 50; 9000] do printfn "%s" <| Convert.ToString (i, 2)
```


Alternatively, by creating a function <code>printBin</code> which prints in binary (more flexible):

```FSharp
open System

// define the function
let printBin (i: int) =
    Convert.ToString (i, 2)
    |> printfn "%s"

// use the function
[5; 50; 9000]
|> List.iter printBin
```


Or more idiomatic so that you can use it with any printf-style function and the <code>%a</code> format specifier (most flexible):


```FSharp
open System
open System.IO

// define a callback function for %a
let bin (tw: TextWriter) value =
    tw.Write("{0}", Convert.ToString(int64 value, 2))

// use it with printfn with %a
[5; 50; 9000]
|> List.iter (printfn "binary: %a" bin)
```

Output (either version):

```txt

101
110010
10001100101000

```



## Factor


```factor
USING: io kernel math math.parser ;

5 >bin print
50 >bin print
9000 >bin print
```



## FBSL


```fbsl
#AppType Console
function Bin(byval n as integer, byval s as string = "") as string
	if n > 0 then return Bin(n \ 2, (n mod 2) & s)
	if s = "" then return "0"
	return s
end function

print Bin(5)
print Bin(50)
print Bin(9000)

pause

```



## Forth


```forth
\ Forth uses a system variable 'BASE' for number conversion

\ HEX is a standard word to change the value of base to 16
\ DECIMAL is a standard word to change the value of base to 10

\ we can easily compile a word into the system to set 'BASE' to 2

  : binary  2 base ! ; ok


\ interactive console test with conversion and binary masking example

hex 0FF binary . 11111111  ok
decimal 679 binary . 1010100111  ok
  ok
binary  11111111111  00000110000  and . 110000  ok

decimal ok


```



## Fortran

Please find compilation instructions and the example run at the start of the FORTRAN90 source that follows.  Thank you.

```FORTRAN

!-*- mode: compilation; default-directory: "/tmp/" -*-
!Compilation started at Sun May 19 23:14:14
!
!a=./F && make $a && $a < unixdict.txt
!f95 -Wall -ffree-form F.F -o F
!101
!110010
!10001100101000
!
!Compilation finished at Sun May 19 23:14:14
!
!
!   tobin=: -.&' '@":@#:
!   tobin 5
!101
!   tobin 50
!110010
!   tobin 9000
!10001100101000

program bits
  implicit none
  integer, dimension(3) :: a
  integer :: i
  data a/5,50,9000/
  do i = 1, 3
    call s(a(i))
  enddo

contains

  subroutine s(a)
    integer, intent(in) :: a
    integer :: i
    if (a .eq. 0) then
      write(6,'(a)')'0'
      return
    endif
    do i = 31, 0, -1
      if (btest(a, i)) exit
    enddo
    do while (0 .lt. i)
      if (btest(a, i)) then
        write(6,'(a)',advance='no')'1'
      else
        write(6,'(a)',advance='no')'0'
      endif
      i = i-1
    enddo
    if (btest(a, i)) then
      write(6,'(a)')'1'
    else
      write(6,'(a)')'0'
    endif
  end subroutine s

end program bits

```



## FreeBASIC


```freebasic

' FreeBASIC v1.05.0 win64
Dim As String fmt = "#### -> &"
Print Using fmt; 5; Bin(5)
Print Using fmt; 50; Bin(50)
Print Using fmt; 9000; Bin(9000)
Print
Print "Press any key to exit the program"
Sleep
End

```


{{out}}

```txt

   5 -> 101
  50 -> 110010
9000 -> 10001100101000

```



## Free Pascal

As part of the RTL (run-time library) that is shipped with every FPC (Free Pascal compiler) distribution, the <tt>system</tt> unit contains the function <tt>binStr</tt>.
The <tt>system</tt> unit is automatically included by ''every'' program and is guaranteed to work on every supported platform.

```pascal
program binaryDigits(input, output, stdErr);
{$mode ISO}

function binaryNumber(const value: nativeUInt): shortString;
const
	one = '1';
var
	representation: shortString;
begin
	representation := binStr(value, bitSizeOf(value));
	// strip leading zeroes, if any; NB: mod has to be ISO compliant
	delete(representation, 1, (pos(one, representation)-1) mod bitSizeOf(value));
	// traditional Pascal fashion:
	// assign result to the (implicitely existent) variable
	// that is named like the function’s name
	binaryNumber := representation;
end;

begin
	writeLn(binaryNumber(5));
	writeLn(binaryNumber(50));
	writeLn(binaryNumber(9000));
end.
```

Note, that the ISO compliant <tt>mod</tt> operation has to be used, which is ensured by the <tt>{$mode}</tt> directive in the second line.


## Frink

The following all provide equivalent output.  Input can be arbitrarily-large integers.

```frink

9000 -> binary
9000 -> base2
base2[9000]
base[9000, 2]

```



## FunL


```funl
for n <- [5, 50, 9000, 9000000000]
  println( n, bin(n) )
```


{{out}}


```txt

5, 101
50, 110010
9000, 10001100101000
9000000000, 1000011000011100010001101000000000

```



## Futhark


We produce the binary number as a 64-bit integer whose digits are all 0s and 1s - this is because Futhark does not have any way to print, nor strings for that matter.


```Futhark

fun main(x: i32): i64 =
  loop (out = 0i64) = for i < 32 do
    let digit = (x >> (31-i)) & 1
    let out = (out * 10i64) + i64(digit)
    in out
  in out

```



## Gambas

'''[https://gambas-playground.proko.eu/?gist=03e84768e6ee2af9b7664efa04fa6da8 Click this link to run this code]'''

```gambas
Public Sub Main()
Dim siBin As Short[] = [5, 50, 9000]
Dim siCount As Short

For siCount = 0 To siBin.Max
  Print Bin(siBin[siCount])
Next

End
```

{{out}}

```txt

101
110010
10001100101000

```



## Go


```go
package main

import (
	"fmt"
)

func main() {
	for i := 0; i < 16; i++ {
		fmt.Printf("%b\n", i)
	}
}
```

{{out}}

```txt

0
1
10
11
100
101
110
111
1000
1001
1010
1011
1100
1101
1110
1111

```



## Groovy

Solutions:

```groovy
print '''
  n        binary
----- ---------------
'''
[5, 50, 9000].each {
    printf('%5d %15s\n', it, Integer.toBinaryString(it))
}
```

{{out}}

```txt
  n        binary
----- ---------------
    5             101
   50          110010
 9000  10001100101000
```



## Haskell


```haskell
import Data.List
import Numeric
import Text.Printf

-- Use the built-in function showIntAtBase.
toBin n = showIntAtBase 2 ("01" !!) n ""

-- Implement our own version.
toBin1 0 = []
toBin1 x =  (toBin1 $ x `div` 2) ++ (show $ x `mod` 2)

-- Or even more efficient (due to fusion) and universal implementation
toBin2 = foldMap show . reverse . toBase 2

toBase base = unfoldr modDiv
  where modDiv 0 = Nothing
        modDiv n = let (q, r) = (n `divMod` base) in Just (r, q)


printToBin n = putStrLn $ printf "%4d  %14s  %14s" n (toBin n) (toBin1 n)

main = do
  putStrLn $ printf "%4s  %14s  %14s" "N" "toBin" "toBin1"
  mapM_ printToBin [5, 50, 9000]
```

{{out}}

```txt

   N           toBin          toBin1
   5             101             101
  50          110010          110010
9000  10001100101000  10001100101000

```


=={{header|Icon}} and {{header|Unicon}}==
There is no built-in way to output the bit string representation of an whole number in Icon and Unicon.  There are generalized radix conversion routines in the Icon Programming Library that comes with every distribution.  This procedure is a customized conversion routine that will populate and use a tunable cache as it goes.

```Icon
procedure main()
every i := 5 | 50 | 255 | 1285 | 9000 do
  write(i," = ",binary(i))
end

procedure binary(n)                      #: return bitstring for integer n
static CT, cm, cb
initial {
   CT := table()                         # cache table for results
   cm := 2 ^ (cb := 4)                   # (tunable) cache modulus & pad bits
   }

b := ""                                  # build reversed bit string
while n > 0 do {                         # use cached result ...
   if not (b ||:= \CT[1(i := n % cm, n /:= cm) ]) then {
      CT[j := i] := ""                   # ...or start new cache entry
      while j > 0 do
         CT[i] ||:=  "01"[ 1(1+j % 2, j /:= 2 )]
      b ||:= CT[i] := left(CT[i],cb,"0") # finish cache with padding
      }
   }
return reverse(trim(b,"0"))              # nothing extraneous
end
```

{{out}}

```txt
5 = 101
50 = 110010
255 = 11111111
1285 = 10100000101
9000 = 10001100101000
```




## Idris


```Idris
module Main

binaryDigit : Integer -> Char
binaryDigit n = if (mod n 2) == 1 then '1' else '0'

binaryString : Integer -> String
binaryString 0 = "0"
binaryString n = pack (loop n [])
  where loop : Integer -> List Char -> List Char
        loop 0 acc = acc
        loop n acc = loop (div n 2) (binaryDigit n :: acc)

main : IO ()
main = do
  putStrLn (binaryString 0)
  putStrLn (binaryString 5)
  putStrLn (binaryString 50)
  putStrLn (binaryString 9000)

```

{{out}}

```txt

0
101
110010
10001100101000

```



## J


```j
   tobin=: -.&' '@":@#:
   tobin 5
101
   tobin 50
110010
   tobin 9000
10001100101000
```

Algorithm: Remove spaces from the character list which results from formatting the binary list which represents the numeric argument.

I am using implicit output.


## Java


```java
public class Main {
    public static void main(String[] args) {
        System.out.println(Integer.toBinaryString(5));
        System.out.println(Integer.toBinaryString(50));
        System.out.println(Integer.toBinaryString(9000));
    }
}
```

{{out}}

```txt
101
110010
10001100101000
```



## JavaScript


### ES5


```javascript
function toBinary(number) {
    return new Number(number)
        .toString(2);
}
var demoValues = [5, 50, 9000];
for (var i = 0; i < demoValues.length; ++i) {
    // alert() in a browser, wscript.echo in WSH, etc.
    print(toBinary(demoValues[i]));
}
```



### ES6

The simplest showBin (or showIntAtBase), using default digit characters, would use JavaScript's standard String.toString(base):


```JavaScript
(() => {

    // showIntAtBase_ :: // Int -> Int -> String
    const showIntAtBase_ = (base, n) => (n)
        .toString(base);

    // showBin :: Int -> String
    const showBin = n => showIntAtBase_(2, n);

    // GENERIC FUNCTIONS FOR TEST ---------------------------------------------

    // intercalate :: String -> [a] -> String
    const intercalate = (s, xs) => xs.join(s);

    // map :: (a -> b) -> [a] -> [b]
    const map = (f, xs) => xs.map(f);

    // unlines :: [String] -> String
    const unlines = xs => xs.join('\n');

    // show :: a -> String
    const show = x => JSON.stringify(x);

    // TEST -------------------------------------------------------------------

    return unlines(map(
        n => intercalate(' -> ', [show(n), showBin(n)]),
        [5, 50, 9000]
    ));
})();
```

{{Out}}

```txt
5 -> 101
50 -> 110010
9000 -> 10001100101000
```


Or, if we need more flexibility with the set of digits used, we can write a version of showIntAtBase which takes a more specific Int -> Char function as as an argument. This one is a rough translation of Haskell's Numeric.showIntAtBase:


```JavaScript
(() => {

    // showBin :: Int -> String
    const showBin = n => {
        const binaryChar = n => n !== 0 ? '一' : '〇';

        return showIntAtBase(2, binaryChar, n, '');
    };

    // showIntAtBase :: Int -> (Int -> Char) -> Int -> String -> String
    const showIntAtBase = (base, toChr, n, rs) => {
        const showIt = ([n, d], r) => {
            const r_ = toChr(d) + r;
            return n !== 0 ? (
                showIt(quotRem(n, base), r_)
            ) : r_;
        };
        return base <= 1 ? (
            'error: showIntAtBase applied to unsupported base'
        ) : n < 0 ? (
            'error: showIntAtBase applied to negative number'
        ) : showIt(quotRem(n, base), rs);
    };

    // quotRem :: Integral a => a -> a -> (a, a)
    const quotRem = (m, n) => [Math.floor(m / n), m % n];

    // GENERIC FUNCTIONS FOR TEST ---------------------------------------------

    // intercalate :: String -> [a] -> String
    const intercalate = (s, xs) => xs.join(s);

    // map :: (a -> b) -> [a] -> [b]
    const map = (f, xs) => xs.map(f);

    // unlines :: [String] -> String
    const unlines = xs => xs.join('\n');

    // show :: a -> String
    const show = x => JSON.stringify(x);

    // TEST -------------------------------------------------------------------

    return unlines(map(
        n => intercalate(' -> ', [show(n), showBin(n)]), [5, 50, 9000]
    ));
})();
```

{{Out}}

```txt
5 -> 一〇一
50 -> 一一〇〇一〇
9000 -> 一〇〇〇一一〇〇一〇一〇〇〇
```



## Joy


```joy
HIDE
_ == [null] [pop] [2 div swap] [48 + putch] linrec
IN
int2bin == [null] [48 + putch] [_] ifte '\n putch
END
```

Using int2bin:

```joy
0 setautoput
0 int2bin
5 int2bin
50 int2bin
9000 int2bin.
```


## jq


```jq
def binary_digits:
  if . == 0 then "0"
  else [recurse( if . == 0 then empty else ./2 | floor end ) % 2 | tostring]
    | reverse
    | .[1:]     # remove the leading 0
    | join("")
  end ;

# The task:
(5, 50, 9000) | binary_digits
```

{{Out}}
 $ jq -n -r -f Binary_digits.jq
 101
 110010
 10001100101000


## Julia

{{works with|Julia|1.0}}


```julia
using Printf

for n in (0, 5, 50, 9000)
    @printf("%6i → %s\n", n, string(n, base=2))
end

# with pad
println("\nwith pad")
for n in (0, 5, 50, 9000)
    @printf("%6i → %s\n", n, string(n, base=2, pad=20))
end
```


{{out}}

```txt
     0 → 0
     5 → 101
    50 → 110010
  9000 → 10001100101000

with pad
     0 → 00000000000000000000
     5 → 00000000000000000101
    50 → 00000000000000110010
  9000 → 00000010001100101000
```



## K


```k
  tobin: ,/$2_vs
  tobin' 5 50 9000
("101"
 "110010"
 "10001100101000")
```



## Kotlin


```scala
// version 1.0.5-2

fun main(args: Array<String>) {
    val numbers = intArrayOf(5, 50, 9000)
    for (number in numbers) println("%4d".format(number) + " -> " + Integer.toBinaryString(number))
}
```


{{out}}

```txt

   5 -> 101
  50 -> 110010
9000 -> 10001100101000

```



## Lang5


```lang5
'%b '__number_format set
[5 50 9000] [3 1] reshape .
```

{{out}}

```txt
[
  [ 101  ]
  [ 110010  ]
  [ 10001100101000  ]
]
```



## LFE


If one is simple printing the results and doesn't need to use them (e.g., assign them to any variables, etc.), this is very concise:

```lisp

(: io format '"~.2B~n~.2B~n~.2B~n" (list 5 50 9000))

```


If, however, you do need to get the results from a function, you can use <code>(: erlang integer_to_list ... )</code>. Here's a simple example that does the same thing as the previous code:

```lisp

(: lists foreach
  (lambda (x)
    (: io format
      '"~s~n"
      (list (: erlang integer_to_list x 2))))
  (list 5 50 9000))

```

{{out|note=for both examples}}

```txt

101
110010
10001100101000

```



## Liberty BASIC


```lb
for a = 0 to 16
print a;"=";dec2bin$(a)
next
a=50:print a;"=";dec2bin$(a)
a=254:print a;"=";dec2bin$(a)
a=9000:print a;"=";dec2bin$(a)
wait

function dec2bin$(num)
   if num=0 then dec2bin$="0":exit function
    while num>0
        dec2bin$=str$(num mod 2)+dec2bin$
        num=int(num/2)
    wend
end function

```



## LLVM

{{trans|C}}

```llvm
; ModuleID = 'binary.c'
; source_filename = "binary.c"
; target datalayout = "e-m:w-i64:64-f80:128-n8:16:32:64-S128"
; target triple = "x86_64-pc-windows-msvc19.21.27702"

; This is not strictly LLVM, as it uses the C library function "printf".
; LLVM does not provide a way to print values, so the alternative would be
; to just load the string into memory, and that would be boring.

; Additional comments have been inserted, as well as changes made from the output produced by clang such as putting more meaningful labels for the jumps

$"\01??_C@_03OFAPEBGM@?$CFs?6?$AA@" = comdat any

;--- String constant defintions
@"\01??_C@_03OFAPEBGM@?$CFs?6?$AA@" = linkonce_odr unnamed_addr constant [4 x i8] c"%s\0A\00", comdat, align 1

;--- The declaration for the external C printf function.
declare i32 @printf(i8*, ...)

;--- The declaration for the external C log10 function.
declare double @log10(double) #1

;--- The declaration for the external C malloc function.
declare noalias i8* @malloc(i64) #2

;--- The declaration for the external C free function.
declare void @free(i8*) #2

;----------------------------------------------------------
;-- Function that allocates a string with a binary representation of a number
define i8* @bin(i32) #0 {
;-- uint32_t x (local copy)
  %2 = alloca i32, align 4
;-- size_t bits
  %3 = alloca i64, align 8
;-- intermediate value
  %4 = alloca i8*, align 8
;-- size_t i
  %5 = alloca i64, align 8
  store i32 %0, i32* %2, align 4
;-- x == 0, start determinig what value to initially store in bits
  %6 = load i32, i32* %2, align 4
  %7 = icmp eq i32 %6, 0
  br i1 %7, label %just_one, label %calculate_logs

just_one:
  br label %assign_bits

calculate_logs:
;-- log10((double) x)/log10(2) + 1
  %8 = load i32, i32* %2, align 4
  %9 = uitofp i32 %8 to double
;-- log10((double) x)
  %10 = call double @log10(double %9) #3
;-- log10(2)
  %11 = call double @log10(double 2.000000e+00) #3
;-- remainder of calculation
  %12 = fdiv double %10, %11
  %13 = fadd double %12, 1.000000e+00
  br label %assign_bits

assign_bits:
;-- bits = (x == 0) ? 1 : log10((double) x)/log10(2) + 1;
;-- phi basically selects what the value to assign should be based on which basic block came before
  %14 = phi double [ 1.000000e+00, %just_one ], [ %13, %calculate_logs ]
  %15 = fptoui double %14 to i64
  store i64 %15, i64* %3, align 8
;-- char *ret = malloc((bits + 1) * sizeof (char));
  %16 = load i64, i64* %3, align 8
  %17 = add i64 %16, 1
  %18 = mul i64 %17, 1
  %19 = call noalias i8* @malloc(i64 %18)
  store i8* %19, i8** %4, align 8
  store i64 0, i64* %5, align 8
  br label %loop

loop:
;-- i < bits;
  %20 = load i64, i64* %5, align 8
  %21 = load i64, i64* %3, align 8
  %22 = icmp ult i64 %20, %21
  br i1 %22, label %loop_body, label %exit

loop_body:
;-- ret[bits - i - 1] = (x & 1) ? '1' : '0';
  %23 = load i32, i32* %2, align 4
  %24 = and i32 %23, 1
  %25 = icmp ne i32 %24, 0
  %26 = zext i1 %25 to i64
  %27 = select i1 %25, i32 49, i32 48
  %28 = trunc i32 %27 to i8
  %29 = load i8*, i8** %4, align 8
  %30 = load i64, i64* %3, align 8
  %31 = load i64, i64* %5, align 8
  %32 = sub i64 %30, %31
  %33 = sub i64 %32, 1
  %34 = getelementptr inbounds i8, i8* %29, i64 %33
  store i8 %28, i8* %34, align 1
;-- x >>= 1;
  %35 = load i32, i32* %2, align 4
  %36 = lshr i32 %35, 1
  store i32 %36, i32* %2, align 4
  br label %loop_increment

loop_increment:
;-- i++;
  %37 = load i64, i64* %5, align 8
  %38 = add i64 %37, 1
  store i64 %38, i64* %5, align 8
  br label %loop

exit:
;-- ret[bits] = '\0';
  %39 = load i8*, i8** %4, align 8
  %40 = load i64, i64* %3, align 8
  %41 = getelementptr inbounds i8, i8* %39, i64 %40
  store i8 0, i8* %41, align 1
;-- return ret;
  %42 = load i8*, i8** %4, align 8
  ret i8* %42
}

;----------------------------------------------------------
;-- Entry point into the program
define i32 @main() #0 {
;-- 32-bit zero for the return
  %1 = alloca i32, align 4
;-- size_t i, for tracking the loop index
  %2 = alloca i64, align 8
;-- char* for the result of the bin call
  %3 = alloca i8*, align 8
;-- initialize
  store i32 0, i32* %1, align 4
  store i64 0, i64* %2, align 8
  br label %loop

loop:
;-- while (i < 20)
  %4 = load i64, i64* %2, align 8
  %5 = icmp ult i64 %4, 20
  br i1 %5, label %loop_body, label %exit

loop_body:
;-- char *binstr = bin(i);
  %6 = load i64, i64* %2, align 8
  %7 = trunc i64 %6 to i32
  %8 = call i8* @bin(i32 %7)
  store i8* %8, i8** %3, align 8
;-- printf("%s\n", binstr);
  %9 = load i8*, i8** %3, align 8
  %10 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @"\01??_C@_03OFAPEBGM@?$CFs?6?$AA@", i32 0, i32 0), i8* %9)
;-- free(binstr);
  %11 = load i8*, i8** %3, align 8
  call void @free(i8* %11)
  br label %loop_increment

loop_increment:
;-- i++
  %12 = load i64, i64* %2, align 8
  %13 = add i64 %12, 1
  store i64 %13, i64* %2, align 8
  br label %loop

exit:
;-- return 0 (implicit)
  %14 = load i32, i32* %1, align 4
  ret i32 %14
}

attributes #0 = { noinline nounwind optnone uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { nounwind "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #2 = { "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #3 = { nounwind }

!llvm.module.flags = !{!0, !1}
!llvm.ident = !{!2}

!0 = !{i32 1, !"wchar_size", i32 2}
!1 = !{i32 7, !"PIC Level", i32 2}
!2 = !{!"clang version 6.0.1 (tags/RELEASE_601/final)"}
```

{{out}}

```txt
0
1
10
11
100
101
110
111
1000
1001
1010
1011
1100
1101
1110
1111
10000
10001
10010
10011
```



## Locomotive Basic


```locobasic
10 PRINT BIN$(5)
20 PRINT BIN$(50)
30 PRINT BIN$(9000)
```

{{out}}

```txt
101
110010
10001100101000
```



## LOLCODE


```LOLCODE
HAI 1.3
HOW IZ I DECIMULBINUR YR DECIMUL
  I HAS A BINUR ITZ ""
  IM IN YR DUUH
    BOTH SAEM DECIMUL AN SMALLR OF DECIMUL AN 0, O RLY?
      YA RLY, GTFO
    OIC
    BINUR R SMOOSH MOD OF DECIMUL AN 2 BINUR MKAY
    DECIMUL R MAEK QUOSHUNT OF DECIMUL AN 2 A NUMBR
  IM OUTTA YR DUUH
  FOUND YR BINUR
IF U SAY SO
VISIBLE I IZ DECIMULBINUR YR 5 MKAY
VISIBLE I IZ DECIMULBINUR YR 50 MKAY
VISIBLE I IZ DECIMULBINUR YR 9000 MKAY
KTHXBYE
```


{{out}}

```txt
101
110010
10001100101000
```



## Lua


```Lua
function dec2bin (n)
    local bin = ""
    while n > 0 do
        bin = n % 2 .. bin
        n = math.floor(n / 2)
    end
    return bin
end

print(dec2bin(5))
print(dec2bin(50))
print(dec2bin(9000))
```

{{out}}

```txt
101
110010
10001100101000
```



## M2000 Interpreter


```M2000 Interpreter

Module Checkit {
      Form 90, 40
      Function BinFunc${
            Dim  Base 0, One$(16)
            One$( 0 ) = "0000", "0001", "0010", "0011", "0100", "0101", "0110", "0111", "1000", "1001", "1010", "1011", "1100", "1101", "1110", "1111"
            =lambda$ One$() (x, oct as long=4, bypass as boolean=True) ->{
                  if oct>0 and oct<5 then {
                       oct=2*(int(4-oct) mod 4+1)-1
                  } Else oct=1
                  hx$ = Hex$(x, 4 )
                  Def Ret$
                  If Bypass then {
                        For i= oct to len(hx$)
                              if bypass Then if Mid$(hx$, i, 1 )="0" Else bypass=false
                              If bypass and i<>Len(hx$) Then Continue
                              Ret$ += One$( EVal( "0x" + Mid$(hx$, i, 1 ) ) )
                        Next i
                        oct=instr(Ret$, "1")
                        if oct=0 then {
                               Ret$="0"
                        } Else Ret$=mid$(Ret$, oct)
                  } Else {
                        For i= oct to len(hx$)
                              Ret$ += One$( EVal( "0x" + Mid$(hx$, i, 1 ) ) )
                        Next i
                  }
                  =Ret$
            }
      }
      Bin$=BinFunc$()
      Stack New {
            Data 9, 50, 9000
            While not empty {
                  Read x
                  Print Format$("The decimal value {0::-10} should produce an output of {1:-32}",x, Bin$(x) )
            }
      }
      Stack New {
            Data 9, 50, 9000
            While not empty {
                  Read x
                  Print Format$("The decimal value {0::-10} should produce an output of {1:-32}",x, Bin$(x,,false) )
            }
      }
      Stack New {
            Data 9, 50, 9000
            While not empty {
                  Read x
                  Print Bin$(x)
            }
      }
}
Checkit

```

{{out}}

```txt
The decimal value          9 should produce an output of                             1001
The decimal value         50 should produce an output of                           110010
The decimal value       9000 should produce an output of                   10001100101000
The decimal value          9 should produce an output of 00000000000000000000000000001001
The decimal value         50 should produce an output of 00000000000000000000000000110010
The decimal value       9000 should produce an output of 00000000000000000010001100101000
1001
110010
10001100101000

</pre >


## Maple


```Maple

> convert( 50, 'binary' );
110010
> convert( 9000, 'binary' );
10001100101000

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
StringJoin @@ ToString /@ IntegerDigits[50, 2]
```


=={{header|MATLAB}} / {{header|Octave}}==

```Matlab
  dec2bin(5)
  dec2bin(50)
  dec2bin(9000)
```

The output is a string containing ascii(48) (i.e. '0') and ascii(49) (i.e. '1').


## MAXScript


```maxscript

-- MAXScript: Output decimal numbers from 0 to 16 as Binary : N.H. 2019
for k = 0 to 16 do
(
temp = ""
binString = ""
b = k
-- While loop wont execute for zero so force string to zero
if b == 0 then temp = "0"
	while b > 0 do
	(
	rem = b
	b = b / 2
	   If ((mod rem 2) as Integer) == 0 then temp = temp + "0"
	   else temp = temp + "1"
    )
-- Reverse the binary string
for r = temp.count to 1 by -1 do
(
binString = binString + temp[r]
)
print binString
)

```

{{out}}
Output to MAXScript Listener:

```txt

"0"
"1"
"10"
"11"
"100"
"101"
"110"
"111"
"1000"
"1001"
"1010"
"1011"
"1100"
"1101"
"1110"
"1111"
"10000"

```



## Maxima


```maxima
digits([arg]) := block(
   [n: first(arg), b: if length(arg) > 1 then second(arg) else 10, v: [ ], q],
   do (
     [n, q]: divide(n, b),
     v: cons(q, v),
     if n=0 then return(v)))$

binary(n) := simplode(digits(n, 2))$
binary(9000);
/*
                                10001100101000
*/
```



## Mercury


```mercury
:- module binary_digits.
:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module int, list, string.

main(!IO) :-
    list.foldl(print_binary_digits, [5, 50, 9000], !IO).

:- pred print_binary_digits(int::in, io::di, io::uo) is det.

print_binary_digits(N, !IO) :-
    io.write_string(int_to_base_string(N, 2), !IO),
    io.nl(!IO).
```



## min

{{works with|min|0.19.3}}

```min
(2 over over mod 'div dip) :divmod2

(
  :n () =list
  (n 0 >) (n divmod2 list append #list @n) while
  list reverse 'string map "" join
  "^0+" "" replace   ;remove leading zeroes
) :bin

(5 50 9000) (bin puts) foreach
```

{{out}}

```txt

101
110010
10001100101000

```



## MiniScript


###  Iterative


```MiniScript
binary = function(n)
    result = ""
    while n
        result = result + str(n%2)
        n = floor(n/2)
    end while
    if not result then return "0"
    return result
end function

print binary(5)
print binary(50)
print binary(9000)
print binary(0)
```



###  Recursive


```MiniScript
binary = function(n,result="")
    if n == 0 then
        if result == "" then return "0" else return result
    end if
    result = str(n%2) + result
    return binary(floor(n/2),result)
end function

print binary(5)
print binary(50)
print binary(9000)
print binary(0)
```

{{out}}

```txt

101
110010
10001100101000
0

```



## mLite


```sml
fun binary
		(0, b)	=	implode ` map (fn x = if int x then chr (x + 48) else x) b
	|	(n, b)	=	binary (n div 2, n mod 2 :: b)
	|	n	=	binary (n, [])
;

```



### = from the REPL =


```txt
mLite
> binary 5;
"101"
> binary 50;
"110010"
> binary 9000;
"10001100101000"
```


=={{header|Modula-2}}==

```modula2
MODULE Binary;
FROM FormatString IMPORT FormatString;
FROM Terminal IMPORT Write,WriteLn,ReadChar;

PROCEDURE PrintByte(b : INTEGER);
VAR v : INTEGER;
BEGIN
    v := 080H;
    WHILE v#0 DO
        IF (b BAND v) # 0 THEN
            Write('1')
        ELSE
            Write('0')
        END;
        v := v SHR 1
    END
END PrintByte;

VAR
    buf : ARRAY[0..15] OF CHAR;
    i : INTEGER;
BEGIN
    FOR i:=0 TO 15 DO
        PrintByte(i);
        WriteLn
    END;

    ReadChar
END Binary.
```


=={{header|Modula-3}}==

```modula3
MODULE Binary EXPORTS Main;

IMPORT IO, Fmt;

VAR num := 10;

BEGIN
  IO.Put(Fmt.Int(num, 2) & "\n");
  num := 150;
  IO.Put(Fmt.Int(num, 2) & "\n");
END Binary.
```

{{out}}

```txt

1010
10010110

```



## NetRexx


```NetRexx
/* NetRexx */
options replace format comments java crossref symbols nobinary

runSample(arg)
return

method getBinaryDigits(nr) public static
  bd = nr.d2x.x2b.strip('L', 0)
  if bd.length = 0 then bd = 0
  return bd

method runSample(arg) public static
  parse arg list
  if list = '' then list = '0 5 50 9000'
  loop n_ = 1 to list.words
    w_ = list.word(n_)
    say w_.right(20)':' getBinaryDigits(w_)
    end n_
```

{{out}}

```txt

                   0: 0
                   5: 101
                  50: 110010
                9000: 10001100101000

```



## Nickle

Using the Nickle output radix operator:


```txt
prompt$ nickle
> 0 # 2
0
> 5 # 2
101
> 50 # 2
110010
> 9000 # 2
10001100101000
```



## Nim


```nim
proc binDigits(x: BiggestInt, r: int): int =
  ## Calculates how many digits `x` has when each digit covers `r` bits.
  result = 1
  var y = x shr r
  while y > 0:
    y = y shr r
    inc(result)

proc toBin*(x: BiggestInt, len: Natural = 0): string =
  ## converts `x` into its binary representation. The resulting string is
  ## always `len` characters long. By default the length is determined
  ## automatically. No leading ``0b`` prefix is generated.
  var
    mask: BiggestInt = 1
    shift: BiggestInt = 0
    len = if len == 0: binDigits(x, 1) else: len
  result = newString(len)
  for j in countdown(len-1, 0):
    result[j] = chr(int((x and mask) shr shift) + ord('0'))
    shift = shift + 1
    mask = mask shl 1

for i in 0..15:
  echo toBin(i)
```

{{out}}

```txt
0
1
10
11
100
101
110
111
1000
1001
1010
1011
1100
1101
1110
1111
```


=={{header|Oberon-2}}==

```oberon2

MODULE BinaryDigits;
IMPORT  Out;

  PROCEDURE OutBin(x: INTEGER);
  BEGIN
    IF x > 1 THEN OutBin(x DIV 2) END;
    Out.Int(x MOD 2, 1);
  END OutBin;


BEGIN
  OutBin(0); Out.Ln;
  OutBin(1); Out.Ln;
  OutBin(2); Out.Ln;
  OutBin(3); Out.Ln;
  OutBin(42); Out.Ln;
END BinaryDigits.

```


{{out}}

```txt

0
1
10
11
101010

```



## Objeck


```objeck
class Binary {
  function : Main(args : String[]) ~ Nil {
    5->ToBinaryString()->PrintLine();
    50->ToBinaryString()->PrintLine();
    9000->ToBinaryString()->PrintLine();
  }
}
```

{{out}}

```txt

101
110010
10001100101000

```



## OCaml


```ocaml
let bin_of_int d =
  if d < 0 then invalid_arg "bin_of_int" else
  if d = 0 then "0" else
  let rec aux acc d =
    if d = 0 then acc else
    aux (string_of_int (d land 1) :: acc) (d lsr 1)
  in
  String.concat "" (aux [] d)

let () =
  let d = read_int () in
  Printf.printf "%8s\n" (bin_of_int d)
```



## Oforth


{{out}}

```txt

>5 asStringOfBase(2) println
101
ok
>50 asStringOfBase(2) println
110010
ok
>9000 asStringOfBase(2) println
10001100101000
ok
>423785674235000123456789 asStringOfBase(2) println
1011001101111010111011110101001101111000000000000110001100000100111110100010101
ok

```



## OxygenBasic

The Assembly code uses block structures to minimise the use of labels.

```oxygenbasic


function BinaryBits(sys n) as string
  string buf=nuls 32
  sys p=strptr buf
  sys le
  mov eax,n
  mov edi,p
  mov ecx,32
  '
  'STRIP LEADING ZEROS
  (
   dec ecx
   jl fwd done
   shl eax,1
   jnc repeat
  )
  'PLACE DIGITS
  '
  mov byte [edi],49 '1'
  inc edi
  (
   cmp ecx,0
   jle exit
   mov dl,48 '0'
   shl eax,1
   (
    jnc exit
    mov dl,49 '1'
   )
   mov [edi],dl
   inc edi
   dec ecx
   repeat
  )
  done:
  '
  sub edi,p
  mov le,edi
  if le then return left buf,le
  return "0"
end function

print BinaryBits 0xaa 'result 10101010

```



## PARI/GP


```parigp
bin(n:int)=concat(apply(s->Str(s),binary(n)))
```



## Panda


```panda
0..15.radix:2 nl>
```

{{out}}

```txt
0
1
10
11
100
101
110
111
1000
1001
1010
1011
1100
1101
1110
1111
```


## Pascal

{{works with|Free Pascal}}
FPC compiler Version 2.6 upwards.The obvious version.

```pascal
program IntToBinTest;
{$MODE objFPC}
uses
  strutils;//IntToBin
function WholeIntToBin(n: NativeUInt):string;
var
  digits: NativeInt;
begin
// BSR?Word -> index of highest set bit but 0 -> 255 ==-1 )
  IF n <> 0 then
  Begin
{$ifdef CPU64}
    digits:= BSRQWord(NativeInt(n))+1;
{$ELSE}
    digits:= BSRDWord(NativeInt(n))+1;
{$ENDIF}
   WholeIntToBin := IntToBin(NativeInt(n),digits);
  end
  else
    WholeIntToBin:='0';
end;
procedure IntBinTest(n: NativeUint);
Begin
  writeln(n:12,' ',WholeIntToBin(n));
end;
BEGIN
  IntBinTest(5);IntBinTest(50);IntBinTest(5000);
  IntBinTest(0);IntBinTest(NativeUint(-1));
end.
```

{{out}}

```txt
           5 101
          50 110010
        5000 1001110001000
           0 0
18446744073709551615 1111111111111111111111111111111111111111111111111111111111111111
```



### alternative 4 chars at a time

using pchar like C insert one nibble at a time.
Beware of the endianess of the constant.
I check performance with random Data.

```pascal

program IntToPcharTest;
uses
  sysutils;//for timing

const
{$ifdef CPU64}
  cBitcnt = 64;
{$ELSE}
  cBitcnt = 32;
{$ENDIF}

procedure IntToBinPchar(AInt : NativeUInt;s:pChar);
//create the Bin-String
//!Beware of endianess ! this is for little endian
const
  IO : array[0..1] of char = ('0','1');//('_','X'); as you like
  IO4 : array[0..15] of LongWord = // '0000','1000' as LongWord
($30303030,$31303030,$30313030,$31313030,
 $30303130,$31303130,$30313130,$31313130,
 $30303031,$31303031,$30313031,$31313031,
 $30303131,$31303131,$30313131,$31313131);
var
  i : NativeInt;

begin
  IF AInt > 0 then
  Begin
  // Get the index of highest set bit
{$ifdef CPU64}
    i := BSRQWord(NativeInt(Aint))+1;
{$ELSE}
    i := BSRDWord(NativeInt(Aint))+1;
{$ENDIF}
    s[i] := #0;
    //get 4 characters at once
    dec(i);
    while i >= 3 do
    Begin
      pLongInt(@s[i-3])^ := IO4[Aint AND 15];
      Aint := Aint SHR 4;
      dec(i,4)
    end;
    //the rest one by one
    while i >= 0 do
    Begin
      s[i] := IO[Aint AND 1];
      AInt := Aint shr 1;
      dec(i);
    end;
  end
  else
  Begin
    s[0] := IO[0];
    s[1] := #0;
  end;
end;

procedure Binary_Digits;
var
 s: pCHar;
begin
  GetMem(s,cBitcnt+4);
  fillchar(s[0],cBitcnt+4,#0);
  IntToBinPchar(   5,s);writeln('   5: ',s);
  IntToBinPchar(  50,s);writeln('  50: ',s);
  IntToBinPchar(9000,s);writeln('9000: ',s);
  IntToBinPchar(NativeUInt(-1),s);writeln('  -1: ',s);
  FreeMem(s);
end;

const
  rounds = 10*1000*1000;

var
  s: pChar;
  t :TDateTime;
  i,l,cnt: NativeInt;
  Testfield : array[0..rounds-1] of NativeUint;
Begin
  randomize;
  cnt := 0;
  For i := rounds downto  1 do
  Begin
    l := random(High(NativeInt));
    Testfield[i] := l;
  {$ifdef CPU64}
    inc(cnt,BSRQWord(l));
  {$ELSE}
    inc(cnt,BSRQWord(l));
  {$ENDIF}
  end;
  Binary_Digits;
  GetMem(s,cBitcnt+4);
  fillchar(s[0],cBitcnt+4,#0);
  //warm up
  For i := 0 to rounds-1 do
    IntToBinPchar(Testfield[i],s);
  //speed test
  t := time;
  For i := 1 to rounds do
    IntToBinPchar(Testfield[i],s);
  t := time-t;
  Write(' Time ',t*86400.0:6:3,' secs, average stringlength ');
  Writeln(cnt/rounds+1:6:3);
  FreeMem(s);
end.
```

{{out}}

```txt

//32-Bit fpc 3.1.1 -O3 -XX -Xs  Cpu i4330 @3.5 Ghz
   5: 101
  50: 110010
9000: 10001100101000
  -1: 11111111111111111111111111111111
 Time  0.133 secs, average stringlength 30.000
  //64-Bit fpc 3.1.1 -O3 -XX -Xs
...
  -1: 1111111111111111111111111111111111111111111111111111111111111111
 Time  0.175 secs, average stringlength 62.000
..the obvious version takes about 1.1 secs generating the string takes most of the time..
```



## Peloton


```sgml
<@ defbaslit>2</@>


<@ saybaslit>0</@>
<@ saybaslit>5</@>
<@ saybaslit>50</@>
<@ saybaslit>9000</@>

```



## Perl


```perl
for (5, 50, 9000) {
  printf "%b\n", $_;
}
```


```txt

101
110010
10001100101000

```



## Perl 6

{{works with|Rakudo|2015.12}}

```perl6
say .fmt("%b") for 5, 50, 9000;
```


```txt

101
110010
10001100101000

```



## Phix


```Phix
printf(1,"%b\n",5)
printf(1,"%b\n",50)
printf(1,"%b\n",9000)
```

{{out}}

```txt

101
110010
10001100101000

```



## PHP


```php
<?php
echo decbin(5);
echo decbin(50);
echo decbin(9000);
```

{{out}}

```txt
101
110010
10001100101000
```



## PicoLisp


```PicoLisp
: (bin 5)
-> "101"

: (bin 50)
-> "110010"

: (bin 9000)
-> "10001100101000"
```



## Piet


Rendered as wikitable, because image upload is not possible:

{| style="border-collapse: collapse; border-spacing: 0; font-family: courier-new,courier,monospace; font-size: 18px; line-height: 1.2em; padding: 0px"
| style="background-color:#ffc0c0; color:#ffc0c0;" | ww
| style="background-color:#ffc0c0; color:#ffc0c0;" | ww
| style="background-color:#ff0000; color:#ff0000;" | ww
| style="background-color:#c0c0ff; color:#c0c0ff;" | ww
| style="background-color:#c0ffc0; color:#c0ffc0;" | ww
| style="background-color:#c0ffc0; color:#c0ffc0;" | ww
| style="background-color:#00ff00; color:#00ff00;" | ww
| style="background-color:#0000ff; color:#0000ff;" | ww
| style="background-color:#00ff00; color:#00ff00;" | ww
| style="background-color:#00c000; color:#00c000;" | ww
| style="background-color:#0000ff; color:#0000ff;" | ww
| style="background-color:#ffff00; color:#ffff00;" | ww
| style="background-color:#0000c0; color:#0000c0;" | ww
| style="background-color:#0000ff; color:#0000ff;" | ww
| style="background-color:#0000ff; color:#0000ff;" | ww
| style="background-color:#00c0c0; color:#00c0c0;" | ww
| style="background-color:#c0c000; color:#c0c000;" | ww
| style="background-color:#ffffc0; color:#ffffc0;" | ww
|-

| style="background-color:#ffffff; color:#ffffff;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#ffffff; color:#ffffff;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#0000c0; color:#0000c0;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#ffffff; color:#ffffff;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#c0c0ff; color:#c0c0ff;" | ww
|-

| style="background-color:#ffffff; color:#ffffff;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#00ff00; color:#00ff00;" | ww
| style="background-color:#c0c0ff; color:#c0c0ff;" | ww
| style="background-color:#0000c0; color:#0000c0;" | ww
| style="background-color:#0000ff; color:#0000ff;" | ww
| style="background-color:#0000ff; color:#0000ff;" | ww
| style="background-color:#c0ffc0; color:#c0ffc0;" | ww
| style="background-color:#00c000; color:#00c000;" | ww
| style="background-color:#00c000; color:#00c000;" | ww
| style="background-color:#0000ff; color:#0000ff;" | ww
| style="background-color:#c0c0ff; color:#c0c0ff;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#ffffff; color:#ffffff;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#c0c0ff; color:#c0c0ff;" | ww
|-

| style="background-color:#c000c0; color:#c000c0;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#ffffff; color:#ffffff;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#c0c0ff; color:#c0c0ff;" | ww
|-

| style="background-color:#ffc0ff; color:#ffc0ff;" | ww
| style="background-color:#ff0000; color:#ff0000;" | ww
| style="background-color:#ff00ff; color:#ff00ff;" | ww
| style="background-color:#ffff00; color:#ffff00;" | ww
| style="background-color:#ffffc0; color:#ffffc0;" | ww
| style="background-color:#ffffc0; color:#ffffc0;" | ww
| style="background-color:#ffffc0; color:#ffffc0;" | ww
| style="background-color:#ffffc0; color:#ffffc0;" | ww
| style="background-color:#ffffc0; color:#ffffc0;" | ww
| style="background-color:#ffffff; color:#ffffff;" | ww
| style="background-color:#ffffff; color:#ffffff;" | ww
| style="background-color:#ffffff; color:#ffffff;" | ww
| style="background-color:#ffffff; color:#ffffff;" | ww
| style="background-color:#ffffff; color:#ffffff;" | ww
| style="background-color:#ffffff; color:#ffffff;" | ww
| style="background-color:#ffffff; color:#ffffff;" | ww
| style="background-color:#ffffff; color:#ffffff;" | ww
| style="background-color:#ffff00; color:#ffff00;" | ww
|-

| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#ffffff; color:#ffffff;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#ffffff; color:#ffffff;" | ww
|-

| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#000000; color:#000000;" | ww
| style="background-color:#ffffc0; color:#ffffc0;" | ww
| style="background-color:#ffffff; color:#ffffff;" | ww
| style="background-color:#ffffff; color:#ffffff;" | ww
| style="background-color:#ffffc0; color:#ffffc0;" | ww
|-
|}

Examples:

    ? 5
    101
    ? 50
    110010
    ? 9000
    10001100101000


Explanation of program flow and image download link on my user page: [http://rosettacode.org/wiki/User:Albedo#Binary_Digits]


## PL/I

Displays binary output trivially, but with leading zeros:

```pli
put edit (25) (B);
```

{{out}}

```txt
Output: 0011001

```

With leading zero suppression:

```pli
   declare text character (50) initial (' ');

   put string(text) edit (25) (b);
   put skip list (trim(text, '0'));

   put string(text) edit (2147483647) (b);
   put skip list (trim(text, '0'));
```

{{out}}

```txt

11001
1111111111111111111111111111111

```




## PowerBASIC

Pretty simple task in PowerBASIC since it has a built-in BIN$-Function. Omitting the second parameter ("Digits") means no leading zeros in the result.

```powerbasic

#COMPILE EXE
#DIM ALL
#COMPILER PBCC 6

FUNCTION PBMAIN () AS LONG
LOCAL i, d() AS DWORD
REDIM d(2)
ARRAY ASSIGN d() = 5, 50, 9000
  FOR i = 0 TO 2
    PRINT STR$(d(i)) & ": " & BIN$(d(i)) & " (" & BIN$(d(i), 32) & ")"
  NEXT i
END FUNCTION
```

{{out}}
```txt

 5: 101 (00000000000000000000000000000101)
 50: 110010 (00000000000000000000000000110010)
 9000: 10001100101000 (00000000000000000010001100101000)

```




## PowerShell

{{libheader|Microsoft .NET Framework}}

```PowerShell
@(5,50,900) | foreach-object { [Convert]::ToString($_,2) }
```

{{out}}

```txt
101
110010
1110000100
```



## Processing


```processing
println(Integer.toBinaryString(5));     //            101
println(Integer.toBinaryString(50));    //         110010
println(Integer.toBinaryString(9000));  // 10001100101000
```

Processing also has a binary() function, but this returns zero-padded results

```processing
println(binary(5));     // 00000000000101
println(binary(50));    // 00000000110010
println(binary(9000));  // 10001100101000
```



## Prolog

{{works with|SWI Prolog}}
{{works with|GNU Prolog}}

```prolog

binary(X) :- format('~2r~n', [X]).
main :- maplist(binary, [5,50,9000]), halt.

```

{{out}}

```txt
101
110010
10001100101000
```



## PureBasic


```PureBasic
If OpenConsole()
  PrintN(Bin(5))    ;101
  PrintN(Bin(50))   ;110010
  PrintN(Bin(9000)) ;10001100101000

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
```

{{out}}

```txt
101
110010
10001100101000
```



## Python

===String.format() method===
{{works with|Python|3.X and 2.6+}}

```python
>>
 for i in range(16): print('{0:b}'.format(i))

0
1
10
11
100
101
110
111
1000
1001
1010
1011
1100
1101
1110
1111
```


===Built-in bin() function===
{{works with|Python|3.X and 2.6+}}

```python
>>
 for i in range(16): print(bin(i)[2:])

0
1
10
11
100
101
110
111
1000
1001
1010
1011
1100
1101
1110
1111
```

Pre-Python 2.6:

```python
>>
 oct2bin = {'0': '000', '1': '001', '2': '010', '3': '011', '4': '100', '5': '101', '6': '110', '7': '111'}
>>> bin = lambda n: ''.join(oct2bin[octdigit] for octdigit in '%o' % n).lstrip('0') or '0'
>>> for i in range(16): print(bin(i))

0
1
10
11
100
101
110
111
1000
1001
1010
1011
1100
1101
1110
1111
```



### Custom functions


Defined in terms of a more general '''showIntAtBase''' function:

```python
'''Binary strings for integers'''


# showBinary :: Int -> String
def showBinary(n):
    '''Binary string representation of an integer.'''
    def binaryChar(n):
        return '1' if n != 0 else '0'
    return showIntAtBase(2)(binaryChar)(n)('')


# TEST ----------------------------------------------------

# main :: IO()
def main():
    '''Test'''

    print('Mapping showBinary over integer list:')
    print(unlines(map(
        showBinary,
        [5, 50, 9000]
    )))

    print(tabulated(
        '\nUsing showBinary as a display function:'
    )(str)(showBinary)(
        lambda x: x
    )([5, 50, 9000]))


# GENERIC -------------------------------------------------

# compose (<<<) :: (b -> c) -> (a -> b) -> a -> c
def compose(g):
    '''Right to left function composition.'''
    return lambda f: lambda x: g(f(x))


# enumFromTo :: (Int, Int) -> [Int]
def enumFromTo(m):
    '''Integer enumeration from m to n.'''
    return lambda n: list(range(m, 1 + n))


# showIntAtBase :: Int -> (Int -> String) -> Int -> String -> String
def showIntAtBase(base):
    '''String representing a non-negative integer
       using the base specified by the first argument,
       and the character representation specified by the second.
       The final argument is a (possibly empty) string to which
       the numeric string will be prepended.'''
    def wrap(toChr, n, rs):
        def go(nd, r):
            n, d = nd
            r_ = toChr(d) + r
            return go(divmod(n, base), r_) if 0 != n else r_
        return 'unsupported base' if 1 >= base else (
            'negative number' if 0 > n else (
                go(divmod(n, base), rs))
        )
    return lambda toChr: lambda n: lambda rs: (
        wrap(toChr, n, rs)
    )


# tabulated :: String -> (a -> String) ->
#                        (b -> String) ->
#                        (a -> b) -> [a] -> String
def tabulated(s):
    '''Heading -> x display function -> fx display function ->
                f -> value list -> tabular string.'''
    def go(xShow, fxShow, f, xs):
        w = max(map(compose(len)(xShow), xs))
        return s + '\n' + '\n'.join(
            xShow(x).rjust(w, ' ') + ' -> ' + fxShow(f(x)) for x in xs
        )
    return lambda xShow: lambda fxShow: lambda f: lambda xs: go(
        xShow, fxShow, f, xs
    )


# unlines :: [String] -> String
def unlines(xs):
    '''A single string derived by the intercalation
       of a list of strings with the newline character.'''
    return '\n'.join(xs)


if __name__ == '__main__':
    main()
```

{{Out}}

```txt
Mapping showBinary over integer list:
101
110010
10001100101000

Using showBinary as a display function:
   5 -> 101
  50 -> 110010
9000 -> 10001100101000
```



Or, using a more specialised function to decompose an integer to a list of boolean values:

```python
'''Decomposition of an integer to a string of booleans.'''


# boolsFromInt :: Int -> [Bool]
def boolsFromInt(n):
    '''List of booleans derived by binary
       decomposition of an integer.'''
    def go(x):
        (q, r) = divmod(x, 2)
        return Just((q, bool(r))) if x else Nothing()
    return unfoldl(go)(n)


# stringFromBools :: [Bool] -> String
def stringFromBools(xs):
    '''Binary string representation of a
       list of boolean values.'''
    def oneOrZero(x):
        return '1' if x else '0'
    return ''.join(map(oneOrZero, xs))


# TEST ----------------------------------------------------
# main :: IO()
def main():
    '''Test'''

    binary = compose(stringFromBools)(boolsFromInt)

    print('Mapping a composed function:')
    print(unlines(map(
        binary,
        [5, 50, 9000]
    )))

    print(
        tabulated(
            '\n\nTabulating a string display from binary data:'
        )(str)(stringFromBools)(
            boolsFromInt
        )([5, 50, 9000])
    )


# GENERIC -------------------------------------------------

# Just :: a -> Maybe a
def Just(x):
    '''Constructor for an inhabited Maybe (option type) value.'''
    return {'type': 'Maybe', 'Nothing': False, 'Just': x}


# Nothing :: Maybe a
def Nothing():
    '''Constructor for an empty Maybe (option type) value.'''
    return {'type': 'Maybe', 'Nothing': True}


# compose (<<<) :: (b -> c) -> (a -> b) -> a -> c
def compose(g):
    '''Right to left function composition.'''
    return lambda f: lambda x: g(f(x))


# enumFromTo :: (Int, Int) -> [Int]
def enumFromTo(m):
    '''Integer enumeration from m to n.'''
    return lambda n: list(range(m, 1 + n))


# tabulated :: String -> (a -> String) ->
#                        (b -> String) ->
#                        (a -> b) -> [a] -> String
def tabulated(s):
    '''Heading -> x display function -> fx display function ->
                f -> value list -> tabular string.'''
    def go(xShow, fxShow, f, xs):
        w = max(map(compose(len)(xShow), xs))
        return s + '\n' + '\n'.join(
            xShow(x).rjust(w, ' ') + ' -> ' + fxShow(f(x)) for x in xs
        )
    return lambda xShow: lambda fxShow: lambda f: lambda xs: go(
        xShow, fxShow, f, xs
    )


# unfoldl(lambda x: Just(((x - 1), x)) if 0 != x else Nothing())(10)
# -> [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
# unfoldl :: (b -> Maybe (b, a)) -> b -> [a]
def unfoldl(f):
    '''Dual to reduce or foldl.
       Where these reduce a list to a summary value, unfoldl
       builds a list from a seed value.
       Where f returns Just(a, b), a is appended to the list,
       and the residual b is used as the argument for the next
       application of f.
       When f returns Nothing, the completed list is returned.'''
    def go(v):
        xr = v, v
        xs = []
        while True:
            mb = f(xr[0])
            if mb.get('Nothing'):
                return xs
            else:
                xr = mb.get('Just')
                xs.insert(0, xr[1])
        return xs
    return lambda x: go(x)


# unlines :: [String] -> String
def unlines(xs):
    '''A single string derived by the intercalation
       of a list of strings with the newline character.'''
    return '\n'.join(xs)


# MAIN -------------------------------------------------
if __name__ == '__main__':
    main()
```

{{Out}}

```txt
Mapping a composed function:
101
110010
10001100101000

Tabulating a string display from binary data:
   5 -> 101
  50 -> 110010
9000 -> 10001100101000
```


## R


```rsplus

dec2bin <- function(num) {
  ifelse(num == 0,
         0,
         sub("^0+","",paste(rev(as.integer(intToBits(num))), collapse = ""))
  )
}

for (anumber in c(0, 5, 50, 9000)) {
         cat(dec2bin(anumber),"\n")
}

```

'''output'''

```txt

0
101
110010
10001100101000

```



## Racket


```racket

#lang racket
;; Option 1: binary formatter
(for ([i 16]) (printf "~b\n" i))
;; Option 2: explicit conversion
(for ([i 16]) (displayln (number->string i 2)))

```


## RapidQ


```vb

'Convert Integer to binary string
Print "bin 5 = ", bin$(5)
Print "bin 50 = ",bin$(50)
Print "bin 9000 = ",bin$(9000)
sleep 10

```


## Red


```Red
Red []

foreach number [5 50 9000] [
  ;; any returns first not false value, used to cut leading zeroes
  binstr: form any [find enbase/base to-binary number 2 "1" "0"]
  print reduce [ pad/left number 5 binstr ]
]

```

'''output'''

```txt
    5 101
   50 110010
 9000 10001100101000

```



## Retro


```Retro
9000 50 5  3 [ binary putn cr decimal ] times
```



## REXX

This version handles the special case of zero simply.

### simple version

Note:   some REXX interpreters have a   '''D2B'''   [Decimal to Binary]   BIF ('''b'''uilt-'''i'''n '''f'''unction).

Programming note:   this REXX version depends on   '''numeric digits'''   being large enough to handle leading zeroes in this manner (by adding a zero (to the binary version) to force superfluous leading zero suppression).

```REXX
/*REXX program to  convert  several  decimal numbers  to  binary  (or base 2).          */
                            numeric digits 1000  /*ensure we can handle larger numbers. */
@.=;             @.1=    0
                 @.2=    5
                 @.3=   50
                 @.4= 9000

  do j=1  while  @.j\==''                        /*compute until a  NULL value is found.*/
  y=x2b( d2x(@.j) )     + 0                      /*force removal of extra leading zeroes*/
  say right(@.j,20) 'decimal, and in binary:' y  /*display the number to the terminal.  */
  end   /*j*/                                    /*stick a fork in it,  we're all done. */
```

{{out|output}}

```txt

                   0 decimal, and in binary: 0
                   5 decimal, and in binary: 101
                  50 decimal, and in binary: 110010
                9000 decimal, and in binary: 10001100101000

```



### elegant version

This version handles the case of zero as a special case more elegantly.

The following versions depend on the setting of   '''numeric digits'''   such that the number in decimal can be expressed as a whole number.

```REXX
/*REXX program to  convert  several  decimal numbers  to  binary  (or base 2).          */
@.=;             @.1=    0
                 @.2=    5
                 @.3=   50
                 @.4= 9000

  do j=1  while  @.j\==''                        /*compute until a  NULL value is found.*/
  y=strip( x2b( d2x( @.j )), 'L', 0)             /*force removal of  all leading zeroes.*/
  if y==''  then y=0                             /*handle the special case of  0 (zero).*/
  say right(@.j,20) 'decimal, and in binary:' y  /*display the number to the terminal.  */
  end   /*j*/                                    /*stick a fork in it,  we're all done. */
```

{{out|output|text=  is identical to the 1<sup>st</sup> REXX version.}}




### concise version

This version handles the case of zero a bit more obtusely, but concisely.

```REXX
/*REXX program to  convert  several  decimal numbers  to  binary  (or base 2).          */
@.=;             @.1=    0
                 @.2=    5
                 @.3=   50
                 @.4= 9000

  do j=1  while  @.j\==''                        /*compute until a  NULL value is found.*/
  y=word( strip( x2b( d2x( @.j )), 'L', 0) 0, 1) /*elides all leading 0s, if null, use 0*/
  say right(@.j,20) 'decimal, and in binary:' y  /*display the number to the terminal.  */
  end   /*j*/                                    /*stick a fork in it,  we're all done. */
```

{{out|output|text=  is identical to the 1<sup>st</sup> REXX version.}}




### conforming version

This REXX version conforms to the strict output requirements of this task (just show the binary output without any blanks).

```REXX
/*REXX program to  convert  several  decimal numbers  to  binary  (or base 2).          */
                            numeric digits 200   /*ensure we can handle larger numbers. */
@.=;             @.1=    0
                 @.2=    5
                 @.3=   50
                 @.4= 9000
                 @.5=423785674235000123456789
                 @.6=         1e138              /*one quinquaquadragintillion      ugh.*/

  do j=1  while  @.j\==''                        /*compute until a  NULL value is found.*/
  y=strip( x2b( d2x( @.j )), 'L', 0)             /*force removal of  all leading zeroes.*/
  if y==''  then y=0                             /*handle the special case of  0 (zero).*/
  say  y                                         /*display binary number to the terminal*/
  end   /*j*/                                    /*stick a fork in it,  we're all done. */
```

{{out|output}}

```txt

0
101
110010
10001100101000
1011001101111010111011110101001101111000000000000110001100000100111110100010101
101010111111101001000101110110100000111011011011110111100110100100000100100001111101101110011101000101110110001101101000100100100110000111001010101011110010001111100011110100010101011011111111000110101110111100001011100111110000000010101100110101001010001001001011000000110000010010010100010010000001110100101000011111001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000

```



## Ring


```ring

see "Number to convert : "
give a
n = 0
while pow(2,n+1) < a
      n = n + 1
end

for i = n to 0 step -1
    x = pow(2,i)
    if a >= x see 1 a = a - x
    else see 0 ok
next

```



## Ruby


```ruby
[5,50,9000].each do |n|
  puts "%b" % n
end
```

or

```ruby
for n in [5,50,9000]
  puts n.to_s(2)
end
```

{{out}}

```txt
101
110010
10001100101000
```



## Run BASIC


```runbasic
input "Number to convert:";a
while 2^(n+1) < a
 n = n + 1
wend

for i = n to 0 step -1
  x = 2^i
  if a >= x then
    print 1;
    a = a - x
   else
    print 0;
  end if
next
```

{{out}}

```txt
Number to convert:?9000
10001100101000
```



## Rust


```rust
fn main() {
    for i in 0..8 {
        println!("{:b}", i)
    }
}
```

Outputs:

```txt
0
1
10
11
100
101
110
111
```



=={{header|S-lang}}==
<lang S-lang>define int_to_bin(d)
{
  variable m = 0x40000000, prn = 0, bs = "";
  do {
    if (d & m) {
      bs += "1";
      prn = 1;
    }
    else if (prn)
      bs += "0";
    m = m shr 1;

  } while (m);

  if (bs == "") bs = "0";
  return bs;
}

() = printf("%s\n", int_to_bin(5));
() = printf("%s\n", int_to_bin(50));
() = printf("%s\n", int_to_bin(9000));
```

{{out}}

```txt
101
110010
10001100101000
```



## Scala

Scala has an implicit conversion from <code>Int</code> to <code>RichInt</code> which has a method <code>toBinaryString</code>.

```scala
scala
 (5 toBinaryString)
res0: String = 101

scala> (50 toBinaryString)
res1: String = 110010

scala> (9000 toBinaryString)
res2: String = 10001100101000
```



## Scheme


```scheme
(display (number->string 5 2)) (newline)
(display (number->string 50 2)) (newline)
(display (number->string 9000 2)) (newline)
```



## Seed7

This example uses the [http://seed7.sourceforge.net/libraries/integer.htm#%28in_integer%29radix%28in_integer%29 radix] operator to write a number in binary.


```seed7
$ include "seed7_05.s7i";

const proc: main is func
  local
    var integer: number is 0;
  begin
    for number range 0 to 16 do
      writeln(number radix 2);
    end for;
  end func;
```

{{out}}

```txt

0
1
10
11
100
101
110
111
1000
1001
1010
1011
1100
1101
1110
1111
10000

```



## Sidef


```ruby
[5, 50, 9000].each { |n|
    say n.as_bin;
}
```

{{out}}

```txt
101
110010
10001100101000
```


## Simula


```simula
BEGIN

    PROCEDURE OUTINTBIN(N); INTEGER N;
    BEGIN
        IF N > 1 THEN OUTINTBIN(N//2);
        OUTINT(MOD(N,2),1);
    END OUTINTBIN;

    INTEGER SAMPLE;
    FOR SAMPLE := 5, 50, 9000 DO BEGIN
        OUTINTBIN(SAMPLE);
        OUTIMAGE;
    END;

END
```

{{out}}

```txt

101
110010
10001100101000

```



## SequenceL


```sequencel
main := toBinaryString([5, 50, 9000]);

toBinaryString(number(0)) :=
    let
        val := "1" when number mod 2 = 1 else "0";
    in
        toBinaryString(floor(number/2)) ++ val when floor(number/2) > 0
    else
        val;
```


{{out}}

```txt

["101","110010","10001100101000"]

```




## SkookumScript



```javascript
println(5.binary)
println(50.binary)
println(9000.binary)
```

Or looping over a list of numbers:

```javascript
{5 50 9000}.do[println(item.binary)]
```

{{out}}

```txt
101
110010
10001100101000
```



## Smalltalk


```smalltalk
5 printOn: Stdout radix:2
50 printOn: Stdout radix:2
9000 printOn: Stdout radix:2
```

or:

```smalltalk
#(5 50 9000) do:[:each | each printOn: Stdout radix:2. Stdout cr]
```



## SNUSP


```SNUSP

         /recurse\
$,binary!\@\>?!\@/<@\.#
           !   \=/  \=itoa=@@@+@+++++#
           /<+>- \    div2
           \?!#-?/+#  mod2

```



## Standard ML


```sml
print (Int.fmt StringCvt.BIN 5 ^ "\n");
print (Int.fmt StringCvt.BIN 50 ^ "\n");
print (Int.fmt StringCvt.BIN 9000 ^ "\n");
```


## Swift


```Swift
for num in [5, 50, 9000] {
    println(String(num, radix: 2))
}
```

{{out}}

```txt

101
110010
10001100101000
```



## Tcl


```tcl
proc num2bin num {
    # Convert to _fixed width_ big-endian 32-bit binary
    binary scan [binary format "I" $num] "B*" binval
    # Strip useless leading zeros by reinterpreting as a big decimal integer
    scan $binval "%lld"
}
```

Demonstrating:

```tcl
for {set x 0} {$x < 16} {incr x} {
    puts [num2bin $x]
}
puts "--------------"
puts [num2bin 5]
puts [num2bin 50]
puts [num2bin 9000]
```

{{out}}

```txt

0
1
10
11
100
101
110
111
1000
1001
1010
1011
1100
1101
1110
1111
--------------
101
110010
10001100101000

```


=={{header|TI-83 BASIC}}==
Using Standard TI-83 BASIC

```ti83b
PROGRAM:BINARY
:Disp "NUMBER TO"
:Disp "CONVERT:"
:Input A
:0→N
:0→B
:While 2^(N+1)≤A
:N+1→N
:End
:While N≥0
:iPart(A/2^N)→C
:10^(N)*C+B→B
:If C=1
:Then
:A-2^N→A
:End
:N-1→N
:End
:Disp B
```

Alternate using a string to display larger numbers.

```ti83b
PROGRAM:BINARY
:Input X
:" "→Str1
:Repeat X=0
   :X/2→X
   :sub("01",2fPart(X)+1,1)+Str1→Str1
   :iPart(X)→X
:End
:Str1
```

Using the baseInput() "real(25," function from [http://www.detachedsolutions.com/omnicalc/ Omnicalc]

```ti83b
PROGRAM:BINARY
:Disp "NUMBER TO"
:Disp "CONVERT"
:Input "Str1"
:Disp real(25,Str1,10,2)
```


More compact version:

```ti83b
:Input "DEC: ",D
:" →Str1
:If not(D:"0→Str1
:While D>0
:If not(fPart(D/2:Then
:"0"+Str1→Str1
:Else
:"1"+Str1→Str1
:End
:iPart(D/2→D
:End
:Disp Str1

```



## uBasic/4tH

This will convert any decimal number to any base between 2 and 16.
<lang>Do
  Input "Enter base (1<X<17): "; b
  While (b < 2) + (b > 16)
Loop

Input "Enter number: "; n
s = (n < 0)                            ' save the sign
n = Abs(n)                             ' make number unsigned

For x = 0 Step 1 Until n = 0           ' calculate all the digits
    @(x) = n % b
    n = n / b
Next x

If s Then Print "-";                   ' reapply the sign

For y = x - 1 To 0 Step -1             ' print all the digits
    If @(y) > 9 Then                   ' take care of hexadecimal digits
       Gosub @(y) * 10
    Else
       Print @(y);                     ' print "decimal" digits
    Endif
Next y

Print                                  ' finish the string
End

100 Print "A"; : Return                ' print hexadecimal digit
110 Print "B"; : Return
120 Print "C"; : Return
130 Print "D"; : Return
140 Print "E"; : Return
150 Print "F"; : Return
```

{{out}}

```txt
Enter base (1<X<17): 2
Enter number: 9000
10001100101000

0 OK, 0:775
```


## UNIX Shell


```sh
# Define a function to output binary digits
tobinary() {
  # We use the bench calculator for our conversion
  echo "obase=2;$1"|bc
}

# Call the function with each of our values
tobinary 5
tobinary 50
```



## VBA

'''2 ways :'''

1- Function DecToBin(ByVal Number As Long) As String

'''Arguments :'''

[Required] Number (Long) : ''should be a positive number''

2- Function DecToBin2(ByVal Number As Long, Optional Places As Long) As String

'''Arguments :'''

[Required] Number (Long) : ''should be >= -512 And <= 511''

[Optional] Places (Long) : ''the number of characters to use.''

Note : If places is omitted, DecToBin2 uses the minimum number of characters necessary.
Places is useful for padding the return value with leading 0s (zeros).


```vb

Option Explicit

Sub Main_Dec2bin()
Dim Nb As Long
Nb = 5
    Debug.Print "The decimal value " & Nb & " should produce an output of : " & DecToBin(Nb)
    Debug.Print "The decimal value " & Nb & " should produce an output of : " & DecToBin2(Nb)
Nb = 50
    Debug.Print "The decimal value " & Nb & " should produce an output of : " & DecToBin(Nb)
    Debug.Print "The decimal value " & Nb & " should produce an output of : " & DecToBin2(Nb)
Nb = 9000
    Debug.Print "The decimal value " & Nb & " should produce an output of : " & DecToBin(Nb)
    Debug.Print "The decimal value " & Nb & " should produce an output of : " & DecToBin2(Nb)
End Sub

Function DecToBin(ByVal Number As Long) As String
Dim strTemp As String

    Do While Number > 1
        strTemp = Number - 2 * (Number \ 2) & strTemp
        Number = Number \ 2
    Loop
    DecToBin = Number & strTemp
End Function

Function DecToBin2(ByVal Number As Long, Optional Places As Long) As String
    If Number > 511 Then
        DecToBin2 = "Error : Number is too large ! (Number must be < 511)"
    ElseIf Number < -512 Then
        DecToBin2 = "Error : Number is too small ! (Number must be > -512)"
    Else
        If Places = 0 Then
            DecToBin2 = WorksheetFunction.Dec2Bin(Number)
        Else
            DecToBin2 = WorksheetFunction.Dec2Bin(Number, Places)
        End If
    End If
End Function

```

{{out}}

```txt
The decimal value 5 should produce an output of : 101
The decimal value 5 should produce an output of : 101
The decimal value 50 should produce an output of : 110010
The decimal value 50 should produce an output of : 110010
The decimal value 9000 should produce an output of : 10001100101000
The decimal value 9000 should produce an output of : Error : Number is too large ! (Number must be < 511)
```




## Vedit macro language

This implementation reads the numeric values from user input and writes the converted binary values in the edit buffer.

```vedit
repeat (ALL) {
    #10 = Get_Num("Give a numeric value, -1 to end: ", STATLINE)
    if (#10 < 0) { break }
    Call("BINARY")
    Update()
}
return

:BINARY:
do {
    Num_Ins(#10 & 1, LEFT+NOCR)
    #10 = #10 >> 1
    Char(-1)
} while (#10 > 0)
EOL
Ins_Newline
Return
```

Example output when values 0, 1, 5, 50 and 9000 were entered:

```txt

0
1
101
110010
10001100101000

```



## Vim Script


```vim
function Num2Bin(n)
    let n = a:n
    let s = ""
    if n == 0
        let s = "0"
    else
        while n
            if n % 2 == 0
                let s = "0" . s
            else
                let s = "1" . s
            endif
            let n = n / 2
        endwhile
    endif
    return s
endfunction

echo Num2Bin(5)
echo Num2Bin(50)
echo Num2Bin(9000)
```


{{Out}}

```txt
101
110010
10001100101000
```



## Visual Basic

{{works with|Visual Basic|VB6 Standard}}

```vb

Public Function Bin(ByVal l As Long) As String
Dim i As Long
  If l Then
    If l And &H80000000 Then 'negative number
      Bin = "1" & String$(31, "0")
      l = l And (Not &H80000000)

      For i = 0 To 30
      If l And (2& ^ i) Then
        Mid$(Bin, Len(Bin) - i) = "1"
      End If
      Next i

    Else                     'positive number
      Do While l
      If l Mod 2 Then
        Bin = "1" & Bin
      Else
        Bin = "0" & Bin
      End If
      l = l \ 2
      Loop
    End If
  Else
    Bin = "0"                'zero
  End If
End Function

'testing:
Public Sub Main()
  Debug.Print Bin(5)
  Debug.Print Bin(50)
  Debug.Print Bin(9000)
End Sub

```

{{out}}

```txt
101
110010
10001100101000
```



## Visual Basic .NET


```vbnet
Module Program
    Sub Main
        For Each number In {5, 50, 9000}
            Console.WriteLine(Convert.ToString(number, 2))
        Next
    End Sub
End Module
```

{{out}}

```txt
101
110010
10001100101000
```



## Visual FoxPro


```vfp

*!* Binary Digits
CLEAR
k = CAST(5 As I)
? NToBin(k)
k = CAST(50 As I)
? NToBin(k)
k = CAST(9000 As I)
? NToBin(k)

FUNCTION NTOBin(n As Integer) As String
LOCAL i As Integer, b As String, v As Integer
b = ""
v = HiBit(n)
FOR i = 0 TO v
    b = IIF(BITTEST(n, i), "1", "0") + b
ENDFOR
RETURN b
ENDFUNC

FUNCTION HiBit(n As Double) As Integer
*!* Find the highest power of 2 in n
LOCAL v As Double
v = LOG(n)/LOG(2)
RETURN FLOOR(v)
ENDFUNC

```

{{out}}

```txt

101
110010
10001100101000

```



## Whitespace


This program prints binary numbers until the internal representation of the current integer overflows to -1; it will never do so on some interpreters. It is almost an exact duplicate of [[Count in octal#Whitespace]].


```Whitespace








































```


It was generated from the following pseudo-Assembly.


```asm
push 0
; Increment indefinitely.
0:
    push -1 ; Sentinel value so the printer knows when to stop.
    copy 1
    call 1
    push 10
    ochr
    push 1
    add
    jump 0
; Get the binary digits on the stack in reverse order.
1:
    dup
    push 2
    mod
    swap
    push 2
    div
    push 0
    copy 1
    sub
    jn 1
    pop
; Print them.
2:
    dup
    jn 3 ; Stop at the sentinel.
    onum
    jump 2
3:
    pop
    ret
```



## Wortel

Using JavaScripts buildin toString method on the Number object, the following function takes a number and returns a string with the binary representation:

```wortel
\.toString 2
; the following function also casts the string to a number
^(@+ \.toString 2)
```

To output to the console:

```wortel
@each ^(console.log \.toString 2) [5 50 900]
```

Outputs:
```txt

101
110010
1110000100
```



## X86 Assembly

Translation of XPL0. Assemble with tasm, tlink /t

```asm
        .model  tiny
        .code
        .486
        org     100h
start:  mov     ax, 5
        call    binout
        call    crlf
        mov     ax, 50
        call    binout
        call    crlf
        mov     ax, 9000
        call    binout

crlf:   mov     al, 0Dh         ;new line
        int     29h
        mov     al, 0Ah
        int     29h
        ret

binout: push    ax
        shr     ax, 1
        je      bo10
         call   binout
bo10:   pop     ax
        and     al, 01h
        or      al, '0'
        int     29h             ;display character
        ret
        end     start
```


{{out}}

```txt

101
110010
10001100101000

```



## XPL0


```XPL0
include c:\cxpl\codes;          \intrinsic code declarations

proc BinOut(N);                 \Output N in binary
int N;
int R;
[R:= N&1;
N:= N>>1;
if N then BinOut(N);
ChOut(0, R+^0);
];

int I;
[I:= 0;
repeat  BinOut(I); CrLf(0);
        I:= I+1;
until   KeyHit or I=0;
]
```


{{out}}

```txt

0
1
10
11
100
101
110
111
1000
...
100000010011110
100000010011111
100000010100000
100000010100001

```



## zkl


```zkl
(9000).toString(2)
```


```zkl
T(5,50,9000).apply("toString",2) //--> L("101","110010","10001100101000")
```


```zkl
"%.2B".fmt(9000)
```



## ZX Spectrum Basic


```zxbasic
10 LET n=5: GO SUB 1000: PRINT s$
20 LET n=50: GO SUB 1000: PRINT s$
30 LET n=9000: GO SUB 1000: PRINT s$
999 STOP
1000 REM convert to binary
1010 LET t=n: REM temporary variable
1020 LET s$="": REM this will contain our binary digits
1030 LET sf=0: REM output has not started yet
1040 FOR l=126 TO 0 STEP -1
1050 LET d$="0": REM assume next digit is zero
1060 IF t>=(2^l) THEN LET d$="1": LET t=t-(2^l): LET sf=1
1070 IF (sf <> 0) THEN LET s$=s$+d$
1080 NEXT l
1090 RETURN
```

