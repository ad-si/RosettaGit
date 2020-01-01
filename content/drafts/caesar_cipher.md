+++
title = "Caesar cipher"
description = ""
date = 2019-08-14T22:16:37Z
aliases = []
[extra]
id = 9913
[taxonomies]
categories = []
tags = []
+++

{{task|Encryption}}
[[Category:String manipulation]]


;Task:
Implement a [[wp:Caesar cipher|Caesar cipher]], both encoding and decoding.

The key is an integer from 1 to 25.

This cipher rotates (either towards left or right) the letters of the alphabet (A to Z).

The encoding replaces each letter with the 1st to 25th next letter in the alphabet (wrapping Z to A).

So key 2 encrypts "HI" to "JK", but key 20 encrypts "HI" to "BC".

This simple "mono-alphabetic substitution cipher" provides almost no security, because an attacker who has the encoded message can either use frequency analysis to guess the key, or just try all 25 keys.

Caesar cipher is identical to [[Vigenère cipher]] with a key of length 1.

Also, [[Rot-13]] is identical to Caesar cipher with key 13.


;Related tasks:
* [[Rot-13]]
* [[Substitution Cipher]]
* [[Vigenère Cipher/Cryptanalysis]]





## 11l


```11l
F caesar(string, =key, decode = 0B)
   I decode
      key = 26 - key

   V r = ‘ ’ * string.len
   L(c) string
      r[L.index] = S c
                      ‘a’..‘z’
                         Char(code' (c.code - ‘a’.code + key) % 26 + ‘a’.code)
                      ‘A’..‘Z’
                         Char(code' (c.code - ‘A’.code + key) % 26 + ‘A’.code)
                      E
                         c
   R r

V msg = ‘The quick brown fox jumped over the lazy dogs’
print(msg)
V enc = caesar(msg, 11)
print(enc)
print(caesar(enc, 11, decode' 1B))
```

{{out}}

```txt

The quick brown fox jumped over the lazy dogs
Esp bftnv mczhy qzi ufxapo zgpc esp wlkj ozrd
The quick brown fox jumped over the lazy dogs

```



## 360 Assembly

A good example of the use of TR instruction to translate a character.

```360asm
*        Caesar cypher             04/01/2019
CAESARO  PROLOG
         XPRNT  PHRASE,L'PHRASE    print phrase
         LH     R3,OFFSET          offset
         BAL    R14,CYPHER         call cypher
         LNR    R3,R3              -offset
         BAL    R14,CYPHER         call cypher
         EPILOG
CYPHER   LA     R4,REF(R3)         @ref+offset
         MVC    A,0(R4)            for A to I
         MVC    J,9(R4)            for J to R
         MVC    S,18(R4)           for S to Z
         TR     PHRASE,TABLE       translate
         XPRNT  PHRASE,L'PHRASE    print phrase
         BR     R14                return
OFFSET   DC     H'22'              between -25 and +25
PHRASE   DC     CL37'THE FIVE BOXING WIZARDS JUMP QUICKLY'
         DC     CL26'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
REF      DC     CL26'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
         DC     CL26'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
TABLE    DC     CL256' '           translate table for TR
         ORG    TABLE+C'A'
A        DS     CL9
         ORG    TABLE+C'J'
J        DS     CL9
         ORG    TABLE+C'S'
S        DS     CL8
         ORG
         YREGS
         END    CAESARO
```

{{out}}

```txt

THE FIVE BOXING WIZARDS JUMP QUICKLY
PDA BERA XKTEJC SEVWNZO FQIL MQEYGHU
THE FIVE BOXING WIZARDS JUMP QUICKLY

```




## 8th


```forth
\ Ensure the output char is in the correct range:
: modulate \ char base -- char
  tuck n:- 26 n:+ 26 n:mod n:+ ;

\ Symmetric Caesar cipher.  Input is text and number of characters to advance
\ (or retreat, if negative).  That value should be in the range 1..26
: caesar \ intext key -- outext
  >r
  (
    \ Ignore anything below '.' as punctuation:
    dup '. n:> if
      \ Do the conversion
      dup r@ n:+ swap
      \ Wrap appropriately
      'A 'Z between if 'A else 'a then modulate
    then
  ) s:map rdrop ;

"The five boxing wizards jump quickly!"
dup . cr
1  caesar dup . cr
-1 caesar . cr
bye
```

{{out}}

```txt

The five boxing wizards jump quickly!
Uif gjwf cpyjoh xjabset kvnq rvjdlmz!
The five boxing wizards jump quickly!

```



## Ada


```Ada
with Ada.Text_IO;

procedure Caesar is

   type M26 is mod 26;

   function To_M26(C: Character; Offset: Character) return M26 is
   begin
      return M26(Character'Pos(C)-Character'Pos(Offset));
   end To_M26;

   function To_Character(Value:   in  M26; Offset: Character)
                        return Character is
   begin
      return Character'Val(Integer(Value)+Character'Pos(Offset));
   end To_Character;

   function Encrypt (Plain: String; Key: M26) return String is
      Ciph: String(Plain'Range);

   begin
      for I in Plain'Range loop
         case Plain(I) is
            when 'A' .. 'Z' =>
               Ciph(I) := To_Character(To_M26(Plain(I), 'A')+Key, 'A');
            when 'a' .. 'z' =>
               Ciph(I) := To_Character(To_M26(Plain(I), 'a')+Key, 'a');
            when others =>
               Ciph(I) := Plain(I);
         end case;
      end loop;
      return Ciph;
   end Encrypt;

   Text:  String := Ada.Text_IO.Get_Line;
   Key: M26 := 3; -- Default key from "Commentarii de Bello Gallico"

begin -- Caesar main program

   Ada.Text_IO.Put_Line("Plaintext ------------>" & Text);
   Text := Encrypt(Text, Key);
   Ada.Text_IO.Put_Line("Ciphertext ----------->" & Text);
   Ada.Text_IO.Put_Line("Decrypted Ciphertext ->" & Encrypt(Text, -Key));

end Caesar;
```

{{out}}

```txt
> ./caesar
The five boxing wizards jump quickly
Plaintext ------------>The five boxing wizards jump quickly
Ciphertext ----------->Wkh ilyh eralqj zlcdugv mxps txlfnob
Decrypted Ciphertext ->The five boxing wizards jump quickly
```



## ALGOL 68

{{trans|Ada|Note: This specimen retains the original [[#Ada|Ada]] coding style.}}
{{works with|ALGOL 68|Revision 1 - no extensions to language used.}}
{{works with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-1.18.0/algol68g-1.18.0-9h.tiny.el5.centos.fc11.i386.rpm/download 1.18.0-9h.tiny].}}
{{wont work with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8-8d] - due to extensive use of '''format'''[ted] ''transput''.}}

```algol68
#!/usr/local/bin/a68g --script #

program caesar: BEGIN

   MODE MODXXVI = SHORT SHORT INT; # MOD26 #

   PROC to m26 = (CHAR c, offset)MODXXVI:
   BEGIN
      ABS c - ABS offset
   END #to m26#;

   PROC to char = (MODXXVI value, CHAR offset)CHAR:
   BEGIN
      REPR ( ABS offset + value MOD 26 )
   END #to char#;

   PROC encrypt = (STRING plain, MODXXVI key)STRING:
   BEGIN
      [UPB plain]CHAR ciph;
      FOR i TO UPB plain DO
         CHAR c = plain[i];
         ciph[i]:=
           IF "A" <= c AND c <= "Z" THEN
               to char(to m26(c, "A")+key, "A")
           ELIF "a" <= c AND c <= "z" THEN
               to char(to m26(c, "a")+key, "a")
           ELSE
               c
           FI
      OD;
      ciph
   END #encrypt#;

# caesar main program #
   STRING text := "The five boxing wizards jump quickly" # OR read string #;
   MODXXVI key := 3; # Default key from "Bello Gallico" #

   printf(($gl$, "Plaintext ------------>" + text));
   text := encrypt(text, key);
   printf(($gl$, "Ciphertext ----------->" + text));
   printf(($gl$, "Decrypted Ciphertext ->" + encrypt(text, -key)))

END #caesar#
```

{{out}}

```txt

Plaintext ------------>The five boxing wizards jump quickly
Ciphertext ----------->Wkh ilyh eralqj zlcdugv mxps txlfnob
Decrypted Ciphertext ->The five boxing wizards jump quickly

```



## APL


```apl

      ∇CAESAR[⎕]∇
    ∇
[0]   A←K CAESAR V
[1]   A←'AaBbCcDdEeFfGgHhIiJjKkLlMmNnOoPpQqRrSsTtUuVvWwXxYyZz'
[2]   ((,V∊A)/,V)←A[⎕IO+52|(2×K)+((A⍳,V)-⎕IO)~52]
[3]   A←V
    ∇

```

{{out}}

```txt

      6 CAESAR ⎕←TEXT_MATRIX
This method uses selective specification
(similar to pattern matching in Haskell)
to modify only alphabetic characters.

Znoy skznuj ayky ykrkizobk yvkioloigzout
(yosorgx zu vgzzkxt sgzinotm ot Ngyqkrr)
zu sujole utre grvnghkzoi ingxgizkxy.


      ¯1 CAESAR ⎕←ENCRYPTED
Ofhbujwf lfzt (efdszqujoh) xpslt: IBM 9000
Negative keys (decrypting) works: HAL 9000

      ¯1.5 CAESAR ⎕←CAPSLOCK
fUN FACT: FRACTIONAL KEYS SWITCH FROM upper case TO LOWER CASE.
Esl dyar: dpyargmlyj icwq qugraf dpmk TOODQ BZRD rm jmucp ayqc.

```



## Applesoft BASIC


```ApplesoftBasic
100 INPUT ""; T$

110 LET K% = RND(1) * 25 + 1
120 PRINT "ENCODED WITH ";
130 GOSUB 200ENCODED

140 LET K% = 26 - K%
150 PRINT "DECODED WITH ";
160 GOSUB 200DECODED

170 END

REM ENCODED/DECODED
200 PRINT "CAESAR " K%;
210 LET K$(1) = " (ROT-13)"
220 PRINT K$(K% = 13)
230 GOSUB 300CAESAR
240 PRINT T$
250 RETURN

REM CAESAR T$ K%
300 FOR I = 1 TO LEN(T$)
310    LET C$ = MID$(T$, I, 1)
320    GOSUB 400ENCODE
330    LET L = I - 1
340    LET T$(0) = MID$(T$, 1, L)
350    LET L = I + 1
360    LET T$ = C$ + MID$(T$, L)
370    LET T$ = T$(0) + T$
380 NEXT I
390 RETURN

REM ENCODE C$ K%
400 LET C = ASC(C$)
410 LET L = (C > 95) * 32
420 LET C = C - L
430 IF C < 65 THEN RETURN
440 IF C > 90 THEN RETURN
450 LET C = C + K%
460 IF C > 90 THEN C = C - 26
470 LET C$ = CHR$(C + L)
480 RETURN
```

{{out}}

```txt
]RUN
The quick brown fox jumps over the lazy dog.
ENCODED WITH CAESAR 12
Ftq cguow ndaiz raj vgybe ahqd ftq xmlk pas.
DECODED WITH CAESAR 14
The quick brown fox jumps over the lazy dog.

]?RND(-6):RUN
4.48226274E-08
PACK MY BOX WITH FIVE DOZEN LIQUOR JUGS
ENCODED WITH CAESAR 13 (ROT-13)
CNPX ZL OBK JVGU SVIR QBMRA YVDHBE WHTF
DECODED WITH CAESAR 13 (ROT-13)
PACK MY BOX WITH FIVE DOZEN LIQUOR JUGS

]K%=1:GOTO120
ENCODED WITH CAESAR 1
QBDL NZ CPY XJUI GJWF EPAFO MJRVPS KVHT
DECODED WITH CAESAR 25
PACK MY BOX WITH FIVE DOZEN LIQUOR JUGS
```



## Arc


```Arc

(= rot (fn (L N)
  (if
    (and (<= 65 L) (>= 90 L))
      (do
      (= L (- L 65))
      (= L (mod (+ N L) 26))
      (= L (+ L 65)))
    (and (<= 97 L) (>= 122 L))
      (do
      (= L (- L 97))
      (= L (mod (+ N L) 26))
      (= L (+ L 97))))
  L))

(= caesar (fn (text (o shift))
  (unless shift (= shift 13))
  (= output (map [int _] (coerce text 'cons)))
  (= output (map [rot _ shift] output))
  (string output)
  ))

```


{{Out}}

```arc

(caesar "The quick brown fox jumps over the lazy dog.")
"Gur dhvpx oebja sbk whzcf bire gur ynml qbt."

```


## ARM Assembly

{{works with|as|Raspberry Pi}}

```ARM Assembly

/* ARM assembly Raspberry PI  */
/*  program caresarcode.s   */

/* Constantes    */
.equ STDOUT, 1                           @ Linux output console
.equ EXIT,   1                           @ Linux syscall
.equ WRITE,  4                           @ Linux syscall

.equ STRINGSIZE,          500

/* Initialized data */
.data
szMessString:            .asciz "String :\n"
szMessEncrip:            .asciz "\nEncrypted :\n"
szMessDecrip:            .asciz "\nDecrypted :\n"
szString1:               .asciz "Why study medicine because there is internet ?"

szCarriageReturn:       .asciz "\n"

/* UnInitialized data */
.bss
szString2:                .skip  STRINGSIZE
szString3:                .skip  STRINGSIZE
/*  code section */
.text
.global main
main:

    ldr r0,iAdrszMessString                     @ display message
    bl affichageMess
    ldr r0,iAdrszString1                        @ display string
    bl affichageMess
    ldr r0,iAdrszString1
    ldr r1,iAdrszString2
    mov r2,#20                                  @ key
    bl encrypt
    ldr r0,iAdrszMessEncrip
    bl affichageMess
    ldr r0,iAdrszString2                        @ display string
    bl affichageMess
    ldr r0,iAdrszString2
    ldr r1,iAdrszString3
    mov r2,#20                                  @ key
    bl decrypt
    ldr r0,iAdrszMessDecrip
    bl affichageMess
    ldr r0,iAdrszString3                        @ display string
    bl affichageMess
    ldr r0,iAdrszCarriageReturn
    bl affichageMess
100:                                            @ standard end of the program
    mov r0, #0                                  @ return code
    mov r7, #EXIT                               @ request to exit program
    svc 0                                       @ perform system call
iAdrszMessString:         .int szMessString
iAdrszMessDecrip:         .int szMessDecrip
iAdrszMessEncrip:         .int szMessEncrip
iAdrszString1:            .int szString1
iAdrszString2:            .int szString2
iAdrszString3:            .int szString2
iAdrszCarriageReturn:     .int szCarriageReturn
/******************************************************************/
/*     encrypt strings                         */
/******************************************************************/
/* r0 contains the address of the string1 */
/* r1 contains the address of the encrypted string */
/* r2 contains the key (1-25)                     */
encrypt:
    push {r3,r4,lr}           @ save  registers
    mov r3,#0                 @ counter byte string 1
1:
    ldrb r4,[r0,r3]           @ load byte string 1
    cmp r4,#0                 @ zero final ?
    streqb r4,[r1,r3]
    moveq r0,r3
    beq 100f
    cmp r4,#65                @ < A ?
    strltb r4,[r1,r3]
    addlt r3,#1
    blt 1b
    cmp r4,#90                @ > Z
    bgt 2f
    add r4,r2                 @ add key
    cmp r4,#90                @ > Z
    subgt r4,#26
    strb r4,[r1,r3]
    add r3,#1
    b 1b
2:
    cmp r4,#97                @ < a ?
    strltb r4,[r1,r3]
    addlt r3,#1
    blt 1b
    cmp r4,#122               @> z
    strgtb r4,[r1,r3]
    addgt r3,#1
    bgt 1b
    add r4,r2
    cmp r4,#122
    subgt r4,#26
    strb r4,[r1,r3]
    add r3,#1
    b 1b

100:
    pop {r3,r4,lr}            @ restaur registers
    bx lr                     @ return
/******************************************************************/
/*     decrypt strings                                           */
/******************************************************************/
/* r0 contains the address of the encrypted string1 */
/* r1 contains the address of the decrypted string */
/* r2 contains the key (1-25)                     */
decrypt:
    push {r3,r4,lr}          @ save  registers
    mov r3,#0                @ counter byte string 1
1:
    ldrb r4,[r0,r3]          @ load byte string 1
    cmp r4,#0                @ zero final ?
    streqb r4,[r1,r3]
    moveq r0,r3
    beq 100f
    cmp r4,#65               @ < A ?
    strltb r4,[r1,r3]
    addlt r3,#1
    blt 1b
    cmp r4,#90               @ > Z
    bgt 2f
    sub r4,r2                @ substract key
    cmp r4,#65               @ < A
    addlt r4,#26
    strb r4,[r1,r3]
    add r3,#1
    b 1b
2:
    cmp r4,#97               @ < a ?
    strltb r4,[r1,r3]
    addlt r3,#1
    blt 1b
    cmp r4,#122              @ > z
    strgtb r4,[r1,r3]
    addgt r3,#1
    bgt 1b
    sub r4,r2                @ substract key
    cmp r4,#97               @ < a
    addlt r4,#26
    strb r4,[r1,r3]
    add r3,#1
    b 1b

100:
    pop {r3,r4,lr}           @ restaur registers
    bx lr                    @ return
/******************************************************************/
/*     display text with size calculation                         */
/******************************************************************/
/* r0 contains the address of the message */
affichageMess:
    push {r0,r1,r2,r7,lr}                       @ save  registers
    mov r2,#0                                   @ counter length */
1:                                              @ loop length calculation
    ldrb r1,[r0,r2]                             @ read octet start position + index
    cmp r1,#0                                   @ if 0 its over
    addne r2,r2,#1                              @ else add 1 in the length
    bne 1b                                      @ and loop
                                                @ so here r2 contains the length of the message
    mov r1,r0                                   @ address message in r1
    mov r0,#STDOUT                              @ code to write to the standard output Linux
    mov r7, #WRITE                              @ code call system "write"
    svc #0                                      @ call system
    pop {r0,r1,r2,r7,lr}                        @ restaur registers
    bx lr                                       @ return


```


## Astro


```python
fun caesar(s, k, decode: false):
    if decode:
        k = 26 - k
    result = ''
    for i in s.uppercase() where 65 <= ord(i) <= 90:
        result.push! char(ord(i) - 65 + k) mod 26 + 65
    return result

let message = "The quick brown fox jumped over the lazy dogs"
let encrypted = caesar(msg, 11)
let decrypted = caesar(enc, 11, decode: true)

print(message, encrypted, decrypted, sep: '\n')
```



## AutoHotkey

This ungodly solution is an attempt at code-golf. It requires input to be all-caps alphabetic, only works on AutoHotkey_L Unicode, and might not run on x64

```AutoHotkey
n=2
s=HI
t:=&s
While *t
o.=Chr(Mod(*t-65+n,26)+65),t+=2
MsgBox % o
```

This next one is much more sane and handles input very well, including case.

```AutoHotkey
Caesar(string, n){
	Loop Parse, string
	{
		If (Asc(A_LoopField) >= Asc("A") and Asc(A_LoopField) <= Asc("Z"))
			out .= Chr(Mod(Asc(A_LoopField)-Asc("A")+n,26)+Asc("A"))
		Else If (Asc(A_LoopField) >= Asc("a") and Asc(A_LoopField) <= Asc("z"))
			out .= Chr(Mod(Asc(A_LoopField)-Asc("a")+n,26)+Asc("a"))
		Else out .= A_LoopField
	}
	return out
}

MsgBox % Caesar("h i", 2) "`n" Caesar("Hi", 20)
```

{{out}}
```txt
j k
Bc
```



## AutoIt


The Ceasar Funktion can enrcypt and decrypt, standart is Encryption, to Decrypt set third parameter to False

```autoit

$Caesar = Caesar("Hi", 2, True)
MsgBox(0, "Caesar", $Caesar)
Func Caesar($String, $int, $encrypt = True)
	If Not IsNumber($int) Or Not StringIsDigit($int) Then Return SetError(1, 0, 0)
	If $int < 1 Or $int > 25 Then Return SetError(2, 0, 0)
	Local $sLetters, $x
	$String = StringUpper($String)
	$split = StringSplit($String, "")
	For $i = 1 To $split[0]
		If Asc($split[$i]) - 64 > 26 Or Asc($split[$i]) - 64 < 1 Then
			$sLetters &= $split[$i]
			ContinueLoop
		EndIf
		If $encrypt = True Then
			$move = Asc($split[$i]) - 64 + $int
		Else
			$move = Asc($split[$i]) - 64 - $int
		EndIf
		If $move > 26 Then
			$move -= 26
		ElseIf $move < 1 Then
			$move += 26
		EndIf
		While $move
			$x = Mod($move, 26)
			If $x = 0 Then $x = 26
			$sLetters &= Chr($x + 64)
			$move = ($move - $x) / 26
		WEnd
	Next
	Return $sLetters
EndFunc   ;==>Caesar

```



## AWK


```awk

#!/usr/bin/awk -f

BEGIN {
    message = "My hovercraft is full of eels."
    key = 1

    cypher = caesarEncode(key, message)
    clear  = caesarDecode(key, cypher)

    print "message: " message
    print " cypher: " cypher
    print "  clear: " clear
    exit
}

function caesarEncode(key, message) {
    return caesarXlat(key, message, "encode")
}

function caesarDecode(key, message) {
    return caesarXlat(key, message, "decode")
}

function caesarXlat(key, message, dir,    plain, cypher, i, num, s) {
    plain = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    cypher = substr(plain, key+1) substr(plain, 1, key)

    if (toupper(substr(dir, 1, 1)) == "D") {
        s = plain
        plain = cypher
        cypher = s
    }

    s = ""
    message = toupper(message)
    for (i = 1; i <= length(message); i++) {
        num = index(plain, substr(message, i, 1))
        if (num) s = s substr(cypher, num, 1)
        else s = s substr(message, i, 1)
    }
    return s
}

```


{{out}}

```txt

message: My hovercraft is full of eels.
 cypher: NZ IPWFSDSBGU JT GVMM PG FFMT.
  clear: MY HOVERCRAFT IS FULL OF EELS.

```



## Babel



```babel
((main
    {"The quick brown fox jumps over the lazy dog.\n"
    dup <<

    17 caesar_enc !
    dup <<

    17 caesar_dec !
    <<})

(caesar_enc
        { 2 take
        { caesar_enc_loop ! }
    nest })

(caesar_enc_loop {
    give
    <- str2ar
        {({ dup is_upper ! }
                { 0x40 -
                -> dup <-
                encrypt !
                0x40 + }
             { dup is_lower ! }
                { 0x60 -
                -> dup <-
                encrypt !
                0x60 + }
            { 1 }
                {fnord})
        cond}
    eachar
    collect !
    ls2lf ar2str})

(collect { -1 take })

(encrypt { + 1 - 26 % 1 + })

(caesar_dec { <- 26 -> - caesar_enc ! })

(is_upper
    { dup
    <- 0x40 cugt ->
    0x5b cult
    cand })

(is_lower
    { dup
    <- 0x60 cugt ->
    0x7b cult
    cand }))
```


{{out}}

```txt
The quick brown fox jumps over the lazy dog.
Kyv hlztb sifne wfo aldgj fmvi kyv crqp ufx.
The quick brown fox jumps over the lazy dog.
```



## BaCon


```qbasic
CONST lc$ = "abcdefghijklmnopqrstuvwxyz"
CONST uc$ = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

CONST txt$ = "The quick brown fox jumps over the lazy dog."

FUNCTION Ceasar$(t$, k)

    lk$ = MID$(lc$ & lc$, k+1, 26)
    uk$ = MID$(uc$ & uc$, k+1, 26)

    RETURN REPLACE$(t$, lc$ & uc$, lk$ & uk$, 2)

END FUNCTION

tokey = RANDOM(25)+1
PRINT "Encrypting text with key: ", tokey

en$ = Ceasar$(txt$, tokey)

PRINT "Encrypted: ", en$
PRINT "Decrypted: ", Ceasar$(en$, 26-tokey)
```

{{out}}

```txt

user@host $ bacon ceasar
Converting 'ceasar.bac'... done, 20 lines were processed in 0.015 seconds.
Compiling 'ceasar.bac'... cc  -c ceasar.bac.c
cc -o ceasar ceasar.bac.o -lbacon -lm
Done, program 'ceasar' ready.
user@host $ ./ceasar
Encrypting text with key: 23
Encrypted: Qeb nrfzh yoltk clu grjmp lsbo qeb ixwv ald.
Decrypted: The quick brown fox jumps over the lazy dog.

```



## BASIC256

<lang>
# Caeser Cipher
# basic256 1.1.4.0

dec$ = ""
type$ = "cleartext "

input "If decrypting enter " + "<d> " + " -- else press enter > ",dec$  # it's a klooj I know...
input "Enter offset > ", iOffset

if dec$ = "d" then
    iOffset = 26 - iOffset
    type$ = "ciphertext "
end if

input "Enter " + type$ + "> ", str$

str$ = upper(str$)          # a bit of a cheat really, however old school encryption is always upper case
len = length(str$)

for i = 1 to len
    iTemp =  asc(mid(str$,i,1))

    if iTemp > 64 AND iTemp < 91 then
        iTemp = ((iTemp - 65) + iOffset) % 26
        print chr(iTemp + 65);
    else
        print chr(iTemp);
    end if

next i

```

{{out}}

```txt

Enter offset > 21
Enter cleartext > My hovercraft is full of eels.
HT CJQZMXMVAO DN APGG JA ZZGN.

Enter offset > 21
Enter ciphertext > HT CJQZMXMVAO DN APGG JA ZZGN.
MY HOVERCRAFT IS FULL OF EELS.

```


## BBC BASIC


```bbcbasic
      plaintext$ = "Pack my box with five dozen liquor jugs"
      PRINT plaintext$

      key% = RND(25)
      cyphertext$ = FNcaesar(plaintext$, key%)
      PRINT cyphertext$

      decyphered$ = FNcaesar(cyphertext$, 26-key%)
      PRINT decyphered$

      END

      DEF FNcaesar(text$, key%)
      LOCAL I%, C%
      FOR I% = 1 TO LEN(text$)
        C% = ASC(MID$(text$,I%))
        IF (C% AND &1F) >= 1 AND (C% AND &1F) <= 26 THEN
          C% = (C% AND &E0) OR (((C% AND &1F) + key% - 1) MOD 26 + 1)
          MID$(text$, I%, 1) = CHR$(C%)
        ENDIF
      NEXT
      = text$

```

{{out}}

```txt
Pack my box with five dozen liquor jugs
Zkmu wi lyh gsdr psfo nyjox vsaeyb teqc
Pack my box with five dozen liquor jugs
```



## Befunge

Almost direct copy of the [[Vigenère cipher#Befunge|Vigenère cipher]], although the code has been reversed and the first line eliminated because of the simpler key initialisation.

The text to encrypt is read from stdin, and the key is the first integer on the stack - 11 (<tt>65+</tt>) in the example below.


```befunge>65+>>>>10p100p1>:v:+
#*,#g1#0-#0:#!<<
"`"::_@#!`\*84:<~<$<^+"A"%*2+9<v"{"\`
**-"A"-::0\`\55*`+#^_\0g+"4"+4^>\`*48
```


{{out}}

```txt
The quick brown fox jumped over the lazy dogs
ESPBFTNVMCZHYQZIUFXAPOZGPCESPWLKJOZRD
```


The decrypter is essentially identical, except for a change of sign on the last line.


```befunge>65+>>>>10p100p1>:v:+
#*,#g1#0-#0:#!<<
"`"::_@#!`\*84:<~<$<^+"A"%*2+9<v"{"\`
**-"A"-::0\`\55*`+#^_\0g-"4"+4^>\`*48
```


{{out}}

```txt
ESPBFTNVMCZHYQZIUFXAPOZGPCESPWLKJOZRD
THEQUICKBROWNFOXJUMPEDOVERTHELAZYDOGS
```


=={{header|Brainfuck}}==

```bf
                                    Author: Ettore Forigo | Hexwell

+                                   start the key input loop
[
                                    memory: | c | 0 | cc | key |
                                              ^

    ,                               take one character of the key

                                    :c : condition for further ifs
                                    != ' ' (subtract 32 (ascii for ' '))
    --------------------------------

                                    (testing for the condition deletes it so duplication is needed)
    [>>+<+<-]                       duplicate
    >                               |
    [<+>-]                          |
    >                               |  :cc : copy of :c

    [                               if :cc
                                    |
        >                           |  :key : already converted digits
                                    |
        [>++++++++++<-]             |  multiply by 10
        >                           |  |
        [<+>-]                      |  |
        <                           |  |
                                    |
        <                           |  :cc
        [-]                         |  clear (making the loop an if)
    ]                               |

    <<                              :c

    [>>+<+<-]                       duplicate
    >                               |
    [<+>-]                          |
    >                               |  :cc

    [                               if :cc
        ----------------            |  subtract 16 (difference between ascii for ' ' (already subtracted before) and ascii for '0'
                                    |    making '0' 0; '1' 1; etc to transform ascii to integer)
                                    |
        [>+<-]                      |  move/add :cc to :key
    ]                               |

    <<                              :c
]

                                    memory: | 0 | 0 | 0 | key |
                                              ^

>>>                                 :key

[<<<+>>>-]                          move key

                                    memory: | key | 0 | 0 | 0 |
                                                            ^
<                                                       ^
+                                   start the word input loop
[
                                    memory: | key | 0 | c | 0 | cc |
                                                        ^

    ,                               take one character of the word

                                    :c : condition for further if
                                    != ' ' (subtract 32 (ascii for ' '))
    --------------------------------

    [>>+<+<-]                       duplicate
    >                               |
    [<+>-]                          |
    >                               |  :cc : copy of :c

    [                               if :cc
                                    |  subtract 65 (difference between ascii for ' ' (already subtracted before) and ascii for 'a'; making a 0; b 1; etc)
        -----------------------------------------------------------------
                                    |
        <<<<                        |  :key
                                    |
        [>>>>+<<<+<-]               |  add :key to :cc := :shifted
        >                           |  |
        [<+>-]                      |  |
                                    |
                                    |  memory: | key | 0 | c | 0 | cc/shifted | 0 | 0 | 0 | 0 | 0 | divisor |
                                    |                  ^
                                    |
        >>>>>>>>>                   |  :divisor
        ++++++++++++++++++++++++++  |  = 26
                                    |
        <<<<<<                      |  :shifted
                                    |
                                    |  memory: | shifted/remainder | 0 | 0 | 0 | 0 | 0 | divisor |
                                    |                   ^
                                    |
                                    |  shifted % divisor
        [                           |  |  while :remainder
                                    |  |  |
                                    |  |  |  memory: | shifted | 0 | 0 | 0 | 0 | 0 | divisor | 0 |
                                    |  |  |               ^
                                    |  |  |
            [>>>>>>>+<<<<+<<<-]     |  |  |  duplicate :cshifted : copy of shifted
                                    |  |  |
                                    |  |  |  memory: | 0 | 0 | 0 | shifted | 0 | 0 | divisor | cshifted |
                                    |  |  |            ^
            >>>>>>                  |  |  |  :divisor                                   ^
                                    |  |  |
            [<<+>+>-]               |  |  |  duplicate
            <                       |  |  |  |
            [>+<-]                  |  |  |  |
            <                       |  |  |  |  :cdiv : copy of divisor
                                    |  |  |
                                    |  |  |  memory: | 0 | 0 | 0 | shifted | cdiv | 0 | divisor | cshifted |
                                    |  |  |                                    ^
                                    |  |  |
                                    |  |  |  subtract :cdiv from :shifted until :shifted is 0
            [                       |  |  |  |  while :cdiv
                <                   |  |  |  |  |  :shifted
                                    |  |  |  |  |
                [<<+>+>-]           |  |  |  |  |  duplicate
                <                   |  |  |  |  |  |
                [>+<-]              |  |  |  |  |  |
                <                   |  |  |  |  |  | :csh
                                    |  |  |  |  |
                                    |  |  |  |  |  memory: | 0 | csh | 0 | shifted/remainder | cdiv | 0 | divisor | cshifted |
                                    |  |  |  |  |                 ^
                                    |  |  |  |  |
                                    |  |  |  |  |  subtract 1 from :shifted if not 0
                [                   |  |  |  |  |  |  if :csh
                    >>-<<           |  |  |  |  |  |  |  subtract 1 from :shifted
                    [-]             |  |  |  |  |  |  |  clear
                ]                   |  |  |  |  |  |  |
                                    |  |  |  |  |
                >>>                 |  |  |  |  |  :cdiv
                -                   |  |  |  |  |  subtract 1
            ]                       |  |  |  |  |
                                    |  |  |
                                    |  |  |
                                    |  |  |  memory: | 0 | 0 | 0 | remainder | 0 | 0 | divisor | cshifted |
                                    |  |  |                                    ^
            <                       |  |  |  :remainder                ^
                                    |  |  |
            [>+<<<<+>>>-]           |  |  |  duplicate
                                    |  |  |
                                    |  |  |  memory: | remainder | 0 | 0 | 0 | crem | 0 | divisor | shifted/modulus |
                                    |  |  |                                ^
            >                       |  |  |  :crem                               ^
                                    |  |  |
                                    |  |  |  clean up modulus if a remainder is left
            [                       |  |  |  if :crem
                >>>[-]<<<           |  |  |  |  clear :modulus
                [-]                 |  |  |  |  clear
            ]                       |  |  |  |
                                    |  |  |
                                    |  |  |  if subtracting :cdiv from :shifted left a remainder we need to do continue subtracting;
                                    |  |  |  otherwise modulus is the modulus between :divisor and :shifted
                                    |  |  |
            <<<<                    |  |  |  :remainder
        ]                           |  |  |
                                    |
                                    |  memory: | 0 | 0 | 0 | 0 | 0 | 0 | divisor | modulus | 0 | cmod | eq26 |
                                    |            ^
                                    |
        >>>>>>>                     |  :modulus
                                    |
        [>>+<+<-]                   |  duplicate
        >                           |  |
        [<+>-]                      |  |
        >                           |  |  :cmod : copy of :modulus
                                    |
                                    |  memory: | 0 | 0 | 0 | 0 | 0 | 0 | divisor | modulus | 0 | cmod | eq26 |
                                    |                                                              ^
                                    |
        --------------------------  |  subtract 26
                                    |
        >                           |  :eq26 : condition equal to 26
        +                           |  add 1 (set true)
                                    |
        <                           |  :cmod
        [                           |  if :cmod not equal 26
            >-<                     |  |  subtract 1 from :eq26 (set false)
            [-]                     |  |  clear
        ]                           |  |
                                    |
        >                           |  :eq26
                                    |
        [                           |  if :eq26
            <<<[-]>>>               |  |  clear :modulus
            [-]                     |  |  clear
        ]                           |  |
                                    |
                                    |  the modulus operation above gives 26 as a valid modulus; so this is a workaround for setting a
                                    |  modulus value of 26 to 0
                                    |
        <<<<                        |
        [-]                         |  clear :divisor
                                    |
                                    |  memory: | c | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | modulus |
                                    |                                            ^
        >                           |  :modulus                                         ^
        [<<<<<<<+>>>>>>>-]          |  move :modulus
                                    |
                                    |  memory: | c | 0 | modulus/cc | 0 | 0 | 0 | 0 | 0 | 0 | 0 |
                                    |                                                         ^
        <<<<<<<                     |  :modulus/cc            ^
                                    |
                                    |  add 97 (ascii for 'a'; making 0 a; 1 b; etc)
        +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        .                           |  print
        [-]                         |  clear
    ]                               |

                                    memory: | c | 0 | modulus/cc |
                                                           ^
    <<                                 :c     ^
]
```

Usage:
Give to the program the key and the word to encrypt, each followed by a space.

Input:
<!-- Using whitespace syntax highlighting to show the spaces, used by the program to separate arguments -->

```whitespace>10 abc </lang

Output:

```txt
klm
```

Input:

```whitespace>16 klm </lang

Output:

```txt
abc
```



## C


```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define caesar(x) rot(13, x)
#define decaesar(x) rot(13, x)
#define decrypt_rot(x, y) rot((26-x), y)

void rot(int c, char *str)
{
	int l = strlen(str);
	const char *alpha[2] = { "abcdefghijklmnopqrstuvwxyz", "ABCDEFGHIJKLMNOPQRSTUVWXYZ"};

	int i;
	for (i = 0; i < l; i++)
	{
		if (!isalpha(str[i]))
			continue;

		if (isupper(str[i]))
                        str[i] = alpha[1][((int)(tolower(str[i]) - 'a') + c) % 26];
                else
                        str[i] = alpha[0][((int)(tolower(str[i]) - 'a') + c) % 26];
	}
}


int main()
{
	char str[] = "This is a top secret text message!";

	printf("Original: %s\n", str);
	caesar(str);
	printf("Encrypted: %s\n", str);
	decaesar(str);
	printf("Decrypted: %s\n", str);

	return 0;
}
```



## C++


```cpp
#include <string>
#include <iostream>
#include <algorithm>
#include <cctype>

class MyTransform {
private :
   int shift ;
public :
   MyTransform( int s ) : shift( s ) { }

  char operator( )( char c ) {
      if ( isspace( c ) )
	 return ' ' ;
      else {
	 static std::string letters( "abcdefghijklmnopqrstuvwxyz" ) ;
	 std::string::size_type found = letters.find(tolower( c )) ;
	 int shiftedpos = ( static_cast<int>( found ) + shift ) % 26 ;
	 if ( shiftedpos < 0 ) //in case of decryption possibly
	    shiftedpos = 26 + shiftedpos ;
	 char shifted = letters[shiftedpos] ;
	 return shifted ;
      }
  }
} ;

int main( ) {
   std::string input ;
   std::cout << "Which text is to be encrypted ?\n" ;
   getline( std::cin , input ) ;
   std::cout << "shift ?\n" ;
   int myshift = 0 ;
   std::cin >> myshift ;
   std::cout << "Before encryption:\n" << input << std::endl ;
   std::transform ( input.begin( ) , input.end( ) , input.begin( ) ,
	 MyTransform( myshift ) ) ;
   std::cout << "encrypted:\n" ;
   std::cout << input << std::endl ;
   myshift *= -1 ; //decrypting again
   std::transform ( input.begin( ) , input.end( ) , input.begin( ) ,
	 MyTransform( myshift ) ) ;
   std::cout << "Decrypted again:\n" ;
   std::cout << input << std::endl ;
   return 0 ;
}
```

{{out}}

```txt
Which text is to be encrypted ?
this is an interesting text
shift ?
3
Before encryption:
this is an interesting text
encrypted:
wklv lv dq lqwhuhvwlqj whaw
Decrypted again:
this is an interesting text

```



'''Alternative version - ''lambda functions, auto, iterators'' '''

===={{works with|C++-11}}====


```Cpp
/* caesar cipher */

#include <string>
#include <iostream>
#include <cctype>

int main( ) {

  using namespace std;

  string input ;
  int key = 0;

  // lambda functions

  auto encrypt = [&](char c, int key ) {
    char A  = ( islower(c) )? 'a': 'A';
    c = (isalpha(c))? (c - A + key) % 26 + A : c;
    return (char) c;
  };

  auto decrypt = [&](char c, int key ) {
    char A  = ( islower(c) )? 'a': 'A';
    c = (isalpha(c))? (c - A + (26 - key) ) % 26 + A : c;
    return (char) c;
  };


  cout << "Enter a line of text.\n";
  getline( cin , input );

  cout << "Enter an integer to shift text.\n";
  cin  >> key;

  while ( (key < 1) || (key > 25) )
    {
      cout << "must be an integer between 1 and 25 -->" << endl;
      cin  >> key;
    }

  cout << "Plain:    \t" << input << endl ;

  for ( auto & cp : input)    // use & for mutability
      cp = encrypt(cp, key);

  cout << "Encrypted:\t" << input << endl;

  for ( auto & cp : input)
      cp = decrypt(cp, key);

  cout << "Decrypted:\t" << input << endl;

  return 0 ;
}

```


{{out}}

```txt

Enter a line of text.
This is a line of plain text, 50 characters long.
Enter an integer to shift text.
5
Plain:    	This is a line of plain text, 50 characters long.
Encrypted:	Ymnx nx f qnsj tk uqfns yj}y1 :5 hmfwfhyjwx qtsl3
Decrypted:	This is a line of plain text, 50 characters long.

```


## C#

```c#
using System;
using System.Linq;

namespace CaesarCypher
{
    class Program
    {
        static char Encrypt(char ch, int code)
        {
            if (!char.IsLetter(ch)) return ch;

            char offset = char.IsUpper(ch) ? 'A' : 'a';
            return (char)((ch + code - offset) % 26 + offset);
        }

        static string Encrypt(string input, int code)
        {
            return new string(input.Select(ch => Encrypt(ch, code)).ToArray());
        }

        static string Decrypt(string input, int code)
        {
            return Encrypt(input, 26 - code);
        }

        const string TestCase = "Pack my box with five dozen liquor jugs.";

        static void Main()
        {
            string str = TestCase;

            Console.WriteLine(str);
            str = Encrypt(str, 5);
            Console.WriteLine("Encrypted: " + str);
            str = Decrypt(str, 5);
            Console.WriteLine("Decrypted: " + str);
            Console.ReadKey();
        }
    }
}
```

{{out}}

```txt
Pack my box with five dozen liquor jugs.
Encrypted: Ufhp rd gtc bnym knaj itejs qnvztw ozlx.
Decrypted: Pack my box with five dozen liquor jugs.
```



## Clojure

Readable version:

```Clojure
(defn encrypt-character [offset c]
  (if (Character/isLetter c)
    (let [v (int c)
          base (if (>= v (int \a))
                 (int \a)
                 (int \A))
          offset (mod offset 26)] ;works with negative offsets too!
      (char (+ (mod (+ (- v base) offset) 26)
               base)))
    c))

(defn encrypt [offset text]
  (apply str (map #(encrypt-character offset %) text)))

(defn decrypt [offset text]
  (encrypt (- 26 offset) text))


(let [text "The Quick Brown Fox Jumps Over The Lazy Dog."
      enc (encrypt -1 text)]
  (print "Original text:" text "\n")
  (print "Encryption:" enc "\n")
  (print "Decryption:" (decrypt -1 enc) "\n"))
```


output:

```txt

Original text: The Quick Brown Fox Jumps Over The Lazy Dog.
Encryption: Sgd Pthbj Aqnvm Enw Itlor Nudq Sgd Kzyx Cnf.
Decryption: The Quick Brown Fox Jumps Over The Lazy Dog.
```


Terser version using replace:

```Clojure
(defn encode [k s]
  (let [f #(take 26 (drop %3 (cycle (range (int %1) (inc (int %2))))))
        a #(map char (concat (f \a \z %) (f \A \Z %)))]
    (apply str (replace (zipmap (a 0) (a k)) s))))

(defn decode [k s]
  (encode (- 26 k) s))
```


output:
```txt

(encode 12 "The Quick Brown Fox jumped over the lazy dog")
=> "Ftq Cguow Ndaiz Raj vgybqp ahqd ftq xmlk pas"
(decode 12 (encode 12 "The Quick Brown Fox jumped over the lazy dog"))
=> "The Quick Brown Fox jumped over the lazy dog"

```



## COBOL

COBOL-85 ASCII or EBCIDIC

```COBOL

       identification division.
       program-id. caesar.
       data division.
       1 msg pic x(50)
           value "The quick brown fox jumped over the lazy dog.".
       1 offset binary pic 9(4) value 7.
       1 from-chars pic x(52).
       1 to-chars pic x(52).
       1 tabl.
        2 pic x(26) value "abcdefghijklmnopqrstuvwxyz".
        2 pic x(26) value "ABCDEFGHIJKLMNOPQRSTUVWXYZ".
        2 pic x(26) value "abcdefghijklmnopqrstuvwxyz".
        2 pic x(26) value "ABCDEFGHIJKLMNOPQRSTUVWXYZ".
       procedure division.
       begin.
           display msg
           perform encrypt
           display msg
           perform decrypt
           display msg
           stop run
           .

       encrypt.
           move tabl (1:52) to from-chars
           move tabl (1 + offset:52) to to-chars
           inspect msg converting from-chars
               to to-chars
           .

       decrypt.
           move tabl (1 + offset:52) to from-chars
           move tabl (1:52) to to-chars
           inspect msg converting from-chars
               to to-chars
           .
       end program caesar.

```

{{out}}

```txt

The quick brown fox jumped over the lazy dog.
aol xBpjr iyvDu mvE qBtwlk vCly Aol shGF kvn.
The quick brown fox jumped over the lazy dog.

```


{{works with|OpenCOBOL|2.0}}

```cobol>       >
SOURCE FORMAT IS FREE
PROGRAM-ID. caesar-cipher.

ENVIRONMENT DIVISION.
CONFIGURATION SECTION.
REPOSITORY.
    FUNCTION encrypt
    FUNCTION decrypt
    .
DATA DIVISION.
WORKING-STORAGE SECTION.
01  plaintext                 PIC X(50).
01  offset                    PIC 99.

01  encrypted-str             PIC X(50).

PROCEDURE DIVISION.
    DISPLAY "Enter a message to encrypt: " NO ADVANCING
    ACCEPT plaintext
    DISPLAY "Enter the amount to shift by: " NO ADVANCING
    ACCEPT offset

    MOVE FUNCTION encrypt(offset, plaintext) TO encrypted-str
    DISPLAY "Encrypted: " encrypted-str
    DISPLAY "Decrypted: " FUNCTION decrypt(offset, encrypted-str)
    .
END PROGRAM caesar-cipher.


FUNCTION-ID. encrypt.

DATA DIVISION.
LOCAL-STORAGE SECTION.
01  i                         PIC 9(3).

01  a                         PIC 9(3).

LINKAGE SECTION.
01  offset                    PIC 99.
01  str                       PIC X(50).

01  encrypted-str             PIC X(50).

PROCEDURE DIVISION USING offset, str RETURNING encrypted-str.
    MOVE str TO encrypted-str
    PERFORM VARYING i FROM 1 BY 1 UNTIL i > FUNCTION LENGTH(str)
        IF encrypted-str (i:1) IS NOT ALPHABETIC OR encrypted-str (i:1) = SPACE
            EXIT PERFORM CYCLE
        END-IF

        IF encrypted-str (i:1) IS ALPHABETIC-UPPER
            MOVE FUNCTION ORD("A") TO a
        ELSE
            MOVE FUNCTION ORD("a") TO a
        END-IF

        MOVE FUNCTION CHAR(FUNCTION MOD(FUNCTION ORD(encrypted-str (i:1))
                - a + offset, 26) + a)
            TO encrypted-str (i:1)
    END-PERFORM
    .
END FUNCTION encrypt.


FUNCTION-ID. decrypt.

ENVIRONMENT DIVISION.
CONFIGURATION SECTION.
REPOSITORY.
    FUNCTION encrypt
    .
DATA DIVISION.
LOCAL-STORAGE SECTION.
01  decrypt-offset            PIC 99.

LINKAGE SECTION.
01  offset                    PIC 99.
01  str                       PIC X(50).

01  decrypted-str             PIC X(50).

PROCEDURE DIVISION USING offset, str RETURNING decrypted-str.
    SUBTRACT 26 FROM offset GIVING decrypt-offset
    MOVE FUNCTION encrypt(decrypt-offset, str) TO decrypted-str
    .
END FUNCTION decrypt.
```


{{out}}

```txt

Enter a message to encrypt: The quick brown fox jumps over the lazy dog.
Enter the amount to shift by: 7
Encrypted: Aol xbpjr iyvdu mve qbtwz vcly aol shgf kvn.
Decrypted: The quick brown fox jumps over the lazy dog.

```



## CoffeeScript


```coffeescript
cipher = (msg, rot) ->
  msg.replace /([a-z|A-Z])/g, ($1) ->
    c = $1.charCodeAt(0)
    String.fromCharCode \
      if c >= 97
      then (c + rot + 26 - 97) % 26 + 97
      else (c + rot + 26 - 65) % 26 + 65

console.log cipher "Hello World", 2
console.log cipher "azAz %^&*()", 3
```

{{out}}

```txt

> coffee foo.coffee
Jgnnq Yqtnf
dcDc %^&*()

```



## Common Lisp


```lisp
(defun encipher-char (ch key)
  (let* ((c  (char-code  ch)) (la (char-code #\a)) (ua (char-code #\A))
         (base (cond ((<= la c (char-code #\z)) la)
                     ((<= ua c (char-code #\Z)) ua)
                     (nil))))
    (if base (code-char (+ (mod (+ (- c base) key) 26) base)) ch)))

(defun caesar-cipher (str key)
  (map 'string #'(lambda (c) (encipher-char c key)) str))

(defun caesar-decipher (str key) (caesar-cipher str (- key)))

(let* ((original-text "The five boxing wizards jump quickly")
       (key 3)
       (cipher-text (caesar-cipher original-text key))
       (recovered-text (caesar-decipher cipher-text key)))
  (format t " Original: ~a ~%" original-text)
  (format t "Encrypted: ~a ~%" cipher-text)
  (format t "Decrypted: ~a ~%" recovered-text))
```

{{out}}

```txt
 Original: The five boxing wizards jump quickly
Encrypted: Wkh ilyh eralqj zlcdugv mxps txlfnob
Decrypted: The five boxing wizards jump quickly
```


```lisp

(defun caesar-encipher (s k)
  (map 'string #'(lambda (c) (z c k)) s))

(defun caesar-decipher (s k) (caesar-encipher s (- k)))

(defun z (h k &aux (c (char-code  h))
                   (b (or (when (<= 97 c 122) 97)
                          (when (<= 65 c 90) 65))))
  (if b (code-char (+ (mod (+ (- c b) k) 26) b)) h))

```

{{out}}

```txt

> (caesar-encipher "The quick brown fox jumps over the lazy dog" 23)
"Qeb nrfzh yoltk clu grjmp lsbo qeb ixwv ald"
> (caesar-decipher "Qeb nrfzh yoltk clu grjmp lsbo qeb ixwv ald" 23)
"The quick brown fox jumps over the lazy dog"

```



## Crystal


```crystal
class String
  ALPHABET = ("A".."Z").to_a

  def caesar_cipher(num)
    self.tr(ALPHABET.join, ALPHABET.rotate(num).join)
  end
end

# demo
encrypted = "THEQUICKBROWNFOXJUMPSOVERTHELAZYDOG".caesar_cipher(5)
decrypted = encrypted.caesar_cipher(-5)

```



## Cubescript


```cubescript
alias modn [ mod (+ (mod $arg1 $arg2) $arg2) $arg2 ]
//Cubescript's built-in mod will fail on negative numbers

alias cipher [
	push alpha [
		"A B C D E F G H I J K L M N O P Q R S T U V W X Y Z"
		"a b c d e f g h i j k l m n o p q r s t u v w x y z"
	] [ push chars [] [
		loop i (strlen $arg1) [
			looplist n $alpha [
				if (! (listlen $chars)) [
					alias chars (? (> (listindex $n (substr $arg1 $i 1)) -1) $n [])
				]
			]
			alias arg1 (
				concatword (substr $arg1 0 $i) (
					? (> (listindex $chars (substr $arg1 $i 1)) -1) (
						at $chars (
							modn (+ (
								listindex $chars (substr $arg1 $i 1)
							) $arg2) (listlen $chars)
						)
					) (substr $arg1 $i 1)
				) (substr $arg1 (+ $i 1) (strlen $arg1))
			)
			alias chars []
		]
	] ]
	result $arg1
]

alias decipher [
	push alpha [
		"A B C D E F G H I J K L M N O P Q R S T U V W X Y Z"
		"a b c d e f g h i j k l m n o p q r s t u v w x y z"
	] [ push chars [] [
		loop i (strlen $arg1) [
			looplist n $alpha [
				if (! (listlen $chars)) [
					alias chars (? (> (listindex $n (substr $arg1 $i 1)) -1) $n [])
				]
			]
			alias arg1 (
				concatword (substr $arg1 0 $i) (
					? (> (listindex $chars (substr $arg1 $i 1)) -1) (
						at $chars (
							modn (- (
								listindex $chars (substr $arg1 $i 1)
							) $arg2 ) (listlen $chars)
						)
					) (substr $arg1 $i 1)
				) (substr $arg1 (+ $i 1) (strlen $arg1))
			)
			alias chars []
		]
	] ]
	result $arg1
]
```


Usage:
<lang>>>> cipher "The Quick Brown Fox Jumps Over The Lazy Dog." 5
> Ymj Vznhp Gwtbs Ktc Ozrux Tajw Ymj Qfed Itl.
>>> decipher "Ymj Vznhp Gwtbs Ktc Ozrux Tajw Ymj Qfed Itl." 5
> The Quick Brown Fox Jumps Over The Lazy Dog.
```



## D


```d
import std.stdio, std.traits;

S rot(S)(in S s, in int key) pure nothrow @safe
if (isSomeString!S) {
    auto res = s.dup;

    foreach (immutable i, ref c; res) {
        if ('a' <= c && c <= 'z')
            c = ((c - 'a' + key) % 26 + 'a');
        else if ('A' <= c && c <= 'Z')
            c = ((c - 'A' + key) % 26 + 'A');
    }
    return res;
}

void main() @safe {
    enum key = 3;
    immutable txt = "The five boxing wizards jump quickly";
    writeln("Original:  ", txt);
    writeln("Encrypted: ", txt.rot(key));
    writeln("Decrypted: ", txt.rot(key).rot(26 - key));
}
```

{{out}}

```txt
Original:  The five boxing wizards jump quickly
Encrypted: Wkh ilyh eralqj zlcdugv mxps txlfnob
Decrypted: The five boxing wizards jump quickly
```

Simpler in-place version (same output):

```d
import std.stdio, std.ascii;

void inplaceRot(char[] txt, in int key) pure nothrow {
    foreach (ref c; txt) {
        if (isLower(c))
            c = (c - 'a' + key) % 26 + 'a';
        else if (isUpper(c))
            c = (c - 'A' + key) % 26 + 'A';
    }
}

void main() {
    enum key = 3;
    auto txt = "The five boxing wizards jump quickly".dup;
    writeln("Original:  ", txt);
    txt.inplaceRot(key);
    writeln("Encrypted: ", txt);
    txt.inplaceRot(26 - key);
    writeln("Decrypted: ", txt);
}
```


A version that uses the standard library (same output):

```d
import std.stdio, std.ascii, std.string, std.algorithm;

string rot(in string s, in int key) pure nothrow @safe {
    auto uppr = uppercase.dup.representation;
    bringToFront(uppr[0 .. key], uppr[key .. $]);
    auto lowr = lowercase.dup.representation;
    bringToFront(lowr[0 .. key], lowr[key .. $]);
    return s.translate(makeTrans(letters, assumeUTF(uppr ~ lowr)));
}

void main() {
    enum key = 3;
    immutable txt = "The five boxing wizards jump quickly";
    writeln("Original:  ", txt);
    writeln("Encrypted: ", txt.rot(key));
    writeln("Decrypted: ", txt.rot(key).rot(26 - key));
}
```



## Dart


```dart
class Caesar {
  int _key;

  Caesar(this._key);

  int _toCharCode(String s) {
    return s.charCodeAt(0);
  }

  String _fromCharCode(int ch) {
    return new String.fromCharCodes([ch]);
  }

  String _process(String msg, int offset) {
    StringBuffer sb=new StringBuffer();
    for(int i=0;i<msg.length;i++) {
      int ch=msg.charCodeAt(i);
      if(ch>=_toCharCode('A')&&ch<=_toCharCode('Z')) {
        sb.add(_fromCharCode(_toCharCode("A")+(ch-_toCharCode("A")+offset)%26));
      }
      else if(ch>=_toCharCode('a')&&ch<=_toCharCode('z')) {
        sb.add(_fromCharCode(_toCharCode("a")+(ch-_toCharCode("a")+offset)%26));
      } else {
        sb.add(msg[i]);
      }
    }
    return sb.toString();
  }

  String encrypt(String msg) {
    return _process(msg, _key);
  }

  String decrypt(String msg) {
    return _process(msg, 26-_key);
   }
}

void trip(String msg) {
  Caesar cipher=new Caesar(10);

  String enc=cipher.encrypt(msg);
  String dec=cipher.decrypt(enc);
  print("\"$msg\" encrypts to:");
  print("\"$enc\" decrypts to:");
  print("\"$dec\"");
  Expect.equals(msg,dec);
}

main() {
  Caesar c2=new Caesar(2);
  print(c2.encrypt("HI"));
  Caesar c20=new Caesar(20);
  print(c20.encrypt("HI"));

  // try a few roundtrips

  trip("");
  trip("A");
  trip("z");
  trip("Caesar cipher");
  trip(".-:/\"\\!");
  trip("The Quick Brown Fox Jumps Over The Lazy Dog.");
}
```

{{out}}

```txt
JK
BC
"" encrypts to:
"" decrypts to:
""
"A" encrypts to:
"K" decrypts to:
"A"
"z" encrypts to:
"j" decrypts to:
"z"
"Caesar cipher" encrypts to:
"Mkockb mszrob" decrypts to:
"Caesar cipher"
".-:/"\!" encrypts to:
".-:/"\!" decrypts to:
".-:/"\!"
"The Quick Brown Fox Jumps Over The Lazy Dog." encrypts to:
"Dro Aesmu Lbygx Pyh Tewzc Yfob Dro Vkji Nyq." decrypts to:
"The Quick Brown Fox Jumps Over The Lazy Dog."
```




## Dyalect


{{trans|C#}}


```dyalect
func Char.encrypt(code) {
    if !this.isLetter() {
        return this
    }
    var offset = (if this.isUpper() {'A'} else {'a'}).order()
    return Char((this.order() + code - offset) % 26 + offset)
}

func String.encrypt(code) {
    var xs = []
    for c in this {
        xs.add(c.encrypt(code))
    }
    return String.concat(values: xs)
}

func String.decrypt(code) {
    this.encrypt(26 - code);
}

var str = "Pack my box with five dozen liquor jugs."
print(str)
str = str.encrypt(5)
print("Encrypted: \(str)")
str = str.decrypt(5)
print("Decrypted: \(str)")
```


{{out}}


```txt
Pack my box with five dozen liquor jugs.
Encrypted: Ufhp rd gtc bnym knaj itejs qnvztw ozlx.
Decrypted: Pack my box with five dozen liquor jugs.
```



## Eiffel


```eiffel

class
	APPLICATION

inherit
	ARGUMENTS

create
	make

feature {NONE} -- Initialization

	make
			-- Run application.
		local
			s: STRING_32
		do
			s := "The tiny tiger totally taunted the tall Till."
			print ("%NString to encode: " + s)
			print ("%NEncoded string: " + encode (s, 12))
			print ("%NDecoded string (after encoding and decoding): " + decode (encode (s, 12), 12))
		end

feature		-- Basic operations

	decode (to_be_decoded: STRING_32; offset: INTEGER): STRING_32
				-- Decode `to be decoded' according to `offset'.
		do
			Result := encode (to_be_decoded, 26 - offset)
		end

	encode (to_be_encoded: STRING_32; offset: INTEGER): STRING_32
				-- Encode `to be encoded' according to `offset'.
		local
			l_offset: INTEGER
			l_char_code: INTEGER
		do
			create Result.make_empty
			l_offset := (offset \\ 26) + 26
			across to_be_encoded as tbe loop
				if tbe.item.is_alpha then
					if tbe.item.is_upper then
						l_char_code := ('A').code + (tbe.item.code - ('A').code + l_offset) \\ 26
						Result.append_character (l_char_code.to_character_32)
					else
						l_char_code := ('a').code + (tbe.item.code - ('a').code + l_offset) \\ 26
						Result.append_character (l_char_code.to_character_32)
					end
				else
					Result.append_character (tbe.item)
				end
			end
		end
end

```

{{out}}

```txt

String to encode: The tiny tiger totally taunted the tall Till.
Encoded string: Ftq fuzk fusqd fafmxxk fmgzfqp ftq fmxx Fuxx.
Decoded string (after encoding and decoding): The tiny tiger totally taunted the tall Till.

```



## Ela



```ela
open number char monad io string

chars = "ABCDEFGHIJKLMOPQRSTUVWXYZ"

caesar _ _ [] = ""
caesar op key (x::xs) = check shifted ++ caesar op key xs
  where orig = indexOf (string.upper $ toString x) chars
        shifted = orig `op` key
        check val | orig == -1 = x
                  | val > 24 = trans $ val - 25
                  | val < 0 = trans $ 25 + val
                  | else = trans shifted
        trans idx = chars:idx

cypher = caesar (+)
decypher = caesar (-)

key = 2

do
  putStrLn "A string to encode:"
  str <- readStr
  putStr "Encoded string: "
  cstr <- return <| cypher key str
  put cstr
  putStrLn ""
  putStr "Decoded string: "
  put $ decypher key cstr
```


{{out}}


```txt
A string to encode:
HELLO! THIS IS A SECRET MESSAGE!
Encoded string: "JGOOQ! VJKU KU C UGETGV PGUUCIG!"
Decoded string: "HELLO! THIS IS A SECRET MESSAGE!"
```



## Elena

ELENA 4.x :

```elena
import system'routines;
import system'math;
import extensions;
import extensions'text;

const string Letters    = "abcdefghijklmnopqrstuvwxyz";
const string BigLetters = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
const string TestText   = "Pack my box with five dozen liquor jugs.";
const int Key = 12;

class Encrypting : Enumerator
{
    int        theKey;
    Enumerator theEnumerator;

    constructor(int key, string text)
    {
        theKey := key;
        theEnumerator := text.enumerator();
    }

    bool next() => theEnumerator;

    reset() => theEnumerator;

    enumerable() => theEnumerator;

    get()
    {
        var ch := theEnumerator.get();

        var index := Letters.indexOf(0, ch);

        if (-1 < index)
        {
            ^ Letters[(theKey+index).mod:26]
        }
        else
        {
            index := BigLetters.indexOf(0, ch);
            if (-1 < index)
            {
                ^ BigLetters[(theKey+index).mod:26]
            }
            else
            {
                ^ ch
            }
        }
    }
}

extension encryptOp
{
    encrypt(key)
        = new Encrypting(key, self).summarize(new StringWriter());

    decrypt(key)
        = new Encrypting(26 - key, self).summarize(new StringWriter());
}

public program()
{
    console.printLine("Original text :",TestText);

    var encryptedText := TestText.encrypt:Key;

    console.printLine("Encrypted text:",encryptedText);

    var decryptedText := encryptedText.decrypt:Key;

    console.printLine("Decrypted text:",decryptedText);

    console.readChar()
}
```

{{out}}

```txt

Original text :Pack my box with five dozen liquor jugs.
Encrypted text:Bmow yk naj iuft ruhq palqz xucgad vgse.
Decrypted text:Pack my box with five dozen liquor jugs.

```



## Elixir


```elixir
defmodule Caesar_cipher do
  defp set_map(map, range, key) do
    org = Enum.map(range, &List.to_string [&1])
    {a, b} = Enum.split(org, key)
    Enum.zip(org, b ++ a) |> Enum.into(map)
  end

  def encode(text, key) do
    map = Map.new |> set_map(?a..?z, key) |> set_map(?A..?Z, key)
    String.graphemes(text) |> Enum.map_join(fn c -> Map.get(map, c, c) end)
  end
end

text = "The five boxing wizards jump quickly"
key = 3
IO.puts "Original:  #{text}"
IO.puts "Encrypted: #{enc = Caesar_cipher.encode(text, key)}"
IO.puts "Decrypted: #{Caesar_cipher.encode(enc, -key)}"
```


{{out}}

```txt

Original:  The five boxing wizards jump quickly
Encrypted: Wkh ilyh eralqj zlcdugv mxps txlfnob
Decrypted: The five boxing wizards jump quickly

```



## Erlang


```Erlang

%% Ceasar cypher in Erlang for the rosetta code wiki.
%% Implemented by J.W. Luiten

-module(ceasar).
-export([main/2]).

%% rot: rotate Char by Key places
rot(Char,Key) when (Char >= $A) and (Char =< $Z) or
                   (Char >= $a) and (Char =< $z) ->
  Offset = $A + Char band 32,
  N = Char - Offset,
  Offset + (N + Key) rem 26;
rot(Char, _Key) ->
  Char.

%% key: normalize key.
key(Key) when Key < 0 ->
  26 + Key rem 26;
key(Key) when Key > 25 ->
  Key rem 26;
key(Key) ->
  Key.

main(PlainText, Key) ->
  Encode = key(Key),
  Decode = key(-Key),

  io:format("Plaintext ----> ~s~n", [PlainText]),

  CypherText = lists:map(fun(Char) -> rot(Char, Encode) end, PlainText),
  io:format("Cyphertext ---> ~s~n", [CypherText]),

  PlainText = lists:map(fun(Char) -> rot(Char, Decode) end, CypherText).


```

Command:
```Erlang
ceasar:main("The five boxing wizards jump quickly", 3).
```

{{out}}

```txt

Plaintext ----> The five boxing wizards jump quickly
Cyphertext ---> Wkh ilyh eralqj zlcdugv mxps txlfnob
"The five boxing wizards jump quickly"

```



## ERRE


```ERRE

PROGRAM CAESAR

!$INCLUDE="PC.LIB"

PROCEDURE CAESAR(TEXT$,KY%->CY$)
    LOCAL I%,C%
    FOR I%=1 TO LEN(TEXT$) DO
      C%=ASC(MID$(TEXT$,I%))
      IF (C% AND $1F)>=1 AND (C% AND $1F)<=26 THEN
        C%=(C% AND $E0) OR (((C% AND $1F)+KY%-1) MOD 26+1)
        CHANGE(TEXT$,I%,CHR$(C%)->TEXT$)
      END IF
    END FOR
    CY$=TEXT$
END PROCEDURE

BEGIN
    RANDOMIZE(TIMER)
    PLAINTEXT$="Pack my box with five dozen liquor jugs"
    PRINT(PLAINTEXT$)

    KY%=1+INT(25*RND(1))  ! generates random between 1 and 25
    CAESAR(PLAINTEXT$,KY%->CYPHERTEXT$)
    PRINT(CYPHERTEXT$)

    CAESAR(CYPHERTEXT$,26-KY%->DECYPHERED$)
    PRINT(DECYPHERED$)

END PROGRAM

```

{{out}}

```txt

Pack my box with five dozen liquor jugs
Qbdl nz cpy xjui gjwf epafo mjrvps kvht
Pack my box with five dozen liquor jugs

```



## Euphoria

{{works with|Euphoria|4.0.0}}

```Euphoria

--caesar cipher for Rosetta Code wiki
--User:Lnettnay

--usage eui caesar ->default text, key and encode flag
--usage eui caesar 'Text with spaces and punctuation!' 5 D
--If text has imbedded spaces must use apostophes instead of quotes so all punctuation works
--key = integer from 1 to 25, defaults to 13
--flag = E (Encode) or D (Decode), defaults to E
--no error checking is done on key or flag

include std/get.e
include std/types.e


sequence cmd = command_line()
sequence val
-- default text for encryption
sequence text = "The Quick Brown Fox Jumps Over The Lazy Dog."
atom key = 13 -- default to Rot-13
sequence flag = "E" -- default to Encrypt
atom offset
atom num_letters = 26 -- number of characters in alphabet

--get text
if length(cmd) >= 3 then
	text = cmd[3]
end if

--get key value
if length(cmd) >= 4 then
	val = value(cmd[4])
	key = val[2]
end if

--get Encrypt/Decrypt flag
if length(cmd) = 5 then
	flag = cmd[5]
	if compare(flag, "D") = 0 then
		key = 26 - key
	end if
end if

for i = 1 to length(text) do
	if t_alpha(text[i]) then
		if t_lower(text[i]) then
			offset = 'a'
		else
			offset = 'A'
		end if
		text[i] = remainder(text[i] - offset + key, num_letters) + offset
	end if
end for

printf(1,"%s\n",{text})


```

{{out}}

```txt

"The Quick Brown Fox Jumps Over The Lazy Dog." encrypts to:
"Gur Dhvpx Oebja Sbk Whzcf Bire Gur Ynml Qbt." decrypts to:
"The Quick Brown Fox Jumps Over The Lazy Dog."

```


=={{header|F_Sharp|F#}}==

```fsharp
module caesar =
    open System

    let private cipher n s =
        let shift c =
            if Char.IsLetter c then
                let a = (if Char.IsLower c then 'a' else 'A') |> int
                (int c - a + n) % 26 + a |> char
            else c
        String.map shift s

    let encrypt n = cipher n
    let decrypt n = cipher (26 - n)
```


```txt
&gt; caesar.encrypt 2 "HI";;
val it : string = "JK"
&gt; caesar.encrypt 20 "HI";;
val it : string = "BC"
&gt; let c = caesar.encrypt 13 "The quick brown fox jumps over the lazy dog.";;
val c : string = "Gur dhvpx oebja sbk whzcf bire gur ynml qbt."
&gt; caesar.decrypt 13 c;;
val it : string = "The quick brown fox jumps over the lazy dog."

```



## Factor

{{works with|Factor|0.97}}
{{trans|F#}}

```factor
USING: io kernel locals math sequences unicode.categories ;
IN: rosetta-code.caesar-cipher

:: cipher ( n s -- s' )
    [| c |
        c Letter? [
            c letter? CHAR: a CHAR: A ? :> a
            c a - n + 26 mod a +
        ]
        [ c ] if
    ] :> shift
    s [ shift call ] map ;

: encrypt ( n s -- s' ) cipher ;
: decrypt ( n s -- s' ) [ 26 swap - ] dip cipher ;

11 "Esp bftnv mczhy qzi ufxapo zgpc esp wlkj ozr." decrypt print
11 "The quick brown fox jumped over the lazy dog." encrypt print
```

{{out}}

```txt

The quick brown fox jumped over the lazy dog.
Esp bftnv mczhy qzi ufxapo zgpc esp wlkj ozr.

```



## Fantom


Shifts upper/lower case letters, leaves other characters as they are.


```fantom

class Main
{
  static Int shift (Int char, Int key)
  {
    newChar := char + key
    if (char >= 'a' && char <= 'z')
    {
      if (newChar - 'a' < 0)   { newChar += 26 }
      if (newChar - 'a' >= 26) { newChar -= 26 }
    }
    else if (char >= 'A' && char <= 'Z')
    {
      if (newChar - 'A' < 0)   { newChar += 26 }
      if (newChar - 'A' >= 26) { newChar -= 26 }
    }
    else // not alphabetic, so keep as is
    {
      newChar = char
    }
    return newChar
  }

  static Str shiftStr (Str msg, Int key)
  {
    res := StrBuf()
    msg.each { res.addChar (shift(it, key)) }
    return res.toStr
  }

  static Str encode (Str msg, Int key)
  {
    return shiftStr (msg, key)
  }

  static Str decode (Str msg, Int key)
  {
    return shiftStr (msg, -key)
  }

  static Void main (Str[] args)
  {
    if (args.size == 2)
    {
      msg := args[0]
      key := Int(args[1])

      echo ("$msg with key $key")
      echo ("Encode: ${encode(msg, key)}")
      echo ("Decode: ${decode(encode(msg, key), key)}")
    }
  }
}

```


Example:

```txt

$ fan caesar.fan "Encrypt - With ! Case," 1
Encrypt - With ! Case, with key 1
Encode: Fodszqu - Xjui ! Dbtf,
Decode: Encrypt - With ! Case,

$ fan caesar.fan "Encrypt - With ! Case," 5
Encrypt - With ! Case, with key 5
Encode: Jshwduy - Bnym ! Hfxj,
Decode: Encrypt - With ! Case,

$ fan caesar.fan "Encrypt - With ! Case," 10
Encrypt - With ! Case, with key 10
Encode: Oxmbizd - Gsdr ! Mkco,
Decode: Encrypt - With ! Case,

```



## Forth


```forth
: ceasar ( c n -- c )
  over 32 or [char] a -
  dup 0 26 within if
    over + 25 > if 26 - then +
  else 2drop then ;

: ceasar-string ( n str len -- )
  over + swap do i c@ over ceasar i c! loop drop ;

: ceasar-inverse ( n -- 'n ) 26 swap - 26 mod ;

2variable test
s" The five boxing wizards jump quickly!" test 2!

3 test 2@ ceasar-string
test 2@ cr type

3 ceasar-inverse test 2@ ceasar-string
test 2@ cr type
```



## Fortran

{{works with|Fortan 90 and later}}

```fortran
program Caesar_Cipher
  implicit none

  integer, parameter :: key = 3
  character(43) :: message = "The five boxing wizards jump quickly"

  write(*, "(2a)") "Original message  = ", message
  call encrypt(message)
  write(*, "(2a)") "Encrypted message = ", message
  call decrypt(message)
  write(*, "(2a)") "Decrypted message = ", message

contains

subroutine encrypt(text)
  character(*), intent(inout) :: text
  integer :: i

  do i = 1, len(text)
    select case(text(i:i))
      case ('A':'Z')
        text(i:i) = achar(modulo(iachar(text(i:i)) - 65 + key, 26) + 65)
      case ('a':'z')
        text(i:i) = achar(modulo(iachar(text(i:i)) - 97 + key, 26) + 97)
    end select
  end do
end subroutine

subroutine decrypt(text)
  character(*), intent(inout) :: text
  integer :: i

  do i = 1, len(text)
    select case(text(i:i))
      case ('A':'Z')
        text(i:i) = achar(modulo(iachar(text(i:i)) - 65 - key, 26) + 65)
      case ('a':'z')
        text(i:i) = achar(modulo(iachar(text(i:i)) - 97 - key, 26) + 97)
    end select
  end do
end subroutine

end program Caesar_Cipher
```

{{out}}

```txt
Original message  = The five boxing wizards jump quickly
Encrypted message = Wkh ilyh eralgj zlcdugv mxps txlfnob
Decrypted message = The five boxing wizards jump quickly
```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Sub Encrypt(s As String, key As Integer)
  Dim c As Integer
  For i As Integer = 0 To Len(s)
    Select Case As Const s[i]
      Case 65 To 90
        c = s[i] + key
        If c > 90 Then c -= 26
        s[i] = c
      Case 97 To 122
        c = s[i] + key
        If c > 122 Then c -= 26
        s[i] = c
    End Select
  Next
End Sub

Sub Decrypt(s As String, key As Integer)
  Dim c As Integer
  For i As Integer = 0 To Len(s)
    Select Case As Const s[i]
      Case 65 To 90
        c = s[i] - key
        If c < 65 Then c += 26
        s[i] = c
      Case 97 To 122
        c = s[i] - key
        If c < 97 Then c += 26
        s[i] = c
    End Select
  Next
End Sub

Dim As String s = "Bright vixens jump; dozy fowl quack."
Print "Plain text : "; s
Encrypt s, 8
Print "Encrypted  : "; s
Decrypt s, 8
Print "Decrypted  : "; s
Sleep
```


{{out}}

```txt

Plain text : Bright vixens jump; dozy fowl quack.
Encrypted  : Jzqopb dqfmva rcux; lwhg nwet yciks.
Decrypted  : Bright vixens jump; dozy fowl quack.

```



## Gambas

'''[https://gambas-playground.proko.eu/?gist=cb96008082bc0d8278224cd2a5ec74d3 Click this link to run this code]'''

```gambas
Public Sub Main()
Dim byKey As Byte = 3                                                                 'The key (Enter 26 to get the same output as input)
Dim byCount As Byte                                                                   'Counter
Dim sCeasar As String = "ABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZ"        'Used to calculate the cipher
Dim sString As String = "The five boxing wizards jump quickly"                        'Phrase to encrypt
Dim sCoded, sTemp As String                                                           'Various strings

For byCount = 1 To Len(sString)                                                       'Count through each letter in the phrase
  If Mid(sString, byCount, 1) = " " Then                                              'If it's a space..
    sCoded &= " "                                                                     'Keep it a space
    Continue                                                                          'Jump to the next iteration of the loop
  Endif
  sTemp = Mid(sCeasar, InStr(sCeasar, Mid(UCase(sString), byCount, 1)) + byKey, 1)    'Get the new 'coded' letter
  If Asc(Mid(sString, byCount, 1)) > 96 Then sTemp = Chr(Asc(sTemp) + 32)             'If the original was lower case then make the new 'coded' letter lower case
  sCoded &= sTemp                                                                     'Add the result to the code string
Next

Print sString & gb.NewLine & sCoded                                                   'Print the result

End
```

Output:

```txt

The five boxing wizards jump quickly
Wkh ilyh eralqj zlcdugv mxps txlfnob

```



## GAP


```gap
CaesarCipher := function(s, n)
	local r, c, i, lower, upper;
	lower := "abcdefghijklmnopqrstuvwxyz";
	upper := "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
	r := "";
	for c in s do
		i := Position(lower, c);
		if i <> fail then
			Add(r, lower[RemInt(i + n - 1, 26) + 1]);
		else
			i := Position(upper, c);
			if i <> fail then
				Add(r, upper[RemInt(i + n - 1, 26) + 1]);
			else
				Add(r, c);
			fi;
		fi;
	od;
	return r;
end;

CaesarCipher("IBM", 25);
# "HAL"

CaesarCipher("Vgg cphvi wzdibn vmz wjmi amzz viy zlpvg di ydbidot viy mdbcon.", 5);
# "All human beings are born free and equal in dignity and rights."
```



## GFA Basic


```basic

'
' Caesar cypher
'
OPENW 1 ! Creates a window for handling input/output
CLEARW 1
INPUT "string to encrypt ";text$
INPUT "encryption key    ";key%
encrypted$=@encrypt$(UPPER$(text$),key%)
PRINT "Encrypted: ";encrypted$
PRINT "Decrypted: ";@decrypt$(encrypted$,key%)
'
PRINT "(Press any key to end program.)"
~INP(2)
CLOSEW 1
'
FUNCTION encrypt$(text$,key%)
  LOCAL result$,i%,c%
  result$=""
  FOR i%=1 TO LEN(text$)
    c%=ASC(MID$(text$,i%))
    IF c%<ASC("A") OR c%>ASC("Z") ! don't encrypt non A-Z
      result$=result$+CHR$(c%)
    ELSE
      c%=c%+key%
      IF c%>ASC("Z")
        c%=c%-26
      ENDIF
      result$=result$+CHR$(c%)
    ENDIF
  NEXT i%
  RETURN result$
ENDFUNC
'
FUNCTION decrypt$(text$,key%)
  RETURN @encrypt$(text$,26-key%)
ENDFUNC

```



## Go

Obvious solution with explicit testing for character ranges:

```go
package main

import (
    "fmt"
    "strings"
)

type ckey struct {
    enc, dec func(rune) rune
}

func newCaesar(k int) (*ckey, bool) {
    if k < 1 || k > 25 {
        return nil, false
    }
    rk := rune(k)
    return &ckey{
        enc: func(c rune) rune {
            if c >= 'a' && c <= 'z'-rk || c >= 'A' && c <= 'Z'-rk {
                return c + rk
            } else if c > 'z'-rk && c <= 'z' || c > 'Z'-rk && c <= 'Z' {
                return c + rk - 26
            }
            return c
        },
        dec: func(c rune) rune {
            if c >= 'a'+rk && c <= 'z' || c >= 'A'+rk && c <= 'Z' {
                return c - rk
            } else if c >= 'a' && c < 'a'+rk || c >= 'A' && c < 'A'+rk {
                return c - rk + 26
            }
            return c
        },
    }, true
}

func (ck ckey) encipher(pt string) string {
    return strings.Map(ck.enc, pt)
}

func (ck ckey) decipher(ct string) string {
    return strings.Map(ck.dec, ct)
}

func main() {
    pt := "The five boxing wizards jump quickly"
    fmt.Println("Plaintext:", pt)
    for _, key := range []int{0, 1, 7, 25, 26} {
        ck, ok := newCaesar(key)
        if !ok {
            fmt.Println("Key", key, "invalid")
            continue
        }
        ct := ck.encipher(pt)
        fmt.Println("Key", key)
        fmt.Println("  Enciphered:", ct)
        fmt.Println("  Deciphered:", ck.decipher(ct))
    }
}
```

Data driven version using functions designed for case conversion.  (And for method using % operator, see [[Vigen%C3%A8re_cipher#Go]].)

```go
package main

import (
    "fmt"
    "strings"
    "unicode"
)

type ckey struct {
    enc, dec unicode.SpecialCase
}

func newCaesar(k int) (*ckey, bool) {
    if k < 1 || k > 25 {
        return nil, false
    }
    i := uint32(k)
    r := rune(k)
    return &ckey{
        unicode.SpecialCase{
            {'A', 'Z' - i, [3]rune{r}},
            {'Z' - i + 1, 'Z', [3]rune{r - 26}},
            {'a', 'z' - i, [3]rune{r}},
            {'z' - i + 1, 'z', [3]rune{r - 26}},
        },
        unicode.SpecialCase{
            {'A', 'A' + i - 1, [3]rune{26 - r}},
            {'A' + i, 'Z', [3]rune{-r}},
            {'a', 'a' + i - 1, [3]rune{26 - r}},
            {'a' + i, 'z', [3]rune{-r}},
        },
    }, true
}

func (ck ckey) encipher(pt string) string {
    return strings.ToUpperSpecial(ck.enc, pt)
}

func (ck ckey) decipher(ct string) string {
    return strings.ToUpperSpecial(ck.dec, ct)
}

func main() {
    pt := "The five boxing wizards jump quickly"
    fmt.Println("Plaintext:", pt)
    for _, key := range []int{0, 1, 7, 25, 26} {
        ck, ok := newCaesar(key)
        if !ok {
            fmt.Println("Key", key, "invalid")
            continue
        }
        ct := ck.encipher(pt)
        fmt.Println("Key", key)
        fmt.Println("  Enciphered:", ct)
        fmt.Println("  Deciphered:", ck.decipher(ct))
    }
}
```

{{out}} (either version)

```txt

Plaintext: The five boxing wizards jump quickly
Key 0 invalid
Key 1
  Enciphered: Uif gjwf cpyjoh xjabset kvnq rvjdlmz
  Deciphered: The five boxing wizards jump quickly
Key 7
  Enciphered: Aol mpcl ivepun dpghykz qbtw xbpjrsf
  Deciphered: The five boxing wizards jump quickly
Key 25
  Enciphered: Sgd ehud anwhmf vhyzqcr itlo pthbjkx
  Deciphered: The five boxing wizards jump quickly
Key 26 invalid

```



## Groovy

Java style:

```groovy
def caesarEncode(​cipherKey, text) {
    def builder = new StringBuilder()
    text.each { character ->
        int ch = character[0] as char
        switch(ch) {
            case 'a'..'z': ch = ((ch - 97 + cipherKey) % 26 + 97); break
            case 'A'..'Z': ch = ((ch - 65 + cipherKey) % 26 + 65); break
        }
        builder << (ch as char)
    }
    builder as String
}
def caesarDecode(cipherKey, text) { caesarEncode(26 - cipherKey, text) }
```


Functional style:

```groovy
def caesarEncode(cipherKey, text) {
    text.chars.collect { c ->
        int off = c.isUpperCase() ? 'A' : 'a'
        c.isLetter() ? (((c as int) - off + cipherKey) % 26 + off) as char : c
    }.join()
}
def caesarDecode(cipherKey, text) { caesarEncode(26 - cipherKey, text) }
```


Ninja style:

```groovy
def caesarEncode(k, text) {
    (text as int[]).collect { it==' ' ? ' ' : (((it & 0x1f) + k - 1) % 26 + 1 | it & 0xe0) as char }.join()
}
def caesarDecode(k, text) { caesarEncode(26 - k, text) }
```


Using built in 'tr' function and a replacement alphabet:

```groovy
def caesarEncode(k, text) {
    text.tr('a-zA-Z', ((('a'..'z')*2)[k..(k+25)] + (('A'..'Z')*2)[k..(k+25)]).join())
}
def caesarDecode(cipherKey, text) { caesarEncode(26 - cipherKey, text) }
```

and the same with closures for somewhat better readability:

```groovy
def caesarEncode(k, text) {
    def c = { (it*2)[k..(k+25)].join() }
    text.tr('a-zA-Z', c('a'..'z') + c('A'..'Z'))
}
def caesarDecode(cipherKey, text) { caesarEncode(26 - cipherKey, text) }
```

Test code:

```groovy

def plainText = "The Quick Brown Fox jumped over the lazy dog"
def cipherKey = 12
def cipherText = caesarEncode(cipherKey, plainText)
def decodedText = caesarDecode(cipherKey, cipherText)

println "plainText: $plainText"
println "cypherText($cipherKey): $cipherText"
println "decodedText($cipherKey): $decodedText"

assert  plainText == decodedText

```


{{out}}

```txt
plainText: The Quick Brown Fox jumped over the lazy dog
cypherText(12): Ftq Cguow Ndaiz Raj vgybqp ahqd ftq xmlk pas
decodedText(12): The Quick Brown Fox jumped over the lazy dog
```



## Haskell


```haskell
module Caesar (caesar, uncaesar) where

import Data.Char

caesar, uncaesar :: (Integral a) => a -> String -> String
caesar k = map f
    where f c = case generalCategory c of
              LowercaseLetter -> addChar 'a' k c
              UppercaseLetter -> addChar 'A' k c
              _               -> c
uncaesar k = caesar (-k)

addChar :: (Integral a) => Char -> a -> Char -> Char
addChar b o c = chr $ fromIntegral (b' + (c' - b' + o) `mod` 26)
    where b' = fromIntegral $ ord b
          c' = fromIntegral $ ord c

```


And trying it out in GHCi:

```txt

*Main> caesar 1 "hal"
"ibm"
*Main> unCaesar 1 "ibm"
"hal"

```


Similarly, but allowing for negative cipher keys, and using isAlpha, isUpper, negate:


```haskell
import Data.Char (ord, chr, isUpper, isAlpha)
import Data.Bool (bool)

caesar, uncaesar :: Int -> String -> String
caesar = fmap . tr

uncaesar = caesar . negate

tr :: Int -> Char -> Char
tr offset c
  | isAlpha c =
    let i = ord $ bool 'a' 'A' (isUpper c)
    in chr $ i + mod ((ord c - i) + offset) 26
  | otherwise = c

main :: IO ()
main = mapM_ print [encoded, decoded]
  where
    encoded = caesar (-114) "Curio, Cesare venne, e vide e vinse ? "
    decoded = uncaesar (-114) encoded
```

{{Out}}

```txt
"Skhye, Suiqhu luddu, u lytu u lydiu ? "
"Curio, Cesare venne, e vide e vinse ? "
```


Or with proper error handling:

```haskell
{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Error (tryRead, tryAt)
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Except (ExceptT, runExceptT)

import Data.Char
import System.Exit (die)
import System.Environment (getArgs)

main :: IO ()
main = runExceptT parseKey >>= \case
           Left err -> die err
           Right k  -> interact $ caesar k

parseKey :: (Read a, Integral a) => ExceptT String IO a
parseKey = liftIO getArgs >>=
           flip (tryAt "Not enough arguments") 0 >>=
           tryRead "Key is not a valid integer"

caesar :: (Integral a) => a -> String -> String
caesar k = map f
    where f c = case generalCategory c of
              LowercaseLetter -> addChar 'a' k c
              UppercaseLetter -> addChar 'A' k c
              _               -> c

addChar :: (Integral a) => Char -> a -> Char -> Char
addChar b o c = chr $ fromIntegral (b' + (c' - b' + o) `mod` 26)
    where b' = fromIntegral $ ord b
          c' = fromIntegral $ ord c
```


=={{header|Icon}} and {{header|Unicon}}==
Strictly speaking a Ceasar Cipher is a shift of 3 (the default in this case).

```Icon
procedure main()
ctext := caesar(ptext := map("The quick brown fox jumped over the lazy dog"))
dtext := caesar(ctext,,"decrypt")
write("Plain text  = ",image(ptext))
write("Encphered text = ",image(ctext))
write("Decphered text = ",image(dtext))
end

procedure caesar(text,k,mode)   #: mono-alphabetic shift cipher
/k := 3
k := (((k % *&lcase) + *&lcase) % *&lcase) + 1
case mode of {
  &null|"e"|"encrypt": return map(text,&lcase,(&lcase||&lcase)[k+:*&lcase])
  "d"|"decrypt"      : return map(text,(&lcase||&lcase)[k+:*&lcase],&lcase)
  }
end
```


{{out}}

```txt
Plain text  = "the quick brown fox jumped over the lazy dog"
Encphered text = "wkh txlfn eurzq ira mxpshg ryhu wkh odcb grj"
Decphered text = "the quick brown fox jumped over the lazy dog"
```


=={{header|IS-BASIC}}==
<lang IS-BASIC>100 PROGRAM "CaesarCi.bas"
110 STRING M$*254
120 INPUT PROMPT "String: ":M$
130 DO
140   INPUT PROMPT "Key (1-25): ":KEY
150 LOOP UNTIL KEY>0 AND KEY<26
160 PRINT "Original message:  ";M$
170 CALL ENCRYPT(M$,KEY)
180 PRINT "Encrypted message: ";M$
190 CALL DECRYPT(M$,KEY)
200 PRINT "Decrypted message: ";M$
210 DEF ENCRYPT(REF M$,KEY)
220   STRING T$*254
230   LET T$=""
240   FOR I=1 TO LEN(M$)
250     SELECT CASE M$(I)
260     CASE "A" TO "Z"
270       LET T$=T$&CHR$(65+MOD(ORD(M$(I))-65+KEY,26))
280     CASE "a" TO "z"
290       LET T$=T$&CHR$(97+MOD(ORD(M$(I))-97+KEY,26))
300     CASE ELSE
310       LET T$=T$&M$(I)
320     END SELECT
330   NEXT
340   LET M$=T$
350 END DEF
360 DEF DECRYPT(REF M$,KEY)
370   STRING T$*254
380   LET T$=""
390   FOR I=1 TO LEN(M$)
400     SELECT CASE M$(I)
410     CASE "A" TO "Z"
420       LET T$=T$&CHR$(65+MOD(ORD(M$(I))-39-KEY,26))
430     CASE "a" TO "z"
440       LET T$=T$&CHR$(97+MOD(ORD(M$(I))-71-KEY,26))
450     CASE ELSE
460       LET T$=T$&M$(I)
470     END SELECT
480   NEXT
490   LET M$=T$
500 END DEF
```



## J

If we assume that the task also requires us to leave non-alphabetic characters alone:

```j
cndx=: [: , 65 97 +/ 26 | (i.26)&+
caesar=: (cndx 0)}&a.@u:@cndx@[ {~ a.i.]
```

Example use:
```j
   2 caesar 'This simple "monoalphabetic substitution cipher" provides almost no security, ...'
Vjku ukorng "oqpqcnrjcdgvke uwduvkvwvkqp ekrjgt" rtqxkfgu cnoquv pq ugewtkva, ...
```

If we instead assume the task only requires we treat upper case characters:

```j
CAESAR=:1 :'(26|m&+)&.((26{.64}.a.)&i.)'
```

Example use:
```j
  20 CAESAR 'HI'
BC
```



## Java

{{works with|Java|1.5+}}

```java5
public class Cipher {
    public static void main(String[] args) {

        String str = "The quick brown fox Jumped over the lazy Dog";

        System.out.println( Cipher.encode( str, 12 ));
        System.out.println( Cipher.decode( Cipher.encode( str, 12), 12 ));
    }

    public static String decode(String enc, int offset) {
        return encode(enc, 26-offset);
    }

    public static String encode(String enc, int offset) {
        offset = offset % 26 + 26;
        StringBuilder encoded = new StringBuilder();
        for (char i : enc.toCharArray()) {
            if (Character.isLetter(i)) {
                if (Character.isUpperCase(i)) {
                    encoded.append((char) ('A' + (i - 'A' + offset) % 26 ));
                } else {
                    encoded.append((char) ('a' + (i - 'a' + offset) % 26 ));
                }
            } else {
                encoded.append(i);
            }
        }
        return encoded.toString();
    }
}
```

{{out}}

```txt

Ftq cguow ndaiz raj Vgybqp ahqd ftq xmlk Pas
The quick brown fox Jumped over the lazy Dog

```



## JavaScript



### ES5



```javascript
function caesar (text, shift) {
  return text.toUpperCase().replace(/[^A-Z]/g,'').replace(/./g, function(a) {
    return String.fromCharCode(65+(a.charCodeAt(0)-65+shift)%26);
  });
}

// Tests
var text = 'veni, vidi, vici';
for (var i = 0; i<26; i++) {
  console.log(i+': '+caesar(text,i));
}
```


{{output}}

```txt

0: VENIVIDIVICI
1: WFOJWJEJWJDJ
2: XGPKXKFKXKEK
3: YHQLYLGLYLFL
...

```



### ES6



```javascript
var caesar = (text, shift) => text
  .toUpperCase()
  .replace(/[^A-Z]/g, '')
  .replace(/./g, a =>
    String.fromCharCode(65 + (a.charCodeAt(0) - 65 + shift) % 26));
```



Or, allowing encoding and decoding of both lower and upper case:


```JavaScript
((key, strPlain) => {

    // Int -> String -> String
    let caesar = (k, s) => s.split('')
        .map(c => tr(
            inRange(['a', 'z'], c) ? 'a' :
            inRange(['A', 'Z'], c) ? 'A' : 0,
            k, c
        ))
        .join('');

    // Int -> String -> String
    let unCaesar = (k, s) => caesar(26 - (k % 26), s);

    // Char -> Int -> Char -> Char
    let tr = (base, offset, char) =>
        base ? (
            String.fromCharCode(
                ord(base) + (
                    ord(char) - ord(base) + offset
                ) % 26
            )
        ) : char;

    // [a, a] -> a -> b
    let inRange = ([min, max], v) => !(v < min || v > max);

    // Char -> Int
    let ord = c => c.charCodeAt(0);

    // range :: Int -> Int -> [Int]
    let range = (m, n) =>
        Array.from({
            length: Math.floor(n - m) + 1
        }, (_, i) => m + i);

    // TEST
    let strCipher = caesar(key, strPlain),
        strDecode = unCaesar(key, strCipher);

    return [strCipher, ' -> ', strDecode];

})(114, 'Curio, Cesare venne, e vide e vinse ? ');
```


{{Out}}

```txt
Mebsy, Mockbo foxxo, o fsno o fsxco ? ,  -> , Curio, Cesare venne, e vide e vinse ?
```



## Jsish

From Typescript entry.

```javascript
/* Caesar cipher, in Jsish */
"use strict";

function caesarCipher(input:string, key:number):string {
        return input.replace(/([a-z])/g,
            function(mat, p1, ofs, str) {
                return Util.fromCharCode((p1.charCodeAt(0) + key + 26 - 97) % 26 + 97);
            }).replace(/([A-Z])/g,
                function(mat, p1, ofs, str) {
                    return Util.fromCharCode((p1.charCodeAt(0) + key + 26 - 65) % 26 + 65);
                });
}

provide('caesarCipher', 1);

if (Interp.conf('unitTest')) {
    var str = 'The five boxing wizards jump quickly';
;    str;
;    'Enciphered:';
;    caesarCipher(str, 3);
;    'Enciphered then deciphered';
;    caesarCipher(caesarCipher(str, 3), -3);
}

/*
=!EXPECTSTART!=
str ==> The five boxing wizards jump quickly
'Enciphered:'
caesarCipher(str, 3) ==> Wkh ilyh eralqj zlcdugv mxps txlfnob
'Enciphered then deciphered'
caesarCipher(caesarCipher(str, 3), -3) ==> The five boxing wizards jump quickly
=!EXPECTEND!=
*/
```


{{out}}

```txt
prompt$ jsish -u caesarCipher.jsi
[PASS] caesarCipher.jsi
```



## Julia

{{works with|Julia|0.6}}


```julia
function rot(ch::Char, key::Integer)
    if key < 1 || key > 25 end
    if isalpha(ch)
        shft = ifelse(islower(ch), 'a', 'A')
        ch = (ch - shft + key) % 26 + shft
    end
    return ch
end
rot(str::AbstractString, key::Integer) = map(x -> rot(x, key), str)

msg = "The five boxing wizards jump quickly"
key = 3
invkey = 26 - 3

println("# original: $msg\n  encrypted: $(rot(msg, key))\n  decrypted: $(rot(rot(msg, key), invkey))")
```


{{out}}

```txt
# original: The five boxing wizards jump quickly
  encrypted: Wkh ilyh eralqj zlcdugv mxps txlfnob
  decrypted: The five boxing wizards jump quickly
```



## K


Assumes lowercase letters, and no punctuation.


```k

  s:"there is a tide in the affairs of men"
  caesar:{ :[" "=x; x; {x!_ci 97+!26}[y]@_ic[x]-97]}'
  caesar[s;1]
"uifsf jt b ujef jo uif bggbjst pg nfo"

```



## Kotlin


```scala
// version 1.0.5-2

object Caesar {
    fun encrypt(s: String, key: Int): String {
        val offset = key % 26
        if (offset == 0) return s
        var d: Char
        val chars = CharArray(s.length)
        for ((index, c) in s.withIndex()) {
            if (c in 'A'..'Z') {
                d = c + offset
                if (d > 'Z') d -= 26
            }
            else if (c in 'a'..'z') {
                d = c + offset
                if (d > 'z') d -= 26
            }
            else
                d = c
            chars[index] = d
        }
        return chars.joinToString("")
    }

    fun decrypt(s: String, key: Int): String {
        return encrypt(s, 26 - key)
    }
}

fun main(args: Array<String>) {
    val encoded = Caesar.encrypt("Bright vixens jump; dozy fowl quack.", 8)
    println(encoded)
    val decoded = Caesar.decrypt(encoded, 8)
    println(decoded)
}
```


{{out}}

```txt

Jzqopb dqfmva rcux; lwhg nwet yciks.
Bright vixens jump; dozy fowl quack.

```



## LabVIEW

For readability, input is in all caps.<br/>{{VI snippet}}<br/>[[File:LabVIEW_Caesar_cipher.png]]


## Liberty BASIC


```lb
key = 7

Print "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"

'Encrypt the text
Print CaesarCypher$("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz", key)

'Decrypt the text by changing the key to (26 - key)
Print CaesarCypher$(CaesarCypher$("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz", key), (26 - key))

Function CaesarCypher$(string$, key)
    If (key < 0) Or (key > 25) Then _
    CaesarCypher$ = "Key is Ouside of Bounds" : Exit Function
    For i = 1 To Len(string$)
        rotate = Asc(Mid$(string$, i, 1))
        rotate = (rotate + key)
        If Asc(Mid$(string$, i, 1)) > Asc("Z") Then
            If rotate > Asc("z") Then rotate = (Asc("a") + (rotate - Asc("z")) - 1)
        Else
            If rotate > Asc("Z") Then rotate = (Asc("A") + (rotate - Asc("Z")) - 1)
        End If
        CaesarCypher$ = (CaesarCypher$ + Chr$(rotate))
    Next i
End Function
```



## LiveCode


```LiveCode
function caesarCipher rot phrase
    local rotPhrase, lowerLetters, upperLetters
    put "abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz" into lowerLetters
    put "ABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZ" into upperLetters
    repeat for each char letter in phrase
        get charTonum(letter)
        if it >= 65 and it <= 90 then
            put char ((it + rot) - 64) of upperLetters after rotPhrase
        else if it >= 97 and it <= 122 then
            put char ((it + rot) - 96) of lowerLetters after rotPhrase
        else
            put letter after rotPhrase
        end if
    end repeat
    return rotPhrase
end caesarCipher
```



## Logo

{{trans|Common Lisp}}
{{works with|UCB Logo}}

```logo
; some useful constants
make "lower_a ascii "a
make "lower_z ascii "z
make "upper_a ascii "A
make "upper_z ascii "Z

; encipher a single character
to encipher_char :char :key
 local "code make "code ascii :char
 local "base make "base 0
 ifelse [and (:code >= :lower_a) (:code <= :lower_z)] [make "base :lower_a] [
     if [and (:code >= :upper_a) (:code <= :upper_z)] [make "base :upper_a] ]
 ifelse [:base > 0] [
   output char (:base + (modulo ( :code - :base + :key ) 26 ))
 ] [
   output :char
 ]
end

; encipher a whole string
to caesar_cipher :string :key
  output map [encipher_char ? :key] :string
end

; Demo
make "plaintext "|The five boxing wizards jump quickly|
make "key 3
make "ciphertext caesar_cipher :plaintext :key
make "recovered  caesar_cipher :ciphertext -:key

print sentence "| Original:| :plaintext
print sentence "|Encrypted:| :ciphertext
print sentence "|Recovered:| :recovered
bye
```


{{out}}

```txt
 Original: The five boxing wizards jump quickly
Encrypted: Wkh ilyh eralqj zlcdugv mxps txlfnob
Recovered: The five boxing wizards jump quickly
```



## Lua



```Lua
local function encrypt(text, key)
	return text:gsub("%a", function(t)
			local base = (t:lower() == t and string.byte('a') or string.byte('A'))

			local r = t:byte() - base
			r = r + key
			r = r%26 -- works correctly even if r is negative
			r = r + base
			return string.char(r)
		end)
end

local function decrypt(text, key)
	return encrypt(text, -key)
end

caesar = {
	encrypt = encrypt,
	decrypt = decrypt,
}

-- test
do
	local text = "ABCDEFGHIJKLMNOPQRSTUVWXYZ abcdefghijklmnopqrstuvwxyz"
	local encrypted = caesar.encrypt(text, 7)
	local decrypted = caesar.decrypt(encrypted, 7)
	print("Original text:  ", text)
	print("Encrypted text: ", encrypted)
	print("Decrypted text: ", decrypted)
end

```



'''Fast version'''

```Lua
local memo = {}

local function make_table(k)
    local t = {}
    local a, A = ('a'):byte(), ('A'):byte()

    for i = 0,25 do
        local  c = a + i
        local  C = A + i
        local rc = a + (i+k) % 26
        local RC = A + (i+k) % 26
        t[c], t[C] = rc, RC
    end

    return t
end

local function caesar(str, k, decode)
    k = (decode and -k or k) % 26

    local t = memo[k]
    if not t then
        t = make_table(k)
        memo[k] = t
    end

    local res_t = { str:byte(1,-1) }
    for i,c in ipairs(res_t) do
        res_t[i] = t[c] or c
    end
    return string.char(unpack(res_t))
end
```



## M2000 Interpreter

We use a Buffer object (is a pointer type to a block of memory), to store string, to have access using unsigned integers.


```M2000 Interpreter

a$="THIS IS MY TEXT TO ENCODE WITH CAESAR CIPHER"
Function Cipher$(a$, N) {
      If Len(a$)=0 Then Exit
      a$=Ucase$(a$)
      N=N mod 25 +1
      \\ Integer in Mem is unsigned number
      Buffer Mem as Integer*Len(a$)
      Return Mem, 0:=a$
      For i=0 to Len(a$)-1 {
            If Eval(mem, i)>=65 and Eval(mem, i)<=90 then Return Mem, i:=(Eval(mem, i)-65+N) mod 26+65
       }
       =Eval$(Mem)
}
B$=Cipher$(a$, 12)
Print B$
Print Cipher$(B$,12)


```



## Maple


```Maple

> StringTools:-Encode( "The five boxing wizards jump quickly", encoding = alpharot[3] );
                       "Wkh ilyh eralqj zlcdugv mxps txlfnob"

> StringTools:-Encode( %, encoding = alpharot[ 23 ] );
                       "The five boxing wizards jump quickly"

```

(The symbol % refers the the last (non-NULL) value computed.)

=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
cypher[mesg_String,n_Integer]:=StringReplace[mesg,Flatten[Thread[Rule[#,RotateLeft[#,n]]]&/@CharacterRange@@@{{"a","z"},{"A","Z"}}]]
```

{{out}}

```txt
cypher["The five boxing wizards jump quickly",3]
-> Wkh ilyh eralqj zlcdugv mxps txlfnob
```


=={{header|MATLAB}} / {{header|Octave}}==

```Matlab
   function s = cipherCaesar(s, key)
          s = char( mod(s - 'A' + key, 25 ) + 'A');
   end;
   function s = decipherCaesar(s, key)
          s = char( mod(s - 'A' - key, 25 ) + 'A');
   end;
```

Here is a test:

```Matlab
   decipherCaesar(cipherCaesar('ABC',4),4)
   ans = ABC
```



## Microsoft Small Basic


```Microsoft Small Basic

TextWindow.Write("Enter a 1-25 number key (-ve number to decode): ")
key = TextWindow.ReadNumber()
TextWindow.Write("Enter message: ")
message = text.ConvertToUpperCase(TextWindow.Read())
caeser = ""
For n = 1 To Text.GetLength(message)
  letter = Text.GetSubText(message,n,1)
  code = Text.GetCharacterCode(letter)
  If code = 32 Then
    newCode = 32
  Else
    newCode = code + key
    If newCode > 90 Then
      newCode = newCode - 26
    ElseIf newCode < 65 then
      newCode = newCode + 26
    EndIf
  EndIf
  codeLetter = Text.GetCharacter(newCode)
  caeser = Text.Append(caeser,codeLetter)
EndFor
TextWindow.WriteLine(message)
TextWindow.WriteLine(caeser)

```

{{out}}

```txt
Enter a 1-25 number key (-ve number to decode): 10
Enter message: HAVE A NICE DAY
HAVE A NICE DAY
RKFO K XSMO NKI
Press any key to continue...

Enter a 1-25 number key (-ve number to decode): -10
Enter message: RKFO K XSMO NKI
RKFO K XSMO NKI
HAVE A NICE DAY
Press any key to continue...

```



## MiniScript


```MiniScript
caesar = function(s, key)
    chars = s.values
    for i in chars.indexes
        c = chars[i]
        if c >= "a" and c <= "z" then chars[i] = char(97 + (code(c)-97+key)%26)
        if c >= "A" and c <= "Z" then chars[i] = char(65 + (code(c)-65+key)%26)
    end for
    return chars.join("")
end function

print caesar("Hello world!", 7)
print caesar("Olssv dvysk!", 26-7)
```


{{out}}

```txt
Olssv dvysk!
Hello world!
```




## ML

=
## mLite
=
In this implementation, the offset can be positive or negative and is wrapped around if greater than 25 or less than -25.

```ocaml
fun readfile () = readfile []
           | x = let val ln = readln ()
                 in if eof ln then
                      rev x
                    else
                      readfile ` ln :: x
                 end

local
	val lower_a = ord #"a";
	val lower_z = ord #"z";
	val upper_a = ord #"A";
	val upper_z = ord #"Z";

	fun which
			(c_upper c) = (upper_a, upper_z)
		|	_	    = (lower_a, lower_z)
		;

	fun scale
			(c, az) where (c > #1 az) = scale( (#0 az + (c - #1 az - 1)), az)
		|	(c, az) 		  = c

in
	fun encipher
			([], offset, t)                             = implode ` rev t
		|	(x :: xs, offset, t) where (c_alphabetic x) = encipher (xs, offset, (chr ` scale (ord x + offset, which x)) :: t)
		|	(x :: xs, offset, t)                        = encipher (xs, offset, x :: t)
		|	(s, offset) 				    = if (offset < 0) then
									encipher (explode s, 26 + (offset rem 26), [])
								      else
									encipher (explode s, offset rem 26, [])
end

fun default
		(false, y) = y
	|	(x,     _) = x

;
map println ` map (fn s = encipher (s,ston ` default (argv 0, "1"))) ` readfile ();

```

The string for encoding is supplied as an input stream, and the offset supplied on the command line (defaults to 1). For example

```txt
echo The cat sat on the mat | mlite -f caecip.m ~5
```

Output:

```txt
Ocz xvo nvo ji ocz hvo
```


=={{header|Modula-2}}==
{{trans|Java}}

```modula2
MODULE CaesarCipher;
FROM Conversions IMPORT IntToStr;
FROM Terminal IMPORT WriteString, WriteLn, ReadChar;

TYPE String = ARRAY[0..64] OF CHAR;

PROCEDURE Encrypt(p : String; key : CARDINAL) : String;
VAR e : String;
VAR i,t : CARDINAL;
VAR c : CHAR;
BEGIN
    FOR i:=0 TO HIGH(p) DO
        IF p[i]=0C THEN BREAK; END;

        t := ORD(p[i]);
        IF (p[i]>='A') AND (p[i]<='Z') THEN
            t := t + key;
            IF t>ORD('Z') THEN
                t := t - 26;
            END;
        ELSIF (p[i]>='a') AND (p[i]<='z') THEN
            t := t + key;
            IF t>ORD('z') THEN
                t := t - 26;
            END;
        END;
        e[i] := CHR(t);
    END;
    RETURN e;
END Encrypt;

PROCEDURE Decrypt(p : String; key : CARDINAL) : String;
VAR e : String;
VAR i,t : CARDINAL;
VAR c : CHAR;
BEGIN
    FOR i:=0 TO HIGH(p) DO
        IF p[i]=0C THEN BREAK; END;

        t := ORD(p[i]);
        IF (p[i]>='A') AND (p[i]<='Z') THEN
            t := t - key;
            IF t<ORD('A') THEN
                t := t + 26;
            END;
        ELSIF (p[i]>='a') AND (p[i]<='z') THEN
            t := t - key;
            IF t<ORD('a') THEN
                t := t + 26;
            END;
        END;
        e[i] := CHR(t);
    END;
    RETURN e;
END Decrypt;

VAR txt,enc : String;
VAR key : CARDINAL;
BEGIN
    txt := "The five boxing wizards jump quickly";
    key := 3;

    WriteString("Original:  ");
    WriteString(txt);
    WriteLn;

    enc := Encrypt(txt, key);
    WriteString("Encrypted: ");
    WriteString(enc);
    WriteLn;

    WriteString("Decrypted: ");
    WriteString(Decrypt(enc, key));
    WriteLn;

    ReadChar;
END CaesarCipher.
```


=={{header|Modula-3}}==
This implementation distinguishes between "encoding" and "encryption."
The former turns letters into numbers;
the latter turns numbers into different ("cryptic") numbers.
The distinction between "decoding" and "decryption" is similar.
It also illustrates the use of exceptions in Modula-3.


```modula3
MODULE Caesar EXPORTS Main;

IMPORT IO, IntSeq, Text;

EXCEPTION BadCharacter;
(* Used whenever message contains a non-alphabetic character. *)

PROCEDURE Encode(READONLY message: TEXT; numbers: IntSeq.T) RAISES { BadCharacter } =
(*
Converts upper or lower case letter to 0..25.
Raises a "BadCharacter" exception for non-alphabetic characters.
*)
VAR
  c: CHAR;
  v: INTEGER;
BEGIN
  FOR i := 0 TO Text.Length(message) - 1 DO
    c := Text.GetChar(message, i);
    CASE c OF
    | 'A'..'Z' => v := ORD(c) - ORD('A');
    | 'a'..'z' => v := ORD(c) - ORD('a');
    ELSE
      RAISE BadCharacter;
    END;
    numbers.addhi(v);
  END;
END Encode;

PROCEDURE Decode(READONLY numbers: IntSeq.T; VAR message: TEXT) =
(* converts numbers in 0..26 to lower case characters *)
BEGIN
  FOR i := 0 TO numbers.size() - 1 DO
    message := message & Text.FromChar(VAL(numbers.get(i) + ORD('a'), CHAR));
  END;
END Decode;

PROCEDURE Crypt(numbers: IntSeq.T; key: INTEGER) =
(*
In the Caesar cipher, encryption and decryption are really the same;
one adds the key, the other subtracts it. We can view this as adding a positive
or nevative integer; the common task is adding an integer. We call this "Crypt".
*)
BEGIN
  FOR i := 0 TO numbers.size() - 1 DO
    numbers.put(i, (numbers.get(i) + key) MOD 26);
  END;
END Crypt;

PROCEDURE Encrypt(numbers: IntSeq.T; key := 4) =
(*
Encrypts a message of numbers using the designated key.
The result is also stored in "numbers".
*)
BEGIN
  Crypt(numbers, key);
END Encrypt;

PROCEDURE Decrypt(numbers: IntSeq.T; key := 4) =
(*
Decrypts a message of numbers using the designated key.
The result is also stored in "numbers".
*)
BEGIN
  Crypt(numbers, -key);
END Decrypt;

VAR

  message := "";
  buffer := NEW(IntSeq.T).init(22); (* sequence of 22 int's *)

BEGIN
  TRY
    Encode("WhenCaesarSetOffToGaul", buffer);
  EXCEPT BadCharacter =>
    (*
      This should never occur.
      Try adding spaces to the above to see what happens.
    *)
    IO.Put("Encountered a bad character in the input; completing partial task\n");
  END;
  Encrypt(buffer);
  Decrypt(buffer);
  Decode(buffer, message);
  IO.Put(message); IO.PutChar('\n');
END Caesar.
```


{{out}}
Notice the output comes out all lower-case.

```txt

whencaesarsetofftogaul

```



## NetRexx

The cipher code in this sample is also used in the [[Rot-13#NetRexx|Rot-13 &ndash; NetRexx]] task.

```NetRexx
/* NetRexx */

options replace format comments java crossref savelog symbols nobinary

messages = [ -
  'The five boxing wizards jump quickly', -
  'Attack at dawn!', -
  'HI']
keys = [1, 2, 20, 25, 13]

loop m_ = 0 to messages.length - 1
  in = messages[m_]
  loop k_ = 0 to keys.length - 1
    say 'Caesar cipher, key:' keys[k_].right(3)
    ec = caesar_encipher(in, keys[k_])
    dc = caesar_decipher(ec, keys[k_])
    say in
    say ec
    say dc
    say
    end k_
  say 'Rot-13:'
  ec = rot13(in)
  dc = rot13(ec)
  say in
  say ec
  say dc
  say
  end m_

return

method rot13(input) public static signals IllegalArgumentException

  return caesar(input, 13, isFalse)

method caesar(input = Rexx, idx = int, caps = boolean) public static signals IllegalArgumentException

  if idx < 1 | idx > 25 then signal IllegalArgumentException()

  --      12345678901234567890123456
  itab = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
  shift = itab.length - idx
  parse itab tl +(shift) tr
  otab = tr || tl

  if caps then input = input.upper

  cipher = input.translate(itab || itab.lower, otab || otab.lower)

  return cipher

method caesar_encipher(input = Rexx, idx = int, caps = boolean) public static signals IllegalArgumentException

  return caesar(input, idx, caps)

method caesar_decipher(input = Rexx, idx = int, caps = boolean) public static signals IllegalArgumentException

  return caesar(input, int(26) - idx, isFalse)

method caesar_encipher(input = Rexx, idx = int) public static signals IllegalArgumentException

  return caesar(input, idx, isFalse)

method caesar_decipher(input = Rexx, idx = int) public static signals IllegalArgumentException

  return caesar(input, int(26) - idx, isFalse)

method caesar_encipher(input = Rexx, idx = int, opt = Rexx) public static signals IllegalArgumentException

  return caesar(input, idx, opt)

method caesar_decipher(input = Rexx, idx = int, opt = Rexx) public static signals IllegalArgumentException

  return caesar(input, int(26) - idx, opt)

method caesar(input = Rexx, idx = int, opt = Rexx) public static signals IllegalArgumentException

  if opt.upper.abbrev('U') >= 1 then caps = isTrue
  else                               caps = isFalse

  return caesar(input, idx, caps)

method caesar(input = Rexx, idx = int) public static signals IllegalArgumentException

  return caesar(input, idx, isFalse)

method isTrue public static returns boolean
  return (1 == 1)

method isFalse public static returns boolean
  return \isTrue
```


<pre style="height: 60ex; overflow: scroll;">
Caesar cipher, key:   1
The five boxing wizards jump quickly
Uif gjwf cpyjoh xjabset kvnq rvjdlmz
The five boxing wizards jump quickly

Caesar cipher, key:   2
The five boxing wizards jump quickly
Vjg hkxg dqzkpi ykbctfu lwor swkemna
The five boxing wizards jump quickly

Caesar cipher, key:  20
The five boxing wizards jump quickly
Nby zcpy vircha qctulxm dogj kocwefs
The five boxing wizards jump quickly

Caesar cipher, key:  25
The five boxing wizards jump quickly
Sgd ehud anwhmf vhyzqcr itlo pthbjkx
The five boxing wizards jump quickly

Caesar cipher, key:  13
The five boxing wizards jump quickly
Gur svir obkvat jvmneqf whzc dhvpxyl
The five boxing wizards jump quickly

Rot-13:
The five boxing wizards jump quickly
Gur svir obkvat jvmneqf whzc dhvpxyl
The five boxing wizards jump quickly

Caesar cipher, key:   1
Attack at dawn!
Buubdl bu ebxo!
Attack at dawn!

Caesar cipher, key:   2
Attack at dawn!
Cvvcem cv fcyp!
Attack at dawn!

Caesar cipher, key:  20
Attack at dawn!
Unnuwe un xuqh!
Attack at dawn!

Caesar cipher, key:  25
Attack at dawn!
Zsszbj zs czvm!
Attack at dawn!

Caesar cipher, key:  13
Attack at dawn!
Nggnpx ng qnja!
Attack at dawn!

Rot-13:
Attack at dawn!
Nggnpx ng qnja!
Attack at dawn!

Caesar cipher, key:   1
HI
IJ
HI

Caesar cipher, key:   2
HI
JK
HI

Caesar cipher, key:  20
HI
BC
HI

Caesar cipher, key:  25
HI
GH
HI

Caesar cipher, key:  13
HI
UV
HI

Rot-13:
HI
UV
HI

```



## Nim

{{trans|Python}}

```nim
import strutils

proc caesar(s: string, k: int, decode = false): string =
  var k = if decode: 26 - k else: k
  result = ""
  for i in toUpper(s):
    if ord(i) >= 65 and ord(i) <= 90:
      result.add(chr((ord(i) - 65 + k) mod 26 + 65))

let msg = "The quick brown fox jumped over the lazy dogs"
echo msg
let enc = caesar(msg, 11)
echo enc
echo caesar(enc, 11, decode = true)
```


=={{header|Oberon-2}}==
Works with oo2c version2

```oberon2

MODULE Caesar;
IMPORT
  Out;
CONST
  encode* = 1;
  decode* = -1;

VAR
  text,cipher: POINTER TO ARRAY OF CHAR;

  PROCEDURE Cipher*(txt: ARRAY OF CHAR; key: INTEGER; op: INTEGER; VAR cipher: ARRAY OF CHAR);
  VAR
    i: LONGINT;
  BEGIN
    i := 0;
    WHILE i < LEN(txt) - 1 DO
      IF (txt[i] >= 'A') & (txt[i] <= 'Z') THEN
        cipher[i] := CHR(ORD('A') + ((ORD(txt[i]) - ORD('A') + (key * op))) MOD 26)
      ELSIF (txt[i] >= 'a') & (txt[i] <= 'z') THEN
        cipher[i] := CHR(ORD('a') + ((ORD(txt[i]) - ORD('a') + (key * op))) MOD 26)
      ELSE
        cipher[i] := txt[i]
      END;
      INC(i)
    END;
    cipher[i] := 0X
  END Cipher;

BEGIN
  NEW(text,3);NEW(cipher,3);
  COPY("HI",text^);
  Out.String(text^);Out.String(" =e=> ");
  Cipher(text^,2,encode,cipher^);
  Out.String(cipher^);

  COPY(cipher^,text^);
  Cipher(text^,2,decode,cipher^);
  Out.String(" =d=> ");Out.String(cipher^);Out.Ln;

  COPY("ZA",text^);
  Out.String(text^);Out.String(" =e=> ");
  Cipher(text^,2,encode,cipher^);
  Out.String(cipher^);

  COPY(cipher^,text^);
  Cipher(text^,2,decode,cipher^);
  Out.String(" =d=> ");Out.String(cipher^);Out.Ln;

  NEW(text,37);NEW(cipher,37);
  COPY("The five boxing wizards jump quickly",text^);
  Out.String(text^);Out.String(" =e=> ");
  Cipher(text^,3,encode,cipher^);
  Out.String(cipher^);

  COPY(cipher^,text^);
  Cipher(text^,3,decode,cipher^);
  Out.String(" =d=> ");Out.String(cipher^);Out.Ln;

END Caesar.

```

Output:

```txt

HI =e=> JK =d=> HI
ZA =e=> BC =d=> ZA
The five boxing wizards jump quickly =e=> Wkh ilyh eralqj zlcdugv mxps txlfnob =d=> The five boxing wizards jump quickly

```



## Objeck


```objeck

class Caesar {
  function : native : Encode(enc : String, offset : Int) ~ String {
    offset := offset % 26 + 26;
    encoded := "";
    enc := enc->ToLower();
    each(i : enc) {
      c := enc->Get(i);
      if(c->IsChar()) {
        j := (c - 'a' + offset) % 26;
        encoded->Append(j + 'a');
      }
      else {
        encoded->Append(c);
      };
    };

    return encoded;
  }

  function : Decode(enc : String, offset : Int) ~ String {
    return Encode(enc, offset * -1);
  }

  function : Main(args : String[]) ~ Nil {
    enc := Encode("The quick brown fox Jumped over the lazy Dog", 12);
    enc->PrintLine();
    Decode(enc, 12)->PrintLine();
  }
}

```

{{out}}

```txt

ftq cguow ndaiz raj vgybqp ahqd ftq xmlk pas
the quick brown fox jumped over the lazy dog

```



## OCaml


```ocaml
let islower c =
  c >= 'a' && c <= 'z'

let isupper c =
  c >= 'A' && c <= 'Z'

let rot x str =
  let upchars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  and lowchars = "abcdefghijklmnopqrstuvwxyz" in
  let rec decal x =
    if x < 0 then decal (x + 26) else x
  in
  let x = (decal x) mod 26 in
  let decal_up = x - (int_of_char 'A')
  and decal_low = x - (int_of_char 'a') in
  String.map (fun c ->
    if islower c then
      let j = ((int_of_char c) + decal_low) mod 26 in
      lowchars.[j]
    else if isupper c then
      let j = ((int_of_char c) + decal_up) mod 26 in
      upchars.[j]
    else
      c
  ) str
```


```ocaml
let () =
  let key = 3 in
  let orig = "The five boxing wizards jump quickly" in
  let enciphered = rot key orig in
  print_endline enciphered;
  let deciphered = rot (- key) enciphered in
  print_endline deciphered;
  Printf.printf "equal: %b\n" (orig = deciphered)
;;
```

{{out}}

```txt
$ ocaml caesar.ml
Wkh ilyh eralqj zlcdugv mxps txlfnob
The five boxing wizards jump quickly
equal: true
```



## Oforth



```Oforth
: ceasar(c, key)
   c dup isLetter ifFalse: [ return ]
   isUpper ifTrue: [ 'A' ] else: [ 'a' ] c key + over - 26 mod + ;

: cipherE(s, key)  s map(#[ key ceasar ]) charsAsString ;
: cipherD(s, key)  cipherE(s, 26 key - ) ;
```


{{out}}

```txt

>cipherE("Pack my box with five dozen liquor jugs.", 5) println
Ufhp rd gtc bnym knaj itejs qnvztw ozlx.
ok
>cipherD("Ufhp rd gtc bnym knaj itejs qnvztw ozlx.", 5) println
Pack my box with five dozen liquor jugs.

```



## OOC


```ooc
main: func (args: String[]) {
        shift := args[1] toInt()
        if (args length != 3) {
                "Usage: #{args[0]} [number] [sentence]" println()
                "Incorrect number of arguments." println()
        } else if (!shift && args[1] != "0"){
                "Usage: #{args[0]} [number] [sentence]" println()
                "Number is not a valid number." println()
        } else {
                str := ""
                for (c in args[2]) {
                        if (c alpha?()) {
                                c = (c lower?() ? 'a' : 'A') + (26 + c toLower() - 'a' + shift) % 26
                        }
                        str += c
                }
                str println()
        }
}
```

{{out}}

```txt
$ ./caesar 8 "This should be a fairly original sentence."
Bpqa apwctl jm i niqztg wzqoqvit amvbmvkm.
$ ./caesar -9 "Yet another, fairly original sentence!"
Pvk refkyvi, wrzicp fizxzerc jvekvetv!
```



## PARI/GP


```parigp
enc(s,n)={
  Strchr(Vecsmall(apply(k->if(k>96&&k<123,(k+n-97)%26+97, if(k>64&&k<91, (k+n-65)%26+65, k)),
  Vec(Vecsmall(s)))))
};
dec(s,n)=enc(s,-n);
```



## Pascal


```pascal
Program CaesarCipher(output);

procedure encrypt(var message: string; key: integer);
  var
    i: integer;
  begin
    for i := 1 to length(message) do
      case message[i] of
        'A'..'Z': message[i] := chr(ord('A') + (ord(message[i]) - ord('A') + key) mod 26);
        'a'..'z': message[i] := chr(ord('a') + (ord(message[i]) - ord('a') + key) mod 26);
      end;
  end;

procedure decrypt(var message: string; key: integer);
  var
    i: integer;
  begin
    for i := 1 to length(message) do
      case message[i] of
       'A'..'Z': message[i] := chr(ord('A') + (ord(message[i]) - ord('A') - key + 26) mod 26);
       'a'..'z': message[i] := chr(ord('a') + (ord(message[i]) - ord('a') - key + 26) mod 26);
      end;
  end;

var
  key: integer;
  message: string;

begin
  key := 3;
  message := 'The five boxing wizards jump quickly';
  writeln ('Original message: ', message);
  encrypt(message, key);
  writeln ('Encrypted message: ', message);
  decrypt(message, key);
  writeln ('Decrypted message: ', message);
end.
```

{{out}}

```txt
>: ./CaesarCipher
Original message: The five boxing wizards jump quickly
Encrypted message: Wkh ilyh eralqj zlcdugv mxps txlfnob
Decrypted message: The five boxing wizards jump quickly
>:
```



## Perl



```Perl
sub caesar {
        my ($message, $key, $decode) = @_;
        $key = 26 - $key if $decode;
        $message =~ s/([A-Z])/chr(((ord(uc $1) - 65 + $key) % 26) + 65)/geir;
}

my $msg = 'THE FIVE BOXING WIZARDS JUMP QUICKLY';
my $enc = caesar($msg, 10);
my $dec = caesar($enc, 10, 'decode');

print "msg: $msg\nenc: $enc\ndec: $dec\n";

```


{{out}}

```txt
msg: THE FIVE BOXING WIZARDS JUMP QUICKLY
enc: DRO PSFO LYHSXQ GSJKBNC TEWZ AESMUVI
dec: THE FIVE BOXING WIZARDS JUMP QUICKLY
```



## Perl 6

{{works with|Rakudo|2015.12}}

```perl6
my @alpha = 'A' .. 'Z';
sub encrypt ( $key where 1..25, $plaintext ) {
    $plaintext.trans( @alpha Z=> @alpha.rotate($key) );
}
sub decrypt ( $key where 1..25, $cyphertext ) {
    $cyphertext.trans( @alpha.rotate($key) Z=> @alpha );
}

my $original = 'THE FIVE BOXING WIZARDS JUMP QUICKLY';
my $en = encrypt( 13, $original );
my $de = decrypt( 13, $en );

.say for $original, $en, $de;

say 'OK' if $original eq all( map { .&decrypt(.&encrypt($original)) }, 1..25 );
```

{{out}}

```txt
THE FIVE BOXING WIZARDS JUMP QUICKLY
GUR SVIR OBKVAT JVMNEQF WHZC DHVPXYL
THE FIVE BOXING WIZARDS JUMP QUICKLY
OK
```



## Phix


```Phix
sequence alpha_b = repeat(0,255)
         alpha_b['A'..'Z'] = 'A'
         alpha_b['a'..'z'] = 'a'

function caesar(string s, integer key)
integer ch, base
    for i=1 to length(s) do
        ch = s[i]
        base = alpha_b[ch]
        if base then
            s[i] = base+remainder(ch-base+key,26)
        end if
    end for
    return s
end function
string s = "One fine day in the middle of the night, two dead men got up to fight. \n"&
           "Back to back they faced each other, drew their swords and shot each other. %^&*()[",
       e = caesar(s,5),
       r = caesar(e,26-5)  ?e ?r
```

{{out}}

```txt

Tsj knsj ifd ns ymj rniiqj tk ymj snlmy, ybt ijfi rjs lty zu yt knlmy.
Gfhp yt gfhp ymjd kfhji jfhm tymjw, iwjb ymjnw xbtwix fsi xmty jfhm tymjw. %^&*()[
One fine day in the middle of the night, two dead men got up to fight.
Back to back they faced each other, drew their swords and shot each other. %^&*()[

```



## PHP


```php
<?php
function caesarEncode( $message, $key ){
    $plaintext = strtolower( $message );
    $ciphertext = "";
    $ascii_a = ord( 'a' );
    $ascii_z = ord( 'z' );
    while( strlen( $plaintext ) ){
        $char = ord( $plaintext );
        if( $char >= $ascii_a && $char <= $ascii_z ){
            $char = ( ( $key + $char - $ascii_a ) % 26 ) + $ascii_a;
        }
        $plaintext = substr( $plaintext, 1 );
        $ciphertext .= chr( $char );
    }
    return $ciphertext;
}

echo caesarEncode( "The quick brown fox Jumped over the lazy Dog", 12 ), "\n";
?>
```

{{out}}

```txt
ftq cguow ndaiz raj vgybqp ahqd ftq xmlk pas
```



## Picat


```Picat

main =>
  S = "All human beings are born free and equal in dignity and rights.",
  println(S),
  println(caesar(S,5)),
  println(caesar(caesar(S,5),-5)),
  nl.

caesar(String, N) = Cipher =>
  Lower = [chr(I): I in 97..122],
  Upper = [chr(I): I in 65..90],
  M = create_map(Lower, Upper, N),
  % If a char is not in a..zA..z then show it as it is.
  Cipher := [M.get(C,C) : C in String].

create_map(Lower,Upper, N) = M =>
  M = new_map(),
  Len = Lower.length,
  foreach(I in 1..Len)
    II = (N+I) mod Len,
    if II == 0 then II := Len end, % Adjust for 1 based
    M.put(Upper[I],Upper[II]),
    M.put(Lower[I],Lower[II])
  end.

```

Test:

```txt

Picat> main
All human beings are born free and equal in dignity and rights.
Fqq mzrfs gjnslx fwj gtws kwjj fsi jvzfq ns inlsnyd fsi wnlmyx.
All human beings are born free and equal in dignity and rights.

```



## PicoLisp


```PicoLisp
(setq *Letters (apply circ (mapcar char (range 65 90))))

(de caesar (Str Key)
   (pack
      (mapcar '((C) (cadr (nth (member C *Letters) Key)))
         (chop (uppc Str)) ) ) )
```

Test:

```txt
: (caesar "IBM" 25)
-> "HAL"
: (caesar @ 1)
-> "IBM"

: (caesar "The quick brown fox jumped over the lazy dog's back" 7)
-> "AOLXBPJRIYVDUMVEQBTWLKVCLYAOLSHGFKVNZIHJR"
: (caesar @ (- 26 7))
-> "THEQUICKBROWNFOXJUMPEDOVERTHELAZYDOGSBACK"
```



## PL/I


```pli
caesar: procedure options (main);
   declare cypher_string character (52) static initial
      ((2)'ABCDEFGHIJKLMNOPQRSTUVWXYZ');
   declare (text, encyphered_text) character (100) varying,
      offset fixed binary;

   get edit (text) (L); /* Read in one line of text */
   get list (offset);
   if offset < 1 | offset > 25 then signal error;
   put skip list ('Plain text=', text);

   encyphered_text = translate(text, substr(cypher_string, offset+1, 26),
      'ABCDEFGHIJKLMNOPQRSTUVWXYZ' );
   put skip list ('Encyphered text=', encyphered_text);

   text = translate(encyphered_text, substr(cypher_string, 27-offset, 26),
      'ABCDEFGHIJKLMNOPQRSTUVWXYZ' );
   put skip list ('Decyphered text=', text);

end caesar;
```

{{out}} with offset of 5:

```txt

Plain text=             THEQUICKBROWNFOXJUMPSOVERTHELAZYDOG
Encyphered text=        YMJVZNHPGWTBSKTCOZRUXTAJWYMJQFEDITL
Decyphered text=        THEQUICKBROWNFOXJUMPSOVERTHELAZYDOG

```



## PowerShell


```Powershell
# Author: M. McNabb
function Get-CaesarCipher
{
Param
(
[Parameter(
Mandatory=$true,ValueFromPipeline=$true)]
[string]
$Text,

[ValidateRange(1,25)]
[int]
$Key = 1,

[switch]
$Decode
)

begin
{
    $LowerAlpha = [char]'a'..[char]'z'
    $UpperAlpha = [char]'A'..[char]'Z'
}

process
{
    $Chars = $Text.ToCharArray()

    function encode
    {
        param
        (
        $Char,
        $Alpha = [char]'a'..[char]'z'
        )
        $Index = $Alpha.IndexOf([int]$Char)
        $NewIndex = ($Index + $Key) - $Alpha.Length
        $Alpha[$NewIndex]
    }

    function decode
    {
        param
        (
        $Char,
        $Alpha = [char]'a'..[char]'z'
        )
        $Index = $Alpha.IndexOf([int]$Char)
        $int = $Index - $Key
        if ($int -lt 0) {$NewIndex = $int + $Alpha.Length}
        else {$NewIndex = $int}
        $Alpha[$NewIndex]
    }

    foreach ($Char in $Chars)
    {
        if ([int]$Char -in $LowerAlpha)
        {
            if ($Decode) {$Char = decode $Char}
            else {$Char = encode $Char}
        }
        elseif ([int]$Char -in $UpperAlpha)
        {
            if ($Decode) {$Char = decode $Char $UpperAlpha}
            else {$Char = encode $Char $UpperAlpha}
        }

        $Char = [char]$Char
        [string]$OutText += $Char
    }

    $OutText
    $OutText = $null
}
}
```

Usage examples:

```txt

Encode:
PS C:\> 'Pack my box with five dozen liquor jugs.' | Get-CaesarCipher -key 3
Sdfn pb era zlwk ilyh grchq oltxru mxjv.

Decode:
PS C:\> 'Sdfn pb era zlwk ilyh grchq oltxru mxjv.' | Get-CaesarCipher -key 3 -Decode
Pack my box with five dozen liquor jugs.

Encode lines of text from a file:
PS C:\> Get-Content C:\Text.txt | Get-CaesarCipher -key 10
Vsxo yxo.
Vsxo dgy!
Vsxo drboo;

```



## Prolog

{{Works with|SWI-Prolog}}
{{libheader|clpfd}}

```Prolog
:- use_module(library(clpfd)).

caesar :-
	L1 = "The five boxing wizards jump quickly",
	writef("Original : %s\n", [L1]),

	% encryption of the sentence
	encoding(3, L1, L2) ,
	writef("Encoding : %s\n", [L2]),

	% deciphering on the encoded sentence
	encoding(3, L3, L2),
	writef("Decoding : %s\n", [L3]).

% encoding/decoding of a sentence
encoding(Key, L1, L2) :-
	maplist(caesar_cipher(Key), L1, L2).

caesar_cipher(_, 32, 32) :- !.

caesar_cipher(Key, V1, V2) :-
	V #= Key + V1,

	% we verify that we are in the limits of A-Z and a-z.
	((V1 #=< 0'Z #/\ V #> 0'Z) #\/ (V1 #=< 0'z #/\ V #> 0'z)
	#\/
	(V1 #< 0'A #/\ V2 #>= 0'A)#\/ (V1 #< 0'a #/\ V2 #>= 0'a)) #==> A,

	% if we are not in these limits A is 1, otherwise 0.
	V2 #= V - A * 26,

	% compute values of V1 and V2
	label([A, V1, V2]).
```

{{out}}

```txt
 ?- caesar.
Original : The five boxing wizards jump quickly
Encoding : Wkh ilyh eralqj zlcdugv mxps txlfnob
Decoding : The five boxing wizards jump quickly
true .

```



## PureBasic

The case is maintained for alphabetic characters (uppercase/lowercase input = uppercase/lowercase output) while non-alphabetic characters, if present are included and left unchanged in the result.

```PureBasic
Procedure.s CC_encrypt(plainText.s, key, reverse = 0)
  ;if reverse <> 0 then reverse the encryption (decrypt)
  If reverse: reverse = 26: key = 26 - key: EndIf

  Static alphabet$ = "ABCEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz"

  Protected result.s, i, length = Len(plainText), letter.s, legal
  If key < 1 Or key > 25: ProcedureReturn: EndIf  ;keep key in range

  For i = 1 To length
    letter = Mid(plainText, i, 1)
    legal = FindString(alphabet$, letter, 1 + reverse)
    If legal
      result + Mid(alphabet$, legal + key, 1)
    Else
      result + letter
    EndIf
  Next
  ProcedureReturn result
EndProcedure

Procedure.s CC_decrypt(cypherText.s, key)
  ProcedureReturn CC_encrypt(cypherText, key, 1)
EndProcedure

If OpenConsole()
  Define key, plainText.s, encryptedText.s, decryptedText.s

  key = Random(24) + 1 ;get a random key in the range 1 -> 25

  plainText = "The quick brown fox jumped over the lazy dogs.": PrintN(RSet("Plain text = ", 17) + #DQUOTE$ + plainText + #DQUOTE$)
  encryptedText = CC_encrypt(plainText, key): PrintN(RSet("Encrypted text = ", 17) + #DQUOTE$ + encryptedText + #DQUOTE$)
  decryptedText = CC_decrypt(encryptedText, key): PrintN(RSet("Decrypted text = ", 17) + #DQUOTE$ + decryptedText + #DQUOTE$)

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
```

{{out}}

```txt
    Plain text = "The quick brown fox jumped over the lazy dogs."
Encrypted text = "Znk waoiq hxuct lud pasvkj ubkx znk rgfe jumy."
Decrypted text = "the quick brown fox jumped over the lazy dogs."
```


### Alternate solution

Here is an alternate and more advanced form of the encrypt procedure.  It improves on the simple version in terms of speed, in case Caesar is using the cipher on some very long documents.  It is meant to replace the encrypt procedure in the previous code and produces identical results.

```PureBasic
Procedure.s CC_encrypt(text.s, key, reverse = 0)
  ;if reverse <> 0 then reverse the encryption (decrypt)
  Protected i, *letter.Character, *resultLetter.Character, result.s = Space(Len(text))

  If reverse: key = 26 - key: EndIf
  If key < 1 Or key > 25: ProcedureReturn: EndIf  ;exit if key out of range

  *letter = @text: *resultLetter = @result
  While *letter\c
    Select *letter\c
      Case 'A' To 'Z'
        *resultLetter\c = ((*letter\c - 65 + key) % 26) + 65
      Case 'a' To 'z'
        *resultLetter\c = ((*letter\c - 97 + key) % 26) + 97
      Default
        *resultLetter\c = *letter\c
    EndSelect
    *letter + SizeOf(Character): *resultLetter + SizeOf(Character)
  Wend
  ProcedureReturn result
EndProcedure
```



## Python


```Python
def caesar(s, k, decode = False):
	if decode: k = 26 - k
	return "".join([chr((ord(i) - 65 + k) % 26 + 65)
				for i in s.upper()
				if ord(i) >= 65 and ord(i) <= 90 ])

msg = "The quick brown fox jumped over the lazy dogs"
print msg
enc = caesar(msg, 11)
print enc
print caesar(enc, 11, decode = True)
```

{{out}}

```txt
The quick brown fox jumped over the lazy dogs
ESPBFTNVMCZHYQZIUFXAPOZGPCESPWLKJOZRD
THEQUICKBROWNFOXJUMPEDOVERTHELAZYDOGS
```

Alternate solution
{{works with|Python|2.x}} (for 3.x change <code>string.maketrans</code> to <code>str.maketrans</code>)

```python
import string
def caesar(s, k, decode = False):
   if decode: k = 26 - k
   return s.translate(
       string.maketrans(
           string.ascii_uppercase + string.ascii_lowercase,
           string.ascii_uppercase[k:] + string.ascii_uppercase[:k] +
           string.ascii_lowercase[k:] + string.ascii_lowercase[:k]
           )
       )
msg = "The quick brown fox jumped over the lazy dogs"
print msg
enc = caesar(msg, 11)
print enc
print caesar(enc, 11, decode = True)
```

{{out}}

```txt

The quick brown fox jumped over the lazy dogs
Esp bftnv mczhy qzi ufxapo zgpc esp wlkj ozrd
The quick brown fox jumped over the lazy dogs

```


Variant with memoization of translation tables
{{works with|Python|3.x}}

```python
import string
def caesar(s, k = 13, decode = False, *, memo={}):
  if decode: k = 26 - k
  k = k % 26
  table = memo.get(k)
  if table is None:
    table = memo[k] = str.maketrans(
                        string.ascii_uppercase + string.ascii_lowercase,
                        string.ascii_uppercase[k:] + string.ascii_uppercase[:k] +
                        string.ascii_lowercase[k:] + string.ascii_lowercase[:k])
  return s.translate(table)
```


A compact alternative solution

```python

from string import ascii_uppercase as abc

def caesar(s, k, decode = False):
    trans = dict(zip(abc, abc[(k,26-k)[decode]:] + abc[:(k,26-k)[decode]]))
    return ''.join(trans[L] for L in s.upper() if L in abc)

msg = "The quick brown fox jumped over the lazy dogs"
print(caesar(msg, 11))
print(caesar(caesar(msg, 11), 11, True))

```

{{out}}

```txt

ESPBFTNVMCZHYQZIUFXAPOZGPCESPWLKJOZRD
THEQUICKBROWNFOXJUMPEDOVERTHELAZYDOGS

```



## R

This is a generalization of the Rot-13 solution for R at: http://rosettacode.org/wiki/Rot-13#R .

```R

# based on Rot-13 solution: http://rosettacode.org/wiki/Rot-13#R
ceasar <- function(x, key)
{
  # if key is negative, wrap to be positive
  if (key < 0) {
    key <- 26 + key
  }

  old <- paste(letters, LETTERS, collapse="", sep="")
  new <- paste(substr(old, key * 2 + 1, 52), substr(old, 1, key * 2), sep="")
  chartr(old, new, x)
}

# simple examples from description
print(ceasar("hi",2))
print(ceasar("hi",20))

# more advanced example
key <- 3
plaintext <- "The five boxing wizards jump quickly."
cyphertext <- ceasar(plaintext, key)
decrypted <- ceasar(cyphertext, -key)

print(paste("    Plain Text: ", plaintext, sep=""))
print(paste("   Cypher Text: ", cyphertext, sep=""))
print(paste("Decrypted Text: ", decrypted, sep=""))



```

{{out}}

```txt

> print(ceasar("hi",2))
[1] "jk"
> print(ceasar("hi",20))
[1] "bc"
> print(paste("Plain Text: ", plaintext, sep=""))
[1] "Plain Text: The five boxing wizards jump quickly."
> print(paste("Cypher Text: ", cyphertext, sep=""))
[1] "Cypher Text: Wkh ilyh eralqj zlcdugv mxps txlfnob."
> print(paste("Decrypted Text: ", decrypted, sep=""))
[1] "Decrypted Text: The five boxing wizards jump quickly."

```



## Racket


```racket

#lang racket

(define A (char->integer #\A))
(define Z (char->integer #\Z))
(define a (char->integer #\a))
(define z (char->integer #\z))

(define (rotate c n)
  (define cnum (char->integer c))
  (define (shift base) (integer->char (+ base (modulo (+ n (- cnum base)) 26))))
  (cond [(<= A cnum Z) (shift A)]
        [(<= a cnum z) (shift a)]
        [else c]))

(define (caesar s n)
  (list->string (for/list ([c (in-string s)]) (rotate c n))))

(define (encrypt s) (caesar s 1))
(define (decrypt s) (caesar s -1))

```

Example:

```txt

> (define s (encrypt "The five boxing wizards jump quickly."))
> s
"Uif gjwf cpyjoh xjabset kvnq rvjdlmz."
> (decrypt s)
"The five boxing wizards jump quickly."

```



## Retro

Retro provides a number of classical cyphers in the '''crypto'''' library. This implementation is from the library.

```Retro
{{
  variable offset
  : rotate  ( cb-c ) tuck - @offset + 26 mod + ;
  : rotate? (  c-c )
    dup 'a 'z within [ 'a rotate ] ifTrue
    dup 'A 'Z within [ 'A rotate ] ifTrue ;
---reveal---
  : ceaser  (  $n-$ )
    !offset dup [ [ @ rotate? ] sip ! ] ^types'STRING each@ ;
}}

( Example )
"THEYBROKEOURCIPHEREVERYONECANREADTHIS" 3 ceaser   ( returns encrypted string )
23 ceaser   ( returns decrypted string )
```



## REXX


### only Latin letters

This version conforms to the task's restrictions.

```rexx
/*REXX program supports the  Caesar cypher for the Latin alphabet only,  no punctuation */
/*──────────── or blanks allowed,  all lowercase Latin letters are treated as uppercase.*/
parse arg key .;  arg . p                        /*get key & uppercased text to be used.*/
p=space(p,0)                                     /*elide any and all spaces (blanks).   */
                    say 'Caesar cypher key:' key /*echo the Caesar cypher key to console*/
                    say '       plain text:' p   /*  "   "       plain text    "    "   */
y=Caesar(p, key);   say '         cyphered:' y   /*  "   "    cyphered text    "    "   */
z=Caesar(y,-key);   say '       uncyphered:' z   /*  "   "  uncyphered text    "    "   */
if z\==p  then say  "plain text doesn't match uncyphered cyphered text."
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
Caesar: procedure; arg s,k;  @='ABCDEFGHIJKLMNOPQRSTUVWXYZ'
        ak=abs(k)                                /*obtain the absolute value of the key.*/
        L=length(@)                              /*obtain the length of the  @  string. */
        if ak>length(@)-1 | k==0  then  call err k  'key is invalid.'
        _=verify(s,@)                            /*any illegal characters specified ?   */
        if _\==0  then call err 'unsupported character:'   substr(s, _, 1)
        if k>0    then ky=k+1                    /*either cypher it,  or ···            */
                  else ky=L+1-ak                 /*     decypher it.                    */
        return translate(s, substr(@||@,ky,L),@) /*return the processed text.           */
/*──────────────────────────────────────────────────────────────────────────────────────*/
err:    say;      say '***error***';       say;          say arg(1);      say;     exit 13
```

'''output'''   when using the input of:


22 The definition of a trivial program is one that has no bugs

```txt

Caesar cypher key: 22
       plain text: THEDEFINITIONOFATRIVIALPROGRAMISONETHATHASNOBUGS
         cyphered: PDAZABEJEPEKJKBWPNEREWHLNKCNWIEOKJAPDWPDWOJKXQCO
       uncyphered: THEDEFINITIONOFATRIVIALPROGRAMISONETHATHASNOBUGS

```



### most characters

This version allows upper and lowercase Latin alphabet as well as all the
characters on the standard (computer) keyboard including blanks.

```rexx
/*REXX program supports the Caesar cypher for most keyboard characters including blanks.*/
parse arg key p                                  /*get key and the text to be cyphered. */
                    say 'Caesar cypher key:' key /*echo the Caesar cypher key to console*/
                    say '       plain text:' p   /*  "   "       plain text    "    "   */
y=Caesar(p, key);   say '         cyphered:' y   /*  "   "    cyphered text    "    "   */
z=Caesar(y,-key);   say '       uncyphered:' z   /*  "   "  uncyphered text    "    "   */
if z\==p  then say  "plain text doesn't match uncyphered cyphered text."
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
Caesar: procedure;     parse arg s,k;     @= 'abcdefghijklmnopqrstuvwxyz'
        @=translate(@)@"0123456789(){}[]<>'"     /*add uppercase, digitss, group symbols*/
        @=@'~!@#$%^&*_+:";?,./`-= '              /*also add other characters to the list*/
        L=length(@)                              /*obtain the length of the  @  string. */
        ak=abs(k)                                /*obtain the absolute value of the key.*/
        if ak>length(@)-1 | k==0  then  call err k  'key is invalid.'
        _=verify(s,@)                            /*any illegal characters specified ?   */
        if _\==0  then call err 'unsupported character:'   substr(s, _, 1)
        if k>0    then ky=k+1                    /*either cypher it,  or ···            */
                  else ky=L+1-ak                 /*     decypher it.                    */
        return translate(s, substr(@ || @, ky, L),  @)
/*──────────────────────────────────────────────────────────────────────────────────────*/
err:    say;      say '***error***';       say;          say arg(1);      say;     exit 13
```

'''output'''   when using the input of:


31 Batman's hood is called a "cowl" (old meaning).

```txt

Caesar cypher key: 31
       plain text: Batman's hood is called a "cowl" (old meaning).
         cyphered: g5^>5~e%d{!!8d}%d75<<98d5dU7!_<UdA!<8d>95~}~)BY
       uncyphered: Batman's hood is called a "cowl" (old meaning).

```



## Ring


```ring

# Project : Caesar cipher

cipher = "pack my box with five dozen liquor jugs"
abc = "abcdefghijklmnopqrstuvwxyz"
see "text is to be encrypted:" + nl
see cipher+ nl + nl
str = ""
key = random(24) + 1
see "key = " + key + nl + nl
see "encrypted:" + nl
caesarencode(cipher, key)
see str + nl + nl
cipher = str
see "decrypted again:" + nl
caesardecode(cipher, key)
see str + nl

func caesarencode(cipher, key)
       str = ""
       for n = 1 to len(cipher)
            if cipher[n] != " "
               pos = substr(abc, cipher[n])
               if pos + key < len(abc)
                  str = str + abc[pos + key]
               else
                  if (pos+key)-len(abc) != 0
                     str = str + abc[(pos+key)%len(abc)]
                  else
                      str = str +abc[key+pos]
                  ok
                    ok
               else
                  str = str + " "
               ok
       next
     return str

func caesardecode(cipher, key)
       str = ""
       for n= 1 to len(cipher)
            if cipher[n] != " "
               pos = substr(abc, cipher[n])
               if (pos - key) > 0 and pos != key
                   str = str + abc[pos - key]
                   loop
               else
                   if pos = key
                      str = str + char(122)
                   else
                     str = str + abc[len(abc)-(key-pos)]
                   ok
               ok
            else
               str = str + " "
            ok
       next
       return str

```

Output:

```txt

text is to be encrypted:
pack my box with five dozen liquor jugs

key = 9

encrypted:
yjlt vh kxg frcq oren mxinw urzdxa sdpb

decrypted again:
pack my box with five dozen liquor jugs

```



## Ruby


```ruby
class String
  ALFABET = ("A".."Z").to_a

  def caesar_cipher(num)
    self.tr(ALFABET.join, ALFABET.rotate(num).join)
  end

end

#demo:
encypted  = "THEYBROKEOURCIPHEREVERYONECANREADTHIS".caesar_cipher(3)
decrypted = encypted.caesar_cipher(-3)

```



## Run BASIC


```runbasic
input "Gimme a ofset:";ofst       ' set any offset you like

a$ = "Pack my box with five dozen liquor jugs"
print " Original: ";a$
a$ = cipher$(a$,ofst)
print "Encrypted: ";a$
print "Decrypted: ";cipher$(a$,ofst+6)

FUNCTION cipher$(a$,ofst)
for i = 1 to len(a$)
  aa$   = mid$(a$,i,1)
  code$ = " "
  if aa$ <> " " then
    ua$   = upper$(aa$)
    a     = asc(ua$) - 64
    code$ = chr$((((a mod 26) + ofst) mod 26) + 65)
    if ua$ <> aa$ then code$ = lower$(code$)
  end if
  cipher$ = cipher$;code$
next i
END FUNCTION
```

{{out}}

```txt
Gimme a ofset:?9
 Original: Pack my box with five dozen liquor jugs
Encrypted: Zkmu wi lyh gsdr psfo nyjox vsaeyb teqc
Decrypted: Pack my box with five dozen liquor jugs
```



## Rust

This example shows proper error handling. It skips non-ASCII characters.

```rust
use std::io::{self, Write};
use std::fmt::Display;
use std::{env, process};

fn main() {
    let shift: u8 = env::args().nth(1)
        .unwrap_or_else(|| exit_err("No shift provided", 2))
        .parse()
        .unwrap_or_else(|e| exit_err(e, 3));

    let plain = get_input()
        .unwrap_or_else(|e| exit_err(&e, e.raw_os_error().unwrap_or(-1)));

    let cipher = plain.chars()
        .map(|c| {
            let case = if c.is_uppercase() {'A'} else {'a'} as u8;
            if c.is_alphabetic() { (((c as u8 - case + shift) % 26) + case) as char } else { c }
        }).collect::<String>();

    println!("Cipher text: {}", cipher.trim());
}


fn get_input() -> io::Result<String> {
    print!("Plain text:  ");
    try!(io::stdout().flush());

    let mut buf = String::new();
    try!(io::stdin().read_line(&mut buf));
    Ok(buf)
}

fn exit_err<T: Display>(msg: T, code: i32) -> ! {
    let _ = writeln!(&mut io::stderr(), "ERROR: {}", msg);
    process::exit(code);
}
```



## Scala


```scala
object Caesar {
  private val alphaU='A' to 'Z'
  private val alphaL='a' to 'z'

  def encode(text:String, key:Int)=text.map{
    case c if alphaU.contains(c) => rot(alphaU, c, key)
    case c if alphaL.contains(c) => rot(alphaL, c, key)
    case c => c
  }
  def decode(text:String, key:Int)=encode(text,-key)
  private def rot(a:IndexedSeq[Char], c:Char, key:Int)=a((c-a.head+key+a.size)%a.size)
}
```


```scala
val text="The five boxing wizards jump quickly"
println("Plaintext  => " + text)
val encoded=Caesar.encode(text, 3)
println("Ciphertext => " + encoded)
println("Decrypted  => " + Caesar.decode(encoded, 3))
```

{{out}}

```txt
Plaintext  => The five boxing wizards jump quickly
Ciphertext => Wkh ilyh eralqj zlcdugv mxps txlfnob
Decrypted  => The five boxing wizards jump quickly
```



### Alternate version

This version first creates non shifted and shifted character sequences
and then encodes and decodes by indexing between those sequences.

```scala
class Caeser(val key: Int) {
  @annotation.tailrec
  private def rotate(p: Int, s: IndexedSeq[Char]): IndexedSeq[Char] = if (p < 0) rotate(s.length + p, s) else s.drop(p) ++ s.take(p)

  val uc = 'A' to 'Z'
  val lc = 'a' to 'z'
  val as = uc ++ lc
  val bs = rotate(key, uc) ++ rotate(key, lc)

  def encode(c: Char) = if (as.contains(c)) bs(as.indexOf(c)) else c
  def decode(c: Char) = if (bs.contains(c)) as(bs.indexOf(c)) else c
}
```



```scala
val text = "The five boxing wizards jump quickly"
val myCaeser = new Caeser(3)
val encoded = text.map(c => myCaeser.encode(c))
println("Plaintext => " + text)
println("Ciphertext => " + encoded)
println("Decrypted => " + encoded.map(c => myCaeser.decode(c)))
```


{{out}}

```txt
Plaintext => The five boxing wizards jump quickly
Ciphertext => Wkh ilyh eralqj zlcdugv mxps txlfnob
Decrypted => The five boxing wizards jump quickly
```



## Scheme



```scheme
;
; Works with R7RS-compatible Schemes (e.g. Chibi).
; Also current versions of Chicken, Gauche and Kawa.
;
(cond-expand
  (chicken (use srfi-13))
  (gauche  (use srfi-13))
  (kawa    (import (srfi :13)))
  (else    (import (scheme base) (scheme write)))) ; R7RS


(define msg "The quick brown fox jumps over the lazy dog.")
(define key 13)

(define (caesar char)
  (define A (char->integer #\A))
  (define Z (char->integer #\Z))
  (define a (char->integer #\a))
  (define z (char->integer #\z))
  (define c (char->integer char))
  (integer->char
    (cond ((<= A c Z) (+ A (modulo (+ key (- c A)) 26)))
          ((<= a c z) (+ a (modulo (+ key (- c a)) 26)))
          (else c)))) ; Return other characters verbatim.

(display (string-map caesar msg))
(newline)

```


{{out}}

```txt

Gur dhvpx oebja sbk whzcf bire gur ynml qbt.

```



## sed

This code is roughly equivalent to the [[Rot-13#sed|rot-13]] cypher sed implementation, except that the conversion table is parameterized by a number and that the conversion done manually, instead of using `y///' command.

```sed
#!/bin/sed -rf
# Input: <number 0..25>\ntext to encode

/^[0-9]+$/ {
	# validate a number and translate it to analog form
	s/$/;9876543210dddddddddd/
	s/([0-9]);.*\1.{10}(.?)/\2/
	s/2/11/
	s/1/dddddddddd/g
	/[3-9]|d{25}d+/ {
	s/.*/Error: Key must be <= 25/
	q
	}
	# append from-table
	s/$/\nabcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ/
	# .. and to-table
	s/$/abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ/
	# rotate to-table, lower and uppercase independently, removing one `d' at a time
	: rotate
	s/^d(.*\n[^Z]+Z)(.)(.{25})(.)(.{25})/\1\3\2\5\4/
	t rotate
	s/\n//
	h
	d
}

# use \n to mark character to convert
s/^/\n/
# append conversion table to pattern space
G
: loop
	# look up converted character and place it instead of old one
	s/\n(.)(.*\n.*\1.{51}(.))/\n\3\2/
	# advance \n even if prev. command fails, thus skip non-alphabetical characters
	/\n\n/! s/\n([^\n])/\1\n/
t loop
s/\n\n.*//
```

{{out}}

```txt

$ ./caesar.sed
3
The five boxing wizards jump quickly
Wkh ilyh eralqj zlcdugv mxps txlfnob
23
Wkh ilyh eralqj zlcdugv mxps txlfnob
The five boxing wizards jump quickly
26
Error: Key must be <= 25

```



## Seed7


```seed7
$ include "seed7_05.s7i";

const func string: rot (in string: stri, in integer: encodingKey) is func
  result
    var string: encodedStri is "";
  local
    var char: ch is ' ';
    var integer: index is 0;
  begin
    encodedStri := stri;
    for ch key index range stri do
      if ch >= 'a' and ch <= 'z' then
        ch := chr((ord(ch) - ord('a') + encodingKey) rem 26 + ord('a'));
      elsif ch >= 'A' and ch <= 'Z' then
        ch := chr((ord(ch) - ord('A') + encodingKey) rem 26 + ord('A'));
      end if;
      encodedStri @:= [index] ch;
    end for;
  end func;

const proc: main is func
  local
    const integer: exampleKey is 3;
    const string: testText is "The five boxing wizards jump quickly";
  begin
    writeln("Original:  " <& testText);
    writeln("Encrypted: " <& rot(testText, exampleKey));
    writeln("Decrypted: " <& rot(rot(testText, exampleKey), 26 - exampleKey));
  end func;
```

{{out}}

```txt

Original:  The five boxing wizards jump quickly
Encrypted: Wkh ilyh eralqj zlcdugv mxps txlfnob
Decrypted: The five boxing wizards jump quickly

```



## SequenceL

You only have to write an encrypt and decrypt function for characters. The semantics of Normalize Transpose allow those functions to be applied to strings.

```sequencel>import <Utilities/Sequence.sl
;
import <Utilities/Conversion.sl>;

lowerAlphabet := "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
upperAlphabet := "abcdefghijklmnopqrstuvwxyz";

caesarEncrypt(ch, key) :=
	let
		correctAlphabet :=
				lowerAlphabet when some(ch = lowerAlphabet)
			else
				upperAlphabet;

		index := Sequence::firstIndexOf(correctAlphabet, ch);

		newIndex := (index + key - 1) mod 26 + 1;
	in
		ch when not(some(ch = lowerAlphabet) or some(ch = upperAlphabet))
	else
		correctAlphabet[newIndex];

caesarDecrypt(ch, key) := caesarEncrypt(ch, 26 - key);

main(args(2)) :=
	let
		key := Conversion::stringToInt(args[2]);
		encrypted := caesarEncrypt(args[1], key);
		decrypted := caesarDecrypt(encrypted, key);
	in
		"Input: \t" ++ args[1] ++ "\n" ++
		"Encrypted:\t" ++ encrypted ++ "\n" ++
		"Decrypted:\t" ++ decrypted;
```

{{out}}

```txt

cmd:> main.exe "Pack my box with five dozen liquor jugs." 5
"Original: Pack my box with five dozen liquor jugs.
Encrypted: Ufhp rd gtc bnym knaj itejs qnvztw ozlx.
Decrypted: Pack my box with five dozen liquor jugs."

```



## Sidef

{{trans|Perl}}

```ruby
func caesar(msg, key, decode=false) {
    decode && (key = (26 - key));
    msg.gsub(/([A-Z])/i, {|c| ((c.uc.ord - 65 + key) % 26) + 65 -> chr});
};

var msg = 'THE FIVE BOXING WIZARDS JUMP QUICKLY';

var enc = caesar(msg, 10);
var dec = caesar(enc, 10, true);

say "msg: #{msg}";
say "enc: #{enc}";
say "dec: #{dec}";
```


{{out}}

```txt

msg: THE FIVE BOXING WIZARDS JUMP QUICKLY
enc: DRO PSFO LYHSXQ GSJKBNC TEWZ AESMUVI
dec: THE FIVE BOXING WIZARDS JUMP QUICKLY

```



## Sinclair ZX81 BASIC

Works with 1k of RAM. A negative key decodes.

```basic
 10 INPUT KEY
 20 INPUT T$
 30 LET C$=""
 40 FOR I=1 TO LEN T$
 50 LET L$=T$(I)
 60 IF L$<"A" OR L$>"Z" THEN GOTO 100
 70 LET L$=CHR$ (CODE L$+KEY)
 80 IF L$>"Z" THEN LET L$=CHR$ (CODE L$-26)
 90 IF L$<"A" THEN LET L$=CHR$ (CODE L$+26)
100 LET C$=C$+L$
110 NEXT I
120 PRINT C$
```

{{in}}

```txt
12
GALLIA EST OMNIS DIVISA IN PARTES TRES
```

{{out}}

```txt
SMXXUM QEF AYZUE PUHUEM UZ BMDFQE FDQE
```

{{in}}

```txt

-12
SMXXUM QEF AYZUE PUHUEM UZ BMDFQE FDQE
```

{{out}}

```txt
GALLIA EST OMNIS DIVISA IN PARTES TRES
```



## Smalltalk

{{works with|Smalltalk/X}}
well, I'm lucky: the standard library already contains a rot:n method!

```Smalltalk
'THE QUICK BROWN FOX' rot:3 -> 'WKH TXLFN EURZQ IRA'
```

but if it wasn't, here is an implementation for other smalltalks:

```smalltalk

!CharacterArray methodsFor:'encoding'!
rot:n
    ^ self class
        streamContents:[:aStream |
            self do:[:char |
                aStream nextPut:(char rot:n) ]]


!Character methodsFor:'encoding'!
rot:n
     (self isLetter) ifTrue:[
        self isLowercase ifTrue:[
            ^ 'abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz' at:(self-$a+1+n)
        ] ifFalse:[
            ^ 'ABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZ' at:(self-$A+1+n)
        ]
    ].
    ^ self


```



## SSEM

ASCII didn't exit in 1948, and the task specification explicitly says we only need to convert Roman capitals; so we adopt a simpler encoding, representing the letters of the alphabet from <tt>A</tt>=0 to <tt>Z</tt>=25.

The program will convert one character at a time. Load the character code into storage address 20, and the key into address 19. The machine will halt with the cyphered encoding in the accumulator. A negative (two's complement) key decodes.

This is in fact a general solution that will work equally well with alphabets of more or fewer than 26 characters: simply replace the constant 26 in storage address 18 with 22 for Hebrew, 24 for Greek, 28 for Arabic, 33 for Russian, etc.

```ssem
00101000000000100000000000000000   0. -20 to c
11001000000000010000000000000000   1. Sub. 19
10101000000001100000000000000000   2. c to 21
10101000000000100000000000000000   3. -21 to c
00000000000000110000000000000000   4. Test
10001000000000000000000000000000   5. 17 to CI
10101000000001100000000000000000   6. c to 21
10101000000000100000000000000000   7. -21 to c
01001000000000010000000000000000   8. Sub. 18
10101000000001100000000000000000   9. c to 21
10101000000000100000000000000000  10. -21 to c
00000000000001110000000000000000  11. Stop
01001000000000010000000000000000  12. Sub. 18
00000000000000110000000000000000  13. Test
00000000000001110000000000000000  14. Stop
10101000000000100000000000000000  15. -21 to c
00000000000001110000000000000000  16. Stop
11010000000000000000000000000000  17. 11
01011000000000000000000000000000  18. 26
```


## Stata



```stata
function caesar(s, k) {
	u = ascii(s)
	i = selectindex(u:>=65 :& u:<=90)
	if (length(i)>0) u[i] = mod(u[i]:+(k-65), 26):+65
	i = selectindex(u:>=97 :& u:<=122)
	if (length(i)>0) u[i] = mod(u[i]:+(k-97), 26):+97
	return(char(u))
}

caesar("layout", 20)
  fusion
```



## Swift


```swift

func usage(_ e:String) {
  print("error: \(e)")
  print("./caeser -e 19 a-secret-string")
  print("./caeser -d 19 tskxvjxlskljafz")
}

func charIsValid(_ c:Character) -> Bool {
  return c.isASCII && ( c.isLowercase || 45 == c.asciiValue ) // '-' = 45
}

func charRotate(_ c:Character, _ by:Int) -> Character {
  var cv:UInt8! = c.asciiValue
  if 45 == cv { cv = 96 }  // if '-', set it to 'a'-1
  cv += UInt8(by)
  if 122 < cv { cv -= 27 } // if larget than 'z', reduce by 27
  if 96 == cv { cv = 45 }  // restore '-'
  return Character(UnicodeScalar(cv))
}

func caesar(_ enc:Bool, _ key:Int, _ word:String) -> String {
  let r = enc ? key : 27 - key
  func charRotateWithKey(_ c:Character) -> Character {
    return charRotate(c,r)
  }
  return String(word.map(charRotateWithKey))
}

func main() {
  var encrypt = true

  if 4 != CommandLine.arguments.count {
    return usage("caesar expects exactly three arguments")
  }

  switch ( CommandLine.arguments[1] ) {
  case "-e":
    encrypt = true
  case "-d":
    encrypt = false
  default:
    return usage("first argument must be -e (encrypt) or -d (decrypt)")
  }

  guard let key = Int(CommandLine.arguments[2]) else {
    return usage("second argument not a number (must be in range 0-26)")
  }

  if key < 0 || 26 < key {
    return usage("second argument not in range 0-26")
  }

  if !CommandLine.arguments[3].allSatisfy(charIsValid) {
    return usage("third argument must only be lowercase ascii characters, or -")
  }

  let ans = caesar(encrypt,key,CommandLine.arguments[3])
  print("\(ans)")
}

func test() {
  if ( Character("a") != charRotate(Character("a"),0) ) {
    print("Test Fail 1")
  }
  if ( Character("-") != charRotate(Character("-"),0) ) {
    print("Test Fail 2")
  }
  if ( Character("-") != charRotate(Character("z"),1) ) {
    print("Test Fail 3")
  }
  if ( Character("z") != charRotate(Character("-"),26)) {
    print("Test Fail 4")
  }
  if ( "ihgmkzma" != caesar(true,8,"a-zecret") ) {
    print("Test Fail 5")
  }
  if ( "a-zecret" != caesar(false,8,"ihgmkzma") ) {
    print("Test Fail 6")
  }
}

test()
main()

```



## Tcl


```tcl
package require Tcl 8.6;    # Or TclOO package for 8.5

oo::class create Caesar {
    variable encryptMap decryptMap
    constructor shift {
	for {set i 0} {$i < 26} {incr i} {
	    # Play fast and loose with string/list duality for shorter code
	    append encryptMap [format "%c %c %c %c " \
		    [expr {$i+65}] [expr {($i+$shift)%26+65}] \
		    [expr {$i+97}] [expr {($i+$shift)%26+97}]]
	    append decryptMap [format "%c %c %c %c " \
		    [expr {$i+65}] [expr {($i-$shift)%26+65}] \
		    [expr {$i+97}] [expr {($i-$shift)%26+97}]]
	}
    }

    method encrypt text {
	string map $encryptMap $text
    }
    method decrypt text {
	string map $decryptMap $text
    }
}
```

Demonstrating:

```tcl
set caesar [Caesar new 3]
set txt "The five boxing wizards jump quickly."
set enc [$caesar encrypt $txt]
set dec [$caesar decrypt $enc]
puts "Original message  = $txt"
puts "Encrypted message = $enc"
puts "Decrypted message = $dec"
```

{{out}}

```txt

Original message  = The five boxing wizards jump quickly.
Encrypted message = Wkh ilyh eralqj zlcdugv mxps txlfnob.
Decrypted message = The five boxing wizards jump quickly.

```



## TUSCRIPT


```tuscript
$$ MODE TUSCRIPT
text="THE QUICK BROWN FOX JUMPS OVER THE LAZY DOG"
PRINT "text orginal    ",text

abc="ABCDEFGHIJKLMNOPQRSTUVWXYZ",key=3,caesarskey=key+1
secretbeg=EXTRACT (abc,#caesarskey,0)
secretend=EXTRACT (abc,0,#caesarskey)
secretabc=CONCAT (secretbeg,secretend)

abc=STRINGS (abc,":</:"),secretabc=STRINGS (secretabc,":</:")
abc=SPLIT (abc),         secretabc=SPLIT (secretabc)
abc2secret=JOIN(abc," ",secretabc),secret2abc=JOIN(secretabc," ",abc)

BUILD X_TABLE abc2secret=*
DATA  {abc2secret}

BUILD X_TABLE secret2abc=*
DATA  {secret2abc}

ENCODED = EXCHANGE (text,abc2secret)
PRINT "text encoded    ",encoded

DECODED = EXCHANGE (encoded,secret2abc)
PRINT "encoded decoded ",decoded
```

{{out}}

```txt

text orginal    THE QUICK BROWN FOX JUMPS OVER THE LAZY DOG
text encoded    WKH TXLFN EURZQ IRA MXPSV RYHU WKH ODCB GRJ
encoded decoded THE QUICK BROWN FOX JUMPS OVER THE LAZY DOG

```



## TXR

The strategy here, one of many possible ones, is to build, at run time,the arguments to be passed to deffilter to construct a pair of filters <code>enc</code> and <code>dec</code> for encoding and decoding. Filters are specified as tuples of strings.

```txr
@(next :args)
@(cases)
@{key /[0-9]+/}
@text
@(or)
@  (throw error "specify <key-num> <text>")
@(end)
@(do
   (defvar k (int-str key 10)))
@(bind enc-dec
       @(collect-each ((i (range 0 25)))
          (let* ((p (tostringp (+ #\a i)))
                 (e (tostringp (+ #\a (mod (+ i k) 26))))
                 (P (upcase-str p))
                 (E (upcase-str e)))
            ^(((,p ,e) (,P ,E))
              ((,e ,p) (,E ,P))))))
@(deffilter enc . @(mappend (fun first) enc-dec))
@(deffilter dec . @(mappend (fun second) enc-dec))
@(output)
encoded: @{text :filter enc}
decoded: @{text :filter dec}
@(end)
```

{{out}}

```txt
$ ./txr caesar.txr 12 'Hello, world!'
encoded: Tqxxa, iadxp!
decoded: Vszzc, kcfzr!
$ ./txr caesar.txr 12 'Vszzc, kcfzr!'
encoded: Hello, world!
decoded: Jgnnq, yqtnf!

```



## TypeScript


```javascript
function replace(input: string, key: number) : string {
	return input.replace(/([a-z])/g,
		($1) => String.fromCharCode(($1.charCodeAt(0) + key + 26 - 97) % 26 + 97)
		).replace(/([A-Z])/g,
		($1) => String.fromCharCode(($1.charCodeAt(0) + key + 26 - 65) % 26 + 65));
}

// test
var str = 'The five boxing wizards jump quickly';
var encoded = replace(str, 3);
var decoded = replace(encoded, -3);

console.log('Enciphered: ' + encoded);
console.log('Deciphered: ' + decoded);
```



## UNIX Shell

{{works with|bash}}
I added a <tt>tr</tt> function to make this "pure" bash. In practice, you'd remove that function and use the external <tt>tr</tt> utility.

```bash
caesar() {
    local OPTIND
    local encrypt n=0
    while getopts :edn: option; do
        case $option in
            e) encrypt=true ;;
            d) encrypt=false ;;
            n) n=$OPTARG ;;
            :) echo "error: missing argument for -$OPTARG" >&2
               return 1 ;;
            ?) echo "error: unknown option -$OPTARG" >&2
               return 1 ;;
        esac
    done
    shift $((OPTIND-1))
    if [[ -z $encrypt ]]; then
        echo "error: specify one of -e or -d" >&2
        return 1
    fi

    local upper=ABCDEFGHIJKLMNOPQRSTUVWXYZ
    local lower=abcdefghijklmnopqrstuvwxyz
    if $encrypt; then
        tr "$upper$lower" "${upper:n}${upper:0:n}${lower:n}${lower:0:n}" <<< "$1"
    else
        tr "${upper:n}${upper:0:n}${lower:n}${lower:0:n}" "$upper$lower" <<< "$1"
    fi
}

tr() {
    local -A charmap
    local i trans line char
    for ((i=0; i<${#1}; i++)); do
        charmap[${1:i:1}]=${2:i:1}
    done
    while IFS= read -r line; do
        trans=""
        for ((i=0; i<${#line}; i++)); do
            char=${line:i:1}
            if [[ -n ${charmap[$char]} ]]; then
                trans+=${charmap[$char]}
            else
                trans+=$char
            fi
        done
        echo "$trans"
    done
}

txt="The five boxing wizards jump quickly."
enc=$(caesar -e -n 5 "$txt")
dec=$(caesar -d -n 5 "$enc")

echo "original:  $txt"
echo "encrypted: $enc"
echo "decrypted: $dec"
```

{{out}}

```txt

original:  The five boxing wizards jump quickly.
encrypted: Ymj knaj gtcnsl bnefwix ozru vznhpqd.
decrypted: The five boxing wizards jump quickly.
```



## Ursa


```ursa
decl string mode
while (not (or (= mode "encode") (= mode "decode")))
	out "encode/decode: " console
	set mode (lower (in string console))
end while

decl string message
out "message:       " console
set message (upper (in string console))

decl int key
out "key:           " console
set key (in int console)
if (or (> key 26) (< key 0))
	out endl "invalid key" endl console
	stop
end if

if (= mode "decode")
	set key (int (- 26 key))
end if

for (decl int i) (< i (size message)) (inc i)
	if (and (> (ord message<i>) 64) (< (ord message<i>) 91))
		out (chr (int (+ (mod (int (+ (- (ord message<i>) 65) key)) 26) 65))) console
	end if
end for
out endl console
```



## Ursala

The reification operator (<code>-:</code>) generates efficient code for applications like this given a table of inputs and outputs, which is obtained in this case by zipping the alphabet with itself rolled the right number of times, done separately for the upper and lower case letters and then combined.

```Ursala
#import std
#import nat

enc "n" = * -:~&@T ^p(rep"n" ~&zyC,~&)~~K30K31X letters    # encryption function
dec "n" = * -:~&@T ^p(~&,rep"n" ~&zyC)~~K30K31X letters    # decryption function

plaintext = 'the five boxing wizards jump quickly THE FIVE BOXING WIZARDS JUMP QUICKLY'

#show+ # exhaustive test

test = ("n". <.enc"n",dec"n"+ enc"n"> plaintext)*= nrange/1 25
```

{{out}}
<pre style="overflow: auto; height: 6em;">
uif gjwf cpyjoh xjabset kvnq rvjdlmz UIF GJWF CPYJOH XJABSET KVNQ RVJDLMZ
the five boxing wizards jump quickly THE FIVE BOXING WIZARDS JUMP QUICKLY
vjg hkxg dqzkpi ykbctfu lwor swkemna VJG HKXG DQZKPI YKBCTFU LWOR SWKEMNA
the five boxing wizards jump quickly THE FIVE BOXING WIZARDS JUMP QUICKLY
wkh ilyh eralqj zlcdugv mxps txlfnob WKH ILYH ERALQJ ZLCDUGV MXPS TXLFNOB
the five boxing wizards jump quickly THE FIVE BOXING WIZARDS JUMP QUICKLY
xli jmzi fsbmrk amdevhw nyqt uymgopc XLI JMZI FSBMRK AMDEVHW NYQT UYMGOPC
the five boxing wizards jump quickly THE FIVE BOXING WIZARDS JUMP QUICKLY
ymj knaj gtcnsl bnefwix ozru vznhpqd YMJ KNAJ GTCNSL BNEFWIX OZRU VZNHPQD
the five boxing wizards jump quickly THE FIVE BOXING WIZARDS JUMP QUICKLY
znk lobk hudotm cofgxjy pasv waoiqre ZNK LOBK HUDOTM COFGXJY PASV WAOIQRE
the five boxing wizards jump quickly THE FIVE BOXING WIZARDS JUMP QUICKLY
aol mpcl ivepun dpghykz qbtw xbpjrsf AOL MPCL IVEPUN DPGHYKZ QBTW XBPJRSF
the five boxing wizards jump quickly THE FIVE BOXING WIZARDS JUMP QUICKLY
bpm nqdm jwfqvo eqhizla rcux ycqkstg BPM NQDM JWFQVO EQHIZLA RCUX YCQKSTG
the five boxing wizards jump quickly THE FIVE BOXING WIZARDS JUMP QUICKLY
cqn oren kxgrwp frijamb sdvy zdrltuh CQN OREN KXGRWP FRIJAMB SDVY ZDRLTUH
the five boxing wizards jump quickly THE FIVE BOXING WIZARDS JUMP QUICKLY
dro psfo lyhsxq gsjkbnc tewz aesmuvi DRO PSFO LYHSXQ GSJKBNC TEWZ AESMUVI
the five boxing wizards jump quickly THE FIVE BOXING WIZARDS JUMP QUICKLY
esp qtgp mzityr htklcod ufxa bftnvwj ESP QTGP MZITYR HTKLCOD UFXA BFTNVWJ
the five boxing wizards jump quickly THE FIVE BOXING WIZARDS JUMP QUICKLY
ftq ruhq najuzs iulmdpe vgyb cguowxk FTQ RUHQ NAJUZS IULMDPE VGYB CGUOWXK
the five boxing wizards jump quickly THE FIVE BOXING WIZARDS JUMP QUICKLY
gur svir obkvat jvmneqf whzc dhvpxyl GUR SVIR OBKVAT JVMNEQF WHZC DHVPXYL
the five boxing wizards jump quickly THE FIVE BOXING WIZARDS JUMP QUICKLY
hvs twjs pclwbu kwnofrg xiad eiwqyzm HVS TWJS PCLWBU KWNOFRG XIAD EIWQYZM
the five boxing wizards jump quickly THE FIVE BOXING WIZARDS JUMP QUICKLY
iwt uxkt qdmxcv lxopgsh yjbe fjxrzan IWT UXKT QDMXCV LXOPGSH YJBE FJXRZAN
the five boxing wizards jump quickly THE FIVE BOXING WIZARDS JUMP QUICKLY
jxu vylu renydw mypqhti zkcf gkysabo JXU VYLU RENYDW MYPQHTI ZKCF GKYSABO
the five boxing wizards jump quickly THE FIVE BOXING WIZARDS JUMP QUICKLY
kyv wzmv sfozex nzqriuj aldg hlztbcp KYV WZMV SFOZEX NZQRIUJ ALDG HLZTBCP
the five boxing wizards jump quickly THE FIVE BOXING WIZARDS JUMP QUICKLY
lzw xanw tgpafy oarsjvk bmeh imaucdq LZW XANW TGPAFY OARSJVK BMEH IMAUCDQ
the five boxing wizards jump quickly THE FIVE BOXING WIZARDS JUMP QUICKLY
max ybox uhqbgz pbstkwl cnfi jnbvder MAX YBOX UHQBGZ PBSTKWL CNFI JNBVDER
the five boxing wizards jump quickly THE FIVE BOXING WIZARDS JUMP QUICKLY
nby zcpy vircha qctulxm dogj kocwefs NBY ZCPY VIRCHA QCTULXM DOGJ KOCWEFS
the five boxing wizards jump quickly THE FIVE BOXING WIZARDS JUMP QUICKLY
ocz adqz wjsdib rduvmyn ephk lpdxfgt OCZ ADQZ WJSDIB RDUVMYN EPHK LPDXFGT
the five boxing wizards jump quickly THE FIVE BOXING WIZARDS JUMP QUICKLY
pda bera xktejc sevwnzo fqil mqeyghu PDA BERA XKTEJC SEVWNZO FQIL MQEYGHU
the five boxing wizards jump quickly THE FIVE BOXING WIZARDS JUMP QUICKLY
qeb cfsb ylufkd tfwxoap grjm nrfzhiv QEB CFSB YLUFKD TFWXOAP GRJM NRFZHIV
the five boxing wizards jump quickly THE FIVE BOXING WIZARDS JUMP QUICKLY
rfc dgtc zmvgle ugxypbq hskn osgaijw RFC DGTC ZMVGLE UGXYPBQ HSKN OSGAIJW
the five boxing wizards jump quickly THE FIVE BOXING WIZARDS JUMP QUICKLY
sgd ehud anwhmf vhyzqcr itlo pthbjkx SGD EHUD ANWHMF VHYZQCR ITLO PTHBJKX
the five boxing wizards jump quickly THE FIVE BOXING WIZARDS JUMP QUICKLY
```



## Vala

This is a port of the C# code present in this page.

```Vala
static void println(string str) {
    stdout.printf("%s\r\n", str);
}

static unichar encrypt_char(unichar ch, int code) {
    if (!ch.isalpha()) return ch;

    unichar offset = ch.isupper() ? 'A' : 'a';
    return (unichar)((ch + code - offset) % 26 + offset);
}

static string encrypt(string input, int code) {
    var builder = new StringBuilder();

    unichar c;
    for (int i = 0; input.get_next_char(ref i, out c);) {
        builder.append_unichar(encrypt_char(c, code));
    }

    return builder.str;
}

static string decrypt(string input, int code) {
    return encrypt(input, 26 - code);
}

const string test_case = "The quick brown fox jumped over the lazy dog";

void main() {
    println(test_case);
    println(encrypt(test_case, -1));
    println(decrypt(encrypt(test_case, -1), -1));
}
```


{{out}}
<pre style="overflow: auto; height: 6em;">
The quick brown fox jumped over the lazy dog
Sgd pthbj aqnvm enw itlodc nudq sgd kvyx cnf
The quick brown fox jumped over the lwzy dog

```



## VBA



```vb

Option Explicit

Sub Main_Caesar()
Dim ch As String
    ch = Caesar_Cipher("CAESAR: Who is it in the press that calls on me? I hear a tongue, shriller than all the music, Cry 'Caesar!' Speak; Caesar is turn'd to hear.", 14)
    Debug.Print ch
    Debug.Print Caesar_Cipher(ch, -14)
End Sub

Function Caesar_Cipher(sText As String, lngNumber As Long) As String
Dim Tbl, strGlob As String, strTemp As String, i As Long, bytAscii As Byte
  Const MAJ As String = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    Const NB_LETTERS As Byte = 26
      Const DIFFASCIIMAJ As Byte = 65 - NB_LETTERS
        Const DIFFASCIIMIN As Byte = 97 - NB_LETTERS

    strTemp = sText
    If lngNumber < NB_LETTERS And lngNumber > NB_LETTERS * -1 Then
        strGlob = String(NB_LETTERS * 4, " ")
        LSet strGlob = MAJ & MAJ & MAJ
        Tbl = Split(StrConv(strGlob, vbUnicode), Chr(0))
        For i = 1 To Len(strTemp)
            If Mid(strTemp, i, 1) Like "[a-zA-Z]" Then
                bytAscii = Asc(Mid(strTemp, i, 1))
                If Mid(strTemp, i, 1) = Tbl(bytAscii - DIFFASCIIMAJ) Then
                    Mid(strTemp, i) = Tbl(bytAscii - DIFFASCIIMAJ + lngNumber)
                Else
                    Mid(strTemp, i) = LCase(Tbl(bytAscii - DIFFASCIIMIN + lngNumber))
                End If
            End If
        Next i
    End If
    Caesar_Cipher = strTemp
End Function

```


{{out}}

```txt
QOSGOF: Kvc wg wh wb hvs dfsgg hvoh qozzg cb as? W vsof o hcbuis, gvfwzzsf hvob ozz hvs aigwq, Qfm 'Qosgof!' Gdsoy; Qosgof wg hifb'r hc vsof.
CAESAR: Who is it in the press that calls on me? I hear a tongue, shriller than all the music, Cry 'Caesar!' Speak; Caesar is turn'd to hear.
```



## Vedit macro language

This implementation ciphers/deciphers a highlighted block of text in-place in current edit buffer.

```vedit
#10 = Get_Num("Enter the key: positive to cipher, negative to de-cipher: ", STATLINE)

Goto_Pos(Block_Begin)
while(Cur_Pos < Block_End) {
    #11 = Cur_Char & 0x60 + 1
    if (Cur_Char >= 'A') {
        Ins_Char((Cur_Char - #11 + 26 + #10) % 26 + #11, OVERWRITE)
    } else {
	Char(1)
    }
}
```


{{out}} with key 13:

```txt

Original text:    Quick brown Fox jumps over the lazy Dog.
Encrypted text:   Dhvpx oebja Sbk whzcf bire gur ynml Qbt.
Decrypted text:   Quick brown Fox jumps over the lazy Dog.

```



## vbscript

Note that a left rotation has an equivalent right rotation so all rotations are converted to the equivalent right rotation prior to translation.

```vbscript

	str = "IT WAS THE BEST OF TIMES, IT WAS THE WORST OF TIMES."

	Wscript.Echo str
	Wscript.Echo Rotate(str,5)
	Wscript.Echo Rotate(Rotate(str,5),-5)

	'Rotate (Caesar encrypt/decrypt) test <numpos> positions.
	'  numpos < 0 - rotate left
	'  numpos > 0 - rotate right
	'Left rotation is converted to equivalent right rotation

	Function Rotate (text, numpos)

		dim dic: set dic = CreateObject("Scripting.Dictionary")
		dim ltr: ltr = Split("A B C D E F G H I J K L M N O P Q R S T U V W X Y Z")
		dim rot: rot = (26 + numpos Mod 26) Mod 26 'convert all to right rotation
		dim ch
		dim i

		for i = 0 to ubound(ltr)
			dic(ltr(i)) = ltr((rot+i) Mod 26)
		next

		Rotate = ""

		for i = 1 to Len(text)
			ch = Mid(text,i,1)
			if dic.Exists(ch) Then
				Rotate = Rotate & dic(ch)
			else
				Rotate = Rotate & ch
			end if
		next

	End Function

```

{{out}}

```txt

D:\script>Caesar.vbs
IT WAS THE BEST OF TIMES, IT WAS THE WORST OF TIMES.
NY BFX YMJ GJXY TK YNRJX, NY BFX YMJ BTWXY TK YNRJX.
IT WAS THE BEST OF TIMES, IT WAS THE WORST OF TIMES.

```



## Visual Basic .NET

{{trans|C#}}

```vbnet
Module Module1

    Function Encrypt(ch As Char, code As Integer) As Char
        If Not Char.IsLetter(ch) Then
            Return ch
        End If

        Dim offset = AscW(If(Char.IsUpper(ch), "A"c, "a"c))
        Dim test = (AscW(ch) + code - offset) Mod 26 + offset
        Return ChrW(test)
    End Function

    Function Encrypt(input As String, code As Integer) As String
        Return New String(input.Select(Function(ch) Encrypt(ch, code)).ToArray())
    End Function

    Function Decrypt(input As String, code As Integer) As String
        Return Encrypt(input, 26 - code)
    End Function

    Sub Main()
        Dim str = "Pack my box with five dozen liquor jugs."

        Console.WriteLine(str)
        str = Encrypt(str, 5)
        Console.WriteLine("Encrypted: {0}", str)
        str = Decrypt(str, 5)
        Console.WriteLine("Decrypted: {0}", str)
    End Sub

End Module
```

{{out}}

```txt
Pack my box with five dozen liquor jugs.
Encrypted: Ufhp rd gtc bnym knaj itejs qnvztw ozlx.
Decrypted: Pack my box with five dozen liquor jugs.
```



## Wortel


```wortel
@let {
  ; this function only replaces letters and keeps case
  ceasar &[s n] !!s.replace &"[a-z]"gi &[x] [
    @vars {
      t x.charCodeAt.
      l ?{
        && > t 96 < t 123 97
        && > t 64 < t 91 65
        0
      }
    }
    !String.fromCharCode ?{
      l +l ~% 26 -+ n t l
      t
    }
  ]
  !!ceasar "abc $%^ ABC" 10
}
```

Returns:

```txt
"klm $%^ KLM"
```



## X86 Assembly

{{trans|C custom implementation}}
{{works with|GCC|7.3.0 - Ubuntu 18.04 64-bit}}

```asm
                                        # Author: Ettore Forigo - Hexwell

.intel_syntax noprefix

.text
.globl main
main:
.PROLOGUE:
    push    rbp
    mov rbp, rsp
    sub rsp, 32
    mov QWORD PTR [rbp-32], rsi         # argv

.BODY:
    mov BYTE PTR [rbp-1], 0             # shift = 0

    .I_LOOP_INIT:
        mov BYTE PTR [rbp-2], 0         # i = 0
        jmp .I_LOOP_CONDITION           # I_LOOP_CONDITION
    .I_LOOP_BODY:
        movzx edx, BYTE PTR[rbp-1]      # shift
        mov eax, edx                    # shift
        sal eax, 2                      # shift << 2 == i * 4
        add eax, edx                    # shift * 4 + shift = shift * 5
        add eax, eax                    # shift * 5 + shift * 5 = shift * 10
        mov ecx, eax                    # shift * 10
        mov rax, QWORD PTR [rbp-32]     # argv
        add rax, 8                      # argv + 1
        mov rdx, QWORD PTR [rax]        # argv[1]
        movzx eax, BYTE PTR [rbp-2]     # i
        add rax, rdx                    # argv[1] + i
        movzx eax, BYTE PTR [rax]       # argv[1][i]
        add eax, ecx                    # shift * 10 + argv[1][i]
        sub eax, 48                     # shift * 10 + argv[1][i] - '0'
        mov BYTE PTR [rbp-1], al        # shift = shift * 10 + argv[1][i] - '0'
    .I_LOOP_INCREMENT:
        movzx eax, BYTE PTR [rbp-2]     # i
        add eax, 1                      # i + 1
        mov BYTE PTR [rbp-2], al        # i++
    .I_LOOP_CONDITION:
        mov rax, QWORD PTR [rbp-32]     # argv
        add rax, 8                      # argv + 1
        mov rax, QWORD PTR [rax]        # argv[1]
        movzx edx, BYTE PTR [rbp-2]     # i
        add rax, rdx                    # argv[1] + i
        movzx rax, BYTE PTR [rax]       # argv[1][i]
        test al, al                     # argv[1][i]?
        jne .I_LOOP_BODY                # I_LOOP_BODY

    .CAESAR_LOOP_INIT:
        mov BYTE PTR [rbp-2], 0         # i = 0
        jmp .CAESAR_LOOP_CONDITION      # CAESAR_LOOP_CONDITION
    .CAESAR_LOOP_BODY:
        mov rax, QWORD PTR [rbp-32]     # argv
        add rax, 16                     # argv + 2
        mov rdx, QWORD PTR [rax]        # argv[2]
        movzx eax, BYTE PTR [rbp-2]     # i
        add rax, rdx                    # argv[2] + i
        mov rbx, rax                    # argv[2] + i
        movzx eax, BYTE PTR [rax]       # argv[2][i]
        cmp al, 32                      # argv[2][i] == ' '
        je .CAESAR_LOOP_INCREMENT       # CAESAR_LOOP_INCREMENT
        movzx edx, BYTE PTR [rbx]       # argv[2][i]
        mov ecx, edx                    # argv[2][i]
        movzx edx, BYTE PTR [rbp-1]     # shift
        add edx, ecx                    # argv[2][i] + shift
        sub edx, 97                     # argv[2][i] + shift - 'a'
        mov BYTE PTR [rbx], dl          # argv[2][i] = argv[2][i] + shift - 'a'
        movzx eax, BYTE PTR [rbx]       # argv[2][i]
        cmp al, 25                      # argv[2][i] <=> 25
        jle .CAESAR_RESTORE_ASCII       # <= CAESAR_RESTORE_ASCII
        movzx edx, BYTE PTR [rbx]       # argv[2][i]
        sub edx, 26                     # argv[2][i] - 26
        mov BYTE PTR [rbx], dl          # argv[2][i] = argv[2][i] - 26
    .CAESAR_RESTORE_ASCII:
        movzx edx, BYTE PTR [rbx]       # argv[2][i]
        add edx, 97                     # argv[2][i] + 'a'
        mov BYTE PTR [rbx], dl          # argv[2][i] = argv[2][i] + 'a'
    .CAESAR_LOOP_INCREMENT:
        movzx eax, BYTE PTR [rbp-2]     # i
        add eax, 1                      # i + 1
        mov BYTE PTR [rbp-2], al        # i++
    .CAESAR_LOOP_CONDITION:
        mov rax, QWORD PTR [rbp-32]     # argv
        add rax, 16                     # argv + 2
        mov rdx, QWORD PTR [rax]        # argv[2]
        movzx eax, BYTE PTR [rbp-2]     # i
        add rax, rdx                    # argv[2] + i
        movzx eax, BYTE PTR [rax]       # argv[2][i]
        test al, al                     # argv[2][i]?
        jne .CAESAR_LOOP_BODY           # CAESAR_LOOP_BODY

    mov rax, QWORD PTR [rbp-32]         # argv
    add rax, 16                         # argv + 2
    mov rax, QWORD PTR [rax]            # argv[2]
    mov rdi, rax                        # argv[2]
    call    puts                        # puts(argv[2])

.RETURN:
    mov eax, 0                          # 0
    leave
    ret                                 # return 0
```

Usage:

```bash
$ gcc caesar.S -o caesar
$ ./caesar 10 abc
klm
$ ./caesar 16 klm
abc
```



## XLISP


```lisp
(defun caesar-encode (text key)
    (defun encode (ascii-code)
        (defun rotate (character alphabet)
            (define code (+ character key))
            (cond
                ((> code (+ alphabet 25)) (- code 26))
                ((< code alphabet) (+ code 26))
                (t code)))
        (cond
            ((and (>= ascii-code 65) (<= ascii-code 90)) (rotate ascii-code 65))
            ((and (>= ascii-code 97) (<= ascii-code 122)) (rotate ascii-code 97))
            (t ascii-code)))
        (list->string (mapcar integer->char (mapcar encode (mapcar char->integer (string->list text))))))

(defun caesar-decode (text key)
    (caesar-encode text (- 26 key)))
```

Test it in a REPL:

```lisp
[1] (define caesar-test (caesar-encode "CAESAR: Who is it in the press that calls on me? I hear a tongue, shriller than all the music, Cry 'Caesar!' Speak; Caesar is turn'd to hear." 14))

CAESAR-TEST
[2] caesar-test

"QOSGOF: Kvc wg wh wb hvs dfsgg hvoh qozzg cb as? W vsof o hcbuis, gvfwzzsf hvob ozz hvs aigwq, Qfm 'Qosgof!' Gdsoy; Qosgof wg hifb'r hc vsof."
[3] (caesar-decode caesar-test 14)

"CAESAR: Who is it in the press that calls on me? I hear a tongue, shriller than all the music, Cry 'Caesar!' Speak; Caesar is turn'd to hear."
```



## XPL0

To decrypt a message use the negative value of the encrypting key.
Usage: caesar key <infile.txt >outfile.xxx


```XPL0
code ChIn=7, ChOut=8, IntIn=10;
int  Key, C;
[Key:= IntIn(8);
repeat  C:= ChIn(1);
        if C>=^a & C<=^z then C:= C-$20;
        if C>=^A & C<=^Z then
            [C:= C+Key;
            if C>^Z then C:= C-26
            else if C<^A then C:= C+26;
            ];
        ChOut(0, C);
until   C=$1A; \EOF
]
```


Example outfile.xxx:

```txt

SDFN PB ERA ZLWK ILYH GRCHQ OLTXRU MXJV.

```



## Yabasic


```Yabasic

REM *** By changing the key and pattern, an encryption system that is difficult to break can be achieved. ***

text$ = "You are encouraged to solve this task according to the task description, using any language you may know."
key$ = "desoxirribonucleic acid" // With a single character you get a Caesar encryption. With more characters the key is much more difficult to discover.
CIPHER = 1 : DECIPHER = -1

print text$ : print
encrypted$ = criptex$(text$, key$, CIPHER)
a = open("cryptedText.txt", "w") : if a then print #a encrypted$ : close #a end if
print encrypted$ : print
print criptex$(encrypted$, key$, DECIPHER)

sub criptex$(text$, key$, mode)
	local i, k, delta, longtext, longPattern, longkey, pattern$, res$

	pattern$ = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890 .,;:()-"

	longPattern = len(pattern$)
	longtext = len(text$)
	longkey = len(key$)

	for i = 1 to longtext
		k = k + 1 : if k > longkey k = 1
		delta = instr(pattern$, mid$(text$, i, 1))
		delta = delta + (mode * instr(pattern$, mid$(key$, k, 1)))
		if delta > longPattern then
			delta = delta - longPattern
		elseif delta < 1 then
			delta = longPattern + delta
		end if
		res$ = res$ + mid$(pattern$, delta, 1)
	next i

	return res$
end sub
```

{{out}}

```txt
You are encouraged to solve this task according to the task description, using any language you may know.

3tNhyAwknprCPumlng4urbwsqOtqCzAB,IoNneflfhsgrrk(MDqCzwbvpGF;pjBfkjsCmssnhSBAFp,pBT;xfwjnbjn:CtNhKjQktpDKo

You are encouraged to solve this task according to the task description, using any language you may know.
```



## zkl


```zkl
fcn caesarCodec(str,n,encode=True){
   var [const] letters=["a".."z"].chain(["A".."Z"]).pump(String);  // static
   if(not encode) n=26 - n;
   m,sz := n + 26, 26 - n;
   ltrs:=String(letters[n,sz],letters[0,n],letters[m,sz],letters[26,n]);
   str.translate(letters,ltrs)
}
```


```zkl
text:="The five boxing wizards jump quickly";
N:=3;
code:=caesarCodec(text,N);
println("text = ",text);
println("encoded(%d) = %s".fmt(N,code));
println("decoded = ",caesarCodec(code,N,False));
```

{{out}}

```txt

text = The five boxing wizards jump quickly
encoded(3) = Wkh ilyh eralqj zlcdugv mxps txlfnob
decoded = The five boxing wizards jump quickly

```



## zonnon


```zonnon

module Caesar;
const
	size = 25;

type
	Operation = (code,decode);

procedure C_D(s:string;k:integer;op: Operation): string;
var
	i,key: integer;
	resp: string;
	n,c: char;
begin
	resp := "";
	if op = Operation.decode then key := k else key := (26 - k) end;
	for i := 0 to len(s) - 1 do
		c := cap(s[i]);
		if (c >= 'A') & (c <= 'Z') then
			resp := resp +
				string(char(integer('A') + ((integer(c) - integer('A') + key )) mod 26));
		else
			resp := resp + string(c)
		end;
	end;
	return  resp
end C_D;

procedure {public} Cipher(s:string;k:integer):string;
var
	i: integer;
	resp: string;
	n,c: char;
begin
	return C_D(s,k,Operation.code)
end Cipher;

procedure {public} Decipher(s:string;k:integer):string;
var
	i: integer;
	resp: string;
	n,c: char;
begin
	return C_D(s,k,Operation.decode)
end Decipher;

var
	txt,cipher,decipher: string;

begin
	txt := "HI";cipher := Caesar.Cipher(txt,2);decipher := Caesar.Decipher(cipher,2);
	writeln(txt," -c-> ",cipher," -d-> ",decipher);
	txt := "ZA";cipher := Caesar.Cipher(txt,2);decipher := Caesar.Decipher(cipher,2);
	writeln(txt," -c-> ",cipher," -d-> ",decipher);
	txt := "The five boxing wizards jump quickly";
	cipher := Caesar.Cipher(txt,2);decipher := Caesar.Decipher(cipher,2);
	writeln(txt," -c-> ",cipher," -d-> ",decipher)
end Caesar.

```

{{Out}}

```txt

  HI -c->   FG -d->   HI
  ZA -c->   XY -d->   ZA
The five boxing wizards jump quickly -c-> RFC DGTC ZMVGLE UGXYPBQ HSKN OSGAIJW -d-> THE FIVE BOXING WIZARDS JUMP QUICKLY

```



## ZX Spectrum Basic

{{trans|BBC BASIC}}

```zxbasic
10 LET t$="PACK MY BOX WITH FIVE DOZEN LIQUOR JUGS"
20 PRINT t$''
30 LET key=RND*25+1
40 LET k=key: GO SUB 1000: PRINT t$''
50 LET k=26-key: GO SUB 1000: PRINT t$
60 STOP
1000 FOR i=1 TO LEN t$
1010 LET c= CODE t$(i)
1020 IF c<65 OR c>90 THEN GO TO 1050
1030 LET c=c+k: IF c>90 THEN LET c=c-90+64
1040 LET t$(i)=CHR$ c
1050 NEXT i
1060 RETURN

```

{{trans|Yabasic}}

```zxbasic
10 LET t$="Wonderful ZX Spectrum."
20 LET c$="a":REM more characters, more difficult for decript
30 LET CIFRA=1: LET DESCIFRA=-1
40 PRINT t$''
50 LET modo=CIFRA: GO SUB 1000
60 PRINT r$''
70 LET t$=r$: LET modo=DESCIFRA: GO SUB 1000
80 PRINT r$''
90 STOP
1000 REM Criptex
1010 LET p$="abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890 .,;:()-"
1020 LET longPatron=LEN p$
1030 LET longTexto=LEN t$
1040 LET longClave=LEN c$
1045 LET k=0: LET r$=""
1050 FOR i=1 TO longTexto
1060 LET k=k+1: IF k>longClave THEN LET k=1
1070 LET x$=t$(i)
1080 FOR j=1 TO longPatron
1090 IF x$=p$(j) THEN LET delta=j: GO TO 1110
1100 NEXT j
1110 LET x$=c$(k)
1120 FOR j=1 TO longPatron
1130 IF x$=p$(j) THEN LET delta=delta+modo*j: GO TO 1150
1140 NEXT j
1150 IF delta>longPatron THEN LET delta=delta-longPatron: GO TO 1170
1160 IF delta<1 THEN LET delta=longPatron+delta
1170 LET r$=r$+p$(delta)
1180 NEXT i
1190 RETURN
```

{{omit from|GUISS}}
