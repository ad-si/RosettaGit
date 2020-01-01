+++
title = "Unicode strings"
description = ""
date = 2019-08-12T02:48:55Z
aliases = []
[extra]
id = 9995
[taxonomies]
categories = []
tags = []
+++

{{task}}
As the world gets smaller each day, internationalization becomes more and more important.   For handling multiple languages, [[Unicode]] is your best friend.

It is a very capable tool, but also quite complex compared to older single- and double-byte character encodings.

How well prepared is your programming language for Unicode?


;Task:
Discuss and demonstrate its unicode awareness and capabilities.


Some suggested topics:
:*   How easy is it to present Unicode strings in source code?
:*   Can Unicode literals be written directly, or be part of identifiers/keywords/etc?
:*   How well can the language communicate with the rest of the world?
:*   Is it good at input/output with Unicode?
:*   Is it convenient to manipulate Unicode strings in the language?
:*   How broad/deep does the language support Unicode?
:*   What encodings (e.g. UTF-8, UTF-16, etc) can be used?
:*   Does it support normalization?


;Note:
This task is a bit unusual in that it encourages general discussion rather than clever coding.


;See also:
*   [[Unicode variable names]]
*   [[Terminal control/Display an extended character]]





## 80386 Assembly


* How well prepared is the programming language for Unicode? - Prepared, in terms of handling: Assembly language can do anything the computer can do. However, it has no Unicode facilities as part of the language.

* How easy is it to present Unicode strings in source code? - Easy, they are in hexadecimal.

* Can Unicode literals be written directly - Depends on the compiler. MASM does not allow this. All data in assembly language is created from a series of bytes. Literal characters are not part of the language. They are number crunched down into a byte sequence by the compiler. If the compiler can read Unicode, then you are onto a winner.

* or be part of identifiers/keywords/etc? - Depends on compiler. Intel notation does not use Unicode identifiers or mnemonics. Assembly language converts to numeric machine code, so everything is represented as mnemonics. You can use your own mnemonics, but you need to be able to compile them. One way to do this is to use a wrapper (which you would create) that converts your Unicode mnemonic notation to the notation that the compiler is expecting.

* How well can the language communicate with the rest of the world? - Difficult. This is a low level language, so all communication can be done, but you have to set up data structures, and produce many lines of code for just basic tasks.

* Is it good at input/output with Unicode? - Yes and No. The Unicode bit is easy, but for input/output, we have to set up data structures and produce many lines of code, or link to code libraries.

* Is it convenient to manipulate Unicode strings in the language? - No. String manipulation requires lots of code. We can link to code libraries though, but it is not as straightforward, as it would be in a higher level language.

* How broad/deep does the language support Unicode? We can do anything in assembly language, so support is 100%, but nothing is convenient with respect to Unicode. Strings are just a series of bytes, treatment of a series of bytes as a string is down to the compiler, if it provides string support as an extension. You need to be prepared to define data structures containing the values that you want.

* What encodings (e.g. UTF-8, UTF-16, etc) can be used? All encodings are supported, but again, nothing is convenient with respect to encodings, although hexadecimal notation is good to use in assembly language. Normalization is not supported unless you write lots of code.


## 8th

* In 8th all strings are UTF-8 encoded.  You can simply enter any text you like in a string.
* For special characters one may use the "\u" escape, e.g. "\u05ad"
* All the string manipulation words are UTF-8 aware, so in general the user doesn't have to be concerned about bytes vs characters


## Ada

* As of Ada 2005, all source/identifiers/keywords/literals/etc can be in up to 32bit characters as long as the compiler is told what encoding you are using.
* Unicode input/output has been in ada for much longer, only unicode source/literals are recent additions to the standard.
* Manipulation is the same as any other strings, but operates from *_Wide_Text_* modules rather than *_Text_*
* Supports the entire set of characters from ISO/IEC 10646:2003
* Extensive support of Unicode (including normalization, collation, etc.) and text codecs are provided by [http://forge.ada-ru.org/matreshka Matreshka].


## ALGOL 68

How well prepared is the programming language for Unicode? - ALGOL 68 is character set agnostic and the standard explicitly permits the use of a universal character set.  The standard includes routines like "make conv" to permit the opening of files and devices using alternate characters sets and converting character sets on the fly.

How easy is it to present Unicode strings in source code? - Easy.

Can Unicode literals be written directly - No, a REPR operator must be used to encode the string in UTF8.

Can Unicode literals be part of identifiers/keywords/etc? - Yes... ALGOL 68 is character set agnostic and the standard explicitly permits the use of a universal character set.  Implementation for English, German, Polish and Cyrillic have been created.  However [[ALGOL 68G]] supports only "Worthy" Character sets.

How well can the language communicate with the rest of the world? - Acceptable.

Is it good at input/output with Unicode? - No, although the "make conv" routine is in the standard it is rarely fully implemented.

Is it convenient to manipulate Unicode strings in the language? - Yes

How broad/deep does the language support Unicode? What encodings (e.g. UTF-8, UTF-16, etc) can be used? The attached set of utility routine is currently only for UTF8.  Currently the Unicode routines like is_digit, is_letter, is_lower_case etc are not implemented.

{{works with|ALGOL 68|Revision 1 - no extensions to language used.}}
{{works with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-1.18.0/algol68g-1.18.0-9h.tiny.el5.centos.fc11.i386.rpm/download 1.18.0-9h.tiny].}}
{{wont work with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8-8d] - due to extensive use of '''format'''[ted] ''transput''.}}

```algol68
#!/usr/local/bin/a68g --script #
# -*- coding: utf-8 -*- #

# UNICHAR/UNICODE must be printed using REPR to convert to UTF8 #

MODE UNICHAR = STRUCT(BITS #31# bits); # assuming bits width >=31 #
MODE UNICODE = FLEX[0]UNICHAR;

OP INITUNICHAR = (BITS bits)UNICHAR: (UNICHAR out; bits OF out := #ABS# bits; out);
OP INITUNICHAR = (CHAR char)UNICHAR: (UNICHAR out; bits OF out := BIN ABS char; out);
OP INITBITS = (UNICHAR unichar)BITS: #BIN# bits OF unichar;

PROC raise value error = ([]UNION(FORMAT,BITS,STRING)argv )VOID: (
  putf(stand error, argv); stop
);

MODE YIELDCHAR =    PROC(CHAR)VOID;    MODE GENCHAR =    PROC(YIELDCHAR)VOID;
MODE YIELDUNICHAR = PROC(UNICHAR)VOID; MODE GENUNICHAR = PROC(YIELDUNICHAR)VOID;

PRIO DOCONV = 1;

# Convert a stream of UNICHAR into a stream of UTFCHAR #
OP DOCONV = (GENUNICHAR gen unichar, YIELDCHAR yield)VOID:(
  BITS non ascii = NOT 2r1111111;
# FOR UNICHAR unichar IN # gen unichar( # ) DO ( #
##   (UNICHAR unichar)VOID: (
    BITS bits := INITBITS unichar;
    IF (bits AND non ascii) = 2r0 THEN # ascii #
      yield(REPR ABS bits)
    ELSE
      FLEX[6]CHAR buf := "?"*6; # initialise work around #
      INT bytes := 0;
      BITS byte lead bits = 2r10000000;
      FOR ofs FROM UPB buf BY -1 WHILE
        bytes +:= 1;
        buf[ofs]:= REPR ABS (byte lead bits OR bits AND 2r111111);
        bits := bits SHR 6;
    # WHILE # bits NE 2r0 DO
        SKIP
      OD;
      BITS first byte lead bits = BIN (ABS(2r1 SHL bytes)-2) SHL (UPB buf - bytes + 1);
      buf := buf[UPB buf-bytes+1:];
      buf[1] := REPR ABS(BIN ABS buf[1] OR first byte lead bits);
      FOR i TO UPB buf DO yield(buf[i]) OD
    FI
# OD # ))
);

# Convert a STRING into a stream of UNICHAR #
OP DOCONV = (STRING string, YIELDUNICHAR yield)VOID: (
  PROC gen char = (YIELDCHAR yield)VOID:
    FOR i FROM LWB string TO UPB string DO yield(string[i]) OD;
  gen char DOCONV yield
);

CO Prosser/Thompson UTF8 encoding scheme
Bits Last code point Byte 1   Byte 2   Byte 3   Byte 4   Byte 5   Byte 6
 7   U+007F          0xxxxxxx
11   U+07FF          110xxxxx 10xxxxxx
16   U+FFFF          1110xxxx 10xxxxxx 10xxxxxx
21   U+1FFFFF        11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
26   U+3FFFFFF       111110xx 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx
31   U+7FFFFFFF      1111110x 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx
END CO

# Quickly calculate the length of the UTF8 encoded string #
PROC upb utf8 = (STRING utf8 string)INT:(
  INT bytes to go := 0;
  INT upb := 0;
  FOR i FROM LWB utf8 string TO UPB utf8 string DO
    CHAR byte := utf8 string[i];
    IF bytes to go = 0 THEN # start new utf char #
      bytes to go :=
        IF   ABS byte <= ABS 2r01111111 THEN 1 #  7 bits #
        ELIF ABS byte <= ABS 2r11011111 THEN 2 # 11 bits #
        ELIF ABS byte <= ABS 2r11101111 THEN 3 # 16 bits #
        ELIF ABS byte <= ABS 2r11110111 THEN 4 # 21 bits #
        ELIF ABS byte <= ABS 2r11111011 THEN 5 # 26 bits #
        ELIF ABS byte <= ABS 2r11111101 THEN 6 # 31 bits #
        ELSE raise value error(("Invalid UTF-8 bytes", BIN ABS byte)); ~ FI
    FI;
    bytes to go -:= 1; # skip over trailing bytes #
    IF bytes to go = 0 THEN upb +:= 1 FI
  OD;
  upb
);

# Convert a stream of CHAR into a stream of UNICHAR #
OP DOCONV = (GENCHAR gen char, YIELDUNICHAR yield)VOID: (
  INT bytes to go := 0;
  INT lshift;
  BITS mask, out;

  # FOR CHAR byte IN # gen char( # ) DO ( #
  ##   (CHAR byte)VOID: (
      INT bits := ABS byte;
      IF bytes to go = 0 THEN # start new unichar #
        bytes to go :=
          IF   bits <= ABS 2r01111111 THEN 1 #  7 bits #
          ELIF bits <= ABS 2r11011111 THEN 2 # 11 bits #
          ELIF bits <= ABS 2r11101111 THEN 3 # 16 bits #
          ELIF bits <= ABS 2r11110111 THEN 4 # 21 bits #
          ELIF bits <= ABS 2r11111011 THEN 5 # 26 bits #
          ELIF bits <= ABS 2r11111101 THEN 6 # 31 bits #
          ELSE raise value error(("Invalid UTF-8 bytes", BIN bits)); ~ FI;
        IF bytes to go = 1 THEN
          lshift := 7; mask := 2r1111111
        ELSE
          lshift := 7 - bytes to go; mask :=  BIN(ABS(2r1 SHL lshift)-1)
        FI;
        out := mask AND BIN bits;

        lshift := 6; mask := 2r111111 # subsequently pic 6 bits at a time #
      ELSE
        out := (out SHL lshift) OR ( mask AND BIN bits)
      FI;
      bytes to go -:= 1;
      IF bytes to go = 0 THEN yield(INITUNICHAR out) FI
  # OD # ))
);

# Convert a string of UNICHAR into a stream of UTFCHAR #
OP DOCONV = (UNICODE unicode, YIELDCHAR yield)VOID:(
  PROC gen unichar = (YIELDUNICHAR yield)VOID:
    FOR i FROM LWB unicode TO UPB unicode DO yield(unicode[i]) OD;
  gen unichar DOCONV yield
);

# Some convenience/shorthand U operators #
# Convert a BITS into a UNICODE char #
OP U = (BITS bits)UNICHAR:
  INITUNICHAR bits;

# Convert a []BITS into a UNICODE char #
OP U = ([]BITS array bits)[]UNICHAR:(
  [LWB array bits:UPB array bits]UNICHAR out;
  FOR i FROM LWB array bits TO UPB array bits DO bits OF out[i]:=array bits[i] OD;
  out
);

# Convert a CHAR into a UNICODE char #
OP U = (CHAR char)UNICHAR:
  INITUNICHAR char;

# Convert a STRING into a UNICODE string #
OP U = (STRING utf8 string)UNICODE: (
  FLEX[upb utf8(utf8 string)]UNICHAR out;
  INT i := 0;
# FOR UNICHAR char IN # utf8 string DOCONV (
##   (UNICHAR char)VOID:
       out[i+:=1] := char
# OD #);
  out
);

# Convert a UNICODE string into a UTF8 STRING #
OP REPR = (UNICODE string)STRING: (
  STRING out;
# FOR CHAR char IN # string DOCONV (
##   (CHAR char)VOID: (
       out +:= char
# OD #));
  out
);

# define the most useful OPerators on UNICODE CHARacter arrays #
# Note: LWB, UPB and slicing works as per normal #

OP + = (UNICODE a,b)UNICODE: (
  [UPB a + UPB b]UNICHAR out;
  out[:UPB a]:= a; out[UPB a+1:]:= b;
  out
);

OP + = (UNICODE a, UNICHAR b)UNICODE: a+UNICODE(b);
OP + = (UNICHAR a, UNICODE b)UNICODE: UNICODE(a)+b;
OP + = (UNICHAR a,b)UNICODE: UNICODE(a)+b;

# Suffix a character to the end of a UNICODE string #
OP +:= = (REF UNICODE a, UNICODE b)VOID: a := a + b;
OP +:= = (REF UNICODE a, UNICHAR b)VOID: a := a + b;

# Prefix a character to the beginning of a UNICODE string #
OP +=: = (UNICODE b, REF UNICODE a)VOID: a := b + a;
OP +=: = (UNICHAR b, REF UNICODE a)VOID: a := b + a;

OP * = (UNICODE a, INT n)UNICODE: (
  UNICODE out := a;
  FOR i FROM 2 TO n DO out +:= a OD;
  out
);
OP * = (INT n, UNICODE a)UNICODE: a * n;

OP * = (UNICHAR a, INT n)UNICODE: UNICODE(a)*n;
OP * = (INT n, UNICHAR a)UNICODE: n*UNICODE(a);

OP *:= = (REF UNICODE a, INT b)VOID: a := a * b;

# Wirthy Operators #
OP LT = (UNICHAR a,b)BOOL: ABS bits OF a LT ABS bits OF b,
   LE = (UNICHAR a,b)BOOL: ABS bits OF a LE ABS bits OF b,
   EQ = (UNICHAR a,b)BOOL: ABS bits OF a EQ ABS bits OF b,
   NE = (UNICHAR a,b)BOOL: ABS bits OF a NE ABS bits OF b,
   GE = (UNICHAR a,b)BOOL: ABS bits OF a GE ABS bits OF b,
   GT = (UNICHAR a,b)BOOL: ABS bits OF a GT ABS bits OF b;

# ASCII OPerators #
OP <  = (UNICHAR a,b)BOOL: a LT b,
   <= = (UNICHAR a,b)BOOL: a LE b,
    = = (UNICHAR a,b)BOOL: a EQ b,
   /= = (UNICHAR a,b)BOOL: a NE b,
   >= = (UNICHAR a,b)BOOL: a GE b,
   >  = (UNICHAR a,b)BOOL: a GT b;

# Non ASCII OPerators
OP ≤ = (UNICHAR a,b)BOOL: a LE b,
   ≠ = (UNICHAR a,b)BOOL: a NE b,
   ≥ = (UNICHAR a,b)BOOL: a GE b;
#

# Compare two UNICODE strings for equality #
PROC unicode cmp = (UNICODE str a,str b)INT: (

  IF LWB str a > LWB str b THEN exit lt ELIF LWB str a < LWB str b THEN exit gt FI;

  INT min upb = UPB(UPB str a < UPB str b | str a | str b );

  FOR i FROM LWB str a TO min upb DO
    UNICHAR a := str a[i], UNICHAR b := str b[i];
    IF a < b THEN exit lt ELIF a > b THEN exit gt FI
  OD;

  IF UPB str a > UPB str b THEN exit gt ELIF UPB str a < UPB str b THEN exit lt FI;

  exit eq:  0 EXIT
  exit lt: -1 EXIT
  exit gt:  1
);

OP LT = (UNICODE a,b)BOOL: unicode cmp(a,b)< 0,
   LE = (UNICODE a,b)BOOL: unicode cmp(a,b)<=0,
   EQ = (UNICODE a,b)BOOL: unicode cmp(a,b) =0,
   NE = (UNICODE a,b)BOOL: unicode cmp(a,b)/=0,
   GE = (UNICODE a,b)BOOL: unicode cmp(a,b)>=0,
   GT = (UNICODE a,b)BOOL: unicode cmp(a,b)> 0;

# ASCII OPerators #
OP <  = (UNICODE a,b)BOOL: a LT b,
   <= = (UNICODE a,b)BOOL: a LE b,
    = = (UNICODE a,b)BOOL: a EQ b,
   /= = (UNICODE a,b)BOOL: a NE b,
   >= = (UNICODE a,b)BOOL: a GE b,
   >  = (UNICODE a,b)BOOL: a GT b;

# Non ASCII OPerators
OP ≤ = (UNICODE a,b)BOOL: a LE b,
   ≠ = (UNICODE a,b)BOOL: a NE b,
   ≥ = (UNICODE a,b)BOOL: a GE b;
#

COMMENT - Todo: for all UNICODE and UNICHAR
  Add NonASCII OPerators: ×, ×:=,
  Add ASCII Operators: &, &:=, &=:
  Add Wirthy OPerators: PLUSTO, PLUSAB, TIMESAB for UNICODE/UNICHAR,
  Add UNICODE against UNICHAR comparison OPerators,
  Add char_in_string and string_in_string PROCedures,
  Add standard Unicode functions:
    to_upper_case, to_lower_case, unicode_block, char_count,
    get_directionality, get_numeric_value, get_type, is_defined,
    is_digit, is_identifier_ignorable, is_iso_control,
    is_letter, is_letter_or_digit, is_lower_case, is_mirrored,
    is_space_char, is_supplementary_code_point, is_title_case,
    is_unicode_identifier_part, is_unicode_identifier_start,
    is_upper_case, is_valid_code_point, is_whitespace
END COMMENT

test:(

  UNICHAR aircraft := U16r 2708;
  printf(($"aircraft: "$, $"16r"16rdddd$, UNICODE(aircraft), $g$, " => ", REPR UNICODE(aircraft), $l$));

  UNICODE chinese forty two = U16r 56db + U16r 5341 + U16r 4e8c;
  printf(($"chinese forty two: "$, $g$, REPR chinese forty two, ", length string = ", UPB chinese forty two, $l$));

  UNICODE poker = U "A123456789♥♦♣♠JQK";
  printf(($"poker: "$, $g$, REPR poker, ", length string = ", UPB poker, $l$));

  UNICODE selectric := U"×÷≤≥≠¬∨∧⏨→↓↑□⌊⌈⎩⎧○⊥¢";
  printf(($"selectric: "$, $g$, REPR selectric, $l$));
  printf(($"selectric*4: "$, $g$, REPR(selectric*4), $l$));

  print((
    "1 < 2 is ",  U"1" < U"2", ", ",
    "111 < 11 is ",U"111" < U"11", ", ",
    "111 < 12 is ",U"111" < U"12", ", ",
    "♥ < ♦ is ",  U"♥" < U"♦", ", ",
    "♥Q < ♥K is ",U"♥Q" < U"♥K", " & ",
    "♥J < ♥K is ",U"♥J" < U"♥K", new line
  ))

)
```

{{out}}

```txt

aircraft: 16r2708 => ✈
chinese forty two: 四十二, length string =          +3
poker: A123456789♥♦♣♠JQK, length string =         +17
selectric: ×÷≤≥≠¬∨∧⏨→↓↑□⌊⌈⎩⎧○⊥¢
selectric*4: ×÷≤≥≠¬∨∧⏨→↓↑□⌊⌈⎩⎧○⊥¢×÷≤≥≠¬∨∧⏨→↓↑□⌊⌈⎩⎧○⊥¢×÷≤≥≠¬∨∧⏨→↓↑□⌊⌈⎩⎧○⊥¢×÷≤≥≠¬∨∧⏨→↓↑□⌊⌈⎩⎧○⊥¢
1 < 2 is T, 111 < 11 is F, 111 < 12 is T, ♥ < ♦ is T, ♥Q < ♥K is F & ♥J < ♥K is T

```



## AutoHotkey

How easy is it to present Unicode strings in source code? - Simple, as long as the script is saved as Unicode and you're using a Unicode build

Can Unicode literals be written directly, or be part of identifiers/keywords/etc? - Yes, see above

How well can the language communicate with the rest of the world? Is it good at input/output with Unicode? - it can create GUI's and send Unicode characters.

Is it convenient to manipulate Unicode strings in the language? - Yes: they act like any other string, apart from lowlevel functions such as NumPut which deal with bytes.

How broad/deep does the language support Unicode? What encodings (e.g. UTF-8, UTF-16, etc) can be used? UTF-8 is most often used. StrPut/StrGet and FileRead/FileAppend allow unicode in AutoHotkey_L (the current build)


## AWK


How well prepared is the programming language for Unicode? - Not really prepared. AWK is a tool for manipulating ASCII input.

How easy is it to present Unicode strings in source code? - Easy. They can be represented in hexadecimal.

Can Unicode literals be written directly - No

or be part of identifiers/keywords/etc? - No

How well can the language communicate with the rest of the world? - The language is not good at communications, but can utilize external tools.

Is it good at input/output with Unicode? - No

Is it convenient to manipulate Unicode strings in the language? - No

How broad/deep does the language support Unicode? What encodings (e.g. UTF-8, UTF-16, etc) can be used? - There is no inbuilt support for Unicode, but all encodings can be represented through hexadecimal strings.


## BBC BASIC

{{works with|BBC BASIC for Windows}}
* How easy is it to present Unicode strings in source code?
As of version 5.94a, the ''BBC BASIC for Windows'' source code editor supports Unicode text in string literals and comments (remarks).  This includes bi-directional text and Arabic ligatures.
* Can Unicode literals be written directly?
If a suitable keyboard and/or Input Method Editor is available Unicode text may be entered directly into the editor.
* or be part of identifiers/keywords/etc?
Identifiers (variable names) and keywords cannot use Unicode characters.
* How well can the language communicate with the rest of the world? Is it good at input/output with Unicode?
Output of Unicode text to both the screen and the printer is supported, but must be enabled using a '''VDU 23,22''' command since the default output mode is ANSI.
The text printing direction can be set to right-to-left for languages such as Hebrew and Arabic.  Run-time support for Arabic ligatures is not built-in, but is provided by means of the FNarabic() function.  No specific support for Unicode input at run time is provided, although this is possible by means of Windows controls.
* Is it convenient to manipulate Unicode strings in the language?
The supported character encoding is UTF-8 which, being a byte stream, is compatible with most of the language's string manipulation functions.  However, the parameters in functions like '''LEFT$''' and '''MID$''' refer to byte counts rather than character counts.

'''Code example:'''
(whether this listing displays correctly will depend on your browser)

```bbcbasic
      VDU 23,22,640;512;8,16,16,128+8 : REM Select UTF-8 mode
      *FONT Times New Roman, 20

      PRINT "Arabic:"

      arabic1$ = "هنا مثال يمكنك من الكتابة من اليمين"
      arabic2$ = "الى اليسار باللغة العربية"

      VDU 23,16,2;0;0;0;13 : REM Select right-to-left printing
      PRINT FNarabic(arabic1$) ' FNarabic(arabic2$)
      VDU 23,16,0;0;0;0;13 : REM Select left-to-right printing

      PRINT '"Hebrew:"

      hebrew$ = "זוהי הדגמה של כתיבת טקסט בעברית מימין לשמאל"

      VDU 23,16,2;0;0;0;13 : REM Select right-to-left printing
      PRINT hebrew$
      VDU 23,16,0;0;0;0;13 : REM Select left-to-right printing

      END

      REM!Eject
      DEF FNarabic(A$)
      LOCAL A%, B%, L%, O%, P%, U%, B$
      A$ += CHR$0
      FOR A% = !^A$ TO !^A$+LENA$-1
        IF ?A%<&80 OR ?A%>=&C0 THEN
          L% = O% : O% = P% : P% = U%
          U% = ((?A% AND &3F) << 6) + (A%?1 AND &3F)
          IF ?A%<&80 U% = 0
          CASE TRUE OF
            WHEN U%=&60C OR U%=&61F: U% = 0
            WHEN U%<&622:
            WHEN U%<&626: U% = &01+2*(U%-&622)
            WHEN U%<&628: U% = &09+4*(U%-&626)
            WHEN U%<&62A: U% = &0F+4*(U%-&628)
            WHEN U%<&62F: U% = &15+4*(U%-&62A)
            WHEN U%<&633: U% = &29+2*(U%-&62F)
            WHEN U%<&63B: U% = &31+4*(U%-&633)
            WHEN U%<&641:
            WHEN U%<&648: U% = &51+4*(U%-&641)
            WHEN U%<&64B: U% = &6D+2*(U%-&648)
          ENDCASE
          IF P% IF P%<&80 THEN
            B% = P%
            IF O%=&5D IF P%<&5 B% += &74
            IF O%=&5D IF P%=&7 B% += &72
            IF O%=&5D IF P%=&D B% += &6E
            IF B%>P% B$=LEFT$(B$,LENB$-3) : O% = L%
            IF U% IF P%>7 IF P%<>&D IF P%<>&13 IF P%<>&29 IF P%<>&2B IF P%<>&2D IF P%<>&2F IF P%<>&6D IF P%<>&6F B% += 2
            IF O% IF O%>7 IF O%<>&D IF O%<>&13 IF O%<>&29 IF O%<>&2B IF O%<>&2D IF O%<>&2F IF O%<>&6D IF O%<>&6F B% += 1
            B$ = LEFT$(LEFT$(B$))+CHR$&EF+CHR$(&BA+(B%>>6))+CHR$(&80+(B%AND&3F))
          ENDIF
        ENDIF
        B$ += CHR$?A%
      NEXT
      = LEFT$(B$)
```

[[Image:unicode_bbc.gif]]


## Bracmat


* How easy is it to present Unicode strings in source code? Can Unicode literals be written directly, or be part of identifiers/keywords/etc?

The few keywords Bracmat knows are all ASCII. Identifiers and values can consist of all non-zero bytes, so UTF-8 encoded strings are allowed.
* How well can the language communicate with the rest of the world? Is it good at input/output with Unicode?
Input and output of UTF-8 encoded data and source code is easy. No special measures have to be taken. On reading HTML and JSON, hexcodes and HTML entities are converted to their UTF-8 equivalents.
* Is it convenient to manipulate Unicode strings in the language?
Yes, apart from counting characters, as UTF-8 has varying width. When converting a string to lower or uppercase, UTF-8 is assumed. If a string is not valid UTF-8, ISO-8859-1 (Latin-1) is assumed.
* How broad/deep does the language support Unicode? What encodings (e.g. UTF-8, UTF-16, etc) can be used?
Most UTF-16 and UTF-32 strings contain null bytes in non-final positions and can therefore not be handled easily.


## C

C is not the most unicode friendly language, to put it mildly.  Generally using unicode in C requires dealing with locales, manage data types carefully, and checking various aspects of your compiler.  Directly embedding unicode strings in your C source might be a bad idea, too; it's safer to use their hex values.  Here's a short example of doing the simplest string handling: print it.
```c
#include <stdio.h>
#include <stdlib.h>
#include <locale.h>

/* wchar_t is the standard type for wide chars; what it is internally
 * depends on the compiler.
 */
wchar_t poker[] = L"♥♦♣♠";
wchar_t four_two[] = L"\x56db\x5341\x4e8c";

int main() {
	/* Set the locale to alert C's multibyte output routines */
	if (!setlocale(LC_CTYPE, "")) {
		fprintf(stderr, "Locale failure, check your env vars\n");
		return 1;
	}

#ifdef __STDC_ISO_10646__
	/* C99 compilers should understand these */
	printf("%lc\n", 0x2708);	/* ✈ */
	printf("%ls\n", poker);		/* ♥♦♣♠ */
	printf("%ls\n", four_two);	/* 四十二 */
#else
	/* oh well */
	printf("airplane\n");
	printf("club diamond club spade\n");
	printf("for ty two\n");
#endif
	return 0;
}
```



## C#

In C#, the native string representation is actually determined by the Common Language Runtime. In CLR, the string data type is a sequence of char, and the char data type represents a UTF-16 code unit. The native string representation is essentially UTF-16, except that strings can contain sequences of UTF-16 code units that aren't valid in UTF-16 if the string contains incorrectly-used high and low surrogates.

C# string literals support the \u escape sequence for 4-digit hexadecimal Unicode code points, \U for 6-digit code points, and UTF-encoded source code is also supported so that "Unicode strings" can be included in the source code as-is.

C# benefits from the extensive support for Unicode in the .NET Base Class Library, including
* Various UTF encodings
* String normalization
* Unicode character database subset
* Breaking strings into text elements


## Common Lisp

Default unicode strings for most implementations. Unicode chars can be used on variable and function names.
Tested in SBCL 1.2.7 and ECL 13.5.1

```lisp

(defvar ♥♦♣♠ "♥♦♣♠")
(defun ✈ () "a plane unicode function")

```



## D


```D
import std.stdio;
import std.uni; // standard package for normalization, composition/decomposition, etc..
import std.utf; // standard package for decoding/encoding, etc...

void main() {
    // normal identifiers are allowed
    int a;
    // unicode characters are allowed for identifers
    int δ;

    char c;  // 1 to 4 byte unicode character
    wchar w; // 2 or 4 byte unicode character
    dchar d; // 4 byte unicode character

    writeln("some text");     // strings by default are UTF8
    writeln("some text"c);    // optional suffix for UTF8
    writeln("こんにちは"c);      // unicode charcters are just fine (stored in the string type)
    writeln("Здравствуйте"w); // also avaiable are UTF16 string  (stored in the wstring type)
    writeln("שלום"d);         // and UTF32 strings (stored in the dstring type)

    // escape sequences like what is defined in C are also allowed inside of strings and characters.
}
```



## DWScript

Source code is expected in Unicode (typically UTF-8 or UTF-16), characters above 127 (non-ASCII) are not part of the language, and are accepted literally as string characters or as identifier characters.

Characters in a string can also by specified explicitly by specifying the Unicode codepoint with a # followed by a decimal number, or a #$ followed by an hexadecimal codepoint (if the codepoint is outside the BMP, it'll result in two UTF-16 words). Contrarily to some other Pascal variants (like Delphi), explicit character codes are always and consistently interpreted as Unicode codepoints.

Strings are UTF-16.

## Elena

ELENA supports both UTF8 and UTF16 strings, Unicode identifiers are also supported:

ELENA 4.x:

```elena
public program()
{
    var 四十二 := "♥♦♣♠";     // UTF8 string
    var строка := "Привет"w;  // UTF16 string

    console.writeLine:строка;
    console.writeLine:四十二;
}
```

{{out}}

```txt

Привет
♥♦♣♠

```



## Elixir

Elixir has exceptionally good Unicode support in Strings. Its String module is fully compliant with the Unicode Standard, version 6.3.0. Internally, Strings are encoded in UTF-8. As source files are also typically Unicode encoded, String literals can be either written directly or via escape sequences. However, non-ASCII Unicode identifiers (variables, functions, ...) are not allowed.


## Erlang

The simplified explanation is that Erlang allows Unicode in comments/data/file names/etc, but not in function or variable names.


## Go

Go source code is specified to be UTF-8 encoded.
This directly allows any Unicode code point in character and string literals.
Unicode is also allowed in identifiers like variables and field names, with some restrictions.
The <code>string</code> data type represents a read-only sequence of bytes, conventionally but not necessarily representing UTF-8-encoded text.
A number of built-in features interpret <code>string</code>s as UTF-8.  For example,

```go
    var i int
    var u rune
    for i, u = range "voilà" {
        fmt.Println(i, u)
    }
```

{{out}}

```txt

0 118
1 111
2 105
3 108
4 224

```

224 being the Unicode code point for the à character.
Note <code>rune</code> is predefined to be a type that can hold a Unicode code point.

In contrast,

```go
    w := "voilà"
    for i := 0; i < len(w); i++ {
        fmt.Println(i, w[i])
    }

```

{{out}}

```txt

0 118
1 111
2 105
3 108
4 195
5 160

```

bytes 4 and 5 showing the UTF-8 encoding of à.
The expression <code>w[i]</code> in this case has the type of <code>byte</code> rather than <code>rune</code>.
A Go blog post covers this in more detail: [http://blog.golang.org/strings Strings, bytes, runes and characters in Go].

The heavily used standard packages <code>bytes</code> and <code>strings</code> both have functions for working with strings both as UTF-8 and as encoding-unspecified bytes.
The standard packages <code>unicode</code>, <code>unicode/utf8</code>, and <code>unicode/utf16</code> have additional functions.

Normalization support is available in the [[:Category:Go sub-repositories|sub-repository]] package <code>golang.org/x/text/unicode/norm</code>.
It contains a number of string manipulation functions that work with the four normalization forms NFC, NFD, NFKC, and NFKD.
The normalization form type in this package implements the <code>io.Reader</code> and <code>io.WriteCloser</code> interfaces to enable on-the-fly normalization during I/O.
A Go blog post covers this in more detail: [http://blog.golang.org/normalization Text normalization in Go].

There is no built-in or automatic handling of byte order marks (which are at best unnecessary with UTF-8).


## Haskell


Unicode is built-in in Haskell, so it can be used in strings and functions names.



## J


Unicode characters can be represented directly in J strings:


```j
   '♥♦♣♠'
♥♦♣♠
```


By default, they are represented as utf-8:


```j
   #'♥♦♣♠'
12
```


The above string requires 12 literal elements to represent the four characters using utf-8.

However, they can be represented as utf-16 instead:


```j
  7 u:'♥♦♣♠'
♥♦♣♠
  #7 u:'♥♦♣♠'
4
```


The above string requires 4 literal elements to represent the four characters using utf-16.  (7 u: string produces a utf-16 result.)

These forms are not treated as equivalent:


```j
   '♥♦♣♠' -: 7 u:'♥♦♣♠'
0
```


The utf-8 string of literals is a different string of literals from the utf-16 string.

unless the character literals themselves are equivalent:


```j
   'abcd'-:7 u:'abcd'
1
```


Here, we were dealing with ascii characters, so the four literals needed to represent the characters using utf-8 matched the four literals needed to represent the characters using utf-16.

When this is likely to be an issue, you should enforce a single representation.  For example:


```j
   '♥♦♣♠' -:&(7&u:) 7 u:'♥♦♣♠'
1
   '♥♦♣♠' -:&(8&u:) 7 u:'♥♦♣♠'
1
```


Here, we see that even when comparing non-ascii characters, we can coerce both arguments to be utf-8 or utf-16 and in either case the resulting literal strings match.  (8 u: string produces a utf-8 result.)

Output uses characters in whatever format they happen to be in.
Character input assumes 8 bit characters but places no additional interpretation on them.

See also: http://www.jsoftware.com/help/dictionary/duco.htm

Non-ascii unicode characters are not legal tokens or names, within current versions J.


## Java


How easy is it to present Unicode strings in source code?

Very easy. It is not specified what encoding the source code must be in, as long as it can be interpreted into a stream of UTF-16 characters. Most compilers probably take UTF-8.

In any case, even using only ASCII characters, any UTF-16 character can be embedded into the source code by using a Unicode escape <code>\uxxxx</code> (where ''xxxx'' is the hex code of the character), which is processed before any other steps by the compiler. This means that it is possible to write an entire program out of Unicode escapes. This also means that a Unicode escape could mess up the language syntax, if it happens to be the escape of a whitespace or quote character (please don't do that).

Can Unicode literals be written directly, or be part of identifiers/keywords/etc?

UTF-16 characters can be written directly in character and string literals and comments (there is no difference between "directly" and using Unicode escapes, since they are processed at the first step). UTF-16 characters can be part of identifiers (either directly or through a Unicode escape).

How well can the language communicate with the rest of the world? Is it good at input/output with Unicode?

Yes

Is it convenient to manipulate Unicode strings in the language?

The <code>String</code> class in Java is basically a sequence of <code>char</code> elements, representing the string encoded in UTF-16. <code>char</code> is a 16-bit type, and thus one <code>char</code> does not necessarily correspond to one Unicode character, since supplementary characters can have code points greater than U+FFFF. However, if your string only consists of characters from the Basic Multilingual Plane (which is most of the time), then one <code>char</code> does correspond to one Unicode character.

Starting in J2SE 5 (1.5), Java has fairly convenient methods for dealing with true Unicode characters, even supplementary ones. Many methods that deal with characters have versions for both <code>char</code> and <code>int</code>. For example, <code>String</code> has the <code>codePointAt</code> method, analogous to the <code>charAt</code> method.

How broad/deep does the language support Unicode? What encodings (e.g. UTF-8, UTF-16, etc) can be used? Normalization?


## jq

jq's data types are JSON's data types.  In particular, jq strings are UTF-8 strings. See [http://json.org json.org] for further details.

jq identifiers, however, are restricted to a subset of ASCII.

The jq command does have several options in support of flexibility when "communicating with the world":
  --raw-input    | -R :: each line of input is converted to a JSON string;
  --ascii-output | -a :: every non-ASCII character that would otherwise
                         be sent to output is translated to an equivalent
                         ASCII escape sequence;
  --raw-output   | -r :: output strings as raw strings, e.g. "a\nb" is
                         output as:

 a
 b


## Julia

Non-ASCII strings in Julia are UTF8-encoded by default, and Unicode identifiers are also supported:

```Julia>julia
 四十二 = "voilà";
julia> println(四十二)
voilà
```

And you can also specify unicode characters by ordinal:

```Julia>julia
println("\u2708")
✈
```



## Kotlin

In the version of Kotlin targetting the JVM, Kotlin strings are mapped to Java strings and so everything that has already been said in the Java entry for this task applies equally to Kotlin.

I would only add that normalization of strings is supported in both languages via the java.text.Normalizer class.

Here's a simple example of using both unicode identifiers and unicode strings in Kotlin:

```scala
// version 1.1.2

fun main(args: Array<String>) {
    val åäö = "as⃝df̅ ♥♦♣♠ 頰"
    println(åäö)
}
```


{{out}}

```txt

as⃝df̅ ♥♦♣♠ 頰

```



## Langur

Source code in langur is UTF-8 with no BOM allowed.

Comments and string literals may use Unicode.

For clarity, identifiers are ASCII only.

Indexing on a string indexes by code point. The index may be a single number, a range, or an array of such things. Indexing returns another string.

Conversion between code point numbers and strings can be done with the cpToString() and stringToCp() functions. The stringToCp() function accepts a single index number or range, returning a single code point number or an array of them. The cpToString() function accepts a single code point or an array.

The len() function returns the number of code points in a string.

Normalization can be handled with the functions nfc(), nfd(), nfkc(), and nfkd().

See langurlang.org for more details.


## Lasso

All string data in Lasso is processed as double-byte Unicode characters. Any input is assumed to be UTF-8 if not otherwise told.
All output is UTF-8 unless specified to a different encoding.
You can specify unicode characters by ordinal.

Variable names can not contain anything but ASCII.


```Lasso
local(unicode = '♥♦♣♠')
#unicode -> append('\u9830')
#unicode
'<br />'
#unicode -> get (2)
'<br />'
#unicode -> get (4) -> integer
```

{{out}}

```txt
♥♦♣♠頰
♦
9824
```



## LFE


As with Erlang, LFE allows Unicode in comments/data/file names/etc, but not in function or variable names.

Here is example UFT-8 encoding:

```lisp

> (set encoded (binary ("åäö ð" utf8)))
#B(195 165 195 164 195 182 32 195 176)

```


Display it in native Erlang format:


```lisp

> (io:format "~tp~n" (list encoded))
<<"åäö ð"/utf8>>

```


Example UFT-8 decoding:

```lisp

> (unicode:characters_to_list encoded 'utf8)
"åäö ð"

```



## Lingo

In recent versions (since v11.5) of Lingo's only implementation "Director" UTF-8 is the default encoding for both scripts and strings. Therefor Unicode string literals can be specified directly in the code, and also variable names support Unicode. To represent/deal with string data in other encodings, you have to use the ByteArray data type. Various ByteArray as well as FileIO methods support an optional 'charSet' parameter that allows to transcode data to/from UTF-8 on the fly. The supported 'charSet' strings can be displayed like this:

```lingo
put _system.getInstalledCharSets()
-- ["big5", "cp1026", "cp866", "ebcdic-cp-us", "gb2312", "ibm437", "ibm737",
"ibm775", "ibm850", "ibm852", "ibm857", "ibm861", "ibm869", "iso-8859-1",
"iso-8859-15", "iso-8859-2", "iso-8859-4", "iso-8859-5", "iso-8859-7",
"iso-8859-9", "johab", "koi8-r", "koi8-u", "ks_c_5601-1987", "macintosh",
"shift_jis", "us-ascii", "utf-16", "utf-16be", "utf-7", "utf-8", "windows-1250",
"windows-1251", "windows-1252", "windows-1253", "windows-1254", "windows-1255",
"windows-1256", "windows-1257", "windows-1258", "windows-874",
"x-ebcdic-greekmodern", "x-mac-ce", "x-mac-cyrillic", "x-mac-greek",
"x-mac-icelandic", "x-mac-turkish"]
```



## Locomotive Basic


The Amstrad CPC464 does not have native Unicode support. It is possible to represent Unicode by using ASCII based hexadecimal number sequences, or by using a form of escape sequence encoding, such as \uXXXX. However, there is only 48k of memory available and 510k would be needed to store the Unicode characters for display, so Unicode is not really viable on this platform.

*  How well prepared is the programming language for Unicode? - Not good. There are no Unicode symbols in the ROM.

* How easy is it to present Unicode strings in source code? - Easy, they are in hexadecimal.

* Can Unicode literals be written directly - No

* or be part of identifiers/keywords/etc? - No

* How well can the language communicate with the rest of the world? - Not good. There is no TCP/IP stack, and the computer does not have an Ethernet port.

* Is it good at input/output with Unicode? - Not good. There are no Unicode symbols in ROM, or on the keyboard.

* Is it convenient to manipulate Unicode strings in the language? - Moderate. The language is not designed for Unicode, so has no inbuilt Unicode functions. However, it is possible to write manipulation routines, and the language is good at arithmetic, so no problem.

* How broad/deep does the language support Unicode? What encodings (e.g. UTF-8, UTF-16, etc) can be used? There is no inbuilt support for Unicode, but all encodings can be represented through hexadecimal strings.

It should be added however that the character set can be easily redefined from BASIC with the SYMBOL and SYMBOL AFTER commands, so the CPC character set can be turned into e.g. Latin-1. As two-byte UTF-8 characters can be converted to Latin-1, at least a subset of Unicode can be printed in this way:


```locobasic
10 CLS:DEFINT a-z
20 ' define German umlauts as in Latin-1
30 SYMBOL AFTER 196
40 SYMBOL 196,&66,&18,&3C,&66,&7E,&66,&66,&0
50 SYMBOL 214,&C6,&0,&7C,&C6,&C6,&C6,&7C,&0
60 SYMBOL 220,&66,&0,&66,&66,&66,&66,&3C,&0
70 SYMBOL 228,&6C,&0,&78,&C,&7C,&CC,&76,&0
80 SYMBOL 246,&66,&0,&0,&3C,&66,&66,&3C,&0
90 SYMBOL 252,&66,&0,&0,&66,&66,&66,&3E,&0
100 SYMBOL 223,&38,&6C,&6C,&78,&6C,&78,&60,&0
110 ' print string
120 READ h
130 IF h=0 THEN 180
140 IF (h AND &X11100000)=&X11000000 THEN uc=(h AND &X11111)*2^6:GOTO 120
150 IF (h AND &X11000000)=&X10000000 THEN uc=uc+(h AND &X111111):h=uc
160 PRINT CHR$(h);
170 GOTO 120
180 PRINT
190 END
200 ' zero-terminated UTF-8 string
210 DATA &48,&C3,&A4,&6C,&6C,&C3,&B6,&20,&4C,&C3,&BC,&64,&77,&69,&67,&2E,&20,&C3,&84,&C3,&96,&C3,&9C
220 DATA &20,&C3,&A4,&C3,&B6,&C3,&BC,&20,&56,&69,&65,&6C,&65,&20,&47,&72,&C3,&BC,&C3,&9F,&65,&21,&00
```


Produces this (slightly nonsensical) output:

[[File:Unicode print locomotive basic.png]]

## M2000 Interpreter

* How easy is it to present Unicode strings in source code?

We copy them in M2000 editor. Internal M2000 use UTF16LE but programs saved in UTF-8 format.

* Can Unicode literals be written directly, or be part of identifiers/keywords/etc?

Yes

* How well can the language communicate with the rest of the world?

GUI support unicode. We can use filenames with names from any language. Text files are UTF-16LE or Ansi (we can use WIDE for specify UNICODE, also we can specify the locale for ANSI), and we can load/Save Document object using UTF-8, UTF-16LE, UTF-16BE and ANSI (we can specify the locale). Clipboard is unicode also.

* Is it good at input/output with Unicode?

If we use proportional text we are ok, byt simple text output/input to console break each word to letter and then send it to console, so we get always a left to right output. We can use diacritical marks combining to same letter.

* Is it convenient to manipulate Unicode strings in the language?

Strings are same as Visual Basic 6 strings (M2000 Interpreter written in VB6). We can get the length of a string as display length wich calculate diacritical marks. We can make strings as json type, using same symbols to represent unicode letters (we can do the reverse, to produce from unicode the proper json string)

* How broad/deep does the language support Unicode?

From variables/keys to files and screen/printer output. Also we can use external COM objects using unicode strings,

* What encodings (e.g. UTF-8, UTF-16, etc) can be used?

A string may contain a one byte char array or a two byte char array. The Len() function always return the two byte length, so 3 bytes string return a length of 1.5. Encoding is not bound to string but with function which process the string. So there are functions to process in UTF-16LE, other to process in ANSI, and one function for conversions from and to UTF-8. UTF-16BE supported only for loading/Saving a document object (internal is always in UTF16-LE)

* Does it support normalization?

No. A string may contain any value including zero. Max size is 2GB. Also we can make strings in buffers with specific length, and any value.



```M2000 Interpreter

Font "Arial"
Mode 32
' M2000 internal editor can display  left to rigtht languages if text is in same line, and same color.
a$="لم أجد هذا الكتاب القديم"
' We can use console to display text, using proportional spacing
Print Part $(4), a$
Print
' We can display right to left using
' the legend statement which render text at a given
' graphic  point, specify the font type and font size
' and optional:
' rotation angle, justification (1 for right,2 for center, 3 for left)
'quality (0 or non 0, which 0 no antialliasing)
' letter spacing in twips (not good for arabic language)
move 6000,6000
legend  a$, "Arial", 32, pi/4, 2, 0
' Variables can use any unicode letter.
' Here we can't display it as in M2000 editor.
' in the editor we see at the left the variable name
' and at the right the value
القديم=10
Print القديم+1=11  ' true

```



## Mathematica

Mathematica supports full Unicode throughout -- in strings, symbols, graphics and external operations -- allowing immediate streamlined use of all standard international character sets, integrated with native text entry.
The global variable $CharacterEncodings is an option for input and output functions which specifies what raw character encoding should be used.


```txt
{"AdobeStandard", "ASCII", "CP936", "CP949", "CP950", "Custom",
"EUC-JP", "EUC", "IBM-850", "ISO10646-1", "ISO8859-15", "ISO8859-1",
"ISO8859-2", "ISO8859-3", "ISO8859-4", "ISO8859-5", "ISO8859-6",
"ISO8859-7", "ISO8859-8", "ISO8859-9", "ISOLatin1", "ISOLatin2",
"ISOLatin3", "ISOLatin4", "ISOLatinCyrillic", "Klingon", "KOI8-R",
"MacintoshArabic", "MacintoshChineseSimplified",
"MacintoshChineseTraditional", "MacintoshCroatian",
"MacintoshCyrillic", "MacintoshGreek", "MacintoshHebrew",
"MacintoshIcelandic", "MacintoshKorean",
"MacintoshNonCyrillicSlavic", "MacintoshRomanian", "MacintoshRoman",
"MacintoshThai", "MacintoshTurkish", "MacintoshUkrainian", "Math1",
"Math2", "Math3", "Math4", "Math5", "Mathematica1", "Mathematica2",
"Mathematica3", "Mathematica4", "Mathematica5", "Mathematica6",
"Mathematica7", "PrintableASCII", "ShiftJIS", "Symbol", "Unicode",
"UTF8", "WindowsANSI", "WindowsBaltic", "WindowsCyrillic",
"WindowsEastEurope", "WindowsGreek", "WindowsThai", "WindowsTurkish",
"ZapfDingbats"}
```



## Nemerle

How easy is it to present Unicode strings in source code? '''Very; they can be input directly, or as 'u####' character literals when needed.'''

Can Unicode literals be written directly, or be part of identifiers/keywords/etc? '''Yes; identifiers, literals and such can be written directly as UTF8 strings.'''

How well can the language communicate with the rest of the world? Is it good at input/output with Unicode? '''Nemerle plays well with the 'rest of the world' (it was developed in Poland and most of its user-base is Polish or Russian, so the 'rest of the world' from the language developers/users perspective is different than that typically envisioned by an Anglophone.)
Input/output in UTF8 is handled readily, other encodings, text directions and such are handled by classes in the <tt>System.Text</tt> and <tt>System.Globalization</tt> namespaces. See [http://msdn.microsoft.com/en-us/library/h6270d0z.aspx this] MSDN page for recommendations on globalization/localization of applications.'''

Is it convenient to manipulate Unicode strings in the language? '''Yes; string methods expect UTF8 strings'''

How broad/deep does the language support Unicode? What encodings (e.g. UTF-8, UTF-16, etc) can be used? Normalization? '''Using the standard .NET libraries, there is a lot of support for unicode string manipulation. For example, normalization is accomplished by simply calling the Normalize() method on a string.'''


## Nim

Strings are assumed to be UTF-8 in Nim.

```nim
let c = "abcdé"
let Δ = 12
let e = "$abcde¢£¤¥©ÇßçĲĳŁłʒλπ•₠₡₢₣₤₥₦₧₨₩₪₫€₭₮₯₰₱₲₳₴₵₵←→⇒∙⌘☺☻ア字文𪚥"
echo e
```



## Oforth


In Oforth, all strings are UT8 strings.

\Uxxxx (with xxxx an hexa number) can be used into literals to represent character with xxxx as its unicode.

All methods on strings are UTF8 manipulations.


## Perl

In Perl, "Unicode" means "UTF-8".  If you want to include utf8 characters in your source file, unless you have set <code>PERL_UNICODE</code> environment correctly, you should do
```Perl>use utf8;</lang
 or you risk the parser treating the file as raw bytes.

Inside the script, utf8 characters can be used both as identifiers and literal strings, and built-in string functions will respect it:
```Perl
$四十二 = "voilà";
print "$四十二";				# voilà
print uc($四十二);			# VOILÀ
```

or you can specify unicode characters by name or ordinal:
```Perl
use charnames qw(greek);
$x = "\N{sigma} \U\N{sigma}";
$y = "\x{2708}";
print scalar reverse("$x $y");		# ✈ Σ σ
```


Regular expressions also have support for unicode based on properties, for example, finding characters that's normally written from right to left:
```Perl
print "Say עִבְרִית" =~ /(\p{BidiClass:R})/g;	# עברית
```


When it comes to IO, one should specify whether a file is to be opened in utf8 or raw byte mode:
```Perl
open IN, "<:utf8", "file_utf";
open OUT, ">:raw", "file_byte";
```

The default of IO behavior can also be set in <code>PERL_UNICODE</code>.

However, when your program interacts with the environment, you may still run into tricky spots if you have incompatible locale settings or your OS is not using unicode; that's not what Perl has control over, unfortunately.


## Perl 6

Perl 6 programs and strings are all in Unicode and operate at a grapheme abstraction level, which is agnostic to underlying encodings or normalizations.  (These are generally handled at program boundaries.)  Opened files default to UTF-8 encoding. All Unicode character properties are in play, so any appropriate characters may be used as parts of identifiers, whitespace, or user-defined operators.  For instance:


```perl6
sub prefix:<∛> (\𝐕) { 𝐕 ** (1/3) }
say ∛27;  # prints 3
```


Non-Unicode strings are represented as Buf types rather than Str types, and Unicode operations may not be applied to Buf types without some kind of explicit conversion.  Only ASCIIish operations are allowed on buffers.

As things develop, Perl 6 intends to support Unicode even better than Perl 5, which already does a great job in recent versions of accessing nearly all Unicode 6.0 functionality.  Perl 6 improves on Perl 5 primarily by offering explicitly typed strings that always know which operations are sensical and which are not.


## Phix

Source code files can be ansi or UTF-8.

Strings containing the escape sequences \xHH, \uHHHH, or \UHHHHHHHH are permitted, with 16 and 32-bit unicode points converted to a UTF-8 substring.

Standard cross-platform routines are provided to convert between UTF-8, UTF-16, and UTF-32.

Phix contains a wrapper for/relies on IUP, and while I know that can display UTF-8, I don't know the current status of input, right-to-left, etc.

String manipulation should be very straightforward, especially if you convert to UTF-32 for any character-by-character stuff.

There are however no routines yet for normalization, collation, capitalisation, classification (eg digit/letter/..), etc.

See also [[Unicode_variable_names#Phix|Unicode_variable_names]], and demo\HelloUTF8.exw for some examples/tests.


## PicoLisp

PicoLisp can directly handle _only_ Unicode (UTF-8) strings. So the problem is rather how to handle non-Unicode strings: They must be pre- or post-processed by external tools, typically with pipes during I/O. For example, to read a line from a file in 8859 encoding:

```PicoLisp
(in '(iconv "-f" "ISO-8859-15" "file.txt") (line))
```



## Python

Python supports writing Unicode literals in any encoding, but you have to declare the encoding being used. This is done by including a special comment as either the first or second line of the source file:

```Python
#!/usr/bin/env python
# -*- coding: latin-1 -*-

u = 'abcdé'
print(ord(u[-1]))
```

In Python 3, the default encoding is UTF-8. Before that it was ASCII.

For more info on Unicode in Python, see its [http://docs.python.org/howto/unicode.html how-to].


## Racket



```Racket

#lang racket

;; Unicode in strings, using ascii
"\u03bb" ; -> "λ"
;; and since Racket source code is read in UTF-8, Unicode can be used
;; directly
"λ" ; -> same

;; The same holds for character constants
#\u3bb ; -> #\λ
#\λ    ; -> same

;; And of course Unicode can be used in identifiers,
(define √ sqrt)
(√ 256) ; -> 16
;; and in fact the standard language makes use of some of these
(λ(x) x) ; -> an identity function

```


Further points:

* IO of strings is the same as IO for code -- using UTF-8

* Manipulation of Unicode strings is easy; Racket uses UCS-4 for representing strings at runtime (which doesn't affect users)

* Racket includes additional related functionality, like some Unicode functions (normalization etc), and IO encoding based on iconv to do IO of many other encodings.


## Ruby

Ruby focuses on encodings (exactly 100 encodings are supported in Ruby 2.1.0). It includes pretty much all known Unicode Transformation Formats, including UTF-8 which is the default encoding since 2.1.0 .

Most Unicode support is to be found in the Regexp engine, for instance /\p{Sc}/ matches everything from the Symbol: Currency category;  \p{} matches a character’s Unicode script, like /\p{Linear_B}/.

Unicode strings are no problem:


```ruby
str = "你好"
str.include?("好") # => true
```


Unicode code is no problem either:


```ruby
def Σ(array)
  array.inject(:+)
end

puts Σ([4,5,6]) #=>15

```

Ruby 2.2 introduced a method to normalize unicode strings:

```ruby

p bad = "¿como\u0301 esta\u0301s?" # => "¿comó estás?"
p bad.unicode_normalized?          # => false
p bad.unicode_normalize!           # => "¿comó estás?"
p bad.unicode_normalized?          # => true

```


Since Ruby 2.4 Ruby strings have full Unicode case mapping.


## Scala

{{libheader|Scala}}

```scala
object UTF8 extends App {

  def charToInt(s: String) = {
    def charToInt0(c: Char, next: Char): Option[Int] = (c, next) match {
      case _ if (c.isHighSurrogate && next.isLowSurrogate) =>
        Some(java.lang.Character.toCodePoint(c, next))
      case _ if (c.isLowSurrogate) => None
      case _                       => Some(c.toInt)
    }

    if (s.length > 1) charToInt0(s(0), s(1)) else Some(s.toInt)
  }

  def intToChars(n: Int) = java.lang.Character.toChars(n).mkString

  println('\uD869'.isHighSurrogate + " " + '\uDEA5'.isLowSurrogate)

  println(charToInt("\uD869\uDEA5"))

  val b = "\uD869\uDEA5"
  println(b)

  val c = "\uD834\uDD1E"
  println(c)

  val a = "$abcde¢£¤¥©ÇßçĲĳŁłʒλπ•₠₡₢₣₤₥₦₧₨₩₪₫€₭₮₯₰₱₲₳₴₵₵←→⇒∙⌘☺☻ア字文𪚥".
    map(c => "%s\t\\u%04X".format(c, c.toInt)).foreach(println)
}
```

{{out}}
<pre style="height:20ex;overflow:scroll">true true
Some(173733)
𪚥
𝄞
$	\u0024
a	\u0061
b	\u0062
c	\u0063
d	\u0064
e	\u0065
¢	\u00A2
£	\u00A3
¤	\u00A4
¥	\u00A5
©	\u00A9
Ç	\u00C7
ß	\u00DF
ç	\u00E7
Ĳ	\u0132
ĳ	\u0133
Ł	\u0141
ł	\u0142
ʒ	\u0292
λ	\u03BB
π	\u03C0
•	\u2022
₠	\u20A0
₡	\u20A1
₢	\u20A2
₣	\u20A3
₤	\u20A4
₥	\u20A5
₦	\u20A6
₧	\u20A7
₨	\u20A8
₩	\u20A9
₪	\u20AA
₫	\u20AB
€	\u20AC
₭	\u20AD
₮	\u20AE
₯	\u20AF
₰	\u20B0
₱	\u20B1
₲	\u20B2
₳	\u20B3
₴	\u20B4
₵	\u20B5
₵	\u20B5
←	\u2190
→	\u2192
⇒	\u21D2
∙	\u2219
⌘	\u2318
☺	\u263A
☻	\u263B
ア	\u30A2
字	\u5B57
文	\u6587
?	\uD869
?	\uDEA5
	\uF8FF

```



## Seed7

The Unicode encoding of Seed7 [http://seed7.sourceforge.net/manual/types.htm#char characters] and
[http://seed7.sourceforge.net/manual/types.htm#string strings] is UTF-32. Seed7 source files use
UTF-8 encoding. [http://seed7.sourceforge.net/manual/tokens.htm#Character_literals Character literals] and
[http://seed7.sourceforge.net/manual/tokens.htm#String_literals string literals] are
therefore written with UTF-8 encoding. Unicode characters are allowed in comments,
but not in identifiers and keywords. Functions, which send strings to the operating system convert
them to the encoding used by the OS. Strings received by the operating system are converted to UTF-32.
Seed7 supports reading and writing [http://seed7.sourceforge.net/libraries/external_file.htm Latin-1],
[http://seed7.sourceforge.net/libraries/utf8.htm UTF-8] and
[http://seed7.sourceforge.net/libraries/utf16.htm UTF-16] files.
Because of UTF-32 there is no distinction between byte and character position.


## Sidef

Sidef use UTF-8 encoding for pretty much everything, such as source files, chars, strings, stdout, stderr and stdin.

```ruby
# International class; name and street
 class 国際( なまえ, Straße ) {

    # Say who am I!
    method 言え {
        say "I am #{self.なまえ} from #{self.Straße}";
    }
}

 # all the people of the world!
 var 民族 = [
              国際( "高田　Friederich", "台湾" ),
              国際( "Smith Σωκράτης", "Cantù" ),
              国際( "Stanisław Lec", "południow" ),
            ];

民族.each { |garçon|
    garçon.言え;
}
```

{{out}}

```txt

I am 高田　Friederich from 台湾
I am Smith Σωκράτης from Cantù
I am Stanisław Lec from południow

```



## Tcl

All characters in Tcl are ''always'' Unicode characters, with ordinary string operations (as listed elsewhere on Rosetta Code) always performed on Unicode.
Input and output characters are translated from and to the system's native encoding automatically (with this being able to be overridden on a per file-handle basis via <code>fconfigure -encoding</code>).
Source files can be written in encodings other than the native encoding — from Tcl 8.5 onwards, the encoding to use for a file can be controlled by the <code>-encoding</code> option to [[tclsh]], [[wish]] and <code>source</code> — though it is usually recommended that programmers maximize their portability by writing in the ASCII subset and using the <code>\uXXXX</code> escape sequence for all other characters.
Tcl does ''not'' handle byte-order marks by default, because that requires deeper understanding of the application level (and sometimes the encoding information is available in metadata anyway, such as when handling HTTP connections).

The way in which characters are encoded in memory is not defined by the Tcl language (the implementation uses byte arrays, UTF-16 arrays and UCS-2 strings as appropriate) and the only characters with any restriction on use as command or variable names are the ASCII parenthesis and colon characters.
However, the <code>$var</code> shorthand syntax is much more restricted (to ASCII alphanumeric plus underline only); other cases have to use the more verbose form: <code>[set funny–var–name]</code>.


## TXR


TXR source code and I/O are all assumed to be text which is UTF-8 encoded.
This is a self-contained implementation, not relying on any encoding library.
TXR ignores LANG and such environment variables.

One of the regression test cases uses Japanese text.

Characters can be coded directly, or encoded indirectly with hexadecimal escape sequences.

The regular expression engine, also an original implementation, self-contained within TXR, supports full Unicode (not only the Basic Multilingual Plane, but all planes).

However, as of version 89, identifiers such as variables are restricted to English letters, numbers and underscores.

Whether or not text outside of the Basic Multilingual Plane can actually be represented by a given port of TXR depends on the width of the C compiler's wchar_t type. A 16 bit wchar_t restricts the program to the BMP.

Japanese test case:

```TXR
@{TITLE /[あ-ん一-耙]+/} (@ROMAJI/@ENGLISH)
@(freeform)
@(coll)@{STANZA /[^\n\x3000 ]+/}@(end)@/.*/

```


Test data: Japanese traditional song:


```txt
春が来た (Haru-ga Kita/Spring has Come)

春が来た　春が来た　どこに来た
山に来た　里に来た　野にも来た

花が咲く　花が咲く　どこに咲く
山に咲く　里に咲く　野にも咲く

鳥がなく　鳥がなく　どこでなく
山でなく　里でなく　野でもなく

```


Expected output (with <code>txr -B</code>):


```txt
TITLE="春が来た"
ROMAJI="Haru-ga Kita"
ENGLISH="Spring has Come"
STANZA[0]="春が来た"
STANZA[1]="春が来た"
STANZA[2]="どこに来た"
STANZA[3]="山に来た"
STANZA[4]="里に来た"
STANZA[5]="野にも来た"
STANZA[6]="花が咲く"
STANZA[7]="花が咲く"
STANZA[8]="どこに咲く"
STANZA[9]="山に咲く"
STANZA[10]="里に咲く"
STANZA[11]="野にも咲く"
STANZA[12]="鳥がなく"
STANZA[13]="鳥がなく"
STANZA[14]="どこでなく"
STANZA[15]="山でなく"
STANZA[16]="里でなく"
STANZA[17]="野でもなく"

```



## UNIX Shell


The Bourne shell does not have any inbuilt Unicode functionality.
However, Unicode can be represented as ASCII based hexadecimal number sequences, or by using form of escape sequence encoding, such as \uXXXX.
The shell will produce its output in ASCII, but can call other programs to produce the Unicode output.
The shell does not have any inbuilt string manipulation utilities, so uses external tools such as cut, expr, grep, sed and awk. These would typically manipulate the hexadecimal sequences to provide string manipulation, or dedicated Unicode based tools could be used.

* How well prepared is the programming language for Unicode? - Fine. All Unicode strings can be represented as hexadecimal sequences.

* How easy is it to present Unicode strings in source code? - Easy, they are in hexadecimal.

* Can Unicode literals be written directly - No

* or be part of identifiers/keywords/etc? - No

* How well can the language communicate with the rest of the world? - Extremely well. The shell makes use of all of the tools that a Unix box has to offer.

* Is it good at input/output with Unicode? - This language is weak on input/output anyway, so its Unicode input/output is also weak. However, the shell makes use of all installed tools, so this is not a problem in real terms.

* Is it convenient to manipulate Unicode strings in the language? - Not really, the shell is not good at string manipulation. Howver, it makes good use of external programs, so Unicode string manipulation should not be a problem.

* How broad/deep does the language support Unicode? There is no inbuilt support for Unicode, but all encodings can be represented through hexadecimal strings.


## Vala


Vala strings are UTF-8 encoded by default. In order to print them correctly on the screen, use stdout.printf instead of print.

```vala
stdout.printf ("UTF-8 encoded string. Let's go to a café!");
```



## Visual Basic .NET

See the C# for some general information about the .NET runtime.
Below is an example of certain parts based of the information in the D entry.
<lang>Module Module1

    Sub Main()
        Console.OutputEncoding = Text.Encoding.Unicode

        ' normal identifiers allowed
        Dim a = 0
        ' unicode characters allowed
        Dim δ = 1

        ' ascii strings
        Console.WriteLine("some text")
        ' unicode strings strings
        Console.WriteLine("こんにちは")
        Console.WriteLine("Здравствуйте")
        Console.WriteLine("שלום")
        ' escape sequences
        Console.WriteLine(vbTab + "text" + vbTab + ChrW(&H2708) + """blue")
        Console.ReadLine()
    End Sub

End Module
```

{{out}}

```txt
some text
こんにちは
Здравствуйте
שלום
	text	✈"blue
```



## WDTE


WDTE supports Unicode in both identifiers and strings. WDTE is very loose about identifier rules. If it doesn't conflict with a syntactic structure, such as a keyword, literal, or operator, than it's allowed as an identifier.


```WDTE
let プリント t => io.writeln io.stdout t;

プリント 'これは実験です。';
```



## zkl

zkl doesn't do unicode; all string handling is 8 bit ASCIIZ. The language source is a subset of ASCII. Then again, it doesn't care if you use UTF-8 and provides "\uXXXX" and "\Ux*;" string escapes for up to 31 bit Unicode characters. You can verify a string is valid UTF-8 by using the string len method: "text".len(8). Future support would be in the form of extension libraries that add objects for whatever Unicode form (32 bit, 16 bit, UTF-8, etc) is desired. zkl is designed for this type of extension.
*Unicode in source: UTF-8 in strings. If your terminal/editor supports UTF-8, you can use the glyphs directly, otherwise, use string escapes to represent the Unicode characters.
*Unicode identifiers, etc? No
*Communications with the rest of the world: Byte streams plus methods to change endianness.
*Can zkl manipulate Unicode? PITA
*Is there any Unicode support? Only encoding/decoding UTF-8.
*Future support? If somebody writes the libraries. For example, it is straight forward to write a front end to PCRE and thus acquire a Unicode aware regular expression engine.


## ZX Spectrum Basic


The ZX Spectrum does not have native Unicode support. However, it does support user defined graphics, which makes it is possible to create custom characters in the UDG area. It is possible to represent Unicode by using ASCII based hexadecimal number sequences, or by using a form of escape sequence encoding, such as \uXXXX. However, there is only 48k of memory available on a traditional rubber key ZX Spectrum, (or 128k on some of the plus versions), and 510k would be needed to store the Unicode characters for display, so Unicode is not really viable on this platform.

*  How well prepared is the programming language for Unicode? - Not good. There are no Unicode symbols in the ROM.

* How easy is it to present Unicode strings in source code? - Easy, they are in hexadecimal.

* Can Unicode literals be written directly - No

* or be part of identifiers/keywords/etc? - No

* How well can the language communicate with the rest of the world? - Not good. There is no TCP/IP stack, and the computer does not have an Ethernet port.

* Is it good at input/output with Unicode? - Not good. There are no Unicode symbols in ROM, or on the keyboard.

* Is it convenient to manipulate Unicode strings in the language? - Moderate. The language is not designed for Unicode, so has no inbuilt Unicode functions. However, it is possible to write manipulation routines, and the language is good at arithmetic, so no problem.

* How broad/deep does the language support Unicode? What encodings (e.g. UTF-8, UTF-16, etc) can be used? There is no inbuilt support for Unicode, but all encodings can be represented through hexadecimal strings. A decoder and output routine would need to be written, but this is easy to do on the Spectrum.

{{omit from|GUISS}}

[[Category:Unicode]]
