+++
title = "Soundex"
description = ""
date = 2019-08-10T23:39:33Z
aliases = []
[extra]
id = 4966
[taxonomies]
categories = ["Text processing", "String algorithms", "task"]
tags = []
languages = [
  "360_assembly",
  "ada",
  "algol_68",
  "ansi_standard_basic",
  "autohotkey",
  "awk",
  "bbc_basic",
  "befunge",
  "c",
  "clipper_xbase",
  "clojure",
  "cobol",
  "common_lisp",
  "cpp",
  "crystal",
  "csharp",
  "d",
  "delphi",
  "elixir",
  "erlang",
  "factor",
  "forth",
  "futurebasic",
  "go",
  "groovy",
  "haskell",
  "j",
  "java",
  "javascript",
  "julia",
  "kotlin",
  "lua",
  "mathematica",
  "mumps",
  "netrexx",
  "objeck",
  "ocaml",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "php",
  "picolisp",
  "pl_i",
  "powershell",
  "prolog",
  "purebasic",
  "python",
  "racket",
  "rexx",
  "ring",
  "ruby",
  "run_basic",
  "scala",
  "scheme",
  "sidef",
  "smalltalk",
  "snobol4",
  "standard_ml",
  "stata",
  "tcl",
  "tse_sal",
  "tuscript",
  "txr",
  "unix_shell",
  "vbscript",
  "xpl0",
]
+++

## Task

Soundex is an algorithm for creating indices for words based on their pronunciation.


### Requirements
The goal is for homophones to be encoded to the same representation so that they can be matched despite minor differences in spelling   (from the   [[wp:soundex|soundex   Wikipedia article]]).

### Caution
There is a major issue in many of the implementations concerning the separation of two consonants that have the same soundex code! According to the official Rules [[https://www.archives.gov/research/census/soundex.html]]. So check for instance if '''Ashcraft'''  is coded to '''A-261'''.
* If a vowel (A, E, I, O, U) separates two consonants that have the same soundex code, the consonant to the right of the vowel is coded. Tymczak is coded as T-522 (T, 5 for the M, 2 for the C, Z ignored (see "Side-by-Side" rule above), 2 for the K). Since the vowel "A" separates the Z and K, the K is coded.
* If "H" or "W" separate two consonants that have the same soundex code, the consonant to the right of the vowel is not coded. Example: Ashcraft is coded A-261 (A, 2 for the S, C ignored, 6 for the R, 1 for the F). It is not coded A-226.





## 360 Assembly

{{trans|VBScript}}
An example of the use of the TR opcode (translate) and the uppercase trick by 'or' with space (X'40').

```360asm
*        Soundex                   02/04/2017
SOUNDEX  CSECT
         USING  SOUNDEX,R13        base register
         B      72(R15)            skip savearea
         DC     17F'0'             savearea
         STM    R14,R12,12(R13)    save previous context
         ST     R13,4(R15)         link backward
         ST     R15,8(R13)         link forward
         LR     R13,R15            set addressability
         LA     R6,1               i=1
       DO WHILE=(C,R6,LE,=A(NTT))  do i=1 to hbound(tt)
         LR     R1,R6                i
         BCTR   R1,0                 -1
         MH     R1,=AL2(L'TT)        *length(tt)
         LA     R4,TT(R1)            @tt(i)
         MVC    S,0(R4)              s=tt(i)
         LA     R1,S                 @s
         LA     R2,L'S               length(s)
LOOP     OI     0(R1),C' '           loop s[l]=ucase(s[l])
         LA     R1,1(R1)               @s++
         BCT    R2,LOOP              endloop
         MVC    CODE,=C'0000'        code='0000'
         MVC    CODE(1),S            code[1]=s[1]
         LA     R8,1                 k=1
         LA     R7,1                 j=1
       DO WHILE=(C,R7,LE,=A(L'S))    do j=1 to length(s)
         LA     R4,S-1                 @s[0]
         AR     R4,R7                  +j
         MVC    CCUR,0(R4)             ccur=s[j]
         TR     CCUR,TABLE             ccur=translate(ccur,table)
       IF C,R7,EQ,=F'1' THEN           if j=1 then
         MVC    CPREV,CCUR               cprev=ccur
       ELSE     ,                      else
*                                        if ccur<>' ' and ccur<>'-'
       IF CLI,CCUR,NE,C' ',AND,CLI,CCUR,NE,C'-',                       *
               AND,CLC,CCUR,NE,CPREV THEN  and ccur<>cprev then
       IF C,R8,LT,=F'4' THEN               if k<4 then
         LA     R8,1(R8)                     k=k+1
         LA     R4,CODE-1(R8)                @code[k]
         MVC    0(1,R4),CCUR                 code[k]=ccur
       ENDIF    ,                          endif
       ENDIF    ,                        endif
       IF CLI,CCUR,NE,C'-' THEN          if ccur<>'-' then
         MVC    CPREV,CCUR                 cprev=ccur
       ENDIF    ,                        endif
       ENDIF    ,                      endif
         LA     R7,1(R7)               j++
       ENDDO    ,                    enddo j
         XDECO  R6,XDEC              edit i
         MVC    PG(2),XDEC+10        i
         MVC    PG+3(L'S),S          s
         MVC    PG+15(L'CODE),CODE   code
         XPRNT  PG,L'PG              print
         LA     R6,1(R6)             i++
       ENDDO    ,                  enddo i
         L      R13,4(0,R13)       restore previous savearea pointer
         LM     R14,R12,12(R13)    restore previous context
         XR     R15,R15            rc=0
         BR     R14                exit
TT       DC     CL12'ashcraft',CL12'ashcroft',CL12'gauss',CL12'ghosh'
         DC     CL12'hilbert',CL12'heilbronn',CL12'lee',CL12'lloyd'
         DC     CL12'moses',CL12'pfister',CL12'robert',CL12'rupert'
         DC     CL12'rubin',CL12'tymczak',CL12'soundex',CL12'example'
TTEND    EQU    *
NTT      EQU    (TTEND-TT)/L'TT    hbound(tt)
S        DS     CL12
CCUR     DS     CL1                current
CPREV    DS     CL1                previous
CODE     DS     CL4
PG       DC     CL80' '
XDEC     DS     CL12
TABLE    DC     CL256' '           translation table
         ORG    TABLE+C'A'
         DC     CL9' 123 12- '     ABCDEFGHI
         ORG    TABLE+C'J'
         DC     CL9'22455 126'     JKLMNOPQR
         ORG    TABLE+C'S'
         DC     CL9'23 1-2 2'      STUVWXYZ
         ORG
         YREGS
         END    SOUNDEX
```

{{out}}

```txt

 1 ASHCRAFT    A261
 2 ASHCROFT    A261
 3 GAUSS       G200
 4 GHOSH       G200
 5 HILBERT     H416
 6 HEILBRONN   H416
 7 LEE         L000
 8 LLOYD       L300
 9 MOSES       M220
10 PFISTER     P236
11 ROBERT      R163
12 RUPERT      R163
13 RUBIN       R150
14 TYMCZAK     T522
15 SOUNDEX     S532
16 EXAMPLE     E251

```




## Ada


```Ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Characters.Handling; use Ada.Characters.Handling;
procedure Soundex is
   type UStrings is array(Natural range <>) of Unbounded_String;
   function "+"(S:String) return Unbounded_String renames To_Unbounded_String;

   function toSoundex (instr : String) return String is
      str  : String := To_Upper(instr);
      output : String := "0000";
      spos : Integer := str'First+1;  opos : Positive := 2;
      map  : array(0..255) of Character := (others => ' ');
      last : Integer := str'First;
   begin
      map(65..90) := " 123 12- 22455 12623 1-2 2";
      for i in str'Range loop str(i) := map(Character'Pos(str(i))); end loop;
      output(1) := str(str'First);
      while (opos <= 4 and spos <= str'Last) loop
         if str(spos) /= '-' and str(spos) /= ' ' then
            if (str(spos-1) = '-' and last = spos-2) and then
              (str(spos) = str(spos-2)) then null;
            elsif (str(spos) = output(opos-1) and last = spos-1) then last := spos;
            else output(opos) := str(spos);  opos := opos + 1; last := spos;
            end if;
         end if;
         spos := spos + 1;
      end loop;
      output(1) := To_Upper(instr(instr'First));
      return output;
   end toSoundex;

   cases : constant UStrings := (+"Soundex", +"Example", +"Sownteks",
      +"Ekzampul", +"Euler", +"Gauss", +"Hilbert", +"Knuth", +"Lloyd",
      +"Lukasiewicz", +"Ellery", +"Ghosh", +"Heilbronn", +"Kant",
      +"Ladd", +"Lissajous", +"Wheaton", +"Burroughs", +"Burrows",
      +"O'Hara", +"Washington", +"Lee", +"Gutierrez", +"Pfister",
      +"Jackson", +"Tymczak", +"VanDeusen", +"Ashcraft");
begin
   for i in cases'Range loop
      Put_Line(To_String(cases(i))&" = "&toSoundex(To_String(cases(i))));
   end loop;
end Soundex;
```

{{out}}

```txt
Soundex = S532
Example = E251
Sownteks = S532
Ekzampul = E251
Euler = E460
Gauss = G200
Hilbert = H416
Knuth = K530
Lloyd = L300
Lukasiewicz = L222
Ellery = E460
Ghosh = G200
Heilbronn = H416
Kant = K530
Ladd = L300
Lissajous = L222
Wheaton = W350
Burroughs = B620
Burrows = B620
O'Hara = O600
Washington = W252
Lee = L000
Gutierrez = G362
Pfister = P236
Jackson = J250
Tymczak = T522
VanDeusen = V532
Ashcraft = A261
```



## ALGOL 68

{{trans|C}}
{{works with|ALGOL 68G|Any - tested with release 2.2.0}}
Note: The only non-standard prelude functions used are to lower, is alpha, and is digit.
These are easy enough to write, vide [[String case#ALGOL 68|String case]]

```Algol68
    PROC soundex = (STRING s) STRING:
    BEGIN
        PROC encode = (CHAR c) CHAR:
        BEGIN
            # We assume the alphabet is contiguous. #
            "-123-12*-22455-12623-1*2-2"[ABS to lower(c) - ABS "a" + 1]
        END;
        INT soundex code length = 4;
        STRING result := soundex code length * "0";
        IF s /= "" THEN
            CHAR previous;
            INT  j;
            result[j := 1] := s[1];
            previous := encode(s[1]);
            FOR i FROM 2 TO UPB s WHILE j < soundex code length
            DO
                IF is alpha(s[i]) THEN
                    CHAR code = encode(s[i]);
                    IF is digit(code) AND code /= previous THEN
                        result[j +:= 1] := code;
                        previous := code
                    ELIF code = "-" THEN
                        # Only vowels (y counts here) hide the last-added character #
                        previous := code
                    FI
                FI
            OD
        FI;
        result
    END;

    # Test code to persuade one that it does work. #

    MODE TEST = STRUCT (STRING input, STRING expected output);

    [] TEST soundex test = (
        ("Soundex",    "S532"), ("Example",     "E251"),
        ("Sownteks",   "S532"), ("Ekzampul",    "E251"),
        ("Euler",      "E460"), ("Gauss",       "G200"),
        ("Hilbert",    "H416"), ("Knuth",       "K530"),
        ("Lloyd",      "L300"), ("Lukasiewicz", "L222"),
        ("Ellery",     "E460"), ("Ghosh",       "G200"),
        ("Heilbronn",  "H416"), ("Kant",        "K530"),
        ("Ladd",       "L300"), ("Lissajous",   "L222"),
        ("Wheaton",    "W350"), ("Burroughs",   "B620"),
        ("Burrows",    "B620"), ("O'Hara",      "O600"),
        ("Washington", "W252"), ("Lee",         "L000"),
        ("Gutierrez",  "G362"), ("Pfister",     "P236"),
        ("Jackson",    "J250"), ("Tymczak",     "T522"),
        ("VanDeusen",  "V532"), ("Ashcraft",    "A261")
    );

    #
      Apologies for the magic number in the padding of the input
      and the wired-in heading.
    #

    print(("Test name   Code Got", newline, "----------------------", newline));
    FOR i FROM LWB soundex test TO UPB soundex test
    DO
        STRING output = soundex(input OF soundex test[i]);
        printf(($g, n (12 - UPB input OF soundex test[i]) x$, input OF soundex test[i]));
        printf(($g, 1x, g, 1x$, expected output OF soundex test[i], output));
        printf(($b("ok", "not ok"), 1l$, output = expected output OF soundex test[i]))
    OD
```



## ANSI Standard BASIC

{{trans|BBC Basic}}
Note: Line numbers (strict ANSI interpretation), LET and the variable after NEXT are not optional.


```ANSI
100 DECLARE EXTERNAL FUNCTION FNSoundex$
110
120 DATA Ashcraft, Ashcroft, Gauss, Ghosh, Hilbert, Heilbronn, Lee, Lloyd
130 DATA Moses, Pfister, Robert, Rupert, Rubin, Tymczak, Soundex, Example
140 FOR i = 1 TO 16
150    READ name$
160    PRINT """"; name$; """"; TAB(15); FNsoundex$(name$)
170 NEXT i
180 END
190
200 EXTERNAL FUNCTION FNsoundex$(name$)
210 LET name$ = UCASE$(name$)
220 LET n$ = "01230129022455012623019202"
230 LET s$ = name$(1:1)
240 LET p = VAL(n$(ORD(s$) - 64 : ORD(s$) - 64))
250 FOR i = 2 TO LEN(name$)
260    LET n = VAL(n$(ORD(name$(i:i)) - 64: ORD(name$(i:i)) - 64))
270    IF n <> 0 AND n <> 9 AND n <> p THEN LET s$ = s$ & STR$(n)
280    IF n <> 9 THEN LET p = n
290 NEXT i
300 LET s$ = s$ & "000"
310 LET FNSoundex$ = s$(1:4)
320 END FUNCTION
```



## AutoHotkey

{{trans|VBScript}}

```AutoHotkey
getCode(c){
        If c in B,F,P,V
            return 1
        If c in C,G,J,K,Q,S,X,Z
            return 2
        If c in D,T
            return 3
        If c = L
            return 4
        If c in M,N
            return 5
        If c = R
            return 6
}

soundex(s){
    code := SubStr(s, 1, 1)
   ,previous := 7
   ,i := 1
    While ++i <= StrLen(s){
        current := getCode(SubStr(s, i, 1))
        If StrLen(current) > 0 And current <> previous
            code := code . current
        previous := current
    }
    soundex := SubStr(code, 1, 4)
    If StrLen(code) < 4
        soundex .= String(4 - StrLen(code), "0")
    return soundex
}

String(a, n){
   Loop n
      o .= a
   return a
}

MsgBox % Soundex("Soundex") "`n" Soundex("Sowndeks") "`n" Soundex("Ashcroft") "`n" Soundex("Ashkrofd")
```



## AWK


The soundex function is embedded in a program to build a table of soundex "homonyms".


```awk
#!/usr/bin/awk -f
BEGIN {
    subsep = ", "
    delete homs
}

/^[a-zA-Z]/ {
    sdx = strToSoundex($0)
    addHom(sdx, $0)
}

END {
    showHoms(3)
}

function strToSoundex(s,    sdx, i, ch, cd, lch) {
    if (length(s) == 0) return ""
    s = tolower(s)
    lch = substr(s, 1, 1);
    sdx = toupper(lch)
    lch = charToSoundex(lch)
    for (i = 2; i <= length(s); i++) {
        ch = substr(s, i, 1)
        cd = charToSoundex(ch)
        if (cd == 7) continue;
        if (cd && cd != lch) sdx = sdx cd
        lch = cd
    }
    sdx = substr(sdx "0000", 1, 4)
    return sdx
}

function charToSoundex(ch,   cd) {
    if      (ch ~ /[bfpv]/)     cd = 1
    else if (ch ~ /[cgjkqsxz]/) cd = 2
    else if (ch ~ /[dt]/)       cd = 3
    else if (ch == "l")         cd = 4
    else if (ch ~ /[mn]/)       cd = 5
    else if (ch == "r")         cd = 6
    else if (ch ~ /[hw]/)       cd = 7
    else                        cd = 0
    return cd;
}

function addHom(sdx, word) {
    if (!(homs[sdx])) homs[sdx] = ""
    homs[sdx] = homs[sdx] (homs[sdx] == "" ? "" : subsep) word
}

function showHoms(toShow,    i, n, wl, j) {
    for (i in homs) {
        printf i " "
        n = split(homs[i], wl, subsep)
        for (j = 1; j <= toShow && j <= n; j++) {
            printf wl[j] "  "
        }
        print (n > toShow ? "..." : "")
    }
}

```



Example run:

```txt

# ./soundex.awk ../unixdict.txt |sort
A000 a  aaa  aau  ...
A100 a&p  aba  abbe  ...
A110 ababa  above  aviv
A111 aboveboard
A112 aboveground
A114 affable
A115 abovementioned
A120 aback  abase  abash  ...
A121 abusable  abusive  appeasable
A122 abacus  abject  abscess  ...
A123 abstain  abstention  abstinent  ...
A124 abigail  absolute  absolution  ...
A125 absence  absent  absentee  ...
A126 absorb  absorbent  absorption  ...
A130 abate  abbot  abbott  ...
A131 affidavit
A132 abdicate  abduct  abidjan  ...
A133 abetted  abutted  apathetic  ...
A135 abdomen  abdominal  abetting  ...
A136 abater  aftereffect  afterglow  ...
A140 abel  able  afoul  ...
A141 appleby
A142 abelson  ablaze  abolish  ...
.
.
.
Z324 zodiacal
Z400 zeal
Z420 zealous  zilch  zoology
Z430 zealot  zloty
Z453 zealand
Z461 zellerbach
Z500 zan  zen  zion  ...
Z510 zambia  zomba  zombie
Z520 zinc  zing
Z521 zanzibar
Z525 zionism
Z530 zenith
Z532 zounds
Z565 zimmerman
Z600 zaire  zero
Z620 zeroes  zurich
Z623 zoroaster  zoroastrian
Z625 zircon  zirconium
Z630 zeroth
Z650 zorn
#

```



## BBC BASIC


```bbcbasic
      DATA Ashcraft, Ashcroft, Gauss, Ghosh, Hilbert, Heilbronn, Lee, Lloyd
      DATA Moses, Pfister, Robert, Rupert, Rubin, Tymczak, Soundex, Example
      FOR i% = 1 TO 16
        READ name$
        PRINT """" name$ """" TAB(15) FNsoundex(name$)
      NEXT
      END

      DEF FNsoundex(name$)
      LOCAL i%, n%, p%, n$, s$
      name$ = FNupper(name$)
      n$ = "01230129022455012623019202"
      s$ = LEFT$(name$,1)
      p% = VALMID$(n$, ASCs$ - 64, 1)
      FOR i% = 2 TO LEN(name$)
        n% = VALMID$(n$, ASCMID$(name$,i%,1) - 64, 1)
        IF n% IF n% <> 9 IF n% <> p% s$ += STR$(n%)
        IF n% <> 9 p% = n%
      NEXT
      = LEFT$(s$ + "000", 4)

      DEF FNupper(A$)
      LOCAL A%,C%
      FOR A% = 1 TO LEN(A$)
        C% = ASCMID$(A$,A%)
        IF C% >= 97 IF C% <= 122 MID$(A$,A%,1) = CHR$(C%-32)
      NEXT
      = A$
```

{{out}}

```txt

"Ashcraft"     A261
"Ashcroft"     A261
"Gauss"        G200
"Ghosh"        G200
"Hilbert"      H416
"Heilbronn"    H416
"Lee"          L000
"Lloyd"        L300
"Moses"        M220
"Pfister"      P236
"Robert"       R163
"Rupert"       R163
"Rubin"        R150
"Tymczak"      T522
"Soundex"      S532
"Example"      E251

```



## Befunge

This is an implementation of the earlier Knuth soundex algorithm - compatible with PHP - which doesn't support the "HW" rule.

The word to translate is read from stdin, and its corresponding soundex encoding is written to stdout.


```befunge>
:~>:48*\`#v_::"`"`\"{"\`*v
^$$_v#!*`*8 8\`\"["::-**84<
>$1^>:88*>v>$$1->vp7+2\"0"<
|!-g71:g8-<      >1+::3`!>|
>17p\:!v@p7      10,+55$$<$
v+1p7+2_\$17g\17gv>>+:>5`|2
v$$:$$_^#\<1!-"0"<^1,<g7:<<
v??????????????????????????
v01230120022455012623010202
```


{{out}} (multiple runs)

```txt
Euler
E460
Gauss
G200
Hilbert
H416
Knuth
K530
Lloyd
L300
Lukasiewicz
L222
O'Hara
O600
Ashcraft
A226
```



## C

Some string examples and rules from [[http://www.archives.gov/research/census/soundex.html]].

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

/* for ASCII only */
static char code[128] = { 0 };
void add_code(const char *s, int c)
{
	while (*s) {
		code[(int)*s] = code[0x20 ^ (int)*s] = c;
		s++;
	}
}

void init()
{
	static const char *cls[] =
		{ "AEIOU", "", "BFPV", "CGJKQSXZ", "DT", "L", "MN", "R", 0};
	int i;
	for (i = 0; cls[i]; i++)
		add_code(cls[i], i - 1);
}

/* returns a static buffer; user must copy if want to save
   result across calls */
const char* soundex(const char *s)
{
	static char out[5];
	int c, prev, i;

	out[0] = out[4] = 0;
	if (!s || !*s) return out;

	out[0] = *s++;

	/* first letter, though not coded, can still affect next letter: Pfister */
	prev = code[(int)out[0]];
	for (i = 1; *s && i < 4; s++) {
		if ((c = code[(int)*s]) == prev) continue;

		if (c == -1) prev = 0;	/* vowel as separator */
		else if (c > 0) {
			out[i++] = c + '0';
			prev = c;
		}
	}
	while (i < 4) out[i++] = '0';
	return out;
}

int main()
{
	int i;
	const char *sdx, *names[][2] = {
		{"Soundex",	"S532"},
		{"Example",	"E251"},
		{"Sownteks",	"S532"},
		{"Ekzampul",	"E251"},
		{"Euler",	"E460"},
		{"Gauss",	"G200"},
		{"Hilbert",	"H416"},
		{"Knuth",	"K530"},
		{"Lloyd",	"L300"},
		{"Lukasiewicz",	"L222"},
		{"Ellery",	"E460"},
		{"Ghosh",	"G200"},
		{"Heilbronn",	"H416"},
		{"Kant",	"K530"},
		{"Ladd",	"L300"},
		{"Lissajous",	"L222"},
		{"Wheaton",	"W350"},
		{"Burroughs",	"B620"},
		{"Burrows",	"B620"},
		{"O'Hara",	"O600"},
		{"Washington",	"W252"},
		{"Lee",		"L000"},
		{"Gutierrez",	"G362"},
		{"Pfister",	"P236"},
		{"Jackson",	"J250"},
		{"Tymczak",	"T522"},
		{"VanDeusen",	"V532"},
		{"Ashcraft",	"A261"},
		{0, 0}
	};

	init();

	puts("  Test name  Code  Got\n----------------------");
	for (i = 0; names[i][0]; i++) {
		sdx = soundex(names[i][0]);
		printf("%11s  %s  %s ", names[i][0], names[i][1], sdx);
		printf("%s\n", strcmp(sdx, names[i][1]) ? "not ok" : "ok");
	}

	return 0;
}
```



## C++


```c

#include <iostream> // required for debug code in main() only
#include <iomanip>  // required for debug code in main() only
#include <string>

std::string soundex( char const* s )
{
    static char const code[] = { 0, -1,  1,  2,  3, -1,  1,  2,  0, -1,  2,  2,  4,  5,  5, -1,  1,  2,  6,  2,  3, -1,  1,  0,  2,  0,  2,  0,  0,  0,  0,  0 };

    if( !s || !*s )
        return std::string();

    std::string out( "0000" );
    out[0] = (*s >= 'a' && *s <= 'z') ? *s - ('a' - 'A') : *s;
    ++s;

    char prev = code[out[0] & 0x1F]; // first letter, though not coded, can still affect next letter: Pfister
    for( unsigned i = 1; *s && i < 4; ++s )
    {
        if( (*s & 0xC0) != 0x40 ) // process only letters in range [0x40 - 0x7F]
            continue;
        auto const c = code[*s & 0x1F];
        if( c == prev )
            continue;

        if( c == -1 )
            prev = 0;    // vowel as separator
        else if( c )
        {
            out[i] = c + '0';
            ++i;
            prev = c;
        }
    }
    return out;
}

int main()
{
    static char const * const names[][2] =
    {
        {"Ashcraft",    "A261"},
        {"Burroughs",   "B620"},
        {"Burrows",     "B620"},
        {"Ekzampul",    "E251"},
        {"Ellery",      "E460"},
        {"Euler",       "E460"},
        {"Example",     "E251"},
        {"Gauss",       "G200"},
        {"Ghosh",       "G200"},
        {"Gutierrez",   "G362"},
        {"Heilbronn",   "H416"},
        {"Hilbert",     "H416"},
        {"Jackson",     "J250"},
        {"Kant",        "K530"},
        {"Knuth",       "K530"},
        {"Ladd",        "L300"},
        {"Lee",         "L000"},
        {"Lissajous",   "L222"},
        {"Lloyd",       "L300"},
        {"Lukasiewicz", "L222"},
        {"O'Hara",      "O600"},
        {"Pfister",     "P236"},
        {"Soundex",     "S532"},
        {"Sownteks",    "S532"},
        {"Tymczak",     "T522"},
        {"VanDeusen",   "V532"},
        {"Washington",  "W252"},
        {"Wheaton",     "W350"}
    };

    for( auto const& name : names )
    {
        auto const sdx = soundex( name[0] );
        std::cout << std::left << std::setw( 16 ) << name[0] << std::setw( 8 ) << sdx << (sdx == name[1] ? " ok" : " ERROR") << std::endl;
    }
    return 0;
}


```

{{out|Example output}}

```txt

Ashcraft        A261     ok
Burroughs       B620     ok
Burrows         B620     ok
Ekzampul        E251     ok
Ellery          E460     ok
Euler           E460     ok
Example         E251     ok
Gauss           G200     ok
Ghosh           G200     ok
Gutierrez       G362     ok
Heilbronn       H416     ok
Hilbert         H416     ok
Jackson         J250     ok
Kant            K530     ok
Knuth           K530     ok
Ladd            L300     ok
Lee             L000     ok
Lissajous       L222     ok
Lloyd           L300     ok
Lukasiewicz     L222     ok
O'Hara          O600     ok
Pfister         P236     ok
Soundex         S532     ok
Sownteks        S532     ok
Tymczak         T522     ok
VanDeusen       V532     ok
Washington      W252     ok
Wheaton         W350     ok

```



## C#



```c sharp
using System;
using System.Collections.Generic;
using System.Linq;

namespace Soundex
{
    internal static class Program
    {
        private static void Main()
        {
            var testWords = new TestWords
                                {
                                    {"Soundex", "S532"},
                                    {"Example", "E251"},
                                    {"Sownteks", "S532"},
                                    {"Ekzampul", "E251"},
                                    {"Euler", "E460"},
                                    {"Gauss", "G200"},
                                    {"Hilbert", "H416"},
                                    {"Knuth", "K530"},
                                    {"Lloyd", "L300"},
                                    {"Lukasiewicz", "L222"},
                                    {"Ellery", "E460"},
                                    {"Ghosh", "G200"},
                                    {"Heilbronn", "H416"},
                                    {"Kant", "K530"},
                                    {"Ladd", "L300"},
                                    {"Lissajous", "L222"},
                                    {"Wheaton", "W350"},
                                    {"Burroughs", "B620"},
                                    {"Burrows", "B620"},
                                    {"O'Hara", "O600"},
                                    {"Washington", "W252"},
                                    {"Lee", "L000"},
                                    {"Gutierrez", "G362"},
                                    {"Pfister", "P236"},
                                    {"Jackson", "J250"},
                                    {"Tymczak", "T522"},
                                    {"VanDeusen", "V532"},
                                    {"Ashcraft", "A261"}
                                };

            foreach (var testWord in testWords)
                Console.WriteLine("{0} -> {1} ({2})", testWord.Word.PadRight(11), testWord.ActualSoundex,
                                  (testWord.ExpectedSoundex == testWord.ActualSoundex));
        }

        // List<TestWord> wrapper to make declaration simpler.
        private class TestWords : List<TestWord>
        {
            public void Add(string word, string expectedSoundex)
            {
                Add(new TestWord(word, expectedSoundex));
            }
        }

        private class TestWord
        {
            public TestWord(string word, string expectedSoundex)
            {
                Word = word;
                ExpectedSoundex = expectedSoundex;
                ActualSoundex = Soundex(word);
            }

            public string Word { get; private set; }
            public string ExpectedSoundex { get; private set; }
            public string ActualSoundex { get; private set; }
        }

        private static string Soundex(string word)
        {
            const string soundexAlphabet = "0123012#02245501262301#202";
            string soundexString = "";
            char lastSoundexChar = '?';
            word = word.ToUpper();

            foreach (var c in from ch in word
                              where ch >= 'A' &&
                                    ch <= 'Z' &&
                                    soundexString.Length < 4
                              select ch)
            {
                char thisSoundexChar = soundexAlphabet[c - 'A'];

                if (soundexString.Length == 0)
                    soundexString += c;
                else if (thisSoundexChar == '#')
                    continue;
                else if (thisSoundexChar != '0' &&
                         thisSoundexChar != lastSoundexChar)
                    soundexString += thisSoundexChar;

                lastSoundexChar = thisSoundexChar;
            }

            return soundexString.PadRight(4, '0');
        }
    }
}
```


{{out}}

```txt
Soundex     -> S532 (True)
Example     -> E251 (True)
Sownteks    -> S532 (True)
Ekzampul    -> E251 (True)
Euler       -> E460 (True)
Gauss       -> G200 (True)
Hilbert     -> H416 (True)
Knuth       -> K530 (True)
Lloyd       -> L300 (True)
Lukasiewicz -> L222 (True)
Ellery      -> E460 (True)
Ghosh       -> G200 (True)
Heilbronn   -> H416 (True)
Kant        -> K530 (True)
Ladd        -> L300 (True)
Lissajous   -> L222 (True)
Wheaton     -> W350 (True)
Burroughs   -> B620 (True)
Burrows     -> B620 (True)
O'Hara      -> O600 (True)
Washington  -> W252 (True)
Lee         -> L000 (True)
Gutierrez   -> G362 (True)
Pfister     -> P236 (True)
Jackson     -> J250 (True)
Tymczak     -> T522 (True)
VanDeusen   -> V532 (True)
Ashcraft    -> A261 (True)

```


=={{header|Caché ObjectScript}}==


```cos

Class Utils.Phonetic [ Abstract ]
{

ClassMethod ToSoundex(String As %String) As %String [ Language = mvbasic ]
{
	Return Soundex(String)
}

}

```

{{out|Examples}}

```txt

USER>For  { Read !, name Quit:name=""  Write " = ", ##class(Utils.Phonetic).ToSoundex(name) }

Soundex = S532
Example = E251
Sownteks = S532
Ekzampul = E251
Euler = E460
Gauss = G200
Hilbert = H416
Knuth = K530
Lloyd = L300
Lukasiewicz = L222
Ellery = E460
Ghosh = G200
Heilbronn = H416
Kant = K530
Ladd = L300
Lissajous = L222
Wheaton = W350
Burroughs = B620
Burrows = B620
O'Hara = O600
Washington = W252
Lee = L000
Gutierrez = G362
Pfister = P236
Jackson = J250
Tymczak = T522
VanDeusen = V532
Ashcraft = A261

```



## Clipper/XBase++



```Clipper/XBase++
FUNCTION Soundex(cWord)

 /*

 This is a Clipper/XBase++ implementation of the standard American Soundex procedure.

 */
LOCAL cSoundex, i, nLast, cChar, nCode

cWord:=ALLTRIM(UPPER(cWord))
cSoundex:=LEFT(cWord, 1)        // first letter is first char
nLast:=-1
FOR i:=2 TO LEN(cWord)
   cChar:=SUBSTR(cWord, i, 1)   // get char
   nCode:=SoundexCode(cChar)    // get soundex code for char
   IF nCode=0                   // if 0, ignore
      LOOP
   ENDIF
   IF nCode#nLast               // if not same code, add to soundex
      nLast:=nCode              // and replace the last one
      cSoundex+=STR(nCode, 1)
   ENDIF
NEXT
cSoundex:=PADR(cSoundex, 4, "0")

RETURN(cSoundex)

*******************************************************************************
STATIC FUNCTION SoundexCode(cLetter)
LOCAL aCodes:={"BFPV", "CGJKQSXZ", "DT", "L", "MN", "R"}, i, nRet:=0

FOR i:=1 TO LEN(aCodes)
   IF cLetter $ aCodes[i]
      nRet:=i
      EXIT
   ENDIF
NEXT

RETURN(nRet)

*******************************************************************************
FUNCTION SoundexDifference(cSound1, cSound2)
LOCAL nMatch:=0, nLen1, nLen2, i

nLen1:=LEN(cSound1)
nLen2:=LEN(cSound2)

// make the two words the same length.  This is a safety.  They both should be 4 characters long.
IF nLen1 > nLen2
   cSound2:=PADR(cSound2, nLen1-nLen2, "0")
ELSEIF nLen1 < nLen2
   cSound1:=PADR(cSound1, nLen2-nLen1, "0")
ENDIF

// compare the corresponding characters between the two words
FOR i:=1 TO LEN(cSound1)
   IF SUBSTR(cSound1, i, 1) == SUBSTR(cSound2, i, 1)
      ++nMatch
   ENDIF
NEXT

RETURN(nMatch)

*******************************************************************************
```

--[[User:Clippersolutions|Clippersolutions]] 23:14, 4 November 2010 (UTC)--[[User:Clippersolutions|Clippersolutions]] 23:14, 4 November 2010 (UTC)


## Clojure


```Clojure
(defn get-code [c]
  (case c
    (\B \F \P \V) 1
    (\C \G \J \K
     \Q \S \X \Z) 2
    (\D \T) 3
    \L 4
    (\M \N) 5
    \R 6
    nil)) ;(\A \E \I \O \U \H \W \Y)

(defn soundex [s]
  (let [[f & s] (.toUpperCase s)]
    (-> (map get-code s)
	distinct
	(concat , "0000")
	(->> (cons f ,)
	     (remove nil? ,)
	     (take 4 ,)
	     (apply str ,)))))
```


Bug here? The distinct function eliminates duplicates. What is needed in Soundex is to eliminate consecutive duplicates.


```Clojure

;;; With proper consecutive duplicates elimination

(defn get-code [c]
  (case c
    (\B \F \P \V) 1
    (\C \G \J \K
     \Q \S \X \Z) 2
    (\D \T) 3
    \L 4
    (\M \N) 5
    \R 6
    nil)) ;(\A \E \I \O \U \H \W \Y)

(defn reduce-fn [acc nxt]
 (let [next-code (get-code nxt)]
   (if (and (not= next-code (last acc))
            (not (nil? next-code)))
     (conj acc next-code)
     acc)))

(defn soundex [the-word]
  (let [[first-char & the-rest] (.toUpperCase the-word)
        next-code (get-code (first the-rest))]
    (if (nil? next-code)
      (recur (apply str first-char (rest the-rest)))
      (let [soundex-nums (reduce reduce-fn [] the-rest)]
        (apply str first-char (take 3 (conj soundex-nums 0 0 0)))))))
```



## COBOL


{{works with|OpenCOBOL}}
{{works with|IBM Enterprise COBOL for z/OS}}


```cobol
      **** sndxtest *********************************************
      * Demonstrate the soundex encoding functions.
      ***************************************************************
       Identification division.
       Program-id. sndxtest.

       Data division.
       Working-storage section.
       01 sample-word-list.
           05 sample-words.
               10 filler pic x(15) value "soundex".
               10 filler pic x(15) value "example".
               10 filler pic x(15) value "sownteks".
               10 filler pic x(15) value "ekzampul".
               10 filler pic x(15) value "Euler".
               10 filler pic x(15) value "Gauss".
               10 filler pic x(15) value "Hilbert".
               10 filler pic x(15) value "Knuth".
               10 filler pic x(15) value "Lloyd".
               10 filler pic x(15) value "Lukasiewicz".
               10 filler pic x(15) value "Ellery".
               10 filler pic x(15) value "ghosh".
               10 filler pic x(15) value "Heilbronn".
               10 filler pic x(15) value "Kand".
               10 filler pic x(15) value "Ladd".
               10 filler pic x(15) value "lissajous".
               10 filler pic x(15) value "Wheaton".
               10 filler pic x(15) value "Burroughs".
               10 filler pic x(15) value "burrows".
               10 filler pic x(15) value "O'Hara".
               10 filler pic x(15) value "Washington".
               10 filler pic x(15) value "lee".
               10 filler pic x(15) value "Gutierrez".
               10 filler pic x(15) value "Phister".
               10 filler pic x(15) value "Jackson".
               10 filler pic x(15) value "tymczak".
               10 filler pic x(15) value "Vandeusen".
               10 filler pic x(15) value "Ashcraft".
           05 sample-word redefines sample-words
                         pic x(15) occurs 28 times indexed by wrd-idx.
       01 wrd-code       pic x999.

       Procedure division.
           Perform varying wrd-idx from 1 by 1
           until wrd-idx greater than 28
               call "sndxenc" using
                   by reference sample-word(wrd-idx)
                   by reference wrd-code
               display wrd-code " " sample-word(wrd-idx)
           end-perform.
           Stop run.

       End program sndxtest.

      *** sndxenc ********************************************
      * Given a string return its soundex encoding.
      ***************************************************************
       Identification division.
       Program-id. sndxenc.

       Data division.
       Local-storage section.
       01 str-idx            pic 99.
       01 let-code           pic  9.
       01 prv-let-code       pic  9.
       01 sdx-idx            pic  9  value 1.

       Linkage section.
       01 str-to-encode.
           05 str-first-let  pic x.
           05 str-rest-let   pic x  occurs 14 times.
       01 sdx-code.
           05 sdx-first-let  pic x.
           05 sdx-nums       pic 9  occurs  3 times.

       Procedure division using
           by reference str-to-encode
           by reference sdx-code.
           Perform encode-start thru encode-done.
           Goback.

       Encode-start.
           Move zeros to sdx-code.
           Move function upper-case(str-first-let) to sdx-first-let.
           Call "sndxchar" using
               by reference str-first-let
               by reference let-code.
           Move let-code to prv-let-code.

       Encode-string.
           Perform varying str-idx from 1 by 1
               until str-idx greater than 15
               or str-rest-let(str-idx) = space
               or sdx-idx greater than 3
               call "sndxchar" using
                   by reference str-rest-let(str-idx)
                   by reference let-code
               if let-code not equal 7 then
                   if let-code not equal 0
                   and let-code not equal prv-let-code
                       move let-code to sdx-nums(sdx-idx)
                       add 1 to sdx-idx
                   end-if
                   move let-code to prv-let-code
               end-if
           end-perform.

       Encode-done.
           continue.
       End program sndxenc.


      *** sndxchar **********************************************
      * Given a character, return its soundex encoding.
      * Code 7 is for h or w, which an encoder should ignore when
      * either one separates double letters.
      ***************************************************************
       Identification division.
       Program-id. sndxchar.

       Data division.
       Local-storage section.
       01 lc-chr pic x.
           88 code1 value "b", "f", "p", "v".
           88 code2 value "c", "g", "j", "k", "q", "s", "x", "z".
           88 code3 value "d", "t".
           88 code4 value "l".
           88 code5 value "m", "n".
           88 code6 value "r".
           88 code7 value "h", "w".

       Linkage section.
       01 char-to-encode pic x.
       01 char-sdx-code  pic 9.

       Procedure division using
           by reference char-to-encode
           by reference char-sdx-code.
           Move function lower-case(char-to-encode) to lc-chr.
           If          code1 then move 1 to char-sdx-code
               else if code2 then move 2 to char-sdx-code
               else if code3 then move 3 to char-sdx-code
               else if code4 then move 4 to char-sdx-code
               else if code5 then move 5 to char-sdx-code
               else if code6 then move 6 to char-sdx-code
               else if code7 then move 7 to char-sdx-code
               else               move 0 to char-sdx-code
           end-if.
       End program sndxchar.
```


{{out}}

```txt

S532 soundex
E251 example
S532 sownteks
E251 ekzampul
E460 Euler
G200 Gauss
H416 Hilbert
K530 Knuth
L300 Lloyd
L222 Lukasiewicz
E460 Ellery
G200 ghosh
H416 Heilbronn
K530 Kand
L300 Ladd
L222 lissajous
W350 Wheaton
B620 Burroughs
B620 burrows
O600 O'Hara
W252 Washington
L000 lee
G362 Gutierrez
P236 Phister
J250 Jackson
T522 tymczak
V532 Vandeusen
A261 Ashcraft

```



## Common Lisp


```lisp
(defun get-code (c)
  (case c
    ((#\B #\F #\P #\V) #\1)
    ((#\C #\G #\J #\K
      #\Q #\S #\X #\Z) #\2)
    ((#\D #\T) #\3)
    (#\L #\4)
    ((#\M #\N) #\5)
    (#\R #\6)))

(defun soundex (s)
  (if (zerop (length s))
    ""
    (let* ((l (coerce (string-upcase s) 'list))
           (o (list (first l))))
      (loop for c in (rest l)
            for cg = (get-code c) and
            for cp = #\Z then cg
            when (and cg (not (eql cg cp))) do
              (push cg o)
            finally
              (return (subseq (coerce (nreverse `(#\0 #\0 #\0 ,@o)) 'string) 0 4))))))
```



## Crystal

{{trans|VBScript}}

```ruby
# version 0.21.1

def get_code(c : Char)
  case c
  when 'B', 'F', 'P', 'V'
    "1"
  when 'C', 'G', 'J', 'K', 'Q', 'S', 'X', 'Z'
    "2"
  when 'D', 'T'
    "3"
  when 'L'
    "4"
  when 'M', 'N'
    "5"
  when 'R'
    "6"
  when 'H', 'W'
    "-"
  else
    ""
  end
end

def soundex(s : String)
  return "" if s == ""
  s = s.upcase
  result = s[0,1]
  prev = get_code s[0]
  s.lchop.each_char {|c|
    curr = get_code c
    result += curr if curr != "" && curr != "-" && curr != prev
    prev = curr unless curr == "-"
  }
  result.ljust(4, '0')[0, 4]
end

pairs = [
          ["Ashcraft"  , "A261"],
          ["Ashcroft"  , "A261"],
          ["Gauss"     , "G200"],
          ["Ghosh"     , "G200"],
          ["Hilbert"   , "H416"],
          ["Heilbronn" , "H416"],
          ["Lee"       , "L000"],
          ["Lloyd"     , "L300"],
          ["Moses"     , "M220"],
          ["Pfister"   , "P236"],
          ["Robert"    , "R163"],
          ["Rupert"    , "R163"],
          ["Rubin"     , "R150"],
          ["Tymczak"   , "T522"],
          ["Soundex"   , "S532"],
          ["Example"   , "E251"]
        ]

pairs.each { |pair|
  puts "#{pair[0].ljust(9)} -> #{pair[1]} -> #{soundex(pair[0]) == pair[1]}"
}
```


{{out}}

```txt

Ashcraft  -> A261 -> true
Ashcroft  -> A261 -> true
Gauss     -> G200 -> true
Ghosh     -> G200 -> true
Hilbert   -> H416 -> true
Heilbronn -> H416 -> true
Lee       -> L000 -> true
Lloyd     -> L300 -> true
Moses     -> M220 -> true
Pfister   -> P236 -> true
Robert    -> R163 -> true
Rupert    -> R163 -> true
Rubin     -> R150 -> true
Tymczak   -> T522 -> true
Soundex   -> S532 -> true
Example   -> E251 -> true

```



## D


### Standard Version

The D standard library (Phobos) contains a soundex function:

```d
import std.stdio: writeln;
import std.string: soundex;

void main() {
    assert(soundex("soundex") == "S532");
    assert(soundex("example") == "E251");
    assert(soundex("ciondecks") == "C532");
    assert(soundex("ekzampul") == "E251");
    assert(soundex("Robert") == "R163");
    assert(soundex("Rupert") == "R163");
    assert(soundex("Rubin") == "R150");
    assert(soundex("Ashcraft") == "A261");
    assert(soundex("Ashcroft") == "A261");
    assert(soundex("Tymczak") == "T522");
}
```

It works according to this document:
http://www.archives.gov/publications/general-info-leaflets/55.html
So soundex("Ashcraft") is A-261 instead of A-226.

### Alternative Version

This version uses the Wikipedia algorithm, it's long because it contains a ddoc text, design by contract (a long post-condition), sanity asserts, unittests and comments. A quite shorter version may be written that loses the safety net that's necessary in serious coding.

This version uses dynamic heap allocations in some places (replace, toupper, several string join) to allow a higher level style of coding, but this function may also be written to perform zero heap allocations. It may even return a char[4] by value, or use a given buffer like the C version.


```d
import std.array, std.string, std.ascii, std.algorithm, std.range;

/**
Soundex is a phonetic algorithm for indexing names by
sound, as pronounced in English. See:
http://en.wikipedia.org/wiki/Soundex
*/
string soundex(in string name) pure /*nothrow*/
out(result) {
    assert(result.length == 4);
    assert(result[0] == '0' || result[0].isUpper);

    if (name.empty)
        assert(result == "0000");
    immutable charCount = name.filter!isAlpha.walkLength;
    assert((charCount == 0) == (result == "0000"));
} body {
    // Adapted from public domain Python code by Gregory Jorgensen:
    // http://code.activestate.com/recipes/52213/
    // digits holds the soundex values for the alphabet.
    static immutable digits = "01230120022455012623010202";
    string firstChar, result;

    // Translate alpha chars in name to soundex digits.
    foreach (immutable dchar c; name.toUpper) { // Not nothrow.
        if (c.isUpper) {
            if (firstChar.empty)
                firstChar ~= c; // Remember first letter.
            immutable char d = digits[c - 'A'];
            // Duplicate consecutive soundex digits are skipped.
            if (!result.length || d != result.back)
                result ~= d;
        }
    }

    // Return 0000 if the name is empty.
    if (!firstChar.length)
        return "0000";

    // Replace first digit with first alpha character.
    assert(!result.empty);
    result = firstChar ~ result[1 .. $];

    // Remove all 0s from the soundex code.
    result = result.replace("0", "");

    // Return soundex code padded to 4 zeros.
    return (result ~ "0000")[0 .. 4];
} unittest { // Tests of soundex().
    auto tests = [["",         "0000"], ["12346",     "0000"],
                  ["he",       "H000"], ["soundex",   "S532"],
                  ["example",  "E251"], ["ciondecks", "C532"],
                  ["ekzampul", "E251"], ["rÃ©sumÃ©",  "R250"],
                  ["Robert",   "R163"], ["Rupert",    "R163"],
                  ["Rubin",    "R150"], ["Ashcraft",  "A226"],
                  ["Ashcroft", "A226"]];
    foreach (const pair; tests)
        assert(pair[0].soundex == pair[1]);
}

void main() {}
```



## Delphi


```Delphi

program SoundexDemo;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  StrUtils;

begin
  Writeln(Soundex('Soundex'));
  Writeln(Soundex('Example'));
  Writeln(Soundex('Sownteks'));
  Writeln(Soundex('Ekzampul'));
  Readln;
end.

```

{{out}}

```txt

S532
E251
S532
E251

```



## Elixir

{{trans|Erlang}}

```elixir
defmodule Soundex do
  def soundex([]), do: []
  def soundex(str) do
    [head|tail] = String.upcase(str) |> to_char_list
    [head | isoundex(tail, [], todigit(head))]
  end

  defp isoundex([], acc, _) do
    case length(acc) do
      n when n == 3 -> Enum.reverse(acc)
      n when n <  3 -> isoundex([], [?0 | acc], :ignore)
      n when n >  3 -> isoundex([], Enum.slice(acc, n-3, n), :ignore)
    end
  end
  defp isoundex([head|tail], acc, lastn) do
    dig = todigit(head)
    if dig != ?0 and dig != lastn do
      isoundex(tail, [dig | acc], dig)
    else
      case head do
        ?H                 -> isoundex(tail, acc, lastn)
        ?W                 -> isoundex(tail, acc, lastn)
        n when n in ?A..?Z -> isoundex(tail, acc, dig)
        _                  -> isoundex(tail, acc, lastn)  # This clause handles non alpha characters
      end
    end
  end

  @digits  '01230120022455012623010202'
  defp todigit(chr) do
    if chr in ?A..?Z, do: Enum.at(@digits, chr - ?A),
                    else: ?0            # Treat non alpha characters as a vowel
  end
end

IO.puts Soundex.soundex("Soundex")
IO.puts Soundex.soundex("Example")
IO.puts Soundex.soundex("Sownteks")
IO.puts Soundex.soundex("Ekzampul")
```


{{out}}

```txt

S532
E251
S532
E251

```



## Erlang

This implements the US Census rules, where W and H are ignored but, unlike vowels, are not separators.

```Erlang
-module(soundex).
-export([soundex/1]).

soundex([]) ->
    [];
soundex(Str) ->
    [Head|Tail] = string:to_upper(Str),
    [Head | isoundex(Tail, [], todigit(Head))].

isoundex([], Acc, _) ->
    case length(Acc) of
	N when N == 3 ->
	    lists:reverse(Acc);
	N when N < 3 ->
	    isoundex([], [$0 | Acc], ignore);
	N when N > 3 ->
	    isoundex([], lists:sublist(Acc, N-2, N), ignore)
    end;
isoundex([Head|Tail], Acc, Lastn) ->
    Dig = todigit(Head),
    case Dig of
	Dig when Dig /= $0, Dig /= Lastn ->
	    isoundex(Tail, [Dig | Acc], Dig);
	_ ->
	    case Head of
		$H ->
		    isoundex(Tail, Acc, Lastn);
		$W ->
		    isoundex(Tail, Acc, Lastn);
		N when N >= $A, N =< $Z ->
		    isoundex(Tail, Acc, Dig);
		_ ->
		    isoundex(Tail, Acc, Lastn)	% This clause handles non alpha characters
	    end
    end.

todigit(Chr) ->
    Digits = "01230120022455012623010202",
    HeadOff = Chr - $A + 1,
    case HeadOff of
	N when N > 0, N < 27 ->
	    lists:nth(HeadOff, Digits);
	_ ->					% Treat non alpha characters as a vowel
	    $0
    end.

```


=={{header|F_Sharp|F#}}==

```FSharp
module Soundex

let soundex (s : string) =
    let code c =
        match c with
        | 'B' | 'F' | 'P' | 'V' -> Some('1')
        | 'C' | 'G' | 'J' | 'K' | 'Q' | 'S' | 'X' | 'Z' -> Some('2')
        | 'D' | 'T' -> Some('3')
        | 'L' -> Some('4')
        | 'M' | 'N' -> Some('5')
        | 'R' -> Some('6')
        | _ -> None

    let rec p l =
        match l with
        | [] -> []
        | x :: y :: tail when (code x) = (code y) -> (p (y :: tail))
        | x :: 'W' :: y :: tail when (code x) = (code y) -> (p (y :: tail))
        | x :: 'H' :: y :: tail when (code x) = (code y) -> (p (y :: tail))
        | x :: tail -> (code x) :: (p tail)

    let chars =
        match (p (s.ToUpper() |> List.ofSeq)) with
        | [] -> ""
        | head :: tail -> new string((s.[0] :: (tail |> List.filter (fun x -> x.IsSome) |> List.map (fun x -> x.Value))) |> List.toArray)
    chars.PadRight(4, '0').Substring(0, 4)

let test (input, se) =
    printfn "%12s\t%s\t%s" input se (soundex input)

let testCases = [|
    ("Ashcraft", "A261"); ("Ashcroft", "A261"); ("Burroughs", "B620"); ("Burrows", "B620");
    ("Ekzampul", "E251"); ("Example", "E251"); ("Ellery", "E460"); ("Euler", "E460");
    ("Ghosh", "G200"); ("Gauss", "G200"); ("Gutierrez", "G362"); ("Heilbronn", "H416");
    ("Hilbert", "H416"); ("Jackson", "J250"); ("Kant", "K530"); ("Knuth", "K530");
    ("Lee", "L000"); ("Lukasiewicz", "L222"); ("Lissajous", "L222"); ("Ladd", "L300");
    ("Lloyd", "L300"); ("Moses", "M220"); ("O'Hara", "O600"); ("Pfister", "P236");
    ("Rubin", "R150"); ("Robert", "R163"); ("Rupert", "R163"); ("Soundex", "S532");
    ("Sownteks", "S532"); ("Tymczak", "T522"); ("VanDeusen", "V532"); ("Washington", "W252");
    ("Wheaton", "W350");
    |]

[<EntryPoint>]
let main args =
    testCases |> Array.sortBy (fun (_, x) -> x) |> Array.iter test
    System.Console.ReadLine() |> ignore

    0

```

{{out}}

```txt

    Ashcraft    A261    A261
    Ashcroft    A261    A261
   Burroughs    B620    B620
     Burrows    B620    B620
    Ekzampul    E251    E251
     Example    E251    E251
      Ellery    E460    E460
       Euler    E460    E460
       Ghosh    G200    G200
       Gauss    G200    G200
   Gutierrez    G362    G362
   Heilbronn    H416    H416
     Hilbert    H416    H416
     Jackson    J250    J250
        Kant    K530    K530
       Knuth    K530    K530
         Lee    L000    L000
 Lukasiewicz    L222    L222
   Lissajous    L222    L222
        Ladd    L300    L300
       Lloyd    L300    L300
       Moses    M220    M220
      O'Hara    O600    O600
     Pfister    P236    P236
       Rubin    R150    R150
      Robert    R163    R163
      Rupert    R163    R163
     Soundex    S532    S532
    Sownteks    S532    S532
     Tymczak    T522    T522
   VanDeusen    V532    V532
  Washington    W252    W252
     Wheaton    W350    W350

```



## Factor


```factor
USE: soundex
"soundex" soundex    ! S532
"example" soundex    ! E251
"ciondecks" soundex  ! C532
"ekzampul" soundex   ! E251
```



## Forth

This implements the US Census rules, where W and H are ignored but, unlike vowels, aren't separators. Further corner cases welcome...


```forth
: alpha-table create does> swap 32 or [char] a - 0 max 26 min + 1+ c@ ;

alpha-table soundex-code
  ,"  123 12. 22455 12623 1.2 2 "
   \ ABCDEFGHIJKLMNOPQRSTUVWXYZ

: soundex ( name len -- pad len )
  over c@ pad c!                  \ First character verbatim
  pad 1+ 3 [char] 0 fill          \ Pad to four characters with zeros
  1 pad c@ soundex-code  ( count code )
  2swap bounds do
    i c@ soundex-code   ( count code next )
    2dup = if         drop else   \ runs are ignored
    dup [char] . = if drop else   \ W, H don't separate runs of consonants
    dup bl = if        nip else   \ vowels separate consonants but aren't coded
      nip
      2dup swap pad + c!
      swap 1+
      tuck 4 = if leave then
    then then then
  loop
  2drop pad 4 ;

\ Knuth's test cases
s" Euler"       soundex cr type   \ E460
s" Gauss"       soundex cr type   \ G200
s" Hilbert"     soundex cr type   \ H416
s" Knuth"       soundex cr type   \ K530
s" Lloyd"       soundex cr type   \ L300
s" Lukasiewicz" soundex cr type   \ L222 (W test)
s" Ellery"      soundex cr type   \ E460
s" Ghosh"       soundex cr type   \ G200
s" Heilbronn"   soundex cr type   \ H416
s" Kant"        soundex cr type   \ K530
s" Ladd"        soundex cr type   \ L300
s" Lissajous"   soundex cr type   \ L222

s" Wheaton"   soundex cr type   \ W350
s" Ashcraft"  soundex cr type   \ A261  (H tests)
s" Burroughs" soundex cr type   \ B620
s" Burrows"   soundex cr type   \ B620  (W test) (any Welsh names?)
s" O'Hara"    soundex cr type   \ O600  (punctuation test)
```



## FutureBasic


```futurebasic

include "ConsoleWindow"

def tab 12
_soundexNil$ = "0000"

local mode
local fn Soundex( codeWord as Str255 ) as Str255
dim as long          i,u
dim as unsigned char charCode,lastCode
dim as Str31         outputStr

outputStr = _soundexNil$

if codeWord[0] = _nil then exit fn

UppercaseStripDiacritics( @codeWord[1], codeWord[0], _smCurrentScript )

outputStr[1] = codeWord[1]
charCode     = outputStr[1] : gosub "getSoundexCode"
lastCode     = charCode
i = 1 : u = 1

while i <= codeWord[0]
   i++ : charCode = codeWord[i] : gosub "getSoundexCode"
   if charCode > 0 and lastCode <> charCode
      u++ : outputStr[u] = charCode
      if u = 4 then exit while
   end if
lastCode = charCode
wend

exit fn

"getSoundexCode"

select charCode
case _"B", _"F", _"P", _"V"
charCode = _"1"
case _"C", _"G", _"J", _"K", _"Q", _"S", _"X", _"Z"
charCode = _"2"
case _"D", _"T"
charCode = _"3"
case _"L"
charCode = _"4"
case _"M", _"N"
charCode = _"5"
case _"R"
charCode = _"6"
case else
charCode = 0
end select
return
end fn = outputStr

dim as Str255 nameStr, testName(100)
dim as long i

testName(0) = "Smith "
testName(1) = "Johnson "
testName(2) = "Williams "
testName(3) = "Jones "
testName(4) = "Brown "
testName(5) = "Davis "
testName(6) = "Miller "
testName(7) = "Wilson "
testName(8) = "Moore "
testName(9) = "Taylor "
testName(10) = "Anderson "
testName(11) = "Thomas "
testName(12) = "Jackson "
testName(13) = "White "
testName(14) = "Harris "
testName(15) = "Martin "
testName(16) = "Thompson "
testName(17) = "Garcia "
testName(18) = "Martinez "
testName(19) = "Robinson "
testName(20) = "Clark "
testName(21) = "Rodriguez "
testName(22) = "Lewis "
testName(23) = "Lee "
testName(24) = "Walker "
testName(25) = "Hall "
testName(26) = "Allen "
testName(27) = "Young "
testName(28) = "Hernandez "
testName(29) = "King "
testName(30) = "Wright "
testName(31) = "Lopez "
testName(32) = "Hill "
testName(33) = "Scott "
testName(34) = "Green "
testName(35) = "Adams "
testName(36) = "Baker "
testName(37) = "Gonzalez "
testName(38) = "Nelson "
testName(39) = "Carter "
testName(40) = "Mitchell "
testName(41) = "Perez "
testName(42) = "Roberts "
testName(43) = "Turner "
testName(44) = "Phillips "
testName(45) = "Campbell "
testName(46) = "Parker "
testName(47) = "Evans "
testName(48) = "Edwards "
testName(49) = "Collins "
testName(50) = "Stewart "
testName(51) = "Sanchez "
testName(52) = "Morris "
testName(53) = "Rogers "
testName(54) = "Reed "
testName(55) = "Cook "
testName(56) = "Morgan "
testName(57) = "Bell "
testName(58) = "Murphy "
testName(59) = "Bailey "
testName(60) = "Rivera "
testName(61) = "Cooper "
testName(62) = "Richardson "
testName(63) = "Cox "
testName(64) = "Howard "
testName(65) = "Ward "
testName(66) = "Torres "
testName(67) = "Peterson "
testName(68) = "Gray "
testName(69) = "Ramirez "
testName(70) = "James "
testName(71) = "Watson "
testName(72) = "Brooks "
testName(73) = "Kelly "
testName(74) = "Sanders "
testName(75) = "Price "
testName(76) = "Bennett "
testName(77) = "Wood "
testName(78) = "Barnes "
testName(79) = "Ross "
testName(80) = "Henderson "
testName(81) = "Coleman "
testName(82) = "Jenkins "
testName(83) = "Perry "
testName(84) = "Powell "
testName(85) = "Long "
testName(86) = "Patterson "
testName(87) = "Hughes "
testName(88) = "Flores "
testName(89) = "Washington "
testName(90) = "Butler "
testName(91) = "Simmons "
testName(92) = "Foster "
testName(93) = "Gonzales "
testName(94) = "Bryant "
testName(95) = "Alexander "
testName(96) = "Russell "
testName(97) = "Griffin "
testName(98) = "Diaz "
testName(99) = "Hayes "

print "Soundex codes for 100 popular American surnames:"
for i = 0 to 99
   nameStr = testName(i)
   print nameStr, "= "; fn Soundex( nameStr )
next

print
print "Soundex codes for similar sounding names:"

print " Stuart = "; fn Soundex( "Stuart"  )
print "Stewart = "; fn Soundex( "Stewart" )
print "Steward = "; fn Soundex( "Steward" )
print " Seward = "; fn Soundex( "Seward"  )


```


Output:

```txt

Soundex codes for 100 popular American surnames:
Smith       = S530
Johnson     = J525
Williams    = W452
Jones       = J520
Brown       = B650
Davis       = D120
Miller      = M460
Wilson      = W425
Moore       = M600
Taylor      = T460
Anderson    = A536
Thomas      = T520
Jackson     = J250
White       = W300
Harris      = H620
Martin      = M635
Thompson    = T512
Garcia      = G620
Martinez    = M635
Robinson    = R152
Clark       = C462
Rodriguez   = R362
Lewis       = L200
Lee         = L000
Walker      = W426
Hall        = H400
Allen       = A450
Young       = Y520
Hernandez   = H655
King        = K520
Wright      = W623
Lopez       = L120
Hill        = H400
Scott       = S300
Green       = G650
Adams       = A352
Baker       = B260
Gonzalez    = G524
Nelson      = N425
Carter      = C636
Mitchell    = M324
Perez       = P620
Roberts     = R163
Turner      = T656
Phillips    = P412
Campbell    = C514
Parker      = P626
Evans       = E152
Edwards     = E363
Collins     = C452
Stewart     = S363
Sanchez     = S522
Morris      = M620
Rogers      = R262
Reed        = R300
Cook        = C200
Morgan      = M625
Bell        = B400
Murphy      = M610
Bailey      = B400
Rivera      = R160
Cooper      = C160
Richardson  = R263
Cox         = C200
Howard      = H630
Ward        = W630
Torres      = T620
Peterson    = P362
Gray        = G600
Ramirez     = R562
James       = J520
Watson      = W325
Brooks      = B620
Kelly       = K400
Sanders     = S536
Price       = P620
Bennett     = B530
Wood        = W300
Barnes      = B652
Ross        = R200
Henderson   = H536
Coleman     = C455
Jenkins     = J525
Perry       = P600
Powell      = P400
Long        = L520
Patterson   = P362
Hughes      = H220
Flores      = F462
Washington  = W252
Butler      = B346
Simmons     = S552
Foster      = F236
Gonzales    = G524
Bryant      = B653
Alexander   = A425
Russell     = R240
Griffin     = G615
Diaz        = D200
Hayes       = H200

Soundex codes for similar sounding names:
 Stuart = S363
Stewart = S363
Steward = S363
 Seward = S630

```



## Go

WP article rules, plus my interpretation for input validation.

```go
package main

import (
    "errors"
    "fmt"
    "unicode"
)

var code = []byte("01230127022455012623017202")

func soundex(s string) (string, error) {
    var sx [4]byte
    var sxi int
    var cx, lastCode byte
    for i, c := range s {
        switch {
        case !unicode.IsLetter(c):
            if c < ' ' || c == 127 {
                return "", errors.New("ASCII control characters disallowed")
            }
            if i == 0 {
                return "", errors.New("initial character must be a letter")
            }
            lastCode = '0'
            continue
        case c >= 'A' && c <= 'Z':
            cx = byte(c - 'A')
        case c >= 'a' && c <= 'z':
            cx = byte(c - 'a')
        default:
            return "", errors.New("non-ASCII letters unsupported")
        }
        // cx is valid letter index at this point
        if i == 0 {
            sx[0] = cx + 'A'
            sxi = 1
            continue
        }
        switch x := code[cx]; x {
        case '7', lastCode:
        case '0':
            lastCode = '0'
        default:
            sx[sxi] = x
            if sxi == 3 {
                return string(sx[:]), nil
            }
            sxi++
            lastCode = x
        }
    }
    if sxi == 0 {
        return "", errors.New("no letters present")
    }
    for ; sxi < 4; sxi++ {
        sx[sxi] = '0'
    }
    return string(sx[:]), nil
}

func main() {
    for _, s := range []string{
        "Robert",   // WP test case = R163
        "Rupert",   // WP test case = R163
        "Rubin",    // WP test case = R150
        "ashcroft", // WP test case = A261
        "ashcraft", // s and c combine across h, t not needed
        "moses",    // s's don't combine across e
        "O'Mally",  // apostrophe allowed, adjacent ll's combine
        "d jay",    // spaces allowed
        "R2-D2",    // digits, hyphen allowed
        "12p2",     // just not in leading position
        "naïve",    // non ASCII disallowed
        "",         // empty string disallowed
        "bump\t",   // ASCII control characters disallowed
    } {
        if x, err := soundex(s); err == nil {
            fmt.Println("soundex", s, "=", x)
        } else {
            fmt.Printf("\"%s\" fail. %s\n", s, err)
        }
    }
}
```

{{out}}

```txt

soundex Robert = R163
soundex Rupert = R163
soundex Rubin = R150
soundex ashcroft = A261
soundex ashcraft = A261
soundex moses = M220
soundex O'Mally = O540
soundex d jay = D200
soundex R2-D2 = R300
"12p2" fail. initial character must be a letter
"naïve" fail. non-ASCII letters unsupported
"" fail. no letters present
"bump   " fail. ASCII control characters disallowed

```



## Groovy


```groovy

def soundex(s) {
    def code = ""
    def lookup = [
       B : 1, F : 1, P : 1, V : 1,
       C : 2, G : 2, J : 2, K : 2, Q : 2, S : 2, X : 2, Z : 2,
       D : 3, T : 3,
       L : 4,
       M : 5, N : 5,
       R : 6
    ]
    s[1..-1].toUpperCase().inject(7) { lastCode, letter ->
        def letterCode = lookup[letter]
        if(letterCode && letterCode != lastCode) {
            code += letterCode
        }
    }
    return "${s[0]}${code}0000"[0..3]
}

println(soundex("Soundex"))
println(soundex("Sownteks"))
println(soundex("Example"))
println(soundex("Ekzampul"))

```



## Haskell

{{libheader|Text.PhoneticCode.Soundex}}

```haskell
import Text.PhoneticCode.Soundex

main :: IO ()
main =
  mapM_ print $
  ((,) <*> soundexSimple) <$> ["Soundex", "Example", "Sownteks", "Ekzampul"]
```

{{Out}}

```txt
("Soundex","S532")
("Example","E251")
("Sownteks","S532")
("Ekzampul","E251")
```


=={{header|Icon}} and {{header|Unicon}}==

```icon
procedure main(arglist)             # computes soundex of each argument
every write(x := !arglist, " => ",soundex(x))
end

procedure soundex(name)
   local  dig,i,x
   static con
   initial {                                   # construct mapping x[i] => i all else .
      x := ["bfpv","cgjkqsxz","dt","l","mn","r"]
      every ( dig := con := "") ||:= repl(i := 1 to *x,*x[i]) do con ||:= x[i]
      con := map(map(&lcase,con,dig),&lcase,repl(".",*&lcase))
      }

   name := map(name)                           # lower case
   name[1] := map(name[1],&lcase,&ucase)       # upper case 1st
   name := map(name,&lcase,con)                # map cons
   every x := !"123456" do
       while name[find(x||x,name)+:2] := x     # kill duplicates
   while name[upto('.',name)] := ""            # kill .
   return left(name,4,"0")
end
```

{{libheader|Icon Programming Library}} implements [http://www.cs.arizona.edu/icon/library/procs/soundex.htm soundex].  The above version is an adaptation of that procedure

=={{header|IS-BASIC}}==
<lang IS-BASIC>100 PROGRAM "Soundex.bas"
110 FOR I=1 TO 20
120   READ NAME$
130   PRINT """";NAME$;"""";TAB(20);SOUNDEX$(NAME$)
140 NEXT
150 DEF SOUNDEX$(NAME$)
160   NUMERIC I,N,P
170   LET NAME$=UCASE$(NAME$):LET S$=NAME$(1)
180   LET N$="01230129022455012623019202"
190   LET P=VAL(N$(ORD(S$)-64))
200   FOR I=2 TO LEN(NAME$)
210     LET N=VAL(N$(ORD(NAME$(I))-64))
220     IF N<>0 AND N<>9 AND N<>P THEN LET S$=S$&STR$(N)
230     IF N<>9 THEN LET P=N
240   NEXT
250   LET S$=S$&"000"
260   LET SOUNDEX$=S$(1:4)
270 END DEF
280 DATA Aschraft,Ashcroft,Euler,Gauss,Ghosh,Hilbert,Heilbronn,Lee,Lissajous,Lloyd
290 DATA Moses,Pfister,Robert,Rupert,Rubin,Tymczak,VanDeusen,Wheaton,Soundex,Example
```



## J

'''Solution'''

```j
removeDups =: {.;.1~ (1 , }. ~: }: )
codes  =: ;: 'BFPV CGJKQSXZ DT L MN R HW'

soundex =: 3 : 0
 if. 0=# k=.toupper y do. '0' return. end.
 ({.k), ,": ,. 3 {. 0-.~ }. removeDups 7 0:`(I.@:=)`]} , k >:@I.@:(e. &>)"0 _ codes
)
```

'''Usage'''

```j
names=: 'Lloyd Woolcock Donnell Baragwanath Williams Ashcroft Euler Ellery Gauss  Ghosh Hilbert Heilbronn Knuth Kant Ladd Lukasiewicz Lissajous'
soundexNames=: 'L300 W422 D540 B625 W452 A226 E460 E460 G200 G200 H416 H416 K530 K530 L300 L222 L222'

   soundex &> ;: names
L300
W422
D540
B625
W452
....
```

'''Test'''

```j
   soundexNames-:(soundex &.>) &. ;: names
1
```



## Java

{{trans|VBScript}}

```java
public static void main(String[] args){
    System.out.println(soundex("Soundex"));
    System.out.println(soundex("Example"));
    System.out.println(soundex("Sownteks"));
    System.out.println(soundex("Ekzampul"));
  }

private static String getCode(char c){
  switch(c){
    case 'B': case 'F': case 'P': case 'V':
      return "1";
    case 'C': case 'G': case 'J': case 'K':
    case 'Q': case 'S': case 'X': case 'Z':
      return "2";
    case 'D': case 'T':
      return "3";
    case 'L':
      return "4";
    case 'M': case 'N':
      return "5";
    case 'R':
      return "6";
    default:
      return "";
  }
}

public static String soundex(String s){
  String code, previous, soundex;
  code = s.toUpperCase().charAt(0) + "";

  // EDITED : previous = "7";
  previous = getCode(s.toUpperCase().charAt(0));

  for(int i = 1;i < s.length();i++){
    String current = getCode(s.toUpperCase().charAt(i));
    if(current.length() > 0 && !current.equals(previous)){
      code = code + current;
    }
    previous = current;
  }
  soundex = (code + "0000").substring(0, 4);
  return soundex;
}
```

{{out}}

```txt
S532
E251
S532
E251
```



## JavaScript



### ES5


### = Version w/o RegExp =


```javascript
var soundex = function (s) {
     var a = s.toLowerCase().split('')
         f = a.shift(),
         r = '',
         codes = {
             a: '', e: '', i: '', o: '', u: '',
             b: 1, f: 1, p: 1, v: 1,
             c: 2, g: 2, j: 2, k: 2, q: 2, s: 2, x: 2, z: 2,
             d: 3, t: 3,
             l: 4,
             m: 5, n: 5,
             r: 6
         };

     r = f +
         a
         .map(function (v, i, a) { return codes[v] })
         .filter(function (v, i, a) { return ((i === 0) ? v !== codes[f] : v !== a[i - 1]); })
         .join('');

     return (r + '000').slice(0, 4).toUpperCase();
};

var tests = {
  "Soundex":     "S532",
  "Example":     "E251",
  "Sownteks":    "S532",
  "Ekzampul":    "E251",
  "Euler":       "E460",
  "Gauss":       "G200",
  "Hilbert":     "H416",
  "Knuth":       "K530",
  "Lloyd":       "L300",
  "Lukasiewicz": "L222",
  "Ellery":      "E460",
  "Ghosh":       "G200",
  "Heilbronn":   "H416",
  "Kant":        "K530",
  "Ladd":        "L300",
  "Lissajous":   "L222",
  "Wheaton":     "W350",
  "Ashcraft":    "A226",
  "Burroughs":   "B622",
  "Burrows":     "B620",
  "O'Hara":      "O600"
  };

for (var i in tests)
  if (tests.hasOwnProperty(i)) {
    console.log(
      i +
      '    \t' +
      tests[i] +
      '\t' +
      soundex(i) +
      '\t' +
      (soundex(i) === tests[i])
    );
}

// Soundex     S532  S532  true
// Example     E251  E251  true
// Sownteks    S532  S532  true
// Ekzampul    E251  E251  true
// Euler       E460  E460  true
// Gauss       G200  G200  true
// Hilbert     H416  H416  true
// Knuth       K530  K530  true
// Lloyd       L300  L300  true
// Lukasiewicz L222  L222  true
// Ellery      E460  E460  true
// Ghosh       G200  G200  true
// Heilbronn   H416  H416  true
// Kant        K530  K530  true
// Ladd        L300  L300  true
// Lissajous   L222  L222  true
// Wheaton     W350  W350  true
// Ashcraft    A226  A226  true
// Burroughs   B622  B622  true
// Burrows     B620  B620  true
// O'Hara      O600  O600  true
```




### = Extended version w/ RegExp =


Note: This version differs from the one above in the following way. According to U.S. National Archives Website, consecutive consonants which map to the same code are not condensed to a single occurrence of the code if they are separated by vowels, but separating W and H do not thus intervene. Therefore Ashcraft is coded A261 and Burroughs is coded B620 rather than A226 and B622


```javascript

function soundex(t) {
  t = t.toUpperCase().replace(/[^A-Z]/g, '');
  return (t[0] || '0') + t.replace(/[HW]/g, '')
    .replace(/[BFPV]/g, '1')
    .replace(/[CGJKQSXZ]/g, '2')
    .replace(/[DT]/g, '3')
    .replace(/[L]/g, '4')
    .replace(/[MN]/g, '5')
    .replace(/[R]/g, '6')
    .replace(/(.)\1+/g, '$1')
    .substr(1)
    .replace(/[AEOIUHWY]/g, '')
    .concat('000')
    .substr(0, 3);
}

// tests
[ ["Example", "E251"], ["Sownteks", "S532"], ["Lloyd", "L300"], ["12346", "0000"],
 ["4-H", "H000"], ["Ashcraft", "A261"], ["Ashcroft", "A261"], ["auerbach", "A612"],
 ["bar", "B600"], ["barre", "B600"], ["Baragwanath", "B625"], ["Burroughs", "B620"],
 ["Burrows", "B620"], ["C.I.A.", "C000"], ["coöp", "C100"], ["D-day", "D000"],
 ["d jay", "D200"], ["de la Rosa", "D462"], ["Donnell", "D540"], ["Dracula", "D624"],
 ["Drakula", "D624"], ["Du Pont", "D153"], ["Ekzampul", "E251"], ["example", "E251"],
 ["Ellery", "E460"], ["Euler", "E460"], ["F.B.I.", "F000"], ["Gauss", "G200"],
 ["Ghosh", "G200"], ["Gutierrez", "G362"], ["he", "H000"], ["Heilbronn", "H416"],
 ["Hilbert", "H416"], ["Jackson", "J250"], ["Johnny", "J500"], ["Jonny", "J500"],
 ["Kant", "K530"], ["Knuth", "K530"], ["Ladd", "L300"], ["Lloyd", "L300"],
 ["Lee", "L000"], ["Lissajous", "L222"], ["Lukasiewicz", "L222"], ["naïve", "N100"],
 ["Miller", "M460"], ["Moses", "M220"], ["Moskowitz", "M232"], ["Moskovitz", "M213"],
 ["O'Conner", "O256"], ["O'Connor", "O256"], ["O'Hara", "O600"], ["O'Mally", "O540"],
 ["Peters", "P362"], ["Peterson", "P362"], ["Pfister", "P236"], ["R2-D2", "R300"],
 ["rÄ≈sumÅ∙", "R250"], ["Robert", "R163"], ["Rupert", "R163"], ["Rubin", "R150"],
 ["Soundex", "S532"], ["sownteks", "S532"], ["Swhgler", "S460"], ["'til", "T400"],
 ["Tymczak", "T522"], ["Uhrbach", "U612"], ["Van de Graaff", "V532"],
 ["VanDeusen", "V532"], ["Washington", "W252"], ["Wheaton", "W350"],
 ["Williams", "W452"], ["Woolcock", "W422"]
].forEach(function(v) {
  var a = v[0], t = v[1], d = soundex(a);
   if (d !== t) {
    console.log('soundex("' + a + '") was ' + d + ' should be ' + t);
  }
});
```



### ES6


Allowing for both Simple Soundex (first example above) and NARA Soundex (second example above)
(Reusing set of tests from second contribution)


```JavaScript
(() => {
    'use strict';

    // Simple Soundex or NARA Soundex (if blnNara = true)

    // soundex :: Bool -> String -> String
    const soundex = (blnNara, name) => {

        // code :: Char -> Char
        const code = c => ['AEIOU', 'BFPV', 'CGJKQSXZ', 'DT', 'L', 'MN', 'R', 'HW']
            .reduce((a, x, i) =>
                a ? a : (x.indexOf(c) !== -1 ? i.toString() : a), '');

        // isAlpha :: Char -> Boolean
        const isAlpha = c => {
            const d = c.charCodeAt(0);
            return d > 64 && d < 91;
        };

        const s = name.toUpperCase()
            .split('')
            .filter(isAlpha);

        return (s[0] || '0') +
            s.map(code)
            .join('')
            .replace(/7/g, blnNara ? '' : '7')
            .replace(/(.)\1+/g, '$1')
            .substr(1)
            .replace(/[07]/g, '')
            .concat('000')
            .substr(0, 3);
    };

    // curry :: ((a, b) -> c) -> a -> b -> c
    const curry = f => a => b => f(a, b),
        [simpleSoundex, naraSoundex] = [false, true]
        .map(bln => curry(soundex)(bln));

    // TEST
    return [
        ["Example", "E251"],
        ["Sownteks", "S532"],
        ["Lloyd", "L300"],
        ["12346", "0000"],
        ["4-H", "H000"],
        ["Ashcraft", "A261"],
        ["Ashcroft", "A261"],
        ["auerbach", "A612"],
        ["bar", "B600"],
        ["barre", "B600"],
        ["Baragwanath", "B625"],
        ["Burroughs", "B620"],
        ["Burrows", "B620"],
        ["C.I.A.", "C000"],
        ["coöp", "C100"],
        ["D-day", "D000"],
        ["d jay", "D200"],
        ["de la Rosa", "D462"],
        ["Donnell", "D540"],
        ["Dracula", "D624"],
        ["Drakula", "D624"],
        ["Du Pont", "D153"],
        ["Ekzampul", "E251"],
        ["example", "E251"],
        ["Ellery", "E460"],
        ["Euler", "E460"],
        ["F.B.I.", "F000"],
        ["Gauss", "G200"],
        ["Ghosh", "G200"],
        ["Gutierrez", "G362"],
        ["he", "H000"],
        ["Heilbronn", "H416"],
        ["Hilbert", "H416"],
        ["Jackson", "J250"],
        ["Johnny", "J500"],
        ["Jonny", "J500"],
        ["Kant", "K530"],
        ["Knuth", "K530"],
        ["Ladd", "L300"],
        ["Lloyd", "L300"],
        ["Lee", "L000"],
        ["Lissajous", "L222"],
        ["Lukasiewicz", "L222"],
        ["naïve", "N100"],
        ["Miller", "M460"],
        ["Moses", "M220"],
        ["Moskowitz", "M232"],
        ["Moskovitz", "M213"],
        ["O'Conner", "O256"],
        ["O'Connor", "O256"],
        ["O'Hara", "O600"],
        ["O'Mally", "O540"],
        ["Peters", "P362"],
        ["Peterson", "P362"],
        ["Pfister", "P236"],
        ["R2-D2", "R300"],
        ["rÄ≈sumÅ∙", "R250"],
        ["Robert", "R163"],
        ["Rupert", "R163"],
        ["Rubin", "R150"],
        ["Soundex", "S532"],
        ["sownteks", "S532"],
        ["Swhgler", "S460"],
        ["'til", "T400"],
        ["Tymczak", "T522"],
        ["Uhrbach", "U612"],
        ["Van de Graaff", "V532"],
        ["VanDeusen", "V532"],
        ["Washington", "W252"],
        ["Wheaton", "W350"],
        ["Williams", "W452"],
        ["Woolcock", "W422"]
    ].reduce((a, [name, naraCode]) => {
        const naraTest = naraSoundex(name),
            simpleTest = simpleSoundex(name);

        const logNara = naraTest !== naraCode ? (
                `${name} was ${naraTest} should be ${naraCode}`
            ) : '',
            logDelta = (naraTest !== simpleTest ? (
                `${name} -> NARA: ${naraTest} vs Simple: ${simpleTest}`
            ) : '');

        return logNara.length || logDelta.length ? (
            a + [logNara, logDelta].join('\n')
        ) : a;
    }, '');
})();
```


{{Out}}

```txt
Ashcraft -> NARA: A261 vs Simple: A226
Ashcroft -> NARA: A261 vs Simple: A226
Burroughs -> NARA: B620 vs Simple: B622
Swhgler -> NARA: S460 vs Simple: S246
```



## Julia

There is a Soundex package for Julia. If that is used:

```julia

using Soundex
@assert soundex("Ashcroft") == "A261"  # true

# Too trivial? OK. Here is an example not using a package:
function soundex(s)
    char2num = Dict('B'=>1,'F'=>1,'P'=>1,'V'=>1,'C'=>2,'G'=>2,'J'=>2,'K'=>2,
        'Q'=>2,'S'=>2,'X'=>2,'Z'=>2,'D'=>3,'T'=>3,'L'=>4,'M'=>5,'N'=>5,'R'=>6)
    s = replace(s, r"[^a-zA-Z]", "")
    if s == ""
        return ""
    end
    ret = "$(uppercase(s[1]))"
    hadvowel = false
    lastletternum = haskey(char2num, ret[1]) ? char2num[ret[1]] : ""
    for c in s[2:end]
        c = uppercase(c)
        if haskey(char2num, c)
            letternum = char2num[c]
            if letternum != lastletternum || hadvowel
                ret = "$ret$letternum"
                lastletternum = letternum
                hadvowel = false
            end
        elseif c in ('A', 'E', 'I', 'O', 'U', 'Y')
            hadvowel = true
        end
    end
    while length(ret) < 4
        ret *= "0"
    end
    ret[1:4]
end
@assert soundex("Ascroft")     == "A261"
@assert soundex("Euler")       == "E460"
@assert soundex("Gausss")      == "G200"
@assert soundex("Hilbert")     == "H416"
@assert soundex("Knuth")       == "K530"
@assert soundex("Lloyd")       == "L300"
@assert soundex("Lukasiewicz") == "L222"
@assert soundex("Ellery")      == "E460"
@assert soundex("Ghosh")       == "G200"
@assert soundex("Heilbronn")   == "H416"
@assert soundex("Kant")        == "K530"
@assert soundex("Ladd")        == "L300"
@assert soundex("Lissajous")   == "L222"
@assert soundex("Wheaton")     == "W350"
@assert soundex("Ashcraft")    == "A261"
@assert soundex("Burroughs")   == "B620"
@assert soundex("Burrows")     == "B620"
@assert soundex("O'Hara")      == "O600"

```



## Kotlin

{{trans|VBScript}}

```scala
// version 1.1.2

fun getCode(c: Char) = when (c) {
    'B', 'F', 'P', 'V' -> "1"
    'C', 'G', 'J', 'K', 'Q', 'S', 'X', 'Z' -> "2"
    'D', 'T' -> "3"
    'L' -> "4"
    'M', 'N' -> "5"
    'R' -> "6"
    'H', 'W' -> "-"
    else -> ""
}

fun soundex(s: String): String {
    if (s == "") return ""
    val sb = StringBuilder().append(s[0].toUpperCase())
    var prev = getCode(sb[0])
    for (i in 1 until s.length) {
        val curr = getCode(s[i].toUpperCase())
        if (curr != "" && curr != "-" && curr != prev) sb.append(curr)
        if (curr != "-") prev = curr
    }
    return sb.toString().padEnd(4, '0').take(4)
}

fun main(args: Array<String>) {
    val pairs = arrayOf(
        "Ashcraft"  to "A261",
        "Ashcroft"  to "A261",
        "Gauss"     to "G200",
        "Ghosh"     to "G200",
        "Hilbert"   to "H416",
        "Heilbronn" to "H416",
        "Lee"       to "L000",
        "Lloyd"     to "L300",
        "Moses"     to "M220",
        "Pfister"   to "P236",
        "Robert"    to "R163",
        "Rupert"    to "R163",
        "Rubin"     to "R150",
        "Tymczak"   to "T522",
        "Soundex"   to "S532",
        "Example"   to "E251"
    )
    for (pair in pairs) {
        println("${pair.first.padEnd(9)} -> ${pair.second} -> ${soundex(pair.first) == pair.second}")
    }
}
```


{{out}}

```txt

Ashcraft  -> A261 -> true
Ashcroft  -> A261 -> true
Gauss     -> G200 -> true
Ghosh     -> G200 -> true
Hilbert   -> H416 -> true
Heilbronn -> H416 -> true
Lee       -> L000 -> true
Lloyd     -> L300 -> true
Moses     -> M220 -> true
Pfister   -> P236 -> true
Robert    -> R163 -> true
Rupert    -> R163 -> true
Rubin     -> R150 -> true
Tymczak   -> T522 -> true
Soundex   -> S532 -> true
Example   -> E251 -> true

```




## Lua

Adapt from D Alternative

```Lua
local d, digits, alpha = '01230120022455012623010202', {}, ('A'):byte()
d:gsub(".",function(c)
  digits[string.char(alpha)] = c
  alpha = alpha + 1
end)

function soundex(w)
  local res = {}
  for c in w:upper():gmatch'.'do
    local d = digits[c]
    if d then
      if #res==0 then
        res[1] =  c
      elseif #res==1 or d~= res[#res] then
        res[1+#res] = d
      end
    end
  end
  if #res == 0 then
    return '0000'
  else
    res = table.concat(res):gsub("0",'')
    return (res .. '0000'):sub(1,4)
  end
end

-- tests
local tests = {
  {"",         "0000"}, {"12346",     "0000"},
  {"he",       "H000"}, {"soundex",   "S532"},
  {"example",  "E251"}, {"ciondecks", "C532"},
  {"ekzampul", "E251"}, {"rÃ©sumÃ©",  "R250"},
  {"Robert",   "R163"}, {"Rupert",    "R163"},
  {"Rubin",    "R150"}, {"Ashcraft",  "A226"},
  {"Ashcroft", "A226"}
}

for i=1,#tests do
  local itm = tests[i]
  assert( soundex(itm[1])==itm[2] )
end
print"all tests ok"
```

{{out}}

```txt
all tests ok
```



## Mathematica


```Mathematica
Soundex[ input_ ] := Module[{x = input, head, body},
{head, body} = {First@#, Rest@#}&@ToLowerCase@Characters@x;
body = (Select[body, FreeQ[Characters["aeiouyhw"],#]&] /. {("b"|"f"|"p"|"v")->1,
("c"|"g"|"j"|"k"|"q"|"s"|"x"|"z")->2, ("d"|"t")->3,"l"->4 ,("m"|"n")->5, "r"->6});
If[Length[body] < 3,
 body = PadRight[body, 3],
 body = DeleteDuplicates[body]
];
StringJoin @@ ToString /@ PrependTo[ body[[1 ;; 3]], ToUpperCase@head]]
```

Example usage:

```txt
Map[Soundex,{"Soundex", "Sownteks", "Example", "Ekzampul"}]
-> {S532, S532, E251, E251}
```



## MUMPS


```MUMPS
SOUNDEX(X,NARA=0)
 ;Converts a string to its Soundex value.
 ;Empty strings return "0000". Non-alphabetic ASCII characters are ignored.
 ;X is the name to be converted to Soundex
 ;NARA is a flag, defaulting to zero, for which implementation to perform.
 ;If NARA is 0, do what seems to be the Knuth implementation
 ;If NARA is a positive integer, do the NARA implementation.
 ; This varies the soundex rule for "W" and "H", and adds variants for prefixed names separated by carets.
 ; http://www.archives.gov/publications/general-info-leaflets/55-census.html
 ;Y is the string to be returned
 ;UP is the list of upper case letters
 ;LO is the list of lower case letters
 ;PREFIX is a list of prefixes to be stripped off
 ;X1 is the upper case version of X
 ;X2 is the name without a prefix
 ;Y2 is the soundex of a name without a prefix
 ;C is a loop variable
 ;DX is a list of Soundex values, in alphabetical order. Underscores are used for the NARA variation letters
 ;XD is a partially processed translation of X into soundex values
 NEW Y,UP,LO,PREFIX,X1,X2,Y2,C,DX,XD
 SET UP="ABCDEFGHIJKLMNOPQRSTUVWXYZ" ;Upper case characters
 SET LO="abcdefghijklmnopqrstuvwxyz" ;Lower case characters
 SET DX=" 123 12_ 22455 12623 1_2 2" ;Soundex values
 SET PREFIX="VAN^CO^DE^LA^LE" ;Prefixes that could create an alternate soundex value
 SET Y="" ;Y is the value to be returned
 SET X1=$TRANSLATE(X,LO,UP) ;Make local copy, and force all letters to be upper case
 SET XD=$TRANSLATE(X1,UP,DX) ;Soundex values for string
 ;
 SET Y=$EXTRACT(X1,1,1) ;Get first character
 FOR C=2:1:$LENGTH(X1) QUIT:$L(Y)>=4  DO
 . ;ignore doubled letters OR and side-by-side soundex values OR same soundex on either side of "H" or "W"
 . QUIT:($EXTRACT(X1,C,C)=$EXTRACT(X1,C-1,C-1))
 . QUIT:($EXTRACT(XD,C,C)=$EXTRACT(XD,C-1,C-1))
 . ;ignore non-alphabetic characters
 . QUIT:UP'[($EXTRACT(X1,C,C))
 . QUIT:NARA&(($EXTRACT(XD,C-1,C-1)="_")&(C>2))&($EXTRACT(XD,C,C)=$EXTRACT(XD,C-2,C-2))
 . QUIT:" _"[$EXTRACT(XD,C,C)
 . SET Y=Y_$EXTRACT(XD,C,C)
 ; Pad with "0" so string length is 4
 IF $LENGTH(Y)<4 FOR C=$L(Y):1:3 SET Y=Y_"0"
 IF NARA DO
 . FOR C=1:1:$LENGTH(PREFIX,"^") DO
 . . IF $EXTRACT(X1,1,$LENGTH($PIECE(PREFIX,"^",C)))=$PIECE(PREFIX,"^",C) DO
 . . . ;Take off the prefix, and any leading spaces
 . . . SET X2=$EXTRACT(X1,$LENGTH($PIECE(PREFIX,"^",C))+1,$LENGTH(X1)-$PIECE(PREFIX,"^",C)) FOR  QUIT:UP[$E(X2,1,1)  SET X2=$E(X2,2,$L(X2))
 . . . SET Y2=$$SOUNDEX(X2,NARA) SET Y=Y_"^"_Y2
 KILL UP,LO,PREFIX,X1,X2,Y2,C,DX,XD
 QUIT Y

```

<p>Examples:
```txt

USER>W $$SOUNDEX^SOUNDEX("")
0000
USER>W $$SOUNDEX^SOUNDEX("ASHCROFT")
A226
USER>W $$SOUNDEX^SOUNDEX("ASHCROFT",1)
A261
USER>W $$SOUNDEX^SOUNDEX("EULER")
E460
USER>W $$SOUNDEX^SOUNDEX("O'HARA")
O600
USER>W $$SOUNDEX^SOUNDEX("naïve")
N100
USER>W $$SOUNDEX^SOUNDEX("Moses")
M220
USER>W $$SOUNDEX^SOUNDEX("Omalley")
O540
USER>W $$SOUNDEX^SOUNDEX("O'Malley")
O540
USER>W $$SOUNDEX^SOUNDEX("Delarosa")
D462
USER>W $$SOUNDEX^SOUNDEX("Delarosa",1)
D462^L620^R200
USER>W $$SOUNDEX^SOUNDEX("De la Rosa")
D462
USER>W $$SOUNDEX^SOUNDEX("de la Rosa",1)
D462^L620^R200
USER>W $$SOUNDEX^SOUNDEX("Van de Graaff")
V532
USER>W $$SOUNDEX^SOUNDEX("Van de Graaff",1)
V532^D261^G610

```
</p>
<p>There's just one small problem...
```txt

USER>W $$SOUNDEX^SOUNDEX("fish")
F200
USER>W $$SOUNDEX^SOUNDEX("ghoti")
G300
```
</p>


## NetRexx

{{trans|Rexx}}

```NetRexx

class Soundex

  method get_soundex(in_) static
    in = in_.upper()
    old_alphabet= 'AEIOUYHWBFPVCGJKQSXZDTLMNR'
    new_alphabet= '@@@@@@**111122222222334556'
    word=''
    loop i=1 for in.length()
      tmp_=in.substr(i, 1) /*obtain a character from word*/
      if tmp_.datatype('M')  then word=word||tmp_
    end

    value=word.strip.left(1)                       /*1st character is left alone.*/
    word=word.translate(new_alphabet, old_alphabet) /*define the current  word.   */
    prev=value.translate(new_alphabet, old_alphabet) /*   "    "  previous   "     */

    loop j=2  to word.length()                      /*process remainder of word.  */
      q=word.substr(j, 1)
      if q\==prev & q.datatype('W')  then do
	value=value || q;  prev=q
      end
      else if q=='@'  then prev=q
    end   /*j*/

    return value.left(4,0)                           /*padded value with zeroes.   */

  method main(args=String[]) static

    test=''; result_=''
    test['1']= "12346"         ;        result_['1']= '0000'
    test['4']= "4-H"           ;        result_['4']= 'H000'
    test['11']= "Ashcraft"      ;        result_['11']= 'A261'
    test['12']= "Ashcroft"      ;        result_['12']= 'A261'
    test['18']= "auerbach"      ;        result_['18']= 'A612'
    test['20']= "Baragwanath"   ;        result_['20']= 'B625'
    test['22']= "bar"           ;        result_['22']= 'B600'
    test['23']= "barre"         ;        result_['23']= 'B600'
    test['20']= "Baragwanath"   ;        result_['20']= 'B625'
    test['28']= "Burroughs"     ;        result_['28']= 'B620'
    test['29']= "Burrows"       ;        result_['29']= 'B620'
    test['30']= "C.I.A."        ;        result_['30']= 'C000'
    test['37']= "coöp"          ;        result_['37']= 'C100'
    test['43']= "D-day"         ;        result_['43']= 'D000'
    test['44']= "d jay"         ;        result_['44']= 'D200'
    test['45']= "de la Rosa"    ;        result_['45']= 'D462'
    test['46']= "Donnell"       ;        result_['46']= 'D540'
    test['47']= "Dracula"       ;        result_['47']= 'D624'
    test['48']= "Drakula"       ;        result_['48']= 'D624'
    test['49']= "Du Pont"       ;        result_['49']= 'D153'
    test['50']= "Ekzampul"      ;        result_['50']= 'E251'
    test['51']= "example"       ;        result_['51']= 'E251'
    test['55']= "Ellery"        ;        result_['55']= 'E460'
    test['59']= "Euler"         ;        result_['59']= 'E460'
    test['60']= "F.B.I."        ;        result_['60']= 'F000'
    test['70']= "Gauss"         ;        result_['70']= 'G200'
    test['71']= "Ghosh"         ;        result_['71']= 'G200'
    test['72']= "Gutierrez"     ;        result_['72']= 'G362'
    test['80']= "he"            ;        result_['80']= 'H000'
    test['81']= "Heilbronn"     ;        result_['81']= 'H416'
    test['84']= "Hilbert"       ;        result_['84']= 'H416'
    test['100']= "Jackson"       ;        result_['100']= 'J250'
    test['104']= "Johnny"        ;        result_['104']= 'J500'
    test['105']= "Jonny"         ;        result_['105']= 'J500'
    test['110']= "Kant"          ;        result_['110']= 'K530'
    test['116']= "Knuth"         ;        result_['116']= 'K530'
    test['120']= "Ladd"          ;        result_['120']= 'L300'
    test['124']= "Llyod"         ;        result_['124']= 'L300'
    test['125']= "Lee"           ;        result_['125']= 'L000'
    test['126']= "Lissajous"     ;        result_['126']= 'L222'
    test['128']= "Lukasiewicz"   ;        result_['128']= 'L222'
    test['130']= "naïve"         ;        result_['130']= 'N100'
    test['141']= "Miller"        ;        result_['141']= 'M460'
    test['143']= "Moses"         ;        result_['143']= 'M220'
    test['146']= "Moskowitz"     ;        result_['146']= 'M232'
    test['147']= "Moskovitz"     ;        result_['147']= 'M213'
    test['150']= "O'Conner"      ;        result_['150']= 'O256'
    test['151']= "O'Connor"      ;        result_['151']= 'O256'
    test['152']= "O'Hara"        ;        result_['152']= 'O600'
    test['153']= "O'Mally"       ;        result_['153']= 'O540'
    test['161']= "Peters"        ;        result_['161']= 'P362'
    test['162']= "Peterson"      ;        result_['162']= 'P362'
    test['165']= "Pfister"       ;        result_['165']= 'P236'
    test['180']= "R2-D2"         ;        result_['180']= 'R300'
    test['182']= "rÄ≈sumÅ∙"      ;        result_['182']= 'R250'
    test['184']= "Robert"        ;        result_['184']= 'R163'
    test['185']= "Rupert"        ;        result_['185']= 'R163'
    test['187']= "Rubin"         ;        result_['187']= 'R150'
    test['191']= "Soundex"       ;        result_['191']= 'S532'
    test['192']= "sownteks"      ;        result_['192']= 'S532'
    test['199']= "Swhgler"       ;        result_['199']= 'S460'
    test['202']= "'til"          ;        result_['202']= 'T400'
    test['208']= "Tymczak"       ;        result_['208']= 'T522'
    test['216']= "Uhrbach"       ;        result_['216']= 'U612'
    test['221']= "Van de Graaff" ;        result_['221']= 'V532'
    test['222']= "VanDeusen"     ;        result_['222']= 'V532'
    test['230']= "Washington"    ;        result_['230']= 'W252'
    test['233']= "Wheaton"       ;        result_['233']= 'W350'
    test['234']= "Williams"      ;        result_['234']= 'W452'
    test['236']= "Woolcock"      ;        result_['236']= 'W422'

    loop i over test
      say test[i].left(10) get_soundex(test[i]) '=' result_[i]
    end

```

{{out}}

```txt

barre      B600 = B600
Wheaton    W350 = W350
Knuth      K530 = K530
auerbach   A612 = A612
Ekzampul   E251 = E251
D-day      D000 = D000
example    E251 = E251
4-H        H000 = H000
Burroughs  B620 = B620
d jay      D200 = D200
F.B.I.     F000 = F000
Lissajous  L222 = L222
Burrows    B620 = B620
coöp       C100 = C100
de la Rosa D462 = D462
Gauss      G200 = G200
Donnell    D540 = D540
Ghosh      G200 = G200
Dracula    D624 = D624
Ellery     E460 = E460
he         H000 = H000
Gutierrez  G362 = G362
Drakula    D624 = D624
Williams   W452 = W452
Heilbronn  H416 = H416
Du Pont    D153 = D153
Robert     R163 = R163
Pfister    P236 = P236
Moskowitz  M232 = M232
Euler      E460 = E460
Hilbert    H416 = H416
Rupert     R163 = R163
Uhrbach    U612 = U612
Moskovitz  M213 = M213
Lukasiewic L222 = L222
Woolcock   W422 = W422
Tymczak    T522 = T522
Rubin      R150 = R150
Swhgler    S460 = S460
Jackson    J250 = J250
Kant       K530 = K530
Ladd       L300 = L300
naïve      N100 = N100
O'Conner   O256 = O256
Miller     M460 = M460
O'Connor   O256 = O256
Washington W252 = W252
R2-D2      R300 = R300
Peters     P362 = P362
Van de Gra V532 = V532
Johnny     J500 = J500
'til       T400 = T400
O'Hara     O600 = O600
Peterson   P362 = P362
Moses      M220 = M220
Llyod      L300 = L300
Soundex    S532 = S532
VanDeusen  V532 = V532
Jonny      J500 = J500
O'Mally    O540 = O540
12346       000 = 0000
Ashcraft   A261 = A261
rÄ≈sumÅ∙   R250 = R250
Ashcroft   A261 = A261
Baragwanat B625 = B625
Lee        L000 = L000
bar        B600 = B600
C.I.A.     C000 = C000
sownteks   S532 = S532

```



## Objeck

{{trans|Java}}


```objeck
class SoundEx {
  function : Main(args : String[]) ~ Nil {
    SoundEx("Soundex")->PrintLine();
      SoundEx("Example")->PrintLine();
      SoundEx("Sownteks")->PrintLine();
      SoundEx("Ekzampul")->PrintLine();
  }

  function : SoundEx(s : String) ~ String {
    input := s->ToUpper()->Get(0);
    code := input->ToString();
    previous := GetCode(input);

    for(i := 1; i < s->Size(); i += 1;) {
      current := GetCode(s->ToUpper()->Get(i));
      if(current->Size() > 0 & <>current->Equals(previous)) {
        code += current;
      };
      previous := current;
    };

    soundex := String->New(code);
    soundex += "0000";
    return soundex->SubString(4);
  }

  function : GetCode(c : Char) ~ String {
    select(c) {
        label 'B': label 'F':
        label 'P': label 'V': {
        return "1";
      }

      label 'C': label 'G':
      label 'J': label 'K':
      label 'Q': label 'S':
      label 'X': label 'Z': {
        return "2";
      }

      label 'D': label 'T': {
        return "3";
      }

      label 'L': {
        return "4";
      }

      label 'M': label 'N': {
        return "5";
      }

      label 'R': {
        return "6";
      }

      other: {
        return "";
      }
    };
  }
}

```


{{out}}

```txt

S532
E251
S532
E251

```



## OCaml


Here is an implementation:


```ocaml
let c2d = function
  | 'B' | 'F' | 'P' | 'V' -> "1"
  | 'C' | 'G' | 'J' | 'K' | 'Q' | 'S' | 'X' | 'Z' -> "2"
  | 'D' | 'T' -> "3"
  | 'L' -> "4"
  | 'M' | 'N' -> "5"
  | 'R' -> "6"
  | _ -> ""

let rec dbl acc = function
  | [] -> (List.rev acc)
  | [c] -> List.rev(c::acc)
  | c1::(c2::_ as tl) ->
      if c1 = c2
      then dbl acc tl
      else dbl (c1::acc) tl

let pad s =
  match String.length s with
  | 0 -> s ^ "000"
  | 1 -> s ^ "00"
  | 2 -> s ^ "0"
  | 3 -> s
  | _ -> String.sub s 0 3

let soundex_aux rem =
  pad(String.concat "" (dbl [] (List.map c2d rem)))

let soundex s =
  let s = String.uppercase s in
  let cl = ref [] in
  String.iter (fun c -> cl := c :: !cl) s;
  match dbl [] (List.rev !cl) with
  | c::rem -> (String.make 1 c) ^ (soundex_aux rem)
  | [] -> invalid_arg "soundex"
```


Test our implementation:


```ocaml
let tests = [
  "Soundex",     "S532";
  "Example",     "E251";
  "Sownteks",    "S532";
  "Ekzampul",    "E251";
  "Euler",       "E460";
  "Gauss",       "G200";
  "Hilbert",     "H416";
  "Knuth",       "K530";
  "Lloyd",       "L300";
  "Lukasiewicz", "L222";
  "Ellery",      "E460";
  "Ghosh",       "G200";
  "Heilbronn",   "H416";
  "Kant",        "K530";
  "Ladd",        "L300";
  "Lissajous",   "L222";
  "Wheaton",     "W350";
  "Ashcraft",    "A226";
  "Burroughs",   "B622";
  "Burrows",     "B620";
  "O'Hara",      "O600";
  ]

let () =
  print_endline " Word   \t Code  Found Status";
  List.iter (fun (word, code1) ->
    let code2 = soundex word in
    let status = if code1 = code2 then "OK " else "Arg" in
    Printf.printf " \"%s\" \t %s  %s  %s\n" word code1 code2 status
  ) tests
```


{{out}}

```txt

 Word            Code  Found Status
 "Soundex"       S532  S532  OK
 "Example"       E251  E251  OK
 "Sownteks"      S532  S532  OK
 "Ekzampul"      E251  E251  OK
 "Euler"         E460  E460  OK
 "Gauss"         G200  G200  OK
 "Hilbert"       H416  H416  OK
 "Knuth"         K530  K530  OK
 "Lloyd"         L300  L300  OK
 "Lukasiewicz"   L222  L222  OK
 "Ellery"        E460  E460  OK
 "Ghosh"         G200  G200  OK
 "Heilbronn"     H416  H416  OK
 "Kant"          K530  K530  OK
 "Ladd"          L300  L300  OK
 "Lissajous"     L222  L222  OK
 "Wheaton"       W350  W350  OK
 "Ashcraft"      A226  A226  OK
 "Burroughs"     B622  B622  OK
 "Burrows"       B620  B620  OK
 "O'Hara"        O600  O600  OK

```


See [[Soundex/OCaml]] for a version that can switch the language (English, French...) with a type which definition is hidden in the interface.


## Pascal


{{works with|Free Pascal|2.6.2}}


```Pascal
program Soundex;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
   SysUtils;

type
  TLang=(en,fr,de);

const
   Examples : array[1..16, 1..2] of string =
     (('Ashcraft', 'A261')
     ,('Ashcroft', 'A261')
     ,('Gauss', 'G200')
     ,('Ghosh', 'G200')
     ,('Hilbert', 'H416')
     ,('Heilbronn', 'H416')
     ,('Lee', 'L000')
     ,('Lloyd', 'L300')
     ,('Moses', 'M220')
     ,('Pfister', 'P236')
     ,('Robert', 'R163')
     ,('Rupert', 'R163')
     ,('Rubin', 'R150')
     ,('Tymczak', 'T522')
     ,('Soundex', 'S532')
     ,('Example', 'E251')
     );

// For Ansi Str
function Soundex(Value: String; Lang: TLang) : String;
const
  // Thx to WP.
  Map: array[TLang, 0..2] of String =(
    // Deals with accented, to improve
    ('abcdefghijklmnopqrstuvwxyz'
    ,'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
    ,' 123 12- 22455 12623 1-2 2'),
    ('aàâäbcçdeéèêëfghiîjklmnoöôpqrstuùûüvwxyz' // all chars with accented
    ,'AAAABCCDEEEEEFGHIIJKLMNOOOPQRSTUUUUVWXYZ' // uppercased
    ,' 123 97- 72455 12683 9-8 8'),             // coding
    ('abcdefghijklmnopqrstuvwxyz'
    ,'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
    ,' 123 12- 22455 12623 1-2 2')
    );
var
  i: Integer;
  c, cOld: Char;

  function Normalize(const s: string): string;
  var
    c: Char;
    p: Integer;
  begin
    result := '';
    for c in LowerCase(s) do
    begin
      p := Pos(c, Map[Lang,0]);
      // unmapped chars are ignored
      if p > 0 then
        Result := Result + Map[Lang, 1][p];
    end;
  End;

  function GetCode(c: Char): Char;
  begin
    Result := Map[Lang, 2][Ord(c)-Ord('A')+1];
  End;

begin
  Value := Trim(Value);
  if Value = '' then
  begin
    Result := '0000';
    exit;
  end;
  Value := Normalize(Value);
  Result := Value[1];
  cOld := GetCode(Value[1]);
  for i := 2 to length(Value) do
  begin
    c := GetCode(Value[i]);
    if (c <> ' ') and (c <> '-') and (c <> cOld) then
      Result := Result + c;
    if c <> '-' then
      cOld := c;
  end;
  Result := Copy(Result+'0000', 1, 4);
End;

const
  Status : array[boolean] of string = ('KO', 'OK');
var
  Found: String;
  tab: array[1..2] of String;
begin
  WriteLn('Word                : Code   Found   Status');
  for tab in Examples do
  begin
    Found := Soundex(tab[1], en);
    WriteLn(Format('%-20s: %s   %s    %s',[tab[1], tab[2], Found, Status[Found = tab[2]]]))
  end;
  ReadLn;
End.
```


{{out}}

```txt
Word                : Code   Found   Status
Ashcraft            : A261   A261    OK
Ashcroft            : A261   A261    OK
Gauss               : G200   G200    OK
Ghosh               : G200   G200    OK
Hilbert             : H416   H416    OK
Heilbronn           : H416   H416    OK
Lee                 : L000   L000    OK
Lloyd               : L300   L300    OK
Moses               : M220   M220    OK
Pfister             : P236   P236    OK
Robert              : R163   R163    OK
Rupert              : R163   R163    OK
Rubin               : R150   R150    OK
Tymczak             : T522   T522    OK
Soundex             : S532   S532    OK
Example             : E251   E251    OK
```



## Perl

The <tt>Text::Soundex</tt> core module supports various soundex algorithms.

```perl
use Text::Soundex;
print soundex("Soundex"), "\n"; # S532
print soundex("Example"), "\n"; # E251
print soundex("Sownteks"), "\n"; # S532
print soundex("Ekzampul"), "\n"; # E251
```



## Perl 6

US census algorithm, so "Ashcraft" and "Burroughs" adjusted to match.
We fake up a first consonant in some cases to make up for the fact that we always trim the first numeric code (so that the 'l' of 'Lloyd' is properly deleted).

```perl6
sub soundex ($name --> Str) {
    my $first = substr($name,0,1).uc;
    gather {
        take $first;
        my $fakefirst = '';
        $fakefirst = "de " if $first ~~ /^ <[AEIOUWH]> /;
        "$fakefirst$name".lc.trans('wh' => '') ~~ /
            ^
            [
                [
                | <[ bfpv     ]>+ { take 1 }
                | <[ cgjkqsxz ]>+ { take 2 }
                | <[ dt       ]>+ { take 3 }
                | <[ l        ]>+ { take 4 }
                | <[ mn       ]>+ { take 5 }
                | <[ r        ]>+ { take 6 }
                ]
            || .
            ]+
            $ { take 0,0,0 }
        /;
    }.flat.[0,2,3,4].join;
}

for < Soundex     S532
      Example     E251
      Sownteks    S532
      Ekzampul    E251
      Euler       E460
      Gauss       G200
      Hilbert     H416
      Knuth       K530
      Lloyd       L300
      Lukasiewicz L222
      Ellery      E460
      Ghosh       G200
      Heilbronn   H416
      Kant        K530
      Ladd        L300
      Lissajous   L222
      Wheaton     W350
      Ashcraft    A261
      Burroughs   B620
      Burrows     B620
      O'Hara      O600 >
-> $n, $s {
    my $s2 = soundex($n);
    say $n.fmt("%16s "), $s, $s eq $s2 ?? " OK" !! " NOT OK $s2";
}
```

{{out}}

```txt
         Soundex S532 OK
         Example E251 OK
        Sownteks S532 OK
        Ekzampul E251 OK
           Euler E460 OK
           Gauss G200 OK
         Hilbert H416 OK
           Knuth K530 OK
           Lloyd L300 OK
     Lukasiewicz L222 OK
          Ellery E460 OK
           Ghosh G200 OK
       Heilbronn H416 OK
            Kant K530 OK
            Ladd L300 OK
       Lissajous L222 OK
         Wheaton W350 OK
        Ashcraft A261 OK
       Burroughs B620 OK
         Burrows B620 OK
          O'Hara O600 OK
```



## Phix


```Phix
constant soundex_alphabet = "0123012#02245501262301#202"
                        --   ABCDEFGHIJKLMNOPQRSTUVWXYZ

function soundex(string name)
    string res = "0000"
    integer rdx = 1, ch, this, prev
    for i=1 to length(name) do
        ch = upper(name[i])
        if ch>='A' and ch<='Z' then
            this = soundex_alphabet[ch-'A'+1]
            if rdx=1 then
                res[1] = ch
                rdx = 2
                prev = this
            elsif this!='#' then
                if this!='0' and this!=prev then
                    res[rdx] = this
                    if rdx=4 then exit end if
                    rdx += 1
                end if
                prev = this
            end if
        end if
    end for
    return res
end function

constant tests = {
                    {"Ashcraft",    "A261"},    -- not "A226"
                    {"Ashcroft",    "A261"},    -- not "A226"
                    {"Ashkrofd",    "A261"},    -- not "A226"
                    {"Burroughs",   "B620"},
                    {"Burrows",     "B620"},
                    {"ciondecks",   "C532"},
                    {"Example",     "E251"},
                    {"Ekzampul",    "E251"},
                    {"Ellery",      "E460"},
                    {"Euler",       "E460"},
                    {"Gauss",       "G200"},
                    {"Ghosh",       "G200"},
                    {"Gutierrez",   "G362"},
                    {"He",          "H000"},
                    {"Heilbronn",   "H416"},
                    {"Hilbert",     "H416"},
                    {"Honeyman",    "H555"},    -- not "H500"
                    {"Jackson",     "J250"},
                    {"Kant",        "K530"},
                    {"Knuth",       "K530"},
                    {"Lee",         "L000"},
                    {"Ladd",        "L300"},
                    {"Lloyd",       "L300"},
                    {"Lissajous",   "L222"},
                    {"Lukasiewicz", "L222"},
                    {"Moses",       "M220"},
                    {"O'Hara",      "O600"},
                    {"Pfister",     "P236"},    -- not "P123"
                    {"Robert",      "R163"},
                    {"Rupert",      "R163"},
                    {"Rubin",       "R150"},
                    {"r~@sum~@",    "R250"},
                    {"Soundex",     "S532"},
                    {"Sownteks",    "S532"},
                    {"Tymczak",     "T522"},    -- not "T520"
                    {"VanDeusen",   "V532"},
                    {"Washington",  "W252"},
                    {"Wheaton",     "W350"},
                    {"Weeton",      "W350"},
                    {"",            "0000"},
                    {"  ",          "0000"},
                    {"12346",       "0000"},
                    {"aaa a",       "A000"}
                }

for i=1 to length(tests) do
    string {name,expected} = tests[i],
           res = soundex(name),
           ok = iff(res=expected?"":"*** ERROR ***")
    printf(1,"%-12s -> %s %s\n",{name,res,ok})
end for
```

{{out}}

```txt

Ashcraft     -> A261
Ashcroft     -> A261
Ashkrofd     -> A261
Burroughs    -> B620
Burrows      -> B620
ciondecks    -> C532
Example      -> E251
Ekzampul     -> E251
Ellery       -> E460
Euler        -> E460
Gauss        -> G200
Ghosh        -> G200
Gutierrez    -> G362
He           -> H000
Heilbronn    -> H416
Hilbert      -> H416
Honeyman     -> H555
Jackson      -> J250
Kant         -> K530
Knuth        -> K530
Lee          -> L000
Ladd         -> L300
Lloyd        -> L300
Lissajous    -> L222
Lukasiewicz  -> L222
Moses        -> M220
O'Hara       -> O600
Pfister      -> P236
Robert       -> R163
Rupert       -> R163
Rubin        -> R150
r~@sum~@     -> R250
Soundex      -> S532
Sownteks     -> S532
Tymczak      -> T522
VanDeusen    -> V532
Washington   -> W252
Wheaton      -> W350
Weeton       -> W350
             -> 0000
             -> 0000
12346        -> 0000
aaa a        -> A000

```



## PHP

PHP already has a built-in <tt>soundex()</tt> function:

```php
<?php
echo soundex("Soundex"), "\n"; // S532
echo soundex("Example"), "\n"; // E251
echo soundex("Sownteks"), "\n"; // S532
echo soundex("Ekzampul"), "\n"; // E251
?>
```



## PicoLisp

Simple:

```PicoLisp
(de soundex (Str)
   (pack
      (pad -4
         (cons
            (uppc (char (char Str)))
            (head 3
               (let Last NIL
                  (extract
                     '((C)
                        (and
                           (setq C
                              (case (uppc C)
                                 (`(chop "BFPV") "1")
                                 (`(chop "CGJKQSXZ") "2")
                                 (("D" "T") "3")
                                 ("L" "4")
                                 (("M" "N") "5")
                                 ("R" "6") ) )
                           (<> Last C)
                           (setq Last C) ) )
                     (cdr (chop Str)) ) ) ) ) ) ) )
```

NARA:

```PicoLisp
(de soundex (Str)
   (let (Str (chop Str) Last)
      (pack
         (pad
            -4
            (cons
               (uppc (car Str))
               (head
                  3
                  (filter
                     gt0
                     (cdr
                        (mapcar
                           '((C)
                              (and
                                 (setq C
                                    (case (uppc C)
                                       (`(chop "AEIOUY") 0)
                                       (`(chop "BFPV") 1)
                                       (`(chop "CGJKQSXZ") 2)
                                       (("D" "T") 3)
                                       ("L" 4)
                                       (("M" "N") 5)
                                       ("R" 6) ) )
                                 (<> Last C)
                                 (setq Last C) ) )
                           Str ) ) ) ) ) ) ) ) )
```



## PL/I


```PL/I
Soundex: procedure (pword) returns (character(4));
   declare pword character (*) varying, value character (length(pword)) varying;
   declare word character (length(pword));
   declare (prevCode, currCode) character (1);
   declare alphabet CHARACTER (26) STATIC INITIAL ('AEIOUHWYBFPVCGJKQSXZDTLMNR');
   declare replace  character (26) static initial ('00000000111122222222334556');
   declare i fixed binary;

   word = pword;

   /* Buffer to build up with character codes */
   value = '';

   /* Make sure the word is at least two characters in length. */
   if length(word) <= 1 then return (word);

   word = uppercase(word); /* Convert to uppercase. */

   /* The current and previous character codes */
   prevCode = '0';

   value = substr(word, 1, 1); /* The first character is unchanged. */

   word = Translate (word, replace, alphabet);

   /* Loop through the remaining characters ... */
   do i = 2 to length(word);
      currCode = substr(word, i, 1);
      /* Check to see if the current code is the same as the last one */
      if currCode ^= prevCode & currCode ^= '0' then
         /* If the current code is a vowel, ignore it. */
         value = value || currCode;
      /* Set the new previous character code */
      prevCode = currCode;
   end; /* of do i = ... */

   return ( left(value, 4, '0') ); /* Pad, if necessary. */

end Soundex;
```



## PowerShell

{{works with|PowerShell 3.0}}

```PowerShell

function Get-Soundex
{
    [CmdletBinding()]
    [OutputType([PSCustomObject])]
    Param
    (
        [Parameter(Mandatory=$true,
                   ValueFromPipeline=$true,
                   ValueFromPipelineByPropertyName=$true,
                   Position=0)]
        [string[]]
        $InputObject
    )

    Begin
    {
        $characterGroup = [PSCustomObject]@{
            1 = @('B','F','P','V')
            2 = @('C','G','J','K','Q','S','X','Z')
            3 = @('D','T')
            4 = @('L')
            5 = @('M','N')
            6 = @('R')
        }

        function ConvertTo-SoundexDigit ([char]$Character)
        {
            switch ($Character)
            {
                {$_ -in $characterGroup.1} {return 1}
                {$_ -in $characterGroup.2} {return 2}
                {$_ -in $characterGroup.3} {return 3}
                {$_ -in $characterGroup.4} {return 4}
                {$_ -in $characterGroup.5} {return 5}
                {$_ -in $characterGroup.6} {return 6}
                Default                    {return 0}
            }
        }
    }
    Process
    {
        foreach ($String in $InputObject)
        {
            $originalString = $String
            $String = $String.ToUpper()
            $isHorWcharacter = $false
            $soundex = New-Object -TypeName System.Text.StringBuilder

            $soundex.Append($String[0]) | Out-Null

            for ($i = 1; $i -lt $String.Length; $i++)
            {
                $currentCharacterDigit = ConvertTo-SoundexDigit $String[$i]

                if ($currentCharacterDigit -ne 0)
                {
                    if ($i -eq (ConvertTo-SoundexDigit $String[$i-1]))
                    {
                        continue
                    }

                    if (($i -gt 2) -and ($isHorWcharacter) -and ($currentCharacterDigit -eq (ConvertTo-SoundexDigit $String[$i-2])))
                    {
                        continue
                    }

                    $soundex.Append($currentCharacterDigit) | Out-Null
                }

                $isHorWcharacter = $String[$i] -in @('H','W')
            }

            $soundexTail = ($soundex.ToString().Substring(1)).TrimStart((ConvertTo-SoundexDigit $String[0]).ToString())

            [PSCustomObject]@{
                String  = $originalString
                Soundex = ($soundex[0] + $soundexTail).PadRight(4,"0").Substring(0,4)
            }
        }
    }
}

```


```PowerShell

"Ashcraft", "Ashcroft", "Gauss", "Ghosh", "Hilbert", "Heilbronn", "Lee", "Lloyd",
"Moses", "Pfister", "Robert", "Rupert", "Rubin", "Tymczak", "Soundex", "Example" | Get-Soundex

```

{{Out}}

```txt

String    Soundex
------    -------
Ashcraft  A261
Ashcroft  A261
Gauss     G000
Ghosh     G000
Hilbert   H416
Heilbronn H465
Lee       L000
Lloyd     L300
Moses     M220
Pfister   P236
Robert    R163
Rupert    R163
Rubin     R150
Tymczak   T522
Soundex   S532
Example   E251

```


### Alternative Version

Here we're using as much PowerShell native functionaity as possible, without reaching deep into .NET libraries.  The goal here is to have script that can be called from the prompt to be easily used in other scripts.

```PowerShell

# script Soundex.ps1
Param([string]$Phrase)
Process {
    $src = $Phrase.ToUpper().Trim()
    $coded = $src[0..($src.Length - 1)] | %{
        if('BFPV'.Contains($_)) { '1' }
        elseif('CGJKQSXZ'.Contains($_)) { '2' }
        elseif('DT'.Contains($_)) { '3' }
        elseif('L'.Contains($_)) { '4' }
        elseif('MN'.Contains($_)) { '5' }
        elseif('R'.Contains($_)) { '6' }
        elseif('AEIOU'.Contains($_)) { 'v' }
        else { '.' }
    } | Where { $_ -ne '.'}
    $coded2 = 0..($coded.Length - 1) | %{ if ($_ -eq 0 -or $coded[$_] -ne $coded[$_ - 1]) { $coded[$_] } else { '' } }
    $coded2 = if ($coded[0] -eq 'v' -or $coded2[0] -ne $coded[0]) { $coded2 } else { $coded2[1..($coded2.Length - 1)] }
    $src[0] + ((-join $($coded2 | Where { $_ -ne 'v'})) + "000").Substring(0,3)
}

```



```PowerShell

 Function t([string]$value, [string]$expect) {
    $result = .\Soundex.ps1 -Phrase $value
    New-Object –TypeName PSObject –Prop @{ "Value"=$value; "Expect"=$expect; "Result"=$result; "Pass"=$($expect -eq $result) }
}
@(
(t "Ashcraft" "A261"); (t "Ashcroft" "A261"); (t "Burroughs" "B620"); (t "Burrows" "B620");
(t "Ekzampul" "E251"); (t "Example" "E251"); (t "Ellery" "E460"); (t "Euler" "E460");
(t "Ghosh" "G200"); (t "Gauss" "G200"); (t "Gutierrez" "G362"); (t "Heilbronn" "H416");
(t "Hilbert" "H416"); (t "Jackson" "J250"); (t "Kant" "K530"); (t "Knuth" "K530");
(t "Lee" "L000"); (t "Lukasiewicz" "L222"); (t "Lissajous" "L222"); (t "Ladd" "L300");
(t "Lloyd" "L300"); (t "Moses" "M220"); (t "O'Hara" "O600"); (t "Pfister" "P236");
(t "Rubin" "R150"); (t "Robert" "R163"); (t "Rupert" "R163"); (t "Soundex" "S532");
(t "Sownteks" "S532"); (t "Tymczak" "T522"); (t "VanDeusen" "V532"); (t "Washington" "W252");
(t "Wheaton" "W350");
) | Format-Table -Property Value,Expect,Result,Pass

```

{{Out}}

```txt

Value       Expect Result Pass
-----       ------ ------ ----
Ashcraft    A261   A261   True
Ashcroft    A261   A261   True
Burroughs   B620   B620   True
Burrows     B620   B620   True
Ekzampul    E251   E251   True
Example     E251   E251   True
Ellery      E460   E460   True
Euler       E460   E460   True
Ghosh       G200   G200   True
Gauss       G200   G200   True
Gutierrez   G362   G362   True
Heilbronn   H416   H416   True
Hilbert     H416   H416   True
Jackson     J250   J250   True
Kant        K530   K530   True
Knuth       K530   K530   True
Lee         L000   L000   True
Lukasiewicz L222   L222   True
Lissajous   L222   L222   True
Ladd        L300   L300   True
Lloyd       L300   L300   True
Moses       M220   M220   True
O'Hara      O600   O600   True
Pfister     P236   P236   True
Rubin       R150   R150   True
Robert      R163   R163   True
Rupert      R163   R163   True
Soundex     S532   S532   True
Sownteks    S532   S532   True
Tymczak     T522   T522   True
VanDeusen   V532   V532   True
Washington  W252   W252   True
Wheaton     W350   W350   True

```



## Prolog

Note: Rather than produce a terse and incomprehensible example, this demonstrates how simply a set of logical rules can be translated into Prolog.

```Prolog
%____________________________________________________________________
% Implements the American soundex algorithm
%   as described at https://en.wikipedia.org/wiki/Soundex
%  In SWI Prolog, a 'string' is specified in 'single quotes',
%    while a "list of codes" may be specified in "double quotes".
%  So, "abc" is equivalent to [97, 98, 99], while
%    'abc' = abc  (an atom), and 'Abc' is also an atom.  There are
%    conversion methods that can produce lists of characters:
%        ?- atom_chars('Abc', X).
%	 X = ['A', b, c].
%    or lists of codes (mapping to unicode code points):
%        ?- atom_codes('Abc', X).
%        X = [65, 98, 99].
%    and the conversion predicates are bidirectional.
%        ?- atom_codes(A, [65, 98, 99]).
%        A = 'Abc'.
%  A single character code may be specified as 0'C, where C is the
%    character you want to convert to a code.
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

%  Relates groups of consonants to representative digits
creplace(Ch, 0'1) :- member(Ch, "bfpv").
creplace(Ch, 0'2) :- member(Ch, "cgjkqsxz").
creplace(Ch, 0'3) :- member(Ch, "dt").
creplace(0'l, 0'4).
creplace(Ch, 0'5) :- member(Ch, "mn").
creplace(0'r, 0'6).

% strips elements contained in <Set> from a string
strip(Set, [H|T], Tr) :- memberchk(H, Set), !, strip(Set, T, Tr).
strip(Set, [H|T], [H|Tr]) :- !, strip(Set, T, Tr).
strip(_, [], []).

% Replace consonants with appropriate digits
consonants([H|T], [Ch|Tr]) :- creplace(H, Ch), !, consonants(T, Tr).
consonants([H|T], [H|Tr]) :- !, consonants(T, Tr).
consonants([], []).

% Replace adjacent digits with single digit
adjacent([Ch, Ch|T], [Ch|Tr]) :- between(0'0, 0'9, Ch), !, adjacent(T, Tr).
adjacent([H|T], [H|Tr]) :- !, adjacent(T, Tr).
adjacent([], []).

% Replace first character with original one if its a digit
chk_digit([H,D|T], [H|T]) :- between(0'0, 0'9, D), !.
chk_digit([_,H|T], [H|T]).

% Faithul representation of soundex rules:
%   1: Save 1st letter, strip "hw"
%   2: Replace consonants with appropriate digits
%   3: Replace adjacent digits with single occurrence
%   4: Remove vowels except 1st letter
%   5: If 1st symbol is a digit, replace it with saved 1st letter
%   6: Ensure trailing zeroes
do_soundex([H|T], Res) :-
	strip("hw", T, Ts), consonants([H|Ts], Tc),
	adjacent(Tc, [C|Ta]), strip("aeiouy", Ta, Tv),
	chk_digit([H,C|Tv], Td), append(Td, "0000", Tr),
	atom_codes(Tf, Tr), sub_string(Tf, 0, 4, _, Res).

% Prepare string, convert to lower case and do the soundex alogorithm
soundex(Text, Res) :-
	downcase_atom(Text, Lower), atom_codes(Lower, T),
	do_soundex(T, Res).

% Perform tests to check that the right values are produced
test(S,V) :- not(soundex(S,V)), writef('%w failed\n', [S]).
test :- test('Robert', 'r163'), !, fail.
test :- test('Rupert', 'r163'), !, fail.
test :- test('Rubin', 'r150'), !, fail.
test :- test('Ashcroft', 'a261'), !, fail.
test :- test('Ashcraft', 'a261'), !, fail.
test :- test('Tymczak', 't522'), !, fail.
test :- test('Pfister', 'p236'), !, fail.
test.  % Succeeds only if all the tests succeed
```



## PureBasic


```PureBasic
Procedure.s getCode(c.s)
    Protected  getCode.s = ""

    If FindString("BFPV", c ,1)     : getCode = "1" : EndIf
    If FindString("CGJKQSXZ", c ,1) : getCode = "2" : EndIf
    If FindString("DT", c ,1)       : getCode = "3" : EndIf
    If "L" = c                      : getCode = "4" : EndIf
    If FindString("MN", c ,1)       : getCode = "5" : EndIf
    If "R" = c                      : getCode = "6" : EndIf
    If FindString("HW", c ,1)       : getCode = "." : EndIf
    ProcedureReturn getCode
EndProcedure

Procedure.s soundex(word.s)
    Protected.s previous.s = "" , code.s , current , soundex
    Protected.i i

    word = UCase(word)
    code = Mid(word,1,1)
    previous = ""
    For i = 2 To (Len(word) + 1)
        current = getCode(Mid(word, i, 1))
        If current = "." : Continue : EndIf
        If Len(current) > 0 And current <> previous
            code + current
        EndIf
        previous = current
        If Len(code) = 4
          Break
        EndIf
    Next
    If Len(code) < 4
        code = LSet(code, 4,"0")
    EndIf
    ProcedureReturn code
EndProcedure

OpenConsole()

PrintN (soundex("Lukasiewicz"))
PrintN("Press any key to exit"): Repeat: Until Inkey() <> ""
```



## Python


```python
from itertools import groupby

def soundex(word):
   codes = ("bfpv","cgjkqsxz", "dt", "l", "mn", "r")
   soundDict = dict((ch, str(ix+1)) for ix,cod in enumerate(codes) for ch in cod)
   cmap2 = lambda kar: soundDict.get(kar, '9')
   sdx =  ''.join(cmap2(kar) for kar in word.lower())
   sdx2 = word[0].upper() + ''.join(k for k,g in list(groupby(sdx))[1:] if k!='9')
   sdx3 = sdx2[0:4].ljust(4,'0')
   return sdx3

```

{{out}}

```Python
>>>
print soundex("soundex")
S532
>>>print soundex("example")
E251
>>>print soundex("ciondecks")
C532
>>>print soundex("ekzampul")
E251
```



## Racket

The [http://rosettacode.org/wiki/Soundex#Scheme Scheme solution] runs as is in Racket.


## REXX

Some assumptions made:
:*   rules are from the algorithm for the '''American Soundex'''.
:*   rules were taken from the Wikipedia article: http://en.wikipedia.org/wiki/Soundex
:*   multiple words   (like ''Van de Graaff'')    are treated as one word.
:*   anything that's not a letter of the Latin alphabet is ignored.
:*   words starting with a non-letter are processed.
:*   letters of the ASCII-extended character set are ignored.
:*   ASCII-extended characters (ÇüéâäàåçêëèïîìÄÅÉæÆôöòûùÿÖÜíóúñÑ) could be added to the program easily.

```rexx
/*REXX program  demonstrates  Soundex codes  from some words  or  from the command line.*/
_=;   @.=                                        /*set a couple of vars to "null".*/
parse arg @.0 .                                  /*allow input from command line. */
                           @.1   = "12346"         ;        #.1   = '0000'
                           @.4   = "4-H"           ;        #.4   = 'H000'
                           @.11  = "Ashcraft"      ;        #.11  = 'A261'
                           @.12  = "Ashcroft"      ;        #.12  = 'A261'
                           @.18  = "auerbach"      ;        #.18  = 'A612'
                           @.20  = "Baragwanath"   ;        #.20  = 'B625'
                           @.22  = "bar"           ;        #.22  = 'B600'
                           @.23  = "barre"         ;        #.23  = 'B600'
                           @.20  = "Baragwanath"   ;        #.20  = 'B625'
                           @.28  = "Burroughs"     ;        #.28  = 'B620'
                           @.29  = "Burrows"       ;        #.29  = 'B620'
                           @.30  = "C.I.A."        ;        #.30  = 'C000'
                           @.37  = "coöp"          ;        #.37  = 'C100'
                           @.43  = "D-day"         ;        #.43  = 'D000'
                           @.44  = "d jay"         ;        #.44  = 'D200'
                           @.45  = "de la Rosa"    ;        #.45  = 'D462'
                           @.46  = "Donnell"       ;        #.46  = 'D540'
                           @.47  = "Dracula"       ;        #.47  = 'D624'
                           @.48  = "Drakula"       ;        #.48  = 'D624'
                           @.49  = "Du Pont"       ;        #.49  = 'D153'
                           @.50  = "Ekzampul"      ;        #.50  = 'E251'
                           @.51  = "example"       ;        #.51  = 'E251'
                           @.55  = "Ellery"        ;        #.55  = 'E460'
                           @.59  = "Euler"         ;        #.59  = 'E460'
                           @.60  = "F.B.I."        ;        #.60  = 'F000'
                           @.70  = "Gauss"         ;        #.70  = 'G200'
                           @.71  = "Ghosh"         ;        #.71  = 'G200'
                           @.72  = "Gutierrez"     ;        #.72  = 'G362'
                           @.80  = "he"            ;        #.80  = 'H000'
                           @.81  = "Heilbronn"     ;        #.81  = 'H416'
                           @.84  = "Hilbert"       ;        #.84  = 'H416'
                           @.100 = "Jackson"       ;        #.100 = 'J250'
                           @.104 = "Johnny"        ;        #.104 = 'J500'
                           @.105 = "Jonny"         ;        #.105 = 'J500'
                           @.110 = "Kant"          ;        #.110 = 'K530'
                           @.116 = "Knuth"         ;        #.116 = 'K530'
                           @.120 = "Ladd"          ;        #.120 = 'L300'
                           @.124 = "Llyod"         ;        #.124 = 'L300'
                           @.125 = "Lee"           ;        #.125 = 'L000'
                           @.126 = "Lissajous"     ;        #.126 = 'L222'
                           @.128 = "Lukasiewicz"   ;        #.128 = 'L222'
                           @.130 = "naïve"         ;        #.130 = 'N100'
                           @.141 = "Miller"        ;        #.141 = 'M460'
                           @.143 = "Moses"         ;        #.143 = 'M220'
                           @.146 = "Moskowitz"     ;        #.146 = 'M232'
                           @.147 = "Moskovitz"     ;        #.147 = 'M213'
                           @.150 = "O'Conner"      ;        #.150 = 'O256'
                           @.151 = "O'Connor"      ;        #.151 = 'O256'
                           @.152 = "O'Hara"        ;        #.152 = 'O600'
                           @.153 = "O'Mally"       ;        #.153 = 'O540'
                           @.161 = "Peters"        ;        #.161 = 'P362'
                           @.162 = "Peterson"      ;        #.162 = 'P362'
                           @.165 = "Pfister"       ;        #.165 = 'P236'
                           @.180 = "R2-D2"         ;        #.180 = 'R300'
                           @.182 = "rÄ≈sumÅ∙"      ;        #.182 = 'R250'
                           @.184 = "Robert"        ;        #.184 = 'R163'
                           @.185 = "Rupert"        ;        #.185 = 'R163'
                           @.187 = "Rubin"         ;        #.187 = 'R150'
                           @.191 = "Soundex"       ;        #.191 = 'S532'
                           @.192 = "sownteks"      ;        #.192 = 'S532'
                           @.199 = "Swhgler"       ;        #.199 = 'S460'
                           @.202 = "'til"          ;        #.202 = 'T400'
                           @.208 = "Tymczak"       ;        #.208 = 'T522'
                           @.216 = "Uhrbach"       ;        #.216 = 'U612'
                           @.221 = "Van de Graaff" ;        #.221 = 'V532'
                           @.222 = "VanDeusen"     ;        #.222 = 'V532'
                           @.230 = "Washington"    ;        #.230 = 'W252'
                           @.233 = "Wheaton"       ;        #.233 = 'W350'
                           @.234 = "Williams"      ;        #.234 = 'W452'
                           @.236 = "Woolcock"      ;        #.236 = 'W422'

      do k=0  for 300;     if @.k==''  then iterate;        $=soundex(@.k)
      say word('nope [ok]', 1 +($==#.k | k==0))   _   $   "is the Soundex for"   @.k
      if k==0  then leave
      end   /*k*/
exit                                                   /*stick a fork in it, we're done.*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
soundex: procedure;     arg x                             /*ARG uppercases the var   X. */
         old_alphabet= 'AEIOUYHWBFPVCGJKQSXZDTLMNR'
         new_alphabet= '@@@@@@**111122222222334556'
         word=                                            /* [+]  exclude  non-letters. */
                do i=1  for length(x);  _=substr(x, i, 1) /*obtain a character from word*/
                if datatype(_,'M')  then word=word || _   /*Upper/lower letter?  Then OK*/
                end   /*i*/

         value=strip(left(word, 1))                       /*1st character is left alone.*/
         word=translate(word, new_alphabet, old_alphabet) /*define the current  word.   */
         prev=translate(value,new_alphabet, old_alphabet) /*   "    "  previous   "     */

           do j=2  to length(word)                        /*process remainder of word.  */
           ?=substr(word, j, 1)
           if ?\==prev & datatype(?,'W')  then do;  value=value || ?;  prev=?;  end
                                          else if ?=='@'  then prev=?
           end   /*j*/

         return left(value,4,0)                           /*padded value with zeroes.   */
```

'''output'''   when using the default (internal) inputs:
<pre style="height:33ex">
[ok]  0000 is the Soundex for 12346
[ok]  H000 is the Soundex for 4-H
[ok]  A261 is the Soundex for Ashcraft
[ok]  A261 is the Soundex for Ashcroft
[ok]  A612 is the Soundex for auerbach
[ok]  B625 is the Soundex for Baragwanath
[ok]  B600 is the Soundex for bar
[ok]  B600 is the Soundex for barre
[ok]  B620 is the Soundex for Burroughs
[ok]  B620 is the Soundex for Burrows
[ok]  C000 is the Soundex for C.I.A.
[ok]  C100 is the Soundex for coöp
[ok]  D000 is the Soundex for d-day
[ok]  D200 is the Soundex for d jay
[ok]  D462 is the Soundex for de la Rosa
[ok]  D540 is the Soundex for Donnell
[ok]  D624 is the Soundex for Dracula
[ok]  D624 is the Soundex for Drakula
[ok]  D153 is the Soundex for Du Pont
[ok]  E251 is the Soundex for Ekzampul
[ok]  E251 is the Soundex for example
[ok]  E460 is the Soundex for Ellery
[ok]  E460 is the Soundex for Euler
[ok]  F000 is the Soundex for F.B.I.
[ok]  G200 is the Soundex for Gauss
[ok]  G200 is the Soundex for Ghosh
[ok]  G362 is the Soundex for Gutierrez
[ok]  H000 is the Soundex for he
[ok]  H416 is the Soundex for Heilbronn
[ok]  H416 is the Soundex for Hilbert
[ok]  J250 is the Soundex for Jackson
[ok]  J500 is the Soundex for Johnny
[ok]  J500 is the Soundex for Jonny
[ok]  K530 is the Soundex for Kant
[ok]  K530 is the Soundex for Knuth
[ok]  L300 is the Soundex for Ladd
[ok]  L300 is the Soundex for Llyod
[ok]  L000 is the Soundex for Lee
[ok]  L222 is the Soundex for Lissajous
[ok]  L222 is the Soundex for Lukasiewicz
[ok]  N100 is the Soundex for naïve
[ok]  M460 is the Soundex for Miller
[ok]  M220 is the Soundex for Moses
[ok]  M232 is the Soundex for Moskowitz
[ok]  M213 is the Soundex for Moskovitz
[ok]  O256 is the Soundex for O'Conner
[ok]  O256 is the Soundex for O'Connor
[ok]  O600 is the Soundex for O'Hara
[ok]  O540 is the Soundex for O'Mally
[ok]  P362 is the Soundex for Peters
[ok]  P362 is the Soundex for Peterson
[ok]  P236 is the Soundex for Pfister
[ok]  R300 is the Soundex for R2-D2
[ok]  R250 is the Soundex for rÄ≈sumÅ∙
[ok]  R163 is the Soundex for Robert
[ok]  R163 is the Soundex for Rupert
[ok]  R150 is the Soundex for Rubin
[ok]  S532 is the Soundex for Soundex
[ok]  S532 is the Soundex for sownteks
[ok]  S460 is the Soundex for Swhgler
[ok]  T400 is the Soundex for 'til
[ok]  T522 is the Soundex for Tymczak
[ok]  U612 is the Soundex for Uhrbach
[ok]  V532 is the Soundex for Van de Graaff
[ok]  V532 is the Soundex for VanDeusen
[ok]  W252 is the Soundex for Washington
[ok]  W350 is the Soundex for Wheaton
[ok]  W452 is the Soundex for Williams
[ok]  W422 is the Soundex for Woolcock

```



## Ring


```ring

# Project: Soundex

name = ["Ashcraf", "Ashcroft", "Gauss", "Ghosh", "Hilbert", "Heilbronn", "Lee", "Lloyd",
              "Moses", "Pfister", "Robert", "Rupert", "Rubin","Tymczak", "Soundex", "Example"]
for i = 1 to 16
     sp = 10 - len(name[i])
     see '"' + name[i] + '"' + copy(" ", sp) + " " + soundex(name[i]) + nl
next

func soundex(name2)
name2 = upper(name2)
n = "01230129022455012623019202"
s = left(name2,1)
p = number(substr(n, ascii(s) - 64, 1))
for i = 2 to len(name2)
     n2 = number(substr(n, ascii(name2[i]) - 64, 1))
     if n2 > 0 and n2 != 9 and n2 != p s = s + string(n2) ok
     if n2 != 9 p = n2 ok
next
return left(s + "000", 4)

```

Output:

```txt

"Ashcraf"    A261
"Ashcroft"   A261
"Gauss"      G200
"Ghosh"      G200
"Hilbert"    H416
"Heilbronn"  H416
"Lee"        L000
"Lloyd"      L300
"Moses"      M220
"Pfister"    P236
"Robert"     R163
"Rupert"     R163
"Rubin"      R150
"Tymczak"    T522
"Soundex"    S532
"Example"    E251

```



## Ruby

Courtesy http://snippets.dzone.com/posts/show/4530

```ruby
class String

  SoundexChars = 'BFPVCGJKQSXZDTLMNR'
  SoundexNums  = '111122222222334556'
  SoundexCharsEx = '^' + SoundexChars
  SoundexCharsDel = '^A-Z'

  # desc: http://en.wikipedia.org/wiki/Soundex
  def soundex(census = true)
    str = self.upcase.delete(SoundexCharsDel)
    str[0,1] + str[1..-1].delete(SoundexCharsEx).
                          tr_s(SoundexChars, SoundexNums)\
                          [0 .. (census ? 2 : -1)].
                          ljust(3, '0') rescue ''
  end

  def sounds_like(other)
    self.soundex == other.soundex
  end
end

%w(Soundex Sownteks Example Ekzampul foo bar).each_slice(2) do |word1, word2|
  [word1, word2].each {|word| puts '%-8s -> %s' % [word, word.soundex]}

  print "'#{word1}' "
  print word1.sounds_like(word2) ? "sounds" : "does not sound"
  print " like '#{word2}'\n"
end
```



```txt
Soundex  -> S532
Sownteks -> S532
'Soundex' sounds like 'Sownteks'
Example  -> E251
Ekzampul -> E251
'Example' sounds like 'Ekzampul'
foo      -> F000
bar      -> B600
'foo' does not sound like 'bar'
```



## Run BASIC

Courtesy http://dkokenge.com/rbp

```runbasic
global val$
val$(1) = "BPFV"
val$(2) = "CSGJKQXZ"
val$(3) = "DT"
val$(4) = "L"
val$(5) = "MN"
val$(6) = "R"

' ---------------------------------
' show soundex on these words
' ---------------------------------
w$(1) = "Robert"      'R163
w$(2) = "Rupert"      'R163
w$(3) = "Rubin"       'R150
w$(4) = "moses"       'M220
w$(5) = "O'Mally"     'O540
w$(6) = "d jay"       'D200

for i = 1 to 6
  print w$(i);" soundex:";soundex$(w$(i))
next i
wait

' ---------------------------------
' Return soundex of word
' ---------------------------------
function soundex$(a$)
a$ = upper$(a$)
for i = 2 to len(a$)
  theLtr$ = mid$(a$,i,1)
  s$      = "0"
  if instr("AEIOUYHW |",theLtr$) <> 0 then s$ = ""
  if theLtr$ <> preLtr$ then
    for j = 1 to 6
     if instr(val$(j),theLtr$) <> 0 then s$ = str$(j)
    next j
  end if
  sdx$    = sdx$ + s$
  preLtr$ = theLtr$
next i
soundex$  = left$(a$,1) + left$(sdx$;"000",3)
end function
```



```txt
Robert soundex:R163
Rupert soundex:R163
Rubin soundex:R150
moses soundex:M220
O'Mally soundex:O054
d jay soundex:D200
```



## Scala


```scala
def soundex(s:String)={
   var code=s.head.toUpper.toString
   var previous=getCode(code.head)
   for(ch <- s.drop(1); current=getCode(ch.toUpper)){
      if (!current.isEmpty && current!=previous)
         code+=current
      previous=current
   }
   code+="0000"
   code.slice(0,4)
}

def getCode(c:Char)={
   val code=Map("1"->List('B','F','P','V'),
      "2"->List('C','G','J','K','Q','S','X','Z'),
      "3"->List('D', 'T'),
      "4"->List('L'),
      "5"->List('M', 'N'),
      "6"->List('R'))

   code.find(_._2.exists(_==c)) match {
      case Some((k,_)) => k
      case _ => ""
   }
}
```



```scala
def main(args: Array[String]): Unit = {
   val tests=Map(
      "Soundex"     -> "S532",
      "Euler"	    -> "E460",
      "Gauss"	    -> "G200",
      "Hilbert"	    -> "H416",
      "Knuth"	    -> "K530",
      "Lloyd"	    -> "L300",
      "Lukasiewicz" -> "L222",
      "Ellery"	    -> "E460",
      "Ghosh"	    -> "G200",
      "Heilbronn"   -> "H416",
      "Kant"	    -> "K530",
      "Ladd"	    -> "L300",
      "Lissajous"   -> "L222",
      "Wheaton"	    -> "W350",
      "Ashcraft"    -> "A226",
      "Burroughs"   -> "B622",
      "Burrows"	    -> "B620",
      "O'Hara"	    -> "O600")

   tests.foreach{(v)=>
      val code=soundex(v._1)
      val status=if (code==v._2) "OK" else "ERROR"
      printf("Name: %-20s  Code: %s   Found: %s  - %s\n", v._1, v._2, code, status)
   }
}
```



## Scheme


This implements American Soundex as described at [http://www.avotaynu.com/soundex.htm].

{{works with|any R6RS Scheme}}


```scheme
;; The American Soundex System
;;
;; The soundex code consist of the first letter of the name followed
;; by three digits. These three digits are determined by dropping the
;; letters a, e, i, o, u, h, w and y and adding three digits from the
;; remaining letters of the name according to the table below. There
;; are only two additional rules. (1) If two or more consecutive
;; letters have the same code, they are coded as one letter. (2) If
;; there are an insufficient numbers of letters to make the three
;; digits, the remaining digits are set to zero.

;; Soundex Table

;;  1 b,f,p,v
;;  2 c,g,j,k,q,s,x,z
;;  3 d, t
;;  4 l
;;  5 m, n
;;  6 r

;; Examples:

;;  Miller M460
;;  Peterson P362
;;  Peters P362
;;  Auerbach A612
;;  Uhrbach U612
;;  Moskowitz M232
;;  Moskovitz M213

(define (char->soundex c)
  (case (char-upcase c)
    ((#\B #\F #\P #\V) #\1)
    ((#\C #\G #\J #\K #\Q #\S #\X #\Z) #\2)
    ((#\D #\T) #\3)
    ((#\L) #\4)
    ((#\M #\N) #\5)
    ((#\R) #\6)
    (else #\nul)))

(define (collapse-dups lst)
  (if (= (length lst) 1) lst
      (if (equal? (car lst) (cadr lst))
	  (collapse-dups (cdr lst))
	  (cons (car lst) (collapse-dups (cdr lst))))))

(define (remove-nul lst)
  (filter (lambda (c)
	    (not (equal? c #\nul)))
	  lst))

(define (force-len n lst)
  (cond ((= n 0) '())
	((null? lst) (force-len n (list #\0)))
	(else (cons (car lst) (force-len (- n 1) (cdr lst))))))

(define (soundex s)
  (let ((slst (string->list s)))
    (force-len 4 (cons (char-upcase (car slst))
		       (remove-nul
			(collapse-dups
			 (map char->soundex (cdr slst))))))))

(soundex "miller")
(soundex "Peterson")
(soundex "PETERS")
(soundex "auerbach")
(soundex "Uhrbach")
(soundex "Moskowitz")
(soundex "Moskovitz")
```


{{out}}

```txt
> "M460"
> "P362"
> "P362"
> "A612"
> "U612"
> "M232"
> "M213"

```



## Sidef


```ruby
func soundex(word, length=3) {

    # Uppercase the argument passed in to normalize it
    # and drop any non-alphabetic characters
    word.uc!.tr!('A-Z', '', 'cd')

    # Return if word does not contain 'A-Z'
    return(nil) if (word.is_empty)

    var firstLetter = word.char(0)

    # Replace letters with corresponding number values
    word.tr!('BFPV',     '1', 's')
    word.tr!('CGJKQSXZ', '2', 's')
    word.tr!('DT',       '3', 's')
    word.tr!('L',        '4', 's')
    word.tr!('MN',       '5', 's')
    word.tr!('R',        '6', 's')

    # Discard the first letter
    word.ft!(1)

    # Remove A, E, H, I, O, U, W, and Y
    word.tr!('AEHIOUWY', '', 'd')

    # Return the soundex code
    firstLetter + (word.chars + length.of('0') -> first(length).join)
}

func testSoundex {

    # Key-value pairs of names and corresponding Soundex codes
    var sndx = Hash(
                "Euler"                => "E4600",
                "Gauss"                => "G2000",
                "Hilbert"              => "H4163",
                "Knuth"                => "K5300",
                "Lloyd"                => "L3000",
                "Lukasieicz"           => "L2220",
                'fulkerson'            => 'F4262',
                'faulkersuhn'          => 'F4262',
                'fpfffffauhlkkersssin' => 'F4262',
                'Aaeh'                 => 'A0000',
               )

    sndx.keys.sort.each { |name|
        var findSdx = soundex(name, 4)
        say "The soundex for #{name} should be #{sndx{name}} and is #{findSdx}"
        if (findSdx != sndx{name}) {
            say "\tHowever, that is incorrect!\n"
        }
    }
}

testSoundex()
```



## SNOBOL4


{{works with|Macro Spitbol}}
{{works with|Snobol4+}}
{{works with|CSnobol}}

US National Archives (NARA) Soundex. Includes the "HW" rule omitted by Knuth and many other implementations.


```SNOBOL4
*       # Soundex coding
*       # ABCDEFGHIJKLMNOPQRSTUVWXYZ
*       # 01230127022455012623017202

        define('soundex(str)init,ch') :(soundex_end)
soundex sdxmap = '01230127022455012623017202'
        str = replace(str,&lcase,&ucase)
sdx1    str notany(&ucase) = :s(sdx1)
        init = substr(str,1,1)
        str = replace(str,&ucase,sdxmap)
sdx2    str len(1) $ ch span(*ch) = ch :s(sdx2)
*       # Omit next line for Knuth's simple Soundex
sdx3    str len(1) $ ch ('7' *ch) = ch :s(sdx3)
        str len(1) = init
sdx4    str any('07') = :s(sdx4)
        str = substr(str,1,4)
        str = lt(size(str),4) str dupl('0',4 - size(str))
        soundex = str :(return)
soundex_end

*       # Test and display
        test = " Washington Lee Gutierrez Pfister Jackson Tymczak"
+              " Ashcroft Swhgler O'Connor Rhys-Davies"
loop    test span(' ') break(' ') . name = :f(end)
        output = soundex(name) ' ' name :(loop)
end
```


{{out}}

```txt
W252 Washington
L000 Lee
G362 Gutierrez
P236 Pfister
J250 Jackson
T522 Tymczak
A261 Ashcroft
S460 Swhgler
O256 O'Connor
```



## Smalltalk


{{works with|Smalltalk/X}}
using a builtin utility:

```smalltalk
PhoneticStringUtilities soundexCodeOf: 'Soundex' "-> S532"
```



## Standard ML

This implementation uses datatypes to encode the different rules for handling duplicate digits
when different characters appear in the input:

```sml
(* There are 3 kinds of letters:
 *   h and w are ignored completely (letters separated by h or w are considered
 *     adjacent, or merged together)
 *   vowels are ignored, but letters separated by a vowel are split apart.
 *   All consonants but h and w map to a digit *)
datatype code =
         Merge
       | Split
       | Digit of char

(* Encodes which characters map to which codes *)
val codeTable =
 [([#"H", #"W"], Merge),
  ([#"A",#"E",#"I", #"O",#"U",#"Y"], Split),
  ([#"B",#"F",#"P",#"V"], Digit #"1"),
  ([#"C",#"G",#"J",#"K",#"Q",#"S",#"X",#"Z"], Digit #"2"),
  ([#"D",#"T"], Digit #"3"),
  ([#"L"], Digit #"4"),
  ([#"M",#"N"], Digit #"5"),
  ([#"R"], Digit #"6")]

(* Find the code that matches a given character *)
fun codeOf (c : char) =
    #2 (valOf (List.find (fn (L,_) => isSome(List.find (fn c' => c = c') L)) codeTable))

(* Remove all the non-digit codes, combining like digits when appropriate. *)
fun collapse (c :: Merge :: cs) = collapse (c :: cs)
  | collapse (Digit d :: Split :: cs) = Digit d :: collapse cs
  | collapse (Digit d :: (cs' as Digit d' :: cs)) =
    if d = d' then collapse (Digit d :: cs)
    else Digit d :: collapse cs'
  | collapse [Digit d] = [Digit d]
  | collapse (c::cs) = collapse cs
  | collapse _ = []

(* dropWhile f L removes the initial elements of L that satisfy f and returns
 * the rest *)
fun dropWhile f [] = []
  | dropWhile f (x::xs) =
    if f x then dropWhile f xs
    else x::xs

fun soundex (s : string) =
    let
      (* Normalize the string to uppercase letters only *)
      val c::cs = map (Char.toUpper) (filter Char.isAlpha(String.explode s))
      fun first3 L = map (fn Digit c => c) (List.take(L,3))
      val padding = [Digit #"0", Digit #"0", Digit #"0"]
      (* Remove any initial section that has the same code as the first character.
       * This comes up in the "Pfister" test case. *)
      val codes = dropWhile (fn Merge => true | Digit d => Digit d = codeOf c | Split => false)
                            (map codeOf (c::cs))
    in
      String.implode(c::first3(collapse codes@padding))
    end

(* Some test cases from Wikipedia *)
fun test input output =
    if soundex input = output then ()
    else raise Fail ("Soundex of " ^ input ^ " should be " ^ output ^ ", not " ^ soundex input)

val () = test "Rupert" "R163"
val () = test "Robert" "R163"
val () = test "Rubin" "R150"
val () = test "Tymczak" "T522"
val () = test "Pfister" "P236"
```



## Stata

The soundex function is built-in. See [http://www.stata.com/help.cgi?soundex Stata help].

```stata
. display soundex_nara("Ashcraft")
A261

. display soundex_nara("Tymczak")
T522
```


There is also a variant:


```stata
. di soundex("Ashcraft")
A226
```



## Tcl

{{tcllib|soundex}} contains an implementation of Knuth's soundex algorithm.

```tcl
package require soundex

foreach string {"Soundex" "Example" "Sownteks" "Ekzampul"} {
    set soundexCode [soundex::knuth $string]
    puts "\"$string\" has code $soundexCode"
}
```

{{out}}

```txt
"Soundex" has code S532
"Example" has code E251
"Sownteks" has code S532
"Ekzampul" has code E251
```



## TSE SAL


```TSE SAL


// library: string: get: soundex <description></description> <version>1.0.0.0.35</version> <version control></version control> (filenamemacro=getstgso.s) [kn, ri, sa, 15-10-2011 18:23:04]
STRING PROC FNStringGetSoundexS( STRING inS )
 // Except the first character, you replace each character in the string with its corresponding mapping number
 // Idea is that you give characters with the same sound the same mapping number (e.g. 'c' is replaced by '2'. And 'k' which might sound the same as a 'c' is also replaced by the same '2'
 STRING map1S[255] = "AEHIOUWYBFPVCGJKQSXZDTLMNR"
 STRING map2S[255] = "00000000111122222222334556"
 STRING s[255] = Upper( inS )
 STRING soundexS[255] = ""
 STRING characterCurrentS[255] = ""
 STRING characterPreviousS[255] = "?"
 STRING characterMapS[255] = ""
 INTEGER mapPositionI = 0
 INTEGER minI = 1
 INTEGER I = minI
 INTEGER maxI = Length( s )
 I = minI
 characterCurrentS = SubStr( s, I, 1 )
 mapPositionI = Pos( characterCurrentS, map1S )
 WHILE ( ( I <= maxI ) AND ( Length( soundexS ) < 4 ) AND ( NOT ( mapPositionI == 0 ) ) )
  // Skip double letters, like CC, KK, PP, ...
  IF ( NOT ( mapPositionI == 0 ) ) AND ( NOT ( characterCurrentS == characterPreviousS ) )
   characterPreviousS = characterCurrentS
   // First character is extracted unchanged, for sorting purposes.
   IF ( I == minI )
    soundexS = Format( soundexS, characterCurrentS )
   ELSE
    mapPositionI = Pos( characterCurrentS, map1S )
    IF ( NOT ( mapPositionI == 0 ) )
     characterMapS = SubStr( map2S, mapPositionI, 1 )
     // skip vowels A, E, I, O, U, further also H, W and Y. In general all characters which have a mapping value of "0"
     IF ( NOT ( characterMapS == "0" ) )
      soundexS = Format( soundexS, characterMapS )
     ENDIF
    ENDIF
   ENDIF
  ENDIF
  I = I + 1
  characterCurrentS = SubStr( s, I, 1 )
 ENDWHILE
 IF ( NOT ( soundexS == "" ) )
  WHILE ( Length( soundexS ) < 4 )
   soundexS = Format( soundexS, "0" )
  ENDWHILE
 ENDIF
 RETURN( soundexS )
END

PROC Main()
 STRING s1[255] = "John Doe"
 // Warn( Format( FNStringGetSoundexS( "Ashcraft" ) ) ) // gives e.g. "A226" // using another rule the value might be "A261" (see Wikipedia, soundex)
 // Warn( Format( FNStringGetSoundexS( "Ashcroft" ) ) ) // gives e.g. "A226" // using another rule the value might be "A261" (see Wikipedia, soundex)
 // Warn( Format( FNStringGetSoundexS( "Davidson, Greg" ) ) ) // gives e.g. "D132"
 // Warn( Format( FNStringGetSoundexS( "Dracula" ) ) ) // gives e.g. "D624"
 // Warn( Format( FNStringGetSoundexS( "Drakula" ) ) ) // gives e.g. "D624"
 // Warn( Format( FNStringGetSoundexS( "Darwin" ) ) ) // gives e.g. "D650"
 // Warn( Format( FNStringGetSoundexS( "Darwin, Daemon" ) ) ) // gives e.g. "D650"
 // Warn( Format( FNStringGetSoundexS( "Darwin, Ian" ) ) ) // gives e.g. "D650"
 // Warn( Format( FNStringGetSoundexS( "Derwin" ) ) ) // gives e.g. "D650"
 // Warn( Format( FNStringGetSoundexS( "Darwent, William" ) ) ) // gives e.g. "D653"
 // Warn( Format( FNStringGetSoundexS( "Ellery" ) ) ) // gives e.g. "E460"
 // Warn( Format( FNStringGetSoundexS( "Euler" ) ) ) // gives e.g. "E460"
 // Warn( Format( FNStringGetSoundexS( "Ghosh" ) ) ) // gives e.g. "G200"
 // Warn( Format( FNStringGetSoundexS( "Gauss" ) ) ) // gives e.g. "G200"
 // Warn( Format( FNStringGetSoundexS( "Heilbronn" ) ) ) // gives e.g. "H416"
 // Warn( Format( FNStringGetSoundexS( "Hilbert" ) ) ) // gives e.g. "H416"
 // Warn( Format( FNStringGetSoundexS( "Johnny" ) ) ) // gives e.g. "J500"
 // Warn( Format( FNStringGetSoundexS( "Jonny" ) ) ) // gives e.g. "J500"
 // Warn( Format( FNStringGetSoundexS( "Kant" ) ) ) // gives e.g. "K530"
 // Warn( Format( FNStringGetSoundexS( "Knuth" ) ) ) // gives e.g. "K530"
 // Warn( Format( FNStringGetSoundexS( "Lissajous" ) ) ) // gives e.g. "L222"
 // Warn( Format( FNStringGetSoundexS( "Lukasiewicz" ) ) ) // gives e.g. "L222"
 // Warn( Format( FNStringGetSoundexS( "Ladd" ) ) ) // gives e.g. "L300"
 // Warn( Format( FNStringGetSoundexS( "Lloyd" ) ) ) // gives e.g. "L300"
 // Warn( Format( FNStringGetSoundexS( "Rubin" ) ) ) // gives e.g. "R150"
 // Warn( Format( FNStringGetSoundexS( "Robert" ) ) ) // gives e.g. "R163"
 // Warn( Format( FNStringGetSoundexS( "Rupert" ) ) ) // gives e.g. "R163"
 REPEAT
  IF ( NOT ( Ask( "string: get: soundex = ", s1, _EDIT_HISTORY_ ) ) AND ( Length( s1 ) > 0 ) ) RETURN() ENDIF
  Warn( Format( FNStringGetSoundexS( s1 ) ) )
 UNTIL FALSE
END


```




## TUSCRIPT


```tuscript

$$ MODE DATA

$$ BUILD X_TABLE soundex = *
:b:1:f:1:p:1:v:1:
:c:2:g:2:j:2:k:2:1:2:s:2:x:2:z:2:
:d:3:t:3:
:l:4:
:m:5:n:5:
:r:6:

$$ names="Christiansen'Kris Jenson'soundex'Lloyd'Woolcock'Donnell'Baragwanath'Williams'Ashcroft'Euler'Ellery'Gauss'Ghosh'Hilbert'Heilbronn'Knuth'Kant'Ladd'Lukasiewicz'Lissajous'Wheaton'Burroughs'Burrows"

$$ MODE TUSCRIPT,{}
LOOP/CLEAR n=names
 first=EXTRACT (n,1,2),second=EXTRACT (n,2,3)
 IF (first==second) THEN
  rest=EXTRACT (n,3,0)
 ELSE
  rest=EXTRACT (n,2,0)
 ENDIF

 soundex=EXCHANGE (rest,soundex)
 soundex=STRINGS  (soundex,":{\0}:a:e:i:o:u:")
 soundex=REDUCE   (soundex)
 soundex=STRINGS  (soundex,":{\0}:",0,0,1,0,"")
 soundex=CONCAT   (soundex,"000")
 soundex=EXTRACT  (soundex,0,4)

 PRINT first,soundex,"=",n
ENDLOOP

```

{{out}}
<pre style='height:30ex;overflow:scroll'>
C623=Christiansen
K625=Kris Jenson
s532=soundex
L300=Lloyd
W422=Woolcock
D540=Donnell
B625=Baragwanath
W452=Williams
A261=Ashcroft
E460=Euler
E460=Ellery
G200=Gauss
G200=Ghosh
H416=Hilbert
H416=Heilbronn
K530=Knuth
K530=Kant
L300=Ladd
L222=Lukasiewicz
L222=Lissajous
W350=Wheaton
B620=Burroughs
B620=Burrows

```



## TXR



### =TXR Pattern Language=


This implements the full Soundex described in [[http://www.archives.gov/research/census/soundex.html U.S. National Archives Website]]. Doubled letters are condensed before separating the first letter, so that for instance "Lloyd" is not treated as L followed by the coding of LOYD but as L followed by the coding of OYD. Consecutive consonants which map to the same code are not condensed to a single occurrence of the code if they are separated by vowels, but separating W and H do not thus intervene. Names with common prefixes are encoded in two ways.


```txr
@(next :args)
@###
@# soundex-related filters
@###
@(deffilter remdbl ("AA" "A") ("BB" "B") ("CC" "C") ("DD" "D") ("EE" "E")
                   ("FF" "F") ("GG" "G") ("HH" "H") ("II" "I") ("JJ" "J")
                   ("KK" "K") ("LL" "L") ("MM" "M") ("NN" "N") ("OO" "O")
                   ("PP" "P") ("QQ" "Q") ("RR" "R") ("SS" "S") ("TT" "T")
                   ("UU" "U") ("VV" "V") ("WW" "W") ("XX" "X") ("YY" "Y")
                   ("ZZ" "Z"))
@(deffilter code ("B" "F" "P" "V" "1")
                 ("C" "G" "J" "K" "Q" "S" "X" "Z" "2")
                 ("D" "T" "3") ("L" "4") ("M" "N" "5")
                 ("R" "6") ("A" "E" "I" "O" "U" "Y" "0") ("H" "W" ""))
@(deffilter squeeze ("11" "111" "1111" "11111" "1")
                    ("22" "222" "2222" "22222" "2")
                    ("33" "333" "3333" "33333" "3")
                    ("44" "444" "4444" "44444" "4")
                    ("55" "555" "5555" "55555" "5")
                    ("66" "666" "6666" "66666" "6"))
@(bind prefix ("VAN" "CON" "DE" "DI" "LA" "LE"))
@(deffilter remzero ("0" ""))
@###
@# soundex function
@###
@(define soundex (in out))
@  (local nodouble letters remainder first rest coded)
@  (next :string in)
@  (coll)@{letters /[A-Za-z]+/}@(end)
@  (cat letters "")
@  (output :into nodouble :filter (:upcase remdbl))
@letters
@  (end)
@  (next :list nodouble)
@  (maybe)
@prefix@remainder
@    (output :into nodouble)
@nodouble
@remainder
@    (end)
@  (end)
@  (next :list nodouble)
@  (collect)
@{first 1}@rest
@    (output :filter (code squeeze remzero) :into coded)
@{rest}000
@    (end)
@    (next :list coded)
@{digits 3}@(skip)
@  (end)
@  (output :into out)
@    (rep):@first@digits@(first)@first@digits@(end)
@  (end)
@  (cat out)
@(end)
@###
@# process arguments and list soundex codes
@###
@(collect :vars ())
@input
@  (output :filter (:fun soundex))
@input
@  (end)
@(end)
@###
@# compare first and second argument under soundex
@###
@(bind (first_arg second_arg . rest_args) input)
@(cases)
@  (bind first_arg second_arg :filter (:fun soundex))
@  (output)
"@first_arg" and "@second_arg" match under soundex
@  (end)
@(end)
```


Run:


```txt
$ txr soundex.txr example soundex Lloyd lee guttierez o\'hara vandeusen dimeola
E251
E251
S532
L300
L000
G362
O600
V532:D250
D540:M400
"example" and "egsampul" match under soundex
```



### =With TXR Lisp=


This solution is similar to some of the solutions in other languages. Its treatment of the algorithm is not as complete as the above solution.


```txr
@(do (defun get-code (c)
       (caseq c
         ((#\B #\F #\P #\V) #\1)
         ((#\C #\G #\J #\K #\Q #\S #\X #\Z) #\2)
         ((#\D #\T) #\3)
         (#\L #\4)
         ((#\M #\N) #\5)
         (#\R #\6)))

     (defun soundex (s)
       (if (zerop (length s))
         ""
         (let* ((su (upcase-str s))
                (o [su 0]))
           (for ((i 1) (l (length su)) cp cg)
                ((< i l) [`@{o}000` 0 4])
                ((inc i) (set cp cg))
             (set cg (get-code [su i]))
             (if (and cg (not (eql cg cp)))
               (set o `@o@cg`)))))))
@(next :args)
@(repeat)
@arg
@  (output)
@arg -> @(soundex arg)
@  (end)
@(end)
```


Run:


```txt
$ ./txr soundex-lisp.txr  soundex sowndex
soundex -> S532
sowndex -> S532
```



## UNIX Shell

{{works with|Bourne Again SHell|4}}

The following functions require this associative array to be declared:


```bash
declare -A value=(
    [B]=1 [F]=1 [P]=1 [V]=1
    [C]=2 [G]=2 [J]=2 [K]=2 [Q]=2 [S]=2 [X]=2 [Z]=2
    [D]=3 [T]=3
    [L]=4
    [M]=5 [N]=5
    [R]=6
)
```


The first algorithm described at https://en.wikipedia.org/wiki/Soundex#American_Soundex can be implemented like this:


```bash
soundex() {
    local -u word=${1//[^[:alpha:]]/.}
    local letter=${word:0:1}
    local soundex=$letter
    local previous=$letter

    word=${word:1}
    word=${word//[AEIOUY]/.}
    word=${word//[WH]/=}

    while [[ ${#soundex} -lt 4 && -n $word ]]; do
        letter=${word:0:1}

        if [[ $letter == "." ]]; then
            previous=""

        elif [[ $letter == "=" ]]; then
            if [[ $previous == [A-Z] && ${word:1:1} == [A-Z] ]] &&
               [[ ${value[$previous]} -eq ${value[${word:1:1}]} ]]
            then
                word=${word:1}
            fi

        elif [[ -z $previous ]] ||
             [[ $letter != $previous && ${value[$letter]} -ne ${value[$previous]} ]]
        then
            previous=$letter
            soundex+=${value[$letter]}
        fi

        word=${word:1}
    done
    # right pad with zeros
    soundex+="000"
    echo "${soundex:0:4}"
}
```


The "simplified" algorithm can be implemented like this:


```bash
soundex2() {
    local -u word=${1//[^[:alpha:]]/}

    # 1. Save the first letter. Remove all occurrences of 'h' and 'w' except first letter.
    local first=${word:0:1}
    word=${word:1}
    word=$first${word//[HW]/}

    # 2. Replace all consonants (include the first letter) with digits as in [2.] above.
    local consonants=$(IFS=; echo "${!value[*]}")
    local tmp letter
    local -i i
    for ((i=0; i < ${#word}; i++)); do
        letter=${word:i:1}
        if [[ $consonants == *$letter* ]]; then
            tmp+=${value[$letter]}
        else
            tmp+=$letter
        fi
    done
    word=$tmp

    # 3. Replace all adjacent same digits with one digit.
    local char
    tmp=${word:0:1}
    local previous=${word:0:1}
    for ((i=1; i < ${#word}; i++)); do
        char=${word:i:1}
        [[ $char != [[:digit:]] || $char != $previous ]] && tmp+=$char
        previous=$char
    done
    word=$tmp

    # 4. Remove all occurrences of a, e, i, o, u, y except first letter.
    tmp=${word:1}
    word=${word:0:1}${tmp//[AEIOUY]/}

    # 5. If first symbol is a digit replace it with letter saved on step 1.
    [[ $word == [[:digit:]]* ]] && word=$first${word:1}

    # 6. right pad with zeros
    word+="000"
    echo "${word:0:4}"
}
```


If we cheat a bit and allow calling out to `tr`, we can do:


```bash
soundex3() {
    local -u word=${1//[^[:alpha:]]/}

    # 1. Save the first letter. Remove all occurrences of 'h' and 'w' except first letter.
    local first=${word:0:1}
    word=$first$( tr -d "HW" <<< "${word:1}" )

    # 2. Replace all consonants (include the first letter) with digits as in [2.] above.
    # 3. Replace all adjacent same digits with one digit.
    local consonants=$( IFS=; echo "${!value[*]}" )
    local values=$( IFS=; echo "${value[*]}" )
    word=$( tr -s "$consonants" "$values" <<< "$word" )

    # 4. Remove all occurrences of a, e, i, o, u, y except first letter.
    # 5. If first symbol is a digit replace it with letter saved on step 1.
    word=$first$( tr -d "AEIOUY" <<< "${word:1}" )

    # 6. right pad with zeros
    word+="000"
    echo "${word:0:4}"
}
```


And some testing code:


```bash
declare -A tests=(
    [Soundex]=S532     [Example]=E251      [Sownteks]=S532   [Ekzampul]=E251
    [Euler]=E460       [Gauss]=G200        [Hilbert]=H416    [Knuth]=K530
    [Lloyd]=L300       [Lukasiewicz]=L222  [Ellery]=E460     [Ghosh]=G200
    [Heilbronn]=H416   [Kant]=K530         [Ladd]=L300       [Lissajous]=L222
    [Wheaton]=W350     [Burroughs]=B620    [Burrows]=B620    ["O'Hara"]=O600
    [Washington]=W252  [Lee]=L000          [Gutierrez]=G362  [Pfister]=P236
    [Jackson]=J250     [Tymczak]=T522      [VanDeusen]=V532  [Ashcraft]=A261
)

run_tests() {
    local func=$1
    echo "Testing with function $func"
    local -i all=0 fail=0
    for name in "${!tests[@]}"; do
        s=$($func "$name")
        if [[ $s != "${tests[$name]}" ]]; then
            echo "FAIL - $s - $name -- EXPECTING ${tests[$name]}"
            ((fail++))
        fi
        ((all++))
    done
    echo "$fail out of $all failures"
}

run_tests soundex
run_tests soundex2
run_tests soundex3
```


{{out}}


```txt
Testing with function soundex
0 out of 28 failures
Testing with function soundex2
0 out of 28 failures
Testing with function soundex3
0 out of 28 failures
```



## VBScript


```vb
' Soundex
    tt=array( _
      "Ashcraft","Ashcroft","Gauss","Ghosh","Hilbert","Heilbronn","Lee","Lloyd", _
      "Moses","Pfister","Robert","Rupert","Rubin","Tymczak","Soundex","Example")
    tv=array( _
      "A261","A261","G200","G200","H416","H416","L000","L300", _
      "M220","P236","R163","R163","R150","T522","S532","E251")
    For i=lbound(tt) To ubound(tt)
        ts=soundex(tt(i))
        If ts<>tv(i) Then ok=" KO "& tv(i) Else ok=""
        Wscript.echo right(" "& i ,2) & " " & left( tt(i) &space(12),12) & " " & ts & ok
    Next 'i

Function getCode(c)
    Select Case c
        Case "B", "F", "P", "V"
            getCode = "1"
        Case "C", "G", "J", "K", "Q", "S", "X", "Z"
            getCode = "2"
        Case "D", "T"
            getCode = "3"
        Case "L"
            getCode = "4"
        Case "M", "N"
            getCode = "5"
        Case "R"
            getCode = "6"
        Case "W","H"
            getCode = "-"
    End Select
End Function 'getCode

Function soundex(s)
    Dim code, previous, i
    code = UCase(Mid(s, 1, 1))
    previous = getCode(UCase(Mid(s, 1, 1)))
    For i = 2 To Len(s)
        current = getCode(UCase(Mid(s, i, 1)))
        If current <> "" And current <> "-" And current <> previous Then code = code & current
        If current <> "-" Then previous = current
    Next 'i
    soundex = Mid(code & "000", 1, 4)
End Function 'soundex

```

{{out}}

```txt

 0 Ashcraft     A261
 1 Ashcroft     A261
 2 Gauss        G200
 3 Ghosh        G200
 4 Hilbert      H416
 5 Heilbronn    H416
 6 Lee          L000
 7 Lloyd        L300
 8 Moses        M220
 9 Pfister      P236
10 Robert       R163
11 Rupert       R163
12 Rubin        R150
13 Tymczak      T522
14 Soundex      S532
15 Example      E251

```



## XPL0


```XPL0
code CrLf=9, Text=12;
string 0;               \use zero-terminated strings

func Soundex(S1);       \Convert name to Soundex string (e.g: Rubin = R150)
char S1;
char S2(80), Tbl;
int  I, J, Char, Dig, Dig0;
[     \abcdefghijklmnopqrstuvwxyz
Tbl:= "01230120022455012623010202";
I:= 0;  J:= 0;                                  \convert all letters to digits
repeat  Char:= S1(I);  I:= I+1;
        if Char>=^A & Char<=^Z then             \convert letter to lowercase
            Char:= Char + $20;
        if Char>=^a & Char<=^z &                \eliminate non letters
           Char#^h & Char#^w then               \eliminate h and w
            [Dig:= Tbl(Char-^a);                \convert letter to digit
            if Dig#^0 & Dig#Dig0 ! J=0 then     \filter out zeros and doubles
                [S2(J):= Dig;  J:= J+1];        \ but always store first digit
            Dig0:= Dig;                         \save digit to detect doubles
            ];
until   S1(I) = 0;
while J<4 do [S2(J):= ^0;  J:= J+1];            \pad with zeros to get 3 digits
S2(0):= S1(0) & ~$20;  S2(4):= 0;               \insert first letter & terminate
return S2;                                      \BEWARE: very temporary string
];

int I, Name;
[Name:=["Ashcraft", "Ashcroft", "de la Rosa", "Gauss", "Ghosh", "Heilbronn",
        "Hilbert", "Knuth", "Lee", "Lloyd", "Moses", "O'Hara", "Pfister",
        "R2-D2", "Robert", "Rubin", "Rupert", "Tymczak", "Soundex", "Example"];
for I:= 0 to 20-1 do
        [Text(0, Soundex(Name(I)));  Text(0, " ");
        Text(0, Name(I));  CrLf(0);
        ];
]
```


{{out}}

```txt

A261 Ashcraft
A261 Ashcroft
D462 de la Rosa
G200 Gauss
G200 Ghosh
H416 Heilbronn
H416 Hilbert
K530 Knuth
L000 Lee
L300 Lloyd
M220 Moses
O600 O'Hara
P236 Pfister
R300 R2-D2
R163 Robert
R150 Rubin
R163 Rupert
T522 Tymczak
S532 Soundex
E251 Example

```

