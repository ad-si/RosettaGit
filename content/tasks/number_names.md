+++
title = "Number names"
description = ""
date = 2019-10-20T16:59:42Z
aliases = []
[extra]
id = 3056
[taxonomies]
categories = ["task", "Arithmetic operations"]
tags = []
languages = [
  "360_assembly",
  "ada",
  "algol_68",
  "applesoft_basic",
  "autohotkey",
  "awk",
  "basic",
  "batch_file",
  "bbc_basic",
  "blitzmax",
  "c",
  "clojure",
  "coffeescript",
  "common_lisp",
  "cpp",
  "csharp",
  "d",
  "elixir",
  "erlang",
  "euphoria",
  "factor",
  "fortran",
  "go",
  "groovy",
  "haskell",
  "hicest",
  "inform_7",
  "java",
  "javascript",
  "joy",
  "jq",
  "julia",
  "kotlin",
  "logo",
  "lua",
  "maple",
  "mathematica",
  "maxima",
  "maxscript",
  "miniscript",
  "nim",
  "objeck",
  "ocaml",
  "pari_gp",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "php",
  "picolisp",
  "pl_i",
  "powerbasic",
  "powershell",
  "prolog",
  "purebasic",
  "racket",
  "related_tasks",
  "rexx",
  "ring",
  "ruby",
  "rust",
  "scala",
  "seed7",
  "sequencel",
  "sidef",
  "sql",
  "swift",
  "tcl",
  "vba",
  "visual_basic",
  "visual_basic_dotnet",
  "xpl0",
  "zkl",
]
+++

## Task

Show how to spell out a number in English.

You can use a preexisting implementation or roll your own, but you should support inputs up to at least one million (or the maximum value of your language's default bounded integer type, if that's less).

Support for inputs other than positive integers (like zero, negative integers, and floating-point numbers) is optional.


## Related tasks

*   [[Spelling of ordinal numbers]].





## 360 Assembly

```360asm
*        Number names              20/02/2017
NUMNAME  CSECT
         USING  NUMNAME,R13
         B      72(R15)
         DC     17F'0'
         STM    R14,R12,12(R13)
         ST     R13,4(R15)
         ST     R15,8(R13)
         LR     R13,R15            end of prolog
         LA     R6,1               i=1
       DO WHILE=(C,R6,LE,=A(NG))   do i=1 to hbound(g)
         LR     R1,R6              i
         SLA    R1,2
         L      R2,G-4(R1)         g(i)
         ST     R2,N               n=g(i)
         L      R4,N
       IF LTR,R4,Z,R4 THEN         if n=0 then
         MVC    R,=CL256'zero'     r='zero'
       ELSE     ,                  else
         MVC    R,=CL256' '        r=''
         MVC    D,=F'10'           d=10
         MVC    C,=F'100'          c=100
         MVC    K,=F'1000'         k=1000
         L      R2,N               n
         LPR    R2,R2              abs(n)
         ST     R2,A               a=abs(n)
         SR     R7,R7              j=0
       DO WHILE=(C,R7,LE,D)        do j=0 to d
         L      R4,A               a
         SRDA   R4,32
         D      R4,C               /c
         M      R4,C               *a
         L      R8,A               a
         SR     R8,R5              h=a-c*a/c
       IF C,R8,GT,=F'0',AND,C,R8,LT,D THEN  if h>0 & h<d then
         LR     R1,R8                h
         MH     R1,=H'10'
         LA     R4,S(R1)             @s(h+1)
         MVC    PG(10),0(R4)         s(h+1)
         MVC    PG+10(246),R         !!r
         MVC    R,PG                 r=s(h+1)!!' '!!r
       ENDIF    ,                  endif
       IF C,R8,GT,=F'9',AND,C,R8,LT,=F'20' THEN  if h>9 & h<20 then
         LR     R1,R8                h
         S      R1,D                 -d
         MH     R1,=H'10'
         LA     R4,T(R1)             @t(h-d+1)
         MVC    PG(10),0(R4)         t(h-d+1)
         MVC    PG+10(246),R         !!r
         MVC    R,PG                 r=t(h-d+1)!!' '!!r
       ENDIF    ,                  endif
       IF C,R8,GT,=F'19',AND,C,R8,LT,C THEN  if h>19 & h<c then
         LR     R4,R8                h
         SRDA   R4,32
         D      R4,D                 /d
         M      R4,D                 *d
         LR     R1,R8                h
         SR     R1,R5                h-d*(h/d)
         ST     R1,X                 x=h-d*(h/d)
         L      R4,X                 x
       IF LTR,R4,NZ,R4 THEN          if x^=0 then
         MVI    Y,C'-'                 y='-'
       ELSE     ,                    else
         MVI    Y,C' '                 y=' '
       ENDIF    ,                    endif
         LR     R4,R8                h
         SRDA   R4,32
         D      R4,D                 /d
         MH     R5,=H'10'
         LA     R4,U(R5)             @u(h/d+1)
         MVC    PG(10),0(R4)         u(h/d+1)
         MVC    PG+10(1),Y           y
         L      R1,X                 x
         MH     R1,=H'10'
         LA     R4,S(R1)             @s(x+1)
         MVC    PG+11(10),0(R4)      s(x+1)
         MVC    PG+21(235),R         !!r
         MVC    R,PG                 r=u(h/d+1)!!y!!s(x+1)!!r
       ENDIF    ,                  endif
         L      R4,A               a
         SRDA   R4,32
         D      R4,K               a/k
         M      R4,K               *k
         L      R8,A               a
         SR     R8,R5              h=a-k*(a/k)
         LR     R4,R8              h
         SRDA   R4,32
         D      R4,C               /c
         LR     R8,R5              h=h/c
       IF LTR,R8,NZ,R8 THEN        if h^=0 then
         LR     R1,R8                h
         MH     R1,=H'10'
         LA     R4,S(R1)             @s(h+1)
         MVC    PG(10),0(R4)         s(h+1)
         MVC    PG+10(10),=CL10' hundred '
         MVC    PG+20(236),R         !!r
         MVC    R,PG                 r=s(h+1)!!' hundred '!!r
       ENDIF    ,                  endif
         L      R4,A               a
         SRDA   R4,32
         D      R4,K               /k
         ST     R5,A               a=a/k
         L      R4,A
       IF LTR,R4,P,R4 THEN         if a>0 then
         L      R4,A                 a
         SRDA   R4,32
         D      R4,K                 /k
         M      R4,K                 *k
         L      R8,A                 a
         SR     R8,R5                h=a-k*(a/k)
       IF LTR,R8,NZ,R8 THEN          if h^=0 then
         LR     R1,R7                  j
         MH     R1,=H'10'
         LA     R4,V(R1)               @v(j+1)
         MVC    PG(10),0(R4)           v(j+1)
         MVC    PG+10(246),R           !!r
         MVC    R,PG                   r=v(j+1)!!' '!!r
       ENDIF    ,                    endif
       ENDIF    ,                  endif
         LA     R3,1               l=0
         LA     R9,256             jr=256
         LA     R10,R              ir=0
         LA     R11,R-1            irr=-1
LOOP     CLI    0(R10),C' '        if r[ii]=' '  .....+
         BNE    OPT                                   |
         CLI    1(R10),C' '        if r[ii+1]=' '     |
         BE     ITER                                  |
         CLI    1(R10),C'-'        if r[ii+1]='-'     |
         BE     ITER                                  |
OPT      LA     R11,1(R11)         irr=irr+1          |
         MVC    0(1,R11),0(R10)    rr=rr!!ci          |
         LA     R3,1(R3)           l=l+1              |
ITER     LA     R10,1(R10)         ir=ir+1            |
         BCT    R9,LOOP            ...................+
         LA     R1,R-1             @r
         AR     R1,R3              +lr
         MVC    0(80,R1),=CL80' '  clean the end
         L      R4,A               a
       IF LTR,R4,NP,R4 THEN        if a<=0 then
         B      LEAVEJ             leave
       ENDIF    ,                  endif a<=0
         LA     R7,1(R7)           j++
       ENDDO    ,                  enddo j
LEAVEJ   L      R4,N               n
       IF LTR,R4,M,R4 THEN         if n<0 then
         MVC    PG(6),=C'minus '   'minus '
         MVC    PG+6(250),R        !!r
         MVC    R,PG               r='minus '!!r
       ENDIF    ,                  endif n<0
       ENDIF    ,                  endif n=0
         MVC    PG,=CL132' '       clear buffer
         L      R1,N               n
         XDECO  R1,PG              edit n
         MVC    PG+13(256),R       r
         XPRNT  PG,132             print buffer
         LA     R6,1(R6)           i++
       ENDDO    ,                  enddo i
         L      R13,4(0,R13)       epilog
         LM     R14,R12,12(R13)
         XR     R15,R15
         BR     R14                exit
S        DC   CL10' ',CL10'one',CL10'two',CL10'three',CL10'four'
         DC   CL10'five',CL10'six',CL10'seven',CL10'eight',CL10'nine'
T        DC   CL50'ten       eleven    twelve    thirteen  fourteen'
         DC   CL50'fifteen   sixteen   seventeen eighteen  nineteen'
U        DC   CL50'                    twenty    thirty    forty'
         DC   CL50'fifty     sixty     seventy   eighty    ninety'
V        DC   CL50'thousand  million   billion   trillion'
G        DC   F'0',F'2',F'19',F'20',F'21',F'99',F'100',F'101',F'-123'
         DC   F'9123',F'467889',F'1234567',F'2147483647'
NG       EQU    (*-G)/4
N        DS     F
D        DS     F
C        DS     F
K        DS     F
A        DS     F
X        DS     F
Y        DS     CL1
R        DS     CL256
XDEC     DS     CL12
PG       DS     CL256
         YREGS
         END    NUMNAME
```

```txt

           0 zero
           2 two
          19 nineteen
          20 twenty
          21 twenty-one
          99 ninety-nine
         100 one hundred
         101 one hundred one
        -123 minus one hundred twenty-three
        9123 nine thousand one hundred twenty-three
      467889 four hundred sixty-seven thousand eight hundred eighty-nine
     1234567 one million two hundred thirty-four thousand five hundred sixty-seven
  2147483647 two billion one hundred forty-seven million four hundred eighty-three thousand six hundred forty-seven

```



## Ada


```ada
with Ada.Text_IO;

procedure Integers_In_English is

   type Spellable is range -999_999_999_999_999_999..999_999_999_999_999_999;
   function Spell (N : Spellable) return String is
      function Twenty (N : Spellable) return String is
      begin
         case N mod 20 is
            when  0 => return "zero";
            when  1 => return "one";
            when  2 => return "two";
            when  3 => return "three";
            when  4 => return "four";
            when  5 => return "five";
            when  6 => return "six";
            when  7 => return "seven";
            when  8 => return "eight";
            when  9 => return "nine";
            when 10 => return "ten";
            when 11 => return "eleven";
            when 12 => return "twelve";
            when 13 => return "thirteen";
            when 14 => return "fourteen";
            when 15 => return "fifteen";
            when 16 => return "sixteen";
            when 17 => return "seventeen";
            when 18 => return "eighteen";
            when others => return "nineteen";
         end case;
      end Twenty;

      function Decade (N : Spellable) return String is
      begin
         case N mod 10 is
            when 2 => return "twenty";
            when 3 => return "thirty";
            when 4 => return "forty";
            when 5 => return "fifty";
            when 6 => return "sixty";
            when 7 => return "seventy";
            when 8 => return "eighty";
            when others => return "ninety";
         end case;
      end Decade;

      function Hundred (N : Spellable) return String is
      begin
         if N < 20 then
            return Twenty (N);
         elsif 0 = N mod 10 then
            return Decade (N / 10 mod 10);
         else
            return Decade (N / 10) & '-' & Twenty (N mod 10);
         end if;
      end Hundred;

      function Thousand (N : Spellable) return String is
      begin
         if N < 100 then
            return Hundred (N);
         elsif 0 = N mod 100 then
            return Twenty (N / 100) & " hundred";
         else
            return Twenty (N / 100) & " hundred and " & Hundred (N mod 100);
         end if;
      end Thousand;

      function Triplet
               (  N     : Spellable;
                  Order : Spellable;
                  Name  : String;
                  Rest  : not null access function (N : Spellable) return String
               )  return String is
         High : Spellable := N / Order;
         Low  : Spellable := N mod Order;
      begin
         if High = 0 then
            return Rest (Low);
         elsif Low = 0 then
            return Thousand (High) & ' ' & Name;
         else
            return Thousand (High) & ' ' & Name & ", " & Rest (Low);
         end if;
      end Triplet;

      function Million (N : Spellable) return String is
      begin
         return Triplet (N, 10**3, "thousand", Thousand'Access);
      end Million;

      function Milliard (N : Spellable) return String is
      begin
         return Triplet (N, 10**6, "million", Million'Access);
      end Milliard;

      function Billion (N : Spellable) return String is
      begin
         return Triplet (N, 10**9, "milliard", Milliard'Access);
      end Billion;

      function Billiard (N : Spellable) return String is
      begin
         return Triplet (N, 10**12, "billion", Billion'Access);
      end Billiard;

   begin
      if N < 0 then
         return "negative " & Spell(-N);
      else
        return Triplet (N, 10**15, "billiard", Billiard'Access);
      end if;
   end Spell;

   procedure Spell_And_Print(N: Spellable) is
      Number: constant String := Spellable'Image(N);
      Spaces: constant String(1 .. 20) := (others => ' '); -- 20 * ' '
   begin
      Ada.Text_IO.Put_Line(Spaces(Spaces'First .. Spaces'Last-Number'Length)
                             & Number & ' ' & Spell(N));
   end Spell_And_Print;

   Samples: constant array (Natural range <>) of Spellable
     := (99, 300, 310, 1_501, 12_609, 512_609, 43_112_609, 77_000_112_609,
         2_000_000_000_100, 999_999_999_999_999_999,
         0, -99, -1501, -77_000_112_609, -123_456_789_987_654_321);

begin
   for I in Samples'Range loop
      Spell_And_Print(Samples(I));
   end loop;
end Integers_In_English;
```

The implementation goes up to 10<sup>18</sup>-1
and also supports negative and zero inputs.
The solution is recursive by the triplets of decimal numbers.
```txt
                  99 ninety-nine
                 300 three hundred
                 310 three hundred and ten
                1501 one thousand, five hundred and one
               12609 twelve thousand, six hundred and nine
              512609 five hundred and twelve thousand, six hundred and nine
            43112609 forty-three million, one hundred and twelve thousand, six hundred and nine
         77000112609 seventy-seven milliard, one hundred and twelve thousand, six hundred and nine
       2000000000100 two billion, one hundred
  999999999999999999 nine hundred and ninety-nine billiard, nine hundred and ninety-nine billion, nine hundred and ninety-nine milliard, nine hundred and ninety-nine million, nine hundred and ninety-nine thousand, nine hundred and ninety-nine
                   0 zero
                 -99 negative ninety-nine
               -1501 negative one thousand, five hundred and one
        -77000112609 negative seventy-seven milliard, one hundred and twelve thousand, six hundred and nine
 -123456789987654321 negative one hundred and twenty-three billiard, four hundred and fifty-six billion, seven hundred and eighty-nine milliard, nine hundred and eighty-seven million, six hundred and fifty-four thousand, three hundred and twenty-one

```



## ALGOL 68

<!-- # From: www.codecodex.com/wiki/index.php%3Ftitle%3DConvert_an_integer_into_words - site states it is GPL # -->
```algol68
PROC number words = (INT n)STRING:(
  # returns a string representation of n in words. Currently
  deals with anything from 0 to 999 999 999. #
    []STRING digits = []STRING
      ("zero","one","two","three","four","five","six","seven","eight","nine")[@0];
    []STRING teens = []STRING
      ("ten","eleven","twelve","thirteen","fourteen","fifteen","sixteen","seventeen","eighteen","nineteen")[@0];
    []STRING decades = []STRING
      ("twenty","thirty","forty","fifty","sixty","seventy","eighty","ninety")[@2];

    PROC three digits = (INT n)STRING: (
      # does the conversion for n from 0 to 999. #
        INT tens = n MOD 100 OVER 10;
        INT units = n MOD 10;
        (n >= 100|digits[n OVER 100] + " " + "hundred" + (n MOD 100 /= 0|" and "|"")|"") +
        (tens /= 0|(tens = 1|teens[units]|decades[tens] + (units /= 0|"-"|""))|"") +
        (units /= 0 AND tens /= 1 OR n = 0|digits[units]|"")
      );
    INT m = n OVER 1 000 000;
    INT k = n MOD 1 000 000 OVER 1000;
    INT u = n MOD 1000;
    (m /= 0|three digits(m) + " million"|"") +
    (m /= 0 AND (k /= 0 OR u >= 100)|", "|"") +
    (k /= 0|three digits(k) + " thousand"|"") +
    ((m /= 0 OR k /= 0) AND u > 0 AND u < 100|" and " |: k /= 0 AND u /= 0|", "|"") +
    (u /= 0 OR n = 0|three digits(u)|"")
  );

on logical file end(stand in, (REF FILE f)BOOL: GOTO stop iteration);
on value error(stand in, (REF FILE f)BOOL: GOTO stop iteration);
DO # until user hits EOF #
  INT n;
  print("n? ");
  read((n, new line));
  print((number words(n), new line))
OD;
stop iteration:
  SKIP
```


```txt

n? 43112609
forty-three million, one hundred and twelve thousand, six hundred and nine

```


```Algol68
MODE EXCEPTION = STRUCT(STRING name, PROC VOID handler);
EXCEPTION value error = ("Value Error", stop);

PROC raise = (EXCEPTION exception, STRING str error)VOID: (
  put(stand error, (name OF exception,": ",str error, new line));
  handler OF exception
);

MODE LINT = LONG LONG INT;

BOOL locale euro := TRUE;

PROC spell integer = (LINT n)STRING: (
    []STRING tens = []STRING (~, ~, "twenty", "thirty", "forty",
            "fifty", "sixty", "seventy", "eighty", "ninety")[@0];

    []STRING small = []STRING ("zero", "one", "two", "three", "four", "five",
             "six", "seven", "eight", "nine", "ten", "eleven",
             "twelve", "thirteen", "fourteen", "fifteen",
             "sixteen", "seventeen", "eighteen", "nineteen")[@0];

    []STRING bl = []STRING (~, ~, "m", "b", "tr", "quadr",
          "quint", "sext", "sept", "oct", "non", "dec")[@0];

    PROC nonzero = (STRING c, LINT n)STRING:
        IF n = 0 THEN "" ELSE c + spell integer(n) FI;

    PROC big =(INT e, LINT n)STRING:
        spell integer(n) +
        CASE e+1 IN
        #0# "",
        #1# " thousand"
        OUT
            " " +
            IF locale euro THEN # handle millard, billard & trillard etc #
              bl[e OVER 2 + 1 ]+"ill" + CASE e MOD 2 IN "ard" OUT "ion" ESAC
            ELSE
              bl[e]+"illion"
            FI
        ESAC;

    PROC base1000 rev = (LINT in n, PROC (INT,LINT)VOID yield)VOID: (
        # generates the value of the digits of n in base 1000 #
        # (i.e. 3-digit chunks), in reverse. #
        LINT n := in n;
        FOR e FROM 0 WHILE n /= 0 DO
            LINT r = n MOD 1000;
                n := n OVER 1000;
            yield(e, r)
        OD
    );

    IF n < 1000 THEN
      INT ssn := SHORTEN SHORTEN n;
      IF ssn < 0 THEN
        raise (value error, "spell integer: negative input"); ~
      ELIF ssn < 20 THEN
        small[ssn]
      ELIF ssn < 100 THEN
        INT a = ssn OVER 10,
            b = ssn MOD 10;
        tens[a] + nonzero("-", b)
      ELIF ssn < 1000 THEN
        INT a = ssn OVER 100,
            b = ssn MOD 100;
        small[a] + " hundred" + ( b NE 0 | " and" | "") + nonzero(" ", b)
      FI
    ELSE
        STRING out := "", sep:="";
      # FOR     e,      x IN # base1000 rev(n, # DO #
           (INT e, LINT x)VOID:
                IF x NE 0 THEN
                    big(e,x) + sep +=: out;
                    sep := IF e = 0 AND x < 100 THEN " and " ELSE ", " FI
                FI
       )
     # OD #;
       out
    FI
);

PROC example = (LINT n)VOID:
  print((whole(n,0),": ", spell integer(n), new line));

# examples #
LINT prod := 0;
FOR i TO 6 DO prod := prod * 10**i + i; example(prod) OD;

example(1278); example(1572); example(2010)
```

```txt

1: one
102: one hundred and two
102003: one hundred and two thousand and three
1020030004: one millard, twenty million, thirty thousand and four
102003000400005: one hundred and two billion, three millard, four hundred thousand and five
102003000400005000006: one hundred and two trillion, three billard, four hundred millard, five million and six
1278: one thousand, two hundred and seventy-eight
1572: one thousand, five hundred and seventy-two
2010: two thousand and ten

```



## Applesoft BASIC

Handles zero and negative integers.  Rounding errors occur with big numbers.

```ApplesoftBASIC
10 INPUT "GIMME A NUMBER! "; N
20 GOSUB 100"NUMBER NAME
30 PRINT R$
40 END

100 REMNUMBER NAME
110 IF R$ = "" THEN FOR I = 0 TO 10 : READ S$(I), T$(I), U$(I), V$(I) : NEXT
120 IF N = 0 THEN R$ = "ZERO" : RETURN
130 R$ = "" : D = 10 : C = 100 : M = 1E3
140 A = ABS(N)
150 FOR U = 0 TO D
160     H = A - C * INT(A / C)
170     IF H > 0 AND H < D THEN R$ = S$(H) + " " + R$
180     IF H > 9 AND H < 20 THEN R$ = T$(H - D) + " " + R$
190     IF H > 19 AND H < C THEN S = H - D * INT(H / D) : R$ = U$(INT(H / D)) + MID$("-",1+(S=0),1) + S$(S) +  " " + R$
200     H = A - M * INT(A / M)
210     H = INT (H / C)
220     IF H THEN R$ = S$(H) + " HUNDRED " + R$
230     A = INT(A / M)
240     IF A > 0 THEN H = A - M * INT(A / M) : IF H THEN R$ = V$(U) + " " + R$
250     IF A > 0 THEN NEXT U
260 IF N < 0 THEN R$ = "NEGATIVE " + R$
270 RETURN

280 DATA "", "TEN", "", "THOUSAND"
281 DATA "ONE", "ELEVEN", "", "MILLION"
282 DATA "TWO", "TWELVE", "TWENTY", "BILLION"
283 DATA "THREE", "THIRTEEN", "THIRTY", "TRILLION"
284 DATA "FOUR", "FOURTEEN", "FORTY", "QUADRILLION"
285 DATA "FIVE", "FIFTEEN", "FIFTY", "QUINTILLION"
286 DATA "SIX", "SIXTEEN", "SIXTY", "SEXTILLION"
287 DATA "SEVEN", "SEVENTEEN", "SEVENTY", "SEPTILLION"
288 DATA "EIGHT", "EIGHTEEN", "EIGHTY", "OCTILLION"
289 DATA "NINE", "NINETEEN", "NINETY", "NONILLION"
290 DATA "", "", "", "DECILLION"
```



## AutoHotkey


```autohotkey
Loop {                                 ; TEST LOOP
    n =
    Random Digits, 1, 36               ; random number with up to 36 digits
    Loop %Digits% {
        Random Digit, 0, 9             ; can have leading 0s
        n .= Digit
    }
    MsgBox 1, Number Names, % PrettyNumber(n) "`n`n" Spell(n) "`n`n"
    IfMsgBox Cancel, Break
}

Spell(n) { ; recursive function to spell out the name of a max 36 digit integer, after leading 0s removed
    Static p1=" thousand ",p2=" million ",p3=" billion ",p4=" trillion ",p5=" quadrillion ",p6=" quintillion "
         , p7=" sextillion ",p8=" septillion ",p9=" octillion ",p10=" nonillion ",p11=" decillion "
         , t2="twenty",t3="thirty",t4="forty",t5="fifty",t6="sixty",t7="seventy",t8="eighty",t9="ninety"
         , o0="zero",o1="one",o2="two",o3="three",o4="four",o5="five",o6="six",o7="seven",o8="eight"
         , o9="nine",o10="ten",o11="eleven",o12="twelve",o13="thirteen",o14="fourteen",o15="fifteen"
         , o16="sixteen",o17="seventeen",o18="eighteen",o19="nineteen"

    n :=RegExReplace(n,"^0+(\d)","$1") ; remove leading 0s from n

    If  (11 < d := (StrLen(n)-1)//3)   ; #of digit groups of 3
        Return "Number too big"

    If (d)                             ; more than 3 digits
        Return Spell(SubStr(n,1,-3*d)) p%d% ((s:=SubStr(n,1-3*d)) ? ", " Spell(s) : "")

    i := SubStr(n,1,1)
    If (n > 99)                        ; 3 digits
        Return o%i% " hundred" ((s:=SubStr(n,2)) ? " and " Spell(s) : "")

    If (n > 19)                        ; n = 20..99
        Return t%i% ((o:=SubStr(n,2)) ? "-" o%o% : "")

    Return o%n%                        ; n = 0..19
}

PrettyNumber(n) { ; inserts thousands separators into a number string
    Return RegExReplace( RegExReplace(n,"^0+(\d)","$1"), "\G\d+?(?=(\d{3})+(?:\D|$))", "$0,")
}
```



## AWK


```AWK

# syntax: GAWK -f NUMBER_NAMES.AWK
BEGIN {
    init_numtowords()
    n = split("-10 0 .1 8 100 123 1001 99999 100000 9123456789 111000000111",arr," ")
    for (i=1; i<=n; i++) {
      printf("%s = %s\n",arr[i],numtowords(arr[i]))
    }
    exit(0)
}
# source: The AWK Programming Language, page 75
function numtowords(n,  minus,str) {
    if (n < 0) {
      n = n * -1
      minus = "minus "
    }
    if (n == 0) {
      str = "zero"
    }
    else {
      str = intowords(n)
    }
    gsub(/  /," ",str)
    gsub(/ $/,"",str)
    return(minus str)
}
function intowords(n) {
    n = int(n)
    if (n >= 1000000000000) {
      return intowords(n/1000000000000) " trillion " intowords(n%1000000000000)
    }
    if (n >= 1000000000) {
      return intowords(n/1000000000) " billion " intowords(n%1000000000)
    }
    if (n >= 1000000) {
      return intowords(n/1000000) " million " intowords(n%1000000)
    }
    if (n >= 1000) {
      return intowords(n/1000) " thousand " intowords(n%1000)
    }
    if (n >= 100) {
      return intowords(n/100) " hundred " intowords(n%100)
    }
    if (n >= 20) {
      return tens[int(n/10)] " " intowords(n%10)
    }
    return(nums[n])
}
function init_numtowords() {
    split("one two three four five six seven eight nine ten eleven twelve thirteen fourteen fifteen sixteen seventeen eighteen nineteen",nums," ")
    split("ten twenty thirty forty fifty sixty seventy eighty ninety",tens," ")
}

```

```txt

-10 = minus ten
0 = zero
.1 =
8 = eight
100 = one hundred
123 = one hundred twenty three
1001 = one thousand one
99999 = ninety nine thousand nine hundred ninety nine
100000 = one hundred thousand
9123456789 = nine billion one hundred twenty three million four hundred fifty six thousand seven hundred eighty nine
111000000111 = one hundred eleven billion one hundred eleven

```



## BASIC

```qbasic
DECLARE FUNCTION int2Text$ (number AS LONG)

'small
DATA "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten"
DATA "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"
'tens
DATA "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"
'big
DATA "thousand", "million", "billion"

DIM SHARED small(1 TO 19) AS STRING, tens(7) AS STRING, big(2) AS STRING

DIM tmpInt AS INTEGER

FOR tmpInt = 1 TO 19
    READ small(tmpInt)
NEXT
FOR tmpInt = 0 TO 7
    READ tens(tmpInt)
NEXT
FOR tmpInt = 0 TO 2
    READ big(tmpInt)
NEXT


DIM n AS LONG

INPUT "Gimme a number! ", n
PRINT int2Text$(n)

FUNCTION int2Text$ (number AS LONG)
    DIM num AS LONG, outP AS STRING, unit AS INTEGER
    DIM tmpLng1 AS LONG

    IF 0 = number THEN
        int2Text$ = "zero"
        EXIT FUNCTION
    END IF

    num = ABS(number)

    DO
        tmpLng1 = num MOD 100
        SELECT CASE tmpLng1
            CASE 1 TO 19
                outP = small(tmpLng1) + " " + outP
            CASE 20 TO 99
                SELECT CASE tmpLng1 MOD 10
                    CASE 0
                        outP = tens((tmpLng1 \ 10) - 2) + " " + outP
                    CASE ELSE
                        outP = tens((tmpLng1 \ 10) - 2) + "-" + small(tmpLng1 MOD 10) + " " + outP
                END SELECT
        END SELECT

        tmpLng1 = (num MOD 1000) \ 100
        IF tmpLng1 THEN
            outP = small(tmpLng1) + " hundred " + outP
        END IF

        num = num \ 1000
        IF num < 1 THEN EXIT DO

        tmpLng1 = num MOD 1000
        IF tmpLng1 THEN outP = big(unit) + " " + outP

        unit = unit + 1
    LOOP

    IF number < 0 THEN outP = "negative " + outP

    int2Text$ = RTRIM$(outP)
END FUNCTION
```


{{out|Sample outputs}} (including the answer to the ultimate question of life, the universe, and everything):

```txt

 Gimme a number! 1
 one
 Gimme a number! 0
 zero
 Gimme a number! -1
 negative one
 Gimme a number! 42
 forty-two
 Gimme a number! 1000000
 one million
 Gimme a number! 1000000001
 one billion one
 Gimme a number! &h7fffffff
 two billion one hundred forty-seven million four hundred eighty-three thousand six hundred forty-seven

```



## Batch File

```dos
::Number Names Task from Rosetta Code Wiki
::Batch File Implementation

@echo off
setlocal enabledelayedexpansion

if "%~1"=="iterate" goto num_name

::Define the words
set "small=One Two Three Four Five Six Seven Eight Nine Ten"
set "small=%small% Eleven Twelve Thirteen Fourteen Fifteen Sixteen Seventeen Eighteen Nineteen"
set "decade=Twenty Thirty Forty Fifty Sixty Seventy Eighty Ninety"
set "big=Thousand Million Billion"

::Seperating each word...
set cnt=0
for %%X in (%small%) do (set /a "cnt+=1"&set small!cnt!=%%X)
set cnt=0
for %%Y in (%decade%) do (set decade!cnt!=%%Y&set /a "cnt+=1")
set cnt=0
for %%Z in (%big%) do (set big!cnt!=%%Z&set /a "cnt+=1")

::The Main Thing
for %%. in (42,27,1090,230000,1001100,-40309,0,123456789) do (
	set input=%%.
	if %%. lss 0 (set /a input*=-1)
	if !input! equ 0 (set TotalOut=Zero) else (
		call :num_word %%.
	)
	echo "!TotalOut!"
)
exit /b
::/The Main Thing

::The Procedure
:num_word
	set outP=
	set unit=0
	set num=!input!
:num_loop
set /a tmpLng1 = num %% 100
set /a tmpLng2 = tmpLng1 %% 10
set /a tmpNum1 = tmpLng1/10 - 2

if !tmpLng1! geq 1 if !tmpLng1! leq 19 (
	set "outP=!small%tmpLng1%! !outP!"
)
if !tmpLng1! geq 20 if !tmpLng1! leq 99 (
	if !tmpLng2! equ 0 (
		set "outP=!decade%tmpNum1%! !outP!"
	) else (
		set "outP=!decade%tmpNum1%!-!small%tmpLng2%! !outP!"
	)
)

set /a tmpLng1 = (num %% 1000)/100
if not !tmpLng1! equ 0 (
	set "outP=!small%tmpLng1%! Hundred !outP!"
)

set /a num/=1000
if !num! lss 1 goto :break_loop

set /a tmpLng1 = num %% 1000
if not !tmpLng1! equ 0 (
	set "outP=!big%unit%! !outP!"
)
set /a unit+=1
goto :num_loop

:break_loop
set "TotalOut=!outP!"
if %1 lss 0 set "TotalOut=Negative !outP!"

set TotalOut=%TotalOut:~0,-1%
goto :EOF
```

```txt
>NUMBER.BAT
"Forty-Two"
"Twenty-Seven"
"One Thousand Ninety"
"Two Hundred Thirty Thousand"
"One Million One Thousand One Hundred"
"Negative Forty Thousand Three Hundred Nine"
"Zero"
"One Hundred Twenty-Three Million Four Hundred Fifty-Six Thousand Seven Hundred Eighty-Nine"

>
```



## BBC BASIC

```bbcbasic
      DIM test%(20)
      test%() = 0, 1, 2, 19, 20, 21, 99, 100, 101, 300, 310, 1001, -1327, 1501, \
      \         10203, 12609, 101104, 102003, 467889, 1005006, -123000789
      FOR i% = 0 TO DIM(test%(),1)
        PRINT FNsaynumber(test%(i%))
      NEXT
      END

      DEF FNsaynumber(n%)
      LOCAL number%(), number$(), i%, t%, a$
      DIM number%(29), number$(29)
      number%() = 1000000000, 1000000, 1000, 100, 90, 80, 70, 60, 50, 40, 30, 20, \
      \           19, 18, 17, 16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2
      number$() = "billion", "million", "thousand", "hundred", "ninety", "eighty", \
      \           "seventy", "sixty", "fifty", "forty", "thirty", "twenty", \
      \           "nineteen", "eighteen", "seventeen", "sixteen", "fifteen", \
      \           "fourteen", "thirteen", "twelve", "eleven", "ten", "nine", \
      \           "eight", "seven", "six", "five", "four", "three", "two"

      IF n% < 0 THEN = "minus " + FNsaynumber(-n%)
      IF n% = 0 THEN = "zero"
      IF n% = 1 THEN = "one "

      FOR i% = 0 TO DIM(number%(),1)
        IF n% >= number%(i%) THEN
          t% = n% DIV number%(i%)
          IF t%=1 AND i%<4 a$ += "one " ELSE IF t%<>1 a$ += FNsaynumber(t%)
          a$ += number$(i%)
          t% = n% MOD number%(i%)
          CASE TRUE OF
            WHEN i%>3 AND i%<12 AND t%<>0: a$ += "-"
            WHEN i%<=3 AND t%>=100: a$ += ", "
            WHEN i%<=3 AND t%<>0 AND t%<100: a$ += " and "
            OTHERWISE: a$ += " "
          ENDCASE
          IF t% a$ += FNsaynumber(t%) ELSE IF i%<12 a$ += " "
          EXIT FOR
        ENDIF
      NEXT i%
      = a$
```

```txt

zero
one
two
nineteen
twenty
twenty-one
ninety-nine
one hundred
one hundred and one
three hundred
three hundred and ten
one thousand and one
minus one thousand, three hundred and twenty-seven
one thousand, five hundred and one
ten thousand, two hundred and three
twelve thousand, six hundred and nine
one hundred and one thousand, one hundred and four
one hundred and two thousand and three
four hundred and sixty-seven thousand, eight hundred and eighty-nine
one million, five thousand and six
minus one hundred and twenty-three million, seven hundred and eighty-nine

```



## BlitzMax

```BlitzMax
SuperStrict

Framework BRL.StandardIO

spellIt(        99)
spellIt(       300)
spellIt(       310)
spellIt(      1501)
spellIt(     12609)
spellIt(    512609)
spellIt(  43112609)
spellIt(1234567890)


Type TSpell

	Field smallNumbers:String[] = ["zero", "one", "two", "three", "four", "five", ..
		"six", "seven", "eight", "nine", "ten", ..
		"eleven", "twelve", "thirteen", "fourteen", "fifteen", ..
		"sixteen", "seventeen", "eighteen", "nineteen" ]

	Field decades:String[] = [ "", "", "twenty", "thirty", "forty", ..
		"fifty", "sixty", "seventy", "eighty", "ninety" ]

	Field thousandPowers:String[] = [ " billion", " million",  " thousand", "" ]

	Method spellHundreds:String(number:Int)
		Local result:String
		If number > 99 Then
			result = smallNumbers[number / 100]
			result :+ " hundred"
			number = number Mod 100
			If number Then
				result :+ " and "
			End If
		End If

		If number >= 20 Then
			result :+ decades[number / 10]
			number = number Mod 10
			If number Then
				result :+ "-"
			End If
		End If
		If number > 0 And number < 20 Then
			result :+ smallNumbers[number]
		End If

		Return result
	End Method

	Method spell:String(number:Long)
		If number < 20 Then
			Return smallNumbers[number]
		End If
		Local result:String

		Local scaleIndex:Int = 0
		Local scaleFactor:Long = 1000000000:Long ' 1 billion
		While scaleFactor > 0
			If number >= scaleFactor
				Local h:Long = number / scaleFactor
				result :+ spellHundreds(h) + thousandPowers[scaleIndex]
				number = number Mod scaleFactor
				If number Then
					result :+ ", "
				End If
			End If
			scaleFactor :/ 1000
			scaleIndex :+ 1
		Wend

		Return result
	End Method

End Type

Function spellIt(number:Long)
	Local numberSpell:TSpell = New TSpell
	Print number + " " + numberSpell.spell(number)
End Function
```


```txt

99 ninety-nine
300 three hundred
310 three hundred and ten
1501 one thousand, five hundred and one
12609 twelve thousand, six hundred and nine
512609 five hundred and twelve thousand, six hundred and nine
43112609 forty-three million, one hundred and twelve thousand, six hundred and nine
1234567890 one billion, two hundred and thirty-four million, five hundred and sixty-seven thousand, eight hundred and ninety

```



## C


```c
#include <stdio.h>
#include <string.h>

const char *ones[] = { 0, "one", "two", "three", "four",
	"five", "six", "seven", "eight", "nine",
	"ten", "eleven", "twelve", "thirteen", "fourteen",
	"fifteen", "sixteen", "seventeen", "eighteen", "nineteen" };
const char *tens[] = { 0, "ten", "twenty", "thirty", "forty",
	"fifty", "sixty", "seventy", "eighty", "ninety" };
const char *llions[] = { 0, "thousand", "million", "billion", "trillion",
//	"quadrillion", "quintillion", "sextillion", "septillion",
//	"octillion", "nonillion", "decillion"
	};
const int maxillion = sizeof(llions) / sizeof(llions[0]) * 3 - 3;

int say_hundred(const char *s, int len, int depth, int has_lead)
{
	int c[3], i;
	for (i = -3; i < 0; i++) {
		if (len + i >= 0) c[i + 3] = s[len + i] - '0';
		else c[i + 3] = 0;
	}
	if (!(c[0] + c[1] + c[2])) return 0;

	if (c[0]) {
		printf("%s hundred", ones[c[0]]);
		has_lead = 1;
	}

	if (has_lead && (c[1] || c[2]))
		printf((!depth || c[0]) && (!c[0] || !c[1]) ? "and " :
			c[0] ? " " : "");

	if (c[1] < 2) {
		if (c[1] || c[2]) printf("%s", ones[c[1] * 10 + c[2]]);
	} else {
		if (c[1]) {
			printf("%s", tens[c[1]]);
			if (c[2]) putchar('-');
		}
		if (c[2]) printf("%s", ones[c[2]]);
	}

	return 1;
}

int say_maxillion(const char *s, int len, int depth, int has_lead)
{
	int n = len / 3, r = len % 3;
	if (!r) {
		n--;
		r = 3;
	}
	const char *e = s + r;
	do {
		if (say_hundred(s, r, n, has_lead) && n) {
			has_lead = 1;
			printf(" %s", llions[n]);
			if (!depth) printf(", ");
			else printf(" ");
		}
		s = e; e += 3;
	} while (r = 3, n--);

	return 1;
}

void say_number(const char *s)
{
	int len, i, got_sign = 0;

	while (*s == ' ') s++;
	if (*s < '0' || *s > '9') {
		if (*s == '-') got_sign = -1;
		else if (*s == '+') got_sign = 1;
		else goto nan;
		s++;
	} else
		got_sign = 1;

	while (*s == '0') {
		s++;
		if (*s == '\0') {
			printf("zero\n");
			return;
		}
	}

	len = strlen(s);
	if (!len) goto nan;

	for (i = 0; i < len; i++) {
		if (s[i] < '0' || s[i] > '9') {
			printf("(not a number)");
			return;
		}
	}
	if (got_sign == -1) printf("minus ");

	int n = len / maxillion;
	int r = len % maxillion;
	if (!r) {
		r = maxillion;
		n--;
	}

	const char *end = s + len - n * maxillion;
	int has_lead = 0;
	do {
		if ((has_lead = say_maxillion(s, r, n, has_lead))) {
			for (i = 0; i < n; i++)
				printf(" %s", llions[maxillion / 3]);
			if (n) printf(", ");
		}
		n--;
		r = maxillion;
		s = end;
		end += r;
	} while (n >= 0);

	printf("\n");
	return;

nan:	printf("not a number\n");
	return;
}

int main()
{
	say_number("-42");
	say_number("1984");
	say_number("10000");
	say_number("1024");
	say_number("1001001001001");
	say_number("123456789012345678901234567890123456789012345678900000001");
	return 0;
}
```

```txt
minus forty-two
one thousand, nine hundred eighty-four
ten thousand,
one thousand, and twenty-four
one trillion, one billion, one million, one thousand, and one
one hundred twenty-three million four hundred fifty-six thousand seven hundred eighty-nine trillion trillion trillion trillion, twelve billion three hundred forty-five million six hundred seventy-eight thousand nine hundredand one trillion trillion trillion, two hundred thirty-four billion five hundred sixty-seven million eight hundred ninety thousand one hundred twenty-three trillion trillion, four hundred fifty-six billion seven hundred eighty-nine million twelve thousand three hundred forty-five trillion, six hundred seventy-eight billion, nine hundred million, and one
```



## C++


```cpp
#include <string>
#include <iostream>
using std::string;

const char* smallNumbers[] = {
  "zero", "one", "two", "three", "four", "five",
  "six", "seven", "eight", "nine", "ten",
  "eleven", "twelve", "thirteen", "fourteen", "fifteen",
  "sixteen", "seventeen", "eighteen", "nineteen"
};

string spellHundreds(unsigned n) {
  string res;
  if (n > 99) {
    res = smallNumbers[n/100];
    res += " hundred";
    n %= 100;
    if (n) res += " and ";
  }
  if (n >= 20) {
    static const char* Decades[] = {
      "", "", "twenty", "thirty", "forty",
      "fifty", "sixty", "seventy", "eighty", "ninety"
    };
    res += Decades[n/10];
    n %= 10;
    if (n) res += "-";
  }
  if (n < 20 && n > 0)
    res += smallNumbers[n];
  return res;
}


const char* thousandPowers[] = {
  " billion", " million",  " thousand", "" };

typedef unsigned long Spellable;

string spell(Spellable n) {
  if (n < 20) return smallNumbers[n];
  string res;
  const char** pScaleName = thousandPowers;
  Spellable scaleFactor = 1000000000;	// 1 billion
  while (scaleFactor > 0) {
    if (n >= scaleFactor) {
      Spellable h = n / scaleFactor;
      res += spellHundreds(h) + *pScaleName;
      n %= scaleFactor;
      if (n) res += ", ";
    }
    scaleFactor /= 1000;
    ++pScaleName;
  }
  return res;
}

int main() {
#define SPELL_IT(x) std::cout << #x " " << spell(x) << std::endl;
  SPELL_IT(      99);
  SPELL_IT(     300);
  SPELL_IT(     310);
  SPELL_IT(    1501);
  SPELL_IT(   12609);
  SPELL_IT(  512609);
  SPELL_IT(43112609);
  SPELL_IT(1234567890);
  return 0;
}
```

```txt

99 ninety-nine
300 three hundred
310 three hundred and ten
1501 one thousand, five hundred and one
12609 twelve thousand, six hundred and nine
512609 five hundred and twelve thousand, six hundred and nine
43112609 forty-three million, one hundred and twelve thousand, six hundred and nine
1234567890 one billion, two hundred and thirty-four million, five hundred and sixty-seven thousand, eight hundred and ninety

```


## C#
```c#
using System;

class NumberNamer {
    static readonly string[] incrementsOfOne =
            { "zero",    "one",     "two",       "three",    "four",
              "five",    "six",     "seven",     "eight",    "nine",
              "ten",     "eleven",  "twelve",    "thirteen", "fourteen",
              "fifteen", "sixteen", "seventeen", "eighteen", "nineteen" };

    static readonly string[] incrementsOfTen =
            { "",      "",      "twenty",  "thirty", "fourty",
              "fifty", "sixty", "seventy", "eighty", "ninety" };

    const string millionName = "million",
                 thousandName = "thousand",
                 hundredName = "hundred",
                 andName = "and";


    public static string GetName( int i ) {
        string output = "";
        if( i >= 1000000 ) {
            output += ParseTriplet( i / 1000000 ) + " " + millionName;
            i %= 1000000;
            if( i == 0 ) return output;
        }

        if( i >= 1000 ) {
            if( output.Length > 0 ) {
                output += ", ";
            }
            output += ParseTriplet( i / 1000 ) + " " + thousandName;
            i %= 1000;
            if( i == 0 ) return output;
        }

        if( output.Length > 0 ) {
            output += ", ";
        }
        output += ParseTriplet( i );
        return output;
    }


    static string ParseTriplet( int i ) {
        string output = "";
        if( i >= 100 ) {
            output += incrementsOfOne[i / 100] + " " + hundredName;
            i %= 100;
            if( i == 0 ) return output;
        }

        if( output.Length > 0 ) {
            output += " " + andName + " ";
        }
        if( i >= 20 ) {
            output += incrementsOfTen[i / 10];
            i %= 10;
            if( i == 0 ) return output;
        }

        if( output.Length > 0 ) {
            output += " ";
        }
        output += incrementsOfOne[i];
        return output;
    }
}


class Program { // Test class
    static void Main( string[] args ) {
        Console.WriteLine( NumberNamer.GetName( 1 ) );
        Console.WriteLine( NumberNamer.GetName( 234 ) );
        Console.WriteLine( NumberNamer.GetName( 31337 ) );
        Console.WriteLine( NumberNamer.GetName( 987654321 ) );
    }
}

```

```txt

one
two hundred and thirty four
thirty one thousand, three hundred and thirty seven
nine hundred and eighty seven million, six hundred and fifty four thousand, three hundred and twenty one

```



## Clojure

```clojure
(clojure.pprint/cl-format nil "~R" 1234)
=> "one thousand, two hundred thirty-four"
```



## CoffeeScript

```coffeescript

spell_integer = (n) ->
  tens = [null, null, "twenty", "thirty", "forty",
      "fifty", "sixty", "seventy", "eighty", "ninety"]

  small = ["zero", "one", "two", "three", "four", "five",
       "six", "seven", "eight", "nine", "ten", "eleven",
       "twelve", "thirteen", "fourteen", "fifteen",
       "sixteen", "seventeen", "eighteen", "nineteen"]

  bl = [null, null, "m", "b", "tr", "quadr",
      "quint", "sext", "sept", "oct", "non", "dec"]

  divmod = (n, d) ->
    [Math.floor(n / d), n % d]

  nonzero = (c, n) ->
    if n == 0
      ""
    else
      c + spell_integer n

  big = (e, n) ->
    if e == 0
      spell_integer n
    else if e == 1
      spell_integer(n) + " thousand"
    else
      spell_integer(n) + " " + bl[e] + "illion"

  base1000_rev = (n) ->
    # generates the value of the digits of n in base 1000
    # (i.e. 3-digit chunks), in reverse.
    chunks = []
    while n != 0
      [n, r] = divmod n, 1000
      chunks.push r
    chunks

  if n < 0
    throw Error "spell_integer: negative input"
  else if n < 20
    small[n]
  else if n < 100
    [a, b] = divmod n, 10
    tens[a] + nonzero("-", b)
  else if n < 1000
    [a, b] = divmod n, 100
    small[a] + " hundred" + nonzero(" ", b)
  else
    chunks = (big(exp, x) for x, exp in base1000_rev(n) when x)
    chunks.reverse().join ', '

# example
console.log spell_integer 1278
console.log spell_integer 1752
console.log spell_integer 2010
console.log spell_integer 4000123007913

```


```txt

> coffee spell_number.coffee
one thousand, two hundred seventy-eight
one thousand, seven hundred fifty-two
two thousand, ten
four trillion, one hundred twenty-three million, seven thousand, nine hundred thirteen

```



## Common Lisp



```lisp
(format nil "~R" 1234)
=> "one thousand two hundred thirty-four"
```



## D

use -version=number_names_main to see the output.

```d
import std.stdio, std.array, std.algorithm, std.bigint, std.range;

immutable tens = ["", "", "twenty", "thirty", "forty",
                  "fifty", "sixty", "seventy", "eighty", "ninety"];
immutable small = ["zero", "one", "two", "three", "four", "five",
                   "six", "seven", "eight", "nine", "ten", "eleven",
                   "twelve", "thirteen", "fourteen", "fifteen",
                   "sixteen", "seventeen", "eighteen", "nineteen"];
immutable huge = ["", ""] ~ ["m", "b", "tr", "quadr", "quint",
                             "sext", "sept", "oct", "non", "dec"]
                            .map!q{ a ~ "illion" }.array;

string spellBigInt(BigInt n) pure /*nothrow @safe*/ {
    static string nonZero(string c, BigInt n, string connect="") pure /*nothrow @safe*/ {
        return (n == 0) ? "" : (connect ~ c ~ n.spellBigInt);
    }

    static string lastAnd(string num) pure /*nothrow*/ @safe {
        if (num.canFind(',')) {
            string pre = num.retro.find(',').retro[0 .. $ - 1];
            string last = num[pre.length + 1 .. $];
            if (!last.canFind(" and "))
                last = " and" ~ last;
            num = pre ~ ',' ~ last;
        }
        return num;
    }

    static string big(in uint e, in BigInt n) pure /*nothrow @safe*/ {
        switch (e) {
            case 0:  return n.spellBigInt;
            case 1:  return n.spellBigInt ~ " thousand";
            default: return n.spellBigInt ~ " " ~ huge[e];
        }
    }

    if (n < 0) {
        return "minus " ~ spellBigInt(-n);
    } else if (n < 20) {
        return small[n.toInt];
    } else if (n < 100) {
        immutable BigInt a = n / 10;
        immutable BigInt b = n % 10;
        return tens[a.toInt] ~ nonZero("-", b);
    } else if (n < 1_000) {
        immutable BigInt a = n / 100;
        immutable BigInt b = n % 100;
        return small[a.toInt] ~ " hundred" ~ nonZero(" ", b, " and");
    } else {
        string[] bigs;
        uint e = 0;
        while (n != 0) {
            immutable BigInt r = n % 1_000;
            n /= 1_000;
            if (r != 0)
                bigs ~= big(e, r);
            e++;
        }

        return lastAnd(bigs.retro.join(", "));
    }
}

version(number_names_main) {
    void main() {
        foreach (immutable n; [0, -3, 5, -7, 11, -13, 17, -19, 23, -29])
            writefln("%+4d -> %s", n, n.BigInt.spellBigInt);
        writeln;

        auto n = 2_0121_002_001;
        while (n) {
            writefln("%-12d -> %s", n, n.BigInt.spellBigInt);
            n /= -10;
        }
        writefln("%-12d -> %s", n, n.BigInt.spellBigInt);
        writeln;
    }
}
```

```txt
  +0 -> zero
  -3 -> minus three
  +5 -> five
  -7 -> minus seven
 +11 -> eleven
 -13 -> minus thirteen
 +17 -> seventeen
 -19 -> minus nineteen
 +23 -> twenty-three
 -29 -> minus twenty-nine

20121002001  -> twenty billion, one hundred and twenty-one million, two thousand, and one
-2012100200  -> minus two billion, twelve million, one hundred thousand, and two hundred
201210020    -> two hundred and one million, two hundred and ten thousand, and twenty
-20121002    -> minus twenty million, one hundred and twenty-one thousand, and two
2012100      -> two million, twelve thousand, and one hundred
-201210      -> minus two hundred and one thousand, two hundred and ten
20121        -> twenty thousand, one hundred and twenty-one
-2012        -> minus two thousand, and twelve
201          -> two hundred and one
-20          -> minus twenty
2            -> two
0            -> zero
```



## Elixir

```elixir
defmodule RC do
  @small  ~w(zero one two three four five six seven eight nine ten
             eleven twelve thirteen fourteen fifteen sixteen seventeen
             eighteen nineteen)
  @tens  ~w(wrong wrong twenty thirty forty fifty sixty seventy eighty ninety)
  @big  [nil, "thousand"] ++
        (~w( m b tr quadr quint sext sept oct non dec) |> Enum.map(&"#{&1}illion"))

  def wordify(number) when number<0, do: "negative #{wordify(-number)}"
  def wordify(number) when number<20, do: Enum.at(@small,number)
  def wordify(number) when number<100 do
    rm = rem(number,10)
    Enum.at(@tens,div(number,10)) <> (if rm==0, do: "", else: "-#{wordify(rm)}")
  end
  def wordify(number) when number<1000 do
    rm = rem(number,100)
    "#{Enum.at(@small,div(number,100))} hundred" <> (if rm==0, do: "", else: " and #{wordify(rm)}")
  end
  def wordify(number) do
    # separate into 3-digit chunks
    chunks = chunk(number, [])
    if length(chunks) > length(@big), do: raise(ArgumentError, "Integer value too large.")
    Enum.map(chunks, &wordify(&1))
    |> Enum.zip(@big)
    |> Enum.filter_map(fn {a,_} -> a != "zero" end, fn {a,b} -> "#{a} #{b}" end)
    |> Enum.reverse
    |> Enum.join(", ")
  end

  defp chunk(0, res), do: Enum.reverse(res)
  defp chunk(number, res) do
    chunk(div(number,1000), [rem(number,1000) | res])
  end
end

data = [-1123, 0, 1, 20, 123, 200, 220, 1245, 2000, 2200, 2220, 467889,
        23_000_467, 23_234_467, 2_235_654_234, 12_123_234_543_543_456,
        987_654_321_098_765_432_109_876_543_210_987_654,
        123890812938219038290489327894327894723897432]

Enum.each(data, fn n ->
  IO.write "#{n}: "
  try do
    IO.inspect RC.wordify(n)
  rescue
    e in ArgumentError -> IO.puts Exception.message(e)
  end
end)
```


```txt

-1123: "negative one thousand, one hundred and twenty-three "
0: "zero"
1: "one"
20: "twenty"
123: "one hundred and twenty-three"
200: "two hundred"
220: "two hundred and twenty"
1245: "one thousand, two hundred and forty-five "
2000: "two thousand"
2200: "two thousand, two hundred "
2220: "two thousand, two hundred and twenty "
467889: "four hundred and sixty-seven thousand, eight hundred and eighty-nine "
23000467: "twenty-three million, four hundred and sixty-seven "
23234467: "twenty-three million, two hundred and thirty-four thousand, four hundred and sixty-seven "
2235654234: "two billion, two hundred and thirty-five million, six hundred and fifty-four thousand, two hundred and thirty-four "
12123234543543456: "twelve quadrillion, one hundred and twenty-three trillion, two hundred and thirty-four billion, five hundred and forty-three million, five hundred and forty-three thousand, four hundred and fifty-six "
987654321098765432109876543210987654: "nine hundred and eighty-seven decillion, six hundred and fifty-four nonillion, three hundred and twenty-one octillion, ninety-eight septillion, seven hundred and sixty-five sextillion, four hundred and thirty-two quintillion, one hundred and nine quadrillion, eight hundred and seventy-six trillion, five hundred and forty-three billion, two hundred and ten million, nine hundred and eighty-seven thousand, six hundred and fifty-four "
123890812938219038290489327894327894723897432: Integer value too large.

```



## Erlang



```erlang

-module(nr2eng).
-import(lists, [foreach/2, seq/2, append/2]).
-import(string, [strip/3, str/2]).
-export([start/0]).

sym(1) -> "one";
sym(2) -> "two";
sym(3) -> "three";
sym(4) -> "four";
sym(5) -> "five";
sym(6) -> "six";
sym(7) -> "seven";
sym(8) -> "eight";
sym(9) -> "nine";
sym(10) -> "ten";
sym(11) -> "eleven";
sym(12) -> "twelve";
sym(13) -> "thirteen";
sym(20) -> "twenty";
sym(30) -> "thirty";
sym(40) -> "forty";
sym(50) -> "fifty";
sym(100) -> "hundred";
sym(1000) -> "thousand";
sym(1000*1000) -> "million";
sym(1000*1000*1000) -> "billion";
sym(_) -> "".

next(1000) -> 100;
next(100) -> 10;
next(10) -> 1;
next(X) -> X div 1000.

concat(PRE, "") ->
   PRE;
concat(PRE, POST) ->
   PRE++" "++POST.
concat("", _, POST) ->
   POST;
concat(PRE, SYM, "") ->
   PRE++" "++SYM;
concat(PRE, SYM, POST) ->
   PRE++" "++SYM++" "++POST.

nr2eng(0, _) ->
   "";
nr2eng(NR, 1) ->
   sym(NR);
nr2eng(NR, 10) when NR =< 20 ->
   case sym(NR) of
        "" -> strip(sym(NR-10), right, $t) ++ "teen";
        _  -> sym(NR)
   end;
nr2eng(NR, 10) ->
   concat(
     case sym((NR div 10)*10) of
        "" -> strip(sym(NR div 10), right, $t) ++ "ty";
        _  -> sym((NR div 10)*10)
     end,
     nr2eng(NR rem 10, 1));
nr2eng(NR, B) ->
   PRE  = nr2eng(NR div B, next(B)),
   POST = nr2eng(NR rem B, next(B)),
   AND  = str(POST, "and"),
   COMMA = if
              POST == "" -> "";
              AND == 0 -> " and";
              B >= 1000 -> ",";
              true -> ""
   end,
   concat(PRE,  sym(B)++COMMA, POST).

start() ->
   lists:foreach(
     fun (X) -> io:fwrite("~p ~p ~n", [X, nr2eng(X, 1000000000)])
     end,
     append(seq(1, 2000), [123123, 43234234])).

```

```txt

1999 "one thousand, nine hundred and ninety nine"
2000 "two thousand"
123123 "one hundred and twenty three thousand, one hundred and twenty three"
43234234 "forty three million, two hundred and thirty four thousand, two hundred and thirty four"

```



## Euphoria

```euphoria
function abs(atom i)
    if i < 0 then
        return -i
    else
        return i
    end if
end function

constant small = {"one", "two", "three", "four", "five", "six", "seven", "eight",
    "nine", "ten","eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen",
    "seventeen", "eighteen", "nineteen"}

constant tens = {"twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty",
    "ninety"}

constant big = {"thousand", "million", "billion"}

function int2text(atom number)
    atom num
    integer unit, tmpLng1
    sequence outP
    outP = ""
    num = 0
    unit = 1
    tmpLng1 = 0

    if number = 0 then
        return "zero"
    end if

    num = abs(number)
    while 1 do
        tmpLng1 = remainder(num,100)
        if tmpLng1 > 0 and tmpLng1 < 20 then
            outP = small[tmpLng1] & ' ' & outP
        elsif tmpLng1 >= 20 then
            if remainder(tmpLng1,10) = 0 then
                outP = tens[floor(tmpLng1/10)-1] & ' ' & outP
            else
                outP = tens[floor(tmpLng1/10)-1] & '-' & small[remainder(tmpLng1, 10)] & ' ' & outP
            end if
        end if

        tmpLng1 = floor(remainder(num, 1000) / 100)
        if tmpLng1 then
            outP = small[tmpLng1] & " hundred " & outP
        end if

        num = floor(num/1000)
        if num < 1 then
            exit
        end if

        tmpLng1 = remainder(num,1000)
        if tmpLng1 then
            outP = big[unit] & ' ' & outP
        end if

        unit = unit + 1
    end while

    if number < 0 then
        outP = "negative " & outP
    end if

    return outP[1..$-1]
end function

puts(1,int2text(900000001) & "\n")
puts(1,int2text(1234567890) & "\n")
puts(1,int2text(-987654321) & "\n")
puts(1,int2text(0) & "\n")
```


```txt
nine hundred million one
one billion two hundred thirty-four million five hundred sixty-seven thousand eight hundred ninety
negative nine hundred eighty-seven million six hundred fifty-four thousand three hundred twenty-one
zero
```


=={{header|F_Sharp|F#}}==
```fsharp
let divMod n d = n / d, n % d

let join = String.concat ", "

let rec nonzero = function
  | _, 0 -> ""
  | c, n -> c + (spellInteger n)

and tens n =
  [| ""; ""; "twenty"; "thirty"; "forty"; "fifty";
             "sixty"; "seventy"; "eighty"; "ninety" |].[n]

and small n =
  [| "zero"; "one"; "two"; "three"; "four"; "five";
     "six"; "seven"; "eight"; "nine"; "ten"; "eleven";
     "twelve"; "thirteen"; "fourteen"; "fifteen";
     "sixteen";"seventeen"; "eighteen"; "nineteen" |].[n]

and bl = [| ""; ""; "m"; "b"; "tr"; "quadr"; "quint";
                    "sext"; "sept"; "oct"; "non"; "dec" |]

and big = function
  | 0, n -> (spellInteger n)
  | 1, n -> (spellInteger n) + " thousand"
  | e, n -> (spellInteger n) + " " + bl.[e] + "illion"

and uff acc = function
  | 0 -> List.rev acc
  | n ->
      let a, b = divMod n 1000
      uff (b::acc) a

and spellInteger = function
  | n when n < 0 -> "minus " + spellInteger (abs n)
  | n when n < 20 -> small n
  | n when n < 100 ->
      let a, b = divMod n 10
      (tens a) + nonzero ("-", b)
  | n when n < 1000 ->
      let a, b = divMod n 100
      (small a) + " hundred" + nonzero (" ", b)
  | n ->
      let seg = uff [] n
      let _, segn =
        (* just add the index of the item in the list *)
        List.fold
          (fun (i,acc) v -> i + 1, (i, v)::acc)
          (0, [])
          seg

      let fsegn =
        (* remove right part "zero" *)
        List.filter
          (function (_, 0) -> false | _ -> true)
          segn

      join (List.map big fsegn)
;;
```



## Factor

Factor "cheats" by having a standard library module for this task:


```factor
IN: scratchpad USE: math.text.english
IN: scratchpad 43112609 number>text print
forty-three million, one hundred and twelve thousand, six hundred and nine

```


=={{header|Fōrmulæ}}==

In [https://wiki.formulae.org/Data_types_tutorial#Number_names this] page you can see the solution of this task.

Fōrmulæ programs are not textual, visualization/edition of programs is done showing/manipulating structures but not text ([http://wiki.formulae.org/Editing_F%C5%8Drmul%C3%A6_expressions more info]). Moreover, there can be multiple visual representations of the same program. Even though it is possible to have textual representation &mdash;i.e. XML, JSON&mdash; they are intended for transportation effects more than visualization and edition.

The option to show Fōrmulæ programs and their results is showing images. Unfortunately images cannot be uploaded in Rosetta Code.


## Fortran

```fortran
program spell

  implicit none
  integer :: e
  integer :: i
  integer :: m
  integer :: n
  character (9), dimension (19), parameter :: small =       &
    & (/'one      ', 'two      ', 'three    ', 'four     ', &
    &   'five     ', 'six      ', 'seven    ', 'eight    ', &
    &   'nine     ', 'ten      ', 'eleven   ', 'twelve   ', &
    &   'thirteen ', 'fourteen ', 'fifteen  ', 'sixteen  ', &
    &   'seventeen', 'eighteen ', 'nineteen '/)
  character (7), dimension (2 : 9), parameter :: tens =        &
    & (/'twenty ', 'thirty ', 'forty  ', 'fifty  ', 'sixty  ', &
    &   'seventy', 'eighty ', 'ninety '/)
  character (8), dimension (3), parameter :: big = &
    & (/'thousand', 'million ', 'billion '/)
  character (256) :: r

  do
    read (*, *, iostat = i) n
    if (i /= 0) then
      exit
    end if
    if (n == 0) then
      r = 'zero'
    else
      r = ''
      m = abs (n)
      e = 0
      do
        if (m == 0) then
          exit
        end if
        if (modulo (m, 1000) > 0) then
          if (e > 0) then
            r = trim (big (e)) // ' ' // r
          end if
          if (modulo (m, 100) > 0) then
            if (modulo (m, 100) < 20) then
              r = trim (small (modulo (m, 100))) // ' ' // r
            else
              if (modulo (m, 10) > 0) then
                r = trim (small (modulo (m, 10))) // ' ' // r
                r = trim (tens (modulo (m, 100) / 10)) // '-' // r
              else
                r = trim (tens (modulo (m, 100) / 10)) // ' ' // r
              end if
            end if
          end if
          if (modulo (m, 1000) / 100 > 0) then
            r = 'hundred' // ' ' // r
            r = trim (small (modulo (m, 1000) / 100)) // ' ' // r
          end if
        end if
        m = m / 1000
        e = e + 1
      end do
      if (n < 0) then
        r = 'negative' // ' ' // r
      end if
    end if
    write (*, '(a)') trim (r)
  end do

end program spell
```

Sample input:

```txt
-1
0
1
42
2147483647
```

Output:

```txt
negative one
zero
one
forty-two
two billion one hundred forty-seven million four hundred eighty-three thousand six hundred forty-seven
```



## Go

Supports integers from <code>math.MinInt64 + 1</code> to <code>math.MaxInt64</code>.

```go
package main

import "fmt"

func main() {
	for _, n := range []int64{12, 1048576, 9e18, -2, 0} {
		fmt.Println(say(n))
	}
}

var small = [...]string{"zero", "one", "two", "three", "four", "five", "six",
	"seven", "eight", "nine", "ten", "eleven", "twelve", "thirteen",
	"fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"}
var tens = [...]string{"", "", "twenty", "thirty", "forty",
	"fifty", "sixty", "seventy", "eighty", "ninety"}
var illions = [...]string{"", " thousand", " million", " billion",
	" trillion", " quadrillion", " quintillion"}

func say(n int64) string {
	var t string
	if n < 0 {
		t = "negative "
		// Note, for math.MinInt64 this leaves n negative.
		n = -n
	}
	switch {
	case n < 20:
		t += small[n]
	case n < 100:
		t += tens[n/10]
		s := n % 10
		if s > 0 {
			t += "-" + small[s]
		}
	case n < 1000:
		t += small[n/100] + " hundred"
		s := n % 100
		if s > 0 {
			t += " " + say(s)
		}
	default:
		// work right-to-left
		sx := ""
		for i := 0; n > 0; i++ {
			p := n % 1000
			n /= 1000
			if p > 0 {
				ix := say(p) + illions[i]
				if sx != "" {
					ix += " " + sx
				}
				sx = ix
			}
		}
		t += sx
	}
	return t
}
```

Output:

```txt

twelve
one million forty-eight thousand five hundred seventy-six
nine quintillion
negative two
zero

```



## Groovy


```groovy
def divMod(BigInteger number, BigInteger divisor) {
    def qr = number.divideAndRemainder(divisor)
    [div:qr[0], remainder:qr[1]]
}

def toText(value) {
    value = value as BigInteger
    def units = ['zero', 'one', 'two', 'three', 'four', 'five', 'six', 'seven', 'eight', 'nine', 'ten',
            'eleven', 'twelve', 'thirteen', 'fourteen', 'fifteen', 'sixteen', 'seventeen', 'eighteen', 'nineteen']
    def tens = ['', '', 'twenty', 'thirty', 'forty', 'fifty', 'sixty', 'seventy', 'eighty', 'ninety']
    def big = ['', 'thousand'] + ['m', 'b', 'tr', 'quadr', 'quint', 'sext', 'sept', 'oct', 'non', 'dec'].collect { "${it}illion"}

    if (value < 0) {
        "negative ${toText(-value)}"
    } else if (value < 20) {
        units[value]
    } else if (value < 100) {
        divMod(value, 10).with { "${tens[div]} ${units[remainder]}".replace(' zero', '') }
    } else if (value < 1000) {
        divMod(value, 100).with { "${toText(div)} hundred and ${toText(remainder)}".replace(' and zero', '') }
    } else {
        def chunks = []
        while (value != 0) {
            divMod(value, 1000).with {
                chunks << remainder
                value = div
            }
        }
        if (chunks.size() > big.size()) {
            throw new IllegalArgumentException("Number overflow")
        }
        def text = []
        (0..<chunks.size()).each { index ->
            if (chunks[index] > 0) {
                text << "${toText(chunks[index])}${index == 0 ? '' : ' ' + big[index]}"
                if (index == 0 && chunks[index] < 100) {
                    text << "and"
                }
            }
        }
        text.reverse().join(', ').replace(', and,', ' and')
    }
}

// Add this method to all Numbers
Number.metaClass.toText = { toText(delegate) }

println toText(29)
println 40.toText()
println toText(401)
println 9003.toText()
println toText(8011673)
println 8000100.toText()
println 4629436.toText()
948623487512387455323784623842314234.toText().split(',').each { println it.trim() }

def verifyToText(expected, value) {
    println "Checking '$expected' == $value"
    def actual = value.toText()
    assert expected == actual
}

verifyToText 'nineteen', 19
verifyToText 'one thousand, two hundred and thirty four', 1234
verifyToText 'twenty three million, four hundred and fifty nine thousand, six hundred and twelve', 23459612
verifyToText 'one thousand, nine hundred and ninety nine', 1999
verifyToText 'negative six hundred and one', -601
verifyToText 'twelve billion and nineteen', 12000000019
verifyToText 'negative one billion, two hundred and thirty four million, five hundred and sixty seven thousand, eight hundred and ninety', -1234567890
verifyToText 'one hundred and one', 101
verifyToText 'one thousand and one', 1001
verifyToText 'one million, one hundred and one', 1000101
verifyToText 'one million and forty five', 1000045
verifyToText 'one million and fifteen', 1000015
verifyToText 'one billion, forty five thousand and one', 1000045001
```

Output:

```txt
twenty nine
forty
four hundred and one
nine thousand and three
eight million, eleven thousand, six hundred and seventy three
eight million, one hundred
four million, six hundred and twenty nine thousand, four hundred and thirty six
nine hundred and forty eight decillion
six hundred and twenty three nonillion
four hundred and eighty seven octillion
five hundred and twelve septillion
three hundred and eighty seven sextillion
four hundred and fifty five quintillion
three hundred and twenty three quadrillion
seven hundred and eighty four trillion
six hundred and twenty three billion
eight hundred and forty two million
three hundred and fourteen thousand
two hundred and thirty four
Checking 'nineteen' == 19
Checking 'one thousand, two hundred and thirty four' == 1234
Checking 'twenty three million, four hundred and fifty nine thousand, six hundred and twelve' == 23459612
Checking 'one thousand, nine hundred and ninety nine' == 1999
Checking 'negative six hundred and one' == -601
Checking 'twelve billion and nineteen' == 12000000019
Checking 'negative one billion, two hundred and thirty four million, five hundred and sixty seven thousand, eight hundred and ninety' == -1234567890
Checking 'one hundred and one' == 101
Checking 'one thousand and one' == 1001
Checking 'one million, one hundred and one' == 1000101
Checking 'one million and forty five' == 1000045
Checking 'one million and fifteen' == 1000015
Checking 'one billion, forty five thousand and one' == 1000045001
```



## Haskell


```haskell
import Data.List (intercalate, unfoldr)

spellInteger :: Integer -> String
spellInteger n
 | n <    0  = "negative " ++ spellInteger (-n)
 | n <   20  = small n
 | n <  100  = let (a, b) = n `divMod` 10
               in  tens a ++ nonzero '-' b
 | n < 1000  = let (a, b) = n `divMod` 100
               in  small a ++ " hundred" ++ nonzero ' ' b
 | otherwise = intercalate ", " $ map big $ reverse $
               filter ((/= 0) . snd) $ zip [0..] $ unfoldr uff n

 where nonzero :: Char -> Integer -> String
       nonzero _ 0 = ""
       nonzero c n = c : spellInteger n

       uff :: Integer -> Maybe (Integer, Integer)
       uff 0 = Nothing
       uff n = Just $ uncurry (flip (,)) $ n `divMod` 1000

       small, tens :: Integer -> String
       small = (["zero", "one", "two", "three", "four", "five",
            "six", "seven", "eight", "nine", "ten", "eleven",
            "twelve", "thirteen", "fourteen", "fifteen", "sixteen",
            "seventeen", "eighteen", "nineteen"] !!) . fromEnum
       tens = ([undefined, undefined, "twenty", "thirty", "forty",
           "fifty", "sixty", "seventy", "eighty", "ninety"] !!) .
           fromEnum

       big :: (Int, Integer) -> String
       big (0, n) = spellInteger n
       big (1, n) = spellInteger n ++ " thousand"
       big (e, n) = spellInteger n ++ ' ' : (l !! e) ++ "illion"
         where l = [undefined, undefined, "m", "b", "tr", "quadr",
                   "quint", "sext", "sept", "oct", "non", "dec"]
```



## HicEst


```HicEst
SUBROUTINE NumberToWords(number)
 CHARACTER outP*255, small*130, tens*80, big*80
 REAL ::   decimal_places = 7
 INIT( APPENDIX("#literals"), small, tens, big)

 num = ABS( INT(number) )
 order = 0
 outP = ' '
 DO i = 1, num + 1
   tmp = MOD(num, 100)
   IF(tmp > 19) THEN
       EDIT(Text=tens, ITeM=INT(MOD(tmp/10, 10)), Parse=medium)
       IF( MOD(tmp, 10) ) THEN
           EDIT(Text=small, ITeM=MOD(tmp,10)+1, Parse=mini)
           outP = medium // '-' // mini // ' ' // outP
       ELSE
           outP = medium // ' ' // outP
       ENDIF
   ELSEIF(tmp > 0) THEN
       EDIT(Text=small, ITeM=tmp+1, Parse=mini)
       outP = mini // ' '// outP
   ELSEIF(number == 0) THEN
       outP = 'zero'
   ENDIF

   tmp = INT(MOD(num, 1000) / 100)
   IF(tmp) THEN
       EDIT(Text=small, ITeM=tmp+1, Parse=oneto19)
       outP = oneto19 // ' hundred ' // outP
   ENDIF

   num = INT(num /1000)
   IF( num == 0) THEN
       IF(number < 0) outP = 'minus ' // outP
       fraction = ABS( MOD(number, 1) )
       IF(fraction) WRITE(Text=outP, APPend) ' point'
       DO j = 1, decimal_places
         IF( fraction >= 10^(-decimal_places) ) THEN
             num = INT( 10.01 * fraction )
             EDIT(Text=small, ITeM=num+1, Parse=digit)
             WRITE(Text=outP, APPend) ' ', digit
             fraction = 10*fraction - num
         ENDIF
       ENDDO
       OPEN(FIle="temp.txt", APPend)
       WRITE(FIle="temp.txt", Format='F10, " = ", A', CLoSe=1) number, outP
       RETURN
   ENDIF

   order = order + 1
   EDIT(Text=big, ITeM=order, Parse=kilo)
   IF( MOD(num, 1000) ) outP = kilo // ' and '// outP
 ENDDO
END

CALL NumberToWords( 0 )
CALL NumberToWords( 1234 )
CALL NumberToWords( 1234/100 )
CALL NumberToWords( 10000000 + 1.2 )
CALL NumberToWords( 2^15 )
CALL NumberToWords( 0.001 )
CALL NumberToWords( -EXP(1) )

#literals
 SMALL= zero one two three four five six seven eight nine ten &
 eleven twelve thirteen fourteen fifteen sixteen seventeen eighteen nineteen

 TENS=ten twenty thirty forty fifty sixty seventy eighty ninety

 BIG=thousand million billion trillion quadrillion
```


```HicEst
0           = zero
1234        = one thousand and two hundred thirty-four
12.34       = twelve point three four
10000001.2  = ten million and one point two
32768       = thirty-two thousand and seven hundred sixty-eight
1E-3        =  point zero zero one
-2.7182818  = minus two point seven one eight two eight one eight
```


== {{header|Icon}} and {{header|Unicon}} ==

```Icon
link numbers    # commas, spell

procedure main(arglist)
every x := !arglist do
   write(commas(x), " -> ",spell(x))
end
```


[http://www.cs.arizona.edu/icon/library/src/procs/numbers.icn numbers:spell] was used as a based for this procedure.


```Icon
procedure spell(n)		#: spell out integer (short scale)
   local m, i
   static scale
   initial {
      scale := [ "thousand", "million", "billion", "trillion", "quadrillion", "quintillion", "sextillion","septillion"]
      every scale[i := 1 to *scale ] := [ integer(repl("999",i + 1)), -3 * i, " "||scale[i] ]
      push(scale,[999,2," hundred"])
     }

   n := integer(n) | stop(image(n)," is not an integer")
   if n < 0 then return "negative " || spell(-n)
   if n <= 12 then return {
      "0zero,1one,2two,3three,4four,5five,6six,7seven,8eight,_
         9nine,10ten,11eleven,12twelve," ? {
            tab(find(n))
            move(*n)
            tab(find(","))
            }
      }
   else if n <= 19 then return {
      spell(n[2] || "0") ?
         (if ="for" then "four" else tab(find("ty"))) || "teen"
      }
   else if n <= 99 then return {
      "2twen,3thir,4for,5fif,6six,7seven,8eigh,9nine," ? {
         tab(find(n[1]))
         move(1)
         tab(find(",")) || "ty" ||
            (if n[2] ~= 0 then "-" || spell(n[2]) else "")
         }
      }
   else if n <= scale[i := 1 to *scale,1] then return {     # generalize based on scale
      spell(n[1:scale[i,2]]) || scale[i,3] ||
         (if (m := n[scale[i,2]:0]) ~= 0 then " and " || spell(m) else "")
      }
   else fail                                                # really big
end
```

Sample output:

```txt
#spell.exe 5 11 15 67 10132767 65535 -1234567890123456

5 -> five
11 -> eleven
15 -> fifteen
67 -> sixty-seven
10,132,767 -> ten million and one hundred and thirty-two thousand and seven hundred and sixty-seven
65,535 -> sixty-five thousand and five hundred and thirty-five
-1,234,567,890,123,456 -> negative one quadrillion and two hundred and thirty-four trillion and five hundred and sixty-seven billion and eight hundred and ninety million and one hundred and twenty-three thousand and four hundred and fifty-six
```



## Inform 7

```inform7>say 32767 in words;</lang


```inform7>say 2147483647 in words;</lang


== {{header|J}} ==
'''Solutions:'''

```j
u=. ;:'one two three four five six seven eight nine'
v=. ;:'ten eleven twelve thirteen fourteen fifteen sixteen seventeen eighteen nineteen'
t=. ;:'twenty thirty forty fifty sixty seventy eighty ninety'
EN100=: '' ; u , v , , t ,&.>/ '';'-',&.>u

z=. '' ; 'thousand' ; (;:'m b tr quadr quint sext sept oct non'),&.> <'illion'
u=. ;:'un duo tre quattuor quin sex septen octo novem'
t=. (;:'dec vigint trigint quadragint quinquagint sexagint septuagint octogint nonagint'),&.><'illion'
ENU=: z , (, t ,~&.>/ '';u) , <'centillion'

en3=: 4 : 0
 'p q'=. 0 100#:y
 (p{::EN100),((*p)#' hundred'),((p*&*q)#x),q{::EN100
)

en=: 4 : 0
 d=. 1000&#.^:_1 y
 assert. (0<:y) *. ((=<.)y) *. d <:&# ENU
 c=. x&en3&.> (*d)#d
 ((0=y)#'zero') , (-2+*{:d) }. ; , c,.(<' '),.(ENU{~I.&.|.*d),.<', '
)

uk=: ' and '&en   NB. British
us=: ' '    &en   NB. American
```


'''Example:'''

```txt
   uk 123456789
one hundred and twenty-three million, four hundred and fifty-six thousand, seven hundred and eighty-nine
   us 123456789
one hundred twenty-three million, four hundred fifty-six thousand, seven hundred eighty-nine
   us 1234567890123456789012345678901234567890123456789012345678901234567890x
one duovigintillion, two hundred thirty-four unvigintillion, five hundred sixty-seven vigintillion, eight hundred ninety novemdecillion, one hundred twenty-three octodecillion, four hundred fifty-six septendecillion, seven hundred eighty-nine sexdecillion, twelve quindecillion, three hundred forty-five quattuordecillion, six hundred seventy-eight tredecillion, nine hundred one duodecillion, two hundred thirty-four undecillion, five hundred sixty-seven decillion, eight hundred ninety nonillion, one hundred twenty-three octillion, four hundred fifty-six septillion, seven hundred eighty-nine sextillion, twelve quintillion, three hundred forty-five quadrillion, six hundred seventy-eight trillion, nine hundred one billion, two hundred thirty-four million, five hundred sixty-seven thousand, eight hundred ninety
```



## Java



```java
public enum IntToWords {
    ;

    private static final String[] small = {
            "", "one", "two", "three", "four",
            "five", "six", "seven", "eight", "nine",
            "ten", "eleven", "twelve", "thirteen", "fourteen",
            "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"};
    private static final String[] tens = {
            "", "", "twenty", "thirty", "forty",
            "fifty", "sixty", "seventy", "eighty", "ninety"};
    private static final String[] big = {
            "", "thousand", "million", "billion", "trillion",
            "quadrillion", "quintillion"};

    public static void main(String[] args) {
        System.out.println(int2Text(0));
        System.out.println(int2Text(10));
        System.out.println(int2Text(30));
        System.out.println(int2Text(47));
        System.out.println(int2Text(100));
        System.out.println(int2Text(999));
        System.out.println(int2Text(1000));
        System.out.println(int2Text(9999));
        System.out.println(int2Text(123_456));
        System.out.println(int2Text(900_000_001));
        System.out.println(int2Text(1_234_567_890));
        System.out.println(int2Text(-987_654_321));
        System.out.println(int2Text(Long.MAX_VALUE));
        System.out.println(int2Text(Long.MIN_VALUE));
    }

    public static String int2Text(long number) {
        StringBuilder sb = new StringBuilder();

        if (number == 0) {
            return "zero";
        }

        long num = -Math.abs(number);

        int unit = 1;
        while (true) {
            int rem100 = (int) -(num % 100);
            if (rem100 >= 20) {
                if (rem100 % 10 == 0) {
                    sb.insert(0, tens[rem100 / 10] + " ");
                } else {
                    sb.insert(0, tens[rem100 / 10] + "-" + small[rem100 % 10] + " ");
                }
            } else if (rem100 != 0) {
                sb.insert(0, small[rem100] + " ");
            }

            int hundreds = (int) -(num % 1000) / 100;
            if (hundreds != 0) {
                sb.insert(0, small[hundreds] + " hundred ");
            }

            num /= 1000;
            if (num == 0) {
                break;
            }

            int rem1000 = (int) -(num % 1000);
            if (rem1000 != 0) {
                sb.insert(0, big[unit] + " ");
            }
            unit++;
        }

        if (number < 0) {
            sb.insert(0, "negative ");
        }

        return sb.toString().trim();
    }
}
```

Output:

```txt
zero
ten
thirty
forty-seven
one hundred
nine hundred ninety-nine
one thousand
nine thousand nine hundred ninety-nine
one hundred twenty-three thousand four hundred fifty-six
nine hundred million one
one billion two hundred thirty-four million five hundred sixty-seven thousand eight hundred ninety
negative nine hundred eighty-seven million six hundred fifty-four thousand three hundred twenty-one
nine quintillion two hundred twenty-three quadrillion three hundred seventy-two trillion thirty-six billion eight hundred fifty-four million seven hundred seventy-five thousand eight hundred seven
negative nine quintillion two hundred twenty-three quadrillion three hundred seventy-two trillion thirty-six billion eight hundred fifty-four million seven hundred seventy-five thousand eight hundred eight
```



### Recursive



```java
public class NumberToWordsConverter { // works upto 9999999

	final private  static String[] units = {"Zero","One","Two","Three","Four",
		"Five","Six","Seven","Eight","Nine","Ten",
		"Eleven","Twelve","Thirteen","Fourteen","Fifteen",
		"Sixteen","Seventeen","Eighteen","Nineteen"};
	final private static String[] tens = {"","","Twenty","Thirty","Forty","Fifty",
		"Sixty","Seventy","Eighty","Ninety"};

	public static String convert(Integer i) {
		//
		if( i < 20)  return units[i];
		if( i < 100) return tens[i/10] + ((i % 10 > 0)? " " + convert(i % 10):"");
		if( i < 1000) return units[i/100] + " Hundred" + ((i % 100 > 0)?" and " + convert(i % 100):"");
		if( i < 1000000) return convert(i / 1000) + " Thousand " + ((i % 1000 > 0)? " " + convert(i % 1000):"") ;
		return convert(i / 1000000) + " Million " + ((i % 1000000 > 0)? " " + convert(i % 1000000):"") ;
	}
}
```



## JavaScript

```JavaScript>const divMod = y => x =
 [Math.floor(y/x), y % x];

const sayNumber = value => {
  let name = '';
  let quotient, remainder;
  const dm = divMod(value);
  const units = ['zero', 'one', 'two', 'three', 'four', 'five', 'six', 'seven',
    'eight', 'nine', 'ten', 'eleven', 'twelve', 'thirteen', 'fourteen',
    'fifteen', 'sixteen', 'seventeen', 'eighteen', 'nineteen'];
  const tens = ['', '', 'twenty', 'thirty', 'forty', 'fifty', 'sixty',
    'seventy', 'eighty', 'ninety'];
  const big = [...['', 'thousand'], ...['m', 'b', 'tr', 'quadr', 'quint',
    'sext', 'sept', 'oct', 'non', 'dec'].map(e => `${e}illion`)];

  if (value < 0) {
    name = `negative ${sayNumber(-value)}`
  } else if (value < 20) {
    name = units[value]
  } else if (value < 100) {
    [quotient, remainder] = dm(10);
    name = `${tens[quotient]} ${units[remainder]}`.replace(' zero', '');
  } else if (value < 1000) {
    [quotient, remainder] = dm(100);
    name = `${sayNumber(quotient)} hundred and ${sayNumber(remainder)}`
      .replace(' and zero', '')
  } else {
    const chunks = [];
    const text = [];
    while (value !== 0) {
      [value, remainder] = divMod(value)(1000);
      chunks.push(remainder);
    }
    chunks.forEach((e,i) => {
      if (e > 0) {
        text.push(`${sayNumber(e)}${i === 0 ? '' : ' ' + big[i]}`);
        if (i === 0 && e < 100) {
          text.push('and');
        }
      }
    });
    name = text.reverse().join(', ').replace(', and,', ' and');
  }
  return name;
};
```



## Joy


```Joy

DEFINE units ==
[ "zero" "one" "two" "three" "four" "five" "six" "seven" "eight" "nine" "ten"
  "eleven" "twelve" "thirteen" "fourteen" "fifteen" "sixteen" "seventeen"
  "eighteen" "nineteen" ];

tens ==
[ "ten" "twenty" "thirty" "forty" "fifty" "sixty" "seventy" "eighty" "ninety" ];

convert6 ==
[1000000 <]
[1000 div swap convert " thousand " putchars convert3]
[1000000 div swap convert " million " putchars convert3]
ifte;

convert5 ==
[null]
[]
[" and " putchars convert]
ifte;

convert4 ==
[1000 <]
[100 div swap units of putchars " hundred" putchars convert5]
[convert6]
ifte;

convert3 ==
[null]
[]
[32 putch convert]
ifte;

convert2 ==
[100 <]
[10 div swap pred tens of putchars convert3]
[convert4]
ifte;

convert ==
[20 <]
[units of putchars]
[convert2]
ifte.

```



## jq


```jq
# Adapted from the go version.
# Tested with jq 1.4
#
# say/0 as defined here supports positive and negative integers within
# the range of accuracy of jq, or up to the quintillions, whichever is
# less.  As of jq version 1.4, jq's integer accuracy is about 10^16.

def say:

  # subfunction zillions recursively handles the thousands,
  # millions, billions, etc.
  #   input: the number
  #   i: which "illion" to use
  #   sx: the string so far
  #   output: the updated string
  def zillions(i; sx):
    ["thousand", "million", "billion",
      "trillion", "quadrillion", "quintillion"] as $illions
    | if . == 0 then sx
       else (. / 1000 | floor)
            | (. % 1000) as $p
            | zillions(i + 1;
                       if $p > 0 then
                          (($p | say) + " " + $illions[i]) as $ix
                          | if sx != "" then $ix + ", " + sx
                            else $ix
                            end
                       else sx
                       end)
       end
  ;

  [ "", "one", "two", "three", "four", "five", "six", "seven",
    "eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen",
    "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"] as $small
  | ["ones", "ten", "twenty", "thirty", "forty",
     "fifty", "sixty", "seventy", "eighty", "ninety"] as $tens

  | if . == 0 then "zero"
    elif . < 0 then "minus " + (-(.) | say)
    elif . < 20 then $small[.]
    elif . < 100 then
        $tens[./10|floor] as $t
        | (. % 10)
        | if . > 0 then ($t + " " + $small[.]) else $t end
    elif . < 1000 then
        ($small[./100|floor] + " hundred") as $h
        | (. % 100)
        | if . > 0 then $h + " and " + (say) else $h end
    else
        # Handle values larger than 1000 by considering
        # the rightmost three digits separately from the rest:
        ((. % 1000)
         | if . == 0 then ""
           elif . < 100 then "and " + say
           else say
           end ) as $sx
        | zillions(0; $sx)
    end ;

say
```

Transcript (input followed by output):

```jq
0
"zero"
-0
"zero"
111
"one hundred and eleven"
1230000
"one million, two hundred and thirty thousand"
123456
"one hundred and twenty three thousand, four hundred and fifty six"
123456789
"one hundred and twenty three million, four hundred and fifty six thousand, seven hundred and eighty nine"
-123000000123
"minus one hundred and twenty three billion, one hundred and twenty three"
12345678912345678
"twelve quadrillion, three hundred and forty five trillion, six hundred and seventy eight billion, nine hundred and twelve million, three hundred and forty five thousand, six hundred and seventy eight"

```



## Julia

The code for this solution became somewhat cumbersome as I worked to fine tune the output, so it might benefit from some hindsight refactoring.  By default the function <tt>num2text</tt> names integers according to the Anglo-American short-scale convention and is limited to numbers less than 10^66.  Optionally, integers can be named according to the Continental long-scale convention, in which case the limit is 10^126.  In either case, one can spell out any of Julia's fixed-bit format integers.  When these limits are exceeded the function returns "too big to say".  0 is treated as a special case, and negative integers are handled by pre-pending "minus" to the output of the corresponding positive integer.

'''Number Names Functions'''

```Julia
const stext = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
const teentext = ["eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen",
    "seventeen", "eighteen", "nineteen"]
const tenstext = ["ten", "twenty", "thirty", "forty", "fifty", "sixty", "seventy",
    "eighty", "ninety"]
const ordstext = ["million", "billion", "trillion", "quadrillion", "quintillion", "sextillion",
    "septillion", "octillion", "nonillion", "decillion", "undecillion", "duodecillion",
    "tredecillion", "quattuordecillion", "quindecillion", "sexdecillion", "septendecillion",
    "octodecillion", "novemdecillion", "vigintillion"]

function normalize_digits!(a::Array{T,1}) where T<:Integer
    while 0 < length(a) && a[end] == 0
        pop!(a)
    end
    length(a)
end

function digits2text!(d::Array{T,1}, use_short_scale=true) where T<:Integer
    ndig = normalize_digits!(d)
    0 < ndig || return ""
    if ndig < 7
        s = ""
        if 3 < ndig
            t = digits2text!(d[1:3])
            s = digits2text!(d[4:end]) * " thousand"
            0 < length(t) || return s
            return occursin(t, "and") ? s * " " * t : s * " and " * t
        end
        if ndig == 3
            s *= stext[pop!(d)] * " hundred"
            ndig = normalize_digits!(d)
            0 < ndig || return s
            s *= " and "
        end
        1 < ndig || return s*stext[pop!(d)]
        j, i = d
        j ≠ 0 || return s*tenstext[i]
        i ≠ 1 || return s*teentext[j]
        return s*tenstext[i] * "-" * stext[j]
    end
    s = digits2text!(d[1:6])
    d = d[7:end]
    dgrp = use_short_scale ? 3 : 6
    ord = 0
    while dgrp < length(d)
        ord += 1
        t = digits2text!(d[1:dgrp])
        d = d[(dgrp+1):end]
        0 < length(t) || continue
        t = t * " " * ordstext[ord]
        s = length(s) == 0 ? t : t * " " * s
    end
    ord += 1
    t = digits2text!(d) * " " * ordstext[ord]
    0 < length(s) || return t
    t * " " * s
end

function num2text(n::T, use_short_scale=true) where T<:Integer
    -1 < n || return "minus "*num2text(-n, use_short_scale)
    0 < n || return "zero"
    toobig = use_short_scale ? big(10)^66 : big(10)^126
    n < toobig || return "too big to say"
    digits2text!(digits(n, base=10), use_short_scale)
end
```


'''Main'''

```Julia
using Printf

println("Some easy ones to start with\n")

for i in [-1:21..., 100, 101, 10000, 10001, 1000000, 1010101]
    @printf("%8d is %s\n", i, num2text(i))
end

println("\nSome larger numbers\n")

println("The largest signed literal integer (short-scale)")
i = typemax(1)
println("    ", i, " is")
println(num2text(i))
println()

println("The largest signed literal integer (long-scale)")
println("    ", i, " is")
println(num2text(i, false))
println()

println("The largest unsigned integer (short-scale)")
i = typemax(UInt128)
println("    ", i, " is")
println(num2text(i))
println()

println("50! (short-scale)")
i = factorial(big(50))
println("    ", i, " is")
println(num2text(i))
println()

println("51! (short-scale)")
i = factorial(big(51))
println("    ", i, " is")
println(num2text(i))
println()

println("51! (long-scale)")
println("    ", i, " is")
println(num2text(i, false))

```


```txt

Testing num2text

Some easy ones to start with

      -1 is minus one
       0 is zero
       1 is one
       2 is two
       3 is three
       4 is four
       5 is five
       6 is six
       7 is seven
       8 is eight
       9 is nine
      10 is ten
      11 is eleven
      12 is twelve
      13 is thirteen
      14 is fourteen
      15 is fifteen
      16 is sixteen
      17 is seventeen
      18 is eighteen
      19 is nineteen
      20 is twenty
      21 is twenty-one
     100 is one hundred
     101 is one hundred and one
   10000 is ten thousand
   10001 is ten thousand and one
 1000000 is one million
 1010101 is one million ten thousand one hundred and one

Some larger numbers

The largest signed literal integer (short-scale)
    9223372036854775807 is
    nine quintillion two hundred and twenty-three quadrillion three hundred
    and seventy-two trillion thirty-six billion eight hundred and fifty-four
    million seven hundred and seventy-five thousand eight hundred and seven

The largest signed literal integer (long-scale)
    9223372036854775807 is
    nine trillion two hundred and twenty-three thousand three hundred and
    seventy-two billion thirty-six thousand eight hundred and fifty-four
    million seven hundred and seventy-five thousand eight hundred and seven

The largest unsigned integer (short-scale)
    340282366920938463463374607431768211455 is
    three hundred and forty undecillion two hundred and eighty-two decillion
    three hundred and sixty-six nonillion nine hundred and twenty octillion
    nine hundred and thirty-eight septillion four hundred and sixty-three
    sextillion four hundred and sixty-three quintillion three hundred and
    seventy-four quadrillion six hundred and seven trillion four hundred
    and thirty-one billion seven hundred and sixty-eight million two hundred
    and eleven thousand four hundred and fifty-five

50! (short-scale)
    30414093201713378043612608166064768844377641568960512000000000000 is
    thirty vigintillion four hundred and fourteen novemdecillion ninety-three
    octodecillion two hundred and one septendecillion seven hundred and
    thirteen sexdecillion three hundred and seventy-eight quindecillion
    forty-three quattuordecillion six hundred and twelve tredecillion six
    hundred and eight duodecillion one hundred and sixty-six undecillion
    sixty-four decillion seven hundred and sixty-eight nonillion eight
    hundred and forty-four octillion three hundred and seventy-seven septillion
    six hundred and forty-one sextillion five hundred and sixty-eight quintillion
    nine hundred and sixty quadrillion five hundred and twelve trillion

51! (short-scale)
    1551118753287382280224243016469303211063259720016986112000000000000 is
    too big to say

51! (long-scale)
    1551118753287382280224243016469303211063259720016986112000000000000 is
    one undecillion five hundred and fifty-one thousand one hundred and
    eighteen decillion seven hundred and fifty-three thousand two hundred
    and eighty-seven nonillion three hundred and eighty-two thousand two
    hundred and eighty octillion two hundred and twenty-four thousand two
    hundred and forty-three septillion sixteen thousand four hundred and
    sixty-nine sextillion three hundred and three thousand two hundred
    and eleven quintillion sixty-three thousand two hundred and fifty-nine
    quadrillion seven hundred and twenty thousand and sixteen trillion
    nine hundred and eighty-six thousand one hundred and twelve billion

```



## Kotlin

The following deals with positive, negative and zero integers within the range of Kotlin's Long type (8 byte integer).

There is an option to use the UK (rather than the US) method of spelling out numbers whereby 'and' is placed at strategic positions.

```scala
// version 1.1.2

val oneNames = listOf(
        "", "one", "two", "three", "four",
        "five", "six", "seven", "eight", "nine",
        "ten", "eleven", "twelve", "thirteen", "fourteen",
        "fifteen", "sixteen", "seventeen", "eighteen", "nineteen")
val tenNames = listOf(
        "", "", "twenty", "thirty", "forty",
        "fifty", "sixty", "seventy", "eighty", "ninety")
val thousandNames = listOf(
        "", "thousand", "million", "billion", "trillion", "quadrillion",
        "quintillion")

fun numToText(n: Long, uk: Boolean = false): String {
    if (n == 0L) return "zero"
    val neg = n < 0L
    val maxNeg = n == Long.MIN_VALUE
    var nn = if (maxNeg) -(n + 1) else if (neg) -n else n
    val digits3 = IntArray(7)
    for (i in 0..6) {  // split number into groups of 3 digits from the right
        digits3[i] = (nn % 1000).toInt()
        nn /= 1000
    }
    if (maxNeg) digits3[0]++

    fun threeDigitsToText(number: Int): String {
        val sb = StringBuilder()
        if (number == 0) return ""
        val hundreds = number / 100
        val remainder = number % 100
        if (hundreds > 0) {
            sb.append(oneNames[hundreds], " hundred")
            if (remainder > 0) sb.append(if (uk) " and " else " ")
        }
        if (remainder > 0) {
            val tens = remainder / 10
            val units = remainder % 10
            if (tens > 1) {
                sb.append(tenNames[tens])
                if (units > 0) sb.append("-", oneNames[units])
            } else sb.append(oneNames[remainder])
        }
        return sb.toString()
    }

    val triplets = Array(7) { threeDigitsToText(digits3[it]) }
    var text = triplets[0]
    var andNeeded = uk && digits3[0] in 1..99
    for (i in 1..6) {
        if (digits3[i] > 0) {
            var text2 = triplets[i] + " " + thousandNames[i]
            if (text != "") {
                text2 += if (andNeeded) " and " else ", "
                andNeeded = false
            } else andNeeded = uk && digits3[i] in 1..99
            text = text2 + text
        }
    }
    return (if (neg) "minus " else "") + text
}

fun main() {
    val exampleNumbers = longArrayOf(
            0, 1, 7, 10, 18, 22, 67, 99, 100, 105, 999, -1056, 1000005000,
            2074000000, 1234000000745003L, Long.MIN_VALUE
    )
    println("Using US representation:")
    for (i in exampleNumbers) println("${"%20d".format(i)} = ${numToText(i)}")
    println()
    println("Using UK representation:")
    for (i in exampleNumbers) println("${"%20d".format(i)} = ${numToText(i, true)}")
}
```


```txt

Using US representation:
                   0 = zero
                   1 = one
                   7 = seven
                  10 = ten
                  18 = eighteen
                  22 = twenty-two
                  67 = sixty-seven
                  99 = ninety-nine
                 100 = one hundred
                 105 = one hundred five
                 999 = nine hundred ninety-nine
               -1056 = minus one thousand, fifty-six
          1000005000 = one billion, five thousand
          2074000000 = two billion, seventy-four million
    1234000000745003 = one quadrillion, two hundred thirty-four trillion, seven hundred forty-five thousand, three
-9223372036854775808 = minus nine quintillion, two hundred twenty-three quadrillion, three hundred seventy-two trillion, thirty-six billion, eight hundred fifty-four million, seven hundred seventy-five thousand, eight hundred eight

Using UK representation:
                   0 = zero
                   1 = one
                   7 = seven
                  10 = ten
                  18 = eighteen
                  22 = twenty-two
                  67 = sixty-seven
                  99 = ninety-nine
                 100 = one hundred
                 105 = one hundred and five
                 999 = nine hundred and ninety-nine
               -1056 = minus one thousand and fifty-six
          1000005000 = one billion and five thousand
          2074000000 = two billion and seventy-four million
    1234000000745003 = one quadrillion, two hundred and thirty-four trillion, seven hundred and forty-five thousand and three
-9223372036854775808 = minus nine quintillion, two hundred and twenty-three quadrillion, three hundred and seventy-two trillion, thirty-six billion, eight hundred and fifty-four million, seven hundred and seventy-five thousand, eight hundred and eight

```



## Logo


```logo
make "numbers {one two three four five six seven eight nine ten
     eleven twelve thirteen fourteen fifteen sixteen seventeen eighteen nineteen}

make "tens {twenty thirty forty fifty sixty seventy eighty ninety}@2

make "thou [[] thousand million billion trillion]		; expand as desired

to to.english.thou :n :thou
  if :n = 0    [output []]
  if :n < 20   [output sentence item :n :numbers  first :thou]
  if :n < 100  [output (sentence item int :n/10 :tens
                                 to.english.thou modulo :n 10 [[]]
                                 first :thou)]
  if :n < 1000 [output (sentence item int :n/100 :numbers
                                 "hundred
                                 to.english.thou modulo :n 100 [[]]
                                 first :thou)]
  output (sentence to.english.thou int :n/1000 butfirst :thou
                   to.english.thou modulo :n 1000 :thou)
end

to to.english :n
  if :n = 0 [output "zero]
  if :n > 0 [output to.english.thou :n :thou]
  [output sentence "negative to.english.thou minus :n :thou]
end

print to.english 1234567   ; one million two hundred thirty four thousand five hundred sixty seven
```



## Lua



```lua
words = {"one ", "two ", "three ", "four ", "five ", "six ", "seven ", "eight ", "nine "}
levels = {"thousand ", "million ", "billion ", "trillion ", "quadrillion ", "quintillion ", "sextillion ", "septillion ", "octillion ", [0] = ""}
iwords = {"ten ", "twenty ", "thirty ", "forty ", "fifty ", "sixty ", "seventy ", "eighty ", "ninety "}
twords = {"eleven ", "twelve ", "thirteen ", "fourteen ", "fifteen ", "sixteen ", "seventeen ", "eighteen ", "nineteen "}

function digits(n)
  local i, ret = -1
  return function()
    i, ret = i + 1, n % 10
	if n > 0 then
      n = math.floor(n / 10)
	  return i, ret
	end
  end
end

level = false
function getname(pos, dig) --stateful, but effective.
  level = level or pos % 3 == 0
  if(dig == 0) then return "" end
  local name = (pos % 3 == 1 and iwords[dig] or words[dig]) .. (pos % 3 == 2 and "hundred " or "")
  if(level) then name, level = name .. levels[math.floor(pos / 3)], false end
  return name
end

local val, vword = io.read() + 0, ""

for i, v in digits(val) do
  vword = getname(i, v) .. vword
end

for i, v in ipairs(words) do
  vword = vword:gsub("ty " .. v, "ty-" .. v)
  vword = vword:gsub("ten " .. v, twords[i])
end

if #vword == 0 then print "zero" else print(vword) end
```



## Maple

<lang>number_name := n -> convert(n, english)
number_name(2001);
                       "two thousand one"
```



## Mathematica

<lang>small = "zero"["one", "two", "three", "four", "five", "six", "seven",
  "eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen",
  "fifteen", "sixteen", "seventeen", "eighteen",
  "nineteen"]; tens = # <> "-" & /@ {"twenty", "thirty", "forty",
   "fifty", "sixty", "seventy", "eighty", "ninety"};
big = Prepend[
   " " <> # & /@ {"thousand", "million", "billion", "trillion",
     "quadrillion", "quintillion", "sextillion", "septillion",
     "octillion", "nonillion", "decillion", "undecillion",
     "duodecillion", "tredecillion"}, ""];
name[n_Integer] := "negative " <> name[-n] /; n < 0;
name[n_Integer] := small[[n]] /; 0 <= n < 20;
name[n_Integer] :=
  StringTrim[tens[[#1 - 1]] <> small[[#2]] & @@ IntegerDigits[n],
    "-zero"] /; 10 <= n < 100;
name[n_Integer] :=
 StringTrim[
   small[[#1]] <> " hundred and " <> name@#2 & @@
    IntegerDigits[n, 100], " and zero"] /; 100 <= n < 1000;
name[n_Integer] :=
 StringJoin@
  Riffle[Select[
    MapThread[StringJoin, {name /@ #, Reverse@big[[;; Length@#]]}] &@
     IntegerDigits[n, 1000], StringFreeQ[#, "zero"] &], ","];
```



## Maxima


```Maxima

l: [99, 300, 310, 1501, 12609, 512609, 43112609, 77000112609, 2000000000100,
999999999999999999, 0, -99, -1501, -77000112609, -123456789987654321];
map( lambda([n], printf(true, "~20d ~r~%", n, n)), l)$

```



## MAXScript


```MAXScript

fn numToEng num =
(
	num = num as integer -- convert to int
	local originalNumber = num -- store the initial value, to check if it was negative afterwards

	num = abs num -- make positive
	local numStr = num as string -- store as string to check the length

	local nonFirstDigits = (if numStr.count > 3 then ((substring numStr ((if mod numStr.count 3 ==0 then 3 else mod numStr.count 3)+1) -1)) else "0") -- this is the string of the number without the beginning, i.e 123456 will give 456, 12035 will give 2035
	local singleDigits = #("One","Two","Three","Four","Five","Six","Seven","Eight","Nine")
	local ElevenTwenty = #("Eleven","Twelve","Thirteen","Fourteen","Fifteen","Sixteen","Seventeen","Eighteen","Nineteen")
	local tens = #("Ten","Twenty","Thirty","Forty","Fifty","Sixty","Seventy","Eighty","Ninety")
	local big = #("Hundred","Thousand","Million","Billion")
	local ret = "" -- this is the value to be returned

	case of
	(
		(num == 0 ): ret += "Zero" -- number is zero
		(num < 10): ret += singleDigits[num] -- number is not and smaller than 10
		(num == 10): ret += tens[1] -- number is 10
		(num < 20): ret += elevenTwenty[abs(10-num)] -- number is between 11 and 19
		(num <= 90 and mod num 10 == 0): ret += tens[num/10] -- number is >= 20 and <= 90 and is dividable by 10
		(num < 100): ret += (numToEng (floor(num/10.0)*10) +" "+ numtoEng (num-(floor(num/10.0))*10)) -- number is >= 20, < 100 and is not dividable by 10
		(num < 1000): ret += (singledigits[floor(num/100) as integer] + " "+big[1]+ (if mod num 100 != 0 then (" and "+numtoeng (num-(floor(num/100.0)*100))) else "")) -- number is >= 100, < 1000
		(num >= 1000): ret += -- number is >= 1000
			(
				numtoeng (substring numStr 1 (if mod numStr.count 3 ==0 then 3 else mod numStr.count 3)) + \
				" " + big[1+((numStr.count-1)/3)] + (if nonFirstDigits as integer == 0 then "" else (if nonFirstDigits as integer < 100 then " and " else ", ")) + \
				(if (mod num 1000 == 0) then "" else (numtoeng nonFirstDigits))

			)
	)

	if originalNumber < 0 and (substring ret 1 8) != "Negative" do ret = ("Negative "+ret) -- if number is negative
	ret = (toupper ret[1]) + (tolower (substring ret 2 -1)) -- make the first char uppercase and rest lowercase
	return ret
)
```


Examples:

```MAXScript

numtoeng 0
"Zero"
numtoeng 50
"Fifty"
numtoeng 123
"One hundred and twenty three"
numtoeng -4235
"Negative four thousand, two hundred and thirty five"
numtoeng 98273
"Ninety eight thousand, two hundred and seventy three"
numtoeng -92836152
"Negative ninety two million, eight hundred and thirty six thousand, one hundred and fifty two"
numtoeng 421752302
"Four hundred and twenty one million, seven hundred and fifty two thousand, three hundred and two"


```



## MiniScript


```MiniScript
singles = " one two three four five six seven eight nine ".split
teens = "ten eleven twelve thirteen fourteen fifteen sixteen seventeen eighteen nineteen ".split
tys = "  twenty thirty forty fifty sixty seventy eighty ninety".split
ions = "thousand million billion".split

numberName = function(n)
    if n == 0 then return "zero"
    a = abs(n)
    r = ""  // (result)
    for u in ions
        h = a % 100
        if h > 0 and h < 10 then r = singles[h] + " " + r
        if h > 9 and h < 20 then r = teens[h-10] + " " + r
        if h > 19 and h < 100 then r = tys[h/10] + "-"*(h%10>0) + singles[h%10] + " " + r
        h = floor((a % 1000) / 100)
        if h then r = singles[h] + " hundred " + r
        a = floor(a / 1000)
        if a == 0 then break
        if a % 1000 > 0 then r = u + " " + r
    end for
    if n < 0 then r = "negative " + r
    return r
end function

// Test cases:
for n in [-1234, 0, 7, 42, 4325, 1000004, 214837564]
    print n + ": " + numberName(n)
end for
```


Output:

```txt
-1234: negative one thousand two hundred thirty-four
0: zero
7: seven
42: forty-two
4325: four thousand three hundred twenty-five
1000004: one million four
214837564: two hundred fourteen million eight hundred thirty-seven thousand five hundred sixty-four
```



## Nim

```nim
import strutils, algorithm

const
  tens = ["", "", "twenty", "thirty", "forty", "fifty", "sixty", "seventy",
          "eighty", "ninety"]
  small = ["zero", "one", "two", "three", "four", "five", "six", "seven",
           "eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen",
           "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]
  huge = ["", "", "million", "billion", "trillion", "quadrillion",
          "quintillion", "sextillion", "septillion", "octillion", "nonillion",
          "decillion"]

proc spellInteger(n: int64): string

proc nonzero(c: string, n: int, connect=""): string =
  if n == 0: "" else: connect & c & spellInteger(n)

proc lastAnd(num): string =
  var num = num
  if "," in num:
    let pos =  num.rfind(",")
    var (pre, last) =
      if pos >= 0: (num[0 .. pos-1], num[pos+1 .. num.high])
      else: ("", num)
    if " and " notin last:
      last = " and" & last
    num = [pre, ",", last].join()
  return num

proc big(e, n): string =
  if e == 0:
    spellInteger(n)
  elif e == 1:
    spellInteger(n) & " thousand"
  else:
    spellInteger(n) & " " & huge[e]

iterator base1000Rev(n): int =
  var n = n
  while n != 0:
    let r = n mod 1000
    n = n div 1000
    yield r

proc spellInteger(n: int64): string =
  if n < 0:
    "minus " & spellInteger(-n)
  elif n < 20:
    small[int(n)]
  elif n < 100:
    let a = n div 10
    let b = n mod 10
    tens[int(a)] & nonzero("-", b)
  elif n < 1000:
    let a = n div 100
    let b = n mod 100
    small[int(a)] & " hundred" & nonzero(" ", b, " and")
  else:
    var sq = newSeq[string]()
    var e = 0
    for x in base1000Rev(n):
      if x > 0:
        sq.add big(e, x)
      inc e
    reverse sq
    lastAnd(sq.join(", "))

for n in [0, -3, 5, -7, 11, -13, 17, -19, 23, -29]:
  echo align($n, 4)," -> ",spellInteger(n)

var n = 201021002001
while n != 0:
  echo align($n, 14)," -> ",spellInteger(n)
  n = n div -10
```

Output:

```txt
   0 -> zero
  -3 -> minus three
   5 -> five
  -7 -> minus seven
  11 -> eleven
 -13 -> minus thirteen
  17 -> seventeen
 -19 -> minus nineteen
  23 -> twenty-three
 -29 -> minus twenty-nine
  201021002001 -> two hundred and one billion, twenty-one million, two thousand, and one
  -20102100200 -> minus twenty billion, one hundred and two million, one hundred thousand, and two hundred
    2010210020 -> two billion, ten million, two hundred and ten thousand, and twenty
    -201021002 -> minus two hundred and one million, twenty-one thousand, and two
      20102100 -> twenty million, one hundred and two thousand, and one hundred
      -2010210 -> minus two million, ten thousand, two hundred and ten
        201021 -> two hundred and one thousand, and twenty-one
        -20102 -> minus twenty thousand, one hundred and two
          2010 -> two thousand, and ten
          -201 -> minus two hundred and one
            20 -> twenty
            -2 -> minus two
```



## Objeck

```objeck

class NumberNames {
  small : static : String[];
  tens : static : String[];
  big : static : String[];

  function : Main(args : String[]) ~ Nil {
    small := ["one", "two", "three", "four", "five", "six", "seven",
      "eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen",
      "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"];
    tens := ["twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"];
    big := ["thousand", "million", "billion", "trillion"];

    Int2Text(900000001)->PrintLine();
    Int2Text(1234567890)->PrintLine();
    Int2Text(-987654321)->PrintLine();
    Int2Text(0)->PrintLine();
  }

  function : native : Int2Text(number : Int) ~ String {
    num := 0;
    outP := "";
          unit := 0;
          tmpLng1 := 0;

    if (number = 0) {
      return "zero";
    };

    num := number->Abs();

    while(true) {
      tmpLng1 := num % 100;
      if (tmpLng1 >= 1 & tmpLng1 <= 19) {
        tmp := String->New();
        tmp->Append(small[tmpLng1 - 1]);
        tmp->Append(" ");
        tmp->Append(outP);
        outP := tmp;
      }
      else if (tmpLng1 >= 20 & tmpLng1 <= 99) {
        if (tmpLng1 % 10 = 0) {
          tmp := String->New();
          tmp->Append(tens[(tmpLng1 / 10) - 2]);
          tmp->Append(" ");
          tmp->Append(outP);
          outP := tmp;
        }
        else {
          tmp := String->New();
          tmp->Append(tens[(tmpLng1 / 10) - 2]);
          tmp->Append( "-");
          tmp->Append(small[(tmpLng1 % 10) - 1]);
          tmp->Append(" ");
          tmp->Append(outP);
          outP := tmp;
        };
      };

      tmpLng1 := (num % 1000) / 100;
      if (tmpLng1 <> 0) {
        tmp := String->New();
        tmp->Append(small[tmpLng1 - 1]);
        tmp->Append(" hundred ");
        tmp->Append(outP);
        outP := tmp;
      };

      num /= 1000;
      if (num = 0) {
        break;
      };

      tmpLng1 := num % 1000;
      if (tmpLng1 <> 0) {
        tmp := String->New();
        tmp->Append(big[unit]);
        tmp->Append(" ");
        tmp->Append(outP);
        outP := tmp;
      };
      unit+=1;
    };

    if (number < 0) {
      tmp := String->New();
      tmp->Append("negative ");
      tmp->Append(outP);
      outP := tmp;
    };

    return outP->Trim();
  }
}

```


output:

```txt

nine hundred million one
one billion two hundred thirty-four million five hundred sixty-seven thousand eight hundred ninety
negative nine hundred eighty-seven million six hundred fifty-four thousand three hundred twenty-one
zero

```


=={{header|Objective-C}}==

```objc>#import <Foundation/Foundation.h


int main() {
  @autoreleasepool {

    NSNumberFormatter *numberFormatter = [[NSNumberFormatter alloc] init];
    numberFormatter.numberStyle = NSNumberFormatterSpellOutStyle;
    numberFormatter.locale = [[NSLocale alloc] initWithLocaleIdentifier:@"en_US"];

    for (NSNumber *n in @[@900000001, @1234567890, @-987654321, @0, @3.14]) {
      NSLog(@"%@", [numberFormatter stringFromNumber:n]);
    }

  }
  return 0;
}
```

Output:

```txt

nine hundred million one
one billion two hundred thirty-four million five hundred sixty-seven thousand eight hundred ninety
minus nine hundred eighty-seven million six hundred fifty-four thousand three hundred twenty-one
zero
three point one four

```



## OCaml



```ocaml
let div_mod n d = (n / d, n mod d)
let join = String.concat ", " ;;

let rec nonzero = function
  | _, 0 -> ""
  | c, n -> c ^ (spell_integer n)

and tens n =
  [| ""; ""; "twenty"; "thirty"; "forty"; "fifty";
             "sixty"; "seventy"; "eighty"; "ninety" |].(n)

and small n =
  [| "zero"; "one"; "two"; "three"; "four"; "five";
     "six"; "seven"; "eight"; "nine"; "ten"; "eleven";
     "twelve"; "thirteen"; "fourteen"; "fifteen";
     "sixteen";"seventeen"; "eighteen"; "nineteen" |].(n)

and bl = [| ""; ""; "m"; "b"; "tr"; "quadr"; "quint";
                    "sext"; "sept"; "oct"; "non"; "dec" |]

and big = function
  | 0, n -> (spell_integer n)
  | 1, n -> (spell_integer n) ^ " thousand"
  | e, n -> (spell_integer n) ^ " " ^ bl.(e) ^ "illion"

and uff acc = function
  | 0 -> List.rev acc
  | n ->
      let a, b = div_mod n 1000 in
      uff (b::acc) a

and spell_integer = function
  | n when n < 0 -> invalid_arg "spell_integer: negative input"
  | n when n < 20 -> small n
  | n when n < 100 ->
      let a, b = div_mod n 10 in
      (tens a) ^ nonzero("-", b)
  | n when n < 1000 ->
      let a, b = div_mod n 100 in
      (small a) ^ " hundred" ^ nonzero(" ", b)
  | n ->
      let seg = (uff [] n) in
      let _, segn =
        (* just add the index of the item in the list *)
        List.fold_left
          (fun (i,acc) v -> (succ i, (i,v)::acc))
          (0,[])
          seg
      in
      let fsegn =
        (* remove right part "zero" *)
        List.filter
          (function (_,0) -> false | _ -> true)
          segn
      in
      join(List.map big fsegn)
;;
```



## PARI/GP


```parigp
Eng(n:int)={
	my(tmp,s="");
	if (n >= 1000000,
		tmp = n\1000000;
		s = Str(s, Eng(tmp), " million");
		n -= tmp * 1000000;
		if (!n, return(s));
		s = Str(s, " ")
	);
	if (n >= 1000,
		tmp = n\1000;
		s = Str(s, Eng(tmp), " thousand");
		n -= tmp * 1000;
		if (!n, return(s));
		s = Str(s, " ")
	);
	if (n >= 100,
		tmp = n\100;
		s = Str(s, Edigit(tmp), " hundred");
		n -= tmp * 100;
		if (!n, return(s));
		s = Str(s, " ")
	);
	if (n < 20,
		return (Str(s, ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "ninteen"][n]))
	);
	tmp = n\10;
	s = Str(s, [0, "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"][tmp]);
	n -= tmp * 10;
	if (n, Str(s, "-", Edigit(n)), s)
};
Edigit(n)={
	["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"][n]
};
```



## Pascal

```pascal
program NumberNames(output);

const
  smallies: array[1..19] of string =
              ('one', 'two', 'three', 'four', 'five', 'six',
               'seven', 'eight', 'nine', 'ten', 'eleven',
      	       'twelve', 'thirteen', 'fourteen', 'fifteen',
	       'sixteen', 'seventeen', 'eighteen', 'nineteen');
  tens: array[2..9] of string =
          ('twenty', 'thirty', 'forty', 'fifty',
           'sixty', 'seventy', 'eighty', 'ninety');

function domaxies(number: int64): string;
  const
    maxies: array[0..5] of string =
              (' thousand', ' million', ' billion',
               ' trillion', ' quadrillion', ' quintillion');
  begin
    domaxies := '';
    if number >= 0 then
      domaxies := maxies[number];
  end;

function doHundreds( number: int64): string;
  begin
    doHundreds := '';
    if number > 99 then
    begin
      doHundreds := smallies[number div 100];
      doHundreds := doHundreds + ' hundred';
      number := number mod 100;
      if number > 0 then
        doHundreds := doHundreds + ' and ';
    end;
    if number >= 20 then
    begin
      doHundreds := doHundreds + tens[number div 10];
      number := number mod 10;
      if number > 0 then
        doHundreds := doHundreds + '-';
    end;
    if (0 < number) and (number < 20) then
      doHundreds := doHundreds + smallies[number];
  end;

function spell(number: int64): string;
  var
    scaleFactor: int64 = 1000000000000000000;
    maxieStart, h: int64;
  begin
    spell := '';
    maxieStart := 5;
    if number < 20 then
      spell := smallies[number];
    while scaleFactor > 0 do
    begin
      if number > scaleFactor then
      begin
	h := number div scaleFactor;
	spell := spell + doHundreds(h) + domaxies(maxieStart);
	number := number mod scaleFactor;
	if number > 0 then
	  spell := spell + ', ';
      end;
      scaleFactor := scaleFactor div 1000;
      dec(maxieStart);
    end;
  end;

begin
  writeln(99, ': ', spell(99));
  writeln(234, ': ', spell(234));
  writeln(7342, ': ', spell(7342));
  writeln(32784, ': ', spell(32784));
  writeln(234345, ': ', spell(234345));
  writeln(2343451, ': ', spell(2343451));
  writeln(23434534, ': ', spell(23434534));
  writeln(234345456, ': ', spell(234345456));
  writeln(2343454569, ': ', spell(2343454569));
  writeln(2343454564356, ': ', spell(2343454564356));
  writeln(2345286538456328, ': ', spell(2345286538456328));
end.
```

Output:

99: ninety-nine

234: two hundred and thirty-four

7342: seven thousand, three hundred and forty-two

32784: thirty-two thousand, seven hundred and eighty-four

234345: two hundred and thirty-four thousand, three hundred and forty-five

2343451: two million, three hundred and forty-three thousand, four hundred and fifty-one

23434534: twenty-three million, four hundred and thirty-four thousand, five hundred and thirty-four

234345456: two hundred and thirty-four million, three hundred and forty-five thousand, four hundred and fifty-six

2343454569: two billion, three hundred and forty-three million, four hundred and fifty-four thousand, five hundred and sixty-nine

23434545643565: twenty-three trillion, four hundred and thirty-four billion, five hundred and forty-five million, six hundred and forty-three thousand, five hundred and sixty-five

2345286538456328: two quadrillion, three hundred and forty-five trillion, two hundred and eighty-six billion, five hundred and thirty-eight million, four hundred and fifty-six thousand, three
hundred and twenty-eight


## Perl


```perl
use Lingua::EN::Numbers 'num2en';

print num2en(123456789), "\n";
```



## Perl 6

Apart from the <tt>$m++</tt> this can be viewed as a purely functional program; we use nested <tt>gather</tt>/<tt>take</tt> constructs to avoid accumulators.

```perl6
constant @I = <zero one    two    three    four     five    six     seven     eight    nine
               ten  eleven twelve thirteen fourteen fifteen sixteen seventeen eighteen nineteen>;
constant @X = <0    X      twenty thirty   forty    fifty   sixty   seventy   eighty   ninety>;
constant @C = @I X~ ' hundred';
constant @M = (<0 thousand>,
    ((<m b tr quadr quint sext sept oct non>,
    (map { ('', <un duo tre quattuor quin sex septen octo novem>).flat X~ $_ },
    <dec vigint trigint quadragint quinquagint sexagint septuagint octogint nonagint>),
    'cent').flat X~ 'illion')).flat;

sub int-name ($num) {
    if $num.substr(0,1) eq '-' { return "negative {int-name($num.substr(1))}" }
    if $num eq '0' { return @I[0] }
    my $m = 0;
    return join ', ', reverse gather for $num.flip.comb(/\d ** 1..3/) {
        my ($i,$x,$c) = .comb».Int;
        if $i or $x or $c {
            take join ' ', gather {
                if $c { take @C[$c] }
                if $x and $x == 1 { take @I[$i+10] }
                else {
                    if $x { take @X[$x] }
                    if $i { take @I[$i] }
                }
                take @M[$m] // die "WOW! ZILLIONS!\n" if $m;
            }
        }
        $m++;
    }
}

while '' ne (my $n = prompt("Number: ")) {
    say int-name($n);
}
```

Output:

```txt
Number: 0
zero
Number: 17
seventeen
Number: -1,234,567,890
negative one billion, two hundred thirty four million, five hundred sixty seven thousand, eight hundred ninety
Number: 42 000
forty two thousand
Number: 1001001001001001001001001001001001001001001001001001001001001001001001001001001001001001001001001001001001001001001001001001001001001001001001001001001001001001001001001001001001001001001001001001001001001001001001001001001001001001001001001
one novemseptuagintillion, one octoseptuagintillion, one septenseptuagintillion, one sexseptuagintillion, one quinseptuagintillion, one quattuorseptuagintillion, one treseptuagintillion, one duoseptuagintillion, one unseptuagintillion, one septuagintillion, one novemsexagintillion, one octosexagintillion, one septensexagintillion, one sexsexagintillion, one quinsexagintillion, one quattuorsexagintillion, one tresexagintillion, one duosexagintillion, one unsexagintillion, one sexagintillion, one novemquinquagintillion, one octoquinquagintillion, one septenquinquagintillion, one sexquinquagintillion, one quinquinquagintillion, one quattuorquinquagintillion, one trequinquagintillion, one duoquinquagintillion, one unquinquagintillion, one quinquagintillion, one novemquadragintillion, one octoquadragintillion, one septenquadragintillion, one sexquadragintillion, one quinquadragintillion, one quattuorquadragintillion, one trequadragintillion, one duoquadragintillion, one unquadragintillion, one quadragintillion, one novemtrigintillion, one octotrigintillion, one septentrigintillion, one sextrigintillion, one quintrigintillion, one quattuortrigintillion, one tretrigintillion, one duotrigintillion, one untrigintillion, one trigintillion, one novemvigintillion, one octovigintillion, one septenvigintillion, one sexvigintillion, one quinvigintillion, one quattuorvigintillion, one trevigintillion, one duovigintillion, one unvigintillion, one vigintillion, one novemdecillion, one octodecillion, one septendecillion, one sexdecillion, one quindecillion, one quattuordecillion, one tredecillion, one duodecillion, one undecillion, one decillion, one nonillion, one octillion, one septillion, one sextillion, one quintillion, one quadrillion, one trillion, one billion, one million, one thousand, one
Number: 198723483017417
one hundred ninety eight trillion, seven hundred twenty three billion, four hundred eighty three million, seventeen thousand, four hundred seventeen
```


Alternately, we could use the Lingua::EN::Numbers module from the Perl 6 ecosystem. It will return similar output for similar inputs as above, but also handles fractions with configurable reduction and denominator, exponential notation, and ordinal notation.


```perl6
use Lingua::EN::Numbers; # Version 2.4.0 or higher

put join "\n", .&cardinal, .&cardinal(:improper) with -7/4;

printf "%-7s : %19s : %s\n", $_, cardinal($_), cardinal($_, :denominator(16)) for 1/16, 2/16 ... 1;

put join "\n", .&cardinal, .&cardinal-year, .&ordinal, .&ordinal-digit with 1999;

.&cardinal.put for 6.022e23, 42000, π;
```



```txt
negative one and three quarters
negative seven quarters
0.0625  :       one sixteenth : one sixteenth
0.125   :          one eighth : two sixteenths
0.1875  :    three sixteenths : three sixteenths
0.25    :         one quarter : four sixteenths
0.3125  :     five sixteenths : five sixteenths
0.375   :       three eighths : six sixteenths
0.4375  :    seven sixteenths : seven sixteenths
0.5     :            one half : eight sixteenths
0.5625  :     nine sixteenths : nine sixteenths
0.625   :        five eighths : ten sixteenths
0.6875  :   eleven sixteenths : eleven sixteenths
0.75    :      three quarters : twelve sixteenths
0.8125  : thirteen sixteenths : thirteen sixteenths
0.875   :       seven eighths : fourteen sixteenths
0.9375  :  fifteen sixteenths : fifteen sixteenths
1       :                 one : one
one thousand, nine hundred ninety-nine
nineteen ninety-nine
one thousand, nine hundred ninety-ninth
1999th
six point zero two two times ten to the twenty-third
forty-two thousand
three point one four one five nine two six five three five eight nine seven nine
```



## Phix

Fraction ideas copied from [[Number_names#HicEst|HicEst]], using billion=10^9, trillion=10^12, quadrillion=10^15, and limited to 999 quadrillion.

Implemented as an [[Executable_library]] for use in [[Names_to_numbers#Phix|Names_to_numbers]].

```Phix
--
-- demo/rosetta/Number_names.exw
--
constant twenties = {"zero","one","two","three","four","five","six","seven","eight","nine","ten",
    "eleven","twelve","thirteen","fourteen","fifteen","sixteen","seventeen","eighteen","nineteen"}

function Twenty(integer N)
    return twenties[mod(N,20)+1]
end function

constant decades = {"twenty","thirty","forty","fifty","sixty","seventy","eighty","ninety"}

function Decade(integer N)
    return decades[mod(N,10)-1]
end function

function Hundred(integer N)
    if N<20 then
        return Twenty(N)
    elsif mod(N,10)=0 then
        return Decade(mod(floor(N/10),10))
    end if
    return Decade(floor(N/10)) & '-' & Twenty(mod(N,10))
end function

function Thousand(integer N, string withand)
    if N<100 then
        return withand & Hundred (N);
    elsif mod(N,100)=0 then
        return withand & Twenty(floor(N/100))&" hundred"
    end if
    return Twenty(floor(N/100)) & " hundred and " & Hundred(mod(N,100))
end function

constant orders = {{power(10,15),"quadrillion"},
                   {power(10,12),"trillion"},
                   {power(10,9),"billion"},
                   {power(10,6),"million"},
                   {power(10,3),"thousand"}}

function Triplet(atom N)
atom Order, High, Low
string Name, res = ""
integer n
    for i=1 to length(orders) do
        {Order,Name} = orders[i]
        High = floor(N/Order)
        Low = mod(N,Order)
        if High!=0 then
            res &= Thousand(High,"")&' '&Name
        end if
        N = Low
        if Low=0 then exit end if
        if length(res) and High!=0 then
            res &= ", "
        end if
    end for
    if N!=0 or res="" then
        res &= Thousand(floor(N),iff(res=""?"":"and "))
        N = abs(mod(N,1))
        if N>1e-6 then
            res &= " point"
            for i=1 to 10 do
                n = floor(N*10.0000001)
                res &= ' '&twenties[n+1]
                N = N*10-n
                if abs(N)<1e-6 then exit end if
            end for
        end if
    end if
    return res
end function

global function spell(atom N)
string res = ""
    if N<0 then
        res = "minus "
        N = -N
    end if
    res &= Triplet(N)
    return res
end function

global
constant Samples = {99, 300, 310, 417,1_501, 12_609, 200000000000100, 999999999999999,
                    -123456787654321,102003000400005,1020030004,102003,102,1,0,-1,-99,
                    -1501,1234,12.34,10000001.2,1E-3,-2.7182818,
                    201021002001,-20102100200,2010210020,-201021002,20102100,-2010210,
                    201021,-20102,2010,-201,20,-2}

global function smartp(atom N)
string res
    if N=floor(N) then return sprintf("%d",N) end if
    res = sprintf("%18.8f",N)
    if find('.',res) then
        res = trim_tail(res,"0")
    end if
    return res
end function

procedure main()
atom si
    for i=1 to length(Samples) do
        si = Samples[i]
        printf(1,"%18s %s\n",{smartp(si),spell(si)})
    end for
end procedure

-- from Executable_library#Phix --
function isMainOrInclude()
-- returns 1 if called from the main file, 0 if from an include
integer res
    #ilASM{
        [32]
            mov eax,[ebp+20]    -- prev_ebp
            mov eax,[eax+8]     -- rtn
            mov [res],eax
        [64]
            mov rax,[rbp+40]    -- prev_ebp
            mov rax,[rax+16]    -- rtn
            mov [res],rax
        []
          }
    return res=21 -- (21=T_maintls)
end function

if isMainOrInclude() then
    main()
end if
```

<pre style="font-size: 8px">
                99 ninety-nine
               300 three hundred
               310 three hundred and ten
               417 four hundred and seventeen
              1501 one thousand, five hundred and one
             12609 twelve thousand, six hundred and nine
   200000000000100 two hundred trillion, and one hundred
   999999999999999 nine hundred and ninety-nine trillion, nine hundred and ninety-nine billion, nine hundred and ninety-nine million, nine hundred and ninety-nine thousand, nine hundred and ninety-nine
  -123456787654321 minus one hundred and twenty-three trillion, four hundred and fifty-six billion, seven hundred and eighty-seven million, six hundred and fifty-four thousand, three hundred and twenty-one
   102003000400005 one hundred and two trillion, three billion, four hundred thousand, and five
        1020030004 one billion, twenty million, thirty thousand, and four
            102003 one hundred and two thousand, and three
               102 one hundred and two
                 1 one
                 0 zero
                -1 minus one
               -99 minus ninety-nine
             -1501 minus one thousand, five hundred and one
              1234 one thousand, two hundred and thirty-four
             12.34 twelve point three four
        10000001.2 ten million, and one point two
             0.001 zero point zero zero one
        -2.7182818 minus two point seven one eight two eight one eight
      201021002001 two hundred and one billion, twenty-one million, two thousand, and one
      -20102100200 minus twenty billion, one hundred and two million, one hundred thousand, and two hundred
        2010210020 two billion, ten million, two hundred and ten thousand, and twenty
        -201021002 minus two hundred and one million, twenty-one thousand, and two
          20102100 twenty million, one hundred and two thousand, and one hundred
          -2010210 minus two million, ten thousand, two hundred and ten
            201021 two hundred and one thousand, and twenty-one
            -20102 minus twenty thousand, one hundred and two
              2010 two thousand, and ten
              -201 minus two hundred and one
                20 twenty
                -2 minus two

```



## PHP


```php
$orderOfMag = array('Hundred', 'Thousand,', 'Million,', 'Billion,', 'Trillion,');
$smallNumbers = array('Zero', 'One', 'Two', 'Three', 'Four', 'Five', 'Six', 'Seven', 'Eight', 'Nine',
'Ten', 'Eleven', 'Twelve', 'Thirteen', 'Fourteen', 'Fifteen', 'Sixteen', 'Seventeen', 'Eighteen', 'Nineteen');
$decades = array('', '', 'Twenty', 'Thirty', 'Forty', 'Fifty', 'Sixty', 'Seventy', 'Eighty', 'Ninety');

function NumberToEnglish($num, $count = 0){
   global $orderOfMag, $smallNumbers, $decades;
   $isLast = true;
   $str = '';

   if ($num < 0){
      $str = 'Negative ';
      $num = abs($num);
   }

   (int) $thisPart = substr((string) $num, -3);

   if (strlen((string) $num) > 3){
      // Number still too big, work on a smaller chunk
      $str .= NumberToEnglish((int) substr((string) $num, 0, strlen((string) $num) - 3), $count + 1);
      $isLast = false;
   }

   // do translation stuff
   if (($count == 0 || $isLast) && ($str == '' || $str == 'Negative '))
      // This is either a very small number or the most significant digits of the number. Either way we don't want a preceeding "and"
      $and = '';
   else
      $and = ' and ';

   if ($thisPart > 99){
      // Hundreds part of the number chunk
      $str .= ($isLast ? '' : ' ') . "{$smallNumbers[$thisPart/100]} {$orderOfMag[0]}";

      if(($thisPart %= 100) == 0){
         // There is nothing else to do for this chunk (was a multiple of 100)
         $str .= " {$orderOfMag[$count]}";
         return $str;
      }
      $and = ' and ';  // Set up our and string to the word "and" since there is something in the hundreds place of this chunk
   }

   if ($thisPart >= 20){
      // Tens part of the number chunk
      $str .= "{$and}{$decades[$thisPart /10]}";
      $and = ' '; // Make sure we don't have any extranious "and"s
      if(($thisPart %= 10) == 0)
         return $str . ($count != 0 ? " {$orderOfMag[$count]}" : '');
   }

   if ($thisPart < 20 && $thisPart > 0)
      // Ones part of the number chunk
      return $str . "{$and}{$smallNumbers[(int) $thisPart]} " . ($count != 0 ? $orderOfMag[$count] : '');
   elseif ($thisPart == 0 && strlen($thisPart) == 1)
      // The number is zero
      return $str . "{$smallNumbers[(int)$thisPart]}";
}
```

Example:

```php
NumberToEnglish(0);
NumberToEnglish(12);
NumberToEnglish(123);
NumberToEnglish(1234567890123);
NumberToEnglish(65535);
NumberToEnglish(-54321);
```

Returns:

```txt
Zero
Twelve
One Hundred and Twenty Three
One Trillion, Two Hundred and Thirty Four Billion, Five Hundred and Sixty Seven Million, Eight Hundred and Ninety Thousand, One Hundred and Twenty Three
Sixty Five Thousand, Five Hundred and Thirty Five
Negative Fifty Four Thousand, Three Hundred and Twenty One
```



## PicoLisp


```PicoLisp
(de numName (N)
   (cond
      ((=0 N) "zero")
      ((lt0 N) (pack "minus " (numName (- N))))
      (T (numNm N)) ) )

(de numNm (N)
   (cond
      ((=0 N))
      ((> 14 N)
         (get '("one" "two" "three" "four" "five" "six" "seven" "eight" "nine" "ten" "eleven" "twelve" "thirteen") N) )
      ((= 15 N) "fifteen")
      ((= 18 N) "eighteen")
      ((> 20 N) (pack (numNm (% N 10)) "teen"))
      ((> 100 N)
         (pack
            (get '("twen" "thir" "for" "fif" "six" "seven" "eigh" "nine") (dec (/ N 10)))
            "ty"
            (unless (=0 (% N 10))
               (pack "-" (numNm (% N 10))) ) ) )
      ((rank N '((100 . "hundred") (1000 . "thousand") (1000000 . "million")))
         (pack (numNm (/ N (car @))) " " (cdr @) " " (numNm (% N (car @)))) ) ) )
```



## PL/I


```PL/I
   declare integer_names (0:20) character (9) varying static initial
      ('zero',  'one',   'two',  'three', 'four',   'five', 'six',
       'seven', 'eight', 'nine', 'ten',   'eleven', 'twelve',
       'thirteen', 'fourteen', 'fifteen', 'sixteen', 'seventeen',
       'eighteen', 'nineteen', 'twenty' );
   declare x(10) character (7) varying static initial
      ('ten', 'twenty', 'thirty', 'fourty', 'fifty',
       'sixty', 'seventy', 'eighty', 'ninety', 'hundred');
   declare y(0:5) character (10) varying static initial
      ('', '', ' thousand ', ' million ', ' billion ', ' trillion ');
   declare (i, j, m, t) fixed binary (31);
   declare (units, tens, hundreds, thousands) fixed binary (7);
   declare (h, v, value) character (200) varying;
   declare (d, k, n) fixed decimal (15);
   declare three_digits fixed decimal (3);

      value = '';
      i = 5;
      k = n;
      do d = 1000000000000 repeat d/1000 while (d > 0);
         i = i - 1;
         three_digits = k/d;
         k = mod(k, d);
         if three_digits = 0 then iterate;

         units = mod(three_digits, 10);
         t = three_digits / 10;
         tens = mod(t, 10);
         hundreds = three_digits / 100;
         m = mod(three_digits, 100);
         if m <= 20 then
            v = integer_names(m);
         else if units = 0 then
            v = '';
         else
            v = integer_names(units);
         if tens >= 2 & units ^= 0 then
            v = x(tens) || v;
         else if tens > 2 & units = 0 then
            v = v || x(tens);

         if units + tens = 0 then
            if n > 0 then  v = '';
         if hundreds > 0 then
            h = integer_names(hundreds) || ' hundred ';
         else
            h = '';
         if three_digits > 100 & (tens + units > 0) then
            v = 'and ' || v;
         if i = 1 & value ^= '' & three_digits <= 9 then
            v = 'and ' || v;
         value = value ||h || v || y(i);
      end;
      put skip edit (trim(N), ' = ', value) (a);
```



## PowerBASIC

Note that the PB compiler has some limitations related to how the <CODE>QUAD</CODE> data type is handled behind the scenes (extremely large values lose precision; see the sample output below the code).


```powerbasic
FUNCTION int2Text (number AS QUAD) AS STRING
    IF 0 = number THEN
        FUNCTION = "zero"
        EXIT FUNCTION
    END IF

    DIM num AS QUAD, outP AS STRING, unit AS LONG
    DIM tmpLng1 AS QUAD

    DIM small(1 TO 19) AS STRING, tens(7) AS STRING, big(5) AS STRING

    DIM tmpInt AS LONG, dcnt AS LONG

    ARRAY ASSIGN small() = "one", "two", "three", "four", "five", "six", _
                           "seven", "eight", "nine", "ten", "eleven", _
                           "twelve", "thirteen", "fourteen", "fifteen", _
                           "sixteen", "seventeen", "eighteen", "nineteen"
    ARRAY ASSIGN tens() = "twenty", "thirty", "forty", "fifty", "sixty", _
                          "seventy", "eighty", "ninety"
    ARRAY ASSIGN big() = "thousand", "million", "billion", "trillion", _
                         "quadrillion", "quintillion"

    num = ABS(number)

    DO
        tmpLng1 = num MOD 100
        SELECT CASE tmpLng1
            CASE 1 TO 19
                outP = small(tmpLng1) + " " + outP
            CASE 20 TO 99
                SELECT CASE tmpLng1 MOD 10
                    CASE 0
                        outP = tens((tmpLng1 \ 10) - 2) + " " + outP
                    CASE ELSE
                        outP = tens((tmpLng1 \ 10) - 2) + "-" + small(tmpLng1 MOD 10) + " " + outP
                END SELECT
        END SELECT

        tmpLng1 = (num MOD 1000) \ 100
        IF tmpLng1 THEN
            outP = small(tmpLng1) + " hundred " + outP
        END IF

        num = num \ 1000
        IF num < 1 THEN EXIT DO

        tmpLng1 = num MOD 1000
        IF tmpLng1 THEN outP = big(unit) + " " + outP

        unit = unit + 1
    LOOP

    IF number < 0 THEN outP = "negative " + outP

    FUNCTION = RTRIM$(outP)
END FUNCTION


FUNCTION PBMAIN () AS LONG
    DIM n AS QUAD

    #IF %DEF(%PB_CC32)
        INPUT "Gimme a number! ", n
    #ELSE
        n = VAL(INPUTBOX$("Gimme a number!", "Now!"))
    #ENDIF
    ? int2Text(n)
END FUNCTION
```


Sample output:
 Gimme a number! 1111111111111111111
 one quintillion one hundred eleven quadrillion one hundred eleven trillion one h
 undred eleven billion one hundred eleven million one hundred eleven thousand one
 hundred ten


## PowerShell


```PowerShell

function Get-NumberName
{
  <#
    .SYNOPSIS
        Spells out a number in English.
    .DESCRIPTION
        Spells out a number in English in the range of 0 to 999,999,999.
    .NOTES
        The code for this function was copied (almost word for word) from the C#
        example on this page to show how similar Powershell is to C#.
    .PARAMETER Number
        One or more integers in the range of 0 to 999,999,999.
    .EXAMPLE
        Get-NumberName -Number 666
    .EXAMPLE
        Get-NumberName 1, 234, 31337, 987654321
    .EXAMPLE
        1, 234, 31337, 987654321 | Get-NumberName
  #>
    [CmdletBinding()]
    [OutputType([string])]
    Param
    (
        [Parameter(Mandatory=$true, ValueFromPipeline=$true)]
        [ValidateRange(0,999999999)]
        [int[]]
        $Number
    )

    Begin
    {
        [string[]]$incrementsOfOne = "zero",    "one",     "two",       "three",    "four",
                                     "five",    "six",     "seven",     "eight",    "nine",
                                     "ten",     "eleven",  "twelve",    "thirteen", "fourteen",
                                     "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"

        [string[]]$incrementsOfTen = "",      "",      "twenty",  "thirty", "fourty",
                                     "fifty", "sixty", "seventy", "eighty", "ninety"

        [string]$millionName  = "million"
        [string]$thousandName = "thousand"
        [string]$hundredName  = "hundred"
        [string]$andName      = "and"

        function GetName([int]$i)
        {
            [string]$output = ""

            if ($i -ge 1000000)
            {
                $remainder = $null
                $output += (ParseTriplet ([Math]::DivRem($i,1000000,[ref]$remainder))) + " " + $millionName
                $i = $remainder

                if ($i -eq 0) { return $output }
            }

            if ($i -ge 1000)
            {
                if ($output.Length -gt 0)
                {
                    $output += ", "
                }

                $remainder = $null
                $output += (ParseTriplet ([Math]::DivRem($i,1000,[ref]$remainder))) + " " + $thousandName
                $i = $remainder

                if ($i -eq 0) { return $output }
            }

            if ($output.Length -gt 0)
            {
                $output += ", "
            }

            $output += (ParseTriplet $i)

            return $output
        }

        function ParseTriplet([int]$i)
        {
            [string]$output = ""

            if ($i -ge 100)
            {
                $remainder = $null
                $output += $incrementsOfOne[([Math]::DivRem($i,100,[ref]$remainder))] + " " + $hundredName
                $i = $remainder

                if ($i -eq 0) { return $output }
            }

            if ($output.Length -gt 0)
            {
                $output += " " + $andName + " "
            }

            if ($i -ge 20)
            {
                $remainder = $null
                $output += $incrementsOfTen[([Math]::DivRem($i,10,[ref]$remainder))]
                $i = $remainder

                if ($i -eq 0) { return $output }
            }

            if ($output.Length -gt 0)
            {
                $output += " "
            }

            $output += $incrementsOfOne[$i]

            return $output
        }
    }
    Process
    {
        foreach ($n in $Number)
        {
            [PSCustomObject]@{
                Number = $n
                Name   = GetName $n
            }
        }
    }
}

1, 234, 31337, 987654321 | Get-NumberName

```

```txt

   Number Name
   ------ ----
        1 one
      234 two hundred and thirty four
    31337 thirty one thousand, three hundred and thirty seven
987654321 nine hundred and eighty seven million, six hundred and fifty four thousand, three hundred and twenty one

```



## Prolog


```prolog

:- module(spell, [spell/2]).

%
% spell numbers up to the nonillions.
%

ones(1, "one").  ones(2, "two").    ones(3, "three").  ones(4, "four").  ones( 5, "five").
ones(6, "six").  ones(7, "seven").  ones(8, "eight").  ones(9, "nine").  ones(10, "ten").

ones(11, "eleven").     ones(12, "twelve").    ones(13, "thirteen").
ones(14, "fourteen").   ones(15, "fifteen").   ones(16, "sixteen").
ones(17, "seventeen").  ones(18, "eighteen").  ones(19, "nineteen").

tens(2, "twenty").  tens(3, "thirty").  tens(4, "forty").   tens(5, "fifty").
tens(6, "sixty").   tens(7, "seventy"). tens(8, "eighty").  tens(9, "ninety").

group( 1, "thousand").   group( 2, "million").      group(3, "bilion").
group( 4, "trillion").   group( 5, "quadrillion").  group(6, "quintillion").
group( 7, "sextilion").  group( 8, "septillion").   group(9, "octillion").
group(10, "nonillion").  group(11, "decillion").

spellgroup(N, G) :- G is floor(log10(N) / 3).

spell(N, S) :-
    N < 0, !,
    NegN is -N, spell(NegN, S0),
    string_concat("negative ", S0, S).
spell(0, "zero") :- !.
spell(N, S) :- between(1, 19, N), ones(N, S), !.
spell(N, S) :-
    N < 100, !,
    divmod(N, 10, Tens, Ones),
    tens(Tens, StrTens), ones_part(Ones, StrOnes),
    string_concat(StrTens, StrOnes, S).
spell(N, S) :-
    N < 1000, !,
    divmod(N, 100, Hundreds, Tens),
    ones(Hundreds, H), string_concat(H, " hundred", StrHundreds),
    tens_part(Tens, StrTens),
    string_concat(StrHundreds, StrTens, S).
spell(N, S) :-
    spellgroup(N, G), group(G, StrG),
    M is 10**(3*G), divmod(N, M, Group, Rest),
    spell(Group, S1),
    spell_remaining(Rest, S2),
    format(string(S), "~w ~w~w", [S1, StrG, S2]).

ones_part(0, "") :- !.
ones_part(N, S) :-
    ones(N, StrN),
    string_concat("-", StrN, S).

tens_part(0, "") :- !.
tens_part(N, S) :-
    spell(N, Tens),
    string_concat(" and ", Tens, S).

spell_remaining(0, "") :- !.
spell_remaining(N, S) :-
    spell(N, Rest),
    string_concat(", ", Rest, S).

```

```txt

?- use_module(library(spell)).
true.

?- spell(0, Txt).
Txt = "zero".

?- spell(73, Txt).
Txt = "seventy-three".

?- spell(-144001, Txt).
Txt = "negative one hundred and forty-four thousand, one".

?- BigPrime is 2**89 - 1, spell(BigPrime, Txt).
BigPrime = 618970019642690137449562111,
Txt = "six hundred and eighteen septillion, nine hundred and seventy sextilion, nineteen quintillion, six hundred and forty-two quadrillion, six hundred and ninety trillion, one hundred and thirty-seven bilion, four hundred and forty-nine million, five hundred and sixty-two thousand, one hundred and eleven".

```



## PureBasic

The range of integers handled has been set at an obscene 45 digits.

```PureBasic
DataSection
  numberNames:
  ;small
  Data.s "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten"
  Data.s "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"
  ;tens
  Data.s "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"
  ;big, non-Chuquet system
  Data.s "thousand", "million", "billion", "trillion", "quadrillion", "quintillion", "sextillion"
  Data.s "septillion", "octillion", "nonillion", "decillion", "undecillion", "duodecillion"
  Data.s "tredecillion"
EndDataSection

Procedure.s numberWords(number.s)
  ;handles integers from -1E45 to +1E45
  Static isInitialized = #False
  Static Dim small.s(19)
  Static Dim tens.s(9)
  Static Dim big.s(14)

  If Not isInitialized
    Restore numberNames
    For i = 1 To 19
      Read.s small(i)
    Next
    For i = 2 To 9
      Read.s tens(i)
    Next
    For i = 1 To 14
      Read.s big(i)
    Next
    isInitialized = #True
  EndIf

  For i = 1 To Len(number)
    If Not FindString("- 0123456789", Mid(number,i,1), 1)
      number = Left(number, i - 1) ;trim number to the last valid character
      Break ;exit loop
    EndIf
  Next

  Protected IsNegative = #False
  number = Trim(number)
  If Left(number,1) = "-"
    IsNegative = #True
    number = Trim(Mid(number, 2))
  EndIf

  If CountString(number, "0") = Len(number)
    ProcedureReturn "zero"
  EndIf

  If Len(number) > 45
    ProcedureReturn "Number is too big!"
  EndIf

  Protected num.s = number, output.s, unit, unitOutput.s, working

  Repeat
    working = Val(Right(num, 2))
    unitOutput = ""
    Select working
      Case 1 To 19
        unitOutput = small(working)
      Case 20 To 99
        If working % 10
          unitOutput = tens(working / 10) + "-" + small(working % 10)
        Else
          unitOutput = tens(working / 10)
        EndIf
    EndSelect

    working = Val(Right(num, 3)) / 100
    If working
      If unitOutput <> ""
        unitOutput = small(working) + " hundred " + unitOutput
      Else
        unitOutput = small(working) + " hundred"
      EndIf
    EndIf

    If unitOutput <> "" And unit > 0
      unitOutput + " " + big(unit)
      If output <> ""
        unitOutput + ", "
      EndIf
    EndIf

    output = unitOutput + output

    If Len(num) > 3
      num = Left(num, Len(num) - 3)
      unit + 1
    Else
      Break ;exit loop
    EndIf
  ForEver

  If IsNegative
    output = "negative " + output
  EndIf

  ProcedureReturn output
EndProcedure

Define n$
If OpenConsole()
  Repeat
    Repeat
      Print("Give me an integer (or q to quit)! ")
      n$ = Input()
    Until n$ <> ""

    If Left(Trim(n$),1) = "q"
      Break ;exit loop
    EndIf
    PrintN(numberWords(n$))
  ForEver
  CloseConsole()
EndIf

```

```txt
Give me an integer (or q to quit)! 3
three
Give me an integer (or q to quit)! -1327
negative one thousand, three hundred twenty-seven
Give me an integer (or q to quit)! 0
zero
Give me an integer (or q to quit)! 100000000002000000000000000300000000000000004
one hundred tredecillion, two decillion, three hundred quadrillion, four
```


== {{header|Python}} ==
Note: This example is also used as a module in the [[Names to numbers#Python]] task and should be kept in-sync.


```python
TENS = [None, None, "twenty", "thirty", "forty",
        "fifty", "sixty", "seventy", "eighty", "ninety"]
SMALL = ["zero", "one", "two", "three", "four", "five",
         "six", "seven", "eight", "nine", "ten", "eleven",
         "twelve", "thirteen", "fourteen", "fifteen",
         "sixteen", "seventeen", "eighteen", "nineteen"]
HUGE = [None, None] + [h + "illion"
                       for h in ("m", "b", "tr", "quadr", "quint", "sext",
                                  "sept", "oct", "non", "dec")]

def nonzero(c, n, connect=''):
    return "" if n == 0 else connect + c + spell_integer(n)

def last_and(num):
    if ',' in num:
        pre, last = num.rsplit(',', 1)
        if ' and ' not in last:
            last = ' and' + last
        num = ''.join([pre, ',', last])
    return num

def big(e, n):
    if e == 0:
        return spell_integer(n)
    elif e == 1:
        return spell_integer(n) + " thousand"
    else:
        return spell_integer(n) + " " + HUGE[e]

def base1000_rev(n):
    # generates the value of the digits of n in base 1000
    # (i.e. 3-digit chunks), in reverse.
    while n != 0:
        n, r = divmod(n, 1000)
        yield r

def spell_integer(n):
    if n < 0:
        return "minus " + spell_integer(-n)
    elif n < 20:
        return SMALL[n]
    elif n < 100:
        a, b = divmod(n, 10)
        return TENS[a] + nonzero("-", b)
    elif n < 1000:
        a, b = divmod(n, 100)
        return SMALL[a] + " hundred" + nonzero(" ", b, ' and')
    else:
        num = ", ".join([big(e, x) for e, x in
                         enumerate(base1000_rev(n)) if x][::-1])
        return last_and(num)

if __name__ == '__main__':
    # examples
    for n in (0, -3, 5, -7, 11, -13, 17, -19, 23, -29):
        print('%+4i -> %s' % (n, spell_integer(n)))
    print('')

    n = 201021002001
    while n:
        print('%-12i -> %s' % (n, spell_integer(n)))
        n //= -10
    print('%-12i -> %s' % (n, spell_integer(n)))
    print('')
```


```txt
  +0 -> zero
  -3 -> minus three
  +5 -> five
  -7 -> minus seven
 +11 -> eleven
 -13 -> minus thirteen
 +17 -> seventeen
 -19 -> minus nineteen
 +23 -> twenty-three
 -29 -> minus twenty-nine

201021002001 -> two hundred and one billion, twenty-one million, two thousand, and one
-20102100201 -> minus twenty billion, one hundred and two million, one hundred thousand, two hundred and one
2010210020   -> two billion, ten million, two hundred and ten thousand, and twenty
-201021002   -> minus two hundred and one million, twenty-one thousand, and two
20102100     -> twenty million, one hundred and two thousand, and one hundred
-2010210     -> minus two million, ten thousand, two hundred and ten
201021       -> two hundred and one thousand, and twenty-one
-20103       -> minus twenty thousand, one hundred and three
2010         -> two thousand, and ten
-201         -> minus two hundred and one
20           -> twenty
-2           -> minus two
0            -> zero
```


An alternative solution that can name very large numbers.


```python

def int_to_english(n):
    if n < 0: return "minus " + int_to_english(-n)
    if n < 10:
        return ["zero", "one", "two", "three", "four", "five",
                "six", "seven", "eight", "nine"][n]
    if n < 20:
        return ["ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen",
                "sixteen", "seventeen", "eighteen", "nineteen"][n-10]
    if n < 100:
        tens = ["twenty", "thirty", "forty", "fifty", "sixty",
                "seventy", "eighty", "ninety"][(n // 10 - 2)%10]
        if n % 10 != 0:
            return tens + "-" + int_to_english(n % 10)
        else:
            return tens
    if n < 1000:
        if n % 100 == 0:
            return int_to_english(n // 100) + " hundred"
        else:
            return int_to_english(n // 100) + " hundred and " +\
               int_to_english(n % 100)
    # http://www.isthe.com/chongo/tech/math/number/tenpower.html
    powers = [("thousand", 3), ("million", 6),
              ("billion", 9), ("trillion", 12), ("quadrillion", 15),
              ("quintillion", 18), ("sextillion", 21), ("septillion", 24),
              ("octillion", 27), ("nonillion", 30), ("decillion", 33),
              ("undecillion", 36), ("duodecillion", 39), ("tredecillion", 42),
              ("quattuordecillion", 45), ("quindecillion", 48),
              ("sexdecillion", 51), ("eptendecillion", 54),
              ("octadecillion", 57), ("novemdecillion", 61),
              ("vigintillion", 64)]
    ns = str(n)
    idx = len(powers) - 1
    while True:
        d = powers[idx][1]
        if len(ns) > d:
            first = int_to_english(int(ns[:-d]))
            second = int_to_english(int(ns[-d:]))
            if second == "zero":
                return first + " " + powers[idx][0]
            else:
                return first + " " + powers[idx][0] + " " + second
        idx = idx - 1

if __name__ == "__main__":
    print(int_to_english(42))
    print(int_to_english(3 ** 7))
    print(int_to_english(2 ** 100))
    print(int_to_english(10 ** (2*64)))

```

```txt

forty-two
two thousand one hundred and eighty-seven
one nonillion two hundred and sixty-seven octillion six hundred and fifty septillion six hundred sextillion two hundred and twenty-eight quintillion two hundred and twenty-nine quadrillion four hundred and one trillion four hundred and ninety-six billion seven hundred and three million two hundred and five thousand three hundred and seventy-six
one vigintillion vigintillion

```



## Racket



```Racket

#lang racket

(define smalls
  (map symbol->string
       '(zero one two three four five six seven eight nine ten eleven twelve
         thirteen fourteen fifteen sixteen seventeen eighteen nineteen)))

(define tens
  (map symbol->string
       '(zero ten twenty thirty forty fifty sixty seventy eighty ninety)))

(define larges
  (map symbol->string
       '(thousand million billion trillion quadrillion quintillion sextillion
         septillion octillion nonillion decillion undecillion duodecillion
         tredecillion quattuordecillion quindecillion sexdecillion
         septendecillion octodecillion novemdecillion vigintillion)))

(define (integer->english n)
  (define (step div suffix separator [subformat integer->english])
    (define-values [q r] (quotient/remainder n div))
    (define S (if suffix (~a (subformat q) " " suffix) (subformat q)))
    (if (zero? r) S (~a S separator (integer->english r))))
  (cond [(< n 0) (~a "negative " (integer->english (- n)))]
        [(< n 20) (list-ref smalls n)]
        [(< n 100) (step 10 #f "-" (curry list-ref tens))]
        [(< n 1000) (step 100 "hundred" " and ")]
        [else (let loop ([N 1000000] [D 1000] [unit larges])
                (cond [(null? unit)
                       (error 'integer->english "number too big: ~e" n)]
                      [(< n N) (step D (car unit) ", ")]
                      [else (loop (* 1000 N) (* 1000 D) (cdr unit))]))]))

(for ([n 10])
  (define e (expt 10 n))
  (define r (+ (* e (random e)) (random e)))
  (printf "~s: ~a\n" r (integer->english r)))

```


```txt

0: zero
46: forty-six
969: nine hundred and sixty-nine
959365: nine hundred and fifty-nine thousand, three hundred and sixty-five
49561453: forty-nine million, five hundred and sixty-one thousand, four hundred and fifty-three
3372839576: three billion, three hundred and seventy-two million, eight hundred and thirty-nine thousand, five hundred and seventy-six
589723344094: five hundred and eighty-nine billion, seven hundred and twenty-three million, three hundred and forty-four thousand, ninety-four
76114840325710: seventy-six trillion, one hundred and fourteen billion, eight hundred and forty million, three hundred and twenty-five thousand, seven hundred and ten
7555965500511815: seven quadrillion, five hundred and fifty-five trillion, nine hundred and sixty-five billion, five hundred million, five hundred and eleven thousand, eight hundred and fifteen
225539847375452743: two hundred and twenty-five quadrillion, five hundred and thirty-nine trillion, eight hundred and forty-seven billion, three hundred and seventy-five million, four hundred and fifty-two thousand, seven hundred and forty-three

```


See also [http://planet.racket-lang.org/package-source/neil/numspell.plt/1/2/planet-docs/numspell/index.html numspell] by Neil van Dyke.


## REXX

The REXX program used for this entry is limited   (for the American-style)   numbers to   <big>10<sup>3002</sup>   -1</big>,
and roughly double that for the British-style numbers.

For the REXX program and its associated help document, see   ───►   [[Number names/REXX]].





## Ring

this simple script has support for zero,negative integers, and floating-point as well as  positive integers
this simple script available [https://github.com/AbdelrahmanGIT/RingSamples/blob/master/src/ConvertNumbersToString.ring here]


```ring

OneList=["zero",    "one",     "two",       "three",    "four",
              "five",    "six",     "seven",     "eight",    "nine",
              "ten",     "eleven",  "twelve",    "thirteen", "fourteen",
              "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]
tenList=["" , "" , "twenty",  "thirty", "fourty",
            "fifty", "sixty", "seventy", "eighty", "ninety"]

millionStr="Million"
thousandStr="Thousand"
hundredStr="Hundred"
andStr="And"
pointStr=" Point "

while true
	see "enter number to convert:"
	give theNumber

	pointSplited=splitString(theNumber,".")
	fraction=0

	useFr=false
	if len(pointSplited) >=1 theNumber=pointSplited[1] ok
	if len(pointSplited) >=2 useFr=true fraction=pointSplited[2] ok
	pointSplited=null

	see getName(number(theNumber))
	if useFr=true see pointStr + getName(number(fraction)) ok
	see nl
end

func getName num
rtn=null
if num=0
    rtn += OneList[floor(num+1)]
	return rtn
ok
if num<0
	return "minus " + getName(fabs(num))
ok
if num>= 1000000
	rtn += getName(num / 1000000) +" "+ millionStr
	num%=1000000
ok
if num>=1000
	if len(rtn)>0 rtn += ", " ok

	rtn += getName(num / 1000)+ " " + thousandStr
	num%=1000
ok

if num >=100
if len(rtn)>0 rtn += ", " ok
	rtn += OneList[floor((num / 100)+1)] + " " + hundredStr
	num%=100
ok

if num=0
	return rtn +
ok
if len(rtn)>0 rtn += " " + andStr + " " ok
if(num>=20)

	rtn += tenList[floor((num / 10)+1)]
	num%=10
ok
if num=0
	return rtn
ok
if len(rtn)>0 rtn +=  " " ok
rtn += OneList[num+1]
return rtn

func splitString str,chr
	for i in str if strcmp(i,chr)=0 i=nl ok next
	return str2list(str)


```

for input of:

```txt

1 2 500 576 1000 1045 1124 2521 5223 10877 233112 12333123 1000000
-124 -421 -656 -323 -1123 -9976
1.5 3.32 0.12 0.100 -3.54 -65.1

```

the output is:

```txt

enter number to convert:1
one
enter number to convert:2
two
enter number to convert:500
five Hundred
enter number to convert:576
five Hundred And seventy six
enter number to convert:1000
one Thousand
enter number to convert:1045
one Thousand And fourty five
enter number to convert:1124
one Thousand, one Hundred And twenty four
enter number to convert:2521
two Thousand, five Hundred And twenty one
enter number to convert:5223
five Thousand, two Hundred And twenty three
enter number to convert:10877
ten Thousand, eight Hundred And seventy seven
enter number to convert:233112
two Hundred And thirty three Thousand, one Hundred And  twelve
enter number to convert:12333123
twelve Million, three Hundred And thirty three Thousand, one Hundred And twenty three
enter number to convert:1000000
one Million
enter number to convert:-124
minus one Hundred And twenty four
enter number to convert:-421
minus four Hundred And twenty one
enter number to convert:-656
minus six Hundred And fifty six
enter number to convert:-323
minus three Hundred And twenty three
enter number to convert:-1123
minus one Thousand, one Hundred And twenty three
enter number to convert:-9976
minus nine Thousand, nine Hundred And seventy six
enter number to convert:1.5
one Point five
enter number to convert:3.32
three Point thirty two
enter number to convert:0.12
zero Point twelve
enter number to convert:0.100
zero Point one Hundred
enter number to convert:-3.54
minus three Point fifty four
enter number to convert:-65.1
minus sixty five Point one
enter number to convert:

```



## Ruby


```ruby
SMALL = %w(zero one two three four five six seven eight nine ten
           eleven twelve thirteen fourteen fifteen sixteen seventeen
           eighteen nineteen)

TENS = %w(wrong wrong twenty thirty forty fifty sixty seventy
          eighty ninety)

BIG = [nil, "thousand"] +
      %w( m b tr quadr quint sext sept oct non dec).map{ |p| "#{p}illion" }

def wordify number
  case
  when number < 0
    "negative #{wordify -number}"

  when number < 20
    SMALL[number]

  when number < 100
    div, mod = number.divmod(10)
    TENS[div] + (mod==0 ? "" : "-#{wordify mod}")

  when number < 1000
    div, mod = number.divmod(100)
    "#{SMALL[div]} hundred" + (mod==0 ? "" : " and #{wordify mod}")

  else
    # separate into 3-digit chunks
    chunks = []
    div = number
    while div != 0
      div, mod = div.divmod(1000)
      chunks << mod                 # will store smallest to largest
    end

    raise ArgumentError, "Integer value too large." if chunks.size > BIG.size

    chunks.map{ |c| wordify c }.
           zip(BIG).    # zip pairs up corresponding elements from the two arrays
           find_all { |c| c[0] != 'zero' }.
           map{ |c| c.join ' '}.    # join ["forty", "thousand"]
           reverse.
           join(', ').              # join chunks
           strip
  end
end

data = [-1123, 0, 1, 20, 123, 200, 220, 1245, 2000, 2200, 2220, 467889,
        23_000_467, 23_234_467, 2_235_654_234, 12_123_234_543_543_456,
        987_654_321_098_765_432_109_876_543_210_987_654,
        123890812938219038290489327894327894723897432]

data.each do |n|
  print "#{n}: "
  begin
    puts "'#{wordify n}'"
  rescue => e
    puts "Error: #{e}"
  end
end
```

```txt

-1123: 'negative one thousand, one hundred and twenty-three'
0: 'zero'
1: 'one'
20: 'twenty'
123: 'one hundred and twenty-three'
200: 'two hundred'
220: 'two hundred and twenty'
1245: 'one thousand, two hundred and forty-five'
2000: 'two thousand'
2200: 'two thousand, two hundred'
2220: 'two thousand, two hundred and twenty'
467889: 'four hundred and sixty-seven thousand, eight hundred and eighty-nine'
23000467: 'twenty-three million, four hundred and sixty-seven'
23234467: 'twenty-three million, two hundred and thirty-four thousand, four hundred and sixty-seven'
2235654234: 'two billion, two hundred and thirty-five million, six hundred and fifty-four thousand, two hundred and thirty-four'
12123234543543456: 'twelve quadrillion, one hundred and twenty-three trillion, two hundred and thirty-four billion, five hundred and forty-three million, five hundred and forty-three thousand, four hundred and fifty-six'
987654321098765432109876543210987654: 'nine hundred and eighty-seven decillion, six hundred and fifty-four nonillion, three hundred and twenty-one octillion, ninety-eight septillion, seven hundred and sixty-five sextillion, four hundred and thirty-two quintillion, one hundred and nine quadrillion, eight hundred and seventy-six trillion, five hundred and forty-three billion, two hundred and ten million, nine hundred and eighty-seven thousand, six hundred and fifty-four'
123890812938219038290489327894327894723897432: Error: Integer value too large.

```



## Rust


```rust
use std::io::{self, Write, stdout};

const SMALL: &[&str] = &[
    "zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten",
    "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen",
    "nineteen",
];

const TENS: &[&str] = &[
    "PANIC", "PANIC", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety",
];

const MAGNITUDE: &[&str] = &[
    "PANIC", "thousand", "million", "billion", "trillion", "quadrillion", "quintillion",
];

fn wordify<W: Write>(w: &mut W, mut number: i64) -> Result<(), io::Error> {
    if number == 0 {
        return write!(w, "zero");
    }
    if number < 0 {
        write!(w, "negative ")?;
        number = -number;
    }
    while number != 0 {
        if number < 20 {
            write!(w, "{}", SMALL[number as usize])?;
            break;
        } else if number < 100 {
            write!(w, "{}", TENS[number as usize / 10])?;
            number %= 10;
            if number != 0 {
                write!(w, "-")?;
            }
        } else if number < 1_000 {
            write!(w, "{} hundred", SMALL[number as usize / 100])?;
            number %= 100;
            if number != 0 {
                write!(w, " and ")?;
            }
        } else {
            let mut top = number;
            let mut magnitude = 0i64;
            let mut magnitude_pow = 1i64;
            while top >= 1_000 {
                top /= 1_000;
                magnitude += 1;
                magnitude_pow *= 1_000;
            }
            wordify(w, top)?;
            number %= magnitude_pow;
            if number == 0 {
                write!(w, " {}", MAGNITUDE[magnitude as usize])?;
            } else if number > 100 {
                write!(w, " {}, ", MAGNITUDE[magnitude as usize])?;
            } else {
                write!(w, " {} and ", MAGNITUDE[magnitude as usize])?;
            }
        }
    }
    Ok(())
}

fn main() {
    let stdout = stdout();
    let mut stdout = stdout.lock();
    for &n in &[12, 1048576, 9_000_000_000_000_000_000, -2, 0, 5_000_000_000_000_000_001, -555_555_555_555] {
        wordify(&mut stdout, n).unwrap();
        write!(&mut stdout, "\n").unwrap();
    }
}
```


```txt

twelve
one million, forty-eight thousand, five hundred and seventy-six
nine quintillion
negative two
zero
five quintillion and one
negative five hundred and fifty-five trillion, five hundred and fifty-five billion, five hundred and fifty-five million, five hundred and fifty-five thousand, five hundred and fifty-five

```



## Scala

```scala
import scala.annotation.tailrec
import scala.collection.parallel.ParSeq

/** Spells an English numeral longhand. The numbers are expressed using words.
 *
 *  The implementation goes up to 10<sup>69</sup>-1 and also supports negative and zero inputs.
 *
 *  @example longhand( 1234 )  // results in: "one thousand two hundred thirty-four".
 */
trait LongHand {
  /** Spells a number longhand
   *
   *  Done by recursively process the triplets of decimal numbers.
   *  @param numeral		the numeric value to be converted
   *  @param showAnd		flag the output extra and in output, default off
   *  @param zeroString		the word for 0, default to "zero"
   *  @param showHyphen		hyphenate all compound numbers e.g. twenty-four, default is on
   *  @return				the numeric value expressed in words
   */
  def longhand(numeral: BigInt,
               showAnd: Boolean = false,
               zeroString: String = "zero",
               showHyphen: Boolean = true): String = {

    val condAndString = if (showAnd) "and " else ""
    val condHyphenString = if (showHyphen) "-" else " "

    // 234 Becomes "two hundred [and] thirty-four"
    def composeScale(nnn: String, isLSDgroup: Boolean, strE3: String): String = {
      nnn match { // Rare exceptions confirms the rule
        case "000" => ""
        case "100" => onesAndTeens(1) + hundredString + strE3 // Solves the faulty hundred AND thousand problem
        case _ => {
          val eval = (nnn.par.map(_.toString.toInt).reverse zip ParSeq('units, 'tens, 'hundreds)).reverse

          eval.map {
            case (d, 'units) if eval.seq.contains(1, 'tens) => onesAndTeens(d + 10)
            case (d, 'units) if (isLSDgroup && nnn == "0") => zeroString
            case (d, 'units) => onesAndTeens(d)
            case (d, 'hundreds) if d > 0 => onesAndTeens(d) + hundredString + condAndString
            case (d, 'tens) if d > 1 && eval.seq.contains(0, 'units) => tens(d)
            case (d, 'tens) if d > 1 => tens(d) + condHyphenString //'
            case _ => ""
          }.mkString + strE3
        }
      }
    } // def composeScale(…

    def compose(n: BigInt): String = {
      // "1234" becomes List((1,"thousand"), (234, ""))
      val decGroups = n.toString.reverse.grouped(3).map(_.reverse).toSeq.par // Group into powers of thousands
      if (decGroups.size <= shortScale.size) // Detect overflow
      { // Send per group section to composeScale
        @tailrec
        def iter(elems: Seq[(String, String)], acc: String): String = {
          elems match {
            case (group, powers) :: tail => {
              iter(tail, acc + composeScale(group, tail == Nil, powers))
            }
            case _ => acc
          }
        } // Group of decimals are accompanied with the short scale name.
        iter(decGroups.zip(shortScale).reverse.toList, "").mkString.trim
      } else "###.overflow.###"
    } // def compose(…

    // Here starts def longhand(…
    if (numeral < 0) "minus " + compose(-numeral) else compose(numeral)
  } // End def longhand(…

  private val onesAndTeens = {
    def dozen = "one two three four five six seven eight nine ten eleven twelve".split(' ').map(_ + " ").par
    def teens = "thir four fif six seven eigh nine".split(' ').map(_ + "teen ").par
    ParSeq("") ++ dozen ++ teens
  }

  private val tens = ParSeq("", "") ++
    ("twen thir for fif six seven eigh nine".split(' ')).map(_ + "ty")
  private final val hundredString = "hundred "
  private val shortScale = {
    def p1 = "m b tr quadr quint sext sept oct non dec".split(' ').map(_ + "illion ").par
    def p2 = "un duo tre quattuor quin sex septen octo novem ".split(' ').map(_ + "decillion ").par
    def p3 = "vigint cent".split(' ').map(_ + "illion ").par
    ParSeq("", "thousand ") ++ p1 ++ p2 ++ p3
  }
} // trait LongHand

object SpellNumber extends LongHand with App {
  // Main entry A little test...
  { // Anonymous ordered list as test set
    def testVal1 = BigInt("1" * 69)
    def testVal9 = BigInt(10).pow(69) - 1

    @tailrec // Series generator of 9, 98, 987, 9876 …
    def inner(counter: Int, elem: BigInt, testList: ParSeq[BigInt]): ParSeq[BigInt] = {
      if (counter < 20)
        inner(counter + 1, elem * 10 + (9 - (counter % 10)), testList ++ ParSeq(elem))
      else testList.par
    }
    inner(0, 0L, // Test values
      ParSeq(-Long.MaxValue, -1000000000, 12, 13, 19, 20, 21, 112, 1001, 1012, 1013,
        Long.MaxValue - 1, Long.MaxValue - 13, testVal1, testVal9)) ++
      (for (z <- 0 to 69) yield BigInt(10).pow(z)) // powers of ten

  }.seq.sorted.foreach(num => println(f"$num%+,80d -> ${longhand(numeral = num, showAnd = true)}"))
} // object SpellNumber @ line 110
```


```txt
                                                      -9.223.372.036.854.775.807 -> minus nine quintillion two hundred and twenty-three quadrillion three hundred and seventy-two trillion thirty-six billion eight hundred and fifty-four million seven hundred and seventy-five thousand eight hundred and seven
                                                                  -1.000.000.000 -> minus one billion
                                                                              +0 -> zero
                                                                              +1 -> one
                                                                              +9 -> nine
                                                                             +10 -> ten
                                                                             +12 -> twelve
                                                                             +13 -> thirteen
                                                                             +19 -> nineteen
                                                                             +20 -> twenty
                                                                             +21 -> twenty-one
                                                                             +98 -> ninety-eight
                                                                            +100 -> one hundred
                                                                            +112 -> one hundred and twelve
                                                                            +987 -> nine hundred and eighty-seven
                                                                          +1.000 -> one thousand
                                                                          +1.001 -> one thousand one
                                                                          +1.012 -> one thousand twelve
                                                                          +1.013 -> one thousand thirteen
                                                                          +9.876 -> nine thousand eight hundred and seventy-six
                                                                         +10.000 -> ten thousand
                                                                         +98.765 -> ninety-eight thousand seven hundred and sixty-five
                                                                        +100.000 -> one hundred thousand
                                                                        +987.654 -> nine hundred and eighty-seven thousand six hundred and fifty-four
                                                                      +1.000.000 -> one million
                                                                      +9.876.543 -> nine million eight hundred and seventy-six thousand five hundred and forty-three
                                                                     +10.000.000 -> ten million
                                                                     +98.765.432 -> ninety-eight million seven hundred and sixty-five thousand four hundred and thirty-two
                                                                    +100.000.000 -> one hundred million
                                                                    +987.654.321 -> nine hundred and eighty-seven million six hundred and fifty-four thousand three hundred and twenty-one
                                                                  +1.000.000.000 -> one billion
                                                                  +9.876.543.210 -> nine billion eight hundred and seventy-six million five hundred and forty-three thousand two hundred and ten
                                                                 +10.000.000.000 -> ten billion
                                                                 +98.765.432.109 -> ninety-eight billion seven hundred and sixty-five million four hundred and thirty-two thousand one hundred and nine
                                                                +100.000.000.000 -> one hundred billion
                                                                +987.654.321.098 -> nine hundred and eighty-seven billion six hundred and fifty-four million three hundred and twenty-one thousand ninety-eight
                                                              +1.000.000.000.000 -> one trillion
                                                              +9.876.543.210.987 -> nine trillion eight hundred and seventy-six billion five hundred and forty-three million two hundred and ten thousand nine hundred and eighty-seven
                                                             +10.000.000.000.000 -> ten trillion
                                                             +98.765.432.109.876 -> ninety-eight trillion seven hundred and sixty-five billion four hundred and thirty-two million one hundred and nine thousand eight hundred and seventy-six
                                                            +100.000.000.000.000 -> one hundred trillion
                                                            +987.654.321.098.765 -> nine hundred and eighty-seven trillion six hundred and fifty-four billion three hundred and twenty-one million ninety-eight thousand seven hundred and sixty-five
                                                          +1.000.000.000.000.000 -> one quadrillion
                                                          +9.876.543.210.987.654 -> nine quadrillion eight hundred and seventy-six trillion five hundred and forty-three billion two hundred and ten million nine hundred and eighty-seven thousand six hundred and fifty-four
                                                         +10.000.000.000.000.000 -> ten quadrillion
                                                         +98.765.432.109.876.543 -> ninety-eight quadrillion seven hundred and sixty-five trillion four hundred and thirty-two billion one hundred and nine million eight hundred and seventy-six thousand five hundred and forty-three
                                                        +100.000.000.000.000.000 -> one hundred quadrillion
                                                        +987.654.321.098.765.432 -> nine hundred and eighty-seven quadrillion six hundred and fifty-four trillion three hundred and twenty-one billion ninety-eight million seven hundred and sixty-five thousand four hundred and thirty-two
                                                      +1.000.000.000.000.000.000 -> one quintillion
                                                      +9.223.372.036.854.775.794 -> nine quintillion two hundred and twenty-three quadrillion three hundred and seventy-two trillion thirty-six billion eight hundred and fifty-four million seven hundred and seventy-five thousand seven hundred and ninety-four
                                                      +9.223.372.036.854.775.806 -> nine quintillion two hundred and twenty-three quadrillion three hundred and seventy-two trillion thirty-six billion eight hundred and fifty-four million seven hundred and seventy-five thousand eight hundred and six
                                                      +9.876.543.210.987.654.321 -> nine quintillion eight hundred and seventy-six quadrillion five hundred and forty-three trillion two hundred and ten billion nine hundred and eighty-seven million six hundred and fifty-four thousand three hundred and twenty-one
                                                     +10.000.000.000.000.000.000 -> ten quintillion
                                                    +100.000.000.000.000.000.000 -> one hundred quintillion
                                                  +1.000.000.000.000.000.000.000 -> one sextillion
                                                 +10.000.000.000.000.000.000.000 -> ten sextillion
                                                +100.000.000.000.000.000.000.000 -> one hundred sextillion
                                              +1.000.000.000.000.000.000.000.000 -> one septillion
                                             +10.000.000.000.000.000.000.000.000 -> ten septillion
                                            +100.000.000.000.000.000.000.000.000 -> one hundred septillion
                                          +1.000.000.000.000.000.000.000.000.000 -> one octillion
                                         +10.000.000.000.000.000.000.000.000.000 -> ten octillion
                                        +100.000.000.000.000.000.000.000.000.000 -> one hundred octillion
                                      +1.000.000.000.000.000.000.000.000.000.000 -> one nonillion
                                     +10.000.000.000.000.000.000.000.000.000.000 -> ten nonillion
                                    +100.000.000.000.000.000.000.000.000.000.000 -> one hundred nonillion
                                  +1.000.000.000.000.000.000.000.000.000.000.000 -> one decillion
                                 +10.000.000.000.000.000.000.000.000.000.000.000 -> ten decillion
                                +100.000.000.000.000.000.000.000.000.000.000.000 -> one hundred decillion
                              +1.000.000.000.000.000.000.000.000.000.000.000.000 -> one undecillion
                             +10.000.000.000.000.000.000.000.000.000.000.000.000 -> ten undecillion
                            +100.000.000.000.000.000.000.000.000.000.000.000.000 -> one hundred undecillion
                          +1.000.000.000.000.000.000.000.000.000.000.000.000.000 -> one duodecillion
                         +10.000.000.000.000.000.000.000.000.000.000.000.000.000 -> ten duodecillion
                        +100.000.000.000.000.000.000.000.000.000.000.000.000.000 -> one hundred duodecillion
                      +1.000.000.000.000.000.000.000.000.000.000.000.000.000.000 -> one tredecillion
                     +10.000.000.000.000.000.000.000.000.000.000.000.000.000.000 -> ten tredecillion
                    +100.000.000.000.000.000.000.000.000.000.000.000.000.000.000 -> one hundred tredecillion
                  +1.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000 -> one quattuordecillion
                 +10.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000 -> ten quattuordecillion
                +100.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000 -> one hundred quattuordecillion
              +1.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000 -> one quindecillion
             +10.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000 -> ten quindecillion
            +100.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000 -> one hundred quindecillion
          +1.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000 -> one sexdecillion
         +10.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000 -> ten sexdecillion
        +100.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000 -> one hundred sexdecillion
      +1.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000 -> one septendecillion
     +10.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000 -> ten septendecillion
    +100.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000 -> one hundred septendecillion
  +1.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000 -> one octodecillion
 +10.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000 -> ten octodecillion
+100.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000 -> one hundred octodecillion
+1.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000 -> one novemdecillion
+10.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000 -> ten novemdecillion
+100.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000 -> one hundred novemdecillion
+1.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000 -> one vigintillion
+10.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000 -> ten vigintillion
+100.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000 -> one hundred vigintillion
+1.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000 -> one centillion
+10.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000 -> ten centillion
+100.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000 -> one hundred centillion
+111.111.111.111.111.111.111.111.111.111.111.111.111.111.111.111.111.111.111.111.111.111.111 -> one hundred and eleven centillion one hundred and eleven vigintillion one hundred and eleven novemdecillion one hundred and eleven octodecillion one hundred and eleven septendecillion one hundred and eleven sexdecillion one hundred and eleven quindecillion one hundred and eleven quattuordecillion one hundred and eleven tredecillion one hundred and eleven duodecillion one hundred and eleven undecillion one hundred and eleven decillion one hundred and eleven nonillion one hundred and eleven octillion one hundred and eleven septillion one hundred and eleven sextillion one hundred and eleven quintillion one hundred and eleven quadrillion one hundred and eleven trillion one hundred and eleven billion one hundred and eleven million one hundred and eleven thousand one hundred and eleven
+999.999.999.999.999.999.999.999.999.999.999.999.999.999.999.999.999.999.999.999.999.999.999 -> nine hundred and ninety-nine centillion nine hundred and ninety-nine vigintillion nine hundred and ninety-nine novemdecillion nine hundred and ninety-nine octodecillion nine hundred and ninety-nine septendecillion nine hundred and ninety-nine sexdecillion nine hundred and ninety-nine quindecillion nine hundred and ninety-nine quattuordecillion nine hundred and ninety-nine tredecillion nine hundred and ninety-nine duodecillion nine hundred and ninety-nine undecillion nine hundred and ninety-nine decillion nine hundred and ninety-nine nonillion nine hundred and ninety-nine octillion nine hundred and ninety-nine septillion nine hundred and ninety-nine sextillion nine hundred and ninety-nine quintillion nine hundred and ninety-nine quadrillion nine hundred and ninety-nine trillion nine hundred and ninety-nine billion nine hundred and ninety-nine million nine hundred and ninety-nine thousand nine hundred and ninety-nine
+1.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000 -> ###.overflow.###
```



###  Recursive

Recursive TreeMap solution (for values up to trillions):

```scala
import scala.collection.immutable.TreeMap

val NUMBERS = TreeMap(
  1 -> "one", 2 -> "two", 3 -> "three", 4 -> "four", 5 -> "five", 6 -> "six", 7 -> "seven", 8 -> "eight", 9 -> "nine",
  10 -> "ten", 11 -> "eleven", 12 -> "twelve", 13 -> "thirteen", 14 -> "fourteen", 15 -> "fifteen", 16 -> "sixteen",
  17 -> "seventeen", 18 -> "eighteen", 19 -> "nineteen", 20 -> "twenty", 30 -> "thirty", 40 -> "forty",
  50 -> "fifty", 60 -> "sixty", 70 -> "seventy", 80 -> "eighty", 90 -> "ninety"
)

val HUNDREDS = TreeMap(
  100l -> "hundred", 1000l -> "thousand", 1000000l -> "million", 1000000000l -> "billion", 1000000000000l -> "trillion"
)

def numberToString(number: Long) : String = {
  if (HUNDREDS.to(number).nonEmpty) {
    val (h, hundreds) = HUNDREDS.to(number).last
    val remainder = number % h
    numberToString(number / h) + hundreds + {if (remainder > 0) {if (remainder < 100) " and " else ", "} + numberToString(remainder) else " "}
  } else if (NUMBERS.to(number.toInt).nonEmpty) {
    val (n, word) = NUMBERS.to(number.toInt).last
    val remainder = number - n
    word + {if (remainder > 0 && remainder < 10) "-" else " "} + numberToString(remainder)
  } else {
    ""
  }
}

```


Examples

```txt

85001 eighty-five thousand and one
155019 one hundred and fifty-five thousand and nineteen
4547000 four million, five hundred and forty-seven thousand
6766027 six million, seven hundred and sixty-six thousand and twenty-seven
55555555555l fifty-five billion, five hundred and fifty-five million, five hundred and fifty-five thousand, five hundred and fifty-five

```



## Seed7

The library [http://seed7.sourceforge.net/libraries/wrinum.htm wrinum.s7i] contains the function [http://seed7.sourceforge.net/libraries/wrinum.htm#str%28ENGLISH,in_integer%29 str(ENGLISH, ...)] which converts an integer to its written english equivalent.


```seed7
$ include "seed7_05.s7i";
  include "stdio.s7i";
  include "wrinum.s7i";

const proc: main is func
  local
    var integer: number is 0;
  begin
    for number range 1 to 999999 do
      writeln(str(ENGLISH, number));
    end for;
  end func;
```



## SequenceL

Works on all 32 bit signed integers.


```sequencel>import <Utilities/Math.sl
;
import <Utilities/Sequence.sl>;
import <Utilities/Conversion.sl>;
import <Utilities/String.sl>;

main(argv(2)) := delimit(numberToEnglish(stringToInt(argv)), '\n');

ones := ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"];
teens := ["eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"];
tens := ["ten", "twenty", "thrity", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"];

magnitudes := ["", "thousand", "million", "billion", "trillion"];

numberToEnglish(num(0)) :=
    let
        triplets[i] :=
                (num / integerPower(1000, i - 1)) mod 1000
                foreach i within 1 ... ceiling(log(1000, num + 1));

        englishtTriplets[j] := numberToEnglishHelper(triplets[j]);

        partials[j] :=
                englishtTriplets[j] ++ magnitudes[j] ++ ", " when size(englishtTriplets[j]) > 0
                foreach j within reverse(1 ... size(triplets));
    in
            "zero" when num = 0
        else
            "negative " ++ numberToEnglish(-num) when num < 0
        else
            trim(allButLast(trim(join(partials))));


numberToEnglishHelper(num(0)) :=
    let
        onesPlace := num mod 10;
        tensPlace := (num mod 100) / 10;
        hundredsPlace :=  (num mod 1000) / 100;

        onesWord :=     "ten " when tensPlace = 1 and onesPlace = 0
                    else
                        "" when onesPlace = 0
                    else
                        teens[onesPlace] ++ " " when tensPlace = 1
                    else
                        ones[onesPlace] ++ " ";

        tensWord :=    "" when tensPlace = 0 or tensPlace = 1
                    else
                        tens[tensPlace] ++ " " when onesPlace = 0
                    else
                        tens[tensPlace] ++ "-";

        hundredsWord :=     "" when hundredsPlace = 0
                         else
                             ones[hundredsPlace] ++ " hundred ";

        andWord := "" when hundredsPlace = 0 or (tensPlace = 0 and onesPlace = 0) else "and ";


    in
        hundredsWord ++ andWord ++ tensWord ++ onesWord;
```


<pre style="height: 40ex; overflow: scroll">
$>NumberName.exe 99 300 -310 1501 12609 -512609 -43112609 123456789
"ninety-nine
three hundred
negative three hundred and ten
one thousand, five hundred and one
twelve thousand, six hundred and nine
negative five hundred and twelve thousand, six hundred and nine
negative forty-three million, one hundred and twelve thousand, six hundred and nine
one hundred and twenty-three million, four hundred and fifty-six thousand, seven hundred and eighty-nine"

```



## Sidef


```ruby
var l = frequire('Lingua::EN::Numbers');
say l.num2en(123456789);
```


```txt

one hundred and twenty-three million, four hundred and fifty-six thousand, seven hundred and eighty-nine

```



## SQL


```SQL

select val, to_char(to_date(val,'j'),'jsp') name
from
(
select
round( dbms_random.value(1, 5373484)) val
from dual
connect by level <= 5
);

select to_char(to_date(5373485,'j'),'jsp') from dual;

```


```txt

       VAL NAME
---------- --------------------------------------------------------------------------------
   5260806 five million two hundred sixty thousand eight hundred six
     10498 ten thousand four hundred ninety-eight
   4338863 four million three hundred thirty-eight thousand eight hundred sixty-three
   2315193 two million three hundred fifteen thousand one hundred ninety-three
   1398202 one million three hundred ninety-eight thousand two hundred two

select to_char(to_date(5373485,'j'),'jsp') from dual
                       *
ERROR at line 1:
ORA-01854: julian date must be between 1 and 5373484

```



## Swift


```swift
extension Int {
  private static let bigNames = [
    1_000: "thousand",
    1_000_000: "million",
    1_000_000_000: "billion",
    1_000_000_000_000: "trillion",
    1_000_000_000_000_000: "quadrillion",
    1_000_000_000_000_000_000: "quintillion"
  ]

  private static let names = [
    1: "one",
    2: "two",
    3: "three",
    4: "four",
    5: "five",
    6: "six",
    7: "seven",
    8: "eight",
    9: "nine",
    10: "ten",
    11: "eleven",
    12: "twelve",
    13: "thirteen",
    14: "fourteen",
    15: "fifteen",
    16: "sixteen",
    17: "seventeen",
    18: "eighteen",
    19: "nineteen",
    20: "twenty",
    30: "thirty",
    40: "forty",
    50: "fifty",
    60: "sixty",
    70: "seventy",
    80: "eighty",
    90: "ninety"
  ]

  public var numberName: String {
    guard self != 0 else {
      return "zero"
    }

    let neg = self < 0
    let maxNeg = self == Int.min
    var nn: Int

    if maxNeg {
      nn = -(self + 1)
    } else if neg {
      nn = -self
    } else {
      nn = self
    }

    var digits3 = [Int](repeating: 0, count: 7)

    for i in 0..<7 {
      digits3[i] = (nn % 1000)
      nn /= 1000
    }

    func threeDigitsToText(n: Int) -> String {
      guard n != 0 else {
        return ""
      }

      var ret = ""

      let hundreds = n / 100
      let remainder = n % 100

      if hundreds > 0 {
        ret += "\(Int.names[hundreds]!) hundred"

        if remainder > 0 {
          ret += " "
        }
      }

      if remainder > 0 {
        let tens = remainder / 10
        let units = remainder % 10

        if tens > 1 {
          ret += Int.names[tens * 10]!

          if units > 0 {
            ret += "-\(Int.names[units]!)"
          }
        } else {
          ret += Int.names[remainder]!
        }
      }

      return ret
    }

    let strings = (0..<7).map({ threeDigitsToText(n: digits3[$0]) })
    var name = strings[0]
    var big = 1000

    for i in 1...6 {
      if digits3[i] > 0 {
        var name2 = "\(strings[i]) \(Int.bigNames[big]!)"

        if name.count > 0 {
          name2 += ", "
        }

        name = name2 + name
      }

      big &*= 1000
    }

    if maxNeg {
      name = String(name.dropLast(5) + "eight")
    }

    return neg ? "minus \(name)" : name
  }
}

let nums = [
  0, 1, 7, 10, 18, 22, 67, 99, 100, 105, 999, -1056, 1000005000,
  2074000000, 1234000000745003, Int.min
]

for number in nums {
  print("\(number) => \(number.numberName)")
}
```


```txt
0 => zero
1 => one
7 => seven
10 => ten
18 => eighteen
22 => twenty-two
67 => sixty-seven
99 => ninety-nine
100 => one hundred
105 => one hundred five
999 => nine hundred ninety-nine
-1056 => minus one thousand, fifty-six
1000005000 => one billion, five thousand
2074000000 => two billion, seventy-four million
1234000000745003 => one quadrillion, two hundred thirty-four trillion, seven hundred forty-five thousand, three
-9223372036854775808 => minus nine quintillion, two hundred twenty-three quadrillion, three hundred seventy-two trillion, thirty-six billion, eight hundred fifty-four million, seven hundred seventy-five thousand, eight hundred eight
```



## Tcl


```tcl
proc int2words {n} {
    if { ! [regexp -- {^(-?\d+)$} $n -> n]} {
        error "not a decimal integer"
    }
    if {$n == 0} {
        return zero
    }
    if {$n < 0} {
        return "negative [int2words [expr {abs($n)}]]"
    }
    if {[string length $n] > 36} {
        error "value too large to represent"
    }

    set groups [get_groups $n]
    set l [llength $groups]
    foreach group $groups {
        incr l -1
        # ensure any group with a leading zero is not treated as octal
        set val [scan $group %d]
        if {$val > 0} {
            lappend result [group2words $val $l]
        }
    }
    return [join $result ", "]
}

set small {"" one two three four five six seven eight nine ten eleven twelve
           thirteen fourteen fifteen sixteen seventeen eighteen nineteen}
set tens {"" "" twenty thirty forty fifty sixty seventy eighty ninety}
set powers {"" thousand}
foreach p {m b tr quadr quint sext sept oct non dec} {lappend powers ${p}illion}

proc group2words {n level} {
    global small tens powers
    if {$n < 20} {
        lappend result [lindex $small $n]
    } elseif {$n < 100} {
        lassign [divmod $n 10] a b
        set result [lindex $tens $a]
        if {$b > 0} {
            append result - [lindex $small $b]
        }
    } else {
        lassign [divmod $n 100] a b
        lappend result [lindex $small $a] hundred
        if {$b > 0} {
            lappend result and [group2words $b 0]
        }
    }
    return [join [concat $result [lindex $powers $level]]]
}

proc divmod {n d} {
    return [list [expr {$n / $d}] [expr {$n % $d}]]
}

proc get_groups {num} {
    # from http://wiki.tcl.tk/5000
    while {[regsub {^([-+]?\d+)(\d\d\d)} $num {\1 \2} num]} {}
    return [split $num]
}

foreach test {
        0 -0 5 -5 10 25 99 100 101 999 1000 1008 1010 54321 1234567890
        0x7F
        123456789012345678901234567890123456
        1234567890123456789012345678901234567
} {
    catch {int2words $test} result
    puts "$test -> $result"
}
```

produces
<div style="width:full;overflow:scroll">

```txt
0 -> zero
-0 -> zero
5 -> five
-5 -> negative five
10 -> ten
25 -> twenty-five
99 -> ninety-nine
100 -> one hundred
101 -> one hundred and one
999 -> nine hundred and ninety-nine
1000 -> one thousand
1008 -> one thousand, eight
1010 -> one thousand, ten
54321 -> fifty-four thousand, three hundred and twenty-one
1234567890 -> one billion, two hundred and thirty-four million, five hundred and sixty-seven thousand, eight hundred and ninety
0x7F -> not a decimal integer
123456789012345678901234567890123456 -> one hundred and twenty-three decillion, four hundred and fifty-six nonillion, seven hundred and eighty-nine octillion, twelve septillion, three hundred and forty-five sextillion, six hundred and seventy-eight quintillion, nine hundred and one quadrillion, two hundred and thirty-four trillion, five hundred and sixty-seven billion, eight hundred and ninety million, one hundred and twenty-three thousand, four hundred and fifty-six
1234567890123456789012345678901234567 -> value too large to represent

```

</div>


## Visual Basic

If one were to use variants further and get them to play nice as <code>Decimal</code>, this could theoretically be extended up to the octillion range.

```vb
Option Explicit

Private small As Variant, tens As Variant, big As Variant

Sub Main()
    small = Array("one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", _
                  "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", _
                  "eighteen", "nineteen")
    tens = Array("twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety")
    big = Array("thousand", "million", "billion")

    Dim tmpInt As Long
    tmpInt = Val(InputBox("Gimme a number!", "NOW!", Trim$(Year(Now)) & IIf(Month(Now) < 10, "0", "") & _
                 Trim$(Month(Now)) & IIf(Day(Now) < 10, "0", "") & Trim$(Day(Now))))
    MsgBox int2Text$(tmpInt)
End Sub

Function int2Text$(number As Long)
    Dim num As Long, outP As String, unit As Integer
    Dim tmpLng1 As Long

    If 0 = number Then
        int2Text$ = "zero"
        Exit Function
    End If

    num = Abs(number)

    Do
        tmpLng1 = num Mod 100
        Select Case tmpLng1
            Case 1 To 19
                outP = small(tmpLng1 - 1) + " " + outP
            Case 20 To 99
                Select Case tmpLng1 Mod 10
                    Case 0
                        outP = tens((tmpLng1 \ 10) - 2) + " " + outP
                    Case Else
                        outP = tens((tmpLng1 \ 10) - 2) + "-" + small(tmpLng1 Mod 10) + " " + outP
                End Select
        End Select

        tmpLng1 = (num Mod 1000) \ 100
        If tmpLng1 Then
            outP = small(tmpLng1 - 1) + " hundred " + outP
        End If

        num = num \ 1000
        If num < 1 Then Exit Do

        tmpLng1 = num Mod 1000
        If tmpLng1 Then outP = big(unit) + " " + outP

        unit = unit + 1
    Loop

    If number < 0 Then outP = "negative " & outP

    int2Text$ = Trim$(outP)
End Function
```


Example output (in a msgbox) is identical to the BASIC output.


## VBA

```vb
Public twenties As Variant
Public decades As Variant
Public orders As Variant

Private Sub init()
    twenties = [{"zero","one","two","three","four","five","six","seven","eight","nine","ten", "eleven","twelve","thirteen","fourteen","fifteen","sixteen","seventeen","eighteen","nineteen"}]
    decades = [{"twenty","thirty","forty","fifty","sixty","seventy","eighty","ninety"}]
    orders = [{1E15,"quadrillion"; 1E12,"trillion"; 1E9,"billion"; 1E6,"million"; 1E3,"thousand"}]
End Sub

Private Function Twenty(N As Variant)
    Twenty = twenties(N Mod 20 + 1)
End Function

Private Function Decade(N As Variant)
    Decade = decades(N Mod 10 - 1)
End Function

Private Function Hundred(N As Variant)
    If N < 20 Then
        Hundred = Twenty(N)
        Exit Function
    Else
        If N Mod 10 = 0 Then
            Hundred = Decade((N \ 10) Mod 10)
            Exit Function
        End If
    End If
    Hundred = Decade(N \ 10) & "-" & Twenty(N Mod 10)
End Function

Private Function Thousand(N As Variant, withand As String)
    If N < 100 Then
        Thousand = withand & Hundred(N)
        Exit Function
    Else
        If N Mod 100 = 0 Then
            Thousand = withand & Twenty(WorksheetFunction.Floor_Precise(N / 100)) & " hundred"
            Exit Function
        End If
    End If
    Thousand = Twenty(N \ 100) & " hundred and " & Hundred(N Mod 100)
End Function

Private Function Triplet(N As Variant)
    Dim Order, High As Variant, Low As Variant
    Dim Name As String, res As String
    For i = 1 To UBound(orders)
        Order = orders(i, 1)
        Name = orders(i, 2)
        High = WorksheetFunction.Floor_Precise(N / Order)
        Low = N - High * Order 'N Mod Order
        If High <> 0 Then
            res = res & Thousand(High, "") & " " & Name
        End If
        N = Low
        If Low = 0 Then Exit For
        If Len(res) And High <> 0 Then
            res = res & ", "
        End If
    Next i
    If N <> 0 Or res = "" Then
        res = res & Thousand(WorksheetFunction.Floor_Precise(N), IIf(res = "", "", "and "))
        N = N - Int(N)
        If N > 0.000001 Then
            res = res & " point"
            For i = 1 To 10
                n_ = WorksheetFunction.Floor_Precise(N * 10.0000001)
                res = res & " " & twenties(n_ + 1)
                N = N * 10 - n_
                If Abs(N) < 0.000001 Then Exit For
            Next i
        End If
    End If
    Triplet = res
End Function

Private Function spell(N As Variant)
    Dim res As String
    If N < 0 Then
        res = "minus "
        N = -N
    End If
    res = res & Triplet(N)
    spell = res
End Function

Private Function smartp(N As Variant)
    Dim res As String
    If N = WorksheetFunction.Floor_Precise(N) Then
        smartp = CStr(N)
        Exit Function
    End If
    res = CStr(N)
    If InStr(1, res, ".") Then
        res = Left(res, InStr(1, res, "."))
    End If
    smartp = res
End Function

Sub Main()
    Dim si As Variant
    init
    Samples1 = [{99, 300, 310, 417, 1501, 12609, 200000000000100, 999999999999999, -123456787654321,102003000400005,1020030004,102003,102,1,0,-1,-99, -1501,1234,12.34}]
    Samples2 = [{10000001.2,1E-3,-2.7182818, 201021002001,-20102100200,2010210020,-201021002,20102100,-2010210, 201021,-20102,2010,-201,20,-2}]
    For i = 1 To UBound(Samples1)
        si = Samples1(i)
        Debug.Print Format(smartp(si), "@@@@@@@@@@@@@@@@"); " "; spell(si)
    Next i
    For i = 1 To UBound(Samples2)
        si = Samples2(i)
        Debug.Print Format(smartp(si), "@@@@@@@@@@@@@@@@"); " "; spell(si)
    Next i
End Sub
```
```txt
              99 ninety-nine
             300 three hundred
             310 three hundred and ten
             417 four hundred and seventeen
            1501 one thousand, five hundred and one
           12609 twelve thousand, six hundred and nine
 200000000000100 two hundred trillion, and one hundred
 999999999999999 nine hundred and ninety-nine trillion, nine hundred and ninety-nine billion, nine hundred and ninety-nine million, nine hundred and ninety-nine thousand, nine hundred and ninety-nine
-123456787654321 minus one hundred and twenty-three trillion, four hundred and fifty-six billion, seven hundred and eighty-seven million, six hundred and fifty-four thousand, three hundred and twenty-one
 102003000400005 one hundred and two trillion, three billion, four hundred thousand, and five
      1020030004 one billion, twenty million, thirty thousand, and four
          102003 one hundred and two thousand, and three
             102 one hundred and two
               1 one
               0 zero
              -1 minus one
             -99 minus ninety-nine
           -1501 minus one thousand, five hundred and one
            1234 one thousand, two hundred and thirty-four
           12,34 twelve point three four
      10000001,2 ten million, and one point two
           0,001 zero point zero zero one
      -2,7182818 minus two point seven one eight two eight one eight
    201021002001 two hundred and one billion, twenty-one million, two thousand, and one
    -20102100200 minus twenty billion, one hundred and two million, one hundred thousand, and two hundred
      2010210020 two billion, ten million, two hundred and ten thousand, and twenty
      -201021002 minus two hundred and one million, twenty-one thousand, and two
        20102100 twenty million, one hundred and two thousand, and one hundred
        -2010210 minus two million, ten thousand, two hundred and ten
          201021 two hundred and one thousand, and twenty-one
          -20102 minus twenty thousand, one hundred and two
            2010 two thousand, and ten
            -201 minus two hundred and one
              20 twenty
              -2 minus two
```


## Visual Basic .NET

'''Platform:''' [[.NET]]

This solution works for integers up to 1000. It should be fairly ovbious how it works, and so can be extended if needed.


```vbnet
Module Module1

    Sub Main()
        Dim i As Integer
        Console.WriteLine("Enter a number")
        i = Console.ReadLine()
        Console.WriteLine(words(i))
        Console.ReadLine()
    End Sub

    Function words(ByVal Number As Integer) As String
        Dim small() As String = {"zero", "one", "two", "three", "four", "five", "six", "seven", "eight",
         "nine", "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen",
         "eighteen", "nineteen"}
        Dim tens() As String = {"", "", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"}
        Select Case Number
            Case Is < 20
                words = small(Number)
            Case 20 To 99
                words = tens(Number \ 10) + " " + small(Number Mod 10)
            Case 100 To 999
                words = small(Number \ 100) + " hundred " + IIf(((Number Mod 100) <> 0), "and ", "") + words(Number Mod 100)
            Case 1000
                words = "one thousand"
        End Select
    End Function

End Module
```



## XPL0


```XPL0
code ChOut=8, CrLf=9, Text=12;

proc NumName(Dev, Num); \Output integer Num in prose to device Dev
int  Dev, Num;
int  OneTbl, TenTbl, ThoTbl, ThoPwr, I, Quot;

        proc Out999(N); \Output number in range 0..999 (0 does nothing)
        int  N;
        int  Huns, Tens, Ones;
        [Huns:= N/100;                          \0..9
        N:= rem(0);                             \0..99
        Tens:= N/10;                            \0..9
        Ones:= rem(0);                          \0..9
        if Huns # 0 then
                [Text(Dev, OneTbl(Huns));       \1..9
                Text(Dev, " hundred ")];
        if Tens >= 2 then
                [Text(Dev, TenTbl(Tens));
                if Ones # 0 then
                        [ChOut(Dev, ^-);  Text(Dev, OneTbl(Ones))];
                ]
        else    if N # 0 then Text(Dev, OneTbl(N));     \N = 1..19
        ];

[if Num = 0 then [Text(Dev, "zero");  return];
if Num < 0 then [Num:= -Num;  Text(Dev, "minus ")];

OneTbl:=[0, "one", "two", "three", "four",
        "five", "six", "seven", "eight", "nine",
        "ten", "eleven", "twelve", "thirteen", "fourteen",
        "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"];
TenTbl:=[0, 0, "twenty", "thirty", "forty",
        "fifty", "sixty", "seventy", "eighty", "ninety"];
ThoTbl:=[" billion ", " million ", " thousand "];

ThoPwr:= 1000000000;
for I:= 0 to 2 do
        [Quot:= Num/ThoPwr;
        Num:= rem(0);
        if Quot # 0 then
                [Out999(Quot);  Text(Dev, ThoTbl(I))];
        ThoPwr:= ThoPwr/1000;
        ];
Out999(Num);
];

[NumName(0, 0);  CrLf(0);
NumName(0, 13);  CrLf(0);
NumName(0, 789); CrLf(0);
NumName(0,  -604_001); CrLf(0);
NumName(0, 1_000_000); CrLf(0);
NumName(0, 1_234_567_890); CrLf(0);
]
```


```txt

zero
thirteen
seven hundred eighty-nine
minus six hundred four thousand one
one million
one billion two hundred thirty-four million five hundred sixty-seven thousand eight hundred ninety

```



## zkl

```zkl
var
   ns   =[1..20].chain([30..90,10]).walk(),
   names=T("one","two","three","four","five","six","seven","eight","nine",
           "ten","eleven","twelve","thirteen","fourteen","fifteen","sixteen",
           "seventeen","eighteen","nineteen","twenty",
	   "thirty","forty","fifty","sixty","seventy","eighty","ninety"),
   hs    =T(  100,     1000,      1000000, 1000000000,1000000000000),
   hnames=T("hundred","thousand","million","billion", "trillion");

fcn numberToString(n){ // n>0
   fcn(n){
      if(100<=n<0d100_000_0000_000){
	 idx,h,name,r := hs.filter1n('>(n))-1, hs[idx], hnames[idx], n%h;
	 String(self.fcn(n/h),name,
	    if(r==0) "" else if(0<r<100) " and " else ", ",
	    self.fcn(r));
      }else if(0<n<=90){
	 idx,t,name,r := ns.filter1n('>(n))-1, ns[idx], names[idx], n-t;
	 String(name, if(0<r<10) "-" else " ", self.fcn(r));
      }else ""
   }(n).strip()  // sometimes there is a trailing space
}
```


```zkl
foreach n in (T(85001,155019,4547000,6766027,55555555555)){
   println("%,d is %s".fmt(n,numberToString(n)));
}
```

```txt

85,001 is eighty-five thousand and one
155,019 is one hundred and fifty-five thousand and nineteen
4,547,000 is four million, five hundred and forty-seven thousand
6,766,027 is six million, seven hundred and sixty-six thousand and twenty-seven
55,555,555,555 is fifty-five billion, five hundred and fifty-five million, five hundred and fifty-five thousand, five hundred and fifty-five

```

