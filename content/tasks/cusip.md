+++
title = "CUSIP"
description = ""
date = 2019-09-19T16:53:08Z
aliases = []
[extra]
id = 21307
[taxonomies]
categories = ["task", "Checksums"]
tags = []
languages = [
  "360_assembly",
  "ada",
  "algol_68",
  "algol_w",
  "awk",
  "c",
  "clojure",
  "common_lisp",
  "cpp",
  "csharp",
  "d",
  "dyalect",
  "factor",
  "fortran",
  "freebasic",
  "go",
  "haskell",
  "j",
  "java",
  "julia",
  "kotlin",
  "langur",
  "lua",
  "nim",
  "objeck",
  "perl",
  "perl_6",
  "phix",
  "picolisp",
  "python",
  "racket",
  "rexx",
  "ring",
  "ruby",
  "rust",
  "scala",
  "snobol4",
  "swift",
  "tcl",
  "vba",
  "visual_basic_dotnet",
  "yabasic",
  "zkl",
]
+++

<br/>

A   '''CUSIP'''   is a nine-character alphanumeric code that identifies a North American financial security for the purposes of facilitating clearing and settlement of trades. The CUSIP was adopted as an American National Standard under Accredited Standards X9.6.


## Task

Ensure the last digit   (i.e., the   ''check digit'')   of the '''CUSIP''' code (the 1<sup>st</sup> column) is correct, against the following:
*    037833100           Apple Incorporated
*    17275R102           Cisco Systems
*    38259P508           Google Incorporated
*    594918104           Microsoft Corporation
*    68389X106           Oracle Corporation   (''incorrect'')
*    68389X105           Oracle Corporation


;Example pseudo-code below.
<lang>algorithm Cusip-Check-Digit(cusip) is
   Input: an 8-character CUSIP

   sum := 0
   for 1 ≤ i ≤ 8 do
      c := the ith character of cusip
      if c is a digit then
         v := numeric value of the digit c
      else if c is a letter then
         p := ordinal position of c in the alphabet (A=1, B=2...)
         v := p + 9
      else if c = "*" then
         v := 36
      else if c = "@" then
         v := 37
      else if' c = "#" then
         v := 38
      end if
      if i is even then
         v := v × 2
      end if

      sum := sum + int ( v div 10 ) + v mod 10
   repeat

   return (10 - (sum mod 10)) mod 10
end function
```


;See related tasks:
* [[SEDOLs|SEDOL]]
* [[Validate_International_Securities_Identification_Number|ISIN]]





## 360 Assembly


```360asm
*        CUSIP                     07/06/2018
CUSIP    CSECT
         USING  CUSIP,R13          base register
         B      72(R15)            skip savearea
         DC     17F'0'             savearea
         SAVE   (14,12)            save previous context
         ST     R13,4(R15)         link backward
         ST     R15,8(R13)         link forward
         LR     R13,R15            set addressability
         LA     R6,1               i=1
       DO WHILE=(C,R6,LE,=F'6')    do i=1 to 6
         LR     R1,R6                i
         MH     R1,=H'9'             *9
         LA     R4,T-9(R1)           @t(i)
         MVC    X,0(R4)              x=t(i)
         SR     R10,R10              w=0
         LA     R7,1                 j=1
       DO WHILE=(C,R7,LE,=F'8')      do j=1 to 8
         LA     R14,X-1                x
         AR     R14,R7                 j
         MVC    Y(1),0(R14)            y=substr(x,j,1)
         LA     R9,L'XX                z=length(xx)
         LA     R8,1                   k=1
       DO WHILE=(C,R8,LE,=A(L'XX))     do k=1 to length(xx)
         LA     R4,XX-1                  xx
         AR     R4,R8                    k
         MVC    C(1),0(R4)               c=substr(xx,k,1)
       IF CLC,Y(1),EQ,C THEN             if y=c then
         LR     R9,R8                      k
         BCTR   R9,0                       z=k-1
       ENDIF    ,                        endif
         LA     R8,1(R8)                 k++
       ENDDO    ,                      enddo k
         LR     R4,R7                  j
         LA     R1,2                   2
         SRDA   R4,32                  ~
         DR     R4,R1                  j/2=0
       IF LTR,R4,Z,R4 THEN             if j//2=0 then
         AR     R9,R9                    z=z+z
       ENDIF    ,                      endif
         LR     R4,R9                  z
         LA     R1,10                  10
         SRDA   R4,32                  ~
         DR     R4,R1                  r4=z//10 ; r5=z/10
         AR     R10,R5                 w+z/10
         AR     R10,R4                 w=w+z/10+z//10
         LA     R7,1(R7)               j++
       ENDDO    ,                    enddo j
         LR     R4,R10               w
         LA     R1,10                10
         SRDA   R4,32                ~
         DR     R4,R1                w/10
         LA     R2,10                10
         SR     R2,R4                10-w//10
         SRDA   R2,32                ~
         DR     R2,R1                /10
         STC    R2,U                 u=(10-w//10)//10
         OI     U,X'F0'              bin to char
       IF CLC,U,EQ,X+8 THEN          if u=substr(x,9,1) then
         MVC    OK,=CL3' '             ok=' '
       ELSE     ,                    else
         MVC    OK,=C'n''t'            ok='n''t'
       ENDIF    ,                    endif
         MVC    PG+6(9),X            output x
         MVC    PG+18(3),OK          output ok
         XPRNT  PG,L'PG              print
         LA     R6,1(R6)             i++
       ENDDO    ,                  enddo i
         L      R13,4(0,R13)       restore previous savearea pointer
         RETURN (14,12),RC=0       restore registers from calling sav
XX       DC     CL39'0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ*@#'
U        DS     CL1
Y        DS     CL1
C        DS     CL1
T        DC     CL9'037833100',CL9'17275R102',CL9'38259P508'
         DC     CL9'594918104',CL9'68389X106',CL9'68389X105'
X        DS     CL9
OK       DS     CL3
PG       DC     CL80'CUSIP ......... is... valid'
         YREGS
         END    CUSIP
```

```txt

CUSIP 037833100 is    valid
CUSIP 17275R102 is    valid
CUSIP 38259P508 is    valid
CUSIP 594918104 is    valid
CUSIP 68389X106 isn't valid
CUSIP 68389X105 is    valid

```



## Ada


```Ada
with Ada.Text_IO;

procedure Cusip_Test is
   use Ada.Text_IO;

   subtype Cusip is String (1 .. 9);

   function Check_Cusip (Code : Cusip) return Boolean is
      Sum : Integer := 0;
      V   : Integer;

   begin
      for I in Code'First .. Code'Last - 1 loop
         case Code (I) is
            when '0' .. '9' =>
               V := Character'Pos (Code (I)) - Character'Pos ('0');
            when 'A' .. 'Z' =>
               V := Character'Pos (Code (I)) - Character'Pos ('A') + 10;
            when '*' => V := 36;
            when '@' => V := 37;
            when '#' => V := 38;
            when others => return False;
         end case;

         if I mod 2 = 0 then
            V := V * 2;
         end if;

         Sum := Sum + V / 10 + (V mod 10);
      end loop;

      return (10 - (Sum mod 10)) mod 10 =
        Character'Pos (Code (Code'Last)) - Character'Pos ('0');
   end Check_Cusip;

   type Cusip_Array is array (Natural range <>) of Cusip;

   Test_Array : Cusip_Array :=
     ("037833100",
      "17275R102",
      "38259P508",
      "594918104",
      "68389X106",
      "68389X105");
begin
   for I in Test_Array'Range loop
      Put (Test_Array (I) & ": ");
      if Check_Cusip (Test_Array (I)) then
         Put_Line ("valid");
      else
         Put_Line ("not valid");
      end if;
   end loop;
end Cusip_Test;
```

```txt
037833100: valid
17275R102: valid
38259P508: valid
594918104: valid
68389X106: not valid
68389X105: valid

```



## ALGOL 68


```algol68
BEGIN
    # returns TRUE if cusip is a valid CUSIP code #
    OP ISCUSIP = ( STRING cusip )BOOL:
       IF ( UPB cusip - LWB cusip ) /= 8
       THEN
           # code is wrong length #
           FALSE
       ELSE
           # string is 9 characters long - check it is valid #
           STRING cusip digits = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ*@#"[ AT 0 ];
           INT check digit := 0;
           IF NOT char in string( cusip[ UPB cusip ], check digit, cusip digits )
           THEN
               # invalid check digit #
               FALSE
           ELSE
               # OK so far compare the calculated check sum to the supplied one #
               INT sum := 0;
               INT c pos := LWB cusip - 1;
               FOR i TO 8 DO
                   INT digit := 0;
                   IF NOT char in string( cusip[ i + c pos ], digit, cusip digits )
                   THEN
                       # invalid digit #
                       digit := -999
                   FI;
                   IF NOT ODD i
                   THEN
                       # even digit #
                       digit *:= 2
                   FI;
                   sum +:= ( digit OVER 10 ) + ( digit MOD 10 )
               OD;
               ( 10 - ( sum MOD 10 ) ) MOD 10 = check digit
           FI
       FI ; # ISCUSIP #

    # task test cases #

    PROC test cusip = ( STRING cusip )VOID:
        print( ( cusip, IF ISCUSIP cusip THEN " valid" ELSE " invalid" FI, newline ) );

    test cusip( "037833100" );
    test cusip( "17275R102" );
    test cusip( "38259P508" );
    test cusip( "594918104" );
    test cusip( "68389X106" );
    test cusip( "68389X105" )
END
```

```txt

037833100 valid
17275R102 valid
38259P508 valid
594918104 valid
68389X106 invalid
68389X105 valid

```



## ALGOL W

Based on Algol 68

```algolw
begin    % returns true if cusip is a valid CUSIP code %
    logical procedure isCusip ( string(9) value cusip ) ;
    begin
        % returns the base 39 digit corresponding to a character of a CUSIP code %
        integer procedure cusipDigit( string(1) value cChar ) ;
            if      cChar >= "0" and cChar <= "9" then ( decode( cChar ) - decode( "0" ) )
            else if cChar >= "A" and cChar <= "Z" then ( decode( cChar ) - decode( "A" ) ) + 10
            else if cChar  = "*"                  then   36
            else if cChar  = "@"                  then   37
            else if cChar  = "#"                  then   38
            else    % invalid digit %                  -999 ;

        integer checkDigit, sum;
        checkDigit := cusipDigit( cusip( 8 // 1 ) );
        for cPos := 1 until 8 do begin
            integer   digit;
            digit := cusipDigit( cusip( ( cPos - 1 ) // 1 ) );
            if not odd( cPos ) then digit := digit * 2;
            sum := sum + ( digit div 10 ) + ( digit rem 10 )
        end for_cPos ;
        ( ( 10 - ( sum rem 10 ) ) rem 10 ) = checkDigit
    end isCusip ;

    begin % task test cases %
        procedure testCusip ( string(9) value cusip ) ;
            write( s_w := 0, cusip, if isCusip( cusip ) then " valid" else " invalid" );

        testCusip( "037833100" );
        testCusip( "17275R102" );
        testCusip( "38259P508" );
        testCusip( "594918104" );
        testCusip( "68389X106" );
        testCusip( "68389X105" )
    end testCases
end.
```

```txt

037833100 valid
17275R102 valid
38259P508 valid
594918104 valid
68389X106 invalid
68389X105 valid

```


## AWK


```AWK

# syntax: GAWK -f CUSIP.AWK
BEGIN {
    n = split("037833100,17275R102,38259P508,594918104,68389X106,68389X105",arr,",")
    for (i=1; i<=n; i++) {
      printf("%9s %s\n",arr[i],cusip(arr[i]))
    }
    exit(0)
}
function cusip(n,  c,i,sum,v,x) {
# returns: 1=OK, 0=NG, -1=bad data
    if (length(n) != 9) {
      return(-1)
    }
    for (i=1; i<=8; i++) {
      c = substr(n,i,1)
      if (c ~ /[0-9]/) {
        v = c
      }
      else if (c ~ /[A-Z]/) {
        v = index("ABCDEFGHIJKLMNOPQRSTUVWXYZ",c) + 9
      }
      else if (c == "*") {
        v = 36
      }
      else if (c == "@") {
        v = 37
      }
      else if (c == "#") {
        v = 38
      }
      else {
        return(-1)
      }
      if (i ~ /[02468]/) {
        v *= 2
      }
      sum += int(v / 10) + (v % 10)
    }
    x = (10 - (sum % 10)) % 10
    return(substr(n,9,1) == x ? 1 : 0)
}

```

```txt

037833100 1
17275R102 1
38259P508 1
594918104 1
68389X106 0
68389X105 1

```



## C

Reads CUSIP strings from a file and prints results to console, usage printed on incorrect invocation.

```C

#include<stdlib.h>
#include<stdio.h>

int cusipCheck(char str[10]){
	int sum=0,i,v;

	for(i=0;i<8;i++){
		if(str[i]>='0'&&str[i]<='9')
			v = str[i]-'0';
		else if(str[i]>='A'&&str[i]<='Z')
			v = (str[i] - 'A' + 10);
		else if(str[i]=='*')
			v = 36;
		else if(str[i]=='@')
			v = 37;
		else if(str[i]=='#')
			v = 38;
		if(i%2!=0)
			v*=2;

		sum += ((int)(v/10) + v%10);
	}
	return ((10 - (sum%10))%10);
}

int main(int argC,char* argV[])
{
	char cusipStr[10];

	int i,numLines;

	if(argC==1)
		printf("Usage : %s <full path of CUSIP Data file>",argV[0]);

	else{
		FILE* fp = fopen(argV[1],"r");

		fscanf(fp,"%d",&numLines);

		printf("CUSIP       Verdict\n");
		printf("-------------------");

		for(i=0;i<numLines;i++){

			fscanf(fp,"%s",cusipStr);

			printf("\n%s : %s",cusipStr,(cusipCheck(cusipStr)==(cusipStr[8]-'0'))?"Valid":"Invalid");
		}

		fclose(fp);
	}
	return 0;
}

```

Input file :

```txt

6
037833100
17275R102
38259P508
594918104
68389X106
68389X105

```

Invocation and output :

```txt

C:\rosettaCode>cusipCheck.exe cusipData.txt
CUSIP       Verdict
-------------------
037833100 : Valid
17275R102 : Valid
38259P508 : Valid
594918104 : Valid
68389X106 : Invalid
68389X105 : Valid

```



## C++

```cpp
#include <iostream>
#include <vector>

bool isCusip(const std::string& s) {
    if (s.size() != 9) return false;

    int sum = 0;
    for (int i = 0; i <= 7; ++i) {
        char c = s[i];

        int v;
        if ('0' <= c && c <= '9') {
            v = c - '0';
        } else if ('A' <= c && c <= 'Z') {
            v = c - '@';
        } else if (c = '*') {
            v = 36;
        } else if (c = '#') {
            v = 38;
        } else {
            return false;
        }
        if (i % 2 == 1) {
            v *= 2;
        }
        sum += v / 10 + v % 10;
    }
    return s[8] - '0' == (10 - (sum % 10)) % 10;
}

int main() {
    using namespace std;

    vector<string> candidates{
        "037833100",
        "17275R102",
        "38259P508",
        "594918104",
        "68389X106",
        "68389X105"
    };

    for (auto str : candidates) {
        auto res = isCusip(str) ? "correct" : "incorrect";
        cout << str.c_str() << " -> " << res << "\n";
    }

    return 0;
}
```

```txt
037833100 -> correct
17275R102 -> correct
38259P508 -> correct
594918104 -> correct
68389X106 -> incorrect
68389X105 -> correct
```


## C#
```c#
using System;
using System.Collections.Generic;

namespace CUSIP {
    class Program {
        static bool IsCusip(string s) {
            if (s.Length != 9) return false;
            int sum = 0;
            for (int i = 0; i <= 7; i++) {
                char c = s[i];

                int v;
                if (c >= '0' && c <= '9') {
                    v = c - 48;
                }
                else if (c >= 'A' && c <= 'Z') {
                    v = c - 64;  // lower case letters apparently invalid
                }
                else if (c == '*') {
                    v = 36;
                }
                else if (c == '#') {
                    v = 38;
                }
                else {
                    return false;
                }
                if (i % 2 == 1) v *= 2;  // check if odd as using 0-based indexing
                sum += v / 10 + v % 10;
            }
            return s[8] - 48 == (10 - (sum % 10)) % 10;
        }

        static void Main(string[] args) {
            List<string> candidates = new List<string>() {
                "037833100",
                "17275R102",
                "38259P508",
                "594918104",
                "68389X106",
                "68389X105"
            };
            foreach (var candidate in candidates) {
                Console.WriteLine("{0} -> {1}", candidate, IsCusip(candidate) ? "correct" : "incorrect");
            }
        }
    }
}
```

```txt
037833100 -> correct
17275R102 -> correct
38259P508 -> correct
594918104 -> correct
68389X106 -> incorrect
68389X105 -> correct
```


=={{header|Caché ObjectScript}}==


```cos
Class Utils.Check [ Abstract ]
{

ClassMethod CUSIP(x As %String) As %Boolean
{
	SET x=$TRANSLATE(x," ")
	// https://leiq.bus.umich.edu/res_codes_cusip.htm
	IF x'?8UNP1N QUIT 0
	SET cd=$EXTRACT(x,*), x=$EXTRACT(x,1,*-1), t=0
	FOR i=1:1:$LENGTH(x) {
		SET n=$EXTRACT(x,i)
		IF n'=+n SET n=$CASE(n,"*":36,"@":37,"#":38,:$ASCII(n)-55)
		IF i#2=0 SET n=n*2
		SET t=t+(n\10)+(n#10)
	}
	QUIT cd=((10-(t#10))#10)
}

}
```

```txt
USER>For  { Read s Quit:s=""  Write ": "_##class(Utils.Check).CUSIP(s), ! }
037833100: 1
17275R102: 1
38259P508: 1
594918104: 1
68389X106: 0
68389X105: 1

USER>
```



## Clojure


```Clojure

(defn- char->value
  "convert the given char c to a value used to calculate the cusip check sum"
  [c]
  (let [int-char (int c)]
    (cond
      (and (>= int-char (int \0)) (<= int-char (int \9))) (- int-char 48)
      (and (>= int-char (int \A)) (<= int-char (int \Z))) (- int-char 64)
      (= c \*) 36
      (= c \@) 37
      (= c \#) 38
      :else nil)))

(defn- calc-sum
  "Calculate cusip sum. nil is returned for an invalid cusip."
  [cusip]
  (reduce
    (fn [sum [i c]]
      (if-let [v (char->value c)]
        (let [v (if (= (mod i 2) 1) (* v 2) v)]
          (+ sum (int (+ (/ v 10) (mod v 10)))))
        (reduced nil)))
    0
    (map-indexed vector (subs cusip 0 8))))

(defn calc-cusip-checksum
  "Given a valid 8 or 9 digit cusip, return the 9th checksum digit"
  [cusip]
  (when (>= (count cusip) 8)
    (let [sum (calc-sum cusip)]
      (when sum
        (mod (- 10 (mod sum 10)) 10)))))

(defn is-valid-cusip9?
  "predicate validating a 9 digit cusip."
  [cusip9]
  (when-let [checksum (and (= (count cusip9) 9)
                           (calc-cusip-checksum cusip9))]
    (= (- (int (nth cusip9 8)) 48)
       checksum)))

(defn rosetta-output
  "show some nice output for the Rosetta Wiki"
  []
  (doseq [cusip ["037833100" "17275R102" "38259P508" "594918104" "68389X106" "68389X105"
                 "EXTRACRD8" "BADCUSIP!" "683&9X106" "68389x105" "683$9X106" "68389}105"]]
    (println cusip (if (is-valid-cusip9? cusip) "valid" "invalid"))))

```


```txt

(rosetta-output)
037833100 valid
17275R102 valid
38259P508 valid
594918104 valid
68389X106 invalid
68389X105 valid
EXTRACRD8 valid
BADCUSIP! invalid
683&9X106 invalid
68389x105 invalid
683$9X106 invalid
68389}105 invalid

```



## Common Lisp


```lisp
(defun char->value (c)
  (cond ((digit-char-p c 36))
        ((char= c #\*) 36)
        ((char= c #\@) 37)
        ((char= c #\#) 38)
        (t (error "Invalid character: ~A" c))))

(defun cusip-p (cusip)
  (and (= 9 (length cusip))
       (loop for i from 1 to 8
             for c across cusip
             for v = (char->value c)
             when (evenp i)
               do (setf v (* 2 v))
             sum (multiple-value-bind (quot rem) (floor v 10)
                   (+ quot rem))
               into sum
             finally (return (eql (digit-char-p (char cusip 8))
                                  (mod (- 10 (mod sum 10)) 10))))))

(defun main ()
  (dolist (cusip '("037833100" "17275R102" "38259P508" "594918104" "68389X106" "68389X105"))
    (format t "~A: ~A~%" cusip (cusip-p cusip))))
```

```txt
037833100: T
17275R102: T
38259P508: T
594918104: T
68389X106: NIL
68389X105: T
```



## D


```D
import std.stdio;

void main(string[] args) {
    writeln("CUSIP       Verdict");
    foreach(arg; args[1..$]) {
        writefln("%9s : %s", arg, isValidCusip(arg) ? "Valid" : "Invalid");
    }
}

class IllegalCharacterException : Exception {
    this(string msg) {
        super(msg);
    }
}

bool isValidCusip(string cusip) in {
    assert(cusip.length == 9, "Incorrect cusip length");
} body {
    try {
        auto check = cusipCheckDigit(cusip);
        return cusip[8] == ('0' + check);
    } catch (IllegalCharacterException e) {
        return false;
    }
}

unittest {
    // Oracle Corporation
    assertEquals(isValidCusip("68389X105"), true);

    // Oracle Corporation (invalid)
    assertEquals(isValidCusip("68389X106"), false);
}

int cusipCheckDigit(string cusip) in {
    assert(cusip.length == 9, "Incorrect cusip length");
} body {
    int sum;
    for (int i=0; i<8; ++i) {
        char c = cusip[i];
        int v;

        switch(c) {
            case '0': .. case '9':
                v = c - '0';
                break;
            case 'A': .. case 'Z':
                v = c - 'A' + 10;
                break;
            case '*':
                v = 36;
                break;
            case '@':
                v = 37;
                break;
            case '#':
                v = 38;
                break;
            default:
                throw new IllegalCharacterException("Saw character: " ~ c);
        }
        if (i%2 == 1) {
            v = 2 * v;
        }

        sum = sum + (v / 10) + (v % 10);
    }

   return (10 - (sum % 10)) % 10;
}

unittest {
    // Apple Incorporated
    assertEquals(cusipCheckDigit("037833100"), 0);

    // Cisco Systems
    assertEquals(cusipCheckDigit("17275R102"), 2);

    // Google Incorporated
    assertEquals(cusipCheckDigit("38259P508"), 8);

    // Microsoft Corporation
    assertEquals(cusipCheckDigit("594918104"), 4);

    // Oracle Corporation
    assertEquals(cusipCheckDigit("68389X105"), 5);
}

version(unittest) {
    void assertEquals(T)(T actual, T expected) {
        import core.exception;
        import std.conv;
        if (actual != expected) {
            throw new AssertError("Actual [" ~ to!string(actual) ~ "]; Expected [" ~ to!string(expected) ~ "]");
        }
    }
}

/// Invoke with `cusip 037833100 17275R102 38259P508 594918104 68389X106 68389X105`
```


```txt
CUSIP       Verdict
037833100 : Valid
17275R102 : Valid
38259P508 : Valid
594918104 : Valid
68389X106 : Invalid
68389X105 : Valid
```



## Dyalect


```dyalect
func isCusip(s) {
    if s.len() != 9 { return false }
    var sum = 0
    for i in 0..7 {
        var c = s[i]
        var v =
            match c {
                '0'..'9' => Integer(c) - 48,
                'A'..'Z' => Integer(c) - 64,
                '*' => 36,
                '@' => 37,
                '#' => 38,
                _ => false
            }
        if i % 2 == 1 { v *= 2 }
        sum += v / 10 + v % 10
    }
    Integer(s[8]) - 48 == (10 - (sum % 10)) % 10
}

var candidates = [
    "037833100",
    "17275R102",
    "38259P508",
    "594918104",
    "68389X106",
    "68389X105"
]

for candidate in candidates {
    var b =
        if isCusip(candidate) {
            "correct"
        } else {
            "incorrect"
        }
    print("\(candidate) -> \(b)")
}
```


```txt
037833100 -> correct
17275R102 -> correct
38259P508 -> correct
594918104 -> correct
68389X106 -> incorrect
68389X105 -> correct
```



## Factor


```factor
USING: combinators.short-circuit formatting kernel math
math.parser qw regexp sequences unicode ;
IN: rosetta-code.cusip

: cusip-check-digit ( seq -- n )
    but-last-slice [
        [ dup alpha? [ digit> ] [ "*@#" index 36 + ] if ] dip
        odd? [ 2 * ] when 10 /mod +
    ] map-index sum 10 mod 10 swap - 10 mod ;

: cusip? ( seq -- ? )
    {
        [ R/ [0-9A-Z*@#]+/ matches? ]
        [ [ last digit> ] [ cusip-check-digit ] bi = ]
    } 1&& ;

qw{ 037833100 17275R102 38259P508 594918104 68389X106 68389X105 }
[ dup cusip? "correct" "incorrect" ? "%s -> %s\n" printf ] each
```

```txt

037833100 -> correct
17275R102 -> correct
38259P508 -> correct
594918104 -> correct
68389X106 -> incorrect
68389X105 -> correct

```



## Fortran

The key notion here is to employ a single sequence of valid characters, VALID, and for each character C of the code under test, use function INDEX(VALID,C) to find its position within that sequence, which turns out to be the desired ''v'' of the example pseudocode. The only slight difficulty is that INDEX starts its counting with one for the first character of VALID, which is zero, so one must be subtracted; similarly, to return a digit character code via indexing into VALID, one must be added. By using a list of valid characters rather than peculiar character arithmetic (such as ''c <= "9" & c >= "0"'' or similar) there is no reliance on the ASCII way of things. Recall that EBCDIC encodements have different orderings and notably, non-alphabetic characters between A and Z.

The source does not bother with the MODULE protocol of F90 and later, and so the type of function CUSIPCHECK must be declared in all routines wishing to invoke it. However, the F90 feature of having the END statement of a subroutine or function give its name is to valuable to ignore. The function returns a character code rather than an integer, since the presumption is that it is to be compared to the check character of the code being inspected, which is known as a character not an integer. This means some blather when extracting the eight characters to be presented to CUSIPCHECK and comparing the result to the ninth character, but the test can be done in one expression.

There is no checking that only valid characters are presented, nor that eight-character codes only are offered, though the compiler might complain if the function were to be invoked with a text literal of the wrong size. In the absence of such checks, there need be no added complications to support a scheme for reporting such errors.
```Fortran
      CHARACTER*1 FUNCTION CUSIPCHECK(TEXT)	!Determines the check sum character.
Committee on Uniform Security Identification Purposes, of the American (i.e. USA) Bankers' Association.
       CHARACTER*8 TEXT		!Specifically, an eight-symbol code.
       CHARACTER*(*) VALID	!These only are valid.
       PARAMETER (VALID = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ*@#")
       INTEGER I,V,S		!Assistants.
        S = 0		!Start the checksum.
        DO I = 1,LEN(TEXT)	!Step through the text.
          V = INDEX(VALID,TEXT(I:I)) - 1	!Since counting starts with one.
          IF (MOD(I,2).EQ.0) V = V*2		!V = V*(2 - MOD(I,2))?
          S = S + V/10 + MOD(V,10)		!Specified calculation.
        END DO			!On to the next character.
        I = MOD(10 - MOD(S,10),10) + 1	!Again, counting starts with one.
        CUSIPCHECK = VALID(I:I)	!Thanks to the MOD 10, surely a digit.
      END FUNCTION CUSIPCHECK	!No checking for invalid input...

      PROGRAM POKE	!Just to try it out.
      INTEGER I,N	!Assistants.
      PARAMETER (N = 6)		!A whole lot of blather
      CHARACTER*9 CUSIP(N)	!Just to have an array of test codes.
      DATA CUSIP/		!Here they are, as specified.
     1  "037833100",
     2  "17275R102",
     3  "38259P508",
     4  "594918104",
     5  "68389X106",
     6  "68389X105"/
      CHARACTER*1 CUSIPCHECK	!Needed as no use of the MODULE protocol.

      DO I = 1,N	!"More than two? Use a DO..."
        WRITE (6,*) CUSIP(I),CUSIPCHECK(CUSIP(I)(1:8)).EQ.CUSIP(I)(9:9)
      END DO

      END
```


Output: standard output is to I/O unit 6, and free-format (the *) will suffice for this. Each line output starts with a space (in case it is to go to a lineprinter, with carriage control), which is convenient for layout here.
 037833100 T
 17275R102 T
 38259P508 T
 594918104 T
 68389X106 F
 68389X105 T

This would have worked first time, except that a fymgre frmble caused the omission of the digit 2 from the text of VALID. The benefits of checking checksums reach to unexpected places!


## FreeBASIC


```freebasic
' version 04-04-2017
' compile with: fbc -s console

sub cusip(input_str As String)

    Print input_str;
    If Len(input_str) <> 9 Then
        Print " length is incorrect, invalid cusip"
        Return
    End If

    Dim As Long i, v , sum
    Dim As UByte x

    For i = 1 To 8
        x = input_str[i-1]
        Select Case x
            Case Asc("0") To Asc("9")
                v = x - Asc("0")
            Case Asc("A") To Asc("Z")
                v = x - Asc("A") + 1 + 9
            Case Asc("*")
                v= 36
            Case Asc("@")
                v = 37
            Case Asc("#")
                v = 38
            Case Else
                Print " found a invalid character, invalid cusip"
                return
        End Select

        If (i And 1) = 0 Then v = v * 2
        sum = sum + v \ 10 + v Mod 10
    Next

    sum = (10 - (sum Mod 10)) Mod 10
    If sum = (input_str[8] - Asc("0")) Then
        Print " is valid"
    Else
        Print " is invalid"
    End If

End Sub

' ------=< MAIN >=------

Data "037833100", "17275R102", "38259P508"
Data "594918104", "68389X106", "68389X105"

Dim As String input_str

Print
For i As Integer = 1 To 6
    Read input_str
    cusip(input_str)
Next

' empty keyboard buffer
While InKey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

```txt
037833100 is valid
17275R102 is valid
38259P508 is valid
594918104 is valid
68389X106 is invalid
68389X105 is valid
```



## Go


```go
package main

import "fmt"

func isCusip(s string) bool {
    if len(s) != 9 { return false }
    sum := 0
    for i := 0; i < 8; i++ {
        c := s[i]
        var v int
        switch {
            case c >= '0' && c <= '9':
                v = int(c) - 48
            case c >= 'A' && c <= 'Z':
                v = int(c) - 64
            case c == '*':
                v = 36
            case c == '@':
                v = 37
            case c == '#':
                v = 38
            default:
                return false
        }
        if i % 2 == 1 { v *= 2 }  // check if odd as using 0-based indexing
        sum += v/10 + v%10
    }
    return int(s[8]) - 48 == (10 - (sum%10)) % 10
}

func main() {
    candidates := []string {
        "037833100",
        "17275R102",
        "38259P508",
        "594918104",
        "68389X106",
        "68389X105",
    }

    for _, candidate := range candidates {
        var b string
        if isCusip(candidate) {
            b = "correct"
        } else {
            b = "incorrect"
        }
        fmt.Printf("%s -> %s\n", candidate, b)
    }
}

```


```txt

037833100 -> correct
17275R102 -> correct
38259P508 -> correct
594918104 -> correct
68389X106 -> incorrect
68389X105 -> correct

```



## Haskell


```haskell
import Data.List(elemIndex)

data Result = Valid | BadCheck | TooLong | TooShort | InvalidContent deriving Show

prependMaybe :: Maybe a -> Maybe [a] -> Maybe [a]
prependMaybe (Just v) (Just vs) = Just (v:vs)
prependMaybe _ _ = Nothing

-- convert a list of Maybe to a Maybe list.
-- result is Nothing if any of values from the original list are Nothing
allMaybe :: [Maybe a] -> Maybe [a]
allMaybe = foldr prependMaybe (Just [])

toValue :: Char -> Maybe Int
toValue c = elemIndex c $ ['0'..'9'] ++ ['A'..'Z'] ++ "*&#"

-- check a list of ints to see if they represent a valid CUSIP
valid :: [Int] -> Bool
valid ns0 =
    let -- multiply values with even index by 2
        ns1 = zipWith (\i n -> (if odd i then n else 2*n)) [1..] $ take 8 ns0

        -- apply div/mod formula from site and sum up results
        sm = sum $ fmap (\s -> ( s `div` 10 ) + s `mod` 10) ns1

    in  -- apply mod/mod formula from site and compare to last value in list
        ns0!!8 == (10 - (sm `mod` 10)) `mod` 10

-- check a String to see if it represents a valid CUSIP
checkCUSIP :: String -> Result
checkCUSIP cs
       | l < 9     = TooShort
       | l > 9     = TooLong
       | otherwise = case allMaybe (fmap toValue cs) of
                         Nothing -> InvalidContent
                         Just ns -> if valid ns then Valid else BadCheck
    where l = length cs

testData =
    [ "037833100"
    , "17275R102"
    , "38259P508"
    , "594918104"
    , "68389X106"
    , "68389X105"
    ]

main = mapM_ putStrLn (fmap (\s -> s ++ ": " ++ show (checkCUSIP s)) testData)
```


```txt
037833100: Valid
17275R102: Valid
38259P508: Valid
594918104: Valid
68389X106: BadCheck
68389X105: Valid

```


Or, making some alternative selections from Haskell's rich libraries:

```Haskell
import qualified Data.Map as M (Map, fromList, lookup)
import Control.Monad (sequence)
import Data.Maybe (fromMaybe)

cusipMap :: M.Map Char Int
cusipMap = M.fromList $ zip (['0' .. '9'] ++ ['A' .. 'Z'] ++ "*&#") [0 ..]

cusipValid :: String -> Bool
cusipValid s =
  let ns = (fromMaybe [] . sequence . fmap (`M.lookup` cusipMap)) s
  in (9 == length ns) &&
     let qrSum =
           sum
             ([quot, rem] <*> zipWith id (cycle [id, (* 2)]) (take 8 ns) <*>
              [10])
     in last ns == rem (10 - rem qrSum 10) 10

main :: IO ()
main =
  mapM_
    (print . ((,) <*> cusipValid))
    [ "037833100"
    , "17275R102"
    , "38259P508"
    , "594918104"
    , "68389X106"
    , "68389X105"
    ]
```

```txt
("037833100",True)
("17275R102",True)
("38259P508",True)
("594918104",True)
("68389X106",False)
("68389X105",True)
```


=={{header|Icon}} and {{header|Unicon}}==

```Icon
# cusip.icn -- Committee on Uniform Security Identification Procedures

procedure main()
   local code, codes
   codes := ["037833100", "17275R102", "38259P508",
            "594918104", "68389X106", "68389X105"]
   while code := pop(codes) do {
      writes(code, " : ")
      if check_code(code) then
         write("valid.")
      else write("not valid.")
      }
end

procedure check_code(c)
   local p, sum, value
   static codetable
   initial codetable := buildtable()

   sum   := 0
   value := 0
   every p := 1 to 8 do {
      if p % 2 = 1 then    # odd position
         value := codetable[c[p]]
      else                 # even position
         value := 2 * codetable[c[p]]
      sum +:= (value / 10) + (value % 10)
      }
   sum := (10 - (sum % 10)) % 10
   if sum = c[9] then return else fail
end

procedure buildtable()
   local chars, n, t
   t := table()
   chars := &digits || &ucase || "*@#"
   every n := 1 to *chars do
      t[chars[n]] := (n - 1)
   return t
end
```


```txt
037833100 : valid.
17275R102 : valid.
38259P508 : valid.
594918104 : valid.
68389X106 : not valid.
68389X105 : valid.

```



## J

One-liner:

```j
   ccd =. 10 | 10 - 10 | [: +/ [: , 10 (#.^:_1) (8 $ 1 2) * '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ*@#' i. ]
   ccd '68389X10'
5
```


More verbose version that checks for correct input:

```j
   CUSIPcheckdigit =. 3 : 0
assert. 8 = $ y NB. Only accept an 8-element long list
assert. */ y e. '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ*@#' NB. Only accept characters from the list of 38
values =. (8 $ 1 2) * '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ*@#' i. ] NB. Verb to translate characters and then double every second value.
sumdigits =. +/@,@(10 10&#:) NB. Verb to sum the base-10 digits in a numerical array
invertedmod =. 10 | 10 - 10 | ] NB. Verb to find the mod-10 of 10 minus mod-10
": invertedmod sumdigits values y NB. Return the check digit as a character
)
   addCUSIPcheckdigit =: , CUSIPcheckdigit
   verifyCUSIPcheckdigit =: {: = CUSIPcheckdigit@}:
```


Examples:

```j
   addCUSIPcheckdigit '68389X10'
68389X105
   verifyCUSIPcheckdigit '68389X106'
0
   verifyCUSIPcheckdigit '68389X105'
1
   samples =: '037833100', '17275R102', '38259P508', '594918104', '68389X106',: '68389X105'
   samples ; verifyCUSIPcheckdigit"1 samples
┌─────────┬─┐
│037833100│1│
│17275R102│1│
│38259P508│1│
│594918104│1│
│68389X106│0│
│68389X105│1│
└─────────┴─┘
```


## Java

Uses Java 9

```Java
import java.util.List;

public class Cusip {
    private static Boolean isCusip(String s) {
        if (s.length() != 9) return false;
        int sum = 0;
        for (int i = 0; i <= 7; i++) {
            char c = s.charAt(i);

            int v;
            if (c >= '0' && c <= '9') {
                v = c - 48;
            } else if (c >= 'A' && c <= 'Z') {
                v = c - 64;  // lower case letters apparently invalid
            } else if (c == '*') {
                v = 36;
            } else if (c == '@') {
                v = 37;
            } else if (c == '#') {
                v = 38;
            } else {
                return false;
            }
            if (i % 2 == 1) v *= 2;  // check if odd as using 0-based indexing
            sum += v / 10 + v % 10;
        }
        return s.charAt(8) - 48 == (10 - (sum % 10)) % 10;
    }

    public static void main(String[] args) {
        List<String> candidates = List.of(
                "037833100",
                "17275R102",
                "38259P508",
                "594918104",
                "68389X106",
                "68389X105"
        );
        for (String candidate : candidates) {
            System.out.printf("%s -> %s%n", candidate, isCusip(candidate) ? "correct" : "incorrect");
        }
    }
}
```

```txt
037833100 -> correct
17275R102 -> correct
38259P508 -> correct
594918104 -> correct
68389X106 -> incorrect
68389X105 -> correct

```



## Julia

```julia
module CUSIP

function _lastdigitcusip(input::AbstractString)
    input = uppercase(input)
    s = 0

    for (i, c) in enumerate(input)
        if isdigit(c)
            v = Int(c) - 48
        elseif isalpha(c)
            v = Int(c) - 64 + 9
        elseif c == '*'
            v = 36
        elseif c == '@'
            v = 37
        elseif c == '#'
            v = 38
        end

        if iseven(i); v *= 2 end
        s += div(v, 10) + rem(v, 10)
    end

    return Char(rem(10 - rem(s, 10), 10) + 48)
end

checkdigit(input::AbstractString) = input[9] == _lastdigitcusip(input[1:8])

end  # module CUSIP

for code in ("037833100", "17275R102", "38259P508", "594918104", "68389X106", "68389X105")
    println("$code is ", CUSIP.checkdigit(code) ? "correct." : "not correct.")
end
```


```txt
037833100 is correct.
17275R102 is correct.
38259P508 is correct.
594918104 is correct.
68389X106 is not correct.
68389X105 is correct.
```



## Kotlin


```scala
// version 1.1.0

fun isCusip(s: String): Boolean {
    if (s.length != 9) return false
    var sum = 0
    for (i in 0..7) {
        val c = s[i]
        var v = when (c) {
            in '0'..'9'  -> c.toInt() - 48
            in 'A'..'Z'  -> c.toInt() - 64  // lower case letters apparently invalid
            '*'          -> 36
            '@'          -> 37
            '#'          -> 38
            else         -> return false
        }
        if (i % 2 == 1) v *= 2  // check if odd as using 0-based indexing
        sum += v / 10 + v % 10
    }
    return s[8].toInt() - 48  == (10 - (sum % 10)) % 10
}

fun main(args: Array<String>) {
    val candidates = listOf(
        "037833100",
        "17275R102",
        "38259P508",
        "594918104",
        "68389X106",
        "68389X105"
    )
    for (candidate in candidates)
        println("$candidate -> ${if(isCusip(candidate)) "correct" else "incorrect"}")
}
```


```txt

037833100 -> correct
17275R102 -> correct
38259P508 -> correct
594918104 -> correct
68389X106 -> incorrect
68389X105 -> correct

```



## Langur


```Langur
val .isCusip = f(.s) {
    if not isString(.s) or len(.s) != 9 {
        return false
    }

    var .sum = 0
    for .i of 8 {
        val .c = .s[.i]
        var .v = 0

        given .c, .c {
            case >= '0', <= '9':
                .v = .c - '0'

            case >= 'A', <= 'Z':
                val .p = .c - 'A' + 1
                .v = .p + 9

            case '*', _: .v = 36
            case '@', _: .v = 37
            case '#', _: .v = 38

            default: return false
        }

        if .i rem 2 == 0 {
            .v x= 2
        }

        .sum += .v \ 10 + .v rem 10
    }

    .s[9] - 48 == (10 - (.sum rem 10)) rem 10
}

val .candidates = [
    "037833100",
    "17275R102",
    "38259P508",
    "594918104",
    "68389X106",
    "68389X105",
]

for .c in .candidates {
    writeln .c, ": ", if(.isCusip(.c): "good" ; "bad")
}
```


```txt
037833100: good
17275R102: good
38259P508: good
594918104: good
68389X106: bad
68389X105: good
```



## Lua

The checkDigit function is a line-for-line translation of the pseudo-code algorithm.

```Lua
function checkDigit (cusip)
  if #cusip ~= 8 then return false end

  local sum, c, v, p = 0
  for i = 1, 8 do
    c = cusip:sub(i, i)
    if c:match("%d") then
      v = tonumber(c)
    elseif c:match("%a") then
      p = string.byte(c) - 64
      v = p + 9
    elseif c == "*" then
      v = 36
    elseif c == "@" then
      v = 37
    elseif c == "#" then
      v = 38
    end
    if i % 2 == 0 then
      v = v * 2
    end

    sum = sum + math.floor(v / 10) + v % 10
  end

  return tostring((10 - (sum % 10)) % 10)
end

local testCases = {
  "037833100",
  "17275R102",
  "38259P508",
  "594918104",
  "68389X106",
  "68389X105"
}
for _, CUSIP in pairs(testCases) do
  io.write(CUSIP .. ": ")
  if checkDigit(CUSIP:sub(1, 8)) == CUSIP:sub(9, 9) then
    print("VALID")
  else
    print("INVALID")
  end
end
```

```txt
037833100: VALID
17275R102: VALID
38259P508: VALID
594918104: VALID
68389X106: INVALID
68389X105: VALID
```


=={{header|Modula-2}}==

```modula2
MODULE CUSIP;
FROM FormatString IMPORT FormatString;
FROM Terminal IMPORT WriteString,WriteLn,ReadChar;

PROCEDURE WriteInt(n : INTEGER);
VAR buf : ARRAY[0..10] OF CHAR;
BEGIN
    FormatString("%i", buf, n);
    WriteString(buf)
END WriteInt;

PROCEDURE cusipCheckDigit(cusip : ARRAY OF CHAR) : INTEGER;
VAR
    i,v,sum : INTEGER;
BEGIN
    i := 0;
    sum := 0;
    WHILE cusip[i] # 0C DO
        IF ('0' <= cusip[i]) AND (cusip[i] <= '9') THEN
            v := ORD(cusip[i]) - 48 (* 0 *)
        ELSIF ('A' <= cusip[i]) AND (cusip[i] <= 'Z') THEN
            v := ORD(cusip[i]) - 65 (* A *) + 10
        ELSIF cusip[i] = '*' THEN
            v := 36
        ELSIF cusip[i] = '@' THEN
            v := 37
        ELSIF cusip[i] = '#' THEN
            v := 38
        ELSE
            RETURN -1
        END;
        IF i MOD 2 = 1 THEN v := 2 * v END;
        IF i < 8 THEN
            sum := sum + (v DIV 10) + (v MOD 10);
        END;
        INC(i)
    END;

    IF i # 9 THEN RETURN -1 END;
    RETURN (10 - (sum MOD 10)) MOD 10
END cusipCheckDigit;

PROCEDURE isValidCusip(cusip : ARRAY OF CHAR) : BOOLEAN;
VAR
    check : INTEGER;
BEGIN
    check := cusipCheckDigit(cusip);
    IF check < 0 THEN RETURN FALSE END;
    RETURN cusip[8] = CHR(48 (* 0 *) + check)
END isValidCusip;

PROCEDURE Print(cusip : ARRAY OF CHAR);
BEGIN
    WriteString(cusip);
    IF isValidCusip(cusip) THEN
        WriteString(" : Valid")
    ELSE
        WriteString(" : Invalid")
    END;
    WriteLn
END Print;

(* main *)
BEGIN
    WriteString("CUSIP       Verdict");
    WriteLn;

    Print("037833100");
    Print("17275R102");
    Print("38259P508");
    Print("594918104");
    Print("68389X106");
    Print("68389X105");

    ReadChar
END CUSIP.
```

```txt
CUSIP       Verdict
037833100 : Valid
17275R102 : Valid
38259P508 : Valid
594918104 : Valid
68389X106 : Invalid
68389X105 : Valid
```



## Nim


```Nim
import strutils

proc cusipCheck(cusip: string): bool =
  if cusip.len != 9:
    return false

  var
    sum, v = 0
  for i, c in cusip[0 ..< ^1]:
    if c.isDigit:
      v = parseInt($c)
    elif c.isUpperAscii:
      v = ord(c) - ord('A') + 10
    elif c == '*':
      v = 36
    elif c == '@':
      v = 37
    elif c == '#':
      v = 38

    if i mod 2 == 1:
      v *= 2

    sum += v div 10 + v mod 10
  let check = (10 - (sum mod 10)) mod 10
  return $check == $cusip[^1]

proc main =
  let codes = [
    "037833100",
    "17275R102",
    "38259P508",
    "594918104",
    "68389X106",
    "68389X105"
  ]

  for code in codes:
    echo code, ": ", if cusipCheck(code): "Valid" else: "Invalid"

main()
```


```txt
037833100: Valid
17275R102: Valid
38259P508: Valid
594918104: Valid
68389X106: Invalid
68389X105: Valid

```



## Objeck

```Objeck
class Cusip {
    function : native : IsCusip(s : String) ~ Bool {
        if(s->Size() <> 9) {
            return false;
        };

        sum := 0;
        for(i := 0; i < 7; i+=1;) {
            c := s->Get(i);

            v : Int;
            if (c >= '0' & c <= '9') {
                v := c - 48;
            } else if (c >= 'A' & c <= 'Z') {
                v := c - 64;  # lower case letters apparently invalid
            } else if (c = '*') {
                v := 36;
            } else if (c = '@') {
                v := 37;
            } else if (c = '#') {
                v := 38;
            } else {
                return false;
            };

            # check if odd as using 0-based indexing
            if(i % 2 = 1) {
                v *= 2;
            };

            sum += v / 10 + v % 10;
        };

        return s->Get(8) - 48 = (10 - (sum % 10)) % 10;
    }

    function : Main(args : String[]) ~ Nil {
        candidates := [
            "037833100",
            "17275R102",
            "38259P508",
            "594918104",
            "68389X106",
            "68389X105"
        ];

        each(i : candidates) {
            candidate := candidates[i];
            "{$candidate} => "->Print();
            if(IsCusip(candidate)) {
                "correct"->PrintLine();
            }
            else {
                "incorrect"->PrintLine();
            };
        };
    }
}
```


Output:

```txt

037833100 => correct
17275R102 => correct
38259P508 => correct
594918104 => correct
68389X106 => incorrect
68389X105 => correct

```



## Perl



```perl
$cv{$_} = $i++ for '0'..'9', 'A'..'Z', '*', '@', '#';

sub cusip_check_digit {
    my @cusip = split m{}xms, shift;
    my $sum = 0;

    for $i (0..7) {
        return 'Invalid character found' unless $cusip[$i] =~ m{\A [[:digit:][:upper:]*@#] \z}xms;
        $v  = $cv{ $cusip[$i] };
        $v *= 2 if $i%2;
        $sum += int($v/10) + $v%10;
    }

    $check_digit = (10 - ($sum%10)) % 10;
    $check_digit == $cusip[8] ? '' : ' (incorrect)';
}

my %test_data = (
    '037833100' => 'Apple Incorporated',
    '17275R102' => 'Cisco Systems',
    '38259P508' => 'Google Incorporated',
    '594918104' => 'Microsoft Corporation',
    '68389X106' => 'Oracle Corporation',
    '68389X105' => 'Oracle Corporation',
);

print "$_ $test_data{$_}" . cusip_check_digit($_) . "\n" for sort keys %test_data;
```

```txt
037833100 Apple Incorporated
17275R102 Cisco Systems
38259P508 Google Incorporated
594918104 Microsoft Corporation
68389X105 Oracle Corporation
68389X106 Oracle Corporation (incorrect)
```



## Perl 6

```perl6
sub divmod ($v, $r) { $v div $r, $v mod $r }
my %chr = (flat 0..9, 'A'..'Z', <* @ #>) Z=> 0..*;

sub cuisp-check ($cuisp where *.chars == 9) {
    my ($code, $chk) = $cuisp.comb(8);
    my $sum = [+] $code.comb.kv.map: { [+] (($^k % 2 + 1) * %chr{$^v}).&divmod(10) };
    so (10 - $sum mod 10) mod 10 eq $chk;
}

# TESTING
say "$_: ", $_.&cuisp-check for <
037833100
17275R102
38259P508
594918104
68389X106
68389X105
>
```

```txt
037833100: True
17275R102: True
38259P508: True
594918104: True
68389X106: False
68389X105: True
```



## Phix


```Phix
sequence cch = {}

function CusipCheckDigit(string cusip)
integer s = 0, c, v
    if length(cch)=0 then
        cch = repeat(-1,256)
        for i='0' to '9' do
            cch[i] = i-'0'
        end for
        for i='A' to 'Z' do
            cch[i] = i-55
        end for
        cch['*'] = 36
        cch['@'] = 37
        cch['#'] = 38
    end if
    if length(cusip)!=9 or find('\0',cusip) then return 0 end if
    for i=1 to 8 do
        c := cusip[i]
        v := cch[c]
        if v=-1 then return 0 end if
        if remainder(i,2)=0 then
            v *= 2
        end if
        s += floor(v/10)+mod(v,10)
    end for
    return cusip[9]=mod(10-mod(s,10),10)+'0'
end function

sequence tests = {"037833100",  -- Apple Incorporated
                  "17275R102",  -- Cisco Systems
                  "38259P508",  -- Google Incorporated
                  "594918104",  -- Microsoft Corporation
                  "68389X106",  -- Oracle Corporation   (incorrect)
                  "68389X105"}  -- Oracle Corporation

for i=1 to length(tests) do
    string ti = tests[i]
    printf(1,"%s : %s\n",{ti,{"invalid","valid"}[CusipCheckDigit(ti)+1]})
end for
```

```txt

037833100 : valid
17275R102 : valid
38259P508 : valid
594918104 : valid
68389X106 : invalid
68389X105 : valid

```



## PicoLisp


```PicoLisp
(de cusip (Str)
   (let (Str (mapcar char (chop Str))  S 0)
      (for (I . C) (head 8 Str)
         (let V
            (cond
               ((<= 48 C 57) (- C 48))
               ((<= 65 C 90) (+ 10 (- C 65)))
               ((= C 42) 36)
               ((= C 64) 37)
               ((= C 35) 38) )
            (or
               (bit? 1 I)
               (setq V (>> -1 V)) )
            (inc
               'S
               (+ (/ V 10) (% V 10)) ) ) )
      (=
         (- (last Str) 48)
         (% (- 10 (% S 10)) 10) ) ) )

(println
   (mapcar
      cusip
      (quote
         "037833100"
         "17275R102"
         "38259P508"
         "68389X106"
         "68389X105" ) ) )
```

```txt
(T T T NIL T)
```



## Python


### Procedural

Requires Python 3.6 for the string template literal in the print statement.


```python
#!/usr/bin/env python3

import math

def cusip_check(cusip):
    if len(cusip) != 9:
        raise ValueError('CUSIP must be 9 characters')

    cusip = cusip.upper()
    total = 0
    for i in range(8):
        c = cusip[i]
        if c.isdigit():
            v = int(c)
        elif c.isalpha():
            p = ord(c) - ord('A') + 1
            v = p + 9
        elif c == '*':
            v = 36
        elif c == '@':
            v = 37
        elif c == '#':
            v = 38

        if i % 2 != 0:
            v *= 2

        total += int(v / 10) + v % 10
    check = (10 - (total % 10)) % 10
    return str(check) == cusip[-1]

if __name__ == '__main__':
    codes = [
            '037833100',
            '17275R102',
            '38259P508',
            '594918104',
            '68389X106',
            '68389X105'
            ]
    for code in codes:
        print(f'{code} -> {cusip_check(code)}')

```

Output:

```txt
037833100 -> True
17275R102 -> True
38259P508 -> True
594918104 -> True
68389X106 -> False
68389X105 -> True

```



### Composition of pure functions

Composing a set of pure functions, including a number of general and reusable abstractions:

```python
'''CUSIP'''

from itertools import (cycle, islice)
from functools import (reduce)
from operator import (add)
from enum import (Enum)


# isCusip :: Dict -> String -> Bool
def isCusip(dct):
    '''Test for the validity of a CUSIP string in the
       context of a supplied dictionary of Char values.
    '''
    def go(s):
        ns = [dct[c] for c in s if c in dct]
        return 9 == len(ns) and (
            ns[-1] == (
                10 - (
                    sum(zipWith(
                        lambda f, x: add(*divmod(f(x), 10))
                    )(cycle([identity, double]))(
                        take(8)(ns)
                    )) % 10
                )
            ) % 10
        )
    return lambda s: go(s)


# cusipCharDict :: () -> Dict Char Int
def cusipCharDict():
    '''Dictionary of integer values for CUSIP characters'''
    def kv(a, ic):
        i, c = ic
        a[c] = i
        return a
    return reduce(
        kv,
        enumerate(
            enumFromTo('0')('9') + (
                enumFromTo('A')('Z') + list('*&#')
            )
        ),
        {}
    )


# TEST -------------------------------------------------
# main :: IO ()
def main():
    '''Test for validity as a CUSIP string'''

    print(
        fTable(main.__doc__ + ':\n')(repr)(str)(
            isCusip(cusipCharDict())
        )([
            '037833100',
            '17275R102',
            '38259P508',
            '594918104',
            '68389X106',
            '68389X105'
        ])
    )


# FORMATTING ----------------------------------------------

# fTable :: String -> (a -> String) ->
#                     (b -> String) -> (a -> b) -> [a] -> String
def fTable(s):
    '''Heading -> x display function -> fx display function ->
                     f -> xs -> tabular string.
    '''
    def go(xShow, fxShow, f, xs):
        ys = [xShow(x) for x in xs]
        w = max(map(len, ys))
        return s + '\n' + '\n'.join(map(
            lambda x, y: y.rjust(w, ' ') + ' -> ' + fxShow(f(x)),
            xs, ys
        ))
    return lambda xShow: lambda fxShow: lambda f: lambda xs: go(
        xShow, fxShow, f, xs
    )


# GENERIC -------------------------------------------------

# double :: Num -> Num
def double(x):
    '''Wrapped here as a function for the zipWith expression'''
    return 2 * x


# enumFromTo :: Enum a => a -> a -> [a]
def enumFromTo(m):
    '''Enumeration of values [m..n]'''
    def go(x, y):
        t = type(m)
        i = fromEnum(x)
        d = 0 if t != float else (x - i)
        return list(map(
            lambda x: toEnum(t)(d + x),
            range(i, 1 + fromEnum(y))
        ) if int != t else range(x, 1 + y))
    return lambda n: go(m, n)


# fromEnum :: Enum a => a -> Int
def fromEnum(x):
    '''Index integer for enumerable value.'''
    return ord(x) if str == type(x) else (
        x.value if isinstance(x, Enum) else int(x)
    )


# mul :: Num -> Num -> Num
def mul(x):
    '''Function version of (*) operator;
       a curried equivalent of operator.mul'''
    return lambda y: x * y


# identity :: a -> a
def identity(x):
    '''The identity function.'''
    return x


# take :: Int -> [a] -> [a]
# take :: Int -> String -> String
def take(n):
    '''The prefix of xs of length n,
       or xs itself if n > length xs.'''
    return lambda xs: (
        xs[0:n]
        if isinstance(xs, list)
        else list(islice(xs, n))
    )


# toEnum :: Type -> Int -> a
def toEnum(t):
    '''Enumerable value from index integer'''
    dct = {
        int: int,
        float: float,
        str: chr,
        bool: bool
    }
    return lambda x: dct[t](x) if t in dct else t(x)


# zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
def zipWith(f):
    '''A list constructed by zipping with a
       custom function, rather than with the
       default tuple constructor.
    '''
    return lambda xs: lambda ys: (
        map(f, xs, ys)
    )


# MAIN ---
if __name__ == '__main__':
    main()
```

```txt
Test for validity as a CUSIP string:

'037833100' -> True
'17275R102' -> True
'38259P508' -> True
'594918104' -> True
'68389X106' -> False
'68389X105' -> True
```



## Racket



```racket
#lang racket
(require srfi/14)

(define 0-char (char->integer #\0))
(define A-char (char->integer #\A))

(define (cusip-value c)
  (cond
    [(char-set-contains? char-set:digit c)
     (- (char->integer c) 0-char)]
    [(char-set-contains? char-set:upper-case c)
     (+ 10 (- (char->integer c) A-char))]
    [(char=? c #\*) 36]
    [(char=? c #\@) 37]
    [(char=? c #\#) 38]))

(define (cusip-check-digit cusip)
  (modulo
   (- 10
      (modulo
       (for/sum
        ((i (sequence-map add1 (in-range 8))) (c (in-string cusip)))
         (let* ((v (cusip-value c)) (v′ (if (even? i) (* v 2) v)))
           (+ (quotient v′ 10) (modulo v′ 10)))) 10)) 10))

(define (CUSIP? s)
  (char=? (string-ref s (sub1 (string-length s)))
          (integer->char (+ 0-char (cusip-check-digit s)))))

(module+ test
  (require rackunit)
  (check-true (CUSIP? "037833100"))
  (check-true (CUSIP? "17275R102"))
  (check-true (CUSIP? "38259P508"))
  (check-true (CUSIP? "594918104"))
  (check-false (CUSIP? "68389X106"))
  (check-true (CUSIP? "68389X105")))
```


no output indicates all tests passed.


## REXX


### idiomatic


```rexx
/*REXX program validates that the  last digit (the check digit)  of a  CUSIP  is valid. */
@.=
parse arg @.1 .
if @.1=='' | @.1==","  then do;   @.1= 037833100       /* Apple Incorporated            */
                                  @.2= 17275R102       /* Cisco Systems                 */
                                  @.3= 38259P508       /* Google Incorporated           */
                                  @.4= 594918104       /* Microsoft Corporation         */
                                  @.5= 68389X106       /* Oracle Corporation (incorrect)*/
                                  @.6= 68389X105       /* Oracle Corporation            */
                            end

     do j=1  while @.j\='';   chkDig=CUSIPchk(@.j)     /*calculate check digit from func*/
     OK=word("isn't is", 1 + (chkDig==right(@.j,1) ) ) /*validate  check digit with func*/
     say 'CUSIP '    @.j    right(OK, 6)     "valid."  /*display the CUSIP and validity.*/
     end   /*j*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
CUSIPchk: procedure;  arg x 9;  $=0;                     abc= 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
                                       do k=1  for 8
                                       y=substr(x, k, 1)
                                          select
                                          when datatype(y,'W')  then #=y
                                          when datatype(y,'U')  then #=pos(y, abc) + 9
                                          when          y=='*'  then #=36
                                          when          y=='@'  then #=37
                                          when          y=='#'  then #=38
                                          otherwise  return 0       /*invalid character.*/
                                          end   /*select*/
                                       if k//2==0  then #=#+#       /*K even?  Double it*/
                                       $=$ + #%10 + #//10
                                       end      /*k*/
           return (10- $//10) // 10
```

'''output'''   when using the default input:

```txt

CUSPID  037833100     is valid.
CUSPID  17275R102     is valid.
CUSPID  38259P508     is valid.
CUSPID  594918104     is valid.
CUSPID  68389X106  isn't valid.
CUSPID  68389X105     is valid.

```



### conciser function


```rexx
/*REXX program validates that the  last digit (the check digit)  of a  CUSIP  is valid. */
@.=
parse arg @.1 .
if @.1=='' | @.1==","  then do;   @.1= 037833100       /* Apple Incorporated            */
                                  @.2= 17275R102       /* Cisco Systems                 */
                                  @.3= 38259P508       /* Google Incorporated           */
                                  @.4= 594918104       /* Microsoft Corporation         */
                                  @.5= 68389X106       /* Oracle Corporation (incorrect)*/
                                  @.6= 68389X105       /* Oracle Corporation            */
                            end

     do j=1  while @.j\='';   chkDig=CUSIPchk(@.j)     /*calculate check digit from func*/
     OK=word("isn't is", 1 + (chkDig==right(@.j,1) ) ) /*validate  check digit with func*/
     say 'CUSIP '    @.j    right(OK, 6)     "valid."  /*display the CUSIP and validity.*/
     end   /*j*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
CUSIPchk: procedure; arg x 9;  $=0;         abc= '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ*@#'
                                      /* [↓]  if  Y  isn' found,  then POS returns zero.*/
                                    do k=1  for 8;   y=substr(x,k,1) /*get a character. */
                                    #=pos(y, abc) - 1                /*get its position.*/
                                    if #   == -1  then return 0      /*invalid character*/
                                    if k//2==  0  then #=#+#         /*K even? double it*/
                                    $=$ + #%10 + #//10
                                    end      /*k*/
          return (10-$//10) // 10
```

'''output'''   is the same as the idiomatic REXX version.




## Ring


```ring

# Project : CUSIP

inputstr = list(6)
inputstr[1] = "037833100"
inputstr[2] = "17275R102"
inputstr[3] = "38259P508"
inputstr[4] = "594918104"
inputstr[5] = "68389X106"
inputstr[6] = "68389X105"
for n = 1 to len(inputstr)
     cusip(inputstr[n])
next

func cusip(inputstr)
        if len(inputstr) != 9
            see " length is incorrect, invalid cusip"
            return
        ok
        v = 0
        sum = 0
        for i = 1 to 8
             flag = 0
             x = ascii(inputstr[i])
             if x >= ascii("0") and x <= ascii("9")
                v = x - ascii("0")
                flag = 1
             ok
             if x >= ascii("A") and x <= ascii("Z")
                v = x - 64
                flag = 1
             ok
             if x = ascii("*")
                v= 36
                flag = 1
             ok
             if x = ascii("@")
                v = 37
                flag = 1
             ok
             if x = ascii("#")
                v = 38
                flag = 1
             ok
             if flag = 0
                  see " found a invalid character, invalid cusip" + nl
             ok
             if (i % 2) = 0
                 v = v * 2
             ok
             sum = sum + floor(v / 10) + v % 10
        next
        sum = (10 - (sum % 10)) % 10
        if sum = (ascii(inputstr[9]) - ascii("0"))
           see inputstr + " is valid" + nl
        else
           see inputstr + " is invalid" + nl
        ok

```

Output:

```txt

037833100 is valid
17275R102 is valid
38259P508 is valid
594918104 is valid
68389X106 is invalid
68389X105 is valid

```



## Ruby


### Following pseudocode


```ruby

#!/usr/bin/env ruby

def check_cusip(cusip)
  abort('CUSIP must be 9 characters') if cusip.size != 9

  sum = 0
  cusip.split('').each_with_index do |char, i|
    next if i == cusip.size - 1
    case
    when char.scan(/\D/).empty?
      v = char.to_i
    when char.scan(/\D/).any?
      pos = char.upcase.ord - 'A'.ord + 1
      v = pos + 9
    when char == '*'
      v = 36
    when char == '@'
      v = 37
    when char == '#'
      v = 38
    end

    v *= 2 unless (i % 2).zero?
    sum += (v/10).to_i + (v % 10)
  end

  check = (10 - (sum % 10)) % 10
  return 'VALID' if check.to_s == cusip.split('').last
  'INVALID'
end

CUSIPs = %w[
  037833100 17275R102 38259P508 594918104 68389X106 68389X105
]

CUSIPs.each do |cusip|
  puts "#{cusip}: #{check_cusip(cusip)}"
end


```


Output:

```txt

037833100: VALID
17275R102: VALID
38259P508: VALID
594918104: VALID
68389X106: INVALID
68389X105: VALID

```


### More concise

Since it uses methods like chain, to_h, sum, and infinite Range syntax (0..), this needs a Ruby version > 2.5

```Ruby

TABLE = ("0".."9").chain("A".."Z", %w(* @ #)).zip(0..).to_h

def valid_CUSIP?(str)
  sum = str[0..-2].chars.each_slice(2).sum do |c1,c2|
    TABLE[c1].divmod(10).sum + (TABLE[c2]*2).divmod(10).sum
  end
  str[-1].to_i == (10 - (sum % 10)) % 10
end

CUSIPs = %w(037833100 17275R102 38259P508 594918104 68389X106 68389X105)
CUSIPs.each{|cusip| puts "#{cusip}: #{valid_CUSIP? cusip}"}

```



## Rust



```rust
fn cusip_check(cusip: &str) -> bool {
    if cusip.len() != 9 {
        return false;
    }

    let mut v = 0;
    let capital_cusip = cusip.to_uppercase();
    let char_indices = capital_cusip.as_str().char_indices().take(7);

    let total = char_indices.fold(0, |total, (i, c)| {
        v = match c {
            '*' => 36,
            '@' => 37,
            '#' => 38,
            _ if c.is_digit(10) => c.to_digit(10).unwrap() as u8,
            _ if c.is_alphabetic() => (c as u8) - b'A' + 1 + 9,
            _ => v,
        };

        if i % 2 != 0 {
            v *= 2
        }
        total + (v / 10) + v % 10
    });

    let check = (10 - (total % 10)) % 10;
    (check.to_string().chars().nth(0).unwrap()) == cusip.chars().nth(cusip.len() - 1).unwrap()
}

fn main() {
    let codes = [
        "037833100",
        "17275R102",
        "38259P508",
        "594918104",
        "68389X106",
        "68389X105",
    ];
    for code in &codes {
        println!("{} -> {}", code, cusip_check(code))
    }
}
```


Output:

```txt
037833100 -> True
17275R102 -> True
38259P508 -> True
594918104 -> True
68389X106 -> False
68389X105 -> True

```



## Scala

{{Out}}See it running in your browser by [https://scalafiddle.io/sf/jwxwWpq/0 ScalaFiddle (JavaScript, non JVM)] or by [https://scastie.scala-lang.org/OBrz9l14Rm2C6tV8tiwhWg Scastie (JVM)].

```Scala
object Cusip extends App {

  val candidates = Seq("037833100", "17275R102", "38259P508", "594918104", "68389X106", "68389X105")

  for (candidate <- candidates)
    printf(f"$candidate%s -> ${if (isCusip(candidate)) "correct" else "incorrect"}%s%n")

  private def isCusip(s: String): Boolean = {
    if (s.length != 9) false
    else {
      var sum = 0
      for (i <- 0 until 7) {
        val c = s(i)
        var v = 0
        if (c >= '0' && c <= '9') v = c - 48
        else if (c >= 'A' && c <= 'Z') v = c - 64 // lower case letters apparently invalid
        else if (c == '*') v = 36
        else if (c == '@') v = 37
        else if (c == '#') v = 38
        else return false
        if (i % 2 == 1) v *= 2 // check if odd as using 0-based indexing
        sum += v / 10 + v % 10
      }
      s(8) - 48 == (10 - (sum % 10)) % 10
    }
  }

}
```



## SNOBOL4


```snobol4
#!/usr/local/bin/snobol4 -r
*  cusip.sno
*   -- Committee on Uniform Security Identification Procedures
*  -r : read data placed after the end label.
*  Verify check digit and size of cusip code.

     define("cusipt()i")                  :(cusipt_end)
cusipt
     chars = &digits &ucase "*@#"
     cusipt = table()
     i = 0
cusipt_1
     chars pos(i) len(1) . c              :f(return)
     cusipt[c] = i
     i = i + 1                            :(cusipt_1)
cusipt_end

     define("check_cusip(line)c,i")       :(check_cusip_end)
check_cusip
     eq(size(line), 9)                    :f(freturn)
     check_cusip = 0
     i = 0
check_cusip_1
     line pos(i) len(1) . c
     value = t[c]
     value = eq(remdr(i, 2), 1) t[c] * 2
     check_cusip = check_cusip + (value / 10) + remdr(value, 10)
     i = lt(i, 7) i + 1                   :s(check_cusip_1)
     check_cusip = remdr(10 - remdr(check_cusip, 10), 10)
     eq(substr(line, 9, 1), check_cusip)  :s(return)f(freturn)
check_cusip_end

*** main ***
     t = cusipt()

read line = input                         :f(end)
     check_cusip(line)                    :f(bad_cusip)
     output = line " valid."              :(read)
bad_cusip
     output =  line " not valid."         :(read)
end
037833100
17275R102
38259P508
594918104
68389X106
68389X105
68389X10
68389X1059
68389x105
```

```txt
037833100 valid.
17275R102 valid.
38259P508 valid.
594918104 valid.
68389X106 not valid.
68389X105 valid.
68389X10 not valid.
68389X1059 not valid.
68389x105 not valid.
```



## Swift



```swift
struct CUSIP {
  var value: String

  private static let alphabet = Array("ABCDEFGHIJKLMNOPQRSTUVWXYZ")

  init?(value: String) {
    if value.count == 9 && String(value.last!) == CUSIP.checkDigit(cusipString: String(value.dropLast())) {
      self.value = value
    } else if value.count == 8, let checkDigit = CUSIP.checkDigit(cusipString: value) {
      self.value = value + checkDigit
    } else {
      return nil
    }
  }

  static func checkDigit(cusipString: String) -> String? {
    guard cusipString.count == 8, cusipString.allSatisfy({ $0.isASCII }) else {
      return nil
    }

    let sum = cusipString.uppercased().enumerated().reduce(0, {sum, pair in
      let (i, char) = pair
      var v: Int

      switch char {
      case "*":
        v = 36
      case "@":
        v = 37
      case "#":
        v = 38
      case _ where char.isNumber:
        v = char.wholeNumberValue!
      case _:
        v = Int(char.asciiValue! - 65) + 10
      }

      if i & 1 == 1 {
        v *= 2
      }

      return sum + (v / 10) + (v % 10)
    })

    return String((10 - (sum % 10)) % 10)
  }
}

let testCases = [
  "037833100",
  "17275R102",
  "38259P508",
  "594918104",
  "68389X106",
  "68389X105"
]

for potentialCUSIP in testCases {
  print("\(potentialCUSIP) -> ", terminator: "")

  switch CUSIP(value: potentialCUSIP) {
  case nil:
    print("Invalid")
  case _:
    print("Valid")
  }
}
```


```txt
037833100 -> Valid
17275R102 -> Valid
38259P508 -> Valid
594918104 -> Valid
68389X106 -> Invalid
68389X105 -> Valid
```



## Tcl


###  Direct translation of pseudocode


```Tcl
proc ordinal-of-alpha {c} {                     ;#  returns ordinal position of c in the alphabet (A=1, B=2...)
    lsearch {_ A B C D E F G H I J K L M N O P Q R S T U V W X Y Z} [string toupper $c]
}

proc Cusip-Check-Digit {cusip} {                ;# algorithm Cusip-Check-Digit(cusip) is
    if {[string length $cusip] != 8} {          ;#    Input: an 8-character CUSIP
        return false
    }

    set sum 0                                   ;#    sum := 0
    for {set i 1} {$i <= 8} {incr i} {          ;#    for 1 ≤ i ≤ 8 do
        set c [string index $cusip $i-1]        ;#       c := the ith character of cusip
        if {[string is digit $c]} {             ;#       if c is a digit then
            set v $c                            ;#          v := numeric value of the digit c
        } elseif {[string is alpha $c]} {       ;#       else if c is a letter then
            set p [ordinal-of-alpha $c]         ;#          p := ordinal position of c in the alphabet (A=1, B=2...)
            set v [expr {$p + 9}]               ;#          v := p + 9
        } elseif {$c eq "*"} {                  ;#       else if c = "*" then
            set v 36                            ;#          v := 36
        } elseif {$c eq "@"} {                  ;#       else if c = "@" then
            set v 37                            ;#          v := 37
        } elseif {$c eq "#"} {                  ;#       else if c = "#" then
            set v 38                            ;#          v := 38
        }                                       ;#       end if
        if {$i % 2 == 0} {                      ;#       if i is even then
            set v [expr {$v * 2}]               ;#          v := v × 2
        }                                       ;#       end if

        incr sum [expr {$v / 10 + $v % 10}]     ;#       sum := sum + int ( v div 10 ) + v mod 10
    }                                           ;#    repeat

    expr {(10 - ($sum % 10)) % 10}              ;#    return (10 - (sum mod 10)) mod 10
}
proc check-cusip {cusip} {
    set last  [string index $cusip end]
    set cusip [string range $cusip 0 end-1]
    expr {$last eq [Cusip-Check-Digit $cusip]}
}
```



###  More idiomatic Tcl


```Tcl
proc check-cusip {code} {
    if {[string length $code] != 9} {
        return false
    }
    set alphabet 0123456789abcdefghijklmnopqrstuvwxyz@#
    set code [split [string tolower $code] ""]
    foreach char $code idx {1 2 3 4 5 6 7 8 9} {
        set v [string first $char $alphabet]
        if {$v == -1} {return false}
        if {$idx % 2 == 0} {
            incr v $v
        }
        set v [::tcl::mathop::+ {*}[split $v ""]]
        incr sum $v
    }
    expr {$sum % 10 == 0}
}
```



###  Common test harness


```Tcl
proc test {} {
    foreach {cusip name} {
        037833100       "Apple Incorporated"
        17275R102       "Cisco Systems"
        38259P508       "Google Incorporated"
        594918104       "Microsoft Corporation"
        68389X106       "Oracle Corporation   (incorrect)"
        68389X105       "Oracle Corporation"
    } {
        puts [format %-40s%s $name [expr {[check-cusip $cusip] ? "valid" : "invalid"}]]
        puts [format %-40s%s $name [expr {[cusip-check $cusip] ? "valid" : "invalid"}]]
    }
}
test
```



###  Output

```txt
Apple Incorporated                      valid
Cisco Systems                           valid
Google Incorporated                     valid
Microsoft Corporation                   valid
Oracle Corporation   (incorrect)        invalid
Oracle Corporation                      valid
```



## VBA


```vb
Private Function Cusip_Check_Digit(s As Variant) As Integer
    Dim Sum As Integer, c As String, v As Integer
    For i = 1 To 8
        c = Mid(s, i, 1)
        If IsNumeric(c) Then
            v = Val(c)
        Else
            Select Case c
                Case "a" To "z"
                    v = Asc(c) - Asc("a") + 10
                Case "A" To "Z"
                    v = Asc(c) - Asc("A") + 10
                Case "*"
                    v = 36
                Case "@"
                    v = 37
                Case "#"
                    v = 38
                Case Else
                    Debug.Print "not expected"
            End Select
        End If
        If i Mod 2 = 0 Then v = v * 2
        Sum = Sum + Int(v \ 10) + v Mod 10
    Next i
    Cusip_Check_Digit = (10 - (Sum Mod 10)) Mod 10
End Function
```
```txt
037833100     is valid
17275R102     is valid
38259P508     is valid
594918104     is valid
68389X106     not valid
68389X105     is valid
```



## Visual Basic .NET

```vbnet
Module Module1

    Function IsCUSIP(s As String) As Boolean
        If s.Length <> 9 Then
            Return False
        End If

        Dim sum = 0
        For i = 0 To 7
            Dim c = s(i)

            Dim v As Integer
            If "0" <= c AndAlso c <= "9" Then
                v = Asc(c) - 48
            ElseIf "A" <= c AndAlso c <= "Z" Then
                v = Asc(c) - 64 ' Lower case letters are apparently invalid
            ElseIf c = "*" Then
                v = 36
            ElseIf c = "#" Then
                v = 38
            Else
                Return False
            End If

            If i Mod 2 = 1 Then
                v *= 2 ' check if odd as using 0-based indexing
            End If
            sum += v \ 10 + v Mod 10
        Next
        Return Asc(s(8)) - 48 = (10 - (sum Mod 10)) Mod 10
    End Function

    Sub Main()
        Dim candidates As New List(Of String) From {
            "037833100",
            "17275R102",
            "38259P508",
            "594918104",
            "68389X106",
            "68389X105"
        }

        For Each candidate In candidates
            Console.WriteLine("{0} -> {1}", candidate, If(IsCUSIP(candidate), "correct", "incorrect"))
        Next
    End Sub

End Module
```

```txt
037833100 -> correct
17275R102 -> correct
38259P508 -> correct
594918104 -> correct
68389X106 -> incorrect
68389X105 -> correct
```



## Yabasic

```Yabasic
sub cusip(inputStr$)
    local i, v, sum, x$

    Print inputStr$;
    If Len(inputStr$) <> 9 Print " length is incorrect, invalid cusip" : return

    For i = 1 To 8
        x$ = mid$(inputStr$, i, 1)
        switch x$
            Case "*": v = 36 : break
            Case "@": v = 37 : break
            Case "#": v = 38 : break
            default:
                if x$ >= "A" and x$ <= "Z" then
                    v = asc(x$) - Asc("A") + 10
                elsif x$ >= "0" and x$ <= "9" then
                    v = asc(x$) - asc("0")
                else
                    Print " found a invalid character, invalid cusip"
                    return
                end if
        End switch

        If and(i, 1) = 0 v = v * 2
        sum = sum + int(v / 10) + mod(v, 10)
    Next

    sum = mod(10 - mod(sum, 10), 10)
    If sum = asc(mid$(inputStr$, 9, 1)) - Asc("0") Then
        Print " is valid"
    Else
        Print " is invalid"
    End If

End Sub

// ------=< MAIN >=------

Data "037833100", "17275R102", "38259P508"
Data "594918104", "68389X106", "68389X105", ""

Print
do
    Read inputStr$
    if inputStr$ = "" break
    cusip(inputStr$)
loop

```



## zkl


```zkl
fcn cusipCheckDigit(cusip){
   var [const] vs=[0..9].chain(["A".."Z"],T("*","@","#")).pump(String);
   try{
      sum:=Walker.cycle(1,2).zipWith(fcn(n,c){ v:=vs.index(c)*n; v/10 + v%10 },
           cusip[0,8]).reduce('+);
      ((10 - sum%10)%10 == cusip[8].toInt()) and cusip.len()==9
   }catch{ False }
}
```


```zkl
foreach cusip in (T("037833100", "17275R102",
		    "38259P508", "594918104", "68389X106", "68389X105")){
   println(cusip,": ",cusipCheckDigit(cusip));
}
```

```txt

037833100: True
17275R102: True
38259P508: True
594918104: True
68389X106: False
68389X105: True

```

