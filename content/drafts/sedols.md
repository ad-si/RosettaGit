+++
title = "SEDOLs"
description = ""
date = 2019-09-12T14:12:28Z
aliases = []
[extra]
id = 2960
[taxonomies]
categories = []
tags = []
+++

{{task|Checksums}}

;Task:
For each number list of '''6'''-digit [[wp:SEDOL|SEDOL]]s, calculate and append the checksum digit.  


That is, given this input:

```txt

710889
B0YBKJ
406566
B0YBLH
228276
B0YBKL
557910
B0YBKR
585284
B0YBKT
B00030

```
 
Produce this output:

```txt

7108899
B0YBKJ7
4065663
B0YBLH2
2282765
B0YBKL9
5579107
B0YBKR5
5852842
B0YBKT7
B000300

```


;Extra credit:
Check each input is correctly formed, especially with respect to valid characters allowed in a SEDOL string.


;Related tasks:
*   [[Luhn test]]
*   [[Calculate International Securities Identification Number|ISIN]]





## ActionScript


```ActionScript
//Return the code corresponding to a given character.
//ActionScript does not have a character type, so 1-digit strings
//are used instead
function toSEDOLCode(char:String):uint {
	//Make sure only a single character was sent.
	if(char.length != 1)
		throw new Error("toSEDOL expected string length of 1, got " + char.length);
	//Character is uppercase
	if (char >= "A" && char <= "Z") {
		return SEDOL.charCodeAt() + 10 - "A".charCodeAt();
	}
	//Character is numeric
	else if (char >= "0" && char <= "9"){
		return uint(char);
	}
	//Error: character is neither numeric nor uppercase
	else{
		throw new Error("toSEDOLCode expected numeric or uppercase character, recieved " + char);
	}
}
//Calculate the weighted sum for the SEDOL.
function toSum(str:String):uint {
	if(str.length != 6)
		throw new Error("toSum expected length 6, recieved " + str.length);
	var sum:uint=0;
	for (var i:uint = 0; i < str.length; i++) 
		sum+=toSEDOLCode(str.charAt(i))*[1,3,1,7,3,9][i];
	return sum;
}
//Calculate the check digit from the weighted sum.
function toCheck(num:int):uint
{
	return (10 -(num % 10)) % 10;
}
//Print the SEDOL with the check digit added.
function printWithCheck(SEDOL:String):void
{
	trace(SEDOL + toCheck(toSum(SEDOL)));
}
printWithCheck("710889");
printWithCheck("B0YBKJ");
printWithCheck("406566");
printWithCheck("B0YBLH");
printWithCheck("228276");
printWithCheck("B0YBKL");
printWithCheck("557910");
printWithCheck("B0YBKR");
printWithCheck("585284");
printWithCheck("B0YBKT");
printWithCheck("B00030");
```



## Ada


```ada
with Ada.Text_IO;  use Ada.Text_IO;

procedure Test_SEDOL is

   subtype SEDOL_String is String (1..6);
   type SEDOL_Sum is range 0..9;

   function Check (SEDOL : SEDOL_String) return SEDOL_Sum is
      Weight : constant array (SEDOL_String'Range) of Integer := (1,3,1,7,3,9);
      Sum    : Integer := 0;
      Item   : Integer;
   begin
      for Index in SEDOL'Range loop
         Item := Character'Pos (SEDOL (Index));
         case Item is
            when Character'Pos ('0')..Character'Pos ('9') =>
               Item := Item - Character'Pos ('0');
            when Character'Pos ('B')..Character'Pos ('D') |
                 Character'Pos ('F')..Character'Pos ('H') |
                 Character'Pos ('J')..Character'Pos ('N') |
                 Character'Pos ('P')..Character'Pos ('T') |
                 Character'Pos ('V')..Character'Pos ('Z') =>
               Item := Item - Character'Pos ('A') + 10;
            when others =>
               raise Constraint_Error;
         end case;
         Sum := Sum + Item * Weight (Index);
      end loop;
      return SEDOL_Sum ((-Sum) mod 10);
   end Check;

   Test : constant array (1..10) of SEDOL_String :=
             (  "710889", "B0YBKJ", "406566", "B0YBLH", "228276",
                "B0YBKL", "557910", "B0YBKR", "585284", "B0YBKT"
             );
begin
   for Index in Test'Range loop
      Put_Line (Test (Index) & Character'Val (Character'Pos ('0') + Check (Test (Index))));
   end loop;
end Test_SEDOL;
```

The function Check raises Constraint_Error upon an invalid input. The calculated sum is trimmed using (-''sum'') mod 10, which is mathematically equivalent to (10 - (''sum'' mod 10)) mod 10.

Sample output:

```txt
7108899
B0YBKJ7
4065663
B0YBLH2
2282765
B0YBKL9
5579107
B0YBKR5
5852842
B0YBKT7
```



## ALGOL 68

{{trans|C}}
{{works with|ALGOL 68G|Any - tested with release mk15-0.8b.fc9.i386 - ''char in string'', ''is alpha'', ''is digit''  and ''to upper'' are not in the standard's prelude}}

```algol68
[]INT sedol weights = (1, 3, 1, 7, 3, 9);
STRING reject = "AEIOUaeiou";
 
PROC strcspn = (STRING s,reject)INT: (
  INT out:=0;
  FOR i TO UPB s DO
    IF char in string(s[i], LOC INT, reject) THEN
      return out
    FI;
    out:=i
  OD;
  return out: out
);

PROC sedol checksum = (REF STRING sedol6)INT:
(
  INT out;

  INT len := UPB sedol6;
  INT sum := 0;
 
  IF sedol6[len-1] = REPR 10 THEN len-:=1; sedol6[len]:=null char FI;
  IF len = 7 THEN
    putf(stand error, ($"SEDOL code already checksummed? ("g")"l$, sedol6)); 
    out := ABS ( BIN ABS sedol6[6] AND 16r7f); return out
  FI;
  IF len > 7 OR len < 6 OR strcspn(sedol6, reject) /= 6 THEN
    putf(stand error, ($"not a SEDOL code? ("g")"l$, sedol6));
    out := -1; return out
  FI;
  FOR i TO UPB sedol6 DO
    sum+:=sedol weights[i]*
      IF is digit(sedol6[i]) THEN
        ABS sedol6[i]- ABS "0"
      ELIF is alpha(sedol6[i]) THEN
        (ABS to upper(sedol6[i])-ABS "A") + 10
      ELSE
        putf(stand error, $"SEDOL with not alphanumeric digit"l$);
        out:=-1; return out
      FI
  OD;
  out := (10 - (sum MOD 10)) MOD 10 + ABS "0";
  return out: out
);
 
main:
(
  STRING line;
  
  on logical file end(stand in, (REF FILE f)BOOL: done);
  DO getf(stand in, ($gl$,line));
    INT sr := sedol checksum(line);
    IF sr > 0 THEN
      printf(($ggl$, line, REPR sedol checksum(line)))
    FI
  OD;
  done: SKIP
)
```

Output:

```txt
7108899
B0YBKJ7
4065663
B0YBLH2
2282765
B0YBKL9
5579107
B0YBKR5
5852842
B0YBKT7
```



## ALGOL W


```algolw
begin
    % returns the check digit for the specified SEDOL %
    string(1) procedure sedolCheckDigit ( string(6) value sedol ) ;
    begin
        integer       checkSum, checkDigit;
        checkSum := 0;
        for cPos := 0 until 5 do begin
            string(1) c;
            integer   digit;
            c := sedol( cPos // 1 );
            if c >= "0" and c <= "9"
            then digit :=        decode( c ) - decode( "0" )
            else digit := 10 + ( decode( c ) - decode( "A" ) );
            checkSum := checkSum + ( ( case cPos + 1 of ( 1, 3, 1, 7, 3, 9 ) ) * digit )
        end for_cPos ;
        checkDigit := ( 10 - ( checkSum rem 10 ) ) rem 10;
        if checkDigit < 10
        then code( decode( "0" ) +   checkDigit        )
        else code( decode( "A" ) + ( checkDigit - 10 ) )
    end sedolCheckDigit ;

    % task test cases %

    procedure testCheckDigit ( string(6) value sedol; string(1) value expectedCheckDigit ) ;
    begin
        string(1) checkDigit;
        checkDigit := sedolCheckDigit( sedol );
        write( s_w := 0, sedol, checkDigit );
        if checkDigit not = expectedCheckDigit then writeon( " ?? expected: ", expectedCheckDigit )
    end testCheckDigit ;

    testCheckDigit( "710889", "9" );
    testCheckDigit( "B0YBKJ", "7" );
    testCheckDigit( "406566", "3" );
    testCheckDigit( "B0YBLH", "2" );
    testCheckDigit( "228276", "5" );
    testCheckDigit( "B0YBKL", "9" );
    testCheckDigit( "557910", "7" );
    testCheckDigit( "B0YBKR", "5" );
    testCheckDigit( "585284", "2" );
    testCheckDigit( "B0YBKT", "7" );
    testCheckDigit( "B00030", "0" )
end.
```

{{out}}

```txt

7108899
B0YBKJ7
4065663
B0YBLH2
2282765
B0YBKL9
5579107
B0YBKR5
5852842
B0YBKT7
B000300

```



## AutoHotkey

ahk forum: [http://www.autohotkey.com/forum/post-276683.html#276683 discussion]


### Full


```AutoHotkey
codes = 710889,B0YBKJ,406566,B0YBLH,228276,B0YBKL,557910,B0YBKR,585284,B0YBKT,B00030,ABCDEF,BBBBBBB
Loop, Parse, codes, `,
    output .= A_LoopField "`t-> " SEDOL(A_LoopField) "`n"
Msgbox %output%

SEDOL(code) {
    Static weight1:=1, weight2:=3, weight3:=1, weight4:=7, weight5:=3, weight6:=9
    If (StrLen(code) != 6)
        Return "Invalid length."
    StringCaseSense On
    Loop, Parse, code
        If A_LoopField is Number
            check_digit += A_LoopField * weight%A_Index%
        Else If A_LoopField in B,C,D,F,G,H,J,K,L,M,N,P,Q,R,S,T,V,W,X,Y,Z
            check_digit += (Asc(A_LoopField)-Asc("A") + 10) * weight%A_Index%
        Else
            Return "Invalid character."
    Return code . Mod(10-Mod(check_digit,10),10)
}
```



### Short

{{works with|AutoHotkey 1.1}}

```AutoHotkey
MsgBox % SEDOL("710889")  ;7108899
MsgBox % SEDOL("B0YBKJ")  ;B0YBKJ7
MsgBox % SEDOL("406566")  ;4065663
MsgBox % SEDOL("B0YBLH")  ;B0YBLH2
MsgBox % SEDOL("228276")  ;2282765
MsgBox % SEDOL("B0YBKL")  ;B0YBKL9
MsgBox % SEDOL("557910")  ;5579107
MsgBox % SEDOL("B0YBKR")  ;B0YBKR5
MsgBox % SEDOL("585284")  ;5852842
MsgBox % SEDOL("B0YBKT")  ;B0YBKT7

SEDOL(w) {
    static weights := [1,3,1,7,3,9]
    loop parse, w
        s += ((c := Asc(A_LoopField)) >= 65 ? c - 65 + 10 : c - 48) * weights[A_Index]
    return w Mod(10 - Mod(s, 10), 10)
}
```



## AWK

Validate or calculate checksum of SEDOL codes read from standard input (one per line)

```awk
function ord(a)
{
  return amap[a]
}

function sedol_checksum(sed)
{
  sw[1] = 1; sw[2] = 3; sw[3] = 1
  sw[4] = 7; sw[5] = 3; sw[6] = 9
  sum = 0
  for(i=1; i <= 6; i++) {
    c = substr(toupper(sed), i, 1)
    if ( c ~ /[[:digit:]]/ ) {
      sum += c*sw[i]
    } else {
      sum += (ord(c)-ord("A")+10)*sw[i]
    }
  }
  return (10 - (sum % 10)) % 10
}

BEGIN { # prepare amap for ord
  for(_i=0;_i<256;_i++) {
    astr = sprintf("%c", _i)
    amap[astr] = _i
  }
}

/[AEIOUaeiou]/ {
  print "'" $0 "' not a valid SEDOL code"
  next
}
{
  if ( (length($0) > 7) || (length($0) < 6) ) {
    print "'" $0 "' is too long or too short to be valid SEDOL"
    next
  }
  sedol = substr($0, 1, 6)
  sedolcheck = sedol_checksum(sedol)
  if ( length($0) == 7 ) {
    if ( (sedol sedolcheck) != $0 ) {
      print sedol sedolcheck " (original " $0 " has wrong check digit"
    } else {
      print sedol sedolcheck
    }
  } else {
    print sedol sedolcheck
  }
}
```



## BASIC

{{works with|QuickBasic|4.5}}

```qbasic
DECLARE FUNCTION getSedolCheckDigit! (str AS STRING)
DO
        INPUT a$
        PRINT a$ + STR$(getSedolCheckDigit(a$))
LOOP WHILE a$ <> ""

FUNCTION getSedolCheckDigit (str AS STRING)
    IF LEN(str) <> 6 THEN
        PRINT "Six chars only please"
        EXIT FUNCTION
    END IF
    str = UCASE$(str)
    DIM mult(6) AS INTEGER
    mult(1) = 1: mult(2) = 3: mult(3) = 1
    mult(4) = 7: mult(5) = 3: mult(6) = 9
    total = 0
    FOR i = 1 TO 6
        s$ = MID$(str, i, 1)
        IF s$ = "A" OR s$ = "E" OR s$ = "I" OR s$ = "O" OR s$ = "U" THEN
                PRINT "No vowels"
                EXIT FUNCTION
        END IF
        IF ASC(s$) >= 48 AND ASC(s$) <= 57 THEN
                total = total + VAL(s$) * mult(i)
        ELSE
                total = total + (ASC(s$) - 55) * mult(i)
        END IF

    NEXT i
    getSedolCheckDigit = (10 - (total MOD 10)) MOD 10
END FUNCTION
```



## BBC BASIC


```bbcbasic
      PRINT FNsedol("710889")
      PRINT FNsedol("B0YBKJ")
      PRINT FNsedol("406566")
      PRINT FNsedol("B0YBLH")
      PRINT FNsedol("228276")
      PRINT FNsedol("B0YBKL")
      PRINT FNsedol("557910")
      PRINT FNsedol("B0YBKR")
      PRINT FNsedol("585284")
      PRINT FNsedol("B0YBKT")
      PRINT FNsedol("B00030")
      END
      
      DEF FNsedol(d$)
      LOCAL a%, i%, s%, weights%()
      DIM weights%(6) : weights%() = 0, 1, 3, 1, 7, 3, 9
      FOR i% = 1 TO 6
        a% = ASCMID$(d$,i%) - &30
        s% += (a% + 7 * (a% > 9)) * weights%(i%)
      NEXT
      = d$ + CHR$(&30 + (10 - s% MOD 10) MOD 10)
```

'''Output:'''

```txt

7108899
B0YBKJ7
4065663
B0YBLH2
2282765
B0YBKL9
5579107
B0YBKR5
5852842
B0YBKT7
B000300

```



## C

'''Notes''': it reads the codes from standard input, one per line (linefeed terminated); the input encoding must meet the following specifications: single byte encoding, digits (0-9) must have codes that follow the same order of the digits (0, 1, 2, ...) and similar for letters, the encoding must match the one used with the compiled source (likely, ASCII based encodings). This should happen 99% of the time (for ASCII, ISO-8859 family and UTF-8 have the same byte encoding for alphanumeric characters).


```c>#include <stdio.h

#include <ctype.h>
#include <string.h>

int sedol_weights[] = {1, 3, 1, 7, 3, 9}; 
const char *reject = "AEIOUaeiou";

int sedol_checksum(const char *sedol6)
{
  int len = strlen(sedol6);
  int sum = 0, i;

  if ( len == 7 ) {
    fprintf(stderr, "SEDOL code already checksummed? (%s)\n", sedol6);
    return sedol6[6] & 0x7f;
  }
  if ( (len > 7) || (len < 6) || ( strcspn(sedol6, reject) != 6 )) {
    fprintf(stderr, "not a SEDOL code? (%s)\n", sedol6);
    return -1;
  }
  for(i=0; i < 6; i++) {
    if ( isdigit(sedol6[i]) ) {
      sum += (sedol6[i]-'0')*sedol_weights[i];
    } else if ( isalpha(sedol6[i]) ) {
      sum += ((toupper(sedol6[i])-'A') + 10)*sedol_weights[i];
    } else {
      fprintf(stderr, "SEDOL with not alphanumeric digit\n");
      return -1;
    }
  }
  return (10 - (sum%10))%10 + '0'; 
}


#define MAXLINELEN 10
int main()
{
  char line[MAXLINELEN];
  int sr, len;
  while( fgets(line, MAXLINELEN, stdin) != NULL ) {
    len = strlen(line);
    if ( line[len-1] == '\n' ) line[len-1]='\0';
    sr = sedol_checksum(line);
    if ( sr > 0 )
      printf("%s%c\n", line, sr);
  }
  return 0;
}
```


Fed the input list from the task description, the output is:


```txt
7108899
B0YBKJ7
4065663
B0YBLH2
2282765
B0YBKL9
5579107
B0YBKR5
5852842
B0YBKT7
```



## C++


```cpp

#include <numeric>
#include <cctype>
#include <iostream>
#include <string>

 
template<typename result_sink_t>
auto sedol_checksum(std::string const& sedol, result_sink_t result_sink)
{
    if(sedol.size() != 6) 
        return result_sink(0, "length of sedol string != 6");
        
    const char * valid_chars = "BCDFGHJKLMNPQRSTVWXYZ0123456789";
    if(sedol.find_first_not_of(valid_chars) != std::string::npos)
        return result_sink(0, "sedol string contains disallowed characters");
   
    const int weights[] = {1,3,1,7,3,9};    
    auto weighted_sum = std::inner_product(sedol.begin(), sedol.end(), weights, 0
                                           , [](int acc, int prod){ return acc + prod; }
                                           , [](char c, int weight){ return (std::isalpha(c) ? c -'A' + 10 : c - '0') * weight; }
                                             );
    return result_sink((10 - (weighted_sum % 10)) % 10, nullptr);
}

int main()
{
    using namespace std; 
    string inputs[] = {
       "710889", "B0YBKJ", "406566", "B0YBLH", "228276", "B0YBKL", 
       "557910", "B0YBKR", "585284", "B0YBKT", "B00030"
   }; 
   for(auto const & sedol : inputs)
   {
        sedol_checksum(sedol, [&](auto sum, char const * errorMessage)
        {
            if(errorMessage)
                cout << "error for sedol: " << sedol << " message: " <<  errorMessage << "\n";
            else
                cout << sedol << sum << "\n"; 
        });
   }
   return 0;
}

```


=={{header|C sharp|C#}}==

```csharp
static int[] sedol_weights = { 1, 3, 1, 7, 3, 9 };
static int sedolChecksum(string sedol)
{
    int len = sedol.Length;
    int sum = 0;

    if (len == 7) //SEDOL code already checksummed?
        return (int)sedol[6];

    if ((len > 7) || (len < 6) || System.Text.RegularExpressions.Regex.IsMatch(sedol, "[AEIOUaeiou]+")) //invalid SEDOL
        return -1;

    for (int i = 0; i < 6; i++)
    {
        if (Char.IsDigit(sedol[i]))
            sum += (((int)sedol[i] - 48) * sedol_weights[i]);

        else if (Char.IsLetter(sedol[i]))
            sum += (((int)Char.ToUpper(sedol[i]) - 55) * sedol_weights[i]);

        else
            return -1;

    }

    return (10 - (sum % 10)) % 10;
}
```


=={{header|Caché ObjectScript}}==


```cos
Class Utils.Check [ Abstract ]
{

ClassMethod SEDOL(x As %String) As %Boolean
{
	// https://en.wikipedia.org/wiki/SEDOL
	IF x'?1(7N,1U5UN1N) QUIT 0
	IF x'=$TRANSLATE(x,"AEIOU") QUIT 0
	SET cd=$EXTRACT(x,*), x=$EXTRACT(x,1,*-1)
	SET wgt="1317391", t=0
	FOR i=1:1:$LENGTH(x) {
		SET n=$EXTRACT(x,i)
		IF n'=+n SET n=$ASCII(n)-55
		SET t=t+(n*$EXTRACT(wgt,i))
	}
	QUIT cd=((10-(t#10))#10)
}

}
```

{{out|Examples}}

```txt
USER>For  { Read s Quit:s=""  Write ": "_##class(Utils.Check).SEDOL(s), ! }
7108899: 1
B0YBKJ7: 1
4065663: 1
B0YBLH2: 1
2282765: 1
B0YBKL9: 1
5579107: 1
B0YBKR5: 1
5852842: 1
B0YBKT7: 1
B000300: 1

USER>
```



## Clojure


```Clojure
(defn sedols [xs]
  (letfn [(sedol [ys] (let [weights [1 3 1 7 3 9]
                            convtonum (map #(Character/getNumericValue %) ys)
                            check (-> (reduce + (map * weights convtonum)) (rem 10) (->> (- 10)) (rem 10))]
                        (str ys check)))]
    (map #(sedol %) xs)))
```



## COBOL

{{works with|GNU Cobol|2.0}}

```cobol>       >
SOURCE FREE
IDENTIFICATION DIVISION.
PROGRAM-ID. sedol.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
    SELECT sedol-file ASSIGN "sedol.txt"
        ORGANIZATION LINE SEQUENTIAL
        FILE STATUS sedol-file-status.

DATA DIVISION.
FILE SECTION.
FD  sedol-file.
01  sedol                               PIC X(6).

WORKING-STORAGE SECTION.
01  sedol-file-status                   PIC XX.
    88  sedol-file-ok                   VALUE "00".

01  digit-num                           PIC 9 COMP.
    
01  digit-weights-area                  VALUE "1317391".
    03  digit-weights                   PIC 9 OCCURS 7 TIMES.
    
01  weighted-sum-parts-area.
    03  weighted-sum-parts              PIC 9(3) COMP OCCURS 6 TIMES.

01  weighted-sum                        PIC 9(3) COMP.

01  check-digit                         PIC 9.

PROCEDURE DIVISION.
    OPEN INPUT sedol-file
    PERFORM UNTIL NOT sedol-file-ok
        READ sedol-file
            AT END
                EXIT PERFORM
        END-READ

        MOVE FUNCTION UPPER-CASE(sedol) TO sedol
        
        PERFORM VARYING digit-num FROM 1 BY 1 UNTIL digit-num > 6
            EVALUATE TRUE
                WHEN sedol (digit-num:1) IS ALPHABETIC-UPPER
                    IF sedol (digit-num:1) = "A" OR "E" OR "I" OR "O" OR "U"
                        DISPLAY "Invalid SEDOL: " sedol
                        EXIT PERFORM CYCLE
                    END-IF
                
                    COMPUTE weighted-sum-parts (digit-num) =
                        (FUNCTION ORD(sedol (digit-num:1)) - FUNCTION ORD("A")
                        + 10) * digit-weights (digit-num)
                        
                WHEN sedol (digit-num:1) IS NUMERIC
                    MULTIPLY FUNCTION NUMVAL(sedol (digit-num:1))
                        BY digit-weights (digit-num)
                        GIVING weighted-sum-parts (digit-num)
                        
                WHEN OTHER
                    DISPLAY "Invalid SEDOL: " sedol
                    EXIT PERFORM CYCLE
            END-EVALUATE
        END-PERFORM

        INITIALIZE weighted-sum
        PERFORM VARYING digit-num FROM 1 BY 1 UNTIL digit-num > 6
            ADD weighted-sum-parts (digit-num) TO weighted-sum
        END-PERFORM
        
        COMPUTE check-digit =
            FUNCTION MOD(10 - FUNCTION MOD(weighted-sum, 10), 10)

        DISPLAY sedol check-digit
    END-PERFORM
    
    CLOSE sedol-file
    .
END PROGRAM sedol.
```



## Common Lisp

Implemented from scratch using the description on Wikipedia as specification.
{{Works with|ClozureCL}}

```lisp
(defun append-sedol-check-digit (sedol &key (start 0) (end (+ start 6)))
  (assert (<= 0 start end (length sedol)))
  (assert (= (- end start) 6))
  (loop 
       :with checksum = 0
       :for weight :in '(1 3 1 7 3 9)
       :for index :upfrom start
       :do (incf checksum (* weight (digit-char-p (char sedol index) 36)))
       :finally (let* ((posn (- 10 (mod checksum 10)))
		       (head (subseq sedol start end))
		       (tail (digit-char posn)))
		  (return (concatenate 'string head (list tail))))))
```



## D


### Functional Version


```d
import std.stdio, std.algorithm, std.string, std.numeric, std.ascii;

char checksum(in char[] sedol) pure @safe /*@nogc*/
in {
    assert(sedol.length == 6);
    foreach (immutable c; sedol)
        assert(c.isDigit || (c.isUpper && !"AEIOU".canFind(c)));
} out (result) {
    assert(result.isDigit);
} body {
    static immutable c2v = (in dchar c) => c.isDigit ? c - '0' : (c - 'A' + 10);
    immutable int d = sedol.map!c2v.dotProduct([1, 3, 1, 7, 3, 9]);
    return digits[10 - (d % 10)];
}

void main() {
    foreach (const sedol; "710889 B0YBKJ 406566 B0YBLH 228276
                          B0YBKL 557910 B0YBKR 585284 B0YBKT".split)
        writeln(sedol, sedol.checksum);
}
```

{{out}}

```txt
7108899
B0YBKJ7
4065663
B0YBLH2
2282765
B0YBKL9
5579107
B0YBKR5
5852842
B0YBKT7
```



### Imperative Version

Longer, faster lower-level version, same output.

```d
import std.stdio, std.algorithm, std.string, std.numeric, std.ascii;

char sedolChecksum(in char[] sedol) pure nothrow @safe /*@nogc*/
in {
    assert(sedol.length == 6, "SEDOL must be 6 chars long.");
    enum uint mask = 0b11_1110_1111_1011_1110_1110_1110;

    foreach (immutable c; sedol)
        assert(c.isDigit ||
               (c > 'A' && c <= 'Z' && ((1U << (c - 'A')) & mask)),
               "SEDOL with wrong char.");
} out(result) {
    assert(result.isDigit);
    static int c2v(in dchar c) pure nothrow @safe @nogc {
        return c.isDigit ? c - '0' : c - 'A' + 10;
    }
    immutable int d = sedol.map!c2v.dotProduct([1, 3, 1, 7, 3, 9]);
    assert((d + result - '0') % 10 == 0);
} body {
    static immutable int[] weights = [1, 3, 1, 7, 3, 9];

    int sum = 0;
    foreach (immutable i, immutable c; sedol) {
        if (c.isDigit)
            sum += (c - '0') * weights[i];
        else
            sum += (c - 'A' + 10) * weights[i];
    }

    return '0' + 10 - (sum % 10);
}

void main() {
    foreach (immutable s; ["710889", "B0YBKJ", "406566", "B0YBLH",
                           "228276", "B0YBKL", "557910", "B0YBKR",
                           "585284", "B0YBKT"])
        writeln(s, s.sedolChecksum);
}
```



### Short Version

Same output.

```d
void main() {
    import std.stdio, std.algorithm, std.string, std.numeric,std.ascii;

    foreach (const s; "710889 B0YBKJ 406566 B0YBLH 228276
                       B0YBKL 557910 B0YBKR 585284 B0YBKT".split)
        writeln(s, '0' + 10 - s
                   .map!(c => c.isDigit ? c - '0' : c - 'A' + 10)
                   .dotProduct([1, 3, 1, 7, 3, 9]) % 10);
}
```



## Delphi



```Delphi
program Sedol;

{$APPTYPE CONSOLE}

uses
  SysUtils;


const
  SEDOL_CHR_COUNT = 6;
  DIGITS = ['0'..'9'];
  LETTERS = ['A'..'Z'];
  VOWELS = ['A', 'E', 'I', 'O', 'U'];
  ACCEPTABLE_CHRS = DIGITS + LETTERS - VOWELS;
  WEIGHTS : ARRAY [1..SEDOL_CHR_COUNT] of integer = (1, 3, 1, 7, 3, 9);
  LETTER_OFFSET = 9;


function AddSedolCheckDigit(Sedol : string) : string;
var
  iChr : integer;
  Checksum : integer;
  CheckDigit : char;
begin
  if Sedol <> uppercase(Sedol) then
    raise ERangeError.CreateFmt('%s contains lower case characters',[Sedol]);
  if length(Sedol) <> SEDOL_CHR_COUNT then
    raise ERangeError.CreateFmt('"%s" length is invalid. Should be 6 characters',[Sedol]);

  Checksum := 0;
  for iChr := 1 to SEDOL_CHR_COUNT do
  begin

    if Sedol[iChr] in Vowels then
      raise ERangeError.CreateFmt('%s contains a vowel (%s) at chr %d',[Sedol, Sedol[iChr], iChr]);
    if not (Sedol[iChr] in ACCEPTABLE_CHRS) then
      raise ERangeError.CreateFmt('%s contains an invalid chr (%s) at position %d',[Sedol, Sedol[iChr], iChr]);

    if Sedol[iChr] in DIGITS then
      Checksum := Checksum + (ord(Sedol[iChr]) - ord('0')) * WEIGHTS[iChr]
    else
      Checksum := Checksum + (ord(Sedol[iChr]) - ord('A') + 1 + LETTER_OFFSET) * WEIGHTS[iChr];

  end;

  Checksum := (Checksum mod 10);
  if Checksum <> 0 then
    Checksum := 10 - Checksum;
  CheckDigit := chr(CheckSum + ord('0'));

  Result := Sedol + CheckDigit;
end;


procedure Test(First6 : string);
begin
  writeln(First6, ' becomes ', AddSedolCheckDigit(First6));
end;


begin
  try
    Test('710889');
    Test('B0YBKJ');
    Test('406566');
    Test('B0YBLH');
    Test('228276');
    Test('B0YBKL');
    Test('557910');
    Test('B0YBKR');
    Test('585284');
    Test('B0YBKT');
    Test('B00030');
  except
    on E : Exception do
      writeln(E.Message);
  end;
  readln;
end.

```


Output:

```txt
710889 becomes 7108899
B0YBKJ becomes B0YBKJ7
406566 becomes 4065663
B0YBLH becomes B0YBLH2
228276 becomes 2282765
B0YBKL becomes B0YBKL9
557910 becomes 5579107
B0YBKR becomes B0YBKR5
585284 becomes 5852842
B0YBKT becomes B0YBKT7
B00030 becomes B000300
```



## E


```e
def weights := [1,3,1,7,3,9]
def Digit := ('0'..'9')
def Letter := ('B'..'D'|'F'..'H'|'J'..'N'|'P'..'T'|'V'..'Z')
def sedolCharValue(c) {
  switch (c) {
    match digit :Digit { return digit - '0' }
    match letter :Letter {
      return letter - 'A'
    }
  }
}

def checksum(sedol :String) {
  require(sedol.size() == 6)
  var sum := 0
  for i => c in sedol {
    sum += weights[i] * sedolCharValue(c)
  }
  return E.toString((10 - sum %% 10) %% 10)
}

def addChecksum(sedol :String) {
  return sedol + checksum(sedol)
}

for sedol in "710889
              B0YBKJ
              406566
              B0YBLH
              228276
              B0YBKL
              557910
              B0YBKR
              585284
              B0YBKT".trim().split("\n") {
  println(addChecksum(sedol.trim()))
}
```



## Elixir

{{trans|Ruby}}

```elixir
defmodule SEDOL do
  @sedol_char  "0123456789BCDFGHJKLMNPQRSTVWXYZ" |> String.codepoints
  @sedolweight  [1,3,1,7,3,9]
  
  defp char2value(c) do
    unless c in @sedol_char, do: raise ArgumentError, "No vowels"
    String.to_integer(c,36)
  end
  
  def checksum(sedol) do
    if String.length(sedol) != length(@sedolweight), do: raise ArgumentError, "Invalid length"
    sum = Enum.zip(String.codepoints(sedol), @sedolweight)
          |> Enum.map(fn {ch, weight} -> char2value(ch) * weight end)
          |> Enum.sum
    to_string(rem(10 - rem(sum, 10), 10))
  end
end

data = ~w{
          710889
          B0YBKJ
          406566
          B0YBLH
          228276
          B0YBKL
          557910
          B0YBKR
          585284
          B0YBKT
          B00030
          C0000
          1234567
          00000A
         }

Enum.each(data, fn sedol ->
  :io.fwrite "~-8s ", [sedol]
  try do
    IO.puts sedol <> SEDOL.checksum(sedol)
  rescue
    e in ArgumentError -> IO.inspect e
  end
end)
```


{{out}}

```txt

710889   7108899
B0YBKJ   B0YBKJ7
406566   4065663
B0YBLH   B0YBLH2
228276   2282765
B0YBKL   B0YBKL9
557910   5579107
B0YBKR   B0YBKR5
585284   5852842
B0YBKT   B0YBKT7
B00030   B000300
C0000    %ArgumentError{message: "Invalid length"}
1234567  %ArgumentError{message: "Invalid length"}
00000A   %ArgumentError{message: "No vowels"}

```



## Excel VBA


```lb

Function getSedolCheckDigit(Input1)
    Dim mult(6) As Integer
    mult(1) = 1: mult(2) = 3: mult(3) = 1
    mult(4) = 7: mult(5) = 3: mult(6) = 9
    If Len(Input1) <> 6 Then
        getSedolCheckDigit = "Six chars only please"
        Exit Function
    End If
    Input1 = UCase(Input1)
    Total = 0
    For i = 1 To 6
        s1 = Mid(Input1, i, 1)
        If (s1 = "A") Or (s1 = "E") Or (s1 = "I") Or (s1 = "O") Or (s1 = "U") Then
                getSedolCheckDigit = "No vowels"
                Exit Function
        End If
        If (Asc(s1) >= 48) And (Asc(s1) <= 57) Then
                Total = Total + Val(s1) * mult(i)
        Else
                Total = Total + (Asc(s1) - 55) * mult(i)
        End If
 
    Next i
    getSedolCheckDigit = Input1 + CStr((10 - (Total Mod 10)) Mod 10)
        
End Function

```


=={{header|F Sharp|F#}}==

```fsharp
open System
let Inputs = ["710889"; "B0YBKJ"; "406566"; "B0YBLH"; "228276"; "B0YBKL"
              "557910"; "B0YBKR"; "585284"; "B0YBKT"; "B00030"]

let Vowels = set ['A'; 'E'; 'I'; 'O'; 'U']
let Weights = [1; 3; 1; 7; 3; 9; 1]

let inline isVowel c = Vowels.Contains (Char.ToUpper c)

let char2value c =   
    if Char.IsDigit c then int c - 0x30 
    else (['A'..'Z'] |> List.findIndex ((=) (Char.ToUpper c))) + 10
        
let sedolCheckDigit (input: string) =
    if input.Length <> 6 || input |> Seq.exists isVowel then 
        failwithf "Input must be six characters long and not contain vowels: %s" input

    let sum = Seq.map2 (fun ch weight -> (char2value ch) * weight) input Weights |> Seq.sum
    (10 - sum%10)%10              

let addCheckDigit inputs =
    inputs |> List.map (fun s -> s + (sedolCheckDigit s).ToString())

let processDigits() =
    try
        addCheckDigit Inputs |> List.iter (printfn "%s")
    with 
        ex -> printfn "ERROR: %s" ex.Message
```



## Factor


```factor
USING: combinators combinators.short-circuit formatting io kernel
math math.parser regexp sequences unicode ;
IN: rosetta-code.sedols

<PRIVATE

CONSTANT: input {
    "710889" "B0YBKJ" "406566" "B0YBLH" "228276" "B0YBKL"
    "557910" "B0YBKR" "585284" "B0YBKT" "B00030" "AEIOUA"
    "123"    ""       "B_7K90"
}

CONSTANT: weights B{ 1 3 1 7 3 9 1 }

: sedol-error ( seq -- err-str )
    {
        { [ dup empty? ] [ drop "no data" ] }
        { [ dup length 6 = not ] [ drop "invalid length" ] }
        [ drop "invalid char(s)" ]
    } cond "*error* " prepend ;

: sedol-valid? ( seq -- ? )
    { [ length 6 = ] [ R/ [0-9BCDFGHJ-NP-TV-Z]+/ matches? ] } 1&& ;

: sedol-value ( m -- n ) dup digit? [ digit> ] [ 55 - ] if ;

: sedol-checksum ( seq -- n )
    [ sedol-value ] { } map-as weights [ * ] 2map sum ;

: (sedol-check-digit) ( seq -- str )
    sedol-checksum 10 mod 10 swap - 10 mod number>string ;

PRIVATE>

: sedol-check-digit ( seq -- str )
    dup sedol-valid? [ (sedol-check-digit) ] [ sedol-error ] if ;

: sedol-demo ( -- )
    "SEDOL   Check digit\n
### ===  ========
" print
    input [ dup sedol-check-digit "%-6s  %s\n" printf ] each ;

MAIN: sedol-demo
```

{{out}}

```txt

SEDOL   Check digit

### ===  ========

710889  9
B0YBKJ  7
406566  3
B0YBLH  2
228276  5
B0YBKL  9
557910  7
B0YBKR  5
585284  2
B0YBKT  7
B00030  0
AEIOUA  *error* invalid char(s)
123     *error* invalid length
        *error* no data
B_7K90  *error* invalid char(s)

```



## Forth


```forth
create weight 1 , 3 , 1 , 7 , 3 , 9 ,

: char>num ( '0-9A-Z' -- 0..35 )
  dup [char] 9 > 7 and - [char] 0 - ;

: check+ ( sedol -- sedol' )
  6 <> abort" wrong SEDOL length"
  0 ( sum )
  6 0 do
    over I + c@ char>num
    weight I cells + @ *
    +
  loop
  10 mod   10 swap -  10 mod  [char] 0 +
  over 6 + c! 7 ;

: sedol"   [char] " parse check+ type ;

sedol" 710889" 7108899 ok
sedol" B0YBKJ" B0YBKJ7 ok
sedol" 406566" 4065663 ok
sedol" B0YBLH" B0YBLH2 ok
sedol" 228276" 2282765 ok
sedol" B0YBKL" B0YBKL9 ok
sedol" 557910" 5579107 ok
sedol" B0YBKR" B0YBKR5 ok
sedol" 585284" 5852842 ok
sedol" B0YBKT" B0YBKT7 ok
```



## Fortran

{{Works with|Fortran|90 and later}}

```fortran
MODULE SEDOL_CHECK
  IMPLICIT NONE
  CONTAINS
 
  FUNCTION Checkdigit(c)
    CHARACTER :: Checkdigit
    CHARACTER(6), INTENT(IN) :: c
    CHARACTER(36) :: alpha = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    INTEGER, DIMENSION(6) :: weights = (/ 1, 3, 1, 7, 3, 9 /), temp
    INTEGER :: i, n

    DO i = 1, 6
      temp(i) = INDEX(alpha, c(i:i)) - 1
    END DO
    temp = temp * weights
    n = MOD(10 - (MOD(SUM(temp), 10)), 10)  
    Checkdigit = ACHAR(n + 48)
  END FUNCTION Checkdigit
 
END MODULE SEDOL_CHECK

PROGRAM SEDOLTEST
  USE SEDOL_CHECK
  IMPLICIT NONE
 
  CHARACTER(31) :: valid = "0123456789BCDFGHJKLMNPQRSTVWXYZ"
  CHARACTER(6) :: codes(10) = (/ "710889", "B0YBKJ", "406566", "B0YBLH", "228276" ,  &
                                 "B0YBKL", "557910", "B0YBKR", "585284", "B0YBKT" /)
  CHARACTER(7) :: sedol
  INTEGER :: i, invalid

  DO i = 1, 10
    invalid = VERIFY(codes(i), valid)
    IF (invalid == 0) THEN
      sedol = codes(i)
      sedol(7:7) = Checkdigit(codes(i))
    ELSE
      sedol = "INVALID"
    END IF
    WRITE(*, "(2A9)") codes(i), sedol
  END DO
   
END PROGRAM SEDOLTEST
```

Output
   710889  7108899
   B0YBKJ  B0YBKJ7
   406566  4065663
   B0YBLH  B0YBLH2
   228276  2282765
   B0YBKL  B0YBKL9
   557910  5579107
   B0YBKR  B0YBKR5
   585284  5852842
   B0YBKT  B0YBKT7


## FreeBASIC


```FreeBASIC
' version 05-07-2015
' compile with: fbc -s console

Function check_sedol(input_nr As String) As Integer
    input_nr = Trim(input_nr)
    Dim As Integer i, j, x, nr_begin, sum
    Dim As String ch, legal = "AEIOU0123456789BCDFGHJKLMNPQRSTVWXYZ"
    Dim As Integer weight(0 To ...) = { 1, 3, 1, 7, 3, 9, 1}

    x = Len(input_nr)
    If x < 6 Or x > 7 Then
        Return -99 ' to long or to short
    End If

    For i = 0 To 5
        ch = Chr(input_nr[i])
        j = InStr(legal,ch)
        If j < 6 Then
            Return -90+j ' not a legal char. or a vowel
        End If
        j = ch[0] - Asc("0")
        If j > 9 Then j = j + (Asc("0") + 10- Asc("A"))
        If i = 0 AndAlso j < 10 Then nr_begin = 1
        If nr_begin = 1 AndAlso i > 0 Then
            If j > 9 Then Return -97 ' first is number then all be numbers
        End If
        sum = sum + j * weight(i)
    Next
    sum= ((10 - (sum Mod 10)) Mod 10)
    If x = 7 Then
        j=input_nr[6] - Asc("0") ' checksum digit is only number
        If j = sum Then
            Return 100+sum ' correct
        Else
            Return -98   ' wrong
        End If
    End If

    Return sum ' checksum digit

End Function

Sub sedol(in As String)

    Dim As Integer checksum = check_sedol(in)
    Print(in);

    Select Case checksum
        Case -99
            Print " Illegal SEDOL: wrong length"
        Case -98
            Print " Illegal SEDOL: checksum digits do not match"
        Case -97
            Print " Illegal SEDOL: starts with number, may only contain numbers"
        Case -90
            Print " Illegal SEDOL: illegal character"
        Case -89 To -85
            Print " Illegal SEDOL: No vowels allowed"
        Case Is > 99
            Print " Valid SEDOL: checksums match"
        Case Else
            Print " checksum calculated : ";in;Str(checksum)
    End Select

End Sub
' ------=< MAIN >=------

Dim As Integer k,checksum
Dim As String in(1 To ...) = {"710889", "B0YBKJ", "406566", "B0YBLH",_
                              "228276", "B0YBKL", "557910", "B0YBKR",_
                                        "585284", "B0YBKT", "B00030"}

Print "Calculated checksum"
For k = 1 To UBound(in) : sedol(in(k)) : Next

Print : Print "Check checksum"
Dim As String in1(1 To ...) = {"7108899", "B0YBKJ7", "4065663", "B0YBLH2",_
                                "2282765", "B0YBKL9","5579107", "B0YBKR5",_
                                          "5852842", "B0YBKT7", "B000300"}

For k = 1 To UBound(in1) : sedol(in1(k)) : Next

Print : Print "Error test"
Dim As String errors(1 To ...) = {"12", "1234567890", "1B0000", "123 45",_
                                                     "A00000", "B000301"}

For k = 1 To UBound(errors) : sedol(errors(k)) : Next

' empty keyboard buffer
While Inkey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

{{out}}

```txt
Calculated checksum
710889 checksum calculated : 7108899
B0YBKJ checksum calculated : B0YBKJ7
406566 checksum calculated : 4065663
B0YBLH checksum calculated : B0YBLH2
228276 checksum calculated : 2282765
B0YBKL checksum calculated : B0YBKL9
557910 checksum calculated : 5579107
B0YBKR checksum calculated : B0YBKR5
585284 checksum calculated : 5852842
B0YBKT checksum calculated : B0YBKT7
B00030 checksum calculated : B000300

Check checksum
7108899 Valid SEDOL: checksums match
B0YBKJ7 Valid SEDOL: checksums match
4065663 Valid SEDOL: checksums match
B0YBLH2 Valid SEDOL: checksums match
2282765 Valid SEDOL: checksums match
B0YBKL9 Valid SEDOL: checksums match
5579107 Valid SEDOL: checksums match
B0YBKR5 Valid SEDOL: checksums match
5852842 Valid SEDOL: checksums match
B0YBKT7 Valid SEDOL: checksums match
B000300 Valid SEDOL: checksums match

Error test
12 Illegal SEDOL: wrong length
1234567890 Illegal SEDOL: wrong length
1B0000 Illegal SEDOL: starts with number, may only contain numbers
123 45 Illegal SEDOL: illegal character
A00000 Illegal SEDOL: No vowels allowed
B000301 Illegal SEDOL: checksum digits do not match
```



## Gambas

'''[https://gambas-playground.proko.eu/?gist=905f91c785f1f15a360726717731862f Click this link to run this code]'''

```gambas
Public Sub Main()
Dim byWeight As Byte[] = [1, 3, 1, 7, 3, 9, 1]
Dim byCount, byCompute As Byte
Dim siTotal As Short
Dim sWork As New String[]
Dim sToProcess As String[] = ["710889", "B0YBKJ", "406566", "B0YBLH", "228276", "B0YBKL",
                              "557910", "B0YBKR", "585284", "B0YBKT", "B00030"]

For byCompute = 0 To sToProcess.Max
  For byCount = 1 To 6 
    If IsLetter(Mid(sToProcess[byCompute], byCount, 1)) Then 
      sWork.Add(Str(Asc(Mid(sToProcess[byCompute], byCount, 1)) - 55) * byWeight[byCount - 1])
    Else
      sWork.Add(Val(Mid(sToProcess[byCompute], byCount, 1)) * byWeight[byCount - 1])
    End If
  Next

  For byCount = 0 To 5
    siTotal += Val(sWork[byCount])
  Next

  siTotal = (10 - (siTotal Mod 10)) Mod 10

  Print sToProcess[byCompute] & " = " & sToProcess[byCompute] & siTotal
  sWork.Clear()
  siTotal = 0
Next

End
```

Output:

```txt

710889 = 7108899
B0YBKJ = B0YBKJ7
406566 = 4065663
B0YBLH = B0YBLH2
228276 = 2282765
B0YBKL = B0YBKL9
557910 = 5579107
B0YBKR = B0YBKR5
585284 = 5852842
B0YBKT = B0YBKT7
B00030 = B000300

```


## Go


```go

package main

import (
    "fmt"
    "strings"
    "strconv"
)

const input = `710889
B0YBKJ
406566
B0YBLH
228276
B0YBKL
557910
B0YBKR
585284
B0YBKT
B00030

B
B0003
B000300
A00030
E00030
I00030
O00030
U00030
β00030
β0003`

var weight = [...]int{1,3,1,7,3,9}

func csd(code string) string {
    switch len(code) {
    case 6:
    case 0:
        return "No data"
    default:
        return "Invalid length"
    }
    sum := 0
    for i, c := range code {
        n, err := strconv.ParseInt(string(c), 36, 0)
        if err != nil || c == 'A' || c == 'E' || c == 'I' || c == 'O' || c == 'U' {
            return "Invalid character"
        }
        sum += int(n)*weight[i]
    }
    return strconv.Itoa(9-(sum-1)%10)
}   
    
func main() {
    for _, s := range strings.Split(input, "\n") {
        d := csd(s)
        if len(d) > 1 {
            fmt.Printf(":%s: %s\n", s, d)
        } else {
            fmt.Println(s + d)
        }
    }
}

```

Output:

```txt

7108899
B0YBKJ7
4065663
B0YBLH2
2282765
B0YBKL9
5579107
B0YBKR5
5852842
B0YBKT7
B000300
:: No data
:B: Invalid length
:B0003: Invalid length
:B000300: Invalid length
:A00030: Invalid character
:E00030: Invalid character
:I00030: Invalid character
:O00030: Invalid character
:U00030: Invalid character
:β00030: Invalid length
:β0003: Invalid character

```



## Groovy


```groovy
def checksum(text) {
    assert text.size() == 6 && !text.toUpperCase().find(/[AEIOU]+/) : "Invalid SEDOL text: $text"

    def sum = 0
    (0..5).each { index ->
        sum +=  Character.digit(text.charAt(index), 36) * [1, 3, 1, 7, 3, 9][index]
    }
    text + (10 - (sum % 10)) % 10
}
String.metaClass.sedol = { this.&checksum(delegate) }
```

Test Code:

```groovy
[ '710889': '7108899', 'B0YBKJ': 'B0YBKJ7', '406566': '4065663', 'B0YBLH': 'B0YBLH2',
  '228276': '2282765', 'B0YBKL': 'B0YBKL9', '557910': '5579107', 'B0YBKR': 'B0YBKR5',
  '585284': '5852842', 'B0YBKT': 'B0YBKT7', 'B00030': 'B000300'].each { text, expected ->
    println "Checking $text -> $expected"
    assert expected == text.sedol()
}
```

Output:

```txt
Checking 710889 -> 7108899
Checking B0YBKJ -> B0YBKJ7
Checking 406566 -> 4065663
Checking B0YBLH -> B0YBLH2
Checking 228276 -> 2282765
Checking B0YBKL -> B0YBKL9
Checking 557910 -> 5579107
Checking B0YBKR -> B0YBKR5
Checking 585284 -> 5852842
Checking B0YBKT -> B0YBKT7
Checking B00030 -> B000300
```



## Haskell


```haskell
import Data.Char (isDigit, isAsciiUpper, ord)

checkSum :: String -> String
checkSum =
  show .
  (`rem` 10) .
  (-) 10 . (`rem` 10) . sum . zipWith (*) [1, 3, 1, 7, 3, 9] . fmap charValue

charValue :: Char -> Int
charValue c
  | c `elem` "AEIOU" = error "No vowels."
  | isDigit c = ord c - ord '0'
  | isAsciiUpper c = ord c - ord 'A' + 10

-- TEST ----------------------------------------------------------------------
main :: IO ()
main =
  mapM_
    (putStrLn . ((++) <*> checkSum))
    [ "710889"
    , "B0YBKJ"
    , "406566"
    , "B0YBLH"
    , "228276"
    , "B0YBKL"
    , "557910"
    , "B0YBKR"
    , "585284"
    , "B0YBKT"
    , "B00030"
    ]
```

{{Out}}

```txt
7108899
B0YBKJ7
4065663
B0YBLH2
2282765
B0YBKL9
5579107
B0YBKR5
5852842
B0YBKT7
B000300
```


=={{header|Icon}} and {{header|Unicon}}==

```Icon
procedure main()
every write(sedol("710889"|"B0YBKJ"|"406566"|"B0YBLH"|"228276"|
           "B0YBKL"|"557910"|"B0YBKR"|"585284"|"B0YBKT"|"B00030"))
end

procedure sedol(x)   #: return the completed sedol with check digit
static w,c
initial {
   every (i := -1, c := table())[!(&digits||&ucase)] := i +:= 1 # map chars
   every c[!"AEIOU"] := &null                 # delete vowels
   w := [1,3,1,7,3,9]                         # weights
   }
   
if *(x := map(x,&lcase,&ucase)) = *w then {   # match lengths
   every (t :=0, i := 1 to *x) do
      t +:= \c[x[i]]*w[i] | fail              # accumulate weighted chars
   return x  || (10 - (t%10))  % 10           # complete
   }
end
```



## J

There are several ways to perform this in J.  This most closely follows the algorithmic description at Wikipedia:

```j
sn   =.  '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ'  
ac0  =:  (, 10 | 1 3 1 7 3 9 +/@:* -)&.(sn i. |:)
```

However, because J is so concise, having written the above, it becomes clear that the negation (<tt>-</tt>) is unnecessary.  

The fundamental operation is the linear combination (<tt>+/@:*</tt>) and neither argument is "special".  In particular, the coefficients are just another array participating in the calculation, and there's no reason we can't modify them as easily as the input array.  Having this insight, it is obvious that manipulating the coefficients, rather than the input array, will be more efficient (because the coefficients are fixed at small size, while the input array can be arbitrarily large).

Which leads us to this more efficient formulation:

```j
ac1  =:  (, 10 | (10 - 1 3 1 7 3 9) +/@:* ])&.(sn i. |:)
```

which reduces to:

```j
ac2  =:  (, 10 | 9 7 9 3 7 1 +/@:* ])&.(sn i. |:)
```

Which is just as concise as <tt>ac0</tt>, but faster.

Following this train of thought, our array thinking leads us to realize that even the modulus isn't necessary.  The number of SEDOL numbers is finite, as is the number of coefficients; therefore the number of possible linear combinations of these is finite.  In fact, there are only 841 possible outcomes.  This is a small number, and can be efficiently stored as a lookup table (even better, since the outcomes will be mod 10, they are restricted to the digits 0-9, and they repeat).

Which leads us to:

```j
ac3  =:  (,"1 0 (841 $ '0987654321') {~ 1 3 1 7 3 9 +/ .*~ sn i. ])
```

Which is more than twice as fast as even the optimized formulation (<tt>ac2</tt>), though it is slightly longer.


## Java


```java
import java.util.Scanner;

public class SEDOL{
	public static void main(String[] args){
		Scanner sc = new Scanner(System.in);
		while(sc.hasNext()){
			String sedol = sc.next();
			System.out.println(sedol + getSedolCheckDigit(sedol));
		}
	}
	
	private static final int[] mult = {1, 3, 1, 7, 3, 9};
	
	public static int getSedolCheckDigit(String str){
	    if(!validateSedol(str)){
	    	System.err.println("SEDOL strings must contain six characters with no vowels.");
	    	return -1;
	    }
	    str = str.toUpperCase();
	    int total = 0;
	    for(int i = 0;i < 6; i++){
	        char s = str.charAt(i);
	        total += Character.digit(s, 36) * mult[i];
	    }
	    return (10 - (total % 10)) % 10;
	}

	public static boolean validateSedol(String str){
		return (str.length() == 6) && !str.toUpperCase().matches(".*?[AEIOU].*?");
	}
}
```



## JavaScript


### Imperative


```javascript
function sedol(input) {
    return input + sedol_check_digit(input);
}

var weight = [1, 3, 1, 7, 3, 9, 1];
function sedol_check_digit(char6) {
    if (char6.search(/^[0-9BCDFGHJKLMNPQRSTVWXYZ]{6}$/) == -1)
        throw "Invalid SEDOL number '" + char6 + "'";
    var sum = 0;
    for (var i = 0; i < char6.length; i++)
        sum += weight[i] * parseInt(char6.charAt(i), 36);
    var check = (10 - sum%10) % 10;
    return check.toString();
}

var input = [ 
    '710889', 'B0YBKJ', '406566', 'B0YBLH', '228276',
    'B0YBKL', '557910', 'B0YBKR', '585284', 'B0YBKT',
    "BOATER" , "12345", "123456", "1234567"
];

var expected = [ 
    '7108899', 'B0YBKJ7', '4065663', 'B0YBLH2', '2282765',
    'B0YBKL9', '5579107', 'B0YBKR5', '5852842', 'B0YBKT7',
    null, null, '1234563', null
];

for (var i in input) {
    try {
        var sedolized = sedol(input[i]);
        if (sedolized == expected[i]) 
            print(sedolized);
        else
            print("error: calculated sedol for input " + input[i] + 
                  " is " + sedolized + ", but it should be " + expected[i]
            );
    }
    catch (e) {
        print("error: " + e);
    }
}
```

output

```txt
7108899
B0YBKJ7
4065663
B0YBLH2
2282765
B0YBKL9
5579107
B0YBKR5
5852842
B0YBKT7
error: Invalid SEDOL number 'BOATER'
error: Invalid SEDOL number '12345'
1234563
error: Invalid SEDOL number '1234567'
```



### Functional


```javascript
(() => {
    'use strict';

    const main = () => {

        // checkSumLR :: String -> Either String String
        const checkSumLR = s => {
            const
                tpl = partitionEithers(map(charValueLR, s));
            return 0 < tpl[0].length ? (
                Left(s + ' -> ' + unwords(tpl[0]))
            ) : Right(rem(10 - rem(
                sum(zipWith(
                    (a, b) => a * b,
                    [1, 3, 1, 7, 3, 9],
                    tpl[1]
                )), 10
            ), 10).toString());
        };

        // charValue :: Char -> Either String Int
        const charValueLR = c =>
            isAlpha(c) ? (
                isUpper(c) ? (
                    elem(c, 'AEIOU') ? Left(
                        'Unexpected vowel: ' + c
                    ) : Right(ord(c) - ord('A') + 10)
                ) : Left('Unexpected lower case character: ' + c)
            ) : isDigit(c) ? Right(
                parseInt(c, 10)
            ) : Left('Unexpected character: ' + c);

        // TESTS ------------------------------------------
        const [problems, checks] = Array.from(
            partitionEithers(map(s => bindLR(
                    checkSumLR(s),
                    c => Right(s + c)
                ),
                [
                    "710889", "B0YBKJ", "406566",
                    "B0YBLH", "228276", "B0YBKL",
                    "557910", "B0YBKR", "585284",
                    "B0YBKT", "B00030"
                ]
            ))
        );
        return unlines(
            0 < problems.length ? (
                problems
            ) : checks
        );
    };

    // GENERIC FUNCTIONS ----------------------------

    // Left :: a -> Either a b
    const Left = x => ({
        type: 'Either',
        Left: x
    });

    // Right :: b -> Either a b
    const Right = x => ({
        type: 'Either',
        Right: x
    });

    // Tuple (,) :: a -> b -> (a, b)
    const Tuple = (a, b) => ({
        type: 'Tuple',
        '0': a,
        '1': b,
        length: 2
    });

    // bindLR (>>=) :: Either a -> (a -> Either b) -> Either b
    const bindLR = (m, mf) =>
        undefined !== m.Left ? (
            m
        ) : mf(m.Right);

    // elem :: Eq a => a -> [a] -> Bool
    const elem = (x, xs) => xs.includes(x);

    // isAlpha :: Char -> Bool
    const isAlpha = c =>
        /[A-Za-z\u00C0-\u00FF]/.test(c);

    // isDigit :: Char -> Bool
    const isDigit = c => {
        const n = ord(c);
        return 48 <= n && 57 >= n;
    };

    // isUpper :: Char -> Bool
    const isUpper = c =>
        /[A-Z]/.test(c);

    // Returns Infinity over objects without finite length.
    // This enables zip and zipWith to choose the shorter
    // argument when one is non-finite, like cycle, repeat etc

    // length :: [a] -> Int
    const length = xs =>
        (Array.isArray(xs) || 'string' === typeof xs) ? (
            xs.length
        ) : Infinity;

    // map :: (a -> b) -> [a] -> [b]
    const map = (f, xs) =>
        (Array.isArray(xs) ? (
            xs
        ) : xs.split('')).map(f);

    // ord :: Char -> Int
    const ord = c => c.codePointAt(0);

    // partitionEithers :: [Either a b] -> ([a],[b])
    const partitionEithers = xs =>
        xs.reduce(
            (a, x) => undefined !== x.Left ? (
                Tuple(a[0].concat(x.Left), a[1])
            ) : Tuple(a[0], a[1].concat(x.Right)),
            Tuple([], [])
        );

    // rem :: Int -> Int -> Int
    const rem = (n, m) => n % m;

    // sum :: [Num] -> Num
    const sum = xs => xs.reduce((a, x) => a + x, 0);

    // take :: Int -> [a] -> [a]
    // take :: Int -> String -> String
    const take = (n, xs) =>
        'GeneratorFunction' !== xs.constructor.constructor.name ? (
            xs.slice(0, n)
        ) : [].concat.apply([], Array.from({
            length: n
        }, () => {
            const x = xs.next();
            return x.done ? [] : [x.value];
        }));

    // unlines :: [String] -> String
    const unlines = xs => xs.join('\n');

    // unwords :: [String] -> String
    const unwords = xs => xs.join(' ');

    // zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
    const zipWith = (f, xs, ys) => {
        const
            lng = Math.min(length(xs), length(ys)),
            as = take(lng, xs),
            bs = take(lng, ys);
        return Array.from({
            length: lng
        }, (_, i) => f(as[i], bs[i], i));
    };

    // MAIN ---
    return main();
})();
```

{{Out}}

```txt
7108899
B0YBKJ7
4065663
B0YBLH2
2282765
B0YBKL9
5579107
B0YBKR5
5852842
B0YBKT7
B000300
```



## jq

{{works with|jq|1.4}}
This implementation accepts strings with lowercase letters, but converts them to uppercase.

```jq
def ascii_upcase:
  explode | map( if 97 <= . and . <= 122 then . - 32  else . end) | implode;

def sedol_checksum:
  def encode(a): 10 + (a|explode[0]) - ("A"|explode[0]);
  . as $sed
  | [1,3,1,7,3,9] as $sw
  | reduce range(0;6) as $i
      (0;
       $sed[$i:$i+1] as $c
       | if ( "0123456789" | index($c) )
         then . + ($c|tonumber) * $sw[$i]
         else . + encode($c) * $sw[$i]
         end )
  | (10 - (. % 10)) % 10 ;

# error on error, else pass input to output
def check_valid_sedol:
  def has_vowel: 
    ("AEIOU"|explode) as $vowels
    | reduce explode[] as $c
        (false; if . then . else $vowels|index($c) end);

  if has_vowel then error( "\(.) is not a valid SEDOL code" )
  else .
  end
  | if length > 7 or length < 6 then
      error( "\(.) is too long or too short to be valid SEDOL")
    else .
    end;

def sedolize: 
  ascii_upcase as $in
  | $in
  | check_valid_sedol
  | .[0:6] as $sedol
  | ($sedol | sedol_checksum | tostring) as $sedolcheck 
  | ($sedol + $sedolcheck) as $ans
  | if length == 7 and $ans != $in then
         $ans + " (original \($in) has wrong checksum digit"
    else $ans
    end ;
sedolize
```

{{Out}}
 # Assuming sedol.txt contains the input in the task description
 $ jq -R -r -M -f sedol.jq sedol.txt
 ... (output is exactly as shown in the task description)


## Julia

{{works with|Julia|0.6}}


```julia
using Base.Test

function appendchecksum(chars::AbstractString)
    if !all(isalnum, chars) throw(ArgumentError("invalid SEDOL number '$chars'")) end
    weights = [1, 3, 1, 7, 3, 9, 1]

    s = 0
    for (w, c) in zip(weights, chars)
        s += w * parse(Int, c, 36)
    end
    return string(chars, (10 - s % 10) % 10)
end

tests = ["710889", "B0YBKJ", "406566", "B0YBLH", "228276", "B0YBKL", "557910", "B0YBKR", "585284", "B0YBKT", "B00030"]
csums = ["7108899", "B0YBKJ7", "4065663", "B0YBLH2", "2282765", "B0YBKL9", "5579107", "B0YBKR5", "5852842", "B0YBKT7", "B000300"]

@testset "Checksums" begin
    for (t, c) in zip(tests, csums)
        @test appendchecksum(t) == c
    end
end
```


{{out}}

```txt
Test Summary: | Pass  Total
Checksums     |   11     11
```



## Kotlin


```scala
// version 1.1.0

val weights = listOf(1, 3, 1, 7, 3, 9, 1)

fun sedol7(sedol6: String): String {
    if (sedol6.length != 6) throw IllegalArgumentException("Length of argument string must be 6")
    var sum = 0
    for (i in 0..5) {
        val c = sedol6[i] 
        val v = when (c) {
            in '0'..'9' -> c.toInt() - 48  
            in 'A'..'Z' -> c.toInt() - 55
            else        -> throw IllegalArgumentException("Argument string contains an invalid character")
        }
        sum += v * weights[i]
    }
    val check = (10 - (sum % 10)) % 10 
    return sedol6 + (check + 48).toChar()
}

fun main(args: Array<String>) {
    val sedol6s = listOf("710889", "B0YBKJ", "406566", "B0YBLH", "228276", "B0YBKL",
                         "557910", "B0YBKR", "585284", "B0YBKT", "B00030")
    for (sedol6 in sedol6s) println("$sedol6 -> ${sedol7(sedol6)}")
}
```


{{out}}

```txt

710889 -> 7108899
B0YBKJ -> B0YBKJ7
406566 -> 4065663
B0YBLH -> B0YBLH2
228276 -> 2282765
B0YBKL -> B0YBKL9
557910 -> 5579107
B0YBKR -> B0YBKR5
585284 -> 5852842
B0YBKT -> B0YBKT7
B00030 -> B000300

```



## Langur

{{trans|Go}}

```Langur
val .weight = [1,3,1,7,3,9]

val .csd = f(.code) {
    given len(.code) {
        case 0:
            return "nada, zip, zilch"
        case != 6:
            return "invalid length"
    }

    if matching(re/[^B-DF-HJ-NP-TV-Z0-9]/, .code) {
        return "invalid character(s)"
    }

    val .sum = foldfrom(
        f(.sum, .i, .c) .sum + toNumber(.c, 36) x .weight[.i],
        0,
        pseries len .code,
        split ZLS, .code,
    )

    toString 9 - (.sum - 1) rem 10
}

val .h = h{
    # invalid...
    "": 0,
    "123": 0,
    "A00030": 0,
    "E00030": 0,
    "I00030": 0,
    "O00030": 0,
    "U00030": 0,
    "β00030": 0,

    # valid...
    "710889": 9,
    "B0YBKJ": 7,
    "406566": 3,
    "B0YBLH": 2,
    "228276": 5,
    "B0YBKL": 9,
    "557910": 7,
    "B0YBKR": 5,
    "585284": 2,
    "B0YBKT": 7,
    "B00030": 0,
}

for .input in sort(keys .h) {
    val .d = .csd(.input)
    if len(.d) > 1 {
        writeln .input, ": ", .d
    } else {
        val .expect = toString .h[.input]
        write .input, .d
        writeln if .expect == .d {""} else {
            $" (SEDOL test failed; expected check digit \.expect;)"}
    }
}
```


{{out}}

```txt
: nada, zip, zilch
123: invalid length
2282765
4065663
5579107
5852842
7108899
A00030: invalid character(s)
B000300
B0YBKJ7
B0YBKL9
B0YBKR5
B0YBKT7
B0YBLH2
E00030: invalid character(s)
I00030: invalid character(s)
O00030: invalid character(s)
U00030: invalid character(s)
β00030: invalid character(s)

```



## Liberty BASIC


```lb

'adapted from BASIC solution
    mult(1) = 1: mult(2) = 3: mult(3) = 1
    mult(4) = 7: mult(5) = 3: mult(6) = 9

DO
        INPUT a$
        PRINT a$ + STR$(getSedolCheckDigit(a$))
LOOP WHILE a$ <> ""

FUNCTION getSedolCheckDigit (str$)
    IF LEN(str$) <> 6 THEN
        PRINT "Six chars only please"
        EXIT FUNCTION
    END IF
    str$ = upper$(str$)
    total = 0
    FOR i = 1 TO 6
        s$ = MID$(str$, i, 1)
        IF (s$ = "A") OR (s$ = "E") OR (s$ = "I") OR (s$ = "O") OR (s$ = "U") THEN
                PRINT "No vowels"
                EXIT FUNCTION
        END IF
        IF (ASC(s$) >= 48) AND (ASC(s$) <= 57) THEN
                total = total + VAL(s$) * mult(i)
        ELSE
                total = total + (ASC(s$) - 55) * mult(i)
        END IF

    NEXT i
    getSedolCheckDigit = (10 - (total MOD 10)) MOD 10
END FUNCTION

```



## M4


```M4
divert(-1)
changequote(`[',`]')
define([_bar],include(sedol.inp))
define([eachlineA],
   [ifelse(eval($2>0),1,
      [$3(substr([$1],0,$2))[]eachline(substr([$1],incr($2)),[$3])])])
define([eachline],[eachlineA([$1],index($1,[
]),[$2])])
define([_idx],
   [index([0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ],substr($1,$2,1))])
define([_wsum],
   [eval(_idx($1,0)+_idx($1,1)*3+_idx($1,2)+_idx($1,3)*7+_idx($1,4)*3+_idx($1,5)*9)])
define([checksum],
   [$1[]eval((10-_wsum($1)%10)%10)
])
divert
eachline(_bar,[checksum])
```



## Mathematica


```Mathematica
SEDOL[Code_?(Function[v,StringFreeQ[v,{"A","E","I","O","U"}]])]:=
Code<>ToString[10-Mod[ToExpression[Quiet[Flatten[Characters[Code]
/.x_?LetterQ->(ToCharacterCode[x]-55)]]].{1,3,1,7,3,9},10]]

Scan[Print[SEDOL[#]] &, {"710889","B0YBKJ","406566","B0YBLH","228276","B0YBKL","557910","B0YBKR","585284","B0YBKT","B00030","DUMMY"}]

->Output:
7108899
B0YBKJ7
4065663
B0YBLH2
2282765
B0YBKL9
5579107
B0YBKR5
5852842
B0YBKT7
B0003010
SEDOL[DUMMY] -> rejected
```



## Mercury

<lang>:- module sedol.
:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module char, int, list, require, string.

main(!IO) :-
    Input = [
        "710889",
        "B0YBKJ",
        "406566",
        "B0YBLH",
        "228276",
        "B0YBKL",
        "557910",
        "B0YBKR",
        "585284",
        "B0YBKT",
        "B00030"
     ],
     list.foldl(print_with_checksum, Input, !IO).

:- pred print_with_checksum(string::in, io::di, io::uo) is det.

print_with_checksum(S, !IO) :-
   io.format("%s%d\n", [s(S), i(sedol_checksum(S))], !IO).

:- func sedol_checksum(string) = int.

sedol_checksum(Sedol) = CheckSum :-
   Digits = string.foldr((func(C, A) = [to_sedol_code(C) | A]), Sedol, []),
   WeightedDigits = list.map_corresponding(int.times, Digits, [1, 3, 1, 7, 3, 9]),
   WeightedSum = list.foldl(int.plus, WeightedDigits, 0),
   CheckSum = (10 - (WeightedSum mod 10)) mod 10.

:- func to_sedol_code(char) = int.

to_sedol_code(Char) =
    ( if char.digit_to_int(Char, Code), not is_vowel(to_upper(Char))
    then Code
    else func_error("invalid SEDOL")
    ).

:- pred is_vowel(char::in) is semidet.

is_vowel('A').
is_vowel('E').
is_vowel('I').
is_vowel('O').
is_vowel('U').
```


=={{header|Modula-3}}==

```modula3
MODULE SEDOL EXPORTS Main;

IMPORT IO, Fmt, Text, Stdio;

EXCEPTION BadSedol(TEXT);

VAR test := ARRAY [1..10] OF TEXT {"710889", "B0YBKJ", "406566", "B0YBLH", 
                                   "228276", "B0YBKL", "557910", "B0YBKR", 
                                   "585284", "B0YBKT" };

PROCEDURE Check(sed: TEXT): INTEGER RAISES {BadSedol}=
  VAR 
    weights := ARRAY [0..5] OF INTEGER {1, 3, 1, 7, 3, 9};
    result, d: INTEGER;
    char: CHAR;
  BEGIN
    IF Text.Length(sed) # 6 THEN
      RAISE BadSedol("ERROR: Must be 6 digits.");
    END;
    result := 0;
    FOR i := 0 TO 5 DO
      char := Text.GetChar(sed, i);
      CASE char OF
      | '0'..'9' => d := ORD(char) - ORD('0');
      | 'B'..'D', 'F'..'H', 'J'..'N', 'P'..'T', 'V'..'Z' 
        => d := ORD(char) - ORD('A') + 10;
      ELSE
        RAISE BadSedol("ERROR: Must be numbers or (non-vowel) letters.");
      END;
      INC(result, d * weights[i]);
    END;
    result := (10 - (result MOD 10)) MOD 10;
    RETURN result;
  END Check;

BEGIN
  TRY
    FOR i := FIRST(test) TO LAST(test) DO
      IO.Put(test[i] & Fmt.Char(VAL(ORD('0') + Check(test[i]), CHAR)) & "\n");
    END;
  EXCEPT
  | BadSedol(text) => IO.Put(text & "\n", Stdio.stderr);
  END;
END SEDOL.
```

Output:

```txt
7108899
B0YBKJ7
4065663
B0YBLH2
2282765
B0YBKL9
5579107
B0YBKR5
5852842
B0YBKT7
```



## MUMPS


```MUMPS
SEDOL
 NEW A,B
SEDOL1
 READ !,"Enter the first 6 digits of a SEDOL: ",A
 SET B=$$SEDOLCHK(A)
 WRITE !,$SELECT($LENGTH(B)=1:"Full SEDOL is "_A_B,1:B)
 GOTO SEDOL1
 QUIT
SEDOLCHK(STOCK)
 NEW WT,VAL,I,CHK,C,FLAG
 SET WT="1317391",VAL=0,FLAG=0
 FOR I=1:1:6 SET C=$$TOUPPER($EXTRACT(STOCK,I)),VAL=VAL+($$SEDVAL(C)*$EXTRACT(WT,I)) SET:"AEIOUaeiou"[C FLAG=1
 SET:$LENGTH(STOCK)'=6 FLAG=1
 KILL WT,I,CHK,C
 QUIT $SELECT(FLAG:"INVALID",'FLAG:(10-(VAL#10))#10)
SEDVAL(X)
 QUIT $SELECT($ISVALIDNUM(X):X,1:$ASCII(X)-$ASCII("@")+9)
TOUPPER(X)
 NEW UP,LO
 SET UP="ABCDEFGHIJKLMNOPQRSTUVWXYZ",LO="abcdefghijklmnopqrstuvwxyz"
 QUIT $TRANSLATE(X,LO,UP)
```

Examples:

```txt
USER>D SEDOL^ROSETTA
 
Enter the first 6 digits of a SEDOL: 710889
Full SEDOL is 7108899
Enter the first 6 digits of a SEDOL: B0YBKJ
Full SEDOL is B0YBKJ7
Enter the first 6 digits of a SEDOL: 406566
Full SEDOL is 4065663
Enter the first 6 digits of a SEDOL: B0YBLH
Full SEDOL is B0YBLH2
Enter the first 6 digits of a SEDOL: 228276
Full SEDOL is 2282765
Enter the first 6 digits of a SEDOL: B0YBKL
Full SEDOL is B0YBKL9
Enter the first 6 digits of a SEDOL: 557910
Full SEDOL is 5579107
Enter the first 6 digits of a SEDOL: B0YBKR
Full SEDOL is B0YBKR5
Enter the first 6 digits of a SEDOL: 585284
Full SEDOL is 5852842
Enter the first 6 digits of a SEDOL: B0YBKT
Full SEDOL is B0YBKT7
Enter the first 6 digits of a SEDOL: B00030
Full SEDOL is B000300
Enter the first 6 digits of a SEDOL: Booo3o
INVALID
Enter the first 6 digits of a SEDOL: B123456
INVALID
```



## Nim


```nim
import strutils

proc c2v(c): int =
  assert c notin "AEIOU"
  let a = ord(c)
  if a < 65: a - 48
  else: a - 55

const weight = [1,3,1,7,3,9]

proc checksum(sedol): string =
  var tmp = 0
  for i,s in sedol:
    tmp += c2v(s) * weight[i]
  result = $((10 - (tmp mod 10)) mod 10)

for sedol in """710889
B0YBKJ
406566
B0YBLH
228276
B0YBKL
557910
B0YBKR
585284
B0YBKT
B00030""".splitLines():
  echo sedol, checksum(sedol)
```



## OCaml


```ocaml
let char2value c =
  assert (not (String.contains "AEIOU" c));
  match c with
  | '0'..'9' -> int_of_char c - int_of_char '0'
  | 'A'..'Z' -> int_of_char c - int_of_char 'A' + 10
  | _ -> assert false
 
let sedolweight = [1;3;1;7;3;9]
 
let explode s =
  s |> String.to_seq |> List.of_seq
 
let checksum sedol =
  let tmp = List.fold_left2 (fun sum ch weight -> sum + char2value ch * weight)
              0 (explode sedol) sedolweight in
  string_of_int ((10 - (tmp mod 10)) mod 10) ;;
 
List.iter (fun sedol -> print_endline (sedol ^ checksum sedol))
  [ "710889";
    "B0YBKJ";
    "406566";
    "B0YBLH";
    "228276";
    "B0YBKL";
    "557910";
    "B0YBKR";
    "585284";
    "B0YBKT" ]
```



## Oforth



```Oforth
func: sedol(s)
   [ 1, 3, 1, 7, 3, 9 ] s 
   zipWith(#[ dup isDigit ifTrue: [ '0' - ] else: [ 'A' - 10 + ] * ]) sum
   10 mod 10 swap - 10 mod
   StringBuffer new s << swap '0' + <<c ;
```


{{out}}

```txt

[ "710889", "B0YBKJ", "406566", "B0YBLH", "228276", "B0YBKL", "557910", "B0YBKR", "585284", "B0YBKT", "B00030" ]
apply(#[ sedol println ])
7108899
B0YBKJ7
4065663
B0YBLH2
2282765
B0YBKL9
5579107
B0YBKR5
5852842
B0YBKT7
B000300

```



## Pascal

{{works with|Free_Pascal}}

```pascal
program Sedols(output);

function index(c: char): integer;
  const
    alpha = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  var
    i: integer;
  begin
    index := 0;
    for i := low(alpha) to high(alpha) do
      if c = alpha[i] then
        index := i;
  end;

function checkdigit(c: string): char;
  const
    weight: array [1..6] of integer = (1, 3, 1, 7, 3, 9);
  var
    i, sum: integer;
  begin
    sum := 0;
    for i := 1 to 6 do
      sum := sum + (index(c[i]) - 1) * weight[i];
    checkdigit := char((10 - (sum mod 10)) mod 10 + 48);
  end;

const
  codes: array [1..11] of string =
    ('710889', 'B0YBKJ', '406566', 'B0YBLH',
     '228276', 'B0YBKL', '557910', 'B0YBKR',
     '585284', 'B0YBKT', 'B00030');

var
  seforl: string;
  i: integer;
 
begin
  for i := low(codes) to high(codes) do
  begin
    seforl := codes[i];
    setlength(seforl, 7);
    seforl[7] := checkdigit(codes[i]);
    writeln(codes[i], ' -> ', seforl);
  end;
end.
```

Output:

```txt
% ./Sedols 
710889 -> 7108899
B0YBKJ -> B0YBKJ7
406566 -> 4065663
B0YBLH -> B0YBLH2
228276 -> 2282765
B0YBKL -> B0YBKL9
557910 -> 5579107
B0YBKR -> B0YBKR5
585284 -> 5852842
B0YBKT -> B0YBKT7
B00030 -> B000300
```



## Perl

This program reads from standard input.

```perl
use List::Util qw(sum);
use POSIX qw(strtol);

sub zip(&\@\@) {
  my $f = shift;
  my @a = @{shift()};
  my @b = @{shift()};
  my @result;
  push(@result, $f->(shift @a, shift @b)) while @a && @b;
  return @result;
}

my @weights = (1, 3, 1, 7, 3, 9);
sub sedol($) {
  my $s = shift;
  $s =~ /[AEIOU]/ and die "No vowels";
  my @vs = map {(strtol $_, 36)[0]} split //, $s;
  my $checksum = sum (zip {$_[0] * $_[1]} @vs, @weights);
  my $check_digit = (10 - $checksum % 10) % 10;
  return $s . $check_digit;
}

while (<>) {
    chomp;
    print sedol($_), "\n";
}
```



## Perl 6

{{trans|Perl}}
{{Works with|rakudo|2015-12-17}}

```perl6
sub sedol( Str $s ) {
    die 'No vowels allowed' if $s  ~~ /<[AEIOU]>/;
    die 'Invalid format'    if $s !~~ /^ <[0..9B..DF..HJ..NP..TV..Z]>**6 $ /;

    my %base36 = (flat 0..9, 'A'..'Z') »=>« ^36;
    my @weights = 1, 3, 1, 7, 3, 9;

    my @vs = %base36{ $s.comb };
    my $checksum = [+] @vs Z* @weights;
    my $check_digit = (10 - $checksum % 10) % 10;
    return $s ~ $check_digit;
}

say sedol($_) for <
    710889
    B0YBKJ
    406566
    B0YBLH
    228276
    B0YBKL
    557910
    B0YBKR
    585284
    B0YBKT
    B00030
>;
```



## Phix


```Phix
type string6(object s)
    return string(s) and length(s)=6
end type

type sedolch(integer ch)
    return ch>='0' and ch<='Z' and (ch<='9' or ch>='A') and not find(ch,"AEIOU")
end type

function sedol(string6 t)
sedolch c
integer s = 0
    for i=1 to 6 do
        c = t[i]
        s += iff(c>='A'?c-'A'+10:c-'0')*{1,3,1,7,3,9}[i]
    end for
    return t & mod(10-mod(s,10),10)+'0'
end function

constant tests = {"710889",
                  "B0YBKJ",
                  "406566",
                  "B0YBLH",
                  "228276",
                  "B0YBKL",
                  "557910",
                  "B0YBKR",
                  "585284",
                  "B0YBKT",
                  "B00030"}
for i=1 to length(tests) do
    ?sedol(tests[i])
end for
```

{{out}}

```txt

"7108899"
"B0YBKJ7"
"4065663"
"B0YBLH2"
"2282765"
"B0YBKL9"
"5579107"
"B0YBKR5"
"5852842"
"B0YBKT7"
"B000300"

```



## PHP


```php
function char2value($c) {
  assert(stripos('AEIOU', $c) === FALSE);
  return intval($c, 36);
}

$sedolweight = array(1,3,1,7,3,9);

function checksum($sedol) {
    global $sedolweight;
    $tmp = array_sum(array_map(create_function('$ch, $weight', 'return char2value($ch) * $weight;'),
                               str_split($sedol), $sedolweight)
                    );
    return strval((10 - ($tmp % 10)) % 10);
}

foreach (array('710889',
               'B0YBKJ',
               '406566',
               'B0YBLH',
               '228276',
               'B0YBKL',
               '557910',
               'B0YBKR',
               '585284',
               'B0YBKT') as $sedol)
    echo $sedol, checksum($sedol), "\n";
```



## PicoLisp


```PicoLisp
(de sedol (Str)
   (pack Str
      (char
         (+ `(char "0")
            (%
               (- 10
                  (%
                     (sum
                        '((W C)
                           (cond
                              ((>= "9" C "0")
                                 (* W (format C)) )
                              ((>= "Z" (setq C (uppc C)) "A")
                                 (* W (+ 10 (- (char C) `(char "A")))) ) ) )
                        (1 3 1 7 3 9)
                        (chop Str) )
                     10 ) )
               10 ) ) ) ) )

(for S '("710889" "B0YBKJ" "406566" "B0YBLH" "228276" "B0YBKL" "557910" "B0YBKR" "585284" "B0YBKT" "B00030")
   (prinl (sedol S)) )
```



## PL/I


```PLI
/* Compute SEDOLs; includes check for invalid characters. */
sedol: procedure options (main); /* 3 March 2012 */
   declare alphabet character (36) static initial
      ('0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ');
   declare weight (6) fixed static initial (1, 3, 1, 7, 3, 9);
   declare s character (6);
   declare (i, v, k) fixed;

   do while ('1'b);
      get edit (s) (a(6));
      put skip edit (s) (a);
      /* Check for invalid characters: */
      if verify(s, '0123456789BCDFGHJKLMNPQRSTVWXYZ') > 0 then stop;
      v = 0;
      do i = 1 to 6;
         k = index(alphabet, substr(s, i, 1)) - 1;
         v = v + weight(i) * k;
      end;
      k = mod(v, 10);
      v = mod(10 - k, 10);
      put edit (s, v) (x(2), a, f(1)); put edit ('  ') (a);
   end;
end sedol;
```


{{out}}

```txt

710889  7108899
B0YBKJ  B0YBKJ7
406566  4065663
B0YBLH  B0YBLH2
228276  2282765
B0YBKL  B0YBKL9
557910  5579107
B0YBKR  B0YBKR5
585284  5852842
B0YBKT  B0YBKT7
B00030  B000300

```



## Potion

No extra credit.

```potion
sedolnum = (c) :
   if ("0" ord <= c ord and c ord <= "9" ord): c number integer.
   else: 10 + c ord - "A" ord.
.

sedol = (str) :
   weight = (1, 3, 1, 7, 3, 9)
   sum = 0
   6 times (i) :
      sum = sum + sedolnum(str(i)) * weight(i)
   .
   (str, (10 - (sum % 10)) % 10) join
.
```



## PowerShell


```powershell
function Add-SEDOLCheckDigit
    {
    Param ( #  Validate input as six-digit SEDOL number
            [ValidatePattern( "^[0123456789bcdfghjklmnpqrstvwxyz]{6}$" )]
            [parameter ( Mandatory = $True ) ]
            [string]
            $SixDigitSEDOL )
 
    #  Convert to array of single character strings, using type char as an intermediary
    $SEDOL = [string[]][char[]]$SixDigitSEDOL
 
    #  Define place weights
    $Weight = @( 1, 3, 1, 7, 3, 9 )
 
    #  Define character values (implicit in 0-based location within string)
    $Characters = "0123456789abcdefghijklmnopqrstuvwxyz"
 
    $CheckSum = 0
   
    #  For each digit, multiply the character value by the weight and add to check sum
    0..5 | ForEach { $CheckSum += $Characters.IndexOf( $SEDOL[$_].ToLower() ) * $Weight[$_] }
 
    #  Derive the check digit from the partial check sum
    $CheckDigit = ( 10 - $CheckSum % 10 ) % 10
 
    #  Return concatenated result
    return ( $SixDigitSEDOL + $CheckDigit )
    }
 
#  Test
$List = @(
    "710889"
    "B0YBKJ"
    "406566"
    "B0YBLH"
    "228276"
    "B0YBKL"
    "557910"
    "B0YBKR"
    "585284"
    "B0YBKT"
    "B00030"
    )
 
ForEach ( $PartialSEDOL in $List )
    {
    Add-SEDOLCheckDigit -SixDigitSEDOL $PartialSEDOL
    }
```

{{out}}

```txt
7108899
B0YBKJ7
4065663
B0YBLH2
2282765
B0YBKL9
5579107
B0YBKR5
5852842
B0YBKT7
B000300
```



## PureBasic


```PureBasic
Procedure.s SEDOLs(rawstring$)
  Protected i, j, sum, c, m
  For i=1 To Len(rawstring$)
    c=Asc(Mid(rawstring$,i,1))
    Select c
      Case Asc("0") To Asc("9")
        j=Val(Mid(rawstring$,i,1))
      Default
        j=c-Asc("A")
    EndSelect
    Select i
      Case 1, 3, 7:  m=1
      Case 2, 5:     m=3
      Case 4:        m=7
      Default:       m=9
    EndSelect
    sum+j*m
  Next
  sum=(10-(sum%10))%10
  ProcedureReturn rawstring$+Str(sum)
EndProcedure

Define result$, i
Restore Tests
For i=0 To 10
  Read.s  SEDOL$
  result$+SEDOLs(SEDOL$)
  If i%2
    result$+#CRLF$
  ElseIf i<10
    result$+", "
  EndIf
Next
MessageRequester("SEDOLs","Result"+#CRLF$+result$)

DataSection
  Tests:
  Data.s  "710889","B0YBKJ","406566","B0YBLH","228276"
  Data.s  "B0YBKL","557910","B0YBKR","585284","B0YBKT","B00030"
EndDataSection
```


[[Image:PB SEDOL.png]]


## Python


```python
def char2value(c):
  assert c not in 'AEIOU', "No vowels"
  return int(c, 36)

sedolweight = [1,3,1,7,3,9]

def checksum(sedol):
    tmp = sum(map(lambda ch, weight: char2value(ch) * weight,
                  sedol, sedolweight)
               )
    return str((10 - (tmp % 10)) % 10)

for sedol in '''
    710889
    B0YBKJ
    406566
    B0YBLH
    228276
    B0YBKL
    557910
    B0YBKR
    585284
    B0YBKT
    '''.split():
    print sedol + checksum(sedol)
```



Or, combining ''reduce'' with an option type – handling disallowed characters without assertion errors:

{{Works with|Python|3.7}}

```python
'''SEDOL checksum digits'''

from functools import reduce


# sedolCheckSumDigitLR :: String -> Either String Char
def sedolCheckSumDigitLR(s):
    '''Either an explanatory message, or a
       checksum digit character to append
       to a given six-character SEDOL string.
    '''
    def goLR(lr, cn):
        c, n = cn
        return bindLR(lr)(
            lambda a: bindLR(sedolValLR(c))(
                lambda x: Right(a + x * n)
            )
        )
    return bindLR(
        reduce(
            goLR,
            zip(s, [1, 3, 1, 7, 3, 9]),
            Right(0)
        )
    )(lambda d: Right(str((10 - (d % 10)) % 10)))


# sedolValLR :: Char -> Either String Char
def sedolValLR(c):
    '''Either an explanatory message, or the
       SEDOL value of a given character.
    '''
    return Right(int(c, 36)) if (
        c not in 'AEIOU'
    ) else Left('Unexpected vowel in SEDOL string: ' + c)


# TEST -------------------------------------------------
def main():
    '''Append checksums where valid.'''

    print(
        fTable(__doc__ + ':\n')(str)(
            either(str)(str)
        )(sedolCheckSumDigitLR)(
            '''710889
               B0YBKJ
               406566
               B0YBLH
               228276
               B0YBKL
               BOYBKL
               557910
               B0YBKR
               585284
               B0YBKT
               B00030
            '''.split()
        )
    )


# GENERIC -------------------------------------------------


# Left :: a -> Either a b
def Left(x):
    '''Constructor for an empty Either (option type) value
       with an associated string.'''
    return {'type': 'Either', 'Right': None, 'Left': x}


# Right :: b -> Either a b
def Right(x):
    '''Constructor for a populated Either (option type) value'''
    return {'type': 'Either', 'Left': None, 'Right': x}


# bindLR (>>=) :: Either a -> (a -> Either b) -> Either b
def bindLR(m):
    '''Either monad injection operator.
       Two computations sequentially composed,
       with any value produced by the first
       passed as an argument to the second.'''
    return lambda mf: (
        mf(m.get('Right')) if None is m.get('Left') else m
    )


# compose (<<<) :: (b -> c) -> (a -> b) -> a -> c
def compose(g):
    '''Right to left function composition.'''
    return lambda f: lambda x: g(f(x))


# either :: (a -> c) -> (b -> c) -> Either a b -> c
def either(fl):
    '''The application of fl to e if e is a Left value,
       or the application of fr to e if e is a Right value.'''
    return lambda fr: lambda e: fl(e['Left']) if (
        None is e['Right']
    ) else fr(e['Right'])


# fTable :: String -> (a -> String) ->
#                     (b -> String) ->
#        (a -> b) -> [a] -> String
def fTable(s):
    '''Heading -> x display function -> fx display function ->
          f -> value list -> tabular string.'''
    def go(xShow, fxShow, f, xs):
        w = max(map(compose(len)(xShow), xs))
        return s + '\n' + '\n'.join([
            xShow(x).rjust(w, ' ') + ' -> ' + fxShow(f(x)) for x in xs
        ])
    return lambda xShow: lambda fxShow: (
        lambda f: lambda xs: go(
            xShow, fxShow, f, xs
        )
    )


# MAIN ---
if __name__ == '__main__':
    main()
```

{{Out}}

```txt
SEDOL checksum digits:

710889 -> 9
B0YBKJ -> 7
406566 -> 3
B0YBLH -> 2
228276 -> 5
B0YBKL -> 9
BOYBKL -> Unexpected vowel in SEDOL string: O
557910 -> 7
B0YBKR -> 5
585284 -> 2
B0YBKT -> 7
B00030 -> 0
```



## Q


```q
scd:{
    v:{("i"$x) - ?[("0"<=x) & x<="9"; "i"$"0"; -10+"i"$"A"]} each x; / Turn characters of SEDOL into their values
    w:sum v*1 3 1 7 3 9;        / Weighted sum of values
    d:(10 - w mod 10) mod 10;   / Check digit value
    x,"c"$(("i"$"0")+d)         / Append to SEDOL
}

scd each ("710889";"B0YBKJ";"406566";"B0YBLH";"228276";"B0YBKL";"557910";"B0YBKR";"585284";"B0YBKT";"B00030")
```



## R


```rsplus
# Read in data from text connection
datalines <- readLines(tc <- textConnection("710889
B0YBKJ
406566
B0YBLH
228276
B0YBKL
557910
B0YBKR
585284
B0YBKT")); close(tc)

# Check data valid
checkSedol <- function(datalines)
{
   ok <- grep("^[[:digit:][:upper:]]{6}$", datalines)
   if(length(ok) < length(datalines))
   {
      stop("there are invalid lines") 
   }
}
checkSedol(datalines)

# Append check digit
appendCheckDigit <- function(x) 
{   
   if(length(x) > 1) return(sapply(x, appendCheckDigit)) 
   ascii <- as.integer(charToRaw(x))
   scores <- ifelse(ascii < 65, ascii - 48, ascii - 55)
   weights <- c(1, 3, 1, 7, 3, 9)
   chkdig <- (10 - sum(scores * weights) %% 10) %% 10
   paste(x, as.character(chkdig), sep="")
}
withchkdig <- appendCheckDigit(datalines)

#Print in format requested
writeLines(withchkdig)
```



## Racket


```racket
#lang racket
;;; Since the Task gives us unchecksummed and checksummed SEDOLs, and
;;; we'll just take a list of the output SEDOLs and remove their last
;;; characters for the input
(define output-SEDOLS
  (list "7108899" "B0YBKJ7" "4065663"
        "B0YBLH2" "2282765" "B0YBKL9"
        "5579107" "B0YBKR5" "5852842"
        "B0YBKT7" "B000300"))
(define (output->input-SEDOL S) (substring S 0 6))
(define input-SEDOLS (map output->input-SEDOL output-SEDOLS))

;;; checksum calculation
(define (SEDOL-character-value c)
  (if (char-numeric? c)
      (- (char->integer c) (char->integer #\0))
      (+ 10 (- (char->integer c) (char->integer #\A)))))
(define (SEDOL-character-sum S)
  (for/sum ((c S)  ; if we run out of c's before the final 1 in weight, we'll have the unchecksummed weighted sum
            (weight (in-list '(1 3 1 7 3 9 1))))
    (* weight (SEDOL-character-value c))))
(define (SEDOL-checksum S) (number->string (modulo (- 10 (SEDOL-character-sum S)) 10)))

;;; build output from input
(define (SEDOL-append-checksum S) (string-append S (SEDOL-checksum S)))

;;; Extra credit -- according to wikipedia:
;;; "SEDOLs are seven characters in length"
;;; "vowels are never used"
;;; there seems to be no statement as to case, but we'll assert that too!
;;;
;;; valid-SEDOL? is a predicate... it doesn't report a reason
(define (invalid-SEDOL? S)
  (define (invalid-SEDOL-character? c)
    (if
     (and (not (char-upper-case? c)) (not (char-numeric? c)))
     (format "contains non upper case/non numeric ~a" c)
     (case c [(#\A #\E #\I #\O #\U) (format "contains vowel ~a" c)] [else #f])))
  (cond
    [(< (string-length S) 7) "too few characters"]
    [(> (string-length S) 7) "too many characters"]
    [(not (zero? (modulo (SEDOL-character-sum S) 10))) "invalid checksum"]
    [(for/first ((c S) #:when (invalid-SEDOL-character? c)) c) => identity]
    [else #f])) ; a.k.a. valid!

(module+ main
  (for* ((S input-SEDOLS))
    (displayln (SEDOL-append-checksum S)))
  (newline)
  (displayln "Extra Credit!")
  (displayln (invalid-SEDOL? "B0YBKT7")) ; expect #f output
  (displayln (invalid-SEDOL? "B000301")) ; expect "invalid checksum" output
  )

(module+ test
  (require rackunit)  
  (check-= (SEDOL-character-value #\3) 3 0)
  (check-= (SEDOL-character-value #\B) 11 0)
  (check-equal? (invalid-SEDOL? "B000301") "invalid checksum")
  (for ((S output-SEDOLS))
    (check-false (invalid-SEDOL? S))
    (check-equal? (SEDOL-append-checksum (substring S 0 6))
                  S (format "test SEDOL for ~a" S))))
```


Output:

```txt
7108899
B0YBKJ7
4065663
B0YBLH2
2282765
B0YBKL9
5579107
B0YBKR5
5852842
B0YBKT7
B000300

Extra Credit!
#f
invalid checksum
```



## REXX


```txt

         ╔════════════════════════════════════════════════════════════════════╗
         ║  If the SEDOL is 6 characters, a check digit is added.             ║
         ║                                                                    ║
         ║  If the SEDOL is 7 characters, a check digit is created and it's   ║
         ║  verified that it's equal to the check digit already on the SEDOL. ║
         ╚════════════════════════════════════════════════════════════════════╝

```


```rexx
/*REXX program computes the  check digit (last digit) for six or seven character SEDOLs.*/
@abcU    = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'          /*the uppercase Latin alphabet.        */
alphaDigs= '0123456789'@abcU                     /*legal characters,  and then some.    */
allowable=space(translate(alphaDigs,,'AEIOU'),0) /*remove the vowels from the alphabet. */
weights  = 1317391                               /*various weights for SEDOL characters.*/
@.=                                              /* [↓]  the ARG statement capitalizes. */
arg @.1 .                                        /*allow a user─specified  SEDOL from CL*/
if @.1==''  then do                              /*if none, then assume eleven defaults.*/
                 @.1  =  710889                  /*if all numeric, we don't need quotes.*/
                 @.2  = 'B0YBKJ'
                 @.3  =  406566
                 @.4  = 'B0YBLH'
                 @.5  =  228276
                 @.6  = 'B0YBKL'
                 @.7  =  557910
                 @.8  = 'B0YBKR'
                 @.9  =  585284
                 @.10 = 'B0YBKT'
                 @.11 = 'B00030'
                 end

      do j=1  while  @.j\=='';      sedol=@.j    /*process each of the specified SEDOLs.*/
      L=length(sedol)
      if L<6 | L>7        then call ser "SEDOL isn't a valid length"
      if left(sedol,1)==9 then call swa 'SEDOL is reserved for end user allocation'
      _=verify(sedol, allowable)
      if _\==0            then call ser 'illegal character in SEDOL:'  substr(sedol, _, 1)
      sum=0                                      /*the  checkDigit  sum  (so far).      */
              do k=1  for 6                      /*process each character in the SEDOL. */
              sum=sum + ( pos( substr(sedol, k, 1), alphaDigs) -1) * substr(weights, k, 1)
              end   /*k*/

      chkDig= (10-sum//10) // 10
      r=right(sedol, 1)
      if L==7 & chkDig\==r  then call ser sedol, 'invalid check digit:' r
      say 'SEDOL:'   left(sedol,15)      'SEDOL + check digit ───► '   left(sedol,6)chkDig
      end       /*j*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
sed:  say;  say 'SEDOL:'  sedol;          say;                          return
ser:  say;  say '***error***'   arg(1);   call sed;                     exit 13
swa:  say;  say '***warning***' arg(1);   say;                          return
```

'''output'''   when using the defaults:

```txt

SEDOL: 710889          SEDOL + check digit ───►  7108899
SEDOL: B0YBKJ          SEDOL + check digit ───►  B0YBKJ7
SEDOL: 406566          SEDOL + check digit ───►  4065663
SEDOL: B0YBLH          SEDOL + check digit ───►  B0YBLH2
SEDOL: 228276          SEDOL + check digit ───►  2282765
SEDOL: B0YBKL          SEDOL + check digit ───►  B0YBKL9
SEDOL: 557910          SEDOL + check digit ───►  5579107
SEDOL: B0YBKR          SEDOL + check digit ───►  B0YBKR5
SEDOL: 585284          SEDOL + check digit ───►  5852842
SEDOL: B0YBKT          SEDOL + check digit ───►  B0YBKT7
SEDOL: B00030          SEDOL + check digit ───►  B000300

```



## Ring


```ring

see sedol("710889") + nl
see sedol("B0YBKJ") + nl
see sedol("406566") + nl
see sedol("B0YBLH") + nl
see sedol("228276") + nl
see sedol("B0YBKL") + nl
see sedol("557910") + nl
see sedol("B0YBKR") + nl
see sedol("585284") + nl
see sedol("B0YBKT") + nl
see sedol("B00030") + nl
 
func sedol d
     d = upper(d)
     s = 0 
     weights  = [1, 3, 1, 7, 3, 9]
     for i = 1 to 6
         a = substr(d,i,1)
         if ascii(a) >= 48 and ascii(a) <= 57
            s += number(a) * weights[i]
         else
            s += (ascii(a) - 55) * weights[i] ok
     next
     return d + (10 - (s % 10)) % 10

```

Output:

```txt

7108899
B0YBKJ7
4065663
B0YBLH2
2282765
B0YBKL9
5579107
B0YBKR5
5852842
B0YBKT7
B000300

```



## Ruby


```ruby
Sedol_char = "0123456789BCDFGHJKLMNPQRSTVWXYZ"
Sedolweight = [1,3,1,7,3,9]

def char2value(c)
  raise ArgumentError, "Invalid char #{c}" unless Sedol_char.include?(c)
  c.to_i(36)
end
 
def checksum(sedol)
  raise ArgumentError, "Invalid length" unless sedol.size == Sedolweight.size
  sum = sedol.chars.zip(Sedolweight).sum{|ch, weight| char2value(ch) * weight }
  ((10 - (sum % 10)) % 10).to_s
end
 
data = %w(710889
          B0YBKJ
          406566
          B0YBLH
          228276
          B0YBKL
          557910
          B0YBKR
          585284
          B0YBKT
          B00030
          C0000
          1234567
          00000A)
 
data.each do |sedol|
  print "%-8s " % sedol
  begin
    puts sedol + checksum(sedol)
  rescue => e
    p e
  end
end
```

{{out}}

```txt

710889   7108899
B0YBKJ   B0YBKJ7
406566   4065663
B0YBLH   B0YBLH2
228276   2282765
B0YBKL   B0YBKL9
557910   5579107
B0YBKR   B0YBKR5
585284   5852842
B0YBKT   B0YBKT7
B00030   B000300
C0000    #<ArgumentError: Invalid length>
1234567  #<ArgumentError: Invalid length>
00000A   #<ArgumentError: No vowels>

```



## Scala


```scala
class SEDOL(s: String) {
  require(s.size == 6 || s.size == 7, "SEDOL length must be 6 or 7 characters")
  require(s.size == 6 || s(6).asDigit == chksum, "Incorrect SEDOL checksum")
  require(s forall (c => !("aeiou" contains c.toLower)), "Vowels not allowed in SEDOL")
  def chksum = 10 - ((s zip List(1, 3, 1, 7, 3, 9) map { case (c, w) => c.asDigit * w } sum) % 10)
  override def toString = s.take(6) + chksum
}
```


Test cases:


```txt

scala> """710889
     | B0YBKJ
     | 406566
     | B0YBLH
     | 228276
     | B0YBKL
     | 557910
     | B0YBKR
     | 585284
     | B0YBKT""".lines.map(_.trim).foreach(s => println(new SEDOL(s)))
7108899
B0YBKJ7
4065663
B0YBLH2
2282765
B0YBKL9
5579107
B0YBKR5
5852842
B0YBKT7

```


Validations:


```txt

scala> new SEDOL("12")
java.lang.IllegalArgumentException: requirement failed: SEDOL length must be 6 or 7 characters

scala> new SEDOL("7108890")
java.lang.IllegalArgumentException: requirement failed: Incorrect SEDOL checksum

scala> new SEDOL("71088A")
java.lang.IllegalArgumentException: requirement failed: Vowels not allowed in SEDOL

```



## Seed7


```seed7
$ include "seed7_05.s7i";

const func char: sedolCheckDigit (in string: sedol) is func
  result
    var char: checkDigit is ' ';
  local
    const array integer: weight is [] (1, 3, 1, 7, 3, 9);
    var char: ch is ' ';
    var integer: index is 0;
    var integer: item is 0;
    var integer: sum is 0;
   begin
     for ch key index range sedol do
       case ch of
         when {'0' .. '9'}:
           item := ord(ch) - ord('0');
         when {'A' .. 'Z'} - {'A', 'E', 'I', 'O', 'U'}:
           item := ord(ch) - ord('A') + 10;
         otherwise:
           raise RANGE_ERROR;
       end case;
       sum +:= item * weight[index];
     end for;
     checkDigit := chr(-sum mod 10 + ord('0'));
   end func;
 
const proc: main is func
  local
    var string: sedol is "";
  begin
    for sedol range [] ("710889", "B0YBKJ", "406566", "B0YBLH", "228276", "B0YBKL",
                        "557910", "B0YBKR", "585284", "B0YBKT", "B00030") do
      writeln(sedol <& sedolCheckDigit(sedol));
    end for;
  end func;
```


{{out}}

```txt

7108899
B0YBKJ7
4065663
B0YBLH2
2282765
B0YBKL9
5579107
B0YBKR5
5852842
B0YBKT7
B000300

```



## Sidef

{{trans|Perl 6}}

```ruby
func sedol(s) {

    die 'No vowels allowed' if (s ~~ /[AEIOU]/);
    die 'Invalid format'    if (s !~ /^[0-9B-DF-HJ-NP-TV-Z]{6}$/);

    const base36 = ((@(0..9) + @('A'..'Z')) ~Z @(0..35) -> flatten.to_h);
    const weights = [1, 3, 1, 7, 3, 9];

    var vs = [base36{ s.chars... }];
    var checksum = (vs ~Z* weights -> sum);
    var check_digit = ((10 - checksum%10) % 10);
    return (s + check_digit);
}

%w(
    710889
    B0YBKJ
    406566
    B0YBLH
    228276
    B0YBKL
    557910
    B0YBKR
    585284
    B0YBKT
    B00030
).each { |s|
    say sedol(s);
}
```

{{out}}

```txt

7108899
B0YBKJ7
4065663
B0YBLH2
2282765
B0YBKL9
5579107
B0YBKR5
5852842
B0YBKT7
B000300

```



## Smalltalk

{{works with|GNU Smalltalk}}

```smalltalk
String extend [
  includesAnyOf: aSet [
      aSet do: [ :e | (self includes: e) ifTrue: [ ^true ] ].
      ^false
  ]
].
```



```smalltalk
Object subclass: SEDOL [
  |weight charList|

  initialize [
    weight := Array from: { 1. 3. 1. 7. 3. 9 }.
    charList :=
      ('ABCDEFGHIJKLMNOPQRSTUVWXYZ' asOrderedCollection)
      collect: [ :c | ('AEIOU' includes: c) ifTrue: [ nil ] ifFalse: [ c ] ].    
  ]

  SEDOL class >> new [
     ^ (self basicNew) initialize
  ]

  "to be considered private"
  blindCheckDigit: aSEDOL [ |sum|
     sum := 0.
     aSEDOL keysAndValuesDo: [ :i :c |
        ('0123456789' includes: c)
          ifTrue: [  sum := sum + 
                            ((weight at: i) * 
                            (Number readFrom: (c asString readStream))).
                  ]
          ifFalse: [ sum := sum + (((charList indexOf: c) + 9) *
                            (weight at: i))
                   ]
     ].
     ^ ((10 - (sum rem: 10)) rem: 10) displayString
  ]

  checked: aSEDOL [
     (aSEDOL size < 6) |
     (aSEDOL size > 7) |
     (aSEDOL asUppercase includesAnyOf: 'AEIOU' asSet )
     ifTrue: [ SystemExceptions.InvalidArgument
                 signalOn: aSEDOL
                 reason: 'Not a valid SEDOL'
             ]
     ifFalse: [ |t| t := aSEDOL copyFrom: 1 to: 6.
                ^ t , (self blindCheckDigit: t)
              ]
  ]
].
```



```smalltalk
|sedol|
sedol := SEDOL new.
{  '710889'.
   'B0YBKJ'.
   '406566'.
   'B0YBLH'.
   '228276'.
   'B0YBKL'.
   '557910'.
   'B0YBKR'.
   '585284'.
   'B0YBKT' } do: [ :c | (sedol checked: c) displayNl ]
```



## SQL PL

{{works with|Db2 LUW}} version 9.7 or higher.
With SQL PL:

```sql pl

--#SET TERMINATOR @

SET SERVEROUTPUT ON@

CREATE OR REPLACE FUNCTION CHECK_SEDOL (
  IN TEXT VARCHAR(6)
   ) RETURNS VARCHAR(7)
 BEGIN
  DECLARE TYPE SEDOL AS CHAR(1) ARRAY [6];
--declare text varchar(6) default 'B12345';
  DECLARE WEIGHT SEDOL;
  DECLARE I SMALLINT;
  DECLARE SENTENCE VARCHAR(256);
  DECLARE CHAR_AT CHAR(1);
  DECLARE OUTPUT CHAR(1);
  DECLARE SUM SMALLINT;
  DECLARE CHECK SMALLINT;
  DECLARE INVALID_CHAR CONDITION FOR SQLSTATE '22004' ;
  DECLARE STMT STATEMENT;

  -- Converts all to upper.
  SET TEXT = UPPER (TEXT);
  -- CALL DBMS_OUTPUT.PUT_LINE(TEXT);
  -- Checks the characters.
  SET I = 1;
  WHILE (I <= 6) DO
   SET CHAR_AT = SUBSTR(TEXT, I, 1);
   -- CALL DBMS_OUTPUT.PUT_LINE('Char ' || CHAR_AT);
   SET SENTENCE = 'SET ? = (SELECT SEDOL FROM (SELECT ''' || CHAR_AT
     || ''' SEDOL FROM SYSIBM.SYSDUMMY1) WHERE SEDOL IN (''B'',''C'',''D'',''F'',''G'',''H'',''J'','
     || '''K'',''L'',''M'',''N'',''P'',''Q'',''R'',''S'',''T'',''V'',''W'',''X'',''Y'',''Z'',''0'','
     || '''1'',''2'',''3'',''4'',''5'',''6'',''7'',''8'',''9''))';
   PREPARE STMT FROM SENTENCE;
   EXECUTE STMT INTO OUTPUT;
   IF (OUTPUT IS NULL) THEN
    SIGNAL INVALID_CHAR;
   END IF;
   SET I = I + 1;
  END WHILE;
 
  -- Assigns weight
  SET WEIGHT[1] = '1';
  SET WEIGHT[2] = '3';
  SET WEIGHT[3] = '1';
  SET WEIGHT[4] = '7';
  SET WEIGHT[5] = '3';
  SET WEIGHT[6] = '9';

  -- Process the SEDOL.
  SET SUM = 0;
  SET I = 1;
  WHILE (I <= 6) DO
   SET CHAR_AT = SUBSTR(TEXT, I, 1);
   IF (ASCII(CHAR_AT) > 65) THEN
    SET SUM = SUM + WEIGHT[I] * (ASCII(CHAR_AT) - 64 + 9);
   ELSE
    SET SUM = SUM + WEIGHT[I] * CHAR_AT;
   END IF;
   SET I = I + 1;
  END WHILE;
  SET CHECK = MOD((10 - MOD(SUM, 10)), 10);
  CALL DBMS_OUTPUT.PUT_LINE(CHECK);
  RETURN TEXT || CHECK;
 END @

```

Output:

```txt

db2 -td@
db2 => SET SERVEROUTPUT ON@
DB20000I  The SET SERVEROUTPUT command completed successfully.
db2 => CREATE OR REPLACE FUNCTION CHECK_SEDOL (
...
db2 (cont.) => END @
DB20000I  The SQL command completed successfully.

db2 -x
db2 => values CHECK_SEDOL('710889')
7108899
9
db2 => values CHECK_SEDOL('B0YBKJ')
B0YBKJ7
7
db2 => values CHECK_SEDOL('406566')
4065663
3
db2 => values CHECK_SEDOL('B0YBLH')
B0YBLH2
2
db2 => values CHECK_SEDOL('228276')
2282765
5
db2 => values CHECK_SEDOL('B0YBKL')
B0YBKL9
9
db2 => values CHECK_SEDOL('557910')
5579107
7
db2 => values CHECK_SEDOL('B0YBKR')
B0YBKR5
5
db2 => values CHECK_SEDOL('585284')
5852842
2
db2 => values CHECK_SEDOL('B0YBKT')
B0YBKT7
7
db2 => values CHECK_SEDOL('B00030')
B000300
0

```



## Standard ML


```sml
fun char2value c =
  if List.exists (fn x => x = c) (explode "AEIOU") then raise Fail "no vowels"
  else if Char.isDigit c then ord c - ord #"0"
  else if Char.isUpper c then ord c - ord #"A" + 10
  else raise Match

val sedolweight = [1,3,1,7,3,9]

fun checksum sedol = let
  val tmp = ListPair.foldlEq (fn (ch, weight, sum) => sum + char2value ch * weight)
              0 (explode sedol, sedolweight)
in
  Int.toString ((10 - (tmp mod 10)) mod 10)
end

app (fn sedol => print (sedol ^ checksum sedol ^ "\n"))
  [ "710889",
    "B0YBKJ",
    "406566",
    "B0YBLH",
    "228276",
    "B0YBKL",
    "557910",
    "B0YBKR",
    "585284",
    "B0YBKT" ];
```



## Tcl


```tcl
namespace eval sedol {
    variable chars {0 1 2 3 4 5 6 7 8 9 "" B C D "" F G H "" J K L M N "" P Q R S T "" V W X Y Z}
    variable weight {1 3 1 7 3 9 1}

    proc checksum {alnum6} {
        variable chars
        variable weight
        set sum 0
        set col 0
        foreach char [split [string toupper [string range $alnum6 0 5]] ""] {
            if {[set idx [lsearch -exact $chars $char]] == -1} {
                error "invalid character: $char"
            }
            incr sum [expr {$idx * [lindex $weight $col]}]
            incr col
        }
        return [expr {(10 - ($sum % 10)) % 10}]
    }
    
    proc valid {alnum7} {
        expr {[checksum [string range $alnum7 0 5]] == [string index $alnum7 6]}
    }
}

proc assert {condition {message "Assertion failed!"}} {
    if { ! [uplevel 1 [list expr $condition]]} {
        return -code error $message
    }
}

set codes {710889 B0YBKJ 406566 B0YBLH 228276 B0YBKL 557910 B0YBKR 585284 B0YBKT}
set answers {7108899 B0YBKJ7 4065663 B0YBLH2 2282765 B0YBKL9 5579107 B0YBKR5 5852842 B0YBKT7}

foreach code $codes answer $answers {
    set sedol "${code}[sedol::checksum $code]"
    assert {$sedol eq $answer} "assertion failed: $sedol ne $answer"
    puts $sedol
}
```


=={{header|Transact-SQL}}==
SQL Server transact-SQL implementation. Compatible with all versions from 6.5 to 2008.
Returns empty string if invalid.


```tsql
CREATE FUNCTION [dbo].[fn_CheckSEDOL]
( @SEDOL varchar(50) )
RETURNS varchar(7)
AS
BEGIN
	declare	@true bit = 1,
		@false bit = 0,
		@isSEDOL bit,		
		@sedol_weights varchar(6) ='131739',
		@sedol_len int = LEN(@SEDOL),
		@sum int = 0
		
					
	if ((@sedol_len = 6)) 
	begin
		select @SEDOL = UPPER(@SEDOL)
		Declare	@vowels varchar(5) = 'AEIOU', 
			@letters varchar(21) = 'BCDFGHJKLMNPQRSTVWXYZ',
			@i int=1,
			@isStillGood bit = @true,					
			@char char = '',
			@weighting int =0
					
		select @isSEDOL = @false
    
		while ((@i < 7) and (@isStillGood = @true))
		begin
			select	@char = SUBSTRING(@SEDOL,@i,1), 
				@weighting = CONVERT (INT,SUBSTRING(@sedol_weights, @i, 1))
			if (CHARINDEX(@char, @vowels) > 0) -- no vowels please
			begin
				select @isStillGood=@false
			end
			else
			begin
				if (ISNUMERIC(@char) = @true) -- is a number
				begin
					select @sum = @sum + (ASCII(@char) - 48) * @weighting
				end
				else if (CHARINDEX(@char, @letters) = 0) -- test for the rest of the alphabet
				begin
					select @isStillGood=@false
				end
				else
				begin
					select @sum = @sum + (ASCII(@char) - 55) * @weighting
				end
			end 
			select @i = @i +1  
		end -- of while loop
		if (@isStillGood = @true) 
		begin
			declare @checksum int = (10 - (@sum%10))%10 
			select @SEDOL = @SEDOL + CONVERT(CHAR,@checksum)
		end
	end
	else
	begin
		select @SEDOL = ''
	end 
	-- Return the result of the function
	RETURN @SEDOL
END
```


Examples:

```txt
print dbo.fn_CheckSEDOL('710889')
print dbo.fn_CheckSEDOL('B0YBKJ')
print dbo.fn_CheckSEDOL('406566')
print dbo.fn_CheckSEDOL('B0YBLH')
print dbo.fn_CheckSEDOL('228276')
print dbo.fn_CheckSEDOL('B0YBKL')
print dbo.fn_CheckSEDOL('557910')
print dbo.fn_CheckSEDOL('B0YBKR')
print dbo.fn_CheckSEDOL('585284')
print dbo.fn_CheckSEDOL('B0YBKT')
print dbo.fn_CheckSEDOL('B00030')

7108899
B0YBKJ7
4065663
B0YBLH2
2282765
B0YBKL9
5579107
B0YBKR5
5852842
B0YBKT7
B000300
```



## TUSCRIPT


```tuscript

$$ MODE TUSCRIPT
check="1'3'1'7'3'9"
values="123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
value=STRINGS (values,":<%:")
BUILD r_TABLE/or illegal=":A:E:I:O:U:"
LOOP input="710889'B0YBKJ'406566'B0YBLH'228276'B0YBKL'557910'B0YBKR'585284'B0YBKT'BOYAKT'B00030",sum=""
  IF (input.ma.illegal) THEN
   PRINT/ERROR input, " illegal"
   CYCLE
  ENDIF
  strings=STRINGS (input,":<%:")
  LOOP d,nr=strings
   c=SELECT (check,#d)
   IF (nr!='digits') nr=FILTER_INDEX (value,":{nr}:",-)
   x=nr*c, sum=APPEND(sum,x)
  ENDLOOP
  endsum=SUM(sum), checksum=10-(endsum%10)
  IF (checksum==10) checksum=0
  PRINT input, " checkdigit: ", checksum
ENDLOOP

```

Output:

```txt

710889 checkdigit: 9
B0YBKJ checkdigit: 7
406566 checkdigit: 3
B0YBLH checkdigit: 2
228276 checkdigit: 5
B0YBKL checkdigit: 9
557910 checkdigit: 7
B0YBKR checkdigit: 5
585284 checkdigit: 2
B0YBKT checkdigit: 7
@@@@@@@@  BOYAKT illegal
B00030 checkdigit: 0 

```



## Ursala

The straightforward approach closely follows the published
specification, using a table-driven finite map (charval) from
characters to numbers, and calculating the inner product as a
cumulative sum of the weight vector zipped with the product function
to the list of character values.


```Ursala
#import std
#import nat

alphabet = digits-- ~=`A-~r letters
weights  = <1,3,1,7,3,9>
charval  = -:@rlXS num alphabet
iprod    = sum:-0+ product*p/weights+ charval*
checksum = difference/10+ remainder\10+ iprod
```


An optimization following the J solution avoids a run-time subtraction
by complementing the coefficients at compile time using these
definitions in place of those above.

```Ursala
weights  = difference/*10 <1,3,1,7,3,9>
checksum = remainder\10+ iprod
```


A further performance improvement subsumes the character value lookup
and multiplcation table within the same finite map in
the version  shown below.


```Ursala
lookup   = -: (^/~& product^|/~& charval)*lsPrK0/weights alphabet
iprod    = sum:-0+ lookup*p/weights
```


To optimize further, we can build a separate smaller multiplication table for
each coefficient, letting the coefficient be hard coded and allowing faster
lookups. The zipwith operation (*p) is also avoided by having each map
index directly into the input list.


```Ursala
lookups  = (-:+ * ^/~&l product^|/charval ~&)* *-* -*weights alphabet
iprod    = sum:-0+ gang +^|(~&,~)*lNrXXK9 ^(~&,&h!)* lookups
```

Here is a test program.

```Ursala
#show+

examples = ^T(~&,~&h+ %nP+ checksum)*t

-[
710889
B0YBKJ
406566
B0YBLH
228276
B0YBKL
557910
B0YBKR
585284
B0YBKT]-
```

output:

```txt
7108899
B0YBKJ7
4065663
B0YBLH2
2282765
B0YBKL9
5579107
B0YBKR5
5852842
B0YBKT7
```



## VBScript

Derived from the BASIC version.

```VBScript

arr = Array("710889",_
            "B0YBKJ",_
	    "406566",_
	    "B0YBLH",_
	    "228276",_
	    "B0YBKL",_
	    "557910",_
            "B0YBKR",_
	    "585284",_
	    "B0YBKT",_
	    "12345",_
	    "A12345",_
	    "B00030")

For j = 0 To UBound(arr)
	WScript.StdOut.Write arr(j) & getSEDOLCheckDigit(arr(j))
	WScript.StdOut.WriteLine
Next 

Function getSEDOLCheckDigit(str)
	If Len(str) <> 6 Then
		getSEDOLCheckDigit = " is invalid. Only 6 character strings are allowed."
		Exit Function
	End If
	Set mult = CreateObject("Scripting.Dictionary")
	With mult
		.Add "1","1" : .Add "2", "3" : .Add "3", "1"
		.Add "4","7" : .Add "5", "3" : .Add "6", "9"
	End With
	total = 0
	For i = 1 To 6
		s  = Mid(str,i,1)
		If s = "A" Or s = "E" Or s = "I" Or s = "O" Or s = "U" Then
			getSEDOLCheckDigit = " is invalid. Vowels are not allowed."
			Exit Function
		End If
		If Asc(s) >= 48 And Asc(s) <=57 Then
			total = total + CInt(s) * CInt(mult.Item(CStr(i)))
		Else
			total = total + (Asc(s) - 55) * CInt(mult.Item(CStr(i)))
		End If
	Next
	getSEDOLCheckDigit = (10 - total Mod 10) Mod 10
End Function
```


{{out}}

```txt
7108899
B0YBKJ7
4065663
B0YBLH2
2282765
B0YBKL9
5579107
B0YBKR5
5852842
B0YBKT7
12345 is invalid. Only 6 character strings are allowed.
A12345 is invalid. Vowels are not allowed.
B000300
```



## Visual FoxPro


```vfp

#DEFINE ALPHABET "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
#DEFINE VOWELS "AEIOU"
#DEFINE VALIDCHARS "0123456789" + ALPHABET
LOCAL cMsg As String, cCode As String 
LOCAL ARRAY codes[12]
codes[1] = "710889"
codes[2] = "B0YBKJ"
codes[3] = "406566"
codes[4] = "B0YBLH"
codes[5] = "228276"
codes[6] = "B0YBKL"
codes[7] = "557910"
codes[8] = "B0YBKR"
codes[9] = "585284"
codes[10] = "B0YBKT"
codes[11] = "B00030"
codes[12] = "B0030A"
DIMENSION w[6]
w[1] = 1
w[2] = 3
w[3] = 1
w[4] = 7
w[5] = 3
w[6] = 9
CLEAR
FOR EACH cCode IN codes
    cMsg = ""
    IF IsValidCode(@cCode, @cMsg)	&& Parameters passed by reference
	cCode = cCode + GetCheckDigit(cCode)
	? cCode
    ELSE
	? cCode, cMsg
    ENDIF			
ENDFOR

FUNCTION GetCheckDigit(tcCode As String) As String
LOCAL i As Integer, c As String, s As Integer, k As Integer
s = 0
FOR i = 1 TO 6
    c = SUBSTR(tcCode, i, 1)
    IF ISDIGIT(c)
	k = VAL(c)
    ELSE
	k = 9 + AT(c, ALPHABET)
    ENDIF
    s = s + k*w[i]
ENDFOR
RETURN TRANSFORM((10 - s%10)%10)
ENDFUNC 

FUNCTION IsValidCode(tcCode As String, tcMsg As String) As Boolean
LOCAL n As Integer, c As String, i As Integer
*!* Get rid of any spaces and convert to upper case
tcCode = UPPER(STRTRAN(tcCode, " "))
n = LEN(tcCode)
IF LEN(tcCode) # 6
    tcMsg = "Code must be 6 characters."
ELSE
    FOR i = 1 TO n
	c = SUBSTR(tcCode, i, 1)
	IF NOT c $ VALIDCHAR
	    tcMsg = c + " is not a valid character."
	    EXIT 
	ELSE
	    IF c $ VOWELS
		tcMsg = "Vowels are not allowed."
		EXIT 
	    ENDIF
	ENDIF 			
    ENDFOR
ENDIF
RETURN EMPTY(tcMsg)
ENDFUNC	

```

{{out}}

```txt

Output as in task description.
The last code is printed as BOO30A Vowels are not allowed.

```



## Yabasic


```Yabasic
data "710889", "B0YBKJ", "406566", "B0YBLH", "228276", "B0YBKL", "557910", "B0YBKR", "585284", "B0YBKT", "B00030", "AB", "B00A03", ""

do
    read d$
    if d$ = "" break
    print sedol$(d$)
loop

sub sedol$(d$)
    LOCAL a, i, s, weights$(1)
    
    a = len(d$)
    if a < 6 or a > 6 return d$ + ": Error in length"
    
    for i = 1 to 6
        if not instr("BCDFGHJKLMNPQRSTVWXYZ0123456789", mid$(d$, i, 1)) return d$ + ": Error in symbol " + mid$(d$, i, 1)
    next
    
    a = token("1 3 1 7 3 9", weights$())
    
    FOR i = 1 TO 6
      a = ASC(MID$(d$, i, 1)) - 48
      s = s + (a + 3 * (a > 9)) * val(weights$(i))
    NEXT
    return d$ + CHR$(48 + mod(10 - mod(s, 10), 10))
end sub
```



## zkl


```zkl
fcn checksum(text){
   ( text.len()!=6 or (text..matches("*[AEIOUaeioua-z]*")) ) and 
      throw(Exception.ValueError("Invalid SEDOL text: "+text));

   text + (10 - text.pump(List,'wrap(c){
                    if("0"<=c<="9") c.toAsc()-0x30;
		    else c.toAsc()-55;
	          }).zipWith('*,T(1,3,1,7,3,9)).sum() % 10) % 10;
}
```

It sure does look like that trailing %10 is extraneous. It also seems like lower case is implicitly invalid.

```zkl
T("710889","B0YBKJ","406566","B0YBLH","228276",
  "B0YBKL","557910","B0YBKR","585284","B0YBKT","B00030")
.apply(checksum).println();
```

{{out}}

```txt

L("7108899","B0YBKJ7","4065663","B0YBLH2","2282765","B0YBKL9",
  "5579107","B0YBKR5","5852842","B0YBKT7","B000300")

```



{{omit from|GUISS}}
