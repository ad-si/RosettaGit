+++
title = "Validate International Securities Identification Number"
description = ""
date = 2019-05-07T00:26:44Z
aliases = []
[extra]
id = 18778
[taxonomies]
categories = []
tags = []
+++

{{task}}

An [[wp:International_Securities_Identification_Number|International Securities Identification Number]] (ISIN) is a unique international identifier for a financial security such as a stock or bond. 

{{task heading}}

Write a function or program that takes a string as input, and checks whether it is a valid ISIN.

It is only valid if it has the correct format, ''and'' the embedded checksum is correct.

Demonstrate that your code passes the test-cases listed below.

{{task heading|Details}}

The format of an ISIN is as follows:

<!-- BEGIN DIAGRAM -->
<div style="margin:0.5em; white-space:nowrap; line-height:20px">
<div><span style="font-size:20px; font-family:'Lucida Console',Monaco,monospace"><span style="color:green; margin:0 0 0 10px">┌───────────── </span></span><span style="color:green">a 2-character ISO country code (A-Z)</span></div>
<div><span style="font-size:20px; font-family:'Lucida Console',Monaco,monospace"><span style="color:green; margin:0 -10px 0 10px">│</span> <span style="color:blue; margin:0 0 0 10px">┌─────────── </span></span><span style="color:blue">a 9-character security code (A-Z, 0-9)</span></div>
<div><span style="font-size:20px; font-family:'Lucida Console',Monaco,monospace"><span style="color:green; margin:0 -10px 0 10px">│</span> <span style="color:blue; margin:0 -5px 0 10px">│</span>        <span style="color:red; margin:0 0 0 5px">┌── </span></span><span style="color:red">a checksum digit (0-9)</span></div>
<div style="font-size:20px; font-family:'Lucida Console',Monaco,monospace"><span style="background: #d9ebd9; color:green; border:solid 1px green; margin:0 1px">AU</span><span style="background:#e0e0ff; color:blue; border:solid 1px blue; margin:0 1px">0000XVGZA</span><span style="background:#feefef; color:red; border:solid 1px red; margin:0 1px">3</span></div>
</div>
<!-- END DIAGRAM -->
For this task, you may assume that any 2-character alphabetic sequence is a valid country code.

The checksum can be validated as follows:
# '''Replace letters with digits''', by converting each character from base 36 to base 10, e.g. <code>AU0000XVGZA3</code> &rarr;<code>1030000033311635103</code>.
# '''Perform the Luhn test on this base-10 number.'''
There is a separate task for this test: ''[[Luhn test of credit card numbers]]''.
You don't have to replicate the implementation of this test here &ndash; you can just call the existing function from that task. (Add a comment stating if you did this.)

{{task heading|Test-cases}}

{| class="wikitable"
! ISIN
! Validity
! Comment
|-
| <tt>US0378331005</tt>  || valid || 
|-
| <tt>US0373831005</tt>  || not valid || The transposition typo is caught by the checksum constraint.
|-
| <tt>U50378331005</tt>  || not valid || The substitution typo is caught by the format constraint.
|-
| <tt>US03378331005</tt> || not valid || The duplication typo is caught by the format constraint.
|-
| <tt>AU0000XVGZA3</tt>  || valid || 
|-
| <tt>AU0000VXGZA3</tt>  || valid || Unfortunately, not ''all'' transposition typos are caught by the checksum constraint.
|-
| <tt>FR0000988040</tt>  || valid || 
|}

(The comments are just informational. Your function should simply return a Boolean result. See [[#Perl_6]] for a reference solution.)

{{task heading|See also}}

Useful resources:
* [https://www.isincodes.net/validate-isin/ Interactive online ISIN validator]
* Wikipedia article: [[wp:International_Securities_Identification_Number|International Securities Identification Number]]



Related tasks:
* [[Luhn test of credit card numbers]]


<hr>


## 360 Assembly


```360asm
*        Validate ISIN             08/03/2019
VALISIN  CSECT
         USING  VALISIN,R13        base register
         B      72(R15)            skip savearea
         DC     17F'0'             savearea
         SAVE   (14,12)            save previous context
         ST     R13,4(R15)         link backward
         ST     R15,8(R13)         link forward
         LR     R13,R15            set addressability
         LA     R7,1               j=1
       DO WHILE=(C,R7,LE,=A(NN))   do j=1 to hbound(tt)
         LR     R1,R7              j
         SLA    R1,4               ~
         LA     R4,TT-16(R1)       @tt(j)
         MVC    CC,0(R4)           cc=tt(j)
         MVC    C,=CL28' '         c=' '
         MVC    R,=CL28' '         r=' '
         MVI    ERR,X'00'          err=false
         MVC    LCC,=F'0'          lcc=0
         LA     R1,L'CC            i=length(cc)
LENTRIA  LA     R5,CC-1            @cc
         AR     R5,R1              +i
         CLI    0(R5),C' '         if cc[i]=' '
         BE     LENTRIB            then iterate loop
         ST     R1,LCC             lcc=lentrim(cc)
         B      LENTRIC            leave loop
LENTRIB  BCT    R1,LENTRIA         i--; if i<>0 then loop
LENTRIC  L      R4,LCC             lcc
       IF    CH,R4,EQ,=H'12' THEN  if lcc=12 then
         MVC    LC,=F'0'           lc=0
         MVC    WW,=CL28' '        ww='' 
         LA     R10,WW             @ww
         LA     R6,1               i=1 
       DO WHILE=(C,R6,LE,LCC)      do i=1 to lcc
         LA     R4,CC-1            @cc
         AR     R4,R6              +i 
         MVC    CI(1),0(R4)        ci=substr(cc,i,1)
         LA     R2,BASE36          @base36
         LA     R3,L'BASE36        length(base36)
         BAL    R14,INDEX          r0=index(base36,ci)
       IF   LTR,R0,NZ,R0 THEN      if p<>0 then
         LR     R1,R0                ip
         BCTR   R1,0                 -1
         XDECO  R1,XDEC              str(ip-1)
         MVC    0(2,R10),XDEC+10     ww=ww||str(p-1)
       ELSE     ,                  else
         MVI    ERR,X'FF'            err=true
       ENDIF    ,                  endif
         LA     R10,2(R10)           @ww+=2
         LA     R6,1(R6)             i++
       ENDDO    ,                  enddo i
         MVC    C,=CL28' '         c=''
         LA     R8,WW              @ww
         LA     R9,C               @c
         LA     R10,0              length(c)
         LA     R6,1               i=1 
       DO WHILE=(C,R6,LE,=A(L'WW)) do i=1 to length(ww)
       IF   CLI,0(R8),NE,C' ' THEN   if ww[i]<>' ' then
         MVC    0(1,R9),0(R8)          c=ww[i]
         LA     R9,1(R9)               @c++
         LA     R10,1(R10)             length(c)++
       ENDIF    ,                    endif
         LA     R8,1(R8)             @ww++
         LA     R6,1(R6)             i++
       ENDDO    ,                  enddo i
         ST     R10,LC             lc=length(c)
         LA     R6,1               i=1
       DO WHILE=(CH,R6,LE,=H'2')   do i=1 to 2
         LA     R4,CC-1              @cc
         AR     R4,R6                +i
         MVC    CI(1),0(R4)          ci=substr(cc,i,1)
         LA     R2,ALPHA             @alpha
         LA     R3,L'ALPHA           length(alpha)
         BAL    R14,INDEX            r0=index(alpha,ci)
       IF   LTR,R0,Z,R0 THEN         if index(alpha,ci)=0 then 
         MVI    ERR,X'FF'              err=true
       ENDIF    ,                    endif
         LA     R6,1(R6)             i++
       ENDDO    ,                  enddo i
         SR     R8,R8              i1=0
         SR     R9,R9              i2=0
       IF   CLI,ERR,EQ,X'00' THEN  if not err then
         SR     R0,R0              0
         L      R6,LC              i=lc
         MVC    R,=CL28' '         r=''
         LA     R10,C              @c
         LA     R11,R-1            @r
         A      R11,LC             @r=@r+length(strip((c))
       DO WHILE=(CH,R6,GE,=H'1')    do i=lc to 1 step -1
         MVC    0(1,R11),0(R10)      r[k]=c[i]
         BCTR   R11,0                @r--
         LA     R10,1(R10)           @c++
         BCTR   R6,0                 i--
       ENDDO    ,                  enddo i
         LA     R6,1               i=1
       DO WHILE=(C,R6,LE,LC)       do i=1 to lc step 2
         LA     R4,R-1               @r
         AR     R4,R6                +i
         MVC    CI(1),0(R4)          ci=substr(r,i,1)
         MVC    XDEC,=CL12' '        ~
         MVC    XDEC(L'CI),CI        ci
         XDECI  R2,XDEC              int(ci)
         AR     R8,R2                i1=i1+int(ci)
         LA     R6,2(R6)             i+=2
       ENDDO    ,                  enddo i
         LA     R6,2               i=2
       DO WHILE=(C,R6,LE,LC)       do i=2 to lc step 2
         LA     R4,R-1               @r
         AR     R4,R6                +i
         MVC    CI(1),0(R4)          ci=substr(r,i,1)
         MVC    XDEC,=CL12' '        ~
         MVC    XDEC(L'CI),CI        ci
         XDECI  R10,XDEC             int(ci)
         SLA    R10,1                ii=int(ci)*2
       IF CH,R10,GE,=H'10' THEN       if ii>=10 then
         SH     R10,=H'9'              ii=ii-9
       ENDIF     ,                   endif
         AR     R9,R10               i2=i2+ii
         LA     R6,2(R6)             i++
       ENDDO    ,                  enddo i
         LR     R2,R8              i1
         AR     R2,R9              +i2
         XDECO  R2,XDEC               s=str(i1+i2)
       IF CLI,XDEC+11,EQ,C'0' THEN if substr(s,length(s),1)='0' then
         MVC    MSG,=CL6'OK'         msg='ok'
       ELSE     ,                  else
         MVC    MSG,=CL6'?err1'      msg='?1'
       ENDIF    ,                  endif 
       ELSE     ,                  else
         MVC    MSG,=CL6'?err2'      msg='?2'
       ENDIF    ,                  endif 
       ELSE     ,                  else
         MVC    MSG,=CL6'?err3'      msg='?3'
       ENDIF    ,                  endif
         XDECO  R7,XDEC            edit j
         MVC    PG(2),XDEC+10      j
         MVC    PG+3(16),CC        cc
         MVC    PG+20(6),MSG       msg
         XPRNT  PG,L'PG            print buffer
         LA     R7,1(R7)           j++
       ENDDO    ,                  enddo j
         L      R13,4(0,R13)       restore previous savearea pointer
         RETURN (14,12),RC=0       restore registers from calling sav
MVCX     MVC    0(0,R4),0(R5)      pattern svc
INDEX    SR     R0,R0              index(r2,ci) r3=len
         LA     R1,1               k=1
SINDEXA  CR     R1,R3              do k=1 to length(ca)
         BH     SINDEXC              ~
         CLC    0(1,R2),CI           if ca[k]=ci
         BNE    SINDEXB              then iterate loop
         LR     R0,R1                ii=k
         B      SINDEXC              exit loop
SINDEXB  LA     R2,1(R2)             @ca++
         LA     R1,1(R1)             k++
         B      SINDEXA            enddo
SINDEXC  BR     R14                end index
NN       EQU    (BASE36-TT)/16     number of items
TT       DC     CL16'US0378331005',CL16'US0373831005'
         DC     CL16'U50378331005',CL16'US03378331005'
         DC     CL16'AU0000XVGZA3',CL16'AU0000VXGZA3'
         DC     CL16'FR0000988040'
BASE36   DC     CL36'0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ'
ALPHA    DC     CL26'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
ERR      DS     X                  error
LCC      DS     F                  length of cc
LC       DS     F                  length of c
CI       DS     CL1
CC       DS     CL16               current element of tt
C        DS     CL28
R        DS     CL28
WW       DS     CL28
MSG      DS     CL6                message
PG       DC     CL80' '            buffer
XDEC     DS     CL12               temp for xdeco and xdeci
         REGEQU
         END    VALISIN
```

{{out}}

```txt

 1 US0378331005     OK
 2 US0373831005     ?err1
 3 U50378331005     ?err2
 4 US03378331005    ?err3
 5 AU0000XVGZA3     OK
 6 AU0000VXGZA3     OK
 7 FR0000988040     OK

```



## Ada

Calling the existing Luhn algorithm implementation from the ''[[Luhn test of credit card numbers]]'' task.


```Ada
procedure ISIN is
   -- Luhn_Test copied from other Task
   function Luhn_Test (Number: String) return Boolean is
      Sum  : Natural := 0;
      Odd  : Boolean := True;
      Digit: Natural range 0 .. 9;
   begin
      for p in reverse Number'Range loop
         Digit := Integer'Value (Number (p..p));
         if Odd then
            Sum := Sum + Digit;
         else
            Sum := Sum + (Digit*2 mod 10) + (Digit / 5);
         end if;
         Odd := not Odd;
      end loop;
      return (Sum mod 10) = 0;
   end Luhn_Test;
   
   subtype Decimal   is Character range '0' .. '9';
   subtype Letter    is Character range 'A' .. 'Z';
   subtype ISIN_Type is String(1..12);
   
   -- converts a string of decimals and letters into a string of decimals
   function To_Digits(S: String) return String is
      -- Character'Pos('A')-Offset=10, Character'Pos('B')-Offset=11, ...
      Offset: constant Integer := Character'Pos('A')-10;
   
      Invalid_Character: exception;
   begin
      if S = "" then
         return "";
      elsif S(S'First) = ' ' then -- skip blanks
         return To_Digits(S(S'First+1 .. S'Last)); 
      elsif S(S'First) in Decimal then 
         return S(S'First) & To_Digits(S(S'First+1 .. S'Last));
      elsif S(S'First) in Letter then
         return To_Digits(Integer'Image(Character'Pos(S(S'First))-Offset))
           & To_Digits(S(S'First+1 .. S'Last));
      else 
         raise Invalid_Character;
      end if;
   end To_Digits;
   
   function Is_Valid_ISIN(S: ISIN_Type) return Boolean is
      Number : String := To_Digits(S);
   begin
      return S(S'First)   in Letter  and
             S(S'First+1) in Letter  and
             S(S'Last)    in Decimal and
             Luhn_Test(Number);
   end Is_Valid_ISIN;

   Test_Cases : constant Array(1..6) of ISIN_Type :=
      ("US0378331005",
       "US0373831005",
       "U50378331005",
       -- excluded by type with fixed length
       -- "US03378331005",
       "AU0000XVGZA3",
       "AU0000VXGZA3",
       "FR0000988040");
begin
   for I in Test_Cases'Range loop
      Ada.Text_IO.Put_Line(Test_Cases(I) & ":" &
        Boolean'Image(Is_Valid_ISIN(Test_Cases(I))));
   end loop;
   -- using wrong length will result in an exception:
   Ada.Text_IO.Put("US03378331005:");
   Ada.Text_IO.Put_Line(Boolean'Image(Is_Valid_Isin("US03378331005")));
exception
   when others =>
      Ada.Text_IO.Put_Line("Exception occured");
end ISIN;
```


Output:

```txt
US0378331005:TRUE
US0373831005:FALSE
U50378331005:FALSE
AU0000XVGZA3:TRUE
AU0000VXGZA3:TRUE
FR0000988040:TRUE
US03378331005:Exception occured
```



## ALGOL W

Uses the LuhnTest procedure from the [[Luhn test of credit card numbers]] task.

```algolw
begin
    % external procedure that returns true if ccNumber passes the Luhn test, false otherwise %
    logical procedure LuhnTest ( string(32) value ccNumber
                               ; integer    value ccLength
                               ) ; algol "LUHN" ;

    % returns true if isin is a valid ISIN, false otherwise    %
    logical procedure isIsin ( string(32) value isin ) ;
        if isin( 12 // 20 ) not = "" then false % code is too long %
        else begin
            % the first two characters must be upper-case letters %

            % returns the digit corresponding to a character of an ISIN %
            integer procedure isinDigit ( string(1) value iChar ) ;
                if      iChar >= "0" and iChar <= "9" then ( decode( iChar ) - decode( "0" ) )
                else if iChar >= "A" and iChar <= "Z" then ( decode( iChar ) - decode( "A" ) ) + 10
                else begin % invalid digit %
                    isValid := false;
                    -1
                end isinDigit ;

            integer d1, d2;
            logical isValid;
            isValid := true;
            d1      := isinDigit( isin( 0 // 1 ) );
            d2      := isinDigit( isin( 1 // 1 ) );
            if d1 < 10 or d1 > 35 or d2 < 10 or d2 > 35 then false % invalid first two characters %
            else begin
                % ok so far - conveet from base 36 to base 10 %
                string(24) base10Isin;
                integer    b10Pos;
                base10Isin := "";
                b10Pos     := 0;
                for cPos := 0 until 10 do begin
                    integer digit;
                    digit := isinDigit( isin( cPos // 1 ) );
                    if isValid then begin
                        % valid digit %
                        if digit > 9 then begin
                            base10Isin( b10Pos // 1 ) := code( ( digit div 10 ) + decode( "0" ) );
                            b10Pos                    := b10Pos + 1;
                        end if_digit_gt_9 ;
                        base10Isin( b10Pos // 1 )     := code( ( digit rem 10 ) + decode( "0" ) );
                        b10Pos                        := b10Pos + 1
                    end if_isValid
                end for_cPos ;
                % add the check digit as is %
                base10Isin( b10Pos // 1 ) := isin( 11 // 1 );
                isValid and LuhnTest( base10Isin, b10Pos + 1 )
            end
        end isIsin ;

    % task test cases %

    procedure testIsIsin ( string(32) value isin
                         ; logical    value expected
                         ) ;
    begin
        logical isValid;
        isValid := isIsin( isin );
        write( s_w := 0
             , isin
             , if isValid then " is valid" else " is invalid"
             , if isValid = expected   then "" else " NOT as expected ??"
             )
    end testIsin ;

    testIsIsin( "US0378331005",  true  );
    testIsIsin( "US0373831005",	 false );
    testIsIsin( "U50378331005",  false );
    testIsIsin( "US03378331005", false );
    testIsIsin( "AU0000XVGZA3",	 true  );
    testIsIsin( "AU0000VXGZA3",	 true  );
    testIsIsin( "FR0000988040",	 true  );
end.
```

{{out}}

```txt

US0378331005                     is valid
US0373831005                     is invalid
U50378331005                     is invalid
US03378331005                    is invalid
AU0000XVGZA3                     is valid
AU0000VXGZA3                     is valid
FR0000988040                     is valid

```



## C


```c>#include <stdio.h


int check_isin(char *a) {
    int i, j, k, v, s[24];
    
    j = 0;
    for(i = 0; i < 12; i++) {
        k = a[i];
        if(k >= '0' && k <= '9') {
            if(i < 2) return 0;
            s[j++] = k - '0';
        } else if(k >= 'A' && k <= 'Z') {
            if(i == 11) return 0;
            k -= 'A' - 10;
            s[j++] = k / 10;
            s[j++] = k % 10;
        } else {
            return 0;
        }
    }
    
    if(a[i]) return 0;
    
    v = 0;
    for(i = j - 2; i >= 0; i -= 2) {
        k = 2 * s[i];
        v += k > 9 ? k - 9 : k;
    }
    
    for(i = j - 1; i >= 0; i -= 2) {
        v += s[i];
    }
    
    return v % 10 == 0;
}

int main() {
    char *test[7] = {"US0378331005", "US0373831005", "U50378331005",
                     "US03378331005", "AU0000XVGZA3", "AU0000VXGZA3",
                     "FR0000988040"};
    int i;
    for(i = 0; i < 7; i++) printf("%c%c", check_isin(test[i]) ? 'T' : 'F', i == 6 ? '\n' : ' ');
    return 0;
}

/* will print: T F F F T T T */
```



## C++


```cpp


#include <string>
#include <regex>
#include <algorithm>
#include <numeric>
#include <sstream>

bool CheckFormat(const std::string& isin)
{
	std::regex isinRegEpx(R"([A-Z]{2}[A-Z0-9]{9}[0-9])");
	std::smatch match;
	return std::regex_match(isin, match, isinRegEpx);
}

std::string CodeISIN(const std::string& isin)
{
	std::string coded;
	int offset = 'A' - 10;
	for (auto ch : isin)
	{
		if (ch >= 'A' && ch <= 'Z')
		{
			std::stringstream ss;
			ss << static_cast<int>(ch) - offset;
			coded += ss.str();
		}
		else
		{
			coded.push_back(ch);
		}
	}

	return std::move(coded);
}

bool CkeckISIN(const std::string& isin)
{
	if (!CheckFormat(isin))
		return false;

	std::string coded = CodeISIN(isin);
// from http://rosettacode.org/wiki/Luhn_test_of_credit_card_numbers#C.2B.2B11
	return luhn(coded);
}


#include <iomanip>
#include <iostream>

int main()
{
	std::string isins[] = { "US0378331005", "US0373831005", "U50378331005",
							"US03378331005", "AU0000XVGZA3", "AU0000VXGZA3",
							"FR0000988040" };
	for (const auto& isin : isins)
	{
		std::cout << isin << std::boolalpha << " - " << CkeckISIN(isin) <<std::endl;
	}
	return 0;
}

```

=={{header|C#|C sharp}}==
{{trans|Java}}

```csharp
using System;
using System.Text;
using System.Text.RegularExpressions;

namespace Validate_ISIN {
    class Program {
        static int DigitValue(char c, int b) {
            if (c >= '0' && c <= '9') {
                return c - '0';
            }
            return c - 'A' + 10;
        }

        static int Digit(char c, int b) {
            int result = DigitValue(c, b);
            if (result >= b) {
                Console.Error.WriteLine("Invalid Number");
                return -1;
            }
            return result;
        }

        static bool ISINtest(string isin) {
            isin = isin.Trim().ToUpper();
            Regex r = new Regex("^[A-Z]{2}[A-Z0-9]{9}\\d$");
            if (!r.IsMatch(isin)) {
                return false;
            }

            StringBuilder sb = new StringBuilder();
            foreach (char c in isin.Substring(0, 12)) {
                sb.Append(Digit(c, 36));
            }

            return LuhnTest(sb.ToString());
        }

        static string ReverseString(string input) {
            char[] intermediate = input.ToCharArray();
            Array.Reverse(intermediate);
            return new string(intermediate);
        }

        static bool LuhnTest(string number) {
            int s1 = 0;
            int s2 = 0;
            string reverse = ReverseString(number);
            for (int i = 0; i < reverse.Length; i++) {
                int digit = Digit(reverse[i], 10);
                //This is for odd digits, they are 1-indexed in the algorithm.
                if (i % 2 == 0) {
                    s1 += digit;
                }
                else { // Add 2 * digit for 0-4, add 2 * digit - 9 for 5-9.
                    s2 += 2 * digit;
                    if (digit >= 5) {
                        s2 -= 9;
                    }
                }
            }

            return (s1 + s2) % 10 == 0;
        }

        static void Main(string[] args) {
            string[] isins = {
                "US0378331005",
                "US0373831005",
                "U50378331005",
                "US03378331005",
                "AU0000XVGZA3",
                "AU0000VXGZA3",
                "FR0000988040"
            };
            foreach (string isin in isins) {
                Console.WriteLine("{0} is {1}", isin, ISINtest(isin) ? "valid" : "not valid");
            }
        }
    }
}
```

{{out}}

```txt
US0378331005 is valid
US0373831005 is not valid
U50378331005 is not valid
US03378331005 is not valid
AU0000XVGZA3 is valid
AU0000VXGZA3 is valid
FR0000988040 is valid
```


=={{header|Caché ObjectScript}}==


```cos
Class Utils.Check [ Abstract ]
{

ClassMethod ISIN(x As %String) As %Boolean
{
	// https://en.wikipedia.org/wiki/International_Securities_Identification_Number
	IF x'?2U9UN1N QUIT 0
	SET cd=$EXTRACT(x,*), x=$EXTRACT(x,1,*-1)
	FOR i=1:1 {
		SET n=$EXTRACT(x,i) IF n="" QUIT
		IF n'=+n SET $EXTRACT(x,i)=$CASE(n,"*":36,"@":37,"#":38,:$ASCII(n)-55)
	}
	// call into luhn check, appending check digit
	QUIT ..Luhn(x_cd)
}

ClassMethod Luhn(x As %String) As %Boolean
{
	// https://www.simple-talk.com/sql/t-sql-programming/calculating-and-verifying-check-digits-in-t-sql/
	SET x=$TRANSLATE(x," "), cd=$EXTRACT(x,*)
	SET x=$REVERSE($EXTRACT(x,1,*-1)), t=0
	FOR i=1:1:$LENGTH(x) {
		SET n=$EXTRACT(x,i)
		IF i#2 SET n=n*2 IF $LENGTH(n)>1 SET n=$EXTRACT(n,1)+$EXTRACT(n,2)
		SET t=t+n
	}
	QUIT cd=((t*9)#10)
}

}
```

{{out|Examples}}

```txt
USER>For  { Read isin Quit:isin=""  Write ": "_##class(Utils.Check).ISIN(isin), ! }
US0378331005: 1
US0373831005: 0
U50378331005: 0
US03378331005: 0
AU0000XVGZA3: 1
AU0000VXGZA3: 1
FR0000988040: 1

USER>
```



## Clojure


```clojure
(defn luhn? [cc]
  (let [sum (->> cc
                 (map #(Character/digit ^char % 10))
                 reverse
                 (map * (cycle [1 2]))
                 (map #(+ (quot % 10) (mod % 10)))
                 (reduce +))]
    (zero? (mod sum 10))))

(defn is-valid-isin? [isin]
  (and (re-matches #"^[A-Z]{2}[A-Z0-9]{9}[0-9]$" isin)
       (->> isin
            (map #(Character/digit ^char % 36))
            (apply str)
            luhn?)))

(use 'clojure.pprint)
(doseq [isin ["US0378331005" "US0373831005" "U50378331005" "US03378331005"
              "AU0000XVGZA3" "AU0000VXGZA3" "FR0000988040"]]
  (cl-format *out* "~A: ~:[invalid~;valid~]~%" isin (is-valid-isin? isin)))

```

<tt>luhn?</tt> is based on ''[[Luhn test of credit card numbers#Clojure]]''.
{{out}}

```txt
US0378331005: valid
US0373831005: invalid
U50378331005: invalid
US03378331005: invalid
AU0000XVGZA3: valid
AU0000VXGZA3: valid
FR0000988040: valid
```



## COBOL

{{works with|GnuCOBOL}}

```cobol>        >
SOURCE FORMAT FREE
*>   this is gnucobol 2.0
identification division.
program-id. callISINtest.
data division.
working-storage section.
01  ISINtest-result binary-int.
procedure division.
start-callISINtest.
    display 'should be valid ' with no advancing
    call 'ISINtest' using 'US0378331005' ISINtest-result
    perform display-ISINtest-result
    display 'should not be valid ' with no advancing
    call 'ISINtest' using 'US0373831005' ISINtest-result
    perform display-ISINtest-result
    display 'should not be valid ' with no advancing
    call 'ISINtest' using 'U50378331005' ISINtest-result
    perform display-ISINtest-result
    display 'should not be valid ' with no advancing
    call 'ISINtest' using 'US03378331005' ISINtest-result
    perform display-ISINtest-result
    display 'should be valid ' with no advancing
    call 'ISINtest' using 'AU0000XVGZA3' ISINtest-result
    perform display-ISINtest-result
    display 'should be valid ' with no advancing
    call 'ISINtest' using 'AU0000VXGZA3' ISINtest-result
    perform display-ISINtest-result
    display 'should be valid ' with no advancing
    call 'ISINtest' using 'FR0000988040' ISINtest-result
    perform display-ISINtest-result
    stop run
    .
display-ISINtest-result.
    evaluate ISINtest-result
    when 0 
        display ' is valid'
    when -1
        display ' invalid length '
    when -2
        display ' invalid countrycode '
    when -3
        display ' invalid base36 digit '
    when -4
        display ' luhn test failed'
    when other
        display ' invalid return code ' ISINtest-result
    end-evaluate
    .
end program callISINtest.

identification division.
program-id. ISINtest.
data division.
working-storage section.
01  country-code-values value
    'ADAEAFAGAIALAMAOAQARASATAUAWAXAZBABBBDBEBFBGBHBIBJBLBMBNBOBQBRBS'
&   'BTBVBWBYBZCACCCDCFCGCHCICKCLCMCNCOCRCUCVCWCXCYCZDEDJDKDMDODZECEE'
&   'EGEHERESETFIFJFKFMFOFRGAGBGDGEGFGGGHGIGLGMGNGPGQGRGSGTGUGWGYHKHM'
&   'HNHRHTHUIDIEILIMINIOIQIRISITJEJMJOJPKEKGKHKIKMKNKPKRKWKYKZLALBLC'
&   'LILKLRLSLTLULVLYMAMCMDMEMFMGMHMKMLMMMNMOMPMQMRMSMTMUMVMWMXMYMZNA'
&   'NCNENFNGNINLNONPNRNUNZOMPAPEPFPGPHPKPLPMPNPRPSPTPWPYQARERORSRURW'
&   'SASBSCSDSESGSHSISJSKSLSMSNSOSRSSSTSVSXSYSZTCTDTFTGTHTJTKTLTMTNTO'
&   'TRTTTVTWTZUAUGUMUSUYUZVAVCVEVGVIVNVUWFWSYEYTZAZMZW'.
    03  country-codes occurs 249
        ascending key country-code
        indexed by cc-idx.
        05  country-code pic xx.

01  b pic 99.
01  base36-digits pic x(36) value
    '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ'.

01  i pic 99.
01  p pic 99.
01  luhn-number pic x(20).
01  luhntest-result binary-int.

linkage section.
01  test-number any length.
01  ISINtest-result binary-int.

procedure division using test-number ISINtest-result.
start-ISINtest.
    display space test-number with no advancing

    *> format test
    if function length(test-number) <> 12
        move -1 to ISINtest-result
        goback
    end-if

    *> countrycode test
    search all country-codes
    at end
        move -2 to ISINtest-result
        goback
    when test-number(1:2) = country-code(cc-idx)
        continue 
    end-search

    *> convert each character from base 36 to base 10
    *> and add to the luhn-number
    move 0 to p
    perform varying i from 1 by 1 until i > 12
        if test-number(i:1) >= '0' and <= '9'
            move test-number(i:1) to luhn-number(p + 1:1)
            add 1 to p
        else
            perform varying b from 9 by 1 until b > 35
            or base36-digits(b + 1:1) = test-number(i:1)
                continue
            end-perform
            if b > 35
                 move -3 to ISINtest-result
                 goback
            end-if
            move b to luhn-number(p + 1:2)
            add 2 to p
        end-if
    end-perform

    call 'luhntest' using luhn-number(1:p) luhntest-result
    if luhntest-result <> 0
        move -4 to ISINtest-result
        goback
    end-if

    move 0 to ISINtest-result
    goback
    .
end program ISINtest.

identification division.
program-id. luhntest.
data division.
working-storage section.
01  i pic S99.
01  check-sum pic 999.
linkage section.
01  test-number any length.
01  luhntest-result binary-int.
procedure division using test-number luhntest-result.
start-luhntest.
    display space test-number with no advancing
    move 0 to check-sum

    *> right to left sum the odd numbered digits
    compute i = function length(test-number)
    perform varying i from i by -2 until i < 1
        add function numval(test-number(i:1)) to check-sum
    end-perform
    display space check-sum with no advancing

    *> right to left double sum the even numbered digits
    compute i = function length(test-number) - 1
    perform varying i from i by -2 until i < 1
        add function numval(test-number(i:1)) to check-sum
        add function numval(test-number(i:1)) to check-sum
        *> convert a two-digit double sum number to a single digit
        if test-number(i:1) >= '5'
            subtract 9 from check-sum
        end-if
    end-perform
    display space check-sum with no advancing

    if function mod(check-sum,10) = 0
        move 0 to luhntest-result *> success
    else
        move -1 to luhntest-result *> failure
    end-if
    goback
    .
end program luhntest.
```


{{out}}

```txt
prompt$ cobc -xj ISINTest.cbl
should be valid  US0378331005 30280378331005 027 050 is valid
should not be valid  US0373831005 30280373831005 022 046 luhn test failed
should not be valid  U50378331005 invalid countrycode
should not be valid  US03378331005 invalid length
should be valid  AU0000XVGZA3 1030000033311635103 018 030 is valid
should be valid  AU0000VXGZA3 1030000031331635103 018 030 is valid
should be valid  FR0000988040 15270000988040 020 050 is valid
```



## Common Lisp


```lisp
(defun alphap (char)
  (char<= #\A char #\Z))

(defun alpha-digit-char-p (char)
  (or (alphap char) (digit-char-p char)))

(defun valid-isin-format-p (isin)
  (and (= (length isin) 12)
       (alphap (char isin 0))
       (alphap (char isin 1))
       (loop for i from 2 to 10
             always (alpha-digit-char-p (char isin i)))
       (digit-char-p (char isin 11))))

(defun isin->digits (isin)
  (apply #'concatenate 'string
         (loop for c across isin
               collect (princ-to-string (digit-char-p c 36)))))

(defun luhn-test (string)
  (loop for c across (reverse string)
        for oddp = t then (not oddp)
        if oddp
          sum (digit-char-p c) into result
        else
          sum (let ((n (* 2 (digit-char-p c))))
                (if (> n 9) (- n 9) n))
            into result
        finally (return (zerop (mod result 10)))))

(defun valid-isin-p (isin)
  (and (valid-isin-format-p isin)
       (luhn-test (isin->digits isin))))

(defun test ()
  (dolist (isin '("US0378331005" "US0373831005" "U50378331005" "US03378331005"
                  "AU0000XVGZA3" "AU0000VXGZA3" "FR0000988040"))
    (format t "~A: ~:[invalid~;valid~]~%" isin (valid-isin-p isin))))
```

{{out}}

```txt
US0378331005: valid
US0373831005: invalid
U50378331005: invalid
US03378331005: invalid
AU0000XVGZA3: valid
AU0000VXGZA3: valid
FR0000988040: valid
```



## D

{{trans|Java}}
Code for the luhn test was taken from [[https://rosettacode.org/wiki/Luhn_test_of_credit_card_numbers#D]]

```D
import std.stdio;

void main() {
    auto isins = [
        "US0378331005",
        "US0373831005",
        "U50378331005",
        "US03378331005",
        "AU0000XVGZA3",
        "AU0000VXGZA3",
        "FR0000988040",
    ];
    foreach (isin; isins) {
        writeln(isin, " is ", ISINvalidate(isin) ? "valid" : "not valid");
    }
}

bool ISINvalidate(string isin) {
    import std.array : appender;
    import std.conv : to;
    import std.regex : matchFirst;
    import std.string : strip, toUpper;

    isin = isin.strip.toUpper;

    if (isin.matchFirst(`^[A-Z]{2}[A-Z0-9]{9}\d$`).empty) {
        return false;
    }

    auto sb = appender!string;
    foreach (c; isin[0..12]) {
        sb.put(
            [c].to!int(36)
               .to!string
        );
    }

    import luhn;
    return luhnTest(sb.data);
}
```


{{out}}

```txt
US0378331005 is valid
US0373831005 is not valid
U50378331005 is not valid
US03378331005 is not valid
AU0000XVGZA3 is valid
AU0000VXGZA3 is valid
FR0000988040 is valid
```



## Elixir

used Luhn module from [[Luhn_test_of_credit_card_numbers#Elixir | here]]

```elixir
isin? = fn str ->
          if str =~ ~r/\A[A-Z]{2}[A-Z0-9]{9}\d\z/ do
            String.codepoints(str)
            |> Enum.map_join(&String.to_integer(&1, 36))
            |> Luhn.valid?
          else
            false
          end
        end

IO.puts "    ISIN        Valid?"
~w(US0378331005
   US0373831005
   U50378331005
   US03378331005
   AU0000XVGZA3
   AU0000VXGZA3
   FR0000988040)
|> Enum.each(&IO.puts "#{&1}\t#{isin?.(&1)}")
```


{{out}}

```txt

    ISIN        Valid?
US0378331005    true
US0373831005    false
U50378331005    false
US03378331005   false
AU0000XVGZA3    true
AU0000VXGZA3    true
FR0000988040    true

```



## Factor

We re-use the <code>luhn?</code> word from ''[[Luhn test of credit card numbers#Factor]]''.

```factor
USING: combinators.short-circuit.smart formatting kernel luhn
math math.parser qw sequences strings unicode ;
IN: rosetta-code.isin

CONSTANT: test-cases qw{
    US0378331005 US0373831005 U50378331005 US03378331005
    AU0000XVGZA3 AU0000VXGZA3 FR0000988040
}

: valid-length? ( str -- ? ) length 12 = ;

: valid-country-code? ( str -- ? ) first2 [ Letter? ] both? ;

: valid-security-code? ( str -- ? )
    [ 2 11 ] dip subseq [ alpha? ] all? ;
    
: valid-checksum-digit? ( str -- ? ) last digit? ;
    
: valid-format? ( str -- ? ) {
        [ valid-length?         ]
        [ valid-country-code?   ]
        [ valid-security-code?  ]
        [ valid-checksum-digit? ]
    } && ;
    
: base36>base10 ( str -- n )
    >upper [ dup LETTER? [ 55 - number>string ] [ 1string ] if ]
    { } map-as concat string>number ;
    
: isin? ( str -- ? )
    { [ valid-format? ] [ base36>base10 luhn? ] } && ;
    
: main ( -- )
    test-cases [
        dup isin? "" " not" ? "%s is%s valid\n" printf
    ] each ;
    
MAIN: main
```

{{out}}

```txt

US0378331005 is valid
US0373831005 is not valid
U50378331005 is not valid
US03378331005 is not valid
AU0000XVGZA3 is valid
AU0000VXGZA3 is valid
FR0000988040 is valid

```



## Fortran



```fortran
program isin
    use ctype
    implicit none
    character(20) :: test(7) = ["US0378331005        ", &
                                "US0373831005        ", &
                                "U50378331005        ", &
                                "US03378331005       ", &
                                "AU0000XVGZA3        ", &
                                "AU0000VXGZA3        ", &
                                "FR0000988040        "]
    print *, check_isin(test)
contains
    elemental logical function check_isin(a)
        character(*), intent(in) :: a
        integer :: s(24)
        integer :: i, j, k, n, v

        check_isin = .false.

        n = len_trim(a)
        if (n /= 12) return
        
        ! Convert to an array of digits
        j = 0
        do i = 1, n
            k = iachar(a(i:i))
            if (k >= 48 .and. k <= 57) then
                if (i < 3) return
                k = k - 48
                j = j + 1
                s(j) = k
            else if (k >= 65 .and. k <= 90) then
                if (i == 12) return
                k = k - 65 + 10
                j = j + 1
                s(j) = k / 10
                j = j + 1
                s(j) = mod(k, 10)
            else
                return
            end if
        end do

        ! Compute checksum
        v = 0
        do i = j - 1, 1, -2
            k = 2 * s(i)
            if (k > 9) k = k - 9
            v = v + k
        end do
        do i = j, 1, -2
            v = v + s(i)
        end do
        
        check_isin = 0 == mod(v, 10)
    end function
end program
```



## FreeBASIC


```freebasic
' version 27-10-2016
' compile with: fbc -s console

#Ifndef TRUE        ' define true and false for older freebasic versions
    #Define FALSE 0
    #Define TRUE Not FALSE
#EndIf

Function luhntest(cardnr As String) As Long

    cardnr = Trim(cardnr) ' remove spaces  

    Dim As String reverse_nr = cardnr
    Dim As Long i, j, s1, s2, l = Len(cardnr) -1

    ' reverse string
    For i = 0 To l
        reverse_nr[i] = cardnr[l - i]
    Next
    ' sum odd numbers
    For i = 0 To l Step 2
        s1 = s1 + (reverse_nr[i] - Asc("0"))
    Next
    ' sum even numbers
    For i = 1 To l Step 2
        j = reverse_nr[i] - Asc("0")
        j = j * 2
        If j > 9 Then j = j Mod 10 +1
        s2 = s2 + j
    Next

    If (s1 + s2) Mod 10 = 0 Then
        Return TRUE
    Else
        Return FALSE
    End If

End Function

' ------=< MAIN >=-----

Dim As String test_str
Dim As String test_set(1 To ...) = { "US0378331005", "US0373831005", _
                    "U50378331005", "US03378331005", "AU0000XVGZA3", _
                                     "AU0000VXGZA3", "FR0000988040" }

Dim As Long i, l, n, x

For i = 1 To UBound(test_set)
    test_str = ""
    l = Len(test_set(i))
    If l <> 12 Then
        Print test_set(i), "Invalid, length <> 12 char."
        Continue For
    End If
    If test_set(i)[0] < Asc("A") Or test_set(i)[1] < Asc("A") Then
        Print test_set(i), "Invalid, number needs to start with 2 characters"
        Continue For
    End If
    For n = 0 To l -1
        x = test_set(i)[n] - Asc("0")
        ' if test_set(i)[i] is a letter we to correct for that
        If x > 9 Then x = x -7
        If x < 10 Then
            test_str = test_str + Str(x)
        Else ' two digest number
            test_str = test_str + Str(x \ 10) + Str(x Mod 10)
        End If
    Next
    Print test_set(i), IIf(luhntest(test_str) = TRUE, "Valid","Invalid, checksum error")
Next


' empty keyboard buffer
While InKey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

{{out}}

```txt
US0378331005  Valid
US0373831005  Invalid, checksum error
U50378331005  Invalid, number needs to start with 2 characters
US03378331005 Invalid, length <> 12 char.
AU0000XVGZA3  Valid
AU0000VXGZA3  Valid
FR0000988040  Valid
```



## Go



```go
package main

import "regexp"

var r = regexp.MustCompile(`^[A-Z]{2}[A-Z0-9]{9}\d$`)

var inc = [2][10]int{
	{0, 1, 2, 3, 4, 5, 6, 7, 8, 9},
	{0, 2, 4, 6, 8, 1, 3, 5, 7, 9},
}

func ValidISIN(n string) bool {
	if !r.MatchString(n) {
		return false
	}
	var sum, p int
	for i := 10; i >= 0; i-- {
		p = 1 - p
		if d := n[i]; d < 'A' {
			sum += inc[p][d-'0']
		} else {
			d -= 'A'
			sum += inc[p][d%10]
			p = 1 - p
			sum += inc[p][d/10+1]
		}
	}
	sum += int(n[11] - '0')
	return sum%10 == 0
}
```



```go
package main

import "testing"

func TestValidISIN(t *testing.T) {
	testcases := []struct {
		isin  string
		valid bool
	}{
		{"US0378331005", true},
		{"US0373831005", false},
		{"U50378331005", false},
		{"US03378331005", false},
		{"AU0000XVGZA3", true},
		{"AU0000VXGZA3", true},
		{"FR0000988040", true},
	}

	for _, testcase := range testcases {
		actual := ValidISIN(testcase.isin)
		if actual != testcase.valid {
			t.Errorf("expected %v for %q, got %v",
				testcase.valid, testcase.isin, actual)
		}
	}
}
```



## Groovy


{{update|Groovy|Use the new test-cases, and consider calling the existing Luhn algorithm implementation from the ''[[Luhn test of credit card numbers]]'' task instead of duplicating it.}}


```groovy
CHARS = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ'

int checksum(String prefix) {
    def digits = prefix.toUpperCase().collect { CHARS.indexOf(it).toString() }.sum()
    def groups = digits.collect { CHARS.indexOf(it) }.inject([[], []]) { acc, i -> [acc[1], acc[0] + i] }
    def ds = groups[1].collect { (2 * it).toString() }.sum().collect { CHARS.indexOf(it) } + groups[0]
    (10 - ds.sum() % 10) % 10
}

assert checksum('AU0000VXGZA') == 3
assert checksum('GB000263494') == 6
assert checksum('US037833100') == 5
assert checksum('US037833107') == 0
```



## Haskell


```Haskell
module ISINVerification2 where

import Data.Char (isUpper, isDigit, digitToInt)

verifyISIN :: String -> Bool
verifyISIN isin =
  correctFormat isin && mod (oddsum + multiplied_even_sum) 10 == 0
  where
    reverted = reverse $ convertToNumber isin
    theOdds = fst $ collectOddandEven reverted
    theEvens = snd $ collectOddandEven reverted
    oddsum = sum $ map digitToInt theOdds
    multiplied_even_sum = addUpDigits $ map ((* 2) . digitToInt) theEvens

capitalLetters :: String
capitalLetters = ['A','B' .. 'Z']

numbers :: String
numbers = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']

correctFormat :: String -> Bool
correctFormat isin =
  (length isin == 12) &&
  all (`elem` capitalLetters) (take 2 isin) &&
  all (\c -> elem c capitalLetters || elem c numbers) (drop 2 $ take 11 isin) &&
  elem (last isin) numbers

convertToNumber :: String -> String
convertToNumber = concatMap convert
  where
    convert :: Char -> String
    convert c =
      if isDigit c
        then show $ digitToInt c
        else show (fromEnum c - 55)

collectOddandEven :: String -> (String, String)
collectOddandEven term
  | odd $ length term =
    ( concat
        [ take 1 $ drop n term
        | n <- [0,2 .. length term - 1] ]
    , concat
        [ take 1 $ drop d term
        | d <- [1,3 .. length term - 2] ])
  | otherwise =
    ( concat
        [ take 1 $ drop n term
        | n <- [0,2 .. length term - 2] ]
    , concat
        [ take 1 $ drop d term
        | d <- [1,3 .. length term - 1] ])

addUpDigits :: [Int] -> Int
addUpDigits list =
  sum $
  map
    (\d ->
        if d > 9
          then sum $ map digitToInt $ show d
          else d)
    list

printSolution :: String -> IO ()
printSolution str = do
  putStr $ str ++ " is"
  if verifyISIN str
    then putStrLn " valid"
    else putStrLn " not valid"

main :: IO ()
main = do
  let isinnumbers =
        [ "US0378331005"
        , "US0373831005"
        , "U50378331005"
        , "US03378331005"
        , "AU0000XVGZA3"
        , "AU0000VXGZA3"
        , "FR0000988040"
        ]
  mapM_ printSolution isinnumbers
```
 

{{out}}

```txt
US0378331005 is valid
US0373831005 is not valid
U50378331005 is not valid
US03378331005 is not valid
AU0000XVGZA3 is valid
AU0000VXGZA3 is valid
FR0000988040 is valid
```


Or, making alternative choices from standard libraries:

```haskell
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

validISIN, isinPattern, luhn :: String -> Bool
validISIN = (&&) <$> isinPattern <*> (luhn . (show =<<) . stringInts)

isinPattern s =
  12 == length s &&
  let [l, m, r] = bites [2, 9, 1] s
  in all (`elem` capitals) l &&
     all (`elem` (capitals ++ digits)) m && head r `elem` digits

luhn x =
  let odd = [(: []), const []]
      even = reverse odd
      stream f = concat $ zipWith ($) (cycle f) (stringInts $ reverse x)
      s1 = sum (stream odd)
      s2 = sum $ sum . stringInts . show . (2 *) <$> stream even
  in rem (s1 + s2) 10 == 0

charMap :: M.Map Char Int
charMap = M.fromList $ zip (digits ++ capitals) [0 ..]

stringInts :: String -> [Int]
stringInts = fromMaybe [] . sequence . fmap (`M.lookup` charMap)

bites :: [Int] -> [a] -> [[a]]
bites ns xs =
  reverse . fst $
  foldr
    (\x (a, r) ->
        let (b, r_) = splitAt x r
        in (b : a, r_))
    ([], xs)
    (reverse ns)

capitals, digits :: String
capitals = ['A' .. 'Z']

digits = ['0' .. '9']

main :: IO ()
main =
  mapM_
    (print . ((,) <*> validISIN))
    [ "US0378331005"
    , "US0373831005"
    , "U50378331005"
    , "US03378331005"
    , "AU0000XVGZA3"
    , "AU0000VXGZA3"
    , "FR0000988040"
    ]
```

{{Out}}

```txt
("US0378331005",True)
("US0373831005",False)
("U50378331005",False)
("US03378331005",False)
("AU0000XVGZA3",True)
("AU0000VXGZA3",True)
("FR0000988040",True)
```



## J


'''Solution:'''

```j
require'regex'
validFmt=: 0 -: '^[A-Z]{2}[A-Z0-9]{9}[0-9]{1}$'&rxindex

df36=: ;@([: <@":"0 '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ'&i.)  NB. decimal from base 36
luhn=: 0 = 10 (| +/@,) 10 #.inv 1 2 *&|: _2 "."0\ |.            NB. as per task Luhn_test_of_credit_card_numbers#J

validISIN=: validFmt *. luhn@df36
```


'''Required Examples:'''

```j
   Tests=: 'US0378331005';'US0373831005';'U50378331005';'US03378331005';'AU0000XVGZA3';'AU0000VXGZA3';'FR0000988040'
   validISIN&> Tests
1 0 0 0 1 1 1
```



## Java

As the Luhn test method from the ''[[Luhn test of credit card numbers]]'' task is only a few lines, it has been embedded in the ISIN class for convenience.


```java
public class ISIN {
 
    public static void main(String[] args) {
        String[] isins = {
            "US0378331005", 
            "US0373831005", 
            "U50378331005", 
            "US03378331005",
            "AU0000XVGZA3", 
            "AU0000VXGZA3", 
            "FR0000988040"
        };
        for (String isin : isins)
            System.out.printf("%s is %s\n", isin, ISINtest(isin) ? "valid" : "not valid");
    }
 
    static boolean ISINtest(String isin) {
        isin = isin.trim().toUpperCase();
 
        if (!isin.matches("^[A-Z]{2}[A-Z0-9]{9}\\d$"))
            return false;
 
        StringBuilder sb = new StringBuilder();
        for (char c : isin.substring(0, 12).toCharArray())
            sb.append(Character.digit(c, 36));
 
        return luhnTest(sb.toString());
    }

    static boolean luhnTest(String number) {
        int s1 = 0, s2 = 0;
        String reverse = new StringBuffer(number).reverse().toString();
        for (int i = 0; i < reverse.length(); i++){
            int digit = Character.digit(reverse.charAt(i), 10);
            //This is for odd digits, they are 1-indexed in the algorithm.
            if (i % 2 == 0){
                s1 += digit;
            } else { // Add 2 * digit for 0-4, add 2 * digit - 9 for 5-9.
                s2 += 2 * digit;
                if(digit >= 5){
                    s2 -= 9;
                }
            }
        }
        return (s1 + s2) % 10 == 0;
    }
}
```



```txt
US0378331005 is valid
US0373831005 is not valid
U50378331005 is not valid
US03378331005 is not valid
AU0000XVGZA3 is valid
AU0000VXGZA3 is valid
FR0000988040 is valid
```



## Julia

{{works with|Julia|0.6}}


```julia
luhntest(x) = luhntest(parse(Int, x))

function checkISIN(inum::AbstractString)
    if length(inum) != 12 || !all(isalpha, inum[1:2]) return false end
    return parse.(Int, collect(inum), 36) |> join |> luhntest
end

for inum in ["US0378331005", "US0373831005", "U50378331005",
    "US03378331005", "AU0000XVGZA3", "AU0000VXGZA3", "FR0000988040"]
    @printf("%-15s %5s\n", inum, ifelse(checkISIN(inum), "pass", "fail"))
end
```


{{out}}

```txt
US0378331005     pass
US0373831005     fail
U50378331005     fail
US03378331005    fail
AU0000XVGZA3     pass
AU0000VXGZA3     pass
FR0000988040     pass
```



## Kotlin

As the Luhn test method is only a few lines, it's reproduced here for convenience:

```scala
// version 1.1

object Isin {
    val r = Regex("^[A-Z]{2}[A-Z0-9]{9}[0-9]$")

    fun isValid(s: String): Boolean {
        // check format
        if (!s.matches(r)) return false
        // validate checksum
        val sb = StringBuilder()
        for (c in s) {
            when (c) {
                in '0'..'9' -> sb.append(c)
                in 'A'..'Z' -> sb.append((c.toInt() - 55).toString().padStart(2, '0'))
            }
        }
        return luhn(sb.toString())
    }

    private fun luhn(s: String): Boolean {
        fun sumDigits(n: Int) = n / 10 + n % 10
        val t = s.reversed()
        val s1 = t.filterIndexed { i, _ -> i % 2 == 0 }.sumBy { it - '0' }
        val s2 = t.filterIndexed { i, _ -> i % 2 == 1 }.map { sumDigits((it - '0') * 2) }.sum()
        return (s1 + s2) % 10 == 0
    }
}

fun main(args: Array<String>) {
    val isins = arrayOf(
        "US0378331005", "US0373831005", "U50378331005", "US03378331005",
        "AU0000XVGZA3", "AU0000VXGZA3", "FR0000988040"
    )
    for (isin in isins) {
        println("$isin\t -> ${if (Isin.isValid(isin)) "valid" else "not valid"}")
    }
}
```


{{out}}

```txt

US0378331005     -> valid
US0373831005     -> not valid
U50378331005     -> not valid
US03378331005    -> not valid
AU0000XVGZA3     -> valid
AU0000VXGZA3     -> valid
FR0000988040     -> valid

```



## Lua


```Lua
function luhn (n)
    local revStr, s1, s2, digit, mod = n:reverse(), 0, 0
    for pos = 1, #revStr do
        digit = tonumber(revStr:sub(pos, pos))
        if pos % 2 == 1 then
            s1 = s1 + digit
        else
            digit = digit * 2
            if digit > 9 then
                mod = digit % 10
                digit = mod + ((digit - mod) / 10)
            end
            s2 = s2 + digit
        end
    end
    return (s1 + s2) % 10 == 0
end

function checkISIN (inStr)
    if #inStr ~= 12 then return false end
    local numStr = ""
    for pos = 1, #inStr do
        numStr = numStr .. tonumber(inStr:sub(pos, pos), 36)
    end
    return luhn(numStr)
end

local testCases = {
    "US0378331005",
    "US0373831005",
    "US0373831005",
    "US03378331005",
    "AU0000XVGZA3",
    "AU0000VXGZA3",
    "FR0000988040"
}
for _, ISIN in pairs(testCases) do print(ISIN, checkISIN(ISIN)) end
```

{{out}}

```txt
US0378331005    true
US0373831005    false
US0373831005    false
US03378331005   false
AU0000XVGZA3    true
AU0000VXGZA3    true
FR0000988040    true
```



## Perl

We reuse the <tt>luhn_test()</tt> function from ''[[Luhn test of credit card numbers#Perl]]''.

```perl
use strict;
use English;
use POSIX;
use Test::Simple tests => 7;

ok(   validate_isin('US0378331005'),  'Test 1');	
ok( ! validate_isin('US0373831005'),  'Test 2');
ok( ! validate_isin('U50378331005'),  'Test 3');
ok( ! validate_isin('US03378331005'), 'Test 4');
ok(   validate_isin('AU0000XVGZA3'),  'Test 5');	
ok(   validate_isin('AU0000VXGZA3'),  'Test 6');
ok(   validate_isin('FR0000988040'),  'Test 7');	
exit 0;

sub validate_isin {
    my $isin = shift;
    $isin =~ /\A[A-Z]{2}[A-Z\d]{9}\d\z/s or return 0;
    my $base10 = join(q{}, map {scalar(POSIX::strtol($ARG, 36))}
                               split(//s, $isin));
    return luhn_test($base10);
}
```

{{out}}

```txt
1..7
ok 1 - Test 1
ok 2 - Test 2
ok 3 - Test 3
ok 4 - Test 4
ok 5 - Test 5
ok 6 - Test 6
ok 7 - Test 7
```



## Perl 6

{{works with|Rakudo|2018.12}}

Using the <tt>luhn-test</tt> function from the ''[[Luhn test of credit card numbers#Perl 6|Luhn test of credit card numbers]]'' task.


```perl6
my $ISIN = /
    ^ <[A..Z]>**2 <[A..Z0..9]>**9 <[0..9]> $
    <?{ luhn-test $/.comb.map({ :36($_) }).join }>
/;

sub luhn-test ($number --> Bool) {
    my @digits = $number.comb.reverse;
    my $sum = @digits[0,2...*].sum
            + @digits[1,3...*].map({ |($_ * 2).comb }).sum;
    return $sum %% 10;
}

# Testing:

say "$_ is { m/$ISIN/ ?? "valid" !! "not valid"}" for <
US0378331005
US0373831005
U50378331005
US03378331005
AU0000XVGZA3
AU0000VXGZA3
FR0000988040
>;
```


{{out}}

```txt

US0378331005 is valid
US0373831005 is not valid
U50378331005 is not valid
US03378331005 is not valid
AU0000XVGZA3 is valid
AU0000VXGZA3 is valid
FR0000988040 is valid

```



## Phix

Note this (slightly better) version of Luhn() has the reverse() inside it, whereas the original did not.

```Phix
function Luhn(string st)
integer s=0, d
    st = reverse(st)
    for i=1 to length(st) do
        d = st[i]-'0'
        s += iff(mod(i,2)?d,d*2-(d>4)*9)
    end for
    return remainder(s,10)=0
end function
 
function valid_ISIN(string st)
-- returns 1 if valid, else 0/2/3/4.
-- (feel free to return 0 instead of 2/3/4)
    if length(st)!=12 then return 2 end if
    for i=length(st) to 1 by -1 do
        integer ch = st[i]
        if ch>='A' then
            if ch>'Z' then return 3 end if
            st[i..i] = sprintf("%d",ch-55)
        elsif i<=2 then
            return 4
        elsif ch<'0' or ch>'9' then
            return 3
        end if
    end for
    return Luhn(st)
end function

sequence tests = {"US0378331005",   --  valid   
                  "US0373831005",   --  not valid       The transposition typo is caught by the checksum constraint.
                  "U50378331005",   --  not valid       The substitution typo is caught by the format constraint.
                  "US03378331005",  --  not valid       The duplication typo is caught by the format constraint.
                  "AU0000XVGZA3",   --  valid   
                  "AU0000VXGZA3",   --  valid   Unfortunately, not all transposition typos are caught by the checksum constraint.
                  "FR0000988040"}   --  valid

constant reasons = {"wrong checksum","valid","wrong length","bad char","wrong country"}

for i=1 to length(tests) do
    string ti = tests[i]
    printf(1,"%s : %s\n",{ti,reasons[valid_ISIN(ti)+1]})
end for
```

{{out}}

```txt

US0378331005 : valid
US0373831005 : wrong checksum
U50378331005 : wrong country
US03378331005 : wrong length
AU0000XVGZA3 : valid
AU0000VXGZA3 : valid
FR0000988040 : valid

```



## PicoLisp

Using the <tt>luhn</tt> function defined at ''[[Luhn test of credit card numbers#PicoLisp]]'':

```PicoLisp
(de isin (Str)
   (let Str (mapcar char (chop Str))
      (and
         (= 12 (length Str))
         (<= 65 (car Str) 90)
         (<= 65 (cadr Str) 90)
         (luhn
            (pack
               (mapcar
                  '((N)
                     (- N (if (<= 48 N 57) 48 55)) )
                  Str ) ) ) ) ) )
(println
   (mapcar
      isin
      (quote
         "US0378331005"
         "US0373831005"
         "U50378331005"
         "US03783310005"
         "AU0000XVGZA3"
         "AU0000VXGZA3"
         "FR0000988040" ) ) )
```

{{out}}

```txt
(0 NIL NIL NIL 0 0 0)
```



## PowerShell


```PowerShell

function Test-ISIN
{
    [CmdletBinding()]
    [OutputType([bool])]
    Param
    (
        [Parameter(Mandatory=$true, Position=0)]
        [ValidatePattern("[A-Z]{2}\w{9}\d")]
        [ValidateScript({$_.Length -eq 12})]
        [string]
        $Number
    )

    function Split-Array
    {
        $array = @(), @()
        $input | ForEach-Object {$array[($index = -not $index)] += $_}
        $array[1], $array[0]
    }

    filter ConvertTo-Digit
    {
        if ($_ -gt 9)
        {
            $_.ToString().ToCharArray() | ForEach-Object -Begin {$n = 0} -Process {$n += [Char]::GetNumericValue($_)} -End {$n}
        }
        else
        {
            $_
        }
    }


    $checkDigit = $Number[-1]

    $digits = ($Number -replace ".$").ToCharArray() | ForEach-Object {
        if ([Char]::IsDigit($_))
        {
            [Char]::GetNumericValue($_)
        }
        else
        {
            [int][char]$_ - 55
        }
    }

    $odds, $evens = ($digits -join "").ToCharArray() | Split-Array

    if ($odds.Count -gt $evens.Count)
    {
        $odds  = $odds  | ForEach-Object {[Char]::GetNumericValue($_) * 2} | ConvertTo-Digit
        $evens = $evens | ForEach-Object {[Char]::GetNumericValue($_)}
    }
    else
    {
        $odds  = $odds  | ForEach-Object {[Char]::GetNumericValue($_)}
        $evens = $evens | ForEach-Object {[Char]::GetNumericValue($_) * 2} | ConvertTo-Digit
    }

    $sum = ($odds  | Measure-Object -Sum).Sum + ($evens | Measure-Object -Sum).Sum

    (10 - ($sum % 10)) % 10 -match $checkDigit
}

```


```PowerShell

"US0378331005","US0373831005","US0337833103","AU0000XVGZA3","AU0000VXGZA3","FR0000988040" | ForEach-Object {
    [PSCustomObject]@{
        ISIN    = $_
        IsValid = Test-ISIN -Number $_
    }
}

```

{{Out}}

```txt

ISIN         IsValid
----         -------
US0378331005    True
US0373831005   False
US0337833103   False
AU0000XVGZA3    True
AU0000VXGZA3    True
FR0000988040    True

```



## PureBasic


```PureBasic
EnableExplicit

Procedure.b Check_ISIN(*c.Character)
  Define count.i=0, Idx.i=1, v.i=0, i.i
  Dim s.i(24)
  
  If MemoryStringLength(*c) > 12 : ProcedureReturn #False : EndIf
  
  While *c\c
    count+1
    If *c\c>='0' And *c\c<='9'      
      If count<=2 : ProcedureReturn #False : EndIf
      s(Idx)= *c\c - '0'
      Idx+1
    ElseIf *c\c>='A' And *c\c<='Z'      
      s(Idx)= (*c\c - ('A'-10)) / 10
      Idx+1
      s(Idx)= (*c\c - ('A'-10)) % 10
      Idx+1
    Else
      ProcedureReturn #False      
    EndIf    
    *c + SizeOf(Character)
  Wend
  
  For i=Idx-2 To 0 Step -2
    If s(i)*2 > 9
      v+ s(i)*2 -9
    Else
      v+ s(i)*2
    EndIf
    v+s(i+1)
  Next

  ProcedureReturn Bool(v%10=0)      
EndProcedure

Define.s s
OpenConsole("Validate_International_Securities_Identification_Number (ISIN)")

If ReadFile(0,"c:\code_pb\rosettacode\data\isin.txt")
  While Not Eof(0)
    s=ReadString(0)
    Print(s+~"\t")
    If Check_ISIN(@s) : PrintN("TRUE") : Else : PrintN("FALSE") : EndIf    
  Wend
  CloseFile(0)
EndIf
Input()
```

{{Out}}

```txt
US0378331005    TRUE
US0373831005    FALSE
U50378331005    FALSE
US03378331005   FALSE
AU0000XVGZA3    TRUE
AU0000VXGZA3    TRUE
FR0000988040    TRUE
```



## Python



```python
def check_isin(a):
    if len(a) != 12 or not all(c.isalpha() for c in a[:2]) or not all(c.isalnum() for c in a[2:]):
        return False
    s = "".join(str(int(c, 36)) for c in a)
    return 0 == (sum(sum(divmod(2 * (ord(c) - 48), 10)) for c in s[-2::-2]) +
                 sum(ord(c) - 48 for c in s[::-2])) % 10

# A more readable version 
def check_isin_alt(a):
    if len(a) != 12:
        return False
    s = []
    for i, c in enumerate(a):
        if c.isdigit():
            if i < 2:
                return False
            s.append(ord(c) - 48)
        elif c.isupper():
            if i == 11:
                return False
            s += divmod(ord(c) - 55, 10)
        else:
            return False
    v = sum(s[::-2])
    for k in s[-2::-2]:
        k = 2 * k
        v += k - 9 if k > 9 else k
    return v % 10 == 0

[check_isin(s) for s in ["US0378331005", "US0373831005", "U50378331005", "US03378331005",
                         "AU0000XVGZA3", "AU0000VXGZA3", "FR0000988040"]]

# [True, False, False, False, True, True, True]
```



## Racket



```racket

#lang racket

;; convert a base36 character (#\0 - #\Z) to its equivalent
;; in base 10 as a string ("0" - "35")
(define (base36-char->base10-string c)
  (let ([char-int (char->integer (char-upcase c))]
        [zero-int (char->integer #\0)]
        [nine-int (char->integer #\9)]
        [A-int (char->integer #\A)]
        [Z-int (char->integer #\Z)])
    (cond [(and (>= char-int zero-int) (<= char-int nine-int)) (~a c)]
          [(and (>= char-int A-int) (<= char-int Z-int)) (~a (+ (- char-int A-int) 10))]
          [else null])))

;; substitute equivalent base 10 numbers for base 36 characters in string
;; this is a character-by-character substitution not a conversion
;; of a base36 number to a base10 number
(define (base36-string-characters->base10-string-characters s)
  (for/fold ([joined ""])
            ([tenstr (map base36-char->base10-string (string->list (string-upcase s)))])
    (values (string-append joined tenstr))))

;; This uses the Racket Luhn solution
(define [isin-test? s]
  (let ([RE (pregexp "^[A-Z]{2}[A-Z0-9]{9}[0-9]{1}$")])
    (and
     (regexp-match? RE s)
     (luhn-test (string->number (base36-string-characters->base10-string-characters s))))))

(define test-cases '("US0378331005" "US0373831005" "U50378331005" "US03378331005" "AU0000XVGZA3" "AU0000VXGZA3" "FR0000988040"))

(map isin-test? test-cases)
;; -> '(#t #f #f #f #t #t #t)

```


{{out}}

'(#t #f #f #f #t #t #t)


## REXX


```rexx
/*REXX program validates the  checksum digit for an  International Securities ID number.*/
parse arg z                                      /*obtain optional  ISINs  from the C.L.*/
if z=''  then z= "US0378331005 US0373831005 U50378331005 US03378331005 AU0000XVGZA3" ,
                 'AU0000VXGZA3 FR0000988040'     /* [↑]  use the default list of  ISINs.*/
                                                 /* [↓]  process  all  specified  ISINs.*/
      do n=1  for words(z);  x=word(z, n);  y=x  /*obtain an  ISIN  from the  Z  list.  */
      $=                                         /* [↓]  construct list of ISIN digits. */
         do k=1  for length(x);  _=substr(x,k,1) /*the ISIN may contain alphabetic chars*/
         p=pos(_, '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ') /*X must contain A──►Z, 0──►9.*/
         if p==0  then y=                                 /*trigger  "not"  valid below.*/
                  else $=$ || p-1                /*convert  X  string (base 36 ──► dec).*/
         end   /*k*/                             /* [↑]  convert  alphabetic ──► digits.*/
      @=                                         /*placeholder for the "not" in message.*/
      if length(y)\==12            then @= "not" /*check if the ISIN is exactly 12 chars*/
      if \datatype( left(x,2),'U') then @= "not" /*  "    "  "    "  1st 2 chars cap let*/
      if \datatype(right(x,1),'W') then @= "not" /*  "    "  "    "  last char not digit*/
      if @==''  then  if \luhn($)  then @= "not" /*  "    "  "    "  passed Luhn test.  */
      say right(x,30)   right(@, 5)   "valid"    /*display the   yea  or  nay   message.*/
      end   /*n*/                                /* [↑] 1st 3 IFs could've been combined*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
Luhn: procedure;  parse arg x;       $=0         /*get credit card number;  zero $ sum. */
      y=reverse(left(0, length(x) // 2)x)        /*add leading zero if needed, & reverse*/
                             do j=1  to length(y)-1  by 2;    _=2  *  substr(y, j+1, 1)
                             $=$ + substr(y, j, 1)  +  left(_, 1)  +  substr(_, 2  , 1, 0)
                             end   /*j*/         /* [↑]   sum the  odd and even  digits.*/
      return right($, 1)==0                      /*return "1" if number passed Luhn test*/
```

'''output'''   when using the defaults for input:

```txt

                  US0378331005       valid
                  US0373831005   not valid
                  U50378331005   not valid
                 US03378331005   not valid
                  AU0000XVGZA3       valid
                  AU0000VXGZA3       valid
                  FR0000988040       valid 

```



## Ring


```ring

# Project : Validate International Securities Identification Number

decimals(0)

test = ["US0378331005",
           "US0373831005",
           "U50378331005",
           "US03378331005",
           "AU0000XVGZA3",
           "AU0000VXGZA3",
           "FR0000988040"]

for n = 1 to len(test)
      testold = test[n] 
      ascii1 = ascii(left(test[n],1))
      ascii2 = ascii(substr(test[n],2,1))
      if len(test[n]) != 12 or (ascii1 < 65 or ascii1 > 90) or (ascii2 < 65 or ascii2 > 90)
         see test[n] + " -> Invalid" + nl
         loop
      ok
      for m = 1 to len(test[n])
           if ascii(test[n][m]) > 64 and ascii(test[n][m]) < 91
              asc = ascii(test[n][m]) - 55
              test[n] = left(test[n],m-1) + string(asc) + right(test[n],len(test[n])-m)
           ok
      next
      see testold + " -> " + cardtest(test[n]) + nl
next
 
func cardtest(numstr)
        revstring = revstr(numstr)
        s1 = revodd(revstring)
        s2 = reveven(revstring)
        s3 =right(string(s1+s2), 1)
        if s3 = "0"
           return "Valid"
        else
           return "Invalid"
        ok
 
func revstr(str)
      strnew = ""
      for nr = len(str) to 1 step -1
           strnew = strnew + str[nr]
      next
      return strnew
 
func revodd(str)
        strnew = ""
        for nr = 1 to len(str) step 2
             strnew = strnew + str[nr]
        next
        sumodd = 0
        for p = 1 to len(strnew)
              sumodd = sumodd + number(strnew[p])
        next     
        return sumodd
 
func reveven(str)
        strnew = ""
        for nr = 2 to len(str) step 2
             strnew = strnew + str[nr]
        next
        lsteven = []
        for p = 1 to len(strnew)
             add(lsteven, string(2*number(strnew[p])))
        next  
        arreven = list(len(lsteven))
        for q = 1 to len(lsteven)
              sum = 0
              for w = 1 to len(lsteven[q])
                    sum = sum + lsteven[q][w]
              next
              arreven[q] = sum
        next
        sumarr = 0
        for x = 1 to len(arreven)
             sumarr = sumarr + arreven[x]
        next
        return sumarr 

```

Output:

```txt

US0378331005 -> Valid
US0373831005 -> Invalid
U50378331005 -> Invalid
US03378331005 -> Invalid
AU0000XVGZA3 -> Valid
AU0000VXGZA3 -> Valid
FR0000988040 -> Valid

```



## Ruby

Using a pre-existing luhn method:

```ruby
RE = /\A[A-Z]{2}[A-Z0-9]{9}[0-9]{1}\z/

def valid_isin?(str)
  return false unless str =~ RE
  luhn(str.chars.map{|c| c.to_i(36)}.join)
end

p %w(US0378331005
US0373831005
U50378331005
US03378331005
AU0000XVGZA3
AU0000VXGZA3
FR0000988040).map{|tc| valid_isin?(tc) }	 
 
# => [true, false, false, false, true, true, true]
```



## Rust



```Rust
extern crate luhn_cc;

use luhn_cc::compute_luhn;

fn main() {
    assert_eq!(validate_isin("US0378331005"), true);
    assert_eq!(validate_isin("US0373831005"), false);
    assert_eq!(validate_isin("U50378331005"), false);
    assert_eq!(validate_isin("US03378331005"), false);
    assert_eq!(validate_isin("AU0000XVGZA3"), true);
    assert_eq!(validate_isin("AU0000VXGZA3"), true);
    assert_eq!(validate_isin("FR0000988040"), true);
}

fn validate_isin(isin: &str) -> bool {
    // Preliminary checks to avoid working on non-ASCII stuff
    if !isin.chars().all(|x| x.is_alphanumeric()) || isin.len() != 12 {
        return false;
    }
    if !isin[..2].chars().all(|x| x.is_alphabetic())
        || !isin[2..12].chars().all(|x| x.is_alphanumeric())
        || !isin.chars().last().unwrap().is_numeric()
    {
        return false;
    }

    // Converts the alphanumeric string in a numeric-only string
    let bytes = isin.as_bytes();

    let s2 = bytes.iter()
        .flat_map(|&c| {
            if c.is_ascii_digit() {
                vec![c]
            }
            else {
                (c + 10 - ('A' as u8)).to_string().into_bytes()
            }
        }).collect::<Vec<u8>>();

    let string = std::str::from_utf8(&s2).unwrap();
    let number = string.parse::<usize>().unwrap();

    return compute_luhn(number);
}

```



## SAS


```sas
data test;
length isin $20 ok $1;
input isin;
keep isin ok;
array s{24};
link isin;
return;
isin:
ok="N";
n=length(isin);
if n=12 then do;
    j=0;
    do i=1 to n;
        k=rank(substr(isin,i,1));
        if k>=48 & k<=57 then do;
            if i<3 then return;
            j+1;
            s{j}=k-48;
        end;
        else if k>=65 & k<=90 then do;
            if i=12 then return;
            k+-55;
            j+1;
            s{j}=int(k/10);
            j+1;
            s{j}=mod(k,10);
        end;
        else return;
    end;

    v=sum(of s{*});
    do i=j-1 to 1 by -2;
        v+s{i}-9*(s{i}>4);
    end;

if mod(v,10)=0 then ok="Y";
end;
return;
cards;
US0378331005
US0373831005
U50378331005
US03378331005
AU0000XVGZA3
AU0000VXGZA3
FR0000988040
;
run;
```



## Scala

{{Out}}Best seen running in your browser either by [https://scalafiddle.io/sf/D9ax4Js/0 ScalaFiddle (ES aka JavaScript, non JVM)] or [https://scastie.scala-lang.org/yOymYqoPSEeA7K7rjgn65g Scastie (remote JVM)].

```Scala
object Isin extends App {
  val isins = Seq("US0378331005", "US0373831005", "U50378331005",
    "US03378331005", "AU0000XVGZA3","AU0000VXGZA3", "FR0000988040")

  private def ISINtest(isin: String): Boolean = {
    val isin0 = isin.trim.toUpperCase

    def luhnTestS(number: String): Boolean = {

      def luhnTestN(digits: Seq[Int]): Boolean = {

        def checksum(digits: Seq[Int]): Int = {
          digits.reverse.zipWithIndex
            .foldLeft(0) {
              case (sum, (digit, i)) =>
                if (i % 2 == 0) sum + digit
                else sum + (digit * 2) / 10 + (digit * 2) % 10
            } % 10
        }

        checksum(digits) == 0
      }

      luhnTestN(number.map { c =>
        assert(c.isDigit, s"$number has a non-digit error")
        c.asDigit
      })
    }

    if (!isin0.matches("^[A-Z]{2}[A-Z0-9]{9}\\d$")) false
    else {
      val sb = new StringBuilder
      for (c <- isin0.substring(0, 12)) sb.append(Character.digit(c, 36))
      luhnTestS(sb.toString)
    }
  }

  isins.foreach(isin => println(f"$isin is ${if (ISINtest(isin)) "" else "not"}%s valid"))

}
```



## SQL PL

{{works with|Db2 LUW}} version 9.7 or higher.
With SQL PL:

```sql pl

--#SET TERMINATOR @

SET SERVEROUTPUT ON @

CREATE OR REPLACE FUNCTION VALIDATE_ISIN (
  IN IDENTIFIER VARCHAR(12)
 ) RETURNS SMALLINT
 -- ) RETURNS BOOLEAN
 BEGIN
  DECLARE CHECKSUM_FUNC CHAR(1);
  DECLARE CONVERTED VARCHAR(24);
  DECLARE I SMALLINT;
  DECLARE LENGTH SMALLINT;
  DECLARE RET SMALLINT DEFAULT 1;
  --DECLARE RET BOOLEAN DEFAULT FALSE;
  DECLARE CHAR_AT CHAR(1);
  DECLARE INVALID_CHAR CONDITION FOR SQLSTATE 'ISIN1';

  SET CHAR_AT = SUBSTR(IDENTIFIER, 1, 1);
  IF (ASCII(CHAR_AT) < 65 OR 90 < ASCII(CHAR_AT)) THEN
   SIGNAL INVALID_CHAR SET MESSAGE_TEXT = 'Country code with invalid characters';
  END IF;
  SET CHAR_AT = SUBSTR(IDENTIFIER, 2, 1);
  IF (ASCII(CHAR_AT) < 65 OR 90 < ASCII(CHAR_AT)) THEN
   SIGNAL INVALID_CHAR SET MESSAGE_TEXT = 'Country code with invalid characters';
  END IF;

  -- Convert letters to numbers.
  SET I = 1;
  SET CONVERTED = '';
  SET LENGTH = LENGTH(IDENTIFIER);
  WHILE (I <= LENGTH) DO
   SET CHAR_AT = SUBSTR(IDENTIFIER, I, 1);
   IF (48 <= ASCII(CHAR_AT) AND ASCII(CHAR_AT) <= 57) THEN
    SET CONVERTED = CONVERTED || CHAR_AT;
   ELSE
    SET CONVERTED = CONVERTED || (ASCII(CHAR_AT) - 55);
   END IF;
   SET I = I + 1;
  END WHILE;

  CALL DBMS_OUTPUT.PUT_LINE(CONVERTED);
  
  -- This function is implemented in Rosetta code.
  SET CHECKSUM_FUNC = LUHN_TEST(CONVERTED);
  IF (CHECKSUM_FUNC = 0) THEN
   SET RET = 0;
   --SET RET = TRUE;
  END IF;

  RETURN RET;
 END @

```

Output:

```txt

db2 -td@
db2 => BEGIN
...
db2 (cont.) => END @
DB20000I  The SQL command completed successfully.
db2 => VALUES VALIDATE_ISIN('US0378331005')@
1     
------
     0

  1 record(s) selected.

30280378331005
It is a valid number 27+23=50
db2 => VALUES VALIDATE_ISIN('US0373831005')@
1     
------
     1

  1 record(s) selected.

30280373831005
It is NOT a valid number 22+24=46
db2 => VALUES VALIDATE_ISIN('U50378331005')@
1     
------
SQL0438N  Application raised error or warning with diagnostic text: "Country 
code with invalid characters".  SQLSTATE=ISIN1
db2 => VALUES VALIDATE_ISIN('U503378331005')@
1     
------
SQL0433N  Value "U503378331005" is too long.  SQLSTATE=22001
db2 => VALUES VALIDATE_ISIN('AU0000XVGZA3')@
1     
------
     0

  1 record(s) selected.

1030000033311635103
It is a valid number 18+12=30
db2 => VALUES VALIDATE_ISIN('AU0000VXGZA3')@
1     
------
     0

  1 record(s) selected.

1030000031331635103
It is a valid number 18+12=30
db2 => VALUES VALIDATE_ISIN('FR0000988040')@

1     
------
     0

  1 record(s) selected.

15270000988040
It is a valid number 20+30=50

```



## Tcl


```Tcl
package require Tcl 8.6   ;# mostly needed for [assert].  Substitute a simpler one or a NOP if required.
```

A proc like assert is always good to have around.  This one tries to report values used in its expression using subst:

```Tcl
proc assert {expr} {    ;# for "static" assertions that throw nice errors
    if {![uplevel 1 [list expr $expr]]} {
        set msg "{$expr}"
        catch {append msg " {[uplevel 1 [list subst -noc $expr]]}"}
        tailcall throw {ASSERT ERROR} $msg
    }
}
```

isin itself is a simple package.  We compute the alphabet when the package is loaded in _init, because that's more fun than typing out the table:

```Tcl
namespace eval isin {
    proc _init {} {         ;# sets up the map used on every call
        variable map
        set alphabet abcdefghijklmnopqrstuvwxyz
        set n 9
        lmap c [split $alphabet ""] {
            lappend map $c [incr n]
        }
    }
    _init

    proc normalize {isin} {
        variable map
        string map $map [string tolower [string trim $isin]]
    }

    # copied from "Luhn test of credit card numbers"
    # included here for ease of testing, and because it is short
    proc luhn digitString {
        if {[regexp {[^0-9]} $digitString]} {error "not a number"}
        set sum 0
        set flip 1
        foreach ch [lreverse [split $digitString {}]] {
            incr sum [lindex {
                {0 1 2 3 4 5 6 7 8 9}
                {0 2 4 6 8 1 3 5 7 9}
            } [expr {[incr flip] & 1}] $ch]
        }
        return [expr {($sum % 10) == 0}]
    }

    proc validate {isin} {
        if {![regexp {^[A-Z]{2}[A-Z0-9]{9}[0-9]$} $isin]} {return false}
        luhn [normalize $isin]
    }

}
```

To run the test suite, we use the tcltest framework included with Tcl:

```Tcl
package require tcltest

tcltest::test isin-1 "Test isin validation" -body {
    foreach {isin ok} {
        US0378331005    yes
        US0373831005    no
        U50378331005    no
        US03378331005   no
        AU0000XVGZA3    yes
        AU0000VXGZA3    yes
        FR0000988040    yes
    } {
        if {$ok} {
            assert {[isin::validate $isin]}
        } else {
            assert {![isin::validate $isin]}
        }
    }
    return ok
} -result ok
```


=={{header|Transact-SQL}}==

<lang Transact-SQL>
CREATE FUNCTION dbo._ISINCheck( @strISIN VarChar(40) )
RETURNS bit
AS
BEGIN
--*** Test an ISIN code and return 1 if it is valid, 0 if invalid.
DECLARE @bValid	 bit;

SET @bValid = CASE WHEN @strISIN LIKE '[A-Z][A-Z][A-Z,0-9][A-Z,0-9][A-Z,0-9][A-Z,0-9][A-Z,0-9][A-Z,0-9][A-Z,0-9][A-Z,0-9][A-Z,0-9][0-9]' THEN 1 ELSE 0 END
IF @bValid = 1
	BEGIN
		DECLARE @strTest VarChar(40) = '';
		DECLARE @strAdd  VarChar(2);
		DECLARE @p INT = 0;
		WHILE @p < LEN(@strISIN)
			BEGIN
				SET @p = @p+1;
				SET @strAdd = SUBSTRING(@strISIN,@p,1);
				IF @strAdd LIKE '[A-Z]' SET @strAdd = CONVERT(VarChar(2),ASCII(UPPER(@strAdd))-55);
				SET @strTest = @strTest + @strAdd;
			END;

		-- Proceed with Luhn test
		DECLARE @strLuhn VarChar(40) = REVERSE(@strTest); -- usage: set once, never changed
		DECLARE @strS2Values VarChar(10) = '0246813579';  -- constant: maps digits to their S2 summed values
		SET @p = 0; -- reset loop counter
		DECLARE @intValue INT;
		DECLARE @intSum	INT = 0;
		-- loop through the reversed string, get the value (even-positioned digits are mapped) and add it to @intSum
		WHILE @p < LEN(@strLuhn)
			BEGIN
				SET @p = @p+1;
				SET @intValue = CONVERT(INT, SUBSTRING(@strLuhn,@p,1) ) -- value of the digit at position @p in the string
				IF @p % 2 = 0	SET @intValue = CONVERT(INT,SUBSTRING(@strS2Values,@intValue+1,1))
				SET @intSum = @intSum + @intValue
			END
		-- If the of the digits' mapped values ends in 0 (modulo 10 = 0) then the Luhn test succeeds
		SET @bValid = CASE WHEN @intSum % 10 = 0 THEN 1 ELSE 0 END 
	END;

RETURN @bValid
END

```

Testing
<lang Transact-SQL>
-- Testing. The following tests all pass.
;WITH ISIN_Tests AS
( SELECT 'US0378331005' AS ISIN, 1 Expected
  UNION SELECT 'US0373831005',0
  UNION SELECT 'U50378331005',0
  UNION SELECT 'US03378331005',0
  UNION SELECT 'AU0000XVGZA3',1
  UNION SELECT 'AU0000VXGZA3',1
  UNION SELECT 'FR0000988040',1
  UNION SELECT '0___garbage',0
  UNION SELECT '',0
)
SELECT ISIN, Expected, dbo._ISINCheck(ISIN) AS TestResult FROM ISIN_Tests ORDER BY ISIN

```



## VBScript


```vb
' Validate International Securities Identification Number - 03/03/2019

buf=buf&test("US0378331005")&vbCrLf
buf=buf&test("US0373831005")&vbCrLf
buf=buf&test("U50378331005")&vbCrLf
buf=buf&test("US03378331005")&vbCrLf
buf=buf&test("AU0000XVGZA3")&vbCrLf
buf=buf&test("AU0000VXGZA3")&vbCrLf
buf=buf&test("FR0000988040")&vbCrLf
msgbox buf,,"Validate International Securities Identification Number"

function test(cc)
	dim err,c,r,s,i1,i2
	if len(cc)=12 then
		for i=1 to len(cc)
			p=instr("0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ",mid(cc,i,1))
			if p<>0 then c=c&(p-1) else err=1
		next 'i
		for i=1 to 2
			if instr("ABCDEFGHIJKLMNOPQRSTUVWXYZ",mid(cc,i,1))=0 then err=1 
		next 'i
		if err=0 then
			for i=len(c) to 1 step -1
				r=r&mid(c,i,1)
			next 'i
			for i=1 to len(r) step 2
				i1=i1+cint(mid(r,i,1))
			next 'i
			for i=2 to len(r) step 2
				ii=cint(mid(r,i,1))*2
				if ii>=10 then ii=ii-9
				i2=i2+ii
			next 'i
			s=cstr(i1+i2)
			if mid(s,len(s),1)="0" then
				msg="valid"
			else
				msg="invalid ??1"
			end if
		else
			msg="invalid ??2"
		end if
	else
		msg="invalid ??3"
	end if
	test=cc&" "&msg
end function 'test 
```

{{out}}

```txt

US0378331005 valid
US0373831005 invalid ??1
U50378331005 invalid ??2
US03378331005 invalid ??3
AU0000XVGZA3 valid
AU0000VXGZA3 valid
FR0000988040 valid

```



## Visual Basic

{{works with|Visual Basic|VB6 Standard}}
Calls LuhnCheckPassed() function described at [[Luhn_test_of_credit_card_numbers#Visual_Basic]]

```vb
Function IsValidISIN(ByVal ISIN As String) As Boolean
Dim s As String, c As String
Dim i As Long
  If Len(ISIN) = 12 Then
    For i = 1 To Len(ISIN)
      c = UCase$(Mid(ISIN, i, 1))
        Select Case c
        Case "A" To "Z"
          If i = 12 Then Exit Function
          s = s & CStr(Asc(c) - 55)
        Case "0" To "9"
          If i < 3 Then Exit Function
          s = s & c
        Case Else
          Exit Function
        End Select
   Next i
  IsValidISIN = LuhnCheckPassed(s)
  End If
End Function
```

Test:

```vb
Sub Main()
  Debug.Assert IsValidISIN("US0378331005")
  Debug.Assert Not IsValidISIN("US0373831005")
  Debug.Assert Not IsValidISIN("U50378331005")
  Debug.Assert Not IsValidISIN("US03378331005")
  Debug.Assert IsValidISIN("AU0000XVGZA3")
  Debug.Assert IsValidISIN("AU0000VXGZA3")
  Debug.Assert IsValidISIN("FR0000988040")
  Debug.Assert Not IsValidISIN("FR000098804O")
End Sub
```



## Yabasic

{{trans|FreeBASIC}}

```Yabasic
sub luhntest(cardnr$)
    local i, j, s1, s2, l
 
    cardnr$ = Trim$(cardnr$) // remove spaces
    
    l = Len(cardnr$)
 
    // sum odd numbers
    For i = l To 1 Step -2
        s1 = s1 + (asc(mid$(cardnr$, i, 1)) - Asc("0"))
    Next
    // sum even numbers
    For i = l-1 To 1 Step -2
        j = asc(mid$(cardnr$, i, 1)) - Asc("0")
        j = j * 2
        If j > 9 j = mod(j, 10) + 1
        s2 = s2 + j
    Next
 
    return mod(s1 + s2, 10) = 0
End sub
 
// ------=< MAIN >=-----
 
data "US0378331005", "US0373831005", "U50378331005", "US03378331005", "AU0000XVGZA3", "AU0000VXGZA3", "FR0000988040", ""
 
do
    read test_item$
    if test_item$ = "" break
    
    l = Len(test_item$)
    If l <> 12 Then
        Print test_item$, " Invalid, length <> 12 char."
        Continue
    End If
    c1$ = mid$(test_item$, 1, 1) : c2$ = mid$(test_item$, 2, 1)
    If c1$ < "A" Or c1$ > "Z" or c2$ < "A" or c2$ > "Z" Then
        Print test_item$, " Invalid, number needs to start with 2 characters"
        Continue
    End If
    test_str$ = ""
    For n = 1 To l
        x = asc(mid$(test_item$, n, 1)) - Asc("0")
        // if is a letter we to correct for that
        If x > 9 x = x - 7
        If x < 10 Then
            test_str$ = test_str$ + Str$(x)
        Else // two digest number
            test_str$ = test_str$ + Str$(int(x / 10)) + Str$(mod(x, 10))
        End If
    Next
    Print test_item$;
    if luhntest(test_str$) then print " Valid" else print " Invalid, checksum error" end if
loop

```



## zkl

Uses the luhn test from [[Luhn_test_of_credit_card_numbers#zkl]] (copied here as it is short).

```zkl
fcn validateISIN(isin){
   RegExp(String("^","[A-Z]"*2,"[A-Z0-9]"*9,"[0-9]$")).matches(isin) and 
      luhnTest(isin.split("").apply("toInt",36).concat().toInt())
}
fcn luhnTest(n){
   0 == (n.split().reverse().reduce(fcn(s,n,clk){
      s + if(clk.inc()%2) n else 2*n%10 + n/5 },0,Ref(1)) %10)
}
```


```zkl
println("     ISIN       Valid?");
foreach isin in (T("US0378331005","US0373831005","U50378331005",
	       "US03378331005","AU0000XVGZA3","AU0000VXGZA3","FR0000988040")){
   println(isin," --> ",validateISIN(isin));
}
```

{{out}}

```txt

     ISIN       Valid?
US0378331005 --> True
US0373831005 --> False
U50378331005 --> False
US03378331005 --> False
AU0000XVGZA3 --> True
AU0000VXGZA3 --> True
FR0000988040 --> True

```

