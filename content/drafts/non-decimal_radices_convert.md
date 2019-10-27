+++
title = "Non-decimal radices/Convert"
description = ""
date = 2019-08-27T10:15:16Z
aliases = []
[extra]
id = 2680
[taxonomies]
categories = []
tags = []
+++

{{task|Arithmetic operations}}

Number base conversion is when you express a stored integer in an integer base, such as in octal (base 8) or binary (base 2). It also is involved when you take a string representing a number in a given base and convert it to the stored integer form. Normally, a stored integer is in binary, but that's typically invisible to the user, who normally enters or sees stored integers as decimal.  


;Task:
Write a function (or identify the built-in function) which is passed a non-negative integer to convert, and another integer representing the base. 

It should return a string containing the digits of the resulting number, without leading zeros except for the number   '''0'''   itself. 

For the digits beyond 9, one should use the lowercase English alphabet, where the digit   '''a''' = 9+1,   '''b''' = a+1,   etc. 

For example:   the decimal number   '''26'''   expressed in base   '''16'''   would be   '''1a'''.

Write a second function which is passed a string and an integer base, and it returns an integer representing that string interpreted in that base.

The programs may be limited by the word size or other such constraint of a given language. There is no need to do error checking for negatives, bases less than 2, or inappropriate digits.





## ACL2


```Lisp
(defun digit-value (chr)
   (cond ((and (char>= chr #\0)
               (char<= chr #\9))
          (- (char-code chr) (char-code #\0)))
         ((and (char>= chr #\A)
               (char<= chr #\Z))
          (+ (- (char-code chr) (char-code #\A)) 10))
         ((and (char>= chr #\a)
               (char<= chr #\z))
          (+ (- (char-code chr) (char-code #\a)) 10))))

(defun value-digit (n)
   (if (< n 10)
       (code-char (+ n (char-code #\0)))
       (code-char (+ (- n 10) (char-code #\A)))))

(defun num-from-cs (cs base)
   (if (endp cs)
       0
       (+ (digit-value (first cs))
          (* base (num-from-cs (rest cs) base)))))

(defun parse-num (str base)
   (num-from-cs (reverse (coerce str 'list)) base))

(include-book "arithmetic-3/top" :dir :system)

(defun num-to-cs (num base)
   (if (or (zp num) (zp base) (= base 1))
       nil
       (cons (value-digit (mod num base))
             (num-to-cs (floor num base) base))))

(defun show-num (num base)
   (coerce (reverse (num-to-cs num base)) 'string))
```



## Ada

Ada provides built-in capability to convert between all bases from 2 through 16. This task requires conversion for bases up to 36. The following program demonstrates such a conversion using an iterative solution.

```ada
with Ada.Text_Io; use Ada.Text_Io;
with Ada.Strings.Fixed;
With Ada.Strings.Unbounded;

procedure Number_Base_Conversion is
   Max_Base : constant := 36;
   subtype Base_Type is Integer range 2..Max_Base;
   Num_Digits : constant String := "0123456789abcdefghijklmnopqrstuvwxyz";
   Invalid_Digit : exception;
   
   function To_Decimal(Value : String; Base : Base_Type) return Integer is
      use Ada.Strings.Fixed;
      Result : Integer := 0;
      Decimal_Value : Integer;
      Radix_Offset : Natural := 0;
   begin
      for I in reverse Value'range loop
         Decimal_Value := Index(Num_Digits, Value(I..I)) - 1;
         if Decimal_Value < 0 then
            raise Invalid_Digit;
         end if; 
         Result := Result + (Base**Radix_Offset * Decimal_Value);
         Radix_Offset := Radix_Offset + 1;
      end loop;
      return Result;
   end To_Decimal;
   
   function To_Base(Value : Natural; Base : Base_Type) return String is
      use Ada.Strings.Unbounded;
      Result : Unbounded_String := Null_Unbounded_String;
      Temp : Natural := Value;
      Base_Digit : String(1..1);
   begin
      if Temp = 0 then
         return "0";
      end if;
      while Temp > 0 loop
         Base_Digit(1) := Num_Digits((Temp mod Base) + 1);
         if Result = Null_Unbounded_String then
            Append(Result, Base_Digit);
         else
            Insert(Source => Result,
               Before => 1,
               New_Item => Base_Digit);
         end if;
         Temp := Temp / Base;
      end loop;
      return To_String(Result);
   end To_Base;
   
begin
   Put_Line("26 converted to base 16 is " & To_Base(26, 16));
   Put_line("1a (base 16) is decimal" & Integer'image(To_Decimal("1a", 16)));
end Number_Base_Conversion;
```



## Aime


```aime
o_text(bfxa(0, 0, 16, 1000000));
o_byte('\n');
o_text(bfxa(0, 0, 5, 1000000));
o_byte('\n');
o_text(bfxa(0, 0, 2, 1000000));
o_byte('\n');

o_integer(alpha("f4240", 16));
o_byte('\n');
o_integer(alpha("224000000", 5));
o_byte('\n');
o_integer(alpha("11110100001001000000", 2));
o_byte('\n');
```



## ALGOL 68


### Built in or standard distribution routines

{{works with|ALGOL 68|Standard - no extensions to language used}}
{{works with|ALGOL 68G|Any - tested with release mk15-0.8b.fc9.i386}}
The ''formatted transput'' in '''ALGOL 68''' uses the '''format''' type ('''mode''').  
This '''format''' type has many similarities with modern ''regular expressions''
and can be used to convert '''string''' patterns to and from many of the 
built in types ('''mode'''s) in ALGOL 68.  Here is an example converting
a numbers base.


```algol68
INT base = 16, from dec = 26;
BITS to bits;

FORMAT hex repr = $n(base)r2d$;

FILE f; STRING str; 

associate(f, str);
putf(f, (hex repr, BIN from dec));
print(("Hex: ",str, new line));

reset(f);
getf(f, (hex repr, to bits));
print(("Int: ",ABS to bits, new line))
```

Output:

```txt

Hex: 1a
Int:         +26

```

Note that the only conversions "officially" available are for the bases 2r, 4r, 8r 
and 16r.  But [[ALGOL 68G]] allows formatting for all numbers in the range 2r to 16r.


### Implementation example

Handles signed and unsigned numbers from all bases.

{{trans|python}}

{{works with|ALGOL 68|Standard - no extensions to language used}}
{{works with|ALGOL 68G|Any - tested with release mk15-0.8b.fc9.i386}}
{{works with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release 1.8.8d.fc9.i386}}

```algol68
STRING numeric alpha = "0123456789abcdefghijklmnopqrstuvwxyz";

PROC raise value error = ([]STRING args)VOID: (
  put(stand error, "Value error");
  STRING sep := ": ";
  FOR index TO UPB args - 1 DO put(stand error, (sep, args[index])); sep:=", " OD;
  new line(stand error);
  stop
);

PROC base n = (INT num, base)STRING: (
  PROC base n = (INT num, base)STRING:
    ( num = 0 | "" |  base n(num OVER base, base) + numeric alpha[@0][num MOD base]);
  ( num = 0 | "0" |: num > 0 | base n(num, base) | "-" + base n(-num, base) )
);

PROC unsigned int = (STRING repr, INT base)INT:
  IF UPB repr < LWB repr THEN 0 ELSE
    INT pos; 
    IF NOT char in string(repr[UPB repr], pos, numeric alpha) THEN 
      raise value error("CHAR """+repr[UPB repr]+""" not valid") 
    FI;
    unsigned int(repr[:UPB repr-1], base) * base + pos - 1
  FI
;

PROC int = (STRING repr, INT base)INT: 
  ( repr[LWB repr]="-" | -unsigned int(repr[LWB repr + 1:], base) | unsigned int(repr, base) );

[]INT test = (-256, -255, -26, -25, 0, 25, 26, 255, 256);
FOR index TO UPB test DO
  INT k = test[index];
  STRING s = base n(k,16); # returns the string 1a #
  INT i = int(s,16);  # returns the integer 26 #
  print((k," => ", s, " => ", i, new line))
OD
```

Output:

```txt

       -256 => -100 =>        -256
       -255 => -ff =>        -255
        -26 => -1a =>         -26
        -25 => -19 =>         -25
         +0 => 0 =>          +0
        +25 => 19 =>         +25
        +26 => 1a =>         +26
       +255 => ff =>        +255
       +256 => 100 =>        +256

```


### Other libraries or implementation specific extensions

As of February 2009 no open source libraries to do this task have been located.


## ALGOL W


```algolw
begin
    % returns with numberInBase set to the number n converted to a string in %
    % the specified base. Number must be non-negative and base must be in    %
    % range 2 to 36                                                          %
    procedure convertToBase( integer    value  n
                           ; integer    value  base
                           ; string(32) result numberInBase
                           ) ;
    begin
        string(36) baseDigits;
        integer    val, strPos;

        assert( n >= 0 and base >= 2 and base <= 36 );

        baseDigits    := "0123456789abcdefghijklmnopqrstuvwxyz";
        numberInBase  := " ";
        val           := n;
        strPos        := 31;
        while
            begin
                % a(b//c) is the substring of a starting at b with length c. %
                % The first character is at position 0. The length must be   %
                % an integer literal so it is known at compile time.         %
                numberInBase( strPos // 1 ) := baseDigits( val rem base // 1 );
                val    := val div base;
                strPos := strPos - 1;
                val > 0
            end
        do begin end
    end convertToBase ;

    % returns the string numberInBase converted to an integer assuming       %
    % numberInBase ia a string in the specified base                         %
    % base must be in range 2 to 36, invalid digits will cause the program   %
    % to crash, spaces are ignored                                           %
    integer procedure convertFromBase( string(32) value numberInBase
                                     ; integer    value base
                                     ) ;
    begin
        string(36) baseDigits;
        integer    val, cPos;

        assert( base >= 2 and base <= 36 );

        baseDigits    := "0123456789abcdefghijklmnopqrstuvwxyz";
        val           := 0;
        for strPos := 0 until 31 do begin
            string(1) c;
            c := numberInBase( strPos // 1 );
            if c not = " " then begin
                cPos := 0;
                while baseDigits( cPos // 1 ) not = c do cPos := cPos + 1;
                val  := ( val * base ) + cPos;
            end
        end;
        val
    end convertFromBase ;

    % test the procedures                                                    %
    string(32) baseNumber;
    i_w := 3; % set integer output width                                     %
    for i := 2 until 36 do begin
        convertToBase( 35, i, baseNumber );
        write( 35, i, baseNumber, " ", convertFromBase( baseNumber, i ) );
    end
end.
```




## AppleScript

{{Trans|JavaScript}}
For more flexibility with digit variants (upper and lower case hex, digits in other languages/scripts etc) we can define '''toBase'''(intBase, n) in terms of a more general '''inBaseDigits'''(strDigits, n) which derives the base from the number of digits to be used:

```AppleScript
-- toBase :: Int -> Int -> String
on toBase(intBase, n)
    if (intBase < 36) and (intBase > 0) then
        inBaseDigits(items 1 thru intBase of "0123456789abcdefghijklmnopqrstuvwxyz", n)
    else
        "not defined for base " & (n as string)
    end if
end toBase

-- inBaseDigits :: String -> Int -> [String]
on inBaseDigits(strDigits, n)
    set intBase to length of strDigits
    
    script nextDigit
        on |λ|(residue)
            set {divided, remainder} to quotRem(residue, intBase)
            if divided > 0 then
                {just:(item (remainder + 1) of strDigits), new:divided, nothing:false}
            else
                {nothing:true}
            end if
            
        end |λ|
    end script
    
    reverse of unfoldr(nextDigit, n) as string
end inBaseDigits

-- OTHER FUNCTIONS DERIVABLE FROM inBaseDigits -------------------------------

-- inUpperHex :: Int -> String
on inUpperHex(n)
    inBaseDigits("0123456789ABCDEF", n)
end inUpperHex

-- inDevanagariDecimal :: Int -> String
on inDevanagariDecimal(n)
    inBaseDigits("०१२३४५६७८९", n)
end inDevanagariDecimal

-- TEST ----------------------------------------------------------------------
on run
    script
        on |λ|(x)
            {{binary:toBase(2, x), octal:toBase(8, x), hex:toBase(16, x)}, ¬
                {upperHex:inUpperHex(x), dgDecimal:inDevanagariDecimal(x)}}
        end |λ|
    end script
    
    map(result, [255, 240])
end run


-- GENERIC FUNCTIONS ---------------------------------------------------------

-- unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
on unfoldr(f, v)
    set lst to {}
    set recM to {nothing:false, new:v}
    tell mReturn(f)
        repeat while (not (nothing of recM))
            set recM to |λ|(new of recM)
            if not nothing of recM then set end of lst to just of recM
        end repeat
    end tell
    lst
end unfoldr

--  quotRem :: Integral a => a -> a -> (a, a)
on quotRem(m, n)
    {m div n, m mod n}
end quotRem

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
```

{{Out}}

```AppleScript
{{{binary:"11111111", octal:"377", hex:"ff"}, {upperHex:"FF", dgDecimal:"२५५"}}, 
{{binary:"11110000", octal:"360", hex:"f0"}, {upperHex:"F0", dgDecimal:"२४०"}}}
```



## AutoHotkey


```AutoHotkey
MsgBox % number2base(200, 16) ; 12
MsgBox % parse(200, 16)  ; 512

number2base(number, base)
{
  While, base < digit := floor(number / base)
  {
    result := mod(number, base) . result
    number := digit
  }
  result := digit . result
  Return result
}

parse(number, base)
{
  result = 0
  pos := StrLen(number) - 1
  Loop, Parse, number 
  {
    result := ((base ** pos) * A_LoopField) + result
    base -= 1
  }
  Return result
}
```

alternate implementation contributed by Laszlo on the ahk [http://www.autohotkey.com/forum/post-276241.html#276241 forum]

```AutoHotkey
MsgBox % ToBase(29,3)
MsgBox % ToBase(255,16)

MsgBox % FromBase("100",8)
MsgBox % FromBase("ff",16)

ToBase(n,b) { ; n >= 0, 1 < b <= 36
   Return (n < b ? "" : ToBase(n//b,b)) . ((d:=mod(n,b)) < 10 ? d : Chr(d+87))
}

FromBase(s,b) { ; convert base b number s=strings of 0..9,a..z, to AHK number
   Return (L:=StrLen(s))=0 ? "":(L>1 ? FromBase(SubStr(s,1,L-1),b)*b:0) + ((c:=Asc(SubStr(s,0)))>57 ? c-87:c-48)
}
```



## AWK


```awk
function strtol(str, base)
{
  symbols = "0123456789abcdefghijklmnopqrstuvwxyz"
  res = 0
  str = tolower(str)
  for(i=1; i < length(str); i++) {
    res += index(symbols, substr(str, i, 1)) - 1
    res *= base
  }
  res += index(symbols, substr(str, length(str), 1)) - 1
  return res
}

function ltostr(num, base)
{
  symbols = "0123456789abcdefghijklmnopqrstuvwxyz"
  res = ""
  do {
    res = substr(symbols, num%base + 1, 1) res
    num = int(num/base)
  } while ( num != 0 )
  return res
}

BEGIN {
  print strtol("7b", 16)
  print ltostr(123, 16)
}
```



## BBC BASIC


```bbcbasic
      PRINT "  0 (decimal) -> " FNtobase(0, 16) " (base 16)"
      PRINT " 26 (decimal) -> " FNtobase(26, 16) " (base 16)"
      PRINT "383 (decimal) -> " FNtobase(383, 16) " (base 16)"
      PRINT " 26 (decimal) -> " FNtobase(26, 2) " (base 2)"
      PRINT "383 (decimal) -> " FNtobase(383, 2) " (base 2)"
      PRINT " 1a (base 16) -> " ;FNfrombase("1a", 16) " (decimal)"
      PRINT " 1A (base 16) -> " ;FNfrombase("1A", 16) " (decimal)"
      PRINT "17f (base 16) -> " ;FNfrombase("17f", 16) " (decimal)"
      PRINT "101111111 (base 2) -> " ;FNfrombase("101111111", 2) " (decimal)"
      END
      
      DEF FNtobase(N%, B%)
      LOCAL D%,A$
      REPEAT
        D% = N% MOD B%
        N% DIV= B%
        A$ = CHR$(48 + D% - 39*(D%>9)) + A$
      UNTIL N% = FALSE
      =A$
      
      DEF FNfrombase(A$, B%)
      LOCAL N%
      REPEAT
        N% *= B%
        N% += ASC(A$) - 48 + 7*(ASCA$>64) + 32*(ASCA$>96)
        A$ = MID$(A$,2)
      UNTIL A$ = ""
      = N%
```

'''Output:'''

```txt

  0 (decimal) -> 0 (base 16)
 26 (decimal) -> 1a (base 16)
383 (decimal) -> 17f (base 16)
 26 (decimal) -> 11010 (base 2)
383 (decimal) -> 101111111 (base 2)
 1a (base 16) -> 26 (decimal)
 1A (base 16) -> 26 (decimal)
17f (base 16) -> 383 (decimal)
101111111 (base 2) -> 383 (decimal)

```



## Bracmat


```bracmat
  ( display
  =   
    .   !arg:<10
      | !arg:<36&chr$(asc$a+!arg+-10)
      | "Base too big"
  )
& ( base
  =   n b
    .     !arg:(?n.?b)
        & !n:<!b
        & ( !n:~<0&display$!n
          | NOTSUPPORTED
          )
      | base$(div$(!n.!b).!b) display$(mod$(!n.!b))
  )
&   whl
  ' (   put
      $ "Enter non-negative integer in decimal notation (or something else to stop):"
    & get':~/#>-1:?n
    & put$"Enter base (less than 37):"
    & get$:~/#>1:~>36:?b
    & out$(!n " in base " !b " is " str$(base$(!n.!b)))
    );
```



## C


```c>#include <stdlib.h

#include <string.h>
#include <stdio.h>
#include <stdint.h>

char *to_base(int64_t num, int base)
{
	char *tbl = "0123456789abcdefghijklmnopqrstuvwxyz";
	char buf[66] = {'\0'};
	char *out;
	uint64_t n;
	int i, len = 0, neg = 0;
	if (base > 36) {
		fprintf(stderr, "base %d too large\n", base);
		return 0;
	}

	/* safe against most negative integer */ 
	n = ((neg = num < 0)) ? (~num) + 1 : num;

	do { buf[len++] = tbl[n % base]; } while(n /= base);

	out = malloc(len + neg + 1);
	for (i = neg; len > 0; i++) out[i] = buf[--len];
	if (neg) out[0] = '-';

	return out;
}

long from_base(const char *num_str, int base)
{
	char *endptr;
	/* there is also strtoul() for parsing into an unsigned long */
	/* in C99, there is also strtoll() and strtoull() for parsing into long long and
	 * unsigned long long, respectively */
	int result = strtol(num_str, &endptr, base);
	return result;
}

int main()
{
	int64_t x;
	x = ~(1LL << 63) + 1;
	printf("%lld in base 2: %s\n", x, to_base(x, 2));
	x = 383;
	printf("%lld in base 16: %s\n", x, to_base(x, 16));
	return 0;
}
```
output

```txt
-9223372036854775808 in base 2: -1000000000000000000000000000000000000000000000000000000000000000
383 in base 16: 17f
```



## C++


```cpp>#include <string

#include <cstdlib>
#include <algorithm>
#include <cassert>

std::string const digits = "0123456789abcdefghijklmnopqrstuvwxyz";

std::string to_base(unsigned long num, int base)
{
  if (num == 0)
    return "0";
  
  std::string result;
  while (num > 0) {
    std::ldiv_t temp = std::div(num, (long)base);
    result += digits[temp.rem];
    num = temp.quot;
  }
  std::reverse(result.begin(), result.end());
  return result;
}

unsigned long from_base(std::string const& num_str, int base)
{
  unsigned long result = 0;
  for (std::string::size_type pos = 0; pos < num_str.length(); ++pos)
    result = result * base + digits.find(num_str[pos]);
  return result;
}
```


=={{header|C sharp|C#}}==

```CSharp

public static class BaseConverter {

    /// <summary>
    /// Converts a string to a number
    /// </summary>
    /// <returns>The number.</returns>
    /// <param name="s">The string to convert.</param>
    /// <param name="b">The base number (between 2 and 36).</param>
    public static long stringToLong(string s, int b) {

        if ( b < 2 || b > 36 )
            throw new ArgumentException("Base must be between 2 and 36", "b");

        checked {

            int slen = s.Length;
            long result = 0;
            bool isNegative = false;

            for ( int i = 0; i < slen; i++ ) {

                char c = s[i];
                int num;

                if ( c == '-' ) {
                    // Negative sign
                    if ( i != 0 )
                        throw new ArgumentException("A negative sign is allowed only as the first character of the string.", "s");

                    isNegative = true;
                    continue;
                }

                if ( c > 0x2F && c < 0x3A )
                    // Numeric character (subtract from 0x30 ('0') to get numerical value)
                    num = c - 0x30;
                else if ( c > 0x40 && c < 0x5B )
                    // Uppercase letter
                    // Subtract from 0x41 ('A'), then add 10
                    num = c - 0x37;  // 0x37 = 0x41 - 10
                else if ( c > 0x60 && c < 0x7B )
                    // Lowercase letter
                    // Subtract from 0x61 ('a'), then add 10
                    num = c - 0x57;  // 0x57 = 0x61 - 10
                else
                    throw new ArgumentException("The string contains an invalid character '" + c + "'", "s");

                // Check that the digit is allowed by the base.

                if ( num >= b )
                    throw new ArgumentException("The string contains a character '" + c + "' which is not allowed in base " + b, "s");

                // Multiply the result by the base, then add the next digit

                result *= b;
                result += num;

            }

            if ( isNegative )
                result = -result;

            return result;

        }

    }

    /// <summary>
    /// Converts a number to a string.
    /// </summary>
    /// <returns>The string.</returns>
    /// <param name="n">The number to convert.</param>
    /// <param name="b">The base number (between 2 and 36).</param>
    public static string longToString(long n, int b) {
        
        // This uses StringBuilder, so it only works with .NET 4.0 or higher. For earlier versions, the StringBuilder
        // can be replaced with simple string concatenation.
        
        if ( b < 2 || b > 36 )
            throw new ArgumentException("Base must be between 2 and 36", "b");

        // If the base is 10, call ToString() directly, which returns a base-10 string.

        if ( b == 10 )
            return n.ToString();

        checked {
            long longBase = b;
            
            StringBuilder sb = new StringBuilder();
            
            if ( n < 0 ) {
                // Negative numbers
                n = -n;
                sb.Append('-');
            }
            
            long div = 1;
            while ( n / div >= b )
                // Continue multiplying the dividend by the base until it reaches the greatest power of
                // the base which is less than or equal to the number.
                div *= b;
            
            while ( true ) {
                byte digit = (byte) (n / div);
            
                if ( digit < 10 )
                    // Numeric character (0x30 = '0')
                    sb.Append((char) (digit + 0x30));
                else
                    // Alphabetic character (for digits > 10) (0x61 = 'a')
                    sb.Append((char) (digit + 0x57));  // 0x61 - 10
            
                if ( div == 1 )
                    // Stop when the dividend reaches 1
                    break;
            
                n %= div;
                div /= b;
            }
            
            return sb.ToString();
        }

    }

}

```


=={{header|Caché ObjectScript}}==


```cos
Class Utils.Number [ Abstract ]
{

ClassMethod ConvertBase10ToN(pNum As %Integer = "", pBase As %Integer = "", pBaseStr As %String = "", pPos As %Integer = 0) As %String
{
	If pNum=0 Quit ""
	Set str=..ConvertBase10ToN(pNum\pBase, pBase, pBaseStr, pPos+1)
	Quit str_$Extract(pBaseStr, pNum#pBase+1)
}

ClassMethod ConvertBaseNTo10(pStr As %String = "", pBase As %Integer = "", pBaseStr As %String = "", pPos As %Integer = 0) As %Integer
{
	If pStr="" Quit 0
	Set num=..ConvertBaseNTo10($Extract(pStr, 1, *-1), pBase, pBaseStr, pPos+1)
	Set dec=$Find(pBaseStr, $Extract(pStr, *))-2
	Quit num+(dec*(pBase**pPos))
}

ClassMethod ConvertBase(pStr As %String = "", pFrom As %Integer = 10, pTo As %Integer = 10, pBaseStr As %String = "", pLen As %Integer = 0) As %String
{
	// some initialisation
	If pBaseStr="" Set pBaseStr="0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
	
	// check input values
	If pFrom=10 Set pStr=$Number(pStr, "i", 0) If pStr="" Quit ""
	Set pFrom=$Number(pFrom, "i", 2, 94) If pFrom="" Quit ""
	Set pTo=$Number(pTo, "i", 2, 94) If pTo="" Quit ""
	Set pLen=$Number(pLen, "i", 0, 32) If pLen="" Quit ""
	
	// does base number exceed base string?
	If pFrom>$Length(pBaseStr) Quit ""
	If pTo>$Length(pBaseStr) Quit ""
	
	// allow for upper/lowercase values
	If pTo=10 {
		If $Match(pStr, "^[0-9a-z]+$"), $Match($Extract(pBaseStr, 1, pFrom), "^[0-9A-Z]+$") {
			Set pStr=$ZConvert(pStr, "U")
		}
		If $Match(pStr, "^[0-9A-Z]+$"), $Match($Extract(pBaseStr, 1, pFrom), "^[0-9a-z]+$") {
			Set pStr=$ZConvert(pStr, "L")
		}
	}
	
	// do the conversion
	If pFrom=pTo {
		Set pStr=pStr
	} ElseIf pFrom=10 {
		Set pStr=..ConvertBase10ToN($Select(pStr=0: "", 1: pStr), pTo, pBaseStr)
	} ElseIf pTo=10 {
		Set pStr=..ConvertBaseNTo10(pStr, pFrom, pBaseStr)
	} Else {
		Set pStr=..ConvertBase10ToN(..ConvertBaseNTo10(pStr, pFrom, pBaseStr), pTo, pBaseStr)
	}
	
	// return value
	If pLen=0 Quit pStr
	If pTo'=10 Quit ..PadStr(pStr, pLen, $Extract(pBaseStr))
	Quit ..PadStr(pStr, pLen)
}

ClassMethod PadStr(pStr As %String, pLen As %Integer, pZero As %String = 0) As %String [ Private ]
{
	If $Length(pStr)>pLen Quit pStr
	Quit $Translate($Justify(pStr, pLen), " ", pZero)
}

}
```

{{out|Examples}}

```txt

USER>Write ##class(Utils.Number).ConvertBase(1010101111001101, 2, 16)
ABCD

USER>Write $ZHex(26)
1A
USER>Write $ZHex("1A")
26

USER>Write ##class(Utils.Number).ConvertBase(26, 10, 16)
1A
USER>Write ##class(Utils.Number).ConvertBase("1A", 16, 10)
26

USER>Write ##class(Utils.Number).ConvertBase(6234900123456700, 10, 42, "!$%-0123456789@ABCDEFGHIJKLMNOPQRSTUVWXYZ_")
A9XUCDBHK6
USER>Write ##class(Utils.Number).ConvertBase("A9XUCDBHK6", 42, 10, "!$%-0123456789@ABCDEFGHIJKLMNOPQRSTUVWXYZ_")
6234900123456700
```



## Common Lisp


```lisp
(parse-integer "1a" :radix 16) ; returns multiple values: 26, 2
(write-to-string 26 :base 16) ; also "1A"
```


Alternative implementation using FORMAT's ~R directive and #nR reader macro

```lisp
(defun decimal-to-base-n (number &key (base 16))
  (format nil (format nil "~~~dr" base) number))

(defun base-n-to-decimal (number &key (base 16))
  (read-from-string (format nil "#~dr~d" base number)))
```


Yet another approach uses FORMAT's ~R in conjunction with ~V for passing arguments to directives (this assumes input as string)

```lisp
(defun change-base (number input-base output-base)
  (format nil "~vr" output-base (parse-integer number :radix input-base)))
```



## D


### Using Standard Functions


```d
import std.stdio, std.conv, std.string, std.ascii;

void main() {
    "1abcd".to!int(16).writeln;

    writeln(60_272_032_366.to!string(36, LetterCase.lower), ' ',
            591_458.to!string(36, LetterCase.lower));
}
```

{{out}}

```txt
109517
rosetta code
```



### One Implementation


```d
import std.stdio, std.array, std.ascii;

immutable string mDigits = digits ~ lowercase;

ulong atoiRadix(in string str, in uint radix=10, int* consumed=null)
nothrow {
    static int dtoi(in char dc, in uint radix) nothrow {
        static int[immutable char] digit;
        immutable char d = dc.toLower;
        if (digit.length == 0) // Not init yet.
            foreach (i, c; mDigits)
                digit[c] = i;
        if (radix > 1 && radix <= digit.length &&
            d in digit && digit[d] < radix)
            return digit[d];
        return int.min; // A negative for error.
    }

    ulong result;
    int sp;
    for (; sp < str.length; sp++) {
        immutable int d = dtoi(str[sp], radix);
        if (d >= 0) // Valid digit char.
            result = radix * result + d;
        else
            break;
    }
    if (sp != str.length) // Some char in str not converted.
        sp = -sp;
    if (consumed !is null) // Signal error if not positive.
        *consumed = sp;
    return result;
}

string itoaRadix(ulong num, in uint radix=10) pure nothrow
in {
    assert(radix > 1 && radix <= mDigits.length);
} body {
    string result;
    while (num > 0) {
        immutable uint d = num % radix;
        result = mDigits[d] ~ result;
        num = (num - d) / radix;
    }
    return result.empty ? "0" : result;
}

void main() {
    immutable string numStr = "1ABcdxyz???";

    int ate;
    writef("'%s' (base %d) = %d", numStr, 16,
           atoiRadix(numStr, 16, &ate));

    if (ate <= 0)
        writefln("\tConverted only: '%s'", numStr[0 .. -ate]);
    else
        writeln();

    writeln(itoaRadix(60_272_032_366, 36), " ",
            itoaRadix(591_458, 36));
}
```

{{out}}

```txt
'1ABcdxyz???' (base 16) = 109517    Converted only: '1ABcd'
rosetta code
```



### Alternative Implementation

{{trans|Haskell}}

```d
import std.stdio, std.algorithm, std.ascii, std.array, std.string;

alias Digits = ubyte[];

Digits toBase(ulong number, in ubyte base) pure nothrow @safe {
    Digits result;
    while (number) {
        result = number % base ~ result;
        number /= base;
    }
    return result;
}

enum fromBase = (in Digits digits, in ubyte base) pure nothrow @safe @nogc =>
    reduce!((n, k) => n * base + k)(0UL, digits);

immutable myDigits = digits ~ lowercase;

enum fromDigits = (in Digits digits) pure nothrow /*@safe*/ =>
    digits.map!(d => myDigits[d]).array;

enum convert = (in dchar d) pure nothrow @safe @nogc =>
    cast(ubyte)(d.isDigit ? d - '0' : std.ascii.toLower(d) - 'a' + 10);

enum toDigits = (in string number) pure nothrow @safe =>
    number.representation.map!convert.array;

void main() {
    "1ABcd".toDigits.fromBase(16).writeln;
}
```

{{out}}
 109517


## E


```e
def stringToInteger := __makeInt
def integerToString(i :int, base :int) {
  return i.toString(base)
}
```



```e
? stringToInteger("200", 16)
# value: 512

? integerToString(200, 16)
# value: "c8"
```



## Elixir


```elixir
iex(1)> String.to_integer("ffff", 16)
65535
iex(2)> Integer.to_string(255, 2)
"11111111"
iex(3)> String.to_integer("NonDecimalRadices", 36)
188498506820338115928429652
```



## Erlang


{{out}}

```txt

12> erlang:list_to_integer("ffff", 17).
78300
13> erlang:integer_to_list(63, 3).
"2100"

```



## Euphoria


```euphoria
function to_base(integer i, integer base)
    integer rem
    sequence s
    s = ""
    while i > 0 do
        rem = remainder(i,base)
        if rem < 10 then
            s = prepend(s, '0'+rem)
        else
            s = prepend(s, 'a'-10+rem)
        end if
        i = floor(i/base)
    end while
    
    if length(s) = 0 then
        s = "0"
    end if
    
    return s
end function

function from_base(sequence s, integer base)
    integer i,d
    i = 0
    for n = 1 to length(s) do
        i *= base
        if s[n] >= '0' and s[n] <= '9' then
            d = s[n]-'0'
        elsif s[n] >= 'a' then
            d = s[n]-'a'+10
        end if
        i += d
    end for
    return i
end function
```



## Factor


```factor
USE: math.parser

12345 16 >base .
"3039" 16 base> .
```



## Forth

Forth has a global user variable, BASE, which determines the radix used for parsing, interpretation, and printing of integers. This can handle bases from 2-36, but there are two words to switch to the most popular bases, DECIMAL and HEX.

```forth
42 dup
2 base !
.   \ 101010
hex
.   \ 2A
decimal
```


Many variants of Forth support literals in some bases, such as hex, using a prefix

```forth
$ff .   \ 255
```



## Fortran

{{Works with|Fortran|90 and later}}

```fortran
MODULE Conversion
  IMPLICIT NONE
  CHARACTER(36) :: alphanum = "0123456789abcdefghijklmnopqrstuvwxyz"
 
  CONTAINS

  FUNCTION ToDecimal(base, instr)
    INTEGER :: ToDecimal
    INTEGER :: length, i, n, base
    CHARACTER(*) :: instr

    ToDecimal = 0
    length = LEN(instr)
    DO i = 1, length
      n = INDEX(alphanum, instr(i:i)) - 1
      n = n * base**(length-i)
      Todecimal = ToDecimal + n
    END DO
  END FUNCTION ToDecimal

  FUNCTION ToBase(base, number)
    CHARACTER(31) :: ToBase
    INTEGER :: base, number, i, rem

    ToBase = "                               "
    DO i = 31, 1, -1
      IF(number < base) THEN
        ToBase(i:i) = alphanum(number+1:number+1)
        EXIT
      END IF
      rem = MOD(number, base)
      ToBase(i:i) = alphanum(rem+1:rem+1)
      number = number / base
    END DO
    ToBase = ADJUSTL(ToBase)
  END FUNCTION ToBase

END MODULE Conversion

PROGRAM Base_Convert
  USE Conversion

  WRITE (*,*) ToDecimal(16, "1a")
  WRITE (*,*) ToBase(16, 26)     

END PROGRAM
```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Function min(x As Integer, y As Integer) As Integer
  Return IIf(x < y, x, y)
End Function

Function convertToBase (n As UInteger, b As UInteger) As String  
  If n < 2 OrElse b < 2 OrElse b = 10 OrElse b > 36 Then Return Str(n)
  Dim result As String = "" 
  Dim digit As Integer
  While n > 0
    digit = n Mod b
    If digit < 10 Then
      result = digit & result
    Else
      result = Chr(digit + 87) + result
    End If
     n \= b
  Wend
  Return result
End Function

Function convertToDecimal (s As Const String, b As UInteger) As UInteger
  If b < 2 OrElse b > 36 Then Return 0
  Dim t As String = LCase(s)
  Dim result As UInteger = 0
  Dim digit As Integer
  Dim multiplier As Integer = 1
  For i As Integer = Len(t) - 1 To 0 Step - 1
     digit = -1
     If t[i] >= 48 AndAlso t[i] <= min(57, 47 + b) Then
       digit = t[i] - 48
     ElseIf b > 10 AndAlso t[i] >= 97 AndAlso t[i] <= min(122, 87 + b) Then
       digit = t[i] - 87
     End If
     If digit = -1 Then Return 0 '' invalid digit present
     If digit > 0 Then result += multiplier * digit
     multiplier *= b
  Next
  Return result
End Function

Dim s As String

For b As UInteger = 2 To 36
  Print "36 base ";
  Print Using "##"; b; 
  s = ConvertToBase(36, b)
  Print " = "; s; Tab(21); " -> base ";
  Print Using "##"; b; 
  Print " = "; convertToDecimal(s, b)
Next

Print
Print "Press any key to quit"
Sleep
```


{{out}}

```txt

36 base  2 = 100100  -> base  2 = 36
36 base  3 = 1100    -> base  3 = 36
36 base  4 = 210     -> base  4 = 36
36 base  5 = 121     -> base  5 = 36
36 base  6 = 100     -> base  6 = 36
36 base  7 = 51      -> base  7 = 36
36 base  8 = 44      -> base  8 = 36
36 base  9 = 40      -> base  9 = 36
36 base 10 = 36      -> base 10 = 36
36 base 11 = 33      -> base 11 = 36
36 base 12 = 30      -> base 12 = 36
36 base 13 = 2a      -> base 13 = 36
36 base 14 = 28      -> base 14 = 36
36 base 15 = 26      -> base 15 = 36
36 base 16 = 24      -> base 16 = 36
36 base 17 = 22      -> base 17 = 36
36 base 18 = 20      -> base 18 = 36
36 base 19 = 1h      -> base 19 = 36
36 base 20 = 1g      -> base 20 = 36
36 base 21 = 1f      -> base 21 = 36
36 base 22 = 1e      -> base 22 = 36
36 base 23 = 1d      -> base 23 = 36
36 base 24 = 1c      -> base 24 = 36
36 base 25 = 1b      -> base 25 = 36
36 base 26 = 1a      -> base 26 = 36
36 base 27 = 19      -> base 27 = 36
36 base 28 = 18      -> base 28 = 36
36 base 29 = 17      -> base 29 = 36
36 base 30 = 16      -> base 30 = 36
36 base 31 = 15      -> base 31 = 36
36 base 32 = 14      -> base 32 = 36
36 base 33 = 13      -> base 33 = 36
36 base 34 = 12      -> base 34 = 36
36 base 35 = 11      -> base 35 = 36
36 base 36 = 10      -> base 36 = 36

```



## FunL

Converting from integer to string:

```funl
$stdout = int( '1a', 16 )
```


{{out}}


```txt

26

```


Converting from string to integer:

```funl
$stdout = str( 26, 16 )
```


{{out}}


```txt

1a

```



## Go

The standard <code>strconv</code> package functions <code>ParseInt</code>, <code>ParseUint</code>, <code>FormatInt</code>, <code>FormatUint</code>,
and the standard <code>math/big</code> package method <code>SetString</code>,
all accept a base argument <code>2 ≤ base ≤ 36</code>.

Note, there is no equivalent formatting function provided for a <code>big.Int</code>, only the standard bases are available via <code>fmt</code> integer formatting verbs
(binary <code>%b</code>, octal <code>%o</code>, decimal <code>%d</code>, and hexidecimal <code>%x</code> or <code>%X</code>).

```go
package main

import (
    "fmt"
    "math/big"
    "strconv"
)

func main () {
    s := strconv.FormatInt(26, 16) // returns the string "1a"
    fmt.Println(s)

    i, err := strconv.ParseInt("1a", 16, 64) // returns the integer (int64) 26
    if err == nil {
        fmt.Println(i)
    }
    b, ok := new(big.Int).SetString("1a", 16) // returns the big integer 26
    if ok {
        fmt.Println(b)
    }
}
```



## Groovy

Solution:

```groovy
def radixParse = { s, radix -> Integer.parseInt(s, radix) }
def radixFormat = { i, radix -> Integer.toString(i, radix) }
```


Test Program:

```groovy
def numString = '101'
(2..Character.MAX_RADIX).each { radix ->
    def value = radixParse(numString, radix)
    assert value == radix**2 + 1
    printf ("         %3s (%2d) == %4d (10)\n", numString, radix, value)
    
    def valM2str = radixFormat(value - 2, radix)
    def biggestDigit = radixFormat(radix - 1, radix)
    assert valM2str == biggestDigit + biggestDigit
    printf ("%3s (%2d) - 2 (10) == %4s (%2d)\n", numString, radix, valM2str, radix)
}
```


Output:
<pre style="height:30ex;overflow:scroll;">         101 ( 2) ==    5 (10)
101 ( 2) - 2 (10) ==   11 ( 2)
         101 ( 3) ==   10 (10)
101 ( 3) - 2 (10) ==   22 ( 3)
         101 ( 4) ==   17 (10)
101 ( 4) - 2 (10) ==   33 ( 4)
         101 ( 5) ==   26 (10)
101 ( 5) - 2 (10) ==   44 ( 5)
         101 ( 6) ==   37 (10)
101 ( 6) - 2 (10) ==   55 ( 6)
         101 ( 7) ==   50 (10)
101 ( 7) - 2 (10) ==   66 ( 7)
         101 ( 8) ==   65 (10)
101 ( 8) - 2 (10) ==   77 ( 8)
         101 ( 9) ==   82 (10)
101 ( 9) - 2 (10) ==   88 ( 9)
         101 (10) ==  101 (10)
101 (10) - 2 (10) ==   99 (10)
         101 (11) ==  122 (10)
101 (11) - 2 (10) ==   aa (11)
         101 (12) ==  145 (10)
101 (12) - 2 (10) ==   bb (12)
         101 (13) ==  170 (10)
101 (13) - 2 (10) ==   cc (13)
         101 (14) ==  197 (10)
101 (14) - 2 (10) ==   dd (14)
         101 (15) ==  226 (10)
101 (15) - 2 (10) ==   ee (15)
         101 (16) ==  257 (10)
101 (16) - 2 (10) ==   ff (16)
         101 (17) ==  290 (10)
101 (17) - 2 (10) ==   gg (17)
         101 (18) ==  325 (10)
101 (18) - 2 (10) ==   hh (18)
         101 (19) ==  362 (10)
101 (19) - 2 (10) ==   ii (19)
         101 (20) ==  401 (10)
101 (20) - 2 (10) ==   jj (20)
         101 (21) ==  442 (10)
101 (21) - 2 (10) ==   kk (21)
         101 (22) ==  485 (10)
101 (22) - 2 (10) ==   ll (22)
         101 (23) ==  530 (10)
101 (23) - 2 (10) ==   mm (23)
         101 (24) ==  577 (10)
101 (24) - 2 (10) ==   nn (24)
         101 (25) ==  626 (10)
101 (25) - 2 (10) ==   oo (25)
         101 (26) ==  677 (10)
101 (26) - 2 (10) ==   pp (26)
         101 (27) ==  730 (10)
101 (27) - 2 (10) ==   qq (27)
         101 (28) ==  785 (10)
101 (28) - 2 (10) ==   rr (28)
         101 (29) ==  842 (10)
101 (29) - 2 (10) ==   ss (29)
         101 (30) ==  901 (10)
101 (30) - 2 (10) ==   tt (30)
         101 (31) ==  962 (10)
101 (31) - 2 (10) ==   uu (31)
         101 (32) == 1025 (10)
101 (32) - 2 (10) ==   vv (32)
         101 (33) == 1090 (10)
101 (33) - 2 (10) ==   ww (33)
         101 (34) == 1157 (10)
101 (34) - 2 (10) ==   xx (34)
         101 (35) == 1226 (10)
101 (35) - 2 (10) ==   yy (35)
         101 (36) == 1297 (10)
101 (36) - 2 (10) ==   zz (36)
```



## Haskell


Using built-in functions to convert integer into string, and vice versa, at any base up to 16:


```haskell>Prelude
 Numeric.showIntAtBase 16 Char.intToDigit 42 ""
"2a"
Prelude> fst $ head $ Numeric.readInt 16 Char.isHexDigit Char.digitToInt "2a"
42
```


It's actually more useful to represent digits internally as numbers instead of characters, because then one can define operations that work directly on this representation.

So conversion to and from digits represented as 0-9 and a-z is done in an additional step.


```haskell
import Data.List
import Data.Char

toBase :: Int -> Int -> [Int]
toBase b v = toBase' [] v where
  toBase' a 0 = a
  toBase' a v = toBase' (r:a) q where (q,r) = v `divMod` b

fromBase :: Int -> [Int] -> Int
fromBase b ds = foldl' (\n k -> n * b + k) 0 ds

toAlphaDigits :: [Int] -> String
toAlphaDigits = map convert where
  convert n | n < 10    = chr (n + ord '0')
            | otherwise = chr (n + ord 'a' - 10)

fromAlphaDigits :: String -> [Int]
fromAlphaDigits = map convert where
 convert c | isDigit c = ord c - ord '0'
           | isUpper c = ord c - ord 'A' + 10
           | isLower c = ord c - ord 'a' + 10
```


Example:


```haskell
*Main> toAlphaDigits $ toBase 16 $ 42
"2a"
*Main> fromBase 16 $ fromAlphaDigits $ "2a"
42
```



Or, to allow for digit variants like upper case vs lower case Hexadecimal, we can express our conversion function(s) in terms of a more general '''inBaseDigits''' function which, given an ordered list of digits as its first argument, returns an Int -> String unfold function. (The base is the length of the digit list).

If we want to assume a default character set, then a general '''toBase'''  (Int -> Int -> String) can be also be derived from '''inBaseDigits'''.


```haskell
import Data.List (unfoldr)
import Data.Char (intToDigit)

inBaseDigits :: [Char] -> Int -> String
inBaseDigits ds n =
  let base = length ds
  in reverse $
     unfoldr
       (\x ->
           (if x > 0
              then let (d, r) = quotRem x base
                   in Just (ds !! r, d)
              else Nothing))
       n

inLowerHex :: Int -> String
inLowerHex = inBaseDigits "0123456789abcdef"

inUpperHex :: Int -> String
inUpperHex = inBaseDigits "0123456789ABCDEF"

inBinary :: Int -> String
inBinary = inBaseDigits "01"

inOctal :: Int -> String
inOctal = inBaseDigits "01234567"

inDevanagariDecimal :: Int -> String
inDevanagariDecimal = inBaseDigits "०१२३४५६७८९"

inHinduArabicDecimal :: Int -> String
inHinduArabicDecimal = inBaseDigits "٠١٢٣٤٥٦٧٨٩"

toBase :: Int -> Int -> String
toBase intBase n =
  if (intBase < 36) && (intBase > 0)
    then inBaseDigits (take intBase (['0' .. '9'] ++ ['a' .. 'z'])) n
    else []

main :: IO ()
main =
  mapM_ putStrLn $
  [ inLowerHex
  , inUpperHex
  , inBinary
  , inOctal
  , toBase 16
  , toBase 2
  , inDevanagariDecimal
  , inHinduArabicDecimal
  ] <*>
  [254]
```


{{Out}}

```txt
fe
FE
11111110
376
fe
11111110
२५४
٢٥٤
```



## HicEst


```hicest
CHARACTER txt*80

    num = 36^7 -1                ! 7836416410
    CALL DecToBase(num, txt, 36)
    WRITE(ClipBoard, Name) num, txt, BaseToDec(36, txt)
 END

FUNCTION BaseToDec(base, string)
 CHARACTER string
    BaseToDec = 0
    length = LEN_TRIM(string)
    DO i = 1, length
      n = INDEX("0123456789abcdefghijklmnopqrstuvwxyz", string(i)) - 1
      BaseToDec = BaseToDec + n * base^(length-i)
    ENDDO
 END

SUBROUTINE DectoBase(decimal, string, base)
 CHARACTER string
    string = '0'
    temp = decimal
    length = CEILING( LOG(decimal+1, base) )
    DO i = length, 1, -1
      n = MOD( temp, base )
      string(i) = "0123456789abcdefghijklmnopqrstuvwxyz"(n+1)
      temp = INT(temp / base)
    ENDDO
 END
```


```hicest>num=7836416410; txt=zzzzzzz; 7836416410;</lang


=={{header|Icon}} and {{header|Unicon}}==
Icon and Unicon natively take integers in radix form for bases 2 through 36.  There is no need to convert to integer as the value will be coerced when needed.  However, a conversion routine is needed to convert integers back into radix form. 


```Icon
procedure main()
   every ( ns := "16r5a" | "-12r1a" ) & 
         ( b := 8 | 12 | 16 ) do {
         ns2 := convert(n := numeric(ns),b)
         printf("ns=%s -> n=%d -> %s\n",ns,n,ns2)
      }
end

link printf

procedure convert(i,b)                 #: convert i to base b radix representation
static digits
initial digits := &digits || &lcase

   i := integer(i) | runerr(101, i)    # arg/error checking
   /b := 10 | ( 2 < (b := integer(b)) <= *digits ) | runerr(205,b)

   if b = 10 then return i
   else {
      p := (s := "", (i := -(0 > i),"-")|"") || b || "r" # prefix/setup
      until i = 0 & *s > 0 do  
         s ||:= digits[1 + 1( i % b, i /:= b)]

      return p || reverse(s)
      }
end
```


{{libheader|Icon Programming Library}}  
[http://www.cs.arizona.edu/icon/library/src/procs/printf.icn printf.icn provides printf] 
There are several conversion routines for bases in the IPL, however, none returns the input radix form.

Output:
```txt
ns=16r5a -> n=90 -> 8r132
ns=16r5a -> n=90 -> 12r76
ns=16r5a -> n=90 -> 16r5a
ns=-12r1a -> n=-22 -> -8r26
ns=-12r1a -> n=-22 -> -12r1a
ns=-12r1a -> n=-22 -> -16r16
```



## J

J supports direct specification of native precision integers by base. The numbers are expressed as the base to be used (using base 10), the letter b, followed by the number itself.  Following the initial letter b, other (lower case) letters represent "digts" 10 (a) through 35 (z), as in these examples:

```j
   2b100 8b100 10b_100 16b100 36b100 36bzy
4 64 _100 256 1296 1294
```


Additionally, J has primitives [http://www.jsoftware.com/help/dictionary/d401.htm #.] and [http://www.jsoftware.com/help/dictionary/d402.htm #:] for dealing with base conversion issues.

Here are programs for conversion of numeric values to literals, and of literals to numbers:

```j
numerals=: '0123456789abcdefghijklmnopqrstuvwxyz'
baseNtoL=: numerals {~ #.inv
baseLtoN=: [ #. numerals i. ]
```

Examples of use:

```j
   2 baseNtoL 100 101
1100100
1100101
   16 baseNtoL 26
1a
   36 baseLtoN 'zy'
1294
```

These may be combined so the conversion performed is derived from the type of argument received.

```j
   base=: baseNtoL :: baseLtoN
   
   16 base 'aa'
170
   16 base 170
aa
```

See also primary verbs [http://www.jsoftware.com/help/dictionary/d401.htm Base] and [http://www.jsoftware.com/help/dictionary/d402.htm Antibase].


## Java

for long's:

```java
public static long backToTen(String num, int oldBase){
   return Long.parseLong(num, oldBase); //takes both uppercase and lowercase letters
}

public static String tenToBase(long num, int newBase){
   return Long.toString(num, newBase);//add .toUpperCase() for capital letters
}
```


for BigInteger's:

```java
public static BigInteger backToTenBig(String num, int oldBase){
   return new BigInteger(num, oldBase); //takes both uppercase and lowercase letters
}

public static String tenBigToBase(BigInteger num, int newBase){
   return num.toString(newBase);//add .toUpperCase() for capital letters
}
```



## JavaScript


### ES5


```javascript
k = 26
s = k.toString(16) //gives 1a
i = parseInt('1a',16) //gives 26
//optional special case for hex:
i = +('0x'+s) //hexadecimal base 16, if s='1a' then i=26.
```


Converts a number of arbitrary length from any base to any base
Limitation: Any base or number that causes accumulator to overflow will lose precision!!
Debugging or following the process is easy as it is kept in the expected base string format and order.

```javascript

var baselist = "0123456789abcdefghijklmnopqrstuvwxyz", listbase = [];
for(var i = 0; i < baselist.length; i++) listbase[baselist[i]] = i; // Generate baselist reverse
function basechange(snumber, frombase, tobase)
{
 var i, t, to = new Array(Math.ceil(snumber.length * Math.log(frombase) / Math.log(tobase))), accumulator;
 if(1 < frombase < baselist.length || 1 < tobase < baselist.length) console.error("Invalid or unsupported base!");
 while(snumber[0] == baselist[0] && snumber.length > 1) snumber = snumber.substr(1); // Remove leading zeros character
 console.log("Number is", snumber, "in base", frombase, "to base", tobase, "result should be",
             parseInt(snumber, frombase).toString(tobase));
 for(i = snumber.length - 1, inexp = 1; i > -1; i--, inexp *= frombase)
  for(accumulator = listbase[snumber[i]] * inexp, t = to.length - 1; accumulator > 0 || t >= 0; t--)
  {
   accumulator += listbase[to[t] || 0];
   to[t] = baselist[(accumulator % tobase)  || 0];
   accumulator = Math.floor(accumulator / tobase);
  }
 return to.join('');
}
console.log("Result:", basechange("zzzzzzzzzz", 36, 10));
```

Using BigInteger, can convert any base.

```javascript

// Tom Wu jsbn.js http://www-cs-students.stanford.edu/~tjw/jsbn/
var baselist = "0123456789abcdefghijklmnopqrstuvwxyz", listbase = [];
for(var i = 0; i < baselist.length; i++) listbase[baselist[i]] = i; // Generate baselist reverse
function baseconvert(snumber, frombase, tobase) // String number in base X to string number in base Y, arbitrary length, base
{
 var i, t, to, accum = new BigInteger(), inexp = new BigInteger('1', 10), tb = new BigInteger(),
     fb = new BigInteger(), tmp = new BigInteger();
 console.log("Number is", snumber, "in base", frombase, "to base", tobase, "result should be",
             frombase < 37 && tobase < 37 ? parseInt(snumber, frombase).toString(tobase) : 'too large');
 while(snumber[0] == baselist[0] && snumber.length > 1) snumber = snumber.substr(1); // Remove leading zeros
 tb.fromInt(tobase);
 fb.fromInt(frombase);
 for(i = snumber.length - 1, to = new Array(Math.ceil(snumber.length * Math.log(frombase) / Math.log(tobase))); i > -1; i--)
 {
  accum = inexp.clone();
  accum.dMultiply(listbase[snumber[i]]);
  for(t = to.length - 1; accum.compareTo(BigInteger.ZERO) > 0 || t >= 0; t--)
  {
   tmp.fromInt(listbase[to[t]] || 0);
   accum = accum.add(tmp);
   to[t] = baselist[accum.mod(tb).intValue()];
   accum = accum.divide(tb);
  }
  inexp = inexp.multiply(fb);
 }
 while(to[0] == baselist[0] && to.length > 1) to = to.slice(1); // Remove leading zeros
 return to.join('');
}

```



### ES6


For more flexibility with digit variants (upper and lower case hex, digits in other languages/scripts etc) we can define '''toBase'''(intBase, n) in terms of a more general '''inBaseDigits'''(strDigits, n) which derives the base from the number of digits to be used.


```JavaScript
(() => {
    'use strict';

    // toBase :: Int -> Int -> String
    const toBase = (intBase, n) =>
        intBase < 36 && intBase > 0 ?
        inBaseDigits('0123456789abcdef'.substr(0, intBase), n) : [];


    // inBaseDigits :: String -> Int -> [String]
    const inBaseDigits = (digits, n) => {
        const intBase = digits.length;

        return unfoldr(maybeResidue => {
                const [divided, remainder] = quotRem(maybeResidue.new, intBase);

                return {
                    valid: divided > 0,
                    value: digits[remainder],
                    new: divided
                };
            }, n)
            .reverse()
            .join('');
    };


    // GENERIC FUNCTIONS

    // unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
    const unfoldr = (mf, v) => {
        var xs = [];
        return (until(
            m => !m.valid,
            m => {
                const m2 = mf(m);
                return (
                    xs = xs.concat(m2.value),
                    m2
                );
            }, {
                valid: true,
                value: v,
                new: v,
            }
        ), xs);
    };

    // curry :: ((a, b) -> c) -> a -> b -> c
    const curry = f => a => b => f(a, b);

    // until :: (a -> Bool) -> (a -> a) -> a -> a
    const until = (p, f, x) => {
        let v = x;
        while (!p(v)) v = f(v);
        return v;
    }

    // quotRem :: Integral a => a -> a -> (a, a)
    const quotRem = (m, n) => [Math.floor(m / n), m % n];

    // show :: a -> String
    const show = x => JSON.stringify(x, null, 2);


    // OTHER FUNCTIONS DERIVABLE FROM inBaseDigits

    // inLowerHex :: Int -> String
    const inLowerHex = curry(inBaseDigits)('0123456789abcdef');

    /// inUpperHex :: Int -> String
    const inUpperHex = curry(inBaseDigits)('0123456789ABCDEF');

    // inOctal :: Int -> String
    const inOctal = curry(inBaseDigits)('01234567');

    // inDevanagariDecimal :: Int -> String
    const inDevanagariDecimal = curry(inBaseDigits)('०१२३४५६७८९');


    // TESTS
    // testNumber :: [Int]
    const testNumbers = [255, 240];

    return testNumbers.map(n => show({
        binary: toBase(2, n),
        base5: toBase(5, n),
        hex: toBase(16, n),
        upperHex: inUpperHex(n),
        octal: inOctal(n),
        devanagariDecimal: inDevanagariDecimal(n)
    }));
})();
```


{{Out}}

```txt
{
  "binary": "11111111",
  "base5": "2010",
  "hex": "ff",
  "upperHex": "FF",
  "octal": "377",
  "devanagariDecimal": "२५५"
}, {
  "binary": "11110000",
  "base5": "1430",
  "hex": "f0",
  "upperHex": "F0",
  "octal": "360",
  "devanagariDecimal": "२४०"
}
```



## jq


```jq
# Convert the input integer to a string in the specified base (2 to 36 inclusive)
def convert(base):
  def stream:
    recurse(if . > 0 then ./base|floor else empty end) | . % base ;
  if . == 0 then "0"
  else  [stream] | reverse | .[1:]
  | if   base <  10 then map(tostring) | join("")
    elif base <= 36 then map(if . < 10 then 48 + . else . + 87 end) | implode
    else error("base too large")
    end
  end;

# input string is converted from "base" to an integer, within limits
# of the underlying arithmetic operations, and without error-checking:
def to_i(base):
  explode
  | reverse
  | map(if . > 96  then . - 87 else . - 48 end)  # "a" ~ 97 => 10 ~ 87
  | reduce .[] as $c
      # state: [power, ans]
      ([1,0]; (.[0] * base) as $b | [$b, .[1] + (.[0] * $c)])
  | .[1];
```

'''Example''':

```jq
(255 | convert(16)),
 ("ff" | to_i(16)),
 ("10" | to_i(10))
```

{{Out}}
 $jq -M -r -n -f Non-decimal_radices.jq
 ff
 255
 10


## Julia


```julia

# 26 in base 16 or 2
base(16, 26)
base(2, 26)

# Parse to integer
parse(Int, "1a", 16)
parse(Int, "101101", 2)

```


{{out}}

```txt

"1a"
"11010"
26
45

```



## Kotlin

An approach from first principles rather than using Java library functions:
{{trans|FreeBASIC}}

```scala
// version 1.0.6

fun min(x: Int, y: Int) = if (x < y) x else y

fun convertToBase(n: Int, b: Int): String {
    if (n < 2 || b < 2 || b == 10 || b > 36) return n.toString() // leave as decimal
    val sb = StringBuilder()
    var digit: Int
    var nn = n
    while (nn > 0) {
        digit = nn % b
        if (digit < 10) sb.append(digit)
        else            sb.append((digit + 87).toChar()) 
        nn /= b
    }
    return sb.reverse().toString()
}

fun convertToDecimal(s: String, b: Int): Int {
    if (b !in 2..36) throw IllegalArgumentException("Base must be between 2 and 36")
    if (b == 10) return s.toInt()
    val t = s.toLowerCase()
    var result = 0
    var digit: Int
    var multiplier = 1
    for (i in t.length - 1 downTo 0) {
        digit = -1
        if (t[i] >= '0' && t[i] <= min(57, 47 + b).toChar())
            digit = t[i].toInt() - 48
        else if (b > 10 && t[i] >= 'a' && t[i] <= min(122, 87 + b).toChar())
            digit = t[i].toInt() - 87
        if (digit == -1) throw IllegalArgumentException("Invalid digit present")
        if (digit > 0) result += multiplier * digit
        multiplier *= b
    }
    return result
}     

fun main(args: Array<String>) {
    for (b in 2..36) {
        val s = convertToBase(36, b)
        val f = "%2d".format(b)
        println("36 base $f = ${s.padEnd(6)} -> base $f = ${convertToDecimal(s, b)}")
    }
}
```


{{out}}

```txt

36 base  2 = 100100 -> base  2 = 36
36 base  3 = 1100   -> base  3 = 36
36 base  4 = 210    -> base  4 = 36
36 base  5 = 121    -> base  5 = 36
36 base  6 = 100    -> base  6 = 36
36 base  7 = 51     -> base  7 = 36
36 base  8 = 44     -> base  8 = 36
36 base  9 = 40     -> base  9 = 36
36 base 10 = 36     -> base 10 = 36
36 base 11 = 33     -> base 11 = 36
36 base 12 = 30     -> base 12 = 36
36 base 13 = 2a     -> base 13 = 36
36 base 14 = 28     -> base 14 = 36
36 base 15 = 26     -> base 15 = 36
36 base 16 = 24     -> base 16 = 36
36 base 17 = 22     -> base 17 = 36
36 base 18 = 20     -> base 18 = 36
36 base 19 = 1h     -> base 19 = 36
36 base 20 = 1g     -> base 20 = 36
36 base 21 = 1f     -> base 21 = 36
36 base 22 = 1e     -> base 22 = 36
36 base 23 = 1d     -> base 23 = 36
36 base 24 = 1c     -> base 24 = 36
36 base 25 = 1b     -> base 25 = 36
36 base 26 = 1a     -> base 26 = 36
36 base 27 = 19     -> base 27 = 36
36 base 28 = 18     -> base 28 = 36
36 base 29 = 17     -> base 29 = 36
36 base 30 = 16     -> base 30 = 36
36 base 31 = 15     -> base 31 = 36
36 base 32 = 14     -> base 32 = 36
36 base 33 = 13     -> base 33 = 36
36 base 34 = 12     -> base 34 = 36
36 base 35 = 11     -> base 35 = 36
36 base 36 = 10     -> base 36 = 36

```



## LFE


Converting decimal numbers 26 and 3000 in LFE, using some different mechanisms:

```lisp

> (: erlang list_to_integer '"1a" 16)
26
> #x1a
26
> (: erlang integer_to_list 26 16)
"1A"
> (: erlang list_to_integer '"101110111000" 2)
3000
> #b101110111000
3000
> (: erlang integer_to_list 3000 2)
"101110111000"

```



## Liberty BASIC


```lb
   '   Base Converter v6

    global      alphanum$
    alphanum$   ="0123456789abcdefghijklmnopqrstuvwxyz"

    for i =1 to 20
    RandNum     =   int( 100 *rnd( 1))
    base        =2 +int( 35  *rnd( 1))

    print "Decimal "; using( "###", RandNum); " to base "; using( "###", base);_
         " is "; toBase$( base,  RandNum),_
         " back to dec. "; toDecimal( base, toBase$( base, RandNum))
    next i

    end '   ___________________________________________________________

    function toBase$( base, number) '   Convert decimal variable to number string.
        toBase$             =""
        for i =10 to 1 step -1
            remainder   =number mod base
            toBase$     =mid$( alphanum$, remainder +1, 1) +toBase$
            number      =int( number /base)
            if number <1 then exit for
        next i
    end function

    function toDecimal( base, s$)   '   Convert number string to decimal variable.
        toDecimal   =0
        for i =1 to len( s$)
            toDecimal =toDecimal *base +instr( alphanum$, mid$( s$, i, 1), 1) -1
        next i
    end function
 
```



## Lua

Only had to write 'dec2base' as the reverse is provided by the in-built function 'tonumber'

```Lua
function dec2base (base, n)
    local result, digit = ""
    while n > 0 do
        digit = n % base
        if digit > 9 then digit = string.char(digit + 87) end
        n = math.floor(n / base)
        result = digit .. result
    end
    return result
end

local x = dec2base(16, 26)
print(x)                    --> 1a
print(tonumber(x, 16))      --> 26
```



## M4


```M4
eval(26,16)
define(`frombase',`eval(0r$2:$1)')
frombase(1a,16)
```


Output:

```txt

1a

26

```



## Maple


```Maple
#converts a number to a given based represented by a string
to_base := proc(num, based)
	local i;
	local chart := "0123456789abcdefghijklmnopqrstuvwxyz";
	local conversion := ListTools:-Reverse((convert(num,base,based)));
	local str := StringTools:-StringBuffer();
	for i in conversion do
		str:-append(chart[i+1]);
	end do;
	return str;
end proc:

#find the location of char in chart
find_digit := proc(char)
	if (StringTools:-HasAlpha(char)) then
		return (StringTools:-Ord(char) - 87);
	else 
		return (StringTools:-Ord(char) - 48);
	end if;
end proc:

#converts a string  with given base to a number
from_base := proc(str, base)
	local char;
	local result := 0;
	for char in str do
		result *= base;
		result += find_digit(char);
	end do;
	return result;
end proc:
```

{{Out|Usage}}

```txt

to_base(32, 11);
to_base(0, 16);
from_base("2a", 11);
from_base("1a",16);

```

{{Out|Output}}

```txt

"2a"
"0"
32
26

```



## Mathematica

Use the built-in functions IntegerString[] and FromDigits[]:

```Mathematica
IntegerString[26,16]
FromDigits["1a", 16])
```


Output:

```txt
"1a"

26
```


=={{header|MATLAB}} / {{header|Octave}}==
Use the built-in functions base2dec() and dec2base():

```Matlab
dec2base(26,16)
base2dec('1a', 16)
```


Output:

```txt
1A

26
```


=={{header|МК-61/52}}==

```txt

П8	->	1	0	П0	ПП	13	ИП7	П0	ИП8
ПП	13	С/П	П7	->	П6	->	1	П4	П5
Сx	<->	^	ПП	68	П3	-	ИП7	*	П2
ПП	68	ИП4	ИП6	*	П4	/	+	ИП2	ИП1
-	x#0	45	L0	27	->	ИП3	^	ИП7	/
ПП	68	ИП7	*	-	ИП5	*	+	ИП5	ИП6
*	П5	->	ИП1	x=0	47	->	В/О	1	+
П1	КИП1	->	->	ИП1	В/О

```


Input: ''N<sub>m</sub> ^ m ^ n В/О С/П''.

Output: ''N<sub>n</sub> -> PX''.


## NetRexx

In NetRexx numbers are held as Rexx strings so you can take advantage of Java's BigInteger to do radix conversions.

```NetRexx
/* NetRexx */
options replace format comments java crossref symbols nobinary

import java.math.BigInteger

numeric digits 200

parse arg input -- input should be val, radix; no input results in using default test data
-- test data - number pairs where 1st is value and 2nd is target radix
inputs = [ -
  '1234,         10', '01234,  8', 'fe,  16', 'f0e,   16', -
  '0,            10', '00,     2', '11,   2', '070,    8', -
  '77,            8', 'f0e,   16', '070, 16', '0xf0e, 36', -
  '000999ABCXYZ, 36', 'ff,    36', 'f,   16', 'z,     37'  -
  ]
if input.length() > 0 then inputs = [input] -- replace test data with user input

loop i_ = 0 to inputs.length - 1
  do
    in = inputs[i_]
    parse in val . ',' radix .
    valB = toDecimal(val, radix)        -- NetRexx default is to store digits as Rexx strings
    valD = fromDecimal(valB + 0, radix) -- Add zero just to prove the result treated as a number
    say val.right(16)'['radix.right(2, 0)']:' valB.right(16)'[10] ==' valD.right(16)'['radix.right(2, 0)']'
  catch nx = NumberFormatException
    say 'Error -- Input:' val', radix:' radix
    nx.printStackTrace()
  end
  end i_

return

method toDecimal(val = String, radix = int 10) public static returns Rexx
  bi = BigInteger(val, radix)
  return bi.toString()

method fromDecimal(val = String, radix = int 10) public static returns Rexx
  bi = BigInteger(val.toString(), 10)
  return bi.toString(radix)

```

'''Output:'''

```txt

            1234[10]:             1234[10] ==             1234[10]
           01234[08]:              668[10] ==             1234[08]
              fe[16]:              254[10] ==               fe[16]
             f0e[16]:             3854[10] ==              f0e[16]
               0[10]:                0[10] ==                0[10]
              00[02]:                0[10] ==                0[02]
              11[02]:                3[10] ==               11[02]
             070[08]:               56[10] ==               70[08]
              77[08]:               63[10] ==               77[08]
             f0e[16]:             3854[10] ==              f0e[16]
             070[16]:              112[10] ==               70[16]
           0xf0e[36]:          1559102[10] ==             xf0e[36]
    000999ABCXYZ[36]:   26115481426427[10] ==        999abcxyz[36]
              ff[36]:              555[10] ==               ff[36]
               f[16]:               15[10] ==                f[16]
Error -- Input: z, radix: 37
java.lang.NumberFormatException: Radix out of range
	at java.math.BigInteger.<init>(BigInteger.java:294)
	at RNonDecRadixConvert.toDecimal(RNonDecRadixConvert.nrx:77)
	at RNonDecRadixConvert.main(RNonDecRadixConvert.nrx:57)

```



## Nim


```nim
import strutils

proc reverse(a: string): string =
  result = newString(a.len)
  for i, c in a:
    result[a.high - i] = c

const digits = "0123456789abcdefghijklmnopqrstuvwxyz"

proc toBase[T](num: T, base: range[2..36]): string =
  if num == 0: return "0"
  result = ""
  if num < 0: result.add '-'
  var tmp = abs(num)
  var s = ""
  while tmp > 0:
    s.add digits[int(tmp mod base)]
    tmp = tmp div base
  result.add s.reverse

proc fromBase(str: string, base: range[2..36]): BiggestInt =
  var str = str
  let first = if str[0] == '-': 1 else: 0

  for i in first .. str.high:
    let c = str[i].toLower
    assert c in digits[0 .. <base]
    result = result * base + digits.find c

  if first == 1: result *= -1

echo 26.toBase 16
echo "1a".fromBase 16
```

Output:

```txt
1a
26
```



## OCaml


```ocaml
let int_of_basen n str =
  match n with
  | 16 -> int_of_string("0x" ^ str)
  |  2 -> int_of_string("0b" ^ str)
  |  8 -> int_of_string("0o" ^ str)
  | _ -> failwith "unhandled"

let basen_of_int n d =
  match n with
  | 16 -> Printf.sprintf "%x" d
  |  8 -> Printf.sprintf "%o" d
  | _ -> failwith "unhandled"
```


 # basen_of_int 16 26 ;;
 - : string = "1a"
 
 # int_of_basen 16 "1a" ;;
 - : int = 26

A real base conversion example: {{trans|Haskell}}


```ocaml
let to_base b v =
  let rec to_base' a v =
    if v = 0 then
      a
    else
      to_base' (v mod b :: a) (v / b)
  in
    to_base' [] v

let from_base b ds =
  List.fold_left (fun n k -> n * b + k) 0 ds

let to_alpha_digit n =
  if n < 10 then
    char_of_int (n + int_of_char '0')
  else
    char_of_int (n + int_of_char 'a' - 10)

let to_alpha_digits ds =
  let buf = Buffer.create (List.length ds) in
    List.iter (fun i -> Buffer.add_char buf (to_alpha_digit i)) ds;
    Buffer.contents buf

let from_alpha_digit c = match c with
    '0'..'9' -> int_of_char c - int_of_char '0'
  | 'A'..'Z' -> int_of_char c - int_of_char 'A' + 10
  | 'a'..'z' -> int_of_char c - int_of_char 'a' + 10

let from_alpha_digits s =
  let result = ref [] in
    String.iter (fun c -> result := from_alpha_digit c :: !result) s;
    List.rev !result
```


Example:


```txt

# to_alpha_digits (to_base 16 42);;
- : string = "2a"
# from_base 16 (from_alpha_digits "2a");;
- : int = 42

```



## PARI/GP


```parigp
toBase(n,b)={
  my(s="",t);
  while(n,
    t=n%b;
    n\=b;
    s=Str(if(t<=9,t,Strchr(Vecsmall([87+t]))),s)
  );
  if(#s,s,"0")
};
fromBase(s,b)={
  my(t=0);
  s=Vecsmall(s);
  for(i=1,#s,1,
    t=b*t+s[i]-if(s[i]<58,48,87)
  );
  t
};
```



## Pascal

{{libheader| Math SysUtils}}
{{works with|Free_Pascal}}

```pascal
Program ConvertDemo(output);

uses
  Math, SysUtils;

const
  alphanum = '0123456789abcdefghijklmnopqrstuvwxyz';

function ToDecimal(base: integer; instring: string): integer;
  var
    inlength, i, n: integer;
  begin 
    ToDecimal := 0;
    inlength := length(instring);
    for i := 1 to inlength do
    begin
      n := pos(instring[i], alphanum) - 1;
      n := n * base**(inlength-i);
      Todecimal := ToDecimal + n;
    end;
  end;

function ToBase(base, number: integer): string;
  var
    i, rem: integer;
  begin
    ToBase :='                               ';
    for i := 31 downto 1 do
    begin
      if (number < base) then
      begin
        ToBase[i] := alphanum[number+1];
        break;
      end;
      rem := number mod base;
      ToBase[i] := alphanum[rem+1];
      number := number div base;
    end;
    ToBase := trimLeft(ToBase);
  end;

begin
  writeln ('1A: ', ToDecimal(16, '1a'));
  writeln ('26: ', ToBase(16, 26));
end.

```

Output:

```txt
% ./Convert
1A: 26
26: 1a
```



## Perl

For base 2 and 16, we can do this entirely with language features:

```perl
sub to2  { sprintf "%b", shift; }
sub to16 { sprintf "%x", shift; }
sub from2  { unpack("N", pack("B32", substr("0" x 32 . shift, -32))); }
sub from16 { hex(shift); }
```


Small functions will handle arbitrary base conversions for bases 2-36:

```perl
sub base_to {
  my($n,$b) = @_;
  my $s = "";
  while ($n) {
    $s .= ('0'..'9','a'..'z')[$n % $b];
    $n = int($n/$b);
  }
  scalar(reverse($s));
}
sub base_from {
  my($n,$b) = @_;
  my $t = 0;
  for my $c (split(//, lc($n))) {
    $t = $b * $t + index("0123456789abcdefghijklmnopqrstuvwxyz", $c);
  }
  $t;
}
```


There are a plethora of modules that perform base conversion.

The core [https://metacpan.org/pod/distribution/perl/ext/POSIX/lib/POSIX.pod POSIX] module includes strtol (and strtoul) which is simple and fast, but only does conversions from a base.  On some platforms the function may be limited to 32-bit even with a 64-bit Perl.

```perl
use POSIX;
my ($num, $n_unparsed) = strtol('1a', 16);
$n_unparsed == 0 or die "invalid characters found";
print "$num\n"; # prints "26"
```


The [https://metacpan.org/pod/ntheory ntheory] module includes functions that will perform base conversion, and is fast.  It supports bases up to 36 and bigints.{{libheader|ntheory}}

```perl
use ntheory qw/fromdigits todigitstring/;
my $n   = 65261;
my $n16 = todigitstring($n, 16) || 0;
my $n10 = fromdigits($n16, 16);
say "$n $n16 $n10";  # prints "65261 feed 65261"
```


Other modules include but are not limited to:

* [https://metacpan.org/pod/Math::BaseCalc Math::BaseCalc]
* [https://metacpan.org/pod/Math::Int2Base Math::Int2Base]
* [https://metacpan.org/pod/Math::NumberBase Math::NumberBase]
* [https://metacpan.org/pod/Convert::AnyBase Convert::AnyBase]
* [https://metacpan.org/pod/Math::BaseCnv Math::BaseCnv]
* [https://metacpan.org/pod/Math::BaseConvert Math::BaseConvert]

The last two are ''much'' slower than the others or the simple functions above, but may have extra features.  Math::Base::Convert and Convert::BaseN are currently not recommended.

The module [https://metacpan.org/pod/Math::Fleximal Math::Fleximal] not only does very arbitrary base conversion, but allows computations in different bases.


## Perl 6


```perl6
sub from-base(Str $str, Int $base) {
    +":$base\<$str>";
}

sub to-base(Real $num, Int $base) {
    $num.base($base);
}
```

These work on any real type including integer types.


## Phix


```Phix
-- demo\rosetta\Convert_base.exw
function to_base(integer i, integer base)
integer c
sequence s = ""
    while i>0 do
        c = remainder(i,base)
        if c<10 then
            c += '0'
        else
            c += 'a'-10
        end if
        s = prepend(s,c)
        i = floor(i/base)
    end while
 
    if length(s) = 0 then
        s = "0"
    end if
 
    return s
end function
 
function from_base(string s, integer base)
integer res = 0, c
    for i=1 to length(s) do
        c = s[i]
        if c>='0' and c<='9' then
            c -= '0'
        else
            c -= 'a'-10
        end if
        res = res*base+c
    end for
    return res
end function

?to_base(256,16)
?from_base("100",16)
```

{{out}}

```txt

"100"
256

```



## PHP

PHP has a base_convert() function that directly converts between strings of one base and strings of another base:

```php
base_convert("26", 10, 16); // returns "1a"
```


If you want to convert a string to an integer, the intval() function optionally takes a base argument when given a string:

```php
intval("1a", 16); // returns 26
```


To go the other way around, I guess you can use base_convert() again; I am unaware of a better way:

```php
base_convert(26, 10, 16); // returns "1a"
```


In addition, there are specialized functions for converting certain bases:

```php
// converts int to binary string
decbin(26); // returns "11010"
// converts int to octal string
decoct(26); // returns "32"
// converts int to hex string
dechex(26); // returns "1a"
// converts binary string to int
bindec("11010"); // returns 26
// converts octal string to int
octdec("32"); // returns 26
// converts hex string to int
hexdec("1a"); // returns 26
```



## PL/I


```PL/I

convert: procedure (N, base) returns (character (64) varying) recursive;
   declare N fixed binary (31), base fixed binary;
   declare table (0:15) character (
      '0', '1', '2', '3', '4', '5', '6', '7',
      '8', '9', 'a', 'b', 'c', 'd', 'e', 'f');
   declare s character (64) varying;

   if N = 0 then return ('');

   s = convert(N/base, base);
   return (s || table(mod(N, base)) );
end convert;

```



## PicoLisp


```PicoLisp
(de numToString (N Base)
   (default Base 10)
   (let L NIL
      (loop
         (let C (% N Base)
            (and (> C 9) (inc 'C 39))
            (push 'L (char (+ C `(char "0")))) )
         (T (=0 (setq N (/ N Base)))) )
      (pack L) ) )

(de stringToNum (S Base)
   (default Base 10)
   (let N 0
      (for C (chop S)
         (when (> (setq C (- (char C) `(char "0"))) 9)
            (dec 'C 39) )
         (setq N (+ C (* N Base))) )
      N ) )

(prinl (numToString 26 16))
(prinl (stringToNum "1a" 16))
(prinl (numToString 123456789012345678901234567890 36))
```

Output:

```txt
"1a"
26
"byw97um9s91dlz68tsi"
```



## Pop11


Pop11 can input and output routines can use any base up to 36
(depending on value 'pop_pr_radix' variable).  'radix_apply'
runs i/o routine temporarly setting 'pop_pr_radix' to given
value.  'sprintf' procedure instead of printing returns string.
So, to convert number to given value we just compose
built-in procedures:


```pop11
define number_to_base(n, base);
    radix_apply(n, '%p', sprintf, base);
enddefine;
```


In input base optionally preceeds the number, for example
8:15 is 13.  So, to convert string in given base we need
to prepend base prefix and read number from string:


```pop11
define string_in_base_to_number(s, base);
    incharitem(stringin(base >< ':' >< s))();
enddefine;
```


## PureBasic


```PureBasic
Global alphanum$ = "0123456789abcdefghijklmnopqrstuvwxyz" ;36 digits
#maxIntegerBitSize = SizeOf(Integer) * 8
 
Procedure toDecimal(base, s.s)
  Protected length, i, toDecimal
  
  length = Len(s)
  If length: toDecimal = FindString(alphanum$, Left(s, 1), 1) - 1: EndIf 
  
  For i = 2 To length
    toDecimal * base + FindString(alphanum$, Mid(s, i, 1), 1) - 1
  Next
  ProcedureReturn toDecimal
EndProcedure
 
Procedure.s toBase(base, number)
  Protected i, rem, toBase.s{#maxIntegerBitSize} = Space(#maxIntegerBitSize) 
  
  For i = #maxIntegerBitSize To 1 Step -1
    rem = number % base
    PokeC(@toBase + i - 1, PeekC(@alphanum$ + rem))
    If number < base: Break: EndIf 
    number / base
  Next
  ProcedureReturn LTrim(toBase)
EndProcedure

If OpenConsole()
  PrintN( Str(toDecimal(16, "1a")) )
  
  PrintN( toBase(16, 26) )
  
  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit")
  Input()
  CloseConsole()
EndIf
```

Sample output:

```txt
26
1a
```



## Python

Converting from string to number is easy:

```python
i = int('1a',16)  # returns the integer 26
```

Converting from number to string is harder:

```python
digits = "0123456789abcdefghijklmnopqrstuvwxyz"
def baseN(num,b):
   return (((num == 0) and  "0" ) 
           or ( baseN(num // b, b).lstrip("0") 
                + digits[num % b]))

# alternatively:
def baseN(num,b):
  if num == 0: return "0"
  result = ""
  while num != 0:
    num, d = divmod(num, b)
    result += digits[d]
  return result[::-1] # reverse

k = 26
s = baseN(k,16) # returns the string 1a
```



## R


```R



int2str <- function(x, b) {
	if(x==0) return("0")
	if(x<0) return(paste0("-", base(-x,b)))
	
	map <- c(as.character(0:9), letters)
	res <- ""
	while (x>0) {
		res <- c(map[x %% b + 1], res)
		x <- x %/% b
	}
	return(paste(res, collapse=""))
}

str2int <- function(s, b) {
	map <- c(as.character(0:9), letters)
	s <- strsplit(s,"")[[1]]
	res <- sapply(s, function(x) which(map==x))
	res <- as.vector((res-1) %*% b^((length(res)-1):0))
	return(res)
}

## example: convert 255 to hex (ff):
int2str(255, 16)

## example: convert "1a" in base 16 to integer (26):
str2int("1a", 16)


```



## Racket



```Racket

#lang racket

;; Both assume valid inputs
(define (num->str N r)
  (let loop ([N N] [digits '()])
    (define-values [N1 d] (quotient/remainder N r))
    (define digits1 (cons (integer->char (+ d (if (< d 10) 48 55))) digits))
    (if (zero? N) (list->string digits1) (loop N1 digits1))))
(define (str->num S r)
  (for/fold ([N 0])
            ([B (string->bytes/utf-8 (string-upcase S))])
    (+ (* N r) (- B (if (< 64 B) 55 48)))))

;; To try it out:
(define (random-test)
  (define N (random 1000000))
  (define r (+ 2 (random 35)))
  (define S (num->str N r))
  (define M (str->num S r))
  (printf "~s -> ~a#~a -> ~a => ~a\n" N S r M (if (= M N) 'OK 'BAD)))
;; (random-test)

```



## REXX

Instead of writing two separate routines, only one was written to handle both tasks.

This routine was ripped out from a bigger version of mine that allowed any number as input, including decimal fractions (or whatever base).

Illegal numerals/digits are detected as well as illegal (or unsupported) bases.

No   ''number-conversion''   BIFs   (<u>B</u>uilt-<u>I</u>n <u>F</u>unctions)   were used in this REXX program.

```txt

  ┌────────────────────────────────────────────────────────────────────┐
┌─┘ Input to this program     (bases must be positive integers > 1):   └─┐
│                                                                        │
│                       x        is required  (it may have a sign).      │
│                     toBase     the base to convert   X   to.           │
│                     inBase     the base  X  is expressed in.           │
│                                                                        │
│  If  X  has a leading sign,  it is maintained (kept) after conversion. │
│                                                                        │
│  toBase   or   inBase    can be a comma (,)  which causes the default  │
└─┐ of  10  to be used.    The limits of bases are:    2 ──► 90.       ┌─┘
  └────────────────────────────────────────────────────────────────────┘

```


```rexx
/*REXX program converts integers from  one base  to  another   (using bases  2 ──► 90). */
@abc = 'abcdefghijklmnopqrstuvwxyz'              /*lowercase (Latin or English) alphabet*/
parse  upper  var  @abc    @abcU                 /*uppercase a version of   @abc.       */
@@ = 0123456789 || @abc || @abcU                 /*prefix them with all numeric digits. */
@@ = @@'<>[]{}()?~!@#$%^&*_=|\/;:¢¬≈'            /*add some special characters as well. */
                                                 /* [↑]  all characters must be viewable*/
numeric digits 3000                              /*what da hey, support gihugeic numbers*/
maxB=length(@@)                                  /*max base/radix supported in this code*/
parse arg x toB inB 1 ox . 1 sigX 2 x2 .         /*obtain:  three args, origX, sign ··· */
if pos(sigX, "+-")\==0  then x=x2                /*does X have a leading sign (+ or -) ?*/
                        else sigX=               /*Nope. No leading sign for the X value*/
if   x==''             then call erm             /*if no  X  number, issue an error msg.*/
if toB=='' | toB==","  then toB=10               /*if skipped, assume the default (10). */
if inB=='' | inB==","  then inB=10               /* "    "        "    "     "      "   */
if inB<2   | inB>maxB  | \datatype(inB,'W')  then call erb  "inBase "  inB
if toB<2   | toB>maxB  | \datatype(toB,'W')  then call erb  "toBase "  toB
#=0                                              /*result of converted  X  (in base 10).*/
      do j=1  for length(x)                      /*convert  X:   base inB  ──► base 10. */
      ?=substr(x,j,1)                            /*pick off a numeral/digit from  X.    */
      _=pos(?, @@)                               /*calculate the value of this numeral. */
      if _==0 | _>inB  then call erd x           /*is  _  character an illegal numeral? */
      #=#*inB+_-1                                /*build a new number,  digit by digit. */
      end    /*j*/                               /* [↑]  this also verifies digits.     */
y=                                               /*the value of   X   in   base  B.     */
      do  while  # >= toB                        /*convert #:    base 10  ──►  base toB.*/
      y=substr(@@, (#//toB)+1, 1)y               /*construct the output number.         */
      #=#%toB                                    /*      ··· and whittle  #  down also. */
      end    /*while*/                           /* [↑]  algorithm may leave a residual.*/
                                                 /* [↓]         Y  is the residual.     */
y=sigX || substr(@@, #+1, 1)y                    /*prepend the sign  if  it existed.    */
say ox  "(base"      inB')'       center("is",20)      y      '(base'    toB")"
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
erb:  call ser  'illegal'   arg(1)",  it must be in the range:  2──►"maxB
erd:  call ser  'illegal digit/numeral  ['?"]  in:  "       x
erm:  call ser  'no argument specified.'
ser:  say; say  '***error!***';         say arg(1);             exit 13
```

'''output'''   when input is expressed in hexadecimal   (maximum positive integer in a signed 32-bit word):   <tt> 7fffffff   ,   16 </tt>

```txt

7fffffff (base 16)          is          2147483647 (base 10)

```

'''output'''   when input used (expressed in decimal) is:   <tt> 4095   2 </tt>

```txt

4095 (base 10)          is          111111111111 (base 2)

```

'''output'''   when input used (expressed in decimal) is:   <tt> 100   3   2 </tt>

```txt

100 (base 2)          is          11 (base 3)

```

'''output'''   when input used (expressed in base 36) is:   <tt> zombieseatingdeadvegetables   10   36 </tt>

```txt

zombieseatingdeadvegetables (base 36)          is          1038334289300125869792154778345043071467300 (base 10)

```



## Ring


```ring

# Project : Non-decimal radices/Convert

see "0 (decimal) -> " + hex(0) + " (base 16)" + nl
see "26 (decimal) -> " + hex(26) + " (base 16)" + nl
see "383 (decimal) -> " + hex(383) + " (base 16)" + nl
see "26 (decimal) -> " + tobase(26, 2) + " (base 2)" + nl
see "383 (decimal) -> " + tobase(383, 2)  + " (base 2)" + nl
see "1a (base 16) -> " + dec("1a") + " (decimal)" + nl
see "1A (base 16) -> " + dec("1A") + " (decimal)" + nl
see "17f (base 16) -> " + dec("17f") + " (decimal)" + nl
see "101111111 (base 2) -> " + bintodec("101111111") + " (decimal)" + nl
 
func tobase(nr, base) 
     binary = 0
     i = 1  
     while(nr != 0) 
           remainder = nr % base
           nr = floor(nr/base)
           binary= binary + (remainder*i)
           i = i*10
     end
     return string(binary)
 
func bintodec(bin)
     binsum = 0
     for n=1  to len(bin)
         binsum = binsum + number(bin[n]) *pow(2, len(bin)-n)
     next
     return binsum

```

Output:

```txt

0 (decimal) -> 0 (base 16)
26 (decimal) -> 1a (base 16)
383 (decimal) -> 17f (base 16)
26 (decimal) -> 11010 (base 2)
383 (decimal) -> 101111111 (base 2)
1a (base 16) -> 26 (decimal)
1A (base 16) -> 26 (decimal)
17f (base 16) -> 383 (decimal)
101111111 (base 2) -> 383 (decimal)

```



## Ruby

This converts strings from any base to any base up to base 36.

```ruby
class String
  def convert_base(from, to)
    Integer(self, from).to_s(to)  
    # self.to_i(from).to_s(to) #if you don't want exceptions
  end
end

# first three taken from TCL
p "12345".convert_base(10, 23) # => "107h"
p "107h".convert_base(23, 7) # =>"50664"
p "50664".convert_base(7, 10) # =>"12345"
p "1038334289300125869792154778345043071467300".convert_base(10, 36) # =>"zombieseatingdeadvegetables"
p "ff".convert_base(15, 10) # => ArgumentError
```



## Run BASIC


```runbasic
global    basCvt$
basCvt$   ="0123456789abcdefghijklmnopqrstuvwxyz"
html "<table border=1><tr bgcolor=wheat align=center><td>Decimal</td><td>To Base</td><td>Num</td><td>to Dec</td></tr>"

for i =1 to 10
  RandNum     =    int(100 * rnd(1))
  base        = 2 +int(35  * rnd(1))
 
  html "<tr align=right><td>";using("###", RandNum);"</td><td>";using("###", base);"</td><td>";toBase$(base,RandNum);"</td><td>";toDecimal( base, toBase$( base, RandNum));"</td></tr>"
next i
html "</table>"
end 

function toBase$(b,n) '   b=base n=nmber
  toBase$             =""
  for i =10 to 1 step -1
     toBase$     =mid$(basCvt$,n mod b +1,1) +toBase$
    n      =int( n /b)
    if n <1 then exit for
  next i
end function
 
function toDecimal( b, s$)   '   scring number to decimal
  toDecimal   =0
  for i =1 to len( s$)
    toDecimal = toDecimal * b + instr(basCvt$,mid$(s$,i,1),1) -1
  next i
end function
```

<table border=1>
<tr bgcolor=wheat align=center><td>Decimal</td><td>To Base</td><td>Num</td><td>to Dec</td></tr>
<tr align=right><td> 51</td><td>  2</td><td>110011</td><td>51</td></tr>
<tr align=right><td> 27</td><td> 10</td><td>27</td><td>27</td></tr>
<tr align=right><td> 12</td><td> 18</td><td>c</td><td>12</td></tr>
<tr align=right><td> 90</td><td> 35</td><td>2k</td><td>90</td></tr>
<tr align=right><td> 99</td><td> 17</td><td>5e</td><td>99</td></tr>
<tr align=right><td> 99</td><td> 18</td><td>59</td><td>99</td></tr>
<tr align=right><td> 55</td><td> 11</td><td>50</td><td>55</td></tr>
<tr align=right><td> 56</td><td> 28</td><td>20</td><td>56</td></tr>
<tr align=right><td> 71</td><td> 34</td><td>23</td><td>71</td></tr>
<tr align=right><td> 61</td><td> 23</td><td>2f</td><td>61</td></tr></table>


## Scala


```Scala
def backToBig(num: String, oldBase: Int): BigInt = BigInt(num, oldBase)

def bigToBase(num: BigInt, newBase: Int): String = num.toString(newBase)
```



## Seed7

The type [http://seed7.sourceforge.net/manual/types.htm#integer integer]
defines the operator [http://seed7.sourceforge.net/libraries/integer.htm#%28in_integer%29radix%28in_integer%29 radix]
and the function [http://seed7.sourceforge.net/libraries/integer.htm#integer%28in_string,in_integer%29 integer],
which convert to string and vice versa.
The type [http://seed7.sourceforge.net/manual/types.htm#bigInteger bigInteger]
defines [http://seed7.sourceforge.net/libraries/bigint.htm#%28in_var_bigInteger%29radix%28in_integer%29 radix]
and [http://seed7.sourceforge.net/libraries/bigint.htm#bigInteger%28in_string,in_integer%29 bigInteger]
for corresponding purposes.


```seed7
$ include "seed7_05.s7i";
  include "bigint.s7i";
 
const proc: main is func
  begin
    writeln(60272032366_ radix 36);      # Convert bigInteger to string
    writeln(591458 radix 36);            # Convert integer to string

    writeln(bigInteger("rosetta", 36));  # Convert string to bigInteger
    writeln(integer("code", 36));        # Convert string to integer
  end func;
```


{{out}}

```txt

rosetta
code
60272032366
591458

```



## Sidef

Built-in: 

```ruby
say 60272032366.base(36)    # convert number to string
say Number("rosetta", 36)   # convert string to number
```


User-defined:
{{trans|Perl}}

```ruby
static to = [@|'0'..'9', @|'a'..'z']
static from = Hash(to.pairs.map{@|_}.flip...)

func base_to(n, b) {
    var s = ""
    while (n) {
        s += to[n % b]
        n //= b
    }
    s.reverse
}

func base_from(n, b) {
    var t = 0
    n.each { |c| t = (b*t + from{c}) }
    t
}

say base_from("rosetta", 36)        # string to number
say base_to(60272032366, 36)        # number to string
```



## Slate


```slate
26 printString &radix: 16
Integer readFrom: '1A' &radix: 16.
```



## Standard ML

{{trans|Haskell}}


```sml
fun toBase b v = let
  fun toBase' (a, 0) = a
    | toBase' (a, v) = toBase' (v mod b :: a, v div b)
in
  toBase' ([], v)
end

fun fromBase b ds =
  foldl (fn (k, n) => n * b + k) 0 ds

val toAlphaDigits = let
  fun convert n = if n < 10 then chr (n + ord #"0")
                            else chr (n + ord #"a" - 10)
in
  implode o map convert
end

val fromAlphaDigits = let
  fun convert c = if      Char.isDigit c then ord c - ord #"0"
                  else if Char.isUpper c then ord c - ord #"A" + 10
                  else if Char.isLower c then ord c - ord #"a" + 10
                  else raise Match
in
  map convert o explode
end
```


Example:


```txt

val toAlphaDigits = fn : int list -> string
- toAlphaDigits (toBase 16 42);
val it = "2a" : string
- fromBase 16 (fromAlphaDigits "2a");
val it = 42 : int

```



## Swift

Converting integer to string:

```swift
println(String(26, radix: 16)) // prints "1a"
```


Converting string to integer: 

```swift
import Darwin
func string2int(s: String, radix: Int) -> Int {
  return strtol(s, nil, Int32(radix))
  // there is also strtoul() for UInt, and strtoll() and strtoull() for Int64 and UInt64, respectively
}
println(string2int("1a", 16)) // prints "26"
```



## Tcl

Tcl <code>scan</code> and <code>format</code> commands can convert between decimal, octal and hexadecimal, but this solution can convert between any arbitrary bases.

```tcl
namespace eval baseconvert {
    variable chars "0123456789abcdefghijklmnopqrstuvwxyz"
    namespace export baseconvert
}
proc baseconvert::dec2base {n b} {
    variable chars
    expr {$n == 0 ? 0
          : "[string trimleft [dec2base [expr {$n/$b}] $b] 0][string index $chars [expr {$n%$b}]]"
    }
}
proc baseconvert::base2dec {n b} {
    variable chars
    set sum 0
    foreach char [split $n ""] {
        set d [string first $char [string range $chars 0 [expr {$b - 1}]]]
        if {$d == -1} {error "invalid base-$b digit '$char' in $n"}
        set sum [expr {$sum * $b + $d}]
    }
    return $sum
}
proc baseconvert::baseconvert {n basefrom baseto} {
    dec2base [base2dec $n $basefrom] $baseto
}

namespace import baseconvert::baseconvert 
baseconvert 12345 10 23 ;# ==> 107h
baseconvert 107h 23 7   ;# ==> 50664
baseconvert 50664 7 10  ;# ==> 12345
```



## Ursala

A function parameterized by the base b performs the conversion in each direction.
Folding (=>), iteration (->), and reification (-:) operators among others are helpful. 

```Ursala
#import std
#import nat

num_to_string "b" = ||'0'! (-: num digits--letters)*+ @NiX ~&r->l ^|rrPlCrlPX/~& division\"b"

string_to_num "b" = @x =>0 sum^|/(-:@rlXS num digits--letters) product/"b"
```

This test program performs the conversions in both directions for a selection of numbers
in base 8 and base 32.

```Ursala
test_data = <1,2,15,32,100,65536,323498993>

#cast %sLnLUL

tests = 

<
   num_to_string32* test_data,
   string_to_num32* num_to_string32* test_data,
   num_to_string8*  test_data,
   string_to_num8*  num_to_string8* test_data>
```

output:

```txt

<
   <'1','2','f','10','34','2000','9kgcvh'>,
   <1,2,15,32,100,65536,323498993>,
   <'1','2','17','40','144','200000','2322031761'>,
   <1,2,15,32,100,65536,323498993>>
```



## VBA


```vb
Private Function to_base(ByVal number As Long, base As Integer) As String
    Dim digits As String, result As String
    Dim i As Integer, digit As Integer
    digits = "0123456789abcdefghijklmnopqrstuvwxyz"
    Do While number > 0
        digit = number Mod base
        result = Mid(digits, digit + 1, 1) & result
        number = number \ base
    Loop
    to_base = result
End Function
Private Function from_base(number As String, base As Integer) As Long
    Dim digits As String, result As Long
    Dim i As Integer
    digits = "0123456789abcdefghijklmnopqrstuvwxyz"
    result = Val(InStr(1, digits, Mid(number, 1, 1), vbTextCompare) - 1)
    For i = 2 To Len(number)
        result = result * base + Val(InStr(1, digits, Mid(number, i, 1), vbTextCompare) - 1)
    Next i
    from_base = result
End Function
Public Sub Non_decimal_radices_Convert()
    Debug.Print "26 decimal in base 16 is: "; to_base(26, 16); ". Conversely, hexadecimal 1a in decimal is: "; from_base("1a", 16)
End Sub
```
{{out}}
```txt
26 decimal in base 16 is: 1a. Conversely, hexadecimal 1a in decimal is:  26 
```


## Wolframalpha

input box:  1801 decimal to base 16

input box:  (99 base 12)+(77 base 8)

This is Mathematica but is worth showing distinctly.  Result provides endian choice and other bases typically.


## XPL0


```XPL0
include c:\cxpl\codes;  \intrinsic 'code' declarations
string 0;               \use zero-terminated string convention

func Num2Str(N, B);     \Convert integer N to a numeric string in base B
int  N, B;
char S(32); int I;
[I:= 31;
S(31):= 0;              \terminate string
repeat  I:= I-1;
        N:= N/B;
        S(I):= rem(0) + (if rem(0)<=9 then ^0 else ^a-10);
until   N=0;
return @S(I);           \BEWARE! very temporary string space
];

func Str2Num(S, B);     \Convert numeric string S in base B to an integer
char S; int B;
int  I, N;
[I:= 0;  N:= 0;
while S(I) do
        [N:= N*B + S(I) - (if S(I)<=^9 then ^0 else ^a-10);  I:= I+1];
return N;
];

[Text(0, Num2Str(0, 10));               CrLf(0);
 Text(0, Num2Str(26, 16));              CrLf(0);
 Text(0, Num2Str($7FFF_FFFF, 2));       CrLf(0);
 IntOut(0, Str2Num("0100", 2));         CrLf(0);
 IntOut(0, Str2Num("1a", 16));          CrLf(0);
 IntOut(0, Str2Num("deadbeef", 16));    CrLf(0);
]
```


Output:

```txt

0
1a
1111111111111111111111111111111
4
26
-559038737

```



## zkl

The toInt(base) and toString(base) methods do this. base is 2..36

```zkl
(26).toString(16)  //--> "1a"
"1a".toInt(16)    //-->26
```

In addition, string format is able to convert to a base:

```zkl
"%x %,.2B".fmt(26,26) //-->"1a 1|1010"
```



[[Category:Radices]]
