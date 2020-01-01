+++
title = "Run-length encoding"
description = ""
date = 2019-07-29T16:11:07Z
aliases = []
[extra]
id = 4080
[taxonomies]
categories = []
tags = []
+++

{{Wikipedia|Run-length_encoding}}

{{task|Compression}}

[[Category: Encodings]]

Given a string containing uppercase characters (A-Z), compress repeated 'runs' of the same character by storing the length of that run, and provide a function to reverse the compression. The output can be anything, as long as you can recreate the input with it.

Example:

: Input:  <code>WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW</code>
: Output: <code>12W1B12W3B24W1B14W</code>

Note: the encoding step in the above example is the same as a step of the [[Look-and-say sequence]].


## Ada


```Ada
with Ada.Text_IO;        use Ada.Text_IO;
with Ada.Strings.Fixed;  use Ada.Strings.Fixed;
procedure Test_Run_Length_Encoding is
   function Encode (Data : String) return String is
   begin
      if Data'Length = 0 then
         return "";
      else
         declare
            Code  : constant Character := Data (Data'First);
            Index : Integer := Data'First + 1;
         begin
            while Index <= Data'Last and then Code = Data (Index) loop
               Index := Index + 1;
            end loop;
            declare
               Prefix : constant String := Integer'Image (Index - Data'First);
            begin
               return Prefix (2..Prefix'Last) & Code & Encode (Data (Index..Data'Last));
            end;
         end;
      end if;
   end Encode;
   function Decode (Data : String) return String is
   begin
      if Data'Length = 0 then
         return "";
      else
         declare
            Index : Integer := Data'First;
            Count : Natural := 0;
         begin
            while Index < Data'Last and then Data (Index) in '0'..'9' loop
               Count := Count * 10 + Character'Pos (Data (Index)) - Character'Pos ('0');
               Index := Index + 1;
            end loop;
            if Index > Data'First then
               return Count * Data (Index) & Decode (Data (Index + 1..Data'Last));
            else
               return Data;
            end if;
         end;
      end if;
   end Decode;
begin
   Put_Line (Encode ("WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW"));
   Put_Line (Decode ("12W1B12W3B24W1B14W"));
end Test_Run_Length_Encoding;
```

Sample output:

```txt

12W1B12W3B24W1B14W
WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW

```



## ALGOL 68

{{works with|ALGOL 68|Revision 1 - no extensions to language used}}

{{works with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-1.18.0/algol68g-1.18.0-9h.tiny.el5.centos.fc11.i386.rpm/download 1.18.0-9h.tiny]}}

{{works with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8-8d]}}

Note: The following uses iterators, eliminating the need of declaring arbitrarily large CHAR arrays for caching.

```algol68
STRING input := "WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW";
STRING output := "12W1B12W3B24W1B14W";

MODE YIELDCHAR = PROC(CHAR)VOID;
MODE GENCHAR = PROC(YIELDCHAR)VOID;

PROC gen char string = (REF STRING s, YIELDCHAR yield)VOID:
  FOR i FROM LWB s TO UPB s DO yield(s[i]) OD;

CO
# Note: The following 2 lines use currying. This not supported by ELLA ALGOL 68RS #
GENCHAR input seq = gen char string(input,),
        output seq = gen char string(output,);
END CO

GENCHAR
  input seq = (YIELDCHAR yield)VOID: gen char string(input, yield),
  output seq = (YIELDCHAR yield)VOID: gen char string(output, yield);

PROC gen encode = (GENCHAR gen char, YIELDCHAR yield)VOID: (
  INT count := 0;
  CHAR prev;
# FOR CHAR c IN # gen char( # ) DO ( #
##   (CHAR c)VOID: (
      IF count = 0 THEN
        count := 1;
        prev := c
      ELIF c NE prev THEN
        STRING str count := whole(count,0);
        gen char string(str count, yield); count := 1;
        yield(prev); prev := c
      ELSE
        count +:=1
      FI
# OD # ));
  IF count NE 0 THEN
    STRING str count := whole(count,0);
    gen char string(str count,yield);
    yield(prev)
  FI
);

STRING zero2nine = "0123456789";

PROC gen decode = (GENCHAR gen char, YIELDCHAR yield)VOID: (
  INT repeat := 0;
# FOR CHAR c IN # gen char( # ) DO ( #
##   (CHAR c)VOID: (
    IF char in string(c, LOC INT, zero2nine) THEN
      repeat := repeat*10 + ABS c - ABS "0"
    ELSE
      FOR i TO repeat DO yield(c) OD;
      repeat := 0
    FI
# OD #  ))
);

# iterate through input string #
print("Encode input: ");
# FOR CHAR c IN # gen encode(input seq, # ) DO ( #
##   (CHAR c)VOID:
    print(c)
# OD # );
print(new line);

# iterate through output string #
print("Decode output: ");
# FOR CHAR c IN # gen decode(output seq, # ) DO ( #
##   (CHAR c)VOID:
    print(c)
# OD # );
print(new line)
```

Output:

```txt

Encode input: 12W1B12W3B24W1B14W
Decode output: WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW

```



## APL


```APL
    ∇ ret←RLL rll;count
[1]   count←∣2-/((1,(2≠/rll),1)×⍳1+⍴rll)~0
[2]   ret←(⍕count,¨(1,2≠/rll)/rll)~' '
    ∇

```

Sample Output:

```txt

      RLL 'WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW'
12W1B12W3B24W1B14W

```



## AutoHotkey


```AutoHotkey
MsgBox % key := rle_encode("WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW")
MsgBox % rle_decode(key)

rle_encode(message)
{
  StringLeft, previous, message, 1
  StringRight, last, message, 1
  message .= Asc(Chr(last)+1)
  count = 0
  Loop, Parse, message
  {
    If (previous == A_LoopField)
      count +=1
    Else
    {
      output .= previous . count
      previous := A_LoopField
      count = 1
    }
  }
  Return output
}

rle_decode(message)
{
  pos = 1
  While, item := RegExMatch(message, "\D", char, pos)
  {
    digpos := RegExMatch(message, "\d+", dig, item)
    Loop, % dig
      output .= char
    pos := digpos
  }
  Return output
}
```



## AWK

{{works with|gawk}}

It works with "textual" input. Lines containing numbers are skipped, since they can't be represented in a not ambiguous way in this implementation (e.g. "11AA" would be encoded as "212A", which would be decoded as A repeated 212 times!)

'''Encoding'''


```awk
BEGIN {
 FS=""
}
/^[^0-9]+$/ {
  cp = $1; j = 0
  for(i=1; i <= NF; i++) {
    if ( $i == cp ) {
      j++;
    } else {
      printf("%d%c", j, cp)
      j = 1
    }
    cp = $i
  }
  printf("%d%c", j, cp)
}
```


'''Decoding'''


```awk
BEGIN {
  RS="[0-9]+[^0-9]"
  final = "";
}
{
  match(RT, /([0-9]+)([^0-9])/, r)
  for(i=0; i < int(r[1]); i++) {
    final = final r[2]
  }
}
END {
  print final
}
```



## BaCon


```qbasic
FUNCTION Rle_Encode$(txt$)

    LOCAL result$, c$ = LEFT$(txt$, 1)
    LOCAL total = 1

    FOR x = 2 TO LEN(txt$)
        IF c$ = MID$(txt$, x, 1) THEN
            INCR total
        ELSE
            result$ = result$ & STR$(total) & c$
            c$ = MID$(txt$, x, 1)
            total = 1
        END IF
    NEXT

    RETURN result$ & STR$(total) & c$

END FUNCTION

FUNCTION Rle_Decode$(txt$)

    LOCAL nr$, result$

    FOR x = 1 TO LEN(txt$)
        IF REGEX(MID$(txt$, x, 1), "[[:digit:]]") THEN
            nr$ = nr$ & MID$(txt$, x, 1)
        ELSE
            result$ = result$ & FILL$(VAL(nr$), ASC(MID$(txt$, x, 1)))
            nr$ = ""
        END IF
    NEXT

    RETURN result$

END FUNCTION

rle_data$ = "WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW"

PRINT "RLEData: ", rle_data$
encoded$ = Rle_Encode$(rle_data$)
PRINT "Encoded: ", encoded$
PRINT "Decoded: ", Rle_Decode$(encoded$)
```

{{out}}

```txt
RLEData: WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW
Encoded: 12W1B12W3B24W1B14W
Decoded: WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW

```



## BASIC


{{works with|QBasic}}

{{trans|PowerBASIC}}


```qbasic
DECLARE FUNCTION RLDecode$ (i AS STRING)
DECLARE FUNCTION RLEncode$ (i AS STRING)

DIM initial AS STRING, encoded AS STRING, decoded AS STRING

INPUT "Type something: ", initial
encoded = RLEncode(initial)
decoded = RLDecode(encoded)
PRINT initial
PRINT encoded
PRINT decoded

FUNCTION RLDecode$ (i AS STRING)
    DIM Loop0 AS LONG, rCount AS STRING, outP AS STRING, m AS STRING

    FOR Loop0 = 1 TO LEN(i)
        m = MID$(i, Loop0, 1)
        SELECT CASE m
            CASE "0" TO "9"
                rCount = rCount + m
            CASE ELSE
                IF LEN(rCount) THEN
                    outP = outP + STRING$(VAL(rCount), m)
                    rCount = ""
                ELSE
                    outP = outP + m
                END IF
        END SELECT
    NEXT
    RLDecode$ = outP
END FUNCTION

FUNCTION RLEncode$ (i AS STRING)
    DIM tmp1 AS STRING, tmp2 AS STRING, outP AS STRING
    DIM Loop0 AS LONG, rCount AS LONG

    tmp1 = MID$(i, 1, 1)
    tmp2 = tmp1
    rCount = 1

    FOR Loop0 = 2 TO LEN(i)
        tmp1 = MID$(i, Loop0, 1)
        IF tmp1 <> tmp2 THEN
            outP = outP + LTRIM$(RTRIM$(STR$(rCount))) + tmp2
            tmp2 = tmp1
            rCount = 1
        ELSE
            rCount = rCount + 1
        END IF
    NEXT

    outP = outP + LTRIM$(RTRIM$(STR$(rCount)))
    outP = outP + tmp2
    RLEncode$ = outP
END FUNCTION
```


Sample output (last one shows errors from using numbers in input string):

 Type something: aaaaeeeeeeiiiioooouuy
 aaaaeeeeeeiiiioooouuy
 4a6e4i4o2u1y
 aaaaeeeeeeiiiioooouuy

 Type something: My dog has fleas.
 My dog has fleas.
 1M1y1 1d1o1g1 1h1a1s1 1f1l1e1a1s1.
 My dog has fleas.

 Type something: 1r
 1r
 111r
 rrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrr


## BASIC256


```BASIC256

function FBString(lon, cad$)
	# Definimos la función String en BASIC256
	cadena$ = ""
	for a = 1 to lon
		cadena$ += cad$
	next a
	return cadena$
end function

function RLDecode(i$)
	rCount$ = "" : outP$ = ""

	for Loop0 = 1 to length(i$)
		m$ = mid(i$, Loop0, 1)
		begin case
			case m$ = "0"
				rCount$ += m$
			case m$ = "1"
				rCount$ += m$
			case m$ = "2"
				rCount$ += m$
			case m$ = "3"
				rCount$ += m$
			case m$ = "4"
				rCount$ += m$
			case m$ = "5"
				rCount$ += m$
			case m$ = "6"
				rCount$ += m$
			case m$ = "7"
				rCount$ += m$
			case m$ = "8"
				rCount$ += m$
			case m$ = "9"
				rCount$ += m$
			else
				if length(rCount$) then
					outP$ += FBString(int(rCount$), m$)
					rCount$ = ""
				else
					outP$ += m$
				end if
		end case
	next Loop0

	RLDecode = outP$
end function

function RLEncode(i$)
	outP$ = ""
	tmp1 = mid(i$, 1, 1)
	tmp2 = tmp1
	rCount = 1

	for Loop0 = 2 to length(i$)
		tmp1 = mid(i$, Loop0, 1)
		if tmp1 <> tmp2 then
			outP$ += string(rCount) + tmp2
			tmp2 = tmp1
			rCount = 1
		else
			rCount += 1
		end if
	next Loop0

	outP$ += replace(string(rCount)," ", "")
	outP$ += tmp2
	RLEncode = outP$
end function

input "Type something: ", initial
encoded$ = RLEncode(initial)
decoded$ = RLDecode(encoded$)
print initial
print encoded$
print decoded$
end

```

{{out}}
La salida es similar a la de [[#BASIC|BASIC]], mostrada arriba.


## BBC BASIC

The run counts are indicated by means of character codes in the range 131 to 255.

```bbcbasic
      input$ = "WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW"
      PRINT "Input:  " input$
      rle$ = FNencodeRLE(input$)
      output$ = FNdecodeRLE(rle$)
      PRINT "Output: " output$
      END

      DEF FNencodeRLE(text$)
      LOCAL n%, r%, c$, o$
      n% = 1
      WHILE n% <= LEN(text$)
        c$ = MID$(text$, n%, 1)
        n% += 1
        r% = 1
        WHILE c$ = MID$(text$, n%, 1) AND r% < 127
          r% += 1
          n% += 1
        ENDWHILE
        IF r% < 3 o$ += STRING$(r%, c$) ELSE o$ += CHR$(128+r%) + c$
      ENDWHILE
      = o$

      DEF FNdecodeRLE(rle$)
      LOCAL n%, c$, o$
      n% = 1
      WHILE n% <= LEN(rle$)
        c$ = MID$(rle$, n%, 1)
        n% += 1
        IF ASC(c$) > 128 THEN
          o$ += STRING$(ASC(c$)-128, MID$(rle$, n%, 1))
          n% += 1
        ELSE
          o$ += c$
        ENDIF
      ENDWHILE
      = o$
```



## Befunge

Not the same format as in the example,it puts "n\n" at the beginning so you can pipe the output back in and receive the input.
Pipe the output of the program-it's more reliable.
{{works with|CCBI|2.1}}

```Befunge
                    ~"y"- ~$         v
  <temp var for when char changes
format:
first,'n' and a newline.             :
a char then a                   v    _"n",v
number then a space continuously          9
example:                                  1
n                               >    v  ,+<
a5 b2
decoded:aaaaabb
the program is ended using   decoder
Ctrl-C on linux,or alt-f4
on windows.copy the output    >\v       encoder
of the program somewhere      ^_ $ v
to encode press y              :        > $11g:,  v
to decode pipe file in      >1-^   ~       v  +1\<
the output of the encoder   \  v<  $    ^     .\_^
starts with n,this is so    ^,:<\&~< _~:,>1>\:v>^
you can pipe it straight in              ^        <
                                              ~
the spaces seem to be a annoying thing        :
thanks to CCBI...if a interpreter dosen't     1
create them it's non-conforming and thus      1
the validity of this program is NOT affected  p-
                                              >^
--written by Gamemanj,for Rosettacode
```



## Bracmat


```bracmat
  ( run-length
  = character otherCharacter acc begin end
    .   :?acc
      & 0:?begin
      & @( !arg
         :   ?
             [!begin
             %@?character
             ?
             [?end
             (   (%@:~!character:?otherCharacter) ?
               & !acc !end+-1*!begin !character:?acc
               & !otherCharacter:?character
               & !end:?begin
               & ~`
             | &!acc !end+-1*!begin !character:?acc
             )
         )
      & str$!acc
  )
&   run-length$WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW

```


```txt
  12W1B12W3B24W1B14W
```



## Burlesque


```burlesque

"WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW"
=[{^^[~\/L[Sh}\m

```



## C

Encoder that can deal with byte streams.  Can encode/decode any byte values and any length with reasonable efficiency.  Also showing OO and polymophism with structs.

```c
#include <stdio.h>
#include <stdlib.h>

typedef struct stream_t stream_t, *stream;
struct stream_t {
	/* get function is supposed to return a byte value (0-255),
		or -1 to signify end of input */
	int (*get)(stream);
	/* put function does output, one byte at a time */
	int (*put)(stream, int);
};

/* next two structs inherit from stream_t */
typedef struct {
	int (*get)(stream);
	int (*put)(stream, int);
	char *string;
	int pos;
} string_stream;

typedef struct {
	int (*get)(stream);
	int (*put)(stream, int);
	FILE *fp;
} file_stream;

/* methods for above streams */
int sget(stream in)
{
	int c;
	string_stream* s = (string_stream*) in;
	c = (unsigned char)(s->string[s->pos]);
	if (c == '\0') return -1;
	s->pos++;
	return c;
}

int sput(stream out, int c)
{
	string_stream* s = (string_stream*) out;
	s->string[s->pos++] = (c == -1) ? '\0' : c;
	if (c == -1) s->pos = 0;
	return 0;
}

int file_put(stream out, int c)
{
	file_stream *f = (file_stream*) out;
	return fputc(c, f->fp);
}

/* helper function */
void output(stream out, unsigned char* buf, int len)
{
	int i;
	out->put(out, 128 + len);
	for (i = 0; i < len; i++)
		out->put(out, buf[i]);
}

/* Specification: encoded stream are unsigned bytes consisting of sequences.
 * First byte of each sequence is the length, followed by a number of bytes.
 * If length <=128, the next byte is to be repeated length times;
 * If length > 128, the next (length - 128) bytes are not repeated.
 * this is to improve efficiency for long non-repeating sequences.
 * This scheme can encode arbitrary byte values efficiently.
 * c.f. Adobe PDF spec RLE stream encoding (not exactly the same)
 */
void encode(stream in, stream out)
{
	unsigned char buf[256];
	int len = 0, repeat = 0, end = 0, c;
	int (*get)(stream) = in->get;
	int (*put)(stream, int) = out->put;

	while (!end) {
		end = ((c = get(in)) == -1);
		if (!end) {
			buf[len++] = c;
			if (len <= 1) continue;
		}

		if (repeat) {
			if (buf[len - 1] != buf[len - 2])
				repeat = 0;
			if (!repeat || len == 129 || end) {
				/* write out repeating bytes */
				put(out, end ? len : len - 1);
				put(out, buf[0]);
				buf[0] = buf[len - 1];
				len = 1;
			}
		} else {
			if (buf[len - 1] == buf[len - 2]) {
				repeat = 1;
				if (len > 2) {
					output(out, buf, len - 2);
					buf[0] = buf[1] = buf[len - 1];
					len = 2;
				}
				continue;
			}
			if (len == 128 || end) {
				output(out, buf, len);
				len = 0;
				repeat = 0;
			}
		}
	}
	put(out, -1);
}

void decode(stream in, stream out)
{
	int c, i, cnt;
	while (1) {
		c = in->get(in);
		if (c == -1) return;
		if (c > 128) {
			cnt = c - 128;
			for (i = 0; i < cnt; i++)
				out->put(out, in->get(in));
		} else {
			cnt = c;
			c = in->get(in);
			for (i = 0; i < cnt; i++)
				out->put(out, c);
		}
	}
}

int main()
{
	char buf[256];
	string_stream str_in = { sget, 0,
		"WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW", 0};
	string_stream str_out = { sget, sput, buf, 0 };
	file_stream file = { 0, file_put, stdout };

	/* encode from str_in to str_out */
	encode((stream)&str_in, (stream)&str_out);

	/* decode from str_out to file (stdout) */
	decode((stream)&str_out, (stream)&file);

	return 0;
}
```


See [[Run-length encoding/C]]


## C++


```cpp
#include <algorithm>
#include <array>
#include <iterator>
#include <limits>
#include <tuple>

namespace detail_ {

// For constexpr digit<->number conversions.
constexpr auto digits = std::array{'0','1','2','3','4','5','6','7','8','9'};

// Helper function to encode a run-length.
template <typename OutputIterator>
constexpr auto encode_run_length(std::size_t n, OutputIterator out)
{
    constexpr auto base = digits.size();

    // Determine the number of digits needed.
    auto const num_digits = [base](auto n)
    {
        auto d = std::size_t{1};
        while ((n /= digits.size()))
            ++d;
        return d;
    }(n);

    // Helper lambda to raise the base to an integer power.
    auto base_power = [base](auto n)
    {
        auto res = decltype(base){1};
        for (auto i = decltype(n){1}; i < n; ++i)
            res *= base;
        return res;
    };

    // From the most significant digit to the least, output the digit.
    for (auto i = decltype(num_digits){0}; i < num_digits; ++i)
        *out++ = digits[(n / base_power(num_digits - i)) % base];

    return out;
}

// Helper function to decode a run-length.
// As of C++20, this can be constexpr, because std::find() is constexpr.
// Before C++20, it can be constexpr by emulating std::find().
template <typename InputIterator>
auto decode_run_length(InputIterator first, InputIterator last)
{
    auto count = std::size_t{0};

    while (first != last)
    {
        // If the next input character is not a digit, we're done.
        auto const p = std::find(digits.begin(), digits.end(), *first);
        if (p == digits.end())
            break;

        // Convert the digit to a number, and append it to the size.
        count *= digits.size();
        count += std::distance(digits.begin(), p);

        // Move on to the next input character.
        ++first;
    }

    return std::tuple{count, first};
}

} // namespace detail_

template <typename InputIterator, typename OutputIterator>
constexpr auto encode(InputIterator first, InputIterator last, OutputIterator out)
{
    while (first != last)
    {
        // Read the next value.
        auto const value = *first++;

        // Increase the count as long as the next value is the same.
        auto count = std::size_t{1};
        while (first != last && *first == value)
        {
            ++count;
            ++first;
        }

        // Write the value and its run length.
        out = detail_::encode_run_length(count, out);
        *out++ = value;
    }

    return out;
}

// As of C++20, this can be constexpr, because std::find() and
// std::fill_n() are constexpr (and decode_run_length() can be
// constexpr, too).
// Before C++20, it can be constexpr by emulating std::find() and
// std::fill_n().
template <typename InputIterator, typename OutputIterator>
auto decode(InputIterator first, InputIterator last, OutputIterator out)
{
    while (first != last)
    {
        using detail_::digits;

        // Assume a run-length of 1, then try to decode the actual
        // run-length, if any.
        auto count = std::size_t{1};
        if (std::find(digits.begin(), digits.end(), *first) != digits.end())
            std::tie(count, first) = detail_::decode_run_length(first, last);

        // Write the run.
        out = std::fill_n(out, count, *first++);
    }

    return out;
}

template <typename Range, typename OutputIterator>
constexpr auto encode(Range&& range, OutputIterator out)
{
    using std::begin;
    using std::end;

    return encode(begin(range), end(range), out);
}

template <typename Range, typename OutputIterator>
auto decode(Range&& range, OutputIterator out)
{
    using std::begin;
    using std::end;

    return decode(begin(range), end(range), out);
}

// Sample application and checking ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#include <iostream>
#include <string_view>

int main()
{
    using namespace std::literals;

    constexpr auto test_string = "WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW"sv;

    std::cout << "Input:  \"" << test_string << "\"\n";
    std::cout << "Output: \"";
    // No need for a temporary string - can encode directly to cout.
    encode(test_string, std::ostreambuf_iterator<char>{std::cout});
    std::cout << "\"\n";

    auto encoded_str = std::string{};
    auto decoded_str = std::string{};
    encode(test_string, std::back_inserter(encoded_str));
    decode(encoded_str, std::back_inserter(decoded_str));

    std::cout.setf(std::cout.boolalpha);
    std::cout << "Round trip works: " << (test_string == decoded_str) << '\n';
}
```


{{libheader|boost}}

```cpp
#include <iostream>
#include <string>
#include <sstream>
#include <boost/regex.hpp>
#include <cstdlib>

std::string encode ( const std::string & ) ;
std::string decode ( const std::string & ) ;

int main( ) {
   std::string to_encode ( "WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW" ) ;
   std::cout << to_encode << " encoded:" << std::endl ;
   std::string encoded ( encode ( to_encode ) ) ;
   std::cout << encoded << std::endl ;
   std::string decoded ( decode( encoded ) ) ;
   std::cout << "Decoded again:\n" ;
   std::cout << decoded << std::endl ;
   if ( to_encode == decoded )
      std::cout << "It must have worked!\n" ;
   return 0 ;
}

std::string encode( const std::string & to_encode ) {
   std::string::size_type found = 0 , nextfound = 0 ;
   std::ostringstream oss ;
   nextfound = to_encode.find_first_not_of( to_encode[ found ] , found ) ;
   while ( nextfound != std::string::npos ) {
      oss << nextfound - found ;
      oss << to_encode[ found ] ;
      found = nextfound ;
      nextfound = to_encode.find_first_not_of( to_encode[ found ] , found ) ;
   }
   //since we must not discard the last characters we add them at the end of the string
   std::string rest ( to_encode.substr( found ) ) ;//last run of characters starts at position found
   oss << rest.length( ) << to_encode[ found ] ;
   return oss.str( ) ;
}

std::string decode ( const std::string & to_decode ) {
   boost::regex e ( "(\\d+)(\\w)" ) ;
   boost::match_results<std::string::const_iterator> matches ;
   std::ostringstream oss ;
   std::string::const_iterator start = to_decode.begin( ) , end = to_decode.end( ) ;
   while ( boost::regex_search ( start , end , matches , e ) ) {
      std::string numberstring ( matches[ 1 ].first , matches[ 1 ].second ) ;
      int number = atoi( numberstring.c_str( ) ) ;
      std::string character ( matches[ 2 ].first , matches[ 2 ].second ) ;
      for ( int i = 0 ; i < number ; i++ )
	 oss << character ;
      start = matches[ 2 ].second ;
   }
   return oss.str( ) ;
}
```


## C#

###  Linq

<!--Martin Freedman 22/02/2018-->

```c#
using System.Collections.Generic;
using System.Linq;
using static System.Console;
using static System.Linq.Enumerable;

namespace RunLengthEncoding
{
    static class Program
    {
          public static string Encode(string input) => input.Length ==0 ? "" : input.Skip(1)
            .Aggregate((t:input[0].ToString(),o:Empty<string>()),
               (a,c)=>a.t[0]==c ? (a.t+c,a.o) : (c.ToString(),a.o.Append(a.t)),
               a=>a.o.Append(a.t).Select(p => (key: p.Length, chr: p[0])))
            .Select(p=> $"{p.key}{p.chr}")
            .StringConcat();

        public static string Decode(string input) => input
            .Aggregate((t: "", o: Empty<string>()), (a, c) => !char.IsDigit(c) ? ("", a.o.Append(a.t+c)) : (a.t + c,a.o)).o
            .Select(p => new string(p.Last(), int.Parse(string.Concat(p.Where(char.IsDigit)))))
            .StringConcat();

        private static string StringConcat(this IEnumerable<string> seq) => string.Concat(seq);

        public static void Main(string[] args)
        {
            const string  raw = "WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW";
            const string encoded = "12W1B12W3B24W1B14W";

            WriteLine($"raw = {raw}");
            WriteLine($"encoded = {encoded}");
            WriteLine($"Encode(raw) = encoded = {Encode(raw)}");
            WriteLine($"Decode(encode) = {Decode(encoded)}");
            WriteLine($"Decode(Encode(raw)) = {Decode(Encode(raw)) == raw}");
            ReadLine();
        }
    }
}
```

Output:

```txt
raw = WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW
encoded = 12W1B12W3B24W1B14W
Encode(raw) = encoded = 12W1B12W3B24W1B14W
Decode(encode) = WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW
Decode(Encode(raw)) = True

```


Many solutions do not follow the suggested output guideline in the challenge (not helped by its wording), instead producing a list of tuples or equivalent. This is much simpler (especially for decode) and the following provides an equivalent of those (IMHO deficient) solutions, to make comparisons easier.

```c#
using System.Collections.Generic;
using System.Linq;
using static System.Console;
namespace RunLengthEncoding
{
    static class Program
    {
         public static string Encode(string input) => input.Length ==0 ? "" : input.Skip(1)
            .Aggregate((t:input[0].ToString(),o:Empty<string>()),
               (a,c)=>a.t[0]==c ? (a.t+c,a.o) : (c.ToString(),a.o.Append(a.t)),
               a=>a.o.Append(a.t).Select(p => (key: p.Length, chr: p[0])));

        public static string Decode(IEnumerable<(int i , char c)> input) =>
            string.Concat(input.Select(t => new string(t.c, t.i)));

        public static void Main(string[] args)
        {
            const string  raw = "WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW";
            var encoded = new[] { (12, 'W'), (1, 'B'), (12, 'W'), (3, 'B'), (24, 'W'), (1, 'B'), (14, 'W') };

            WriteLine($"raw = {raw}");
            WriteLine($"Encode(raw) = encoded = {Encode(raw).TupleListToString()}");
            WriteLine($"Decode(encoded) = {Decode(encoded)}");
            WriteLine($"Decode(Encode(raw)) = {Decode(Encode(raw)) == raw}");
            ReadLine();
        }
        private static string TupleListToString(this IEnumerable<(int i, char c)> list) =>
            string.Join(",", list.Select(t => $"[{t.i},{t.c}]"));
    }
}
```

Output:

```txt
raw = WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW
Encode(raw) = encoded = [12,W],[1,B],[12,W],[3,B],[24,W],[1,B],[14,W]
Decode(encoded) = WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW
Decode(Encode(raw)) = True

```

Stringbuilder version. Might be more performant but mixes output formatting with encoding/decoding logic.
<!--Martin Freedman 22/02/2018-->

```c#
using System.Collections.Generic;
using System.Linq;
using static System.Console;
using static System.Text;

namespace RunLengthEncoding
{
    static class Program
    {
         public static string Encode(string input) => input.Length == 0 ? "" : input.Skip(1)
          .Aggregate((len: 1, chr: input[0], sb: new StringBuilder()),
             (a, c) => a.chr == c ? (a.len + 1, a.chr, a.sb)
                                  : (1, c, a.sb.Append(a.len).Append(a.chr))),
             a => a.sb.Append(a.len).Append(a.chr)))
          .ToString();

         public static string Decode(string input) => input
           .Aggregate((t: "", sb: new StringBuilder()),
             (a, c) => !char.IsDigit(c) ? ("", a.sb.Append(new string(c, int.Parse(a.t))))
                                        : (a.t + c, a.sb))
           .sb.ToString();

        public static void Main(string[] args)
        {
            const string  raw = "WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW";
            const string encoded = "12W1B12W3B24W1B14W";

            WriteLine($"raw = {raw}");
            WriteLine($"encoded = {encoded}");
            WriteLine($"Encode(raw) = encoded = {Encode(raw)}");
            WriteLine($"Decode(encode) = {Decode(encoded)}");
            WriteLine($"Decode(Encode(raw)) = {Decode(Encode(raw)) == raw}");
            ReadLine();
        }
    }
}
```

Output:

```txt
raw = WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW
encoded = 12W1B12W3B24W1B14W
Encode(raw) = encoded = 12W1B12W3B24W1B14W
Decode(encode) = WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW
Decode(Encode(raw)) = True

```



###  Imperative

This example only works if there are no digits in the string to be encoded and then decoded.


```c#
       public static void Main(string[] args)
       {
           string input = "WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW";
           Console.WriteLine(Encode(input));//Outputs: 12W1B12W3B24W1B14W
           Console.WriteLine(Decode(Encode(input)));//Outputs: WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW
           Console.ReadLine();
       }
       public static string Encode(string s)
       {
           StringBuilder sb = new StringBuilder();
           int count = 1;
           char current =s[0];
           for(int i = 1; i < s.Length;i++)
           {
               if (current == s[i])
               {
                   count++;
               }
               else
               {
                   sb.AppendFormat("{0}{1}", count, current);
                   count = 1;
                   current = s[i];
               }
           }
           sb.AppendFormat("{0}{1}", count, current);
           return sb.ToString();
       }
       public static string Decode(string s)
       {
           string a = "";
           int count = 0;
           StringBuilder sb = new StringBuilder();
           char current = char.MinValue;
           for(int i = 0; i < s.Length; i++)
           {
               current = s[i];
               if (char.IsDigit(current))
                   a += current;
               else
               {
                   count = int.Parse(a);
                   a = "";
                   for (int j = 0; j < count; j++)
                       sb.Append(current);
               }
           }
           return sb.ToString();
       }
```



###  RegEx

Somewhat shorter, using Regex.Replace with MatchEvaluator (using C#2 syntax only):

```c#
using System;
using System.Text.RegularExpressions;

public class Program
{
    private delegate void fOk(bool ok, string message);

    public static int Main(string[] args)
    {
        const string raw = "WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW";
        const string code = "12W1B12W3B24W1B14W";

        fOk Ok = delegate(bool ok, string message)
        {
            Console.WriteLine("{0}: {1}", ok ? "ok" : "not ok", message);
        };
        Ok(code.Equals(Encode(raw)), "Encode");
        Ok(raw.Equals(Decode(code)), "Decode");
        return 0;
    }

    public static string Encode(string input)
    {
        return Regex.Replace(input, @"(.)\1*", delegate(Match m)
        {
            return string.Concat(m.Value.Length, m.Groups[1].Value);
        });
    }

    public static string Decode(string input)
    {
        return Regex.Replace(input, @"(\d+)(\D)", delegate(Match m)
        {
            return new string(m.Groups[2].Value[0], int.Parse(m.Groups[1].Value));
        });
    }
}
```



## Ceylon


```ceylon
shared void run() {

    "Takes a string such as aaaabbbbbbcc and returns 4a6b2c"
    String compress(String string) {
        if (exists firstChar = string.first) {
            if (exists index = string.firstIndexWhere((char) => char != firstChar)) {
                return "``index````firstChar````compress(string[index...])``";
            }
            else {
                return "``string.size````firstChar``";
            }
        }
        else {
            return "";
        }
    }

    "Takes a string such as 4a6b2c and returns aaaabbbbbbcc"
    String decompress(String string) =>
            let (runs = string.split(Character.letter, false).paired)
    		"".join {
        		for ([length, char] in runs)
        		if (is Integer int = Integer.parse(length))
        		char.repeat(int)
        	};

    assert (compress("aaaabbbbbaa") == "4a5b2a");
    assert (decompress("4a6b2c") == "aaaabbbbbbcc");
    assert (compress("WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW") == "12W1B12W3B24W1B14W");
    assert (decompress("24a") == "aaaaaaaaaaaaaaaaaaaaaaaa");
}
```



## Clojure


```clojure
(defn compress [s]
  (->> (partition-by identity s) (mapcat (juxt count first)) (apply str)))

(defn extract [s]
  (->> (re-seq #"(\d+)([A-Z])" s)
       (mapcat (fn [[_ n ch]] (repeat (Integer/parseInt n) ch)))
       (apply str)))
```



## COBOL

{{works with|GNU Cobol|2.0}}

```cobol>       >
SOURCE FREE
IDENTIFICATION DIVISION.
PROGRAM-ID. run-length-encoding.

ENVIRONMENT DIVISION.
CONFIGURATION SECTION.
REPOSITORY.
    FUNCTION encode
    FUNCTION decode
    .
DATA DIVISION.
WORKING-STORAGE SECTION.
01  input-str                           PIC A(100).
01  encoded                             PIC X(200).
01  decoded                             PIC X(200).

PROCEDURE DIVISION.
    ACCEPT input-str
    MOVE encode(FUNCTION TRIM(input-str)) TO encoded
    DISPLAY "Encoded: " FUNCTION TRIM(encoded)
    DISPLAY "Decoded: " FUNCTION TRIM(decode(encoded))
    .
END PROGRAM run-length-encoding.


IDENTIFICATION DIVISION.
FUNCTION-ID. encode.

DATA DIVISION.
LOCAL-STORAGE SECTION.
01  str-len                             PIC 9(3) COMP.

01  i                                   PIC 9(3) COMP.

01  current-char                        PIC A.

01  num-chars                           PIC 9(3) COMP.
01  num-chars-disp                      PIC Z(3).

01  encoded-pos                         PIC 9(3) COMP VALUE 1.

LINKAGE SECTION.
01  str                                 PIC X ANY LENGTH.

01  encoded                             PIC X(200).

PROCEDURE DIVISION USING str RETURNING encoded.
    MOVE FUNCTION LENGTH(str) TO str-len
    MOVE str (1:1) TO current-char
    MOVE 1 TO num-chars
    PERFORM VARYING i FROM 2 BY 1 UNTIL i > str-len
        IF str (i:1) <> current-char
            CALL "add-num-chars" USING encoded, encoded-pos,
                CONTENT current-char, num-chars

            MOVE str (i:1) TO current-char
            MOVE 1 TO num-chars
        ELSE
            ADD 1 TO num-chars
        END-IF
    END-PERFORM

    CALL "add-num-chars" USING encoded, encoded-pos, CONTENT current-char,
        num-chars
    .
END FUNCTION encode.

IDENTIFICATION DIVISION.
PROGRAM-ID. add-num-chars.

DATA DIVISION.
WORKING-STORAGE SECTION.
01  num-chars-disp                      PIC Z(3).

LINKAGE SECTION.
01  str                                 PIC X(200).

01  current-pos                         PIC 9(3) COMP.

01  char-to-encode                      PIC X.

01  num-chars                           PIC 9(3) COMP.

PROCEDURE DIVISION USING str, current-pos, char-to-encode, num-chars.
    MOVE num-chars TO num-chars-disp
    MOVE FUNCTION TRIM(num-chars-disp) TO str (current-pos:3)
    ADD FUNCTION LENGTH(FUNCTION TRIM(num-chars-disp)) TO current-pos
    MOVE char-to-encode TO str (current-pos:1)
    ADD 1 TO current-pos
    .
END PROGRAM add-num-chars.


IDENTIFICATION DIVISION.
FUNCTION-ID. decode.

DATA DIVISION.
LOCAL-STORAGE SECTION.
01  encoded-pos                         PIC 9(3) COMP VALUE 1.
01  decoded-pos                         PIC 9(3) COMP VALUE 1.

01  num-of-char                         PIC 9(3) COMP VALUE 0.

LINKAGE SECTION.
01  encoded                             PIC X(200).

01  decoded                             PIC X(100).

PROCEDURE DIVISION USING encoded RETURNING decoded.
    PERFORM VARYING encoded-pos FROM 1 BY 1
            UNTIL encoded (encoded-pos:2) = SPACES OR encoded-pos > 200
        IF encoded (encoded-pos:1) IS NUMERIC
            COMPUTE num-of-char = num-of-char * 10
                + FUNCTION NUMVAL(encoded (encoded-pos:1))
        ELSE
            PERFORM UNTIL num-of-char = 0
                MOVE encoded (encoded-pos:1) TO decoded (decoded-pos:1)
                ADD 1 TO decoded-pos
                SUBTRACT 1 FROM num-of-char
            END-PERFORM
        END-IF
    END-PERFORM
    .
END FUNCTION decode.
```


{{out}}

```txt

WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW
Encoded: 12W1B12W3B24W1B14W
Decoded: WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW

```



## CoffeeScript



```coffeescript
encode = (str) ->
    str.replace /(.)\1*/g, (w) ->
        w[0] + w.length

decode = (str) ->
    str.replace /(.)(\d+)/g, (m,w,n) ->
        new Array(+n+1).join(w)

console.log s = "WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW"
console.log encode s
console.log decode encode s
```



```txt

WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW
W12B1W12B3W24B1W14
WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW

```


The following version encodes the number of ocurrences as an unicode character. You can change the way it looks by rotating the offset.


```coffeescript
encode = (str, offset = 75) ->
    str.replace /(.)\1*/g, (w) ->
        w[0] + String.fromCharCode(offset+w.length)

decode = (str, offset = 75) ->
    str.split('').map((w,i) ->
        if not (i%2) then w else new Array(+w.charCodeAt(0)-offset).join(str[i-1])
    ).join('')
```



```txt

> encode "WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW"
WWBLWWBNWcBLWY
> encode "WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW", 1200
WҼBұWҼBҳWӈBұWҾ
> encode "WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW", 5200
WᑜBᑑWᑜBᑓWᑨBᑑWᑞ

```



## Common Lisp


```lisp
(defun group-similar (sequence &key (test 'eql))
  (loop for x in (rest sequence)
        with temp = (subseq sequence 0 1)
        if (funcall test (first temp) x)
          do (push x temp)
        else
          collect temp
          and do (setf temp (list x))))

(defun run-length-encode (sequence)
  (mapcar (lambda (group) (list (first group) (length group)))
          (group-similar (coerce sequence 'list))))

(defun run-length-decode (sequence)
  (reduce (lambda (s1 s2) (concatenate 'simple-string s1 s2))
          (mapcar (lambda (elem)
                    (make-string (second elem)
                                 :initial-element
                                 (first elem)))
                  sequence)))

(run-length-encode "WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW")
(run-length-decode '((#\W 12) (#\B 1) (#\W 12) (#\B 3) (#\W 24) (#\B 1)))
```



## D


### Short Functional Version


```d
import std.algorithm, std.array;

alias encode = group;

auto decode(Group!("a == b", string) enc) {
    return enc.map!(t => [t[0]].replicate(t[1])).join;
}

void main() {
    immutable s = "WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWW" ~
                  "WWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW";
    assert(s.encode.decode.equal(s));
}
```



### Basic Imperative Version


```d
import std.stdio, std.array, std.conv;

// Similar to the 'look and say' function.
string encode(in string input) pure nothrow @safe {
    if (input.empty)
        return input;
    char last = input[$ - 1];
    string output;
    int count;

    foreach_reverse (immutable c; input) {
        if (c == last) {
            count++;
        } else {
            output = count.text ~ last ~ output;
            count = 1;
            last = c;
        }
    }

    return count.text ~ last ~ output;
}

string decode(in string input) pure /*@safe*/ {
    string i, result;

    foreach (immutable c; input)
        switch (c) {
            case '0': .. case '9':
                i ~= c;
                break;
            case 'A': .. case 'Z':
                if (i.empty)
                    throw new Exception("Can not repeat a letter " ~
                        "without a number of repetitions");
                result ~= [c].replicate(i.to!int);
                i.length = 0;
                break;
            default:
                throw new Exception("'" ~ c ~ "' is not alphanumeric");
        }

    return result;
}

void main() {
    immutable txt = "WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWW" ~
                    "WWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW";
    writeln("Input: ", txt);
    immutable encoded = txt.encode;
    writeln("Encoded: ", encoded);
    assert(txt == encoded.decode);
}
```

{{out}}

```txt
Input: WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW
Encoded: 12W1B12W3B24W1B14W
```



### UTF String Version

D's native string is utf-encoded. This version works for utf string, and uses a [[Variable-length_quantity|Variable-length Quantity]] [[Variable-length_quantity#D|module]].


```d
import std.stdio, std.conv, std.utf, std.array;
import vlq;

struct RLE { // for utf string
    ubyte[] encoded;

    RLE encode(const string s) {
        validate(s); // check if s is well-formed utf, throw if not
        encoded.length = 0; // reset
        if (s.length == 0) return this; // empty string
        string last;
        VLQ count;
        for (int i = 0; i < s.length; ) {
            auto k = s.stride(i);
            auto ucode = cast(string)s[i .. i + k];
            if (i == 0) last = ucode;
            if (ucode == last)
                count++;
            else {
                encoded ~= count.toVLQ ~ cast(ubyte[])last;
                last = ucode;
                count = 1;
            }
            i += k;
        }
        encoded ~= VLQ(count).toVLQ ~ cast(ubyte[])last;
        return this;
    }

    int opApply(int delegate(ref ulong c, ref string u) dg) {
        VLQ count;
        string ucode;

        for (int i = 0; i < encoded.length; ) {
            auto k = count.extract(encoded[i .. $]);
            i += k;
            if (i >= encoded.length)
                throw new Exception("not valid encoded string");
            k = stride(cast(string) encoded[i .. $], 0);
            if (k == 0xff) // not valid utf code point
                throw new Exception("not valid encoded string");
            ucode = cast(string)encoded[i .. i + k].dup;
            dg(count.value, ucode);
            i += k;
        }

        return 0;
    }

    string toString() {
        string res;
        foreach (ref i, s ; this)
            if (indexOf("0123456789#", s) == -1)
                res ~= text(i) ~ s;
            else
                res ~= text(i) ~ '#' ~ s;
        return res;
    }

    string decode() {
        string res;
        foreach (ref i, s; this)
            res ~= replicate(s, cast(uint)i);
        return res;
    }
}

void main() {
    RLE r;
    auto s = "尋尋覓覓冷冷清清淒淒慘慘戚戚\nWWWWWWWWWWWWBWWWWWWWWWWW" ~
             "WBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW\n" ~
             "11#222##333";
    auto f = File("display.txt", "w");
    f.writeln(s);
    r.encode(s);
    f.writefln("-----\n%s\n-----\n%s", r, r.decode());
    auto sEncoded = RLE.init.encode(s).encoded ;
    assert(s == RLE(sEncoded).decode(), "Not work");
}
```


output from "display.txt":

```txt
尋尋覓覓冷冷清清淒淒慘慘戚戚
WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW
11#222##333
-----
2尋2覓2冷2清2淒2慘2戚1
12W1B12W3B24W1B14W1
2#11##3#22##3#3
-----
尋尋覓覓冷冷清清淒淒慘慘戚戚
WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW
11#222##333
```


'''NOTE:''' some characters in this section use Chinese font.


### UTF String Version with Regular Expression

{{trans|Python}}

The code looks more complex than the third Python version because this also handles digits by escaping them with #.

```d
import std.stdio, std.conv, std.array, std.regex, std.utf,
       std.algorithm;

string reEncode(string s) {
    validate(s); // Throw if it's not a well-formed UTF string
    static string rep(Captures!string m) {
        auto c = canFind("0123456789#", m[1]) ? "#" ~ m[1] : m[1];
        return text(m.hit.length / m[1].length) ~ c;
    }
    return std.regex.replace!rep(s, regex(`(.|[\n\r\f])\1*`, "g"));
}


string reDecode(string s) {
    validate(s); // Throw if it's not a well-formed UTF string
    static string rep(Captures!string m) {
        string c = m[2];
        if (c.length > 1 && c[0] == '#')
            c = c[1 .. $];
        return replicate(c, to!int(m[1]));
    }
    auto r=regex(`(\d+)(#[0123456789#]|[\n\r\f]|[^0123456789#\n\r\f]+)`
                 , "g");
    return std.regex.replace!rep(s, r);
}

void main() {
    auto s = "尋尋覓覓冷冷清清淒淒慘慘戚戚\nWWWWWWWWWWWWBWWWWWWWWWWW" ~
             "WBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW\n" ~
             "11#222##333";
    assert(s == reDecode(reEncode(s)));
}
```


=={{header|Déjà Vu}}==

```dejavu
rle:
	if not dup:
		drop
		return []

	swap ]

	local :source chars
	pop-from source
	1
	for c in source:
		if = c over:
			++
		else:
			1 c &
	&
	return [

rld:
	)
	for pair in swap:
		repeat &< pair:
			&> pair
	concat(


rle "WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW"
!. dup
!. rld
```

{{out}}

```txt
[ & 12 "W" & 1 "B" & 12 "W" & 3 "B" & 24 "W" & 1 "B" & 14 "W" ]
"WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW"
```



## E



```e
def rle(string) {
  var seen := null
  var count := 0
  var result := []
  def put() {
    if (seen != null) {
      result with= [count, seen]
    }
  }
  for ch in string {
    if (ch != seen) {
      put()
      seen := ch
      count := 0
    }
    count += 1
  }
  put()
  return result
}

def unrle(coded) {
  var result := ""
  for [count, ch] in coded {
    result += E.toString(ch) * count
  }
  return result
}
```



```e
? rle("WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW")
# value: [[12, 'W'], [1, 'B'], [12, 'W'], [3, 'B'], [24, 'W'], [1, 'B'], [14, 'W']]

? unrle(rle("WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW"))
# value: "WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW"
```


## Elena

ELENA 4.x :

```elena
import system'text;
import system'routines;
import extensions;
import extensions'text;

singleton compressor
{
    string compress(string s)
    {
        auto  tb := new TextBuilder();
        int count := 0;
        char current := s[0];
        s.forEach:(ch)
        {
            if (ch == current)
            {
                count += 1
            }
            else
            {
                tb.writeFormatted("{0}{1}",count,current);
                count := 1;
                current := ch
            }
        };

        tb.writeFormatted("{0}{1}",count,current);

        ^ tb
    }

    string decompress(string s)
    {
        auto tb := new TextBuilder();
        char current := $0;
        var a := new StringWriter();
        s.forEach:(ch)
        {
            current := ch;
            if (current.isDigit())
            {
                a.append(ch)
            }
            else
            {
                int count := a.toInt();
                a.clear();

                tb.fill(current,count)
            }
        };

        ^ tb
    }
}

public program()
{
    var s := "WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW";

    s := compressor.compress(s);
    console.printLine(s);

    s := compressor.decompress(s);
    console.printLine(s)
}
```

{{out}}

```txt

12W1B12W3B24W1B14W
WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW

```



## Elixir


```elixir
defmodule Run_length do
  def encode(str) when is_bitstring(str) do
    to_char_list(str) |> encode |> to_string
  end
  def encode(list) when is_list(list) do
    Enum.chunk_by(list, &(&1))
    |> Enum.flat_map(fn chars -> to_char_list(length(chars)) ++ [hd(chars)] end)
  end

  def decode(str) when is_bitstring(str) do
    Regex.scan(~r/(\d+)(.)/, str)
    |> Enum.map_join(fn [_,n,c] -> String.duplicate(c, String.to_integer(n)) end)
  end
  def decode(list) when is_list(list) do
    to_string(list) |> decode |> to_char_list
  end
end

text = [ string:    "WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW",
         char_list: 'WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW' ]

Enum.each(text, fn {type, txt} ->
  IO.puts type
  txt                  |> IO.inspect
  |> Run_length.encode |> IO.inspect
  |> Run_length.decode |> IO.inspect
end)
```


{{out}}

```txt

string
"WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW"
"12W1B12W3B24W1B14W"
"WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW"
char_list
'WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW'
'12W1B12W3B24W1B14W'
'WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW'
```




## Emacs Lisp


```lisp

(defun run-length-encode (str_arg)
  "Return the run length encoding of a string argument STR_ARG."

    (let ((dalist (string-to-list str_arg)) (letters) (frequency))

      (dolist (element dalist)
	(if (not (equal (car letters) element))
	    (progn
	      (setq letters (cons element letters))
	      (setq frequency (cons 1 frequency)))
	  (setq frequency (cons (+ (car frequency ) 1) (cdr frequency)))))

      (apply 'concat
	     (reverse
	      (mapcar*
	       (lambda (x y) (concat (number-to-string x)
				     (char-to-string y)))
	       frequency letters)))))

```



## Erlang


A single-threaded/process version with a simple set of unit test.


```erlang
-module(rle).

-export([encode/1,decode/1]).

-include_lib("eunit/include/eunit.hrl").

encode(S) ->
    doEncode(string:substr(S, 2), string:substr(S, 1, 1), 1, []).

doEncode([], CurrChar, Count, R) ->
    R ++ integer_to_list(Count) ++ CurrChar;
doEncode(S, CurrChar, Count, R) ->
    NextChar = string:substr(S, 1, 1),
    if
        NextChar == CurrChar ->
            doEncode(string:substr(S, 2), CurrChar, Count + 1, R);
        true ->
            doEncode(string:substr(S, 2), NextChar, 1,
                R ++ integer_to_list(Count) ++ CurrChar)
    end.

decode(S) ->
    doDecode(string:substr(S, 2), string:substr(S, 1, 1), []).

doDecode([], _, R) ->
    R;
doDecode(S, CurrString, R) ->
    NextChar = string:substr(S, 1, 1),
    IsInt = erlang:is_integer(catch(erlang:list_to_integer(NextChar))),
    if
        IsInt ->
            doDecode(string:substr(S, 2), CurrString ++ NextChar, R);
        true ->
            doDecode(string:substr(S, 2), [],
                R ++ string:copies(NextChar, list_to_integer(CurrString)))
    end.

rle_test_() ->
    PreEncoded =
        "WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW",
    Expected = "12W1B12W3B24W1B14W",
    [
        ?_assert(encode(PreEncoded) =:= Expected),
        ?_assert(decode(Expected) =:= PreEncoded),
        ?_assert(decode(encode(PreEncoded)) =:= PreEncoded)
    ].
```


A version that works on character lists:


```erlang

-module(rle).

-export([encode/1, decode/1]).

encode(L) -> encode(L, []).
encode([], Acc) -> {rle, lists:reverse(Acc)};
encode([H|T], []) ->
    encode(T, [{1, H}]);
encode([H|T], [{Count, Char}|AT]) ->
    if
        H =:= Char ->
            encode(T, [{Count + 1, Char}|AT]);
        true ->
            encode(T, [{1, H}|[{Count, Char}|AT]])
    end.

decode({rle, L}) -> lists:append(lists:reverse(decode(L, []))).
decode([], Acc) -> Acc;
decode([{Count, Char}|T], Acc) ->
    decode(T, [[Char || _ <- lists:seq(1, Count)]|Acc]).

```



## Euphoria


```euphoria
include misc.e

function encode(sequence s)
    sequence out
    integer prev_char,count
    if length(s) = 0 then
        return {}
    end if
    out = {}
    prev_char = s[1]
    count = 1
    for i = 2 to length(s) do
        if s[i] != prev_char then
            out &= {count,prev_char}
            prev_char = s[i]
            count = 1
        else
            count += 1
        end if
    end for
    out &= {count,prev_char}
    return out
end function

function decode(sequence s)
    sequence out
    out = {}
    for i = 1 to length(s) by 2 do
        out &= repeat(s[i+1],s[i])
    end for
    return out
end function

sequence s
s = encode("WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW")
pretty_print(1,s,{3})
puts(1,'\n')
puts(1,decode(s))
```


Output:

```txt
{12,'W',1,'B',12,'W',3,'B',24,'W',1,'B',14,'W'}
WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW
```


=={{header|F Sharp|F#}}==

```fsharp

open System
open System.Text.RegularExpressions

let encode data =
    // encodeData : seq<'T> -> seq<int * 'T> i.e. Takes a sequence of 'T types and return a sequence of tuples containing the run length and an instance of 'T.
    let rec encodeData input =
        seq { if not (Seq.isEmpty input) then
                 let head = Seq.head input
                 let runLength = Seq.length (Seq.takeWhile ((=) head) input)
                 yield runLength, head
                 yield! encodeData (Seq.skip runLength input) }

    encodeData data |> Seq.fold(fun acc (len, d) -> acc + len.ToString() + d.ToString()) ""

let decode str =
    [ for m in Regex.Matches(str, "(\d+)(.)") -> m ]
    |> List.map (fun m -> Int32.Parse(m.Groups.[1].Value), m.Groups.[2].Value)
    |> List.fold (fun acc (len, s) -> acc + String.replicate len s) ""

```



## Factor


```factor
USING: io kernel literals math.parser math.ranges sequences
sequences.extras sequences.repeating splitting.extras
splitting.monotonic strings ;
IN: rosetta-code.run-length-encoding

CONSTANT: alpha $[ CHAR: A CHAR: Z [a,b] >string ]

: encode ( str -- str )
    [ = ] monotonic-split [ [ length number>string ] [ first ]
    bi suffix ] map concat ;

: decode ( str -- str )
    alpha split* [ odd-indices ] [ even-indices
    [ string>number ] map ] bi [ repeat ] 2map concat ;

"WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW"
"12W1B12W3B24W1B14W"
[ encode ] [ decode ] bi* [ print ] bi@
```

{{out}}

```txt

12W1B12W3B24W1B14W
WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW

```



## FALSE


```false
1^[^$~][$@$@=$[%%\1+\$0~]?~[@.,1\$]?%]#%\.,  {encode}
```


```false
[0[^$$'9>'0@>|~]['0-\10*+]#]n:
[n;!$~][[\$][1-\$,]#%%]#%%                   {decode}
```



## Fan


```Fan
**
** Generates a run-length encoding for a string
**
class RLE
{
  Run[] encode(Str s)
  {
    runs := Run[,]

    s.size.times |i|
    {
      ch := s[i]
      if (runs.size==0 || runs.last.char != ch)
        runs.add(Run(ch))
      runs.last.inc
    }
    return runs
  }

  Str decode(Run[] runs)
  {
    buf := StrBuf()
    runs.each |run|
    {
      run.count.times { buf.add(run.char.toChar) }
    }
    return buf.toStr
  }

  Void main()
  {
    echo(decode(encode(
"WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW"
        )))
  }

}

internal class Run
{
  Int char
  Int count := 0
  new make(Int ch) { char = ch }
  Void inc() { ++count }

  override Str toStr() { return "${count}${char.toChar}" }
}
```



## Forth


```forth
variable a
: n>a  (.) tuck a @ swap move a +! ;
: >a   a @ c! 1 a +! ;
: encode ( c-addr +n a -- a n' )
  dup a ! -rot over c@ 1 2swap 1 /string bounds ?do
    over i c@ = if 1+
    else n>a >a i c@ 1 then
  loop n>a >a  a @ over - ;

: digit?  [char] 0 [ char 9 1+ literal ] within ;
: decode ( c-addr +n a -- a n' )
  dup a ! 0 2swap bounds ?do
    i c@ digit? if 10 * i c@ [char] 0 - + else
    a @ over i c@ fill a +! 0 then
  loop drop a @ over - ;
```


Example:


```forth
s" WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW"
here 1000 + encode here 2000 + decode cr 3 spaces type
   WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW
```



## Fortran

{{works with|Fortran|95 and later}}

```fortran
program RLE
  implicit none

  integer, parameter :: bufsize = 100   ! Sets maximum size of coded and decoded strings, adjust as necessary
  character(bufsize) :: teststr = "WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW"
  character(bufsize) :: codedstr = "" Encode (Data (Index..Data'Last));
            end;
         end;
      end if;
   end Encode;
   function Decode (Data : String) return String is
   begin
      if Data'Length = 0 then
         return "";
      else
         declare
            Index : Integer := Data'First;
            Count : Natural := 0;
         begin
            while Index , decodedstr = ""

  call Encode(teststr, codedstr)
  write(*,"(a)") trim(codedstr)
  call Decode(codedstr, decodedstr)
  write(*,"(a)") trim(decodedstr)

contains

subroutine Encode(instr, outstr)
  character(*), intent(in)  :: instr
  character(*), intent(out) :: outstr
  character(8) :: tempstr = ""
  character(26) :: validchars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  integer :: a, b, c, i

  if(verify(trim(instr), validchars) /= 0) then
    outstr = "Invalid input"
    return
  end if
  outstr = ""
  c = 1
  a = iachar(instr(1:1))
  do i = 2, len(trim(instr))
    b = iachar(instr(i:i))
    if(a == b) then
      c = c + 1
    else
      write(tempstr, "(i0)") c
      outstr = trim(outstr) // trim(tempstr) // achar(a)
      a = b
      c = 1
    end if
  end do
  write(tempstr, "(i0)") c
  outstr = trim(outstr) // trim(tempstr) // achar(b)
end subroutine

subroutine Decode(instr, outstr)
  character(*), intent(in)  :: instr
  character(*), intent(out) :: outstr
  character(26) :: validchars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  integer :: startn, endn, n

  outstr = ""
  startn = 1
  do while(startn < len(trim(instr)))
    endn = scan(instr(startn:), validchars) + startn - 1
    read(instr(startn:endn-1), "(i8)") n
    outstr = trim(outstr) // repeat(instr(endn:endn), n)
    startn = endn + 1
  end do
end subroutine
end program
```



## FreeBASIC


```freebasic

Dim As String initial, encoded, decoded

Function RLDecode(i As String) As String
    Dim As Long Loop0
    dim as string rCount, outP, m

    For Loop0 = 1 To Len(i)
        m = Mid(i, Loop0, 1)
        Select Case m
        Case "0" To "9"
            rCount += m
        Case Else
            If Len(rCount) Then
                outP += String(Val(rCount), m)
                rCount = ""
            Else
                outP += m
            End If
        End Select
    Next
    RLDecode = outP
End Function

Function RLEncode(i As String) As String
    Dim As String tmp1, tmp2, outP
    Dim As Long Loop0, rCount

    tmp1 = Mid(i, 1, 1)
    tmp2 = tmp1
    rCount = 1

    For Loop0 = 2 To Len(i)
        tmp1 = Mid(i, Loop0, 1)
        If tmp1 <> tmp2 Then
            outP += Ltrim(Rtrim(Str(rCount))) + tmp2
            tmp2 = tmp1
            rCount = 1
        Else
            rCount += 1
        End If
    Next

    outP += Ltrim(Rtrim(Str(rCount)))
    outP += tmp2
    RLEncode = outP
End Function

Input "Type something: ", initial
encoded = RLEncode(initial)
decoded = RLDecode(encoded)
Print initial
Print encoded
Print decoded
End

```

{{out}}
La salida es similar a la de [[#BASIC|BASIC]], mostrada arriba.


## Gambas

'''[https://gambas-playground.proko.eu/?gist=b30707043cb64effba91a2edc4d4be94 Click this link to run this code]'''

```gambas
Public Sub Main()
Dim sString As String = "WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW"
Dim siCount As Short = 1
Dim siStart As Short = 1
Dim sHold As New String[]
Dim sTemp As String

sString &= " "

Repeat
  sTemp = Mid(sString, siCount, 1)
  Do
    Inc siCount
    If Mid(sString, siCount, 1) <> sTemp Then Break
    If siCount = Len(sString) Then Break
  Loop
  sHold.add(Str(siCount - siStart) & sTemp)
  siStart = siCount
Until siCount = Len(sString)

Print sString & gb.NewLine & sHold.Join(", ")

End
```

Output:

```txt

WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW
12W, 1B, 12W, 3B, 24W, 1B, 14W

```



## Go

Decoder kind of necessary to demonstrate task requirement that I can recreate the input.

```go
package main

import "fmt"

// encoding scheme:
// encode to byte array
// byte value < 26 means single character: byte value + 'A'
// byte value 26..255 means (byte value - 24) copies of next byte
func rllEncode(s string) (r []byte) {
    if s == "" {
        return
    }
    c := s[0]
    if c < 'A' || c > 'Z' {
        panic("invalid")
    }
    nc := byte(1)
    for i := 1; i < len(s); i++ {
        d := s[i]
        switch {
        case d != c:
        case nc < (255 - 24):
            nc++
            continue
        }
        if nc > 1 {
            r = append(r, nc+24)
        }
        r = append(r, c-'A')
        if d < 'A' || d > 'Z' {
            panic("invalid")
        }
        c = d
        nc = 1
    }
    if nc > 1 {
        r = append(r, nc+24)
    }
    r = append(r, c-'A')
    return
}

func main() {
    s := "WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW"
    fmt.Println("source: ", len(s), "bytes:", s)
    e := rllEncode(s)
    fmt.Println("encoded:", len(e), "bytes:", e)
    d := rllDecode(e)
    fmt.Println("decoded:", len(d), "bytes:", d)
    fmt.Println("decoded = source:", d == s)
}

func rllDecode(e []byte) string {
    var c byte
    var d []byte
    for i := 0; i < len(e); i++ {
        b := e[i]
        if b < 26 {
            c = 1
        } else {
            c = b - 24
            i++
            b = e[i]
        }
        for c > 0 {
            d = append(d, b+'A')
            c--
        }
    }
    return string(d)
}
```

Output:

```txt

source:  67 bytes: WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW
encoded: 12 bytes: [36 22 1 36 22 27 1 48 22 1 38 22]
decoded: 67 bytes: WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW
decoded = source: true

```



## Groovy


```groovy
def rleEncode(text) {
    def encoded = new StringBuilder()
    (text =~ /(([A-Z])\2*)/).each { matcher ->
        encoded.append(matcher[1].size()).append(matcher[2])
    }
    encoded.toString()
}

def rleDecode(text) {
    def decoded = new StringBuilder()
    (text =~ /([0-9]+)([A-Z])/).each { matcher ->
        decoded.append(matcher[2] * Integer.parseInt(matcher[1]))
    }
    decoded.toString()
}
```

Test code

```groovy
def text = 'WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW'
def rleEncoded = rleEncode(text)
assert rleEncoded == '12W1B12W3B24W1B14W'
assert text == rleDecode(rleEncoded)

println "Original Text: $text"
println "Encoded Text: $rleEncoded"
```

Output:

```txt
Original Text: WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW
Encoded Text: 12W1B12W3B24W1B14W
```



## Haskell


### Version 1


```haskell
import Data.List (group)
import Control.Arrow ((&&&))

-- Datatypes
type Encoded = [(Int, Char)]  -- An encoded String with form [(times, char), ...]
type Decoded = String

-- Takes a decoded string and returns an encoded list of tuples
rlencode :: Decoded -> Encoded
rlencode = map (length &&& head) . group

-- Takes an encoded list of tuples and returns the associated decoded String
rldecode :: Encoded -> Decoded
rldecode = concatMap (uncurry replicate)

main :: IO ()
main = do
  -- Get input
  putStr "String to encode: "
  input <- getLine
  -- Output encoded and decoded versions of input
  let encoded = rlencode input
      decoded = rldecode encoded
  putStrLn $ "Encoded: " ++ show encoded ++ "\nDecoded: " ++ show decoded
```


### Version 2


```Haskell

import Data.List
import Data.Char

runLengthEncode = concatMap (\xs@(x:_) -> (show.length $ xs) ++ [x]).group
runLengthDecode = concat.uncurry (zipWith (\[x] ns -> replicate (read ns) x))
                 .foldr (\z (x,y) -> (y,z:x)) ([],[]).groupBy (\x y -> all isDigit [x,y])

main = do
 let text = "WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW"
 let encode = runLengthEncode text
 let decode = runLengthDecode encode
 mapM_ putStrLn [text,encode,decode]
 putStrLn $ "test: text == decode => " ++ (show $ text == decode)

```

{{out}}

```txt

WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW
12W1B12W3B24W1B14W
WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW
test: text == decode => True

```


=={{header|Icon}} and {{header|Unicon}}==

```Icon
procedure main(arglist)

   s := "WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW"

   write(" s=",image(s))
   write("s1=",image(s1 := rle_encode(s)))
   write("s2=",image(s2 := rle_decode(s1)))

   if s ~== s2 then write("Encode/Decode problem.")
               else write("Encode/Decode worked.")
end

procedure rle_encode(s)
   es := ""
   s ? while c := move(1) do es ||:= *(move(-1),tab(many(c))) || c
   return es
end

procedure rle_decode(es)
   s := ""
   es ? while s ||:= Repl(tab(many(&digits)),move(1))
   return s
end

procedure Repl(n, c)
    return repl(c,n)
end
```


Sample output:

```txt
 s="WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW"
s1="12W1B12W3B24W1B14W"
s2="WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW"
Encode/Decode worked.
```



## J

'''Solution:'''

```j
rle=: ;@(<@(":@(#-.1:),{.);.1~ 1, 2 ~:/\ ])
rld=: ;@(-.@e.&'0123456789' <@({:#~1{.@,~".@}:);.2 ])
```


'''Example:'''

```j
   rle 'WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW'
12W1B12W3B24W1B14W

   rld '12W1B12W3B24W1B14W'
WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW
```


Note that this implementation fails for the empty case. Here's a version that fixes that:


```j
rle=: ;@(<@(":@#,{.);.1~ 2 ~:/\ (a.{.@-.{.),])
```


Other approaches include using <nowiki>rle ::(''"_)</nowiki> or <nowiki>rle^:(*@#)</nowiki> or equivalent variations on the original sentence.


###  Alternative Implementation


A numeric approach, based on a discussion in the J forums (primarily [http://jsoftware.com/pipermail/programming/2015-June/042139.html Pascal Jasmin] and [http://jsoftware.com/pipermail/programming/2015-June/042141.html Marshall Lochbaum]):


```j
   torle=: (#, {.);.1~ 1,2 ~:/\ ]
   frle=: #/@|:
```


Task example:


```j
   torle a.i.'WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW'
12 87
 1 66
12 87
 3 66
24 87
 1 66
14 87
   u: frle torle a.i.'WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW'
WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW
```


Note that this approach also fails on the empty case.


## Java


```java
import java.util.regex.Matcher;
import java.util.regex.Pattern;
public class RunLengthEncoding {

    public static String encode(String source) {
        StringBuffer dest = new StringBuffer();
        for (int i = 0; i < source.length(); i++) {
            int runLength = 1;
            while (i+1 < source.length() && source.charAt(i) == source.charAt(i+1)) {
                runLength++;
                i++;
            }
            dest.append(runLength);
            dest.append(source.charAt(i));
        }
        return dest.toString();
    }

    public static String decode(String source) {
        StringBuffer dest = new StringBuffer();
        Pattern pattern = Pattern.compile("[0-9]+|[a-zA-Z]");
        Matcher matcher = pattern.matcher(source);
        while (matcher.find()) {
            int number = Integer.parseInt(matcher.group());
            matcher.find();
            while (number-- != 0) {
                dest.append(matcher.group());
            }
        }
        return dest.toString();
    }

    public static void main(String[] args) {
        String example = "WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW";
        System.out.println(encode(example));
        System.out.println(decode("1W1B1W1B1W1B1W1B1W1B1W1B1W1B"));
    }
}
```

Tests:

{{libheader|JUnit}}

```java
import static org.junit.Assert.assertEquals;

import org.junit.Test;

public class RunLengthEncodingTest {
	private RLE = new RunLengthEncoding();

	@Test
	public void encodingTest() {
		assertEquals("1W", RLE.encode("W"));
		assertEquals("4W", RLE.encode("WWWW"));
		assertEquals("5w4i7k3i6p5e4d2i1a",
				RLE.encode("wwwwwiiiikkkkkkkiiippppppeeeeeddddiia"));
		assertEquals("12B1N12B3N24B1N14B",
				RLE.encode("BBBBBBBBBBBBNBBBBBBBBBBBBNNNBBBBBBBBBBBBBBBBBBBBBBBBNBBBBBBBBBBBBBB"));
		assertEquals("12W1B12W3B24W1B14W",
				RLE.encode("WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW"));
		assertEquals("1W1B1W1B1W1B1W1B1W1B1W1B1W1B", RLE.encode("WBWBWBWBWBWBWB"));

	}

	@Test
	public void decodingTest() {
		assertEquals("W", RLE.decode("1W"));
		assertEquals("WWWW", RLE.decode("4W"));
		assertEquals("WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW",
				RLE.decode("12W1B12W3B24W1B14W"));
		assertEquals("WBWBWBWBWBWBWB", RLE.decode("1W1B1W1B1W1B1W1B1W1B1W1B1W1B"));
		assertEquals("WBWBWBWBWBWBWB", RLE.decode("1W1B1W1B1W1B1W1B1W1B1W1B1W1B"));

	}
}
```



## JavaScript

Here's an encoding method that walks the input string character by character

```javascript
function encode(input) {
    var encoding = [];
    var prev, count, i;
    for (count = 1, prev = input[0], i = 1; i < input.length; i++) {
        if (input[i] != prev) {
            encoding.push([count, prev]);
            count = 1;
            prev = input[i];
        }
        else
            count ++;
    }
    encoding.push([count, prev]);
    return encoding;
}
```


Here's an encoding method that uses a regular expression to grab the character runs ({{works with|JavaScript|1.6}} for the <code>forEach</code> method)

```javascript
function encode_re(input) {
    var encoding = [];
    input.match(/(.)\1*/g).forEach(function(substr){ encoding.push([substr.length, substr[0]]) });
    return encoding;
}
```


And to decode (see [[Repeating a string#JavaScript|Repeating a string]])

```javascript
function decode(encoded) {
    var output = "";
    encoded.forEach(function(pair){ output += new Array(1+pair[0]).join(pair[1]) })
    return output;
}
```



## jq

Note: "run_length_decode" as defined below requires a version of jq with regex support.

'''Utility function:'''

```jq
def runs:
  reduce .[] as $item
    ( [];
      if . == [] then [ [ $item, 1] ]
      else .[length-1] as $last
      | if $last[0] == $item then .[length-1] = [$item, $last[1] + 1]
        else . + [[$item, 1]]
        end
      end ) ;
```

'''Run-length encoding and decoding''':

```jq
def run_length_encode:
  explode | runs | reduce .[] as $x (""; . + "\($x[1])\([$x[0]]|implode)");

def run_length_decode:
  reduce (scan( "[0-9]+[A-Z]" )) as $pair
    ( "";
      ($pair[0:-1] | tonumber) as $n
      | $pair[-1:] as $letter
      | . + ($n * $letter)) ;
```

'''Example''':

```jq
"ABBCCC" | run_length_encode | run_length_decode
```

 {{out}}

```sh
$ jq -n -f Run_length_encoding.jq
"ABBCCC"
```



## Julia

{{works with|Julia|0.6}}


```julia
using IterTools

encode(str::String) = collect((length(g), first(g)) for g in groupby(first, str))
decode(cod::Vector) = join(repeat("$l", n) for (n, l) in cod)

for original in ["aaaaahhhhhhmmmmmmmuiiiiiiiaaaaaa", "WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW"]
    encoded = encode(original)
    decoded = decode(encoded)
    println("Original: $original\n -> encoded: $encoded\n -> decoded: $decoded")
end
```


{{out}}

```txt
Original: aaaaahhhhhhmmmmmmmuiiiiiiiaaaaaa
 -> encoded: Tuple{Int64,Char}[(5, 'a'), (6, 'h'), (7, 'm'), (1, 'u'), (7, 'i'), (6, 'a')]
 -> decoded: aaaaahhhhhhmmmmmmmuiiiiiiiaaaaaa
Original: WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW
 -> encoded: Tuple{Int64,Char}[(12, 'W'), (1, 'B'), (12, 'W'), (3, 'B'), (24, 'W'), (1, 'B'), (14, 'W')]
 -> decoded: WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW
```



## K



```k
rle: {,/($-':i,#x),'x@i:&1,~=':x}
```


{{trans|J}}


```k
rld: {d:"0123456789"; ,/(.(d," ")@d?/:x)#'x _dvl d}
```


'''Example:'''


```k
  rle "WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW"
"12W1B12W3B24W1B14W"
  rld "12W1B12W3B24W1B14W"
"WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW"
```



## Kotlin

Tail recursive implementation of Run Length Encoding

```scala
tailrec fun runLengthEncoding(text:String,prev:String=""):String {
    if (text.isEmpty()){
        return prev
    }
    val initialChar = text.get(0)
    val count = text.takeWhile{ it==initialChar }.count()
    return runLengthEncoding(text.substring(count),prev + "$count$initialChar" )
}

fun main(args: Array<String>) {
    assert(runLengthEncoding("TTESSST") == "2T1E3S1T")
    assert(runLengthEncoding("WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW")
                   == "12W1B12W3B24W1B14W")
}
```



## Lasso


```Lasso
define rle(str::string)::string => {
	local(orig = #str->values->asCopy,newi=array, newc=array, compiled=string)
	while(#orig->size) => {
		if(not #newi->size) => {
			#newi->insert(1)
			#newc->insert(#orig->first)
			#orig->remove(1)
		else
			if(#orig->first == #newc->last) => {
				#newi->get(#newi->size) += 1
			else
				#newi->insert(1)
				#newc->insert(#orig->first)
			}
			#orig->remove(1)
		}
	}
	loop(#newi->size) => {
		#compiled->append(#newi->get(loop_count)+#newc->get(loop_count))
	}
	return #compiled
}
define rlde(str::string)::string => {
	local(o = string)
	while(#str->size) => {
		loop(#str->size) => {
			if(#str->isualphabetic(loop_count)) => {
				if(loop_count == 1) => {
					#o->append(#str->get(loop_count))
					#str->removeLeading(#str->get(loop_count))
					loop_abort
				}
				local(num = integer(#str->substring(1,loop_count)))
				#o->append(#str->get(loop_count)*#num)
				#str->removeLeading(#num+#str->get(loop_count))
				loop_abort
			}
		}
	}
	return #o
}
//Tests:
rle('WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW')
rle('dsfkjhhkdsjfhdskhshdjjfhhdlsllw')

rlde('12W1B12W3B24W1B14W')
rlde('1d1s1f1k1j2h1k1d1s1j1f1h1d1s1k1h1s1h1d2j1f2h1d1l1s2l1w')
```


{{out}}

```txt
12W1B12W3B24W1B14W
1d1s1f1k1j2h1k1d1s1j1f1h1d1s1k1h1s1h1d2j1f2h1d1l1s2l1w


WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW
dsfkjhhkdsjfhdskhshdjjfhhdlsllw
```



## Liberty BASIC


```lb
mainwin 100 20

    'In$ ="aaaaaaaaaaaaaaaaaccbbbbbbbbbbbbbbba" ' testing...
    In$ ="WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW"
    '   Out$= "12W1B12W3B24W1B14W"

    Out$ =Encoded$( In$)
    Inv$ =Decoded$( Out$)

    print " Supplied string ="; In$
    Print " RLE version     ="; Out$
    print " Decoded back to ="; Inv$

    end

    function Encoded$( k$)
        r$    =""
        r     =1
        for i =2 to len( k$)
            prev$   =mid$( k$, i -1, 1)
            c$      =mid$( k$, i,    1)
            if c$ =prev$ then   '   entering a run of this character
                r =r +1
            else                '   it occurred only once
                r$ =r$ +str$( r) +prev$
                r =1
            end if
        next i
        r$ =r$ +str$( r) +c$
        Encoded$ =r$
    end function

    function Decoded$( k$)
        r$ =""
        v  =0
        for i =1 to len( k$)
            i$ =mid$( k$, i, 1)
            if instr( "0123456789", i$) then
                v =v *10 +val( i$)
            else
                for m =1 to v
                    r$ =r$ +i$
                next m
                v =0
            end if
        next i
        Decoded$ =r$
    end function
```



## LiveCode


```LiveCode
function rlEncode str
    local charCount
    put 1 into charCount
    repeat with i = 1 to the length of str
        if char i of str = char (i + 1) of str then
            add 1 to charCount
        else
            put char i of str & charCount after rle
            put 1 into charCount
        end if
    end repeat
    return rle
end rlEncode

function rlDecode str
    repeat with i = 1 to the length of str
        if char i of str is not a number then
            put char i of str into curChar
            put 0 into curNum
        else
            repeat with n = i to len(str)
                if isnumber(char n of str) then
                    put char n of str after curNum
                else
                    put repeatString(curChar,curNum) after rldec
                    put n - 1 into i
                    exit repeat
                end if
            end repeat
        end if
        if i = len(str) then --dump last char
            put repeatString(curChar,curNum) after rldec
        end if
    end repeat
    return rldec
end rlDecode

function repeatString str,rep
    repeat rep times
        put str after repStr
    end repeat
    return repStr
end repeatString
```




## Logo


```logo
to encode :str [:out "||] [:count 0] [:last first :str]
  if empty? :str [output (word :out :count :last)]
  if equal? first :str :last [output (encode bf :str :out :count+1 :last)]
  output (encode bf :str (word :out :count :last) 1 first :str)
end

to reps :n :w
  output ifelse :n = 0 ["||] [word :w reps :n-1 :w]
end
to decode :str [:out "||] [:count 0]
  if empty? :str [output :out]
  if number? first :str [output (decode bf :str :out 10*:count + first :str)]
  output (decode bf :str word :out reps :count first :str)
end

make "foo "WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW
make "rle encode :foo
show equal? :foo decode :rle
```



## Lua



```lua
local C, Ct, R, Cf, Cc = lpeg.C, lpeg.Ct, lpeg.R, lpeg.Cf, lpeg.Cc
astable = Ct(C(1)^0)

function compress(t)
    local ret = {}
    for i, v in ipairs(t) do
      if t[i-1] and v == t[i-1] then
        ret[#ret - 1] = ret[#ret - 1] + 1
      else
        ret[#ret + 1] = 1
        ret[#ret + 1] = v
      end
    end
    t = ret
    return table.concat(ret)
end
q = io.read()
print(compress(astable:match(q)))

undo = Ct((Cf(Cc"0" * C(R"09")^1, function(a, b) return 10 * a + b end) * C(R"AZ"))^0)

function decompress(s)
  t = undo:match(s)
  local ret = ""
  for i = 1, #t - 1, 2 do
    for _ = 1, t[i] do
      ret = ret .. t[i+1]
    end
  end
  return ret
end
```


## M2000 Interpreter


```M2000 Interpreter

Module RLE_example {
	inp$="WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW"
	Print "Input: ";inp$
	Function RLE$(r$){
		Function rle_run$(&r$) {
			if len(r$)=0 then exit
			p=1
			c$=left$(r$,1)
			while c$=mid$(r$, p, 1) {p++}
			=format$("{0}{1}",p-1, c$)
			r$=mid$(r$, p)
		}
		def repl$
		while len(r$)>0 {repl$+=rle_run$(&r$)}
		=repl$
	}
	RLE_encode$=RLE$(inp$)
	Print "RLE Encoded: ";RLE_encode$
	Function RLE_decode$(r$) {
		def repl$
		def long m, many=1
		while r$<>"" and many>0 {
			many=val(r$, "INT", &m)
			repl$+=string$(mid$(r$, m, 1), many)
			r$=mid$(r$,m+1)
		}
		=repl$
	}
	RLE_decode$=RLE_decode$(RLE_encode$)
	Print "RLE Decoded: ";RLE_decode$
	Print "Checked: ";RLE_decode$=inp$
}
RLE_example

```


{{out}}
<pre style="height:30ex;overflow:scroll">
Input: WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW
RLE Encoded: 12W1B12W3B24W1B14W
RLE Decoded: WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW
Checked: True
</pre >


## Mathematica

Custom functions using Map, Apply, pure functions, replacing using pattern matching, delayed rules and other functions:

```Mathematica
RunLengthEncode[input_String]:=StringJoin@@Sequence@@@({ToString @Length[#],First[#]}&/@Split[Characters[input]])
RunLengthDecode[input_String]:=StringJoin@@ConstantArray@@@Reverse/@Partition[(Characters[input]/.(ToString[#]->#&/@Range[0,9]))//.{x___,i_Integer,j_Integer,y___}:>{x,10i+j,y},2]
```

Example:

```Mathematica
mystring="WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW";
RunLengthEncode[mystring]
RunLengthDecode[%]
%==mystring
```

gives back:

```Mathematica
12W1B12W3B24W1B14W
WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW
True
```

An alternate solution:

```Mathematica
RunLengthEncode[s_String] := StringJoin[
  {ToString[Length[#]] <> First[#]} & /@ Split[StringSplit[s, ""]]
  ]

RunLengthDecode[s_String] := StringJoin[
  Table[#[[2]], {ToExpression[#[[1]]]}] & /@
   Partition[StringSplit[s, x : _?LetterQ :> x], 2]
  ]
```

This second encode function is adapted from the MathWorld example.


## Maxima


```maxima
rle(a) := block(
   [n: slength(a), b: "", c: charat(a, 1), k: 1],
   for i from 2 thru n do
      if cequal(c, charat(a, i)) then k: k + 1 else (b: sconcat(b, k, c), c: charat(a, i), k: 1),
   sconcat(b, k, c)
)$

rle("WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW");
"12W1B12W3B24W1B14W"
```



## MMIX


```mmix
	LOC	Data_Segment
	GREG	@
Buf	OCTA	0,0,0,0         integer print buffer
Char	BYTE	0,0             single char print buffer
task	BYTE	"WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWW"
	BYTE    "WWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW",0
len	GREG	@-1-task

// task should become this
tEnc	BYTE	"12W1B12W3B24W1B14W",0

	GREG	@
// tuple array for encoding purposes
// each tuple is a tetra (4 bytes long or 2 wydes long)
// (c,l) in which c is a char and l = number of chars c
// high wyde of the tetra contains the char
// low wyde  .. ..  ..    contains the length
RLE	TETRA	0

	LOC	#100            locate program
	GREG	@
// print number to stdout
// destroys input arg $3 !
Prt64	LDA	$255,Buf+23    points to LSD
//			       do
2H	DIV	$3,$3,10        (N,R) = divmod (N,10)
	GET	$13,rR          get remainder
	INCL	$13,'0'         convert to ascii
	STBU	$13,$255        store ascii digit
	BZ	$3,3F
	SUB	$255,$255,1     move pointer down
	JMP	2B             While N !=0
3H	TRAP	0,Fputs,StdOut print number to standard out
	GO	$127,$127,0    return

	GREG	@
// print char to stdout
PChar	LDA	$255,Char
	STBU	$4,$255
	TRAP	0,Fputs,StdOut
	GO	$127,$127,0

	GREG	@
// encode routine
// $0 string pointer
// $1 index var
// $2 pointer to tuple array
// $11 temp var tuple
Encode	SET	$1,0        initialize index = 0
	SET	$11,0       postion in string = 0
	LDBU	$3,$0,$1    get first char
	ADDU	$6,$3,0     remember it
                            do
1H	INCL	$1,1          repeat  incr index
	LDBU	$3,$0,$1              get a char
	BZ	$3,2F                 if EOS then finish
	CMP	$7,$3,$6
	PBZ	$7,1B         while new == old
	XOR	$4,$4,$4      new tuple
	ADDU	$4,$6,0
	SLU	$4,$4,16      old char to tuple -> (c,_)
	SUB	$7,$1,$11     length = index - previous position
	ADDU	$11,$1,0      incr position
	OR	$4,$4,$7      length l to tuple -> (c,l)
	STT	$4,$2         put tuple in array
	ADDU	$6,$3,0       remember new char
	INCL	$2,4          incr 'tetra' pointer
	JMP	1B          loop
2H	XOR	$4,$4,$4    put last tuple in array
	ADDU	$4,$6,0
	SLU	$4,$4,16
	SUB	$7,$1,$11
	ADDU	$11,$1,0
	OR	$4,$4,$7
	STT	$4,$2
	GO	$127,$127,0 return

	GREG	@
Main	LDA	$0,task      pointer uncompressed string
	LDA	$2,RLE	     pointer tuple array
	GO	$127,Encode  encode string
	LDA	$2,RLE	     points to start tuples
	SET	$5,#ffff     mask for extracting length
1H	LDTU	$3,$2        while not End of Array
	BZ	$3,2F
	SRU	$4,$3,16      char   = (c,_)
	AND	$3,$3,$5      length = (_,l)
	GO	$127,Prt64    print length
	GO	$127,PChar    print char
	INCL	$2,4          incr tuple pointer
	JMP	1B           wend
2H	SET	$4,#a        print NL
	GO	$127,PChar

// decode using the RLE tuples
	LDA	$2,RLE	     pointer tuple array
	SET	$5,#ffff     mask
1H	LDTU	$3,$2        while not End of Array
	BZ	$3,2F
	SRU	$4,$3,16      char   = (c,_)
	AND	$3,$3,$5      length = (_,l)
//                            for (i=0;i<length;i++) {
3H	GO	$127,PChar      print a char
	SUB	$3,$3,1
	PBNZ	$3,3B
	INCL	$2,4
	JMP	1B            }
2H	SET	$4,#a        print NL
	GO	$127,PChar
	TRAP	0,Halt,0     EXIT
```

Example run encode --> decode:

```txt
~/MIX/MMIX/Rosetta> mmix rle
12W1B12W3B24W1B14W
WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW
```



## Nim

{{trans|Python}}

```nim
import strutils

type RunLength = tuple[c: char, n: int]

proc encode(inp): seq[RunLength] =
  result = @[]
  var count = 1
  var prev: char

  for c in inp:
    if c != prev:
      if prev != chr(0):
        result.add((prev,count))
      count = 1
      prev = c
    else:
      inc(count)
  result.add((prev,count))

proc decode(lst: openarray[RunLength]): string =
  result = ""
  for x in lst:
    result.add(repeatChar(x.n, x.c))

echo encode("aaaaahhhhhhmmmmmmmuiiiiiiiaaaaaa")
echo decode([('a', 5), ('h', 6), ('m', 7), ('u', 1), ('i', 7), ('a', 6)])
```



## Objeck


```objeck
use RegEx;

class RunLengthEncoding {
  function : Main(args : String[]) ~ Nil {
    input := "WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW";

    encoded := Encode(input);
    "encoding: {$encoded}"->PrintLine();
    test := encoded->Equals("12W1B12W3B24W1B14W");
    "encoding match: {$test}"->PrintLine();

    decoded := Decode(encoded);
    test := input->Equals(decoded);
    "decoding match: {$test}"->PrintLine();
  }

  function : Encode(source : String) ~ String {
    dest := "";
    each(i : source) {
      runLength := 1;
      while(i+1 < source->Size() & source->Get(i) = source->Get(i+1)) {
        runLength+= 1;
        i+= 1;
      };
      dest->Append(runLength);
      dest->Append(source->Get(i));
    };

    return dest;
  }

  function : Decode(source : String) ~ String {
    output := "";
    regex := RegEx->New("[0-9]+|([A-Z]|[a-z])");
    found := regex->Find(source);
    count : Int;
    each(i : found) {
      if(i % 2 = 0) {
        count := found->Get(i)->As(String)->ToInt();
      }
      else {
        letter := found->Get(i)->As(String);
        while(count <> 0) {
          output->Append(letter);
          count -= 1;
        };
      };
    };

    return output;
  }
}
```



```txt
encoding: 12W1B12W3B24W1B14W
encoding match: true
decoding match: true
```


=={{header|Objective-C}}==
See [[Run-length encoding/Objective-C]]


## OCaml


```ocaml
let encode str =
  let len = String.length str in
  let rec aux i acc =
    if i >= len then List.rev acc
    else
      let c1 = str.[i] in
      let rec aux2 j =
        if j >= len then (c1, j-i)
        else
          let c2 = str.[j] in
          if c1 = c2
          then aux2 (j+1)
          else (c1, j-i)
      in
      let (c,n) as t = aux2 (i+1) in
      aux (i+n) (t::acc)
  in
  aux 0 []
;;

let decode lst =
  let l = List.map (fun (c,n) -> String.make n c) lst in
  (String.concat "" l)
```



```ocaml
let () =
  let e = encode "aaaaahhhhhhmmmmmmmuiiiiiiiaaaaaa" in
  List.iter (fun (c,n) ->
    Printf.printf " (%c, %d);\n" c n;
  ) e;
  print_endline (decode [('a', 5); ('h', 6); ('m', 7); ('u', 1); ('i', 7); ('a', 6)]);
;;
```


;Using regular expressions

```ocaml
#load "str.cma";;

open Str

let encode =
  global_substitute (Str.regexp "\\(.\\)\\1*")
    (fun s -> string_of_int (String.length (matched_string s)) ^
              matched_group 1 s)

let decode =
  global_substitute (Str.regexp "\\([0-9]+\\)\\([^0-9]\\)")
    (fun s -> String.make (int_of_string (matched_group 1 s))
                          (matched_group 2 s).[0])

let () =
  print_endline (encode "WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW");
  print_endline (decode "12W1B12W3B24W1B14W");
```



## Oforth



```Oforth
: encode(s)
   StringBuffer new
   s group apply(#[ tuck size asString << swap first <<c ]) ;

: decode(s)
| c i |
   StringBuffer new
   0 s forEach: c [
      c isDigit ifTrue: [ 10 * c asDigit + continue ]
      loop: i [ c <<c ] 0
      ]
   drop ;
```


{{out}}

```txt

>"WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW" encode .s
[1] (StringBuffer) 12W1B12W3B24W1B14W
ok
>decode .s
[1] (StringBuffer) WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW
ok

```



## Oz


```oz
declare
  fun {RLEncode Xs}
     for G in {Group Xs} collect:C do
	{C {Length G}#G.1}
     end
  end

  fun {RLDecode Xs}
     for C#Y in Xs append:Ap do
	{Ap {Replicate Y C}}
     end
  end

  %% Helpers
  %% e.g. "1122" -> ["11" "22"]
  fun {Group Xs}
     case Xs of nil then nil
     [] X|Xr then
	Ys Zs
        {List.takeDropWhile Xr fun {$ W} W==X end ?Ys ?Zs}
     in
        (X|Ys) | {Group Zs}
     end
  end
  %% e.g. 3,4 -> [3 3 3 3]
  fun {Replicate X N}
     case N of 0 then nil
     else X|{Replicate X N-1}
     end
  end

  Data = "WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW"
  Enc = {RLEncode Data}
in
  {System.showInfo Data}
  {Show Enc}
  {System.showInfo {RLDecode Enc}}
```


## PARI/GP


```parigp
rle(s)={
  if(s=="", return(s));
  my(v=Vec(s),cur=v[1],ct=1,out="");
  v=concat(v,99); \\ sentinel
  for(i=2,#v,
    if(v[i]==cur,
      ct++
    ,
      out=Str(out,ct,cur);
      cur=v[i];
      ct=1
    )
  );
  out
};
elr(s)={
  if(s=="", return(s));
  my(v=Vec(s),ct=eval(v[1]),out="");
  v=concat(v,99); \\ sentinel
  for(i=2,#v,
    if(v[i]>="0" && v[i]<="9",
      ct=10*ct+eval(v[i])
    ,
      for(j=1,ct,out=Str(out,v[i]));
      ct=0
    )
  );
  out
};
rle("WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW")
elr(%)
```

Output:

```txt
%1 = "12W1B12W3B24W1B14W"

%2 = "WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW"
```



## Pascal


```pascal
Program RunLengthEncoding(output);

procedure encode(s: string; var counts: array of integer; var letters: string);
  var
    i, j: integer;
  begin
    j := 0;
    letters := '';
    if length(s) > 0 then
    begin
      j := 1;
      letters := letters + s[1];
      counts[1] := 1;
      for i := 2 to length(s) do
        if s[i] = letters[j] then
          inc(counts[j])
        else
        begin
          inc(j);
          letters := letters + s[i];
          counts[j] := 1;
        end;
    end;
  end;

procedure decode(var s: string; counts: array of integer; letters: string);
  var
    i, j: integer;
  begin
    s := '';
    for i := 1 to length(letters) do
      for j := 1 to counts[i] do
        s := s + letters[i];
  end;

var
  s: string;
  counts: array of integer;
  letters: string;
  i: integer;
begin
  s := 'WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWW';
  writeln(s);
  setlength(counts, length(s));
  encode(s, counts, letters);
  for i := 1 to length(letters) - 1 do
    write(counts[i], ' * ', letters[i], ', ');
  writeln(counts[length(letters)], ' * ', letters[length(letters)]);
  decode(s, counts, letters);
  writeln(s);
end.
```

Output:

```txt
:> ./RunLengthEncoding
WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWW
12 * W, 1 * B, 12 * W, 3 * B, 24 * W, 1 * B, 13 * W
WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWW

```



## Perl


Simple version using ASCII numerals as length markers, like the example in the task description (won't work correctly on input strings that already contain digits):


```perl
sub encode {
    shift =~ s/(.)\1*/length($&).$1/grse;
}

sub decode {
    shift =~ s/(\d+)(.)/$2 x $1/grse;
}
```


Modified version that can take arbitrary byte strings as input (produces encoded byte strings that are compatible with the [[#C|C solution]]):


```perl
sub encode {
    shift =~ s/(.)\1{0,254}/pack("C", length($&)).$1/grse;
}

sub decode {
    shift =~ s/(.)(.)/$2 x unpack("C", $1)/grse;
}
```


Further modified version that supports compact representation of longer non-repeating substrings, just like the [[#C|C solution]] (so should be fully compatible with that solution for both encoding and decoding):


```perl
sub encode {
    my $str = shift;
    my $ret = "";
    my $nonrep = "";
    while ($str =~ m/(.)\1{0,127}|\z/gs) {
        my $len = length($&);
        if (length($nonrep) && (length($nonrep) == 127 || $len != 1)) {
            $ret .= pack("C", 128 + length($nonrep)) . $nonrep;
            $nonrep = "";
        }
        if    ($len == 1) { $nonrep .= $1 }
        elsif ($len > 1)  { $ret .= pack("C", $len) . $1 }
    }
    return $ret;
}

sub decode {
    my $str = shift;
    my $ret = "";
    for (my $i = 0; $i < length($str);) {
        my $len = unpack("C", substr($str, $i, 1));
        if ($len <= 128) {
            $ret .= substr($str, $i + 1, 1) x $len;
            $i += 2;
        }
        else {
            $ret .= substr($str, $i + 1, $len - 128);
            $i += 1 + $len - 128;
        }
    }
    return $ret;
}
```


Demonstration of the third version:


```perl
use Data::Dump qw(dd);
dd my $str = "XXXXXABCDEFGHIoooooooooooooooooooooooooAAAAAA";
dd my $enc = encode($str);
dd decode($enc);
```


{{out}}

```txt

"XXXXXABCDEFGHIoooooooooooooooooooooooooAAAAAA"
"\5X\x89ABCDEFGHI\31o\6A"
"XXXXXABCDEFGHIoooooooooooooooooooooooooAAAAAA"

```



## Perl 6

Note that Perl 6 regexes don't care about unquoted whitespace, and that backrefs
count from 0, not from 1.


```perl6
sub encode($str) { $str.subst(/(.) $0*/, { $/.chars ~ $0 }, :g) }

sub decode($str) { $str.subst(/(\d+) (.)/, { $1 x $0 }, :g) }

my $e = encode('WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW');
say $e;
say decode($e);
```


Output:


```txt
12W1B12W3B24W1B14W
WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW
```



## Phix

Copy of [[Run-length_encoding#Euphoria|Euphoria]]

```Phix
function encode(sequence s)
sequence out = {}
integer prev_char,count = 1
    if length(s) then
        prev_char = s[1]
        for i=2 to length(s) do
            if s[i]!=prev_char then
                out &= {count,prev_char}
                prev_char = s[i]
                count = 1
            else
                count += 1
            end if
        end for
        out &= {count,prev_char}
    end if
    return out
end function

function decode(sequence s)
sequence out = {}
    for i=1 to length(s) by 2 do
        out &= repeat(s[i+1],s[i])
    end for
    return out
end function

sequence s = encode("WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW")
pp(s)
?decode(s)
```

{{out}}

```txt

{12,87'W',1,66'B',12,87'W',3,66'B',24,87'W',1,66'B',14,87'W'}
"WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW"

```



## PHP


```php
<?php
function encode($str)
{
    return preg_replace_callback('/(.)\1*/', function ($match) {
        return strlen($match[0]) . $match[1];
    }, $str);
}

function decode($str)
{
    return preg_replace_callback('/(\d+)(\D)/', function($match) {
        return str_repeat($match[2], $match[1]);
    }, $str);
}

echo encode('WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW'), PHP_EOL;
echo decode('12W1B12W3B24W1B14W'), PHP_EOL;
?>
```



## PicoLisp


```PicoLisp
(de encode (Str)
   (pack
      (make
         (for (Lst (chop Str) Lst)
            (let (N 1  C)
               (while (= (setq C (pop 'Lst)) (car Lst))
                  (inc 'N) )
               (link N C) ) ) ) ) )

(de decode (Str)
   (pack
      (make
         (let N 0
            (for C (chop Str)
               (if (>= "9" C "0")
                  (setq N (+ (format C) (* 10 N)))
                  (do N (link C))
                  (zero N) ) ) ) ) ) )

(and
   (prinl "Data:    " "WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW")
   (prinl "Encoded: " (encode @))
   (prinl "Decoded: " (decode @)) )
```

Output:

```txt
Data:    WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW
Encoded: 12W1B12W3B24W1B14W
Decoded: WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW
```



## PL/I


```pli
declare (c1, c2) character (1);
declare run_length fixed binary;
declare input file;

open file (input) title ('/RLE.DAT,type(text),recsize(20000)');
on endfile (input) go to epilog;

get file (input) edit (c1) (a(1));
run_length = 1;
do forever;
   get file (input) edit (c2) (a(1));
   if c1 = c2 then
      run_length = run_length + 1;
   else
      do; put edit (trim(run_length), c1) (a); run_length=1; end;
   c1 = c2;
end;
epilog:
   put edit (trim(run_length), c1) (a);
   put skip;


/* The reverse of the above operation: */
declare c character (1);
declare i fixed binary;
declare new file;

open file (new) title ('/NEW.DAT,type(text),recsize(20000)');
on endfile (new) stop;
do forever;
   run_length = 0;
   do forever;
      get file (new) edit (c) (a(1));
      if index('0123456789', c) = 0 then leave;
      run_length = run_length*10 + c;
   end;
   put edit ((c do i = 1 to run_length)) (a);
end;
```



## PowerBASIC


This version can handle any arbitrary string that doesn't contain numbers (not just letters). (A flag value could be added which would allow the inclusion of ''any'' character, but such a flag isn't in this example.)


```powerbasic
FUNCTION RLDecode (i AS STRING) AS STRING
    DIM Loop0 AS LONG, rCount AS STRING, outP AS STRING, m AS STRING

    FOR Loop0 = 1 TO LEN(i)
        m = MID$(i, Loop0, 1)
        SELECT CASE m
            CASE "0" TO "9"
                rCount = rCount & m
            CASE ELSE
                IF LEN(rCount) THEN
                    outP = outP & STRING$(VAL(rCount), m)
                    rCount=""
                ELSE
                    outP = outP & m
                END IF
        END SELECT
    NEXT
    FUNCTION = outP
END FUNCTION

FUNCTION RLEncode (i AS STRING) AS STRING
    DIM tmp1 AS STRING, tmp2 AS STRING, outP AS STRING
    DIM Loop0 AS LONG, rCount AS LONG

    tmp1 = MID$(i, 1, 1)
    tmp2 = tmp1
    rCount = 1

    FOR Loop0 = 2 TO LEN(i)
        tmp1 = MID$(i, Loop0, 1)
        IF tmp1 <> tmp2 THEN
            outP = outP & TRIM$(STR$(rCount)) & tmp2
            tmp2 = tmp1
            rCount = 1
        ELSE
            INCR rCount
        END IF
    NEXT

    outP = outP & TRIM$(STR$(rCount))
    outP = outP & tmp2
    FUNCTION = outP
END FUNCTION

FUNCTION PBMAIN () AS LONG
    DIM initial AS STRING, encoded AS STRING, decoded AS STRING
    initial = INPUTBOX$("Type something.")
    encoded = RLEncode(initial)
    decoded = RLDecode(encoded)
    'in PB/Win, "?" = MSGBOX; in PB/DOS & PB/CC. "?" = PRINT
    ? initial & $CRLF & encoded & $CRLF & decoded
END FUNCTION
```


Outputs are similar to those in [[#BASIC|BASIC]], above.


## PowerShell


```powershell
function Compress-RLE ($s) {
    $re = [regex] '(.)\1*'
    $ret = ""
    foreach ($m in $re.Matches($s)) {
        $ret += $m.Length
        $ret += $m.Value[0]
    }
    return $ret
}

function Expand-RLE ($s) {
    $re = [regex] '(\d+)(.)'
    $ret = ""
    foreach ($m in $re.Matches($s)) {
        $ret += [string] $m.Groups[2] * [int] [string] $m.Groups[1]
    }
    return $ret
}
```

Output:

```txt
PS> Compress-RLE "WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW"
12W1B12W3B24W1B14W
PS> Expand-RLE "12W1B12W3B24W1B14W"
WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW
```


## Prolog

Works with SWI-Prolog.

This code is inspired from a code found here : http://groups.google.com/group/comp.lang.prolog/browse_thread/thread/b053ea2512e8b350 (author : Pascal J. Bourguignon).

```Prolog
% the test
run_length :-
	L = "WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW",
	writef('encode %s\n', [L]),
	encode(L, R),
	writeln(R), nl,
	writef('decode %w\n', [R]),
	decode(R, L1),
	writeln(L1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  encode
%
%  translation
%  from
%  "WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW"
%  to
%  "12W1B12W3B24W1B14W"
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
encode(In, Out) :-
	% Because of the special management of the "strings" by Prolog
	( is_list(In) -> I = In; string_to_list(In, I)),
	packList(I, R1),
	dcg_packList2List(R1,R2, []),
	string_to_list(Out,R2).



dcg_packList2List([[N, V]|T]) -->
	{ number_codes(N, LN)},
	LN,
	[V],
	dcg_packList2List(T).

dcg_packList2List([]) --> [].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  decode
%
%  translation
%  from
%  "12W1B12W3B24W1B14W"
%  to
%  "WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW"
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
decode(In, Out) :-
	% Because of the special management of the "strings" by Prolog
	( is_list(In) -> I = In; string_to_list(In, I)),
	dcg_List2packList(I, R1, []),
	packList(L1, R1),
	string_to_list(Out, L1).


dcg_List2packList([H|T]) -->
	{code_type(H, digit)},
	parse_number([H|T], 0).

dcg_List2packList([]) --> [].


parse_number([H|T], N) -->
	{code_type(H, digit), !,
	N1 is N*10 + H - 48 },
	parse_number(T, N1).

parse_number([H|T], N) -->
	[[N, H]],
	dcg_List2packList(T).


% use of library clpfd allows packList(?In, ?Out) to works
% in both ways In --> Out and In <-- Out.

:- use_module(library(clpfd)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% ?- packList([a,a,a,b,c,c,c,d,d,e], L).
%  L = [[3,a],[1,b],[3,c],[2,d],[1,e]] .
% ?- packList(R,  [[3,a],[1,b],[3,c],[2,d],[1,e]]).
% R = [a,a,a,b,c,c,c,d,d,e] .
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
packList([],[]).

packList([X],[[1,X]]) :- !.


packList([X|Rest],[XRun|Packed]):-
    run(X,Rest, XRun,RRest),
    packList(RRest,Packed).


run(Var,[],[1,Var],[]).

run(Var,[Var|LRest],[N1, Var],RRest):-
    N #> 0,
    N1 #= N + 1,
    run(Var,LRest,[N, Var],RRest).


run(Var,[Other|RRest], [1,Var],[Other|RRest]):-
    dif(Var,Other).
```

Output :

```txt
 ?- run_length.
encode WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW
12W1B12W3B24W1B14W

decode 12W1B12W3B24W1B14W
WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW
true .

```



## Pure


```pure
using system;

encode s = strcat $ map (sprintf "%d%s") $ encode $ chars s with
  encode [] = [];
  encode xs@(x:_) = (#takewhile (==x) xs,x) : encode (dropwhile (==x) xs);
end;

decode s = strcat [c | n,c = parse s; i = 1..n] with
  parse s::string = regexg item "([0-9]+)(.)" REG_EXTENDED s 0;
  item info = val (reg 1 info!1), reg 2 info!1;
end;

let s = "WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW";
let r = encode s; // "12W1B12W3B24W1B14W"
decode r;
```



## PureBasic

{{trans|PowerBasic}} with some optimations to use pointers instead of string functions.  According to the task description it works with uppercase A - Z.  In this implementation it also functions with all characters that are non-digits and whose value is non-zero.

```PureBasic
Procedure.s RLDecode(toDecode.s)
  Protected.s repCount, output, currChar, tmp
  Protected *c.Character = @toDecode

  While *c\c <> #Null
    currChar = Chr(*c\c)
    Select *c\c
      Case '0' To '9'
        repCount + currChar
      Default
        If repCount
          tmp = Space(Val(repCount))
          ReplaceString(tmp, " ", currChar, #PB_String_InPlace)
          output + tmp
          repCount = ""
        Else
          output + currChar
        EndIf
    EndSelect
    *c + SizeOf(Character)
  Wend

  ProcedureReturn output
EndProcedure

Procedure.s RLEncode(toEncode.s)
  Protected.s currChar, prevChar, output
  Protected repCount
  Protected *c.Character = @toEncode

  prevChar = Chr(*c\c)
  repCount = 1

  *c + SizeOf(Character)
  While *c\c <> #Null
    currChar = Chr(*c\c)
    If currChar <> prevChar
      output + Str(repCount) + prevChar
      prevChar = currChar
      repCount = 1
    Else
      repCount + 1
    EndIf
    *c + SizeOf(Character)
  Wend

  output + Str(repCount)
  output + prevChar
  ProcedureReturn output
EndProcedure

If OpenConsole()
  Define initial.s, encoded.s, decoded.s

  Print("Type something: ")
  initial = Input()
  encoded = RLEncode(initial)
  decoded = RLDecode(encoded)
  PrintN(initial)
  PrintN(RLEncode(initial))
  PrintN(RLDecode(encoded))

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit")
  Input()
  CloseConsole()
EndIf
```

Sample output:

```txt
Type something: WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWW
WWW
WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW
12W1B12W3B24W1B14W
WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW
```



## Python


```python
def encode(input_string):
    count = 1
    prev = ''
    lst = []
    for character in input_string:
        if character != prev:
            if prev:
                entry = (prev,count)
                lst.append(entry)
                #print lst
            count = 1
            prev = character
        else:
            count += 1
    else:
        try:
            entry = (character,count)
            lst.append(entry)
            return (lst, 0)
        except Exception as e:
            print("Exception encountered {e}".format(e=e))
            return (e, 1)

def decode(lst):
    q = ""
    for character, count in lst:
        q += character * count
    return q

#Method call
value = encode("aaaaahhhhhhmmmmmmmuiiiiiiiaaaaaa")
if value[1] == 0:
    print("Encoded value is {}".format(value[0]))
    decode(value[0])
```


Functional
{{works with|Python|2.4}}

```python
from itertools import groupby
def encode(input_string):
    return [(len(list(g)), k) for k,g in groupby(input_string)]

def decode(lst):
    return ''.join(c * n for n,c in lst)

encode("aaaaahhhhhhmmmmmmmuiiiiiiiaaaaaa")
decode([(5, 'a'), (6, 'h'), (7, 'm'), (1, 'u'), (7, 'i'), (6, 'a')])
```



'''By regular expression'''

The simplified input range of only uppercase characters allows a simple regular expression to be applied repeatedly for encoding, and another for decoding:

```python
from re import sub

def encode(text):
    '''
    Doctest:
        >>> encode('WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW')
        '12W1B12W3B24W1B14W'
    '''
    return sub(r'(.)\1*', lambda m: str(len(m.group(0))) + m.group(1),
               text)

def decode(text):
    '''
    Doctest:
        >>> decode('12W1B12W3B24W1B14W')
        'WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW'
    '''
    return sub(r'(\d+)(\D)', lambda m: m.group(2) * int(m.group(1)),
               text)

textin = "WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW"
assert decode(encode(textin)) == textin
```



## R

R has a built-in function, rle, for run length encoding.  This modification allows input and output in the forms specified above.

```rsplus
runlengthencoding <- function(x)
{
   splitx <- unlist(strsplit(input, ""))
   rlex <- rle(splitx)
   paste(with(rlex, as.vector(rbind(lengths, values))), collapse="")
}

input <- "WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW"
runlengthencoding(input)
```

Similarly, inverse.rle provides decompression after a run length encoding.

```rsplus
inverserunlengthencoding <- function(x)
{
    lengths <- as.numeric(unlist(strsplit(output, "[[:alpha:]]")))
    values <- unlist(strsplit(output, "[[:digit:]]"))
    values <- values[values != ""]
    uncompressed <- inverse.rle(list(lengths=lengths, values=values))
    paste(uncompressed, collapse="")
}

output <- "12W1B12W3B24W1B14W"
inverserunlengthencoding(output)
```



## Racket



```Racket

#lang racket
(define (encode str)
  (regexp-replace* #px"(.)\\1*" str (λ (m c) (~a (string-length m) c))))
(define (decode str)
  (regexp-replace* #px"([0-9]+)(.)" str (λ (m n c) (make-string (string->number n) (string-ref c 0)))))

```



## REXX


### version 1

The task (input) rule was relaxed a bit as this program accepts upper─ and lowercase input.

Note that this REXX version (for encoding and decoding) uses a   ''replication''   count, not the   ''count''   of characters,

so a replication count of   '''11'''   represents a count of   '''12'''   characters.

### =encoding=


```rexx
/*REXX program  encodes  a string  by using a  run─length  encoding scheme.   */
parse arg x .                          /*normally, input would be in a file.  */
def= 'WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW'
if x=''  then x=def                    /*Input not specified? Then use default*/
Lx=length(x)                           /*get the length of the  X  string.    */
y=                                     /*Y:  is the output string  (so far).  */
   do j=1  by 0  to Lx                 /*J:  is incremented within the loop.  */
   c=substr(x,j,1)                     /*pick a character, check for an error.*/
   if \datatype(c,'M')  then do; say "error!: data isn't alphabetic:" c; exit 13; end
   r=0                                 /*R:  is NOT the number of characters. */
            do k=j+1  to Lx  while  substr(x,k,1)==c;   r=r+1
            end   /*k*/                /*R:  is a replication count for a char*/
   j=j+1+r                             /*increment (add to) the DO loop index.*/
   if r==0  then r=                    /*don't use  R  if it is equal to zero.*/
   Y = Y || r || c                     /*add character to the encoded string. */
   end   /*j*/

say '  input='  x
say 'encoded='  y                      /*stick a fork in it,  we're all done. */
```

'''output''' when using the default input:

```txt

 input= WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW
output= 11WB11W2B23WB13W

```



### =decoding=


```rexx
/*REXX program  decodes a string by using a  run─length  decoding scheme.     */
parse arg x .                          /*normally, input would be in a file.  */
if x==''  then x=11WB11W2B23WB13W      /*X  not specified?   Then use default.*/
Lx=length(x)                           /*get the length of the input string.  */
y=                                     /*Y:  is the output string  (so far).  */
    do j=1  by 0  to Lx                /*warning!  J  is modified within loop.*/
    c=substr(x,j,1)
    if \datatype(c,'W') then do        /*a loner char, simply add to output.  */
                             y=y || c;     j=j+1;      iterate  /*j*/
                             end
    d=1                                            /* [↓]  W:  a Whole number.*/
        do k=j+1  to Lx  while datatype(substr(x,k,1),'w'); d=d+1  /*end of #?*/
        end   /*k*/                    /*D: is the number of characters so far*/

    n=substr(x,j,d)+1                  /*D:  is length of the encoded number. */
    y=y || copies(substr(x,k,1), n)    /*N:  is now the number of characters. */
    j=j+1+d                            /*increment the DO loop index by D+1.  */
    end   /*j*/

say '  input=' x
say 'decoded=' y                       /*stick a fork in it,  we're all done. */
```

'''output''' when using the default input:

```txt

  input= 11WB11W2B23WB13W
decoded= WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW

```



### version 2


```rexx
s='WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW'
Say '  s='s
enc=encode(s)
Say 'enc='enc
dec=decode(enc)
Say 'dec='dec
if dec==s Then Say 'OK'
Exit

encode: Procedure
Parse Arg s
c=left(s,1)
cnt=1
ol=''
Do i=2 To length(s)
  If substr(s,i,1)=c Then
    cnt=cnt+1
  Else Do
    Call o cnt||c
    c=substr(s,i,1)
    cnt=1
    End
  End
Call o cnt||c
Return ol

decode: Procedure
Parse Arg s
abc='ABCDEFGHIJKLMNOPQRSTUVWXYZ'
ol=''
Do While s<>''
  p=verify(s,abc,'M')
  Parse Var s cnt =(p) c +1 s
  Call o copies(c,cnt)
  End
Return ol

o: ol=ol||arg(1)
   Return
```

{{out}}

```txt
  s=WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW
enc=12W1B12W3B24W1B14W
dec=WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW
OK
```



### version 3

No need to output counts that are 1

```rexx
s='WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW'
Say '  s='s
enc=encode(s)
Say 'enc='enc
dec=decode(enc)
Say 'dec='dec
if dec==s Then Say 'OK'
Exit

encode: Procedure
Parse Arg s
c=left(s,1)
cnt=1
ol=''
Do i=2 To length(s)
  If substr(s,i,1)=c Then
    cnt=cnt+1
  Else Do
    If cnt=1 Then
      Call o c
    Else
      Call o cnt||c
    c=substr(s,i,1)
    cnt=1
    End
  End
Call o cnt||c
Return ol

decode: Procedure
Parse Arg s
abc='ABCDEFGHIJKLMNOPQRSTUVWXYZ'
ol=''
Do While s<>''
  p=verify(s,abc,'M')
  If pos(left(s,1),abc)>0 Then Do
    Parse Var s c +1 s
    Call o c
    End
  Else Do
    Parse Var s cnt =(p) c +1 s
    Call o copies(c,cnt)
    End
  End
Return ol

o: ol=ol||arg(1)
   Return
```

{{out}}

```txt
  s=WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW
enc=12WB12W3B24WB14W
dec=WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW
OK
```



## Ring


```ring

# Project : Run-length encoding

load "stdlib.ring"
test = "WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW"
num = 0
nr = 0
decode = newlist(7,2)
for n = 1 to len(test) - 1
     if test[n] = test[n+1]
        num = num + 1
     else
        nr = nr + 1
        decode[nr][1] = (num + 1)
        decode[nr][2] = test[n]
        see "" + (num + 1) + test[n]
        num = 0
     ok
next
see "" + (num + 1) + test[n]
see nl
nr = nr + 1
decode[nr][1] = (num + 1)
decode[nr][2] = test[n]
for n = 1 to len(decode)
     dec = copy(decode[n][2], decode[n][1])
     see dec
next

```

Output:

```txt

12W1B12W3B24W1B14W
WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW

```



## Ruby



'''Built-in'''


Ruby has built-in run-length encoding in the form of <code>chunk</code>, here I provide a thin wrapper around it:


```ruby

# run_encode("aaabbbbc") #=> [["a", 3], ["b", 4], ["c", 1]]
def run_encode(string)
  string
    .chars
    .chunk{|i| i}
    .map {|kind, array| [kind, array.length]}
end

# run_decode([["a", 3], ["b", 4], ["c", 1]]) #=> "aaabbbbc"
def run_decode(char_counts)
  char_counts
    .map{|char, count| char * count}
    .join
end


```



```ruby
def encode(string)
  string.scan(/(.)(\1*)/).collect do |char, repeat|
    [1 + repeat.length, char]
  end.join
end

def decode(string)
  string.scan(/(\d+)(\D)/).collect {|length, char| char * length.to_i}.join
end
```


This usage also seems to be idiomatic, and perhaps less cryptic:

```ruby
def encode(string)
  string.scan(/(.)(\1*)/).inject("") do |encoding, (char, repeat)|
    encoding << (1 + repeat.length).to_s << char
  end
end

def decode(string)
  string.scan(/(\d+)(\D)/).inject("") do |decoding, (length, char)|
    decoding << char * length.to_i
  end
end
```



'''By regular expression'''

The simplified input range of only uppercase characters allows a simple regular expression to be applied repeatedly for encoding, and another for decoding:

```ruby
def encode(str)
    str.gsub(/(.)\1*/) {$&.length.to_s + $1}
end

def decode(str)
    str.gsub(/(\d+)(\D)/) {$2 * $1.to_i}
end
```


'''Test:'''

```ruby
orig = "WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW"
p enc = encode(orig)
p dec = decode(enc)
puts "success!" if dec == orig
```


{{out}}

```txt

"12W1B12W3B24W1B14W"
"WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW"
success!

```



## Run BASIC


```runbasic
string$ = "WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW"
beg = 1
i   = 1
[loop]
s$ = mid$(string$,beg,1)
while mid$(string$,i,1) = s$
  i = i + 1
wend
press$ = press$ ; i-beg;s$
beg    = i
if i < len(string$) then goto [loop]
print "Compressed:";press$

beg = 1
i   = 1
[expand]
while mid$(press$,i,1) <= "9"
  i = i + 1
wend
for j = 1 to val(mid$(press$,beg, i - beg))
  expand$ = expand$ + mid$(press$,i,1)
next j
i   = i + 1
beg = i
if i < len(press$) then goto [expand]
print "  Expanded:";expand$
```
Output:

```txt
Compressed:12W1B12W3B24W1B14W
  Expanded:WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW
```



## Scala

Care is taken to use StringBuilder for performance reasons.


```scala
def encode(s: String) = (1 until s.size).foldLeft((1, s(0), new StringBuilder)) {
  case ((len, c, sb), index) if c != s(index) => sb.append(len); sb.append(c); (1, s(index), sb)
  case ((len, c, sb), _) => (len + 1, c, sb)
} match {
  case (len, c, sb) => sb.append(len); sb.append(c); sb.toString
}

def decode(s: String) = {
  val sb = new StringBuilder
  val Code = """(\d+)([A-Z])""".r
  for (Code(len, c) <- Code findAllIn s) sb.append(c * len.toInt)
  sb.toString
}
```


A simpler (?) encoder:

```scala
def encode(s:String) = {
  s.foldLeft((0,s(0),""))( (t,c) => t match {case (i,p,s) => if (p==c) (i+1,p,s) else (1,c,s+i+p)})
    match {case (i,p,s) => s+i+p}
}
```


To make it faster (it's also faster than the longer implementation above) just replace '''""''' with '''new StringBuilder''' and '''s+i+p''' with '''{s.append(i);s.append(p)}'''


## Scheme


```scheme
(define (run-length-decode v)
   (apply string-append (map (lambda (p) (make-string (car p) (cdr p))) v)))

(define (run-length-encode s)
(let ((n (string-length s)))
(let loop ((i (- n 2)) (c (string-ref s (- n 1))) (k 1) (v '()))
(if (negative? i) (cons (cons k c) v)
    (let ((x (string-ref s i)))
    (if (char=? c x) (loop (- i 1) c (+ k 1) v)
                     (loop (- i 1) x 1 (cons (cons k c) v))))))))

(run-length-encode "WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW")
; ((12 . #\W) (1 . #\B) (12 . #\W) (3 . #\B) (24 . #\W) (1 . #\B) (14 . #\W))
(run-length-decode '((12 . #\W) (1 . #\B) (12 . #\W) (3 . #\B) (24 . #\W) (1 . #\B) (14 . #\W)))
; "WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW"
```



## sed

The encode script:

```sed

	/^$/ b
:start
	/^[0-9]/ b
	s/^/1/
:loop
	h
	/^9+([^0-9])\1+/ {
		s/^(9+).*/0\1/
		y/09/10/
		G
		s/^(.+)\n[0-9]+.(.*)/\1\2/
		b loop }
	/^[0-9]*[0-8]([^0-9])\1+/ {
		s/^[0-9]*([0-8]).*/\1/
		y/012345678/123456789/
		G
		s/^(.)\n([0-9]*)[0-8].(.*)/\2\1\3/
		b loop }
	/^[0-9]+9+([^0-9])\1+/ {
		s/^[0-9]*([0-8]9+).*/\1/
		y/0123456789/1234567890/
		G
		s/^(.+)\n([0-9]*)[0-8]9+.(.*)/\2\1\3/
		b loop }
	s/^([0-9]+.)(.*)/\2\1/
	b start

```


The decode script:

```sed

	/^$/ b
:start
	/^[^0-9]/ b
:loop
	/^1[^0-9]/ {
		s/^1(.)(\1*)(.*)/\3\1\2/
		b start }
	h
	/^[0-9]*[1-9][^0-9]/ {
		s/^[0-9]*([1-9]).*/\1/
		y/123456789/012345678/
		G
		s/^([0-8])\n([0-9]*)[1-9]([^0-9])(.*)/\2\1\3\3\4/
		b loop }
	/^[0-9]+0[^0-9]/ {
		s/^[0-9]*([1-9]0+)[^0-9].*/\1/
		y/0123456789/9012345678/
		G
		s/^([0-9]+)\n([0-9]*)[1-9]0+([^0-9])(.*)/\2\1\3\3\4/
		s/^0+//
		b loop }

```


Example (assuming the scripts reside in the files <code>encode.sed</code> and <code>decode.sed</code>):

```bash

sed -rf encode.sed <<< "foo oops"
# 1f2o1 2o1p1s

sed -rf decode.sed <<< "1f2o1 2o1p1s"
# foo oops

(sed -rf decode.sed | sed -rf encode.sed) <<< 1000.
# 1000.

```



## Seed7


```seed7
$ include "seed7_05.s7i";
  include "scanstri.s7i";

const func string: letterRleEncode (in string: data) is func
  result
    var string: result is "";
  local
    var char: code is ' ';
    var integer: index is 1;
  begin
    if length(data) <> 0 then
      code := data[1];
      repeat
        incr(index);
      until index > length(data) or code <> data[index];
      result := str(pred(index)) & str(code) & letterRleEncode(data[index ..]);
    end if;
  end func;

const func string: letterRleDecode (in var string: data) is func
  result
    var string: result is "";
  local
    var integer: count is 0;
  begin
    if length(data) <> 0 then
      count := integer parse getDigits(data);
      result := data[1 len 1] mult count & letterRleDecode(data[2 ..]);
    end if;
  end func;

const proc: main is func
  begin
    writeln(letterRleEncode("WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW"));
    writeln(letterRleDecode("12W1B12W3B24W1B14W"));
  end func;
```



## Sidef

First solution:

```ruby
func encode(str) {
  str.gsub(/((.)(\2*))/, {|a,b| "#{a.len}#{b}" });
}

func decode(str) {
  str.gsub(/(\d+)(.)/, {|a,b| b * a.to_i });
}
```

{{out}}

```txt
12W1B12W3B24W1B14W
```


Second solution, encoding the length into a byte:

```ruby
func encode(str) {
    str.gsub(/(.)(\1{0,254})/, {|a,b| b.len+1 -> chr + a});
}

func decode(str) {
     var chars = str.chars;
     var r = '';
     (chars.len/2 -> int).range.each { |i|
         r += (chars[2*i + 1] * chars[2*i].ord);
     }
     return r;
}
```

{{out}}

```txt
"\fW\1B\fW\3B\30W\1B\16W"
```



## Smalltalk

See [[Run-length encoding/Smalltalk]]

A "functional" version without RunArray:
{{works with|Smalltalk/X}} (and others)


```smalltalk
|compress decompress|
compress := [:string |
   String streamContents:[:out |
       |count prev|

       count := 0.
       (string,'*')  "trick to avoid final run handling in loop"
          inject:nil
          into:[:prevChar :ch |
              ch ~= prevChar ifTrue:[
                  count = 0 ifFalse:[
                      count printOn:out.
                      out nextPut:prevChar.
                      count := 0.
                  ].
              ].
              count := count + 1.
              ch
          ]
   ]
].

decompress := [:string |
   String streamContents:[:out |
       string readingStreamDo:[:in |
           [in atEnd] whileFalse:[
               |n ch|
               n := Integer readFrom:in.
               ch := in next.
               out next:n put:ch.
            ]
       ]
   ].
].
```



```smalltalk
compress value:'WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW'
-> '12W1B12W3B24W1B14W'

decompress value:'12W1B12W3B24W1B14W'
-> 'WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW'
```


Most Smalltalk dialects include a class named "RunArray", which can be used as:
{{works with|Smalltalk/X}}
{{works with|VisualWorks}}

```smalltalk
compress := [:string |
    String streamContents:[:out |
        string asRunArray runsDo:[:count :char |
            count printOn:out. out nextPut:char]]].
```



## SNOBOL4


{{works with|Macro Spitbol}}
{{works with|Snobol4+}}
{{works with|CSnobol}}


```SNOBOL4
*       # Encode RLE
        define('rle(str)c,n') :(rle_end)
rle     str len(1) . c :f(return)
        str span(c) @n =
        rle = rle n c :(rle)
rle_end

*       # Decode RLE
        define('elr(str)c,n') :(elr_end)
elr     str span('0123456789') . n len(1) . c = :f(return)
        elr = elr dupl(c,n) :(elr)
elr_end

*       # Test and display
        str = 'WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW'
        output = str;
        str = rle(str); output = str
        str = elr(str); output = str
end
```


Output:

```txt
WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW
12W1B12W3B24W1B14W
WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW
```



## SQL

{{works with|PL/pgSQL}}


* RLE encoding

```SQL

-- variable table
drop table if exists var;
create temp table var (	value varchar(1000) );
insert into var(value) select 'WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW';

-- select
with recursive
ints(num) as
(
	select 1
	union all
	select num+1
	from ints
	where num+1 <= length((select value from var))
)
,
chars(num,chr,nextChr,isGroupEnd) as
(
	select tmp.*, case when tmp.nextChr <> tmp.chr then 1 else 0 end groupEnds
	from (
	select num,
		   substring((select value from var), num, 1) chr,
		   (select substring((select value from var), num+1, 1)) nextChr
	from ints
	) tmp
)
select (select value from var) plain_text, (
	select string_agg(concat(cast(maxNoWithinGroup as varchar(10)) , chr), '' order by num)
	from (
		select *, max(noWithinGroup) over (partition by chr, groupNo) maxNoWithinGroup
		from (
			select	num,
					chr,
					groupNo,
					row_number() over( partition by chr, groupNo order by num) noWithinGroup
			from (
			select *, (select count(*)
					   from chars chars2
					   where chars2.isGroupEnd = 1 and
					   chars2.chr = chars.chr and
					   chars2.num < chars.num) groupNo
			from chars
			) tmp
		) sub
	) final
	where noWithinGroup = 1
	) Rle_Compressed

```


* RLE decoding

```SQL

-- variable table
DROP TABLE IF EXISTS var;
CREATE temp TABLE var (	VALUE VARCHAR(1000) );
INSERT INTO var(VALUE) SELECT '1A2B3C4D5E6F';

-- select
WITH recursive
ints(num) AS
(
	SELECT 1
	UNION ALL
	SELECT num+1
	FROM ints
	WHERE num+1 <= LENGTH((SELECT VALUE FROM var))
)
,
chars(num,chr,nextChr) AS
(
	SELECT tmp.*
	FROM (
	SELECT num,
		SUBSTRING((SELECT VALUE FROM var), num, 1) chr,
		(SELECT SUBSTRING((SELECT VALUE FROM var), num+1, 1)) nextChr
	FROM ints
	) tmp
)
,
charsWithGroup(num,chr,nextChr,group_no) AS
(
	SELECT *,(SELECT COUNT(*)
		FROM chars chars2
		WHERE chars2.chr !~ '[0-9]' AND
		chars2.num < chars.num) group_No
	FROM chars
)
,
charsWithGroupAndLetter(num,chr,nextChr,group_no,group_letter) AS
(
	SELECT *,(SELECT chr
		FROM charsWithGroup g2
		where g2.group_no = charsWithGroup.group_no
		ORDER BY num DESC
		LIMIT 1)
	FROM charsWithGroup
)
,
lettersWithCount(group_no,amount,group_letter) AS
(
	SELECT group_no, string_agg(chr, '' ORDER BY num), group_letter
	FROM charsWithGroupAndLetter
	WHERE chr ~ '[0-9]'
	GROUP BY group_no, group_letter
)
,
lettersReplicated(group_no,amount,group_letter, replicated_Letter) AS
(
	SELECT *, rpad(group_letter, cast(amount as int), group_letter)
	FROM lettersWithCount
)
select (SELECT value FROM var) rle_encoded,
	string_agg(replicated_Letter, '' ORDER BY group_no) decoded_string
FROM lettersReplicated

```



## Standard ML


```sml
fun encode str =
  let
    fun aux (sub, acc) =
      case Substring.getc sub
       of NONE           => rev acc
        | SOME (x, sub') =>
            let
              val (y, z) = Substring.splitl (fn c => c = x) sub'
            in
              aux (z, (x, Substring.size y + 1) :: acc)
            end
  in
    aux (Substring.full str, [])
  end

fun decode lst =
  concat (map (fn (c,n) => implode (List.tabulate (n, fn _ => c))) lst)
```

Example:

```txt

- encode "aaaaahhhhhhmmmmmmmuiiiiiiiaaaaaa";
val it = [(#"a",5),(#"h",6),(#"m",7),(#"u",1),(#"i",7),(#"a",6)]
  : (char * int) list
- decode [(#"a",5),(#"h",6),(#"m",7),(#"u",1),(#"i",7),(#"a",6)];
val it = "aaaaahhhhhhmmmmmmmuiiiiiiiaaaaaa" : string

```



## Swift

Using array as the internal representation of the encoded input:

```swift
import Foundation

// "WWWBWW" -> [(3, W), (1, B), (2, W)]
func encode(input: String) -> [(Int, Character)] {
    return input.characters.reduce([(Int, Character)]()) {
        if $0.last?.1 == $1 { var r = $0; r[r.count - 1].0++; return r }
        return $0 + [(1, $1)]
    }
}

// [(3, W), (1, B), (2, W)] -> "WWWBWW"
func decode(encoded: [(Int, Character)]) -> String {
    return encoded.reduce("") { $0 + String(count: $1.0, repeatedValue: $1.1) }
}

```


'''Usage:'''


```swift

let input = "WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW"
let output = decode(encode(input))
print(output == input)

```


{{Out}}


```txt
true
```


Converting encoded array into the string and then decoding it using NSScanner:


```swift
// "3W1B2W" -> "WWWBWW"
func decode(encoded: String) -> String {
    let scanner = NSScanner(string: encoded)
    var char: NSString? = nil
    var count: Int = 0
    var out = ""

    while scanner.scanInteger(&count) {
        while scanner.scanCharactersFromSet(NSCharacterSet.letterCharacterSet(), intoString: &char) {
            out += String(count: count, repeatedValue: Character(char as! String))
        }
    }

    return out
}

```



```swift
let encodedString = encode(input).reduce("") { $0 + "\($1.0)\($1.1)" }
print(encodedString)
let outputString = decode(encodedString)
print(outputString == input)

```


{{Out}}

```txt

12W1B12W3B24W1B14W
true

```



## Tcl

The encoding is an even-length list with elements <tt>{count char ...}</tt>

```tcl
proc encode {string} {
    set encoding {}
    # use a regular expression to match runs of one character
    foreach {run -} [regexp -all -inline {(.)\1+|.} $string] {
        lappend encoding [string length $run] [string index $run 0]
    }
    return $encoding
}

proc decode {encoding} {
    foreach {count char} $encoding  {
        append decoded [string repeat $char $count]
    }
    return $decoded
}
```



```tcl
set str "WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW"
set enc [encode $str] ;# ==> {12 W 1 B 12 W 3 B 24 W 1 B 14 W}
set dec [decode $enc]
if {$str eq $dec} {
    puts "success"
}
```



## TUSCRIPT


```tuscript

$$ MODE TUSCRIPT,{}
input="WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW",output=""
string=strings(input," ? ")
letter=ACCUMULATE(string,freq)
freq=SPLIT(freq),letter=SPLIT(letter)
output=JOIN(freq,"",letter)
output=JOIN(output,"")
PRINT input
PRINT output

```

Output:

```txt

WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW
12W1B12W3B24W1B14W

```



## Ursala

A standard library function, rlc, does most of the work for this task,
which is a second order function taking a binary predicate that decides
when consecutive items of an input list belong to the same run.

```Ursala
#import std
#import nat

encode = (rlc ==); *= ^lhPrNCT\~&h %nP+ length

decode = (rlc ~&l-=digits); *=zyNCXS ^|DlS/~& iota+ %np

test_data = 'WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW'

#show+

example =

<
   encode test_data,
   decode encode test_data>
```

The output shows an encoding of the test data, and a decoding of the encoding, which
matches the original test data.

```txt
12W1B12W3B24W1B14W
WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW
```



## VBA


```vb

Option Explicit

Sub Main()
Dim p As String
   p = length_encoding("WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW")
   Debug.Print p
   Debug.Print length_decoding(p)
End Sub

Private Function length_encoding(S As String) As String
Dim F As String, r As String, a As String, n As Long, c As Long, k As Long
   r = Left(S, 1)
   c = 1
   For n = 2 To Len(S)
      If r <> Mid(S, n, 1) Then
         a = a & c & r
         r = Mid(S, n, 1)
         c = 1
      Else
         c = c + 1
      End If
   Next
   length_encoding = a & c & r
End Function

Private Function length_decoding(S As String) As String
Dim F As Long, r As String, a As String
   For F = 1 To Len(S)
      If IsNumeric(Mid(S, F, 1)) Then
         r = r & Mid(S, F, 1)
      Else
         a = a & String(CLng(r), Mid(S, F, 1))
         r = vbNullString
      End If
   Next
   length_decoding = a
End Function
```

{{out}}

```txt
12W1B12W3B24W1B14W
WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW
```



## Vedit macro language

The following example encodes/decodes an entire file.
Each run is coded with two bytes. The first byte is the run length with high bit set, the second byte is the character code.
ASCII characters with run length of 1 are left unchanged. Character codes above 127 are always coded with run length.
Newlines are not converted (the regular expression does not count newlines).
This methods supports any type of input.

```vedit
:RL_ENCODE:
BOF
While (!At_EOF) {
    if (At_EOL) { Line(1) Continue }    // skip newlines
    #1 = Cur_Char                       // #1 = character
    Match("(.)\1*", REGEXP)             // count run length
    #2 = Chars_Matched                  // #2 = run length
    if (#2 > 127) { #2 = 127 }          // can be max 127
    if (#2 > 1 || #1 > 127) {
        Del_Char(#2)
        Ins_Char(#2 | 128)              // run length (high bit set)
        Ins_Char(#1)                    // character
    } else {                            // single ASCII char
        Char                            // skip
    }
}
Return

:RL_DECODE:
BOF
While (!At_EOF) {
    #2 = Cur_Char
    if (#2 > 127) {                     // is this run length?
        #1 = Cur_Char(1)                // #1 = character value
        Del_Char(2)
        Ins_Char(#1, COUNT, #2 & 127)
    } else {                            // single ASCII char
        Char
    }
}
Return
```



## XPL0


```XPL0
include c:\cxpl\codes;  \intrinsic 'code' declarations
string 0;               \use zero-terminated strings, instead of MSb terminated

proc Compress(S);       \Compress string using run-length encoding, & display it
char S;
int  I, C0, C, N;
[I:= 0;
C0:= S(I);  I:= I+1;
repeat  ChOut(0, C0);
        N:= 0;
        repeat  C:= S(I);  I:= I+1;
                N:= N+1;
        until   C#C0;
        if N>1 then IntOut(0, N-1);
        C0:= C;
until   C=0;
]; \Compress

proc Expand(S);         \Expand compressed string, and display it
char S;
int  I, C0, C, N;
[I:= 0;
C0:= S(I);  I:= I+1;
repeat  ChOut(0, C0);
        C:= S(I);  I:= I+1;
        if C>=^1 & C<=^9 then
                [N:= 0;
                while C>=^0 & C<=^9 do
                        [N:= N*10 + C-^0;
                        C:= S(I);  I:= I+1;
                        ];
                while N do [ChOut(0, C0);  N:= N-1];
                ];
        C0:= C;
until   C=0;
]; \Expand

[Compress("WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW");
CrLf(0);
Expand("W11BW11B2W23BW13");  CrLf(0);
]
```


Output (with slightly better compression than the example):

```txt

W11BW11B2W23BW13
WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW

```



## zkl

A byte stream to byte stream codec, the streams hard coded for ease. Character '\1' is used as a sentinel and there are limits on the run length to avoid byte overflow.

```zkl
const MAX_LEN=250, MIN_LEN=3;
fcn compress(text){ // !empty byte/text stream -->Data (byte stream)
   sink:=Data(); cnt:=Ref(0);
   write:='wrap(c,n){ // helper function
      while(n>MAX_LEN){
         sink.write(1); sink.write(MAX_LEN); sink.write(c);
	 n-=MAX_LEN;
      }
      if(n>MIN_LEN){ sink.write(1); sink.write(n); sink.write(c); }
      else { do(n) { sink.write(c); } }
   };
   text.reduce('wrap(a,b){
      if(a==b) cnt.inc();
      else{ write(a,cnt.value); cnt.set(1); }
      b
   },text[0]) : write(_,cnt.value);
   sink;
}
```


```zkl
fcn inflate(data){  //-->String
   data.howza(3).pump(String,
      fcn(c){ // if c==1, read n,c2 and expand, else write c
         if(c=="\x01") return(Void.Read,2) else return(Void.Write,c) },
      fcn(_,n,c){ c*n.toAsc() })
}
```


```zkl
text:="WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW";
d:=compress(text);
d.bytes().println();
println(text.len()," bytes --> ",d.len()," bytes");
println(text==inflate(d));
```

{{out}}

```txt

L(1,12,87,66,1,12,87,66,66,66,1,24,87,66,1,14,87)
67 bytes --> 17 bytes
True

```

