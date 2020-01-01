+++
title = "URL decoding"
description = ""
date = 2019-10-14T11:22:50Z
aliases = []
[extra]
id = 9937
[taxonomies]
categories = []
tags = []
+++

{{task}}
[[Category:String manipulation]]

This task   (the reverse of   [[URL encoding]]   and distinct from   [[URL parser]])   is to provide a function
or mechanism to convert an URL-encoded string into its original unencoded form.


;Test cases:
*   The encoded string   "<code><nowiki>http%3A%2F%2Ffoo%20bar%2F</nowiki></code>"   should be reverted to the unencoded form   "<code><nowiki>http://foo bar/</nowiki></code>".

*   The encoded string   "<code><nowiki>google.com/search?q=%60Abdu%27l-Bah%C3%A1</nowiki></code>"   should revert to the unencoded form   "<code><nowiki>google.com/search?q=`Abdu'l-Bahá</nowiki></code>".





## ABAP



```ABAP
REPORT Z_DECODE_URL.

DATA: lv_encoded_url TYPE string VALUE 'http%3A%2F%2Ffoo%20bar%2F',
      lv_decoded_url TYPE string.

CALL METHOD CL_HTTP_UTILITY=>UNESCAPE_URL
  EXPORTING
    ESCAPED   = lv_encoded_url
  RECEIVING
    UNESCAPED = lv_decoded_url.

WRITE: 'Encoded URL: ', lv_encoded_url, /, 'Decoded URL: ', lv_decoded_url.
```



## Ada

{{libheader|AWS}}

```Ada
with AWS.URL;
with Ada.Text_IO; use Ada.Text_IO;
procedure Decode is
   Encoded : constant String := "http%3A%2F%2Ffoo%20bar%2F";
begin
   Put_Line (AWS.URL.Decode (Encoded));
end Decode;

```


Without external libraries:


```Ada
package URL is
   function Decode (URL : in String) return String;
end URL;
```



```Ada
package body URL is
   function Decode (URL : in String) return String is
      Buffer   : String (1 .. URL'Length);
      Filled   : Natural := 0;
      Position : Positive := URL'First;
   begin
      while Position in URL'Range loop
         Filled := Filled + 1;

        case URL (Position) is
            when '+' =>
               Buffer (Filled) := ' ';
               Position := Position + 1;
            when '%' =>
               Buffer (Filled) :=
                 Character'Val
                   (Natural'Value
                      ("16#" & URL (Position + 1 .. Position + 2) & "#"));
               Position := Position + 3;
            when others =>
               Buffer (Filled) := URL (Position);
               Position := Position + 1;
         end case;
      end loop;

      return Buffer (1 .. Filled);
   end Decode;
end URL;
```



```Ada
with Ada.Command_Line,
     Ada.Text_IO;

with URL;

procedure Test_URL_Decode is
   use Ada.Command_Line, Ada.Text_IO;
begin
   if Argument_Count = 0 then
      Put_Line (URL.Decode ("http%3A%2F%2Ffoo%20bar%2F"));
   else
      for I in 1 .. Argument_Count loop
         Put_Line (URL.Decode (Argument (I)));
      end loop;
   end if;
end Test_URL_Decode;
```



## Apex


```apex
EncodingUtil.urlDecode('http%3A%2F%2Ffoo%20bar%2F', 'UTF-8');
EncodingUtil.urlDecode('google.com/search?q=%60Abdu%27l-Bah%C3%A1', 'UTF-8');
```


```txt
http://foo bar/
google.com/search?q=`Abdu'l-Bahá
```



## AppleScript

{{libheader|AppleScript Toolbox}}

```AppleScript
AST URL decode "google.com/search?q=%60Abdu%27l-Bah%C3%A1"
```



## Arturo



```arturo
url1 "http%3A%2F%2Ffoo%20bar%2F"
print $(decodeUrl url1)

url2 "google.com/search?q=%60Abdu%27l-Bah%C3%A1"
print $(decodeUrl url2)
```


{{out}}


```txt
http://foo bar/
google.com/search?q=`Abdu'l-Bahá
```



## AutoHotkey


```AutoHotkey
encURL := "http%3A%2F%2Ffoo%20bar%2F"
SetFormat, Integer, hex
Loop Parse, encURL
   If A_LoopField = `%
      reading := 2, read := ""
   else if reading
   {
      read .= A_LoopField, --reading
      if not reading
         out .= Chr("0x" . read)
   }
   else out .= A_LoopField
MsgBox % out ; http://foo bar/

```



## AWK


```AWK

# syntax:
awk '
BEGIN {
    str = "http%3A%2F%2Ffoo%20bar%2F" # "http://foo bar/"
    printf("%s\n",str)
    len=length(str)
    for (i=1;i<=len;i++) {
      if ( substr(str,i,1) == "%") {
        L = substr(str,1,i-1) # chars to left of "%"
        M = substr(str,i+1,2) # 2 chars to right of "%"
        R = substr(str,i+3)   # chars to right of "%xx"
        str = sprintf("%s%c%s",L,hex2dec(M),R)
      }
    }
    printf("%s\n",str)
    exit(0)
}
function hex2dec(s,  num) {
    num = index("0123456789ABCDEF",toupper(substr(s,length(s)))) - 1
    sub(/.$/,"",s)
    return num + (length(s) ? 16*hex2dec(s) : 0)
} '

```

{{out}}

```txt

http%3A%2F%2Ffoo%20bar%2F
http://foo bar/

```



## BaCon


```freebasic
FUNCTION Url_Decode$(url$)

    LOCAL result$

    SPLIT url$ BY "%" TO item$ SIZE total
    FOR x = 1 TO total-1
        result$ = result$ & CHR$(DEC(LEFT$(item$[x], 2))) & MID$(item$[x], 3)
    NEXT
    RETURN item$[0] & result$

END FUNCTION

PRINT Url_Decode$("http%3A%2F%2Ffoo%20bar%2F")
PRINT Url_Decode$("google.com/search?q=%60Abdu%27l-Bah%C3%A1")
```

{{out}}

```txt

http://foo bar/
google.com/search?q=`Abdu'l-Bahá

```



## BBC BASIC

{{works with|BBC BASIC for Windows}}

```bbcbasic
      PRINT FNurldecode("http%3A%2F%2Ffoo%20bar%2F")
      END

      DEF FNurldecode(url$)
      LOCAL i%
      REPEAT
        i% = INSTR(url$, "%", i%+1)
        IF i% THEN
          url$ = LEFT$(url$,i%-1) + \
          \      CHR$EVAL("&"+FNupper(MID$(url$,i%+1,2))) + \
          \      MID$(url$,i%+3)
        ENDIF
      UNTIL i% = 0
      = url$

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

http://foo bar/

```



## Bracmat


```bracmat
(     ( decode
      =   decoded hexcode notencoded
        .   :?decoded
          &   whl
            ' ( @(!arg:?notencoded "%" (% %:?hexcode) ?arg)
              & !decoded !notencoded chr$(x2d$!hexcode):?decoded
              )
          & str$(!decoded !arg)
      )
    & out$(decode$http%3A%2F%2Ffoo%20bar%2F)
);
```

{{out}}

```txt
http://foo bar/
```



## C


```c
#include <stdio.h>
#include <string.h>

inline int ishex(int x)
{
	return	(x >= '0' && x <= '9')	||
		(x >= 'a' && x <= 'f')	||
		(x >= 'A' && x <= 'F');
}

int decode(const char *s, char *dec)
{
	char *o;
	const char *end = s + strlen(s);
	int c;

	for (o = dec; s <= end; o++) {
		c = *s++;
		if (c == '+') c = ' ';
		else if (c == '%' && (	!ishex(*s++)	||
					!ishex(*s++)	||
					!sscanf(s - 2, "%2x", &c)))
			return -1;

		if (dec) *o = c;
	}

	return o - dec;
}

int main()
{
	const char *url = "http%3A%2F%2ffoo+bar%2fabcd";
	char out[strlen(url) + 1];

	printf("length: %d\n", decode(url, 0));
	puts(decode(url, out) < 0 ? "bad string" : out);

	return 0;
}
```



## C++

{{libheader|Poco}}
{{works with|g++}}

```cpp
#include <string>
#include "Poco/URI.h"
#include <iostream>

int main( ) {
   std::string encoded( "http%3A%2F%2Ffoo%20bar%2F" ) ;
   std::string decoded ;
   Poco::URI::decode ( encoded , decoded ) ;
   std::cout << encoded << " is decoded: " << decoded << " !" << std::endl ;
   return 0 ;
}
```

{{out}}

```txt
http%3A%2F%2Ffoo%20bar%2F is decoded: http://foo bar/ !
```



## C sharp



```csharp
using System;

namespace URLEncode
{
    internal class Program
    {
        private static void Main(string[] args)
        {
            Console.WriteLine(Decode("http%3A%2F%2Ffoo%20bar%2F"));
        }

        private static string Decode(string uri)
        {
            return Uri.UnescapeDataString(uri);
        }
    }
}
```


{{out}}

```txt

http://foo bar/

```


=={{header|Caché ObjectScript}}==


```txt
USER>Write $ZConvert("http%3A%2F%2Ffoo%20bar%2F", "I", "URL")
http://foo bar/
```



## Clojure



```clojure
(java.net.URLDecoder/decode "http%3A%2F%2Ffoo%20bar%2F")
```



## CoffeeScript



```coffeescript

console.log decodeURIComponent "http%3A%2F%2Ffoo%20bar%2F?name=Foo%20Barson"

```


<lang>
> coffee foo.coffee
http://foo bar/?name=Foo Barson

```



## Common Lisp


```lisp
(defun decode (string &key start)
  (assert (char= (char string start) #\%))
  (if (>= (length string) (+ start 3))
      (multiple-value-bind (code pos)
          (parse-integer string :start (1+ start) :end (+ start 3) :radix 16 :junk-allowed t)
        (if (= pos (+ start 3))
            (values (code-char code) pos)
            (values #\% (1+ start))))
      (values #\% (1+ start))))

(defun url-decode (url)
  (loop with start = 0
        for pos = (position #\% url :start start)
        collect (subseq url start pos) into chunks
        when pos
          collect (multiple-value-bind (decoded next) (decode url :start pos)
                    (setf start next)
                    (string decoded))
            into chunks
        while pos
        finally (return (apply #'concatenate 'string chunks))))

(url-decode "http%3A%2F%2Ffoo%20bar%2F")
```

{{out}}

```txt
"http://foo bar/"
```



## D


```d
import std.stdio, std.uri;

void main() {
    writeln(decodeComponent("http%3A%2F%2Ffoo%20bar%2F"));
}
```


```txt
http://foo bar/
```



## Delphi


```Delphi
program URLEncoding;

{$APPTYPE CONSOLE}

uses IdURI;

begin
  Writeln(TIdURI.URLDecode('http%3A%2F%2Ffoo%20bar%2F'));
end.
```



## Elixir


```elixir
IO.inspect URI.decode("http%3A%2F%2Ffoo%20bar%2F")
IO.inspect URI.decode("google.com/search?q=%60Abdu%27l-Bah%C3%A1")
```


{{out}}

```txt

"http://foo bar/"
"google.com/search?q=`Abdu'l-Bahá"

```



## Erlang

Built in.

```txt

34> http_uri:decode("http%3A%2F%2Ffoo%20bar%2F").
"http://foo bar/"

```


=={{header|F_Sharp|F#}}==
{{trans|C#}}

```fsharp
open System

let decode uri = Uri.UnescapeDataString(uri)

[<EntryPoint>]
let main argv =
    printfn "%s" (decode "http%3A%2F%2Ffoo%20bar%2F")
    0
```



## Factor


```factor
USING: io kernel urls.encoding ;
IN: rosetta-code.url-decoding

"http%3A%2F%2Ffoo%20bar%2F"
"google.com/search?q=%60Abdu%27l-Bah%C3%A1"
[ url-decode print ] bi@
```

{{out}}

```txt

http://foo bar/
google.com/search?q=`Abdu'l-Bahá

```



## Free Pascal



```pascal
function urlDecode(data: String): AnsiString;
var
  ch: Char;
  pos, skip: Integer;

begin
  pos := 0;
  skip := 0;
  Result := '';

  for ch in data do begin
    if skip = 0 then begin
      if (ch = '%') and (pos < data.length -2) then begin
        skip := 2;
        Result := Result + AnsiChar(Hex2Dec('$' + data[pos+2] + data[pos+3]));

      end else begin
        Result := Result + ch;
      end;

    end else begin
      skip := skip - 1;
    end;
    pos := pos +1;
  end;
end;
```



## Go


```go
package main

import (
	"fmt"
	"log"
	"net/url"
)

func main() {
	for _, escaped := range []string{
		"http%3A%2F%2Ffoo%20bar%2F",
		"google.com/search?q=%60Abdu%27l-Bah%C3%A1",
	} {
		u, err := url.QueryUnescape(escaped)
		if err != nil {
			log.Println(err)
			continue
		}
		fmt.Println(u)
	}
}
```

{{out}}

```txt

http://foo bar/
google.com/search?q=`Abdu'l-Bahá

```



## Groovy


```groovy
assert URLDecoder.decode('http%3A%2F%2Ffoo%20bar%2F') == 'http://foo bar/'
```



## Haskell


```Haskell
import qualified Data.Char as Char

urlDecode :: String -> Maybe String
urlDecode [] = Just []
urlDecode ('%':xs) =
  case xs of
    (a:b:xss) ->
      urlDecode xss
      >>= return . ((Char.chr . read $ "0x" ++ [a,b]) :)
    _ -> Nothing
urlDecode ('+':xs) = urlDecode xs >>= return . (' ' :)
urlDecode (x:xs) = urlDecode xs >>= return . (x :)

main :: IO ()
main = putStrLn . maybe "Bad decode" id $ urlDecode "http%3A%2F%2Ffoo%20bar%2F"
```

{{out}}

```txt
http://foo bar/
```


Another approach:

```haskell
import Data.Char (chr)
import Data.List.Split (splitOn)

deCode :: String -> String
deCode url =
  let ps = splitOn "%" url
  in concat $
     head ps :
     ((\(a, b) -> (chr . read) (mappend "0x" a) : b) <$> (splitAt 2 <$> tail ps))

-- TEST ------------------------------------------------------------------------
main :: IO ()
main = putStrLn $ deCode "http%3A%2F%2Ffoo%20bar%2F"
```

{{Out}}

```txt
http://foo bar/
```


=={{header|Icon}} and {{header|Unicon}}==

```Icon
link hexcvt

procedure main()
ue := "http%3A%2F%2Ffoo%20bar%2F"
ud := decodeURL(ue) | stop("Improperly encoded string ",image(ue))
write("encoded = ",image(ue))
write("decoded = ",image(ue))
end

procedure decodeURL(s)              #: decode URL/URI encoded data
static de
initial {                           # build lookup table for everything
  de := table()
  every de[hexstring(ord(c := !string(&ascii)),2)] := c
  }

c := ""
s ? until pos(0) do                 # decode every %xx or fail
   c ||:= if ="%" then \de[move(2)] | fail
   else move(1)
return c
end
```


{{libheader|Icon Programming Library}}
[http://www.cs.arizona.edu/icon/library/src/procs/hexcvt.icn hexcvt provides hexstring]

{{out}}

```txt
encoded = "http%3A%2F%2Ffoo%20bar%2F"
decoded = "http://foo bar/"
```



## J

J does not have a native urldecode (until version 7 when the <code>jhs</code> ide addon includes a jurldecode).

Here is an implementation:


```j
require'strings convert'
urldecode=: rplc&(~.,/;"_1&a."2(,:tolower)'%',.toupper hfd i.#a.)
```


Example use:


```j
   urldecode 'http%3A%2F%2Ffoo%20bar%2F'
http://foo bar/
```


Note that an earlier implementation assumed the j6 implementation of <code>hfd</code> which where hexadecimal letters resulting from <code>hfd</code> were upper case. J8, in contrast, provides a lower case result from hfd. The addition of <code>toupper</code> guarantees the case insensitivity required by [http://tools.ietf.org/html/rfc3986#section-2.1 RFC 3986] regardless of which version of J you are using. As the parenthesized expression containing <code>hfd</code> is only evaluated at definition time, there's no performance penalty from the use of <code>toupper</code>.

Example use:


```j
   urldecode 'google.com/search?q=%60Abdu%27l-Bah%C3%A1'
google.com/search?q=`Abdu'l-Bahá
```



## Java



```java
import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;

public class Main
{
    public static void main(String[] args) throws UnsupportedEncodingException
    {
        String encoded = "http%3A%2F%2Ffoo%20bar%2F";
        String normal = URLDecoder.decode(encoded, "utf-8");
        System.out.println(normal);
    }
}
```


{{out}}

```txt
http://foo bar/
```



## JavaScript


```javascript
decodeURIComponent("http%3A%2F%2Ffoo%20bar%2F")
```



## jq

{{works with|jq|1.4}}
If your jq already has "until", then the definition given below may be omitted.

```jq
# Emit . and stop as soon as "condition" is true.
def until(condition; next):
  def u: if condition then . else (next|u) end;
  u;

def url_decode:
  # The helper function converts the input string written in the given
  # "base" to an integer
  def to_i(base):
    explode
    | reverse
    | map(if 65 <= . and . <= 90 then . + 32  else . end)   # downcase
    | map(if . > 96  then . - 87 else . - 48 end)  # "a" ~ 97 => 10 ~ 87
    | reduce .[] as $c
        # base: [power, ans]
        ([1,0]; (.[0] * base) as $b | [$b, .[1] + (.[0] * $c)]) | .[1];

  .  as $in
  | length as $length
  | [0, ""]  # i, answer
  | until ( .[0] >= $length;
      .[0] as $i
      |  if $in[$i:$i+1] == "%"
         then [ $i + 3, .[1] + ([$in[$i+1:$i+3] | to_i(16)] | implode) ]
         else [ $i + 1, .[1] + $in[$i:$i+1] ]
         end)
  | .[1];  # answer
```

'''Example''':

```jq
"http%3A%2F%2Ffoo%20bar%2F" |  url_decode
```

{{out}}

```sh

"http://foo bar/"
```



## Julia


```Julia

using URIParser

enc = "http%3A%2F%2Ffoo%20bar%2F"

dcd = unescape(enc)

println(enc, " => ", dcd)

```


{{out}}

```txt

http%3A%2F%2Ffoo%20bar%2F => http://foo bar/

```



## Kotlin


```scala
// version 1.1.2

import java.net.URLDecoder

fun main(args: Array<String>) {
    val encoded = arrayOf("http%3A%2F%2Ffoo%20bar%2F", "google.com/search?q=%60Abdu%27l-Bah%C3%A1")
    for (e in encoded) println(URLDecoder.decode(e, "UTF-8"))
}
```


{{out}}

```txt

http://foo bar/
google.com/search?q=`Abdu'l-Bahá

```



## Lasso


```Lasso
bytes('http%3A%2F%2Ffoo%20bar%2F') -> decodeurl
```

-> http://foo bar/


## Liberty BASIC


```lb

    dim lookUp$( 256)

    for i =0 to 256
        lookUp$( i) ="%" +dechex$( i)
    next i

    url$ ="http%3A%2F%2Ffoo%20bar%2F"

    print "Supplied URL '"; url$; "'"
    print "As string    '"; url2string$( url$); "'"

    end

function url2string$( i$)
    for j =1 to len( i$)
        c$ =mid$( i$, j, 1)
        if c$ ="%" then
            nc$         =chr$( hexdec( mid$( i$, j +1, 2)))
            url2string$ =url2string$ +nc$
            j =j +2
        else
            url2string$ =url2string$ +c$
        end if
    next j
end function

```

 Supplied URL 'http%3A%2F%2Ffoo%20bar%2F'
 As string 'http://foo bar/'


## Lingo


```Lingo
----------------------------------------
-- URL decodes a string
-- @param {string} str
-- @return {string}
----------------------------------------
on urldecode (str)
    res = ""
    ba = bytearray()
    len = str.length
    repeat with i = 1 to len
        c = str.char[i]
        if (c = "%") then
            -- fastest hex-to-dec conversion hack based on Lingo's rgb object
            ba.writeInt8(rgb(str.char[i+1..i+2]).blue)
            i = i + 2
        else if (c = "+") then
            ba.writeInt8(32)
        else
            ba.writeInt8(chartonum(c))
        end if
    end repeat
    ba.position = 1
    return ba.readRawString(ba.length)
end
```


```Lingo
put urldecode("http%3A%2F%2Ffoo%20bar%2F")
put urldecode("google.com/search?q=%60Abdu%27l-Bah%C3%A1")
```

{{Out}}

```txt

-- "http://foo bar/"
-- "google.com/search?q=`Abdu'l-Bahá"

```



## LiveCode


```LiveCode
put urlDecode("http%3A%2F%2Ffoo%20bar%2F") & cr & \
    urlDecode("google.com/search?q=%60Abdu%27l-Bah%C3%A1")
```
Results<lang>http://foo bar/
google.com/search?q=`Abdu'l-Bah√°
```



## Lua


```lua
function decodeChar(hex)
	return string.char(tonumber(hex,16))
end

function decodeString(str)
	local output, t = string.gsub(str,"%%(%x%x)",decodeChar)
	return output
end

-- will print "http://foo bar/"
print(decodeString("http%3A%2F%2Ffoo%20bar%2F"))
```



## M2000 Interpreter

Function Len(string) return length in words, so a value 1.5 means 3 bytes

We can add strings with half word at the end of a series of words.

A$=str$("A") has a length of 0.5

b$=chr$(a$) revert bytes to words adding zeroes after each character

```M2000 Interpreter

Module CheckIt {
      Function decodeUrl$(a$) {
            DIM a$()
            a$()=Piece$(a$, "%")
            if len(a$())=1 then =str$(a$):exit
            k=each(a$(),2)
            \\ convert to one byte per character using str$(string)
            acc$=str$(a$(0))
            While k {
                        \\ chr$() convert to UTF16LE
                        \\ str$()  convert to ANSI using locale (can be 1033 we can set it before as Locale 1033)
                        \\ so chr$(0x93) give 0x201C
                        \\ str$(chr$(0x93)) return one byte 93 in ANSI as string of one byte length
                        \\ numbers are for UTF-8 so we have to preserve them
                  acc$+=str$(Chr$(Eval("0x"+left$(a$(k^),2)))+Mid$(a$(k^),3))
            }
            =acc$
      }
      \\ decode from utf8
      final$=DecodeUrl$("google.com/search?q=%60Abdu%27l-Bah%C3%A1")
      Print string$(final$ as utf8dec)="google.com/search?q=`Abdu'l-Bahá"
      final$=DecodeUrl$("http%3A%2F%2Ffoo%20bar%2F")
      Print string$(final$ as utf8dec)="http://foo bar/"
}
CheckIt

```



## Maple


```Maple
StringTools:-Decode("http%3A%2F%2Ffoo%20bar%2F", encoding=percent);
```


{{Out}}

```txt
                           "http://foo bar/"
```



## Mathematica


```mathematica
URLDecoding[url_] :=
 StringReplace[url, "%" ~~ x_ ~~ y_ :> FromDigits[x ~~ y, 16]] //.
  StringExpression[x___, Longest[n__Integer], y___] :>
   StringExpression[x, FromCharacterCode[{n}, "UTF8"], y]
```


Example use:


```mathematica
URLDecoding["http%3A%2F%2Ffoo%20bar%2F"]
```


{{out}}

```txt
http://foo bar/
```


=={{header|MATLAB}} / {{header|Octave}}==

```MATLAB
function u = urldecoding(s)
    u = '';
    k = 1;
    while k<=length(s)
        if s(k) == '%' && k+2 <= length(s)
            u = sprintf('%s%c', u, char(hex2dec(s((k+1):(k+2)))));
            k = k + 3;
        else
            u = sprintf('%s%c', u, s(k));
            k = k + 1;
        end
    end
end
```

Usage:

```txt
octave:3> urldecoding('http%3A%2F%2Ffoo%20bar%2F')
ans = http://foo bar/
```



## NetRexx


```NetRexx
/* NetRexx */
options replace format comments java crossref savelog symbols nobinary

url = [ -
  'http%3A%2F%2Ffoo%20bar%2F', -
  'mailto%3A%22Ivan%20Aim%22%20%3Civan%2Eaim%40email%2Ecom%3E', -
  '%6D%61%69%6C%74%6F%3A%22%49%72%6D%61%20%55%73%65%72%22%20%3C%69%72%6D%61%2E%75%73%65%72%40%6D%61%69%6C%2E%63%6F%6D%3E' -
  ]

loop u_ = 0 to url.length - 1
  say url[u_]
  say DecodeURL(url[u_])
  say
  end u_

return

method DecodeURL(arg) public static

  Parse arg encoded
  decoded = ''
  PCT = '%'

  loop label e_ while encoded.length() > 0
    parse encoded head (PCT) +1 code +2 tail
    decoded = decoded || head
    select
      when code.strip('T').length() = 2 & code.datatype('X') then do
        code = code.x2c()
        decoded = decoded || code
        end
      when code.strip('T').length() \= 0 then do
        decoded = decoded || PCT
        tail = code || tail
        end
      otherwise do
        nop
        end
      end
    encoded = tail
    end e_

  return decoded

```


{{out}}
<pre style="height: 20ex; overflow: scroll;">
http%3A%2F%2Ffoo%20bar%2F
http://foo bar/

mailto%3A%22Ivan%20Aim%22%20%3Civan%2Eaim%40email%2Ecom%3E
mailto:"Ivan Aim" <ivan.aim@email.com>

%6D%61%69%6C%74%6F%3A%22%49%72%6D%61%20%55%73%65%72%22%20%3C%69%72%6D%61%2E%75%73%65%72%40%6D%61%69%6C%2E%63%6F%6D%3E
mailto:"Irma User" <irma.user@mail.com>

```



## NewLISP


```NewLISP
;; universal decoder, works for ASCII and UTF-8
;; (source http://www.newlisp.org/index.cgi?page=Code_Snippets)
(define (url-decode url (opt nil))
  (if opt (replace "+" url " "))
  (replace "%([0-9a-f][0-9a-f])" url (pack "b" (int $1 0 16)) 1))

(url-decode "http%3A%2F%2Ffoo%20bar%2F")
```



## Nim


```nim
import cgi

echo decodeUrl("http%3A%2F%2Ffoo%20bar%2F")
```


=={{header|Oberon-2}}==
{{works with|oo2c}}

```oberon2

MODULE URLDecoding;
IMPORT
  URI := URI:String,
  Out := NPCT:Console;
BEGIN
  Out.String(URI.Unescape("http%3A%2F%2Ffoo%20bar%2F"));Out.Ln;
  Out.String(URI.Unescape("google.com/search?q=%60Abdu%27l-Bah%C3%A1"));Out.Ln;
END URLDecoding.

```

{{out}}

```txt

http://foo bar/
google.com/search?q=`Abdu'l-Bahá

```



## Objeck


```objeck

class UrlDecode {
  function : Main(args : String[]) ~ Nil {
    Net.UrlUtility->Decode("http%3A%2F%2Ffoo%20bar%2F")->PrintLine();
  }
}

```


=={{header|Objective-C}}==

```objc
NSString *encoded = @"http%3A%2F%2Ffoo%20bar%2F";
NSString *normal = [encoded stringByReplacingPercentEscapesUsingEncoding:NSUTF8StringEncoding];
NSLog(@"%@", normal);
```

{{works with|Mac OS X|10.9+}}{{works with|iOS|7+}}

```objc
NSString *encoded = @"http%3A%2F%2Ffoo%20bar%2F";
NSString *normal = [encoded stringByRemovingPercentEncoding];
NSLog(@"%@", normal);
```



## OCaml


Using the library [http://projects.camlcity.org/projects/ocamlnet.html ocamlnet] from the interactive loop:


```ocaml
$ ocaml
# #use "topfind";;
# #require "netstring";;

# Netencoding.Url.decode "http%3A%2F%2Ffoo%20bar%2F" ;;
- : string = "http://foo bar/"
```


== {{header|ooRexx}} ==
While the implementation shown for [[#REXX|Rexx]] will also work with ooRexx, this version uses ooRexx syntax to invoke the built-in functions.

```ooRexx
/* Rexx */
  X = 0
  url. = ''
  X = X + 1; url.0 = X; url.X = 'http%3A%2F%2Ffoo%20bar%2F'
  X = X + 1; url.0 = X; url.X = 'mailto%3A%22Ivan%20Aim%22%20%3Civan%2Eaim%40email%2Ecom%3E'
  X = X + 1; url.0 = X; url.X = '%6D%61%69%6C%74%6F%3A%22%49%72%6D%61%20%55%73%65%72%22%20%3C%69%72%6D%61%2E%75%73%65%72%40%6D%61%69%6C%2E%63%6F%6D%3E'

  Do u_ = 1 to url.0
    Say url.u_
    Say DecodeURL(url.u_)
    Say
    End u_

Exit

DecodeURL: Procedure

  Parse Arg encoded
  decoded = ''
  PCT = '%'

  Do label e_ while encoded~length() > 0
    Parse Var encoded head (PCT) +1 code +2 tail
    decoded = decoded || head
    Select
      when code~strip('T')~length() = 2 & code~datatype('X') then Do
        code = code~x2c()
        decoded = decoded || code
        End
      when code~strip('T')~length() \= 0 then Do
        decoded = decoded || PCT
        tail = code || tail
        End
      otherwise
        Nop
      End
    encoded = tail
    End e_

  Return decoded
```


{{out}}

```txt

http%3A%2F%2Ffoo%20bar%2F
http://foo bar/

mailto%3A%22Ivan%20Aim%22%20%3Civan%2Eaim%40email%2Ecom%3E
mailto:"Ivan Aim" <ivan.aim@email.com>

%6D%61%69%6C%74%6F%3A%22%49%72%6D%61%20%55%73%65%72%22%20%3C%69%72%6D%61%2E%75%73%65%72%40%6D%61%69%6C%2E%63%6F%6D%3E
mailto:"Irma User" <irma.user@mail.com>

```



## Perl



```Perl
sub urldecode {
    my $s = shift;
    $s =~ tr/\+/ /;
    $s =~ s/\%([A-Fa-f0-9]{2})/pack('C', hex($1))/eg;
    return $s;
}

print urldecode('http%3A%2F%2Ffoo+bar%2F')."\n";

```



```Perl
#!/usr/bin/perl -w
use strict ;
use URI::Escape ;

my $encoded = "http%3A%2F%2Ffoo%20bar%2F" ;
my $unencoded = uri_unescape( $encoded ) ;
print "The unencoded string is $unencoded !\n" ;
```



## Perl 6


```perl6
my $url = "http%3A%2F%2Ffoo%20bar%2F";

say $url.subst: :g,
    /'%'(<:hexdigit>**2)/,
    ->  ($ord          ) { chr(:16(~$ord)) }

# Alternately, you can use an in-place substitution:
$url ~~ s:g[ '%' (<:hexdigit> ** 2) ] = chr :16(~$0);
say $url;
```



## Phix


```Phix
--
-- demo\rosetta\decode_url.exw
--
### =====================

--
function decode_url(string s)
integer skip = 0
string res = ""
    for i=1 to length(s) do
        if skip then
            skip -= 1
        else
            integer ch = s[i]
            if ch='%' then
                sequence scanres = {}
                if i+2<=length(s) then
                    scanres = scanf("#"&s[i+1..i+2],"%x")
                end if
                if length(scanres)!=1 then
                    return "decode error"
                end if
                skip = 2
                ch = scanres[1][1]
            elsif ch='+' then
                ch = ' '
            end if
            res &= ch
        end if
    end for
    return res
end function

printf(1,"%s\n",{decode_url("http%3A%2F%2Ffoo%20bar%2F")})
printf(1,"%s\n",{decode_url("google.com/search?q=%60Abdu%27l-Bah%C3%A1")})
```

{{Out}}

```txt

http://foo bar/
google.com/search?q=`Abdu'l-Bahá

```



## PHP


```php
<?php
$encoded = "http%3A%2F%2Ffoo%20bar%2F";
$unencoded = rawurldecode($encoded);
echo "The unencoded string is $unencoded !\n";
?>
```



## PicoLisp


```txt
: (ht:Pack (chop "http%3A%2F%2Ffoo%20bar%2F") T)
-> "http://foo bar/"
```



## PowerShell


```PowerShell

[System.Web.HttpUtility]::UrlDecode("http%3A%2F%2Ffoo%20bar%2F")

```

{{Out}}

```txt

http://foo bar/

```



## PureBasic


```PureBasic
URL$ = URLDecoder("http%3A%2F%2Ffoo%20bar%2F")

Debug URL$  ; http://foo bar/
```



## Python


```Python

#Python 2.X
import urllib
print urllib.unquote("http%3A%2F%2Ffoo%20bar%2F")
#Python 3.5+
from urllib.parse import unquote
print(unquote('http%3A%2F%2Ffoo%20bar%2F'))

```



## R



```R
URLdecode("http%3A%2F%2Ffoo%20bar%2F")
```



## Racket



```racket

#lang racket
(require net/uri-codec)
(uri-decode "http%3A%2F%2Ffoo%20bar%2F")

```



## Retro

This is provided by the '''casket''' library (used for web app development).


```Retro
create buffer 32000 allot

{{
  create bit 5 allot
  : extract  ( $c-$a ) drop @+ bit ! @+ bit 1+ ! bit ;
  : render   ( $c-$n )
    dup '+ = [ drop 32 ] ifTrue
    dup 13 = [ drop 32 ] ifTrue
    dup 10 = [ drop 32 ] ifTrue
    dup '% = [ extract hex toNumber decimal ] ifTrue ;
  : <decode> (  $-$  ) repeat @+ 0; render ^buffer'add again ;
---reveal---
  : decode   (  $-   ) buffer ^buffer'set <decode> drop ;
}}

"http%3A%2F%2Ffoo%20bar%2F" decode buffer puts
```



## REXX


### version 1

{{Trans|ooRexx}}
Tested with the ooRexx and Regina interpreters.

```REXX
/* Rexx */

Do
  X = 0
  url. = ''
  X = X + 1; url.0 = X; url.X = 'http%3A%2F%2Ffoo%20bar%2F'
  X = X + 1; url.0 = X; url.X = 'mailto%3A%22Ivan%20Aim%22%20%3Civan%2Eaim%40email%2Ecom%3E'
  X = X + 1; url.0 = X; url.X = '%6D%61%69%6C%74%6F%3A%22%49%72%6D%61%20%55%73%65%72%22%20%3C%69%72%6D%61%2E%75%73%65%72%40%6D%61%69%6C%2E%63%6F%6D%3E'

  Do u_ = 1 to url.0
    Say url.u_
    Say DecodeURL(url.u_)
    Say
    End u_

  Return
End
Exit

DecodeURL:
Procedure
Do
  Parse Arg encoded
  decoded = ''
  PCT = '%'

  Do while length(encoded) > 0
    Parse Var encoded head (PCT) +1 code +2 tail
    decoded = decoded || head
    Select
      When length(strip(code, 'T')) = 2 & datatype(code, 'X') then Do
        code = x2c(code)
        decoded = decoded || code
        End
      When length(strip(code, 'T')) \= 0 then Do
        decoded = decoded || PCT
        tail = code || tail
        End
      Otherwise Do
        Nop
        End
      End
    encoded = tail
    End

  Return decoded
End
Exit

```


{{out}}

```txt

http%3A%2F%2Ffoo%20bar%2F
http://foo bar/

mailto%3A%22Ivan%20Aim%22%20%3Civan%2Eaim%40email%2Ecom%3E
mailto:"Ivan Aim" <ivan.aim@email.com>

%6D%61%69%6C%74%6F%3A%22%49%72%6D%61%20%55%73%65%72%22%20%3C%69%72%6D%61%2E%75%73%65%72%40%6D%61%69%6C%2E%63%6F%6D%3E
mailto:"Irma User" <irma.user@mail.com>

```



### version 2

This REXX version is identical to version 1, but with superfluous and dead code removed.

```REXX
/*REXX program converts a   URL─encoded string  ──►  its original unencoded form.       */
url.1='http%3A%2F%2Ffoo%20bar%2F'
url.2='mailto%3A%22Ivan%20Aim%22%20%3Civan%2Eaim%40email%2Ecom%3E'
url.3='%6D%61%69%6C%74%6F%3A%22%49%72%6D%61%20%55%73%65%72%22%20%3C%69%72%6D%61%2E%75%73%65%72%40%6D%61%69%6C%2E%63%6F%6D%3E'
URLs =3
            do j=1  for URLs
            say url.j
            say decodeURL(url.j)
            say
            end   /*j*/
exit
/*──────────────────────────────────────────────────────────────────────────────────────*/
decodeURL:  procedure;  parse arg encoded;     decoded= ''

              do  while encoded\==''
              parse var encoded   head  '%'  +1  code  +2  tail
              decoded= decoded || head
              L= length( strip( code, 'T') )
                 select
                 when L==2 & datatype(code, "X")  then       decoded= decoded || x2c(code)
                 when L\==0                       then do;   decoded= decoded'%'
                                                             tail= code || tail
                                                       end
                 otherwise nop
                 end    /*select*/
              encoded= tail
              end   /*while*/

            return decoded
```

{{out|output|text=  is identical to the 1<sup>st</sup> REXX version.}}


### version 3

This REXX version is a shorter version of version 2.

```REXX
/*REXX program converts & displays a URL─encoded string ──► its original unencoded form.*/
url. =
url.1='http%3A%2F%2Ffoo%20bar%2F'
url.2='mailto%3A%22Ivan%20Aim%22%20%3Civan%2Eaim%40email%2Ecom%3E'
url.3='%6D%61%69%6C%74%6F%3A%22%49%72%6D%61%20%55%73%65%72%22%20%3C%69%72%6D%61%2E%75%73%65%72%40%6D%61%69%6C%2E%63%6F%6D%3E'

            do j=1  until url.j=='';   say       /*process each URL; display blank line.*/
            say           url.j                  /*display the original URL.            */
            say URLdecode(url.j)                 /*   "     "  decoded   "              */
            end   /*j*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
URLdecode:  procedure;  parse arg yyy            /*get encoded URL from argument list.  */
            yyy= translate(yyy, , '+')           /*a special case for an encoded blank. */
            URL=
                          do  until yyy==''
                          parse var  yyy     plain  '%'  +1  code  +2  yyy
                          URL= URL || plain
                          if datatype(code, 'X')  then URL= URL || x2c(code)
                                                  else URL= URL'%'code
                          end   /*until*/
            return URL
```

{{out|output|text=  is identical to the 1<sup>st</sup> REXX version.}}


## Ruby

Use any one of <code>CGI.unescape</code> or <code>URI.decode_www_form_component</code>. These methods also convert "+" to " ".


```ruby
require 'cgi'
puts CGI.unescape("http%3A%2F%2Ffoo%20bar%2F")
# => "http://foo bar/"
```


{{works with|Ruby|1.9.2}}

```ruby
require 'uri'
puts URI.decode_www_form_component("http%3A%2F%2Ffoo%20bar%2F")
# => "http://foo bar/"
```


<code>URI.unescape</code> (alias <code>URI.unencode</code>) still works. <code>URI.unescape</code> is obsolete since Ruby 1.9.2 because of problems with its sibling <code>URI.escape</code>.


## Rust


```rust
const INPUT1: &str = "http%3A%2F%2Ffoo%20bar%2F";
const INPUT2: &str = "google.com/search?q=%60Abdu%27l-Bah%C3%A1";

fn append_frag(text: &mut String, frag: &mut String) {
    if !frag.is_empty() {
        let encoded = frag.chars()
            .collect::<Vec<char>>()
            .chunks(2)
            .map(|ch| {
                u8::from_str_radix(&ch.iter().collect::<String>(), 16).unwrap()
            }).collect::<Vec<u8>>();
        text.push_str(&std::str::from_utf8(&encoded).unwrap());
        frag.clear();
    }
}

fn decode(text: &str) -> String {
    let mut output = String::new();
    let mut encoded_ch = String::new();
    let mut iter = text.chars();
    while let Some(ch) = iter.next() {
        if ch == '%' {
            encoded_ch.push_str(&format!("{}{}", iter.next().unwrap(), iter.next().unwrap()));
        } else {
            append_frag(&mut output, &mut encoded_ch);
            output.push(ch);
        }
    }
    append_frag(&mut output, &mut encoded_ch);
    output
}

fn main() {
    println!("{}", decode(INPUT1));
    println!("{}", decode(INPUT2));
}
```

{{out}}

```txt

http://foo bar/
google.com/search?q=`Abdu'l-Bahá

```



## Scala

{{libheader|Scala}}
```scala
import java.net.{URLDecoder, URLEncoder}
import scala.compat.Platform.currentTime

object UrlCoded extends App {
  val original = """http://foo bar/"""
  val encoded: String = URLEncoder.encode(original, "UTF-8")

  assert(encoded == "http%3A%2F%2Ffoo+bar%2F", s"Original: $original not properly encoded: $encoded")

  val percentEncoding = encoded.replace("+", "%20")
  assert(percentEncoding == "http%3A%2F%2Ffoo%20bar%2F", s"Original: $original not properly percent-encoded: $percentEncoding")

  assert(URLDecoder.decode(encoded, "UTF-8") == URLDecoder.decode(percentEncoding, "UTF-8"))

  println(s"Successfully completed without errors. [total ${currentTime - executionStart} ms]")
}
```



## Seed7

The library [http://seed7.sourceforge.net/libraries/encoding.htm encoding.s7i] defines functions
to handle URL respectively percent encoding.
The function [http://seed7.sourceforge.net/libraries/encoding.htm#fromPercentEncoded%28in_string%29 fromPercentEncoded]
decodes a percend-encoded string.
The function [http://seed7.sourceforge.net/libraries/encoding.htm#fromUrlEncoded%28in_string%29 fromUrlEncoded]
works like ''fromPercentEncoded'' and additionally decodes '+' with a space.
Both functions return byte sequences.
To decode Unicode characters it is necessary to convert them from UTF-8 with ''utf8ToStri'' afterwards.

```seed7
$ include "seed7_05.s7i";
  include "encoding.s7i";

const proc: main is func
  begin
    writeln(fromPercentEncoded("http%3A%2F%2Ffoo%20bar%2F"));
    writeln(fromUrlEncoded("http%3A%2F%2Ffoo+bar%2F"));
  end func;
```


{{out}}

```txt

http://foo bar/
http://foo bar/

```



## Sidef

{{trans|Perl}}

```ruby
func urldecode(str) {
    str.gsub!('+', ' ');
    str.gsub!(/\%([A-Fa-f0-9]{2})/, {|a| 'C'.pack(a.hex)});
    return str;
}

say urldecode('http%3A%2F%2Ffoo+bar%2F');  # => "http://foo bar/"
```



## Swift


```swift
import Foundation

let encoded = "http%3A%2F%2Ffoo%20bar%2F"
if let normal = encoded.stringByReplacingPercentEscapesUsingEncoding(NSUTF8StringEncoding) {
  println(normal)
}
```



## Tcl

This code is careful to ensure that any untoward metacharacters in the input string still do not cause any problems.

```tcl
proc urlDecode {str} {
    set specialMap {"[" "%5B" "]" "%5D"}
    set seqRE {%([0-9a-fA-F]{2})}
    set replacement {[format "%c" [scan "\1" "%2x"]]}
    set modStr [regsub -all $seqRE [string map $specialMap $str] $replacement]
    return [encoding convertfrom utf-8 [subst -nobackslash -novariable $modStr]]
}
```

Demonstrating:

```tcl
puts [urlDecode "http%3A%2F%2Ffoo%20bar%2F"]
```

{{out}}

```txt
http://foo bar/
```


## TUSCRIPT


```tuscript

$$ MODE TUSCRIPT
url_encoded="http%3A%2F%2Ffoo%20bar%2F"
BUILD S_TABLE hex=":%><:><2<>2<%:"
hex=STRINGS (url_encoded,hex), hex=SPLIT(hex)
hex=DECODE (hex,hex)
url_decoded=SUBSTITUTE(url_encoded,":%><2<>2<%:",0,0,hex)
PRINT "encoded: ", url_encoded
PRINT "decoded: ", url_decoded

```

{{out}}

```txt

encoded: http%3A%2F%2Ffoo%20bar%2F
decoded: http://foo bar/

```



## UNIX Shell


{{works with|bash}}
{{works with|ksh}}


```bash
urldecode() { local u="${1//+/ }"; printf '%b' "${u//%/\\x}"; }
```


Example:


```bash
urldecode http%3A%2F%2Ffoo%20bar%2F
http://foo bar/

urldecode google.com/search?q=%60Abdu%27l-Bah%C3%A1
google.com/search?q=`Abdu'l-Bahároot@

```





```bash
function urldecode
{
        typeset encoded=$1 decoded= rest= c= c1= c2=
        typeset rest2= bug='rest2=${rest}'

        if [[ -z ${BASH_VERSION:-} ]]; then
                typeset -i16 hex=0; typeset -i8 oct=0

                # bug /usr/bin/sh HP-UX 11.00
                typeset _encoded='xyz%26xyz'
                rest="${_encoded#?}"
                c="${_encoded%%${rest}}"
                if (( ${#c} != 1 )); then
                        typeset qm='????????????????????????????????????????????????????????????????????????'
                        typeset bug='(( ${#rest} > 0 )) && typeset -L${#rest} rest2="${qm}" || rest2=${rest}'
                fi
        fi

	rest="${encoded#?}"
	eval ${bug}
	c="${encoded%%${rest2}}"
	encoded="${rest}"

	while [[ -n ${c} ]]; do
		if [[ ${c} = '%' ]]; then
			rest="${encoded#?}"
			eval ${bug}
			c1="${encoded%%${rest2}}"
			encoded="${rest}"

			rest="${encoded#?}"
			eval ${bug}
			c2="${encoded%%${rest2}}"
			encoded="${rest}"

			if [[ -z ${c1} || -z ${c2} ]]; then
				c="%${c1}${c2}"
				echo "WARNING: invalid % encoding: ${c}" >&2
			elif [[ -n ${BASH_VERSION:-} ]]; then
				c="\\x${c1}${c2}"
				c=$(\echo -e "${c}")
			else
				hex="16#${c1}${c2}"; oct=hex
				c="\\0${oct#8\#}"
				c=$(print -- "${c}")
			fi
		elif [[ ${c} = '+' ]]; then
			c=' '
		fi

		decoded="${decoded}${c}"

		rest="${encoded#?}"
		eval ${bug}
		c="${encoded%%${rest2}}"
		encoded="${rest}"
	done

	if [[ -n ${BASH_VERSION:-} ]]; then
		\echo -E "${decoded}"
	else
		print -r -- "${decoded}"
	fi
}

```



## VBScript


```VBScript
Function RegExTest(str,patrn)
    Dim regEx
    Set regEx = New RegExp
    regEx.IgnoreCase = True
    regEx.Pattern = patrn
    RegExTest = regEx.Test(str)
End Function

Function URLDecode(sStr)
    Dim str,code,a0
    str=""
    code=sStr
    code=Replace(code,"+"," ")
    While len(code)>0
        If InStr(code,"%")>0 Then
            str = str & Mid(code,1,InStr(code,"%")-1)
            code = Mid(code,InStr(code,"%"))
            a0 = UCase(Mid(code,2,1))
            If a0="U" And RegExTest(code,"^%u[0-9A-F]{4}") Then
                str = str & ChrW((Int("&H" & Mid(code,3,4))))
                code = Mid(code,7)
            ElseIf a0="E" And RegExTest(code,"^(%[0-9A-F]{2}){3}") Then
                str = str & ChrW((Int("&H" & Mid(code,2,2)) And 15) * 4096 + (Int("&H" & Mid(code,5,2)) And 63) * 64 + (Int("&H" & Mid(code,8,2)) And 63))
                code = Mid(code,10)
            ElseIf a0>="C" And a0<="D" And RegExTest(code,"^(%[0-9A-F]{2}){2}") Then
                str = str & ChrW((Int("&H" & Mid(code,2,2)) And 3) * 64 + (Int("&H" & Mid(code,5,2)) And 63))
                code = Mid(code,7)
            ElseIf (a0<="B" Or a0="F") And RegExTest(code,"^%[0-9A-F]{2}") Then
                str = str & Chr(Int("&H" & Mid(code,2,2)))
                code = Mid(code,4)
            Else
                str = str & "%"
                code = Mid(code,2)
            End If
        Else
            str = str & code
            code = ""
        End If
    Wend
    URLDecode = str
End Function

url = "http%3A%2F%2Ffoo%20bar%C3%A8%2F"
WScript.Echo "Encoded URL: " & url & vbCrLf &_
	"Decoded URL: " & UrlDecode(url)
```


{{out}}

```txt
Encoded URL: http%3A%2F%2Ffoo%20bar%C3%A8%2F
Decoded URL: http://foo barè/
```



## XPL0


```XPL0
code Text=12;
string 0;               \use zero-terminated strings

func Decode(S0);        \Decode URL string and return its address
char S0;
char S1(80);            \BEWARE: very temporary string space returned
int  C, N, I, J;
[I:= 0;  J:= 0;
repeat  C:= S0(I);  I:= I+1;                    \get char
        if C=^% then                            \convert hex to char
                [C:= S0(I);  I:= I+1;
                if C>=^a then C:= C & ~$20;     \convert to uppercase
                N:= C - (if C<=^9 then ^0 else ^A-10);
                C:= S0(I);  I:= I+1;
                if C>=^a then C:= C & ~$20;
                C:= N*16 + C - (if C<=^9 then ^0 else ^A-10);
                ];
        S1(J):= C;  J:= J+1;                    \put char in output string
until   C=0;
return S1;
];

Text(0, Decode("http%3A%2F%2Ffoo%20bar%2f"))
```


{{out}}

```txt

http://foo bar/

```



## Yabasic

{{trans|Phix}}

```Yabasic
sub decode_url$(s$)
    local res$, ch$

    while(s$ <> "")
        ch$ = left$(s$, 1)
        if ch$ = "%" then
            ch$ = chr$(dec(mid$(s$, 2, 2)))
            s$ = right$(s$, len(s$) - 3)
        else
            if ch$ = "+" ch$ = " "
            s$ = right$(s$, len(s$) - 1)
	endif
        res$ = res$ + ch$
    wend
    return res$
end sub

print decode_url$("http%3A%2F%2Ffoo%20bar%2F")
print decode_url$("google.com/search?q=%60Abdu%27l-Bah%C3%A1")
```



## zkl


```zkl
"http%3A%2F%2Ffoo%20bar%2F".pump(String,  // push each char through these fcns:
   fcn(c){ if(c=="%") return(Void.Read,2); return(Void.Skip,c) },// %-->read 2 chars else pass through
   fcn(_,b,c){ (b+c).toInt(16).toChar() })  // "%" (ignored)  "3"+"1"-->0x31-->"1"
```

{{out}}

```txt
http://foo bar/
```

or use libCurl:

```zkl
var Curl=Import.lib("zklCurl");
Curl.urlDecode("http%3A%2F%2Ffoo%20bar%2F");
```

{{out}}
```txt
http://foo bar/
```

