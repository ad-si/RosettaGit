+++
title = "URL encoding"
description = ""
date = 2019-10-14T11:20:32Z
aliases = []
[extra]
id = 9936
[taxonomies]
categories = []
tags = []
+++

{{task}}
[[Category:String manipulation]]
[[Category: Encodings]]

;Task:
Provide a function or mechanism to convert a provided string into URL encoding representation.

In URL encoding, special characters, control characters and extended characters
are converted into a percent symbol followed by a two digit hexadecimal code,
So a space character encodes into %20 within the string.

For the purposes of this task, every character except 0-9, A-Z and a-z requires conversion, so the following characters all require conversion by default:

* ASCII control codes (Character ranges 00-1F hex (0-31 decimal) and 7F (127 decimal).
* ASCII symbols (Character ranges 32-47 decimal (20-2F hex))
* ASCII symbols (Character ranges 58-64 decimal (3A-40 hex))
* ASCII symbols (Character ranges 91-96 decimal (5B-60 hex))
* ASCII symbols (Character ranges 123-126 decimal (7B-7E hex))
* Extended characters with character codes of 128 decimal (80 hex) and above.



;Example:
The string "<code><nowiki>http://foo bar/</nowiki></code>" would be encoded as "<code><nowiki>http%3A%2F%2Ffoo%20bar%2F</nowiki></code>".


;Variations:
* Lowercase escapes are legal, as in "<code><nowiki>http%3a%2f%2ffoo%20bar%2f</nowiki></code>".
* Some standards give different rules: RFC 3986, ''Uniform Resource Identifier (URI): Generic Syntax'', section 2.3, says that "-._~" should not be encoded. HTML 5, section [http://www.whatwg.org/specs/web-apps/current-work/multipage/association-of-controls-and-forms.html#url-encoded-form-data 4.10.22.5 URL-encoded form data], says to preserve "-._*", and to encode space " " to "+". The options below provide for utilization of an exception string, enabling preservation (non encoding) of particular characters to meet specific standards.


;Options:
It is permissible to use an exception string (containing a set of symbols
that do not need to be converted).
However, this is an optional feature and is not a requirement of this task.


;Related tasks:
*   [[URL decoding]]
*   [[URL parser]]





## Ada

{{libheader|AWS}}

```Ada
with AWS.URL;
with Ada.Text_IO; use Ada.Text_IO;
procedure Encode is
   Normal : constant String := "http://foo bar/";
begin
   Put_Line (AWS.URL.Encode (Normal));
end Encode;
```

{{out}}

```txt
http%3A%2F%2Ffoo%20bar%2F
```



## AutoHotkey


```AutoHotkey
MsgBox, % UriEncode("http://foo bar/")

; Modified from http://goo.gl/0a0iJq
UriEncode(Uri)
{
	VarSetCapacity(Var, StrPut(Uri, "UTF-8"), 0)
	StrPut(Uri, &Var, "UTF-8")
	f := A_FormatInteger
	SetFormat, IntegerFast, H
	While Code := NumGet(Var, A_Index - 1, "UChar")
		If (Code >= 0x30 && Code <= 0x39 ; 0-9
			|| Code >= 0x41 && Code <= 0x5A ; A-Z
			|| Code >= 0x61 && Code <= 0x7A) ; a-z
			Res .= Chr(Code)
		Else
			Res .= "%" . SubStr(Code + 0x100, -1)
	SetFormat, IntegerFast, %f%
	Return, Res
}
```



## Apex


```apex
EncodingUtil.urlEncode('http://foo bar/', 'UTF-8')
```


```txt
http%3A%2F%2Ffoo+bar%2F
```



## AppleScript

{{libheader|AppleScript Toolbox}}

```AppleScript
AST URL encode "http://foo bar/"
```

{{out}}

```txt
"http%3A%2F%2Ffoo%20bar%2F"
```



## Arturo



```arturo
url "http://foo bar/"

print $(encodeUrl url)
```


{{out}}


```txt
http%3A%2F%2Ffoo%20bar%2F
```



## AWK


```awk
BEGIN {
	for (i = 0; i <= 255; i++)
		ord[sprintf("%c", i)] = i
}

# Encode string with application/x-www-form-urlencoded escapes.
function escape(str,    c, len, res) {
	len = length(str)
	res = ""
	for (i = 1; i <= len; i++) {
		c = substr(str, i, 1);
		if (c ~ /[0-9A-Za-z]/)
		#if (c ~ /[-._*0-9A-Za-z]/)
			res = res c
		#else if (c == " ")
		#	res = res "+"
		else
			res = res "%" sprintf("%02X", ord[c])
	}
	return res
}

# Escape every line of input.
{ print escape($0) }
```


The array <code>ord[]</code> uses idea from [[Character codes#AWK]].

To follow the rules for HTML 5, uncomment the two lines that convert " " to "+", and use the regular expression that preserves "-._*".


## BBC BASIC


```bbcbasic
      PRINT FNurlencode("http://foo bar/")
      END

      DEF FNurlencode(url$)
      LOCAL c%, i%
      WHILE i% < LEN(url$)
        i% += 1
        c% = ASCMID$(url$, i%)
        IF c%<&30 OR c%>&7A OR c%>&39 AND c%<&41 OR c%>&5A AND c%<&61 THEN
          url$ = LEFT$(url$,i%-1) + "%" + RIGHT$("0"+STR$~c%,2) + MID$(url$,i%+1)
        ENDIF
      ENDWHILE
      = url$
```

{{out}}

```txt

http%3A%2F%2Ffoo%20bar%2F

```



## Bracmat


```bracmat
(     ( encode
      =   encoded exceptions octet string
        .     !arg:(?exceptions.?string)
            & :?encoded
            & @( !string
               :   ?
                   ( %@?octet ?
                   &     !encoded
                         (   !octet
                           : ( ~<0:~>9
                             | ~<A:~>Z
                             | ~<a:~>z
                             )
                         |   @(!exceptions:? !octet ?)
                           & !octet
                         | "%" d2x$(asc$!octet)
                         )
                     : ?encoded
                   & ~
                   )
               )
          | str$!encoded
      )
    & out$"without exceptions:
"
    & out$(encode$(."http://foo bar/"))
    & out$(encode$(."mailto:Ivan"))
    & out$(encode$(."Aim <ivan.aim@email.com>"))
    & out$(encode$(."mailto:Irma"))
    & out$(encode$(."User <irma.user@mail.com>"))
    & out$(encode$(."http://foo.bar.com/~user-name/_subdir/*~.html"))
    & out$"
with RFC 3986 rules:
"
    & out$(encode$("-._~"."http://foo bar/"))
    & out$(encode$("-._~"."mailto:Ivan"))
    & out$(encode$("-._~"."Aim <ivan.aim@email.com>"))
    & out$(encode$("-._~"."mailto:Irma"))
    & out$(encode$("-._~"."User <irma.user@mail.com>"))
    & out$(encode$("-._~"."http://foo.bar.com/~user-name/_subdir/*~.html"))
);

```

{{out}}

```txt
without exceptions:

http%3A%2F%2Ffoo%20bar%2F
mailto%3AIvan
Aim%20%3Civan%2Eaim%40email%2Ecom%3E
mailto%3AIrma
User%20%3Cirma%2Euser%40mail%2Ecom%3E
http%3A%2F%2Ffoo%2Ebar%2Ecom%2F%7Euser%2Dname%2F%5Fsubdir%2F%2A%7E%2Ehtml

with RFC 3986 rules:

http%3A%2F%2Ffoo%20bar%2F
mailto%3AIvan
Aim%20%3Civan.aim%40email.com%3E
mailto%3AIrma
User%20%3Cirma.user%40mail.com%3E
http%3A%2F%2Ffoo.bar.com%2F~user-name%2F_subdir%2F%2A~.html
```



## C


```c
#include <stdio.h>
#include <ctype.h>

char rfc3986[256] = {0};
char html5[256] = {0};

/* caller responsible for memory */
void encode(const char *s, char *enc, char *tb)
{
	for (; *s; s++) {
		if (tb[*s]) sprintf(enc, "%c", tb[*s]);
		else        sprintf(enc, "%%%02X", *s);
		while (*++enc);
	}
}

int main()
{
	const char url[] = "http://foo bar/";
	char enc[(strlen(url) * 3) + 1];

	int i;
	for (i = 0; i < 256; i++) {
		rfc3986[i] = isalnum(i)||i == '~'||i == '-'||i == '.'||i == '_'
			? i : 0;
		html5[i] = isalnum(i)||i == '*'||i == '-'||i == '.'||i == '_'
			? i : (i == ' ') ? '+' : 0;
	}

	encode(url, enc, rfc3986);
	puts(enc);

	return 0;
}
```



## C++

using Qt 4.6 as a library

```cpp
#include <QByteArray>
#include <iostream>

int main( ) {
   QByteArray text ( "http://foo bar/" ) ;
   QByteArray encoded( text.toPercentEncoding( ) ) ;
   std::cout << encoded.data( ) << '\n' ;
   return 0 ;
}
```

{{out}}
<PRE>http%3A%2F%2Ffoo%20bar%2F</PRE>


## C#



```c sharp
using System;

namespace URLEncode
{
    internal class Program
    {
        private static void Main(string[] args)
        {
            Console.WriteLine(Encode("http://foo bar/"));
        }

        private static string Encode(string uri)
        {
            return Uri.EscapeDataString(uri);
        }
    }
}
```


{{out}}

```txt
http%3A%2F%2Ffoo%20bar%2F

```



## Clojure

Using Java's URLEncoder:

```clojure
(import 'java.net.URLEncoder)
(URLEncoder/encode "http://foo bar/" "UTF-8")
```


{{out}}
```txt
"http%3A%2F%2Ffoo+bar%2F"
```



## ColdFusion



## Common Lisp


```lisp
(defun needs-encoding-p (char)
  (not (digit-char-p char 36)))

(defun encode-char (char)
  (format nil "%~2,'0X" (char-code char)))

(defun url-encode (url)
  (apply #'concatenate 'string
         (map 'list (lambda (char)
                      (if (needs-encoding-p char)
                          (encode-char char)
                          (string char)))
              url)))

(url-encode "http://foo bar/")
```

{{out}}

```txt
"http%3A%2F%2Ffoo%20bar%2F"
```



## D


```d
import std.stdio, std.uri;

void main() {
    writeln(encodeComponent("http://foo bar/"));
}
```


```txt
http%3A%2F%2Ffoo%20bar%2F
```



## Elixir


```elixir
iex(1)> URI.encode("http://foo bar/", &URI.char_unreserved?/1)
"http%3A%2F%2Ffoo%20bar%2F"
```



## Erlang

Built in, <code>http_uri:encode/1</code> accepts lists and binary:


```txt

1> http_uri:encode("http://foo bar/").
"http%3A%2F%2Ffoo%20bar%2F"

```



###  Unicode

If you are URL encoding unicode data though <code>http_uri:encode/1</code> will produce incorrect results:

```txt

1> http_uri:encode("étanchéité d une terrasse").
"étanchéité%20d%20une%20terrasse"

```


You should use the built-in (non-documented) <code>edoc_lib:escape_uri/1</code> instead:

```txt

1> edoc_lib:escape_uri("étanchéité d une terrasse").
"%c3%a9tanch%c3%a9it%c3%a9%20d%20une%20terrasse"

```


And for binary you will need to take care and use <code>unicode:characters_to_{list,binary}/1</code>:

```txt

1> unicode:characters_to_binary(edoc_lib:escape_uri(unicode:characters_to_list(<<"étanchéité d une terrasse"/utf8>>))).
<<"%c3%a9tanch%c3%a9it%c3%a9%20d%20une%20terrasse">>

```


=={{header|F_Sharp|F#}}==

```fsharp
open System

[<EntryPoint>]
let main args =
    printfn "%s" (Uri.EscapeDataString(args.[0]))
    0
```

{{out}}

```txt
>URLencoding.exe "http://foo bar/"
http%3A%2F%2Ffoo%20bar%2F
```



## Factor

Factor's built-in URL encoder doesn't encode <code>:</code> or <code>/</code>. However, we can write our own predicate quotation that tells <code>(url-encode)</code> what characters to exclude.

```factor
USING: combinators.short-circuit unicode urls.encoding.private ;

: my-url-encode ( str -- encoded )
    [ { [ alpha? ] [ "-._~" member? ] } 1|| ] (url-encode) ;

"http://foo bar/" my-url-encode print
```

{{out}}

```txt

http%3A%2F%2Ffoo%20bar%2F

```



## Free Pascal



```pascal
function urlEncode(data: string): AnsiString;
var
  ch: AnsiChar;
begin
  Result := '';
  for ch in data do begin
    if ((Ord(ch) < 65) or (Ord(ch) > 90)) and ((Ord(ch) < 97) or (Ord(ch) > 122)) then begin
      Result := Result + '%' + IntToHex(Ord(ch), 2);
    end else
      Result := Result + ch;
  end;
end;
```



## Go


```go
package main

import (
    "fmt"
    "net/url"
)

func main() {
    fmt.Println(url.QueryEscape("http://foo bar/"))
}
```

{{out}}

```txt

http%3A%2F%2Ffoo+bar%2F

```



## Groovy


```groovy

def normal = "http://foo bar/"
def encoded = URLEncoder.encode(normal, "utf-8")
println encoded

```

{{out}}

```txt

http%3A%2F%2Ffoo+bar%2F

```



## Haskell


```Haskell
import qualified Data.Char as Char
import Text.Printf

encode :: Char -> String
encode c
  | c == ' ' = "+"
  | Char.isAlphaNum c || c `elem` "-._~" = [c]
  | otherwise = printf "%%%02X" c

urlEncode :: String -> String
urlEncode = concatMap encode

main :: IO ()
main = putStrLn $ urlEncode "http://foo bar/"
```

{{out}}

```txt
http%3A%2F%2Ffoo+bar%2F
```


=={{header|Icon}} and {{header|Unicon}}==

```Icon
link hexcvt

procedure main()
write("text    = ",image(u := "http://foo bar/"))
write("encoded = ",image(ue := encodeURL(u)))
end

procedure encodeURL(s)           #: encode data for inclusion in a URL/URI
static en
initial {                           # build lookup table for everything
   en := table()
   every en[c := !string(~(&digits++&letters))] := "%"||hexstring(ord(c),2)
   every /en[c := !string(&cset)] := c
   }

every (c := "") ||:= en[!s]         # re-encode everything
return c
end

```


{{libheader|Icon Programming Library}}
[http://www.cs.arizona.edu/icon/library/src/procs/hexcvt.icn hexcvt provides hexstring]

{{out}}

```txt
text    = "http://foo bar/"
encoded = "http%3A%2F%2Ffoo%20bar%2F"
```



## J


J has a urlencode in the gethttp package, but this task requires that all non-alphanumeric characters be encoded.

Here's an implementation that does that:


```j
require'strings convert'
urlencode=: rplc&((#~2|_1 47 57 64 90 96 122 I.i.@#)a.;"_1'%',.hfd i.#a.)
```


Example use:


```j
   urlencode 'http://foo bar/'
http%3A%2F%2Ffoo%20bar%2F
```



## Java


The built-in URLEncoder in Java converts the space " " into a plus-sign "+" instead of "%20":

```java
import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;

public class Main
{
    public static void main(String[] args) throws UnsupportedEncodingException
    {
        String normal = "http://foo bar/";
        String encoded = URLEncoder.encode(normal, "utf-8");
        System.out.println(encoded);
    }
}
```


{{out}}

```txt
http%3A%2F%2Ffoo+bar%2F
```



## JavaScript

Confusingly, there are 3 different URI encoding functions in JavaScript: <code>escape()</code>, <code>encodeURI()</code>, and <code>encodeURIComponent()</code>. Each of them encodes a different set of characters. See [http://www.javascripter.net/faq/escape.htm this article] and [http://xkr.us/articles/javascript/encode-compare/ this article] for more information and comparisons.

```javascript
var normal = 'http://foo/bar/';
var encoded = encodeURIComponent(normal);
```



## jq

{{works with| jq | 1.4}}

jq has a built-in function, @uri, for "percent-encoding". It preserves the characters that RFC 3968 mandates
be preserved, but also preserves the following five characters: !'()*

To address the task requirement, therefore, we can first use @uri and then convert the exceptional characters.
(For versions of jq with regex support, this could be done using gsub, but here we perform
the translation directly.)

Note that @uri also converts multibyte characters to multi-triplets, e.g.

```jq
"á" | @uri
```
 produces: "%C3%A1"

```jq
def url_encode:
   # The helper function checks whether the input corresponds to one of the characters: !'()*
   def recode: . as $c | [33,39,40,41,42] | index($c);
   def hex:   if . < 10 then 48 + . else  55 + . end;
   @uri
   | explode
   # 37 ==> "%", 50 ==> "2"
   | map( if recode then (37, 50, ((. - 32) | hex)) else . end )
   | implode;
```

'''Examples:'''


```jq
"http://foo bar/" | @uri
```

produces: "http%3A%2F%2Ffoo%20bar%2F"


```jq
"http://foo bar/" | @uri == url_encode
```
 produces: true

To contrast the difference between "@uri" and "url_encode", we compare the characters that are unaltered:


```jq
[range(0;1024) | [.] | implode | if @uri == . then . else empty end] | join(null)
```

produces: "!'()*-.0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz~"


```jq
[range(0;1024) | [.] | implode | if url_encode == . then . else empty end] | join(null)
```

produces: "-.0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz~"


## Julia


```Julia

//version 1.0.1
import HTTP.URIs: escapeuri

dcd = "http://foo bar/"
enc = escapeuri(dcd)

println(dcd, " => ", enc)

```


{{out}}

```txt

http://foo bar/ => http%3A%2F%2Ffoo%20bar%2F

```



## Kotlin


```scala
// version 1.1.2

import java.net.URLEncoder

fun main(args: Array<String>) {
    val url = "http://foo bar/"
    println(URLEncoder.encode(url, "utf-8")) // note: encodes space to + not %20
}
```


{{out}}

```txt

http%3A%2F%2Ffoo+bar%2F

```



## Lasso


```Lasso
bytes('http://foo bar/') -> encodeurl
```

-> http%3A%2F%2Ffoo%20bar%2F


## Liberty BASIC


```lb

    dim lookUp$( 256)

    for i =0 to 256
        lookUp$( i) ="%" +dechex$( i)
    next i

    string$ ="http://foo bar/"

    print "Supplied string '"; string$; "'"
    print "As URL          '"; url$( string$); "'"

    end

function url$( i$)
    for j =1 to len( i$)
        c$ =mid$( i$, j, 1)
        if instr( "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz", c$) then
            url$ =url$ +c$
        else
            url$ =url$ +lookUp$( asc( c$))
        end if
    next j
end function

```


```txt

 Supplied string 'http://foo bar/'
 As URL 'http%3A%2F%2Ffoo%20bar%2F'

```



## Lingo

Lingo implements old-school URL encoding (with spaces encoded as "+") out of the box:


```lingo
put urlencode("http://foo bar/")
-- "http%3a%2f%2ffoo+bar%2f"
```


For RFC 3986 URL encoding (i.e. without the "+" fuss) a custom function is needed - which might call the above function and then just replace all "+" with "%20".


## LiveCode


```LiveCode
urlEncode("http://foo bar/")
-- http%3A%2F%2Ffoo+bar%2F
```



## Lua


```lua
function encodeChar(chr)
	return string.format("%%%X",string.byte(chr))
end

function encodeString(str)
	local output, t = string.gsub(str,"[^%w]",encodeChar)
	return output
end

-- will print "http%3A%2F%2Ffoo%20bar%2F"
print(encodeString("http://foo bar/"))
```



## M2000 Interpreter


```M2000 Interpreter

Module Checkit {
            Function decodeUrl$(a$) {
                  DIM a$()
                  a$()=Piece$(a$, "%")
                  if len(a$())=1 then =str$(a$):exit
                  k=each(a$(),2)
                  acc$=str$(a$(0))
                  While k {
                  acc$+=str$(Chr$(Eval("0x"+left$(a$(k^),2)))+Mid$(a$(k^),3))
                  }
                  =string$(acc$ as utf8dec)
            }
            Group Parse$ {
                 all$, c=1
                 tc$=""
                 Enum UrlType {None=0, RFC3986, HTML5}
                 variation
                 TypeData=("","-._~","-._*")
                 Function Next {
                            .tc$<=mid$(.all$,.c,1)
                             .c++
                             =.tc$<>""
                  }
                  Value {
                        =.tc$
                  }
                  Function DecodeOne$ {
                        if .tc$="" then exit
                        if .tc$ ~"[A-Za-z0-9]" then =.tc$ : exit
                        If .tc$=" " Then =if$(.variation=.HTML5->"+","%20") :exit
                        if instr(.TypeData#val$(.variation),.tc$)>0 then =.tc$ :exit
                        ="%"+hex$(asc(.tc$), 1)
                  }
                  Function Decode$ {
                        acc$=""
                        .c<=1
                        While .Next()  {
                               acc$+=.DecodeOne$()
                        }
                        =acc$
                  }
                  Set () {
                        \\ using optional argument
                        var=.None
                        Read a$, ? var
                        a$=chr$(string$(a$ as utf8enc))
                        .variation<=var
                        .all$<=a$
                        .c<=1
                  }
            }
            \\ MAIN
            Parse$()="http://foo bar/"
            Print Quote$(Parse.Decode$())
            Parse.variation=Parse.HTML5
            Print Quote$(Parse.Decode$())
            Parse.variation=Parse.RFC3986
            Print Quote$(Parse.Decode$())
            Parse$(Parse.RFC3986) ={mailto:"Irma User" <irma.user@mail.com>}
            Print Quote$(Parse.Decode$())
            Parse$(Parse.RFC3986) ={http://foo.bar.com/~user-name/_subdir/*~.html}
            m=each(Parse.UrlType)
            while m {
                 Parse.variation=eval(m)
                 Print Quote$(Parse.Decode$())
                 Print decodeUrl$(Parse.Decode$())
            }
}
CheckIt

```

<pre style="height:30ex;overflow:scroll">
"http%3A%2F%2Ffoo%20bar%2F"
"http%3A%2F%2Ffoo+bar%2F"
"mailto%3A%22Irma%20User%22%20%3Cirma.user%40mail.com%3E"
"http%3A%2F%2Ffoo%2Ebar%2Ecom%2F%7Euser%2Dname%2F%5Fsubdir%2F%2A%7E%2Ehtml"
"http%3A%2F%2Ffoo.bar.com%2F~user-name%2F_subdir%2F%2A~.html"
"http%3A%2F%2Ffoo.bar.com%2F%7Euser-name%2F_subdir%2F*%7E.html"

</pre >


## Maple


```maple
URL:-Escape("http://foo bar/");
```


{{out}}

```txt
"http%3A%2F%2Ffoo%20bar%2F"
```



## Mathematica


```mathematica
URLEncoding[url_] :=
 StringReplace[url,
  x : Except[
     Join[CharacterRange["0", "9"], CharacterRange["a", "z"],
      CharacterRange["A", "Z"]]] :>
   StringJoin[("%" ~~ #) & /@
     IntegerString[ToCharacterCode[x, "UTF8"], 16]]]
```


Example use:


```mathematica
URLEncoding["http://foo bar/"]
```


{{out}}

```txt
http%3a%2f%2ffoo%20bar%2f
```


=={{header|MATLAB}} / {{header|Octave}}==

```MATLAB
function u = urlencoding(s)
	u = '';
	for k = 1:length(s),
		if isalnum(s(k))
			u(end+1) = s(k);
		else
			u=[u,'%',dec2hex(s(k)+0)];
		end;
	end
end
```

Usage:

```txt
octave:3> urlencoding('http://foo bar/')
ans = http%3A%2F%2Ffoo%20bar%2F
```




## NetRexx


```NetRexx
/* NetRexx */
options replace format comments java crossref symbols nobinary

/* -------------------------------------------------------------------------- */
  testcase()
  say
  say 'RFC3986'
  testcase('RFC3986')
  say
  say 'HTML5'
  testcase('HTML5')
  say
  return

/* -------------------------------------------------------------------------- */
method encode(url, varn) public static

  variation = varn.upper
  opts = ''
  opts['RFC3986'] = '-._~'
  opts['HTML5']   = '-._*'

  rp = ''
  loop while url.length > 0
    parse url tc +1 url
    select
      when tc.datatype('A') then do
        rp = rp || tc
        end
      when tc == ' ' then do
        if variation = 'HTML5' then
          rp = rp || '+'
        else
          rp = rp || '%' || tc.c2x
        end
      otherwise do
        if opts[variation].pos(tc) > 0 then do
          rp = rp || tc
          end
        else do
          rp = rp || '%' || tc.c2x
          end
        end
      end
    end

  return rp

/* -------------------------------------------------------------------------- */
method testcase(variation = '') public static

  url = [ -
      'http://foo bar/' -
    , 'mailto:"Ivan Aim" <ivan.aim@email.com>' -
    , 'mailto:"Irma User" <irma.user@mail.com>' -
    , 'http://foo.bar.com/~user-name/_subdir/*~.html' -
    ]

  loop i_ = 0 to url.length - 1
    say url[i_]
    say encode(url[i_], variation)
    end i_

  return

```


{{out}}

```txt

http://foo bar/
http%3A%2F%2Ffoo%20bar%2F
mailto:"Ivan Aim" <ivan.aim@email.com>
mailto%3A%22Ivan%20Aim%22%20%3Civan%2Eaim%40email%2Ecom%3E
mailto:"Irma User" <irma.user@mail.com>
mailto%3A%22Irma%20User%22%20%3Cirma%2Euser%40mail%2Ecom%3E
http://foo.bar.com/~user-name/_subdir/*~.html
http%3A%2F%2Ffoo%2Ebar%2Ecom%2F%7Euser%2Dname%2F%5Fsubdir%2F%2A%7E%2Ehtml

RFC3986
http://foo bar/
http%3A%2F%2Ffoo%20bar%2F
mailto:"Ivan Aim" <ivan.aim@email.com>
mailto%3A%22Ivan%20Aim%22%20%3Civan.aim%40email.com%3E
mailto:"Irma User" <irma.user@mail.com>
mailto%3A%22Irma%20User%22%20%3Cirma.user%40mail.com%3E
http://foo.bar.com/~user-name/_subdir/*~.html
http%3A%2F%2Ffoo.bar.com%2F~user-name%2F_subdir%2F%2A~.html

HTML5
http://foo bar/
http%3A%2F%2Ffoo+bar%2F
mailto:"Ivan Aim" <ivan.aim@email.com>
mailto%3A%22Ivan+Aim%22+%3Civan.aim%40email.com%3E
mailto:"Irma User" <irma.user@mail.com>
mailto%3A%22Irma+User%22+%3Cirma.user%40mail.com%3E
http://foo.bar.com/~user-name/_subdir/*~.html
http%3A%2F%2Ffoo.bar.com%2F%7Euser-name%2F_subdir%2F*%7E.html

```



## NewLISP


```NewLISP
;; simple encoder
;; (source http://www.newlisp.org/index.cgi?page=Code_Snippets)
(define (url-encode str)
  (replace {([^a-zA-Z0-9])} str (format "%%%2X" (char $1)) 0))

(url-encode "http://foo bar/")
```




## Nim


```nim
import cgi

echo encodeUrl("http://foo/bar/")
```


=={{header|Oberon-2}}==
{{works with|oo2c}}

```oberon2

MODULE URLEncoding;
IMPORT
  Out := NPCT:Console,
  ADT:StringBuffer,
  URI := URI:String;
VAR
  encodedUrl: StringBuffer.StringBuffer;
BEGIN
  encodedUrl := NEW(StringBuffer.StringBuffer,512);
  URI.AppendEscaped("http://foo bar/","",encodedUrl);
  Out.String(encodedUrl.ToString());Out.Ln
END URLEncoding.

```

{{out}}

```txt

http%3A%2F%2Ffoo%20bar%2F

```



## Objeck


```objeck

use FastCgi;

bundle Default {
  class UrlEncode {
    function : Main(args : String[]) ~ Nil {
      url := "http://foo bar/";
      UrlUtility->Encode(url)->PrintLine();
    }
  }
}

```


=={{header|Objective-C}}==

```objc
NSString *normal = @"http://foo bar/";
NSString *encoded = [normal stringByAddingPercentEscapesUsingEncoding:NSUTF8StringEncoding];
NSLog(@"%@", encoded);
```


The Core Foundation function <code>CFURLCreateStringByAddingPercentEscapes()</code> provides more options.

{{works with|Mac OS X|10.9+}}
{{works with|iOS|7+}}

```objc
NSString *normal = @"http://foo bar/";
NSString *encoded = [normal stringByAddingPercentEncodingWithAllowedCharacters:[NSCharacterSet alphanumericCharacterSet]];
NSLog(@"%@", encoded);
```


For encoding for various parts of the URL, the allowed character sets <code>[NSCharacterSet URLUserAllowedCharacterSet]</code>, <code>[NSCharacterSet URLPasswordAllowedCharacterSet]</code>, <code>[NSCharacterSet URLHostAllowedCharacterSet]</code>, <code>[NSCharacterSet URLPathAllowedCharacterSet]</code>, <code>[NSCharacterSet URLQueryAllowedCharacterSet]</code>, or <code>[NSCharacterSet URLFragmentAllowedCharacterSet]</code> are provided.


## OCaml


Using the library [http://projects.camlcity.org/projects/ocamlnet.html ocamlnet] from the interactive loop:


```ocaml
$ ocaml
# #use "topfind";;
# #require "netstring";;

# Netencoding.Url.encode "http://foo bar/" ;;
- : string = "http%3A%2F%2Ffoo+bar%2F"
```



## ooRexx

The solution shown at [[#REXX|Rexx]] (version 1) is a valid ooRexx program.

version 2 uses constructs not supported by ooRexx:

url.=; and $ as variable symbol


## Perl



```perl
sub urlencode {
    my $s = shift;
    $s =~ s/([^-A-Za-z0-9_.!~*'() ])/sprintf("%%%02X", ord($1))/eg;
    $s =~ tr/ /+/;
    return $s;
}

print urlencode('http://foo bar/')."\n";

```

{{out}}

```txt
http%3A%2F%2Ffoo+bar%2F

```



```perl
use URI::Escape;

my $s = 'http://foo/bar/';
print uri_escape($s);
```


Use standard CGI module:

```perl
use 5.10.0;
use CGI;

my $s = 'http://foo/bar/';
say $s = CGI::escape($s);
say $s = CGI::unescape($s);
```



## Perl 6



```perl6
my $url = 'http://foo bar/';

say $url.subst(/<-alnum>/, *.ord.fmt("%%%02X"), :g);
```


{{out}}

```txt
http%3A%2F%2Ffoo%20bar%2F
```



## Phix


```Phix
--
-- demo\rosetta\encode_url.exw
--
### =====================

--
function nib(integer b)
    return b+iff(b<=9?'0':'A'-10)
end function

function encode_url(string s, string exclusions="", integer spaceplus=0)
string res = ""
    for i=1 to length(s) do
        integer ch = s[i]
        if ch=' ' and spaceplus then
            ch = '+'
        elsif not find(ch,exclusions)
          and (ch<'0'
           or (ch>'9' and ch<'A')
           or (ch>'Z' and ch<'a')
           or  ch>'z') then
            res &= '%'
            res &= nib(floor(ch/#10))
            ch = nib(and_bits(ch,#0F))
        end if
        res &= ch
    end for
    return res
end function

printf(1,"%s\n",{encode_url("http://foo bar/")})
```

{{Out}}

```txt

http%3A%2F%2Ffoo%20bar%2F

```



## PHP


```php
<?php
$s = 'http://foo/bar/';
$s = rawurlencode($s);
?>
```

There is also <code>urlencode()</code>, which also encodes spaces as "+" signs


## PicoLisp


```PicoLisp
(de urlEncodeTooMuch (Str)
   (pack
      (mapcar
         '((C)
            (if (or (>= "9" C "0") (>= "Z" (uppc C) "A"))
               C
               (list '% (hex (char C))) ) )
         (chop Str) ) ) )
```

Test:

```txt
: (urlEncodeTooMuch "http://foo bar/")
-> "http%3A%2F%2Ffoo%20bar%2F"
```




## Powershell


```Powershell

[uri]::EscapeDataString('http://foo bar/')

http%3A%2F%2Ffoo%20bar%2F

```



## PureBasic


```PureBasic
URL$ = URLEncoder("http://foo bar/")
```



## Python


```python
import urllib
s = 'http://foo/bar/'
s = urllib.quote(s)
```

There is also <code>urllib.quote_plus()</code>, which also encodes spaces as "+" signs


## R


R has a built-in


```R
URLencode("http://foo bar/")
```


function, but it doesn't fully follow RFC guidelines, so we have to use another R package to accomplish the task:


```R

library(RCurl)
curlEscape("http://foo bar/")

```




## Racket


```racket

#lang racket
(require net/uri-codec)
(uri-encode "http://foo bar/")

```



## REALbasic

Using the built-in encoding method, which doesn't permit exceptions:

```vb

  Dim URL As String = "http://foo bar/"
  URL = EncodeURLComponent(URL)
  Print(URL)

```


With optional exceptions. A "ParamArray" is an array of zero or more additional arguments passed by the caller:

```vb

Function URLEncode(Data As String, ParamArray Exceptions() As String) As String
  Dim buf As String
  For i As Integer = 1 To Data.Len
    Dim char As String = Data.Mid(i, 1)
    Select Case Asc(char)
    Case 48 To 57, 65 To 90, 97 To 122, 45, 46, 95
      buf = buf + char
    Else
      If Exceptions.IndexOf(char) > -1 Then
        buf = buf + char
      Else
        buf = buf + "%" + Left(Hex(Asc(char)) + "00", 2)
      End If
    End Select
  Next
  Return buf
End Function

 'usage
 Dim s As String = URLEncode("http://foo bar/") ' no exceptions
 Dim t As String = URLEncode("http://foo bar/", "!", "?", ",") ' with exceptions

```



## REXX


### version 1


```REXX
/* Rexx */
do
  call testcase
  say
  say RFC3986
  call testcase RFC3986
  say
  say HTML5
  call testcase HTML5
  say
  return
end
exit

/* -------------------------------------------------------------------------- */
encode:
procedure
do
  parse arg url, varn .
  parse upper var varn variation
  drop RFC3986 HTML5
  opts. = ''
  opts.RFC3986 = '-._~'
  opts.HTML5   = '-._*'

  rp = ''
  do while length(url) > 0
    parse var url tc +1 url
    select
      when datatype(tc, 'A') then do
        rp = rp || tc
        end
      when tc == ' ' then do
        if variation = HTML5 then
          rp = rp || '+'
        else
          rp = rp || '%' || c2x(tc)
        end
      otherwise do
        if pos(tc, opts.variation) > 0 then do
          rp = rp || tc
          end
        else do
          rp = rp || '%' || c2x(tc)
          end
        end
      end
    end

  return rp
end
exit

/* -------------------------------------------------------------------------- */
testcase:
procedure
do
  parse arg variation
  X = 0
  url. = ''
  X = X + 1; url.0 = X; url.X = 'http://foo bar/'
  X = X + 1; url.0 = X; url.X = 'mailto:"Ivan Aim" <ivan.aim@email.com>'
  X = X + 1; url.0 = X; url.X = 'mailto:"Irma User" <irma.user@mail.com>'
  X = X + 1; url.0 = X; url.X = 'http://foo.bar.com/~user-name/_subdir/*~.html'

  do i_ = 1 to url.0
    say url.i_
    say encode(url.i_, variation)
    end i_

  return
end

```


{{out}}

```txt

http://foo bar/
http%3A%2F%2Ffoo%20bar%2F
mailto:"Ivan Aim" <ivan.aim@email.com>
mailto%3A%22Ivan%20Aim%22%20%3Civan%2Eaim%40email%2Ecom%3E
mailto:"Irma User" <irma.user@mail.com>
mailto%3A%22Irma%20User%22%20%3Cirma%2Euser%40mail%2Ecom%3E
http://foo.bar.com/~user-name/_subdir/*~.html
http%3A%2F%2Ffoo%2Ebar%2Ecom%2F%7Euser%2Dname%2F%5Fsubdir%2F%2A%7E%2Ehtml

RFC3986
http://foo bar/
http%3A%2F%2Ffoo%20bar%2F
mailto:"Ivan Aim" <ivan.aim@email.com>
mailto%3A%22Ivan%20Aim%22%20%3Civan.aim%40email.com%3E
mailto:"Irma User" <irma.user@mail.com>
mailto%3A%22Irma%20User%22%20%3Cirma.user%40mail.com%3E
http://foo.bar.com/~user-name/_subdir/*~.html
http%3A%2F%2Ffoo.bar.com%2F~user-name%2F_subdir%2F%2A~.html

HTML5
http://foo bar/
http%3A%2F%2Ffoo+bar%2F
mailto:"Ivan Aim" <ivan.aim@email.com>
mailto%3A%22Ivan+Aim%22+%3Civan.aim%40email.com%3E
mailto:"Irma User" <irma.user@mail.com>
mailto%3A%22Irma+User%22+%3Cirma.user%40mail.com%3E
http://foo.bar.com/~user-name/_subdir/*~.html
http%3A%2F%2Ffoo.bar.com%2F%7Euser-name%2F_subdir%2F*%7E.html

```



### version 2


```rexx
/*REXX program encodes a  URL  text,    blanks ──► +,    preserves  -._*    and   -._~  */
url.=;                              url.1= 'http://foo bar/'
                                    url.2= 'mailto:"Ivan Aim" <ivan.aim@email.com>'
                                    url.3= 'mailto:"Irma User" <irma.user@mail.com>'
                                    url.4= 'http://foo.bar.com/~user-name/_subdir/*~.html'
     do j=1  while url.j\=='';  say
     say '  original: '             url.j
     say '   encoded: '   URLencode(url.j)
     end   /*j*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
URLencode: procedure; parse arg $,,z;        t1= '-._~'              /*get args, null Z.*/
           skip=0;                           t2= '-._*'
                    do k=1  for length($);   _=substr($, k, 1)       /*get a character. */
                    if skip\==0  then do;    skip=skip-1             /*skip t1 or t2 ?  */
                                             iterate                 /*skip a character.*/
                                      end
                       select
                       when datatype(_, 'A')    then z=z || _        /*is alphanumeric ?*/
                       when _==' '              then z=z'+'          /*is it  a  blank ?*/
                       when substr($, k, 4)==t1 |,                   /*is it  t1 or t2 ?*/
                            substr($, k, 4)==t2   then do;  skip=3   /*skip 3 characters*/
                                                            z=z || substr($, k, 4)
                                                       end
                       otherwise   z=z'%'c2x(_)                      /*special character*/
                       end   /*select*/
                    end      /*k*/
           return z
```

'''output'''   when using the default input:

```txt

  original:  http://foo bar/
   encoded:  http%3A%2F%2Ffoo+bar%2F

  original:  mailto:"Ivan Aim" <ivan.aim@email.com>
   encoded:  mailto%3A%22Ivan+Aim%22+%3Civan%2Eaim%40email%2Ecom%3E

  original:  mailto:"Irma User" <irma.user@mail.com>
   encoded:  mailto%3A%22Irma+User%22+%3Cirma%2Euser%40mail%2Ecom%3E

  original:  http://foo.bar.com/~user-name/_subdir/*~.html
   encoded:  http%3A%2F%2Ffoo%2Ebar%2Ecom%2F%7Euser%2Dname%2F%5Fsubdir%2F%2A%7E%2Ehtml

```



## Ruby

<code>CGI.escape</code> encodes all characters except '-.0-9A-Z_a-z'.


```ruby
require 'cgi'
puts CGI.escape("http://foo bar/").gsub("+", "%20")
# => "http%3A%2F%2Ffoo%20bar%2F"
```


Programs should not call <code>URI.escape</code> (alias <code>URI.encode</code>), because it fails to encode some characters. <code>URI.escape</code> is [http://www.ruby-forum.com/topic/207489 obsolete] since Ruby 1.9.2.

<code>URI.encode_www_form_component</code> is a new method from Ruby 1.9.2. It obeys HTML 5 and encodes all characters except '-.0-9A-Z_a-z' and '*'.

{{works with|Ruby|1.9.2}}

```ruby
require 'uri'
puts URI.encode_www_form_component("http://foo bar/").gsub("+", "%20")
# => "http%3A%2F%2Ffoo%20bar%2F"
```



## Run BASIC


```runbasic
urlIn$ = "http://foo bar/"

for i = 1 to len(urlIn$)
  a$ = mid$(urlIn$,i,1)
  if (a$ >= "0" and a$ <= "9") _
  or (a$ >= "A" and a$ <= "Z") _
  or (a$ >= "a" and a$ <= "z") then url$ = url$ + a$ else url$ = url$ + "%"+dechex$(asc(a$))
next i
print urlIn$;" -> ";url$
```


```txt
http://foo bar/ -> http%3A%2F%2Ffoo%20bar%2F
```



## Rust


```Rust
const INPUT: &str = "http://foo bar/";
const MAX_CHAR_VAL: u32 = std::char::MAX as u32;

fn main() {
    let mut buff = [0; 4];
    println!("{}", INPUT.chars()
        .map(|ch| {
            match ch as u32 {
                0 ..= 47 | 58 ..= 64 | 91 ..= 96 | 123 ..= MAX_CHAR_VAL => {
                    ch.encode_utf8(&mut buff);
                    buff[0..ch.len_utf8()].iter().map(|&byte| format!("%{:X}", byte)).collect::<String>()
                },
                _ => ch.to_string(),
            }
        })
        .collect::<String>()
    );
}
```

{{Out}}

```txt

http%3A%2F%2Ffoo%20bar%2F

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
The function [http://seed7.sourceforge.net/libraries/encoding.htm#toPercentEncoded%28in_string%29 toPercentEncoded]
encodes every character except 0-9, A-Z, a-z and the characters '-', '.', '_', '~'.
The function [http://seed7.sourceforge.net/libraries/encoding.htm#toUrlEncoded%28in_string%29 toUrlEncoded]
works like ''toPercentEncoded'' and additionally encodes a space with '+'.
Both functions work for byte sequences (characters beyond '\255\' raise the exception RANGE_ERROR).
To encode Unicode characters it is necessary to convert them to UTF-8 with ''striToUtf8'' before.
```seed7
$ include "seed7_05.s7i";
  include "encoding.s7i";

const proc: main is func
  begin
    writeln(toPercentEncoded("http://foo bar/"));
    writeln(toUrlEncoded("http://foo bar/"));
  end func;
```
{{out}}
 http%3A%2F%2Ffoo%20bar%2F
 http%3A%2F%2Ffoo+bar%2F


## Sidef

{{trans|Perl}}

```ruby
func urlencode(str) {
    str.gsub!(%r"([^-A-Za-z0-9_.!~*'() ])", {|a| "%%%02X" % a.ord});
    str.gsub!(' ', '+');
    return str;
}

say urlencode('http://foo bar/');
```

{{out}}

```txt
http%3A%2F%2Ffoo+bar%2F
```



## Tcl


```tcl
# Encode all except "unreserved" characters; use UTF-8 for extended chars.
# See http://tools.ietf.org/html/rfc3986 §2.4 and §2.5
proc urlEncode {str} {
    set uStr [encoding convertto utf-8 $str]
    set chRE {[^-A-Za-z0-9._~\n]};		# Newline is special case!
    set replacement {%[format "%02X" [scan "\\\0" "%c"]]}
    return [string map {"\n" "%0A"} [subst [regsub -all $chRE $uStr $replacement]]]
}
```

Demonstrating:

```tcl
puts [urlEncode "http://foo bar/"]
```

{{out}}

```txt
http%3A%2F%2Ffoo%20bar%2F%E2%82%AC
```



## TUSCRIPT


```tuscript

$$ MODE TUSCRIPT
text="http://foo bar/"
BUILD S_TABLE spez_char="::>/:</::<%:"
spez_char=STRINGS (text,spez_char)
LOOP/CLEAR c=spez_char
c=ENCODE(c,hex),c=concat("%",c),spez_char=APPEND(spez_char,c)
ENDLOOP
url_encoded=SUBSTITUTE(text,spez_char,0,0,spez_char)
print "text:    ", text
PRINT "encoded: ", url_encoded

```

{{out}}

```txt

text:    http://foo bar/
encoded: http%3A%2F%2Ffoo%20bar%2F

```



## UNIX Shell


{{works with|bash}}
unfortunately ksh does not support <code>"'$c"</code> syntax

```bash
function urlencode
{
	typeset decoded=$1 encoded= rest= c=
	typeset rest2= bug='rest2=${rest}'

	if [[ -z ${BASH_VERSION} ]]; then
		# bug /usr/bin/sh HP-UX 11.00
		typeset _decoded='xyz%26xyz'
		rest="${_decoded#?}"
		c="${_decoded%%${rest}}"
		if (( ${#c} != 1 )); then
			typeset qm='????????????????????????????????????????????????????????????????????????'
			typeset bug='(( ${#rest} > 0 )) && typeset -L${#rest} rest2="${qm}" || rest2=${rest}'
		fi
	fi

	rest="${decoded#?}"
	eval ${bug}
	c="${decoded%%${rest2}}"
	decoded="${rest}"

	while [[ -n ${c} ]]; do
		case ${c} in
		[-a-zA-z0-9.])
			;;
		' ')
			c='+'
			;;
		*)
			c=$(printf "%%%02X" "'$c")
			;;
		esac

		encoded="${encoded}${c}"

		rest="${decoded#?}"
		eval ${bug}
		c="${decoded%%${rest2}}"
		decoded="${rest}"
	done

	if [[ -n ${BASH_VERSION:-} ]]; then
		\echo -E "${encoded}"
	else
		print -r -- "${encoded}"
	fi
}

```



## VBScript


```VBScript
Function UrlEncode(url)
	For i = 1 To Len(url)
		n = Asc(Mid(url,i,1))
		If (n >= 48 And n <= 57) Or (n >= 65 And n <= 90) _
			Or (n >= 97 And n <= 122) Then
			UrlEncode = UrlEncode & Mid(url,i,1)
		Else
			ChrHex = Hex(Asc(Mid(url,i,1)))
                        For j = 0 to (Len(ChrHex) / 2) - 1
			    UrlEncode = UrlEncode & "%" & Mid(ChrHex,(2*j) + 1,2)
                        Next
		End If
	Next
End Function

WScript.Echo UrlEncode("http://foo baré/")
```


{{out}}

```txt
http%3A%2F%2Ffoo%20bar%C3%A9%2F
```



## XPL0


```XPL0
code Text=12;
string 0;               \use zero-terminated strings

func Encode(S0);        \Encode URL string and return its address
char S0;
char HD, S1(80);        \BEWARE: very temporary string space returned
int  C, I, J;
[HD:= "0123456789ABCDEF"; \hex digits
I:= 0;  J:= 0;
repeat  C:= S0(I);  I:= I+1;
        if C>=^0 & C<=^9 ! C>=^A & C<=^Z ! C>=^a & C<=^z ! C=0
        then    [S1(J):= C;  J:= J+1]           \simply pass char to S1
        else    [S1(J):= ^%;  J:= J+1;          \encode char into S1
                 S1(J):= HD(C>>4);  J:= J+1;
                 S1(J):= HD(C&$0F); J:= J+1;
                ];
until   C=0;
return S1;
];

Text(0, Encode("http://foo bar/"))
```


{{out}}

```txt

http%3A%2F%2Ffoo%20bar%2F

```



## Yabasic

{{trans|Phix}}

```Yabasic
sub encode_url$(s$, exclusions$, spaceplus)
    local res$, i, ch$

    for i=1 to len(s$)
        ch$ = mid$(s$, i, 1)
        if ch$ = " " and spaceplus then
            ch$ = "+"
        elsif not instr(esclusions$, ch$) and (ch$ < "0" or (ch$ > "9" and ch$ < "A") or (ch$ > "Z" and ch$ < "a") or  ch$ > "z") then
            res$ = res$ + "%"
            ch$ = upper$(hex$(asc(ch$)))
        end if
        res$ = res$ + ch$
    next i
    return res$
end sub

print encode_url$("http://foo bar/")
```



## zkl

Using lib cURL:

```zkl
var CURL=Import("zklCurl");
CURL.urlEncode("http://foo bar/") //--> "http%3A%2F%2Ffoo%20bar%2F"
```

