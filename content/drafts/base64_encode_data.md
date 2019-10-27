+++
title = "Base64 encode data"
description = ""
date = 2019-08-26T20:28:51Z
aliases = []
[extra]
id = 16043
[taxonomies]
categories = []
tags = []
+++

{{draft task}}
Convert an array of bytes or binary string to the [[wp:Base64|base64-encoding]] of that string and output that value. Use [http://rosettacode.org/favicon.ico the icon for Rosetta Code] as the data to convert.

See also [[Base64 decode data]].


## ABAP


```ABAP
DATA: li_client  TYPE REF TO if_http_client,
      lv_encoded TYPE string,
      lv_data    TYPE xstring.


cl_http_client=>create_by_url(
  EXPORTING
    url    = 'http://rosettacode.org/favicon.ico'
  IMPORTING
    client = li_client ).

li_client->send( ).
li_client->receive( ).

lv_data = li_client->response->get_data( ).

CALL FUNCTION 'SSFC_BASE64_ENCODE'
  EXPORTING
    bindata = lv_data
  IMPORTING
    b64data = lv_encoded.

WHILE strlen( lv_encoded ) > 100.
  WRITE: / lv_encoded(100).
  lv_encoded = lv_encoded+100.
ENDWHILE.
WRITE: / lv_encoded.

```


{{out}}

```txt

AAABAAIAEBAAAAAAAABoBQAAJgAAACAgAAAAAAAAqAgAAI4FAAAoAAAAEAAAACAAAAABAAgAAAAAAEABAAAAAAAAAAAAAAAAAAAA
...
AAEAAAABAAAAAQAAAAEAAAABAAAAAQAAAAEAAAABAAAAAQAAAAE=

```



## ALGOL 68

This program is run on a modified Algol 68 Genie 2.8. That interpreter has some bugs, so it does not do binary tcp/ip requests, and I made patches/bugfixes to it in order to run this task.

```algol68

STRING codes = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"[@0];

PROC get web page = (STRING host, path) STRING:
   BEGIN
      STRING reply;
      INT rc;
      IF rc := tcp request (reply, host,
                               "GET /favicon.ico  HTTP/1.0" + crlf +
                               "Host: rosettacode.org" + crlf +
                               crlf, 80);
            rc = 0 THEN
	 SKIP #print (reply)#
      ELSE print (strerror (rc))
      FI;
      IF rc = 0 AND grep in string ("^HTTP/[0-9.]+ 200", reply, NIL, NIL) = 0 THEN
	 INT p := 0;
	 FOR i TO UPB reply WHILE p = 0 DO
	    IF reply[i] = carriage return ANDF reply[i+1] = line feed AND reply[i+2] = carriage return AND reply[i+3] = line feed THEN
	       p := i
	    FI
	 OD;	       
	 IF p /= 0 THEN
	    STRING headers = reply[:p],
	           body = reply[p+4:];
	    body
	 ELSE
	    ""
	 FI 
      ELSE 
	 print (strerror (rc)); ""
      FI
   END;


PROC base64_encode = (STRING s) STRING:
   BEGIN 
      STRING result := "";
      BITS u;
      FOR i BY 3 TO UPB s DO
	 u := BIN ABS s[i] SHL 16 OR
	      IF i+1 <= UPB s THEN BIN ABS s[i+1] SHL 8 OR
	         IF i+2 <= UPB s THEN BIN ABS s[i+2]
		 ELSE 16r0
                 FI 
	      ELSE 16r0
	      FI;

	 result +:= codes[ABS (u SHR 18 AND 16r3f)] +
                    codes[ABS (u SHR 12 AND 16r3f)] + 
	            (i + 1 <= UPB s | codes[ABS (u SHR 6 AND 16r3f)] | "=") +
		    (i + 2 <= UPB s | codes[ABS (u AND 16r3f)] | "=")
      OD;
      result
   END;


CHAR line feed = REPR 10, carriage return = REPR 13;
STRING crlf = carriage return + line feed;
STRING host = "rosettacode.org";

STRING rosettacode icon = get web page (host, "http://rosettacode.org/favicon.ico");
STRING encoded icon = base64_encode (rosettacode icon);
print ((encoded icon, new line))

```

{{out}}

```txt

First 80 chars and last 80 chars of output
AAABAAIAEBAAAAAAAABoBQAAJgAAACAgAAAAAAAAqAgAAI4FAAAoAAAAEAAAACAAAAABAAgAAAAAAEAB
...
AAABAAAAAQAAAAEAAAABAAAAAQAAAAEAAAABAAAAAQAAAAEAAAABAAAAAQAAAAEAAAABAAAAAQAAAAE=

```


## ARM Assembly


```ARM Assembly

.section .rodata
ch64: .ascii "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
example_text: .ascii "Hello World"
example_base64: .ascii "SGVsbG8gV29ybGQ="

.section .bss
.lcomm buffer, 64

.section .text
.global _start

@ int base64_encode(char *d, char *s, int len)
base64_encode:
    push {r3-r7}
    ldr r6, =ch64
    mov r7, r0
be_cycle:
    ldrb r3, [r1]
    lsr r4, r3, #2
    ldrb r4, [r6, r4]
    strb r4, [r0]

    add r0, r0, #1
    add r1, r1, #1
    subs r2, r2, #1
    moveq r4, #0
    ldrneb r4, [r1]

    and r3, r3, #3
    lsl r3, r3, #4
    lsr r5, r4, #4
    orr r3, r3, r5
    ldrb r3, [r6, r3]
    strb r3, [r0]

    add r0, r0, #1
    add r1, r1, #1
    beq be_store2
    subs r2, r2, #1
    moveq r3, #0
    ldrneb r3, [r1]

    and r4, r4, #15
    lsl r4, r4, #2
    lsr r5, r3, #6
    orr r4, r4, r5
    ldrb r4, [r6, r4]
    strb r4, [r0]

    add r0, r0, #1
    add r1, r1, #1
    beq be_store1

    and r3, r3, #0x3f
    ldrb r3, [r6, r3]
    strb r3, [r0]

    add r0, r0, #1
    subs r2, r2, #1
    beq be_exit
    bne be_cycle
be_store2:
    mov r3, #'='
    strb r3, [r0]
    add r0, r0, #1
be_store1:
    mov r3, #'='
    strb r3, [r0]
    add r0, r0, #1
be_exit:
    sub r0, r0, r7
    pop {r3-r7}
    mov pc, lr

@ int base64_decode(char *d, char *s, int len)
base64_decode:
    push {r3-r6,lr}
    mov r3, r0
    mov r6, r0
bd_cycle:
    ldrb r0, [r1]
    bl char_decode
    add r1, r1, #1

    lsl r4, r0, #2
    ldrb r0, [r1]
    bl char_decode
    add r1, r1, #1

    lsr r5, r0, #4
    orr r4, r4, r5
    strb r4, [r3]
    add r3, r3, #1

    lsl r4, r0, #4
    ldrb r0, [r1]
    cmp r0, #'='
    beq exit
    bl char_decode
    add r1, r1, #1

    lsr r5, r0, #2
    orr r4, r4, r5
    strb r4, [r3]
    add r3, r3, #1

    lsl r4, r0, #6
    ldrb r0, [r1]
    cmp r0, #'='
    beq exit
    bl char_decode
    add r1, r1, #1

    orr r4, r4, r0
    strb r4, [r3]
    add r3, r3, #1
    subs r2, r2, #4
    bne bd_cycle
exit:
    sub r0, r3, r6
    pop {r3-r6, pc}

@ char char_decode(char c)
char_decode:
    subs r0, r0, #'A'
    blt cd_digit
    cmp r0, #25
    subgt r0, r0, #6
    mov pc, lr
cd_digit:
    adds r0, r0, #17
    blt cd_ps
    add r0, r0, #52
    mov pc, lr
cd_ps:
    cmn r0, #5
    moveq r0, #62
    movne r0, #63
    mov pc, lr

_start:
    ldr r0, =buffer
    ldr r1, =example_text
    mov r2, #11
    bl base64_encode
    
    mov r2, r0
    mov r7, #4
    mov r0, #1
    ldr r1, =buffer
    swi #0
    
    ldr r0, =buffer
    ldr r1, =example_base64
    mov r2, #16
    bl base64_decode

    mov r2, r0
    mov r7, #4
    mov r0, #1
    ldr r1, =buffer
    swi #0

    mov r7, #1
    mov r0, #0
    swi #0

```



## C


### libresolv

{{libheader|libresolv}} (libresolv is included on most Unix-like systems)

```c>#include <stdio.h

#include <stdlib.h>
#include <resolv.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/mman.h>

int main() {
  int fin = open("favicon.ico",  O_RDONLY);
  if (fin == -1)
    return 1;

  struct stat st;
  if (fstat(fin, &st))
    return 1;

  void *bi = mmap(0, st.st_size, PROT_READ, MAP_PRIVATE, fin,  0);
  if (bi == MAP_FAILED)
    return 1;

  int outLength = ((st.st_size + 2) / 3) * 4 + 1;
  char *outBuffer = malloc(outLength);
  if (outBuffer == NULL)
    return 1;

  int encodedLength = b64_ntop(bi, st.st_size, outBuffer, outLength);
  if (encodedLength < 0)
    return 1;

  puts(outBuffer);

  free(outBuffer);
  munmap(bi, st.st_size);
  close(fin);

  return 0;
}
```

Compile with

```txt
gcc -lresolv -o base64encode base64encode.c
```



### Manual implementation

The following reads standard input and writes base64-encoded stream to standard output, e.g. <tt>./a.out <some_random_file >/dev/null</tt> if you don't want to see the output.  It gives identical output as the common <tt>base64</tt> utility program, though much less efficiently.

```c>#include <stdio.h

#include <unistd.h>

typedef unsigned long UL;

int main(void)
{
	const char *alpha =	"ABCDEFGHIJKLMNOPQRSTUVWXYZ"
				"abcdefghijklmnopqrstuvwxyz"
				"0123456789+/";
	unsigned char c[4];
	UL u, len, w = 0;

	do {
		c[1] = c[2] = 0;

		if (!(len = read(fileno(stdin), c, 3))) break;
		u = (UL)c[0]<<16 | (UL)c[1]<<8 | (UL)c[2];

		putchar(alpha[u>>18]);
		putchar(alpha[u>>12 & 63]);
		putchar(len < 2 ? '=' : alpha[u>>6 & 63]);
		putchar(len < 3 ? '=' : alpha[u & 63]);

		if (++w == 19) w = 0, putchar('\n');
	} while (len == 3);

	if (w) putchar('\n');

	return 0;
}
```



## C++


```cpp

#include <iostream>
#include <fstream>
#include <vector>

typedef unsigned char byte;
using namespace std;

const unsigned m1 = 63 << 18, m2 = 63 << 12, m3 = 63 << 6;

class base64
{
public:
    base64() { char_set = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"; }
    string encode( vector<byte> v )
    {
	string res;
	unsigned d, a = 0, l = static_cast<unsigned>( v.size() );
	while( l > 2 )
	{
	    d = v[a++] << 16 | v[a++] << 8 | v[a++];	
	    res.append( 1, char_set.at( ( d & m1 ) >> 18 ) );	
	    res.append( 1, char_set.at( ( d & m2 ) >> 12 ) );	
	    res.append( 1, char_set.at( ( d & m3 ) >>  6 ) );
	    res.append( 1, char_set.at( d & 63 ) );
	    l -= 3;
	}
	if( l == 2 )
	{
	    d = v[a++] << 16 | v[a++] << 8;
	    res.append( 1, char_set.at( ( d & m1 ) >> 18 ) );	
	    res.append( 1, char_set.at( ( d & m2 ) >> 12 ) );	
	    res.append( 1, char_set.at( ( d & m3 ) >>  6 ) );
	    res.append( 1, '=' );
	}
	else if( l == 1 )
	{
	    d = v[a++] << 16;
	    res.append( 1, char_set.at( ( d & m1 ) >> 18 ) );	
	    res.append( 1, char_set.at( ( d & m2 ) >> 12 ) );	
	    res.append( "==", 2 );
	}
	return res;
    }

private:
    string char_set;
};

int main( int argc, char* argv[] )
{
    base64 b;
    basic_ifstream<byte> f( "favicon.ico", ios::binary );
    string r = b.encode( vector<byte>( ( istreambuf_iterator<byte>( f ) ), istreambuf_iterator<byte>() ) );
    copy( r.begin(), r.end(), ostream_iterator<char>( cout ) );
    return 0;
}

```

{{out}}

```txt

AAABAAIAEBAAAAAAAABoBQAAJgAAACAgAAAAAAAAqAgAAI4FAAAoAAAAEAAAACAAAAABAAgAAAAAAEAB
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAP///wCGiYcARkhHAL/CwAAmKScAam1rAOPm5ACgo6EAV1pYABcZ
...
AAEAAAABAAAAAQAAAAEAAAABAAAAAQAAAAEAAAABAAAAAQAAAAEAAAABAAAAAQAAAAEAAAABAAAAAQAA
AAEAAAABAAAAAQAAAAEAAAABAAAAAQAAAAEAAAABAAAAAQAAAAE=

```


=={{header|C sharp|C#}}==

```csharp
namespace RosettaCode.Base64EncodeData
{
    using System;
    using System.Net;

    internal static class Program
    {
        private static void Main()
        {
            const string path = "http://rosettacode.org/favicon.ico";

            byte[] input;
            using (var client = new WebClient())
            {
                input = client.DownloadData(path);
            }

            var output = Convert.ToBase64String(input);
            Console.WriteLine(output);
        }
    }
}
```

Output:

```txt
AAABAAIAEBAAAAAAAABoBQAAJgAAACAg...AAABAAAAAQAAAAEAAAABAAAAAQAAAAE=
```



## D


```d
void main() {
    import std.stdio, std.base64, std.net.curl, std.string;

    const f = "http://rosettacode.org/favicon.ico".get.representation;
    Base64.encode(f).writeln;
}
```

{{out}}

```txt
AAABAAIAEBAAAAAAAABoBQAAJgAAACAgAAAAAAAAwqgIAADCjgUAACgAAAAQAAAAIAA...
AAAQAAAAEAAAABAAAAAQAAAAEAAAABAAAAAQAAAAEAAAABAAAAAQAAAAEAAAABAAAAAQ==
```



## Delphi


```delphi
program Base64EncodeData;
{$APPTYPE CONSOLE}
uses IdHTTP, IdCoderMIME;

var
  lSrcString: string;
  lHTTP: TIdHTTP;
begin
  lHTTP := TIdHTTP.Create(nil);
  try
    lSrcString := lHTTP.Get('http://rosettacode.org/favicon.ico');
    Writeln(TIdEncoderMIME.EncodeString(lSrcString));
  finally
    lHTTP.Free;
  end;
end.
```



## Elixir


```elixir
data = File.read!("favicon.ico")
encoded = :base64.encode(data)
IO.puts encoded
```


{{out}}

```txt

AAABAAIAEBAAAAAAAABoBQAAJgAAACAgAAAAAAAAqAgAAI4FAAAoAAAAEAAAACAAAAABAAgAAAAAAEAB
...
AAEAAAABAAAAAQAAAAEAAAABAAAAAQAAAAEAAAABAAAAAQAAAAE=

```



## Erlang


```erlang
-module(base64demo).
-export([main/0]).

main() ->
    {ok, Data} = file:read_file("favicon.ico"),
    Encoded = encode_library(Data),
    io:format("~s",[Encoded]).

%% Demonstrating with the library function.
encode_library(Data) ->
    base64:encode(Data).
```


{{out}}

```txt
AAABAAIAEBAAAAAAAABoBQAAJgAAACAgAAAAAAAAqAgAAI4F...AAAEAAAABAAAAAQAAAAEAAAABAAAAAQAAAAE=
```



## Factor


```factor
USING: base64 http.client io kernel strings ;

"http://rosettacode.org/favicon.ico" http-get nip
>base64-lines >string print
```

{{out}}

```txt

AAABAAIAEBAAAAAAAABoBQAAJgAAACAg...AAABAAAAAQAAAAEAAAABAAAAAQAAAAE=

```



## F#


### Standard Library

{{works with|fsharp|4.5}}

```fsharp
open System
open System.Net

let url = "https://rosettacode.org/favicon.ico"

let download (url: string) =
    use client = new WebClient()
    client.DownloadData url

let raw = download url
let encoded = Convert.ToBase64String raw

printfn "%s" encoded
```


{{out}}

```txt
AAABAAIAEBAAAAAAAABoBQAAJgAAACAgAAAAAAAAqAgAAI4FAAAoAAAAEAAAACAAAAABAAgAAAAAAEABAAAA ...
```



### Manual Implementation

{{works with|fsharp|4.5}}

```fsharp
open System.Net

let encode s =
    let chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/".ToCharArray()
                |> Array.map string
    let paddingSize =
        let c = Array.length s % 3
        if c = 0 then 0 else 3 - c
    let s = Array.append s (Array.replicate paddingSize (byte '\x00')) |> Array.map int
    let calc c =
        let n = (s.[c] <<< 16) + (s.[c + 1] <<< 8) + s.[c + 2]
        let n1 = (n >>> 18) &&& 63
        let n2 = (n >>> 12) &&& 63
        let n3 = (n >>> 6) &&& 63
        let n4 = n &&& 63
        chars.[n1] + chars.[n2] + chars.[n3] + chars.[n4]
    [0..3..Array.length s - 1]
    |> List.map calc
    |> List.reduce (+)
    |> fun r -> r.Substring(0, String.length r - paddingSize) + String.replicate paddingSize "="
 
let url = "https://rosettacode.org/favicon.ico"
 
let download (url: string) =
    use client = new WebClient()
    client.DownloadData url

let encoded = url |> download |> encode

printfn "%s" encoded

```


{{out}}

```txt
AAABAAIAEBAAAAAAAABoBQAAJgAAACAgAAAAAAAAqAgAAI4FAAAoAAAAEAAAACAAAAABAAgAAAAAAEABAAAA ...
```



## Go


### Standard Library


```Go
package main

import (
    "encoding/base64"
    "fmt"
    "io/ioutil"
    "net/http"
)

func main() {
    r, err := http.Get("http://rosettacode.org/favicon.ico")
    if err != nil {
        fmt.Println(err)
        return
    }
    defer r.Body.Close()
    d, err := ioutil.ReadAll(r.Body)
    if err != nil {
        fmt.Println(err)
        return
    }
    fmt.Println(base64.StdEncoding.EncodeToString(d))
}
```

{{out}}

```txt

AAABAAIAEBAAAAAAAABoBQAAJg ... AAAABAAAAAQAAAAE=

```


### Manual implementation


```go
// base64 encoding
// A port, with slight variations, of the C version found here:
// http://rosettacode.org/wiki/Base64#C (manual implementation)
//
// go build ; cat favicon.ico | ./base64

package main

import (
	"bytes"
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"strings"
)

const (
	B64_CHUNK_SIZE = 76
)

type UL int64

// Our lookup table.
var alpha string = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

// Base64 encode a raw byte stream.
func B64Encode(raw []byte) (string, error) {
	var buffer strings.Builder
	var reader *bytes.Reader
	var u UL // w UL
	var length int
	var chunk []byte
	var err error

	length = 3
	reader = bytes.NewReader(raw)
	chunk = make([]byte, 3)

	for length == 3 {

		chunk[1] = 0
		chunk[2] = 0

		length, err = reader.Read(chunk)
		if err != nil || len(chunk) == 0 {
			break
		}

		u = UL(chunk[0])<<16 | UL(chunk[1])<<8 | UL(chunk[2])

		buffer.WriteString(string(alpha[u>>18]))
		buffer.WriteString(string(alpha[u>>12&63]))
		if length < 2 {
			buffer.WriteString("=")
		} else {
			buffer.WriteString(string(alpha[u>>6&63]))
		}

		if length < 3 {
			buffer.WriteString("=")
		} else {
			buffer.WriteString(string(alpha[u&63]))
		}
	}

	return buffer.String(), nil
}

// Prettifies the base64 result by interspersing \n chars every B64_CHUNK_SIZE bytes.
// Even though there's a performance hit, i'd rather compose these.
func B64EncodePretty(raw []byte) (string, error) {
	var buffer strings.Builder
	encoded, err := B64Encode(raw)
	if err != nil {
		return "", err
	}
	length := len(encoded)
	chunks := int(length/B64_CHUNK_SIZE) + 1
	for i := 0; i < chunks; i++ {
		chunk := i * B64_CHUNK_SIZE
		end := chunk + B64_CHUNK_SIZE
		if end > length {
			end = chunk + (length - chunk)
		}
		buffer.WriteString(encoded[chunk:end] + "\n")
	}
	return buffer.String(), err
}

func main() {
	contents, err := ioutil.ReadAll(os.Stdin)
	if err != nil {
		log.Fatal("Error reading input: ", err)
	}
	encoded, err := B64EncodePretty(contents)
	if err != nil {
		log.Fatal("Error base64 encoding the input: ", err)
	}
	fmt.Printf("%s", encoded)
}
```

{{out}}

```txt

AAABAAIAEBAAAAAAAABoBQAAJgAAACAgAAAAAAAAqAgAAI4FAAAoAAAAEAAAACAAAAABAAgAAAAA
AEABAAAAAAAAAAAAAAAAAAAAAAAAAAAAAP///wCGiYcARkhHAL/CwAAmKScAam1rAOPm5ACgo6EA
...
AAAAAQAAAAEAAAABAAAAAQAAAAEAAAABAAAAAQAAAAEAAAABAAAAAQAAAAEAAAABAAAAAQAAAAEA
AAABAAAAAQAAAAEAAAABAAAAAQAAAAEAAAABAAAAAQAAAAEAAAABAAAAAQAAAAE=

```



## Haskell


### By hand

{{Trans|C}}
This Haskell code is ported from the C solution (manual implementation) with slight variations.

```Haskell
-- | Base 64 Encoding.
-- A port, with slight variations, of the C version found here:
-- http://rosettacode.org/wiki/Base64#C (manual implementation)
--
-- ghc -Wall base64_encode.hs ; cat favicon.ico | ./base64_encode
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Data.Bits
import Data.Char

import qualified Data.ByteString.Char8 as C

-- | alphaTable: Our base64 lookup table.
alphaTable :: C.ByteString
alphaTable = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

-- | b64Encode: Simple base64 encode function operating on normal C.ByteString's
b64Encode :: C.ByteString -> C.ByteString
b64Encode stream =
  if C.null stream
    then C.empty
    else alphaTable `C.index` shiftR _u 18 `C.cons` alphaTable `C.index`
         (shiftR _u 12 .&. 63) `C.cons`
         (if C.length chunk < 2
            then '='
            else alphaTable `C.index` (shiftR _u 6 .&. 63)) `C.cons`
         (if C.length chunk < 3
            then '='
            else alphaTable `C.index` (_u .&. 63)) `C.cons`
         b64Encode (C.drop 3 stream)
  where
    chunk = C.take 3 stream
    _u = u chunk

-- | b64EncodePretty: Intersperses \n every 76 bytes for prettier output
b64EncodePretty :: C.ByteString -> C.ByteString
b64EncodePretty = makePretty 76 . b64Encode

-- | u: base64 encoding magic
u :: C.ByteString -> Int
u chunk = fromIntegral result :: Int
  where
    result =
      foldl (.|.) 0 $
      zipWith
        shiftL
        (C.foldr (\c acc -> charToInteger c : acc) [] chunk)
        [16, 8, 0]

-- lazy foldl to fix formatting
-- | charToInteger: Convert a Char to an Integer
charToInteger :: Char -> Integer
charToInteger c = fromIntegral (ord c) :: Integer

-- | makePretty: Add new line characters throughout a character stream
makePretty :: Int -> C.ByteString -> C.ByteString
makePretty _ (C.uncons -> Nothing) = C.empty
makePretty by stream = first `C.append` "\n" `C.append` makePretty by rest
  where
    (first, rest) = C.splitAt by stream

main :: IO ()
main = C.getContents >>= C.putStr . b64EncodePretty
```



### Using Data.ByteString.Base64


```haskell
import qualified Data.ByteString.Base64 as Base64 (decode, encode)
import qualified Data.ByteString.Char8 as B (putStrLn, readFile)

main :: IO ()
main = B.readFile "favicon.ico" >>= (B.putStrLn . Base64.encode)
```



## J

'''Solution''' (''[http://www.jsoftware.com/wsvn/addons/trunk/convert/misc/base64.ijs standard library]''):
```j
   load'convert/misc/base64'  NB. use 'tobase64'
```

'''Solution''' (''handrolled''):
```j
   tobase64 =:  padB64~ b2B64 
     padB64 =:  , '=' #~ 0 2 1 i. 3 | #
     b2B64  =:  BASE64 {~ _6 #.\ (8#2) ,@:#: a.&i.
```

'''Example''':
```j
   load'web/gethttp'
   76 {. tobase64 gethttp 'http://rosettacode.org/favicon.ico'
AAABAAIAEBAAAAAAAABoBQAAJgAAACAgAAAAAAAAqAgAAI4FAAAoAAAAEAAAACAAAAABAAgAAAAA
```



## Java


Can also use org.apache.commons.codec.binary.Base64 from Apache Commons Codec


```Java
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.net.URLConnection;
import java.util.Arrays;

public class Base64 {

    private static final char[] alpha = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/".toCharArray();

    static String base64(InputStream is) throws IOException {
        StringBuilder sb = new StringBuilder();
        int blocks = 0;

        while (true) {
            int c0 = is.read();
            if (c0 == -1)
                break;
            int c1 = is.read();
            int c2 = is.read();

            int block = ((c0 & 0xFF) << 16) | ((Math.max(c1, 0) & 0xFF) << 8) | (Math.max(c2, 0) & 0xFF);

            sb.append(alpha[block >> 18 & 63]);
            sb.append(alpha[block >> 12 & 63]);
            sb.append(c1 == -1 ? '=' : alpha[block >> 6 & 63]);
            sb.append(c2 == -1 ? '=' : alpha[block & 63]);

            if (++blocks == 19) {
                blocks = 0;
                sb.append('\n');
            }
        }

        if (blocks > 0)
            sb.append('\n');

        return sb.toString();
    }

    private static void assertBase64(String expected, byte[] bytes) throws IOException {
        String actual = base64(new ByteArrayInputStream(bytes));
        if (!actual.equals(expected)) {
            throw new IllegalStateException(String.format("Expected %s for %s, but got %s.",
                    expected, Arrays.toString(bytes), actual));
        }
    }

    private static void testBase64() throws IOException {
        assertBase64("", new byte[]{});
        assertBase64("AA==\n", new byte[]{0});
        assertBase64("AAA=\n", new byte[]{0, 0});
        assertBase64("AAAA\n", new byte[]{0, 0, 0});
        assertBase64("AAAAAA==\n", new byte[]{0, 0, 0, 0});
        assertBase64("/w==\n", new byte[]{-1});
        assertBase64("//8=\n", new byte[]{-1, -1});
        assertBase64("////\n", new byte[]{-1, -1, -1});
        assertBase64("/////w==\n", new byte[]{-1, -1, -1, -1});
    }

    public static void main(String[] args) throws IOException {
        testBase64();

        URLConnection conn = new URL("http://rosettacode.org/favicon.ico").openConnection();
        conn.addRequestProperty("User-Agent", "Mozilla"); // To prevent an HTTP 403 error.
        try (InputStream is = conn.getInputStream()) {
            System.out.println(base64(is));
        }
    }
}
```


```txt
AAABAAIAEBAAAAAAAABoBQ...QAAAAEAAAABAAAAAQAAAAE=
```



###  Java 8 version 


```java
import java.nio.file.*;
import java.util.Base64;

public class Base64Task {

    public static void main(String[] args) throws Exception {
        byte[] bytes = Files.readAllBytes(Paths.get("favicon.ico"));
        String result = Base64.getEncoder().encodeToString(bytes);
        System.out.println(result);
    }
}
```



```txt
AAABAAIAEBAAAAAAAABoBQ...QAAAAEAAAABAAAAAQAAAAE=
```



## JavaScript


```JavaScript
(function(){//ECMAScript doesn't have an internal base64 function or method, so we have to do it ourselves, isn't that exciting?
    function stringToArrayUnicode(str){for(var i=0,l=str.length,n=[];i<l;i++)n.push(str.charCodeAt(i));return n;}
    function generateOnesByLength(n){//Attempts to generate a binary number full of ones given a length.. they don't redefine each other that much.
        var x=0;
        for(var i=0;i<n;i++){
            x<<=1;x|=1;//I don't know if this is performant faster than Math.pow but seriously I don't think I'll need Math.pow, do I?
        }
        return x;
    }
    function paf(_offset,_offsetlength,_number){//I don't have any name for this function at ALL, but I will explain what it does, it takes an offset, a number and returns the base64 number and the offset of the next number.
        //the next function will be used to extract the offset of the number..
        var a=6-_offsetlength,b=8-a;//Oh god, 8 is HARDCODED! Because 8 is the number of bits in a byte!!!
        //And 6 is the mini-byte used by wikipedia base64 article... at least on 2013.
        //I imagine this code being read in 2432 or something, that probably won't happen..
        return [_number&generateOnesByLength(b),b,(_offset<<a)|(_number>>b)];//offset & offsetlength & number 
    }
    function toBase64(uint8array){//of bits, each value may not have more than 255 bits... //a normal "array" should work fine too..
        //From 0x29 to 0x5a plus from 0x61 to 0x7A AND from 0x30 to 0x39
        //Will not report errors if an array index has a value bigger than 255.. it will likely fail.
        var a=[],i,output=[];
        for(i=0x41;i<=0x5a;i++){//A-Z
            a.push(String.fromCharCode(i));
        }
        for(i=0x61;i<=0x7A;i++){//a-z
            a.push(String.fromCharCode(i));
        }
        for(i=0x30;i<=0x39;i++){//0-9
            a.push(String.fromCharCode(i));
        }
        a.push('+','/');
        var offset=0,offsetLength=0,x;
        for(var i=0,l=uint8array.length;i<l;i++){
            if(offsetLength==6){//if offsetlength is 6 that means that a whole offset is occupying the space of a byte, can you believe it.
                offsetLength=0;
                output.push(a[offset]);
                offset=0;
                i--;
                continue;
            }
            x=paf(offset,offsetLength,uint8array[i]);
            offset=x[0];
            offsetLength=x[1];
            output.push(a[x[2]]);
        }
        if(offsetLength){
            if(offsetLength==6){
                output.push(a[offset]);
            }else{
                var y=(6-offsetLength)/2;
                x=paf(offset,offsetLength,0);
                offset=x[0];
                output.push(a[x[2]]);
                switch (y){
                    case 2:output.push('=');//This thingy right here, you know.. the offsets also, no break statement;
                    case 1:output.push('=');break;
                }
            }
        }
        return output.join('');//You can change it so the result is an array instead!!!!
    }

    //Usage

    return toBase64(stringToArrayUnicode("Nothing seems hard to the people who don't know what they're talking about."))
}())
```


===Using btoa (HTML5)===
{{works with|Gecko}}
{{works with|WebKit}}
Works with IE10 or higher.

HTML5 saves the day! introducing two methods to the DOM!
These are btoa and atob, see [http://dev.w3.org/html5/spec-LC/webappapis.html#atob spec]

```JavaScript
window.btoa("String to encode, etc..");//Will throw error if any unicode character is larger than 255 it's counterpart it's the window.atob
```
To make it.. just work, you could convert it to UTF-8 Manually or..
JSON.stringify it or..
encodeURIComponent it.


### Using Node.js

{{works with|Node.js}}

```JavaScript
var http = require('http');
var options = {
  host: 'rosettacode.org',
  path: '/favicon.ico'
};
callback = function(response) {
  var str = '';
  response.on('data', function (chunk) {
    str += chunk;
  });
  response.on('end', function () {
    console.log(new Buffer(str).toString('base64'));//Base64 encoding right here.
  });
}

```



## Jsish

Using a contributed entry that is a small change of the Jsi ''lib/Jsi_Wget.jsi'' sources, to the ''httpGet.jsi'' module. Stored as an attachment at
<nowiki>https://jsish.org/fossil/jsi/wiki/Wget</nowiki> and also listed at [[HTTP#Jsish]].


```javascript
/* Base64, in Jsish */
require('httpGet');
var icon = httpGet('http://rosettacode.org/favicon.ico');
printf("%s", Util.base64(icon, false))
```


{{out}}

```txt
prompt$ jsish base64.jsi | sed -ne '1p;$p'
AAABAAIAEBAAAAAAAABoBQAAJgAAACAgAAAAAAAAqAgAAI4FAAAoAAAAEAAAACAAAAABAAgA
AAAAAQAAAAEAAAABAAAAAQAAAAE=prompt$
```



## Julia

{{works with|Julia|0.6}}


```julia
using Requests

file = read(get("https://rosettacode.org/favicon.ico"))
encoded = base64encode(file)

print(encoded)
```


{{out}}

```txt
AAABAAIAEBAAAAAAAABoBQAAJgAAACAgAAAAAAAAqAgAAI4FAAAoAAAAEAAAACAAAAABAAgAAAAAAEABAAAAAAAAAAAAAAAAAAAAAAAAAAAAAP[...]QAAAAE=
```



## Kotlin


```scala
// version 1.1.2

import java.io.File
import java.util.Base64

fun main(args: Array<String>) {
    val path = "favicon.ico" // already downloaded to current directory
    val bytes = File(path).readBytes()
    val base64 = Base64.getEncoder().encodeToString(bytes)
    println(base64)
}
```


{{out}}

```txt

AAABAAIAEBAAAAAAAABoBQ.....AAAAEAAAABAAAAAQAAAAE=

```



## Lasso


```Lasso 
local(
	src = curl('http://rosettacode.org/favicon.ico'),
	srcdata = #src->result
)
#srcdata->encodebase64

// or, in one movement:
curl('http://rosettacode.org/favicon.ico')->result->encodebase64
```



## LiveCode


```LiveCode
put URL "http://rosettacode.org/favicon.ico" into rosettaico
put base64encode(rosettaico)

Ouput
AAABAA...S0tLS0tLS0t...QAAAAE=
```



## Lua


```lua

local dic = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
function encode( t, f ) 
    local b1, b2, b3, b4	
    b1 = 1 + ( ( t & 0xfc0000 ) >> 18 )
    b2 = 1 + ( ( t & 0x03f000 ) >> 12 )
    b3 = 1 + ( ( t & 0x000fc0 ) >>  6 )
    b4 = 1 + ( t & 0x00003f )
    io.write( dic:sub( b1, b1 ), dic:sub( b2, b2 ) )
    if f > 1 then io.write( dic:sub( b3, b3 ) ) else io.write( "=" ) end
    if f > 2 then io.write( dic:sub( b4, b4 ) ) else io.write( "=" ) end
end

local i = assert( io.open( "favicon.ico", "rb" ) )
local iconData = i:read( "*all" )
local dataLen, s, t = #iconData, 1
while( dataLen > 2 ) do 
    t =     ( iconData:sub( s, s ):byte() << 16 ); s = s + 1
    t = t + ( iconData:sub( s, s ):byte() <<  8 ); s = s + 1
    t = t + ( iconData:sub( s, s ):byte()       ); s = s + 1
    dataLen = dataLen - 3
    encode( t, 3 )
end 
if dataLen == 2 then
    t =	    ( iconData:sub( s, s ):byte() << 16 ); s = s + 1;
    t = t + ( iconData:sub( s, s ):byte() <<  8 ); s = s + 1;
    encode( t, 2 )
elseif dataLen == 1 then
    t =	    ( iconData:sub( s, s ):byte() << 16 ); s = s + 1;
    encode( t, 1 )
end
print()

```

{{out}}

```txt

AAABAAIAEBAAAAAAAABoBQAAJgAAACAgAAAAAAAAqAgAAI4FAAAoAAAAEAAAACAAAAABAA
gAAAAAAEABAAAAAAAAAAAAAAAAAAAAAAAAAAAAAP///wCGiYcARkhHAL/CwAAmKScAam1r
...
AAAAAQAAAAEAAAABAAAAAQAAAAEAAAABAAAAAQAAAAEAAAABAAAAAQAAAAEAAAABAAAAAQ
AAAAEAAAABAAAAAQAAAAEAAAABAAAAAQAAAAEAAAABAAAAAQAAAAEAAAABAAAAAQAAAAE=

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
Print[ExportString[
   Import["http://rosettacode.org/favicon.ico", "Text"], "Base64"]];
```

Very interesting results.
{{out}}

```txt
AAABAAIAEBAAAAAAAABoBQAAJgAAACAgAAAAAAAAqAgAAI4FAAAoAAAAEAAAACAAAAABAAgAAAAA
AEABAAAAAAAAAAAAAAAAAAAAAAAAAAAAAP///wCGiYcARkhHAL/CwAAmKScAam1rAOPm5ACgo6EA
V1pYABcZGADO0c8AODs5AK2wrgBzdnQA6+7sAPz//QAAAwEAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
............................................................................
AOaFhYbu7zPmhYWF5oaGhoaGhoaGhoaGhoaGhoaFhekA/////wAAAAEAAAABAAAAAQAAAAEAAAAB
AAAAAQAAAAEAAAABAAAAAQAAAAEAAAABAAAAAQAAAAEAAAABAAAAAQAAAAEAAAABAAAAAQAAAAEA
AAABAAAAAQAAAAEAAAABAAAAAQAAAAEAAAABAAAAAQAAAAEAAAABAAAAAQAAAAE=
```


=={{header|Objective-C}}==
{{works with|Mac OS X|10.6+}}
{{works with|iOS|4.0+}}

```objc>#import <Foundation/Foundation.h


int main(int argc, const char *argv[]) {
  @autoreleasepool {
    NSData *data = [NSData dataWithContentsOfURL:[NSURL URLWithString:@"http://rosettacode.org/favicon.ico"]];
    NSLog(@"%@", [data base64Encoding]);
  }
  return 0;
}
```




## OCaml



```txt

# First we install with opam the lib: https://github.com/mirage/ocaml-base64
$ opam install base64
# Be sure to use the opam environment
$ eval $(opam env)
# Download the file to encode (there is no simple KISS way to do it in OCaml)
$ wget http://rosettacode.org/favicon.ico
# Starting the ocaml toplevel
$ rlwrap ocaml -I $(ocamlfind query base64) base64.cma
        OCaml version 4.07.1

# let load_file f =
    let ic = open_in f in
    let n = in_channel_length ic in
    let s = Bytes.create n in
    really_input ic s 0 n;
    close_in ic;          
    (Bytes.to_string s)
  ;;
val load_file : string -> string = <fun>

# let enc = Base64.encode_exn (load_file "favicon.ico") ;;
val enc : string =
  "AAABAAIAEBAAAAAAAABoBQAAJgAAACAgAAAAAAAAqAgAAI4F"... (* string length 4852; truncated *)

```



## Perl


```perl
#!perl
use strict;
use warnings;
use MIME::Base64;
open(my($fh), "<", "favicon.ico") or die;
local $/;
print encode_base64(<$fh>);

```


{{out}}
The first and last lines of output are:

```txt
AAABAAIAEBAAAAAAAABoBQAAJgAAACAgAAAAAAAAqAgAAI4FAAAoAAAAEAAAACAAAAABAAgAAAAA
```


```txt
AAABAAAAAQAAAAEAAAABAAAAAQAAAAEAAAABAAAAAQAAAAEAAAABAAAAAQAAAAE=
```



## Perl 6


```perl6
sub MAIN {
    my $buf = slurp("./favicon.ico", :bin);
    say buf-to-Base64($buf);
}

my @base64map = flat 'A' .. 'Z', 'a' .. 'z', ^10, '+', '/';

sub buf-to-Base64($buf) {
    join '', gather for $buf.list -> $a, $b = [], $c = [] {
        my $triplet = ($a +< 16) +| ($b +< 8) +| $c;
        take @base64map[($triplet +> (6 * 3)) +& 0x3F];
        take @base64map[($triplet +> (6 * 2)) +& 0x3F];
        if $c.elems {
            take @base64map[($triplet +> (6 * 1)) +& 0x3F];
            take @base64map[($triplet +> (6 * 0)) +& 0x3F];
        }
        elsif $b.elems {
            take @base64map[($triplet +> (6 * 1)) +& 0x3F];
            take '=';
        }
        else { take '==' }
    }
}
```

{{out}}

```txt
AAABAAIAEBAAAAAAAABoBQAAJgAAACAgAA...QAAAAEAAAABAAAAAQAAAAE=
```



## Phix

As this is a draft task, I've gone with the example from wp, for now. [[User:Petelomax|Pete Lomax]] ([[User talk:Petelomax|talk]]) 10:25, 11 September 2015 (UTC)

```Phix
include builtins\base64.e

string s = "Man is distinguished, not only by his reason, but by this singular passion from "&
           "other animals, which is a lust of the mind, that by a perseverance of delight "&
           "in the continued and indefatigable generation of knowledge, exceeds the short "&
           "vehemence of any carnal pleasure."
string e = encode_base64(s)
?e
?decode_base64(e)
```

{{out}}

```txt

TWFuIGlzIGRpc3Rpbmd1aXNoZWQsIG5vdCBvbmx5IGJ5IGhpcyByZWFzb24sIGJ1dCBieSB0aGlzIHNpbmd1bGFyIHBhc3Npb24gZnJvbSBvdGhlciBhbmltYWxzLCB3aGljaCBpcyBhIGx1c3Qgb2YgdGhlIG1p
bmQsIHRoYXQgYnkgYSBwZXJzZXZlcmFuY2Ugb2YgZGVsaWdodCBpbiB0aGUgY29udGludWVkIGFuZCBpbmRlZmF0aWdhYmxlIGdlbmVyYXRpb24gb2Yga25vd2xlZGdlLCBleGNlZWRzIHRoZSBzaG9ydCB2ZWhl
bWVuY2Ugb2YgYW55IGNhcm5hbCBwbGVhc3VyZS4=
"Man is distinguished, not only by his reason, but by this singular passion from other animals, which is a lust of the mind, that by a perseverance of delight i
n the continued and indefatigable generation of knowledge, exceeds the short vehemence of any carnal pleasure."

```



## PHP


```php
<?php echo base64_encode(file_get_contents("http://rosettacode.org/favicon.ico"));/*1 liner*/  ?>
```



## PicoLisp


```PicoLisp
`(== 64 64)
(setq *Char64
   `'(chop
      "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/" ) )
(de char64 (A B)
   (default B 0)
   (get *Char64 (inc (| A B))) )
(de base64 (S)
   (let S (mapcar char (chop S))
      (pack
         (make
            (while (cut 3 'S)
               (let ((A B C) @)
                  (link (char64 (>> 2 A)))
                  (nond
                     (B
                        (link
                           (char64 (>> -4 (& A 3)))
                           '=
                           '= ) )
                     (C
                        (link
                           (char64 (>> -4 (& A 3)) (>> 4 B))
                           (char64 (>> -2 (& B 15)))
                           '= ) )
                     (NIL
                        (link
                           (char64 (>> -4 (& A 3)) (>> 4 B))
                           (char64 (>> -2 (& B 15)) (>> 6 C))
                           (char64 (& C 63))) ) ) ) ) ) ) ) )
(test
   "cGxlYXN1cmUu"
   (base64 "pleasure.") )
(test
   "bGVhc3VyZS4="
   (base64 "leasure.") )
(test
   "ZWFzdXJlLg=="
   (base64 "easure.") )
(test
   "YXN1cmUu"
   (base64 "asure.") )
(test
   "c3VyZS4="
   (base64 "sure.") )
```



## PowerShell


```PowerShell
$webClient = [Net.WebClient]::new()
$bytes = $webClient.DownloadData('http://rosettacode.org/favicon.ico')
 
$output = [Convert]::ToBase64String($bytes)
 
$output
```

{{out}}

```txt

AAABAAIAEBAAAAAAAABoBQAAJgAAACAg...AAABAAAAAQAAAAEAAAABAAAAAQAAAAE=

```



## PureBasic


```purebasic
InitNetwork()
 
*BufferRaw = ReceiveHTTPMemory("http://rosettacode.org/favicon.ico")
If *BufferRaw
	Debug Base64Encoder(*BufferRaw, MemorySize(*BufferRaw))
Else
	Debug "Download failed"
EndIf
```



## Python


```python
import urllib
import base64

data = urllib.urlopen('http://rosettacode.org/favicon.ico').read()
print base64.b64encode(data)
```

(For me this gets the wrong data; the data is actually an error message. But still, it base-64 encodes it.)


## Racket


```racket

#lang racket
(require net/url net/base64)
(base64-encode (call/input-url (string->url "http://rosettacode.org/favicon.ico") 
                               get-pure-port port->bytes))

```

Output:

```racket

#"AAABAAIAEBAAAAAAAABoBQAA...AQAAAAE=\r\n"

```



## REXX

Some computers   (or REXX implementations)   are limited in virtual memory, so the   ''chunk''   size (below) is 

specified as   '''20000'''   to show how the file   (if specified)   can be read in chunks instead of reading it whole. 

A much higher value for   '''chunk'''   could be used for modern systems or implementations.   

```rexx
/*REXX program  converts  text  (from a file  or  the C.L.)  to a  base64  text string. */
parse arg iFID @                                 /*pbtaom optional arguments from the CL*/
if iFID=='' | iFID==","  then iFID='favicon.ico' /*Not specified?  Then use the default.*/
chunk=20000                                      /*# of bytes to read a file at one time*/
                                                 /*If @ isn't a blank, then use CL text.*/
if @=''  then do s=1  by chunk  until y==''      /* " " is    "   "      "   "  the file*/
              y=charin(iFID, s, chunk)           /*read a chunk of the file, assign to Y*/
              @=@ || y                           /*append the chunk  (Y)  to the  @  var*/
              end   /*s*/                        /* [↑]  read a chunk of the file ──► @ */
t=base64(@)                                      /*convert the   @   string to  base 64.*/
say center(' input', 79, "─");     say @         /*show the header and the  input  text.*/
say center('base64', 79, "─");     say t         /*  "   "     "    "   "   base64   "  */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
base64: procedure;  parse arg Q;   $=            /*obtain input string;  and nullify $. */
        z= 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/'

            do i=0  for 64                       /*process each char in the  Z  string. */
            !.i=substr(z, i+1, 1)                /*assign a char of  Z  to a  !  element*/
            end   /*i*/

        b=x2b( c2x(Q) )0000000000000000          /*Q ──► binary,  add some zero padding.*/
        L=length(Q) * 8                          /*compute   Q's   length  (in bits).   */

            do j=1  by 6  to L                   /*traipse through bit string by six.   */
            _=x2d( b2x( substr(b, j, 6) ))       /*compute index into the BASE64 table. */
            $=$ || !._                           /*append to   $   (the output string). */
            end   /*j*/
                                                 /* [↓]  maybe append equal signs to $. */
        return $ || copies('=', 2 * (L//6==2) + (L//6==4) )
```

For the various outputs, several input texts from the Wikipedia article on   ''Base64''   [http://en.wikipedia.org/wiki/Base64]   were used to demonstrate how padding works.



{{out|output|text=  when using the input of:   <tt> , any carnal pleasure. </tt>}}

```txt

──────────────────────────────────── input─────────────────────────────────────
any carnal pleasure.
────────────────────────────────────base64─────────────────────────────────────
YW55IGNhcm5hbCBwbGVhc3VyZS4=

```

{{out|output|text=  when using the input of:   <tt> , any carnal pleasure </tt>}}

```txt

──────────────────────────────────── input─────────────────────────────────────
any carnal pleasure
────────────────────────────────────base64─────────────────────────────────────
YW55IGNhcm5hbCBwbGVhc3VyZQ==

```

{{out|output|text=  when using the input of:   <tt> , any carnal pleasur </tt>}}

```txt

──────────────────────────────────── input─────────────────────────────────────
any carnal pleasur
────────────────────────────────────base64─────────────────────────────────────
YW55IGNhcm5hbCBwbGVhc3Vy

```

{{out|output|text=  when using the input of:   <tt> , any carnal pleasu </tt>}}

```txt

──────────────────────────────────── input─────────────────────────────────────
any carnal pleasu
────────────────────────────────────base64─────────────────────────────────────
YW55IGNhcm5hbCBwbGVhc3U=

```

{{out|output|text=  when using the input of:   <tt> , any carnal pleas </tt>}}

```txt

──────────────────────────────────── input─────────────────────────────────────
any carnal pleas
────────────────────────────────────base64─────────────────────────────────────
YW55IGNhcm5hbCBwbGVhcw==

```



## Ruby


```ruby
require 'open-uri'
require 'base64'

puts Base64.encode64 open('http://rosettacode.org/favicon.ico') {|f| f.read}
```



## Scala


```Scala
import java.net.URL
import java.util.Base64

object Base64S extends App {
  val conn = new URL("http://rosettacode.org/favicon.ico").openConnection
  val bytes = conn.getInputStream.readAllBytes()

  val result = Base64.getEncoder.encodeToString(bytes)
  println(s"${result.take(22)} ... ${result.drop(4830)}")

  assert(Base64.getDecoder.decode(result) sameElements bytes)

  println(s"Successfully completed without errors. [total ${compat.Platform.currentTime - executionStart} ms]")
}
```


## Seed7

The Seed7 library [http://seed7.sourceforge.net/libraries/encoding.htm encoding.s7i] defines
the function [http://seed7.sourceforge.net/libraries/encoding.htm#toBase64(in_string) toBase64],
which encodes a string with the Base64 encoding.


```seed7
$ include "seed7_05.s7i";
  include "gethttp.s7i";
  include "encoding.s7i";

const proc: main is func
  begin
    writeln(toBase64(getHttp("rosettacode.org/favicon.ico")));
  end func;
```



## Sidef


```ruby
var data = %f'favicon.ico'.read(:raw)   # binary string
print data.encode_base64                # print to STDOUT
```



## Tcl

{{works with|Tcl|8.6}}

```tcl
package require Tcl 8.6
package require http

set tok [http::geturl http://rosettacode.org/favicon.ico]
set icondata [http::data $tok]
http::cleanup $tok

puts [binary encode base64 -maxlen 64 $icondata]
```

With older versions of Tcl, the base64 encoding is best supported via an external package:
{{tcllib|base64}}

```tcl
package require base64
package require http

set tok [http::geturl http://rosettacode.org/favicon.ico]
set icondata [http::data $tok]
http::cleanup $tok

puts [base64::encode -maxlen 64 $icondata]
```



## VBA


```vb
Option Explicit
Public Function Decode(s As String) As String
    Dim i As Integer, j As Integer, r As Byte
    Dim FirstByte As Byte, SecndByte As Byte, ThirdByte As Byte
    Dim SixBitArray() As Byte, ResultString As String, Token As String
    Dim Counter As Integer, InputLength As Integer
    InputLength = Len(s)
    ReDim SixBitArray(InputLength + 1)
    j = 1 'j counts the tokens excluding cr, lf and padding
    For i = 1 To InputLength 'loop over s and translate tokens to 0-63
        Token = Mid(s, i, 1)
        Select Case Token
            Case "A" To "Z"
                SixBitArray(j) = Asc(Token) - Asc("A")
                j = j + 1
            Case "a" To "z"
                SixBitArray(j) = Asc(Token) - Asc("a") + 26
                j = j + 1
            Case "0" To "9"
                SixBitArray(j) = Asc(Token) - Asc("0") + 52
                j = j + 1
            Case "+"
                SixBitArray(j) = 62
                j = j + 1
            Case "/"
                SixBitArray(j) = 63
                j = j + 1
            Case "="
                'padding'
            Case Else
                'cr and lf
        End Select
    Next i
    r = (j - 1) Mod 4
    Counter = 1
    For i = 1 To (j - 1) \ 4 'loop over the six bit byte quadruplets
        FirstByte = SixBitArray(Counter) * 4 + SixBitArray(Counter + 1) \ 16
        SecndByte = (SixBitArray(Counter + 1) Mod 16) * 16 + SixBitArray(Counter + 2) \ 4
        ThirdByte = (SixBitArray(Counter + 2) Mod 4) * 64 + SixBitArray(Counter + 3)
        ResultString = ResultString & Chr(FirstByte) & Chr(SecndByte) & Chr(ThirdByte)
        Counter = Counter + 4
    Next i
    Select Case r
        Case 3
            FirstByte = SixBitArray(Counter) * 4 + SixBitArray(Counter + 1) \ 16
            SecndByte = (SixBitArray(Counter + 1) Mod 16) * 16 + SixBitArray(Counter + 2) \ 4
            ResultString = ResultString & Chr(FirstByte) & Chr(SecndByte)
        Case 2
            FirstByte = SixBitArray(Counter) * 4 + SixBitArray(Counter + 1) \ 16
            ResultString = ResultString & Chr(FirstByte)
    End Select
    Decode = ResultString
End Function
Public Function Encode(s As String) As String
    Dim InputLength As Integer, FirstByte As Byte, SecndByte As Byte, ThirdByte As Byte, r As Byte
    Dim LineNumber As Integer, z As Integer, q() As String, ResultString As String
    Dim FullLines As Integer, LastLineLength As Integer, Counter As Integer
    q = Split("A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,a,b,c,d,e,f,g,h,i,j," & _
        "k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,0,1,2,3,4,5,6,7,8,9,+,/", ",", -1, vbTextCompare)
    InputLength = Len(s)
    r = InputLength Mod 3
    FullLines = ((InputLength - r) / 3) \ 20 + 1: LastLineLength = (InputLength - r) / 3 Mod 20 - 1
    Counter = 1
    For LineNumber = 1 To FullLines
        For z = 0 To IIf(LineNumber < FullLines, 19, LastLineLength) 'loop over the byte triplets
            FirstByte = Asc(Mid(s, Counter, 1))
            SecndByte = Asc(Mid(s, Counter + 1, 1))
            ThirdByte = Asc(Mid(s, Counter + 2, 1))
            Counter = Counter + 3
            ResultString = ResultString & q(FirstByte \ 4) & q((FirstByte Mod 4) * 16 + _
                (SecndByte \ 16)) & q((SecndByte Mod 16) * 4 + (ThirdByte \ 64)) & q(ThirdByte Mod 64)
        Next z
        If LineNumber < FullLines Then ResultString = ResultString & vbCrLf
    Next LineNumber
    Select Case r
        Case 2
            FirstByte = Asc(Mid(s, Counter, 1))
            SecndByte = Asc(Mid(s, Counter + 1, 1))
            ResultString = ResultString & q(FirstByte \ 4) & q((FirstByte Mod 4) * 16 + _
                (SecndByte \ 16)) & q((SecndByte Mod 16) * 4) & "="
        Case 1
            FirstByte = Asc(Mid(s, Counter, 1))
            ResultString = ResultString & q(FirstByte \ 4) & q((FirstByte Mod 4) * 16) & "=="
    End Select
    Encode = ResultString
End Function
Private Function ReadWebFile(ByVal vWebFile As String) As String
    'adapted from https://www.ozgrid.com/forum/forum/help-forums/excel-general/86714-vba-read-text-file-from-a-url
    Dim oXMLHTTP As Object, i As Long, vFF As Long, oResp() As Byte
    Set oXMLHTTP = CreateObject("MSXML2.XMLHTTP")
    oXMLHTTP.Open "GET", vWebFile, False
    oXMLHTTP.send
    Do While oXMLHTTP.readyState <> 4: DoEvents: Loop
    oResp = oXMLHTTP.responseBody 'Returns the results as a byte array
    ReadWebFile = StrConv(oResp, vbUnicode)
    Set oXMLHTTP = Nothing
End Function
Public Sub Task()
    Dim In_ As String, Out As String, bIn As String
    Dim filelength As Integer
    Dim i As Integer
    In_ = ReadWebFile("http://rosettacode.org/favicon.ico")
    Out = Encode(In_)
    bIn = Decode(Out)
    Debug.Print "The first eighty and last eighty characters after encoding:"
    Debug.Print Left(Out, 82) & "..." & vbCrLf & Join(Split(Right(Out, 82), vbCrLf), "")
    Debug.Print "Result of string comparison of input and decoded output: " & StrComp(In_, bIn, vbBinaryCompare)
    Debug.Print "A zero indicates both strings are equal."
End Sub
```

{{out}}
```txt
The first eighty and last eighty characters after encoding:
AAABAAIAEBAAAAAAAABoBQAAJgAAACAgAAAAAAAAqAgAAI4FAAAoAAAAEAAAACAAAAABAAgAAAAAAEAB
...
AAABAAAAAQAAAAEAAAABAAAAAQAAAAEAAAABAAAAAQAAAAEAAAABAAAAAQAAAAEAAAABAAAAAQAAAAE=
Result of string comparison of input and decoded output: 0.
A zero indicates both strings are equal.
```



## zkl

Using shared libraries for cURL and message hashing:

```zkl
var [const] MsgHash=Import("zklMsgHash"), Curl=Import("zklCurl");
 
icon:=Curl().get("http://rosettacode.org/favicon.ico"); //-->(Data(4,331),693,0)
icon=icon[0][icon[1],*];	// remove header
b64:=MsgHash.base64encode(icon);
println("Is the Rosetta Code icon the same (byte for byte) encoded then decoded: ",
   icon==MsgHash.base64decode(b64));
b64.println();
b64.text.println();
```

{{out}}
Encoded to 72 characters per line

```txt

Is the Rosetta Code icon the same (byte for byte) encoded then decoded: True
Data(4,920)
AAABAAIAEBAAAAAAAABoBQAAJgAAACAgAAAAAAAAqAgAAI4FAAAoAAAAEAAAACAAAAABAAgA
AAAAAEABAAAAAAAAAAAAAAAAAAAAAAAAAAAAAP///wCGiYcARkhHAL/CwAAmKScAam1rAOPm
...
AAEAAAABAAAAAQAAAAEAAAABAAAAAQAAAAEAAAABAAAAAQAAAAEAAAABAAAAAQAAAAEAAAAB
AAAAAQAAAAEAAAABAAAAAQAAAAE=

```

