+++
title = "Read a file character by character/UTF8"
description = ""
date = 2019-01-06T21:39:58Z
aliases = []
[extra]
id = 13123
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "autohotkey",
  "c",
  "common_lisp",
  "csharp",
  "factor",
  "funl",
  "go",
  "j",
  "java",
  "jq",
  "julia",
  "kotlin",
  "lua",
  "m2000_interpreter",
  "netrexx",
  "perl",
  "perl_6",
  "phix",
  "picolisp",
  "python",
  "racket",
  "related_tasks",
  "rexx",
  "ring",
  "ruby",
  "run_basic",
  "seed7",
  "sidef",
  "tcl",
  "zkl",
]
+++

## Task

Read a file one character at a time, as opposed to [[Read entire file|reading the entire file at once]].

The solution may be implemented as a procedure, which returns the next character in the file on each consecutive call (returning EOF when the end of the file is reached).

The procedure should support the reading of files containing UTF8 encoded wide characters, returning whole characters for each consecutive read.

## Related tasks

*   [[Read a file line by line]]





## AutoHotkey

```AutoHotkey
File := FileOpen("input.txt", "r")
while !File.AtEOF
	MsgBox, % File.Read(1)
```



## C


```c
#include <stdio.h>
#include <wchar.h>
#include <stdlib.h>
#include <locale.h>

int main(void)
{
    /* If your native locale doesn't use UTF-8 encoding
     * you need to replace the empty string with a
     * locale like "en_US.utf8"
     */
    char *locale = setlocale(LC_ALL, "");
    FILE *in = fopen("input.txt", "r");

    wint_t c;
    while ((c = fgetwc(in)) != WEOF)
        putwchar(c);
    fclose(in);

    return EXIT_SUCCESS;
}
```



## Common Lisp

```lisp
;; CLISP puts the external formats into a separate package
#+clisp (import 'charset:utf-8 'keyword)

(with-open-file (s "input.txt" :external-format :utf-8)
  (loop for c = (read-char s nil)
        while c
        do (format t "~a" c)))
```


## C#

```c#
using System;
using System.IO;
using System.Text;

namespace RosettaFileByChar
{
    class Program
    {
        static char GetNextCharacter(StreamReader streamReader) => (char)streamReader.Read();

        static void Main(string[] args)
        {
            Console.OutputEncoding = Encoding.UTF8;
            char c;
            using (FileStream fs = File.OpenRead("input.txt"))
            {
                using (StreamReader streamReader = new StreamReader(fs, Encoding.UTF8))
                {
                    while (!streamReader.EndOfStream)
                    {
                        c = GetNextCharacter(streamReader);
                        Console.Write(c);
                    }
                }
            }
        }
    }
}
```


=={{header|Déjà Vu}}==


```dejavu
#helper function that deals with non-ASCII code points
local (read-utf8-char) file tmp:
	!read-byte file
	if = :eof dup:
		drop
		raise :unicode-error
	resize-blob tmp ++ dup len tmp
	set-to tmp
	try:
		return !decode!utf-8 tmp
	catch unicode-error:
		if < 3 len tmp:
			raise :unicode-error
		(read-utf8-char) file tmp

#reader function
read-utf8-char file:
	!read-byte file
	if = :eof dup:
		return
	local :tmp make-blob 1
	set-to tmp 0
	try:
		return !decode!utf-8 tmp
	catch unicode-error:
		(read-utf8-char) file tmp

#if the module is used as a script, read from the file "input.txt",
#showing each code point separately
if = (name) :(main):
	local :file !open :read "input.txt"

	while true:
		read-utf8-char file
		if = :eof dup:
			drop
			!close file
			return
		!.
```



## Factor

<lang>USING: kernel io io.encodings.utf8 io.files strings ;
IN: rosetta-code.read-one

"input.txt" utf8 [
    [ read1 dup ] [ 1string write ] while drop
] with-file-reader
```



## FunL


```funl
import io.{InputStreamReader, FileInputStream}

r = InputStreamReader( FileInputStream('input.txt'), 'UTF-8' )

while (ch = r.read()) != -1
  print( chr(ch) )

r.close()
```



## Go


```go
package main

import (
	"bufio"
	"fmt"
	"io"
	"os"
)

func Runer(r io.RuneReader) func() (rune, error) {
	return func() (r rune, err error) {
		r, _, err = r.ReadRune()
		return
	}
}

func main() {
	runes := Runer(bufio.NewReader(os.Stdin))
	for r, err := runes(); err != nil; r,err = runes() {
		fmt.Printf("%c", r)
	}
}
```



## J


Reading a file a character at a time is antithetical not only to the architecture of J, but to the architecture and design of most computers and most file systems. Nevertheless, this can be a useful concept if you're building your own hardware. So let's model it...

First, we know that the first 8-bit value in a utf-8 sequence tells us the length of the sequence needed to represent that character. Specifically: we can convert that value to binary, and count the number of leading 1s to find the length of the character (except the length is always at least 1 character long).


```J>u8len=: 1
. 0 i.~ (8#2)#:a.&i.
```


So now, we can use indexed file read to read a utf-8 character starting at a specific file index. What we do is read the first octet and then read as many additional characters as we need based on whatever we started with. If that's not possible, we will return EOF:


```J
indexedread1u8=:4 :0
  try.
    octet0=. 1!:11 y;x,1
    octet0,1!:11 y;(x+1),<:u8len octet0
  catch.
    'EOF'
  end.
)
```


The length of the result tells us what to add to the file index to find the next available file index for reading.

Of course, this is massively inefficient. So if someone ever asks you to do this, make sure you ask them "Why?" Because the answer to that question is going to be important (and might suggest a completely different implementation).

Note also that it would make more sense to return an empty string, instead of the string 'EOF', when we reach the end of the file. But that is out of scope for this task.


## Java

```java
import java.io.*;

public class RUTF8CharacterReader {
  private String slurped;
  private String encoding;
  private String fName;
  private File fFile;
  // ---------------------------------------------------------------------------
  public String slurpChars(String fileName) {
    StringBuilder slrp = new StringBuilder();
    fName = fileName;
    fFile = new File(fName);
    try (Reader fr = new FileReader(fFile)) {
      encoding = ((InputStreamReader) fr).getEncoding();
      forever: for (;;) {
        int ic;
        if ((ic = fr.read()) < 0) { break forever; }
        char cc = (char) ic;
        slrp.append(cc);
      }
    }
    catch (FileNotFoundException ex) {
      ex.printStackTrace();
    }
    catch (IOException ex) {
      ex.printStackTrace();
    }
    slurped = slrp.length() > 0 ? slrp.toString() : null;
    return slurped;
  }
  // ---------------------------------------------------------------------------
  public void encodingDetails() {
    String FMT_000 = "file_encoding=\"%s\" file_name=\"%s\"%n";
    String FMT_001 = "unicode_string_length=\"%d\" code_point_count=\"%d\" string=\"%s\"%n";
    String FMT_002 = "codepoint_index=\"%03d\" character_count=\"%d\" unicode_id=\"U+%05X\" hex=\"%#08x\" dec=\"%07d\" oct=\"%07o\" string=\"%s\" utf-16=\"%s\" utf-8=\"%s\" character_name=\"%s\"%n";
    String str = slurped;
    System.out.printf(FMT_000, encoding, fFile.getAbsoluteFile());
    System.out.printf(FMT_001, str.length(), Character.codePointCount(str, 0, str.length()), str);
    for (int ix = 0; ix < str.length(); ++ix) {
      int cp = Character.codePointAt(str, ix);
      int cc = Character.charCount(cp);
      String cpName = Character.getName(cp);
      String x_utf16;
      String x_utf8;
      x_utf16 = "";
      x_utf8 = "";
      try {
        x_utf16 = codePointToUTF16(cp);
        x_utf8 = codePointToUTF8(cp);
      }
      catch (UnsupportedEncodingException ex) {
        ex.printStackTrace();
      }
      System.out.printf(FMT_002, ix, cc, cp, cp, ((long) cp & 0x00000000ffffffff), cp, new String(Character.toChars(cp)), x_utf16, x_utf8, cpName);
      if (cc > 1) {
        int[] surrogates = { (int) Character.highSurrogate(cp), (int) Character.lowSurrogate(cp), };
        int ixx = ix++;
        for (int sp : surrogates) {
          String spName = Character.getName(sp);
          x_utf16 = "";
          x_utf8 = "";
          try {
            x_utf16 = codePointToUTF16(sp);
            x_utf8 = codePointToUTF8(sp);
          }
          catch (UnsupportedEncodingException ex) {
            ex.printStackTrace();
          }
          int sc = Character.charCount(sp);
          System.out.printf(FMT_002, ixx++, sc, sp, sp, ((long) sp & 0x00000000ffffffff), sp, new String(Character.toChars(sp)), x_utf16, x_utf8, spName);
        }
      }
    }
    return;
  }
  // ---------------------------------------------------------------------------
  public static String codePointToUTF8(int cp) throws UnsupportedEncodingException {
    String scp = new String(Character.toChars(cp));
    boolean comma = false;
    StringBuilder xparts = new StringBuilder();
    byte[] b_utf8 = scp.getBytes("UTF-8");
    for (int xx = 0; xx < b_utf8.length; ++xx) {
      if (comma) { xparts.append(','); }
      xparts.append(String.format("%02x", b_utf8[xx]));
      comma = true;
    }
    return xparts.toString();
  }
  // ---------------------------------------------------------------------------
  public static String codePointToUTF16(int cp) throws UnsupportedEncodingException {
    String scp = new String(Character.toChars(cp));
    StringBuilder xparts = new StringBuilder();
    byte[] b_utf16 = scp.getBytes("UTF-16BE");
    boolean comma = false;
    for (int xx = 0; xx < b_utf16.length; xx += 2) {
      if (comma) { xparts.append(','); }
      xparts.append(String.format("%02x%02x", b_utf16[xx], b_utf16[xx + 1]));
      comma = true;
    }
    return xparts.toString();
  }
  // ---------------------------------------------------------------------------
  public static void main(String[] args) {
    String inFile;
    if (args.length > 0 && args[0].length() > 0) { inFile = args[0]; }
    else { inFile = "./data/utf8-001.txt"; }
    RUTF8CharacterReader lcl = new RUTF8CharacterReader();
    lcl.slurpChars(inFile);
    lcl.encodingDetails();
    return;
  }
}

```

```txt

file_encoding="UTF8" file_name="/Users/RosettaCode/java/./data/utf8-001.txt"
unicode_string_length="10" code_point_count="8" string="yä®€𝄞𝄢12"
codepoint_index="000" character_count="1" unicode_id="U+00079" hex="0x000079" dec="0000121" oct="0000171" string="y" utf-16="0079" utf-8="79" character_name="LATIN SMALL LETTER Y"
codepoint_index="001" character_count="1" unicode_id="U+000E4" hex="0x0000e4" dec="0000228" oct="0000344" string="ä" utf-16="00e4" utf-8="c3,a4" character_name="LATIN SMALL LETTER A WITH DIAERESIS"
codepoint_index="002" character_count="1" unicode_id="U+000AE" hex="0x0000ae" dec="0000174" oct="0000256" string="®" utf-16="00ae" utf-8="c2,ae" character_name="REGISTERED SIGN"
codepoint_index="003" character_count="1" unicode_id="U+020AC" hex="0x0020ac" dec="0008364" oct="0020254" string="€" utf-16="20ac" utf-8="e2,82,ac" character_name="EURO SIGN"
codepoint_index="004" character_count="2" unicode_id="U+1D11E" hex="0x01d11e" dec="0119070" oct="0350436" string="𝄞" utf-16="d834,dd1e" utf-8="f0,9d,84,9e" character_name="MUSICAL SYMBOL G CLEF"
codepoint_index="004" character_count="1" unicode_id="U+0D834" hex="0x00d834" dec="0055348" oct="0154064" string="?" utf-16="fffd" utf-8="3f" character_name="HIGH SURROGATES D834"
codepoint_index="005" character_count="1" unicode_id="U+0DD1E" hex="0x00dd1e" dec="0056606" oct="0156436" string="?" utf-16="fffd" utf-8="3f" character_name="LOW SURROGATES DD1E"
codepoint_index="006" character_count="2" unicode_id="U+1D122" hex="0x01d122" dec="0119074" oct="0350442" string="𝄢" utf-16="d834,dd22" utf-8="f0,9d,84,a2" character_name="MUSICAL SYMBOL F CLEF"
codepoint_index="006" character_count="1" unicode_id="U+0D834" hex="0x00d834" dec="0055348" oct="0154064" string="?" utf-16="fffd" utf-8="3f" character_name="HIGH SURROGATES D834"
codepoint_index="007" character_count="1" unicode_id="U+0DD22" hex="0x00dd22" dec="0056610" oct="0156442" string="?" utf-16="fffd" utf-8="3f" character_name="LOW SURROGATES DD22"
codepoint_index="008" character_count="1" unicode_id="U+00031" hex="0x000031" dec="0000049" oct="0000061" string="1" utf-16="0031" utf-8="31" character_name="DIGIT ONE"
codepoint_index="009" character_count="1" unicode_id="U+00032" hex="0x000032" dec="0000050" oct="0000062" string="2" utf-16="0032" utf-8="32" character_name="DIGIT TWO"

```



## jq

jq being stream-oriented, it makes sense to define `readc` so that it emits a stream of the UTF-8 characters in the input:

```jq
def readc:
  inputs + "\n" | explode[] | [.] | implode;
```


Example:

```sh

    echo '过活' | jq -Rn 'include "readc"; readc'
    "过"
    "活"
    "\n"
```



## Julia


The built-in <code>read(stream, Char)</code> function reads a single UTF8-encoded character from a given stream.


```julia
open("myfilename") do f
    while !eof(f)
        c = read(f, Char)
        println(c)
    end
end
```



## Kotlin


```scala
// version 1.1.2

import java.io.File

const val EOF = -1

fun main(args: Array<String>) {
    val reader = File("input.txt").reader()  // uses UTF-8 by default
    reader.use {
        while (true) {
            val c = reader.read()
            if (c == EOF) break
            print(c.toChar()) // echo to console
        }
    }
}
```




## Lua

```Lua

-- Return whether the given string is a single ASCII character.
function is_ascii (str)
  return string.match(str, "[\0-\x7F]")
end

-- Return whether the given string is an initial byte in a multibyte sequence.
function is_init (str)
    return string.match(str, "[\xC2-\xF4]")
end

-- Return whether the given string is a continuation byte in a multibyte sequence.
function is_cont (str)
    return string.match(str, "[\x80-\xBF]")
end

-- Accept a filestream.
-- Return the next UTF8 character in the file.
function read_char (file)
    local multibyte -- build a valid multibyte Unicode character

    for c in file:lines(1) do
        if is_ascii(c) then
            if multibyte then
                -- We've finished reading a Unicode character; unread the next byte,
                -- and return the Unicode character.
                file:seek("cur", -1)
                return multibyte
            else
                return c
            end
        elseif is_init(c) then
            if multibyte then
                file:seek("cur", -1)
                return multibyte
            else
                multibyte = c
            end
        elseif is_cont(c) then
            multibyte = multibyte .. c
        else
            assert(false)
        end
    end
end

-- Test.
function read_all ()
    testfile = io.open("tmp.txt", "w")
    testfile:write("𝄞AöЖ€𝄞Ελληνικάyä®€成长汉\n")
    testfile:close()
    testfile = io.open("tmp.txt", "r")

    while true do
        local c = read_char(testfile)
        if not c then return else io.write(" ", c) end
    end
end

```

 𝄞 A ö Ж € 𝄞 Ε λ λ η ν ι κ ά y ä ® € 成 长 汉

## M2000 Interpreter

from revision 27, version 9.3, of M2000 Environment, Chinese 长 letter displayed in console (as displayed in editor)


```M2000 Interpreter

Module checkit {
      \\ prepare a file
      \\ Save.Doc and Append.Doc  to file, Load.Doc and Merge.Doc from file
      document a$
      a$={First Line
            Second line
            Third Line
            Ελληνικά Greek Letters
            yä®€
            成长汉
            }
      Save.Doc a$, "checkthis.txt", 2  ' 2 for UTF-8
      b$="*"
      final$=""
      buffer Clear bytes as byte*16
      Buffer One as byte
      Buffer Two as byte*2
      Buffer Three as byte*3
      Locale 1033
      open "checkthis.txt" for input as #f
      seek#f, 4 ' skip BOM
      While b$<>"" {
            GetOneUtf8Char(&b$)
            final$+=b$
      }
      close #f
      Report final$
      Sub GetOneUtf8Char(&ch$)
            ch$=""
            if Eof(#f) then Exit Sub
            Get #f, One
            Return Bytes, 0:=Eval(one, 0)
            local mrk=Eval(one, 0)
           Try ok {
                  If Binary.And(mrk, 0xE0)=0xC0 then {
                        Get #f,one
                        Return Bytes, 1:=Eval$(one, 0,1)
                        ch$=Eval$(Bytes, 0, 2)
                  } Else.if Binary.And(mrk, 0xF0)=0xE0 then {
                        Get #f,two
                        Return Bytes, 1:=Eval$(two,0,2)
                        ch$=Eval$(Bytes, 0, 3)
                  } Else.if Binary.And(mrk, 0xF8)=0xF0 then {
                        Get #f,three
                        Return Bytes, 1:=Eval$(three, 0, 3)
                        ch$=Eval$(Bytes, 0, 4)
                  } Else ch$=Eval$(Bytes, 0, 1)
            }
            if Error or not ok then ch$="" : exit sub
            ch$=left$(string$(ch$ as Utf8dec),1)
      End Sub
}
checkit

```


using document as final$


```M2000 Interpreter

Module checkit {
      \\ prepare a file
      \\ Save.Doc and Append.Doc  to file, Load.Doc and Merge.Doc from file
      document a$
      a$={First Line
            Second line
            Third Line
            Ελληνικά Greek Letters
            yä®€
            成长汉
            }
      Save.Doc a$, "checkthis.txt", 2  ' 2 for UTF-8
      b$="*"
      document final$
      buffer Clear bytes as byte*16
      Buffer One as byte
      Buffer Two as byte*2
      Buffer Three as byte*3
      Locale 1033
      open "checkthis.txt" for input as #f
      seek#f, 4 ' skip BOM
      oldb$=""
      While b$<>"" {
            GetOneUtf8Char(&b$)
            \\ if final$ is document then 10 and 13 if comes alone are new line
            \\ so we need to throw 10 after the 13, so we have to use oldb$
            if b$=chr$(10)  then if oldb$=chr$(13)  then  oldb$="": continue
            oldb$=b$
            final$=b$  ' we use = for append to document
      }
      close #f
      Report final$
      Sub GetOneUtf8Char(&ch$)
            ch$=""
            if Eof(#f) then Exit Sub
            Get #f, One
            Return Bytes, 0:=Eval(one, 0)
            local mrk=Eval(one, 0)
           Try ok {
                  If Binary.And(mrk, 0xE0)=0xC0 then {
                        Get #f,one
                        Return Bytes, 1:=Eval$(one, 0,1)
                        ch$=Eval$(Bytes, 0, 2)
                  } Else.if Binary.And(mrk, 0xF0)=0xE0 then {
                        Get #f,two
                        Return Bytes, 1:=Eval$(two,0,2)
                        ch$=Eval$(Bytes, 0, 3)
                  } Else.if Binary.And(mrk, 0xF8)=0xF0 then {
                        Get #f,three
                        Return Bytes, 1:=Eval$(three, 0, 3)
                        ch$=Eval$(Bytes, 0, 4)
                  } Else ch$=Eval$(Bytes, 0, 1)
            }
            if Error or not ok then ch$="" : exit sub
            ch$=left$(string$(ch$ as Utf8dec),1)
      End Sub
}
checkit


```



## NetRexx

[[Java]] and by extension [[NetRexx]] provides I/O functions that read UTF-8 encoded character data directly from an attached input stream.
The <tt>Reader.read()</tt> method reads a single character as an integer value in the range 0 &ndash; 65535 [0x00 &ndash; 0xffff], reading from a file encoded in UTF-8 will read each codepoint into an <tt>int</tt>.
In the sample below the <tt>readCharacters</tt> method reads the file character by character into a <tt>String</tt> and returns the result to the caller.  The rest of this sample examines the result and formats the details.

:The file <tt>data/utf8-001.txt</tt> it a UTF-8 encoded text file containing the following: &#x79;&#xE4;&#xAE;&#x20AC;&#x1D11E;&#x1D122;&#x31;&#x32;.

```NetRexx
/* NetRexx */
options replace format comments java crossref symbols nobinary
numeric digits 20

runSample(arg)
return

-- ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
method readCharacters(fName) public static binary returns String
  slurped = String('')
  slrp = StringBuilder()
  fr = Reader null
  fFile = File(fName)
  EOF = int -1 -- End Of File indicator
  do
    fr = BufferedReader(FileReader(fFile))
    ic = int
    cc = char
    -- read the contents of the file one character at a time
    loop label rdr forever
      -- Reader.read reads a single character as an integer value in the range 0 - 65535 [0x00 - 0xffff]
      -- or -1 on end of stream i.e. End Of File
      ic = fr.read()
      if ic == EOF then leave rdr
      cc = Rexx(ic).d2c
      slrp.append(cc)
      end rdr
    -- load the results of the read into a variable
    slurped = slrp.toString()
  catch fex = FileNotFoundException
    fex.printStackTrace()
  catch iex = IOException
    iex.printStackTrace()
  finally
    if fr \= null then do
      fr.close()
      catch iex = IOException
        iex.printStackTrace()
      end
  end
  return slurped

-- ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
method encodingDetails(str = String) public static
  stlen = str.length()
  cplen = Character.codePointCount(str, 0, stlen)
  say 'Unicode: length="'stlen'" code_point_count="'cplen'" string="'str'"'
  loop ix = 0 to stlen - 1
    cp = Rexx(Character.codePointAt(str, ix))
    cc = Rexx(Character.charCount(cp))
    say '  'formatCodePoint(ix, cc, cp)
    if cc > 1 then do
      surrogates = [Rexx(Character.highSurrogate(cp)).c2d(), Rexx(Character.lowSurrogate(cp)).c2d()]
      loop sx = 0 to cc - 1
        ix = ix + sx
        cp = surrogates[sx]
        say '  'formatCodePoint(ix, 1, cp)
        end sx
      end
    end ix
  say
  return

-- ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
-- @see http://docs.oracle.com/javase/6/docs/technotes/guides/intl/encoding.doc.html
-- @since Java 1.7
method formatCodePoint(ix, cc, cp) private static
  scp = Rexx(Character.toChars(cp))
  icp = cp.d2x(8).x2d(9) -- signed to unsigned conversion
  ocp = Rexx(Integer.toOctalString(icp))
  x_utf16 = ''
  x_utf8  = ''
  do
    b_utf16 = String(scp).getBytes('UTF-16BE')
    b_utf8  = String(scp).getBytes('UTF-8')
    loop bv = 0 to b_utf16.length - 1 by 2
      x_utf16 = x_utf16 Rexx(b_utf16[bv]).d2x(2) || Rexx(b_utf16[bv + 1]).d2x(2)
      end bv
    loop bv = 0 to b_utf8.length - 1
      x_utf8 = x_utf8 Rexx(b_utf8[bv]).d2x(2)
      end bv
    x_utf16 = x_utf16.space(1, ',')
    x_utf8  = x_utf8.space(1, ',')
  catch ex = UnsupportedEncodingException
    ex.printStackTrace()
  end
  cpName = Character.getName(cp)
  fmt =                        -
    'CodePoint:'               -
    'index="'ix.right(3, 0)'"' -
    'character_count="'cc'"'   -
    'id="U+'cp.d2x(5)'"'       -
    'hex="0x'cp.d2x(6)'"'      -
    'dec="'icp.right(7, 0)'"'  -
    'oct="'ocp.right(7, 0)'"'  -
    'char="'scp'"'             -
    'utf-16="'x_utf16'"'       -
    'utf-8="'x_utf8'"'         -
    'name="'cpName'"'
  return fmt

-- ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
method runSample(arg) public static
  parse arg fileNames
  if fileNames = '' then fileNames = 'data/utf8-001.txt'
  loop while fileNames \= ''
    parse fileNames fileName fileNames
    slurped = readCharacters(fileName)
    say "Input:" slurped
    encodingDetails(slurped)
    end
  say
  return

```

```txt

Input: yä®€𝄞𝄢12
Unicode: length="10" code_point_count="8" string="yä®€𝄞𝄢12"
  CodePoint: index="000" character_count="1" id="U+00079" hex="0x000079" dec="0000121" oct="0000171" char="y" utf-16="0079" utf-8="79" name="LATIN SMALL LETTER Y"
  CodePoint: index="001" character_count="1" id="U+000E4" hex="0x0000E4" dec="0000228" oct="0000344" char="ä" utf-16="00E4" utf-8="C3,A4" name="LATIN SMALL LETTER A WITH DIAERESIS"
  CodePoint: index="002" character_count="1" id="U+000AE" hex="0x0000AE" dec="0000174" oct="0000256" char="®" utf-16="00AE" utf-8="C2,AE" name="REGISTERED SIGN"
  CodePoint: index="003" character_count="1" id="U+020AC" hex="0x0020AC" dec="0008364" oct="0020254" char="€" utf-16="20AC" utf-8="E2,82,AC" name="EURO SIGN"
  CodePoint: index="004" character_count="2" id="U+1D11E" hex="0x01D11E" dec="0119070" oct="0350436" char="𝄞" utf-16="D834,DD1E" utf-8="F0,9D,84,9E" name="MUSICAL SYMBOL G CLEF"
  CodePoint: index="004" character_count="1" id="U+0D834" hex="0x00D834" dec="0055348" oct="0154064" char="?" utf-16="FFFD" utf-8="3F" name="HIGH SURROGATES D834"
  CodePoint: index="005" character_count="1" id="U+0DD1E" hex="0x00DD1E" dec="0056606" oct="0156436" char="?" utf-16="FFFD" utf-8="3F" name="LOW SURROGATES DD1E"
  CodePoint: index="006" character_count="2" id="U+1D122" hex="0x01D122" dec="0119074" oct="0350442" char="𝄢" utf-16="D834,DD22" utf-8="F0,9D,84,A2" name="MUSICAL SYMBOL F CLEF"
  CodePoint: index="006" character_count="1" id="U+0D834" hex="0x00D834" dec="0055348" oct="0154064" char="?" utf-16="FFFD" utf-8="3F" name="HIGH SURROGATES D834"
  CodePoint: index="007" character_count="1" id="U+0DD22" hex="0x00DD22" dec="0056610" oct="0156442" char="?" utf-16="FFFD" utf-8="3F" name="LOW SURROGATES DD22"
  CodePoint: index="008" character_count="1" id="U+00031" hex="0x000031" dec="0000049" oct="0000061" char="1" utf-16="0031" utf-8="31" name="DIGIT ONE"
  CodePoint: index="009" character_count="1" id="U+00032" hex="0x000032" dec="0000050" oct="0000062" char="2" utf-16="0032" utf-8="32" name="DIGIT TWO"

```



## Perl


```perl
binmode STDOUT, ':utf8';  # so we can print wide chars without warning

open my $fh, "<:encoding(UTF-8)", "input.txt" or die "$!\n";

while (read $fh, my $char, 1) {
    printf "got character $char [U+%04x]\n", ord $char;
}

close $fh;
```


If the contents of the ''input.txt'' file are <code>aă€⼥</code> then the output would be:

```txt

got character a [U+0061]
got character ă [U+0103]
got character € [U+20ac]
got character ⼥ [U+2f25]

```



## Perl 6

Perl 6 has a built in method .getc to get a single character from an open file handle. File handles default to UTF-8, so they will handle multi-byte characters correctly.

To read a single character at a time from the Standard Input terminal; $*IN in Perl 6:

```perl6
.say while defined $_ = $*IN.getc;
```


Or, from a file:

```perl6
my $filename = 'whatever';

my $in = open( $filename, :r ) orelse .die;

print $_ while defined $_ = $in.getc;
```



## Phix

Generally I use utf8_to_utf32() on whole lines when I want to do character-counting.

You can find that routine in builtins/utfconv.e, and here is a modified copy that reads
precisely one unicode character from a file. If there is a genuine demand for it, I
could easily add this to that file permanently, and document/autoinclude it properly.

```Phix
constant INVALID_UTF8 = #FFFD

function get_one_utf8_char(integer fn)
-- returns INVALID_UTF8 on error, else a string of 1..4 bytes representing one character
object res
integer headb, bytes, c

    -- headb = first byte of utf-8 character:
    headb = getc(fn)
    if headb=-1 then return -1 end if
    res = ""&headb

    -- calculate length of utf-8 character in bytes (1..4):
    if    headb<0           then bytes = 0  -- (utf-8 starts at #0)
    elsif headb<=0b01111111 then bytes = 1  -- 0b_0xxx_xxxx
    elsif headb<=0b10111111 then bytes = 0  -- (it's a tail byte)
    elsif headb<=0b11011111 then bytes = 2  -- 0b_110x_xxxx
    elsif headb<=0b11101111 then bytes = 3  -- 0b_1110_xxxx
    elsif headb<=0b11110100 then bytes = 4  -- 0b_1111_0xzz
    else                         bytes = 0  -- (utf-8 ends at #10FFFF)
    end if

    -- 2..4 bytes encoding (tail range: 0b_1000_0000..0b_1011_1111);
    for j=1 to bytes-1 do                   -- tail bytes are valid?
        c = getc(fn)
        if c<#80 or c>#BF then
            bytes = 0                       -- invalid tail byte or eof
            exit
        end if
        res &= c
    end for

    -- 1 byte encoding (head range: 0b_0000_0000..0b_0111_1111):
    if bytes=1 then
        c = headb                               -- UTF-8 = ASCII

    -- 2 bytes encoding (head range: 0b_1100_0000..0b_1101_1111):
    elsif bytes=2 then
        c = and_bits(headb, #1F)*#40 +          -- 0b110[7..11] headb
            and_bits(res[2], #3F)               -- 0b10[1..6] tail
        if c>#7FF then ?9/0 end if              -- sanity check
        if c<#80 then                           -- long form?
            res = INVALID_UTF8
        end if

    -- 3 bytes encoding (head range: 0b_1110_0000..0b_1110_1111):
    elsif bytes=3 then
        c = and_bits(headb, #0F)*#1000 +        -- 0b1110[13..16] head
            and_bits(res[2], #3F)*#40 +         -- 0b10[7..12] tail
            and_bits(res[3], #3F)               -- 0b10[1..6] tail
        if c>#FFFF then ?9/0 end if             -- sanity check
        if c<#800                               -- long form?
        or (c>=#D800 and c<=#DFFF) then         -- utf-16 incompatible
            res = INVALID_UTF8
        end if

    -- 4 bytes encoding (head range: 0b_1111_0000..0b_1111_0111):
    elsif bytes=4 then
        c = and_bits(headb, #07)*#040000 +      -- 0b11110[19..21] head
            and_bits(res[2], #3F)*#1000 +       -- 0b10[13..18] tail
            and_bits(res[3], #3F)*#0040 +       -- 0b10[7..12] tail
            and_bits(res[4], #3F)               -- 0b10[1..6] tail
        if c<#10000                             -- long form?
        or c>#10FFFF then
            res = INVALID_UTF8                  -- utf-8 ends at #10FFFF
        end if

    -- bytes = 0; current byte is not encoded correctly:
    else
        res = INVALID_UTF8
    end if

    return res
end function
```

Test code:

```Phix
--string utf8 = "aă€⼥"  -- (same results as next)
string utf8 = utf32_to_utf8({#0061,#0103,#20ac,#2f25})
printf(1,"length of utf8 is %d bytes\n",length(utf8))
integer fn = open("test.txt","wb")
puts(fn,utf8)
close(fn)
fn = open("test.txt","r")
for i=1 to 5 do
    object res = get_one_utf8_char(fn)
    if string(res) then
        if platform()=LINUX then
            printf(1,"char %d (%s) is %d bytes\n",{i,res,length(res)})
        else
            -- unicode and consoles tricky on windows, so I'm
            -- just avoiding that issue altogther (t)here.
            printf(1,"char %d is %d bytes\n",{i,length(res)})
        end if
    elsif res=-1 then
        printf(1,"char %d - EOF\n",i)
        exit
    else
        printf(1,"char %d - INVALID_UTF8\n",i)
        exit
    end if
end for
close(fn)
```

```txt

length of utf8 is 9 bytes
char 1 is 1 bytes
char 2 is 2 bytes
char 3 is 3 bytes
char 4 is 3 bytes
char 5 - EOF

```



## PicoLisp

Pico Lisp uses UTF-8 until told otherwise.

```PicoLisp

(in "wordlist"
  (while (char)
    (process @))

```



## Python

```python

def get_next_character(f):
  # note: assumes valid utf-8
  c = f.read(1)
  while c:
    while True:
      try:
        yield c.decode('utf-8')
      except UnicodeDecodeError:
        # we've encountered a multibyte character
        # read another byte and try again
        c += f.read(1)
      else:
        # c was a valid char, and was yielded, continue
        c = f.read(1)
        break

# Usage:
with open("input.txt","rb") as f:
    for c in get_next_character(f):
        print(c)

```


Python 3 simplifies the handling of text files since you can specify an encoding.

```python
def get_next_character(f):
    """Reads one character from the given textfile"""
    c = f.read(1)
    while c:
        yield c
        c = f.read(1)

# Usage:
with open("input.txt", encoding="utf-8") as f:
    for c in get_next_character(f):
        print(c, sep="", end="")
```



## Racket

Don't we all love self reference?

```racket

#lang racket
; This file contains utf-8 charachters: λ, α, γ ...
(for ([c (in-port read-char (open-input-file "read-file.rkt"))])
  (display c))

```

Output:

```racket

#lang racket
; This file contains utf-8 charachters: λ, α, γ ...
(for ([c (in-port read-char (open-input-file "read-file.rkt"))])
  (display c))

```


## REXX


### version 1

REXX doesn't support UTF8 encoded wide characters, just bytes.

The task's requirement stated that '''EOF''' was to be returned upon reaching the end-of-file, so this programming example was written as a subroutine (procedure).

Note that displaying of characters that may modify screen behavior such as tab usage, backspaces, line feeds, carriage returns, "bells" and others are suppressed, but their hexadecimal equivalents are displayed.

```rexx
/*REXX program  reads and displays  a file char by char, returning   'EOF'   when done. */
parse arg iFID .                                 /*iFID:     is the fileID to be read.  */
                                                 /* [↓]  show the file's contents.      */
if iFID\==''  then do j=1  until  x=='EOF'       /*J  count's the file's characters.    */
                   x=getchar(iFID);    y=        /*get a character  or  an 'EOF'.       */
                   if x>>' '   then y=x          /*display   X   if presentable.        */
                   say  right(j, 12)     'character,  (hex,char)'      c2x(x)      y
                   end   /*j*/                   /* [↑]  only display  X  if not low hex*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
getchar: procedure;  parse arg z;  if chars(z)==0  then return 'EOF';     return charin(z)
```

'''input'''   file:   '''ABC'''

and was created by the DOS command (under Windows/XP):     '''echo 123 [¬ a prime]> ABC'''

```txt

123 [¬ a prime]

```

'''output'''   (for the above  [ABC]  input file:

```txt

           1 character,  (hex,char) 31 1
           2 character,  (hex,char) 32 2
           3 character,  (hex,char) 33 3
           4 character,  (hex,char) 20
           5 character,  (hex,char) 5B [
           6 character,  (hex,char) AA ¬
           7 character,  (hex,char) 20
           8 character,  (hex,char) 61 a
           9 character,  (hex,char) 20
          10 character,  (hex,char) 70 p
          11 character,  (hex,char) 72 r
          12 character,  (hex,char) 69 i
          13 character,  (hex,char) 6D m
          14 character,  (hex,char) 65 e
          15 character,  (hex,char) 5D ]
          16 character,  (hex,char) 0D
          17 character,  (hex,char) 0A
          18 character,  (hex,char) 454F46 EOF
End-Of-File.

```



### version 2


```rexx
/* REXX ---------------------------------------------------------------
* 29.12.2013 Walter Pachl
* read one utf8 character at a time
* see http://de.wikipedia.org/wiki/UTF-8#Kodierung
*--------------------------------------------------------------------*/
oid='utf8.txt';'erase' oid /* first create file containing utf8 chars*/
Call charout oid,'79'x
Call charout oid,'C3A4'x
Call charout oid,'C2AE'x
Call charout oid,'E282AC'x
Call charout oid,'F09D849E'x
Call lineout oid
fid='utf8.txt'             /* then read it and show the contents     */
Do Until c8='EOF'
  c8=get_utf8char(fid)
  Say left(c8,4) c2x(c8)
  End
Exit

get_utf8char: Procedure
  Parse Arg f
  If chars(f)=0 Then
    Return 'EOF'
  c=charin(f)
  b=c2b(c)
  If left(b,1)=0 Then
    Nop
  Else Do
    p=pos('0',b)
    Do i=1 To p-2
      If chars(f)=0 Then Do
        Say 'illegal contents in file' f
        Leave
        End
      c=c||charin(f)
      End
    End
  Return c

c2b: Return x2b(c2x(arg(1)))
```

output:

```txt
y    79
Ã¤   C3A4
Â®   C2AE
â‚¬  E282AC
ð„ž F09D849E
EOF  454F46
```




## Ring


```ring

fp = fopen("C:\Ring\ReadMe.txt","r")
r = fgetc(fp)
while isstring(r)
      r = fgetc(fp)
      see r
end
fclose(fp)

```

Output:

```txt


### ============================================

The Ring Programming Language
http://ring-lang.net/
Version 1.0
Release Date : January 25, 2016
Update Date : March 27, 2016

### =============================================

Binary release for Microsoft Windows

### =============================================


Run Start.bat to open Ring Notepad then
start learning from the documentation

Join Ring Group for questions
https://groups.google.com/forum/#!forum/ring-lang

Greetings,
Mahmoud Fayed
msfclipper@yahoo.com
http://www.facebook.com/mahmoudfayed1986

```



## Ruby

```ruby
File.open('input.txt', 'r:utf-8') do |f|
  f.each_char{|c| p c}
end
```


or


```ruby
File.open('input.txt', 'r:utf-8') do |f|
  while c = f.getc
    p c
  end
end
```



## Run BASIC


```runbasic
open file.txt" for binary as #f
numChars = 1              ' specify number of characters to read
a$ = input$(#f,numChars)  ' read number of characters specified
b$ = input$(#f,1)         ' read one character
close #f
```



## Seed7

The library [http://seed7.sourceforge.net/libraries/utf8.htm utf8.s7i]
provides the functions [http://seed7.sourceforge.net/libraries/utf8.htm#openUtf8%28in_string,in_string%29 openUtf8]
and [http://seed7.sourceforge.net/libraries/utf8.htm#getc%28in_utf8_file%29 getc].
When a file has been opened with <code>openUtf8</code> fhe function <code>getc</code> reads UTF-8 characters from the file.
To allow writing Unicode characters to standard output
the file [http://seed7.sourceforge.net/libraries/utf8.htm#STD_UTF8_OUT STD_UTF8_OUT] is used.


```seed7
$ include "seed7_05.s7i";
  include "utf8.s7i";

const proc: main is func
  local
    var file: inFile is STD_NULL;
    var char: ch is ' ';
  begin
    OUT := STD_UTF8_OUT;
    inFile := openUtf8("readAFileCharacterByCharacterUtf8.in", "r");
    if inFile <> STD_NULL then
      while hasNext(inFile) do
        ch := getc(inFile);
        writeln("got character " <& ch <& " [U+" <& ord(ch) radix 16 <& "]");
      end while;
      close(inFile);
    end if;
  end func;
```


When the input file <tt>readAFileCharacterByCharacterUtf8.in</tt> contains the characters <tt>aă€⼥</tt> the output is:

```txt

got character a [U+61]
got character ă [U+103]
got character € [U+20ac]
got character ⼥ [U+2f25]

```



## Sidef


```ruby
var file = File('input.txt')        # the input file contains: "aă€⼥"
var fh = file.open_r                # equivalent with: file.open('<:utf8')
fh.each_char { |char|
    printf("got character #{char} [U+%04x]\n", char.ord)
}
```

```txt

got character a [U+0061]
got character ă [U+0103]
got character € [U+20ac]
got character ⼥ [U+2f25]

```



## Tcl

To read a single character from a file, use:

```tcl
set ch [read $channel 1]
```

This will read multiple bytes sufficient to obtain a Unicode character if a suitable encoding has been configured on the channel. For binary channels, this will always consume exactly one byte. However, the low-level channel buffering logic may consume more than one byte (which only really matters where the channel is being handed on to another process and the channel is over a file descriptor that doesn't support the <tt>lseek</tt> OS call); the extent of buffering can be controlled via:

```tcl
fconfigure $channel -buffersize $byteCount
```

When the channel is only being accessed from Tcl (or via Tcl's C API) it is not normally necessary to adjust this option.


## zkl

zkl doesn't know much about UTF-8 or Unicode but is able to test whether a string or number is valid UTF-8 or not. This code uses that to build a state machine to decode a byte stream into UTF-8 characters.

```zkl
fcn readUTF8c(chr,s=""){ // transform UTF-8 character stream
   s+=chr;
   try{ s.len(8); return(s) }
   catch{ if(s.len()>6) throw(__exception) } // 6 bytes max for UTF-8
   return(Void.Again,s);  // call me again with s & another character
}
```

Used to modify a zkl iterator, it can consume any stream-able (files, strings, lists, etc) and provides support for foreach, map, look ahead, push back, etc.

```zkl
fcn utf8Walker(obj){
   obj.walker(3)  // read characters
   .tweak(readUTF8c)
}
```


```zkl
s:="-->\u20AC123";  // --> e2,82,ac,31,32,33 == -->€123
utf8Walker(s).walk().println();

w:=utf8Walker(Data(Void,s,"\n")); // Data is a byte bucket
foreach c in (utf8Walker(Data(Void,s,"\n"))){ print(c) }

utf8Walker(Data(Void,0xe2,0x82,"123456")).walk().println(); // € is short 1 byte
```

```txt

L("-","-",">","€","1","2","3")
-->€123
VM#1 caught this unhandled exception:
   ValueError : Invalid UTF-8 string

```

If you wish to push a UTF-8 stream through one or more functions, you can use the same state machine:

```zkl
stream:=Data(Void,s,"\n").howza(3); // character stream
stream.pump(List,readUTF8c,"print")
```

```txt
-->€123
```

and returns a list of the eight UTF-8 characters (with newline).
Or, if file "foo.txt" contains the characters:

```zkl
File("foo.txt","rb").howza(3).pump(List,readUTF8c,"print");
```

produces the same result.

