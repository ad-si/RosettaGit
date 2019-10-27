+++
title = "Binary strings"
description = ""
date = 2019-10-05T17:46:26Z
aliases = []
[extra]
id = 4071
[taxonomies]
categories = []
tags = []
+++

{{task|String manipulation}}
Many languages have powerful and useful ('''binary safe''') [[wp:String (computer science)|string]] [[wp:Comparison of programming languages (string functions)|manipulation functions]], while others don't, making it harder for these languages to accomplish some tasks.
This task is about creating functions to handle ''binary'' strings (strings made of arbitrary bytes, i.e. ''byte strings'' according to Wikipedia) for those languages that don't have built-in support for them. If your language of choice does have this built-in support, show a possible alternative implementation for the ''functions'' or ''abilities'' already provided by the language.
In particular the functions you need to create are:
* String creation and destruction (when needed and if there's no [[garbage collection]] or similar mechanism)
* String assignment
* String comparison
* String cloning and copying
* Check if a string is empty
* Append a byte to a string
* Extract a substring from a string
* Replace every occurrence of a byte (or a string) in a string with another string
* Join strings



Possible contexts of use: compression algorithms (like [[LZW compression]]), L-systems (manipulation of symbols), many more.





## Ada

Ada has native support for single dimensioned arrays, which provide all specified operations. String is a case of array. The array of bytes is predefined in Ada in the package System.Storage_Elements ([http://www.adaic.org/standards/05rm/html/RM-13-7-1.html LRM 13.7.1]). Storage_Element is substitute for byte.


```Ada
declare
   Data : Storage_Array (1..20); -- Data created
begin
   Data := (others => 0); -- Assign all zeros
   if Data = (1..10 => 0) then -- Compare with 10 zeros
      declare
         Copy : Storage_Array := Data; -- Copy Data
      begin
         if Data'Length = 0 then -- If empty
            ...
         end if;
      end;
   end if;
   ... Data & 1 ...         -- The result is Data with byte 1 appended
   ... Data & (1,2,3,4) ... -- The result is Data with bytes 1,2,3,4 appended
   ... Data (3..5) ...      -- The result the substring of Data from 3 to 5
end; -- Data destructed
```

Storage_Array is "binary string" used for memory representation. For stream-oriented I/O communication Ada provides alternative "binary string" called Stream_Element_Array ([http://www.adaic.org/standards/05rm/html/RM-13-13-1.html LRM 13.13.1]). When dealing with octets of bits, programmers are encouraged to provide a data type of their own to ensure that the byte is exactly 8 bits length. For example:

```Ada
type Octet is mod 2**8;
for Octet'Size use 8;
type Octet_String is array (Positive range <>) of Octet;
```

Alternatively:

```Ada
with Interfaces; use Interfaces;
...
type Octet is new Interfaces.Unsigned_8;
type Octet_String is array (Positive range <>) of Octet;
```

Note that all of these types will have all operations described above.


## ALGOL 68

{{trans|Tcl}}

{{works with|ALGOL 68|Standard - no extensions to language used}}
{{works with|ALGOL 68G|Any - tested with release mk15-0.8b.fc9.i386}}
<!-- {{does not work with|ELLA ALGOL 68|Any (with appropriate job cards AND formatted transput statements removed) - tested with release 1.8.8d.fc9.i386 - ELLA has no FORMATted transput}} -->

```algol68
# String creation #
STRING a,b,c,d,e,f,g,h,i,j,l,r;
a := "hello world";
print((a, new line));

# String destruction (for garbage collection) #
b := ();
BEGIN
  LOC STRING lb := "hello earth";  # allocate off the LOC stack  #
  HEAP STRING hb := "hello moon"; # allocate out of the HEAP space #
  ~
END; # local variable "lb" has LOC stack space recovered at END #

# String assignment #
c := "a"+REPR 0+"b";
print (("string length c:", UPB c, new line));# ==> 3 #

# String comparison #
l := "ab"; r := "CD";

BOOL result;
FORMAT summary = $""""g""" is "b("","NOT ")"lexicographically "g" """g""""l$ ;

result := l <  r OR l LT r;              printf((summary, l, result, "less than", r));
result := l <= r OR l LE r # OR l ≤ r #; printf((summary, l, result, "less than or equal to", r));
result := l  = r OR l EQ r;              printf((summary, l, result, "equal to", r));
result := l /= r OR l NE r # OR l ≠ r #; printf((summary, l, result, "not equal to", r));
result := l >= r OR l GE r # OR l ≥ r #; printf((summary, l, result, "greater than or equal to", r));
result := l >  r OR l GT r;              printf((summary, l, result, "greater than", r));

# String cloning and copying #
e := f;

# Check if a string is empty #
IF g = "" THEN print(("g is empty", new line)) FI;
IF UPB g = 0 THEN print(("g is empty", new line)) FI;

# Append a byte to a string #
h +:= "A";

# Append a string to a string #
h +:= "BCD";
h PLUSAB "EFG";

# Prepend a string to a string - because STRING addition isn't communitive #
"789" +=: h;
"456" PLUSTO h;
print(("The result of prepends and appends: ", h, new line));

# Extract a substring from a string #
i := h[2:3];
print(("Substring 2:3 of ",h," is ",i, new line));

# Replace every occurrences of a byte (or a string) in a string with another string #
PROC replace = (STRING string, old, new, INT count)STRING: (
  INT pos;
  STRING tail := string, out;
  TO count WHILE string in string(old, pos, tail) DO
    out +:= tail[:pos-1]+new;
    tail := tail[pos+UPB old:]
  OD;
  out+tail
);

j := replace("hello world", "world", "planet", max int);
print(("After replace string: ", j, new line));

INT offset = 7;
# Replace a character at an offset in the string #
j[offset] := "P";
print(("After replace 7th character: ", j, new line));

# Replace a substring at an offset in the string #
j[offset:offset+3] := "PlAN";
print(("After replace 7:10th characters: ", j, new line));

# Insert a string before an offset in the string #
j := j[:offset-1]+"INSERTED "+j[offset:];
print(("Insert string before 7th character: ", j, new line));

# Join strings  #
a := "hel";
b := "lo w";
c := "orld";
d := a+b+c;

print(("a+b+c is ",d, new line));

# Pack a string into the target CPU's word #
BYTES word := bytes pack(d);

# Extract a CHAR from a CPU word #
print(("7th byte in CPU word is: ", offset ELEM word, new line))
```

Output:

```txt

hello world
string length c:         +3
"ab" is NOT lexicographically less than "CD"
"ab" is NOT lexicographically less than or equal to "CD"
"ab" is NOT lexicographically equal to "CD"
"ab" is lexicographically not equal to "CD"
"ab" is lexicographically greater than or equal to "CD"
"ab" is lexicographically greater than "CD"
g is empty
g is empty
The result of prepends and appends: 456789ABCDEFG
Substring 2:3 of 456789ABCDEFG is 56
After replace string: hello planet
After replace 7th character: hello Planet
After replace 7:10th characters: hello PlANet
Insert string before 7th character: hello INSERTED PlANet
a+b+c is hello world
7th byte in CPU word is: w

```



## AWK


```AWK
#!/usr/bin/awk -f 

BEGIN {
	# string creation
	a="123\0 abc ";
	b="456\x09";
	c="789";
	printf("abc=<%s><%s><%s>\n",a,b,c);

	# string comparison
	printf("(a==b) is %i\n",a==b)

	# string copying
	A = a;
	B = b;
	C = c;
	printf("ABC=<%s><%s><%s>\n",A,B,C);

	# check if string is empty
	if (length(a)==0) {
		printf("string a is empty\n");
	} else {
		printf("string a is not empty\n");
	}

	# append a byte to a string 
	a=a"\x40";
	printf("abc=<%s><%s><%s>\n",a,b,c);

	# substring 
	e = substr(a,1,6);
	printf("substr(a,1,6)=<%s>\n",e);

	# join strings
	d=a""b""c;
	printf("d=<%s>\n",d);
}
```


Output:

```txt
abc=<123 abc ><456	><789>
(a==b) is 0
ABC=<123 abc ><456	><789>
string a is not empty
abc=<123 abc @><456	><789>
substr(a,1,6)=<123 a>
d=<123 abc @456	789>

```



## BASIC


=
## Applesoft BASIC
=

```ApplesoftBasic
REM STRING CREATION AND DESTRUCTION (WHEN NEEDED AND IF THERE'S NO GARBAGE COLLECTION OR SIMILAR MECHANISM)
A$ = "STRING" : REM CREATION
A$ = "" : REM DESTRUCTION
PRINT FRE(0) : REM GARBAGE COLLECTION

REM STRING ASSIGNMENT
A$ = "STRING" : R$ = "DEUX"

REM STRING COMPARISON
PRINT A$ = B$; A$ <> B$; A$ < B$; A$ > B$; A$ <= B$; A$ >= B$

REM STRING CLONING AND COPYING
B$ = A$

REM CHECK IF A STRING IS EMPTY
PRINT LEN(A$) = 0

REM APPEND A BYTE TO A STRING
A$ = A$ + CHR$(0)

REM EXTRACT A SUBSTRING FROM A STRING
S$ = MID$(A$, 2, 3)

REM REPLACE EVERY OCCURRENCE OF A BYTE (OR A STRING) IN A STRING WITH ANOTHER STRING
S = LEN(S$) : R = LEN(R$) : A = LEN(A$) : IF A > S THEN B$ = "" : FOR I = 1 TO A : F = MID$(A$, I, S) = S$ : B$ = B$ + MID$(R$, 1, R * F) + MID$(A$, I, F = 0) : NEXT I : A$ = B$ : PRINT A$

REM JOIN STRINGS
J$ = A$ + STR$(42) + " PUDDLES " + B$ + CHR$(255) : REM USE +
```


=
## ZX Spectrum Basic
=

```basic
10 REM create two strings
20 LET s$ = "Hello"
30 LET t$ = "Bob"
40 REM choose any random character
50 LET c = INT(RND*256)
60 REM add the character to the string
70 LET s$ = s$ + CHR$(c)
80 REM check if the string is empty
90 IF s$ = "" THEN PRINT "String is empty"
100 REM compare two strings
110 IF s$ = t$ THEN PRINT "Strings are the same"
120 REM print characters 2 to 4 of a string (a substring)
130 PRINT s$(2 TO 4)
```



## BBC BASIC


```bbcbasic
      A$ = CHR$(0) + CHR$(1) + CHR$(254) + CHR$(255) : REM assignment
      B$ = A$                                        : REM clone / copy
      IF A$ = B$ THEN PRINT "Strings are equal"      : REM comparison
      IF A$ = "" THEN PRINT "String is empty"        : REM Check if empty
      A$ += CHR$(128)                                : REM Append a byte
      S$ = MID$(A$, S%, L%)                          : REM Extract a substring
      C$ = A$ + B$                                   : REM Join strings
      
      REM To replace every occurrence of a byte:
      old$ = CHR$(1)
      new$ = CHR$(5)
      REPEAT
        I% = INSTR(A$, old$)
        IF I% MID$(A$, I%, 1) = new$
      UNTIL I% = 0

```



## C


```c>#include <stdio.h

#include <stdlib.h>
#include <string.h>

typedef struct str_t {
	size_t len, alloc;
	unsigned char *s;
} bstr_t, *bstr;

#define str_len(s) ((s)->len)
bstr str_new(size_t len)
{
	bstr s = malloc(sizeof(bstr_t));
	if (len < 8) len = 8;
	s->alloc = len;
	s->s = malloc(len);
	s->len = 0;
	return s;
}

void str_extend(bstr s)
{
	size_t ns = s->alloc * 2;
	if (ns - s->alloc > 1024) ns = s->alloc + 1024;
	s->s = realloc(s->s, ns);
	s->alloc = ns;
}

void str_del(bstr s)
{
	free(s->s), free(s);
}

int str_cmp(bstr l, bstr r)
{
	int res, len = l->len;
	if (len > r->len) len = r->len;

	if ((res = memcmp(l->s, r->s, len))) return res;
	return l->len > r->len ? 1 : -1;
}

bstr str_dup(bstr src)
{
	bstr x = str_new(src->len);
	memcpy(x->s, src->s, src->len);
	x->len = src->len;
	return x;
}

bstr str_from_chars(const char *t)
{
	if (!t) return str_new(0);
	size_t l = strlen(t);
	bstr x = str_new(l + 1);
	x->len = l;
	memcpy(x->s, t, l);
	return x;
}

void str_append(bstr s, unsigned char b)
{
	if (s->len >= s->alloc) str_extend(s);
	s->s[s->len++] = b;
}

bstr str_substr(bstr s, int from, int to)
{
	if (!to) to = s->len;
	if (from < 0) from += s->len;
	if (from < 0 || from >= s->len)
		return 0;
	if (to < from) to = from + 1;
	bstr x = str_new(to - from);
	x->len = to - from;
	memcpy(x->s, s->s + from, x->len);
	return x;
}

bstr str_cat(bstr s, bstr s2)
{
	while (s->alloc < s->len + s2->len) str_extend(s);
	memcpy(s->s + s->len, s2->s, s2->len);
	s->len += s2->len;
	return s;
}

void str_swap(bstr a, bstr b)
{
	size_t tz;
	unsigned char *ts;
	tz = a->alloc; a->alloc = b->alloc; b->alloc = tz;
	tz = a->len; a->len = b->len; b->len = tz;
	ts = a->s; a->s = b->s; b->s = ts;
}

bstr str_subst(bstr tgt, bstr pat, bstr repl)
{
	bstr tmp = str_new(0);
	int i;
	for (i = 0; i + pat->len <= tgt->len;) {
		if (memcmp(tgt->s + i, pat->s, pat->len)) {
			str_append(tmp, tgt->s[i]);
			i++;
		} else {
			str_cat(tmp, repl);
			i += pat->len;
			if (!pat->len) str_append(tmp, tgt->s[i++]);
		}
	}
	while (i < tgt->len) str_append(tmp, tgt->s[i++]);
	str_swap(tmp, tgt);
	str_del(tmp);
	return tgt;
}

void str_set(bstr dest, bstr src)
{
	while (dest->len < src->len) str_extend(dest);
	memcpy(dest->s, src->s, src->len);
	dest->len = src->len;
}

int main()
{
	bstr s = str_from_chars("aaaaHaaaaaFaaaaHa");
	bstr s2 = str_from_chars("___.");
	bstr s3 = str_from_chars("");

	str_subst(s, s3, s2);
	printf("%.*s\n", s->len, s->s);

	str_del(s);
	str_del(s2);
	str_del(s3);

	return 0;
}
```


=={{header|C sharp|C#}}==
{{works with|C sharp|3.0}}


```csharp
using System;

class Program
{
    static void Main()
    {
        //string creation
        var x = "hello world";

        //# mark string for garbage collection
        x = null;

        //# string assignment with a null byte
        x = "ab\0";
        Console.WriteLine(x);
        Console.WriteLine(x.Length); // 3

        //# string comparison
        if (x == "hello")
            Console.WriteLine("equal");
        else
            Console.WriteLine("not equal");

        if (x.CompareTo("bc") == -1)
            Console.WriteLine("x is lexicographically less than 'bc'");

        //# string cloning 
        var c = new char[3];
        x.CopyTo(0, c, 0, 3);
        object objecty = new string(c);
        var y = new string(c);

        Console.WriteLine(x == y);      //same as string.equals
        Console.WriteLine(x.Equals(y)); //it overrides object.Equals

        Console.WriteLine(x == objecty); //uses object.Equals, return false

        //# check if empty
        var empty = "";
        string nullString = null;
        var whitespace = "   ";
        if (nullString == null && empty == string.Empty && 
            string.IsNullOrEmpty(nullString) && string.IsNullOrEmpty(empty) &&
            string.IsNullOrWhiteSpace(nullString) && string.IsNullOrWhiteSpace(empty) &&
            string.IsNullOrWhiteSpace(whitespace))
            Console.WriteLine("Strings are null, empty or whitespace");

        //# append a byte
        x = "helloworld";
        x += (char)83;
        Console.WriteLine(x);

        //# substring
        var slice = x.Substring(5, 5);
        Console.WriteLine(slice);

        //# replace bytes
        var greeting = x.Replace("worldS", "");
        Console.WriteLine(greeting);

        //# join strings
        var join = greeting + " " + slice;
        Console.WriteLine(join);
    }
}
```



## Common Lisp

String creation (garbage collection will handle its destruction)
using the string as an atom and casting a character list to a string

```lisp

"string"
(coerce '(#\s #\t #\r #\i #\n #\g) 'string)

```


String assignment

```lisp

(defvar *string* "string")

```


comparing two string

```lisp

(equal "string" "string")

```


copy a string

```lisp

(copy-seq "string")

```



```lisp

(defun string-empty-p (string)
  (zerop (length string)))
```



```lisp

(concatenate 'string "string" "b")

```



```lisp

(subseq "string" 1 6)
"ring"

```


string replacement isn't covered by the ansi standard probably best to use (replace-all) or cl-ppcre


joining strings works in the same way as appending bytes


## Component Pascal

BlackBox Component Builder

```oberon2

MODULE NpctBinaryString;
IMPORT StdLog,Strings;

PROCEDURE Do*;
VAR
	str: ARRAY 256 OF CHAR;
	pStr,pAux: POINTER TO ARRAY OF CHAR;
	b: BYTE;
	pIni: INTEGER;
BEGIN
	(* String creation, on heap *)
	NEW(pStr,256); (* Garbage collectable *)
	NEW(pAux,256);
	
	(* String assingment *)
	pStr^ := "This is a string on a heap";
	pAux^ := "This is a string on a heap";
	str := "This is other string";
	
	(* String comparision *)
	StdLog.String("pStr = str:> ");StdLog.Bool(pStr$ = str$);StdLog.Ln;
	StdLog.String("pStr = pAux:> ");StdLog.Bool(pStr$ = pAux$);StdLog.Ln;
	
	(* String cloning and copying *)
	NEW(pAux,LEN(pStr$) + 1);pAux^ := pStr$;
	
	(* Check if a string is empty *)
	(* version 1 *)
	pAux^ := "";
	StdLog.String("is empty pAux?(1):> ");StdLog.Bool(pAux$ = "");StdLog.Ln;
	(* version 2 *)
	pAux[0] := 0X;
	StdLog.String("is empty pAux?(2):> ");StdLog.Bool(pAux$ = "");StdLog.Ln;
	(* version 3 *)
	pAux[0] := 0X;
	StdLog.String("is empty pAux?(3):> ");StdLog.Bool(pAux[0] = 0X);StdLog.Ln;
	(* version 4 *)
	pAux^ := "";
	StdLog.String("is empty pAux?(4):> ");StdLog.Bool(pAux[0] = 0X);StdLog.Ln;
	
	(* Append a byte to a string *)
	NEW(pAux,256);pAux^ := "BBBBBBBBBBBBBBBBBBBBB";
	b := 65;pAux[LEN(pAux$)] := CHR(b);
	StdLog.String("pAux:> ");StdLog.String(pAux);StdLog.Ln;
	
	(* Extract a substring from a string *)
	Strings.Extract(pStr,0,16,pAux);
	StdLog.String("pAux:> ");StdLog.String(pAux);StdLog.Ln;
	
	(* Replace a every ocurrence of a string with another string *)
	pAux^ := "a"; (* Pattern *)
	Strings.Find(pStr,pAux,0,pIni);
	WHILE pIni > 0 DO
		Strings.Replace(pStr,pIni,LEN(pAux$),"one");
		Strings.Find(pStr,pAux,pIni + 1,pIni);
	END;
	StdLog.String("pStr:> ");StdLog.String(pStr);StdLog.Ln;
	
	(* Join strings *)
	pStr^ := "First string";pAux^ := "Second String";
	str := pStr$ + "." + pAux$;
	StdLog.String("pStr + '.' + pAux:>");StdLog.String(str);StdLog.Ln
END Do;
END NpctBinaryString.

```

Execute: ^Q NpctBinaryString.Do<br/>
Output:

```txt

pStr = str:>  $FALSE
pStr = pAux:>  $TRUE
is empty pAux?(1):>  $TRUE
is empty pAux?(2):>  $TRUE
is empty pAux?(3):>  $TRUE
is empty pAux?(4):>  $TRUE
pAux:> BBBBBBBBBBBBBBBBBBBBBA
pAux:> This is a string
pStr:> This is one string on one heonep
pStr + '.' + pAux:>First string.Second String

```



## D


```d
void main() /*@safe*/ {
    import std.array: empty, replace;
    import std.string: representation, assumeUTF;

    // String creation (destruction is usually handled by
    // the garbage collector).
    ubyte[] str1;

    // String assignments.
    str1 = "blah".dup.representation;
    // Hex string, same as "\x00\xFB\xCD\x32\xFD\x0A"
    // whitespace and newlines are ignored.
    str1 = cast(ubyte[])x"00 FBCD 32FD 0A";

    // String comparison.
    ubyte[] str2;
    if (str1 == str2) {} // Strings equal.

    // String cloning and copying.
    str2 = str1.dup; // Copy entire string or array.

    // Check if a string is empty
    if (str1.empty) {} // String empty.
    if (str1.length) {} // String not empty.
    if (!str1.length) {} // String empty.

    // Append a ubyte to a string.
    str1 ~= x"0A";
    str1 ~= 'a';

    // Extract a substring from a string.
    str1 = "blork".dup.representation;
    // This takes off the first and last bytes and
    // assigns them to the new ubyte string.
    // This is just a light slice, no string data copied.
    ubyte[] substr = str1[1 .. $ - 1];

    // Replace every occurrence of a ubyte (or a string)
    // in a string with another string.
    str1 = "blah".dup.representation;
    replace(str1.assumeUTF, "la", "al");

    // Join two strings.
    ubyte[] str3 = str1 ~ str2;
}
```


=={{header|Déjà Vu}}==

Déjà Vu has a <code>blob</code> type, which is much like Python 3's <code>bytearray</code>. They are used for dealing with binary data in the standard library, and works basically like a list, except it can only have integer numbers from 0 to 255 as elements, pushing and popping is not supported, and can be resized to any size in a single step.


```dejavu
local :b make-blob 10 #ten bytes of initial size
set-to b 0 255
!. get-from b 0 #prints 255
!. b #prints (blob:ff000000000000000000)
local :b2 make-blob 3
set-to b2 0 97
set-to b2 1 98
set-to b2 2 99
!. b #prints (blob:"abc")
!. !encode!utf-8 b #prints "abc"

```



## E


(Since the task is not a specific program, the code here consists of example [[REPL]] sessions, not a whole program.)

In E, binary data is represented as ELists (implemented as arrays or ropes) of integers; a <code>String</code> is strictly a character string. ELists come in Flex (mutable) and Const (immutable) varieties.

To work with binary strings we must first have a byte type; this is a place where E shows its Java roots (to be fixed).


```e>? def int8 := <type:java.lang.Byte

# value: int8
```


<ol>
<li>There are several ways to create a FlexList; perhaps the simplest is:

```e
? def bstr := [].diverge(int8)
# value: [].diverge()

? def bstr1 := [1,2,3].diverge(int8)
# value: [1, 2, 3].diverge()

? def bstr2 := [-0x7F,0x2,0x3].diverge(int8)
# value: [-127, 2, 3].diverge()
```

As E is a memory-safe garbage-collected language there is no explicit destruction. It is good practice to work with immutable ConstLists when reasonable, however; especially when passing strings around.
</li><li>There is no specific assignment between FlexLists; a reference may be passed in the usual manner, or the contents of one could be copied to another as shown below.
</li><li>There is no comparison operation between FlexLists (since it would not be a stable ordering <!-- XXX cite? -->), but there is between ConstLists.

```e
? bstr1.snapshot() < bstr2.snapshot()
# value: false
```

</li><li>To make an independent copy of a FlexList, simply <code>.diverge()</code> it again.
</li><li>
```e
? bstr1.size().isZero()
# value: false

? bstr.size().isZero()
# value: true
```

</li><li>Appending a single element to a FlexList is done by <code>.push(<var>x</var>)</code>:

```e
? bstr.push(0)
? bstr
# value: [0].diverge()
```

</li><li>Substrings, or ''runs'', are always immutable and specified as start-end indexes (as opposed to first-last or start-count). Or, one can copy an arbitrary portion of one list into another using <code>replace(<var>target range</var>, <var>source list</var>, <var>source range</var>)</code>.

```e
? bstr1(1, 2)
# value: [2]

? bstr.replace(0, bstr.size(), bstr2, 1, 3)
? bstr
# value: [2, 3].diverge()
```

</li><li>Replacing must be written as an explicit loop; there is no built-in operation (though there is for character strings).

```e>? for i =
 byte ? (byte == 2) in bstr2 { bstr2[i] := -1 }
? bstr2
# value: [-127, -1, 3].diverge()
```

</li><li>Two lists can be concatenated into a ConstList by <code>+</code>: <code>bstr1 + bstr2</code>. <code>append</code> appends on the end of a FlexList, and <code>replace</code> can be used to insert at the beginning or anywhere inside.

```e
? bstr1.append(bstr2)
? bstr1
# value: [1, 2, 3, -127, 2, 3].diverge()
```

</li></ol>


## Elixir

Note: Elixir data types are immutable.

```elixir
# String creation
x = "hello world"

# String destruction
x = nil

# String assignment with a null byte
x = "a\0b"
IO.inspect x                  #=> <<97, 0, 98>>
IO.puts String.length(x)      #=> 3

# string comparison
if x == "hello" do
  IO.puts "equal"
else
  IO.puts "not equal"         #=> not equal
end
y = "bc"
if x < y do
  IO.puts "#{x} is lexicographically less than #{y}"  #=> a b is lexicographically less than bc
end

# string cloning 
xx = x
IO.puts x == xx               #=> true  （same length and content)

# check if empty
if x=="" do
  IO.puts "is empty"
end
if String.length(x)==0 do
  IO.puts "is empty"
end

# append a byte
IO.puts x <> "\07"            #=> a b 7
IO.inspect x <> "\07"         #=> <<97, 0, 98, 0, 55>>

# substring
IO.puts String.slice("elixir", 1..3)          #=> lix
IO.puts String.slice("elixir", 2, 3)          #=> ixi

# replace bytes
IO.puts String.replace("a,b,c", ",", "-")     #=> a-b-c

# string interpolation
a = "abc"
n = 100
IO.puts "#{a} : #{n}"         #=> abc : 100

# join strings
a = "hel"
b = "lo w"
c = "orld"
IO.puts a <> b <> c           #=> hello world
```



## Erlang


```erlang
-module(binary_string).
-compile([export_all]).

%% Erlang has very easy handling of binary strings. Using
%% binary/bitstring syntax the various task features will be
%% demonstrated.


%% Erlang has GC so destruction is not shown.
test() ->
    Binary = <<0,1,1,2,3,5,8,13>>, % binaries can be created directly
    io:format("Creation: ~p~n",[Binary]),
    Copy = binary:copy(Binary), % They can also be copied
    io:format("Copy: ~p~n",[Copy]),
    Compared = Binary =:= Copy, % They can be compared directly
    io:format("Equal: ~p = ~p ? ~p~n",[Binary,Copy,Compared]),
    Empty1 = size(Binary) =:= 0, % The empty binary would have size 0
    io:format("Empty: ~p ? ~p~n",[Binary,Empty1]),
    Empty2 = size(<<>>) =:= 0, % The empty binary would have size 0
    io:format("Empty: ~p ? ~p~n",[<<>>,Empty2]),
    Substring = binary:part(Binary,3,3),
    io:format("Substring: ~p [~b..~b] => ~p~n",[Binary,3,5,Substring]),
    Replace = binary:replace(Binary,[<<1>>],<<42>>,[global]),
    io:format("Replacement: ~p~n",[Replace]),
    Append = <<Binary/binary,21>>,
    io:format("Append: ~p~n",[Append]),
    Join = <<Binary/binary,<<21,34,55>>/binary>>,
    io:format("Join: ~p~n",[Join]).

%% Since the task also asks that we show how these can be reproduced
%% rather than just using BIFs, the following are some example
%% recursive functions reimplementing some of the above.

%% Empty string
is_empty(<<>>) ->
    true;
is_empty(_) ->
    false.

%% Replacement:
replace(Binary,Value,Replacement) ->
    replace(Binary,Value,Replacement,<<>>).

replace(<<>>,_,_,Acc) ->
    Acc;
replace(<<Value,Rest/binary>>,Value,Replacement,Acc) ->
    replace(Rest,Value,Replacement,<< Acc/binary, Replacement >>);
replace(<<Keep,Rest/binary>>,Value,Replacement,Acc) ->
    replace(Rest,Value,Replacement,<< Acc/binary, Keep >>).
```

{{out}}

```erlang>215
 binary_string:test().
Creation: <<0,1,1,2,3,5,8,13>>
Copy: <<0,1,1,2,3,5,8,13>>
Equal: <<0,1,1,2,3,5,8,13>> = <<0,1,1,2,3,5,8,13>> ? true
Empty: <<0,1,1,2,3,5,8,13>> ? false
Empty: <<>> ? true
Substring: <<0,1,1,2,3,5,8,13>> [3..5] => <<2,3,5>>
Replacement: <<0,42,42,2,3,5,8,13>>
Append: <<0,1,1,2,3,5,8,13,21>>
Join: <<0,1,1,2,3,5,8,13,21,34,55>>
```



## Factor

Factor has a <code>byte-array</code> type which works exactly like other arrays, except only bytes can be stored in it. Comparisons on <code>byte-array</code>s (like comparisons on arrays) are lexicographic.

To convert a string to a byte-array:

```factor
"Hello, byte-array!" utf8 encode .
```


```txt

B{
    72 101 108 108 111 44 32 98 121 116 101 45 97 114 114 97 121 33
}

```

Reverse:

```factor
B{ 147 250 150 123 } shift-jis decode .
```


```txt
"日本"
```



## Forth

In Forth, as in Assembler, all strings are binary ie: they are simply bytes in memory. 

Using primitive memory operations the programmer can quickly build a string word set. 
This code is an example of how to create string functions from low-level operations.


Using an Indirect Threaded Forth, this code compiles to only 304 bytes on a 16 bit controller! (with labels stripped out)
Adding the 256 byte buffer it takes only 560 bytes; useable in small embedded environments.

 

```forth
\ Rosetta Code Binary Strings Demo in Forth
\ Portions of this code are found at http://forth.sourceforge.net/mirror/toolbelt-ext/index.html

\ String words created in this code:
\       STR<    STR>    STR=    COMPARESTR      SUBSTR  STRPAD  CLEARSTR
\       =""     ="      STRING: MAXLEN  REPLACE-CHAR    COPYSTR WRITESTR
\       ,"      APPEND-CHAR     STRING, PLACE   CONCAT  APPEND  C+!  ENDSTR
\       COUNT   STRLEN

: STRLEN  ( addr -- length)  c@ ;        \ alias the "character fetch" operator

: COUNT   ( addr --  addr+1 length)      \ Standard word. Shown for explanation
          dup  strlen swap 1+ swap ;     \ returns the address+1 and the length byte on the stack

: ENDSTR  ( str -- addr)                 \ calculate the address at the end of a string
          COUNT + ;

: C+!     ( n addr -- )                  \ primitive: increment a byte at addr by n
          DUP C@ ROT + SWAP C! ;

: APPEND  ( addr1 length addr2 -- )      \ Append addr1 length to addr2
          2dup 2>r  endstr swap move 2r> c+! ;

: CONCAT  ( string1 string2 -- )          \ concatenate counted string1 to counted string2
           >r  COUNT  R> APPEND ;

: PLACE  ( addr1 len addr2 -- )           \ addr1 and length, placed at addr2 as counted string
           2dup 2>r  char+  swap  move  2r> c! ;

: STRING, ( addr len -- )                 \ compile a string at the next available memory (called 'HERE')
            here  over char+  allot  place ;

: APPEND-CHAR  ( char string -- )         \ append char to string
           dup >r  count dup 1+ r> c! + c! ;

: ,"       [CHAR] " PARSE  STRING, ;      \ Parse input stream until '"' and compile into memory


: WRITESTR ( string -- )                  \ output a counted string with a carriage return
           count  type CR ;

: COPYSTR  ( string1 string3 -- )         \ String cloning and copying COPYSTR
           >r  count  r> PLACE ;

: REPLACE-CHAR ( char1 char2 string -- )  \ replace all char2 with char1 in string
          count                           \ get string's address and length
          BOUNDS                          \ calc start and end addr of string for do-loop
          DO                              \ do a loop from start address to end address
             I C@ OVER =                  \ fetch the char at loop index compare to CHAR2
             IF
                OVER I C!                 \ if its equal, store CHAR1 into the index address
             THEN
          LOOP
          2drop ;                         \ drop the chars off the stack


 256 constant maxlen                      \ max size of byte counted string in this example

: string:   CREATE    maxlen ALLOT ;      \ simple string variable constructor


: ="      ( string -- )                   \ String variable assignment operator (compile time only)
          [char] " PARSE  ROT  PLACE ;

: =""     ( string -- )  0 swap c! ;      \ empty a string, set count to zero


: clearstr ( string -- )                  \ erase a string variables contents, fill with 0
           maxlen erase ;


  string: strpad                           \ general purpose storage buffer

: substr  ( string1 start length -- strpad) \ Extract a substring of string and return an output string
          >r >r                             \ push start,length
          count                             \ compute addr,len 
          r> 1- /string                     \ pop start, subtract 1, cut string 
          drop r>                           \ drop existing length, pop new length
          strpad place                      \ place new stack string in strpad
          strpad ;                          \ return address of strpad

\ COMPARE takes the 4 inputs from the stack (addr1 len1  addr2 len2 )
\ and returns a flag for equal (0) , less-than (1)  or greater-than (-1) on the stack

  : comparestr ( string1 string2 -- flag)  \ adapt for use with counted strings
              count rot count compare ;

\ now it's simple to make new operators
  : STR=   ( string1 string2 -- flag)
             comparestr  0= ;

  : STR>   ( string1 string2 -- flag)
             comparestr -1 = ;

  : STR<   ( string1 string2 -- flag)
             comparestr 1 = ;


```


With the above code compiled into our system, we can test interactively 
at the Forth console to see if we have satisfied the Rosetta code requirements


```forth
\ Rosetta Code Binary String tasks Console Tests

\ 1. String creation and destruction (when needed and if there's no garbage collection or similar mechanism)

\ RAW Forth can manually create a binary string with the C, operator.
\ C, takes a byte off the stack and writes it into the next available memory address
\ then increments the Forth internal memory pointer by 1 byte.
\ 'binary_string'  drops it's address on the stack. Nothing more. (ie: pointer to the string)

HEX ok
    create binary_string   9 c,  1 c, 2 c, 3 c, 4 c, 5 c, 
                           0A c, 0B c, 0C c, 0FF c,        \ 1st byte is length
ok

\ test what we created using the DUMP utility

 binary_string count dump
 25EC:7365  01 02 03 04 05 0A 0B 0C  FF 04 44 55 4D 50 00 20  ..........DUMP.
 ok


\ Alternatively we can create static string variables using our constructor 
    string: buffer1  ok
    string: buffer2  ok
  
DECIMAL  ok
  
\ 2. String assignment 

\ create string constants with assignments(static, counted strings)  ok
     create string1  ," Now is the time for all good men to come to the aid"
     create string2  ," Right now!"  ok

\ assign text to string variables with syntacic sugar
     buffer1 =" This text will go into the memory allocated for buffer1"  ok
     buffer2 =""  ok

\ or use S" and PLACE
     S" The rain in Spain..." buffer2 PLACE ok
 
\ Test the assignments  
     string2 writestr Right now!
 ok
     string1 writestr Now is the time for all good men to come to the aid
 ok
     buffer1 writestr This text will go into the memory allocated for buffer1
 ok
     buffer2 writestr The rain in Spain...
 ok


\ destroy string contents. Fill string with zero
     buffer1 clearstr  ok
     buffer1 40 dump
25EC:7370  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  ................
25EC:7380  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  ................
25EC:7390  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  ................
 ok

\ 3. String comparison. ( the '.'  prints the top of the stack in these examples)
     buffer1 =" ABCDEFG"  ok
     buffer2 =" ABCDEFG"  ok
   
     buffer1 buffer2 STR= .  ( should be -1, TRUE flag) -1  ok

     string1 buffer1 str> .  ( should be  0) 0  ok
     string1 buffer1 str< .  ( should be -1) -1  ok


\ 4. String cloning and copying
     string1 buffer1 COPYSTR  ok

     string1 writestr Now is the time for all good men to come to the aid  ok
     buffer1 writestr Now is the time for all good men to come to the aid  ok


\ 5. Check if a string is empty
     buffer1 len . 55  ok
     buffer1 =""           \ assign null string  ok
     buffer1 len . 0  ok



\ 6. Append a byte to a string
     buffer2 =" Append this"  ok
     buffer2 writestr Append this
 ok
     char !  buffer2 APPEND-CHAR  ok
     buffer2 writestr Append this!
 ok
hex  ok
     0A buffer2 APPEND-CHAR     \ append a raw carriage return  ok
     0D buffer2 APPEND-CHAR     \ append a raw line-feed  ok
  ok
     buffer2 writestr Append this!

 ok
\ we see the extra line before OK so Appending binary chars worked

 decimal ok

\ 7. Extract a substring from a string. Result placed in a temp buffer automagically

     string1 writestr Now is the time for all good men to come to the aid ok

     string1 5 11 substr writestr is the time ok


\ 8. Replace every occurrence of a byte (or a string) in a string with another string
\    BL is a system constant for "Blank" ie the space character (HEX 020)

     buffer1 =" This*string*is*full*of*stars*"  ok
  ok
     BL  char *  buffer1 REPLACE-CHAR  ok
     buffer1 writestr This string is full of stars
 ok


\ 9. Join strings
     buffer1 =" James "  ok
     buffer2 =" Alexander"  ok
     buffer2 buffer1 CONCAT  ok
  ok
     buffer1 writestr James Alexander
 ok


```



## Go


```go
package main

import (
    "bytes"
    "fmt"
)

// Strings in Go allow arbitrary bytes.  They are implemented basically as
// immutable byte slices and syntactic sugar.  This program shows functions
// required by the task on byte slices, thus it mostly highlights what
// happens behind the syntactic sugar.  The program does not attempt to
// reproduce the immutability property of strings, as that does not seem
// to be the intent of the task.

func main() {
    // Task point: String creation and destruction.
    // Strings are most often constructed from literals as in s := "binary"
    // With byte slices,
    b := []byte{'b', 'i', 'n', 'a', 'r', 'y'}
    fmt.Println(b) // output shows numeric form of bytes.
    // Go is garbage collected.  There are no destruction operations.

    // Task point: String assignment.
    // t = s assigns strings.  Since strings are immutable, it is irrelevant
    // whether the string is copied or not.
    // With byte slices, the same works,
    var c []byte
    c = b
    fmt.Println(c)

    // Task point: String comparison.
    // operators <, <=, ==, >=, and > work directly on strings comparing them
    // by lexicographic order.
    // With byte slices, there are standard library functions, bytes.Equal
    // and bytes.Compare.
    fmt.Println(bytes.Equal(b, c)) // prints true

    // Task point: String cloning and copying.
    // The immutable property of Go strings makes cloning and copying
    // meaningless for strings.
    // With byte slices though, it is relevant.  The assignment c = b shown
    // above does a reference copy, leaving both c and b based on the same
    // underlying data.  To clone or copy the underlying data,
    d := make([]byte, len(b)) // allocate new space
    copy(d, b)                // copy the data
    // The data can be manipulated independently now:
    d[1] = 'a'
    d[4] = 'n'
    fmt.Println(string(b)) // convert to string for readable output
    fmt.Println(string(d))

    // Task point: Check if a string is empty.
    // Most typical for strings is s == "", but len(s) == 0 works too.
    // For byte slices, "" does not work, len(b) == 0 is correct.
    fmt.Println(len(b) == 0)

    // Task point: Append a byte to a string.
    // The language does not provide a way to do this directly with strings.
    // Instead, the byte must be converted to a one-byte string first, as in,
    // s += string('z')
    // For byte slices, the language provides the append function,
    z := append(b, 'z')
    fmt.Printf("%s\n", z) // another way to get readable output

    // Task point: Extract a substring from a string.
    // Slicing syntax is the for both strings and slices.
    sub := b[1:3]
    fmt.Println(string(sub))

    // Task point: Replace every occurrence of a byte (or a string)
    // in a string with another string.
    // Go supports this with similar library functions for strings and
    // byte slices.  Strings:  t = strings.Replace(s, "n", "m", -1).
    // The byte slice equivalent returns a modified copy, leaving the
    // original byte slice untouched,
    f := bytes.Replace(d, []byte{'n'}, []byte{'m'}, -1)
    fmt.Printf("%s -> %s\n", d, f)

    // Task point: Join strings.
    // Using slicing syntax again, with strings,
    // rem := s[:1] + s[3:] leaves rem == "bary".
    // Only the concatenation of the parts is different with byte slices,
    rem := append(append([]byte{}, b[:1]...), b[3:]...)
    fmt.Println(string(rem))
}
```

{{out}}

```txt

[98 105 110 97 114 121]
[98 105 110 97 114 121]
true
binary
banany
false
binaryz
in
banany -> bamamy
bary

```



## Haskell

Note that any of the following functions can be assigned
to 'variables' in a working program or could just as easily
be written as one-off expressions.  They are given here as
they are to elucidate the workings of Haskell's type system.
Hopefully the type declarations will help beginners understand
what's going on.
Also note that there are likely more concise ways to express many
of the  below functions.  However, I have opted for clarity here
as Haskell can be somewhat intimidating to the (currently) non-
functional programmer.

```haskell
import Text.Regex
{- The above import is needed only for the last function.
It is used there purely for readability and conciseness -}

{- Assigning a string to a 'variable'.
We're being explicit about it just for show.
Haskell would be able to figure out the type
of "world" -}
string = "world" :: String
```



```haskell
{- Comparing two given strings and
returning a boolean result using a
simple conditional -}
strCompare :: String -> String -> Bool
strCompare x y =
    if x == y
        then True
        else False
```



```haskell
{- As strings are equivalent to lists
of characters in Haskell, test and
see if the given string is an empty list -}
strIsEmpty :: String -> Bool
strIsEmpty x =
    if x == []
        then True
        else False
```



```haskell
{- This is the most obvious way to
append strings, using the built-in
(++) concatenation operator 
Note the same would work to join
any two strings (as 'variables' or
as typed strings -}
strAppend :: String -> String -> String
strAppend x y = x ++ y
```



```haskell
{- Take the specified number of characters
from the given string -}
strExtract :: Int -> String -> String
strExtract x s = take x s
```



```haskell
{- Take a certain substring, specified by
two integers, from the given string -}
strPull :: Int -> Int -> String -> String
strPull x y s = take (y-x+1) (drop x s)
```



```haskell
{- Much thanks to brool.com for this nice
and elegant solution.  Using an imported standard library
(Text.Regex), replace a given substring with another -}
strReplace :: String -> String -> String -> String
strReplace old new orig = subRegex (mkRegex old) orig new
```


=={{header|Icon}} and {{header|Unicon}}==
Icon and Unicon strings strings are variable length and unrestricted. See [[Logical_operations#Icon_and_Unicon|Logical Operations]] for ways to manipulate strings at the bit level.

```Icon
s := "\x00" # strings can contain any value, even nulls
s := "abc"             # create a string
s := &null             # destroy a string (garbage collect value of s; set new value to &null)
v := s                 # assignment
s == t                 # expression s equals t
s << t                 # expression s less than t
s <<= t                # expression s less than or equal to t
v := s                 # strings are immutable, no copying or cloning are needed
s == ""                # equal empty string
*s = 0                 # string length is zero
s ||:= "a"             # append a byte "a" to s via concatenation
t := s[2+:3]           # t is set to position 2 for 3 characters 
s := replace(s,s2,s3)  # IPL replace function 
s := s1 || s2          # concatenation (joining) of strings
```


The {{libheader|Icon Programming Library}} provides the procedure [http://www.cs.arizona.edu/icon/library/src/procs/strings.icn replace in strings]

```Icon
procedure replace(s1, s2, s3)		#: string replacement
   local result, i

   result := ""
   i := *s2
   if i = 0 then fail			# would loop on empty string

   s1 ? {
      while result ||:= tab(find(s2)) do {
         result ||:= s3
         move(i)
         }
      return result || tab(0)
      }

end
```



## J

J's literal data type supports arbitrary binary data (strings are binary strings by default).  J's semantics are pass by value (with garbage collection) with a minor exception (mapped files).

* Example binary string creation

```j
   name=: ''
```


* Example binary string deletion (removing all references to a string allows it to be deleted, in this case we give the name a numeric value to replace its prior string value):

```j>   name=: 0</lang


* Example binary string assignment

```j
   name=: 'value'
```


* Example binary string comparison

```j
   name1 -: name2
```


* Example binary string cloning and copying

```j
   name1=: 'example'
   name2=: name1
```


Though, technically, its the internal reference which is cloned, not the internal representation of the value.  But operations which modify strings are copy on write, so this distinction is not visible without going outside the language.

* Example check if a binary string is empty

```j>   0=#string</lang


* Example apppend a byte to a binary string

```j
   string=: 'example'
   byte=: DEL
   string=: string,byte
```


* Extract a substring from a binary string

```j
   3{.5}.'The quick brown fox runs...'
```


* Replace every occurrence of a byte (or a string) in a string with another string

```j
require 'strings'
'The quick brown fox runs...' rplc ' ';' !!! '
```


* Join strings

```j
   'string1','string2'
```


Note also: given an integer n, the corresponding byte value may be obtained by indexing into <code>a.</code> which is the ordered array of all bytes.:

```j
   n{a.
```


Thus, the binary string containing bytes with numeric values 1 0 255 can be obtained this way:

```j
1 0 255{a.
```



## Java



```java
import java.io.ByteArrayOutputStream;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;

public class MutableByteString {

    private byte[] bytes;
    private int length;

    public MutableByteString(byte... bytes) {
        setInternal(bytes);
    }

    public int length() {
        return length;
    }

    public boolean isEmpty() {
        return length == 0;
    }

    public byte get(int index) {
        return bytes[check(index)];
    }

    public void set(byte[] bytes) {
        setInternal(bytes);
    }

    public void set(int index, byte b) {
        bytes[check(index)] = b;
    }

    public void append(byte b) {
        if (length >= bytes.length) {
            int len = 2 * bytes.length;
            if (len < 0)
                len = Integer.MAX_VALUE;
            bytes = Arrays.copyOf(bytes, len);
        }
        bytes[length] = b;
        length++;
    }

    public MutableByteString substring(int from, int to) {
        return new MutableByteString(Arrays.copyOfRange(bytes, from, to));
    }

    public void replace(byte[] from, byte[] to) {
        ByteArrayOutputStream copy = new ByteArrayOutputStream();
        if (from.length == 0) {
            for (byte b : bytes) {
                copy.write(to, 0, to.length);
                copy.write(b);
            }
            copy.write(to, 0, to.length);
        } else {
            for (int i = 0; i < length; i++) {
                if (regionEquals(i, from)) {
                    copy.write(to, 0, to.length);
                    i += from.length - 1;
                } else {
                    copy.write(bytes[i]);
                }
            }
        }
        set(copy.toByteArray());
    }

    public boolean regionEquals(int offset, MutableByteString other, int otherOffset, int len) {
        if (Math.max(offset, otherOffset) + len < 0)
            return false;
        if (offset + len > length || otherOffset + len > other.length())
            return false;
        for (int i = 0; i < len; i++) {
            if (bytes[offset + i] != other.get(otherOffset + i))
                return false;
        }
        return true;
    }

    public String toHexString() {
        char[] hex = new char[2 * length];
        for (int i = 0; i < length; i++) {
            hex[2 * i] = "0123456789abcdef".charAt(bytes[i] >> 4 & 0x0F);
            hex[2 * i + 1] = "0123456789abcdef".charAt(bytes[i] & 0x0F);
        }
        return new String(hex);
    }

    public String toStringUtf8() {
        return new String(bytes, 0, length, StandardCharsets.UTF_8);
    }

    private void setInternal(byte[] bytes) {
        this.bytes = bytes.clone();
        this.length = bytes.length;
    }

    private boolean regionEquals(int offset, byte[] other) {
        int len = other.length;
        if (offset < 0 || offset + len < 0)
            return false;
        if (offset + len > length)
            return false;
        for (int i = 0; i < len; i++) {
            if (bytes[offset + i] != other[i])
                return false;
        }
        return true;
    }

    private int check(int index) {
        if (index < 0 || index >= length)
            throw new IndexOutOfBoundsException(String.valueOf(index));
        return index;
    }
}
```


Test code:


```java
import static org.hamcrest.CoreMatchers.is;

import java.nio.charset.StandardCharsets;
import org.junit.Assert;
import org.junit.Test;

public class MutableByteStringTest {

    @Test
    public void testReplaceEmpty() {
        MutableByteString str = new MutableByteString("hello".getBytes(StandardCharsets.UTF_8));
        str.replace(new byte[]{}, new byte[]{'-'});

        Assert.assertThat(str.toStringUtf8(), is("-h-e-l-l-o-"));
    }

    @Test
    public void testReplaceMultiple() {
        MutableByteString str = new MutableByteString("hello".getBytes(StandardCharsets.UTF_8));
        str.replace(new byte[]{'l'}, new byte[]{'1', '2', '3'});

        Assert.assertThat(str.toStringUtf8(), is("he123123o"));
    }

    @Test
    public void testToHexString() {
        MutableByteString str = new MutableByteString("hello".getBytes(StandardCharsets.UTF_8));

        Assert.assertThat(str.toHexString(), is("68656c6c6f"));
    }

    @Test
    public void testAppend() {
        MutableByteString str = new MutableByteString("hello".getBytes(StandardCharsets.UTF_8));
        str.append((byte) ',');
        str.append((byte) ' ');
        str.append((byte) 'w');
        str.append((byte) 'o');
        str.append((byte) 'r');
        str.append((byte) 'l');
        str.append((byte) 'd');

        Assert.assertThat(str.toStringUtf8(), is("hello, world"));
    }
    @Test
    public void testSubstring() {
        MutableByteString str = new MutableByteString("hello, world".getBytes(StandardCharsets.UTF_8));

        Assert.assertThat(str.substring(0, 5).toStringUtf8(), is("hello"));
        Assert.assertThat(str.substring(7, 12).toStringUtf8(), is("world"));
    }

    @Test
    public void testRegionEquals() {
        MutableByteString str = new MutableByteString("hello".getBytes(StandardCharsets.UTF_8));

        Assert.assertThat(str.regionEquals(0, new MutableByteString(new byte[]{'h'}), 0, 1), is(true));
        Assert.assertThat(str.regionEquals(0, new MutableByteString(new byte[]{'h'}), 0, 2), is(false));
    }
}
```



## JavaScript


JavaScript has native support for binary strings.  All strings are "binary" and they're not zero terminated; however to be more exact you can't really see the bytes on the string, strings go from Unicode 0 to Unicode FFFF

```JavaScript
//String creation
var str='';
//or 
str2=new String();


//String assignment
str="Hello";
//or
str2=', Hey there'; //can use " or '
str=str+str2;//concantenates
//string deletion
delete str2;//this will return true or false, true when it has been successfully deleted, it shouldn't/won't work when the variable has been declared with the keyword 'var', you don't have to initialize variables with the var keyword in JavaScript, but when you do, you cannot 'delete' them. However JavaScript garbage collects, so when the function returns, the variable declared on the function is erased.

//String comparison
str!=="Hello"; //!== not equal-> returns true there's also !===
str=="Hello, Hey there"; //returns true
//compares 'byte' by 'byte'
"Character Z">"Character A"; //returns true, when using > or < operators it converts the string to an array and evalues the first index that is higher than another. (using unicode values) in this case 'Z' char code is 90 (decimal) and 'A' char code is 65, therefore making one string "larger" than the other.

//String cloning and copying
string=str;//Strings are immutable therefore when you assign a string to a variable another one is created. So for two variables to have the 'same' string you have to add that string to an object, and get/set the string from that object

//Check if a string is empty
Boolean(''); //returns false
''[0]; //returns undefined
''.charCodeAt(); //returns NaN
''==0; //returns true 
''===0; //returns false
''==false; //returns true

//Append byte to String
str+="\x40";//using + operator before the equal sign on a string makes it equal to str=str+"\x40"

//Extract a substring from a string
//str is "Hello, Hey there@"
str.substr(3); //returns "lo, Hey there@"
str.substr(-5); //returns "here@" negative values just go to the end
str.substr(7,9); //returns "Hey there" index of 7 + 9 characters after the 7
str.substring(3); //same as substr
str.substring(-5); //negative values don't work on substring same as substr(0)
str.substring(7,9); //returns "He" that is, whatever is between index 7 and index 9, same as substring(9,7)

//Replace every occurence of x byte with another string
str3="url,url,url,url,url";
str3.replace(/,/g,'\n') //Regex ,returns the same string with the , replaced by \n 
str4=str3.replace(/./g,function(index){//it also supports callback functions, the function will be called when a match has been found..
return index==','?'\n':index;//returns replacement
})

//Join Strings
[str," ",str3].join(" "/*this is the character that will glue the strings*/)//we can join an array of strings
str3+str4; 
str.concat('\n',str4); //concantenate them
```



## jq


jq's strings are JSON strings and so cannot be safely used as "binary strings" in the sense of this article.  The most convenient way to store a string of bytes in jq is as a jq array of integers, it being understood that jq itself does **not** provide a mechanism for guaranteeing that all the elements of a particular array are integers in the expected range. 

It is appropriate therefore to introduce a filter for verifying that an entity is an array of integers in the appropriate range:
```jq
# If the input is a valid representation of a binary string
# then pass it along:
def check_binary:
  . as $a
  | reduce .[] as $x
    ($a; 
     if $x | (type == "number" and . == floor 
              and 0 <= . and . <= 255) then $a
     else error("\(.) is an invalid representation of a byte")
     end );
```

Examples

```jq
## Creation of an entity representing an empty binary string

[]

## Assignment

# Unless a check is appropriate, assignment can be done in the
# usual ways, for example:

[0] as $x    # assignment to a variable, $x

s as $x      # assignment of s to a variable

.key = s     # assignment to a key in a JSON object

# If s must be checked, these become:

(s|check_binary) as $x

.key = (s|check_binary)

## Concatenation:

str+str2

## Comparison

[72,101,108,108,111] == ("Hello"|explode)  # evaluates to true

# Other jq comparison operators (!=, <, >, <=, >=) can be used as well.

## Cloning and copying
# In jq, all entities are immutable and so the distinction between
# copying and cloning is irrelevant in jq.
# For example, consider the expression "$s[0] = 1"
# in the following:

[0] as $s | $s[0] = 1 | $s

# The result is [0] because the expression "$s[0] = 1"
# evaluates to [1] but does not alter $s.  The value of
# $s can be changed by assignment, e.g.

[0] as $s | $s[0] = 1 | . as $s  

## Check if an entity represents the empty binary string

length == 0
# or
s == []

## append a byte, b

s + [b]                 # if the byte, b, is known to be in range
s + ([b]|check_binary)  # if b is suspect

## Extract a substring from a string

# jq uses an index origin of 0 for both JSON arrays strings, 
# so to extract the substring with indices from m to (n-1)
# inclusive, the expression s[m:n] can be used.

# There are many other possibilities, such as s[m:], s[-1], etc.

## Replace every occurrence of one byte, x, with
## another sequence of bytes presented as an array, a,
## of byte-valued integers:

reduce .[] as $byte ([];
  if $byte == x then . + a else . + [$byte] end)

```



## Julia

{{trans|MATLAB}}

```julia

# String assignment. Creation and garbage collection are automatic.
a = "123\x00 abc "  # strings can contain bytes that are not printable in the local font
b = "456" * '\x09'
c = "789"
println(a)
println(b)
println(c)

# string comparison
println("(a == b) is $(a == b)")
 
# String copying.
A = a
B = b
C = c
println(A)
println(B)
println(C)
 
# check if string is empty
if length(a) == 0
println("string a is empty")
else
println("string a is not empty")
end

# append a byte (actually this is a Char in Julia, and may also be up to 32 bit Unicode) to a string 
a= a * '\x64'
println(a)
 
# extract a substring from string
e = a[1:6]
println(e)
      
# repeat strings with ^
b4 = b ^ 4
println(b4)
    
# Replace every occurrence of a string in another string with third string
r = replace(b4, "456" => "xyz")
println(r)

# join strings with *
d = a * b * c
println(d)

```

{{output}}
```txt

123  abc
456
789
(a == b) is false
123  abc
456
789
string a is not empty
123  abc d
123  a
456     456     456     456
xyz     xyz     xyz     xyz
123  abc d456   789

```



## Kotlin

Strings in Kotlin are sequences of 16-bit unicode characters and have a lot of functions built-in, including all those required by this task.

To do something different and consistent with the task, I've therefore implemented a simple ByteString class as a sequence of 8-bit signed bytes (Kotlin doesn't yet have an unsigned byte type) using the ISO 8859-1 encoding which, of course, represents the first 256 unicode characters. It's not possible to create user-defined literals in Kotlin and so ByteStrings need to be converted to 'ordinary' Strings to display them as such.

The implementation is not intended to be particularly efficient as I've sometimes delegated to the corresponding String class functions in the interests of both simplicity and brevity. Moreover, when Java 9's 'compact strings' feature is implemented, it won't even save memory as Strings which don't contain characters with code-points above 255 are apparently going to be flagged and stored internally as arrays of single bytes by the JVM, not arrays of 2 byte characters as at present.

```scala
class ByteString(private val bytes: ByteArray) : Comparable<ByteString> {
    val length get() = bytes.size

    fun isEmpty() = bytes.isEmpty()

    operator fun plus(other: ByteString): ByteString = ByteString(bytes + other.bytes)

    operator fun plus(byte: Byte) = ByteString(bytes + byte)

    operator fun get(index: Int): Byte {
        require (index in 0 until length)
        return bytes[index]
    }

    fun toByteArray() = bytes

    fun copy() = ByteString(bytes.copyOf())

    override fun compareTo(other: ByteString) = this.toString().compareTo(other.toString())

    override fun equals(other: Any?): Boolean {
        if (other == null || other !is ByteString) return false
        return compareTo(other) == 0
    }

    override fun hashCode() = this.toString().hashCode()

    fun substring(startIndex: Int) = ByteString(bytes.sliceArray(startIndex until length))

    fun substring(startIndex: Int, endIndex: Int) =
        ByteString(bytes.sliceArray(startIndex until endIndex))

    fun replace(oldByte: Byte, newByte: Byte): ByteString {
        val ba = ByteArray(length) { if (bytes[it] == oldByte) newByte else bytes[it] }
        return ByteString(ba)
    }

    fun replace(oldValue: ByteString, newValue: ByteString) =
        this.toString().replace(oldValue.toString(), newValue.toString()).toByteString()

    override fun toString(): String {
        val chars = CharArray(length)
        for (i in 0 until length) {
            chars[i] = when (bytes[i]) {
                in 0..127  -> bytes[i].toChar()
                else       -> (256 + bytes[i]).toChar()
            }
        }
        return chars.joinToString("")
    }
}

fun String.toByteString(): ByteString {
    val bytes = ByteArray(this.length)
    for (i in 0 until this.length) {
        bytes[i] = when (this[i].toInt()) {
            in 0..127   -> this[i].toByte()
            in 128..255 -> (this[i] - 256).toByte()
            else        -> '?'.toByte()  // say
        }
    }
    return ByteString(bytes)
}

/* property to be used as an abbreviation for String.toByteString() */
val String.bs get() = this.toByteString()

fun main(args: Array<String>) {
    val ba  = byteArrayOf(65, 66, 67)
    val ba2 = byteArrayOf(68, 69, 70)
    val bs  = ByteString(ba)
    val bs2 = ByteString(ba2)
    val bs3 = bs + bs2
    val bs4 = "GHI£€".toByteString()
    println("The length of $bs is ${bs.length}")
    println("$bs + $bs2 = $bs3")
    println("$bs + D = ${bs + 68}")
    println("$bs == ABC is ${bs == bs.copy()}")
    println("$bs != ABC is ${bs != bs.copy()}")
    println("$bs >= $bs2 is ${bs > bs2}")
    println("$bs <= $bs2 is ${bs < bs2}")
    println("$bs is ${if (bs.isEmpty()) "empty" else "not empty"}")
    println("ABC[1] = ${bs[1].toChar()}")
    println("ABC as a byte array is ${bs.toByteArray().contentToString()}")
    println("ABCDEF(1..5) = ${bs3.substring(1)}")
    println("ABCDEF(2..4) = ${bs3.substring(2,5)}")
    println("ABCDEF with C replaced by G is ${bs3.replace(67, 71)}")
    println("ABCDEF with CD replaced by GH is ${bs3.replace("CD".bs, "GH".bs)}")
    println("GHI£€ as a ByteString is $bs4")
}
```


{{out}}

```txt

The length of ABC is 3
ABC + DEF = ABCDEF
ABC + D = ABCD
ABC == ABC is true
ABC != ABC is false
ABC >= DEF is false
ABC <= DEF is true
ABC is not empty
ABC[1] = B
ABC as a byte array is [65, 66, 67]
ABCDEF(1..5) = BCDEF
ABCDEF(2..4) = CDE
ABCDEF with C replaced by G is ABGDEF
ABCDEF with CD replaced by GH is ABGHEF
GHI£€ as a ByteString is GHI£?

```



## Liberty BASIC

Liberty BASIC's strings are native byte strings. They can contain any byte sequence. They are not zero-terminated. They can be huge in size.

```lb

'string creation
s$ = "Hello, world!"

'string destruction - not needed because of garbage collection
s$ = ""

'string comparison
s$ = "Hello, world!"
If s$ = "Hello, world!" then print "Equal Strings"

'string copying
a$ = s$

'check If empty
If s$ = "" then print "Empty String"

'append a byte
s$ = s$ + Chr$(33)

'extract a substring
a$ = Mid$(s$, 1, 5)

'replace bytes
a$ = "Hello, world!"
for i = 1 to len(a$)
    if mid$(a$,i,1)="l" then
        a$=left$(a$,i-1)+"L"+mid$(a$,i+1)
    end if
next
print a$

'join strings
s$ = "Good" + "bye" + " for now."

```



## Lingo


```Lingo
-- String creation and destruction
foo = "Hello world!" -- created by assignment; destruction via garbage collection

-- Strings are binary safe
put numtochar(0) into char 6 of foo
put chartonum(foo.char[6])
-- 0
put str.char[7..foo.length]
-- "world!"

-- String cloning and copying
bar = foo -- copies foo contents to bar

-- String comparison
put (foo=bar) -- TRUE
put (foo<>bar) -- FALSE

-- Check if a string is empty
put (foo=EMPTY)
put (foo="")
put (foo.length=0)

-- Append a byte to a string
put "X" after foo
put chartonum(88) after foo

-- Extract a substring from a string
put foo.char[3..5]

-- Replace every occurrence of a byte (or a string) in a string with another string

----------------------------------------
-- Replace in string
-- @param {string} stringToFind
-- @param {string} stringToInsert
-- @param {string} input
-- @return {string}
----------------------------------------
on replaceAll (stringToFind, stringToInsert, input)
    output = ""
    findLen = stringToFind.length - 1
    repeat while TRUE
        currOffset = offset(stringToFind, input)
        if currOffset=0 then exit repeat
        put input.char[1..currOffset] after output
        delete the last char of output
        put stringToInsert after output
        delete input.char[1..(currOffset + findLen)]
    end repeat
    put input after output
    return output
end

put replaceAll("o", "X", foo)

-- Join strings (4x the same result)
foo = "Hello " & "world!"
foo = "Hello" & numtochar(32) & "world!"
foo = "Hello" & SPACE & "world!"
foo = "Hello" && "world!"
```



## Lua


```lua
foo = 'foo'             -- Ducktyping foo to be string 'foo'
bar = 'bar'
assert (foo == "foo")   -- Comparing string var to string literal
assert (foo ~= bar)
str = foo               -- Copy foo contents to str
if #str == 0 then       -- # operator returns string length
    print 'str is empty'
end
str=str..string.char(50)-- Char concatenated with .. operator
substr = str:sub(1,3)   -- Extract substring from index 1 to 3, inclusively

str = "string string string string"
-- This function will replace all occurances of 'replaced' in a string with 'replacement'
function replaceAll(str,replaced,replacement)
    local function sub (a,b)
        if b > a then
            return str:sub(a,b)
        end
        return nil
    end
    a,b = str:find(replaced)
    while a do
        str = str:sub(1,a-1) .. replacement .. str:sub(b+1,#str)
        a,b = str:find(replaced)
    end
    return str
end
str = replaceAll (str, 'ing', 'ong')
print (str)

str = foo .. bar -- Strings concatenate with .. operator
```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
(* String creation and destruction *)  BinaryString = {}; BinaryString = . ;
(* String assignment *)   BinaryString1 = {12,56,82,65} ,  BinaryString2 = {83,12,56,65} 
-> {12,56,82,65}
-> {83,12,56,65}
(* String comparison *)   BinaryString1 === BinaryString2
-> False
(* String cloning and copying *)  BinaryString3 = BinaryString1
-> {12,56,82,65}
(* Check if a string is empty *)  BinaryString3 === {}
-> False 
(* Append a byte to a string *)   AppendTo[BinaryString3, 22]
-> {12,56,82,65,22}
(* Extract a substring from a string *)  Take[BinaryString3, {2, 5}]
-> {56,82,65,22}
(* Replace every occurrence of a byte (or a string) in a string with another string *) 
BinaryString3 /. {22 -> Sequence[33, 44]}
-> {12,56,82,65,33,44}
(* Join strings *)  BinaryString4 = Join[BinaryString1 , BinaryString2]
-> {12,56,82,65,83,12,56,65}
```


=={{header|MATLAB}} / {{header|Octave}}==

```Matlab
	
	a=['123',0,' abc '];
	b=['456',9];
	c='789';
	disp(a);
	disp(b);
	disp(c);

	% string comparison
	printf('(a==b) is %i\n',strcmp(a,b));

	% string copying
	A = a;
	B = b;
	C = c;
	disp(A);
	disp(B);
	disp(C);

	% check if string is empty
	if (length(a)==0)
		printf('\nstring a is empty\n');
	else
		printf('\nstring a is not empty\n');
	end

	% append a byte to a string 
	a=[a,64];
        disp(a);

	% substring 
	e = a(1:6);
        disp(e);

	% join strings
	d=[a,b,c];
	disp(d);

```

Output:

```txt

123 abc 
456	
789
(a==b) is 0
123 abc 
456	
789

string a is not empty
123 abc @
123 a
123 abc @456	789

```



## Nim


```nim
var # creation
  x = "this is a string"
  y = "this is another string"
  z = "this is a string"

if x == z: echo "x is z" # comparison

z = "now this is another string too" # assignment

y = z # copying

if x.len == 0: echo "empty" # check if empty

x.add('!') # append a byte

echo x[5..8] # substring
echo x[8 .. ^1] # substring

z = x & y # join strings

import strutils

echo z.replace('t', 'T') # replace occurences of t with T
```



## OCaml


* String creation and destruction

<code>String.create n</code> returns a fresh string of length n, which initially contains arbitrary characters:

```ocaml
# String.create 10 ;;
- : string = "\000\023\000\000\001\000\000\000\000\000"
```


No destruction, OCaml features a garbage collector.

OCaml strings can contain any of the 256 possible bytes included the null character '\000'.

* String assignment

```ocaml
# let str = "some text" ;;
val str : string = "some text"

(* modifying a character, OCaml strings are mutable *)
# str.[0] <- 'S' ;;
- : unit = ()
```


* String comparison

```ocaml
# str = "Some text" ;;
- : bool = true

# "Hello" > "Ciao" ;;
- : bool = true
```


* String cloning and copying

```ocaml
# String.copy str ;;
- : string = "Some text"
```


* Check if a string is empty

```ocaml
# let string_is_empty s = (s = "") ;;
val string_is_empty : string -> bool = <fun>

# string_is_empty str ;;
- : bool = false

# string_is_empty "" ;;
- : bool = true
```


* Append a byte to a string

it is not possible to append a byte to a string, 
in the sens modifying the length of a given string,
but we can use the concatenation operator to append 
a byte and return the result as a new string


```ocaml
# str ^ "!" ;;
- : string = "Some text!"
```


But OCaml has a module named Buffer for string buffers.
This module implements string buffers that automatically expand as necessary. It provides accumulative concatenation of strings in quasi-linear time (instead of quadratic time when strings are concatenated pairwise).


```ocaml
Buffer.add_char str c
```


* Extract a substring from a string

```ocaml
# String.sub str 5 4 ;;
- : string = "text"
```


* Replace every occurrence of a byte (or a string) in a string with another string
using the '''Str''' module

```ocaml
# #load "str.cma";;
# let replace str occ by =
    Str.global_replace (Str.regexp_string occ) by str
  ;;
val replace : string -> string -> string -> string = <fun>
# replace "The white dog let out a single, loud bark." "white" "black" ;;
- : string = "The black dog let out a single, loud bark."
```


* Join strings

```ocaml
# "Now just remind me" ^ " how the horse moves again?" ;;
- : string = "Now just remind me how the horse moves again?"
```



## PARI/GP

This code accepts arbitrary characters, but you can use <code>Strchr</code> to display ASCII strings.

```parigp
cmp_str(u,v)=u==v
copy_str(v)=v \\ Creates a copy, not a pointer
append_str(v,n)=concat(v,n)
replace_str(source, n, replacement)=my(v=[]);for(i=1,#source,v=concat(v,if(source[i]==n,replacement,source[i]))); v

u=[72, 101, 108, 108, 111, 44, 32, 119, 111, 114, 108, 100];
v=[];
cmp_str(u,v)
w=copy_str(v)
#w==0
append_str(u,33)
u[8..12]
replace_str(u,108,[121])
concat(v,w)
```

{{out}}

```txt
%1 = 0
%2 = []
%3 = 1
%4 = [72, 101, 108, 108, 111, 44, 32, 119, 111, 114, 108, 100, 33]
%5 = [119, 111, 114, 108, 100]
%6 = [72, 101, 121, 121, 111, 44, 32, 119, 111, 114, 121, 100]
%7 = []
```



## Pascal

Pascal's original strings were limited to 255 characters. Most implementations had the string length in byte 0. Extension exist for longer strings as well as C compatible string terminated by null. See Examples below

```pascal
const
  greeting = 'Hello';
var
  s1: string;
  s2: ansistring;
  s3: pchar;
begin
{ Assignments }
  s1 := 'Mister Presiden';  (* typo is on purpose. See below! *)
{ Comparisons }
  if s2 > 'a' then
    writeln ('The first letter of ', s1, ' is later than a');
{ Cloning and copying }
  s2 := greeting;
{ Check if a string is empty }
  if s1 = '' then
    writeln('This string is empty!');
{ Append a byte to a string }
  s1 := s1 + 't';
{ Extract a substring from a string }
  s3 := copy(S2, 2, 4);  (* s3 receives ello *)
{ String replacement }  (* the unit StrUtils of the FreePascal rtl has AnsiReplaceStr *)
  s1 := AnsiReplaceStr('Thees ees a text weeth typos', 'ee', 'i');
{ Join strings}
  s3 := greeting + ' and how are you, ' + s1 + '?';
end.
```



## Perl

Effective string manipulation has been a part of Perl since the beginning.  Simple stuff is simply done, but modern Perl also supports Unicode, and tools like <code>pack/unpack</code> let you operate on strings below the level of bytes.

```perl
$s = undef;
say 'Nothing to see here' if ! defined $s;  # 'Nothing to see here'
say $s = '';                                # ''
say 'Empty string' if $s eq '';             # 'Empty string'
say $s = 'be';                              # 'be'
say $t = $s;                                # 'be'
say 'Same' if $t eq $s;                     # 'Same'
say $t = $t .'e'                            # 'bee'
say $t .= 'keeper';                         # 'beekeeper'
$t =~ s/ee/ook/; say $t;                    # 'bookkeeper'
say $u = substr $t, 2, 2;                   # 'ok'
say 'Oklahoma' . ' is ' . uc $u;            # 'Oklahoma is OK'
```



## Perl 6

{{Works with|rakudo|2018.03}}

```perl6
# Perl 6 is perfectly fine with NUL *characters* in strings:
my Str $s = 'nema' ~ 0.chr ~ 'problema!';
say $s;

# However, Perl 6 makes a clear distinction between strings
# (i.e. sequences of characters), like your name, or …
my Str $str = "My God, it's full of chars!";
# … and sequences of bytes (called Bufs), for example a PNG image, or …
my Buf $buf = Buf.new(255, 0, 1, 2, 3);
say $buf;

# Strs can be encoded into Blobs …
my Blob $this = 'round-trip'.encode('ascii');
# … and Blobs can be decoded into Strs …
my Str $that = $this.decode('ascii');

# So it's all there. Nevertheless, let's solve this task explicitly
# in order to see some nice language features:

# We define a class …
class ByteStr {
    # … that keeps an array of bytes, and we delegate some
    # straight-forward stuff directly to this attribute:
    # (Note: "has byte @.bytes" would be nicer, but that is
    # not yet implemented in Rakudo.)
    has Int @.bytes handles(< Bool elems gist push >);

    # A handful of methods …
    method clone() {
        self.new(:@.bytes);
    }

    method substr(Int $pos, Int $length) {
        self.new(:bytes(@.bytes[$pos .. $pos + $length - 1]));
    }

    method replace(*@substitutions) {
        my %h = @substitutions;
        @.bytes.=map: { %h{$_} // $_ }
    }
}

# A couple of operators for our new type:
multi infix:<cmp>(ByteStr $x, ByteStr $y) { $x.bytes.join cmp $y.bytes.join }
multi infix:<~>  (ByteStr $x, ByteStr $y) { ByteStr.new(:bytes(|$x.bytes, |$y.bytes)) }

# create some byte strings (destruction not needed due to garbage collection)
my ByteStr $b0 = ByteStr.new;
my ByteStr $b1 = ByteStr.new(:bytes( |'foo'.ords, 0, 10, |'bar'.ords ));

# assignment ($b1 and $b2 contain the same ByteStr object afterwards):
my ByteStr $b2 = $b1;

# comparing:
say 'b0 cmp b1 = ', $b0 cmp $b1;
say 'b1 cmp b2 = ', $b1 cmp $b2;

# cloning:
my $clone = $b1.clone;
$b1.replace('o'.ord => 0);
say 'b1 = ', $b1;
say 'b2 = ', $b2;
say 'clone = ', $clone;

# to check for (non-)emptiness we evaluate the ByteStr in boolean context:
say 'b0 is ', $b0 ?? 'not empty' !! 'empty';
say 'b1 is ', $b1 ?? 'not empty' !! 'empty';

# appending a byte:
$b1.push: 123;
say 'appended = ', $b1;

# extracting a substring:
my $sub = $b1.substr(2, 4);
say 'substr = ', $sub;

# replacing a byte:
$b2.replace(102 => 103);
say 'replaced = ', $b2;

# joining:
my ByteStr $b3 = $b1 ~ $sub;
say 'joined = ', $b3;
```


{{out}}
Note: The ␀ represents a NUL byte.

```txt
nema␀problema!
Buf:0x<ff 00 01 02 03>
round-trip
b0 cmp b1 = Less
b1 cmp b2 = Same
b1 = [102 0 0 0 10 98 97 114]
b2 = [102 0 0 0 10 98 97 114]
clone = [102 111 111 0 10 98 97 114]
b0 is empty
b1 is not empty
appended = [102 0 0 0 10 98 97 114 123]
substr = [0 0 10 98]
replaced = [103 0 0 0 10 98 97 114 123]
joined = [103 0 0 0 10 98 97 114 123 0 0 10 98]
```



## Phix

The native string type in Phix can be used to store raw binary data and supports all of the operations mentioned in this task.
Strings are reference counted, and mutable with copy-on-write semantics. Memory is managed automatically and very efficiently, strings can easily be a billion characters long (on 32-bit, the precise limit is in fact 1,610,612,711 characters, available memory and performance impacts aside) and have a null terminator for C compatibility, but can contain embedded nulls as well.
Note that attempting to set an element (character/byte) to a value outside the range 0..255 will result in automatic expansion to dword-(or qword-)sequence, and can result in a run-time type check.

```Phix
string s = "abc"
    s = x"ef bb bf" -- explicit binary string (the utf8 BOM)
    s[2] = 0
    s[3] = 'z'
    if s="\#EF\0z" then puts(1,"ok\n") end if
string t = s
    t[1..2] = "xy"  -- s remains unaltered
    ?t              -- "xyz"
    t = "food"      ?t
    t[2..3] = 'e'   ?t -- "feed"
    t[3..2] = "ast" ?t -- "feasted"
    t[3..-2] = ""   ?t -- "fed"
    if length(t)=0 then puts(1,"t is empty\n") end if
    if t!="" then puts(1,"t is not empty\n") end if
    t = "be"
    t &= 't'        ?t  -- bet
    t = 'a'&t       ?t  -- abet
    ?t[2..3]            -- be
    ?substitute(t,"be","bbo")   -- abbot
    ?substitute(t,"be","dep")   -- adept
    t = substitute(t,"be","dep") -- to actually modify t
    ?join({"abc","def","ghi"})  -- "abc def ghi"
    ?join({"abc","def","ghi"},"")   -- "abcdefghi"
    ?join({"abc","def","ghi"},"\n") -- "abc\ndef\nghi"
```

{{out}}

```txt

ok
"xyz"
"food"
"feed"
"feasted"
"fed"
t is not empty
"bet"
"abet"
"be"
"abbot"
"adept"
"abc def ghi"
"abcdefghi"
"abc\ndef\nghi"

```



## PicoLisp

Byte strings are represented in PicoLisp as lists of numbers. They can be
maniplated easily with the built-in list functionality.

I/O of raw bytes is done via the 'wr' (write) and 'rd' (read) functions. The
following creates a file consisting of 256 bytes, with values from 0 to 255:

```PicoLisp
: (out "rawfile"
   (mapc wr (range 0 255)) )
```

Looking at a hex dump of that file:

```PicoLisp
: (hd "rawfile")
00000000  00 01 02 03 04 05 06 07 08 09 0A 0B 0C 0D 0E 0F  ................
00000010  10 11 12 13 14 15 16 17 18 19 1A 1B 1C 1D 1E 1F  ................
00000020  20 21 22 23 24 25 26 27 28 29 2A 2B 2C 2D 2E 2F   !"#$%&'()*+,-./
00000030  30 31 32 33 34 35 36 37 38 39 3A 3B 3C 3D 3E 3F  0123456789:;<=>?
...
```

To read part of that file, an external tool like 'dd' might be used:

```PicoLisp
: (in '(dd "skip=32" "bs=1" "count=16" "if=rawfile")
   (make
      (while (rd 1)
         (link @) ) ) )
-> (32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47)
```

Now such byte lists can be assigned the normal way ('let', 'setq' etc.), they
can be compared with '=', '>', '>=' etc, and manipulated with all internal map-,
filter-, concatenation-, reversal-, pattern matching, and other functions.

If desired, a string containing meaningful values can also be converted to
a transient symbol, e.g. the example above

```PicoLisp
: (pack (mapcar char (32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47)))
-> " !\"#$%&'()*+,-./"
```



## PL/I


```PL/I

/* PL/I has immediate facilities for all those operations except for */
/* replace. */
s = t;                     /* assignment */
s = t || u;                /* catenation - append one or more bytes. */
if length(s) = 0 then ...  /* test for an empty string.              */
if s = t then ...          /* compare strings.                       */
u = substr(t, i, j);       /* take a substring of t beginning at the */
                           /* i-th character andcontinuing for j     */
                           /* characters.                            */
substr(u, i, j) = t;       /* replace j characters in u, beginning   */
                           /* with the i-th character.               */

/* In string t, replace every occurrence of string u with string v.  */
replace: procedure (t, u, v);
   declare (t, u, v) character (*) varying;

   do until (k = 0);
      k = index(t, u);
      if k > 0 then
         t = substr(t, 1, k-1) || v || substr(t, k+length(u));
   end;
end replace;

```



## PowerShell


```PowerShell

Clear-Host

## String creation (which is string assignment):
Write-Host "`nString creation (which is string assignment):" -ForegroundColor Cyan
Write-Host '[string]$s = "Hello cruel world"' -ForegroundColor Yellow
[string]$s = "Hello cruel world"

## String (or any variable) destruction:
Write-Host "`nString (or any variable) destruction:" -ForegroundColor Cyan
Write-Host 'Remove-Variable -Name s -Force' -ForegroundColor Yellow
Remove-Variable -Name s -Force

## Now reassign the variable:
Write-Host "`nNow reassign the variable:" -ForegroundColor Cyan
Write-Host '[string]$s = "Hello cruel world"' -ForegroundColor Yellow
[string]$s = "Hello cruel world"

Write-Host "`nString comparison -- default is case insensitive:" -ForegroundColor Cyan
Write-Host '$s -eq "HELLO CRUEL WORLD"' -ForegroundColor Yellow
$s -eq "HELLO CRUEL WORLD"
Write-Host '$s -match "HELLO CRUEL WORLD"' -ForegroundColor Yellow
$s -match "HELLO CRUEL WORLD"
Write-Host '$s -cmatch "HELLO CRUEL WORLD"' -ForegroundColor Yellow
$s -cmatch "HELLO CRUEL WORLD"

## Copy a string:
Write-Host "`nCopy a string:" -ForegroundColor Cyan
Write-Host '$t = $s' -ForegroundColor Yellow
$t = $s

## Check if a string is empty:
Write-Host "`nCheck if a string is empty:" -ForegroundColor Cyan
Write-Host 'if ($s -eq "") {"String is empty."} else {"String = $s"}' -ForegroundColor Yellow
if ($s -eq "") {"String is empty."} else {"String = $s"}

## Append a byte to a string:
Write-Host "`nAppend a byte to a string:" -ForegroundColor Cyan
Write-Host "`$s += [char]46`n`$s" -ForegroundColor Yellow
$s += [char]46
$s

## Extract (and display) substring from a string:
Write-Host "`nExtract (and display) substring from a string:" -ForegroundColor Cyan
Write-Host '"Is the world $($s.Substring($s.IndexOf("c"),5))?"' -ForegroundColor Yellow
"Is the world $($s.Substring($s.IndexOf("c"),5))?"

## Replace every occurrence of a byte (or a string) in a string with another string:
Write-Host "`nReplace every occurrence of a byte (or a string) in a string with another string:" -ForegroundColor Cyan
Write-Host "`$t = `$s -replace `"cruel`", `"beautiful`"`n`$t" -ForegroundColor Yellow
$t = $s -replace "cruel", "beautiful"
$t

## Join strings:
Write-Host "`nJoin strings [1]:" -ForegroundColor Cyan
Write-Host '"Is the world $($s.Split()[1]) or $($t.Split()[1])?"' -ForegroundColor Yellow
"Is the world $($s.Split()[1]) or $($t.Split()[1])?"
Write-Host "`nJoin strings [2]:" -ForegroundColor Cyan
Write-Host '"{0} or {1}... I don''t care." -f (Get-Culture).TextInfo.ToTitleCase($s.Split()[1]), $t.Split()[1]' -ForegroundColor Yellow
"{0} or {1}... I don't care." -f (Get-Culture).TextInfo.ToTitleCase($s.Split()[1]), $t.Split()[1]
Write-Host "`nJoin strings [3] (display an integer array using the -join operater):" -ForegroundColor Cyan
Write-Host '1..12 -join ", "' -ForegroundColor Yellow
1..12 -join ", "

## Display an integer array in a tablular format:
Write-Host "`nMore string madness... display an integer array in a tablular format:" -ForegroundColor Cyan
Write-Host '1..12 | Format-Wide {$_.ToString().PadLeft(2)}-Column 3 -Force' -NoNewline -ForegroundColor Yellow
1..12 | Format-Wide {$_.ToString().PadLeft(2)} -Column 3 -Force

```

{{Out}}

```txt

String creation (which is string assignment):
[string]$s = "Hello cruel world"

String (or any variable) destruction:
Remove-Variable -Name s -Force

Now reassign the variable:
[string]$s = "Hello cruel world"

String comparison -- default is case insensitive:
$s -eq "HELLO CRUEL WORLD"
True
$s -match "HELLO CRUEL WORLD"
True
$s -cmatch "HELLO CRUEL WORLD"
False

Copy a string:
$t = $s

Check if a string is empty:
if ($s -eq "") {"String is empty."} else {"String = $s"}
String = Hello cruel world

Append a byte to a string:
$s += [char]46
$s
Hello cruel world.

Extract (and display) substring from a string:
"Is the world $($s.Substring($s.IndexOf("c"),5))?"
Is the world cruel?

Replace every occurrence of a byte (or a string) in a string with another string:
$t = $s -replace "cruel", "beautiful"
$t
Hello beautiful world.

Join strings [1]:
"Is the world $($s.Split()[1]) or $($t.Split()[1])?"
Is the world cruel or beautiful?

Join strings [2]:
"{0} or {1}... I don't care." -f (Get-Culture).TextInfo.ToTitleCase($s.Split()[1]), $t.Split()[1]
Cruel or beautiful... I don't care.

Join strings [3] (display an integer array using the -join operater):
1..12 -join ", "
1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12

More string madness... display an integer array in a tablular format:
1..12 | Format-Wide {$_.ToString().PadLeft(2)}-Column 3 -Force

 1                                                            2                                                            3                                                         
 4                                                            5                                                            6                                                         
 7                                                            8                                                            9                                                         
10                                                           11                                                           12                                                         

```



## Prolog



```prolog
% Create a string (no destruction necessary)
?- X = "a test string".
X = "a test string".

% String assignment, there is no assignment but you can unify between variables, also 'String cloning and copying'
?- X = "a test string", X = Y.
X = Y, Y = "a test string".

% String comparison
?- X = "a test string", Y = "a test string", X = Y.
X = Y, Y = "a test string".

?- X = "a test string", Y = "a different string", X = Y.
false.

% Test for empty string, this is the same as string comparison.
?- X = "a test string", Y = "", X = Y.
false.

?- X = "", Y = "", X = Y.
false.

% Append a byte to a string
?- X = "a test string", string_concat(X, "!", Y).
X = "a test string",
Y = "a test string!".

% Extract a substring from a string
?- X = "a test string", sub_string(X, 2, 4, _, Y).
X = "a test string",
Y = "test".

?- X = "a test string", sub_string(X, Before, Len, After, test).
X = "a test string",
Before = 2,
Len = 4,
After = 7 ;
false.

% Replace every occurrence of a byte (or a string) in a string with another string
?- X = "a test string", re_replace('t'/g, 'o', X, Y).
X = "a test string",
Y = "a oeso soring".

% Join strings
?- X = "a test string", Y = " with extra!", string_concat(X, Y, Z).
X = "a test string",
Y = " with extra!",
Z = "a test string with extra!".

```



## PureBasic


```PureBasic

;string creation
x$ = "hello world"
 
;string destruction
x$ = ""
 
;string comparison
If x$ = "hello world" : PrintN("String is equal") : EndIf  
 
;string copying;
y$ = x$
 
; check If empty
If x$ = "" : PrintN("String is empty") : EndIf
 
; append a byte
x$ = x$ + Chr(41)
 
; extract a substring
x$ = Mid(x$, 1, 5) 
 
; replace bytes
x$ = ReplaceString(x$, "world", "earth") 
 
; join strings
x$ = "hel" + "lo w" + "orld"

```



## Python


### 2.x

Python 2.x's string type (<code>str</code>) is a native byte string type. They can contain any byte sequence - they're not zero-terminated. There is a separate type for Unicode data (<code>unicode</code>).

* String creation


```python
s1 = "A 'string' literal \n"
s2 = 'You may use any of \' or " as delimiter'
s3 = """This text 
   goes over several lines
       up to the closing triple quote"""
```


* String assignment

There is nothing special about assignments:


```python
s = "Hello "
t = "world!"
u = s + t   # + concatenates
```


* String comparison

They're compared byte by byte, lexicographically:


```python
assert "Hello" == 'Hello'
assert '\t' == '\x09'
assert "one" < "two"
assert "two" >= "three"
```


* String cloning and copying

Strings are immutable, so there is no need to clone/copy them. If you want to modify a string, you must create a new one with the desired contents. (There is another type, ''array'', that provides a mutable buffer)

* Check if a string is empty


```python
if x=='': print "Empty string"
if not x: print "Empty string, provided you know x is a string"
```


* Append a byte to a string


```python
txt = "Some text"
txt += '\x07'
# txt refers now to a new string having "Some text\x07"
```


* Extract a substring from a string

Strings are sequences, they can be indexed with s[index] (index is 0-based) and sliced s[start:stop] (all characters from s[start] up to, but ''not'' including, s[stop])


```python
txt = "Some more text"
assert txt[4] == " "
assert txt[0:4] == "Some"
assert txt[:4] == "Some" # you can omit the starting index if 0
assert txt[5:9] == "more"
assert txt[5:] == "more text" # omitting the second index means "to the end"
```


Negative indexes count from the end: -1 is the last byte, and so on:


```python
txt = "Some more text"
assert txt[-1] == "t"
assert txt[-4:] == "text"
```


* Replace every occurrence of a byte (or a string) in a string with another string

Strings are objects and have methods, like replace:


```python
v1 = "hello world"
v2 = v1.replace("l", "L")
print v2 # prints heLLo worLd
```


* Join strings

If they're separate variables, use the + operator:


```python
v1 = "hello" 
v2 = "world"
msg = v1 + " " + v2
```


If the elements to join are contained inside any iterable container (e.g. a list)


```python
items = ["Smith", "John", "417 Evergreen Av", "Chimichurri", "481-3172"]
joined = ",".join(items)
print joined
# output:
# Smith,John,417 Evergreen Av,Chimichurri,481-3172
```


The reverse operation (split) is also possible:


```python
line = "Smith,John,417 Evergreen Av,Chimichurri,481-3172"
fields = line.split(',')
print fields
# output:
# ['Smith', 'John', '417 Evergreen Av', 'Chimichurri', '481-3172']
```



### 3.x

Python 3.x has two binary string types: <code>bytes</code> (immutable) and <code>bytearray</code> (mutable). They can contain any byte sequence. They are completely separate from the string type (<code>str</code>). Most of the operators for strings, also work on <code>bytes</code> and <code>bytearray</code>

To specify a literal immutable byte string (<code>bytes</code>), prefix a string literal with "b":

```python
s1 = b"A 'byte string' literal \n"
s2 = b'You may use any of \' or " as delimiter'
s3 = b"""This text 
   goes over several lines
       up to the closing triple quote"""
```


You can use the normal string escape sequences to encode special bytes.

Indexing a byte string results in an integer (the byte value at that byte):

```python
x = b'abc'
x[0] # evaluates to 97
```


Similarly, a byte string can be converted to and from a list of integers:


```python
x = b'abc'
list(x) # evaluates to [97, 98, 99]
bytes([97, 98, 99]) # evaluates to b'abc'
```



## Racket



```racket

#lang racket

;; Byte strings can be created either by a function (b1)
;; or as a literal string (b2). No operation is needed for
;; destruction due to garbage collection.

(define b1 (make-bytes 5 65))  ; b1 -> #"AAAAA"
(define b2 #"BBBBB")           ; b2 -> #"BBBBB"

;; String assignment. Note that b2 cannot be
;; mutated since literal byte strings are immutable.

(bytes-set! b1 0 66)           ; b1 -> #"BAAAA"

;; Comparison. Less than & greater than are
;; lexicographic comparison.

(bytes=? b1 b2)                ; -> #f
(bytes<? b1 b2)                ; -> #t
(bytes>? b1 b2)                ; -> #f

;; Byte strings can be cloned by copying to a
;; new one or by overwriting an existing one.

(define b3 (bytes-copy b1))    ; b3 -> #"BAAAA"
(bytes-copy! b1 0 b2)          ; b1 -> #"BBBBB"

;; Byte strings can be appended to one another. A
;; single byte is appended as a length 1 string.

(bytes-append b1 b2)           ; -> #"BBBBBBBBBB"
(bytes-append b3 #"B")         ; -> #"BAAAAB"

;; Substring

(subbytes b3 0)                ; -> #"BAAAA"
(subbytes b3 0 2)              ; -> #"BA"

;; Regular expressions can be used to do replacements
;; in a byte string (or ordinary strings)

(regexp-replace #"B" b1 #"A")  ; -> #"ABBBB" (only the first one)
(regexp-replace* #"B" b1 #"A") ; -> #"AAAAA"

;; Joining strings

(bytes-join (list b2 b3) #" ") ; -> #"BBBBB BAAAA"

```


## Red


```Red
Red []
s: copy "abc"   ;; string creation

s: none ;; destruction
t: "Abc"
if t == "abc" [print "equal case"]              ;; comparison , case sensitive
if t = "abc" [print "equal (case insensitive)"]  ;; comparison , case insensitive
s: copy ""                                        ;; copying string
if empty? s [print "string is empty "]          ;; check if string is empty 
append s #"a"                                    ;; append byte
substr: copy/part at "1234" 3 2               ;; ~ substr ("1234" ,3,2) , red has 1 based indices !
?? substr
s: replace/all "abcabcabc" "bc" "x"             ;; replace all "bc" by "x"
?? s
s: append "hello " "world"                        ;; join 2 strings
?? s
s: rejoin ["hello " "world" " !"]                   ;; join multiple strings
?? s

```

{{out}}

```txt
equal (case insensitive)
string is empty 
substr: "34"
s: "axaxax"
s: "hello world"
s: "hello world !"
>> 

```



## REXX


```REXX
/*REXX program demonstrates methods (code examples)  to use and express  binary strings.*/
dingsta= '11110101'b                             /*four versions, bit string assignment.*/
dingsta= "11110101"b                             /*this is the same assignment as above.*/
dingsta= '11110101'B                             /*  "   "  "    "      "       "   "   */
dingsta= '1111 0101'B                            /*  "   "  "    "      "       "       */

dingsta2= dingsta                                /*clone one string to another (a copy).*/
other= '1001 0101 1111 0111'b                    /*another binary  (or bit)  string.    */
if dingsta= other    then say 'they are equal'   /*compare the two  (binary)  strings.  */
if other== ''        then say "OTHER is empty."  /*see if the   OTHER   string is empty.*/
otherA= other || '$'                             /*append a dollar sign ($)  to  OTHER. */
otherB= other'$'                                 /*same as above,  but with less fuss.  */
  guts= substr(c2b(other), 10, 3)                /*obtain the  10th  through  12th bits.*/
   new= changeStr('A' , other, "Z")              /*change the upper  letter  A  ──►  Z. */
    tt= changeStr('~~', other, ";")              /*change two tildes ──►  one semicolon.*/
joined= dingsta || dingsta2                      /*join two strings together (concat).  */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
c2b:       return x2b( c2x( arg(1) ) )           /*return the string as a binary string.*/
```

Some older REXXes don't have a   '''changestr'''   BIF,   so one is included here   ──►   [[CHANGESTR.REX]].





## Ring

The String in the Ring programming language holds and manipulates an arbitrary sequence of bytes. 


```ring
# string creation
x = "hello world"
 
# string destruction
x = NULL
 
# string assignment with a null byte
x = "a"+char(0)+"b"
see len(x)  # ==> 3
 
# string comparison
if x = "hello"
  See "equal"
else
  See "not equal"
ok
y = 'bc'
if strcmp(x,y) < 0
  See x + " is lexicographically less than " + y
ok
 
# string cloning 
xx = x
See x = xx       # true, same length and content

# check if empty
if x = NULL
  See "is empty"
ok
 
# append a byte
x += char(7)
 
# substring
x = "hello"
x[1] = "H"
See x + nl
 
# join strings
a = "hel"
b = "lo w"
c = "orld"
See a + b + c

```



## Ruby

A String object holds and manipulates an arbitrary sequence of bytes.  There are also the [http://www.ruby-doc.org/core/classes/Array.html#M002222 Array#pack] and [http://www.ruby-doc.org/core/classes/String.html#M000760 String#unpack] methods to convert data to binary strings.

```ruby
# string creation
x = "hello world"
 
# string destruction
x = nil
 
# string assignment with a null byte
x = "a\0b"
x.length  # ==> 3
 
# string comparison
if x == "hello"
  puts "equal"
else
  puts "not equal"
end
y = 'bc'
if x < y
  puts "#{x} is lexicographically less than #{y}"
end
 
# string cloning 
xx = x.dup
x == xx       # true, same length and content
x.equal?(xx)  # false, different objects
 
# check if empty
if x.empty?
  puts "is empty"
end
 
# append a byte
p x << "\07"
 
# substring
p xx = x[0..-2]
x[1,2] = "XYZ"
p x
 
# replace bytes
p y = "hello world".tr("l", "L")
 
# join strings
a = "hel"
b = "lo w"
c = "orld"
p d = a + b + c
```



## Run BASIC


```runbasic
' Create string
s$ = "Hello, world"  
 
' String destruction
s$ = ""             
 
' String comparison
If s$ = "Hello, world" then print "Equal String"
 
' Copying string
a$ = s$

' Check If empty
If s$ = "" then print "String is MT"
 
' Append a byte
s$ = s$ + Chr$(65)
 
' Extract a substring
a$ = Mid$(s$, 1, 5)   ' bytes 1 -> 5
 
'substitute string "world" with "universe"
a$ = "Hello, world"
for i = 1 to len(a$)
    if mid$(a$,i,5)="world" then
        a$=left$(a$,i-1)+"universe"+mid$(a$,i+5)
    end if
next
print a$
 
'join strings
s$ = "See " + "you " + "later."
print s$
```



## Rust

For extra documentation, refer to [https://doc.rust-lang.org/std/string/struct.String.html] and [https://doc.rust-lang.org/book/strings.html].

```rust
use std::str;

fn main() {
    // Create new string
    let string = String::from("Hello world!");
    println!("{}", string);
    assert_eq!(string, "Hello world!", "Incorrect string text");

    // Create and assign value to string
    let mut assigned_str = String::new();
    assert_eq!(assigned_str, "", "Incorrect string creation");
    assigned_str += "Text has been assigned!";
    println!("{}", assigned_str);
    assert_eq!(assigned_str, "Text has been assigned!","Incorrect string text");

    // String comparison, compared lexicographically byte-wise same as the asserts above
    if string == "Hello world!" && assigned_str == "Text has been assigned!" {
        println!("Strings are equal");
    }

    // Cloning -> string can still be used after cloning
    let clone_str = string.clone();
    println!("String is:{}  and  Clone string is: {}", string, clone_str);
    assert_eq!(clone_str, string, "Incorrect string creation");

    // Copying, string won't be usable anymore, accessing it will cause compiler failure
    let copy_str = string;
    println!("String copied now: {}", copy_str);

    // Check if string is empty
    let empty_str = String::new();
    assert!(empty_str.is_empty(), "Error, string should be empty");

    // Append byte, Rust strings are a stream of UTF-8 bytes
    let byte_vec = [65]; // contains A
    let byte_str = str::from_utf8(&byte_vec).unwrap();
    assert_eq!(byte_str, "A", "Incorrect byte append");

    // Substrings can be accessed through slices
    let test_str = "Blah String";
    let mut sub_str = &test_str[0..11];
    assert_eq!(sub_str, "Blah String", "Error in slicing");
    sub_str = &test_str[1..5];
    assert_eq!(sub_str, "lah ", "Error in slicing");
    sub_str = &test_str[3..];
    assert_eq!(sub_str, "h String", "Error in slicing");
    sub_str = &test_str[..2];
    assert_eq!(sub_str, "Bl", "Error in slicing");

    // String replace, note string is immutable
    let org_str = "Hello";
    assert_eq!(org_str.replace("l", "a"), "Heaao", "Error in replacement");
    assert_eq!(org_str.replace("ll", "r"), "Hero", "Error in replacement");

    // Joining strings requires a `String` and an &str or a two `String`s one of which needs an & for coercion
    let str1 = "Hi";
    let str2 = " There";
    let fin_str = str1.to_string() + str2;
    assert_eq!(fin_str, "Hi There", "Error in concatenation");

    // Joining strings requires a `String` and an &str or two `Strings`s, one of which needs an & for coercion
    let str1 = "Hi";
    let str2 = " There";
    let fin_str = str1.to_string() + str2;
    assert_eq!(fin_str, "Hi There", "Error in concatenation");

    // Splits -- note Rust supports passing patterns to splits
    let f_str = "Pooja and Sundar are up in Tumkur";
    let split_str: Vec<_> = f_str.split(' ').collect();
    assert_eq!(split_str, ["Pooja", "and", "Sundar", "are", "up", "in", "Tumkur"], "Error in string split");
}
```



## Seed7

Seed7 strings are capable to hold binary data.
The memory of Seed7 strings is managed automatically.
String declaration:

```txt

var string: stri is "asdf";   # variable declaration
const string: stri is "jkl";  # constant declaration

```


String assignment

```txt

stri := "blah";

```


String comparison

```txt

stri1 =  stri2         # equal
stri1 <> stri2         # not equal
stri1 <  stri2         # less than
stri1 <= stri2         # less than or equal
stri1 >  stri2         # greater than
stri1 >= stri2         # greater than or equal
compare(stri1, stri2)  # return -1, 0 or 1, depending on the comparison.

```


String copying (same as assignment)

```txt

stri2 := stri2;

```


Check if a string is empty

```txt

stri = ""         # compare with ""
length(stri) = 0  # check length

```


Append a byte to a string

```txt

stri &:= 'a';

```


Extract a substring from a string

```txt

stri[startPos .. endPos]   # substring from startPos to endPos
stri[startPos ..]          # substring from startPos to the end of stri
stri[.. endPos]            # substring from the beginning of stri to endPos
stri[startPos len aLength  # substring from startPos with maximum length of aLength

```


Replace every occurrence of a byte (or a string) in a string with another string

```txt

replace(stri,"la","al");

```


Join strings 

```txt

stri3 = stri1 & stri2;

```


The [http://seed7.sourceforge.net/libraries/string.htm string.s7i] library contains
more string functions.


## Smalltalk

Smalltalk strings are variable length and unrestricted. They are builtin and no additional library is req'd.


```Smalltalk
s := "abc"             # create a string (immutable if its a literal constant in the program)
s := #[16r01 16r02 16r00 16r03] asString # strings can contain any value, even nulls
s := String new:3.     # a mutable string
v := s                 # assignment
s = t                  # same contents
s < t                  # less
s <= t                 # less or equal
s = ''                 # equal empty string
s isEmpty              # ditto
s size                 # string length
t := s copy            # a copy
t := s copyFrom:2 to:3 # a substring
t := s copyReplaceFrom:2 to:3 with:'**'  # a copy with some replacements
s replaceFrom:2 to:3 with:'**'           # inplace replace (must be mutable)
s replaceAll:$x with:$y                  # destructive replace of characters
s copyReplaceAll:$x with:$y              # non-destructive replace
s replaceString:s1 withString:s2         # substring replace
s3 := s1 , s2          # concatenation of strings
s2 := s1 , $x          # append a character
s2 := s1 , 123 asCharacter # append an arbitrary byte
s := 'Hello / 今日は'   # they support unicode (at least up to 16rFFFF, some more)
```


In addition (because they inherit from collection), a lot more is inherited (map, fold, enumeration, finding substrings, etc.)


## Tcl

Tcl strings are binary safe, and a binary string is any string that only contains UNICODE characters in the range <tt>\u0000</tt>–<tt>\u00FF</tt>.

```tcl
# string creation
set x "hello world"

# string destruction
unset x

# string assignment with a null byte
set x a\0b
string length $x ;# ==> 3

# string comparison
if {$x eq "hello"} {puts equal} else {puts "not equal"}
set y bc
if {$x < $y} {puts "$x is lexicographically less than $y"}

# string copying; cloning happens automatically behind the scenes
set xx $x

# check if empty
if {$x eq ""} {puts "is empty"}
if {[string length $x] == 0} {puts "is empty"}

# append a byte
append x \07

# substring
set xx [string range $x 0 end-1]

# replace bytes
set y [string map {l L} "hello world"]

# join strings
set a "hel"
set b "lo w"
set c "orld"
set d $a$b$c
```



## VBA

Before start, see this link :
https://msdn.microsoft.com/fr-fr/VBA/Language-Reference-VBA/articles/option-compare-statement :

```txt
The Option Compare instruction is used at module level to declare the default comparison method to use when string data is compared.
The default text comparison method is Binary.

```


```vb

'Set the string comparison method to Binary.
Option Compare Binary ' That is, "AAA" is less than "aaa".
' Set the string comparison method to Text.
Option Compare Text ' That is, "AAA" is equal to "aaa".
```

String creation and destruction :

```vb

Sub Creation_String_FirstWay()
Dim myString As String
    'Here, myString is created and equal ""
    
End Sub '==> Here the string is destructed !
```

String assignment :

```vb
Sub String_Assignment()
Dim myString$
    'Here, myString is created and equal ""
    
    'assignments :
    myString = vbNullString     'return : ""
    myString = "Hello World"    'return : "Hello World"
    myString = String(12, "A")  'return : "AAAAAAAAAAAA"
End Sub
```

String comparison :

```vb
Sub String_Comparison_FirstWay()
Dim A$, B$, C$

    If A = B Then Debug.Print "A = B"
    
    A = "creation": B = "destruction": C = "CREATION"
    
    'test equality : (operator =)
    If A = B Then
         Debug.Print A & " = " & B
    'used to Sort : (operator < and >)
    ElseIf A > B Then
         Debug.Print A & " > " & B
    Else 'here : A < B
         Debug.Print A & " < " & B
    End If
    
    'test if A is different from C
    If A <> C Then Debug.Print A & " and " & B & " are differents."
    'same test without case-sensitive
    If UCase(A) = UCase(C) Then Debug.Print A & " = " & C & " (no case-sensitive)"
    
    'operator Like :
    If A Like "*ation" Then Debug.Print A & " Like *ation"
    If Not B Like "*ation" Then Debug.Print B & " Not Like *ation"
    'See Also : 
'https://docs.microsoft.com/en-us/dotnet/visual-basic/language-reference/operators/like-operator
End Sub
```

String cloning and copying :

```vb
Sub String_Clone_Copy()
Dim A As String, B$
    A = "Hello world!"
    'cloning :
    B = A
End Sub
```

Check if a string is empty :

```vb
Sub Check_Is_Empty()
Dim A As String, B As Variant

    Debug.Print IsEmpty(A)          'return False
    Debug.Print IsEmpty(Null)       'return False
    Debug.Print IsEmpty(B)          'return True ==> B is a Variant

    Debug.Print A = vbNullString    'return True
    Debug.Print StrPtr(A)           'return 0 (zero)
    
    'Press the OK button without enter a data in the InputBox :
    A = InputBox("Enter your own String : ")
    Debug.Print A = ""              'return True
    Debug.Print IsEmpty(A)          'return False
    Debug.Print StrPtr(A) = 0       'return False
    
    'Press the cancel button (with or without enter a data in the InputBox)
    A = InputBox("Enter your own String : ")
    Debug.Print StrPtr(A) = 0       'return True
    Debug.Print IsEmpty(A)          'return False
    Debug.Print A = ""              'return True
    'Note : StrPtr is the only way to know if you cancel the inputbox
End Sub
```

Append a byte to a string :

```vb
Sub Append_to_string()
Dim A As String
    A = "Hello worl"
    Debug.Print A & Chr(100)        'return : Hello world
End Sub
```

Extract a substring from a string :

```vb
Sub ExtractFromString()
Dim A$, B As String
    A = "Hello world"
    B = Mid(A, 3, 8)
    Debug.Print B                   'return : llo worl
End Sub
```

Replace every occurrence of a byte (or a string) in a string with another string :

```vb
Sub ReplaceInString()
Dim A$, B As String, C$
    A = "Hello world"
    B = Chr(108)        ' "l"
    C = " "
    Debug.Print Replace(A, B, C)    'return : He  o wor d
End Sub
```

Join Strings :

```vb
Sub Join_Strings()
Dim A$, B As String

    A = "Hello"
    B = "world"
    Debug.Print A & " " & B         'return : Hello world
    'If you're sure that A and B are Strings, you can use + instead of & :
    Debug.Print A + " " + B         'return : Hello world
End Sub

```



## zkl

Strings in zkl are bytes with a zero terminator. So they are binary safe as long as the data doesn't contain a zero. Data objects are true binary strings: bytes with a length. It is basically a binary editor. Datas come in two flavors: Int and String. The difference is some methods treat String type as containing (zero or more) null terminated strings. Datas have an implicit null terminator so they can easily convert to a String.

```zkl
Data(0,.Int,1,2,3) // bytes
Data(0,String,1,2,3) // same
Data(0,Int,"foo","bar") //-->foobar\0
d:=Data(0,String,"foo","bar") //-->foo\0bar\0\0
d==d // -->True: byte by byte comparison
d.copy()  //-->clone
d.len()  //-->8, 0 if empty

d.append("1").len(); //-->10  // or d+"1"
Data(0,Int,"foo","bar").len() //-->6
Data(0,Int,"foo","bar").append("1").len() //-->7

d.readString(4)    //-->"bar"
d.readNthString(2) //-->"1"
d[2,4]             //-->"o", really "o\0ba" but String sees the null

while(Void!=(n:=d.findString("bar"))){ d[n,4]="hoho" } 
d.bytes() //-->L(102,111,111,0,104,111,104,111,0,49,0)

d2:=Data(0,Int,"sam");
d.append(d2).text // or d+d2
```


{{omit from|GUISS}}
{{omit from|Lotus 123 Macro Scripting}}
