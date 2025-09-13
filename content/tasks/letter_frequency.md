+++
title = "Letter frequency"
description = ""
date = 2019-09-03T12:21:27Z
aliases = []
[extra]
id = 10543
[taxonomies]
categories = ["task", "Probability_and_statistics"]
tags = []
+++

## Task

Open a text file and count the occurrences of each letter.

Some of these programs count all characters (including punctuation),
but some only count letters A to Z.





## ACL2


```Lisp
(defun increment-alist (tbl key)
   (cond ((endp tbl) (list (cons key 1)))
         ((eql (car (first tbl)) key)
          (cons (cons key (1+ (cdr (first tbl))))
                (rest tbl)))
         (t (cons (first tbl)
                  (increment-alist (rest tbl) key)))))

(defun freq-table (xs)
   (if (endp xs)
       nil
       (increment-alist (freq-table (rest xs))
                        (first xs))))

(defun letter-freq (str)
   (freq-table (coerce str 'list)))
```



## Ada



```Ada
with Ada.Text_IO;

procedure Letter_Frequency is
   Counters: array (Character) of Natural := (others => 0); -- initialize all Counters to 0
   C:        Character;
   File:     Ada.Text_IO.File_Type;

begin
   Ada.Text_IO.Open(File, Mode => Ada.Text_IO.In_File, Name => "letter_frequency.adb");
   while not Ada.Text_IO.End_Of_File(File) loop
      Ada.Text_IO.Get(File, C);
      Counters(C) := Counters(C) + 1;
   end loop;

   for I in Counters'Range loop
      if Counters(I) > 0 then
            Ada.Text_IO.Put_Line("'" & I & "':" & Integer'Image(Counters(I)));
      end if;
   end loop;
end Letter_Frequency;
```


{{out}} (counting the characters of its own source code):

```txt
>./letter_frequency
' ': 122
'"': 6
'&': 3

... [a lot of lines omitted]

'x': 7
'y': 5
'z': 1
```



## Aikido


```aikido
import ctype

var letters = new int [26]

var s = openin (args[0])
while (!s.eof()) {
    var ch = s.getchar()
    if (s.eof()) {
        break
    }
    if (ctype.isalpha (ch)) {
        var n = cast<int>(ctype.tolower(ch) - 'a')
        ++letters[n]
    }
}

foreach i letters.size() {
    println (cast<char>('a' + i) + " " + letters[i])
}
```



## Aime

Letters proper:

```aime
file f;
index x;
integer c;

f.affix("unixdict.txt");

while ((c = f.pick) ^ -1) {
    x[c] += 1;
}

c = 'A';
while (c <= 'Z') {
    o_form("%c: /w5/\n", c, x[c] += x[c + 'a' - 'A'] += 0);
    c += 1;
}
```

All chars:

```aime
file f;
index x;
integer c, n;

f.affix("unixdict.txt");

while ((c = f.pick) ^ -1) {
    x[c] += 1;
}

for (c, n in x) {
    o_form("%c: /w5/\n", c, n);
}
```



## ALGOL 68


```algol68

BEGIN
   [0:max abs char]INT histogram;
   FOR i FROM 0 TO max abs char DO histogram[i] := 0 OD;
   FILE input file;
   STRING input file name = "Letter_frequency.a68";
   IF open (input file, input file name, stand in channel) /= 0 THEN
      put (stand error, ("Cannot open ", input file name, newline));
      stop
   ELSE
      on file end (input file, (REF FILE f) BOOL: (close (f); GOTO finished))
   FI;
   DO
      STRING s;
      get (input file, (s, newline));
      FOR i TO UPB s DO
	 CHAR c = s[i];
	 IF "A" <= c AND c <= "Z" OR "a" <= c AND c <= "z" THEN
	    histogram[ABS c] PLUSAB 1
	 FI
     OD
   OD;
   close (input file);
finished:
   FOR i FROM ABS "A" TO ABS "Z" DO printf (($a3xg(0)l$, REPR i, histogram[i])) OD;
   FOR i FROM ABS "a" TO ABS "z" DO printf (($a3xg(0)l$, REPR i, histogram[i])) OD
END

```

{{out}} Counting letters in its own source code:

```txt

A   11
B   9
C   2
D   13
E   11
F   14
G   4
H   3
I   10
J   0
[[ Omitted for K – Z and a – p ]]
q   1
r   15
s   19
t   24
u   10
v   0
w   3
x   4
y   1
z   2

```



## APL


```apl

      freq←{(⍪∪⍵),+/(∪⍵)∘.⍷⍵}

      freq 0 1 2 3 2 3 4 3 4 4 4
0 1
1 1
2 2
3 3
4 4

      freq 'balloon'
b 1
a 1
l 2
o 2
n 1

```



## AutoHotkey


```AutoHotkey
OpenFile = %A_ScriptFullPath% ; use own source code
FileRead, FileText, %OpenFile%
Loop 26
{
	StringReplace, junk, FileText, % Chr(96+A_Index),, UseErrorLevel
	out .= Chr(96+A_Index) ": " ErrorLevel "`n"
}
MsgBox % out
```

{{out}} (using script's own file):

```txt

a: 6
b: 1
c: 6
d: 4
e: 24
[several lines omitted]
x: 5
y: 0
z: 0
```



## AutoIt


This function prints the Letter frequency of a given textfile.
You can choose to use case sensitive search and if special chars should be searched too.

<lang>
Func _Letter_frequency($Path, $fcase = True, $fspecial_chars = True)
	Local $hFile, $sRead, $iupto, $iStart, $iCount
	If Not $fcase Then $fcase = False
	If Not $fspecial_chars Then
		$iStart = 64
		If Not $fcase Then
			$iupto = 26
		Else
			$iupto = 58
		EndIf
	Else
		$iStart = 31
		$iupto = 224
	EndIf
	$hFile = FileOpen($Path, 0)
	$sRead = FileRead($hFile)
	FileClose($hFile)
	For $i = 1 To $iupto
		If Not $fspecial_chars Then
			If $iStart + $i > 90 And $iStart + $i < 97 Then ContinueLoop
		EndIf
		$sRead = StringReplace($sRead, Chr($iStart + $i), "", 0, $fcase)
		$iCount = @extended
		If $iCount > 0 Then ConsoleWrite(Chr($iStart + $i) & " : " & $iCount & @CRLF)
	Next
EndFunc   ;==>_Letter_frequency
```


```txt

A : 32
B : 2
C : 15
E : 31
F : 10
[several lines omitted]
u : 14
v : 1
w : 1
x : 14
```



## AWK


```AWK

# usage: awk -f letters.awk HolyBible.txt

BEGIN { FS="" }
      { for(i=1;i<=NF;i++) m[$i]++}
END   { for(i in m) printf("%9d %-14s\n", m[i],i) }

```



## BaCon


```freebasic
txt$ = LOAD$("bible.txt")

FOR x = 97 TO 122
    PRINT CHR$(x-32), " ", CHR$(x), " : ", COUNT(txt$, x-32), " - ", COUNT(txt$, x)
NEXT

```

```txt

A a : 17915 - 257815
B b : 4714 - 44161
C c : 1698 - 53373
D d : 8782 - 149313
E e : 2710 - 409525
F f : 2386 - 81157
G g : 6206 - 49096
H h : 3208 - 279471
I i : 13302 - 180660
J j : 6374 - 2515
K k : 547 - 21745
L l : 9222 - 120716
M m : 3056 - 76884
N n : 1891 - 223166
O o : 8896 - 234290
P p : 1877 - 41377
Q q : 6 - 958
R r : 7568 - 162761
S s : 4906 - 185124
T t : 7763 - 309983
U u : 333 - 83140
V v : 107 - 30258
W w : 2408 - 63079
X x : 2 - 1476
Y y : 569 - 58007
Z z : 904 - 2068

```



## BBC BASIC


```bbcbasic
      DIM cnt%(255)

      file% = OPENIN("C:\unixdict.txt")
      IF file%=0 ERROR 100, "Could not open file"

      REPEAT
        A$ = GET$#file%
        L% = LEN(A$)
        IF L% THEN
          FOR I% = 1 TO L%
            cnt%(ASCMID$(A$,I%)) += 1
          NEXT
        ENDIF
      UNTIL EOF#file%
      CLOSE #file%

      FOR c% = &41 TO &5A
        PRINT CHR$(c%)CHR$(c%+32) ": " cnt%(c%)+cnt%(c%+32)
      NEXT
```

```txt

Aa:      16421
Bb:       4115
Cc:       8216
Dd:       5799
Ee:      20144
Ff:       2662
Gg:       4129
Hh:       5208
Ii:      13980
Jj:        430
Kk:       1925
Ll:      10061
Mm:       5828
Nn:      12097
Oo:      12738
Pp:       5516
Qq:        378
Rr:      13436
Ss:      10210
Tt:      12836
Uu:       6489
Vv:       1902
Ww:       1968
Xx:        617
Yy:       3633
Zz:        433

```



## Bracmat


```bracmat
(lc=
  counts c
.     fil$(!arg,r)                        {open file for reading}
    & 0:?counts
    &   whl
      ' ( fil$:?c                         {read a byte}
        &     ( !c:(~<A:~>Z|~<a:~>z)
              | 0
              )
            + !counts
          : ?counts                       {simply add any found letter to the sum}
        )
    & fil$(,SET,-1)                       {close the file by seeking to impossible file position.}
  | !counts                               {return the sum}
);

lc$"valid.bra"                            {example: count letters in Bracmat's validation suite.}

```


```bracmat
107*A
+ 33*B
+ 37*C
+ 39*D
+ 74*E
+ 50*F
+ 27*G
+ 28*H
+ 20*I
+ 55*J
+ 32*K
+ 112*L
+ 36*M
+ 32*N
+ 621*O
+ 43*P
+ 25*R
+ 67*S
+ 62*T
+ 64*U
+ 5*V
+ 26*W
+ 353*X
+ 248*Y
+ 70*Z
+ 2173*a
+ 840*b
+ 738*c
+ 639*d
+ 1345*e
+ 472*f
+ 372*g
+ 568*h
+ 91*j
+ 142*k
+ 529*l
+ 409*m
+ 941*n
+ 840*o
+ 336*p
+ 65*q
+ 993*r
+ 1018*s
+ 2097*t
+ 978*u
+ 122*v
+ 156*w
+ 909*x
+ 685*y
+ 211*z
+ 1035*i
```



## C


```c
/* declare array */
int frequency[26];
int ch;
FILE* txt_file = fopen ("a_text_file.txt", "rt");

/* init the freq table: */
for (ch = 0; ch < 26; ch++)
    frequency[ch] = 0;

while (1) {
    ch = fgetc(txt_file);
    if (ch == EOF) break; /* end of file or read error.  EOF is typically -1 */

    /* assuming ASCII; "letters" means "a to z" */
    if ('a' <= ch && ch <= 'z')      /* lower case */
        frequency[ch-'a']++;
    else if ('A' <= ch && ch <= 'Z') /* upper case */
        frequency[ch-'A']++;
}
```



## C#


```c#
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

class Program
{
    static SortedDictionary<TItem, int> GetFrequencies<TItem>(IEnumerable<TItem> items)
    {
        var dictionary = new SortedDictionary<TItem, int>();
        foreach (var item in items)
        {
            if (dictionary.ContainsKey(item))
            {
                dictionary[item]++;
            }
            else
            {
                dictionary[item] = 1;
            }
        }
        return dictionary;
    }

    static void Main(string[] arguments)
    {
        var file = arguments.FirstOrDefault();
        if (File.Exists(file))
        {
            var text = File.ReadAllText(file);
            foreach (var entry in GetFrequencies(text))
            {
                Console.WriteLine("{0}: {1}", entry.Key, entry.Value);
            }
        }
    }
}
```

```txt
 : 1
!: 1
,: 1
H: 1
d: 1
e: 1
l: 3
o: 2
r: 1
w: 1
```


Declarative approach:


```c#

var freq =  from c in str
            where char.IsLetter(c)
            orderby c
            group c by c into g
            select g.Key + ":" + g.Count();

foreach(var g in freq)
    Console.WriteLine(g);

```


```txt

C:2
I:1
K:1
L:2
W:1
a:4
...
y:2

```



## C++


```cpp
#include <fstream>
#include <iostream>

int main()
{
	std::ifstream input("filename.txt", std::ios_base::binary);
	if (!input)
	{
		std::cerr << "error: can't open file\n";
		return -1;
	}

	size_t count[256];
	std::fill_n(count, 256, 0);

	for (char c; input.get(c); ++count[uint8_t(c)]) // process input file
		; // empty loop body

	for (size_t i = 0; i < 256; ++i)
	{
		if (count[i] && isgraph(i)) // non-zero counts of printable characters
		{
			std::cout << char(i) << " = " << count[i] << '\n';
		}
	}
}
```

{{out}} when file contains "Hello, world!" (without quotes):

```txt

! = 1
, = 1
H = 1
d = 1
e = 1
l = 3
o = 2
r = 1
w = 1

```



## Common Lisp


```lisp
(defun letter-freq (file)
  (with-open-file (stream file)
    (let ((str (make-string (file-length stream)))
	  (arr (make-array 256 :element-type 'integer :initial-element 0)))
      (read-sequence str stream)
      (loop for c across str do (incf (aref arr (char-code c))))
      (loop for c from 32 to 126 for i from 1 do
	    (format t "~c: ~d~a"
		    (code-char c) (aref arr c)
		    (if (zerop (rem i 8)) #\newline #\tab))))))

(letter-freq "test.lisp")
```



## Clojure


```Clojure
(println (sort-by second >
			(frequencies (map #(java.lang.Character/toUpperCase %)
					  (filter #(java.lang.Character/isLetter %) (slurp "text.txt"))))))
```



## Component Pascal

BlackBox Component Builder

```oberon2

MODULE LetterFrecuency;
IMPORT Files,StdLog,Strings;

PROCEDURE Do*;
VAR
	loc: Files.Locator;
	fd: Files.File;
	rd: Files.Reader;
	x: BYTE;
	frecuency: ARRAY 26 OF LONGINT;
	c: CHAR;
	i: INTEGER;
BEGIN
	loc := Files.dir.This("BBTest/Mod");
	fd := Files.dir.Old(loc,"LetterFrecuency.odc",FALSE);
	rd := fd.NewReader(NIL);

	(* init the frecuency array *)
	FOR i := 0 TO LEN(frecuency) - 1 DO frecuency[i] := 0 END;

	(* collect frecuencies *)
	WHILE ~rd.eof DO
		rd.ReadByte(x);c := CAP(CHR(x));
		(* convert vowels with diacritics *)
		CASE ORD(c) OF
			 193: c := 'A';
			|201: c := 'E';
			|205: c := 'I';
			|211: c := 'O';
			|218: c := 'U';
			ELSE
		END;
		IF (c >= 'A') & (c <= 'Z') THEN
			INC(frecuency[ORD(c) - ORD('A')]);
		END
	END;

	(* show data *)
	FOR i := 0 TO LEN(frecuency) - 1 DO
		StdLog.Char(CHR(i + ORD('A')));StdLog.String(":> ");StdLog.Int(frecuency[i]);
		StdLog.Ln
	END
END Do;
END LetterFrecuency.

```

Execute: ^Q LetterFrecuency.Do<br/>
```txt

A:>  28
B:>  7
C:>  100
D:>  94
E:>  168
F:>  30
G:>  10
H:>  11
I:>  49
J:>  0
K:>  1
L:>  67
M:>  25
N:>  57
O:>  81
P:>  3
Q:>  0
R:>  91
S:>  90
T:>  94
U:>  32
V:>  14
W:>  15
X:>  15
Y:>  17
Z:>  3

```



## D


```d
void main() {
    import std.stdio, std.ascii, std.algorithm, std.range;

    uint[26] frequency;

    foreach (const buffer; "unixdict.txt".File.byChunk(2 ^^ 15))
        foreach (immutable c; buffer.filter!isAlpha)
            frequency[c.toLower - 'a']++;

    writefln("%(%(%s, %),\n%)", frequency[].chunks(10));
}
```

```txt
16421, 4115, 8216, 5799, 20144, 2662, 4129, 5208, 13980, 430,
1925, 10061, 5828, 12097, 12738, 5516, 378, 13436, 10210, 12836,
6489, 1902, 1968, 617, 3633, 433
```



## EchoLisp

We use a property list - plist for short - which is a hash table, to store the pairs ( letter . count) .

```lisp

;; bump count when letter added
(define (hash-counter hash key )
		;; (set! key (string-downcase key)) - if ignore case wanted
		(putprop hash (1+ (or (getprop hash key) 0 )) key))

;;  apply to exploded string
;;  and sort result
(define (hash-compare a b) ( < (first a) (first b)))
(define (count-letters hash string)
		(map (curry hash-counter hash) (string->list string))
		(list-sort hash-compare (symbol-plist hash)))

```

```lisp

(define (file-stats file string)
	(set-plist! 'file-stats null) ; reset counters
	(writeln (count-letters 'file-stats string))
	(writeln "Total letters:" (string-length string))
	(writeln "Total lines:" (getprop 'file-stats "#\\newline")))

; frequency for 'help.html' file
(file->string file-stats) ; browser 'open' dialog

➛ help.html -> string
➛ (( 28918) (! 138) (# 1035) (#\newline 4539) (#\tab 409) ($ 7) (% 24) (& 136) (' 1643) ((3577) () 3583) (* 233)
 (+ 303) (, 599) (- 3164) (. 1454) (/ 5388) (0 1567) (1 1769) (2 1258) (3 857) (4 1872) (5 453) (6 581) (7 344)
 (8 337) (9 411) (: 1235) (; 647) (< 9951) (= 1834) (> 10255) (? 392) (@ 11) (A 166) (B 92) (C 144) (D 72) (E 224)
 (F 52) (G 35) (H 42) (I 193) (J 31) (K 36) (L 196) (M 82) (N 94) (O 132) (P 192) (Q 27) (R 56) (S 220) (T 226) (U 37)
 (V 51) (W 28) (X 6) (Y 38) (Z 2) ([ 237) (\ 12) (] 215) (^ 28) (_ 107) (` 7) (a 8420) (b 4437) (c 3879) (d 4201)
 (e 11905) (f 2989) (g 2068) (h 3856) (i 11313) (j 334) (k 653) (l 5748) (m 3048) (n 7020) (o 7207) (p 3585) (q 249)
 (r 8312) (s 8284) (t 8704) (u 3833) (v 1135) (w 861) (x 1172) (y 1451) (z 268) ({ 123) (| 62) (} 123) (~ 7) (§ 1) (© 1)
 (« 1) (» 1) (É 2) (à 18) (â 3) (ç 3) (è 6) (é 53) (î 1) (ö 9) (û 1) (œ 1) (ε 2) (λ 12) (μ 1) (ο 2) (ς 1)
 (τ 1) (а 1) (д 1) (е 1) (з 1) (л 1) (м 1) (н 1) (я 3) (ἄ 1) (— 2) (“ 2) (” 2) (… 184) (→ 465) (∅ 57) (∈ 4) (∏ 1)
 (∑ 2) (∘ 6) (√ 4)(∞ 12) (∫ 2) (⌚ 2) (⌛ 1) (⏳ 4) (☕ 1) (♠ 7) (♡ 2) (♢ 2) (♣ 6) (♤ 2) (♥ 8) (♦ 8)
 (♧ 2) (⚁ 1) (⚃ 2) (⚪ 1) (⛔ 1) (✋ 1) (❄ 1) (❅ 1) (❆ 1) (❇ 1) (❈ 1) (❉ 1) (❊ 1) (❋ 1) (❌ 3) (❍ 1)
 (❎ 1) (❗ 1) (➛ 900) (➰ 1) (⭕ 2) ... )
➛ Total letters:     212631
➛ Total lines:     4539

```



## Eiffel



```Eiffel
class
	APPLICATION

create
	make

feature {NONE} -- Initialization

	make
			-- Read from the file and print frequencies.
		local
			file: PLAIN_TEXT_FILE
		do
			create file.make_open_read("input.txt")
			file.read_stream(file.count)
			file.close
			across get_frequencies(file.last_string) as f loop
				print(f.key.out + ": " + f.item.out + "%N")
			end
		end

feature -- Access

	get_frequencies (s: STRING): HASH_TABLE[INTEGER, CHARACTER]
			-- Hash table of counts for alphabetic characters in `s'.
		local
			char: CHARACTER
		do
			create Result.make(0)
			across s.area as st loop
				char := st.item
				if char.is_alpha then
					if Result.has(char) then
						Result.force(Result.at(char) + 1, char)
					else
						Result.put (1, char)
					end
				end
			end
		end
end
```

{{out}} when file contains "Hello, Eiffel world!":

```txt
H: 1
e: 2
l: 4
o: 2
E: 1
i: 1
f: 2
w: 1
r: 1
d: 1
```



## Elixir


```elixir
file = hd(System.argv)

File.read!(file)
|> String.upcase
|> String.graphemes
|> Enum.filter(fn c -> c =~ ~r/[A-Z]/ end)
|> Enum.reduce(Map.new, fn c,acc -> Map.update(acc, c, 1, &(&1+1)) end)
|> Enum.sort_by(fn {_k,v} -> -v end)
|> Enum.each(fn {k,v} -> IO.puts "#{k}  #{v}" end)
```


```txt

C:\Elixir>elixir letterfrequency.exs \work\unixdict.txt
E  20144
A  16421
I  13980
R  13436
T  12836
O  12738
N  12097
S  10210
L  10061
C  8216
U  6489
M  5828
D  5799
P  5516
H  5208
G  4129
B  4115
Y  3633
F  2662
W  1968
K  1925
V  1902
X  617
Z  433
J  430
Q  378

```



## Erlang


```erlang
%% Implemented by Arjun Sunel
-module(letter_frequency).
-export([main/0, letter_freq/1]).
main() ->
	case  file:read_file("file.txt") of
		{ok, FileData} ->
			letter_freq(binary_to_list(FileData));
		_FileNotExist ->
			io:format("File do not exist~n")
	end.

letter_freq(Data) ->
	lists:foreach(fun(Char) ->
					LetterCount = lists:foldl(fun(Element, Count) ->
											case Element =:= Char of
												true ->
													Count+1;
												false ->
													Count
											end
										end, 0, Data),

					case LetterCount >0 of
						true ->
							io:format("~p	:	~p~n", [[Char], LetterCount]);
						false ->
							io:format("")
					end
				end,  lists:seq(0, 222)).

```

```txt
"\n"    :       5
" "     :       4
","     :       1
"."     :       22
":"     :       3
"M"     :       1
"a"     :       2
"e"     :       2
"i"     :       1
"j"     :       1
"l"     :       1
"m"     :       1
"n"     :       3
"r"     :       1
"s"     :       2
"u"     :       2
"y"     :       1
"}"     :       2
ok

```


Alternatively letter_freq/1 above can be replaced with

```Erlang

letter_freq( Data ) ->
	Dict = lists:foldl( fun (Char, Dict) -> dict:update_counter( Char, 1, Dict ) end, dict:new(), Data ),
	[io:fwrite( "~p	:	~p~n", [[X], dict:fetch(X, Dict)]) || X <- dict:fetch_keys(Dict)].

```




## ERRE

Using ERRE help file for testing.

```ERRE
PROGRAM LETTER

DIM CNT[255]

BEGIN

      OPEN("I",1,"f:\errev30\erre.hlp")

      REPEAT
        GET(#1,A$)
        L%=LEN(A$)
        IF L%>0 THEN
          FOR I%=1 TO L% DO
            A%=ASC(MID$(A$,I%))
            CNT[A%]+=1
          END FOR
        END IF
      UNTIL EOF(1)
      CLOSE(1)

      FOR C%=$41 TO $5A DO
        PRINT(CHR$(C%);CHR$(C%+32);": ";CNT[C%]+CNT[C%+32])
      END FOR

END PROGRAM

```



## Euphoria

```euphoria

-- LetterFrequency.ex
-- Count frequency of each letter in own source code.

include std/console.e
include std/io.e
include std/text.e

sequence letters = repeat(0,26)

sequence content = read_file("LetterFrequency.ex")

content = lower(content)

for i = 1 to length(content) do
	if content[i] > 96 and content[i] < 123 then
		letters[content[i]-96] += 1
	end if
end for

for i = 1 to 26 do
	printf(1,"%s:  %d\n",{i+96,letters[i]})
end for

if getc(0) then end if

```

```txt

a: 4
b: 0
c: 21
-snip
x: 3
y: 3
z: 0

```


=={{header|F_Sharp|F#}}==

```fsharp
let alphabet =
    ['A'..'Z'] |> Set.ofList

let letterFreq (text : string) =
    text.ToUpper().ToCharArray()
    |> Array.filter (fun x -> alphabet.Contains(x))
    |> Seq.countBy (fun x -> x)
    |> Seq.sort

let v = "Now is the time for all good men to come to the aid of the party"

let res = letterFreq v

for (letter, freq) in res do
    printfn "%A, %A" letter freq
```



## FBSL

The result of the first evaluation of ASC() is retained in the symbol ASC for later use. This is a standard feature of FBSL functions. The ascii array is dynamic. Command(1) is the name of the script file.


```qbasic
#APPTYPE CONSOLE

'Open a text file and count the occurrences of each letter.
FUNCTION countBytes(fileName AS STRING)
	DIM c AS STRING
	DIM ascii[]
	DIM handle AS INTEGER = FILEOPEN(fileName, BINARY)
	WHILE NOT FILEEOF(handle)
		c = FILEGETC(handle)
		IF c = "" THEN EXIT WHILE
		ascii[ASC] = ascii[ASC(c)] + 1
	WEND
	FILECLOSE(handle)
	RETURN ascii
END SUB

DIM counters = countBytes(COMMAND(1))
FOR DIM i = LBOUND(counters) TO UBOUND(counters)
	PRINT i, TAB, IIF(i <= 32, i, CHR(i)), TAB, counters[i]
NEXT

PAUSE

```



## Factor


```factor
USING: hashtables locals io assocs kernel io.encodings.utf8 io.files formatting ;
IN: count-letters

<PRIVATE

: count-from-stream ( -- counts )
  52 <hashtable>
  [ read1 dup ] [ over inc-at ] while
  drop ;

: print-counts ( counts -- )
  [ "%c: %d\n" printf ] assoc-each ;

PRIVATE>

: count-letters ( filename -- )
  utf8 [ count-from-stream ] with-file-reader
    print-counts ;

```



## Forth


```forth
create counts 26 cells allot

: freq ( filename -- )
  counts 26 cells erase
  slurp-file bounds do
    i c@ 32 or 'a -
    dup 0 26 within if
      cells counts +
      1 swap +!
    else drop then
  loop
  26 0 do
    cr [char] ' emit  'a i + emit  ." ': "
    counts i cells + @ .
  loop ;

s" example.txt" freq
```



## Fortran

Using the configuration file (which has changed since the example was documented) of the J example, compilation and output of this program on a gnu/linux system is

```fortran

-*- mode: compilation; default-directory: "/tmp/" -*-
Compilation started at Sat May 18 18:09:46

a=./F && make $a && $a < configuration.file
f95 -Wall -ffree-form F.F -o F
          92          21          17          24          82          19          19          22          67           0           2          27          27          57          55          31           1          61          43          60          20           6           2           0          10           0

Compilation finished at Sat May 18 18:09:46

```

And here's the FORTRAN90 program source.  The program reads stdin and writes the result to stdout.  Future enhancement: use block size records.

```FORTRAN

! count letters from stdin
program LetterFrequency
  implicit none
  character (len=1) :: s
  integer, dimension(26) :: a
  integer :: ios, i, t
  data a/26*0/,i/0/
  open(unit=7, file='/dev/stdin', access='direct', form='formatted', recl=1, status='old', iostat=ios)
  if (ios .ne. 0) then
    write(0,*)'Opening stdin failed'
    stop
  endif
  do i=1, huge(i)
    read(unit=7, rec = i, fmt = '(a)', iostat = ios ) s
    if (ios .ne. 0) then
      !write(0,*)'ios on failure is ',ios
      close(unit=7)
      exit
    endif
    t = ior(iachar(s(1:1)), 32) - iachar('a')
    if ((0 .le. t) .and. (t .le. iachar('z'))) then
      t = t+1
      a(t) = a(t) + 1
    endif
  end do
  write(6, *) a
end program LetterFrequency

```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Dim a(65 to 90) As Integer  ' array to hold frequency of each letter, all elements zero initially
Dim fileName As String = "input.txt"
Dim s As String
Dim i As Integer
Open fileName For Input As #1

While Not Eof(1)
  Line Input #1, s
  s = UCase(s)
  For i = 0 To Len(s) - 1
    a(s[i]) += 1
  Next
Wend

Close #1

Print "The frequency of each letter in the file "; fileName; " is as follows:"
Print
For i = 65 To 90
  If a(i) > 0 Then
    Print Chr(i); " : "; a(i)
  End If
Next
Print
Print "Press any key to quit"
Sleep
```


```txt

/'
   results for input.txt which contains the single line:
   The quick brown fox jumps over the lazy dog.
'/

The frequency of each letter in the file input.txt is as follows:

A :  1
B :  1
C :  1
D :  1
E :  3
F :  1
G :  1
H :  2
I :  1
J :  1
K :  1
L :  1
M :  1
N :  1
O :  4
P :  1
Q :  1
R :  2
S :  1
T :  2
U :  2
V :  1
W :  1
X :  1
Y :  1
Z :  1

```


Input:

```txt

This is the one question that most people ask. Why bother learning a completely different computing environment, when the operating
system that ships with most desktops, laptops, and servers works just fine? To answer that question, I would pose another question.
Does that operating system you’re currently using really work “just fine”? Or are you constantly battling viruses, malware, slow
downs, crashes, costly repairs, and licensing fees?

If you struggle with the above, and want to free yourself from the constant fear of losing data or having to take your computer in
for the “yearly clean up,” Linux might be the perfect platform for you. Linux has evolved into one of the most reliable computer
ecosystems on the planet. Combine that reliability with zero cost of entry and you have the perfect solution for a desktop platform.

```


## Gambas


```gambas
Public Sub Form_Open()
Dim sData As String = File.Load("data.txt")
Dim iCount, iSpaces, iLetters, iOther As Integer
Dim bPunctuation As Boolean

For iCount = 1 To Len(sData)
  If InStr("ABCDEFGHIJKLMNOPQRSTUVWXYZ", UCase(Mid(sData, iCount, 1))) Then
    Inc iLetters
    bPunctuation = True
  End If
   If Mid(sData, icount, 1) = " " Then
    Inc iSpaces
    bPunctuation = True
  End If
  If bPunctuation = False Then Inc iOther
  bPunctuation = False
Next

Message("Text contains " & Len(sData) & " characters\n" & iLetters & " Letters\n" & iSpaces & " Spaces\n" & iOther & " Punctuation, newlines etc.")

End
```

Output:

```txt

Text contains 854 characters
677 Letters
135 Spaces
42 Punctuation, newlines etc.

```



## Go


```go
package main

import (
    "fmt"
    "io/ioutil"
    "sort"
    "unicode"
)

const file = "unixdict.txt"

func main() {
    bs, err := ioutil.ReadFile(file)
    if err != nil {
        fmt.Println(err)
        return
    }
    m := make(map[rune]int)
    for _, r := range string(bs) {
        m[r]++
    }
    // answer is now in m.  sort and format output:
    lfs := make(lfList, 0, len(m))
    for l, f := range m {
        lfs = append(lfs, &letterFreq{l, f})
    }
    sort.Sort(lfs)
    fmt.Println("file:", file)
    fmt.Println("letter  frequency")
    for _, lf := range lfs {
        if unicode.IsGraphic(lf.rune) {
            fmt.Printf("   %c    %7d\n", lf.rune, lf.freq)
        } else {
            fmt.Printf("%U  %7d\n", lf.rune, lf.freq)
        }
    }
}

type letterFreq struct {
    rune
    freq int
}
type lfList []*letterFreq

func (lfs lfList) Len() int { return len(lfs) }
func (lfs lfList) Less(i, j int) bool {
    switch fd := lfs[i].freq - lfs[j].freq; {
    case fd < 0:
        return false
    case fd > 0:
        return true
    }
    return lfs[i].rune < lfs[j].rune
}
func (lfs lfList) Swap(i, j int) {
    lfs[i], lfs[j] = lfs[j], lfs[i]
}
```

```txt

file: unixdict.txt
letter  frequency
U+000A    25104
   e      20144
   a      16421
   i      13980
   r      13436
   t      12836
   o      12738
   n      12097
   s      10210
   l      10061
   c       8216
   u       6489
   m       5828
   d       5799
   p       5516
   h       5208
   g       4129
   b       4115
   y       3633
   f       2662
   w       1968
   k       1925
   v       1902
   x        617
   z        433
   j        430
   q        378
   '        105
   &          6
   .          6
   1          2
   0          1
   2          1
   3          1
   4          1
   5          1
   6          1
   7          1
   8          1
   9          1

```



## Groovy


```groovy
def frequency = { it.inject([:]) { map, value -> map[value] = (map[value] ?: 0) + 1; map } }

frequency(new File('frequency.groovy').text).each { key, value ->
    println "'$key': $value"
}
```

```txt
'd': 1
'e': 19
'f': 4
' ': 29
'r': 5
'q': 3
'u': 8
[lines omitted]
'o': 2
'x': 1
'h': 1
'k': 2
'"': 2
'$': 2
```



## Harbour


```visualfoxpro
PROCEDURE Main()
   LOCAL s := hb_MemoRead( Left( __FILE__ , At( ".", __FILE__ )) +"prg")
   LOCAL c, n, i
   LOCAL a := {}

   FOR EACH c IN s
      IF Asc( c ) > 31
         AAdd( a, c )
      ENDIF
   NEXT
   a := ASort( a )
   i := 1
   WHILE i <= Len( a )
      c := a[i] ; n := 1
      i++
      IF i < Len(a) .AND. c == a[i]
         WHILE c == a[i]
            n++ ; i++
         END
      ENDIF
      ?? "'" + c + "'" + "=" + hb_NtoS( n ) + " "
   END

   RETURN
```


{{Out}} (counting the printable characters of its own source code):

```txt

' '=190 '"'=12 ' ' '=2 '('=10 ')'=10 '+'=12 ','=5 '.'=3 '1'=3 '3'=1 ':'=6 ';'=2 '<'=2 '='=12
'>'=1 '?'=2 'A'=10 'C'=5 'D'=6 'E'=13 'F'=7 'H'=3 'I'=9 'L'=13 'M'=2 'N'=9 'O'=5 'P'=1
'R'=6 'S'=2 'T'=2 'U'=2 'W'=2 'X'=1 '['=3 ']'=3 '_'=10 'a'=12 'b'=2 'c'=9 'd'=3 'e'=5
'f'=1 'g'=1 'h'=2 'i'=11 'm'=1 'n'=7 'o'=3 'p'=1 'r'=2 's'=3 't'=5 'w'=1 '{'=1 '}'=1

```



## Haskell

Short version:

```Haskell
import Data.List (group,sort)
import Control.Arrow ((&&&))
main = interact (show . map (head &&& length) . group . sort)
```


=={{header|Icon}} and {{header|Unicon}}==
The example below counts (case insensitive) letters and was run on a version of this source file.

```Icon
link printf

procedure main(A)
every PrintCount(CountLetters(!A))
end

procedure CountLetters(fn)  #: Return case insensitive count of letters
   K := table(0)
   if f := open(fn,"r") then {
      every c := !map(|read(f)) do
         if any(&lcase,c) then K[c] +:= 1
      close(f)
      return K
      }
   else write(&errout,"Unable to open file ",fn)
end

procedure PrintCount(T)    #: Print the letters
every c := key(T) do
   printf("%s - %d\n",c,T[c])
end
```


[http://www.cs.arizona.edu/icon/library/src/procs/printf.icn printf.icn provides formatting]

```txt
c - 17
k - 5
s - 10
h - 2
p - 10
e - 41
m - 2
u - 12
b - 2
r - 25
o - 16
w - 1
d - 10
l - 10
t - 27
a - 10
i - 13
y - 5
f - 12
n - 28
v - 4
```


=={{header|IS-BASIC}}==
<lang IS-BASIC>100 PROGRAM "Letters.bas"
110 NUMERIC LETT(65 TO 90)
120 FOR I=65 TO 90
130   LET LETT(I)=0
140 NEXT
150 LET EOF=0
160 OPEN #1:"list.txt"
170 WHEN EXCEPTION USE IOERROR
180   DO
190     GET #1:A$
200     LET A$=UCASE$(A$)
210     IF A$>="A" AND A$<="Z" THEN LET LETT(ORD(A$))=LETT(ORD(A$))+1
220   LOOP UNTIL EOF
230 END WHEN
240 FOR I=65 TO 90
250   PRINT CHR$(I);":";LETT(I),
260 NEXT
270 HANDLER IOERROR
280   LET EOF=-1
290   CLOSE #1
300   CONTINUE
310 END HANDLER
```



## J


Input is a directory-path with filename. Result is 26 integers representing counts of each letter, in alphabetic order (a's count is first).


```j
ltrfreq=: 3 : 0
  letters=. u: 65 + i.26  NB. upper case letters
  <: #/.~ letters (, -. -.~) toupper fread y
)
```


Example use (based on [[Read_a_configuration_file|a configuration file from another task]]):


```j
   ltrfreq 'config.file'
88 17 17 24 79 18 19 19 66 0 2 26 26 57 54 31 1 53 43 59 19 6 2 0 8 0
```



## Java

```java5
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.Arrays;

public class LetterFreq {
	public static int[] countLetters(String filename) throws IOException{
		int[] freqs = new int[26];
		BufferedReader in = new BufferedReader(new FileReader(filename));
		String line;
		while((line = in.readLine()) != null){
			line = line.toUpperCase();
			for(char ch:line.toCharArray()){
				if(Character.isLetter(ch)){
					freqs[ch - 'A']++;
				}
			}
		}
		in.close();
		return freqs;
	}

	public static void main(String[] args) throws IOException{
		System.out.println(Arrays.toString(countLetters("filename.txt")));
	}
}
```

In Java 7, we can use try with resources. The <code>countLetters</code> method would look like this:

```java5
public static int[] countLetters(String filename) throws IOException{
	int[] freqs = new int[26];
	try(BufferedReader in = new BufferedReader(new FileReader(filename))){
		String line;
		while((line = in.readLine()) != null){
			line = line.toUpperCase();
			for(char ch:line.toCharArray()){
				if(Character.isLetter(ch)){
					freqs[ch - 'A']++;
				}
			}
		}
	}
	return freqs;
}
```

In Java 8, we can use streams.  This code also handles unicode codepoints as well.  The <code>countLetters</code> method would look like this:

```java5
public static Map<Integer, Long> countLetters(String filename) throws IOException {
    return Files.lines(Paths.get(filename))
        .flatMapToInt(String::chars)
        .filter(Character::isLetter)
        .boxed()
        .collect(Collectors.groupingBy(Function.identity(), Collectors.counting()));
}
```



## JavaScript


JavaScript is no longer used only in environments which are carefully isolated from file systems, but JavaScript standards still do not specify standard file-system functions.
Leaving aside the particular and variable details of how files will be opened and read in environments like Node.js and OS X JavaScript for Automation etc.,
we can still use core JavasScript (ES5 in the example below), to count the characters in a text once it has been read from a file system.


```JavaScript
(function(txt) {

    var cs = txt.split(''),
        i = cs.length,
        dct =  {},
        c = '',
        keys;

    while (i--) {
        c = cs[i];
        dct[c] = (dct[c] || 0) + 1;
    }

    keys = Object.keys(dct);
    keys.sort();
    return keys.map(function (c) { return [c, dct[c]]; });

})("Not all that Mrs. Bennet, however, with the assistance of her five\
daughters, could ask on the subject, was sufficient to draw from her\
husband any satisfactory description of Mr. Bingley. They attacked him\
in various ways--with barefaced questions, ingenious suppositions, and\
distant surmises; but he eluded the skill of them all, and they were at\
last obliged to accept the second-hand intelligence of their neighbour,\
Lady Lucas. Her report was highly favourable. Sir William had been\
delighted with him. He was quite young, wonderfully handsome, extremely\
agreeable, and, to crown the whole, he meant to be at the next assembly\
with a large party. Nothing could be more delightful! To be fond of\
dancing was a certain step towards falling in love; and very lively\
hopes of Mr. Bingley's heart were entertained.");
```


```JavaScript
[[" ", 121], ["!", 1], ["'", 1], [",", 13], ["-", 3], [".", 9], [";", 2],
["B", 3], ["H", 2], ["L", 2], ["M", 3], ["N", 2], ["S", 1], ["T", 2], ["W", 1],
["a", 53], ["b", 13], ["c", 17], ["d", 29], ["e", 82], ["f", 17], ["g", 16], ["h", 36],
["i", 44], ["j", 1], ["k", 3], ["l", 34], ["m", 11], ["n", 41], ["o", 40], ["p", 8],
["q", 2], ["r", 35], ["s", 39], ["t", 55], ["u", 20], ["v", 7], ["w", 17], ["x", 2], ["y", 16]]
```



## jq

The following program will report the frequency of all characters in the input file, including newlines, returns, etc, provided the file will fit in memory.
```jq

# Input: an array of strings.
# Output: an object with the strings as keys,
# the values of which are the corresponding frequencies.
def counter:
  reduce .[] as $item ( {}; .[$item] += 1 ) ;

# For neatness we sort the keys:
explode | map( [.] | implode ) | counter | . as $counter
 | keys | sort[] | [., $counter[.] ]


```

Example:
```sh
jq -s -R -c -f Letter_frequency.jq somefile.txt
```

```txt
["\n",12]
[" ",124]
["#",1]
["$",8]
["(",4]
[")",4]
["+",3]
[",",4]
["-",4]
[".",9]
["0",3]
["1",7]
[":",2]
[";",2]
["=",4]
...
```



## Julia

```julia
using DataStructures

function letterfreq(file::AbstractString; fltr::Function=(_) -> true)
    sort(Dict(counter(filter(fltr, readstring(file)))))
end

display(letterfreq("src/Letter_frequency.jl"; fltr=isalpha))
```


```txt

DataStructures.OrderedDict{Char,Int64} with 29 entries:
  'A' => 1
  'C' => 1
  'D' => 2
  'F' => 1
  'L' => 3
  'S' => 2
  'a' => 9
  'b' => 1
  'c' => 13
  'd' => 5
  'e' => 30
  'f' => 13
  'g' => 4
  'h' => 10
  'i' => 14
  'j' => 1
  'k' => 3
  'l' => 11
  'n' => 15
  ⋮   => ⋮
```



## K


```K
+(?a;#:'=a:,/0:`)
```


Example: The file "hello.txt" contains the string "Hello, world!"


```K

  c:+(?a;#:'=a:,/0:`hello.txt)

```


```txt

(("H";1)
 ("e";1)
 ("l";3)
 ("o";2)
 (",";1)
 (" ";1)
 ("w";1)
 ("r";1)
 ("d";1)
 ("!";1))

```


Sort on decreasing occurrences:


```K

     c@>c[;1]

```


```txt

(("l";3)
 ("o";2)
 ("H";1)
 ("e";1)
 (",";1)
 (" ";1)
 ("w";1)
 ("r";1)
 ("d";1)
 ("!";1))

```



## Kotlin


```scala
// version 1.1.2

import java.io.File

fun main(args: Array<String>) {
    val text = File("input.txt").readText().toLowerCase()
    val letterMap = text.filter { it in 'a'..'z' }.groupBy { it }.toSortedMap()
    for (letter in letterMap) println("${letter.key} = ${letter.value.size}")
    val sum = letterMap.values.sumBy { it.size }
    println("\nTotal letters = $sum")
}
```


'input.txt' just contains two pangrams:

The quick brown fox jumps over the lazy dog.
Sphinx of black quartz, judge my vow.

```txt

a = 3
b = 2
c = 2
d = 2
e = 4
f = 2
g = 2
h = 3
i = 2
j = 2
k = 2
l = 2
m = 2
n = 2
o = 6
p = 2
q = 2
r = 3
s = 2
t = 3
u = 4
v = 2
w = 2
x = 2
y = 2
z = 2

Total letters = 64

```



## Liberty BASIC

Un-rem a line to convert to all-upper-case.
Letter freq'y is printed as percentages.

```lb

    open "text.txt" for input as #i
        txt$ =input$( #i, lof( #i))
        Le =len( txt$)
    close #i

    dim LetterFreqy( 255)

    '   txt$ =upper$( txt$)

    for i =1 to Le
        char =asc( mid$( txt$, i, 1))
        if char >=32 then LetterFreqy( char) =LetterFreqy( char) +1
    next i

    for j =32 to 255
        if LetterFreqy( j) <>0 then print " Character #"; j, "("; chr$( j);_
         ") appeared "; using( "##.##", 100 *LetterFreqy( j) /Le); "% of the time."
    next j

    end

```



## Lasso


```Lasso
local(
	str 	= 'Hello world!',
	freq	= map
)
// as a loop. arguably quicker than query expression
loop(#str->size) => {
	#freq->keys !>> #str->get(loop_count) ?
		#freq->insert(#str->get(loop_count) = #str->values->find(#str->get(loop_count))->size)
}

// or
local(
	str 	= 'Hello world!',
	freq	= map
)
// as query expression, less code
with i in #str->values where #freq->keys !>> #i do => {
	#freq->insert(#i = #str->values->find(#i)->size)
}

// output #freq
with elem in #freq->keys do => {^
	'"'+#elem+'": '+#freq->find(#elem)+'\r'
^}
```



## Lua

This solution counts letters only, which could be changed by altering the pattern argument to 'gmatch' on line 31.  It also treats upper and lower case letters as distinct, which could be changed by changing everything to upper or lower case with string.upper() or string.lower() before tallying.

```lua
-- Return entire contents of named file
function readFile (filename)
  local file = assert(io.open(filename, "r"))
  local contents = file:read("*all")
  file:close()
  return contents
end

-- Return a closure to keep track of letter counts
function tally ()
  local t = {}

  -- Add x to tally if supplied, return tally list otherwise
  local function count (x)
    if x then
      if t[x] then
        t[x] = t[x] + 1
      else
        t[x] = 1
      end
    else
      return t
    end
  end

  return count
end

-- Main procedure
local letterCount = tally()
for letter in readFile(arg[1]):gmatch("%a") do
  letterCount(letter)
end
for k, v in pairs(letterCount()) do
  print(k, v)
end
```

Output from running this script on itself:

```txt
i       24
g       2
h       4
e       61
f       16
c       19
d       17
R       2
o       31
p       7
m       4
n       42
k       4
l       40
y       4
w       1
x       7
u       18
v       2
s       14
t       54
a       24
C       3
M       1
A       1
F       2
r       32
```



## M2000 Interpreter


```M2000 Interpreter

document file1$={Open a text file and count the occurrences of each letter.
		Some of these programs count all characters (including punctuation), but some only count letters A to Z
		}
const Ansi=3, nl$=chr$(13)+chr$(10), Console=-2
save.doc file1$, "checkdoc.txt", Ansi
open "checkdoc.txt" for input as F
buffer onechar as byte
m=0
dim m(65 to 90)
while not eof(#F)
	get #F, onechar
	a$=chr$(eval(onechar,0))
	if a$ ~ "[A-Za-z]" then
		m++
		m(asc(ucase$(a$)))++
	end if
end while
close #F
document Export$
for i=65 to 90
	if m(i)>0 then Export$=format$("{0} - {1:2:4}%",chr$(i),m(i)/m*100)+nl$
next
print #Console, Export$
clipboard Export$

```


<pre style="height:30ex;overflow:scroll">
A - 6,87%
B - 0,76%
C - 8,40%
D - 1,53%
E - 12,2%
F - 2,29%
G - 1,53%
H - 3,05%
I - 3,05%
L - 5,34%
M - 2,29%
N - 8,40%
O - 9,92%
P - 2,29%
R - 6,11%
S - 5,34%
T - 12,2%
U - 6,11%
X - 0,76%
Y - 0,76%
Z - 0,76%
</pre >


## Maple


```Maple
StringTools:-CharacterFrequencies(readbytes("File.txt",infinity,TEXT))
```


=={{header|Mathematica}} / {{header|Wolfram Language}}==


```Mathematica
Tally[Characters[Import["file.txt","Text"]]]
```


=={{header|MATLAB}} / {{header|Octave}}==

```MATLAB
function u = letter_frequency(t)
	if ischar(t)
		t = abs(t);
	end;
	A = sparse(t+1,1,1,256,1);
	printf('"%c":%i\n',[find(A)-1,A(A>0)]')
end
```



## NetRexx

```netrexx
/* NetRexx ************************************************************
* 22.05.2013 Walter Pachl  translated from REXX
**********************************************************************/
options replace format comments java crossref symbols nobinary

  parse arg dsn .
  if dsn = '' then
    dsn = 'test.txt'
  cnt=0
  totChars=0                         /*count of the total num of chars*/
  totLetters=0                       /*count of the total num letters.*/
  indent=' '.left(20)                /*used for indentation of output.*/
  lines = scanFile(dsn)
  loop l_ = 1 to lines[0]
    line = lines[l_]

    Say '>'line'<' line.length       /* that's in test.txt            */
    /*
    lrx=left_right(line)
    Parse lrx leftx rightx
    Say ' 'leftx
    Say ' 'rightx
    */
    loop k=1 for line.length()       /*loop over characters           */
      totChars=totChars+1            /*Increment total number of chars*/
      c=line.substr(k,1)             /*get character number k         */
      cnt[c]=cnt[c]+1                /*increment the character's count*/
      End
    end l_

  w=totChars.length                  /*used for right-aligning counts.*/
  say 'file -----' dsn "----- has" lines[0] 'records.'
  say 'file -----' dsn "----- has" totChars 'characters.'
  Loop L=0 to 255                    /* display nonzero letter counts */
    c=l.d2c                          /* the character in question     */
    if cnt[c]>0 & c.datatype('M')>0 Then Do /* was found in the file  */
                                     /* and is a latin letter         */
      say indent "(Latin) letter " c 'count:' cnt[c].right(w) /* tell */
      totLetters=totLetters+cnt[c]   /* increment number of letters   */
      End
    End

  say 'file -----' dsn "----- has" totLetters '(Latin) letters.'
  say '                           other charactes follow'
  other=0
  loop m=0 to 255                    /* now for non-letters           */
    c=m.d2c                          /* the character in question     */
    y=c.c2x                          /* the hex representation        */
    if cnt[c]>0 & c.datatype('M')=0 Then Do /* was found in the file  */
                                     /* and is not a latin letter     */
      other=other+cnt[c]             /* increment count               */
      _=cnt[c].right(w)              /* prepare output of count       */
      select                         /*make the character viewable.   */
       when c<<' ' | m==255 then say indent "'"y"'x character count:" _
       when c==' '          then say indent "blank character count:" _
       otherwise                 say indent "   " c 'character count:' _
       end
     end
   end
  say 'file -----' dsn "----- has" other 'other characters.'
  say 'file -----' dsn "----- has" totLetters 'letters.'

-- Read a file and return contents as a Rexx indexed string
method scanFile(dsn) public static returns Rexx

  fileLines = ''
  do
    inFile = File(dsn)
    inFileScanner = Scanner(inFile)
    loop l_ = 1 while inFileScanner.hasNext()
      fileLines[0] = l_
      fileLines[l_] = inFileScanner.nextLine()
      end l_
    inFileScanner.close()

  catch ex = FileNotFoundException
    ex.printStackTrace
  end

  return fileLines
```



## Nim


```nim
import tables, os

var t = initCountTable[char]()
var f = open(paramStr(1))
for l in f.lines:
  for c in l:
    t.inc(c)
echo t
```



## Objeck


```objeck

use IO;

bundle Default {
  class Test {
    function : Main(args : String[]) ~ Nil {
      freqs := CountLetters("filename.txt");
      for(i := 'A'; i < 'Z'; i += 1;) {
        Console->Print(i->As(Char))->Print("=>")->PrintLine(freqs[i - 'A']);
      };
    }

    function : CountLetters(filename : String) ~ Int[] {
      freqs := Int->New[26];
      reader := FileReader->New(filename);
      while(reader->IsEOF() <> true) {
        line := reader->ReadString()->ToUpper();
        each(i : line) {
          ch := line->Get(i);
          if(ch->IsChar()){
            index := ch - 'A';
            freqs[index] := freqs[index] + 1;
          };
        };
      };
      reader->Close();

      return freqs;
    }
  }
}

```


=={{header|Objective-C}}==


```objc>#import <Foundation/Foundation.h


int main (int argc, const char *argv[]) {
  @autoreleasepool {

    NSData *data = [NSData dataWithContentsOfFile:@(argv[1])];
    NSString *string = [[NSString alloc] initWithData:data encoding:NSUTF8StringEncoding];
    NSCountedSet *countedSet = [[NSCountedSet alloc] init];
    NSUInteger len = [string length];
    for (NSUInteger i = 0; i < len; i++) {
      unichar c = [string characterAtIndex:i];
      if ([[NSCharacterSet letterCharacterSet] characterIsMember:c])
        [countedSet addObject:@(c)];
    }
    for (NSNumber *chr in countedSet) {
      NSLog(@"%C => %lu", (unichar)[chr integerValue], [countedSet countForObject:chr]);
    }

  }
  return 0;
}
```



## OCaml


We open a text file and compute letter frequency. Other characters than [a-z] and [A-Z] are ignored, and upper case letters are first converted to lower case before to compute letter frequency.


```ocaml
let () =
  let ic = open_in Sys.argv.(1) in
  let base = int_of_char 'a' in
  let arr = Array.make 26 0 in
  try while true do
    let c = Char.lowercase(input_char ic) in
    let ndx = int_of_char c - base in
    if ndx < 26 && ndx >= 0 then
      arr.(ndx) <- succ arr.(ndx)
  done
  with End_of_file ->
    close_in ic;
    for i=0 to 25 do
      Printf.printf "%c -> %d\n" (char_of_int(i + base)) arr.(i)
    done
```


If we want to compute all characters in an UTF8 file, we must use an external library, for example Batteries. The following function takes as input a string that contains the path to the file, and prints all the characters together with their frequencies, ordered by increasing frequencies, on the standard output.


```ocaml

open Batteries

let frequency file =
  let freq = Hashtbl.create 52 in
    File.with_file_in file
      (Enum.iter (fun c -> Hashtbl.modify_def 1 c succ freq) % Text.chars_of);
    List.iter (fun (k,v) -> Text.write_text stdout k;
                            Printf.printf " %d\n" v)
    @@ List.sort (fun (_,v) (_,v') -> compare v v')
    @@ Hashtbl.fold (fun k v l -> (Text.of_uchar k,v) :: l) freq []

```



## OxygenBasic


```oxygenbasic

indexbase 0

sys a,e,i,c[255]

string s=getfile "t.txt"

e=len s

for i=1 to e
  a=asc(s,i)
  ++c(a)
next

cr=chr(13)+chr(10)
pr="Char Frequencies" cr cr
for i=32 to 255
  pr+=chr(i) chr(9) c(i) cr
next

print pr
'putfile "CharCount.txt",pr

```



## PARI/GP


```parigp
v=vector(26);
U=readvec("foo.txt");
for(i=1,#U,u=Vecsmall(U[i]);for(j=1,#u,if(u[j]>64&&u[j]<91,v[u[j]-64]++,u[j]>96&&u[j]<123,v[u[j]-96]++)));
v
```



## Pascal


```pascal
program letterFrequency(input, output, stdErr);
var
	chart: array[char] of integer;
	c: char;
begin
	for c := low(chart) to high(chart) do
	begin
		chart[c] := 0;
	end;

	// parameter-less EOF() checks for EOF(input)
	while not EOF() do
	begin
		read(c);
		inc(chart[c]);
	end;

	// now, chart[someLetter] gives you the letter’s frequency
end.
```



## Perl

Counts letters in files given on command line or piped to stdin.  Case insensitive.

```perl
while (<>) { $cnt{lc chop}++ while length }
print "$_: ", $cnt{$_}//0, "\n" for 'a' .. 'z';
```



## Perl 6

In perl6, whenever you want to count things in a collection, the rule of thumb is to use the Bag structure.

```perl6
.&ws.say for slurp.comb.Bag.sort: -*.value;

sub ws ($pair) {
    $pair.key ~~ /\n/
    ?? ('NEW LINE' => $pair.value)
    !! $pair.key ~~ /\s/
    ?? ($pair.key.uniname => $pair.value)
    !! $pair
}
```

```txt
SPACE => 522095
e => 325692
t => 222916
a => 199790
o => 180974
h => 170210
n => 167006
i => 165201
s => 157585
r => 145118
d => 106987
l => 97131
NEW LINE => 67662
u => 67340
c => 62717
m => 56021
f => 53494
w => 53301
, => 48784
g => 46060
p => 39932
y => 37985
b => 34276
. => 30589
v => 24045
" => 14340
k => 14169
T => 12547
- => 11037
I => 10067
A => 7355
H => 6600
M => 6206
; => 5885
E => 4968
C => 4583
S => 4392
' => 3938
x => 3692
! => 3539
R => 3531
P => 3424
O => 3401
j => 3390
B => 3185
W => 3180
N => 3053
? => 2976
F => 2754
G => 2508
: => 2468
J => 2448
L => 2444
q => 2398
V => 2200
_ => 2070
z => 1847
D => 1756
é => 1326
Y => 1238
U => 895
1 => 716
8 => 412
X => 333
K => 321
è => 292
3 => 259
2 => 248
5 => 220
0 => 218
* => 181
4 => 181
) => 173
( => 173
6 => 167
É => 146
7 => 143
Q => 135
] => 122
[ => 122
9 => 117
æ => 106
= => 75
ê => 74
Z => 59
à => 59
â => 56
> => 50
< => 50
/ => 50
ç => 48
NO-BREAK SPACE => 45
î => 39
ü => 37
| => 36
ô => 34
# => 26
ù => 18
ï => 18
Æ => 10
û => 9
+ => 5
È => 5
ë => 5
À => 4
@ => 2
ñ => 2
Ç => 2
$ => 2
% => 1
& => 1
{ => 1
} => 1
½ => 1

```



## Phix

Counts own source or supplied filename

```Phix
sequence lc = repeat(0,#7E)
integer fn = open(command_line()[$],"rb"), ch
while 1 do
    ch = getc(fn)
    if ch=-1 then exit end if
    if ch>=' ' and ch<#7F then
        lc[ch] += 1
    end if
end while
close(fn)

for i=' ' to #7E do
    if lc[i]!=0 then
        printf(1,"'%c': %d%s",{i,lc[i],iff(mod(i,32)=31?'\n':'\t')})
    end if
end for
{} = wait_key()
```

```txt

' ': 77 '!': 1  '"': 4  '#': 3  '$': 1  '%': 3  ''': 10 '(': 9  ')': 9  '+': 1  ',': 8  '-': 1  '0': 2  '1': 5  '2': 1  '3': 2  '7': 3  ':': 2  '<': 1  '=': 10 '>': 1  '?': 1
'E': 2  'F': 1  '[': 4  '\': 2  ']': 4  '_': 2
'a': 4  'b': 1  'c': 15 'd': 11 'e': 23 'f': 14 'g': 2  'h': 11 'i': 19 'k': 1  'l': 8  'm': 3  'n': 19 'o': 9  'p': 3  'q': 1  'r': 6  's': 3  't': 11 'u': 1  'w': 3  'x': 1    'y': 1  '{': 2  '}': 2

```



## PHP


```php
<?php
print_r(array_count_values(str_split(file_get_contents($argv[1]))));
?>
```



## PicoLisp


```PicoLisp
(let Freq NIL
   (in "file.txt"
      (while (char) (accu 'Freq @ 1)) )
   (sort Freq) )
```

For a "file.txt":

```txt
abcd
cdef
```

```txt
-> (("^J" . 2) ("a" . 1) ("b" . 1) ("c" . 2) ("d" . 2) ("e" . 1) ("f" . 1))
```



## PL/I


```PL/I

frequencies: procedure options (main);
   declare tallies(26) fixed binary static initial ((26) 0);
   declare alphabet character (26) static initial
      ('ABCDEFGHIJKLMNOPQRSTUVWXYZ');
   declare c character (1), i fixed binary;
   declare in file;

   open file (in) title ('/LETTER.DAT,type(text),recsize(200)') input;

   on endfile (in) go to prepare_list;

   do while('1'b);
      get file (in) edit (c) (a(1)); put edit (c) (a);
      i = index(alphabet, c);
      if i > 0 then tallies(i) = tallies(i) + 1;
   end;

prepare_list:
   put skip list('Letter', 'Frequency');
   do i = 1 to 26;
      if tallies(i) > 0 then
         put skip list (substr(alphabet, i, 1), tallies(i));
   end;
end frequencies;
```

Data:

```txt

THEQUICKBROWNFOX
JUMPSOVERTHELAZYDOG

```

```txt

Letter                  Frequency
A                               1
B                               1
C                               1
D                               1
E                               3
F                               1
G                               1
H                               2
I                               1
J                               1
K                               1
L                               1
M                               1
N                               1
O                               4
P                               1
Q                               1
R                               2
S                               1
T                               2
U                               2
V                               1
W                               1
X                               1
Y                               1
Z                               1

```



## PowerShell


```PowerShell

function frequency ($string) {
    $arr = $string.ToUpper().ToCharArray() |where{$_ -match '[A-KL-Z]'}
    $n = $arr.count
    $arr | group | foreach{
        [pscustomobject]@{letter = "$($_.name)"; frequency  = "$([math]::round($($_.Count/$n),5))"; count = "$($_.count)"}
    } | sort letter
}
$file = "$($MyInvocation.MyCommand.Name )" #Put the name of your file here
frequency $(get-content $file -Raw)

```

<b>Output:</b>

```txt

letter frequency count
------ --------- -----
A      0.06809   16
B      0.00426   1
C      0.06809   16
D      0.00851   2
E      0.11064   26
F      0.0383    9
G      0.01702   4
H      0.02979   7
I      0.03404   8
J      0.00426   1
K      0.00426   1
L      0.02553   6
M      0.04255   10
N      0.09362   22
O      0.08085   19
P      0.02128   5
Q      0.01277   3
R      0.10638   25
S      0.02128   5
T      0.10213   24
U      0.05957   14
V      0.00426   1
W      0.00851   2
Y      0.02979   7
Z      0.00426   1

```



## Prolog

Works with SWI-Prolog.

Only alphabetic codes are computed in uppercase state.

Uses '''packlist/2''' defined there : [[Run-length encoding#Prolog]]


```Prolog
frequency(File) :-
	read_file_to_codes(File, Code, []),

	% we only keep alphabetic codes
	include(my_code_type, Code, LstCharCode),

	% we translate char_codes into uppercase atoms.
	maplist(my_upcase, LstCharCode, LstChar),

	% sort and pack the list
	msort(LstChar, SortLstChar),
	packList(SortLstChar, Freq),
	maplist(my_write, Freq).


my_write([Num, Atom]) :-
	swritef(A, '%3r', [Num]),
	writef('Number of %w :%w\n', [Atom, A]).


my_code_type(Code) :-
	code_type(Code, alpha).

my_upcase(CharCode, UpChar) :-
	char_code(Atom, CharCode),
	upcase_atom(Atom, UpChar).

:- use_module(library(clpfd)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ?- packList([a,a,a,b,c,c,c,d,d,e], L).
%  L = [[3,a],[1,b],[3,c],[2,d],[1,e]] .
%
% ?- packList(R,  [[3,a],[1,b],[3,c],[2,d],[1,e]]).
% R = [a,a,a,b,c,c,c,d,d,e] .
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
packList([],[]).

packList([X],[[1,X]]) :-
	!.

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

{{out}} for this file

```txt
Number of A : 63
Number of B :  7
Number of C : 53
Number of D : 29
Number of E : 65
...
Number of T : 52
Number of U : 20
Number of V : 10
Number of W :  8
Number of X :  6
Number of Y : 12
true .

```



## PureBasic

Alphabetic codes are converted to uppercase before being used and no other codes are used as part of the calculations.


```PureBasic
Procedure countLetters(Array letterCounts(1), textLine.s)
  ;counts only letters A -> Z, uses index 0 of letterCounts() to keep a total of all counts
  Protected i, lineLength = Len(textLine), letter

  textLine = UCase(textLine)
  For i = 1 To lineLength
    letter = Asc(Mid(textLine, i, 1)) - 'A' + 1
    If letter >= 1 And letter <= 26
      letterCounts(letter) + 1 ;tally individual letter count
      letterCounts(0) + 1      ;increment total letter count
    EndIf
  Next
EndProcedure

If OpenConsole()
  Define filename.s, fileID, i
  filename = OpenFileRequester("Select text file to examine", "*.txt", "Text (*.txt)|*.txt;|All files (*.*)|*.*", 0)
  fileID = 0
  If ReadFile(fileID, filename)
    Dim letterCounts(26) ;A - Z only, index 0 contains the total of all letter counts

    Define textLine.s
    While Not Eof(fileID)
      textLine = ReadString(fileID)
      countLetters(letterCounts(), textLine)
    Wend
    CloseFile(fileID)

    PrintN("File: " + filename + #CRLF$)
    PrintN("Letter  %Freq  Count")
    For i = 1 To 26
      Print("  " + Chr(64 + i) + "     ")
      Print(RSet(StrF(100 * letterCounts(i) / letterCounts(0), 1), 5, " ") + "  ")
      PrintN(Str(letterCounts(i)))
    Next
    PrintN(#CRLF$ + "Total letter count in file: " + Str(letterCounts(0)))
  EndIf

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
```

```txt
File: D:\_T\Text\dictionary.txt

Letter  %Freq  Count
  A       7.6  27743
  B       2.0  7248
  C       4.3  15433
  D       3.8  13798
  E      11.8  42917
  F       1.4  5030
  G       2.8  10336
  H       2.1  7720
  I       8.6  31141
  J       0.2  588
  K       0.8  2964
  L       5.3  19399
  M       2.7  9821
  N       7.1  25682
  O       6.1  22084
  P       2.9  10696
  Q       0.2  714
  R       7.5  27055
  S       8.0  28898
  T       7.1  25773
  U       3.3  12032
  V       1.1  4019
  W       0.9  3348
  X       0.3  1096
  Y       1.7  6251
  Z       0.3  1177

Total letter count in file: 362963
```



## Python


### Functional


### =Using collections.Counter=

```python
import collections, sys

def filecharcount(openfile):
    return sorted(collections.Counter(c for l in openfile for c in l).items())

f = open(sys.argv[1])
print(filecharcount(f))
```



### =As a fold=

Character counting can be conveniently expressed in terms of fold/reduce. See the example below, which also generates column-wrapped output:
```python
'''Character counting as a fold'''

from functools import reduce
from itertools import repeat
from os.path import expanduser


# charCounts :: String -> Dict Char Int
def charCounts(s):
    '''A dictionary of
       (character, frequency) mappings
    '''
    def tally(dct, c):
        dct[c] = 1 + dct[c] if c in dct else 1
        return dct
    return reduce(tally, list(s), {})


# TEST ----------------------------------------------------
# main :: IO ()
def main():
    '''Listing in descending order of frequency.'''

    print(
        tabulated(
            'Descending order of frequency:\n'
        )(compose(repr)(fst))(compose(str)(snd))(
            5
        )(stet)(
            sorted(
                charCounts(
                    readFile('~/Code/charCount/readme.txt')
                ).items(),
                key=swap,
                reverse=True
            )
        )
    )


# GENERIC -------------------------------------------------

# chunksOf :: Int -> [a] -> [[a]]
def chunksOf(n):
    '''A series of lists of length n,
       subdividing the contents of xs.
       Where the length of xs is not evenly divible,
       the final list will be shorter than n.'''
    return lambda xs: reduce(
        lambda a, i: a + [xs[i:n + i]],
        range(0, len(xs), n), []
    ) if 0 < n else []


# compose (<<<) :: (b -> c) -> (a -> b) -> a -> c
def compose(g):
    '''Right to left function composition.'''
    return lambda f: lambda x: g(f(x))


# fst :: (a, b) -> a
def fst(tpl):
    '''First member of a pair.'''
    return tpl[0]


# readFile :: FilePath -> IO String
def readFile(fp):
    '''The contents of any file at the path
       derived by expanding any ~ in fp.'''
    with open(expanduser(fp), 'r', encoding='utf-8') as f:
        return f.read()


# paddedMatrix :: a -> [[a]] -> [[a]]
def paddedMatrix(v):
    ''''A list of rows padded to equal length
        (where needed) with instances of the value v.'''
    def go(rows):
        return paddedRows(
            len(max(rows, key=len))
        )(v)(rows)
    return lambda rows: go(rows) if rows else []


# paddedRows :: Int -> a -> [[a]] -[[a]]
def paddedRows(n):
    '''A list of rows padded (but never truncated)
       to length n with copies of value v.'''
    def go(v, xs):
        def pad(x):
            d = n - len(x)
            return (x + list(repeat(v, d))) if 0 < d else x
        return list(map(pad, xs))
    return lambda v: lambda xs: go(v, xs) if xs else []


# showColumns :: Int -> [String] -> String
def showColumns(n):
    '''A column-wrapped string
       derived from a list of rows.'''
    def go(xs):
        def fit(col):
            w = len(max(col, key=len))

            def pad(x):
                return x.ljust(4 + w, ' ')
            return ''.join(map(pad, col)).rstrip()

        q, r = divmod(len(xs), n)
        return '\n'.join(map(
            fit,
            zip(*paddedMatrix('')(
                chunksOf(q + int(bool(r)))(xs)
            ))
        ))
    return lambda xs: go(xs)


# snd :: (a, b) -> b
def snd(tpl):
    '''Second member of a pair.'''
    return tpl[1]


# stet :: a -> a
def stet(x):
    '''The identity function.
       The usual 'id' is reserved in Python.'''
    return x


# swap :: (a, b) -> (b, a)
def swap(tpl):
    '''The swapped components of a pair.'''
    return (tpl[1], tpl[0])


# tabulated :: String -> (a -> String) ->
#                        (b -> String) ->
#                        Int ->
#                        (a -> b) -> [a] -> String
def tabulated(s):
    '''Heading -> x display function -> fx display function ->
          number of columns -> f -> value list -> tabular string.'''
    def go(xShow, fxShow, intCols, f, xs):
        def mxw(fshow, g):
            return max(map(compose(len)(fshow), map(g, xs)))
        w = mxw(xShow, lambda x: x)
        fw = mxw(fxShow, f)
        return s + '\n' + showColumns(intCols)([
            xShow(x).rjust(w, ' ') + ' -> ' + (
                fxShow(f(x)).rjust(fw, ' ')
            )
            for x in xs
        ])
    return lambda xShow: lambda fxShow: lambda nCols: (
        lambda f: lambda xs: go(
            xShow, fxShow, nCols, f, xs
        )
    )


# MAIN ---
if __name__ == '__main__':
    main()
```

```txt
Descending order of frequency:

 ' ' -> 568     ')' ->  62     'v' ->  25     'w' ->   7     '5' ->   3
'\t' -> 382     '(' ->  62     '1' ->  24     'k' ->   7     '4' ->   3
 'e' -> 274     'd' ->  60     'G' ->  22     '9' ->   6     '+' ->   3
 'n' -> 233     'g' ->  59     ']' ->  17     'S' ->   5     '¬' ->   2
'\n' -> 228     'u' ->  58     '[' ->  17     'R' ->   5     '=' ->   2
 't' -> 204     '|' ->  54     'λ' ->  16     'M' ->   5     '.' ->   2
 's' -> 198     'x' ->  53     '2' ->  15     'F' ->   5     'L' ->   1
 '-' -> 178     'm' ->  52     'N' ->  11     '<' ->   5     'C' ->   1
 'i' -> 145     'c' ->  52     '}' ->  10     '6' ->   5     'A' ->   1
 'o' -> 126     'h' ->  47     '{' ->  10     'z' ->   4     '3' ->   1
 'f' -> 100     ':' ->  47     'T' ->  10     "'" ->   4     '&' ->   1
 'r' ->  96     ',' ->  38     'I' ->  10     '^' ->   3     '$' ->   1
 'a' ->  86     'b' ->  32     '0' ->  10     'E' ->   3
 'l' ->  70     'y' ->  31     '"' ->  10     '8' ->   3
 'p' ->  68     '>' ->  28     'J' ->   9     '7' ->   3
```



### Procedural


### =Without using collections.Counter=


```python
import string
if hasattr(string, 'ascii_lowercase'):
    letters = string.ascii_lowercase       # Python 2.2 and later
else:
    letters = string.lowercase             # Earlier versions
offset = ord('a')

def countletters(file_handle):
    """Traverse a file and compute the number of occurences of each letter
    return results as a simple 26 element list of integers."""
    results = [0] * len(letters)
    for line in file_handle:
        for char in line:
            char = char.lower()
            if char in letters:
                results[ord(char) - offset] += 1
                # Ordinal minus ordinal of 'a' of any lowercase ASCII letter -> 0..25
    return results

if __name__ == "__main__":
    sourcedata = open(sys.argv[1])
    lettercounts = countletters(sourcedata)
    for i in xrange(len(lettercounts)):
        print "%s=%d" % (chr(i + ord('a')), lettercounts[i]),
```


This example defines the function and provides a sample usage.  The ''if ... __main__...'' line allows it to be cleanly imported into any other Python code while also allowing it to function as a standalone script.  (A very common Python idiom).

Using a numerically indexed array (list) for this is artificial and clutters the code somewhat.


### =Using defaultdict=

```python
...
from collections import defaultdict
def countletters(file_handle):
    """Count occurences of letters and return a dictionary of them
    """
    results = defaultdict(int)
    for line in file_handle:
        for char in line:
            if char.lower() in letters:
                c = char.lower()
                results[c] += 1
    return results
```


Which eliminates the ungainly fiddling with ordinal values and offsets in function countletters of a previous example above.  More importantly it allows the results to be more simply printed using:


```python
lettercounts = countletters(sourcedata)
for letter,count in lettercounts.iteritems():
    print "%s=%s" % (letter, count),
```


Again eliminating all fussing with the details of converting letters into list indices.


## R


```R
letter.frequency <- function(filename)
{
    file <- paste(readLines(filename), collapse = '')
    chars <- strsplit(file, NULL)[[1]]
    summary(factor(chars))
}
```


Usage on itself:


```R>
 source('letter.frequency.r')
> letter.frequency('letter.frequency.r')
    -  ,  .  '  (  )  [  ]  {  }  <  =  1  a  c  d  e  f  h  i  l  L  m  n  N  o  p  q  r  s  t  u  U  y
22  3  2  1  2  6  6  2  2  1  1  3  1  1  9  6  1 14  7  2  7  8  3  4  6  1  3  3  1  8  8  7  3  1  2
```



## Racket


```racket

#lang racket
(require math)

(define (letter-frequencies ip)
  (count-samples
   (port->list read-char ip)))

(letter-frequencies (open-input-string "abaabdc"))

```

```txt

'(#\a #\b #\d #\c)
'(3 2 1 1)

```


Using input from a text file:

```racket

(letter-frequencies (open-input-file "somefile.txt"))

```



## Raven


```Raven
define count_letters use $words
   { } as $wordHash    [ ] as $keys   [ ]  as $vals
   $words each chr
      dup $wordHash swap get 0 prefer 1 +   # stack: chr cnt
      swap $wordHash swap set
   $wordHash keys copy sort each
      dup $keys push
      $wordHash swap get $vals push
   $keys $vals combine  print "\n" print

"test.dat" as $file
$file read as $all_data
$all_data count_letters
```



## REXX


### version 1

It should be noted that the file being read is read one line at time, so the

line-end characters (presumably the line-feed, carriage return, new-line, or

whatever control characters are being used) are not reported.

These characters could be read and reported if the   '''charin'''   BIF would be used instead of the   '''linein'''   BIF.


Also note that this REXX program is ASCII or EBCDIC independent, but what constitutes a letter is restricted to

the Latin (Roman) alphabet (that is, which characters are considered to be letters of a particular language.

The version of REXX that was used was the '''English''' version of Regina REXX.   It should be noted that almost all

REXX interpreters assume the English language for such things as determining what characters are considered

letters unless another language is specified   (Regina REXX uses an environmental variable for this purpose).

All characters are still counted, whether a letter or not, including non-displayable characters.

```rexx
/*REXX program counts the occurrences of all characters in a file, & note that*/
/*     all Latin alphabet letters are uppercased for counting {Latin} letters.*/
/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
abc = 'abcdefghijklmnopqrstuvwxyz'     /*define an (Latin or English) alphabet*/
abcU= 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'     /*define an uppercase version of  [↑]. */
parse arg fileID .                     /*this last char isn't a middle dot: · */
if fileID==''  then fileID='JUNK.TXT'  /*¿none specified? Then use the default*/
totChars=0;    totLetters=0            /*count of all chars and of all letters*/
pad=left('',18);    pad9=left('',18%2) /*used for the indentations of output. */
@.=0                                   /*wouldn't it be neat to use Θ instead?*/
     do j=1  while lines(fileID)\==0   /*read the file 'til the cows come home*/
     rec=linein(fileID)                /*get a line/record from the input file*/
                                       /* [↓]  process all characters in  REC.*/
       do k=1  for length(rec)         /*examine/count each of the characters.*/
       totChars=totChars+1             /*bump count of number of characters.  */
       c=substr(rec,k,1);  @.c=@.c+1   /*Peel off a character; bump its count.*/
       if \datatype(c,'M') then iterate  /*Not a Latin letter?  Get next char.*/
       totLetters=totLetters+1         /*bump the count for [Latin] letters.  */
       upper c   /* ◄«««««««««««««««««««««««««««◄ uppercase a Latin character.*/
       @..c=@..c+1                     /*bump the (Latin) letter's count.     */
       end   /*k*/                     /*no Greek glyphs: π Γ Σ µ α ß Φ ε δ σ */
     end     /*j*/                     /*maybe we're ½ done by now, or maybe ¬*/
                                           LL= '(Latin) letter'
w=length(totChars)                     /*used for right─aligning the counts.  */
say 'file ─────' fileId "───── has" j-1 'records and has' totLetters LL"s."; say
  do L=0  for 256;    c=d2c(L)         /*display all none─zero letter counts. */
  if @..c==0  then iterate             /*A zero count?  Then ignore character.*/
  say pad9  LL' '   c   " (also" translate(c,abc,abcU)')  count:'  right(@..c,w)
  end   /*L*/                          /*we may be in a rut, but not a cañyon.*/

say;    say 'file ─────'  fileId   "───── has"   totChars   'unique characters.'
say
     do #=0  for 256;    y=d2c(#)      /*display all none─zero char counts.   */
     if @.y==0  then iterate           /*A zero count?  Then ignore character.*/
     c=d2c(#);  ch=c                   /*C  is the character glyph of a char. */
     if c<<' ' | #==255  then ch=      /*don't show control characters or null*/
     if c==' '           then ch='blank'                /*show a blank's name.*/
     say pad right(ch,5)     " ('"d2x(#,2)"'x  character count:"    right(@.c,w)
     end   /*#*/                       /*255 isn't quite ∞, but sometimes ∙∙∙ */
say                                    /*not a good place for dithering: ░▒▓█ */
say  pad   pad9   '☼ end─of─list ☼'    /*show we are at the end of the list.  */
                                       /*stick a fork in it, we're all done. ☻*/
```

'''output'''   when using the (above) REXX program for the input file:

Note that this REXX program works with ASCII or EBCDIC, but the order of the output will

be different because of the order in which EBCDIC and ASCII stores characters.
<pre style="height:150ex">
file ───── countfrq.rex ───── has 42 records and has 1540 (Latin) letters.

          (Latin) letter  A  (also a)  count:  133
          (Latin) letter  B  (also b)  count:   23
          (Latin) letter  C  (also c)  count:  101
          (Latin) letter  D  (also d)  count:   55
          (Latin) letter  E  (also e)  count:  176
          (Latin) letter  F  (also f)  count:   49
          (Latin) letter  G  (also g)  count:   23
          (Latin) letter  H  (also h)  count:   73
          (Latin) letter  I  (also i)  count:   86
          (Latin) letter  J  (also j)  count:    6
          (Latin) letter  K  (also k)  count:   11
          (Latin) letter  L  (also l)  count:   91
          (Latin) letter  M  (also m)  count:   20
          (Latin) letter  N  (also n)  count:  101
          (Latin) letter  O  (also o)  count:   97
          (Latin) letter  P  (also p)  count:   35
          (Latin) letter  Q  (also q)  count:    4
          (Latin) letter  R  (also r)  count:  107
          (Latin) letter  S  (also s)  count:   81
          (Latin) letter  T  (also t)  count:  164
          (Latin) letter  U  (also u)  count:   47
          (Latin) letter  V  (also v)  count:    3
          (Latin) letter  W  (also w)  count:   16
          (Latin) letter  X  (also x)  count:    9
          (Latin) letter  Y  (also y)  count:   23
          (Latin) letter  Z  (also z)  count:    6

file ───── countfrq.rex ───── has 3186 unique characters.

                          ('02'x  character count:    1
                          ('0F'x  character count:    2
                          ('11'x  character count:    2
                          ('18'x  character count:    1
                          ('19'x  character count:    1
                   blank  ('20'x  character count: 1041
                       "  ('22'x  character count:   12
                       #  ('23'x  character count:    6
                       %  ('25'x  character count:    1
                       &  ('26'x  character count:    1
                       '  ('27'x  character count:   47
                       (  ('28'x  character count:   21
                       )  ('29'x  character count:   20
                       *  ('2A'x  character count:   78
                       +  ('2B'x  character count:    4
                       ,  ('2C'x  character count:   16
                       -  ('2D'x  character count:    1
                       .  ('2E'x  character count:   38
                       /  ('2F'x  character count:   80
                       0  ('30'x  character count:    8
                       1  ('31'x  character count:   10
                       2  ('32'x  character count:   10
                       5  ('35'x  character count:    7
                       6  ('36'x  character count:    2
                       8  ('38'x  character count:    2
                       9  ('39'x  character count:    3
                       :  ('3A'x  character count:    5
                       ;  ('3B'x  character count:    9
                       <  ('3C'x  character count:    2
                       =  ('3D'x  character count:   38
                       ?  ('3F'x  character count:    5
                       @  ('40'x  character count:    9
                       A  ('41'x  character count:    3
                       B  ('42'x  character count:    1
                       C  ('43'x  character count:    8
                       D  ('44'x  character count:    6
                       E  ('45'x  character count:    4
                       F  ('46'x  character count:    1
                       G  ('47'x  character count:    3
                       H  ('48'x  character count:    1
                       I  ('49'x  character count:    8
                       J  ('4A'x  character count:    2
                       K  ('4B'x  character count:    2
                       L  ('4C'x  character count:   22
                       M  ('4D'x  character count:    2
                       N  ('4E'x  character count:    3
                       O  ('4F'x  character count:    1
                       P  ('50'x  character count:    2
                       Q  ('51'x  character count:    1
                       R  ('52'x  character count:    3
                       S  ('53'x  character count:    1
                       T  ('54'x  character count:    6
                       U  ('55'x  character count:    4
                       V  ('56'x  character count:    1
                       W  ('57'x  character count:    1
                       X  ('58'x  character count:    4
                       Y  ('59'x  character count:    1
                       Z  ('5A'x  character count:    1
                       [  ('5B'x  character count:    3
                       \  ('5C'x  character count:    2
                       ]  ('5D'x  character count:    3
                       a  ('61'x  character count:  130
                       b  ('62'x  character count:   22
                       c  ('63'x  character count:   93
                       d  ('64'x  character count:   49
                       e  ('65'x  character count:  172
                       f  ('66'x  character count:   48
                       g  ('67'x  character count:   20
                       h  ('68'x  character count:   72
                       i  ('69'x  character count:   78
                       j  ('6A'x  character count:    4
                       k  ('6B'x  character count:    9
                       l  ('6C'x  character count:   69
                       m  ('6D'x  character count:   18
                       n  ('6E'x  character count:   98
                       o  ('6F'x  character count:   96
                       p  ('70'x  character count:   33
                       q  ('71'x  character count:    3
                       r  ('72'x  character count:  104
                       s  ('73'x  character count:   80
                       t  ('74'x  character count:  158
                       u  ('75'x  character count:   43
                       v  ('76'x  character count:    2
                       w  ('77'x  character count:   15
                       x  ('78'x  character count:    5
                       y  ('79'x  character count:   22
                       z  ('7A'x  character count:    5
                       {  ('7B'x  character count:    1
                       |  ('7C'x  character count:    1
                       }  ('7D'x  character count:    1
                       ~  ('7E'x  character count:   76
                       ñ  ('A4'x  character count:    1
                       ¿  ('A8'x  character count:    1
                       ¬  ('AA'x  character count:    1
                       ½  ('AB'x  character count:    1
                       «  ('AE'x  character count:   27
                       ░  ('B0'x  character count:    1
                       ▒  ('B1'x  character count:    1
                       ▓  ('B2'x  character count:    1
                       ─  ('C4'x  character count:   25
                       █  ('DB'x  character count:    1
                       α  ('E0'x  character count:    1
                       ß  ('E1'x  character count:    1
                       Γ  ('E2'x  character count:    1
                       π  ('E3'x  character count:    1
                       Σ  ('E4'x  character count:    1
                       σ  ('E5'x  character count:    1
                       µ  ('E6'x  character count:    1
                       Φ  ('E8'x  character count:    1
                       Θ  ('E9'x  character count:    1
                       δ  ('EB'x  character count:    1
                       ∞  ('EC'x  character count:    1
                       ε  ('EE'x  character count:    1
                       ∙  ('F9'x  character count:    3
                       ·  ('FA'x  character count:    1

                             ☼ end─of─list ☼

```


===Version 2 (for TSO)===

```rexx
/*REXX program counts the occurences of all characters in a file
* Adapted version 1 for TSO (EXECIO instead of linein)
* No translation to uppercase takes place
* There is no need for tails being hex
* 25.07.2012 Walter Pachl
***********************************************************************/

  Parse arg dsn .                    /*Data set to be processed       */
  if dsn='' Then                     /*none specified?                */
    dsn='PRIV.V100(TEST)'            /* Use default.                  */
  c.=0                               /* Character counts              */
  "ALLOC   FI(IN) DA("dsn") SHR REUSE"
  'EXECIO   * DISKR IN (STEM L. FINIS'
  'FREE   FI(IN)'
  totChars=0                         /*count of the total num of chars*/
  totLetters=0                       /*count of the total num letters.*/
  indent=left('',20)                 /*used for indentation of output.*/

  do j=1 to l.0                      /*process all lines              */
    rec=l.j                          /*take line number j             */
    Say '>'rec'<' length(rec)        /*that's in PRIV.V100(TEST)      */
    Say ' E8C44D8FF015674BCDEF'
    Say ' 61100711200000000002'
    do k=1 for length(rec)           /*loop over characters           */
      totChars=totChars+1            /*Increment total number of chars*/
      c=substr(rec,k,1)              /*get character number k         */
      c.c=c.c+1                      /*increment the character's count*/
      End
    End                              /*maybe we're ½ done by now, or ¬*/

  w=length(totChars)                 /*used for right-aligning counts.*/
  say 'file -----' dsn "----- has" j-1 'records.'
  say 'file -----' dsn "----- has" totChars 'characters.'

  do L=0 to 255                      /* display nonzero letter counts */
    c=d2c(l)                         /* the character in question     */
    if c.c>0 &,                      /* was found in the file         */
       datatype(c,'M')>0 Then Do     /* and is a Latin letter         */
      say indent "(Latin) letter " c 'count:' right(c.c,w) /* tell    */
      totLetters=totLetters+c.c      /* increment number of letters   */
      End
    End

  say 'file -----' dsn "----- has" totLetters '(Latin) letters.'
  say '                           other characters follow'
  other=0
  do m=0 to 255                      /* now for non-letters           */
    c=d2c(m)                         /* the character in question     */
    y=c2x(c)                         /* the hex representation        */
    if c.c>0 &,                      /* was found in the file         */
       datatype(c,'M')=0 Then Do     /* and is not a Latin letter     */
      other=other+c.c                /* increment count               */
      _=right(c.c,w)                 /* prepare output of count       */
      select                         /*make the character viewable.   */
       when c<<' ' | m==255 then say indent  "'"y"'x character count:" _
       when c==' '          then say indent   "blank character count:" _
       otherwise                 say indent "   " c 'character count:' _
       end
     end
   end
say 'file -----' dsn "----- has" other 'other characters.'
```

Output:

```txt

>WaA  Pa12 :&-: :äüÖ2< 20
 E8C44D8FF015674BCDEF
 61100711200000000002
file ----- PRIV.V100(TEST) ----- has 1 records.
file ----- PRIV.V100(TEST) ----- has 20 characters.
                     (Latin) letter  a count:  2
                     (Latin) letter  A count:  1
                     (Latin) letter  P count:  1
                     (Latin) letter  W count:  1
file ----- PRIV.V100(TEST) ----- has 5 (Latin) letters.
                           other characters follow
                     '00'x character count:  1
                     '10'x character count:  1
                     blank character count:  3
                         & character count:  1
                         - character count:  1
                         : character count:  1
                         : character count:  1
                         ä character count:  1
                         ü character count:  1
                         Ö character count:  1
                         1 character count:  1
                         2 character count:  2
file ----- PRIV.V100(TEST) ----- has 15 other characters.
```



## Ring


```ring

textData = read("C:\Ring\ReadMe.txt")
ln =len(textData)
charCount = list(255)
totCount = 0

for i =1 to ln
    char = ascii(substr(textData,i,1))
    charCount[char] = charCount[char] + 1
    if char > 31 totCount = totCount + 1 ok
next

for i = 32 to 255
    if charCount[i] > 0 see char(i) + " = " + charCount[i] + " " + (charCount[i]/totCount)*100 + " %" + nl ok
next

```



## Ruby


```ruby
def letter_frequency(file)
  letters = 'a' .. 'z'
  File.read(file) .
       split(//) .
       group_by {|letter| letter.downcase} .
       select   {|key, val| letters.include? key} .
       collect  {|key, val| [key, val.length]}
end

letter_frequency(ARGV[0]).sort_by {|key, val| -val}.each {|pair| p pair}
```

example output, using the program file as input:

```txt
$ ruby letterFrequency.rb letterFrequency.rb
["e", 34]
["l", 20]
["t", 17]
["r", 14]
["a", 12]
["y", 9]
["c", 8]
["i", 7]
["v", 6]
["n", 6]
["f", 6]
["s", 6]
["d", 5]
["p", 5]
["k", 5]
["u", 4]
["o", 4]
["g", 3]
["b", 2]
["h", 2]
["q", 2]
["z", 1]
["w", 1]
```



### Ruby 2.0


```ruby
def letter_frequency(file)
  freq = Hash.new(0)
  file.each_char.lazy.grep(/[[:alpha:]]/).map(&:upcase).each_with_object(freq) do |char, freq_map|
    freq_map[char] += 1
  end
end

letter_frequency(ARGF).sort.each do |letter, frequency|
  puts "#{letter}: #{frequency}"
end
```


note that this version *should* use less memory, even on a gigantic file. This is done by using lazy enumerables, which ruby 2.0 introduces.

example output, using the (somewhat large) dictionary file as the input. Also note that this versions works on unicode text.


```txt
$ ruby letter_frequency.rb /usr/share/dict/words
A: 64439
B: 15526
C: 31872
D: 28531
E: 88833
F: 10675
G: 22712
H: 19320
I: 66986
J: 1948
K: 8409
L: 41107
M: 22508
N: 57144
O: 48944
P: 22274
Q: 1524
R: 57347
S: 90113
T: 53006
U: 26118
V: 7989
W: 7530
X: 2124
Y: 12652
Z: 3281
Å: 1
á: 10
â: 6
ä: 7
å: 3
ç: 5
è: 28
é: 144
ê: 6
í: 2
ñ: 8
ó: 8
ô: 2
ö: 16
û: 3
ü: 12

```



## Run BASIC


```Runbasic
open "c:\rbp101\public\textFile.txt" for input as #f
textData$ = input$(#f, lof( #f))
ln =len(textData$)
close #f

dim charCount( 255)

for i =1 to ln
   char            = asc(mid$(textData$,i,1))
   charCount(char) = charCount(char) + 1
   if char > 31 then totCount = totCount + 1
next i

for i = 32 to 255
if charCount(i) > 0 then print "Ascii:";using("###",i);" char:";chr$(i);" Count:";using("#######",charCount(i));" ";using("##.#",(charCount(i) / totCount) * 100);"%"
next i
```


Output uses this program to count itself:
<pre style="height: 40ex; overflow: scroll">
Ascii: 32 char:  Count:     76 16.1%
Ascii: 34 char:" Count:     18  3.8%
Ascii: 35 char:# Count:     17  3.6%
Ascii: 36 char:$ Count:      6  1.3%
Ascii: 37 char:% Count:      1  0.2%
Ascii: 40 char:( Count:     16  3.4%
Ascii: 41 char:) Count:     16  3.4%
Ascii: 42 char:* Count:      1  0.2%
Ascii: 43 char:+ Count:      2  0.4%
Ascii: 44 char:, Count:      6  1.3%
Ascii: 46 char:. Count:      2  0.4%
Ascii: 47 char:/ Count:      1  0.2%
Ascii: 48 char:0 Count:      4  0.8%
Ascii: 49 char:1 Count:      8  1.7%
Ascii: 50 char:2 Count:      3  0.6%
Ascii: 51 char:3 Count:      2  0.4%
Ascii: 53 char:5 Count:      4  0.8%
Ascii: 58 char:: Count:      4  0.8%
Ascii: 59 char:; Count:      8  1.7%
Ascii: 61 char:= Count:      7  1.5%
Ascii: 62 char:> Count:      2  0.4%
Ascii: 65 char:A Count:      1  0.2%
Ascii: 67 char:C Count:     10  2.1%
Ascii: 68 char:D Count:      3  0.6%
Ascii: 70 char:F Count:      1  0.2%
Ascii: 92 char:\ Count:      3  0.6%
Ascii: 97 char:a Count:     19  4.0%
Ascii: 98 char:b Count:      2  0.4%
Ascii: 99 char:c Count:     17  3.6%
Ascii:100 char:d Count:      3  0.6%
Ascii:101 char:e Count:     13  2.7%
Ascii:102 char:f Count:     10  2.1%
Ascii:103 char:g Count:      3  0.6%
Ascii:104 char:h Count:     14  3.0%
Ascii:105 char:i Count:     24  5.1%
Ascii:108 char:l Count:      7  1.5%
Ascii:109 char:m Count:      2  0.4%
Ascii:110 char:n Count:     25  5.3%
Ascii:111 char:o Count:     21  4.4%
Ascii:112 char:p Count:      6  1.3%
Ascii:114 char:r Count:     17  3.6%
Ascii:115 char:s Count:      7  1.5%
Ascii:116 char:t Count:     38  8.0%
Ascii:117 char:u Count:     16  3.4%
Ascii:120 char:x Count:      7  1.5%

```


## Rust

Works with all UTF-8 characters

```rust
use std::collections::btree_map::BTreeMap;
use std::{env, process};
use std::io::{self, Read, Write};
use std::fmt::Display;
use std::fs::File;

fn main() {
    let filename = env::args().nth(1)
        .ok_or("Please supply a file name")
        .unwrap_or_else(|e| exit_err(e, 1));

    let mut buf = String::new();
    let mut count = BTreeMap::new();

    File::open(&filename)
        .unwrap_or_else(|e| exit_err(e, 2))
        .read_to_string(&mut buf)
        .unwrap_or_else(|e| exit_err(e, 3));


    for c in buf.chars() {
        *count.entry(c).or_insert(0) += 1;
    }

    println!("Number of occurences per character");
    for (ch, count) in &count {
        println!("{:?}: {}", ch, count);
    }
}

#[inline]
fn exit_err<T>(msg: T, code: i32) -> ! where T: Display {
    writeln!(&mut io::stderr(), "{}", msg).expect("Could not write to stderr");
    process::exit(code)
}
```


Output when run on source file:


```txt

Number of occurences per character
'\n': 35
' ': 167
'!': 4
'\"': 10
'#': 1
'&': 4
'(': 25
')': 25
'*': 1
'+': 1
',': 12
'-': 1
'.': 10
'0': 1
'1': 3
'2': 2
'3': 2
':': 37
';': 13
'<': 1
'=': 4
'>': 2
'?': 1
'B': 2
'C': 1
'D': 2
'F': 2
'M': 2
'N': 1
'P': 1
'R': 1
'S': 1
'T': 5
'W': 1
'[': 1
']': 1
'_': 15
'a': 20
'b': 5
'c': 22
'd': 12
'e': 75
'f': 14
'g': 5
'h': 6
'i': 29
'k': 1
'l': 23
'm': 13
'n': 36
'o': 28
'p': 17
'r': 45
's': 33
't': 42
'u': 24
'v': 2
'w': 8
'x': 6
'y': 4
'{': 9
'|': 6
'}': 9

```



## Scala



```scala
import io.Source.fromFile

def letterFrequencies(filename: String) =
  fromFile(filename).mkString groupBy (c => c) mapValues (_.length)
```



## Scheme


Using guile scheme 2.0.11.

Note that this prints the scheme representations of characters in no particular order.


```scheme
(use-modules (ice-9 format))

(define (char-freq port table)
  (if
   (eof-object? (peek-char port))
   table
   (char-freq port (add-char (read-char port) table))))

(define (add-char char table)
  (cond
   ((null? table) (list (list char 1)))
   ((eq? (caar table) char) (cons (list char (+ (cadar table) 1)) (cdr table)))
   (#t (cons (car table) (add-char char (cdr table))))))

(define (format-table table)
  (for-each (lambda (t) (format #t "~10s~10d~%" (car t) (cadr t))) table))

(define (print-freq filename)
  (format-table (char-freq (open-input-file filename) '())))

(print-freq "letter-frequency.scm")
```


Output when reading own source:
 #\(               45
 #\u                5
 #\s                9
 #\e               47
 #\-               19
 #\m                9
 #\o               16
 #\d               19
 #\l               25
 #\space           83
 #\i               15
 #\c               28
 #\9                1
 #\f               20
 #\r               39
 #\a               47
 #\t               36
 #\)               45
 #\newline         21
 #\n               15
 #\h               14
 #\q                7
 #\p                9
 #\b               16
 #\j                1
 #\?                3
 #\k                1
 #\1                4
 #\+                1
 #\#                2
 #\"                4
 #\~                3
 #\0                2
 #\%                1
 #\'                1
 #\y                1
 #\.                1

An implementation for CHICKEN scheme:


```scheme

(with-input-from-string "foobar"
  (lambda ()
    (port-fold (lambda (x s)
                 (alist-update x
                               (add1 (alist-ref x s eq? 0))
                               s))
               '()
               read-char)))

```


which shows: ((#\f . 1) (#\o . 2) (#\b . 1) (#\a . 1) (#\r . 1))


## Seed7


```seed7
$ include "seed7_05.s7i";

const type: charHash is hash [char] integer;

const proc: main is func
  local
    var charHash: numberOfChars is charHash.EMPTY_HASH;
    var char: ch is ' ';
  begin
    ch := getc(IN);
    while ch <> EOF do
      if ch in numberOfChars then
        incr(numberOfChars[ch]);
      else
        numberOfChars @:= [ch] 1;
      end if;
      ch := getc(IN);
    end while;
    for ch range sort(keys(numberOfChars)) do
      writeln(ch <& " " <& numberOfChars[ch]);
    end for;
  end func;
```


Output when the program uses itself as input:

```txt
 22
  129
" 4
$ 1
& 2
' 2
( 6
) 6
. 2
0 1
...
s 21
t 9
u 9
v 2
w 3
y 2
```



## Sidef


```ruby
func letter_frequency(File file) {
    file.read.chars.grep{.match(/[[:alpha:]]/)} \
        .group_by {|letter| letter.downcase}    \
        .map_val  {|_, val| val.len}            \
        .sort_by  {|_, val| -val}
}

var top = letter_frequency(File(__FILE__))
top.each{|pair| say "#{pair[0]}: #{pair[1]}"}
```

```txt

e: 22
l: 17
a: 16
t: 14
r: 14
p: 12
f: 8
i: 8
n: 7
c: 6
u: 6
o: 6
v: 6
y: 5
s: 5
h: 3
w: 2
q: 2
b: 2
m: 2
g: 2
d: 1

```



## SIMPOL

Example: open a text file and compute letter frequency.

```simpol
constant iBUFSIZE 500

function main(string filename)
  fsfileinputstream fpi
  integer e, i, aval, zval, cval
  string s, buf, c
  array chars

  e = 0
  fpi =@ fsfileinputstream.new(filename, error=e)
  if fpi =@= .nul
    s = "Error, file """ + filename + """ not found{d}{a}"
  else
    chars =@ array.new()
    aval = .charval("a")
    zval = .charval("z")
    i = 1
    while i <= 26
      chars[i] = 0
      i = i + 1
    end while
    buf = .lcase(fpi.getstring(iBUFSIZE, 1))
    while not fpi.endofdata and buf > ""
      i = 1
      while i <= .len(buf)
        c = .substr(buf, i, 1)
        cval = .charval(c)
        if cval >= aval and cval <= zval
          chars[cval - aval + 1] = chars[cval - aval + 1] + 1
        end if
        i = i + 1
      end while
      buf = .lcase(fpi.getstring(iBUFSIZE, 1))
    end while

    s = "Character counts for """ + filename + """{d}{a}"
    i = 1
    while i <= chars.count()
      s = s + .char(aval + i - 1) + ": " + .tostr(chars[i], 10) + "{d}{a}"
      i = i + 1
    end while
  end if
end function s
```


As this was being created I realized that in [SIMPOL] I wouldn't have done it this way (in fact, I wrote it differently the first time and had to go back and change it to use an array afterward). In [SIMPOL] we would have used the set object. It acts similarly to a single-dimensional array, but can also use various set operations, such as difference, unite, intersect, etc. One of th einteresting things is that each unique value is stored only once, and the number of duplicates is stored with it. The sample then looks a little cleaner:


```simpol
constant iBUFSIZE 500

function main(string filename)
  fsfileinputstream fpi
  integer e, i, aval, zval
  string s, buf, c
  set chars

  e = 0
  fpi =@ fsfileinputstream.new(filename, error=e)
  if fpi =@= .nul
    s = "Error, file """ + filename + """ not found{d}{a}"
  else
    chars =@ set.new()
    aval = .charval("a")
    zval = .charval("z")
    buf = .lcase(fpi.getstring(iBUFSIZE, 1))
    while not fpi.endofdata and buf > ""
      i = 1
      while i <= .len(buf)
        c = .substr(buf, i, 1)
        if .charval(c) >= aval and .charval(c) <= zval
          chars.addvalue(c)
        end if
        i = i + 1
      end while
      buf = .lcase(fpi.getstring(iBUFSIZE, 1))
    end while

    s = "Character counts for """ + filename + """{d}{a}"
    i = 1
    while i <= chars.count()
      s = s + chars[i] + ": " + .tostr(chars.valuecount(chars[i]), 10) + "{d}{a}"
      i = i + 1
    end while
  end if
end function s
```


The final stage simply reads the totals for each character. One caveat, if a character is unrepresented, then it will not show up at all in this second implementation.


## Swift



```swift
import Foundation

let dictPath: String

switch CommandLine.arguments.count {
case 2:
  dictPath = CommandLine.arguments[1]
case _:
  dictPath = "/usr/share/dict/words"
}

let wordsData = FileManager.default.contents(atPath: dictPath)!
let allWords = String(data: wordsData, encoding: .utf8)!
let words = allWords.components(separatedBy: "\n")
let counts = words.flatMap({ $0.map({ ($0, 1) }) }).reduce(into: [:], { $0[$1.0, default: 0] += $1.1 })

for (char, count) in counts {
  print("\(char): \(count)")
}
```



## Tcl


```tcl
proc letterHistogram {fileName} {
    # Initialize table (in case of short texts without every letter)
    for {set i 97} {$i<=122} {incr i} {
        set frequency([format %c $i]) 0
    }
    # Iterate over characters in file
    set f [open $fileName]
    foreach c [split [read $f] ""] {
        # Count them if they're alphabetic
        if {[string is alpha $c]} {
            incr frequency([string tolower $c])
        }
    }
    close $f
    # Print the histogram
    parray frequency
}

letterHistogram the/sample.txt
```


## TUSCRIPT


```tuscript

$$ MODE TUSCRIPT
words = REQUEST ("http://www.puzzlers.org/pub/wordlists/unixdict.txt")

DICT letters create
MODE {}
COMPILE
LOOP word=words
 letters=SPLIT (word,|":?:")
 LOOP letter=letters
  DICT letters ADD/QUIET/COUNT letter
 ENDLOOP
ENDLOOP
ENDCOMPILE
DICT letters unload letter,size,cnt

index    =DIGIT_INDEX (cnt)
index    =REVERSE (index)
letter   =INDEX_SORT (letter,index)
cnt      =INDEX_SORT (cnt,index)
frequency=JOIN (letter," --- ",cnt)

*{frequency}

```

Output:
<pre style='height:30ex;overflow:scroll'>
e --- 20144
a --- 16421
i --- 13980
r --- 13436
t --- 12836
o --- 12738
n --- 12097
s --- 10210
l --- 10061
c --- 8216
u --- 6489
m --- 5828
d --- 5799
p --- 5516
h --- 5208
g --- 4129
b --- 4115
y --- 3633
f --- 2662
w --- 1968
k --- 1925
v --- 1902
x --- 617
z --- 433
j --- 430
q --- 378
' --- 105
. --- 6
& --- 6
1 --- 2
9 --- 1
8 --- 1
7 --- 1
6 --- 1
5 --- 1
4 --- 1
3 --- 1
2 --- 1
0 --- 1

```



## TXR



### TXR Extraction Language plus TXR Lisp



```txr
@(do (defvar h (hash :equal-based)))
@(repeat)
@(coll :vars ())@\
  @{letter /[A-Za-z]/}@(filter :upcase letter)@\
  @(do (inc [h letter 0]))@\
@(end)
@(end)
@(do (dohash (key value h)
       (format t "~a: ~a\n" key value)))
```


```txt
$ ./txr letterfreq.txr /usr/share/dict/words
A: 64123
B: 15524
C: 31569
[ ... abridged ... ]
X: 2124
Y: 12507
Z: 3238
```



### TXR Lisp



```txrlisp
(let* ((s (open-file "/usr/share/dict/words" "r"))
       (chrs [keep-if* chr-isalpha (gun (get-char s))])
       (h [group-reduce (hash) chr-toupper (op succ @1) chrs 0]))
  (dohash (key value h)
    (put-line `@key: @value`)))
```



## Vala

Counts every character except new line character.

```vala

using Gee;

void main(string[] args){
    string filename = args[1];
    var file = FileStream.open(filename, "r");

    var	counter	= new HashMap<char, int>();

    string line = file.read_line();
    while (line != null){
        for (int x = 0;	x < line.length; x++){
            counter[line[x]] = counter[line[x]] + 1;
	}
        line = file.read_line();
    }

    foreach (var elem in counter.entries){
	stdout.printf("%c occured %d times\n", elem.key, elem.value);
    }
}

```


Sample output (run on its own source code) with several lines omitted:

```txt

v occured 5 times
, occured 4 times
w occured 2 times
	 occured 19 times
S occured 1 times
1 occured 2 times
! occured 1 times
k occured 1 times
l occured 22 times

```


## VBA



```VBA

Public Sub LetterFrequency(fname)
'count number of letters in text file "fname" (ASCII-coded)
'note: we count all characters but print only the letter frequencies

Dim Freqs(255) As Long
Dim abyte As Byte
Dim ascal as Byte 'ascii code for lowercase a
Dim ascau as Byte 'ascii code for uppercase a

'try to open the file
On Error GoTo CantOpen
Open fname For Input As #1
On Error GoTo 0

'initialize
For i = 0 To 255
  Freqs(i) = 0
Next i

'process file byte-per-byte
While Not EOF(1)
 abyte = Asc(Input(1, #1))
 Freqs(abyte) = Freqs(abyte) + 1
Wend
Close #1

'add lower and upper case together and print result
Debug.Print "Frequencies:"
ascal = Asc("a")
ascau = Asc("A")
For i = 0 To 25
  Debug.Print Chr$(ascal + i), Freqs(ascal + i) + Freqs(ascau + i)
Next i
Exit Sub

CantOpen:
  Debug.Print "can't find or read the file "; fname
  Close
End Sub

```


Output:

```txt

LetterFrequency "d:\largetext.txt"
Frequencies:
a              24102
b              4985
c              4551
d              19127
e              61276
f              2734
g              10661
h              8243
i              21589
j              4904
k              7186
l              12026
m              7454
n              31963
o              19021
p              4960
q              37
r              21166
s              13403
t              21090
u              6117
v              8612
w              5017
x              168
y              299
z              4159

```



## VBScript


```vb

filepath = "SPECIFY FILE PATH HERE"

Set objfso = CreateObject("Scripting.FileSystemObject")
Set objdict = CreateObject("Scripting.Dictionary")
Set objfile = objfso.OpenTextFile(filepath,1)

txt = objfile.ReadAll

For i = 1 To Len(txt)
	char = Mid(txt,i,1)
	If objdict.Exists(char) Then
		objdict.Item(char) = objdict.Item(char) + 1
	Else
		objdict.Add char,1
	End If
Next

For Each key In objdict.Keys
	WScript.StdOut.WriteLine key & " = " & objdict.Item(key)
Next

objfile.Close
Set objfso = Nothing
Set objdict = Nothing

```



## Vedit macro language



```vedit
File_Open("c:\txt\a_text_file.txt")
Update()

for (#1='A'; #1<='Z'; #1++) {
    Out_Reg(103) Char_Dump(#1,NOCR) Out_Reg(CLEAR)
    #2 = Search(@103, BEGIN+ALL+NOERR)
    Message(@103) Num_Type(#2)
}
```


Example output:

```txt

A   76
B   23
C   51
D   64
E  192
F   51
G   32
H   59
I  146
J    1
K    9
L   73
M   34
N   94
O  113
P   27
Q    1
R   92
S   89
T  138
U   63
V   26
W   35
X   16
Y   16
Z    2

```



## Whitespace



```Whitespace






















































```


```asm
push 127
; Initialize a slot in the heap for each ASCII character.
0:
    dup
    push 0
    store
    push 1
    sub
    dup
    jn 1
    jump 0
; Read until EOF, incrementing the relevant heap slot.
1:
    push 0
    dup
    ichr
    load
    dup
    jn 2 ; Done reading, proceed to print.
    dup
    load
    push 1
    add
    store
    jump 1
; Stack is [-1 -1], but [0] would be nice.
2:
    sub
; Print characters with tallies greater than 0.
3:
    push 1
    add
    dup
    push 128
    sub
    jz 4 ; All done.
    dup
    load
    jz 3 ; Don't print if no occurrences.
    dup
    ochr ; Display the character,
    push 32
    ochr ; a space,
    dup
    load
    onum ; its frequency,
    push 10
    ochr ; and a newline.
    jump 3
4:
    pop
    exit
```


```txt
$ cat freq.ws | wspace freq.ws
	 64

 55
  119
```



## XPL0


This takes advantage of DOS's ability to redirect input from a file to
the keyboard. The input file must be terminated with an end-of-file
character ($1A). Usage: count <filename.ext


```XPL0
include c:\cxpl\codes;          \intrinsic 'code' declarations
int A(256), C, I;
[for C:= 0 to 256-1 do A(C):= 0;
repeat  C:= ChIn(1);            \device 1 doesn't buffer nor echo chars
        A(C):= A(C)+1;          \count character
until   C=\EOF\$1A;
C:= 0;
for I:= 0 to 128-1 do           \only show 7-bit ASCII
        [ChOut(0, \tab\9);
        case C of
          $0A: ChOut(6, $19);   \line feed = down arrow
          $0D: ChOut(6, $1B)    \carriage return = left arrow
        other  ChOut(6, C);     \all other characters display on device 6
        ChOut(0, ^ );
        IntOut(0, A(C));        \show count
        C:= C+16;               \columnar order
        if (I&7) = 7 then [CrLf(0);  C:= C-8*16+1];
        ];
]
```


Example output of count.xpl counting itself:
[[File:Count2XPL0.gif]]


## Yabasic


```Yabasic
dim ascCodes(255)

f = open("unixdict.txt", "r")

if f then
    while(not eof(#f))
        line input #f a$
        for i = 1 to len(a$)
            c = asc(mid$(a$, i, 1))
            ascCodes(c) = ascCodes(c) + 1
        next
    wend
    for i = 1 to 255
        c = ascCodes(i)
        if c print chr$(i), " = ", c
    next
    close #f
end if
```



## zkl


```zkl
fcn ccnt(textInBitBucket){
   letters:=["a".."z"].pump(List().write,0); // array of 26 zeros
   textInBitBucket.howza(0).pump(Void,'wrap(c){  // pump text as ints
      if(97<=c<=122)     c-=97;
      else if(65<=c<=90) c-=65;
      else return(Void.Skip);
      letters[c]+=1
   });
   sum:=letters.sum(); println(sum," letters");
   letters.enumerate().pump(List,'wrap([(c,n)]){
      "%s(%d:%d%)".fmt((c+65).toChar(),n,n*100/sum)})
   .concat(",").println();
}

ccnt(Data(0,Int,"This is a test"));
ccnt(File("dict.txt").read());
```

```txt

11 letters
A(1:9%),B(0:0%),C(0:0%),D(0:0%),E(1:9%),F(0:0%),G(0:0%),H(1:9%),I(2:18%),J(0:0%),K(0:0%),L(0:0%),M(0:0%),N(0:0%),O(0:0%),P(0:0%),Q(0:0%),R(0:0%),S(3:27%),T(3:27%),U(0:0%),V(0:0%),W(0:0%),X(0:0%),Y(0:0%),Z(0:0%)

181171 letters
A(16421:9%),B(4115:2%),C(8216:4%),D(5799:3%),E(20144:11%),F(2662:1%),G(4129:2%),H(5208:2%),I(13980:7%),J(430:0%),K(1925:1%),L(10061:5%),M(5828:3%),N(12097:6%),O(12738:7%),P(5516:3%),Q(378:0%),R(13436:7%),S(10210:5%),T(12836:7%),U(6489:3%),V(1902:1%),W(1968:1%),X(617:0%),Y(3633:2%),Z(433:0%)

```

