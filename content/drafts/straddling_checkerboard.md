+++
title = "Straddling checkerboard"
description = ""
date = 2019-08-12T14:35:01Z
aliases = []
[extra]
id = 9882
[taxonomies]
categories = []
tags = []
+++

{{task|Encryption}}

;Task:
Implement functions to encrypt and decrypt a message using the [[wp:Straddling_checkerboard|straddling checkerboard]] method. The checkerboard should take a 28 character alphabet (A-Z plus a full stop and an escape character) and two different numbers representing the blanks in the first row. The output will be a series of decimal digits. 

Numbers should be encrypted by inserting the escape character before each digit, then including the digit unencrypted. This should be reversed for decryption.





## ALGOL 68

{{trans|C++}} Note: This specimen retains the original [[#C++|C++]] coding style.
{{works with|ALGOL 68|Revision 1 - no extensions to language used.}}
{{works with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-1.18.0/algol68g-1.18.0-9h.tiny.el5.centos.fc11.i386.rpm/download 1.18.0-9h.tiny].}}
{{wont work with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8-8d] - due to extensive use of '''format'''[ted] ''transput''.}}

```algol68
#!/usr/local/bin/a68g --script #

PRIO MIN=5, MAX=5;
OP MIN = (INT a, b)INT: (a<b|a|b),
   MAX = (INT a, b)INT: (a>b|a|b);

MODE STRADDLINGCHECKERBOARD=STRUCT(
  [0:9]CHAR first, second, third,
  [ABS ".": ABS "Z"]STRING table,
  INT row u, row v,
  CHAR esc, skip
);

STRUCT(
  PROC (REF STRADDLINGCHECKERBOARD #self#, STRING #alphabet#, INT #u#, INT #v#)VOID init,
  PROC (REF STRADDLINGCHECKERBOARD #self#, STRING #plain#)STRING encode,
  PROC (REF STRADDLINGCHECKERBOARD #self#, STRING #plain#)STRING decode
) sc class = (
# PROC init = # (REF STRADDLINGCHECKERBOARD self, STRING in alphabet, INT u, v)VOID:
  (
    STRING alphabet = in alphabet[@0];

    esc OF self := alphabet[UPB alphabet]; # use the last CHAR as the escape #
    skip OF self := alphabet[UPB alphabet-1];

    row u OF self := u MIN v;
    row v OF self := u MAX v;

    OP DIGIT = (INT i)CHAR: REPR(ABS "0" + i );

    INT j := LWB alphabet;

  # (first OF self)[u] := (first OF self)[v] := skip; #

    FOR i FROM LWB first OF self TO UPB first OF self  DO
      IF i NE u AND i NE v THEN
        (first OF self)[i] := alphabet[j];
        (table OF self)[ABS alphabet[j]] := DIGIT i;
        j+:=1
      FI;

      (second OF self)[i] := alphabet[i+8];
      (table OF self)[ABS alphabet[i+8]] := DIGIT (row u OF self) + DIGIT i;

      (third OF self)[i] := alphabet[i+18];
      (table OF self)[ABS alphabet[i+18]] := DIGIT (row v OF self) + DIGIT i
    OD
  ),

# PROC encode = # (REF STRADDLINGCHECKERBOARD self, STRING plain)STRING:
  (
    STRING esc = (table OF self)[ABS (esc OF self)];
    INT l2u = ABS "A" - ABS "a";

    STRING out := "";
    FOR i FROM LWB plain TO UPB plain DO
      CHAR c := plain[i];
      IF "a" <= c AND c <= "z" THEN
        c := REPR ( ABS c + l2u) FI;

      IF "A" <= c AND c <= "Z" THEN
        out +:= (table OF self)[ABS c]
      ELIF "0" <= c AND c <= "9" THEN
        out +:= esc + c
      FI
    OD;
    out # EXIT #
  ),

# PROC decode = # (REF STRADDLINGCHECKERBOARD self, STRING cipher)STRING:
  (
    CHAR null = REPR 0;
    STRING out;
    INT state := 0;
    FOR i FROM LWB cipher TO UPB cipher DO
      INT n := ABS cipher[i] - ABS "0";

      CHAR next :=
        CASE state IN
          #1:# (second OF self)[n],
          #2:# (third OF self)[n],
          #3:# cipher[i]
        OUT
          IF n = row u OF self THEN
            state := 1; null
          ELIF n = row v OF self THEN
            state := 2; null
          ELSE
            (first OF self)[n]
          FI
        ESAC;

      IF next = "/" THEN
        state := 3
      ELIF next NE null THEN
        state := 0;
        out +:= next
      FI
    OD;
    out # EXIT #
  )
);

main:
(
  STRADDLINGCHECKERBOARD sc; (init OF sc class)(sc, "HOLMESRTABCDFGIJKNPQUVWXYZ./", 3, 7);

  STRING original := "One night-it was on the twentieth of March, 1888-I was returning"[@0];
  STRING en := (encode OF sc class)(sc, original);
  STRING de := (decode OF sc class)(sc, en);

  printf(($ggl$,
          "Original: ", original,
          "Encoded:  ", en,
          "Decoded:  ", de
  ))

)
```

Output:

```txt

Original: One night-it was on the twentieth of March, 1888-I was returning
Encoded:  139539363509369743061399059745399365901344308320791798798798367430685972839363935
Decoded:  ONENIGHTITWASONTHETWENTIETHOFMARCH1888IWASRETURNING

```



## AutoHotkey


```AutoHotkey
board := "
(
ET AON RIS
BCDFGHJKLM
PQ/UVWXYZ.
)"
Text = One night-it was on the twentieth of March, 1888-I was returning
StringUpper, Text, Text
Text := RegExReplace(text, "[^A-Z0-9]")
Num2 := InStr(board, A_Space)               -1
Num3 := InStr(board, A_Space, true, Num1+1) -1
Loop Parse, Text
{
	char := A_LoopField
	Loop Parse, board, `n
	{
		If (Pos := InStr(A_LoopField, char))
			out .= Num%A_Index% . Pos-1
		else if (Pos := InStr(A_LoopField, "/")) && InStr("0123456789", char)
			out .= Num%A_Index% . Pos-1 . char
	}
}
MsgBox % out

i := 0
While (LoopField := SubStr(out, ++i, 1)) <> ""
{
	If (LoopField = num2) or (LoopField = num3)
		col := SubStr(out, ++i, 1)
	else	col := 0
	Loop Parse, board, `n
	{
		If !col && A_Index = 1
			dec .= SubStr(A_LoopField, LoopField+1, 1)
		Else If (Num%A_Index% = LoopField) && col
			dec .= SubStr(A_LoopField, col+1, 1)
		If SubStr(dec, 0) = "/"
			dec := SubStr(dec, 1, StrLen(dec)-1) . SubStr(out, ++i, 1)
	}
}
MsgBox % dec
```


## C

{{libheader|GLib}}


```c>#include <stdio.h

#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <ctype.h>
#include <glib.h>

#define ROWS   4
#define COLS  10
#define NPRX  "/"

/* wikipedia table
const char *table[ROWS][COLS] =
{
  { "0", "1", "2",  "3", "4", "5", "6",  "7", "8", "9" },
  { "E", "T", NULL, "A", "O", "N", NULL, "R", "I", "S  },
  { "B", "C", "D",  "F", "G", "H", "J",  "K", "L", "M" },
  { "P", "Q", NPRX, "U", "V", "W", "X",  "Y", "Z", "." }
};
*/

/* example of extending the table, COLS must be 11
const char *table[ROWS][COLS] =
{
  { "0", "1", "2", "3",  "4", "5", "6", "7",  "8", "9",  ":"  },
  { "H", "O", "L", NULL, "M", "E", "S", NULL, "R", "T",  ","  },
  { "A", "B", "C", "D",  "F", "G", "I", "J",  "K", "N",  "-"  },
  { "P", "Q", "U", "V",  "W", "X", "Y", "Z",  ".", NPRX, "?"  }
};
*/

// task table 
const char *table[ROWS][COLS] =
{
  { "0", "1", "2", "3",  "4", "5", "6", "7",  "8", "9"  },
  { "H", "O", "L", NULL, "M", "E", "S", NULL, "R", "T"  },
  { "A", "B", "C", "D",  "F", "G", "I", "J",  "K", "N"  },
  { "P", "Q", "U", "V",  "W", "X", "Y", "Z",  ".", NPRX }
};


GHashTable *create_table_from_array(const char *table[ROWS][COLS], bool is_encoding)
{
  char buf[16];

  GHashTable *r = g_hash_table_new_full(g_str_hash, g_str_equal, free, free);
  size_t i, j, k, m;

  for(i = 0, m = 0; i < COLS; i++)
  {
    if (table[1][i] == NULL) m++;
  }

  const size_t SELNUM = m;
  
  size_t selectors[SELNUM];
  size_t numprefix_row, numprefix_col;
  bool has_numprefix = false;

  // selectors keep the indexes of the symbols to select 2nd and 3rd real row;
  // nulls must be placed into the 2nd row of the table
  for(i = 0, k = 0; i < COLS && k < SELNUM; i++)
  {
    if ( table[1][i] == NULL )
    {
      selectors[k] = i;
      k++;
    }
  }

  // numprefix is the prefix to insert symbols from the 1st row of table (numbers)
  for(j = 1; j < ROWS; j++)
  {
    for(i = 0; i < COLS; i++)
    {
      if (table[j][i] == NULL) continue;
      if ( strcmp(table[j][i], NPRX) == 0 )
      {
        numprefix_col = i;
        numprefix_row = j;
        has_numprefix = true;
	break;
      }
    }
  }

  // create the map for each symbol
  for(i = has_numprefix ? 0 : 1; i < ROWS; i++)
  {
    for(j = 0; j < COLS; j++)
    {
      if (table[i][j] == NULL) continue;
      if (strlen(table[i][j]) > 1)
      {
	fprintf(stderr, "symbols must be 1 byte long\n");
	continue; // we continue just ignoring the issue
      }
      if (has_numprefix && i == (ROWS-1) && j == numprefix_col && i == numprefix_row) continue;
      if (has_numprefix && i == 0)
      {
	snprintf(buf, sizeof(buf), "%s%s%s", table[0][selectors[SELNUM-1]], table[0][numprefix_col], table[0][j]);
      }
      else if (i == 1)
      {
	snprintf(buf, sizeof(buf), "%s", table[0][j]);
      }
      else
      {
	snprintf(buf, sizeof(buf), "%s%s", table[0][selectors[i-2]], table[0][j]);
      }
      if (is_encoding) g_hash_table_insert(r, strdup(table[i][j]), strdup(buf));
      else g_hash_table_insert(r, strdup(buf), strdup(table[i][j]));
    }
  }
  if (is_encoding) g_hash_table_insert(r, strdup("mode"), strdup("encode"));
  else g_hash_table_insert(r, strdup("mode"), strdup("decode"));

  return r;
}

char *decode(GHashTable *et, const char *enctext)
{
  char *r = NULL;

  if (et == NULL || enctext == NULL || strlen(enctext) == 0 ||
      g_hash_table_lookup(et, "mode") == NULL ||
      strcmp(g_hash_table_lookup(et, "mode"), "decode") != 0) return NULL;

  GString *res = g_string_new(NULL);
  GString *en = g_string_new(NULL);

  for( ; *enctext != '\0'; enctext++ )
  {
    if (en->len < 3)
    {
      g_string_append_c(en, *enctext);
      r = g_hash_table_lookup(et, en->str);
      if (r == NULL) continue;
      g_string_append(res, r);
      g_string_truncate(en, 0);
    }
    else
    {
      fprintf(stderr, "decoding error\n");
      break;
    }
  }
  
  r = res->str;
  g_string_free(res, FALSE);
  g_string_free(en, TRUE);
  return r;
}

char *encode(GHashTable *et, const char *plaintext, int (*trasf)(int), bool compress_spaces)
{
  GString *s;
  char *r = NULL;
  char buf[2] = { 0 };

  if (plaintext == NULL ||
      et == NULL || g_hash_table_lookup(et, "mode") == NULL ||
      strcmp(g_hash_table_lookup(et, "mode"), "encode") != 0) return NULL;

  s = g_string_new(NULL);

  for(buf[0] = trasf ? trasf(*plaintext) : *plaintext; 
      buf[0] != '\0'; 
      buf[0] = trasf ? trasf(*++plaintext) : *++plaintext)
  {
    if ( (r = g_hash_table_lookup(et, buf)) != NULL )
    {
      g_string_append(s, r);
    }
    else if (isspace(buf[0])) 
    {
      if (!compress_spaces) g_string_append(s, buf);
    } 
    else
    {
      fprintf(stderr, "char '%s' is not encodable%s\n",
	      isprint(buf[0]) ? buf : "?", 
	      !compress_spaces ? ", replacing with a space" : "");
      if (!compress_spaces) g_string_append_c(s, ' ');
    }
  }

  r = s->str;
  g_string_free(s, FALSE);
  return r;
}


int main()
{
  GHashTable *enctab = create_table_from_array(table, true);  // is encoding? true
  GHashTable *dectab = create_table_from_array(table, false); // is encoding? false (decoding)

  const char *text = "One night-it was on the twentieth of March, 1888-I was returning";

  char *encoded = encode(enctab, text, toupper, true);
  printf("%s\n", encoded);

  char *decoded = decode(dectab, encoded);
  printf("%s\n", decoded);

  free(decoded);
  free(encoded);
  g_hash_table_destroy(enctab);
  g_hash_table_destroy(dectab);

  return 0;
}
```



### Shorter version


```C>#include <stdio.h

#include <stdlib.h>

const char * board =  "ET AON RIS"
                      "BCDFGHJKLM"
                      "PQ/UVWXYZ.";

char encode[128] = {0};
char decode[128] = {0};
int row[2] = {0};

void read_table(const char *s)
{
        int i, code;
        for (i = 0; i < 30; i++) {
                if (s[i] == '\0') {
                        fprintf(stderr, "Table too short\n");
                        exit(1);
                }

                if (s[i] == ' ') {
                        row[  row[0] ? 1 : 0 ] = i;
                        continue;
                }

                code = ((i < 10) ? 0 : i < 20 ? row[0] : row[1])
                                * 10 + (i % 10);
                encode[0 + s[i]] = code; /* guess what 0 + s[i] does, sigh */
                decode[code] = s[i];
        }
}

void encipher(const char *in, char *out, int strip)
{
#define PUTCODE(c) { if (c > 9) {*(out++) = c / 10 + '0'; c %= 10;} *(out++) = c + '0'; }
        int c, code;
        while ((c = *(in++)) != '\0') {
                if (c >= '0' && c <= '9') {
                        code = encode['.'];
                        c -= '0';
                        PUTCODE(code);
                        PUTCODE(c);
                        continue;
                }

                c &= ~0x20;

                if (c >= 'A' && c <= 'Z') code = encode[c];
                else if (strip && !c )    continue;
                else                      code = encode['/'];

                PUTCODE(code);
        }
        *(out++) = '\0';
}

void decipher(const char *in, char *out, int strip)
{
        int c;
        while ((c = *(in++)) != '\0') {
                c -= '0';
                if (c == row[0] || c == row[1])
                        c = c * 10 + *(in++) - '0';

                c = decode[c];

                if (c == '.') c = *(in++);
                if (c == '/' && !strip) c = ' ';
                *(out++) = c;
        }
        *(out++) = '\0';
}

int main()
{
        const char *msg = "In the winter 1965/we were hungry/just barely alive";
        char enc[100] = {0}, dec[100] = {0};
        read_table(board);

        printf("message: %s\n", msg);
        encipher(msg, enc, 0); printf("encoded: %s\n", enc);
        decipher(enc, dec, 0); printf("decoded: %s\n", dec);

        printf("\nNo spaces:\n");
        encipher(msg, enc, 1); printf("encoded: %s\n", enc);
        decipher(enc, dec, 1); printf("decoded: %s\n", dec);
        return 0;
}
```
Output:<lang>message: In the winter 1965/we were hungry/just barely alive
encoded: 85621250626585107626916996966956265062650706225635247676226639162203702867623288640
decoded: IN THE WINTER 1965 WE WERE HUNGRY JUST BARELY ALIVE

No spaces:
encoded: 851250658510769169969669562650650702563524767622663912037028673288640
decoded: INTHEWINTER1965/WEWEREHUNGRY/JUSTBARELYALIVE
```



## C#

Translation of [[Straddling_checkerboard#Java|Java]] via [[Straddling_checkerboard#D|D]]

```csharp
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;

namespace StraddlingCheckerboard
{
    class Program
    {
        public readonly static IReadOnlyDictionary<char, string> val2Key;
        public readonly static IReadOnlyDictionary<string, char> key2Val;

        static Program()
        {
            val2Key = new Dictionary<char, string> {
                {'A',"30"},  {'B',"31"}, {'C',"32"},  {'D',"33"},  {'E',"5"},   {'F',"34"},  {'G',"35"},
                {'H',"0"},   {'I',"36"}, {'J',"37"},  {'K',"38"},  {'L',"2"},   {'M',"4"},   {'.',"78"},
                {'N',"39"},  {'/',"79"}, {'O',"1"},   {'0',"790"}, {'P',"70"},  {'1',"791"}, {'Q',"71"},
                {'2',"792"}, {'R',"8"},  {'3',"793"}, {'S',"6"},   {'4',"794"}, {'T',"9"},   {'5',"795"},
                {'U',"72"},  {'6',"796"},{'V',"73"},  {'7',"797"}, {'W',"74"},  {'8',"798"}, {'X',"75"},
                {'9',"799"}, {'Y',"76"}, {'Z',"77"}};

            key2Val = val2Key.ToDictionary(kv => kv.Value, kv => kv.Key);
        }

        public static string Encode(string s)
        {
            return string.Concat(s.ToUpper().ToCharArray()
                .Where(c => val2Key.ContainsKey(c)).Select(c => val2Key[c]));
        }

        public static string Decode(string s)
        {
            return string.Concat(Regex.Matches(s, "79.|7.|3.|.").Cast<Match>()
                .Where(m => key2Val.ContainsKey(m.Value)).Select(m => key2Val[m.Value]));
        }

        static void Main(string[] args)
        {
            var enc = Encode("One night-it was on the twentieth of March, 1888-I was returning");
            Console.WriteLine(enc);
            Console.WriteLine(Decode(enc));

            Console.ReadLine();
        }
    }
}
```


```txt
139539363509369743061399059745399365901344308320791798798798367430685972839363935
ONENIGHTITWASONTHETWENTIETHOFMARCH1888IWASRETURNING
```



## C++


```cpp>#include <iostream

#include <string>
#include <map>
#include <algorithm> // for min, max
using namespace std;

class StraddlingCheckerboard
{
  map<char, string> table;
  char first[10], second[10], third[10];
  int rowU, rowV;

public:
  StraddlingCheckerboard(const string &alphabet, int u, int v)
  {
    rowU = min(u, v);
    rowV = max(u, v);

    for(int i = 0, j = 0; i < 10; ++i)
    {
      if(i != u && i != v)
      {
        first[i] = alphabet[j];
        table[alphabet[j]] = '0' + i;
        ++j;
      }

      second[i] = alphabet[i+8];
      table[alphabet[i+8]] = '0' + rowU;
      table[alphabet[i+8]] += '0' + i;

      third[i] = alphabet[i+18];
      table[alphabet[i+18]] = '0' + rowV;
      table[alphabet[i+18]] += '0' + i;
    }
  }

  string encode(const string &plain)
  {
    string out;
    for(int i = 0; i < plain.size(); ++i)
    {
      char c = plain[i];
      if(c >= 'a' && c <= 'z')
        c += 'A' - 'a';

      if(c >= 'A' && c <= 'Z')
        out += table[c];
      else if(c >= '0' && c <= '9')
      {
        out += table['/'];
        out += c;
      }
    }
    return out;
  }

  string decode(const string &cipher)
  {
    string out;
    int state = 0;
    for(int i = 0; i < cipher.size(); ++i)
    {
      int n = cipher[i] - '0';
      char next = 0;

      if(state == 1)
        next = second[n];
      else if(state == 2)
        next = third[n];
      else if(state == 3)
        next = cipher[i];
      else if(n == rowU)
        state = 1;
      else if(n == rowV)
        state = 2;
      else
        next = first[n];

      if(next == '/')
        state = 3;
      else if(next != 0)
      {
        state = 0;
        out += next;
      }
    }
    return out;
  }
};
```


Test program:

```cpp
int main()
{
  StraddlingCheckerboard sc("HOLMESRTABCDFGIJKNPQUVWXYZ./", 3, 7);
  
  string original = "One night-it was on the twentieth of March, 1888-I was returning";
  string en = sc.encode(original);
  string de = sc.decode(en);

  cout << "Original: " << original << endl;
  cout << "Encoded:  " << en << endl;
  cout << "Decoded:  " << de << endl;

  return 0;
}
```


Output:

```txt

Original: One night-it was on the twentieth of March, 1888-I was returning
Encoded:  139539363509369743061399059745399365901344308320791798798798367430685972839363935
Decoded:  ONENIGHTITWASONTHETWENTIETHOFMARCH1888IWASRETURNING

```



## D

Partially based on the PicoLisp version:

```d
import std.stdio, std.algorithm, std.string, std.array;

immutable T = ["79|0|1|2|3|4|5|6|7|8|9", "|H|O|L||M|E|S||R|T",
               "3|A|B|C|D|F|G|I|J|K|N", "7|P|Q|U|V|W|X|Y|Z|.|/"]
              .map!(r => r.split("|")).array;

enum straddle = (in string s) pure /*nothrow @safe*/ =>
    toUpper(s)
    .split("")
    .cartesianProduct(T)
    .filter!(cL => cL[1].canFind(cL[0]))
    .map!(cL => cL[1][0] ~ T[0][cL[1].countUntil(cL[0])])
    .join;

string unStraddle(string s) pure nothrow @safe {
    string result;
    for (; !s.empty; s.popFront) {
        immutable i = [T[2][0], T[3][0]].countUntil([s[0]]);
        if (i >= 0) {
            s.popFront;
            immutable n = T[2 + i][T[0].countUntil([s[0]])];
            if (n == "/") {
                s.popFront;
                result ~= s[0];
            } else result ~= n;
        } else
            result ~= T[1][T[0].countUntil([s[0]])];
    }
    return result;
}

void main() {
    immutable O = "One night-it was on the twentieth of March, 1888-I was returning";
    writeln("Encoded: ", O.straddle);
    writeln("Decoded: ", O.straddle.unStraddle);
}
```

{{out}}

```txt
Encoded: 139539363509369743061399059745399365901344308320791798798798367430685972839363935
Decoded: ONENIGHTITWASONTHETWENTIETHOFMARCH1888IWASRETURNING
```



### Alternative Version

{{trans|C++}}
Same output:

```d
import std.stdio, std.algorithm, std.ascii;

struct StraddlingCheckerboard {
  private:
    string[char] table;
    char[10] first, second, third;
    const int rowU, rowV;

  public:
    this(in string alphabet, in int u, in int v) pure nothrow {
        rowU = min(u, v);
        rowV = max(u, v);

        int j = 0;
        foreach (immutable i; 0 .. 10) {
            if (i != u && i != v) {
                first[i] = alphabet[j];
                table[alphabet[j]] = digits[i .. i + 1];
                j++;
            }

            second[i] = alphabet[i + 8];
            table[alphabet[i + 8]] = [digits[rowU], digits[i]];

            third[i] = alphabet[i + 18];
            table[alphabet[i + 18]] = [digits[rowV], digits[i]];
        }
    }

    string encode(in string plain) const pure nothrow {
        string r;
        foreach (immutable char c; plain) {
            if      (c.isLower) r ~= table[c.toUpper];
            else if (c.isUpper) r ~= table[c];
            else if (c.isDigit) r ~= table['/'] ~ c;
        }
        return r;
    }

    string decode(in string cipher) const pure nothrow {
        string r;
        int state = 0;

        foreach (immutable char c; cipher) {
            immutable int n = c - '0';
            char next = '\0';

            if (state == 1)      next = second[n];
            else if (state == 2) next = third[n];
            else if (state == 3) next = c;
            else if (n == rowU)  state = 1;
            else if (n == rowV)  state = 2;
            else                 next = first[n];

            if (next == '/')
                state = 3;
            else if (next != 0) {
                state = 0;
                r ~= next;
            }
        }
        return r;
    }
}

void main() {
    immutable orig =
    "One night-it was on the twentieth of March, 1888-I was returning";
    writeln("Original: ", orig);
    const sc = StraddlingCheckerboard("HOLMESRTABCDFGIJKNPQUVWXYZ./",
                                      3, 7);
    const en = sc.encode(orig);
    writeln("Encoded:  ", en);
    writeln("Decoded:  ", sc.decode(en));
}
```


===Dictionary-Based Version===
{{trans|Java}}

```d
import std.stdio, std.string, std.algorithm, std.regex, std.array, std.range, std.typecons;

immutable string[const string] val2key, key2val;

static this() pure /*nothrow @safe*/ {
    immutable aa = ["A":"30", "B":"31", "C":"32", "D":"33", "E":"5", "F":"34", "G":"35",
        "H":"0", "I":"36", "J":"37", "K":"38", "L":"2", "M":"4", ".":"78", "N":"39",
        "/":"79", "O":"1", "0":"790", "P":"70", "1":"791", "Q":"71", "2":"792",
        "R":"8", "3":"793", "S":"6", "4":"794", "T":"9", "5":"795", "U":"72",
        "6":"796", "V":"73", "7":"797", "W":"74", "8":"798", "X":"75", "9":"799",
        "Y":"76", "Z":"77"];
    val2key = aa;
    key2val = aa.byKeyValue.map!(t => tuple(t.value, t.key)).assocArray;
}

string encode(in string s) pure /*nothrow*/ @safe {
    return s.toUpper.split("").map!(c => val2key.get(c, "")).join;
}

string decode(in string s) /*pure nothrow*/ @safe {
    return s.matchAll("79.|3.|7.|.").map!(g => key2val.get(g[0], "")).join;
}

void main() @safe {
    immutable s = "One night-it was on the twentieth of March, 1888-I was returning";
    s.encode.writeln;
    s.encode.decode.writeln;
}
```

{{out}}

```txt
139539363509369743061399059745399365901344308320791798798798367430685972839363935
ONENIGHTITWASONTHETWENTIETHOFMARCH1888IWASRETURNING
```


=={{header|F_Sharp|F#}}==

```fsharp

(*
Encode and Decode using StraddlingCheckerboard
Nigel Galloway May 15th., 2017
*)
type G={n:char;i:char;g:System.Collections.Generic.Dictionary<(char*char),string>;e:System.Collections.Generic.Dictionary<char,string>}
       member G.encode n=n|>Seq.map(fun n->if (n='/') then G.e.['/']+string n else match (G.e.TryGetValue(n)) with |(true,n)->n|(false,_)->G.e.['/']+string n)
       member G.decode n =
         let rec fn n = seq{ if not (Seq.isEmpty n) 
           then match (match Seq.head n with |g when g=G.n||g=G.i->(G.g.[(g,(Seq.item 1 n))],(Seq.skip 2 n))|g->(G.g.[('/',g)],(Seq.tail n))) with
                |(a,b) when a="/"->yield string (Seq.head b); yield! fn (Seq.tail b)
                |(a,b)           ->yield a;                   yield! fn b
         }
         fn n
let G n i g e l z= 
  let a = new System.Collections.Generic.Dictionary<(char*char),string>()
  let b = new System.Collections.Generic.Dictionary<char,string>()
  Seq.iter2 (fun ng gn->a.[('/'   ,char ng)]<-string gn;b.[gn]<-ng) (List.except [n;i] z) g
  Seq.iter2 (fun ng gn->a.[(char n,char ng)]<-string gn;b.[gn]<-n+ng)                  z  e
  Seq.iter2 (fun ng gn->a.[(char i,char ng)]<-string gn;b.[gn]<-i+ng)                  z  l
  {n=char n;i=char i;g=a;e=b}

```

{{out}}
{{out}}

```fsharp

let N = G "2" "6" "ETAONRIS" "BCDFGHJKLM" "PQ/UVWXYZ." ["0";"1";"2";"3";"4";"5";"6";"7";"8";"9"]
N.decode "450582425181653945125016505180125423293721256216286286288653970163758524"|>Seq.iter(fun n->printf "%s" n);; //ONENIGHTITWASONTHETWENTIETHOFMARCH1888IWASRETURNING
N.encode "IN THE WINTER 1965 WE WERE HUNGRY JUST BARELY ALIVE"|>Seq.iter(fun n->printfn "%s" n);; //8562 125062 658510762 62162962662562 65062 6507062 256352476762 26639162 20370286762 3288640
N.decode "8562 125062 658510762 62162962662562 65062 6507062 256352476762 26639162 20370286762 3288640"|>Seq.iter(fun n->printf "%s" n);; //IN THE WINTER 1965 WE WERE HUNGRY JUST BARELY ALIVE

```


## Go


```go
package main

import (
    "fmt"
    "strings"
)

func main() {
    key := `
 8752390146
 ET AON RIS
5BC/FGHJKLM
0PQD.VWXYZU`
    p := "you have put on 7.5 pounds since I saw you."
    fmt.Println(p)
    c := enc(key, p)
    fmt.Println(c)
    fmt.Println(dec(key, c))
}

func enc(bd, pt string) (ct string) {
    enc := make(map[byte]string)
    row := strings.Split(bd, "\n")[1:]
    r2d := row[2][:1]
    r3d := row[3][:1]
    for col := 1; col <= 10; col++ {
        d := string(row[0][col])
        enc[row[1][col]] = d
        enc[row[2][col]] = r2d+d
        enc[row[3][col]] = r3d+d
    }
    num := enc['/']
    delete(enc, '/')
    delete(enc, ' ')
    for i := 0; i < len(pt); i++ {
        if c := pt[i]; c <= '9' && c >= '0' {
            ct += num + string(c)
        } else {
            if c <= 'z' && c >= 'a' {
                c -= 'a'-'A'
            }
            ct += enc[c]
        }
    }
    return
}

func dec(bd, ct string) (pt string) {
    row := strings.Split(bd, "\n")[1:]
    var cx [10]int
    for i := 1; i <= 10; i++ {
        cx[row[0][i]-'0'] = i
    }
    r2d := row[2][0]-'0'
    r3d := row[3][0]-'0'
    for i := 0; i < len(ct); i++ {
        var r int
        switch d := ct[i]-'0'; d {
        case r2d:
            r = 2
        case r3d:
            r = 3
        default:
            pt += string(row[1][cx[d]])
            continue
        }
        i++
        if b := row[r][cx[ct[i]-'0']]; b == '/' {
            i++
            pt += string(ct[i])
        } else {
            pt += string(b)
        }
    }
    return
}
```

{{out}}

```txt

you have put on 7.5 pounds since I saw you.
01306592038080673955702555083069056649578462090130602
YOUHAVEPUTON7.5POUNDSSINCEISAWYOU.

```


## Haskell


```haskell
import Data.Char
import Data.Map

charToInt :: Char -> Int
charToInt c = ord c - ord '0'

-- Given a string, decode a single character from the string.
-- Return the decoded char and the remaining undecoded string.
decodeChar :: String -> (Char,String)
decodeChar ('7':'9':r:rs) = (r,rs)
decodeChar ('7':r:rs)     = ("PQUVWXYZ. " !! charToInt r, rs)
decodeChar ('3':r:rs)     = ("ABCDFGIJKN" !! charToInt r, rs)
decodeChar (r:rs)         = ("HOL MES RT" !! charToInt r, rs)

-- Decode an entire string.
decode :: String -> String
decode [] = []
decode st = let (c, s) = decodeChar st in c:decode s

-- Given a string, decode a single character from the string.
-- Return the decoded char and the part of the encoded string
-- used to encode that character.
revEnc :: String -> (Char, String)
revEnc enc = let (dec, rm) = decodeChar enc in (dec, take (length enc - length rm) enc)

ds :: String
ds = ['0'..'9']

-- Decode all 1000 possible encodings of three digits and 
-- use results to construct map used to encode.
encodeMap :: Map Char String
encodeMap = fromList [ revEnc [d2,d1,d0] | d2 <- ds, d1 <- ds, d0 <- ds ]

-- Encode a single char using encoding map.
encodeChar :: Char -> String
encodeChar c = findWithDefault "" c encodeMap 

-- Encode an entire string.
encode :: String -> String
encode st = concatMap encodeChar $ fmap toUpper st

-- Test by encoding, decoding, printing results.
main = let orig = "One night-it was on the twentieth of March, 1888-I was returning"
           enc = encode orig
           dec = decode enc
       in mapM_ putStrLn [ "Original: " ++ orig
                         , "Encoded: " ++ enc 
                         , "Decoded: " ++ dec ]
```


{{out}}
<pre style="font-size:80%">Original: One night-it was on the twentieth of March, 1888-I was returning
Encoded: 139539363509369743061399059745399365901344308320791798798798367430685972839363935
Decoded: ONENIGHTITWASONTHETWENTIETHOFMARCH1888IWASRETURNING

```

=={{header|Icon}} and {{header|Unicon}}==

```Icon
procedure main()
StraddlingCheckerBoard("setup","HOLMESRTABCDFGIJKNPQUVWXYZ./", 3,7)

text := "One night. it was on the twentieth of March, 1888. I was returning"
write("text   = ",image(text))
write("encode = ",image(en := StraddlingCheckerBoard("encode",text)))
write("decode = ",image(StraddlingCheckerBoard("decode",en)))
end

procedure StraddlingCheckerBoard(act,text,b1,b2)
static SCE,SCD
case act of {
   "setup" : {  
      if (b1 < b2 < 10) & (*text = *cset(text) = 28) then {
         SCE := table("")   
         SCD := table()         
         esc := text[-1]                               # escape
         every text[(b1|b2)+1+:0] := " "               # blanks
         uix := ["",b1,b2]                             # 1st position
         every c := text[1 + (i := 0 to *text-1)] do   # build translation
            if c ~== " " then                          # skip blanks
               SCD[SCE[c] := SCE[map(c)] := uix[i/10+1]||(i%10) ] := c         
         every c := !&digits do 
            SCD[SCE[c] := SCE[esc] || c] := c
         delete(SCD,SCE[esc])
         delete(SCE,esc)          
         }
         else stop("Improper setup: ",image(text),", ",b1,", ",b2)
      }
   "encode" : {
      every (s := "") ||:= x := SCE[c := !text]
      return s
      }
   "decode" : {  
      s := ""
      text ? until pos(0) do 
         s ||:= \SCD[k := move(1 to 3)]
      return s       
      }
   }
end
```


Output:
```txt
text   = "One night. it was on the twentieth of March, 1888. I was returning"
encode = "1395393635097836974306139905974539936590134430832079179879879878367430685972839363935"
decode = "ONENIGHT.ITWASONTHETWENTIETHOFMARCH1888.IWASRETURNING"
```



## J

'''Solution:'''

```j
'Esc Stop'=: '/.'
'Nums Alpha'=: '0123456789';'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
Charset=: Nums,Alpha,Stop

escapenum=: (,@:((; Esc&,)&>) Nums) rplc~ ]               NB. escape numbers
unescapenum=: ((, ; ' '&,@])"1 0  Nums"_) rplc~ ]         NB. unescape coded numbers (x is escape code, y is cipher)
expandKeyatUV=: 0:`[`(1 #~ 2 + #@])} #inv ]
makeChkBrd=: Nums , expandKeyatUV

chkbrd=: conjunction define
  'uv key'=. n
  board=. uv makeChkBrd key
  select. m
  case. 0 do.                                                      NB. encode
    digits=. board 10&#.inv@i. escapenum y
    ' ' -.~ ,(":@{:"1 digits) ,.~ (1 1 0 2{":uv) {~ {."1 digits
  case. 1 do.                                                      NB. decode
    esc=. 0 chkbrd (uv;key) Esc                                    NB. find code for Esc char
    tmp=. esc unescapenum esc,'0',y                                
    tmp=. ((":uv) ((-.@e.~ _1&|.) *. e.~) tmp) <;.1 tmp            NB. box on chars from rows 0 2 3
    idx=. (}. ,~ (1 1 0 2{":uv) ":@i. {.) each tmp                 NB. recreate indices for rows 0 2 3
    idx=. ;(2&{. , [: ((0 1 $~ +:@#) #inv!.'1' ]) 2&}.) each idx   NB. recreate indices for row 1
    }.board {~ _2 (_&".)\ idx    
  end.
)
```

'''Example usage:'''

```j
   preprocess=: (#~ Charset e.~ ])@toupper                 NB. verb to compress out non-alphanumerics
   chkbrdRC=: chkbrd (3 7;'HOLMESRTABCDFGIJKNPQUVWXYZ./')  NB. define adverb by applying Rosetta Code key to chkbrd conjunction
   0 chkbrdRC preprocess 'One night-it was on the twentieth of March, 1888-I was returning'
139539363509369743061399059745399365901344308320791798798798367430685972839363935
   1 chkbrdRC 0 chkbrdRC preprocess 'One night-it was on the twentieth of March, 1888-I was returning'
ONENIGHTITWASONTHETWENTIETHOFMARCH1888IWASRETURNING
```

Or using the rules proposed by [[User:Util|Util]] on the discussion page:

```j
   preprocess=: Stop&([`([: I. Charset -.@e.~ ])`]} toupper) NB. replace all non-alphanumerics with Stop
   1 chkbrdRC 0 chkbrdRC preprocess 'One night-it was on the twentieth of March, 1888-I was returning'
ONE.NIGHT.IT.WAS.ON.THE.TWENTIETH.OF.MARCH..1888.I.WAS.RETURNING
```



Another method is to create a set of keys which correspond to the encoded letters in the alphabet. This allows for the relatively simple design of the encoding and decoding functions as they are essentially hash table lookups.

'''Solution:'''

```j
NB. stops setcode alphabet
NB. creates verbs encode and decode which change between unencoded text and lists of encoded numbers
   setcode=: 3 :0
2 6 setcode y
:
alphabet=: y, ,":"0 i.10
stops=. x
alphkeys=. (a: , ,&.> x) ([ -.~ [: , ,&.>/) i.10
esckey=. >(alphabet i. '/'){alphkeys
numkeys=. esckey&,&.> i.10
keys=. alphkeys,numkeys

encode=: ([: ; keys {~ alphabet&i.) :. decode
break=. </.~ i.@# - _1|. ([: >/\.&.|. e.&stops) + _1|.2*esckey&E.
decode=: (alphabet {~ keys i. break f.) :. encode
i.0 0
)
```


'''Example usage:'''

```j
   3 7 setcode 'HOLMESRTABCDFGIJKNPQUVWXYZ./'
   preprocess=: (#~ alphabet e.~ ])@toupper
   ,":"0 encode message=: preprocess 'One night-it was on the twentieth of March, 1888-I was returning'
139539363509369743061399059745399365901344308320791798798798367430685972839363935
   decode encode message
ONENIGHTITWASONTHETWENTIETHOFMARCH1888IWASRETURNING
   ]s=. ((10|+) 0 4 5 2$~$)&.encode message NB. scramble by taking a modular sum with 0 4 5 2 while encoded
OWVKRNEOAMTMXROWOHTMTMTROTQ4SEMRRLRZLVSTTLLOROMHALSFOHECMRWESWEE
   ((10|-) 0 4 5 2$~$)&.encode s
ONENIGHTITWASONTHETWENTIETHOFMARCH1888IWASRETURNING
```



## Java

{{works with|Java|7}}

```java
import java.util.HashMap;
import java.util.Map;
import java.util.regex.*;

public class StraddlingCheckerboard {

    final static String[] keyvals = {"H:0", "O:1", "L:2", "M:4", "E:5", "S:6",
        "R:8", "T:9", "A:30", "B:31", "C:32", "D:33", "F:34", "G:35", "I:36",
        "J:37", "K:38", "N:39", "P:70", "Q:71", "U:72", "V:73", "W:74", "X:75",
        "Y:76", "Z:77", ".:78", "/:79", "0:790", "1:791", "2:792", "3:793",
        "4:794", "5:795", "6:796", "7:797", "8:798", "9:799"};

    final static Map<String, String> val2key = new HashMap<>();
    final static Map<String, String> key2val = new HashMap<>();

    public static void main(String[] args) {
        for (String keyval : keyvals) {
            String[] kv = keyval.split(":");
            val2key.put(kv[0], kv[1]);
            key2val.put(kv[1], kv[0]);
        }
        String enc = encode("One night-it was on the twentieth of March, "
                + "1888-I was returning");
        System.out.println(enc);
        System.out.println(decode(enc));
    }

    static String encode(String s) {
        StringBuilder sb = new StringBuilder();
        for (String c : s.toUpperCase().split("")) {
            c = val2key.get(c);
            if (c != null)
                sb.append(c);
        }
        return sb.toString();
    }

    static String decode(String s) {
        Matcher m = Pattern.compile("(79.|3.|7.|.)").matcher(s);
        StringBuilder sb = new StringBuilder();
        while (m.find()) {
            String v = key2val.get(m.group(1));
            if (v != null)
                sb.append(v);
        }
        return sb.toString();
    }
}
```



```txt
139539363509369743061399059745399365901344308320791798798798367430685972839363935
ONENIGHTITWASONTHETWENTIETHOFMARCH1888IWASRETURNING
```



## JavaScript


```javascript><script

var alphabet=new Array("ESTONIA  R","BCDFGHJKLM","PQUVWXYZ./") // scramble alphabet as you wish
var prefixes=new Array("",alphabet[0].indexOf(" "),alphabet[0].lastIndexOf(" ")) 

function straddle(message){
  var out=""
  message=message.toUpperCase()
  message=message.replace(/([0-9])/g,"/$1") // dumb way to escape numbers
  for(var i=0;i<message.length;i++){
    var chr=message[i]
	if(chr==" ")continue
	for(var j=0;j<3;j++){
	  var k=alphabet[j].indexOf(chr)
	  if(k<0)continue
	  out+=prefixes[j].toString()+k
	}
	if(chr=="/")out+=message[++i]
  }
  return out
}

function unstraddle(message){
  var out=""
  var n,o
  for(var i=0;i<message.length;i++){
	n=message[i]*1
    switch(n){
	  case prefixes[1]: o=alphabet[1][message[++i]];break
	  case prefixes[2]: o=alphabet[2][message[++i]];break
	  default: o=alphabet[0][n]
	}
	o=="/"?out+=message[++i]:out+=o
  }
  return out
}

str="One night-it was on the twentieth of March, 1888-I was returning."
document.writeln(str)
document.writeln(straddle(str))
document.writeln(unstraddle(straddle(str)))
</script>
```


Output:
```txt
One night-it was on the twentieth of March, 1888-I was returning.
34045747525284613427502840425027537379697175891898898898584619028294547488
ONENIGHTITWASONTHETWENTIETHOFMARCH1888IWASRETURNING.
```



## Julia

Unlike the precomputed table versions, this version takes a 30-character string specifying the 3 rows of the checkerboard 
as an argument, recomputing the lookup table each run, which allows easier changes of keys without modifying the code.

```julia

function straddlingcheckerboard(board, msg, doencode)
    lookup = Dict()
    reverselookup = Dict()
    row2 = row3 = slash = -1  
    function encode(x)
        s = ""
        for ch in replace(replace(uppercase(x), r"([01-9])", s";=;\1"), r";=;", slash)
            c = string(ch)
            if haskey(lookup, c)
                s *= lookup[c]
            elseif contains("0123456789", c)
                s *= c
            end
        end
        s
    end
    function decode(x)
        s = ""
        i = 1
        while i <= length(x)
            c = string(x[i])
            if haskey(reverselookup, c)
                s *= reverselookup[c]
                i += 1
            else
                if "$c$(x[i+1])" == slash
                    s *= string(x[i+2])
                    i += 3
                else
                    s *= reverselookup["$c$(x[i+1])"]
                    i += 2
                end
            end
        end
        s
    end
    for (i,c) in enumerate(board)
        c = string(c)
        if c == " "
            if row2 == -1 
                row2 = i-1
            else
                row3 = i-1
            end
        else
            if i < 11
                lookup[c] = "$(i-1)"; reverselookup["$(i-1)"] =  c
            elseif i < 21
                lookup[c] = "$row2$(i-11)"; reverselookup["$row2$(i-11)"] = c
            else
                lookup[c] = "$row3$(i-21)"; reverselookup["$row3$(i-21)"] = c
            end
            if c == "/"
                slash = lookup[c]
            end
        end     
    end
    doencode ? encode(msg) : decode(msg)
end

btable = "ET AON RISBCDFGHJKLMPQ/UVWXYZ."
message = "Thecheckerboardcakerecipespecifies3largeeggsand2.25cupsofflour."
encoded = straddlingcheckerboard(btable, message, true)
decoded = straddlingcheckerboard(btable, encoded, false)
println("Original: $message\nEncoded:  $encoded\nDecoded:  $decoded")

```

{{output}}

```txt

Original: Thecheckerboardcakerecipespecifies3largeeggsand2.25cupsofflour.
Encoded:  125021250212707204372221327070218600960021823809623283724002424935226226962262521636094232328463769
Decoded:  THECHECKERBOARDCAKERECIPESPECIFIES3LARGEEGGSAND2.25CUPSOFFLOUR.

```



## Kotlin


```scala
// version 1.2.0

val board = "ET AON RISBCDFGHJKLMPQ/UVWXYZ."
val digits = "0123456789"
val rows = " 26"
val escape = "62"
val key = "0452"

fun encrypt(message: String): String {
    val msg = message.toUpperCase()
                     .filter { (it in board || it in digits) && it !in " /" }
    val sb = StringBuilder()
    for (c in msg) {
        val idx = board.indexOf(c)
        if (idx > -1) {
            val row = idx / 10
            val col = idx % 10
            sb.append(if (row == 0) "$col" else "${rows[row]}$col")
        }
        else {
            sb.append("$escape$c")
        }
    }
    val enc = sb.toString().toCharArray()
    for ((i, c) in enc.withIndex()) {
        val k = key[i % 4] - '0'
        if (k == 0) continue
        val j = c - '0'
        enc[i] = '0' + ((j + k) % 10)
    }
    return String(enc)
}

fun decrypt(encoded: String): String {
    val enc = encoded.toCharArray()
    for ((i, c) in enc.withIndex()) {
        val k = key[i % 4] - '0'
        if (k == 0) continue
        val j = c - '0'
        enc[i] = '0' + if (j >= k) (j - k) % 10 else (10 + j - k) % 10
    }
    val len = enc.size
    val sb = StringBuilder()
    var i = 0
    while (i < len) {
        val c = enc[i]
        val idx = rows.indexOf(c)
        if (idx == -1) {
            val idx2 = c - '0'
            sb.append(board[idx2])
            i++
        }
        else if ("$c${enc[i + 1]}" == escape) {
            sb.append(enc[i + 2])
            i += 3
        }
        else {
            val idx2 = idx * 10 + (enc[i + 1] - '0')
            sb.append(board[idx2])
            i += 2
        }
    }
    return sb.toString()
}

fun main(args: Array<String>) {
    val messages = listOf(
        "Attack at dawn",
        "One night-it was on the twentieth of March, 1888-I was returning",
        "In the winter 1965/we were hungry/just barely alive",
        "you have put on 7.5 pounds since I saw you.",
        "The checkerboard cake recipe specifies 3 large eggs and 2.25 cups of flour."
    )
    for (message in messages) {
        val encrypted = encrypt(message)
        val decrypted = decrypt(encrypted)
        println("\nMessage   : $message")
        println("Encrypted : $encrypted")
        println("Decrypted : $decrypted")
    }
}
```


{{out}}

```txt

Message   : Attack at dawn
Encrypted : 3565257935743007
Decrypted : ATTACKATDAWN

Message   : One night-it was on the twentieth of March, 1888-I was returning
Encrypted : 495786945533698149645468540384645875238925776668221480708005915367278976
Decrypted : ONENIGHTITWASONTHETWENTIETHOFMARCH1888IWASRETURNING

Message   : In the winter 1965/we were hungry/just barely alive
Encrypted : 89645417896270736648201450026959060839767024608116557470618482160
Decrypted : INTHEWINTER1965WEWEREHUNGRYJUSTBARELYALIVE

Message   : you have put on 7.5 pounds since I saw you.
Encrypted : 61983605685800834914704829124087264189730245691940889
Decrypted : YOUHAVEPUTON7.5POUNDSSINCEISAWYOU.

Message   : The checkerboard cake recipe specifies 3 large eggs and 2.25 cups of flour.
Encrypted : 160225770664742408892673362274738052905225343251668487244454469139746678907466041088039436848815704
Decrypted : THECHECKERBOARDCAKERECIPESPECIFIES3LARGEEGGSAND2.25CUPSOFFLOUR.

```



## Lua


```lua

local brd = { "HOL MES RT", "ABCDFGIJKN", "PQUVWXYZ./" }
local dicE, dicD, s1, s2 = {}, {}, 0, 0

function dec( txt )
    local i, numb, s, t, c = 1, false
    while( i < #txt ) do
        c = txt:sub( i, i )
        if not numb then
            if tonumber( c ) == s1 then
                i = i + 1; s = string.format( "%d%s", s1, txt:sub( i, i ) ) 
                t = dicD[s]
            elseif tonumber( c ) == s2 then
                i = i + 1; s = string.format( "%d%s", s2, txt:sub( i, i ) ) 
                t = dicD[s]
            else
                t = dicD[c]
            end
            if t == "/" then
                numb = true
            else 
                io.write( t )
            end
        else
            io.write( c )
            numb = false
        end
        i = i + 1
    end
    print()
end
function enc( txt )
    local c
    for i = 1, #txt do
        c = txt:sub( i, i )
        if c >= "A" and c <= "Z" then
            io.write( dicE[c] )
        elseif c >= "0" and c <= "9" then
            io.write( string.format( "%s%s", dicE["/"], c ) )
        end
    end
    print()
end
function createDictionaries()
    for i = 1, 10 do
        c = brd[1]:sub( i, i )
        if c == " " then
            if s1 == 0 then s1 = i - 1
            elseif s2 == 0 then s2 = i - 1 end
        else
            dicE[c] = string.format( "%d", i - 1 )
            dicD[string.format( "%d", i - 1 )] = c
        end
    end
    for i = 1, 10 do
        dicE[brd[2]:sub( i, i )] = string.format( "%d%d", s1, i - 1 )
        dicE[brd[3]:sub( i, i )] = string.format( "%d%d", s2, i - 1 )
        dicD[string.format( "%d%d", s1, i - 1 )] = brd[2]:sub( i, i )
        dicD[string.format( "%d%d", s2, i - 1 )] = brd[3]:sub( i, i )
    end
end
function enterText()
    createDictionaries()
    io.write( "\nEncrypt or Decrypt (E/D)? " )
    d = io.read()
    io.write( "\nEnter the text:\n" )
    txt = io.read():upper()
    if d == "E" or d == "e" then enc( txt ) 
    elseif d == "D" or d == "d" then dec( txt )
    end 
end
-- entry point 
enterText()

```

{{out}}

```txt

Lua 5.3.3  Copyright (C) 1994-2016 Lua.org, PUC-Rio
> dofile "straddling.lua"

Encrypt or Decrypt (E/D)? e

Enter the text:
One night-it was on the twentieth of March, 1888-I was returning
139539363509369743061399059745399365901344308320791798798798367430685972839363935
> dofile "straddling.lua"

Encrypt or Decrypt (E/D)? d

Enter the text:
139539363509369743061399059745399365901344308320791798798798367430685972839363935
ONENIGHTITWASONTHETWENTIETHOFMARCH1888IWASRETURNING

```


## M2000 Interpreter


```M2000 Interpreter

module Straddling_checkerboard {
	function encrypt$(message$, alphabet) {
		message$=filter$(ucase$(message$),"-/,. ")
		def num1, num2
		num1=instr(alphabet#val$(0)," ")
		num2=instr(alphabet#val$(0)," ", num1+1)-1
		num1--
		escapenum=instr(alphabet#val$(2),"/")-1
		escapenum1=instr(alphabet#val$(2),".")-1
		num0=-10
		num10=num1*10-20
		num20=num2*10-30
		numbers=(escapenum+num2*10)*10
		numbers1=(escapenum1+num2*10)*10
		boardrow=each(alphabet)
		cipherkey$="0123456789" :	while boardrow :cipherkey$+=array$(boardrow) :end while
		encoded$=""
		for i=1 to len(message$)
			n=instr(cipherkey$, mid$(message$,i,1))-1
			if n<10 then
				n+=if(random(10)<5->numbers, numbers1)
			else.if n<20 then
				n+=num0
			else.if n<30 then
				n+=num10
			else
				n+=num20
			end if
			encoded$+=str$(n,"")
		next
		=encoded$
	}
	function decrypt$(encoded$, alphabet) {
		def num1, num2
		num1=instr(alphabet#val$(0)," ")
		num2=instr(alphabet#val$(0)," ", num1+1)-1
		num1--
		escapenum=instr(alphabet#val$(2),"/")-1
		escapenum1=instr(alphabet#val$(2),".")-1
		def i=1, decoded$
		j=len(encoded$)+1
		while i<j
		m=val(mid$(encoded$,i,1))
		if m=num1 then
			decoded$+=mid$(alphabet#val$(1), val(mid$(encoded$,i+1,1))+1,1)
			i++
		else.if m=num2 then
			if i+1<j then
				mm=val(mid$(encoded$,i+1,1))
				if mm=escapenum or mm=escapenum1 then
					decoded$+=mid$(encoded$,i+2,1)
					i+=2
				else
					decoded$+=mid$(alphabet#val$(2), val(mid$(encoded$,i+1,1))+1,1)
					i++
				end if
			else
				decoded$+=mid$(alphabet#val$(2), val(mid$(encoded$,i+1,1))+1,1)
				i++	
			end if
		else
			decoded$+=mid$(alphabet#val$(0), m+1,1)
		end if
		i++
		end while
		=decoded$
	}
	Module DisplayBoard (alphabet, &Doc$){
		num1=instr(alphabet#val$(0)," ")
		num2=instr(alphabet#val$(0)," ", num1+1)-1
		num1--
		escapenum=instr(alphabet#val$(2),"/")-1
		escapenum1=instr(alphabet#val$(2),".")-1
		\\ display straddling checkerboard 
		checkerboard =cons(("0123456789",),alphabet)
		labels=(" "," ",str$(num1,""), str$(num2,""))
		disp=each(checkerboard)
		while disp
			Doc$=labels#val$(disp^)+" "+array$(disp)+{
			}
		end while
	}
	Document Doc$
	Const nl$={
	}
	Function OnePad$(Message$, PadSerial, n=1) {
		x=random(!PadSerial)  ' push old seed, set the new one
		encoded$=""
		for i=1 to len(Message$)
		encoded$+=str$((val(mid$(Message$,i,1))+10+n*random(1, 10))mod 10,"")
		next i
		x=random(!) ' restore old seed
		=encoded$
	}
	\\ select a random alphabet
	select case random(1,3)
	case 1
	alphabet=("ESTONIAR  ","BCDFGHJKLM","/PQUVWXYZ.") : Doc$="First "+nl$
	case 2
	alphabet=("ET AON RIS","BCDFGHJKLM","PQ/UVWXYZ.") : Doc$="Second"+nl$
	case 3
	alphabet=("ESTONIAR  ","BCDFGHJKLM","PQU.VWXYZ/") : Doc$="Third"+nl$
	end select
	DisplayBoard alphabet, &Doc$
	msg$="One night-it was on the twentieth of March, 1888-I was returning"
	Doc$= "original message:"+nl$
	Doc$= msg$+nl$
	Doc$= "encrypted message:"+nl$
	crypt$=encrypt$(msg$, alphabet)
	Doc$=crypt$+nl$
	serial=random(1234567,9345678)
	Doc$= "encrypted message using one pad using serial:"+str$(serial)+nl$
	crypt$=OnePad$(crypt$, serial)
	Doc$=crypt$+nl$
	Doc$= "decrypted message using one pad:"+nl$
	crypt$=OnePad$(crypt$, serial,-1)
	Doc$=crypt$+nl$
	Doc$= "decrypted message:"+nl$
	Doc$=decrypt$(crypt$, alphabet)
	Print #-2, Doc$  ' render to console using new lines codes from Doc$
	Clipboard Doc$
}
Straddling_checkerboard

```


{{out}}
<pre style="height:30ex;overflow:scroll">
First 
  0123456789
  ESTONIAR  
8 BCDFGHJKLM
9 /PQUVWXYZ.
original message:
One night-it was on the twentieth of March, 1888-I was returning
encrypted message:
340458485252956134285029504250285383896781859019989989985956170293745484
encrypted message using one pad using serial: 7745037
811354381526409435575275634030693156335135381796263056259799033307312819
decrypted message using one pad:
340458485252956134285029504250285383896781859019989989985956170293745484
decrypted message:
ONENIGHTITWASONTHETWENTIETHOFMARCH1888IWASRETURNING
Second
  0123456789
  ET AON RIS
2 BCDFGHJKLM
6 PQ/UVWXYZ.
original message:
One night-it was on the twentieth of March, 1888-I was returning
encrypted message:
450582425181653945125016505180125423293721256216986986288653970163758524
encrypted message using one pad using serial: 8485713
339360039726873741044731817004082441136155821888897367354417518371636266
decrypted message using one pad:
450582425181653945125016505180125423293721256216986986288653970163758524
decrypted message:
ONENIGHTITWASONTHETWENTIETHOFMARCH1888IWASRETURNING
</pre >



## Perl

{{trans|Perl 6}}

```perl
use strict;
use warnings;
use feature 'say';
use List::Util <min max>;

my(%encode,%decode,@table);

sub build {
    my($u,$v,$alphabet) = @_;
    my(@flat_board,%p2c,%c2p);
    my $numeric_escape = '/';

    @flat_board = split '', uc $alphabet;
    splice @flat_board, min($u,$v), 0, undef;
    splice @flat_board, max($u,$v), 0, undef;

    push @table, [' ', 0..9];
    push @table, [' ', map { defined $_ ? $_ : ' '} @flat_board[ 0 ..  9] ];
    push @table, [$u,  @flat_board[10 .. 19]];
    push @table, [$v,  @flat_board[20 .. 29]];

    my @nums = my @order = 0..9;
    push @nums, (map { +"$u$_" } @order), map { +"$v$_" } @order;

    @c2p{@nums} = @flat_board;
    for (keys %c2p) { delete $c2p{$_} unless defined $c2p{$_} }
    @p2c{values %c2p} = keys %c2p;
    $p2c{$_} = $p2c{$numeric_escape} . $_ for 0..9;
    while (my ($k, $v) = each %p2c) {
        $encode{$k} = $v;
        $decode{$v} = $k unless $k eq $numeric_escape;
    }
}

sub decode {
    my($string) = @_;
    my $keys = join '|', keys %decode;
    $string =~ s/($keys)/$decode{$1}/gr;
}

sub encode {
    my($string) = uc shift;
    $string =~ s#(.)#$encode{$1} // $encode{'.'}#ger;
}

my $sc = build(3, 7, 'HOLMESRTABCDFGIJKNPQUVWXYZ./');
say join  ' ', @$_ for @table;
say '';
say 'Original: ', my $original = 'One night-it was on the twentieth of March, 1888-I was returning';
say 'Encoded:  ', my $en = encode($original);
say 'Decoded:  ', decode($en);
```

{{out}}

```txt
  0 1 2 3 4 5 6 7 8 9
  H O L   M E S   R T
3 A B C D F G I J K N
7 P Q U V W X Y Z . /

Original: One night-it was on the twentieth of March, 1888-I was returning
Encoded:  13957839363509783697874306781397890578974539936590781347843083207878791798798798783678743067885972839363935
Decoded:  ONE.NIGHT.IT.WAS.ON.THE.TWENTIETH.OF.MARCH..1888.I.WAS.RETURNING
```



## Perl 6

{{works with|Rakudo|2018.03}}
The .trans method in Perl 6 improves on Perl 5's tr/// by allowing multi-character translation tables.

We build the full table during .new, which simplifies .encode and .decode.

```perl6
class Straddling_Checkerboard {
    has @!flat_board; # 10x3 stored as 30x1
    has $!plain2code; # full translation table, invertable
    has @!table;      # Printable layout, like Wikipedia entry
 
    my $numeric_escape = '/';
    my $exclude = /<-[A..Z0..9.]>/; # Omit the escape character

    method display_table { gather { take ~ .list for @!table } };

    method decode ( Str $s --> Str ) {
        $s.trans($!plain2code.antipairs);
    }

    method encode ( Str $s, :$collapse? --> Str ) {
        my $replace = $collapse ?? '' !! '.';
        $s.uc.subst( $exclude, $replace, :g ).trans($!plain2code);
    }

    submethod BUILD ( :$alphabet, :$u where 0..9, :$v where 0..9 ) {
        die if $u == $v;
        die if $alphabet.comb.sort.join ne [~] flat './', 'A'..'Z';

        @!flat_board = $alphabet.uc.comb;
        @!flat_board.splice( $u min $v, 0, Any );
        @!flat_board.splice( $u max $v, 0, Any );

        @!table = [ ' ',|            [ 0 ..  9]                              ],
                  [ ' ',|@!flat_board[ 0 ..  9].map: {.defined ?? $_ !! ' '} ],
                  [ $u, |@!flat_board[10 .. 19]                              ],
                  [ $v, |@!flat_board[20 .. 29]                              ];

        my @order = 0..9; # This may be passed as a param in the future

        my @nums = flat @order,
                   @order.map({ +"$u$_" }),
                   @order.map({ +"$v$_" });

        my %c2p = @nums Z=> @!flat_board;
        %c2p{$_}:delete if %c2p{$_} eqv Any for keys %c2p;
        my %p2c = %c2p.invert;
        %p2c{$_} = %p2c{$numeric_escape} ~ $_ for 0..9;
        $!plain2code = [%p2c.keys] => [%p2c.values];
    }
}

sub MAIN ( :$u = 3, :$v = 7, :$alphabet = 'HOLMESRTABCDFGIJKNPQUVWXYZ./' ) {
    my Straddling_Checkerboard $sc .= new: :$u, :$v, :$alphabet;
    $sc.display_table;

    for 0..1 -> $collapse {
        my $original = 'One night-it was on the twentieth of March, 1888-I was returning';
        my $en = $sc.encode($original, :$collapse);
        my $de = $sc.decode($en);
        say '';
        say "Original: $original";
        say "Encoded:  $en";
        say "Decoded:  $de";
    }
}
```


Output:
```txt
  0 1 2 3 4 5 6 7 8 9
  H O L   M E S   R T
3 A B C D F G I J K N
7 P Q U V W X Y Z . /

Original: One night-it was on the twentieth of March, 1888-I was returning
Encoded:  13957839363509783697874306781397890578974539936590781347843083207878791798798798783678743067885972839363935
Decoded:  ONE.NIGHT.IT.WAS.ON.THE.TWENTIETH.OF.MARCH..1888.I.WAS.RETURNING

Original: One night-it was on the twentieth of March, 1888-I was returning
Encoded:  139539363509369743061399059745399365901344308320791798798798367430685972839363935
Decoded:  ONENIGHTITWASONTHETWENTIETHOFMARCH1888IWASRETURNING
```



## Phix

{{trans|C}}

```Phix
function read_table(string cb)
    sequence encode = repeat("",128),
             decode = repeat(0,128),
             row = {0}
    if length(cb)!=30 then crash("table wrong length") end if
    for i=1 to 30 do
        integer c = cb[i]
        if c=' ' then
            row &= i-1
        else
            integer code = row[floor((i-1)/10)+1]*10+mod((i-1),10)
            encode[c] = sprintf("%d",code)
            decode[code+1] = c
        end if
    end for
    return {encode, decode}
end function
 
function encipher(sequence encode, string in, bool strip=false)
    string out = ""
    for i=1 to length(in) do
        integer c = upper(in[i]), code
        if c>='0' and c<='9' then
            out &= encode['.']&c
        elsif c>='A' and c<='Z' then
            out &= encode[c]
        elsif not strip or c='/' then   -- (see note)
            out &= encode['/']
        end if
    end for
    return out
end function 

function decipher(sequence decode, string in, bool strip=false)
    string out = ""
    integer i = 1
    while i<=length(in) do
        integer c = in[i]-'0'+1
        if decode[c]=0 then
            i += 1
            c = (c-1)*10+in[i]-'0'+1
        end if
        integer d = decode[c]
        if d='.' then
            i += 1
            d = in[i]
        elsif d='/' and not strip then  -- (see note)
            d = ' '
        end if
        out &= d
        i += 1
    end while
    return out
end function
 
constant cb =  "ET AON RIS"&
               "BCDFGHJKLM"&
               "PQ/UVWXYZ."
sequence {encode,decode} = read_table(cb)

--  Note there is a subtle difference in space handling, to exactly match other outputs try
--  {encode,decode} = read_table("HOL MES RTABCDFGIJKNPQUVWXYZ/.") instead of
--  {encode,decode} = read_table("HOL MES RTABCDFGIJKNPQUVWXYZ./"), and
--  {encode,decode} = read_table("ET AON RISBCDFGHJKLMPQ.UVWXYZ/") instead of
--  {encode,decode} = read_table("ET AON RISBCDFGHJKLMPQ/UVWXYZ.")

string msg = "In the winter 1965/we were hungry/just barely alive"
printf(1,"message: %s\n", {msg})
string enc = encipher(encode, msg),
       dec = decipher(decode, enc)
printf(1,"encoded: %s\n", {enc})
printf(1,"decoded: %s\n", {dec})
 
printf(1,"\nNo spaces:\n")
enc = encipher(encode, msg, true)
dec = decipher(decode, enc, true)
printf(1,"encoded: %s\n", {enc})
printf(1,"decoded: %s\n", {dec})
```

{{out}}

```txt

message: In the winter 1965/we were hungry/just barely alive
encoded: 85621250626585107626916996966956265062650706225635247676226639162203702867623288640
decoded: IN THE WINTER 1965 WE WERE HUNGRY JUST BARELY ALIVE

No spaces:
encoded: 851250658510769169969669562650650702563524767622663912037028673288640
decoded: INTHEWINTER1965/WEWEREHUNGRY/JUSTBARELYALIVE

```



## PHP

{{trans|Java}}
{{works with|PHP 7}}

```php
$key2val = ["A"=>"30", "B"=>"31", "C"=>"32", "D"=>"33", "E"=>"5", "F"=>"34", "G"=>"35",
        "H"=>"0", "I"=>"36", "J"=>"37", "K"=>"38", "L"=>"2", "M"=>"4", "."=>"78", "N"=>"39",
        "/"=>"79", "O"=>"1", "0"=>"790", "P"=>"70", "1"=>"791", "Q"=>"71", "2"=>"792",
        "R"=>"8", "3"=>"793", "S"=>"6", "4"=>"794", "T"=>"9", "5"=>"795", "U"=>"72",
        "6"=>"796", "V"=>"73", "7"=>"797", "W"=>"74", "8"=>"798", "X"=>"75", "9"=>"799",
        "Y"=>"76", "Z"=>"77"];
        
$val2key = array_flip($key2val);

function encode(string $s) : string  {
    global $key2val;
     
    $callback = function($c) use ($key2val) { return $key2val[$c] ?? ''; };

    return implode(array_map($callback, str_split(strtoupper($s))));
}
    
function decode(string $s) : string {
     global $val2key;

     $callback = function($c) use ($val2key) { return $val2key[$c] ?? ''; };
     
     preg_match_all('/79.|7.|3.|./', $s, $m);
     
     return implode(array_map($callback, $m[0]));
}   

function main($s) {
    $encoded = encode($s);
    echo $encoded;
    echo "\n";
    echo decode($encoded);    
}

main('One night-it was on the twentieth of March, 1888-I was returning');
```


```txt

139539363509369743061399059745399365901344308320791798798798367430685972839363935
ONENIGHTITWASONTHETWENTIETHOFMARCH1888IWASRETURNING

```



## PicoLisp


```PicoLisp
(de *Straddling
   (NIL  "H"  "O"  "L"  NIL  "M"  "E"  "S"  NIL  "R"  "T")
   ("3"  "A"  "B"  "C"  "D"  "F"  "G"  "I"  "J"  "K"  "N")
   ("7"  "P"  "Q"  "U"  "V"  "W"  "X"  "Y"  "Z"  "."  "/")
   ("79" "0"  "1"  "2"  "3"  "4"  "5"  "6"  "7"  "8"  "9") )

(de straddle (Str)
   (pack
      (mapcar
         '((C)
            (pick
               '((L)
                  (and
                     (index C (cdr L))
                     (cons (car L) (dec @)) ) )
               *Straddling ) )
         (chop (uppc Str)) ) ) )

(de unStraddle (Str)
   (pack
      (make
         (for (L (chop Str)  L)
            (let C (pop 'L)
               (setq C
                  (if (assoc C *Straddling)
                     (get (cdr @) (inc (format (pop 'L))))
                     (get (cdar *Straddling) (inc (format C))) ) )
               (link (if (= "/" C) (pop 'L) C)) ) ) ) ) )
```

Output:

```txt
: (straddle "One night-it was on the twentieth of March, 1888-I was returning")
-> "139539363509369743061399059745399365901344308320791798798798367430685972839363935"

: (unStraddle @)
-> "ONENIGHTITWASONTHETWENTIETHOFMARCH1888IWASRETURNING"
```



## PureBasic


```purebasic
Procedure.s encrypt_SC(originalText.s, alphabet.s, blank_1, blank_2)
  Static notEscape.s = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ."
  Protected preDigit_1.s = Str(blank_1), preDigit_2.s = Str(blank_2)
  Protected i, index, curChar.s, escapeChar.s, outputText.s
  Protected NewMap cipher.s()
  
  ;build cipher reference
  alphabet = UCase(alphabet)
  For i = 1 To 28
    curChar = Mid(alphabet, i, 1)
    If Not FindString(notEscape, curChar)
      escapeChar = curChar
    EndIf 
    
    Select i
      Case 1 To 8
        If index = blank_1 Or index = blank_2: index + 1: EndIf ;adjust index for blank
        
        cipher(curChar) = Str(index)
        index + 1
      Case 9 To 18
        cipher(curChar) = preDigit_1 + Str(i - 9)
      Case 19 To 28
        cipher(curChar) = preDigit_2 + Str(i - 19)
    EndSelect
  Next
  
  For i = 0 To 9: cipher(Str(i)) = cipher(escapeChar) + Str(i): Next 
  
  ;encrypt each character
  originalText = UCase(originalText)
  Protected length = Len(originalText)
  For i = 1 To length
    outputText + cipher(Mid(originalText, i, 1))
  Next 
  
  ProcedureReturn outputText
EndProcedure

Procedure.s decrypt_SC(cipherText.s, alphabet.s, blank_1, blank_2)
  Static notEscape.s = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ."
  Protected preDigit_1.s = Str(blank_1), preDigit_2.s = Str(blank_2)
  Protected i, index, curChar.s, escapeCipher.s, outputText.s
  Protected NewMap cipher.s()
  
  ;build decipher reference
  alphabet = UCase(alphabet)
  For i = 1 To 28
    curChar = Mid(alphabet, i, 1)
    
    Select i
      Case 1 To 8
        If index = blank_1 Or index = blank_2: index + 1: EndIf ;adjust index for blank

        cipher(Str(index)) = curChar
        index + 1
      Case 9 To 18
        cipher(preDigit_1 + Str(i - 9)) = curChar
      Case 19 To 28
        cipher(preDigit_2 + Str(i - 19)) = curChar
    EndSelect
    If Not FindString(notEscape, curChar)
      escapeCipher = MapKey(cipher())
    EndIf
  Next
  
  For i = 0 To 9: cipher(escapeCipher + Str(i)) = Str(i): Next 
  
  ;decrypt each character
  Protected length = Len(cipherText)
  index = 1
  While index <=length
    curChar = Mid(cipherText, index, 1)
    If curChar = preDigit_1 Or curChar = preDigit_2
      curChar = Mid(cipherText, index, 2)
      If curChar = escapeCipher: curChar = Mid(cipherText, index, 3): EndIf
    EndIf 
    outputText + cipher(curChar)
    index + Len(curChar)
  Wend
  
  ProcedureReturn outputText
EndProcedure

If OpenConsole()
  Define message.s = "One night-it was on the twentieth of March, 1888-I was returning"
  Define cipher.s = "HOLMESRTABCDFGIJKNPQUVWXYZ./", encoded.s
  
  PrintN("Original: " + message)
  encoded = encrypt_SC(message, cipher, 3, 7)
  PrintN("encoded: " + encoded)
  PrintN("decoded: " + decrypt_SC(encoded, cipher, 3, 7))
  
  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
```

Sample output:

```txt
Original: One night-it was on the twentieth of March, 1888-I was returning
encoded: 13953936350936974306139905974539936590134430832079179879879836743068597
2839363935
decoded: ONENIGHTITWASONTHETWENTIETHOFMARCH1888IWASRETURNING
```



## Python

Partially based on the PicoLisp version:

```python
T = [["79", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9"],
     ["",   "H", "O", "L", "",  "M", "E", "S", "",  "R", "T"],
     ["3",  "A", "B", "C", "D", "F", "G", "I", "J", "K", "N"],
     ["7",  "P", "Q", "U", "V", "W", "X", "Y", "Z", ".", "/"]]

def straddle(s):
    return "".join(L[0]+T[0][L.index(c)] for c in s.upper() for L in T if c in L)

def unstraddle(s):
    s = iter(s)
    for c in s:
        if c in [T[2][0], T[3][0]]:
            i = [T[2][0], T[3][0]].index(c)
            n = T[2 + i][T[0].index(s.next())]
            yield s.next() if n == "/" else n
        else:
            yield T[1][T[0].index(c)]

O = "One night-it was on the twentieth of March, 1888-I was returning"
print "Encoded:", straddle(O)
print "Decoded:", "".join(unstraddle(straddle(O)))
```

Output:

```txt
Encoded: 139539363509369743061399059745399365901344308320791798798798367430685972839363935
Decoded: ONENIGHTITWASONTHETWENTIETHOFMARCH1888IWASRETURNING
```



## Racket

Based in the PicoLisp version.

We store the straddling checkerboard in a structure, so it can be reused or changed for the different examples. The “constructor” transforms the lines of the checkboard into an internal representation. 

```Racket
#lang racket

(struct *straddling (header main original) 
  #:reflection-name 'straddling
  #:methods gen:custom-write 
  [(define (write-proc board port mode)
     (write-string "#<straddling " port)
     (write (*straddling-original board) port)
     (write-string ">" port))]) 

(define string->vector (compose list->vector string->list))

(define (straddling . lines)
  (define header-tail (reverse 
                       (for/fold ([rev-ret '()]) 
                                 ([char (in-list (string->list (car lines)))]
                                  [i (in-naturals)])
                         (if (equal? char #\space)
                             (cons (number->string i) rev-ret)
                             rev-ret))))
  (define main (list->vector
                (map string->vector
                     (cons "0123456789" 
                           (map string-upcase 
                                lines)))))
  (define temporal-board 
    (*straddling (list->vector (list* "?" "" header-tail)) main lines))
  (define escape (straddling-encode-char #\/ temporal-board))
  (*straddling (list->vector (list* escape "" header-tail)) main lines))
```

Now we define the functions to straddle and unstraddle the message.

```Racket
(define (straddling-encode-char char board)
  (or (for/or ([head (in-vector (*straddling-header board))]
                [line (in-vector (*straddling-main board))]) 
        (let ([pos (vector-member char line)])
          (if pos
              (string-append head (number->string pos))
              #f)))
      ""))

(define (straddle message board)
  (apply string-append 
         (map (lambda (char)
                (if (or (equal? char #\space) (equal? char #\/))
                    ""
                    (straddling-encode-char char board)))
              (string->list (string-upcase message)))))


(define (unstraddle message board)
  (define char->string string)
  (define (straddling-decode-char str row)
    (vector-ref (vector-ref (*straddling-main board) row) (string->number str)))
  (list->string
   (reverse
    (let-values ([(row rev-ret)
                  (for/fold ([row #f] ;row to read in multichar codes 
                                      ;#f means start of new code
                             [rev-ret '()]) ;result
                            ([str (map char->string (string->list (string-upcase message)))])
                    (if (not row)
                        (let ([pos (vector-member str (*straddling-header board))])
                          (if pos 
                              (values pos rev-ret)
                              (values #f (cons (straddling-decode-char str 1) rev-ret))))
                        (let ([decoded (straddling-decode-char str row)])
                          (if (equal? decoded #\/)
                              (values 0 rev-ret)
                              (values #f (cons decoded rev-ret))))))])
      (unless row ;check that last number was not missing
        rev-ret)))))
```

'''Two examples:'''

```Racket
(define (test-straddling message board)
  (let* ([encoded (straddle message board)]
         [decoded (unstraddle encoded board)])
    (displayln board)
    (displayln message)
    (displayln encoded)
    (displayln decoded)))
  
(test-straddling "One night-it was on the twentieth of March, 1888-I was returning"
                 (straddling "HOL MES RT"
                             "ABCDFGIJKN"
                             "PQUVWXYZ./"))
(newline)
(test-straddling "One night-it was on the twentieth of March, 1888-I was returning"
                 (straddling "ET AON RIS"
                             "BCDFGHJKLM"
                             "PQ/UVWXYZ."))
```

{{out}}

```txt
#<straddling ("HOL MES RT" "ABCDFGIJKN" "PQUVWXYZ./")>
One night-it was on the twentieth of March, 1888-I was returning
139539363509369743061399059745399365901344308320791798798798367430685972839363935
ONENIGHTITWASONTHETWENTIETHOFMARCH1888IWASRETURNING

#<straddling ("ET AON RIS" "BCDFGHJKLM" "PQ/UVWXYZ.")>
One night-it was on the twentieth of March, 1888-I was returning
450582425181653945125016505180125423293721256216286286288653970163758524
ONENIGHTITWASONTHETWENTIETHOFMARCH1888IWASRETURNING
```



## REXX

Extra coding was added to:
:*   allow the use of any table   (specifiable on the invocation of the   '''genCipher'''   subroutine, the 5<sup>th</sup> line in the REXX program), 
:*   no hard-coding of the location of blanks,
:*   no hard-coding or the numbered lines in the straddled checkerboard table, 
:*   no hard-coding of the location of the escape character, 
:*   support the usage of a blank in the 1<sup>st</sup> character (the top line of the table).

```rexx
/*REXX program uses the  straddling checkerboard  cipher to encrypt/decrypt a message.  */
arg msg                                          /*obtain optional message from the C.L.*/
if msg=''  then msg= 'One night-it was the twentieth of March, 1888-I was returning'
                      say 'plain text='  msg
call genCipher  'et aon ris',  'bcdfghjklm',  'pq/uvwxyz.'    /*build the cipher (board)*/
enc=encrypt(msg);     say ' encrypted='  enc     /*encrypt message and show encryption. */
dec=decrypt(enc);     say ' decrypted='  dec     /*decrypt    "     "    "  decryption. */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
genCipher: @.=;   arg @..,two,three;    z=-1; @.z= @..        /*build top row of cipher.*/
           _=pos(' ', @..     )  - 1;  @._= two               /*  "   2nd  "   "    "   */
           _=pos(' ', @.., _+2)  - 1;  @._= three             /*  "   3rd  "   "    "   */
             do j=0  for 9;    @..=@.. || @.j    /*construct a table for fast searching.*/
             if @.j\==''  then @.r=@.r || j
             _=pos('/', @.j)                     /*is the escape character in this row? */
             if _\==0  then @.dig=j || (_-1)     /*define    "       "     for numerals.*/
             end   /*j*/
           @..=space(@.., 0)                     /*purify the table of encryptable chars*/
           return
/*──────────────────────────────────────────────────────────────────────────────────────*/
encrypt: procedure expose @.;  arg !,,$          /*$:  output  (encrypted text)  so far.*/
           do j=1  for length(!)                 /*process each of the plain─text chars.*/
           x=substr(!, j, 1)                     /*obtain a message char to be encrypted*/
           if datatype(x, 'W')  then do;  $=$ || @.dig || x;  iterate;  end  /*numeral? */
           if pos(x, @..)==0    then iterate     /*Not one of the allowable chars?  Skip*/
              do k=-1  for 10;  y=pos(x, @.k)    /*traipse thru rows, looking for match.*/
              if y==0           then iterate     /*Not in this row?   Then keep looking.*/
              z=k;    if z==-1  then z=          /*construct the index of the cypher row*/
              $=$ || z || (y-1)                  /*add an encrypted character to output.*/
              iterate j                          /*go and process the next msg character*/
              end   /*k*/
           end      /*j*/
         return $
/*──────────────────────────────────────────────────────────────────────────────────────*/
decrypt: procedure expose @.;  parse arg !,,$    /*$:  output  (decrypted text)  so far.*/
           do j=1  to length(!);  rw=-1          /*process each of the encypted numbers.*/
            x=substr(!,j,1)                      /*obtain a message char to be decrypted*/
           if substr(!,j,2)==@.dig  then do; j=j+2; $=$ ||  substr(!, j, 1);  iterate; end
           if pos(x, @.r)\==0       then do; j=j+1; rw=x; x=substr(!, j, 1);           end
           $=$ || substr(@.rw, x+1, 1)           /*add a character to decrypted message.*/
           end   /*j*/
         return $
```

{{out|output|text=  when using the default input:}}

```txt

plain text= One night-it was the twentieth of March, 1888-I was returning
 encrypted= 4505824251816539125016505180125423293721256216286286288653970163758524
 decrypted= ONENIGHTITWASTHETWENTIETHOFMARCH1888IWASRETURNING

```



## Ruby

{{works with|Ruby|2.0}}

```ruby
class StraddlingCheckerboard
  EncodableChars = "A-Z0-9."
  SortedChars = "  ./" + [*"A".."Z"].join
  
  def initialize(board = nil)
    if board.nil?
      # create a new random board
      rest = "BCDFGHJKLMPQUVWXYZ/.".chars.shuffle
      @board = ["  ESTONIAR".chars.shuffle, rest[0..9], rest[10..19]]
    elsif board.chars.sort.join == SortedChars
      @board = board.chars.each_slice(10).to_a
    else
      raise ArgumentError, "invalid #{self.class}: #{board}"
    end
    # find the indices of the first row blanks
    @row_labels = @board[0].each_with_index.select {|v, i| v == " "}.map {|v,i| i}
    
    @mapping = {}
    @board[0].each_with_index {|char, idx| @mapping[char] = idx.to_s unless char == " "}
    @board[1..2].each_with_index do |row, row_idx|
      row.each_with_index do |char, idx|
        @mapping[char] = "%d%d" % [@row_labels[row_idx], idx]
      end
    end
  end
  
  def encode(message)
    msg = message.upcase.delete("^#{EncodableChars}")
    msg.chars.inject("") do |crypt, char|
      crypt << (char =~ /[0-9]/ ? @mapping["/"] + char : @mapping[char])
    end
  end
  
  def decode(code)
    msg = ""
    tokens = code.chars
    until tokens.empty?
      token = tokens.shift
      itoken = token.to_i
      unless @row_labels.include?(itoken)
        msg << @board[0][itoken]
      else
        token2 = tokens.shift
        if @mapping["/"] == token + token2
          msg << tokens.shift
        else
          msg << @board[1+@row_labels.index(itoken)][token2.to_i]
        end
      end
    end
    msg
  end
  
  def to_s
    @board.inject("") {|res, row| res << row.join}
  end
  
  def inspect
    "#<%s board=%p, row_labels=%p, mapping=%p>" % [self.class, to_s, @row_labels, @mapping]
  end
end
```


The test suite

```ruby
require 'test/unit'
class StraddlingCheckerboardTest < Test::Unit::TestCase
  def setup
    @msg = "One night-it was on the twentieth of March, 1888-I was returning"
    @expected = "ONENIGHTITWASONTHETWENTIETHOFMARCH1888IWASRETURNING"
  end
  
  def test1
    sc = StraddlingCheckerboard.new "ET AON RISBCDFGHJKLMPQ/UVWXYZ."
    code = sc.encode(@msg)
    plaintext = sc.decode(code)
    
    puts "using checkerboard: #{sc}"
    puts "original: #{@msg}"
    puts "encoded: #{code}"
    puts "decoded: #{plaintext}"
    assert_equal("450582425181653945125016505180125423293721256216286286288653970163758524", code)
    assert_equal(@expected, plaintext)
  end
  
  def test_board_with_space_in_first_char
    sc = StraddlingCheckerboard.new " RIOESN ATG./LXYBDKQMJUHWPVCFZ"
    code = sc.encode(@msg)
    plaintext = sc.decode(code)
    #p sc
    #puts "encoded: #{code}"
    #puts "decoded: #{plaintext}"
    assert_equal(@expected, plaintext)
  end
  
  def test_random_board
    sc = StraddlingCheckerboard.new
    plaintext = sc.decode(sc.encode(@msg))
    assert_equal(@expected, plaintext)
  end
  
  def test_invalid_input
    assert_raise(ArgumentError) {StraddlingCheckerboard.new "ET ON RISBCDFGHJKLMPQ/UVWXYZ.!"}
  end
end
```


output

```txt
Run options: 

# Running tests:

using checkerboard: ET AON RISBCDFGHJKLMPQ/UVWXYZ.
original: One night-it was on the twentieth of March, 1888-I was returning
encoded: 450582425181653945125016505180125423293721256216286286288653970163758524
decoded: ONENIGHTITWASONTHETWENTIETHOFMARCH1888IWASRETURNING
....

Finished tests in 0.007873s, 508.0378 tests/s, 635.0472 assertions/s.

4 tests, 5 assertions, 0 failures, 0 errors, 0 skips
```



## Scala

{{Out}}Best seen running in your browser either by [https://scalafiddle.io/sf/xCHsvaC/0 ScalaFiddle (ES aka JavaScript, non JVM)] or [https://scastie.scala-lang.org/42qQPkm6TiONpAOC1ByTCg Scastie (remote JVM)].

```Scala
object StraddlingCheckerboard extends App {

  private val dictonary = Map("H" -> "0", "O" -> "1",
    "L" -> "2", "M" -> "4", "E" -> "5", "S" -> "6", "R" -> "8", "T" -> "9",
    "A" -> "30", "B" -> "31", "C" -> "32", "D" -> "33", "F" -> "34", "G" -> "35",
    "I" -> "36", "J" -> "37", "K" -> "38", "N" -> "39", "P" -> "70", "Q" -> "71",
    "U" -> "72", "V" -> "73", "W" -> "74", "X" -> "75", "Y" -> "76", "Z" -> "77",
    "." -> "78", "/" -> "79", "0" -> "790", "1" -> "791", "2" -> "792", "3" -> "793",
    "4" -> "794", "5" -> "795", "6" -> "796", "7" -> "797", "8" -> "798", "9" -> "799")

  private def encode(s: String) =
    s.toUpperCase.map { case ch: Char => dictonary.getOrElse(ch.toString, "") }.mkString

  private def decode(s: String) = {
    val revDictionary: Map[String, String] = dictonary.map {case (k, v) => (v, k)}

    val pat = "(79.|3.|7.|.)".r
    pat.findAllIn(s).map { el => revDictionary.getOrElse(el, "")}.addString(new StringBuilder)
  }

  val enc = encode(
    "One night-it was on the twentieth of March, " + "1888-I was returning"
  )
  println(enc)
  println(decode(enc))
}
```



## Tcl

Structured as a class, the instances of which contain encoding and decoding mappings suitable for use with Tcl's built in string remapping engine. This uses the fact that no sequence of digits in the encoded form that maps to one character is a prefix of the sequence for any other character. (Inspired by the ''description'' of the [[#Perl 6|Perl 6]] solution.)<!--not a translation though; I didn't read the perl6 code…-->

```tcl
package require Tcl 8.6

oo::class create StraddlingCheckerboardCypher {
    variable encmap decmap
    constructor table {
	# Sanity check the table
	foreach ch [lindex $table 0] i {"" 0 1 2 3 4 5 6 7 8 9} {
	    if {$ch eq "" && $i ne "" && [lsearch -index 0 $table $i] < 1} {
		error "bad checkerboard table"
	    }
	}
	# Synthesize the escaped number row
	foreach row $table {
	    if {"/" ni $row} continue
	    set pfx [lindex $row 0][expr {[lsearch -exact $row "/"]-1}]
	    lappend table [list $pfx 0 1 2 3 4 5 6 7 8 9]
	    break
	}
	# Build the actual per-character mapping
	foreach row $table {
	    foreach ch [lrange $row 1 end] n {0 1 2 3 4 5 6 7 8 9} {
		if {$ch in {"" "/"}} continue;  # Skip escape cases
		lappend encmap $ch [lindex $row 0]$n
		lappend decmap [lindex $row 0]$n $ch
	    }
	}
    }

    # These methods just sanitize their input and apply the map
    method encode msg {
	string map $encmap [regsub -all {[^A-Z0-9.]} [string toupper $msg] ""]
    }
    method decode msg {
	string map $decmap [regsub -all {[^0-9]} $msg ""]
    }
}
```

Demonstration code:

```tcl
StraddlingCheckerboardCypher create demo {
    {{} E T {} A O N {} R I S}
    {2  B C D  F G H J  K L M}
    {6  P Q /  U V W X  Y Z .}
}
set input "One night-it was on the twentieth of March, 1888-I was returning"
set encrypted [demo encode $input]
set output [demo decode $encrypted]
puts "Input:   $input"
puts "Encoded: $encrypted"
puts "Decoded: $output"
```

Output:

```txt

Input:   One night-it was on the twentieth of March, 1888-I was returning
Encoded: 450582425181653945125016505180125423293721256216286286288653970163758524
Decoded: ONENIGHTITWASONTHETWENTIETHOFMARCH1888IWASRETURNING

```



## VBScript

{{trans|PureBasic}}

```vb
' Straddling checkerboard - VBScript - 19/04/2019

Function encrypt(ByVal originalText, ByVal alphabet, blank1, blank2)
  Const notEscape = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ."
  Dim i, j, curChar, escChar, outText
  Dim cipher  'Hash table
  Set cipher = CreateObject("Scripting.Dictionary")
  'build cipher reference
  alphabet = UCase(alphabet) : j = 0
  For i = 1 To 28
    curChar = Mid(alphabet, i, 1)
    Select Case True
      Case i>= 1 And i<= 8
        If j = blank1 Or j = blank2 Then j = j + 1  'adjust for blank
		cipher.Add curChar, CStr(j)
        j = j + 1
      Case i>= 9 And i<=18
		cipher.Add curChar, CStr(blank1) & CStr(i -  9)
      Case i>=19 And i<=28
		cipher.Add curChar, CStr(blank2) & CStr(i - 19)
    End Select 'i
    If InStr(notEscape, curChar) = 0 Then
      escChar = curChar
      'Wscript.Echo "escChar=" & escChar & "  cipher(escChar)=" & cipher(escChar)
    End If
  Next 'i
  For i = 0 To 9: cipher.Add CStr(i), cipher(escChar) & CStr(i): Next 
  'encrypt each character
  originalText = UCase(originalText)
  For i = 1 To Len(originalText)
    outText = outText & cipher(Mid(originalText, i, 1))
  Next 
  encrypt=outText
End Function 'encrypt
 
Function decrypt(ByVal cipherText, ByVal alphabet, blank1, blank2)
  Const notEscape = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ."
  Dim i, j, curChar, escCipher, outText
  Dim cipher  'Hash table
  Set cipher = CreateObject("Scripting.Dictionary")
  'build decipher reference
  alphabet = UCase(alphabet) : j = 0
  For i = 1 To 28
    curChar = Mid(alphabet, i, 1)
    Select Case True
      Case i>= 1 And i<= 8
        If j = blank1 Or j = blank2 Then j = j + 1  'adjust for blank
		cipher.Add CStr(j),curChar
        j = j + 1
      Case i>= 9 And i<=18
        cipher.Add CStr(blank1) & CStr(i -  9), curChar
      Case i>=19 And i<=28
        cipher.Add CStr(blank2) & CStr(i - 19), curChar
    End Select 'i
    If InStr(notEscape, curChar) = 0 Then
      'the last element of cipher
      arrayKeys=cipher.keys
      escCipher = arrayKeys(cipher.count-1)
      'Wscript.Echo "escCipher=" & escCipher & "  cipher(escCipher)=" & cipher(escCipher)
    End If
  Next 'i
  For i = 0 To 9: cipher.Add escCipher & CStr(i), CStr(i): Next 
  'decrypt each character
  i = 1
  Do While i <= Len(cipherText)
    curChar = Mid(cipherText, i, 1)
    If curChar = CStr(blank1) Or curChar = CStr(blank2) Then
      curChar = Mid(cipherText, i, 2)
      If curChar = escCipher Then curChar = Mid(cipherText, i, 3)
    End If 
    outText = outText & cipher(curChar)
    i = i + Len(curChar)
  Loop 'i
  decrypt=outText
End Function 'decrypt
 
  message = "One night-it was on the twentieth of March, 1888-I was returning"
  cipher = "HOLMESRTABCDFGIJKNPQUVWXYZ./"
  '3 7         <=8
  'HOL MES RT
  'ABCDFGIJKN
  'PQUVWXYZ./
  Buffer=Buffer & "Original: " & message & vbCrlf 
  encoded = encrypt(message, cipher, 3, 7)
  Buffer=Buffer &  "encoded: " & encoded & vbCrlf
  decoded = decrypt(encoded, cipher, 3, 7)
  Buffer=Buffer &  "decoded: " & decoded & vbCrlf
  Wscript.Echo Buffer 
```

{{out}}

```txt

Original: One night-it was on the twentieth of March, 1888-I was returning
encoded: 139539363509369743061399059745399365901344308320791798798798367430685972839363935
decoded: ONENIGHTITWASONTHETWENTIETHOFMARCH1888IWASRETURNING

```





## zkl

{{trans|D}}

```zkl
var [const]
   val2key=Dictionary(
    "A","30",  "B","31",  "C","32",  "D","33",  "E","5",   "F","34",  "G","35",
    "H","0",   "I","36",  "J","37",  "K","38",  "L","2",   "M","4",   ".","78", 
    "N","39",  "/","79",  "O","1",   "0","790", "P","70",  "1","791", "Q","71", 
    "2","792", "R","8",   "3","793", "S","6",   "4","794" ,"T","9",   "5","795",
    "U","72",  "6","796", "V","73",  "7","797", "W","74",  "8","798", "X","75",
    "9","799", "Y","76",  "Z","77"),
   key2val=val2key.pump(Dictionary(),"reverse");
 
fcn encode(txt){ txt.toUpper().pump(String,val2key.find.fp1("")) }
fcn decode(str){ RegExp("79.|3.|7.|.").pump(str,String,key2val.get) }
```


```zkl
txt:="One night-it was on the twentieth of March, 1888-I was returning";
str:=encode(txt);
println("Encoded: ", str);
println("Decoded: ", decode(str));
```

{{out}}

```txt

ncoded: 139539363509369743061399059745399365901344308320791798798798367430685972839363935
Decoded: ONENIGHTITWASONTHETWENTIETHOFMARCH1888IWASRETURNING

```

