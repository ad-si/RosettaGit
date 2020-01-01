+++
title = "Move-to-front algorithm"
description = ""
date = 2019-04-22T10:59:28Z
aliases = []
[extra]
id = 17636
[taxonomies]
categories = []
tags = []
+++

{{task}}
Given a symbol table of a ''zero-indexed'' array of all possible input symbols
[[wp:Move-to-front transform|this algorithm]] reversibly transforms a sequence
of input symbols into an array of output numbers (indices).

The transform in many cases acts to give frequently repeated input symbols
lower indices which is [[wp:Move-to-front_transform#Use_in_practical_data_compression_algorithms| useful in some compression algorithms]].

;Encoding algorithm:

```txt

    for each symbol of the input sequence:
        output the index of the symbol in the symbol table
        move that symbol to the front of the symbol table

```


;Decoding algorithm:

```txt

    # Using the same starting symbol table
    for each index of the input sequence:
        output the symbol at that index of the symbol table
        move that symbol to the front of the symbol table

```


;Example:
Encoding the string of character symbols 'broood' using a symbol table of
the characters 'a'-to-'z'

{| class="wikitable" border="1"
|-
! Input
! Output
! SymbolTable
|-
| '''b'''roood
| 1
| 'abcdefghijklmnopqrstuvwxyz'
|-
| b'''r'''oood
| 1 17
| 'bacdefghijklmnopqrstuvwxyz'
|-
| br'''o'''ood
| 1 17 15
| 'rbacdefghijklmnopqstuvwxyz'
|-
| bro'''o'''od
| 1 17 15 0
| 'orbacdefghijklmnpqstuvwxyz'
|-
| broo'''o'''d
| 1 17 15 0 0
| 'orbacdefghijklmnpqstuvwxyz'
|-
| brooo'''d'''
| 1 17 15 0 0 5
| 'orbacdefghijklmnpqstuvwxyz'
|}

Decoding the indices back to the original symbol order:
{| class="wikitable" border="1"
|-
! Input
! Output
! SymbolTable
|-
|  '''1''' 17 15 0 0 5
| b
| 'abcdefghijklmnopqrstuvwxyz'
|-
| 1 '''17''' 15 0 0 5
| br
| 'bacdefghijklmnopqrstuvwxyz'
|-
| 1 17 '''15''' 0 0 5
| bro
| 'rbacdefghijklmnopqstuvwxyz'
|-
| 1 17 15 '''0''' 0 5
| broo
| 'orbacdefghijklmnpqstuvwxyz'
|-
| 1 17 15 0 '''0''' 5
| brooo
| 'orbacdefghijklmnpqstuvwxyz'
|-
| 1 17 15 0 0 '''5'''
| broood
| 'orbacdefghijklmnpqstuvwxyz'
|}

;Task:
* Encode and decode the following three strings of characters using the symbol table of the characters 'a'-to-'z' as above.
* Show the strings and their encoding here.
* Add a check to ensure that the decoded string is the same as the original.



The strings are:

   <big> broood       </big>
   <big> bananaaa     </big>
   <big> hiphophiphop </big>

(Note the spellings.)





## Ada



```Ada
with Ada.Text_IO;

procedure Move_To_Front is

   subtype Lower_Case is Character range 'a' .. 'z';
   subtype Index is Integer range 0 .. 25;
   type Table is array (Index) of Lower_Case;
   Alphabet: constant Table := "abcdefghijklmnopqrstuvwxyz";
   type Number_String is array(Positive range <>) of Natural;

   function Encode(S: String) return Number_String is
      Key: Table := Alphabet;

      function Encode(S: String; Tab: in out Table) return Number_String is

	 procedure Look_Up(A: in out Table; Ch: Lower_Case; Pos: out Index) is
	 begin
	    for I in A'Range loop
	       if A(I) = Ch then
		  Pos := I;
		  A := A(Pos) & A(A'First .. Pos-1) & A(Pos+1 .. A'Last);
		  return;
	       end if;
	    end loop;
	    raise Program_Error with "unknown character";
	 end Look_Up;

	 Empty: Number_String(1 .. 0);
	 Result: Natural;
      begin
	 if S'Length = 0 then
	    return Empty;
	 else
	    Look_Up(Tab, S(S'First), Result);
	    return Result & Encode(S(S'First+1 .. S'Last), Tab);
	 end if;
      end Encode;

   begin
      return Encode(S, Key);
   end Encode;

   function Decode(N: Number_String) return String is
      Key: Table := Alphabet;

      function Decode(N: Number_String; Tab: in out Table) return String is

	 procedure Look_Up(A: in out Table; Pos: Index; Ch: out Lower_Case) is
	 begin
	    Ch := A(Pos);
	    A := A(Pos) & A(A'First .. Pos-1) & A(Pos+1 .. A'Last);
	 end Look_Up;

	 Result: String(N'Range);
      begin
	 for I in N'Range loop
	    Look_Up(Tab, N(I), Result(I));
	 end loop;
	 return Result;
      end Decode;

   begin
      return Decode(N, Key);
   end Decode;

   procedure Encode_Write_Check(S: String) is
      N: Number_String := Encode(S);
      T: String := Decode(N);
      Check: String := (if S=T then "Correct!" else "*WRONG*!");
   begin
      Ada.Text_IO.Put("'" & S & "' encodes to");
      for Num of N loop
	 Ada.Text_IO.Put(Integer'Image(Num));
      end loop;
      Ada.Text_IO.Put_Line(". This decodes to '" & T & "'. " & Check);
   end Encode_Write_Check;

begin
   Encode_Write_Check("broood");
   Encode_Write_Check("bananaaa");
   Encode_Write_Check("hiphophiphop");
end Move_To_Front;
```


{{out}}


```txt
'broood' encodes to 1 17 15 0 0 5. This decodes to 'broood'. Correct!
'bananaaa' encodes to 1 1 13 1 1 1 0 0. This decodes to 'bananaaa'. Correct!
'hiphophiphop' encodes to 7 8 15 2 15 2 2 3 2 2 3 2. This decodes to 'hiphophiphop'. Correct!
```



## Aime


```aime
text
decode(list l)
{
    integer c, e;
    data al, s;

    al = "abcdefghijklmnopqrstuvwxyz";
    for (, e in l) {
        s.append(c = al[e]);
        al.delete(e).insert(0, c);
    }

    s;
}

list
encode(text s)
{
    integer c, e;
    data al;
    list l;

    al = "abcdefghijklmnopqrstuvwxyz";
    for (, c in b_draft(s)) {
        l.append(e = al.place(c));
        al.delete(e).insert(0, c);
    }

    l;
}

integer
main(void)
{
    for (, text s in list("broood", "bananaaa", "hiphophiphop")) {
        list l;

        l = encode(s);
        l.ucall(o_, 1, " ");
        o_(": ", decode(l), "\n");
    }

    0;
}
```

{{out}}

```txt
 1 17 15 0 0 5: broood
 1 1 13 1 1 1 0 0: bananaaa
 7 8 15 2 15 2 2 3 2 2 3 2: hiphophiphop
```



## ALGOL 68

{{works with|ALGOL 68G|Any - tested with release 2.8.win32}}

```algol68
# move the character at text pos to the front of text                         #
# note text pos is based from 0                                               #
PROC move to front = ( STRING text, INT text pos )STRING:
    IF text pos < 1
    THEN
        # the character is already at the front (or not in the string)        #
        text
    ELSE
        # the character isn't already at the front - construct the new string #

        INT    char pos = LWB text + text pos;

        ( text[ char pos     : char pos     ]
        + text[              : char pos - 1 ]
        + text[ char pos + 1 :              ]
        )

    FI;


# encode the string "text", using "initial table" as the starting symbol table#
PROC encode = ( STRING text, STRING initial table )[]INT:
BEGIN

    [ 1 : ( UPB text - LWB text ) + 1 ]INT result;

    STRING symbol table := initial table;

    FOR text pos FROM LWB text TO UPB text
    DO
        INT    symbol pos := 0;

        result[ text pos ]
            := IF char in string( text[ text pos ], symbol pos, symbol table )
               THEN
                   # the character is in the symbol table at symbol pos       #
                   # (indexed from LWB text) - we store the positions         #
                   # indexed from 0                                           #
                   symbol pos - LWB text
               ELSE
                   # the character isn't in the symbol table                  #
                   -1
               FI;

        # modify the symbol table so the latest character is at the front     #
        symbol table := move to front( symbol table, result[ text pos ] )

    OD;

    result
END; # encode #



# decode "encoded", using "initial table" as the starting symbol table        #
PROC decode = ( []INT encoded, STRING initial table )STRING:
BEGIN

    STRING result       := "";
    STRING symbol table := initial table;

    FOR text pos FROM LWB encoded TO UPB encoded
    DO
        result
           +:= IF encoded[ text pos ] < 0
               THEN
                   # the encoded character wasn't in the string               #
                   "?"
               ELSE
                  # the character is in the symbol table                      #
                  symbol table[ encoded[ text pos ] + LWB symbol table ]
                FI;

        # modify the symbol table so the latest character is at the front     #
        symbol table := move to front( symbol table, encoded[ text pos ] )

    OD;

    result
END; # decode #



# routine to test the encode and decode routines                              #
PROC test encode and decode = ( STRING text )VOID:
BEGIN

    # initial value for the symbol table                                      #
    []CHAR initial table = "abcdefghijklmnopqrstuvwxyz";

    # procedure to format the encoded value                                   #
    PROC format encoded value = ( []INT values )STRING:
    BEGIN
        STRING result := "";
        FOR value pos FROM LWB values TO UPB values
        DO
            result +:= ( " " + whole( values[ value pos ], 0 ) )
        OD;

        result
    END; # format encoded value #

    []INT  encoded = encode( text,    initial table );
    STRING decoded = decode( encoded, initial table );

    print( ( ( text
             + " encodes to ["
             + format encoded value( encoded )
             + " ] which "
             + IF text = decoded
               THEN
                   "correctly"
               ELSE
                   "INCORRECTLY"
               FI
             + " decodes to """
             + decoded
             + """"
             )
           , newline
           )
         )

END; # test encode and decode #



main: (

     test encode and decode( "broood" )
  ;  test encode and decode( "bananaaa" )
  ;  test encode and decode( "hiphophiphop" )

# ;  test encode and decode( "abcdefghijklmnopqrstuvwxyz" ) #
# ;  test encode and decode( "zyxwvutsrqponmlkjihgfedcba" ) #

)
```

{{out}}

```txt

broood encodes to [ 1 17 15 0 0 5 ] which correctly decodes to "broood"
bananaaa encodes to [ 1 1 13 1 1 1 0 0 ] which correctly decodes to "bananaaa"
hiphophiphop encodes to [ 7 8 15 2 15 2 2 3 2 2 3 2 ] which correctly decodes to "hiphophiphop"

```



## AutoHotkey


```AutoHotkey
MTF_Encode(string){
	str := "abcdefghijklmnopqrstuvwxyz"
	loop, parse, string
		code .= (A_Index>1 ? ",":"") . InStr(str, A_LoopField) - 1	, str := A_LoopField . StrReplace(str, A_LoopField)
	return code
}

MTF_Decode(code){
	str := "abcdefghijklmnopqrstuvwxyz"
	loop, parse, code, `,
		string .= (letter := SubStr(str, A_LoopField+1, 1)) 		, str := letter . StrReplace(str, letter)
	return string
}

```

Examples:
```AutoHotkey
testStrings = broood,bananaaa,hiphophiphop
loop, parse, testStrings, `,
	Output .= A_LoopField "`t" MTF_Encode(A_LoopField) "`t" MTF_Decode(MTF_Encode(A_LoopField)) "`n"
MsgBox  % Output
return
```

Outputs:
```txt
broood		1,17,15,0,0,5			broood
bananaaa	1,1,13,1,1,1,0,0		bananaaa
hiphophiphop	7,8,15,2,15,2,2,3,2,2,3,2	hiphophiphop
```



## Bracmat


```bracmat
  ( encode
  =   string symboltable
    .   !arg:(?string.?symboltable)
      &   vap
        $ ( (
            =   A Z i
              .   !symboltable:?A [?i !arg ?Z
                & !arg !A !Z:?symboltable
                & !i
            )
          . !string
          )
  )
& ( decode
  =   indices symboltable
    .   !arg:(?indices.?symboltable)
      &   str
        $ ( map
          $ ( (
              =   A Z symbol
                .   !symboltable:?A [!arg %?symbol ?Z
                  & !symbol !A !Z:?symboltable
                  & !symbol
              )
            . !indices
            )
          )
  )
& ( test
  =   string symboltable encoded decoded
    .   !arg:(?string.?symboltable)
      & put$str$("input:" !string ", ")
      & encode$(!string.!symboltable):?encoded
      & put$("encoded:" !encoded ", ")
      & decode$(!encoded.!symboltable):?decoded
      & put$str$("decoded:" !decoded ", ")
      & (   !string:!decoded
          & out$OK
        | out$WRONG
        )
  )
&   a b c d e f g h i j k l m n o p q r s t y v w x y z
  : ?symboltable
& test$(broood.!symboltable)
& test$(bananaaa.!symboltable)
& test$(hiphophiphop.!symboltable)
```



## C


```c
#include <stdio.h>
#include<stdlib.h>
#include<string.h>

#define MAX_SIZE 100

int move_to_front(char *str,char c)
{
    char *q,*p;
    int shift=0;
    p=(char *)malloc(strlen(str)+1);
    strcpy(p,str);
    q=strchr(p,c); //returns pointer to location of char c in string str
    shift=q-p;      // no of characters from 0 to position of c in str
    strncpy(str+1,p,shift);
    str[0]=c;
    free(p);
  //  printf("\n%s\n",str);
    return shift;
}

void decode(int* pass,int size,char *sym)
{
    int i,index;
    char c;
    char table[]="abcdefghijklmnopqrstuvwxyz";
    for(i=0;i<size;i++)
    {
        c=table[pass[i]];
        index=move_to_front(table,c);
        if(pass[i]!=index) printf("there is an error");
        sym[i]=c;
    }
    sym[size]='\0';
}

void encode(char *sym,int size,int *pass)
{
    int i=0;
    char c;
    char table[]="abcdefghijklmnopqrstuvwxyz";
    for(i=0;i<size;i++)
    {
        c=sym[i];
        pass[i]=move_to_front(table,c);
    }
}

int check(char *sym,int size,int *pass)
{
    int *pass2=malloc(sizeof(int)*size);
    char *sym2=malloc(sizeof(char)*size);
    int i,val=1;

    encode(sym,size,pass2);
    i=0;
    while(i<size && pass[i]==pass2[i])i++;
    if(i!=size)val=0;

    decode(pass,size,sym2);
    if(strcmp(sym,sym2)!=0)val=0;

    free(sym2);
    free(pass2);

    return val;
}

int main()
{
    char sym[3][MAX_SIZE]={"broood","bananaaa","hiphophiphop"};
    int pass[MAX_SIZE]={0};
    int i,len,j;
    for(i=0;i<3;i++)
    {
        len=strlen(sym[i]);
        encode(sym[i],len,pass);
        printf("%s : [",sym[i]);
        for(j=0;j<len;j++)
            printf("%d ",pass[j]);
        printf("]\n");
        if(check(sym[i],len,pass))
            printf("Correct :)\n");
        else
            printf("Incorrect :(\n");
    }
    return 0;
}
```

{{Out}}

```txt
broood : [1 17 15 0 0 5 ]
Correct :)
bananaaa : [1 1 13 1 1 1 0 0 ]
Correct :)
hiphophiphop : [7 8 15 2 15 2 2 3 2 2 3 2 ]
Correct :)
```



## C++


```Cpp

#include <iostream>
#include <iterator>
#include <sstream>
#include <vector>

using namespace std;

class MTF
{
public:
    string encode( string str )
    {
	fillSymbolTable();
	vector<int> output;
	for( string::iterator it = str.begin(); it != str.end(); it++ )
	{
	    for( int i = 0; i < 26; i++ )
	    {
		if( *it == symbolTable[i] )
		{
		    output.push_back( i );
		    moveToFront( i );
		    break;
		}
	    }
	}
	string r;
	for( vector<int>::iterator it = output.begin(); it != output.end(); it++ )
	{
	    ostringstream ss;
	    ss << *it;
	    r += ss.str() + " ";
	}
	return r;
    }

    string decode( string str )
    {
	fillSymbolTable();
	istringstream iss( str ); vector<int> output;
	copy( istream_iterator<int>( iss ), istream_iterator<int>(), back_inserter<vector<int> >( output ) );
	string r;
	for( vector<int>::iterator it = output.begin(); it != output.end(); it++ )
	{
	    r.append( 1, symbolTable[*it] );
	    moveToFront( *it );
	}
	return r;
    }

private:
    void moveToFront( int i )
    {
	char t = symbolTable[i];
	for( int z = i - 1; z >= 0; z-- )
	    symbolTable[z + 1] = symbolTable[z];

        symbolTable[0] = t;
    }

    void fillSymbolTable()
    {
        for( int x = 0; x < 26; x++ )
	    symbolTable[x] = x + 'a';
    }

    char symbolTable[26];
};

int main()
{
    MTF mtf;
    string a, str[] = { "broood", "bananaaa", "hiphophiphop" };
    for( int x = 0; x < 3; x++ )
    {
        a = str[x];
        cout << a << " -> encoded = ";
        a = mtf.encode( a );
        cout << a << "; decoded = " << mtf.decode( a ) << endl;
    }
    return 0;
}

```

{{out}}

```txt

broood -> encoded = 1 17 15 0 0 5 ; decoded = broood
bananaaa -> encoded = 1 1 13 1 1 1 0 0 ; decoded = bananaaa
hiphophiphop -> encoded = 7 8 15 2 15 2 2 3 2 2 3 2 ; decoded = hiphophiphop

```



## C#


```csharp

using System;
using System.Collections.Generic;
using System.Text;

namespace MoveToFront
{
    class Program
    {
        private static char[] symbolTable;
        private static void setSymbolTable()
        {
            symbolTable = "abcdefghijklmnopqrstuvwxyz".ToCharArray();
        }

        private static void moveToFront(int charIndex)
        {
            char toFront = symbolTable[charIndex];
            for (int j = charIndex; j > 0; j--)
            {
                symbolTable[j] = symbolTable[j - 1];
            }
            symbolTable[0] = toFront;
        }

        public static int[] Encode(string input)
        {
            setSymbolTable();
            var output = new List<int>();
            foreach (char c in input)
            {
                for (int i = 0; i < 26; i++)
                {
                    if (symbolTable[i] == c)
                    {
                        output.Add(i);
                        moveToFront(i);
                        break;
                    }
                }
            }
            return output.ToArray();
        }

        public static string Decode(int[] input)
        {
            setSymbolTable();
            var output = new StringBuilder(input.Length);
            foreach (int n in input)
            {
                output.Append(symbolTable[n]);
                moveToFront(n);
            }
            return output.ToString();
        }

        static void Main(string[] args)
        {
            string[] testInputs = new string[] { "broood", "bananaaa", "hiphophiphop" };
            int[] encoding;
            foreach (string s in testInputs)
            {
                Console.WriteLine($"Encoding for '{s}':");
                encoding = Encode(s);
                foreach (int i in encoding)
                {
                    Console.Write($"{i} ");
                }
                Console.WriteLine($"\nDecoding for '{s}':");
                Console.WriteLine($"{Decode(encoding)}\n");
            }
        }
    }
}

```



## Clojure


```clojure
(def lowercase (map char (range (int \a) (inc (int \z)))))

(defn move-to-front [x xs]
  (cons x (remove #{x} xs)))

(defn encode [text table & {:keys [acc] :or {acc []}}]
  (let [c (first text)
        idx (.indexOf table c)]
    (if (empty? text) acc (recur (drop 1 text) (move-to-front c table) {:acc (conj acc idx)}))))

(defn decode [indices table & {:keys [acc] :or {acc []}}]
  (if (empty? indices) (apply str acc)
    (let [n (first indices)
          c (nth table n)]
      (recur (drop 1 indices) (move-to-front c table) {:acc (conj acc c)}))))

(doseq [word ["broood" "bananaaa" "hiphophiphop"]]
  (let [encoded (encode word lowercase)
        decoded (decode encoded lowercase)]
    (println (format "%s encodes to %s which decodes back to %s."
                   word encoded decoded))))
```


{{Out}}

```txt

broood encodes to [1 17 15 0 0 5] which decodes back to broood.
bananaaa encodes to [1 1 13 1 1 1 0 0] which decodes back to bananaaa.
hiphophiphop encodes to [7 8 15 2 15 2 2 3 2 2 3 2] which decodes back to hiphophiphop.

```



## Common Lisp


```lisp
(defconstant +lower+ (coerce "abcdefghijklmnopqrstuvwxyz" 'list))

(defun move-to-front (x xs)
  (cons x (remove x xs)))

(defun enc (text table)
  (map 'list
       (lambda (c)
               (let ((idx (position c table)))
                 (setf table (move-to-front c table))
                 idx))
       text))

(defun dec (indices table)
    (coerce (mapcar (lambda (idx)
                      (let ((c (nth idx table)))
                        (setf table (move-to-front c table))
                        c))
                    indices)
            'string))

(loop for word in '("broood" "bananaaa" "hiphophiphop")
      do (let* ((encoded (enc word +lower+))
                (decoded (dec encoded +lower+)))
           (assert (string= word decoded))
           (format T "~s encodes to ~a which decodes back to ~s.~%"
                   word encoded decoded)))
```


{{Out}}

```txt
"broood" encodes to (1 17 15 0 0 5) which decodes back to "broood".
"bananaaa" encodes to (1 1 13 1 1 1 0 0) which decodes back to "bananaaa".
"hiphophiphop" encodes to (7 8 15 2 15 2 2 3 2 2 3 2) which decodes back to "hiphophiphop".
```



## D


```d
import std.stdio, std.string, std.ascii, std.algorithm;

ptrdiff_t[] mtfEncoder(in string data) pure nothrow @safe
in {
    assert(data.countchars("a-z") == data.length);
} out(result) {
    assert(result.length == data.length);
    assert(result.all!(e => e >= 0 && e < lowercase.length));
} body {
    ubyte[lowercase.length] order = lowercase.representation;
    auto encoded = new typeof(return)(data.length);

    size_t i = 0;
    foreach (immutable b; data) {
        immutable j = encoded[i++] = order[].countUntil(b);
        bringToFront(order[0 .. j], order[j .. j + 1]);
    }

    return encoded;
}

string mtfDecoder(in ptrdiff_t[] encoded) pure nothrow @safe
in {
    assert(encoded.all!(e => e >= 0 && e < lowercase.length));
} out(result) {
    assert(result.length == encoded.length);
    assert(result.countchars("a-z") == result.length);
} body {
    ubyte[lowercase.length] order = lowercase.representation;
    auto decoded = new char[encoded.length];

    size_t i = 0;
    foreach (immutable code; encoded) {
        decoded[i++] = order[code];
        bringToFront(order[0 .. code], order[code .. code + 1]);
    }

    return decoded;
}

void main() {
    foreach (immutable word; ["broood", "bananaaa", "hiphophiphop"]) {
        immutable encoded = word.mtfEncoder;
        immutable decoded = encoded.mtfDecoder;
        writefln("'%s' encodes to %s, which decodes back to '%s'",
                 word, encoded, decoded);
        assert(word == decoded);
    }
}
```

{{out}}

```txt
'broood' encodes to [1, 17, 15, 0, 0, 5], which decodes back to 'broood'
'bananaaa' encodes to [1, 1, 13, 1, 1, 1, 0, 0], which decodes back to 'bananaaa'
'hiphophiphop' encodes to [7, 8, 15, 2, 15, 2, 2, 3, 2, 2, 3, 2], which decodes back to 'hiphophiphop'
```



## Elixir


```elixir
defmodule MoveToFront do
  @table  Enum.to_list(?a..?z)

  def encode(text), do: encode(to_char_list(text), @table, [])

  defp encode([], _, output), do: Enum.reverse(output)
  defp encode([h|t], table, output) do
    i = Enum.find_index(table, &(&1 == h))
    encode(t, move2front(table, i), [i | output])
  end

  def decode(indices), do: decode(indices, @table, [])

  defp decode([], _, output), do: Enum.reverse(output) |> to_string
  defp decode([h|t], table, output) do
    decode(t, move2front(table, h), [Enum.at(table, h) | output])
  end

  def move2front(table, i), do: [Enum.at(table,i) | List.delete_at(table, i)]
end

Enum.each(["broood", "bananaaa", "hiphophiphop"], fn word ->
  IO.inspect word
  IO.inspect enc = MoveToFront.encode(word)
  IO.puts "#{word == MoveToFront.decode(enc)}\n"
end)
```


{{out}}

```txt

"broood"
[1, 17, 15, 0, 0, 5]
true

"bananaaa"
[1, 1, 13, 1, 1, 1, 0, 0]
true

"hiphophiphop"
[7, 8, 15, 2, 15, 2, 2, 3, 2, 2, 3, 2]
true

```



## Factor


```factor
USING: formatting kernel locals make sequences ;

: to-front ( elt seq -- seq' ) over [ remove ] dip prefix ;

: encode ( symbols msg -- indices )
    [ [ swap 2dup index , to-front ] each ] { } make nip ;

: decode ( symbols indices -- msg )
    [ [ swap [ nth ] keep over , to-front ] each ] "" make nip ;

:: round-trip ( msg symbols -- )
    symbols msg encode :> encoded
    symbols encoded decode :> decoded
    msg decoded assert=
    msg encoded decoded "%s -> %u -> %s\n" printf ;

"broood" "bananaaa" "hiphophiphop"
[ "abcdefghijklmnopqrstuvwxyz" round-trip ] tri@
```

{{out}}

```txt

broood -> { 1 17 15 0 0 5 } -> broood
bananaaa -> { 1 1 13 1 1 1 0 0 } -> bananaaa
hiphophiphop -> { 7 8 15 2 15 2 2 3 2 2 3 2 } -> hiphophiphop

```



## Gambas

'''[https://gambas-playground.proko.eu/?gist=4268c779c36def03ea10764630cdc401 Click this link to run this code]'''

```gambas
Public Sub Main()
Dim sToCode As String[] = ["broood", "bananaaa", "hiphophiphop"]  'Samples to process
Dim sHold As New String[]                                         'To store results
Dim siCount, siCounter, siPos As Short                            'Various variables
Dim sOutput, sCode, sWork, sEach As String                        'Various variables

For siCounter = 0 To sToCode.Max                                  'To loop through each 'Sample'
  sCode = "abcdefghijklmnopqrstuvwxyz"                            'Set sCode to default setting
  For siCount = 1 To Len(sToCode[siCounter])                      'Loop through each letter in 'Sample'
    sWork = Mid(sToCode[siCounter], siCount, 1)                   'sWork to store the Letter
    siPos = InStr(scode, sWork) - 1                               'Find the position of the letter in sCode, -1 for '0' based array
    sOutput &= Str(siPos) & " "                                   'Add the postion to sOutput
    sCode = Mid(sCode, siPos + 1, 1) & Replace(sCode, sWork, "")  'sCode = the letter + the rest of sCode except the letter
  Next
  Print sToCode[siCounter] & " = " & sOutput                      'Print the 'Sample' and the coded version
  sHold.Add(Trim(sOutput))                                        'Add the code to the sHold array
  sOutput = ""                                                    'Clear sOutput
Next

Print                                                             'Print a blank line

For siCounter = 0 To sHold.Max                                    'To loop through each coded 'Sample'
  sCode = "abcdefghijklmnopqrstuvwxyz"                            'Set sCode to default setting
  For Each sEach In Split(sHold[siCounter], " ")                  'For each 'code' in coded 'Sample'
    sWork = Mid(sCode, Val(sEach) + 1, 1)                         'sWork = the decoded letter
    sOutput &= sWork                                              'Add the decoded letter to sOutput
    sCode = sWork & Replace(sCode, sWork, "")                     'sCode = the decoded letter + the rest of sCode except the letter
  Next
  Print sHold[siCounter] & " = " & sOutput                        'Print the coded and decoded result
  sOutput = ""                                                    'Clear sOutput
Next

End
```

Output:

```txt

broood = 1 17 15 0 0 5
bananaaa = 1 1 13 1 1 1 0 0
hiphophiphop = 7 8 15 2 15 2 2 3 2 2 3 2

1 17 15 0 0 5 = broood
1 1 13 1 1 1 0 0 = bananaaa
7 8 15 2 15 2 2 3 2 2 3 2 = hiphophiphop

```



## Go

{{trans|Python}}

```go
package main

import (
	"bytes"
	"fmt"
)

type symbolTable string

func (symbols symbolTable) encode(s string) []byte {
	seq := make([]byte, len(s))
	pad := []byte(symbols)
	for i, c := range []byte(s) {
		x := bytes.IndexByte(pad, c)
		seq[i] = byte(x)
		copy(pad[1:], pad[:x])
		pad[0] = c
	}
	return seq
}

func (symbols symbolTable) decode(seq []byte) string {
	chars := make([]byte, len(seq))
	pad := []byte(symbols)
	for i, x := range seq {
		c := pad[x]
		chars[i] = c
		copy(pad[1:], pad[:x])
		pad[0] = c
	}
	return string(chars)
}

func main() {
	m := symbolTable("abcdefghijklmnopqrstuvwxyz")
	for _, s := range []string{"broood", "bananaaa", "hiphophiphop"} {
		enc := m.encode(s)
		dec := m.decode(enc)
		fmt.Println(s, enc, dec)
		if dec != s {
			panic("Whoops!")
		}
	}
}
```

{{out}}

```txt

broood [1 17 15 0 0 5] broood
bananaaa [1 1 13 1 1 1 0 0] bananaaa
hiphophiphop [7 8 15 2 15 2 2 3 2 2 3 2] hiphophiphop

```



## Haskell


```Haskell
import Data.List (delete, elemIndex, mapAccumL)

import Data.Maybe (fromJust)

table :: String
table = ['a' .. 'z']

encode :: String -> [Int]
encode =
  let f t s = (s : delete s t, fromJust (elemIndex s t))
  in snd . mapAccumL f table

decode :: [Int] -> String
decode = snd . mapAccumL f table
  where
    f t i =
      let s = (t !! i)
      in (s : delete s t, s)

main :: IO ()
main =
  mapM_ print $
  (,) <*> uncurry ((==) . fst) <$> -- Test that ((fst . fst) x) == snd x)
  ((,) <*> (decode . snd) <$>
   ((,) <*> encode <$> ["broood", "bananaaa", "hiphophiphop"]))
```

{{out}}

```txt
((("broood",[1,17,15,0,0,5]),"broood"),True)
((("bananaaa",[1,1,13,1,1,1,0,0]),"bananaaa"),True)
((("hiphophiphop",[7,8,15,2,15,2,2,3,2,2,3,2]),"hiphophiphop"),True)
```


=={{header|Icon}} and {{header|Unicon}}==

Works in both languages:

```unicon
procedure main(A)
    every writes(s := !A, " -> [") do {
        every writes(!(enc := encode(&lcase,s))," ")
        writes("] -> ",s2 := decode(&lcase,enc))
        write((s == s2, " (Correct)") | " (Incorrect)")
        }
end

procedure encode(m,s)
    enc := []
    every c := !s do {
        m ?:= reorder(tab(i := upto(c)),move(1),tab(0))
        put(enc,i-1)   # Strings are 1-based
        }
    return enc
end

procedure decode(m,enc)
    dec := ""
    every i := 1 + !enc do {	# Lists are 1-based
        dec ||:= m[i]
        m ?:= reorder(tab(i),move(1),tab(0))
        }
    return dec
end

procedure reorder(s1,s2,s3)
    return s2||s1||s3
end
```


Sample run:

```txt

->mtfa broood bananaaa hiphophiphop
broood -> [1 17 15 0 0 5 ] -> broood (Correct)
bananaaa -> [1 1 13 1 1 1 0 0 ] -> bananaaa (Correct)
hiphophiphop -> [7 8 15 2 15 2 2 3 2 2 3 2 ] -> hiphophiphop (Correct)
->

```



## J



```J
spindizzy=:3 :0
  'seq table'=. y
  ndx=.$0
  orig=. table
  for_sym. seq do.
    ndx=.ndx,table i.sym
    table=. sym,table-.sym
  end.
  ndx
  assert. seq-:yzzidnips ndx;orig
)

yzzidnips=:3 :0
  'ndx table'=. y
  seq=.''
  for_n. ndx do.
    seq=.seq,sym=. n{table
    table=. sym,table-.sym
  end.
  seq
)
```


Required examples:


```J
   spindizzy 'broood';'abcdefghijklmnopqrstuvwxyz'
1 17 15 0 0 5
   spindizzy 'bananaaa';'abcdefghijklmnopqrstuvwxyz'
1 1 13 1 1 1 0 0
   spindizzy 'hiphophiphop';'abcdefghijklmnopqrstuvwxyz'
7 8 15 2 15 2 2 3 2 2 3 2
```


It's not clear, though, why anyone would think that this is any better than lookups against an unmodified symbol table.

## Java

{{works with|Java|1.5+}}

```java5
import java.util.LinkedList;
import java.util.List;

public class MTF{
	public static List<Integer> encode(String msg, String symTable){
		List<Integer> output = new LinkedList<Integer>();
		StringBuilder s = new StringBuilder(symTable);
		for(char c : msg.toCharArray()){
			int idx = s.indexOf("" + c);
			output.add(idx);
			s = s.deleteCharAt(idx).insert(0, c);
		}
		return output;
	}

	public static String decode(List<Integer> idxs, String symTable){
		StringBuilder output = new StringBuilder();
		StringBuilder s = new StringBuilder(symTable);
		for(int idx : idxs){
			char c = s.charAt(idx);
			output = output.append(c);
			s = s.deleteCharAt(idx).insert(0, c);
		}
		return output.toString();
	}

	private static void test(String toEncode, String symTable){
		List<Integer> encoded = encode(toEncode, symTable);
		System.out.println(toEncode + ": " + encoded);
		String decoded = decode(encoded, symTable);
		System.out.println((toEncode.equals(decoded) ? "" : "in") + "correctly decoded to " + decoded);
	}

	public static void main(String[] args){
		String symTable = "abcdefghijklmnopqrstuvwxyz";
		test("broood", symTable);
		test("bananaaa", symTable);
		test("hiphophiphop", symTable);
	}
}
```

{{out}}

```txt
broood: [1, 17, 15, 0, 0, 5]
correctly decoded to broood
bananaaa: [1, 1, 13, 1, 1, 1, 0, 0]
correctly decoded to bananaaa
hiphophiphop: [7, 8, 15, 2, 15, 2, 2, 3, 2, 2, 3, 2]
correctly decoded to hiphophiphop
```



## JavaScript


```javascript
var encodeMTF = function (word) {
  var init = {wordAsNumbers: [], charList: 'abcdefghijklmnopqrstuvwxyz'.split('')};

  return word.split('').reduce(function (acc, char) {
    var charNum = acc.charList.indexOf(char); //get index of char
    acc.wordAsNumbers.push(charNum); //add original index to acc
    acc.charList.unshift(acc.charList.splice(charNum, 1)[0]); //put at beginning of list
    return acc;
  }, init).wordAsNumbers; //return number list
};

var decodeMTF = function (numList) {
  var init = {word: '', charList: 'abcdefghijklmnopqrstuvwxyz'.split('')};

  return numList.reduce(function (acc, num) {
    acc.word += acc.charList[num];
    acc.charList.unshift(acc.charList.splice(num, 1)[0]); //put at beginning of list
    return acc;
  }, init).word;
};

//test our algorithms
var words = ['broood', 'bananaaa', 'hiphophiphop'];
var encoded = words.map(encodeMTF);
var decoded = encoded.map(decodeMTF);

//print results
console.log("from encoded:");
console.log(encoded);
console.log("from decoded:");
console.log(decoded);
```

{{out}}

```txt
from encoded:
[
  [1, 17, 15, 0, 0, 5],
  [1, 1, 13, 1, 1, 1, 0, 0],
  [7, 8, 15, 2, 15, 2, 2, 3, 2, 2, 3, 2]
]
from decoded:
['broood', 'bananaaa', 'hiphophiphop']
```



## jq

{{works with|jq|q.4}}

```jq
# Input is the string to be encoded, st is the initial symbol table (an array)
# Output: the encoded string (an array)
def m2f_encode(st):
  reduce explode[] as $ch
    ( [ [], st];                  # state: [ans, st]
      (.[1]|index($ch)) as $ix
      | .[1] as $st
      | [ (.[0] + [ $ix ]),  [$st[$ix]] + $st[0:$ix] + $st[$ix+1:] ] )
  | .[0];

# Input should be the encoded string (an array)
# and st should be the initial symbol table (an array)
def m2f_decode(st):
  reduce .[] as $ix
    ( [ [], st];                  # state: [ans, st]
      .[1] as $st
      | [ (.[0] + [ $st[$ix] ]),  [$st[$ix]] + $st[0:$ix] + $st[$ix+1:] ] )
  | .[0]
  | implode;
```

'''Example''':

```jq
("abcdefghijklmnopqrstuvwxyz" | explode) as $ST
  | ("broood", "bananaaa", "hiphophiphop")
  | . as $string
  | m2f_encode($ST)
  | . as $encoded
  | m2f_decode($ST) as $decoded
  | if $string == $decoded then "\($string) => \($encoded) => \($decoded)"
    else "INTERNAL ERROR: encoding of \($string) => \($encoded) => \($decoded)"
    end
```

{{Out}}

```sh
$ jq -r -n -f move_to_front.jq
broood => [1,17,15,0,0,5] => broood
bananaaa => [1,1,13,1,1,1,0,0] => bananaaa
hiphophiphop => [7,8,15,2,15,2,2,3,2,2,3,2] => hiphophiphop
```



## Julia

{{works with|Julia|0.6}}

```julia
function encodeMTF(str::AbstractString, symtable::Vector{Char}=collect('a':'z'))
    function encode(ch::Char)
        r = findfirst(symtable, ch)
        deleteat!(symtable, r)
        prepend!(symtable, ch)
        return r
    end
    collect(encode(ch) for ch in str)
end

function decodeMTF(arr::Vector{Int}, symtable::Vector{Char}=collect('a':'z'))
    function decode(i::Int)
        r = symtable[i]
        deleteat!(symtable, i)
        prepend!(symtable, r)
        return r
    end
    join(decode(i) for i in arr)
end

testset = ["broood", "bananaaa", "hiphophiphop"]
encoded = encodeMTF.(testset)
decoded = decodeMTF.(encoded)
for (str, enc, dec) in zip(testset, encoded, decoded)
    println("Test string: $str\n -> Encoded: $enc\n -> Decoded: $dec")
end

using Base.Test
@testset "Decoded string equal to original" begin
    for (str, dec) in zip(testset, decoded)
        @test str == dec
    end
end
```


{{out}}

```txt
Test string: broood
 -> Encoded: [2, 18, 16, 1, 1, 6]
 -> Decoded: broood
Test string: bananaaa
 -> Encoded: [2, 2, 14, 2, 2, 2, 1, 1]
 -> Decoded: bananaaa
Test string: hiphophiphop
 -> Encoded: [8, 9, 16, 3, 16, 3, 3, 4, 3, 3, 4, 3]
 -> Decoded: hiphophiphop
Test Summary:                    | Pass  Total
Decoded string equal to original |    3      3
```



## Kotlin


```scala
// version 1.1.2

fun encode(s: String): IntArray {
    if (s.isEmpty()) return intArrayOf()
    val symbols = "abcdefghijklmnopqrstuvwxyz".toCharArray()
    val result = IntArray(s.length)
    for ((i, c) in s.withIndex()) {
        val index = symbols.indexOf(c)
        if (index == -1)
            throw IllegalArgumentException("$s contains a non-alphabetic character")
        result[i] = index
        if (index == 0) continue
        for (j in index - 1 downTo 0) symbols[j + 1] = symbols[j]
        symbols[0] = c
    }
    return result
}

fun decode(a: IntArray): String {
    if (a.isEmpty()) return ""
    val symbols = "abcdefghijklmnopqrstuvwxyz".toCharArray()
    val result = CharArray(a.size)
    for ((i, n) in a.withIndex()) {
        if (n !in 0..25)
            throw IllegalArgumentException("${a.contentToString()} contains an invalid number")
        result[i] = symbols[n]
        if (n == 0) continue
        for (j in n - 1 downTo 0) symbols[j + 1] = symbols[j]
        symbols[0] = result[i]
    }
    return result.joinToString("")
}

fun main(args: Array<String>) {
    val strings = arrayOf("broood", "bananaaa", "hiphophiphop")
    val encoded = Array<IntArray?>(strings.size) { null }
    for ((i, s) in strings.withIndex()) {
        encoded[i] = encode(s)
        println("${s.padEnd(12)} -> ${encoded[i]!!.contentToString()}")
    }
    println()
    val decoded = Array<String?>(encoded.size) { null }
    for ((i, a) in encoded.withIndex()) {
        decoded[i] = decode(a!!)
        print("${a.contentToString().padEnd(38)} -> ${decoded[i]!!.padEnd(12)}")
        println(" -> ${if (decoded[i] == strings[i]) "correct" else "incorrect"}")
    }
}
```


{{out}}

```txt

broood       -> [1, 17, 15, 0, 0, 5]
bananaaa     -> [1, 1, 13, 1, 1, 1, 0, 0]
hiphophiphop -> [7, 8, 15, 2, 15, 2, 2, 3, 2, 2, 3, 2]

[1, 17, 15, 0, 0, 5]                   -> broood       -> correct
[1, 1, 13, 1, 1, 1, 0, 0]              -> bananaaa     -> correct
[7, 8, 15, 2, 15, 2, 2, 3, 2, 2, 3, 2] -> hiphophiphop -> correct

```



## Lua


```lua
-- Return table of the alphabet in lower case
function getAlphabet ()
    local letters = {}
    for ascii = 97, 122 do table.insert(letters, string.char(ascii)) end
    return letters
end

-- Move the table value at ind to the front of tab
function moveToFront (tab, ind)
    local toMove = tab[ind]
    for i = ind - 1, 1, -1 do tab[i + 1] = tab[i] end
    tab[1] = toMove
end

-- Perform move-to-front encoding on input
function encode (input)
    local symbolTable, output, index = getAlphabet(), {}
    for pos = 1, #input do
        for k, v in pairs(symbolTable) do
            if v == input:sub(pos, pos) then index = k end
        end
        moveToFront(symbolTable, index)
        table.insert(output, index - 1)
    end
    return table.concat(output, " ")
end

-- Perform move-to-front decoding on input
function decode (input)
    local symbolTable, output = getAlphabet(), ""
    for num in input:gmatch("%d+") do
        output = output .. symbolTable[num + 1]
        moveToFront(symbolTable, num + 1)
    end
    return output
end

-- Main procedure
local testCases, output = {"broood", "bananaaa", "hiphophiphop"}
for _, case in pairs(testCases) do
    output = encode(case)
    print("Original string: " .. case)
    print("Encoded: " .. output)
    print("Decoded: " .. decode(output))
    print()
end
```

{{out}}

```txt
Original string: broood
Encoded: 1 17 15 0 0 5
Decoded: broood

Original string: bananaaa
Encoded: 1 1 13 1 1 1 0 0
Decoded: bananaaa

Original string: hiphophiphop
Encoded: 7 8 15 2 15 2 2 3 2 2 3 2
Decoded: hiphophiphop
```


edit

## M2000 Interpreter

In Encode$() we use a string of parameters (function Quote$() make this)

Function Param() get the string with parameters and place it in code (inline).

In Decode$ we use current stack (first we flush to ensure that isn't some other arguments on it, M2000 has no signature control over the call of user functions, we can place anything)

For deleting and inserting characters in a string we use Insert. Insert of "" in a place of 1 char means delete it. So Insert k, 1 SymbolTable$="" means that in k position, for one char we place empty string.

To extract information to clipboard we use global variables, is handy;

Number pop a number from stack of values, if no number found then raise error.


```M2000 Interpreter

Module CheckIt {
      Global All$, nl$
      \\ upgrade to document
      Document All$
      Nl$<={
      }
      Function Encode$(Inp$) {
           Def SymbolTable$="abcdefghijklmnopqrstuvwxyz", Out$=""
            For i=1 to Len(Inp$)
                  c$=Mid$(Inp$, i, 1)
                  k=Instr(SymbolTable$, c$)
                  Insert k, 1 SymbolTable$=""
                  Out$=If$(Out$="" -> Quote$(k-1),Quote$(Param(Out$), k-1))
                  Insert 1 SymbolTable$=c$
                  \\ we use <= for globals
                  All$<=Format$("   {0} {1:30} {2}", c$, Out$, SymbolTable$)+nl$
            Next i
            =Out$
      }
      Function Decode$(Inp$) {
           Def SymbolTable$="abcdefghijklmnopqrstuvwxyz", Out$=""
           flush
           Data Param(Inp$)
           While not empty {
                  k=Number+1
                  c$=Mid$(SymbolTable$, k, 1)
                  Out$+=c$
                  Insert k, 1 SymbolTable$=""  ' erase at position k
                  Insert 1 SymbolTable$=c$
                  All$<=Format$("{0::-2} {1} {2:30} {3}", k-1, c$, Out$, SymbolTable$)+nl$
            }
            =Out$
      }
      TryThis("broood")
      TryThis("bananaaa")
      TryThis("hiphophiphop")
      ClipBoard All$
      Sub TryThis(a$)
            Local Out$=Encode$(a$)
            Local final$=Decode$(Out$)
            Print final$, final$=a$
      End Sub
}
CheckIt

```


{{out}}
<pre style="height:30ex;overflow:scroll">
broood       True
bananaaa     True
hiphophiphop True

```


From clipboard
<pre style="height:30ex;overflow:scroll">
   b 1                              bacdefghijklmnopqrstuvwxyz
   r 1,17                           rbacdefghijklmnopqstuvwxyz
   o 1,17,15                        orbacdefghijklmnpqstuvwxyz
   o 1,17,15,0                      orbacdefghijklmnpqstuvwxyz
   o 1,17,15,0,0                    orbacdefghijklmnpqstuvwxyz
   d 1,17,15,0,0,5                  dorbacefghijklmnpqstuvwxyz
 1 b b                              bacdefghijklmnopqrstuvwxyz
17 r br                             rbacdefghijklmnopqstuvwxyz
15 o bro                            orbacdefghijklmnpqstuvwxyz
 0 o broo                           orbacdefghijklmnpqstuvwxyz
 0 o brooo                          orbacdefghijklmnpqstuvwxyz
 5 d broood                         dorbacefghijklmnpqstuvwxyz
   b 1                              bacdefghijklmnopqrstuvwxyz
   a 1,1                            abcdefghijklmnopqrstuvwxyz
   n 1,1,13                         nabcdefghijklmopqrstuvwxyz
   a 1,1,13,1                       anbcdefghijklmopqrstuvwxyz
   n 1,1,13,1,1                     nabcdefghijklmopqrstuvwxyz
   a 1,1,13,1,1,1                   anbcdefghijklmopqrstuvwxyz
   a 1,1,13,1,1,1,0                 anbcdefghijklmopqrstuvwxyz
   a 1,1,13,1,1,1,0,0               anbcdefghijklmopqrstuvwxyz
 1 b b                              bacdefghijklmnopqrstuvwxyz
 1 a ba                             abcdefghijklmnopqrstuvwxyz
13 n ban                            nabcdefghijklmopqrstuvwxyz
 1 a bana                           anbcdefghijklmopqrstuvwxyz
 1 n banan                          nabcdefghijklmopqrstuvwxyz
 1 a banana                         anbcdefghijklmopqrstuvwxyz
 0 a bananaa                        anbcdefghijklmopqrstuvwxyz
 0 a bananaaa                       anbcdefghijklmopqrstuvwxyz
   h 7                              habcdefgijklmnopqrstuvwxyz
   i 7,8                            ihabcdefgjklmnopqrstuvwxyz
   p 7,8,15                         pihabcdefgjklmnoqrstuvwxyz
   h 7,8,15,2                       hpiabcdefgjklmnoqrstuvwxyz
   o 7,8,15,2,15                    ohpiabcdefgjklmnqrstuvwxyz
   p 7,8,15,2,15,2                  pohiabcdefgjklmnqrstuvwxyz
   h 7,8,15,2,15,2,2                hpoiabcdefgjklmnqrstuvwxyz
   i 7,8,15,2,15,2,2,3              ihpoabcdefgjklmnqrstuvwxyz
   p 7,8,15,2,15,2,2,3,2            pihoabcdefgjklmnqrstuvwxyz
   h 7,8,15,2,15,2,2,3,2,2          hpioabcdefgjklmnqrstuvwxyz
   o 7,8,15,2,15,2,2,3,2,2,3        ohpiabcdefgjklmnqrstuvwxyz
   p 7,8,15,2,15,2,2,3,2,2,3,2      pohiabcdefgjklmnqrstuvwxyz
 7 h h                              habcdefgijklmnopqrstuvwxyz
 8 i hi                             ihabcdefgjklmnopqrstuvwxyz
15 p hip                            pihabcdefgjklmnoqrstuvwxyz
 2 h hiph                           hpiabcdefgjklmnoqrstuvwxyz
15 o hipho                          ohpiabcdefgjklmnqrstuvwxyz
 2 p hiphop                         pohiabcdefgjklmnqrstuvwxyz
 2 h hiphoph                        hpoiabcdefgjklmnqrstuvwxyz
 3 i hiphophi                       ihpoabcdefgjklmnqrstuvwxyz
 2 p hiphophip                      pihoabcdefgjklmnqrstuvwxyz
 2 h hiphophiph                     hpioabcdefgjklmnqrstuvwxyz
 3 o hiphophipho                    ohpiabcdefgjklmnqrstuvwxyz
 2 p hiphophiphop                   pohiabcdefgjklmnqrstuvwxyz
</pre >


## Mathematica



```Mathematica
mtf[word_]:=Module[{f,f2,p,q},
   f[{output_,symList_},next_]:=Module[{index},index=Position[symList,next][[1,1]]-1;
       {output~Append~index,Prepend[Delete[symList,index+1],next]}];
   p=Fold[f,{{},CharacterRange["a","z"]},Characters[ToString[word]]][[1]];
   f2[{output_,symList_},next_]:=Module[{index},index=symList[[next+1]];
       {output~Append~index,Prepend[DeleteCases[symList,ToString[index]],index]}];
   q=Fold[f2,{{},CharacterRange["a","z"]},p][[1]];
   Print["'", word,"' encodes to: ",p, " - "  ,p," decodes to: '",StringJoin@q,"' - Input equals Output: " ,word===StringJoin@q];]
```

Testing out the function:

```Mathematica
mtf /@ {"broood", "bananaaa", "hiphophiphop"}
```

{{out}}

```txt
'broood' encodes to: {1,17,15,0,0,5} - {1,17,15,0,0,5} decodes to: 'broood' - Input equals Output: True
'bananaaa' encodes to: {1,1,13,1,1,1,0,0} - {1,1,13,1,1,1,0,0} decodes to: 'bananaaa' - Input equals Output: True
'hiphophiphop' encodes to: {7,8,15,2,15,2,2,3,2,2,3,2} - {7,8,15,2,15,2,2,3,2,2,3,2} decodes to: 'hiphophiphop' - Input equals Output: True
```



## MATLAB


```MATLAB
function testMTF
    symTable = 'abcdefghijklmnopqrstuvwxyz';
    inStr = {'broood' 'bananaaa' 'hiphophiphop'};
    for k = 1:length(inStr)
        outArr = encodeMTF(inStr{k}, symTable);
        outStr = decodeMTF(outArr, symTable);
        fprintf('%s: [ %s]\n', inStr{k}, sprintf('%d ', outArr))
        fprintf('%scorrectly decoded to %s\n', char('in'.*~strcmp(outStr, inStr{k})), outStr)
    end
end

function arr = encodeMTF(str, symTable)
    n = length(str);
    arr = zeros(1, n);
    for k = 1:n
        arr(k) = find(str(k) == symTable, 1);
        symTable = [symTable(arr(k)) symTable(1:arr(k)-1) symTable(arr(k)+1:end)];
    end
    arr = arr-1; % Change to zero-indexed array
end

function str = decodeMTF(arr, symTable)
    arr = arr+1; % Change to one-indexed array
    n = length(arr);
    str = char(zeros(1, n));
    for k = 1:n
        str(k) = symTable(arr(k));
        symTable = [symTable(arr(k)) symTable(1:arr(k)-1) symTable(arr(k)+1:end)];
    end
end
```

{{out}}

```txt
broood: [ 1 17 15 0 0 5 ]
correctly decoded to broood
bananaaa: [ 1 1 13 1 1 1 0 0 ]
correctly decoded to bananaaa
hiphophiphop: [ 7 8 15 2 15 2 2 3 2 2 3 2 ]
correctly decoded to hiphophiphop
```



## Perl


```perl
use strict;
use warnings;
sub encode {
	my ($str) = @_;
	my $table = join '', 'a' .. 'z';
	map {
		$table =~ s/(.*?)$_/$_$1/ or die;
		length($1);
	} split //, $str;
}

sub decode {
	my $table = join '', 'a' .. 'z';
	join "", map {
		$table =~ s/(.{$_})(.)/$2$1/ or die;
		$2;
	} @_;
}

for my $test ( qw(broood bananaaa hiphophiphop) ) {
	my @encoded = encode($test);
	print "$test: @encoded\n";
	my $decoded = decode(@encoded);
	print "in" x ( $decoded ne $test );
	print "correctly decoded to $decoded\n";
}

```

{{out}}

```txt
broood: 1 17 15 0 0 5
correctly decoded to broood
bananaaa: 1 1 13 1 1 1 0 0
correctly decoded to bananaaa
hiphophiphop: 7 8 15 2 15 2 2 3 2 2 3 2
correctly decoded to hiphophiphop

```


## Perl 6

{{works with|rakudo|2015-09-24}}

```perl6
sub encode ( Str $word ) {
    my @sym = 'a' .. 'z';
    gather for $word.comb -> $c {
	die "Symbol '$c' not found in @sym" if $c eq @sym.none;
        @sym[0 .. take (@sym ... $c).end] .= rotate(-1);
    }
}

sub decode ( @enc ) {
    my @sym = 'a' .. 'z';
    [~] gather for @enc -> $pos {
        take @sym[$pos];
        @sym[0..$pos] .= rotate(-1);
    }
}

use Test;
plan 3;
for <broood bananaaa hiphophiphop> -> $word {
    my $enc = encode($word);
    my $dec = decode($enc);
    is $word, $dec, "$word.fmt('%-12s') ($enc[])";
}
```

{{out}}

```txt
1..3
ok 1 - broood       (1 17 15 0 0 5)
ok 2 - bananaaa     (1 1 13 1 1 1 0 0)
ok 3 - hiphophiphop (7 8 15 2 15 2 2 3 2 2 3 2)
```



## Phix


```Phix
function encode(string s)
    string symtab = "abcdefghijklmnopqrstuvwxyz"
    sequence res = {}
    for i=1 to length(s) do
        integer ch = s[i]
        integer k = find(ch,symtab)
        res &= k-1
        for j=k to 2 by -1 do
            symtab[j] = symtab[j-1]
        end for
        symtab[1] = ch
    end for
    return res
end function

function decode(sequence s)
    string symtab = "abcdefghijklmnopqrstuvwxyz"
    string res = ""
    for i=1 to length(s) do
        integer k = s[i]+1
        integer ch = symtab[k]
        res &= ch
        for j=k to 2 by -1 do
            symtab[j] = symtab[j-1]
        end for
        symtab[1] = ch
    end for
    return res
end function

procedure test(string s)
    sequence e = encode(s)
    string d = decode(e)
    ?{s,e,d,{"**ERROR**","ok"}[(s=d)+1]}
end procedure
test("broood")
test("bananaaa")
test("hiphophiphop")
```

{{out}}

```txt

{"broood",{1,17,15,0,0,5},"broood","ok"}
{"bananaaa",{1,1,13,1,1,1,0,0},"bananaaa","ok"}
{"hiphophiphop",{7,8,15,2,15,2,2,3,2,2,3,2},"hiphophiphop","ok"}

```



## PicoLisp


```PicoLisp
(de encode (Str)
   (let Table (chop "abcdefghijklmnopqrstuvwxyz")
      (mapcar
         '((C)
            (dec
               (prog1
                  (index C Table)
                  (rot Table @) ) ) )
         (chop Str) ) ) )

(de decode (Lst)
   (let Table (chop "abcdefghijklmnopqrstuvwxyz")
      (pack
         (mapcar
            '((N)
               (prog1
                  (get Table (inc 'N))
                  (rot Table N) ) )
            Lst ) ) ) )
```

Test:

```PicoLisp
(test (1 17 15 0 0 5)
   (encode "broood") )
(test "broood"
   (decode (1 17 15 0 0 5)) )

(test (1 1 13 1 1 1 0 0)
   (encode "bananaaa") )
(test "bananaaa"
   (decode (1 1 13 1 1 1 0 0)) )

(test (7 8 15 2 15 2 2 3 2 2 3 2)
   (encode "hiphophiphop") )
(test "hiphophiphop"
   (decode (7 8 15 2 15 2 2 3 2 2 3 2)) )
```



## PL/I


```pli
*process source attributes xref or(!);
 /*********************************************************************
 * 25.5.2014 Walter Pachl translated from REXX
 *********************************************************************/
 ed: Proc Options(main);
 Call enc_dec('broood');
 Call enc_dec('bananaaa');
 Call enc_dec('hiphophiphop');

 enc_dec: Proc(in);
 Dcl in  Char(*);
 Dcl out Char(20) Var Init('');
 Dcl st Char(26) Init('abcdefghijklmnopqrstuvwxyz');
 Dcl sta Char(26) Init((st));
 Dcl enc(20) Bin Fixed(31);
 Dcl encn Bin Fixed(31) Init(0);
 Dcl (i,p.k) Bin Fixed(31);
 Dcl c       Char(1);
 Do i=1 To length(in);
   c=substr(in,i,1);
   p=index(st,c);
   encn+=1;
   enc(encn)=p-1;
   st=c!!left(st,p-1)!!substr(st,p+1);
   End;
 Put Skip List(' in='!!in);
 Put Skip List('sta='!!sta!!' original symbol table');
 Put Skip Edit('enc=',(enc(i) do i=1 To encn))(a,20(F(3)));
 Put Skip List(' st='!!st!!' symbol table after encoding');
 Do i=1 To encn;
   k=enc(i)+1;
   out=out!!substr(sta,k,1);
   sta=substr(sta,k,1)!!left(sta,k-1)!!substr(sta,k+1);
   End;
 Put Skip List('out='!!out);
 Put Skip List(' ');
 If out=in Then;
 Else
   Put Skip List('all wrong!!');
 End;
```

{{out}}

```txt
 in=broood
sta=abcdefghijklmnopqrstuvwxyz original symbol table
enc=  1 17 15  0  0  5
 st=dorbacefghijklmnpqstuvwxyz symbol table after encoding
out=broood

 in=bananaaa
sta=abcdefghijklmnopqrstuvwxyz original symbol table
enc=  1  1 13  1  1  1  0  0
 st=anbcdefghijklmopqrstuvwxyz symbol table after encoding
out=bananaaa

 in=hiphophiphop
sta=abcdefghijklmnopqrstuvwxyz original symbol table
enc=  7  8 15  2 15  2  2  3  2  2  3  2
 st=pohiabcdefgjklmnqrstuvwxyz symbol table after encoding
out=hiphophiphop
```



## PowerShell


```PowerShell
Function Test-MTF
{
    [CmdletBinding()]
    Param
    (
        [Parameter(Mandatory=$true,Position=0)]
        [string]$word,

        [Parameter(Mandatory=$false)]
        [string]$SymbolTable = 'abcdefghijklmnopqrstuvwxyz'
    )
    Begin
    {
        Function Encode
        {
            Param
            (
                [Parameter(Mandatory=$true,Position=0)]
                [string]$word,

                [Parameter(Mandatory=$false)]
                [string]$SymbolTable = 'abcdefghijklmnopqrstuvwxyz'
            )
            foreach ($letter in $word.ToCharArray())
            {
                $index = $SymbolTable.IndexOf($letter)

                $prop = [ordered]@{
                    Input = $letter
                    Output = [int]$index
                    SymbolTable = $SymbolTable
                }
                New-Object PSobject -Property $prop
                $SymbolTable = $SymbolTable.Remove($index,1).Insert(0,$letter)
            }
        }
        Function Decode
        {
            Param
            (
                [Parameter(Mandatory=$true,Position=0)]
                [int[]]$index,

                [Parameter(Mandatory=$false)]
                [string]$SymbolTable = 'abcdefghijklmnopqrstuvwxyz'
            )
            foreach ($i in $index)
            {
                #Write-host $i -ForegroundColor Red
                $letter = $SymbolTable.Chars($i)

                $prop = [ordered]@{
                    Input = $i
                    Output = $letter
                    SymbolTable = $SymbolTable
                }
                New-Object PSObject -Property $prop
                $SymbolTable = $SymbolTable.Remove($i,1).Insert(0,$letter)
            }
        }
    }
    Process
    {
        #Encoding
        Write-Host "Encoding $word" -NoNewline
        $Encoded = (Encode -word $word).output
        Write-Host -NoNewline ": $($Encoded -join ',')"

        #Decoding
        Write-Host "`nDecoding $($Encoded -join ',')" -NoNewline
        $Decoded = (Decode -index $Encoded).output -join ''
        Write-Host -NoNewline ": $Decoded`n"
    }
    End{}
}
```


{{out}}

```txt
Test-MTF -word broood
Test-MTF -word bananaaa
Test-MTF -word hiphophiphop
Encoding broood: 1,17,15,0,0,5
Decoding 1,17,15,0,0,5: broood
Encoding bananaaa: 1,1,13,1,1,1,0,0
Decoding 1,1,13,1,1,1,0,0: bananaaa
Encoding hiphophiphop: 7,8,15,2,15,2,2,3,2,2,3,2
Decoding 7,8,15,2,15,2,2,3,2,2,3,2: hiphophiphop
```



## Python



### Python: Procedural


```python
from __future__ import print_function
from string import ascii_lowercase

SYMBOLTABLE = list(ascii_lowercase)

def move2front_encode(strng, symboltable):
    sequence, pad = [], symboltable[::]
    for char in strng:
        indx = pad.index(char)
        sequence.append(indx)
        pad = [pad.pop(indx)] + pad
    return sequence

def move2front_decode(sequence, symboltable):
    chars, pad = [], symboltable[::]
    for indx in sequence:
        char = pad[indx]
        chars.append(char)
        pad = [pad.pop(indx)] + pad
    return ''.join(chars)

if __name__ == '__main__':
    for s in ['broood', 'bananaaa', 'hiphophiphop']:
        encode = move2front_encode(s, SYMBOLTABLE)
        print('%14r encodes to %r' % (s, encode), end=', ')
        decode = move2front_decode(encode, SYMBOLTABLE)
        print('which decodes back to %r' % decode)
        assert s == decode, 'Whoops!'
```


{{out}}

```txt
      'broood' encodes to [1, 17, 15, 0, 0, 5], which decodes back to 'broood'
    'bananaaa' encodes to [1, 1, 13, 1, 1, 1, 0, 0], which decodes back to 'bananaaa'
'hiphophiphop' encodes to [7, 8, 15, 2, 15, 2, 2, 3, 2, 2, 3, 2], which decodes back to 'hiphophiphop'
```



### Python: Functional

From the procedural version note that through the encoding or decoding loops both the final output is accumulated as well as the symbol table being rearranged.

For the functional forms a 2-item list of output to be accumulated and symboltable manipulation is calculated then only the former accumulated as the later works to transform the symbol table in-place.


```python
def m2f_e(s, st):
    return [[st.index(ch), st.insert(0, st.pop(st.index(ch)))][0] for ch in s]

def m2f_d(sq, st):
    return ''.join([st[i], st.insert(0, st.pop(i))][0] for i in sq)

ST = list('abcdefghijklmnopqrstuvwxyz')
for s in ['broood', 'bananaaa', 'hiphophiphop']:
    encode = m2f_e(s, ST[::])
    print('%14r encodes to %r' % (s, encode), end=', ')
    decode = m2f_d(encode, ST[::])
    print('decodes back to %r' % decode)
    assert s == decode, 'Whoops!'
```


{{out}}
Similar to that of the procedural version above.


## Racket


```racket
#lang racket
(define default-symtab "abcdefghijklmnopqrstuvwxyz")

(define (move-to-front:encode in (symtab default-symtab))
  (define inner-encode
    (match-lambda**
     [((? string? (app string->list in)) st acc) ; make input all listy
      (inner-encode in st acc)]
     [(in (? string? (app string->list st)) acc) ; make symtab all listy
      (inner-encode in st acc)]
     [((list) _ (app reverse rv)) ; nothing more to encode
      rv]
     [((list a tail ...) (list left ... a right ...) acc) ; encode and recur
      (inner-encode tail `(,a ,@left ,@right) (cons (length left) acc))]))
  (inner-encode in symtab null))

(define (move-to-front:decode in (symtab default-symtab))
  (define inner-decode
    (match-lambda**
     [(in (? string? (app string->list st)) acc) ; make symtab all listy
      (inner-decode in st acc)]
     [((list) _ (app (compose list->string reverse) rv)) ; nothing more to encode
      rv]
     [((list a tail ...) symbols acc) ; decode and recur
      (match/values
       (split-at symbols a)
       [(l (cons ra rd))
        (inner-decode tail (cons ra (append l rd)) (cons ra acc))])]))
  (inner-decode in symtab null))

(module+ test
  ;; Test against the example in the task
  (require rackunit)
  (check-equal? (move-to-front:encode "broood") '(1 17 15 0 0 5))
  (check-equal? (move-to-front:decode '(1 17 15 0 0 5)) "broood")
  (check-equal? (move-to-front:decode (move-to-front:encode "broood")) "broood"))

(module+ main
  (define (encode+decode-string str)
    (define enc (move-to-front:encode str))
    (define dec (move-to-front:decode enc))
    (define crt (if (equal? dec str) "correctly" "incorrectly"))
    (printf "~s encodes to ~s, which decodes ~s to ~s.~%" str enc crt dec))

  (for-each encode+decode-string '("broood" "bananaaa" "hiphophiphop")))
```


{{out}}

```txt
"broood" encodes to (1 17 15 0 0 5), which decodes "correctly" to "broood".
"bananaaa" encodes to (1 1 13 1 1 1 0 0), which decodes "correctly" to "bananaaa".
"hiphophiphop" encodes to (7 8 15 2 15 2 2 3 2 2 3 2), which decodes "correctly" to "hiphophiphop".
```



## REXX


### version 1


```rexx
/* REXX ***************************************************************
* 25.05.2014 Walter Pachl
* REXX strings start with position 1
**********************************************************************/
Call enc_dec 'broood'
Call enc_dec 'bananaaa'
Call enc_dec 'hiphophiphop'
Exit
enc_dec: Procedure
Parse Arg in
st='abcdefghijklmnopqrstuvwxyz'
sta=st /* remember this for decoding */
enc=''
Do i=1 To length(in)
  c=substr(in,i,1)
  p=pos(c,st)
  enc=enc (p-1)
  st=c||left(st,p-1)substr(st,p+1)
  End
Say ' in='in
Say 'sta='sta 'original symbol table'
Say 'enc='enc
Say ' st='st  'symbol table after encoding'
out=''
Do i=1 To words(enc)
  k=word(enc,i)+1
  out=out||substr(sta,k,1)
  sta=substr(sta,k,1)left(sta,k-1)substr(sta,k+1)
  End
Say 'out='out
Say ' '
If out==in Then Nop
Else
  Say 'all wrong!!'
Return

```

{{out}}

```txt
 in=broood
sta=abcdefghijklmnopqrstuvwxyz original symbol table
enc= 1 17 15 0 0 5
 st=dorbacefghijklmnpqstuvwxyz symbol table after encoding
out=broood

 in=bananaaa
sta=abcdefghijklmnopqrstuvwxyz original symbol table
enc= 1 1 13 1 1 1 0 0
 st=anbcdefghijklmopqrstuvwxyz symbol table after encoding
out=bananaaa

 in=hiphophiphop
sta=abcdefghijklmnopqrstuvwxyz original symbol table
enc= 7 8 15 2 15 2 2 3 2 2 3 2
 st=pohiabcdefgjklmnqrstuvwxyz symbol table after encoding
out=hiphophiphop
```



### version 2

Programming note:   the two REXX statements that add/subtract   <big> '''one''' </big>  deal with the task's requirement that the symbol table be   ''zero-indexed''   (the REXX language uses   ''unity-based''   strings).

```rexx
/*REXX program demonstrates the  move─to─front  algorithm  encode/decode  symbol table. */
parse arg xxx;   if xxx=''  then xxx= 'broood bananaaa hiphophiphop'  /*use the default?*/
                 one=1                           /*(offset) for this task's requirement.*/
  do j=1  for words(xxx);     x=word(xxx, j)     /*process a single word at a time.     */
  @= 'abcdefghijklmnopqrstuvwxyz';      @@=@     /*symbol table: the lowercase alphabet */
  $=                                             /*set the decode string to a   null.   */
      do k=1  for length(x);  z=substr(x, k, 1)  /*encrypt a symbol in the word.        */
      _=pos(z, @);        if _==0  then iterate  /*the symbol position in symbol table. */
      $=$  (_ - one);     @=z || delstr(@, _, 1) /*(re─)adjust the symbol table string. */
      end   /*k*/                                /* [↑]   the  move─to─front  encoding. */
  !=                                             /*set the encode string to a   null.   */
      do m=1  for words($);   n=word($, m) + one /*decode the sequence table string.    */
      y=substr(@@, n, 1);     !=! || y           /*the decode symbol for the   N   word.*/
      @@=y || delstr(@@, n, 1)                   /*rebuild the symbol table string.     */
      end   /*m*/                                /* [↑]   the  move─to─front  decoding. */
  say '  word: '   left(x, 20)   "encoding:"   left($, 35)   word('wrong OK', 1 + (!==x) )
  end       /*j*/                                /*stick a fork in it,  we're all done. */
```

{{out|output|text=  when using the default input:}}

```txt

  word:  broood               encoding:  1 17 15 0 0 5                      OK
  word:  bananaaa             encoding:  1 1 13 1 1 1 0 0                   OK
  word:  hiphophiphop         encoding:  7 8 15 2 15 2 2 3 2 2 3 2          OK

```



## Ring


```ring

# Project : Move-to-front algorithm

test("broood")
test("bananaaa")
test("hiphophiphop")

func encode(s)
        symtab = "abcdefghijklmnopqrstuvwxyz"
        res = ""
        for i=1 to len(s)
             ch = s[i]
             k = substr(symtab, ch)
             res = res + " " + (k-1)
             for j=k to 2 step -1
                  symtab[j] = symtab[j-1]
             next
             symtab[1] = ch
        next
        return res

func decode(s)
        s = str2list( substr(s, " ", nl) )
        symtab = "abcdefghijklmnopqrstuvwxyz"
        res = ""
        for i=1 to len(s)
             k = number(s[i]) + 1
             ch = symtab[k]
             res = res + " " + ch
             for j=k to 2 step -1
                   symtab[j] = symtab[j-1]
             next
             symtab[1] = ch
        next
        return right(res, len(res)-2)

func test(s)
        e = encode(s)
        d = decode(e)
        see "" + s + " => " + "(" + right(e, len(e) - 1) + ") " + " => " + substr(d, " ", "") + nl

```

Output:

```txt

broood => (1 17 15 0 0 5)  => broood
bananaaa => (1 1 13 1 1 1 0 0)  => bananaaa
hiphophiphop => (7 8 15 2 15 2 2 3 2 2 3 2)  => hiphophiphop

```



## Ruby

Use a module as namespace:

```ruby
module MoveToFront

  ABC = ("a".."z").to_a.freeze

  def self.encode(str)
    ar = ABC.dup
    str.chars.each_with_object([]) do |char, memo|
      memo << (i = ar.index(char))
      ar = m2f(ar,i)
    end
  end

  def self.decode(indices)
    ar = ABC.dup
    indices.each_with_object("") do |i, str|
      str << ar[i]
      ar = m2f(ar,i)
    end
  end

  private
  def self.m2f(ar,i)
    [ar.delete_at(i)] + ar
  end

end

['broood', 'bananaaa', 'hiphophiphop'].each do |word|
  p word == MoveToFront.decode(p MoveToFront.encode(p word))
end
```


{{out}}

```txt

"broood"
[1, 17, 15, 0, 0, 5]
true
"bananaaa"
[1, 1, 13, 1, 1, 1, 0, 0]
true
"hiphophiphop"
[7, 8, 15, 2, 15, 2, 2, 3, 2, 2, 3, 2]
true

```



## Rust



```rust
fn main() {
    let examples = vec!["broood", "bananaaa", "hiphophiphop"];
    for example in examples {
        let encoded = encode(example);
        let decoded = decode(&encoded);
        println!(
            "{} encodes to {:?} decodes to {}",
            example, encoded, decoded
        );
    }
}

fn get_symbols() -> Vec<u8> {
    (b'a'..b'z').collect()
}

fn encode(input: &str) -> Vec<usize> {
    input
        .as_bytes()
        .iter()
        .fold((Vec::new(), get_symbols()), |(mut o, mut s), x| {
            let i = s.iter().position(|c| c == x).unwrap();
            let c = s.remove(i);
            s.insert(0, c);
            o.push(i);
            (o, s)
        })
        .0
}

fn decode(input: &[usize]) -> String {
    input
        .iter()
        .fold((Vec::new(), get_symbols()), |(mut o, mut s), x| {
            o.push(s[*x]);
            let c = s.remove(*x);
            s.insert(0, c);
            (o, s)
        })
        .0
        .into_iter()
        .map(|c| c as char)
        .collect()
}
```


{{out}}

```txt

broood encodes to [1, 17, 15, 0, 0, 5] decodes to broood
bananaaa encodes to [1, 1, 13, 1, 1, 1, 0, 0] decodes to bananaaa
hiphophiphop encodes to [7, 8, 15, 2, 15, 2, 2, 3, 2, 2, 3, 2] decodes to hiphophiphop

```



## Scala

{{works with|Scala|2.11.8+}}

```scala
package rosetta

import scala.annotation.tailrec

object MoveToFront {
  /**
   *  Default radix
   */
  private val R = 256

  /**
   *  Default symbol table
   */
  private def symbolTable = (0 until R).map(_.toChar).mkString

  /**
   *  Apply move-to-front encoding using default symbol table.
   */
  def encode(s: String): List[Int] = {
    encode(s, symbolTable)
  }

  /**
   *  Apply move-to-front encoding using symbol table <tt>symTable</tt>.
   */
  def encode(s: String, symTable: String): List[Int] = {
    val table = symTable.toCharArray

    @inline @tailrec def moveToFront(ch: Char, index: Int, tmpout: Char): Int = {
      val tmpin = table(index)
      table(index) = tmpout
      if (ch != tmpin)
        moveToFront(ch, index + 1, tmpin)
      else {
        table(0) = ch
        index
      }
    }

    @tailrec def encodeString(output: List[Int], s: List[Char]): List[Int] = s match {
      case Nil => output
      case x :: xs => {
        encodeString(moveToFront(x, 0, table(0)) :: output, s.tail)
      }
    }
    encodeString(Nil, s.toList).reverse
  }

  /**
   *  Apply move-to-front decoding using default symbol table.
   */
  def decode(ints: List[Int]): String = {
    decode(ints, symbolTable)
  }

  /**
   *  Apply move-to-front decoding using symbol table <tt>symTable</tt>.
   */
  def decode(lst: List[Int], symTable: String): String = {
    val table = symTable.toCharArray

    @inline def moveToFront(c: Char, index: Int) {
      for (i <- index-1 to 0 by -1)
        table(i+1) = table(i)
      table(0) = c
    }

    @tailrec def decodeList(output: List[Char], lst: List[Int]): List[Char] = lst match {
      case Nil => output
      case x :: xs => {
        val c = table(x)
        moveToFront(c, x)
        decodeList(c :: output, xs)
      }
    }
    decodeList(Nil, lst).reverse.mkString
  }

  def test(toEncode: String, symTable: String) {
		val encoded = encode(toEncode, symTable)
		println(toEncode + ": " + encoded)
		val decoded = decode(encoded, symTable)
		if (toEncode != decoded)
		  print("in")
		println("correctly decoded to " + decoded)
	}
}

/**
 * Unit tests the <tt>MoveToFront</tt> data type.
 */
object RosettaCodeMTF extends App {
	val symTable = "abcdefghijklmnopqrstuvwxyz"
	MoveToFront.test("broood", symTable)
	MoveToFront.test("bananaaa", symTable)
	MoveToFront.test("hiphophiphop", symTable)
}
```

{{out}}

```txt
broood: List(1, 17, 15, 0, 0, 5)
correctly decoded to broood
bananaaa: List(1, 1, 13, 1, 1, 1, 0, 0)
correctly decoded to bananaaa
hiphophiphop: List(7, 8, 15, 2, 15, 2, 2, 3, 2, 2, 3, 2)
correctly decoded to hiphophiphop
```



## Sidef

{{trans|Perl}}
Implemented using regular expressions:

```ruby
func encode(str) {
    var table = ('a'..'z' -> join);
    str.chars.map { |c|
        var s = '';
        table.sub!(Regex('(.*?)' + c), {|s1| s=s1; c + s1});
        s.len;
    }
}

func decode(nums) {
    var table = ('a'..'z' -> join);
    nums.map { |n|
        var s = '';
        table.sub!(Regex('(.{' + n + '})(.)'), {|s1, s2| s=s2; s2 + s1});
        s;
    }.join;
}

%w(broood bananaaa hiphophiphop).each { |test|
    var encoded = encode(test);
    say "#{test}: #{encoded}";
    var decoded = decode(encoded);
    print "in" if (decoded != test);
    say "correctly decoded to #{decoded}";
}
```


Alternatively, implemented as a module, using arrays:

```ruby
module MoveToFront {

  define ABC = @("a".."z")

  func m2f(ar,i) {
    [ar.delete_index(i)] + ar
  }

  func encode(str) {
    var ar = ABC+[]
    gather {
      str.each_char { |char|
        take(var i = ar.index(char))
        ar = m2f(ar, i);
      }
    }
  }

  func decode(indices) {
    var ar = ABC+[]
    gather {
      indices.each { |i|
        take ar[i];
        ar = m2f(ar, i)
      }
    }.join
  }
}

%w(broood bananaaa hiphophiphop).each { |test|
    var encoded = MoveToFront::encode(test);
    say "#{test}: #{encoded}";
    var decoded = MoveToFront::decode(encoded);
    print "in" if (decoded != test);
    say "correctly decoded to #{decoded}";
}
```


{{out}}

```txt

broood: 1 17 15 0 0 5
correctly decoded to broood
bananaaa: 1 1 13 1 1 1 0 0
correctly decoded to bananaaa
hiphophiphop: 7 8 15 2 15 2 2 3 2 2 3 2
correctly decoded to hiphophiphop

```



## Swift


```swift


var str="broood"
var number:[Int]=[1,17,15,0,0,5]

//function to encode the string
func encode(st:String)->[Int]
{

var array:[Character]=["a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z"]

	var num:[Int]=[]
	var temp:Character="a"
	var i1:Int=0
	for i in st.characters
	{
		for j in 0...25
		{
			if i==array[j]
			{
				num.append(j)
				temp=array[j]
				i1=j
				while(i1>0)
				{
					array[i1]=array[i1-1]
					i1=i1-1

				}
				array[0]=temp
			}

		}


	}

	return num

}

func decode(s:[Int])->[Character]
{

	var st1:[Character]=[]
	var alph:[Character]=["a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z"]
	var temp1:Character="a"
	var i2:Int=0
	for i in 0...s.character.count-1
	{
		i2=s[i]
		st1.append(alph[i2])
		temp1=alph[i2]

				while(i2>0)
				{
					alph[i2]=alph[i2-1]
					i2=i2-1

				}
				alph[0]=temp1

	}
	return st1

}

var encarr:[Int]=encode(st:str)
var decarr:[Character]=decode(s:number)
print(encarr)
print(decarr)

```



## Tcl

{{works with|Tcl|8.6}}

```tcl
package require Tcl 8.6

oo::class create MoveToFront {
    variable symbolTable
    constructor {symbols} {
	set symbolTable [split $symbols ""]
    }

    method MoveToFront {table index} {
	list [lindex $table $index] {*}[lreplace $table $index $index]
    }
    method encode {text} {
	set t $symbolTable
	set r {}
	foreach c [split $text ""] {
	    set i [lsearch -exact $t $c]
	    lappend r $i
	    set t [my MoveToFront $t $i]
	}
	return $r
    }
    method decode {numbers} {
	set t $symbolTable
	set r ""
	foreach n $numbers {
	    append r [lindex $t $n]
	    set t [my MoveToFront $t $n]
	}
	return $r
    }
}

MoveToFront create mtf "abcdefghijklmnopqrstuvwxyz"
foreach tester {"broood" "bananaaa" "hiphophiphop"} {
    set enc [mtf encode $tester]
    set dec [mtf decode $enc]
    puts [format "'%s' encodes to %s. This decodes to '%s'. %s" \
	    $tester $enc $dec [expr {$tester eq $dec ? "Correct!" : "WRONG!"}]]
}
```

{{out}}

```txt

'broood' encodes to 1 17 15 0 0 5. This decodes to 'broood'. Correct!
'bananaaa' encodes to 1 1 13 1 1 1 0 0. This decodes to 'bananaaa'. Correct!
'hiphophiphop' encodes to 7 8 15 2 15 2 2 3 2 2 3 2. This decodes to 'hiphophiphop'. Correct!

```



## VBScript


```vb
Function mtf_encode(s)
	'create the array list and populate it with the initial symbol position
	Set symbol_table = CreateObject("System.Collections.ArrayList")
	For j = 97 To 122 'a to z in decimal.
		symbol_table.Add Chr(j)
	Next
	output = ""
	For i = 1 To Len(s)
		char = Mid(s,i,1)
		If i = Len(s) Then
			output = output & symbol_table.IndexOf(char,0)
			symbol_table.RemoveAt(symbol_table.LastIndexOf(char))
			symbol_table.Insert 0,char
		Else
			output = output & symbol_table.IndexOf(char,0) & " "
			symbol_table.RemoveAt(symbol_table.LastIndexOf(char))
			symbol_table.Insert 0,char
		End If
	Next
	mtf_encode = output
End Function

Function mtf_decode(s)
	'break the function argument into an array
	code = Split(s," ")
	'create the array list and populate it with the initial symbol position
	Set symbol_table = CreateObject("System.Collections.ArrayList")
	For j = 97 To 122 'a to z in decimal.
		symbol_table.Add Chr(j)
	Next
	output = ""
	For i = 0 To UBound(code)
		char = symbol_table(code(i))
		output = output & char
		If code(i) <> 0 Then
			symbol_table.RemoveAt(symbol_table.LastIndexOf(char))
			symbol_table.Insert 0,char
		End If
	Next
	mtf_decode = output
End Function

'Testing the functions
wordlist = Array("broood","bananaaa","hiphophiphop")
For Each word In wordlist
	WScript.StdOut.Write word & " encodes as " & mtf_encode(word) & " and decodes as " &_
		mtf_decode(mtf_encode(word)) & "."
	WScript.StdOut.WriteBlankLines(1)
Next
```


{{Out}}

```txt

broood encodes as 1 17 15 0 0 5 and decodes as broood.
bananaaa encodes as 1 1 13 1 1 1 0 0 and decodes as bananaaa.
hiphophiphop encodes as 7 8 15 2 15 2 2 3 2 2 3 2 and decodes as hiphophiphop.

```



## zkl


```zkl
fcn encode(text){ //-->List
   st:=["a".."z"].pump(Data);	//"abcd..z" as byte array
   text.reduce(fcn(st,c,sink){
      n:=st.index(c); sink.write(n); st.del(n).insert(0,c); },st,sink:=L());
   sink;
}
```

Strings are immutable so we create a bit bucket (which is mutable) to hold the symbol table which can then be modified in place.

```zkl
fcn decode(list){ //-->String
   st:=["a".."z"].pump(String);		//"abcd..z"
   sink:=Sink(String);
   list.reduce('wrap(st,n){ c:=st[n]; sink.write(c); c+st.del(n); },st);
   sink.close();
}
```

Here, we create a new symbol table each round as we would have to convert the byte we got from the bit bucket to string (so it is a wash garbage wise).

```zkl
texts:=T("broood","bananaaa","hiphophiphop");
out:=texts.apply(encode);
texts.zipWith(fcn(t,e){ println(t,"-->",e) },out);

out.apply(decode).println();
texts.zipWith('==,out.apply(decode)).println();
```

{{out}}

```txt

broood-->L(1,17,15,0,0,5)
bananaaa-->L(1,1,13,1,1,1,0,0)
hiphophiphop-->L(7,8,15,2,15,2,2,3,2,2,3,2)
L("broood","bananaaa","hiphophiphop")
L(True,True,True)

```

