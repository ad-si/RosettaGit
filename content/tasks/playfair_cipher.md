+++
title = "Playfair cipher"
description = ""
date = 2018-10-28T21:21:26Z
aliases = []
[extra]
id = 13455
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "cpp",
  "d",
  "freebasic",
  "go",
  "haskell",
  "j",
  "java",
  "kotlin",
  "netrexx",
  "oorexx",
  "perl",
  "perl_6",
  "phix",
  "python",
  "rexx",
  "sidef",
  "sql",
  "tcl",
  "vba",
  "zkl",
]
+++

{{draft task}} [[Category:Encryption]]
Implement a [[wp: Playfair cipher| Playfair cipher]] for encryption and decryption.

The user must be able to choose J = I or no Q in the alphabet.

The output of the encrypted and decrypted message must be in capitalized digraphs, separated by spaces.

Output example: HI DE TH EG OL DI NT HE TR EX ES TU MP.


## C++


```cpp
#include <iostream>
#include <string>

using namespace std;

class playfair
{
public:
    void doIt( string k, string t, bool ij, bool e )
    {
	createGrid( k, ij ); getTextReady( t, ij, e );
	if( e ) doIt( 1 ); else doIt( -1 );
	display();
    }

private:
    void doIt( int dir )
    {
	int a, b, c, d; string ntxt;
	for( string::const_iterator ti = _txt.begin(); ti != _txt.end(); ti++ )
	{
	    if( getCharPos( *ti++, a, b ) )
		if( getCharPos( *ti, c, d ) )
		{
		    if( a == c )     { ntxt += getChar( a, b + dir ); ntxt += getChar( c, d + dir ); }
		    else if( b == d ){ ntxt += getChar( a + dir, b ); ntxt += getChar( c + dir, d ); }
		    else             { ntxt += getChar( c, b ); ntxt += getChar( a, d ); }
		}
	}
	_txt = ntxt;
    }

    void display()
    {
	cout << "\n\n OUTPUT:\n
### ===
" << endl;
	string::iterator si = _txt.begin(); int cnt = 0;
	while( si != _txt.end() )
	{
	    cout << *si; si++; cout << *si << " "; si++;
	    if( ++cnt >= 26 ) cout << endl, cnt = 0;
	}
	cout << endl << endl;
    }

    char getChar( int a, int b )
    {
	return _m[ (b + 5) % 5 ][ (a + 5) % 5 ];
    }

    bool getCharPos( char l, int &a, int &b )
    {
	for( int y = 0; y < 5; y++ )
	    for( int x = 0; x < 5; x++ )
		if( _m[y][x] == l )
		{ a = x; b = y; return true; }

	return false;
    }

    void getTextReady( string t, bool ij, bool e )
    {
	for( string::iterator si = t.begin(); si != t.end(); si++ )
	{
	    *si = toupper( *si ); if( *si < 65 || *si > 90 ) continue;
	    if( *si == 'J' && ij ) *si = 'I';
	    else if( *si == 'Q' && !ij ) continue;
	    _txt += *si;
	}
	if( e )
	{
	    string ntxt = ""; size_t len = _txt.length();
	    for( size_t x = 0; x < len; x += 2 )
	    {
		ntxt += _txt[x];
		if( x + 1 < len )
		{
		    if( _txt[x] == _txt[x + 1] ) ntxt += 'X';
		    ntxt += _txt[x + 1];
		}
	    }
	    _txt = ntxt;
	}
	if( _txt.length() & 1 ) _txt += 'X';
    }

    void createGrid( string k, bool ij )
    {
	if( k.length() < 1 ) k = "KEYWORD";
	k += "ABCDEFGHIJKLMNOPQRSTUVWXYZ"; string nk = "";
	for( string::iterator si = k.begin(); si != k.end(); si++ )
	{
	    *si = toupper( *si ); if( *si < 65 || *si > 90 ) continue;
	    if( ( *si == 'J' && ij ) || ( *si == 'Q' && !ij ) )continue;
	    if( nk.find( *si ) == -1 ) nk += *si;
	}
	copy( nk.begin(), nk.end(), &_m[0][0] );
    }

    string _txt; char _m[5][5];
};

int main( int argc, char* argv[] )
{
    string key, i, txt; bool ij, e;
    cout << "(E)ncode or (D)ecode? "; getline( cin, i ); e = ( i[0] == 'e' || i[0] == 'E' );
    cout << "Enter a en/decryption key: "; getline( cin, key );
    cout << "I <-> J (Y/N): "; getline( cin, i ); ij = ( i[0] == 'y' || i[0] == 'Y' );
    cout << "Enter the text: "; getline( cin, txt );
    playfair pf; pf.doIt( key, txt, ij, e ); return system( "pause" );
}
```

```txt

(E)ncode or (D)ecode? e
Enter a en/decryption key: playfair example
I <-> J (Y/N): y
Enter the text: Hide the gold in the tree stump

OUTPUT:

### ===

BM OD ZB XD NA BE KU DM UI XM MO UV IF


(E)ncode or (D)ecode? d
Enter a en/decryption key: playfair example
I <-> J (Y/N): y
Enter the text: BMODZBXDNABEKUDMUIXMMOUVIF

OUTPUT:

### ===

HI DE TH EG OL DI NT HE TR EX ES TU MP

```



## D

```d
import std.stdio, std.array, std.algorithm, std.range, std.ascii,
       std.conv, std.string, std.regex, std.typecons;

string unique(in string s) pure nothrow @safe {
    string result;
    foreach (immutable char c; s)
        if (!result.representation.canFind(c)) // Assumes ASCII string.
            result ~= c;
    return result;
}

struct Playfair {
    string from, to;
    string[string] enc, dec;

    this(in string key, in string from_ = "J", in string to_ = null)
    pure /*nothrow @safe*/ {
        this.from = from_;
        if (to_.empty)
            this.to = (from_ == "J") ? "I" : "";

        immutable m = _canonicalize(key ~ uppercase)
                      .unique
                      .chunks(5)
                      .map!text
                      .array;
        auto I5 = 5.iota;

        foreach (immutable R; m)
            foreach (immutable i, immutable j; cartesianProduct(I5, I5))
                if (i != j)
                    enc[[R[i], R[j]]] = [R[(i + 1) % 5], R[(j+1) % 5]];

        foreach (immutable r; I5) {
            immutable c = m.transversal(r).array;
            foreach (immutable i, immutable j; cartesianProduct(I5, I5))
                if (i != j)
                    enc[[c[i], c[j]]] = [c[(i + 1) % 5], c[(j+1) % 5]];
        }

        foreach (i1, j1, i2, j2; cartesianProduct(I5, I5, I5, I5))
            if (i1 != i2 && j1 != j2)
                enc[[m[i1][j1], m[i2][j2]]] = [m[i1][j2], m[i2][j1]];

        dec = enc.byKeyValue.map!(t => tuple(t.value, t.key)).assocArray;
    }

    private string _canonicalize(in string s) const pure @safe {
        return s.toUpper.removechars("^A-Z").replace(from, to);
    }

    string encode(in string s) const /*pure @safe*/ {
        return _canonicalize(s)
               .matchAll(r"(.)(?:(?!\1)(.))?")
               .map!(m => enc[m[0].leftJustify(2, 'X')])
               .join(' ');
    }

    string decode(in string s) const pure @safe {
        return _canonicalize(s).chunks(2).map!(p => dec[p.text]).join(' ');
    }
}

void main() /*@safe*/ {
    /*immutable*/ const pf = Playfair("Playfair example");
    immutable orig = "Hide the gold in...the TREESTUMP!!!";
    writeln("Original: ", orig);
    immutable enc = pf.encode(orig);
    writeln(" Encoded: ", enc);
    writeln(" Decoded: ", pf.decode(enc));
}
```

```txt
Original: Hide the gold in...the TREESTUMP!!!
 Encoded: BM OD ZB XD NA BE KU DM UI XM MO UV IF
 Decoded: HI DE TH EG OL DI NT HE TR EX ES TU MP
```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Enum PlayFairOption
  noQ
  iEqualsJ
End Enum

Dim Shared pfo As PlayFairOption  '' set this before calling buildTable
Dim Shared table(1 To 5, 1 To 5) As UInteger

Sub buildTable(keyword As String)
  Dim used(1 To 26) As Boolean  '' all false by default
  If pfo = noQ Then
    used(17) = True
  Else
    used(10) = True
  End If
  Dim As String alphabet = UCase(keyword) + "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  Dim As UInteger i = 1, j = 1, k
  Dim As Integer c
  For k = 0 To Len(alphabet) - 1
    c = alphabet[k] - 64
    If c < 1 OrElse c > 26 Then Continue For
    If Not used(c) Then
      table(i, j) = c
      used(c) = True
      j += 1
      If j = 6 Then
        i += 1
        If i = 6 Then Return  '' table has been filled
        j = 1
      End If
    End If
  Next k
End Sub

Function getCleanText(plainText As String) As String
  plainText = UCase(plainText) '' ensure everything is upper case
  ' get rid of any non-letters and insert X between duplicate letters
  Dim As String cleanText = "", prevChar = "", nextChar
  For i As UInteger = 1 To Len(plainText)
    nextChar = Mid(plainText, i, 1)
    ' It appears that Q should be omitted altogether if noQ option is specified - we assume so anyway
    If nextChar < "A" OrElse nextChar > "Z"  OrElse (nextChar = "Q" AndAlso pfo = noQ) Then Continue For
    ' If iEqualsJ option specified, replace J with I
    If nextChar = "J" AndAlso pfo = iEqualsJ Then nextChar = "I"
    If nextChar <> prevChar Then
      cleanText += nextChar
    Else
      cleanText += "X" + nextChar
    End If
    prevChar = nextChar
  Next
  If Len(cleanText) Mod 2 = 1 Then  '' dangling letter at end so add another letter to complete digram
    If Right(cleanText, 1) <> "X" Then
      cleanText += "X"
    Else
      cleanText += "Z"
    End If
  End If
  Return cleanText
End Function

Sub findChar(c As uInteger, ByRef row As UInteger, ByRef col As UInteger)
  For i As UInteger = 1 To 5
    For j As UInteger = 1 To 5
      If table(i, j) = c Then
        row = i
        col = j
        Return
      End If
    Next j
  Next i
End Sub

Function encodePlayfair(plainText As String) As String
  Dim As String cleanText = getCleanText(plainText)
  Dim As String digram, cipherDigram, cipherText = ""
  Dim As UInteger length = Len(cleanText)
  Dim As UInteger char1, char2, row1, col1, row2, col2
  For i As UInteger = 1 To length Step 2
    digram = Mid(cleanText, i, 2)
    char1 = digram[0] - 64
    char2 = digram[1] - 64
    findChar char1, row1, col1
    findChar char2, row2, col2
    If row1 = row2 Then
      cipherDigram =  Chr(table(row1, col1 Mod 5 + 1) + 64)
      cipherDigram += Chr(table(row2, col2 Mod 5 + 1) + 64)
    ElseIf col1 = col2 Then
      cipherDigram =  Chr(table(row1 Mod 5 + 1, col1) + 64)
      cipherDigram += Chr(table(row2 Mod 5 + 1, col2) + 64)
    Else
      cipherDigram =  Chr(table(row1, col2) + 64)
      cipherDigram += Chr(table(row2, col1) + 64)
    End If
    cipherText += cipherDigram
    If i < length Then cipherText += " "
  Next i
  Return cipherText
End Function

Function decodePlayfair(cipherText As String) As String
  Dim As String digram, cipherDigram, decodedText = ""
  Dim As UInteger length = Len(cipherText)
  Dim As UInteger char1, char2, row1, col1, row2, col2
  For i As UInteger = 1 To length Step 3  '' cipherText will include spaces so we need to skip them
    cipherDigram = Mid(cipherText, i, 2)
    char1 = cipherDigram[0] - 64
    char2 = cipherDigram[1] - 64
    findChar char1, row1, col1
    findChar char2, row2, col2
    If row1 = row2 Then
      digram =  Chr(table(row1, IIf(col1 > 1, col1 - 1, 5)) + 64)
      digram += Chr(table(row2, IIf(col2 > 1, col2 - 1, 5)) + 64)
    ElseIf col1 = col2 Then
      digram =  Chr(table(IIf(row1 > 1, row1 - 1, 5), col1) + 64)
      digram += Chr(table(IIf(row2 > 1, row2 - 1, 5), col2) + 64)
    Else
      digram =  Chr(table(row1, col2) + 64)
      digram += Chr(table(row2, col1) + 64)
    End If
    decodedText += digram
    If i < length Then decodedText += " "
  Next i
  Return decodedText
End Function

Dim As String keyword, ignoreQ, plainText, encodedText, decodedText
Line Input "Enter Playfair keyword : "; keyword

Do
  Line Input "Ignore Q when buiding table  y/n : "; ignoreQ
  ignoreQ = LCase(ignoreQ)
Loop Until ignoreQ = "y" Or ignoreQ = "n"

pfo = IIf(ignoreQ = "y", noQ, iEqualsJ)
buildTable(keyword)
Print "The table to be used is : " : Print
For i As UInteger = 1 To 5
  For j As UInteger = 1 To 5
    Print Chr(table(i, j) + 64); " ";
  Next j
  Print
Next i

Print
Line Input "Enter plain text : "; plainText
Print
encodedText = encodePlayfair(plainText)
Print "Encoded text is : "; encodedText
decodedText = decodePlayFair(encodedText)
Print "Decoded text is : "; decodedText
Print
Print "Press any key to quit"
Sleep
```

Sample input/output:
```txt

Enter Playfair keyword : ? Playfair example
Ignore Q when buiding table  y/n : ? n
The table to be used is :

P L A Y F
I R E X M
B C D G H
K N O Q S
T U V W Z

Enter plain text : ? Hide the gold in...the TREESTUMP!!!!

Encoded text is : BM OD ZB XD NA BE KU DM UI XM MO UV IF
Decoded text is : HI DE TH EG OL DI NT HE TR EX ES TU MP

```



## Go

```go
package main

import (
    "bufio"
    "fmt"
    "os"
    "strings"
)

type playfairOption int

const (
    noQ playfairOption = iota
    iEqualsJ
)

type playfair struct {
    keyword string
    pfo     playfairOption
    table   [5][5]byte
}

func (p *playfair) init() {
    // Build table.
    var used [26]bool // all elements false
    if p.pfo == noQ {
        used[16] = true // Q used
    } else {
        used[9] = true // J used
    }
    alphabet := strings.ToUpper(p.keyword) + "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    for i, j, k := 0, 0, 0; k < len(alphabet); k++ {
        c := alphabet[k]
        if c < 'A' || c > 'Z' {
            continue
        }
        d := int(c - 65)
        if !used[d] {
            p.table[i][j] = c
            used[d] = true
            j++
            if j == 5 {
                i++
                if i == 5 {
                    break // table has been filled
                }
                j = 0
            }
        }
    }
}

func (p *playfair) getCleanText(plainText string) string {
    // Ensure everything is upper case.
    plainText = strings.ToUpper(plainText)
    // Get rid of any non-letters and insert X between duplicate letters.
    var cleanText strings.Builder
    // Safe to assume null byte won't be present in plainText.
    prevByte := byte('\000')
    for i := 0; i < len(plainText); i++ {
        nextByte := plainText[i]
        // It appears that Q should be omitted altogether if NO_Q option is specified;
        // we assume so anyway.
        if nextByte < 'A' || nextByte > 'Z' || (nextByte == 'Q' && p.pfo == noQ) {
            continue
        }
        // If iEqualsJ option specified, replace J with I.
        if nextByte == 'J' && p.pfo == iEqualsJ {
            nextByte = 'I'
        }
        if nextByte != prevByte {
            cleanText.WriteByte(nextByte)
        } else {
            cleanText.WriteByte('X')
            cleanText.WriteByte(nextByte)
        }
        prevByte = nextByte
    }
    l := cleanText.Len()
    if l%2 == 1 {
        // Dangling letter at end so add another letter to complete digram.
        if cleanText.String()[l-1] != 'X' {
            cleanText.WriteByte('X')
        } else {
            cleanText.WriteByte('Z')
        }
    }
    return cleanText.String()
}

func (p *playfair) findByte(c byte) (int, int) {
    for i := 0; i < 5; i++ {
        for j := 0; j < 5; j++ {
            if p.table[i][j] == c {
                return i, j
            }
        }
    }
    return -1, -1
}

func (p *playfair) encode(plainText string) string {
    cleanText := p.getCleanText(plainText)
    var cipherText strings.Builder
    l := len(cleanText)
    for i := 0; i < l; i += 2 {
        row1, col1 := p.findByte(cleanText[i])
        row2, col2 := p.findByte(cleanText[i+1])
        switch {
        case row1 == row2:
            cipherText.WriteByte(p.table[row1][(col1+1)%5])
            cipherText.WriteByte(p.table[row2][(col2+1)%5])
        case col1 == col2:
            cipherText.WriteByte(p.table[(row1+1)%5][col1])
            cipherText.WriteByte(p.table[(row2+1)%5][col2])
        default:
            cipherText.WriteByte(p.table[row1][col2])
            cipherText.WriteByte(p.table[row2][col1])
        }
        if i < l-1 {
            cipherText.WriteByte(' ')
        }
    }
    return cipherText.String()
}

func (p *playfair) decode(cipherText string) string {
    var decodedText strings.Builder
    l := len(cipherText)
    // cipherText will include spaces so we need to skip them.
    for i := 0; i < l; i += 3 {
        row1, col1 := p.findByte(cipherText[i])
        row2, col2 := p.findByte(cipherText[i+1])
        switch {
        case row1 == row2:
            temp := 4
            if col1 > 0 {
                temp = col1 - 1
            }
            decodedText.WriteByte(p.table[row1][temp])
            temp = 4
            if col2 > 0 {
                temp = col2 - 1
            }
            decodedText.WriteByte(p.table[row2][temp])
        case col1 == col2:
            temp := 4
            if row1 > 0 {
                temp = row1 - 1
            }
            decodedText.WriteByte(p.table[temp][col1])
            temp = 4
            if row2 > 0 {
                temp = row2 - 1
            }
            decodedText.WriteByte(p.table[temp][col2])
        default:
            decodedText.WriteByte(p.table[row1][col2])
            decodedText.WriteByte(p.table[row2][col1])
        }
        if i < l-1 {
            decodedText.WriteByte(' ')
        }
    }
    return decodedText.String()
}

func (p *playfair) printTable() {
    fmt.Println("The table to be used is :\n")
    for i := 0; i < 5; i++ {
        for j := 0; j < 5; j++ {
            fmt.Printf("%c ", p.table[i][j])
        }
        fmt.Println()
    }
}

func main() {
    scanner := bufio.NewScanner(os.Stdin)
    fmt.Print("Enter Playfair keyword : ")
    scanner.Scan()
    keyword := scanner.Text()
    var ignoreQ string
    for ignoreQ != "y" && ignoreQ != "n" {
        fmt.Print("Ignore Q when building table  y/n : ")
        scanner.Scan()
        ignoreQ = strings.ToLower(scanner.Text())
    }
    pfo := noQ
    if ignoreQ == "n" {
        pfo = iEqualsJ
    }
    var table [5][5]byte
    pf := &playfair{keyword, pfo, table}
    pf.init()
    pf.printTable()
    fmt.Print("\nEnter plain text : ")
    scanner.Scan()
    plainText := scanner.Text()
    if err := scanner.Err(); err != nil {
        fmt.Fprintln(os.Stderr, "reading standard input:", err)
        return
    }
    encodedText := pf.encode(plainText)
    fmt.Println("\nEncoded text is :", encodedText)
    decodedText := pf.decode(encodedText)
    fmt.Println("Deccoded text is :", decodedText)
}
```


Sample run:

```txt

Enter Playfair keyword : Playfair example
Ignore Q when building table  y/n : n
The table to be used is :

P L A Y F
I R E X M
B C D G H
K N O Q S
T U V W Z

Enter plain text : Hide the gold...in the TREESTUMP!!!!

Encoded text is : BM OD ZB XD NA BE KU DM UI XM MO UV IF
Deccoded text is : HI DE TH EG OL DI NT HE TR EX ES TU MP

```



## Haskell

(My guess is that  map (\[x, y] -> if x == y then [x, 'x'] else [x, y]).chunksOf 2 is simply discarding the y. [[User:Petelomax|Pete Lomax]] ([[User talk:Petelomax|talk]]) 05:54, 13 October 2018 (UTC))

```haskell

import Control.Monad     (guard)
import Data.Array        (Array, assocs, elems, listArray, (!))
import Data.Char         (toUpper)
import Data.List         (nub, (\\))
import Data.List.Split   (chunksOf)
import Data.Maybe        (listToMaybe)
import Data.String.Utils (replace)

type Square a = Array (Int, Int) a

-- | Turns a list into an n*m-array.
array2D ::
       (Int, Int) -- ^ n * m
    -> [e] -> Square e
array2D maxCoord = listArray ((1, 1), maxCoord)

-- | Generates a playfair table starting with the specified string.
--
-- >>> makeTable "hello"
-- "HELOABCDFGIKMNPQRSTUVWXYZ"
makeTable :: String -> String
makeTable k = nub key ++ (alpha \\ key)
    where
      alpha = ['A' .. 'Z'] \\ "J"
      key = map toUpper =<< words k

-- | Turns a playfair table into a 5*5 alphabet square.
makeSquare :: [a] -> Square a
makeSquare = array2D (5, 5)

-- | Displays a playfair square, formatted as a square.
showSquare :: Square Char -> String
showSquare d = unlines $ chunksOf 5 (elems d)

-- | Given a value and an association list of x-coordinate * y-coordinate * value, returns the coordinates
getIndex' :: (Eq a) => a -> [((Int, Int), a)] -> Maybe (Int, Int)
getIndex' el = fmap fst . listToMaybe . filter ((== el) . snd)

encodePair, decodePair :: Eq a => Square a -> (a, a) -> Maybe (a, a)
encodePair = pairHelper (\x -> if x == 5 then 1 else x + 1)
decodePair = pairHelper (\x -> if x == 1 then 5 else x - 1)

pairHelper :: (Eq t)
    => (Int -> Int) -- ^ a function used for wrapping around the square
    -> Square t -- ^ a playfair square
    -> (t, t) -- ^ two characters
    -> Maybe (t, t) -- ^ the two resulting/encoded characters
pairHelper adjust sqr (c1, c2) =
    do let ps = assocs sqr
       -- assigns an association list of (x-coord * y-coord) * value to ps
       (x1, y1) <- getIndex' c1 ps
       (x2, y2) <- getIndex' c2 ps
       -- returns the coordinates of two values in the square
       -- these will later be swapped
       guard $ c1 /= c2
       -- the characters (and coordinates) cannot be the same
       let get x = sqr ! x
       -- a small utility function for extracting a value from the square
       Just $
           -- wrap the coordinates around and find the encrypted characters
           case () of
             () | y1 == y2 ->
                    (get (adjust x1, y1), get (adjust x2, y2))
                | x1 == x2 ->
                    (get (x1, adjust y1), get (x2, adjust y2))
                | otherwise ->
                    (get (x1, y2), get (x2, y1))

-- | Turns two characters into a tuple.
parsePair :: String -> [(Char, Char)]
parsePair = fmap (\[x, y] -> (x, y)) . words . fmap toUpper

-- | Turns a tuple of two characters into a string.
unparsePair :: [(Char, Char)] -> String
unparsePair = unwords . fmap (\(x, y) -> [x, y])

codeHelper :: (Square Char -> (Char, Char) -> Maybe (Char, Char))
    -> String -> String -> Maybe String
codeHelper subs key =
    fmap unparsePair .
    mapM (subs (makeSquare $ makeTable key)) .
    parsePair

playfair, unplayfair :: String -> String -> Maybe String
playfair key = codeHelper encodePair key . formatEncode
unplayfair = codeHelper decodePair

formatEncode :: String -> String
formatEncode =
    map toUpper .
    unwords .
    map (\[x, y] -> if x == y then [x, 'x'] else [x, y]) .
    chunksOf 2 .
    replace "j" "i" .
    concatMap adjustLength .
    words .
    filter (\n -> n `elem` (['A'..'Z'] ++ ['a'..'z']))
    where
      adjustLength str
          | odd (length str) = str ++ "x"
          | otherwise = str

```



```txt

>>> playfair "playfair example" "hide the gold in the tree stump"
Just "BM OD ZB XD NA BE KU DM UI XM KZ ZR YI"

>>> unplayfair "playfair example" "BM OD ZB XD NA BE KU DM UI XM KZ ZR YI"
Just "HI DE TH EG OL DI NT HE TR EX ST UM PX"

```



## J

Rather than implement two versions of the rules, one for encrypt, one for decrypt, let's just make a lookup table (mapping character pairs to character pairs). To decrypt we can look up in the other direction.

Implementation:


```J
choose=:3 :0
  sel=. 'Q'e.y
  alph=:(sel{'JQ')-.~a.{~65+i.26
  norm=: alph restrict('I' I.@:=&'J'@]} ])`(-.&'Q')@.sel@toupper
  ''
)
restrict=: ] -. -.~
choose 'Q'

setkey=:3 :0
  key=. ~.norm y,alph
  ref=: ,/2{."1 ~."1 (,"0/~alph),"1 norm 'XQV'
  mode=. #.=/"2 inds=.5 5#:key i.ref
  inds0=. (0 3,:2 1)&{@,"2 inds
  inds1=.5|1 0+"1 inds NB. same column
  inds2=.5|0 1+"1 inds NB. same row
  alt=:key{~5 #.mode{"_1 inds0,"2 3 inds1,:"2 inds2
  i.0 0
)

pairs=: 3 :0
  2{."1 -.&' '"1 ~."1 (_2]\ norm y),"1 'XQV'
)

encrypt=:3 :0
  ,alt{~ref i. pairs y
)

decrypt=:3 :0
  ,ref{~alt i. pairs y
)
```


Example use:


```J
   choose 'IJ'

   setkey 'playfair example'
   encrypt 'Hide the gold in the tree stump'
BMODZBXDNABEKUDMUIXMKZZRYI
   decrypt 'BMODZBXDNABEKUDMUIXMKZZRYI'
HIDETHEGOLDINTHETREXSTUMPX
```



## Java


```java
import java.awt.Point;
import java.util.Scanner;

public class PlayfairCipher {
    private static char[][] charTable;
    private static Point[] positions;

    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);

        String key = prompt("Enter an encryption key (min length 6): ", sc, 6);
        String txt = prompt("Enter the message: ", sc, 1);
        String jti = prompt("Replace J with I? y/n: ", sc, 1);

        boolean changeJtoI = jti.equalsIgnoreCase("y");

        createTable(key, changeJtoI);

        String enc = encode(prepareText(txt, changeJtoI));

        System.out.printf("%nEncoded message: %n%s%n", enc);
        System.out.printf("%nDecoded message: %n%s%n", decode(enc));
    }

    private static String prompt(String promptText, Scanner sc, int minLen) {
        String s;
        do {
            System.out.print(promptText);
            s = sc.nextLine().trim();
        } while (s.length() < minLen);
        return s;
    }

    private static String prepareText(String s, boolean changeJtoI) {
        s = s.toUpperCase().replaceAll("[^A-Z]", "");
        return changeJtoI ? s.replace("J", "I") : s.replace("Q", "");
    }

    private static void createTable(String key, boolean changeJtoI) {
        charTable = new char[5][5];
        positions = new Point[26];

        String s = prepareText(key + "ABCDEFGHIJKLMNOPQRSTUVWXYZ", changeJtoI);

        int len = s.length();
        for (int i = 0, k = 0; i < len; i++) {
            char c = s.charAt(i);
            if (positions[c - 'A'] == null) {
                charTable[k / 5][k % 5] = c;
                positions[c - 'A'] = new Point(k % 5, k / 5);
                k++;
            }
        }
    }

    private static String encode(String s) {
        StringBuilder sb = new StringBuilder(s);

        for (int i = 0; i < sb.length(); i += 2) {

            if (i == sb.length() - 1)
                sb.append(sb.length() % 2 == 1 ? 'X' : "");

            else if (sb.charAt(i) == sb.charAt(i + 1))
                sb.insert(i + 1, 'X');
        }
        return codec(sb, 1);
    }

    private static String decode(String s) {
        return codec(new StringBuilder(s), 4);
    }

    private static String codec(StringBuilder text, int direction) {
        int len = text.length();
        for (int i = 0; i < len; i += 2) {
            char a = text.charAt(i);
            char b = text.charAt(i + 1);

            int row1 = positions[a - 'A'].y;
            int row2 = positions[b - 'A'].y;
            int col1 = positions[a - 'A'].x;
            int col2 = positions[b - 'A'].x;

            if (row1 == row2) {
                col1 = (col1 + direction) % 5;
                col2 = (col2 + direction) % 5;

            } else if (col1 == col2) {
                row1 = (row1 + direction) % 5;
                row2 = (row2 + direction) % 5;

            } else {
                int tmp = col1;
                col1 = col2;
                col2 = tmp;
            }

            text.setCharAt(i, charTable[row1][col1]);
            text.setCharAt(i + 1, charTable[row2][col2]);
        }
        return text.toString();
    }
}
```



###  alternative version



```java
import java.util.Scanner;

public class PlayfairCipherEncryption
{
    private String KeyWord        = new String();
    private String Key            = new String();
    private char   matrix_arr[][] = new char[5][5];

    public void setKey(String k)
    {
        String K_adjust = new String();
        boolean flag = false;
        K_adjust = K_adjust + k.charAt(0);
        for (int i = 1; i < k.length(); i++)
        {
            for (int j = 0; j < K_adjust.length(); j++)
            {
                if (k.charAt(i) == K_adjust.charAt(j))
                {
                    flag = true;
                }
            }
            if (flag == false)
                K_adjust = K_adjust + k.charAt(i);
            flag = false;
        }
        KeyWord = K_adjust;
    }

    public void KeyGen()
    {
        boolean flag = true;
        char current;
        Key = KeyWord;
        for (int i = 0; i < 26; i++)
        {
            current = (char) (i + 97);
            if (current == 'j')
                continue;
            for (int j = 0; j < KeyWord.length(); j++)
            {
                if (current == KeyWord.charAt(j))
                {
                    flag = false;
                    break;
                }
            }
            if (flag)
                Key = Key + current;
            flag = true;
        }
        System.out.println(Key);
        matrix();
    }

    private void matrix()
    {
        int counter = 0;
        for (int i = 0; i < 5; i++)
        {
            for (int j = 0; j < 5; j++)
            {
                matrix_arr[i][j] = Key.charAt(counter);
                System.out.print(matrix_arr[i][j] + " ");
                counter++;
            }
            System.out.println();
        }
    }

    private String format(String old_text)
    {
        int i = 0;
        int len = 0;
        String text = new String();
        len = old_text.length();
        for (int tmp = 0; tmp < len; tmp++)
        {
            if (old_text.charAt(tmp) == 'j')
            {
                text = text + 'i';
            }
            else
                text = text + old_text.charAt(tmp);
        }
        len = text.length();
        for (i = 0; i < len; i = i + 2)
        {
            if (text.charAt(i + 1) == text.charAt(i))
            {
                text = text.substring(0, i + 1) + 'x' + text.substring(i + 1);
            }
        }
        return text;
    }

    private String[] Divid2Pairs(String new_string)
    {
        String Original = format(new_string);
        int size = Original.length();
        if (size % 2 != 0)
        {
            size++;
            Original = Original + 'x';
        }
        String x[] = new String[size / 2];
        int counter = 0;
        for (int i = 0; i < size / 2; i++)
        {
            x[i] = Original.substring(counter, counter + 2);
            counter = counter + 2;
        }
        return x;
    }

    public int[] GetDiminsions(char letter)
    {
        int[] key = new int[2];
        if (letter == 'j')
            letter = 'i';
        for (int i = 0; i < 5; i++)
        {
            for (int j = 0; j < 5; j++)
            {
                if (matrix_arr[i][j] == letter)
                {
                    key[0] = i;
                    key[1] = j;
                    break;
                }
            }
        }
        return key;
    }

    public String encryptMessage(String Source)
    {
        String src_arr[] = Divid2Pairs(Source);
        String Code = new String();
        char one;
        char two;
        int part1[] = new int[2];
        int part2[] = new int[2];
        for (int i = 0; i < src_arr.length; i++)
        {
            one = src_arr[i].charAt(0);
            two = src_arr[i].charAt(1);
            part1 = GetDiminsions(one);
            part2 = GetDiminsions(two);
            if (part1[0] == part2[0])
            {
                if (part1[1] < 4)
                    part1[1]++;
                else
                    part1[1] = 0;
                if (part2[1] < 4)
                    part2[1]++;
                else
                    part2[1] = 0;
            }
            else if (part1[1] == part2[1])
            {
                if (part1[0] < 4)
                    part1[0]++;
                else
                    part1[0] = 0;
                if (part2[0] < 4)
                    part2[0]++;
                else
                    part2[0] = 0;
            }
            else
            {
                int temp = part1[1];
                part1[1] = part2[1];
                part2[1] = temp;
            }
            Code = Code + matrix_arr[part1[0]][part1[1]]
                    + matrix_arr[part2[0]][part2[1]];
        }
        return Code;
    }

    public static void main(String[] args)
    {
        PlayfairCipherEncryption x = new PlayfairCipherEncryption();
        Scanner sc = new Scanner(System.in);
        System.out.println("Enter a keyword:");
        String keyword = sc.next();
        x.setKey(keyword);
        x.KeyGen();
        System.out
                .println("Enter word to encrypt: (Make sure length of message is even)");
        String key_input = sc.next();
        if (key_input.length() % 2 == 0)
        {
            System.out.println("Encryption: " + x.encryptMessage(key_input));
        }
        else
        {
            System.out.println("Message length should be even");
        }
        sc.close();
    }
}
```



## Kotlin

```scala
// version 1.0.5-2

enum class PlayfairOption {
    NO_Q,
    I_EQUALS_J
}

class Playfair(keyword: String, val pfo: PlayfairOption) {
    private val table: Array<CharArray> = Array(5, { CharArray(5) })  // 5 x 5 char array

    init {
        // build table
        val used = BooleanArray(26)  // all elements false
        if (pfo == PlayfairOption.NO_Q)
            used[16] = true  // Q used
        else
            used[9]  = true  // J used
        val alphabet = keyword.toUpperCase() + "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        var i = 0
        var j = 0
        var c: Char
        var d: Int
        for (k in 0 until alphabet.length) {
            c = alphabet[k]
            if (c !in 'A'..'Z') continue
            d = c.toInt() - 65
            if (!used[d]) {
                table[i][j] = c
                used[d] = true
                if (++j == 5) {
                    if (++i == 5) break // table has been filled
                    j = 0
                }
            }
        }
    }

    private fun getCleanText(plainText: String): String {
        val plainText2 = plainText.toUpperCase()  // ensure everything is upper case
        // get rid of any non-letters and insert X between duplicate letters
        var cleanText = ""
        var prevChar = '\u0000'  // safe to assume null character won't be present in plainText
        var nextChar: Char
        for (i in 0 until plainText2.length) {
            nextChar = plainText2[i]
            // It appears that Q should be omitted altogether if NO_Q option is specified - we assume so anyway
            if (nextChar !in 'A'..'Z' || (nextChar == 'Q' && pfo == PlayfairOption.NO_Q)) continue
            // If I_EQUALS_J option specified, replace J with I
            if (nextChar == 'J' && pfo == PlayfairOption.I_EQUALS_J) nextChar = 'I'
            if (nextChar != prevChar)
                cleanText += nextChar
            else
                cleanText += "X" + nextChar
            prevChar = nextChar
        }
        val len = cleanText.length
        if (len % 2 == 1)  {  // dangling letter at end so add another letter to complete digram
            if (cleanText[len - 1] != 'X')
                cleanText += 'X'
            else
                cleanText += 'Z'
        }
        return cleanText
    }

    private fun findChar(c: Char): Pair<Int, Int> {
       for (i in 0..4)
           for (j in 0..4)
               if (table[i][j] == c) return Pair(i, j)
       return Pair(-1, -1)
    }

    fun encode(plainText: String): String {
        val cleanText = getCleanText(plainText)
        var cipherText = ""
        val length = cleanText.length
        for (i in 0 until length step 2) {
            val (row1, col1) = findChar(cleanText[i])
            val (row2, col2) = findChar(cleanText[i + 1])
            cipherText += when {
                row1 == row2 -> table[row1][(col1 + 1) % 5].toString() + table[row2][(col2 + 1) % 5]
                col1 == col2 -> table[(row1 + 1) % 5][col1].toString() + table[(row2 + 1) % 5][col2]
                else         -> table[row1][col2].toString() + table[row2][col1]
            }
            if (i < length - 1) cipherText += " "
        }
        return cipherText
    }

    fun decode(cipherText: String): String {
        var decodedText = ""
        val length = cipherText.length
        for (i in 0 until length step 3) {  // cipherText will include spaces so we need to skip them
            val (row1, col1) = findChar(cipherText[i])
            val (row2, col2) = findChar(cipherText[i + 1])
            decodedText += when {
                row1 == row2 -> table[row1][if (col1 > 0) col1 - 1 else 4].toString() + table[row2][if (col2 > 0) col2 - 1 else 4]
                col1 == col2 -> table[if (row1 > 0) row1- 1 else 4][col1].toString() + table[if (row2 > 0) row2 - 1 else 4][col2]
                else         -> table[row1][col2].toString() + table[row2][col1]
            }
            if (i < length - 1) decodedText += " "
        }
        return decodedText
    }

    fun printTable() {
        println("The table to be used is :\n")
        for (i in 0..4) {
            for (j in 0..4) print(table[i][j] + " ")
            println()
        }
    }
}

fun main(args: Array<String>) {
    print("Enter Playfair keyword : ")
    val keyword: String = readLine()!!
    var ignoreQ: String
    do {
         print("Ignore Q when buiding table  y/n : ")
         ignoreQ = readLine()!!.toLowerCase()
    }
    while (ignoreQ != "y" && ignoreQ != "n")
    val pfo = if (ignoreQ == "y") PlayfairOption.NO_Q else PlayfairOption.I_EQUALS_J
    val playfair = Playfair(keyword, pfo)
    playfair.printTable()
    print("\nEnter plain text : ")
    val plainText: String = readLine()!!
    val encodedText = playfair.encode(plainText)
    println("\nEncoded text is : $encodedText")
    val decodedText = playfair.decode(encodedText)
    println("Decoded text is : $decodedText")
}
```


```txt

Enter Playfair keyword : Playfair example
Ignore Q when buiding table  y/n : n
The table to be used is :

P L A Y F
I R E X M
B C D G H
K N O Q S
T U V W Z

Enter plain text : Hide the gold...in the TREESTUMP!!!!

Encoded text is : BM OD ZB XD NA BE KU DM UI XM MO UV IF
Decoded text is : HI DE TH EG OL DI NT HE TR EX ES TU MP

```



## NetRexx


```NetRexx
/* NetRexx */
options replace format comments java crossref symbols nobinary

-- input arguments:
--   encipher/decipher IQ-switch key text
--   encipher/decipher -
--     a character E, D (default E; . is an alias for E)
--   IQ-switch         -
--     a character I, Q (default I for I==J, Q for exclude Q; . is an alias for I)
--   key               -
--     the cipher key - default plugh_xyzzy (really just PLUGHXYZ)
--   text              -
--     the text to encipher/decipher

runSample(arg)
return

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method cipher(d_, km, digraphs) public static
  if d_.upper() = 'D' then d_ = -1 -- encode or decode adjustment
  else                     d_ = +1
  cipherText = ''
  loop dw = 1 to digraphs.words()
    -- process the digraphs one at a time
    digraph = digraphs.word(dw)
    cl = ''
    cr = ''
    -- get each letter from the digraph
    parse digraph dl +1 dr
    loop r_ = 1 to km[0] while (cl cr).words() < 4
      -- locate the row/col of each letter in the cipher matrix
      clx = km[r_].wordpos(dl)
      crx = km[r_].wordpos(dr)
      if clx > 0 then cl = r_ clx
      if crx > 0 then cr = r_ crx
      end r_

    -- process the digraph's rows, columns or rectangles
    select
      when cl.word(1) = cr.word(1) then do
        -- a row
        rx  = cl.word(1)
        clx = cl.word(2) + d_
        crx = cr.word(2) + d_
        if clx > 5 then clx = 1 -- wrap
        if crx > 5 then crx = 1
        if clx < 1 then clx = 5
        if crx < 1 then crx = 5
        cy = km[rx].word(clx) || km[rx].word(crx)
        cipherText = cipherText cy
        end
      when cl.word(2) = cr.word(2) then do
        -- a column
        cx  = cl.word(2)
        rlx = cl.word(1) + d_
        rrx = cr.word(1) + d_
        if rlx > 5 then rlx = 1 -- wrap
        if rrx > 5 then rrx = 1
        if rlx < 1 then rlx = 5
        if rrx < 1 then rrx = 5
        cy = km[rlx].word(cx) || km[rrx].word(cx)
        cipherText = cipherText cy
        end
      otherwise do
        -- a rectangle
        r1  = cl.word(1)
        r2  = cr.word(1)
        cy = km[r1].word(cr.word(2)) || km[r2].word(cl.word(2))
        cipherText = cipherText cy
        end
      end
    end dw
  return cipherText.strip()

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method encipher(km, digraphs) public static
  return cipher('E', km, digraphs)

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method decipher(km, digraphs) public static
  return cipher('D', km, digraphs)

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method getDigraphs(text, IorQ, ED) private static
  a2z = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
  if ED.upper() \= 'D' then eks = 'X'
  else                      eks = ''
  tp = text.upper().translate('', a2z)
  text = text.upper().translate(' '.copies(tp.length()), tp).space(0)
  rtext = ''
  ll = ''

  loop while text \= ''
    parse text lc +1 text
    if ll \= lc then rtext = rtext || lc
    else             rtext = rtext || eks || lc
    ll = lc
    end

  -- I == J or remove Q fixup
  if IorQ \= 'Q' then
    parse 'J I' IorQ sc .
  else
    sc = ''

  rtext = rtext.changestr(IorQ, sc)
  if rtext.length() // 2 \= 0 then rtext = rtext || eks
  digraphs = ''
  loop while rtext \= ''
    parse rtext digraph +2 rtext
    digraphs = digraphs digraph
    end
  return digraphs.space()

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method getKey(key, IorQ) private static
  a2z = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
  kp = (key || a2z).upper()
  kr = ''
  loop while kp \= ''
    parse kp xx +1 kp
    if \xx.datatype('u') then iterate
    kr = kr xx
    kp = kp.changestr(xx, '')
    end

  if IorQ = 'I' | IorQ = 'J' | IorQ = '' then
    kr = kr.changestr('J', '')
  else
    kr = kr.changestr('Q', '')

  return kr.space()

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method getKeyMatrix(kr) private static
  km = ''
  loop r_ = 1 while kr \= ''
    parse kr kp +10 kr
    km[0]  = r_
    km[r_] = kp
    end r_
  return km

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method runSample(arg) private static
  parse arg ciph IorQ key text
  if ciph = '' | ciph = '.' then ciph = 'E'
  if IorQ = '' | IorQ = '.' then IorQ = 'I'
  if key  = '' | key  = '.' then key  = 'plugh_xyzzy'
  if text = '' | text = '.' then text = 'NetRexx; not just a programing language for kings & queens!'

  kr = getKey(key.space(0), IorQ)
  km = getKeyMatrix(kr)

  digraphs = getDigraphs(text, IorQ, ciph)
  -- fixup for a Q in the text when Q is excluded (substitute K for Q)
  if IorQ.upper = 'Q' then digraphs = digraphs.changestr('Q', 'K')
  if ciph = 'E' then say text
  say digraphs
  if ciph.upper() = 'E' then
    say encipher(km, digraphs)
  else
    say decipher(km, digraphs)

  return

```

```txt

$ java -cp .:.. RPlayfairCipher00
NetRexx; not just a programing language for kings & queens!
NE TR EX XX NO TI US TA PR OG RA MI NG LA NG UA GE FO RK IN GS QU EX EN SX
TN VS CZ YY OQ WE LT VZ XP VA VX QD OU GY OU GZ UF OV PR EQ LV NH CZ NT RY

$ java -cp .:.. RPlayfairCipher00 d . . TN VS CZ YY OQ WE LT VZ XP VA VX QD OU GY OU GZ UF OV PR EQ LV NH CZ NT RY
TN VS CZ YY OQ WE LT VZ XP VA VX QD OU GY OU GZ UF OV PR EQ LV NH CZ NT RY
NE TR EX XX NO TI US TA PR OG RA MI NG LA NG UA GE FO RK IN GS QU EX EN SX

$ java -cp .:.. RPlayfairCipher00 e q playfair Hide the gold in the tree stump\!\!
Hide the gold in the tree stump!!
HI DE TH EG OL DI NT HE TR EX ES TU MP
EB IK OK GH NA IR OM JG ND JU JM MZ UI

$ java -cp .:.. RPlayfairCipher00 d q playfair EB IK OK GH NA IR OM JG ND JU JM MZ UI
EB IK OK GH NA IR OM JG ND JU JM MZ UI
HI DE TH EG OL DI NT HE TR EX ES TU MP

```



## ooRexx


```oorexx
/*---------------------------------------------------------------------
* REXX program implements a PLAYFAIR cipher (encryption & decryption).
* 11.11.2013 Walter Pachl revamped, for ooRexx, the REXX program
*                the logic of which was devised by Gerard Schildberger
* Invoke as rexx pf O abcd efgh ( phrase to be processed
* Defaults:           'Playfair example.'
*                   J
*                                 'Hide the gold in the tree stump'
* Major changes: avoid language elements not allowed in ooRexx
*                show use of a.[expr1,expr2]
*                allow key to be more than one word
*                add restriction of using X or Q in text
* 12.11.2013 change order of arguments
*            and comment the use of a.[expr1,expr2]
*            Program should run on all Rexxes that have changestr-bif
*--------------------------------------------------------------------*/
  Parse Upper Version v
  oorexx=pos('OOREXX',v)>0

  Parse Arg omit oldk '(' text
  If omit='' Then omit='J'
  If oldk='' Then oldk='Playfair example.'
  If text=''            Then text='Hide the gold in the tree stump!!'

  newkey=scrub(oldk,1)
  newtext=scrub(text)
  If newtext==''        Then Call err 'TEXT is empty or has no letters'
  If length(omit)\==1   Then Call err 'OMIT letter must be only one letter'
  If\datatype(omit,'M') Then Call err 'OMIT letter must be a Latin alphabet letter.'
  omit=translate(omit)
  cant='must not contain the "OMIT" character: ' omit
  If pos(omit,newtext)\==0 Then Call err 'TEXT' cant
  If pos(omit,newkey)\==0  Then Call err 'cipher key' cant
  abc='abcdefghijklmnopqrstuvwxyz'
  abcu=translate(abc)                 /* uppercase alphabet           */
  abcx=space(translate(abcu,,omit),0) /*elide OMIT char from alphabet */
  xx='X'                            /* char used for double characters*/
  If omit==xx Then
    xx='Q'
  If pos(xx,newtext)>0 Then
    Call err 'Sorry,' xx 'is not allowed in text'
  If length(newkey)<3 Then
    Call err 'cipher key is too short, must be at least 3 different letters'
  abcx=space(translate(abcx,,newkey),0) /*remove any cipher characters   */
  grid=newkey||abcx                     /* only first  25  chars are used*/
  Say 'old cipher key: ' strip(oldk)
  padl=14+2
  pad=left('',padl)
  Say 'new cipher key: ' newkey
  padx=left('',padl,"-")'Playfair'
  Say '     omit char: ' omit       /* [?]  lowercase of double char. */
  Say '   double char: ' xx
  lxx=translate(xx,abc,abcu)
  Say ' original text: ' strip(text)/* [?]  doubled version of  Lxx.  */
  Call show 'cleansed',newtext
  lxxlxx=lxx||lxx
  n=0                               /* number of grid characters used.*/
  Do row=1 For 5                    /* build array of individual cells*/
    Do col=1 For 5
      n=n+1
      a.row.col=substr(grid,n,1)
      If row==1 Then
        a.0.col=a.1.col
      If col==5 Then Do
        a.row.6=a.row.1
        a.row.0=a.row.5
        End
      If row==5 Then Do
        a.6.col=a.1.col
        a.0.col=a.5.col
        End
      End
    End

  etext=playfair(newtext,1)
  Call show 'encrypted',etext
  ptext=playfair(etext,-1)
  Call show 'plain',ptext
  qtext=changestr(xx||xx,ptext,lxx)     /*change doubled doublechar-?sing*/
  qtext=changestr(lxx||xx,qtext,lxxlxx) /*change Xx --? lowercase dblChar*/
  qtext=space(translate(qtext,,xx),0)   /*remove char used for "doubles."*/
  qtext=translate(qtext)                /*reinstate the use of upperchars*/
  Call show 'decoded',qtext
  Say ' original text: ' newtext        /* ·· and show the original text.*/
  Say ''
  Exit

playfair:
/*---------------------------------------------------------------------
* encode (e=1) or decode (e=-1) the given text (t) using the grid
*--------------------------------------------------------------------*/
  Arg t,e
  d=''
  If e=1 Then Do
    Do k=1 By 1 Until c=''
      c=substr(t,k,1)
      If substr(t,k+1,1)==c Then
        t=left(t,k)||lxx||substr(t,k+1)
      End
    End
  t=translate(t)
  Do j=1 By 2 To length(t)
    c2=strip(substr(t,j,2))
    If length(c2)==1 Then
      c2=c2||xx                         /* append X or Q char, rule 1*/
    Call LR
    Select
      /*- This could/should be used on ooRexx -------------------------
      When rowl==rowr Then c2=a.[rowl,coll+e]a.[rowr,colr+e] /*rule 2*/
      When coll==colr Then c2=a.[rowl+e,coll]a.[rowr+e,colr] /*rule 3*/
      *--------------------------------------------------------------*/
      When rowl==rowr Then c2=aa(rowl,coll+e)aa(rowr,colr+e) /*rule 2*/
      When coll==colr Then c2=aa(rowl+e,coll)aa(rowr+e,colr) /*rule 3*/
      Otherwise            c2=a.rowl.colr||a.rowr.coll       /*rule 4*/
      End
    d=d||c2
    End
  Return d

aa:
/*---------------------------------------------------------------------
* ooRexx allows to use a.[rowl,coll+e]
* this function can be removed when ooRexx syntax is used to access a.
*--------------------------------------------------------------------*/
  Parse Arg xrow,xcol
  Return a.xrow.xcol

err:
/*---------------------------------------------------------------------
* Exit with an error message
*--------------------------------------------------------------------*/
  Say
  Say '***error!***' arg(1)
  Say
  Exit 13

lr:
/*---------------------------------------------------------------------
* get grid positions of the 2 characters
*--------------------------------------------------------------------*/
  Parse Value rowcol(left(c2,1)) with rowl coll
  Parse Value rowcol(right(c2,1)) with rowr colr
  Return

rowcol: procedure Expose grid
/*---------------------------------------------------------------------
* compute row and column of the given character in the 5x5 grid
*--------------------------------------------------------------------*/
  Parse Arg c
  p=pos(c,grid)
  col=(p-1)//5+1
  row=(4+p)%5
  Return row col

show:
/*---------------------------------------------------------------------
* Show heading and text
*--------------------------------------------------------------------*/
  Arg,y
  Say
  Say right(arg(1) 'text: ',padl) digram(arg(2))
  result=space(arg(2),0)
  If arg(1)='decoded' Then Do
    result=strip(result,'T',xx)
    End
  Say pad result
  Return

scrub: Procedure
/*---------------------------------------------------------------------
* Remove all non-letters from the given string, uppercase letters
* and, if unique=1 remove duplicates
* 'aB + c1Bb' -> 'ABCBB' or 'ABC', respectively
*--------------------------------------------------------------------*/
  Arg xxx,unique                    /* ARG caps all args             */
  d=''
  used.=0
  Do While xxx<>''
    Parse Var xxx c +1 xxx
    If datatype(c,'U') Then
      If (unique=1 & pos(c,d)=0) |,
          unique<>1 Then
        d=d||c
    End
  Return d

digram: Procedure
/*---------------------------------------------------------------------
* Return the given string as character pairs separated by blanks
* 'ABCDEF' -> 'AB CD EF'
* 'ABCDE'  -> 'AB CD E'
*--------------------------------------------------------------------*/
  Parse Arg x
  d=''
  Do j=1 By 2 To length(x)
    d=d||substr(x,j,2)' '
    End
  Return strip(d)
```

Output (sample):

```txt
old cipher key:  this is my little key
new cipher key:  THISMYLEK
     omit char:  X
   double char:  Q
 original text:  to be or not to bee

 cleansed text:  TO BE OR NO TT OB EE
                 TOBEORNOTTOBEE

encrypted text:  IJ DY JV OP MJ IJ DY OA JJ
                 IJDYJVOPMJIJDYOAJJ

    plain text:  TO BE OR NO TQ TO BE QE QQ
                 TOBEORNOTQTOBEQEQQ

  decoded text:  TO BE OR NO TT OB EE Q
                 TOBEORNOTTOBEE
 original text:  TOBEORNOTTOBEE
```



## Perl

```perl
use Math::Cartesian::Product;

# Pregenerate all forward and reverse translations
sub playfair {
    our($key,$from) = @_;
    $from //= 'J';
    our $to = $from eq 'J' ? 'I' : '';
    my(%ENC,%DEC,%seen,@m);

    sub canon {
        my($str) = @_;
        $str =~ s/[^[:alpha:]]//g;
        $str =~ s/$from/$to/gi;
        uc $str;
    }

    my @uniq = grep { ! $seen{$_}++ } split '', canon($key . join '', 'A'..'Z');
    while (@uniq) { push @m, [splice @uniq, 0, 5] }

    # Map pairs in same row.
    for my $r (@m)  {
        for my $x (cartesian {@_} [0..4], [0..4]) {
        my($i,$j) = @$x;
        next if $i == $j;
        $ENC{ @$r[$i] . @$r[$j] } =  @$r[($i+1)%5] . @$r[($j+1)%5];
        }
    }

    # Map pairs in same column.
    for my $c (0..4) {
        my @c = map { @$_[$c] } @m;
        for my $x (cartesian {@_} [0..4], [0..4]) {
        my($i,$j) = @$x;
        next if $i == $j;
        $ENC{ $c[$i] . $c[$j] } = $c[($i+1)%5] . $c[($j+1)%5];
        }
    }

    # Map pairs with cross-connections.
    for my $x (cartesian {@_} [0..4], [0..4], [0..4], [0..4]) {
        my($i1,$j1,$i2,$j2) = @$x;
        next if $i1 == $i2 or $j1 == $j2;
        $ENC{ $m[$i1][$j1] . $m[$i2][$j2] } = $m[$i1][$j2] . $m[$i2][$j1];
    }

    # Generate reverse translations.
     while (my ($k, $v) = each %ENC) { $DEC{$v} = $k }

    # Return code-refs for encoding/decoding routines
    return
    sub { my($red) = @_; # encode
        my $str = canon($red);

        my @list;
        while ($str =~ /(.)(?(?=\1)|(.?))/g) {
            push @list, substr($str,$-[0], $-[2] ? 2 : 1);
        }
        join ' ', map { length($_)==1 ? $ENC{$_.'X'} : $ENC{$_} } @list;
    },
    sub { my($black) = @_; # decode
        join ' ', map { $DEC{$_} } canon($black) =~ /../g;
    }
}

my($encode,$decode) = playfair('Playfair example');

my $orig  = "Hide the gold in...the TREESTUMP!!!";
my $black = &$encode($orig);
my $red   = &$decode($black);
print " orig:\t$orig\n";
print "black:\t$black\n";
print "  red:\t$red\n";
```

```txt
 orig:  Hide the gold in...the TREESTUMP!!!
black:  BM OD ZB XD NA BE KU DM UI XM MO UV IF
  red:  HI DE TH EG OL DI NT HE TR EX ES TU MP

```



## Perl 6

```perl6
# Instantiate a specific encoder/decoder.

sub playfair( $key,
              $from = 'J',
	      $to = $from eq 'J' ?? 'I' !! ''
) {

    sub canon($str) { $str.subst(/<-alpha>/,'', :g).uc.subst(/$from/,$to,:g) }

    # Build 5x5 matrix.
    my @m = canon($key ~ ('A'..'Z').join).comb.unique.map:
     -> $a,$b,$c,$d,$e { [$a,$b,$c,$d,$e] }

    # Pregenerate all forward translations.
    my %ENC = gather {
	# Map pairs in same row.
	for @m -> @r {
	    for ^@r X ^@r -> (\i,\j) {
		next if i == j;
		take @r[i] ~ @r[j] => @r[(i+1)%5] ~ @r[(j+1)%5];
	    }
	}

	# Map pairs in same column.
	for ^5 -> $c {
	    my @c = @m.map: *.[$c];
	    for ^@c X ^@c -> (\i,\j) {
		next if i == j;
		take @c[i] ~ @c[j] => @c[(i+1)%5] ~ @c[(j+1)%5];
	    }
	}

	# Map pairs with cross-connections.
	for ^5 X ^5 X ^5 X ^5 -> (\i1,\j1,\i2,\j2) {
	    next if i1 == i2 or j1 == j2;
	    take @m[i1][j1] ~ @m[i2][j2] => @m[i1][j2] ~ @m[i2][j1];
	}
    }

    # Generate reverse translations.
    my %DEC = %ENC.invert;

    return
	anon sub enc($red) {
	    my @list = canon($red).comb(/(.) (.?) <?{ $1 ne $0 }>/);
	    ~@list.map: { .chars == 1 ?? %ENC{$_~'X'} !! %ENC{$_} }
	},
	anon sub dec($black) {
	    my @list = canon($black).comb(/../);
	    ~@list.map: { %DEC{$_} }
	}
}

my (&encode,&decode) = playfair 'Playfair example';

my $orig = "Hide the gold in...the TREESTUMP!!!";
say " orig:\t$orig";

my $black = encode $orig;
say "black:\t$black";

my $red = decode $black;
say "  red:\t$red";
```

```txt
 orig:	Hide the gold in...the TREESTUMP!!!
black:	BM OD ZB XD NA BE KU DM UI XM MO UV IF
  red:	HI DE TH EG OL DI NT HE TR EX ES TU MP
```



## Phix

Originally translated from Kotlin, now uses a combined routine (playfair) for encoding and decoding, and direct char lookups, and x removal.

```Phix
sequence table,
         findchar

type pfoption(integer option)
    return find(option,"QJ")!=0
end type

pfoption pfo -- 'Q' or 'J'

procedure build_table(string keyword, integer option)
-- option should be 'Q' to ignore Q, or 'J' to replace Js with I
    pfo = option
    table = repeat(repeat(' ',5),5)
    findchar = repeat(0,26)
    findchar[pfo-'A'+1] = {0,0}
    -- (note that any pfo (J/Q) in keyword are simply ignored)
    string alphabet = upper(keyword) & "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    integer i=1, j=1
    for k=1 to length(alphabet) do
        integer c = alphabet[k]
        if c>='A' and c<='Z' then
            integer d = c-'A'+1
            if findchar[d]=0 then
                table[i][j] = c
                findchar[d] = {i,j}
                j += 1
                if j=6 then
                    i += 1
                    if i=6 then exit end if -- table filled
                    j = 1
                end if
            end if
        end if
    end for
end procedure

function clean_text(string plaintext)
-- get rid of any non-letters and insert X between duplicate letters
    plaintext = upper(plaintext)
    string cleantext = ""
    integer prevChar = -1
    for i=1 to length(plaintext) do
        integer nextChar = plaintext[i]
        if nextChar>='A' and nextChar<='Z'
        and (nextChar!='Q' or pfo!='Q') then
            if nextChar='J' and pfo='J' then nextChar = 'I' end if
            if nextChar=prevChar then
                cleantext &= 'X'
            end if
            cleantext &= nextChar
            prevChar = nextChar
        end if
    end for
    if remainder(length(cleantext),2) then
        -- dangling letter at end so add another letter to complete digram
        cleantext &= iff(cleantext[$]='X'?'Z':'X')
    end if
    return cleantext
end function

function remove_x(string text)
    for i=2 to length(text)-1 do
        if text[i]='X'
        and ((text[i-1]=' ' and text[i-2]=text[i+1]) or
             (text[i+1]=' ' and text[i-1]=text[i+2])) then
            text[i] = '_'
        end if
    end for
    return text
end function

function playfair(string text, integer step, sequence d)
    string res = ""
    for i=1 to length(text) by step do
        integer {row1, col1} = findchar[text[i]-'A'+1],
                {row2, col2} = findchar[text[i+1]-'A'+1]
        if i>1 then res &= " " end if
        if row1=row2 then
            res &= table[row1][d[col1]] & table[row2][d[col2]]
        elsif col1=col2 then
            res &= table[d[row1]][col1] & table[d[row2]][col2]
        else
            res &= table[row1][col2] & table[row2][col1]
        end if
    end for
    return res
end function

constant p1 = {2,3,4,5,1},  -- easier than playing with mod(+1,5)
         m1 = {5,1,2,3,4}   --              ""          mod(-1,5)

function encode(string plaintext)
    return playfair(clean_text(plaintext),2,p1)
end function

function decode(string ciphertext)
    -- ciphertext includes spaces we need to skip, hence by 3
    return remove_x(playfair(ciphertext,3,m1))
end function

string keyword = "Playfair example"
build_table(keyword,'Q')
printf(1,"Playfair keyword : %s\n",{keyword})
printf(1,"Option: J=I or no Q (J/Q) : %s\n",pfo)
printf(1,"The table to be used is :\n\n%s\n\n",{join(table,"\n")})
string plaintext = "Hide the gold...in the TREESTUMP!!!!",
       encoded = encode(plaintext),
       decoded = decode(encoded)
printf(1,"The plain text : %s\n\n",{plaintext})
printf(1,"Encoded text is : %s\n",{encoded})
printf(1,"Decoded text is : %s\n",{decoded})
```

```txt

Playfair keyword : Playfair example
Option: J=I or no Q (J/Q) : Q
The table to be used is :

PLAYF
IREXM
BCDGH
JKNOS
TUVWZ

The plain text : Hide the gold...in the TREESTUMP!!!!

Encoded text is : BM ND ZB XD KY BE JV DM UI XM MN UV IF
Decoded text is : HI DE TH EG OL DI NT HE TR E_ ES TU MP

```



## Python

```python
from string import ascii_uppercase
from itertools import product
from re import findall

def uniq(seq):
    seen = {}
    return [seen.setdefault(x, x) for x in seq if x not in seen]

def partition(seq, n):
    return [seq[i : i + n] for i in xrange(0, len(seq), n)]


"""Instantiate a specific encoder/decoder."""
def playfair(key, from_ = 'J', to = None):
    if to is None:
        to = 'I' if from_ == 'J' else ''

    def canonicalize(s):
        return filter(str.isupper, s.upper()).replace(from_, to)

    # Build 5x5 matrix.
    m = partition(uniq(canonicalize(key + ascii_uppercase)), 5)

    # Pregenerate all forward translations.
    enc = {}

    # Map pairs in same row.
    for row in m:
        for i, j in product(xrange(5), repeat=2):
            if i != j:
                enc[row[i] + row[j]] = row[(i + 1) % 5] + row[(j + 1) % 5]

    # Map pairs in same column.
    for c in zip(*m):
        for i, j in product(xrange(5), repeat=2):
            if i != j:
                enc[c[i] + c[j]] = c[(i + 1) % 5] + c[(j + 1) % 5]

    # Map pairs with cross-connections.
    for i1, j1, i2, j2 in product(xrange(5), repeat=4):
        if i1 != i2 and j1 != j2:
            enc[m[i1][j1] + m[i2][j2]] = m[i1][j2] + m[i2][j1]

    # Generate reverse translations.
    dec = dict((v, k) for k, v in enc.iteritems())

    def sub_enc(txt):
        lst = findall(r"(.)(?:(?!\1)(.))?", canonicalize(txt))
        return " ".join(enc[a + (b if b else 'X')] for a, b in lst)

    def sub_dec(encoded):
        return " ".join(dec[p] for p in partition(canonicalize(encoded), 2))

    return sub_enc, sub_dec


(encode, decode) = playfair("Playfair example")
orig = "Hide the gold in...the TREESTUMP!!!"
print "Original:", orig
enc = encode(orig)
print "Encoded:", enc
print "Decoded:", decode(enc)
```

```txt
Original: Hide the gold in...the TREESTUMP!!!
Encoded: BM OD ZB XD NA BE KU DM UI XM MO UV IF
Decoded: HI DE TH EG OL DI NT HE TR EX ES TU MP
```



## REXX

Quite a bit of the REXX code deals with error checking, accepting arguments, and displaying the options used, and displaying input and output.

For ease of viewing and comparing, the output is in capitalized digraphs (which are really ''digrams'') as well as the original input(s).

Thanks to Walter Pachl, this program is now sensitive of using a suitable ''double character'' when   '''X'''   is present in the cipher key.

Also, more thanks are due to Walter Pachl for finding that the cipher key can't contain the OMIT character.

A fair amount of code was added to massage the decrypted encryption to remove doubled   '''X'''es   so as to match the original text

(this is the ''possible text'' part of the REXX code).

```rexx
/*REXX program implements a  PLAYFAIR cipher  (encryption & decryption).*/
@abc='abcdefghijklmnopqrstuvwxyz';  @abcU=@abc  /*lower and upper ABC's.*/
parse arg omit key '(' text            /*TEXT is the phrase to be used. */  ;oldK=key  /*save old.*/
if key =='' | key ==','  then do; key='Playfair example.'; oldK=key "   ◄───using the default."; end
if omit=='' | omit==','  then omit='J' /*the  "omitted"  character.     */
if text=''               then text='Hide the gold in the tree stump!!'   /*default.*/
newKey =scrub(key , 1)                 /*scrub old cipher key──► newKey */
newText=scrub(text   )                 /*  "    "     text   ──► newText*/
if newText==''           then call err 'TEXT is empty or has no letters'
if length(omit)\==1      then call err 'OMIT letter must be only one letter'
if \datatype(omit,'M')   then call err 'OMIT letter must be a Latin alphabet letter.'
upper omit @abcU                       /*uppercase OMIT char & alphabet.*/
                         cant='can''t contain the "OMIT" character: ' omit
if pos(omit,newText)\==0 then call err 'TEXT'        cant
if pos(omit,newKey) \==0 then call err 'cipher key'  cant
fill=space(translate(@abcU,, omit), 0) /*elide OMIT char from alphabet. */
xx='X';   if omit==xx  then xx='Q'     /*char used for double characters*/
if length(newKey)<3  then call err 'cipher key is too short, must be ≥3 unique characters.'
fill=space(translate(fill,,newKey),0)  /*remove any cipher characters.  */
grid=newKey || fill                    /*only first  25  chars are used.*/
say 'old cipher key: ' strip(oldK)     ; padL=14+2;   pad=left('',padL)
say 'new cipher key: ' newKey          ; padX=left('',padL,"═")'Playfair'
say '     omit char: ' omit            /* [↓]  lowercase of double char.*/
say '   double char: ' xx              ; Lxx=translate(xx, @abc, @abcU)
say ' original text: ' strip(text)     /* [↓]  doubled version of  Lxx. */
call show 'cleansed', newText          ; LxxLxx=Lxx || Lxx
#=0                                    /*number of grid characters used.*/
           do row   =1  for 5          /*build array of individual cells*/
              do col=1  for 5;     #=#+1;       @.row.col=substr(grid,#,1)
              if row==1  then      @.0.col=@.1.col
              if col==5  then do;  @.row.6=@.row.1;  @.row.0=@.row.5;  end
              if row==5  then do;  @.6.col=@.1.col;  @.0.col=@.5.col;  end
              end   /*col*/
           end      /*row*/

eText=.Playfair(newText, 1);           call show 'encrypted' , eText
pText=.Playfair(eText     );           call show 'plain'     , pText
qText=changestr(xx ||xx,pText,Lxx)     /*change doubled doublechar─►sing*/
qText=changestr(Lxx||xx,qText,LxxLxx)  /*change Xx ──► lowercase dblChar*/
qText=space(translate(qText,,xx),0)    /*remove char used for "doubles."*/
upper qText                            /*reinstate the use of upperchars*/
if length(qText)\==length(pText)  then call show 'possible', qText
say ' original text: ' newText;   say  /*··· and show the original text.*/
if qtext==newText  then say padx 'encryption──► decryption──► encryption worked.'
exit                                   /*stick a fork in it, we're done.*/
/*──────────────────────────────────one─line subroutines───────────────────────────────*/
@@:    parse arg Xrow,Xcol;                         return  @.Xrow.Xcol
err:   say;   say '***error!***'  arg(1);           say;    exit 13
LR:    rowL=row(left(__,1)); colL=_; rowR=row(right(__,1)); colR=_; return length(__)
row:   ?=pos(arg(1),grid);        _=(?-1)//5+1;     return  (4+?)%5
show:  arg ,y; say; say right(arg(1) 'text: ',padL) digram(y); say pad space(y,0); return
/*──────────────────────────────────SCRUB subroutine────────────────────*/
scrub: procedure;  arg xxx,unique; xxx=space(xxx,0)  /*ARG caps all args*/
$=;       do j=1  for length(xxx);     _=substr(xxx,j,1)
          if unique==1  then  if  pos(_,$)\==0  then iterate   /*unique?*/
          if datatype(_,'M')  then $=$||_     /*only use Latin letters. */
          end   /*j*/
return $
/*──────────────────────────────────DIGRAM subroutine───────────────────*/
digram: procedure;  parse arg x;  $=;      do j=1  by 2  to length(x)
                                           $=$ || substr(x,j,2)' '
                                           end   /*j*/
return strip($)
/*──────────────────────────────────.PLAYFAIR subroutine────────────────*/
.Playfair: arg T,encrypt;        i=-1;    if encrypt==1  then i=1;      $=
        do k=1  while  i==1;     _=substr(T,k,1);     if _==' '  then leave
        if _==substr(T,k+1 ,1)   then T=left(T,k) || Lxx || substr(T,k+1)
        end   /*k*/
upper T
  do j=1  by 2  to length(T);    __=strip(substr(T,j,2))
  if LR()==1  then __=__ || xx;  call LR    /*append X or Q char, rule 1*/
    select
    when rowL==rowR  then __=@@(rowL,  colL+i)@@(rowR,  colR+i) /*rule 2*/
    when colL==colR  then __=@@(rowL+i,colL  )@@(rowR+i,colR)   /*rule 3*/
    otherwise             __=@@(rowL,  colR  )@@(rowR,  colL)   /*rule 4*/
    end   /*select*/
  $=$ || __
  end   /*j*/
return $
```

Some older REXXes don't have a '''changestr''' bif, so one is included here ──► [[CHANGESTR.REX]].



'''output''' when using the default inputs:

```txt

old cipher key:  Playfair example.    ◄───using the default.
new cipher key:  PLAYFIREXM
     omit char:  J
   double char:  X
 original text:  Hide the gold in the tree stump!!

 cleansed text:  HI DE TH EG OL DI NT HE TR EE ST UM P
                 HIDETHEGOLDINTHETREESTUMP

encrypted text:  BM OD ZB XD NA BE KU DM UI XM MO UV IF
                 BMODZBXDNABEKUDMUIXMMOUVIF

    plain text:  HI DE TH EG OL DI NT HE TR EX ES TU MP
                 HIDETHEGOLDINTHETREXESTUMP

 possible text:  HI DE TH EG OL DI NT HE TR EE ST UM P
                 HIDETHEGOLDINTHETREESTUMP
 original text:  HIDETHEGOLDINTHETREESTUMP

════════════════Playfair encryption──► decryption──► encryption worked.

```

'''output''' when using the input of:   <tt> x stuvw (myteest </tt>

```txt

old cipher key:  stuvw
new cipher key:  STUVW
     omit char:  X
   double char:  Q
 original text:  myteest

 cleansed text:  MY TE ES T
                 MYTEEST

encrypted text:  NR WB ZB TU
                 NRWBZBTU

    plain text:  MY TE QE ST
                 MYTEQEST

 possible text:  MY TE ES T
                 MYTEEST
 original text:  MYTEEST

════════════════Playfair encryption──► decryption──► encryption worked.

```



## Sidef

```ruby
func playfair(key, from = 'J', to = (from == 'J' ? 'I' : '')) {

    func canon(str) {
        str.gsub(/[^[:alpha:]]/, '').uc.gsub(from, to)
    }

    var m = canon(key + ('A'..'Z' -> join)).chars.uniq.slices(5)

    var :ENC = gather {
        m.each { |r|
            for i,j in (^r ~X ^r) {
                i == j && next
                take(Pair("#{r[i]}#{r[j]}", "#{r[(i+1)%5]}#{r[(j+1)%5]}"))
            }
        }

        ^5 -> each { |k|
            var c = m.map {|a| a[k] }
            for i,j in (^c ~X ^c) {
                i == j && next
                take(Pair("#{c[i]}#{c[j]}", "#{c[(i+1)%5]}#{c[(j+1)%5]}"))
            }
        }

        cartesian([^5, ^5, ^5, ^5], {|i1,j1,i2,j2|
            i1 == i2 && next
            j1 == j2 && next
            take(Pair("#{m[i1][j1]}#{m[i2][j2]}", "#{m[i1][j2]}#{m[i2][j1]}"))
        })
    }.map { (.key, .value) }...

    var DEC = ENC.flip

    func enc(red) {
        gather {
            var str = canon(red)
            while (var m = (str =~ /(.)(?(?=\1)|(.?))/g)) {
                take("#{m[0]}#{m[1] == '' ? 'X' : m[1]}")
            }
        }.map { ENC{_} }.join(' ')
    }

    func dec(black) {
        canon(black).split(2).map { DEC{_} }.join(' ')
    }

    return(enc, dec)
}

var (encode, decode) = playfair('Playfair example')

var orig = "Hide the gold in...the TREESTUMP!!!"
say " orig:\t#{orig}"

var black = encode(orig)
say "black:\t#{black}"

var red = decode(black)
say "  red:\t#{red}"
```

```txt

 orig:	Hide the gold in...the TREESTUMP!!!
black:	BM OD ZB XD NA BE KU DM UI XM MO UV IF
  red:	HI DE TH EG OL DI NT HE TR EX ES TU MP

```



## SQL


```sql

--Clean up previous run
IF EXISTS (SELECT *
           FROM   SYS.TYPES
           WHERE  NAME = 'FairPlayTable')
  DROP TYPE FAIRPLAYTABLE

--Set Types
CREATE TYPE FAIRPLAYTABLE AS TABLE (LETTER VARCHAR(1), COLID INT, ROWID INT)

GO

--Configuration Variables
DECLARE @KEYWORD VARCHAR(25) = 'CHARLES' --Keyword for encryption
DECLARE @INPUT VARCHAR(MAX) = 'Testing Seeconqz' --Word to be encrypted
DECLARE @Q INT = 0 -- Q removed?
DECLARE @ENCRYPT INT = 1 --Encrypt?
--Setup Variables
DECLARE @WORDS TABLE
  (
     WORD_PRE  VARCHAR(2),
     WORD_POST VARCHAR(2)
  )
DECLARE @T_TABLE FAIRPLAYTABLE
DECLARE @NEXTLETTER CHAR(1)
DECLARE @WORD VARCHAR(2),
        @COL1 INT,
        @COL2 INT,
        @ROW1 INT,
        @ROW2 INT,
        @TMP  INT
DECLARE @SQL     NVARCHAR(MAX) = '',
        @COUNTER INT = 1,
        @I       INT = 1
DECLARE @COUNTER_2 INT = 1

SET @INPUT = REPLACE(@INPUT, ' ', '')
SET @KEYWORD = UPPER(@KEYWORD)

DECLARE @USEDLETTERS VARCHAR(MAX) = ''
DECLARE @TESTWORDS VARCHAR(2),
        @A         INT = 0

WHILE @COUNTER_2 <= 5
  BEGIN
      WHILE @COUNTER <= 5
        BEGIN
            IF LEN(@KEYWORD) > 0
              BEGIN
                  SET @NEXTLETTER = LEFT(@KEYWORD, 1)
                  SET @KEYWORD = RIGHT(@KEYWORD, LEN(@KEYWORD) - 1)

                  IF CHARINDEX(@NEXTLETTER, @USEDLETTERS) = 0
                    BEGIN
                        INSERT INTO @T_TABLE
                        SELECT @NEXTLETTER,
                               @COUNTER,
                               @COUNTER_2

                        SET @COUNTER = @COUNTER + 1
                        SET @USEDLETTERS = @USEDLETTERS + @NEXTLETTER
                    END
              END
            ELSE
              BEGIN
                  WHILE 1 = 1
                    BEGIN
                        IF CHARINDEX(CHAR(64 + @I), @USEDLETTERS) = 0
                           AND NOT ( CHAR(64 + @I) = 'Q'
                                     AND @Q = 1 )
                           AND NOT ( @Q = 0
                                     AND CHAR(64 + @I) = 'J' )
                          BEGIN
                              SET @NEXTLETTER = CHAR(64 + @I)
                              SET @USEDLETTERS = @USEDLETTERS + CHAR(64 + @I)
                              SET @I = @I + 1

                              BREAK
                          END

                        SET @I = @I + 1
                    END

                  -- SELECT 1 AS [T]
                  --BREAK
                  INSERT INTO @T_TABLE
                  SELECT @NEXTLETTER,
                         @COUNTER,
                         @COUNTER_2

                  SET @COUNTER = @COUNTER + 1
              END
        END

      SET @COUNTER_2 = @COUNTER_2 + 1
      SET @COUNTER = 1
  END

--Split word into Digraphs
WHILE @A < 1
  BEGIN
      SET @TESTWORDS = UPPER(LEFT(@INPUT, 2))

      IF LEN(@TESTWORDS) = 1
        BEGIN
            SET @TESTWORDS = @TESTWORDS + 'X'
            SET @A = 1
        END
      ELSE IF RIGHT(@TESTWORDS, 1) = LEFT(@TESTWORDS, 1)
        BEGIN
            SET @TESTWORDS = RIGHT(@TESTWORDS, 1) + 'X'
            SET @INPUT = RIGHT(@INPUT, LEN(@INPUT) - 1)
        END
      ELSE
        SET @INPUT = RIGHT(@INPUT, LEN(@INPUT) - 2)

      IF LEN(@INPUT) = 0
        SET @A = 1

      INSERT @WORDS
      SELECT @TESTWORDS,
             ''
  END

--Start Encryption
IF @ENCRYPT = 1
  BEGIN
      --Loop through Digraphs amd encrypt
      DECLARE WORDS_LOOP CURSOR LOCAL FORWARD_ONLY FOR
        SELECT WORD_PRE
        FROM   @WORDS
        FOR UPDATE OF WORD_POST

      OPEN WORDS_LOOP

      FETCH NEXT FROM WORDS_LOOP INTO @WORD

      WHILE @@FETCH_STATUS = 0
        BEGIN
            --Find letter positions
            SET @ROW1 = (SELECT ROWID
                         FROM   @T_TABLE
                         WHERE  LETTER = LEFT(@WORD, 1))
            SET @ROW2 = (SELECT ROWID
                         FROM   @T_TABLE
                         WHERE  LETTER = RIGHT(@WORD, 1))
            SET @COL1 = (SELECT COLID
                         FROM   @T_TABLE
                         WHERE  LETTER = LEFT(@WORD, 1))
            SET @COL2 = (SELECT COLID
                         FROM   @T_TABLE
                         WHERE  LETTER = RIGHT(@WORD, 1))

            --Move positions according to encryption rules
            IF @COL1 = @COL2
              BEGIN
                  SET @ROW1 = @ROW1 + 1
                  SET @ROW2 = @ROW2 + 1
              --select 'row'
              END
            ELSE IF @ROW1 = @ROW2
              BEGIN
                  SET @COL1 = @COL1 + 1
                  SET @COL2 = @COL2 + 1
              --select 'col'
              END
            ELSE
              BEGIN
                  SET @TMP = @COL2
                  SET @COL2 = @COL1
                  SET @COL1 = @TMP
              --select 'reg'
              END

            IF @ROW1 = 6
              SET @ROW1 = 1

            IF @ROW2 = 6
              SET @ROW2 = 1

            IF @COL1 = 6
              SET @COL1 = 1

            IF @COL2 = 6
              SET @COL2 = 1

            --Find encrypted letters by positions
            UPDATE @WORDS
            SET    WORD_POST = (SELECT (SELECT LETTER
                                        FROM   @T_TABLE
                                        WHERE  ROWID = @ROW1
                                               AND COLID = @COL1)
                                       + (SELECT LETTER
                                          FROM   @T_TABLE
                                          WHERE  COLID = @COL2
                                                 AND ROWID = @ROW2))
            WHERE  WORD_PRE = @WORD

            FETCH NEXT FROM WORDS_LOOP INTO @WORD
        END

      CLOSE WORDS_LOOP

      DEALLOCATE WORDS_LOOP
  END
--Start Decryption
ELSE
  BEGIN
      --Loop through Digraphs amd decrypt
      DECLARE WORDS_LOOP CURSOR LOCAL FORWARD_ONLY FOR
        SELECT WORD_PRE
        FROM   @WORDS
        FOR UPDATE OF WORD_POST

      OPEN WORDS_LOOP

      FETCH NEXT FROM WORDS_LOOP INTO @WORD

      WHILE @@FETCH_STATUS = 0
        BEGIN
            --Find letter positions
            SET @ROW1 = (SELECT ROWID
                         FROM   @T_TABLE
                         WHERE  LETTER = LEFT(@WORD, 1))
            SET @ROW2 = (SELECT ROWID
                         FROM   @T_TABLE
                         WHERE  LETTER = RIGHT(@WORD, 1))
            SET @COL1 = (SELECT COLID
                         FROM   @T_TABLE
                         WHERE  LETTER = LEFT(@WORD, 1))
            SET @COL2 = (SELECT COLID
                         FROM   @T_TABLE
                         WHERE  LETTER = RIGHT(@WORD, 1))

            --Move positions according to encryption rules
            IF @COL1 = @COL2
              BEGIN
                  SET @ROW1 = @ROW1 - 1
                  SET @ROW2 = @ROW2 - 1
              --select 'row'
              END
            ELSE IF @ROW1 = @ROW2
              BEGIN
                  SET @COL1 = @COL1 - 1
                  SET @COL2 = @COL2 - 1
              --select 'col'
              END
            ELSE
              BEGIN
                  SET @TMP = @COL2
                  SET @COL2 = @COL1
                  SET @COL1 = @TMP
              --select 'reg'
              END

            IF @ROW1 = 0
              SET @ROW1 = 5

            IF @ROW2 = 0
              SET @ROW2 = 5

            IF @COL1 = 0
              SET @COL1 = 5

            IF @COL2 = 0
              SET @COL2 = 5

            --Find decrypted letters by positions
            UPDATE @WORDS
            SET    WORD_POST = (SELECT (SELECT LETTER
                                        FROM   @T_TABLE
                                        WHERE  ROWID = @ROW1
                                               AND COLID = @COL1)
                                       + (SELECT LETTER
                                          FROM   @T_TABLE
                                          WHERE  COLID = @COL2
                                                 AND ROWID = @ROW2))
            WHERE  WORD_PRE = @WORD

            FETCH NEXT FROM WORDS_LOOP INTO @WORD
        END

      CLOSE WORDS_LOOP

      DEALLOCATE WORDS_LOOP
  END

--Output
DECLARE WORDS CURSOR LOCAL FAST_FORWARD FOR
  SELECT WORD_POST
  FROM   @WORDS

OPEN WORDS

FETCH NEXT FROM WORDS INTO @WORD

WHILE @@FETCH_STATUS = 0
  BEGIN
      SET @SQL = @SQL + @WORD + ' '

      FETCH NEXT FROM WORDS INTO @WORD
  END

CLOSE WORDS

DEALLOCATE WORDS

SELECT @SQL

--Cleanup
IF EXISTS (SELECT *
           FROM   SYS.TYPES
           WHERE  NAME = 'FairPlayTable')
  DROP TYPE FAIRPLAYTABLE

```



## Tcl

(My guess is that lappend digraphs $c0 [expr {$c0 eq $c ? "X" : $c}] is simply discarding $c. [[User:Petelomax|Pete Lomax]] ([[User talk:Petelomax|talk]]) 05:54, 13 October 2018 (UTC))

```tcl
package require TclOO

oo::class create Playfair {
    variable grid lookup excluder
    constructor {{keyword "PLAYFAIR EXAMPLE"} {exclude "J"}} {
	# Tweaking according to exact operation mode
	if {$exclude eq "J"} {
	    set excluder "J I"
	} else {
	    set excluder [list $exclude ""]
	}

	# Clean up the keyword source
	set keys [my Clean [append keyword "ABCDEFGHIJKLMNOPQRSTUVWXYZ"]]

	# Generate the encoding grid
	set grid [lrepeat 5 [lrepeat 5 ""]]
	set idx -1
	for {set i 0} {$i < 5} {incr i} {for {set j 0} {$j < 5} {} {
	    if {![info exist lookup([set c [lindex $keys [incr idx]]])]} {
		lset grid $i $j $c
		set lookup($c) [list $i $j]
		incr j
	    }
	}}

	# Sanity check
	if {[array size lookup] != 25} {
	    error "failed to build encoding table correctly"
	}
    }

    # Worker to apply a consistent cleanup/split rule
    method Clean {str} {
	set str [string map $excluder [string toupper $str]]
	split [regsub -all {[^A-Z]} $str ""] ""
    }

    # These public methods are implemented by a single non-public method
    forward encode my Transform 1
    forward decode my Transform -1

    # The application of the Playfair cypher transform
    method Transform {direction message} {
	# Split message into true digraphs
	foreach c [my Clean $message] {
	    if {![info exists lookup($c)]} continue
	    if {[info exist c0]} {
		lappend digraphs $c0 [expr {$c0 eq $c ? "X" : $c}]
		unset c0
	    } else {
		set c0 $c
	    }
	}
	if {[info exist c0]} {
	    lappend digraphs $c0 "Z"
	}

	# Encode the digraphs
	set result ""
	foreach {a b} $digraphs {
	    lassign $lookup($a) ai aj
	    lassign $lookup($b) bi bj
	    if {$ai == $bi} {
		set aj [expr {($aj + $direction) % 5}]
		set bj [expr {($bj + $direction) % 5}]
	    } elseif {$aj == $bj} {
		set ai [expr {($ai + $direction) % 5}]
		set bi [expr {($bi + $direction) % 5}]
	    } else {
		set tmp $aj
		set aj $bj
		set bj $tmp
	    }
	    lappend result [lindex $grid $ai $aj][lindex $grid $bi $bj]
	}

	# Real use would be: return [join $result ""]
	return $result
    }
}
```

Demonstrating:

```tcl
Playfair create cypher "Playfair Example"
set plaintext "Hide the gold in...the TREESTUMP!!!"
set encoded [cypher encode $plaintext]
set decoded [cypher decode $encoded]
puts "Original: $plaintext"
puts "Encoded:  $encoded"
puts "Decoded:  $decoded"
```

```txt

Original: Hide the gold in...the TREESTUMP!!!
Encoded:  BM OD ZB XD NA BE KU DM UI XM KZ ZR FT
Decoded:  HI DE TH EG OL DI NT HE TR EX ST UM PZ

```



## VBA


```vb

Option Explicit

Private Type Adress
   Row As Integer
   Column As Integer
End Type

Private myTable() As String

Sub Main()
Dim keyw As String, boolQ As Boolean, text As String, test As Long
Dim res As String
   keyw = InputBox("Enter your keyword : ", "KeyWord", "Playfair example")
   If keyw = "" Then GoTo ErrorHand
   Debug.Print "Enter your keyword : " & keyw
   boolQ = MsgBox("Ignore Q when buiding table  y/n : ", vbYesNo) = vbYes
   Debug.Print "Ignore Q when buiding table  y/n : " & IIf(boolQ, "Y", "N")
   Debug.Print ""
   Debug.Print "Table : "
   myTable = CreateTable(keyw, boolQ)
   On Error GoTo ErrorHand
   test = UBound(myTable)
   On Error GoTo 0
   text = InputBox("Enter your text", "Encode", "hide the gold in the TRRE stump")
   If text = "" Then GoTo ErrorHand
   Debug.Print ""
   Debug.Print "Text to encode : " & text
   Debug.Print "-------------------------------------------------"
   res = Encode(text)
   Debug.Print "Encoded text is : " & res
   res = Decode(res)
   Debug.Print "Decoded text is : " & res
   text = InputBox("Enter your text", "Encode", "hide the gold in the TREE stump")
   If text = "" Then GoTo ErrorHand
   Debug.Print ""
   Debug.Print "Text to encode : " & text
   Debug.Print "-------------------------------------------------"
   res = Encode(text)
   Debug.Print "Encoded text is : " & res
   res = Decode(res)
   Debug.Print "Decoded text is : " & res
   Exit Sub
ErrorHand:
   Debug.Print "error"
End Sub

Private Function CreateTable(strKeyword As String, Q As Boolean) As String()
Dim r As Integer, c As Integer, temp(1 To 5, 1 To 5) As String, t, cpt As Integer
Dim strT As String, coll As New Collection
Dim s As String

   strKeyword = UCase(Replace(strKeyword, " ", ""))
   If Q Then
      If InStr(strKeyword, "J") > 0 Then
         Debug.Print "Your keyword isn't available with your choice : Not Q (==> J) !"
         Exit Function
      End If
   Else
      If InStr(strKeyword, "Q") > 0 Then
         Debug.Print "Your keyword isn't available with your choice : Q (==> Not J) !"
         Exit Function
      End If
   End If
   strT = IIf(Not Q, "ABCDEFGHIKLMNOPQRSTUVWXYZ", "ABCDEFGHIJKLMNOPRSTUVWXYZ")
   t = Split(StrConv(strKeyword, vbUnicode), Chr(0))
   For c = LBound(t) To UBound(t) - 1
      strT = Replace(strT, t(c), "")
      On Error Resume Next
      coll.Add t(c), t(c)
      On Error GoTo 0
   Next
   strKeyword = vbNullString
   For c = 1 To coll.Count
      strKeyword = strKeyword & coll(c)
   Next
   t = Split(StrConv(strKeyword & strT, vbUnicode), Chr(0))
   c = 1: r = 1
   For cpt = LBound(t) To UBound(t) - 1
      temp(r, c) = t(cpt)
      s = s & " " & t(cpt)
      c = c + 1
      If c = 6 Then c = 1: r = r + 1: Debug.Print "   " & s: s = ""
   Next
   CreateTable = temp
End Function

Private Function Encode(s As String) As String
Dim i&, t() As String, cpt&
   s = UCase(Replace(s, " ", ""))
   'insert "X"
   For i = 1 To Len(s) - 1
      If Mid(s, i, 1) = Mid(s, i + 1, 1) Then s = Left(s, i) & "X" & Right(s, Len(s) - i)
   Next
   'Do the pairs
   For i = 1 To Len(s) Step 2
      ReDim Preserve t(cpt)
      t(cpt) = Mid(s, i, 2)
      cpt = cpt + 1
   Next i
   If Len(t(UBound(t))) = 1 Then t(UBound(t)) = t(UBound(t)) & "X"
   Debug.Print "[the pairs : " & Join(t, " ") & "]"
   'swap the pairs
   For i = LBound(t) To UBound(t)
      t(i) = SwapPairsEncoding(t(i))
   Next
   Encode = Join(t, " ")
End Function

Private Function SwapPairsEncoding(s As String) As String
Dim r As Integer, c As Integer, d1 As String, d2 As String, Flag As Boolean
Dim addD1 As Adress, addD2 As Adress, resD1 As Adress, resD2 As Adress
   d1 = Left(s, 1): d2 = Right(s, 1)
   For r = 1 To 5
      For c = 1 To 5
         If d1 = myTable(r, c) Then addD1.Row = r: addD1.Column = c
         If d2 = myTable(r, c) Then addD2.Row = r: addD2.Column = c
         If addD1.Row <> 0 And addD2.Row <> 0 Then Flag = True: Exit For
      Next
      If Flag Then Exit For
   Next
   Select Case True
      Case addD1.Row = addD2.Row And addD1.Column <> addD2.Column
         'same row, different columns
         resD1.Column = IIf(addD1.Column + 1 = 6, 1, addD1.Column + 1)
         resD2.Column = IIf(addD2.Column + 1 = 6, 1, addD2.Column + 1)
         SwapPairsEncoding = myTable(addD1.Row, resD1.Column) & myTable(addD2.Row, resD2.Column)
      Case addD1.Row <> addD2.Row And addD1.Column = addD2.Column
         'same columns, different rows
         resD1.Row = IIf(addD1.Row + 1 = 6, 1, addD1.Row + 1)
         resD2.Row = IIf(addD2.Row + 1 = 6, 1, addD2.Row + 1)
         SwapPairsEncoding = myTable(resD1.Row, addD1.Column) & myTable(resD2.Row, addD2.Column)
      Case addD1.Row <> addD2.Row And addD1.Column <> addD2.Column
         'different rows, different columns
         resD1.Row = addD1.Row
         resD2.Row = addD2.Row
         resD1.Column = addD2.Column
         resD2.Column = addD1.Column
         SwapPairsEncoding = myTable(resD1.Row, resD1.Column) & myTable(resD2.Row, resD2.Column)
   End Select
End Function

Private Function Decode(s As String) As String
Dim t, i&, j&, e&
   t = Split(s, " ")
   e = UBound(t) - 1
   'swap the pairs
   For i = LBound(t) To UBound(t)
      t(i) = SwapPairsDecoding(CStr(t(i)))
   Next
   'remove "X"
   For i = LBound(t) To e
      If Right(t(i), 1) = "X" And Left(t(i), 1) = Left(t(i + 1), 1) Then
         t(i) = Left(t(i), 1) & Left(t(i + 1), 1)
         For j = i + 1 To UBound(t) - 1
            t(j) = Right(t(j), 1) & Left(t(j + 1), 1)
         Next j
         If Right(t(j), 1) = "X" Then
            ReDim Preserve t(j - 1)
         Else
            t(j) = Right(t(j), 1) & "X"
         End If
      ElseIf Left(t(i + 1), 1) = "X" And Right(t(i), 1) = Right(t(i + 1), 1) Then
         For j = i + 1 To UBound(t) - 1
            t(j) = Right(t(j), 1) & Left(t(j + 1), 1)
         Next j
         If Right(t(j), 1) = "X" Then
            ReDim Preserve t(j - 1)
         Else
            t(j) = Right(t(j), 1) & "X"
         End If
      End If
   Next
   Decode = Join(t, " ")
End Function

Private Function SwapPairsDecoding(s As String) As String
Dim r As Integer, c As Integer, d1 As String, d2 As String, Flag As Boolean
Dim addD1 As Adress, addD2 As Adress, resD1 As Adress, resD2 As Adress
   d1 = Left(s, 1): d2 = Right(s, 1)
   For r = 1 To 5
      For c = 1 To 5
         If d1 = myTable(r, c) Then addD1.Row = r: addD1.Column = c
         If d2 = myTable(r, c) Then addD2.Row = r: addD2.Column = c
         If addD1.Row <> 0 And addD2.Row <> 0 Then Flag = True: Exit For
      Next
      If Flag Then Exit For
   Next
   Select Case True
      Case addD1.Row = addD2.Row And addD1.Column <> addD2.Column
         'same row, different columns
         resD1.Column = IIf(addD1.Column - 1 = 0, 5, addD1.Column - 1)
         resD2.Column = IIf(addD2.Column - 1 = 0, 5, addD2.Column - 1)
         SwapPairsDecoding = myTable(addD1.Row, resD1.Column) & myTable(addD2.Row, resD2.Column)
      Case addD1.Row <> addD2.Row And addD1.Column = addD2.Column
         'same columns, different rows
         resD1.Row = IIf(addD1.Row - 1 = 0, 5, addD1.Row - 1)
         resD2.Row = IIf(addD2.Row - 1 = 0, 5, addD2.Row - 1)
         SwapPairsDecoding = myTable(resD1.Row, addD1.Column) & myTable(resD2.Row, addD2.Column)
      Case addD1.Row <> addD2.Row And addD1.Column <> addD2.Column
         'different rows, different columns
         resD1.Row = addD1.Row
         resD2.Row = addD2.Row
         resD1.Column = addD2.Column
         resD2.Column = addD1.Column
         SwapPairsDecoding = myTable(resD1.Row, resD1.Column) & myTable(resD2.Row, resD2.Column)
   End Select
End Function
```

```txt
Enter your keyword : Playfair example
Ignore Q when buiding table  y/n : N

Table :
    P L A Y F
    I R E X M
    B C D G H
    K N O Q S
    T U V W Z

Text to encode : hide the gold in the TRRE stump
-------------------------------------------------
[the pairs : HI DE TH EG OL DI NT HE TR XR ES TU MP]
Encoded text is : BM OD ZB XD NA BE KU DM UI ME MO UV IF
Decoded text is : HI DE TH EG OL DI NT HE TR RE ST UM PX

Text to encode : hide the gold in the TREE stump
-------------------------------------------------
[the pairs : HI DE TH EG OL DI NT HE TR EX ES TU MP]
Encoded text is : BM OD ZB XD NA BE KU DM UI XM MO UV IF
Decoded text is : HI DE TH EG OL DI NT HE TR EE ST UM PX
```



## zkl


```zkl
fcn genKeyTable(key,deadChr){ // deadChr=="Q" or "J"
   deadChr=deadChr.toUpper();
   key=key.toUpper().unique() - " " - deadChr;
   return(key + (["A".."Z"].pump(String) - deadChr - key), deadChr);
}
```


```zkl
fcn playfair(text,keytable){  // text is a-z only
   keyTable,deadChr:=keytable;
   text=text.toUpper();
   text-=text - keyTable;	// remove unencodable characters
   if(deadChr=="Q") text-=deadChr; else text=text.replace("J","I");

   text=text.pump(String,T(Void.Read,1,False),
			 fcn(a,b=""){ if(a==b) a+"X"+b else a+b });
   if(text.len().isOdd) text+="Z";

   row:='wrap(c){ keyTable.index(c)/5 };
   col:='wrap(c){ keyTable.index(c)%5 };
   ltrRight:='wrap(c){ keyTable[(keyTable.index(c) + 1)%25] };
   ltrBelow:='wrap(c){ keyTable[(keyTable.index(c) + 5)%25] };
   ltrAt:='wrap(r,c) { keyTable[r*5 + c] };

   text.pump(String, Void.Read,  //-->digraph
      'wrap(a,b){
	 if((ra:=row(a))==(rb:=row(b))) return(ltrRight(a) + ltrRight(b));
	 if((ca:=col(a))==(cb:=col(b))) return(ltrBelow(a) + ltrBelow(b));
	 return(ltrAt(ra,cb) + ltrAt(rb,ca));
      })
   .pump(String,Void.Read,"".create.fp(" ")).strip(); // insert blanks
}
```


```zkl
fcn decodePF(text,keyTable){
   keyTable,_=keyTable;
   text-=" ";
   row:='wrap(c){ keyTable.index(c)/5 };
   col:='wrap(c){ keyTable.index(c)%5 };
   ltrLeft:='wrap(c){ keyTable[(keyTable.index(c) - 1)%25] };
   ltrUp:='wrap(c){ n:=keyTable.index(c) - 5; if(n<0)n+=25; keyTable[n%25] };
   ltrAt:='wrap(r,c){ keyTable[r*5 + c] };
   text.pump(String,Void.Read,  //-->digraph
      'wrap(a,b){
	 if((ra:=row(a))==(rb:=row(b))) return(ltrLeft(a) + ltrLeft(b));
	 if((ca:=col(a))==(cb:=col(b))) return(ltrUp(a)   + ltrUp(b));
	 return(ltrAt(ra,cb) + ltrAt(rb,ca));
      })
   .pump(String,Void.Read,"".create.fp(" ")).strip(); // insert blanks
}

```


```zkl
msg:="Hide the gold in the tree stump!!!";
keyTable:=genKeyTable("playfair example");
msg.println();
e:=playfair(msg,keyTable); e.println();
decodePF(e,keyTable).println();
playfair("XX",keyTable).println() : decodePF(_,keyTable).println();
```

```txt

Hide the gold in the tree stump!!!
BM OD ZB XD NA BE KU DM UI XM MO UV IF
HI DE TH EG OL DI NT HE TR EX ES TU MP
MM MW
XX XZ

```

