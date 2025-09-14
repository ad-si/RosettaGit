+++
title = "NYSIIS"
description = ""
date = 2018-12-28T06:07:38Z
aliases = []
[extra]
id = 13128
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "cpp",
  "d",
  "go",
  "java",
  "julia",
  "kotlin",
  "perl",
  "perl_6",
  "phix",
  "python",
  "racket",
  "rexx",
  "tcl",
  "zkl",
]
+++

The [[wp:New York State Identification and Intelligence System|New York State Identification and Intelligence System phonetic code]], commonly known as NYSIIS, is a phonetic algorithm for creating indices for words based on their pronunciation. The goal is for homophones to be encoded to the same representation so that they can be matched despite minor differences in spelling.

The task here is to implement the original NYSIIS algorithm, shown in Wikipedia, rather than any other subsequent modification.  Also, before the algorithm is applied the input string should be converted to upper case with all white space removed.

An optional step is to handle multiple names, including double-barrelled names or double surnames (e.g. 'Hoyle-Johnson' or 'Vaughan Williams') and unnecessary suffixes/honours that are not required for indexing purposes (e.g. 'Jnr', 'Sr', 'III', etc) - a small selection will suffice.  The original implementation is also restricted to six characters, but this is not a requirement.

## See also

* [[Soundex]]





## C++

Implementation based on Wikipedia description of the algorithm.


```c

#include <iostream>   // required for debug code in main() only
#include <iomanip>    // required for debug code in main() only
#include <string>

std::string NYSIIS( std::string const& str )
{
    std::string s, out;
    s.reserve( str.length() );
    for( auto const c : str )
    {
        if( c >= 'a' && c <= 'z' )
            s += c - ('a' - 'A');
        else if( c >= 'A' && c <= 'Z' )
            s += c;
    }

    auto replace = []( char const * const from, char const* to, char* const dst ) -> bool
    {
        auto const n = strlen( from );
        if( strncmp( from, dst, n ) == 0 )
        {
            strncpy( dst, to, n );
            return true;
        }
        return false;
    };

    auto multiReplace = []( char const* const* from, char const* to, char* const dst ) -> bool
    {
        auto const n = strlen( *from );
        for( ; *from; ++from )
            if( strncmp( *from, dst, n ) == 0 )
            {
                memcpy( dst, to, n );
                return true;
            }
        return false;
    };

    auto isVowel = []( char const c ) -> bool
    {
        return c == 'A' || c == 'E' || c == 'I' || c == 'O' || c == 'U';
    };

    size_t n = s.length();
    replace( "MAC", "MCC", &s[0] );
    replace( "KN", "NN", &s[0] );
    replace( "K", "C", &s[0] );
    char const* const prefix[] = { "PH", "PF", 0 };
    multiReplace( prefix, "FF", &s[0] );
    replace( "SCH", "SSS", &s[0] );

    char const* const suffix1[] = { "EE", "IE", 0 };
    char const* const suffix2[] = { "DT", "RT", "RD", "NT", "ND", 0 };
    if( multiReplace( suffix1, "Y", &s[n - 2] ) || multiReplace( suffix2, "D", &s[n - 2] ))
    {
        s.pop_back();
        --n;
    }

    out += s[0];

    char* vowels[] = { "A", "E", "I", "O", "U", 0 };
    for( unsigned i = 1; i < n; ++i )
    {
        char* const c = &s[i];
        if( !replace( "EV", "AV", c ) )
            multiReplace( vowels, "A", c );
        replace( "Q", "G", c );
        replace( "Z", "S", c );
        replace( "M", "N", c );
        if( !replace( "KN", "NN", c ))
            replace( "K", "C", c );
        replace( "SCH", "SSS", c );
        replace( "PH", "FF", c );
        if( *c == 'H' && (!isVowel( s[i - 1] ) || i + 1 >= n || !isVowel( s[i + 1] )))
            *c = s[i - 1];
        if( *c == 'W' && isVowel( s[i - 1] ))
            *c = 'A';
        if( out.back() != *c )
            out += *c;
    }

    if( out.back() == 'S' || out.back() == 'A' )
        out.pop_back();
    n = out.length() - 2;
    if( out[n] == 'A' && out[n + 1] == 'Y' )
        out = out.substr( 0, n ) + "Y";

    return out;
}

int main()
{
    static char const * const names[][2] = {
    { "Bishop", "BASAP" },
    { "Carlson", "CARLSAN" },
    { "Carr", "CAR" },
    { "Chapman", "CAPNAN" },
    { "Franklin", "FRANCLAN" },
    { "Greene", "GRAN" },
    { "Harper", "HARPAR" },
    { "Jacobs", "JACAB" },
    { "Larson", "LARSAN" },
    { "Lawrence", "LARANC" },
    { "Lawson", "LASAN" },
    { "Louis, XVI", "LASXV" },
    { "Lynch", "LYNC" },
    { "Mackenzie", "MCANSY" },
    { "Matthews", "MATA" },
    { "McCormack", "MCARNAC" },
    { "McDaniel", "MCDANAL" },
    { "McDonald", "MCDANALD" },
    { "Mclaughlin", "MCLAGLAN" },
    { "Morrison", "MARASAN" },
    { "O'Banion", "OBANAN" },
    { "O'Brien", "OBRAN" },
    { "Richards", "RACARD" },
    { "Silva", "SALV" },
    { "Watkins", "WATCAN" },
    { "Wheeler", "WALAR" },
    { "Willis", "WALA" },
    { "brown, sr", "BRANSR" },
    { "browne, III", "BRAN" },
    { "browne, IV", "BRANAV" },
    { "knight", "NAGT" },
    { "mitchell", "MATCAL" },
    { "o'daniel", "ODANAL" } };

    for( auto const& name : names )
    {
        auto const code = NYSIIS( name[0] );
        std::cout << std::left << std::setw( 16 ) << name[0] << std::setw( 8 ) << code;
        if( code == std::string( name[1] ))
            std::cout << " ok";
        else
            std::cout << " ERROR: " << name[1] << " expected";
        std::cout << std::endl;
    }

    return 0;
}


```

```txt
 
Bishop          BASAP    ok
Carlson         CARLSAN  ok
Carr            CAR      ok
Chapman         CAPNAN   ok
Franklin        FRANCLAN ok
Greene          GRAN     ok
Harper          HARPAR   ok
Jacobs          JACAB    ok
Larson          LARSAN   ok
Lawrence        LARANC   ok
Lawson          LASAN    ok
Louis, XVI      LASXV    ok
Lynch           LYNC     ok
Mackenzie       MCANSY   ok
Matthews        MATA     ok
McCormack       MCARNAC  ok
McDaniel        MCDANAL  ok
McDonald        MCDANALD ok
Mclaughlin      MCLAGLAN ok
Morrison        MARASAN  ok
O'Banion        OBANAN   ok
O'Brien         OBRAN    ok
Richards        RACARD   ok
Silva           SALV     ok
Watkins         WATCAN   ok
Wheeler         WALAR    ok
Willis          WALA     ok
brown, sr       BRANSR   ok
browne, III     BRAN     ok
browne, IV      BRANAV   ok
knight          NAGT     ok
mitchell        MATCAL   ok
o'daniel        ODANAL   ok

```


=={{header|Caché ObjectScript}}==
Refactored code based on other examples to reduce footprint.


```cos

Class Utils.Phonetic [ Abstract ]
{

ClassMethod EncodeName(pAlgorithm As %String = "", pName As %String = "", Output pCode As %String, pSuffixRem As %Boolean = 1, pTruncate As %Integer = 0) As %Status
{
	// check algorithm and name
	Set pAlgorithm=$ZConvert(pAlgorithm, "l")
	If pAlgorithm="" Quit $$$ERROR($$$GeneralError, "No algorithm specified.")
	If $Case(pAlgorithm, "nysiis":1, :0)=0 Quit $$$ERROR($$$GeneralError, "Unknown algorithm specified.")
	If $Match(pName, ".*\d.*# no numbers") Quit $$$ERROR($$$GeneralError, "Name cannot contain numerics.")
	
	// remove apostrophes, find punctuation and replace with spaces (exclude hyphens)
	Set pName=$Translate(pName, "'")
	Set pun=$ZStrip(pName, "*E'P", "-")
	Set pName=$Translate(pName, pun, $Justify(" ", $Length(pun)))
	
	// convert name(s) to uppercase and remove all white space
	Set pName=$ZStrip($ZConvert(pName, "U"), "<=>W")
	
	// remove suffixes (e.g. 'Jnr', 'OBE', 'DSC', etc), including roman numerals (e.g. 'II', 'VIII')
	// - http://en.wikipedia.org/wiki/List_of_post-nominal_letters_(United_Kingdom)
	If pSuffixRem {
		Set ords=$ListBuild("KG", "LG", "KT", "LT", "GCB", "KCB", "DCB", "CB", "GCMG", "KCMG", "DCMG", "CMG", "DSO", 
			"GCVO", "KCVO", "DCVO", "CVO", "LVO", "MVO", "OM", "ISO", "GBE", "KBE", "DBE", "CBE", "OBE", "MBE", "CH")
		Set decs=$ListBuild("VC", "GC", "CGC", "RRC", "DSC", "MC", "DFC", "AFC", "ARRC", "OBI", "IOM")
		Set regexp="( )(SNR|SR|JNR|JR|ESQ|"_$ListToString(ords, "|")_"|"_$ListToString(decs, "|")_"|[IVX]+)"
		Set rem=##class(%Regex.Matcher).%New(regexp, pName)
		Set pName=rem.ReplaceAll("")
	}
	
	// replace hyphen and white space, plus some final validation
	Set pName=$ZStrip($Translate(pName, "-", " "), "<=>W")
	If $Length($Piece(pName, " "))<2 Quit $$$ERROR($$$GeneralError, "Invalid name.")
	
	// begin algorithm and truncate result, if necessary
	Set pCode=""
	For piece=1:1:$Length(pName, " ") {
		If pAlgorithm="nysiis" Set pCode=pCode_..ToNYSIIS($Piece(pName, " ", piece))
	}
	If pTruncate {
		Set pName=pCode
		Set pCode=$Extract(pCode, 1, pTruncate)
		Set $Extract(pName, 1, pTruncate)=""
		If $Length(pName) Set pCode=pCode_"["_pName_"]"
	}
	
	// finished
	Quit $$$OK
}

ClassMethod ToNYSIIS(pName As %String) As %String
{
	/*
		New York State Identification and Intelligence System (NYSIIS) Phonetic Encoder
		- http://en.wikipedia.org/wiki/New_York_State_Identification_and_Intelligence_System
		- http://www.dropby.com/indexLF.html?content=/NYSIIS.html
	*/
	
	// create regexp matcher instance, remove punctuation and convert all to upper case
	Set rem=##class(%Regex.Matcher).%New(" ")
	Set rem.Text=$ZConvert($ZStrip(pName, "*P"), "U")
	
	// translate first characters of name:
	// => MAC->MCC, KN->N, K->C, PH/PF->FF, SCH->SSS
	For rule="^MAC->MCC", "^KN->N", "^K->C", "^(PH|PF)->FF", "^SCH->SSS" {
		Set rem.Pattern=$Piece(rule, "->")
		If rem.Locate() Set rem.Text=rem.ReplaceFirst($Piece(rule, "->", 2)) Quit
	}

	// translate last characters of name:
	// => EE/IE->Y, DT/RT/RD/NT/ND->D
	For rule="(EE|IE)$->Y", "(DT|RT|RD|NT|ND)$->D" {
		Set rem.Pattern=$Piece(rule, "->")
		If rem.Locate() Set rem.Text=rem.ReplaceFirst($Piece(rule, "->", 2)) Quit
	}
	
	// first character of key = first character of name
	Set pName1=$Extract(rem.Text, 1), rem.Text=$Extract(rem.Text, 2, *)
	
	// translate remaining characters by following rules, incrementing by one character each time:
	// => EV->AF else A,E,I,O,U->A
	// => Q->G, Z->S, M->N
	// => KN->N else K->C
	// => SCH->SSS, PH->FF
	// => H->if previous or next is non-vowel, previous
	// => W->if previous is vowel, A (A is the only vowel left)
	// => add current to key if current is not same as the last key character
	Set ptr=0, rules=$ListBuild("EV->AF", "(A|E|I|O|U)->A", "Q->G", "Z->S", "M->N", "KN->N", "K->C", 
		"SCH->SSS", "PH->FF", "H[^A]", "[^A]H", "AW->A")
	While $ListNext(rules, ptr, rule) {
		Set rem.Pattern=$Piece(rule, "->")
		If $Piece(rule, "->", 2)="",  rem.Locate() {
			Set $Piece(rule, "->", 2)=$Translate(rem.Group, "H")
		}
		Set rem.Text=rem.ReplaceAll($Piece(rule, "->", 2))
	}
	Set pName=$ZStrip(rem.Text, "=U")  // remove duplicates
	
	// if last character is S, remove it
	If $Extract(pName, *)="S" Set pName=$Extract(pName, 1, *-1)
	
	// if last characters are AY, replace with Y
	If $Extract(pName, *-1, *)="AY" Set pName=$Extract(pName, 1, *-2)_"Y"
	
	// if last character is A, remove it
	If $Extract(pName, *)="A" Set pName=$Extract(pName, 1, *-1)
	
	// append translated key to removed first character
	Quit pName1_pName
}

}

```

```txt

USER>For  { Read !, name Quit:name=""  Set sc=##class(Utils.Phonetic).EncodeName("nysiis", name, .code,, 6) If sc Write " -> ", code }

knight -> NAGT
mitchell -> MATCAL
o'daniel -> ODANAL
brown sr -> BRAN
browne III -> BRAN
browne IV -> BRAN
O'Banion -> OBANAN
Mclaughlin -> MCLAGL[AN]
McCormack -> MCARNA[C]
Chapman -> CHAPNA[N]
Silva -> SALV
McDonald -> MCDANA[LD]
Lawson -> LASAN
Jacobs -> JACAB
Greene -> GRAN
O'Brien -> OBRAN
Morrison -> MARASA[N]
Larson -> LARSAN
Willis -> WAL
Mackenzie -> MCANSY
Carr -> CAR
Lawrence -> LARANC
Matthews -> MAT
Richards -> RACARD
Bishop -> BASAP
Franklin -> FRANCL[AN]
McDaniel -> MCDANA[L]
Harper -> HARPAR
Lynch -> LYNC
Watkins -> WATCAN
Carlson -> CARLSA[N]
Wheeler -> WHALAR
Louis XVI -> L
Hoyle-Johnson -> HAYLJA[NSAN]
Vaughan Williams -> VAGANW[ALAN]
D'Souza -> DSAS
de Sousa -> DSAS

```



## D

```d
import std.stdio, std.regex, std.algorithm, std.range, std.string;

string replaceAt(in string text, in uint pos, in string[] fromList,
                 in string[] toList) pure /*nothrow*/ @safe {
    foreach (const f, const t; zip(fromList, toList))
        if (text[pos .. $].startsWith(f))
            return [text[0 .. pos], t, text[pos + f.length .. $]].join;
    return text;
}

string replaceEnd(in string text, in string[] fromList,
                  in string[] toList) pure /*nothrow*/ @safe {
    foreach (const f, const t; zip(fromList, toList))
        if (text.endsWith(f))
            return text[0 .. $ - f.length] ~ t;
    return text;
}

string nysiis(string name) /*pure nothrow*/ @safe {
    enum vowels = "AEIOU";
    name = name.replaceAll(r"\W".regex, "").toUpper;
    name = name.replaceAt(0, ["MAC", "KN", "K", "PH", "PF", "SCH"],
                             ["MCC", "N",  "C", "FF", "FF", "SSS"]);
    name = name.replaceEnd(["EE", "IE", "DT", "RT", "RD", "NT", "ND"],
                           ["Y",  "Y",  "D",  "D",  "D",  "D",  "D"]);
    string key = name[0 .. 1];
    string key1;

    foreach (immutable i; 1 .. name.length) {
        immutable n_1 = name[i - 1 .. i];
        immutable n = name[i];
        immutable n1b = (i + 1 < name.length) ? name[i+1 .. i+2] : "";
        name = name.replaceAt(i, ["EV"] ~ vowels.split(""), ["AF"] ~
                              ["A"].replicate(5));
        name = name.replaceAt(i, "QZM".split(""), "GSN".split(""));
        name = name.replaceAt(i, ["KN", "K"], ["N", "C"]);
        name = name.replaceAt(i, ["SCH", "PH"], ["SSS", "FF"]);
        if (n == 'H' && (!vowels.canFind(n_1) || !vowels.canFind(n1b)))
            name = [name[0 .. i], n_1, name[i + 1 .. $]].join;
        if (n == 'W' && vowels.canFind(n_1))
            name = [name[0 .. i], "A", name[i + 1 .. $]].join;
        if (!key.empty && key[$ - 1] != name[i])
            key ~= name[i];
    }

    return key1 ~ replaceEnd(key, ["S", "AY", "A"], ["", "Y", ""]);
}

void main() @safe {
    immutable names = ["Bishop", "Carlson", "Carr", "Chapman",
        "Franklin", "Greene", "Harper", "Jacobs", "Larson", "Lawrence",
        "Lawson", "Louis, XVI", "Lynch", "Mackenzie", "Matthews",
         "McCormack", "McDaniel", "McDonald", "Mclaughlin", "Morrison",
         "O'Banion", "O'Brien", "Richards", "Silva", "Watkins",
         "Wheeler", "Willis", "brown, sr", "browne, III", "browne, IV",
         "knight", "mitchell", "o'daniel"];

    foreach (immutable name; names)
        writefln("%11s: %s", name, name.nysiis);
}
```

```txt
     Bishop: BASAP
    Carlson: CARLSAN
       Carr: CAR
    Chapman: CAPNAN
   Franklin: FRANCLAN
     Greene: GRAN
     Harper: HARPAR
     Jacobs: JACAB
     Larson: LARSAN
   Lawrence: LARANC
     Lawson: LASAN
 Louis, XVI: LASXV
      Lynch: LYNC
  Mackenzie: MCANSY
   Matthews: MATA
  McCormack: MCARNAC
   McDaniel: MCDANAL
   McDonald: MCDANALD
 Mclaughlin: MCLAGLAN
   Morrison: MARASAN
   O'Banion: OBANAN
    O'Brien: OBRAN
   Richards: RACARD
      Silva: SALV
    Watkins: WATCAN
    Wheeler: WALAR
     Willis: WALA
  brown, sr: BRANSR
browne, III: BRAN
 browne, IV: BRANAV
     knight: NAGT
   mitchell: MATCAL
   o'daniel: ODANAL
```



## Go

```go
package main

import (
    "fmt"
    "strings"
)

type pair struct{ first, second string }

var (
    fStrs = []pair{{"MAC", "MCC"}, {"KN", "N"}, {"K", "C"}, {"PH", "FF"},
        {"PF", "FF"}, {"SCH", "SSS"}}
    lStrs = []pair{{"EE", "Y"}, {"IE", "Y"}, {"DT", "D"}, {"RT", "D"},
        {"RD", "D"}, {"NT", "D"}, {"ND", "D"}}
    mStrs = []pair{{"EV", "AF"}, {"KN", "N"}, {"SCH", "SSS"}, {"PH", "FF"}}
    eStrs = []string{"JR", "JNR", "SR", "SNR"}
)

func isVowel(b byte) bool {
    return strings.ContainsRune("AEIOU", rune(b))
}

func isRoman(s string) bool {
    if s == "" {
        return false
    }
    for _, r := range s {
        if !strings.ContainsRune("IVX", r) {
            return false
        }
    }
    return true
}

func nysiis(word string) string {
    if word == "" {
        return ""
    }
    w := strings.ToUpper(word)
    ww := strings.FieldsFunc(w, func(r rune) bool {
        return r == ' ' || r == ','
    })
    if len(ww) > 1 {
        last := ww[len(ww)-1]
        if isRoman(last) {
            w = w[:len(w)-len(last)]
        }
    }
    for _, c := range " ,'-" {
        w = strings.Replace(w, string(c), "", -1)
    }
    for _, eStr := range eStrs {
        if strings.HasSuffix(w, eStr) {
            w = w[:len(w)-len(eStr)]
        }
    }
    for _, fStr := range fStrs {
        if strings.HasPrefix(w, fStr.first) {
            w = strings.Replace(w, fStr.first, fStr.second, 1)
        }
    }
    for _, lStr := range lStrs {
        if strings.HasSuffix(w, lStr.first) {
            w = w[:len(w)-2] + lStr.second
        }
    }
    initial := w[0]
    var key strings.Builder
    key.WriteByte(initial)
    w = w[1:]
    for _, mStr := range mStrs {
        w = strings.Replace(w, mStr.first, mStr.second, -1)
    }
    sb := []byte{initial}
    sb = append(sb, w...)
    le := len(sb)
    for i := 1; i < le; i++ {
        switch sb[i] {
        case 'E', 'I', 'O', 'U':
            sb[i] = 'A'
        case 'Q':
            sb[i] = 'G'
        case 'Z':
            sb[i] = 'S'
        case 'M':
            sb[i] = 'N'
        case 'K':
            sb[i] = 'C'
        case 'H':
            if !isVowel(sb[i-1]) || (i < le-1 && !isVowel(sb[i+1])) {
                sb[i] = sb[i-1]
            }
        case 'W':
            if isVowel(sb[i-1]) {
                sb[i] = 'A'
            }
        }
    }
    if sb[le-1] == 'S' {
        sb = sb[:le-1]
        le--
    }
    if le > 1 && string(sb[le-2:]) == "AY" {
        sb = sb[:le-2]
        sb = append(sb, 'Y')
        le--
    }
    if le > 0 && sb[le-1] == 'A' {
        sb = sb[:le-1]
        le--
    }
    prev := initial
    for j := 1; j < le; j++ {
        c := sb[j]
        if prev != c {
            key.WriteByte(c)
            prev = c
        }
    }
    return key.String()
}

func main() {
    names := []string{
        "Bishop", "Carlson", "Carr", "Chapman",
        "Franklin", "Greene", "Harper", "Jacobs", "Larson", "Lawrence",
        "Lawson", "Louis, XVI", "Lynch", "Mackenzie", "Matthews", "May jnr",
        "McCormack", "McDaniel", "McDonald", "Mclaughlin", "Morrison",
        "O'Banion", "O'Brien", "Richards", "Silva", "Watkins", "Xi",
        "Wheeler", "Willis", "brown, sr", "browne, III", "browne, IV",
        "knight", "mitchell", "o'daniel", "bevan", "evans", "D'Souza",
        "Hoyle-Johnson", "Vaughan Williams", "de Sousa", "de la Mare II",
    }
    for _, name := range names {
        name2 := nysiis(name)
        if len(name2) > 6 {
            name2 = fmt.Sprintf("%s(%s)", name2[:6], name2[6:])
        }
        fmt.Printf("%-16s : %s\n", name, name2)
    }
}
```


```txt

Bishop           : BASAP
Carlson          : CARLSA(N)
Carr             : CAR
Chapman          : CAPNAN
Franklin         : FRANCL(AN)
Greene           : GRAN
Harper           : HARPAR
Jacobs           : JACAB
Larson           : LARSAN
Lawrence         : LARANC
Lawson           : LASAN
Louis, XVI       : LA
Lynch            : LYNC
Mackenzie        : MCANSY
Matthews         : MATA
May jnr          : MY
McCormack        : MCARNA(C)
McDaniel         : MCDANA(L)
McDonald         : MCDANA(LD)
Mclaughlin       : MCLAGL(AN)
Morrison         : MARASA(N)
O'Banion         : OBANAN
O'Brien          : OBRAN
Richards         : RACARD
Silva            : SALV
Watkins          : WATCAN
Xi               : X
Wheeler          : WALAR
Willis           : WAL
brown, sr        : BRAN
browne, III      : BRAN
browne, IV       : BRAN
knight           : NAGT
mitchell         : MATCAL
o'daniel         : ODANAL
bevan            : BAFAN
evans            : EVAN
D'Souza          : DSAS
Hoyle-Johnson    : HAYLAJ(ANSAN)
Vaughan Williams : VAGANW(ALAN)
de Sousa         : DASAS
de la Mare II    : DALANA(R)

```



## Java

```java
import static java.util.Arrays.*;
import static java.lang.System.out;

public class NYSIIS {

    final static String[][] first = {{"MAC", "MCC"}, {"KN", "N"}, {"K", "C"},
    {"PH", "FF"}, {"PF", "FF"}, {"SCH", "SSS"}};

    final static String[][] last = {{"EE", "Y"}, {"IE", "Y"}, {"DT", "D"},
    {"RT", "D"}, {"RD", "D"}, {"NT", "D"}, {"ND", "D"}};

    final static String Vowels = "AEIOU";

    public static void main(String[] args) {
        stream(args).parallel().map(n -> transcode(n)).forEach(out::println);
    }

    static String transcode(String s) {
        int len = s.length();
        StringBuilder sb = new StringBuilder(len);

        for (int i = 0; i < len; i++) {
            char c = s.charAt(i);
            if (c >= 'a' && c <= 'z')
                sb.append((char) (c - 32));
            else if (c >= 'A' && c <= 'Z')
                sb.append(c);
        }

        replace(sb, 0, first);
        replace(sb, sb.length() - 2, last);

        len = sb.length();
        sb.append(" ");
        for (int i = 1; i < len; i++) {
            char prev = sb.charAt(i - 1);
            char curr = sb.charAt(i);
            char next = sb.charAt(i + 1);

            if (curr == 'E' && next == 'V')
                sb.replace(i, i + 2, "AF");

            else if (isVowel(curr))
                sb.setCharAt(i, 'A');

            else if (curr == 'Q')
                sb.setCharAt(i, 'G');

            else if (curr == 'Z')
                sb.setCharAt(i, 'S');

            else if (curr == 'M')
                sb.setCharAt(i, 'N');

            else if (curr == 'K' && next == 'N')
                sb.setCharAt(i, 'N');

            else if (curr == 'K')
                sb.setCharAt(i, 'C');

            else if (sb.indexOf("SCH", i) == i)
                sb.replace(i, i + 3, "SSS");

            else if (curr == 'P' && next == 'H')
                sb.replace(i, i + 2, "FF");

            else if (curr == 'H' && (!isVowel(prev) || !isVowel(next)))
                sb.setCharAt(i, prev);

            else if (curr == 'W' && isVowel(prev))
                sb.setCharAt(i, prev);

            if (sb.charAt(i) == prev) {
                sb.deleteCharAt(i--);
                len--;
            }
        }
        sb.setLength(sb.length() - 1); // We've added a space

        int lastPos = sb.length() - 1;
        if (lastPos > 1) {

            if (sb.lastIndexOf("AY") == lastPos - 1)
                sb.delete(lastPos - 1, lastPos + 1).append("Y");

            else if (sb.charAt(lastPos) == 'S')
                sb.setLength(lastPos);

            else if (sb.charAt(lastPos) == 'A')
                sb.setLength(lastPos);
        }

        if (sb.length() > 6)
            sb.insert(6, '[').append(']');

        return String.format("%s -> %s", s, sb);
    }

    private static void replace(StringBuilder sb, int start, String[][] maps) {
        if (start >= 0)
            for (String[] map : maps) {
                if (sb.indexOf(map[0]) == start) {
                    sb.replace(start, start + map[0].length(), map[1]);
                    break;
                }
            }
    }

    private static boolean isVowel(char c) {
        return Vowels.indexOf(c) != -1;
    }
}
```



```txt
Carr -> CAR
Lawson -> LASAN
LouisXVI -> LASXV
Greene -> GRAN
Willis -> WALA
Mackenzie -> MCANSY
brownsr -> BRANSR
browneIV -> BRANAV
O'Brien -> OBRAN
Lynch -> LYNC
Wheeler -> WALAR
Lawrence -> LARANC
Larson -> LARSAN
McCormack -> MCARNA[C]
Jacobs -> JACAB
Harper -> HARPAR
Mclaughlin -> MCLAGL[AN]
Morrison -> MARASA[N]
McDonald -> MCDANA[LD]
O'Banion -> OBANAN
McDaniel -> MCDANA[L]
Silva -> SALV
mitchell -> MATCAL
Richards -> RACARD
Carlson -> CARLSA[N]
browneIII -> BRAN
Matthews -> MATA
knight -> NAGT
Franklin -> FRANCL[AN]
Chapman -> CAPNAN
Bishop -> BASAP
o'daniel -> ODANAL
Watkins -> WATCAN
```



## Julia

```julia
function replaceat(text::AbstractString, position::Int, fromlist, tolist)
    for (f, t) in zip(fromlist, tolist)
        if startswith(text[position:end], f)
            return text[1:position-1] * t * text[position+length(f):end]
        end
    end
    return text
end

function replaceend(text::AbstractString, fromlist, tolist)
    for (f, t) in zip(fromlist, tolist)
        if endswith(text, f)
            return text[1:end-length(f)] * t
        end
    end
    return text
end

function nysiis(name::AbstractString)
    vowels = ["A", "E", "I", "O", "U"]

    name = uppercase(filter(isalpha, name))
    name = replaceat(name, 1, ["MAC", "KN", "K", "PH", "PF", "SCH"],
                               ["MCC", "N",  "C", "FF", "FF", "SSS"])
    name = replaceend(name, ["EE", "IE", "DT", "RT", "RD", "NT", "ND"],
                             ["Y",  "Y",  "D",  "D",  "D",  "D",  "D"])
    key, key1 = name[1:1], ""
    for i in 2:length(name)
        prev, curr = name[(i:i)-1], name[i:i]
        next = i < length(name) ? name[(i:i)+1] : ""
        name = replaceat(name, i, vcat("EV", vowels), ["AF", "A", "A", "A", "A", "A"])
        name = replaceat(name, i, "QZM", "GSN")
        name = replaceat(name, i, ["KN", "K"], ["N", "C"])
        name = replaceat(name, i, ["SCH", "PH"], ["SSS", "FF"])
        if curr == "H" && (prev ∉ vowels || next ∉ vowels)
            name = name[1:i-1] * prev * name[i+1:end]
        end
        if curr == "W" && prev ∈ vowels
            name = name[1:i-1] * "A" * name[i+1:end]
        end
        if !isempty(key) && key[end:end] != name[i:i]
            key *= name[i:i]
        end
        i += 1
    end
    key = replaceend(key, ["S", "AY", "A"], ["", "Y", ""])
    return key1 * key
end

for name in ["Bishop", "Carlson", "Carr", "Chapman", "Franklin",
             "Greene", "Harper", "Jacobs", "Larson", "Lawrence",
             "Lawson", "Louis, XVI", "Lynch", "Mackenzie", "Matthews",
             "McCormack", "McDaniel", "McDonald", "Mclaughlin", "Morrison",
             "O'Banion", "O'Brien", "Richards", "Silva", "Watkins",
             "Wheeler", "Willis", "brown, sr", "browne, III", "browne, IV",
             "knight", "mitchell", "o'daniel"]
    @printf("%15s: %s\n", name, nysiis(name))
end
```


```txt
         Bishop: BASAP
        Carlson: CARLSAN
           Carr: CAR
        Chapman: CAPNAN
       Franklin: FRANCLAN
         Greene: GRAN
         Harper: HARPAR
         Jacobs: JACAB
         Larson: LARSAN
       Lawrence: LARANC
         Lawson: LASAN
     Louis, XVI: LASXV
          Lynch: LYNC
      Mackenzie: MCANSY
       Matthews: MATA
      McCormack: MCARNAC
       McDaniel: MCDANAL
       McDonald: MCDANALD
     Mclaughlin: MCLAGLAN
       Morrison: MARASAN
       O'Banion: OBANAN
        O'Brien: OBRAN
       Richards: RACARD
          Silva: SALV
        Watkins: WATCAN
        Wheeler: WALAR
         Willis: WALA
      brown, sr: BRANSR
    browne, III: BRAN
     browne, IV: BRANAV
         knight: NAGT
       mitchell: MATCAL
       o'daniel: ODANAL
```



## Kotlin


```scala
// version 1.1.2

val fStrs = listOf("MAC" to "MCC", "KN" to "N", "K" to "C", "PH" to "FF",
                   "PF" to "FF", "SCH" to "SSS")

val lStrs = listOf("EE" to "Y", "IE" to "Y", "DT" to "D", "RT" to "D",
                   "RD" to "D", "NT" to "D", "ND" to "D")

val mStrs = listOf("EV" to "AF", "KN" to "N", "SCH" to "SSS", "PH" to "FF")

val eStrs = listOf("JR", "JNR", "SR", "SNR")

fun Char.isVowel() = this in "AEIOU"

fun String.isRoman() = this.all { it in "IVX" }
 
fun nysiis(word: String): String {
    if (word.isEmpty()) return word
    var w = word.toUpperCase()
    val ww = w.split(' ', ',')
    if (ww.size > 1 && ww.last().isRoman()) w = w.dropLast(ww.last().length)
    for (c in " ,'-") w = w.replace(c.toString(), "")
    for (eStr in eStrs) 
        if (w.endsWith(eStr)) w = w.dropLast(eStr.length)

    for (fStr in fStrs) 
        if (w.startsWith(fStr.first)) w = w.replaceFirst(fStr.first, fStr.second)
    
    for (lStr in lStrs) 
        if (w.endsWith(lStr.first)) w = w.dropLast(2) + lStr.second
    
    val key = StringBuilder().append(w[0])
    w = w.drop(1)
    for (mStr in mStrs) w = w.replace(mStr.first, mStr.second)    
    val sb = StringBuilder().append(key[0]).append(w)
    var i = 1
    var len = sb.length
    while (i < len) {
        when (sb[i]) {  
            in "EIOU" -> sb[i] = 'A'       
            'Q'       -> sb[i] = 'G'
            'Z'       -> sb[i] = 'S'
            'M'       -> sb[i] = 'N'
            'K'       -> sb[i] = 'C'
            'H'       -> if (!sb[i - 1].isVowel() || (i < len - 1 && !sb[i + 1].isVowel())) sb[i] = sb[i - 1]
            'W'       -> if (sb[i - 1].isVowel()) sb[i] = 'A' 
        }
        i++
    }    
    if (sb[len - 1] == 'S') {
        sb.setLength(len - 1)
        len--
    }
    if (len > 1 && sb.substring(len - 2) == "AY") {
        sb.setLength(len - 2)
        sb.append("Y")
        len--
    }
    if (len > 0 && sb[len - 1] == 'A') {       
        sb.setLength(len - 1)
        len--
    }
    var prev = key[0]
    for (j in 1 until len) {
        val c = sb[j]
        if (prev != c) {
           key.append(c)
           prev = c
        }
    }
    return key.toString()
}

fun main(args:Array<String>) {
    val names = listOf(
        "Bishop", "Carlson", "Carr", "Chapman",
        "Franklin", "Greene", "Harper", "Jacobs", "Larson", "Lawrence",
        "Lawson", "Louis, XVI", "Lynch", "Mackenzie", "Matthews", "May jnr",
        "McCormack", "McDaniel", "McDonald", "Mclaughlin", "Morrison",
        "O'Banion", "O'Brien", "Richards", "Silva", "Watkins", "Xi",
        "Wheeler", "Willis", "brown, sr", "browne, III", "browne, IV",
        "knight", "mitchell", "o'daniel", "bevan", "evans", "D'Souza",
        "Hoyle-Johnson", "Vaughan Williams", "de Sousa", "de la Mare II"
    )
    for (name in names) { 
        var name2 = nysiis(name)
        if (name2.length > 6) name2 = "${name2.take(6)}(${name2.drop(6)})"
        println("${name.padEnd(16)} : $name2")
    }
}
```


```txt

Bishop           : BASAP
Carlson          : CARLSA(N)
Carr             : CAR
Chapman          : CAPNAN
Franklin         : FRANCL(AN)
Greene           : GRAN
Harper           : HARPAR
Jacobs           : JACAB
Larson           : LARSAN
Lawrence         : LARANC
Lawson           : LASAN
Louis, XVI       : LA
Lynch            : LYNC
Mackenzie        : MCANSY
Matthews         : MATA
May jnr          : MY
McCormack        : MCARNA(C)
McDaniel         : MCDANA(L)
McDonald         : MCDANA(LD)
Mclaughlin       : MCLAGL(AN)
Morrison         : MARASA(N)
O'Banion         : OBANAN
O'Brien          : OBRAN
Richards         : RACARD
Silva            : SALV
Watkins          : WATCAN
Xi               : X
Wheeler          : WALAR
Willis           : WAL
brown, sr        : BRAN
browne, III      : BRAN
browne, IV       : BRAN
knight           : NAGT
mitchell         : MATCAL
o'daniel         : ODANAL
bevan            : BAFAN
evans            : EVAN
D'Souza          : DSAS
Hoyle-Johnson    : HAYLAJ(ANSAN)
Vaughan Williams : VAGANW(ALAN)
de Sousa         : DASAS
de la Mare II    : DALANA(R)

```



## Perl

```perl
sub no_suffix {
    my($name) = @_;
    $name =~ s/\h([JS]R)|([IVX]+)$//i;
    return uc $name;
}

sub nysiis {
    my($name) = @_;
    local($_) = uc $name;

    s/[^A-Z]//g;
    s/^MAC/MCC/;
    s/^P[FH]/FF/;
    s/^SCH/SSS/;
    s/^KN/N/;
    s/[IE]E$/Y/;
    s/[DRN]T$/D/;
    s/[RN]D$/D/;
    s/(.)EV/$1AF/g;
    s/(.)[AEIOU]+/$1A/g;
    s/(.)Q/$1G/g;
    s/(.)Z/$1S/g;
    s/(.)M/$1N/g;
    s/(.)KN/$1N/g;
    s/(.)K/$1C/g;
    s/(.)SCH/$1S/g;
    s/(.)PF/$1F/g;
    s/(.)K/$1C/g;
    s/(.)H([^AEIOU])/$1$2/g;
    s/([^AEIOU])H/$1/g;
    s/(.)W/$1/g;
    s/AY$/Y/;
    s/S$//;
    s/A$//;
    s/(.)\1+/$1/g;
    return $_;
}

for (
    "knight",     "mitchell",  "o'daniel",    "brown sr",   "browne III",
    "browne IV",  "O'Banion",  "Mclaughlin",  "McCormack",  "Chapman",
    "Silva",      "McDonald",  "Lawson",      "Jacobs",     "Greene",
    "O'Brien",    "Morrison",  "Larson",      "Willis",     "Mackenzie",
    "Carr",       "Lawrence",  "Matthews",    "Richards",   "Bishop",
    "Franklin",   "McDaniel",  "Harper",      "Lynch",      "Watkins",
    "Carlson",    "Wheeler",   "Louis XVI"
) {
    my $nysiis = nysiis no_suffix $_;
    $nysiis =~ s/^(......)(.*)$/$1\[$2\]/ if length($nysiis) > 6;
    printf "%10s,  %s\n", $_, $nysiis;
}

```

<pre style="height:35ex">    knight,  NAGT
  mitchell,  MATCAL
  o'daniel,  ODANAL
  brown sr,  BRAN
browne III,  BRAN
 browne IV,  BRAN
  O'Banion,  OBANAN
Mclaughlin,  MCLAGL[AN]
 McCormack,  MCARNA[C]
   Chapman,  CAPNAN
     Silva,  SALV
  McDonald,  MCDANA[LD]
    Lawson,  LASAN
    Jacobs,  JACAB
    Greene,  GRAN
   O'Brien,  OBRAN
  Morrison,  MARASA[N]
    Larson,  LARSAN
    Willis,  WAL
 Mackenzie,  MCANSY
      Carr,  CAR
  Lawrence,  LARANC
  Matthews,  MAT
  Richards,  RACARD
    Bishop,  BASAP
  Franklin,  FRANCL[AN]
  McDaniel,  MCDANA[L]
    Harper,  HARPAR
     Lynch,  LYNC
   Watkins,  WATCAN
   Carlson,  CARLSA[N]
   Wheeler,  WALAR
 Louis XVI,  L
```



## Perl 6

This implementation removes common name suffixes similar to the reference implementation, even though it is not specified in the task description or on the linked [[wp:New York State Identification and Intelligence System|NYSIIS]] page. This algorithm isn't too friendly to certain French kings. :)


```perl6
sub no_suffix ($name) {
    $name.uc.subst: /\h (<[JS]>R) | (<[IVX]>+) $/, '';
}

sub nysiis ($name is copy) {
    given $name .= uc {
        s:g/<-[A..Z]>//;
        s/^MAC/MCC/;
        s/^P<[FH]>/FF/;
        s/^SCH/SSS/;
        s/^KN/N/;
        s/<[IE]>E$  /Y/;
        s/<[DRN]>T$ /D/;
        s/<[RN]>D$  /D/;
        s:c(1):g/EV/AF/;
        s:c(1):g/<[AEIOU]>+/A/;
        s:c(1):g/Q/G/;
        s:c(1):g/Z/S/;
        s:c(1):g/M/N/;
        s:c(1):g/KN/N/;
        s:c(1):g/K/C/;
        s:c(1):g/SCH/S/;
        s:c(1):g/PF/F/;
        s:c(1):g/K/C/;
        s:c(1):g/H(<-[AEIOU]>)/$0/;
        s:g/(<-[AEIOU]>)H/$0/;
        s:g/(.)W/$0/;
        s/ AY$ /Y/;
        s/  S$ //;
        s/  A$ //;
        s:g/ (.)$0+ /$0/;
     }
     return $name;
}


for «
    knight      mitchell        "o'daniel"      "brown  sr"     "browne III"
    "browne IV" "O'Banion"      Mclaughlin      McCormack       Chapman
    Silva       McDonald        Lawson          Jacobs          Greene
    "O'Brien"   Morrison        Larson          Willis          Mackenzie
    Carr        Lawrence        Matthews        Richards        Bishop
    Franklin    McDaniel        Harper          Lynch           Watkins
    Carlson     Wheeler         "Louis XVI"
» {
    my $nysiis = nysiis no_suffix $_;
    if $nysiis.chars > 6 {
        $nysiis .= subst: rx/ <after .**6> .+ /, -> $/ { "[$/]" };
    }
    printf "%10s,  %s\n", $_, $nysiis;
}
```


Output:


```txt

    knight,  NAGT
  mitchell,  MATCAL
  o'daniel,  ODANAL
  brown sr,  BRAN
browne III,  BRAN
 browne IV,  BRAN
  O'Banion,  OBANAN
Mclaughlin,  MCLAGL[AN]
 McCormack,  MCARNA[C]
   Chapman,  CAPNAN
     Silva,  SALV
  McDonald,  MCDANA[LD]
    Lawson,  LASAN
    Jacobs,  JACAB
    Greene,  GRAN
   O'Brien,  OBRAN
  Morrison,  MARASA[N]
    Larson,  LARSAN
    Willis,  WAL
 Mackenzie,  MCANSY
      Carr,  CAR
  Lawrence,  LARANC
  Matthews,  MAT
  Richards,  RACARD
    Bishop,  BASAP
  Franklin,  FRANCL[AN]
  McDaniel,  MCDANA[L]
    Harper,  HARPAR
     Lynch,  LYNC
   Watkins,  WATCAN
   Carlson,  CARLSA[N]
   Wheeler,  WALAR
 Louis XVI,  L

```



## Phix

```Phix
function isVowel(integer byte)
    return find(byte,"AEIOU")!=0
end function
 
function isRoman(string s)
    if s == "" then
        return false
    end if
    for i=1 to length(s) do
        if not find(s[i],"IVX") then
            return false
        end if
    end for
    return true
end function
 
function nysiis(string word)
    if word == "" then return "" end if
    word = upper(word)
    sequence ww = split_any(word, ", ", no_empty:=true)
    if length(ww)>1 then
        string last = ww[$]
        if isRoman(last) then
            word = word[1..-length(last)-1]
        end if
    end if
    word = substitute_all(word, " ,'-", repeat("",4))
    sequence eStrs = {"JR", "JNR", "SR", "SNR"}
    for i=1 to length(eStrs) do
        string ei = eStrs[i]
        integer lei = length(ei)
        if length(word)>lei
        and word[-lei..$]=ei then
            word = word[1..-lei-1]
        end if
    end for
    sequence fStrs = {{"MAC","MCC"}, {"KN","N"}, {"K","C"},
                      {"PH","FF"}, {"PF","FF"}, {"SCH","SSS"}}
    for i=1 to length(fStrs) do
        string {fi,rfi} = fStrs[i]
        integer lfi = length(fi)
        if length(word)>lfi
        and word[1..lfi]=fi then
            word[1..lfi] = rfi
        end if
    end for
    if length(word)>=2 then
        string l2 = word[-2..-1]
        if find(l2,{"EE","IE"}) then
            word[-2..-1] = "Y"
        elsif find(l2,{"DT","RT","RD","NT","ND"}) then
            word[-2..-1] = "D"
        end if
    end if
    integer initial = word[1]
    string key = word[1..1]
    word = word[2..$]
    word = substitute_all(word,{"EV","KN","SCH","PH"},
                               {"AF","N", "SSS","FF"})
    string sb = key&word
    integer le := length(sb)
    for i=2 to le do
        switch sb[i] do
            case 'E', 'I', 'O', 'U':    sb[i] = 'A'
            case 'Q':                   sb[i] = 'G'
            case 'Z':                   sb[i] = 'S'
            case 'M':                   sb[i] = 'N'
            case 'K':                   sb[i] = 'C'
            case 'H':   if (i> 1 and not isVowel(sb[i-1]))
                        or (i<le and not isVowel(sb[i+1])) then
                            sb[i] = sb[i-1]
                        end if
            case 'W':   if isVowel(sb[i-1]) then
                            sb[i] = sb[i-1]
                        end if
        end switch
    end for
    integer prev := initial
    for j=2 to le do
        integer c := sb[j]
        if prev != c then
            key &= c
            prev = c
        end if
    end for
    if length(key)>=1 and key[$]      == 'S'  then  key[$  ..$] = ""    end if
    if length(key)>=2 and key[-2..-1] == "AY" then  key[$-1..$] = "Y"   end if
    if length(key)>=1 and key[$]      == 'A'  then  key[$  ..$] = ""    end if
    return key
end function
 
constant tests = {
                  { "Bishop", "BASAP" },
                  { "Carlson", "CARLSAN" },
                  { "Carr", "CAR" },
                  { "Chapman", "CAPNAN" },
                  { "Franklin", "FRANCLAN" },
                  { "Greene", "GRAN" },
                  { "Harper", "HARPAR" },
                  { "Jacobs", "JACAB" },
                  { "Larson", "LARSAN" },
                  { "Lawrence", "LARANC" },
                  { "Lawson", "LASAN" },
                  { "Louis, XVI", "L" },    -- (see note)
                  { "Lynch", "LYNC" },
                  { "Mackenzie", "MCANSY" },
                  { "Matthews", "MAT" },    -- (see note)
                  { "May jnr", "MY" },
                  { "McCormack", "MCARNAC" },
                  { "McDaniel", "MCDANAL" },
                  { "McDonald", "MCDANALD" },
                  { "Mclaughlin", "MCLAGLAN" },
                  { "Morrison", "MARASAN" },
                  { "O'Banion", "OBANAN" },
                  { "O'Brien", "OBRAN" },
                  { "Richards", "RACARD" },
                  { "Silva", "SALV" },
                  { "Watkins", "WATCAN" },
                  { "Wheeler", "WALAR" },
                  { "Willis", "WAL" },      -- (see note)
                  { "Xi", "X" },
                  { "bevan", "BAFAN" },
                  { "brown, sr", "BRAN" },
                  { "brown sr", "BRAN" },
                  { "browne, III", "BRAN" },
                  { "browne, IV", "BRAN" },
                  { "evans", "EVAN" },
                  { "knight", "NAGT" },
                  { "mitchell", "MATCAL" },
                  { "o'daniel", "ODANAL" },
                  { "D'Souza", "DSAS" },
                  { "de Sousa", "DASAS" },
                  { "Hoyle-Johnson", "HAYLAJANSAN" },
                  { "Vaughan Williams", "VAGANWALAN" },
                  { "de la Mare II", "DALANAR" } }

integer errors = 0
for i=1 to length(tests) do
    string {name,expected} = tests[i],
            name2 := nysiis(name)
    if name2!=expected then
        errors += 1
        if length(name2) > 6 then
            name2 = sprintf("%s(%s)", {name2[1..6], name2[7..$]})
        end if
        printf(1,"%-16s : %s\n", {name, name2})
    end if
end for
printf(1,"All tests completed, %d errors\n",errors)
```

Note: After some careful consideration, I have decided that all three (see note) tests <i>are</i> in fact correct, or at least follow wp, specifically step 6 <i>before</i> step 7.
```txt

All tests completed, 0 errors

```



## Python

A literal translation of the algorithm from the [[wp:New York State Identification and Intelligence System|Wikipedia article]].

```python
import re

_vowels = 'AEIOU'

def replace_at(text, position, fromlist, tolist):
    for f, t in zip(fromlist, tolist):
        if text[position:].startswith(f):
            return ''.join([text[:position],
                            t,
                            text[position+len(f):]])
    return text

def replace_end(text, fromlist, tolist):
    for f, t in zip(fromlist, tolist):
        if text.endswith(f):
            return text[:-len(f)] + t
    return text

def nysiis(name):
    name = re.sub(r'\W', '', name).upper()
    name = replace_at(name, 0, ['MAC', 'KN', 'K', 'PH', 'PF', 'SCH'],
                               ['MCC', 'N',  'C', 'FF', 'FF', 'SSS'])
    name = replace_end(name, ['EE', 'IE', 'DT', 'RT', 'RD', 'NT', 'ND'],
                             ['Y',  'Y',  'D',  'D',  'D',  'D',  'D'])
    key, key1 = name[0], ''
    i = 1
    while i < len(name):
        #print(i, name, key1, key)
        n_1, n = name[i-1], name[i]
        n1_ = name[i+1] if i+1 < len(name) else ''
        name = replace_at(name, i, ['EV'] + list(_vowels), ['AF'] + ['A']*5)
        name = replace_at(name, i, 'QZM', 'GSN')
        name = replace_at(name, i, ['KN', 'K'], ['N', 'C'])
        name = replace_at(name, i, ['SCH', 'PH'], ['SSS', 'FF'])
        if n == 'H' and (n_1 not in _vowels or n1_ not in _vowels):
            name = ''.join([name[:i], n_1, name[i+1:]])
        if n == 'W' and n_1 in _vowels:
            name = ''.join([name[:i], 'A', name[i+1:]])
        if key and key[-1] != name[i]:
            key += name[i]
        i += 1
    key = replace_end(key, ['S', 'AY', 'A'], ['', 'Y', ''])
    return key1 + key

if __name__ == '__main__':
    names = ['Bishop', 'Carlson', 'Carr', 'Chapman', 'Franklin',
             'Greene', 'Harper', 'Jacobs', 'Larson', 'Lawrence',
             'Lawson', 'Louis, XVI', 'Lynch', 'Mackenzie', 'Matthews',
             'McCormack', 'McDaniel', 'McDonald', 'Mclaughlin', 'Morrison',
             "O'Banion", "O'Brien", 'Richards', 'Silva', 'Watkins',
             'Wheeler', 'Willis', 'brown, sr', 'browne, III', 'browne, IV',
             'knight', 'mitchell', "o'daniel"]
    for name in names:
        print('%15s: %s' % (name, nysiis(name)))
```

```txt
         Bishop: BASAP
        Carlson: CARLSAN
           Carr: CAR
        Chapman: CAPNAN
       Franklin: FRANCLAN
         Greene: GRAN
         Harper: HARPAR
         Jacobs: JACAB
         Larson: LARSAN
       Lawrence: LARANC
         Lawson: LASAN
     Louis, XVI: LASXV
          Lynch: LYNC
      Mackenzie: MCANSY
       Matthews: MATA
      McCormack: MCARNAC
       McDaniel: MCDANAL
       McDonald: MCDANALD
     Mclaughlin: MCLAGLAN
       Morrison: MARASAN
       O'Banion: OBANAN
        O'Brien: OBRAN
       Richards: RACARD
          Silva: SALV
        Watkins: WATCAN
        Wheeler: WALAR
         Willis: WALA
      brown, sr: BRANSR
    browne, III: BRAN
     browne, IV: BRANAV
         knight: NAGT
       mitchell: MATCAL
       o'daniel: ODANAL
```



## Racket


This is a translation of [[Python]] to ensure that these results are consistent.
This allows them to be tested against a known output from another solution.

If any of the <code>check-equal?</code>s fails, it will print an error message.
None of them fail, so no output is seen.

I&rsquo;ve gone out of my way to number the rules &mdash; which may make it a little
verbose.


```racket
#lang racket/base
(require racket/string racket/match)
(define (str-rplc-at str replacement start (end (string-length str)))
  (string-append (substring str 0 start) replacement (substring str end)))

(define (split-on-commas s) (string-split s "," #:trim? #f))

(define (str-rplc-at* fxt pos . more)
  (match more
    [(list (app split-on-commas froms) (app split-on-commas tos) even-more ...)
     (define txt-maybe
       (for/first ((f froms) (t tos) #:when (string-prefix? (substring fxt pos) f))
         (str-rplc-at fxt t pos (+ pos (string-length f)))))
     (apply str-rplc-at* (or txt-maybe fxt) pos even-more)]
    [_ fxt]))

(define (str-rplc-end* txf . more)
  (match more
    [(list (app split-on-commas froms) (app split-on-commas tos) even-more ...)
     (define txt-maybe
       (for/first ((f froms) (t tos) #:when (string-suffix? txf f))
         (str-rplc-at txf t (- (string-length txf) (string-length f)))))
     (apply str-rplc-end* (or txt-maybe txf) even-more)]
    [_ txf]))

(define vowels '("A" "E" "I" "O" "U"))
(define (vowel? s) (member s vowels))

;; ---------------------------------------------------------------------------------------------------
(define (normalise n) (regexp-replace* #px"\\W" (string-upcase n) ""))

(define (r:1 n) (str-rplc-at* n 0 "MAC,KN,K,PH,PF,SCH" "MCC,N,C,FF,FF,SSS"))

(define (r:2 n) (str-rplc-end* n "EE,IE,DT,RT,RD,NT,ND" "Y,Y,D,D,D,D,D"))

(define (r:3/4 in)
  (define (loop-4 name-4.1 key-3 i)
    (cond
      [(< i (string-length name-4.1))
       (define name-4.2/3/4
         (str-rplc-at* name-4.1 i "EV,A,E,I,O,U" "AF,A,A,A,A,A" #|4.1|# "Q,Z,M" "G,S,N" #|4.2|#
                       "KN,K" "N,C" #|4.3|# "SCH,PH" "SSS,FF" #|4.4|#))
       (define name-4.5/6
         (match-let ([(or (regexp "^(.)(.)(.)" (list n_1 n n1_))
                          (regexp "^(.)(.)" (list (app (λ _ "") n1_) n_1 n)))
                      (substring name-4.1 (sub1 i))])
           (match n
             ["H" #:when (or (not (vowel? n_1)) (not (vowel? n1_)))
                  (str-rplc-at name-4.2/3/4 n_1 i (add1 i))] ; 4.5
             ["W" #:when (vowel? n_1) (str-rplc-at name-4.2/3/4 "A" i (add1 i))] ; 4.6
             [_ name-4.2/3/4])))
       (define name-4.6_i (substring name-4.5/6 i (add1 i)))
       (define key-4.7 (if (string=? name-4.6_i (substring key-3 (sub1 (string-length key-3))))
                           key-3 (string-append key-3 name-4.6_i)))
       (loop-4 name-4.5/6 key-4.7 (add1 i))]
      [else key-3]))
  (loop-4 in (substring in 0 1) 1))

(define (r:5/6/7/8 n) (str-rplc-end* n "S,AY,A" ",Y,"))

(define r:9 (match-lambda [(regexp #px"^(.{6})(.+)" (list _ l r)) (format "~a[~a]" l r)] [n n]))

(define nysiis (apply compose (reverse (list normalise r:1 r:2 r:3/4 r:5/6/7/8 r:9))))

(module+ test
  (require rackunit)
  (define names
    (list "Bishop" "Carlson" "Carr" "Chapman" "Franklin" "Greene" "Harper" "Jacobs" "Larson"
          "Lawrence" "Lawson" "Louis, XVI" "Lynch" "Mackenzie" "Matthews" "McCormack" "McDaniel"
          "McDonald" "Mclaughlin" "Morrison" "O'Banion" "O'Brien" "Richards" "Silva" "Watkins"
          "Wheeler" "Willis" "brown, sr" "browne, III" "browne, IV" "knight" "mitchell" "o'daniel"))

  (define py-nysiis-names ; results from python (with [] added)
    (list "BASAP" "CARLSA[N]" "CAR" "CAPNAN" "FRANCL[AN]" "GRAN" "HARPAR" "JACAB" "LARSAN" "LARANC"
          "LASAN" "LASXV" "LYNC" "MCANSY" "MATA" "MCARNA[C]" "MCDANA[L]" "MCDANA[LD]" "MCLAGL[AN]"
          "MARASA[N]" "OBANAN" "OBRAN" "RACARD" "SALV" "WATCAN" "WALAR" "WALA" "BRANSR" "BRAN"
          "BRANAV" "NAGT" "MATCAL" "ODANAL"))

  (for ((n names) (p py-nysiis-names))
    (check-equal? (nysiis n) p (format "(nysiis ~s) = ~s" n p))))
```



## REXX

This REXX version allows a blank to be inserted into names by using an underscore or underbar character   [ <big><big>'''_'''</big></big> ].

Code was added to the REXX program to allow various titles.

Any post-nominal letters (generational, honorific, professional,or other)   ending in a period is ignored as well as most Roman numeral letters. 

If the rule of only returning (up to) six characters is to be enforced, then the last REXX statement should be
replaced with: 

```rexx
return strip( left(key, 6) )                     /*return the leftmost six characters.  */
```


```rexx
/*REXX program implements the  NYSIIS  phonetic algorithm  (for various test names).    */
names="Bishop brown_sr browne_III browne_IV Carlson Carr Chapman D'Souza de_Sousa Franklin",
      "Greene Harper Hoyle-Johnson Jacobs knight Larson Lawrence Lawson Louis_XVI Lynch",
      "Mackenzie Marshall,ESQ Matthews McCormack McDaniel McDonald Mclaughlin mitchell Morrison",
      "O'Banion O'Brien o'daniel Richards Silva Vaughan_Williams Watkins Wheeler Willis Xavier,MD."
arg z;  if z=''  then z=names                    /*obtain optional name list from the CL*/

        do i=1  for words(z)                     /*process each name (word) in the list.*/
        xx=translate( word(z,i), , '_')          /*reconstitute any blanks using TRANS. */
        say right(xx, 35)   ' ──► '   nysiis(xx) /*display some stuff to the terminal.  */
        end   /*i*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
$:     p=substr(x,j-1,1) /*prev*/;  n=substr(x,j+1,1) /*next*/;  return substr(x,j,arg(1))
vowel: return  pos(arg(1), 'AEIOUaeiou') \== 0   /*returns 1 if the argument has a vowel*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
nysiis:  procedure;  arg x;  x=space( translate(x, , ',')) /*elide commas, excess blanks*/
w=words(x);          Lw=word(x, w)               /*pick off the last word in name list. */
titles= 'ESQ JNR JR SNR SR'                      /* [↓]  last word post─nominal letters?*/
if w\==1  then if pos('IL', lw)==0  then                      /*disallow IL as Roman #. */
                          if pos(., x)\==0             |,     /*Sr.  Jr.  Esq.  ... ?   */
                             datatype(left(Lw,1), 'W') |,     /*2nd  3rd  4th   ... ?   */
                             verify(Lw, 'IVXL') ==0    |,     /*Roman numeral suffix?   */
                             wordpos(x, titles)\==0  then x=subword(x, 1, w-1)
x=space(x, 0)                                    /*remove all whitespace from the name. */
if left(x, 3)=='MAC'                   then x= 'MCC'substr(x, 4)     /*start with MAC ? */
if left(x, 2)=='KN'                    then x=   'N'substr(x, 3)     /*  "     "  KN  ? */
if left(x, 1)=='K'                     then x=   'C'substr(x, 2)     /*  "     "  K   ? */
if left(x, 2)=='PH' | left(x,2)=='PF'  then x=  'FF'substr(x, 3)     /*  "     "  PH,PF?*/
if left(x, 3)=='SCH'                   then x= 'SSS'substr(x, 4)     /*  "     "  SCH ? */
r2=right(x, 2)
if wordpos(r2, 'EE IE')         \==0   then x= left(x,length(x)-2)"Y"  /*ends with ··· ?*/
if wordpos(r2, 'DT RT RD NT ND')\==0   then x= left(x,length(x)-2)"D"  /*  "    "   "  "*/
key=left(x, 1)                                                         /*use first char.*/

   do j=2  to length(x);  if \datatype($(1),'U')  then iterate /*¬ Latin letter? Skip it*/
   if $(2)=='EV'   then x=overlay("F", x, j+1)                 /*have an  EV ?   Use F  */
                   else x=overlay( translate($(1), 'AAAAGSN', "EIOUQZM"), x, j)
   if $(2)=='KN'   then x=left(x, j-1)"N"substr(x, j+1)        /*have a   KN ?   Use N  */
                   else if $(1)=="K"  then x=overlay('C',x,j)  /*  "  "   K  ?   Use C  */
   if $(3)=='SCH'  then x=overlay("SSS", x, j)                 /*  "  "   SCH?   Use SSS*/
   if $(2)=='PH'   then x=overlay("FF",  x, j)                 /*  "  "   PH ?   Use FF */
   if $(1)=='H'    then if \vowel(p) | \vowel(n)  then x=overlay( p , x, j)
   if $(1)=='W'    then if  vowel(p)              then x=overlay("A", x, j)
   if $(1)\==right(key, 1)                        then key=key || $(1)  /*append to KEY.*/
   end   /*j*/
                                                                        /* [↓]  elide:  */
if right(key, 1)=='S'   then key=left(key, max(1, length(key) -1))      /*ending S      */
if right(key, 2)=='AY'  then key=left(key,        length(key) -2)"Y"    /*  "    A in AY*/
if right(key, 1)=='A'   then key=left(key, max(1, length(key) -1))      /*  "    A      */
return strip(key)                                /*return the whole key  (all of it).   */
```

'''output''' when using the default input(s):

```txt

                             Bishop  ──►  BASAP
                           brown sr  ──►  BRANSR
                         browne III  ──►  BRAN
                          browne IV  ──►  BRAN
                            Carlson  ──►  CARLSAN
                               Carr  ──►  CAR
                            Chapman  ──►  CAPNAN
                            D'Souza  ──►  DSAS
                           de Sousa  ──►  DASAS
                           Franklin  ──►  FRANCLAN
                             Greene  ──►  GRAN
                             Harper  ──►  HARPAR
                      Hoyle-Johnson  ──►  HAYLAJANSAN
                             Jacobs  ──►  JACAB
                             knight  ──►  NAGT
                             Larson  ──►  LARSAN
                           Lawrence  ──►  LARANC
                             Lawson  ──►  LASAN
                          Louis XVI  ──►  L
                              Lynch  ──►  LYNC
                          Mackenzie  ──►  MCANSY
                       Marshall,ESQ  ──►  MARSALASG
                           Matthews  ──►  MAT
                          McCormack  ──►  MCARNAC
                           McDaniel  ──►  MCDANAL
                           McDonald  ──►  MCDANALD
                         Mclaughlin  ──►  MCLAGLAN
                           mitchell  ──►  MATCAL
                           Morrison  ──►  MARASAN
                           O'Banion  ──►  OBANAN
                            O'Brien  ──►  OBRAN
                           o'daniel  ──►  ODANAL
                           Richards  ──►  RACARD
                              Silva  ──►  SALV
                   Vaughan Williams  ──►  VAGANWALAN
                            Watkins  ──►  WATCAN
                            Wheeler  ──►  WALAR
                             Willis  ──►  WAL
                         Xavier,MD.  ──►  XAVAR

```



## Tcl


```tcl
proc nysiis {name {truncate false}} {
    # Normalize to first word, uppercased, without non-letters
    set name [regsub -all {[^A-Z]+} [string toupper [regexp -inline {\S+} $name]] ""]
    # Prefix map
    foreach {from to} {MAC MCC KN N K C PH FF PF FF SCH SSS} {
	if {[regsub ^$from $name $to name]} break
    }
    # Suffix map
    foreach {from to} {EE Y IE Y DT D RT D NT D ND D} {
	if {[regsub $from$ $name $to name]} break
    }
    # Split
    regexp (.)(.*) $name -> name rest
    # Reduce suffix
    regsub -all {[AEIOU]} [regsub -all EV $rest AF] A rest
    set rest [string map {Q G Z S M N KN N K C SCH SSS PH FF} $rest]
    regsub -all {([^A])H|(.)H(?=[^A])} $rest {\1\2} rest
    regsub -all AW $rest A rest
    regsub -all {(.)\1+} $rest {\1} rest
    regsub {S$} $rest "" rest
    regsub {A(Y?)$} $rest {\1} rest
    append name $rest
    # Apply truncation if needed
    if {$truncate} {
	set name [string range $name 0 5]
    }
    return $name
}
```

Demonstrating:

```tcl
foreach name {
    knight      mitchell        "o'daniel"      "brown  sr"     "browne III"
    "browne IV" "O'Banion"      Mclaughlin      McCormack       Chapman
    Silva       McDonald        Lawson          Jacobs          Greene
    "O'Brien"   Morrison        Larson          Willis          Mackenzie
    Carr        Lawrence        Matthews        Richards        Bishop
    Franklin    McDaniel        Harper          Lynch           Watkins
    Carlson     Wheeler         "Louis XVI"
} {
    puts "$name -> [nysiis $name]"
}
```

```txt

knight -> NAGT
mitchell -> MATCAL
o'daniel -> ODANAL
brown  sr -> BRAN
browne III -> BRAN
browne IV -> BRAN
O'Banion -> OBANAN
Mclaughlin -> MCLAGLAN
McCormack -> MCARNAC
Chapman -> CHAPNAN
Silva -> SALV
McDonald -> MCDANALD
Lawson -> LASAN
Jacobs -> JACAB
Greene -> GRAN
O'Brien -> OBRAN
Morrison -> MARASAN
Larson -> LARSAN
Willis -> WAL
Mackenzie -> MCANSY
Carr -> CAR
Lawrence -> LARANC
Matthews -> MAT
Richards -> RACARD
Bishop -> BASAP
Franklin -> FRANCLAN
McDaniel -> MCDANAL
Harper -> HARPAR
Lynch -> LYNC
Watkins -> WATCAN
Carlson -> CARLSAN
Wheeler -> WHALAR
Louis XVI -> L

```



## zkl

```zkl
fcn replaceAt(text,pos,fromList,toList){
   foreach f,t in (fromList.zip(toList)){
      if(0==text[pos,*].find(f)) return(text.set(pos,f.len(),t));
   }
   text
}
fcn replaceEnd(text,fromList,toList){
   foreach f,t in (fromList.zip(toList)){
      if ((text.len() - f.len())==text.rfind(f)) return(text.set(-f.len(),*,t));
   }
   text
}
 
fcn nysiis(name){
   vowels := "AEIOU";
//   name = name.filter(fcn(c){ (not c.isSpace()) }).toUpper();
   name = name.toUpper().filter("matches","[A-Z]");
   name = replaceAt(name,0, T("MAC", "KN", "K", "PH", "PF", "SCH"),
                            T("MCC", "N",  "C", "FF", "FF", "SSS"));
   name = replaceEnd(name,T("EE", "IE", "DT", "RT", "RD", "NT", "ND"),
                          T("Y",  "Y",  "D",  "D",  "D",  "D",  "D"));
   key := name[0];
   foreach i in ([1 .. name.len()-1]){
      n_1,n,n1b:= name[i-1], name[i], name[i+1,1]; // "" if i+1>len
      name  = replaceAt(name,i, T("EV").extend(vowels.split("")), 
                                T("AF","A","A","A","A","A"));
      name  = replaceAt(name,i, T("Q","Z","M"), T("G","S","N"));
      name  = replaceAt(name,i, T("KN", "K"), T("N", "C"));
      name  = replaceAt(name,i, T("SCH", "PH"), T("SSS", "FF"));
      if (n=="H" and (not vowels.holds(n_1) or not vowels.holds(n1b)))
            name = name.set(i,1,n_1);
      if (n=="W" and vowels.holds(n_1)) name = name.set(i,1,"A");
      if (key[-1,1] != name[i]) key += name[i];
   }
   replaceEnd(key, T("S", "AY", "A"), T("", "Y", ""));
}
```


```zkl
names := T("Bishop", "Carlson", "Carr", "Chapman",
        "Franklin", "Greene", "Harper", "Jacobs", "Larson", "Lawrence",
        "Lawson", "Louis, XVI", "Lynch", "Mackenzie", "Matthews",
         "McCormack", "McDaniel", "McDonald", "Mclaughlin", "Morrison",
         "O'Banion", "O'Brien", "Richards", "Silva", "Watkins",
         "Wheeler", "Willis", "brown, sr", "browne, III", "browne, IV",
         "knight", "mitchell", "o'daniel");
 
foreach name in (names){ println("%11s: %s".fmt(name, nysiis(name))) }
```

```txt

     Bishop: BASAP
    Carlson: CARLSAN
       Carr: CAR
    Chapman: CAPNAN
   Franklin: FRANCLAN
     Greene: GRAN
     Harper: HARPAR
     Jacobs: JACAB
     Larson: LARSAN
   Lawrence: LARANC
     Lawson: LASAN
 Louis, XVI: LASXV
      Lynch: LYNC
  Mackenzie: MCANSY
   Matthews: MATA
  McCormack: MCARNAC
   McDaniel: MCDANAL
   McDonald: MCDANALD
 Mclaughlin: MCLAGLAN
   Morrison: MARASAN
   O'Banion: OBANAN
    O'Brien: OBRAN
   Richards: RACARD
      Silva: SALV
    Watkins: WATCAN
    Wheeler: WALAR
     Willis: WALA
  brown, sr: BRANSR
browne, III: BRAN
 browne, IV: BRANAV
     knight: NAGT
   mitchell: MATCAL
   o'daniel: ODANAL

```

