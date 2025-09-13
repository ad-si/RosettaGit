+++
title = "Bacon cipher"
description = ""
date = 2019-10-11T01:28:56Z
aliases = []
[extra]
id = 19556
[taxonomies]
categories = ["Encryption", "task"]
tags = []
+++

## Task
[[wp: Bacon's cipher| Bacon's cipher]] is a method of steganography created by Francis Bacon.
This task is to implement a program for encryption and decryption of plaintext using the simple alphabet of the Baconian cipher or some other kind of representation of this alphabet (make anything signify anything).

The Baconian alphabet:

 a   AAAAA   g     AABBA   n    ABBAA   t     BAABA
 b   AAAAB   h     AABBB   o    ABBAB   u-v   BAABB
 c   AAABA   i-j   ABAAA   p    ABBBA   w     BABAA
 d   AAABB   k     ABAAB   q    ABBBB   x     BABAB
 e   AABAA   l     ABABA   r    BAAAA   y     BABBA
 f   AABAB   m     ABABB   s    BAAAB   z     BABBB

# The Baconian alphabet may optionally be extended to encode all lower case characters individually and/or adding a few punctuation characters such as the space.
# It is impractical to use the original change in font for the steganography. For this task you must provide an example that uses a change in the case of successive alphabetical characters instead. Other examples for the language are encouraged to explore alternative steganographic means.
# Show an example plaintext message encoded and then decoded here on this page.



## Agena

Tested with Agena 2.9.5 Win32
{{Trans|ALGOL 68}}

```agena
# Bacon cipher

# Bacon's letter codes but with distinct values for i & j and u & v and an extra for any non-letter
baconCodes := [ "A" ~ "AAAAA", "B" ~ "AAAAB", "C" ~ "AAABA", "D" ~ "AAABB", "E" ~ "AABAA"
              , "F" ~ "AABAB", "G" ~ "AABBA", "H" ~ "AABBB", "I" ~ "ABAAA", "J" ~ "ABAAB"
              , "K" ~ "ABABA", "L" ~ "ABABB", "M" ~ "ABBAA", "N" ~ "ABBAB", "O" ~ "ABBBA"
              , "P" ~ "ABBBB", "Q" ~ "BAAAA", "R" ~ "BAAAB", "S" ~ "BAABA", "T" ~ "BAABB"
              , "U" ~ "BABAA", "V" ~ "BABAB", "W" ~ "BABBA", "X" ~ "BABBB", "Y" ~ "BBAAA"
              , "Z" ~ "BBAAB", "*" ~ "BBBAA"
              ];
# yields plain text encoded via stego template
toBacon    :=
    proc( plainText :: string, stegoTemplate :: string ) :: string is
        local stegoPos   := 0;
        local stegoLen   := size stegoTemplate;
        # selects the next character from the stego template - wraps-around from the end to the beginning
        local nextStegoPos := proc() is inc stegoPos, 1; mod stegoPos, stegoLen end;
        # encode the plain text
        local encoded    := "";
        for pos to size plainText do
            # get the Bacon code of the next character #
            local plainChar := upper( plainText[ pos ] );
            local code := baconCodes[ if plainChar < "A" or plainChar > "Z" then "*" else plainChar fi ];
            for c to size code do
                # copy punctuation as is from the stego template to the result
                local s := upper( stegoTemplate[ stegoPos + 1 ] );
                while s < "A" or s > "Z" do
                    encoded := encoded & s;
                    nextStegoPos();
                    s       := if stegoPos < size stegoTemplate then upper( stegoTemplate[ stegoPos + 1 ] ) else "A" fi
                od;
                # encode the character by changing the case of the stego character as appropriate
                local templateChar := stegoTemplate[ stegoPos + 1 ];
                encoded := encoded & if code[ c ] = "A" then lower( templateChar ) else upper( templateChar ) fi;
                nextStegoPos()
            od
        od;
        return encoded
    end ; # toBacon
# yields bacon text decoded via stego template
toPlain := proc( baconText :: string, stegoTemplate :: string ) :: string is
    local decoded    := "";
    local codedChar  := 0;
    local letters    := 0;
    local codeLength := size baconCodes[ "A" ];
    for pos to size baconText do
        local c := baconText[ pos ];
        if c >= "a" and c <= "z"
        then
            # lower case letter
            mul codedChar, 2;
            inc letters,   1
        elif c >= "A" and c <= "Z"
        then
            # upper case letter
            mul codedChar, 2;
            inc codedChar, 1;
            inc letters,   1
        fi;
        if letters = codeLength
        then
            # have a full letter to decode
            decoded   := decoded & if codedChar > 25 then " " else char( abs( "a" ) + codedChar ) fi;
            letters   := 0;
            codedChar := 0
        fi
    od;
    return decoded
end ; # toPlain
# test encode and decode
scope
    local nl           := char( 10 );
    local testTemplate := "bacon's cipher is a method of steganography created by francis bacon."            & nl
                        & "this task is to implement a program for encryption and decryption of "            & nl
                        & "plaintext using the simple alphabet of the baconian cipher or some "              & nl
                        & "other kind of representation of this alphabet (make anything signify anything). " & nl
                        & "the baconian alphabet may optionally be extended to encode all lower "            & nl
                        & "case characters individually and/or adding a few punctuation characters "         & nl
                        & "such as the space."                                                               & nl
                        ;
    local plainText    := "the quick brown fox jumps over the lazy dog";
    local baconEncoded := toBacon( plainText,    testTemplate );
    local baconDecoded := toPlain( baconEncoded, testTemplate );
    print( "encoded..." );
    print( baconEncoded );
    print( "-----------------------------------------------------" );
    print( "decoded..." );
    print( baconDecoded );
    print( "-----------------------------------------------------" );
    print( if baconDecoded <> plainText then "UNSUCESSFUL" else "sucessful" fi, " decode" )
epocs
```

{{out}}

```txt

encoded...
BacON's cIPHer Is a METhoD of stEgAnogRaphy crEatEd By FRAncis baCOn.
thIs TASk Is TO imPLeMENT a proGrAm FOR eNcRYPTIOn anD deCRyPtioN Of
plAINTExt UsING the SIMpLe AlPhaBet Of thE BAConIan CIphER Or sOme
OTHer kInD Of reprESenTATion OF This alPHaBET (makE An
-----------------------------------------------------
decoded...
the quick brown fox jumps over the lazy dog
-----------------------------------------------------
sucessful	 decode

```



## ALGOL 68


```algol68
# Bacon's letter codes but with distinct values for i & j and u & v and an extra for any non-letter #
[]STRING bacon codes = ( #a# "AAAAA", "AAAAB", "AAABA", "AAABB", "AABAA", "AABAB", "AABBA", "AABBB", "ABAAA"
                       , #j# "ABAAB", "ABABA", "ABABB", "ABBAA", "ABBAB", "ABBBA", "ABBBB", "BAAAA", "BAAAB"
                       , #s# "BAABA", "BAABB", "BABAA", "BABAB", "BABBA", "BABBB", "BBAAA", "BBAAB", "BBBAA"
                       );
# operators to convert case #
OP LCASE = ( CHAR c )CHAR: IF c < "A" OR c > "Z" THEN c ELSE REPR ( ( ABS c - ABS "A" ) + ABS "a" ) FI;
OP UCASE = ( CHAR c )CHAR: IF c < "a" OR c > "z" THEN c ELSE REPR ( ( ABS c - ABS "a" ) + ABS "A" ) FI;
# yields plain text encoded via stego template #
PROC to bacon = ( STRING plain text, STRING stego template )STRING:
     BEGIN
         INT    stego pos   := 0;
         INT    stego len    = ( UPB stego template + 1 ) - LWB stego template;
         INT    stego start  = LWB stego template;
         # selects the next character from the stego template - wraps-around from the end to the beginning #
         PROC next stego pos = VOID: ( stego pos +:= 1; stego pos MODAB stego len);
         # encode the plain text #
         STRING encoded     := "";
         FOR pos FROM LWB plain text TO UPB plain text DO
             # get the Bacon code of the next character #
             CHAR   plain char = UCASE plain text[ pos ];
             STRING code =
                 bacon codes[ IF plain char < "A" OR plain char > "Z" THEN UPB bacon codes ELSE ( ABS plain char - ABS "A" ) + 1 FI ];
             FOR c FROM LWB code TO UPB code DO
                 # copy punctuation as is from the stego template to the result #
                 WHILE CHAR s := UCASE stego template[ stego pos + stego start ];
                       s < "A" OR s > "Z"
                 DO
                     encoded   +:= s;
                     next stego pos
                 OD;
                 # encode the character by changing the case of the stego character as appropriate #
                 CHAR template char = stego template[ stego pos + stego start ];
                 encoded +:= IF code[ c ] = "A" THEN LCASE template char ELSE UCASE template char FI;
                 next stego pos
             OD
         OD;
         encoded
     END ; # to bacon #
# yields bacon text decoded via stego template #
PROC to plain = ( STRING bacon text, stego template )STRING:
     BEGIN
         STRING decoded := "";
         INT coded char := 0;
         INT letters    := 0;
         INT code length = ( UPB bacon codes[ 1 ] - LWB bacon codes[ 1 ] ) + 1;
         FOR pos FROM LWB bacon text TO UPB bacon text DO
             CHAR c = bacon text[ pos ];
             IF c >= "a" AND c <= "z"
             THEN
                 # lower case letter #
                 coded char *:= 2;
                 letters    +:= 1
             ELIF c >= "A" AND c <= "Z"
             THEN
                 # upper case letter #
                 coded char *:= 2;
                 coded char +:= 1;
                 letters    +:= 1
             FI;
             IF letters = code length
             THEN
                 # have a full letter to decode #
                 decoded    +:= IF coded char > 25 THEN " " ELSE REPR ( ABS "a" + coded char ) FI;
                 letters     := 0;
                 coded char  := 0
             FI
         OD;
         decoded
     END ; # to plain #
# test encode and decode #
STRING test template = "bacon's cipher is a method of steganography created by francis bacon."            + REPR 10
                     + "this task is to implement a program for encryption and decryption of "            + REPR 10
                     + "plaintext using the simple alphabet of the baconian cipher or some "              + REPR 10
                     + "other kind of representation of this alphabet (make anything signify anything). " + REPR 10
                     + "the baconian alphabet may optionally be extended to encode all lower "            + REPR 10
                     + "case characters individually and/or adding a few punctuation characters "         + REPR 10
                     + "such as the space."                                                               + REPR 10
                     ;
STRING plain text    = "the quick brown fox jumps over the lazy dog";
STRING bacon encoded = to bacon( plain text,    test template );
STRING bacon decoded = to plain( bacon encoded, test template );
print( ( "encoded...", newline ) );
print( ( bacon encoded, newline ) );
print( ( "-----------------------------------------------------", newline, "decoded...", newline ) );
print( ( bacon decoded, newline ) );
print( ( "-----------------------------------------------------", newline ) );
print( ( IF bacon decoded /= plain text THEN "UNSUCESSFUL" ELSE "sucessful" FI, " decode", newline ) )
```

{{out}}

```txt

encoded...
BacON's cIPHer Is a METhoD of stEgAnogRaphy crEatEd By FRAncis baCOn.
thIs TASk Is TO imPLeMENT a proGrAm FOR eNcRYPTIOn anD deCRyPtioN Of
plAINTExt UsING the SIMpLe AlPhaBet Of thE BAConIan CIphER Or sOme
OTHer kInD Of reprESenTATion OF This alPHaBET (makE An
-----------------------------------------------------
decoded...
the quick brown fox jumps over the lazy dog
-----------------------------------------------------
sucessful decode

```



## BaCon

A Bacon cipher in [[BaCon]]. Using unique identifiers 'aaaaa'-'bbaab' for a-z and, as other examples on this page, using 'bbbaa' (28) for the space character.

```qbasic
msg$ = "the quick brown fox jumps over the lazy dog"

txt$ = "Bacon's cipher is a method of steganography created by Francis Bacon." \
"This task is to implement a program for encryption and decryption of plaintext " \
"using the simple alphabet of the Baconian cipher or some other kind of representation " \
"of this alphabet (make anything signify anything)."

position = 1

FOR x = 1 TO LEN(msg$)
    value = ASC(MID$(msg$, x, 1))-97

    IF value < 0 OR value > 25 THEN value = 28

    FOR b = 4 DOWNTO 0
        WHILE NOT(REGEX(MID$(txt$, position, 1), "[[:alpha:]]"))
            steg$ = steg$ & MID$(txt$, position, 1)
            INCR position
        WEND

        IF value & BIT(b) THEN
            steg$ = steg$ & UCASE$(MID$(txt$, position, 1))
        ELSE
            steg$ = steg$ & LCASE$(MID$(txt$, position, 1))
        END IF

        INCR position
    NEXT
NEXT

PRINT "Encoded:", NL$, steg$

position = 0

FOR x = 1 TO LEN(steg$)
    IF NOT(REGEX(MID$(steg$, x, 1), "[[:alpha:]]")) THEN CONTINUE

    letter = letter << 1

    IF ASC(MID$(steg$, x, 1))-97 < 0 THEN letter = letter | 1

    INCR position

    IF MOD(position, 5) = 0 THEN
        result$ = result$ & CHR$(IIF(letter=28, 32, letter+97))
        letter = 0
    END IF
NEXT

PRINT "Decoded:", NL$, result$
```

{{out}}

```txt
Encoded:
BacON's cIPHer Is a METhoD of stEgAnogRaphy crEatEd By FRAncis baCOn.thIs TASk Is TO imPLeMENT a proGrAm FOR eNcRYPTIOn anD deCRyPtioN Of plAINTExt UsING the SIMpLe AlPhaBet Of thE BAConIan CIphER Or sOme OTHer kInD Of reprESenTATion OF This alPHaBET (makE An
Decoded:
the quick brown fox jumps over the lazy dog
```



## C

{{trans|Kotlin}}

```c
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

/* maps successively from 'a' to 'z' plus ' ' to denote any non-letter */
char *codes[] = {
    "AAAAA", "AAAAB", "AAABA", "AAABB", "AABAA",
    "AABAB", "AABBA", "AABBB", "ABAAA", "ABAAB",
    "ABABA", "ABABB", "ABBAA", "ABBAB", "ABBBA",
    "ABBBB", "BAAAA", "BAAAB", "BAABA", "BAABB",
    "BABAA", "BABAB", "BABBA", "BABBB", "BBAAA",
    "BBAAB", "BBBAA"
};

char *get_code(const char c) {
    if (c >= 97 && c <= 122) return codes[c - 97];
    return codes[26];
}

char get_char(const char *code) {
    int i;
    if (!strcmp(codes[26], code)) return ' ';
    for (i = 0; i < 26; ++i) {
        if (strcmp(codes[i], code) == 0) return 97 + i;
    }
    printf("\nCode \"%s\" is invalid\n", code);
    exit(1);
}

void str_tolower(char s[]) {
    int i;
    for (i = 0; i < strlen(s); ++i) s[i] = tolower(s[i]);
}

char *bacon_encode(char plain_text[], char message[]) {
    int i, count;
    int plen = strlen(plain_text), mlen = strlen(message);
    int elen = 5 * plen;
    char c;
    char *p, *et, *mt;
    et = malloc(elen + 1);
    str_tolower(plain_text);
    for (i = 0, p = et; i < plen; ++i, p += 5) {
        c = plain_text[i];
        strncpy(p, get_code(c), 5);
    }
    *++p = '\0';

    /* 'A's to be in lower case, 'B's in upper case */
    str_tolower(message);
    mt = calloc(mlen + 1, 1);
    for (i = 0, count = 0; i < mlen; ++i) {
        c = message[i];
        if (c >= 'a' && c <= 'z') {
            if (et[count] == 'A')
                mt[i] = c;
            else
                mt[i] = c - 32;  /* upper case equivalent */
            if (++count == elen) break;
        }
        else mt[i] = c;
    }
    free(et);
    return mt;
}

char *bacon_decode(char cipher_text[]) {
    int i, count, clen = strlen(cipher_text);
    int plen;
    char *p, *ct, *pt;
    char c, quintet[6];
    ct = calloc(clen + 1, 1);
    for (i = 0, count = 0; i < clen; ++i) {
        c = cipher_text[i];
        if (c >= 'a' && c <= 'z')
            ct[count++] = 'A';
        else if (c >= 'A' && c <= 'Z')
            ct[count++] = 'B';
    }

    plen = strlen(ct) / 5;
    pt = malloc(plen + 1);
    for (i = 0, p = ct; i < plen; ++i, p += 5) {
        strncpy(quintet, p, 5);
        quintet[5] = '\0';
        pt[i] = get_char(quintet);
    }
    pt[plen] = '\0';
    free(ct);
    return pt;
}

int main() {
    char plain_text[] = "the quick brown fox jumps over the lazy dog";
    char message[] = "bacon's cipher is a method of steganography created by francis bacon."
        "this task is to implement a program for encryption and decryption of "
        "plaintext using the simple alphabet of the baconian cipher or some "
        "other kind of representation of this alphabet (make anything signify anything). "
        "the baconian alphabet may optionally be extended to encode all lower "
        "case characters individually and/or adding a few punctuation characters "
        "such as the space.";
    char *cipher_text, *hidden_text;
    cipher_text = bacon_encode(plain_text, message);
    printf("Cipher text ->\n\n%s\n", cipher_text);
    hidden_text = bacon_decode(cipher_text);
    printf("\nHidden text ->\n\n%s\n", hidden_text);
    free(cipher_text);
    free(hidden_text);
    return 0;
}
```


{{output}}

```txt

Cipher text ->

BacON's cIPHer Is a METhoD of stEgAnogRaphy crEatEd By FRAncis baCOn.thIs TASk Is TO imPLeMENT a proGrAm FOR eNcRYPTIOn anD deCRyPtioN Of plAINTExt UsING the SIMpLe AlPhaBet Of thE BAConIan CIphER Or sOme OTHer kInD Of reprESenTATion OF This alPHaBET (makE An

Hidden text ->

the quick brown fox jumps over the lazy dog

```



## C++

Bacon cipher implementation


```cpp

#include <iostream>
#include <algorithm>
#include <vector>
#include <bitset>
#include <string>

class bacon {
public:
    bacon() {
        int x = 0;
        for( ; x < 9; x++ )
            bAlphabet.push_back( std::bitset<5>( x ).to_string() );
        bAlphabet.push_back( bAlphabet.back() );

        for( ; x < 20; x++ )
            bAlphabet.push_back( std::bitset<5>( x ).to_string() );
        bAlphabet.push_back( bAlphabet.back() );

        for( ; x < 24; x++ )
            bAlphabet.push_back( std::bitset<5>( x ).to_string() );
    }

    std::string encode( std::string txt ) {
        std::string r;
        size_t z;
        for( std::string::iterator i = txt.begin(); i != txt.end(); i++ ) {
            z = toupper( *i );
            if( z < 'A' || z > 'Z' ) continue;
            r.append( bAlphabet.at( ( *i & 31 ) - 1 ) );
        }
        return r;
    }

    std::string decode( std::string txt ) {
        size_t len = txt.length();
        while( len % 5 != 0 ) len--;
        if( len != txt.length() ) txt = txt.substr( 0, len );
        std::string r;
        for( size_t i = 0; i < len; i += 5 ) {
            r.append( 1, 'A' + std::distance( bAlphabet.begin(), std::find( bAlphabet.begin(), bAlphabet.end(), txt.substr( i, 5 ) ) ) );
        }
        return r;
    }

private:
    std::vector<std::string> bAlphabet;
};

```


These next 2 classes use the 0's & 1's generated by the 'Bacon encryption' to create different the outputs.
One could go wild here...

```cpp

class cipherI {
public:
    std::string encode( std::string txt ) {
        txt = b.encode( txt );
        std::string e, d = "one morning, when gregor samsa woke from troubled dreams, he found himself transformed "
        "in his bed into a horrible vermin. he lay on his armour-like back, and if he lifted his head a little he "
        "could see his brown belly, slightly domed and divided by arches into stiff sections.";
        size_t r = 0;
        char t;
        for( std::string::iterator i = txt.begin(); i != txt.end(); i++ ) {
            t = d.at( r );
            while( t < 'a' || t > 'z' ) {
                e.append( 1, t );
                r++;
                t = d.at( r );
            }
            r++;
            e.append( 1, *i == '1' ? t - 32 : t );
        }

        return e;
    }

    std::string decode( std::string txt ) {
        std::string h;
        for( std::string::iterator i = txt.begin(); i != txt.end(); i++ ) {
            if( *i < 'a' && ( *i < 'A' || *i > 'Z' ) || *i > 'z' ) continue;
            h.append( 1, *i & 32 ? '0' : '1' );
        }
        return b.decode( h );
    }

private:
    bacon b;
};

class cipherII {
public:
    std::string encode( std::string txt ) {
        txt = b.encode( txt );
        std::string e;
        for( std::string::iterator i = txt.begin(); i != txt.end(); i++ )
            e.append( 1, *i == '0' ? 0xf9 : 0xfa );
        return e;
    }

    std::string decode( std::string txt ) {
        std::string h;
        for( std::string::iterator i = txt.begin(); i != txt.end(); i++ ) {
            h.append( 1, *i == ( char )0xf9 ? '0' : '1' );
        }
        return b.decode( h );
    }

private:
    bacon b;
};

int main( int argc, char* argv[] ) {
    cipherI c1;
    cipherII c2;
    std::string s = "lets have some fun with bacon cipher";

    std::string h1 = c1.encode( s ),
                h2 = c2.encode( s );

    std::cout << h1 << std::endl << std::endl << c1.decode( h1 ) << std::endl << std::endl;
    std::cout << h2 << std::endl << std::endl << c2.decode( h2 ) << std::endl << std::endl;

    return 0;
}

```

{{out}}

```txt

oNe MornIng, WheN gRegoR saMSA woke fRom TRouBleD dreAmS, He FoUnD HimSelf tRaNS
foRMeD In hIs Bed iNto a HorRiblE VErmin. He lay on hiS arMOuR-lIKe back, And If
 he lIFTed hIS HeaD a lIttle

LETSHAUESOMEFUNWITHBACONCIPHER

¨·¨·¨¨¨·¨¨·¨¨·¨·¨¨¨·¨¨···¨¨¨¨¨·¨¨··¨¨·¨¨·¨¨¨·¨··¨·¨·¨··¨¨·¨¨¨¨·¨··¨¨··¨··¨¨·¨·¨¨
¨·¨¨¨·¨¨·¨¨¨···¨¨¨¨·¨¨¨¨¨¨¨¨·¨¨··¨·¨··¨¨¨¨¨·¨¨·¨¨¨¨···¨¨¨···¨¨·¨¨·¨¨¨¨

LETSHAUESOMEFUNWITHBACONCIPHER

```


## C#
{{trans|Java}}

```c#
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace BaconCipher {
    class Program {
        private static Dictionary<char, string> codes = new Dictionary<char, string> {
            {'a', "AAAAA" }, {'b', "AAAAB" }, {'c', "AAABA" }, {'d', "AAABB" }, {'e', "AABAA" },
            {'f', "AABAB" }, {'g', "AABBA" }, {'h', "AABBB" }, {'i', "ABAAA" }, {'j', "ABAAB" },
            {'k', "ABABA" }, {'l', "ABABB" }, {'m', "ABBAA" }, {'n', "ABBAB" }, {'o', "ABBBA" },
            {'p', "ABBBB" }, {'q', "BAAAA" }, {'r', "BAAAB" }, {'s', "BAABA" }, {'t', "BAABB" },
            {'u', "BABAA" }, {'v', "BABAB" }, {'w', "BABBA" }, {'x', "BABBB" }, {'y', "BBAAA" },
            {'z', "BBAAB" }, {' ', "BBBAA" }, // use ' ' to denote any non-letter
        };

        private static string Encode(string plainText, string message) {
            string pt = plainText.ToLower();
            StringBuilder sb = new StringBuilder();
            foreach (char c in pt) {
                if ('a' <= c && c <= 'z') sb.Append(codes[c]);
                else sb.Append(codes[' ']);
            }
            string et = sb.ToString();
            string mg = message.ToLower();  // 'A's to be in lower case, 'B's in upper case
            sb.Length = 0;
            int count = 0;
            foreach (char c in mg) {
                if ('a' <= c && c <= 'z') {
                    if (et[count] == 'A') sb.Append(c);
                    else sb.Append((char)(c - 32)); // upper case equivalent
                    count++;
                    if (count == et.Length) break;
                }
                else sb.Append(c);
            }

            return sb.ToString();
        }

        private static string Decode(string message) {
            StringBuilder sb = new StringBuilder();
            foreach (char c in message) {
                if ('a' <= c && c <= 'z') sb.Append('A');
                else if ('A' <= c && c <= 'Z') sb.Append('B');
            }
            string et = sb.ToString();
            sb.Length = 0;
            for (int i = 0; i < et.Length; i += 5) {
                string quintet = et.Substring(i, 5);
                char key = codes.Where(a => a.Value == quintet).First().Key;
                sb.Append(key);
            }
            return sb.ToString();
        }

        static void Main(string[] args) {
            string plainText = "the quick brown fox jumps over the lazy dog";
            string message = "bacon's cipher is a method of steganography created by francis bacon. " +
                "this task is to implement a program for encryption and decryption of " +
                "plaintext using the simple alphabet of the baconian cipher or some " +
                "other kind of representation of this alphabet (make anything signify anything). " +
                "the baconian alphabet may optionally be extended to encode all lower " +
                "case characters individually and/or adding a few punctuation characters " +
                "such as the space.";
            string cipherText = Encode(plainText, message);
            Console.WriteLine("Cipher text ->\n{0}", cipherText);
            string decodedText = Decode(cipherText);
            Console.WriteLine("\nHidden text ->\n{0}", decodedText);
        }
    }
}
```

{{out}}

```txt
Cipher text ->
BacON's cIPHer Is a METhoD of stEgAnogRaphy crEatEd By FRAncis baCOn. thIs TASk Is TO imPLeMENT a proGrAm FOR eNcRYPTIOn anD deCRyPtioN Of plAINTExt UsING the SIMpLe AlPhaBet Of thE BAConIan CIphER Or sOme OTHer kInD Of reprESenTATion OF This alPHaBET (makE An

Hidden text ->
the quick brown fox jumps over the lazy dog
```



## D


```d
import std.array;
import std.stdio;
import std.uni;

immutable string[char] codes;
shared static this() {
    codes = [
        'a': "AAAAA", 'b': "AAAAB", 'c': "AAABA", 'd': "AAABB", 'e': "AABAA",
        'f': "AABAB", 'g': "AABBA", 'h': "AABBB", 'i': "ABAAA", 'j': "ABAAB",
        'k': "ABABA", 'l': "ABABB", 'm': "ABBAA", 'n': "ABBAB", 'o': "ABBBA",
        'p': "ABBBB", 'q': "BAAAA", 'r': "BAAAB", 's': "BAABA", 't': "BAABB",
        'u': "BABAA", 'v': "BABAB", 'w': "BABBA", 'x': "BABBB", 'y': "BBAAA",
        'z': "BBAAB", ' ': "BBBAA", // use ' ' to denote any non-letter
    ];
}

string encode(string plainText, string message) {
    string pt = plainText.toLower;
    auto sb = appender!string;
    foreach (c; pt) {
        if ('a' <= c && c <= 'z') {
            sb ~= codes[c];
        } else {
            sb ~= codes[' '];
        }
    }
    string et = sb.data;
    string mg = message.toLower;
    sb = appender!string;
    int count = 0;
    foreach (c; mg) {
        if ('a' <= c && c <= 'z') {
            if (et[count] == 'A') {
                sb ~= c;
            } else {
                sb ~= cast(char)(c - 32);
            }
            count++;
            if (count == et.length) {
                break;
            }
        } else {
            sb ~= c;
        }
    }

    return sb.data;
}

string decode(string message) {
    auto sb = appender!string;
    foreach (c; message) {
        if ('a' <= c && c <= 'z') {
            sb ~= 'A';
        } else if ('A' <= c && c <= 'Z') {
            sb ~= 'B';
        }
    }
    string et = sb.data;
    sb = appender!string;
    for (int i=0; i<et.length; i+=5) {
        string quintet = et[i .. i+5];
        foreach (k,v; codes) {
            if (v == quintet) {
                sb ~= k;
                break;
            }
        }
    }
    return sb.data;
}

void main() {
    string plainText = "the quick brown fox jumps over the lazy dog";
    string message = "bacon's cipher is a method of steganography created by francis bacon. " ~
                    "this task is to implement a program for encryption and decryption of " ~
                    "plaintext using the simple alphabet of the baconian cipher or some " ~
                    "other kind of representation of this alphabet (make anything signify anything). " ~
                    "the baconian alphabet may optionally be extended to encode all lower " ~
                    "case characters individually and/or adding a few punctuation characters " ~
                    "such as the space.";
    string cipherText = encode(plainText, message);
    writeln("Cipher text ->");
    writeln(cipherText);
    writeln;

    string decodedText = decode(cipherText);
    writeln("Hidden text ->");
    writeln(decodedText);
}
```

{{out}}

```txt
Cipher text ->
BacON's cIPHer Is a METhoD of stEgAnogRaphy crEatEd By FRAncis baCOn. thIs TASk Is TO imPLeMENT a proGrAm FOR eNcRYPTIOn anD deCRyPtioN Of plAINTExt UsING the SIMpLe AlPhaBet Of thE BAConIan CIphER Or sOme OTHer kInD Of reprESenTATion OF This alPHaBET (makE An

Hidden text ->
the quick brown fox jumps over the lazy dog
```


=={{header|Fōrmulæ}}==

In [https://wiki.formulae.org/Bacon_cipher this] page you can see the solution of this task.

Fōrmulæ programs are not textual, visualization/edition of programs is done showing/manipulating structures but not text ([http://wiki.formulae.org/Editing_F%C5%8Drmul%C3%A6_expressions more info]). Moreover, there can be multiple visual representations of the same program. Even though it is possible to have textual representation &mdash;i.e. XML, JSON&mdash; they are intended for transportation effects more than visualization and edition.

The option to show Fōrmulæ programs and their results is showing images. Unfortunately images cannot be uploaded in Rosetta Code.


## Go

{{trans|Kotlin}}

```go
package main

import(
    "fmt"
    "strings"
)

var codes = map[rune]string {
    'a' : "AAAAA", 'b' : "AAAAB", 'c' : "AAABA", 'd' : "AAABB", 'e' : "AABAA",
    'f' : "AABAB", 'g' : "AABBA", 'h' : "AABBB", 'i' : "ABAAA", 'j' : "ABAAB",
    'k' : "ABABA", 'l' : "ABABB", 'm' : "ABBAA", 'n' : "ABBAB", 'o' : "ABBBA",
    'p' : "ABBBB", 'q' : "BAAAA", 'r' : "BAAAB", 's' : "BAABA", 't' : "BAABB",
    'u' : "BABAA", 'v' : "BABAB", 'w' : "BABBA", 'x' : "BABBB", 'y' : "BBAAA",
    'z' : "BBAAB", ' ' : "BBBAA",  // use ' ' to denote any non-letter
}

func baconEncode(plainText string, message string) string {
    pt := strings.ToLower(plainText)
    var sb []byte
    for _, c := range pt {
        if c >= 'a' && c <= 'z' {
            sb = append(sb, codes[c]...)
        } else {
            sb = append(sb, codes[' ']...)
        }
    }
    et := string(sb)
    mg := strings.ToLower(message)  // 'A's to be in lower case, 'B's in upper case
    sb = nil  // clear the byte slice
    var count = 0
    for _, c := range mg {
        if c >= 'a' && c <= 'z' {
            if et[count] == 'A' {
                sb = append(sb, byte(c))
            } else {
                sb = append(sb, byte(c - 32))  // upper case equivalent
            }
            count++
            if count == len(et) { break }
        } else {
            sb = append(sb, byte(c))
        }
    }
    return string(sb)
}

func baconDecode(message string) string {
    var sb []byte
    for _, c := range message {
        if c >= 'a' && c <= 'z' {
            sb = append(sb, 'A')
        } else if c >= 'A' && c <= 'Z' {
            sb = append(sb, 'B')
        }
    }
    et := string(sb)
    sb = nil  // clear the byte slice
    for i := 0; i < len(et); i += 5 {
        quintet := et[i : i + 5]
        for k, v := range codes {
            if v == quintet {
                sb = append(sb, byte(k))
                break
            }
        }
    }
    return string(sb)
}

func main() {
    plainText := "the quick brown fox jumps over the lazy dog"
    message := "bacon's cipher is a method of steganography created by francis bacon." +
        "this task is to implement a program for encryption and decryption of " +
        "plaintext using the simple alphabet of the baconian cipher or some " +
        "other kind of representation of this alphabet (make anything signify anything). " +
        "the baconian alphabet may optionally be extended to encode all lower " +
        "case characters individually and/or adding a few punctuation characters " +
        "such as the space."
    cipherText := baconEncode(plainText, message)
    fmt.Printf("Cipher text ->\n\n%s\n", cipherText)
    decodedText := baconDecode(cipherText)
    fmt.Printf("\nHidden text ->\n\n%s\n", decodedText)
}
```


{{out}}

```txt

Cipher text ->

BacON's cIPHer Is a METhoD of stEgAnogRaphy crEatEd By FRAncis baCOn.thIs TASk Is TO imPLeMENT a proGrAm FOR eNcRYPTIOn anD deCRyPtioN Of plAINTExt UsING the SIMpLe AlPhaBet Of thE BAConIan CIphER Or sOme OTHer kInD Of reprESenTATion OF This alPHaBET (makE An

Hidden text ->

the quick brown fox jumps over the lazy dog

```



## Haskell


Necessary imports

```Haskell
import Data.List (findIndex, unfoldr)
import Data.Char (isAlpha, isUpper, toUpper, toLower)
import Data.Bool (bool)
```


The list of characters to be encoded:

```Haskell
chars = ['a'..'z'] ++ ['0'..'9'] ++ ",.;?! "
bitsPerChar = 6 :: Int
```


Some simple helper functions:

```Haskell
toBinary :: Int -> [Bool]
toBinary = unfoldr (pure . (\(a,b)->(odd b,a)) . (`divMod` 2))

fromBinary :: [Bool] -> Int
fromBinary = foldr (\x n -> 2*n + bool 0 1 x) 0
```


And, finally main functions -- encoding:

```Haskell
encode :: String -> String -> Either String String
encode message txt = do
  mask <- traverse coding message
  zipAlphas (bool toLower toUpper) (concat mask) txt
  where
    coding ch = case findIndex (== ch) chars of
      Nothing -> Left $ "Unknown symbol " ++ show ch
      Just i -> Right $ take bitsPerChar (toBinary i)

    zipAlphas f = go
      where go _ [] = Left "Text is not long enough!"
            go [] _ = Right []
            go (x:xs) (y:ys) | isAlpha y = (f x y :) <$> go xs ys
                             | otherwise = (y :) <$> go (x:xs) ys
```


And decoding:

```Haskell
decode :: String -> String
decode = map decipher . chunksOf bitsPerChar . filter isAlpha
  where
    decipher = (chars !!) . min (length chars-1) . fromBinary . map isUpper
    chunksOf n = takeWhile (not . null) . unfoldr (pure . splitAt n)
```


'''Examples'''


```Haskell
text = concat ["Bacon's cipher is a method of steganography created by Francis Bacon. "
               ,"This task is to implement a program for encryption and decryption of "
               ,"plaintext using the simple alphabet of the Baconian cipher or some "
               ,"other kind of representation of this alphabet (make anything signify "
               ,"anything). The Baconian alphabet may optionally be extended to encode "
               ,"all lower case characters individually and/or adding a few punctuation "
               ,"characters such as the space." ]

message = "the quick brown fox jumps over the lazy dog"
```



```Haskell
λ> let m = encode message text
λ> m
Right "BAcoN's CIPher is A metHod Of StegaNogrApHy creAted By franCiS baCon. ThIS task iS to iMplEMEnt a PRoGrAm FOr eNcrYpTIoN and dECRypTIOn Of PlaInTExt Using ThE simPLe aLPHAbet Of tHe BacOnIaN CIphEr Or Some OtheR kinD oF rePrESEntAtION of thiS alpHabEt (MAKe Anything sIgnIFy anyTHiNg). tHe BAConian ALPhabET may"
λ> decode <$> m
Right "the quick brown fox jumps over the lazy dog"
λ> encode "something wrong @  in the message" text
Left "Unknown symbol '@'"
λ> encode message "abc"
Left "Text is not long enough!"
```





## J


Implementation:


```J
alfa=: 'ABCDEFGHIKLMNOPQRSTUWXYZ'
beta=: 26{.(}.~i.&'A')a.
norm=: ([ -. -.)&alfa@(rplc&('JIVU'))@toupper
enca=:(5#2),@:#:alfa i. norm
gena=: ]`((,:tolower)@(beta {~ 26 ?@#~ #))}

encrypt=: gena@enca@norm
decrypt=: alfa {~ _5 #.\ 90 < a.&i.
```


We use random letters as the basis for our steganography and we use case to represent "font".

Example use:


```J
   encrypt 'this is a test'
nWVkJAPkamEuUJIeTGKnUsTVRfAWWuNBIIHdEIcOAPuTBeXKQduQAdU
   encrypt 'this is a test'
sFLkBQKqqaQsGGXzAXQsKlZFBcILRlUIRAQaEQoNUBcHIhFTWbeRAlM
   decrypt encrypt 'this is a test'
THISISATEST
```



## Java

{{trans|Kotlin}}
{{works with|Java|9}}

```Java
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

public class BaconCipher {
    private static final Map<Character, String> codes;

    static {
        codes = new HashMap<>();
        codes.putAll(Map.of(
            'a', "AAAAA", 'b', "AAAAB", 'c', "AAABA", 'd', "AAABB", 'e', "AABAA",
            'f', "AABAB", 'g', "AABBA", 'h', "AABBB", 'i', "ABAAA", 'j', "ABAAB"
        ));
        codes.putAll(Map.of(
            'k', "ABABA", 'l', "ABABB", 'm', "ABBAA", 'n', "ABBAB", 'o', "ABBBA",
            'p', "ABBBB", 'q', "BAAAA", 'r', "BAAAB", 's', "BAABA", 't', "BAABB"
        ));
        codes.putAll(Map.of(
            'u', "BABAA", 'v', "BABAB", 'w', "BABBA", 'x', "BABBB", 'y', "BBAAA",
            'z', "BBAAB", ' ', "BBBAA" // use ' ' to denote any non-letter
        ));
    }

    private static String encode(String plainText, String message) {
        String pt = plainText.toLowerCase();
        StringBuilder sb = new StringBuilder();
        for (char c : pt.toCharArray()) {
            if ('a' <= c && c <= 'z') sb.append(codes.get(c));
            else sb.append(codes.get(' '));
        }
        String et = sb.toString();
        String mg = message.toLowerCase();  // 'A's to be in lower case, 'B's in upper case
        sb.setLength(0);
        int count = 0;
        for (char c : mg.toCharArray()) {
            if ('a' <= c && c <= 'z') {
                if (et.charAt(count) == 'A') sb.append(c);
                else sb.append(((char) (c - 32))); // upper case equivalent
                count++;
                if (count == et.length()) break;
            } else sb.append(c);
        }
        return sb.toString();
    }

    private static String decode(String message) {
        StringBuilder sb = new StringBuilder();
        for (char c : message.toCharArray()) {
            if ('a' <= c && c <= 'z') sb.append('A');
            if ('A' <= c && c <= 'Z') sb.append('B');
        }
        String et = sb.toString();
        sb.setLength(0);
        for (int i = 0; i < et.length(); i += 5) {
            String quintet = et.substring(i, i + 5);
            Character key = codes.entrySet().stream().filter(a -> Objects.equals(a.getValue(), quintet)).findFirst().map(Map.Entry::getKey).orElse(null);
            sb.append(key);
        }
        return sb.toString();
    }

    public static void main(String[] args) {
        String plainText = "the quick brown fox jumps over the lazy dog";
        String message = "bacon's cipher is a method of steganography created by francis bacon. " +
            "this task is to implement a program for encryption and decryption of " +
            "plaintext using the simple alphabet of the baconian cipher or some " +
            "other kind of representation of this alphabet (make anything signify anything). " +
            "the baconian alphabet may optionally be extended to encode all lower " +
            "case characters individually and/or adding a few punctuation characters " +
            "such as the space.";
        String cipherText = encode(plainText, message);
        System.out.printf("Cipher text ->\n\n%s\n", cipherText);
        String decodedText = decode(cipherText);
        System.out.printf("\nHidden text ->\n\n%s\n", decodedText);
    }
}
```

{{out}}

```txt
Cipher text ->

BacON's cIPHer Is a METhoD of stEgAnogRaphy crEatEd By FRAncis baCOn. thIs TASk Is TO imPLeMENT a proGrAm FOR eNcRYPTIOn anD deCRyPtioN Of plAINTExt UsING the SIMpLe AlPhaBet Of thE BAConIan CIphER Or sOme OTHer kInD Of reprESenTATion OF This alPHaBET (makE An

Hidden text ->

the quick brown fox jumps over the lazy dog

```



## Julia

{{works with|Julia|0.6}}
{{trans|Python}}

'''Module''':

```julia
module BaconCipher

using Formatting, IterTools.chain

const text = """All children, except one, grow up. They soon know that they will grow
    up, and the way Wendy knew was this. One day when she was two years old
    she was playing in a garden, and she plucked another flower and ran with
    it to her mother. I suppose she must have looked rather delightful, for
    Mrs. Darling put her hand to her heart and cried, "Oh, why can't you
    remain like this for ever!" This was all that passed between them on
    the subject, but henceforth Wendy knew that she must grow up. You always
    know after you are two. Two is the beginning of the end.

    Of course they lived at 14 [their house number on their street], and
    until Wendy came her mother was the chief one. She was a lovely lady,
    with a romantic mind and such a sweet mocking mouth. Her romantic
    mind was like the tiny boxes, one within the other, that come from the
    puzzling East, however many you discover there is always one more; and
    her sweet mocking mouth had one kiss on it that Wendy could never get,
    though there it was, perfectly conspicuous in the right-hand corner.""" |> lowercase

const byte = FormatSpec("05b")
const lc2bin = Dict{Char,String}(ch => fmt(byte, i) for (i, ch) in
    enumerate(chain('a':'z', '.', ' ')))
const bin2lc = Dict{String,Char}(v => k for (k, v) in lc2bin)

to5binary(msg::AbstractString) = collect(ch == '1' for lt in lowercase(msg) for ch in get(lc2bin, lt, ""))

function encrypt(msg::AbstractString, text::AbstractString=text)::String
    bin5 = to5binary(msg)
    tlist = collect(Char, lowercase(text))
    out = IOBuffer()
    for capitalise in bin5
        while !isempty(tlist)
            ch = shift!(tlist)
            if isalpha(ch)
                if capitalise
                    ch = uppercase(ch)
                end
                print(out, ch)
                break
            else
                print(out, ch)
            end
        end
    end
    println(out, "...")
    return take!(out)
end

function decrypt(text::AbstractString)::String
    binary = Char[]
    out    = IOBuffer()
    for ch in text
        if isalpha(ch)
            push!(binary, ifelse(isupper(ch), '1', '0'))
            if length(binary) == 5
                print(out, bin2lc[join(binary)])
                empty!(binary)
            end
        end
    end
    return take!(out)
end

end  # module BaconCipher
```


'''Main''':

```julia
let msg = "Rosetta code Bacon cipher example secret phrase to encode in the capitalisation of peter pan"
    enc = BaconCipher.encrypt(msg)
    dec = BaconCipher.decrypt(enc)
    println("\nOriginal:\n", msg)
    println(" -> Encrypted:\n", enc)
    println(" -> Decrypted:\n", dec)
end
```


{{out}}

```txt
Original:
Rosetta code Bacon cipher example secret phrase to encode in the capitalisation of peter pan
 -> Encrypted:
All ChiLDREN, exCEpt OnE, GrOw uP. tHey soon KNOW that tHEy WILL grOw
up, aNd THE Way wenDy knew Was tHIs. ONE DaY WHeN SHe was tWO yEarS Old
she Was plaYiNG in A gARDen, anD sHE Plucked aNoTHeR Flower ANd ran WiTH
IT to Her MOthEr. I supPOSe sHe muSt HAvE loOKEd rAther dEligHtfUl, for
mRS. daRLinG pUT HEr hAnD to hER HEART and cRiEd, "OH, Why caN'T yOU
REmaIn likE tHIS For eVer!" ThIS WaS ALl tHaT pasSed betWeEN THem on
tHE subjECt, but hEncEFoRth wendY kNEw thAt sHE muST grow UP. yOu alWayS
kNOW AfTER yOU Are tWO. TWo iS ThE BEgiNning of ThE EnD.

of coUrSE thEy LIVed At 14 [their hoUsE NUm...

 -> Decrypted:
rosetta code bacon cipher example secret phrase to encode in the capitalisation of peter pan
```



## Kotlin

The 'full' Bacon alphabet, which has separate letters for i, j, u and v, has been used in the following:

```scala
object Bacon {
    private val codes = mapOf(
        'a' to "AAAAA", 'b' to "AAAAB", 'c' to "AAABA", 'd' to "AAABB", 'e' to "AABAA",
        'f' to "AABAB", 'g' to "AABBA", 'h' to "AABBB", 'i' to "ABAAA", 'j' to "ABAAB",
        'k' to "ABABA", 'l' to "ABABB", 'm' to "ABBAA", 'n' to "ABBAB", 'o' to "ABBBA",
        'p' to "ABBBB", 'q' to "BAAAA", 'r' to "BAAAB", 's' to "BAABA", 't' to "BAABB",
        'u' to "BABAA", 'v' to "BABAB", 'w' to "BABBA", 'x' to "BABBB", 'y' to "BBAAA",
        'z' to "BBAAB", ' ' to "BBBAA" // use ' ' to denote any non-letter
    )

    fun encode(plainText: String, message: String): String {
        val pt = plainText.toLowerCase()
        val sb = StringBuilder()
        for (c in pt)
            if (c in 'a'..'z') sb.append(codes[c])
            else sb.append(codes[' '])
        val et = sb.toString()
        val mg = message.toLowerCase()  // 'A's to be in lower case, 'B's in upper case
        sb.setLength(0)
        var count = 0
        for (c in mg)
            if (c in 'a'..'z') {
                if (et[count] == 'A') sb.append(c)
                else sb.append(c - 32) // upper case equivalent
                count++
                if (count == et.length) break
            } else sb.append(c)
        return sb.toString()
    }

    fun decode(message: String): String {
        val sb = StringBuilder()
        for (c in message)
            when (c) {
                in 'a'..'z' -> sb.append('A')
                in 'A'..'Z' -> sb.append('B')
            }
        val et = sb.toString()
        sb.setLength(0)
        for (i in 0 until et.length step 5) {
            val quintet = et.substring(i, i + 5)
            val key = codes.entries.find { it.value == quintet }!!.key
            sb.append(key)
        }
        return sb.toString()
    }
}

fun main(args: Array<String>) {
    val plainText = "the quick brown fox jumps over the lazy dog"
    val message = "bacon's cipher is a method of steganography created by francis bacon." +
        "this task is to implement a program for encryption and decryption of " +
        "plaintext using the simple alphabet of the baconian cipher or some " +
        "other kind of representation of this alphabet (make anything signify anything). " +
        "the baconian alphabet may optionally be extended to encode all lower " +
        "case characters individually and/or adding a few punctuation characters " +
        "such as the space."
    val cipherText = Bacon.encode(plainText, message)
    println("Cipher text ->\n\n$cipherText")
    val decodedText = Bacon.decode(cipherText)
    println("\nHidden text ->\n\n$decodedText")
}
```


{{out}}

```txt

Cipher text ->

BacON's cIPHer Is a METhoD of stEgAnogRaphy crEatEd By FRAncis baCOn.thIs TASk Is TO imPLeMENT a proGrAm FOR eNcRYPTIOn anD deCRyPtioN Of plAINTExt UsING the SIMpLe AlPhaBet Of thE BAConIan CIphER Or sOme OTHer kInD Of reprESenTATion OF This alPHaBET (makE An

Hidden text ->

the quick brown fox jumps over the lazy dog

```



## Lua

Based on C++ version

```Lua

function Bacon( txt, secret, e )
    local alpha = {}
    function encode( txt, secret )
        function toAlpha( secret )
            local str, z = "", 0
            secret = secret:upper()
            for i = 1, string.len( secret ) do
                z = secret:sub( i, i )
                if z < 'A' or z > 'Z'  then
                    str = str .. alpha[27]
                else
                    k = z:byte( 1 ) - 65 + 1
                    str = str .. alpha[k]
                end
            end
            return str
        end

        local sec, encoded, idx = toAlpha( secret ), "", 0
        if sec:len() > txt:len() then
            print( "Text is too short!" )
           return
        end

        txt = txt:lower()
        for i = 1, string.len( sec ) do
            t = txt:sub( idx, idx )
            while( t < 'a' or t > 'z' ) do
                encoded = encoded .. t
                idx = idx + 1
                t = txt:sub( idx, idx )
            end

            idx = idx + 1
            if sec:sub( i, i ) == '1' then
                encoded = encoded .. string.char( t:byte(1) - 32 )
            else
                encoded = encoded .. t
            end
        end
        return encoded
    end

    function decode( txt )
        local secret, c = "", 0
        for i = 1, string.len( txt ) do
            c = txt:sub( i, i )
            if not( c < 'a' and ( c < 'A' or c > 'Z' ) or c > 'z' ) then
                local s = 0
                if c == c:upper() then s = 1 end
                secret = secret .. s
            end
        end

        function fromAlpha( secret )
            function find( a, arr )
                for i = 1, #arr do
                    if arr[i] == a then return i end
                end
                return -1
            end

            local l, msg, c, idx = secret:len(), "", 0, 0
            if math.fmod( l, 5 ) ~= 0 then
                print( "Message length does not match!" )
                return
            end
            for i = 1, l, 5 do
                c = secret:sub( i, i + 4 )
                idx = find( c, alpha )
                if idx > 0 then
                    if idx == 27 then
                        msg = msg .. " "  -- unknown char - add space
                    else
                        msg = msg .. string.char( 64 + idx )
                    end
                end
            end
            return msg
        end
        return fromAlpha( secret )
    end

    -- create alphabet
    for i = 0, 26 do
        local t, num = "", i
        for b = 5, 1, -1 do
            t =  math.fmod( num, 2 ) .. t
            num = math.floor( num / 2 )
        end
        alpha[#alpha + 1] = t
    end

    -- encode or decode
    if e == 1 then
        return encode( txt, secret )
    elseif e == 0 then
        return decode( secret )
    end
end

local a = Bacon( "Chase the pig around the house present belly, scratch hand when stroked. "..
                 "When in doubt, wash jump around on couch, meow constantly until given food, "..
                 "favor packaging over toy. And sometimes switches in french and say 'miaou' "..
                 "just because well why not has closed eyes but still sees you lick yarn hanging "..
                 "out of own butt so pelt around the house and up and down stairs chasing phantoms.",
                 "Fall over dead, not really but gets sypathy", 1 )
print( a )
print( Bacon( "", a, 0 ) )

```

{{out}}

```txt

>lua -e "io.stdout:setvbuf 'no'" "bacon.lua"
chAsE the pig ArOUnD tHE HOuSe pRESeNt BeLly, ScrAtch HANd When sTRokEd. when in douBT, WAsH jUMp AroUNd On COUcH, meOW COnStAntlY unTil given fOoD, FaVoR PACkagINg Over toY. AnD soMetIMES sWitcHEs in FreNch AND saY 'mIAoU' jUst BeCAuse wELL Why not Has CLosED EYEs bu
FALL OVER DEAD  NOT REALLY BUT GETS SYPATHY

```



## MiniScript


```MiniScript
c = {}
c["a"] = "AAAAA"; c["b"] = "AAAAB"; c["c"] = "AAABA"; c["d"] = "AAABB"; c["e"] = "AABAA"; c["f"] = "AABAB";
c["g"] = "AABBA"; c["h"] = "AABBB"; c["i"] = "ABAAA"; c["j"] = "ABAAB"; c["k"] = "ABABA"; c["l"] = "ABABB";
c["m"] = "ABBAA"; c["n"] = "ABBAB"; c["o"] = "ABBBA"; c["p"] = "ABBBB"; c["q"] = "BAAAA"; c["r"] = "BAAAB";
c["s"] = "BAABA"; c["t"] = "BAABB"; c["u"] = "BABAA"; c["v"] = "BABAB"; c["w"] = "BABBA"; c["x"] = "BABBB";
c["y"] = "BBAAA"; c["z"] = "BBAAB"; c[" "] = "BBBAA";
codeMap = c  // (used "c" above just to make the lines shorter)
decodeMap = {}
for kv in codeMap
	decodeMap[kv.value] = kv.key
end for

message = "Computers are everywhere.  There are the obvious ones, like your smart phone or game system. "
message = message + "There are less obvious ones, like in your microwave or your bedside clock.  And then "
message = message + "there are the really invisible ones, like the 50 or so small computers in your car."
message = message + "All these computers work in pretty much the same way."
message = message.values  // (convert to list)

encodeChar = function(ch)
	out = []
	for bit in codeMap[ch]
		while message[0].upper == message[0].lower
			out.push message.pull   // (not a letter we can use; pass through as-is)
		end while
		if bit == "A" then out.push message.pull.upper else out.push message.pull.lower
	end for
	return out
end function

encode = function(s)
	out = []
	for ch in s
		out = out + encodeChar(ch)
	end for
	return out.join("")
end function

decodeChar = function(encodedChar)
	code = ""
	for ch in encodedChar
		if ch == ch.upper then code = code + "A" else code = code + "B"
	end for
	if decodeMap.hasIndex(code) then return decodeMap[code]
	return "?"
end function

decode = function(s)
	// strip out all non-letters
	schars = s.values
	for i in range(schars.len-1, 0)
		if schars[i].lower == schars[i].upper then schars.remove i
	end for
	s = schars.join("")
	// now, decode using groups of 5 characters
	out = []
	for i in range(0, s.len-1, 5)
		out.push decodeChar(s[i:i+5])
	end for
	return out.join("")
end function

codedMsg = encode("the quick brown fox jumps over the lazy dog")
print codedMsg
print decode(codedMsg)
```


{{out}}

```txt
cOMpuTErs aRE eVErywHErE.  THErE aRE ThE OBVIOuS OnEs, Like YOUR SMarT PHoNe or GaMe sYSteM. therE ARE lEsS obvIoUs ones, lIKE iN YouR mICRowAVE or youR BeDsidE CLock.  AnD tHeN ThERe ARE the rEAlLY inVIsibLE oNEs, liKE ThE 50 or SO SMAll COmpuTERs in YOUR Car.All tHESe cO
the quick brown fox jumps over the lazy dog
```




## Perl

{{trans|Perl 6}}

```perl
use strict;
use warnings;
use utf8;
binmode(STDOUT, ':utf8');

my $secret = <<'END';
This task is to implement a program for encryption and decryption
of plaintext using the simple alphabet of the Baconian cipher or
some other kind of representation of this alphabet (make anything
signify anything). This example will work with anything in the
ASCII range... even code! $r%_-^&*(){}+~ #=`/\';*1234567890"'
END

my $text = <<'END';
Bah. It isn't really practical to use typeface changes to encode
information, it is too easy to tell that there is something going
on and will attract attention. Font changes with enough regularity
to encode mesages relatively efficiently would need to happen so
often it would be obvious that there was some kind of manipulation
going on. Steganographic encryption where it is obvious that there
has been some tampering with the carrier is not going to be very
effective. Not that any of these implementations would hold up to
serious scrutiny anyway. Anyway, here's a semi-bogus implementation
that hides information in white space. The message is hidden in this
paragraph of text. Yes, really. It requires a fairly modern file
viewer to display (not display?) the hidden message, but that isn't
too unlikely any more. It may be stretching things to call this a
Bacon cipher, but I think it falls within the spirit of the task,
if not the exact definition.
END

my @enc = ("\N{U+FEFF}", "\N{U+200B}"); # zero-width spaces
my %dec;
$dec{$enc[0]} = 0;
$dec{$enc[1]} = 1;

sub encode { my($c) = @_; join '', @enc[split '', sprintf "%07b", ord($c)] }
sub hide {
    my($text, @msg) = @_;
    my $head = substr($text, 0, @msg);
    my $tail = substr($text, @msg);
    my @head = split '', $head;
    my $merge;
    while (@msg) { $merge .= shift(@head) . shift(@msg) }
    $merge . $tail;
}

sub reveal {
    my($steganography) = @_;
    my $message;
    (my $cleaned = $steganography) =~ s/\w|[,?:.!\-&*()*"']| |\n//g;
    for my $coded_char (split /(.{7})/, $cleaned) {
        next if length $coded_char == 0;
        my $bits = '';
        $bits .= $dec{$_} for split //, $coded_char;
        $message .= chr eval('0b'.$bits);
    }
    $message;
}
my @hidden = map  { encode($_) } split '', $secret;
my $steganography = hide($text, @hidden);
my $decoded       = reveal $steganography;

print "$steganography\n"
print "$decoded\n"
```

{{out}}
<pre style="height:35ex">Steganograpic message hidden in text:
B​﻿​﻿​﻿﻿​a​﻿​﻿﻿﻿​​h﻿​﻿﻿​​​​.﻿﻿​​﻿​﻿﻿ ﻿﻿﻿​​​﻿​I﻿﻿​​﻿﻿﻿﻿t​​​​﻿﻿​​ ​​﻿​﻿​​﻿i​﻿﻿﻿﻿﻿​​s﻿​﻿﻿​​​​n﻿﻿​​﻿​﻿﻿'﻿﻿﻿​​​﻿​t﻿﻿​​﻿​​​ ​﻿​﻿﻿﻿﻿﻿r​​﻿​﻿﻿​​e​﻿​​﻿​​​a​﻿﻿﻿﻿​​﻿l​​﻿﻿​​﻿﻿l​﻿​​​﻿​​y﻿​​​﻿﻿​﻿ ​​​﻿​​​﻿p​​​﻿​﻿﻿﻿r​﻿﻿﻿﻿﻿​​a﻿﻿﻿﻿​﻿​﻿c﻿﻿﻿﻿​​​﻿t﻿﻿﻿​​​﻿﻿i​﻿​​﻿​​​c​​​﻿﻿​​​a​​​﻿﻿​﻿​l​﻿﻿﻿﻿​​​ ﻿​​﻿​﻿​﻿t﻿﻿﻿﻿​​﻿﻿o​​﻿​​﻿​​ ​​​​​﻿﻿​u﻿﻿​﻿﻿﻿﻿﻿s​​﻿﻿​﻿​​e​﻿​​​﻿​​ ﻿﻿﻿​​​​​t﻿﻿​﻿​​​​y﻿﻿​​​​﻿﻿p﻿﻿​​​﻿​﻿e﻿​​﻿​﻿﻿​f​​﻿​​​​​a​﻿​​​﻿﻿​c﻿﻿﻿﻿﻿​​﻿e﻿﻿﻿​​​﻿​ ​​﻿​​﻿﻿​c﻿﻿﻿​﻿﻿﻿﻿h﻿​​﻿﻿​﻿﻿a​​﻿﻿​﻿​​n​﻿﻿﻿​​​​g​﻿﻿​﻿​​​e​﻿﻿​​​​﻿s﻿﻿﻿​​​﻿​ ﻿﻿​​﻿​﻿﻿t​​​﻿​​​​o​​﻿​​​﻿﻿ ﻿﻿​﻿​﻿​​e﻿​​​​​​﻿n﻿​​﻿﻿​﻿﻿c﻿﻿﻿​​​﻿﻿o﻿﻿​​﻿​​﻿d﻿​​﻿﻿﻿﻿​e​​﻿​﻿﻿​​
i​﻿​﻿﻿​​﻿n﻿​﻿​​​​​f﻿﻿﻿​​​﻿​o﻿﻿﻿​﻿﻿﻿﻿r﻿​​​﻿​﻿​m​​​﻿﻿​​​a​﻿​﻿﻿​​​t﻿​​​﻿​​﻿i﻿​​​﻿​﻿﻿o﻿﻿﻿​​​﻿​n﻿﻿​​﻿​﻿﻿,﻿​​﻿﻿​﻿​ ﻿​﻿﻿﻿﻿﻿​i​​﻿﻿​​​​t﻿​﻿﻿​​​﻿ ​​﻿​​​​﻿i﻿﻿﻿​​﻿​​s﻿﻿​​﻿﻿​﻿ ​﻿​﻿﻿﻿﻿﻿t​​﻿﻿﻿﻿​​o​﻿​​﻿﻿​​o​﻿﻿﻿﻿​​﻿ ​﻿﻿﻿​​﻿﻿e﻿﻿​​​﻿﻿﻿a​﻿​​﻿﻿​﻿s​​​​﻿​﻿﻿y﻿​﻿﻿﻿﻿﻿​ ​﻿​​​​​​t﻿﻿​​﻿﻿​﻿o﻿﻿﻿﻿​​​﻿ ​﻿﻿​​﻿​﻿t﻿﻿​​﻿﻿​﻿e​﻿​﻿﻿﻿﻿﻿l​﻿﻿﻿﻿​﻿​l​﻿﻿﻿﻿​​​ ﻿﻿﻿​​​​﻿t​​​​​​﻿​h​​﻿​​﻿​﻿a﻿​​​﻿﻿﻿﻿t​​​﻿​​​﻿ ﻿​﻿﻿﻿﻿﻿​t​﻿﻿﻿​​​​h﻿​﻿﻿​​​​e﻿﻿﻿﻿​​﻿​r﻿﻿﻿​​﻿﻿​e﻿​​​​﻿﻿​ ﻿﻿​﻿﻿﻿﻿﻿i​​﻿​​​​​s​​﻿﻿​﻿﻿﻿ ﻿​﻿​﻿​​​s﻿﻿​​​​﻿​o​​​​​﻿​​m﻿​​​﻿﻿​﻿e​﻿​﻿﻿﻿﻿﻿t​​﻿​​​​​h​​﻿​﻿﻿​​i﻿​﻿﻿﻿​​﻿n﻿​﻿​​​​﻿g﻿​﻿﻿​﻿﻿﻿ ﻿﻿​​﻿​﻿​g​​​﻿​﻿﻿​o​​﻿​​​﻿​i​﻿﻿​﻿﻿﻿​n﻿﻿﻿﻿﻿​​﻿g​​​​​​﻿﻿
o﻿﻿​​​﻿﻿​n﻿​​﻿﻿​﻿​ ​​​﻿﻿﻿﻿​a​​﻿﻿​﻿​​n﻿﻿​﻿​​​​d﻿﻿​​​​﻿﻿ ​﻿​​​﻿​​w​﻿​​​﻿​﻿i﻿​​﻿﻿﻿﻿​l​​​﻿​﻿﻿​l​﻿​﻿﻿​​​ ﻿​​​​​​﻿a​​​﻿﻿​﻿﻿t﻿﻿﻿​​﻿​​t​​​​﻿﻿​​r﻿﻿​﻿﻿﻿﻿﻿a​​​﻿​﻿﻿​c​﻿​﻿﻿﻿​​t﻿​﻿﻿​​​​ ﻿﻿​​﻿​﻿﻿a﻿﻿﻿​​﻿﻿﻿t﻿​​​﻿​​﻿t﻿​​​﻿﻿﻿﻿e​​﻿​﻿﻿﻿​n​﻿﻿﻿﻿​​​t﻿﻿﻿​﻿​​﻿i﻿​﻿​​​​﻿o​﻿﻿﻿​﻿﻿﻿n﻿﻿﻿​﻿​﻿﻿.﻿​​﻿​​﻿​ ​​﻿﻿﻿﻿​​F​﻿​﻿​​​​o﻿﻿​﻿​﻿​﻿n﻿﻿﻿﻿​​﻿﻿t﻿﻿​​​﻿​​ ​﻿​​​​﻿﻿c​​​​﻿​﻿﻿h​​﻿​﻿﻿﻿​a​﻿​﻿﻿​​​n﻿​​​﻿​​﻿g﻿​​​﻿﻿﻿​e﻿​﻿​​​﻿﻿s​​​​﻿​﻿﻿ ​​​﻿﻿​​​w​​﻿​​​﻿​i​﻿​﻿﻿​​​t﻿﻿​​﻿​​​h​﻿﻿​﻿​﻿﻿ ﻿﻿﻿​​﻿﻿﻿e﻿​​​﻿​​​n﻿​​​​﻿﻿​o​​​﻿​﻿﻿​u​﻿​﻿﻿﻿​​g﻿​﻿﻿​​​﻿h​​​﻿​​﻿﻿ ​​​﻿​﻿​﻿r﻿​﻿​﻿​​​e﻿﻿​﻿﻿﻿﻿﻿g​﻿​﻿​﻿﻿​u​﻿​﻿﻿﻿​​l﻿​﻿﻿​​​​a﻿﻿​​﻿​﻿﻿r﻿﻿﻿​​﻿﻿​i﻿​​​​​﻿﻿t﻿​​﻿﻿﻿﻿​y​​﻿​​﻿​​
t﻿​​﻿﻿​​﻿o﻿​﻿​﻿​﻿﻿ ﻿﻿﻿​​​﻿​e​​​​﻿​﻿﻿n​​​﻿​​﻿﻿c​​﻿​​﻿﻿﻿o​﻿﻿﻿﻿﻿​​d​﻿​​​​​﻿e​​​​​​​﻿ ﻿​﻿​​﻿​﻿m​​﻿​﻿﻿﻿﻿e﻿​​​﻿​​​s​​﻿​﻿﻿​​a​​﻿​﻿﻿​​g﻿​﻿﻿﻿﻿​﻿e﻿﻿﻿﻿​​﻿﻿s﻿﻿​​​﻿​​ ​﻿​​​​﻿﻿r​​​​﻿​﻿﻿e​​﻿​﻿﻿﻿​l​﻿​﻿﻿​​​a﻿​​​﻿​​﻿t﻿​​​﻿​﻿﻿i﻿﻿﻿​​﻿​﻿v﻿​​​﻿​​​e﻿﻿​﻿﻿﻿﻿﻿l​​​﻿​﻿﻿​y​﻿​﻿﻿﻿​​ ﻿﻿​﻿​﻿﻿﻿e​﻿​﻿​﻿﻿﻿f﻿﻿​​﻿​﻿﻿f​​​﻿﻿﻿﻿​i​​﻿﻿​﻿﻿​c​﻿﻿​﻿﻿​﻿i​﻿﻿﻿﻿﻿​​e​﻿﻿​﻿​​﻿n﻿﻿﻿​​​﻿​t​​﻿​​﻿﻿​l​​​​﻿﻿​﻿y​﻿​﻿​​​﻿ ﻿​﻿​​​﻿﻿w​﻿​​​﻿﻿​o﻿﻿﻿﻿﻿​​﻿u﻿​﻿​​​​﻿l​​﻿​​﻿﻿​d﻿​​​﻿​​​ ﻿﻿​﻿﻿﻿﻿﻿n​​﻿﻿﻿​​​e​﻿​​​​​​e﻿﻿​﻿﻿​​﻿d﻿​﻿​﻿​﻿﻿ ﻿﻿​﻿​﻿﻿﻿t﻿﻿﻿​﻿﻿​﻿o﻿​​​﻿﻿​﻿ ﻿​﻿﻿​﻿​​h﻿​​​​​﻿​a﻿​​﻿​​﻿​p​​​﻿﻿​﻿﻿p​​﻿﻿​﻿​﻿e​﻿﻿​﻿​﻿﻿n﻿﻿​﻿​﻿﻿​ ​​​​﻿​​​s​​​​﻿​﻿​o﻿​﻿​​​​​
o﻿﻿﻿﻿​﻿﻿﻿f​​﻿​​​​﻿t​​​﻿﻿﻿﻿﻿e﻿​﻿​​​​​n﻿​​​﻿﻿﻿​ ﻿﻿​​​﻿​​i​﻿​​﻿​﻿​t﻿​﻿﻿​​﻿﻿ ﻿​﻿​​﻿﻿​w﻿﻿​​﻿﻿​​o﻿​​﻿​﻿﻿﻿u​​﻿​﻿​﻿​l​﻿​​﻿﻿​​d﻿​​​﻿​​​ ﻿﻿﻿﻿​​​﻿b﻿​﻿​​﻿﻿﻿e﻿﻿​﻿﻿﻿​﻿ ﻿​﻿﻿​​​﻿o﻿﻿​﻿​﻿﻿﻿bvious that there was some kind of manipulation
going on. Steganographic encryption where it is obvious that there
has been some tampering with the carrier is not going to be very
effective. Not that any of these implementations would hold up to
serious scrutiny anyway. Anyway, here's a semi-bogus implementation
that hides information in white space. The message is hidden in this
paragraph of text. Yes, really. It requires a fairly modern file
viewer to display (not display?) the hidden message, but that isn't
too unlikely any more. It may be stretching things to call this a
Bacon cipher, but I think it falls within the spirit of the task,
if not the exact definition.

Hidden message revealed:
This task is to implement a program for encryption and decryption
of plaintext using the simple alphabet of the Baconian cipher or
some other kind of representation of this alphabet (make anything
signify anything). This example will work with anything in the
ASCII range... even code! $r%_-^&*(){}+~ #=`/\';*1234567890"'

```



## Perl 6

Not truly a Bacon Cipher as it doesn't encode using font variations. But fits with the spirit if not the exact definition.
{{works with|Rakudo|2015-11-20}}

```perl6
my $secret = q:to/END/;
    This task is to implement a program for encryption and decryption
    of plaintext using the simple alphabet of the Baconian cipher or
    some other kind of representation of this alphabet (make anything
    signify anything). This example will work with anything in the
    ASCII range... even code! $r%_-^&*(){}+~ #=`/\';*1234567890"'
    END

my $text = q:to/END/;
    Bah. It isn't really practical to use typeface changes to encode
    information, it is too easy to tell that there is something going
    on and will attract attention. Font changes with enough regularity
    to encode mesages relatively efficiently would need to happen so
    often it would be obvious that there was some kind of manipulation
    going on. Steganographic encryption where it is obvious that there
    has been some tampering with the carrier is not going to be very
    effective. Not that any of these implementations would hold up to
    serious scrutiny anyway. Anyway, here's a semi-bogus implementation
    that hides information in white space. The message is hidden in this
    paragraph of text. Yes, really. It requires a fairly modern file
    viewer to display (not display?) the hidden message, but that isn't
    too unlikely any more. It may be stretching things to call this a
    Bacon cipher, but I think it falls within the spirit of the task,
    if not the exact definition.
    END
#'
my @enc = "﻿", "​";
my %dec = @enc.pairs.invert;

sub encode ($c) { @enc[($c.ord).fmt("%07b").comb].join('') }

sub hide ($msg is copy, $text) {
    $msg ~= @enc[0] x (0 - ($msg.chars % 8)).abs;
    my $head = $text.substr(0,$msg.chars div 8);
    my $tail = $text.substr($msg.chars div 8, *-1);
    ($head.comb «~» $msg.comb(/. ** 8/)).join('') ~ $tail;
}

sub reveal ($steg) {
    join '', map { :2(%dec{$_.comb}.join('')).chr },
    $steg.subst( /\w | <punct> | " " | "\n" /, '', :g).comb(/. ** 7/);
}

my $hidden = join '', map  { .&encode }, $secret.comb;

my $steganography = hide $hidden, $text;

say "Steganograpic message hidden in text:";
say $steganography;

say '*' x 70;

say "Hidden message revealed:";
say reveal $steganography;
```


{{out}}

```txt
Steganograpic message hidden in text:
B​﻿​﻿​﻿﻿​a​﻿​﻿﻿﻿​​h﻿​﻿﻿​​​​.﻿﻿​​﻿​﻿﻿ ﻿﻿﻿​​​﻿​I﻿﻿​​﻿﻿﻿﻿t​​​​﻿﻿​​ ​​﻿​﻿​​﻿i​﻿﻿﻿﻿﻿​​s﻿​﻿﻿​​​​n﻿﻿​​﻿​﻿﻿'﻿﻿﻿​​​﻿​t﻿﻿​​﻿​​​ ​﻿​﻿﻿﻿﻿﻿r​​﻿​﻿﻿​​e​﻿​​﻿​​​a​﻿﻿﻿﻿​​﻿l​​﻿﻿​​﻿﻿l​﻿​​​﻿​​y﻿​​​﻿﻿​﻿ ​​​﻿​​​﻿p​​​﻿​﻿﻿﻿r​﻿﻿﻿﻿﻿​​a﻿﻿﻿﻿​﻿​﻿c﻿﻿﻿﻿​​​﻿t﻿﻿﻿​​​﻿﻿i​﻿​​﻿​​​c​​​﻿﻿​​​a​​​﻿﻿​﻿​l​﻿﻿﻿﻿​​​ ﻿​​﻿​﻿​﻿t﻿﻿﻿﻿​​﻿﻿o​​﻿​​﻿​​ ​​​​​﻿﻿​u﻿﻿​﻿﻿﻿﻿﻿s​​﻿﻿​﻿​​e​﻿​​​﻿​​ ﻿﻿﻿​​​​​t﻿﻿​﻿​​​​y﻿﻿​​​​﻿﻿p﻿﻿​​​﻿​﻿e﻿​​﻿​﻿﻿​f​​﻿​​​​​a​﻿​​​﻿﻿​c﻿﻿﻿﻿﻿​​﻿e﻿﻿﻿​​​﻿​ ​​﻿​​﻿﻿​c﻿﻿﻿​﻿﻿﻿﻿h﻿​​﻿﻿​﻿﻿a​​﻿﻿​﻿​​n​﻿﻿﻿​​​​g​﻿﻿​﻿​​​e​﻿﻿​​​​﻿s﻿﻿﻿​​​﻿​ ﻿﻿​​﻿​﻿﻿t​​​﻿​​​​o​​﻿​​​﻿﻿ ﻿﻿​﻿​﻿​​e﻿​​​​​​﻿n﻿​​﻿﻿​﻿﻿c﻿﻿﻿​​​﻿﻿o﻿﻿​​﻿​​﻿d﻿​​﻿﻿﻿﻿​e​​﻿​﻿﻿​​
i​﻿​﻿﻿​​﻿n﻿​﻿​​​​​f﻿﻿﻿​​​﻿​o﻿﻿﻿​﻿﻿﻿﻿r﻿​​​﻿​﻿​m​​​﻿﻿​​​a​﻿​﻿﻿​​​t﻿​​​﻿​​﻿i﻿​​​﻿​﻿﻿o﻿﻿﻿​​​﻿​n﻿﻿​​﻿​﻿﻿,﻿​​﻿﻿​﻿​ ﻿​﻿﻿﻿﻿﻿​i​​﻿﻿​​​​t﻿​﻿﻿​​​﻿ ​​﻿​​​​﻿i﻿﻿﻿​​﻿​​s﻿﻿​​﻿﻿​﻿ ​﻿​﻿﻿﻿﻿﻿t​​﻿﻿﻿﻿​​o​﻿​​﻿﻿​​o​﻿﻿﻿﻿​​﻿ ​﻿﻿﻿​​﻿﻿e﻿﻿​​​﻿﻿﻿a​﻿​​﻿﻿​﻿s​​​​﻿​﻿﻿y﻿​﻿﻿﻿﻿﻿​ ​﻿​​​​​​t﻿﻿​​﻿﻿​﻿o﻿﻿﻿﻿​​​﻿ ​﻿﻿​​﻿​﻿t﻿﻿​​﻿﻿​﻿e​﻿​﻿﻿﻿﻿﻿l​﻿﻿﻿﻿​﻿​l​﻿﻿﻿﻿​​​ ﻿﻿﻿​​​​﻿t​​​​​​﻿​h​​﻿​​﻿​﻿a﻿​​​﻿﻿﻿﻿t​​​﻿​​​﻿ ﻿​﻿﻿﻿﻿﻿​t​﻿﻿﻿​​​​h﻿​﻿﻿​​​​e﻿﻿﻿﻿​​﻿​r﻿﻿﻿​​﻿﻿​e﻿​​​​﻿﻿​ ﻿﻿​﻿﻿﻿﻿﻿i​​﻿​​​​​s​​﻿﻿​﻿﻿﻿ ﻿​﻿​﻿​​​s﻿﻿​​​​﻿​o​​​​​﻿​​m﻿​​​﻿﻿​﻿e​﻿​﻿﻿﻿﻿﻿t​​﻿​​​​​h​​﻿​﻿﻿​​i﻿​﻿﻿﻿​​﻿n﻿​﻿​​​​﻿g﻿​﻿﻿​﻿﻿﻿ ﻿﻿​​﻿​﻿​g​​​﻿​﻿﻿​o​​﻿​​​﻿​i​﻿﻿​﻿﻿﻿​n﻿﻿﻿﻿﻿​​﻿g​​​​​​﻿﻿
o﻿﻿​​​﻿﻿​n﻿​​﻿﻿​﻿​ ​​​﻿﻿﻿﻿​a​​﻿﻿​﻿​​n﻿﻿​﻿​​​​d﻿﻿​​​​﻿﻿ ​﻿​​​﻿​​w​﻿​​​﻿​﻿i﻿​​﻿﻿﻿﻿​l​​​﻿​﻿﻿​l​﻿​﻿﻿​​​ ﻿​​​​​​﻿a​​​﻿﻿​﻿﻿t﻿﻿﻿​​﻿​​t​​​​﻿﻿​​r﻿﻿​﻿﻿﻿﻿﻿a​​​﻿​﻿﻿​c​﻿​﻿﻿﻿​​t﻿​﻿﻿​​​​ ﻿﻿​​﻿​﻿﻿a﻿﻿﻿​​﻿﻿﻿t﻿​​​﻿​​﻿t﻿​​​﻿﻿﻿﻿e​​﻿​﻿﻿﻿​n​﻿﻿﻿﻿​​​t﻿﻿﻿​﻿​​﻿i﻿​﻿​​​​﻿o​﻿﻿﻿​﻿﻿﻿n﻿﻿﻿​﻿​﻿﻿.﻿​​﻿​​﻿​ ​​﻿﻿﻿﻿​​F​﻿​﻿​​​​o﻿﻿​﻿​﻿​﻿n﻿﻿﻿﻿​​﻿﻿t﻿﻿​​​﻿​​ ​﻿​​​​﻿﻿c​​​​﻿​﻿﻿h​​﻿​﻿﻿﻿​a​﻿​﻿﻿​​​n﻿​​​﻿​​﻿g﻿​​​﻿﻿﻿​e﻿​﻿​​​﻿﻿s​​​​﻿​﻿﻿ ​​​﻿﻿​​​w​​﻿​​​﻿​i​﻿​﻿﻿​​​t﻿﻿​​﻿​​​h​﻿﻿​﻿​﻿﻿ ﻿﻿﻿​​﻿﻿﻿e﻿​​​﻿​​​n﻿​​​​﻿﻿​o​​​﻿​﻿﻿​u​﻿​﻿﻿﻿​​g﻿​﻿﻿​​​﻿h​​​﻿​​﻿﻿ ​​​﻿​﻿​﻿r﻿​﻿​﻿​​​e﻿﻿​﻿﻿﻿﻿﻿g​﻿​﻿​﻿﻿​u​﻿​﻿﻿﻿​​l﻿​﻿﻿​​​​a﻿﻿​​﻿​﻿﻿r﻿﻿﻿​​﻿﻿​i﻿​​​​​﻿﻿t﻿​​﻿﻿﻿﻿​y​​﻿​​﻿​​
t﻿​​﻿﻿​​﻿o﻿​﻿​﻿​﻿﻿ ﻿﻿﻿​​​﻿​e​​​​﻿​﻿﻿n​​​﻿​​﻿﻿c​​﻿​​﻿﻿﻿o​﻿﻿﻿﻿﻿​​d​﻿​​​​​﻿e​​​​​​​﻿ ﻿​﻿​​﻿​﻿m​​﻿​﻿﻿﻿﻿e﻿​​​﻿​​​s​​﻿​﻿﻿​​a​​﻿​﻿﻿​​g﻿​﻿﻿﻿﻿​﻿e﻿﻿﻿﻿​​﻿﻿s﻿﻿​​​﻿​​ ​﻿​​​​﻿﻿r​​​​﻿​﻿﻿e​​﻿​﻿﻿﻿​l​﻿​﻿﻿​​​a﻿​​​﻿​​﻿t﻿​​​﻿​﻿﻿i﻿﻿﻿​​﻿​﻿v﻿​​​﻿​​​e﻿﻿​﻿﻿﻿﻿﻿l​​​﻿​﻿﻿​y​﻿​﻿﻿﻿​​ ﻿﻿​﻿​﻿﻿﻿e​﻿​﻿​﻿﻿﻿f﻿﻿​​﻿​﻿﻿f​​​﻿﻿﻿﻿​i​​﻿﻿​﻿﻿​c​﻿﻿​﻿﻿​﻿i​﻿﻿﻿﻿﻿​​e​﻿﻿​﻿​​﻿n﻿﻿﻿​​​﻿​t​​﻿​​﻿﻿​l​​​​﻿﻿​﻿y​﻿​﻿​​​﻿ ﻿​﻿​​​﻿﻿w​﻿​​​﻿﻿​o﻿﻿﻿﻿﻿​​﻿u﻿​﻿​​​​﻿l​​﻿​​﻿﻿​d﻿​​​﻿​​​ ﻿﻿​﻿﻿﻿﻿﻿n​​﻿﻿﻿​​​e​﻿​​​​​​e﻿﻿​﻿﻿​​﻿d﻿​﻿​﻿​﻿﻿ ﻿﻿​﻿​﻿﻿﻿t﻿﻿﻿​﻿﻿​﻿o﻿​​​﻿﻿​﻿ ﻿​﻿﻿​﻿​​h﻿​​​​​﻿​a﻿​​﻿​​﻿​p​​​﻿﻿​﻿﻿p​​﻿﻿​﻿​﻿e​﻿﻿​﻿​﻿﻿n﻿﻿​﻿​﻿﻿​ ​​​​﻿​​​s​​​​﻿​﻿​o﻿​﻿​​​​​
o﻿﻿﻿﻿​﻿﻿﻿f​​﻿​​​​﻿t​​​﻿﻿﻿﻿﻿e﻿​﻿​​​​​n﻿​​​﻿﻿﻿​ ﻿﻿​​​﻿​​i​﻿​​﻿​﻿​t﻿​﻿﻿​​﻿﻿ ﻿​﻿​​﻿﻿​w﻿﻿​​﻿﻿​​o﻿​​﻿​﻿﻿﻿u​​﻿​﻿​﻿​l​﻿​​﻿﻿​​d﻿​​​﻿​​​ ﻿﻿﻿﻿​​​﻿b﻿​﻿​​﻿﻿﻿e﻿﻿​﻿﻿﻿​﻿ ﻿​﻿﻿​​​﻿o﻿﻿​﻿​﻿﻿﻿bvious that there was some kind of manipulation
going on. Steganographic encryption where it is obvious that there
has been some tampering with the carrier is not going to be very
effective. Not that any of these implementations would hold up to
serious scrutiny anyway. Anyway, here's a semi-bogus implementation
that hides information in white space. The message is hidden in this
paragraph of text. Yes, really. It requires a fairly modern file
viewer to display (not display?) the hidden message, but that isn't
too unlikely any more. It may be stretching things to call this a
Bacon cipher, but I think it falls within the spirit of the task,
if not the exact definition.
**********************************************************************
Hidden message revealed:
This task is to implement a program for encryption and decryption
of plaintext using the simple alphabet of the Baconian cipher or
some other kind of representation of this alphabet (make anything
signify anything). This example will work with anything in the
ASCII range... even code! $r%_-^&*(){}+~ #=`/\';*1234567890"'
```



## Phix

Rather than explictly using 'a'="AAAAA", 'b'='AAAAB', c='AAABA", etc, this
notes that "AAAAA".."BBBAA" are just another form of binary, and therefore
handles them as 0..26, ie a-z plus space for any non-letters.

You could of course extend this to encode full binary, by changing the bits
to 8, and accepting every input byte as-is, or use an image file instead of
plaintext simply by reading it using get_text() and (from some appropriate
offset) setting the pixel colours to odd/even instead of upper/lower, and
start by encoding the message length in bytes, that is rather than sending
half of/an obviously corrupt image file.


```Phix
constant bits = 5,
         mask = power(2,bits-1)

function bacon(string msg, plaintext)
    plaintext = lower(plaintext)
    integer ptdx = 0
    for i=1 to length(msg) do
        integer inch = lower(msg[i])-'a'
        if inch<0 or inch>25 then inch = 26 end if
        integer m = mask
        while true do       -- encode one character
            while true do       -- find an a-z for this bit
                ptdx += 1
                if ptdx>length(plaintext) then
                    crash("plaintext too short")
                end if
                integer ch = plaintext[ptdx]
                if ch>='a' and ch<='z' then
                    if and_bits(inch,m) then
                        plaintext[ptdx] = upper(ch)
                    end if
                    exit
                end if
            end while
            if m=1 then exit end if
            m /= 2
        end while
    end for
    return plaintext[1..ptdx]
--  return plaintext -- note: yields ...dogaaaaaaaaaa.....
end function         --       [or the "oops?" in nocab()]

function nocab(string plaintext)
    string res = ""
    integer m = mask, ch = 'a'
    for i=1 to length(plaintext) do
        integer inch = lower(plaintext[i])
        if inch>='a' and inch<='z' then
            if inch!=plaintext[i] then
                ch += m
            end if
            if m=1 then
                res &= iff(ch>'z'?' ':ch)
                m = mask
                ch = 'a'
            else
                m /= 2
            end if
        end if
    end for
    if m!=mask or ch!='a' then crash("oops?") end if
    return res
end function

constant plaintext = """
Bacon's cipher is a method of steganography created by Francis Bacon.
This task is to implement a program for encryption and decryption of
plaintext using the simple alphabet of the baconian cipher or other
representation of this alphabet. The baconian alphabet may optionally
be extended to encode all lower case characters individually and/or
add a few punctuation characters such as the space."""

constant msg = "The quick brown fox jumps over the lazy dog"

string ant = bacon(msg,plaintext),
       dec = nocab(ant)
puts(1,"Encrypted:\n"&ant&"\n")
puts(1,"Decrypted:\n"&dec&"\n")
```

{{out}}

```txt

Encrypted:
BacON's cIPHer Is a MEtHoD of stEgAnogRaphy crEatEd By FRaNcis baCOn.
thIs TASk Is TO imPLeMENt A proGrAm FOR eNcRYPTIoN anD deCRyPtioN Of
plAINTExt UsINg The SIMpLe AlPhaBet Of thE BAcOnIan CIphER Or oTheR
RePreSeNTation OF thIS AlphABeT. the bACoNIAn alPHa
Decrypted:
the quick brown fox jumps over the lazy dog

```



## Python

This deviates from the Bacon method as it encodes to different capitalisation of text rather than differences in font.


```python
import string

sometext = """All children, except one, grow up. They soon know that they will grow
up, and the way Wendy knew was this. One day when she was two years old
she was playing in a garden, and she plucked another flower and ran with
it to her mother. I suppose she must have looked rather delightful, for
Mrs. Darling put her hand to her heart and cried, "Oh, why can't you
remain like this for ever!" This was all that passed between them on
the subject, but henceforth Wendy knew that she must grow up. You always
know after you are two. Two is the beginning of the end.

Of course they lived at 14 [their house number on their street], and
until Wendy came her mother was the chief one. She was a lovely lady,
with a romantic mind and such a sweet mocking mouth. Her romantic
mind was like the tiny boxes, one within the other, that come from the
puzzling East, however many you discover there is always one more; and
her sweet mocking mouth had one kiss on it that Wendy could never get,
though there it was, perfectly conspicuous in the right-hand corner.""".lower()

lc2bin = {ch: '{:05b}'.format(i)
          for i, ch in enumerate(string.ascii_lowercase + ' .')}
bin2lc = {val: key for key, val in lc2bin.items()}

phrase = 'Rosetta code Bacon cipher example secret phrase to encode in the capitalisation of peter pan'.lower()

def to_5binary(msg):
    return ( ch == '1' for ch in ''.join(lc2bin.get(ch, '') for ch in msg.lower()))

def encrypt(message, text):
    bin5 = to_5binary(message)
    textlist = list(text.lower())
    out = []
    for capitalise in bin5:
        while textlist:
            ch = textlist.pop(0)
            if ch.isalpha():
                if capitalise:
                    ch = ch.upper()
                out.append(ch)
                break
            else:
                out.append(ch)
        else:
            raise Exception('ERROR: Ran out of characters in sometext')
    return ''.join(out) + '...'


def  decrypt(bacontext):
    binary = []
    bin5 = []
    out = []
    for ch in bacontext:
        if ch.isalpha():
            binary.append('1' if ch.isupper() else '0')
            if len(binary) == 5:
                bin5 = ''.join(binary)
                out.append(bin2lc[bin5])
                binary = []
    return ''.join(out)


print('PLAINTEXT = \n%s\n' % phrase)
encrypted = encrypt(phrase, sometext)
print('ENCRYPTED = \n%s\n' % encrypted)
decrypted = decrypt(encrypted)
print('DECRYPTED = \n%s\n' % decrypted)
assert phrase == decrypted, 'Round-tripping error'
```


{{out}}

```txt
PLAINTEXT =
rosetta code bacon cipher example secret phrase to encode in the capitalisation of peter pan

ENCRYPTED =
All cHiLDReN, exCept One, GroW UP. thEY soon kNOw That tHey WILl groW
Up, aNd tHE wAy wendY knew was tHis. ONE daY WhEN ShE was tWo yEars oLD
SHe wAS PlaYinG in a GARdEn, anD shE pLUCked anoTHer fLOWEr AnD Ran WitH
It To Her MothEr. i supPoSe shE muSt hAve LOOKeD raTHER deLIGHtfuL, for
mrS. daRlinG puT HeR hAnd TO hER HeARt And cRied, "OH, wHy caN't yOU
RemaiN LikE thIS fOr eVer!" thIS wAS AlL tHat PAssED BetWeeN ThEm on
tHe subjecT, BUT hEnceForTH wendy kNeW ThAt shE muSt grow uP. yoU AlWays
kNOW afTEr YOU aRe tWO. Two iS tHE BeGinNING of The End.

OF coUrsE theY LIvEd aT 14 [THEir housE NuM...

DECRYPTED =
rosetta code bacon cipher example secret phrase to encode in the capitalisation of peter pan
```



## Racket



```racket
#lang racket
(require xml)

(define (char->bacon-number C)
  (define c (char-downcase C))
  (define c-code (- (char->integer c) (char->integer #\a)))
  (and (<= 0 c-code 26) (- c-code (if (> c-code  8) 1 0) (if (> c-code 20) 1 0))))

(define (inr-encode bacons f-cs seg/r rv/r last-bacon-bit fce)
  (cond
    [(null? bacons) (append (reverse (if (null? seg/r) rv/r (cons seg/r rv/r))) (list f-cs))]
    [(null? f-cs) (error 'bacon-encode->list "not enough false message to hide the text")]
    [(zero? last-bacon-bit) (inr-encode (cdr bacons) f-cs seg/r rv/r 5 fce)]
    [(not (char-alphabetic? (car f-cs)))
     (inr-encode bacons (cdr f-cs) (cons (car f-cs) seg/r) rv/r last-bacon-bit fce)]
    [else
     (define bit (sub1 last-bacon-bit))
     (define bs? (bitwise-bit-set? (car bacons) bit))
     (match-define (cons f-a f-d) f-cs)
     (match (cons bs? fce)
       [(or '(#f . 1) '(#t . 2)) (inr-encode bacons f-d (cons f-a seg/r) rv/r bit fce)]
       [_ (inr-encode bacons f-d (list f-a) (cons (reverse seg/r) rv/r) bit (if bs? 2 1))])]))

(define (bacon-encode->segments-list plain-text false-message)
  (define bacon-numbers (filter-map char->bacon-number (string->list plain-text)))
  (map list->string (inr-encode bacon-numbers (string->list false-message) null null 5 1)))

(define (bacon-encode->html plain-text false-message
                            (->face1 (λ (s) `(span ((face "1")) ,s)))
                            (->face2 (λ (s) `(span ((face "2")) ,s))))
  (define segments (bacon-encode->segments-list plain-text false-message))
  (xexpr->string
   (list* 'div '((style "white-space: pre"))
          (for/list ((seg (in-list segments)) (face (in-cycle (in-list (list ->face1 ->face2)))))
            (face seg)))))

(module+ main
  (define plain-text "i wrote this F.B.")
  (define false-text #<<EOS
To be, or not to be, that is the question:
Whether 'tis Nobler in the mind to suffer
The Slings and Arrows of outrageous Fortune,
[...]
EOS
    )

  (displayln (bacon-encode->html plain-text false-text values (λ (s) `(i ,s)))))
```


{{out}}

*Literal

```txt
<div style="white-space: pre">T<i>o </i>be, o<i>r </i>n<i>o</i>t t<i>o </i>be, tha<i>t i</i>s <i>th</i>e q<i>u</i>est<i>i</i>on:
<i>W</i>he<i>t</i>her '<i>tis </i>N<i>o</i>ble<i>r </i>in t<i>h</i>e m<i>i</i>n<i>d </i>to su<i>f</i>fer
The Slings and Arrows of outrageous Fortune,
[...]</div>
```


*HTML
<div style="white-space: pre">T<i>o </i>be, o<i>r </i>n<i>o</i>t t<i>o </i>be, tha<i>t i</i>s <i>th</i>e q<i>u</i>est<i>i</i>on:
<i>W</i>he<i>t</i>her '<i>tis </i>N<i>o</i>ble<i>r </i>in t<i>h</i>e m<i>i</i>n<i>d </i>to su<i>f</i>fer
The Slings and Arrows of outrageous Fortune,
[...]</div>


## REXX


### assigned cipher codes

This REXX version supports a full (26-letter) Roman (Latin) alphabet, plus a few punctuation symbols:

<big>'''.'''</big>   (period),
<big>''','''</big>   (comma),
<big>'''?'''</big>   (question mark),
<big>''':'''</big>   (colon),
<big>'''!'''</big>   (exclamation mark),   and blanks.

All alphabetic letters are handled as if they were in uppercase   (i.e., lowercase letters are uppercased).

```rexx
/*REXX program implements and demonstrates a (full)  "Bacon"  cipher (cypher).*/
parse arg plain                        /*obtain optional arguments from the CL*/
if plain=''  then plain =  "The quick brown fox jumped over the lazy dog."
                                       /* [↓]  code supports complete alphabet*/
@.=; @.a=11111; @.b=11110; @.c=11101; @.d=11100; @.e=11011; @.f=11010; @.g=11001
     @.h=11000; @.i=10111; @.j=00111; @.k=10110; @.l=10101; @.m=10100; @.n=10011
     @.o=10010; @.p=10001; @.q=10000; @.r=01111; @.s=01110; @.t=01101; @.u=01100
     @.v=00100; @.w=01011; @.x=01010; @.y=01001; @.z=01000; @.?=00000; @.!=00101
     @..=00110;   _=','  ; @._=00001;   _=' '  ; @._=00011;   _=':'  ; @._=00010
                                       /* [↑]  code supports some punctuation.*/
say ' plain text: '    plain           /*display the original  (plain)  text. */
      encoded=BaconEnc(plain)          /*encode using a (full)  Bacon  cipher.*/
say 'cipher text: '    encoded         /*display the ciphered  (coded)  text. */
      decoded=BaconDec(encoded)        /*decode ciphered text──►plain (almost)*/
say 'cycled text: '    decoded         /*display the recycled text  (~ plain),*/
exit                                   /*stick a fork in it,  we're all done. */
/*────────────────────────────────────────────────────────────────────────────*/
BaconEnc: procedure expose @.;        arg x;                $=;     Lx=length(x)
            do j=1  for Lx;           _=substr(x,j,1);      $=$ || @._;      end
          return $
/*────────────────────────────────────────────────────────────────────────────*/
BaconDec: procedure expose @.;        parse arg x;          $=;     Lx=length(x)
            do k=0 for 256; _=d2c(k); if @._=='' then iterate; q=@._; !.q=_; end
            do j=1  to Lx  by 5;      y=substr(x,j,5);      $=$ || !.y;      end
          return $
```

'''output'''   when using the default input:

```txt

 plain text:  The quick brown fox jumped over the lazy dog.
cipher text:  011011100011011000111000001100101111110110110000111111001111100100101110011000111101010010010100001100111011001010010001110111110000011100100010011011011110001101101110001101100011101011111101000010010001111100100101100100110
cycled text:  THE QUICK BROWN FOX JUMPED OVER THE LAZY DOG.

```



### generated cipher codes

The twp glyphs (characters) chosen for this REXX program are:
:::*   the          ''bottom tee''              <big><big>┴</big></big>     (sometimes known as the   ''bottom junction'')
:::*   the     ''top tee''       <big><big>┬</big></big>     (sometimes known as the   ''top junction'')

```rexx
/*REXX program implements and demonstrates a (full)  "Bacon"  cipher (cypher).*/
parse arg plain                        /*obtain optional arguments from the CL*/
if plain=''  then plain =  "The quick brown fox jumped over the lazy dog."
                                       /*alphabet must be in uppercase letters*/
alphabet= 'ABCDEFGHIJKLMNOPQRSTUVWXYZ .,?!:'  /*list of letters & punctuation.*/
@.=                                           /*assign a default for all chars*/
     do j=0  for min(32,length(alphabet));              _=substr(alphabet,j+1,1)
     @._=translate(right(x2b(d2x(j)), 5, 0),  '┴┬', 01)
     end   /*j*/                       /* [↑]  build the symbol table (max=32)*/
                                       /* [↑]  code supports some punctuation.*/
say ' plain text: '    plain           /*display the original  (plain)  text. */
      encoded=BaconEnc(plain)          /*encode using a (full)  Bacon  cipher.*/
say 'cipher text: '    encoded         /*display the ciphered  (coded)  text. */
      decoded=BaconDec(encoded)        /*decode ciphered text──►plain (almost)*/
say 'cycled text: '    decoded         /*display the recycled text  (~ plain),*/
exit                                   /*stick a fork in it,  we're all done. */
/*────────────────────────────────────────────────────────────────────────────*/
BaconEnc: procedure expose @.;        arg x;                $=;     Lx=length(x)
            do j=1  for Lx;           _=substr(x,j,1);      $=$ || @._;      end
          return $
/*────────────────────────────────────────────────────────────────────────────*/
BaconDec: procedure expose @.;        parse arg x;          $=;     Lx=length(x)
            do k=0 for 256; _=d2c(k); if @._=='' then iterate; q=@._; !.q=_; end
            do j=1  to Lx  by 5;      y=substr(x,j,5);      $=$ || !.y;      end
          return $
```

'''output'''   when using the default input:

```txt

 plain text:  The quick brown fox jumped over the lazy dog.
cipher text:  ┬┴┴┬┬┴┴┬┬┬┴┴┬┴┴┬┬┴┬┴┬┴┴┴┴┬┴┬┴┴┴┬┴┴┴┴┴┴┬┴┴┬┴┬┴┬┬┴┬┴┴┴┴┴┬┬┴┴┴┬┴┬┬┬┴┬┴┬┬┴┴┬┬┴┬┬┬┴┬┴┴┴┬┴┬┴┬┬┬┴┬┴┬┬┬┬┬┴┬┴┴┬┴┴┬┬┴┬┴┴┴┬┬┴┴┴┬┬┬┬┴┴┬┴┴┴┴┴┬┬┬┬┴┬┴┴┬┬┬┴┬┴┬┴┬┴┴┬┴┴┬┴┴┴┬┬┬┴┬┴┬┴┴┬┬┴┴┬┬┬┴┴┬┴┴┬┬┴┬┴┴┬┴┬┬┴┴┴┴┴┬┬┴┴┬┬┬┴┴┴┬┬┴┬┴┴┴┴┬┬┴┬┬┬┴┴┴┬┬┴┬┬┴┬┬
cycled text:  THE QUICK BROWN FOX JUMPED OVER THE LAZY DOG.

```



### uses upper/lower case


```rexx
/*REXX program implements and demonstrates a (full)  "Bacon"  cipher (cypher).*/
parse arg plain                        /*obtain optional arguments from the CL*/
if plain=''  then plain =  "The quick brown fox jumped over the lazy dog."
                                       /*alphabet must be in uppercase letters*/
alphabet= 'ABCDEFGHIJKLMNOPQRSTUVWXYZ .,?!:'  /*list of letters & punctuation.*/
@.=                                           /*assign a default for all chars*/
     do j=0  for min(32,length(alphabet));              _=substr(alphabet,j+1,1)
     @._=translate(right(x2b(d2x(j)), 5, 0),  'sS', 01)
     end   /*j*/                       /* [↑]  build the symbol table (max=32)*/
                                       /* [↑]  code supports some punctuation.*/
say ' plain text: '    plain           /*display the original  (plain)  text. */
      encoded=BaconEnc(plain)          /*encode using a (full)  Bacon  cipher.*/
say 'cipher text: '    encoded         /*display the ciphered  (coded)  text. */
      decoded=BaconDec(encoded)        /*decode ciphered text──►plain (almost)*/
say 'cycled text: '    decoded         /*display the recycled text  (~ plain),*/
exit                                   /*stick a fork in it,  we're all done. */
/*────────────────────────────────────────────────────────────────────────────*/
BaconEnc: procedure expose @.;        arg x;                $=;     Lx=length(x)
            do j=1  for Lx;           _=substr(x,j,1);      $=$ || @._;      end
          return $
/*────────────────────────────────────────────────────────────────────────────*/
BaconDec: procedure expose @.;        parse arg x;          $=;     Lx=length(x)
            do k=0 for 256; _=d2c(k); if @._=='' then iterate; q=@._; !.q=_; end
            do j=1  to Lx  by 5;      y=substr(x,j,5);      $=$ || !.y;      end
          return $
```

'''output'''   when using the default input:

```txt

 plain text:  The quick brown fox jumped over the lazy dog.
cipher text:  SssSSssSSSssSssSSsSsSssssSsSsssSssssssSssSsSsSSsSsssssSSsssSsSSSsSsSSssSSsSSSsSsssSsSsSSSsSsSSSSSsSssSssSSsSsssSSsssSSSSssSsssssSSSSsSssSSSsSsSsSssSssSsssSSSsSsSssSSssSSSssSssSSsSssSsSSsssssSSssSSSsssSSsSssssSSsSSSsssSSsSSsSS
cycled text:  THE QUICK BROWN FOX JUMPED OVER THE LAZY DOG.

```



## Visual Basic .NET

{{trans|C#}}

```vbnet
Imports System.Text

Module Module1

    ReadOnly CODES As New Dictionary(Of Char, String) From {
        {"a", "AAAAA"}, {"b", "AAAAB"}, {"c", "AAABA"}, {"d", "AAABB"}, {"e", "AABAA"},
        {"f", "AABAB"}, {"g", "AABBA"}, {"h", "AABBB"}, {"i", "ABAAA"}, {"j", "ABAAB"},
        {"k", "ABABA"}, {"l", "ABABB"}, {"m", "ABBAA"}, {"n", "ABBAB"}, {"o", "ABBBA"},
        {"p", "ABBBB"}, {"q", "BAAAA"}, {"r", "BAAAB"}, {"s", "BAABA"}, {"t", "BAABB"},
        {"u", "BABAA"}, {"v", "BABAB"}, {"w", "BABBA"}, {"x", "BABBB"}, {"y", "BBAAA"},
        {"z", "BBAAB"}, {" ", "BBBAA"} ' use " " To denote any non-letter
    }

    Function Encode(plainText As String, message As String) As String
        Dim pt = plainText.ToLower()
        Dim sb As New StringBuilder()
        For Each c In pt
            If "a" <= c AndAlso c <= "z" Then
                sb.Append(CODES(c))
            Else
                sb.Append(CODES(" "))
            End If
        Next

        Dim et = sb.ToString()
        Dim mg = message.ToLower() '"A"s to be in lower case, "B"s in upper case

        sb.Length = 0
        Dim count = 0
        For Each c In mg
            If "a" <= c AndAlso c <= "z" Then
                If et(count) = "A" Then
                    sb.Append(c)
                Else
                    sb.Append(Chr(Asc(c) - 32)) ' upper case equivalent
                End If
                count += 1
                If count = et.Length Then
                    Exit For
                End If
            Else
                sb.Append(c)
            End If
        Next

        Return sb.ToString()
    End Function

    Function Decode(message As String) As String
        Dim sb As New StringBuilder

        For Each c In message
            If "a" <= c AndAlso c <= "z" Then
                sb.Append("A")
            ElseIf "A" <= c AndAlso c <= "Z" Then
                sb.Append("B")
            End If
        Next

        Dim et = sb.ToString()
        sb.Length = 0
        For index = 0 To et.Length - 1 Step 5
            Dim quintet = et.Substring(index, 5)
            Dim key = CODES.Where(Function(a) a.Value = quintet).First().Key
            sb.Append(key)
        Next

        Return sb.ToString()
    End Function

    Sub Main()
        Dim plainText = "the quick brown fox jumps over the lazy dog"
        Dim message =
            "bacon's cipher is a method of steganography created by francis bacon. " +
            "this task is to implement a program for encryption and decryption of " +
            "plaintext using the simple alphabet of the baconian cipher or some " +
            "other kind of representation of this alphabet (make anything signify anything). " +
            "the baconian alphabet may optionally be extended to encode all lower " +
            "case characters individually and/or adding a few punctuation characters " +
            "such as the space."

        Dim cipherText = Encode(plainText, message)
        Console.WriteLine("Cipher text ->" & Environment.NewLine & "{0}", cipherText)

        Dim decodedText = Decode(cipherText)
        Console.WriteLine(Environment.NewLine & "Hidden text ->" & Environment.NewLine & "{0}", decodedText)
    End Sub

End Module
```

{{out}}

```txt
Cipher text ->
BacON's cIPHer Is a METhoD of stEgAnogRaphy crEatEd By FRAncis baCOn. thIs TASk Is TO imPLeMENT a proGrAm FOR eNcRYPTIOn anD deCRyPtioN Of plAINTExt UsING the SIMpLe AlPhaBet Of thE BAConIan CIphER Or sOme OTHer kInD Of reprESenTATion OF This alPHaBET (makE An

Hidden text ->
the quick brown fox jumps over the lazy dog
```



## zkl

{{trans|Python}}

```zkl
class Bacon{
   fcn init(_keyText){
      var [const] keyText=_keyText.toLower(),
        lowerLetters=["a".."z"].pump(String),
	upperLetters=["A".."Z"].pump(String),
	letters=String(lowerLetters,upperLetters),
	lc2bin=[0..].zip(lowerLetters + " .") //-->("a":"00000", ...)
	   .pump(Dictionary(),fcn([(n,ch)]){ T(ch,"%05.2B".fmt(n)) }),
	bin2lc=lc2bin.pump(Dictionary(),"reverse");  //-->("00000":"a", ...),
      ;
   }
   fcn to5binary(msg){ //-->stream of 1s and 0s (1 means capitalize)
      msg.toLower().pump(String,lc2bin.get.fp1(""))  //-->("abcde"|"") * length
      .pump(Data,"toAsc",'-(0x30)).howza(0)  // "1"-->1, treat result as bytes
   }
   fcn encrypt(msg){
      bin5:=to5binary(msg).walker();  // capitalization overlay of keyText
      e:=keyText.pump(String, 'wrap(ch){
	 if(not lowerLetters.holds(ch)) return(Void.Write); // encrypt only ASCII
	 if(not bin5._next())           return(Void.Stop);  // end of msg
	 (bin5.value and ch.toUpper() or ch);
      });
      if(not bin5.atEnd) throw(Exception.ValueError("Ran out of characters in key text"));
      e + "...."  // pad
   }
   fcn decrypt(bacontext){
      bacontext.filter(letters.holds).pump(String,T(Void.Read,4),
        fcn{ vm.arglist.pump(String,upperLetters.holds,"toInt") : bin2lc[_] });
   }
}
```


```zkl
bacon:=Bacon(
#<<<
0'|All children, except one, grow up. They soon know that they will grow
up, and the way Wendy knew was this. One day when she was two years old
she was playing in a garden, and she plucked another flower and ran with
it to her mother. I suppose she must have looked rather delightful, for
Mrs. Darling put her hand to her heart and cried, "Oh, why can't you
remain like this for ever!" This was all that passed between them on
the subject, but henceforth Wendy knew that she must grow up. You always
know after you are two. Two is the beginning of the end.

Of course they lived at 14 [their house number on their street], and
until Wendy came her mother was the chief one. She was a lovely lady,
with a romantic mind and such a sweet mocking mouth. Her romantic
mind was like the tiny boxes, one within the other, that come from the
puzzling East, however many you discover there is always one more; and
her sweet mocking mouth had one kiss on it that Wendy could never get,
though there it was, perfectly conspicuous in the right-hand corner.|
);
#<<<

phrase:="Rosetta code Bacon cipher example secret phrase to encode in the capitalization of peter pan";

println("PLAINTEXT = \n%s".fmt(phrase));
encrypted,decrypted:=bacon.encrypt(phrase), bacon.decrypt(encrypted);
println("ENCRYPTED = \n%s".fmt(encrypted));
println("DECRYPTED = \n%s".fmt(decrypted));
if(phrase.toLower()!=decrypted) throw(Exception.AssertionError("Round-tripping error"));
```

{{out}}

```txt

PLAINTEXT =
Rosetta code Bacon cipher example secret phrase to encode in the capitalization of peter pan
ENCRYPTED =
All cHiLDReN, exCept One, GroW UP. thEY soon kNOw That tHey WILl groW
Up, aNd tHE wAy wendY knew was tHis. ONE daY WhEN ShE was tWo yEars oLD
SHe wAS PlaYinG in a GARdEn, anD shE pLUCked anoTHer fLOWEr AnD Ran WitH
It To Her MothEr. i supPoSe shE muSt hAve LOOKeD raTHER deLIGHtfuL, for
mrS. daRlinG puT HeR hAnd TO hER HeARt And cRied, "OH, wHy caN't yOU
RemaiN LikE thIS fOr eVer!" thIS wAS AlL tHat PAssED BetWeeN ThEm on
tHe subjecT, BUT hEnceForTH wendy kNeW ThAt shE MusT grow uP. yoU AlWays
kNOW afTEr YOU aRe tWO. Two iS tHE BeGinNING of The End.

OF coUrsE theY LIvEd aT 14 [THEir housE NuM....
DECRYPTED =
rosetta code bacon cipher example secret phrase to encode in the capitalization of peter pan

```

